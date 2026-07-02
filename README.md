# netcadmakro

'TARIH   : 27.09.2001
'AMA�    : Ada ve Parsel i�indeki ADA_NO ve PARSEL_NO yaz�lar�n� Kapal� Objenin
         ' ADI k�sm�na Ada/Parsel �eklinde atamak i�in kullan�l�r.
'OBJELER :KAPALI �OKLUDOGRU - ADA ve PARSEL
         '(KAD_ADA_PL  ve KAD_PARSEL_PL tabakalar�nda kapal� �okludogrular.)

         'TEXT - ADA VE PARSEL NO
         '(KAD_ADA_NO ve KAD_PARSEL_NO tabakalar�nda ada - parsel no'lar)
'UYARILAR : Ada ve Parsel nolar�n yaz� uygulama noktas� ortalanacak
          ' Parsel nolar ayr�ca ekrana ada/parsel olarak yazaca��ndan parsel nolar�n
          ' blok olarak saklanarak sonradan dosya ekle ile eklenmesi gerekmektedir.
' NetCAD �stanbul B�lge M�d�rl���

Function Intersect_Lines(x1,y1, x2, y2,u1, v1, u2, v2)

dim a1,b1,c1,a2,b2,c2,det_inv
dim m1,m2
dim xi,yi
dim p

if (x2-x1)=0 then b1=1e+10 else b1 = (y2-y1)/(x2-x1)
if (u2-u1)=0 then b2=1e+10 else b2 = (v2-v1)/(u2-u1)

a1 = y1-b1*x1
a2 = v1-b2*u1 
xi = - (a1-a2)/(b1-b2)
yi = a1+b1*xi

if (x1-xi)*(xi-x2)>=0 AND (u1-xi)*(xi-u2)>=0 AND _
   (y1-yi)*(yi-y2)>=0 AND (v1-yi)*(yi-v2)>=0  then
    Intersect_lines=True
   else
    Intersect_lines=False
end if
End Function

'---------------------------------------------------------------------------------
Function inside(p , poly)
Dim i, j, count
Dim lt, lp
Dim c1,c2
set c1=netcad.NewObject
set c2=netcad.NewObject
   count = 0
   j = 0
   c2.p1.x=p.x
   c2.p1.y=p.y
   c2.p2.x=999099999999999999999999999
   c2.p2.y=999999099999999999999999999
   For i = 0 To poly.num-1
       c1.p1 = poly.Cor(i)
       if i=poly.num-1 then c1.p2 = poly.Cor(0) else  c1.p2 = poly.Cor(i + 1)
       If intersect_Lines (c1.p1.x,c1.p1.y,c1.p2.x,c1.p2.y, c2.p1.x,c2.p1.y,c2.p2.x,c2.p2.y) Then
          count = count + 1
          end if
   Next
   inside = (count Mod 2 <> 0)
End Function

'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada

with netcad

'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Ada","Ada Poligon Tabakas�", "PARSEL_PL", 15
  BD.GetString  "AdaNo","Ada Yaz� Tabakas�", "PARSEL_NO", 15


  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "

  if BD.showmodal then
    AdaTabaka=.FoundLayer(BD.ValueByName("Ada"))
    AdaNoTabaka=.FoundLayer(BD.ValueByName("AdaNo"))

    if  AdaTabaka=-1 or AdaNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
  Else
    Exit Sub
  End if
  set BD = Nothing

'-------------------------Dialog Son-------------------------------------------------

'******************* Adaya i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        ' Coklu dogrumu ?
          .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(AdaNoTabaka), array(otext)          'Filitre uygula
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname="0/"&o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                ' Filitre uygulamayi birak.
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
next
set ada=nothing

.BackMessage

end with
END SUB





SUB Main
DIM ss,o,i,j,oo,p,sel,poly,tabaka,yazi,a,c   ,bd, secenek
DIM kt() ,t()

With netcad
a=.getparam(94)*3/1000
 set SEL = .NewSelectionSet             ' Yeni kume yarat      .CenterOfMass
  set o = .NewObject
  set poly=.newpoly

   .setparam beginblock,true
   if SEL.SELECT("Kapal� �oklu do�rular� se�",array(opline)) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy

      'tabaka=.LayerNameOf(o.tabaka)&"_KAD_ADA_NO"
      set poly=.getplineext(o)
       yazi=split(o.pname,"_")
       set c = poly.CenterOfMass
       c.x=c.x+40
      .AddObject (.MakeText (c, yazi(0)  , 0,0, a,0,"M",.CreateLayer("ADA_NO",4)))
    next
    .setparam endblock,true
    .NetcadCommand("REDRAW")
    set sel = nothing
    set poly = nothing               ' obje icin aldigimiz memory'i geri ver
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if
End With
END SUB

'Ama� : Ada_Pafta_parsel bi�imindeki kolonlar� ay�r�r.
' 20050218
' Bar�� G�ral
sub main
 dim vt,dg,tbll,tbs,rs
 dim tbarray,sql,pafcol,adacol,parcol,col
 dim verit,tablo,kolon,i,ayirac

 verit  = "\\Syildirimpc\EYUP\KADASTRAL\KAD_PARSEL.mdb" 'Veritaban� Ad�
 tablo  = "PARSEL"  ' Tablo Ad�
 col    = "ADI"     ' B�l�necek olan kolon
 pafCol = "PAFTA"   ' Pafta Kolonu
 adaCol = "ADA"     ' Ada Kolonu
 parCol = "PARSEL"  ' Parsel Kolonu
 ayirac = "_"       ' Ay�ra� Karakteri
 i = 0
 set vt = CreateObject("ADODB.Connection")
 vt.Provider="Microsoft.Jet.OLEDB.4.0"
 vt.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
 vt.Open verit

 set rs = CreateObject("ADODB.Recordset")
 sql = "SELECT * FROM "&tablo
 rs.open sql,vt,1,3
 rs.update

 while not rs.eof
    tbArray = Split(rs(col),ayirac)

    on error resume next
    if tbarray(0) then
      rs(pafCol).Value = tbArray(0)
    else
      rs(pafCol).Value = 0
    end if

    on error resume next
    if tbArray(1) then
      rs(adaCol).Value = tbArray(1)
    else
      rs(adaCol).Value = 0
    end if

    on error resume next
    if tbArray(2) then
      rs(parCol).Value = tbArray(2)
    else
      rs(parCol).Value = 0
    end if
    i = i + 1
    netcad.backmessage
    netcad.setmessage i
    rs.MoveNext
 wend
 rs.close
end sub


'TARIH   : 27.09.2001
'AMA�    : Ada ve Parsel i�indeki ADA_NO ve PARSEL_NO yaz�lar�n� Kapal� Objenin
         ' ADI k�sm�na Ada/Parsel �eklinde atamak i�in kullan�l�r.
'OBJELER :KAPALI �OKLUDOGRU - ADA ve PARSEL
         '(KAD_ADA_PL  ve KAD_PARSEL_PL tabakalar�nda kapal� �okludogrular.)

         'TEXT - ADA VE PARSEL NO
         '(KAD_ADA_NO ve KAD_PARSEL_NO tabakalar�nda ada - parsel no'lar)
'UYARILAR : Ada ve Parsel nolar�n yaz� uygulama noktas� ortalanacak
          ' Parsel nolar ayr�ca ekrana ada/parsel olarak yazaca��ndan parsel nolar�n
          ' blok olarak saklanarak sonradan dosya ekle ile eklenmesi gerekmektedir.
' NetCAD �stanbul B�lge M�d�rl���

Function Intersect_Lines(x1,y1, x2, y2,u1, v1, u2, v2)

dim a1,b1,c1,a2,b2,c2,det_inv
dim m1,m2
dim xi,yi
dim p

if (x2-x1)=0 then b1=1e+10 else b1 = (y2-y1)/(x2-x1)
if (u2-u1)=0 then b2=1e+10 else b2 = (v2-v1)/(u2-u1)

a1 = y1-b1*x1
a2 = v1-b2*u1 
xi = - (a1-a2)/(b1-b2)
yi = a1+b1*xi

if (x1-xi)*(xi-x2)>=0 AND (u1-xi)*(xi-u2)>=0 AND _
   (y1-yi)*(yi-y2)>=0 AND (v1-yi)*(yi-v2)>=0  then
    Intersect_lines=True
   else
    Intersect_lines=False
end if
End Function

'---------------------------------------------------------------------------------
Function inside(p , poly)
Dim i, j, count
Dim lt, lp
Dim c1,c2
set c1=netcad.NewObject
set c2=netcad.NewObject
   count = 0
   j = 0
   c2.p1.x=p.x
   c2.p1.y=p.y
   c2.p2.x=999099999999999999999999999
   c2.p2.y=999999099999999999999999999
   For i = 0 To poly.num-1
       c1.p1 = poly.Cor(i)
       if i=poly.num-1 then c1.p2 = poly.Cor(0) else  c1.p2 = poly.Cor(i + 1)
       If intersect_Lines (c1.p1.x,c1.p1.y,c1.p2.x,c1.p2.y, c2.p1.x,c2.p1.y,c2.p2.x,c2.p2.y) Then
          count = count + 1
          end if
   Next
   inside = (count Mod 2 <> 0)
End Function

'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada

with netcad

'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Ada","Ada Poligon Tabakas�", "KAD_ADA_PL", 15
  BD.GetString  "AdaNo","Ada Yaz� Tabakas�", "KAD_ADA_NO", 15
  BD.GetString  "Parsel","Parsel Poligon Tabakas�", "KAD_PARSEL_PL  ", 15
  BD.GetString  "ParselNo","Parsel Yaz� Tabakas�", "KAD_PARSEL_NO", 15

  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "

  if BD.showmodal then
    AdaTabaka=.FoundLayer(BD.ValueByName("Ada"))
    AdaNoTabaka=.FoundLayer(BD.ValueByName("AdaNo"))
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("ParselNo"))
    if  AdaTabaka=-1 or AdaNoTabaka=-1 or ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
  Else
    Exit Sub
  End if
  set BD = Nothing

'-------------------------Dialog Son-------------------------------------------------

'******************* Adaya i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        ' Coklu dogrumu ?
          .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(AdaNoTabaka), array(otext)          'Filitre uygula
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                ' Filitre uygulamayi birak.
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
next
set ada=nothing

'************ Parsele i�indeki yaz�ya ada/parsel  ata ***************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(ParselNoTabaka), array(otext)       
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 o.s=ada.Pname&"/"&o.S
                 .AddObject .MakeText(o.p1, o.s, 0,0, 1,0,0,.CreateLayer("TEMP_AD", 6))
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing              
next
set ada=nothing

'******************* Parsele i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=ParselTabaka  then       
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(.CreateLayer("TEMP_AD", 6)), array(otext)
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing               
next
set ada=nothing
.BackMessage

end with
END SUB


'TARIH   : 27.09.2001
'AMA�    : Ada ve Parsel i�indeki ADA_NO ve PARSEL_NO yaz�lar�n� Kapal� Objenin
         ' ADI k�sm�na Ada/Parsel �eklinde atamak i�in kullan�l�r.
'OBJELER :KAPALI �OKLUDOGRU - ADA ve PARSEL
         '(KAD_ADA_PL  ve KAD_PARSEL_PL tabakalar�nda kapal� �okludogrular.)

         'TEXT - ADA VE PARSEL NO
         '(KAD_ADA_NO ve KAD_PARSEL_NO tabakalar�nda ada - parsel no'lar)
'UYARILAR : Ada ve Parsel nolar�n yaz� uygulama noktas� ortalanacak
          ' Parsel nolar ayr�ca ekrana ada/parsel olarak yazaca��ndan parsel nolar�n
          ' blok olarak saklanarak sonradan dosya ekle ile eklenmesi gerekmektedir.
' NetCAD �stanbul B�lge M�d�rl���


'---------------------------------------------------------------------------------
Function inside(p , poly)
   inside =   poly.InPoly(p)
End Function

'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada

with netcad

'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Ada","Ada Poligon Tabakas�", "KAD_ADA_PL", 15
  BD.GetString  "AdaNo","Ada Yaz� Tabakas�", "KAD_ADA_NO", 15
  BD.GetString  "Parsel","Parsel Poligon Tabakas�", "KAD_PARSEL_PL  ", 15
  BD.GetString  "ParselNo","Parsel Yaz� Tabakas�", "KAD_PARSEL_NO", 15

  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "

  if BD.showmodal then
    AdaTabaka=.FoundLayer(BD.ValueByName("Ada"))
    AdaNoTabaka=.FoundLayer(BD.ValueByName("AdaNo"))
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("ParselNo"))
    if  AdaTabaka=-1 or AdaNoTabaka=-1 or ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
  Else
    Exit Sub
  End if
  set BD = Nothing

'-------------------------Dialog Son-------------------------------------------------

'******************* Adaya i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        ' Coklu dogrumu ?
          .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(AdaNoTabaka), array(otext)          'Filitre uygula
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                ' Filitre uygulamayi birak.
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
next
set ada=nothing

'************ Parsele i�indeki yaz�ya ada/parsel  ata ***************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(ParselNoTabaka), array(otext)       
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 o.s=ada.Pname&"/"&o.S
                 .AddObject .MakeText(o.p1, o.s, 0,0, 1,0,0,.CreateLayer("TEMP_AD", 6))
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing              
next
set ada=nothing

'******************* Parsele i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=ParselTabaka  then       
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(.CreateLayer("TEMP_AD", 6)), array(otext)
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing               
next
set ada=nothing
.BackMessage

end with
END SUB


'TARIH   : 27.09.2001
'AMA�    : Ada ve Parsel i�indeki ADA_NO ve PARSEL_NO yaz�lar�n� Kapal� Objenin
         ' ADI k�sm�na Ada/Parsel �eklinde atamak i�in kullan�l�r.
'OBJELER :KAPALI �OKLUDOGRU - ADA ve PARSEL
         '(KAD_ADA_PL  ve KAD_PARSEL_PL tabakalar�nda kapal� �okludogrular.)

         'TEXT - ADA VE PARSEL NO
         '(KAD_ADA_NO ve KAD_PARSEL_NO tabakalar�nda ada - parsel no'lar)
'UYARILAR : Ada ve Parsel nolar�n yaz� uygulama noktas� ortalanacak
          ' Parsel nolar ayr�ca ekrana ada/parsel olarak yazaca��ndan parsel nolar�n
          ' blok olarak saklanarak sonradan dosya ekle ile eklenmesi gerekmektedir.
' NetCAD �stanbul B�lge M�d�rl���

Function Intersect_Lines(x1,y1, x2, y2,u1, v1, u2, v2)

dim a1,b1,c1,a2,b2,c2,det_inv
dim m1,m2
dim xi,yi
dim p

if (x2-x1)=0 then b1=1e+10 else b1 = (y2-y1)/(x2-x1)
if (u2-u1)=0 then b2=1e+10 else b2 = (v2-v1)/(u2-u1)

a1 = y1-b1*x1
a2 = v1-b2*u1 
xi = - (a1-a2)/(b1-b2)
yi = a1+b1*xi

if (x1-xi)*(xi-x2)>=0 AND (u1-xi)*(xi-u2)>=0 AND _
   (y1-yi)*(yi-y2)>=0 AND (v1-yi)*(yi-v2)>=0  then
    Intersect_lines=True
   else
    Intersect_lines=False
end if
End Function

'---------------------------------------------------------------------------------
Function inside(p , poly)
Dim i, j, count
Dim lt, lp
Dim c1,c2
set c1=netcad.NewObject
set c2=netcad.NewObject
   count = 0
   j = 0
   c2.p1.x=p.x
   c2.p1.y=p.y
   c2.p2.x=999099999999999999999999999
   c2.p2.y=999999099999999999999999999
   For i = 0 To poly.num-1
       c1.p1 = poly.Cor(i)
       if i=poly.num-1 then c1.p2 = poly.Cor(0) else  c1.p2 = poly.Cor(i + 1)
       If intersect_Lines (c1.p1.x,c1.p1.y,c1.p2.x,c1.p2.y, c2.p1.x,c2.p1.y,c2.p2.x,c2.p2.y) Then
          count = count + 1
          end if
   Next
   inside = (count Mod 2 <> 0)
End Function

'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada

with netcad

'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Ada","Ada Poligon Tabakas�", "KAD_ADA_PL", 15
  BD.GetString  "AdaNo","Ada Yaz� Tabakas�", "KAD_ADA_NO", 15
  BD.GetString  "Parsel","Parsel Poligon Tabakas�", "KAD_PARSEL_PL  ", 15
  BD.GetString  "ParselNo","Parsel Yaz� Tabakas�", "KAD_PARSEL_NO", 15

  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "

  if BD.showmodal then
    AdaTabaka=.FoundLayer(BD.ValueByName("Ada"))
    AdaNoTabaka=.FoundLayer(BD.ValueByName("AdaNo"))
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("ParselNo"))
    if  AdaTabaka=-1 or AdaNoTabaka=-1 or ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
  Else
    Exit Sub
  End if
  set BD = Nothing

'-------------------------Dialog Son-------------------------------------------------

'******************* Adaya i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        ' Coklu dogrumu ?
          .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(AdaNoTabaka), array(otext)          'Filitre uygula
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                ' Filitre uygulamayi birak.
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
next
set ada=nothing

'************ Parsele i�indeki yaz�ya ada/parsel  ata ***************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(ParselNoTabaka), array(otext)       
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 o.s=ada.Pname&"/"&o.S
                 .PutObject .CurObjPos, o
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing              
next
set ada=nothing

'******************* Parsele i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
         .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=ParselTabaka  then       
           .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(ParselNoTabaka), array(otext)
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 ada.Pname=o.S
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                   
        end if
        set o = nothing               
next
set ada=nothing
.BackMessage

end with
END SUB

'
'  Sayisallastir Isleminden Cagrilan Scriptler icin Ornek
'  Bu dosya \Netcad\TOOLS\Script\GW dizine konmali.
'

Sub Main
Dim o,p,UseConman,ObjEdit,defValues

  with NCParam
    ' o ve p varmi kontrol et
    if .IsValueExists("sp_o") and .IsValueExists("sp_p") then
      set o = .GetValue("sp_o")
      set p = .GetValue("sp_p")
      UseConman  = .GetValue("sp_UseConman")
      ObjEdit    = .GetValue("sp_ObjEdit")

      set defValues = nothing
      if .IsValueExists("sp_DefValues") then
        set defValues = .GetValue("sp_DefValues")
      end if

      if UseConman then
        if ObjEdit then
          Netcad.XSetParam PNX_SHOWCMINSPECTORDLG, ""
        end if
        Netcad.AddObjectDBEx o, p, defValues
        'msgbox "DB Mode = " & o.objname
      else
        Netcad.AddObjectX o, p
        'msgbox "Normal Mode"
      end if
    else
      msgbox "Obje parametreleri yok !"
    end if
    set o = nothing
  end with
End Sub

' AMA�: Ekrandan se�ilen �okludo�rular�n ad� bilgisini al�p
'      �okludo�rular�n vt kodu  bilgisine yazmaktad�r.
' GIRDILER :�oklu dogrular
' UYARI : Secim menusu kullan�larak istenilen cokludogrular secilir.
'
'
Sub Main
Dim i,j,o,SEL,K
  with Netcad
    set SEL = .NewSelectionSet
    set o = .NewObject
    if SEL.SELECT("'VT' Kodlar� 'Ad' Olarak Atanacak �okluDo�rular� Se�in",array(opline)) then
      for i = 0 to SEL.NE-1
        j = SEL.GetSelectedObject(i, o)
       o.objname=o.pname
        .putobject j, o
      next
      SEL.RedrawAndRewind
    end if
    set SEL = nothing
    set o = nothing
  end with
end sub





'Tarih : 30/06/2003 V1.00
'Ama� : Ekrandan se�ilen alan objelerinin ADI,VT veya ALAN Bilgisinin "/" i�aretinin
'       solu veya sa��ndaki yaz�y� kapal� objenin i�ine yazmak.
'       Ad� 101/12 olan parselinin parsel no'sunu 12 olarak kapal� alan i�ine yazar.
'Girdi :Kapal� alan - �okludogru
'Uyar� :Kodlamada  yaz�(0) ise "/" i�aretinin solundaki yaz�y�
'       yaz�(1) ise "/" i�aretinin sag�ndaki yaz�y� kapal� objenin i�ine yazar


SUB Main
DIM ss,o,i,j,oo,p,sel,poly,tabaka,yazi   ,bd, secenek
DIM kt() ,t()

With netcad
  set SEL = .NewSelectionSet             ' Yeni kume yarat
  set o = .NewObject
  set poly=.newpoly
'---------------------------------secenek sor ---------------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetRadio  "Secenek","Ada Poligon Tabakas�", "Ad�|VtKodu|Alan",0
  if BD.showmodal then
    secenek=BD.ValueByName("secenek")
   Else
    Exit Sub
  End if
  set BD = Nothing
'------------------------------------------------------------------------------------------
   .setparam beginblock,true
   if SEL.SELECT("Kapal� �oklu do�rular� se�",array(opline)) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy

      tabaka=.LayerNameOf(o.tabaka)&"_YAZI"
      set poly=.getplineext(o)
      if secenek=0 then yazi=split(o.pname,"/")
      if secenek=1 then yazi=o.objname
      if secenek=2 then yazi=round(poly.area,3)
      .AddObject (.MakeText (poly.CenterOfMass, yazi(1)  , 0,0, 2,0,"M",.CreateLayer(tabaka, blue)))
    next
    .setparam endblock,true
    .NetcadCommand("REDRAW")
    set sel = nothing
    set poly = nothing               ' obje icin aldigimiz memory'i geri ver
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if
End With
END SUB

Sub Main
       Dim p, i,ToplamY, ToplamX, ToplamZ
       Dim AgMerY, AgMerX, AgMerZ

   with Netcad
          set p = .NewPoly
          if .GetPolygon("Kapal� Alan Se�",p) then
             for i=0 to p.num-2
                ToplamY= p.cor(i).y + ToplamY
                ToplamX= p.cor(i).x + ToplamX
                ToplamZ= p.cor(i).z + ToplamZ
             next

         AgMerY = Round(ToplamY / (p.num-1),3)
             AgMerX = Round(ToplamX / (p.num-1),3)
             AgMerZ = Round(ToplamZ / (p.num-1),3)

         msgbox "Nokta Say�s� = " & p.num-1 & chr(13) & "A��rl�k Merkezi =" & AgmerY & ", " & AgMerX & ", " & AgMerZ
         end if
         

     set p = nothing
     end with
End Sub


'Ama� : ALAN OBJELER� MERKEZ�NE ALAN TABAKASINDA NOKTA OBJES� �RETME
'
' Not  :
' Yazan: Erc�ment KORKMAZ
' Tarih: 10.07.2006

Sub main
Dim i,j,o,p, nok, ss, gnok
  ss =1
   with Netcad
     set nok = .Newc(0, 0, 0)
      for i = 0 to .numobject-1
        set o = .getobject(i)
        if o.tag = opline then
          .drawobject o, Black
          set p = .getplineext(o)
           set nok =    p.CenterOfMass
           set gnok = .MakePoint(nok, ss, "code", o.Tabaka)
           .AddObject gnok
          set p = nothing
        ss = ss+1
        end if
        set o = nothing
      next
   end with
End Sub




'Tarih    : 25.10.2003
'Ama�     : B�R KAPALI ALAN ���NDEK� ��FT YAZILARI SILER
'Girdiler : Kapal� Alan -�okludogru  - KAD_PARSEL_PL tabakas�nda
'           Text  - PARSEL_NO tabakas�nda
'UYARI    :ICINDE YAZI YOKSA,YAZILAR BIRBIRINDEN FARKLI ISE VE CIFT YAZIDAN BIRINI SILMIS
'          ISE �LG�L� PARSEL SORUNLU TABAKASINA ALINIR.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Parsel","Ada Poligon Tabakas�", "KAD_PARSEL_PL", 15
  BD.GetString  "Yazi","Parsel No Tabakas�", "PARSEL_NO", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.InPoly(by.p1) then
             s=s+1
             if s>1 then
               if iy=by.s then .DelObject .CurObjPos, by
             end if
             iy=by.s
           end if
         end if
       Loop

       if s>0 then
         if s=1 then
           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
          else
           parsel.tabaka=.CreateLayer("sorunlu",7)
           .PutObject i, parsel
           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
         end if
        else
         parsel.tabaka=.CreateLayer("sorunlu",7)
         .PutObject i, parsel
         .ResetFilter                ' Filitre uygulamayi birak.
         s=0
         mode=0
         iy=""
       end if
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


'TARIH : 27/12/2001    V1.00
'AMA�  : 3D DE BINALARI GORUNTULEMEK AMACI ILE KULLANILIR
'        BINA MESKUN KAPALI ALANI ICINDEKI BINA KATADEDI YAZISINI VERILEN DEGERLE CARPARAK
'        OBJENIN KALINLIK BILGISINE YAZAR
'GIRDILER : KAPALI_ALAN-COKLUDOGRU-BINA_MESKUN TABAKASINDA
'           TEXT - YAZI- BINA_KATADEDI TABAKASINDA
'           KAT YUKSEKLIGI - 2.75 (ortalama bir kat�n yukseklik degeri)
'UAYARILAR : Binalar�n tabakalar�n� katadedi degerine gore degi�tirir.
'18/01/2005 : D�zenledi�i alanlara zoom yapma eklendi ' Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���

Function Intersect_Lines(x1,y1, x2, y2,u1, v1, u2, v2)

dim a1,b1,c1,a2,b2,c2,det_inv
dim m1,m2
dim xi,yi
dim p

if (x2-x1)=0 then b1=1e+10 else b1 = (y2-y1)/(x2-x1)
if (u2-u1)=0 then b2=1e+10 else b2 = (v2-v1)/(u2-u1)

a1 = y1-b1*x1
a2 = v1-b2*u1 
xi = - (a1-a2)/(b1-b2)
yi = a1+b1*xi

if (x1-xi)*(xi-x2)>=0 AND (u1-xi)*(xi-u2)>=0 AND _
   (y1-yi)*(yi-y2)>=0 AND (v1-yi)*(yi-v2)>=0  then
    Intersect_lines=True
   else
    Intersect_lines=False
end if
End Function

'---------------------------------------------------------------------------------
Function inside(p , poly)
Dim i, j, count
Dim lt, lp
Dim c1,c2
set c1=netcad.NewObject
set c2=netcad.NewObject
   count = 0
   j = 0
   c2.p1.x=p.x
   c2.p1.y=p.y
   c2.p2.x=999099999999999999999999999
   c2.p2.y=999999099999999999999999999
   For i = 0 To poly.num-1
       c1.p1 = poly.Cor(i)
       if i=poly.num-1 then c1.p2 = poly.Cor(0) else  c1.p2 = poly.Cor(i + 1)
       If intersect_Lines (c1.p1.x,c1.p1.y,c1.p2.x,c1.p2.y, c2.p1.x,c2.p1.y,c2.p2.x,c2.p2.y) Then
          count = count + 1
          end if
   Next
   inside = (count Mod 2 <> 0)
End Function

'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka, KatYuksekligi
Dim Ada
Dim world

with netcad
for j=1 to 50
 .CreateLayer j, red
next



'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Ada","Alan Poligon Tabakas�", "BINA_MESKUN", 15
  BD.GetString  "AdaNo","Yaz� Tabakas�", "BINA_KATADEDI", 15
  BD.GetFloat "KatYuksekligi","Kat Y�ksekli�i",2.75,2

  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "

  KatYuksekligi=BD.ValueByName("KatYuksekligi")

  if BD.showmodal then
    AdaTabaka=.FoundLayer(BD.ValueByName("Ada"))
    AdaNoTabaka=.FoundLayer(BD.ValueByName("AdaNo"))
    if  AdaTabaka=-1 or AdaNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
  Else
    Exit Sub
  End if
  set BD = Nothing

'-------------------------Dialog Son-------------------------------------------------

'******************* Poligona i�indeki yaz�y� ata *************
for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        .BackMessage
        .SetMessage i
        set ada = .getobject(i)         ' i. objeyi al
        if ada.tag = opline and ada.tabaka=AdaTabaka  then        ' Coklu dogrumu ?
          .DrawObject ada, blue
          .SetFilter .ObjectExtends(ada), array(AdaNoTabaka), array(otext)          'Filitre uygula
          Do
            set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
            if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
              exit do                ' Bu durumda donguyu durdur
              else
              if inside(o.p1, .GetPlineExt(ada)) then
                 if o.s>50 then msgbox "Bulunan yazi de�eri= " & o.s
                 set world = ada.limits
                 .SetCurrentWindow world, true
                 ada.tabaka=.FoundLayer(o.s)
                 ada.thicknes=o.S*KatYuksekligi
                 .PutObject i, ada
              end if
            end if
            Loop
          .ResetFilter                ' Filitre uygulamayi birak.
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
next
set ada=nothing

end with
END SUB

'AMA� : MAPINFODAN ALINAN TEXT DOSYASININ ISTENILEN KOLONUNU BILGILERINE GORE BLOK
'       OLARAK ATAMAK
'GIRDILER : TEXT DOSYASI
'UYARILAR : TXT DOSYANIN PATH'I YAZILIR
'           TXT DOSYASINDAKI BILGILERE GORE KOORDINAT VE BLOK URETILIR
'----------------------------------------------------------------------------------------------

Sub Main
   Const ForReading = 1, ForWriting = 2, ForAppending = 8
   Dim fso, f
   Dim Satir  ,c1  ,o

Set fso = CreateObject("Scripting.FileSystemObject")
Set f = fso.OpenTextFile("E:\MAPINFO\CALISMA\PENDIK.TXT", ForReading, True)

Do While Not f.AtEndOfStream
 Satir=Split(F.ReadLine,",")

' objelerin netcad de olu�turulmas�
with netcad
  set c1=.newc(satir(1),satir(2),0)

  set o=.MakeBlock ("KAYA",c1, 0, 0, 1,1,.CreateLayer("OKU",brown))
  o.objname=satir(0)
  .AddObject o

end with

Loop
f.Close

End Sub


   '
'----------------------------------------------------------------------------------------------
  'AMA� : MAPINFODAN ALINAN TEXT DOSYASININ ISTENILEN KOLONUNUN YAZI DIGER KOLONLAINDA KOORDINAT
'        BILGISI OLARAK OLARAK ATAMAK
'GIRDILER : TEXT DOSYASI
'UYARILAR : TXT DOSYANIN PATH'I YAZILIR
'         TXT DOSYASINDAKI BILGILERE GORE KOORDINAT VE YAZI URETILIR
'         BIR TEXT DOSYASINDAN ISTENILEN SUTUN YAZI DIGER IKI SUTUN YAZININ YERLESECEGI KOORDINATI IFADE EDER
Sub Main
   Const ForReading = 1, ForWriting = 2, ForAppending = 8
   Dim fso, f
   Dim Satir  ,c1

Set fso = CreateObject("Scripting.FileSystemObject")
Set f = fso.OpenTextFile("E:\MAPINFO\CALISMA\PENDIK.txt", ForReading, True)

Do While Not f.AtEndOfStream
 Satir=Split(F.ReadLine,",")

' objelerin netcad de olu�turulmas�
with netcad
  set c1=.newc(satir(1),satir(2),0)

  .AddObject .MakeText(c1, satir(0), 0,0, 1,0,"M",.CreateLayer("OKU",brown))
end with

Loop
f.Close

End Sub


' TARIH         : 24.01.2005
' AMA�          : Proje-Farkl� Kaydet-DXF ile yaz�lan dxf dosyalar�n�n
'                 Autocad ortam�nda a��lmas�nda ya�anan sorunlar� giderir.
'                 NCZ dosyay� sorunsuz olarak saklanmas� i�in d�zenler.
'
' GEREKL�L�KLER : NCMacro 4.0.015 ve �st� versiyon.
'
' HAZIRLAYAN    : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
sub main
  'obje Rengi2TabakaRengi
  objCol2LayerCol
  'Tabaka Rengi Kontrll�
  setLayerColor
  'Tabaka Ad� Kontrol�
  CorrectLayerNames
  'Blok Ad� Kontrol�
  checkBlockNames
  msgbox "Proje D�zenlendi..."
end sub

sub SetLayerColor
dim i,j
  with nclayermanager
    for i = 0 to .numlayer-1
      if .layer(i).color = 80 or _
         .layer(i).color = 15 or _
         .layer(i).color = 0 or _
         .layer(i).color = 16  then
         .layer(i).color =  4
     end if
    next
  end with
end sub

sub objCol2LayerCol
dim i,obj
  with netcad
    set obj = .getobject(i)
    if obj.renk <> 0 then
      obj.renk = 0
    end if
    set obj = nothing
  end with
end sub

sub CorrectLayerNames
dim n,i
  with nclayermanager
    for i = 0 to .numlayer-1
      n = ucase(.layer(i).name)
      n = replace(n,"�","U")
      n = replace(n,"�","g")
      n = replace(n,"�","I")
      n = replace(n,"�","I")
      n = replace(n,"�","s")
      n = replace(n,"�","c")
      n = replace(n,"�","o")
      n = replace(n,".","_")
      .layer(i).name = UCase(n)
    next
  end with
end sub

sub checkBlockNames
dim i,obj
  with netcad
    .setfilter nothing,array(),array(oblock)
    set obj = .newobject
    while .getnextobject2(obj)
     if obj.pname = "" then
       msgbox "�simsiz Blok Var. L�tfen �simsiz Olan Blok Objesine Bir �sim Veriniz..."
       .resetFilter
       exit sub
     end if
    wend
    .resetfilter
  end with
end sub

<kml xmlns="http://earth.google.com/kml/2.0">
<Document>
<Style id="default0">
<IconStyle>
<scale>1</scale>
</IconStyle>
</Style>
<Folder>
<name>Netcad Verileri</name>

<ScreenOverlay>
<name>www.netcad.com.tr</name>
<visibility>1</visibility>
<Icon>
<href>http://download.netcadportal.com/images/netcad_logo.jpg</href>
</Icon>
<overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/>
<screenXY x="0" y="1" xunits="fraction" yunits="fraction"/>
<size x="0" y="0" xunits="pixels" yunits="pixels"/>
</ScreenOverlay>

<Folder><name>KAM_ALN</name>
<Placemark>
<name>B21</name>
<styleUrl>#default0</styleUrl>
<visibility>1</visibility>
<open>0</open>
<Style><LineStyle><color>FF0000FF</color></LineStyle><PolyStyle><color>FF0000FF</color></PolyStyle></Style>
<LineString>
<extrude>1</extrude>
<tessellate>1</tessellate>
<altitudeMode>relativeToGround</altitudeMode>
<coordinates>
44.0359216889781,37.7334333906013,4.95692380550015E+140 44.0360897835897,37.7334543337878,4.95692380550015E+140 44.0362578783011,37.7334752767359,4.95692380550015E+140 44.0362051386611,37.7337422783962,4.95692380550015E+140 44.0363732340579,37.733763221188,4.95692380550015E+140 44.036443574509,37.7334071084849,4.95692380550015E+140 44.0359392909611,37.7333442797233,4.95692380550015E+140 44.0359216889781,37.7334333906013,4.95692380550015E+140 
</coordinates>
</LineString>
</Placemark>

</Folder>
</Folder>
</Document>
</kml>


sub main
dim yaz,c,a2,layerno,bd,bz,yaz1,a1,d,a

 with Netcad

     a=.getparam(94)
layerno=.CREATELAYER("bas�sonu" ,0)

     set BD = Netcad.NewBDialog("BA�LANGI� KM")

    BD.Getstring "item","YAZI","",50


    if BD.showmodal then


set c = .newc(0,0,0)
set yaz=.maketext(c, "BA�LANGI� KM",0,0,(a/1000)*3,0,"1",layerno)
.WalkObject "yaz� i�in Yer Se�",c,-1,yaz
yaz.p1 = c
.addobject(yaz)
c.x=c.x-(3*a/1000)
set a2=.maketext(c,BD.ValueByName("item"),0,0,(a/1000)*3,0,"1",layerno)
.addobject(a2)


end if


        set Bz = Netcad.NewBDialog("B�T�� KM")

    Bz.Getstring "item","YAZI","",50

    if Bz.showmodal then


set d = .newc(0,0,0)
'set yaz1=.maketext(d, "BA�LANGI� KM",0,0,(a/1000)*3,0,"1",layerno)
set yaz1=.maketext(d,"B�T�� KM",0,0,(a/1000)*3,0,"1",layerno)
.WalkObject "yaz� i�in Yer Se�",d,-1,yaz1
yaz1.p1 = d
.addobject(yaz1)
d.x=d.x-(3*a/1000)
set a1=.maketext(d,Bz.ValueByName("item"),0,0,(a/1000)*3,0,"1",layerno)
.addobject(a1)




 end if
end with
end sub




 Sub Main
   with netcad

       daimigez
       gecicigez
       yazinoktagez
       KDN_OTELE
       KDN_OTELE_1
       
       
       
       
        dim obj

       .setfilter nothing, array(.foundlayer("TXT_ALANLAR_1"),_
                                 .foundlayer("PAR_NO_ADA")),array(otext)
                DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE
                    .DRAWOBJECT OBJ,10
                obj.s=replace(obj.s,"Daimi �rt","�ST HAKKI")
                obj.s=replace(obj.s,"ADA:","")
               .PUTOBJECT .CUROBJPOS,OBJ
                end if

                loop

       .resetfilter
       
       .setfilter nothing, array(.foundlayer("NOT_IRTIFA")),array(otext)
                       DO
                       SET OBJ=.GETNEXTOBJECT
       
                       IF OBJ IS NOTHING THEN
                          EXIT DO
                       ELSE
                           .DRAWOBJECT OBJ,10
                       obj.sc=1.4
                     
                      .PUTOBJECT .CUROBJPOS,OBJ
                       end if
       
                       loop
       
       .resetfilter
       
       
       Dim i,j,o,p,obj10,pos,w,s,yazi,a,b,koord,x
       
         
             set obj10 = .Newobject
       
           .setfilter nothing,array(.FOUNDLAYER("ALN_DAIMI_IRTIFA")), array(opline)
              Do
                set obj10=.getnextobject
                if obj10 is nothing then
                   exit do
                else
       
                 set s=.GetPlineExt(obj10)
                 set w=.newworld(0,0,0,0) :   w.BuildRegion s.CenterOfMass,200,200
                 .SetFilter w, array(.foundlayer("ALN_GECICI_IRTIFA")),array(opline)
       
                 while .GetNextObject2(obj10)
       
                       .drawobject obj10, Red
                       obj10.tabaka=.createlayer("ALN_GECICI_IRTIFA_", red)
                        .closelayer(obj10.tabaka)
                       .putObject .curobjpos, obj10
                 wend
                 .ResetFilter
               end if
               loop
               .ResetFilter
               
               
                set obj10 = .Newobject
             for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
               set o = .getobject(i)
                  if o.tag=opline and o.tabaka=.foundlayer("ALN_GECICI_IRTIFA") then
                   set s=.GetPlineExt(o)
                   set w=.newworld(0,0,0,0) :   w.BuildRegion s.CenterOfMass,100,100
                    .SetFilter w, array(),array()
       
                 while .GetNextObject2(obj10)
                       .drawobject obj10, blue
                       .putObject .curobjpos, obj10
       
                 wend
                  .ResetFilter
                     set yazi= .MakeText(s.CenterOfMass,"SiL",0,0,200,0,"L",.createlayer("S�L",4))
                           .addobject(yazi)
                  end if
                  next
       
       
 

     

dim wLayerIndex1 
dim wLayerIndex2 
dim wLayerIndex3 
dim wLayerIndex4 
dim wLayerIndex5 
dim wLayerIndex6 
dim wLayerIndex7 
dim wLayerIndex8 
dim wLayerIndex9 
dim wLayerIndex10
dim wLayerIndex11
dim wLayerIndex12
dim wLayerIndex13
dim wLayerIndex14
dim wLayerIndex15
dim wLayerIndex16
dim wlayerindex17
 wLayerIndex1 =  .FoundLayer("GECICI_YAZI")
 wLayerIndex2 =  .FoundLayer("KR_B_ADANO")
 wLayerIndex3 =  .FoundLayer("KR_B_KURUMNO")
 wLayerIndex4 =  .FoundLayer("KR_B_MAHAL")
 wLayerIndex5 =  .FoundLayer("KR_B_MEVKI")
 wLayerIndex6 =  .FoundLayer("KR_B_PAFTA")
 wLayerIndex7 =  .FoundLayer("KR_B_PARNO")
 wLayerIndex8 =  .FoundLayer("KR_CERCEVE")
 wLayerIndex9 =  .FoundLayer("KR_ILCE")
 wLayerIndex10 =  .FoundLayer("KR_ILI")
 wLayerIndex11 =  .FoundLayer("KR_KLISE")
 wLayerIndex12 =  .FoundLayer("KR_YAZI")
 wLayerIndex13 =  .FoundLayer("KROK�_YAZI")
 wLayerIndex14 =  .FoundLayer("PAR_NO_GEC")




 with NCLayerManager
         if wLayerIndex1 < 0 then
         else
  .Delete .Find("GECICI_YAZI"), TRUE
         end if

 if wLayerIndex2 < 0 then
         else
  .Delete .Find("KR_B_ADANO"), TRUE
    end if
  
   if wLayerIndex3 < 0 then
         else
  .Delete .Find("KR_B_KURUMNO"), TRUE
  end if
  
   if wLayerIndex4 < 0 then
         else
  .Delete .Find("KR_B_MAHAL"), TRUE
  end if
  
   if wLayerIndex5 < 0 then
         else
  
  .Delete .Find("KR_B_MEVKI"), TRUE
  end if

   if wLayerIndex6 < 0 then
         else
  
  .Delete .Find("KR_B_PAFTA"), TRUE
  end if

   if wLayerIndex7 < 0 then
         else
  
  .Delete .Find("KR_B_PARNO"), TRUE
  end if

   if wLayerIndex8 < 0 then
         else
  
  .Delete .Find("KR_CERCEVE"), TRUE
  end if

   if wLayerIndex9 < 0 then
         else
  
  .Delete .Find("KR_ILCE"), TRUE
  end if

   if wLayerIndex10 < 0 then
         else
  
  .Delete .Find("KR_ILI"), TRUE
  end if

   if wLayerIndex11 < 0 then
         else
  
  .Delete .Find("KR_KLISE"), TRUE
  end if

   if wLayerIndex12 < 0 then
         else
  
  .Delete .Find("KR_YAZI"), TRUE
  end if

   if wLayerIndex13 < 0 then
         else
  
  .Delete .Find("KROK�_YAZI"), TRUE
  end if

   if wLayerIndex14 < 0 then
         else
  
  .Delete .Find("PAR_NO_GEC"), TRUE
  end if








end with

end with



End Sub






Sub daimigez
dim obj3
dim poly
dim i


with Netcad

              .SetFilter nothing, ARRAY(.foundlayer("ALN_DAIMI_IRTIFA")), ARRAY(opline)

            DO
                SET OBJ3=.GETNEXTOBJECT

                IF OBJ3 IS NOTHING THEN
                   EXIT DO
                ELSE

             set poly =obj3.getobjectaspline
              'For i=0 to poly.num-1


                      noktayaziayir poly


             'next
           end if
           .PUTOBJECT .CUROBJPOS,OBJ3

           loop
            .resetfilter

           end with
end sub


sub noktayaziayir(poly)
          dim w
          dim obj
          dim i
          with netcad
               'set w=.newworld(0,0,0,0) :   w.BuildRegion c,0.02,0.02

                .setfilter nothing,array(.FOUNDLAYER("GEC_IRTIFA")),ARRAY(oline)

            DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE

                  if poly.onpoly(true,obj.p1) and poly.onpoly(true,obj.p2) then



                  'obj.tabaka=.createlayer("DAIMIIRTNOKTAYAZI",3)
                 .delOBJECT .CUROBJPOS,OBJ
                   end if


                 end if
           LOOP
           .resetfilter
           end with

end sub

Sub gecicigez
dim obj3
dim poly
dim i
dim nadi
dim obj4


with Netcad

              .SetFilter nothing, ARRAY(.foundlayer("GEC_IRTIFA")), ARRAY(oline)

            DO
                SET OBJ3=.GETNEXTOBJECT

                IF OBJ3 IS NOTHING THEN
                   EXIT DO
                ELSE
                    'nadi=obj3.pname
                    'set obj4=obj3



                      noktatabakadegistir obj3



           end if
  .PUTOBJECT .CUROBJPOS,OBJ3

           loop
            .resetfilter

           end with
end sub

 sub noktatabakadegistir(obj3)
          dim w
          dim obj
          dim poly



          with netcad



                .setfilter nothing,array(.foundlayer("YENINOKTA")),ARRAY(opoint)

            DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE

                if   obj3.p1.x=obj.p1.x or obj3.p2.x=obj.p1.x then

                    obj.tabaka=.createlayer("GECICIIRTNOKTA",5)
                    .closelayer(obj3.tabaka)


                 .PUTOBJECT .CUROBJPOS,OBJ


                   end if
                 end if
           LOOP
           .resetfilter
           end with

end sub

Sub yazinoktagez
dim obj3
dim poly
dim i
dim nadi
dim obj4
dim c


with Netcad

              .SetFilter nothing, ARRAY(.foundlayer("GECICIIRTNOKTA")), ARRAY(opoint)
            DO
                SET OBJ3=.GETNEXTOBJECT

                IF OBJ3 IS NOTHING THEN
                   EXIT DO
                ELSE
                    nadi=obj3.pname




                      noktayazisil nadi,obj3



           end if
           .PUTOBJECT .CUROBJPOS,OBJ3

           loop
            .resetfilter
           end with
end sub

 sub noktayazisil(nadi,obj3)

          dim obj
          dim poly
          dim i




          with netcad
          Dim w : set w=.newworld(0,0,0,0) :   w.BuildRegion obj3.p1,20,20


                .setfilter w,array(.foundlayer("KOSE_NO_IRT"),_
                                   .foundlayer("KOSE_NO_PAR")),ARRAY(otext)

            DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE
                if nadi=obj.s  then

                    .DRAWOBJEcT OBJ ,  14
                    .DRAWOBJEcT OBJ3 , 14
                   obj.tabaka=.createlayer("GECICIIRTYAZI",5)
                   .closelayer(obj.tabaka)
                    .closelayer(obj3.tabaka)
                 .PUTOBJECT .CUROBJPOS,OBJ

                end if

                 end if
           LOOP
           .resetfilter
           end with

end sub






Sub KDN_OTELE

dim obj
dim layer
dim txt
dim pos

dim a
dim i


with netcad


   .setfilter nothing,array(.foundlayer("TXT_ALANLAR")),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if



       pos=Instr(1, obj.s,"/I")

       if pos > 0 then

              dene obj

        end if

       loop
   .resetfilter
end with

end sub




sub dene(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y,obj.p1.x,obj.p1.y+100,obj.p1.x-50)
.setfilter w,array(.foundlayer("TXT_ALANLAR")),array(otext)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if

           obj1.tabaka=.createlayer("TXT_ALANLAR_1",5)
            .PUTOBJECT .CUROBJPOS,OBJ1





           loop
 .resetfilter
 end with
 end sub

 
Sub KDN_OTELE_1

dim obj
dim layer
dim txt
dim pos

dim a
dim i


with netcad


   .setfilter nothing,array(.foundlayer("TXT_ALANLAR")),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if



       pos=Instr(1, obj.s,"/G")

       if pos > 0 then

              dene1 obj

        end if

       loop
   .resetfilter
end with

end sub




sub dene1(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y,obj.p1.x,obj.p1.y+100,obj.p1.x-100)
.setfilter w,array(.foundlayer("TXT_ALANLAR")),array(otext)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if

          obj1.tabaka=.createlayer("TXT_ALANLAR_2",10)
          .PUTOBJECT .CUROBJPOS,OBJ1
          .CLOSELAYER(OBJ1.TABAKA)





           loop
 .resetfilter
 end with
 end sub




' Yazan : 
' Tarih : 27.01.2014
' A��klama : 

Sub Main

dim limitGENEL

 with netcad
 set limitGENEL=.newworld(0,0,0,0)

  if .GetRegion(limitGENEL) then




Dim obj

        .setfilter limitGENEL,array(.foundlayer("BEY_INCE")),array(oline)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if

               if obj.length(0) > 15.24 and obj.length(0) < 15.25  then

               obj.p1.y =  obj.p1.y -12
               obj.p2.y =  obj.p2.y -12
                .putObject .curobjpos, obj
                 end if

         loop
 .resetfilter
                






   dim obj2
        .setfilter limitGENEL,array(.foundlayer("BEY_YAZI")),array(otext)
       do
         set obj2=.getnextobject

         if obj2 is nothing then
            exit do
         else
            end if

               if obj2.s ="�L�" then

               obj2.p1.y =  obj2.p1.y -  12

                .putObject .curobjpos, obj2
                    hizzala obj2
                 end if

                  if obj2.s ="�L�ES�" then

               obj2.p1.y =  obj2.p1.y -  15

                .putObject .curobjpos, obj2
                    hizzala obj2
                 end if

                  if obj2.s ="Mahalle veya k�y�" then

               obj2.p1.y =  obj2.p1.y -  15

                .putObject .curobjpos, obj2
                    hizzala obj2
                 end if


                    if obj2.s ="Mevkii" then

               obj2.p1.y =  obj2.p1.y -  15

                .putObject .curobjpos, obj2
                    hizzala obj2
                 end if








         loop
 .resetfilter
                









        .setfilter limitGENEL,array(.foundlayer("BEY_YAZI")),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if

               if obj.s="Mevkii" then
                     mevkii obj
                 end if

         loop
 .resetfilter




    dim yazi1

        .setfilter limitGENEL,array(.foundlayer("LEJANT_PAFTA")),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
           end if
                  obj.p1.x=obj.p1.x +29
                  obj.p1.y=obj.p1.y +155


             set yazi1= .MakeText(obj.p1,obj.s,0,0,obj.sc,0,0,.createlayer("LEJANT_PAFTA_1",0))

              .addobject yazi1

         loop
 .resetfilter




   dim yazi2,yazi3

        .setfilter limitGENEL,array(.foundlayer("BEY_YAZI")),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
           end if
           obj.s=replace(obj.s,"merkezde doldurulacakt�r","merkez/b�lge m�d�rl��� taraf�ndan")
    .putobject .curobjpos,obj


           if mid(obj.s,1,2)= "Bu" then
                obj.p1.x=obj.p1.x+83
               set yazi3=.MakeText(obj.p1,"Poligon ve K��e Koordinatlar�,A��Mesafe Bilgileri",0,0,obj.sc,0,0,obj.tabaka)
                   .addobject yazi3
                   ciz obj


            end if

           if mid(obj.s,1,2)= "Bu" then
                obj.p1.x=obj.p1.x-83
                obj.p1.y=obj.p1.y+70
               set yazi2=.MakeText(obj.p1,"doldurulacakt�r",0,0,obj.sc,0,0,obj.tabaka)
                   .addobject yazi2

            end if





           loop





  end if

 end with
 end sub


























sub mevkii(obj)
dim obj1,cizgi,yazi
 with Netcad
   Dim w : set w=.newworld(obj.p1.y,obj.p1.x,obj.p1.y-20,obj.p1.x)
        .setfilter w,array(.foundlayer("BEY_INCE")),array(oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
                obj1.p1.y=obj1.p1.y+28
                obj1.p2.y=obj1.p2.y+28
              set cizgi=.MakeLine(obj1.p1,obj1.p2,obj1.tabaka,0,0)
              .addobject cizgi

                obj.p1.y=obj.p1.y+30

              set yazi= .MakeText(obj.p1,"Pafta no",0,0,obj.sc,0,0,obj.tabaka)

              .addobject yazi

         loop
 .resetfilter
                
  end with

  end sub





  sub hizzala(obj2)
  dim obj1
    with Netcad
    Dim w : set w=.newworld(obj2.p1.y,obj2.p1.x,obj2.p1.y+20,obj2.p1.x-20)
        .setfilter w,array(.foundlayer("LEJANT_BILGI")),array(otext)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if



               obj1.p1.y =  obj2.p1.y
               .putObject .curobjpos, obj1






         loop
 .resetfilter

  end with
 end sub




 sub ciz(obj)
dim obj1,cizgi,yazi
 with Netcad
   Dim w : set w=.newworld(obj.p1.y,obj.p1.x,obj.p1.y,obj.p1.x-10)
        .setfilter w,array(.foundlayer("BEY_INCE")),array(oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
                obj1.p1.x=obj1.p1.x+4.5
                obj1.p2.x=obj1.p2.x+4.5
              set cizgi=.MakeLine(obj1.p1,obj1.p2,obj1.tabaka,0,0)
              .addobject cizgi


         loop
 .resetfilter
                
  end with

  end sub



'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      h.tabaka=b.tabaka
      h.W=b.W
      h.LT=b.LT
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      h.s=b.s
      'h.p1.y=b.p1.y
      'h.sc=b.sc
      'h.p1.x=b.p1.x
      'h.angle=b.angle
      ' h.tabaka=b.tabaka
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      'h.sc=b.sc
      'h.s=b.s
      'h.p1.X=b.p1.X
      h.angle=b.angle
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


 ' Halihazir datalarda sundarmalar�n 3D icin temizlenmesi ; kapali alan ismi ve icindeki yazinin tabakasi verilerek islem yap.
 ' Pafta kenarlar�nda kapatilan binalar�n icindeki cift yazilari siler
 ' icinde yazi olmayan binalar� sorunlu tabakasina alir (Kat adedi olmayanlar secilmis olur boylece sundurmalar secilir.
 '----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("sundurma ve pafta kenarlar�ndaki cift yazi temizle")          ' yeni dialog yarat
  BD.GetString  "Parsel","BINA", "BINA_MESKUN", 15
  BD.GetString  "Yazi","KAT_ADEDI", "BINA_KATADEDI", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.InPoly(by.p1) then
             s=s+1
             if s>1 then
               if iy=by.s then .DelObject .CurObjPos, by
             end if
             iy=by.s
           end if
         end if
       Loop

       if s>0 then
         if s=1 then
           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
          else


           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
         end if
        else

         .ResetFilter                ' Filitre uygulamayi birak.
         s=0
         mode=0
         iy=""
       end if
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


'Ama� : Arazi kotu ile bina kotu kar��la�t�r�larak katy�ksekli�inin bulunmas�
'

Sub Main
Dim c,cc,i,j,k,z,z1,PL1,obj1,sel,tx,tx1,kat
Const ForReading = 1, ForWriting = 2
Dim fso, f
Set fso = CreateObject("Scripting.FileSystemObject")
Set f = fso.OpenTextFile("c:\katyuksekligi.txt", ForWriting, True)
f.WriteLine "BINA"&"   "&"ARAZI"&"  "&"FARK"&"  "&"KAT"

with Netcad
set SEL = .NewSelectionSet   ' Yeni kume yarat
set c=.NewC(0,0,0)
set obj1= .NewObject
if SEL.SELECT("KAT BILGILERI YAZILACAK OBJELERI SECINIZ",array(opline)) then  ' istenen turleri kumeye ekle
   for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
       j = SEL.GetSelectedObject(i, obj1)    ' objeyi ve gercek indeksini al
       set PL1=.GetPlineExt(obj1)
       set c=PL1.CenterOfMass
            z= .CalcZOf(c)
           z1=0
       for k = 0 to PL1.num-1 'Poly objesini olu�turam noktalar�n y�ksekliklerini topla
           z1=z1+PL1.cor(k).z
       next
       z1=z1/PL1.num   'Poly objesinin ortalama y�ksekli�inin bulunmas�
       kat=((z1-z)-0.6)/3 'poly objesinin �cgen modelden hesaplanan merkez kotu ile poly objesinin ortalama kotunun fark�ndan katy�ksekli�i
       if (Int(kat)+0.5)>kat then
          kat=Int(kat)
          else
          kat=Int(kat)+1
       end if
       if kat=0 then kat=1
       f.WriteLine ROUND(z1,2)&"   "&ROUND(z,2)&"  "&ROUND((z1-z),2)&"  "&kat
       set tx=.MakeText(c,kat,0,0,1,0,"M",0)
       .addobject(tx)
   next
end if
    set obj1=nothing
    set PL1=nothing
    set j = nothing
    set c = nothing
    set z=nothing
    set z1=nothing
  end with
End Sub




' TAR�H :  08/05/2001     V2.00
' AMA�:   Balastro yok etme makrosu
'         dogrular�n ucundaki dairelere g�re �al���r
'GIRDILER :  Daire (balastro) ve dogrular (parsel hatlar�)
' UYARI : Program tabaka filtresi uygulamaz projedeki t�m do�ru, daire objelerini kullan�r.
'         ��eleme ba�lamadan �nce sadece i�lem g�recek nesneleri projenizde bulundurun.
' NetCAD �stanbul B�lge M�d�rl���

SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
dim csay,rsay,dy,dx,c,r,maxob
dim p1,p2,bl
dim BD
dim dairex(),dairey()
ds=0

with netcad
'--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = Netcad.NewBDialog("Blastro Yoket V.2.00   R:08/05/2001")                ' yeni dialog yarat
  BD.GetFloat   "BlastroR","Blastro yar��ap�n� Girin",1.01 , 3
  BD.GetInteger "MaxOb","Max obje say�s�",30
  BD.PutPrompt  "Program tabaka filtresi uygulamaz !"                              ' Aciklama Yazdir.
  BD.PutPrompt  "Projedeki t�m do�ru, daire objelerini kullan�r !"
  BD.PutPrompt  "��eleme ba�lamadan �nce sadece i�lem "
  BD.PutPrompt  "g�recek nesneleri projenizde bulundurun. "
  BD.PutPrompt  " "
  if BD.showmodal then
    d=BD.ValueByName("BlastroR")      ' kullanicinin girdigi degerleri ata
    MaxOb=BD.ValueByName("MaxOb")
  Else
    Exit Sub
  End if
  set BD = Nothing
'-------------------------Dialog Son------------------------------------------------------------------

.SetFilter nothing, array(), array(ocircle)          'Filitre uygula
Do
  set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
  if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
    exit do                ' Bu durumda donguyu durdur
  else
    ds=ds+1
  end if
  set o = nothing               ' obje icin aldigimiz memory'i geri ver
Loop
.ResetFilter                   ' Filitre uygulamayi birak.
ReDim dairex(ds),dairey(ds)

csay= abs(sqr(ds/MaxOb))                                     ' Kolon say�s�
rsay= abs(sqr(ds/MaxOb))                                     ' Sat�r say�s�
if ds<1 then
  msgbox "Projede daire objesi bulunamad�..."
  exit sub
end if

set c1=.Newc(0, 0, 0)
set c2=.Newc(0, 0, 0)
.SelectPoint "Ba�lang�� Noktas� se�",c1,-1     ' ��lem alan� ba�lang�c�
.SelectPoint "Biti� Noktas� se�",c2,-1         ' ��lem alan� sonu
dy=(c2.y-c1.y)/csay                               ' Kolon aral���
dx=(c2.x-c1.x)/rsay                               ' Sat�r Aral���


for c=0 to csay-1
  for r=0 to rsay-1
    set reg=.NewWorld(c1.y+c*dy, c1.x+r*dx, c1.y+(c+1)*dy, c1.x+(r+1)*dx)
    set p1=.Newc(c1.y+c*dy, c1.x+r*dx, 0)
    set p2=.Newc(c1.y+(c+1)*dy, c1.x+(r+1)*dx, 0)
    set bl=.MakeRect (p1, p2, "",250)
    .DrawObject bl,11
    set bl=nothing
     ds=0
     .SetFilter reg, array(), array(ocircle)          'Filitre uygula
     Do
       set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
       if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
         exit do
       else                  ' Bu durumda donguyu durdur
         ds=ds+1
         dairey(ds)=o.p1.y
         dairex(ds)=o.p1.x
       end if
       set o = nothing               ' obje icin aldigimiz memory'i geri ver
     Loop
     .ResetFilter                   ' Filitre uygulamayi birak.

    .SetFilter reg, array(), array(oline)          'Filitre uygula
    Do
      set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
      if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
        exit do
      else                  ' Bu durumda donguyu durdur
        for  j = 1 to ds
          mes = Sqr ((dairex(j)-o.p1.x)^2+(dairey(j)-o.p1.y)^2)
          if mes < d then
            o.p1.x=dairex(j)
            o.p1.y=dairey(j)
            .putobject .CurObjPos,o
          end if
          mes = Sqr ((dairex(j)-o.p2.x)^2+(dairey(j)-o.p2.y)^2)
          if mes < d then
            o.p2.x=dairex(j)
            o.p2.y=dairey(j)
            .putobject .CurObjPos,o
          end if
        next
        lnum=lnum+1
        .BackMessage
        .SetMessage "c:"&c&" r:"&r&"  "&ds&"  ---->  "&lnum
      end if
      set o = nothing               ' obje icin aldigimiz memory'i geri ver
    Loop
    .ResetFilter                   ' Filitre uygulamayi birak.
  next
next
.BackMessage
end with
END SUB


' Yazan : 
' Tarih : 13.03.2014
' A��klama : 

Sub Main
Dim obj
  with Netcad

           .setfilter nothing,array(),array(opoint)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else




if obj.pname="" then
.delobject .curobjpos,obj

end if




end if

loop

.resetfilter


  end with
End Sub

sub main
dim obj
dim i
dim pos
dim TxtBaba
dim TxtMalik

dim LayerBaba
dim LayerMalik

dim otxt1
dim c
dim ar ,s,ST,st1




with netcad
     set c=.newc(0,0,0)

     .setfilter nothing ,array(.foundlayer("YAZI_MALIK"),.foundlayer("YAZI_BABA"),.foundlayer("YAZI_CINS")),array(otext)

        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else

                   st=""
                   ar=split(obj.s," ")

                   for i=0 to ubound(ar)
                      ST=ST & " " &  YAZI_DUZENLE(ar(i))
                   next





                    obj.s=st
                   .PutObject .CurObjPos, obj
                    .DrawObject obj, 16

           end if
        loop
     .resetfilter
end with
end sub

function YAZI_DUZENLE(s)
dim i,j,Count
dim B
dim Str
dim Chr


Str=""
B=1

s=trim(s)
count=len(s)

for i=1 to Count
    
Chr=Mid(s,i,1)

if B=1 then
  if chr="i" or chr="�" then
     chr="�"
     str=str & chr
     B=0
  elseif chr="�" or chr="I" then
     chr="I"
     str=str & chr
     B=0
  else
    Str=Str & Ucase(chr)
    B=0
  end if

else

  if chr="i" or chr="�" then
     chr="i"
     str=str & chr
  elseif chr="�" or chr="I" then
     chr="�"
     str=str & chr
  else
     Str= Str & Lcase(chr)
  end if

end if

next

YAZI_DUZENLE=STR
end function



function GetText1(txt)
dim Temp
dim pos

    pos=instr(1,txt,":")

    if pos>0 then
       temp=mid(txt,1,pos-1)
       gettext1=temp
       exit function
    end if

end function



function GetText2(txt)
dim Temp
dim pos

    pos=instr(1,txt,":")

    if pos>0 then
       temp=mid(txt,pos+1)
       gettext2=temp
       exit function
    end if
end function


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      h.sc=b.sc
      'h.s=b.s
      'h.p1.X=b.p1.X
      'h.angle=b.angle
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


'�ak��an yaz�lar� bulmak

Sub Main
Dim i,j,o,p,obj
   with Netcad
      set obj = .Newobject
      for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        set o = .getobject(i)         ' i. objeyi al
        if o.tag = otext then        ' yaz� m� ?
          .SetFilter o.Limits, array(),array()
          while .GetNextObject2(obj)
             if obj.tag=otext then
                .drawobject o, Red
                o.tabaka=1
             end if
          wend
          .ResetFilter
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
      next
   end with
End Sub




' Ama� : �oklu do�ru kotlar�n�n ��genleme �ncesi seyreltilmesi
'        ve yaz�lara kot noktalar� �retilmesi.
'Girdi : �okludogru - Egriler
'        Text - Yer kotu
'Uyar� : Ekranda Sadece bu objeleri a��k b�rak�n


'---------------------------- Coklu dogru sadelestirerek nokta uretir --------------------------
Sub CDogruSadelestir
Dim AtlamaSay, Point ,EgriKotAdim
Dim i,j,o,p
EgriKotAdim=50
AtlamaSay=10

with Netcad
  .SetFilter nothing, AcikTabakalar,array(opline)          'Filitre uygula
  Do
    set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
    if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
      exit do                ' Bu durumda donguyu durdur
     else
       set p = .getplineext(o)     ' objenin noktalarini tutan pline yapisini al
       if (p.cor(0).z mod EgriKotAdim )=0 then
         .AddObject  .MakePoint(p.cor(1), "T", "TIN",.FoundLayer("TIN") )
         .AddObject  .MakePoint(p.cor(p.num-2), "T", "TIN",.FoundLayer("TIN"))
         for j = 1 to p.num-2        ' coklu dogrunun herbir noktasi icin
           if (j mod AtlamaSay)=0 then .AddObject  .MakePoint(p.cor(j), "T","TIN" ,.FoundLayer("TIN"))
         next
         set p = nothing             ' noktalar icin aldigimiz memory'i geri ver
       end if
       set o = nothing               ' obje icin aldigimiz memory'i geri ver
    end if
  loop
end with
End Sub
'-----------------------------------------------------------------------------------------------------------
Sub KotYaziToNokta         ' Microstation dan gelen 123.12 kot yaz�lar�n�n '.' s�na kot noktas� atar.
Dim AtlamaSay, Point
Dim i,j,o,p

with Netcad
  .SetFilter nothing, AcikTabakalar,array(otext)          'Filitre uygula
  Do
    set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
    if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
      exit do                ' Bu durumda donguyu durdur
     else
       if o.s="." then .AddObject .MakePoint(o.p1, "T","TIN" ,.FoundLayer("TIN"))
       set o = nothing               ' obje icin aldigimiz memory'i geri ver
    end if
  loop
end with
End Sub
'----------------------------------------------------------------------------------------
Function AcikTabakalar
Dim i
Dim t()
  With netcad
    ReDim t(.NumLayers)
    For i=0 to .NumLayers-1        't() ye tabakalar� doldur
      if .IsLayerOpen(i) then t(i)=i else t(i)=-1
    Next
    AcikTabakalar=t
  End with
End Function


Sub Main
  With netcad

    msgbox "Sadece kotu �retilecek tabakalar� a��k b�kar�n.     "
    .CreateLayer "TIN", 11
    KotYaziToNokta
    CDogruSadelestir
  End With
End Sub

' BELNET VERITABANI ���N
'
' Ama� :  Binalar�n, tam olarak parsellerin i�inde olmad�klar� durumlarda
'         Spatial bina objesinin merkezi hangi spatial parsel objesinin
'         i�inde ise, onun PARSEL_ID'sini GEOBINA tablosunun PARSEL_ID
'         kolonuna atar
' Bar�� G�ral
' �stanbul B�lge M�d�rl���
sub main
dim sfParsel,sfBina,rsParsel,rsBina,esKolon
dim i,j,p,filter,c
  with netcad
   if not .EscPressed then
    i = 0
    sfParsel = "JEOLOJI" ' ADA S�n�f�
    sfBina = "PARSEL"  ' BINA S�n�f�
    esKolon = "ADA_ALAN" ' E�le�tirilecek Kolon

    set rsParsel = ncconman.finddatasetbyfc(false,sfparsel)
    set rsbina = ncconman.finddatasetbyfc(false,sfbina)
    rsbina.first
    'filtre haz�rlan�yor
    set filter = .NewDatasetFilter
    filter.FilterType = NC_GISDS_IncludePoint
    set p = .newpoly
    if rsbina.open then
      while not rsbina.eof
        set c = rsbina.geometry.p.CenterOfMass
        p.Clear
        p.AddCoor(c)
        filter.poly = p
        rsparsel.setfilter filter
          if rsParsel.open then
             rsBina.edit
             rsBina.ColumnByName("JEOLOJI").value = rsParsel.ColumnByName("UYGUNLUK").Value
             'rsBina.ColumnByName("YAN_CEKME").value = rsParsel.ColumnByName("YAN_CEKME").Value
             'rsBina.ColumnByName("ARKA_CEKME").value = rsParsel.ColumnByName("ARKA_CEKME").Value
             'rsBina.ColumnByName("INSAAT_NIZAMI").value = rsParsel.ColumnByName("INSAAT_NIZAMI").Value
             rsParsel.resetFilter
             rsParsel.close  
             .backmessage
             .setmessage rsBina.Recordcount & " /  " & rsBina.RecordNo
          end if
        rsbina.next
      wend
    end if
    .backmessage
    .setmessage "HAZIR ..."
    end if
  end with
end sub

'VERSION     : v0.01
'TARIH       : 30.09.2004
'AMA�        : Beliri bir tabakada bulunan ve �ak��an alan, yaz�, sembol, blok, do�ru
              'objelerinin ay�klanmas�
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
sub Main
dim i,j,o,oo,p,obj,k
dim dlg,layers,objects
dim objtype,layerId,nLayerId
   with Netcad
   set dlg = .NewBDialog("Obje Ay�kla")

   for i = 0 to  nclayermanager.NumLayer-1
      layers = layers & .LayerNameOf(i) & "|"
   next
   objects = "Alan" & "|" & "Yaz�" & "|" & "Sembol" & "|" & "Blok"  & "|" & "Do�ru" & "|" & "Nokta"

   dlg.GetCombo "LAYER", "TABAKALAR", layers, 0
   dlg.getcombo "OBJECT","OBJELER",objects,0
   dlg.GetString "NLYR", "��FT OBJE TABAKASI", "", 20

   if dlg.showmodal then

     .createLayer dlg.ValueByName("NLYR"), red
     nlayerId = .foundlayer(dlg.ValueByName("NLYR"))
    .closeLayer(nlayerId)

     layerId  = dlg.ValueByName("LAYER")
     objType = getObjType(dlg.ValueByName("OBJECT"))
     moveObjects layerId,nLayerId,objType

   end if
   end with
End Sub

function getObjType(idx)
     if idx = 0 then
       getobjtype = opline
     elseif idx = 1 then
        getobjtype = otext
     elseif idx = 2 then
        getobjtype = oshape
      elseif idx = 3 then
        getobjtype = oblock
      elseif idx = 4 then
        getobjtype = oline
      elseif idx = 5 then
        getobjtype = opoint
     end if
end function


sub moveObjects(LayerId,nLayerId,objType)
dim o,oo,i,j,k
    with netcad
      k = 0
      set oo = .newobject
      for i = 0 to .numobject-1
         .backmessage
         .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
         set o = .getobject(i)
         if o.tag = objType and o.tabaka = LayerId then
           .setfilter o.Limits,array(LayerId),array(objType)
           do while .GetNextObject2(oo)
              if (o.tag = objtype) and (oo.tag = objtype) and (i <> .CurObjPos) and _
              (o.tabaka = layerId) and (oo.tabaka = layerId) and _
              (o.Limits.cll.x = oo.Limits.cll.x) and (o.Limits.cll.y = oo.Limits.cll.y) and _
              (o.Limits.cur.x = oo.Limits.cur.x) and (o.Limits.cur.y = oo.Limits.cur.y) then
              o.tabaka = nLayerId
              '.setcurrentwindow o.limits,true
              .drawobject o,black
              .putobject i,o
               k = k + 1
              .backmessage
              .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
              exit do
             end if
           loop
           .ResetFilter
         end if
         set o = nothing
      next
    .backmessage
    .setmessage "HAZIR ..."
    msgbox "Ay�klanan obje say�s� : " & k
    end with
end sub

'VERSION     : v0.02
'TARIH       : 30.09.2004
'AMA�        : Beliri bir tabakada bulunan ve �ak��an alan, yaz�, sembol, blok, do�ru
              'objelerinin ay�klanmas�
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'20050412 : T�m tabakalarda arama eklendi.
'
sub Main
dim i,j,o,oo,p,obj,k
dim dlg,layers,objects
dim objtype,layerId,nLayerId
dim all
   with Netcad
   set dlg = .NewBDialog("Obje Ay�kla")

   for i = 0 to  nclayermanager.NumLayer-1
      layers = layers & .LayerNameOf(i) & "|"
   next
   objects = "Alan" & "|" & "Yaz�" & "|" & "Sembol" & "|" & "Blok"  & "|" & "Do�ru" & "|" & "Nokta"

   dlg.GetCombo "LAYER", "TABAKALAR", layers, 0
   dlg.GetCheck "TUMU", "T�M TABAKALARDA ARA", 0
   dlg.getcombo "OBJECT","OBJELER",objects,0
   dlg.GetString "NLYR", "��FT OBJE TABAKASI", "", 20

   if dlg.showmodal then

     .createLayer dlg.ValueByName("NLYR"), red
     nlayerId = .foundlayer(dlg.ValueByName("NLYR"))
    .closeLayer(nlayerId)

     layerId  = dlg.ValueByName("LAYER")
     objType = getObjType(dlg.ValueByName("OBJECT"))
     all = dlg.ValueByName("TUMU")

     moveObjects layerId,nLayerId,objType,all

   end if
   end with
End Sub

function getObjType(idx)
     if idx = 0 then
       getobjtype = opline
     elseif idx = 1 then
        getobjtype = otext
     elseif idx = 2 then
        getobjtype = oshape
      elseif idx = 3 then
        getobjtype = oblock
      elseif idx = 4 then
        getobjtype = oline
      elseif idx = 5 then
        getobjtype = opoint
     end if
end function


sub moveObjects(LayerId,nLayerId,objType,all)
dim o,oo,i,j,k
    with netcad
      k = 0
      set oo = .newobject
      if all = 0 then
      for i = 0 to .numobject-1
         .backmessage
         .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
         set o = .getobject(i)
         if o.tag = objType and o.tabaka = LayerId then
           .setfilter o.Limits,array(LayerId),array(objType)
           do while .GetNextObject2(oo)
              if (o.tag = objtype) and (oo.tag = objtype) and (i <> .CurObjPos) and _
              (o.tabaka = layerId) and (oo.tabaka = layerId) and _
              (o.Limits.cll.x = oo.Limits.cll.x) and (o.Limits.cll.y = oo.Limits.cll.y) and _
              (o.Limits.cur.x = oo.Limits.cur.x) and (o.Limits.cur.y = oo.Limits.cur.y) then
              o.tabaka = nLayerId
              '.setcurrentwindow o.limits,true
              .drawobject o,black
              .putobject i,o
               k = k + 1
              .backmessage
              .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
              exit do
             end if
           loop
           .ResetFilter
         end if
         set o = nothing
      next
      else
      for i = 0 to .numobject-1
         .backmessage
         .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
         set o = .getobject(i)
         if o.tag = objType then
           .setfilter o.Limits,array(),array(objType)
           do while .GetNextObject2(oo)
              if (o.tag = objtype) and (oo.tag = objtype) and (i <> .CurObjPos) and _
              (o.tabaka = oo.tabaka ) and _
              (o.tabaka <> nLayerId) and _
              (oo.tabaka <> nLayerId) and (o.Limits.cll.x = oo.Limits.cll.x) and (o.Limits.cll.y = oo.Limits.cll.y) and _
              (o.Limits.cur.x = oo.Limits.cur.x) and (o.Limits.cur.y = oo.Limits.cur.y) then
              o.tabaka = nLayerId
              '.setcurrentwindow o.limits,true
              .drawobject o,black
              .putobject i,o
               k = k + 1
              .backmessage
              .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
              exit do
             end if
           loop
           .ResetFilter
         end if
         set o = nothing
      next
      end if
    .backmessage
    .setmessage "HAZIR ..."
    msgbox "Ay�klanan obje say�s� : " & k
    end with
end sub

'VERSION     : v0.01
'TARIH       : 30.09.2004
'AMA�        : Beliri bir tabakada bulunan ve �ak��an alan, yaz�, sembol, blok, do�ru
              'objelerinin ay�klanmas�
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
sub Main
dim i,j,o,oo,p,obj,k
dim dlg,layers,objects
dim objtype,layerId,nLayerId
   with Netcad
   set dlg = .NewBDialog("Obje Ay�kla")

   for i = 0 to  nclayermanager.NumLayer-1
      layers = layers & .LayerNameOf(i) & "|"
   next
   objects = "Alan" & "|" & "Yaz�" & "|" & "Sembol" & "|" & "Blok"  & "|" & "Do�ru" & "|" & "Nokta"

   dlg.GetCombo "LAYER", "TABAKALAR", layers, 0
   dlg.getcombo "OBJECT","OBJELER",objects,0
   dlg.GetString "NLYR", "��FT OBJE TABAKASI", "", 20

   if dlg.showmodal then

     .createLayer dlg.ValueByName("NLYR"), red
     nlayerId = .foundlayer(dlg.ValueByName("NLYR"))
    .closeLayer(nlayerId)

     layerId  = dlg.ValueByName("LAYER")
     objType = getObjType(dlg.ValueByName("OBJECT"))
     moveObjects layerId,nLayerId,objType

   end if
   end with
End Sub

function getObjType(idx)
     if idx = 0 then
       getobjtype = opline
     elseif idx = 1 then
        getobjtype = otext
     elseif idx = 2 then
        getobjtype = oshape
      elseif idx = 3 then
        getobjtype = oblock
      elseif idx = 4 then
        getobjtype = oline
      elseif idx = 5 then
        getobjtype = opoint
     end if
end function


sub moveObjects(LayerId,nLayerId,objType)
dim o,oo,i,j,k
    with netcad
      k = 0
      set oo = .newobject
      for i = 0 to .numobject-1
         .backmessage
         .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
         set o = .getobject(i)
         if o.tag = objType and o.tabaka = LayerId then
           .setfilter o.Limits,array(LayerId),array(objType)
           do while .GetNextObject2(oo)
              if (o.tag = objtype) and (oo.tag = objtype) and (i <> .CurObjPos) and _
              (o.tabaka = layerId) and (oo.tabaka = layerId) and _
              (o.Limits.cll.x = oo.Limits.cll.x) and (o.Limits.cll.y = oo.Limits.cll.y) and _
              (o.Limits.cur.x = oo.Limits.cur.x) and (o.Limits.cur.y = oo.Limits.cur.y) then
              o.tabaka = nLayerId
              '.setcurrentwindow o.limits,true
              .drawobject o,black
              .putobject i,o
               k = k + 1
              .backmessage
              .setmessage "Ay�klanan obje say�s� : " & k & " : liste i " & i
              exit do
             end if
           loop
           .ResetFilter
         end if
         set o = nothing
      next
    .backmessage
    .setmessage "HAZIR ..."
    msgbox "Ay�klanan obje say�s� : " & k
    end with
end sub

'
' Dil  : Visual Basic
' Ama� : Istenen Siniftaki Objeleri Istenen Tabakaya Almak
'        ve NewBDialog kullanim ornegi.
Sub Main
Dim BD, i, sinif, Tabaka, TabNo, o

  set BD = Netcad.NewBDialog("")                 ' yeni dialog yarat
  BD.GetString  "item1","S�n�f", "", 15        ' Yazi iste. en fazla 15 karakter olsun
  BD.GetString  "item2","Tabaka", "", 15        ' Yazi iste. en fazla 15 karakter olsun
  if BD.showmodal then
    'MsgBox "Sinif = " & BD.ValueByName("item1")      ' kullanicinin girdigi degerleri goster
    'MsgBox "Tabaka = " & BD.ValueByName("item2")

    Sinif =  BD.ValueByName("item1")
    Tabaka = BD.ValueByName("item2")

    TabNo = Netcad.CreateLayer(Tabaka, red)

    for i = 0 to Netcad.numobject-1       ' projedeki tum objeleri sirayla tara
        set o = Netcad.getobject(i)         ' i. objeyi al
        if o.cls = Sinif then
          o.Tabaka = TabNo
          Netcad.putobject i,o              ' objeyi yenile
          Netcad.drawobject o, -1          ' objeyi ciz
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
    next

    MsgBox Sinif & " Objeleri " & Tabaka & " Tabakasina Alindi"

  end if
  set BD = Nothing
End sub


'
' Dil  : Visual Basic
' Ama� : 1- Bir alt procedure cagrilmasi
'        2- Projedeki coklu dogrularin verilen mesafe kadar kaydirilmasi
'
Sub MovePoly (MoveDistZ)
Dim i,j,o,p
   with Netcad
      for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        set o = .getobject(i)         ' i. objeyi al
        if o.tag = opline then        ' Coklu dogrumu ?
          .drawobject o, Black        ' objeyi silinmis gibi siyahla ciz
          set p = .getplineext(o)     ' objenin noktalarini tutan pline yapisini al
          for j = 0 to p.num-1        ' coklu dogrunun herbir noktasi icin
            p.cor(j).Z = p.cor(j).Z + MoveDistZ     ' j. noktayi kaydir

          next
          .putplineext o,p            ' noktalari yenile
          .putobject i,o              ' objeyi yenile
          .drawobject o, Red          ' objeyi kirmizi renkle ciz
          set p = nothing             ' noktalar icin aldigimiz memory'i geri ver
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
      next
   end with
End Sub

Sub Main
  ' Program buradan basliyor.
  ' Cokludogrulari 10 metre saga 10 metre sola kaydir
  MovePoly 2
End Sub

'
' Ama� : Conman Dataset Filter Kullan�m �rne�i
'
'        Ekrandan se�ilen noktan�n hangi spatial kay�tlar�n i�ine
'        d��t���n� bulur ve bunlar� k�rm�z� renkle �izer
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

Sub Main
dim DS, Filter,c,p,a, Sinif,w

   Sinif = "KITLE"

   ' Orginal Dataseti Sinifindan Bulur, yoksa uyar
   set DS = NCConman.FindDatasetByFC(False, Sinif)
   if DS is nothing then
     msgbox "Projede " & Sinif & " adl� bir s�n�f yok !"
     Exit Sub
   end if

   ' Spatial Tablonun limitlerini bulup projede buraya yaklas
   set w = DS.Limits(true)
   Netcad.SetCurrentWindow w, true

   with Netcad
     set c = .Newc(0, 0, 0)
     set p = .NewPoly
     while .SelectPoint ("Bir Nokta se�in", c, -1)
       p.Clear
       p.AddCoor(c)

       ' Noktayi icerenler filitresi yarat
       set Filter = .NewDatasetFilter
       Filter.FilterType = NC_GISDS_IncludePoint
       filter.poly = p

       DS.Close
       DS.SetFilter( Filter )  ' Filitreyi uygula
       a = 0
       if DS.Open then
         DS.First
         while not DS.EOF
           DS.Geometry.Draw(red)
           a = a + 1
           DS.Next
         wend
         DS.Close
       end if
       DS.ResetFilter    ' Filitreyi Kapat
       set Filter = nothing

       msgbox a & " obje bulundu."
     wend
     set p = nothing
     set c = nothing
   end with

   set DS = nothing
End Sub

'TARIH       : 30.09.2004
'AMA�        : �stenilen bir klas�rde bulunan dosyalar�n teker teker a��l�p,
              'istenilen d�zenlemelerin yap�lmas� ve dosyalar�n istenilen
              'klas�re kay�t edilmesi.
'GEREKLILIKLER : Gerekli dosyalar olan 
'                - Netplot.lin Netcad kurulum dizinine
'                - DGC Netcad kurulum dizindinde SECME klas�r�ne
'                - Plan.NCS ve Plan.Ini Kurulum dizininde SEMBOL klas�r�ne.
'                kopyalanmal�d�r. 
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
' Sabitler
const InPath  = "G:\SILIVRI\DGN_MACRO\"    'Girdi Dosyalar�n�n yeri
const OutPath = "G:\SILIVRI\HHAZIR_1000_NCZ\"    'Sonu� Dosyalar�n�n yeri
'
'
'Ana Program
Sub main
dim dir,fSo,Count,file,inFile
  with netcad
    set fSo = CreateObject("Scripting.FileSystemObject") 'Dosya Sistemi objesi olu�turulur.
    set dir = fso.GetFolder(InPath)                      'Bu Objenin �zelliklerinden GetFolder
                                                         'kullan�larak klas�r bir objeye atan�yor.
    set Count = dir.Files                                'Klas�rdeki dosya listesi al�n�yor
    for each file in Count
      .Loadfile InPath&file.name,fs_microst               'InPath Klas�rdeki dosya file.name isimli
                                                          'dosyalar s�rayla y�kleniyor. Yanda
                                                          'LoadFile komutu ile y�klenecek dosya tipi
                                                          'parametresi olarak fs_microstat (Microstation)
                                                          'g�nderiliyor.


      '
      '
      'Hatal� gelen objeler i�in bir tabaka olu�turulup, bu objeler o tabakaya yerle�tirilmektedir.
      createErrorLayer
      '
      '�stenmeyen tabakalar silinmektedir.
      deleteLayers
      '
      '0 tabakas�ndaki tan�md��� nesneler olu�turulmu� olan tabakaya aktar�l�yor.
      removeObjects
      '
      'NCZ Dosyas�n�n projeksiyonu ayarlanmaktad�r.
      setProjection
      '
      ' Kullan�lmayan tabakalar, �izgi tipleri, yaz� tipleri , semboller , bloklar,
      ' bilgilendirme men�s� g�r�nt�lenmeden silinmektedir.
      .NetcadCommand "PROJECT CLEAN 1,1,1,1,1,1"
      
      ' Proje d�zenleme sonras�nda olu�an bozuk objeler silinir.
      DeleteObjects
      
      .NetcadCommand "PROJECT CLEAN 0,0,0,0,0,1"
      '
      '
      inFile = split(file.name,".",-1,1)
      .SaveToFile OutPath&inFile(0)&".ncz"                ' Yap�lan d�zenleme i�leminden sonra a��lm��
                                                          ' olan proje, Outpath sabitinde belirtilmi�
                                                          ' olan yere saklan�yor.
      '
      .NetcadCommand "CLOSE ACTIVEPROJECT"                ' Aktif Proje Kapat�l�yor
    Next
  end with
end Sub

sub createErrorLayer
  with ncLayerManager     ' Tan�md��� objeler i�in ncLayarManager objesi ile bir tabaka
    .add "TANIMDISI",red  ' olu�turuluyor. Bu tabakaya isim olarak "TANIMDISI" se�ilmi�tir.
  end with                ' Tabaka rengi ise k�rm�z� (red) olarak ayarlanm��t�r.
end sub

sub removeObjects
dim obj,i
  with netcad
    for i = 0 to .numobject-1
      set obj = .getobject(i)                  ' T�m objeler taran�yor.
      if obj.tabaka = 0 then                   ' 0 Tabakas�ndaki objeler s�rayla se�iliyor.
        obj.tabaka = .foundlayer("TANIMDISI")  ' 0 Tabakas�ndaki objelerin tabakalar� "TANIMDISI" olarak
        .putobject i,obj                       ' se�iliyor ve objeler obje listesine yeniden ekleniyor.
      end if
      set obj = nothing                        ' Ge�ici obj nesnesinin i�eri�i haf�zadan siliniyor.
    next
  end with
end sub

sub DeleteLayers
  with ncLayerManager  ' ncLayerManager nesnesi ile istenmeyen tabakalar mevcut ise siliniyor.
    if .Find("PFT_LEJANT") <> -1 then netcad.closelayer(.FOUNDLAYER("PFT_LEJANT"))  ' .find komutununun sonucu -1 ise ilgili tabaka projede bulunmamaktad�r.
    if .Find("PAFTA_BILGI")<> -1 then netcad.closelayer(.FOUNDLAYER("PAFTA_BILGI")) ' -1 den farkl� oldu�unda ise tabaka .delete komutu ile silinmektedir.
    if .Find("PFT_K��EKOORD�NAT") <> -1 then netcad.closelayer(.FOUNDLAYER("PFT_K��EKOORD�NAT"))
    if .Find("PAFTA") <> -1 then netcad.closelayer(.FOUNDLAYER("PAFTA"))
  end with
end sub

sub setProjection 
dim nProj
  with netcad
    set nProj = .NewProjection           'Aktif dosyan�n projeksiyonunun belirlenmesi i�in
    nProj.ProjectionType   = PR_UTM3     '�ncelikle bir projeksiyon nesnesi olu�turulmal�d�r.
    nProj.Datum            = DATUM_TUR_1 'Projeksiyon parametre de�erleri, kurulum dizininde, NCMACRO
    nProj.Zone             = 30          'klas�r�n�n alt�ndaki NVBASIC dosyas�ndan se�ilmi�tir.
    nProj.setToCurrentProject            'Olu�turulmu� olan projeksiyon aktif projenin projeksiyonu 
  end with                               'olarak set ediliyor.
end sub

sub DeleteObjects
dim obj,i
     with netcad
     for i = 0 to .numobject-1
       set obj = .getobject(i)
       ' tespit edilebilmi� tipteki bozuk objeler taran�r ve silinir.
       ' Yeni tespit edilmi� olanlar buraya eklenmelidir.
       if obj.tag = opline and obj.TABAKA = .foundlayer("E�R�_10M") and obj.length(false) > 500 then 
         .delobject i,obj
       elseif obj.tag = ocircle and obj.TABAKA = .foundlayer("E�R�_1M") then
         .delobject i,obj
       end if
      set obj = nothing
     next
   end with
end sub

Const InPath   = "\\csagdicpc\BUTUN_ISTANBUL_DGN\"   'T�m Dosya klas�r�
Const OutPath  = "E:\DGNTEST\DGN2\"                  'Kopyalancak klas�r
Const index    = "E:\DGNTEST\paflist\kadikoy.txt"    'Pafta listesi yeri

Sub main
dim dir,fSo,fst,Count,file,inFile,fn
dim txt,txtStream
'Klas�r objesi i�eri�i ile birlikte olu�turuluyor
  set fSo = CreateObject("Scripting.FileSystemObject")
  set dir = fso.GetFolder(InPath)
  set Count = dir.Files  
'Dosya listesi text dosyadan okunuyor.
  set fSt = CreateObject("Scripting.FileSystemObject")
  Set txt = fst.GetFile(index)
  For Each file in Count
    fn = split(file.name,".",-1,1)
    Set txtStream = txt.OpenAsTextStream(1)
    Do While Not txtStream.AtEndOfStream
      if  txtStream.ReadLine = fn(0) then
        file.Copy outPath&file.Name  ' dosya kopyalan�yor.
        exit do
      end if
    Loop
    txtStream.Close
    set txtStream = nothing
  next
End Sub

'Makro Yazar�:      Harita M�hendisi O�uzalp BOZKURT
'Ama�:              Se�ilen tabakan�n toplam alan�n�n bulunmas�
'Tarih:             HAZ�RAN-2012

Sub Main

with netcad

Dim obj
Dim secim
dim alanobje
dim alanobje1
dim topla
dim c
dim yaz
dim yazi
dim a
dim b
dim text




set secim = .NewSelectStatus()

while .SelectObjectInstant("YAZI SE�",2,array(otext),secim)

set alanobje = secim.objects(0)
set alanobje1 = secim.objects(1)

a=(alanobje.s)
b=(alanobje1.s)

topla= a-b

wend
 
msgbox round(topla,2)


end with
end sub


' Yazan : Ercument korkmaz
' Tarih : 23.11.2005
' A��klama :   toplu olarak se�ilen do�rulara istenilen kot e�erinin verilmesi
' 4.0.032 versyonunda �al���yor
Sub Main
Dim i,j,o,SEL, p, m , a, b , bd
with Netcad
set bd = .NewBDialog("KOT YAZ")
BD.GetFloat   "a","Kot De�eri Girin", 0, 3
bd.showmodal
  a = bd.ValueByName("a")
    set SEL = .NewSelectionSet   ' Yeni kume yarat
    set o = .NewObject
    if SEL.SELECT("CokDogru Objelerini Se�",array(opline)) then  ' istenen turleri kumeye ekle
      for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
        j = SEL.GetSelectedObject(i, o)    ' objeyi ve gercek indeksini al
         set p = .getplineext(o)            ' objenin noktalarini tutan pline yapisini al
          for m = 0 to p.num-1                  ' coklu dogrunun herbir noktasi icin
            p.cor(m).z =  a
          next
          .putplineext o,p            ' noktalari yenile
        .putobject j, o                    ' objeyi geri koy
      next
      SEL.RedrawAndRewind                  ' secim kumesini toplu kendi renginde
    end if                                 ' cizdir ve kumeyi basa sardir.
    set SEL = nothing
    set o = nothing
  end with
end sub



' Yazan : 
' Tarih : 22.4.2014
' A��klama : 

Sub Main
Dim OBJ
dim poly


with Netcad
.SetFilter nothing, ARRAY(.foundlayer("ALN_DAIMI_IRTIFA")), ARRAY(opline)
DO
SET OBJ=.GETNEXTOBJECT
IF OBJ IS NOTHING THEN
EXIT DO
ELSE

set poly =obj.getobjectaspline
bul poly
end if
loop
.resetfilter
end with
End Sub





sub bul(poly)
dim obj1
with netcad
.SetFilter nothing, ARRAY(.foundlayer("YENINOKTA")), ARRAY(opoint)
DO
SET OBJ1=.GETNEXTOBJECT
IF OBJ1 IS NOTHING THEN
EXIT DO
ELSE
if poly.onpoly(true,obj1.p1) then
obj1.tabaka=.createlayer("DA�M�_NOK_KAPAT",5)
.putobject .curobjpos, obj1
end if


end if


loop
.resetfilter
end with
end sub




' Yazan : 
' Tarih : 22.4.2014
' A��klama : 

Sub Main
Dim OBJ
dim poly



with Netcad
.SetFilter nothing, ARRAY(.foundlayer("ALN_DAIMI_IRTIFA")), ARRAY(opline)
DO
SET OBJ=.GETNEXTOBJECT
IF OBJ IS NOTHING THEN
EXIT DO
ELSE
set poly =obj.getobjectaspline
bul poly
end if
loop
.resetfilter




end with
ikinci


End Sub





sub bul(poly)
dim obj1
with netcad
.SetFilter nothing, ARRAY(.foundlayer("YENINOKTA")), ARRAY(opoint)
DO
SET OBJ1=.GETNEXTOBJECT
IF OBJ1 IS NOTHING THEN
EXIT DO
ELSE
if poly.onpoly(true,obj1.p1) then
obj1.tabaka=.createlayer("DA�M�_NOK",3)
.putobject .curobjpos, obj1
end if
end if
loop
.resetfilter
end with
end sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub ikinci
Dim OBJ3

with Netcad
.SetFilter nothing, ARRAY(.foundlayer("YENINOKTA")), ARRAY(opoint)
DO
SET OBj3=.GETNEXTOBJECT
IF OBJ3 IS NOTHING THEN
EXIT DO
ELSE

bul2 obj3




end if
loop
.resetfilter
end with
End Sub






sub bul2(obj3)
dim obj2
with netcad
.SetFilter nothing, ARRAY(.foundlayer("KOSE_NO_IRT"),_
                          .foundlayer("KOSE_NO_PAR")), ARRAY(otext)
DO
SET OBJ2=.GETNEXTOBJECT
IF OBJ2 IS NOTHING THEN
EXIT DO
ELSE
if obj3.pname=obj2.s then
obj2.tabaka=.createlayer("KAPAT_YAZI",3)
.putobject .curobjpos, obj2
end if
end if
loop
.resetfilter
end with
end sub



' A��a��daki de�erleri 0  yaparsan tam ortas�na atar.
' DGN den gelmeyen yerkotu yaz�lar�ndan kotlu nokta �retir.
'2005.04.19
'�stanbul B�lge M�d�rl���.
const dy = 0.42
const dx = -0.60

sub main
dim o,k,p,ly,world
  with netcad
    set o = .newobject
    if .foundlayer("NOKTA") = -1 then ly = .createLayer("NOKTA",red)
    .setfilter nothing,array(.foundlayer("YERKOTU")),array(otext)
    while .getnextobject2(o)
     if not .escPressed then
       'msgbox  cDbl(o.s)
       set world = o.Limits.getAsPLine
       set p = .newc(world.centerofmass.y+dy,world.centerofmass.x+dx,cDbl(o.s))
      .addObject .MakePoint(p, "a", "a", ly)
     else
       exit sub
     end if
    wend
  end with
end sub




' A��a��daki de�erleri 0  yaparsan tam ortas�na atar.
' DGN den gelmeyen yerkotu yaz�lar�ndan kotlu nokta �retir.
'2005.04.19
'�stanbul B�lge M�d�rl���.
const dy = 0.42
const dx = -0.60

sub main
dim o,k,p,ly,world
  with netcad
    set o = .newobject
    if .foundlayer("NOKTA") = -1 then ly = .createLayer("NOKTA",red)
    .setfilter nothing,array(.foundlayer("YERKOTU")),array(otext)
    while .getnextobject2(o)
     if not .escPressed then
       'msgbox  cDbl(o.s)
       set world = o.Limits.getAsPLine
       set p = .newc(world.centerofmass.y+dy,world.centerofmass.x+dx,cDbl(o.s))
      .addObject .MakePoint(p, "a", "a", ly)
     else
       exit sub
     end if
    wend
  end with
end sub




'Makro Yazar�:      Harita Y�ksek M�hendisi O�uzalp BOZKURT
'Ama�:              TABAKA RENG�N�N DE���T�R�LMES�


Sub Main

with netcad

Dim obj
dim selection
Dim BD
dim item
dim a




set selection = .NewSelectStatus

    while .SelectObjectInstant("RENK KODU G�R",1,array(),selection)
          set obj = selection.objects(0)



.SetFilter nothing, array(obj.tabaka), array()

           do

             set obj=.getnextobject

             if obj is nothing then

           exit do

             end if


                   .Delobject .curobjpos, obj


           loop

.resetfilter

    wend




 


end with

end sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

'set secim = .NewSelectStatus()

set c = .newc(0,0,0)

layerno=.foundlayer("BEY_YAZI")




 while.SelectPoint("Nokta se�", c, 2)

   set yaz1=.maketext(c,"(ED50/39-3)",0,0,2,0,"1",.createlayer("text_datum",1))
   .addobject(yaz1)
 wend


end with
end sub

'' www.sabangul.com.tr Web Sayfas�ndan �ndirilmi�tir
' �aban G�L , Harita M�hendisi
' Her T�rl� Hata, �stek ve �neriler ��in 
' haritaakademi@gmail.com veya sagulnet@gmail.com
' adresine durumu anlatan bir e-posta g�nderiniz.




Sub Main
 with netcad



Dim i
 dim j
 dim line
 dim multiline
 dim o
 dim SEL
 dim xls
 dim xlspath
 dim alan
 dim DEG
 dim CL
 dim bd
 DIM U,V,R,W
 dim ruhangul
 dim elifyaren
 DIM NO(50000,2)
 DEG = ""
 CL=0:U=0:R=0
 set xls = CreateObject("excel.application")







set BD = Netcad.NewBDialog("Excelden Kritere G�re Tabakaland�rma [Harita Akademi, �aban G�L]")
 BD.GetFileName "item1","Aktar�m Yap�lacak Excel Dosyas� Se�iniz:","","Excel Dosyalari|*.xls|Tum Dosyalar|*.*","xls"
 BD.Getcombo "item2","Parsel Numaras� Hangi S�tunda Bulunuyor ? ","A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z" ,0
 BD.Getcombo "item3","Tabakaland�r�lacak Kriter Hangi S�tunda Bulunuyor ? ","A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z" ,1
 BD.Getcheck "item4","Tabakalar� Grupland�r.(@...)" ,1
 BD.GetString "item5", "Grup Ad�", "sagul", 5
 BD.GetString "item6", "Bo� De�erlerin Al�naca�� Tabaka", "BO�_DE�ER", 10
 if BD.showmodal then
 xlspath = BD.ValueByName("item1")
 RUHANGUL=BD.ValueByName("item4")
 elifyaren= BD.ValueByName("item5")
 else
 exit sub
 end if

dim saban,ruhan

saban= BD.ValueByName("item2")
 ruhan= BD.ValueByName("item3")

saban=1
 ruhan=2

if BD.ValueByName("item2")="A" then saban=1
 if BD.ValueByName("item2")="B" then saban=2
 if BD.ValueByName("item2")="C" then saban=3
 if BD.ValueByName("item2")="D" then saban=4
 if BD.ValueByName("item2")="E" then saban=5
 if BD.ValueByName("item2")="F" then saban=6
 if BD.ValueByName("item2")="G" then saban=7
 if BD.ValueByName("item2")="H" then saban=8
 if BD.ValueByName("item2")="I" then saban=9
 if BD.ValueByName("item2")="J" then saban=10
 if BD.ValueByName("item2")="K" then saban=11
 if BD.ValueByName("item2")="L" then saban=12
 if BD.ValueByName("item2")="M" then saban=13
 if BD.ValueByName("item2")="N" then saban=14
 if BD.ValueByName("item2")="O" then saban=15
 if BD.ValueByName("item2")="P" then saban=16
 if BD.ValueByName("item2")="Q" then saban=17
 if BD.ValueByName("item2")="R" then saban=18
 if BD.ValueByName("item2")="S" then saban=19
 if BD.ValueByName("item2")="T" then saban=20
 if BD.ValueByName("item2")="U" then saban=21
 if BD.ValueByName("item2")="V" then saban=22
 if BD.ValueByName("item2")="W" then saban=23
 if BD.ValueByName("item2")="X" then saban=24
 if BD.ValueByName("item2")="Y" then saban=25
 if BD.ValueByName("item2")="Z" then saban=26



if BD.ValueByName("item3")="A" then ruhan=1
 if BD.ValueByName("item3")="B" then ruhan=2
 if BD.ValueByName("item3")="C" then ruhan=3
 if BD.ValueByName("item3")="D" then ruhan=4
 if BD.ValueByName("item3")="E" then ruhan=5
 if BD.ValueByName("item3")="F" then ruhan=6
 if BD.ValueByName("item3")="G" then ruhan=7
 if BD.ValueByName("item3")="H" then ruhan=8
 if BD.ValueByName("item3")="I" then ruhan=9
 if BD.ValueByName("item3")="J" then ruhan=10
 if BD.ValueByName("item3")="K" then ruhan=11
 if BD.ValueByName("item3")="L" then ruhan=12
 if BD.ValueByName("item3")="M" then ruhan=13
 if BD.ValueByName("item3")="N" then ruhan=14
 if BD.ValueByName("item3")="O" then ruhan=15
 if BD.ValueByName("item3")="P" then ruhan=16
 if BD.ValueByName("item3")="Q" then ruhan=17
 if BD.ValueByName("item3")="R" then ruhan=18
 if BD.ValueByName("item3")="S" then ruhan=19
 if BD.ValueByName("item3")="T" then ruhan=20
 if BD.ValueByName("item3")="U" then ruhan=21
 if BD.ValueByName("item3")="V" then ruhan=22
 if BD.ValueByName("item3")="W" then ruhan=23
 if BD.ValueByName("item3")="X" then ruhan=24
 if BD.ValueByName("item3")="Y" then ruhan=25
 if BD.ValueByName("item3")="Z" then ruhan=26





set BD = Nothing
 xls.workbooks.open(xlspath)
 xls.range("A1").select

FOR U=1 TO 100000
 CL=CL+1
 NO(U,1)="*" & XLS.CELLS(U,saban)
 NO(U,2)= XLS.CELLS(U,ruhan)
 IF NO(U,2)="" THEN NO(U,2)=0
 IF NO(U,1)="*" THEN U=100000
 NEXT
 xls.quit





MSGBOX CL-1 & " Adet Parsel Excel Dosyas�ndan Ba�ar�yla Okundu. L�tfen ��lem G�recek Parselleri Se�iniz."



FOR V=1 TO CL
 Dim max,min,elif ,sabangul,tabaka
 max=255
 min=1
 Randomize
 elif=Int((max-min+1)*Rnd+min)



if elif=0 or elif=15 then elif=4
 if elif>79 and elif <96 then elif=5

with NCLayerManager



if RUHANGUL=0 then
 .Add NO(V,2), elif
 else
 .Add "@"&elifyaren,5
 .Add elifyaren & "_" & NO(V,2), elif
 end if
 END With

sabangul=0
 for tabaka = 0 to .numlayers - 1
 sabangul=sabangul+1
 next
 if sabangul>254 then
 msgbox ("Tabaka Say�s� Netcad'in S�n�r�n� A�mak �zere!!" &chr(13)&chr(10)&" L�tfen projenizi inceleyiniz veya tabakalar� azalt�n�z" ),64,"Harita Akademi, �aban G�L"
 msgbox ("Proje ve Veri G�venli�i ��in ��leme Devam Edilmeyecektir." &chr(13)&chr(10)&"Tabakalar� azalt�p tekrar deneyiniz." ),64,"Harita Akademi, �aban G�L"
 exit sub
 end if

next

with Netcad
 set SEL = .NewSelectionSet
 set o = .NewObject





if SEL.SELECT("L�tfen ��lem G�recek Parselleri Se�iniz...",array(opline)) then

for i = 0 to SEL.NE-1
 j = SEL.GetSelectedObject(i, o)
 alan = o.pname

FOR V=1 TO CL
 W=NO(V,1)
 if W ="*" & alan then
 with NCLayerManager
 if RUHANGUL=0 then
 o.tabaka = .Find(NO(V,2))
 else
 o.tabaka = .Find(elifyaren & "_" & NO(V,2))
 end if

if .Find(NO(V,2))="" then
 o.tabaka=BD.ValueByName("item6")
 end if

end with
 .putobject j, o
 R=R+1
 V=U
 end if
 NEXT
 next
 SEL.RedrawAndRewind

end if



set SEL = nothing
 set o = nothing
 end with
 end with
 MSGBOX R & " adet Parselin Tabakas� De�i�tirildi."
 end sub
 


' Yazan :ERCUMENT KORKMAZ 
' Tarih : 10.08.2006
' A��klama : PAFTA KOORD�NATLARININ EXCELE ALINMASI MACROSU
Sub Main
Dim i , o, p, xl
  with Netcad
   set xl = CreateObject("excel.application")
    xl.visible = true
     xl.workbooks.add
        for i = 0 to .numobject-1
        set o = .getobject(i)
        if o.tag = opline then
          set p = .getplineext(o)
          'msgbox p.cor(1).y
       'xl.Cells(i+1, 1).Value = 1
    xl.Cells(i+1, 1).Value = p.cor(1).y
    xl.Cells(i+1, 2).Value = p.cor(1).x
    xl.Cells(i+1, 3).Value = p.cor(2).y
    xl.Cells(i+1, 4).Value = p.cor(2).x
    xl.Cells(i+1, 5).Value = p.cor(3).y
    xl.Cells(i+1, 6).Value = p.cor(3).x
    xl.Cells(i+1, 7).Value = p.cor(4).y
    xl.Cells(i+1, 8).Value = p.cor(4).x
    xl.Cells(i+1, 9).Value = o.objname
    xl.Cells(i+1, 10).Value = o.pname
          set p = nothing
        end if
        set o = nothing
      next
   msgbox ""
  end with
End Sub

'
'  Amac : Netcad Aktif Proje Window.unun bir kismini imaj olarak
'         bir dosyaya saklamak.
'

Sub Main
dim w,h
  with Netcad
    w = .CurrentWinWdPixel
    h = .CurrentWinHtPixel
    if .SaveCurWinImage(w/2-75, w/2+75, h/2-50, h/2+50, IMAGE_JPG, _
       "C:\TEST.JPG", 95) then
      msgbox "Saklandi"
    end if
  end with
End Sub

const ATabaka = "PL_LIMANTESISAL" ' Konut Tabakas�
const YTabaka = "SM_EMSAL"  ' TAKS/KAKS Belirleyici �izgi Tabakas�
sub main()
  with netcad
    .createLayer "GIS_EMSAL",red   ' De�i�kenler i�in Tabakalar olu�tur
    .createLAyer "GIS_HMAX",red
    distPlanVarsToLayers Atabaka,YTabaka ' Tabakalar� Ay�r
  end with
end sub

sub distPlanVarsToLayers(AlanTabaka,YaziTabaka)
dim o,oo,ooo,i,j,k,objLine,objText,cLine,cText,fark
dim clinex,cliney,ctextx,ctexty
dim tmpstr
    with netcad
      set ooo = .newobject
      set oo = .newobject
      set o  = .newobject
      .setfilter nothing,array(.foundLayer(AlanTabaka)),array(opline)
      do While .getNextObject2(o)
        i = .curobjpos
        k = 0
        if not .escpressed then
          '.setfilter o.Limits,array(.foundLayer(YaziTabaka)),array(oline)
          'do while .getNextObject2(oo)
            'if (o.getobjectaspline.inpoly(oo.limits.cll)) and _
               '(o.getobjectaspline.inpoly(oo.limits.cll)) and _
               '(oo.tag = oline) then
              'set objLine = oo
              'cline = getCenter(objLine.Limits.cll.x,objLine.Limits.cll.y, _
                                'objLine.Limits.cur.x,objLine.Limits.cur.y)

              'clinex = getcentercoor(objLine.Limits.cll.x,objLine.Limits.cur.x)
              'cliney = getcentercoor(objLine.Limits.cll.y,objLine.Limits.cur.y)
            ' set cline = .newc(cliney,clinex,0)
              .setfilter o.Limits,array(.foundLayer(YaziTabaka)),array(otext)
              while .GetNextObject2(ooo)
                'if (o.getobjectaspline.inpoly(ooo.limits.cll)) and _
                   '(o.getobjectaspline.inpoly(ooo.limits.cur)) and _
                    if (ooo.tag = otext) then
                  'cText = getCenter(ooo.limits.cll.x,ooo.limits.cll.y,ooo.limits.cur.x,ooo.Limits.cur.y)
                     tmpstr = split(ooo.s,"=",-1,1)
                     if tmpstr(0) = "E" then
                     ooo.s = tmpstr(1)
                     ooo.tabaka = .foundlayer("GIS_EMSAL")
                     .putobject .curobjpos,ooo
                    elseif tmpstr(0)= "Hmax" then
                      ooo.s = tmpstr(1)
                     ooo.tabaka = .foundlayer("GIS_HMAX")
                     .putobject .curobjpos,ooo
                    end if

                  'ctextx = getcentercoor(ooo.Limits.cll.x,ooo.Limits.cur.x)
                  'ctexty = getcentercoor(ooo.Limits.cll.y,ooo.Limits.cur.y)
                  'msgbox ctextx & " :::" & clinex
                  'set ctext = .newc(ctexty,ctextx,0)
                  'fark = ncmath.Distance(ctext, cline, false)

                  'if checkyazi(ctextx,ctexty,clinex,cliney,fark) = 0 then
                     'ooo.tabaka = .foundlayer("GIS_TAKS")
                    '.putobject .curobjpos,ooo
                    'k = "TAKS"
                  'elseif checkyazi(ctextx,ctexty,clinex,cliney,fark) = 1 then
                     'ooo.tabaka = .foundlayer("GIS_KAKS")
                    '.putobject .curobjpos,ooo
                    'k = "KAKS"
                  'end if
                  .backmessage
                  .setmessage .curobjpos

                end if
                 'msgbox ctextx & " :::" & clinex & " :::" & k & "::::" & ooo.s & " :::" & fark
              wend
              .ResetFilter
            'end if
          'loop
          '.resetFilter
        end if
      loop
      set o = nothing
      set oo = nothing
      set ooo = nothing
      .resetFilter
      .backmessage
      .setmessage "HAZIR ..."
      msgbox "De�i�tirilen obje say�s� : " & k
    end with
end sub

function getCenter(x1,y1,x2,y2)
  getCenter = (x1+x2)/(y1+y2)
end function

function getCenterCoor(a,b)
  getCenterCoor = (a+b)/2
end function

function checkFark(f,xt,yt,xl,yl)
  if (f > 1) and (f < 3.09) and (yt<yl) and ((xt=<xl) or (xt=>xl)) then        ' insaat nizam
    checkFark = 0
  elseif (f > 1) and (f < 2.9) and (yt>yl) and (xt =<xl )then  ' Kat Adedi
    checkFark = 1
  elseif (f > 2.6) and (f < 3.4)  and (xl<xt) and (yt => yl)  then    ' GIS_AOC
    checkFark = 2
  elseif (f > 3) and (f < 4.5)  and (xl>xt) and  (yt > yl)then    ' GIS_YAC
    checkFark = 3
  else                                 '
    checkFark = -1
  end if
end function

function checkyazi(xt,yt,xl,yl,fark)
 if xt >= xl and fark > 0 and fark < 5 then
   checkyazi = 0
 elseif xt < xl and fark > 0 and fark < 5  then
   checkyazi = 1
 else
   checkyazi = -1
 end if
end function

const ATabaka = "PL_KONUT" ' Konut Tabakas�
const YTabaka = "SM_TAKS"  ' TAKS/KAKS Belirleyici �izgi Tabakas�
const DTabaka = "SM_DOGRU" ' Do�ru Tabakas�
sub main()
  with netcad
    .createLayer "GIS_TAKS",red   ' De�i�kenler i�in Tabakalar olu�tur
    .createLAyer "GIS_KAKS",red
    distPlanVarsToLayers Atabaka,YTabaka ' Tabakalar� Ay�r
  end with
end sub

sub distPlanVarsToLayers(AlanTabaka,YaziTabaka)
dim o,oo,ooo,i,j,k,objLine,objText,cLine,cText,fark
dim clinex,cliney,ctextx,ctexty
    with netcad
      set ooo = .newobject
      set oo = .newobject
      set o  = .newobject
      .setfilter nothing,array(.foundLayer(AlanTabaka)),array(opline)
      do While .getNextObject2(o)
        i = .curobjpos
        k = 0
        if not .escpressed then
          .setfilter o.Limits,array(.FoundLayer(DTabaka)),array(oline)
          do while .getNextObject2(oo)

            if (o.getobjectaspline.inpoly(oo.limits.cll)) and _
               (o.getobjectaspline.inpoly(oo.limits.cll)) and _
               (oo.tag = oline) then
              set objLine = oo

              'cline = getCenter(objLine.Limits.cll.x,objLine.Limits.cll.y, _
                                'objLine.Limits.cur.x,objLine.Limits.cur.y)

              clinex = getcentercoor(objLine.Limits.cll.x,objLine.Limits.cur.x)
              cliney = getcentercoor(objLine.Limits.cll.y,objLine.Limits.cur.y)
             set cline = .newc(cliney,clinex,0)
              .setfilter o.Limits,array(.foundLayer(YaziTabaka)),array(otext)
              while .GetNextObject2(ooo)
                'if (o.getobjectaspline.inpoly(ooo.limits.cll)) and _
                   '(o.getobjectaspline.inpoly(ooo.limits.cur)) and _
                    if (ooo.tag = otext) then
                  'cText = getCenter(ooo.limits.cll.x,ooo.limits.cll.y,ooo.limits.cur.x,ooo.Limits.cur.y)
                  ctextx = getcentercoor(ooo.Limits.cll.x,ooo.Limits.cur.x)
                  ctexty = getcentercoor(ooo.Limits.cll.y,ooo.Limits.cur.y)
                  'msgbox ctextx & " :::" & clinex
                  set ctext = .newc(ctexty,ctextx,0)
                  fark = ncmath.Distance(ctext, cline, false)

                  if checkyazi(ctextx,ctexty,clinex,cliney,fark) = 0 then
                     ooo.tabaka = .foundlayer("GIS_TAKS")
                    .putobject .curobjpos,ooo
                    k = "TAKS"
                  elseif checkyazi(ctextx,ctexty,clinex,cliney,fark) = 1 then
                     ooo.tabaka = .foundlayer("GIS_KAKS")
                    .putobject .curobjpos,ooo
                    k = "KAKS"
                  end if
                  .backmessage
                  .setmessage .curobjpos

                end if
                 'msgbox ctextx & " :::" & clinex & " :::" & k & "::::" & ooo.s & " :::" & fark
              wend
              .ResetFilter
            end if
          loop
          .resetFilter
        end if
      loop
      set o = nothing
      set oo = nothing
      set ooo = nothing
      .resetFilter
      .backmessage
      .setmessage "HAZIR ..."
      msgbox "De�i�tirilen obje say�s� : " & k
    end with
end sub

function getCenter(x1,y1,x2,y2)
  getCenter = (x1+x2)/(y1+y2)
end function

function getCenterCoor(a,b)
  getCenterCoor = (a+b)/2
end function

function checkFark(f,xt,yt,xl,yl)
  if (f > 1) and (f < 3.09) and (yt<yl) and ((xt=<xl) or (xt=>xl)) then        ' insaat nizam
    checkFark = 0
  elseif (f > 1) and (f < 2.9) and (yt>yl) and (xt =<xl )then  ' Kat Adedi
    checkFark = 1
  elseif (f > 2.6) and (f < 3.4)  and (xl<xt) and (yt => yl)  then    ' GIS_AOC
    checkFark = 2
  elseif (f > 3) and (f < 4.5)  and (xl>xt) and  (yt > yl)then    ' GIS_YAC
    checkFark = 3
  else                                 '
    checkFark = -1
  end if
end function

function checkyazi(xt,yt,xl,yl,fark)
 if xt >= xl and fark > 0 and fark < 5 then
   checkyazi = 0
 elseif xt < xl and fark > 0 and fark < 5  then
   checkyazi = 1
 else
   checkyazi = -1
 end if
end function

const ATabaka = "PL_KONUT" ' Konut Tabakas�
const YTabaka = "SM_YAPI"  ' Yap� Bilgileri Tabakas�
const TYTabaka = "T_SM_YAPI" ' Tadilatl� yap�lar i�in Yap� bilgileri
const CTabaka = "SM_DAIRE"
'(01/04/2005) : Standart bir plan i�in bilgiler al�nabiliyor. Ancak Tadilatl� yap�lar ayr�ca al�nmal�.

sub main()
  with netcad
    .createLayer "GIS_ONC",red   ' De�i�kenler i�in Tabakalar olu�tur
    .createLAyer "GIS_YANC",red
    .createLayer "GIS_INNIZ",red
    .createLayer "GIS_KATADET",red
    'distPlanVarsToLayers Atabaka,YTabaka ' Tabakalar� Ay�r  'KATYAPI_AYRI
    'KatAdediYapibilgisiTek                                 'KATYAPI_BITISIK
    katAdediYapiBilgisiTek_Eyup_Rami                       'RAMI_ICIN
  end with
end sub

sub distPlanVarsToLayers(AlanTabaka,YaziTabaka)
dim o,oo,ooo,i,j,k,objLine,objText,cLine,cText,fark
dim clinex,cliney,ctextx,ctexty
    with netcad
      set ooo = .newobject
      set oo = .newobject
      set o  = .newobject
      .setfilter nothing,array(.foundLayer(AlanTabaka)),array(opline)
      do While .getNextObject2(o)
        i = .curobjpos
        k = 0
        if not .escpressed then
          .setfilter o.Limits,array(.foundLayer(YaziTabaka)),array(oline)
          do while .getNextObject2(oo)
            if (o.getobjectaspline.inpoly(oo.limits.cll)) and _
               (o.getobjectaspline.inpoly(oo.limits.cll)) and _
               (oo.tag = oline) then
              set objLine = oo
              'cline = getCenter(objLine.Limits.cll.x,objLine.Limits.cll.y, _
                                'objLine.Limits.cur.x,objLine.Limits.cur.y)

              clinex = getcentercoor(objLine.Limits.cll.x,objLine.Limits.cur.x)
              cliney = getcentercoor(objLine.Limits.cll.y,objLine.Limits.cur.y)
             set cline = .newc(cliney,clinex,0)
              .setfilter o.Limits,array(.foundLayer(YaziTabaka)),array(otext)
              while .GetNextObject2(ooo)
                'if (o.getobjectaspline.inpoly(ooo.limits.cll)) and _
                   '(o.getobjectaspline.inpoly(ooo.limits.cur)) and _
                    if (ooo.tag = otext) then
                  'cText = getCenter(ooo.limits.cll.x,ooo.limits.cll.y,ooo.limits.cur.x,ooo.Limits.cur.y)
                  ctextx = getcentercoor(ooo.Limits.cll.x,ooo.Limits.cur.x)
                  ctexty = getcentercoor(ooo.Limits.cll.y,ooo.Limits.cur.y)
                  set ctext = .newc(ctexty,ctextx,0)

                  fark = ncmath.Distance(ctext, cline, false)

'msgbox fark & "::::" & ooo.s & " ::: " & ctexty & " :::: " & cliney

               '   msgbox checkFark(fark,ctextx,ctexty,clinex,cliney)

                  if checkFark(fark,ctextx,ctexty,clinex,cliney) = 0 then
                    ooo.tabaka = .foundlayer("GIS_INNIZ")
                    .putobject .curobjpos,ooo
                  elseif checkFark(fark,ctextx,ctexty,clinex,cliney) = 1 then
                    ooo.tabaka =.foundlayer("GIS_KATADET")
                    .putobject .curobjpos,ooo
                  elseif checkFark(fark,ctextx,ctexty,clinex,cliney)= 2 then
                    ooo.tabaka = .foundlayer("GIS_OAC")
                    .putobject .curobjpos,ooo
                  elseif checkFark(fark,ctextx,ctexty,clinex,cliney) = 3 then
                    ooo.tabaka = .foundlayer("GIS_YANC")
                    .putobject .curobjpos,ooo
                  end if
                  .backmessage
                  .setmessage .curobjpos & "<=>" & fark
                  k = k +1
                end if
              wend
              .ResetFilter
            end if
          loop
          .resetFilter
        end if
      loop
      set o = nothing
      set oo = nothing
      set ooo = nothing
      .resetFilter
      .backmessage
      .setmessage "HAZIR ..."
      msgbox "De�i�tirilen obje say�s� : " & k
    end with
end sub

function getCenter(x1,y1,x2,y2)
  getCenter = (x1+x2)/(y1+y2)
end function

function getCenterCoor(a,b)
  getCenterCoor = (a+b)/2
end function

function checkFark(f,xt,yt,xl,yl)
  if (f > 1) and (f < 3.09) and (yt<yl) and ((xt=<xl) or (xt=>xl)) then        ' insaat nizam
    checkFark = 0
  elseif (f > 1) and (f < 3.2) and (yt>yl) then  ' Kat Adedi
    checkFark = 1
  elseif (f > 2.6) and (f < 17.60)  and (xl<xt) and (yt => yl)  then    ' GIS_AOC
    checkFark = 2
  elseif (f > 2.6) and (f < 4.5)  and (xl>xt) and  (yt > yl)then    ' GIS_YAC
    checkFark = 3
  else                                 '
    checkFark = -1
  end if
end function


function KatAdediYapibilgisiTek() ' Biti�ik Olan Yap�lar i�in
dim i,o,c
dim ColArr
  with netcad
    set o = .newObject()
    set c = .newObject()
    .setFilter nothing,CreateLayerList,array(oPline)
    while .getNextOBject2(o)
      .setFilter o.Limits,array(.foundLayer(CTabaka)),array(oCircle)
      reDim objArr(1)
      while .getNextObject2(c)
         'KatAdediYapibilgisiTek_SetLayers
         serachForTexts(c)
      wend
      .resetFilter
    wend
    .resetFilter
  end with
end function

function serachForTexts(c)
dim t,objArr
dim i,maxi,mini,midi,o
dim maxx,minx,cc,cor
  with netcad
    redim objArr(0)
    maxi = 0
    mini = 0

    maxx = 0
    minx = 99999999999
    set t = .newObject
    .setFilter c.Limits,array(.foundLayer(YTabaka)),array(oText)
    if not .escPressed then
      while .getNextOBject2(t)

         set cc = .GetTextRefPoint(t)

        if cc.x > maxx and (cc.x <> 0) then

          maxi = .curObjPos
          maxx = cc.x
        end if

        if cc.x < minx and (cc.x <> 0) then
          mini = .curobjPos
          minx = cc.x
        end if
        .setCurrentWindow t.Limits,true

   wend
   end if
    .resetFilter

    .setFilter c.Limits,array(.foundLayer(YTabaka)),array(oText)
    if not .escPressed then
      while .getNextOBject2(t)

       if .curObjPos <> mini and .curObjPos <> maxi then
         midi = .curObjPos
       end if

      wend
   end if
    .resetFilter


  if mini <> 0 and midi <> 0 and maxi <> 0 then
    set o = .getObject(mini)
    o.tabaka = .foundLAyer("GIS_YANC")
    .putObject mini,o

    set o = .getObject(maxi)
    o.tabaka = .foundLAyer("GIS_OAC")
    .putObject maxi,o


    i = split(.getObject(midi).s,"-")
    'msgbox i(0)
    set cor = .newc(.getTextRefPoint(.getOBject(midi)).y-1.5,.getTextRefPoint(.getOBject(midi)).x,0)
    set o = .MakeText(cor, i(0), 0,1, 2.1,0,0,.foundLayer("GIS_INNIZ"))
    .AddObject o

    set cor = .newc(.getTextRefPoint(.getOBject(midi)).y+1.5,.getTextRefPoint(.getOBject(midi)).x,0)
   on error resume next
    set o = .MakeText(cor, i(1), 0,1, 2.1,0,0,.foundLayer("GIS_KATADET"))
    .AddObject o

  end if
  end with
end function


function CreateLayerList()
dim i,arr
  reDim Arr(0)
  with ncLayerManager
    for i = 0 to .NumLayer-1
      if inStr(1,.layer(i).Name,"PL_") <> 0 then
        reDim preserve arr(ubound(arr)+1)
        arr(uBound(arr)) = i
      end if
    next
    CreateLayerList = arr
  end with
end function

function KatAdediYapibilgisiTek_SetLayers(arr)
dim i,maxi,mini,midi,o
  with netcad
    maxi = 1
    mini = 1

   if uBound(arr) = 3 then
     'msgbox arr(1).s
    for i = 0 to uBound(arr)-1
    'on Error Resume Next
    'msgbox arr(i).s
     if arr(i).GetObjectAsPline.CenterOfMass.x > arr(maxi).GetObjectAsPline.CenterOfMass.x and i<> maxi then
         maxi = i
       end if
    next

    for i = 0 to uBound(arr)-1
      if arr(i).getObjectAsPline.centerOfMass.x < arr(maxi).getObjectAsPline.centerOfMass.x and i<>mini then
         mini = i
      end if
    next

    for i = 0 to uBound(arr)-1
      if  i <> mini and i <> maxi then
        midi = i
      end if
    next


    end if
  end with
end function


function katAdediYapiBilgisiTek_Eyup_Rami()
dim i,o,c
dim ColArr
  with netcad
    set o = .newObject()
    set c = .newObject()
    .setFilter nothing,CreateLayerList,array(oPline)
    while .getNextOBject2(o)
      .setFilter o.Limits,array(.foundLayer(CTabaka)),array(oCircle)
      reDim objArr(1)
      while .getNextObject2(c)
         'KatAdediYapibilgisiTek_SetLayers
         SetTexts_eyup_rami(c)
      wend
      .resetFilter
    wend
    .resetFilter
  end with
end function


function SetTexts_eyup_rami(c)
dim t,objArr
dim i,maxi,mini,midi,o
dim maxx,minx,cc,cor
  with netcad
    redim objArr(0)
    maxi = 0
    mini = 0
    maxx = 0
    minx = 99999999999
    set t = .newObject

    .setFilter c.Limits,array(.foundLayer(YTabaka)),array(oText)
    if not .escPressed then
      while .getNextOBject2(t)
         set cc = .GetTextRefPoint(t)
        if cc.x > maxx and (cc.x <> 0) then
          maxi = .curObjPos
          maxx = cc.x
        end if
        if cc.x < minx and (cc.x <> 0) then
          mini = .curobjPos
          minx = cc.x
        end if
        .setCurrentWindow t.Limits,true
   wend
   end if
    .resetFilter
  if mini <> 0  and maxi <> 0 then
   ' set o = .getObject(maxi)
   ' if len(o.s) < 3 then
   '   o.tabaka = .foundLAyer("GIS_OAC")
   '   .putObject maxi,o
   ' end if
    i = split(.getObject(mini).s,"-")
    'msgbox i(0)


    set cor = .newc(.getTextRefPoint(.getOBject(mini)).y-1.5,.getTextRefPoint(.getOBject(mini)).x,0)
    set o = .MakeText(cor, i(0), 0,1, 2.1,0,0,.foundLayer("GIS_INNIZ"))
    .AddObject o


    set cor = .newc(.getTextRefPoint(.getOBject(mini)).y+1.5,.getTextRefPoint(.getOBject(mini)).x,0)
    on error resume next
    set o = .MakeText(cor, i(1), 0,1, 2.1,0,0,.foundLayer("GIS_KATADET"))

    .AddObject o
  end if
  end with
end function





Sub Main
    Dim a,b,c,m,alan,o,d,e,poly,s,y,z,regpoly,bd

    with netcad

     set BD = Netcad.NewBDialog("PAFTA ADINI G�R�N�Z")
     BD.Getstring "item","PAFTA ADI",0,10
     if BD.showmodal then


     set regPOLY = .newpoly()                                                     

    if .GetPolygon("alan �evir::",regPOLY) then


           .setfilter nothing, array(),array(opline)
        do
           set o=.getnextobject

           if o is nothing then
              exit do
           else
             end if
               
                set poly=.getplineext(o)
                 set c = poly.centerofmass
                 c.x=c.x-25
                 c.y=c.y-25

                   if regpoly.inpoly(c) then
                   set z=.newc(c.y+50,c.x+50,0)
                   set y= .MakeRect(c,z,BD.ValueByName("item"),.createlayer("KUTU_S�L",14))
                    .addobject y
                   end if

      loop

       end if
      end if

      msgbox ("KAP DOSYANIZI PAFTA �LE �L��K�LEND�REB�L�RS�N�Z." &chr(13)&chr(10)&"EKS�K PAFTA NUMARALI PARSELLER� PROJEDEN TAMAMLAYINIZ. "),64,"B�LG�LEND�RME"

  end with
end sub


' Yazan : 
' Tarih : 11.11.2013
' A��klama : 

Sub Main
Dim i
dim obj
dim layerno

     with NCLayerManager


       .Layer( .Find("GRID") ).name= "GRID_ED50"
       .Layer( .Find("GRIDKOOR") ).name= "GRIDKOOR_ED50"
       .layer(.Find("GRIDKOOR_ED50")).color=17
       .layer(.Find("GRID_ED50")).color=17
end with

    with Netcad

       .SetFilter nothing, array(.foundlayer("GRIDKOOR_ED50")), array(otext)

         do
            set obj=.getnextobject
            if obj is nothing then
            exit do
  end if

            obj.flags=1
            obj.s="("&obj.s&")"


             .PUTOBJECT .CUROBJPOS,OBJ



             loop




  end with
End Sub


' Yazan : 
' Tarih : 11.11.2013
' A��klama : 

Sub Main
Dim i
dim obj
dim layerno

     with NCLayerManager


       .Layer( .Find("GRID") ).name= "GRID_ITRF"
       .Layer( .Find("GRIDKOOR") ).name= "GRIDKOOR_ITRF"
       .layer(.Find("GRIDKOOR_ITRF")).color=0
       .layer(.Find("GRID_ITRF")).color=0
end with





End Sub

hatlara cephe yazmak amac�yla duzenlenm�st�r
' Yazan : KADR� BORA
' Tarih : 28.04.2011
' A��klama : hatlara cephe yazmak amac�yla duzenlenm�st�r
SUB Main
DIM sayac,obje,tabakano,yazi,p1,p2,cizgiuzunluk,ytext,cizgimerkezC,cizgiaci,fnt,xx,yy,z,pp,oo,ss,b,TabakaAl
with netcad
set p1=.newc(0,0,0) ' p1 isimli netcad koordinat objesi
set p2=.newc(0,0,0) ' p2 isimli netcad koordinat objesi
set cizgimerkezC=.newc(0,0,0)
fnt=0
'Get_Lyr TabakaAl
set ss = .NewSelectStatus
.SelectObjectInstant "�l��lendirilecek tabakada bir �izgi se�iniz...",1,array(),ss
set b= ss.objects(0)
Get_Lyr TabakaAl,b
with NCLayerManager
tabakano=b.tabaka
end with
for sayac=0 to .numobject-1
set obje=.getobject(sayac)
if obje.tag=oline and obje.tabaka=tabakano then
p1.x=obje.p1.x ' se�ilen �izginin ilk noktas�n�n x de�erini p1 in x de�erine ata
p1.y=obje.p1.y ' se�ilen �izginin ilk noktas�n�n y de�erini p1 in y de�erine ata
p2.x=obje.p2.x ' se�ilen �izginin ikinci noktas�n�n x de�erini p2 in x de�erine ata
p2.y=obje.p2.y ' se�ilen �izginin ikinci noktas�n�n y de�erini p2 in y de�erine ata
cizgimerkezC.x=(p2.x+p1.x)/2
cizgimerkezC.y=(p2.y+p1.y)/2
if (p2.y-p1.y)<>0 then
cizgiaci=((p2.x-p1.x)/(p2.y-p1.y))
cizgiaci=atn(cizgiaci)
else
cizgiaci=1.57
end if
xx=obje.p2.x-obje.p1.x
yy=obje.p2.y-obje.p1.y
z=(xx^2+yy^2)
cizgiuzunluk=sqr(z)
cizgiuzunluk=round(cizgiuzunluk,2)
ytext=trim(cstr(cizgiuzunluk))
pp=instr(ytext,".")
if pp=0 then
ytext=ytext & ".00"
else
oo=mid(ytext,len(ytext)-1,1)
if oo="." then ytext=ytext & "0"
end if
.addobject (.maketext(cizgimerkezC,ytext,0,fnt,2,cizgiaci,"7",TabakaAl))
end if
next
end with
set sayac=nothing
set obje=nothing
set tabakano=nothing
set yazi=nothing
set p1=nothing
set p2=nothing
set cizgiuzunluk=nothing
set ytext=nothing
set cizgimerkezC=nothing
set cizgiaci=nothing
set fnt=nothing
set xx=nothing
set yy=nothing
set z=nothing
set pp=nothing
set oo=nothing
set b=nothing
end sub
sub Get_Lyr (TabakaAl,b)
dim i,Dialog,ComboListesi
with netcad
For i=0 to NCLayerManager.NumLayer()-1 step 1
ComboListesi=ComboListesi+.layernameof(i)+" | "
next
ComboListesi=ComboListesi+.layernameof(i+1)
set Dialog = Netcad.NewBDialog("TABAKA SE��M�")
Dialog.GetRadio "item1","","AKT�F TABAKAYI KULLAN.. | SE��LEN ��ZG� TABAKASINI KULLAN.. ",-1
Dialog.GetString "item2","YEN� TABAKA OLU�TUR..", "", 15
Dialog.getCombo "item3","TABAKA SE�..",ComboListesi,0
if Dialog.showmodal then
if Dialog.ValueByName("item2") <> "" then
with NCLayerManager
.Add (Dialog.ValueByName("item2")), blue
TabakaAl = .Find(Dialog.ValueByName("item2"))
end with
end if
if Dialog.ValueByName("item1")=0 then TabakaAl=.GetCurrentLayer
if Dialog.ValueByName("item1")=1 then TabakaAl=b.tabaka
if Dialog.ValueByName("item3")<>0 then TabakaAl=Dialog.ValueByName("item3")
end if
end with
set i=nothing
set Dialog=nothing
set ComboListesi=nothing
end sub
' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

set c = .newc(0,0,0)

layerno=.createlayer("KROK�_BEYAN_YAZI",0)
if .SelectPoint("Nokta se�", c, 2) then
   set yaz1=.maketext(c, "''HESAP �ZET� EKTED�R.''",0,0,2.5,0,"1",layerno)

   .addobject(yaz1)


end if
set o = nothing




end with
end sub


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      'h.s=b.s
      h.p1.y=b.p1.y
      'h.sc=b.sc
      'h.p1.x=b.p1.x-15
      'h.angle=b.angle
      ' h.tabaka=b.tabaka
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB

  function layer_select(list,index)
dim tara,ad
with netcad
index=.numlayers-1
for tara=0 to index
list(tara+1,1)=.layernameof(tara)
next
end with
end function
sub main
dim dialog,list(300,2),tara,index,pagediv,pagecount,page,pagestart,lname,lcod,delcount
layer_select list,index
with netcad
pagediv=25
delcount=0
pagecount=round(index/pagediv,0)
if pagecount=0 then pagecount=1
pagestart=1
for page=1 to pagecount
set dialog = Netcad.NewBDialog("Tabaka Sil ["&pagestart&" - "&pagestart+pagediv&" aras�]")
for tara=pagestart to pagestart+pagediv
if list(tara,1)<>"" then
dialog.GetCheck "item"&tara,list(tara,1),0
end if
next
if dialog.showmodal then
for tara=pagestart to pagestart+pagediv
if list(tara,1)<>"" then
list(tara,2)=dialog.ValueByName("item"&tara)
end if
next
end if
pagestart=pagestart+pagediv+1
next
for tara=1 to index
if list(tara,2)=1 then
lname=list(tara,1)
with nclayermanager
lcod=.find(lname)
if lcod<>-1 then
.Delete .find (lname),true
delcount=delcount+1
netcad.netcadcommand("REGEN")
end if
end with
end if
next
if delcount<>0 then msgbox "Toplam "&cstr(delcount)&" Tabaka Silindi",64,"Tabaka Silme"
end with
end sub



   ' AMA� : Kapal� alan ��indeki yaz�y� GIS ba�lant� anahtar� de�i�kenine atar.
   ' G�RD� : Kapal� alan - �okludogru - KAD_PARSEL_PL
   '         TEXT - YAZI - KAD_PARSEL_NO
   'UYARI : Oncelikle parsel i�indeki �ift yaz�lar temizlenmi� olmal�d�r.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("��indeki Yaz�y� Vt Koduna Ata")          ' yeni dialog yarat
  BD.GetString  "Parsel","Poligon Tabakas�", "KAD_PARSEL_PL", 15
  BD.GetString  "Yazi","Yaz� Tabakas�", "KAD_PARSEL_NO", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.inPoly(by.p1) then
            .DrawObject Parsel, RED
             parsel.OBJname=by.s
             .putobject i,parsel
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


   ' AMA� : Kapal� alan ��indeki yaz�y� GIS ba�lant� anahtar� de�i�kenine atar.
   ' G�RD� : Kapal� alan - �okludogru - KAD_PARSEL_PL
   '         TEXT - YAZI - KAD_PARSEL_NO
   'UYARI : Oncelikle parsel i�indeki �ift yaz�lar temizlenmi� olmal�d�r.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("��indeki Yaz�y� Vt Koduna Ata")          ' yeni dialog yarat
  BD.GetString  "Parsel","Poligon Tabakas�", "KAD_PARSEL_PL", 15
  BD.GetString  "Yazi","Yaz� Tabakas�", "KAD_PARSEL_NO", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    'if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      'msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      'Exit Sub
    'End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.inPoly(by.p1) then
            .DrawObject Parsel, RED
             parsel.OBJname=by.s
             .putobject i,parsel
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


function CheckLAyerName(param,ly)
dim i,arr

  with ncLayerManager
    for i = 0 to .NumLayer-1
      if inStr(1,.layer(i).Name,param) <> 0 and netcad.LAyerNameOf(ly)=.layer(i).name then
        CheckLAyerName = 1
        exit function
      else
        CheckLAyerName = 0
      end if
    next
  end with
end function





   ' AMA� : Kapal� alan ��indeki yaz�y� objenin ad� de�erine atar.
   ' G�RD� : Kapal� alan - �okludogru - KAD_PARSEL_PL
   '         TEXT - YAZI - KAD_PARSEL_NO
   'UYARI : Oncelikle parsel i�indeki �ift yaz�lar temizlenmi� olmal�d�r.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("��indeki Yaz�y� Ad� olarak Atar")          ' yeni dialog yarat
  BD.GetString  "Parsel","Poligon Tabakas�", "KAD_PARSEL_PL", 15
  BD.GetString  "Yazi","Yaz� Tabakas�", "KAD_PARSEL_NO", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.inPoly(by.p1) then
            .DrawObject Parsel, RED
             parsel.pname=by.s
             .putobject i,parsel
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


'Tarih    : 23.06.2006
'Ama�     : B�R KAPALI ALAN ���NDE ��FT YAZI VARSA ALAN OBJES�N� PROBLEML� TABAKASINA ALIR

SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln
with netcad
  set BD = .NewBDialog("")
  BD.GetString  "Parsel","Alan Tabakas�", "ALAN", 15
  BD.GetString  "Yazi","Yaz� Tabakas�", "NO", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  for i = 0 to .numobject-1
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then
          .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject
         if by is nothing then
                    exit do
          else
           set pln=.GetPlineExt(parsel)
           if pln.InPoly(by.p1) then
             s=s+1
           end if
         end if
       Loop

       if s>0 then
         if s=1 then
           .ResetFilter
           s=0
           mode=0
           iy=""
          else
           parsel.tabaka=.CreateLayer("PROBLEML�",7)
           .PutObject i, parsel
           .ResetFilter
           s=0
           mode=0
           iy=""
         end if
        else
         parsel.tabaka=.CreateLayer("PROBLEML�",7)
         .PutObject i, parsel
         .ResetFilter
         s=0
         mode=0
         iy=""
       end if
    end if
    set by = nothing
  next
  set parsel=nothing
  .BackMessage
  .NetcadCommand("REGEN")
end with
END SUB


' Yazan : 
' Tarih : 09.07.2005
' A��klama : 

Sub Main
Dim i, o , tbk , tbk1, p, by, pln
  with Netcad
    'tbk =  .FoundLayer("0")
    tbk1 = .FoundLayer("1")
    .SetFilter nothing, array() , array (opline)
    do
      set o = .GetNextObject
      i =  .CurObjPos
      if o  is nothing then
    exit do
      end if
        .SetFilter .ObjectExtends(o), array (tbk1), array (otext)
          Do
         set by = .GetNextObject
         if by is nothing then
           exit do
          else
           set pln=.GetPlineExt(o)
           if pln.InPoly(by.p1) then
            .DrawObject o, RED
             o.objname= by.s
             .putobject i, o
           end if
         end if
       Loop
        .resetfilter
    loop
    .ResetFilter
  end with
End Sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

set secim = .NewSelectStatus()
set c = .newc(0,0,0)

top= 0

         set yazi = .NewSelectStatus

        while .SelectObjectInstant ("Ad� de�i�tirilecek yaz�y� se� ?",1,array(otext),yazi)
               set o = yazi.objects(0)



                    top=top+o.s





       wend


layerno=.FOUNDLAYER("NOT_IRTIFA")
if .SelectPoint("Nokta se�", c, 2) then
   set yaz1=.maketext(c, "BOTA� GENEL M�D. LEH�NE " & top &" m2",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz2=.maketext(c, "MUSTAK�L VE DA�M� �ST HAKKI VARDIR.  ",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   .addobject(yaz1)
   .addobject(yaz2)

end if
set o = nothing




end with
end sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad


set c = .newc(0,0,0)







 while.SelectPoint("Nokta se�", c, 2)

   set yaz1=.maketext(c,"(ITRF 96/39-3)",0,0,2,0,"1",.createlayer("text_datum",1))
   c.x=c.x-5
   .addobject(yaz1)
wend
end with
end sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o
dim a



with netcad

layerno=.createlayer("BA�LIK",0)

set c = .newc(0,0,0)



a=.getparam(94)/1000




 while.SelectPoint("Nokta se�", c, 2)

   set yaz1=.maketext(c, "SOME B�LG�LER� TABLOSU",0,1,2.7*a,0,"3",layerno)


   .addobject(yaz1)


  wend
                           '.MakeText(c, txt, flags,font, size,angle,just,tabaka)


end with
end sub

' B.G / 20050701 : KapiNoSayisallastir makrosu, Numarataj i�lerinde kullan�lmak
' �zere �zelle�tirildi.
'
' Performans�n artt�r�lmas� i�in veritaban� ba�lant�lar�n�n bir b�l�m�,
' Netcad dataset fonksiyonlar� ile, di�er bir b�l�m� ise veritaban�na ado ile
' olu�turulmu�tur.


' B.G / 20050718 : Dik d��me fonksiyonu poligonlar�n her segmenti i�in
' noktalara olan uzunluklar� hesapl�yor, minimum uzunluk al�n�yor.
'
' B.G / 20060128 : Numarataj i�in maksimum minimum kap� nolar� ve �iziim y�n� ile
' ilgili veriler veritaban�na yaz�l�yor
'
' B.G / 20060130 : B�l�m no ve b�l�m say� ekleme eklendi /
' B.G / 20060130 : Perfonrms i�in bolum no bol�m say� kald�r�ld�
' B.G / 20060130 : Numarataj i�in yola bakan bina say�s� otomatik dolduruluyor.
'
'

const dbPath = "\\aozkayapc\Z_BURNUGIS\NUMARATAJ\NUMARATAJ.mdb"

sub main
dim i,j,obj,sel,sset,val,skTable,skIdCol,skCol,myResult,segCol,skSegCol,os
dim s,scol,ds,p,f,k,idx,tmp,skYolAdiCol,segYolCol,yolTabaka,karr,kapiNoCol,parr
dim pnt,ln,skcls,lnx,z
  with netcad

    s    = "KAPI"           ' KAPI Tablosu Netcad S�n�f�
    skTable = "YOL"         ' YOL Tablosu
    skCls   = "YOLORTA"     ' YOL S�n�f�


    skCol = "YOL_ID"        ' SOKAK Tablosu Yol Id kolonu
    scol = "ULASIM_YOL_ID"  ' KAPI Tablosu Yol Id Kolonu

    segCol = "SEGMENT_ID"   ' KAPI Tablosu Segment Id Kolonu
    skIdCol = "PARCA_ID"    ' YOL Tablosu Segment Id S�n�f�

    skSegCol = "OBJECTID"   ' YOL Tablosu S�n�f�n Ba��l� Oldu�u Kolonunu ad�

    skYolAdiCol = "YOL_ISMI"' YOL Tablosu Yol Ad� Kolonu
    segYolCol = "YOL_ADI"   ' KAPI Tablosu yol Ad� Kolonu

    kapiNoCol = "KAPI_SIL"   ' Kapi no kolonu


    if .foundlayer("AKS_DUZENLEME") = -1 then .CreateLayer "AKS_DUZENLEME", red end if
    if .foundlayer("DIK") = -1 then .CreateLayer "DIK", red end if
    '.CLOSELAYER .FOUNDLAYER("DIK")
    set ds  = ncconman.finddatasetbyfc(false,s)
    set f = .newdatasetfilter
    f.filtertype = NC_GISDS_Region
    set sel = .newselectionset
    set p   = .newpoly
    set obj = .newobject
    p.clear
     if sel.Select ("�lk Objeyi Se�in",array(opline)) then
       pressF2Button
      if .GetPolygon("De�i�tirilecek de�er : "&val, p) then 

      for os = 0 to sel.ne-1
        k = 0
        idx = sel.GetSelectedObject(os,obj)
        val = obj.objname
        f.poly = p
        redim karr(6)
        karr(0) = 0
        karr(1) = 0
        karr(2) = 0
        karr(3) = 0
        karr(4) = ""
        karr(5) = 0
        redim parr(4)
        parr(0) = 0
        parr(1) = 0
        parr(2) = 0
        parr(3) = 0

        ds.setfilter( f )
        z=0
        if ds.open then
          myResult = getRequiredValue(val,skCol,skSegCol,skTable,skidcol,skYolAdiCol)

          while not ds.eof
                ' Max ve min kapi numaralar� i�in tan�mlamalar
            ' setIfInExtent obj,ds.geometry.o.p1
            'if obj.inObject(ds.geometry.o.p1) or obj.onObject(ds.geometry.o.p1) then
            if p.inpoly(ds.geometry.o.p1) then

                if isObject(AddSideShootLine(ds.geometry.o,obj.getObjectAsPline,.FOUNDLAYER("DIK"))) then
                  set pnt = AddSideShootLine(ds.geometry.o,obj.getObjectAsPline,.FOUNDLAYER("DIK"))
                end if

                   if isObject(pnt) then

                     if  abs(ncmath.distance(ds.geometry.o.p1,pnt,false)) < 200 then

                   k = k + 1
                   ds.edit
                    'msgbox ds.ColumnByName(kapiNoCol).Value

                   karr = setKapiNoArr(ds.ColumnByName(kapiNoCol).Value,karr)
                  on error resume next
                   set parr(karr(4)) = pnt

                   if karr(5) = 1 then
                     z = z + 1
                   end if

                   karr(5) = ""
                   tmp = split(myResult,"|")
                   'msgbox myresult
                   on error resume next
                   ds.columnbyname(scol).value = CLng(tmp(0))
                   on error resume next
                   ds.columnByName(segCol).Value = CLng(tmp(1))
                   if not isNull(tmp(2)) then
                    ds.columnByName(segYolCol).Value = tmp(2)
                   end if
                  setValuesForKapi ds.columnByName("OBJECTID").Value,tmp
                  ds.geometry.draw(red)
                end if
              end if
              end if
              ds.next
            wend
            'if k > 0 then
            '  obj.renk = red
            '  .putobject idx,obj
            'end if
            ds.resetfilter
            ds.close
            .backmessage
            .setmessage k & " adet objenin ilgili kolonuna " & val & " de�eri atand�..."
          'msgarr parr
          set obj = getSetDirectionForYol(parr,obj)
          'msgbox obj.renk
          .putobject idx,obj

          setValuesForYol val,karr,z
          end if
      next
     end if
    end if
  end with
end sub

function setIfInExtent(poly,p)
dim ext
   with netcad
     set ext = .getPlineExt(poly)
     if ext.limits.cll.x <= p.x and _
        ext.limits.cll.y >= p.y and _
        ext.limits.cur.x >= p.x and _
        ext.limits.cur.y >= p.y then

       msgbox "i�inde"
     else
       msgbox "d���nda"
     end if
   end with
end function

function getSetDirectionForYol(parr,obj)
dim ln,l1,l2,lnarr
dim res,p,i
  with netcad
   on error resume next
    set ln = getSetDirectionForyol_getBaseLine(parr,obj)

     l1 = ncmath.distance(ln.p1,obj.getobjectaspline.cor(0),false)
     l2 = ncmath.distance(ln.p1,obj.getobjectaspline.cor(obj.getobjectaspline.num-1),false)

    if findPositive(l1) < findPositive(l2) then
      obj.renk = 14
    else
      obj.renk = red
    end if
  set getSetDirectionForYol = obj
  end with
end function

function findPositive(val)
  if val < 0 then
    findPositive = val * -1
  else
    findPositive = val
  end if
end function

function getSetDirectionForyol_getBaseLine(parr,obj)
dim ln
  with netcad
  if isObject(parr(0)) and isObject(parr(1)) then
    set getSetDirectionForyol_getBaseLine = .MakeLine(parr(0), parr(1), 0, 0, 0)
    exit function
  elseif isObject(parr(2)) and isObject(parr(3)) then
    set getSetDirectionForyol_getBaseLine = .MakeLine(parr(2), parr(3), 0, 0, 0)
    exit function
  elseif isObject(parr(0)) and isObject(parr(3)) then
    set getSetDirectionForyol_getBaseLine = .MakeLine(parr(0), parr(3), 0, 0, 0)
    exit function
  elseif isObject(parr(2)) and isObject(parr(1)) then
    set getSetDirectionForyol_getBaseLine = .MakeLine(parr(2), parr(1), 0, 0, 0)
    exit function
  end if
  end with
end function

function getSetDirectionForyol_getBaseLine1(parr,obj)
dim ln
  with netcad
    if isObject(parr(0))  then
      msgbox 1
      set getSetDirectionForyol_getBaseLine = array(parr(0),0)
      exit function
    elseif isObject(parr(2)) then
      msgbox 2
      set getSetDirectionForyol_getBaseLine = array(parr(2),0)
      exit function
    elseif isObject(parr(1)) then
      msgbox 3
      set getSetDirectionForyol_getBaseLine = array(parr(1),1)
      exit function
    elseif isObject(parr(3))  then
      msgbox 4
      set getSetDirectionForyol_getBaseLine = array(parr(3),1)
      exit function
    end if
  end with
end function

function setValuesForKapi(value,arr)
dim ds,con,sql
  set con = adoConnectionObject(dbPath)
  sql = "Select * From KAPI where OBJECTID = "&value
  set ds = adoRecordsetObject(sql,con)
  ds("ULASIM_YOL_ID").Value = Clng(arr(0))
  ds("SEGMENT_ID").Value = Clng(arr(1))
  on error resume next
  ds("YOL_ADI").Value = arr(2)
  ds.update
  ds.Close
end function

function setValuesFromBinaToYol(yolid)
dim con,sql,ds 
set con = adoConnectionObject(dbPath)
 ' msgbox yolid

  sql = "SELECT DISTINCT KAPI.YAPI_ID, YOL.PARCA_ID"
  sql = sql + " FROM KAPI INNER JOIN YOL ON KAPI.SEGMENT_ID = YOL.PARCA_ID"
  sql = sql +  " GROUP BY KAPI.YAPI_ID, YOL.PARCA_ID"
  sql = sql + " HAVING  (((KAPI.YAPI_ID) Is Not Null) AND ((YOL.PARCA_ID)='"&yolid&"'))"
  'msgbox yolid
  'msgbox sql
'  sql = "SELECT Count(KAPI.YAPI_ID) AS cnt, YOL.YOL_ID FROM KAPI INNER JOIN YOL ON KAPI.ULASIM_YOL_ID = YOL.YOL_ID"
'  sql = sql +   " GROUP BY YOL.YOL_ID HAVING (((YOL.YOL_ID)="&yolid&"))"

  'msgbox sql
  set ds = adoRecordsetObject(sql,con)
  'msgbox ds.recordcount
  setValuesFromBinaToYol = ds.recordcount
end function




function setValuesForYol(value,arr,p)
dim ds,con,sql,sbin


  set con = adoConnectionObject(dbPath)
  sql = "Select * From YOL where OBJECTID like "&value
  'msgbox SQL
  set ds = adoRecordsetObject(sql,con)
    msgbox " all "&arr(0) & "   :  " & arr(1) & "  :  " & arr(2) & "  :  " & arr(3)
    ds("MIN_TEK_KAPI_NO").Value = arr(0)
    ds("MAKS_TEK_KAPI_NO").Value = arr(1)
    ds("MIN_CIFT_KAPI_NO").Value = arr(2)
    ds("MAKS_CIFT_KAPI_NO").Value = arr(3)
    sbin = setValuesFromBinaToYol(ds("PARCA_ID").Value)
    ds("CEPHE_ALAN_BINA").Value = sbin
    alerValues(array(arr(0),arr(1),arr(2),arr(3),sbin,p))
  ds.update
  ds.Close
end function

function alerValues(valarr)
dim dlg
  with netcad
    set dlg = .NewBDialog("Alerting")
    dlg.putPrompt("min tek : "&valarr(0))
    dlg.putPrompt("max tek : "&valarr(1))
    dlg.putPrompt("min �ift : "&valarr(2))
    dlg.putPrompt("max �ift : "&valarr(3))
    dlg.putPrompt("toplam yapi : "&valarr(4))
    if valarr(5)>0 then
    dlg.putPrompt("---------------------")
    dlg.putPrompt("Taplam  : "&valarr(5)&" adet kapinosuz nokta var")
    end if
    dlg.showmodal
   end with
end function

function createLineFromPoly(p)
  with netcad
    set createLineFromPoly = .MakeLine(p.Cor(0), p.Cor(p.Num-1), 0, 0,0)
  end with
end function

function getRequiredValue(objId,col,idCol,tableName,sokSegCol,sokAdiCol)
dim o,p,i,con,ds,sql,myResult
  with netcad
    set con = adoConnectionObject(dbPath)
    sql = "Select * From "&tableName&" where "&idCol&" like "&objId
        set ds = adoRecordsetObject(sql,con)
   '  msgbox sql
    'msgbox tablename & "  : " & idcol & "  :  " & objid
   ' msgbox ds(col).Value & "  :  " & ds(sokSegCol).Value & "  :  " & ds(sokAdiCol).Value

    myResult = ds(col).Value &"|"&ds(sokSegCol).Value&"|"&ds(sokAdiCol).Value
    getRequiredValue = myResult
    set ds = nothing
    set con = nothing
  end with
end function

function adoConnectionObject(dbPath)
dim conMain
   set conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function

function adoRecordsetObject(sql,connection)
dim tables
  set tables = CreateObject("ADODB.Recordset")
  tables.Open sql,connection,1,3
  set adoRecordsetObject =  tables
end function

function AddSideShootLine(p,l,layer)
dim MinIndex, MinDist, i,j,oi,oj,st,sl,DikNokta,MaxDikLength
dim y,lnarr,pntArr
  with netcad
    'msgbox 1
    reDim lnArr(0)
    reDim pntArr(0)
    for y = 1 to l.Num-1
      minDist = 99999999
      MaxDikLength = 100
      sl = NCMath.INV_Side_Length(p.p1, l.Cor(y-1), l.Cor(y))
      'msgbox sl
      if (sl)>0 then
        if (sl<ncMath.Distance(l.Cor(y-1), l.Cor(y), false)) then
          st =  NCMath.INV_Side_Tail(p.p1, l.Cor(y-1), l.Cor(y))

          if findPositive(st)<findPositive(MinDist) then
            MinDist = st
          end if
        end if
      end if
      'msgbox mindist
      if (findPositive(MinDist)<MaxDikLength) then

        sl = NCMath.INV_Side_Length(p.p1, l.Cor(y-1), l.Cor(y))

        'sl = abs(sl)
        set DikNokta = NCMath.Side_Shoot(l.Cor(y-1), l.Cor(y), sl, 0)
        '.drawobject .MakeLine(p.p1, DikNokta, Layer, 0,0),red
        redim preserve lnArr(Ubound(lnArr)+1)
        redim preserve pntArr(Ubound(pntArr)+1)
        lnArr(Ubound(lnArr)) =  sl
        set pntArr(Ubound(pntArr)) = DikNokta
      end if
    next
      ' .drawobject .MakeLine(p.p1, DikNokta, Layer, 0,0),red
      if not  isEmpty(findMinValueIndex(lnArr)(1)) then
        set DikNokta = pntArr(findMinValueIndex(lnArr)(0))
        .drawobject .MakeLine(p.p1, DikNokta, Layer, 0,0),red
        set  AddSideShootLine = diknokta
      else
        AddSideShootLine = DikNokta
      end if
  end with
end function

function findMinValueIndex(arr)
dim i,k,y,resArr
  k = 99999999
  y = 0
  reDim resArr(2)
  for i = 1 to Ubound(arr)
    if arr(i) < k then
      k = arr(i)
      y = i
      resArr(0) = y
      resArr(1) = k
    end if
  next
  findMinValueIndex = resArr
end function

sub pressF2Button
dim shell
  set Shell = CreateObject("WScript.Shell")
   Shell.SendKeys "%{F2}"
  set shell = nothing
end sub


function setKapiNoArr(val,arr)
dim valarr
  on error resume next
  valarr = split(val,"/")

  if ubound(valarr) > 0 then
    val = valarr(0)

  else
    if not isNumeric(val) then
      val = mid(val,1,len(val)-1)
    else
      val = val
    end if
  end if

  'msgbox val

  if val <> 0 then

    if (val mod 2 ) = 0 then

      if Cint(arr(3)) = 0 and Cint(arr(2)) = 0   then
        arr(3) = Cint(val)
        arr(2) = Cint(val)
      end if

      if CInt(val) >= CInt(arr(3))  then

        arr(3) = Cint(val)
        arr(4) = 3

      elseif CInt(val) <= CInt(arr(2)) then

        arr(2) = Cint(val)
        arr(4) = 2

      end if

    else

      if arr(1) = 0 and arr(0) = 0  then
        arr(1) = Cint(val)
        arr(0) = Cint(val)
      end if

      if CInt(val) >= CInt(arr(1)) then

        arr(1) = Cint(val)
        arr(4) = 1

      elseif CInt(val) <= CInt(arr(0)) then

        arr(0) = Cint(val)
        arr(4) = 0

      end if

    end if
  end if

  if not isNumeric(cint(val)) then
    'msgbox val
    arr(5) =  1
  else
     arr(5) = 0
  end if
  setKapiNoArr = arr
end function


function msgarr(arr)
dim res,i
  res = ""
  for i = 0 to UBound(arr)
    res = res & " : " & arr(i)
  next
  msgbox res
end function

'
' ERCUMENT KORKMAZ
' 05/12/2006
' Numarataj /  Kap� nolar� art�rarak yazma
'
Sub Main
Dim c1, c2, yaz, bd, boy, bas, tbk, tbki
  with Netcad
  set bd = Netcad.NewBDialog("Sec")
    BD.GetInteger "item1","Baslang�c De�er Giriniz ", 1
    BD.GetFloat   "item2","Yaz� Boyu Giriniz ", 2, 1
    BD.GetString  "item3","Tabaka Ad� Giriniz ", "KAPI_NO", 15
   if BD.showmodal then
    boy = BD.ValueByName("item2")
    bas = BD.ValueByName("item1")
    tbk = BD.ValueByName("item3")
    end if
      .CreateLayer tbk, 4
     tbki = .FoundLayer(tbk)
    set yaz = .NewObject
    set c1 = .newc(0,0,0)
    set c2 = .newc(0,0,0)
    while .SelectPoint("Nokta i�in bir Yer G�ster",c1,-1)
      set yaz =  .MakeText(c1, bas, 0,0, boy ,0,7,tbki)
      .addobject yaz
      bas = bas + 2
    wend
    set c1 = nothing
    set c2 = nothing
  end with
end sub


' Bir s�n�f�n id de�erini di�er bir s�n�f�n istenilen kolonuna yazma
' Se�im y�ntemi ile.
' B.G / 20050701
const dbPath = "E:\PROJELER\NUMARATAJ\YOLORTA.MDB"
sub main
dim i,j,obj,sel,sset,val,skTable,skIdCol,skCol,myResult,segCol,skSegCol
dim s,scol,ds,p,f,k,idx,tmp,skYolAdiCol,segYolCol
  with netcad

    s    = "KAPI"
    skCol = "E_YOL_ID"
    scol = "YOL_ID"
    segCol = "SEGMENT_ID"

    skTable = "yolorta"
    skIdCol = "OBJECTID"
    skSegCol = "E_PARCA_ID"
    skYolAdiCol = "E_YOL_ISMI"

    segYolCol = "YOL_ADI"


    if .foundlayer("AKS_DUZENLEME") = -1 then .CreateLayer "AKS_DUZENLEME", red end if
    set ds  = ncconman.finddatasetbyfc(false,s)
    set f = .newdatasetfilter
    f.filtertype = NC_GISDS_Region
    set sel = .newselectionset
    set p   = .newpoly
    set obj = .newobject
    p.clear
    if sel.Select ("�lk Objeyi Se�in",array(opline)) then
      if sel.ne = 1 then
        k = 0
        idx = sel.GetSelectedObject(0,obj)
        val = obj.objname
        if .GetPolygon("De�i�tirilecek de�er : "&val, p) then
          f.poly = p
          ds.setfilter( f )
          if ds.open then
            myResult = getRequiredValue(val,skCol,skIdCol,skTable,skSegCol,skYolAdiCol)
            while not ds.eof
              k = k + 1
              if p.inpoly(ds.geometry.o.p1) then
                ds.edit
                tmp = split(myResult,"|")
                ds.columnbyname(scol).value = tmp(0)
                ds.columnByName(segCol).Value = tmp(1)
                'msgbox tmp(2)
                ds.columnByName(segYolCol).Value = tmp(2)
                ds.geometry.draw(red)
              end if

              'Dik D�� Makrosu

              AddSideShootLine ds.geometry.o,createLineFromPoly(obj.getObjectAsPline),0

              ds.next

            wend
            'msgbox ds.recordcount
            if k > 0 then
              obj.tabaka = .foundlayer("AKS_DUZENLEME")
              .putobject idx,obj
            end if
            ds.resetfilter
            ds.close
            .backmessage
            .setmessage k & " adet objenin ilgili kolonuna " & val & " de�eri atand�..."
          end if
        end if
      else
        msgbox "Birden Fazla Obje Se�tiniz Tek obje Se�iniz..."
      end if
    end if
  end with
end sub

function createLineFromPoly(p)
  with netcad
    set createLineFromPoly = .MakeLine(p.Cor(0), p.Cor(p.Num-1), 0, 0,0)
  end with
end function

function getRequiredValue(objId,col,idCol,tableName,sokSegCol,sokAdiCol)
dim o,p,i,con,ds,sql,myResult
  with netcad
    set con = adoConnectionObject(dbPath)
    sql = "Select * From "&tableName&" where "&idCol&" Like '"&objId&"'"
    set ds = adoRecordsetObject(sql,con)
    myResult = ds(col).Value &"|"&ds(sokSegCol).Value&"|"&ds(sokAdiCol).Value
    getRequiredValue = myResult
    set ds = nothing
    set con = nothing
  end with
end function

function adoConnectionObject(dbPath)
dim conMain
   set conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function

function adoRecordsetObject(sql,connection)
dim tables
  set tables = CreateObject("ADODB.Recordset")
  tables.Open sql,connection
  set adoRecordsetObject =  tables
end function

function AddSideShootLine(p,l,layer)
dim MinIndex, MinDist, i,j,oi,oj,st,sl,DikNokta,MaxDikLength
  with netcad
    minDist = 99999999
    MaxDikLength = 50
    sl = NCMath.INV_Side_Length(p.p1, l.p1, l.p2)
    if sl>0 then
      if (sl<l.Length(false)) then
        st =  NCMath.INV_Side_Tail(p.p1, l.p1, l.p2)
        if abs(st)<abs(MinDist) then
          MinDist = st
        end if
      end if
    end if
    if (abs(MinDist)<MaxDikLength) then
      sl = NCMath.INV_Side_Length(p.p1, l.p1, l.p2)
      set DikNokta = NCMath.Side_Shoot(l.p1, l.p2, sl, 0)
      .AddObject .MakeLine(p.p1, DikNokta, Layer, 0,0)
    end if
  end with
end function



'Arazi kotu ile bina kotu kar��la�t�r�larak katy�ksekli�inin bulunmas�
sub main
dim dlg
  with netcad
    set dlg = .newbdialog("ARAZ� KOTU <> KAT Y�KSEKL���")
    dlg.GetFileName "fileName", "Dosya Ad�", "", "TXT Dosyalar�|*.TXT|Tum Dosyalar|*.*","TXT"
    if dlg.showModal then
      if dlg.valuebyname("fileName") <> "" then
        writeValuesToFile dlg.valuebyname("fileName")
      else
        msgbox "L�rfen Verilerin Yaz�labilece�i Bir Text Dosyas� Se�iniz !!!"
        exit sub
      end if
    end if
  end with
end sub

Sub writeValuesTofile(fname)
Dim c,cc,i,j,k,z,z1,PL1,obj1,sel,tx,tx1,kat
Const ForReading = 1, ForWriting = 2
Dim fso, f
Set fso = CreateObject("Scripting.FileSystemObject")
Set f = fso.OpenTextFile(fname, ForWriting, True)
f.WriteLine "BINA"&"   "&"ARAZI"&"  "&"FARK"&"  "&"KAT"

with Netcad
set SEL = .NewSelectionSet   ' Yeni kume yarat
set c=.NewC(0,0,0)
set obj1= .NewObject
if SEL.SELECT("KAT BILGILERI YAZILACAK OBJELERI SECINIZ",array(opline)) then  ' istenen turleri kumeye ekle
   for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
       j = SEL.GetSelectedObject(i, obj1)    ' objeyi ve gercek indeksini al
       set PL1=.GetPlineExt(obj1)
       set c=PL1.CenterOfMass
            z= .CalcZOf(c)
           z1=0
       for k = 0 to PL1.num-1 'Poly objesini olu�turam noktalar�n y�ksekliklerini topla
           z1=z1+PL1.cor(k).z
       next
       z1=z1/PL1.num   'Poly objesinin ortalama y�ksekli�inin bulunmas�
       kat=((z1-z)-0.6)/3 'poly objesinin �cgen modelden hesaplanan merkez kotu ile poly objesinin ortalama kotunun fark�ndan katy�ksekli�i
       if (Int(kat)+0.5)>kat then
          kat=Int(kat)
          else
          kat=Int(kat)+1
       end if
       if kat=0 then kat=1
       f.WriteLine ROUND(z1,2)&"   "&ROUND(z,2)&"  "&ROUND((z1-z),2)&"  "&kat
       set tx=.MakeText(c,kat,0,0,1,0,"M",0)
       .addobject(tx)
   next
end if
    set obj1=nothing
    set PL1=nothing
    set j = nothing
    set c = nothing
    set z=nothing
    set z1=nothing
  end with
End Sub

function openValuesFile(fileName)
dim shell

end function



Sub main
Dim i,j,o,p
   with Netcad
      for i = 0 to .numobject-1
        set o = .getobject(i)
        if o.tag = opline then
          set p = .getplineext(o)
          for j = 0 to p.num-1
              p.cor(j).z = 0
          next
          .putplineext o,p
          .putobject i,o
          set p = nothing
	elseif o.tag = oLine then          
	  o.p1.z = 0
          o.p2.z = 0
          o.p3.z = 0
          .putobject i,o
        end if
        .drawobject o,14
        set o = nothing
      next
   end with
End Sub


'TARIH       : 09.11.2004
'AMA�        : Belirli bir kota sahip bir noktan�n kotuna g�re
'              projede bulunan di�er noktalar�n kot de�erlerinin
'              ve kot kesit sembol�n�n projeye eklenmesi
'GEREKENLER  : "KOT" isminde bir blok objesi.
'HAZIRLAYAN  : Bar�� G�ral
'
'
'NetCAD �stanbul B�lge M�d�rl���
'
'
'
sub Main
dim sset,sel,refPoint,dg,newZ,oldZ,objZ,dh,obj,txt,blk,olcs,olcek,ftext,fblock,coor,fcoor,idx
dim i,j
  with netcad
    set refPoint = .NewObject
    set sset = .newSelectStatus
    set sel = .NewSelectionSet

    set dg = .newbDialog("Kota D���r")

    while .selectObjectinstant("R�latif Nokta Se�iniz....",1,array(opoint),sset)
      set refPoint = sset.Objects(0)
        oldZ = refpoint.p1.z
        dg.PutPrompt "Referans Kot : " & oldZ
        dg.GetString "newZ","Yeni Kot        :","10",15
        olcs="100"&"|"&"200"&"|"&"500"&"|"&"1000"&"|"&"2000"
        dg.getcombo "olcek","�l�ek            :",olcs,0
    wend



    if oldZ = empty then
      msgbox "R�latif Nokta Se�mediniz..."
      exit sub
    else
      if dg.Showmodal then
        newZ = dg.ValueByName("newZ")
        olcek = dg.ValueByName("olcek")
        if olcek = 0 then
          olcek = 100
        elseif olcek = 1 then
          olcek = 200
        elseif olcek = 2 then
          olcek = 500
         elseif olcek = 3 then
           olcek = 1000
         elseif olcek = 2000 then
           olcek = 2000
         end if

      end if
      set dg = nothing
      if newZ = empty then
        msgbox "Referans Kot Girmediniz..."
        Exit Sub
      else
        dh = oldZ-newZ
        ftext = (olcek*1.5)/1000
        fblock = (olcek*0.5)/1000
        fcoor  = (olcek*2)/1000

        if sel.select("Kota Getirilecek noktalar� se�iniz...",array(opoint)) then
         set obj = .newobject
        for i = 0 to sel.ne-1
         idx = sel.GetSelectedObject(i, obj)
          objZ = obj.p1.z - dh
          set coor = .newc(obj.p1.y,obj.p1.x+fcoor,obj.p1.z)
          set txt = .MakeText(coor,mid(objz,1,4), 0,0,ftext,0,"6",.CreateLayer("KOT_YAZI",16))
          set blk = .MakeBlock("KOT", obj.p1, 0, 0, fblock, fblock, .CreateLayer("KOT_BLOK",16))
          .addObject txt
          .addObject blk
          set txt = nothing
          set blk = nothing
          'set obj = nothing
        next

        end if
      end if
    end if
  end with
end sub


' Yazan : 
' Tarih : 08.03.2007
' A��klama : Bu makro Netcad �oklu do�rular�n�n ve do�rular�n�n
'            z koordinat de�erlerini 0 kotuna getirir.
'            Hasan Mutlu --  hasan.mutlu@netcad.com.tr


Sub Main
Dim i,obj ,j,pline,c,x,y,pline2 ,say
  with Netcad

       set obj = .Newobject
       set c=.Newc(0, 0, 0)
       set pline2=.NewPoly
       .SetFilter nothing, array(),array(opline,oline)

          while .GetNextObject2(obj)
             say=say+1
             if obj.tag=opline then
             set pline=obj.getObjectAsPline()

             for i=0 to pline.Num-1
                 x=pline.Cor(i).x
                 y=pline.Cor(i).y


                 set c=.Newc(y,x,0)
                 pline2.AddCoor(c)
             next
            elseif obj.tag=oline then
               obj.p1.z=0
               obj.p2.z=0
            end if

             .PutPlineExt obj, pline2

              j=.curobjpos
             .putobject j,obj
              pline2.Clear()

          wend
        .resetFilter
        set obj=nothing
  end with

  Msgbox say & " tane objenin z koordinatlar� 0 yap�lm��t�r."
End Sub


' A��klama : Bu makro Netcad complex kapal� �oklu do�rular�n�n, a��k �oklu do�rular�n ve do�rular�n
'            z koordinat de�erlerini 0 kotuna getirir.

Dim isCorrectClosedPoly,isCorrectLine,isCorrectOpenPoly

Sub Main
Dim i,obj,j,pline,c,x,y ,closedPolySay, dogruSay,openPolySay
Dim complexPoly,outColl,inColl,outNum,inNum,complexPoly2
dim objCount,objNum,newObj,sonucMesaj,dogruMesaj,closedPolyMesaj,openPolyMesaj

'd�zeltilen obje sayilaridir.
closedPolySay=0
dogruSay=0

'kullanici secimleridir
isCorrectClosedPoly=1
isCorrectOpenPoly=1
isCorrectLine=1

'kullanicidan hangi objelerin d�zeltilecegi bilgileri alinir.
if GetDialogResult=false then
  exit sub
end if

'kullanici hi�bir obje t�r�n� duzeltmeyi se�memi� ise i�lem iptal edilir.
if isCorrectClosedPoly=0 and isCorrectLine=0 and isCorrectOpenPoly=0 then
  exit sub
end if

  with Netcad
       set obj = .Newobject
       set c=.Newc(0, 0, 0)
       set complexPoly=.NewComplexPoly
       objCount=.NumObject
       for objNum=0 to objCount-1
       set obj=.GetObject(objNum)
          'kapal� �oklu do�rular �zerinde i�lem yap�l�r
          if obj.tag=opline then
             if (obj.flags and 1)>0  and isCorrectClosedPoly=1 then

             'yeni complexpoly yaratilir
             set complexPoly2=.NewComplexPoly
             set complexPoly=obj.GetObjectAsComplexPline()

             'd�� polyline lar�n z de�erleri s�f�rlan�r
             set outColl=complexPoly.outs
             if outColl.Num>0 then
             for outNum=0 to outColl.Num-1
              set pline=outColl.get(outNum)
              SetPlineZero pline
              complexPoly2.outs.add pline
              pline.destroyPoly()
              set pline=nothing
             next
             end if

             'i� polyline lar�n z de�erleri s�f�rlan�r
             set inColl=complexPoly.ins
             if inColl.Num>0 then
             for inNum=0 to inColl.Num-1
               set pline=inColl.get(inNum)
               for i=0 to pline.Num-1
                 pline.Cor(i).z=0.0
                next
                complexPoly2.ins.add pline
               pline.destroyPoly()
              set pline=nothing
             next
             end if

             'yeni complex polyline Netcad e eklenir
             set newObj= obj.GetCopy
             .AddComplexPoly newObj, complexPoly2

             'de�i�en obje say�s� i�in bir artt�r�l�r
             closedPolySay=closedPolySay+1

             'kullan�lan objeler bellekten silinir.
             set complexPoly=nothing
             set inColl=nothing
             set outColl=nothing
             set complexPoly2=nothing
             set newObj=nothing

             'eski obje mutlaka silinmelidir.
             .delObject objNum,obj

             elseif (obj.flags and 1)<1 and isCorrectOpenPoly=1 then  'a��k �oklu do�rular�n z degerlerini sifirlar
               if MakeZZeroOpenPoly(obj,objNum)=true then
                 openPolySay=openPolySay+1
               end if
             end if
          elseif obj.Tag=oline and isCorrectLine=1 then
             'dogrularin kotlari sifirlanir.
             obj.p1.z=0
             obj.p2.z=0
             .PutObject objNum, obj
             dogruSay=dogruSay+1
          end if
        set obj=nothing
      next
  end with

  'sonuc mesajlari duzeltilir.
  dogruMesaj=dogruSay & " tane do�runun z koordinat� s�f�rlanm��t�r."
  closedPolyMesaj= closedPolySay & " tane kapal� �oklu do�runun z koordinat� s�f�rlanm��t�r."
  openPolyMesaj=openPolySay & " tane a��k �oklu do�runun z koordinat� s�f�rlanm��t�r."

  if isCorrectClosedPoly=1 then
    if sonucMesaj<>"" then
      sonucMesaj=sonucMesaj & closedPolyMesaj & VbCrLf
    else
      sonucMesaj=closedPolyMesaj & VbCrLf
    end if
  end if

  if isCorrectOpenPoly=1 then
    if sonucMesaj<>"" then
      sonucMesaj=sonucMesaj &  openPolyMesaj & VbCrLf
    else
      sonucMesaj=openPolyMesaj & VbCrLf
    end if
  end if

  if isCorrectLine=1 then
    if sonucMesaj<>"" then
      sonucMesaj=sonucMesaj &  dogruMesaj & VbCrLf
    else
      sonucMesaj=dogruMesaj & VbCrLf
    end if
  end if

  'mesaj gosterilir.
  Msgbox sonucMesaj,0,"Netcad"
End Sub

sub SetPlineZero(pline)
dim i
  if not pline is nothing then
    for i=0 to pline.Num-1
      pline.Cor(i).z=0.0
    next
  end if
end sub

function GetDialogResult
Dim dlg
with Netcad
  set dlg = .newbdialog("Kot S�f�rlama Makrosu")
  dlg.GetCheck "cdogruChk", "Kapal� �oklu do�rular�n kotlar�n� s�f�rla",isCorrectClosedPoly
  dlg.GetCheck "acikCDogru","A��k �oklu do�rular�n kotlar�n� s�f�rla",isCorrectOpenPoly
  dlg.GetCheck "dogruChk", "Do�rular�n kotlar�n� s�f�rla",isCorrectLine

  if dlg.showmodal then
     isCorrectClosedPoly=dlg.valueByName("cdogruChk")
     isCorrectOpenPoly=dlg.valueByName("acikCDogru")
     isCorrectLine=dlg.valueByName("dogruChk")
  else
      GetDialogResult=false
      exit function
  end if
end with
  GetDialogResult=true
end function

function MakeZZeroOpenPoly(polyObj,objNum)
  Dim pline,plineNew,x,y,res,i,c
  set pline=polyObj.getObjectAsPline()
  with Netcad
    if not pline is Nothing then
      set plineNew=.NewPoly
      for i=0 to pline.Num-1
        x=pline.Cor(i).x
        y=pline.Cor(i).y
        set c=.Newc(y,x,0)
        c.flag=pline.Cor(i).flag
        plineNew.AddCoor(c)
      next
      .PutPlineExt polyObj, plineNew
      .PutObject objNum,polyObj
      res=true
    end if
  end with
  MakeZZeroOpenPoly=res

end function


' Yazan : SYILDIRIM
' Tarih : 04.02.2006
' A��klama : Bitisik yer kotu yaz�lar�ndan kotlu nokta uretir.
' NOT      : Tum yaz�lar�n uygulama noktas�, yazilarin merkezi olmalidir

Sub Main
Dim i,ktabaka,ntabaka,o,p,c
ktabaka = "0" 'kot yaz� tabakas�
ntabaka = "NOKTA" 'nokta tabakas�

  with Netcad
    .setfilter nothing,array(.foundlayer(ktabaka)),array(otext)
    if .foundlayer(ntabaka) = -1 then .createlayer ntabaka,0
    set o = .newobject()
    while .getnextobject2(o)
      set c = .newc(o.p1.y,o.p1.x,CDbl(o.s))
      set p = .MakePoint(c, "", "", .foundlayer(ntabaka))
      .addobject p
    wend
    .resetfilter
  end with
End Sub

' Tarih :28.11.2003
' Ama� : Yer kotu yaz�lar�na kot noktalar� �retilmesi.
'        Microstation dan gelen 123.12 kot yaz�lar�n�n '.' s�na kot noktas� atar.
' Girdiler : Yer kotu yaz�lar�
' Uyar�lar : Ekranda sadece yerkotu a��k b�rak�l�r
'           TIN isimlitabakaya kotlu noktalar uretilir.

 ' Konumsal olarak degil,objenin creat edilme s�ras�na g�re cal�s�r.
 ' 123.12 Ekranda once nokta sonra tamsay� 123 ve decimal 12 yarat�ld�g�ndan
 ' yola c�karak kot yaz�lar�n�n '.' Noktan�n X,Y degeri ile text degerinden Z degeri elde edilir.

' YaziyaKotNoktalariUret.nvb 'den fark� yaz� objelerinin  Z degerlerinin 0 olmas�
' nedeniyle haz�rlanm��t�r.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM KI, KP, KD, Kot, Tabaka, i
Dim BD


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("sundurma ve pafta kenarlar�ndaki cift yazi temizle")          ' yeni dialog yarat
  BD.GetString  "Tabaka","Tabaka", "YERKOTU", 20
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    Tabaka=.FoundLayer(BD.ValueByName("Tabaka"))
    if  Tabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set KP = .getobject(i)         ' i. objeyi al
    if KP.tag = otext and KP.tabaka=Tabaka  then
      if KP.s="." then
        set KI=.getobject(i+1)
        set KD=.getobject(i+2)
        kot=KI.s&"."&KD.s
        on error resume next
        KP.P1.Z=KOT
        .AddObject .MakePoint(KP.P1,"","",.CreateLayer("TIN", 2))

      end if
    end if
    set KI=nothing
    set KP=nothing
    set KD=nothing
  next



  .BackMessage
  .NetcadCommand("REGEN")
end with
END SUB



' Yazan : 
' Tarih : 30.09.2013
' A��klama : 

Sub Main
Dim i,obj

dim wLayerIndex1 
dim wLayerIndex2 
dim wLayerIndex3 
dim wLayerIndex4 
dim wLayerIndex5 
dim wLayerIndex6 
dim wLayerIndex7 
dim wLayerIndex8 
dim wLayerIndex9 
dim wLayerIndex10
dim wLayerIndex11
dim wLayerIndex12
dim wLayerIndex13
dim wLayerIndex14
dim wLayerIndex15
dim wLayerIndex16
dim wLayerIndex17


  with Netcad
  dim obj1

       .setfilter nothing, array(.foundlayer("TXT_ALANLAR"),_
                                  .foundlayer("PAR_NO_ADA")),array(otext)
                DO
                SET OBJ1=.GETNEXTOBJECT

                IF OBJ1 IS NOTHING THEN
                   EXIT DO
                ELSE
                .DRAWOBJECT OBJ1,10
                obj1.s=replace(obj1.s,"Daimi �rt","�ST HAKKI")
                obj1.s=replace(obj1.s,"ADA:","")
               .PUTOBJECT .CUROBJPOS,OBJ1
                end if

                loop

       .resetfilter

 wLayerIndex1 =  .FoundLayer("BEY_CERCEVE")
 wLayerIndex2 =  .FoundLayer("BEY_INCE")
 wLayerIndex3 =  .FoundLayer("BEY_YAZI")
 wLayerIndex4 =  .FoundLayer("LEJANT_ADAPAR")
 wLayerIndex5 =  .FoundLayer("LEJANT_BILGI")
 wLayerIndex6 =  .FoundLayer("LEJANT_CINSI")
 wLayerIndex7 =  .FoundLayer("LEJANT_MALIK")
 wLayerIndex8 =  .FoundLayer("LEJANT_MLK_EK")
 wLayerIndex9 =  .FoundLayer("LEJANT_PAFTA")
 wLayerIndex10 =  .FoundLayer("LEJANT_TAPUALAN")
 wLayerIndex11 =  .FoundLayer("NOT_IRTIFA")
 wLayerIndex12 =  .FoundLayer("SILGI")
 wLayerIndex13 =  .FoundLayer("LEJANT_BILGI2")
 wLayerIndex14 =  .FoundLayer("NOT_HATA")
 wLayerIndex15 =  .FoundLayer("GEC_IRTIFA")
 wLayerIndex16 =  .Foundlayer("DAI_IRTIFA")
 wLayerIndex17 = .Foundlayer("LEJANT_BILGI1")
        .setfilter nothing, array(.foundlayer("ALN_GECICI_IRTIFA")),array(opline)

        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
            obj.lt=2
           end if
          .PUTOBJECT .CUROBJPOS,OBJ
        loop
    .resetfilter


  with NCLayerManager


         if wLayerIndex1 < 0 then
         else
     .Delete .Find("BEY_CERCEVE"), TRUE
	end if     
     
     if wLayerIndex2 < 0 then
         else
     .Delete .Find("BEY_INCE"), TRUE
     end if  
     
     if wLayerIndex3 < 0 then
         else
     .Delete .Find("BEY_YAZI"), TRUE
     end if  
     
     if wLayerIndex4 < 0 then
         else
     .Delete .Find("LEJANT_ADAPAR"), TRUE
     end if  
     
     if wLayerIndex5 < 0 then
         else
     .Delete .Find("LEJANT_BILGI"), TRUE
     end if  
     
     if wLayerIndex6 < 0 then
         else
     .Delete .Find("LEJANT_CINSI"), TRUE
     end if  
     
     if wLayerIndex7 < 0 then
         else
     .Delete .Find("LEJANT_MALIK"), TRUE
     end if  
     
     if wLayerIndex8 < 0 then
         else
     .Delete .Find("LEJANT_MLK_EK"), TRUE
     end if  
     
     if wLayerIndex9 < 0 then
         else
     .Delete .Find("LEJANT_PAFTA"), TRUE
     end if  
     
     if wLayerIndex10 < 0 then
         else
     .Delete .Find("LEJANT_TAPUALAN"), TRUE
     end if  
     
     if wLayerIndex11 < 0 then
         else
     .Delete .Find("NOT_IRTIFA"), TRUE
     end if  
     
     if wLayerIndex12 < 0 then
         else
     .Delete .Find("SILGI"), TRUE
     end if  
     
     if wLayerIndex13 < 0 then
         else
   .Delete .Find("LEJANT_BILGI2"), TRUE
     end if  
     
     if wLayerIndex14 < 0 then
         else
     .Delete .Find("NOT_HATA"), TRUE
     end if 
     
     if wLayerIndex15 < 0 then
         else
     .delete .find("GEC_IRTIFA"),TRUE
     end if

 if wLayerIndex16 < 0 then
         else
     .delete .find("DAI_IRTIFA"),TRUE
end if   
 if wLayerIndex17 < 0 then
         else
     .delete .find("LEJANT_BILGI1"),TRUE
end if   

     Netcad.NetcadCommand "REGEN"
end with

  end with
End Sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

set c = .newc(0,0,0)

layerno=.createlayer("KROK�_BEYAN_YAZI",0)
if .SelectPoint("Nokta se�", c, 2) then
   set yaz1=.maketext(c, "''KROK�S� EKTED�R''",0,0,2.5,0,"1",layerno)

   .addobject(yaz1)


end if
set o = nothing




end with
end sub


'Ama�: �ak��an yaz�lar� bulmak i�in kullan�l�r
'Girdi : Text - Yaz�

Sub Main
Dim i,j,o,p,obj
   with Netcad
      set obj = .Newobject
      for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        set o = .getobject(i)         ' i. objeyi al
        if o.tag = otext then        ' yaz� m� ?
          .SetFilter o.Limits, array(),array()
          while .GetNextObject2(obj)
             if obj.tag=otext then
                .drawobject o, Red
                o.tabaka=1
             end if
          wend
          .ResetFilter
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
      next
   end with
End Sub



'***************************************
'*    Visual Basic Script Language     *
'* Predefined Constants and Functions  *
'*          Netcad For Windows         *
'***************************************
' BG,2005.02.09,function openDirDialog(title) added

Option Explicit

'*** Renkler
const Black        = 0
const Blue         = 1
const Green        = 2
const Cyan         = 3
const Red          = 4
const Magenta      = 5
const Brown        = 6
const LightGray    = 7
const DarkGray     = 8
const LightBlue    = 9
const LightGreen   =10
const LightCyan    =11
const LightRed     =12
const LightMagenta =13
const Yellow       =14
const White        =15

'*** Global Object Tag Constants
const  odeleted   =  0
const  opoint     =  1
const  oline      =  2
const  ocircle    =  3
const  oarc       =  4
const  otext      =  5
const  oshape     =  6
const  opline     =  7
const  ospiral    =  8
const  oizohdr    =  9
const  orectangle = 10
const  ostpafta   = 11
const  otriang    = 12
const  oblock     = 13
const  omark      = 14

'*** Object tag names
Dim objectnames
objectnames = array("SiLiNDi","NOKTA","DO�RU","DAiRE","YAY","YAZI",_
                          "�EKiL","�DOGRU","SP�RAL","E�R�","KUTU","STPAFTA",_
                          "��GEN","BLOK","MARK")

'*** MakePline Modlari, (flag parametresi)
'*** Istenenleri toplayarak kullanabilirsiniz
const  POLYCLOSED =  1   ' kapali cokludogru
const  POLYFILLED =  2   ' tarali
const  POLYFIT1   =  4   ' Spline yumusat
const  POLYARROW  =  8   ' Ok ciz
const  POLYNAME   = 16   ' Adini goster
const  POLYNOCON  = 32
const  POLYFIT    = 64   ' Noktadan gecen Yumusatma

'*** Pline.Operation Modlar�
const polyintersect = 0   ' Kesi�imleri
const polysubtract  = 1   ' 1.den 2.yi ��kart
const polyunion     = 2   ' Birle�imleri

'*** Message modes
const  NCM_INFO    = 0
const  NCM_WARNING = 1
const  NCM_ERROR   = 2
const  NCM_NONE    = 3

'*** Cursorler
const  cursordefault        = -1
const  cursorbox            =  1
const  cursorcross          =  2
const  cursorbcross         =  4
const  cursorfcross         =  8
const  cursorcrosswpoint    = 16
const  cursorintbox         = 32
const  cursornocross        = 64
const  cursornearest        = 128

const  cursorLargeSnap      = 8192
const  cursorOrtho          = 131072

'*** Undo modes
const  undo         = 0
const  beginblock   = 1
const  endblock     = 2
const  beginadd     = 3
const  endadd       = 4
const  endbookkeep  = 5
const  initbookkeep = 6
const  clearundo    = 7

'*** FileTypes
const  fs_none      = 0
const  fs_bnetcad   = 1   ' *.NCZ
const  fs_netcad    = 2   ' *.PLT
const  fs_autocad   = 3   ' *.DXF
const  fs_microst   = 4   ' *.DGN, *.PLN
const  fs_text      = 5
const  fs_nokta     = 6   ' *.NOK
const  fs_pafta     = 7
const  fs_block     = 8

'*** Netcad.GetParam ve SetParam da kullanilir. (sonuc veya parametresi string)
const  PNC_MAXPOINTNUM       = 0003 'en buyuk nokta no
const  PNC_MAXAREANUM        = 0004 'en buyuk alan no
const  PNC_DBASE             = 0016
const  PNC_DBASELINKF        = 0017

'*** Netcad.GetParam da kullanilir (sonuc string doner)
const  PNC_PROGRAMNAME       = 0005 'Netcad For Windows
const  PNC_VERSION           = 0006
const  PNC_VERSIONDATE       = 0007
const  PNC_USERNAME          = 0008
const  PNC_NETCADDIR         = 0009 'netcad dir
const  PNC_AUXDIR            = 0010 'secme
const  PNC_TEMPDIR           = 0011 'temp
const  PNC_FONTDIR           = 0012 'fontlar
const  PNC_SHAPEDIR          = 0013 'semboller
const  PNC_MODULDIR          = 0014 'modul baslangic dir
const  PNC_BMP3DDIR          = 0015 '3d bitmaps here
const  PNC_CURRENTFILE       = 0034 'Current Project File Name

'*** Netcad.GetParam da kullanilir (sonuc integer doner)
const  PNC_CURFONTREF        = 0038 'get current fontfile ref no
const  PNC_CURSHAPEREF       = 0039 'get current shapefile ref no
const  PNC_DOM               = 0049 'dilim orta meridyeni
const  PNC_DOMSCALE          = 0072 'dilim orta meridyeni olcegi 0 veya 1
const  PNC_ZEROALL           = 0086 'aktif projedeki tum objeleri ve tabakalari siler
const  PNC_VPORTSCALE        = 0087 'Current projenin cizim olcegi

'*** Netcad.SetParam da kullanilir (parametresi integer)
const  PNC_FASTIZO           = 0021 'set/reset fast izolines 0-1
const  PNC_FASTIZODELTA      = 0022 'set fast izoline delta (m)
const  PNC_PNTNAMES          = 0025 'Nokta adlarinin ac kapa 0-1
const  PNC_PNTZS             = 0026 'Nokta kotlarini ac kapa 0-1
const  PNC_PNTCODES          = 0027 'Nokta kodlarini ac kapa 0-1
const  PNC_PLINENAME         = 0028 'Alan adlarinin ac kapa  0-1
const  PNC_PLINEZS           = 0029 'Alan kotlari ac kapa    0-1
const  PNC_PLINEHATCHS       = 0030 'Alan taramalari ac kapa 0-1
const  PNC_BLASTRO           = 0031 'Blastrolar acik kapali  0-1
const  PNC_WIDTHS            = 0032 'Kalin obje cizimi on off 0-1
const  PNC_ARASTERMARK       = 0033 'aktif raster isaretle 0-1
const  PNC_RAWADD            = 0037 'Row Add ac kapa 0-1
const  PNC_SNAPNODEOF        = 0041 'Asagidakiler Snap modlarini acar/kapar 1-0
const  PNC_SNAPENDOF         = 0042
const  PNC_SNAPINTOF         = 0043
const  PNC_SNAPINSOF         = 0044
const  PNC_SNAPMIDOF         = 0045
const  PNC_SNAPGRID          = 0046
const  PNC_SNAPORTHO         = 0047
const  PNC_SNAPNEAREST	     = 0085
const  PNC_SNAPGRIDSPACING   = 0048 'grid araligini acar kapar-netcad icerisinde Ortho 0-1
const  PNC_PNC_AUTOSAVETIME  = 0084 'otomatik saklama zamanini set/get eder. 0 verirsen kapanir.
const  PNC_0WPIXEL           = 0088 'Obje cizim pixel kalinligini ayarlar

'*** Netcad.XSetParam da kullanilir
const  PNX_SHOWCMINSPECTORDLG = 0011 'no param, when set, next AddObjectDB opens a modal inspector dlg. After dlg, its reset. So set before every call.

'*** Macro Sonuc Dosya Bilgilerini Okumak ve Yazmak icin Sabitler Degerler
const  MACRO_SONUC_DOSYA_ADI   = "MACRO_SONUC_DOSYA_ADI"
const  MACRO_SONUC_DOSYA_TIPI  = "MACRO_SONUC_DOSYA_TIPI"

'*** Macro Sonuc Dosya tipleri
const  MACRO_SONUC_DOSYA_JPEG  = "image/jpeg"
const  MACRO_SONUC_DOSYA_HTML  = "text/html"

'*** Netcad.SaveCurWinImage() komutundaki imagetype degerleri
const  IMAGE_ECW   = "ECW"
const  IMAGE_BMP   = "BMP"
const  IMAGE_JPG   = "JPG"
const  IMAGE_PNG   = "PNG"
const  IMAGE_PCX   = "PCX"
const  IMAGE_TIF   = "TIF"
const  IMAGE_TIFG4 = "TIFG4"
const  IMAGE_TGA   = "TGA"

'***  GeoRaster ve RasterProcessor.SaveToFile() komutundaki Exparams degerleri
const  RASTER_UNCOMP    = "UNCOMP$"   'uncompressed
const  RASTER_PACKBITS  = "PACKBITS$" 'packbits
const  RASTER_JPEG      = "JPEG$" 'JPEG
const  RASTER_G4        = "G4$" 'G4
const  RASTER_LOSSLESS  = "LOSSLESS$" 'Lossless Compression
const  RASTER_COMPQNN   = "COMPQNN$" 'Comp Ratio
const  RASTER_GEOTIFF   = "GEOTIFF$" 'geoTiff
const  RASTER_TIFF      = "TIFF$" 'Tiff

'*** ColumnMC.ColType degerleri
const  CT_OTHER    = -1
const  CT_INTEGER  = 0
const  CT_FLOAT    = 1
const  CT_DATETIME = 2
const  CT_STRING   = 3
const  CT_BLOB     = 4

'*** NCDatasetFilter.FilterType degerleri
const NC_GISDS_Region              = 115
const NC_GISDS_IncludePoint        = 116
const NC_GISDS_JustSQL             = 117
const NC_GISDS_Polygon             = 6

'*** NetcadGeometry.GeometryType degerleri
const FC_Undef = 7
const FC_Point = 1
const FC_Line  = 2
const FC_Area  = 4


'*** NetcadReferenceMC.IsSupport() keyword degerleri
const Macro_Support_Grid         = "GRID"
const Macro_Support_FindZ        = "FINDZ"
const Macro_Support_ShortestPath = "SHORTEST"
const Macro_Support_Network      = "NETWORK"
const Macro_Support_Spatial      = "SPATIAL"
const Macro_Support_GeoRaster    = "GEORASTER"

'*** Netcad.newRasterProcessor(const ProcName: WideString) Procname degerleri
const RASTER_PROC_Median             = "MedianEx"
const RASTER_PROC_Destripe           = "DestripeEx"
const RASTER_PROC_PCA                = "PCAEx"
const RASTER_PROC_BandDecomp         = "BandDecomposeEx"
const RASTER_PROC_BandComp           = "BandComposeEx"
const RASTER_PROC_LandsatCalib       = "LandsatCalibrationEx"
const RASTER_PROC_BadPixel           = "BadPixelReplacementEx"
const RASTER_PROC_Fusion             = "FusionEx"
const RASTER_PROC_ReducePromoseColor = "ReducePromoteColorEx"
const RASTER_PROC_ConvertToGrayScale = "Convert2GrayScaleEx"
const RASTER_PROC_ColorSpace         = "ColorSpaceTransitionEx"
const RASTER_PROC_RGBtoHSV           = "RGB2HSVEx"
const RASTER_PROC_HSVtoRGB           = "HSV2RGBEx"
const RASTER_PROC_RGBtoHLS           = "RGB2HLSEx"
const RASTER_PROC_HLStoRGB           = "HLS2RGBEx"
const RASTER_PROC_IHStoRGB           = "IHS2RGBEx"
const RASTER_PROC_RGBtoIHS           = "RGB2IHSEx"
const RASTER_PROC_RGBtoCMY           = "RGB2CMYEx"
const RASTER_PROC_CMYtoRGB           = "CMY2RGBEx"
const RASTER_PROC_RGBtoCMYK          = "RGB2CMYKEx"
const RASTER_PROC_CMYKtoRGB          = "CMYK2RGBEx"
const RASTER_PROC_Histogram          = "HistogramStrecthingEx"
const RASTER_PROC_ContrastAdjust     = "ContrastAdjustmentEx"
const RASTER_PROC_AntiAlias          = "AntiAliasingEx"
const RASTER_PROC_Resize             = "ResizingImageEx"
const RASTER_PROC_Rotation           = "RotationImageEx"
const RASTER_PROC_INCTransform       = "INCTransformEx"
const RASTER_PROC_Pipeline           = "ProcessorPipeLineEx"
const RASTER_PROC_BandArithmetic     = "BandArithmeticEx"
const RASTER_PROC_ROI                = "applyROIEx"

'*** NetcadProjection.ProjectionType degerleri
const PR_NONE                        = 0  '        { Undefined }
const PR_GEO                         = 1  '        { Geographic }
const PR_UTM                         = 2  '        { Universal Transverse Mercator }
const PR_UTM3                        = 3  '        { Universal Transverse Mercator 3� Dilim}
const PR_TM                          = 4  '        { Transverse Mercator }
const PR_LAMCC                       = 5  '        { Lambert Conformal Conic }
const PR_LAMAZ                       = 6  '        { Lambert Azimuthal Equal Area }
const PR_ALBERS                      = 7  '        { Albers Conical Equal Area }
const PR_MERCAT                      = 8  '        { Mercator }
const PR_STEREO                      = 9  '        { Stereographic }
const PR_AZMEQD                      =10  '        { Azimuthal Equidistant }
const PR_GNOMON                      =11  '        { Gnomonic }
const PR_ORTHO                       =12  '        { Orthographic }
const PR_PS                          =13  '        { Polar Stereographic }
const PR_POLYC                       =14  '        { Polyconic }
const PR_SNSOID                      =15  '        { Sinusoidal }
const PR_EQRECT                      =16  '        { Equirectangular }
const PR_MILLER                      =17  '        { Miller Cylindrical }
const PR_VGRINT                      =18  '        { Van der Grinten }
const PR_ROBIN                       =19  '        { Robinson }

'*** NetcadProjection.Datum degerleri
const DATUM_KULTN = -1                'Kullan�c� Tan�ml�
const DATUM_WGS84 =  0                'WGS-84
const DATUM_GRS80 =  1                'GRS80 Y�netmelik ��in Datum
const DATUM_AUA   =  2                'AUSTRALIAN GEODETIC 1966
const DATUM_AUG   =  3                'AUSTRALIAN GEODETIC 1984
const DATUM_EUR_M =  4                'EUROPEAN 1950, Mean (3 Param)
const DATUM_EUR_A =  5                'EUROPEAN 1950, Western Europe
const DATUM_EUR_B =  6                'EUROPEAN 1950, Greece
const DATUM_EUR_C =  7                'EUROPEAN 1950, Norway & Finland
const DATUM_EUR_D =  8                'EUROPEAN 1950, Portugal & Spain
const DATUM_EUR_E =  9                'EUROPEAN 1950, Cyprus
const DATUM_EUR_F = 10                'EUROPEAN 1950, Egypt
const DATUM_EUR_G = 11                'EUROPEAN 1950, England, Channel
const DATUM_EUR_H = 12                'EUROPEAN 1950, Iran
const DATUM_EUR_I = 13                'EUROPEAN 1950, Sardinia(Italy)
const DATUM_EUR_J = 14                'EUROPEAN 1950, Sicily(Italy)
const DATUM_EUR_K = 15                'EUROPEAN 1950, England, Ireland
const DATUM_EUR_L = 16                'EUROPEAN 1950, Malta
const DATUM_EUR_S = 17                'EUROPEAN 1950, Iraq, Israel
const DATUM_EUR_T = 18                'EUROPEAN 1950, Tunisia
const DATUM_EUS   = 19                'EUROPEAN 1979
const DATUM_FAH   = 20                'OMAN
const DATUM_FLO   = 21                'OBSERVATORIO MET. 1939, Flores
const DATUM_FOT   = 22                'FORT THOMAS 1955, Leeward Is.
const DATUM_GAA   = 23                'GAN 1970, Rep. of Maldives
const DATUM_GEO   = 24                'GEODETIC DATUM 1949, NZ
const DATUM_GIZ   = 25                'DOS 1968, Gizo Island
const DATUM_GRA   = 26                'GRACIOSA BASE SW 1948, Azores
const DATUM_GUA   = 27                'GUAM 1963
const DATUM_GSE   = 28                'GUNUNG SEGARA, Indonesia
const DATUM_HEN   = 29                'HERAT NORTH, Afghanistan
const DATUM_HER   = 30                'HERMANNSKOGEL, old Yugoslavia
const DATUM_HIT   = 31                'PROVISIONAL SOUTH CHILEAN 1963
const DATUM_HJO   = 32                'HJORSEY 1955, Iceland
const DATUM_HKD   = 33                'HONG KONG 1963
const DATUM_HTN   = 34                'HU-TZU-SHAN, Taiwan
const DATUM_IBE   = 35                'BELLEVUE (IGN), Efate Is.
const DATUM_IDN   = 36                'INDONESIAN 1974
const DATUM_IND_B = 37                'INDIAN, Bangladesh
const DATUM_IND_I = 38                'INDIAN, India & Nepal
const DATUM_IND_P = 39                'INDIAN, Pakistan
const DATUM_INF_A = 40                'INDIAN 1954, Thailand
const DATUM_ING_A = 41                'INDIAN 1960, Vietnam 16N
const DATUM_ING_B = 42                'INDIAN 1960, Con Son Island
const DATUM_INH_A = 43                'INDIAN 1975, Thailand
const DATUM_INH_A1= 44                'INDIAN 1975, Thailand
const DATUM_IRL   = 45                'IRELAND 1965
const DATUM_ISG   = 46                'ISTS 061 ASTRO 1968, S Georgia
const DATUM_IST   = 47                'ISTS 073 ASTRO 1969, Diego Garc
const DATUM_JOH   = 48                'JOHNSTON ISLAND 1961
const DATUM_KAN   = 49                'KANDAWALA, Sri Lanka
const DATUM_KEG   = 50                'KERGUELEN ISLAND 1949
const DATUM_KEA   = 51                'KERTAU 1948, W Malaysia & Sing.
const DATUM_KUS   = 52                'KUSAIE ASTRO 1951, Caroline Is.
const DATUM_LCF   = 53                'L.C. 5 ASTRO 1961, Cayman Brac
const DATUM_LEH   = 54                'LEIGON, Ghana
const DATUM_LIB   = 55                'LIBERIA 1964
const DATUM_LUZ_A = 56                'LUZON, Phillipines
const DATUM_LUZ_B = 57                'LUZON, Mindanao Island
const DATUM_MAS   = 58                'MASSAWA, Ethiopia
const DATUM_MER   = 59                'MERCHICH, Morocco
const DATUM_MID   = 60                'MIDWAY ASTRO 1961, Midway Is.
const DATUM_MIK   = 61                'MAHE 1971, Mahe Is.
const DATUM_MIN_A = 62                'MINNA, Cameroon
const DATUM_MIN_B = 63                'MINNA, Nigeria
const DATUM_MOD   = 64                'ROME 1940, Sardinia
const DATUM_MPO   = 65                'M"PORALOKO, Gabon
const DATUM_MVS   = 66                'VITI LEVU 1916, Viti Levu Is.
const DATUM_NAH_A = 67                'NAHRWAN, Masirah Island (Oman)
const DATUM_NAH_B = 68                'NAHRWAN, United Arab Emirates
const DATUM_NAH_C = 69                'NAHRWAN, Saudi Arabia
const DATUM_NAP   = 70                'NAPARIMA, Trinidad & Tobago
const DATUM_NAR_A = 71                'NORTH AMERICAN 1983, Alaska
const DATUM_NAR_B = 72                'NORTH AMERICAN 1983, Canada
const DATUM_NAR_C = 73                'NORTH AMERICAN 1983, CONUS
const DATUM_NAR_D = 74                'NORTH AMERICAN 1983, Mexico
const DATUM_OGB_M = 75                'ORDNANCE GB 1936, Mean (3 Para)
const DATUM_OGB_A = 76                'ORDNANCE GB 1936, England
const DATUM_OGB_B = 77                'ORDNANCE GB 1936, Eng., Wales
const DATUM_OGB_C = 78                'ORDNANCE GB 1936, Scotland
const DATUM_OGB_D = 79                'ORDNANCE GB 1936, Wales
const DATUM_WGS72 = 80                'WGS-72
const DATUM_EUR_7 = 81                'EUROPEAN 1950, Mean (7 Param)
const DATUM_OGB_7 = 82                'ORDNANCE GB 1936, Mean (7 Para)
const DATUM_TUR_1 = 83                'ED-50 TURKIYE (7 Param)


Function Min(a,b)
  Min = a
  if b < a then
    Min = b
  end if
End Function

Function Max(a,b)
  Max = a
  if b > a then
    Max = b
  end if
End Function

Function XMLStartTag(Tag)
  XMLStartTag = "<" + Tag +  ">"
End Function

Function XMLEndTag(Tag)
  XMLEndTag = "</" + Tag +  ">"
End Function

Function StrToXML( value )
  StrToXML = value
End Function

Function XMLSection(Tag, Value)
  XMLSection = XMLStartTag(Tag) + StrToXML( value ) +  XMLEndTag(Tag)
End Function

Function GetNetcad_GML( GML, Projection )
dim WKT
   WKT = ""
   if not Projection is nothing then
     WKT = XMLStartTag("WKT") & Projection.GetAsWKT & XMLEndTag("WKT")
   end if
   GetNetcad_GML = XMLStartTag("NETCAD_GML") + GML + WKT + XMLEndTag("NETCAD_GML")
End Function

'Klas�r Se�im Penceresi A�ar.
function openDirDialog(title)
dim shell,folder,filePath,fileRoot
  Set shell = CreateObject("Shell.Application")
  Set folder = shell.BrowseForFolder(&H0,title,&H0008,&H0011)
  on error resume next
  openDirDialog = folder.ParentFolder.ParseName(folder.title).Path&"\"
  if err.number <> 0 then
    fileRoot = mid(folder.items.item(0).path,1,3)
    openDirDialog = fileRoot
  end if
  set shell  = nothing
  set folder = nothing
end function

' XML Servisleri ve GML �evrim �rne�i

Sub Main
dim INPUTXML, OUTXML, c, GC, OC, o
  with Netcad
    set c = .newc(0,0,0)
    while .SelectPoint("Kot �l�mek i�in nokta g�ster", c, -1)

      ' Bu serviste Input ve Output XML yapisi tamamen GML oldugu icin
      ' GML Converter kullanarak objeleri donustur
      set GC = .NewGMLConverter

      INPUTXML = GC.Coor2GML(c)    ' input XML Datayi hazirla

      ' Servisi Cagir
      OUTXML = NCXMLServiceMan.RunXML("NETCAD.GETZ", INPUTXML, "")

      set OC = GC.GML2Netcad( OUTXML )

      if not OC is nothing then
        if OC.NE > 0 then
          set o = .newobject
          OC.GetObject 0, o, nothing   ' Sonuc 1 tane nokta objesi gelmeli
          msgbox o.p1.z                ' Bulunan Kotu ekrana raporla
        end if
      end if
    wend
  end with
End sub


' Ama� : CONVERTPROJECTION XML servisi kullan�m� 
'        Projenin projeksiyonu cinsinden secilen iki nokta arasindaki mesafeyi
'        Farkl� bir projeksiyon cinsinden raporlamak
'        �rne�in: iki geographic coordinatin UTM cinsinden mesafesini metre olarak bulmak.

' Not  : Bu �rne�in �al��mas� i�in Projenin projeksiyonu secilmi� olmal� (�rne�in Co�rafi)
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

sub main
dim dlg
dim inPr,inDat,inZone,inProjParam,outPr,outDat,outZone
  with netcad
    set dlg = .newbdialog("PROJECTION > PROJECTION MESAFE")
    dlg.getCombo "inProj","Girdi Projeksiyonu",createProjList,0
    dlg.getcombo "inDatum","Girdi Datumu",createDatumList,0
    dlg.getString "inZone","Utm i�in Zone (derece)","",3
    dlg.getCheck "inProjParam","Girdi Olarak Projenin Proj. Kullan",0
    dlg.getCombo  "outProj","��kt� Projeksiyonu",createProjList,0
    dlg.getCombo  "outDatum","��kt� Datumu",createDatumList,0
    dlg.getString "outZone","Utm i�in Zone (derece)","",3
    if dlg.showmodal then
      inPr  = getProjection(dlg.valueByName("inProj"))
      inDat = getDatum(dlg.ValueByName("inDatum"))
      if isNumeric(dlg.valuebyname("inZone")) then
        inZone = dlg.valuebyname("inZone")
      else
        inZone = 30
      end if
      inProjParam = dlg.ValueByName("inProjParam")

      outPr  = getProjection(dlg.ValueByName("outProj"))
      outDat = getDatum(dlg.ValueByName("outDatum"))
      if isNumeric(dlg.valuebyname("outZone")) then
         outZone = dlg.valuebyname("outZone")
      else
         outZone = 30
      end if
      convertProjLength inPr,inDat,inZone,inProjParam,outPr,outDat,outZone
    end if
  end with
end sub

Sub convertProjLength(inPr,inDat,inZone,inProjParam,outPr,outDat,outZone)
dim INPUTXML, OUTXML, c1,c2, GC, OC, o, InProj, OutProj, GML, WKT
  with Netcad
    set c1 = .newc(0,0,0)
    set c2 = .newc(0,0,0)
    while .SelectPoint("Projeden Mesafe �l�mek i�in birinci noktay� g�ster", c1, -1) and _
          .SelectPoint("Projeden Mesafe �l�mek i�in ikinci  noktay� g�ster", c2, -1)

      ' NCDD.CONVERTPROJECTION XML Servisine parametre olarak verilecek INPUTXML hazirlaniyor...
      ' �nce noktalari bir listeye doldur
      set OC =  .NewCollection
      set o = .MakePoint(c1, "", "", 0)
      OC.AddObject o, -1, nothing
      set o = .MakePoint(c2, "", "", 0)
      OC.AddObject o, -1, nothing

      ' sonra noktalardan GML yap
      set GC = .NewGMLConverter
      GML = GC.Netcad2GML( OC )

      ' Aktif Projenin Projeksiyonunu �gren
      set InProj = .NewProjection
      if inProjParam = 1 then
        InProj.GetFromCurrentProject
      else
        inProj.ProjectionType = inPr
        inProj.Datum          = inDat
        inProj.Zone           = inZone
      end if
      ' Sonucta istenen Projeksiyonu Belirle
      set OutProj = .NewProjection
      OutProj.ProjectionType = outPr
      OutProj.Datum          = outDat
      OutProj.Zone           = outZone
      WKT = XMLStartTag("WKT") & OutProj.GetAsWKT & XMLEndTag("WKT")

      ' Servis XML yapisini tamamla
      INPUTXML =  XMLStartTag("CONVERTPROJECTION") & GetNETCAD_GML(GML,InProj) & WKT & XMLEndTag("CONVERTPROJECTION")

      ' Servisi Cagir
      OUTXML = NCXMLServiceMan.RunXML("NCDD.CONVERTPROJECTION", INPUTXML, "")

      ' Sonucta Bize gelen OUTXML yap�s� i�inde haz�r bulunan <totalDistance> elaman�na ula��p
      ' Noktalar arasindaki mesafeyi ��renebiliriz
      ' �stenirse �evrimi yap�lan noktalar�n koordinat bilgilerinede benzer �ekilde
      ' Ula�abilirsiniz.
      ReportDistance OUTXML
    wend
  end with
End sub

Sub ReportDistance(OUTXML)
dim XML, distXML, d
   set XML = CreateObject("Microsoft.XMLDOM")
   XML.async = false
   XML.loadXML OUTXML
   set distXML = XML.documentElement.selectSingleNode("/CONVERT_PROJECTION/NETCAD_GML/totalDistance")
   d = distXML.text
   msgbox "Mesafe = " & d & " m."
End sub

function createProjList
dim projList
  projList = "Co�rafi"&"|"&"UTM 3 Derecelik"&"|"&"UTM 6 Derecelik"
  createProjList = projList
end function

function getProjection(i)
  if i = 0 then
    getProjection = PR_GEO
  elseif i= 1 then
    getProjection = PR_UTM3
  elseif i = 2 then
    getProjection = PR_UTM
  end if
end function

function createDatumList
dim datumList
  datumList = "ED50"&"|"&"WGS84"&"|"&"GRS80"
  createDatumList = datumList
end function

function getDatum(i)
  if i = 0 then
    getDatum = DATUM_TUR_1
  elseif i= 1 then
    getDatum = DATUM_WGS84
  elseif i = 2 then
    getDatum = DATUM_GRS80
  end if
end function



' Amac : NETCAD.GETZ XML Servisi kullanim ve GML �evrim �rne�i
'        Bu servisin �al��mas� i�in projedeki bir Grid Referanstan yada
'        Netcad ��gen objeleri i�inde bir nokta se�mek gerekli.

' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

Sub Main
dim INPUTXML, OUTXML, c, GC, OC, o
  with Netcad
    set c = .newc(0,0,0)
    while .SelectPoint("Kot �l�mek i�in nokta g�ster", c, -1)

      ' Bu serviste Input ve Output XML yapisi tamamen GML oldugu icin
      ' GML Converter kullanarak objeleri donustur
      set GC = .NewGMLConverter

      INPUTXML = GC.Coor2GML(c)    ' input XML Datayi hazirla

      ' Servisi Cagir
      OUTXML = NCXMLServiceMan.RunXML("NETCAD.GETZ", INPUTXML, "")

      set OC = GC.GML2Netcad( OUTXML )

      if not OC is nothing then
        if OC.NE > 0 then
          set o = .newobject
          OC.GetObject 0, o, nothing   ' Sonuc 1 tane nokta objesi gelmeli
          msgbox o.p1.z                ' Bulunan Kotu ekrana raporla
        end if
      end if
    wend
  end with
End sub

' Macro Yazar� : Muhammed Ali AYDIN aydin992000@yahoo.com
' Tarih        : 05/11/2006
Sub Main
  with Netcad
  Dim BD,da,na,xx,yy,nd,nx,ny
  set BD = Netcad.NewBDialog("Macrox Writing By M.Ali Ayd�n")                 ' yeni dialog yarat
  BD.PUTPROMPT "Karatay Belediyesi Harita M�d�rl���"
  BD.PUTPROMPT "_____________________________________"
  BD.PUTPROMPT "Dikkat ! A�a��daki i�lemleri yapt�ktan sonra"
  BD.PUTPROMPT "   Macroyu �al��t�r�n"
  BD.PUTPROMPT "_____________________________________"
  BD.PUTPROMPT "Cks Dosyas�n�n i�eri�ini kopyalay�n "
  BD.PUTPROMPT "Paragraf yaz'a yap��t�r�n ve projeye ekleyin"
  BD.PUTPROMPT "_____________________________________"
  BD.GetFileName "item1","Dosya Adi Gir","c:\","NCN Dosyalari|*.NCN|Tum Dosyalar|*.*","bmp"  ' file filitresi ve default dosya uzantisi ver
  if BD.showmodal then
     da=BD.ValueByName("item1")
     da=da+".NCN"
     Const ForReading = 1, ForWriting = 2
     Dim fso, f,o
     Set fso = CreateObject("Scripting.FileSystemObject")
     Set f = fso.OpenTextFile(da, ForWriting, True)
      na=0:xx=0:yy=0
     dim x,p,sts,say
       for x=0 to .numobject-1
         set p=.getobject(x)
          if x=0 then
          end if
          if p.tag=otext and (p.s<>" " and instr(p.s,"�")=0 and instr(p.s,"�")=0 and instr(p.s,"�")=0 and instr(p.s,"")=0 and instr(p.s,"Nokta")=0 and p.s<>"Y " and p.s<>"X ") then
          if na=1 and yy=1 and xx=0 then
          nx=p.s
          xx=1
          end if
          if na=1 and yy=0 and xx<>1  then
          ny=p.s
          yy=1
          end if
          if na=0 then
          nd=p.s
          na=1
          end if
          if na=1 and xx=1 and yy=1 then
          na=0:xx=0:yy=0
          say=say+1
          sts=nd&chr(9)&ny&chr(9)&nx
          f.WriteLine sts
          end if
          end if
       next
     Set f = fso.OpenTextFile(da, ForReading)
      if say="" then
       sts="Projede Yaz� yok dolay�s�yla okuma yap�lamad� . "
      .message 0,"Macrox designed by Muhammed Ali AYDIN" ,sts
      else
      sts="Toplam :"&Say&" Nokta "&chr(10)&chr(13)&da&chr(10)&chr(13)&" Dosyas�yla Diske Sakland� "
      .message 0,"Macrox designed by Muhammed Ali AYDIN" ,sts
     end if
  end if
  end with
  end sub


' TARIH         : 24.01.2005
' AMA�          : Proje-Farkl� Kaydet-DXF ile yaz�lan dxf dosyalar�n�n
'                 Autocad ortam�nda a��lmas�nda ya�anan sorunlar� giderir.
'                 NCZ dosyay� sorunsuz olarak saklanmas� i�in d�zenler.
'
' GEREKL�L�KLER : NCMacro 4.0.015 ve �st� versiyon.
'
' HAZIRLAYAN    : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
sub main
  'obje Rengi2TabakaRengi
  objCol2LayerCol
  'Tabaka Rengi Kontrll�
  setLayerColor
  'Tabaka Ad� Kontrol�
  CorrectLayerNames
  'Blok Ad� Kontrol�
  checkBlockNames
  netcad.netcadcommand "PROJECT CLEAN 1,1,1,1,1,1,1"
  msgbox "Proje D�zenlendi..."
end sub

sub SetLayerColor
dim i,j
  with nclayermanager
    for i = 0 to .numlayer-1
      if .layer(i).color = 80 or _
         .layer(i).color = 15 or _
         .layer(i).color = 0 or _
         .layer(i).color = 16  then
         .layer(i).color =  4
     end if
    next
  end with
end sub

sub objCol2LayerCol
dim i,obj
  with netcad
    set obj = .getobject(i)
    if obj.renk <> 0 then
      obj.renk = 0
    end if
    set obj = nothing
  end with
end sub

sub CorrectLayerNames
dim n,i
  with nclayermanager
    for i = 0 to .numlayer-1
      n = ucase(.layer(i).name)
      n = replace(n,"�","U")
      n = replace(n,"�","g")
      n = replace(n,"�","I")
      n = replace(n,"�","I")
      n = replace(n,"�","s")
      n = replace(n,"�","c")
      n = replace(n,"�","o")
      n = replace(n,".","_")
      .layer(i).name = UCase(n)
    next
  end with
end sub

sub checkBlockNames
dim i,obj
  with netcad
    .setfilter nothing,array(),array(oblock)
    set obj = .newobject
    while .getnextobject2(obj)
     if obj.pname = "" then
       msgbox "�simsiz Blok Var. L�tfen �simsiz Olan Blok Objesine Bir �sim Veriniz..."
       .resetFilter
       exit sub
     end if
    wend
    .resetfilter
  end with
end sub


' Yazan : 
' Tarih : 17.02.2007
' A��klama : 

Sub Main
Dim i
  with Netcad
         set i=createobject("mapro.CMain")

         i.start(netcad)

         set i=nothing


  end with
End Sub

' Amac : Net3d yi baslatip, bir N3D dosyasi yuklemek
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

sub main

  Netcad3d.ShowNet3D
  Netcad3d.LoadFile "D:\ornekler\net3D.n3d"
end sub

' BG 07.01.2005 - De�i�kenler i�in Dialoglar Eklendi - Projedeki s�n�flar� bulup getiren combo
'                 Kal�nl�k i�in kal�nl�k sabiti ve referans kolon de�eri eklendi.
' KO 15.02.2005 - Duzeltmeler yapildi

dim simplify,relColName,thConst

Sub Main
dim sinif,fileName
dim dlg,FCList,FCArr

  Netcad3d.CloseNet3D 'calisirken bize engel olmasin, sonra acariz..

  with netcad
    FCList = findFClassList()
    FCArr = Split(FCList,"|")
    if ubound(FCArr) <> -1 then
     set dlg = .newbDialog("3D THEMATIC YAP")
     dlg.GetCombo "sinif", "S�n�f", FCList , 0
     dlg.GetFileName "fileName", "Dosya Ad�", "", "N3D Dosyalar�|*.N3D|Tum Dosyalar|*.*","N3D"
     if dlg.showmodal then
       Sinif = FCArr(dlg.valueByName("sinif"))

       if dlg.valueByName("fileName") = "" then
         msgbox "DOSYA SE�MED�N�Z !!!"
         exit sub
       else
         FileName = dlg.valueByName("fileName")
       end if

       if GetColName(sinif) = 0 then
         exit sub
       end if

       make3dThematic sinif,FileName,simplify,relColName,thConst
     end if
     set dlg = nothing
    else
       msgbox "Projede S�n�f Bulunamam��t�r. L�tfen A��klamay� Okuyunuz !!!"
    end if
    .NetcadCommand "REGEN"
  end with
end sub

Sub make3dThematic(sinif,FileName,BASITLESTIRME,relColName,thConst)
dim DS,a,dsw,N3DFILE, o,p,RefName
dim REF, Lejand, Layer
dim mesafe, YAKINMESAFE, UZAKMESAFE
dim po,filter,dlg
  with Netcad
   set DS = NCConman.FindDatasetByFC(False, Sinif)
   if DS is nothing then
     msgbox "Projede " & Sinif & " adl� bir s�n�f yok !"
     Exit Sub
   end if
   RefName = DS.AliasName & "." & DS.TableName & ".NCSPATIAL"
   set REF = NCRefman.FindReference("", RefName)
   set Lejand = REF.Lejand

   Lejand.SetSource DS    ' Lejanda Kaynak olarak bu Dataseti ver.

   ' Orginal Dataseti Sinifindan Bulur, yoksa uya
   'netcad.NetcadCommand("REGEN")
   set N3DFILE = Netcad3d.New3DFileCreator
   'on error resume next
   N3DFILE.CreateFile FileName
   N3DFILE.AddLayer "0", blue
   N3DFILE.WriteLayers
       set p = .GetWorld.GetAsPline
       set Filter = .NewDatasetFilter
       Filter.FilterType = NC_GISDS_Region
       filter.poly = p
       DS.SetFilter Filter   ' Filitreyi uygula

       if DS.Open then
        set dsw = DS.Limits(false)
         mesafe = NCMath.Distance(dsw.cll, dsw.cur,false)
         YAKINMESAFE   = mesafe / 100
         UZAKMESAFE    = mesafe*3
         DS.First
         while not DS.EOF
           DS.Geometry.Draw(magenta)
           set o = DS.Geometry.o
           set p = DS.Geometry.p
           p.Simplify 0, BASITLESTIRME                  ' Poligonu Indirge
           'msgbox relcolName
           if relcolName <> -1 then
             o.thicknes = DS.EvaluateMacro("$[#"&relColName&"]*"&thConst, 3)   ' Objeye Kalinlik ver
           else
             o.thicknes = thConst ' Relatif kolon yok ise varsay�lan kolonu ver
           end if
           set Layer = Lejand.GetLayer(o, p)            ' Bu objenin Tematikteki Tabaka bilgilerine ulas
           o.renk = layer.color                         ' Ve Objeye Tematik rengini ver 0-255
           N3DFILE.WriteObject o, p , magenta
           a = a + 1
           DS.Next
         wend
         DS.Close
       end if
       DS.ResetFilter    ' Filitreyi Kapat
   end with
   N3DFILE.CloseFile

   ' Netcad-3d yi baslat
   Netcad3d.ShowNet3D
   Netcad3d.LoadFile FileName
   Netcad3d.SetClipDistance YAKINMESAFE, UZAKMESAFE
   set Filter = nothing
   set po = nothing
   set DS = nothing
End Sub

function findFClassList()
dim i,j
  with ncconman
    findFClassList = ""
    for i = 0 to .NumConnection-1
       for j = 0 to .Connection(i).NumTables-1
         if .Connection(i).table(j,false).isspatial then
            if findFClassList = "" then
              findFClassList =  .Connection(i).table(j,false).FCName
            else
              findFClassList = findFClassList & "|" & .Connection(i).table(j,false).FCName
            end if
         end if
       next
    next
  end with
end function

function findKolonList(sinif)
dim ds,i
  findKolonList=""
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds.open then
    for i = 0 to ds.columnCount-1
      if  i = 0 then
        findKolonList = ds.columnByNo(i).name
      else
        findKolonList = findKolonList & "|" &  ds.columnByNo(i).name
      end if
    next
  end if
end function

function GetColName(sinif)
dim dlg, Kols, KolArr
  with Netcad
     GetColName = 0
     Kols = "Kolon Se�in|" & findKolonList(sinif)  ' en basa null ekledim , yok anlaminda
     KolArr = Split(Kols,"|")
     set dlg = .newbDialog("OBJE KALINLIKLARI")
     dlg.PutPrompt sinif & " S�n�f�"
     dlg.PutPrompt "Objelere Kal�nl�k vermek i�in Kolon Se�iniz ve Katsay�s� de�eri verin"
     dlg.GetCombo "relCol", "Obje Kal�nl�klar� Kolonu", Kols, 0
     dlg.GetFloat "thick", "Kal�nl�k Katsay�s� (m)", 10 , 2
     dlg.GetFloat "simplify", "Obje Basitle�tirme (m)", 10, 2
     if dlg.showmodal then
       simplify = dlg.valueByName("simplify")

       if dlg.valueByName("thick") = 0 then
         thConst = 10
       else
         thConst = dlg.valueByName("thick")
       end if

       relColName = KolArr(dlg.valueByName("relCol"))
       if dlg.valueByName("relCol")=0 then ' ilk kolon adi yok demek
          relColName = -1
       end if

       GetColName = 1
     end if
     set dlg = nothing
  end with
end function



' Yazan : Kamran �zcan
' Tarih : 07.04.2005
' A��klama : Yeni Blok tan�m� olu�turan �rnek

const MY_BLOCKNAME = "MYBLOK"

Sub Main
Dim i, Col, o
  with Netcad
    ' Yaratacagimiz Blok Tanimi zaten varmi diye bakmaliyiz.
    if not .IsBlockDefExists( MY_BLOCKNAME ) then
      ' Blogun tanimini olusturan objeleri ekleyelim.
      ' Cizgilerden bi ucgen yaratiyoruz
      set Col =  .NewCollection

      set o =  .MakeLine(.Newc(0,0,0), .Newc(20,20,0), 0, 0, 0)
      Col.AddObject o, -1, nothing

      set o =  .MakeLine(.Newc(20,20,0), .Newc(20,0,0), 0, 0, 0)
      Col.AddObject o, -1, nothing

      set o =  .MakeLine(.Newc(20,0,0), .Newc(0,0,0), 0, 0, 0)
      Col.AddObject o, -1, nothing

      ' *** Yeni Blok tanimini Netcade ekle. ***
      .CreateNewBlock MY_BLOCKNAME, Col

      set o = nothing
      set Col = nothing
    else
      msgbox MY_BLOCKNAME & " Adli Blok Zaten var !"
    end if

    ' Simdi artik bloktan degisik boylarda netcade bi kac tane ekleyebiliriz..
    set o = .MakeBlock(MY_BLOCKNAME, .Newc(100,200,0), 0,0, 1, 1, 0)
    .AddObject o
    set o = .MakeBlock(MY_BLOCKNAME, .Newc(200,200,0), 0,0, 2, 2, 0)
    .AddObject o
    set o = .MakeBlock(MY_BLOCKNAME, .Newc(300,200,0), 0,0, 3, 3, 0)
    .AddObject o
  end with
End Sub

' Ama� : Tabaka Y�neticisi Kullan�m �rne�i
' Yazan : Kamran �zcan
' Tarih : 21.09.2004
Sub Main

dim LayerNo, i

  with NCLayerManager
     ' Bir Ka� Yeni Tabaka Ekle
     .Add "TABAKA1", yellow
     .Add "TABAKA2", green
     .Add "TABAKA3", blue

     ' 1. Tabakayi Bul
     LayerNo = .Find("TABAKA1")

     ' Bunu Aktif tabaka Yap
     .CurrentLayer = LayerNo

     ' 2. Tabakanin rengini kirmizi yap
     .Layer( .Find("TABAKA2") ).Color = red

     ' 3. Tabakayi, objeleri ile birlikte Sil
     .Delete .Find("TABAKA3"), TRUE

     ' Projeyi Yeniden Ekrana �iz
     Netcad.NetcadCommand "REGEN"
  end with
End Sub

' Ama� : �stenen bir dosyay� A�mak,
'        �zerinde ��lemler yap�p yeni isimle Saklad�ktan sonra Kapatmak.
' Yazan : Kamran �zcan
' Tarih : 21.09.2004
sub main

dim OLDFN, NEWFN

  with Netcad
     OLDFN = "D:\sil\cap88.ncz"    ' Y�klenecek dosya
     NEWFN = "D:\sil\cap99.ncz"    ' Bu �simde saklanacak

    .LoadFile  OLDFN, fs_bnetcad          ' Dosyayi yukler

    .NetcadCommand "PROJECT CLEAN 1,0,1,0,0,1"
    '[Layers],[LineTypes],[Fonts],[Symbols],[Blocks],[Slient]
    'Bu Ornekte Sadece Kullanilmayan Tabakalar ve Fontlar Temizleniyor
    'Temizlenmesini istediklerinizi 1 yapmaniz yeterli
    'Ayrica Ekranda Mesaj gostermez (Slient=1)

    .SaveToFile NEWFN                     ' NCZ olarak saklar

    .NetcadCommand "CLOSE ACTIVEPROJECT"  ' ��imiz bitince Projeyi Kapat
  end with
end sub

' Ornek Raster Processing
' Yazan: Kamran Ozcan, 24.03.2005 
' Uc degisik turde Raster dosyalarina ulasmak mumkun. Asagidaki ornege bakarsaniz
' Su anda bunlardan sadece biri acik, digerleri remark edilmis durumda,
' Dosyadan, Refmandan veya Netcad den herhangi bir rastera ulasilabiliyor.
' Sabitler dosyasina NcBasic.nvb ve javaya, RASTER_PROC_xxx sabitleri eklendi.

' Not : input ve output dosya adlarini kontrol etmelisiniz !.


sub main
dim g, p, keyword,input,output
  with netcad

   input =  "D:\Data\proje\blokprj\689L.DRE"  ' Her turlu raster dosyada olabilir. DRE olmak zorunda degil !!
   output = "D:\sil\689.jpg"


   'set g = .newGeoRaster(input)                           ' 1- Dosyadan yukleme ornegi
   'set g =  NCRasterManager.GetRaster(0)                 ' 2- Netcad e Tam yuklu rasterlardan alma ornegi
   set g =  NCRefman.Category(0).Reference(0).GeoRaster  ' 3- Refman/Referanslardan alma ornegi

   set p = .newRasterProcessor(RASTER_PROC_ContrastAdjust)
   keyword = p.GetInputKeyword(0)

   if p.SetSource(keyword, g) then
     if p.Edit then
      if p.SaveToFile(output, IMAGE_JPG, true) then
         msgbox "Dosya cevrimi basarili"
       end if
     end if
   end if
  end with
end sub


' Ornek Raster Processing
' Yazan: Kamran Ozcan, 22.09.2005 

' - Bir Rasteri Netcade Yukler
' - Gorunurluk Alanini koruyan bir Raster Islemciden gecirip GEOTIFF olarak saklar
' - Rasteri Netcad den siler

sub main
dim g, p, keyword,input,output
  with netcad

    input  = "D:\Data\proje\raster\classified1.DRE"
    output = "D:\Data\proje\raster\MYTEST.TIF"

    NCRasterManager.AddRaster input
    .FindWorld  ' ekranda gormek icin

    set g =  NCRasterManager.GetRaster(0)  'Netcad e Tam yuklu rasterlardan alma ornegi
    set p = .newRasterProcessor(RASTER_PROC_ROI)
    keyword = p.GetInputKeyword(0)

    if p.SetSource(keyword, g) then
      if p.SaveToFile(output, IMAGE_TIF, true) then
         msgbox "Dosya cevrimi basarili"
       end if
    end if

    NCRasterManager.RemoveRaster input
    Netcad.FindWorld  ' ekranda gormek icin
  end with
end sub


'
' Dil   : Visual Basic
' Ama�  : Aktif Projedeki NCZ Objelerini PocketNetcad SVG Formatinda yazmak
' Yazan : �afak ��ra� / 17.12.2004


Const POWER_2_TO_8  = &H100
Const POWER_2_TO_16 = &H10000

Function BGR2RGB(color)
  Dim r,g,b
  r = color and &HFF
  g = (color/POWER_2_TO_8) and &HFF
  b = (color/POWER_2_TO_16) and &HFF
  
  BGR2RGB = POWER_2_TO_16*r + POWER_2_TO_8*g + b
End Function


Function replacePointDelimeter(xml)
  replacePointDelimeter = Replace(xml,",",".")
End Function

Class NetcadGeomHandler
  Dim f1 'file handle

  Function beginHandler(DosyaAdi)
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Set f1 = fso.CreateTextFile(DosyaAdi, true)
  End Function

  Sub endHandler()
    f1.Close()
  End Sub

  Sub addString(xml)
    f1.Write(replacePointDelimeter(xml))
  End Sub

  Function getColor(NetcadObject)
    if( NetcadObject.renk = 0 ) then
      getColor = 0
    else
      getColor = NCPalette.ColorIdxToRGB(NetcadObject.renk)
    end if

  End Function

  Sub pointHandler(NetcadObject,currentLayer)
    Dim color
    color = Hex(getColor(NetcadObject))
    addString("<rect x="&Chr(34)&NetcadObject.p1.x&Chr(34))
    addString(" y="&Chr(34)&NetcadObject.p1.y&Chr(34))
    addString(" width="&Chr(34)&"1"&Chr(34))
    addString(" height="&Chr(34)&"1"&Chr(34))
    addString(" stroke="&Chr(34)& "#"&color &Chr(34)&"/>"&Chr(13)&Chr(10))
  End Sub

  Sub lineHandler(NetcadObject,currentLayer)
    Dim color
    color = Hex(getColor(NetcadObject))

    addString("<line x1="&Chr(34)&NetcadObject.p1.x&Chr(34))
    addString(" y1="&Chr(34)&NetcadObject.p1.y&Chr(34))
    addString(" x2="&Chr(34)&NetcadObject.p2.x&Chr(34))
    addString(" y2="&Chr(34)&NetcadObject.p2.y&Chr(34))
    addString(" stroke="&Chr(34)& "#"&color  &Chr(34)&"/>"&Chr(13)&Chr(10))
  End Sub

  Sub circleHandler(NetcadObject,currentLayer)
    Dim color
    color = Hex(getColor(NetcadObject))

    addString("<circle cx="&Chr(34)&NetcadObject.p1.x&Chr(34))
    addString(" cy="&Chr(34)&NetcadObject.p1.y&Chr(34))
    addString(" r="&Chr(34)&NetcadObject.rad&Chr(34)) '
    addString(" fill="&Chr(34)&CStr(-1)&Chr(34)) '
    addString(" stroke="&Chr(34)&"#"&color &Chr(34)&"/>"&Chr(13)&Chr(10))
  End Sub

  Sub arcHandler(NetcadObject,currentLayer)
  End Sub

  Sub textHandler(NetcadObject,currentLayer)
    Dim color
    color = Hex(getColor(NetcadObject))

    addString "<text x="&Chr(34)&NetcadObject.p1.x&Chr(34)
    addString " y="&Chr(34)&NetcadObject.p1.y&Chr(34)
    addString " fill="&Chr(34)&"#"&color &Chr(34)
    addString " transform="&Chr(34)&"rotate=(30)"&Chr(34)&">"
    addString NetcadObject.s
    addString "</text>"&Chr(13)&Chr(10)
  End Sub

  Sub shapeHandler(NetcadObject,currentLayer)
  End Sub

  Sub plineHandler(NetcadObject,currentLayer)
    Dim p,i,j,lyr
    Dim color
    color = Hex(getColor(NetcadObject))
    Set p = Netcad.GetPlineExt(NetcadObject)

    if (NetcadObject.flags and POLYCLOSED) <> 0 then
      addString "<path d="& Chr(34) &"M"
      For j=0 to p.Num-1
        if j=0 then'�oklu do�runun referans noktas�
          addString p.Cor(j).x& " " & p.Cor(j).y & " "&Chr(13)&Chr(10)
        else'referans al�nan noktadan ba�lay�p
          'di�er noktalar� dola�arak line �iz.
          addString "L" & p.Cor(j).x & " " & p.Cor(j).y & " "&Chr(13)&Chr(10)
        end if
      Next
    else
      addString "<path d="& Chr(34) &"M"
      For j=0 to p.Num-1
       if j=0 then'�oklu do�runun referans noktas�
          addString p.Cor(j).x& " " & p.Cor(j).y & " "&Chr(13)&Chr(10)
        else'referans al�nan noktadan ba�lay�p
          'di�er noktalar� dola�arak line �iz.
          addString "L" & p.Cor(j).x & " " & p.Cor(j).y & " "&Chr(13)&Chr(10)
        end if
      Next
      addString Chr(34)
      addString " fill-opacity="&Chr(34)&"0.0"&Chr(34)
      addString " stroke="&Chr(34)&"#"&color &Chr(34)
      addString " stroke-width="&Chr(34)&"2"&Chr(34)&"/>"&Chr(13)&Chr(10)
    end if

     if (NetcadObject.flags and POLYCLOSED) <> 0 then'kapal� ise
       if (NetcadObject.flags and POLYFILLED) <> 0 then'kapal� ve i�i dolu ise
         addString " Z"&Chr(34)
         addString " fill-opacity="&Chr(34)&"1.0"&Chr(34)
         addString " fill="&Chr(34)&"#"&color &Chr(34)
         addString " filled="&Chr(34)&"1"&Chr(34)
         addString "/>"&Chr(13)&Chr(10)
       elseif (NetcadObject.flags and POLYFILLED) = 0 then'kapal� ve i�i bo� ise
         addString " Z"&Chr(34)
         addString " fill-opacity="&Chr(34)&"0.0"&Chr(34)'&" fill="&Chr(34)&"white"&Chr(34)
         addString " stroke="&Chr(34)&"#"&color &Chr(34)
         addString " stroke-width="&Chr(34)&"2"&Chr(34)&"/>"&Chr(13)&Chr(10)
       end if
     end if
  End Sub

  Sub izohdrHandler(NetcadObject,currentLayer)
  End Sub

  Sub rectHandler(NetcadObject,currentLayer)
    Dim color
    color = Hex(getColor(NetcadObject))
    addString("<rect x="&Chr(34)&NetcadObject.p1.x&Chr(34))
    addString(" y="&Chr(34)&NetcadObject.p1.y&Chr(34))
    addString(" width="&Chr(34)&(width)&Chr(34))
    addString(" height="&Chr(34)&(height)&Chr(34))
    addString(" stroke="&Chr(34)&"#"&color &Chr(34)&"/>"&Chr(13)&Chr(10))
  End Sub

  Sub stpaftaHandler(NetcadObject,currentLayer)
  End Sub

  Sub triangHandler(NetcadObject,currentLayer)
  End Sub

  Sub blockHandler(NetcadObject,currentLayer)
  End Sub
End Class

Function Netcad2SVGConverter(filename)
Dim o,p,i,j,world
Dim gHandler
Dim lyr,lyrColor

  With Netcad
    Set world = .GetWorld() 'T�m haritan�n limitini al.
    Set gHandler = New NetcadGeomHandler
    gHandler.beginHandler(filename)

    gHandler.addString "<svg cllX="&Chr(34)&CStr(world.cll.x) & Chr(34)
    gHandler.addString " cllY=" & Chr(34) & CStr(world.cll.y) & Chr(34)
    gHandler.addString " curX=" & Chr(34) & CStr(world.cur.x) & Chr(34)
    gHandler.addString " curY=" & Chr(34) & CStr(world.cur.y) & Chr(34)
    gHandler.addString " >"&Chr(13)&Chr(10)

    For i=0 to NCLayerManager.NumLayer()-1 step 1 't�m katmanlar� tara
      Set lyr = NCLayerManager.Layer(i)'indekse g�re katman� se�.
      'katmanlar� birbirinden ay�rmak i�in svg'nin g tag�n� kullan
      gHandler.addString "<g id="& Chr(34) & CStr(i) & Chr(34)&" name=" & Chr(34) & lyr.Name & Chr(34)
      lyrColor = lyr.ColorBGR
      lyrColor = Hex(lyrColor)
      gHandler.addString " color="& Chr(34)&"#"&lyrColor&Chr(34)

      if lyr.VisibiltyActive then
        gHandler.addString " visActive="& Chr(34)&"1"&Chr(34)
      else
        gHandler.addString " visActive="& Chr(34)&"0"&Chr(34)
      end if

      gHandler.addString " visScaleMin="& Chr(34)&CStr(lyr.VisStartScale)&Chr(34)
      gHandler.addString " visScaleMax="& Chr(34)&CStr(lyr.VisEndScale)&Chr(34)
      gHandler.addString " >"&Chr(13)&Chr(10)

      Set o = .NewObject()
       .SetFilter nothing,array(i),array() 'se�ili katmana ait geometrileri filtrele.

      while .GetNextObject2(o)
        if o.tag = odeleted then 'nesne silindi ise

        elseif o.tag = opoint  then 'nesne nokta ise
          gHandler.pointHandler o,lyr

        elseif o.tag = oline  then'nesne do�ru ise
          gHandler.lineHandler o,lyr

        elseif o.tag = ocircle then 'nesne daire ise
          gHandler.circleHandler o,lyr

        elseif o.tag = oarc then 'nesne yay ise
          gHandler.arcHandler o,lyr

        elseif o.tag = otext then 'nesne yaz� ise
          gHandler.textHandler o,lyr

        elseif o.tag = oshape then 'nesne �ekil ise
          gHandler.shapeHandler o,lyr

        elseif o.tag = opline then 'nesne �oklu do�ru ise
          gHandler.plineHandler o,lyr

        elseif o.tag = ospiral then 'nesne spiral ise
          '
        elseif o.tag = oizohdr then'nesne e�ri ise
          gHandler.izohdrHandler o,lyr

        elseif o.tag = orectangle then'nesne kutu ise
          gHandler.rectHandler o,lyr

        elseif o.tag = ostpafta then 'nesne stpafta ise
          gHandler.stpaftaHandler o,lyr

        elseif  o.tag = otriang then'nesne ��gen ise
          gHandler.triangHandler o,lyr

        elseif  o.tag = oblock then'nesne blok ise
          gHandler.blockHandler o,lyr

        else
          .Message 0,"Hata","Geom Handler"

        end if
      Wend 'while bitti ayn� katmandaki nesneleri tar�yor.
      Set o = Nothing
      .ResetFilter

      gHandler.addString "</g>"&Chr(13)&Chr(10)

    Next 'for bitti katmanlar� tar�yor.

    gHandler.addString "</svg>"&Chr(13)&Chr(10)
    gHandler.endHandler 'dosyay� kapat
  End With
End Function


Function Main
Dim BD
  set BD = Netcad.NewBDialog("NCZ > SVG")                 ' yeni dialog yarat
  BD.GetFileName "dosyaAdi","Dosya Ad� G�r","","SVG Dosyalar�|*.svg|Tum Dosyalar|*.*","svg"  ' file filitresi ve default dosya uzantisi ver
  if BD.showmodal then
    Netcad2SVGConverter(BD.ValueByName("dosyaAdi"))
  end if
  set BD = Nothing
End Function



' Yazan : 
' Tarih : 11.06.2012
' A��klama : 

Sub Main
Dim Top
Dim obj
dim bd
dim bz

  with Netcad
     set BD = Netcad.NewBDialog("eski yaz�")

    BD.Getstring "item","YAZI","",50

    if BD.showmodal then

        set Bz = Netcad.NewBDialog("yeni yaz�")

    Bz.Getstring "item","YAZI","",50

    if Bz.showmodal then

    .setfilter nothing, array(),array(opoint)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
      obj.pname=replace(obj.pname,BD.ValueByName("item"),Bz.ValueByName("item"))
           end if
            .PUTOBJECT .CUROBJPOS,OBJ
        loop
    .resetfilter

    end if
    end if
  end with
End Sub

sub main
	dim x 
	
	on error resume next
	
	set x = createobject("NoktaKaydir.clsMain")

	call x.Start(NetCad)

end  sub
Sub Main
Dim i,BD,obj,POLY,j,lineOBJ,say
with Netcad
     set BD = .NewBDialog("Alana Nokta Ta��ma Sihirbaz�:::")
     BD.GetCombo "LAYER_ALAN", "ALAN Tabakas�n� Se�in : ", "0", 0
     for i = 1 to .numlayers - 1
         BD.AddCombo .LayerNameOf(i)
     next
     BD.GetCombo "LAYER_NOKTA", "NOKTA Tabakas�n� Se�in : ", "0", 0
     for i = 1 to .numlayers - 1
         BD.AddCombo .LayerNameOf(i)
     next
     BD.PutPrompt "_____________________________________________________________"
     BD.GetFloat "BUFFER", "Tampon Mesafesi (m): ", 0.025, 3
     BD.PutPrompt "_____________________________________________________________"
     BD.GetCheck "LIMIT_BUL", "��lem G�ren Parselin Limitini Bul...", -1
     BD.GetCheck "LIMIT", "Proje Limitini Bul...", -1
     BD.PutPrompt "_____________________________________________________________"
     BD.PutPrompt "ULUSAL CAD ve GIS ��Z�MLER� A.�."
     if BD.ShowModal then
        set obj = .newobject()
        .SetFilter nothing, array(BD.ValueByName("LAYER_ALAN")), array(opline)
        while .GetNextObject2(obj)
             say = say + 1
             set POLY = .newpoly()
             set POLY = obj.GetObjectAsPline()
             if BD.ValueByName("LIMIT_BUL") = 1 then
                .SetCurrentWindow obj.limits, true
                .DrawObject .MakePline("",3,0,0,0,0,POLY), 222
             end if
             '************************************
             for j = 0 to POLY.Num - 2
                 set lineOBJ = .newobject()
                 set lineOBJ = .MakeLine(POLY.Cor(j),POLY.Cor(j+1),0,0,3)
                 pointMove lineOBJ,BD.ValueByName("LAYER_NOKTA"),BD.ValueByName("BUFFER")
             next
             '************************************
             set POLY = nothing
             .BackMessage : .setMessage "[ " & say & ". ] Alan Objesi Okunuyor..."
        wend
        .ResetFilter
        set obj = nothing
        .Message 0, say & " Adet Alan Objesi Se�ildi...", "Noktalar En Yak�n K��eye Ta��nd�..."
     end if
     .backMessage
     if BD.ValueByName("LIMIT") = 1 then .findworld
end with
End Sub

Function pointMove(lineOBJ,pointLAYER,buffer)
dim i,ext,obj,POLY,lenght1,lenght2
with netcad
     set ext = .NewWorld(0,0,0,0)
     set ext = lineOBJ.Limits
     ext.Expand buffer, buffer
     set obj = .newobject()
     .SetFilter ext, array(pointLAYER), array(opoint)
     while .GetNextObject2(obj)
           if NCMath.OnlineSeg(obj.p1,lineOBJ.p1,lineOBJ.p2,buffer) then
              lenght1 = NCMath.Distance(obj.p1,lineOBJ.p1,false)
              lenght2 = NCMath.Distance(obj.p1,lineOBJ.p2,false)
              if lenght1 < lenght2 then
                 obj.p1.x = lineOBJ.p1.x
                 obj.p1.y = lineOBJ.p1.y
                 obj.p1.z = lineOBJ.p1.z
                 .PutObject .CurObjPos, obj
              else
                 obj.p1.x = lineOBJ.p2.x
                 obj.p1.y = lineOBJ.p2.y
                 obj.p1.z = lineOBJ.p2.z
                 .PutObject .CurObjPos, obj
              end if
           end if
     wend
     .resetfilter
     set obj = nothing
end with
end function

'Makro Yazar�:      Harita M�hendisi O�uzalp BOZKURT
'Ama�:              Belirlenen alana d��en kurum dosya numaralar�n�n �telenmesi
'Tarih:             Temmuz-2012
'Not:               Kurum dosya numaralar� KDN tabakas�nda olmal�

Sub Main

Dim i
dim obj
dim regpoly
dim bd

with Netcad

set BD = Netcad.NewBDialog("�teleme miktar�n� giriniz?")

    BD.Getinteger "item","�teleme",0

   if BD.showmodal then

      .SetFilter nothing, ARRAY(.FOUNDLAYER("Nokta")), ARRAY(opoint)

            DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE


                 obj.pname=obj.pname+BD.ValueByName("item")
                 .PUTOBJECT .CUROBJPOS,OBJ

                   END IF

           LOOP


  end if

.netcadcommand("REGEN")

end with

End Sub




' Numarataj Verisi Haz�rlama Makrosu
' Sokak ba�lang�� numaras� ve Art�r�m de�eri verildikten sonra
' binalarda bulunan numarataj bloklar�na numarataj de�erleri �retir.
' Numarataj paftalar�n�n �retilmesi i�in kullan�labilir.
' ��lem �ncesinde binalara ait numarataj bloklar� yerle�tirilmelidir. �nce
' bu bloklara ait a�� de�erini al�r ve yaz� objelerini bu de�eri kullanarak �retir.

' Yenilikler
'----------------------------------
' v.0.4. Versiyonundan sonra yaz� y�nleri, blok a��lar� aksi olsa dahi kuzeye g�re
' okunabilir olma �zelli�i eklendi





SUB MAIN
dim blokaci,yaziaci,yazi,secim,deger,baslangic,atlama,blokx,bloky,koord,ytext
with netcad

Dim BD
  set BD = Netcad.NewBDialog("Numarataj Diyalog.. V.0.4...!! H.N.")
  BD.GetInteger "item1","S/C Ba�lang�� No :", 1
  BD.GetInteger   "item2","Atlama De�eri :", 2

  if BD.showmodal then
     baslangic= BD.ValueByName("item1")
     atlama = BD.ValueByName("item2")
  end if
set BD = Nothing
set secim=.newselectstatus

yazi=baslangic

while .selectobjectinstant("Blo�u Se�in...",1,array(oblock),secim)
   set deger=secim.objects(0)
   blokx=deger.p1.x
   bloky=deger.p1.y
   set koord=.newc(bloky,blokx,0)
   blokaci=deger.angle
   if blokaci>6.2831853 then blokaci=blokaci-6.2831853

if yazi=baslangic then
   if blokaci<3.14159265 then
     if blokaci<1.57079633 then
     set ytext = .MakeText(koord," "& yazi, 0,0, 1,blokaci,5,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     else
     blokaci=blokaci+3.14159265
     set ytext = .MakeText(koord," "& yazi, 0,0, 1,blokaci,1,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
   else
     if blokaci>3.14159265 and blokaci<4.71238898 then
     blokaci=blokaci-3.14159265
     set ytext = .MakeText(koord," "&yazi, 0,0, 1,blokaci,1,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
     if blokaci>4.71238898 then
     set ytext = .MakeText(koord," "&yazi, 0,0, 1,blokaci,5,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
     end if
ELSE
    if blokaci<3.14159265 then
    if blokaci<1.57079633 then
     set ytext = .MakeText(koord," "& yazi, 0,0, 1,blokaci,5,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     else
     blokaci=blokaci+3.14159265
     set ytext = .MakeText(koord," "& yazi, 0,0, 1,blokaci,1,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
   else
     if blokaci>3.14159265 and blokaci<4.71238898 then
     blokaci=blokaci-3.14159265
     set ytext = .MakeText(koord," "&yazi, 0,0, 1,blokaci,1,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
     if blokaci>4.71238898 then
     set ytext = .MakeText(koord," "&yazi, 0,0, 1,blokaci,5,.CreateLayer("NUMARATAJ_NO", RED))
     .addobject(ytext)
     yazi=yazi+atlama
     end if
     end if

END IF
wend
SET SECIM=NOTHING
SET YAZI=NOTHING
end with

END SUB

'
' Dil  : Visual Basic
' Ama� : Filitre yontemi ile aktif projedeki istenen objeleri
'        taramak ve objelerin turlerini mesajla gostermek
'
Sub Main
Dim o
  with Netcad
    set o = .NewStrings
    o.Clear
    o.AddItem "Helo"
    msgbox o.count
    msgbox o.getitem(0)
    msgbox o.Test.Area(5)
    set o = nothing              ' obje icin alinan memoryi geri ver
  end with
end sub

Dim g_sIlID,g_sIlceID,g_sKoy,g_sBaseDir,g_sSablonPath
Dim g_conn,sConn

Sub ayarYukle
Dim sNetcadDir,sConfigPath
Dim FS,sTmp
Dim dlg,os

  g_sBaseDir = ""
  sNetcadDir = Netcad.GetParam(PNC_NETCADDIR)

  Set FS = CreateObject("Scripting.FileSystemObject")
  sTmp = FS.OpenTextFile(sNetcadDir&"TOOLS\OGM\configuration.txt").ReadAll
  Set FS = Nothing

  g_sBaseDir =Mid(sTmp,InStr(sTmp,"=")+1,InStr(sTmp,Chr(13))-InStr(sTmp,"=")-1)
  sTmp= Mid(sTmp,InStr(sTmp,Chr(13)&Chr(10))+2)
  g_sSablonPath = Mid(sTmp,InStr(sTmp,"=")+1,InStr(sTmp,Chr(13)&Chr(10))-InStr(sTmp,"=")-1)
End Sub





Function ilTablo
  Dim rs,dlg,ilListe,counter,sSehir,sBaseDir,sSablonPath,dlgError,FS,os,ayarlar

 'mSGbOX Netcad.GetParam(PNC_NETCADDIR)
  Set g_conn = CreateObject("ADODB.Connection")
  sConn = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" & Netcad.GetParam(PNC_NETCADDIR)&"TOOLS\OGM\IL_ILCE_KOY_SABLON.MDB" & ";"
  g_conn.Open(sConn)


  sSehir = ""
  Set rs = g_Conn.Execute("SELECT OBJECTID,SEHIR_ADI FROM IL ORDER BY SEHIR_ADI")
  if rs.eof then 
    'MsgBox "OLMADI"
  else
    Set dlg = Netcad.NewBDialog(".:.")
    dlg.GetCombo "cbxIL", "�ller", "----------", 0
    while not rs.eof
      dlg.AddCombo rs("SEHIR_ADI").Value
      rs.moveNext
    wend
    if dlg.ShowModal then
      'sBaseDir = dlg.ValueByName("txtBaseDir")
      'if sBaseDir=""then
      '  Set dlgError = Netcad.NewBDialog("Hata")
      '  dlgError.PutPrompt "Ana Dizini bo� b�rakmay�n�z"
      '  if dlgError.showModal then
      '    ilTablo
      '  end if
      '  Exit Function
      'end if
      'g_sBaseDir = sBaseDir

      'sSablonPath = dlg.ValueByName("txtSablonPath")
      'if sSablonPath=""then
      '  Set dlgError = Netcad.NewBDialog("Hata")
      '  dlgError.PutPrompt "�ablon b�l�m�n� bo� b�rakmay�n�z"
      '  if dlgError.showModal then
      '    'g_conn.Close
      '    ilTablo
      '  end if
      '  Exit Function
      'end if
      'g_sSablonPath = sSablonPath



      ilTablo = dlg.ValueByName("cbxIL")

      if CInt(ilTablo) > 0 then
        rs.moveFirst
        counter = 0
        while not rs.eof
          counter = counter +1
          if counter = CInt(ilTablo) then
            sSehir = rs("SEHIR_ADI").Value
            g_sIlID = rs("OBJECTID").Value
          end if
          rs.moveNext

        wend

        Set FS = CreateObject("Scripting.FileSystemObject")
        set os = FS.CreateTextFile(Netcad.GetParam(PNC_NETCADDIR)&"TOOLS\OGM\configuration.txt", True)
        os.write "ANA_DIZIN="&g_sBaseDir&Chr(13)&Chr(10)&"SABLON_NCZ="&g_sSablonPath&Chr(13)&Chr(10)
        Set FS = Nothing
        'g_conn.Close
        ilTablo = sSehir
      else
        Set dlgError = Netcad.NewBDialog("Hata")
        dlgError.PutPrompt "Bir il se�iniz"
        if dlgError.showModal then
          'g_conn.Close
          ilTablo
          Exit Function
        end if
        ilTablo = ""
        Exit Function
      end if
    end if
  end if
End Function

Function ilceTablo(sSecilenIL)
  Dim rs,dlg,ilListe,counter,sIlce,dlgError
  sIlce = ""
  'g_conn.Open sConn
  Set rs = g_Conn.Execute("SELECT OBJECTID,ILCE_ADI FROM ILCE WHERE IL_OBJECTID="&g_sIlID&" ORDER BY ILCE_ADI")
  if rs.eof then 
    MsgBox "�l�e bulunamad�"
  else
    Set dlg = Netcad.NewBDialog(".:.")
    dlg.PutPrompt "   IL : "&sSecilenIL
    dlg.GetCombo "cbxILCE", "�l�eler", "----------", 0
    while not rs.eof
      dlg.AddCombo rs("ILCE_ADI").Value
      rs.moveNext
    wend
    if dlg.ShowModal then
      ilceTablo = dlg.ValueByName("cbxILCE")
      if CInt(ilceTablo) > 0 then
        rs.moveFirst
        counter = 0
        while not rs.eof
          counter = counter +1
          if counter = CInt(ilceTablo) then
            sIlce = rs("ILCE_ADI").Value
            g_sIlceID = rs("OBJECTID").Value
          end if
         rs.moveNext
        wend
        'g_conn.Close
        ilceTablo = sIlce
      else
        Set dlgError = Netcad.NewBDialog("Hata")
        dlgError.PutPrompt "Bir il�e se�iniz"
        if dlgError.showModal then
          'g_conn.Close
          ilceTablo = ilceTablo(sSecilenIL)
          Exit Function
        end if
        ilceTablo = ""
        Exit Function
      end if
    else
      ilceTablo = ""
      Exit Function
    end if
  end if
End Function

Function koyTablo(sSecilenIL,sSecilenILCE)
  Dim rs,dlg,ilListe,counter,sKoy,rsQuery,dlgError,maxID
  sKoy = ""
  g_sIlceID = CStr(g_sIlceID)
  if Len(g_sIlceID)<2 then
    g_sIlceID = "0"&g_sIlceID
  end if
  'g_conn.Open sConn
  Set rs = g_Conn.Execute("SELECT OBJECTID,AD FROM KOY_MERKEZ WHERE ILCE_OBJECTID="&g_sIlceID&" ORDER BY AD")
  if rs.eof then 
    MsgBox "K�y Bulunamad�"
  else
    Set dlg = Netcad.NewBDialog(".:.")
    dlg.PutPrompt "   IL   : "&sSecilenIL
    dlg.PutPrompt "   ILCE : "&sSecilenILCE
    dlg.GetString "txtYeniKoy", "Yeni K�y �smi", "", 200
    dlg.GetCombo "cbxKoy", "K�y �simleri", "----------", 0
    while not rs.eof
      dlg.AddCombo rs("AD").Value
      rs.moveNext
    wend
    if dlg.ShowModal then
      sKoy = dlg.ValueByName("txtYeniKoy")
      sKoy = Trim(sKoy)
      if sKoy<>"" then
        'g_conn.Close
        'g_conn.Open sConn
        Set rsQuery = g_Conn.Execute("SELECT MAX(OBJECTID) FROM KOY_MERKEZ")
        maxID = rsQuery(0).Value
        maxID = maxID+1

        g_Conn.Execute "INSERT INTO KOY_MERKEZ(OBJECTID,ILCE_OBJECTID,AD) VALUES("&maxID&","&g_sIlceID&",'"&sKoy&"')"
      else
        koyTablo = dlg.ValueByName("cbxKoy")
        if CInt(koyTablo) > 0 then
          rs.moveFirst
          counter = 0
          while not rs.eof
            counter = counter +1
            if counter = CInt(koyTablo) then
              sKoy = rs("AD").Value
            end if
            rs.moveNext
          wend
        else
          Set dlgError = Netcad.NewBDialog("Hata")
          dlgError.PutPrompt "Bir k�y se�iniz"
          if dlgError.showModal then
            'g_conn.Close
            koyTablo = koyTablo(sSecilenIL,sSecilenILCE)
            Exit Function
          end if
          koyTablo = ""
          Exit Function
        end if
      end if
      koyTablo = sKoy
    end if
  end if
End Function

Function dizinOlustur(il,ilce,koy)
  Dim fso, f ,posDelim,tmpStr,tmpDir,tmpLast,bFolderExist
  Set fso = CreateObject("Scripting.FileSystemObject")

  bFolderExist = true
  If not fso.FolderExists(g_sBaseDir) Then
    tmpStr = g_sBaseDir
    posDelim = InStr(tmpStr,"\")
    tmpLast = ""
    while posDelim>0
      tmpDir = Mid(tmpStr,1,posDelim)
      tmpStr = Mid(tmpStr,posDelim+1)

      tmpLast = tmpLast & tmpDir
      tmpLast = Mid(tmpLast,1)

      If not fso.FolderExists(tmpLast) Then
        Set f = fso.CreateFolder(tmpLast)
        bFolderExist = false
      End If
      posDelim = InStr(tmpStr,"\")
    wend

    tmpDir = tmpStr
    tmpLast = tmpLast & tmpDir
    tmpLast = Mid(tmpLast,1)
     If not fso.FolderExists(tmpLast) Then
        Set f = fso.CreateFolder(tmpLast)
        bFolderExist = false
    End If
  End If

  If not fso.FolderExists(g_sBaseDir&"\"&il) Then
    Set f = fso.CreateFolder(g_sBaseDir&"\"&il)
    bFolderExist = false
  End If

  If not fso.FolderExists(g_sBaseDir&"\"&il&"\"&ilce) Then
    Set f = fso.CreateFolder(g_sBaseDir&"\"&il&"\"&ilce)
    bFolderExist = false
  End If

  If not fso.FolderExists(g_sBaseDir&"\"&il&"\"&ilce&"\"&koy) Then
    Set f = fso.CreateFolder(g_sBaseDir&"\"&il&"\"&ilce&"\"&koy)
    bFolderExist = false
  End If

  if not bFolderExist then
    dizinOlustur = f.Path
  else
    dizinOlustur = g_sBaseDir&"\"&il&"\"&ilce&"\"&koy
  end if
End function

Sub Main
Dim secilenIL,secilenILCE,secilenKOY
Dim OLDFN, NEWFN,sSablonDBPath
Dim sPath

Dim fso
Dim ayarlar,dlg,olcek,ncProj,bProjSet,iDlgRes
  ayarYukle

  if g_sBaseDir="" or g_sSablonPath="" then
    Set dlg = Netcad.NewBDialog("Uyar�")
    dlg.PutPrompt "Ana Dizin veya �ablon belirtilmemi�."
    dlg.ShowModal
    Exit Sub
  end if

  secilenIL = ilTablo

  if secilenIL<>"" then
    secilenILCE = ilceTablo(secilenIL)
    if secilenILCE<>"" then
      secilenKOY = koyTablo(secilenIL,secilenILCE)
      if secilenKOY<>""then
        sPath = dizinOlustur(secilenIL,secilenILCE,secilenKOY)
        with Netcad
          OLDFN = g_sSablonPath
          if OLDFN="" then
            Exit Sub
          end if
          Set fso = CreateObject("Scripting.FileSystemObject")
          if fso.FileExists(OLDFN) then
            sSablonDBPath = Mid(OLDFN,1,InstrRev(OLDFN,".NCZ"))&"MDB"
            NEWFN = sPath&"\"&secilenKOY&".NCZ"
            fso.CopyFile OLDFN,NEWFN
            fso.CopyFile sSablonDBPath,sPath&"\"&secilenKOY&".MDB"
            .LoadFile  NEWFN, fs_bnetcad          ' Dosyayi yukler
            .NetcadCommand("EXECUTE_NETCAD_COMMAND 21001")

           olcek = .GetParam(PNC_VPORTSCALE)
           Set ncProj = .NewProjection
           if ncProj.GetFromCurrentProject then
             if ncProj.ProjectionType=PR_NONE then
               bProjSet = false
             else
               bProjSet = true
             end if
           end if

           while not bProjSet
             Set dlg = Netcad.NewBDialog("Uyar�")
             dlg.PutPrompt "Projeksiyon atanmad�.Tekrar deneyiniz."
             if dlg.ShowModal then
               .SetParam PNC_VPORTSCALE,5000
               iDlgRes = .NetcadCommand("EXECUTE_NETCAD_COMMAND 21001")
               if iDlgRes >= 0 then
                 if ncProj.GetFromCurrentProject then
                   if ncProj.ProjectionType=PR_NONE then
                     bProjSet = false
                   else
                     bProjSet = true
                   end if
                 end if
               else
                 bProjSet = true
               end if'proje �zellikler
             else
               bProjSet = true
             end if'ShowModal
           wend
          end if
        end with
      end if
    end if
  end if




End Sub

Dim g_sIlID,g_sIlceID,g_sKoy,g_sBaseDir,g_sSablonPath

Function baglantiLookup
  Dim sNetcadDir,sConfigPath
  Dim FS,sTmp
  Dim dlg,os

  g_sBaseDir = ""
  sNetcadDir = Netcad.GetParam(PNC_NETCADDIR)

  Set FS = CreateObject("Scripting.FileSystemObject")
  sTmp = FS.OpenTextFile(sNetcadDir&"TOOLS\OGM\configuration.txt").ReadAll
  Set FS = Nothing

  g_sBaseDir =Mid(sTmp,InStr(sTmp,"=")+1,InStr(sTmp,Chr(13))-InStr(sTmp,"=")-1)
  sTmp= Mid(sTmp,InStr(sTmp,Chr(13)&Chr(10))+2)
  g_sSablonPath = Mid(sTmp,InStr(sTmp,"=")+1,InStr(sTmp,Chr(13)&Chr(10))-InStr(sTmp,"=")-1)


  Set dlg = Netcad.NewBDialog(".:.")
  dlg.GetString "txtBaseDir", "Ana Dizin", g_sBaseDir, 255
  dlg.GetFileName "txtSablonPath", "�ablon", g_sSablonPath , "*.NCZ", ""

  if dlg.ShowModal then
    g_sSablonPath = dlg.ValueByName("txtSablonPath")
    g_sBaseDir = dlg.ValueByName("txtBaseDir")
    'if g_sBaseDir="" and g_sSablonPath="" then
    '  baglantiLookup
    '  Exit Function
    'end if

    Set FS = CreateObject("Scripting.FileSystemObject")
    set os = FS.CreateTextFile(Netcad.GetParam(PNC_NETCADDIR)&"TOOLS\OGM\configuration.txt", True)
    os.write "ANA_DIZIN="&g_sBaseDir&Chr(13)&Chr(10)&"SABLON_NCZ="&g_sSablonPath&Chr(13)&Chr(10)
    Set FS = Nothing
    baglantiLookup = true

  else
    if g_sSablonPath="" or g_sBaseDir="" then
      baglantiLookup = false
    end if
  end if


  baglantiLookup = true

  Set FS = Nothing
End Function

Sub Main
Dim ayarlar
  ayarlar = baglantiLookup
  while not ayarlar
    ayarlar = baglantiLookup
  wend
End Sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi

with netcad

set secim = .NewSelectStatus()
set c = .newc(0,0,0)

if.SelectObjectInstant( "KAYDIRILACAK YAZIYI SE�",1,array(otext),secim) then
set obj = secim.objects(0)
.DrawObject obj,red
yazi = obj.s


layerno=.foundlayer("TXT_KOORDINAT")

if .SelectPoint("Nokta se�", c, 2) then
   set yaz1=.maketext(c, "NOT: Bu boru hat�na ait daimi irtifak hakk� ile  ",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz2=.maketext(c, "daha sonra kurulan m�stakil ve daimi  ",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz3=.maketext(c, "�st hakk� (a) ile g�sterilen" & yazi&"m2 lik",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz4=.maketext(c, "k�sm�nda �rt��mektedir.(kesi�mektedir)",0,0,1.5,0,"1",layerno)
   .addobject(yaz1)
   .addobject(yaz2)
   .addobject(yaz3)
   .addobject(yaz4)
end if

if .SelectPoint("Nokta se�", c, 2) then
   set yaz1=.maketext(c, "(a) ile g�sterilen " &yazi&"m2 lik k�s�m .... ",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz2=.maketext(c, "sayfada tescilli boru hat�� daimi irtifak hakk� ",0,0,1.5,0,"1",layerno)
   c.x=c.x-2.5
   set yaz3=.maketext(c,"hakk� ile �rt��mektedir. ",0,0,1.5,0,"1",layerno)

   .addobject(yaz1)
   .addobject(yaz2)
   .addobject(yaz3)
end if

end if

end with
end sub


Sub Main
Dim i,j,o,p
   with Netcad



Dim BD
dim item


set BD = Netcad.NewBDialog("NE KADAR YAKINDAK� NOKTALARI YAKALASIN???")

    BD.Getfloat "item","UZAKLIK",0,1

    if BD.showmodal then


      for i = 0 to .numobject-1
        set o = .getobject(i)
          if o.tag = opline then
          .drawobject o,1
          set p = .getplineext(o)
             for j = 0 to p.num-1
                 NoktaAyir p.cor(j),BD
                 .putplineext o,p
                 .putobject i,o
             next
           end if
           next
           end if
msgbox "Parsel k�r�klar� noktalara �ekildi"
end with
End Sub
'-------------------------------------------------------------------------
sub noktaayir(c,BD)
dim obj1
with netcad
dim w: set w=.newworld(0,0,0,0):   w.BuildRegion c,BD.ValueByName("item"),BD.ValueByName("item")
.setfilter w,array(),array(opoint)
Do
set obj1=.getnextobject
if obj1 is nothing then
exit do
else
.drawobject obj1,110
c.x=obj1.p1.x
c.y=obj1.p1.y
c.z=obj1.p1.z
.putobject .curobjpos, obj1
end if
loop
.resetfilter
end with

end sub
    



' Yazan : 
' Tarih : 11.06.2012
' A��klama : 

Sub Main
Dim Top
Dim obj
dim bd
dim bz

  with Netcad
     set BD = Netcad.NewBDialog("eski yaz�")

    BD.Getstring "item","YAZI","",50

    if BD.showmodal then

        set Bz = Netcad.NewBDialog("yeni yaz�")

    Bz.Getstring "item","YAZI","",50

    if Bz.showmodal then

    .setfilter nothing, array(),array(opline)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
      obj.pname=replace(obj.pname,BD.ValueByName("item"),Bz.ValueByName("item"))
           end if
            .PUTOBJECT .CUROBJPOS,OBJ
        loop
    .resetfilter

    end if
    end if
  end with
End Sub


Sub Main
Dim i,j,o,p
   with Netcad



Dim BD
dim item


set BD = Netcad.NewBDialog("NE KADAR YAKINDAK� NOKTALARI YAKALASIN???")

    BD.Getfloat "item","UZAKLIK",0,1

    if BD.showmodal then


      for i = 0 to .numobject-1
        set o = .getobject(i)
          if o.tag = opline  and o.tabaka=.foundlayer("ALAN_GECICI") or o.tabaka=.foundlayer("ALAN_USTHAKKI") then

          .drawobject o,1
          set p = .getplineext(o)
             for j = 0 to p.num-1
                 NoktaAyir p.cor(j),BD
                 .putplineext o,p
                 .putobject i,o
             next

           end if


           next
           end if
msgbox "Parsel k�r�klar� noktalara �ekildi"
end with
End Sub
'-------------------------------------------------------------------------
sub noktaayir(c,BD)
dim obj1
dim x
dim b
with netcad
dim w: set w=.newworld(0,0,0,0):   w.BuildRegion c,BD.ValueByName("item"),BD.ValueByName("item")
.setfilter w,array(),array(opoint)
Do
set obj1=.getnextobject
if obj1 is nothing then
exit do
else
end if

.drawobject obj1,110
c.x=obj1.p1.x
c.y=obj1.p1.y
c.z=obj1.p1.z

noktaayir1 c,obj1,BD

loop




.resetfilter
end with

end sub
    
'-------------------------------------------------------------------------
sub noktaayir1(c,obj1,BD)
dim obj
with netcad
dim w: set w=.newworld(0,0,0,0):   w.BuildRegion c,BD.ValueByName("item"),BD.ValueByName("item")
.setfilter w,array(.foundlayer("ALAN_USTHAKKI_NOKTA"),_
                   .foundlayer("ALAN_GECICI_NOKTA")),array(opoint)
Do
set obj=.getnextobject
if obj is nothing then
exit do
else
end if

obj.p1.x=obj1.p1.x
obj.p1.y=obj1.p1.y
obj.p1.z=obj1.p1.z
.putobject .curobjpos, obj


loop
.resetfilter
end with

end sub





SUB Main
DIM ss,o,i,j,oo,p,sel,poly,tabaka,yazi,a   ,bd, secenek
DIM kt() ,t()

With netcad
a=.getparam(94)*1.7/1000
 set SEL = .NewSelectionSet             ' Yeni kume yarat
  set o = .NewObject
  set poly=.newpoly

   .setparam beginblock,true
   if SEL.SELECT("Kapal� �oklu do�rular� se�",array(opline)) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy

      set poly=.getplineext(o)
       yazi=split(o.pname,"_")
      .AddObject (.MakeText (poly.CenterOfMass, yazi(1)  , 0,0, a,0,"M",.CreateLayer("PARSEL_NO",10)))
    next
    .setparam endblock,true
    .NetcadCommand("REDRAW")
    set sel = nothing
    set poly = nothing               ' obje icin aldigimiz memory'i geri ver
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if
End With
END SUB





SUB Main
DIM ss,o,i,j,oo,p,sel,poly,tabaka,yazi,a,c   ,bd, secenek
DIM kt() ,t()

With netcad
a=.getparam(94)*2/1000
 set SEL = .NewSelectionSet             ' Yeni kume yarat      .CenterOfMass
  set o = .NewObject
  set poly=.newpoly

   .setparam beginblock,true
   if SEL.SELECT("Kapal� �oklu do�rular� se�",array(opline)) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy

      'tabaka=.LayerNameOf(o.tabaka)&"_KAD_ADA_NO"
      set poly=.getplineext(o)
       set c = poly.CenterOfMass
      .AddObject (.MakeText (c,o.pname, 0,0, a,0,"M",.CreateLayer("ADA_NO",4)))
    next
    .setparam endblock,true
    .NetcadCommand("REDRAW")
    set sel = nothing
    set poly = nothing               ' obje icin aldigimiz memory'i geri ver
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if
End With
END SUB

' V0.001 : Plan alanlar�na g�re farkl� tabakalardaki objelerin oranlanmas� i�in haz�rlanm��t�r.
' plan d��� ama�lar i�in de kullan�labilir.
Sub Main
Dim i,ada,bina,adaTabaka,arrType,arrValue,cond,lyChar,aln,j
adaTabaka = "SINIR_PLANONAMA"  ' Ada Tabakas�
lyChar = "PL" 'Ada i�inde bak�lacak tabakalar i�in ba�lang�� kriteri
  with Netcad
    redim arrType(0)
    redim arrValue(0)
    aln = 0
    j = 0
    set ada = .newobject()
    .setFilter nothing,array(.foundlayer(adatabaka)),array(opline)
    while .getNextObject2(ada)
      set bina = .newobject()
      .setFilter ada.limits,array(),array(opline)
      while .getNextObject2(bina)
        if (bina.tabaka <> .foundlayer(adatabaka)) and (ada.getObjectasPline.inpoly(bina.getObjectasPline.centerofmass)) and inStr(.layerNameOf(bina.tabaka),lyChar) > 0 then
           cond = checkValInArray(arrType,bina.tabaka)
           '.drawobject bina,red
           if cond = -1 then
             redim preserve arrType(ubound(arrType)+1)
             redim preserve arrValue(ubound(arrValue)+1)
             arrType(ubound(arrtype)) = bina.tabaka
             arrValue(ubound(arrValue)) = abs(bina.getobjectaspline.area)
             aln = aln + abs(bina.getobjectaspline.area)
           else
             arrValue(cond) = arrValue(cond) + abs(bina.getobjectaspline.area)
             aln = aln + abs(bina.getobjectaspline.area)
           end if
         end if
      wend
      .resetFilter
      'alertItall arrType,arrValue,aln
      writeToFile "",arrType,arrValue,aln,ada.pname,j
      j = j +1
      aln = 0
      redim arrType(0)
      redim arrValue(0)
      set bina = nothing
    wend
    set ada = nothing
    .resetFilter
  end with
  msgbox "Sonu�lar C:\Sonuclar.TXT dosyas�na yaz�ld�. "
End Sub

function alertItall(arrType,arrValue,adaAlan)
dim dlg,i
  with netcad
    set dlg = .newBDialog("Ada Sonu�lar�")
      for i = 0 to ubound(arrType)
        if i > 0 then
          dlg.PutPrompt .layerNameOf(arrType(i)) & " : %" & Round((arrValue(i)*100)/adaAlan,4)
        end if
      next
    if dlg.ShowModal then
    end if
  end with
end function


function checkValInArray(arr,val)
dim i
  for i = 0 to ubound(arr)
    if arr(i) = val then
      checkValInArray = i
      exit function
    end if
  next
  checkValInArray = -1
end function

function writeToFile(fName,arrType,arrValue,adaAlan,adaName,cond)
dim fso ,file,i
  fName = "C:\Sonuclar.TXT"
  set fso = CreateObject("Scripting.FileSystemObject")
  if cond = 0 then
    set file  = fso.CreateTextFile(fName,false)
  else
   set file  = fso.OpenTextFile(fName,8)
  end if
  file.WriteLine "ADA NO : " & adaName
  file.writeLine "--------------------------"
  for i = 0 to ubound(arrType)
    if i > 0 then
      file.WriteLine netcad.layerNameOf(arrType(i)) & " : %" & Round((arrValue(i)*100)/adaAlan,4)
    end if
  next
  file.WriteBlankLines(3)
  file.close
end function

' DGN Saklamadan �nce �al��t�r�lmal�.
' 20050426
' Bar�� G�ral

 sub main()
 dim o
   with netcad
     set o = .newobject
     .setFilter nothing,array(),array(oshape)
     while .getnextobject2(o)
       o.p1.x = o.p1.x + 0.05
       o.p1.y = o.p1.y - 0.6
       .putobject .curobjpos,o
     wend
   end with
 end sub


'AMA�: UST USTE CIZILMIS KAPALI ALANLARI BULMAK
'GIRDILER : KAPALI ALANLAR - COKLUDOGRU
'KULLANIM : SECIM MENUSU �LE KAPALI ALANLAR SECILIR CIFT OBJELER FARKLI TABAKAYA ALINIR
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln   ,PlnBulunan, gt, ft


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Parsel","Ada Poligon Tabakas�", "SOKAK_AKS", 15

  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))

    if  ParselTabaka=-1  then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  gt=.CreateLayer ("GEOCIFT",red)
  ft=.CreateLayer ("FULLCIFT",green)
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and .IsLayerOpen(parsel.tabaka) and parsel.tabaka<>gt and parsel.tabaka<>ft then
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(), array(opline)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           set plnBulunan=.Getplineext(by)
           if pln.CenterOfMass.y=PlnBulunan.CenterOfMass.y and pln.CenterOfMass.x=PlnBulunan.CenterOfMass.x and .curobjpos<>i then
             .DrawObject By, RED
             if by.tabaka=Parsel.tabaka and by.pname=parsel.pname then
                by.tabaka=ft
               .putobject .CurObjPos,by
             end if
             if by.tabaka<>ft then
                by.tabaka=gt
               .putobject .CurObjPos,by
             end if
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  '.NetcadCommand("REGEN")
end with
END SUB


' Yazan :Hasan Mutlu hasan@hasanmutlu.com
' Tarih : 25.01.2007
' A��klama : 

Sub Main
Dim c
  with Netcad
       .setmessage "Proje duzenleniyor"
       .NetcadCommand "PROJECT CLEAN 1,1,1,1,1"

	msgbox "Islem basari ile gerceklesti",,"Proje Duzenle"
 .setmessage ""
  end with
end sub



' Amac  : Aktif Projenin Projeksiyonunu ve Objelerini UTM6/36/WGS84 e cevirmek
'         Ancak Bunun �alismasi i�in, Aktif Projenin mevcut bir projeksiyonu olmak zorunda.
' Tarih : 02.08.2005
' Yazan : Kamran �zcan

Sub Main
dim pc

  set pc = Netcad.NewProjection
  pc.ProjectionType = PR_UTM
  pc.Zone = 36
  pc.Datum = DATUM_WGS84
  pc.SetToCurrentProject TRUE   ' Objeleride d�n��t�r�r
  Netcad.FindWorld
End Sub

' Amac  : Raster Projeksiyon donusumu
' Tarih : 02.08.2005
' Yazan : Kamran �zcan

Sub Main
dim pc

  set pc = Netcad.NewProjection
  pc.ProjectionType = PR_GEO
  pc.Datum = DATUM_WGS84

  msgbox Netcad.RasterProjection("D:\ornekler\raster\i35_A1.ECW","C:\deneme.ecw",pc, 0,0,0)
End Sub

sub Main
dim ss,o,i,j,oo,c,p
dim gml,gc,refxml,srvxml
dim xml,cat,dlg
With netcad
     set c = .newc(0,0,0)
     set dlg = .NewBDialog("Referans Dosyalar")
     while .selectpoint("Se�ti�iniz noktadaki referans katmalar belirlenecektir",c,-1)  ' obje sec
     for i = 0 to ncrefman.numCategory-1
       for j=0 to ncrefman.category(i).numReference-1
       set cat = ncrefman.category(i).reference(j)

         if c.x > cat.limits.cll.x and _
            c.x < cat.limits.cur.x and _
            c.y > cat.limits.cll.y and _
            c.y < cat.limits.cll.x then
            if ncrefman.category(i).isvisible and ncrefman.category(i).active  then
              dlg.putprompt ncrefman.category(i).name&" : "&cat.name
              end if
          end if
       next
     next
    set gc = nothing
    set c = nothing
    if dlg.showmodal then exit sub
   wend
 End With
end sub


' Ama� : Referans Yap�s�na komut bazinda ula��m �rnekleri
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

Sub Main
   with netcad
      .NetcadCommand("RM CLEAR")
      .NetcadCommand("RM ADD CATEGORY GRIDLER")
      .NetcadCommand("RM ADD REFERENCE D:\Data\proje\dted\E020_N32.DT1 1 GRIDLER")
      .FindWorld
      .NetcadCommand("RM DELETE REFERENCE D:\Data\proje\dted\E020_N32.DT1")
      .NetcadCommand("RM SAVE")
   end with
End sub


' Ama� : Projedeki Referanslardan Grid Deste�i olanlar� bulur
'        Secilen noktalarin kotunu gosterir
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

Sub Main
dim Reflist,i,Grid,c,Z

  ' Grid Deste�i olan Aktif referanslari iste ve adlarini goster
  RefList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
  for i = 0 to UBound(RefList)
    msgbox RefList(i).DisplayName
    msgbox "Projection Type=" & RefList(i).Projection.ProjectionType
  next

  ' Bunlardan ilki uzerinde calis
  if UBound(RefList) >=0 then
    set Grid = RefList(0).Grid  ' Referanstan Grid nesnesi iste

    msgbox "Grid Minimum Kot = " & Grid.MinValue
    msgbox "Grid Maximum Kot = " & Grid.MaxValue
    msgbox "Grid Ge�ersiz Deger = " & Grid.NoData

    set c = Netcad.Newc(0, 0, 0)
    while Netcad.SelectPoint("Nokta g�ster", c, -1)
      Z = Grid.GetValue(c)
      msgbox "Noktan�n Kotu = " & Z
    wend
  end if

End Sub


' Ama� : Projedeki Referanslardan Kisayol Destekleyenleri Bulur
'        Ekrandan g�sterilen iki nokta aras�ndaki en k�sa yolu bulur
'        Kisayol Bulma sirasinda Filitre uygular, En az 10 m genisligindeki yollardan
'        gecebilir. Ayrica yol cinslerini ve agirliklarinida belirler.
'        Bulunan sonucu Obje olarak Netcade ekler. Ayrica Adreside Gosterir.
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

sub main
dim dlg,refList,roadWidth,index,usefilter
  with netcad
    set dlg = .newbdialog("EN KISAYOL SORGUSU")
    reflist = makeSupRefListforDialog()
    if reflist <> -1 then
      dlg.getCombo "refList","K�sa Yol i�in Referans",reflist,1
      dlg.GetFloat "roadWd","Minimum Yol Geni�li�i","10",2
      dlg.GetCheck "filter", "Filtre Kullan", 0
      if dlg.showmodal then
        if dlg.valuebyname("refList") <> 0 then
          index = dlg.valuebyname("refList")
          roadWidth = dlg.valuebyname("roadWd")
          usefilter = dlg.valuebyname("filter")
          findShortest index-1,UseFilter,roadWidth
        else
          msgbox "L�tfen Ge�erli Bir Referans Se�iniz !!!"
          exit sub
        end if
      end if
    else
      msgbox "Projede En K�sayol Sorgulamas� Destekleyen Bir Referans Bulunamam��t�r !!!"
    end if
  end with
end sub

Sub findShortest(index,UseFilter,roadWidth)
dim Reflist,i,SP,c1,c2,Adres,o,Filter

  ' Bunlardan ilki uzerinde kisayol bul
    RefList = NCRefman.GetSupportedReferences(Macro_Support_ShortestPath, TRUE)
    set SP = RefList(index).ShortestPath  ' Referanstan Kisayol nesnesi iste

    set c1 = Netcad.Newc(0, 0, 0)
    set c2 = Netcad.Newc(0, 0, 0)
    if Netcad.SelectPoint("K�sayol i�in Ba�lang�� noktas� g�ster", c1, -1) and _
       Netcad.SelectPoint("K�sayol i�in Biti� noktas� g�ster", c2, -1) then

      ' Yollara Filitre uygula (Genislik ve Yol cinsleri)
      ' Ancak Bu Filitrelerin uygulanmasi icin Referansin
      ' Network ozelliklerinde Yol Cinsi ve Genislik Kolonlarinin
      ' secilmis olmasi gerekir.
      if useFilter = 1 then ' Kullan�c� Filtre iste�i denetleniyor.
        set Filter = Netcad.NewNetworkFilter
        Filter.SetRoadWidth roadWidth      ' Yollar En Az 10 m genislikte olmali
        Filter.AddRoadKind "SOKAK", 2.0
        Filter.AddRoadKind "CADDE", 1.0
        Filter.AddRoadKind "BULVAR", 0.4
        SP.SetFilter Filter  ' Hazirlanan Filitreyi uygula
      end if
      if SP.FindShortestPath(c1, c2) then
        ' Yol Degerini goster
        msgbox "K�sayol Yol De�eri = " & SP.Cost

        ' Bulunan kisayolu obje olarak projeye ekle
        set o = Netcad.MakePline("KISAYOL", 0, 0, 0,0, 0, SP.Poly)
        o.renk = red
        Netcad.drawobject o,red

        ' Adres Tarifini goster
        Adres = SP.Address
        for i = 0 to UBound(Adres)
          msgbox Adres(i)
        next
      else
          msgbox "K�sayol bulunamad� !"
      end if
    end if
End Sub

function makeSupRefListforDialog()
dim RefList,i,tmpStr
  RefList = NCRefman.GetSupportedReferences(Macro_Support_ShortestPath, TRUE)
  if ubound(reflist) <> -1 then
    for i = 0 to UBound(RefList)
      tmpStr = split(RefList(i).DisplayName,".")
      makeSupRefListforDialog = makeSupRefListforDialog & "|" & tmpStr(ubound(tmpstr))
    next
  else
    makeSupRefListforDialog = -1
  end if
end function



' Ama� : Projedeki Referanslardan Network Analiz Destekleyenleri Bulur
'        Ekrandan bir baslangic noktasi g�sterildiginde,
'        Network uzerindeki diger tum ulasilabilen Noktalardan baslangica
'        Uzakligi 10000 metreden az olanlari
'        Ekranda kirmizi daire icinde gosterir.
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004
' En k�sa yol destekleyen referanslar comboya doldurulur.
' �lgili parametreler i�in form yap�s� .ncbdialog eklenecek.

sub main
dim dlg,refList,index,cost
  with netcad
    set dlg = .newbdialog("NETWORK ANAL�Z�")
    reflist = makeSupRefListforDialog()
    if reflist <> -1 then
      dlg.getcombo "reflist","Ge�erli Referanslar",refList,1
      dlg.GetFloat "cost","Uzakl�k Kriteri (m)","10000",2
    else
      msgbox "Projede Uygun Referans Yok !!!"
      exit sub
    end if
    if dlg.showmodal then
      cost = dlg.valuebyname("cost")
      if dlg.valuebyname("refList") > 0 then
        index = dlg.valuebyname("refList")
      else
        msgbox "Ge�erli Bir Referans Se�mediniz !!!"
        exit Sub
      end if
      NetworkAnalysis index-1,cost
    end if
  end with
end sub

Sub NetworkAnalysis(index,cost)
dim Reflist,i,NW,c,o,Filter, cnt

  ' Network Analiz Yapabilen Aktif referanslari iste ve adlarini goster
    RefList = NCRefman.GetSupportedReferences(Macro_Support_Network, TRUE)
  ' Bunlardan ilki uzerinde Network Analiz yap
    set NW = RefList(index).Network  ' Referanstan Network nesnesi iste
    set c = Netcad.Newc(0, 0, 0)
    if Netcad.SelectPoint("Ba�lang�� Noktas�n� g�ster", c, -1) then

      if NW.Analize(c) then
        cnt = 0
        ' Tum nodlara bak ve Ulasilabilen nodlari isaretle
        for i = 0 to NW.NodCount-1
          if not NW.Nod(i).IsBadCost then
            if NW.Nod(i).Cost < cost then
              set o = Netcad.MakeCircle(NW.Nod(i).c, 50, 0, 0, 0)
              Netcad.DrawObject o, red
              cnt = cnt + 1
            end if
          end if
        next
        msgbox cost & " metreden yak�n ve ula��labilen nokta say�s� = " & cnt
      else
          msgbox "Network Analiz Yap�lamad� !"
      end if
    end if
End Sub

function makeSupRefListforDialog()
dim RefList,i,tmpStr
  RefList = NCRefman.GetSupportedReferences(Macro_Support_Network, TRUE)
  if ubound(reflist) <> -1 then
    for i = 0 to UBound(RefList)
      tmpStr = split(RefList(i).DisplayName,".")
      makeSupRefListforDialog = makeSupRefListforDialog & "|" & tmpStr(ubound(tmpstr))
    next
  else
    makeSupRefListforDialog = -1
  end if
end function





' Ama� : Projedeki Referanslardan Grid Deste�i olanlar� bulur
'        ve Relief Shaded olarak (G�lgelendirilmi�) JPEG format�nda saklar
' Yazan: Kamran �zcan 
' Tarih: 30.09.2004

Sub Main
dim Reflist,i,Grid, Shade, BD

  ' Grid Deste�i olan Aktif referanslari iste ve adlarini goster
  RefList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
  for i = 0 to UBound(RefList)
    msgbox RefList(i).DisplayName
  next

  ' Bunlardan ilki uzerinde calis
  if UBound(RefList) >=0 then
    set Grid = RefList(0).Grid  ' Referanstan Grid nesnesi iste
    set Shade = Grid.GetReliefShading ' Gridten G�lgelendirme nesnesini iste
    Shade.SetXYAngle 45
    Shade.SetZAngle 45
    if Shade.SaveToFile("C:\TEST.JPG", IMAGE_JPG, TRUE) then
      msgbox "Raster Sakland�"
    end if
  end if
End Sub


' Ama� : Projedeki Referanslardan Grid Deste�i olanlar� bulur
'        ve Relief Shaded olarak (G�lgelendirilmi�) JPEG format�nda saklar
' Yazan: Kamran �zcan 
' Tarih: 30.09.2004
'Eklenecekler : 
'  Referans list komboya eklenecek, dosya ad� ve path'i se�ilecek.

sub main
dim dlg,refList
dim index,fileName,xyAngle,zAngle
  with netcad
    reflist =  makeSupRefListforDialog
    if reflist <> -1 then
      set dlg = .newBDialog("GRID > RELIEF SHADED RESIM OLU�TUR")
      dlg.GetCombo "refIndex","Grid Referanslar",refList,1
      dlg.GetFileName "fileName", "Resim Dosyas�n�n Ad�", "", "JPG Dosyalar�|*.JPG|Tum Dosyalar|*.*","JPG"
      dlg.GetFloat "xyAngle", "XY Y�n�ndeki A��", "45", 2
      dlg.GetFloat "zAngle", "Z Y�n�ndeki A��", "45", 2
      if dlg.showModal then
        index = dlg.ValueByName("refIndex")
        if dlg.ValueByName("fileName") = "" then
          msgbox "Bir Dosya Ad� ve yolu Se�mediniz"
          exit sub
        else
          fileName = dlg.ValueByName("fileName")
        end if
        xyAngle = dlg.ValueByName("xyAngle")
        zAngle = 45
        MakeReliefShadedFile index-1,fileName,xyAngle,zAngle
      end if
    else
      msgbox "Projede Grid Referans Bulunamam��t�r !!!"
    end if
  end with
end sub

function MakeReliefShadedFile(index,fileName,xyAngle,zAngle)
dim Reflist,i,Grid, Shade, BD
    RefList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
    set Grid = RefList(index).Grid  ' Referanstan Grid nesnesi iste
    set Shade = Grid.GetReliefShading ' Gridten G�lgelendirme nesnesini iste
    on error resume next
    Shade.SetXYAngle xyAngle
    on error resume next
    Shade.SetZAngle zAngle
    if Shade.SaveToFile(FileNAme, IMAGE_JPG, TRUE) then
      msgbox "Raster Sakland�"
    end if
End function

function makeSupRefListforDialog()
dim RefList,i,tmpStr
  RefList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
  if ubound(reflist) <> -1 then
    for i = 0 to UBound(RefList)
      tmpStr = split(RefList(i).DisplayName,"\")
      makeSupRefListforDialog = makeSupRefListforDialog & "|" & tmpStr(UBound(tmpStr))
    next
  else
    makeSupRefListforDialog = -1
  end if
end function


' Ama� : Profil Alma �rne�i. G�r�n�rl�k Analizi nesnesi kullan�larak
'        Projedeki Grid Referanslar �zerinde se�ilen iki nokta aras�ndaki
'        profili bulur ve Projede se�ilen bir yere ekler.
' Yazan: Kamran �zcan 
' Tarih: 16.09.2004

Sub Main
dim Vis, Reflist,i,Grid, c, c1, c2, delta,poly,o, p, snapc, markedpos
  with netcad

    set Vis = .NewVisibilityAnalysis

    ' Grid Deste�i olan Aktif referanslari, gorunurluk analizi nesnesine ekle
    RefList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
    for i = 0 to UBound(RefList)
      set Grid = RefList(i).Grid  ' Referanstan Grid nesnesi iste
      Vis.AddGrid Grid
    next

    if UBound(RefList) >=0 then
      set c1 = .Newc(0, 0, 0)
      set c2 = .Newc(0, 0, 0)
      set poly = .Newpoly
      while .SelectPoint("Ba�lang�� Noktas�n� g�ster", c1, -1) and _
         .SelectPoint("Biti� Noktas�n� g�ster", c2, -1)

         ' Profili Bul
         delta = NCMath.Distance(c1, c2, false) / 20 ' Ayrica 20 noktadan olcum yap
         Vis.ProfileOnLine c1, c2, delta, poly

         ' Bulunan Profili cizim haline getir
         set p = .Newpoly
         for i = 0 to poly.num-1
           set c = .Newc(0, 0, 0)
           c.y =   NCMath.Distance(c1, poly.Cor(i),false)  ' plandan mesafeler
           c.x =   poly.Cor(i).z                           ' dusey degerler = kot
           p.AddCoor(c)
         next

         ' Profil egrisi hazir
         ' simdi obje bufferi isaretle, profil egrisini ekle
         ' ve kullaniciya projede bir yer sectir
         markedpos = .NumObject
         set o = .makepline("xx", 0,0,0,0,0, p)
         o.renk = red
         .addobject o

         set snapc = p.Cor(0)
         .Place "Profil icin yer gosteriniz", snapc, markedpos

         msgbox "Tamam"
      wend
    end if

  end with
End Sub

' Bu ornek Tan�ml� Paletin Lejand�n� Haz�rlar

Sub Main
  Dim i,j,o, p
  with netcad
    set p = .NewPoly
    p.AddCoor(.NewC(0,0,0))
    p.AddCoor(.NewC(100,0,0))
    p.AddCoor(.NewC(100,100,0))
    p.AddCoor(.NewC(0,100,0))
    for i = 0 to 15
       p.Cor(0).Y=i*100
       p.cor(1).Y=p.cor(0).Y+80
       p.cor(2).Y=p.cor(0).Y+80
       p.cor(3).Y=p.cor(0).Y
       for j = 0 to 15
         p.Cor(0).X=1500-j*100
         p.cor(1).X=p.cor(0).X
         p.cor(2).X=p.cor(0).X+80
         p.cor(3).X=p.cor(0).X+80
         set o = .MakePline("",POLYCLOSED+POLYFILLED,0,0,0,0,p)
         o.renk = J*16+I
         .AddObject o
         set o = nothing
       next
    next
    set p = nothing
    .FindWorld
  end with
end sub



' Yazan : 
' Tarih : 17.03.2010
' A��klama :

'..................................................................
  const S_DIGIT=2       'Uzunluk i�in  virgulden sonra hane Say�s�
  const S_CEPHE=0.003    'Cephe Yaz� Boyu

'..................................................................
 

Sub Main
Dim obj
dim OLCEK
dim a


  with Netcad
OLCEK =.getparam(94)
          .setfilter nothing, array(.foundlayer("PINDEX_5000"),_
                                    .foundlayer("PINDEX_2000"),_
                                    .foundlayer("PINDEX"),_
                                    .foundlayer("PINDEX_1000")),array(opline)

          do
            set obj=.getnextobject()
              if obj is nothing then
                  exit sub
              else
                       CepheYaz obj,OLCEK
                       obj.lt=2

              end if
            .PUTOBJECT .CUROBJPOS,OBJ


          loop
          .resetfilter



  end with
End Sub




sub CepheYaz(PlObje,OLCEK)
dim i
dim pl
dim n
dim LStart
dim LEnd
dim sKenar
dim Semt
dim sAngle
dim c
dim txt
dim RO

RO=50/ATN(1)

    with netcad
              set pl=plobje.GetObjectAsPline
              n=pl.num

              for i=0 to n-2
                LStart=i
                LEnd=i+1

                if LEnd=n then
                   LEnd=0
                end if

               sKenar=PlObje.pname 'Round(KENAR(pl.cor(LStart),pl.cor(LEnd)),S_DIGIT)

               'SKenar="Pafta-" & skenar

               Semt=SEMTBUL(pl.cor(LStart),pl.cor(LEnd))

               if semt >200 then
                  semt =semt-200
               elseif semt>=300 then
                  semt =semt-300
               else
                   semt=semt-400
               end if

               sAngle= (100-Semt) / RO

               set c=YanNokta(pl.cor(LStart),pl.cor(LEnd),S_CEPHE * (-1) *OLCEK * 0.75)

               set txt=.MakeText(c, sKenar, 0,0,S_CEPHE * OLCEK ,sAngle,"M",.createlayer("PINDEX_NO",0))
               .addobject(txt)


               set txt=nothing
               set c=nothing

              next

   end with
End sub

'...........................................................................
'   Yan Nokta Hesab�
'...........................................................................
Function  YanNokta(p1,p2,R)
dim Semt
dim sKenar
dim x,y
dim RO

RO=50/ATN(1)
with netcad
     Semt=Semtbul(p1,p2)/ RO
     sKenar=Kenar(p1,p2)/2

     y=p1.y + sKenar * Sin(Semt) + R * Cos(Semt)
     x=p1.x + sKenar * Cos(Semt) - R * Sin(Semt)

    set YanNokta=.newc(y,x,0)

end with
End Function

'.................................................
'            Semt Hesab�
'.................................................
FUNCTION SEMTBUL (PT1,PT2)
DIM DY
DIM DX
DIM SEMT
DIM ACI
DIM RO

    RO=50/ATN(1)

    DY=PT2.Y-PT1.Y
    DX=PT2.X-PT1.X

   IF DY=0 AND DX=0 THEN
      SEMT=0
   ELSEIF DY=0 AND DX<0 THEN
      SEMT=200
   ELSEIF DY=0 AND DX>0  THEN
      SEMT=0
   ELSEIF DY>0 AND DX=0 THEN
      SEMT=100
   ELSEIF DY<0 AND- DX=0 THEN
      SEMT=300
   END IF

   IF DX<>0  THEN
    ACI=ATN(ABS(DY/DX)) *RO

    IF DY>0 AND DX>0 THEN
       SEMT=ACI
    ELSEIF DY>0 AND DX<0 THEN
       SEMT=200-ACI
    ELSEIF DY<0 AND DX<0  THEN
       SEMT=200 + ACI
    ELSE
         SEMT=400-ACI
    END IF
  END IF

   SEMTBUL=SEMT

END FUNCTION


'........................................................
'     Kenar Hesab�
'........................................................
FUNCTION KENAR (PT1,PT2)
    KENAR=SQR((PT1.X-PT2.X)^2+(PT1.Y-PT2.Y)^2)
END FUNCTION


' Dil  : Visual Basic
' Ama� : �oklu do�ru kotlar�n�n ��genleme �ncesi seyreltilmesi. ve yaz�lara kot noktalr� �retilmesi.


'---------------------------- Coklu dogru sadelestirerek nokta uretir --------------------------
Sub CDogruSadelestir
Dim AtlamaSay, Point ,EgriKotAdim
Dim i,j,o,p
EgriKotAdim=50
AtlamaSay=1

with Netcad
  .SetFilter nothing, AcikTabakalar,array(opline)          'Filitre uygula
  Do
    set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
    if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
      exit do                ' Bu durumda donguyu durdur
     else
       set p = .getplineext(o)     ' objenin noktalarini tutan pline yapisini al
       if p.cor(0).z=70 then
         .AddObject  .MakePoint(p.cor(1), "T", "TIN",.FoundLayer("TIN") )
         .AddObject  .MakePoint(p.cor(p.num-2), "T", "TIN",.FoundLayer("TIN"))
         for j = 1 to p.num-2        ' coklu dogrunun herbir noktasi icin
           if (j mod AtlamaSay)=0 then .AddObject  .MakePoint(p.cor(j), "T","TIN" ,.FoundLayer("TIN"))
         next
         set p = nothing             ' noktalar icin aldigimiz memory'i geri ver
       end if
       set o = nothing               ' obje icin aldigimiz memory'i geri ver
    end if
  loop
end with
End Sub
'-----------------------------------------------------------------------------------------------------------
Sub KotYaziToNokta         ' Microstation dan gelen 123.12 kot yaz�lar�n�n '.' s�na kot noktas� atar.
Dim AtlamaSay, Point
Dim i,j,o,p

with Netcad
  .SetFilter nothing, AcikTabakalar,array(otext)          'Filitre uygula
  Do
    set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
    if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
      exit do                ' Bu durumda donguyu durdur
     else
       if o.s="." then .AddObject .MakePoint(o.p1, "T","TIN" ,.FoundLayer("TIN"))
       set o = nothing               ' obje icin aldigimiz memory'i geri ver
    end if
  loop
end with
End Sub
'----------------------------------------------------------------------------------------
Function AcikTabakalar
Dim i
Dim t()
  With netcad
    ReDim t(.NumLayers)
    For i=0 to .NumLayers-1        't() ye tabakalar� doldur
      if .IsLayerOpen(i) then t(i)=i else t(i)=-1
    Next
    AcikTabakalar=t
  End with
End Function


Sub Main
  With netcad

    msgbox "Sadece kotu �retilecek tabakalar� a��k b�kar�n.     "
    .CreateLayer "TIN", 11
    CDogruSadelestir
  End With
End Sub

'Tarih :  26/05/2001 V1.00
'Ama� : Ekrandan se�ilen objenin bulundu�u tabaka haricindeki tabakalar kapat�l�r.
'Girdiler : Netcad objeleri


SUB Main
DIM ss,o,i,j,oo,p,sel
DIM kt() ,t()

With netcad
  ReDim kt(.NumLayers)
  ReDim t(.NumLayers)
  set SEL = .NewSelectionSet             ' Yeni kume yarat
  set o = .NewObject
  if SEL.SELECT("Obje Se�",array()) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy
      kt(i)= o.tabaka
    next
    set ss = nothing
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if

  For i=0 to .NumLayers-1        't() ye tabakalar� doldur
    t(i)=i
  Next

  For j=0 To SEl.NE-1            'Kapanacak tabakalar�n filtrelerini �ret
    For i=0 to .NumLayers-1
      if kt(j)=i then t(i)=0
    Next
  Next

  For i=0 to .NumLayers-1        'Se�ilen obzelerin tabakas�n� kapat
    .CloseLayer t(i)
  Next

  .SetFilter .GetCurrentWindow, t, array()  'Kapanan objeleri zemin renginde �iz
    Do
    set oo = .GetNextObject
      if oo is nothing then
        exit do
      else
        .drawobject oo, Black
      end if
    Loop
  .FastRedraw
  .ResetFilter

End With
END SUB

'Tarih :18/05/2001 V2.00
'Ama� : Ekrandan se�ilen objenin bulundu�u tabaka kapat�l�r.
'Girdiler :Netcad objeleri
'Uyar� :Aktif tabakan�n kapat�lmas� engellenmi�tir.


SUB Main
DIM ss,o,i,j,oo,p

With netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    while .SelectObjectInstant("Se�ti�iniz objelerin tabakas� kapat�lacakt�r.",1,array(),ss)  ' obje sec
      set o = ss.objects(0)         ' Secim objesinin ilk objesini al
      if not(o.tabaka=.GetCurrentLayer) then
       .SetFilter .GetCurrentWindow, array(o.tabaka), array()          'Filitre uygula
       Do
         set oo = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if oo is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
         else
          .drawobject oo, Black   ' objeyi silinmis gibi siyahla ciz
         end if

       Loop
        .CloseLayer o.tabaka
        .FastRedraw
        else
          msgbox "aktif tabaka kapat�lamaz"
      end if
    wend
    set ss = nothing
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  End With
END SUB

' Tarih : 04/07/2001 V1.00
' Ama� : Se�ilen yaz�y� hedef �okludo�runun VT kodu olarak atar.
'Girdiler : Kapal� �okludogru - yaz�
' Kullan�m : TEK TEK GOSTERILEN YAZIYI KAPALI POLIGONUN
'            GIS BAGLANTI ANAHTARI DEGISKENINE ATAR
'


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    while .SelectObjectInstant ("Yaz�y� se�in",1,array(otext),ss)
      set b= ss.objects(0)                              'Bi�imleri b objesine ata
       .SelectObjectInstant "Se�ti�iniz objenin VT'si de�i�ecek.",1,array(opline),ss  ' obje sec
        set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
        h.ObjName=b.s
        .PutObject ss.indexs(0), h                      ' objeyi geri koy
        .DrawObject h,-1                                ' kendi rengi ile ciz
     wend
        set ss = nothing
        set b = nothing
        set h = nothing

  end with
END SUB

' Tarih : 04/07/2001 V1.00
' Ama� : Se�ilen yaz�y� hedef �okludo�runun VT kodu olarak atar.
'Girdiler : Kapal� �okludogru - yaz�
' Kullan�m : TEK TEK GOSTERILEN YAZIYI KAPALI POLIGONUN
'            GIS BAGLANTI ANAHTARI DEGISKENINE ATAR
'


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    while .SelectObjectInstant ("Yaz�y� se�in",1,array(otext),ss)
      set b= ss.objects(0)                              'Bi�imleri b objesine ata
       .SelectObjectInstant "Se�ti�iniz objenin VT'si de�i�ecek.",1,array(opline),ss  ' obje sec
        set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
        h.ObjName=b.s
        .PutObject ss.indexs(0), h                      ' objeyi geri koy
        .DrawObject h,-1                                ' kendi rengi ile ciz
     wend
        set ss = nothing
        set b = nothing
        set h = nothing

  end with
END SUB

'Makro Yazar�:      Harita Y�ksek M�hendisi O�uzalp BOZKURT
'Ama�:              TABAKA RENG�N�N DE���T�R�LMES�


Sub Main

with netcad

Dim obj
dim selection
Dim BD
dim item
dim a


set BD = Netcad.NewBDialog("RENK KODU G�R")

    BD.Getfloat "item","RENK KODU",0,1

    if BD.showmodal then

set selection = .NewSelectStatus

    while .SelectObjectInstant("RENK KODU G�R",1,array( ),selection)
          set obj = selection.objects(0)
          .DrawObject obj,blue


.SetFilter nothing, array(obj.tabaka), array( )

           do

             set obj=.getnextobject

             if obj is nothing then

           exit do

             end if

             with NCLayerManager

                  .layer(obj.tabaka).color=BD.ValueByName("item")

              end with
           loop

.resetfilter

    wend

    end if

.netcadcommand("REGEN")
 


end with

end sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT

sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

set secim = .NewSelectStatus()
set c = .newc(0,0,0)

top= 0
set yazi = .NewSelectStatus
while .SelectObjectInstant ("Ad� de�i�tirilecek yaz�y� se� ?",1,array(otext),yazi)


set o = yazi.objects(0)
top=top+o.s
o.tabaka=.createlayer("kapat",2)
.PutObject yazi.indexs(0), o

wend



if .SelectPoint("Nokta se�", c, 2) then



   set yaz1=.maketext(c,top,0,0,o.sc,o.angle,"1",.createlayer("ROL_CEPHE_1",4))
   .addobject(yaz1)


end if
set o = nothing




end with
end sub

'
'
const dbPath = "E:\PROJELER\NUMARATAJ\YOLORTA.MDB"
'
'
'
' B.G / 20050701
sub main
dim ds,o,p,i,clsSok,filter,yolIdList,skTbName,yolIdCol,kpYolCol,skSegColName
dim segmentList,kpNoCol,kpNoVal,kpTbName,kpSegColName,firstSegment,firstSegmentObj
dim nextSegment,seg,segObj,segNo,segCount,segNoCol,segCntCol,kpArr,kpVals
dim minTCol,maxTCol,minCCol,maxCCol
  with netcad
    clsSok = "YOLORTA"

    skTbName = "yolorta" ' Sokak Tablosu Ad�
    kpTbName = "KAPI"    ' Kap� Tablosu Ad�
    yolIdCol = "E_YOL_ID"' Yol Tablosundaki YOL_ID De�erinin bulundu�u kolon

    kpYolCol = "YOL_ID"  ' Kap� Tablosundaki YOL_ID De�erinin bulundu�u kolon
    kpNoCol =  "KAPI_NO"   ' Kap� Tablosundaki kap�no de�erinin bulundu�u kolon
    kpSegColName = "SEGMENT_ID" ' Kap� Tablosundaki Segment_id de�erinin bulundu�u kolon
    skSegColName = "E_PARCA_ID" ' Yol Tablosunda segment_id de�erinin bulundu�u kolon

    segNoCol = "BOLUM_NO"      ' B�l�m Numaras� Kolonu
    segCntCol = "BOLUM_SAYI"   ' B�l�m Say�s� Kolonu

    minTCol = "MIN_TEK_KAPI_NO"
    maxTCol = "MAKS_TEK_KAPI_NO"
    minCCol = "MIN_CIFT_KAPI_NO"
    maxCCol = "MAKS_CIFT_KAPI_NO"

    redim kpCols(4)

    kpCols(0) = minTCol
    kpCols(1) = maxTCol
    kpCols(2) = minCCol
    kpCols(3) = maxCCol


    set firstSegmentObj = .newObject()
    set yolIdList =  getYolIdList(skTbName,yolIdCol)

    while not yolIdList.Eof

      if yolIdList(yolIdCol).Value <> 0 then

        firstSegment = findFirstSegment(kpTbName,yolIdList(yolIdCol).Value _
                      ,kpYolCol,kpSegColName,kpNoCol)

        if  firstSegment <> -1 then

          segNo = 1

          kpVals = getKapiNoValus(kpTbName,kpNoCol,kpYolCol,yolIdList(yolIdCol).Value)


          'msgbox kpVals(0) & " : " & kpVals(1) &" : " & kpVals(2) &" : "  &kpVals(3) &" : "


          segCount = getSegmentList(skTbName,yolIdCol,yolIdList(yolIdCol).Value)


          set firstSegmentObj = getFirstSegmentObject(sksegColName,firstSegment,clsSok,segNo,_
                                                      segCount,segNoCol,segCntCol,kpvals,kpCols)

          .drawObject .MakePline(0, 0,0,0,0, 3, firstSegmentObj),red
          seg = firstSegment
          set segObj = firstSegmentObj

          do
            nextSegment = getNextSegment2(segObj.limits,clsSok,skSegColName,seg,_
                                          yolIdCol,yolIdList(yolIdCol).Value,firstSegment)
            firstSegment = seg
            seg = nextSegment

            if seg = -1 then exit do
            segNo = segNo + 1
            set segObj =  getFirstSegmentObject(sksegColName,seg,clsSok,segNo,_
                                                segCount,segNoCol,segCntCol,kpvals,kpCols)

          loop Until seg = -1

          set firstSegmentObj = nothing
        end if
      end if

      yolIdList.MoveNext

    wend
  end with
end sub

function getNextSegment2(incLimit,sinif,segIdCol,notId,sokIdCol,sokId,notId2)
dim o,p,ds,filter,fooPoly,arr
  with netcad
    set p = getNewExtend(incLimit)
    set o = .newobject()
    .drawObject .MakePline(0, 0,0,0,0, 3, p),red
    set ds = ncconman.findDatasetByFC(false,sinif)
    set filter = .newdatasetfilter()
    filter.FilterType = NC_GISDS_Region
    set filter.poly = p
    ds.setFilter filter
    ds.first
    if ds.Open then
      while not ds.Eof
        if ds.columnByName(sokIdCol).Value = sokId and _
           ds.columnByName(segIdCol).Value <> notId and _
           ds.columnByName(segIdCol).Value <> notId2 then

          ds.geometry.draw red

          getNextSegment2 = ds.ColumnByName(segIdCol).Value
          ds.resetFilter

          set ds = nothing

          exit function
        end if
        ds.next
      wend
    else
      msgbox "Bilinmeyen Hata in : getNextSegment"
    end if
    ds.resetfilter
    set ds = nothing
    getNextSegment2 = -1
  end with
end function

function getFirstSegmentObject(segIdCol,segIdVal,cls,segNo,segCount,segNoCol,segCntCol,kpvals,kpCols)
dim ds,value,filter,p
  with netcad
    set ds = ncconman.findDatasetByFc(false,cls)
    set filter = .newdatasetFilter
    set p = .newpoly
    p.clear
    filter.filterType = 117
    filter.extraSQL = segidCol & " = " &segIdVal
    filter.Poly = p
    if not ds.Open then
      msgbox "B�yle Bir S�n�f Yok"
      exit function
    end if
     ds.first
     ds.Find segIdCol,segIdVal
     while Not ds.Eof
       if ds.ColumnByName(segIdCol).Value = segIdVal then
         ds.edit
         ds.columnbyname(segNoCol).Value = segNo
         ds.columnbyname(segCntCol).Value = segCount
         ds.columnbyname(kpCols(0)).Value = kpVals(0)
         ds.columnbyname(kpCols(1)).Value = kpVals(1)
         ds.columnbyname(kpCols(2)).Value = kpVals(2)
         ds.columnbyname(kpCols(3)).Value = kpVals(3)
         ds.post
         set getFirstSegmentObject = ds.geometry.p
         set ds = nothing
         exit function
       end if
       ds.next
     wend
     set ds = nothing
  end with
end function

function getYolIdList(tableName,yolIdCol)
dim con,sql,ds
  set con = adoConnectionObject(dbPath)
  sql = "SELECT DISTINCT "&yolIdCol&" FROM "&tableName
  set ds = adoRecordsetObject(sql,con)
  set getYolIdList = ds
end function

function getSegmentList(tableName,yolIdCol,yolIdVal)
dim con,sql,ds
  set con = adoConnectionObject(dbPath)
  sql = "SELECT * FROM "&tableName&" WHERE "&yolIdCol&" = "&yolIdVal
  set ds = adoRecordsetObject(sql,con)
  getSegmentList = ds.RecordCount
end function

function getKapiNoValus(kpTable,kpNoCol,yolIdCol,yolIdVal)
dim con,sql,ds,minT,maxT,minC,maxC,k,arr,kpNo
  set con = adoConnectionObject(dbPath)
  sql = "SELECT "&kpNoCol&" FROM "&kpTable&" WHERE "&yolIdCol&" = "&yolIdVal
  set ds = adoRecordsetObject(sql,con)
  minT = 1
  minC = 2
  maxT = 0
  maxC = 0
  while not ds.Eof
    kpNo = cInt(ds(kpNoCol).Value)
    if (kpNo mod 2) = 0 then
      if kpNo <= minC then
        minC = kpNo
      elseif kpNo >= maxC then
        maxC = kpNo
      end if
    else
      if kpNo <= minT then
        minT = kpNo
      elseif kpNo >= maxT then
        maxT = kpNo
      end if
    end if
    ds.MoveNext
  wend
  redim arr(4)
  arr(0) = minT
  arr(1) = maxT
  arr(2) = minC
  arr(3) = maxC
  getKapiNoValus = arr
end function


function findFirstSegment(kapiTable,kapiYolId,kapiYolCol,kapiSegCol,kapiNoCol)
dim con,sql,ds
  if kapiYolID <> 0 then
    set con = adoConnectionObject(dbPath)
    sql = "SELECT " &kapiSegCol& ", Min("&kapiNoCol&") AS KAPI FROM "&kapiTable&" GROUP BY "&kapiSegCol&", "&kapiYolCol&" HAVING ((("&kapiYolCol&")="&kapiYolId&")) ORDER BY Min("&kapiNoCol&")"
    set ds = adoRecordsetObject(sql,con)

    if ds.RecordCount <> 0 then
      findFirstSegment = ds(kapiSegCol).Value
    else
      findFirstSegment = -1
    end if
    set ds = nothing
  else
    findFirstSegment = -1
  end if
end function

function adoConnectionObject(dbPath)
dim conMain
   set conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function

function adoRecordsetObject(sql,connection)
dim tables
  set tables = CreateObject("ADODB.Recordset")
 'msgbox sql
  tables.Open sql,connection,1,3
  set adoRecordsetObject =  tables
end function

function getNewExtend(extend)
dim world
  with netcad

    set world = .newWorld(0,0,0,0)

    world.cll.x = extend.cll.x -10
    world.cll.y = extend.cll.y -10

    world.cur.x = extend.cur.x +10
    world.cur.y = extend.cur.y +10

    set getNewExtend = world.getAsPline

  end with
end function

' Konu : Yazilari g�stermek i�in kullan�lan sembolleri yaz� objesine �evirir
' 03.02.2005
' Bar�� G�ral
const tYazi = "YAZI" 'Yaz�lar�n bulunaca�� tabaka
sub main
dim o,oo,lyIndex
  with netcad
      if .foundlayer(tYazi) = -1 then
        lyIndex = .createLAyer(tYazi,red)
      else
        lyIndex = .foundlayer(tYazi)
      end if
    set o = .newobject
    .setfilter nothing,array(),array(oshape)
    while .getnextobject2(o)
      if o.sembolno = 72 then
        set oo = .MakeText(o.p1, "1", 0,0, 1,0,"M",lyIndex)
        .drawobject o,red
        .addobject oo
        .delobject .curobjpos,o
      elseif o.sembolno = 73 then
        set oo = .MakeText(o.p1, "2", 0,0, 1,0,"M",lyIndex)
        .drawobject o,red
        .addobject oo
        .delobject .curobjpos,o
      elseif o.sembolno = 74 then
        set oo = .MakeText(o.p1, "3", 0,0, 1,0,"M",lyIndex)
        .drawobject o,red
        .addobject oo
        .delobject .curobjpos,o
      end if
    wend
    set o = nothing
    .netcadcommand "REGEN"
  end with
end sub


const mainTable = "yolorta" ' Tablo Ad�
const colYolId = "E_YOL_ID" ' Yol Id kolonu
const colsegmentId = "E_PARCA_ID" ' Segment Id kolonu
const yolTableIdCol = "OBJECTID" ' Segment Tablosundaki id kolonu
const dbPath = "E:\PROJELER\NUMARATAJ\YOLORTA.MDB" ' veritaban�n�n yeri

sub main
dim o, i, j, dlg,sel
  with netcad
    set o = .newobject
    set dlg = .newBDialog("Yol ID de�eri giriniz")
    dlg.GetInteger "yolId", "Yol Id : ", 0
    set sel = .newSelectionSet
    if sel.select("Segmentleri Se�iniz",array(opline)) then
      if dlg.ShowModal then
        for i = 0 to sel.Ne-1
          j = sel.GetSelectedObject(i,o)

          if addYolId(o.objName,dlg.ValueByName("yolId"),yolTableIdCol,mainTable,colYolId) <> 0 then
            .drawobject o,cyan
          else
            .drawobject o,red
          end if
        next
      end if
    end if
  end with
end sub

function addYolId(segmentId,yolId,yolTableIdCol,mainTable,colYolId)
dim ds,con,sql
  set con = adoConnectionObject(dbPath)
  sql = "Select * From "&mainTable& " Where "&yolTableIdCol&" = '" &segmentId&"'"
  set ds = adoRecordSetObject(sql,con)
  while not ds.Eof
      if ds(colYolId).Value = 0 then
        ds(colYolId).Value = yolId
        addYolId = 0
      else
        msgbox "Yol Id 0 dan farkl� de�er atland�. ��aretlendi."
        addYolId = -1
      end if
     ds.update
     ds.movenext
  wend
end function

function adoConnectionObject(dbPath)
dim conMain
   set conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function

function adoRecordsetObject(sql,connection)
dim tables
  set tables = CreateObject("ADODB.Recordset")
  msgbox sql
  tables.Open sql,connection,1,3
  set adoRecordsetObject =  tables
end function




' Ama� : Istenen Siniftaki Objeleri Istenen Tabakaya Almak
' Girdiler : Netcad objeleri
' Kullan�m : S�n�f� ayn� olan objeleri istenilen tabakaya almak

Sub Main
Dim BD, i, sinif, Tabaka, TabNo, o

  set BD = Netcad.NewBDialog("")                 ' yeni dialog yarat
  BD.GetString  "item1","S�n�f", "", 15        ' Yazi iste. en fazla 15 karakter olsun
  BD.GetString  "item2","Tabaka", "", 15        ' Yazi iste. en fazla 15 karakter olsun
  if BD.showmodal then
    'MsgBox "Sinif = " & BD.ValueByName("item1")      ' kullanicinin girdigi degerleri goster
    'MsgBox "Tabaka = " & BD.ValueByName("item2")

    Sinif =  BD.ValueByName("item1")
    Tabaka = BD.ValueByName("item2")

    TabNo = Netcad.CreateLayer(Tabaka, red)

    for i = 0 to Netcad.numobject-1       ' projedeki tum objeleri sirayla tara
        set o = Netcad.getobject(i)         ' i. objeyi al
        if o.cls = Sinif then
          o.Tabaka = TabNo
          Netcad.putobject i,o              ' objeyi yenile
          Netcad.drawobject o, -1          ' objeyi ciz
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
    next

    MsgBox Sinif & " Objeleri " & Tabaka & " Tabakasina Alindi"

  end if
  set BD = Nothing
End sub


'Belirli bir say�dan ba�layarak objelere vtKodu atama
sub main
dim dlg,sel,obj,oi,i
  with netcad
    set obj = .newobject()
    set dlg = .newbDialog("Ba�lat� Kodu Atama")
    dlg.GetString "startVal", "Ba�lang�� De�eri", "VT_", 20
    dlg.GetString "Cls", "S�n�f", "", 50
    if dlg.ShowModal then
      set sel = .newSelectionSet
      if sel.SELECT("Ba�lant� Kodu �retmek istedi�iniz Objeleri Se�iniz", array()) then
        for i = 0 to sel.NE-1
          oi = sel.GetSelectedObject(i, obj)
          if not isNumeric(dlg.ValueByName("startVal")) then
           obj.objName = dlg.ValueByName("startVal")&i
          else
            obj.objName = dlg.ValueByName("startVal")+i
          end if
          obj.cls = dlg.ValueByName("Cls")
          .putObject oi,obj
        next
      end if
    end if
  end with
end sub





SUB Main
DIM ss,o,i,j,oo,p,sel,poly,tabaka,yazi,a   ,bd, secenek
DIM kt() ,t()

With netcad
a=.getparam(94)*3/1000
 set SEL = .NewSelectionSet             ' Yeni kume yarat
  set o = .NewObject
  set poly=.newpoly

   .setparam beginblock,true
   if SEL.SELECT("Kapal� �oklu do�rular� se�",array(opline)) then ' istenen turleri kumeye ekle
    for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
      j = SEL.GetSelectedObject(i, o)    ' objeyi geri koy

      'tabaka=.LayerNameOf(o.tabaka)&"_KAD_PAR_NO"
      set poly=.getplineext(o)
       yazi=split(o.pname,"/")
      .AddObject (.MakeText (poly.CenterOfMass, yazi(1)  , 0,0, a,0,"M",.CreateLayer("PARSEL_NO",10)))
    next
    .setparam endblock,true
    .NetcadCommand("REDRAW")
    set sel = nothing
    set poly = nothing               ' obje icin aldigimiz memory'i geri ver
    set o = nothing               ' obje icin aldigimiz memory'i geri ver
  end if
End With
END SUB

' Yazan : Erc�ment KORKMAZ
' Tarih : 22.03.2006
' A��klama : spatial alan olan bir tablonun a��rl�k merkezine nokta objesi �retme noktalar�n GIS Ba�lant� anahtar�na alan
' objesini anahtar�n� yazma
Sub Main
Dim i, ds, con, a, b, c , ta, s, o , po, obj, snf, oi, bd, co, col, nok, nokk, sn
a = 0
c = 0
s = 0
sn = 1
  with Netcad
  set nok = .Newc(0, 0, 0)
    set bd = .NewBDialog("SINIF VE KOLON ADI G�R")
    bd.GetString "snf", "SINIF G�R", "MARMARA", 20
    bd.GetString "col", "ID KOLON ADI G�R", "OBJECTID", 20
    bd.showmodal
    snf = bd.ValueByName("snf")
    col = bd.ValueByName("col")
    set ds =  NCConman.FindDatasetByFC(false, snf)
      ds.open
      ds.first
       set con = ds.ColumnByName(co)
       set oi = ds.ColumnByName(col)
      while not ds.EOF
       on error resume next
        set o=  ds.Geometry.p
           set nok = o.CenterOfMass
           set nokk = .MakePoint(nok, sn, "nok", 0)
        nokk.objname= oi.value
       .AddObject nokk
       sn = sn + 1
      ds.next
      wend
  end with
End Sub


' Yazan : ERC�MENT KORKMAZ
' Tarih : 26.10.2006
' A��klama :  KOT B�LG�S� OLAN SPAT�AL VER�YE KOTLU NOKTA �RETME

Sub Main
Dim i, ds, con, a, b, c , ta, s, o , po, obj, snf, oi, bd, co, col,kot, stp
a = 0
c = 1
s = 0
  with Netcad
    set bd = .NewBDialog("SINIF VE COLON ADI G�R")
    bd.GetString "snf", "SINIF G�R", "B", 20
    bd.GetString "col", "ID KOLON ADI G�R", "OBJECTID", 20
    bd.GetString "co", "TEMAT�K KOLON ADI G�R", "KOT", 20
    BD.GetInteger "step","NOKTA ARALIK SAYISI", 1
    bd.showmodal
    snf = bd.ValueByName("snf")
    co = bd.ValueByName("co")
    col = bd.ValueByName("col")
    stp = bd.ValueByName("step")
    set ds =  NCConman.FindDatasetByFC(false, snf)
      ds.open
      ds.first
       set con = ds.ColumnByName(co)
       set oi = ds.ColumnByName(col)
      while not ds.EOF
       on error resume next
        kot= con.value
        if con.value =< 0 then a = CInt(con.Value)
        if con.value = "" then a = 0
        NCLayerManager.Add  "NOKTA", 4
        a = NCLayerManager.Find ("NOKTA")
        set o=  ds.Geometry.p
        for i = 0 to o.num-1 step stp
     set po =  .MakePoint(o.cor(i), c, "A", a)
          po.p1.z = kot
       .AddObject po
       c = c+1
       next
      ds.next
      wend
  end with
End Sub

' Yazan : Erc�ment KORKMAZ
' Tarih : 18.11.2005
' A��klama : spatial olan bir tablonun obje haline getirilerek  tabaka bazl� tematik yapma
Sub Main
Dim i, ds, con, a, b, c , ta, s, o , po, obj, snf, oi, bd, co, col
a = 0
c = 0
s = 0
  with Netcad
    set bd = .NewBDialog("SINIF VE COLON ADI G�R")
    bd.GetString "snf", "SINIF G�R", "BAYRAMPASA", 20
    bd.GetString "col", "ID KOLON ADI G�R", "OBJECTID", 20
    bd.GetString "co", "TEMAT�K KOLON ADI G�R", "ANA_FONKSIYON_Z", 20
    bd.showmodal
    snf = bd.ValueByName("snf")
    co = bd.ValueByName("co")
    col = bd.ValueByName("col")
    set ds =  NCConman.FindDatasetByFC(false, snf)
      ds.open
      ds.first
       set con = ds.ColumnByName(co)
       set oi = ds.ColumnByName(col)
      while not ds.EOF
       on error resume next
        a= con.value
        if con.value =< 0 then a = CInt(con.Value)
        if con.value = "" then a = 0
        NCLayerManager.Add  a, 4
        a = NCLayerManager.Find (a)

        set o=  ds.Geometry.p
     set po =  .MakePline (a, 3, 0, a,0, 0, o)
        po.objname= oi.value
       .AddObject po
      ds.next
      wend
      for b= 102 to 160
        set ta = NCLayerManager.Layer (s)
        ta.Color = b
        s= s + 1
      next
       for i = 0 to .NumObject-1
        set obj = .GetObject(i)
        'obj.Tabaka = .FoundLayer (obj.pname)
        obj.cls  = snf
        .PutObject i, obj
       next
  end with
End Sub

' Ver   : 0.01
' Ama�  : Veritaban� kolonlar�ndaki de�erlerin,Spatial Objenin Z,kalinlik
'         veya �izgi kal�nl��� de�erlerine atanmas� ve veritaban�
'         objesinin ncz objesi olarak  projeye eklenmesi
' Tarih : 28.02.2005
' Haz�rlayan : Bar�� G�ral
sub main
dim dlg,clsList,cls,layer,zcol,clsArr,zParam,owParam,lwParam,simParam,simp
  with netcad
    clsList = findFClassList
    clsArr  = split(clsList,"|")
    set dlg = .newBdialog("Parametreler")
    dlg.getcombo "fcClass","SINIF",clsList,0
    dlg.getString "zCol","Ekstra Kolon","",25
    dlg.GetCheck "chZ", "Kolonu Z Olarak Kullan", 1
    dlg.GetCheck "chOw", "Kolonu Kal�nl�k Olarak Kullan", 0
    dlg.getCheck "chLw","Kolonu �izgi Kal�nl��� Olarak Kullan",0
    dlg.getString "layer","Yeni Obje Tabakas�","POLY",25
    dlg.getCheck  "chSimplify","�oklu Do�rular� Basitle�tir",0
    dlg.GetFloat "simp", "Basitle�tirme E�ik De�eri", 1, 2
    if dlg.showmodal then
      cls     = clsArr(dlg.valueByName("fcClass"))
      layer   = dlg.ValueByName("layer")
      zParam  = dlg.ValueByName("chZ")
      simParam = dlg.ValueByName("chSimplify")
      zCol =  dlg.ValueByName("zCol")
      if simParam = 1 then
        simp = dlg.ValueByName("simp")
      end if
      owParam = dlg.ValueByName("chOw")
      lwParam = dlg.ValueByName("chLw")
      if cls <> "" then
        if layer <> "" then
          walkOnDb cls,zCol,layer,zParam,owParam,lwParam,simParam,simp
         else
           .Message 2, "Tabaka ismi girmediniz !!!", ""
         end if
       else
        .Message 2, "Bir S�n�f Se�mediniz !!!", ""
        'msgbox "Bir S�n�f Se�mediniz !!!"
      end if
    end if
  end with
end sub

function walkOnDb(sinif,zCol,layer,zParam,owParam,lwParam,simParam,simp)
dim ds,o,i,j,p,c,y,x,z,zo,index,geomType
  with netcad
    set ds = ncconman.finddatasetbyFc(false,sinif)
    set p = .newPoly()
    if ds is nothing then
      .message 3,"Projede "&sinif&" isimli bir s�n�f yok !!!",""
      exit function
    end if
    if .foundlayer(layer) = -1 then
      .createLayer layer,red
    end if
    index = 0
    geomType = checkObjType(sinif)
    if ds.open then
      ds.first
      if geomType = 7 then
       .message 2,"Se�ti�iniz "&sinif&" s�n�f� i�in geometry tipi tan�ml� de�il. L�tfen geometri tipini tan�mlay�p yeniden deneyiniz !!!",""
        exit function
      end if
      while not ds.eof
        set p = ds.geometry.p
        'PolyLine or Line : Closed or Open Poly
         if not .escPressed then
           if geomType = 4 or geomType = 2 then
             if zCol <> "" then
               addPolyObjToProject p,ds.ColumnbyName(zCol).Value,layer,zParam,owParam,lwParam,simParam,simp
             else
               addPolyObjToProject p,"",layer,zParam,owParam,lwParam,simParam,simp
             end if
           end if
          'Point
           if geomType = 1 then
             if zCol <> "" then
               addPointObjToProject p,ds.ColumnbyName(zCol).Value,layer,zParam
             end if
           end if
          index = index+1
          .backmessage
          .setmessage ds.recordNo & " objesi projeye eklendi !!!"
          ds.next
        else
          exit function
        end if
      wend
      ds.close
    end if
    .backmessage
  end with
  set ds = nothing
  set c = nothing
  set p = nothing
end function

function checkZCol(zcol,sinif)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds is nothing then
    exit function
  end if
  if ds.open then
    for i = 0 to ds.columncount-1
      if ds.columnbyNo(i).name = zCol then
          checkZCol = 1
        exit function
      end if
    next
  end if
  checkZCol = 0
  ds.close
end function

function checkObjType(sinif)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds is nothing then
    checkObjType = 0
    exit function
  end if
  if ds.open then
    checkObjType = ds.geometry.GeometryType
  end if
  ds.close
end function

function addPolyObjToProject(p,z,layer,zParam,owParam,lwParam,simParam,simp)'Polyline obje noktalar�n�'tarar ve kot de�erini objelere atar
dim j,obj
  with netcad
  set obj = .newobject
   'msgbox z
   if zParam = 1 then
     for j = 0 to p.num-1
       p.cor(j).z = z
     next
   end if
   if simParam = 1 then
     p.simplify 0,simp
   end if
   set obj = .MakePline("", 0, 0, .foundlayer(layer),0, 1, p)
   if owParam = 1 then
    ' msgbox z
     obj.thicknes = z
   end if
   if lwParam = 1 then
     obj.w = z*10
   end if
   .addObject obj
  end with
  set obj = nothing
  set p = nothing
end function

function addPointObjToProject(p,z,layer,zParam)
dim obj
  with netcad
    if zParam = 1 then
      p.cor(0).z = z
    end if
    set obj = .MakePoint(p.cor(0),"","",.foundlayer(layer))
    .addObject obj
  end with
  set obj = nothing
  set p = nothing
end function

function findFClassList()
dim i,j
  with ncconman
    findFClassList = ""
    for i = 0 to .NumConnection-1
       for j = 0 to .Connection(i).NumTables-1
         if .Connection(i).table(j,false).isspatial then
           if j > 0 then
             findFClassList = findFClassList & "|" & getSpatialColInfo(.Connection(i).table(j,false).tableName,i)
           else
             findFClassList = getSpatialColInfo(.Connection(i).table(j,false).tableName,i)
           end if
         end if
       next
    next
  end with
end function

function getSpatialColInfo(tableName,conId)
dim i,j
  with ncconman
      for i=0 to .connection(conId).numTables-1
        if .connection(conId).table(i,false).tableName = "NCSPATINFO" then
            .connection(conId).table(i,false).open
            while not .connection(conId).table(i,false).eof
            if .connection(conId).table(i,false).columnbyname("TABLENAME").value = tableName then
               getSpatialColInfo = .connection(conId).table(i,false).columnbyname("FCNAME").value
               .connection(conId).table(i,false).close
               exit function
            end if
            .connection(conId).table(i,false).next
            wend
        end if
      next
     .connection(conId).table(i,false).close
  end with
end function




' Ver   : 0.01
' Ama�  : Veritaban� kolonlar�ndaki de�erlerin,Spatial Objenin Z,kalinlik
'         veya �izgi kal�nl��� de�erlerine atanmas� ve veritaban�
'         objesinin ncz objesi olarak  projeye eklenmesi
' Tarih : 28.02.2005
' Haz�rlayan : Bar�� G�ral
' 4.5 Poly z leri i�in de�i�iklik yap�ld� (06/12/2005) Adil YOLTAY
sub main
dim dlg,clsList,cls,layer,zcol,clsArr,zParam,owParam,lwParam,simParam,simp
  with netcad
    clsList = findFClassList
    clsArr  = split(clsList,"|")
    set dlg = .newBdialog("Parametreler")
    dlg.getcombo "fcClass","SINIF :",clsList,1
    dlg.getString "zCol","Ekstra Kolon :","YUKSEKLIK",25
    dlg.GetCheck "chZ", "Kolonu Z Olarak Kullan", 1
    dlg.GetCheck "chOw", "Kolonu Kal�nl�k Olarak Kullan", 0
    dlg.getCheck "chLw","Kolonu �izgi Kal�nl��� Olarak Kullan",0
    dlg.getString "layer","Yeni Obje Tabakas� :","TAU_POLY",25
    dlg.getCheck  "chSimplify","�oklu Do�rular� Basitle�tir :",1
    dlg.GetFloat "simp", "Basitle�tirme E�ik De�eri :", 5, 2
    if dlg.showmodal then
      cls     = clsArr(dlg.valueByName("fcClass"))
      layer   = dlg.ValueByName("layer")
      zParam  = dlg.ValueByName("chZ")
      simParam = dlg.ValueByName("chSimplify")
      zCol =  dlg.ValueByName("zCol")
      if simParam = 1 then
        simp = dlg.ValueByName("simp")
      end if
      owParam = dlg.ValueByName("chOw")
      lwParam = dlg.ValueByName("chLw")
      if cls <> "" then
        if layer <> "" then
          walkOnDb cls,zCol,layer,zParam,owParam,lwParam,simParam,simp
         else
           .Message 2, "Tabaka ismi girmediniz !!!", ""
         end if
       else
        .Message 2, "Bir S�n�f Se�mediniz !!!", ""
        'msgbox "Bir S�n�f Se�mediniz !!!"
      end if
    end if
  end with
end sub

function walkOnDb(sinif,zCol,layer,zParam,owParam,lwParam,simParam,simp)
dim ds,o,i,j,p,c,y,x,z,zo,index,geomType
  with netcad
    set ds = ncconman.finddatasetbyFc(false,sinif)
    set p = .newPoly()
    if ds is nothing then
      .message 3,"Projede "&sinif&" isimli bir s�n�f yok !!!",""
      exit function
    end if
    if .foundlayer(layer) = -1 then
      .createLayer layer,red
    end if
    index = 0
    geomType = checkObjType(sinif)
    if ds.open then
      ds.first
      if geomType = 7 then
       .message 2,"Se�ti�iniz "&sinif&" s�n�f� i�in geometry tipi tan�ml� de�il. L�tfen geometri tipini tan�mlay�p yeniden deneyiniz !!!",""
        exit function
      end if
      while not ds.eof
        set p = ds.geometry.p
        'PolyLine or Line : Closed or Open Poly
         if not .escPressed then
           if geomType = 4 or geomType = 2 then
             if zCol <> "" then
               addPolyObjToProject p,ds.ColumnbyName(zCol).Value,layer,zParam,owParam,lwParam,simParam,simp
             else
               addPolyObjToProject p,"",layer,zParam,owParam,lwParam,simParam,simp
             end if
           end if
          'Point
           if geomType = 1 then
             if zCol <> "" then
               addPointObjToProject p,ds.ColumnbyName(zCol).Value,layer,zParam
             end if
           end if
          index = index+1
          .backmessage
          .setmessage ds.recordNo & " objesi projeye eklendi !!!"
          ds.next
        else
          exit function
        end if
      wend
      ds.close
    end if
    .backmessage
  end with
  set ds = nothing
  set c = nothing
  set p = nothing
end function

function checkZCol(zcol,sinif)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds is nothing then
    exit function
  end if
  if ds.open then
    for i = 0 to ds.columncount-1
      if ds.columnbyNo(i).name = zCol then
          checkZCol = 1
        exit function
      end if
    next
  end if
  checkZCol = 0
  ds.close
end function

function checkObjType(sinif)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds is nothing then
    checkObjType = 0
    exit function
  end if
  if ds.open then
    checkObjType = ds.geometry.GeometryType
  end if
  ds.close
end function

function addPolyObjToProject(p,z,layer,zParam,owParam,lwParam,simParam,simp)'Polyline obje noktalar�n�'tarar ve kot de�erini objelere atar
dim j,obj,cn

  with netcad
  set obj = .newobject
     set cn=.Newc(0,0,0)
   if zParam = 1 then
       cn.z=z

    for j = 0 to p.num-1
        cn.x= p.cor(j).x
        cn.y= p.cor(j).y
        p.cor(j)=cn
    next

   end if
   if simParam = 1 then
     p.simplify 0,simp
   end if
   set obj = .MakePline("", 0, 0, .foundlayer(layer),0, 1, p)
   if owParam = 1 then

     obj.thicknes = z
   end if
   if lwParam = 1 then
     obj.w = z*10
   end if
   .addObject obj
  end with
  set obj = nothing
  set p = nothing
end function

function addPointObjToProject(p,z,layer,zParam)
dim obj
  with netcad
    if zParam = 1 then
      p.cor(0).z = z
    end if
    set obj = .MakePoint(p.cor(0),"","",.foundlayer(layer))
    .addObject obj
  end with
  set obj = nothing
  set p = nothing
end function

function findFClassList()
dim i,j
  with ncconman
    findFClassList = ""
    for i = 0 to .NumConnection-1
       for j = 0 to .Connection(i).NumTables-1
         if .Connection(i).table(j,false).isspatial then
           if j > 0 then
             findFClassList = findFClassList & "|" & getSpatialColInfo(.Connection(i).table(j,false).tableName,i)
           else
             findFClassList = getSpatialColInfo(.Connection(i).table(j,false).tableName,i)
           end if
         end if
       next
    next
  end with
end function

function getSpatialColInfo(tableName,conId)
dim i,j
  with ncconman
      for i=0 to .connection(conId).numTables-1
        if .connection(conId).table(i,false).tableName = "NCSPATINFO" then
            .connection(conId).table(i,false).open
            while not .connection(conId).table(i,false).eof
            if .connection(conId).table(i,false).columnbyname("TABLENAME").value = tableName then
               getSpatialColInfo = .connection(conId).table(i,false).columnbyname("FCNAME").value
               .connection(conId).table(i,false).close
               exit function
            end if
            .connection(conId).table(i,false).next
            wend
        end if
      next
     .connection(conId).table(i,false).close
  end with
end function





 ' AMA� : Halihazir datalarda sundurmalar�n 3D icin temizlenmesi ;
 '        kapali alan ismi ve icindeki yazinin tabakasi verilerek islem yap�l�r.
 ' KULLANIM :Pafta kenarlar�nda kapatilan binalar�n icindeki cift yazilari siler
 '           icinde yazi olmayan binalar� sorunlu tabakasina alir
 '           (Kat adedi olmayanlar secilmis olur boylece sundurmalar secilir.)
 ' GIRDILER : KAPALI COKLUDOGRU - BINA_MESKUN
 '            TEXT - BINA_KATADEDI
 '
 '----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("sundurma ve pafta kenarlar�ndaki cift yazi temizle")          ' yeni dialog yarat
  BD.GetString  "Parsel","BINA", "BINA_MESKUN", 15
  BD.GetString  "Yazi","KAT_ADEDI", "BINA_KATADEDI", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.InPoly(by.p1) then
             s=s+1
             if s>1 then
               if iy=by.s then .DelObject .CurObjPos, by
             end if
             iy=by.s
           end if
         end if
       Loop

       if s>0 then
         if s=1 then
           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
          else


           .ResetFilter                ' Filitre uygulamayi birak.
           s=0
           mode=0
           iy=""
         end if
        else
         parsel.tabaka=.CreateLayer("sorunlu",7)
         .PutObject i, parsel
         .ResetFilter                ' Filitre uygulamayi birak.
         s=0
         mode=0
         iy=""
       end if
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB

const path = "E:\PROJELER\PLAN\odf\25000.odf"

function main
dim file,fso,str,arr,excSheet,excLine
dim i
 ' set fso = CreateObject("Scripting.FileSystemObject")
 ' set file = fso.openTextFile(path,1)
  set excSheet = CreateObject("Excel.Application")
  excSheet.workbooks.add
  excLine = 1
  with ncLayermanager
    for i = 0 to .numLayer-1
      excLine = excLine + 1
      excSheet.cells(excLine,1)= .layer(i).Name
    next
  end with
  excSheet.visible = True
  set fso = nothing
  set file = nothing
end function

'odf i�in de�i�tirmeyin
sub main34
dim file,fso,str,arr,excSheet,excLine
  set fso = CreateObject("Scripting.FileSystemObject")
  set file = fso.openTextFile(path,1)
  set excSheet = CreateObject("Excel.Application")
  excSheet.workbooks.add
  excLine = 1
  do until file.atEndOfStream
    arr =  Split(file.ReadLine,"=")
    if arr(0) = "TABAKA" then
      excLine = excLine + 1
      excSheet.cells(excLine,1)= arr(1)
    end if
  loop
  excSheet.visible = True
  set fso = nothing
  set file = nothing
end sub



' Yazan : Harita M�h. O�uzalp Bozkurt
' Tarih : 2013
' A��klama : 

Sub Main
Dim i,BD

 with Netcad

 set BD = Netcad.NewBDialog("Yaz�")

    BD.Getstring "item","Yaz�","" ,50

    if BD.showmodal then

     for i= 0 to .NumLayers-1

     with nclayermanager

          .Layer(i).name = BD.ValueByName("item")&.Layer(i).name

     end with


     next
     end if

  end with

End Sub

Sub Main
Dim i
   with NCLayerManager

     .Add "ALAN_A,C...",3
     .Add "ALAN_B", 32
     '.Add "ALAN_C", 3
     .Add "ALAN_GECICI", 64
     .Add "ALAN_GECICI_NOK", 64
     .Add "ALAN_TH", 5
     .Add "ALAN_USTHAKKI", 48
     .Add "ALAN_USTHAKKI_NOK", 48
     .Add "@ALAN", 2
    
    
    
       
  end with
End Sub

Sub Main
Dim i
   with NCLayerManager

     .Add "@GUZERGAH", 3
     .Add "GUZERGAH_GECICI", 2
     .Add "GUZERGAH_DAIMI", 3
     .Add "GUZERGAH_SOME", 3
     .Add "GUZERGAH_KM", 3
     .Add "PARAMETRE", 5
     .Add "GUZERGAH_EKSEN", 14
    
    
       
  end with
End Sub


' Yazan : Erc�ment KORKMAZ
' Tarih : 29.01.2007
' A��klama : TAKS KAKS HESABI

Sub Main
on error resume next
Dim i, o , tbk , tbk1, tbk3, p, by, pln, pln1 , barea, txt , kat, aaln, say, taks, kaks
dim BD, ytbk, ytbk1, ytbk3, taksy, kaksy
set BD = Netcad.NewBDialog("Hello Dialog")
BD.PutPrompt  "Gereken Tabaka Adlar�n� Giriniz...  !"
BD.GetString  "item","Ada Tabakas�", "KAD_ADA_PL", 15
BD.GetString  "item1","Bina Tabakas� ", "KONUT", 15
BD.GetString  "item3","Katadedi Tabakas�", "YAZI_KAT", 15
if BD.showmodal then
ytbk =  BD.ValueByName("item")
ytbk1 =  BD.ValueByName("item1")
ytbk3 =  BD.ValueByName("item3")
end if
barea = 0
kat = 0
aaln = 0
say =0
taks = 0
kaks= 0
with Netcad
tbk =  .FoundLayer(ytbk)
tbk1 = .FoundLayer(ytbk1)
tbk3 = .FoundLayer(ytbk3)
.CreateLayer "TAKS", 5
.CreateLayer "KAKS", 5
set taksy = .NewObject
set kaksy = .NewObject
.SetFilter nothing, array (tbk) , array (opline)
do
set o = .GetNextObject
i =  .CurObjPos
if o  is nothing then
exit do
end if
aaln = o.area
.SetFilter .ObjectExtends(o), array (tbk1), array (opline)
Do
set by = .GetNextObject
if by is nothing then
exit do
else
set pln=.GetPlineExt(o)
set pln1= .GetPlineExt(by)
if pln.InPoly(pln1.CenterOfMass) then
.DrawObject o, RED
barea = barea + by.area
end if
end if
Loop
.resetfilter
.SetFilter .ObjectExtends(o), array (tbk3), array (otext)
Do
set txt = .GetNextObject
if txt is nothing then
exit do
else
if pln.InPoly(txt.p1) then
kat = kat + txt.s
say = say +1
end if
end if
Loop
.resetfilter
taks =  round(barea / aaln , 2)
if kat > 0 then kaks = round(taks * (kat/say),2)
.addobject .MakeText (pln.CenterOfMass, taks, 0,0, 2,0,0,.foundlayer("TAKS"))
.addobject .MakeText (pln.CenterOfMass, Kaks, 0,0, 2,0,0,.foundlayer("KAKS"))
barea = 0
kat = 0
say =0
taks = 0
kaks= 0
loop
.ResetFilter
end with
End Sub

'yaz�lar�n etraf�na pline �izer

Sub Main
Dim o, texto

 with Netcad
    set texto=.Newobject

        .setfilter nothing, array(), array(otext) ' yaz�lar� secen filtre
    while .GetNextobject2(texto)      

       	  set o=.MakePline(texto.objname, 1, 0, 0, 0, 1, texto.limits.GetAsPline)
          o.cls=texto.cls
          o.Objname=texto.objname
          .Addobject(o)

    wend
          set o=nothing
          set texto=nothing
          .resetfilter
  end with
End Sub


' Yazan : 
' Tarih : 26.2.2014
' A��klama : 

Sub Main
Dim obj
with Netcad
.SetFilter nothing, array(), array(opline)
do

set obj=.getnextobject
if obj is nothing then
exit do
end if
.drawobject obj,102
obj.tarea=0
.PUTOBJECT .CUROBJPOS,OBJ

loop
end with
End Sub

' Dil   	: Visual Basic
' Ama�  	: Alan adlar�n� h�zl� bir bi�imde de�i�tirmek.
' Yazan 	: O�uzalp Bozkurt 2012
Sub Main
    Dim a,b,c,m,alan,o,d,e

    with netcad
         set alan = .NewSelectStatus

         while .SelectObjectInstant(" alan� se� ?",1,array(opline),alan)
               
               set o = alan.objects(0)
               m=.getparam(94)
               a=o.tarea
               b=o.area
               d=abs(a-b)
               c=0.0004*m*sqr(a)+0.0003*a
               e = c - d

               if abs(a-b)> c and a > b  then

               msgbox "TECV�Z DI�INDA"&chr(13)&round(abs(e),2)&" m� ARTACAK",16

               elseif abs(a-b)> c and b > a then


               msgbox "TECV�Z DI�INDA"&chr(13)&  round(e,2)&" m� AZALACAK",16

               else

               msgbox "TECV�Z ���NDE",64


                 end if

         wend

  end with
end sub

' Dil   	: Visual Basic
' Ama�  	: Alan adlar�n� h�zl� bir bi�imde de�i�tirmek.
' Yazan 	: O�uzalp Bozkurt 2012
Sub Main
    Dim a,b,c,m,alan,o,d,e,poly,s,y

    with netcad

           .setfilter nothing, array(),array(opline)
        do
           set o=.getnextobject

           if o is nothing then
              exit do
           else
             end if
               
                set poly=.getplineext(o)
               m=.getparam(94)
               a=o.tarea
               b=o.area
               d=abs(a-b)
               c=0.0004*m*sqr(a)+0.0003*a
               e = c - d

    if abs(a-b)> c and a > b  then


                   set  y=.MakeText(poly.centerofmass,round(abs(e),2)&" m�", 0,0,5,0,0,.createlayer("tecviz",1))

                  .addobject y


elseif abs(a-b)> c and b > a then
		 

                   set  y=.MakeText(poly.centerofmass, round(e,2)&" m�", 0,0, 5,0,0,.createlayer("tecviz",1))

                  .addobject y

                else

                 end if




      loop

  end with
end sub

'TARIH       : 29.12.2004
'AMA�        : Servis y�netici kullan�larak makro ile tematik haz�rlamak

'GEREKLILIKLER : - Referans Y�neticisi,Lejant Bilgileri penceresinde
'                  bulunan, kaydet ile kaydedilmi� xml dosya
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
Sub main
dim txt,fSt,s
  set fSt = CreateObject("Scripting.FileSystemObject")
  Set txt = fst.openTextFile("F:\ISTANBUL\GENEL\deneme�stanbul.xml", 1, false)
  s = txt.readall 'xml dosyan�n t�m i�eri�i okunur ve bir de�i�kene atan�r
  txt.close       'File System Objesi kapat�l�r.

  ncxmlserviceman.runxml "REFMAN.THEMATIC",s,""  'Refman.Thematic servisi �a�r�l�r
  ncxmlserviceman.runxml "NETCAD.GETMAP","<getmap></getmap>","" 'Netcad.Getmap servisi ile
                                                                'harita g�ncellenir
End Sub


' Yazan : 
' Tarih : 3.6.2014
' A��klama : 

Sub Main
Dim i
  with Netcad
dim obj
dim layer
dim txt
dim pos
dim limit

dim a




  set limit=.newworld(0,0,0,0)

  if .GetRegion(limit) then







   .setfilter limit,array(),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if





       if obj.s="HesapAlan" then

              dene obj

        end if

        if obj.s="TAPU ALANI" then

              dene1 obj

        end if

          if obj.s="Tapu Alan" then

              dene2 obj

        end if


       loop
   .resetfilter






.setfilter limit,array(),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if





       if obj.s="Tapu Alan" then

              obj.s="HesapAlan"
                  .PUTOBJECT .CUROBJPOS,OBJ


        end if




       loop
   .resetfilter

           end if


end with

end sub






sub dene(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y,obj.p1.x+5,obj.p1.y+23.5,obj.p1.x-1000)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,4
           obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1





           loop
 .resetfilter
 end with
 end sub



 sub dene1(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y-2,obj.p1.x,obj.p1.y+85,obj.p1.x-10)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,10
           obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1
                 .CLOSELAYER(OBJ1.TABAKA)




           loop
 .resetfilter
 end with
 end sub



  sub dene2(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y-2,obj.p1.x+4,obj.p1.y+15,obj.p1.x-200)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,10

           obj1.p1.y= obj1.p1.y-25.50
           obj1.p2.y= obj1.p2.y-25.50
           'obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1





           loop
 .resetfilter
 end with
 end sub


 




' Yazan : 
' Tarih : 3.6.2014
' A��klama : 

Sub Main
Dim i
  with Netcad
dim obj
dim layer
dim txt
dim pos
dim limit

dim a




  set limit=.newworld(0,0,0,0)

  if .GetRegion(limit) then







   .setfilter limit,array(),array(otext)
       do
         set obj=.getnextobject

         if obj is nothing then
            exit do
         else
            end if





       if obj.s="HesapAlan" then

              dene obj

        end if

        if obj.s="TAPU" then

              dene1 obj

        end if

          if obj.s="Tapu Alan" then

              dene2 obj

        end if


       loop
   .resetfilter

     end if

end with

end sub




sub dene(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y,obj.p1.x+3,obj.p1.y+25,obj.p1.x-1000)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,4
           obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1





           loop
 .resetfilter
 end with
 end sub



 sub dene1(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y-2,obj.p1.x,obj.p1.y+85,obj.p1.x-10)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,10
           obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1
                 .CLOSELAYER(OBJ1.TABAKA)




           loop
 .resetfilter
 end with
 end sub



  sub dene2(obj)
with netcad
dim obj1



   Dim w : set w=.newworld(obj.p1.y-2,obj.p1.x+4,obj.p1.y+15,obj.p1.x-200)
.setfilter w,array(.foundlayer("TXT_ALANLAR"),_
                   .foundlayer("TXT_KOORDINAT")),array(otext,oline)
       do
         set obj1=.getnextobject

         if obj1 is nothing then
            exit do
         else
            end if
            .drawobject obj1,10

           obj1.p1.y= obj1.p1.y-26.13
           obj1.p2.y= obj1.p2.y-26.13
           'obj1.tabaka=.createlayer("sil",5)
            .PUTOBJECT .CUROBJPOS,OBJ1





           loop
 .resetfilter
 end with
 end sub


 




' Yazan : 
' Tarih : 23.7.2014
' A��klama : 

Sub Main
Dim i
dim obj
  with Netcad






   .setfilter nothing, array(.foundlayer("BEY_YAZI")),array(otext)

   do
   SET OBJ=.GETNEXTOBJECT                         
                                                                      
   IF OBJ IS NOTHING THEN
      EXIT DO                                                         
   ELSE

   end if

   if obj.s="Cinsi" then
          .drawobject obj,2
        yaz obj
                                                                      
                                                                      
   end if

   loop
   .resetfilter
                  
     end with
      END SUB     
                  



sub yaz(obj)
      with Netcad 
       dim obj20
       dim yaz1,yaz2,yaz3
       dim c

              dim w: set w=.newworld(obj.p1.y,obj.p1.x,obj.p1.y+16,obj.p1.x-16)

              .setfilter w, array(.foundlayer("LEJANT_CINSI")),array(otext)
              DO                                                                                                           
              SET OBJ20=.GETNEXTOBJECT                                                                                     
                                                                                                                           
              IF OBJ20 IS NOTHING THEN                                                                                     
                 EXIT DO                                                                                                   
              ELSE                                                                                                         
                                                                                                                           
                                                                                                                           
              end if                                                                                                       
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
              if obj20.s="YOL" or obj20.s="DERE" or obj20.s="HAM TOPRAK"   then                                            

                                                                                                                           
              set c = obj20.p1                                                                                             
                                                                                                                           
              c.x=c.x-128.50                                                                                               
              c.y=c.y-74.85                                                                                                
                                                                                                                           
               set yaz1=.maketext(c, "''4586 SAYILI PETROL�N BORU HATTI �LE TRANS�T",0,0,2,0,"1",.foundlayer("BEY_YAZI"))  
               c.x=c.x-5                                                                                                   
               set yaz2=.maketext(c, "GE����NE DA�R KANUNUN 8/e MADDES�NE G�RE",0,0,2,0,"1",.foundlayer("BEY_YAZI"))       
               c.x=c.x-5                                                                                                   
               set yaz3=.maketext(c, "D�ZENLENM��T�R.'' ",0,0,2,0,"1",.foundlayer("BEY_YAZI"))                             
               c.x=c.x-5                                                                                                   
               .addobject(yaz1)                                                                                            
               .addobject(yaz2)                                                                                            
               .addobject(yaz3)
               end if


              if obj20.s="MERA"   then

                                                                                                                           
              set c = obj20.p1                                                                                             
                                                                                                                           
              c.x=c.x-128.50                                                                                               
              c.y=c.y-74.85                                                                                                
                                                                                                                           
               set yaz1=.maketext(c, "''4586 SAYILI PETROL�N BORU HATTI �LE TRANS�T",0,0,2,0,"1",.foundlayer("BEY_YAZI"))  
               c.x=c.x-5                                                                                                   
               set yaz2=.maketext(c, "GE����NE DA�R KANUNUN 8/f MADDES�NE G�RE",0,0,2,0,"1",.foundlayer("BEY_YAZI"))
               c.x=c.x-5                                                                                                   
               set yaz3=.maketext(c, "D�ZENLENM��T�R.'' ",0,0,2,0,"1",.foundlayer("BEY_YAZI"))                             
               c.x=c.x-5                                                                                                   
               .addobject(yaz1)                                                                                            
               .addobject(yaz2)                                                                                            
               .addobject(yaz3)
               end if







              loop

            .resetfilter

            end with
 end sub









' Yazan : 
' Tarih : 11.06.2012
' A��klama : 

Sub Main
Dim Top
Dim obj
dim a
dim obj1

  with Netcad

    .setfilter nothing, array(.foundlayer("BEY_YAZI")),array(otext)
        do
           set obj1=.getnextobject

           if obj1 is nothing then
              exit do
           else
           end if



       if obj1.s="Parsel" then


         yaz obj1
         yaz1 obj1
         yaz2 obj1
         yaz3 obj1

       end if







        loop
 .resetfilter



   .setfilter nothing, array(.foundlayer("KR_B_PARNO")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if



                if obj.s >10000 and  obj.s <10100 then
                 a= obj.s mod 100
                 obj.s= "YOL"& a &"("& obj.s&")"
                   obj.tabaka=.createlayer("KR_B_PARNO_1",0)
                .putobject .curobjpos,obj

      end if







        loop
 .resetfilter





    .setfilter nothing, array(.foundlayer("KR_B_PARNO")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if





                 if obj.s >20000 and obj.s <20100 then
                 a= obj.s mod 100
                 obj.s= "DERE"& a &"("& obj.s&")"
                  obj.tabaka=.createlayer("KR_B_PARNO_1",0)
                .putobject .curobjpos,obj

                end if









        loop
 .resetfilter




    .setfilter nothing, array(.foundlayer("KR_B_PARNO")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if




                if obj.s >30000 and obj.s <30100 then
                 a= obj.s mod 100
                 obj.s= "TH"& a &"("& obj.s&")"
                 obj.tabaka=.createlayer("KR_B_PARNO_1",0)
                .putobject .curobjpos,obj

                end if






        loop
 .resetfilter



    .setfilter nothing, array(.foundlayer("KR_B_PARNO")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if




                if obj.s >40000 and obj.s <40100 then
                 a= obj.s mod 100
                 obj.s= "ORMAN"& a &"("& obj.s&")"
                 obj.tabaka=.createlayer("KR_B_PARNO_1",0)
                .putobject .curobjpos,obj

                end if






        loop
 .resetfilter



   .setfilter nothing, array(.foundlayer("PAR_NO_PARSEL")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if



                if obj.s >10000 and  obj.s <10100 then
                 a= obj.s mod 100
                 obj.s= "YOL"& a &"("& obj.s&")"
                  obj.tabaka=.createlayer("PAR_NO_PARSEL_1",0)
                .putobject .curobjpos,obj

      end if







        loop
 .resetfilter




    .setfilter nothing, array(.foundlayer("PAR_NO_PARSEL")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if





                    if obj.s >20000 and obj.s <20100 then
                 a= obj.s mod 100
                 obj.s= "DERE"& a &"("& obj.s&")"
                   obj.tabaka=.createlayer("PAR_NO_PARSEL_1",0)
                .putobject .curobjpos,obj

                end if










        loop
 .resetfilter




    .setfilter nothing, array(.foundlayer("PAR_NO_PARSEL")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if







                if obj.s >30000 and obj.s <30100 then
                 a= obj.s mod 100
                 obj.s= "TH"& a &"("& obj.s&")"
                   obj.tabaka=.createlayer("PAR_NO_PARSEL_1",0)
                .putobject .curobjpos,obj

                end if







        loop
 .resetfilter



   .setfilter nothing, array(.foundlayer("PAR_NO_PARSEL")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if







                if obj.s >40000 and obj.s <40100 then
                 a= obj.s mod 100
                 obj.s= "ORMAN"& a &"("& obj.s&")"
                   obj.tabaka=.createlayer("PAR_NO_PARSEL_1",0)
                .putobject .curobjpos,obj

                end if







        loop
 .resetfilter









  end with
End Sub




Sub YAZ(obj1)
Dim Top
Dim obj
dim b
dim a

  with Netcad
           Dim w : set w=.newworld(obj1.p1.y-3,obj1.p1.x,obj1.p1.y+12,obj1.p1.x-15.30)
    .setfilter w, array(.foundlayer("LEJANT_ADAPAR")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if



                if obj.s>10000 and obj.s <10030 then
                 a= obj.s mod 100
                 obj.s= "YOL"& a &"("& obj.s&")"

                   obj.tabaka=.createlayer("LEJANT_ADAPAR_1",0)
                   obj.p1.y=obj.p1.y-2
                   obj.wsc=0.80

                  end if

                     .putobject .curobjpos,obj


        loop

 .resetfilter

 end with
 end sub



 Sub YAZ1(obj1)
Dim Top
Dim obj
dim b
dim a

  with Netcad
           Dim w : set w=.newworld(obj1.p1.y-3,obj1.p1.x,obj1.p1.y+12,obj1.p1.x-15.30)
    .setfilter w, array(.foundlayer("LEJANT_ADAPAR")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if





                if obj.s >20000 and obj.s <20030 then
                 a= obj.s mod 100
                 obj.s= "DERE"& a &"("& obj.s&")"
                    obj.tabaka=.createlayer("LEJANT_ADAPAR_1",0)
                       obj.p1.y=obj.p1.y-2
                   obj.wsc=0.80
                 .putobject .curobjpos,obj


                end if



        loop

 .resetfilter

 end with
 end sub


Sub YAZ2(obj1)
Dim Top
Dim obj
dim b
dim a

  with Netcad
           Dim w : set w=.newworld(obj1.p1.y-3,obj1.p1.x,obj1.p1.y+12,obj1.p1.x-15.30)
    .setfilter w, array(.foundlayer("LEJANT_ADAPAR")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if



                if obj.s >30000 and obj.s <30100 then
                 a= obj.s mod 100
                 obj.s= "TH"& a &"("& obj.s&")"
                     obj.tabaka=.createlayer("LEJANT_ADAPAR_1",0)

                      obj.p1.y=obj.p1.y-2
                   obj.wsc=0.80
                 .putobject .curobjpos,obj


                end if

        loop

 .resetfilter

 end with
 end sub



 Sub YAZ3(obj1)
Dim Top
Dim obj
dim b
dim a

  with Netcad
           Dim w : set w=.newworld(obj1.p1.y-3,obj1.p1.x,obj1.p1.y+12,obj1.p1.x-15.30)
    .setfilter w, array(.foundlayer("LEJANT_ADAPAR")),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
           end if



                if obj.s >40000 and obj.s <40100 then
                 a= obj.s mod 100
                 obj.s= "ORMAN"& a &"("& obj.s&")"
                     obj.tabaka=.createlayer("LEJANT_ADAPAR_1",0)

                      obj.p1.y=obj.p1.y-2
                   obj.wsc=0.80
                 .putobject .curobjpos,obj


                end if

        loop

 .resetfilter

 end with
 end sub

' Macro Yazar� : Harita M�h. O�uzalp BOZKURT
' Ama� : Beyannamelere not yazma
sub main
dim secim,c,layerno,obj
dim yaz1
dim yaz2
dim yaz3
dim yaz4
dim yazi
dim top
dim o

with netcad

'set secim = .NewSelectStatus()

set c = .newc(0,0,0)

layerno=.foundlayer("BEY_YAZI")





 while.SelectPoint("Nokta se�", c, 2)

   set yaz1=.maketext(c, "''4586 SAYILI PETROL�N BORU HATTI �LE TRANS�T",0,0,2,0,"1",layerno)
   c.x=c.x-5
   set yaz2=.maketext(c, "GE����NE DA�R KANUNUN 8/e MADDES�NE G�RE",0,0,2,0,"1",layerno)
   c.x=c.x-5
    set yaz3=.maketext(c, "D�ZENLENM��T�R.'' ",0,0,2,0,"1",layerno)
   c.x=c.x-5
   .addobject(yaz1)
   .addobject(yaz2)
    .addobject(yaz3)
set o = nothing



wend
end with
end sub

'
' Dil  : Visual Basic
' Ama� : 1- Bir alt procedure cagrilmasi
'        2- Projedeki coklu dogrularin verilen mesafe kadar kaydirilmasi
'
Sub MovePoly (MoveDistY, MoveDistX)
Dim i,j,o,p
   with Netcad
      for i = 0 to .numobject-1       ' projedeki tum objeleri sirayla tara
        set o = .getobject(i)         ' i. objeyi al
        if o.tag = opline then        ' Coklu dogrumu ?
          .drawobject o, Black        ' objeyi silinmis gibi siyahla ciz
          set p = .getplineext(o)     ' objenin noktalarini tutan pline yapisini al
          for j = 0 to p.num-1        ' coklu dogrunun herbir noktasi icin
            p.cor(j).y = p.cor(j).y + MoveDistY     ' j. noktayi kaydir
            p.cor(j).x = p.cor(j).x + MoveDistX
          next
          .putplineext o,p            ' noktalari yenile
          .putobject i,o              ' objeyi yenile
          .drawobject o, Red          ' objeyi kirmizi renkle ciz
          set p = nothing             ' noktalar icin aldigimiz memory'i geri ver
        end if
        set o = nothing               ' obje icin aldigimiz memory'i geri ver
      next
   end with
End Sub

Sub Main
  ' Program buradan basliyor.
  ' Cokludogrulari 10 metre saga 10 metre sola kaydir
  MovePoly 10,10
End Sub

'
' Dil  : Visual Basic
' Ama� : Filitre yontemi ile aktif projedeki istenen objeleri
'        taramak ve objelerin turlerini mesajla gostermek
'
Sub Main
Dim o
  with Netcad
    .SetFilter nothing, array(), array(opoint,oline,opline,oshape)  'Filitre uygula
    Do
      set o = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
      if o is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
        exit do                ' Bu durumda donguyu durdur
      else                     ' Yoksa obje gelmistir
        msgbox ObjectNames(o.tag) & " = " & .CurObjPos  ' Ekrana mesaj olarak turunu
      end if                                            ' ve indeksini goster
      set o = nothing              ' obje icin alinan memoryi geri ver
    Loop
    .ResetFilter                   ' Filitre uygulamayi birak.
  end with
end sub

'
' Dil  : Visual Basic
' Ama� : SetFilter, ResetFilter, Newobject, GetNextObject2 nin kullanimi
'
Sub Main
Dim o
  with Netcad
    ' Tum Proje alani, Tum Tabakalar ve (nokta,dogru,cokdogru,sekil)
    ' Filitresi uygula.
    .SetFilter nothing,array(),array(opoint,oline,opline,oshape)
    set o = .Newobject           ' Gecici obje yarat
    while .GetNextObject2(o)     ' Eger yukardaki kurala uyan obje varsa
                                 ' onu bizim objenin icine getir
      msgbox objectnames(o.tag)  ' gelen objenin turunu goster
    wend
    set o = nothing              ' Gecici objeyi hafizadan sil
    .ResetFilter                 ' Filitre Uygulama.
  end with
End Sub


'
' Dil  : Visual Basic
' Ama� : Ekrandan Nokta Se�mek.
'        Newc ve SelectPoint kullanim ornegi
'
Sub Main
Dim c
  with Netcad
    set c = .newc(0,0,0)    ' Gecici nokta yarat
    while .SelectPoint("Nokta i�in bir Yer G�ster",c,-1)  'Nokta sectir
      msgbox c.y            ' Secilen nokta bizim c objesinin icine geldi.
                            ' ekranda mesaj olarak Y koordinatini goster.
    wend                    ' Kullanici iptal edene kadar devam et.
    set c = nothing         ' Gecici noktayi yok et.
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Ekranda Baslangici Ayni olan Cizgiler Yurutmek ve
'        Bunlari Projeye Eklemek.
'        Not : WalkLine, MakeLine, AddObject kullanim ornegi.
Sub Main
Dim c1,c2,o
  with Netcad
    set c1 = .newc(0,0,0)   ' iki tane gecici nokta yarat
    set c2 = .newc(0,0,0)
    if .SelectPoint("Ba�lang�� Noktas� se�",c1,-1) then     ' Baslangici sectir
      while .WalkLine("2. Noktay� se�", c2,-1,c1)  ' Cizgileri bu baslangica gore yurut
        set o= .MakeLine(c1,c2,0,0,0)           ' her bir secilen c2 icin Cizgi objesi yarat
        .AddObject(o)                           ' ve projeye ekle
        set o= nothing                          ' gecici objeyi hafizadan sil
      wend
    end if
    set c2 = nothing        ' gecici noktalari sil
    set c1 = nothing
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Sembol Yaratip, ekranda Gezdirerek Projeye eklemek
'        MakeSembol, WalkObject kullanimi.
Sub Main
Dim c,o
  with Netcad
    set c = .newc(0,0,0)
    set o = .MakeSembol(c,155,0,2,0,0)                ' 155 nolu semboldan yarat
    while .WalkObject("Sembol i�in Yer Se�",c,-1,o)   ' Sembolu Ekranda yurut
      o.p1 = c                                        ' Gelen koordinati al
      .AddObject(o)                                   ' projeye ekle
    wend                                              ' iptal edilene kadar
    set c = nothing
    set o = nothing
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Objeleri Tek Tek secmek.
'        NewSelectStatus, SelectObjectInstant kullanimi
'
Sub Main
Dim ss,o
  with netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    while .SelectObjectInstant("Dogru objesi Se�",1,array(oline),ss)  'Dogru objesi sec
      set o = ss.objects(0)         ' Secim objesinin ilk objesini al
      o.renk = yellow               ' rengini sari yap
      .PutObject ss.indexs(0), o    ' objeyi geri koy
      .DrawObject o,-1  ' kendi rengi ile ciz
      set o = nothing
    wend
    set ss = nothing
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Secim Kumesi olusturma
'
Sub Main
Dim i,j,o,SEL
  with Netcad
    set SEL = .NewSelectionSet   ' Yeni kume yarat
    set o = .NewObject
    if SEL.SELECT("Dogru ve CokDogru Objelerini Se�",array(opline,oline)) then  ' istenen turleri kumeye ekle
      for i = 0 to SEL.NE-1                ' kumenin her bir elemani icin
        j = SEL.GetSelectedObject(i, o)    ' objeyi ve gercek indeksini al
        o.renk = yellow                    ' rengini sari yap
        .putobject j, o                    ' objeyi geri koy
      next
      SEL.RedrawAndRewind                  ' secim kumesini toplu kendi renginde
    end if                                 ' cizdir ve kumeyi basa sardir.
    set SEL = nothing
    set o = nothing
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Kullanicidan bir dialog dolusu bilgi almak
'        NewBDialog kullanim ornegi.
Sub Main
Dim BD
  set BD = Netcad.NewBDialog("Hello Dialog")                 ' yeni dialog yarat
  BD.PutPrompt  "Bu bir A��klamad�r !"                       ' Aciklama Yazdir.
  BD.GetInteger "item1","Bir Tam Sayi Girin", 1555           ' tam sayi iste. baslangic degeri 1555 olsun
  BD.GetFloat   "item2","Bir Real Sayi Girin", 1555.334, 3   ' real sayi iste. baslangic degeri 1555.334 olsun
  BD.GetString  "item3","Bir Yazi Girin", "Hello", 15        ' Yazi iste. en fazla 15 karakter olsun
  BD.GetCheck   "item4","Bunu i�aretle", 1                        ' CheckBox iste. baslangicta secili olsun
  BD.GetRadio   "item5","Birini Se�","Bir|Iki|Uc|Dort",2     ' Radio Elemanlar. Basta 2. eleman secili.
  BD.GetCombo   "item6","Bunuda Se�","one|two|three|four",1  ' ComboBox iste. Basta 1. eleman secili.
  BD.GetFileName "item7","Dosya Adi Gir","c:\my.bmp","Resim Dosyalari|*.bmp|Tum Dosyalar|*.*","bmp"  ' file filitresi ve default dosya uzantisi ver
  if BD.showmodal then
    MsgBox "item1 = " & BD.ValueByName("item1")      ' kullanicinin girdigi degerleri goster
    MsgBox "item2 = " & BD.ValueByName("item2")
    MsgBox "item3 = " & BD.ValueByName("item3")
    MsgBox "item4 = " & BD.ValueByName("item4")
    MsgBox "item5 = " & BD.ValueByName("item5")
    MsgBox "item6 = " & BD.ValueByName("item6")
    MsgBox "item7 = " & BD.ValueByName("item7")
  end if
  set BD = Nothing
End sub


'
' Dil  : Visual Basic
' Amac : En basit obje yaratimi
'
Sub Main
  with Netcad
    .AddObject .MakeLine(.newc(0,0,0), .newc(100,100,0), .CreateLayer("TABAKA1",yellow), 0, 0)
    .AddObject .MakeCircle(.newc(200,200,0), 50, .CreateLayer("TABAKA2",red), 0, 0)
  end with
End Sub


'
' Dil  : Visual Basic
' Amac : Kapali Alan Secmek
'
Sub Main
Dim p
  with Netcad
    set p = .NewPoly      ' noktalari tutmak icin poly objesi yarat
    if .GetPolygon("Kapal� Alan Se�",p) then  ' bunun icini doldur
      msgbox "Nokta Say�s� = " & p.num        ' nokta sayisini goster, veya bu alani kullan
    end if
    set p = nothing
  end with
End Sub


'
' Dil  : Visual Basic
' Ama� : Netcad Parametlerini degistirmek
'
Sub Main
  with Netcad
    .SetParam PNC_SNAPENDOF, 1          'Son Nokta yakala modunu ac
    .SetParam PNC_MAXPOINTNUM, "P100"   'Max Nokta No.yu set et
    msgbox .getparam(PNC_MAXPOINTNUM)   'Max Nokta No.yu ogren
  end with
End Sub


'
' Dil  : Visual Basic
' Ama� : Modullerdeki COM objelerine ulasim
'
Sub Main
dim foto1
  set foto1 = createobject("Fotocad.FotocadCOM")
    foto1.method1
  set foto1 = nothing
end sub


'
' Dil  : Visual Basic
' Ama� : Modullerdeki COM objelerine ulasim
'
Sub Main
dim foto1
  set foto1 = createobject("TestCOM.MyTestObje")
    'foto1.SayHello
    foto1.SetNetcad(true)
    'foto1.Message("DENEME")  ' bu mesaji netcad soyluyor
    foto1.ShowMyForm
    while foto1.formavail
      foto1.processmessages
    wend
    foto1.SetNetcad(false)
  set foto1 = nothing
end sub


'
' Dil  : Visual Basic
' Ama� : Sbsmodul deki COM objesi Sokak bilgi sisteminde
'        iki nokta arasindaki En Kisayolu bulmak.
' p.s. : Bu Islemden once SBS Server hazir olmali (Baglatida Kurulmali)

Sub Main

 dim Sbs
 dim c1, c2, cn, P, i, o, snapc
 dim CY, CX, s
 dim MARKNO

  set Sbs = createobject("SbsMod.SbsCOM")

  Sbs.StartKisaYolBul      ' Kisayol bulma islemlerini baslat

  with Netcad
    set c1 = .newc(0,0,0)    ' Gecici nokta yarat
    set c2 = .newc(0,0,0)    ' Gecici nokta yarat
    set P = .NewPoly         ' Sonucu tutmak icin poly objesi yarat
    while .SelectPoint("Ba�lang�� Noktas�n� Se�",c1,-1)
       if .SelectPoint("Biti� Noktas�n� Se�",c2,-1) then

          ' Simdi Kisayol buldur
          if Sbs.KisayolBul(C1.Y, C1.X, C2.Y, C2.X) then

             ' sonuclar Sbs objesinde duruyor.
             ' once sonuc Cokludogrunun noktalari alalim
             P.Clear
             for i = 0 to Sbs.PolyNum-1  ' sonuc yol noktasi sayisi
               CY = Sbs.GetPolyCY(i)
               CX = Sbs.GetPolyCX(i)
               set cn = .newc(CY,CX,0)
               P.AddCoor(cn)                 ' P ye ekle
             next
             ' bulunan yola zoom edelim
             P.CalcLimits
             P.Limits.cll.y = P.Limits.cll.y - 20  ' 20 metre kadar sinirlari buyutelim
             P.Limits.cll.x = P.Limits.cll.x - 20  ' 20 metre kadar sinirlari buyutelim
             P.Limits.cur.y = P.Limits.cur.y + 20  ' 20 metre kadar sinirlari buyutelim
             P.Limits.cur.x = P.Limits.cur.x + 20  ' 20 metre kadar sinirlari buyutelim
             .SetCurrentWindow P.Limits, true

             ' simdi sonuc P yi Cokludogru olarak Projeye ekle
             set o= .MakePline("KISAYOL", 0, 0, 0, 0, 0, P)  ' 0. tabakaya ekle
             o.renk =  Red                                   ' rengi kirmizi
             .AddObject(o)
             set o= nothing

             ' Simdide Adres bilgilerini Alalim
             ' Place Komutu ile aldigimiz Adres satirlarini Kullaniciya
             ' Ekrandan yer sectirip oraya tasitacaz.
             ' Bunun icin Netcad objelerinin sonunu simdiden saklamamiz gerek

             if Sbs.AdresNum > 0 then

               MARKNO = .NumObject

               ' Simdi Sbs nin buldugu son adres satirlarini isteyelim

               set cn = .newc(0,0,0)        ' adres yazdirmak icin gecici baslangic
               for i = 0 to Sbs.AdresNum-1  ' sonuc adres bilgisi sayisi
                 s = Sbs.GetAdresStr(i)    ' i.ci adres satirini al
                 set o = .MakeText(cn, s, 0, 0, 2, 0, "L", 0)
                 .AddObject(o)
                 set o= nothing
                 cn.x = cn.x - 2*1.5      ' bir alt satira git
               next

               set snapc = .newc(0,0,0)

               .Place "Yaz�lar i�in Yer g�sterin", snapc, MARKNO
             end if  ' adres bilgisi var

          end if

       end if                ' of bitis sec
    wend                     ' of baslangic sec

    set c1 = nothing         ' Gecici noktayi yok et.
    set c2 = nothing         ' Gecici noktayi yok et.
    set P = nothing
  end with

  Sbs.StopKisaYolBul       ' Kisayol bulma islemlerini bitir

  set Sbs = nothing
end sub


'
' Dil  : Visual Basic
' Ama� : SetFilter, ResetFilter, Newobject, GetNextObject2 kullanimi
'        Secilen bolgedeki ve tabakadaki pline objelerini bulmak
' Macro Versiyon : 1.2r4

Sub Main
Dim o, w, layer1, layer2
  with Netcad
    layer1 = .CreateLayer("TESTLAYER1", yellow)
    layer2 = .CreateLayer("TESTLAYER2", blue)
    set w = .newworld(0,0,0,0)
    if .GetRegion(w) then
        set o = .Newobject           ' Gecici obje yarat
        .SetFilter w, array(layer1, layer2), array(opline)
        while .GetNextObject2(o)       ' Eger yukardaki kurala uyan obje varsa
                                       ' onu bizim objenin icine doldur
           msgbox o.pname    ' gelen objenin adini goster
        wend
        set o = nothing              ' Gecici objeyi hafizadan sil
        .ResetFilter                 ' Filitre Uygulama.
    end if
    set w = nothing
  end with
End Sub


'
' Dil  : Visual Basic
' Ama� : Secilen Objeleri kesistirmek
'        NewSelectStatus, SelectObjectInstant kullanimi
'
Sub Main
Dim ss,o, o1, o2, ni, i, inta
  with netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    while .SelectObjectInstant("2 tane obje Se�",2,array(oline, opline),ss)  'Dogru objesi sec
      set o1 = ss.objects(0)         ' Secim objesinin ilk objesini al
      set o2 = ss.objects(1)         ' Secim objesinin ikinci objesini al
      set inta = .newpoly
      ni = .IntersectObjects(o1, o2, inta)
      if ni > 0 then
        for i = 0 to ni-1
          set o = .MakeSembol(inta.cor(i), 5, 0, 2, 0, 0)
          .AddObject(o)
          set o = nothing
        next
      end if
      set inta = nothing
      set o2 = nothing
      set o1 = nothing
    wend
    set ss = nothing
  end with
end sub



' Bu ornek Secilen kapali cokludogru objesinin icine dusen
' objeleri secer.


' BU RUTIN CIZGININ ETRAFINDAKI YAZILARI BULUR
sub DoSomething (oi, lineo)
Dim texto
  ' Bu rutin bir cizginin etrafindaki yazilari secer
  with netcad
    ' burda cizginin limitlerini biraz genisletmek mumkun
    ' ornegin 50 metre buyutelim
    ' boylece civardaki yazilarida yakalamak mumkun
    lineo.limits.cll.x = lineo.limits.cll.x - 50
    lineo.limits.cll.y = lineo.limits.cll.y - 50
    lineo.limits.cur.x = lineo.limits.cur.x + 50
    lineo.limits.cur.x = lineo.limits.cur.x + 50
    set texto = .Newobject              ' Gecici obje yarat
    .SetFilter lineo.limits, array(), array(otext)  'Filitre uygula
    while .GetNextObject2(texto)     ' Eger yukardaki kurala uyan obje varsa
                                     ' onu bizim objenin icine getir

       ' Burada Cizgiye En yakin yaziyi bulmak icin birseyler yapabiliriz.
       .DrawObject texto, red   ' Emin olmak icin farkli bir renkle ciz, bunu
                                ' kullanmayabilirsiniz
    wend
    .ResetFilter  ' Filitre uygulamayi birak.
    set texto = nothing
  end with
end sub


' ASIL PROGRAM BURADAN BASLIYOR
Sub Main
Dim ss, o, okapali
Dim linesel, sembolsel
Dim ni, p, inta, i, oi

  with netcad
    set ss = .NewSelectStatus       ' Anlik Secim objesi yarat
    set o = .Newobject              ' Gecici obje yarat
    set linesel = .NewSelectionSet    ' Cizgileri tutan array
    set sembolsel = .NewSelectionSet  ' Sembolleri tutan array
    set inta = .newpoly             ' obje kesismeleri icin lazim

    if .SelectObjectInstant("Kapali CokluDogru objesi Se�",1,array(opline),ss) then 'obje sec
      set okapali = ss.objects(0)   ' Secim objesinin ilk objesini al,
      set p = .getplineext(okapali) ' pline yapisini al

      ' Simdi filitre uygula ve kapali cokludogrunun icine dusenleri listelere ekle.
      .SetFilter okapali.limits, array(), array(oline,oshape)  'Filitre uygula
      while .GetNextObject2(o)     ' Eger yukardaki kurala uyan obje varsa
                                   ' onu bizim objenin icine getir

        ' Cizgileri linesel adli listeye ekle
        if o.tag = oline then
          ni = .IntersectObjects(okapali, o, inta)
          if (ni = 0) and p.inpoly(o.p1) and p.inpoly(o.p2)  then

            linesel.AddSelection .CurObjPos   ' line listesine ekle
            .DrawObject o, yellow    ' Emin olmak icin farkli bir renkle ciz, bunu
                                     ' kullanmayabilirsiniz
          end if
        end if

        ' sembolleri sembolsel adli listeye ekle
        if o.tag = oshape then
          if p.inpoly(o.p1) then

            sembolsel.addselection .CurObjPos   ' sembol listesine ekle
            .DrawObject o, yellow    ' Emin olmak icin farkli bir renkle ciz, bunu
                                     ' kullanmayabilirsiniz
          end if
        end if

      wend
      .ResetFilter                 ' Filitre uygulamayi birak.

      ' Simdi bu listelerdeki objeleri baska islerde kullanabiliriz
      for i = 0 to linesel.NE-1
        oi = linesel.GetSelectedObject(i, o)  ' hem objeyi hemde gercek obje indexini dondurur
                                              ' her ikiside bi yerlerde lazim olabilir.

        DoSomething oi, o   ' Buraya bu cizgi ile yapilacak bi seyler konabilir.

      next

      '
      ' ... Burada Semboller icinde ayni donguden yapabiliriz.
      ' for i = 0 to sembolsel.NE-1
      ' ....
      ' next
      '

      set p = nothing
      set okapali = nothing
    end if

    ' Ters sirada free etmek memoryde bosluk kalintisi yaratmaz
    set inta = nothing
    set sembolsel = nothing
    set linesel = nothing
    set o = nothing              ' Gecici objeyi hafizadan sil
    set ss = nothing
  end with
end sub


'
' Dil  : Visual Basic
' Ama� : Secilen Coklu Dogrularin Objeleri kesistirmek
'        NewSelectStatus, SelectObjectInstant,
'        poly.Operation kullanim ornegi
'        Ayrica Yeni objeleri BINA tablosuna ekler
'
Sub Main
Dim ss, o, p1, p2, px, i, OC, backref
  with netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    while .SelectObjectInstant("2 tane Kapali coklu dogru objesi Se�",2,array(opline),ss)  'CokluDogru objesi sec
      set p1 = .GetPlineExt(ss.objects(0))
      set p2 = .GetPlineExt(ss.objects(1))
      set OC = p1.Operation(polyintersect, p2)  ' Kesisim islemi
      set o  = .NewObject
      set px = .NewPoly

      ' Sonucta bulunan CokluDogrulari Yeni Obje olarak ekle.
      for i = 0 to OC.NE-1
        backref = OC.GetObject(i, o, px)
        set o = .MakePline("POLYX", POLYCLOSED+POLYFILLED, 0, 0, 0, 0, px)
        o.cls = "BINA"
        if not .AddObjectDB(o, px) then
          msgbox "Obje ve Kayit Eklenmedi ! (Kullanici Veri girisini iptal etti veya Baglanti Yoneticisinde SINIF tanimi yok)"
        end if
      next

      set px = nothing
      set o  = nothing
      set OC = nothing
      set p1 = nothing
      set p2 = nothing
    wend
    set ss = nothing
  end with
end sub


Sub Main
Dim ss,o1,P1,o2,p2,w,o3, oi1
  with netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    set w = .newworld(0,0,0,0)
    while .SelectObjectInstant("KAPALI objesi Se�",1,array(opline),ss)  'Dogru objesi sec
      set o1 = ss.objects(0)         ' Secim objesinin ilk objesini al
      set o2= .NewObject
      set o3=.newobject
          .drawobject o1, red        ' objeyi silinmis gibi siyahla ciz
          oi1 = ss.Indexs(0)
          msgbox  oi1
          set p1 = .getplineext(o1)     ' objenin noktalarini tutan pline yapisini al
           .SetFilter o1.limits, array(), array(opline)
            while .GetNextObject2(o2)
                  if (o2.tag = opline) and NOT(oi1 = .CurObjPos) then
                    .drawobject o2, green        ' objeyi silinmis gibi siyahla ciz
                    msgbox  oi1 & "  " &  .CurObjPos
                    set p2 = .GetPlineExt(o2)
                       if  (p1.inpoly(p2.cor(0))  AND NOT p1.inpoly(p2.cor(p2.num-1))) _
                        OR (p1.inpoly(p2.cor(p2.num-1))  AND NOT p1.inpoly( p2.cor(0)))    then

                         msgbox "Bir ucu icerde"

                         if p1.inpoly(p2.cor(0)) then
                           msgbox "ilk noktasi icerde"

                           'w.BuildRegion p2.cor(p2.num-1), 0.501,0.501
                           'msgbox w.cll.x
                           'msgbox w.cur.x
                           w.cll.x = p2.cor(p2.num-1).x - 1
                           w.cll.y = p2.cor(p2.num-1).y - 1
                           w.cur.x = p2.cor(p2.num-1).x + 1
                           w.cur.y = p2.cor(p2.num-1).y + 1
                           .setfilter  w,array(),array(oshape)
                           while .getnextobject2(o3)
                              msgbox o3.sembolno
                              .drawobject o3,red
                           wend
                          .resetfilter
                          '.SetCurrentWindow o3.limits, true
                         else
                           msgbox "son noktasi icerde"
                        '   w.BuildRegion p2.cor(0), 0.501,0.501
                           w.cll.x = p2.cor(0).x - 1
                           w.cll.y = p2.cor(0).y - 1
                           w.cur.x = p2.cor(0).x + 1
                           w.cur.y = p2.cor(0).y + 1

                           .setfilter  w,array(),array(oshape)
                           while .getnextobject2(o3)
                              msgbox o3.sembolno
                              .drawobject o3,red
                           wend
                          .resetfilter

                         end if
                       end if
                  end if
           wend
          .Resetfilter
          '.PutObject ss.indexs(0), o1    ' objeyi geri koy
          '.DrawObject o1,-1  ' kendi rengi ile ciz
    wend
        set o1 = nothing
        set o2=nothing
        set p2=nothing
        set p1=nothing
        set ss = nothing
  end with
end sub

'
'  Amac : Macro icersinden baska bir macro dosyasini calistirmak.
'         Bunu yaparken macro dosyasina parametreler vermek
'         Ve Onun degistirdigi veya yeni ekledigi parametrelere
'         ulasmak.
'
Sub Main
dim i

  with Netcad
    'set params = .NewParameter   ' parametreleri tutacak obje yarat

    NCparam.SetValue "AA", "X111"  ' parametreler ekle
    NCparam.SetValue "BB", "X222"

    ' Macro dosyasini verilen parametreler ile calistir
    if .RunMacroFile(netcad.getparam(PNC_MODULDIR)+"ncmacro\ornek\test22.nvb") then
      ' Macro calistiktan sonra
      ' Sonuclari ister tek tek oku
      msgbox "AA=" & NCparam.GetValue("AA")
      msgbox "BB=" & NCparam.GetValue("BB")
      msgbox "CC=" & NCparam.GetValue("CC")

      ' Istersen sirayla KEY=VALUE seklinde oku
      for i = 0 to NCparam.NumParam-1
        msgbox "KEY AND VALUES =  " & NCparam.GetKeyAndValue(i)
      next
    end if

    'set params = nothing
  end with
End Sub

'
'  Amac : Bir macro dosyasi bazi parametreler ile cagrildiginda
'         bu parametrelere ulasmak ve gerekirse degistirmek, eklemek
'

Sub Main
  msgbox "HELLO Test22 is here"
  ' Bize gonderilen parametreleri NCParam objesi ile ogrenebiliriz
  msgbox "Given AA = " & NCParam.GetValue("AA")
  msgbox "Given BB = " & NCParam.GetValue("BB")

  ' Istersek yeni parametreler ekleyebiliriz.
  ' Eger Parametre Listede varsa degeri yenilenir, yoksa yeni eklenir.
  with NCParam
    .SetValue "AA", 5
    .SetValue "BB", "NEW222"
    .SetValue "CC", "NEW333"
    .SetValue "DD", "NEW444"
  end with
  msgbox "Test22 Finished"
End Sub

'
'  Amac : Netcad Aktif Proje Window.unun bir kismini imaj olarak
'         bir dosyaya saklamak.
'

Sub Main
dim w,h
  with Netcad
    w = .CurrentWinWdPixel
    h = .CurrentWinHtPixel
    if .SaveCurWinImage(w/2-75, w/2+75, h/2-50, h/2+50, IMAGE_JPG, _
       "C:\TEST.JPG", 95) then
      msgbox "Saklandi"
    end if
  end with
End Sub

'
' Dil  : Visual Basic
' Ama� : Yazi Objelerinin Yazisini VtKoduna cevirmek
'
Sub Main
Dim ss,o
  with netcad
    set ss = .NewSelectStatus   ' Anlik Secim objesi yarat
    while .SelectObjectInstant("Yaz� objesi Se�",1,array(otext),ss)  'obje sec
      set o = ss.objects(0)         ' Secim objesinin ilk objesini al
      .DrawObject o,0   ' silinmis gibi ciz
      o.s = o.objname   ' VT Kodunu yazisina ver
      .PutObject ss.indexs(0), o    ' objeyi geri koy
      .DrawObject o,-1  ' kendi rengi ile ciz
      set o = nothing
    wend
    set ss = nothing
  end with
end sub


'TIFTOECW makrosu
'A��klama : Makro yaln�zca ECW Compressor progmar� ile birlikte �al���r.
'03/02/2005
'Bar�� G�ral
'Netcad
sub main
dim dlg,inpath,outpath,app,dlgg
  with netcad
    set dlg = .newbdialog("TIFF > ECW")
    dlg.GetFileName "file","Dosya Yeri","C:\Program Files\Earth Resource Mapping\ECW Compressor 2.6\Bin\ecw_compress_free.exe","EXE Dosyalar�|*.exe|Tum Dosyalar|*.*","exe"  ' file filitresi ve default dosya uzantisi ver
    if dlg.showmodal then
      if dlg.Valuebyname("file") <> "" then
        app = dlg.Valuebyname("file")
      else
        msgbox "Makrodan ��kt�n�z !!!"
        exit sub
      end if
    end if
    set dlg = nothing
    inpath = openDirDialog("D�n��t�r�lecek Dosyalar�n Yeri")
    if inpath <> "" then
      outpath = openDirDialog("Saklanacak Dosyalar�n Yeri")
      if outpath <> "" then
      set dlgg = .newbdialog("TIFF > ECW")
        dlgg.GetString "app", "ECWCOmpressor", app, 250
        dlgg.getstring "inPath","Giri� Klas�r�",inPath&"\",250
        dlgg.getString "outPath","��k�� Dosyalar�",outpath&"\",250
        if dlgg.showmodal then
          ConvertFiles dlgg.ValueByName("inpath"),dlgg.ValueByName("outpath"),dlgg.ValueByName("app")
        end if
        set dlgg = nothing
      else
        msgbox "Makrodan ��kt�n�z !!! Saklanacak Dosyalar i�in bir yer se�iniz."
      end if
    else
      msgbox "Makrodan ��kt�n�z !!! D�n��t�r�lecek Dosyalar�n Yerini se�iniz"
    end if
  end with
end sub

function openDirDialog(t)
dim shell,folder,filePath
  Set shell = CreateObject("Shell.Application")
  Set folder = shell.BrowseForFolder(&H0,t,&H4031,&H0011)
  on error resume next
  openDirDialog = folder.ParentFolder.ParseName(folder.title).Path
end function


sub ConvertFiles(inpath,outpath,app)
dim shell,path,fname,fpath
dim dir,fSo,Count,file,inFile,outfileL,outfile
  set shell = CreateObject("WScript.Shell")
  set fSo = CreateObject("Scripting.FileSystemObject")
  set dir = fso.GetFolder(InPath)
  set Count = dir.Files
  for each file in Count
    with netcad
      if not .escpressed then
      infile = file.name
      outFileL = split(file.name,".",-1,1)
      outfile = outfileL(0)
      shell.run """" & app & """" & " " & inpath&infile &" -o " & outpath&outfile &" -c 20 -rgb -nowait",1,true
      else
        msgbox "Makro Kullan�c� Taraf�ndan Bitirildi !!!"
      end if
    end with
  Next
  set shell = nothing
  set fso = nothing
end sub




'TARIH       : 30.09.2004
'AMA�        : �stenilen bir klas�rde bulunan dosyalar�n teker teker a��l�p,
              'istenilen d�zenlemelerin yap�lmas� ve dosyalar�n istenilen
              'klas�re kay�t edilmesi.
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
' Sabitler
const InPath  = "G:\PENDIK\MERKEZ\"    'Girdi Dosyalar�n�n yeri
const OutPath = "G:\PENDIK\MERKEZ1\"     'Sonu� Dosyalar�n�n yeri
'const ly = array("0","1","2")
'
'
'Ana Program
const tabaka1 = "BINA_MESKUN"

Sub main
dim dir,fSo,Count,file,inFile
dim ly,b

  'ly = array("A","B","C")                                ' Silinecek Tabakalar
  'b  = 3                                                 ' Tabaka Say�s�




  with netcad
    set fSo = CreateObject("Scripting.FileSystemObject") 'Dosya Sistemi objesi olu�turulur.
    set dir = fso.GetFolder(InPath)                      'Bu Objenin �zelliklerinden GetFolder
                                                         'kullan�larak klas�r bir objeye atan�yor.
    set Count = dir.Files                                'Klas�rdeki dosya listesi al�n�yor
    for each file in Count
      .Loadfile InPath&file.name,fs_bnetcad               'InPath Klas�rdeki dosya file.name isimli
                                                          'dosyalar s�rayla y�kleniyor. Yanda
                                                          'LoadFile komutu ile y�klenecek dosya tipi
                                                          'g�nderiliyor.


      '
      '
      'Hatal� gelen objeler i�in bir tabaka olu�turulup, bu objeler o tabakaya yerle�tirilmektedir.
      'createErrorLayer
      '
      '�stenmeyen tabakalar silinmektedir.
      'deleteLayers
      '
      '0 tabakas�ndaki tan�md��� nesneler olu�turulmu� olan tabakaya aktar�l�yor.
      'removeObjects
      '
      'renkTabaka

      'renkObjects

      'delPrefText

      closeLayers

      'hangeFonts(0)
      'NCZ Dosyas�n�n projeksiyonu ayarlanmaktad�r.
      setProjection
      '
      ' Kullan�lmayan tabakalar, �izgi tipleri, yaz� tipleri , semboller , bloklar,
      ' bilgilendirme men�s� g�r�nt�lenmeden silinmektedir.
      '.NetcadCommand "PROJECT CLEAN 1,1,1,1,1,1"
      
      ' Proje d�zenleme sonras�nda olu�an bozuk objeler silinir.
      'DeleteObjects
      
      .NetcadCommand "PROJECT CLEAN 1,1,1,1,1,1"
      '
      '
      inFile = split(file.name,".",-1,1)
      .SaveToFile OutPath&inFile(0)&".ncz"                ' Yap�lan d�zenleme i�leminden sonra a��lm��
                                                          ' olan proje, Outpath sabitinde belirtilmi�
                                                          ' olan yere saklan�yor.
                                                          '
      .NetcadCommand "CLOSE ACTIVEPROJECT"                ' Aktif Proje Kapat�l�yor
    Next
  end with
end Sub

sub createErrorLayer
  with ncLayerManager     ' Tan�md��� objeler i�in ncLayarManager objesi ile bir tabaka
    .add "TANIMDISI",red  ' olu�turuluyor. Bu tabakaya isim olarak "TANIMDISI" se�ilmi�tir.
  end with                ' Tabaka rengi ise k�rm�z� (red) olarak ayarlanm��t�r.
end sub

sub removeObjects
dim obj,i
  with netcad
    for i = 0 to .numobject-1
      set obj = .getobject(i)                  ' T�m objeler taran�yor.
      if obj.tabaka = 0 then                   ' 0 Tabakas�ndaki objeler s�rayla se�iliyor.
        obj.tabaka = .foundlayer("TANIMDISI")  ' 0 Tabakas�ndaki objelerin tabakalar� "TANIMDISI" olarak
        .putobject i,obj                       ' se�iliyor ve objeler obje listesine yeniden ekleniyor.
      end if
      set obj = nothing                        ' Ge�ici obj nesnesinin i�eri�i haf�zadan siliniyor.
    next
  end with
end sub

sub DeleteLayers1
dim i
'dim tabakalar("1","2","3")
dim dimension
dim t
  with ncLayerManager  ' ncLayerManager nesnesi ile istenmeyen tabakalar mevcut ise siliniyor.
    for i = 0 to .numLayer-1
       t = split(.layer(i).name,"_",-1,1)
       if not ((.layer(i).name = "ENH") or (.layer(i).name = "MEVKI_ADI") or (.layer(i).name = "KAT_ADEDI") or (t(0) = "BINA") or (.layer(i).name = "0" ) or (.layer(i).name = "TANIMDISI")) then
         .layer(i).closelayer(0)
       end if
       next
  end with
end sub

sub closeLayers
dim i,j
 with netcad
    with nclayermanager
      for i = 0 to .numLayer-1
        if .layer(i).name = "PAFTA"  then
          .layer(i).CloseLayer 0
        elseif .layer(i).name ="PAFTA_BILGI" then
          .layer(i).CloseLayer(0)
         elseif .layer(i).name ="PAFTA_K��EKOORD�NAT" then
          .layer(i).CloseLayer(0)
         elseif .layer(i).name ="PFT_K��EKOORD�NAT" then
          .layer(i).CloseLayer(0)
         elseif .layer(i).name ="PINDEX" then
          .layer(i).CloseLayer(0)
           elseif .layer(i).name ="BPAFTA" then
          .layer(i).CloseLayer(0)
           elseif .layer(i).name ="KPAFTA" then
          .layer(i).CloseLayer(0)
        elseif .layer(i).name ="PFT_LEJANT" then
          .layer(i).CloseLayer(0) 
        elseif .layer(i).name ="GRID" then
           .layer(i).CloseLayer(0)
        else
           .layer(i).openLayer
        end if
      next
    end with
  end with
  'end with
end sub

sub changeFonts(i)
dim obj,txt,c
  with netcad
    set obj = .newobject
    .setfilter nothing,array(),array(otext)
    while .getnextobject2(obj)
      set c = ncmath.MidPointOf(obj.limits.cll, obj.limits.cur)
      set txt =  .MakeText(obj.p1, obj.s, obj.flags,i, obj.sc,obj.angle,"M",obj.tabaka)
      set obj = txt
      .putobject .curobjpos,obj
    wend
  end with
end sub

sub renkTabaka
dim LayerNo, i
  with netcad
    for i = 0 to .numlayers-1
      if nclayermanager.layer(i).color = 14 then
        nclayermanager.layer(i).color = 4
      end if
    next
  end with
end sub

sub renkObjects
dim i,o
  with netcad
    for i = 0 to .numobject-1
      set o = .getobject(i)
      if o.renk <> 0 then
        o.renk = 0
        .putobject i,o
      end if
    next
    set o = nothing
  end with
end sub

sub setProjection
dim nProj
  with netcad
    set nProj = .NewProjection           'Aktif dosyan�n projeksiyonunun belirlenmesi i�in
    nProj.ProjectionType   = PR_UTM3     '�ncelikle bir projeksiyon nesnesi olu�turulmal�d�r.
    nProj.Datum            = DATUM_TUR_1 'Projeksiyon parametre de�erleri, kurulum dizininde, NCMACRO
    nProj.Zone             = 30          'klas�r�n�n alt�ndaki NVBASIC dosyas�ndan se�ilmi�tir.
    nProj.setToCurrentProject            'Olu�turulmu� olan projeksiyon aktif projenin projeksiyonu 
  end with                               'olarak set ediliyor.
end sub

sub DeleteObjects
dim obj,i
     with netcad
     for i = 0 to .numobject-1
       set obj = .getobject(i)
       ' tespit edilebilmi� tipteki bozuk objeler taran�r ve silinir.
       ' Yeni tespit edilmi� olanlar buraya eklenmelidir.
       if obj.tag = opline and obj.TABAKA = .foundlayer("E�R�_10M") and obj.length(false) > 500 then 
         .delobject i,obj
       elseif obj.tag = ocircle and obj.TABAKA = .foundlayer("E�R�_1M") then
         .delobject i,obj
       end if
      set obj = nothing
     next
   end with
end sub

sub delPrefText
dim i,j,o
dim refX,refY,refObj
  'j = 0
  with netcad
    set o = .newobject
    set refObj = getRefObj
    refX = getLineMinX(refObj)
    refY = getLineMaxY(refObj)
    .setfilter nothing,array(.foundlayer("0")),array(otext)
    while .getnextobject2(o)
      'j = j +1
      if o.p1.x < refX or o.p1.Y > refY then
        .delobject .curobjpos,o
      end if
    wend
    'msgbox j
  end with
end sub

function getRefObj
dim o
  with netcad
    set o = .newobject
    .setfilter nothing,array(.foundlayer("PAFTA")),array(oline)
    while .getnextobject2(o)
      set getRefObj = o
      exit function
    wend
  end with
end function


function getLineMaxY(o)
  with netcad
    if o.p1.y > o.p2.y then
      getLineMaxY = o.p1.y
    else
      getLineMaxY = o.p2.y
    end if
  end with
end function

function getLineMinX(o)
  with netcad
    if o.p1.x < o.p2.x then
      getLineMinX = o.p1.x
    else
      getLineMinX = o.p2.x
    end if
  end with
end function

sub deleteLayers
dim lyarr,i
   lyarr = createLayerList
   for i = 0 to Ubound(lyArr)-1
     deletelayer(lyArr(i))
   next
end sub

function createLayerList
dim i,arr
  with ncLayerManager
    redim arr(.numlayer)
    for i = 0 to .numlayer-1
       arr(i) = .layer(i).name
    next
  end with
  createLayerList = arr
end function

function deleteLayer(nm)
dim i
    with nclayermanager
    for i = 0 to .numlayer-1
       if isNumeric((.layer(i).name)) or _
          inStr(1,.layer(i).name,"J_") <> 0 or _
          inStr(1,.layer(i).name,"KAD_") <> 0 or _
          .layer(i).name = "KADAST" or _
          .layer(i).name = "TELCIT" and _
          .layer(i).name = nm then
         .Delete i, True
         exit function
       end if
      next
    end with
end function

'TARIH       : 01.10.2004
'AMA�        : �stenilen bir klas�rde bulunan dosyalar�n teker teker a��l�p,
              'istenilen tabakalar�n silinmesi ve belirtilen
              'klas�re kay�t edilmesi.
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
' Sabitler
const InPath   = "d:\MAKRO_TEST\NCZ1\"    'Girdi Dosyalar�n�n yeri
const OutPath  = "d:\MAKRO_TEST\NCZ2\"    'Sonu� Dosyalar�n�n yeri
const delLayer = "TANIMDISI"              'Silinecek Tabakan�n ad�
'
'
'Ana Program
Sub main
dim dir,fSo,Count,file,inFile
  with netcad
    set fSo = CreateObject("Scripting.FileSystemObject") 'Dosya Sistemi objesi olu�turulur.
    set dir = fso.GetFolder(InPath)                      'Bu Objenin �zelliklerinden GetFolder
                                                         'kullan�larak klas�r bir objeye atan�yor.
    set Count = dir.Files                                'Klas�rdeki dosya listesi al�n�yor
    for each file in Count
      .Loadfile InPath&file.name,fs_bnetcad               'InPath Klas�rdeki dosya file.name isimli
                                                          'dosyalar s�rayla y�kleniyor. Yanda
                                                          'LoadFile komutu ile y�klenecek dosya tipi
                                                          'g�nderiliyor.
      '
      'Projelerde istenmeyen tabaka silinmektedir.
      '
      deleteLayers
      '
      inFile = split(file.name,".",-1,1)
      .SaveToFile OutPath&inFile(0)&".ncz"                ' Yap�lan d�zenleme i�leminden sonra a��lm��
                                                          ' olan proje, Outpath sabitinde belirtilmi�
                                                          ' olan yere saklan�yor.
      '
      .NetcadCommand "CLOSE ACTIVEPROJECT"                ' Aktif Proje Kapat�l�yor
    Next
  end with
end Sub

sub DeleteLayers()
  'Tabaka mevcut ise silinir.
  with ncLayerManager
     if .find(delLayer) <> -1 then .delete .find(delLayer)
    wend
   end with
end sub

sub main
dim o,p,t,i,res

const tumYol = "YOL_TUM"
const yolYonuDegisecek = "YOL_YONU_DEGISECEK"

  with netcad
    set o = .newobject()
    set p = .newObject()
    .setFilter nothing, array(.foundLayer(yolYonuDegisecek)),array(opline)
    while .getNextObject2(o)
      .setFilter o.getObjectasPline.limits,array(.FoundLayer(tumYol)),array(opline)
      while .getNextObject2(p)


        if o.getObjectasPline.cor(0).x = p.getObjectasPline.cor(0).x and _
           o.getObjectasPline.cor(0).y = p.getObjectasPline.cor(0).y and _
           o.getObjectasPline.cor(o.getObjectasPline.num-1).x = p.getObjectasPline.cor(p.getObjectasPline.num-1).x and _
           o.getObjectasPline.cor(o.getObjectasPline.num-1).y = p.getObjectasPline.cor(p.getObjectasPline.num-1).y then

           p.getObjectasPline.traverse

           set t = .newpoly()
           for i = p.getobjectaspline.num-1 to 0 step -1
             t.addCoor(p.getobjectaspline.cor(i))
           next
           set res= .MakePline(p.objname, 0, 0, p.tabaka,p.lt, p.thicknes, t)

           .putObject .curobjPos,res

        end if



      wend
      .resetFilter
    wend
    .resetFilter
    end with
end sub

'TAR�H :26/05/2001 V1.00
'AMA� : Ekrandan se�ilen objenin bulundu�u tabaka haricindeki tabakalar kapat�l�r.
'G�RD�LER : NETCAD Objeleri


SUB Main
DIM i,oo
DIM t()

With netcad
  ReDim t(.NumLayers)

  For i=0 to .NumLayers-1        't() ye tabakalar� doldur
    t(i)=i
  Next

  For i=0 to .NumLayers-1        'T�m tabakalar� a�
    .OpenLayer i
  Next

  .SetFilter .GetCurrentWindow, t, array()  'Objeleri ekrana �iz
    Do
      set oo = .GetNextObject
      if oo is nothing then
        exit do
      else
        .drawobject oo, -1
      end if
    Loop
  .FastRedraw
  .ResetFilter

End With
END SUB


' Yazan : 
' Tarih : 15.07.2012
' A��klama : 

Sub Main
Dim i
dim a

  with Netcad
     for i= 0 to .NumLayers-1

     with nclayermanager
          .Layer(i).color = 0

     end with

     next

   .netcadcommand("REGEN")


  end with

End Sub

' AMA�: OBJE UZERINDEKI YAZIYI GIS BAGLANTI ANAHTARINA ATAMAK
' G�RD� : �okludogru ve text
' KULLANIM :�OKLUDOGRU UZERINE YAZDIRILMIS YAZILARI ILGILI OBJENIN GIS BAGLANTI ANAHTARINA Atama
' UYARI : MACRO KULLANIMI ONCESI SORGU DEGISTIR ILE YAZILARIN TUTMA NOKTASI SOL ALT YAPILMALIDIR.
'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("�zerindeki yaz�y� VT Koda Ata")          ' yeni dialog yarat
  BD.GetString  "Parsel","Poligon Tabakas�", "SOKAK_AKS", 15
  BD.GetString  "Yazi","Yaz� Tabakas�", "OKU", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.OnPoly(false, by.p1) then
            .DrawObject Parsel, RED
             parsel.OBJname=by.s
             .putobject i,parsel
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB



'Ama� : Referans y�neticindeki se�ili grid dosyalar�n�n VRML'e d�n��t�r�lerek
'       IE'de g�r�nt�lenmesi. Bu makro'nun do�ru �al��mas� i�in en az IMPEXP.dll
'       versiyon 4.0.055 ve IE'de Cortona VRML plugin gerekmektedir. Grid dosyalar�n�n
'       tamam� ayn� projeksiyon sisteminde olmal�d�r.Doku kaplamada kullan�lacak dosyalar
'       "PNG" ya da "JPG" uzant�l� olmal�d�r.
'       Cortona VRML plugin, http://www.parallelgraphics.com/ adresinden temin edilebilir.
'Yazan : Tarkan Kalyoncuoglu



sub navigate (URL)
dim ie
	set ie = CreateObject("internetexplorer.application")
        ie.Visible=True
        ie.navigate(URL)
end sub


Sub main
dim objFSO, xmlDoc, projType, noerr , vrmlIn, vrmlOut, n, i, rs, dirpath, texture, vrmlDir, hdistance, vdistance, scale, directory, rsCheck, brsCheck, refList, resampling

  refList = NCRefman.GetSupportedReferences(Macro_Support_Grid, TRUE)
  if UBound(reflist) = -1 then
    msgBox("Referans y�neticisinde y�kl� grid dosyas� yok!")
  else
    projType = refList(0).Projection.ProjectionType
    for i = 0 to UBound(refList)
      if projType <> refList(i).Projection.ProjectionType then
        msgBox("Referans y�neticisinde y�kl� gridlerin hepsi ayn� projeksiyon sisteminde olmal�d�r!")
        exit Sub
      end if
    next
    n = InStrRev(refList(0).Name, "\")
    dirpath = Left(refList(0).Name, n - 1)
    set vrmlDir =  Netcad.NewBDialog("Vrml")
    if projType = PR_GEO then
      vrmlDir.GetFloat "item1", "1 Derecenin yatayda ka� metreye e�it oldu�unu belirtiniz", 84000, 3
      vrmlDir.GetFloat "item2", "1 Derecenin d��eyde ka� metreye e�it oldu�unu belirtiniz", 114000, 3
    end if
    vrmlDir.GetFloat "item3", "Y�kseklik abart�s� belirtiniz", 1, 1
    vrmlDir.GetString "item4", "Vrml Dosyas� i�in dizin ad� belirtiniz", dirpath, 200
    vrmlDir.GetCheck "item5", "Grid tarama aral��� belirmek istiyorum", 0
    vrmlDir.GetCheck "item6", "Doku kaplamak istiyorum", 0
    if vrmlDir.ShowModal then
      if projType = PR_GEO then
        hdistance = vrmlDir.ValueByName("item1")
        vdistance = vrmlDir.ValueByName("item2")
      end if
      texture   = vrmlDir.ValueByName("item6")
      scale     = vrmlDir.ValueByName("item3")
      directory = vrmlDir.ValueByName("item4")
      set objFSO = CreateObject("Scripting.FileSystemObject")
      if not objFSO.FolderExists(directory) then
        msgBox("Verilen dizin yolu ge�erli de�il!")
        exit Sub
      end if
      rsCheck   = vrmlDir.ValueByName("item5")
      if rsCheck then
        set rs = Netcad.NewBDialog("Resampling")
        if projType = PR_GEO then
          rs.GetFloat "item1", "Grid tarama aral���(metre) belirtiniz", 700, 3
        else
          rs.GetFloat "item1", "Grid tarama aral���(metre) belirtiniz", 10, 3
        end if
        if rs.ShowModal then
          brsCheck = true
          resampling = rs.ValueByName("item1")
        else
          brsCheck = false
        end if
      else
        brsCheck = false
      end if
      vrmlIn = "<VRML><LayerMesh><GridFiles>"
      for i = 0 to UBound(reflist)
        vrmlIn = vrmlIn & "<FileName>" & refList(i).Name & "</FileName>"
      next
      vrmlIn = vrmlIn & "</GridFiles>"
      if projType = PR_GEO then
        vrmlIn = vrmlIn & "<HorizontalDistance>" & hDistance & "</HorizontalDistance>"
        vrmlIn = vrmlIn & "<VerticalDistance>" & vDistance & "</VerticalDistance>"
      end if
      vrmlIn = vrmlIn & "<Scale>" & scale & "</Scale>"
      if brsCheck then
        vrmlIn = vrmlIn & "<Resampling>" & resampling & "</Resampling>"
      end if
      if texture then
        vrmlIn = vrmlIn & "<Texture>" & "1" & "</Texture>"
      end if
      vrmlIn = vrmlIn & "</LayerMesh><Directory>" & directory &  "</Directory></VRML>"
      vrmlOut = ncxmlserviceman.runxml("NETCAD.VRML", vrmlIn, "")
      set xmlDoc   = CreateObject("Microsoft.XMLDOM")
      xmlDoc.async = "false"
      noerr = xmlDoc.loadXML(vrmlOut)
      if noerr then
        navigate(directory & "\" & xmlDoc.documentElement.Text)
      else
        msgBox("XMLDOM y�kleme hatas�!")
      end if
    end if
  end if
End Sub



' AMA� : Ekrandan se�ilen �okludo�rular�n VTKODU bilgisini al�p
'        �okludo�rular�n ADI bilgisine yazmaktad�r.
' GIRDILER : Ekrandan se�im menusu ile se�ilecek �okludogrular
'




Sub Main
Dim i,j,o,SEL,K
  with Netcad
    set SEL = .NewSelectionSet
    set o = .NewObject
    if SEL.SELECT("'VT' Kodlar� 'Ad' Olarak Atanacak �okluDo�rular� Se�in",array(opline)) then
      for i = 0 to SEL.NE-1
        j = SEL.GetSelectedObject(i, o)
        o.pname=o.objname
        .putobject j, o
      next
      SEL.RedrawAndRewind
    end if
    set SEL = nothing
    set o = nothing
  end with
end sub


'A��klama :vtKoduna ve gis s�n�f�na g�re objelerin
'          renklendirilmesini/tabakaland�r�lmas�n� sa�lar
'20050609
'Bar�� G�ral
'Netcad

'BG - 20050624 : Lejand �izmi eklendi ve opsiyonel hale getirildi.
sub main
dim i,o,dlg,listFc,listLy,clsList,clsArr,cls,file,chkCol,chkLy,lyName,lyArr,colName
  with netcad
    clsList = findFcClassList
    clsArr  = split(clsList,"|")
    listFc = findFcClassList
    listLy = getLayerList
    lyArr = split(listLy,"|")
    set dlg = .newbdialog("Parametreler")
    dlg.GetFileName "file","Dosya Yeri","","MDB Dosyalar�|*.MDB|Tum Dosyalar|*.*","MDB"  '
    dlg.GetCombo "cls", "S�n�f",listFc , 0
    dlg.getCombo "ly","S�n�f �le �li�kili Tabaka",listLy,0
    dlg.GetRadio "opType", "Uygulanacak i�lem", "�li�kili Kolona G�re Tabakaland�r|Kolon De�erine G�re Renklendir", 0
    dlg.GetString "colName", "�li�kili Koln Ad�", "", 25
    dlg.GetCheck "lejand", "Lejand �iz", 1
    if dlg.showmodal then
      if dlg.ValueByName("file") <> "" then
        if dlg.ValueByName("colName") <> "" then
          if checkcolName(dlg.ValueByName("colName"),clsarr(dlg.ValueByName("cls"))) <> 0 then
            cls = clsarr(dlg.ValueByName("cls"))
            file = dlg.ValueByName("file")
            if dlg.ValueByName("opType") = 1 then
              lyName = lyArr(dlg.ValueByName("ly"))
              colName = dlg.ValueByName("colName")
              ColorizeObjects cls,lyName,colName,file
            else
              lyName = lyArr(dlg.ValueByName("ly"))
              colName = dlg.ValueByName("colName")

              if dlg.ValueByName("lejand") then
                LayerizeObjects cls,lyName,colName,file , 1
              else
                LayerizeObjects cls,lyName,colName,file , 0
              end if
            end if
          else
            msgbox "Belirtti�iniz kolon Tabloda Bulunmuyor..."
          end if
        else
          msgbox "Kolon Ad� Belirtmediniz..."
        end if
      else
        msgbox "Dosyan�n Yerini G�stermediniz..."
      end if
    end if
  end with
end sub

function ColorizeObjects(cls,ly,colName,filePath)
dim ds,o,i,cl,val,colDisList,value,vtcol,col
   with netcad
     colDisList = getColorDisList(gettableName(cls),colName,filePath)
     vtCol = getFcColumn(cls)
     set o = .newobject()
     if ubound(colDisList) <> -1 then
      .setFilter nothing,array(.foundlayer(ly)),array()
      while .getNextObject2(o)
        .DrawObject o, red
        value = getObjectsColValue(o.objname,cls,vtCol,colName,filePath)
        'if value > 255 then value = value - 255
         col = findInArray(colDisList,Value)
         if col > 255 then col = col - 255
        o.renk = col
        .putobject .curobjpos,o
      wend
     end if
     .NetcadCommand "REGEN"

   end with
end function

function searchforid(cls,vtCol,vtVal,tableName,dbPath,colName)
dim sql,colorList,con,colList,colArr,colLs,i
    if checkColType(cls,vtcol) <> CT_STRING then
      sql = "SELECT * FROM "&tableName&" WHERE "&vtCol&" = "&vtVal
    else
     sql = "SELECT * FROM "&tableName&" WHERE "&vtCol&" Like '"&vtVal&"'"
    end if
    'msgbox sql
    set con = adoConnectionObject(dbPath)
    set colorList = adoRecordsetObject(sql,con)
    on error resume next
    searchforid = colorList(colName).Value
end function

function findInArray(arr,value)
dim i
  for i = 0 to UBound(arr)-1
    if arr(i) = value then
      findInArray = i
      exit function
    end if
  next
end function

function getObjectsColValue(vt,cls,vtCol,colName,file)
dim o,i,j,ds,tbName,fcColName
  with netcad
     tbName = getTableName(cls)
     if searchforid(cls,vtCol,vt,tbName,file,colName) <> "" then
        getObjectsColValue = searchforid(cls,vtCol,vt,tbName,file,colName)
     else
        getObjectsColValue = "0"
     end if
  end with
end function


function adoConnectionObject(dbPath)
   conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function


function adoRecordsetObject(sql,connection)
  tables = CreateObject("ADODB.Recordset")
  tables.Open sql,connection,3,2
  set adoRecordsetObject =  tables
end function

function getColorDisList(tableName,colName,dbPath)
dim sql,colorList,con,colList,colArr,colLs,i
    colArr = ""
    sql = "SELECT DISTINCT "&colName&" FROM "&tableName
    set con = adoConnectionObject(dbPath)
    set colorList = adoRecordsetObject(sql,con)
    i = 0
    while not colorList.Eof
      if i = 0 then
        colLs =  colorList(colName).Value
      else
        colLs = colLs & "|" & colorList(colName).Value
      end if
      colorList.moveNext
      i = i +1
    wend
    colArr = Split(colLs,"|")
    getColorDisList = colArr
end function



function LayerizeObjects(cls,ly,colName,filePath,drwLjd)
dim ds,o,i,cl,val,colDisList,value,vtcol,ljdList
   with netcad
     colDisList = getColorDisList(gettableName(cls),colName,filePath)
     vtCol = getFcColumn(cls)
     ljdList = "XXX" ' Catch the foo ones.
     set o = .newobject()
      .setFilter nothing,array(.foundlayer(ly)),array()
      while .getNextObject2(o)
        .DrawObject o, red
        value = getObjectsColValue(o.objname,cls,vtCol,colName,filePath)
        'msgbox Value
        if value <> "0" then
          if checkLayerName(Value) = -1 then
            .createLayer value,.numLayers
            ljdList = ljdList + "|" + value
          end if
            o.tabaka = .foundlayer(value)
            on error resume next
            .putobject .curobjpos,o
        end if
      wend
     .NetcadCommand "REGEN"
     if drwLjd = 1 then DrawLejand ljdList
   end with
end function

function checkLayerName(ly)
  if netcad.foundLayer(ly) <> -1 then
    checkLayerName = ly
  else
    checkLayerName = -1
  end if
end function

function getTableName(cls)
  dim ds,o,i,cl,val
   with netcad
     set ds = ncconman.findDatasetByFc(false,cls)
     if ds.open then
       getTableName = ds.TableName
       exit function
     else
       msgbox "Bir Hata Olu�tu - getTableName"
     end if
   end with
end function

function getFcColumn(cls)
  dim ds,o,i,cl,val
   with netcad
     set ds = ncconman.findDatasetByFc(false,cls)
     if ds.open then
       getFcColumn = ds.FcColumn.Name
       exit function
     else
       msgbox "Bir Hata Olu�tu - getFcColumn"
     end if
   end with
end function

function getLayerList()
dim i,j,sList
  sList = ""
  with ncLayermanager
    for i = 0 to .numLayer-1
      if i = 0 then
        sList = .layer(i).Name
      else
        sList = sList & "|" & .layer(i).Name
      end if
    next
    getLayerList = sList
  end with
end function

function findFcClassList()
dim i,j,sList
   sList = ""
   with ncconman
     for i = 0 to .numConnection-1
       for j = 0 to .connection(i).numTables -1
         if .Connection(i).table(j,false).fcName <> "" then
           if i = 0 then
             sList = .Connection(i).table(j,false).fcName
           else
             sList = "|" & .connection(i).table(j,false).FcName
           end if
         end if
       next
     next
     findFcClassList = sList
   end with
end function

function adoConnectionObject(dbPath)
dim conMain
   set conMain = CreateObject("ADODB.Connection")
   conMain.Provider="Microsoft.Jet.OLEDB.4.0"
   conMain.ConnectionString = "Driver={Microsoft Access Driver (*.mdb)};Uid=Admin;Pwd="
   conMain.Open dbPath
   set adoConnectionObject = conMain
end function


function adoRecordsetObject(sql,connection)
dim tables
  set tables = CreateObject("ADODB.Recordset")
  tables.Open sql,connection
  set adoRecordsetObject =  tables
end function

function checkColName(colName,sinif)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,sinif)
  if ds.open then
    for i = 0 to ds.columnCount-1
      if ds.columnByNo(i).name = colName  then
        checkColName = 1
        exit function
      end if
    next
  end if
  checkColName = 0
end function

function checkcolType(cls,colName)
dim ds,i
  set ds = ncconman.finddatasetbyFc(false,cls)
  checkColType = ds.columnByName(colName).colType
  exit function
end function

sub DrawLejand(ljdList)
dim  objpos,lyArr,i,frameHeigth,frameWidth
  with netcad
    lyArr = split(ljdList,"|")
    objpos = .numObject
    for i = 0 to ubound(lyArr)
      if .foundlayer(lyArr(i)) <> -1 then
        DrawLejandCell .foundLayer(lyArr(i)),lyArr(i),1,0,(0+i*30)
      end if
    next
    frameHeigth = ubound(lyArr)*33
    frameWidth  = 300
    'DrawLejandFrame .newc(-20,5,0),.newc(frameWidth,frameHeigth,0)
    .Place "TEST", .newc(0,0,0), objPos

  end with
end sub

function DrawLejandCell(color,caption,scf,starty,startx)
dim p1,p2,box,pLine,cpText,param
  with netcad
    set pLine = .newpoly()
    param = getScf()
'    msgbox caption
    pLine.AddCoor(.newc(starty,startx,0))
    pLine.AddCoor(.newc(starty+0,startx+25,0))
    pLine.AddCoor(.newc(starty+75,startx+25,0))
    pLine.AddCoor(.newc(starty+75,startx+0,0))
    pLine.AddCoor(.newc(starty+0,startx+0,0))

    set cpText = .MakeText(.newc(starty+102,startx+5,0), caption, 0,0, 10,0,0,.foundlayer(caption))
    set box = .MakePline("LjBox", 3, 0,.foundlayer(caption),0, 0, pLine)
    box.renk = color
    cpText.renk = 16
   .addobject cpText
   .addobject box
  end with
end function

function DrawLejandFrame(p1,p2)
dim o
  with netcad
   set o = .MakeRect(p1, p2, "", 0)
   o.renk = 16
   .addobject o
  end with
end function

function getScf()
dim world
  with netcad
    set world = .findWorld
    getScf = (abs(world.cll.y-world.cur.y)*6)/800
  end with
end function





'Belirli bir say�dan ba�layarak objelere vtKodu atama
sub main
dim dlg,sel,obj,i,oi,o,cnt
  with netcad
    set obj = .newobject()
    set dlg = .newbDialog("Ba�lat� Kodu Atama")
    dlg.GetString "startVal", "Ba�lang�� De�eri", "VT_", 20
    dlg.GetString "Cls", "S�n�f", "", 50

    if dlg.ShowModal then
      set sel = .newSelectionSet
      if sel.SELECT("Ba�lant� Kodu �retmek istedi�iniz Objeleri Se�iniz", array()) then
        for i = 0 to sel.NE-1
          oi = sel.GetSelectedObject(i, obj)
          if isNumeric(dlg.ValueByName("startVal")) then
            cnt = dlg.ValueByName("startVal") + i
            obj.objName = dlg.ValueByName("startVal") + i
          else
            cnt =  dlg.ValueByName("startVal")&i
            obj.objName = dlg.ValueByName("startVal")&i
          end if
          .putObject oi,obj

        next
      end if
    end if
    msgbox cnt
  end with
end sub


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",0,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",0,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      'h.s=b.s
      'h.p1.y=b.p1.y
      'h.sc=b.sc
      h.p1.x=b.p1.x
      'h.angle=b.angle
      ' h.tabaka=b.tabaka
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",0,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",0,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      'h.s=b.s
      'h.p1.y=b.p1.y
      'h.sc=b.sc
      h.p1.x=b.p1.x
      'h.angle=b.angle
      ' h.tabaka=b.tabaka
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB



sub main

with netcad
  ALANGEZ


end with

end sub






sub ALANGEZ

          dim obj
          dim i
          dim poly
          with netcad


                .setfilter nothing,array(),ARRAY(opline)

            DO
                SET OBJ=.GETNEXTOBJECT

                IF OBJ IS NOTHING THEN
                   EXIT DO
                ELSE


               set poly =obj.getobjectaspline


                if obj.pname="X" then


                       yazigez poly,obj




                       end if

      .putOBJECT .CUROBJPOS,OBJ



                 end if
           LOOP
           .resetfilter
           end with

end sub

sub yaziGEZ(poly,obj)

          dim obj1
          dim i
          with netcad


                .setfilter nothing,array(),ARRAY(otext)

            DO
                SET OBJ1=.GETNEXTOBJECT

                IF OBJ1 IS NOTHING THEN
                   EXIT DO
                ELSE

                if poly.inPoly(obj1.p1) then

                obj.pname=obj1.s
                .putOBJECT .CUROBJPOS,OBJ1

                 end if

                 end if
           LOOP
           .resetfilter
           end with

end sub

'
' Dil  : Visual Basic
' Ama� : Macro.RUN XML Servisi icin Ornek macro.
'        
'
Sub Main
Dim abc, o, w
  with Netcad
    abc = NCParam.GetValue("ABC")
    set o= NCParam.GetValue("MYOBJECT")  ' nokta objesi
    o.x = o.x + abc

    set w = .NewWorld(o.y-100, o.x-100, o.y+100, o.x+100)

    ' Sonuclari Doldur
    NCParam.SetValue "KLM", o.x
    NCParam.SetValue "OUTOBJECT", w
  end with
end sub


'Tarih :04/03/2001 V1.00
' Ama� : Ekrandan se�ilen objenin renk, tabaka,hattipi de�erlerini
'        se�ilen di�er objelere atamak.
'Girdi : Obje


SUB Main
DIM ss,b,h

  with netcad
    set ss = .NewSelectStatus                         ' Anlik Secim objesi yarat
    .SelectObjectInstant "Se�ti�iniz objenin bi�imi al�nacak.",1,array(),ss
    set b= ss.objects(0)                              'Bi�imleri b objesine ata
    while .SelectObjectInstant("Se�ti�iniz objenin bi�imi de�i�ecek.",1,array(),ss)  ' obje sec
      set h = ss.objects(0)                           ' Secim objesinin ilk objesini al
      'h.tabaka=b.tabaka
      'h.W=b.W
      'h.LT=b.LT
      'h.s=b.s
       h.p1.y=b.p1.y
      'h.sc=b.sc
      'h.p1.x=b.p1.x
      'h.angle=b.angle
      ' h.tabaka=b.tabaka
      .PutObject ss.indexs(0), h                      ' objeyi geri koy
      .DrawObject h,-1                                ' kendi rengi ile ciz
    wend
    set ss = nothing
    set b = nothing
    set h = nothing
  end with


END SUB


' Yazan : ERC�MENT KORKMAZ
' Tarih : 20.07.2006
' A��klama : Yay objelerinin ters olan kal�nl�klar�n� d�zelten macro
'
'

Sub Main
Dim i, o, tbk, nok1, nok2, c, d, ln, a, b, word, dtbk, oo, nokk1, nokk2
dim x1, y1, x2, y2, x3, y3, x4, y4
dim bd
  with Netcad
  set bd = .NewBDialog("")
BD.Getstring   "a","Do�rular ��in Tabaka Giriniz", "K", 20
BD.Getstring   "b","Yay dan D�nen Do�rular ��in Tabaka Giriniz", "Y", 20
bd.showmodal
  a = bd.ValueByName("a")
  b = bd.valuebyname("b")
     tbk = .FoundLayer(a)
     dtbk = .FoundLayer(b)
       .setfilter nothing, array(dtbk), array(oline)
         do
          set o = .getnextobject
          if o is nothing then
          exit do
          end if
          i =  .CurObjPos
       if o.w<0 then o.w = (o.w * -1)
       .putobject i,o

          if o.tabaka = dtbk then
          set nok1 = o.p1
         x1 = round(nok1.x,3)
         y1 = round(nok1.y,3)
           set nok2 = o.p2
         x2 = round(nok2.x,3)
         y2 = round(nok2.y,3)
         set word = .NewWorld(o.p1.y-0.1, o.p1.x-0.1, o.p1.y+0.1, o.p1.x+0.1)
         .setfilter word, array(tbk), array(oline)
             do

              set oo = .getnextobject
              if oo is nothing then
              exit do
              end if
              set nokk1 = oo.p1
               x3 = round(nokk1.x,3)
               y3 = round(nokk1.y,3)
              set nokk2 = oo.p2
               x4 = round(nokk2.x,3)
               y4 = round(nokk2.y,3)
             if x1 = x3 and y1 = y3 then o.p1.x = x2: o.p1.y = y2: o.p2.x= x1: o.p2.y = y1: o.tabaka = tbk': msgbox "a" ' o.w = o.w * -1
             if x2 = x4 and y2 = y4 then o.p1.x = x2: o.p1.y = y2: o.p2.x= x1: o.p2.y = y1: o.tabaka = tbk': msgbox "b"
             .PutObject i, o
             loop
            .resetfilter
          else o.tabaka = tbk

          end if
         loop
       .resetfilter
  end with
End Sub

' Dil   : Visual Basic
' Ama�  : Yaz� objelerinin i�eri�ini h�zl� bir bi�imde de�i�tirmek.
' Yazan : Arif �engel 2007
'
Sub Main
    Dim ss,o,dlg

    with netcad
         set ss = .NewSelectStatus

         while .SelectObjectInstant("��eri�i de�i�ecek yaz�y� se�?",1,array(otext),ss)
               set o = ss.objects(0)

               set dlg = .NewBDialog("Yaz� De�i�tir")

               dlg.GetString "yazi", "", o.s ,500

               if dlg.showmodal then
                  o.s=dlg.ValueByName("yazi")
               end if

               .PutObject ss.indexs(0), o
               '.DrawObject o,-1
               .NetcadCommand("REGEN")

               set o = nothing
         wend
    set ss = nothing
  end with
end sub


' Yazan : ERC�MENT KORKMAZ
' Tarih : 30.11.2005
' A��klama : NETCAD DATASINDA BULUNAN B�T�N T�K�E KARAKTERL� YAZI OBJELER�N�N
            'T�RK�E HARFLER�N� D�ZELT�YOR.

Sub Main
Dim i,o, n
  with Netcad
     set o =  .NewObject
   .SetFilter nothing, array(), array(otext)
    Do
     set o = .GetNextObject
    if o is nothing then
    exit do
      end if
       i =  o.s
        i = replace(i,"�","U")
        i = replace(i,"�","u")
        i = replace(i,"�","S")
        i = replace(i,"�","s")
        i = replace(i,"�","I")
        i = replace(i,"�","i")
        i = replace(i,"�","G")
        i = replace(i,"�","g")
        i = replace(i,"�","O")
        i = replace(i,"�","o")
        i = replace(i,"�","C")
        i = replace(i,"�","c")
        o.s = i
         n = .CurObjPos
        .PutObject n, o
        set o = nothing 
       loop
   .ResetFilter
  end with
End Sub


'----------------------------------------------------------------------------------------------------
SUB Main
DIM o,ss,b,h,i,d,ds ,j,mes,mysqr,c1,c2,reg ,cnum, lnum
Dim BD
Dim AdaTabaka, ParselTabaka, AdaNoTabaka, ParselNoTabaka
Dim Ada
Dim s,iy,by,mode,parsel,pln


with netcad
  '--------------------------- Dialog kutusunda de�i�kenleri sor ---------------------------------------
  set BD = .NewBDialog("Ada/Parsel �ret V.1.00 27/09/2001")          ' yeni dialog yarat
  BD.GetString  "Parsel","Ada Poligon Tabakas�", "SOKAK_AKS", 15
  BD.GetString  "Yazi","Parsel No Tabakas�", "OKU", 15
  BD.PutPrompt  "Tabaka isimlerini b�y�k harfle yaz�n !"             ' Aciklama Yazdir.
  BD.PutPrompt  " "
  if BD.showmodal then
    ParselTabaka=.FoundLayer(BD.ValueByName("Parsel"))
    ParselNoTabaka=.FoundLayer(BD.ValueByName("Yazi"))
    if  ParselTabaka=-1 or ParselNoTabaka=-1 then
      msgbox "girdi�iniz tabakalardan biri veya birka�� projede bulunamad� !!!"
      Exit Sub
    End if
   Else
    Exit Sub
  End if
  set BD = Nothing
  '-------------------------Dialog Son-------------------------------------------------
  '******************* Adaya i�indeki yaz�y� ata *************
  for i = 0 to .numobject-1      ' projedeki tum objeleri sirayla tara
    .BackMessage
    .SetMessage i
    set parsel = .getobject(i)         ' i. objeyi al
    if parsel.tag = opline and parsel.tabaka=ParselTabaka  then        ' Coklu dogrumu ?
      .DrawObject Parsel, blue
      .SetFilter .ObjectExtends(Parsel), array(ParselNoTabaka), array(otext)          'Filitre uygula
       Do
         set by = .GetNextObject   ' Yukardaki Kurala uyan bir sonraki objeyi getir
         if by is nothing then     ' Eger kurala uyan obje kalmadiysa sonuc "nothing" doner
           exit do                ' Bu durumda donguyu durdur
          else
           set pln=.GetPlineExt(parsel)
           if pln.OnPoly(false, by.p1) then
            .DrawObject Parsel, RED
             parsel.OBJname=by.s
             .putobject i,parsel
           end if
         end if
       Loop
       .resetfilter
    end if
    set by = nothing               ' obje icin aldigimiz memory'i geri ver
  next
  set parsel=nothing
  .BackMessage

  .NetcadCommand("REGEN")
end with
END SUB


' Yazan : ERC�MENT KORKMAZ
' Tarih : 30.11.2005
' A��klama : NETCAD DATASINDA BULUNAN B�T�N T�K�E KARAKTERL� YAZI OBJELER�N�N
            'T�RK�E HARFLER�N� D�ZELT�YOR.

Sub Main
Dim i,o, n , yn, p
p = 1
  with Netcad
     set o =  .NewObject
     set n =  .Newc(0, 0, 0)
   .SetFilter nothing, array(), array(otext)
    Do
     set o = .GetNextObject
    if o is nothing then
    exit do
      end if

    set n =  o.Limits.GetAsPline.CenterOfMass
      ' n.x = o.p1.x
       'n.y = o.p1.y
       n.z = o.s
      set yn = .MakePoint(n, p, "1", 0)
          .AddObject yn
        set o = nothing 
        p = p + 1
       loop
   .ResetFilter
  end with
End Sub

' Se�ilen yaz� objesinin bilgisini 2.75m ile �arparak �okludo�runun
' kal�nl�k bilgisi olarak atayan makrodur. �BB Fotogrametri Paftalar�n�n
' 3-D projelerinin haz�rlanmas�nda kullan�labilir.
' MANUEL TEK TEK SECEREK CALISMAKTADIR
' SECME MENUSUNU KULLANARAK KALINLIK URETMEK ICIN  AlanIcindekiYazi2Kalinlik.nvb �ALI�TIRINIZ
'

SUB Main
DIM SECIM,ATA,DATA,KOT

  with netcad
    set SECIM = .NewSelectStatus
    while .SelectObjectInstant ("Yaz�y� se�in",1,array(otext),SECIM)
      set ATA= SECIM.objects(0)
       .SelectObjectInstant "Se�ilen Polyline �n Kal�nl�k De�eri De�i�ecek",1,array(opline),SECIM
        set DATA = SECIM.objects(0)
        KOT=ATA.s*2.75
        DATA.THICKNES=KOT
        .PutObject SECIM.indexs(0), DATA
        .DrawObject DATA,-1
     wend
        set SECIM = nothing
        set ATA = nothing
        set DATA = nothing

  end with
END SUB

'Makro Yazar�:      Harita M�hendisi O�uzalp BOZKURT
'Ama�:              Yaz� Boyunun De�i�tirilmesi
'Tarih:             HAZ�RAN-2012

Sub Main

with netcad

Dim obj
dim selection
Dim BD
dim item


set BD = Netcad.NewBDialog("YAZI BOYUNU G�R�N�Z")

    BD.Getfloat "item","YAZI BOYU",0,1

    if BD.showmodal then

set selection = .NewSelectStatus

    while .SelectObjectInstant("BOYU DE���ECEK YAZI GRUBUNA A�T B�R YAZI OBJES� SE�",1,array(otext),selection)
          set obj = selection.objects(0)
          .DrawObject obj,blue


.SetFilter nothing, array(obj.tabaka), array(oText)

           do

             set obj=.getnextobject

             if obj is nothing then

           exit do

             end if

                 obj.sc = BD.ValueByName("item")
                 .PutObject .CurObjPos, obj

           loop

.resetfilter

    wend

    end if

.netcadcommand("REGEN")
 


end with

end sub

sub main
dim o,oo,i,j,dlg,txtLy,pntLy,foo,c
dim aOddLyFilter,npLayer,check
  with netcad
    set o = .newobject
    set oo = .newpoly
    set foo = .newobject
    set dlg = .newBDialog("Yaz�dan Nokta Olu�tur")

    dlg.GetCombo "lList", "Yaz� Tabaka", getLayerList, 0
    dlg.getString "nLayer","Nokta Tabakas�","NOKTA",50
    dlg.GetCheck "checkProb", "Alan ��indemi ?", 1
    dlg.getString "aOddLyFilter","Kontrol i�in tabaka filtresi ","YAPI",10
    dlg.getString "npLayer","Problemli Nokta Tabakas�","NOKTA_SORUNLU",50

    if dlg.showModal then
      txtLy = dlg.valueByName("lList")
      pntLy = dlg.valueByName("nLAyer")
      aOddLyFilter = dlg.ValueByName("aOddLyFilter")
      npLayer= dlg.ValueByName("npLayer")
      check = dlg.ValueByName("checkProb")

      .createLayer pntLy,red
      .createLayer npLayer, cyan

      if txtLy <> "" and pntLy <> "" then
        .setFilter nothing,array(txtLy),array(otext)
        while .getNextObject2(o)
          set c = getCenter(o.limits.cll,o.limits.cur)
          set foo = .MakePoint(c,o.s,o.s,.foundLAyer(pntLy))
          foo.pname = o.s
          .addObject foo
        wend
        .resetFilter
      else
        msgBox "Nokta veya Yaz� tabakas� Yok !!!"
        exit sub
      end if

      if check = 1 then
         checkIfInside aOddLyFilter,pntLy,npLayer
      end if
    end if
  end with
end sub

function getLayerList
dim i, s
  with ncLayerManager
    for i = 0 to .numLayer-1
      if i = 0 then
        s = .layer(i).Name
      else
        s = s & "|" & .layer(i).Name
      end if
    next
    getLayerList = s
  end with
end function

function getCenter(p1,p2)
  with netcad
   set getCenter = .newC((p1.y+p2.y)/2,(p1.x+p2.x)/2,(p1.z+p2.z)/2)
  end with
end function

function checkIfInside(filter,pointLayer,pnPointLayer)
dim o,oo,i,j,foo,arrList
  with netcad

    foo = .createLayer("foo",red)
    set o =  .newobject
    set oo =  .newobject
    arrList = createLayerList(filter)
    .setfilter nothing,arrList,array()
    while .getNextObject2(o)
      .drawObject o,red
      .setFilter o.Limits,array(.foundLayer(pointLayer)),array(opoint)
      while .getNextObject2(oo)
        'msgbox "geldi"
        if o.inObject(oo.p1) then
          oo.Tabaka = foo
          .putObject .curobjPos,oo
        end if
      wend
      .resetFilter
    wend
    .resetfilter

    .setFilter nothing,array(.foundLayer(pointLayer)),array(opoint)
    while .getNextOBject2(o)
      o.Tabaka = .foundLAyer(pnPointLayer)
      .putObject .curobjpos,o
    wend
    .resetFilter

    .setFilter nothing,array(foo),array(opoint)
    while .getNextOBject2(o)
      o.Tabaka = .foundLAyer(pointLayer)
      .putObject .curobjpos,o
    wend
    .resetFilter
    ncLayerManager.Delete foo, true
  end with
end function

function createLayerList(filter)
dim arr,i,foo
  redim arr(0)
  with netcad
    for i = 0 to .NumLayers
      if (inStr(.layerNameOf(i),filter) = 1) and (inStr(.layerNameOf(i),"@") = 0) then
        redim preserve arr(ubound(arr)+1)
         ' msgbox .layerNameOf(i)
          arr(ubound(arr)) = i
      end if
    next
    createLayerList = arr
  end with
end function


' Yazan : 
' Tarih : 11.06.2012
' A��klama : 

Sub Main
Dim Top
Dim obj
dim bd
dim bz

  with Netcad
     set BD = Netcad.NewBDialog("eski yaz�")

    BD.Getstring "item","YAZI","",50

    if BD.showmodal then

        set Bz = Netcad.NewBDialog("yeni yaz�")

    Bz.Getstring "item","YAZI","",50

    if Bz.showmodal then

    .setfilter nothing, array(),array(otext)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
      obj.s=replace(obj.s,BD.ValueByName("item"),Bz.ValueByName("item"))
           end if
            .PUTOBJECT .CUROBJPOS,OBJ
        loop
    .resetfilter

    end if
    end if
  end with
End Sub

'Makro Yazar�:      Harita M�h. O�uzalp BOZKURT
'Ama�:              Kopyalama modu a��k olarak yaz� objelerinin h�zl� kayd�r�lmas�
'Tarih:             MAYIS-2012

Sub Main
Dim obj,secim,layer,yazi,c,yaz,alan,obj1

with netcad
set secim = .NewSelectStatus()
.SelectObjectInstant "KAYDIRILACAK YAZIYI SE�",1,array(otext),secim

set obj = secim.objects(0)
.DrawObject obj,red
yazi = obj.s
set c = .newc(0,0,0)
layer = obj.tabaka
set yaz=.maketext(c,yazi,0,0,obj.sc,obj.angle,"L",layer)
yaz.p1 = c
yaz.wsc = obj.wsc
while .WalkObject("YAZI ���N YER �E�",c,8,yaz)
.addobject(yaz)
wend

.netcadcommand("REGEN")
end with
end sub





'Makro Yazar�:      Harita M�hendisi O�uzalp BOZKURT
'Ama�:              Se�ilen tabakan�n toplam alan�n�n bulunmas�
'Tarih:             HAZ�RAN-2012

Sub Main

with netcad

Dim obj
Dim secim
dim alanobje
dim topla
dim c
dim yaz
dim yazi
dim a
dim b
dim text


topla= 0

set secim = .NewSelectStatus()

while .SelectObjectInstant("YAZI SE�",1,array(otext),secim)

set alanobje = secim.objects(0)

.drawobject alanobje,9

topla = topla+(alanobje.s)

wend
 
msgbox round(topla,2)


end with
end sub

'
' Ama� : Yer kotu yaz�lar�na kot noktalar� �retilmesi.
'        Microstation dan gelen 123.12 kot yaz�lar�n�n '.' s�na kot noktas� atar.
' Girdiler : Yer kotu yaz�lar�
' Uyar�lar : Ekranda sadece yerkotu a��k b�rak�l�r
'           TIN isimlitabakaya kotlu noktalar uretilir.


Sub KotYaziToNokta         
Dim AtlamaSay, Point
Dim i,j,o,p

with Netcad
  .SetFilter nothing, AcikTabakalar,array(otext)          
  Do
    set o = .GetNextObject   
    if o is nothing then     
      exit do                
     else
       if (o.s=".") and (o.tabaka = .foundLayer("YERKOTU")) then .AddObject .MakePoint(o.p1,"T",1,0,6,.FoundLayer("TIN"))
       set o = nothing                                     ' .MakeText(c, txt, flags,font, size,angle,just,tabaka)
    end if                                                  '.MakePoint(c, name, code, Tabaka)
  loop
end with
End Sub
'----------------------------------------------------------------------------------------
Function AcikTabakalar
Dim i
Dim t()
  With netcad
    ReDim t(.NumLayers)
    For i=0 to .NumLayers-1        't() ye tabakalar� doldur
      if .IsLayerOpen(i) then t(i)=i else t(i)=-1
    Next
    AcikTabakalar=t
  End with
End Function


Sub Main
  With netcad

   ' msgbox "Sadece kotu �retilecek tabakalar� a��k b�kar�n.     "
    .CreateLayer "TIN", 11
    KotYaziToNokta
  End With
End Sub

 sub main
 with netcad
dim pos
 dim obj



 .setfilter nothing,array(),array(otext)
       Do
         set obj=.getnextobject
         if obj is nothing then
            exit do
         else


            pos=split(obj.s,"/")
           'pos = InStr(1, obj.s, "_")
             'obj.s=pos(0)

         obj.s=replace(obj.s,pos(0),"")
         obj.s=replace(obj.s,"/","")
           .putobject .curobjpos,obj




         end if

         loop
         end with

         end sub




' Yazan : 
' Tarih : 11.06.2012
' A��klama : 

Sub Main
Dim Top
Dim obj
dim bd
dim bz

  with Netcad
     set BD = Netcad.NewBDialog("eski yaz�")

    BD.Getstring "item","YAZI","",50

    if BD.showmodal then

        set Bz = Netcad.NewBDialog("yeni yaz�")

    Bz.Getstring "item","YAZI","",50

    if Bz.showmodal then

    .setfilter nothing, array(),array(opline)
        do
           set obj=.getnextobject

           if obj is nothing then
              exit do
           else
      obj.pname=replace(obj.pname,BD.ValueByName("item"),Bz.ValueByName("item"))
           end if
            .PUTOBJECT .CUROBJPOS,OBJ
        loop
    .resetfilter

    end if
    end if
  end with
End Sub


' Yazan : 
' Tarih : 29.5.2014
' A��klama : 

Sub Main
Dim obj2,ss,b,c,secim

  with Netcad
  set secim = .NewSelectStatus
   while .SelectObjectInstant ("GE��C� VE DA�M� KOL�DOR C�ZG�LER�N� SE�",2,array(oline), secim)
    set b=  secim.objects(0)
    set c=  secim.objects(1)
    wend


      .SetFilter nothing, ARRAY(b.tabaka,_
                                c.tabaka), ARRAY(oline)

            DO
                SET OBJ2=.GETNEXTOBJECT
                    IF OBJ2 IS NOTHING THEN
                EXIT DO
                ELSE
                END iF



                cevir obj2

            LOOP

    .resetfilter
  end with
End Sub




sub cevir(obj2)
with netcad
dim obj
dim poly

             .SetFilter nothing, ARRAY(), ARRAY(opline)

            DO
                SET OBJ=.GETNEXTOBJECT
                IF OBJ IS NOTHING THEN
                EXIT DO
                ELSE
                END iF
                 set poly = .getplineext(obj)



               if  .IntersectObjects(obj2, obj, poly)>0  then
                .drawobject obj,7
               obj.tabaka=.createlayer("KAD_ALN",104)

               .PUTOBJECT .CUROBJPOS,OBJ
                end if




           loop

  .resetfilter
end with
end sub



'TARIH       : 30.09.2004
'AMA�        : �stenilen bir klas�rde bulunan dosyalar�n teker teker a��l�p,
              'istenilen d�zenlemelerin yap�lmas� ve dosyalar�n istenilen
              'klas�re kay�t edilmesi.
'GEREKLILIKLER : Gerekli dosyalar olan 
'                - Netplot.lin Netcad kurulum dizinine
'                - DGC Netcad kurulum dizindinde SECME klas�r�ne
'                - Plan.NCS ve Plan.Ini Kurulum dizininde SEMBOL klas�r�ne.
'                kopyalanmal�d�r. 
' HAZIRLAYAN : Bar�� G�ral
' NetCAD �stanbul B�lge M�d�rl���
'
'
' Sabitler
const InPath  = "\\Bgoralpc\5000\DGN_NCZ\BAGCILAR\DGN_MACRO\"    'Girdi Dosyalar�n�n yeri
const OutPath = "\\Bgoralpc\5000\DGN_NCZ\BAGCILAR\NCZ\"    'Sonu� Dosyalar�n�n yeri
'
'
'Ana Program
Sub main
dim dir,fSo,Count,file,inFile
  with netcad
    set fSo = CreateObject("Scripting.FileSystemObject") 'Dosya Sistemi objesi olu�turulur.
    set dir = fso.GetFolder(InPath)                      'Bu Objenin �zelliklerinden GetFolder
                                                         'kullan�larak klas�r bir objeye atan�yor.
    set Count = dir.Files                                'Klas�rdeki dosya listesi al�n�yor
    for each file in Count
      .Loadfile InPath&file.name,fs_microst               'InPath Klas�rdeki dosya file.name isimli
                                                          'dosyalar s�rayla y�kleniyor. Yanda
                                                          'LoadFile komutu ile y�klenecek dosya tipi
                                                          'parametresi olarak fs_microstat (Microstation)
                                                          'g�nderiliyor.


      '
      '
      'Hatal� gelen objeler i�in bir tabaka olu�turulup, bu objeler o tabakaya yerle�tirilmektedir.
      createErrorLayer
      '
      '�stenmeyen tabakalar silinmektedir.
      'deleteLayers
      '
      '0 tabakas�ndaki tan�md��� nesneler olu�turulmu� olan tabakaya aktar�l�yor.
      removeObjects
      '
      'NCZ Dosyas�n�n projeksiyonu ayarlanmaktad�r.
      setProjection
      '
      ' Kullan�lmayan tabakalar, �izgi tipleri, yaz� tipleri , semboller , bloklar,
      ' bilgilendirme men�s� g�r�nt�lenmeden silinmektedir.
      .NetcadCommand "PROJECT CLEAN 1,1,1,1,1,1"
      
      ' Proje d�zenleme sonras�nda olu�an bozuk objeler silinir.
      DeleteObjects
      'Objeleri Tescile esas Dgn stillerine d�n��t�r�r.
      SetDgnStyle
      '
      .NetcadCommand "PROJECT CLEAN 0,0,0,0,0,1"
      '
      '
      inFile = split(file.name,".",-1,1)
      .SaveToFile OutPath&inFile(0)&".ncz"                ' Yap�lan d�zenleme i�leminden sonra a��lm��
                                                          ' olan proje, Outpath sabitinde belirtilmi�
                                                          ' olan yere saklan�yor.
      '
      .NetcadCommand "CLOSE ACTIVEPROJECT"                ' Aktif Proje Kapat�l�yor
    Next
  end with
end Sub

sub createErrorLayer
  with ncLayerManager     ' Tan�md��� objeler i�in ncLayarManager objesi ile bir tabaka
    .add "TANIMDISI",red  ' olu�turuluyor. Bu tabakaya isim olarak "TANIMDISI" se�ilmi�tir.
  end with                ' Tabaka rengi ise k�rm�z� (red) olarak ayarlanm��t�r.
end sub

sub removeObjects
dim obj,i
  with netcad
    for i = 0 to .numobject-1
      set obj = .getobject(i)                  ' T�m objeler taran�yor.
      if obj.tabaka = 0 then                   ' 0 Tabakas�ndaki objeler s�rayla se�iliyor.
        obj.tabaka = .foundlayer("TANIMDISI")  ' 0 Tabakas�ndaki objelerin tabakalar� "TANIMDISI" olarak
        .putobject i,obj                       ' se�iliyor ve objeler obje listesine yeniden ekleniyor.
      end if
      set obj = nothing                        ' Ge�ici obj nesnesinin i�eri�i haf�zadan siliniyor.
    next
  end with
end sub

sub DeleteLayers
  with ncLayerManager  ' ncLayerManager nesnesi ile istenmeyen tabakalar mevcut ise siliniyor.
    if .Find("PFT_LEJANT") <> -1 then .Delete .Find("PFT_LEJANT"),True  ' .find komutununun sonucu -1 ise ilgili tabaka projede bulunmamaktad�r.
    if .Find("PAFTA_BILGI")<> -1 then .Delete .Find("PAFTA_BILGI"),True ' -1 den farkl� oldu�unda ise tabaka .delete komutu ile silinmektedir.
    if .Find("PFT_K��EKOORD�NAT") <> -1 then .Delete .Find("PFT_K��EKOORD�NAT"),True
    if .Find("PAFTA") <> -1 then .Delete .Find("PAFTA"),True
  end with
end sub

sub setProjection 
dim nProj
  with netcad
    set nProj = .NewProjection           'Aktif dosyan�n projeksiyonunun belirlenmesi i�in
    nProj.ProjectionType   = PR_UTM3     '�ncelikle bir projeksiyon nesnesi olu�turulmal�d�r.
    nProj.Datum            = DATUM_TUR_1 'Projeksiyon parametre de�erleri, kurulum dizininde, NCMACRO
    nProj.Zone             = 30          'klas�r�n�n alt�ndaki NVBASIC dosyas�ndan se�ilmi�tir.
    nProj.setToCurrentProject            'Olu�turulmu� olan projeksiyon aktif projenin projeksiyonu 
  end with                               'olarak set ediliyor.
end sub

sub DeleteObjects
dim obj,i
     with netcad
     for i = 0 to .numobject-1
       set obj = .getobject(i)
       ' tespit edilebilmi� tipteki bozuk objeler taran�r ve silinir.
       ' Yeni tespit edilmi� olanlar buraya eklenmelidir.
       if obj.tag = opline and obj.TABAKA = .foundlayer("E�R�_10M") and obj.length(false) > 500 then 
         .delobject i,obj
       elseif obj.tag = ocircle and obj.TABAKA = .foundlayer("E�R�_1M") then
         .delobject i,obj
       end if
      set obj = nothing
     next
   end with
end sub

sub SetDgnStyle
dim i,obj,txt
   dim i,obj,txt
   with netcad
     for i = 0 to .numobject-1
       set obj = .getobject(i)
       if (obj.tag = opline) and (obj.tabaka = .foundLayer("E�R�_10M") or _
           obj.tabaka = .foundLayer("E�R�_1M") or _
           obj.tabaka = .foundLayer("E�R�_2M") or _
           obj.tabaka = .foundLayer("E�R�_5M") or _
           obj.tabaka = .foundLayer("E�R�_ARA"))then
          obj.flags = 64
         .putobject i,obj
       elseif (obj.tag = otext) then
         set txt = .MakeText(.gettextrefpoint(obj), obj.s, obj.flags,0, obj.sc,obj.angle,obj.just,obj.tabaka)
         set obj = txt
         .putobject i,obj
       end if
      set obj = nothing
      set txt = nothing
     next
   end with
end sub




'Makro Yazar�:      Harita M�hendisi O�uzalp BOZKURT
'Ama�:              Yaz� Boyunun De�i�tirilmesi
'Tarih:             HAZ�RAN-2012

Sub Main

with netcad

Dim obj
dim selection
Dim BD
dim item


set BD = Netcad.NewBDialog("D�N�KL�K")

    BD.Getfloat "item","D�N�KL�K",0,1

    if BD.showmodal then

set selection = .NewSelectStatus

    while .SelectObjectInstant("BOYU DE���ECEK GRUBUNA A�T B�R  OBJE SE�",1,array(),selection)
          set obj = selection.objects(0)
          .DrawObject obj,blue


.SetFilter nothing, array(obj.tabaka), array()

           do

             set obj=.getnextobject

             if obj is nothing then

           exit do

             end if

                 obj.angle = (BD.ValueByName("item"))*0.015707962
                 .PutObject .CurObjPos, obj

           loop

.resetfilter

    wend

    end if

.netcadcommand("REGEN")
 


end with

end sub
