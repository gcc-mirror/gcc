MODULE constvec2 ;

CONST
   con2 = con1 ;

TYPE
   Vec5 = ARRAY [1..5] OF LONGREAL;

CONST
   con1 = Vec5 { 1.0,
                 2.0,
                 3.0,
                 4.0,
                 5.0 } ;

VAR
   x: LONGREAL;
BEGIN
   x := con1[3] ;
   x := con2[3]
END constvec2.
