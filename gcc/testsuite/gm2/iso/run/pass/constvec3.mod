MODULE constvec3 ;

FROM libc IMPORT exit ;

CONST
   con2 = con1 ;

TYPE
   Vec5 = ARRAY [1..5] OF INTEGER ;

CONST
   con1 = Vec5 { 100,
                 200,
                 300,
                 400,
                 500 } ;

VAR
   x: INTEGER ;
BEGIN
   x := con1[3] ;
   IF x # con2[1] + con2[2]
   THEN
      exit (1)
   END
END constvec3.
