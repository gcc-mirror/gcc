MODULE testimportconst ;  (*!m2iso+gm2*)

FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT printf ;
FROM constdef IMPORT StrConst ;
IMPORT constdef ;


PROCEDURE init ;
BEGIN
   IF NOT StrEqual (StrConst, 'hello')
   THEN
      printf ("failed to import 'hello' from constdef\n");
      HALT (1)
   END ;
   IF NOT StrEqual (constdef.StrConst, 'hello')
   THEN
      printf ("failed constdef.StrConst does not equal 'hello'\n");
      HALT (2)
   END
END init ;


BEGIN
   init
END testimportconst.
