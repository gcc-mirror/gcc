MODULE moddiv ;  (*!m2iso+gm2*)

FROM libc IMPORT printf, exit ;

PROCEDURE modpos (x, y: INTEGER) : INTEGER ;
<* gm2.local.fpositive_mod_floor_div=TRUE *>
BEGIN
   RETURN x MOD y
END modpos ;

PROCEDURE modneg (x, y: INTEGER) : INTEGER ;
<* gm2.local.fpositive_mod_floor_div=FALSE *>
<* gm2.local.fpim4=FALSE *>
<* gm2.local.fiso=FALSE *>
BEGIN
   RETURN x MOD y
END modneg ;

BEGIN
   IF NOT (modpos (-31, 10) = 9)
   THEN
      printf ("modpos (-31, 10) should be 9\n");
      exit (1)
   END ;
   IF NOT (modneg (-31, 10) = -1)
   THEN
      printf ("modneg (-31, 10) should be -1\n");
      exit (1)
   END ;
   printf ("tests pass\n");
END moddiv.
