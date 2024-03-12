MODULE packedrecord3 ;  (*!m2iso+gm2*)

FROM libc IMPORT printf, exit ;

TYPE
   subrange = [0..63] <* bytealignment (0) *> ;

   packedrec = RECORD
                  <* bytealignment (0) *>
                  bool: BOOLEAN ;
                  col : (white, black) ;
                  sub : subrange ;
               END ;


VAR
   global: subrange ;
   pr    : packedrec ;


PROCEDURE test (s: subrange; level: CARDINAL) ;
BEGIN
   IF s # global
   THEN
      printf ("failed to pass %d into test\n", ORD (s)) ;
      exit (1)
   END ;
   IF level > 0
   THEN
      test (s, level-1)
   END
END test ;


BEGIN
   IF SIZE (pr) # 1
   THEN
      printf ("test failed as SIZE (pr) should be 1 not %d\n", SIZE (pr)) ;
      exit (1)
   END ;
   FOR global := MIN (subrange) TO MAX (subrange) DO
      test (global, 2)
   END ;
   FOR global := MIN (subrange) TO MAX (subrange) DO
      pr.bool := FALSE ;
      pr.sub := global ;
      test (pr.sub, 2)
   END
END packedrecord3.
