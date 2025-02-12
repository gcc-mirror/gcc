MODULE testbitfns ;  

FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT clz, clzll, ctz, ctzll ;


(*
   Assert - 
*)

PROCEDURE Assert (name: ARRAY OF CHAR;
                  input, left, right: INTEGER; line: CARDINAL) ;
BEGIN
   IF left = right
   THEN
      printf ("pass: %s (%d) = %d\n", name, input, left)
   ELSE
      printf ("fail: %s (%d) # %d (should be %d)\n", name, input, left, right) ;
      exitcode := 1
   END
END Assert ;


(*
   runtests - 
*)

PROCEDURE runtests ;
VAR
   c     : CARDINAL ;
   l     : LONGCARD ;
   result: INTEGER ;
BEGIN
   c := 16 ;
   result := ctz (c) ;
   Assert ("ctz", c, result, 4, __LINE__) ;
   c := 4 ;
   result := ctz (c) ;   
   Assert ("ctz", c, result, 2, __LINE__) ;   
   c := 14 ;
   result := ctz (c) ;   
   Assert ("ctz", c, result, 1, __LINE__) ; 
   l := 16 ;
   result := ctzll (l) ;   
   Assert ("ctzll", l, result, 4, __LINE__) ;
   l := 4 ;
   result := ctzll (l) ;   
   Assert ("ctzll", l, result, 2, __LINE__) ;   
   l := 14 ;
   result := ctzll (l) ;   
   Assert ("ctzll", l, result, 1, __LINE__) ;

   IF SIZE (c) = 4
   THEN
      c := 16 ;
      result := clz (c) ;
      Assert ("clz", c, result, 27, __LINE__) ;
      c := 4 ;
      result := clz (c) ;   
      Assert ("clz", c, result, 29, __LINE__) ;   
      c := 14 ;
      result := clz (c) ;   
      Assert ("clz", c, result, 28, __LINE__) ;
      IF SIZE (l) = 8
      THEN
         l := 16 ;
         result := clzll (l) ;   
         Assert ("clzll", l, result, 59, __LINE__) ;
         l := 4 ;
         result := clzll (l) ;   
         Assert ("clzll", l, result, 61, __LINE__) ;   
         l := 14 ;
         result := clzll (l) ;   
         Assert ("ctzll", l, result, 60, __LINE__)
      END
   END
END runtests ;


VAR
   exitcode: INTEGER ;
BEGIN
   exitcode := 0 ;
   runtests ;
   IF exitcode = 0
   THEN
      printf ("all tests pass!\n")
   END ;
   exit (exitcode)
END testbitfns.
