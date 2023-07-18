MODULE testnew6 ;

FROM Storage IMPORT ALLOCATE ;

TYPE
   PtrToVec = POINTER TO RECORD
                           x, y: INTEGER ;
                         END ;

PROCEDURE test ;
VAR
   p: PtrToVec ;
BEGIN
   NEW (p) ;
   WITH p^ DO
      x := 1 ;
      x := 2   (* Deliberate typo, user meant y.  *)
   END ;
   IF p^.y = 2
   THEN
   END
END test ;


BEGIN
   test
END testnew6.
