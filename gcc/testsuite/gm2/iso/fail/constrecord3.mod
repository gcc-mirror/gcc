MODULE constrecord3 ;   (*!m2iso*)

TYPE
   VEC = RECORD
            x, y, z: REAL ;
         END ;

CONST
   VecConst = VEC {1.0, 2.0, 3.0} ;

BEGIN
   WITH VecConst DO
      y := 1.0
   END
END constrecord3.
