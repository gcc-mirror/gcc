MODULE constrecord ;   (*!m2iso*)

TYPE
   VEC = RECORD
            x, y, z: REAL ;
         END ;

CONST
   VecConst = VEC {1.0, 2.0, 3.0} ;

BEGIN
   VecConst.y := 1.0
END constrecord.
