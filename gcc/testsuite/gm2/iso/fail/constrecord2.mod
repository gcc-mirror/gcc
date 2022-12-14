MODULE constrecord2 ;   (*!m2iso*)

TYPE
   VEC = RECORD
            x, y, z: REAL ;
         END ;

CONST
   VecConst = VEC {1.0, 2.0, 3.0} ;

BEGIN
   VecConst := VEC {2.0, 3.0, 4.0}
END constrecord2.
