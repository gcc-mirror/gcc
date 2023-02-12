MODULE constarray2 ;

TYPE
   VEC = ARRAY [0..2] OF REAL;

CONST
   VecConst = VEC {1.0, 2.0, 3.0};

BEGIN
   VecConst := VEC {2.0, 3.0, 4.0}
END constarray2.
