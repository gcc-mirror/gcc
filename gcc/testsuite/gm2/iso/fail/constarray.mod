MODULE constarray ;

TYPE
   VEC = ARRAY [0..2] OF REAL;

CONST
   VecConst = VEC {1.0, 2.0, 3.0};

BEGIN
   VecConst[1] := 1.0
END constarray.
