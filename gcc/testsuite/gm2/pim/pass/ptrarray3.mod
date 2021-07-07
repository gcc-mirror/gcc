MODULE ptarray3 ;

TYPE
  point = ARRAY [0..2] OF REAL;

PROCEDURE CalcPlane(VAR p : ARRAY OF point);
BEGIN
END CalcPlane;

PROCEDURE Calling;
VAR
   pts: ARRAY [0..3] OF point;
BEGIN
  CalcPlane(pts);
END Calling;

BEGIN
   Calling
END ptarray3.
