MODULE highexp ;


VAR
   a: ARRAY [0..9] OF CHAR ;
   c: CARDINAL ;
BEGIN
   c := 1 + HIGH (a)
END highexp.
