MODULE testrotate ;  

IMPORT SYSTEM;

VAR
  v: PACKEDSET OF [0..31];
  i: INTEGER;
BEGIN
  i := 3;
  v := SYSTEM.ROTATE (v, i);
END testrotate.
