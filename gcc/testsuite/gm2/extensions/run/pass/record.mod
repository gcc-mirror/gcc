MODULE record ;

FROM SYSTEM IMPORT CARDINAL16;
FROM libc IMPORT exit ;

TYPE
  InOut = RECORD
             in  : CARDINAL16;
             in2 : CARDINAL16; (* remove this and it works?! *)
             out : CARDINAL;
          END ;

VAR
   io : InOut;
BEGIN
   io.in:=1718;
   io.in2:=198; (* or set in2 to 0 and it works *)

   io.out:=io.in;
   IF io.out#io.in
   THEN
      exit(1)
   END
END record.
