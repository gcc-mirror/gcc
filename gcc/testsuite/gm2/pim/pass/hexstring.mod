MODULE hexstring ;  

CONST
   HexDigits = "0123456789ABCDEF" ;

TYPE
   ArrayType = ARRAY [0..HIGH (HexDigits)] OF CHAR ;

CONST
   HexArray = ArrayType { HexDigits } ;

VAR
   four: CHAR ;
BEGIN
   four := HexArray[4]
END hexstring.
