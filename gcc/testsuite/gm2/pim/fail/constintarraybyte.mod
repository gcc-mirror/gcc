MODULE constintarraybyte ;  

FROM FormatStrings IMPORT Sprintf1 ;
FROM DynamicStrings IMPORT String, InitString ;

VAR
   s: String ;
BEGIN
   s := Sprintf1 (InitString("abc%x\n"), 42)
END constintarraybyte.
