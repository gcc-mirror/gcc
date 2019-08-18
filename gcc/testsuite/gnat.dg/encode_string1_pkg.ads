generic
   type C is private;
   type S is array (Positive range <>) of C;
   with function Encode (Val : S) return String;

procedure Encode_String1_Pkg (Val : S);
