package Opt62_Pkg is

   Default_String : constant String := "This is a default string";

   subtype Length is Natural range 0..255;

   type Root (D1 : Length) is tagged  record
      S1  : String(1..D1) := Default_String(1..D1);
   end record;

   type Unconstrained_Der is new Root with  record
      Str1 : String(1..5) := "abcde";
   end record;

   type Der (D2 : Length) is new Unconstrained_Der (D1 => 10) with record
      S2 : String(1..D2);
   end record;

end Opt62_Pkg;
