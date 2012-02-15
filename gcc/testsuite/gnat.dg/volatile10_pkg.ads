package Volatile10_Pkg is

   type Num is mod 2**9;

   type Rec is record
      B1  : Boolean;
      N1  : Num;
      B2  : Boolean;
      N2  : Num;
      B3  : Boolean;
      B4  : Boolean;
      B5  : Boolean;
      B6  : Boolean;
      B7  : Boolean;
      B8  : Boolean;
      B9  : Boolean;
      B10 : Boolean;
      B11 : Boolean;
      B12 : Boolean;
      B13 : Boolean;
      B14 : Boolean;
   end record;
   pragma Pack (Rec);
   for Rec'Size use 32;
   pragma Volatile(Rec);

   function F return Rec;

end Volatile10_Pkg;
