package Opt72_Pkg is

   type Rec is record
      Flag : Boolean;
      Size : Positive;
   end record;
   for Rec use record
      Flag at 0 range 0 .. 0;
      Size at 0 range 1 .. 31;
   end record;

end Opt72_Pkg;
