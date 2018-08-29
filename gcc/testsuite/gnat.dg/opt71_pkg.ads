package Opt71_Pkg is

   type Rec is record
      Flag : Boolean;
      Size : Positive;
   end record;
   pragma Pack (Rec);

end Opt71_Pkg;
