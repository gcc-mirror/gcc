package Ghost3 is
   type Small_Int is new Natural range 0 .. 5;
   type Large_Int is new Natural range 0 .. 5000;

   type Rec_Typ is record
      Comp_1 : Small_Int;
      Comp_2 : Large_Int;
   end record;

   generic
      type Any_Typ;
   package Gen is
   end Gen;

   package Freezer with Ghost is
      package Inst is new Gen (Rec_Typ);
   end Freezer;

   procedure Dummy;
end Ghost3;
