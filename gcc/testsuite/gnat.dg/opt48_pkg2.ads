package Opt48_Pkg2 is

   pragma Pure;

   type Rec (L : Natural) is record
      S : String (1 .. L);
   end record;

   function F return Rec;

end Opt48_Pkg2;
