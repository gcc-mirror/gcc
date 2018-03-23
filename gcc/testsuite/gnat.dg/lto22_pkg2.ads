package Lto22_Pkg2 is  

   subtype Index_Type is Integer range 1 .. 20;

   type Rec (<>) is private;           

   function F return Rec;

private

   type Rec (D : Index_Type := 2) is record     
      S : String (1 .. D) := "Hi";
   end record;

end Lto22_Pkg2;
