package Inline1_Pkg is

   function Valid_Real (Number : Float) return Boolean;
   pragma Inline_Always (Valid_Real);

   function Invalid_Real return Float;
   pragma Inline_Always (Invalid_Real);

end Inline1_Pkg;
