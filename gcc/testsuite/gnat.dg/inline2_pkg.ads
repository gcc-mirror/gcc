package Inline2_Pkg is

   function Valid_Real (Number : Float) return Boolean;
   pragma Inline (Valid_Real);

   function Invalid_Real return Float;
   pragma Inline (Invalid_Real);

end Inline2_Pkg;
