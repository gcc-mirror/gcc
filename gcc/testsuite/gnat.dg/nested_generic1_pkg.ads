generic
package Nested_Generic1_Pkg is

   type Element_Renderer is access procedure;

   generic procedure Image_Generic (Renderer : in not null Element_Renderer);

end Nested_Generic1_Pkg;
