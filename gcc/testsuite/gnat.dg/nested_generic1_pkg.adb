package body Nested_Generic1_Pkg is

   procedure Image_Generic
     (Renderer : in not null Element_Renderer) is
   begin
      null;
   end;

   procedure Image_Standard_Instance is new Image_Generic;

end Nested_Generic1_Pkg;
