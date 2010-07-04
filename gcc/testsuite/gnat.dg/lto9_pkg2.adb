package body Lto9_Pkg2 is

   procedure Put (List     : in out List_Type;
                  Elem_Ptr : in     Element_Ptr;
                  Location : in     Index) is
   begin
      List.Elements(Location) := Elem_Ptr;
   end Put;

end Lto9_Pkg2;
