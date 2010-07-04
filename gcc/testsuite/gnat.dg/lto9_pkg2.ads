generic

   Size : in Positive;
   type Element_Type (<>) is private;
   type Element_Ptr       is access all Element_Type;

package Lto9_Pkg2 is

   subtype Index is Positive range 1 .. (Size + 1);

   type List_Array is array (Index) of Element_Ptr;

   type List_Type is record
      Elements : List_Array;
   end record;

   procedure Put (List     : in out List_Type;
                  Elem_Ptr : in     Element_Ptr;
                  Location : in     Index);

end Lto9_Pkg2;
