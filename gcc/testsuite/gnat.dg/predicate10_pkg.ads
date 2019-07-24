package Predicate10_Pkg is
   type I_Array is array (Positive range <>) of access Integer;

   subtype NI_Array is I_Array with Dynamic_Predicate =>
     (for all I of NI_Array => I /= null);

   type I_Pointer is access Integer;

   procedure Foo (
     Length  : Natural;
     Initial : I_Pointer
   );
end;
