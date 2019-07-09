package body Predicate10_Pkg is
   procedure Foo (
     Length  : Natural;
     Initial : I_Pointer
   ) is
      A : NI_Array  := (1 .. Length => Initial);
   begin
      null;
   end Foo;
end;
