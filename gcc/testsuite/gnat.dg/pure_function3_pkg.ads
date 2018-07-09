package Pure_Function3_Pkg is

   type T is limited private;
   function F (Self : T) return Integer with Pure_Function;
   procedure Set (Self : in out T);
   function F_And_Set (Self : in out T) return Integer with Pure_Function;

private

   type T is limited record
      F : Integer;
   end record;

end Pure_Function3_Pkg;
