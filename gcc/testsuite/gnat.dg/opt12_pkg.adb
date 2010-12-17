package body Opt12_Pkg is

   function Equal (L, R: Static_Integer_Subtype) return Boolean is
   begin
      return (L = R);
   end;

end Opt12_Pkg;
