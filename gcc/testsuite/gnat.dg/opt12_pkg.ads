package Opt12_Pkg is

   type Static_Integer_Subtype is range -32_000 .. 32_000;

   function Equal (L, R: Static_Integer_Subtype) return Boolean;

   type My_Fixed is delta 0.1 range -5.0 .. 5.0;

   Fix_Half : My_Fixed := 0.5;

end Opt12_Pkg;
