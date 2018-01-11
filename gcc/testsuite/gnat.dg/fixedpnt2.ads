package Fixedpnt2 is

   type T_Integer_32 is range -2 ** 31 .. 2 ** 31 - 1
      with Size => 32;

   type T_Integer_64 is range -2 ** 63 .. 2 ** 63 - 1
      with Size => 64;

   C_Unit  : constant := 0.001; -- One millisecond.
   C_First : constant := (-2 ** 63) * C_Unit;
   C_Last  : constant := (2 ** 63 - 1) * C_Unit;

   type My_Type is
      delta C_Unit range C_First .. C_Last
      with Size  => 64,
           Small => C_Unit;

   function "*" (Left : in T_Integer_32; Right : in My_Type)
     return My_Type;
   function "*" (Left : in My_Type;      Right : in T_Integer_32)
     return My_Type;

end Fixedpnt2;
