--  { dg-do compile }
--  { dg-options "-gnatws" }

procedure Fixedpnt3 is
  C_Unit : constant := 0.001;

  type T_Fixed_Point is
     delta C_Unit range (-2 ** 63) * C_Unit .. (2 ** 63 - 1) * C_Unit
     with Size  => 64, Small => C_Unit;

  type T_Short_Fixed_Point is
     new T_Fixed_Point range (-2 ** 31) * C_Unit .. (2 ** 31 - 1) * C_Unit
     with Size  => 32;
begin
   null;
end Fixedpnt3;
