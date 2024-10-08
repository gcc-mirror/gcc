-- { dg-do run }
-- { dg-options "-gnat2022" }

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Aggr31 is

  My_Array : constant array (1 .. 5) of Unbounded_String :=
    [others => To_Unbounded_String ("Test")];

  This_Crashes : constant array (Natural range <>) of Unbounded_String :=
    [for I of My_Array => I];

begin
  null;
end;

