-- { dg-do compile }
-- { dg-options "-O" }

with Renaming7_Pkg; use Renaming7_Pkg;
with System;

procedure Renaming7 is
  C : constant System.Address := A'Address;
  D : System.Address renames C;
begin
  null;
end;
