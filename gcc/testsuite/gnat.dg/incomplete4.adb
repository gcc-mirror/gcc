-- { dg-do compile }

with Incomplete4_Pkg; use Incomplete4_Pkg;
with System;

procedure Incomplete4 is
  L : System.Address := A'Address;
begin
  null;
end;
