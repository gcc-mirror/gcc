-- { dg-do run }
-- { dg-options "-O2" }

with System;
with Opt21_Pkg; use Opt21_Pkg;

procedure Opt21 is
   V : System.Address := Convert (null);
begin
   null;
end;
