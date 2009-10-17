-- { dg-do compile }
-- { dg-options "-gnatws" }

with Slice8_Pkg1;
with Slice8_Pkg3;

procedure Slice8 is

   package Bp is new Slice8_Pkg3 (Slice8_Pkg1);

begin
   null;
end;
