--  { dg-do compile }
--  { dg-options "-gnatwa" }

with Warn20_Pkg;

procedure Warn20 is
   package P is new Warn20_Pkg (Integer, 0);
   pragma Unreferenced (P);
begin
   null;
end Warn20;
