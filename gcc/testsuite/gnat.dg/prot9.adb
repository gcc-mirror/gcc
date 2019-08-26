--  { dg-do compile }

with Prot9_Gen;
with Prot9_Pkg1;

procedure Prot9 is
   package Dummy is new Prot9_Gen (Prot9_Pkg1.Prot_Type);
begin
   null;
end Prot9;
