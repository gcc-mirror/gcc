--  { dg-do compile }

with Gen_Formal_Pkg_A, Gen_Formal_Pkg_B, Gen_Formal_Pkg_W;

procedure Gen_Formal_Pkg is
  package AI is new Gen_Formal_Pkg_A (Long_Float);
  package WI is new Gen_Formal_Pkg_W (AI);
begin
   null;
end;
