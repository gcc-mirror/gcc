--  { dg-do compile }
--  { dg-options "-O -gnatn" }

with Generic_Inst11_Pkg;

procedure Generic_Inst11 is
begin
   Generic_Inst11_Pkg.Proc;
end;
