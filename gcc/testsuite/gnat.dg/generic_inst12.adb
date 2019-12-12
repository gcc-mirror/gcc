--  { dg-do run }
--  { dg-options "-O -gnatn" }
with Generic_Inst12_Pkg2;

procedure Generic_Inst12 is

  procedure My_Inner_G is new Generic_Inst12_Pkg2.Inner_G;

begin
  My_Inner_G (1);
  Generic_Inst12_Pkg2.Proc (1);
end;
