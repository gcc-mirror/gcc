--  { dg-do compile }
--  { dg-options "-O -gnatn" }

with Inline21_Q;

procedure Inline21 is
begin
  Inline21_Q.My_Nested_G.Proc;
end;
