--  { dg-do compile }
--  { dg-options "-O -gnatn2" }
with Inline20_Q.IO;
with Inline20_R;

procedure Inline20 is
begin
   Inline20_R.Log (Inline20_Q.IO.F);
end;
