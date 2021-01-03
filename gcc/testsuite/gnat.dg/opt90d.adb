-- { dg-do run }
-- { dg-options "-O2" }

with Ada.Calendar; use Ada.Calendar;
with Opt90d_Pkg; use Opt90d_Pkg;

procedure Opt90d is
  B : constant Integer := Year (Clock);
  V : Data;

begin
  V := (R => (A => 0, B => B, C => 0, D => "000000000000"));
  if V.R.B /= B then
    raise Program_Error;
  end if;
end;
