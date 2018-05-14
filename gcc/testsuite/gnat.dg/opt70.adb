-- { dg-do run }
-- { dg-options "-O2" }

with Ada.Calendar;
with Opt70_Pkg;

procedure Opt70 is
  T : Ada.Calendar.Time := Ada.Calendar.Time_Of (2001, 10, 31);
begin
  if Opt70_Pkg.Image (T, "%y") /= "01" then
    raise Program_Error;
  end if;
end;
