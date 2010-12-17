-- { dg-do compile }
-- { dg-options "-O1 -gnatp -gnatn" }

with Dse_Step; use Dse_Step;

procedure Test_Dse_Step is
   Start : My_Counter := (Value => 0, Step => 1);
   Steps : Natural := Nsteps;
begin
   Step_From (Start);
   if Mv /= Steps then
      raise Program_Error;
   end if;
end;
