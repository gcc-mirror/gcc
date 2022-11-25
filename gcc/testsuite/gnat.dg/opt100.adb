-- { dg-do run }
-- { dg-options "-O2 -gnatp" }

with Opt100_Pkg; use Opt100_Pkg;

procedure Opt100 is
  R : constant Rec := (K => B, N => 1);

begin
  if Func (R) /= 1 then
     raise Program_Error;
  end if;
end;
