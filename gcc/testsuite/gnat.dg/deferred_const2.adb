-- { dg-do run }

with System; use System;
with Deferred_Const2_Pkg; use Deferred_Const2_Pkg;

procedure Deferred_Const2 is
begin
  if I'Address /= S'Address then
    raise Program_Error;
  end if;
end;
