-- { dg-do run }

with System; use System;
with Deferred_Const3_Pkg; use Deferred_Const3_Pkg;

procedure Deferred_Const3 is
begin
  if C1'Address /= C'Address then
    raise Program_Error;
  end if;

  if C2'Address /= C'Address then
    raise Program_Error;
  end if;

  if C3'Address /= C'Address then
    raise Program_Error;
  end if;
end;
