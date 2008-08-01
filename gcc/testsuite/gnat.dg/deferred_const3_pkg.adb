with System; use System;

package body Deferred_Const3_Pkg is

  procedure Dummy is begin null; end;

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
end Deferred_Const3_Pkg;
