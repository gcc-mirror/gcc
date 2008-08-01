with System; use System;

package body Deferred_Const2_Pkg is

  procedure Dummy is begin null; end;

begin
  if S'Address /= I'Address then
    raise Program_Error;
  end if;
end Deferred_Const2_Pkg;
