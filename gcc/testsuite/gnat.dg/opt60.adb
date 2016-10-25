-- { dg-do compile }
-- { dg-options "-gnatws -O2 -fdump-tree-optimized" }

with System; use System;
with System.CRTL; use System.CRTL;

function Opt60 (Size : size_t) return System.Address is
  Result : System.Address;
begin
  Result := malloc (Size);
  if Result = System.Null_Address then
    raise Program_Error;
  end if;
  return Result;
end;

-- { dg-final { scan-tree-dump "== 0B" "optimized" } }
