-- { dg-do run }
-- { dg-options "-gnatws" }

with System; use System;

procedure Trampoline2 is

  A : Integer;

  type FuncPtr is access function (I : Integer) return Integer;

  function F (I : Integer) return Integer is
  begin
    return A + I;
  end F;

  P : FuncPtr := F'Access;
  CA : System.Address := F'Code_Address;
  I : Integer;

begin
  if CA = System.Null_Address then
    raise Program_Error;
  end if;

  I := P(0);
end;
