--  { dg-do run }
--  { dg-options "-O2" }

with System;

procedure SSO18 is

  type Arr is array (1..32) of Short_Integer;
  type Rev_Arr is array (1..32) of Short_Integer
    with Scalar_Storage_Order => System.High_Order_First;
  C : constant Arr := (others => 16);
  RA : Rev_Arr;
  A  : Arr;

begin
  RA := Rev_Arr(C);
  A := Arr (RA);
  if A /= C or else RA(1) /= 16 then
     raise Program_Error;
  end if;
end;
