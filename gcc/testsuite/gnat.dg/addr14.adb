--  { dg-do run }

with System;

procedure Addr14 is

  type Arr is array (1 .. 4) of aliased Integer;

  A : Arr := (1, 2, 3, 4);
  I : Natural := 0;

  function Get_Address return System.Address is
  begin
    I := I + 1;
    return A(I)'Address;
  end;

  Foo : Integer with Address => Get_Address;

begin
  if Foo /= 1 then
    raise Program_Error;
  end if;
end;
