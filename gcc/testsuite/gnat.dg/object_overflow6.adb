-- { dg-do compile }

with System;

procedure Object_Overflow6 is

  type Address is range 0 .. System.Memory_Size;

  type Arr is array (Address) of Character;

  A : Arr; -- { dg-warning "Storage_Error" }

begin
  A(1) := 'a';
end;
