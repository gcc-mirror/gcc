-- { dg-do compile }

with System;

procedure Array11 is

  type Rec is null record;
  type Index_T is mod System.Memory_Size;

  type Arr1 is array (1 .. 8) of aliased Rec; -- { dg-warning "padded" }
  type Arr2 is array (Index_T) of aliased Rec; -- { dg-warning "padded" }

  A1 : Arr1;
  A2 : Arr2;

begin
  null;
end;
