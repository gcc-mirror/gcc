-- { dg-do compile }

procedure Array11 is

  type Rec is null record;
  type Ptr is access all Rec;

  type Arr1 is array (1..8) of aliased Rec; -- { dg-warning "padded" }
  type Arr2 is array (Long_Integer) of aliased Rec; -- { dg-warning "padded" }

  A1 : Arr1;
  A2 : Arr2; -- { dg-warning "Storage_Error will be raised" }

begin
  null;
end;
