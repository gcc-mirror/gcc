-- { dg-do compile }

procedure Object_Overflow is

  type Rec is null record;

  procedure Proc (x : Rec) is begin null; end;

  type Arr is array(Long_Integer) of Rec;
  Obj : Arr; -- { dg-warning "Storage_Error will be raised" }

begin
  Proc (Obj(1));
end;
