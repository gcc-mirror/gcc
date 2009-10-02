-- { dg-do compile }

procedure Object_Overflow is

  procedure Proc (x : Boolean) is begin null; end;

  type Arr is array(Long_Integer) of Boolean;
  Obj : Arr; -- { dg-warning "Storage_Error will be raised" }

begin
  Obj(1) := True;
  Proc (Obj(1));
end;
