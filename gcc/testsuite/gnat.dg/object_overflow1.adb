-- { dg-do compile }

procedure Object_Overflow1 is

  procedure Proc (x : Boolean) is begin null; end;

  type Arr is array(Long_Integer) of Boolean;
  Obj : Arr; -- { dg-warning "Storage_Error" }

begin
  Obj(1) := True;
  Proc (Obj(1));
end;
