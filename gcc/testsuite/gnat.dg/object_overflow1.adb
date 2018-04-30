-- { dg-do compile }

with Interfaces.C; use Interfaces.C;

procedure Object_Overflow1 is

  procedure Proc (x : Boolean) is begin null; end;

  type Arr is array(ptrdiff_t) of Boolean;
  Obj : Arr; -- { dg-warning "Storage_Error" }

begin
  Obj(1) := True;
  Proc (Obj(1));
end;
