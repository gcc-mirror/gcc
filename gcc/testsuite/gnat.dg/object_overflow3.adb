-- { dg-do compile }

with Interfaces.C; use Interfaces.C;

procedure Object_Overflow3 is

  procedure Proc (x : Boolean) is begin null; end;

  type Arr is array(0 .. ptrdiff_t'Last) of Boolean;

  type Rec is record
    A : Arr;
    B : Arr;
  end record;

  Obj : Rec; -- { dg-warning "Storage_Error" }

begin
  Obj.A(1) := True;
  Proc (Obj.A(1));
end;
