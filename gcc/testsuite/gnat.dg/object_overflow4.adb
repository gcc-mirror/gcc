-- { dg-do compile }

with Interfaces.C; use Interfaces.C;

procedure Object_Overflow4 is

  procedure Proc (x : Integer) is begin null; end;

  type Index_T is new ptrdiff_t range 0 .. ptrdiff_t'Last;

  type Arr is array(Index_T range <>) of Integer;

  type Rec (Size: Index_T := 6) is record -- { dg-warning "Storage_Error" }
    A: Arr (0..Size);
  end record;

  Obj : Rec; -- { dg-warning "Storage_Error" }

begin
  Obj.A(1) := 0;
  Proc (Obj.A(1));
end;
