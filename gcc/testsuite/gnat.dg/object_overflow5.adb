-- { dg-do compile }

with Interfaces.C; use Interfaces.C;

procedure Object_Overflow5 is

  procedure Proc (c : Character) is begin null; end;

  type Index_T is new ptrdiff_t range 0 .. ptrdiff_t'Last;

  type Arr is array(Index_T range <>) of Character;

  type Rec (Size: Index_T := 6) is record -- { dg-warning "Storage_Error" }
    A: Arr (0..Size);
  end record;

  Obj : Rec; -- { dg-warning "Storage_Error" }

begin
  Obj.A(1) := 'a';
  Proc (Obj.A(1));
end;
