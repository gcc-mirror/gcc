-- { dg-do compile }

procedure Object_Overflow4 is

  procedure Proc (x : Integer) is begin null; end;

  type Index is new Long_Integer range 0 .. Long_Integer'Last;

  type Arr is array(Index range <>) of Integer;

  type Rec (Size: Index := 6) is record -- { dg-warning "Storage_Error" }
    A: Arr (0..Size);
  end record;

  Obj : Rec; -- { dg-warning "Storage_Error" }

begin
  Obj.A(1) := 0;
  Proc (Obj.A(1));
end;
