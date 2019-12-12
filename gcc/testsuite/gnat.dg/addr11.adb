--  { dg-do run }

procedure Addr11 is

  type Rec is record
    I : Short_Integer;
    C : Character;
  end record;

  type Derived is new Rec;
  for Derived use record
    I at 1 range 0 .. 15;
    C at 0 range 0 .. 7;
  end record;

  Init : constant Rec := ( 1515, 'A' );

  D1 : Derived;
  D2 : Derived;
  pragma Volatile (D2);
  for D2'Address use D1'Address;

begin
  D2 := Derived (Init);
  if D1 /= Derived (Init) then
    raise Program_Error;
  end if;
end;
