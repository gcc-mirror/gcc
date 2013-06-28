-- { dg-do compile }

procedure Derived_Type4 is

  type Root (D : Positive) is record
     S : String (1 .. D);
  end record;

  subtype Short is Positive range 1 .. 10;
  type Derived (N : Short := 1) is new Root (D => N);

  Obj : Derived;

begin
  Obj := (N => 5, S => "Hello");
end;
