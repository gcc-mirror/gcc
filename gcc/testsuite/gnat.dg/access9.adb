--  { dg-do run }

procedure Access9 is

  type A_Type is access procedure;

  type B_Type is new A_Type;

  procedure Invoke (B : B_Type) is
  begin
    B.all;
  end;

  procedure Nested is begin null; end;

  A : A_Type := Nested'Access;

begin
  Invoke (B_Type (A));
end;