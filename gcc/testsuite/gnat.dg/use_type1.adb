-- { dg-do compile }

procedure Use_Type1 is

  package Nested is
    type T is (X, Y, Z);
    procedure Proc (Obj : T) is null;
  end Nested;

  use all type Nested.T;

  Obj : Nested.T := X;

begin
  Proc (Obj);
end;
