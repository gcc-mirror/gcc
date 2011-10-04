-- { dg-do compile }
-- { dg-options "-O" }

procedure Opt19 is

  type Enum is (One, Two);

  type Vector_T is array (Enum) of Integer;

  Zero_Vector : constant Vector_T := (Enum => 0);

  type T is record
    Vector : Vector_T;
  end record;

  procedure Nested (Value : in out T; E : Enum; B : out Boolean) is
    I : Integer renames Value.Vector(E);
  begin
    B := I /= 0;
  end;

  Obj : T := (Vector => Zero_Vector);
  B : Boolean;

begin
  Nested (Obj, One, B);
end;
