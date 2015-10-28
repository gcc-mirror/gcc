-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Discr44 is

  function Ident (I : Integer) return Integer is
  begin
    return I;
  end;

  type Int is range 1 .. 10;

  type Str is array (Int range <>) of Character;

  type Parent (D1, D2 : Int; B : Boolean) is record
    S : Str (D1 .. D2);
  end record;

  type Derived (D : Int) is new Parent (D1 => D, D2 => D, B => False);

  X1 : Derived (D => Int (Ident (7)));

begin
  if X1.D /= 7 then
    raise Program_Error;
  end if;
end;
