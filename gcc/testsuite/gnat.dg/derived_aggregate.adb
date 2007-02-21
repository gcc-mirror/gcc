-- { dg-do run }
-- { dg-options "-O2" }

procedure Derived_Aggregate is
  type Int is range 1 .. 10;
  type Str is array (Int range <>) of Character;

  type Parent (D1, D2 : Int; B : Boolean) is
    record
      S : Str (D1 .. D2);
      case B is
        when False => C1 : Integer;
        when True =>  C2 : Float;
      end case;
    end record;

  for Parent'Alignment use 8;

  type Derived (D : Int) is new Parent (D1 => D, D2 => D, B => False);

  function Ident (I : Integer) return integer is
  begin
     return I;
  end;

  Y : Derived := (D => 7, S => "b", C1 => Ident (32));

begin
  if Parent(Y).D1 /= 7 then
    raise Program_Error;
  end if;
end;
