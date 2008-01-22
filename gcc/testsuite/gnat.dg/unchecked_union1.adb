-- { dg-do run }

procedure Unchecked_Union1 is

  type Bit is (Zero, One);

  type U (X : Bit := Zero) is record
    case X is
      when Zero => I: Integer;
      when One => F : Float;
    end case;
  end record;
  for U use record
    I at 0 range  0 .. 31;
    F at 0 range  0 .. 31;
  end record;
  pragma Unchecked_Union(U);

begin
  if U'Object_Size /= 32 then
    raise Program_Error;
  end if;
end;
