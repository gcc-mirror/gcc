-- { dg-do run }
-- { dg-options "-O -gnatws" }

procedure Discr47 is

  type Rec (D : Boolean := False) is record
    case D is
      when True => null;
      when False => C : Character;
    end case;
  end record;

  R : Rec;

begin
  if R'Size /= 16 then
    raise Program_Error;
  end if;
end;
