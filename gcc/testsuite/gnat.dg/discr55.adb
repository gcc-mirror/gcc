-- { dg-do run }

procedure Discr55 is

  type Rec (C : Character) is record
    case C is
      when 'Z' .. Character'Val (128) => I : Integer;
      when others                     => null;
    end case;
  end record;

  R : Rec ('Z');

begin
  R.I := 0;
end;
