-- { dg-do compile }
-- { dg-options "-O2 -gnatws" }

procedure Discr45 is

  function Ident_Int (I : Integer) return Integer is
  begin
    return I;
  end;

  procedure Proc (Signal : Boolean) is

    subtype Index is Integer range 1..10;

    type My_Arr is array (Index range <>) OF Integer;

    type Rec (D3 : Integer := Ident_Int(1)) is record
      case D3 is
        when -5..10 => C1 : My_Arr(D3..Ident_Int(11));
        when Others => C2 : Integer := Ident_Int(5);
      end case;
    end record;

    X : Rec;

    function Value return Rec;
    pragma No_Inline (Value);

    function Value return Rec is
    begin
      return X;
    end;

  begin
    if X /= Value then
      raise Constraint_Error;
    elsif Signal then
      raise Program_Error;
    end if;
  end;

begin
  Proc (True);
end;
