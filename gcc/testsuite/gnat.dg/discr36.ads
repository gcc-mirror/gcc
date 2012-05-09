package Discr36 is

  type R (D : Boolean := True) is record
    case D is
      when True  => I : Integer;
      when False => null;
    end case;
  end record;

  function N return Natural;

end Discr36;
