package Discr21_Pkg is

  type Position is record
    x,y,z : Float;
  end record;

  type Dim is (Two, Three);

  type VPosition (D: Dim := Three) is record
    x, y : Float;
    case D is
      when Two => null;
      when Three => z : Float;
    end case;
  end record;

  function To_Position (x, y, z : Float) return VPosition;

end Discr21_Pkg;
