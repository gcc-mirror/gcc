package Discr32_Pkg is

  type Enum is (One, Two, Three);

  type R (D : Enum) is record
    case D is
      when One   => B : Boolean;
      when Two   => I : Integer;
      when Three => F : Float;
    end case;
  end record;

  for R use record
     D at 0 range 0 .. 1;
     B at 1 range 0 .. 0;
     I at 4 range 0 .. 31 + 128;
--     F at 4 range 0 .. 31;
  end record;

  subtype R1 is R (One);
  subtype R2 is R (Two);
  subtype R3 is R (Three);

end Discr32_Pkg;
