package Opt102_Pkg is

  type Enum is (One, Two, Three);

  function Get (E : Enum; F, M : access Integer) return Integer
    with Pre => (E = One) = (F = null and M = null) and
                (E = Two) = (F /= null) and
                (E = Three) = (M /= null);

end Opt102_Pkg;
