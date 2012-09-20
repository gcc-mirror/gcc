with Array16_Pkg;

package Array16 is

  type T1 (D : Integer) is record
    case D is
      when 1 => I : Integer;
      when others => null;
    end case;
  end record;

  type Arr is array (Integer range <>) of Integer;

  type My_T1 is new T1 (Array16_Pkg.N);
  type My_T2 is new Arr (1 .. Integer'Min (2, Array16_Pkg.N));

  function F1 (A : access My_T1) return My_T1;
  pragma Inline (F1);

  function F2 (A : access My_T2) return My_T2;
  pragma Inline (F2);

  procedure Proc (A : access My_T1; B : access My_T2);

end Array16;
