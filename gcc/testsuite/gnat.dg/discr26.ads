with Discr26_Pkg;

package Discr26 is

  type T1 (D : Integer) is record
    case D is
      when 1 => I : Integer;
      when others => null;
    end case;
  end record;

  type My_T1 is new T1 (Discr26_Pkg.N);

  procedure Proc;

end Discr26;
