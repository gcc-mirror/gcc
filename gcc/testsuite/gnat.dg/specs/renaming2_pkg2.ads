package Renaming2_Pkg2 is

  type Root is private;

private

  type Root (D : Boolean := False) is record
    case D is
      when True => N : Natural;
      when False => null;
    end case;
  end record;

end Renaming2_Pkg2;
