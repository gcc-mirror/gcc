with Opt99_Pkg2;

package Opt99_Pkg1 is

  type My_Character (D : Boolean := False) is record
    case D is
      when False => null;
      when True  => C : Character;
    end case;
  end record;

  type Derived is new Opt99_Pkg2.Root with record
    I : Integer;
    C1, C2 : My_Character;
  end record;

  procedure Set (D: in out Derived; C1, C2 : My_Character);

end Opt99_Pkg1;
