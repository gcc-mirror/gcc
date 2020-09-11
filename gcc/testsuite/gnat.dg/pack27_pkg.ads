pragma No_Component_Reordering;

package Pack27_Pkg is

  type Enum is (One, Two, Three);

  type Rec1 (D : Enum := One) is record
    case D is
      when One => null;
      when Two => null;
      when Three => C : Character;
    end case;
  end record;
  pragma Pack (Rec1);

  type Rec2 is record
    R : Rec1;
  end record;
  pragma Pack (Rec2);

  type Rec3 is record
    B : boolean;
    R : Rec2;
  end record;
  pragma Pack (Rec3);

  type Rec4 is record
    B : Boolean;
    R : Rec3;
  end record;
  pragma Pack (Rec4);

end Pack27_Pkg;
