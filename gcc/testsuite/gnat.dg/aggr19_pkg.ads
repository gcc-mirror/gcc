package Aggr19_Pkg is

  type Rec1 (D : Boolean := False) is record
    case D is
      when False => null;
      when True => Pos : Integer;
    end case;
  end record;

  type Rec2 is record
    L : Rec1;
  end record;

  type Rec3 is tagged null record;

  type Enum is (One, Two, Three);

  type Rec4 (Kind : Enum := One) is record
    Node : Rec2;
    case Kind is
      when One => R : Rec3;
      when Others => I : Integer;
    end case;
  end record;

  type Rec5 is record
    Ent : Rec4;
  end record;

  procedure Proc (Pool : in out Rec5);

end Aggr19_Pkg;
