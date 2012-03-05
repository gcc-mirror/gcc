package Discr35 is

  type Rec1 is tagged null record;

  type Enum is (One, Two);

  type Rec2 (D : Enum := One) is
  record
    case D is
      when One => null;
      when Two => R : Rec1;
    end case;
  end record;

   Null_Rec2 : Constant Rec2;

   procedure Proc1;

   procedure Proc2;

private

   Null_Rec2 : Constant Rec2 := (D => One);

end Discr35;
