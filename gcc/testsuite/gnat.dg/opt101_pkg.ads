package Opt101_Pkg is

  type Int is mod 16;

  type Rec is record
    S : Short_Integer;
    I1, I2 : Int;
  end record;
  pragma Pack (Rec);
  for Rec'Alignment use 4;

  type Cont1 is record
    R : Rec;
    I1, I2 : Int;
  end record;
  pragma Pack (Cont1);

  type Cont2 is record
    I1 : Int;
    R  : Rec;
    I2 : Int;
  end record;
  pragma Pack (Cont2);
  pragma No_Component_Reordering (Cont2);

end Opt101_Pkg;
