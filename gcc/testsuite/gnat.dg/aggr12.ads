package Aggr12 is

  type Hair_Color_Type is (Black, Brown, Blonde, Grey, White, Red);

  type Rec is record
    I1, I2 : Hair_Color_Type;
  end record;

  A : constant Rec := (Black, Blonde);

  procedure Print (Data : String);

  procedure Test;

end Aggr12;
