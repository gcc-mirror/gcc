package Loop_Optimization12 is

  type Enum1 is (A, B, C, D, E, F, G, H, I, J);

  type Enum2 is (A, B, C);

  type Enum3 is (A, B, C, D, E, F);

  type Enum4 is (A, B, C, D);

  type Enum5 is (A, B, C, D, E);

  type Arr is array (Enum3, Enum4, Enum4, Enum5, Enum5, Enum3,
                     Enum2, Enum3, Enum5, Enum3) of Natural;

  type Arr_Ptr is access Arr;
  type Ext_Arr is array (Enum1) of Arr_Ptr;

  type Rec is record
    F : Ext_Arr;
  end record;

  type Rec_Ptr is access Rec;

  procedure Reset (S : Rec_Ptr);

end Loop_Optimization12;
