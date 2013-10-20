package Uninit_Array_Pkg Is

  type Rec is record
    B1, B2, B3, B4: Boolean;
  end record;

  type Arr is array (Boolean) of Rec;

  function F (R : Rec) return Integer;

end Uninit_Array_Pkg;
