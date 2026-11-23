package Access13 is

  type Acc is access procedure (B : Boolean);

  type Rec is record
    A : Acc;
  end record;

  Global_R : Rec;

  procedure Set_R (A : access procedure (B : Boolean));

  procedure Attach_Local_To_Global;

end Access13;
