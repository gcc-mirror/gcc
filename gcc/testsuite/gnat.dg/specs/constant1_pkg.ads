package Constant1_Pkg is

  type Id_T is mod Natural'Last + 1;

  type Timer_Id_T is tagged record
    Id : Id_T := Id_T'Last;
  end record;

  Null_Timer_Id : constant Timer_Id_T := (Id => Id_T'Last - 1);

end Constant1_Pkg;
