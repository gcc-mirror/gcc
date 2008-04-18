package body Varsize_Return_Pkg1 is
  
  function Is_Fixed return Boolean is
  begin
    return True;
  end Is_Fixed;

  function Do_Item (I : Natural) return Variable_Data_Fixed_T is
    It : Variable_Data_Fixed_T;
  begin
    return It;
  end Do_Item;

  My_Db : Db.T;

  procedure Run is
    Kitem : Variable_Data_Fixed_T;
    I : Natural;
  begin
    Kitem := Db.Get (My_Db);
    Kitem := Do_Item (I);
  end Run;

end Varsize_Return_Pkg1;
