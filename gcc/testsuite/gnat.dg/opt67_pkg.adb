package body Opt67_Pkg is

  function Make_TSS_Name (Typ : Entity_Id; Nam : TNT) return Name_Id is
  begin
    return 0;
  end;

  function Stream_Operation_OK (N : Entity_Id; Name : TNT) return Boolean is
  begin
    return True;
  end;

  procedure Append_To (N1 : Natural; N2 : Node_Id) is
  begin
    null;
  end;

  function Predef (Loc : Source_Ptr; Name : Name_Id; E : Entity_Id)
    return Node_Id is
  begin
    return 0;
  end;

  function Init return Natural is
  begin
    return 0;
  end;

end Opt67_Pkg;
