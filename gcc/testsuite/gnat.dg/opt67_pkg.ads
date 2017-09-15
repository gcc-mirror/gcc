package Opt67_Pkg is

  type Source_Ptr is new Natural;
  type Entity_Id is new Natural;
  type Node_Id is new Natural;
  type Name_Id is new Natural;

  type TSS_Name_Type is new String (1 .. 2);
  subtype TNT is TSS_Name_Type;

  TSS_Stream_Input       : constant TNT := "SI";
  TSS_Stream_Output      : constant TNT := "SO";
  TSS_Stream_Read        : constant TNT := "SR";
  TSS_Stream_Write       : constant TNT := "SW";
  TSS_To_Any             : constant TNT := "TA";

  function Make_TSS_Name (Typ : Entity_Id; Nam : TSS_Name_Type) return Name_Id;

  function Stream_Operation_OK (N : Entity_Id; Name : TNT) return Boolean;

  procedure Append_To (N1 : Natural; N2 : Node_Id);

  function Predef (Loc : Source_Ptr; Name : Name_Id; E : Entity_Id)
    return Node_Id;

  function Init return Natural;

end Opt67_Pkg;
