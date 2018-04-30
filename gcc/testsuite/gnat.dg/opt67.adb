-- { dg-do run }
-- { dg-options "-O" }

with Opt67_Pkg; use Opt67_Pkg;

procedure Opt67 is

  function Predef_Stream_Attr_Spec
    (Loc      : Source_Ptr;
     Tag_Typ  : Entity_Id;
     Name     : TSS_Name_Type) return Node_Id is
  begin
    return Predef (Loc, Make_TSS_Name (Tag_Typ, Name), Tag_Typ);
  end;

  Stream_Op_TSS_Names :
    constant array (Integer range <>) of TSS_Name_Type :=
      (TSS_Stream_Read,
       TSS_Stream_Write,
       TSS_Stream_Input,
       TSS_Stream_Output);

  Tag_Typ : constant Entity_Id := Entity_Id(Init);
  Res : constant Natural := Init;
  Loc : constant Source_Ptr := Source_Ptr(Init);

begin
  for Op in Stream_Op_TSS_Names'Range loop
    if Stream_Operation_OK (Tag_Typ, Stream_Op_TSS_Names (Op)) then
      Append_To (Res,
         Predef_Stream_Attr_Spec (Loc, Tag_Typ,
                                  Stream_Op_TSS_Names (Op)));
    end if;
  end loop;
end;
