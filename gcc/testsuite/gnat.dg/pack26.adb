--  { dg-do run }

pragma Extend_System (Aux_DEC);

with System;

procedure Pack26 is

  type Bool_Array is array (1 .. 8) of Boolean;
  pragma pack (Bool_Array);

  All_True : Bool_Array := (others => True);
  Old_Value : Boolean := False;

begin

  System.Clear_Interlocked (All_True (2), Old_Value);

  if not Old_Value then
    raise Program_Error;
  end if;

end;