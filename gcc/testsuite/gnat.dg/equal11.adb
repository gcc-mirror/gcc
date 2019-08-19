--  { dg-do run }

with Equal11_Record;

procedure Equal11 is

  use Equal11_Record;

  R : My_Record_Type;
  L : My_Record_Type_List_Pck.List;
begin
  -- Single record
  R.F := 42;
  R.Put;
  if Put_Result /= 42 then
    raise Program_Error;
  end if;

  -- List of records
  L.Append ((F => 3));
  L.Append ((F => 2));
  L.Append ((F => 1));

  declare
    Expected : constant array (Positive range <>) of Integer :=
      (3, 2, 1);
    I : Positive := 1;
  begin
    for LR of L loop
      LR.Put;
      if Put_Result /= Expected (I) then
        raise Program_Error;
      end if;
      I := I + 1;
    end loop;
  end;
end Equal11;
