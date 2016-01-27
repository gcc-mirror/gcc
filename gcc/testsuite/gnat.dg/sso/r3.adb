-- { dg-do run }

with Init3; use Init3;
with Text_IO; use Text_IO;
with Dump;

procedure R3 is

  function Get_Elem (R : R1) return Integer is
    Tmp : R1 := R;
  begin
    return Tmp.I;
  end;

  procedure Set_Elem (R : access R1; I : Integer) is
    Tmp : R1 := R.all;
  begin
    Tmp.I := I;
    R.all := Tmp;
  end;

  function Get_Elem (R : R2) return Integer is
    Tmp : R2 := R;
  begin
    return Tmp.I;
  end;

  procedure Set_Elem (R : access R2; I : Integer) is
    Tmp : R2 := R.all;
  begin
    Tmp.I := I;
    R.all := Tmp;
  end;

  A1 : aliased R1 := My_R1;
  A2 : aliased R2 := My_R2;

begin

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  if Get_Elem (A1) /= 16#12345678# then
    raise Program_Error;
  end if;

  Set_Elem (A1'Access, 16#CD0034#);
  if Get_Elem (A1) /= 16#CD0034# then
    raise Program_Error;
  end if;

  if Get_Elem (A2) /= 16#12345678# then
    raise Program_Error;
  end if;

  Set_Elem (A2'Access, 16#CD0034#);
  if Get_Elem (A2) /= 16#CD0034# then
    raise Program_Error;
  end if;

end;
