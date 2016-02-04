-- { dg-do run }

with Init7; use Init7;
with Text_IO; use Text_IO;
with Dump;

procedure R7 is

  function Get_Elem (R : R1) return Integer is
    Tmp : R1 := R;
  begin
    return Tmp.N.C1;
  end;

  procedure Set_Elem (R : access R1; I : Integer) is
    Tmp : R1 := R.all;
  begin
    Tmp.N.C1 := I;
    R.all := Tmp;
  end;

  function Get_Elem (R : R2) return Integer is
    Tmp : R2 := R;
  begin
    return Tmp.N.C1;
  end;

  procedure Set_Elem (R : access R2; I : Integer) is
    Tmp : R2 := R.all;
  begin
    Tmp.N.C1 := I;
    R.all := Tmp;
  end;

  A1 : aliased R1 := My_R1;
  A2 : aliased R2 := My_R2;

begin

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : 78 56 34 12 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 12 34 56 78 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  if Get_Elem (A1) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A1'Access, 16#CD0034#);
  if Get_Elem (A1) /= 16#CD0034# then
    raise Program_Error;
  end if;

  if Get_Elem (A2) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A2'Access, 16#CD0034#);
  if Get_Elem (A2) /= 16#CD0034# then
    raise Program_Error;
  end if;

end;
