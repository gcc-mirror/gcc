-- { dg-do run }

with Init12; use Init12;
with Text_IO; use Text_IO;
with Dump;

procedure R12 is

  function Get_Elem (A : Arr1) return Integer is
    Tmp : Arr1 := A;
  begin
    return Tmp(1);
  end;

  procedure Set_Elem (A : access Arr1; I : Integer) is
    Tmp : Arr1 := A.all;
  begin
    Tmp(1) := I;
    A.all := Tmp;
  end;

  function Get_Elem (A : Arr11) return Integer is
    Tmp : Arr11 := A;
  begin
    return Tmp(1,1);
  end;

  procedure Set_Elem (A : access Arr11; I : Integer) is
    Tmp : Arr11 := A.all;
  begin
    Tmp(1,1) := I;
    A.all := Tmp;
  end;

  function Get_Elem (A : Arr2) return Integer is
    Tmp : Arr2 := A;
  begin
    return Tmp(1);
  end;

  procedure Set_Elem (A : access Arr2; I : Integer) is
    Tmp : Arr2 := A.all;
  begin
    Tmp(1) := I;
    A.all := Tmp;
  end;

  function Get_Elem (A : Arr22) return Integer is
    Tmp : Arr22 := A;
  begin
    return Tmp(1,1);
  end;

  procedure Set_Elem (A : access Arr22; I : Integer) is
    Tmp : Arr22 := A.all;
  begin
    Tmp(1,1) := I;
    A.all := Tmp;
  end;

  A1  : aliased Arr1  := My_A1;
  A11 : aliased Arr11 := My_A11;

  A2  : aliased Arr2  := My_A2;
  A22 : aliased Arr22 := My_A22;

begin
  Put ("A1  :");
  Dump (A1'Address, Arr1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1  : 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("A11 :");
  Dump (A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Put ("A2  :");
  Dump (A2'Address, Arr2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2  : 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Put ("A22 :");
  Dump (A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  if Get_Elem (A1) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A1'Access, 16#CD0034#);
  if Get_Elem (A1) /= 16#CD0034# then
    raise Program_Error;
  end if;

  if Get_Elem (A11) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A11'Access, 16#CD0034#);
  if Get_Elem (A11) /= 16#CD0034# then
    raise Program_Error;
  end if;

  if Get_Elem (A2) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A2'Access, 16#CD0034#);
  if Get_Elem (A2) /= 16#CD0034# then
    raise Program_Error;
  end if;

  if Get_Elem (A22) /= 16#AB0012# then
    raise Program_Error;
  end if;

  Set_Elem (A22'Access, 16#CD0034#);
  if Get_Elem (A22) /= 16#CD0034# then
    raise Program_Error;
  end if;
end;
