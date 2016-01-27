-- { dg-do run }

with Init12; use Init12;
with Text_IO; use Text_IO;
with Dump;

procedure Q12 is

  A1  : Arr1  := My_A1;
  A11 : Arr11 := My_A11;

  A2  : Arr2  := My_A2;
  A22 : Arr22 := My_A22;

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

  if A1(1) /= A11(1,1) then
    raise Program_Error;
  end if;

  if A2(1) /= A22(1,1) then
    raise Program_Error;
  end if;
end;
