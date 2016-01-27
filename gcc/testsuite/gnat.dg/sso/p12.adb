-- { dg-do run }

with Init12; use Init12;
with Text_IO; use Text_IO;
with Dump;

procedure P12 is

  Local_A1  : Arr1;
  Local_A11 : Arr11;
  Local_A2  : Arr2;
  Local_A22 : Arr22;

begin
  Put ("My_A1     :");
  Dump (My_A1'Address, Arr1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_A1     : 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("My_A11    :");
  Dump (My_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_A11    : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Put ("My_A2     :");
  Dump (My_A2'Address, Arr2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_A2     : 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Put ("My_A22    :");
  Dump (My_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_A22    : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  Local_A1 := My_A1;
  Put ("Local_A1  :");
  Dump (Local_A1'Address, Arr1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A1  : 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Local_A11 := My_A11;
  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Local_A2 := My_A2;
  Put ("Local_A2  :");
  Dump (Local_A2'Address, Arr2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A2  : 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Local_A22 := My_A22;
  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  Local_A1 := (16#AB0012#, 16#CD0034#, 16#EF0056#);
  Put ("Local_A1  :");
  Dump (Local_A1'Address, Arr1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A1  : 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Local_A11 := (1 => (16#AB0012#, 16#CD0034#),
                2 => (16#AB0012#, 16#CD0034#));
  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Local_A2 := (16#AB0012#, 16#CD0034#, 16#EF0056#);
  Put ("Local_A2  :");
  Dump (Local_A2'Address, Arr2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A2  : 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Local_A22 := (1 => (16#AB0012#, 16#CD0034#),
                2 => (16#AB0012#, 16#CD0034#));
  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  Local_A1(1) := Local_A2(1);
  Local_A1(2) := Local_A2(2);
  Local_A1(3) := Local_A2(3);

  Put ("Local_A1  :");
  Dump (Local_A1'Address, Arr1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A1  : 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Local_A11(1,1) := Local_A22(1,1);
  Local_A11(1,2) := Local_A22(1,2);
  Local_A11(2,1) := Local_A22(2,1);
  Local_A11(2,2) := Local_A22(2,2);

  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Local_A2(1) := Local_A1(1);
  Local_A2(2) := Local_A1(2);
  Local_A2(3) := Local_A1(3);

  Put ("Local_A2  :");
  Dump (Local_A2'Address, Arr2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A2  : 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Local_A22(1,1) := Local_A11(1,1);
  Local_A22(1,2) := Local_A11(1,2);
  Local_A22(2,1) := Local_A11(2,1);
  Local_A22(2,2) := Local_A11(2,2);

  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }
end;
