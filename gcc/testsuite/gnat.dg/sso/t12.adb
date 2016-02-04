-- { dg-do run }
-- 
with Init12; use Init12;
with Text_IO; use Text_IO;
with Dump;

procedure T12 is

  Local_A11 : Arr11;
  Local_A22 : Arr22;

begin
  Local_A11(1,1) := My_A11(1,1) + 1;
  Local_A11(1,2) := My_A11(1,2) + 1;
  Local_A11(2,1) := My_A11(2,1) + 1;
  Local_A11(2,2) := My_A11(2,2) + 1;

  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 13 00 ab 00 35 00 cd 00 13 00 ab 00 35 00 cd 00.*\n" }

  Local_A22(1,1) := My_A22(1,1) + 1;
  Local_A22(1,2) := My_A22(1,2) + 1;
  Local_A22(2,1) := My_A22(2,1) + 1;
  Local_A22(2,2) := My_A22(2,2) + 1;

  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 13 00 cd 00 35 00 ab 00 13 00 cd 00 35.*\n" }

  Local_A11 := (1 => (16#AB0012#, 16#CD0034#),
                2 => (16#AB0012#, 16#CD0034#));
  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Local_A22 := (1 => (16#AB0012#, 16#CD0034#),
                2 => (16#AB0012#, 16#CD0034#));
  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  Local_A11(1,1) := Local_A11(1,1) + 1;
  Local_A11(1,2) := Local_A11(1,2) + 1;
  Local_A11(2,1) := Local_A11(2,1) + 1;
  Local_A11(2,2) := Local_A11(2,2) + 1;

  Put ("Local_A11 :");
  Dump (Local_A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A11 : 13 00 ab 00 35 00 cd 00 13 00 ab 00 35 00 cd 00.*\n" }

  Local_A22(1,1) := Local_A22(1,1) + 1;
  Local_A22(1,2) := Local_A22(1,2) + 1;
  Local_A22(2,1) := Local_A22(2,1) + 1;
  Local_A22(2,2) := Local_A22(2,2) + 1;

  Put ("Local_A22 :");
  Dump (Local_A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_A22 : 00 ab 00 13 00 cd 00 35 00 ab 00 13 00 cd 00 35.*\n" }
end;
