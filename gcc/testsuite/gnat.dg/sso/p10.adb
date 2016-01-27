-- { dg-do run }
-- { dg-options "-gnatws" }

with Init10; use Init10;
with Text_IO; use Text_IO;
with Dump;

procedure P10 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Put ("My_R1    :");
  Dump (My_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R1    : 78 56 34 12.*\n" }

  Put ("My_R2    :");
  Dump (My_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R2    : 12 34 56 78.*\n" }

  Local_R1 := My_R1;
  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 78 56 34 12.*\n" }

  Local_R2 := My_R2;
  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 78.*\n" }

  Local_R1.I := 16#12345678#;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 78 56 34 12.*\n" }

  Local_R2.I := 16#12345678#;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 78.*\n" }

  Local_R1.I := Local_R2.I;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 78 56 34 12.*\n" }

  Local_R2.I := Local_R1.I;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 78.*\n" }
end;
