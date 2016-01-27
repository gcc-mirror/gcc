-- { dg-do run }
-- { dg-options "-gnatws" }

with Init10; use Init10;
with Text_IO; use Text_IO;
with Dump;

procedure T10 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Local_R1.I := My_R1.I + 1;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 79 56 34 12.*\n" }

  Local_R2.I := My_R2.I + 1;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 79.*\n" }

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

  Local_R1.I := Local_R1.I + 1;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 79 56 34 12.*\n" }

  Local_R2.I := Local_R2.I + 1;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 79.*\n" }

end;
