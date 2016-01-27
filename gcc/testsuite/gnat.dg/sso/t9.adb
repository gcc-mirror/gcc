-- { dg-do run }

with Init9; use Init9;
with Ada.Numerics; use Ada.Numerics;
with Text_IO; use Text_IO;
with Dump;

procedure T9 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Local_R1.F := My_R1.F + 1.0;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 8c 16 22 aa fd 90 10 40.*\n" }

  Local_R2.F := My_R2.F + 1.0;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 10 90 fd aa 22 16 8c.*\n" }

  Local_R1.F := Pi;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 18 2d 44 54 fb 21 09 40.*\n" }

  Local_R2.F := Pi;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 09 21 fb 54 44 2d 18.*\n" }

  Local_R1.F := Local_R1.F + 1.0;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 8c 16 22 aa fd 90 10 40.*\n" }

  Local_R2.F := Local_R2.F + 1.0;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 10 90 fd aa 22 16 8c.*\n" }

end;
