-- { dg-do run }

with Init4; use Init4;
with Ada.Numerics; use Ada.Numerics;
with Text_IO; use Text_IO;
with Dump;

procedure P4 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Put ("My_R1    :");
  Dump (My_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R1    : db 0f 49 40.*\n" }

  Put ("My_R2    :");
  Dump (My_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R2    : 40 49 0f db.*\n" }

  Local_R1 := My_R1;
  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : db 0f 49 40.*\n" }

  Local_R2 := My_R2;
  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 49 0f db.*\n" }

  Local_R1.F := Pi;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : db 0f 49 40.*\n" }

  Local_R2.F := Pi;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 49 0f db.*\n" }

  Local_R1.F := Local_R2.F;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : db 0f 49 40.*\n" }

  Local_R2.F := Local_R1.F;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 49 0f db.*\n" }
end;
