-- { dg-do run }

with Init4; use Init4;
with Ada.Numerics; use Ada.Numerics;
with Text_IO; use Text_IO;
with Dump;

procedure T4 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Local_R1.F := My_R1.F + 1.0;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : ee 87 84 40.*\n" }

  Local_R2.F := My_R2.F + 1.0;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 84 87 ee.*\n" }

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

  Local_R1.F := Local_R1.F + 1.0;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : ee 87 84 40.*\n" }

  Local_R2.F := Local_R2.F + 1.0;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 40 84 87 ee.*\n" }

end;
