-- { dg-do run }

with Init4; use Init4;
with Ada.Numerics; use Ada.Numerics;
with Text_IO; use Text_IO;
with Dump;

procedure Q4 is

  A1 : R1 := My_R1;
  B1 : R1 := My_R1;

  A2 : R2 := My_R2;
  B2 : R2 := My_R2;

begin
  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : db 0f 49 40.*\n" }

  Put ("B1 :");
  Dump (B1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B1 : db 0f 49 40.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 40 49 0f db.*\n" }

  Put ("B2 :");
  Dump (B2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B2 : 40 49 0f db.*\n" }

  if A1.F /= B1.F then
    raise Program_Error;
  end if;

  if A1.F /= Pi then
    raise Program_Error;
  end if;

  if A2.F /= B2.F then
    raise Program_Error;
  end if;

  if A2.F /= Pi then
    raise Program_Error;
  end if;

end;
