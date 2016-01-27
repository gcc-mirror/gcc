-- { dg-do run }

with Init6; use Init6;
with Text_IO; use Text_IO;
with Dump;

procedure Q6 is

  A1 : R1 := My_R1;
  B1 : R1 := My_R1;

  A2 : R2 := My_R2;
  B2 : R2 := My_R2;

begin
  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : 78 56 34 12 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Put ("B1 :");
  Dump (B1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B1 : 78 56 34 12 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 12 34 56 78 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("B2 :");
  Dump (B2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B2 : 12 34 56 78 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  if A1.I /= B1.I or A1.A(1) /= B1.A(1) then
    raise Program_Error;
  end if;

  if A2.I /= B2.I or A2.A(1) /= B2.A(1) then
    raise Program_Error;
  end if;

end;
