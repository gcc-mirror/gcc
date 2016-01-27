-- { dg-do run }

with Init5; use Init5;
with Text_IO; use Text_IO;
with Dump;

procedure S5 is

  A1 : R1 := My_R1;
  A2 : R2 := My_R2;

  AA1 : Arr1;
  AA2 : Arr2;

  C1 : Integer;
  C2 : Integer;
  C3 : Integer;

begin

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : 78 56 34 12 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 12 34 56 78 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  AA1 := A1.A;
  C1 := AA1(1);
  C2 := AA1(2);
  C3 := AA1(3);

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 11206674.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 13434932.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 15663190.*\n" }

  AA1(1) := C1;
  AA1(2) := C2;
  AA1(3) := C3;
  A1.A := AA1;

  AA2 := A2.A;
  C1 := AA2(1);
  C2 := AA2(2);
  C3 := AA2(3);

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 11206674.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 13434932.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 15663190.*\n" }

  AA2(1) := C1;
  AA2(2) := C2;
  AA2(3) := C3;
  A2.A := AA2;

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : 78 56 34 12 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 12 34 56 78 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }
end;
