-- { dg-do run }

with Init12; use Init12;
with Text_IO; use Text_IO;
with Dump;

procedure S12 is

  A11 : Arr11 := My_A11;
  A22 : Arr22 := My_A22;

  A1 : Arr1;
  A2 : Arr2;

  C1 : Integer;
  C2 : Integer;
  C3 : Integer;

begin
  Put ("A11 :");
  Dump (A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Put ("A22 :");
  Dump (A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }

  A1 := (A11(1,1), A11(1,2), A11(2,1));
  C1 := A1(1);
  C2 := A1(2);
  C3 := A1(3);

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 11206674.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 13434932.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 11206674.*\n" }

  A1(1) := C1;
  A1(2) := C2;
  A1(3) := C3;
  A11(1,1) := A1(1); A11(1,2) := A1(2); A11(2,1) := A1(3);

  A2 := (A22(1,1), A22(1,2), A22(2,1));
  C1 := A2(1);
  C2 := A2(2);
  C3 := A2(3);

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 11206674.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 13434932.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 11206674.*\n" }

  A2(1) := C1;
  A2(2) := C2;
  A2(3) := C3;
  A22(1,1) := A2(1); A22(1,2) := A2(2); A22(2,1) := A2(3);

  Put ("A11 :");
  Dump (A11'Address, Arr11'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A11 : 12 00 ab 00 34 00 cd 00 12 00 ab 00 34 00 cd 00.*\n" }

  Put ("A22 :");
  Dump (A22'Address, Arr22'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A22 : 00 ab 00 12 00 cd 00 34 00 ab 00 12 00 cd 00 34.*\n" }
end;
