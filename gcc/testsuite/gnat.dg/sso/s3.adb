-- { dg-do run }

with Init3; use Init3;
with Text_IO; use Text_IO;
with Dump;

procedure S3 is

  A1 : R1 := My_R1;
  A2 : R2 := My_R2;

  N1 : Nested1;
  N2 : Nested2;

  C1 : Init3.Count;
  C2 : Init3.Count;
  C3 : Init3.Count;

begin

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  N1 := A1.N;
  C1 := N1.C1;
  C2 := N1.C2;
  C3 := N1.C3;

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 171.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 205.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 239.*\n" }

  N1.C1 := C1;
  N1.C2 := C2;
  N1.C3 := C3;
  A1.N := N1;

  N2 := A2.N;
  C1 := N2.C1;
  C2 := N2.C2;
  C3 := N2.C3;

  Put_Line("C1 :" & C1'Img);
  -- { dg-output "C1 : 171.*\n" }

  Put_Line("C2 :" & C2'Img);
  -- { dg-output "C2 : 205.*\n" }

  Put_Line("C3 :" & C3'Img);
  -- { dg-output "C3 : 239.*\n" }

  N2.C1 := C1;
  N2.C2 := C2;
  N2.C3 := C3;
  A2.N := N2;

  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 84 8d 15 9e 15 5b 35 df.*\n" }

end;
