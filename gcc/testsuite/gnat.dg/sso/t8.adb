-- { dg-do run }

with Init8; use Init8;
with Text_IO; use Text_IO;
with Dump;

procedure T8 is
  Verbose : constant Boolean := False;

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Local_R1.I    := My_R1.I + 1;
  Local_R1.N.C1 := My_R1.N.C1 + 1;
  Local_R1.N.C2 := My_R1.N.C2 + 1;
  Local_R1.N.C3 := My_R1.N.C3 + 1;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 79 56 34 12 00 ab 00 13 00 cd 00 35 00 ef 00 57.*\n" }

  Local_R2.I    := My_R2.I + 1;
  Local_R2.N.C1 := My_R2.N.C1 + 1;
  Local_R2.N.C2 := My_R2.N.C2 + 1;
  Local_R2.N.C3 := My_R2.N.C3 + 1;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 79 13 00 ab 00 35 00 cd 00 57 00 ef 00.*\n" }

  Local_R1 := (I => 16#12345678#,
               N => (16#AB0012#, 16#CD0034#, 16#EF0056#));
  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 78 56 34 12 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" }

  Local_R2 := (I => 16#12345678#,
               N => (16#AB0012#, 16#CD0034#, 16#EF0056#));
  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 78 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" }

  Local_R1.I    := Local_R1.I + 1;
  Local_R1.N.C1 := Local_R1.N.C1 + 1;
  Local_R1.N.C2 := Local_R1.N.C2 + 1;
  Local_R1.N.C3 := Local_R1.N.C3 + 1;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : 79 56 34 12 00 ab 00 13 00 cd 00 35 00 ef 00 57.*\n" }

  Local_R2.I    := Local_R2.I + 1;
  Local_R2.N.C1 := Local_R2.N.C1 + 1;
  Local_R2.N.C2 := Local_R2.N.C2 + 1;
  Local_R2.N.C3 := Local_R2.N.C3 + 1;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 12 34 56 79 13 00 ab 00 35 00 cd 00 57 00 ef 00.*\n" }

end;
