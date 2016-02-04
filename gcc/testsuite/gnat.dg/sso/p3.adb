-- { dg-do run }

with Init3; use Init3;
with Text_IO; use Text_IO;
with Dump;

procedure P3 is

  Local_R1 : R1;
  Local_R2 : R2;

begin
  Put ("My_R1    :");
  Dump (My_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R1    : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("My_R2    :");
  Dump (My_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "My_R2    : 84 8d 15 9e 15 5b 35 df.*\n" }

  Local_R1 := My_R1;
  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Local_R2 := My_R2;
  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  Local_R1 := (S1 => 2,
               I  => 16#12345678#,
               S2 => 1,
               N  => (16#AB#, 16#CD#, 16#EF#),
               B  => True);
  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Local_R2 := (S1 => 2,
               I  => 16#12345678#,
               S2 => 1,
               N  => (16#AB#, 16#CD#, 16#EF#),
               B  => True);
  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  Local_R1.S1   := Local_R2.S1;
  Local_R1.I    := Local_R2.I;
  Local_R1.S2   := Local_R2.S2;
  Local_R1.N.C1 := Local_R2.N.C1;
  Local_R1.N.C2 := Local_R2.N.C2;
  Local_R1.N.C3 := Local_R2.N.C3;
  Local_R1.B    := Local_R2.B;

  Put ("Local_R1 :");
  Dump (Local_R1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Local_R2.S1   := Local_R1.S1;
  Local_R2.I    := Local_R1.I;
  Local_R2.S2   := Local_R1.S2;
  Local_R2.N.C1 := Local_R1.N.C1;
  Local_R2.N.C2 := Local_R1.N.C2;
  Local_R2.N.C3 := Local_R1.N.C3;
  Local_R2.B    := Local_R1.B;

  Put ("Local_R2 :");
  Dump (Local_R2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" }
end;
