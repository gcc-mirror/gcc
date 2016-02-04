-- { dg-do run }

with Init3; use Init3;
with Text_IO; use Text_IO;
with Dump;

procedure Q3 is

  A1 : R1 := My_R1;
  B1 : R1 := My_R1;

  A2 : R2 := My_R2;
  B2 : R2 := My_R2;

begin
  Put ("A1 :");
  Dump (A1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("B1 :");
  Dump (B1'Address, R1'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B1 : e2 59 d1 48 b4 aa d9 bb.*\n" }

  Put ("A2 :");
  Dump (A2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "A2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  Put ("B2 :");
  Dump (B2'Address, R2'Max_Size_In_Storage_Elements);
  New_Line;
  -- { dg-output "B2 : 84 8d 15 9e 15 5b 35 df.*\n" }

  if A1.S1 /= B1.S1 then
    raise Program_Error;
  end if;

  if A1.S1 /= 2 then
    raise Program_Error;
  end if;

  if A2.S1 /= B2.S1 then
    raise Program_Error;
  end if;

  if A2.S1 /= 2 then
    raise Program_Error;
  end if;

  if A1.I /= B1.I or A1.N.C1 /= B1.N.C1 then
    raise Program_Error;
  end if;

  if A2.I /= B2.I or A2.N.C1 /= B2.N.C1 then
    raise Program_Error;
  end if;

end;
