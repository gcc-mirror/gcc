--  { dg-do compile }

procedure OpenAcc1 is
   type Integer_Array is array (1 .. 32) of Integer;
   Data : Integer_Array;
begin
   for i in Data'Range loop
      pragma Acc_Parallel;
      pragma Acc_Loop(Worker);
      Data (i) := i;
   end loop;
end;
