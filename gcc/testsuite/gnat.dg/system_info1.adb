--  { dg-do run }

with System.Multiprocessors;
with System.Task_Info;

procedure System_Info1 is
   Ncpus : constant System.Multiprocessors.CPU :=
     System.Multiprocessors.Number_Of_CPUS;
   Nprocs : constant Integer :=
     System.Task_Info.Number_Of_Processors;

   use type System.Multiprocessors.CPU;
begin
   if Nprocs <= 0 or else Nprocs > 1024 then
      raise Program_Error;
   end if;
   if Ncpus <= 0 or else Ncpus > 1024 then
      raise Program_Error;
   end if;
   if Nprocs /= Integer (Ncpus) then
      raise Program_Error;
   end if;
end;