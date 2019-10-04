--  A basic test initially intended to check that
--  System.Task_Info.Number_Of_Processors yields sensible results on
--  both 32bit and 64bit Windows. Additional configurations where the
--  feature was verified to work can opt-in.

--  { dg-do run { target *-*-linux* *-*-mingw* *-*-solaris2.* } }

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
