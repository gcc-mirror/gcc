-- { dg-do run }

procedure Atomic8 is

   V : array (1 .. 2) of Natural := (0,0) with Atomic_Components;

   task type TT1;
   task body TT1 is
   begin
      while V (1) + V (2) < 1_000_000 loop
         V (1) := V (1) + 1;
      end loop;
   end TT1;

   task type TT2;
   task body TT2 is
   begin
      while V (1) + V (2) < 1_000_000 loop
         V (2) := V (2) + 1;
      end loop;
   end TT2;

begin
   declare
      T1 : TT1;
      T2 : TT2;
   begin
      null;
   end;
   if V (1) + V (2) not in 1_000_000 | 1_000_001 then
      raise Program_Error;
   end if;
end;
