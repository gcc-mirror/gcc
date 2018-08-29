--  { dg-options "-gnatws" }

with Ada.Text_IO; use Ada.Text_IO;

with Aggr23_TT; use Aggr23_TT;

procedure Aggr23_Q (Count : Natural) is
   Ts : array (1 .. Count) of TA
         := (others => new T (new Integer));  --  Test
begin
   if Ts (1).D = Ts (2).D then
      Put ("ERROR");
   end if;
end;
