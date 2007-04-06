-- { dg-do run }

with Ada.Real_Time;
        
procedure Test_Delay is
begin
   delay until Ada.Real_Time.Clock;
end Test_Delay;
