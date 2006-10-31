-- { dg-do run }

with Interfaces; use Interfaces;
procedure Conv_Real is
   Small : constant := 10.0**(-9);
   type Time_Type is delta Small range -2**63 * Small .. (2**63-1) * Small;
   for Time_Type'Small use Small;
   for Time_Type'Size use 64; 
   procedure Cache (Seconds_Per_GDS_Cycle : in Time_Type) is
      Cycles_Per_Second : constant Time_Type  := (1.0 / Seconds_Per_GDS_Cycle);
   begin   
      if Integer_32 (Seconds_Per_GDS_Cycle * Cycles_Per_Second) /= 1 then
         raise Program_Error;
      end if; 
   end Cache;
begin   
   Cache (0.035);
end;
