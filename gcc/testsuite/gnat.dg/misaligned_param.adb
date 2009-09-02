-- { dg-do run }
-- { dg-options "-gnatws" }

with Misaligned_Param_Pkg;

procedure Misaligned_Param is

   procedure Channel_Eth (Status : out Integer; Kind : out Integer);

   pragma Import (External, Channel_Eth);
   pragma Import_Valued_Procedure
     (Channel_Eth, "channel_eth", (Integer, Integer), (VALUE, REFERENCE));

   type Channel is record
      B : Boolean;
      Kind : Integer;
   end record;
   pragma Pack (Channel);

   MyChan : Channel;
   Status : Integer;

begin
   MyChan.Kind := 0;
   Channel_Eth (Status => Status, Kind => MyChan.Kind);

   if Mychan.Kind = 0 then
      raise Program_Error;
   end if;
end;
