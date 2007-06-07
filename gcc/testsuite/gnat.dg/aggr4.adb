--  { dg-do compile }
--  { dg-options "-gnatws" }

procedure aggr4 is
   type Byte is range 0 .. 2**8 - 1;
   for Byte'Size use 8;
        
   type Time is array (1 .. 3) of Byte; 
        
   type UTC_Time is record 
      Values : Time;
   end record;

   type Local_Time is record
      Values : Time;
   end record;
   for Local_Time use record
      Values at 0 range 1 .. 24;
   end record;

   LOC : Local_Time;
   UTC : UTC_Time;

begin
   UTC.Values := LOC.Values;
   UTC := (Values => LOC.Values);
end;
