-- { dg-do run }

procedure self_aggregate_with_array is

   type Value_Bounds is array (1 .. 2) of Natural;

   type Sensor is record
      Value  : Natural;
      Bounds : Value_Bounds;
   end record;

   Pressure : Sensor;

begin
   Pressure.Value := 256;
   Pressure := (Value => Pressure.Value, Bounds => (1, 2));

   if Pressure.Value /= 256 then
      raise Program_Error;
   end if;
end;
