-- { dg-do run }

procedure self_aggregate_with_zeros is

   type Sensor is record
      Value  : Natural;
      A, B, C, D, E, F, G, H, I, J, K, L, M : Natural;
   end record;

   Pressure : Sensor;

begin
   Pressure.Value := 256;
   Pressure := (Pressure.Value, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

   if Pressure.Value /= 256 then
      raise Program_Error;
   end if;
end;
