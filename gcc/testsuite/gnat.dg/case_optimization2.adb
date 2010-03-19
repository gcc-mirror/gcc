-- PR ada/43106
-- Testcase by Bill Neven <neven@hitt.nl>

-- { dg-do run }
-- { dg-options "-O" }

with Case_Optimization_Pkg2; use Case_Optimization_Pkg2;

procedure Case_Optimization2 is
   Radar : Radar_T;
begin
   Radar.Sensor_Type := radcmb;
   Initialize (Radar);
end;
