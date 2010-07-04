-- PR ada/43106
-- Testcase by Bill Neven <neven@hitt.nl>

-- { dg-do run }
-- { dg-options "-O2 -flto" { target lto } }

with Lto1_Pkg; use Lto1_Pkg;

procedure Lto1 is
   Radar : Radar_T;
begin
   Radar.Sensor_Type := radcmb;
   Initialize (Radar);
end;
