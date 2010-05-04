package body Lto1_Pkg is

  procedure Initialize (Radar : in Radar_T) is
    Antenna1 : Antenna_Type_T;
    Antenna2 : Antenna_Type_T;
  begin
    case Radar.Sensor_Type is
      when radpr | radssr =>
        Antenna1 := Radar.Sensor_Type;
        Antenna2 := Radar.Sensor_Type;
      when radcmb =>
        Antenna1 := radpr;
        Antenna2 := radssr;
      when others =>
        Antenna1 := radpr;
        Antenna2 := radssr;
    end case;
    if Antenna1 /= radpr or Antenna2 /= radssr then
      raise Program_Error;
    end if;
  end Initialize;

end Lto1_Pkg;
