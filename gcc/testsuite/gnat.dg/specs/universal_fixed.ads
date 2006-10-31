-- { dg-do compile }

package Universal_Fixed is
    Nm2Metres : constant := 1852.0;
    type Metres is delta 1.0 range 0.0 .. 1_000_000.0;
    type Nautical_Miles is
      delta 0.001 range 0.0 .. (Metres'Last + (Nm2Metres / 2)) / Nm2Metres;
end Universal_Fixed;
