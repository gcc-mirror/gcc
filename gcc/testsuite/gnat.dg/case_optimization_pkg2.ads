package Case_Optimization_Pkg2 is

   type Unsigned_64 is mod 2 ** 64;

   type Associated_Report_T is (miss, radpr, radssr, radcmb);

   -- sensor type : primary, secondary, co-rotating (combined)
   subtype Sensor_Type_T is Associated_Report_T; -- range radpr .. radcmb;
   subtype Antenna_Type_T is Sensor_Type_T range radpr .. radssr;

   type Filtering_Level_T is (none, pr_in_clutter, ssr_plots, pr_plots);
   type Filtering_Levels_T is array (Filtering_Level_T) of boolean;

   type Radar_T is record
      External_Sensor_ID : Unsigned_64;
      Dual_Radar_Index : Integer;
      Compatible_Filtering_Levels : Filtering_Levels_T;
      Sensor_Type : Sensor_Type_T;
   end record;

   procedure Initialize (Radar : in Radar_T);

end Case_Optimization_Pkg2;
