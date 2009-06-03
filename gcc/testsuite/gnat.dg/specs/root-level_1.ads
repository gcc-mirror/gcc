package Root.Level_1 is

   type Level_1_Type (First  : Natural;
                      Second : Natural) is new Root_Type with private;

private

   type Level_1_Type (First  : Natural;
                      Second : Natural) is new Root_Type (First => First)
   with record
      Buffer_1 : Buffer_Type (1 .. Second);
   end record;

end Root.Level_1;
