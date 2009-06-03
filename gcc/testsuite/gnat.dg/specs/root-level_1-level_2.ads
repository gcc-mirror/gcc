package Root.Level_1.Level_2 is

   type Level_2_Type (First  : Natural;
                      Second : Natural) is new
     Level_1.Level_1_Type (First => First, Second => Second) with null record;

end Root.Level_1.Level_2;
