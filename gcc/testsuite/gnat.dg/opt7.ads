with Ada.Calendar; use Ada.Calendar;

package Opt7 is

   type time_t is (Absolute_Time, Delta_Time);

   procedure Parse (Str       :     String;
                    Time_Type : out time_t;
                    Abs_Time  : out Time;
                    Delt_Time : out Duration);

end Opt7;
