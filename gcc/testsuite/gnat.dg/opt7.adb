-- { dg-do compile }
-- { dg-options "-Os -g" }

with Opt7_Pkg;

package body Opt7 is

   procedure Parse (Str       :     String;
                    Time_Type : out time_t;
                    Abs_Time  : out Time;
                    Delt_Time : out Duration) is
      Year         : Year_Number;
      Month        : Month_Number;
      Day          : Day_Number;
      Minute       : Integer := 0;
      Idx          : Integer := Str'First;
      Ch           : Character := Str (Idx);
      Current_Time : Time;

   begin
      if Ch = '-' then
         Time_Type := Absolute_Time;
         Current_Time := Clock;
         Day   := Ada.Calendar.Day (Current_Time);
         Month := Ada.Calendar.Month (Current_Time);
         Year  := Ada.Calendar.Year (Current_Time);
      else
         Time_Type := Delta_Time;
      end if;
      while Ch in '0' .. '9' loop
         Minute := Minute + Character'Pos (Ch);
         Idx := Idx + 1;
         Ch  := Str (Idx);
      end loop;
      if Time_Type = Absolute_Time then
         Abs_Time := Time_Of (Year, Month, Day, Day_Duration (1));
      else
         Delt_Time := Duration (Float (Minute));
      end if;
   exception
      when others => Opt7_Pkg.My_Raise_Exception;
   end;

end Opt7;
