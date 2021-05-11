------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C A L E N D A R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2021, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Extensions;

package body GNAT.Calendar is
   use Ada.Calendar;
   use Interfaces;

   -----------------
   -- Day_In_Year --
   -----------------

   function Day_In_Year (Date : Time) return Day_In_Year_Number is
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Day_Secs : Day_Duration;
      pragma Unreferenced (Day_Secs);
   begin
      Split (Date, Year, Month, Day, Day_Secs);
      return Julian_Day (Year, Month, Day) - Julian_Day (Year, 1, 1) + 1;
   end Day_In_Year;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Date : Time) return Day_Name is
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Day_Secs : Day_Duration;
      pragma Unreferenced (Day_Secs);
   begin
      Split (Date, Year, Month, Day, Day_Secs);
      return Day_Name'Val ((Julian_Day (Year, Month, Day)) mod 7);
   end Day_Of_Week;

   ----------
   -- Hour --
   ----------

   function Hour (Date : Time) return Hour_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      pragma Unreferenced (Year, Month, Day, Minute, Second, Sub_Second);
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Hour;
   end Hour;

   ----------------
   -- Julian_Day --
   ----------------

   --  Julian_Day is used to by Day_Of_Week and Day_In_Year. Note that this
   --  implementation is not expensive.

   function Julian_Day
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number) return Integer
   is
      Internal_Year  : Integer;
      Internal_Month : Integer;
      Internal_Day   : Integer;
      Julian_Date    : Integer;
      C              : Integer;
      Ya             : Integer;

   begin
      Internal_Year  := Integer (Year);
      Internal_Month := Integer (Month);
      Internal_Day   := Integer (Day);

      if Internal_Month > 2 then
         Internal_Month := Internal_Month - 3;
      else
         Internal_Month := Internal_Month + 9;
         Internal_Year  := Internal_Year - 1;
      end if;

      C  := Internal_Year / 100;
      Ya := Internal_Year - (100 * C);

      Julian_Date := (146_097 * C) / 4 +
        (1_461 * Ya) / 4 +
        (153 * Internal_Month + 2) / 5 +
        Internal_Day + 1_721_119;

      return Julian_Date;
   end Julian_Day;

   ------------
   -- Minute --
   ------------

   function Minute (Date : Time) return Minute_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      pragma Unreferenced (Year, Month, Day, Hour, Second, Sub_Second);
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Minute;
   end Minute;

   ------------
   -- Second --
   ------------

   function Second (Date : Time) return Second_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      pragma Unreferenced (Year, Month, Day, Hour, Minute, Sub_Second);
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Second;
   end Second;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      Day_Secs : Day_Duration;
      Secs     : Natural;

   begin
      Split (Date, Year, Month, Day, Day_Secs);

      Secs       := (if Day_Secs = 0.0 then 0 else Natural (Day_Secs - 0.5));
      Sub_Second := Second_Duration (Day_Secs - Day_Duration (Secs));
      Hour       := Hour_Number (Secs / 3_600);
      Secs       := Secs mod 3_600;
      Minute     := Minute_Number (Secs / 60);
      Second     := Second_Number (Secs mod 60);
   end Split;

   ---------------------
   -- Split_At_Locale --
   ---------------------

   procedure Split_At_Locale
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      procedure Ada_Calendar_Split
        (Date        : Time;
         Year        : out Year_Number;
         Month       : out Month_Number;
         Day         : out Day_Number;
         Day_Secs    : out Day_Duration;
         Hour        : out Integer;
         Minute      : out Integer;
         Second      : out Integer;
         Sub_Sec     : out Duration;
         Leap_Sec    : out Boolean;
         Use_TZ      : Boolean;
         Is_Historic : Boolean;
         Time_Zone   : Long_Integer);
      pragma Import (Ada, Ada_Calendar_Split, "__gnat_split");

      Ds : Day_Duration;
      Le : Boolean;

      pragma Unreferenced (Ds, Le);

   begin
      --  Even though the input time zone is UTC (0), the flag Use_TZ will
      --  ensure that Split picks up the local time zone. ???But Use_TZ is
      --  False below, and anyway, Use_TZ has no effect if Time_Zone is 0.

      Ada_Calendar_Split
        (Date        => Date,
         Year        => Year,
         Month       => Month,
         Day         => Day,
         Day_Secs    => Ds,
         Hour        => Hour,
         Minute      => Minute,
         Second      => Second,
         Sub_Sec     => Sub_Second,
         Leap_Sec    => Le,
         Use_TZ      => False,
         Is_Historic => False,
         Time_Zone   => 0);
   end Split_At_Locale;

   ----------------
   -- Sub_Second --
   ----------------

   function Sub_Second (Date : Time) return Second_Duration is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      pragma Unreferenced (Year, Month, Day, Hour, Minute, Second);
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Sub_Second;
   end Sub_Second;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0) return Time
   is
      Day_Secs : constant Day_Duration :=
                   Day_Duration (Hour   * 3_600) +
                   Day_Duration (Minute *    60) +
                   Day_Duration (Second)         +
                                 Sub_Second;
   begin
      return Time_Of (Year, Month, Day, Day_Secs);
   end Time_Of;

   -----------------------
   -- Time_Of_At_Locale --
   -----------------------

   function Time_Of_At_Locale
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0) return Time
   is
      function Ada_Calendar_Time_Of
        (Year         : Year_Number;
         Month        : Month_Number;
         Day          : Day_Number;
         Day_Secs     : Day_Duration;
         Hour         : Integer;
         Minute       : Integer;
         Second       : Integer;
         Sub_Sec      : Duration;
         Leap_Sec     : Boolean;
         Use_Day_Secs : Boolean;
         Use_TZ       : Boolean;
         Is_Historic  : Boolean;
         Time_Zone    : Long_Integer) return Time;
      pragma Import (Ada, Ada_Calendar_Time_Of, "__gnat_time_of");

   begin
      --  Even though the input time zone is UTC (0), the flag Use_TZ will
      --  ensure that Split picks up the local time zone. ???But there is no
      --  call to Split here.

      return
        Ada_Calendar_Time_Of
          (Year         => Year,
           Month        => Month,
           Day          => Day,
           Day_Secs     => 0.0,
           Hour         => Hour,
           Minute       => Minute,
           Second       => Second,
           Sub_Sec      => Sub_Second,
           Leap_Sec     => False,
           Use_Day_Secs => False,
           Use_TZ       => False,
           Is_Historic  => False,
           Time_Zone    => 0);
   end Time_Of_At_Locale;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : not null access timeval) return Duration is

      procedure timeval_to_duration
        (T    : not null access timeval;
         sec  : not null access C.Extensions.long_long;
         usec : not null access C.long);
      pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

      Micro : constant := 10**6;
      sec   : aliased C.Extensions.long_long;
      usec  : aliased C.long;

   begin
      timeval_to_duration (T, sec'Access, usec'Access);
      pragma Annotate (CodePeer, Modified, sec);
      pragma Annotate (CodePeer, Modified, usec);

      return Duration (sec) + Duration (usec) / Micro;
   end To_Duration;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval (D : Duration) return timeval is

      procedure duration_to_timeval
        (Sec  : C.Extensions.long_long;
         Usec : C.long;
         T : not null access timeval);
      pragma Import (C, duration_to_timeval, "__gnat_duration_to_timeval");

      Micro  : constant := 10**6;
      Result : aliased timeval;
      sec    : C.Extensions.long_long;
      usec   : C.long;

   begin
      if D = 0.0 then
         sec  := 0;
         usec := 0;
      else
         sec  := C.Extensions.long_long (D - 0.5);
         usec := C.long ((D - Duration (sec)) * Micro - 0.5);
      end if;

      duration_to_timeval (sec, usec, Result'Access);

      return Result;
   end To_Timeval;

   ------------------
   -- Week_In_Year --
   ------------------

   function Week_In_Year (Date : Time) return Week_In_Year_Number is
      Year : Year_Number;
      Week : Week_In_Year_Number;
      pragma Unreferenced (Year);
   begin
      Year_Week_In_Year (Date, Year, Week);
      return Week;
   end Week_In_Year;

   -----------------------
   -- Year_Week_In_Year --
   -----------------------

   procedure Year_Week_In_Year
     (Date : Time;
      Year : out Year_Number;
      Week : out Week_In_Year_Number)
   is
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      Jan_1      : Day_Name;
      Shift      : Week_In_Year_Number;
      Start_Week : Week_In_Year_Number;

      pragma Unreferenced (Hour, Minute, Second, Sub_Second);

      function Is_Leap (Year : Year_Number) return Boolean;
      --  Return True if Year denotes a leap year. Leap centennial years are
      --  properly handled.

      function Jan_1_Day_Of_Week
        (Jan_1     : Day_Name;
         Year      : Year_Number;
         Last_Year : Boolean := False;
         Next_Year : Boolean := False) return Day_Name;
      --  Given the weekday of January 1 in Year, determine the weekday on
      --  which January 1 fell last year or will fall next year as set by
      --  the two flags. This routine does not call Time_Of or Split.

      function Last_Year_Has_53_Weeks
        (Jan_1 : Day_Name;
         Year  : Year_Number) return Boolean;
      --  Given the weekday of January 1 in Year, determine whether last year
      --  has 53 weeks. A False value implies that the year has 52 weeks.

      -------------
      -- Is_Leap --
      -------------

      function Is_Leap (Year : Year_Number) return Boolean is
      begin
         if Year mod 400 = 0 then
            return True;
         elsif Year mod 100 = 0 then
            return False;
         else
            return Year mod 4 = 0;
         end if;
      end Is_Leap;

      -----------------------
      -- Jan_1_Day_Of_Week --
      -----------------------

      function Jan_1_Day_Of_Week
        (Jan_1     : Day_Name;
         Year      : Year_Number;
         Last_Year : Boolean := False;
         Next_Year : Boolean := False) return Day_Name
      is
         Shift : Integer := 0;

      begin
         if Last_Year then
            Shift := (if Is_Leap (Year - 1) then -2 else -1);
         elsif Next_Year then
            Shift := (if Is_Leap (Year) then 2 else 1);
         end if;

         return Day_Name'Val ((Day_Name'Pos (Jan_1) + Shift) mod 7);
      end Jan_1_Day_Of_Week;

      ----------------------------
      -- Last_Year_Has_53_Weeks --
      ----------------------------

      function Last_Year_Has_53_Weeks
        (Jan_1 : Day_Name;
         Year  : Year_Number) return Boolean
      is
         Last_Jan_1 : constant Day_Name :=
                        Jan_1_Day_Of_Week (Jan_1, Year, Last_Year => True);

      begin
         --  These two cases are illustrated in the table below

         return
           Last_Jan_1 = Thursday
             or else (Last_Jan_1 = Wednesday and then Is_Leap (Year - 1));
      end Last_Year_Has_53_Weeks;

   --  Start of processing for Week_In_Year

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      --  According to ISO 8601, the first week of year Y is the week that
      --  contains the first Thursday in year Y. The following table contains
      --  all possible combinations of years and weekdays along with examples.

      --    +-------+------+-------+---------+
      --    | Jan 1 | Leap | Weeks | Example |
      --    +-------+------+-------+---------+
      --    |  Mon  |  No  |  52   |  2007   |
      --    +-------+------+-------+---------+
      --    |  Mon  | Yes  |  52   |  1996   |
      --    +-------+------+-------+---------+
      --    |  Tue  |  No  |  52   |  2002   |
      --    +-------+------+-------+---------+
      --    |  Tue  | Yes  |  52   |  1980   |
      --    +-------+------+-------+---------+
      --    |  Wed  |  No  |  52   |  2003   |
      --    +-------+------#########---------+
      --    |  Wed  | Yes  #  53   #  1992   |
      --    +-------+------#-------#---------+
      --    |  Thu  |  No  #  53   #  1998   |
      --    +-------+------#-------#---------+
      --    |  Thu  | Yes  #  53   #  2004   |
      --    +-------+------#########---------+
      --    |  Fri  |  No  |  52   |  1999   |
      --    +-------+------+-------+---------+
      --    |  Fri  | Yes  |  52   |  1988   |
      --    +-------+------+-------+---------+
      --    |  Sat  |  No  |  52   |  1994   |
      --    +-------+------+-------+---------+
      --    |  Sat  | Yes  |  52   |  1972   |
      --    +-------+------+-------+---------+
      --    |  Sun  |  No  |  52   |  1995   |
      --    +-------+------+-------+---------+
      --    |  Sun  | Yes  |  52   |  1956   |
      --    +-------+------+-------+---------+

      --  A small optimization, the input date is January 1. Note that this
      --  is a key day since it determines the number of weeks and is used
      --  when special casing the first week of January and the last week of
      --  December.

      Jan_1 := Day_Of_Week (if Day = 1 and then Month = 1
                            then Date
                            else (Time_Of (Year, 1, 1, 0.0)));

      --  Special cases for January

      if Month = 1 then

         --  Special case 1: January 1, 2 and 3. These three days may belong
         --  to last year's last week which can be week number 52 or 53.

         --    +-----+-----+-----+=====+-----+-----+-----+
         --    | Mon | Tue | Wed # Thu # Fri | Sat | Sun |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 26  | 27  | 28  # 29  # 30  | 31  |  1  |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 27  | 28  | 29  # 30  # 31  |  1  |  2  |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 28  | 29  | 30  # 31  #  1  |  2  |  3  |
         --    +-----+-----+-----+=====+-----+-----+-----+

         if (Day = 1 and then Jan_1 in Friday .. Sunday)
               or else
            (Day = 2 and then Jan_1 in Friday .. Saturday)
               or else
            (Day = 3 and then Jan_1 = Friday)
         then
            Week := (if Last_Year_Has_53_Weeks (Jan_1, Year) then 53 else 52);

            --  January 1, 2 and 3 belong to the previous year

            Year := Year - 1;
            return;

         --  Special case 2: January 1, 2, 3, 4, 5, 6 and 7 of the first week

         --    +-----+-----+-----+=====+-----+-----+-----+
         --    | Mon | Tue | Wed # Thu # Fri | Sat | Sun |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 29  | 30  | 31  #  1  #  2  |  3  |  4  |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 30  | 31  |  1  #  2  #  3  |  4  |  5  |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    | 31  |  1  |  2  #  3  #  4  |  5  |  6  |
         --    +-----+-----+-----+-----+-----+-----+-----+
         --    |  1  |  2  |  3  #  4  #  5  |  6  |  7  |
         --    +-----+-----+-----+=====+-----+-----+-----+

         elsif (Day <= 4 and then Jan_1 in Monday .. Thursday)
                  or else
               (Day = 5  and then Jan_1 in Monday .. Wednesday)
                  or else
               (Day = 6  and then Jan_1 in Monday ..  Tuesday)
                  or else
               (Day = 7  and then Jan_1 = Monday)
         then
            Week := 1;
            return;
         end if;

      --  Month other than 1

      --  Special case 3: December 29, 30 and 31. These days may belong to
      --  next year's first week.

      --    +-----+-----+-----+=====+-----+-----+-----+
      --    | Mon | Tue | Wed # Thu # Fri | Sat | Sun |
      --    +-----+-----+-----+-----+-----+-----+-----+
      --    | 29  | 30  | 31  #  1  #  2  |  3  |  4  |
      --    +-----+-----+-----+-----+-----+-----+-----+
      --    | 30  | 31  |  1  #  2  #  3  |  4  |  5  |
      --    +-----+-----+-----+-----+-----+-----+-----+
      --    | 31  |  1  |  2  #  3  #  4  |  5  |  6  |
      --    +-----+-----+-----+=====+-----+-----+-----+

      elsif Month = 12 and then Day > 28 then
         declare
            Next_Jan_1 : constant Day_Name :=
                           Jan_1_Day_Of_Week (Jan_1, Year, Next_Year => True);
         begin
            if (Day = 29 and then Next_Jan_1 = Thursday)
                  or else
               (Day = 30 and then Next_Jan_1 in Wednesday .. Thursday)
                  or else
               (Day = 31 and then Next_Jan_1 in Tuesday .. Thursday)
            then
               Year := Year + 1;
               Week := 1;
               return;
            end if;
         end;
      end if;

      --  Determine the week from which to start counting. If January 1 does
      --  not belong to the first week of the input year, then the next week
      --  is the first week.

      Start_Week := (if Jan_1 in Friday .. Sunday then 1 else 2);

      --  At this point all special combinations have been accounted for and
      --  the proper start week has been found. Since January 1 may not fall
      --  on a Monday, shift 7 - Day_Name'Pos (Jan_1). This action ensures an
      --  origin which falls on Monday.

      Shift := 7 - Day_Name'Pos (Jan_1);
      Week  := Start_Week + (Day_In_Year (Date) - Shift - 1) / 7;
   end Year_Week_In_Year;

end GNAT.Calendar;
