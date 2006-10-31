------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . F O R M A T T I N G               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Unchecked_Conversion;

package body Ada.Calendar.Formatting is

   use Leap_Sec_Ops;

   Days_In_4_Years          : constant := 365 * 3 + 366;
   Seconds_In_Day           : constant := 86_400;
   Seconds_In_4_Years       : constant := Days_In_4_Years * Seconds_In_Day;
   Seconds_In_Non_Leap_Year : constant := 365 * Seconds_In_Day;

   --  Exact time bounds for the range of Ada time: January 1, 1901 -
   --  December 31, 2099. These bounds are based on the Unix Time of Epoc,
   --  January 1, 1970. Start of Time is -69 years from TOE while End of
   --  time is +130 years and one second from TOE.

   Start_Of_Time : constant Time :=
                     Time (-(17 * Seconds_In_4_Years +
                                  Seconds_In_Non_Leap_Year));

   End_Of_Time   : constant Time :=
                     Time (32 * Seconds_In_4_Years +
                            2 * Seconds_In_Non_Leap_Year) +
                                All_Leap_Seconds;

   Days_In_Month : constant array (Month_Number) of Day_Number :=
     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   procedure Check_Char (S : String; C : Character; Index : Integer);
   --  Subsidiary to the two versions of Value. Determine whether the
   --  input strint S has character C at position Index. Raise
   --  Constraint_Error if there is a mismatch.

   procedure Check_Digit (S : String; Index : Integer);
   --  Subsidiary to the two versions of Value. Determine whether the
   --  character of string S at position Index is a digit. This catches
   --  invalid input such as 1983-*1-j3 u5:n7:k9 which should be
   --  1983-01-03 05:07:09. Raise Constraint_Error if there is a mismatch.

   ----------------
   -- Check_Char --
   ----------------

   procedure Check_Char (S : String; C : Character; Index : Integer) is
   begin
      if S (Index) /= C then
         raise Constraint_Error;
      end if;
   end Check_Char;

   -----------------
   -- Check_Digit --
   -----------------

   procedure Check_Digit (S : String; Index : Integer) is
   begin
      if S (Index) not in '0' .. '9' then
         raise Constraint_Error;
      end if;
   end Check_Digit;

   ---------
   -- Day --
   ---------

   function Day
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Day_Number
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
      return Day;
   end Day;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Date : Time) return Day_Name is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

      D           : Duration;
      Day_Count   : Long_Long_Integer;
      Midday_Date : Time;
      Secs_Count  : Long_Long_Integer;

   begin
      --  Split the Date to obtain the year, month and day, then build a time
      --  value for the middle of the same day, so that we don't have to worry
      --  about leap seconds in the subsequent arithmetic.

      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second);

      Midday_Date := Time_Of (Year, Month, Day, 12, 0, 0);
      D           := Midday_Date - Start_Of_Time;

      --  D is a positive Duration value counting seconds since 1901. Convert
      --  it into an integer for ease of arithmetic.

      declare
         type D_Int is range 0 .. 2 ** (Duration'Size - 1) - 1;
         for D_Int'Size use Duration'Size;

         function To_D_Int is new Unchecked_Conversion (Duration, D_Int);

         D_As_Int  : constant D_Int := To_D_Int (D);
         Small_Div : constant D_Int := D_Int (1.0 / Duration'Small);

      begin
         Secs_Count := Long_Long_Integer (D_As_Int / Small_Div);
      end;

      Day_Count := Secs_Count / Seconds_In_Day;
      Day_Count := Day_Count + 1;  --  Jan 1, 1901 was a Tuesday;

      return Day_Name'Val (Day_Count mod 7);
   end Day_Of_Week;

   ----------
   -- Hour --
   ----------

   function Hour
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Hour_Number
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
      return Hour;
   end Hour;

   -----------
   -- Image --
   -----------

   function Image
     (Elapsed_Time          : Duration;
      Include_Time_Fraction : Boolean := False) return String
   is
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      SS_Nat     : Natural;

      Result : String := "00:00:00.00";

   begin
      Split (Elapsed_Time, Hour, Minute, Second, Sub_Second);
      SS_Nat := Natural (Sub_Second * 100.0);

      declare
         Hour_Str   : constant String := Hour_Number'Image (Hour);
         Minute_Str : constant String := Minute_Number'Image (Minute);
         Second_Str : constant String := Second_Number'Image (Second);
         SS_Str     : constant String := Natural'Image (SS_Nat);

      begin
         --  Hour processing, positions 1 and 2

         if Hour < 10 then
            Result (2) := Hour_Str (2);
         else
            Result (1) := Hour_Str (2);
            Result (2) := Hour_Str (3);
         end if;

         --  Minute processing, positions 4 and 5

         if Minute < 10 then
            Result (5) := Minute_Str (2);
         else
            Result (4) := Minute_Str (2);
            Result (5) := Minute_Str (3);
         end if;

         --  Second processing, positions 7 and 8

         if Second < 10 then
            Result (8) := Second_Str (2);
         else
            Result (7) := Second_Str (2);
            Result (8) := Second_Str (3);
         end if;

         --  Optional sub second processing, positions 10 and 11

         if Include_Time_Fraction then
            if SS_Nat < 10 then
               Result (11) := SS_Str (2);
            else
               Result (10) := SS_Str (2);
               Result (11) := SS_Str (3);
            end if;

            return Result;
         else
            return Result (1 .. 8);
         end if;
      end;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Date                  : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone             : Time_Zones.Time_Offset := 0) return String
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      SS_Nat      : Natural;
      Leap_Second : Boolean;

      Result : String := "0000-00-00 00:00:00.00";

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);

      SS_Nat := Natural (Sub_Second * 100.0);

      declare
         Year_Str   : constant String := Year_Number'Image (Year);
         Month_Str  : constant String := Month_Number'Image (Month);
         Day_Str    : constant String := Day_Number'Image (Day);
         Hour_Str   : constant String := Hour_Number'Image (Hour);
         Minute_Str : constant String := Minute_Number'Image (Minute);
         Second_Str : constant String := Second_Number'Image (Second);
         SS_Str     : constant String := Natural'Image (SS_Nat);

      begin
         --  Year processing, positions 1, 2, 3 and 4

         Result (1) := Year_Str (2);
         Result (2) := Year_Str (3);
         Result (3) := Year_Str (4);
         Result (4) := Year_Str (5);

         --  Month processing, positions 6 and 7

         if Month < 10 then
            Result (7) := Month_Str (2);
         else
            Result (6) := Month_Str (2);
            Result (7) := Month_Str (3);
         end if;

         --  Day processing, positions 9 and 10

         if Day < 10 then
            Result (10) := Day_Str (2);
         else
            Result (9)  := Day_Str (2);
            Result (10) := Day_Str (3);
         end if;

         --  Hour processing, positions 12 and 13

         if Hour < 10 then
            Result (13) := Hour_Str (2);
         else
            Result (12) := Hour_Str (2);
            Result (13) := Hour_Str (3);
         end if;

         --  Minute processing, positions 15 and 16

         if Minute < 10 then
            Result (16) := Minute_Str (2);
         else
            Result (15) := Minute_Str (2);
            Result (16) := Minute_Str (3);
         end if;

         --  Second processing, positions 18 and 19

         if Second < 10 then
            Result (19) := Second_Str (2);
         else
            Result (18) := Second_Str (2);
            Result (19) := Second_Str (3);
         end if;

         --  Optional sub second processing, positions 21 and 22

         if Include_Time_Fraction then
            if SS_Nat < 10 then
               Result (22) := SS_Str (2);
            else
               Result (21) := SS_Str (2);
               Result (22) := SS_Str (3);
            end if;

            return Result;
         else
            return Result (1 .. 19);
         end if;
      end;
   end Image;

   ------------
   -- Minute --
   ------------

   function Minute
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Minute_Number
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
      return Minute;
   end Minute;

   -----------
   -- Month --
   -----------

   function Month
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Month_Number
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
      return Month;
   end Month;

   ------------
   -- Second --
   ------------

   function Second (Date : Time) return Second_Number is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second);
      return Second;
   end Second;

   ----------------
   -- Seconds_Of --
   ----------------

   function Seconds_Of
     (Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0) return Day_Duration is

   begin
      --  Validity checks

      if not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
        or else not Sub_Second'Valid
      then
         raise Constraint_Error;
      end if;

      return Day_Duration (Hour   * 3600) +
             Day_Duration (Minute *   60) +
             Day_Duration (Second)        +
                           Sub_Second;
   end Seconds_Of;

   -----------
   -- Split --
   -----------

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      Secs : Natural;

   begin
      --  Validity checks

      if not Seconds'Valid then
         raise Constraint_Error;
      end if;

      if Seconds = 0.0 then
         Secs := 0;
      else
         Secs := Natural (Seconds - 0.5);
      end if;

      Sub_Second := Second_Duration (Seconds - Day_Duration (Secs));
      Hour       := Hour_Number (Secs / 3600);
      Secs       := Secs mod 3600;
      Minute     := Minute_Number (Secs / 60);
      Second     := Second_Number (Secs mod 60);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date        : Time;
      Year        : out Year_Number;
      Month       : out Month_Number;
      Day         : out Day_Number;
      Seconds     : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone   : Time_Zones.Time_Offset := 0)
   is
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);

      Seconds := Seconds_Of (Hour, Minute, Second, Sub_Second);
   end Split;

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
      Sub_Second : out Second_Duration;
      Time_Zone  : Time_Zones.Time_Offset := 0)
   is
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date        : Time;
      Year        : out Year_Number;
      Month       : out Month_Number;
      Day         : out Day_Number;
      Hour        : out Hour_Number;
      Minute      : out Minute_Number;
      Second      : out Second_Number;
      Sub_Second  : out Second_Duration;
      Leap_Second : out Boolean;
      Time_Zone   : Time_Zones.Time_Offset := 0)
   is
      Ada_Year_Min    : constant Year_Number := Year_Number'First;
      Day_In_Year     : Integer;
      Day_Second      : Integer;
      Elapsed_Leaps   : Duration;
      Hour_Second     : Integer;
      In_Leap_Year    : Boolean;
      Modified_Date   : Time;
      Next_Leap       : Time;
      Remaining_Years : Integer;
      Seconds_Count   : Long_Long_Integer;

   begin
      --  Our measurement of time is the number of seconds that have elapsed
      --  since the Unix TOE. To calculate a UTC date from this we do a
      --  sequence of divides and mods to get the components of a date based
      --  on 86,400 seconds in each day. Since, UTC time depends upon the
      --  occasional insertion of leap seconds, the number of leap seconds
      --  that have been added prior to the input time are then subtracted
      --  from the previous calculation. In fact, it is easier to do the
      --  subtraction first, so a more accurate discription of what is
      --  actually done, is that the number of added leap seconds is looked
      --  up using the input Time value, than that number of seconds is
      --  subtracted before the sequence of divides and mods.
      --
      --  If the input date turns out to be a leap second, we don't add it to
      --  date (we want to return 23:59:59) but we set the Leap_Second output
      --  to true.

      --  Is there a need to account for a difference from Unix time prior
      --  to the first leap second ???

      --  Step 1: Determine the number of leap seconds since the start
      --  of Ada time and the input date as well as the next leap second
      --  occurence and process accordingly.

      Cumulative_Leap_Secs (Start_Of_Time, Date, Elapsed_Leaps, Next_Leap);

      Leap_Second   := Date >= Next_Leap;
      Modified_Date := Date - Elapsed_Leaps;

      if Leap_Second then
         Modified_Date := Modified_Date - Duration (1.0);
      end if;

      --  Step 2: Process the time zone

      Modified_Date := Modified_Date + Duration (Time_Zone * 60);

      --  Step 3: Sanity check on the calculated date. Since the leap
      --  seconds and the time zone have been eliminated, the result needs
      --  to be within the range of Ada time.

      if Modified_Date < Start_Of_Time
        or else Modified_Date >= (End_Of_Time - All_Leap_Seconds)
      then
         raise Time_Error;
      end if;

      Modified_Date := Modified_Date - Start_Of_Time;

      declare
         type D_Int is range 0 .. 2 ** (Duration'Size - 1) - 1;
         for D_Int'Size use Duration'Size;

         function To_D_Int is new Unchecked_Conversion (Duration, D_Int);
         function To_Duration is new Unchecked_Conversion (D_Int, Duration);
         function To_Duration is new Unchecked_Conversion (Time, Duration);

         D_As_Int  : constant D_Int := To_D_Int (To_Duration (Modified_Date));
         Small_Div : constant D_Int := D_Int (1.0 / Duration'Small);

      begin
         Seconds_Count := Long_Long_Integer (D_As_Int / Small_Div);
         Sub_Second    := Second_Duration
                            (To_Duration (D_As_Int rem Small_Div));
      end;

      --  Step 4: Calculate the number of years since the start of Ada time.
      --  First consider sequences of four years, then the remaining years.

      Year := Ada_Year_Min + 4 * Integer (Seconds_Count / Seconds_In_4_Years);
      Seconds_Count   := Seconds_Count mod Seconds_In_4_Years;
      Remaining_Years := Integer (Seconds_Count / Seconds_In_Non_Leap_Year);

      if Remaining_Years > 3 then
         Remaining_Years := 3;
      end if;

      Year := Year + Remaining_Years;

      --  Remove the seconds elapsed in those remaining years

      Seconds_Count := Seconds_Count - Long_Long_Integer
                         (Remaining_Years * Seconds_In_Non_Leap_Year);
      In_Leap_Year := (Year mod 4) = 0;

      --  Step 5: Month and day processing. Determine the day to which the
      --  remaining seconds map to.

      Day_In_Year := Integer (Seconds_Count / Seconds_In_Day) + 1;

      Month := 1;

      if Day_In_Year > 31 then
         Month       := 2;
         Day_In_Year := Day_In_Year - 31;

         if Day_In_Year > 28
           and then ((not In_Leap_Year)
                        or else Day_In_Year > 29)
         then
            Month       := 3;
            Day_In_Year := Day_In_Year - 28;

            if In_Leap_Year then
               Day_In_Year := Day_In_Year - 1;
            end if;

            while Day_In_Year > Days_In_Month (Month) loop
               Day_In_Year := Day_In_Year - Days_In_Month (Month);
               Month := Month + 1;
            end loop;
         end if;
      end if;

      --  Step 6: Hour, minute and second processing

      Day         := Day_In_Year;
      Day_Second  := Integer (Seconds_Count mod Seconds_In_Day);
      Hour        := Day_Second / 3600;
      Hour_Second := Day_Second mod 3600;
      Minute      := Hour_Second / 60;
      Second      := Hour_Second mod 60;
   end Split;

   ----------------
   -- Sub_Second --
   ----------------

   function Sub_Second (Date : Time) return Second_Duration is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second);

      return Sub_Second;
   end Sub_Second;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Seconds     : Day_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone   : Time_Zones.Time_Offset := 0) return Time
   is
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Sec_Num    : Second_Number;
      Sub_Sec    : Second_Duration;
      Whole_Part : Integer;

   begin
      if not Seconds'Valid then
         raise Constraint_Error;
      end if;

      --  The fact that Seconds can go to 86,400 creates all this extra work.
      --  Perhaps a Time_Of just like the next one but allowing the Second_
      --  Number input to reach 60 should become an internal version that this
      --  and the next version call.... but for now we do the ugly bumping up
      --  of Day, Month and Year;

      if Seconds = 86_400.0 then
         declare
            Adj_Year  : Year_Number  := Year;
            Adj_Month : Month_Number := Month;
            Adj_Day   : Day_Number   := Day;

         begin
            Hour    := 0;
            Minute  := 0;
            Sec_Num := 0;
            Sub_Sec := 0.0;

            if Day < Days_In_Month (Month)
              or else (Month = 2
                         and then Year mod 4 = 0)
            then
               Adj_Day := Day + 1;
            else
               Adj_Day := 1;

               if Month < 12 then
                  Adj_Month := Month + 1;
               else
                  Adj_Month := 1;
                  Adj_Year  := Year + 1;
               end if;
            end if;

            return Time_Of (Adj_Year, Adj_Month, Adj_Day, Hour, Minute,
                            Sec_Num, Sub_Sec, Leap_Second, Time_Zone);
         end;
      end if;

      declare
         type D_Int is range 0 .. 2 ** (Duration'Size - 1) - 1;
         for D_Int'Size use Duration'Size;

         function To_D_Int is new Unchecked_Conversion (Duration, D_Int);
         function To_Duration is new Unchecked_Conversion (D_Int, Duration);

         D_As_Int  : constant D_Int := To_D_Int (Seconds);
         Small_Div : constant D_Int := D_Int (1.0 / Duration'Small);

      begin
         Whole_Part := Integer (D_As_Int / Small_Div);
         Sub_Sec    := Second_Duration
                         (To_Duration (D_As_Int rem Small_Div));
      end;

      Hour       := Hour_Number (Whole_Part / 3600);
      Whole_Part := Whole_Part mod 3600;
      Minute     := Minute_Number (Whole_Part / 60);
      Sec_Num    := Second_Number (Whole_Part mod 60);

      return Time_Of (Year, Month, Day,
                      Hour, Minute, Sec_Num, Sub_Sec, Leap_Second, Time_Zone);
   end Time_Of;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone   : Time_Zones.Time_Offset := 0) return Time
   is
      Cumulative_Days_Before_Month :
        constant array (Month_Number) of Natural :=
          (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

      Ada_Year_Min         : constant Year_Number := Year_Number'First;
      Count                : Integer;
      Elapsed_Leap_Seconds : Duration;
      Fractional_Second    : Duration;
      Next_Leap            : Time;
      Result               : Time;

   begin
      --  The following checks are redundant with respect to the constraint
      --  error checks that should normally be made on parameters, but we
      --  decide to raise Constraint_Error in any case if bad values come in
      --  (as a result of checks being off in the caller, or for other
      --  erroneous or bounded error cases).

      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
        or else not Sub_Second'Valid
        or else not Time_Zone'Valid
      then
         raise Constraint_Error;
      end if;

      --  Start the accumulation from the beginning of Ada time

      Result := Start_Of_Time;

      --  Step 1: Determine the number of leap and non-leap years since 1901
      --  and the input date.

      --  Count the number of four year segments

      Count  := (Year - Ada_Year_Min) / 4;
      Result := Result + Duration (Count * Seconds_In_4_Years);

      --  Count the number of remaining non-leap years

      Count  := (Year - Ada_Year_Min) mod 4;
      Result := Result + Duration (Count * Seconds_In_Non_Leap_Year);

      --  Step 2: Determine the number of days elapsed singe the start of the
      --  input year and add them to the result.

      --  Do not include the current day since it is not over yet

      Count := Cumulative_Days_Before_Month (Month) + Day - 1;

      --  The input year is a leap year and we have passed February

      if (Year mod 4) = 0
        and then Month > 2
      then
         Count := Count + 1;
      end if;

      Result := Result + Duration (Count * Seconds_In_Day);

      --  Step 3: Hour, minute and second processing

      Result := Result + Duration (Hour   * 3600) +
                         Duration (Minute *   60) +
                         Duration (Second);

      --  The sub second may designate a whole second

      if Sub_Second = 1.0 then
         Result            := Result + Duration (1.0);
         Fractional_Second := 0.0;
      else
         Fractional_Second := Sub_Second;
      end if;

      --  Step 4: Time zone processing

      Result := Result - Duration (Time_Zone * 60);

      --  Step 5: The caller wants a leap second

      if Leap_Second then
         Result := Result + Duration (1.0);
      end if;

      --  Step 6: Calculate the number of leap seconds occured since the
      --  start of Ada time and the current point in time. The following
      --  is an approximation which does not yet count leap seconds. It
      --  can be pushed beyond 1 leap second, but not more.

      Cumulative_Leap_Secs
        (Start_Of_Time, Result, Elapsed_Leap_Seconds, Next_Leap);

      Result := Result + Elapsed_Leap_Seconds;

      --  Step 7: Validity check of a leap second occurence. It requires an
      --  additional comparison to Next_Leap to ensure that we landed right
      --  on a valid occurence and that Elapsed_Leap_Seconds did not shoot
      --  past it.

      if Leap_Second
        and then
           not (Result >= Next_Leap
                  and then Result - Duration (1.0) < Next_Leap)
      then
         raise Time_Error;
      end if;

      --  Step 8: Final sanity check on the calculated duration value

      if Result < Start_Of_Time
        or else Result >= End_Of_Time
      then
         raise Time_Error;
      end if;

      --  Step 9: Lastly, add the sub second part

      return Result + Fractional_Second;
   end Time_Of;

   -----------
   -- Value --
   -----------

   function Value
     (Date      : String;
      Time_Zone : Time_Zones.Time_Offset := 0) return Time
   is
      D          : String (1 .. 22);
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0;

   begin
      --  Validity checks

      if not Time_Zone'Valid then
         raise Constraint_Error;
      end if;

      --  Length checks

      if Date'Length /= 19
        and then Date'Length /= 22
      then
         raise Constraint_Error;
      end if;

      --  After the correct length has been determined, it is safe to
      --  copy the Date in order to avoid Date'First + N indexing.

      D (1 .. Date'Length) := Date;

      --  Format checks

      Check_Char (D, '-', 5);
      Check_Char (D, '-', 8);
      Check_Char (D, ' ', 11);
      Check_Char (D, ':', 14);
      Check_Char (D, ':', 17);

      if Date'Length = 22 then
         Check_Char (D, '.', 20);
      end if;

      --  Leading zero checks

      Check_Digit (D, 6);
      Check_Digit (D, 9);
      Check_Digit (D, 12);
      Check_Digit (D, 15);
      Check_Digit (D, 18);

      if Date'Length = 22 then
         Check_Digit (D, 21);
      end if;

      --  Value extraction

      Year   := Year_Number   (Year_Number'Value   (D (1 .. 4)));
      Month  := Month_Number  (Month_Number'Value  (D (6 .. 7)));
      Day    := Day_Number    (Day_Number'Value    (D (9 .. 10)));
      Hour   := Hour_Number   (Hour_Number'Value   (D (12 .. 13)));
      Minute := Minute_Number (Minute_Number'Value (D (15 .. 16)));
      Second := Second_Number (Second_Number'Value (D (18 .. 19)));

      --  Optional part

      if Date'Length = 22 then
         Sub_Second := Second_Duration (Second_Duration'Value (D (20 .. 22)));
      end if;

      --  Sanity checks

      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
        or else not Sub_Second'Valid
      then
         raise Constraint_Error;
      end if;

      return Time_Of (Year, Month, Day,
                      Hour, Minute, Second, Sub_Second, False, Time_Zone);

   exception
      when others => raise Constraint_Error;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Elapsed_Time : String) return Duration is
      D          : String (1 .. 11);
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0;

   begin
      --  Length checks

      if Elapsed_Time'Length /= 8
        and then Elapsed_Time'Length /= 11
      then
         raise Constraint_Error;
      end if;

      --  After the correct length has been determined, it is safe to
      --  copy the Elapsed_Time in order to avoid Date'First + N indexing.

      D (1 .. Elapsed_Time'Length) := Elapsed_Time;

      --  Format checks

      Check_Char (D, ':', 3);
      Check_Char (D, ':', 6);

      if Elapsed_Time'Length = 11 then
         Check_Char (D, '.', 9);
      end if;

      --  Leading zero checks

      Check_Digit (D, 1);
      Check_Digit (D, 4);
      Check_Digit (D, 7);

      if Elapsed_Time'Length = 11 then
         Check_Digit (D, 10);
      end if;

      --  Value extraction

      Hour   := Hour_Number   (Hour_Number'Value   (D (1 .. 2)));
      Minute := Minute_Number (Minute_Number'Value (D (4 .. 5)));
      Second := Second_Number (Second_Number'Value (D (7 .. 8)));

      --  Optional part

      if Elapsed_Time'Length = 11 then
         Sub_Second := Second_Duration (Second_Duration'Value (D (9 .. 11)));
      end if;

      --  Sanity checks

      if not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
        or else not Sub_Second'Valid
      then
         raise Constraint_Error;
      end if;

      return Seconds_Of (Hour, Minute, Second, Sub_Second);

   exception
      when others => raise Constraint_Error;
   end Value;

   ----------
   -- Year --
   ----------

   function Year
     (Date      : Time;
      Time_Zone : Time_Zones.Time_Offset := 0) return Year_Number
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;

   begin
      Split (Date, Year, Month, Day,
             Hour, Minute, Second, Sub_Second, Leap_Second, Time_Zone);
      return Year;
   end Year;

end Ada.Calendar.Formatting;
