------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

with Ada.Unchecked_Conversion;

with System.OS_Primitives;
--  used for Clock

package body Ada.Calendar is

   --------------------------
   -- Implementation Notes --
   --------------------------

   --  In complex algorithms, some variables of type Ada.Calendar.Time carry
   --  suffix _S or _N to denote units of seconds or nanoseconds.
   --
   --  Because time is measured in different units and from different origins
   --  on various targets, a system independent model is incorporated into
   --  Ada.Calendar. The idea behing the design is to encapsulate all target
   --  dependent machinery in a single package, thus providing a uniform
   --  interface to any existing and potential children.

   --     package Ada.Calendar
   --        procedure Split (5 parameters) -------+
   --                                              | Call from local routine
   --     private                                  |
   --        package Formatting_Operations         |
   --           procedure Split (11 parameters) <--+
   --        end Formatting_Operations             |
   --     end Ada.Calendar                         |
   --                                              |
   --     package Ada.Calendar.Formatting          | Call from child routine
   --        procedure Split (9 or 10 parameters) -+
   --     end Ada.Calendar.Formatting

   --  The behaviour of the interfacing routines is controlled via various
   --  flags. All new Ada 2005 types from children of Ada.Calendar are
   --  emulated by a similar type. For instance, type Day_Number is replaced
   --  by Integer in various routines. One ramification of this model is that
   --  the caller site must perform validity checks on returned results.
   --  The end result of this model is the lack of target specific files per
   --  child of Ada.Calendar (a-calfor, a-calfor-vms, a-calfor-vxwors, etc).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time;
      End_Date      : Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out Time);
   --  Elapsed_Leaps is the sum of the leap seconds that have occured on or
   --  after Start_Date and before (strictly before) End_Date. Next_Leap_Sec
   --  represents the next leap second occurence on or after End_Date. If
   --  there are no leaps seconds after End_Date, After_Last_Leap is returned.
   --  After_Last_Leap can be used as End_Date to count all the leap seconds
   --  that have occured on or after Start_Date.
   --
   --  Note: Any sub seconds of Start_Date and End_Date are discarded before
   --  the calculations are done. For instance: if 113 seconds is a leap
   --  second (it isn't) and 113.5 is input as an End_Date, the leap second
   --  at 113 will not be counted in Leaps_Between, but it will be returned
   --  as Next_Leap_Sec. Thus, if the caller wants to know if the End_Date is
   --  a leap second, the comparison should be:
   --
   --     End_Date >= Next_Leap_Sec;
   --
   --  After_Last_Leap is designed so that this comparison works without
   --  having to first check if Next_Leap_Sec is a valid leap second.

   function To_Abs_Duration (T : Time) return Duration;
   --  Convert a time value into a duration value. Note that the returned
   --  duration is always positive.

   function To_Abs_Time (D : Duration) return Time;
   --  Return the time equivalent of a duration value. Since time cannot be
   --  negative, the absolute value of D is used. It is upto the called to
   --  decide how to handle negative durations converted into time.

   ---------------------
   -- Local Constants --
   ---------------------

   Ada_Min_Year          : constant Year_Number := Year_Number'First;
   After_Last_Leap       : constant Time := Time'Last;
   Leap_Seconds_Count    : constant Natural := 23;
   Secs_In_Four_Years    : constant := (3 * 365 + 366) * Secs_In_Day;
   Secs_In_Non_Leap_Year : constant := 365 * Secs_In_Day;
   Time_Zero             : constant Time := Time'First;

   --  Even though the upper bound of Ada time is 2399-12-31 86_399.999999999
   --  GMT, it must be shifted to include all leap seconds.

   Ada_High_And_Leaps : constant Time :=
                          Ada_High + Time (Leap_Seconds_Count) * Nano;

   Hard_Ada_High_And_Leaps : constant Time :=
                               Hard_Ada_High +
                               Time (Leap_Seconds_Count) * Nano;

   --  The Unix lower time bound expressed as nanoseconds since the
   --  start of Ada time in GMT.

   Unix_Min : constant Time := (17 * 366 + 52 * 365) * Nanos_In_Day;

   Cumulative_Days_Before_Month :
     constant array (Month_Number) of Natural :=
       (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

   Leap_Second_Times : array (1 .. Leap_Seconds_Count) of Time;
   --  Each value represents a time value which is one second before a leap
   --  second occurence. This table is populated during the elaboration of
   --  Ada.Calendar.

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);

   begin
      if Right = 0.0 then
         return Left;

      elsif Right < 0.0 then

         --  Type Duration has one additional number in its negative subrange,
         --  which is Duration'First. The subsequent invocation of "-" will
         --  perform among other things an Unchecked_Conversion on that
         --  particular value, causing overflow. If not properly handled,
         --  the erroneous value will cause an infinite recursion between "+"
         --  and "-". To properly handle this boundary case, we make a small
         --  adjustment of one second to Duration'First.

         if Right = Duration'First then
            return Left - abs (Right + 1.0) - 1.0;
         else
            return Left - abs (Right);
         end if;

      else
         declare
            --  The input time value has been normalized to GMT

            Result : constant Time := Left + To_Abs_Time (Right);

         begin
            --  The end result may excede the upper bound of Ada time. Note
            --  that the comparison operator is ">=" rather than ">" since
            --  the smallest increment of 0.000000001 to the legal end of
            --  time (2399-12-31 86_399.999999999) will render the result
            --  equal to Ada_High (2400-1-1 0.0).

            if Result >= Ada_High_And_Leaps then
               raise Time_Error;
            end if;

            return Result;
         end;
      end if;

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
   begin
      return Right + Left;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);

   begin
      if Right = 0.0 then
         return Left;

      elsif Right < 0.0 then
         return Left + abs (Right);

      else
         declare
            Result  : Time;
            Right_T : constant Time := To_Abs_Time (Right);

         begin
            --  Subtracting a larger time value from a smaller time value
            --  will cause a wrap around since Time is a modular type. Note
            --  that the time value has been normalized to GMT.

            if Left < Right_T then
               raise Time_Error;
            end if;

            Result := Left - Right_T;

            if Result < Ada_Low
              or else Result > Ada_High_And_Leaps
            then
               raise Time_Error;
            end if;

            return Result;
         end;
      end if;

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);

      function To_Time is new Ada.Unchecked_Conversion (Duration, Time);

      --  Since the absolute values of the upper and lower bound of duration
      --  are denoted by the same number, it is sufficend to use Duration'Last
      --  when performing out of range checks.

      Duration_Bound : constant Time := To_Time (Duration'Last);

      Earlier  : Time;
      Later    : Time;
      Negate   : Boolean := False;
      Result   : Time;
      Result_D : Duration;

   begin
      --  This routine becomes a little tricky since time cannot be negative,
      --  but the subtraction of two time values can produce a negative value.

      if Left > Right then
         Later   := Left;
         Earlier := Right;
      else
         Later   := Right;
         Earlier := Left;
         Negate  := True;
      end if;

      Result := Later - Earlier;

      --  Check whether the resulting difference is within the range of type
      --  Duration. The following two conditions are examined with the same
      --  piece of code:
      --
      --     positive result > positive upper bound of duration
      --
      --     negative (negative result) > abs (negative bound of duration)

      if Result > Duration_Bound then
         raise Time_Error;
      end if;

      Result_D := To_Abs_Duration (Result);

      if Negate then
         Result_D := -Result_D;
      end if;

      return Result_D;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Time_Rep (Left) < Time_Rep (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Time_Rep (Left) <= Time_Rep (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Time_Rep (Left) > Time_Rep (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Time_Rep (Left) >= Time_Rep (Right);
   end ">=";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Elapsed_Leaps : Natural;
      Next_Leap     : Time;

      --  The system clock returns the time in GMT since the Unix Epoch of
      --  1970-1-1 0.0. We perform an origin shift to the Ada Epoch by adding
      --  the number of nanoseconds between the two origins.

      Now : Time := To_Abs_Time (System.OS_Primitives.Clock) + Unix_Min;

      Rounded_Now : constant Time := Now - (Now mod Nano);

   begin
      --  Determine how many leap seconds have elapsed until this moment

      Cumulative_Leap_Seconds (Time_Zero, Now, Elapsed_Leaps, Next_Leap);

      Now := Now + Time (Elapsed_Leaps) * Nano;

      --  The system clock may fall exactly on a leap second occurence

      if Rounded_Now = Next_Leap then
         Now := Now + Time (1) * Nano;
      end if;

      --  Add the buffer set aside for time zone processing since Split in
      --  Ada.Calendar.Formatting_Operations expects it to be there.

      return Now + Buffer_N;
   end Clock;

   -----------------------------
   -- Cumulative_Leap_Seconds --
   -----------------------------

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time;
      End_Date      : Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out Time)
   is
      End_Index   : Positive;
      End_T       : Time := End_Date;
      Start_Index : Positive;
      Start_T     : Time := Start_Date;

   begin
      --  Both input dates need to be normalized to GMT in order for this
      --  routine to work properly.

      pragma Assert (End_Date >= Start_Date);

      Next_Leap_Sec := After_Last_Leap;

      --  Make sure that the end date does not excede the upper bound
      --  of Ada time.

      if End_Date > Ada_High then
         End_T := Ada_High;
      end if;

      --  Remove the sub seconds from both dates

      Start_T := Start_T - (Start_T mod Nano);
      End_T   := End_T   - (End_T   mod Nano);

      --  Some trivial cases:
      --                     Leap 1 . . . Leap N
      --  ---+========+------+############+-------+========+-----
      --     Start_T  End_T                       Start_T  End_T

      if End_T < Leap_Second_Times (1) then
         Elapsed_Leaps := 0;
         Next_Leap_Sec := Leap_Second_Times (1);
         return;

      elsif Start_T > Leap_Second_Times (Leap_Seconds_Count) then
         Elapsed_Leaps := 0;
         Next_Leap_Sec := After_Last_Leap;
         return;
      end if;

      --  Perform the calculations only if the start date is within the leap
      --  second occurences table.

      if Start_T <= Leap_Second_Times (Leap_Seconds_Count) then

         --    1    2                  N - 1   N
         --  +----+----+--  . . .  --+-------+---+
         --  | T1 | T2 |             | N - 1 | N |
         --  +----+----+--  . . .  --+-------+---+
         --         ^                   ^
         --         | Start_Index       | End_Index
         --         +-------------------+
         --             Leaps_Between

         --  The idea behind the algorithm is to iterate and find two
         --  closest dates which are after Start_T and End_T. Their
         --  corresponding index difference denotes the number of leap
         --  seconds elapsed.

         Start_Index := 1;
         loop
            exit when Leap_Second_Times (Start_Index) >= Start_T;
            Start_Index := Start_Index + 1;
         end loop;

         End_Index := Start_Index;
         loop
            exit when End_Index > Leap_Seconds_Count
              or else Leap_Second_Times (End_Index) >= End_T;
            End_Index := End_Index + 1;
         end loop;

         if End_Index <= Leap_Seconds_Count then
            Next_Leap_Sec := Leap_Second_Times (End_Index);
         end if;

         Elapsed_Leaps := End_Index - Start_Index;

      else
         Elapsed_Leaps := 0;
      end if;
   end Cumulative_Leap_Seconds;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return D;
   end Day;

   -------------
   -- Is_Leap --
   -------------

   function Is_Leap (Year : Year_Number) return Boolean is
   begin
      --  Leap centenial years

      if Year mod 400 = 0 then
         return True;

      --  Non-leap centenial years

      elsif Year mod 100 = 0 then
         return False;

      --  Regular years

      else
         return Year mod 4 = 0;
      end if;
   end Is_Leap;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return M;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return S;
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      H  : Integer;
      M  : Integer;
      Se : Integer;
      Ss : Duration;
      Le : Boolean;
      Tz : constant Long_Integer :=
             Time_Zones_Operations.UTC_Time_Offset (Date) / 60;

   begin
      Formatting_Operations.Split
        (Date, Year, Month, Day, Seconds, H, M, Se, Ss, Le, Tz);

      --  Validity checks

      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Seconds'Valid
      then
         raise Time_Error;
      end if;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time
   is
      --  The values in the following constants are irrelevant, they are just
      --  placeholders; the choice of constructing a Day_Duration value is
      --  controlled by the Use_Day_Secs flag.

      H  : constant Integer := 1;
      M  : constant Integer := 1;
      Se : constant Integer := 1;
      Ss : constant Duration := 0.1;

      Mid_Offset : Long_Integer;
      Mid_Result : Time;
      Offset     : Long_Integer;

   begin
      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Seconds'Valid
      then
         raise Time_Error;
      end if;

      --  Building a time value in a local time zone is tricky since the
      --  local time zone offset at the point of creation may not be the
      --  same as the actual time zone offset designated by the input
      --  values. The following example is relevant to New York, USA.
      --
      --     Creation date: 2006-10-10 0.0  Offset -240 mins (in DST)
      --     Actual date  : 1901-01-01 0.0  Offset -300 mins (no DST)

      --  We first start by obtaining the current local time zone offset
      --  using Ada.Calendar.Clock, then building an intermediate time
      --  value using that offset.

      Mid_Offset := Time_Zones_Operations.UTC_Time_Offset (Clock) / 60;
      Mid_Result := Formatting_Operations.Time_Of
                      (Year, Month, Day, Seconds, H, M, Se, Ss,
                       Leap_Sec     => False,
                       Leap_Checks  => False,
                       Use_Day_Secs => True,
                       Time_Zone    => Mid_Offset);

      --  This is the true local time zone offset of the input time values

      Offset := Time_Zones_Operations.UTC_Time_Offset (Mid_Result) / 60;

      --  It is possible that at the point of invocation of Time_Of, both
      --  the current local time zone offset and the one designated by the
      --  input values are in the same DST mode.

      if Offset = Mid_Offset then
         return Mid_Result;

      --  In this case we must calculate the new time with the new offset. It
      --  is no sufficient to just take the relative difference between the
      --  two offsets and adjust the intermediate result, because this does not
      --  work around leap second times.

      else
         declare
            Result : constant Time :=
                       Formatting_Operations.Time_Of
                         (Year, Month, Day, Seconds, H, M, Se, Ss,
                         Leap_Sec     => False,
                         Leap_Checks  => False,
                         Use_Day_Secs => True,
                         Time_Zone    => Offset);

         begin
            return Result;
         end;
      end if;
   end Time_Of;

   ---------------------
   -- To_Abs_Duration --
   ---------------------

   function To_Abs_Duration (T : Time) return Duration is
      pragma Unsuppress (Overflow_Check);
      function To_Duration is new Ada.Unchecked_Conversion (Time, Duration);

   begin
      return To_Duration (T);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end To_Abs_Duration;

   -----------------
   -- To_Abs_Time --
   -----------------

   function To_Abs_Time (D : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
      function To_Time is new Ada.Unchecked_Conversion (Duration, Time);

   begin
      --  This operation assumes that D is positive

      if D < 0.0 then
         raise Constraint_Error;
      end if;

      return To_Time (D);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end To_Abs_Time;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return Y;
   end Year;

   --  The following packages assume that Time is a modular 64 bit integer
   --  type, the units are nanoseconds and the origin is the start of Ada
   --  time (1901-1-1 0.0).

   ---------------------------
   -- Arithmetic_Operations --
   ---------------------------

   package body Arithmetic_Operations is

      ---------
      -- Add --
      ---------

      function Add (Date : Time; Days : Long_Integer) return Time is
      begin
         if Days = 0 then
            return Date;

         elsif Days < 0 then
            return Subtract (Date, abs (Days));

         else
            declare
               Result : constant Time := Date + Time (Days) * Nanos_In_Day;

            begin
               --  The result excedes the upper bound of Ada time

               if Result > Ada_High_And_Leaps then
                  raise Time_Error;
               end if;

               return Result;
            end;
         end if;

      exception
         when Constraint_Error =>
            raise Time_Error;
      end Add;

      ----------------
      -- Difference --
      ----------------

      procedure Difference
        (Left         : Time;
         Right        : Time;
         Days         : out Long_Integer;
         Seconds      : out Duration;
         Leap_Seconds : out Integer)
      is
         Diff_N        : Time;
         Diff_S        : Time;
         Earlier       : Time;
         Elapsed_Leaps : Natural;
         Later         : Time;
         Negate        : Boolean := False;
         Next_Leap     : Time;
         Sub_Seconds   : Duration;

      begin
         --  Both input time values are assumed to be in GMT

         if Left >= Right then
            Later   := Left;
            Earlier := Right;
         else
            Later   := Right;
            Earlier := Left;
            Negate  := True;
         end if;

         --  First process the leap seconds

         Cumulative_Leap_Seconds (Earlier, Later, Elapsed_Leaps, Next_Leap);

         if Later >= Next_Leap then
            Elapsed_Leaps := Elapsed_Leaps + 1;
         end if;

         Diff_N := Later - Earlier - Time (Elapsed_Leaps) * Nano;

         --  Sub second processing

         Sub_Seconds := Duration (Diff_N mod Nano) / Nano_F;

         --  Convert to seconds. Note that his action eliminates the sub
         --  seconds automatically.

         Diff_S := Diff_N / Nano;

         Days := Long_Integer (Diff_S / Secs_In_Day);
         Seconds := Duration (Diff_S mod Secs_In_Day) + Sub_Seconds;
         Leap_Seconds := Integer (Elapsed_Leaps);

         if Negate then
            Days         := -Days;
            Seconds      := -Seconds;
            Leap_Seconds := -Leap_Seconds;
         end if;
      end Difference;

      --------------
      -- Subtract --
      --------------

      function Subtract (Date : Time; Days : Long_Integer) return Time is
      begin
         if Days = 0 then
            return Date;

         elsif Days < 0 then
            return Add (Date, abs (Days));

         else
            declare
               Days_T : constant Time := Time (Days) * Nanos_In_Day;
               Result : Time;

            begin
               --  Subtracting a larger number of days from a smaller time
               --  value will cause wrap around since time is a modular type.

               if Date < Days_T then
                  raise Time_Error;
               end if;

               Result := Date - Days_T;

               if Result < Ada_Low
                 or else Result > Ada_High_And_Leaps
               then
                  raise Time_Error;
               end if;

               return Result;
            end;
         end if;

      exception
         when Constraint_Error =>
            raise Time_Error;
      end Subtract;
   end Arithmetic_Operations;

   ----------------------
   -- Delay_Operations --
   ----------------------

   package body Delays_Operations is

      -----------------
      -- To_Duration --
      -----------------

      function To_Duration (Ada_Time : Time) return Duration is
         Elapsed_Leaps : Natural;
         Modified_Time : Time;
         Next_Leap     : Time;
         Result        : Duration;
         Rounded_Time  : Time;

      begin
         Modified_Time := Ada_Time;
         Rounded_Time  := Modified_Time - (Modified_Time mod Nano);

         --  Remove all leap seconds

         Cumulative_Leap_Seconds
           (Time_Zero, Modified_Time, Elapsed_Leaps, Next_Leap);

         Modified_Time := Modified_Time - Time (Elapsed_Leaps) * Nano;

         --  The input time value may fall on a leap second occurence

         if Rounded_Time = Next_Leap then
            Modified_Time := Modified_Time - Time (1) * Nano;
         end if;

         --  Perform a shift in origins

         Result := Modified_Time - Unix_Min;

         --  Remove the buffer period used in time zone processing

         return Result - Buffer_D;
      end To_Duration;
   end Delays_Operations;

   ---------------------------
   -- Formatting_Operations --
   ---------------------------

   package body Formatting_Operations is

      -----------------
      -- Day_Of_Week --
      -----------------

      function Day_Of_Week (Date : Time) return Integer is
         Y  : Year_Number;
         Mo : Month_Number;
         D  : Day_Number;
         Dd : Day_Duration;
         H  : Integer;
         Mi : Integer;
         Se : Integer;
         Su : Duration;
         Le : Boolean;

         Day_Count     : Long_Integer;
         Midday_Date_S : Time;

      begin
         Formatting_Operations.Split
           (Date, Y, Mo, D, Dd, H, Mi, Se, Su, Le, 0);

         --  Build a time value in the middle of the same day, remove the
         --  lower buffer and convert the time value to seconds.

         Midday_Date_S := (Formatting_Operations.Time_Of
                             (Y, Mo, D, 0.0, 12, 0, 0, 0.0,
                              Leap_Sec     => False,
                              Leap_Checks  => False,
                              Use_Day_Secs => False,
                              Time_Zone    => 0) - Buffer_N) / Nano;

         --  Count the number of days since the start of Ada time. 1901-1-1
         --  GMT was a Tuesday.

         Day_Count := Long_Integer (Midday_Date_S / Secs_In_Day) + 1;

         return Integer (Day_Count mod 7);
      end Day_Of_Week;

      -----------
      -- Split --
      -----------

      procedure Split
        (Date         : Time;
         Year         : out Year_Number;
         Month        : out Month_Number;
         Day          : out Day_Number;
         Day_Secs     : out Day_Duration;
         Hour         : out Integer;
         Minute       : out Integer;
         Second       : out Integer;
         Sub_Sec      : out Duration;
         Leap_Sec     : out Boolean;
         Time_Zone    : Long_Integer)
      is
         --  The following constants represent the number of nanoseconds
         --  elapsed since the start of Ada time to and including the non
         --  leap centenial years.

         Year_2101 : constant Time := (49 * 366 + 151 * 365) * Nanos_In_Day;
         Year_2201 : constant Time := (73 * 366 + 227 * 365) * Nanos_In_Day;
         Year_2301 : constant Time := (97 * 366 + 303 * 365) * Nanos_In_Day;

         Abs_Time_Zone   : Time;
         Day_Seconds     : Natural;
         Elapsed_Leaps   : Natural;
         Four_Year_Segs  : Natural;
         Hour_Seconds    : Natural;
         Is_Leap_Year    : Boolean;
         Modified_Date_N : Time;
         Modified_Date_S : Time;
         Next_Leap_N     : Time;
         Rem_Years       : Natural;
         Rounded_Date_N  : Time;
         Year_Day        : Natural;

      begin
         Modified_Date_N := Date;

         if Modified_Date_N < Hard_Ada_Low
           or else Modified_Date_N > Hard_Ada_High_And_Leaps
         then
            raise Time_Error;
         end if;

         --  Step 1: Leap seconds processing in GMT

         --  Day_Duration:    86_398  86_399  X (86_400) 0 (1)  1 (2)
         --  Time        :  --+-------+-------+----------+------+-->
         --  Seconds     :    58      59      60 (Leap)  1      2

         --   o Modified_Date_N falls between 86_399 and X (86_400)
         --       Elapsed_Leaps  = X - 1 leaps
         --       Rounded_Date_N = 86_399
         --       Next_Leap_N    = X (86_400)
         --       Leap_Sec       = False

         --   o Modified_Date_N falls exactly on X (86_400)
         --       Elapsed_Leaps  = X - 1 leaps
         --       Rounded_Date_N = X (86_400)
         --       Next_Leap_N    = X (86_400)
         --       Leap_Sec       = True
         --     An invisible leap second will be added.

         --   o Modified_Date_N falls between X (86_400) and 0 (1)
         --       Elapsed_Leaps  = X - 1 leaps
         --       Rounded_Date_N = X (86_400)
         --       Next_Leap_N    = X (86_400)
         --       Leap_Sec       = True
         --     An invisible leap second will be added.

         --   o Modified_Date_N falls on 0 (1)
         --       Elapsed_Leaps  = X
         --       Rounded_Date_N = 0 (1)
         --       Next_Leap_N    = X + 1
         --       Leap_Sec       = False
         --     The invisible leap second has already been accounted for in
         --     Elapsed_Leaps.

         Cumulative_Leap_Seconds
           (Time_Zero, Modified_Date_N, Elapsed_Leaps, Next_Leap_N);

         Rounded_Date_N  := Modified_Date_N - (Modified_Date_N mod Nano);
         Leap_Sec        := Rounded_Date_N = Next_Leap_N;
         Modified_Date_N := Modified_Date_N - Time (Elapsed_Leaps) * Nano;

         if Leap_Sec then
            Modified_Date_N := Modified_Date_N - Time (1) * Nano;
         end if;

         --  Step 2: Time zone processing. This action converts the input date
         --  from GMT to the requested time zone.

         if Time_Zone /= 0 then
            Abs_Time_Zone := Time (abs (Time_Zone)) * 60 * Nano;

            if Time_Zone < 0 then
               --  The following test is obsolete since the date already
               --  contains the dedicated buffer for time zones, thus no
               --  error will be raised. However it is a good idea to keep
               --  it should the representation of time change.

               Modified_Date_N := Modified_Date_N - Abs_Time_Zone;
            else
               Modified_Date_N := Modified_Date_N + Abs_Time_Zone;
            end if;
         end if;

         --  After the elapsed leap seconds have been removed and the date
         --  has been normalized, it should fall withing the soft bounds of
         --  Ada time.

         if Modified_Date_N < Ada_Low
           or else Modified_Date_N > Ada_High
         then
            raise Time_Error;
         end if;

         --  Before any additional arithmetic is performed we must remove the
         --  lower buffer period since it will be accounted as few additional
         --  days.

         Modified_Date_N := Modified_Date_N - Buffer_N;

         --  Step 3: Non-leap centenial year adjustment in local time zone

         --  In order for all divisions to work properly and to avoid more
         --  complicated arithmetic, we add fake Febriary 29s to dates which
         --  occur after a non-leap centenial year.

         if Modified_Date_N >= Year_2301 then
            Modified_Date_N := Modified_Date_N + Time (3) * Nanos_In_Day;

         elsif Modified_Date_N >= Year_2201 then
            Modified_Date_N := Modified_Date_N + Time (2) * Nanos_In_Day;

         elsif Modified_Date_N >= Year_2101 then
            Modified_Date_N := Modified_Date_N + Time (1) * Nanos_In_Day;
         end if;

         --  Step 4: Sub second processing in local time zone

         Sub_Sec := Duration (Modified_Date_N mod Nano) / Nano_F;

         --  Convert the date into seconds, the sub seconds are automatically
         --  dropped.

         Modified_Date_S := Modified_Date_N / Nano;

         --  Step 5: Year processing in local time zone. Determine the number
         --  of four year segments since the start of Ada time and the input
         --  date.

         Four_Year_Segs := Natural (Modified_Date_S / Secs_In_Four_Years);

         if Four_Year_Segs > 0 then
            Modified_Date_S := Modified_Date_S - Time (Four_Year_Segs) *
                                                 Secs_In_Four_Years;
         end if;

         --  Calculate the remaining non-leap years

         Rem_Years := Natural (Modified_Date_S / Secs_In_Non_Leap_Year);

         if Rem_Years > 3 then
            Rem_Years := 3;
         end if;

         Modified_Date_S := Modified_Date_S - Time (Rem_Years) *
                                              Secs_In_Non_Leap_Year;

         Year := Ada_Min_Year + Natural (4 * Four_Year_Segs + Rem_Years);
         Is_Leap_Year := Is_Leap (Year);

         --  Step 6: Month and day processing in local time zone

         Year_Day := Natural (Modified_Date_S / Secs_In_Day) + 1;

         Month := 1;

         --  Processing for months after January

         if Year_Day > 31 then
            Month    := 2;
            Year_Day := Year_Day - 31;

            --  Processing for a new month or a leap February

            if Year_Day > 28
              and then (not Is_Leap_Year
                          or else Year_Day > 29)
            then
               Month    := 3;
               Year_Day := Year_Day - 28;

               if Is_Leap_Year then
                  Year_Day := Year_Day - 1;
               end if;

               --  Remaining months

               while Year_Day > Days_In_Month (Month) loop
                  Year_Day := Year_Day - Days_In_Month (Month);
                  Month    := Month + 1;
               end loop;
            end if;
         end if;

         --  Step 7: Hour, minute, second and sub second processing in local
         --  time zone.

         Day          := Day_Number (Year_Day);
         Day_Seconds  := Integer (Modified_Date_S mod Secs_In_Day);
         Day_Secs     := Duration (Day_Seconds) + Sub_Sec;
         Hour         := Day_Seconds / 3_600;
         Hour_Seconds := Day_Seconds mod 3_600;
         Minute       := Hour_Seconds / 60;
         Second       := Hour_Seconds mod 60;
      end Split;

      -------------
      -- Time_Of --
      -------------

      function Time_Of
        (Year         : Year_Number;
         Month        : Month_Number;
         Day          : Day_Number;
         Day_Secs     : Day_Duration;
         Hour         : Integer;
         Minute       : Integer;
         Second       : Integer;
         Sub_Sec      : Duration;
         Leap_Sec     : Boolean;
         Leap_Checks  : Boolean;
         Use_Day_Secs : Boolean;
         Time_Zone    : Long_Integer) return Time
      is
         Abs_Time_Zone    : Time;
         Count            : Integer;
         Elapsed_Leaps    : Natural;
         Next_Leap_N      : Time;
         Result_N         : Time;
         Rounded_Result_N : Time;

      begin
         --  Step 1: Check whether the day, month and year form a valid date

         if Day > Days_In_Month (Month)
           and then (Day /= 29 or else Month /= 2 or else not Is_Leap (Year))
         then
            raise Time_Error;
         end if;

         --  Start accumulating nanoseconds from the low bound of Ada time.
         --  Note: This starting point includes the lower buffer dedicated
         --  to time zones.

         Result_N := Ada_Low;

         --  Step 2: Year processing and centenial year adjustment. Determine
         --  the number of four year segments since the start of Ada time and
         --  the input date.

         Count    := (Year - Year_Number'First) / 4;
         Result_N := Result_N + Time (Count) * Secs_In_Four_Years * Nano;

         --  Note that non-leap centenial years are automatically considered
         --  leap in the operation above. An adjustment of several days is
         --  required to compensate for this.

         if Year > 2300 then
            Result_N := Result_N - Time (3) * Nanos_In_Day;

         elsif Year > 2200 then
            Result_N := Result_N - Time (2) * Nanos_In_Day;

         elsif Year > 2100 then
            Result_N := Result_N - Time (1) * Nanos_In_Day;
         end if;

         --  Add the remaining non-leap years

         Count    := (Year - Year_Number'First) mod 4;
         Result_N := Result_N + Time (Count) * Secs_In_Non_Leap_Year * Nano;

         --  Step 3: Day of month processing. Determine the number of days
         --  since the start of the current year. Do not add the current
         --  day since it has not elapsed yet.

         Count := Cumulative_Days_Before_Month (Month) + Day - 1;

         --  The input year is leap and we have passed February

         if Is_Leap (Year)
           and then Month > 2
         then
            Count := Count + 1;
         end if;

         Result_N := Result_N + Time (Count) * Nanos_In_Day;

         --  Step 4: Hour, minute, second and sub second processing

         if Use_Day_Secs then
            Result_N := Result_N + To_Abs_Time (Day_Secs);

         else
            Result_N := Result_N +
              Time (Hour * 3_600 + Minute * 60 + Second) * Nano;

            if Sub_Sec = 1.0 then
               Result_N := Result_N + Time (1) * Nano;
            else
               Result_N := Result_N + To_Abs_Time (Sub_Sec);
            end if;
         end if;

         --  Step 4: Time zone processing. At this point we have built an
         --  arbitrary time value which is not related to any time zone.
         --  For simplicity, the time value is normalized to GMT, producing
         --  a uniform representation which can be treated by arithmetic
         --  operations for instance without any additional corrections.

         if Result_N < Ada_Low
           or else Result_N > Ada_High
         then
            raise Time_Error;
         end if;

         if Time_Zone /= 0 then
            Abs_Time_Zone := Time (abs (Time_Zone)) * 60 * Nano;

            if Time_Zone < 0 then
               Result_N := Result_N + Abs_Time_Zone;
            else
               --  The following test is obsolete since the result already
               --  contains the dedicated buffer for time zones, thus no
               --  error will be raised. However it is a good idea to keep
               --  this comparison should the representation of time change.

               if Result_N < Abs_Time_Zone then
                  raise Time_Error;
               end if;

               Result_N := Result_N - Abs_Time_Zone;
            end if;
         end if;

         --  Step 5: Leap seconds processing in GMT

         Cumulative_Leap_Seconds
           (Time_Zero, Result_N, Elapsed_Leaps, Next_Leap_N);

         Result_N := Result_N + Time (Elapsed_Leaps) * Nano;

         --  An Ada 2005 caller requesting an explicit leap second or an Ada
         --  95 caller accounting for an invisible leap second.

         Rounded_Result_N := Result_N - (Result_N mod Nano);

         if Leap_Sec
           or else Rounded_Result_N = Next_Leap_N
         then
            Result_N := Result_N + Time (1) * Nano;
            Rounded_Result_N := Rounded_Result_N + Time (1) * Nano;
         end if;

         --  Leap second validity check

         if Leap_Checks
           and then Leap_Sec
           and then Rounded_Result_N /= Next_Leap_N
         then
            raise Time_Error;
         end if;

         --  Final bounds check

         if Result_N < Hard_Ada_Low
           or else Result_N > Hard_Ada_High_And_Leaps
         then
            raise Time_Error;
         end if;

         return Result_N;
      end Time_Of;
   end Formatting_Operations;

   ---------------------------
   -- Time_Zones_Operations --
   ---------------------------

   package body Time_Zones_Operations is

      --  The Unix time bounds in seconds: 1970/1/1 .. 2037/1/1

      Unix_Min : constant Time :=
                   Time (17 * 366 + 52 * 365 + 2) * Secs_In_Day;
      --  1970/1/1

      Unix_Max : constant Time :=
                   Time (34 * 366 + 102 * 365 + 2) * Secs_In_Day +
                   Time (Leap_Seconds_Count);
      --  2037/1/1

      --  The following constants denote February 28 during non-leap
      --  centenial years, the units are nanoseconds.

      T_2100_2_28 : constant Time :=
                      (Time (49 * 366 + 150 * 365 + 59 + 2) * Secs_In_Day +
                         Time (Leap_Seconds_Count)) * Nano;

      T_2200_2_28 : constant Time :=
                      (Time (73 * 366 + 226 * 365 + 59 + 2) * Secs_In_Day +
                         Time (Leap_Seconds_Count)) * Nano;

      T_2300_2_28 : constant Time :=
                      (Time (97 * 366 + 302 * 365 + 59 + 2) * Secs_In_Day +
                         Time (Leap_Seconds_Count)) * Nano;

      --  56 years (14 leap years + 42 non leap years) in seconds:

      Secs_In_56_Years : constant := (14 * 366 + 42 * 365) * Secs_In_Day;

      --  Base C types. There is no point dragging in Interfaces.C just for
      --  these four types.

      type char_Pointer is access Character;
      subtype int is Integer;
      subtype long is Long_Integer;
      type long_Pointer is access all long;

      --  The Ada equivalent of struct tm and type time_t

      type tm is record
         tm_sec    : int;           -- seconds after the minute (0 .. 60)
         tm_min    : int;           -- minutes after the hour (0 .. 59)
         tm_hour   : int;           -- hours since midnight (0 .. 24)
         tm_mday   : int;           -- day of the month (1 .. 31)
         tm_mon    : int;           -- months since January (0 .. 11)
         tm_year   : int;           -- years since 1900
         tm_wday   : int;           -- days since Sunday (0 .. 6)
         tm_yday   : int;           -- days since January 1 (0 .. 365)
         tm_isdst  : int;           -- Daylight Savings Time flag (-1 .. 1)
         tm_gmtoff : long;          -- offset from UTC in seconds
         tm_zone   : char_Pointer;  -- timezone abbreviation
      end record;

      type tm_Pointer is access all tm;

      subtype time_t is long;
      type time_t_Pointer is access all time_t;

      procedure localtime_tzoff
       (C   : time_t_Pointer;
        res : tm_Pointer;
        off : long_Pointer);
      pragma Import (C, localtime_tzoff, "__gnat_localtime_tzoff");
      --  This is a lightweight wrapper around the system library function
      --  localtime_r. Parameter 'off' captures the UTC offset which is either
      --  retrieved from the tm struct or calculated from the 'timezone' extern
      --  and the tm_isdst flag in the tm struct.

      ---------------------
      -- UTC_Time_Offset --
      ---------------------

      function UTC_Time_Offset (Date : Time) return Long_Integer is

         Adj_Cent   : Integer := 0;
         Adj_Date_N : Time;
         Adj_Date_S : Time;
         Offset     : aliased long;
         Secs_T     : aliased time_t;
         Secs_TM    : aliased tm;

      begin
         Adj_Date_N := Date;

         --  Dates which are 56 years appart fall on the same day, day light
         --  saving and so on. Non-leap centenial years violate this rule by
         --  one day and as a consequence, special adjustment is needed.

         if Adj_Date_N > T_2100_2_28 then
            if Adj_Date_N > T_2200_2_28 then
               if Adj_Date_N > T_2300_2_28 then
                  Adj_Cent := 3;
               else
                  Adj_Cent := 2;
               end if;

            else
               Adj_Cent := 1;
            end if;
         end if;

         if Adj_Cent > 0 then
            Adj_Date_N := Adj_Date_N - Time (Adj_Cent) * Nanos_In_Day;
         end if;

         --  Convert to seconds and shift date within bounds of Unix time

         Adj_Date_S := Adj_Date_N / Nano;
         while Adj_Date_S < Unix_Min loop
            Adj_Date_S := Adj_Date_S + Secs_In_56_Years;
         end loop;

         while Adj_Date_S >= Unix_Max loop
            Adj_Date_S := Adj_Date_S - Secs_In_56_Years;
         end loop;

         --  Perform a shift in origins from Ada to Unix

         Adj_Date_S := Adj_Date_S - Unix_Min;

         Secs_T := time_t (Adj_Date_S);

         localtime_tzoff
           (Secs_T'Unchecked_Access,
            Secs_TM'Unchecked_Access,
            Offset'Unchecked_Access);

         return Offset;
      end UTC_Time_Offset;
   end Time_Zones_Operations;

--  Start of elaboration code for Ada.Calendar

begin
   System.OS_Primitives.Initialize;

   --  Population of the leap seconds table

   declare
      type Leap_Second_Date is record
         Year  : Year_Number;
         Month : Month_Number;
         Day   : Day_Number;
      end record;

      Leap_Second_Dates :
        constant array (1 .. Leap_Seconds_Count) of Leap_Second_Date :=
          ((1972,  6, 30), (1972, 12, 31), (1973, 12, 31), (1974, 12, 31),
           (1975, 12, 31), (1976, 12, 31), (1977, 12, 31), (1978, 12, 31),
           (1979, 12, 31), (1981,  6, 30), (1982,  6, 30), (1983,  6, 30),
           (1985,  6, 30), (1987, 12, 31), (1989, 12, 31), (1990, 12, 31),
           (1992,  6, 30), (1993,  6, 30), (1994,  6, 30), (1995, 12, 31),
           (1997,  6, 30), (1998, 12, 31), (2005, 12, 31));

      Days_In_Four_Years : constant := 365 * 3 + 366;

      Days  : Natural;
      Leap  : Leap_Second_Date;
      Years : Natural;

   begin
      for Index in 1 .. Leap_Seconds_Count loop
         Leap := Leap_Second_Dates (Index);

         --  Calculate the number of days from the start of Ada time until
         --  the current leap second occurence. Non-leap centenial years
         --  are not accounted for in these calculations since there are
         --  no leap seconds after 2100 yet.

         Years := Leap.Year - Ada_Min_Year;
         Days  := (Years / 4) * Days_In_Four_Years;
         Years := Years mod 4;

         if Years = 1 then
            Days := Days + 365;

         elsif Years = 2 then
            Days := Days + 365 * 2;

         elsif Years = 3 then
            Days := Days + 365 * 3;
         end if;

         Days := Days + Cumulative_Days_Before_Month (Leap.Month);

         if Is_Leap (Leap.Year)
           and then Leap.Month > 2
         then
            Days := Days + 1;
         end if;

         Days := Days + Leap.Day;

         --  Index - 1 previous leap seconds are added to Time (Index) as
         --  well as the lower buffer for time zones.

         Leap_Second_Times (Index) := Ada_Low +
           (Time (Days) * Secs_In_Day + Time (Index - 1)) * Nano;
      end loop;
   end;

end Ada.Calendar;
