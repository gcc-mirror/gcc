------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

--  This is the Alpha/VMS version

with Ada.Unchecked_Conversion;

with System.Aux_DEC;       use System.Aux_DEC;
with System.OS_Primitives; use System.OS_Primitives;

package body Ada.Calendar is

   --------------------------
   -- Implementation Notes --
   --------------------------

   --  Variables of type Ada.Calendar.Time have suffix _S or _M to denote
   --  units of seconds or milis.

   --  Because time is measured in different units and from different origins
   --  on various targets, a system independent model is incorporated into
   --  Ada.Calendar. The idea behind the design is to encapsulate all target
   --  dependent machinery in a single package, thus providing a uniform
   --  interface to all existing and any potential children.

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

   procedure Check_Within_Time_Bounds (T : OS_Time);
   --  Ensure that a time representation value falls withing the bounds of Ada
   --  time. Leap seconds support is taken into account.

   procedure Cumulative_Leap_Seconds
     (Start_Date    : OS_Time;
      End_Date      : OS_Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out OS_Time);
   --  Elapsed_Leaps is the sum of the leap seconds that have occurred on or
   --  after Start_Date and before (strictly before) End_Date. Next_Leap_Sec
   --  represents the next leap second occurrence on or after End_Date. If
   --  there are no leaps seconds after End_Date, End_Of_Time is returned.
   --  End_Of_Time can be used as End_Date to count all the leap seconds that
   --  have occurred on or after Start_Date.
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

   function To_Duration (T : Time) return Duration;
   function To_Relative_Time (D : Duration) return Time;
   --  It is important to note that duration's fractional part denotes nano
   --  seconds while the units of Time are 100 nanoseconds. If a regular
   --  Unchecked_Conversion was employed, the resulting values would be off
   --  by 100.

   --------------------------
   -- Leap seconds control --
   --------------------------

   Flag : Integer;
   pragma Import (C, Flag, "__gl_leap_seconds_support");
   --  This imported value is used to determine whether the compilation had
   --  binder flag "-y" present which enables leap seconds. A value of zero
   --  signifies no leap seconds support while a value of one enables the
   --  support.

   Leap_Support : constant Boolean := Flag = 1;
   --  The above flag controls the usage of leap seconds in all Ada.Calendar
   --  routines.

   Leap_Seconds_Count : constant Natural := 25;

   ---------------------
   -- Local Constants --
   ---------------------

   --  The range of Ada time expressed as milis since the VMS Epoch

   Ada_Low  : constant OS_Time :=  (10 * 366 +  32 * 365 + 45) * Milis_In_Day;
   Ada_High : constant OS_Time := (131 * 366 + 410 * 365 + 45) * Milis_In_Day;

   --  Even though the upper bound of time is 2399-12-31 23:59:59.9999999
   --  UTC, it must be increased to include all leap seconds.

   Ada_High_And_Leaps : constant OS_Time :=
                          Ada_High + OS_Time (Leap_Seconds_Count) * Mili;

   --  Two constants used in the calculations of elapsed leap seconds.
   --  End_Of_Time is later than Ada_High in time zone -28. Start_Of_Time
   --  is earlier than Ada_Low in time zone +28.

   End_Of_Time   : constant OS_Time := Ada_High + OS_Time (3) * Milis_In_Day;
   Start_Of_Time : constant OS_Time := Ada_Low  - OS_Time (3) * Milis_In_Day;

   --  The following table contains the hard time values of all existing leap
   --  seconds. The values are produced by the utility program xleaps.adb.

   Leap_Second_Times : constant array (1 .. Leap_Seconds_Count) of OS_Time :=
     (35855136000000000,
      36014112010000000,
      36329472020000000,
      36644832030000000,
      36960192040000000,
      37276416050000000,
      37591776060000000,
      37907136070000000,
      38222496080000000,
      38695104090000000,
      39010464100000000,
      39325824110000000,
      39957408120000000,
      40747104130000000,
      41378688140000000,
      41694048150000000,
      42166656160000000,
      42482016170000000,
      42797376180000000,
      43271712190000000,
      43744320200000000,
      44218656210000000,
      46427904220000000,
      47374848230000000,
      48478176240000000);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Left + To_Relative_Time (Right);
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Right + Left;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Left - To_Relative_Time (Right);
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);

      --  The bound of type Duration expressed as time

      Dur_High : constant OS_Time :=
                   OS_Time (To_Relative_Time (Duration'Last));
      Dur_Low  : constant OS_Time :=
                   OS_Time (To_Relative_Time (Duration'First));

      Res_M : OS_Time;

   begin
      Res_M := OS_Time (Left) - OS_Time (Right);

      --  Due to the extended range of Ada time, "-" is capable of producing
      --  results which may exceed the range of Duration. In order to prevent
      --  the generation of bogus values by the Unchecked_Conversion, we apply
      --  the following check.

      if Res_M < Dur_Low
        or else Res_M >= Dur_High
      then
         raise Time_Error;

      --  Normal case, result fits

      else
         return To_Duration (Time (Res_M));
      end if;

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return OS_Time (Left) < OS_Time (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return OS_Time (Left) <= OS_Time (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return OS_Time (Left) > OS_Time (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return OS_Time (Left) >= OS_Time (Right);
   end ">=";

   ------------------------------
   -- Check_Within_Time_Bounds --
   ------------------------------

   procedure Check_Within_Time_Bounds (T : OS_Time) is
   begin
      if Leap_Support then
         if T < Ada_Low or else T > Ada_High_And_Leaps then
            raise Time_Error;
         end if;
      else
         if T < Ada_Low or else T > Ada_High then
            raise Time_Error;
         end if;
      end if;
   end Check_Within_Time_Bounds;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Elapsed_Leaps : Natural;
      Next_Leap_M   : OS_Time;
      Res_M         : constant OS_Time := OS_Clock;

   begin
      --  Note that on other targets a soft-link is used to get a different
      --  clock depending whether tasking is used or not. On VMS this isn't
      --  needed since all clock calls end up using SYS$GETTIM, so call the
      --  OS_Primitives version for efficiency.

      --  If the target supports leap seconds, determine the number of leap
      --  seconds elapsed until this moment.

      if Leap_Support then
         Cumulative_Leap_Seconds
           (Start_Of_Time, Res_M, Elapsed_Leaps, Next_Leap_M);

         --  The system clock may fall exactly on a leap second

         if Res_M >= Next_Leap_M then
            Elapsed_Leaps := Elapsed_Leaps + 1;
         end if;

      --  The target does not support leap seconds

      else
         Elapsed_Leaps := 0;
      end if;

      return Time (Res_M + OS_Time (Elapsed_Leaps) * Mili);
   end Clock;

   -----------------------------
   -- Cumulative_Leap_Seconds --
   -----------------------------

   procedure Cumulative_Leap_Seconds
     (Start_Date    : OS_Time;
      End_Date      : OS_Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out OS_Time)
   is
      End_Index   : Positive;
      End_T       : OS_Time := End_Date;
      Start_Index : Positive;
      Start_T     : OS_Time := Start_Date;

   begin
      pragma Assert (Leap_Support and then End_Date >= Start_Date);

      Next_Leap_Sec := End_Of_Time;

      --  Make sure that the end date does not exceed the upper bound
      --  of Ada time.

      if End_Date > Ada_High then
         End_T := Ada_High;
      end if;

      --  Remove the sub seconds from both dates

      Start_T := Start_T - (Start_T mod Mili);
      End_T   := End_T   - (End_T   mod Mili);

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
         Next_Leap_Sec := End_Of_Time;
         return;
      end if;

      --  Perform the calculations only if the start date is within the leap
      --  second occurrences table.

      if Start_T <= Leap_Second_Times (Leap_Seconds_Count) then

         --    1    2                  N - 1   N
         --  +----+----+--  . . .  --+-------+---+
         --  | T1 | T2 |             | N - 1 | N |
         --  +----+----+--  . . .  --+-------+---+
         --         ^                   ^
         --         | Start_Index       | End_Index
         --         +-------------------+
         --             Leaps_Between

         --  The idea behind the algorithm is to iterate and find two closest
         --  dates which are after Start_T and End_T. Their corresponding
         --  index difference denotes the number of leap seconds elapsed.

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
      pragma Unreferenced (Y, M, S);
   begin
      Split (Date, Y, M, D, S);
      return D;
   end Day;

   -------------
   -- Is_Leap --
   -------------

   function Is_Leap (Year : Year_Number) return Boolean is
   begin
      --  Leap centennial years

      if Year mod 400 = 0 then
         return True;

      --  Non-leap centennial years

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
      pragma Unreferenced (Y, D, S);
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
      pragma Unreferenced (Y, M, D);
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

   begin
      --  Use UTC as the local time zone on VMS, the status of flag Is_Ada_05
      --  is irrelevant in this case.

      Formatting_Operations.Split
        (Date      => Date,
         Year      => Year,
         Month     => Month,
         Day       => Day,
         Day_Secs  => Seconds,
         Hour      => H,
         Minute    => M,
         Second    => Se,
         Sub_Sec   => Ss,
         Leap_Sec  => Le,
         Is_Ada_05 => False,
         Time_Zone => 0);

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

   begin
      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Seconds'Valid
      then
         raise Time_Error;
      end if;

      --  Use UTC as the local time zone on VMS, the status of flag Is_Ada_05
      --  is irrelevant in this case.

      return
        Formatting_Operations.Time_Of
          (Year         => Year,
           Month        => Month,
           Day          => Day,
           Day_Secs     => Seconds,
           Hour         => H,
           Minute       => M,
           Second       => Se,
           Sub_Sec      => Ss,
           Leap_Sec     => False,
           Use_Day_Secs => True,
           Is_Ada_05    => False,
           Time_Zone    => 0);
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
      function Time_To_Duration is
        new Ada.Unchecked_Conversion (Time, Duration);
   begin
      return Time_To_Duration (T * 100);
   end To_Duration;

   ----------------------
   -- To_Relative_Time --
   ----------------------

   function To_Relative_Time (D : Duration) return Time is
      function Duration_To_Time is
        new Ada.Unchecked_Conversion (Duration, Time);
   begin
      return Duration_To_Time (D / 100.0);
   end To_Relative_Time;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
      pragma Unreferenced (M, D, S);
   begin
      Split (Date, Y, M, D, S);
      return Y;
   end Year;

   --  The following packages assume that Time is a Long_Integer, the units
   --  are 100 nanoseconds and the starting point in the VMS Epoch.

   ---------------------------
   -- Arithmetic_Operations --
   ---------------------------

   package body Arithmetic_Operations is

      ---------
      -- Add --
      ---------

      function Add (Date : Time; Days : Long_Integer) return Time is
         pragma Unsuppress (Overflow_Check);
         Date_M : constant OS_Time := OS_Time (Date);
      begin
         return Time (Date_M + OS_Time (Days) * Milis_In_Day);
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
         Diff_M        : OS_Time;
         Diff_S        : OS_Time;
         Earlier       : OS_Time;
         Elapsed_Leaps : Natural;
         Later         : OS_Time;
         Negate        : Boolean := False;
         Next_Leap     : OS_Time;
         Sub_Seconds   : Duration;

      begin
         --  This classification is necessary in order to avoid a Time_Error
         --  being raised by the arithmetic operators in Ada.Calendar.

         if Left >= Right then
            Later   := OS_Time (Left);
            Earlier := OS_Time (Right);
         else
            Later   := OS_Time (Right);
            Earlier := OS_Time (Left);
            Negate  := True;
         end if;

         --  If the target supports leap seconds, process them

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Earlier, Later, Elapsed_Leaps, Next_Leap);

            if Later >= Next_Leap then
               Elapsed_Leaps := Elapsed_Leaps + 1;
            end if;

         --  The target does not support leap seconds

         else
            Elapsed_Leaps := 0;
         end if;

         Diff_M := Later - Earlier - OS_Time (Elapsed_Leaps) * Mili;

         --  Sub second processing

         Sub_Seconds := Duration (Diff_M mod Mili) / Mili_F;

         --  Convert to seconds. Note that his action eliminates the sub
         --  seconds automatically.

         Diff_S := Diff_M / Mili;

         Days := Long_Integer (Diff_S / Secs_In_Day);
         Seconds := Duration (Diff_S mod Secs_In_Day) + Sub_Seconds;
         Leap_Seconds := Integer (Elapsed_Leaps);

         if Negate then
            Days    := -Days;
            Seconds := -Seconds;

            if Leap_Seconds /= 0 then
               Leap_Seconds := -Leap_Seconds;
            end if;
         end if;
      end Difference;

      --------------
      -- Subtract --
      --------------

      function Subtract (Date : Time; Days : Long_Integer) return Time is
         pragma Unsuppress (Overflow_Check);
         Date_M : constant OS_Time := OS_Time (Date);
      begin
         return Time (Date_M - OS_Time (Days) * Milis_In_Day);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end Subtract;
   end Arithmetic_Operations;

   ---------------------------
   -- Conversion_Operations --
   ---------------------------

   package body Conversion_Operations is

      Epoch_Offset : constant OS_Time := 35067168000000000;
      --  The difference between 1970-1-1 UTC and 1858-11-17 UTC expressed in
      --  100 nanoseconds.

      -----------------
      -- To_Ada_Time --
      -----------------

      function To_Ada_Time (Unix_Time : Long_Integer) return Time is
         pragma Unsuppress (Overflow_Check);
         Unix_Rep : constant OS_Time := OS_Time (Unix_Time) * Mili;
      begin
         return Time (Unix_Rep + Epoch_Offset);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end To_Ada_Time;

      -----------------
      -- To_Ada_Time --
      -----------------

      function To_Ada_Time
        (tm_year  : Integer;
         tm_mon   : Integer;
         tm_day   : Integer;
         tm_hour  : Integer;
         tm_min   : Integer;
         tm_sec   : Integer;
         tm_isdst : Integer) return Time
      is
         pragma Unsuppress (Overflow_Check);

         Year_Shift  : constant Integer := 1900;
         Month_Shift : constant Integer := 1;

         Year   : Year_Number;
         Month  : Month_Number;
         Day    : Day_Number;
         Second : Integer;
         Leap   : Boolean;
         Result : OS_Time;

      begin
         --  Input processing

         Year  := Year_Number (Year_Shift + tm_year);
         Month := Month_Number (Month_Shift + tm_mon);
         Day   := Day_Number (tm_day);

         --  Step 1: Validity checks of input values

         if not Year'Valid
           or else not Month'Valid
           or else not Day'Valid
           or else tm_hour not in 0 .. 24
           or else tm_min not in 0 .. 59
           or else tm_sec not in 0 .. 60
           or else tm_isdst not in -1 .. 1
         then
            raise Time_Error;
         end if;

         --  Step 2: Potential leap second

         if tm_sec = 60 then
            Leap   := True;
            Second := 59;
         else
            Leap   := False;
            Second := tm_sec;
         end if;

         --  Step 3: Calculate the time value

         Result :=
           OS_Time
             (Formatting_Operations.Time_Of
               (Year         => Year,
                Month        => Month,
                Day          => Day,
                Day_Secs     => 0.0,      --  Time is given in h:m:s
                Hour         => tm_hour,
                Minute       => tm_min,
                Second       => Second,
                Sub_Sec      => 0.0,      --  No precise sub second given
                Leap_Sec     => Leap,
                Use_Day_Secs => False,    --  Time is given in h:m:s
                Is_Ada_05    => True,     --  Force usage of explicit time zone
                Time_Zone    => 0));      --  Place the value in UTC
         --  Step 4: Daylight Savings Time

         if tm_isdst = 1 then
            Result := Result + OS_Time (3_600) * Mili;
         end if;

         return Time (Result);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end To_Ada_Time;

      -----------------
      -- To_Duration --
      -----------------

      function To_Duration
        (tv_sec  : Long_Integer;
         tv_nsec : Long_Integer) return Duration
      is
         pragma Unsuppress (Overflow_Check);
      begin
         return Duration (tv_sec) + Duration (tv_nsec) / Mili_F;
      end To_Duration;

      ------------------------
      -- To_Struct_Timespec --
      ------------------------

      procedure To_Struct_Timespec
        (D       : Duration;
         tv_sec  : out Long_Integer;
         tv_nsec : out Long_Integer)
      is
         pragma Unsuppress (Overflow_Check);
         Secs      : Duration;
         Nano_Secs : Duration;

      begin
         --  Seconds extraction, avoid potential rounding errors

         Secs   := D - 0.5;
         tv_sec := Long_Integer (Secs);

         --  100 Nanoseconds extraction

         Nano_Secs := D - Duration (tv_sec);
         tv_nsec := Long_Integer (Nano_Secs * Mili);
      end To_Struct_Timespec;

      ------------------
      -- To_Struct_Tm --
      ------------------

      procedure To_Struct_Tm
        (T       : Time;
         tm_year : out Integer;
         tm_mon  : out Integer;
         tm_day  : out Integer;
         tm_hour : out Integer;
         tm_min  : out Integer;
         tm_sec  : out Integer)
      is
         pragma Unsuppress (Overflow_Check);
         Year      : Year_Number;
         Month     : Month_Number;
         Second    : Integer;
         Day_Secs  : Day_Duration;
         Sub_Sec   : Duration;
         Leap_Sec  : Boolean;

      begin
         --  Step 1: Split the input time

         Formatting_Operations.Split
           (T, Year, Month, tm_day, Day_Secs,
            tm_hour, tm_min, Second, Sub_Sec, Leap_Sec, True, 0);

         --  Step 2: Correct the year and month

         tm_year := Year - 1900;
         tm_mon  := Month - 1;

         --  Step 3: Handle leap second occurrences

         tm_sec := (if Leap_Sec then 60 else Second);
      end To_Struct_Tm;

      ------------------
      -- To_Unix_Time --
      ------------------

      function To_Unix_Time (Ada_Time : Time) return Long_Integer is
         pragma Unsuppress (Overflow_Check);
         Ada_OS_Time : constant OS_Time := OS_Time (Ada_Time);
      begin
         return Long_Integer ((Ada_OS_Time - Epoch_Offset) / Mili);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end To_Unix_Time;
   end Conversion_Operations;

   ---------------------------
   -- Formatting_Operations --
   ---------------------------

   package body Formatting_Operations is

      -----------------
      -- Day_Of_Week --
      -----------------

      function Day_Of_Week (Date : Time) return Integer is
         Y : Year_Number;
         M : Month_Number;
         D : Day_Number;
         S : Day_Duration;

         Day_Count     : Long_Integer;
         Midday_Date_S : Time;

      begin
         Split (Date, Y, M, D, S);

         --  Build a time value in the middle of the same day and convert the
         --  time value to seconds.

         Midday_Date_S := Time_Of (Y, M, D, 43_200.0) / Mili;

         --  Count the number of days since the start of VMS time. 1858-11-17
         --  was a Wednesday.

         Day_Count := Long_Integer (Midday_Date_S / Secs_In_Day) + 2;

         return Integer (Day_Count mod 7);
      end Day_Of_Week;

      -----------
      -- Split --
      -----------

      procedure Split
        (Date      : Time;
         Year      : out Year_Number;
         Month     : out Month_Number;
         Day       : out Day_Number;
         Day_Secs  : out Day_Duration;
         Hour      : out Integer;
         Minute    : out Integer;
         Second    : out Integer;
         Sub_Sec   : out Duration;
         Leap_Sec  : out Boolean;
         Is_Ada_05 : Boolean;
         Time_Zone : Long_Integer)
      is
         --  The flag Is_Ada_05 is present for interfacing purposes

         pragma Unreferenced (Is_Ada_05);

         procedure Numtim
           (Status : out Unsigned_Longword;
            Timbuf : out Unsigned_Word_Array;
            Timadr : Time);

         pragma Interface (External, Numtim);

         pragma Import_Valued_Procedure
           (Numtim, "SYS$NUMTIM",
           (Unsigned_Longword, Unsigned_Word_Array, Time),
           (Value, Reference, Reference));

         Status : Unsigned_Longword;
         Timbuf : Unsigned_Word_Array (1 .. 7);

         Ada_Min_Year : constant := 1901;
         Ada_Max_Year : constant := 2399;

         Date_M        : OS_Time;
         Elapsed_Leaps : Natural;
         Next_Leap_M   : OS_Time;

      begin
         Date_M := OS_Time (Date);

         --  Step 1: Leap seconds processing

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Start_Of_Time, Date_M, Elapsed_Leaps, Next_Leap_M);

            Leap_Sec := Date_M >= Next_Leap_M;

            if Leap_Sec then
               Elapsed_Leaps := Elapsed_Leaps + 1;
            end if;

         --  The target does not support leap seconds

         else
            Elapsed_Leaps := 0;
            Leap_Sec      := False;
         end if;

         Date_M := Date_M - OS_Time (Elapsed_Leaps) * Mili;

         --  Step 2: Time zone processing

         if Time_Zone /= 0 then
            Date_M := Date_M + OS_Time (Time_Zone) * 60 * Mili;
         end if;

         --  After the leap seconds and time zone have been accounted for,
         --  the date should be within the bounds of Ada time.

         if Date_M < Ada_Low
           or else Date_M > Ada_High
         then
            raise Time_Error;
         end if;

         --  Step 3: Sub second processing

         Sub_Sec := Duration (Date_M mod Mili) / Mili_F;

         --  Drop the sub seconds

         Date_M := Date_M - (Date_M mod Mili);

         --  Step 4: VMS system call

         Numtim (Status, Timbuf, Time (Date_M));

         if Status mod 2 /= 1
           or else Timbuf (1) not in Ada_Min_Year .. Ada_Max_Year
         then
            raise Time_Error;
         end if;

         --  Step 5: Time components processing

         Year   := Year_Number (Timbuf (1));
         Month  := Month_Number (Timbuf (2));
         Day    := Day_Number (Timbuf (3));
         Hour   := Integer (Timbuf (4));
         Minute := Integer (Timbuf (5));
         Second := Integer (Timbuf (6));

         Day_Secs := Day_Duration (Hour   * 3_600) +
                     Day_Duration (Minute *    60) +
                     Day_Duration (Second)         +
                                   Sub_Sec;
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
         Leap_Sec     : Boolean := False;
         Use_Day_Secs : Boolean := False;
         Is_Ada_05    : Boolean := False;
         Time_Zone    : Long_Integer := 0) return Time
      is
         procedure Cvt_Vectim
           (Status         : out Unsigned_Longword;
            Input_Time     : Unsigned_Word_Array;
            Resultant_Time : out Time);

         pragma Interface (External, Cvt_Vectim);

         pragma Import_Valued_Procedure
           (Cvt_Vectim, "LIB$CVT_VECTIM",
           (Unsigned_Longword, Unsigned_Word_Array, Time),
           (Value, Reference, Reference));

         Status : Unsigned_Longword;
         Timbuf : Unsigned_Word_Array (1 .. 7);

         Y  : Year_Number  := Year;
         Mo : Month_Number := Month;
         D  : Day_Number   := Day;
         H  : Integer      := Hour;
         Mi : Integer      := Minute;
         Se : Integer      := Second;
         Su : Duration     := Sub_Sec;

         Elapsed_Leaps : Natural;
         Int_Day_Secs  : Integer;
         Next_Leap_M   : OS_Time;
         Res           : Time;
         Res_M         : OS_Time;
         Rounded_Res_M : OS_Time;

      begin
         --  No validity checks are performed on the input values since it is
         --  assumed that the called has already performed them.

         --  Step 1: Hour, minute, second and sub second processing

         if Use_Day_Secs then

            --  A day seconds value of 86_400 designates a new day

            if Day_Secs = 86_400.0 then
               declare
                  Adj_Year  : Year_Number := Year;
                  Adj_Month : Month_Number := Month;
                  Adj_Day   : Day_Number   := Day;

               begin
                  if Day < Days_In_Month (Month)
                    or else (Month = 2
                               and then Is_Leap (Year))
                  then
                     Adj_Day := Day + 1;

                  --  The day adjustment moves the date to a new month

                  else
                     Adj_Day := 1;

                     if Month < 12 then
                        Adj_Month := Month + 1;

                     --  The month adjustment moves the date to a new year

                     else
                        Adj_Month := 1;
                        Adj_Year  := Year + 1;
                     end if;
                  end if;

                  Y  := Adj_Year;
                  Mo := Adj_Month;
                  D  := Adj_Day;
                  H  := 0;
                  Mi := 0;
                  Se := 0;
                  Su := 0.0;
               end;

            --  Normal case (not exactly one day)

            else
               --  Sub second extraction

               Int_Day_Secs :=
                 (if Day_Secs > 0.0
                  then Integer (Day_Secs - 0.5)
                  else Integer (Day_Secs));

               H  := Int_Day_Secs / 3_600;
               Mi := (Int_Day_Secs / 60) mod 60;
               Se := Int_Day_Secs mod 60;
               Su := Day_Secs - Duration (Int_Day_Secs);
            end if;
         end if;

         --  Step 2: System call to VMS

         Timbuf (1) := Unsigned_Word (Y);
         Timbuf (2) := Unsigned_Word (Mo);
         Timbuf (3) := Unsigned_Word (D);
         Timbuf (4) := Unsigned_Word (H);
         Timbuf (5) := Unsigned_Word (Mi);
         Timbuf (6) := Unsigned_Word (Se);
         Timbuf (7) := 0;

         Cvt_Vectim (Status, Timbuf, Res);

         if Status mod 2 /= 1 then
            raise Time_Error;
         end if;

         --  Step 3: Sub second adjustment

         Res_M := OS_Time (Res) + OS_Time (Su * Mili_F);

         --  Step 4: Bounds check

         Check_Within_Time_Bounds (Res_M);

         --  Step 5: Time zone processing

         if Time_Zone /= 0 then
            Res_M := Res_M - OS_Time (Time_Zone) * 60 * Mili;
         end if;

         --  Step 6: Leap seconds processing

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Start_Of_Time, Res_M, Elapsed_Leaps, Next_Leap_M);

            Res_M := Res_M + OS_Time (Elapsed_Leaps) * Mili;

            --  An Ada 2005 caller requesting an explicit leap second or an
            --  Ada 95 caller accounting for an invisible leap second.

            if Leap_Sec
              or else Res_M >= Next_Leap_M
            then
               Res_M := Res_M + OS_Time (1) * Mili;
            end if;

            --  Leap second validity check

            Rounded_Res_M := Res_M - (Res_M mod Mili);

            if Is_Ada_05
              and then Leap_Sec
              and then Rounded_Res_M /= Next_Leap_M
            then
               raise Time_Error;
            end if;
         end if;

         return Time (Res_M);
      end Time_Of;
   end Formatting_Operations;

   ---------------------------
   -- Time_Zones_Operations --
   ---------------------------

   package body Time_Zones_Operations is

      ---------------------
      -- UTC_Time_Offset --
      ---------------------

      function UTC_Time_Offset (Date : Time) return Long_Integer is
         --  Formal parameter Date is here for interfacing, but is never
         --  actually used.

         pragma Unreferenced (Date);

         function get_gmtoff return Long_Integer;
         pragma Import (C, get_gmtoff, "get_gmtoff");

      begin
         --  VMS is not capable of determining the time zone in some past or
         --  future point in time denoted by Date, thus the current time zone
         --  is retrieved.

         return get_gmtoff;
      end UTC_Time_Offset;
   end Time_Zones_Operations;
end Ada.Calendar;
