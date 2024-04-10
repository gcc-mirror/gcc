------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.OS_Primitives;

with System.OS_Lib;

package body Ada.Calendar with
  SPARK_Mode => Off
is
   --------------------------
   -- Implementation Notes --
   --------------------------

   --  In complex algorithms, some variables of type Ada.Calendar.Time carry
   --  suffix _S or _N to denote units of seconds or nanoseconds.
   --
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

   --  The behavior of the interfacing routines is controlled via various
   --  flags. All new Ada 2005 types from children of Ada.Calendar are
   --  emulated by a similar type. For instance, type Day_Number is replaced
   --  by Integer in various routines. One ramification of this model is that
   --  the caller site must perform validity checks on returned results.
   --  The end result of this model is the lack of target specific files per
   --  child of Ada.Calendar (e.g. a-calfor).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Within_Time_Bounds (T : Time_Rep);
   --  Ensure that a time representation value falls withing the bounds of Ada
   --  time. Leap seconds support is taken into account.

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time_Rep;
      End_Date      : Time_Rep;
      Elapsed_Leaps : out Natural;
      Next_Leap     : out Time_Rep);
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

   function Duration_To_Time_Rep is
     new Ada.Unchecked_Conversion (Duration, Time_Rep);
   --  Convert a duration value into a time representation value

   function Time_Rep_To_Duration is
     new Ada.Unchecked_Conversion (Time_Rep, Duration);
   --  Convert a time representation value into a duration value

   function UTC_Time_Offset
     (Date        : Time;
      Is_Historic : Boolean) return Long_Integer;
   --  This routine acts as an Ada wrapper around __gnat_localtime_tzoff which
   --  in turn utilizes various OS-dependent mechanisms to calculate the time
   --  zone offset of a date. Formal parameter Date represents an arbitrary
   --  time stamp, either in the past, now, or in the future. If the flag
   --  Is_Historic is set, this routine would try to calculate to the best of
   --  the OS's abilities the time zone offset that was or will be in effect
   --  on Date. If the flag is set to False, the routine returns the current
   --  time zone with Date effectively set to Clock.
   --
   --  NOTE: Targets which support localtime_r will aways return a historic
   --  time zone even if flag Is_Historic is set to False because this is how
   --  localtime_r operates.

   -----------------
   -- Local Types --
   -----------------

   --  An integer time duration. The type is used whenever a positive elapsed
   --  duration is needed, for instance when splitting a time value. Here is
   --  how Time_Rep and Time_Dur are related:

   --            'First  Ada_Low                  Ada_High  'Last
   --  Time_Rep: +-------+------------------------+---------+
   --  Time_Dur:         +------------------------+---------+
   --                    0                                  'Last

   type Time_Dur is range 0 .. 2 ** 63 - 1;

   --------------------------
   -- Leap seconds control --
   --------------------------

   Flag : constant Integer;
   pragma Import (C, Flag, "__gl_leap_seconds_support");
   --  This imported value is used to determine whether the compilation had
   --  binder flag "-y" present which enables leap seconds. A value of zero
   --  signifies no leap seconds support while a value of one enables support.

   Leap_Support : constant Boolean := (Flag = 1);
   --  Flag to controls the usage of leap seconds in all Ada.Calendar routines

   Leap_Seconds_Count : constant Natural := 27;

   ---------------------
   -- Local Constants --
   ---------------------

   Ada_Min_Year          : constant Year_Number := Year_Number'First;
   Secs_In_Four_Years    : constant := (3 * 365 + 366) * Secs_In_Day;
   Secs_In_Non_Leap_Year : constant := 365 * Secs_In_Day;
   Nanos_In_Four_Years   : constant := Secs_In_Four_Years * Nano;

   --  Lower and upper bound of Ada time. Note that the lower and upper bound
   --  account for the non-leap centennial years. See "Implementation of Time"
   --  in the spec for what the zero value represents.

   Ada_Low  : constant Time_Rep := -(61 * 366 + 188 * 365) * Nanos_In_Day;
   Ada_High : constant Time_Rep :=  (60 * 366 + 190 * 365) * Nanos_In_Day;

   --  Even though the upper bound of time is 2399-12-31 23:59:59.999999999
   --  UTC, it must be increased to include all leap seconds.

   Ada_High_And_Leaps : constant Time_Rep :=
     Ada_High + Time_Rep (Leap_Seconds_Count) * Nano;

   --  Two constants used in the calculations of elapsed leap seconds.
   --  End_Of_Time is later than Ada_High in time zone -28. Start_Of_Time
   --  is earlier than Ada_Low in time zone +28.

   End_Of_Time   : constant Time_Rep :=
     Ada_High + Time_Rep (3) * Nanos_In_Day;
   Start_Of_Time : constant Time_Rep :=
     Ada_Low - Time_Rep (3) * Nanos_In_Day;

   --  The Unix lower time bound expressed as nanoseconds since the start of
   --  Ada time in UTC.

   Unix_Min : constant Time_Rep :=
     Ada_Low + Time_Rep (17 * 366 + 52 * 365) * Nanos_In_Day;

   --  The Unix upper time bound expressed as nanoseconds since the start of
   --  Ada time in UTC.

   Unix_Max : constant Time_Rep :=
     Ada_Low + Time_Rep (34 * 366 + 102 * 365) * Nanos_In_Day +
     Time_Rep (Leap_Seconds_Count) * Nano;

   Cumulative_Days_Before_Month :
     constant array (Month_Number) of Natural :=
       [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];

   --  The following table contains the hard time values of all existing leap
   --  seconds. The values are produced by the utility program xleaps.adb. This
   --  must be updated when additional leap second times are defined.

   Leap_Second_Times : constant array (1 .. Leap_Seconds_Count) of Time_Rep :=
     [-5601484800000000000,
      -5585587199000000000,
      -5554051198000000000,
      -5522515197000000000,
      -5490979196000000000,
      -5459356795000000000,
      -5427820794000000000,
      -5396284793000000000,
      -5364748792000000000,
      -5317487991000000000,
      -5285951990000000000,
      -5254415989000000000,
      -5191257588000000000,
      -5112287987000000000,
      -5049129586000000000,
      -5017593585000000000,
      -4970332784000000000,
      -4938796783000000000,
      -4907260782000000000,
      -4859827181000000000,
      -4812566380000000000,
      -4765132779000000000,
      -4544207978000000000,
      -4449513577000000000,
      -4339180776000000000,
      -4244572775000000000,
      -4197052774000000000];

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
      Left_N : constant Time_Rep := Time_Rep (Left);
   begin
      return Time (Left_N + Duration_To_Time_Rep (Right));
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
      Left_N : constant Time_Rep := Time_Rep (Left);
   begin
      return Time (Left_N - Duration_To_Time_Rep (Right));
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);

      Dur_Low  : constant Time_Rep := Duration_To_Time_Rep (Duration'First);
      Dur_High : constant Time_Rep := Duration_To_Time_Rep (Duration'Last);
      --  The bounds of type Duration expressed as time representations

      Res_N : Time_Rep;

   begin
      Res_N := Time_Rep (Left) - Time_Rep (Right);

      --  Due to the extended range of Ada time, "-" is capable of producing
      --  results which may exceed the range of Duration. In order to prevent
      --  the generation of bogus values by the Unchecked_Conversion, we apply
      --  the following check.

      if Res_N < Dur_Low or else Res_N > Dur_High then
         raise Time_Error;
      end if;

      return Time_Rep_To_Duration (Res_N);

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

   ------------------------------
   -- Check_Within_Time_Bounds --
   ------------------------------

   procedure Check_Within_Time_Bounds (T : Time_Rep) is
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
      Next_Leap_N   : Time_Rep;

      --  The system clock returns the time in UTC since the Unix Epoch of
      --  1970-01-01 00:00:00.0. We perform an origin shift to the Ada Epoch
      --  by adding the number of nanoseconds between the two origins.

      Res_N : Time_Rep :=
        Duration_To_Time_Rep (System.OS_Primitives.Clock) + Unix_Min;

   begin
      --  If the target supports leap seconds, determine the number of leap
      --  seconds elapsed until this moment.

      if Leap_Support then
         Cumulative_Leap_Seconds
           (Start_Of_Time, Res_N, Elapsed_Leaps, Next_Leap_N);

         --  The system clock may fall exactly on a leap second

         if Res_N >= Next_Leap_N then
            Elapsed_Leaps := Elapsed_Leaps + 1;
         end if;

      --  The target does not support leap seconds

      else
         Elapsed_Leaps := 0;
      end if;

      Res_N := Res_N + Time_Rep (Elapsed_Leaps) * Nano;

      return Time (Res_N);
   end Clock;

   -----------------------------
   -- Cumulative_Leap_Seconds --
   -----------------------------

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time_Rep;
      End_Date      : Time_Rep;
      Elapsed_Leaps : out Natural;
      Next_Leap     : out Time_Rep)
   is
      End_Index   : Positive;
      End_T       : Time_Rep := End_Date;
      Start_Index : Positive;
      Start_T     : Time_Rep := Start_Date;

   begin
      --  Both input dates must be normalized to UTC

      pragma Assert (Leap_Support and then End_Date >= Start_Date);

      Next_Leap := End_Of_Time;

      --  Make sure that the end date does not exceed the upper bound
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
         Next_Leap     := Leap_Second_Times (1);

      elsif Start_T > Leap_Second_Times (Leap_Seconds_Count) then
         Elapsed_Leaps := 0;
         Next_Leap     := End_Of_Time;

      else
         --  Perform the calculations only if the start date is within the leap
         --  second occurrences table.

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
            Next_Leap := Leap_Second_Times (End_Index);
         end if;

         Elapsed_Leaps := End_Index - Start_Index;
      end if;
   end Cumulative_Leap_Seconds;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      D : Day_Number;
      Y : Year_Number;
      M : Month_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return D;
   end Day;

   ------------------
   -- Epoch_Offset --
   ------------------

   function Epoch_Offset return Time_Rep is
   begin
      return (136 * 365 + 44 * 366) * Nanos_In_Day;
   end Epoch_Offset;

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

   begin
      --  Even though the input time zone is UTC (0), the flag Use_TZ will
      --  ensure that Split picks up the local time zone.

      Formatting_Operations.Split
        (Date        => Date,
         Year        => Year,
         Month       => Month,
         Day         => Day,
         Day_Secs    => Seconds,
         Hour        => H,
         Minute      => M,
         Second      => Se,
         Sub_Sec     => Ss,
         Leap_Sec    => Le,
         Use_TZ      => False,
         Is_Historic => True,
         Time_Zone   => 0);

      --  Validity checks

      if not Year'Valid    or else
         not Month'Valid   or else
         not Day'Valid     or else
         not Seconds'Valid
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
      --  Validity checks

      if not Year'Valid    or else
         not Month'Valid   or else
         not Day'Valid     or else
         not Seconds'Valid
      then
         raise Time_Error;
      end if;

      --  Even though the input time zone is UTC (0), the flag Use_TZ will
      --  ensure that Split picks up the local time zone.

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
           Use_TZ       => False,
           Is_Historic  => True,
           Time_Zone    => 0);
   end Time_Of;

   ---------------------
   -- UTC_Time_Offset --
   ---------------------

   function UTC_Time_Offset
     (Date        : Time;
      Is_Historic : Boolean) return Long_Integer
   is
      --  The following constants denote February 28 during non-leap centennial
      --  years, the units are nanoseconds.

      T_2100_2_28 : constant Time_Rep := Ada_Low +
                      (Time_Rep (49 * 366 + 150 * 365 + 59) * Secs_In_Day +
                       Time_Rep (Leap_Seconds_Count)) * Nano;

      T_2200_2_28 : constant Time_Rep := Ada_Low +
                      (Time_Rep (73 * 366 + 226 * 365 + 59) * Secs_In_Day +
                       Time_Rep (Leap_Seconds_Count)) * Nano;

      T_2300_2_28 : constant Time_Rep := Ada_Low +
                      (Time_Rep (97 * 366 + 302 * 365 + 59) * Secs_In_Day +
                       Time_Rep (Leap_Seconds_Count)) * Nano;

      --  56 years (14 leap years + 42 non-leap years) in nanoseconds:

      Nanos_In_56_Years : constant := (14 * 366 + 42 * 365) * Nanos_In_Day;

      type int_Pointer  is access all Interfaces.C.int;
      type long_Pointer is access all Interfaces.C.long;

      type OS_Time_Pointer is access all System.OS_Lib.OS_Time;

      procedure localtime_tzoff
        (timer       : OS_Time_Pointer;
         is_historic : int_Pointer;
         off         : long_Pointer);
      pragma Import (C, localtime_tzoff, "__gnat_localtime_tzoff");
      --  This routine is a interfacing wrapper around the library function
      --  __gnat_localtime_tzoff. Parameter 'timer' represents a Unix-based
      --  time equivalent of the input date. If flag 'is_historic' is set, this
      --  routine would try to calculate to the best of the OS's abilities the
      --  time zone offset that was or will be in effect on 'timer'. If the
      --  flag is set to False, the routine returns the current time zone
      --  regardless of what 'timer' designates. Parameter 'off' captures the
      --  UTC offset of 'timer'.

      Adj_Cent : Integer;
      Date_N   : Time_Rep;
      Flag     : aliased Interfaces.C.int;
      Offset   : aliased Interfaces.C.long;
      Secs_T   : aliased System.OS_Lib.OS_Time;

   --  Start of processing for UTC_Time_Offset

   begin
      Date_N := Time_Rep (Date);

      --  Dates which are 56 years apart fall on the same day, day light saving
      --  and so on. Non-leap centennial years violate this rule by one day and
      --  as a consequence, special adjustment is needed.

      Adj_Cent :=
        (if    Date_N <= T_2100_2_28 then 0
         elsif Date_N <= T_2200_2_28 then 1
         elsif Date_N <= T_2300_2_28 then 2
         else                             3);

      if Adj_Cent > 0 then
         Date_N := Date_N - Time_Rep (Adj_Cent) * Nanos_In_Day;
      end if;

      --  Shift the date within bounds of Unix time

      while Date_N < Unix_Min loop
         Date_N := Date_N + Nanos_In_56_Years;
      end loop;

      while Date_N >= Unix_Max loop
         Date_N := Date_N - Nanos_In_56_Years;
      end loop;

      --  Perform a shift in origins from Ada to Unix

      Date_N := Date_N - Unix_Min;

      --  Convert the date into seconds

      Secs_T := System.OS_Lib.To_Ada (Long_Long_Integer (Date_N / Nano));

      --  Determine whether to treat the input date as historical or not. A
      --  value of "0" signifies that the date is NOT historic.

      Flag := (if Is_Historic then 1 else 0);

      localtime_tzoff
        (Secs_T'Unchecked_Access,
         Flag'Unchecked_Access,
         Offset'Unchecked_Access);
      pragma Annotate (CodePeer, Modified, Offset);

      return Long_Integer (Offset);
   end UTC_Time_Offset;

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

   --  The following packages assume that Time is a signed 64 bit integer
   --  type, the units are nanoseconds and the origin is the start of Ada
   --  time (1901-01-01 00:00:00.0 UTC).

   ---------------------------
   -- Arithmetic_Operations --
   ---------------------------

   package body Arithmetic_Operations is

      ---------
      -- Add --
      ---------

      function Add (Date : Time; Days : Long_Integer) return Time is
         pragma Unsuppress (Overflow_Check);
         Date_N : constant Time_Rep := Time_Rep (Date);
      begin
         return Time (Date_N + Time_Rep (Days) * Nanos_In_Day);
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
         Res_Dur       : Time_Dur;
         Earlier       : Time_Rep;
         Elapsed_Leaps : Natural;
         Later         : Time_Rep;
         Negate        : Boolean := False;
         Next_Leap_N   : Time_Rep;
         Sub_Secs      : Duration;
         Sub_Secs_Diff : Time_Rep;

      begin
         --  Both input time values are assumed to be in UTC

         if Left >= Right then
            Later   := Time_Rep (Left);
            Earlier := Time_Rep (Right);
         else
            Later   := Time_Rep (Right);
            Earlier := Time_Rep (Left);
            Negate  := True;
         end if;

         --  If the target supports leap seconds, process them

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Earlier, Later, Elapsed_Leaps, Next_Leap_N);

            if Later >= Next_Leap_N then
               Elapsed_Leaps := Elapsed_Leaps + 1;
            end if;

         --  The target does not support leap seconds

         else
            Elapsed_Leaps := 0;
         end if;

         --  Sub seconds processing. We add the resulting difference to one
         --  of the input dates in order to account for any potential rounding
         --  of the difference in the next step.

         Sub_Secs_Diff := Later mod Nano - Earlier mod Nano;
         Earlier       := Earlier + Sub_Secs_Diff;
         Sub_Secs      := Duration (Sub_Secs_Diff) / Nano_F;

         --  Difference processing. This operation should be able to calculate
         --  the difference between opposite values which are close to the end
         --  and start of Ada time. To accommodate the large range, we convert
         --  to seconds. This action may potentially round the two values and
         --  either add or drop a second. We compensate for this issue in the
         --  previous step.

         Res_Dur :=
           Time_Dur (Later / Nano - Earlier / Nano) - Time_Dur (Elapsed_Leaps);

         Days         := Long_Integer (Res_Dur / Secs_In_Day);
         Seconds      := Duration (Res_Dur mod Secs_In_Day) + Sub_Secs;
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
         Date_N : constant Time_Rep := Time_Rep (Date);
      begin
         return Time (Date_N - Time_Rep (Days) * Nanos_In_Day);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end Subtract;

   end Arithmetic_Operations;

   ---------------------------
   -- Conversion_Operations --
   ---------------------------

   package body Conversion_Operations is

      -----------------
      -- To_Ada_Time --
      -----------------

      function To_Ada_Time (Unix_Time : Long_Integer) return Time is
         pragma Unsuppress (Overflow_Check);
         Unix_Rep : constant Time_Rep := Time_Rep (Unix_Time) * Nano;
      begin
         return Time (Unix_Rep - Epoch_Offset);
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
         Year   : Year_Number;
         Month  : Month_Number;
         Day    : Day_Number;
         Second : Integer;
         Leap   : Boolean;
         Result : Time_Rep;

      begin
         --  Input processing

         Year  := Year_Number (1900 + tm_year);
         Month := Month_Number (1 + tm_mon);
         Day   := Day_Number (tm_day);

         --  Step 1: Validity checks of input values

         if not Year'Valid or else not Month'Valid or else not Day'Valid
           or else tm_hour  not in 0 .. 24
           or else tm_min   not in 0 .. 59
           or else tm_sec   not in 0 .. 60
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
           Time_Rep
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
                Use_TZ       => True,     --  Force usage of explicit time zone
                Is_Historic  => True,
                Time_Zone    => 0));      --  Place the value in UTC

         --  Step 4: Daylight Savings Time

         if tm_isdst = 1 then
            Result := Result + Time_Rep (3_600) * Nano;
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
         return Duration (tv_sec) + Duration (tv_nsec) / Nano_F;
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

         --  Nanoseconds extraction

         Nano_Secs := D - Duration (tv_sec);
         tv_nsec := Long_Integer (Nano_Secs * Nano);
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
           (Date        => T,
            Year        => Year,
            Month       => Month,
            Day         => tm_day,
            Day_Secs    => Day_Secs,
            Hour        => tm_hour,
            Minute      => tm_min,
            Second      => Second,
            Sub_Sec     => Sub_Sec,
            Leap_Sec    => Leap_Sec,
            Use_TZ      => True,
            Is_Historic => False,
            Time_Zone   => 0);

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
         Ada_Rep : constant Time_Rep := Time_Rep (Ada_Time);
      begin
         return Long_Integer ((Ada_Rep + Epoch_Offset) / Nano);
      exception
         when Constraint_Error =>
            raise Time_Error;
      end To_Unix_Time;
   end Conversion_Operations;

   ----------------------
   -- Delay_Operations --
   ----------------------

   package body Delay_Operations is

      -----------------
      -- To_Duration --
      -----------------

      function To_Duration (Date : Time) return Duration is
         pragma Unsuppress (Overflow_Check);

         Safe_Ada_High : constant Time_Rep := Ada_High - Epoch_Offset;
         --  This value represents a "safe" end of time. In order to perform a
         --  proper conversion to Unix duration, we will have to shift origins
         --  at one point. For very distant dates, this means an overflow check
         --  failure. To prevent this, the function returns the "safe" end of
         --  time (roughly 2219) which is still distant enough.

         Elapsed_Leaps : Natural;
         Next_Leap_N   : Time_Rep;
         Res_N         : Time_Rep;

      begin
         Res_N := Time_Rep (Date);

         --  Step 1: If the target supports leap seconds, remove any leap
         --  seconds elapsed up to the input date.

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Start_Of_Time, Res_N, Elapsed_Leaps, Next_Leap_N);

            --  The input time value may fall on a leap second occurrence

            if Res_N >= Next_Leap_N then
               Elapsed_Leaps := Elapsed_Leaps + 1;
            end if;

         --  The target does not support leap seconds

         else
            Elapsed_Leaps := 0;
         end if;

         Res_N := Res_N - Time_Rep (Elapsed_Leaps) * Nano;

         --  Step 2: Perform a shift in origins to obtain a Unix equivalent of
         --  the input. Guard against very large delay values such as the end
         --  of time since the computation will overflow.

         Res_N := (if Res_N > Safe_Ada_High then Safe_Ada_High
                                            else Res_N + Epoch_Offset);

         return Time_Rep_To_Duration (Res_N);
      end To_Duration;

   end Delay_Operations;

   ---------------------------
   -- Formatting_Operations --
   ---------------------------

   package body Formatting_Operations is

      -----------------
      -- Day_Of_Week --
      -----------------

      function Day_Of_Week (Date : Time) return Integer is
         Date_N    : constant Time_Rep := Time_Rep (Date);
         Time_Zone : constant Long_Integer := UTC_Time_Offset (Date, True);
         Ada_Low_N : Time_Rep;
         Day_Count : Long_Integer;
         Day_Dur   : Time_Dur;
         High_N    : Time_Rep;
         Low_N     : Time_Rep;

      begin
         --  As declared, the Ada Epoch is set in UTC. For this calculation to
         --  work properly, both the Epoch and the input date must be in the
         --  same time zone. The following places the Epoch in the input date's
         --  time zone.

         Ada_Low_N := Ada_Low - Time_Rep (Time_Zone) * Nano;

         if Date_N > Ada_Low_N then
            High_N := Date_N;
            Low_N  := Ada_Low_N;
         else
            High_N := Ada_Low_N;
            Low_N  := Date_N;
         end if;

         --  Determine the elapsed seconds since the start of Ada time

         Day_Dur := Time_Dur (High_N / Nano - Low_N / Nano);

         --  Count the number of days since the start of Ada time. 1901-01-01
         --  GMT was a Tuesday.

         Day_Count := Long_Integer (Day_Dur / Secs_In_Day) + 1;

         return Integer (Day_Count mod 7);
      end Day_Of_Week;

      -----------
      -- Split --
      -----------

      procedure Split
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
         Time_Zone   : Long_Integer)
      is
         --  The following constants represent the number of nanoseconds
         --  elapsed since the start of Ada time to and including the non
         --  leap centennial years.

         Year_2101 : constant Time_Rep := Ada_Low +
                       Time_Rep (49 * 366 + 151 * 365) * Nanos_In_Day;
         Year_2201 : constant Time_Rep := Ada_Low +
                       Time_Rep (73 * 366 + 227 * 365) * Nanos_In_Day;
         Year_2301 : constant Time_Rep := Ada_Low +
                       Time_Rep (97 * 366 + 303 * 365) * Nanos_In_Day;

         Date_Dur       : Time_Dur;
         Date_N         : Time_Rep;
         Day_Seconds    : Natural;
         Elapsed_Leaps  : Natural;
         Four_Year_Segs : Natural;
         Hour_Seconds   : Natural;
         Is_Leap_Year   : Boolean;
         Next_Leap_N    : Time_Rep;
         Rem_Years      : Natural;
         Sub_Sec_N      : Time_Rep;
         Year_Day       : Natural;

      begin
         Date_N := Time_Rep (Date);

         --  Step 1: Leap seconds processing in UTC

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Start_Of_Time, Date_N, Elapsed_Leaps, Next_Leap_N);

            Leap_Sec := Date_N >= Next_Leap_N;

            if Leap_Sec then
               Elapsed_Leaps := Elapsed_Leaps + 1;
            end if;

         --  The target does not support leap seconds

         else
            Elapsed_Leaps := 0;
            Leap_Sec      := False;
         end if;

         Date_N := Date_N - Time_Rep (Elapsed_Leaps) * Nano;

         --  Step 2: Time zone processing. This action converts the input date
         --  from GMT to the requested time zone. Applies from Ada 2005 on.

         if Use_TZ then
            if Time_Zone /= 0 then
               Date_N := Date_N + Time_Rep (Time_Zone) * 60 * Nano;
            end if;

         --  Ada 83 and 95

         else
            declare
               Off : constant Long_Integer :=
                 UTC_Time_Offset (Time (Date_N), Is_Historic);

            begin
               Date_N := Date_N + Time_Rep (Off) * Nano;
            end;
         end if;

         --  Step 3: Non-leap centennial year adjustment in local time zone

         --  In order for all divisions to work properly and to avoid more
         --  complicated arithmetic, we add fake February 29s to dates which
         --  occur after a non-leap centennial year.

         if Date_N >= Year_2301 then
            Date_N := Date_N + Time_Rep (3) * Nanos_In_Day;

         elsif Date_N >= Year_2201 then
            Date_N := Date_N + Time_Rep (2) * Nanos_In_Day;

         elsif Date_N >= Year_2101 then
            Date_N := Date_N + Time_Rep (1) * Nanos_In_Day;
         end if;

         --  Step 4: Sub second processing in local time zone

         Sub_Sec_N := Date_N mod Nano;
         Sub_Sec   := Duration (Sub_Sec_N) / Nano_F;
         Date_N    := Date_N - Sub_Sec_N;

         --  Convert Date_N into a time duration value, changing the units
         --  to seconds.

         Date_Dur := Time_Dur (Date_N / Nano - Ada_Low / Nano);

         --  Step 5: Year processing in local time zone. Determine the number
         --  of four year segments since the start of Ada time and the input
         --  date.

         Four_Year_Segs := Natural (Date_Dur / Secs_In_Four_Years);

         if Four_Year_Segs > 0 then
            Date_Dur := Date_Dur - Time_Dur (Four_Year_Segs) *
                                   Secs_In_Four_Years;
         end if;

         --  Calculate the remaining non-leap years

         Rem_Years := Natural (Date_Dur / Secs_In_Non_Leap_Year);

         if Rem_Years > 3 then
            Rem_Years := 3;
         end if;

         Date_Dur := Date_Dur - Time_Dur (Rem_Years) * Secs_In_Non_Leap_Year;

         Year := Ada_Min_Year + Natural (4 * Four_Year_Segs + Rem_Years);
         Is_Leap_Year := Is_Leap (Year);

         --  Step 6: Month and day processing in local time zone

         Year_Day := Natural (Date_Dur / Secs_In_Day) + 1;

         Month := 1;

         --  Processing for months after January

         if Year_Day > 31 then
            Month    := 2;
            Year_Day := Year_Day - 31;

            --  Processing for a new month or a leap February

            if Year_Day > 28
              and then (not Is_Leap_Year or else Year_Day > 29)
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
         Day_Seconds  := Integer (Date_Dur mod Secs_In_Day);
         Day_Secs     := Duration (Day_Seconds) + Sub_Sec;
         Hour         := Day_Seconds / 3_600;
         Hour_Seconds := Day_Seconds mod 3_600;
         Minute       := Hour_Seconds / 60;
         Second       := Hour_Seconds mod 60;

      exception
         when Constraint_Error =>
            raise Time_Error;
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
         Use_Day_Secs : Boolean;
         Use_TZ       : Boolean;
         Is_Historic  : Boolean;
         Time_Zone    : Long_Integer) return Time
      is
         Count         : Integer;
         Elapsed_Leaps : Natural;
         Next_Leap_N   : Time_Rep;
         Res_N         : Time_Rep;
         Rounded_Res_N : Time_Rep;

      begin
         --  Step 1: Check whether the day, month and year form a valid date

         if Day > Days_In_Month (Month)
           and then (Day /= 29 or else Month /= 2 or else not Is_Leap (Year))
         then
            raise Time_Error;
         end if;

         --  Start accumulating nanoseconds from the low bound of Ada time

         Res_N := Ada_Low;

         --  Step 2: Year processing and centennial year adjustment. Determine
         --  the number of four year segments since the start of Ada time and
         --  the input date.

         Count := (Year - Year_Number'First) / 4;

         for Four_Year_Segments in 1 .. Count loop
            Res_N := Res_N + Nanos_In_Four_Years;
         end loop;

         --  Note that non-leap centennial years are automatically considered
         --  leap in the operation above. An adjustment of several days is
         --  required to compensate for this.

         if Year > 2300 then
            Res_N := Res_N - Time_Rep (3) * Nanos_In_Day;

         elsif Year > 2200 then
            Res_N := Res_N - Time_Rep (2) * Nanos_In_Day;

         elsif Year > 2100 then
            Res_N := Res_N - Time_Rep (1) * Nanos_In_Day;
         end if;

         --  Add the remaining non-leap years

         Count := (Year - Year_Number'First) mod 4;
         Res_N := Res_N + Time_Rep (Count) * Secs_In_Non_Leap_Year * Nano;

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

         Res_N := Res_N + Time_Rep (Count) * Nanos_In_Day;

         --  Step 4: Hour, minute, second and sub second processing

         if Use_Day_Secs then
            Res_N := Res_N + Duration_To_Time_Rep (Day_Secs);

         else
            Res_N :=
              Res_N + Time_Rep (Hour * 3_600 + Minute * 60 + Second) * Nano;

            if Sub_Sec = 1.0 then
               Res_N := Res_N + Time_Rep (1) * Nano;
            else
               Res_N := Res_N + Duration_To_Time_Rep (Sub_Sec);
            end if;
         end if;

         --  At this point, the generated time value should be withing the
         --  bounds of Ada time.

         Check_Within_Time_Bounds (Res_N);

         --  Step 4: Time zone processing. At this point we have built an
         --  arbitrary time value which is not related to any time zone.
         --  For simplicity, the time value is normalized to GMT, producing
         --  a uniform representation which can be treated by arithmetic
         --  operations for instance without any additional corrections.

         if Use_TZ then
            if Time_Zone /= 0 then
               Res_N := Res_N - Time_Rep (Time_Zone) * 60 * Nano;
            end if;

         --  Ada 83 and 95

         else
            declare
               Cur_Off   : constant Long_Integer :=
                 UTC_Time_Offset (Time (Res_N), Is_Historic);
               Cur_Res_N : constant Time_Rep :=
                 Res_N - Time_Rep (Cur_Off) * Nano;
               Off       : constant Long_Integer :=
                 UTC_Time_Offset (Time (Cur_Res_N), Is_Historic);

            begin
               Res_N := Res_N - Time_Rep (Off) * Nano;
            end;
         end if;

         --  Step 5: Leap seconds processing in GMT

         if Leap_Support then
            Cumulative_Leap_Seconds
              (Start_Of_Time, Res_N, Elapsed_Leaps, Next_Leap_N);

            Res_N := Res_N + Time_Rep (Elapsed_Leaps) * Nano;

            --  An Ada 2005 caller requesting an explicit leap second or an
            --  Ada 95 caller accounting for an invisible leap second.

            if Leap_Sec or else Res_N >= Next_Leap_N then
               Res_N := Res_N + Time_Rep (1) * Nano;
            end if;

            --  Leap second validity check

            Rounded_Res_N := Res_N - (Res_N mod Nano);

            if Use_TZ
              and then Leap_Sec
              and then Rounded_Res_N /= Next_Leap_N
            then
               raise Time_Error;
            end if;
         end if;

         return Time (Res_N);
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
      begin
         return UTC_Time_Offset (Date, True);
      end UTC_Time_Offset;

   end Time_Zones_Operations;

--  Start of elaboration code for Ada.Calendar

begin
   System.OS_Primitives.Initialize;

end Ada.Calendar;
