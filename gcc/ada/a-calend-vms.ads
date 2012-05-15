------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is the OpenVMS version

with System.OS_Primitives;

package Ada.Calendar is

   type Time is private;

   --  Declarations representing limits of allowed local time values. Note that
   --  these do NOT constrain the possible stored values of time which may well
   --  permit a larger range of times (this is explicitly allowed in Ada 95).

   subtype Year_Number  is Integer range 1901 .. 2399;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 31;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;
   --  Note that a value of 86_400.0 is the start of the next day

   function Clock return Time;
   --  The returned time value is the number of nanoseconds since the start
   --  of Ada time (1901-01-01 00:00:00.0 UTC). If leap seconds are enabled,
   --  the result will contain all elapsed leap seconds since the start of
   --  Ada time until now.

   function Year    (Date : Time) return Year_Number;
   function Month   (Date : Time) return Month_Number;
   function Day     (Date : Time) return Day_Number;
   function Seconds (Date : Time) return Day_Duration;

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);
   --  Break down a time value into its date components set in the current
   --  time zone. If Split is called on a time value created using Ada 2005
   --  Time_Of in some arbitrary time zone, the input value will always be
   --  interpreted as relative to the local time zone.

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time;
   --  GNAT Note: Normally when procedure Split is called on a Time value
   --  result of a call to function Time_Of, the out parameters of procedure
   --  Split are identical to the in parameters of function Time_Of. However,
   --  when a non-existent time of day is specified, the values for Seconds
   --  may or may not be different. This may happen when Daylight Saving Time
   --  (DST) is in effect, on the day when switching to DST, if Seconds
   --  specifies a time of day in the hour that does not exist. For example,
   --  in New York:
   --
   --    Time_Of (Year => 1998, Month => 4, Day => 5, Seconds => 10740.0)
   --
   --  will return a Time value T. If Split is called on T, the resulting
   --  Seconds may be 14340.0 (3:59:00) instead of 10740.0 (2:59:00 being
   --  a time that not exist).

   function "+" (Left : Time;     Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time)     return Time;
   function "-" (Left : Time;     Right : Duration) return Time;
   function "-" (Left : Time;     Right : Time)     return Duration;
   --  The first three functions will raise Time_Error if the resulting time
   --  value is less than the start of Ada time in UTC or greater than the
   --  end of Ada time in UTC. The last function will raise Time_Error if the
   --  resulting difference cannot fit into a duration value.

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   Time_Error : exception;

private
   pragma Inline (Clock);

   pragma Inline (Year);
   pragma Inline (Month);
   pragma Inline (Day);

   pragma Inline ("+");
   pragma Inline ("-");

   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");

   --  Although the units are 100 nanoseconds, for the purpose of better
   --  readability, this unit will be called "mili".

   Mili         : constant := 10_000_000;
   Mili_F       : constant := 10_000_000.0;
   Milis_In_Day : constant := 864_000_000_000;
   Secs_In_Day  : constant := 86_400;

   --  Time is represented as the number of 100-nanosecond (ns) units from the
   --  system base date and time 1858-11-17 0.0 (the Smithsonian base date and
   --  time for the astronomic calendar).

   --  The time value stored is typically a UTC value, as provided in standard
   --  Unix environments. If this is the case then Split and Time_Of perform
   --  required conversions to and from local times.

   --  Notwithstanding this definition, Time is not quite the same as OS_Time.
   --  Relative Time is positive, whereas relative OS_Time is negative,
   --  but this declaration makes for easier conversion.

   type Time is new System.OS_Primitives.OS_Time;

   Days_In_Month : constant array (Month_Number) of Day_Number :=
                     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
   --  Days in month for non-leap year, leap year case is adjusted in code

   Invalid_Time_Zone_Offset : Long_Integer;
   pragma Import (C, Invalid_Time_Zone_Offset, "__gnat_invalid_tzoff");

   function Is_Leap (Year : Year_Number) return Boolean;
   --  Determine whether a given year is leap

   ----------------------------------------------------------
   -- Target-Independent Interface to Children of Calendar --
   ----------------------------------------------------------

   --  The following packages provide a target-independent interface to the
   --  children of Calendar - Arithmetic, Conversions, Delays, Formatting and
   --  Time_Zones.

   --  NOTE: Delays does not need a target independent interface because
   --  VMS already has a target specific file for that package.

   ---------------------------
   -- Arithmetic_Operations --
   ---------------------------

   package Arithmetic_Operations is

      function Add (Date : Time; Days : Long_Integer) return Time;
      --  Add a certain number of days to a time value

      procedure Difference
        (Left         : Time;
         Right        : Time;
         Days         : out Long_Integer;
         Seconds      : out Duration;
         Leap_Seconds : out Integer);
      --  Calculate the difference between two time values in terms of days,
      --  seconds and leap seconds elapsed. The leap seconds are not included
      --  in the seconds returned. If Left is greater than Right, the returned
      --  values are positive, negative otherwise.

      function Subtract (Date : Time; Days : Long_Integer) return Time;
      --  Subtract a certain number of days from a time value

   end Arithmetic_Operations;

   ---------------------------
   -- Conversion_Operations --
   ---------------------------

   package Conversion_Operations is

      function To_Ada_Time (Unix_Time : Long_Integer) return Time;
      --  Unix to Ada Epoch conversion

      function To_Ada_Time
        (tm_year  : Integer;
         tm_mon   : Integer;
         tm_day   : Integer;
         tm_hour  : Integer;
         tm_min   : Integer;
         tm_sec   : Integer;
         tm_isdst : Integer) return Time;
      --  Struct tm to Ada Epoch conversion

      function To_Duration
        (tv_sec  : Long_Integer;
         tv_nsec : Long_Integer) return Duration;
      --  Struct timespec to Duration conversion

      procedure To_Struct_Timespec
        (D       : Duration;
         tv_sec  : out Long_Integer;
         tv_nsec : out Long_Integer);
      --  Duration to struct timespec conversion

      procedure To_Struct_Tm
        (T       : Time;
         tm_year : out Integer;
         tm_mon  : out Integer;
         tm_day  : out Integer;
         tm_hour : out Integer;
         tm_min  : out Integer;
         tm_sec  : out Integer);
      --  Time to struct tm conversion

      function To_Unix_Time (Ada_Time : Time) return Long_Integer;
      --  Ada to Unix Epoch conversion

   end Conversion_Operations;

   ---------------------------
   -- Formatting_Operations --
   ---------------------------

   package Formatting_Operations is

      function Day_Of_Week (Date : Time) return Integer;
      --  Determine which day of week Date falls on. The returned values are
      --  within the range of 0 .. 6 (Monday .. Sunday).

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
         Time_Zone   : Long_Integer);
      pragma Export (Ada, Split, "__gnat_split");
      --  Split a time value into its components. If flag Is_Historic is set,
      --  this routine would try to use to the best of the OS's abilities the
      --  time zone offset that was or will be in effect on Date. Set Use_TZ
      --  to use the local time zone (the value in Time_Zone is ignored) when
      --  splitting a time value.

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
         Time_Zone    : Long_Integer) return Time;
      pragma Export (Ada, Time_Of, "__gnat_time_of");
      --  Given all the components of a date, return the corresponding time
      --  value. Set Use_Day_Secs to use the value in Day_Secs, otherwise the
      --  day duration will be calculated from Hour, Minute, Second and Sub_
      --  Sec. If flag Is_Historic is set, this routine would try to use to the
      --  best of the OS's abilities the time zone offset that was or will be
      --  in effect on the input date. Set Use_TZ to use the local time zone
      --  (the value in formal Time_Zone is ignored) when building a time value
      --  and to verify the validity of a requested leap second.

   end Formatting_Operations;

   ---------------------------
   -- Time_Zones_Operations --
   ---------------------------

   package Time_Zones_Operations is

      function UTC_Time_Offset (Date : Time) return Long_Integer;
      --  Return (in seconds) the difference between the local time zone and
      --  UTC time at a specific historic date.

   end Time_Zones_Operations;

end Ada.Calendar;
