------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is the Alpha/VMS version

with System.OS_Primitives;

package Ada.Calendar is

   package OSP renames System.OS_Primitives;

   type Time is private;

   --  Declarations representing limits of allowed local time values. Note
   --  that these do NOT constrain the possible stored values of time which
   --  may well permit a larger range of times (this is explicitly allowed
   --  in Ada 95).

   subtype Year_Number  is Integer range 1901 .. 2399;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 31;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   function Clock return Time;

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

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time;

   function "+" (Left : Time;     Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time)     return Time;
   function "-" (Left : Time;     Right : Duration) return Time;
   function "-" (Left : Time;     Right : Time)     return Duration;

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

   type Time is new OSP.OS_Time;

   Days_In_Month : constant array (Month_Number) of Day_Number :=
                     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   Invalid_Time_Zone_Offset : Long_Integer;
   pragma Import (C, Invalid_Time_Zone_Offset, "__gnat_invalid_tzoff");

   function Is_Leap (Year : Year_Number) return Boolean;
   --  Determine whether a given year is leap

   --  The following packages provide a target independent interface to the
   --  children of Calendar - Arithmetic, Formatting and Time_Zones.

   --  NOTE: Delays does not need a target independent interface because
   --  VMS already has a target specific file for that package.

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

   package Formatting_Operations is
      function Day_Of_Week (Date : Time) return Integer;
      --  Determine which day of week Date falls on. The returned values are
      --  within the range of 0 .. 6 (Monday .. Sunday).

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
         Time_Zone : Long_Integer);
      --  Split a time value into its components. Set Is_Ada_05 to use the
      --  local time zone (the value in Time_Zone is ignored) when splitting
      --  a time value.

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
         Is_Ada_05    : Boolean;
         Time_Zone    : Long_Integer) return Time;
      --  Given all the components of a date, return the corresponding time
      --  value. Set Use_Day_Secs to use the value in Day_Secs, otherwise the
      --  day duration will be calculated from Hour, Minute, Second and Sub_
      --  Sec. Set Is_Ada_05 to use the local time zone (the value in formal
      --  Time_Zone is ignored) when building a time value and to verify the
      --  validity of a requested leap second.
   end Formatting_Operations;

   package Time_Zones_Operations is
      function UTC_Time_Offset (Date : Time) return Long_Integer;
      --  Return the offset in seconds from UTC
   end Time_Zones_Operations;

end Ada.Calendar;
