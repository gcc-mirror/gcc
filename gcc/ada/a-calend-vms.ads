------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

   --  Declarations representing limits of allowed local time values. Note that
   --  these do NOT constrain the possible stored values of time which may well
   --  permit a larger range of times (this is explicitly allowed in Ada 95).

   subtype Year_Number  is Integer range 1901 .. 2099;
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
      Seconds : Day_Duration := 0.0)
      return    Time;

   function "+" (Left : Time;     Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time)     return Time;
   function "-" (Left : Time;     Right : Duration) return Time;
   function "-" (Left : Time;     Right : Time)     return Duration;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   Time_Error : exception;

   Unimplemented : exception;

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

   --  Time is represented as the number of 100-nanosecond (ns) units offset
   --  from the system base date and time, which is 00:00 o'clock,
   --  November 17, 1858 (the Smithsonian base date and time for the
   --  astronomic calendar).

   --  The time value stored is typically a GMT value, as provided in standard
   --  Unix environments. If this is the case then Split and Time_Of perform
   --  required conversions to and from local times.

   type Time is new OSP.OS_Time;

   --  Notwithstanding this definition, Time is not quite the same as OS_Time.
   --  Relative Time is positive, whereas relative OS_Time is negative,
   --  but this declaration makes for easier conversion.

   --  The following package provides handling of leap seconds. It is
   --  used by Ada.Calendar.Arithmetic and Ada.Calendar.Formatting, both
   --  Ada 2005 children of Ada.Calendar.

   package Leap_Sec_Ops is

      After_Last_Leap : constant Time := Time'Last;
      --  Bigger by far than any leap second value. Not within range of
      --  Ada.Calendar specified dates.

      procedure Cumulative_Leap_Secs
        (Start_Date    : Time;
         End_Date      : Time;
         Leaps_Between : out Duration;
         Next_Leap_Sec : out Time);
      --  Leaps_Between is the sum of the leap seconds that have occured
      --  on or after Start_Date and before (strictly before) End_Date.
      --  Next_Leap_Sec represents the next leap second occurence on or
      --  after End_Date. If there are no leaps seconds after End_Date,
      --  After_Last_Leap is returned. This does not provide info about
      --  the next leap second (pos/neg or ?). After_Last_Leap can be used
      --  as End_Date to count all the leap seconds that have occured on
      --  or after Start_Date.
      --
      --  Important Notes: any fractional parts of Start_Date and End_Date
      --  are discarded before the calculations are done. For instance: if
      --  113 seconds is a leap second (it isn't) and 113.5 is input as an
      --  End_Date, the leap second at 113 will not be counted in
      --  Leaps_Between, but it will be returned as Next_Leap_Sec. Thus, if
      --  the caller wants to know if the End_Date is a leap second, the
      --  comparison should be:
      --
      --     End_Date >= Next_Leap_Sec;
      --
      --  After_Last_Leap is designed so that this comparison works without
      --  having to first check if Next_Leap_Sec is a valid leap second.

      function All_Leap_Seconds return Duration;
      --  Returns the sum off all of the leap seoncds.

   end Leap_Sec_Ops;

   procedure Split_With_Offset
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration;
      Offset  : out Long_Integer);
   --  Split_W_Offset has the same spec as Split with the addition of an
   --  offset value which give the offset of the local time zone from UTC
   --  at the input Date. This value comes for free during the implementation
   --  of Split and is needed by UTC_Time_Offset. The returned Offset time
   --  is straight from the C tm struct and is in seconds. If the system
   --  dependent code has no way to find the offset it will return the value
   --  Invalid_TZ_Offset declared below. Otherwise no checking is done, so
   --  it is up to the user to check both for Invalid_TZ_Offset and otherwise
   --  for a value that is acceptable.

   Invalid_TZ_Offset : Long_Integer;
   pragma Import (C, Invalid_TZ_Offset, "__gnat_invalid_tzoff");

end Ada.Calendar;
