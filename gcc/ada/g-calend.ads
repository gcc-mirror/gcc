------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C A L E N D A R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2014, Free Software Foundation, Inc.         --
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

--  This package extends Ada.Calendar to handle Hour, Minute, Second,
--  Second_Duration and Day_Of_Week and Day_In_Year from Calendar.Time.
--  Second_Duration precision depends on the target clock precision.
--
--  GNAT.Calendar provides the same kind of abstraction found in Ada.Calendar.
--  It provides Split and Time_Of to build and split a Time data. And it
--  provides accessor functions to get only one of Hour, Minute, Second,
--  Second_Duration. Other functions are to access more advanced values like
--  Day_Of_Week, Day_In_Year and Week_In_Year.

with Ada.Calendar;
with Interfaces.C;

package GNAT.Calendar is

   type Day_Name is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
   pragma Ordered (Day_Name);

   subtype Hour_Number         is Natural range 0 .. 23;
   subtype Minute_Number       is Natural range 0 .. 59;
   subtype Second_Number       is Natural range 0 .. 59;
   subtype Second_Duration     is Ada.Calendar.Day_Duration range 0.0 .. 1.0;
   subtype Day_In_Year_Number  is Positive range 1 .. 366;
   subtype Week_In_Year_Number is Positive range 1 .. 53;

   No_Time : constant Ada.Calendar.Time;
   --  A constant set to the first date that can be represented by the type
   --  Time. It can be used to indicate an uninitialized date.

   function Hour       (Date : Ada.Calendar.Time) return Hour_Number;
   function Minute     (Date : Ada.Calendar.Time) return Minute_Number;
   function Second     (Date : Ada.Calendar.Time) return Second_Number;
   function Sub_Second (Date : Ada.Calendar.Time) return Second_Duration;
   --  Hour, Minute, Second and Sub_Second returns the complete time data for
   --  the Date (H:M:S.SS). See Ada.Calendar for Year, Month, Day accessors.
   --  Second_Duration precision depends on the target clock precision.

   function Day_Of_Week (Date : Ada.Calendar.Time) return Day_Name;
   --  Return the day name

   function Day_In_Year (Date : Ada.Calendar.Time) return Day_In_Year_Number;
   --  Return the day number in the year. (1st January is day 1 and 31st
   --  December is day 365 or 366 for leap year).

   procedure Split
     (Date       : Ada.Calendar.Time;
      Year       : out Ada.Calendar.Year_Number;
      Month      : out Ada.Calendar.Month_Number;
      Day        : out Ada.Calendar.Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);
   --  Split a standard Ada.Calendar.Time value in date data (Year, Month, Day)
   --  and Time data (Hour, Minute, Second, Sub_Second).

   procedure Split_At_Locale
     (Date       : Ada.Calendar.Time;
      Year       : out Ada.Calendar.Year_Number;
      Month      : out Ada.Calendar.Month_Number;
      Day        : out Ada.Calendar.Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);
   --  Split a standard Ada.Calendar.Time value in date data (Year, Month, Day)
   --  and Time data (Hour, Minute, Second, Sub_Second). This version of Split
   --  utilizes the time zone and DST bias of the locale (equivalent to Clock).
   --  Due to this simplified behavior, the implementation does not require
   --  expensive system calls on targets such as Windows.
   --  WARNING: Split_At_Locale is no longer aware of historic events and may
   --  produce inaccurate results over DST changes which occurred in the past.

   function Time_Of
     (Year       : Ada.Calendar.Year_Number;
      Month      : Ada.Calendar.Month_Number;
      Day        : Ada.Calendar.Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0) return Ada.Calendar.Time;
   --  Return an Ada.Calendar.Time data built from the date and time values

   function Time_Of_At_Locale
     (Year       : Ada.Calendar.Year_Number;
      Month      : Ada.Calendar.Month_Number;
      Day        : Ada.Calendar.Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0) return Ada.Calendar.Time;
   --  Return an Ada.Calendar.Time data built from the date and time values.
   --  This version of Time_Of utilizes the time zone and DST bias of the
   --  locale (equivalent to Clock). Due to this simplified behavior, the
   --  implementation does not require expensive system calls on targets such
   --  as Windows.
   --  WARNING: Split_At_Locale is no longer aware of historic events and may
   --  produce inaccurate results over DST changes which occurred in the past.

   function Week_In_Year (Date : Ada.Calendar.Time) return Week_In_Year_Number;
   --  Return the week number as defined in ISO 8601. A week always starts on
   --  a Monday and the first week of a particular year is the one containing
   --  the first Thursday. A year may have 53 weeks when January 1st is a
   --  Wednesday and the year is leap or January 1st is a Thursday. Note that
   --  the last days of December may belong to the first week on the next year
   --  and conversely, the first days of January may belong to the last week
   --  of the last year.

   procedure Year_Week_In_Year
     (Date : Ada.Calendar.Time;
      Year : out Ada.Calendar.Year_Number;
      Week : out Week_In_Year_Number);
   --  Return the week number as defined in ISO 8601 along with the year in
   --  which the week occurs.

   --  C timeval conversion

   --  C timeval represent a duration (used in Select for example). This
   --  structure is composed of a number of seconds and a number of micro
   --  seconds. The timeval structure is not exposed here because its
   --  definition is target dependent. Interface to C programs is done via a
   --  pointer to timeval structure.

   type timeval is private;

   function To_Duration (T : not null access timeval) return Duration;
   function To_Timeval  (D : Duration) return timeval;

private
   --  This is a dummy declaration that should be the largest possible timeval
   --  structure of all supported targets.

   type timeval is array (1 .. 3) of Interfaces.C.long;

   function Julian_Day
     (Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number) return Integer;
   --  Compute Julian day number
   --
   --  The code of this function is a modified version of algorithm 199 from
   --  the Collected Algorithms of the ACM. The author of algorithm 199 is
   --  Robert G. Tantzen.

   No_Time : constant Ada.Calendar.Time :=
               Ada.Calendar.Time_Of
                 (Ada.Calendar.Year_Number'First,
                  Ada.Calendar.Month_Number'First,
                  Ada.Calendar.Day_Number'First);

end GNAT.Calendar;
