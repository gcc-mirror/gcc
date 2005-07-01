------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C A L E N D A R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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

--  This package extends Ada.Calendar to handle Hour, Minute, Second,
--  Second_Duration and Day_Of_Week and Day_In_Year from Calendar.Time.
--  Second_Duration precision depends on the target clock precision.
--
--  GNAT.Calendar provides the same kind of abstraction found in
--  Ada.Calendar. It provides Split and Time_Of to build and split a Time
--  data. And it provides accessor functions to get only one of Hour, Minute,
--  Second, Second_Duration. Other functions are to access more advanced
--  valueas like Day_Of_Week, Day_In_Year and Week_In_Year.

with Ada.Calendar;
with Interfaces.C;

package GNAT.Calendar is

   type Day_Name is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   subtype Hour_Number         is Natural range 0 .. 23;
   subtype Minute_Number       is Natural range 0 .. 59;
   subtype Second_Number       is Natural range 0 .. 59;
   subtype Second_Duration     is Ada.Calendar.Day_Duration range 0.0 .. 1.0;
   subtype Day_In_Year_Number  is Positive range 1 .. 366;
   subtype Week_In_Year_Number is Positive range 1 .. 53;

   function Hour       (Date : Ada.Calendar.Time) return Hour_Number;
   function Minute     (Date : Ada.Calendar.Time) return Minute_Number;
   function Second     (Date : Ada.Calendar.Time) return Second_Number;
   function Sub_Second (Date : Ada.Calendar.Time) return Second_Duration;
   --  Hour, Minute, Sedond and Sub_Second returns the complete time data for
   --  the Date (H:M:S.SS). See Ada.Calendar for Year, Month, Day accessors.
   --  Second_Duration precision depends on the target clock precision.

   function Day_Of_Week (Date : Ada.Calendar.Time) return Day_Name;
   --  Return the day name.

   function Day_In_Year (Date : Ada.Calendar.Time) return Day_In_Year_Number;
   --  Returns the day number in the year. (1st January is day 1 and 31st
   --  December is day 365 or 366 for leap year).

   function Week_In_Year (Date : Ada.Calendar.Time) return Week_In_Year_Number;
   --  Returns the week number in the year with Monday as first day of week

   procedure Split
     (Date       : Ada.Calendar.Time;
      Year       : out Ada.Calendar.Year_Number;
      Month      : out Ada.Calendar.Month_Number;
      Day        : out Ada.Calendar.Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);
   --  Split the standard Ada.Calendar.Time data in date data (Year, Month,
   --  Day) and Time data (Hour, Minute, Second, Sub_Second)

   function Time_Of
     (Year       : Ada.Calendar.Year_Number;
      Month      : Ada.Calendar.Month_Number;
      Day        : Ada.Calendar.Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0)
      return Ada.Calendar.Time;
   --  Returns an Ada.Calendar.Time data built from the date and time values.

   --  C timeval conversion

   --  C timeval represent a duration (used in Select for example). This
   --  structure is composed of a number of seconds and a number of micro
   --  seconds. The timeval structure is not exposed here because its
   --  definition is target dependent. Interface to C programs is done via a
   --  pointer to timeval structure.

   type timeval is private;

   function To_Duration (T : access timeval) return Duration;
   function To_Timeval  (D : Duration) return timeval;

private
   --  This is a dummy declaration that should be the largest possible timeval
   --  structure of all supported targets.

   type timeval is array (1 .. 2) of Interfaces.C.long;

   function Julian_Day
     (Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number)
      return  Integer;
   --  Compute Julian day number.
   --
   --  The code of this function is a modified version of algorithm
   --  199 from the Collected Algorithms of the ACM.
   --  The author of algorithm 199 is Robert G. Tantzen.
end GNAT.Calendar;
