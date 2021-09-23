------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . C O N V E R S I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2008-2021, Free Software Foundation, Inc.         --
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

--  This package provides various routines for conversion between Ada and Unix
--  time models - Time, Duration, struct tm and struct timespec.

with Interfaces.C;

package Ada.Calendar.Conversions is

   function To_Ada_Time (Unix_Time : Interfaces.C.long) return Time;
   --  Convert a time value represented as number of seconds since the
   --  Unix Epoch to a time value relative to an Ada implementation-defined
   --  Epoch. The units of the result are nanoseconds on all targets. Raises
   --  Time_Error if the result cannot fit into a Time value.

   function To_Ada_Time
     (tm_year  : Interfaces.C.int;
      tm_mon   : Interfaces.C.int;
      tm_day   : Interfaces.C.int;
      tm_hour  : Interfaces.C.int;
      tm_min   : Interfaces.C.int;
      tm_sec   : Interfaces.C.int;
      tm_isdst : Interfaces.C.int) return Time;
   --  Convert a time value expressed in Unix-like fields of struct tm into
   --  a Time value relative to the Ada Epoch. The ranges of the formals are
   --  as follows:

   --     tm_year   --  years since 1900
   --     tm_mon    --  months since January [0 .. 11]
   --     tm_day    --  day of the month [1 .. 31]
   --     tm_hour   --  hours since midnight [0 .. 24]
   --     tm_min    --  minutes after the hour [0 .. 59]
   --     tm_sec    --  seconds after the minute [0 .. 60]
   --     tm_isdst  --  Daylight Savings Time flag [-1 .. 1]

   --  The returned value is in UTC and may or may not contain leap seconds
   --  depending on whether binder flag "-y" was used. Raises Time_Error if
   --  the input values are out of the defined ranges or if tm_sec equals 60
   --  and the instance in time is not a leap second occurrence.

   function To_Duration
     (tv_sec  : Interfaces.C.long;
      tv_nsec : Interfaces.C.long) return Duration;
   --  Convert an elapsed time value expressed in Unix-like fields of struct
   --  timespec into a Duration value. The expected ranges are:

   --     tv_sec   -  seconds
   --     tv_nsec  -  nanoseconds

   procedure To_Struct_Timespec
     (D       : Duration;
      tv_sec  : out Interfaces.C.long;
      tv_nsec : out Interfaces.C.long);
   --  Convert a Duration value into the constituents of struct timespec.
   --  Formal tv_sec denotes seconds and tv_nsecs denotes nanoseconds.

   procedure To_Struct_Tm
     (T       : Time;
      tm_year : out Interfaces.C.int;
      tm_mon  : out Interfaces.C.int;
      tm_day  : out Interfaces.C.int;
      tm_hour : out Interfaces.C.int;
      tm_min  : out Interfaces.C.int;
      tm_sec  : out Interfaces.C.int);
   --  Convert a Time value set in the Ada Epoch into the constituents of
   --  struct tm. The ranges of the out formals are as follows:

   --     tm_year   --  years since 1900
   --     tm_mon    --  months since January [0 .. 11]
   --     tm_day    --  day of the month [1 .. 31]
   --     tm_hour   --  hours since midnight [0 .. 24]
   --     tm_min    --  minutes after the hour [0 .. 59]
   --     tm_sec    --  seconds after the minute [0 .. 60]
   --     tm_isdst  --  Daylight Savings Time flag [-1 .. 1]

   --  The input date is considered to be in UTC

   function To_Unix_Time (Ada_Time : Time) return Interfaces.C.long;
   --  Convert a time value represented as number of time units since the Ada
   --  implementation-defined Epoch to a value relative to the Unix Epoch. The
   --  units of the result are seconds. Raises Time_Error if the result cannot
   --  fit into a Time value.

   function To_Unix_Nano_Time
     (Ada_Time : Time) return Interfaces.C.long_long;
   --  Convert a time value represented as number of time units since the Ada
   --  implementation-defined Epoch to a value relative to the Unix Epoch. The
   --  units of the result are nanoseconds. Raises Time_Error if the result
   --  cannot fit into a Time value.

end Ada.Calendar.Conversions;
