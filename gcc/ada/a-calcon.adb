------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . C O N V E R S I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
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

with Interfaces.C; use Interfaces.C;

package body Ada.Calendar.Conversions is

   -----------------
   -- To_Ada_Time --
   -----------------

   function To_Ada_Time (Unix_Time : long) return Time is
      Val : constant Long_Integer := Long_Integer (Unix_Time);
   begin
      return Conversion_Operations.To_Ada_Time (Val);
   end To_Ada_Time;

   -----------------
   -- To_Ada_Time --
   -----------------

   function To_Ada_Time
     (tm_year  : int;
      tm_mon   : int;
      tm_day   : int;
      tm_hour  : int;
      tm_min   : int;
      tm_sec   : int;
      tm_isdst : int) return Time
   is
      Year   : constant Integer := Integer (tm_year);
      Month  : constant Integer := Integer (tm_mon);
      Day    : constant Integer := Integer (tm_day);
      Hour   : constant Integer := Integer (tm_hour);
      Minute : constant Integer := Integer (tm_min);
      Second : constant Integer := Integer (tm_sec);
      DST    : constant Integer := Integer (tm_isdst);
   begin
      return
        Conversion_Operations.To_Ada_Time
          (Year, Month, Day, Hour, Minute, Second, DST);
   end To_Ada_Time;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration
     (tv_sec  : long;
      tv_nsec : long) return Duration
   is
      Secs      : constant Long_Integer := Long_Integer (tv_sec);
      Nano_Secs : constant Long_Integer := Long_Integer (tv_nsec);
   begin
      return Conversion_Operations.To_Duration (Secs, Nano_Secs);
   end To_Duration;

   ------------------------
   -- To_Struct_Timespec --
   ------------------------

   procedure To_Struct_Timespec
     (D       : Duration;
      tv_sec  : out long;
      tv_nsec : out long)
   is
      Secs      : Long_Integer;
      Nano_Secs : Long_Integer;

   begin
      Conversion_Operations.To_Struct_Timespec (D, Secs, Nano_Secs);

      tv_sec  := long (Secs);
      tv_nsec := long (Nano_Secs);
   end To_Struct_Timespec;

   ------------------
   -- To_Struct_Tm --
   ------------------

   procedure To_Struct_Tm
     (T       : Time;
      tm_year : out int;
      tm_mon  : out int;
      tm_day  : out int;
      tm_hour : out int;
      tm_min  : out int;
      tm_sec  : out int)
   is
      Year   : Integer;
      Month  : Integer;
      Day    : Integer;
      Hour   : Integer;
      Minute : Integer;
      Second : Integer;

   begin
      Conversion_Operations.To_Struct_Tm
        (T, Year, Month, Day, Hour, Minute, Second);

      tm_year := int (Year);
      tm_mon  := int (Month);
      tm_day  := int (Day);
      tm_hour := int (Hour);
      tm_min  := int (Minute);
      tm_sec  := int (Second);
   end To_Struct_Tm;

   ------------------
   -- To_Unix_Time --
   ------------------

   function To_Unix_Time (Ada_Time : Time) return long is
      Val : constant Long_Integer :=
              Conversion_Operations.To_Unix_Time (Ada_Time);
   begin
      return long (Val);
   end To_Unix_Time;

end Ada.Calendar.Conversions;
