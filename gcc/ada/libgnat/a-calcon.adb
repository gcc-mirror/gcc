------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . C O N V E R S I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 2008-2024, Free Software Foundation, Inc.           --
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

with Interfaces.C;            use Interfaces.C;

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

   -----------------------
   -- To_Unix_Nano_Time --
   -----------------------

   function To_Unix_Nano_Time (Ada_Time : Time) return long_long is
      pragma Unsuppress (Overflow_Check);
      Ada_Rep : constant Time_Rep := Time_Rep (Ada_Time);

   begin
      return long_long (Ada_Rep + Epoch_Offset);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end To_Unix_Nano_Time;

end Ada.Calendar.Conversions;
