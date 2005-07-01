------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for POSIX-like operating systems

package body System.OS_Primitives is

   --  ??? These definitions are duplicated from System.OS_Interface
   --  because we don't want to depend on any package. Consider removing
   --  these declarations in System.OS_Interface and move these ones in
   --  the spec.

   type struct_timezone is record
      tz_minuteswest  : Integer;
      tz_dsttime   : Integer;
   end record;
   pragma Convention (C, struct_timezone);
   type struct_timezone_ptr is access all struct_timezone;

   type time_t is new Long_Integer;

   type struct_timeval is record
      tv_sec       : time_t;
      tv_usec      : Long_Integer;
   end record;
   pragma Convention (C, struct_timeval);

   function gettimeofday
     (tv : access struct_timeval;
      tz : struct_timezone_ptr) return Integer;
   pragma Import (C, gettimeofday, "gettimeofday");

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : Long_Integer;
   end record;
   pragma Convention (C, timespec);

   function nanosleep (rqtp, rmtp : access timespec) return Integer;
   pragma Import (C, nanosleep, "nanosleep");

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TV     : aliased struct_timeval;

      Result : Integer;
      pragma Unreferenced (Result);

   begin
      Result := gettimeofday (TV'Access, null);
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
   end Clock;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration renames Clock;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec;

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return
        timespec'(tv_sec  => S,
                  tv_nsec => Long_Integer (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is
      Request : aliased timespec;
      Remaind : aliased timespec;
      Rel_Time : Duration;
      Abs_Time : Duration;
      Check_Time : Duration := Clock;

      Result : Integer;
      pragma Unreferenced (Result);

   begin
      if Mode = Relative then
         Rel_Time := Time;
         Abs_Time := Time + Check_Time;
      else
         Rel_Time := Time - Check_Time;
         Abs_Time := Time;
      end if;

      if Rel_Time > 0.0 then
         loop
            Request := To_Timespec (Rel_Time);
            Result := nanosleep (Request'Access, Remaind'Access);
            Check_Time := Clock;

            exit when Abs_Time <= Check_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

end System.OS_Primitives;
