------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a Darwin Threads version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C.Extensions;

package body System.OS_Interface is
   use Interfaces.C;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   ------------------------
   -- To_Target_Priority --
   ------------------------

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Prio);
   end To_Target_Priority;

   -----------------
   -- To_Timespec --
   -----------------

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

      return timespec'(tv_sec => S,
        tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------------
   -- clock_gettime --
   -------------------

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int
   is
      pragma Unreferenced (clock_id);

      --  Darwin Threads don't have clock_gettime, so use gettimeofday

      use Interfaces;

      type timeval is array (1 .. 3) of C.long;
      --  The timeval array is sized to contain long_long sec and long usec.
      --  If long_long'Size = long'Size then it will be overly large but that
      --  won't effect the implementation since it's not accessed directly.

      procedure timeval_to_duration
        (T    : not null access timeval;
         sec  : not null access C.Extensions.long_long;
         usec : not null access C.long);
      pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

      Micro  : constant := 10**6;
      sec    : aliased C.Extensions.long_long;
      usec   : aliased C.long;
      TV     : aliased timeval;
      Result : int;

      function gettimeofday
        (Tv : access timeval;
         Tz : System.Address := System.Null_Address) return int;
      pragma Import (C, gettimeofday, "gettimeofday");

   begin
      Result := gettimeofday (TV'Access, System.Null_Address);
      pragma Assert (Result = 0);
      timeval_to_duration (TV'Access, sec'Access, usec'Access);
      tp.all := To_Timespec (Duration (sec) + Duration (usec) / Micro);
      return Result;
   end clock_gettime;

   ------------------
   -- clock_getres --
   ------------------

   function clock_getres
     (clock_id : clockid_t;
      res      : access timespec) return int
   is
      pragma Unreferenced (clock_id);

      --  Darwin Threads don't have clock_getres.

      Nano   : constant := 10**9;
      nsec   : int := 0;
      Result : int := -1;

      function clock_get_res return int;
      pragma Import (C, clock_get_res, "__gnat_clock_get_res");

   begin
      nsec := clock_get_res;
      res.all := To_Timespec (Duration (0.0) + Duration (nsec) / Nano);

      if nsec > 0 then
         Result := 0;
      end if;

      return Result;
   end clock_getres;

   -----------------
   -- sched_yield --
   -----------------

   function sched_yield return int is
      procedure sched_yield_base (arg : System.Address);
      pragma Import (C, sched_yield_base, "pthread_yield_np");

   begin
      sched_yield_base (System.Null_Address);
      return 0;
   end sched_yield;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Unreferenced (thread);
   begin
      return System.Null_Address;
   end Get_Stack_Base;

end System.OS_Interface;
