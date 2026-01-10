------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2026, Free Software Foundation, Inc.         --
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

with Interfaces.C; use Interfaces.C;

package body System.OS_Interface is

   ------------------------
   -- To_Target_Priority --
   ------------------------

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Prio);
   end To_Target_Priority;

   -------------------
   -- clock_gettime --
   -------------------

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access C_Time.timespec) return int
   is
      pragma Unreferenced (clock_id);

      --  Darwin Threads don't have clock_gettime, so use gettimeofday

      TV     : aliased C_Time.timeval;
      Result : int;

      function gettimeofday
        (Tv : access C_Time.timeval;
         Tz : System.Address := System.Null_Address) return int;
      pragma Import (C, gettimeofday, "gettimeofday");

   begin
      Result := gettimeofday (TV'Access, System.Null_Address);
      pragma Assert (Result = 0);
      tp.all := C_Time.To_Timespec (TV);
      return Result;
   end clock_gettime;

   ------------------
   -- clock_getres --
   ------------------

   function clock_getres
     (clock_id : clockid_t;
      res      : access C_Time.timespec) return int
   is
      pragma Unreferenced (clock_id);

      --  Darwin Threads don't have clock_getres.

      nsec   : int := 0;
      Result : int := -1;

      function clock_get_res return int;
      pragma Import (C, clock_get_res, "__gnat_clock_get_res");

   begin
      nsec := clock_get_res;
      res.all := C_Time.Nanoseconds_To_Timespec (nsec);

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
