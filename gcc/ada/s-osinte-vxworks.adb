------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--             Copyright (C) 1997-2005 Free Software Foundation             --
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

--  This is the VxWorks version.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

package body System.OS_Interface is

   use type Interfaces.C.int;

   Low_Priority : constant := 255;
   --  VxWorks native (default) lowest scheduling priority.

   ----------
   -- kill --
   ----------

   function kill (pid : t_id; sig : Signal) return int is
      function c_kill (pid : t_id; sig : Signal) return int;
      pragma Import (C, c_kill, "kill");
   begin
      return c_kill (pid, sig);
   end kill;

   --------------------
   -- Set_Time_Slice --
   --------------------

   function Set_Time_Slice (ticks : int) return int is
      function kernelTimeSlice (ticks : int) return int;
      pragma Import (C, kernelTimeSlice, "kernelTimeSlice");
   begin
      return kernelTimeSlice (ticks);
   end Set_Time_Slice;

   -------------
   -- sigwait --
   -------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int
   is
      Result : int;

      function sigwaitinfo
        (set : access sigset_t; sigvalue : System.Address) return int;
      pragma Import (C, sigwaitinfo, "sigwaitinfo");

   begin
      Result := sigwaitinfo (set, System.Null_Address);

      if Result /= -1 then
         sig.all := Signal (Result);
         return 0;
      else
         sig.all := 0;
         return errno;
      end if;
   end sigwait;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10#1#E9;
   end To_Duration;

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

      return timespec'(ts_sec  => S,
                       ts_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------------------
   -- To_VxWorks_Priority --
   -------------------------

   function To_VxWorks_Priority (Priority : in int) return int is
   begin
      return Low_Priority - Priority;
   end To_VxWorks_Priority;

   --------------------
   -- To_Clock_Ticks --
   --------------------

   --  ??? - For now, we'll always get the system clock rate
   --  since it is allowed to be changed during run-time in
   --  VxWorks. A better method would be to provide an operation
   --  to set it that so we can always know its value.
   --
   --  Another thing we should probably allow for is a resultant
   --  tick count greater than int'Last. This should probably
   --  be a procedure with two output parameters, one in the
   --  range 0 .. int'Last, and another representing the overflow
   --  count.

   function To_Clock_Ticks (D : Duration) return int is
      Ticks          : Long_Long_Integer;
      Rate_Duration  : Duration;
      Ticks_Duration : Duration;

   begin
      if D < 0.0 then
         return -1;
      end if;

      --  Ensure that the duration can be converted to ticks
      --  at the current clock tick rate without overflowing.

      Rate_Duration := Duration (sysClkRateGet);

      if D > (Duration'Last / Rate_Duration) then
         Ticks := Long_Long_Integer (int'Last);
      else
         Ticks_Duration := D * Rate_Duration;
         Ticks := Long_Long_Integer (Ticks_Duration);

         if Ticks_Duration > Duration (Ticks) then
            Ticks := Ticks + 1;
         end if;

         if Ticks > Long_Long_Integer (int'Last) then
            Ticks := Long_Long_Integer (int'Last);
         end if;
      end if;

      return int (Ticks);
   end To_Clock_Ticks;

   ----------------
   -- VX_FP_TASK --
   ----------------

   function VX_FP_TASK return int is
   begin
      return 16#0008#;
   end VX_FP_TASK;

end System.OS_Interface;
