------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2020, Free Software Foundation, Inc.         --
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

--  This version is for VxWorks targets

with System.OS_Interface;
--  Since the thread library is part of the VxWorks kernel, using OS_Interface
--  is not a problem here, as long as we only use System.OS_Interface as a
--  set of C imported routines: using Ada routines from this package would
--  create a dependency on libgnarl in libgnat, which is not desirable.

with System.OS_Constants;
with Interfaces.C;

package body System.OS_Primitives is

   use System.OS_Interface;
   use type Interfaces.C.int;

   package OSC renames System.OS_Constants;

   ------------------------
   -- Internal functions --
   ------------------------

   function To_Clock_Ticks (D : Duration) return int;
   --  Convert a duration value (in seconds) into clock ticks.
   --  Note that this routine is duplicated from System.OS_Interface since
   --  as explained above, we do not want to depend on libgnarl

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

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TS     : aliased timespec;
      Result : int;
   begin
      Result := clock_gettime (OSC.CLOCK_RT_Ada, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10#1#E9;
   end Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Base_Time  : constant Duration := Clock;
      Check_Time : Duration := Base_Time;
      Ticks      : int;

      Result     : int;
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
            Ticks := To_Clock_Ticks (Rel_Time);

            if Mode = Relative and then Ticks < int'Last then
               --  The first tick will delay anytime between 0 and
               --  1 / sysClkRateGet seconds, so we need to add one to
               --  be on the safe side.

               Ticks := Ticks + 1;
            end if;

            Result := taskDelay (Ticks);
            Check_Time := Clock;

            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

end System.OS_Primitives;
