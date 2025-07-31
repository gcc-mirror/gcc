------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     S Y S T E M . O S _ P R I M I T I V E S. T I M E D _ D E L A Y       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2025, AdaCore                     --
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

--  This is the Posix, Posix2008, and LynxOS version of this procedure.

separate (System.OS_Primitives)
procedure Timed_Delay
  (Time : Duration;
   Mode : Integer)
is
   Request    : aliased timespec;
   Remaind    : aliased timespec;
   Rel_Time   : Duration;
   Abs_Time   : Duration;
   Base_Time  : constant Duration := Clock;
   Check_Time : Duration := Base_Time;
   Time_Chunk : Duration;

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

   --  To keep a sensible Max_Sensible_Delay on a target whose system
   --  maximum is less than sensible, we split the delay into manageable
   --  chunks of time less than or equal to the Max_System_Delay.

   if Rel_Time > 0.0 then
      Time_Chunk := Rel_Time;
      loop
         pragma Warnings (Off, "condition is always *");
         if Max_System_Delay < Max_Sensible_Delay and then
            Time_Chunk > Max_System_Delay
         then
            Time_Chunk := Max_System_Delay;
         end if;
         pragma Warnings (On);

         Request := To_Timespec (Time_Chunk);
         Result := nanosleep (Request'Access, Remaind'Access);

         Check_Time := Clock;

         exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

         Time_Chunk := Abs_Time - Check_Time;
      end loop;
   end if;
end Timed_Delay;
