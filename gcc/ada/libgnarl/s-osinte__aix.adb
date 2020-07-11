------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1997-2020, Free Software Foundation, Inc.         --
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

--  This is a AIX (Native) version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

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
      Dispatching_Policy : Character;
      pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

      Time_Slice_Val : Integer;
      pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   begin
      --  For the case SCHED_OTHER the only valid priority across all supported
      --  versions of AIX is 1 (note that the scheduling policy can be set
      --  with the pragma Task_Dispatching_Policy or setting the time slice
      --  value). Otherwise, for SCHED_RR and SCHED_FIFO, the system defines
      --  priorities in the range 1 .. 127. This means that we must map
      --  System.Any_Priority in the range 0 .. 126 to 1 .. 127.

      if Dispatching_Policy = ' ' and then Time_Slice_Val < 0 then
         return 1;
      else
         return Interfaces.C.int (Prio) + 1;
      end if;
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

      --  If F is negative due to a round-up, adjust for positive F value

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(tv_sec => S,
                       tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -----------------
   -- sched_yield --
   -----------------

   --  AIX Thread does not have sched_yield;

   function sched_yield return int is
      procedure pthread_yield;
      pragma Import (C, pthread_yield, "sched_yield");
   begin
      pthread_yield;
      return 0;
   end sched_yield;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Warnings (Off, thread);
   begin
      return Null_Address;
   end Get_Stack_Base;

   --------------------------
   -- PTHREAD_PRIO_INHERIT --
   --------------------------

   AIX_Version : Integer := 0;
   --  AIX version in the form xy for AIX version x.y (0 means not set)

   SYS_NMLN : constant := 32;
   --  AIX system constant used to define utsname, see sys/utsname.h

   subtype String_NMLN is String (1 .. SYS_NMLN);

   type utsname is record
      sysname    : String_NMLN;
      nodename   : String_NMLN;
      release    : String_NMLN;
      version    : String_NMLN;
      machine    : String_NMLN;
      procserial : String_NMLN;
   end record;
   pragma Convention (C, utsname);

   procedure uname (name : out utsname);
   pragma Import (C, uname);

   function PTHREAD_PRIO_INHERIT return int is
      name : utsname;

      function Val (C : Character) return Integer;
      --  Transform a numeric character ('0' .. '9') to an integer

      ---------
      -- Val --
      ---------

      function Val (C : Character) return Integer is
      begin
         return Character'Pos (C) - Character'Pos ('0');
      end Val;

   --  Start of processing for PTHREAD_PRIO_INHERIT

   begin
      if AIX_Version = 0 then

         --  Set AIX_Version

         uname (name);
         AIX_Version := Val (name.version (1)) * 10 + Val (name.release (1));
      end if;

      if AIX_Version < 53 then

         --  Under AIX < 5.3, PTHREAD_PRIO_INHERIT is defined as 0 in pthread.h

         return 0;

      else
         --  Under AIX >= 5.3, PTHREAD_PRIO_INHERIT is defined as 3

         return 3;
      end if;
   end PTHREAD_PRIO_INHERIT;

end System.OS_Interface;
