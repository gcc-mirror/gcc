------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--                     Copyright (C) 1995-2018, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for QNX operating systems

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

with Interfaces.C; use Interfaces.C;
package body System.OS_Interface is

   -----------------
   -- sigaltstack --
   -----------------

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int
   is
      pragma Unreferenced (ss, oss);
   begin
      return 0;
   end sigaltstack;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Unreferenced (thread);
   begin
      return Null_Address;
   end Get_Stack_Base;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

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
      return Interfaces.C.int (Prio + 1);
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

end System.OS_Interface;
