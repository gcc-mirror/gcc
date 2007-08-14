------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 1991-2007, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the FreeBSD THREADS version of this package

with Interfaces.C; use Interfaces.C;

package body System.OS_Interface is

   -----------
   -- Errno --
   -----------

   function Errno return int is
      type int_ptr is access all int;

      function internal_errno return int_ptr;
      pragma Import (C, internal_errno, "__error");

   begin
      return (internal_errno.all);
   end Errno;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Unreferenced (thread);
   begin
      return (0);
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
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10#1#E9;
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

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(ts_sec => S,
                       ts_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

end System.OS_Interface;
