------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1998-2007, Free Software Foundation, Inc.          --
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

--  This is the DEC Unix version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C; use Interfaces.C;
with System.Machine_Code; use System.Machine_Code;

package body System.OS_Interface is

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

   ------------------
   -- pthread_self --
   ------------------

   function pthread_self return pthread_t is
      Self : pthread_t;
   begin
      Asm ("call_pal 0x9e" & ASCII.LF & ASCII.HT &
           "bis $31, $0, %0",
           Outputs  => pthread_t'Asm_Output ("=r", Self),
           Clobber  => "$0",
           Volatile => True);
      return Self;
   end pthread_self;

   ----------------------
   -- Hide_Yellow_Zone --
   ----------------------

   procedure Hide_Unhide_Yellow_Zone (Hide : Boolean) is
      type Teb_Ptr is access all pthread_teb_t;
      Teb : Teb_Ptr;
      Res : Interfaces.C.int;
      pragma Unreferenced (Res);

   begin
      --  Get the Thread Environment Block address

      Asm ("call_pal 0x9e" & ASCII.LF & ASCII.HT &
           "bis $31, $0, %0",
           Outputs  => Teb_Ptr'Asm_Output ("=r", Teb),
           Clobber  => "$0",
           Volatile => True);

      --  Stick a guard page right above the Yellow Zone if it exists

      if Teb.all.stack_yellow /= Teb.all.stack_guard then
         if Hide then
            Res := mprotect (Teb.all.stack_yellow, Get_Page_Size, PROT_ON);
         else
            Res := mprotect (Teb.all.stack_yellow, Get_Page_Size, PROT_OFF);
         end if;
      end if;
   end Hide_Unhide_Yellow_Zone;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
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

      return timespec'(tv_sec => S,
                       tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

end System.OS_Interface;
