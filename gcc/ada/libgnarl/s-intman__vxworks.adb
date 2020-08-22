------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This is the VxWorks version of this package

--  It is simpler than other versions because the Ada interrupt handling
--  mechanisms are used for hardware interrupts rather than signals.

package body System.Interrupt_Management is

   use System.OS_Interface;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function State (Int : Interrupt_ID) return Character;
   pragma Import (C, State, "__gnat_get_interrupt_state");
   --  Get interrupt state. Defined in init.c The input argument is the
   --  hardware interrupt number, and the result is one of the following:

   Runtime : constant Character := 'r';
   Default : constant Character := 's';
   --    'n'   this interrupt not set by any Interrupt_State pragma
   --    'u'   Interrupt_State pragma set state to User
   --    'r'   Interrupt_State pragma set state to Runtime
   --    's'   Interrupt_State pragma set state to System (use "default"
   --           system handler)

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;
   --  Set to True once Initialize is called, further calls have no effect

   procedure Initialize is

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      Abort_Task_Interrupt := SIGABRT;

      --  Initialize hardware interrupt handling

      pragma Assert (Reserve = (Interrupt_ID'Range => False));

      --  Check all interrupts for state that requires keeping them reserved

      for J in Interrupt_ID'Range loop
         if State (J) = Default or else State (J) = Runtime then
            Reserve (J) := True;
         end if;
      end loop;

   end Initialize;

end System.Interrupt_Management;
