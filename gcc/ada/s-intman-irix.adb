------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
---                     Copyright (C) 1995-2005, AdaCore                     --
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

--  This is a SGI Pthread version of this package.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.
--  Be on the lookout for special signals that
--  may be used by the thread library.

with Interfaces.C;
--  used for int

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use System.OS_Interface;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List :=
     (SIGTSTP, SIGILL, SIGTRAP, SIGEMT, SIGFPE, SIGBUS, SIGSTOP, SIGKILL,
      SIGSEGV, SIGSYS, SIGXCPU, SIGXFSZ, SIGPROF, SIGPTINTR, SIGPTRESCHED,
      SIGABRT, SIGPIPE);

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   use type Interfaces.C.int;

begin
   declare
      function State (Int : Interrupt_ID) return Character;
      pragma Import (C, State, "__gnat_get_interrupt_state");

      --  Get interrupt state.  Defined in a-init.c
      --  The input argument is the interrupt number,
      --  and the result is one of the following:

      User    : constant Character := 'u';
      Runtime : constant Character := 'r';
      Default : constant Character := 's';
      --    'n'   this interrupt not set by any Interrupt_State pragma
      --    'u'   Interrupt_State pragma set state to User
      --    'r'   Interrupt_State pragma set state to Runtime
      --    's'   Interrupt_State pragma set state to System (use "default"
      --           system handler)

   begin
      Abort_Task_Interrupt := SIGABRT;

      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      pragma Assert (Keep_Unmasked = (Interrupt_ID'Range => False));
      pragma Assert (Reserve = (Interrupt_ID'Range => False));

      --  Process state of exception signals

      for J in Exception_Interrupts'Range loop
         if State (Exception_Interrupts (J)) /= User then
            Keep_Unmasked (Exception_Interrupts (J)) := True;
            Reserve (Exception_Interrupts (J)) := True;
         end if;
      end loop;

      if State (Abort_Task_Interrupt) /= User then
         Keep_Unmasked (Abort_Task_Interrupt) := True;
         Reserve (Abort_Task_Interrupt) := True;
      end if;

      --  Set SIGINT to unmasked state as long as it's
      --  not in "User" state.  Check for Unreserve_All_Interrupts last

      if State (SIGINT) /= User then
         Keep_Unmasked (SIGINT) := True;
      end if;

      --  Check all signals for state that requires keeping them
      --  unmasked and reserved

      for J in Interrupt_ID'Range loop
         if State (J) = Default or else State (J) = Runtime then
            Keep_Unmasked (J) := True;
            Reserve (J) := True;
         end if;
      end loop;

      --  Process pragma Unreserve_All_Interrupts. This overrides any
      --  settings due to pragma Interrupt_State:

      if Unreserve_All_Interrupts /= 0 then
         Keep_Unmasked (SIGINT) := False;
         Reserve (SIGINT) := False;
      end if;

      --  We do not have Signal 0 in reality. We just use this value
      --  to identify not existing signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.

      Reserve (0) := True;
   end;
end System.Interrupt_Management;
