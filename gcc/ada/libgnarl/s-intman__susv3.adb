------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This is the SuSV3 threads version of this package

--  Make a careful study of all signals available under the OS, to see which
--  need to be reserved, kept always unmasked, or kept always unmasked. Be on
--  the lookout for special signals that may be used by the thread library.

--  Since this is a multi target file, the signal <-> exception mapping
--  is simple minded. If you need a more precise and target specific
--  signal handling, create a new s-intman.adb that will fit your needs.

--  This file assumes that:

--    SIGINT exists and will be kept unmasked unless the pragma
--     Unreserve_All_Interrupts is specified anywhere in the application.

--    System.OS_Interface contains the following:
--      SIGADAABORT: the signal that will be used to abort tasks.
--      Unmasked: the OS specific set of signals that should be unmasked in
--                all the threads. SIGADAABORT is unmasked by
--                default
--      Reserved: the OS specific set of signals that are reserved.

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.OS_Interface;

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function State (Int : Interrupt_ID) return Character;
   pragma Import (C, State, "__gnat_get_interrupt_state");
   --  Get interrupt state. Defined in init.c The input argument is the
   --  interrupt number, and the result is one of the following:

   User    : constant Character := 'u';
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

   procedure Initialize is
   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Need to call pthread_init very early because it is doing signal
      --  initializations.

      pthread_init;

      Abort_Task_Interrupt := SIGADAABORT;

      pragma Assert (Keep_Unmasked = (Interrupt_ID'Range => False));
      pragma Assert (Reserve = (Interrupt_ID'Range => False));

      --  Process state of exception signals

      for J in Exception_Signals'Range loop
         declare
            Sig : constant Signal := Exception_Signals (J);
            Id : constant Interrupt_ID := Interrupt_ID (Sig);
         begin
            if State (Id) /= User then
               Keep_Unmasked (Id) := True;
               Reserve (Id) := True;
            end if;
         end;
      end loop;

      if State (Abort_Task_Interrupt) /= User then
         Keep_Unmasked (Abort_Task_Interrupt) := True;
         Reserve (Abort_Task_Interrupt) := True;
      end if;

      --  Set SIGINT to unmasked state as long as it is not in "User" state.
      --  Check for Unreserve_All_Interrupts last.

      if State (SIGINT) /= User then
         Keep_Unmasked (SIGINT) := True;
         Reserve (SIGINT) := True;
      end if;

      --  Check all signals for state that requires keeping them unmasked and
      --  reserved.

      for J in Interrupt_ID'Range loop
         if State (J) = Default or else State (J) = Runtime then
            Keep_Unmasked (J) := True;
            Reserve (J) := True;
         end if;
      end loop;

      --  Add the set of signals that must always be unmasked for this target

      for J in Unmasked'Range loop
         Keep_Unmasked (Interrupt_ID (Unmasked (J))) := True;
         Reserve (Interrupt_ID (Unmasked (J))) := True;
      end loop;

      --  Add target-specific reserved signals

      for J in Reserved'Range loop
         Reserve (Interrupt_ID (Reserved (J))) := True;
      end loop;

      --  Process pragma Unreserve_All_Interrupts. This overrides any settings
      --  due to pragma Interrupt_State:

      if Unreserve_All_Interrupts /= 0 then
         Keep_Unmasked (SIGINT) := False;
         Reserve (SIGINT) := False;
      end if;

      --  We do not really have Signal 0. We just use this value to identify
      --  non-existent signals (see s-intnam.ads). Therefore, Signal should not
      --  be used in all signal related operations hence mark it as reserved.

      Reserve (0) := True;
   end Initialize;

end System.Interrupt_Management;
