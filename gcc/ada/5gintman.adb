------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--            Copyright (C) 1997-1998, Florida State University             --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  This is an Irix (old pthread library) version of this package.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.
--  Be on the lookout for special signals that
--  may be used by the thread library.

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use System.OS_Interface;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;

   Exception_Interrupts : constant Interrupt_List :=
     (SIGILL,
      SIGABRT,
      SIGFPE,
      SIGSEGV,
      SIGBUS);

   Reserved_Interrupts : constant Interrupt_List :=
     (0,
      SIGTRAP,
      SIGKILL,
      SIGSYS,
      SIGALRM,
      SIGSTOP,
      SIGPTINTR,
      SIGPTRESCHED);

   Abort_Signal : constant := 48;
   --
   --  Serious MOJO:  The SGI pthreads library only supports the
   --                 unnamed signal number 48 for pthread_kill!
   --

   ----------------------
   -- Notify_Exception --
   ----------------------

   --  This function identifies the Ada exception to be raised using the
   --  information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code has to
   --  be provided for different target.
   --  On SGI, the signal handling is done is a-init.c, even when tasking is
   --  involved.

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Nothing needs to be done on this platform.

   procedure Initialize_Interrupts is
   begin
      null;
   end Initialize_Interrupts;

begin
   Abort_Task_Interrupt := Abort_Signal;

   for I in Reserved_Interrupts'Range loop
      Keep_Unmasked (Reserved_Interrupts (I)) := True;
      Reserve (Reserved_Interrupts (I)) := True;
   end loop;

   for I in Exception_Interrupts'Range loop
      Keep_Unmasked (Exception_Interrupts (I)) := True;
      Reserve (Reserved_Interrupts (I)) := True;
   end loop;

end System.Interrupt_Management;
