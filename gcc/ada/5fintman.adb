------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--            Copyright (C) 1991-2001, Florida State University             --
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

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Nothing needs to be done on this platform.

   procedure Initialize_Interrupts is
   begin
      null;
   end Initialize_Interrupts;

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   use type Interfaces.C.int;

begin
   Abort_Task_Interrupt := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   for I in Exception_Interrupts'Range loop
      Keep_Unmasked (Exception_Interrupts (I)) := True;
   end loop;

   --  By keeping SIGINT unmasked, allow the user to do a Ctrl-C, but in the
   --  same time, disable the ability of handling this signal via
   --  Ada.Interrupts.
   --  The pragma Unreserve_All_Interrupts let the user the ability to
   --  change this behavior.

   if Unreserve_All_Interrupts = 0 then
      Keep_Unmasked (SIGINT) := True;
   end if;

   Keep_Unmasked (Abort_Task_Interrupt) := True;

   Reserve := Keep_Unmasked or Keep_Masked;
   Reserve (0) := True;
end System.Interrupt_Management;
