------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   I N T E R F A C E S . V T H R E A D S                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--       Copyright (C) 2002-2003, Free Software Foundation, Inc.            --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  Implement APEX process registration for AE653. The routines exported
--  by this package are only called from the APEX CREATE and START routines
--  in the AE653 vThreads API. A context clause for this unit must appear in
--  the Ada APEX binding.
--
--  If this package appears in a context clause for an application that will
--  be run in a non-AE653 version of VxWorks, or in a non-vThreads AE653
--  partition, link or load errors for the symbols procCreateHookAdd and
--  procStartHookAdd will occur, unless these routines are defined
--  in the application. This is used when simulating AE653 in AE 1.1.

with System.OS_Interface;
with Interfaces.C;

package Interfaces.Vthreads is

   function Setup_Thread return System.Address;
   --  Register an existing vxWorks task. This routine is used
   --  under AE 1.1 when simulating AE 653.

   function Install_Signal_Handlers return Interfaces.C.int;
   pragma Export (C, Install_Signal_Handlers,
                  "__gnat_install_signal_handlers");
   --  Map the synchronous signals SIGSEGV, SIGFPE, SIGILL and
   --  SIGBUS to Ada exceptions for the calling ARINC process.
   --  This routine should be called as early as possible in
   --  each ARINC process body.
   --  C declaration:
   --  extern int __gnat_install_signal_handlers ();
   --  This call is unnecessary on AE 1.1.

private
   package OSI renames System.OS_Interface;

   function Register_Foreign (T : OSI.Thread_Id) return OSI.STATUS;
   --  Create runtime structures necessary for Ada language support for
   --  an ARINC process. Called from APEX CREATE routine.

   function Reset_Foreign (T : OSI.Thread_Id) return OSI.STATUS;
   --  Reset runtime structures upon an AE653 process restart. Called from
   --  APEX START routine.

   --  When defining the following routines for export in an AE 1.1
   --  simulation of AE653, Interfaces.C.int may be used for the
   --  parameters of FUNCPTR.
   type FUNCPTR is access function (T : OSI.Thread_Id) return OSI.STATUS;

   --------------------------------
   -- Imported vThreads Routines --
   --------------------------------

   procedure procCreateHookAdd (createHookFunction : FUNCPTR);
   pragma Import (C, procCreateHookAdd, "procCreateHookAdd");
   --  Registers task registration routine for AE653

   procedure procStartHookAdd (StartHookFunction : FUNCPTR);
   pragma Import (C, procStartHookAdd, "procStartHookAdd");
   --  Registers task restart routine for AE653

end Interfaces.Vthreads;
