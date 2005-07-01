------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . C T R L _ C                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2002 Ada Core Technologies, Inc.             --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package may be used to intercept the interruption of a running
--  program by the operator typing Control-C, without having to use an Ada
--  interrupt handler protected object.
--
--  This package is currently implemented under Windows and Unix platforms.
--
--  Note concerning Unix systems:

--  The behavior of this package when using tasking depends on the interaction
--  between sigaction() and the thread library.

--  On most implementations, the interaction will be no different whether
--  tasking is involved or not. An exception is GNU/Linux systems where
--  each task/thread is considered as a separate process by the kernel,
--  meaning in particular that a Ctrl-C from the keyboard will be sent to
--  all tasks instead of only one, resulting in multiple calls to the handler.

package GNAT.Ctrl_C is

   type Handler_Type is access procedure;
   --  Any parameterless library level procedure can be used as a handler.
   --  Handler_Type should not propagate exceptions.

   procedure Install_Handler (Handler : Handler_Type);
   --  Set up Handler to be called if the operator hits Ctrl-C.

   procedure Uninstall_Handler;
   --  Reinstall the standard Control-C handler.
   --  If Install_Handler has never been called, this procedure has no effect.

private
   pragma Import (C, Install_Handler, "__gnat_install_int_handler");
   pragma Import (C, Uninstall_Handler, "__gnat_uninstall_int_handler");
end GNAT.Ctrl_C;
