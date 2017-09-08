------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . C T R L _ C                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2002-2017, AdaCore                    --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package may be used to intercept the interruption of a running
--  program by the operator typing Control-C, without having to use an Ada
--  interrupt handler protected object.

--  This package is currently implemented under Windows and Unix platforms

--  Note concerning Unix systems:

--  The behavior of this package when using tasking depends on the interaction
--  between sigaction() and the thread library.

package GNAT.Ctrl_C is

   type Handler_Type is access procedure;
   --  Any parameterless library level procedure can be used as a handler.
   --  Handler_Type should not propagate exceptions.

   procedure Install_Handler (Handler : Handler_Type);
   --  Set up Handler to be called if the operator hits Ctrl-C, instead of the
   --  standard Control-C handler.

   procedure Uninstall_Handler;
   --  Reinstall the standard Control-C handler.
   --  If Install_Handler has never been called, this procedure has no effect.

private
   pragma Import (C, Uninstall_Handler, "__gnat_uninstall_int_handler");
end GNAT.Ctrl_C;
