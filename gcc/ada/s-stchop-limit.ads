------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1999-2007, Free Software Foundation, Inc.         --
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

--  This version of this package is for implementations which use
--  the stack limit approach (the limit of the stack is stored into a per
--  thread variable).

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the binder
--  does not handle references to this package.

pragma Polling (Off);
--  Turn off polling, we do not want polling to take place during stack
--  checking operations. It causes infinite loops and other problems.

package System.Stack_Checking.Operations is
   pragma Preelaborate;

   procedure Initialize_Stack_Limit;
   pragma Export (C, Initialize_Stack_Limit,
                    "__gnat_initialize_stack_limit");
   --  This procedure is called before elaboration to setup the stack limit
   --  for the environment task and to register the hook to be called at
   --  task creation.
end System.Stack_Checking.Operations;
