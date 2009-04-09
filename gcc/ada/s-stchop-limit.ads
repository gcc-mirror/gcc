------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1999-2009, Free Software Foundation, Inc.         --
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
