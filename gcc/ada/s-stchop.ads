------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 1999-2005 Free Software Foundation, Inc.        --
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

--  This package provides a implementation of stack checking operations
--  using comparison with stack base and limit.

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the
--  binder does not handle references to this package.

pragma Polling (Off);
--  Turn off polling, we do not want polling to take place during stack
--  checking operations. It causes infinite loops and other problems.

package System.Stack_Checking.Operations is
   procedure Update_Stack_Cache (Stack : Stack_Access);
   --  Set the stack cache for the current task. Note that this is only
   --  for optimization purposes, nothing can be assumed about the
   --  contents of the cache at any time, see Set_Stack_Info.

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access);
   --  Invalidate cache entries for the task T that owns Any_Stack.
   --  This causes the Set_Stack_Info function to be called during
   --  the next stack check done by T. This can be used to interrupt
   --  task T asynchronously.
   --  Stack_Check should be called in loops for this to work reliably.

   function Stack_Check (Stack_Address : System.Address) return Stack_Access;
   --  This version of Stack_Check should not be inlined.

private

   Cache : aliased Stack_Access := Null_Stack;

   pragma Export (C, Cache, "_gnat_stack_cache");
   pragma Export (C, Stack_Check, "_gnat_stack_check");

end System.Stack_Checking.Operations;
