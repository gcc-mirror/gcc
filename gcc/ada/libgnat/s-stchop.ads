------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1999-2017, Free Software Foundation, Inc.         --
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

--  This package provides a implementation of stack checking operations using
--  comparison with stack base and limit.

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the binder
--  does not handle references to this package.

pragma Polling (Off);
--  Turn off polling, we do not want polling to take place during stack
--  checking operations. It causes infinite loops and other problems.

with System.Storage_Elements;

package System.Stack_Checking.Operations is
   pragma Preelaborate;

   procedure Update_Stack_Cache (Stack : Stack_Access);
   --  Set the stack cache for the current task. Note that this is only for
   --  optimization purposes, nothing can be assumed about the contents of the
   --  cache at any time, see Set_Stack_Info.
   --
   --  The stack cache should contain the bounds of the current task.  But
   --  because the RTS is not aware of task switches, the stack cache may be
   --  incorrect.  So when the stack pointer is not within the bounds of the
   --  stack cache, Stack_Check first update the cache (which is a costly
   --  operation hence the need of a cache).

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access);
   --  Invalidate cache entries for the task T that owns Any_Stack. This causes
   --  the Set_Stack_Info function to be called during the next stack check
   --  done by T. This can be used to interrupt task T asynchronously.
   --  Stack_Check should be called in loops for this to work reliably.

   function Stack_Check (Stack_Address : System.Address) return Stack_Access;
   --  This version of Stack_Check should not be inlined

   procedure Notify_Stack_Attributes
     (Initial_SP : System.Address;
      Size       : System.Storage_Elements.Storage_Offset);
   --  Register Initial_SP as the initial stack pointer value for the current
   --  task when it starts and Size as the associated stack area size. This
   --  should be called once, after the soft-links have been initialized and
   --  prior to the first "Stack_Check" call.

private
   Cache : aliased Stack_Access := Null_Stack;

   pragma Export (C, Cache, "_gnat_stack_cache");
   pragma Export (C, Stack_Check, "_gnat_stack_check");

end System.Stack_Checking.Operations;
