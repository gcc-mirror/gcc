------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . S T A C K _ C H E C K I N G                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1999-2001 Free Software Foundation, Inc.         --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a system-independent implementation of stack
--  checking using comparison with stack base and limit.

with System.Storage_Elements;

pragma Polling (Off);
--  Turn off polling, we do not want polling to take place during stack
--  checking operations. It causes infinite loops and other problems.

package System.Stack_Checking is
   type Stack_Info is record
      Limit : System.Address := System.Null_Address;
      Base  : System.Address := System.Null_Address;
      Size  : System.Storage_Elements.Storage_Offset := 0;
   end record;
   --  This record may be part of a larger data structure like the
   --  task control block in the tasking case.
   --  This specific layout has the advantage of being compatible with the
   --  Intel x86 BOUNDS instruction.

   type Stack_Access is access all Stack_Info;
   --  Unique local storage associated with a specific task. This storage is
   --  used for the stack base and limit, and is returned by Checked_Self.
   --  Only self may write this information, it may be read by any task.
   --  At no time the address range Limit .. Base (or Base .. Limit for
   --  upgrowing stack) may contain any address that is part of another stack.
   --  The Stack_Access may be part of a larger data structure.

   Multi_Processor        : constant Boolean := False; --  Not supported yet

   ----------------------
   -- Client Interface --
   ----------------------

   procedure Set_Stack_Size
     (Stack_Size : System.Storage_Elements.Storage_Offset);
   --  Specify the stack size for the current task.

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

   Null_Stack_Info  : aliased Stack_Info :=
     (Limit => System.Null_Address,
      Base  => System.Null_Address,
      Size  => 0);
   --  Use explicit assignment to avoid elaboration code (call to _init_proc).

   Null_Stack       : constant Stack_Access := Null_Stack_Info'Access;
   --  Stack_Access value that will return a Stack_Base and Stack_Limit
   --  that fail any stack check.

   Cache            : aliased Stack_Access := Null_Stack;

   pragma Export (C, Cache, "_gnat_stack_cache");
   pragma Export (C, Stack_Check, "_gnat_stack_check");

end System.Stack_Checking;
