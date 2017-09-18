------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 S Y S T E M . S T A C K _ C H E C K I N G                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--           Copyright (C) 1999-2017, Free Software Foundation, Inc.        --
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

--  This package provides a system-independent implementation of stack
--  checking using comparison with stack base and limit.

--  This package defines basic types and objects. Operations related to
--  stack checking can be found in package System.Stack_Checking.Operations.

pragma Compiler_Unit_Warning;

with System.Storage_Elements;

package System.Stack_Checking is
   pragma Preelaborate;
   pragma Elaborate_Body;
   --  This unit has a junk null body. The reason is that historically we
   --  used to have a real body, and it causes bootstrapping path problems
   --  to eliminate it, since the old body may still be present in the
   --  compilation environment for a build.

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

   Multi_Processor : constant Boolean := False; --  Not supported yet

private

   Null_Stack_Info  : aliased Stack_Info :=
     (Limit => System.Null_Address,
      Base  => System.Null_Address,
      Size  => 0);
   --  Use explicit assignment to avoid elaboration code (call to init proc)

   Null_Stack : constant Stack_Access := Null_Stack_Info'Access;
   --  Stack_Access value that will return a Stack_Base and Stack_Limit
   --  that fail any stack check.

end System.Stack_Checking;
