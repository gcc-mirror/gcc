------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . C O M M A N D _ L I N E . R E M O V E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

--  This package is intended to be used in conjunction with its parent unit,
--  Ada.Command_Line. It provides facilities for logically removing arguments
--  from the command line, so that subsequent calls to Argument_Count and
--  Argument will reflect the removals.

--  For example, if the original command line has three arguments A B C, so
--  that Argument_Count is initially three, then after removing B, the second
--  argument, Argument_Count will be 2, and Argument (2) will return C.

package Ada.Command_Line.Remove is
   pragma Preelaborate;

   procedure Remove_Argument (Number : Positive);
   --  Removes the argument identified by Number, which must be in the
   --  range 1 .. Argument_Count (i.e. an in range argument number which
   --  reflects removals). If Number is out of range Constraint_Error
   --  will be raised.
   --
   --  Note: the numbering of arguments greater than Number is affected
   --  by the call. If you need a loop through the arguments, removing
   --  some as you go, run the loop in reverse to avoid confusion from
   --  this renumbering:
   --
   --    for J in reverse 1 .. Argument_Count loop
   --      if Should_Remove (Arguments (J)) then
   --        Remove_Argument (J);
   --      end if;
   --    end loop;
   --
   --  Reversing the loop in this manner avoids the confusion.

   procedure Remove_Arguments (From : Positive; To : Natural);
   --  Removes arguments in the given From..To range. From must be in the
   --  range 1 .. Argument_Count and To in the range 0 .. Argument_Count.
   --  Constraint_Error is raised if either argument is out of range. If
   --  To is less than From, then the call has no effect.

   procedure Remove_Argument (Argument : String);
   --  Removes the argument which matches the given string Argument. Has
   --  no effect if no argument matches the string. If more than one
   --  argument matches the string, all are removed.

   procedure Remove_Arguments (Argument_Prefix : String);
   --  Removes all arguments whose prefix matches Argument_Prefix. Has
   --  no effect if no argument matches the string. For example a call
   --  to Remove_Arguments ("--") removes all arguments starting with --.

end Ada.Command_Line.Remove;
