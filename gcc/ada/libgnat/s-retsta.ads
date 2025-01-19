------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . R E T U R N _ S T A C K                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2022-2025, Free Software Foundation, Inc.      --
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

--  This small package provides direct access to the return stack of the code
--  generator for functions returning a by-reference type. This return stack
--  is the portion of the primary stack that has been allocated by callers of
--  the functions and onto which the functions put the result before returning.

with System.Storage_Elements;

package System.Return_Stack is
   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   procedure RS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count);
   pragma Import (Intrinsic, RS_Allocate, "__builtin_return_slot");
   --  Allocate enough space on the return stack of the invoking task to
   --  accommodate a return of size Storage_Size. Return the address of the
   --  first byte of the allocation in Addr.

private
   RS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler.

end System.Return_Stack;
