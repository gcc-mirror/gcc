------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Version for use in HI-E mode

with System.Storage_Elements;

package System.Secondary_Stack is

   package SSE renames System.Storage_Elements;

   Default_Secondary_Stack_Size : Natural := 10 * 1024;
   --  Default size of a secondary stack. May be modified by binder -D switch

   procedure SS_Init
     (Stk  : System.Address;
      Size : Natural := Default_Secondary_Stack_Size);
   --  Initialize the secondary stack with a main stack of the given Size.
   --
   --  Stk is an "in" parameter that is already pointing to a memory area of
   --  size Size.
   --
   --  The secondary stack is fixed, and any attempt to allocate more than the
   --  initial size will result in a Storage_Error being raised.

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count);
   --  Allocate enough space for a 'Storage_Size' bytes object with Maximum
   --  alignment. The address of the allocated space is returned in 'Address'

   procedure SS_Free (Stk : in out System.Address);
   --  Release the memory allocated for the Secondary Stack. That is to say,
   --  all the allocated chuncks.
   --  Upon return, Stk will be set to System.Null_Address

   type Mark_Id is private;
   --  Type used to mark the stack.

   function SS_Mark return Mark_Id;
   --  Return the Mark corresponding to the current state of the stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the stack corresponding to the mark M. If an
   --  additional chunk have been allocated, it will never be freed during a

private

   SS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler

   type Mark_Id is new SSE.Integer_Address;

end System.Secondary_Stack;
