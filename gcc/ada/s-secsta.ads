------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

with System.Storage_Elements;

package System.Secondary_Stack is

   package SSE renames System.Storage_Elements;

   Default_Secondary_Stack_Size : Natural := 10 * 1024;
   --  Default size of a secondary stack. May be modified by binder -D switch

   procedure SS_Init
     (Stk  : in out Address;
      Size : Natural := Default_Secondary_Stack_Size);
   --  Initialize the secondary stack with a main stack of the given Size.
   --
   --  If System.Parameters.Sec_Stack_Ratio equals Dynamic, Stk is really an
   --  OUT parameter that will be allocated on the heap. Then all further
   --  allocations which do not overflow the main stack will not generate
   --  dynamic (de)allocation calls. If the main Stack overflows, a new
   --  chuck of at least the same size will be allocated and linked to the
   --  previous chunk.
   --
   --  Otherwise (Sec_Stack_Ratio between 0 and 100), Stk is an IN parameter
   --  that is already pointing to a Stack_Id. The secondary stack in this case
   --  is fixed, and any attempt to allocated more than the initial size will
   --  result in a Storage_Error being raised.
   --
   --  Note: the reason that Stk is passed is that SS_Init is called before
   --  the proper interface is established to obtain the address of the
   --  stack using System.Soft_Links.Get_Sec_Stack_Addr.

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count);
   --  Allocate enough space for a 'Storage_Size' bytes object with Maximum
   --  alignment. The address of the allocated space is returned in Addr.

   procedure SS_Free (Stk : in out Address);
   --  Release the memory allocated for the Secondary Stack. That is
   --  to say, all the allocated chunks. Upon return, Stk will be set
   --  to System.Null_Address.

   type Mark_Id is private;
   --  Type used to mark the stack for mark/release processing

   function SS_Mark return Mark_Id;
   --  Return the Mark corresponding to the current state of the stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the stack corresponding to the mark M. If an
   --  additional chunk have been allocated, it will never be freed during a

   function SS_Get_Max return Long_Long_Integer;
   --  Return maximum used space in storage units for the current secondary
   --  stack. For a dynamically allocated secondary stack, the returned
   --  result is always -1. For a statically allocated secondary stack,
   --  the returned value shows the largest amount of space allocated so
   --  far during execution of the program to the current secondary stack,
   --  i.e. the secondary stack for the current task.

   generic
      with procedure Put_Line (S : String);
   procedure SS_Info;
   --  Debugging procedure used to print out secondary Stack allocation
   --  information. This procedure is generic in order to avoid a direct
   --  dependance on a particular IO package.

private
   SS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler

   type SS_Ptr is new SSE.Integer_Address;
   --  Stack pointer value for secondary stack

   type Mark_Id is record
      Sstk : System.Address;
      Sptr : SS_Ptr;
   end record;
   --  A mark value contains the address of the secondary stack structure,
   --  as returned by System.Soft_Links.Get_Sec_Stack_Addr, and a stack
   --  pointer value corresponding to the point of the mark call.

end System.Secondary_Stack;
