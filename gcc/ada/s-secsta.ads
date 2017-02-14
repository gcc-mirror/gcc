------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with System.Storage_Elements;

package System.Secondary_Stack is

   package SSE renames System.Storage_Elements;

   Default_Secondary_Stack_Size : Natural := 10 * 1024;
   --  Default size of a secondary stack. May be modified by binder -D switch
   --  which causes the binder to generate an appropriate assignment in the
   --  binder generated file.

   function Minimum_Secondary_Stack_Size return Natural;
   --  The minimum size of the secondary stack so that the internal
   --  requirements of the stack are met.

   procedure SS_Init
     (Stk  : in out Address;
      Size : Natural := Default_Secondary_Stack_Size);
   --  Initialize the secondary stack with a main stack of the given Size.
   --
   --  If System.Parameters.Sec_Stack_Percentage equals Dynamic, Stk is really
   --  an OUT parameter that will be allocated on the heap. Then all further
   --  allocations which do not overflow the main stack will not generate
   --  dynamic (de)allocation calls. If the main Stack overflows, a new
   --  chuck of at least the same size will be allocated and linked to the
   --  previous chunk.
   --
   --  Otherwise (Sec_Stack_Percentage between 0 and 100), Stk is an IN
   --  parameter that is already pointing to a Stack_Id. The secondary stack
   --  in this case is fixed, and any attempt to allocate more than the initial
   --  size will result in a Storage_Error being raised.
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
   --  ??? missing comment here

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
