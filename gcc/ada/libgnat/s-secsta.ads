------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with System.Parameters;
with System.Storage_Elements;

package System.Secondary_Stack is
   pragma Preelaborate;

   package SP renames System.Parameters;
   package SSE renames System.Storage_Elements;

   type SS_Stack (Size : SP.Size_Type) is private;
   --  Data structure for secondary stacks

   type SS_Stack_Ptr is access all SS_Stack;
   --  Pointer to secondary stack objects

   procedure SS_Init
     (Stack : in out SS_Stack_Ptr;
      Size  : SP.Size_Type := SP.Unspecified_Size);
   --  Initialize the secondary stack Stack. If Stack is null allocate a stack
   --  from the heap or from the default-sized secondary stack pool if the
   --  pool exists and the requested size is Unspecified_Size.

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count);
   --  Allocate enough space for a 'Storage_Size' bytes object with Maximum
   --  alignment. The address of the allocated space is returned in Addr.

   procedure SS_Free (Stack : in out SS_Stack_Ptr);
   --  Release the memory allocated for the Stack. If the stack was statically
   --  allocated the SS_Stack record is not freed.

   type Mark_Id is private;
   --  Type used to mark the stack for mark/release processing

   function SS_Mark return Mark_Id;
   --  Return the Mark corresponding to the current state of the stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the stack corresponding to the mark M

   function SS_Get_Max return Long_Long_Integer;
   --  Return the high water mark of the secondary stack for the current
   --  secondary stack in bytes.

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

   -------------------------------------
   -- Secondary Stack Data Structures --
   -------------------------------------

   --  This package provides fixed and dynamically sized secondary stack
   --  implementations centered around a common data structure SS_Stack. This
   --  record contains an initial secondary stack allocation of the requested
   --  size, and markers for the current top of the stack and the high-water
   --  mark of the stack. A SS_Stack can be either pre-allocated outside the
   --  package or SS_Init can allocate a stack from the heap or the
   --  default-sized secondary stack from a pool generated by the binder.

   --  For dynamically allocated secondary stacks, the stack can grow via a
   --  linked list of stack chunks allocated from the heap. New chunks are
   --  allocated once the initial static allocation and any existing chunks are
   --  exhausted. The following diagram illustrated the data structures used
   --  for a dynamically allocated secondary stack:
   --
   --                                       +------------------+
   --                                       |       Next       |
   --                                       +------------------+
   --                                       |                  | Last (300)
   --                                       |                  |
   --                                       |                  |
   --                                       |                  |
   --                                       |                  |
   --                                       |                  |
   --                                       |                  | First (201)
   --                                       +------------------+
   --    +-----------------+       +------> |          |       |
   --    |                 | (100) |        +--------- | ------+
   --    |                 |       |                ^  |
   --    |                 |       |                |  |
   --    |                 |       |                |  V
   --    |                 |       |        +------ | ---------+
   --    |                 |       |        |       |          |
   --    |                 |       |        +------------------+
   --    |                 |       |        |                  | Last (200)
   --    |                 |       |        |         C        |
   --    |                 | (1)   |        |         H        |
   --    +-----------------+       |  +---->|         U        |
   --    |  Current_Chunk ---------+  |     |         N        |
   --    +-----------------+          |     |         K        |
   --    |       Top      ------------+     |                  | First (101)
   --    +-----------------+                +------------------+
   --    |       Size      |                |       Prev       |
   --    +-----------------+                +------------------+
   --
   --  The implementation used by the runtime is controlled via the constant
   --  System.Parameter.Sec_Stack_Dynamic. If True, the implementation is
   --  permitted to grow the secondary stack at runtime. The implementation is
   --  designed for the compiler to include only code to support the desired
   --  secondary stack behavior.

   subtype SS_Ptr is SP.Size_Type;
   --  Stack pointer value for the current position within the secondary stack.
   --  Size_Type is used as the base type since the Size discriminate of
   --  SS_Stack forms the bounds of the internal memory array.

   type Memory is array (SS_Ptr range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;
   --  The region of memory that holds the stack itself. Requires maximum
   --  alignment for efficient stack operations.

   --  Chunk_Id

   --  Chunk_Id is a contiguous block of dynamically allocated stack. First
   --  and Last indicate the range of secondary stack addresses present in the
   --  chunk. Chunk_Ptr points to a Chunk_Id block.

   type Chunk_Id (First, Last : SS_Ptr);
   type Chunk_Ptr is access all Chunk_Id;

   type Chunk_Id (First, Last : SS_Ptr) is record
      Prev, Next : Chunk_Ptr;
      Mem        : Memory (First .. Last);
   end record;

   --  Secondary stack data structure

   type SS_Stack (Size : SP.Size_Type) is record
      Top : SS_Ptr;
      --  Index of next available location in the stack. Initialized to 1 and
      --  then incremented on Allocate and decremented on Release.

      Max : SS_Ptr;
      --  Contains the high-water mark of Top. Initialized to 1 and then
      --  may be incremented on Allocate but never decremented. Since
      --  Top = Size + 1 represents a fully used stack, Max - 1 indicates
      --  the size of the stack used in bytes.

      Current_Chunk : Chunk_Ptr;
      --  A link to the chunk containing the highest range of the stack

      Freeable : Boolean;
      --  Indicates if an object of this type can be freed

      Internal_Chunk : aliased Chunk_Id (1, Size);
      --  Initial memory allocation of the secondary stack
   end record;

   type Mark_Id is record
      Sec_Stack : SS_Stack_Ptr;
      Sptr      : SS_Ptr;
   end record;
   --  Contains the pointer to the secondary stack object and the stack pointer
   --  value corresponding to the top of the stack at the time of the mark
   --  call.

   ------------------------------------
   -- Binder Allocated Stack Support --
   ------------------------------------

   --  When the No_Implicit_Heap_Allocations or No_Implicit_Task_Allocations
   --  restrictions are in effect the binder statically generates secondary
   --  stacks for tasks who are using default-sized secondary stack. Assignment
   --  of these stacks to tasks is handled by SS_Init. The following variables
   --  assist SS_Init and are defined here so the runtime does not depend on
   --  the binder.

   Binder_SS_Count : Natural;
   pragma Export (Ada, Binder_SS_Count, "__gnat_binder_ss_count");
   --  The number of default sized secondary stacks allocated by the binder

   Default_SS_Size : SP.Size_Type;
   pragma Export (Ada, Default_SS_Size, "__gnat_default_ss_size");
   --  The default size for secondary stacks. Defined here and not in init.c/
   --  System.Init because these locations are not present on ZFP or
   --  Ravenscar-SFP run-times.

   Default_Sized_SS_Pool : System.Address;
   pragma Export (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");
   --  Address to the secondary stack pool generated by the binder that
   --  contains default sized stacks.

   Num_Of_Assigned_Stacks : Natural := 0;
   --  The number of currently allocated secondary stacks

end System.Secondary_Stack;
