------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with System.Parameters;
with System.Storage_Elements;

package System.Secondary_Stack is
   pragma Preelaborate;

   package SP  renames System.Parameters;
   package SSE renames System.Storage_Elements;

   use type SP.Size_Type;

   type SS_Stack (Default_Chunk_Size : SP.Size_Type) is private;
   --  An abstraction for a heap structure maintained in a stack-like fashion.
   --  The structure is comprised of chunks which accommodate allocations of
   --  varying sizes. See the private part of the package for further details.
   --  Default_Chunk_Size indicates the size of the static chunk, and provides
   --  a minimum size for all dynamic chunks.

   type SS_Stack_Ptr is access all SS_Stack;
   --  A reference to a secondary stack

   type Mark_Id is private;
   --  An abstraction for tracking the state of the secondary stack

   procedure SS_Init
     (Stack : in out SS_Stack_Ptr;
      Size  : SP.Size_Type := SP.Unspecified_Size);
   --  Initialize or reuse a secondary stack denoted by reference Stack. If
   --  Stack is null, create a new stack of size Size in the following manner:
   --
   --    * If Size denotes Unspecified_Size, allocate the stack from the binder
   --      generated pool as long as the pool has not been exhausted.
   --
   --    * Otherwise allocate the stack from the heap.
   --
   --  If Stack is not null, reset the state of the stack. No existing chunks
   --  are freed because they may be reused again.

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count := Standard'Maximum_Alignment);
   --  Allocate enough space on the secondary stack of the invoking task to
   --  accommodate an allocation of size Storage_Size. Return the address of
   --  the first byte of the allocation in Addr, which is a multiple of
   --  Alignment. The routine may carry out one or more of the following
   --  actions:
   --
   --    * Reuse an existing chunk that is big enough to accommodate the
   --      requested Storage_Size.
   --
   --    * Free an existing chunk that is too small to accommodate the
   --      requested Storage_Size.
   --
   --    * Create a new chunk that fits the requested Storage_Size.

   procedure SS_Free (Stack : in out SS_Stack_Ptr);
   --  Free all dynamic chunks of secondary stack Stack. If possible, free the
   --  stack itself.

   function SS_Mark return Mark_Id;
   --  Capture and return the state of the invoking task's secondary stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the invoking task's secondary stack to mark M

   function SS_Get_Max return Long_Long_Integer;
   --  Return the high water mark of the invoking task's secondary stack, in
   --  bytes.

   generic
      with procedure Put_Line (S : String);
   procedure SS_Info;
   --  Debugging procedure for outputting the internals of the invoking task's
   --  secondary stack. This procedure is generic in order to avoid a direct
   --  dependence on a particular IO package. Instantiate with Text_IO.Put_Line
   --  for example.

private
   SS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler.

   ------------------
   -- Introduction --
   ------------------

   --  The secondary stack is a runtime data structure managed in a stack-like
   --  fashion. It is part of the runtime support for functions that return
   --  results of caller-unknown size.
   --
   --  The secondary stack is utilized as follows:
   --
   --    * The compiler pushes the caller-unknown size result on the secondary
   --      stack as part of return statement or build-in-place semantics.
   --
   --    * The caller receives a reference to the result.
   --
   --    * Using the reference, the caller may "offload" the result into its
   --      primary stack, or use it in-place while still on the secondary
   --      stack.
   --
   --    * Once the caller has utilized the result, the compiler reclaims the
   --      memory occupied by the result by popping the secondary stack up to
   --      a safe limit.

   ------------
   -- Design --
   ------------

   --  1) Chunk
   --
   --  The secondary stack is a linked structure which consist of "chunks".
   --  A chunk is both a memory storage and a linked-list node. Addresses of
   --  allocated objects refer to addresses within the memory storage of a
   --  chunk. Chunks come in two variants - static and dynamic.
   --
   --  1.1) Static chunk
   --
   --  The secondary stack has exactly one static chunk that is created on the
   --  primary stack. The static chunk allows secondary-stack usage on targets
   --  where dynamic allocation is not available or desirable. The static chunk
   --  is always the "first" chunk and precedes all dynamic chunks.
   --
   --  1.2) Dynamic chunk
   --
   --  The secondary stack may have zero or more dynamic chunks, created on the
   --  heap. Dynamic chunks allow the secondary stack to grow beyond the limits
   --  of the initial static chunk. They provide a finer-grained management of
   --  the memory by means of reuse and deallocation.
   --
   --  2) Mark
   --
   --  The secondary stack captures its state in a "mark". The mark is used by
   --  the compiler to indicate how far the stack can be safely popped after a
   --  sequence of pushes has taken place.
   --
   --  3) Secondary stack
   --
   --  The secondary stack maintains a singly-linked list of chunks, starting
   --  with the static chunk, along with a stack pointer.
   --
   --  4) Allocation
   --
   --  The process of allocation equates to "pushing" on the secondary stack.
   --  Depending on whether dynamic allocation is allowed or not, there are
   --  two variants of allocation - static and dynamic.
   --
   --  4.1) Static allocation
   --
   --  In this case the secondary stack has only the static chunk to work with.
   --  The requested size is reserved on the static chunk and the stack pointer
   --  is advanced. If the requested size will overflow the static chunk, then
   --  Storage_Error is raised.
   --
   --  4.2) Dynamic allocation
   --
   --  In this case the secondary stack may carry out several actions depending
   --  on how much free memory is available in the chunk indicated by the stack
   --  pointer.
   --
   --    * If the indicated chunk is big enough, allocation is carried out on
   --      it.
   --
   --    * If the indicated chunk is too small, subsequent chunks (if any) are
   --      examined. If a subsequent chunk is big enough, allocation is carried
   --      out on it, otherwise the subsequent chunk is deallocated.
   --
   --    * If none of the chunks following and including the indicated chunk
   --      are big enough, a new chunk is created and the allocation is carried
   --      out on it.
   --
   --  This model of operation has several desirable effects:
   --
   --    * Leftover chunks from prior allocations, followed by at least one pop
   --      are either reused or deallocated. This compacts the memory footprint
   --      of the secondary stack.
   --
   --    * When a new chunk is created, its size is exactly the requested size.
   --      This keeps the memory usage of the secondary stack tight.
   --
   --    * Allocation is in general an expensive operation. Compaction is thus
   --      added to this cost, rather than penalizing mark and pop operations.
   --
   --  5) Marking
   --
   --  The process of marking involves capturing the secondary-stack pointer
   --  in a mark for later restore.
   --
   --  6) Releasing
   --
   --  The process of releasing equates to "popping" the secondary stack. It
   --  moves the stack pointer to a previously captured mark, causing chunks
   --  to become reusable or deallocatable during the allocation process.

   ------------------
   -- Architecture --
   ------------------

   --      Secondary stack
   --
   --      +------------+
   --      | Top.Byte  ------------------------+
   --      | Top.Chunk ------------------+     |
   --      |            |                |     |
   --      |            |                v     |
   --      +------------+   +--------+   +-----|--+   +--------+
   --      | Memory     |   | Memory |   | Memo|y |   | Memory |
   --      | #########  |   | #####  |   | ####|  |   | #####  |
   --      |            |   |        |   |        |   |        |
   --      | Next      ---> | Next  ---> | Next  ---> | Next  ---> x
   --      +------------+   +--------+   +--------+   +--------+
   --
   --       Static chunk     Chunk 2      Chunk 3      Chunk 4

   --------------------------
   -- Memory-related types --
   --------------------------

   subtype Memory_Size_With_Invalid is SP.Size_Type;
   --  Memory storage size which also includes an invalid negative range

   Invalid_Memory_Size : constant Memory_Size_With_Invalid := -1;

   subtype Memory_Size is
     Memory_Size_With_Invalid range 0 .. SP.Size_Type'Last;
   --  The memory storage size of a single chunk or the whole secondary stack.
   --  A non-negative size is considered a "valid" size.

   subtype Memory_Index is Memory_Size;
   --  Index into the memory storage of a single chunk

   type Chunk_Memory is array (Memory_Size range <>) of SSE.Storage_Element;
   for Chunk_Memory'Alignment use Standard'Maximum_Alignment;
   --  The memory storage of a single chunk

   --------------
   -- SS_Chunk --
   --------------

   type SS_Chunk (Size : Memory_Size);
   --  Abstraction for a chunk. Size indicates the memory capacity of the
   --  chunk.

   type SS_Chunk_Ptr is access all SS_Chunk;
   --  Reference to the static or any dynamic chunk

   type SS_Chunk (Size : Memory_Size) is record
      Next : SS_Chunk_Ptr;
      --  Pointer to the next chunk. The direction of the pointer is from the
      --  static chunk to the first dynamic chunk, and so on.

      Size_Up_To_Chunk : Memory_Size;
      --  The size of the secondary stack up to, but excluding the current
      --  chunk. This value aids in calculating the total amount of memory
      --  the stack is consuming, for high-water-mark update purposes.

      Memory : Chunk_Memory (1 .. Size);
      --  The memory storage of the chunk. The 1-indexing facilitates various
      --  size and indexing calculations.
   end record;

   -------------------
   -- Stack_Pointer --
   -------------------

   --  Abstraction for a secondary stack pointer

   type Stack_Pointer is record
      Byte : Memory_Index;
      --  The position of the first free byte within the memory storage of
      --  Chunk.all. Byte - 1 denotes the last occupied byte within Chunk.all.

      Chunk : SS_Chunk_Ptr;
      --  Reference to the chunk that accommodated the most recent allocation.
      --  This could be the static or any dynamic chunk.
   end record;

   --------------
   -- SS_Stack --
   --------------

   type SS_Stack (Default_Chunk_Size : SP.Size_Type) is record
      Freeable : Boolean;
      --  Indicates whether the secondary stack can be freed

      High_Water_Mark : Memory_Size;
      --  The maximum amount of memory in use throughout the lifetime of the
      --  secondary stack.

      Top : Stack_Pointer;
      --  The stack pointer

      Static_Chunk : aliased SS_Chunk (Default_Chunk_Size);
      --  A special chunk with a default size. On targets that do not support
      --  dynamic allocations, this chunk represents the capacity of the whole
      --  secondary stack.
   end record;

   -------------
   -- Mark_Id --
   -------------

   type Mark_Id is record
      Stack : SS_Stack_Ptr;
      --  The secondary stack whose mark was taken

      Top : Stack_Pointer;
      --  The value of Stack.Top at the point in time when the mark was taken
   end record;

   ------------------
   -- Testing Aids --
   ------------------

   --  The following section provides lightweight versions of all abstractions
   --  used to implement a secondary stack. The contents of these versions may
   --  look identical to the original abstractions, however there are several
   --  important implications:
   --
   --    * The versions do not expose pointers.
   --
   --    * The types of the versions are all definite. In addition, there are
   --      no per-object constrained components. As a result, the versions do
   --      not involve the secondary stack or the heap in any way.
   --
   --    * The types of the versions do not contain potentially big components.

   subtype Chunk_Id_With_Invalid is Natural;
   --  Numeric Id of a chunk with value zero

   Invalid_Chunk_Id : constant Chunk_Id_With_Invalid := 0;

   subtype Chunk_Id is
     Chunk_Id_With_Invalid range 1 .. Chunk_Id_With_Invalid'Last;
   --  Numeric Id of a chunk. A positive Id is considered "valid" because a
   --  secondary stack will have at least one chunk (the static chunk).

   subtype Chunk_Count is Natural;
   --  Number of chunks in a secondary stack

   --  Lightweight version of SS_Chunk

   type Chunk_Info is record
      Size : Memory_Size_With_Invalid;
      --  The memory capacity of the chunk

      Size_Up_To_Chunk : Memory_Size_With_Invalid;
      --  The size of the secondary stack up to, but excluding the current
      --  chunk.
   end record;

   Invalid_Chunk : constant Chunk_Info :=
                     (Size             => Invalid_Memory_Size,
                      Size_Up_To_Chunk => Invalid_Memory_Size);

   --  Lightweight version of Stack_Pointer

   type Stack_Pointer_Info is record
      Byte : Memory_Index;
      --  The position of the first free byte within the memory storage of
      --  Chunk. Byte - 1 denotes the last occupied byte within Chunk.

      Chunk : Chunk_Id_With_Invalid;
      --  The Id of the chunk that accommodated the most recent allocation.
      --  This could be the static or any dynamic chunk.
   end record;

   --  Lightweight version of SS_Stack

   type Stack_Info is record
      Default_Chunk_Size : Memory_Size;
      --  The default memory capacity of a chunk

      Freeable : Boolean;
      --  Indicates whether the secondary stack can be freed

      High_Water_Mark : Memory_Size;
      --  The maximum amount of memory in use throughout the lifetime of the
      --  secondary stack.

      Number_Of_Chunks : Chunk_Count;
      --  The total number of static and dynamic chunks in the secondary stack

      Top : Stack_Pointer_Info;
      --  The stack pointer
   end record;

   function Get_Chunk_Info
     (Stack : SS_Stack_Ptr;
      C_Id  : Chunk_Id) return Chunk_Info;
   --  Obtain the information attributes of a chunk that belongs to secondary
   --  stack Stack and is identified by Id C_Id.

   function Get_Stack_Info (Stack : SS_Stack_Ptr) return Stack_Info;
   --  Obtain the information attributes of secondary stack Stack

end System.Secondary_Stack;
