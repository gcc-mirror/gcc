------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.CHERI;        use Interfaces.CHERI;
with System.Parameters;       use System.Parameters;
with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Secondary_Stack is

   ------------------------------------
   -- Binder Allocated Stack Support --
   ------------------------------------

   --  When at least one of the following restrictions
   --
   --    No_Implicit_Heap_Allocations
   --    No_Implicit_Task_Allocations
   --
   --  is in effect, the binder creates a static secondary stack pool, where
   --  each stack has a default size. Assignment of these stacks to tasks is
   --  performed by SS_Init. The following variables are defined in this unit
   --  in order to avoid depending on the binder. Their values are set by the
   --  binder.

   Binder_SS_Count : Natural := 0;
   pragma Export (Ada, Binder_SS_Count, "__gnat_binder_ss_count");
   --  The number of secondary stacks in the pool created by the binder

   Binder_Default_SS_Size : Size_Type;
   pragma Export (Ada, Binder_Default_SS_Size, "__gnat_default_ss_size");
   --  The default secondary stack size as specified by the binder. The value
   --  is defined here rather than in init.c or System.Init because the ZFP and
   --  Ravenscar-ZFP run-times lack these locations.

   Binder_Default_SS_Pool : Address;
   pragma Export (Ada, Binder_Default_SS_Pool, "__gnat_default_ss_pool");
   --  The address of the secondary stack pool created by the binder

   Binder_Default_SS_Pool_Index : Natural := 0;
   --  Index into the secondary stack pool created by the binder

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Allocate_Dynamic
     (Stack    : SS_Stack_Ptr;
      Mem_Size : Memory_Size;
      Addr     : out Address);
   pragma Inline (Allocate_Dynamic);
   --  Allocate enough space on dynamic secondary stack Stack to fit a request
   --  of size Mem_Size. Addr denotes the address of the first byte of the
   --  allocation.

   procedure Allocate_On_Chunk
     (Stack      : SS_Stack_Ptr;
      Prev_Chunk : SS_Chunk_Ptr;
      Chunk      : SS_Chunk_Ptr;
      Byte       : Memory_Index;
      Mem_Size   : Memory_Size;
      Addr       : out Address);
   pragma Inline (Allocate_On_Chunk);
   --  Allocate enough space on chunk Chunk to fit a request of size Mem_Size.
   --  Stack is the owner of the allocation Chunk. Prev_Chunk is the preceding
   --  chunk of Chunk. Byte indicates the first free byte within Chunk. Addr
   --  denotes the address of the first byte of the allocation. This routine
   --  updates the state of Stack.all to reflect the side effects of the
   --  allocation.

   procedure Allocate_Static
     (Stack    : SS_Stack_Ptr;
      Mem_Size : Memory_Size;
      Addr     : out Address);
   pragma Inline (Allocate_Static);
   --  Allocate enough space on static secondary stack Stack to fit a request
   --  of size Mem_Size. Addr denotes the address of the first byte of the
   --  allocation.

   procedure Free is new Ada.Unchecked_Deallocation (SS_Chunk, SS_Chunk_Ptr);
   --  Free a dynamically allocated chunk

   procedure Free is new Ada.Unchecked_Deallocation (SS_Stack, SS_Stack_Ptr);
   --  Free a dynamically allocated secondary stack

   function Has_Enough_Free_Memory
     (Chunk    : SS_Chunk_Ptr;
      Byte     : Memory_Index;
      Mem_Size : Memory_Size) return Boolean;
   pragma Inline (Has_Enough_Free_Memory);
   --  Determine whether chunk Chunk has enough room to fit a memory request of
   --  size Mem_Size, starting from the first free byte of the chunk denoted by
   --  Byte.

   function Number_Of_Chunks (Stack : SS_Stack_Ptr) return Chunk_Count;
   pragma Inline (Number_Of_Chunks);
   --  Count the number of static and dynamic chunks of secondary stack Stack

   function Size_Up_To_And_Including (Chunk : SS_Chunk_Ptr) return Memory_Size;
   pragma Inline (Size_Up_To_And_Including);
   --  Calculate the size of secondary stack which houses chunk Chunk, from the
   --  start of the secondary stack up to and including Chunk itself. The size
   --  includes the following kinds of memory:
   --
   --    * Free memory in used chunks due to alignment holes
   --    * Occupied memory by allocations
   --
   --  This is a constant time operation, regardless of the secondary stack's
   --  nature.

   function Top_Chunk_Id (Stack : SS_Stack_Ptr) return Chunk_Id_With_Invalid;
   pragma Inline (Top_Chunk_Id);
   --  Obtain the Chunk_Id of the chunk indicated by secondary stack Stack's
   --  pointer.

   function Used_Memory_Size (Stack : SS_Stack_Ptr) return Memory_Size;
   pragma Inline (Used_Memory_Size);
   --  Calculate the size of stack Stack's occupied memory usage. This includes
   --  the following kinds of memory:
   --
   --    * Free memory in used chunks due to alignment holes
   --    * Occupied memory by allocations
   --
   --  This is a constant time operation, regardless of the secondary stack's
   --  nature.

   function Padding_For_Bounds_Alignment
     (Ptr  : Address;
      Size : Memory_Size)
      return Memory_Size;
   pragma Inline (Padding_For_Bounds_Alignment);
   --  Calculate the amount of padding needed to align an address up to the
   --  next representable boundary.

   ----------------------
   -- Allocate_Dynamic --
   ----------------------

   procedure Allocate_Dynamic
     (Stack    : SS_Stack_Ptr;
      Mem_Size : Memory_Size;
      Addr     : out Address)
   is
      function Allocate_New_Chunk return SS_Chunk_Ptr;
      pragma Inline (Allocate_New_Chunk);
      --  Create a new chunk which is big enough to fit a request of size
      --  Mem_Size.

      ------------------------
      -- Allocate_New_Chunk --
      ------------------------

      function Allocate_New_Chunk return SS_Chunk_Ptr is
         Chunk_Size : Memory_Size;

      begin
         --  The size of the new chunk must fit the memory request precisely.
         --  In the case where the memory request is way too small, use the
         --  default chunk size. This avoids creating multiple tiny chunks.

         Chunk_Size := Mem_Size;

         if Chunk_Size < Stack.Default_Chunk_Size then
            Chunk_Size := Stack.Default_Chunk_Size;
         end if;

         return new SS_Chunk (Chunk_Size);

      --  The creation of the new chunk may exhaust the heap. Raise a new
      --  Storage_Error to indicate that the secondary stack is exhausted
      --  as well.

      exception
         when Storage_Error =>
            raise Storage_Error with "secondary stack exhausted";
      end Allocate_New_Chunk;

      --  Local variables

      Next_Chunk : SS_Chunk_Ptr;

   --  Start of processing for Allocate_Dynamic

   begin
      --  Determine whether the chunk indicated by the stack pointer is big
      --  enough to fit the memory request and if it is, allocate on it.

      if Has_Enough_Free_Memory
           (Chunk    => Stack.Top.Chunk,
            Byte     => Stack.Top.Byte,
            Mem_Size => Mem_Size)
      then
         Allocate_On_Chunk
           (Stack      => Stack,
            Prev_Chunk => null,
            Chunk      => Stack.Top.Chunk,
            Byte       => Stack.Top.Byte,
            Mem_Size   => Mem_Size,
            Addr       => Addr);

         return;
      end if;

      --  At this point it is known that the chunk indicated by the stack
      --  pointer is not big enough to fit the memory request. Examine all
      --  subsequent chunks, and apply the following criteria:
      --
      --    * If the current chunk is too small, free it
      --
      --    * If the current chunk is big enough, allocate on it
      --
      --  This ensures that no space is wasted. The process is costly, however
      --  allocation is costly in general. Paying the price here keeps routines
      --  SS_Mark and SS_Release cheap.

      while Stack.Top.Chunk.Next /= null loop

         --  The current chunk is big enough to fit the memory request,
         --  allocate on it.

         if Has_Enough_Free_Memory
              (Chunk    => Stack.Top.Chunk.Next,
               Byte     => Stack.Top.Chunk.Next.Memory'First,
               Mem_Size => Mem_Size)
         then
            Allocate_On_Chunk
              (Stack      => Stack,
               Prev_Chunk => Stack.Top.Chunk,
               Chunk      => Stack.Top.Chunk.Next,
               Byte       => Stack.Top.Chunk.Next.Memory'First,
               Mem_Size   => Mem_Size,
               Addr       => Addr);

            return;

         --  Otherwise the chunk is too small, free it

         else
            Next_Chunk := Stack.Top.Chunk.Next.Next;

            --  Unchain the chunk from the stack. This keeps the next candidate
            --  chunk situated immediately after Top.Chunk.
            --
            --    Top.Chunk     Top.Chunk.Next   Top.Chunk.Next.Next
            --        |               |              (Next_Chunk)
            --        v               v                   v
            --    +-------+     +------------+     +--------------+
            --    |       | --> |            | --> |              |
            --    +-------+     +------------+     +--------------+
            --                   to be freed

            Free (Stack.Top.Chunk.Next);
            Stack.Top.Chunk.Next := Next_Chunk;
         end if;
      end loop;

      --  At this point one of the following outcomes took place:
      --
      --    * Top.Chunk is the last chunk in the stack
      --
      --    * Top.Chunk was not the last chunk originally. It was followed by
      --      chunks which were too small and as a result were deleted, thus
      --      making Top.Chunk the last chunk in the stack.
      --
      --  Either way, nothing should be hanging off the chunk indicated by the
      --  stack pointer.

      pragma Assert (Stack.Top.Chunk.Next = null);

      --  Create a new chunk big enough to fit the memory request, and allocate
      --  on it.

      Stack.Top.Chunk.Next := Allocate_New_Chunk;

      Allocate_On_Chunk
        (Stack      => Stack,
         Prev_Chunk => Stack.Top.Chunk,
         Chunk      => Stack.Top.Chunk.Next,
         Byte       => Stack.Top.Chunk.Next.Memory'First,
         Mem_Size   => Mem_Size,
         Addr       => Addr);
   end Allocate_Dynamic;

   -----------------------
   -- Allocate_On_Chunk --
   -----------------------

   procedure Allocate_On_Chunk
     (Stack      : SS_Stack_Ptr;
      Prev_Chunk : SS_Chunk_Ptr;
      Chunk      : SS_Chunk_Ptr;
      Byte       : Memory_Index;
      Mem_Size   : Memory_Size;
      Addr       : out Address)
   is
      New_High_Water_Mark : Memory_Size;
      Padding             : Memory_Size;

   begin
      --  The allocation occurs on a reused or a brand new chunk. Such a chunk
      --  must always be connected to some previous chunk.

      if Prev_Chunk /= null then
         pragma Assert (Prev_Chunk.Next = Chunk);

         --  Update the Size_Up_To_Chunk because this value is invalidated for
         --  reused and new chunks.
         --
         --                         Prev_Chunk          Chunk
         --                             v                 v
         --    . . . . . . .     +--------------+     +--------
         --                . --> |##############| --> |
         --    . . . . . . .     +--------------+     +--------
         --                       |            |
         --    -------------------+------------+
         --      Size_Up_To_Chunk      Size
         --
         --  The Size_Up_To_Chunk is equal to the size of the whole stack up to
         --  the previous chunk, plus the size of the previous chunk itself.

         Chunk.Size_Up_To_Chunk := Size_Up_To_And_Including (Prev_Chunk);
      end if;

      --  The chunk must have enough room to fit the memory request. If this is
      --  not the case, then a previous step picked the wrong chunk.

      pragma Assert (Has_Enough_Free_Memory (Chunk, Byte, Mem_Size));

      --  The first byte of the allocation is the first free byte within the
      --  chunk.

      Addr := Chunk.Memory (Byte)'Address;

      --  Align the address to ensure that the CHERI bounds will be
      --  representable.

      Padding := Padding_For_Bounds_Alignment (Addr, Mem_Size);
      Addr    := Addr + Storage_Offset (Padding);

      --  The chunk becomes the chunk indicated by the stack pointer. This is
      --  either the currently indicated chunk, an existing chunk, or a brand
      --  new chunk.

      Stack.Top.Chunk := Chunk;

      --  The next free byte is immediately after the memory request
      --
      --          Addr     Top.Byte
      --          |        |
      --    +-----|--------|----+
      --    |##############|    |
      --    +-------------------+

      --  ??? this calculation may overflow on 32bit targets

      Stack.Top.Byte := Byte + Mem_Size + Padding;

      --  At this point the next free byte cannot go beyond the memory capacity
      --  of the chunk indicated by the stack pointer, except when the chunk is
      --  full, in which case it indicates the byte beyond the chunk. Ensure
      --  that the occupied memory is at most as much as the capacity of the
      --  chunk. Top.Byte - 1 denotes the last occupied byte.

      pragma Assert (Stack.Top.Byte - 1 <= Stack.Top.Chunk.Size);

      --  Calculate the new high water mark now that the memory request has
      --  been fulfilled, and update if necessary. The new high water mark is
      --  technically the size of the used memory by the whole stack.

      New_High_Water_Mark := Used_Memory_Size (Stack);

      if New_High_Water_Mark > Stack.High_Water_Mark then
         Stack.High_Water_Mark := New_High_Water_Mark;
      end if;
   end Allocate_On_Chunk;

   ---------------------
   -- Allocate_Static --
   ---------------------

   procedure Allocate_Static
     (Stack    : SS_Stack_Ptr;
      Mem_Size : Memory_Size;
      Addr     : out Address)
   is
   begin
      --  Static secondary stack allocations are performed only on the static
      --  chunk. There should be no dynamic chunks following the static chunk.

      pragma Assert (Stack.Top.Chunk = Stack.Static_Chunk'Access);
      pragma Assert (Stack.Top.Chunk.Next = null);

      --  Raise Storage_Error if the static chunk does not have enough room to
      --  fit the memory request. This indicates that the stack is about to be
      --  depleted.

      if not Has_Enough_Free_Memory
               (Chunk    => Stack.Top.Chunk,
                Byte     => Stack.Top.Byte,
                Mem_Size => Mem_Size)
      then
         raise Storage_Error with "secondary stack exhaused";
      end if;

      Allocate_On_Chunk
        (Stack      => Stack,
         Prev_Chunk => null,
         Chunk      => Stack.Top.Chunk,
         Byte       => Stack.Top.Byte,
         Mem_Size   => Mem_Size,
         Addr       => Addr);
   end Allocate_Static;

   --------------------
   -- Get_Chunk_Info --
   --------------------

   function Get_Chunk_Info
     (Stack : SS_Stack_Ptr;
      C_Id  : Chunk_Id) return Chunk_Info
   is
      function Find_Chunk return SS_Chunk_Ptr;
      pragma Inline (Find_Chunk);
      --  Find the chunk which corresponds to Id. Return null if no such chunk
      --  exists.

      ----------------
      -- Find_Chunk --
      ----------------

      function Find_Chunk return SS_Chunk_Ptr is
         Chunk : SS_Chunk_Ptr;
         Id    : Chunk_Id;

      begin
         Chunk := Stack.Static_Chunk'Access;
         Id    := 1;
         while Chunk /= null loop
            if Id = C_Id then
               return Chunk;
            end if;

            Chunk := Chunk.Next;
            Id    := Id + 1;
         end loop;

         return null;
      end Find_Chunk;

      --  Local variables

      Chunk : constant SS_Chunk_Ptr := Find_Chunk;

   --  Start of processing for Get_Chunk_Info

   begin
      if Chunk = null then
         return Invalid_Chunk;

      else
         return (Size             => Chunk.Size,
                 Size_Up_To_Chunk => Chunk.Size_Up_To_Chunk);
      end if;
   end Get_Chunk_Info;

   --------------------
   -- Get_Stack_Info --
   --------------------

   function Get_Stack_Info (Stack : SS_Stack_Ptr) return Stack_Info is
      Info : Stack_Info;

   begin
      Info.Default_Chunk_Size := Stack.Default_Chunk_Size;
      Info.Freeable           := Stack.Freeable;
      Info.High_Water_Mark    := Stack.High_Water_Mark;
      Info.Number_Of_Chunks   := Number_Of_Chunks (Stack);
      Info.Top.Byte           := Stack.Top.Byte;
      Info.Top.Chunk          := Top_Chunk_Id (Stack);

      return Info;
   end Get_Stack_Info;

   ----------------------------
   -- Has_Enough_Free_Memory --
   ----------------------------

   function Has_Enough_Free_Memory
     (Chunk    : SS_Chunk_Ptr;
      Byte     : Memory_Index;
      Mem_Size : Memory_Size) return Boolean
   is
      Padding : Memory_Size;

   begin
      --  First check if the chunk is full (Byte is > Memory'Last in that
      --  case), then check there is enough free memory.

      --  Byte - 1 denotes the last occupied byte. Subtracting that byte from
      --  the memory capacity of the chunk yields the size of the free memory
      --  within the chunk. The chunk can fit the request as long as the free
      --  memory is as big as the request.

      --  We also need to consider any extra padding needed to align the
      --  address to ensure that the CHERI lower bound is representable.

      Padding :=
        Padding_For_Bounds_Alignment (Chunk.Memory (Byte)'Address, Mem_Size);

      return Chunk.Memory'Last >= Byte
        and then Chunk.Size - (Byte - 1) >= Mem_Size
        and then Chunk.Size - (Byte - 1) - Mem_Size >= Padding;

   end Has_Enough_Free_Memory;

   ----------------------
   -- Number_Of_Chunks --
   ----------------------

   function Number_Of_Chunks (Stack : SS_Stack_Ptr) return Chunk_Count is
      Chunk : SS_Chunk_Ptr;
      Count : Chunk_Count;

   begin
      Chunk := Stack.Static_Chunk'Access;
      Count := 0;
      while Chunk /= null loop
         Chunk := Chunk.Next;
         Count := Count + 1;
      end loop;

      return Count;
   end Number_Of_Chunks;

   ------------------------------
   -- Size_Up_To_And_Including --
   ------------------------------

   function Size_Up_To_And_Including
     (Chunk : SS_Chunk_Ptr) return Memory_Size
   is
   begin
      return Chunk.Size_Up_To_Chunk + Chunk.Size;
   end Size_Up_To_And_Including;

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : Storage_Count;
      Alignment    : SSE.Storage_Count := Standard'Maximum_Alignment)
   is

      function Round_Up (Size : Storage_Count) return Memory_Size;
      pragma Inline (Round_Up);
      --  Round Size up to the nearest multiple of the maximum alignment

      function Align_Addr (Addr : Address) return Address;
      pragma Inline (Align_Addr);
      --  Align Addr to the next multiple of Alignment

      ----------------
      -- Align_Addr --
      ----------------

      function Align_Addr (Addr : Address) return Address is
      begin

         --  L : Alignment
         --  A : Standard'Maximum_Alignment

         --           Addr
         --      L     |     L           L
         --      A--A--A--A--A--A--A--A--A--A--A
         --                  |     |
         --      \----/      |     |
         --     Addr mod L   |   Addr + L
         --                  |
         --                Addr + L - (Addr mod L)

         return Addr + (Alignment - (Addr mod Alignment));
      end Align_Addr;

      --------------
      -- Round_Up --
      --------------

      function Round_Up (Size : Storage_Count) return Memory_Size is
         Algn_MS : constant Memory_Size := Standard'Maximum_Alignment;
         Size_MS : constant Memory_Size := Memory_Size (Size);

      begin
         --  Detect a case where the Size is very large and may yield
         --  a rounded result which is outside the range of Chunk_Memory_Size.
         --  Treat this case as secondary-stack depletion.

         if Memory_Size'Last - Algn_MS < Size_MS then
            raise Storage_Error with "secondary stack exhausted";
         end if;

         return ((Size_MS + Algn_MS - 1) / Algn_MS) * Algn_MS;
      end Round_Up;

      --  Local variables

      Stack    : constant SS_Stack_Ptr := Get_Sec_Stack.all;
      Mem_Size : Memory_Size;

      Over_Aligning : constant Boolean :=
        Alignment > Standard'Maximum_Alignment;

      Over_Align_Padding : SSE.Storage_Count := 0;

      Adjusted_Storage_Size  : Interfaces.CHERI.Bounds_Length;
      --  Storage_Size plus padding for over-alignment and extra padding to
      --  align the capability's upper bound.

      Capability_Lower_Bound : Address;

   --  Start of processing for SS_Allocate

   begin
      --  Alignment must be a power of two and can be:

      --  - lower than or equal to Maximum_Alignment, in which case the result
      --    will be aligned on Maximum_Alignment;
      --  - higher than Maximum_Alignment, in which case the result will be
      --    dynamically realigned.

      if Over_Aligning then
         Over_Align_Padding := Alignment;
      end if;

      --  It should not be possible to request an allocation of negative
      --  size.

      pragma Assert (Storage_Size >= 0);

      --  Round the requested size (plus the needed padding in case of
      --  over-alignment) to ensure that the CHERI bounds length will be
      --  representable.

      Adjusted_Storage_Size :=
        Representable_Length
          (Bounds_Length (Storage_Size + Over_Align_Padding));

      --  Round up to the nearest multiple of the default alignment to ensure
      --  efficient access and that the next available Byte is always aligned
      --  on the default alignement value.

      Mem_Size := Round_Up (Storage_Count (Adjusted_Storage_Size));

      if Sec_Stack_Dynamic then
         Allocate_Dynamic (Stack, Mem_Size, Addr);
      else
         Allocate_Static  (Stack, Mem_Size, Addr);
      end if;

      --  Restrict the capability bounds to the requested allocation size,
      --  possibly with some padding for alignment of the bounds.

      Capability_Lower_Bound :=
        Capability_With_Address_Aligned_Up (Addr, Adjusted_Storage_Size);

      Addr := Capability_With_Exact_Bounds
        (Capability_Lower_Bound, Adjusted_Storage_Size);

      if Over_Aligning then
         Addr := Align_Addr (Addr);
      end if;

   end SS_Allocate;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stack : in out SS_Stack_Ptr) is
      Static_Chunk : constant SS_Chunk_Ptr := Stack.Static_Chunk'Access;
      Next_Chunk   : SS_Chunk_Ptr;

   begin
      --  Free all dynamically allocated chunks. The first dynamic chunk is
      --  found immediately after the static chunk of the stack.

      while Static_Chunk.Next /= null loop
         Next_Chunk := Static_Chunk.Next.Next;
         Free (Static_Chunk.Next);
         Static_Chunk.Next := Next_Chunk;
      end loop;

      --  At this point one of the following outcomes has taken place:
      --
      --    * The stack lacks any dynamic chunks
      --
      --    * The stack had dynamic chunks which were all freed
      --
      --  Either way, there should be nothing hanging off the static chunk

      pragma Assert (Static_Chunk.Next = null);

      --  Free the stack only when it was dynamically allocated

      if Stack.Freeable then
         Free (Stack);
      end if;
   end SS_Free;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Long_Long_Integer is
      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;

   begin
      return Long_Long_Integer (Stack.High_Water_Mark);
   end SS_Get_Max;

   -------------
   -- SS_Info --
   -------------

   procedure SS_Info is
      procedure SS_Info_Dynamic (Stack : SS_Stack_Ptr);
      pragma Inline (SS_Info_Dynamic);
      --  Output relevant information concerning dynamic secondary stack Stack

      function Total_Memory_Size (Stack : SS_Stack_Ptr) return Memory_Size;
      pragma Inline (Total_Memory_Size);
      --  Calculate the size of stack Stack's total memory usage. This includes
      --  the following kinds of memory:
      --
      --    * Free memory in used chunks due to alignment holes
      --    * Free memory in the topmost chunk due to partial usage
      --    * Free memory in unused chunks following the chunk indicated by the
      --      stack pointer.
      --    * Memory occupied by allocations
      --
      --  This is a linear-time operation on the number of chunks.

      ---------------------
      -- SS_Info_Dynamic --
      ---------------------

      procedure SS_Info_Dynamic (Stack : SS_Stack_Ptr) is
      begin
         Put_Line
           ("  Number of Chunks        : " & Number_Of_Chunks (Stack)'Img);

         Put_Line
           ("  Default size of Chunks  : " & Stack.Default_Chunk_Size'Img);
      end SS_Info_Dynamic;

      -----------------------
      -- Total_Memory_Size --
      -----------------------

      function Total_Memory_Size (Stack : SS_Stack_Ptr) return Memory_Size is
         Chunk : SS_Chunk_Ptr;
         Total : Memory_Size;

      begin
         --  The total size of the stack is equal to the size of the stack up
         --  to the chunk indicated by the stack pointer, plus the size of the
         --  indicated chunk, plus the size of any subsequent chunks.

         Total := Size_Up_To_And_Including (Stack.Top.Chunk);

         Chunk := Stack.Top.Chunk.Next;
         while Chunk /= null loop
            Total := Total + Chunk.Size;
            Chunk := Chunk.Next;
         end loop;

         return Total;
      end Total_Memory_Size;

      --  Local variables

      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;

   --  Start of processing for SS_Info

   begin
      Put_Line ("Secondary Stack information:");

      Put_Line
        ("  Total size              : "
         & Total_Memory_Size (Stack)'Img
         & " bytes");

      Put_Line
        ("  Current allocated space : "
         & Used_Memory_Size (Stack)'Img
         & " bytes");

      if Sec_Stack_Dynamic then
         SS_Info_Dynamic (Stack);
      end if;
   end SS_Info;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stack : in out SS_Stack_Ptr;
      Size  : Size_Type := Unspecified_Size)
   is
      function Next_Available_Binder_Sec_Stack return SS_Stack_Ptr;
      pragma Inline (Next_Available_Binder_Sec_Stack);
      --  Return a pointer to the next available stack from the pool created by
      --  the binder. This routine updates global Default_Sec_Stack_Pool_Index.

      -------------------------------------
      -- Next_Available_Binder_Sec_Stack --
      -------------------------------------

      function Next_Available_Binder_Sec_Stack return SS_Stack_Ptr is

         --  The default-sized secondary stack pool generated by the binder
         --  is passed to this unit as an Address because it is not possible
         --  to define a pointer to an array of unconstrained components. The
         --  pointer is instead obtained using an unchecked conversion to a
         --  constrained array of secondary stacks with the same size as that
         --  specified by the binder.

         --  WARNING: The following data structure must be synchronized with
         --  the one created in Bindgen.Gen_Output_File_Ada. The version in
         --  bindgen is called Sec_Default_Sized_Stacks.

         type SS_Pool is
           array (1 .. Binder_SS_Count)
             of aliased SS_Stack (Binder_Default_SS_Size);

         type SS_Pool_Ptr is access SS_Pool;
         --  A reference to the secondary stack pool

         function To_SS_Pool_Ptr is
           new Ada.Unchecked_Conversion (Address, SS_Pool_Ptr);

         --  Use an unchecked conversion to obtain a pointer to one of the
         --  secondary stacks from the pool generated by the binder. There
         --  are several reasons for using the conversion:
         --
         --    * Accessibility checks prevent a value of a local pointer to be
         --      stored outside this scope. The conversion is safe because the
         --      pool is global to the whole application.
         --
         --    * Unchecked_Access may circumvent the accessibility checks, but
         --      it is incompatible with restriction No_Unchecked_Access.
         --
         --    * Unrestricted_Access may circumvent the accessibility checks,
         --      but it is incompatible with pure Ada constructs.
         --      ??? cannot find the restriction or switch

         pragma Warnings (Off);
         function To_SS_Stack_Ptr is
           new Ada.Unchecked_Conversion (Address, SS_Stack_Ptr);
         pragma Warnings (On);

         Pool : SS_Pool_Ptr;

      begin
         --  Obtain a typed view of the pool

         Pool := To_SS_Pool_Ptr (Binder_Default_SS_Pool);

         --  Advance the stack index to the next available stack

         Binder_Default_SS_Pool_Index := Binder_Default_SS_Pool_Index + 1;

         --  Return a pointer to the next available stack

         return To_SS_Stack_Ptr (Pool (Binder_Default_SS_Pool_Index)'Address);
      end Next_Available_Binder_Sec_Stack;

      --  Local variables

      Stack_Size : Memory_Size_With_Invalid;

   --  Start of processing for SS_Init

   begin
      --  Allocate a new stack on the heap or use one from the pool created by
      --  the binder.

      if Stack = null then

         --  The caller requested a pool-allocated stack. Determine the proper
         --  size of the stack based on input from the binder or the runtime in
         --  case the pool is exhausted.

         if Size = Unspecified_Size then

            --  Use the default secondary stack size as specified by the binder
            --  only when it has been set. This prevents a bootstrap issue with
            --  older compilers where the size is never set.

            if Binder_Default_SS_Size > 0 then
               Stack_Size := Binder_Default_SS_Size;

            --  Otherwise use the default stack size of the particular runtime

            else
               Stack_Size := Runtime_Default_Sec_Stack_Size;
            end if;

         --  Otherwise the caller requested a heap-allocated stack. Use the
         --  specified size directly.

         else
            Stack_Size := Size;
         end if;

         --  The caller requested a pool-allocated stack. Use one as long as
         --  the pool created by the binder has available stacks. This stack
         --  cannot be deallocated.

         if Size = Unspecified_Size
           and then Binder_SS_Count > 0
           and then Binder_Default_SS_Pool_Index < Binder_SS_Count
         then
            Stack := Next_Available_Binder_Sec_Stack;
            Stack.Freeable := False;

         --  Otherwise the caller requested a heap-allocated stack, or the pool
         --  created by the binder ran out of available stacks. This stack can
         --  be deallocated.

         else
            --  It should not be possible to create a stack with a negative
            --  default chunk size.

            pragma Assert (Stack_Size in Memory_Size);

            Stack := new SS_Stack (Stack_Size);
            Stack.Freeable := True;
         end if;

      --  Otherwise the stack was already created either by the compiler or by
      --  the user, and is about to be reused.

      else
         null;
      end if;

      --  The static chunk becomes the chunk indicated by the stack pointer.
      --  Note that the stack may still hold dynamic chunks, which in turn may
      --  be reused or freed.

      Stack.Top.Chunk := Stack.Static_Chunk'Access;

      --  The first free byte is the first free byte of the chunk indicated by
      --  the stack pointer.

      Stack.Top.Byte := Stack.Top.Chunk.Memory'First;

      --  Since the chunk indicated by the stack pointer is also the first
      --  chunk in the stack, there are no prior chunks, therefore the size
      --  of the stack up to the chunk is zero.

      Stack.Top.Chunk.Size_Up_To_Chunk := 0;

      --  Reset the high water mark to account for brand new allocations

      Stack.High_Water_Mark := 0;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;

   begin
      return (Stack => Stack, Top => Stack.Top);
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      M.Stack.Top := M.Top;
   end SS_Release;

   ------------------
   -- Top_Chunk_Id --
   ------------------

   function Top_Chunk_Id (Stack : SS_Stack_Ptr) return Chunk_Id_With_Invalid is
      Chunk : SS_Chunk_Ptr;
      Id    : Chunk_Id;

   begin
      Chunk := Stack.Static_Chunk'Access;
      Id    := 1;
      while Chunk /= null loop
         if Chunk = Stack.Top.Chunk then
            return Id;
         end if;

         Chunk := Chunk.Next;
         Id    := Id + 1;
      end loop;

      return Invalid_Chunk_Id;
   end Top_Chunk_Id;

   ----------------------
   -- Used_Memory_Size --
   ----------------------

   function Used_Memory_Size (Stack : SS_Stack_Ptr) return Memory_Size is
   begin
      --  The size of the occupied memory is equal to the size up to the chunk
      --  indicated by the stack pointer, plus the size in use by the indicated
      --  chunk itself. Top.Byte - 1 is the last occupied byte.
      --
      --                                     Top.Byte
      --                                     |
      --    . . . . . . .     +--------------|----+
      --                . ..> |##############|    |
      --    . . . . . . .     +-------------------+
      --                       |             |
      --    -------------------+-------------+
      --      Size_Up_To_Chunk   size in use

      --  ??? this calculation may overflow on 32bit targets

      return Stack.Top.Chunk.Size_Up_To_Chunk + Stack.Top.Byte - 1;
   end Used_Memory_Size;

   ----------------------------------
   -- Padding_For_Bounds_Alignment --
   ----------------------------------

   function Padding_For_Bounds_Alignment
     (Ptr  : Address;
      Size : Memory_Size)
      return Memory_Size
   is
      IA : constant Integer_Address := To_Integer (Ptr);
   begin
      return Memory_Size (Align_Address_Up (IA, Bounds_Length (Size)) - IA);
   end Padding_For_Bounds_Alignment;

end System.Secondary_Stack;
