------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Parameters;       use System.Parameters;
with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Secondary_Stack is

   procedure Free is new Ada.Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);
   --  Free a dynamically allocated chunk

   procedure SS_Allocate_Dynamic
     (Stack       : SS_Stack_Ptr;
      Mem_Request : SS_Ptr;
      Addr        : out Address);
   pragma Inline (SS_Allocate_Dynamic);
   --  Allocate enough space on dynamic secondary stack Stack to accommodate an
   --  object of size Mem_Request. Addr denotes the address where the object is
   --  to be placed.

   procedure SS_Allocate_Static
     (Stack       : SS_Stack_Ptr;
      Mem_Request : SS_Ptr;
      Addr        : out Address);
   pragma Inline (SS_Allocate_Static);
   --  Allocate enough space on static secondary stack Stack to accommodate an
   --  object of size Mem_Request. Addr denotes the address where the object is
   --  to be placed.

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : Storage_Count)
   is
      function Round_Up (Size : Storage_Count) return SS_Ptr;
      pragma Inline (Round_Up);
      --  Round up Size to the nearest multiple of the maximum alignment on the
      --  target.

      function Round_Up_Overflows (Size : Storage_Count) return Boolean;
      pragma Inline (Round_Up_Overflows);
      --  Determine whether a round up of Size to the nearest multiple of the
      --  maximum alignment will overflow the operation.

      --------------
      -- Round_Up --
      --------------

      function Round_Up (Size : Storage_Count) return SS_Ptr is
         Max_Align : constant SS_Ptr := SS_Ptr (Standard'Maximum_Alignment);

      begin
         return ((SS_Ptr (Size) + Max_Align - 1) / Max_Align) * Max_Align;
      end Round_Up;

      ------------------------
      -- Round_Up_Overflows --
      ------------------------

      function Round_Up_Overflows (Size : Storage_Count) return Boolean is
         Max_Align : constant Storage_Count := Standard'Maximum_Alignment;

      begin
         return Storage_Count (SS_Ptr'Last) - Max_Align < Size;
      end Round_Up_Overflows;

      --  Local variables

      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;
      --  The secondary stack of the current task

      Mem_Request : SS_Ptr;

   --  Start of processing for SS_Allocate

   begin
      --  It should not be possible to allocate an object of size zero

      pragma Assert (Storage_Size > 0);

      --  Round up the requested allocation size to the nearest multiple of the
      --  maximum alignment value for the target. This ensures efficient stack
      --  access. Check that the rounding operation does not overflow SS_Ptr.

      if Round_Up_Overflows (Storage_Size) then
         raise Storage_Error;
      end if;

      Mem_Request := Round_Up (Storage_Size);

      if Sec_Stack_Dynamic then
         SS_Allocate_Dynamic (Stack, Mem_Request, Addr);

      else
         SS_Allocate_Static (Stack, Mem_Request, Addr);
      end if;
   end SS_Allocate;

   -------------------------
   -- SS_Allocate_Dynamic --
   -------------------------

   procedure SS_Allocate_Dynamic
     (Stack       : SS_Stack_Ptr;
      Mem_Request : SS_Ptr;
      Addr        : out Address)
   is
      procedure Delete_Chunk (Chunk : in out Chunk_Ptr);
      pragma Inline (Delete_Chunk);
      --  Unchain chunk Chunk from the secondary stack and delete it

      procedure Link_Chunks (First : Chunk_Ptr; Second : Chunk_Ptr);
      pragma Inline (Link_Chunks);
      --  Link chunk Second to chunk First

      procedure Update_Max;
      pragma Inline (Update_Max);
      --  Raise the Max watermark if needed, based on Stack.Top

      ------------------
      -- Delete_Chunk --
      ------------------

      procedure Delete_Chunk (Chunk : in out Chunk_Ptr) is
         Next : constant Chunk_Ptr := Chunk.Next;
         Prev : constant Chunk_Ptr := Chunk.Prev;

      begin
         --  A chunk must always succeed another chunk. In the base case, that
         --  chunk is the Internal_Chunk.

         pragma Assert (Prev /= null);

         Chunk.Next := null;    --        Chunk --> X
         Chunk.Prev := null;    --  X <-- Chunk

         --  The chunk being deleted is the last chunk

         if Next = null then
            Prev.Next := null;  --  Prev --> X

         --  Otherwise link both the Prev and Next chunks

         else
            Link_Chunks (Prev, Next);
         end if;

         Free (Chunk);
      end Delete_Chunk;

      -----------------
      -- Link_Chunks --
      -----------------

      procedure Link_Chunks (First : Chunk_Ptr; Second : Chunk_Ptr) is
      begin
         First.Next  := Second;  --  First --> Second
         Second.Prev := First;   --  First <-- Second
      end Link_Chunks;

      ----------------
      -- Update_Max --
      ----------------

      procedure Update_Max is
      begin
         if Stack.Top > Stack.Max then
            Stack.Max := Stack.Top;
         end if;
      end Update_Max;

      --  Local variables

      Chunk      : Chunk_Ptr;
      Chunk_Size : SS_Ptr;
      Next_Chunk : Chunk_Ptr;
      Top_Chunk  : Chunk_Ptr;

   --  Start of processing for SS_Allocate_Dynamic

   begin
      --  Find the chunk where Top lives by going in reverse, starting from
      --  Current_Chunk.
      --
      --          Top
      --          |
      --    +--------+ --> +----------+ --> +-----------------+
      --    |#####|  |     |####      |     |###########      |
      --    +--------+ <-- +----------+ <-- +-----------------+
      --                                             ^
      --                                        Current_Chunk

      Top_Chunk := Stack.Current_Chunk;

      while Top_Chunk.First > Stack.Top loop
         Top_Chunk := Top_Chunk.Prev;
      end loop;

      --  Inspect Top_Chunk to determine whether the remaining space is big
      --  enough to fit the object.
      --
      --      Addr Top
      --      |    |
      --    +--------+ ...
      --    |######| |
      --    +--------+ ...
      --         ^
      --     Top_Chunk

      if Top_Chunk.Last - Stack.Top + 1 >= Mem_Request then
         Addr      := Top_Chunk.Mem (Stack.Top)'Address;
         Stack.Top := Stack.Top + Mem_Request;
         Update_Max;

         return;
      end if;

      --  At this point it is known that Top_Chunk is not big enough to fit
      --  the object. Examine subsequent chunks using the following criteria:
      --
      --    * If a chunk is too small to fit the object, delete it
      --
      --    * If a chunk is big enough to fit the object, use that chunk

      Chunk := Top_Chunk.Next;
      while Chunk /= null loop

         --  Capture the next chunk in case the current one is deleted

         Next_Chunk := Chunk.Next;

         --  The current chunk is too small to fit the object and must be
         --  deleted to avoid creating a hole in the secondary stack. Note
         --  that this may delete the Current_Chunk.

         if Chunk.Last - Chunk.First + 1 < Mem_Request then
            Delete_Chunk (Chunk);

         --  Otherwise the chunk is big enough to fit the object. Use this
         --  chunk to store the object.
         --
         --                    Addr   Top
         --                    |      |
         --    +--------+ --> +----------+ ... ...................
         --    |#####   |     |#######|  |     :                 :
         --    +--------+ <-- +----------+ ... ...................
         --        ^               ^                    ^
         --    Top_Chunk         Chunk             Current_Chunk

         else
            Addr      := Chunk.Mem (Chunk.First)'Address;
            Stack.Top := Chunk.First + Mem_Request;
            Update_Max;

            return;
         end if;

         Chunk := Next_Chunk;
      end loop;

      --  At this point one of the following outcomes took place:
      --
      --    * Top_Chunk is the last chunk in the secondary stack
      --
      --    * Top_Chunk was not the last chunk originally. It was followed by
      --      chunks which were too small to fit the object and as a result
      --      were deleted, thus making Top_Chunk the last chunk.

      pragma Assert (Top_Chunk.Next = null);

      --  Create a new chunk big enough to fit the object. The size of the
      --  chunk must be at least the minimum default size.

      if Mem_Request <= Stack.Size then
         Chunk_Size := Stack.Size;
      else
         Chunk_Size := Mem_Request;
      end if;

      --  Check that the indexing limits are not exceeded

      if SS_Ptr'Last - Top_Chunk.Last < Chunk_Size then
         raise Storage_Error;
      end if;

      Chunk :=
        new Chunk_Id
              (First => Top_Chunk.Last + 1,
               Last  => Top_Chunk.Last + Chunk_Size);

      --  Grow the secondary stack by adding the new chunk to Top_Chunk. The
      --  new chunk also becomes the Current_Chunk because it is the last in
      --  the list of chunks.
      --
      --                    Addr      Top
      --                    |         |
      --    +--------+ --> +-------------+
      --    |#####   |     |##########|  |
      --    +--------+ <-- +-------------+
      --        ^                ^
      --    Top_Chunk       Current_Chunk

      Link_Chunks (Top_Chunk, Chunk);
      Stack.Current_Chunk := Chunk;

      Addr      := Chunk.Mem (Chunk.First)'Address;
      Stack.Top := Chunk.First + Mem_Request;
      Update_Max;
   end SS_Allocate_Dynamic;

   ------------------------
   -- SS_Allocate_Static --
   ------------------------

   procedure SS_Allocate_Static
     (Stack       : SS_Stack_Ptr;
      Mem_Request : SS_Ptr;
      Addr        : out Address)
   is
   begin
      --  Check if the max stack usage is increasing

      if Stack.Max - Stack.Top < Mem_Request then

         --  Check if the stack will be exceeded. Note that Stack.Top points to
         --  the first free byte, therefore the Stack.Top of a fully allocated
         --  stack is equal to Stack.Size + 1. This check prevents overflow.

         if Stack.Size - Stack.Top + 1 < Mem_Request then
            raise Storage_Error;
         end if;

         --  Record new max usage

         Stack.Max := Stack.Top + Mem_Request;
      end if;

      --  Set resulting address and update top of stack pointer
      --
      --        Addr   Top
      --        |      |
      --    +-------------------+
      --    |##########|        |
      --    +-------------------+
      --              ^
      --        Internal_Chunk

      Addr      := Stack.Internal_Chunk.Mem (Stack.Top)'Address;
      Stack.Top := Stack.Top + Mem_Request;
   end SS_Allocate_Static;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stack : in out SS_Stack_Ptr) is
      procedure Free is
        new Ada.Unchecked_Deallocation (SS_Stack, SS_Stack_Ptr);

      Chunk : Chunk_Ptr;

   begin
      --  If using dynamic secondary stack, free any external chunks

      if SP.Sec_Stack_Dynamic then
         Chunk := Stack.Current_Chunk;

         --  Go to top of linked list and free backwards. Do not free the
         --  internal chunk as it is part of SS_Stack.

         while Chunk.Next /= null loop
            Chunk := Chunk.Next;
         end loop;

         while Chunk.Prev /= null loop
            Chunk := Chunk.Prev;
            Free (Chunk.Next);
         end loop;
      end if;

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
      --  Stack.Max points to the first untouched byte in the stack, thus the
      --  maximum number of bytes that have been allocated on the stack is one
      --  less the value of Stack.Max.

      return Long_Long_Integer (Stack.Max - 1);
   end SS_Get_Max;

   -------------
   -- SS_Info --
   -------------

   procedure SS_Info is
      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;
   begin
      Put_Line ("Secondary Stack information:");

      --  Case of fixed secondary stack

      if not SP.Sec_Stack_Dynamic then
         Put_Line ("  Total size              : "
                   & SS_Ptr'Image (Stack.Size)
                   & " bytes");

         Put_Line ("  Current allocated space : "
                   & SS_Ptr'Image (Stack.Top - 1)
                   & " bytes");

      --  Case of dynamic secondary stack

      else
         declare
            Chunk     : Chunk_Ptr := Stack.Current_Chunk;
            Nb_Chunks : Integer   := 1;

         begin
            while Chunk.Prev /= null loop
               Chunk := Chunk.Prev;
            end loop;

            while Chunk.Next /= null loop
               Nb_Chunks := Nb_Chunks + 1;
               Chunk := Chunk.Next;
            end loop;

            --  Current Chunk information

            --  Note that First of each chunk is one more than Last of the
            --  previous one, so Chunk.Last is the total size of all chunks;
            --  we do not need to walk all the chunks to compute the total
            --  size.

            Put_Line ("  Total size              : "
                      & SS_Ptr'Image (Chunk.Last)
                      & " bytes");

            Put_Line ("  Current allocated space : "
                      & SS_Ptr'Image (Stack.Top - 1)
                      & " bytes");

            Put_Line ("  Number of Chunks        : "
                      & Integer'Image (Nb_Chunks));

            Put_Line ("  Default size of Chunks  : "
                      & SP.Size_Type'Image (Stack.Size));
         end;
      end if;
   end SS_Info;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stack : in out SS_Stack_Ptr;
      Size  : SP.Size_Type := SP.Unspecified_Size)
   is
      Stack_Size : Size_Type;

   begin
      --  If Stack is not null then the stack has been allocated outside the
      --  package (by the compiler or the user) and all that is left to do is
      --  initialize the stack. Otherwise, SS_Init will allocate a secondary
      --  stack from either the heap or the default-sized secondary stack pool
      --  generated by the binder. In the later case, this pool is generated
      --  only when the either No_Implicit_Heap_Allocations
      --  or No_Implicit_Task_Allocations are active, and SS_Init will allocate
      --  all requests for a secondary stack of Unspecified_Size from this
      --  pool.

      if Stack = null then
         if Size = Unspecified_Size then

            --  Cover the case when bootstraping with an old compiler that does
            --  not set Default_SS_Size.

            if Default_SS_Size > 0 then
               Stack_Size := Default_SS_Size;
            else
               Stack_Size := Runtime_Default_Sec_Stack_Size;
            end if;

         else
            Stack_Size := Size;
         end if;

         if Size = Unspecified_Size
           and then Binder_SS_Count > 0
           and then Num_Of_Assigned_Stacks < Binder_SS_Count
         then
            --  The default-sized secondary stack pool is passed from the
            --  binder to this package as an Address since it is not possible
            --  to have a pointer to an array of unconstrained objects. A
            --  pointer to the pool is obtainable via an unchecked conversion
            --  to a constrained array of SS_Stacks that mirrors the one used
            --  by the binder.

            --  However, Ada understandably does not allow a local pointer to
            --  a stack in the pool to be stored in a pointer outside of this
            --  scope. While the conversion is safe in this case, since a view
            --  of a global object is being used, using Unchecked_Access
            --  would prevent users from specifying the restriction
            --  No_Unchecked_Access whenever the secondary stack is used. As
            --  a workaround, the local stack pointer is converted to a global
            --  pointer via System.Address.

            declare
               type Stk_Pool_Array is array (1 .. Binder_SS_Count) of
                 aliased SS_Stack (Default_SS_Size);
               type Stk_Pool_Access is access Stk_Pool_Array;

               function To_Stack_Pool is new
                 Ada.Unchecked_Conversion (Address, Stk_Pool_Access);

               pragma Warnings (Off);
               function To_Global_Ptr is new
                 Ada.Unchecked_Conversion (Address, SS_Stack_Ptr);
               pragma Warnings (On);
               --  Suppress aliasing warning since the pointer we return will
               --  be the only access to the stack.

               Local_Stk_Address : System.Address;

            begin
               Num_Of_Assigned_Stacks := Num_Of_Assigned_Stacks + 1;

               Local_Stk_Address :=
                 To_Stack_Pool
                   (Default_Sized_SS_Pool) (Num_Of_Assigned_Stacks)'Address;
               Stack := To_Global_Ptr (Local_Stk_Address);
            end;

            Stack.Freeable := False;
         else
            Stack := new SS_Stack (Stack_Size);
            Stack.Freeable := True;
         end if;
      end if;

      Stack.Top := 1;
      Stack.Max := 1;
      Stack.Current_Chunk := Stack.Internal_Chunk'Access;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
      Stack : constant SS_Stack_Ptr := Get_Sec_Stack.all;

   begin
      return (Sec_Stack => Stack, Sptr => Stack.Top);
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      M.Sec_Stack.Top := M.Sptr;
   end SS_Release;

end System.Secondary_Stack;
