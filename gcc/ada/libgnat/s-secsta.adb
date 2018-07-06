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
with System.Soft_Links;

package body System.Secondary_Stack is

   package SSL renames System.Soft_Links;

   use type System.Parameters.Size_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);
   --  Free a dynamically allocated chunk

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Count;

      Max_Align   : constant SS_Ptr := SS_Ptr (Standard'Maximum_Alignment);
      Mem_Request : SS_Ptr;

      Stack       : constant SS_Stack_Ptr := SSL.Get_Sec_Stack.all;
   begin
      --  Round up Storage_Size to the nearest multiple of the max alignment
      --  value for the target. This ensures efficient stack access. First
      --  perform a check to ensure that the rounding operation does not
      --  overflow SS_Ptr.

      if SSE.Storage_Count (SS_Ptr'Last) - Standard'Maximum_Alignment <
        Storage_Size
      then
         raise Storage_Error;
      end if;

      Mem_Request := ((SS_Ptr (Storage_Size) + Max_Align - 1) / Max_Align) *
                       Max_Align;

      --  Case of fixed secondary stack

      if not SP.Sec_Stack_Dynamic then
         --  Check if max stack usage is increasing

         if Stack.Max - Stack.Top - Mem_Request < 0 then

            --  If so, check if the stack is exceeded, noting Stack.Top points
            --  to the first free byte (so the value of Stack.Top on a fully
            --  allocated stack will be Stack.Size + 1). The comparison is
            --  formed to prevent integer overflows.

            if Stack.Size - Stack.Top - Mem_Request < -1 then
               raise Storage_Error;
            end if;

            --  Record new max usage

            Stack.Max := Stack.Top + Mem_Request;
         end if;

         --  Set resulting address and update top of stack pointer

         Addr := Stack.Internal_Chunk.Mem (Stack.Top)'Address;
         Stack.Top := Stack.Top + Mem_Request;

      --  Case of dynamic secondary stack

      else
         declare
            Chunk                : Chunk_Ptr;
            Chunk_Size           : SS_Ptr;
            To_Be_Released_Chunk : Chunk_Ptr;

         begin
            Chunk := Stack.Current_Chunk;

            --  The Current_Chunk may not be the best one if a lot of release
            --  operations have taken place. Go down the stack if necessary.

            while Chunk.First > Stack.Top loop
               Chunk := Chunk.Prev;
            end loop;

            --  Find out if the available memory in the current chunk is
            --  sufficient, if not, go to the next one and eventually create
            --  the necessary room.

            while Chunk.Last - Stack.Top - Mem_Request < -1 loop
               if Chunk.Next /= null then
                  --  Release unused non-first empty chunk

                  if Chunk.Prev /= null and then Chunk.First = Stack.Top then
                     To_Be_Released_Chunk := Chunk;
                     Chunk := Chunk.Prev;
                     Chunk.Next := To_Be_Released_Chunk.Next;
                     To_Be_Released_Chunk.Next.Prev := Chunk;
                     Free (To_Be_Released_Chunk);
                  end if;

               --  Create a new chunk

               else
                  --  The new chunk should be no smaller than the default
                  --  chunk size to minimize the amount of secondary stack
                  --  management.

                  if Mem_Request <= Stack.Size then
                     Chunk_Size := Stack.Size;
                  else
                     Chunk_Size := Mem_Request;
                  end if;

                  --  Check that the indexing limits are not exceeded

                  if SS_Ptr'Last - Chunk.Last - Chunk_Size < 0 then
                     raise Storage_Error;
                  end if;

                  Chunk.Next :=
                    new Chunk_Id
                      (First => Chunk.Last + 1,
                       Last  => Chunk.Last + Chunk_Size);

                  Chunk.Next.Prev := Chunk;
               end if;

               Chunk     := Chunk.Next;
               Stack.Top := Chunk.First;
            end loop;

            --  Resulting address is the address pointed by Stack.Top

            Addr                := Chunk.Mem (Stack.Top)'Address;
            Stack.Top           := Stack.Top + Mem_Request;
            Stack.Current_Chunk := Chunk;

            --  Record new max usage

            if Stack.Top > Stack.Max then
               Stack.Max := Stack.Top;
            end if;

         end;
      end if;
   end SS_Allocate;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stack : in out SS_Stack_Ptr) is
      procedure Free is
         new Ada.Unchecked_Deallocation (SS_Stack, SS_Stack_Ptr);
   begin
      --  If using dynamic secondary stack, free any external chunks

      if SP.Sec_Stack_Dynamic then
         declare
            Chunk : Chunk_Ptr;

            procedure Free is
              new Ada.Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);

         begin
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
         end;
      end if;

      if Stack.Freeable then
         Free (Stack);
      end if;
   end SS_Free;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Long_Long_Integer is
      Stack : constant SS_Stack_Ptr := SSL.Get_Sec_Stack.all;
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
      Stack : constant SS_Stack_Ptr := SSL.Get_Sec_Stack.all;
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
            Nb_Chunks : Integer   := 1;
            Chunk     : Chunk_Ptr := Stack.Current_Chunk;

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
            --  previous one, so Chunk.Last is the total size of all chunks; we
            --  don't need to walk all the chunks to compute the total size.

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
      use Parameters;

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
      Stack : constant SS_Stack_Ptr := SSL.Get_Sec_Stack.all;
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
