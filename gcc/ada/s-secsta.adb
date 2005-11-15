------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

with System.Soft_Links;
with System.Parameters;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Secondary_Stack is

   package SSL renames System.Soft_Links;

   use type SSE.Storage_Offset;
   use type System.Parameters.Size_Type;

   SS_Ratio_Dynamic : constant Boolean :=
                        Parameters.Sec_Stack_Ratio = Parameters.Dynamic;
   --  There are two entirely different implementations of the secondary
   --  stack mechanism in this unit, and this Boolean is used to select
   --  between them (at compile time, so the generated code will contain
   --  only the code for the desired variant). If SS_Ratio_Dynamic is
   --  True, then the secondary stack is dynamically allocated from the
   --  heap in a linked list of chunks. If SS_Ration_Dynamic is False,
   --  then the secondary stack is allocated statically by grabbing a
   --  section of the primary stack and using it for this purpose.

   type Memory is array (SS_Ptr range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;
   --  This is the type used for actual allocation of secondary stack
   --  areas. We require maximum alignment for all such allocations.

   ---------------------------------------------------------------
   -- Data Structures for Dynamically Allocated Secondary Stack --
   ---------------------------------------------------------------

   --  The following is a diagram of the data structures used for the
   --  case of a dynamically allocated secondary stack, where the stack
   --  is allocated as a linked list of chunks allocated from the heap.

   --                                      +------------------+
   --                                      |       Next       |
   --                                      +------------------+
   --                                      |                  | Last (200)
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  | First (101)
   --                                      +------------------+
   --                         +----------> |          |       |
   --                         |            +----------+-------+
   --                         |                    |  |
   --                         |                    ^  V
   --                         |                    |  |
   --                         |            +-------+----------+
   --                         |            |       |          |
   --                         |            +------------------+
   --                         |            |                  | Last (100)
   --                         |            |         C        |
   --                         |            |         H        |
   --    +-----------------+  |  +-------->|         U        |
   --    |  Current_Chunk -|--+  |         |         N        |
   --    +-----------------+     |         |         K        |
   --    |       Top      -|-----+         |                  | First (1)
   --    +-----------------+               +------------------+
   --    | Default_Size    |               |       Prev       |
   --    +-----------------+               +------------------+
   --

   type Chunk_Id (First, Last : SS_Ptr);
   type Chunk_Ptr is access all Chunk_Id;

   type Chunk_Id (First, Last : SS_Ptr) is record
      Prev, Next : Chunk_Ptr;
      Mem        : Memory (First .. Last);
   end record;

   type Stack_Id is record
      Top           : SS_Ptr;
      Default_Size  : SSE.Storage_Count;
      Current_Chunk : Chunk_Ptr;
   end record;

   type Stack_Ptr is access Stack_Id;
   --  Pointer to record used to represent a dynamically allocated secondary
   --  stack descriptor for a secondary stack chunk.

   procedure Free is new Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);
   --  Free a dynamically allocated chunk

   function To_Stack_Ptr is new
     Unchecked_Conversion (Address, Stack_Ptr);
   function To_Addr is new
     Unchecked_Conversion (Stack_Ptr, Address);
   --  Convert to and from address stored in task data structures

   --------------------------------------------------------------
   -- Data Structures for Statically Allocated Secondary Stack --
   --------------------------------------------------------------

   --  For the static case, the secondary stack is a single contiguous
   --  chunk of storage, carved out of the primary stack, and represented
   --  by the following data strcuture

   type Fixed_Stack_Id is record
      Top : SS_Ptr;
      --  Index of next available location in Mem. This is initialized to
      --  0, and then incremented on Allocate, and Decremented on Release.

      Last : SS_Ptr;
      --  Length of usable Mem array, which is thus the index past the
      --  last available location in Mem. Mem (Last-1) can be used. This
      --  is used to check that the stack does not overflow.

      Max : SS_Ptr;
      --  Maximum value of Top. Initialized to 0, and then may be incremented
      --  on Allocate, but is never Decremented. The last used location will
      --  be Mem (Max - 1), so Max is the maximum count of used stack space.

      Mem : Memory (0 .. 0);
      --  This is the area that is actually used for the secondary stack.
      --  Note that the upper bound is a dummy value properly defined by
      --  the value of Last. We never actually allocate objects of type
      --  Fixed_Stack_Id, so the bounds declared here do not matter.
   end record;

   Dummy_Fixed_Stack : Fixed_Stack_Id;
   pragma Warnings (Off, Dummy_Fixed_Stack);
   --  Well it is not quite true that we never allocate an object of the
   --  type. This dummy object is allocated for the purpose of getting the
   --  offset of the Mem field via the 'Position attribute (such a nuisance
   --  that we cannot apply this to a field of a type!)

   type Fixed_Stack_Ptr is access Fixed_Stack_Id;
   --  Pointer to record used to describe statically allocated sec stack

   function To_Fixed_Stack_Ptr is new
     Unchecked_Conversion (Address, Fixed_Stack_Ptr);
   --  Convert from address stored in task data structures

   --------------
   -- Allocate --
   --------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align    : constant SS_Ptr := SS_Ptr (Standard'Maximum_Alignment);
      Max_Size     : constant SS_Ptr :=
                       ((SS_Ptr (Storage_Size) + Max_Align - 1) / Max_Align)
                         * Max_Align;

   begin
      --  Case of fixed allocation secondary stack

      if not SS_Ratio_Dynamic then
         declare
            Fixed_Stack : constant Fixed_Stack_Ptr :=
                            To_Fixed_Stack_Ptr (SSL.Get_Sec_Stack_Addr.all);

         begin
            --  Check if max stack usage is increasing

            if Fixed_Stack.Top + Max_Size > Fixed_Stack.Max then

               --  If so, check if max size is exceeded

               if Fixed_Stack.Top + Max_Size > Fixed_Stack.Last then
                  raise Storage_Error;
               end if;

               --  Record new max usage

               Fixed_Stack.Max := Fixed_Stack.Top + Max_Size;
            end if;

            --  Set resulting address and update top of stack pointer

            Addr := Fixed_Stack.Mem (Fixed_Stack.Top)'Address;
            Fixed_Stack.Top := Fixed_Stack.Top + Max_Size;
         end;

      --  Case of dynamically allocated secondary stack

      else
         declare
            Stack : constant Stack_Ptr :=
                      To_Stack_Ptr (SSL.Get_Sec_Stack_Addr.all);
            Chunk : Chunk_Ptr;

            To_Be_Released_Chunk : Chunk_Ptr;

         begin
            Chunk := Stack.Current_Chunk;

            --  The Current_Chunk may not be the good one if a lot of release
            --  operations have taken place. So go down the stack if necessary

            while Chunk.First > Stack.Top loop
               Chunk := Chunk.Prev;
            end loop;

            --  Find out if the available memory in the current chunk is
            --  sufficient, if not, go to the next one and eventally create
            --  the necessary room.

            while Chunk.Last - Stack.Top + 1 < Max_Size loop
               if Chunk.Next /= null then

                  --  Release unused non-first empty chunk

                  if Chunk.Prev /= null and then Chunk.First = Stack.Top then
                     To_Be_Released_Chunk := Chunk;
                     Chunk := Chunk.Prev;
                     Chunk.Next := To_Be_Released_Chunk.Next;
                     To_Be_Released_Chunk.Next.Prev := Chunk;
                     Free (To_Be_Released_Chunk);
                  end if;

                  --  Create new chunk of default size unless it is not
                  --  sufficient to satisfy the current request.

               elsif SSE.Storage_Count (Max_Size) <= Stack.Default_Size then
                  Chunk.Next :=
                    new Chunk_Id
                      (First => Chunk.Last + 1,
                       Last  => Chunk.Last + SS_Ptr (Stack.Default_Size));

                  Chunk.Next.Prev := Chunk;

                  --  Otherwise create new chunk of requested size

               else
                  Chunk.Next :=
                    new Chunk_Id
                      (First => Chunk.Last + 1,
                       Last  => Chunk.Last + Max_Size);

                  Chunk.Next.Prev := Chunk;
               end if;

               Chunk     := Chunk.Next;
               Stack.Top := Chunk.First;
            end loop;

            --  Resulting address is the address pointed by Stack.Top

            Addr                := Chunk.Mem (Stack.Top)'Address;
            Stack.Top           := Stack.Top + Max_Size;
            Stack.Current_Chunk := Chunk;
         end;
      end if;
   end SS_Allocate;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stk : in out Address) is
   begin
      --  Case of statically allocated secondary stack, nothing to free

      if not SS_Ratio_Dynamic then
         return;

      --  Case of dynamically allocated secondary stack

      else
         declare
            Stack : Stack_Ptr := To_Stack_Ptr (Stk);
            Chunk : Chunk_Ptr;

            procedure Free is new Unchecked_Deallocation (Stack_Id, Stack_Ptr);

         begin
            Chunk := Stack.Current_Chunk;

            while Chunk.Prev /= null loop
               Chunk := Chunk.Prev;
            end loop;

            while Chunk.Next /= null loop
               Chunk := Chunk.Next;
               Free (Chunk.Prev);
            end loop;

            Free (Chunk);
            Free (Stack);
            Stk := Null_Address;
         end;
      end if;
   end SS_Free;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Long_Long_Integer is
   begin
      if SS_Ratio_Dynamic then
         return -1;
      else
         declare
            Fixed_Stack : constant Fixed_Stack_Ptr :=
                            To_Fixed_Stack_Ptr (SSL.Get_Sec_Stack_Addr.all);
         begin
            return Long_Long_Integer (Fixed_Stack.Max);
         end;
      end if;
   end SS_Get_Max;

   -------------
   -- SS_Info --
   -------------

   procedure SS_Info is
   begin
      Put_Line ("Secondary Stack information:");

      --  Case of fixed secondary stack

      if not SS_Ratio_Dynamic then
         declare
            Fixed_Stack : constant Fixed_Stack_Ptr :=
                            To_Fixed_Stack_Ptr (SSL.Get_Sec_Stack_Addr.all);

         begin
            Put_Line (
                      "  Total size              : "
                      & SS_Ptr'Image (Fixed_Stack.Last)
                      & " bytes");

            Put_Line (
                      "  Current allocated space : "
                      & SS_Ptr'Image (Fixed_Stack.Top - 1)
                      & " bytes");
         end;

      --  Case of dynamically allocated secondary stack

      else
         declare
            Stack     : constant Stack_Ptr :=
                          To_Stack_Ptr (SSL.Get_Sec_Stack_Addr.all);
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

            Put_Line (
                      "  Total size              : "
                      & SS_Ptr'Image (Chunk.Last)
                      & " bytes");

            Put_Line (
                      "  Current allocated space : "
                      & SS_Ptr'Image (Stack.Top - 1)
                      & " bytes");

            Put_Line (
                      "  Number of Chunks       : "
                      & Integer'Image (Nb_Chunks));

            Put_Line (
                      "  Default size of Chunks : "
                      & SSE.Storage_Count'Image (Stack.Default_Size));
         end;
      end if;
   end SS_Info;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : in out Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is
   begin
      --  Case of fixed size secondary stack

      if not SS_Ratio_Dynamic then
         declare
            Fixed_Stack : constant Fixed_Stack_Ptr :=
                            To_Fixed_Stack_Ptr (Stk);

         begin
            Fixed_Stack.Top  := 0;
            Fixed_Stack.Max  := 0;

            if Size < Dummy_Fixed_Stack.Mem'Position then
               Fixed_Stack.Last := 0;
            else
               Fixed_Stack.Last :=
                 SS_Ptr (Size) - Dummy_Fixed_Stack.Mem'Position;
            end if;
         end;

      --  Case of dynamically allocated secondary stack

      else
         declare
            Stack : Stack_Ptr;
         begin
            Stack               := new Stack_Id;
            Stack.Current_Chunk := new Chunk_Id (1, SS_Ptr (Size));
            Stack.Top           := 1;
            Stack.Default_Size  := SSE.Storage_Count (Size);
            Stk := To_Addr (Stack);
         end;
      end if;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
      Sstk : constant System.Address := SSL.Get_Sec_Stack_Addr.all;
   begin
      if SS_Ratio_Dynamic then
         return (Sstk => Sstk, Sptr => To_Stack_Ptr (Sstk).Top);
      else
         return (Sstk => Sstk, Sptr => To_Fixed_Stack_Ptr (Sstk).Top);
      end if;
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      if SS_Ratio_Dynamic then
         To_Stack_Ptr (M.Sstk).Top := M.Sptr;
      else
         To_Fixed_Stack_Ptr (M.Sstk).Top := M.Sptr;
      end if;
   end SS_Release;

   -------------------------
   -- Package Elaboration --
   -------------------------

   --  Allocate a secondary stack for the main program to use

   --  We make sure that the stack has maximum alignment. Some systems require
   --  this (e.g. Sun), and in any case it is a good idea for efficiency.

   Stack : aliased Stack_Id;
   for Stack'Alignment use Standard'Maximum_Alignment;

   Chunk : aliased Chunk_Id (1, SS_Ptr (Default_Secondary_Stack_Size));
   for Chunk'Alignment use Standard'Maximum_Alignment;

   Chunk_Address : Address;

begin
   if SS_Ratio_Dynamic then
      Stack.Top           := 1;
      Stack.Current_Chunk := Chunk'Access;
      Stack.Default_Size  := SSE.Storage_Offset (Default_Secondary_Stack_Size);
      System.Soft_Links.Set_Sec_Stack_Addr_NT (Stack'Address);

   else
      Chunk_Address := Chunk'Address;
      SS_Init (Chunk_Address, Default_Secondary_Stack_Size);
      System.Soft_Links.Set_Sec_Stack_Addr_NT (Chunk_Address);
   end if;
end System.Secondary_Stack;
