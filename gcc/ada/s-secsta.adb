------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
   --
   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;

   type Chunk_Id (First, Last : Mark_Id);
   type Chunk_Ptr is access all Chunk_Id;

   type Chunk_Id (First, Last : Mark_Id) is record
      Prev, Next : Chunk_Ptr;
      Mem        : Memory (First .. Last);
   end record;

   type Stack_Id is record
      Top           : Mark_Id;
      Default_Size  : SSE.Storage_Count;
      Current_Chunk : Chunk_Ptr;
   end record;

   type Fixed_Stack_Id is record
      Top  : Mark_Id;
      Last : Mark_Id;
      Mem  : Memory (1 .. Mark_Id'Last / 2 - 1);
      --  This should really be 1 .. Mark_Id'Last, but there is a bug in gigi
      --  with this type, introduced Sep 2001, that causes gigi to reject this
      --  type because its size in bytes overflows ???
   end record;

   type Stack_Ptr is access Stack_Id;
   type Fixed_Stack_Ptr is access Fixed_Stack_Id;

   function From_Addr is new Unchecked_Conversion (Address, Stack_Ptr);
   function To_Addr   is new Unchecked_Conversion (Stack_Ptr, System.Address);
   function To_Stack  is new Unchecked_Conversion (Fixed_Stack_Ptr, Stack_Ptr);
   function To_Fixed  is new Unchecked_Conversion (Stack_Ptr, Fixed_Stack_Ptr);

   procedure Free is new Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);

   --------------
   -- Allocate --
   --------------

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count)
   is
      Stack        : constant Stack_Ptr :=
                       From_Addr (SSL.Get_Sec_Stack_Addr.all);
      Fixed_Stack  : Fixed_Stack_Ptr;
      Chunk        : Chunk_Ptr;
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : constant Mark_Id :=
                       ((Mark_Id (Storage_Size) + Max_Align - 1) / Max_Align)
                         * Max_Align;

      Count_Unreleased_Chunks : Natural;
      To_Be_Released_Chunk    : Chunk_Ptr;

   begin
      --  If the secondary stack is fixed in the primary stack, then the
      --  handling becomes simple

      if not SS_Ratio_Dynamic then
         Fixed_Stack := To_Fixed (Stack);

         if Fixed_Stack.Top + Max_Size > Fixed_Stack.Last then
            raise Storage_Error;
         end if;

         Address := Fixed_Stack.Mem (Fixed_Stack.Top)'Address;
         Fixed_Stack.Top := Fixed_Stack.Top + Mark_Id (Max_Size);
         return;
      end if;

      Chunk := Stack.Current_Chunk;

      --  The Current_Chunk may not be the good one if a lot of release
      --  operations have taken place. So go down the stack if necessary

      while  Chunk.First > Stack.Top loop
         Chunk := Chunk.Prev;
      end loop;

      --  Find out if the available memory in the current chunk is sufficient.
      --  if not, go to the next one and eventally create the necessary room

      Count_Unreleased_Chunks := 0;

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

         --  Create new chunk of the default size unless it is not sufficient

         elsif SSE.Storage_Count (Max_Size) <= Stack.Default_Size then
            Chunk.Next := new Chunk_Id (
              First => Chunk.Last + 1,
              Last  => Chunk.Last + Mark_Id (Stack.Default_Size));

            Chunk.Next.Prev := Chunk;

         else
            Chunk.Next := new Chunk_Id (
              First => Chunk.Last + 1,
              Last  => Chunk.Last + Max_Size);

            Chunk.Next.Prev := Chunk;
         end if;

         Chunk     := Chunk.Next;
         Stack.Top := Chunk.First;
      end loop;

      --  Resulting address is the address pointed by Stack.Top

      Address      := Chunk.Mem (Stack.Top)'Address;
      Stack.Top    := Stack.Top + Max_Size;
      Stack.Current_Chunk := Chunk;
   end SS_Allocate;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stk : in out System.Address) is
      Stack : Stack_Ptr;
      Chunk : Chunk_Ptr;

      procedure Free is new Unchecked_Deallocation (Stack_Id, Stack_Ptr);

   begin
      if not SS_Ratio_Dynamic then
         return;
      end if;

      Stack := From_Addr (Stk);
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
   end SS_Free;

   -------------
   -- SS_Info --
   -------------

   procedure SS_Info is
      Stack       : constant Stack_Ptr :=
                      From_Addr (SSL.Get_Sec_Stack_Addr.all);
      Fixed_Stack : Fixed_Stack_Ptr;
      Nb_Chunks   : Integer            := 1;
      Chunk       : Chunk_Ptr          := Stack.Current_Chunk;

   begin
      Put_Line ("Secondary Stack information:");

      if not SS_Ratio_Dynamic then
         Fixed_Stack := To_Fixed (Stack);
         Put_Line (
           "  Total size              : "
           & Mark_Id'Image (Fixed_Stack.Last)
           & " bytes");
         Put_Line (
           "  Current allocated space : "
           & Mark_Id'Image (Fixed_Stack.Top - 1)
           & " bytes");
         return;
      end if;

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
        & Mark_Id'Image (Chunk.Last)
        & " bytes");
      Put_Line (
        "  Current allocated space : "
        & Mark_Id'Image (Stack.Top - 1)
        & " bytes");

      Put_Line (
        "  Number of Chunks       : "
        & Integer'Image (Nb_Chunks));

      Put_Line (
        "  Default size of Chunks : "
        & SSE.Storage_Count'Image (Stack.Default_Size));
   end SS_Info;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : in out System.Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is
      Stack : Stack_Ptr;
      Fixed_Stack : Fixed_Stack_Ptr;

   begin
      if not SS_Ratio_Dynamic then
         Fixed_Stack      := To_Fixed (From_Addr (Stk));
         Fixed_Stack.Top  := Fixed_Stack.Mem'First;

         if Size < 2 * Mark_Id'Max_Size_In_Storage_Elements then
            Fixed_Stack.Last := 0;
         else
            Fixed_Stack.Last := Mark_Id (Size) -
              2 * Mark_Id'Max_Size_In_Storage_Elements;
         end if;

         return;
      end if;

      Stack               := new Stack_Id;
      Stack.Current_Chunk := new Chunk_Id (1, Mark_Id (Size));
      Stack.Top           := 1;
      Stack.Default_Size  := SSE.Storage_Count (Size);

      Stk := To_Addr (Stack);
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return From_Addr (SSL.Get_Sec_Stack_Addr.all).Top;
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      From_Addr (SSL.Get_Sec_Stack_Addr.all).Top := M;
   end SS_Release;

   -------------------------
   -- Package Elaboration --
   -------------------------

   --  Allocate a secondary stack for the main program to use.
   --  We make sure that the stack has maximum alignment. Some systems require
   --  this (e.g. Sun), and in any case it is a good idea for efficiency.

   Stack : aliased Stack_Id;
   for Stack'Alignment use Standard'Maximum_Alignment;

   Chunk : aliased Chunk_Id (1, Default_Secondary_Stack_Size);
   for Chunk'Alignment use Standard'Maximum_Alignment;

   Chunk_Address : System.Address;

begin
   if SS_Ratio_Dynamic then
      Stack.Top           := 1;
      Stack.Current_Chunk := Chunk'Access;
      Stack.Default_Size  := Default_Secondary_Stack_Size;
      System.Soft_Links.Set_Sec_Stack_Addr_NT (Stack'Address);

   else
      Chunk_Address := Chunk'Address;
      SS_Init (Chunk_Address, Default_Secondary_Stack_Size);
      System.Soft_Links.Set_Sec_Stack_Addr_NT (Chunk_Address);
   end if;
end System.Secondary_Stack;
