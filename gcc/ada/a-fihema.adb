------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . F I N A L I Z A T I O N . H E A P _ M A N A G E M E N T      --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 2008-2011, Free Software Foundation, Inc.         --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;                  use System;
with System.Address_Image;
with System.IO;               use System.IO;
with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;
with System.Storage_Pools;    use System.Storage_Pools;

package body Ada.Finalization.Heap_Management is

   Header_Size   : constant Storage_Count  := Node'Size / Storage_Unit;
   Header_Offset : constant Storage_Offset := Header_Size;
   --  Comments needed???

   function Address_To_Node_Ptr is
     new Ada.Unchecked_Conversion (Address, Node_Ptr);

   procedure Attach (N : Node_Ptr; L : Node_Ptr);
   --  Prepend a node to a list

   procedure Detach (N : Node_Ptr);
   --  Unhook a node from an arbitrary list

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   ---------------------------
   -- Add_Offset_To_Address --
   ---------------------------

   function Add_Offset_To_Address
     (Addr   : System.Address;
      Offset : System.Storage_Elements.Storage_Offset) return System.Address
   is
   begin
      return System.Storage_Elements."+" (Addr, Offset);
   end Add_Offset_To_Address;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Collection   : in out Finalization_Collection;
      Addr         : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count;
      Needs_Header : Boolean := True)
   is
   begin
      --  Allocation of a controlled object

      if Needs_Header then

         --  Do not allow the allocation of controlled objects while the
         --  associated collection is being finalized.

         if Collection.Finalization_Started then
            raise Program_Error with "allocation after finalization started";
         end if;

         declare
            N_Addr : Address;
            N_Ptr  : Node_Ptr;

         begin
            --  Use the underlying pool to allocate enough space for the object
            --  and the list header. The returned address points to the list
            --  header.

            Allocate
              (Collection.Base_Pool.all,
               N_Addr,
               Storage_Size + Header_Size,
               Alignment);

            --  Map the allocated memory into a Node record. This converts the
            --  top of the allocated bits into a list header.

            N_Ptr := Address_To_Node_Ptr (N_Addr);
            Attach (N_Ptr, Collection.Objects);

            --  Move the address from Prev to the start of the object. This
            --  operation effectively hides the list header.

            Addr := N_Addr + Header_Offset;
         end;

      --  Allocation of a non-controlled object

      else
         Allocate
           (Collection.Base_Pool.all,
            Addr,
            Storage_Size,
            Alignment);
      end if;
   end Allocate;

   ------------
   -- Attach --
   ------------

   procedure Attach (N : Node_Ptr; L : Node_Ptr) is
   begin
      Lock_Task.all;

      L.Next.Prev := N;
      N.Next := L.Next;
      L.Next := N;
      N.Prev := L;

      Unlock_Task.all;

   exception
      when others =>
         Unlock_Task.all;
         raise;
   end Attach;

   ---------------
   -- Base_Pool --
   ---------------

   function Base_Pool
     (Collection : Finalization_Collection) return Any_Storage_Pool_Ptr
   is
   begin
      return Collection.Base_Pool;
   end Base_Pool;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Collection   : in out Finalization_Collection;
      Addr         : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count;
      Has_Header   : Boolean := True)
   is
   begin
      --  Deallocation of a controlled object

      if Has_Header then
         declare
            N_Addr : Address;
            N_Ptr  : Node_Ptr;

         begin
            --  Move the address from the object to the beginning of the list
            --  header.

            N_Addr := Addr - Header_Offset;

            --  Converts the bits preceding the object into a list header

            N_Ptr := Address_To_Node_Ptr (N_Addr);
            Detach (N_Ptr);

            --  Use the underlying pool to destroy the object along with the
            --  list header.

            Deallocate
              (Collection.Base_Pool.all,
               N_Addr,
               Storage_Size + Header_Size,
               Alignment);
         end;

      --  Deallocation of a non-controlled object

      else
         Deallocate
           (Collection.Base_Pool.all,
            Addr,
            Storage_Size,
            Alignment);
      end if;
   end Deallocate;

   ------------
   -- Detach --
   ------------

   procedure Detach (N : Node_Ptr) is
   begin
      Lock_Task.all;

      if N.Prev /= null
        and then N.Next /= null
      then
         N.Prev.Next := N.Next;
         N.Next.Prev := N.Prev;
         N.Prev := null;
         N.Next := null;
      end if;

      Unlock_Task.all;

   exception
      when others =>
         Unlock_Task.all;
         raise;
   end Detach;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Collection : in out Finalization_Collection)
   is
      function Head (L : Node_Ptr) return Node_Ptr;
      --  Return the node which comes after the dummy head

      function Is_Dummy_Head (N : Node_Ptr) return Boolean;
      --  Determine whether a node acts as a dummy head. Such nodes do not
      --  have an actual "object" attached to them and point to themselves.

      function Is_Empty_List (L : Node_Ptr) return Boolean;
      --  Determine whether a list is empty

      function Node_Ptr_To_Address (N : Node_Ptr) return Address;
      --  Not the reverse of Address_To_Node_Ptr. Return the address of the
      --  object following the list header.

      ----------
      -- Head --
      ----------

      function Head (L : Node_Ptr) return Node_Ptr is
      begin
         return L.Next;
      end Head;

      -------------------
      -- Is_Dummy_Head --
      -------------------

      function Is_Dummy_Head (N : Node_Ptr) return Boolean is
      begin
         --  To be a dummy head, the node must point to itself in both
         --  directions.

         return
           N.Next /= null
             and then N.Next = N
             and then N.Prev /= null
             and then N.Prev = N;
      end Is_Dummy_Head;

      -------------------
      -- Is_Empty_List --
      -------------------

      function Is_Empty_List (L : Node_Ptr) return Boolean is
      begin
         return L = null or else Is_Dummy_Head (L);
      end Is_Empty_List;

      -------------------------
      -- Node_Ptr_To_Address --
      -------------------------

      function Node_Ptr_To_Address (N : Node_Ptr) return Address is
      begin
         return N.all'Address + Header_Offset;
      end Node_Ptr_To_Address;

      Curr_Ptr : Node_Ptr;
      Ex_Occur : Exception_Occurrence;
      Next_Ptr : Node_Ptr;
      Raised   : Boolean := False;

   --  Start of processing for Finalize

   begin
      --  Lock the collection to prevent any allocations while the objects are
      --  being finalized. The collection remains locked because the associated
      --  access type is about to go out of scope.

      Collection.Finalization_Started := True;

      while not Is_Empty_List (Collection.Objects) loop

         --  Find the real head of the collection, skipping the dummy head

         Curr_Ptr := Head (Collection.Objects);

         --  If the dummy head is the only remaining node, all real objects
         --  have already been detached and finalized.

         if Is_Dummy_Head (Curr_Ptr) then
            exit;
         end if;

         --  Store the next node now since the detachment will destroy the
         --  reference to it.

         Next_Ptr := Curr_Ptr.Next;

         --  Remove the current node from the list

         Detach (Curr_Ptr);

         --  ??? Kludge: Don't do anything until the proper place to set
         --  primitive Finalize_Address has been determined.

         if Collection.Finalize_Address /= null then
            begin
               Collection.Finalize_Address (Node_Ptr_To_Address (Curr_Ptr));

            exception
               when Fin_Except : others =>
                  if not Raised then
                     Raised := True;
                     Save_Occurrence (Ex_Occur, Fin_Except);
                  end if;
            end;
         end if;

         Curr_Ptr := Next_Ptr;
      end loop;

      --  Deallocate the dummy head

      Free (Collection.Objects);

      --  If the finalization of a particular node raised an exception, reraise
      --  it after the remainder of the list has been finalized.

      if Raised then
         Reraise_Occurrence (Ex_Occur);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Collection : in out Finalization_Collection)
   is
   begin
      Collection.Objects := new Node;

      --  The dummy head must point to itself in both directions

      Collection.Objects.Next := Collection.Objects;
      Collection.Objects.Prev := Collection.Objects;
   end Initialize;

   ----------
   -- pcol --
   ----------

   procedure pcol (Collection : Finalization_Collection) is
      Head_Seen : Boolean := False;
      N_Ptr     : Node_Ptr;

   begin
      --  Output the basic contents of the collection

      --    Collection: 0x123456789
      --    Base_Pool : null <or> 0x123456789
      --    Fin_Addr  : null <or> 0x123456789
      --    Fin_Start : TRUE <or> FALSE

      Put ("Collection: ");
      Put_Line (Address_Image (Collection'Address));

      Put ("Base_Pool : ");
      if Collection.Base_Pool = null then
         Put_Line (" null");
      else
         Put_Line (Address_Image (Collection.Base_Pool'Address));
      end if;

      Put ("Fin_Addr  : ");
      if Collection.Finalize_Address = null then
         Put_Line ("null");
      else
         Put_Line (Address_Image (Collection.Finalize_Address'Address));
      end if;

      Put ("Fin_Start : ");
      Put_Line (Collection.Finalization_Started'Img);

      --  Output all chained elements. The format is the following:

      --    ^ <or> ? <or> null
      --    |Header: 0x123456789 (dummy head)
      --    |  Prev: 0x123456789
      --    |  Next: 0x123456789
      --    V

      --  ^ - the current element points back to the correct element
      --  ? - the current element points back to an erroneous element
      --  n - the current element points back to null

      --  Header - the address of the list header
      --  Prev   - the address of the list header which the current element
      --         - points back to
      --  Next   - the address of the list header which the current element
      --         - points to
      --  (dummy head) - present if dummy head

      N_Ptr := Collection.Objects;

      while N_Ptr /= null loop
         Put_Line ("V");

         --  The current node is the head. If we have already traversed the
         --  chain, the head will be encountered again since the chain is
         --  circular.

         if N_Ptr = Collection.Objects then
            if Head_Seen then
               exit;
            else
               Head_Seen := True;
            end if;
         end if;

         --  The current element points back to null. This should never happen
         --  since the list is circular.

         if N_Ptr.Prev = null then
            Put_Line ("null (ERROR)");

         --  The current element points back to the correct element

         elsif N_Ptr.Prev.Next = N_Ptr then
            Put_Line ("^");

         --  The current element points back to an erroneous element

         else
            Put_Line ("? (ERROR)");
         end if;

         --  Output the header and fields

         Put ("|Header: ");
         Put (Address_Image (N_Ptr.all'Address));

         --  Detect the dummy head

         if N_Ptr = Collection.Objects then
            Put_Line (" (dummy head)");
         else
            Put_Line ("");
         end if;

         Put ("|  Prev: ");
         if N_Ptr.Prev = null then
            Put_Line ("null");
         else
            Put_Line (Address_Image (N_Ptr.Prev.all'Address));
         end if;

         Put ("|  Next: ");
         if N_Ptr.Next = null then
            Put_Line ("null");
         else
            Put_Line (Address_Image (N_Ptr.Next.all'Address));
         end if;

         N_Ptr := N_Ptr.Next;
      end loop;
   end pcol;

   ------------------------------
   -- Set_Finalize_Address_Ptr --
   ------------------------------

   procedure Set_Finalize_Address_Ptr
     (Collection : in out Finalization_Collection;
      Proc_Ptr   : Finalize_Address_Ptr)
   is
   begin
      Collection.Finalize_Address := Proc_Ptr;
   end Set_Finalize_Address_Ptr;

   --------------------------
   -- Set_Storage_Pool_Ptr --
   --------------------------

   procedure Set_Storage_Pool_Ptr
     (Collection : in out Finalization_Collection;
      Pool_Ptr   : Any_Storage_Pool_Ptr)
   is
   begin
      Collection.Base_Pool := Pool_Ptr;
   end Set_Storage_Pool_Ptr;

end Ada.Finalization.Heap_Management;
