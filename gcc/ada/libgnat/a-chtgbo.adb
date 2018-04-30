------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           ADA.CONTAINERS.HASH_TABLES.GENERIC_BOUNDED_OPERATIONS          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2018, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with System; use type System.Address;

package body Ada.Containers.Hash_Tables.Generic_Bounded_Operations is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -------------------
   -- Checked_Index --
   -------------------

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type'Class;
      Node       : Count_Type) return Hash_Type
   is
      Lock : With_Lock (Hash_Table.TC'Unrestricted_Access);
   begin
      return Index (Hash_Table, Hash_Table.Nodes (Node));
   end Checked_Index;

   -----------
   -- Clear --
   -----------

   procedure Clear (HT : in out Hash_Table_Type'Class) is
   begin
      TC_Check (HT.TC);

      HT.Length := 0;
      --  HT.Busy := 0;
      --  HT.Lock := 0;
      HT.Free := -1;
      HT.Buckets := (others => 0);  -- optimize this somehow ???
   end Clear;

   --------------------------
   -- Delete_Node_At_Index --
   --------------------------

   procedure Delete_Node_At_Index
     (HT   : in out Hash_Table_Type'Class;
      Indx : Hash_Type;
      X    : Count_Type)
   is
      Prev : Count_Type;
      Curr : Count_Type;

   begin
      Prev := HT.Buckets (Indx);

      if Checks and then Prev = 0 then
         raise Program_Error with
           "attempt to delete node from empty hash bucket";
      end if;

      if Prev = X then
         HT.Buckets (Indx) := Next (HT.Nodes (Prev));
         HT.Length := HT.Length - 1;
         return;
      end if;

      if Checks and then HT.Length = 1 then
         raise Program_Error with
           "attempt to delete node not in its proper hash bucket";
      end if;

      loop
         Curr := Next (HT.Nodes (Prev));

         if Checks and then Curr = 0 then
            raise Program_Error with
              "attempt to delete node not in its proper hash bucket";
         end if;

         Prev := Curr;
      end loop;
   end Delete_Node_At_Index;

   ---------------------------
   -- Delete_Node_Sans_Free --
   ---------------------------

   procedure Delete_Node_Sans_Free
     (HT : in out Hash_Table_Type'Class;
      X  : Count_Type)
   is
      pragma Assert (X /= 0);

      Indx : Hash_Type;
      Prev : Count_Type;
      Curr : Count_Type;

   begin
      if Checks and then HT.Length = 0 then
         raise Program_Error with
           "attempt to delete node from empty hashed container";
      end if;

      Indx := Checked_Index (HT, X);
      Prev := HT.Buckets (Indx);

      if Checks and then Prev = 0 then
         raise Program_Error with
           "attempt to delete node from empty hash bucket";
      end if;

      if Prev = X then
         HT.Buckets (Indx) := Next (HT.Nodes (Prev));
         HT.Length := HT.Length - 1;
         return;
      end if;

      if Checks and then HT.Length = 1 then
         raise Program_Error with
           "attempt to delete node not in its proper hash bucket";
      end if;

      loop
         Curr := Next (HT.Nodes (Prev));

         if Checks and then Curr = 0 then
            raise Program_Error with
              "attempt to delete node not in its proper hash bucket";
         end if;

         if Curr = X then
            Set_Next (HT.Nodes (Prev), Next => Next (HT.Nodes (Curr)));
            HT.Length := HT.Length - 1;
            return;
         end if;

         Prev := Curr;
      end loop;
   end Delete_Node_Sans_Free;

   -----------
   -- First --
   -----------

   function First (HT : Hash_Table_Type'Class) return Count_Type is
      Indx : Hash_Type;

   begin
      if HT.Length = 0 then
         return 0;
      end if;

      Indx := HT.Buckets'First;
      loop
         if HT.Buckets (Indx) /= 0 then
            return HT.Buckets (Indx);
         end if;

         Indx := Indx + 1;
      end loop;
   end First;

   ----------
   -- Free --
   ----------

   procedure Free
     (HT : in out Hash_Table_Type'Class;
      X  : Count_Type)
   is
      N : Nodes_Type renames HT.Nodes;

   begin
      --  This subprogram "deallocates" a node by relinking the node off of the
      --  active list and onto the free list. Previously it would flag index
      --  value 0 as an error. The precondition was weakened, so that index
      --  value 0 is now allowed, and this value is interpreted to mean "do
      --  nothing". This makes its behavior analogous to the behavior of
      --  Ada.Unchecked_Deallocation, and allows callers to avoid having to add
      --  special-case checks at the point of call.

      if X = 0 then
         return;
      end if;

      pragma Assert (X <= HT.Capacity);

      --  pragma Assert (N (X).Prev >= 0);  -- node is active
      --  Find a way to mark a node as active vs. inactive; we could
      --  use a special value in Color_Type for this.  ???

      --  The hash table actually contains two data structures: a list for
      --  the "active" nodes that contain elements that have been inserted
      --  onto the container, and another for the "inactive" nodes of the free
      --  store.
      --
      --  We desire that merely declaring an object should have only minimal
      --  cost; specially, we want to avoid having to initialize the free
      --  store (to fill in the links), especially if the capacity is large.
      --
      --  The head of the free list is indicated by Container.Free. If its
      --  value is non-negative, then the free store has been initialized
      --  in the "normal" way: Container.Free points to the head of the list
      --  of free (inactive) nodes, and the value 0 means the free list is
      --  empty. Each node on the free list has been initialized to point
      --  to the next free node (via its Parent component), and the value 0
      --  means that this is the last free node.
      --
      --  If Container.Free is negative, then the links on the free store
      --  have not been initialized. In this case the link values are
      --  implied: the free store comprises the components of the node array
      --  started with the absolute value of Container.Free, and continuing
      --  until the end of the array (Nodes'Last).
      --
      --  ???
      --  It might be possible to perform an optimization here. Suppose that
      --  the free store can be represented as having two parts: one
      --  comprising the non-contiguous inactive nodes linked together
      --  in the normal way, and the other comprising the contiguous
      --  inactive nodes (that are not linked together, at the end of the
      --  nodes array). This would allow us to never have to initialize
      --  the free store, except in a lazy way as nodes become inactive.

      --  When an element is deleted from the list container, its node
      --  becomes inactive, and so we set its Next component to value of
      --  the node's index (in the nodes array), to indicate that it is
      --  now inactive. This provides a useful way to detect a dangling
      --  cursor reference.  ???

      Set_Next (N (X), Next => X);  -- Node is deallocated (not on active list)

      if HT.Free >= 0 then
         --  The free store has previously been initialized. All we need to
         --  do here is link the newly-free'd node onto the free list.

         Set_Next (N (X), HT.Free);
         HT.Free := X;

      elsif X + 1 = abs HT.Free then
         --  The free store has not been initialized, and the node becoming
         --  inactive immediately precedes the start of the free store. All
         --  we need to do is move the start of the free store back by one.

         HT.Free := HT.Free + 1;

      else
         --  The free store has not been initialized, and the node becoming
         --  inactive does not immediately precede the free store. Here we
         --  first initialize the free store (meaning the links are given
         --  values in the traditional way), and then link the newly-free'd
         --  node onto the head of the free store.

         --  ???
         --  See the comments above for an optimization opportunity. If
         --  the next link for a node on the free store is negative, then
         --  this means the remaining nodes on the free store are
         --  physically contiguous, starting as the absolute value of
         --  that index value.

         HT.Free := abs HT.Free;

         if HT.Free > HT.Capacity then
            HT.Free := 0;

         else
            for I in HT.Free .. HT.Capacity - 1 loop
               Set_Next (Node => N (I), Next => I + 1);
            end loop;

            Set_Next (Node => N (HT.Capacity), Next => 0);
         end if;

         Set_Next (Node => N (X), Next => HT.Free);
         HT.Free := X;
      end if;
   end Free;

   ----------------------
   -- Generic_Allocate --
   ----------------------

   procedure Generic_Allocate
     (HT   : in out Hash_Table_Type'Class;
      Node : out Count_Type)
   is
      N : Nodes_Type renames HT.Nodes;

   begin
      if HT.Free >= 0 then
         Node := HT.Free;

         --  We always perform the assignment first, before we
         --  change container state, in order to defend against
         --  exceptions duration assignment.

         Set_Element (N (Node));
         HT.Free := Next (N (Node));

      else
         --  A negative free store value means that the links of the nodes
         --  in the free store have not been initialized. In this case, the
         --  nodes are physically contiguous in the array, starting at the
         --  index that is the absolute value of the Container.Free, and
         --  continuing until the end of the array (Nodes'Last).

         Node := abs HT.Free;

         --  As above, we perform this assignment first, before modifying
         --  any container state.

         Set_Element (N (Node));
         HT.Free := HT.Free - 1;
      end if;
   end Generic_Allocate;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal
     (L, R : Hash_Table_Type'Class) return Boolean
   is
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      Lock_L : With_Lock (L.TC'Unrestricted_Access);
      Lock_R : With_Lock (R.TC'Unrestricted_Access);

      L_Index : Hash_Type;
      L_Node  : Count_Type;

      N : Count_Type;

   begin
      if L'Address = R'Address then
         return True;
      end if;

      if L.Length /= R.Length then
         return False;
      end if;

      if L.Length = 0 then
         return True;
      end if;

      --  Find the first node of hash table L

      L_Index := L.Buckets'First;
      loop
         L_Node := L.Buckets (L_Index);
         exit when L_Node /= 0;
         L_Index := L_Index + 1;
      end loop;

      --  For each node of hash table L, search for an equivalent node in hash
      --  table R.

      N := L.Length;
      loop
         if not Find (HT => R, Key => L.Nodes (L_Node)) then
            return False;
         end if;

         N := N - 1;

         L_Node := Next (L.Nodes (L_Node));

         if L_Node = 0 then

            --  We have exhausted the nodes in this bucket

            if N = 0 then
               return True;
            end if;

            --  Find the next bucket

            loop
               L_Index := L_Index + 1;
               L_Node := L.Buckets (L_Index);
               exit when L_Node /= 0;
            end loop;
         end if;
      end loop;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (HT : Hash_Table_Type'Class) is
      Node : Count_Type;

   begin
      if HT.Length = 0 then
         return;
      end if;

      for Indx in HT.Buckets'Range loop
         Node := HT.Buckets (Indx);
         while Node /= 0 loop
            Process (Node);
            Node := Next (HT.Nodes (Node));
         end loop;
      end loop;
   end Generic_Iteration;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type'Class)
   is
      N  : Count_Type'Base;

   begin
      Clear (HT);

      Count_Type'Base'Read (Stream, N);

      if Checks and then N < 0 then
         raise Program_Error with "stream appears to be corrupt";
      end if;

      if N = 0 then
         return;
      end if;

      if Checks and then N > HT.Capacity then
         raise Capacity_Error with "too many elements in stream";
      end if;

      for J in 1 .. N loop
         declare
            Node : constant Count_Type := New_Node (Stream);
            Indx : constant Hash_Type := Checked_Index (HT, Node);
            B    : Count_Type renames HT.Buckets (Indx);
         begin
            Set_Next (HT.Nodes (Node), Next => B);
            B := Node;
         end;

         HT.Length := HT.Length + 1;
      end loop;
   end Generic_Read;

   -------------------
   -- Generic_Write --
   -------------------

   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      HT     : Hash_Table_Type'Class)
   is
      procedure Write (Node : Count_Type);
      pragma Inline (Write);

      procedure Write is new Generic_Iteration (Write);

      -----------
      -- Write --
      -----------

      procedure Write (Node : Count_Type) is
      begin
         Write (Stream, HT.Nodes (Node));
      end Write;

   begin
      Count_Type'Base'Write (Stream, HT.Length);
      Write (HT);
   end Generic_Write;

   -----------
   -- Index --
   -----------

   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Type) return Hash_Type is
   begin
      return Buckets'First + Hash_Node (Node) mod Buckets'Length;
   end Index;

   function Index
     (HT   : Hash_Table_Type'Class;
      Node : Node_Type) return Hash_Type is
   begin
      return Index (HT.Buckets, Node);
   end Index;

   ----------
   -- Next --
   ----------

   function Next
     (HT   : Hash_Table_Type'Class;
      Node : Count_Type) return Count_Type
   is
      Result : Count_Type;
      First  : Hash_Type;

   begin
      Result := Next (HT.Nodes (Node));

      if Result /= 0 then  -- another node in same bucket
         return Result;
      end if;

      --  This was the last node in the bucket, so move to the next
      --  bucket, and start searching for next node from there.

      First := Checked_Index (HT'Unrestricted_Access.all, Node) + 1;
      for Indx in First .. HT.Buckets'Last loop
         Result := HT.Buckets (Indx);

         if Result /= 0 then  -- bucket is not empty
            return Result;
         end if;
      end loop;

      return 0;
   end Next;

end Ada.Containers.Hash_Tables.Generic_Bounded_Operations;
