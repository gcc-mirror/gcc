------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.HASH_TABLES.GENERIC_OPERATIONS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2025, Free Software Foundation, Inc.         --
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

with Ada.Containers.Prime_Numbers;
with Ada.Unchecked_Deallocation;

with System; use type System.Address;

package body Ada.Containers.Hash_Tables.Generic_Operations is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   type Buckets_Allocation is access all Buckets_Type;
   --  Used for allocation and deallocation (see New_Buckets and Free_Buckets).
   --  This is necessary because Buckets_Access has an empty storage pool.

   ------------
   -- Adjust --
   ------------

   procedure Adjust (HT : in out Hash_Table_Type) is
      Src_Buckets : constant Buckets_Access := HT.Buckets;
      N           : constant Count_Type := HT.Length;
      Src_Node    : Node_Access;
      Dst_Prev    : Node_Access;

   begin
      --  If the counts are nonzero, execution is technically erroneous, but
      --  it seems friendly to allow things like concurrent "=" on shared
      --  constants.

      Zero_Counts (HT.TC);

      HT.Buckets := null;
      HT.Length := 0;

      if N = 0 then
         return;
      end if;

      --  Technically it isn't necessary to allocate the exact same length
      --  buckets array, because our only requirement is that following
      --  assignment the source and target containers compare equal (that is,
      --  operator "=" returns True). We can satisfy this requirement with any
      --  hash table length, but we decide here to match the length of the
      --  source table. This has the benefit that when iterating, elements of
      --  the target are delivered in the exact same order as for the source.

      HT.Buckets := New_Buckets (Length => Src_Buckets'Length);

      for Src_Index in Src_Buckets'Range loop
         Src_Node := Src_Buckets (Src_Index);

         if Src_Node /= null then
            declare
               Dst_Node : constant Node_Access := Copy_Node (Src_Node);

               --  See note above

               pragma Assert (Checked_Index (HT, Dst_Node) = Src_Index);

            begin
               HT.Buckets (Src_Index) := Dst_Node;
               HT.Length := HT.Length + 1;

               Dst_Prev := Dst_Node;
            end;

            Src_Node := Next (Src_Node);
            while Src_Node /= null loop
               declare
                  Dst_Node : constant Node_Access := Copy_Node (Src_Node);

                  --  See note above

                  pragma Assert (Checked_Index (HT, Dst_Node) = Src_Index);

               begin
                  Set_Next (Node => Dst_Prev, Next => Dst_Node);
                  HT.Length := HT.Length + 1;

                  Dst_Prev := Dst_Node;
               end;

               Src_Node := Next (Src_Node);
            end loop;
         end if;
      end loop;

      pragma Assert (HT.Length = N);
   end Adjust;

   --------------
   -- Capacity --
   --------------

   function Capacity (HT : Hash_Table_Type) return Count_Type is
   begin
      if HT.Buckets = null then
         return 0;
      end if;

      return HT.Buckets'Length;
   end Capacity;

   -------------------
   -- Checked_Index --
   -------------------

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type;
      Buckets    : Buckets_Type;
      Node       : Node_Access) return Hash_Type
   is
      Lock : With_Lock (Hash_Table.TC'Unrestricted_Access);
   begin
      return Index (Buckets, Node);
   end Checked_Index;

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type;
      Node       : Node_Access) return Hash_Type
   is
   begin
      return Checked_Index (Hash_Table, Hash_Table.Buckets.all, Node);
   end Checked_Index;

   -----------
   -- Clear --
   -----------

   procedure Clear (HT : in out Hash_Table_Type) is
      Index : Hash_Type := 0;
      Node  : Node_Access;

   begin
      TC_Check (HT.TC);

      while HT.Length > 0 loop
         while HT.Buckets (Index) = null loop
            Index := Index + 1;
         end loop;

         declare
            Bucket : Node_Access renames HT.Buckets (Index);
         begin
            loop
               Node := Bucket;
               Bucket := Next (Bucket);
               HT.Length := HT.Length - 1;
               Free (Node);
               exit when Bucket = null;
            end loop;
         end;
      end loop;
   end Clear;

   --------------------------
   -- Delete_Node_At_Index --
   --------------------------

   procedure Delete_Node_At_Index
     (HT   : in out Hash_Table_Type;
      Indx : Hash_Type;
      X    : in out Node_Access)
   is
      Prev : Node_Access;
      Curr : Node_Access;

   begin
      Prev := HT.Buckets (Indx);

      if Prev = X then
         HT.Buckets (Indx) := Next (Prev);
         HT.Length := HT.Length - 1;
         Free (X);
         return;
      end if;

      if Checks and then HT.Length = 1 then
         raise Program_Error with
           "attempt to delete node not in its proper hash bucket";
      end if;

      loop
         Curr := Next (Prev);

         if Checks and then Curr = null then
            raise Program_Error with
              "attempt to delete node not in its proper hash bucket";
         end if;

         if Curr = X then
            Set_Next (Node => Prev, Next => Next (Curr));
            HT.Length := HT.Length - 1;
            Free (X);
            return;
         end if;

         Prev := Curr;
      end loop;
   end Delete_Node_At_Index;

   ---------------------------
   -- Delete_Node_Sans_Free --
   ---------------------------

   procedure Delete_Node_Sans_Free
     (HT : in out Hash_Table_Type;
      X  : Node_Access)
   is
      pragma Assert (X /= null);

      Indx : Hash_Type;
      Prev : Node_Access;
      Curr : Node_Access;

   begin
      if Checks and then HT.Length = 0 then
         raise Program_Error with
           "attempt to delete node from empty hashed container";
      end if;

      Indx := Checked_Index (HT, X);
      Prev := HT.Buckets (Indx);

      if Checks and then Prev = null then
         raise Program_Error with
           "attempt to delete node from empty hash bucket";
      end if;

      if Prev = X then
         HT.Buckets (Indx) := Next (Prev);
         HT.Length := HT.Length - 1;
         return;
      end if;

      if Checks and then HT.Length = 1 then
         raise Program_Error with
           "attempt to delete node not in its proper hash bucket";
      end if;

      loop
         Curr := Next (Prev);

         if Checks and then Curr = null then
            raise Program_Error with
              "attempt to delete node not in its proper hash bucket";
         end if;

         if Curr = X then
            Set_Next (Node => Prev, Next => Next (Curr));
            HT.Length := HT.Length - 1;
            return;
         end if;

         Prev := Curr;
      end loop;
   end Delete_Node_Sans_Free;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (HT : in out Hash_Table_Type) is
   begin
      Clear (HT);
      Free_Buckets (HT.Buckets);
   end Finalize;

   -----------
   -- First --
   -----------

   function First
     (HT       : Hash_Table_Type) return Node_Access
   is
      Dummy : Hash_Type;
   begin
      return First (HT, Dummy);
   end First;

   function First
     (HT       : Hash_Table_Type;
      Position : out Hash_Type) return Node_Access is
   begin
      if HT.Length = 0 then
         Position := Hash_Type'Last;
         return null;
      end if;

      Position := HT.Buckets'First;
      loop
         if HT.Buckets (Position) /= null then
            return HT.Buckets (Position);
         end if;

         Position := Position + 1;
      end loop;
   end First;

   ------------------
   -- Free_Buckets --
   ------------------

   procedure Free_Buckets (Buckets : in out Buckets_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Allocation);

   begin
      --  Buckets must have been created by New_Buckets. Here, we convert back
      --  to the Buckets_Allocation type, and do the free on that.

      Free (Buckets_Allocation (Buckets));
   end Free_Buckets;

   ---------------------
   -- Free_Hash_Table --
   ---------------------

   procedure Free_Hash_Table (Buckets : in out Buckets_Access) is
      Node : Node_Access;

   begin
      if Buckets = null then
         return;
      end if;

      for J in Buckets'Range loop
         while Buckets (J) /= null loop
            Node := Buckets (J);
            Buckets (J) := Next (Node);
            Free (Node);
         end loop;
      end loop;

      Free_Buckets (Buckets);
   end Free_Hash_Table;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean
   is
   begin
      if L.Length /= R.Length then
         return False;
      end if;

      if L.Length = 0 then
         return True;
      end if;

      declare
         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         Lock_L : With_Lock (L.TC'Unrestricted_Access);
         Lock_R : With_Lock (R.TC'Unrestricted_Access);

         L_Index : Hash_Type;
         L_Node  : Node_Access;

         N : Count_Type;
      begin
         --  Find the first node of hash table L

         L_Index := 0;
         loop
            L_Node := L.Buckets (L_Index);
            exit when L_Node /= null;
            L_Index := L_Index + 1;
         end loop;

         --  For each node of hash table L, search for an equivalent node in
         --  hash table R.

         N := L.Length;
         loop
            if not Find (HT => R, Key => L_Node) then
               return False;
            end if;

            N := N - 1;

            L_Node := Next (L_Node);

            if L_Node = null then
               --  We have exhausted the nodes in this bucket

               if N = 0 then
                  return True;
               end if;

               --  Find the next bucket

               loop
                  L_Index := L_Index + 1;
                  L_Node := L.Buckets (L_Index);
                  exit when L_Node /= null;
               end loop;
            end if;
         end loop;
      end;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (HT : Hash_Table_Type) is
      procedure Wrapper (Node : Node_Access; Dummy_Pos : Hash_Type);

      -------------
      -- Wrapper --
      -------------

      procedure Wrapper (Node : Node_Access; Dummy_Pos : Hash_Type) is
      begin
         Process (Node);
      end Wrapper;

      procedure Internal_With_Pos is
        new Generic_Iteration_With_Position (Wrapper);

   --  Start of processing for Generic_Iteration

   begin
      Internal_With_Pos (HT);
   end Generic_Iteration;

   -------------------------------------
   -- Generic_Iteration_With_Position --
   -------------------------------------

   procedure Generic_Iteration_With_Position
     (HT : Hash_Table_Type)
   is
      Node : Node_Access;

   begin
      if HT.Length = 0 then
         return;
      end if;

      for Indx in HT.Buckets'Range loop
         Node := HT.Buckets (Indx);
         while Node /= null loop
            Process (Node, Indx);
            Node := Next (Node);
         end loop;
      end loop;
   end Generic_Iteration_With_Position;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type)
   is
      N  : Count_Type'Base;
      NN : Hash_Type;

   begin
      Clear (HT);

      Count_Type'Base'Read (Stream, N);

      if Checks and then N < 0 then
         raise Program_Error with "stream appears to be corrupt";
      end if;

      if N = 0 then
         return;
      end if;

      --  The RM does not specify whether or how the capacity changes when a
      --  hash table is streamed in. Therefore we decide here to allocate a new
      --  buckets array only when it's necessary to preserve representation
      --  invariants.

      if HT.Buckets = null
        or else HT.Buckets'Length < N
      then
         Free_Buckets (HT.Buckets);
         NN := Prime_Numbers.To_Prime (N);
         HT.Buckets := New_Buckets (Length => NN);
      end if;

      for J in 1 .. N loop
         declare
            Node : constant Node_Access := New_Node (Stream);
            Indx : constant Hash_Type := Checked_Index (HT, Node);
            B    : Node_Access renames HT.Buckets (Indx);
         begin
            Set_Next (Node => Node, Next => B);
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
      HT     : Hash_Table_Type)
   is
      procedure Write (Node : Node_Access);
      pragma Inline (Write);

      procedure Write is new Generic_Iteration (Write);

      -----------
      -- Write --
      -----------

      procedure Write (Node : Node_Access) is
      begin
         Write (Stream, Node);
      end Write;

   begin
      --  See Generic_Read for an explanation of why we do not stream out the
      --  buckets array length too.

      Count_Type'Base'Write (Stream, HT.Length);
      Write (HT);
   end Generic_Write;

   -----------
   -- Index --
   -----------

   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Access) return Hash_Type is
   begin
      return Hash_Node (Node) mod Buckets'Length;
   end Index;

   function Index
     (Hash_Table : Hash_Table_Type;
      Node       : Node_Access) return Hash_Type is
   begin
      return Index (Hash_Table.Buckets.all, Node);
   end Index;

   ----------
   -- Move --
   ----------

   procedure Move (Target, Source : in out Hash_Table_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      TC_Check (Source.TC);

      Clear (Target);

      declare
         Buckets : constant Buckets_Access := Target.Buckets;
      begin
         Target.Buckets := Source.Buckets;
         Source.Buckets := Buckets;
      end;

      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

   -----------------
   -- New_Buckets --
   -----------------

   function New_Buckets (Length : Hash_Type) return Buckets_Access is
      subtype Rng is Hash_Type range 0 .. Length - 1;

   begin
      --  Allocate in Buckets_Allocation'Storage_Pool, then convert to
      --  Buckets_Access.

      return Buckets_Access (Buckets_Allocation'(new Buckets_Type (Rng)));
   end New_Buckets;

   ----------
   -- Next --
   ----------

   function Next
     (HT            : aliased in out Hash_Table_Type;
      Node          : Node_Access;
      Position : in out Hash_Type) return Node_Access
   is
      Result : Node_Access;
      First  : Hash_Type;

   begin
      --  First, check if the node has other nodes chained to it
      Result := Next (Node);

      if Result /= null then
         return Result;
      end if;

      --  Check if we were supplied a position for Node, from which we
      --  can start iteration on the buckets.

      if Position /= Hash_Type'Last then
         First := Position + 1;
      else
         First := Checked_Index (HT, Node) + 1;
      end if;

      for Indx in First .. HT.Buckets'Last loop
         Result := HT.Buckets (Indx);

         if Result /= null then
            Position := Indx;
            return Result;
         end if;
      end loop;

      return null;
   end Next;

   function Next
     (HT            : aliased in out Hash_Table_Type;
      Node          : Node_Access) return Node_Access
   is
      Pos : Hash_Type := Hash_Type'Last;
   begin
      return Next (HT, Node, Pos);
   end Next;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (HT : in out Hash_Table_Type;
      N  : Count_Type)
   is
      NN : Hash_Type;

   begin
      if HT.Buckets = null then
         if N > 0 then
            NN := Prime_Numbers.To_Prime (N);
            HT.Buckets := New_Buckets (Length => NN);
         end if;

         return;
      end if;

      if HT.Length = 0 then

         --  This is the easy case. There are no nodes, so no rehashing is
         --  necessary. All we need to do is allocate a new buckets array
         --  having a length implied by the specified capacity. (We say
         --  "implied by" because bucket arrays are always allocated with a
         --  length that corresponds to a prime number.)

         if N = 0 then
            Free_Buckets (HT.Buckets);
            return;
         end if;

         if N = HT.Buckets'Length then
            return;
         end if;

         NN := Prime_Numbers.To_Prime (N);

         if NN = HT.Buckets'Length then
            return;
         end if;

         declare
            X : Buckets_Access := HT.Buckets;
            pragma Warnings (Off, X);
         begin
            HT.Buckets := New_Buckets (Length => NN);
            Free_Buckets (X);
         end;

         return;
      end if;

      if N = HT.Buckets'Length then
         return;
      end if;

      if N < HT.Buckets'Length then

         --  This is a request to contract the buckets array. The amount of
         --  contraction is bounded in order to preserve the invariant that the
         --  buckets array length is never smaller than the number of elements
         --  (the load factor is 1).

         if HT.Length >= HT.Buckets'Length then
            return;
         end if;

         NN := Prime_Numbers.To_Prime (HT.Length);

         if NN >= HT.Buckets'Length then
            return;
         end if;

      else
         NN := Prime_Numbers.To_Prime (Count_Type'Max (N, HT.Length));

         if NN = HT.Buckets'Length then -- can't expand any more
            return;
         end if;
      end if;

      TC_Check (HT.TC);

      Rehash : declare
         Dst_Buckets : Buckets_Access := New_Buckets (Length => NN);
         Src_Buckets : Buckets_Access := HT.Buckets;
         pragma Warnings (Off, Src_Buckets);

         L : Count_Type renames HT.Length;
         LL : constant Count_Type := L;

         Src_Index : Hash_Type := Src_Buckets'First;

      begin
         while L > 0 loop
            declare
               Src_Bucket : Node_Access renames Src_Buckets (Src_Index);

            begin
               while Src_Bucket /= null loop
                  declare
                     Src_Node : constant Node_Access := Src_Bucket;

                     Dst_Index : constant Hash_Type :=
                       Checked_Index (HT, Dst_Buckets.all, Src_Node);

                     Dst_Bucket : Node_Access renames Dst_Buckets (Dst_Index);

                  begin
                     Src_Bucket := Next (Src_Node);

                     Set_Next (Src_Node, Dst_Bucket);

                     Dst_Bucket := Src_Node;
                  end;

                  pragma Assert (L > 0);
                  L := L - 1;
               end loop;

            exception
               when others =>

                  --  If there's an error computing a hash value during a
                  --  rehash, then AI-302 says the nodes "become lost." The
                  --  issue is whether to actually deallocate these lost nodes,
                  --  since they might be designated by extant cursors. Here
                  --  we decide to deallocate the nodes, since it's better to
                  --  solve real problems (storage consumption) rather than
                  --  imaginary ones (the user might, or might not, dereference
                  --  a cursor designating a node that has been deallocated),
                  --  and because we have a way to vet a dangling cursor
                  --  reference anyway, and hence can actually detect the
                  --  problem.

                  for Dst_Index in Dst_Buckets'Range loop
                     declare
                        B : Node_Access renames Dst_Buckets (Dst_Index);
                        X : Node_Access;
                     begin
                        while B /= null loop
                           X := B;
                           B := Next (X);
                           Free (X);
                        end loop;
                     end;
                  end loop;

                  Free_Buckets (Dst_Buckets);
                  raise Program_Error with
                    "hash function raised exception during rehash";
            end;

            Src_Index := Src_Index + 1;
         end loop;

         HT.Buckets := Dst_Buckets;
         HT.Length := LL;

         Free_Buckets (Src_Buckets);
      end Rehash;
   end Reserve_Capacity;

end Ada.Containers.Hash_Tables.Generic_Operations;
