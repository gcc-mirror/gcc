------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       A D A . C O N T A I N E R S .                      --
--       H A S H _ T A B L E S . G E N E R I C _ O P E R A T I O N S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This body needs commenting ???

with Ada.Containers.Prime_Numbers;
with Ada.Unchecked_Deallocation;

with System;  use type System.Address;

package body Ada.Containers.Hash_Tables.Generic_Operations is

   procedure Free is
     new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (HT : in out Hash_Table_Type) is
      Src_Buckets : constant Buckets_Access := HT.Buckets;
      N           : constant Count_Type := HT.Length;
      Src_Node    : Node_Access;
      Dst_Prev    : Node_Access;

   begin
      HT.Buckets := null;
      HT.Length := 0;

      if N = 0 then
         return;
      end if;

      HT.Buckets := new Buckets_Type (Src_Buckets'Range);
      --  TODO: allocate minimum size req'd.  (See note below.)

      --  NOTE: see note below about these comments.
      --  Probably we have to duplicate the Size (Src), too, in order
      --  to guarantee that

      --    Dst := Src;
      --    Dst = Src is true

      --  The only quirk is that we depend on the hash value of a dst key
      --  to be the same as the src key from which it was copied.
      --  If we relax the requirement that the hash value must be the
      --  same, then of course we can't guarantee that following
      --  assignment that Dst = Src is true ???
      --
      --  NOTE: 17 Apr 2005
      --  What I said above is no longer true.  The semantics of (map) equality
      --  changed, such that we use key in the left map to look up the
      --  equivalent key in the right map, and then compare the elements (using
      --  normal equality) of the equivalent keys.  So it doesn't matter that
      --  the maps have different capacities (i.e. the hash tables have
      --  different lengths), since we just look up the key, irrespective of
      --  its map's hash table length.  All the RM says we're required to do
      --  it arrange for the target map to "=" the source map following an
      --  assignment (that is, following an Adjust), so it doesn't matter
      --  what the capacity of the target map is.  What I'll probably do is
      --  allocate a new hash table that has the minimum size necessary,
      --  instead of allocating a new hash table whose size exactly matches
      --  that of the source.  (See the assignment that immediately precedes
      --  these comments.)  What we really need is a special Assign operation
      --  (not unlike what we have already for Vector) that allows the user to
      --  choose the capacity of the target.
      --  END NOTE.

      for Src_Index in Src_Buckets'Range loop
         Src_Node := Src_Buckets (Src_Index);

         if Src_Node /= null then
            declare
               Dst_Node : constant Node_Access := Copy_Node (Src_Node);

               --   See note above

               pragma Assert (Index (HT, Dst_Node) = Src_Index);

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

                  pragma Assert (Index (HT, Dst_Node) = Src_Index);

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

   -----------
   -- Clear --
   -----------

   procedure Clear (HT : in out Hash_Table_Type) is
      Index : Hash_Type := 0;
      Node  : Node_Access;

   begin
      if HT.Busy > 0 then
         raise Program_Error;
      end if;

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
      if HT.Length = 0 then
         raise Program_Error;
      end if;

      Indx := Index (HT, X);
      Prev := HT.Buckets (Indx);

      if Prev = null then
         raise Program_Error;
      end if;

      if Prev = X then
         HT.Buckets (Indx) := Next (Prev);
         HT.Length := HT.Length - 1;
         return;
      end if;

      if HT.Length = 1 then
         raise Program_Error;
      end if;

      loop
         Curr := Next (Prev);

         if Curr = null then
            raise Program_Error;
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
      Free (HT.Buckets);
   end Finalize;

   -----------
   -- First --
   -----------

   function First (HT : Hash_Table_Type) return Node_Access is
      Indx : Hash_Type;

   begin
      if HT.Length = 0 then
         return null;
      end if;

      Indx := HT.Buckets'First;
      loop
         if HT.Buckets (Indx) /= null then
            return HT.Buckets (Indx);
         end if;

         Indx := Indx + 1;
      end loop;
   end First;

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

      Free (Buckets);
   end Free_Hash_Table;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean is

      L_Index : Hash_Type;
      L_Node  : Node_Access;

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

      L_Index := 0;

      loop
         L_Node := L.Buckets (L_Index);
         exit when L_Node /= null;
         L_Index := L_Index + 1;
      end loop;

      N := L.Length;

      loop
         if not Find (HT => R, Key => L_Node) then
            return False;
         end if;

         N := N - 1;

         L_Node := Next (L_Node);

         if L_Node = null then
            if N = 0 then
               return True;
            end if;

            loop
               L_Index := L_Index + 1;
               L_Node := L.Buckets (L_Index);
               exit when L_Node /= null;
            end loop;
         end if;
      end loop;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (HT : Hash_Table_Type) is
      Busy : Natural renames HT'Unrestricted_Access.all.Busy;

   begin
      if HT.Length = 0 then
         return;
      end if;

      Busy := Busy + 1;

      declare
         Node : Node_Access;
      begin
         for Indx in HT.Buckets'Range loop
            Node := HT.Buckets (Indx);
            while Node /= null loop
               Process (Node);
               Node := Next (Node);
            end loop;
         end loop;
      exception
         when others =>
            Busy := Busy - 1;
            raise;
      end;

      Busy := Busy - 1;
   end Generic_Iteration;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (Stream : access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type)
   is
      X, Y : Node_Access;

      Last, I : Hash_Type;
      N, M    : Count_Type'Base;

   begin
      Clear (HT);

      Hash_Type'Read (Stream, Last);

      Count_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      if N = 0 then
         return;
      end if;

      if HT.Buckets = null
        or else HT.Buckets'Last /= Last
      then
         Free (HT.Buckets);
         HT.Buckets := new Buckets_Type (0 .. Last);
      end if;

      --  TODO: should we rewrite this algorithm so that it doesn't
      --  depend on preserving the exactly length of the hash table
      --  array?  We would prefer to not have to (re)allocate a
      --  buckets array (the array that HT already has might be large
      --  enough), and to not have to stream the count of the number
      --  of nodes in each bucket.  The algorithm below is vestigial,
      --  as it was written prior to the meeting in Palma, when the
      --  semantics of equality were changed (and which obviated the
      --  need to preserve the hash table length).

      loop
         Hash_Type'Read (Stream, I);
         pragma Assert (I in HT.Buckets'Range);
         pragma Assert (HT.Buckets (I) = null);

         Count_Type'Base'Read (Stream, M);
         pragma Assert (M >= 1);
         pragma Assert (M <= N);

         HT.Buckets (I) := New_Node (Stream);
         pragma Assert (HT.Buckets (I) /= null);
         pragma Assert (Next (HT.Buckets (I)) = null);

         Y := HT.Buckets (I);

         HT.Length := HT.Length + 1;

         for J in Count_Type range 2 .. M loop
            X := New_Node (Stream);
            pragma Assert (X /= null);
            pragma Assert (Next (X) = null);

            Set_Next (Node => Y, Next => X);
            Y := X;

            HT.Length := HT.Length + 1;
         end loop;

         N := N - M;

         exit when N = 0;
      end loop;
   end Generic_Read;

   -------------------
   -- Generic_Write --
   -------------------

   procedure Generic_Write
     (Stream : access Root_Stream_Type'Class;
      HT     : Hash_Table_Type)
   is
      M : Count_Type'Base;
      X : Node_Access;

   begin
      if HT.Buckets = null then
         Hash_Type'Write (Stream, 0);
      else
         Hash_Type'Write (Stream, HT.Buckets'Last);
      end if;

      Count_Type'Base'Write (Stream, HT.Length);

      if HT.Length = 0 then
         return;
      end if;

      --  TODO: see note in Generic_Read???

      for Indx in HT.Buckets'Range loop
         X := HT.Buckets (Indx);

         if X /= null then
            M := 1;
            loop
               X := Next (X);
               exit when X = null;
               M := M + 1;
            end loop;

            Hash_Type'Write (Stream, Indx);
            Count_Type'Base'Write (Stream, M);

            X := HT.Buckets (Indx);
            for J in Count_Type range 1 .. M loop
               Write (Stream, X);
               X := Next (X);
            end loop;

            pragma Assert (X = null);
         end if;
      end loop;
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

      if Source.Busy > 0 then
         raise Program_Error;
      end if;

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

   ----------
   -- Next --
   ----------

   function Next
     (HT   : Hash_Table_Type;
      Node : Node_Access) return Node_Access
   is
      Result : Node_Access := Next (Node);

   begin
      if Result /= null then
         return Result;
      end if;

      for Indx in Index (HT, Node) + 1 .. HT.Buckets'Last loop
         Result := HT.Buckets (Indx);

         if Result /= null then
            return Result;
         end if;
      end loop;

      return null;
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
            HT.Buckets := new Buckets_Type (0 .. NN - 1);
         end if;

         return;
      end if;

      if HT.Length = 0 then
         if N = 0 then
            Free (HT.Buckets);
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
         begin
            HT.Buckets := new Buckets_Type (0 .. NN - 1);
            Free (X);
         end;

         return;
      end if;

      if N = HT.Buckets'Length then
         return;
      end if;

      if N < HT.Buckets'Length then
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

      if HT.Busy > 0 then
         raise Program_Error;
      end if;

      Rehash : declare
         Dst_Buckets : Buckets_Access := new Buckets_Type (0 .. NN - 1);
         Src_Buckets : Buckets_Access := HT.Buckets;

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
                       Index (Dst_Buckets.all, Src_Node);

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
                  --  rehash, then AI-302 says the nodes "become lost."  The
                  --  issue is whether to actually deallocate these lost nodes,
                  --  since they might be designated by extant cursors.  Here
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

                  Free (Dst_Buckets);
                  raise Program_Error;
            end;

            Src_Index := Src_Index + 1;
         end loop;

         HT.Buckets := Dst_Buckets;
         HT.Length := LL;

         Free (Src_Buckets);
      end Rehash;
   end Reserve_Capacity;

end Ada.Containers.Hash_Tables.Generic_Operations;
