------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.HASH_TABLES.GENERIC_OPERATIONS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This body needs commenting ???

with Ada.Containers.Prime_Numbers;
with Ada.Unchecked_Deallocation;

with System;  use type System.Address;

package body Ada.Containers.Hash_Tables.Generic_Operations is

   procedure Free is
     new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Rehash
     (HT   : in out Hash_Table_Type;
      Size : Hash_Type);

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

      --  Probably we have to duplicate the Size (Src), too, in order
      --  to guarantee that

      --    Dst := Src;
      --    Dst = Src is true

      --  The only quirk is that we depend on the hash value of a dst key
      --  to be the same as the src key from which it was copied.
      --  If we relax the requirement that the hash value must be the
      --  same, then of course we can't guarantee that following
      --  assignment that Dst = Src is true ???

      for Src_Index in Src_Buckets'Range loop
         Src_Node := Src_Buckets (Src_Index);

         if Src_Node /= Null_Node then
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
            while Src_Node /= Null_Node loop
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
      while HT.Length > 0 loop
         while HT.Buckets (Index) = Null_Node loop
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
               exit when Bucket = Null_Node;
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
      pragma Assert (X /= Null_Node);

      Indx : Hash_Type;
      Prev : Node_Access;
      Curr : Node_Access;

   begin
      if HT.Length = 0 then
         raise Program_Error;
      end if;

      Indx := Index (HT, X);
      Prev := HT.Buckets (Indx);

      if Prev = Null_Node then
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

         if Curr = Null_Node then
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

   ---------------------
   -- Ensure_Capacity --
   ---------------------

   procedure Ensure_Capacity
     (HT : in out Hash_Table_Type;
      N  : Count_Type)
   is
      NN : Hash_Type;

   begin
      if N = 0 then
         if HT.Length = 0 then
            Free (HT.Buckets);

         elsif HT.Length < HT.Buckets'Length then
            NN := Prime_Numbers.To_Prime (HT.Length);

            --  ASSERT: NN >= HT.Length

            if NN < HT.Buckets'Length then
               Rehash (HT, Size => NN);
            end if;
         end if;

         return;
      end if;

      if HT.Buckets = null then
         NN := Prime_Numbers.To_Prime (N);

         --  ASSERT: NN >= N

         Rehash (HT, Size => NN);
         return;
      end if;

      if N <= HT.Length then
         if HT.Length >= HT.Buckets'Length then
            return;
         end if;

         NN := Prime_Numbers.To_Prime (HT.Length);

         --  ASSERT: NN >= HT.Length

         if NN < HT.Buckets'Length then
            Rehash (HT, Size => NN);
         end if;

         return;
      end if;

      --  ASSERT: N > HT.Length

      if N = HT.Buckets'Length then
         return;
      end if;

      NN := Prime_Numbers.To_Prime (N);

      --  ASSERT: NN >= N
      --  ASSERT: NN > HT.Length

      if NN /= HT.Buckets'Length then
         Rehash (HT, Size => NN);
      end if;
   end Ensure_Capacity;

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
         return Null_Node;
      end if;

      Indx := HT.Buckets'First;
      loop
         if HT.Buckets (Indx) /= Null_Node then
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
         while Buckets (J) /= Null_Node loop
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
         exit when L_Node /= Null_Node;
         L_Index := L_Index + 1;
      end loop;

      N := L.Length;

      loop
         if not Find (HT => R, Key => L_Node) then
            return False;
         end if;

         N := N - 1;

         L_Node := Next (L_Node);

         if L_Node = Null_Node then
            if N = 0 then
               return True;
            end if;

            loop
               L_Index := L_Index + 1;
               L_Node := L.Buckets (L_Index);
               exit when L_Node /= Null_Node;
            end loop;
         end if;
      end loop;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (HT : Hash_Table_Type) is
      Node : Node_Access;

   begin
      if HT.Buckets = null
        or else HT.Length = 0
      then
         return;
      end if;

      for Indx in HT.Buckets'Range loop
         Node := HT.Buckets (Indx);
         while Node /= Null_Node loop
            Process (Node);
            Node := Next (Node);
         end loop;
      end loop;
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
      --  As with the sorted set, it's not clear whether read is allowed to
      --  have side effect if it fails. For now, we assume side effects are
      --  allowed since it simplifies the algorithm ???
      --
      Clear (HT);

      declare
         B : Buckets_Access := HT.Buckets;
      begin
         HT.Buckets := null;
         HT.Length := 0;
         Free (B); -- can this fail???
      end;

      Hash_Type'Read (Stream, Last);

      if Last /= 0 then
         HT.Buckets := new Buckets_Type (0 .. Last);
      end if;

      Count_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);
      while N > 0 loop
         Hash_Type'Read (Stream, I);
         pragma Assert (I in HT.Buckets'Range);
         pragma Assert (HT.Buckets (I) = Null_Node);

         Count_Type'Base'Read (Stream, M);
         pragma Assert (M >= 1);
         pragma Assert (M <= N);

         HT.Buckets (I) := New_Node (Stream);
         pragma Assert (HT.Buckets (I) /= Null_Node);
         pragma Assert (Next (HT.Buckets (I)) = Null_Node);

         Y := HT.Buckets (I);

         HT.Length := HT.Length + 1;

         for J in Count_Type range 2 .. M loop
            X := New_Node (Stream);
            pragma Assert (X /= Null_Node);
            pragma Assert (Next (X) = Null_Node);

            Set_Next (Node => Y, Next => X);
            Y := X;

            HT.Length := HT.Length + 1;
         end loop;

         N := N - M;
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

      for Indx in HT.Buckets'Range loop
         X := HT.Buckets (Indx);

         if X /= Null_Node then
            M := 1;
            loop
               X := Next (X);
               exit when X = Null_Node;
               M := M + 1;
            end loop;

            Hash_Type'Write (Stream, Indx);
            Count_Type'Base'Write (Stream, M);

            X := HT.Buckets (Indx);
            for J in Count_Type range 1 .. M loop
               Write (Stream, X);
               X := Next (X);
            end loop;

            pragma Assert (X = Null_Node);
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

      if Target.Length > 0 then
         raise Constraint_Error;
      end if;

      Free (Target.Buckets);

      Target.Buckets := Source.Buckets;
      Source.Buckets := null;

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
      if Result /= Null_Node then
         return Result;
      end if;

      for Indx in Index (HT, Node) + 1 .. HT.Buckets'Last loop
         Result := HT.Buckets (Indx);

         if Result /= Null_Node then
            return Result;
         end if;
      end loop;

      return Null_Node;
   end Next;

   ------------
   -- Rehash --
   ------------

   procedure Rehash
     (HT   : in out Hash_Table_Type;
      Size : Hash_Type)
   is
      subtype Buckets_Range is Hash_Type range 0 .. Size - 1;

      Dst_Buckets : Buckets_Access := new Buckets_Type (Buckets_Range);
      Src_Buckets : Buckets_Access := HT.Buckets;

      L  : Count_Type renames HT.Length;
      LL : constant Count_Type := L;

   begin
      if Src_Buckets = null then
         pragma Assert (L = 0);
         HT.Buckets := Dst_Buckets;
         return;
      end if;

      if L = 0 then
         HT.Buckets := Dst_Buckets;
         Free (Src_Buckets);
         return;
      end if;

      --  We might want to change this to iter from 1 .. L instead ???

      for Src_Index in Src_Buckets'Range loop

         declare
            Src_Bucket : Node_Access renames Src_Buckets (Src_Index);
         begin
            while Src_Bucket /= Null_Node loop
               declare
                  Src_Node   : constant Node_Access := Src_Bucket;
                  Dst_Index  : constant Hash_Type :=
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

               --  Not clear that we can deallocate the nodes,
               --  because they may be designated by outstanding
               --  iterators.  Which means they're now lost... ???

               --                 for J in NB'Range loop
               --                    declare
               --                       Dst : Node_Access renames NB (J);
               --                       X   : Node_Access;
               --                    begin
               --                       while Dst /= Null_Node loop
               --                          X := Dst;
               --                          Dst := Succ (Dst);
               --                          Free (X);
               --                       end loop;
               --                    end;
               --                 end loop;


               Free (Dst_Buckets);
               raise;
         end;

         --  exit when L = 0;
         --  need to bother???

      end loop;

      pragma Assert (L = 0);

      HT.Buckets := Dst_Buckets;
      HT.Length := LL;

      Free (Src_Buckets);
   end Rehash;

end Ada.Containers.Hash_Tables.Generic_Operations;

