------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       A D A . C O N T A I N E R S .                      --
--       H A S H _ T A B L E S . G E N E R I C _ O P E R A T I O N S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
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

      --  Technically it isn't necessary to allocate the exact same length
      --  buckets array, because our only requirement is that following
      --  assignment the source and target containers compare equal (that is,
      --  operator "=" returns True). We can satisfy this requirement with any
      --  hash table length, but we decide here to match the length of the
      --  source table. This has the benefit that when iterating, elements of
      --  the target are delivered in the exact same order as for the source.

      HT.Buckets := new Buckets_Type (Src_Buckets'Range);

      for Src_Index in Src_Buckets'Range loop
         Src_Node := Src_Buckets (Src_Index);

         if Src_Node /= null then
            declare
               Dst_Node : constant Node_Access := Copy_Node (Src_Node);

               --  See note above

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
      Node : Node_Access;

   begin
      if HT.Length = 0 then
         return;
      end if;

      for Indx in HT.Buckets'Range loop
         Node := HT.Buckets (Indx);
         while Node /= null loop
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
      N  : Count_Type'Base;
      NN : Hash_Type;

   begin
      Clear (HT);

      Count_Type'Base'Read (Stream, N);

      if N < 0 then
         raise Program_Error;
      end if;

      if N = 0 then
         return;
      end if;

      if HT.Buckets = null
        or else HT.Buckets'Length < N
      then
         Free (HT.Buckets);
         NN := Prime_Numbers.To_Prime (N);
         HT.Buckets := new Buckets_Type (0 .. NN - 1);
      end if;

      for J in 1 .. N loop
         declare
            Node : constant Node_Access := New_Node (Stream);
            Indx : constant Hash_Type := Index (HT, Node);
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
     (Stream : access Root_Stream_Type'Class;
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
