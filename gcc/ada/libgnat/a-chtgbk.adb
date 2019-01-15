------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.HASH_TABLES.GENERIC_BOUNDED_KEYS             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Hash_Tables.Generic_Bounded_Keys is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -----------------------------
   -- Checked_Equivalent_Keys --
   -----------------------------

   function Checked_Equivalent_Keys
     (HT   : aliased in out Hash_Table_Type'Class;
      Key  : Key_Type;
      Node : Count_Type) return Boolean
   is
      Lock : With_Lock (HT.TC'Unrestricted_Access);
   begin
      return Equivalent_Keys (Key, HT.Nodes (Node));
   end Checked_Equivalent_Keys;

   -------------------
   -- Checked_Index --
   -------------------

   function Checked_Index
     (HT  : aliased in out Hash_Table_Type'Class;
      Key : Key_Type) return Hash_Type
   is
      Lock : With_Lock (HT.TC'Unrestricted_Access);
   begin
      return HT.Buckets'First + Hash (Key) mod HT.Buckets'Length;
   end Checked_Index;

   --------------------------
   -- Delete_Key_Sans_Free --
   --------------------------

   procedure Delete_Key_Sans_Free
     (HT  : in out Hash_Table_Type'Class;
      Key : Key_Type;
      X   : out Count_Type)
   is
      Indx : Hash_Type;
      Prev : Count_Type;

   begin
      if HT.Length = 0 then
         X := 0;
         return;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      TC_Check (HT.TC);

      Indx := Checked_Index (HT, Key);
      X := HT.Buckets (Indx);

      if X = 0 then
         return;
      end if;

      if Checked_Equivalent_Keys (HT, Key, X) then
         TC_Check (HT.TC);
         HT.Buckets (Indx) := Next (HT.Nodes (X));
         HT.Length := HT.Length - 1;
         return;
      end if;

      loop
         Prev := X;
         X := Next (HT.Nodes (Prev));

         if X = 0 then
            return;
         end if;

         if Checked_Equivalent_Keys (HT, Key, X) then
            TC_Check (HT.TC);
            Set_Next (HT.Nodes (Prev), Next => Next (HT.Nodes (X)));
            HT.Length := HT.Length - 1;
            return;
         end if;
      end loop;
   end Delete_Key_Sans_Free;

   ----------
   -- Find --
   ----------

   function Find
     (HT  : Hash_Table_Type'Class;
      Key : Key_Type) return Count_Type
   is
      Indx : Hash_Type;
      Node : Count_Type;

   begin
      if HT.Length = 0 then
         return 0;
      end if;

      Indx := Checked_Index (HT'Unrestricted_Access.all, Key);

      Node := HT.Buckets (Indx);
      while Node /= 0 loop
         if Checked_Equivalent_Keys
           (HT'Unrestricted_Access.all, Key, Node)
         then
            return Node;
         end if;
         Node := Next (HT.Nodes (Node));
      end loop;

      return 0;
   end Find;

   --------------------------------
   -- Generic_Conditional_Insert --
   --------------------------------

   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type'Class;
      Key      : Key_Type;
      Node     : out Count_Type;
      Inserted : out Boolean)
   is
      Indx : Hash_Type;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      TC_Check (HT.TC);

      Indx := Checked_Index (HT, Key);
      Node := HT.Buckets (Indx);

      if Node = 0 then
         if Checks and then HT.Length = HT.Capacity then
            raise Capacity_Error with "no more capacity for insertion";
         end if;

         Node := New_Node;
         Set_Next (HT.Nodes (Node), Next => 0);

         Inserted := True;

         HT.Buckets (Indx) := Node;
         HT.Length := HT.Length + 1;

         return;
      end if;

      loop
         if Checked_Equivalent_Keys (HT, Key, Node) then
            Inserted := False;
            return;
         end if;

         Node := Next (HT.Nodes (Node));

         exit when Node = 0;
      end loop;

      if Checks and then HT.Length = HT.Capacity then
         raise Capacity_Error with "no more capacity for insertion";
      end if;

      Node := New_Node;
      Set_Next (HT.Nodes (Node), Next => HT.Buckets (Indx));

      Inserted := True;

      HT.Buckets (Indx) := Node;
      HT.Length := HT.Length + 1;
   end Generic_Conditional_Insert;

   -----------------------------
   -- Generic_Replace_Element --
   -----------------------------

   procedure Generic_Replace_Element
     (HT   : in out Hash_Table_Type'Class;
      Node : Count_Type;
      Key  : Key_Type)
   is
      pragma Assert (HT.Length > 0);
      pragma Assert (Node /= 0);

      BB : Buckets_Type renames HT.Buckets;
      NN : Nodes_Type renames HT.Nodes;

      Old_Indx : Hash_Type;
      New_Indx : constant Hash_Type := Checked_Index (HT, Key);

      New_Bucket : Count_Type renames BB (New_Indx);
      N, M       : Count_Type;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      --  The following block appears to be vestigial -- this should be done
      --  using Checked_Index instead. Also, we might have to move the actual
      --  tampering checks to the top of the subprogram, in order to prevent
      --  infinite recursion when calling Hash. (This is similar to how Insert
      --  and Delete are implemented.) This implies that we will have to defer
      --  the computation of New_Index until after the tampering check. ???

      declare
         Lock : With_Lock (HT.TC'Unrestricted_Access);
      begin
         Old_Indx := HT.Buckets'First + Hash (NN (Node)) mod HT.Buckets'Length;
      end;

      --  Replace_Element is allowed to change a node's key to Key
      --  (generic formal operation Assign provides the mechanism), but
      --  only if Key is not already in the hash table. (In a unique-key
      --  hash table as this one, a key is mapped to exactly one node.)

      if Checked_Equivalent_Keys (HT, Key, Node) then
         TE_Check (HT.TC);

         --  The new Key value is mapped to this same Node, so Node
         --  stays in the same bucket.

         Assign (NN (Node), Key);
         return;
      end if;

      --  Key is not equivalent to Node, so we now have to determine if it's
      --  equivalent to some other node in the hash table. This is the case
      --  irrespective of whether Key is in the same or a different bucket from
      --  Node.

      N := New_Bucket;
      while N /= 0 loop
         if Checks and then Checked_Equivalent_Keys (HT, Key, N) then
            pragma Assert (N /= Node);
            raise Program_Error with
              "attempt to replace existing element";
         end if;

         N := Next (NN (N));
      end loop;

      --  We have determined that Key is not already in the hash table, so
      --  the change is tentatively allowed. We now perform the standard
      --  checks to determine whether the hash table is locked (because you
      --  cannot change an element while it's in use by Query_Element or
      --  Update_Element), or if the container is busy (because moving a
      --  node to a different bucket would interfere with iteration).

      if Old_Indx = New_Indx then
         --  The node is already in the bucket implied by Key. In this case
         --  we merely change its value without moving it.

         TE_Check (HT.TC);

         Assign (NN (Node), Key);
         return;
      end if;

      --  The node is a bucket different from the bucket implied by Key

      TC_Check (HT.TC);

      --  Do the assignment first, before moving the node, so that if Assign
      --  propagates an exception, then the hash table will not have been
      --  modified (except for any possible side-effect Assign had on Node).

      Assign (NN (Node), Key);

      --  Now we can safely remove the node from its current bucket

      N := BB (Old_Indx);  -- get value of first node in old bucket
      pragma Assert (N /= 0);

      if N = Node then  -- node is first node in its bucket
         BB (Old_Indx) := Next (NN (Node));

      else
         pragma Assert (HT.Length > 1);

         loop
            M := Next (NN (N));
            pragma Assert (M /= 0);

            if M = Node then
               Set_Next (NN (N), Next => Next (NN (Node)));
               exit;
            end if;

            N := M;
         end loop;
      end if;

      --  Now we link the node into its new bucket (corresponding to Key)

      Set_Next (NN (Node), Next => New_Bucket);
      New_Bucket := Node;
   end Generic_Replace_Element;

   -----------
   -- Index --
   -----------

   function Index
     (HT  : Hash_Table_Type'Class;
      Key : Key_Type) return Hash_Type is
   begin
      return HT.Buckets'First + Hash (Key) mod HT.Buckets'Length;
   end Index;

end Ada.Containers.Hash_Tables.Generic_Bounded_Keys;
