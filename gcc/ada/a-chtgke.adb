------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.HASH_TABLES.GENERIC_KEYS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2013, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Hash_Tables.Generic_Keys is

   -----------------------------
   -- Checked_Equivalent_Keys --
   -----------------------------

   function Checked_Equivalent_Keys
     (HT   : aliased in out Hash_Table_Type;
      Key  : Key_Type;
      Node : Node_Access) return Boolean
   is
      Result : Boolean;

      B : Natural renames HT.Busy;
      L : Natural renames HT.Lock;

   begin
      B := B + 1;
      L := L + 1;

      Result := Equivalent_Keys (Key, Node);

      B := B - 1;
      L := L - 1;

      return Result;

   exception
      when others =>
         B := B - 1;
         L := L - 1;

         raise;
   end Checked_Equivalent_Keys;

   -------------------
   -- Checked_Index --
   -------------------

   function Checked_Index
     (HT  : aliased in out Hash_Table_Type;
      Key : Key_Type) return Hash_Type
   is
      Result : Hash_Type;

      B : Natural renames HT.Busy;
      L : Natural renames HT.Lock;

   begin
      B := B + 1;
      L := L + 1;

      Result := Hash (Key) mod HT.Buckets'Length;

      B := B - 1;
      L := L - 1;

      return Result;

   exception
      when others =>
         B := B - 1;
         L := L - 1;

         raise;
   end Checked_Index;

   --------------------------
   -- Delete_Key_Sans_Free --
   --------------------------

   procedure Delete_Key_Sans_Free
     (HT  : in out Hash_Table_Type;
      Key : Key_Type;
      X   : out Node_Access)
   is
      Indx : Hash_Type;
      Prev : Node_Access;

   begin
      if HT.Length = 0 then
         X := null;
         return;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Indx := Checked_Index (HT, Key);
      X := HT.Buckets (Indx);

      if X = null then
         return;
      end if;

      if Checked_Equivalent_Keys (HT, Key, X) then
         if HT.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;
         HT.Buckets (Indx) := Next (X);
         HT.Length := HT.Length - 1;
         return;
      end if;

      loop
         Prev := X;
         X := Next (Prev);

         if X = null then
            return;
         end if;

         if Checked_Equivalent_Keys (HT, Key, X) then
            if HT.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (container is busy)";
            end if;
            Set_Next (Node => Prev, Next => Next (X));
            HT.Length := HT.Length - 1;
            return;
         end if;
      end loop;
   end Delete_Key_Sans_Free;

   ----------
   -- Find --
   ----------

   function Find
     (HT  : aliased in out Hash_Table_Type;
      Key : Key_Type) return Node_Access
   is
      Indx : Hash_Type;
      Node : Node_Access;

   begin
      if HT.Length = 0 then
         return null;
      end if;

      Indx := Checked_Index (HT, Key);

      Node := HT.Buckets (Indx);
      while Node /= null loop
         if Checked_Equivalent_Keys (HT, Key, Node) then
            return Node;
         end if;
         Node := Next (Node);
      end loop;

      return null;
   end Find;

   --------------------------------
   -- Generic_Conditional_Insert --
   --------------------------------

   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean)
   is
      Indx : Hash_Type;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Indx := Checked_Index (HT, Key);
      Node := HT.Buckets (Indx);

      if Node = null then
         if HT.Length = Count_Type'Last then
            raise Constraint_Error;
         end if;

         Node := New_Node (Next => null);
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

         Node := Next (Node);

         exit when Node = null;
      end loop;

      if HT.Length = Count_Type'Last then
         raise Constraint_Error;
      end if;

      Node := New_Node (Next => HT.Buckets (Indx));
      Inserted := True;

      HT.Buckets (Indx) := Node;
      HT.Length := HT.Length + 1;
   end Generic_Conditional_Insert;

   -----------------------------
   -- Generic_Replace_Element --
   -----------------------------

   procedure Generic_Replace_Element
     (HT   : in out Hash_Table_Type;
      Node : Node_Access;
      Key  : Key_Type)
   is
      pragma Assert (HT.Length > 0);
      pragma Assert (Node /= null);

      Old_Indx : Hash_Type;
      New_Indx : constant Hash_Type := Checked_Index (HT, Key);

      New_Bucket : Node_Access renames HT.Buckets (New_Indx);
      N, M       : Node_Access;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         B : Natural renames HT.Busy;
         L : Natural renames HT.Lock;

      begin
         B := B + 1;
         L := L + 1;

         Old_Indx := Hash (Node) mod HT.Buckets'Length;

         B := B - 1;
         L := L - 1;

      exception
         when others =>
            B := B - 1;
            L := L - 1;

            raise;
      end;

      if Checked_Equivalent_Keys (HT, Key, Node) then
         if HT.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (container is locked)";
         end if;

         --  We can change a node's key to Key (that's what Assign is for), but
         --  only if Key is not already in the hash table. (In a unique-key
         --  hash table as this one a key is mapped to exactly one node only.)
         --  The exception is when Key is mapped to Node, in which case the
         --  change is allowed.

         Assign (Node, Key);
         return;
      end if;

      --  Key is not equivalent to Node, so we now have to determine if it's
      --  equivalent to some other node in the hash table. This is the case
      --  irrespective of whether Key is in the same or a different bucket from
      --  Node.

      N := New_Bucket;
      while N /= null loop
         if Checked_Equivalent_Keys (HT, Key, N) then
            pragma Assert (N /= Node);
            raise Program_Error with
              "attempt to replace existing element";
         end if;

         N := Next (N);
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

         if HT.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (container is locked)";
         end if;

         Assign (Node, Key);
         return;
      end if;

      --  The node is a bucket different from the bucket implied by Key

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      --  Do the assignment first, before moving the node, so that if Assign
      --  propagates an exception, then the hash table will not have been
      --  modified (except for any possible side-effect Assign had on Node).

      Assign (Node, Key);

      --  Now we can safely remove the node from its current bucket

      N := HT.Buckets (Old_Indx);
      pragma Assert (N /= null);

      if N = Node then
         HT.Buckets (Old_Indx) := Next (Node);

      else
         pragma Assert (HT.Length > 1);

         loop
            M := Next (N);
            pragma Assert (M /= null);

            if M = Node then
               Set_Next (Node => N, Next => Next (Node));
               exit;
            end if;

            N := M;
         end loop;
      end if;

      --  Now we link the node into its new bucket (corresponding to Key)

      Set_Next (Node => Node, Next => New_Bucket);
      New_Bucket := Node;
   end Generic_Replace_Element;

   -----------
   -- Index --
   -----------

   function Index
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Hash_Type
   is
   begin
      return Hash (Key) mod HT.Buckets'Length;
   end Index;

end Ada.Containers.Hash_Tables.Generic_Keys;
