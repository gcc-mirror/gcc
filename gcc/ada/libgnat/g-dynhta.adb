------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D Y N A M I C _ H T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2023, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body GNAT.Dynamic_HTables is

   -------------------
   -- Hash_Two_Keys --
   -------------------

   function Hash_Two_Keys
     (Left  : Bucket_Range_Type;
      Right : Bucket_Range_Type) return Bucket_Range_Type
   is
      Half : constant := 2 ** (Bucket_Range_Type'Size / 2);
      Mask : constant := Half - 1;

   begin
      --  The hash is obtained in the following manner:
      --
      --    1) The low bits of Left are obtained, then shifted over to the high
      --       bits position.
      --
      --    2) The low bits of Right are obtained
      --
      --  The results from 1) and 2) are or-ed to produce a value within the
      --  range of Bucket_Range_Type.

      return
        ((Left  and Mask) * Half)
            or
         (Right and Mask);
   end Hash_Two_Keys;

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is
      function Get_Non_Null (T : Instance) return Elmt_Ptr;
      --  Returns Null_Ptr if Iterator_Started is False or if the Table is
      --  empty. Returns Iterator_Ptr if non null, or the next non null element
      --  in table if any.

      ---------
      -- Get --
      ---------

      function Get (T : Instance; K : Key) return Elmt_Ptr is
         Elmt : Elmt_Ptr;

      begin
         if T = null then
            return Null_Ptr;
         end if;

         Elmt := T.Table (Hash (K));

         loop
            if Elmt = Null_Ptr then
               return Null_Ptr;

            elsif Equal (Get_Key (Elmt), K) then
               return Elmt;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         T.Iterator_Started := True;
         T.Iterator_Index := T.Table'First;
         T.Iterator_Ptr := T.Table (T.Iterator_Index);
         return Get_Non_Null (T);
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Elmt_Ptr is
      begin
         if T = null or else not T.Iterator_Started then
            return Null_Ptr;
         end if;

         T.Iterator_Ptr := Next (T.Iterator_Ptr);
         return Get_Non_Null (T);
      end Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      function Get_Non_Null (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         while T.Iterator_Ptr = Null_Ptr  loop
            if T.Iterator_Index = T.Table'Last then
               T.Iterator_Started := False;
               return Null_Ptr;
            end if;

            T.Iterator_Index := T.Iterator_Index + 1;
            T.Iterator_Ptr   := T.Table (T.Iterator_Index);
         end loop;

         return T.Iterator_Ptr;
      end Get_Non_Null;

      ------------
      -- Remove --
      ------------

      procedure Remove  (T : Instance; K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
         Next_Elmt : Elmt_Ptr;

      begin
         if T = null then
            return;
         end if;

         Elmt := T.Table (Index);

         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            T.Table (Index) := Next (Elmt);

         else
            loop
               Next_Elmt := Next (Elmt);

               if Next_Elmt = Null_Ptr then
                  return;

               elsif Equal (Get_Key (Next_Elmt), K) then
                  Set_Next (Elmt, Next (Next_Elmt));
                  return;

               else
                  Elmt := Next_Elmt;
               end if;
            end loop;
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : in out Instance) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Instance_Data, Instance);

      begin
         if T = null then
            return;
         end if;

         for J in T.Table'Range loop
            T.Table (J) := Null_Ptr;
         end loop;

         Free (T);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; E : Elmt_Ptr) is
         Index : Header_Num;

      begin
         if T = null then
            T := new Instance_Data;
         end if;

         Index := Hash (Get_Key (E));
         Set_Next (E, T.Table (Index));
         T.Table (Index) := E;
      end Set;

   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   package body Simple_HTable is
      procedure Free is new
        Ada.Unchecked_Deallocation (Element_Wrapper, Elmt_Ptr);

      ---------
      -- Get --
      ---------

      function Get (T : Instance; K : Key) return Element is
         Tmp : Elmt_Ptr;

      begin
         if T = Nil then
            return No_Element;
         end if;

         Tmp := Tab.Get (Tab.Instance (T), K);

         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_First (Tab.Instance (T));

      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_First;

      -------------------
      -- Get_First_Key --
      -------------------

      function Get_First_Key (T : Instance) return Key_Option is
         Tmp : constant Elmt_Ptr := Tab.Get_First (Tab.Instance (T));
      begin
         if Tmp = null then
            return Key_Option'(Present => False);
         else
            return Key_Option'(Present => True, K => Tmp.all.K);
         end if;
      end Get_First_Key;

      -------------
      -- Get_Key --
      -------------

      function Get_Key (E : Elmt_Ptr) return Key is
      begin
         return E.K;
      end Get_Key;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_Next (Tab.Instance (T));
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_Next;

      ------------------
      -- Get_Next_Key --
      ------------------

      function Get_Next_Key (T : Instance) return Key_Option is
         Tmp : constant Elmt_Ptr := Tab.Get_Next (Tab.Instance (T));
      begin
         if Tmp = null then
            return Key_Option'(Present => False);
         else
            return Key_Option'(Present => True, K => Tmp.all.K);
         end if;
      end Get_Next_Key;

      ----------
      -- Next --
      ----------

      function Next (E : Elmt_Ptr) return Elmt_Ptr is
      begin
         return E.Next;
      end Next;

      ------------
      -- Remove --
      ------------

      procedure Remove  (T : Instance; K : Key) is
         Tmp : Elmt_Ptr;

      begin
         Tmp := Tab.Get (Tab.Instance (T), K);

         if Tmp /= null then
            Tab.Remove (Tab.Instance (T), K);
            Free (Tmp);
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : in out Instance) is
         E1, E2 : Elmt_Ptr;

      begin
         E1 := Tab.Get_First (Tab.Instance (T));
         while E1 /= null loop
            E2 := Tab.Get_Next (Tab.Instance (T));
            Free (E1);
            E1 := E2;
         end loop;

         Tab.Reset (Tab.Instance (T));
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; K : Key; E : Element) is
         Tmp : constant Elmt_Ptr := Tab.Get (Tab.Instance (T), K);
      begin
         if Tmp = null then
            Tab.Set (Tab.Instance (T), new Element_Wrapper'(K, E, null));
         else
            Tmp.E := E;
         end if;
      end Set;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr) is
      begin
         E.Next := Next;
      end Set_Next;
   end Simple_HTable;

   -------------------------
   -- Dynamic_Hash_Tables --
   -------------------------

   package body Dynamic_Hash_Tables is
      Minimum_Size : constant Bucket_Range_Type := 8;
      --  Minimum size of the buckets

      Safe_Compression_Size : constant Bucket_Range_Type :=
                                Minimum_Size * Compression_Factor;
      --  Maximum safe size for hash table compression. Beyond this size, a
      --  compression will violate the minimum size constraint on the buckets.

      Safe_Expansion_Size : constant Bucket_Range_Type :=
                              Bucket_Range_Type'Last / Expansion_Factor;
      --  Maximum safe size for hash table expansion. Beyond this size, an
      --  expansion will overflow the buckets.

      procedure Delete_Node
        (T   : Dynamic_Hash_Table;
         Nod : Node_Ptr);
      pragma Inline (Delete_Node);
      --  Detach and delete node Nod from table T

      procedure Destroy_Buckets (Bkts : Bucket_Table_Ptr);
      pragma Inline (Destroy_Buckets);
      --  Destroy all nodes within buckets Bkts

      procedure Detach (Nod : Node_Ptr);
      pragma Inline (Detach);
      --  Detach node Nod from the bucket it resides in

      procedure Ensure_Circular (Head : Node_Ptr);
      pragma Inline (Ensure_Circular);
      --  Ensure that dummy head Head is circular with respect to itself

      procedure Ensure_Created (T : Dynamic_Hash_Table);
      pragma Inline (Ensure_Created);
      --  Verify that hash table T is created. Raise Not_Created if this is not
      --  the case.

      procedure Ensure_Unlocked (T : Dynamic_Hash_Table);
      pragma Inline (Ensure_Unlocked);
      --  Verify that hash table T is unlocked. Raise Iterated if this is not
      --  the case.

      function Find_Bucket
        (Bkts : Bucket_Table_Ptr;
         Key  : Key_Type) return Node_Ptr;
      pragma Inline (Find_Bucket);
      --  Find the bucket among buckets Bkts which corresponds to key Key, and
      --  return its dummy head.

      function Find_Node (Head : Node_Ptr; Key : Key_Type) return Node_Ptr;
      pragma Inline (Find_Node);
      --  Traverse a bucket indicated by dummy head Head to determine whether
      --  there exists a node with key Key. If such a node exists, return it,
      --  otherwise return null.

      procedure First_Valid_Node
        (T        : Dynamic_Hash_Table;
         Low_Bkt  : Bucket_Range_Type;
         High_Bkt : Bucket_Range_Type;
         Idx      : out Bucket_Range_Type;
         Nod      : out Node_Ptr);
      pragma Inline (First_Valid_Node);
      --  Find the first valid node in the buckets of hash table T constrained
      --  by the range Low_Bkt .. High_Bkt. If such a node exists, return its
      --  bucket index in Idx and reference in Nod. If no such node exists,
      --  Idx is set to 0 and Nod to null.

      procedure Free is
        new Ada.Unchecked_Deallocation (Bucket_Table, Bucket_Table_Ptr);

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Dynamic_Hash_Table_Attributes, Dynamic_Hash_Table);

      procedure Free is
        new Ada.Unchecked_Deallocation (Node, Node_Ptr);

      function Is_Valid (Iter : Iterator) return Boolean;
      pragma Inline (Is_Valid);
      --  Determine whether iterator Iter refers to a valid key-value pair

      function Is_Valid (Nod : Node_Ptr; Head : Node_Ptr) return Boolean;
      pragma Inline (Is_Valid);
      --  Determine whether node Nod is non-null and does not refer to dummy
      --  head Head, thus making it valid.

      function Load_Factor (T : Dynamic_Hash_Table) return Threshold_Type;
      pragma Inline (Load_Factor);
      --  Calculate the load factor of hash table T

      procedure Lock (T : Dynamic_Hash_Table);
      pragma Inline (Lock);
      --  Lock all mutation functionality of hash table T

      procedure Mutate_And_Rehash
        (T    : Dynamic_Hash_Table;
         Size : Bucket_Range_Type);
      pragma Inline (Mutate_And_Rehash);
      --  Replace the buckets of hash table T with a new set of buckets of size
      --  Size. Rehash all key-value pairs from the old to the new buckets.

      procedure Prepend (Nod : Node_Ptr; Head : Node_Ptr);
      pragma Inline (Prepend);
      --  Insert node Nod immediately after dummy head Head

      function Present (Bkts : Bucket_Table_Ptr) return Boolean;
      pragma Inline (Present);
      --  Determine whether buckets Bkts exist

      function Present (Nod : Node_Ptr) return Boolean;
      pragma Inline (Present);
      --  Determine whether node Nod exists

      procedure Unlock (T : Dynamic_Hash_Table);
      pragma Inline (Unlock);
      --  Unlock all mutation functionality of hash table T

      --------------
      -- Contains --
      --------------

      function Contains
        (T   : Dynamic_Hash_Table;
         Key : Key_Type) return Boolean
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created (T);

         --  Obtain the dummy head of the bucket which should house the
         --  key-value pair.

         Head := Find_Bucket (T.Buckets, Key);

         --  Try to find a node in the bucket which matches the key

         Nod := Find_Node (Head, Key);

         return Is_Valid (Nod, Head);
      end Contains;

      ------------
      -- Create --
      ------------

      function Create (Initial_Size : Positive) return Dynamic_Hash_Table is
         Size : constant Bucket_Range_Type :=
                           Bucket_Range_Type'Max
                             (Bucket_Range_Type (Initial_Size), Minimum_Size);
         --  Ensure that the buckets meet a minimum size

         T : constant Dynamic_Hash_Table := new Dynamic_Hash_Table_Attributes;

      begin
         T.Buckets      := new Bucket_Table (0 .. Size - 1);
         T.Initial_Size := Size;

         return T;
      end Create;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (T   : Dynamic_Hash_Table;
         Key : Key_Type)
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (T);
         Ensure_Unlocked (T);

         --  Obtain the dummy head of the bucket which should house the
         --  key-value pair.

         Head := Find_Bucket (T.Buckets, Key);

         --  Try to find a node in the bucket which matches the key

         Nod := Find_Node (Head, Key);

         --  If such a node exists, remove it from the bucket and deallocate it

         if Is_Valid (Nod, Head) then
            Delete_Node (T, Nod);
         end if;
      end Delete;

      -----------------
      -- Delete_Node --
      -----------------

      procedure Delete_Node
        (T   : Dynamic_Hash_Table;
         Nod : Node_Ptr)
      is
         procedure Compress;
         pragma Inline (Compress);
         --  Determine whether hash table T requires compression, and if so,
         --  half its size.

         --------------
         -- Compress --
         --------------

         procedure Compress is
            pragma Assert (Present (T));
            pragma Assert (Present (T.Buckets));

            Old_Size : constant Bucket_Range_Type := T.Buckets'Length;

         begin
            --  The ratio of pairs to buckets is under the desited threshold.
            --  Compress the hash table only when there is still room to do so.

            if Load_Factor (T) < Compression_Threshold
              and then Old_Size >= Safe_Compression_Size
            then
               Mutate_And_Rehash (T, Old_Size / Compression_Factor);
            end if;
         end Compress;

         --  Local variables

         Ref : Node_Ptr := Nod;

      --  Start of processing for Delete_Node

      begin
         pragma Assert (Present (Ref));
         pragma Assert (Present (T));

         Detach (Ref);
         Free   (Ref);

         --  The number of key-value pairs is updated when the hash table
         --  contains a valid node which represents the pair.

         T.Pairs := T.Pairs - 1;

         --  Compress the hash table if the load factor drops below the value
         --  of Compression_Threshold.

         Compress;
      end Delete_Node;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (T : in out Dynamic_Hash_Table) is
      begin
         Ensure_Created  (T);
         Ensure_Unlocked (T);

         --  Destroy all nodes in all buckets

         Destroy_Buckets (T.Buckets);
         Free (T.Buckets);
         Free (T);
      end Destroy;

      ---------------------
      -- Destroy_Buckets --
      ---------------------

      procedure Destroy_Buckets (Bkts : Bucket_Table_Ptr) is
         procedure Destroy_Bucket (Head : Node_Ptr);
         pragma Inline (Destroy_Bucket);
         --  Destroy all nodes in a bucket with dummy head Head

         --------------------
         -- Destroy_Bucket --
         --------------------

         procedure Destroy_Bucket (Head : Node_Ptr) is
            Nod : Node_Ptr;

         begin
            --  Destroy all valid nodes which follow the dummy head

            while Is_Valid (Head.Next, Head) loop
               Nod := Head.Next;

               --  Invoke the value destructor before deallocating the node

               Destroy_Value (Nod.Value);

               Detach (Nod);
               Free   (Nod);
            end loop;
         end Destroy_Bucket;

      --  Start of processing for Destroy_Buckets

      begin
         pragma Assert (Present (Bkts));

         for Scan_Idx in Bkts'Range loop
            Destroy_Bucket (Bkts (Scan_Idx)'Access);
         end loop;
      end Destroy_Buckets;

      ------------
      -- Detach --
      ------------

      procedure Detach (Nod : Node_Ptr) is
         pragma Assert (Present (Nod));

         Next : constant Node_Ptr := Nod.Next;
         Prev : constant Node_Ptr := Nod.Prev;

      begin
         pragma Assert (Present (Next));
         pragma Assert (Present (Prev));

         Prev.Next := Next;  --  Prev ---> Next
         Next.Prev := Prev;  --  Prev <--> Next

         Nod.Next := null;
         Nod.Prev := null;
      end Detach;

      ---------------------
      -- Ensure_Circular --
      ---------------------

      procedure Ensure_Circular (Head : Node_Ptr) is
         pragma Assert (Present (Head));

      begin
         if not Present (Head.Next) and then not Present (Head.Prev) then
            Head.Next := Head;
            Head.Prev := Head;
         end if;
      end Ensure_Circular;

      --------------------
      -- Ensure_Created --
      --------------------

      procedure Ensure_Created (T : Dynamic_Hash_Table) is
      begin
         if not Present (T) then
            raise Not_Created;
         end if;
      end Ensure_Created;

      ---------------------
      -- Ensure_Unlocked --
      ---------------------

      procedure Ensure_Unlocked (T : Dynamic_Hash_Table) is
      begin
         pragma Assert (Present (T));

         --  The hash table has at least one outstanding iterator

         if T.Iterators > 0 then
            raise Iterated;
         end if;
      end Ensure_Unlocked;

      -----------------
      -- Find_Bucket --
      -----------------

      function Find_Bucket
        (Bkts : Bucket_Table_Ptr;
         Key  : Key_Type) return Node_Ptr
      is
         pragma Assert (Present (Bkts));

         Idx : constant Bucket_Range_Type := Hash (Key) mod Bkts'Length;

      begin
         return Bkts (Idx)'Access;
      end Find_Bucket;

      ---------------
      -- Find_Node --
      ---------------

      function Find_Node (Head : Node_Ptr; Key : Key_Type) return Node_Ptr is
         pragma Assert (Present (Head));

         Nod : Node_Ptr;

      begin
         --  Traverse the nodes of the bucket, looking for a key-value pair
         --  with the same key.

         Nod := Head.Next;
         while Is_Valid (Nod, Head) loop
            if Nod.Key = Key then
               return Nod;
            end if;

            Nod := Nod.Next;
         end loop;

         return null;
      end Find_Node;

      ----------------------
      -- First_Valid_Node --
      ----------------------

      procedure First_Valid_Node
        (T        : Dynamic_Hash_Table;
         Low_Bkt  : Bucket_Range_Type;
         High_Bkt : Bucket_Range_Type;
         Idx      : out Bucket_Range_Type;
         Nod      : out Node_Ptr)
      is
         Head : Node_Ptr;

      begin
         pragma Assert (Present (T));
         pragma Assert (Present (T.Buckets));

         --  Assume that no valid node exists

         Idx := 0;
         Nod := null;

         --  Examine the buckets of the hash table within the requested range,
         --  looking for the first valid node.

         for Scan_Idx in Low_Bkt .. High_Bkt loop
            Head := T.Buckets (Scan_Idx)'Access;

            --  The bucket contains at least one valid node, return the first
            --  such node.

            if Is_Valid (Head.Next, Head) then
               Idx := Scan_Idx;
               Nod := Head.Next;
               return;
            end if;
         end loop;
      end First_Valid_Node;

      ---------
      -- Get --
      ---------

      function Get
        (T   : Dynamic_Hash_Table;
         Key : Key_Type) return Value_Type
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created (T);

         --  Obtain the dummy head of the bucket which should house the
         --  key-value pair.

         Head := Find_Bucket (T.Buckets, Key);

         --  Try to find a node in the bucket which matches the key

         Nod := Find_Node (Head, Key);

         --  If such a node exists, return the value of the key-value pair

         if Is_Valid (Nod, Head) then
            return Nod.Value;
         end if;

         return No_Value;
      end Get;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Iterator) return Boolean is
         Is_OK : constant Boolean := Is_Valid (Iter);
         T     : constant Dynamic_Hash_Table := Iter.Table;

      begin
         pragma Assert (Present (T));

         --  The iterator is no longer valid which indicates that it has been
         --  exhausted. Unlock all mutation functionality of the hash table
         --  because the iterator cannot be advanced any further.

         if not Is_OK then
            Unlock (T);
         end if;

         return Is_OK;
      end Has_Next;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (T : Dynamic_Hash_Table) return Boolean is
      begin
         Ensure_Created (T);

         return T.Pairs = 0;
      end Is_Empty;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (Iter : Iterator) return Boolean is
      begin
         --  The invariant of Iterate and Next ensures that the iterator always
         --  refers to a valid node if there exists one.

         return Present (Iter.Curr_Nod);
      end Is_Valid;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (Nod : Node_Ptr; Head : Node_Ptr) return Boolean is
      begin
         --  A node is valid if it is non-null, and does not refer to the dummy
         --  head of some bucket.

         return Present (Nod) and then Nod /= Head;
      end Is_Valid;

      -------------
      -- Iterate --
      -------------

      function Iterate (T : Dynamic_Hash_Table) return Iterator is
         Iter : Iterator;

      begin
         Ensure_Created (T);
         pragma Assert (Present (T.Buckets));

         --  Initialize the iterator to reference the first valid node in
         --  the full range of hash table buckets. If no such node exists,
         --  the iterator is left in a state which does not allow it to
         --  advance.

         First_Valid_Node
           (T        => T,
            Low_Bkt  => T.Buckets'First,
            High_Bkt => T.Buckets'Last,
            Idx      => Iter.Curr_Idx,
            Nod      => Iter.Curr_Nod);

         --  Associate the iterator with the hash table to allow for future
         --  mutation functionality unlocking.

         Iter.Table := T;

         --  Lock all mutation functionality of the hash table while it is
         --  being iterated on.

         Lock (T);

         return Iter;
      end Iterate;

      -----------------
      -- Load_Factor --
      -----------------

      function Load_Factor (T : Dynamic_Hash_Table) return Threshold_Type is
         pragma Assert (Present (T));
         pragma Assert (Present (T.Buckets));

      begin
         --  The load factor is the ratio of key-value pairs to buckets

         return Threshold_Type (T.Pairs) / Threshold_Type (T.Buckets'Length);
      end Load_Factor;

      ----------
      -- Lock --
      ----------

      procedure Lock (T : Dynamic_Hash_Table) is
      begin
         --  The hash table may be locked multiple times if multiple iterators
         --  are operating over it.

         T.Iterators := T.Iterators + 1;
      end Lock;

      -----------------------
      -- Mutate_And_Rehash --
      -----------------------

      procedure Mutate_And_Rehash
        (T    : Dynamic_Hash_Table;
         Size : Bucket_Range_Type)
      is
         procedure Rehash (From : Bucket_Table_Ptr; To : Bucket_Table_Ptr);
         pragma Inline (Rehash);
         --  Remove all nodes from buckets From and rehash them into buckets To

         procedure Rehash_Bucket (Head : Node_Ptr; To : Bucket_Table_Ptr);
         pragma Inline (Rehash_Bucket);
         --  Detach all nodes starting from dummy head Head and rehash them
         --  into To.

         procedure Rehash_Node (Nod : Node_Ptr; To : Bucket_Table_Ptr);
         pragma Inline (Rehash_Node);
         --  Rehash node Nod into To

         ------------
         -- Rehash --
         ------------

         procedure Rehash (From : Bucket_Table_Ptr; To : Bucket_Table_Ptr) is
         begin
            pragma Assert (Present (From));
            pragma Assert (Present (To));

            for Scan_Idx in From'Range loop
               Rehash_Bucket (From (Scan_Idx)'Access, To);
            end loop;
         end Rehash;

         -------------------
         -- Rehash_Bucket --
         -------------------

         procedure Rehash_Bucket (Head : Node_Ptr; To : Bucket_Table_Ptr) is
            pragma Assert (Present (Head));

            Nod : Node_Ptr;

         begin
            --  Detach all nodes which follow the dummy head

            while Is_Valid (Head.Next, Head) loop
               Nod := Head.Next;

               Detach (Nod);
               Rehash_Node (Nod, To);
            end loop;
         end Rehash_Bucket;

         -----------------
         -- Rehash_Node --
         -----------------

         procedure Rehash_Node (Nod : Node_Ptr; To : Bucket_Table_Ptr) is
            pragma Assert (Present (Nod));

            Head : Node_Ptr;

         begin
            --  Obtain the dummy head of the bucket which should house the
            --  key-value pair.

            Head := Find_Bucket (To, Nod.Key);

            --  Ensure that the dummy head of an empty bucket is circular with
            --  respect to itself.

            Ensure_Circular (Head);

            --  Prepend the node to the bucket

            Prepend (Nod, Head);
         end Rehash_Node;

         --  Local declarations

         Old_Bkts : Bucket_Table_Ptr;

      --  Start of processing for Mutate_And_Rehash

      begin
         pragma Assert (Present (T));

         Old_Bkts  := T.Buckets;
         T.Buckets := new Bucket_Table (0 .. Size - 1);

         --  Transfer and rehash all key-value pairs from the old buckets to
         --  the new buckets.

         Rehash (From => Old_Bkts, To => T.Buckets);
         Free (Old_Bkts);
      end Mutate_And_Rehash;

      ----------
      -- Next --
      ----------

      procedure Next (Iter : in out Iterator; Key : out Key_Type) is
         Is_OK : constant Boolean  := Is_Valid (Iter);
         Saved : constant Node_Ptr := Iter.Curr_Nod;
         T     : constant Dynamic_Hash_Table := Iter.Table;
         Head  : Node_Ptr;

      begin
         pragma Assert (Present (T));
         pragma Assert (Present (T.Buckets));

         --  The iterator is no longer valid which indicates that it has been
         --  exhausted. Unlock all mutation functionality of the hash table as
         --  the iterator cannot be advanced any further.

         if not Is_OK then
            Unlock (T);
            raise Iterator_Exhausted;
         end if;

         --  Advance to the next node along the same bucket

         Iter.Curr_Nod := Iter.Curr_Nod.Next;
         Head := T.Buckets (Iter.Curr_Idx)'Access;

         --  If the new node is no longer valid, then this indicates that the
         --  current bucket has been exhausted. Advance to the next valid node
         --  within the remaining range of buckets. If no such node exists, the
         --  iterator is left in a state which does not allow it to advance.

         if not Is_Valid (Iter.Curr_Nod, Head) then
            First_Valid_Node
              (T        => T,
               Low_Bkt  => Iter.Curr_Idx + 1,
               High_Bkt => T.Buckets'Last,
               Idx      => Iter.Curr_Idx,
               Nod      => Iter.Curr_Nod);
         end if;

         Key := Saved.Key;
      end Next;

      -------------
      -- Prepend --
      -------------

      procedure Prepend (Nod : Node_Ptr; Head : Node_Ptr) is
         pragma Assert (Present (Nod));
         pragma Assert (Present (Head));

         Next : constant Node_Ptr := Head.Next;

      begin
         Head.Next := Nod;
         Next.Prev := Nod;

         Nod.Next := Next;
         Nod.Prev := Head;
      end Prepend;

      -------------
      -- Present --
      -------------

      function Present (Bkts : Bucket_Table_Ptr) return Boolean is
      begin
         return Bkts /= null;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (Nod : Node_Ptr) return Boolean is
      begin
         return Nod /= null;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (T : Dynamic_Hash_Table) return Boolean is
      begin
         return T /= Nil;
      end Present;

      ---------
      -- Put --
      ---------

      procedure Put
        (T     : Dynamic_Hash_Table;
         Key   : Key_Type;
         Value : Value_Type)
      is
         procedure Expand;
         pragma Inline (Expand);
         --  Determine whether hash table T requires expansion, and if so,
         --  double its size.

         procedure Prepend_Or_Replace (Head : Node_Ptr);
         pragma Inline (Prepend_Or_Replace);
         --  Update the value of a node within a bucket with dummy head Head
         --  whose key is Key to Value. If there is no such node, prepend a new
         --  key-value pair to the bucket.

         ------------
         -- Expand --
         ------------

         procedure Expand is
            pragma Assert (Present (T));
            pragma Assert (Present (T.Buckets));

            Old_Size : constant Bucket_Range_Type := T.Buckets'Length;

         begin
            --  The ratio of pairs to buckets is over the desited threshold.
            --  Expand the hash table only when there is still room to do so.

            if Load_Factor (T) > Expansion_Threshold
              and then Old_Size <= Safe_Expansion_Size
            then
               Mutate_And_Rehash (T, Old_Size * Expansion_Factor);
            end if;
         end Expand;

         ------------------------
         -- Prepend_Or_Replace --
         ------------------------

         procedure Prepend_Or_Replace (Head : Node_Ptr) is
            pragma Assert (Present (Head));

            Nod : Node_Ptr;

         begin
            --  If the bucket containst at least one valid node, then there is
            --  a chance that a node with the same key as Key exists. If this
            --  is the case, the value of that node must be updated.

            Nod := Head.Next;
            while Is_Valid (Nod, Head) loop
               if Nod.Key = Key then
                  Nod.Value := Value;
                  return;
               end if;

               Nod := Nod.Next;
            end loop;

            --  At this point the bucket is either empty, or none of the nodes
            --  match key Key. Prepend a new key-value pair.

            Nod := new Node'(Key, Value, null, null);

            Prepend (Nod, Head);

            --  The number of key-value pairs must be updated for a prepend,
            --  never for a replace.

            T.Pairs := T.Pairs + 1;
         end Prepend_Or_Replace;

         --  Local variables

         Head : Node_Ptr;

      --  Start of processing for Put

      begin
         Ensure_Created  (T);
         Ensure_Unlocked (T);

         --  Obtain the dummy head of the bucket which should house the
         --  key-value pair.

         Head := Find_Bucket (T.Buckets, Key);

         --  Ensure that the dummy head of an empty bucket is circular with
         --  respect to itself.

         Ensure_Circular (Head);

         --  In case the bucket already contains a node with the same key,
         --  replace its value, otherwise prepend a new key-value pair.

         Prepend_Or_Replace (Head);

         --  Expand the hash table if the ratio of pairs to buckets goes over
         --  Expansion_Threshold.

         Expand;
      end Put;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : Dynamic_Hash_Table) is
      begin
         Ensure_Created  (T);
         Ensure_Unlocked (T);

         --  Destroy all nodes in all buckets

         Destroy_Buckets (T.Buckets);
         Free (T.Buckets);

         --  Recreate the buckets using the original size from creation time

         T.Buckets := new Bucket_Table (0 .. T.Initial_Size - 1);
         T.Pairs   := 0;
      end Reset;

      ----------
      -- Size --
      ----------

      function Size (T : Dynamic_Hash_Table) return Natural is
      begin
         Ensure_Created (T);

         return T.Pairs;
      end Size;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (T : Dynamic_Hash_Table) is
      begin
         --  The hash table may be locked multiple times if multiple iterators
         --  are operating over it.

         T.Iterators := T.Iterators - 1;
      end Unlock;
   end Dynamic_Hash_Tables;

end GNAT.Dynamic_HTables;
