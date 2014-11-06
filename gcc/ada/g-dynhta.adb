------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D Y N A M I C _ H T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      type Table_Type is array (Header_Num) of Elmt_Ptr;

      type Instance_Data is record
         Table            : Table_Type;
         Iterator_Index   : Header_Num;
         Iterator_Ptr     : Elmt_Ptr;
         Iterator_Started : Boolean := False;
      end record;

      function Get_Non_Null (T : Instance) return Elmt_Ptr;
      --  Returns Null_Ptr if Iterator_Started is False or if the Table is
      --  empty. Returns Iterator_Ptr if non null, or the next non null
      --  element in table if any.

      ---------
      -- Get --
      ---------

      function  Get (T : Instance; K : Key) return Elmt_Ptr is
         Elmt  : Elmt_Ptr;

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
               Next_Elmt :=  Next (Elmt);

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

      function  Get (T : Instance; K : Key) return Element is
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

   ------------------------
   -- Load_Factor_HTable --
   ------------------------

   package body Load_Factor_HTable is

      Min_Size_Increase : constant := 5;
      --  The minimum increase expressed as number of buckets. This value is
      --  used to determine the new size of small tables and/or small growth
      --  percentages.

      procedure Attach
        (Elmt  : not null Element_Ptr;
         Chain : not null Element_Ptr);
      --  Prepend an element to a bucket chain. Elmt is inserted after the
      --  dummy head of Chain.

      function Create_Buckets (Size : Positive) return Buckets_Array_Ptr;
      --  Allocate and initialize a new set of buckets. The buckets are created
      --  in the range Range_Type'First .. Range_Type'First + Size - 1.

      procedure Detach (Elmt : not null Element_Ptr);
      --  Remove an element from an arbitrary bucket chain

      function Find
        (Key   : Key_Type;
         Chain : not null Element_Ptr) return Element_Ptr;
      --  Try to locate the element which contains a particular key within a
      --  bucket chain. If no such element exists, return No_Element.

      procedure Free is
        new Ada.Unchecked_Deallocation (Buckets_Array, Buckets_Array_Ptr);

      procedure Free is
        new Ada.Unchecked_Deallocation (Element, Element_Ptr);

      function Is_Empty_Chain (Chain : not null Element_Ptr) return Boolean;
      --  Determine whether a bucket chain contains only one element, namely
      --  the dummy head.

      ------------
      -- Attach --
      ------------

      procedure Attach
        (Elmt  : not null Element_Ptr;
         Chain : not null Element_Ptr)
      is
      begin
         Chain.Next.Prev := Elmt;
         Elmt.Next  := Chain.Next;
         Chain.Next := Elmt;
         Elmt.Prev  := Chain;
      end Attach;

      --------------------
      -- Create_Buckets --
      --------------------

      function Create_Buckets (Size : Positive) return Buckets_Array_Ptr is
         Low_Bound : constant Range_Type := Range_Type'First;
         Buckets   : Buckets_Array_Ptr;

      begin
         Buckets :=
           new Buckets_Array (Low_Bound .. Low_Bound + Range_Type (Size) - 1);

         --  Ensure that the dummy head of each bucket chain points to itself
         --  in both directions.

         for Index in Buckets'Range loop
            declare
               Bucket : Element renames Buckets (Index);

            begin
               Bucket.Prev := Bucket'Unchecked_Access;
               Bucket.Next := Bucket'Unchecked_Access;
            end;
         end loop;

         return Buckets;
      end Create_Buckets;

      ------------------
      -- Current_Size --
      ------------------

      function Current_Size (T : Table) return Positive is
      begin
         --  The table should have been properly initialized during object
         --  elaboration.

         if T.Buckets = null then
            raise Program_Error;

         --  The size of the table is determined by the number of buckets

         else
            return T.Buckets'Length;
         end if;
      end Current_Size;

      ------------
      -- Detach --
      ------------

      procedure Detach (Elmt : not null Element_Ptr) is
      begin
         if Elmt.Prev /= null and Elmt.Next /= null then
            Elmt.Prev.Next := Elmt.Next;
            Elmt.Next.Prev := Elmt.Prev;
            Elmt.Prev := null;
            Elmt.Next := null;
         end if;
      end Detach;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (T : in out Table) is
         Bucket : Element_Ptr;
         Elmt   : Element_Ptr;

      begin
         --  Inspect the buckets and deallocate bucket chains

         for Index in T.Buckets'Range loop
            Bucket := T.Buckets (Index)'Unchecked_Access;

            --  The current bucket chain contains an element other than the
            --  dummy head.

            while not Is_Empty_Chain (Bucket) loop

               --  Skip the dummy head, remove and deallocate the element

               Elmt := Bucket.Next;
               Detach (Elmt);
               Free   (Elmt);
            end loop;
         end loop;

         --  Deallocate the buckets

         Free (T.Buckets);
      end Finalize;

      ----------
      -- Find --
      ----------

      function Find
        (Key   : Key_Type;
         Chain : not null Element_Ptr) return Element_Ptr
      is
         Elmt : Element_Ptr;

      begin
         --  Skip the dummy head, inspect the bucket chain for an element whose
         --  key matches the requested key. Since each bucket chain is circular
         --  the search must stop once the dummy head is encountered.

         Elmt := Chain.Next;
         while Elmt /= Chain loop
            if Equal (Elmt.Key, Key) then
               return Elmt;
            end if;

            Elmt := Elmt.Next;
         end loop;

         return No_Element;
      end Find;

      ---------
      -- Get --
      ---------

      function Get (T : Table; Key : Key_Type) return Value_Type is
         Bucket : Element_Ptr;
         Elmt   : Element_Ptr;

      begin
         --  Obtain the bucket chain where the (key, value) pair should reside
         --  by calculating the proper hash location.

         Bucket := T.Buckets (Hash (Key, Current_Size (T)))'Unchecked_Access;

         --  Try to find an element whose key matches the requested key

         Elmt := Find (Key, Bucket);

         --  The hash table does not contain a matching (key, value) pair

         if Elmt = No_Element then
            return No_Value;
         else
            return Elmt.Val;
         end if;
      end Get;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (T : in out Table) is
      begin
         pragma Assert (T.Buckets = null);

         T.Buckets       := Create_Buckets (Initial_Size);
         T.Element_Count := 0;
      end Initialize;

      --------------------
      -- Is_Empty_Chain --
      --------------------

      function Is_Empty_Chain (Chain : not null Element_Ptr) return Boolean is
      begin
         return Chain.Next = Chain and Chain.Prev = Chain;
      end Is_Empty_Chain;

      ------------
      -- Remove --
      ------------

      procedure Remove (T : in out Table; Key : Key_Type) is
         Bucket : Element_Ptr;
         Elmt   : Element_Ptr;

      begin
         --  Obtain the bucket chain where the (key, value) pair should reside
         --  by calculating the proper hash location.

         Bucket := T.Buckets (Hash (Key, Current_Size (T)))'Unchecked_Access;

         --  Try to find an element whose key matches the requested key

         Elmt := Find (Key, Bucket);

         --  Remove and deallocate the (key, value) pair

         if Elmt /= No_Element then
            Detach (Elmt);
            Free   (Elmt);
         end if;
      end Remove;

      ---------
      -- Set --
      ---------

      procedure Set
        (T   : in out Table;
         Key : Key_Type;
         Val : Value_Type)
      is
         Curr_Size : constant Positive := Current_Size (T);

         procedure Grow;
         --  Grow the table to a new size according to the desired percentage
         --  and relocate all existing elements to the new buckets.

         ----------
         -- Grow --
         ----------

         procedure Grow is
            Buckets     : Buckets_Array_Ptr;
            Elmt        : Element_Ptr;
            Hash_Loc    : Range_Type;
            Old_Bucket  : Element_Ptr;
            Old_Buckets : Buckets_Array_Ptr := T.Buckets;
            Size        : Positive;

         begin
            --  Calculate the new size and allocate a new set of buckets. Note
            --  that a table with a small size or a small growth percentage may
            --  not always grow (for example, 10 buckets and 3% increase). In
            --  that case, enforce a minimum increase.

            Size :=
              Positive'Max (Curr_Size * ((100 + Growth_Percentage) / 100),
                            Min_Size_Increase);
            Buckets := Create_Buckets (Size);

            --  Inspect the old buckets and transfer all elements by rehashing
            --  all (key, value) pairs in the new buckets.

            for Index in Old_Buckets'Range loop
               Old_Bucket := Old_Buckets (Index)'Unchecked_Access;

               --  The current bucket chain contains an element other than the
               --  dummy head.

               while not Is_Empty_Chain (Old_Bucket) loop

                  --  Skip the dummy head and find the new hash location

                  Elmt     := Old_Bucket.Next;
                  Hash_Loc := Hash (Elmt.Key, Size);

                  --  Remove the element from the old buckets and insert it
                  --  into the new buckets. Note that there is no need to check
                  --  for duplicates because the hash table did not have any to
                  --  begin with.

                  Detach (Elmt);
                  Attach
                    (Elmt  => Elmt,
                     Chain => Buckets (Hash_Loc)'Unchecked_Access);
               end loop;
            end loop;

            --  Associate the new buckets with the table and reclaim the
            --  storage occupied by the old buckets.

            T.Buckets := Buckets;

            Free (Old_Buckets);
         end Grow;

         --  Local variables

         subtype LLF is Long_Long_Float;

         Count    : Natural renames T.Element_Count;
         Bucket   : Element_Ptr;
         Hash_Loc : Range_Type;

      --  Start of processing for Set

      begin
         --  Find the bucket where the (key, value) pair should be inserted by
         --  computing the proper hash location.

         Hash_Loc := Hash (Key, Curr_Size);
         Bucket   := T.Buckets (Hash_Loc)'Unchecked_Access;

         --  Ensure that the key is not already present in the bucket in order
         --  to avoid duplicates.

         if Find (Key, Bucket) = No_Element then
            Attach
              (Elmt  => new Element'(Key, Val, null, null),
               Chain => Bucket);
            Count := Count + 1;

            --  Multiple insertions may cause long bucket chains and decrease
            --  the performance of basic operations. If this is the case, grow
            --  the table and rehash all existing elements.

            if (LLF (Count) / LLF (Curr_Size)) > LLF (Load_Factor) then
               Grow;
            end if;
         end if;
      end Set;
   end Load_Factor_HTable;

end GNAT.Dynamic_HTables;
