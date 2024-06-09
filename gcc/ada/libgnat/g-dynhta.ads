------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D Y N A M I C _ H T A B L E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2024, AdaCore                     --
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

--  Hash table searching routines

--  This package contains two separate packages. The Simple_HTable package
--  provides a very simple abstraction that associates one element to one key
--  value and takes care of all allocations automatically using the heap. The
--  Static_HTable package provides a more complex interface that allows full
--  control over allocation.

--  This package provides a facility similar to that of GNAT.HTable, except
--  that this package declares types that can be used to define dynamic
--  instances of hash tables, while instantiations in GNAT.HTable creates a
--  single instance of the hash table.

--  Note that this interface should remain synchronized with those in
--  GNAT.HTable to keep as much coherency as possible between these two
--  related units.

--  Note: this unit is used during bootstrap, see ADA_GENERATED_FILES in
--  gcc-interface/Make-lang.in for details on the constraints.

package GNAT.Dynamic_HTables is

   function Hash_Two_Keys
     (Left  : Bucket_Range_Type;
      Right : Bucket_Range_Type) return Bucket_Range_Type;
   pragma Inline (Hash_Two_Keys);
   --  Obtain the hash value of keys Left and Right

   -------------------
   -- Static_HTable --
   -------------------

   --  A low-level Hash-Table abstraction, not as easy to instantiate as
   --  Simple_HTable. This mirrors the interface of GNAT.HTable.Static_HTable,
   --  but does require dynamic allocation (since we allow multiple instances
   --  of the table). The model is that each Element contains its own Key that
   --  can be retrieved by Get_Key. Furthermore, Element provides a link that
   --  can be used by the HTable for linking elements with same hash codes:

   --       Element

   --         +-------------------+
   --         |       Key         |
   --         +-------------------+
   --         :    other data     :
   --         +-------------------+
   --         |     Next Elmt     |
   --         +-------------------+

   generic
      type Header_Num is range <>;
      --  An integer type indicating the number and range of hash headers

      type Element (<>) is limited private;
      --  The type of element to be stored

      type Elmt_Ptr is private;
      --  The type used to reference an element (will usually be an access
      --  type, but could be some other form of type such as an integer type).

      Null_Ptr : Elmt_Ptr;
      --  The null value of the Elmt_Ptr type

      with function Next (E : Elmt_Ptr) return Elmt_Ptr;
      with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      --  The type must provide an internal link for the sake of the
      --  staticness of the HTable.

      type Key is limited private;
      with function Get_Key (E : Elmt_Ptr) return Key;
      with function Hash (F : Key) return Header_Num;
      with function Equal (F1 : Key; F2 : Key) return Boolean;

   package Static_HTable is
      type Instance is private;
      Nil : constant Instance;

      procedure Reset (T : in out Instance);
      --  Resets the hash table by releasing all memory associated with it. The
      --  hash table can safely be reused after this call. For the most common
      --  case where Elmt_Ptr is an access type, and Null_Ptr is null, this is
      --  only needed if the same table is reused in a new context. If Elmt_Ptr
      --  is other than an access type, or Null_Ptr is other than null, then
      --  Reset must be called before the first use of the hash table.

      procedure Set (T : in out Instance; E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (T : Instance; K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key or
      --  null if none.

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the given
      --  key if any, does nothing if none.

      function Get_First (T : Instance) return Elmt_Ptr;
      --  Returns Null_Ptr if the Htable is empty, otherwise returns one
      --  unspecified element. There is no guarantee that 2 calls to this
      --  function will return the same element.

      function Get_Next (T : Instance) return Elmt_Ptr;
      --  Returns an unspecified element that has not been returned by the same
      --  function since the last call to Get_First or Null_Ptr if there is no
      --  such element or Get_First has never been called. If there is no call
      --  to 'Set' in between Get_Next calls, all the elements of the Htable
      --  will be traversed.

   private
      type Table_Type is array (Header_Num) of Elmt_Ptr;

      type Instance_Data is record
         Table            : Table_Type;
         Iterator_Index   : Header_Num;
         Iterator_Ptr     : Elmt_Ptr;
         Iterator_Started : Boolean := False;
      end record;

      type Instance is access all Instance_Data;

      Nil : constant Instance := null;
   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   --  A simple hash table abstraction, easy to instantiate, easy to use.
   --  The table associates one element to one key with the procedure Set.
   --  Get retrieves the Element stored for a given Key. The efficiency of
   --  retrieval is function of the size of the Table parameterized by
   --  Header_Num and the hashing function Hash.

   generic
      type Header_Num is range <>;
      --  An integer type indicating the number and range of hash headers

      type Element is private;
      --  The type of element to be stored

      No_Element : Element;
      --  The object that is returned by Get when no element has been set for
      --  a given key

      type Key is private;
      with function Hash (F : Key) return Header_Num;
      with function Equal (F1 : Key; F2 : Key) return Boolean;

   package Simple_HTable is
      type Instance is private;
      Nil : constant Instance;

      type Key_Option (Present : Boolean := False) is record
         case Present is
            when True  => K : Key;
            when False => null;
         end case;
      end record;

      procedure Set (T : in out Instance; K : Key; E : Element);
      --  Associates an element with a given key. Overrides any previously
      --  associated element.

      procedure Reset (T : in out Instance);
      --  Releases all memory associated with the table. The table can be
      --  reused after this call (it is automatically allocated on the first
      --  access to the table).

      function Get (T : Instance; K : Key) return Element;
      --  Returns the Element associated with a key or No_Element if the given
      --  key has not associated element

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the given
      --  key if any, does nothing if none.

      function Get_First (T : Instance) return Element;
      --  Returns No_Element if the Htable is empty, otherwise returns one
      --  unspecified element. There is no guarantee that two calls to this
      --  function will return the same element, if the Htable has been
      --  modified between the two calls.

      function Get_First_Key (T : Instance) return Key_Option;
      --  Returns an option type giving an unspecified key. If the Htable
      --  is empty, the discriminant will have field Present set to False,
      --  otherwise its Present field is set to True and the field K contains
      --  the key. There is no guarantee that two calls to this function will
      --  return the same key, if the Htable has been modified between the two
      --  calls.

      function Get_Next (T : Instance) return Element;
      --  Returns an unspecified element that has not been returned by the
      --  same function since the last call to Get_First or No_Element if
      --  there is no such element. If there is no call to 'Set' in between
      --  Get_Next calls, all the elements of the Htable will be traversed.
      --  To guarantee that all the elements of the Htable will be traversed,
      --  no modification of the Htable (Set, Reset, Remove) should occur
      --  between a call to Get_First and subsequent consecutive calls to
      --  Get_Next, until one of these calls returns No_Element.

      function Get_Next_Key (T : Instance) return Key_Option;
      --  Same as Get_Next except that this returns an option type having field
      --  Present set either to False if there no key never returned before by
      --  either Get_First_Key or this very same function, or to True if there
      --  is one, with the field K containing the key specified as before. The
      --  same restrictions apply as Get_Next.

   private
      type Element_Wrapper;
      type Elmt_Ptr is access all Element_Wrapper;
      type Element_Wrapper is record
         K    : Key;
         E    : Element;
         Next : Elmt_Ptr;
      end record;

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      function  Get_Key  (E : Elmt_Ptr) return Key;

      package Tab is new Static_HTable
        (Header_Num => Header_Num,
         Element    => Element_Wrapper,
         Elmt_Ptr   => Elmt_Ptr,
         Null_Ptr   => null,
         Set_Next   => Set_Next,
         Next       => Next,
         Key        => Key,
         Get_Key    => Get_Key,
         Hash       => Hash,
         Equal      => Equal);

      type Instance is new Tab.Instance;
      Nil : constant Instance := Instance (Tab.Nil);
   end Simple_HTable;

   -------------------------
   -- Dynamic_Hash_Tables --
   -------------------------

   --  The following package offers a hash table abstraction with the following
   --  characteristics:
   --
   --    * Dynamic resizing based on load factor
   --    * Creation of multiple instances, of different sizes
   --    * Iterable keys
   --
   --  This type of hash table is best used in scenarios where the size of the
   --  key set is not known. The dynamic resizing aspect allows for performance
   --  to remain within reasonable bounds as the size of the key set grows.
   --
   --  The following use pattern must be employed when operating this table:
   --
   --    Table : Dynamic_Hash_Table := Create (<some size>);
   --
   --    <various operations>
   --
   --    Destroy (Table);
   --
   --  The destruction of the table reclaims all storage occupied by it.

   --  The following type denotes the multiplicative factor used in expansion
   --  and compression of the hash table.

   subtype Factor_Type is Bucket_Range_Type range 2 .. 100;

   --  The following type denotes the threshold range used in expansion and
   --  compression of the hash table.

   subtype Threshold_Type is Long_Float range 0.0 .. Long_Float'Last;

   generic
      type Key_Type is private;
      type Value_Type is private;
      --  The types of the key-value pairs stored in the hash table

      No_Value : Value_Type;
      --  An indicator for a non-existent value

      Expansion_Threshold : Threshold_Type;
      Expansion_Factor    : Factor_Type;
      --  Once the load factor goes over Expansion_Threshold, the size of the
      --  buckets is increased using the formula
      --
      --    New_Size = Old_Size * Expansion_Factor
      --
      --  An Expansion_Threshold of 1.5 and Expansion_Factor of 2 indicate that
      --  the size of the buckets will be doubled once the load factor exceeds
      --  1.5.

      Compression_Threshold : Threshold_Type;
      Compression_Factor    : Factor_Type;
      --  Once the load factor drops below Compression_Threshold, the size of
      --  the buckets is decreased using the formula
      --
      --    New_Size = Old_Size / Compression_Factor
      --
      --  A Compression_Threshold of 0.5 and Compression_Factor of 2 indicate
      --  that the size of the buckets will be halved once the load factor
      --  drops below 0.5.

      with function "="
             (Left  : Key_Type;
              Right : Key_Type) return Boolean;

      with procedure Destroy_Value (Val : in out Value_Type);
      --  Value destructor

      with function Hash (Key : Key_Type) return Bucket_Range_Type;
      --  Map an arbitrary key into the range of buckets

   package Dynamic_Hash_Tables is

      ----------------------
      -- Table operations --
      ----------------------

      --  The following type denotes a hash table handle. Each instance must be
      --  created using routine Create.

      type Dynamic_Hash_Table is private;
      Nil : constant Dynamic_Hash_Table;

      function Contains
        (T   : Dynamic_Hash_Table;
         Key : Key_Type) return Boolean;
      --  Determine whether key Key exists in hash table T

      function Create (Initial_Size : Positive) return Dynamic_Hash_Table;
      --  Create a new table with bucket capacity Initial_Size. This routine
      --  must be called at the start of a hash table's lifetime.

      procedure Delete
        (T   : Dynamic_Hash_Table;
         Key : Key_Type);
      --  Delete the value which corresponds to key Key from hash table T. The
      --  routine has no effect if the value is not present in the hash table.
      --  This action will raise Iterated if the hash table has outstanding
      --  iterators. If the load factor drops below Compression_Threshold, the
      --  size of the buckets is decreased by Copression_Factor.

      procedure Destroy (T : in out Dynamic_Hash_Table);
      --  Destroy the contents of hash table T, rendering it unusable. This
      --  routine must be called at the end of a hash table's lifetime. This
      --  action will raise Iterated if the hash table has outstanding
      --  iterators.

      function Get
        (T   : Dynamic_Hash_Table;
         Key : Key_Type) return Value_Type;
      --  Obtain the value which corresponds to key Key from hash table T. If
      --  the value does not exist, return No_Value.

      function Is_Empty (T : Dynamic_Hash_Table) return Boolean;
      --  Determine whether hash table T is empty

      function Present (T : Dynamic_Hash_Table) return Boolean;
      --  Determine whether hash table T exists

      procedure Put
        (T     : Dynamic_Hash_Table;
         Key   : Key_Type;
         Value : Value_Type);
      --  Associate value Value with key Key in hash table T. If the table
      --  already contains a mapping of the same key to a previous value, the
      --  previous value is overwritten. This action will raise Iterated if
      --  the hash table has outstanding iterators. If the load factor goes
      --  over Expansion_Threshold, the size of the buckets is increased by
      --  Expansion_Factor.

      procedure Reset (T : Dynamic_Hash_Table);
      --  Destroy the contents of hash table T, and reset it to its initial
      --  created state. This action will raise Iterated if the hash table
      --  has outstanding iterators.

      function Size (T : Dynamic_Hash_Table) return Natural;
      --  Obtain the number of key-value pairs in hash table T

      -------------------------
      -- Iterator operations --
      -------------------------

      --  The following type represents a key iterator. An iterator locks
      --  all mutation operations, and unlocks them once it is exhausted.
      --  The iterator must be used with the following pattern:
      --
      --    Iter := Iterate (My_Table);
      --    while Has_Next (Iter) loop
      --       Key := Next (Iter);
      --       . . .
      --    end loop;
      --
      --  It is possible to advance the iterator by using Next only, however
      --  this risks raising Iterator_Exhausted.

      type Iterator is private;

      function Has_Next (Iter : Iterator) return Boolean;
      --  Determine whether iterator Iter has more keys to examine. If the
      --  iterator has been exhausted, restore all mutation functionality of
      --  the associated hash table.

      function Iterate (T : Dynamic_Hash_Table) return Iterator;
      --  Obtain an iterator over the keys of hash table T. This action locks
      --  all mutation functionality of the associated hash table.

      procedure Next (Iter : in out Iterator; Key : out Key_Type);
      --  Return the current key referenced by iterator Iter and advance to
      --  the next available key. If the iterator has been exhausted and
      --  further attempts are made to advance it, this routine restores
      --  mutation functionality of the associated hash table, and then
      --  raises Iterator_Exhausted.

   private
      --  The following type represents a doubly linked list node used to
      --  store a key-value pair. There are several reasons to use a doubly
      --  linked list:
      --
      --    * Most read and write operations utilize the same primitve
      --      routines to locate, create, and delete a node, allowing for
      --      greater degree of code sharing.
      --
      --    * Special cases are eliminated by maintaining a circular node
      --      list with a dummy head (see type Bucket_Table).
      --
      --  A node is said to be "valid" if it is non-null, and does not refer to
      --  the dummy head of some bucket.

      type Node;
      type Node_Ptr is access all Node;
      type Node is record
         Key   : Key_Type;
         Value : Value_Type := No_Value;
         --  Key-value pair stored in a bucket

         Prev : Node_Ptr := null;
         Next : Node_Ptr := null;
      end record;

      --  The following type represents a bucket table. Each bucket contains a
      --  circular doubly linked list of nodes with a dummy head. Initially,
      --  the head does not refer to itself. This is intentional because it
      --  improves the performance of creation, compression, and expansion by
      --  avoiding a separate pass to link a head to itself. Several routines
      --  ensure that the head is properly formed.

      type Bucket_Table is array (Bucket_Range_Type range <>) of aliased Node;
      type Bucket_Table_Ptr is access Bucket_Table;

      --  The following type represents a hash table

      type Dynamic_Hash_Table_Attributes is record
         Buckets : Bucket_Table_Ptr := null;
         --  Reference to the compressing / expanding buckets

         Initial_Size : Bucket_Range_Type := 0;
         --  The initial size of the buckets as specified at creation time

         Iterators : Natural := 0;
         --  Number of outstanding iterators

         Pairs : Natural := 0;
         --  Number of key-value pairs in the buckets
      end record;

      type Dynamic_Hash_Table is access Dynamic_Hash_Table_Attributes;
      Nil : constant Dynamic_Hash_Table := null;

      --  The following type represents a key iterator

      type Iterator is record
         Curr_Idx : Bucket_Range_Type := 0;
         --  Index of the current bucket being examined. This index is always
         --  kept within the range of the buckets.

         Curr_Nod : Node_Ptr := null;
         --  Reference to the current node being examined within the current
         --  bucket. The invariant of the iterator requires that this field
         --  always point to a valid node. A value of null indicates that the
         --  iterator is exhausted.

         Table : Dynamic_Hash_Table := null;
         --  Reference to the associated hash table
      end record;
   end Dynamic_Hash_Tables;

end GNAT.Dynamic_HTables;
