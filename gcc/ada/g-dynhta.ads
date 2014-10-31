------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D Y N A M I C _ H T A B L E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2014, AdaCore                     --
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

--  This package contains three separate packages. The Simple_HTable package
--  provides a very simple abstraction that associates one element to one key
--  value and takes care of all allocations automatically using the heap. The
--  Static_HTable package provides a more complex interface that allows full
--  control over allocation. The Load_Factor_HTable package provides a more
--  complex abstraction where collisions are resolved by chaining, and the
--  table grows by a percentage after the load factor has been exceeded.

--  This package provides a facility similar to that of GNAT.HTable, except
--  that this package declares types that can be used to define dynamic
--  instances of hash tables, while instantiations in GNAT.HTable creates a
--  single instance of the hash table.

--  Note that this interface should remain synchronized with those in
--  GNAT.HTable to keep as much coherency as possible between these two
--  related units.

private with Ada.Finalization;

package GNAT.Dynamic_HTables is

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

      with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      with function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      --  The type must provide an internal link for the sake of the
      --  staticness of the HTable.

      type Key is limited private;
      with function Get_Key (E : Elmt_Ptr) return Key;
      with function Hash    (F : Key)      return Header_Num;
      with function Equal   (F1, F2 : Key) return Boolean;

   package Static_HTable is

      type Instance is private;
      Nil : constant Instance;

      procedure Reset (T : in out Instance);
      --  Resets the hash table by releasing all memory associated with
      --  it. The hash table can safely be reused after this call. For the
      --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
      --  null, this is only needed if the same table is reused in a new
      --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
      --  other than null, then Reset must be called before the first use of
      --  the hash table.

      procedure Set (T : in out Instance; E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (T : Instance; K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key
      --  or null if none.

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      function Get_First (T : Instance) return Elmt_Ptr;
      --  Returns Null_Ptr if the Htable is empty, otherwise returns one
      --  non specified element. There is no guarantee that 2 calls to this
      --  function will return the same element.

      function Get_Next (T : Instance) return Elmt_Ptr;
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or Null_Ptr if
      --  there is no such element or Get_First has never been called. If
      --  there is no call to 'Set' in between Get_Next calls, all the
      --  elements of the Htable will be traversed.

   private
      type Instance_Data;
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
      with function Hash  (F : Key)      return Header_Num;
      with function Equal (F1, F2 : Key) return Boolean;

   package Simple_HTable is

      type Instance is private;
      Nil : constant Instance;

      procedure Set (T : in out Instance; K : Key; E : Element);
      --  Associates an element with a given key. Overrides any previously
      --  associated element.

      procedure Reset (T : in out Instance);
      --  Releases all memory associated with the table. The table can be
      --  reused after this call (it is automatically allocated on the first
      --  access to the table).

      function Get (T : Instance; K : Key) return Element;
      --  Returns the Element associated with a key or No_Element if the
      --  given key has not associated element

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      function Get_First (T : Instance) return Element;
      --  Returns No_Element if the Htable is empty, otherwise returns one
      --  non specified element. There is no guarantee that two calls to this
      --  function will return the same element, if the Htable has been
      --  modified between the two calls.

      function Get_Next (T : Instance) return Element;
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or No_Element if
      --  there is no such element. If there is no call to 'Set' in between
      --  Get_Next calls, all the elements of the Htable will be traversed.
      --  To guarantee that all the elements of the Htable will be traversed,
      --  no modification of the Htable (Set, Reset, Remove) should occur
      --  between a call to Get_First and subsequent consecutive calls to
      --  Get_Next, until one of these calls returns No_Element.

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

   ------------------------
   -- Load_Factor_HTable --
   ------------------------

   --  A simple hash table abstraction capable of growing once a threshold has
   --  been exceeded. Collisions are resolved by chaining elements onto lists
   --  hanging from individual buckets. This implementation does not make any
   --  effort to minimize the number of necessary rehashes once the table has
   --  been expanded, hence the term "simple".

   --  WARNING: This hash table implementation utilizes dynamic allocation.
   --  Storage reclamation is performed by the hash table.

   --  WARNING: This hash table implementation is not thread-safe. To achieve
   --  proper concurrency and synchronization, wrap an instance of a table in
   --  a protected object.

   generic
      type Range_Type is range <>;
      --  The underlying range of the hash table. Note that this type must be
      --  large enough to accommodate multiple expansions of the table.

      type Key_Type is private;
      type Value_Type is private;
      --  The types of the (key, value) pair stored in the hash table

      No_Value : Value_Type;
      --  A predefined value denoting a non-existent value

      Initial_Size : Positive;
      --  The starting size of the hash table. The hash table must contain at
      --  least one bucket.

      Growth_Percentage : Positive;
      --  The amount of increase expressed as a percentage. The hash table must
      --  grow by at least 1%. To illustrate, a value of 100 will increase the
      --  table by 100%, effectively doubling its size.

      Load_Factor : Float;
      --  The ratio of the elements stored within the hash table divided by the
      --  current size of the table. This value acts as the growth threshold.
      --  If exceeded, the hash table is expanded by Growth_Percentage.

      with function Equal
        (Left  : Key_Type;
         Right : Key_Type) return Boolean;

      with function Hash
        (Key  : Key_Type;
         Size : Positive) return Range_Type;
      --  Parameter Size denotes the current size of the hash table

   package Load_Factor_HTable is
      type Table is tagged limited private;

      function Current_Size (T : Table) return Positive;
      --  Obtain the current size of the table

      function Get (T : Table; Key : Key_Type) return Value_Type;
      --  Obtain the value associated with a key. This routine returns No_Value
      --  if the key is not present in the hash table.

      procedure Remove (T : in out Table; Key : Key_Type);
      --  Remove the value associated with the given key. This routine has no
      --  effect if the key is not present in the hash table.

      procedure Set
        (T   : in out Table;
         Key : Key_Type;
         Val : Value_Type);
      --  Associate a value with a given key. This routine has no effect if the
      --  the (key, value) pair is already present in the hash table. Note that
      --  this action may cause the table to grow.

   private
      --  The following types model a bucket chain. Note that the key is also
      --  stored for rehashing purposes.

      type Element;
      type Element_Ptr is access all Element;
      type Element is record
         Key  : Key_Type;
         Val  : Value_Type;
         Prev : Element_Ptr := null;
         Next : Element_Ptr := null;
      end record;

      No_Element : constant Element_Ptr := null;

      --  The following types model the buckets of the hash table. Each bucket
      --  has a dummy head to facilitate insertion and deletion of elements.

      type Buckets_Array is array (Range_Type range <>) of aliased Element;
      type Buckets_Array_Ptr is access all Buckets_Array;

      type Table is new Ada.Finalization.Limited_Controlled with record
         Buckets : Buckets_Array_Ptr := null;

         Element_Count : Natural := 0;
         --  The number of (key, value) pairs stored in the hash table
      end record;

      procedure Finalize (T : in out Table);
      --  Destroy the contents of a hash table by reclaiming all storage used
      --  by buckets and their respective chains.

      procedure Initialize (T : in out Table);
      --  Create a hash table with buckets within the range Range_Type'First ..
      --  Range_Type'First + Initial_Size - 1.

   end Load_Factor_HTable;

end GNAT.Dynamic_HTables;
