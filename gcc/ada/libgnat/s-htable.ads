------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . H T A B L E                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2021, AdaCore                     --
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
--  provides a very simple abstraction that associates one element to one
--  key value and takes care of all allocations automatically using the heap.
--  The Static_HTable package provides a more complex interface that allows
--  complete control over allocation.

pragma Compiler_Unit_Warning;

package System.HTable is
   pragma Preelaborate;

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
      --  a given key.

      type Key is private;
      with function Hash  (F : Key)      return Header_Num;
      with function Equal (F1, F2 : Key) return Boolean;

   package Simple_HTable is

      procedure Set (K : Key; E : Element);
      --  Associates an element with a given key. Overrides any previously
      --  associated element.

      procedure Reset;
      --  Removes and frees all elements in the table

      function Get (K : Key) return Element;
      --  Returns the Element associated with a key or No_Element if the
      --  given key has no associated element.

      procedure Remove (K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      function Get_First return Element;
      --  Returns No_Element if the HTable is empty, otherwise returns one
      --  non specified element. There is no guarantee that two calls to this
      --  function will return the same element.

      function Get_Next return Element;
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or No_Element if
      --  there is no such element. If there is no call to Set in between
      --  Get_Next calls, all the elements of the HTable will be traversed.

      procedure Get_First (K : in out Key; E : out Element);
      --  This version of the iterator returns a key/element pair. A non-
      --  specified entry is returned, and there is no guarantee that two
      --  calls to this procedure will return the same element. If the table
      --  is empty, E is set to No_Element, and K is unchanged, otherwise
      --  K and E are set to the first returned entry.

      procedure Get_Next (K : in out Key; E : out Element);
      --  This version of the iterator returns a key/element pair. It returns
      --  a non-specified element that has not been returned since the last
      --  call to Get_First. If there is no remaining element, then E is set
      --  to No_Element, and the value in K is unchanged, otherwise K and E
      --  are set to the next entry. If there is no call to Set in between
      --  Get_Next calls, all the elements of the HTable will be traversed.

   end Simple_HTable;

   -------------------
   -- Static_HTable --
   -------------------

   --  A low-level Hash-Table abstraction, not as easy to instantiate as
   --  Simple_HTable but designed to allow complete control over the
   --  allocation of necessary data structures. Particularly useful when
   --  dynamic allocation is not desired. The model is that each Element
   --  contains its own Key that can be retrieved by Get_Key. Furthermore,
   --  Element provides a link that can be used by the HTable for linking
   --  elements with same hash codes:

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
      --  The type of element to be stored. This is historically part of the
      --  interface, even though it is not used at all in the operations of
      --  the package.

      pragma Warnings (Off, Element);
      --  We have to kill warnings here, because Element is and always
      --  has been unreferenced, but we cannot remove it at this stage,
      --  since this unit is in wide use, and it certainly seems harmless.

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

      procedure Reset;
      --  Resets the hash table by setting all its elements to Null_Ptr. The
      --  effect is to clear the hash table so that it can be reused. For the
      --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
      --  null, this is only needed if the same table is reused in a new
      --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
      --  other than null, then Reset must be called before the first use
      --  of the hash table.

      procedure Set (E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key
      --  or null if none.

      function Present (K : Key) return Boolean;
      --  True if an element whose Get_Key is K is in the table

      function Set_If_Not_Present (E : Elmt_Ptr) return Boolean;
      --  If Present (Get_Key (E)), returns False. Otherwise, does Set (E), and
      --  then returns True. Present (Get_Key (E)) is always True afterward,
      --  and the result True indicates E is newly Set.

      procedure Remove (K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      function Get_First return Elmt_Ptr;
      --  Returns Null_Ptr if the HTable is empty, otherwise returns one
      --  non specified element. There is no guarantee that two calls to this
      --  function will return the same element.

      function Get_Next return Elmt_Ptr;
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or Null_Ptr if
      --  there is no such element or Get_First has never been called. If
      --  there is no call to 'Set' in between Get_Next calls, all the
      --  elements of the HTable will be traversed.

   end Static_HTable;

   ----------
   -- Hash --
   ----------

   --  A generic hashing function working on String keys

   generic
      type Header_Num is range <>;
   function Hash (Key : String) return Header_Num;

end System.HTable;
