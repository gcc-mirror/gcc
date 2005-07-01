------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . H T A B L E                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2005 AdaCore                      --
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

--  Note: actual code is found in System.HTable (s-htable.ads/adb) since
--  this facility is accessed from run time routines, but clients should
--  always access the version supplied via GNAT.HTable.

with System.HTable;

package GNAT.HTable is
pragma Preelaborate (HTable);

pragma Elaborate_Body;
--  The elaborate body is because we have a dummy body to deal with bootstrap
--  path problems (we used to have a real body, and now we don't need it any
--  more, but the bootstrap requires that we have a dummy body, since otherwise
--  the old body gets picked up.

   -------------------
   -- Simple_HTable --
   -------------------

   --  A simple hash table abstraction, easy to instantiate, easy to use.
   --  The table associates one element to one key with the procedure Set.
   --  Get retrieves the Element stored for a given Key. The efficiency of
   --  retrieval is function of the size of the Table parameterized by
   --  Header_Num and the hashing function Hash.

   generic package Simple_HTable renames System.HTable.Simple_HTable;

   --  For convenience of reference here is what this package has in it:

   --  generic
   --     type Header_Num is range <>;
   --     --  An integer type indicating the number and range of hash headers

   --     type Element is private;
   --     --  The type of element to be stored

   --     No_Element : Element;
   --     --  The object that is returned by Get when no element has been set
   --     --  for a given key

   --     type Key is private;
   --     with function Hash  (F : Key)      return Header_Num;
   --     with function Equal (F1, F2 : Key) return Boolean;

   --  package Simple_HTable is

   --     procedure Set (K : Key; E : Element);
   --     --  Associates an element with a given key. Overrides any previously
   --     --  associated element.

   --     procedure Reset;
   --     --  Removes and frees all elements in the table

   --     function Get (K : Key) return Element;
   --     --  Returns the Element associated with a key or No_Element if the
   --     --  given key has not associated element

   --     procedure Remove (K : Key);
   --     --  Removes the latest inserted element pointer associated with the
   --     --  given key if any, does nothing if none.

   --     function Get_First return Element;
   --     --  Returns No_Element if the HTable is empty, otherwise returns one
   --     --  non specified element. There is no guarantee that 2 calls to
   --     --  this function will return the same element.

   --     function Get_Next return Element;
   --     --  Returns a non-specified element that has not been returned by the
   --     --  same function since the last call to Get_First or No_Element if
   --     --  there is no such element. If there is no call to 'Set' in between
   --     --  Get_Next calls, all the elements of the HTable will be traversed.
   --  end Simple_HTable;

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

   generic package Static_HTable renames System.HTable.Static_HTable;

   --  For convenience of reference here is what this package has in it:

   --  generic
   --     type Header_Num is range <>;
   --     --  An integer type indicating the number and range of hash headers.

   --     type Element (<>) is limited private;
   --     --  The type of element to be stored

   --     type Elmt_Ptr is private;
   --     --  The type used to reference an element (will usually be an
   --     --  access type, but could be some other form of type such as
   --     --  an integer type).

   --     Null_Ptr : Elmt_Ptr;
   --     --  The null value of the Elmt_Ptr type.

   --     with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
   --     with function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
   --     --  The type must provide an internal link for the sake of the
   --     --  staticness of the HTable.

   --     type Key is limited private;
   --     with function Get_Key (E : Elmt_Ptr) return Key;
   --     with function Hash    (F : Key)      return Header_Num;
   --     with function Equal   (F1, F2 : Key) return Boolean;

   --  package Static_HTable is

   --     procedure Reset;
   --     --  Resets the hash table by setting all its elements to Null_Ptr.
   --     --  The effect is to clear the hash table so that it can be reused.
   --     --  For the most common case where Elmt_Ptr is an access type, and
   --     --  Null_Ptr is null, this is only needed if the same table is
   --     --  reused in a new context. If Elmt_Ptr is other than an access
   --     --  type, or Null_Ptr is other than null, then Reset must be called
   --     --  before the first use of the hash table.

   --     procedure Set (E : Elmt_Ptr);
   --     --  Insert the element pointer in the HTable

   --     function Get (K : Key) return Elmt_Ptr;
   --     --  Returns the latest inserted element pointer with the given Key
   --     --  or null if none.

   --     procedure Remove (K : Key);
   --     --  Removes the latest inserted element pointer associated with the
   --     --  given key if any, does nothing if none.

   --     function Get_First return Elmt_Ptr;
   --     --  Returns Null_Ptr if the HTable is empty, otherwise returns one
   --     --  non specified element. There is no guarantee that 2 calls to
   --     --  this function will return the same element.

   --     function Get_Next return Elmt_Ptr;
   --     --  Returns a non-specified element that has not been returned by
   --     --  the same function since the last call to Get_First or Null_Ptr
   --     --  if there is no such element or Get_First has bever been called.
   --     --  If there is no call to 'Set' in between Get_Next calls, all
   --     --  the elements of the HTable will be traversed.

   --  end Static_HTable;

   ----------
   -- Hash --
   ----------

   --  A generic hashing function working on String keys

   generic function Hash renames System.HTable.Hash;

   --  generic
   --     type Header_Num is range <>;
   --  function Hash (Key : String) return Header_Num;

end GNAT.HTable;
