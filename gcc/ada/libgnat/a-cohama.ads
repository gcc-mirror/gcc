------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            A D A . C O N T A I N E R S . H A S H E D _ M A P S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Iterator_Interfaces;

private with Ada.Containers.Hash_Tables;
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Text_Buffers;

--  The language-defined generic package Containers.Hashed_Maps provides
--  private types Map and Cursor, and a set of operations for each type. A map
--  container allows an arbitrary type to be used as a key to find the element
--  associated with that key. A hashed map uses a hash function to organize the
--  keys.
--
--  A map contains pairs of keys and elements, called nodes. Map cursors
--  designate nodes, but also can be thought of as designating an element (the
--  element contained in the node) for consistency with the other containers.
--  There exists an equivalence relation on keys, whose definition is different
--  for hashed maps and ordered maps. A map never contains two or more nodes
--  with equivalent keys. The length of a map is the number of nodes it
--  contains.
--
--  Each nonempty map has two particular nodes called the first node and the
--  last node (which may be the same). Each node except for the last node has a
--  successor node. If there are no other intervening operations, starting with
--  the first node and repeatedly going to the successor node will visit each
--  node in the map exactly once until the last node is reached.

generic
   type Key_Type is private;
   type Element_Type is private;

   with function Hash (Key : Key_Type) return Hash_Type;
   --  The actual function for the generic formal function Hash is expected to
   --  return the same value each time it is called with a particular key
   --  value. For any two equivalent key values, the actual for Hash is
   --  expected to return the same value. If the actual for Hash behaves in
   --  some other manner, the behavior of this package is unspecified. Which
   --  subprograms of this package call Hash, and how many times they call it,
   --  is unspecified.

   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   --  The actual function for the generic formal function Equivalent_Keys on
   --  Key_Type values is expected to return the same value each time it is
   --  called with a particular pair of key values. It should define an
   --  equivalence relationship, that is, be reflexive, symmetric, and
   --  transitive. If the actual for Equivalent_Keys behaves in some other
   --  manner, the behavior of this package is unspecified. Which subprograms
   --  of this package call Equivalent_Keys, and how many times they call it,
   --  is unspecified.

   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   --  The actual function for the generic formal function "=" on Element_Type
   --  values is expected to define a reflexive and symmetric relationship and
   --  return the same result value each time it is called with a particular
   --  pair of values.  If it behaves in some other manner, the function "=" on
   --  map values returns an unspecified value. The exact arguments and number
   --  of calls of this generic formal function by the function "=" on map
   --  values are unspecified.
package Ada.Containers.Hashed_Maps with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate;
   pragma Remote_Types;

   type Map is tagged private
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type,
      Aggregate         => (Empty     => Empty,
                            Add_Named => Insert);

   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   function "=" (Left, Right : Cursor) return Boolean;
   --  The representation of cursors includes a component used to optimize
   --  iteration over maps. This component may become unreliable after
   --  multiple map insertions, and must be excluded from cursor equality,
   --  so we need to provide an explicit definition for it, instead of
   --  using predefined equality (as implied by a questionable comment
   --  in the RM).

   Empty_Map : constant Map;
   --  Map objects declared without an initialization expression are
   --  initialized to the value Empty_Map.

   No_Element : constant Cursor;
   --  Cursor objects declared without an initialization expression are
   --  initialized to the value No_Element.

   function Empty (Capacity : Count_Type := 1000) return Map;

   function Has_Element (Position : Cursor) return Boolean;
   --  Returns True if Position designates an element, and returns False
   --  otherwise.

   package Map_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function "=" (Left, Right : Map) return Boolean;
   --  If Left and Right denote the same map object, then the function returns
   --  True. If Left and Right have different lengths, then the function
   --  returns False. Otherwise, for each key K in Left, the function returns
   --  False if:
   --
   --  * a key equivalent to K is not present in Right; or
   --
   --  * the element associated with K in Left is not equal to the
   --    element associated with K in Right (using the generic formal
   --    equality operator for elements).
   --
   --  If the function has not returned a result after checking all of the
   --  keys, it returns True. Any exception raised during evaluation of key
   --  equivalence or element equality is propagated.

   function Capacity (Container : Map) return Count_Type;
   --  Returns the current capacity of the map. Capacity is the maximum length
   --  before which rehashing in guaranteed not to occur.

   procedure Reserve_Capacity (Container : in out Map; Capacity : Count_Type);
   --  Adjusts the current capacity, by allocating a new buckets array. If the
   --  requested capacity is less than the current capacity, then the capacity
   --  is contracted (to a value not less than the current length). If the
   --  requested capacity is greater than the current capacity, then the
   --  capacity is expanded (to a value not less than what is requested). In
   --  either case, the nodes are rehashed from the old buckets array onto the
   --  new buckets array (Hash is called once for each existing key in order to
   --  compute the new index), and then the old buckets array is deallocated.

   function Length (Container : Map) return Count_Type;
   --  Returns the number of items in the map

   function Is_Empty (Container : Map) return Boolean;
   --  Equivalent to Length (Container) = 0

   procedure Clear (Container : in out Map);
   --  Removes all of the items from the map

   function Key (Position : Cursor) return Key_Type;
   --  Key returns the key component of the node designated by Position.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated.

   function Element (Position : Cursor) return Element_Type;
   --  Element returns the element component of the node designated
   --  by Position.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated.

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  Replace_Element assigns New_Item to the element of the node designated
   --  by Position.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated.

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access
                   procedure (Key : Key_Type; Element : Element_Type));
   --  Query_Element calls Process.all with the key and element from the node
   --  designated by Position as the arguments.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated.
   --
   --  Tampering with the elements of the map that contains the element
   --  designated by Position is prohibited during the execution of the call on
   --  Process.all. Any exception raised by Process.all is propagated.

   procedure Update_Element
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access
                    procedure (Key : Key_Type; Element : in out Element_Type));
   --  Update_Element calls Process.all with the key and element from the node
   --  designated by Position as the arguments.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated.
   --
   --  Tampering with the elements of Container is prohibited during the
   --  execution of the call on Process.all. Any exception raised by
   --  Process.all is propagated.

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Element_Type) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Map;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);
   --  This function (combined with the Constant_Indexing and
   --  Implicit_Dereference aspects) provides a convenient way to gain read
   --  access to an individual element of a map given a cursor.
   --  Constant_Reference returns an object whose discriminant is an access
   --  value that designates the element designated by Position.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated.
   --
   --  Tampering with the elements of Container is prohibited
   --  while the object returned by Constant_Reference exists and has not been
   --  finalized.

   function Reference
     (Container : aliased in out Map;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);
   --  This function (combined with the Variable_Indexing and
   --  Implicit_Dereference aspects) provides a convenient way to gain read and
   --  write access to an individual element of a map given a cursor.
   --  Reference returns an object whose discriminant is an access value that
   --  designates the element designated by Position.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated.
   --
   --  Tampering with the elements of Container is prohibited while the object
   --  returned by Reference exists and has not been finalized.

   function Constant_Reference
     (Container : aliased Map;
      Key       : Key_Type) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);
   --  Equivalent to Constant_Reference (Container, Find (Container, Key)).

   function Reference
     (Container : aliased in out Map;
      Key       : Key_Type) return Reference_Type;
   pragma Inline (Reference);
   --  Equivalent to Reference (Container, Find (Container, Key)).

   procedure Assign (Target : in out Map; Source : Map);
   --  If Target denotes the same object as Source, the operation has no
   --  effect. Otherwise, the key/element pairs of Source are copied to Target
   --  as for an assignment_statement assigning Source to Target.

   function Copy (Source : Map; Capacity : Count_Type := 0) return Map;

   procedure Move (Target : in out Map; Source : in out Map);
   --  If Target denotes the same object as Source, then the operation has no
   --  effect. Otherwise, the operation is equivalent to Assign (Target,
   --  Source) followed by Clear (Source).

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);
   --  Insert checks if a node with a key equivalent to Key is already present
   --  in Container. If a match is found, Inserted is set to False and Position
   --  designates the element with the matching key.  Otherwise, Insert
   --  allocates a new node, initializes it to Key and New_Item, and adds it to
   --  Container; Inserted is set to True and Position designates the
   --  newly-inserted node. Any exception raised during allocation is
   --  propagated and Container is not modified.

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);
   --  Insert inserts Key into Container as per the five-parameter Insert, with
   --  the difference that an element initialized by default (see 3.3.1) is
   --  inserted.

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Insert inserts Key and New_Item into Container as per the five-parameter
   --  Insert, with the difference that if a node with a key equivalent to Key
   --  is already in the map, then Constraint_Error is propagated.

   procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Include inserts Key and New_Item into Container as per the
   --  five-parameter Insert, with the difference that if a node with a key
   --  equivalent to Key is already in the map, then this operation assigns Key
   --  and New_Item to the matching node. Any exception raised during
   --  assignment is propagated.

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Replace checks if a node with a key equivalent to Key is present in
   --  Container. If a match is found, Replace assigns Key and New_Item to the
   --  matching node; otherwise, Constraint_Error is propagated.

   procedure Exclude (Container : in out Map; Key : Key_Type);
   --  Exclude checks if a node with a key equivalent to Key is present in
   --  Container. If a match is found, Exclude removes the node from the map.

   procedure Delete (Container : in out Map; Key : Key_Type);
   --  Delete checks if a node with a key equivalent to Key is present in
   --  Container. If a match is found, Delete removes the node from the map;
   --  otherwise, Constraint_Error is propagated.

   procedure Delete (Container : in out Map; Position : in out Cursor);
   --  Delete removes the node designated by Position from the map. Position is
   --  set to No_Element on return.
   --
   --  If Position equals No_Element, then Constraint_Error is propagated. If
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated.

   function First (Container : Map) return Cursor;
   --  If Length (Container) = 0, then First returns No_Element.  Otherwise,
   --  First returns a cursor that designates the first node in Container.

   function Next (Position : Cursor) return Cursor;
   --  Returns a cursor that designates the successor of the node designated by
   --  Position. If Position designates the last node, then No_Element is
   --  returned. If Position equals No_Element, then No_Element is returned.

   procedure Next (Position : in out Cursor);
   --  Equivalent to Position := Next (Position)

   function Find (Container : Map; Key : Key_Type) return Cursor;
   --  If Length (Container) equals 0, then Find returns No_Element.
   --  Otherwise, Find checks if a node with a key equivalent to Key is present
   --  in Container. If a match is found, a cursor designating the matching
   --  node is returned; otherwise, No_Element is returned.

   function Contains (Container : Map; Key : Key_Type) return Boolean;
   --  Equivalent to Find (Container, Key) /= No_Element.

   function Element (Container : Map; Key : Key_Type) return Element_Type;
   --  Equivalent to Element (Find (Container, Key))

   function Equivalent_Keys (Left, Right : Cursor) return Boolean;
   --  Returns the result of calling Equivalent_Keys with the keys of the nodes
   --  designated by cursors Left and Right.

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean;
   --  Returns the result of calling Equivalent_Keys with key of the node
   --  designated by Left and key Right.

   function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean;
   --  Returns the result of calling Equivalent_Keys with key Left and the node
   --  designated by Right.

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));
   --  Iterate calls Process.all with a cursor that designates each node in
   --  Container, starting with the first node and moving the cursor according
   --  to the successor relation. Tampering with the cursors of Container is
   --  prohibited during the execution of a call on Process.all. Any exception
   --  raised by Process.all is propagated.

   function Iterate
     (Container : Map) return Map_Iterator_Interfaces.Forward_Iterator'Class;

private
   pragma Inline ("=");
   pragma Inline (Length);
   pragma Inline (Is_Empty);
   pragma Inline (Clear);
   pragma Inline (Key);
   pragma Inline (Element);
   pragma Inline (Move);
   pragma Inline (Contains);
   pragma Inline (Capacity);
   pragma Inline (Reserve_Capacity);
   pragma Inline (Has_Element);
   pragma Inline (Equivalent_Keys);
   pragma Inline (Next);

   type Node_Type;
   type Node_Access is access Node_Type;

   type Node_Type is limited record
      Key     : Key_Type;
      Element : aliased Element_Type;
      Next    : Node_Access;
   end record;

   package HT_Types is
     new Hash_Tables.Generic_Hash_Table_Types (Node_Type, Node_Access);

   type Map is new Ada.Finalization.Controlled with record
      HT : HT_Types.Hash_Table_Type;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Map);

   overriding procedure Adjust (Container : in out Map);

   overriding procedure Finalize (Container : in out Map);

   use HT_Types, HT_Types.Implementation;
   use Ada.Finalization;
   use Ada.Streams;

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Map);

   for Map'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Map);

   for Map'Read use Read;

   type Map_Access is access all Map;
   for Map_Access'Storage_Size use 0;

   type Cursor is record
      Container : Map_Access;
      --  Access to this cursor's container

      Node      : Node_Access;
      --  Access to the node pointed to by this cursor

      Position  : Hash_Type := Hash_Type'Last;
      --  Position of the node in the buckets of the container. If this is
      --  equal to Hash_Type'Last, then it will not be used. Position is
      --  not requried by the implementation, but improves the efficiency
      --  of various operations.
      --
      --  However, this value must be maintained so that the predefined
      --  equality operation acts as required by RM A.18.4-18/2, which
      --  states: "The predefined "=" operator for type Cursor returns True
      --  if both cursors are No_Element, or designate the same element
      --  in the same container."
   end record;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor);

   for Cursor'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor);

   for Cursor'Write use Write;

   subtype Reference_Control_Type is Implementation.Reference_Control_Type;
   --  It is necessary to rename this here, so that the compiler can find it

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
      record
         Control : Reference_Control_Type :=
           raise Program_Error with "uninitialized reference";
         --  The RM says, "The default initialization of an object of
         --  type Constant_Reference_Type or Reference_Type propagates
         --  Program_Error."
      end record;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type);

   for Constant_Reference_Type'Write use Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type);

   for Constant_Reference_Type'Read use Read;

   type Reference_Type
     (Element : not null access Element_Type) is
      record
         Control : Reference_Control_Type :=
           raise Program_Error with "uninitialized reference";
         --  The RM says, "The default initialization of an object of
         --  type Constant_Reference_Type or Reference_Type propagates
         --  Program_Error."
      end record;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type);

   for Reference_Type'Write use Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type);

   for Reference_Type'Read use Read;

   --  See Ada.Containers.Vectors for documentation on the following

   procedure _Next (Position : in out Cursor) renames Next;

   function Pseudo_Reference
     (Container : aliased Map'Class) return Reference_Control_Type;
   pragma Inline (Pseudo_Reference);
   --  Creates an object of type Reference_Control_Type pointing to the
   --  container, and increments the Lock. Finalization of this object will
   --  decrement the Lock.

   type Element_Access is access all Element_Type with
     Storage_Size => 0;

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access;
   --  Returns a pointer to the element designated by Position.

   Empty_Map : constant Map := (Controlled with others => <>);

   No_Element : constant Cursor := (Container => null, Node => null,
                                    Position  => Hash_Type'Last);

   type Iterator is new Limited_Controlled and
     Map_Iterator_Interfaces.Forward_Iterator with
   record
      Container : Map_Access;
   end record
     with Disable_Controlled => not T_Check;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end Ada.Containers.Hashed_Maps;
