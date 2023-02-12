------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                A D A . C O N T A I N E R S . V E C T O R S               --
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

with Ada.Containers.Helpers;
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Text_Buffers;

--  The language-defined generic package Containers.Vectors provides private
--  types Vector and Cursor, and a set of operations for each type. A vector
--  container allows insertion and deletion at any position, but it is
--  specifically optimized for insertion and deletion at the high end (the end
--  with the higher index) of the container. A vector container also provides
--  random access to its elements.
--
--  A vector container behaves conceptually as an array that expands as
--  necessary as items are inserted. The length of a vector is the number of
--  elements that the vector contains. The capacity of a vector is the maximum
--  number of elements that can be inserted into the vector prior to it being
--  automatically expanded.
--
--  Elements in a vector container can be referred to by an index value of a
--  generic formal type. The first element of a vector always has its index
--  value equal to the lower bound of the formal type.
--
--  A vector container may contain empty elements. Empty elements do not have a
--  specified value.

generic
   type Index_Type is range <>;
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   --  The actual function for the generic formal function "=" on Element_Type
   --  values is expected to define a reflexive and symmetric relationship and
   --  return the same result value each time it is called with a particular
   --  pair of values. If it behaves in some other manner, the functions
   --  defined to use it return an unspecified value. The exact arguments and
   --  number of calls of this generic formal function by the functions defined
   --  to use it are unspecified.

package Ada.Containers.Vectors with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate;
   pragma Remote_Types;

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;
   --  The subtype Extended_Index includes the indices covered by Index_Type
   --  plus the value No_Index and, if it exists, the successor to the
   --  Index_Type'Last.

   No_Index : constant Extended_Index := Extended_Index'First;
   --  No_Index represents a position that does not correspond to any element.

   type Vector is tagged private
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type,
      Aggregate         => (Empty          => Empty,
                            Add_Unnamed    => Append,
                            New_Indexed    => New_Vector,
                            Assign_Indexed => Replace_Element);

   pragma Preelaborable_Initialization (Vector);
   --  Vector type, to be instantiated by users of this package. If an object
   --  of type Vector is not otherwise initialized, it is initialized to
   --  Empty_Vector.

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   --  Cursor pointing into an instance of vector. If an object of type Cursor
   --  is not otherwise initialized, it is initialized to No_Element

   No_Element : constant Cursor;
   --  No_Element represents a cursor that designates no element.

   function Has_Element (Position : Cursor) return Boolean;
   --  Returns True if Position designates an element, and returns False
   --  otherwise.

   package Vector_Iterator_Interfaces is new
      Ada.Iterator_Interfaces (Cursor, Has_Element);

   Empty_Vector : constant Vector;
   --  Empty_Vector represents the empty vector object. It has a length of 0.

   function Empty (Capacity : Count_Type := 10) return Vector;

   overriding function "=" (Left, Right : Vector) return Boolean;
   --  If Left and Right denote the same vector object, then the function
   --  returns True. If Left and Right have different lengths, then the
   --  function returns False. Otherwise, it compares each element in Left to
   --  the corresponding element in Right using the generic formal equality
   --  operator. If any such comparison returns False, the function returns
   --  False; otherwise it returns True. Any exception raised during evaluation
   --  of element equality is propagated.

   function To_Vector (Length : Count_Type) return Vector;
   --  Returns a vector with a length of Length, filled with empty elements.

   function To_Vector
     (New_Item : Element_Type;
      Length   : Count_Type) return Vector;
   --  Returns a vector with a length of Length, filled with elements
   --  initialized to the value New_Item.

   function "&" (Left, Right : Vector) return Vector;
   --  Returns a vector comprising the elements of Left followed by the
   --  elements of Right.

   function "&" (Left : Vector; Right : Element_Type) return Vector;
   --  Returns a vector comprising the elements of Left followed by the element
   --  Right.

   function "&" (Left : Element_Type; Right : Vector) return Vector;
   --  Returns a vector comprising the element Left followed by the elements of
   --  Right.

   function "&" (Left, Right : Element_Type) return Vector;
   --  Returns a vector comprising the element Left followed by the element
   --  Right.

   function Capacity (Container : Vector) return Count_Type;
   --  Returns the capacity of Container.

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Count_Type);
   --  Reserve_Capacity allocates new internal data structures such that the
   --  length of the resulting vector can become at least the value Capacity
   --  without requiring an additional call to Reserve_Capacity, and is large
   --  enough to hold the current length of Container. Reserve_Capacity then
   --  copies the elements into the new data structures and deallocates the old
   --  data structures. Any exception raised during allocation is propagated
   --  and Container is not modified.

   function Length (Container : Vector) return Count_Type;
   --  Returns the number of elements in Container.

   procedure Set_Length
     (Container : in out Vector;
      Length    : Count_Type);
   --  If Length is larger than the capacity of Container, Set_Length calls
   --  Reserve_Capacity (Container, Length), then sets the length of the
   --  Container to Length. If Length is greater than the original length of
   --  Container, empty elements are added to Container; otherwise elements are
   --  removed from Container.

   function Is_Empty (Container : Vector) return Boolean;
   --  Equivalent to Length (Container) = 0.

   procedure Clear (Container : in out Vector);
   --  Removes all the elements from Container. The capacity of Container does
   --  not change.

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index) return Cursor;
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container), then No_Element is returned. Otherwise, a cursor
   --  designating the element at position Index in Container is returned.

   function To_Index (Position : Cursor) return Extended_Index;
   --  If Position is No_Element, No_Index is returned. Otherwise, the index
   --  (within its containing vector) of the element designated by Position is
   --  returned.

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type;
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container), then Constraint_Error is propagated. Otherwise, Element
   --  returns the element at position Index.

   function Element (Position : Cursor) return Element_Type;
   --  If Position equals No_Element, then Constraint_Error is propagated.
   --  Otherwise, Element returns the element designated by Position.

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type);
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container), then Constraint_Error is propagated. Otherwise
   --  Replace_Element assigns the value New_Item to the element at position
   --  Index. Any exception raised during the assignment is propagated. The
   --  element at position Index is not an empty element after successful call
   --  to Replace_Element.

   procedure Replace_Element
     (Container : in out Vector;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated. Otherwise Replace_Element assigns New_Item to the element
   --  designated by Position. Any exception raised during the assignment is
   --  propagated. The element at Position is not an empty element after
   --  successful call to Replace_Element.

   procedure Query_Element
     (Container : Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : Element_Type));
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container), then Constraint_Error is propagated. Otherwise,
   --  Query_Element calls Process.all with the element at position Index as
   --  the argument. Program_Error is propagated if Process.all tampers with
   --  the elements of Container. Any exception raised by Process.all is
   --  propagated.

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));
   --  If Position equals No_Element, then Constraint_Error is propagated.
   --  Otherwise, Query_Element calls Process.all with the element designated
   --  by Position as the argument. Program_Error is propagated if Process.all
   --  tampers with the elements of Container. Any exception raised by
   --  Process.all is propagated.

   procedure Update_Element
     (Container : in out Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type));
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container), then Constraint_Error is propagated. Otherwise,
   --  Update_Element calls Process.all with the element at position Index as
   --  the argument. Program_Error is propagated if Process.all tampers with
   --  the elements of Container. Any exception raised by Process.all is
   --  propagated.
   --
   --  If Element_Type is unconstrained and definite, then the actual Element
   --  parameter of Process.all shall be unconstrained.
   --
   --  The element at position Index is not an empty element after successful
   --  completion of this operation.

   procedure Update_Element
     (Container : in out Vector;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type));
   --  If Position equals No_Element, then Constraint_Error is propagated; if
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated. Otherwise Update_Element calls Process.all with the
   --  element designated by Position as the argument. Program_Error is
   --  propagated if Process.all tampers with the elements of Container. Any
   --  exception raised by Process.all is propagated.
   --
   --  If Element_Type is unconstrained and definite, then the actual Element
   --  parameter of Process.all shall be unconstrained.
   --
   --  The element designated by Position is not an empty element after
   --  successful completion of this operation.

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is
   private
   with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Element_Type) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type) return Reference_Type;
   pragma Inline (Reference);

   procedure Assign (Target : in out Vector; Source : Vector);

   function Copy (Source : Vector; Capacity : Count_Type := 0) return Vector;

   procedure Move (Target : in out Vector; Source : in out Vector);
   --  If Target denotes the same object as Source, then Move has no effect.
   --  Otherwise, Move first calls Clear (Target); then, each element from
   --  Source is removed from Source and inserted into Target in the original
   --  order. The length of Source is 0 after a successful call to Move.

   function New_Vector (First, Last : Index_Type) return Vector
     with Pre => First = Index_Type'First;
   --  Ada 2022 aggregate operation.

   procedure Insert_Vector
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector);
   --  If Before is not in the range First_Index (Container) .. Last_Index
   --  (Container) + 1, then Constraint_Error is propagated. If
   --  Length(New_Item) is 0, then Insert_Vector does nothing. Otherwise, it
   --  computes the new length NL as the sum of the current length and Length
   --  (New_Item); if the value of Last appropriate for length NL would be
   --  greater than Index_Type'Last then Constraint_Error is propagated.
   --
   --  If the current vector capacity is less than NL, Reserve_Capacity
   --  (Container, NL) is called to increase the vector capacity. Then
   --  Insert_Vector slides the elements in the range Before .. Last_Index
   --  (Container) up by Length(New_Item) positions, and then copies the
   --  elements of New_Item to the positions starting at Before. Any exception
   --  raised during the copying is propagated.

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector) renames Insert_Vector;
   --  Retained for now for compatibility; AI12-0400 will remove this.

   procedure Insert_Vector
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector);
   --  If Before is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. Otherwise, if
   --  Length(New_Item) is 0, then Insert_Vector does nothing. If Before is
   --  No_Element, then the call is equivalent to Insert_Vector (Container,
   --  Last_Index (Container) + 1, New_Item); otherwise the call is equivalent
   --  to Insert_Vector (Container, To_Index (Before), New_Item);

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector) renames Insert_Vector;
   --  Retained for now for compatibility; AI12-0400 will remove this.

   procedure Insert_Vector
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector;
      Position  : out Cursor);
   --  If Before is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. If Before equals
   --  No_Element, then let T be Last_Index (Container) + 1; otherwise, let T
   --  be To_Index (Before). Insert_Vector (Container, T, New_Item) is called,
   --  and then Position is set to To_Cursor (Container, T).

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector;
      Position  : out Cursor) renames Insert_Vector;
   --  Retained for now for compatibility; AI12-0400 will remove this.

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Equivalent to:
   --  Insert_Vector (Container, Before, To_Vector (New_Item, Count));

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Equivalent to:
   --  Insert_Vector (Container, Before, To_Vector (New_Item, Count));

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1);
   --  Equivalent to
   --  Insert_Vector (Container, Before, To_Vector (New_Item, Count), Position)

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1);
   --  If Before is not in the range First_Index (Container) .. Last_Index
   --  (Container) + 1, then Constraint_Error is propagated. If Count is 0,
   --  then Insert does nothing. Otherwise, it computes the new length NL as
   --  the sum of the current length and Count; if the value of Last
   --  appropriate for length NL would be greater than Index_Type'Last then
   --  Constraint_Error is propagated.
   --
   --  If the current vector capacity is less than NL, Reserve_Capacity
   --  (Container, NL) is called to increase the vector capacity. Then Insert
   --  slides the elements in the range Before .. Last_Index (Container) up by
   --  Count positions, and then inserts elements that are initialized by
   --  default (see 3.3.1) in the positions starting at Before.

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1);
   --  If Before is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. If Before equals
   --  No_Element, then let T be Last_Index (Container) + 1; otherwise, let T
   --  be To_Index (Before). Insert (Container, T, Count) is called, and then
   --  Position is set to To_Cursor (Container, T).

   procedure Prepend_Vector
     (Container : in out Vector;
      New_Item  : Vector);
   --  Equivalent to Insert (Container, First_Index (Container), New_Item).

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Vector) renames Prepend_Vector;
   --  Retained for now for compatibility; AI12-0400 will remove this.

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Equivalent to Insert (Container, First_Index (Container), New_Item,
   --  Count).

   procedure Append_Vector
     (Container : in out Vector;
      New_Item  : Vector);
   --  Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item).

   procedure Append
     (Container : in out Vector;
      New_Item  : Vector) renames Append_Vector;
   --  Retained for now for compatibility; AI12-0400 will remove this.

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type);
   --  Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item,
   --  Count).

   procedure Append (Container : in out Vector;
                     New_Item  :        Element_Type);

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1);
   --  If Before is not in the range First_Index (Container) .. Last_Index
   --  (Container) + 1, then Constraint_Error is propagated. If Count is 0,
   --  then Insert_Space does nothing. Otherwise, it computes the new length NL
   --  as the sum of the current length and Count; if the value of Last
   --  appropriate for length NL would be greater than Index_Type'Last then
   --  Constraint_Error is propagated.
   --
   --  If the current vector capacity is less than NL, Reserve_Capacity
   --  (Container, NL) is called to increase the vector capacity. Then
   --  Insert_Space slides the elements in the range Before .. Last_Index
   --  (Container) up by Count positions, and then inserts empty elements in
   --  the positions starting at Before.

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1);
   --  If Before is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. If Before equals
   --  No_Element, then let T be Last_Index (Container) + 1; otherwise, let T
   --  be To_Index (Before). Insert_Space (Container, T, Count) is called, and
   --  then Position is set to To_Cursor (Container, T).

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type := 1);
   --  If Index is not in the range First_Index (Container) .. Last_Index
   --  (Container) + 1, then Constraint_Error is propagated. If Count is 0,
   --  Delete has no effect. Otherwise Delete slides the elements (if any)
   --  starting at position Index + Count down to Index. Any exception raised
   --  during element assignment is propagated.

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1);
   --  If Position equals No_Element, then Constraint_Error is propagated. If
   --  Position does not designate an element in Container, then Program_Error
   --  is propagated. Otherwise, Delete (Container, To_Index (Position), Count)
   --  is called, and then Position is set to No_Element.

   procedure Delete_First
     (Container : in out Vector;
      Count     : Count_Type := 1);
   --  Equivalent to Delete (Container, First_Index (Container), Count).

   procedure Delete_Last
     (Container : in out Vector;
      Count     : Count_Type := 1);
   --  If Length (Container) <= Count then Delete_Last is equivalent to Clear
   --  (Container). Otherwise it is equivalent to Delete (Container,
   --  Index_Type'Val(Index_Type'Pos(Last_Index (Container)) - Count + 1),
   --  Count).

   procedure Reverse_Elements (Container : in out Vector);
   --  Reorders the elements of Container in reverse order.

   procedure Swap (Container : in out Vector; I, J : Index_Type);
   --  If either I or J is not in the range First_Index (Container) ..
   --  Last_Index (Container), then Constraint_Error is propagated. Otherwise,
   --  Swap exchanges the values of the elements at positions I and J.

   procedure Swap (Container : in out Vector; I, J : Cursor);
   --  If either I or J is No_Element, then Constraint_Error is propagated. If
   --  either I or J do not designate an element in Container, then
   --  Program_Error is propagated. Otherwise, Swap exchanges the values of the
   --  elements designated by I and J.

   function First_Index (Container : Vector) return Index_Type;
   --  Returns the value Index_Type'First.

   function First (Container : Vector) return Cursor;
   --  If Container is empty, First returns No_Element. Otherwise, it returns a
   --  cursor that designates the first element in Container.

   function First_Element (Container : Vector) return Element_Type;
   --  Equivalent to Element (Container, First_Index (Container)).

   function Last_Index (Container : Vector) return Extended_Index;
   --  If Container is empty, Last_Index returns No_Index. Otherwise, it
   --  returns the position of the last element in Container.

   function Last (Container : Vector) return Cursor;
   --  If Container is empty, Last returns No_Element. Otherwise, it returns a
   --  cursor that designates the last element in Container.

   function Last_Element (Container : Vector) return Element_Type;
   --  Equivalent to Element (Container, Last_Index (Container)).

   function Next (Position : Cursor) return Cursor;
   --  If Position equals No_Element or designates the last element of the
   --  container, then Next returns the value No_Element. Otherwise, it returns
   --  a cursor that designates the element with index To_Index (Position) + 1
   --  in the same vector as Position.

   procedure Next (Position : in out Cursor);
   --  Equivalent to Position := Next (Position).

   function Previous (Position : Cursor) return Cursor;
   --  If Position equals No_Element or designates the first element of the
   --  container, then Previous returns the value No_Element. Otherwise, it
   --  returns a cursor that designates the element with index To_Index
   --  (Position) - 1 in the same vector as Position.

   procedure Previous (Position : in out Cursor);
   --  Equivalent to Position := Previous (Position).

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index;
   --  Searches the elements of Container for an element equal to Item (using
   --  the generic formal equality operator). The search starts at position
   --  Index and proceeds towards Last_Index (Container). If no equal
   --  element is found, then Find_Index returns No_Index. Otherwise, it
   --  returns the index of the first equal element encountered.

   function Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;
   --  If Position is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. Otherwise Find searches
   --  the elements of Container for an element equal to Item (using the
   --  generic formal equality operator). The search starts at the first
   --  element if Position equals No_Element, and at the element designated
   --  by Position otherwise. It proceeds towards the last element of
   --  Container. If no equal element is found, then Find returns
   --  No_Element. Otherwise, it returns a cursor designating the first
   --  equal element encountered.

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index;
   --  Searches the elements of Container for an element equal to Item (using
   --  the generic formal equality operator). The search starts at position
   --  Index or, if Index is greater than Last_Index (Container), at
   --  position Last_Index (Container). It proceeds towards First_Index
   --  (Container). If no equal element is found, then Reverse_Find_Index
   --  returns No_Index. Otherwise, it returns the index of the first equal
   --  element encountered.

   function Reverse_Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;
   --  If Position is not No_Element, and does not designate an element in
   --  Container, then Program_Error is propagated. Otherwise Reverse_Find
   --  searches the elements of Container for an element equal to Item
   --  (using the generic formal equality operator). The search starts at
   --  the last element if Position equals No_Element, and at the element
   --  designated by Position otherwise. It proceeds towards the first
   --  element of Container. If no equal element is found, then Reverse_Find
   --  returns No_Element. Otherwise, it returns a cursor designating the
   --  first equal element encountered.

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean;
   --  Equivalent to Has_Element (Find (Container, Item)).

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor));
   --  Invokes Process.all with a cursor that designates each element in
   --  Container, in index order. Program_Error is propagated if Process.all
   --  tampers with the cursors of Container. Any exception raised by Process
   --  is propagated.

   procedure Reverse_Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor));
   --  Iterates over the elements in Container as per Iterate, except that
   --  elements are traversed in reverse index order.
   --

   function Iterate (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Container : Vector; Start : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
      --  The actual function for the generic formal function "<" of
      --  Generic_Sorting is expected to return the same value each time it is
      --  called with a particular pair of element values. It should define a
      --  strict ordering relationship, that is, be irreflexive, asymmetric,
      --  and transitive; it should not modify Container. If the actual for "<"
      --  behaves in some other manner, the behavior of the subprograms of
      --  Generic_Sorting are unspecified. How many times the subprograms of
      --  Generic_Sorting call "<" is unspecified.
   package Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean;
      --  Returns True if the elements are sorted smallest first as determined
      --  by the generic formal "<" operator; otherwise, Is_Sorted returns
      --  False. Any exception raised during evaluation of "<" is propagated.

      procedure Sort (Container : in out Vector);
      --  Reorders the elements of Container such that the elements are sorted
      --  smallest first as determined by the generic formal "<" operator
      --  provided. Any exception raised during evaluation of "<" is
      --  propagated.

      procedure Merge (Target : in out Vector; Source : in out Vector);
      --  Merge removes elements from Source and inserts them into Target;
      --  afterwards, Target contains the union of the elements that were
      --  initially in Source and Target; Source is left empty. If Target and
      --  Source are initially sorted smallest first, then Target is ordered
      --  smallest first as determined by the generic formal "<" operator;
      --  otherwise, the order of elements in Target is unspecified. Any
      --  exception raised during evaluation of "<" is propagated.

   end Generic_Sorting;

private

   pragma Inline (Append);
   pragma Inline (First_Index);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (First_Element);
   pragma Inline (Last_Element);
   pragma Inline (Query_Element);
   pragma Inline (Update_Element);
   pragma Inline (Replace_Element);
   pragma Inline (Is_Empty);
   pragma Inline (Contains);
   pragma Inline (Next);
   pragma Inline (Previous);

   use Ada.Containers.Helpers;
   package Implementation is new Generic_Implementation;
   use Implementation;

   type Elements_Array is array (Index_Type range <>) of aliased Element_Type;
   function "=" (L, R : Elements_Array) return Boolean is abstract;

   type Elements_Type (Last : Extended_Index) is limited record
      EA : Elements_Array (Index_Type'First .. Last);
   end record;

   type Elements_Access is access all Elements_Type;

   use Finalization;
   use Streams;

   type Vector is new Controlled with record
      Elements : Elements_Access := null;
      Last     : Extended_Index := No_Index;
      TC       : aliased Tamper_Counts;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Vector);

   overriding procedure Adjust (Container : in out Vector);
   overriding procedure Finalize (Container : in out Vector);

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Vector);

   for Vector'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Vector);

   for Vector'Read use Read;

   type Vector_Access is access all Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor is record
      Container : Vector_Access;
      Index     : Index_Type := Index_Type'First;
   end record;

   procedure Read
     (Stream   : not null access Root_Stream_Type'Class;
      Position : out Cursor);

   for Cursor'Read use Read;

   procedure Write
     (Stream   : not null access Root_Stream_Type'Class;
      Position : Cursor);

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

   --  Three operations are used to optimize the expansion of "for ... of"
   --  loops: the Next(Cursor) (or Previous) procedure in the visible part,
   --  and the following Pseudo_Reference and Get_Element_Access functions.
   --  See Exp_Ch5 for details, including the leading underscores here.

   procedure _Next (Position : in out Cursor) renames Next;
   procedure _Previous (Position : in out Cursor) renames Previous;

   function Pseudo_Reference
     (Container : aliased Vector'Class) return Reference_Control_Type;
   pragma Inline (Pseudo_Reference);
   --  Creates an object of type Reference_Control_Type pointing to the
   --  container, and increments the Lock. Finalization of this object will
   --  decrement the Lock.

   type Element_Access is access all Element_Type;

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access;
   --  Returns a pointer to the element designated by Position.

   No_Element : constant Cursor := Cursor'(null, Index_Type'First);

   Empty_Vector : constant Vector := (Controlled with others => <>);

   type Iterator is new Limited_Controlled and
     Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
      Index     : Index_Type'Base;
   end record
     with Disable_Controlled => not T_Check;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end Ada.Containers.Vectors;
