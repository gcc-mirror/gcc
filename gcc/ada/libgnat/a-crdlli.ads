------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.RESTRICTED_DOUBLY_LINKED_LISTS               --
--                                                                          --
--                                 S p e c                                  --
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

--  The doubly-linked list container provides constant-time insertion and
--  deletion at all positions, and allows iteration in both the forward and
--  reverse directions. This list form allocates storage for all nodes
--  statically (there is no dynamic allocation), and a discriminant is used to
--  specify the capacity. This container is also "restricted", meaning that
--  even though it does raise exceptions (as described below), it does not use
--  internal exception handlers. No state changes are made that would need to
--  be reverted (in the event of an exception), and so as a consequence, this
--  container cannot detect tampering (of cursors or elements).

generic
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Ada.Containers.Restricted_Doubly_Linked_Lists is
   pragma Pure;

   type List (Capacity : Count_Type) is tagged limited private;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_List : constant List;
   --  The default value for list objects declared without an explicit
   --  initialization expression.

   No_Element : constant Cursor;
   --  The default value for cursor objects declared without an explicit
   --  initialization expression.

   function "=" (Left, Right : List) return Boolean;
   --  If Left denotes the same list object as Right, then equality returns
   --  True. If the length of Left is different from the length of Right, then
   --  it returns False. Otherwise, list equality iterates over Left and Right,
   --  comparing the element of Left to the corresponding element of Right
   --  using the generic actual equality operator for elements. If the elements
   --  compare False, then the iteration terminates and list equality returns
   --  False. Otherwise, if all elements return True, then list equality
   --  returns True.

   procedure Assign (Target : in out List; Source : List);
   --  If Target denotes the same list object as Source, the operation does
   --  nothing. If Target.Capacity is less than Source.Length, then it raises
   --  Constraint_Error. Otherwise, it clears Target, and then inserts each
   --  element of Source into Target.

   function Length (Container : List) return Count_Type;
   --  Returns the total number of (active) elements in Container

   function Is_Empty (Container : List) return Boolean;
   --  Returns True if Container.Length is 0

   procedure Clear (Container : in out List);
   --  Deletes all elements from Container. Note that this is a bounded
   --  container and so the element is not "deallocated" in the same sense that
   --  an unbounded form would deallocate the element. Rather, the node is
   --  relinked off of the active part of the list and onto the inactive part
   --  of the list (the storage from which new elements are "allocated").

   function Element (Position : Cursor) return Element_Type;
   --  If Position equals No_Element, then Constraint_Error is raised.
   --  Otherwise, function Element returns the element designed by Position.

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  If Position equals No_Element, then Constraint_Error is raised. If
   --  Position is associated with a list object different from Container,
   --  Program_Error is raised. Otherwise, the element designated by Position
   --  is assigned the value New_Item.

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));
   --  If Position equals No_Element, then Constraint_Error is raised.
   --  Otherwise, it calls Process with (a constant view of) the element
   --  designated by Position as the parameter.

   procedure Update_Element
     (Container : in out List;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type));
   --  If Position equals No_Element, then Constraint_Error is raised.
   --  Otherwise, it calls Process with (a variable view of) the element
   --  designated by Position as the parameter.

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Inserts Count new elements, all with the value New_Item, into Container,
   --  immediately prior to the position specified by Before. If Before has the
   --  value No_Element, this is interpreted to mean that the elements are
   --  appended to the list. If Before is associated with a list object
   --  different from Container, then Program_Error is raised. If there are
   --  fewer than Count nodes available, then Constraint_Error is raised.

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1);
   --  Inserts elements into Container as described above, but with the
   --  difference that cursor Position is returned, which designates the first
   --  of the new elements inserted. If Count is 0, Position returns the value
   --  Before.

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1);
   --  Inserts elements in Container as described above, but with the
   --  difference that the new elements are initialized to the default value
   --  for objects of type Element_Type.

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Inserts Count elements, all having the value New_Item, prior to the
   --  first element of Container.

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);
   --  Inserts Count elements, all having the value New_Item, following the
   --  last element of Container.

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type := 1);
   --  If Position equals No_Element, Constraint_Error is raised. If Position
   --  is associated with a list object different from Container, then
   --  Program_Error is raised. Otherwise, the Count nodes starting from
   --  Position are removed from Container ("removed" meaning that the nodes
   --  are unlinked from the active nodes of the list and relinked to inactive
   --  storage). On return, Position is set to No_Element.

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1);
   --  Removes the first Count nodes from Container

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1);
   --  Removes the last Count nodes from Container

   procedure Reverse_Elements (Container : in out List);
   --  Relinks the nodes in reverse order

   procedure Swap
     (Container : in out List;
      I, J      : Cursor);
   --  If I or J equals No_Element, then Constraint_Error is raised. If I or J
   --  is associated with a list object different from Container, then
   --  Program_Error is raised. Otherwise, Swap exchanges (copies) the values
   --  of the elements (on the nodes) designated by I and J.

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor);
   --  If I or J equals No_Element, then Constraint_Error is raised. If I or J
   --  is associated with a list object different from Container, then
   --  Program_Error is raised. Otherwise, Swap exchanges (relinks) the nodes
   --  designated by I and J.

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : in out Cursor);
   --  If Before is associated with a list object different from Container,
   --  then Program_Error is raised. If Position equals No_Element, then
   --  Constraint_Error is raised; if it associated with a list object
   --  different from Container, then Program_Error is raised. Otherwise, the
   --  node designated by Position is relinked immediately prior to Before. If
   --  Before equals No_Element, this is interpreted to mean to move the node
   --  designed by Position to the last end of the list.

   function First (Container : List) return Cursor;
   --  If Container is empty, the function returns No_Element. Otherwise, it
   --  returns a cursor designating the first element.

   function First_Element (Container : List) return Element_Type;
   --  Equivalent to Element (First (Container))

   function Last (Container : List) return Cursor;
   --  If Container is empty, the function returns No_Element. Otherwise, it
   --  returns a cursor designating the last element.

   function Last_Element (Container : List) return Element_Type;
   --  Equivalent to Element (Last (Container))

   function Next (Position : Cursor) return Cursor;
   --  If Position equals No_Element or Last (Container), the function returns
   --  No_Element. Otherwise, it returns a cursor designating the node that
   --  immediately follows the node designated by Position.

   procedure Next (Position : in out Cursor);
   --  Equivalent to Position := Next (Position)

   function Previous (Position : Cursor) return Cursor;
   --  If Position equals No_Element or First (Container), the function returns
   --  No_Element. Otherwise, it returns a cursor designating the node that
   --  immediately precedes the node designated by Position.

   procedure Previous (Position : in out Cursor);
   --  Equivalent to Position := Previous (Position)

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;
   --  Searches for the node whose element is equal to Item, starting from
   --  Position and continuing to the last end of the list. If Position equals
   --  No_Element, the search starts from the first node. If Position is
   --  associated with a list object different from Container, then
   --  Program_Error is raised. If no node is found having an element equal to
   --  Item, then Find returns No_Element.

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;
   --  Searches in reverse for the node whose element is equal to Item,
   --  starting from Position and continuing to the first end of the list. If
   --  Position equals No_Element, the search starts from the last node. If
   --  Position is associated with a list object different from Container, then
   --  Program_Error is raised. If no node is found having an element equal to
   --  Item, then Reverse_Find returns No_Element.

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean;
   --  Equivalent to Container.Find (Item) /= No_Element

   function Has_Element (Position : Cursor) return Boolean;
   --  Equivalent to Position /= No_Element

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));
   --  Calls Process with a cursor designating each element of Container, in
   --  order from Container.First to Container.Last.

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));
   --  Calls Process with a cursor designating each element of Container, in
   --  order from Container.Last to Container.First.

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : List) return Boolean;
      --  Returns False if there exists an element which is less than its
      --  predecessor.

      procedure Sort (Container : in out List);
      --  Sorts the elements of Container (by relinking nodes), according to
      --  the order specified by the generic formal less-than operator, such
      --  that smaller elements are first in the list. The sort is stable,
      --  meaning that the relative order of elements is preserved.

   end Generic_Sorting;

private

   type Node_Type is limited record
      Prev    : Count_Type'Base;
      Next    : Count_Type;
      Element : Element_Type;
   end record;

   type Node_Array is array (Count_Type range <>) of Node_Type;

   type List (Capacity : Count_Type) is tagged limited record
      Nodes  : Node_Array (1 .. Capacity) := (others => <>);
      Free   : Count_Type'Base := -1;
      First  : Count_Type := 0;
      Last   : Count_Type := 0;
      Length : Count_Type := 0;
   end record;

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   type Cursor is
      record
         Container : List_Access;
         Node      : Count_Type := 0;
      end record;

   Empty_List : constant List := (0, others => <>);

   No_Element : constant Cursor := (null, 0);

end Ada.Containers.Restricted_Doubly_Linked_Lists;
