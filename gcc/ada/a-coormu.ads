------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--     A D A . C O N T A I N E R S . O R D E R E D _ M U L T I S E T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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

--  The ordered multiset container is similar to the ordered set, but with the
--  difference that multiple equivalent elements are allowed. It also provides
--  additional operations, to iterate over items that are equivalent.

private with Ada.Containers.Red_Black_Trees;
private with Ada.Finalization;
private with Ada.Streams;

generic
   type Element_Type is private;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Ordered_Multisets is
   pragma Preelaborate;
   pragma Remote_Types;

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean;
   --  Returns False if Left is less than Right, or Right is less than Left;
   --  otherwise, it returns True.

   type Set is tagged private;
   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Set : constant Set;
   --  The default value for set objects declared without an explicit
   --  initialization expression.

   No_Element : constant Cursor;
   --  The default value for cursor objects declared without an explicit
   --  initialization expression.

   function "=" (Left, Right : Set) return Boolean;
   --  If Left denotes the same set object as Right, then equality returns
   --  True. If the length of Left is different from the length of Right, then
   --  it returns False. Otherwise, set equality iterates over Left and Right,
   --  comparing the element of Left to the element of Right using the equality
   --  operator for elements. If the elements compare False, then the iteration
   --  terminates and set equality returns False. Otherwise, if all elements
   --  compare True, then set equality returns True.

   function Equivalent_Sets (Left, Right : Set) return Boolean;
   --  Similar to set equality, but with the difference that elements are
   --  compared for equivalence instead of equality.

   function To_Set (New_Item : Element_Type) return Set;
   --  Constructs a set object with New_Item as its single element

   function Length (Container : Set) return Count_Type;
   --  Returns the total number of elements in Container

   function Is_Empty (Container : Set) return Boolean;
   --  Returns True if Container.Length is 0

   procedure Clear (Container : in out Set);
   --  Deletes all elements from Container

   function Element (Position : Cursor) return Element_Type;
   --  If Position equals No_Element, then Constraint_Error is raised.
   --  Otherwise, function Element returns the element designed by Position.

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  If Position equals No_Element, then Constraint_Error is raised. If
   --  Position is associated with a set different from Container, then
   --  Program_Error is raised. If New_Item is equivalent to the element
   --  designated by Position, then if Container is locked (element tampering
   --  has been attempted), Program_Error is raised; otherwise, the element
   --  designated by Position is assigned the value of New_Item. If New_Item is
   --  not equivalent to the element designated by Position, then if the
   --  container is busy (cursor tampering has been attempted), Program_Error
   --  is raised; otherwise, the element designed by Position is assigned the
   --  value of New_Item, and the node is moved to its new position (in
   --  canonical insertion order).

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));
   --  If Position equals No_Element, then Constraint_Error is
   --  raised. Otherwise, it calls Process with the element designated by
   --  Position as the parameter. This call locks the container, so attempts to
   --  change the value of the element while Process is executing (to "tamper
   --  with elements") will raise Program_Error.

   procedure Move (Target : in out Set; Source : in out Set);
   --  If Target denotes the same object as Source, the operation does
   --  nothing. If either Target or Source is busy (cursor tampering is
   --  attempted), then it raises Program_Error. Otherwise, Target is cleared,
   --  and the nodes from Source are moved (not copied) to Target (so Source
   --  becomes empty).

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor);
   --  Insert adds New_Item to Container, and returns cursor Position
   --  designating the newly inserted node. The node is inserted after any
   --  existing elements less than or equivalent to New_Item (and before any
   --  elements greater than New_Item). Note that the issue of where the new
   --  node is inserted relative to equivalent elements does not arise for
   --  unique-key containers, since in that case the insertion would simply
   --  fail. For a multiple-key container (the case here), insertion always
   --  succeeds, and is defined such that the new item is positioned after any
   --  equivalent elements already in the container.

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type);
   --  Inserts New_Item in Container, but does not return a cursor designating
   --  the newly-inserted node.

--  TODO: include Replace too???
--
--     procedure Replace
--       (Container : in out Set;
--        New_Item  : Element_Type);

   procedure Exclude
     (Container : in out Set;
      Item      : Element_Type);
   --  Deletes from Container all of the elements equivalent to Item

   procedure Delete
     (Container : in out Set;
      Item      : Element_Type);
   --  Deletes from Container all of the elements equivalent to Item. If there
   --  are no elements equivalent to Item, then it raises Constraint_Error.

   procedure Delete
     (Container : in out Set;
      Position  : in out Cursor);
   --  If Position equals No_Element, then Constraint_Error is raised. If
   --  Position is associated with a set different from Container, then
   --  Program_Error is raised. Otherwise, the node designated by Position is
   --  removed from Container, and Position is set to No_Element.

   procedure Delete_First (Container : in out Set);
   --  Removes the first node from Container

   procedure Delete_Last (Container : in out Set);
   --  Removes the last node from Container

   procedure Union (Target : in out Set; Source : Set);
   --  If Target is busy (cursor tampering is attempted), the Program_Error is
   --  raised. Otherwise, it inserts each element of Source into
   --  Target. Elements are inserted in the canonical order for multisets, such
   --  that the elements from Source are inserted after equivalent elements
   --  already in Target.

   function Union (Left, Right : Set) return Set;
   --  Returns a set comprising the all elements from Left and all of the
   --  elements from Right. The elements from Right follow the equivalent
   --  elements from Left.

   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in out Set; Source : Set);
   --  If Target denotes the same object as Source, the operation does
   --  nothing. If Target is busy (cursor tampering is attempted),
   --  Program_Error is raised. Otherwise, the elements in Target having no
   --  equivalent element in Source are deleted from Target.

   function Intersection (Left, Right : Set) return Set;
   --  If Left denotes the same object as Right, then the function returns a
   --  copy of Left. Otherwise, it returns a set comprising the equivalent
   --  elements from both Left and Right. Items are inserted in the result set
   --  in canonical order, such that the elements from Left precede the
   --  equivalent elements from Right.

   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in out Set; Source : Set);
   --  If Target is busy (cursor tampering is attempted), then Program_Error is
   --  raised. Otherwise, the elements in Target that are equivalent to
   --  elements in Source are deleted from Target.

   function Difference (Left, Right : Set) return Set;
   --  Returns a set comprising the elements from Left that have no equivalent
   --  element in Right.

   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set);
   --  If Target is busy, then Program_Error is raised. Otherwise, the elements
   --  in Target equivalent to elements in Source are deleted from Target, and
   --  the elements in Source not equivalent to elements in Target are inserted
   --  into Target.

   function Symmetric_Difference (Left, Right : Set) return Set;
   --  Returns a set comprising the union of the elements from Target having no
   --  equivalent in Source, and the elements of Source having no equivalent in
   --  Target.

   function "xor" (Left, Right : Set) return Set renames Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean;
   --  Returns True if Left contains an element equivalent to an element of
   --  Right.

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;
   --  Returns True if every element in Subset has an equivalent element in
   --  Of_Set.

   function First (Container : Set) return Cursor;
   --  If Container is empty, the function returns No_Element. Otherwise, it
   --  returns a cursor designating the smallest element.

   function First_Element (Container : Set) return Element_Type;
   --  Equivalent to Element (First (Container))

   function Last (Container : Set) return Cursor;
   --  If Container is empty, the function returns No_Element. Otherwise, it
   --  returns a cursor designating the largest element.

   function Last_Element (Container : Set) return Element_Type;
   --  Equivalent to Element (Last (Container))

   function Next (Position : Cursor) return Cursor;
   --  If Position equals No_Element or Last (Container), the function returns
   --  No_Element. Otherwise, it returns a cursor designating the node that
   --  immediately follows (as per the insertion order) the node designated by
   --  Position.

   procedure Next (Position : in out Cursor);
   --  Equivalent to Position := Next (Position)

   function Previous (Position : Cursor) return Cursor;
   --  If Position equals No_Element or First (Container), the function returns
   --  No_Element. Otherwise, it returns a cursor designating the node that
   --  immediately precedes (as per the insertion order) the node designated by
   --  Position.

   procedure Previous (Position : in out Cursor);
   --  Equivalent to Position := Previous (Position)

   function Find (Container : Set; Item : Element_Type) return Cursor;
   --  Returns a cursor designating the first element in Container equivalent
   --  to Item. If there is no equivalent element, it returns No_Element.

   function Floor (Container : Set; Item : Element_Type) return Cursor;
   --  If Container is empty, the function returns No_Element. If Item is
   --  equivalent to elements in Container, it returns a cursor designating the
   --  first equivalent element. Otherwise, it returns a cursor designating the
   --  largest element less than Item, or No_Element if all elements are
   --  greater than Item.

   function Ceiling (Container : Set; Item : Element_Type) return Cursor;
   --  If Container is empty, the function returns No_Element. If Item is
   --  equivalent to elements of Container, it returns a cursor designating the
   --  last equivalent element. Otherwise, it returns a cursor designating the
   --  smallest element greater than Item, or No_Element if all elements are
   --  less than Item.

   function Contains (Container : Set; Item : Element_Type) return Boolean;
   --  Equivalent to Container.Find (Item) /= No_Element

   function Has_Element (Position : Cursor) return Boolean;
   --  Equivalent to Position /= No_Element

   function "<" (Left, Right : Cursor) return Boolean;
   --  Equivalent to Element (Left) < Element (Right)

   function ">" (Left, Right : Cursor) return Boolean;
   --  Equivalent to Element (Right) < Element (Left)

   function "<" (Left : Cursor; Right : Element_Type) return Boolean;
   --  Equivalent to Element (Left) < Right

   function ">" (Left : Cursor; Right : Element_Type) return Boolean;
   --  Equivalent to Right < Element (Left)

   function "<" (Left : Element_Type; Right : Cursor) return Boolean;
   --  Equivalent to Left < Element (Right)

   function ">" (Left : Element_Type; Right : Cursor) return Boolean;
   --  Equivalent to Element (Right) < Left

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));
   --  Calls Process with a cursor designating each element of Container, in
   --  order from Container.First to Container.Last.

   procedure Reverse_Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));
   --  Calls Process with a cursor designating each element of Container, in
   --  order from Container.Last to Container.First.

   procedure Iterate
     (Container : Set;
      Item      : Element_Type;
      Process   : not null access procedure (Position : Cursor));
   --  Call Process with a cursor designating each element equivalent to Item,
   --  in order from Container.Floor (Item) to Container.Ceiling (Item).

   procedure Reverse_Iterate
     (Container : Set;
      Item      : Element_Type;
      Process   : not null access procedure (Position : Cursor));
   --  Call Process with a cursor designating each element equivalent to Item,
   --  in order from Container.Ceiling (Item) to Container.Floor (Item).

   generic
      type Key_Type (<>) is private;

      with function Key (Element : Element_Type) return Key_Type;

      with function "<" (Left, Right : Key_Type) return Boolean is <>;

   package Generic_Keys is

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
      --  Returns False if Left is less than Right, or Right is less than Left;
      --  otherwise, it returns True.

      function Key (Position : Cursor) return Key_Type;
      --  Equivalent to Key (Element (Position))

      function Element (Container : Set; Key : Key_Type) return Element_Type;
      --  Equivalent to Element (Find (Container, Key))

      procedure Exclude (Container : in out Set; Key : Key_Type);
      --  Deletes from Container any elements whose key is equivalent to Key

      procedure Delete (Container : in out Set; Key : Key_Type);
      --  Deletes from Container any elements whose key is equivalent to
      --  Key. If there are no such elements, then it raises Constraint_Error.

      function Find (Container : Set; Key : Key_Type) return Cursor;
      --  Returns a cursor designating the first element in Container whose key
      --  is equivalent to Key. If there is no equivalent element, it returns
      --  No_Element.

      function Floor (Container : Set; Key : Key_Type) return Cursor;
      --  If Container is empty, the function returns No_Element. If Item is
      --  equivalent to the keys of elements in Container, it returns a cursor
      --  designating the first such element. Otherwise, it returns a cursor
      --  designating the largest element whose key is less than Item, or
      --  No_Element if all keys are greater than Item.

      function Ceiling (Container : Set; Key : Key_Type) return Cursor;
      --  If Container is empty, the function returns No_Element. If Item is
      --  equivalent to the keys of elements of Container, it returns a cursor
      --  designating the last such element. Otherwise, it returns a cursor
      --  designating the smallest element whose key is greater than Item, or
      --  No_Element if all keys are less than Item.

      function Contains (Container : Set; Key : Key_Type) return Boolean;
      --  Equivalent to Find (Container, Key) /= No_Element

      procedure Update_Element  -- Update_Element_Preserving_Key ???
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access
                       procedure (Element : in out Element_Type));
      --  If Position equals No_Element, then Constraint_Error is raised. If
      --  Position is associated with a set object different from Container,
      --  then Program_Error is raised. Otherwise, it makes a copy of the key
      --  of the element designated by Position, and then calls Process with
      --  the element as the parameter. Update_Element then compares the key
      --  value obtained before calling Process to the key value obtained from
      --  the element after calling Process. If the keys are equivalent then
      --  the operation terminates. If Container is busy (cursor tampering has
      --  been attempted), then Program_Error is raised. Otherwise, the node
      --  is moved to its new position (in canonical order).

      procedure Iterate
        (Container : Set;
         Key       : Key_Type;
         Process   : not null access procedure (Position : Cursor));
      --  Call Process with a cursor designating each element equivalent to
      --  Key, in order from Floor (Container, Key) to
      --  Ceiling (Container, Key).

      procedure Reverse_Iterate
        (Container : Set;
         Key       : Key_Type;
         Process   : not null access procedure (Position : Cursor));
      --  Call Process with a cursor designating each element equivalent to
      --  Key, in order from Ceiling (Container, Key) to
      --  Floor (Container, Key).

   end Generic_Keys;

private

   pragma Inline (Next);
   pragma Inline (Previous);

   type Node_Type;
   type Node_Access is access Node_Type;

   type Node_Type is limited record
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Red_Black_Trees.Color_Type := Red_Black_Trees.Red;
      Element : Element_Type;
   end record;

   package Tree_Types is new Red_Black_Trees.Generic_Tree_Types
     (Node_Type,
      Node_Access);

   type Set is new Ada.Finalization.Controlled with record
      Tree : Tree_Types.Tree_Type;
   end record;

   overriding
   procedure Adjust (Container : in out Set);

   overriding
   procedure Finalize (Container : in out Set) renames Clear;

   use Red_Black_Trees;
   use Tree_Types;
   use Ada.Finalization;
   use Ada.Streams;

   type Set_Access is access all Set;
   for Set_Access'Storage_Size use 0;

   type Cursor is record
      Container : Set_Access;
      Node      : Node_Access;
   end record;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor);

   for Cursor'Write use Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor);

   for Cursor'Read use Read;

   No_Element : constant Cursor := Cursor'(null, null);

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Set);

   for Set'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Set);

   for Set'Read use Read;

   Empty_Set : constant Set :=
                 (Controlled with Tree => (First  => null,
                                           Last   => null,
                                           Root   => null,
                                           Length => 0,
                                           Busy   => 0,
                                           Lock   => 0));

end Ada.Containers.Ordered_Multisets;
