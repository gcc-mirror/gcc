------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_DOUBLY_LINKED_LISTS                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This spec is derived from Ada.Containers.Bounded_Doubly_Linked_Lists in the
--  Ada 2012 RM. The modifications are to facilitate formal proofs by making it
--  easier to express properties.

--  The modifications are:

--    A parameter for the container is added to every function reading the
--    contents of a container: Next, Previous, Query_Element, Has_Element,
--    Iterate, Reverse_Iterate, Element. This change is motivated by the need
--    to have cursors which are valid on different containers (typically a
--    container C and its previous version C'Old) for expressing properties,
--    which is not possible if cursors encapsulate an access to the underlying
--    container.

--    There are three new functions:

--      function Strict_Equal (Left, Right : List) return Boolean;
--      function Left  (Container : List; Position : Cursor) return List;
--      function Right (Container : List; Position : Cursor) return List;

--    See detailed specifications for these subprograms

private with Ada.Streams;
with Ada.Containers;
with Ada.Iterator_Interfaces;

generic
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
                      return Boolean is <>;

package Ada.Containers.Formal_Doubly_Linked_Lists is
   pragma Pure;

   type List (Capacity : Count_Type) is tagged private with
      Constant_Indexing => Constant_Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;
   --  pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_List : constant List;

   No_Element : constant Cursor;

   function Not_No_Element (Position : Cursor) return Boolean;

   package List_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor => Cursor, Has_Element => Not_No_Element);

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'Class;

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

   procedure Assign (Target : in out List; Source : List);

   function Copy (Source : List; Capacity : Count_Type := 0) return List;

   function Element (Container : List; Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type);

   procedure Query_Element
     (Container : List; Position : Cursor;
      Process   : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in out List;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type));

   procedure Move (Target : in out List; Source : in out List);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1);

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type := 1);

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1);

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1);

   procedure Reverse_Elements (Container : in out List);

   procedure Swap
     (Container : in out List;
      I, J      : Cursor);

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor);

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List);

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor);

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor);

   function First (Container : List) return Cursor;

   function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   function Last_Element (Container : List) return Element_Type;

   function Next (Container : List; Position : Cursor) return Cursor;

   procedure Next (Container : List; Position : in out Cursor);

   function Previous (Container : List; Position : Cursor) return Cursor;

   procedure Previous (Container : List; Position : in out Cursor);

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean;

   function Has_Element (Container : List; Position : Cursor) return Boolean;

   procedure Iterate
     (Container : List;
      Process   :
      not null access procedure (Container : List; Position : Cursor));

   procedure Reverse_Iterate
     (Container : List;
      Process   :
      not null access procedure (Container : List; Position : Cursor));

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : List) return Boolean;

      procedure Sort (Container : in out List);

      procedure Merge (Target, Source : in out List);

   end Generic_Sorting;

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : List; Position : Cursor)    --  SHOULD BE ALIASED
   return Constant_Reference_Type;

   function Strict_Equal (Left, Right : List) return Boolean;
   --  Strict_Equal returns True if the containers are physically equal, i.e.
   --  they are structurally equal (function "=" returns True) and that they
   --  have the same set of cursors.

   function Left  (Container : List; Position : Cursor) return List;
   function Right (Container : List; Position : Cursor) return List;
   --  Left returns a container containing all elements preceding Position
   --  (excluded) in Container. Right returns a container containing all
   --  elements following Position (included) in Container. These two new
   --  functions can be used to express invariant properties in loops which
   --  iterate over containers. Left returns the part of the container already
   --  scanned and Right the part not scanned yet.

private

   type Node_Type is record
      Prev    : Count_Type'Base := -1;
      Next    : Count_Type;
      Element : aliased Element_Type;
   end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;

   type Node_Array is array (Count_Type range <>) of Node_Type;
   function "=" (L, R : Node_Array) return Boolean is abstract;

   type List (Capacity : Count_Type) is tagged record
      Nodes  : Node_Array (1 .. Capacity) := (others => <>);
      Free   : Count_Type'Base := -1;
      Busy   : Natural := 0;
      Lock   : Natural := 0;
      Length : Count_Type := 0;
      First  : Count_Type := 0;
      Last   : Count_Type := 0;
   end record;

   use Ada.Streams;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out List);

   for List'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : List);

   for List'Write use Write;

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   type Cursor is record
      Node : Count_Type := 0;
   end record;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor);

   for Cursor'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor);

   for Cursor'Write use Write;

   Empty_List : constant List := (0, others => <>);

   No_Element : constant Cursor := (Node => 0);

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is null record;

end Ada.Containers.Formal_Doubly_Linked_Lists;
