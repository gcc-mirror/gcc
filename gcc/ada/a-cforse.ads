------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . F O R M A L _ O R D E R E D _ S E T S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2013, Free Software Foundation, Inc.         --
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

--  This spec is derived from package Ada.Containers.Bounded_Ordered_Sets in
--  the Ada 2012 RM. The modifications are meant to facilitate formal proofs by
--  making it easier to express properties, and by making the specification of
--  this unit compatible with SPARK 2014. Note that the API of this unit may be
--  subject to incompatible changes as SPARK 2014 evolves.

--  The modifications are:

--    A parameter for the container is added to every function reading the
--    content of a container: Key, Element, Next, Query_Element, Previous,
--    Has_Element, Iterate, Reverse_Iterate. This change is motivated by the
--    need to have cursors which are valid on different containers (typically
--    a container C and its previous version C'Old) for expressing properties,
--    which is not possible if cursors encapsulate an access to the underlying
--    container. The operators "<" and ">" that could not be modified that way
--    have been removed.

--    There are three new functions:

--      function Strict_Equal (Left, Right : Set) return Boolean;
--      function Left  (Container : Set; Position : Cursor) return Set;
--      function Right (Container : Set; Position : Cursor) return Set;

--    See detailed specifications for these subprograms

private with Ada.Containers.Red_Black_Trees;

generic
   type Element_Type is private;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Formal_Ordered_Sets is
   pragma Annotate (GNATprove, External_Axiomatization);
   pragma Pure;

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean
   with
     Global => null;

   type Set (Capacity : Count_Type) is private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element);
   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Set : constant Set;

   No_Element : constant Cursor;

   function "=" (Left, Right : Set) return Boolean with
     Global => null;

   function Equivalent_Sets (Left, Right : Set) return Boolean with
     Global => null;

   function To_Set (New_Item : Element_Type) return Set with
     Global => null;

   function Length (Container : Set) return Count_Type with
     Global => null;

   function Is_Empty (Container : Set) return Boolean with
     Global => null;

   procedure Clear (Container : in out Set) with
     Global => null;

   procedure Assign (Target : in out Set; Source : Set) with
     Pre => Target.Capacity >= Length (Source);

   function Copy (Source : Set; Capacity : Count_Type := 0) return Set with
     Global => null,
     Pre    => Capacity = 0 or else Capacity >= Source.Capacity;

   function Element
     (Container : Set;
      Position  : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Move (Target : in out Set; Source : in out Set) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source);

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity
                 and then (not Contains (Container, New_Item));

   procedure Include
     (Container : in out Set;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity;

   procedure Replace
     (Container : in out Set;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Contains (Container, New_Item);

   procedure Exclude
     (Container : in out Set;
      Item      : Element_Type)
   with
     Global => null;

   procedure Delete
     (Container : in out Set;
      Item      : Element_Type)
   with
     Global => null,
     Pre    => Contains (Container, Item);

   procedure Delete
     (Container : in out Set;
      Position  : in out Cursor)
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Delete_First (Container : in out Set) with
     Global => null;

   procedure Delete_Last (Container : in out Set) with
     Global => null;

   procedure Union (Target : in out Set; Source : Set) with
     Global => null,
     Pre    => Length (Target) + Length (Source) -
                 Length (Intersection (Target, Source)) <= Target.Capacity;

   function Union (Left, Right : Set) return Set with
     Global => null,
     Pre    => Length (Left) + Length (Right) -
                 Length (Intersection (Left, Right)) <= Count_Type'Last;

   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in out Set; Source : Set) with
     Global => null;

   function Intersection (Left, Right : Set) return Set with
     Global => null;

   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in out Set; Source : Set) with
     Global => null;

   function Difference (Left, Right : Set) return Set with
     Global => null;

   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set) with
     Global => null,
     Pre    => Length (Target) + Length (Source) -
                 2 * Length (Intersection (Target, Source)) <= Target.Capacity;

   function Symmetric_Difference (Left, Right : Set) return Set with
     Global => null,
     Pre    => Length (Left) + Length (Right) -
                 2 * Length (Intersection (Left, Right)) <= Count_Type'Last;

   function "xor" (Left, Right : Set) return Set renames Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean with
     Global => null;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean with
     Global => null;

   function First (Container : Set) return Cursor with
     Global => null;

   function First_Element (Container : Set) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Last (Container : Set) return Cursor;

   function Last_Element (Container : Set) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Next (Container : Set; Position : Cursor) return Cursor with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   procedure Next (Container : Set; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Previous (Container : Set; Position : Cursor) return Cursor with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   procedure Previous (Container : Set; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Find (Container : Set; Item : Element_Type) return Cursor with
     Global => null;

   function Floor (Container : Set; Item : Element_Type) return Cursor with
     Global => null;

   function Ceiling (Container : Set; Item : Element_Type) return Cursor with
     Global => null;

   function Contains (Container : Set; Item : Element_Type) return Boolean with
     Global => null;

   function Has_Element (Container : Set; Position : Cursor) return Boolean
   with
     Global => null;

   generic
      type Key_Type (<>) is private;

      with function Key (Element : Element_Type) return Key_Type;

      with function "<" (Left, Right : Key_Type) return Boolean is <>;

   package Generic_Keys is

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean with
        Global => null;

      function Key (Container : Set; Position : Cursor) return Key_Type with
        Global => null;

      function Element (Container : Set; Key : Key_Type) return Element_Type
      with
        Global => null;

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      with
        Global => null;

      procedure Exclude (Container : in out Set; Key : Key_Type) with
        Global => null;

      procedure Delete (Container : in out Set; Key : Key_Type) with
        Global => null;

      function Find (Container : Set; Key : Key_Type) return Cursor with
        Global => null;

      function Floor (Container : Set; Key : Key_Type) return Cursor with
        Global => null;

      function Ceiling (Container : Set; Key : Key_Type) return Cursor with
        Global => null;

      function Contains (Container : Set; Key : Key_Type) return Boolean with
        Global => null;

   end Generic_Keys;

   function Strict_Equal (Left, Right : Set) return Boolean with
        Global => null;
   --  Strict_Equal returns True if the containers are physically equal, i.e.
   --  they are structurally equal (function "=" returns True) and that they
   --  have the same set of cursors.

   function Left  (Container : Set; Position : Cursor) return Set with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;
   function Right (Container : Set; Position : Cursor) return Set with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;
   --  Left returns a container containing all elements preceding Position
   --  (excluded) in Container. Right returns a container containing all
   --  elements following Position (included) in Container. These two new
   --  functions can be used to express invariant properties in loops which
   --  iterate over containers. Left returns the part of the container already
   --  scanned and Right the part not scanned yet.

private

   pragma Inline (Next);
   pragma Inline (Previous);

   type Node_Type is record
      Has_Element : Boolean := False;
      Parent  : Count_Type := 0;
      Left    : Count_Type := 0;
      Right   : Count_Type := 0;
      Color   : Red_Black_Trees.Color_Type;
      Element : Element_Type;
   end record;

   package Tree_Types is
     new Red_Black_Trees.Generic_Bounded_Tree_Types (Node_Type);

   type Set (Capacity : Count_Type) is
     new Tree_Types.Tree_Type (Capacity) with null record;

   use Red_Black_Trees;

   type Cursor is record
      Node : Count_Type;
   end record;

   No_Element : constant Cursor := (Node => 0);

   Empty_Set : constant Set := (Capacity => 0, others => <>);

end Ada.Containers.Formal_Ordered_Sets;
