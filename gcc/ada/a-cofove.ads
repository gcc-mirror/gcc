------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . F O R M A L _ V E C T O R S        --
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

--  This spec is derived from package Ada.Containers.Bounded_Vectors in the Ada
--  2012 RM. The modifications are meant to facilitate formal proofs by making
--  it easier to express properties, and by making the specification of this
--  unit compatible with SPARK 2014. Note that the API of this unit may be
--  subject to incompatible changes as SPARK 2014 evolves.

--  The modifications are:

--    A parameter for the container is added to every function reading the
--    content of a container: Element, Next, Query_Element, Previous, Iterate,
--    Has_Element, Reverse_Iterate. This change is motivated by the need
--    to have cursors which are valid on different containers (typically a
--    container C and its previous version C'Old) for expressing properties,
--    which is not possible if cursors encapsulate an access to the underlying
--    container.

--    There are three new functions:

--      function Strict_Equal (Left, Right : Vector) return Boolean;
--      function Left  (Container : Vector; Position : Cursor) return Vector;
--      function Right (Container : Vector; Position : Cursor) return Vector;

--    See detailed specifications for these subprograms

with Ada.Containers;
use Ada.Containers;

generic
   type Index_Type is range <>;
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Formal_Vectors is
   pragma Annotate (GNATprove, External_Axiomatization);
   pragma Pure;

   subtype Extended_Index is Index_Type'Base
   range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   subtype Capacity_Range is
     Count_Type range 0 .. Count_Type (Index_Type'Last - Index_Type'First + 1);

   type Vector (Capacity : Capacity_Range) is private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Vector : constant Vector;

   No_Element : constant Cursor;

   function "=" (Left, Right : Vector) return Boolean with
     Global => null;

   function To_Vector
     (New_Item : Element_Type;
      Length   : Count_Type) return Vector
   with
     Global => null;

   function "&" (Left, Right : Vector) return Vector with
     Global => null,
     Pre    => Capacity_Range'Last - Length (Left) >= Length (Right);

   function "&" (Left : Vector; Right : Element_Type) return Vector with
     Global => null,
     Pre    => Length (Left) < Capacity_Range'Last;

   function "&" (Left : Element_Type; Right : Vector) return Vector with
     Global => null,
     Pre    => Length (Right) < Capacity_Range'Last;

   function "&" (Left, Right : Element_Type) return Vector with
     Global => null,
     Pre    => Capacity_Range'Last >= 2;

   function Capacity (Container : Vector) return Count_Type with
     Global => null;

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Count_Type)
   with
     Global => null,
     Pre    => Capacity <= Container.Capacity;

   function Length (Container : Vector) return Count_Type with
     Global => null;

   procedure Set_Length
     (Container : in out Vector;
      New_Length    : Count_Type)
   with
     Global => null,
     Pre    => New_Length <= Length (Container);

   function Is_Empty (Container : Vector) return Boolean with
     Global => null;

   procedure Clear (Container : in out Vector) with
     Global => null;

   procedure Assign (Target : in out Vector; Source : Vector) with
     Global => null,
     Pre    => Length (Source) <= Target.Capacity;

   function Copy
     (Source   : Vector;
      Capacity : Count_Type := 0) return Vector
   with
     Global => null,
     Pre    => Length (Source) <= Capacity and then Capacity in Capacity_Range;

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index) return Cursor
   with
     Global => null;

   function To_Index (Position : Cursor) return Extended_Index with
     Global => null;

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   with
     Global => null,
     Pre    => First_Index (Container) <= Index
                 and then Index <= Last_Index (Container);

   function Element
     (Container : Vector;
      Position  : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => First_Index (Container) <= Index
                 and then Index <= Last_Index (Container);

   procedure Replace_Element
     (Container : in out Vector;
      Position  : Cursor;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Move (Target : in out Vector; Source : in out Vector) with
     Global => null,
     Pre    => Length (Source) <= Target.Capacity;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   with
     Global => null,
     Pre    => First_Index (Container) <= Before
                 and then Before <= Last_Index (Container) + 1
                 and then Length (Container) < Container.Capacity;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity
                 and then (Has_Element (Container, Before)
                            or else Before = No_Element);

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector;
      Position  : out Cursor)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity
                 and then (Has_Element (Container, Before)
                            or else Before = No_Element);

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => First_Index (Container) <= Before
                 and then Before <= Last_Index (Container) + 1
                 and then Length (Container) + Count <= Container.Capacity;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => Length (Container) + Count <= Container.Capacity
                 and then (Has_Element (Container, Before)
                            or else Before = No_Element);

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => Length (Container) + Count <= Container.Capacity
                 and then (Has_Element (Container, Before)
                            or else Before = No_Element);

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Vector)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity;

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => Length (Container) + Count <= Container.Capacity;

   procedure Append
     (Container : in out Vector;
      New_Item  : Vector)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => Length (Container) + Count <= Container.Capacity;

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => First_Index (Container) <= Index
                 and then Index <= Last_Index (Container) + 1;

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   with
     Global => null,
     Pre    => Has_Element (Container, Position);

   procedure Delete_First
     (Container : in out Vector;
      Count     : Count_Type := 1)
   with
     Global => null;

   procedure Delete_Last
     (Container : in out Vector;
      Count     : Count_Type := 1)
   with
     Global => null;

   procedure Reverse_Elements (Container : in out Vector) with
     Global => null;

   procedure Swap (Container : in out Vector; I, J : Index_Type) with
     Global => null,
     Pre    => First_Index (Container) <= I
                 and then I <= Last_Index (Container)
                 and then First_Index (Container) <= J
                 and then J <= Last_Index (Container);

   procedure Swap (Container : in out Vector; I, J : Cursor) with
     Global => null,
     Pre    => Has_Element (Container, I) and then Has_Element (Container, J);

   function First_Index (Container : Vector) return Index_Type with
     Global => null;

   function First (Container : Vector) return Cursor with
     Global => null;

   function First_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Last_Index (Container : Vector) return Extended_Index with
     Global => null;

   function Last (Container : Vector) return Cursor with
     Global => null;

   function Last_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Next (Container : Vector; Position : Cursor) return Cursor with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   procedure Next (Container : Vector; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Previous (Container : Vector; Position : Cursor) return Cursor with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   procedure Previous (Container : Vector; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   with
     Global => null;

   function Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   with
     Global => null;

   function Reverse_Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   with
     Global => null;

   function Has_Element (Container : Vector; Position : Cursor) return Boolean
   with
     Global => null;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean with
        Global => null;

      procedure Sort (Container : in out Vector) with
        Global => null;

      procedure Merge (Target : in out Vector; Source : in out Vector) with
        Global => null;

   end Generic_Sorting;

   function Strict_Equal (Left, Right : Vector) return Boolean with
     Global => null;
   --  Strict_Equal returns True if the containers are physically equal, i.e.
   --  they are structurally equal (function "=" returns True) and that they
   --  have the same set of cursors.

   function Left (Container : Vector; Position : Cursor) return Vector with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;
   function Right (Container : Vector; Position : Cursor) return Vector with
     Global => null,
     Pre    => Has_Element (Container, Position) or else Position = No_Element;
   --  Left returns a container containing all elements preceding Position
   --  (excluded) in Container. Right returns a container containing all
   --  elements following Position (included) in Container. These two new
   --  functions can be used to express invariant properties in loops which
   --  iterate over containers. Left returns the part of the container already
   --  scanned and Right the part not scanned yet.

private

   pragma Inline (First_Index);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (First_Element);
   pragma Inline (Last_Element);
   pragma Inline (Replace_Element);
   pragma Inline (Contains);
   pragma Inline (Next);
   pragma Inline (Previous);

   type Elements_Array is array (Count_Type range <>) of Element_Type;
   function "=" (L, R : Elements_Array) return Boolean is abstract;

   type Vector (Capacity : Capacity_Range) is record
      Elements : Elements_Array (1 .. Capacity);
      Last     : Extended_Index := No_Index;
   end record;

   type Cursor is record
      Valid : Boolean    := True;
      Index : Index_Type := Index_Type'First;
   end record;

   Empty_Vector : constant Vector := (Capacity => 0, others => <>);

   No_Element : constant Cursor := (Valid => False, Index => Index_Type'First);

end Ada.Containers.Formal_Vectors;
