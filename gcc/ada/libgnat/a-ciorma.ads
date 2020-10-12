------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.INDEFINITE_ORDERED_MAPS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2020, Free Software Foundation, Inc.         --
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

private with Ada.Containers.Red_Black_Trees;
private with Ada.Finalization;
private with Ada.Streams;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Ordered_Maps with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate;
   pragma Remote_Types;

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

   type Map is tagged private
   with Constant_Indexing => Constant_Reference,
        Variable_Indexing => Reference,
        Default_Iterator  => Iterate,
        Iterator_Element  => Element_Type;

   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Map : constant Map;

   No_Element : constant Cursor;
   function Has_Element (Position : Cursor) return Boolean;

   package Map_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

   function Key (Position : Cursor) return Key_Type;

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type));

   procedure Update_Element
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access procedure (Key     : Key_Type;
                                             Element : in out Element_Type));

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

   function Reference
     (Container : aliased in out Map;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   function Constant_Reference
     (Container : aliased Map;
      Key       : Key_Type) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Map;
      Key       : Key_Type) return Reference_Type;
   pragma Inline (Reference);

   procedure Assign (Target : in out Map; Source : Map);

   function Copy (Source : Map) return Map;

   procedure Move (Target : in out Map; Source : in out Map);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Exclude (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Position : in out Cursor);

   procedure Delete_First (Container : in out Map);

   procedure Delete_Last (Container : in out Map);

   function First (Container : Map) return Cursor;

   function First_Element (Container : Map) return Element_Type;

   function First_Key (Container : Map) return Key_Type;

   function Last (Container : Map) return Cursor;

   function Last_Element (Container : Map) return Element_Type;

   function Last_Key (Container : Map) return Key_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find (Container : Map; Key : Key_Type) return Cursor;

   function Element (Container : Map; Key : Key_Type) return Element_Type;

   function Floor (Container : Map; Key : Key_Type) return Cursor;

   function Ceiling (Container : Map; Key : Key_Type) return Cursor;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function "<" (Left, Right : Cursor) return Boolean;

   function ">" (Left, Right : Cursor) return Boolean;

   function "<" (Left : Cursor; Right : Key_Type) return Boolean;

   function ">" (Left : Cursor; Right : Key_Type) return Boolean;

   function "<" (Left : Key_Type; Right : Cursor) return Boolean;

   function ">" (Left : Key_Type; Right : Cursor) return Boolean;

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

   --  The map container supports iteration in both the forward and reverse
   --  directions, hence these constructor functions return an object that
   --  supports the Reversible_Iterator interface.

   function Iterate
     (Container : Map)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate
     (Container : Map;
      Start     : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class;

private

   pragma Inline (Next);
   pragma Inline (Previous);

   type Node_Type;
   type Node_Access is access Node_Type;

   type Key_Access is access Key_Type;
   type Element_Access is access all Element_Type;

   type Node_Type is limited record
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Red_Black_Trees.Color_Type := Red_Black_Trees.Red;
      Key     : Key_Access;
      Element : Element_Access;
   end record;

   package Tree_Types is new Red_Black_Trees.Generic_Tree_Types
     (Node_Type,
      Node_Access);

   type Map is new Ada.Finalization.Controlled with record
      Tree : Tree_Types.Tree_Type;
   end record;

   overriding procedure Adjust (Container : in out Map);

   overriding procedure Finalize (Container : in out Map) renames Clear;

   use Red_Black_Trees;
   use Tree_Types, Tree_Types.Implementation;
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

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type);

   for Constant_Reference_Type'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type);

   for Constant_Reference_Type'Write use Write;

   type Reference_Type
     (Element : not null access Element_Type) is
      record
         Control : Reference_Control_Type :=
           raise Program_Error with "uninitialized reference";
         --  The RM says, "The default initialization of an object of
         --  type Constant_Reference_Type or Reference_Type propagates
         --  Program_Error."
      end record;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type);

   for Reference_Type'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type);

   for Reference_Type'Write use Write;

   --  Three operations are used to optimize in the expansion of "for ... of"
   --  loops: the Next(Cursor) procedure in the visible part, and the following
   --  Pseudo_Reference and Get_Element_Access functions.  See Sem_Ch5 for
   --  details.

   function Pseudo_Reference
     (Container : aliased Map'Class) return Reference_Control_Type;
   pragma Inline (Pseudo_Reference);
   --  Creates an object of type Reference_Control_Type pointing to the
   --  container, and increments the Lock. Finalization of this object will
   --  decrement the Lock.

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access;
   --  Returns a pointer to the element designated by Position.

   Empty_Map : constant Map := (Controlled with others => <>);

   No_Element : constant Cursor := Cursor'(null, null);

   type Iterator is new Limited_Controlled and
     Map_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Map_Access;
      Node      : Node_Access;
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

end Ada.Containers.Indefinite_Ordered_Maps;
