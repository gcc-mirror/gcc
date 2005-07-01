------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--             I N D E F I N I T E _ O R D E R E D _ M A P S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with Ada.Containers.Red_Black_Trees;
with Ada.Finalization;
with Ada.Streams;

generic

   type Key_Type (<>) is private;

   type Element_Type (<>) is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Ordered_Maps is
pragma Preelaborate (Indefinite_Ordered_Maps);

   type Map is tagged private;

   type Cursor is private;

   Empty_Map : constant Map;

   No_Element : constant Cursor;

   function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

   function Key (Position : Cursor) return Key_Type;

   function Element (Position : Cursor) return Element_Type;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type));

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : in out Element_Type));

   procedure Replace_Element (Position : Cursor; By : Element_Type);

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

   procedure Delete
     (Container : in out Map;
      Key       : Key_Type);

   procedure Delete
     (Container : in out Map;
      Position  : in out Cursor);

   procedure Delete_First (Container : in out Map);

   procedure Delete_Last (Container : in out Map);

   procedure Exclude
     (Container : in out Map;
      Key       : Key_Type);

   function Contains
     (Container : Map;
      Key       : Key_Type) return Boolean;

   function Find
     (Container : Map;
      Key       : Key_Type) return Cursor;

   function Element
     (Container : Map;
      Key       : Key_Type) return Element_Type;

   function Floor
     (Container : Map;
      Key       : Key_Type) return Cursor;

   function Ceiling
     (Container : Map;
      Key       : Key_Type) return Cursor;

   function First (Container : Map) return Cursor;

   function First_Key (Container : Map) return Key_Type;

   function First_Element (Container : Map) return Element_Type;

   function Last (Container : Map) return Cursor;

   function Last_Key (Container : Map) return Key_Type;

   function Last_Element (Container : Map) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

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

private

   type Node_Type;
   type Node_Access is access Node_Type;

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

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

   procedure Adjust (Container : in out Map);

   procedure Finalize (Container : in out Map) renames Clear;

   use Red_Black_Trees;
   use Tree_Types;
   use Ada.Finalization;

   type Map_Access is access Map;
   for Map_Access'Storage_Size use 0;

   type Cursor is record
      Container : Map_Access;
      Node      : Node_Access;
   end record;

   No_Element : constant Cursor := Cursor'(null, null);

   use Ada.Streams;

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Map);

   for Map'Write use Write;

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container : out Map);

   for Map'Read use Read;

   Empty_Map : constant Map :=
                 (Controlled with Tree => (First  => null,
                                           Last   => null,
                                           Root   => null,
                                           Length => 0,
                                           Busy   => 0,
                                           Lock   => 0));

end Ada.Containers.Indefinite_Ordered_Maps;
