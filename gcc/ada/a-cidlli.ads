------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.INDEFINITE_DOUBLY_LINKED_LISTS               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Finalization;
with Ada.Streams;

generic

   type Element_Type (<>) is private;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Ada.Containers.Indefinite_Doubly_Linked_Lists is
   pragma Preelaborate (Indefinite_Doubly_Linked_Lists);

   type List is tagged private;

   type Cursor is private;

   Empty_List : constant List;

   No_Element : constant Cursor;

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

   function Element (Position : Cursor)
      return Element_Type;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in out Element_Type));

   procedure Replace_Element
     (Position : Cursor;
      By       : Element_Type);

   procedure Move
     (Target : in out List;
      Source : in out List);

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

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

   generic
      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;
   procedure Generic_Sort (Container : in out List);

   generic
      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;
   procedure Generic_Merge
     (Target : in out List;
      Source : in out List);

   procedure Reverse_List (Container : in out List);

   procedure Swap (I, J : Cursor);

   procedure Swap_Links (Container : in out List; I, J : Cursor);

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List);

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Position : Cursor);

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : Cursor);

   function First (Container : List) return Cursor;

   function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   function Last_Element (Container : List) return Element_Type;

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean;

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

private
   type Node_Type;
   type Node_Access is access Node_Type;

   type Element_Access is access Element_Type;

   type Node_Type is
      record
         Element : Element_Access;
         Next    : Node_Access;
         Prev    : Node_Access;
      end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;

   use Ada.Finalization;

   type List is
     new Controlled with record
        First  : Node_Access;
        Last   : Node_Access;
        Length : Count_Type := 0;
     end record;

   procedure Adjust (Container : in out List);

   procedure Finalize (Container : in out List) renames Clear;

   use Ada.Streams;

   procedure Read
     (Stream : access Root_Stream_Type'Class;
      Item   : out List);

   for List'Read use Read;

   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Item   : List);

   for List'Write use Write;

   Empty_List : constant List := List'(Controlled with null, null, 0);

   type List_Access is access constant List;
   for List_Access'Storage_Size use 0;

   type Cursor is
      record
         Container : List_Access;
         Node      : Node_Access;
      end record;

   No_Element : constant Cursor := Cursor'(null, null);

end Ada.Containers.Indefinite_Doubly_Linked_Lists;


