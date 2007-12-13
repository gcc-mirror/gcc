------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--               I N D E F I N I T E _ H A S H E D _ M A P S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2007, Free Software Foundation, Inc.         --
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

private with Ada.Containers.Hash_Tables;
private with Ada.Streams;
private with Ada.Finalization;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;

   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Hashed_Maps is
   pragma Preelaborate;
   pragma Remote_Types;

   type Map is tagged private;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Map  : constant Map;
   --  Map objects declared without an initialization expression are
   --  initialized to the value Empty_Map.

   No_Element : constant Cursor;
   --  Cursor objects declared without an initialization expression are
   --  initialized to the value No_Element.

   function "=" (Left, Right : Map) return Boolean;
   --  For each key/element pair in Left, equality attempts to find the key in
   --  Right; if a search fails the equality returns False. The search works by
   --  calling Hash to find the bucket in the Right map that corresponds to the
   --  Left key. If bucket is non-empty, then equality calls Equivalent_Keys
   --  to compare the key (in Left) to the key of each node in the bucket (in
   --  Right); if the keys are equivalent, then the equality test for this
   --  key/element pair (in Left) completes by calling the element equality
   --  operator to compare the element (in Left) to the element of the node
   --  (in Right) whose key matched.

   function Capacity (Container : Map) return Count_Type;
   --  Returns the current capacity of the map. Capacity is the maximum length
   --  before which rehashing in guaranteed not to occur.

   procedure Reserve_Capacity (Container : in out Map; Capacity : Count_Type);
   --  Adjusts the current capacity, by allocating a new buckets array. If the
   --  requested capacity is less than the current capacity, then the capacity
   --  is contracted (to a value not less than the curent length). If the
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
   --  Returns the key of the node designated by the cursor

   function Element (Position : Cursor) return Element_Type;
   --  Returns the element of the node designated by the cursor

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  Assigns the value New_Item to the element designated by the cursor

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type));
   --  Calls Process with the key and element (both having only a constant
   --  view) of the node designed by the cursor.

   procedure Update_Element
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access procedure (Key     : Key_Type;
                                             Element : in out Element_Type));
   --  Calls Process with the key (with only a constant view) and element (with
   --  a variable view) of the node designed by the cursor.

   procedure Move (Target : in out Map; Source : in out Map);
   --  Clears Target (if it's not empty), and then moves (not copies) the
   --  buckets array and nodes from Source to Target.

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);
   --  Conditionally inserts New_Item into the map. If Key is already in the
   --  map, then Inserted returns False and Position designates the node
   --  containing the existing key/element pair (neither of which is modified).
   --  If Key is not already in the map, the Inserted returns True and Position
   --  designates the newly-inserted node container Key and New_Item. The
   --  search for the key works as follows. Hash is called to determine Key's
   --  bucket; if the bucket is non-empty, then Equivalent_Keys is called to
   --  compare Key to each node in that bucket. If the bucket is empty, or
   --  there were no matching keys in the bucket, the search "fails" and the
   --  key/item pair is inserted in the map (and Inserted returns True);
   --  otherwise, the search "succeeds" (and Inserted returns False).

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Attempts to insert Key into the map, performing the usual search (which
   --  involves calling both Hash and Equivalent_Keys); if the search succeeds
   --  (because Key is already in the map), then it raises Constraint_Error.
   --  (This version of Insert is similar to Replace, but having the opposite
   --  exception behavior. It is intended for use when you want to assert that
   --  Key is not already in the map.)

   procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Attempts to insert Key into the map. If Key is already in the map, then
   --  both the existing key and element are assigned the values of Key and
   --  New_Item, respectively. (This version of Insert only raises an exception
   --  if cursor tampering occurs. It is intended for use when you want to
   --  insert the key/element pair in the map, and you don't care whether Key
   --  is already present.)

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);
   --  Searches for Key in the map; if the search fails (because Key was not in
   --  the map), then it raises Constraint_Error. Otherwise, both the existing
   --  key and element are assigned the values of Key and New_Item rsp. (This
   --  is similar to Insert, but with the opposite exception behavior. It is
   --  intended for use when you want to assert that Key is already in the
   --  map.)

   procedure Exclude (Container : in out Map; Key : Key_Type);
   --  Searches for Key in the map, and if found, removes its node from the map
   --  and then deallocates it. The search works as follows. The operation
   --  calls Hash to determine the key's bucket; if the bucket is not empty, it
   --  calls Equivalent_Keys to compare Key to each key in the bucket. (This is
   --  the deletion analog of Include. It is intended for use when you want to
   --  remove the item from the map, but don't care whether the key is already
   --  in the map.)

   procedure Delete (Container : in out Map; Key : Key_Type);
   --  Searches for Key in the map (which involves calling both Hash and
   --  Equivalent_Keys). If the search fails, then the operation raises
   --  Constraint_Eror. Otherwise it removes the node from the map and then
   --  deallocates it. (This is the deletion analog of non-conditional
   --  Insert. It is intended for use when you want to assert that the item is
   --  already in the map.)

   procedure Delete (Container : in out Map; Position : in out Cursor);
   --  Removes the node designated by Position from the map, and then
   --  deallocates the node. The operation calls Hash to determine the bucket,
   --  and then compares Position to each node in the bucket until there's a
   --  match (it does not call Equivalent_Keys).

   function First (Container : Map) return Cursor;
   --  Returns a cursor that designates the first non-empty bucket, by
   --  searching from the beginning of the buckets array.

   function Next (Position : Cursor) return Cursor;
   --  Returns a cursor that designates the node that follows the current one
   --  designated by Position. If Position designates the last node in its
   --  bucket, the operation calls Hash to compute the index of this bucket,
   --  and searches the buckets array for the first non-empty bucket, starting
   --  from that index; otherwise, it simply follows the link to the next node
   --  in the same bucket.

   procedure Next (Position : in out Cursor);
   --  Equivalent to Position := Next (Position)

   function Find (Container : Map; Key : Key_Type) return Cursor;
   --  Searches for Key in the map. Find calls Hash to determine the key's
   --  bucket; if the bucket is not empty, it calls Equivalent_Keys to compare
   --  Key to each key in the bucket. If the search succeeds, Find returns a
   --  cursor designating the matching node; otherwise, it returns No_Element.

   function Contains (Container : Map; Key : Key_Type) return Boolean;
   --  Equivalent to Find (Container, Key) /= No_Element

   function Element (Container : Map; Key : Key_Type) return Element_Type;
   --  Equivalent to Element (Find (Container, Key))

   function Has_Element (Position : Cursor) return Boolean;
   --  Equivalent to Position /= No_Element

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
   --  Calls Process for each node in the map

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

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node_Type is limited record
      Key     : Key_Access;
      Element : Element_Access;
      Next    : Node_Access;
   end record;

   package HT_Types is new Hash_Tables.Generic_Hash_Table_Types
     (Node_Type,
      Node_Access);

   type Map is new Ada.Finalization.Controlled with record
      HT : HT_Types.Hash_Table_Type;
   end record;

   use HT_Types;
   use Ada.Finalization;
   use Ada.Streams;

   overriding
   procedure Adjust (Container : in out Map);

   overriding
   procedure Finalize (Container : in out Map);

   type Map_Access is access constant Map;
   for Map_Access'Storage_Size use 0;

   type Cursor is
      record
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

   No_Element : constant Cursor :=
     (Container => null,
      Node      => null);

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Map);

   for Map'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Map);

   for Map'Read use Read;

   Empty_Map : constant Map := (Controlled with HT => (null, 0, 0, 0));

end Ada.Containers.Indefinite_Hashed_Maps;
