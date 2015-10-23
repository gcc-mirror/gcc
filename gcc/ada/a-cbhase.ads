------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . B O U N D E D _ H A S H E D _ S E T S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2015, Free Software Foundation, Inc.         --
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

private with Ada.Containers.Hash_Tables;
with Ada.Containers.Helpers;
private with Ada.Streams;
private with Ada.Finalization; use Ada.Finalization;

generic
   type Element_Type is private;

   with function Hash (Element : Element_Type) return Hash_Type;

   with function Equivalent_Elements
          (Left, Right : Element_Type) return Boolean;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Bounded_Hashed_Sets is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Pure;
   pragma Remote_Types;

   type Set (Capacity : Count_Type; Modulus : Hash_Type) is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Element_Type;

   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Set : constant Set;
   --  Set objects declared without an initialization expression are
   --  initialized to the value Empty_Set.

   No_Element : constant Cursor;
   --  Cursor objects declared without an initialization expression are
   --  initialized to the value No_Element.

   function Has_Element (Position : Cursor) return Boolean;
   --  Equivalent to Position /= No_Element

   package Set_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function "=" (Left, Right : Set) return Boolean;
   --  For each element in Left, set equality attempts to find the equal
   --  element in Right; if a search fails, then set equality immediately
   --  returns False. The search works by calling Hash to find the bucket in
   --  the Right set that corresponds to the Left element. If the bucket is
   --  non-empty, the search calls the generic formal element equality operator
   --  to compare the element (in Left) to the element of each node in the
   --  bucket (in Right); the search terminates when a matching node in the
   --  bucket is found, or the nodes in the bucket are exhausted. (Note that
   --  element equality is called here, not Equivalent_Elements. Set equality
   --  is the only operation in which element equality is used. Compare set
   --  equality to Equivalent_Sets, which does call Equivalent_Elements.)

   function Equivalent_Sets (Left, Right : Set) return Boolean;
   --  Similar to set equality, with the difference that the element in Left is
   --  compared to the elements in Right using the generic formal
   --  Equivalent_Elements operation instead of element equality.

   function To_Set (New_Item : Element_Type) return Set;
   --  Constructs a singleton set comprising New_Element. To_Set calls Hash to
   --  determine the bucket for New_Item.

   function Capacity (Container : Set) return Count_Type;
   --  Returns the current capacity of the set. Capacity is the maximum length
   --  before which rehashing in guaranteed not to occur.

   procedure Reserve_Capacity (Container : in out Set; Capacity : Count_Type);
   --  If the value of the Capacity actual parameter is less or equal to
   --  Container.Capacity, then the operation has no effect.  Otherwise it
   --  raises Capacity_Error (as no expansion of capacity is possible for a
   --  bounded form).

   function Default_Modulus (Capacity : Count_Type) return Hash_Type;
   --  Returns a modulus value (hash table size) which is optimal for the
   --  specified capacity (which corresponds to the maximum number of items).

   function Length (Container : Set) return Count_Type;
   --  Returns the number of items in the set

   function Is_Empty (Container : Set) return Boolean;
   --  Equivalent to Length (Container) = 0

   procedure Clear (Container : in out Set);
   --  Removes all of the items from the set

   function Element (Position : Cursor) return Element_Type;
   --  Returns the element of the node designated by the cursor

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type);
   --  If New_Item is equivalent (as determined by calling Equivalent_Elements)
   --  to the element of the node designated by Position, then New_Element is
   --  assigned to that element. Otherwise, it calls Hash to determine the
   --  bucket for New_Item. If the bucket is not empty, then it calls
   --  Equivalent_Elements for each node in that bucket to determine whether
   --  New_Item is equivalent to an element in that bucket. If
   --  Equivalent_Elements returns True then Program_Error is raised (because
   --  an element may appear only once in the set); otherwise, New_Item is
   --  assigned to the node designated by Position, and the node is moved to
   --  its new bucket.

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));
   --  Calls Process with the element (having only a constant view) of the node
   --  designated by the cursor.

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is private
        with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Set;
      Position  : Cursor) return Constant_Reference_Type;

   procedure Assign (Target : in out Set; Source : Set);
   --  If Target denotes the same object as Source, then the operation has no
   --  effect. If the Target capacity is less than the Source length, then
   --  Assign raises Capacity_Error.  Otherwise, Assign clears Target and then
   --  copies the (active) elements from Source to Target.

   function Copy
     (Source   : Set;
      Capacity : Count_Type := 0;
      Modulus  : Hash_Type := 0) return Set;
   --  Constructs a new set object whose elements correspond to Source.  If the
   --  Capacity parameter is 0, then the capacity of the result is the same as
   --  the length of Source. If the Capacity parameter is equal or greater than
   --  the length of Source, then the capacity of the result is the specified
   --  value. Otherwise, Copy raises Capacity_Error. If the Modulus parameter
   --  is 0, then the modulus of the result is the value returned by a call to
   --  Default_Modulus with the capacity parameter determined as above;
   --  otherwise the modulus of the result is the specified value.

   procedure Move (Target : in out Set; Source : in out Set);
   --  Clears Target (if it's not empty), and then moves (not copies) the
   --  buckets array and nodes from Source to Target.

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);
   --  Conditionally inserts New_Item into the set. If New_Item is already in
   --  the set, then Inserted returns False and Position designates the node
   --  containing the existing element (which is not modified). If New_Item is
   --  not already in the set, then Inserted returns True and Position
   --  designates the newly-inserted node containing New_Item. The search for
   --  an existing element works as follows. Hash is called to determine
   --  New_Item's bucket; if the bucket is non-empty, then Equivalent_Elements
   --  is called to compare New_Item to the element of each node in that
   --  bucket. If the bucket is empty, or there were no equivalent elements in
   --  the bucket, the search "fails" and the New_Item is inserted in the set
   --  (and Inserted returns True); otherwise, the search "succeeds" (and
   --  Inserted returns False).

   procedure Insert  (Container : in out Set; New_Item : Element_Type);
   --  Attempts to insert New_Item into the set, performing the usual insertion
   --  search (which involves calling both Hash and Equivalent_Elements); if
   --  the search succeeds (New_Item is equivalent to an element already in the
   --  set, and so was not inserted), then this operation raises
   --  Constraint_Error. (This version of Insert is similar to Replace, but
   --  having the opposite exception behavior. It is intended for use when you
   --  want to assert that the item is not already in the set.)

   procedure Include (Container : in out Set; New_Item : Element_Type);
   --  Attempts to insert New_Item into the set. If an element equivalent to
   --  New_Item is already in the set (the insertion search succeeded, and
   --  hence New_Item was not inserted), then the value of New_Item is assigned
   --  to the existing element. (This insertion operation only raises an
   --  exception if cursor tampering occurs. It is intended for use when you
   --  want to insert the item in the set, and you don't care whether an
   --  equivalent element is already present.)

   procedure Replace (Container : in out Set; New_Item : Element_Type);
   --  Searches for New_Item in the set; if the search fails (because an
   --  equivalent element was not in the set), then it raises
   --  Constraint_Error. Otherwise, the existing element is assigned the value
   --  New_Item. (This is similar to Insert, but with the opposite exception
   --  behavior. It is intended for use when you want to assert that the item
   --  is already in the set.)

   procedure Exclude (Container : in out Set; Item : Element_Type);
   --  Searches for Item in the set, and if found, removes its node from the
   --  set and then deallocates it. The search works as follows. The operation
   --  calls Hash to determine the item's bucket; if the bucket is not empty,
   --  it calls Equivalent_Elements to compare Item to the element of each node
   --  in the bucket. (This is the deletion analog of Include. It is intended
   --  for use when you want to remove the item from the set, but don't care
   --  whether the item is already in the set.)

   procedure Delete  (Container : in out Set; Item : Element_Type);
   --  Searches for Item in the set (which involves calling both Hash and
   --  Equivalent_Elements). If the search fails, then the operation raises
   --  Constraint_Error. Otherwise it removes the node from the set and then
   --  deallocates it. (This is the deletion analog of non-conditional
   --  Insert. It is intended for use when you want to assert that the item is
   --  already in the set.)

   procedure Delete (Container : in out Set; Position : in out Cursor);
   --  Removes the node designated by Position from the set, and then
   --  deallocates the node. The operation calls Hash to determine the bucket,
   --  and then compares Position to each node in the bucket until there's a
   --  match (it does not call Equivalent_Elements).

   procedure Union (Target : in out Set; Source : Set);
   --  Iterates over the Source set, and conditionally inserts each element
   --  into Target.

   function Union (Left, Right : Set) return Set;
   --  The operation first copies the Left set to the result, and then iterates
   --  over the Right set to conditionally insert each element into the result.

   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in out Set; Source : Set);
   --  Iterates over the Target set (calling First and Next), calling Find to
   --  determine whether the element is in Source. If an equivalent element is
   --  not found in Source, the element is deleted from Target.

   function Intersection (Left, Right : Set) return Set;
   --  Iterates over the Left set, calling Find to determine whether the
   --  element is in Right. If an equivalent element is found, it is inserted
   --  into the result set.

   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in out Set; Source : Set);
   --  Iterates over the Source (calling First and Next), calling Find to
   --  determine whether the element is in Target. If an equivalent element is
   --  found, it is deleted from Target.

   function Difference (Left, Right : Set) return Set;
   --  Iterates over the Left set, calling Find to determine whether the
   --  element is in the Right set. If an equivalent element is not found, the
   --  element is inserted into the result set.

   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set);
   --  The operation iterates over the Source set, searching for the element
   --  in Target (calling Hash and Equivalent_Elements). If an equivalent
   --  element is found, it is removed from Target; otherwise it is inserted
   --  into Target.

   function Symmetric_Difference (Left, Right : Set) return Set;
   --  The operation first iterates over the Left set. It calls Find to
   --  determine whether the element is in the Right set. If no equivalent
   --  element is found, the element from Left is inserted into the result. The
   --  operation then iterates over the Right set, to determine whether the
   --  element is in the Left set. If no equivalent element is found, the Right
   --  element is inserted into the result.

   function "xor" (Left, Right : Set) return Set
     renames Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean;
   --  Iterates over the Left set (calling First and Next), calling Find to
   --  determine whether the element is in the Right set. If an equivalent
   --  element is found, the operation immediately returns True. The operation
   --  returns False if the iteration over Left terminates without finding any
   --  equivalent element in Right.

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;
   --  Iterates over Subset (calling First and Next), calling Find to determine
   --  whether the element is in Of_Set. If no equivalent element is found in
   --  Of_Set, the operation immediately returns False. The operation returns
   --  True if the iteration over Subset terminates without finding an element
   --  not in Of_Set (that is, every element in Subset is equivalent to an
   --  element in Of_Set).

   function First (Container : Set) return Cursor;
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

   function Find
     (Container : Set;
      Item      : Element_Type) return Cursor;
   --  Searches for Item in the set. Find calls Hash to determine the item's
   --  bucket; if the bucket is not empty, it calls Equivalent_Elements to
   --  compare Item to each element in the bucket. If the search succeeds, Find
   --  returns a cursor designating the node containing the equivalent element;
   --  otherwise, it returns No_Element.

   function Contains (Container : Set; Item : Element_Type) return Boolean;
   --  Equivalent to Find (Container, Item) /= No_Element

   function Equivalent_Elements (Left, Right : Cursor) return Boolean;
   --  Returns the result of calling Equivalent_Elements with the elements of
   --  the nodes designated by cursors Left and Right.

   function Equivalent_Elements
     (Left  : Cursor;
      Right : Element_Type) return Boolean;
   --  Returns the result of calling Equivalent_Elements with element of the
   --  node designated by Left and element Right.

   function Equivalent_Elements
     (Left  : Element_Type;
      Right : Cursor) return Boolean;
   --  Returns the result of calling Equivalent_Elements with element Left and
   --  the element of the node designated by Right.

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));
   --  Calls Process for each node in the set

   function Iterate
     (Container : Set)
      return Set_Iterator_Interfaces.Forward_Iterator'Class;

   generic
      type Key_Type (<>) is private;

      with function Key (Element : Element_Type) return Key_Type;

      with function Hash (Key : Key_Type) return Hash_Type;

      with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

   package Generic_Keys is

      function Key (Position : Cursor) return Key_Type;
      --  Applies generic formal operation Key to the element of the node
      --  designated by Position.

      function Element (Container : Set; Key : Key_Type) return Element_Type;
      --  Searches (as per the key-based Find) for the node containing Key, and
      --  returns the associated element.

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type);
      --  Searches (as per the key-based Find) for the node containing Key, and
      --  then replaces the element of that node (as per the element-based
      --  Replace_Element).

      procedure Exclude (Container : in out Set; Key : Key_Type);
      --  Searches for Key in the set, and if found, removes its node from the
      --  set and then deallocates it. The search works by first calling Hash
      --  (on Key) to determine the bucket; if the bucket is not empty, it
      --  calls Equivalent_Keys to compare parameter Key to the value of
      --  generic formal operation Key applied to element of each node in the
      --  bucket.

      procedure Delete (Container : in out Set; Key : Key_Type);
      --  Deletes the node containing Key as per Exclude, with the difference
      --  that Constraint_Error is raised if Key is not found.

      function Find (Container : Set; Key : Key_Type) return Cursor;
      --  Searches for the node containing Key, and returns a cursor
      --  designating the node. The search works by first calling Hash (on Key)
      --  to determine the bucket. If the bucket is not empty, the search
      --  compares Key to the element of each node in the bucket, and returns
      --  the matching node. The comparison itself works by applying the
      --  generic formal Key operation to the element of the node, and then
      --  calling generic formal operation Equivalent_Keys.

      function Contains (Container : Set; Key : Key_Type) return Boolean;
      --  Equivalent to Find (Container, Key) /= No_Element

      procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access
                       procedure (Element : in out Element_Type));
      --  Calls Process with the element of the node designated by Position,
      --  but with the restriction that the key-value of the element is not
      --  modified. The operation first makes a copy of the value returned by
      --  applying generic formal operation Key on the element of the node, and
      --  then calls Process with the element. The operation verifies that the
      --  key-part has not been modified by calling generic formal operation
      --  Equivalent_Keys to compare the saved key-value to the value returned
      --  by applying generic formal operation Key to the post-Process value of
      --  element. If the key values compare equal then the operation
      --  completes. Otherwise, the node is removed from the map and
      --  Program_Error is raised.

      type Reference_Type (Element : not null access Element_Type) is private
        with Implicit_Dereference => Element;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Position  : Cursor) return Reference_Type;

      function Constant_Reference
        (Container : aliased Set;
         Key       : Key_Type) return Constant_Reference_Type;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Key       : Key_Type) return Reference_Type;

   private
      type Set_Access is access all Set;
      for Set_Access'Storage_Size use 0;

      package Impl is new Helpers.Generic_Implementation;

      type Reference_Control_Type is
         new Impl.Reference_Control_Type with
      record
         Container : Set_Access;
         Index     : Hash_Type;
         Old_Pos   : Cursor;
         Old_Hash  : Hash_Type;
      end record;

      overriding procedure Finalize (Control : in out Reference_Control_Type);
      pragma Inline (Finalize);

      type Reference_Type (Element : not null access Element_Type) is record
         Control  : Reference_Control_Type;
      end record;

      use Ada.Streams;

      procedure Read
        (Stream : not null access Root_Stream_Type'Class;
         Item   : out Reference_Type);

      for Reference_Type'Read use Read;

      procedure Write
        (Stream : not null access Root_Stream_Type'Class;
         Item   : Reference_Type);

      for Reference_Type'Write use Write;

   end Generic_Keys;

private
   pragma Inline (Next);

   type Node_Type is record
      Element : aliased Element_Type;
      Next    : Count_Type;
   end record;

   package HT_Types is
     new Hash_Tables.Generic_Bounded_Hash_Table_Types (Node_Type);

   type Set (Capacity : Count_Type; Modulus : Hash_Type) is
     new HT_Types.Hash_Table_Type (Capacity, Modulus) with null record;

   use HT_Types, HT_Types.Implementation;
   use Ada.Streams;

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Set);

   for Set'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Set);

   for Set'Read use Read;

   type Set_Access is access all Set;
   for Set_Access'Storage_Size use 0;

   --  Note: If a Cursor object has no explicit initialization expression,
   --  it must default initialize to the same value as constant No_Element.
   --  The Node component of type Cursor has scalar type Count_Type, so it
   --  requires an explicit initialization expression of its own declaration,
   --  in order for objects of record type Cursor to properly initialize.

   type Cursor is record
      Container : Set_Access;
      Node      : Count_Type := 0;
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

   --  Three operations are used to optimize in the expansion of "for ... of"
   --  loops: the Next(Cursor) procedure in the visible part, and the following
   --  Pseudo_Reference and Get_Element_Access functions. See Sem_Ch5 for
   --  details.

   function Pseudo_Reference
     (Container : aliased Set'Class) return Reference_Control_Type;
   pragma Inline (Pseudo_Reference);
   --  Creates an object of type Reference_Control_Type pointing to the
   --  container, and increments the Lock. Finalization of this object will
   --  decrement the Lock.

   type Element_Access is access all Element_Type with
     Storage_Size => 0;

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access;
   --  Returns a pointer to the element designated by Position.

   Empty_Set : constant Set :=
                 (Hash_Table_Type with Capacity => 0, Modulus => 0);

   No_Element : constant Cursor := (Container => null, Node => 0);

   type Iterator is new Limited_Controlled and
     Set_Iterator_Interfaces.Forward_Iterator with
   record
      Container : Set_Access;
   end record
     with Disable_Controlled => not T_Check;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end Ada.Containers.Bounded_Hashed_Sets;
