------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                   ADA.CONTAINERS.INDEFINITE_MULTIWAY_TREES               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
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

with Ada.Containers.Helpers;
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Text_Buffers;

generic
   type Element_Type (<>) is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Multiway_Trees with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate;
   pragma Remote_Types;

   type Tree is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Element_Type;

   pragma Preelaborable_Initialization (Tree);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Tree : constant Tree;

   No_Element : constant Cursor;
   function Has_Element (Position : Cursor) return Boolean;

   package Tree_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Equal_Subtree
     (Left_Position  : Cursor;
      Right_Position : Cursor) return Boolean;

   function "=" (Left, Right : Tree) return Boolean;

   function Is_Empty (Container : Tree) return Boolean;

   function Node_Count (Container : Tree) return Count_Type;

   function Subtree_Node_Count (Position : Cursor) return Count_Type;

   function Depth (Position : Cursor) return Count_Type;

   function Is_Root (Position : Cursor) return Boolean;

   function Is_Leaf (Position : Cursor) return Boolean;

   function Root (Container : Tree) return Cursor;

   procedure Clear (Container : in out Tree);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out Tree;
      Position  : Cursor;
      New_Item  : Element_Type);

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in out Tree;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type));

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is private
        with Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Element_Type) is private
        with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Tree;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Tree;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   procedure Assign (Target : in out Tree; Source : Tree);

   function Copy (Source : Tree) return Tree;

   procedure Move (Target : in out Tree; Source : in out Tree);

   procedure Delete_Leaf
     (Container : in out Tree;
      Position  : in out Cursor);

   procedure Delete_Subtree
     (Container : in out Tree;
      Position  : in out Cursor);

   procedure Swap
     (Container : in out Tree;
      I, J      : Cursor);

   function Find
     (Container : Tree;
      Item      : Element_Type) return Cursor;

   --  This version of the AI:
   --   10-06-02  AI05-0136-1/07
   --  declares Find_In_Subtree this way:
   --
   --  function Find_In_Subtree
   --    (Container : Tree;
   --     Item      : Element_Type;
   --     Position  : Cursor) return Cursor;
   --
   --  It seems that the Container parameter is there by mistake, but we need
   --  an official ruling from the ARG. ???

   function Find_In_Subtree
     (Position : Cursor;
      Item     : Element_Type) return Cursor;

   --  This version of the AI:
   --   10-06-02  AI05-0136-1/07
   --  declares Ancestor_Find this way:
   --
   --  function Ancestor_Find
   --    (Container : Tree;
   --     Item      : Element_Type;
   --     Position  : Cursor) return Cursor;
   --
   --  It seems that the Container parameter is there by mistake, but we need
   --  an official ruling from the ARG. ???

   function Ancestor_Find
     (Position : Cursor;
      Item     : Element_Type) return Cursor;

   function Contains
     (Container : Tree;
      Item      : Element_Type) return Boolean;

   procedure Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor));

   procedure Iterate_Subtree
     (Position  : Cursor;
      Process   : not null access procedure (Position : Cursor));

   function Iterate (Container : Tree)
     return Tree_Iterator_Interfaces.Forward_Iterator'Class;

   function Iterate_Subtree (Position : Cursor)
     return Tree_Iterator_Interfaces.Forward_Iterator'Class;

   function Iterate_Children
     (Container : Tree;
      Parent    : Cursor)
     return Tree_Iterator_Interfaces.Reversible_Iterator'Class;

   function Child_Count (Parent : Cursor) return Count_Type;

   function Child_Depth (Parent, Child : Cursor) return Count_Type;

   procedure Insert_Child
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Insert_Child
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1);

   procedure Prepend_Child
     (Container : in out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Append_Child
     (Container : in out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Delete_Children
     (Container : in out Tree;
      Parent    : Cursor);

   procedure Copy_Subtree
     (Target   : in out Tree;
      Parent   : Cursor;
      Before   : Cursor;
      Source   : Cursor);

   procedure Splice_Subtree
     (Target   : in out Tree;
      Parent   : Cursor;
      Before   : Cursor;
      Source   : in out Tree;
      Position : in out Cursor);

   procedure Splice_Subtree
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      Position  : Cursor);

   procedure Splice_Children
     (Target          : in out Tree;
      Target_Parent   : Cursor;
      Before          : Cursor;
      Source          : in out Tree;
      Source_Parent   : Cursor);

   procedure Splice_Children
     (Container       : in out Tree;
      Target_Parent   : Cursor;
      Before          : Cursor;
      Source_Parent   : Cursor);

   function Parent (Position : Cursor) return Cursor;

   function First_Child (Parent : Cursor) return Cursor;

   function First_Child_Element (Parent : Cursor) return Element_Type;

   function Last_Child (Parent : Cursor) return Cursor;

   function Last_Child_Element (Parent : Cursor) return Element_Type;

   function Next_Sibling (Position : Cursor) return Cursor;

   function Previous_Sibling (Position : Cursor) return Cursor;

   procedure Next_Sibling (Position : in out Cursor);

   procedure Previous_Sibling (Position : in out Cursor);

   --  This version of the AI:
   --   10-06-02  AI05-0136-1/07
   --  declares Iterate_Children this way:
   --
   --  procedure Iterate_Children
   --    (Container : Tree;
   --     Parent    : Cursor;
   --     Process   : not null access procedure (Position : Cursor));
   --
   --  It seems that the Container parameter is there by mistake, but we need
   --  an official ruling from the ARG. ???

   procedure Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor));

private

   use Ada.Containers.Helpers;
   package Implementation is new Generic_Implementation;
   use Implementation;

   type Tree_Node_Type;
   type Tree_Node_Access is access all Tree_Node_Type;

   type Children_Type is record
      First : Tree_Node_Access;
      Last  : Tree_Node_Access;
   end record;

   type Element_Access is access all Element_Type;

   type Tree_Node_Type is record
      Parent   : Tree_Node_Access;
      Prev     : Tree_Node_Access;
      Next     : Tree_Node_Access;
      Children : Children_Type;
      Element  : Element_Access;
   end record;

   use Ada.Finalization;

   --  The Count component of type Tree represents the number of nodes that
   --  have been (dynamically) allocated. It does not include the root node
   --  itself. As implementors, we decide to cache this value, so that the
   --  selector function Node_Count can execute in O(1) time, in order to be
   --  consistent with the behavior of the Length selector function for other
   --  standard container library units. This does mean, however, that the
   --  two-container forms for Splice_XXX (that move subtrees across tree
   --  containers) will execute in O(n) time, because we must count the number
   --  of nodes in the subtree(s) that get moved. (We resolve the tension
   --  between Node_Count and Splice_XXX in favor of Node_Count, under the
   --  assumption that Node_Count is the more common operation).

   type Tree is new Controlled with record
      Root  : aliased Tree_Node_Type;
      TC    : aliased Tamper_Counts;
      Count : Count_Type := 0;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Tree);

   overriding procedure Adjust (Container : in out Tree);

   overriding procedure Finalize (Container : in out Tree) renames Clear;

   use Ada.Streams;

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Tree);

   for Tree'Write use Write;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Tree);

   for Tree'Read use Read;

   type Tree_Access is access all Tree;
   for Tree_Access'Storage_Size use 0;

   type Cursor is record
      Container : Tree_Access;
      Node      : Tree_Node_Access;
   end record;

   procedure Write
     (Stream   : not null access Root_Stream_Type'Class;
      Position : Cursor);

   for Cursor'Write use Write;

   procedure Read
     (Stream   : not null access Root_Stream_Type'Class;
      Position : out Cursor);

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
   --  Pseudo_Reference and Get_Element_Access functions. See Exp_Ch5 for
   --  details.

   function Pseudo_Reference
     (Container : aliased Tree'Class) return Reference_Control_Type;
   pragma Inline (Pseudo_Reference);
   --  Creates an object of type Reference_Control_Type pointing to the
   --  container, and increments the Lock. Finalization of this object will
   --  decrement the Lock.

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access;
   --  Returns a pointer to the element designated by Position.

   Empty_Tree : constant Tree := (Controlled with others => <>);

   No_Element : constant Cursor := (others => <>);

end Ada.Containers.Indefinite_Multiway_Trees;
