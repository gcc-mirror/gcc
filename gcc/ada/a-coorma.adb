------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       ADA.CONTAINERS.ORDERED_MAPS                        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

with Ada.Containers.Red_Black_Trees.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Keys);

with System;  use type System.Address;

package body Ada.Containers.Ordered_Maps is

   use Red_Black_Trees;

   type Node_Type is limited record
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Red_Black_Trees.Color_Type := Red;
      Key     : Key_Type;
      Element : Element_Type;
   end record;

   -----------------------------
   -- Node Access Subprograms --
   -----------------------------

   --  These subprograms provide a functional interface to access fields
   --  of a node, and a procedural interface for modifying these values.

   function Color (Node : Node_Access) return Color_Type;
   pragma Inline (Color);

   function Left (Node : Node_Access) return Node_Access;
   pragma Inline (Left);

   function Parent (Node : Node_Access) return Node_Access;
   pragma Inline (Parent);

   function Right (Node : Node_Access) return Node_Access;
   pragma Inline (Right);

   procedure Set_Parent (Node : Node_Access; Parent : Node_Access);
   pragma Inline (Set_Parent);

   procedure Set_Left (Node : Node_Access; Left : Node_Access);
   pragma Inline (Set_Left);

   procedure Set_Right (Node : Node_Access; Right : Node_Access);
   pragma Inline (Set_Right);

   procedure Set_Color (Node : Node_Access; Color : Color_Type);
   pragma Inline (Set_Color);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Copy_Node (Source : Node_Access) return Node_Access;
   pragma Inline (Copy_Node);

   function Copy_Tree (Source_Root : Node_Access) return Node_Access;

   procedure Delete_Tree (X : in out Node_Access);

   function Is_Equal_Node_Node (L, R : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Node_Node);

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean;
   pragma Inline (Is_Greater_Key_Node);

   function Is_Less_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean;
   pragma Inline (Is_Less_Key_Node);

   --------------------------
   -- Local Instantiations --
   --------------------------

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   package Tree_Operations is
     new Red_Black_Trees.Generic_Operations
       (Tree_Types => Tree_Types,
        Null_Node  => Node_Access'(null));

   use Tree_Operations;

   package Key_Ops is
     new Red_Black_Trees.Generic_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Key_Type,
        Is_Less_Key_Node    => Is_Less_Key_Node,
        Is_Greater_Key_Node => Is_Greater_Key_Node);

   function Is_Equal is
     new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left.Node.Key < Right.Node.Key;
   end "<";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Left.Node.Key < Right;
   end "<";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Left < Right.Node.Key;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.Tree, Right.Tree);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      return Right.Node.Key < Left.Node.Key;
   end ">";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Right < Left.Node.Key;
   end ">";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Right.Node.Key < Left;
   end ">";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Map) is
      Tree : Tree_Type renames Container.Tree;

      N : constant Count_Type := Tree.Length;
      X : constant Node_Access := Tree.Root;

   begin
      if N = 0 then
         pragma Assert (X = null);
         return;
      end if;

      Tree := (Length => 0, others => null);

      Tree.Root := Copy_Tree (X);
      Tree.First := Min (Tree.Root);
      Tree.Last := Max (Tree.Root);
      Tree.Length := N;
   end Adjust;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Ceiling (Container.Tree, Key);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
      Tree : Tree_Type renames Container.Tree;
      Root : Node_Access := Tree.Root;
   begin
      Tree := (Length => 0, others => null);
      Delete_Tree (Root);
   end Clear;

   -----------
   -- Color --
   -----------

   function Color (Node : Node_Access) return Color_Type is
   begin
      return Node.Color;
   end Color;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Source : Node_Access) return Node_Access is
      Target : constant Node_Access :=
                 new Node_Type'(Parent  => null,
                                Left    => null,
                                Right   => null,
                                Color   => Source.Color,
                                Key     => Source.Key,
                                Element => Source.Element);
   begin
      return Target;
   end Copy_Node;

   ---------------
   -- Copy_Tree --
   ---------------

   function Copy_Tree (Source_Root : Node_Access) return Node_Access is
      Target_Root : Node_Access := Copy_Node (Source_Root);
      P, X : Node_Access;

   begin
      if Source_Root.Right /= null then
         Target_Root.Right := Copy_Tree (Source_Root.Right);
         Target_Root.Right.Parent := Target_Root;
      end if;

      P := Target_Root;
      X := Source_Root.Left;

      while X /= null loop
         declare
            Y : Node_Access := Copy_Node (X);

         begin
            P.Left := Y;
            Y.Parent := P;

            if X.Right /= null then
               Y.Right := Copy_Tree (X.Right);
               Y.Right.Parent := Y;
            end if;

            P := Y;
            X := X.Left;
         end;
      end loop;

      return Target_Root;

   exception
      when others =>
         Delete_Tree (Target_Root);
         raise;
   end Copy_Tree;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if Position = No_Element then
         return;
      end if;

      if Position.Container /= Map_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      Delete_Node_Sans_Free (Container.Tree, Position.Node);
      Free (Position.Node);

      Position.Container := null;
   end Delete;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if X = null then
         raise Constraint_Error;
      end if;

      Delete_Node_Sans_Free (Container.Tree, X);
      Free (X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Map) is
      Position : Cursor := First (Container);
   begin
      Delete (Container, Position);
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Map) is
      Position : Cursor := Last (Container);
   begin
      Delete (Container, Position);
   end Delete_Last;


   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (X : in out Node_Access) is
      Y : Node_Access;
   begin
      while X /= null loop
         Y := X.Right;
         Delete_Tree (Y);
         Y := X.Left;
         Free (X);
         X := Y;
      end loop;
   end Delete_Tree;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);
   begin
      return Node.Element;
   end Element;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      X : Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if X /= null then
         Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end if;
   end Exclude;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
   begin
      if Container.Tree.First = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Tree.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Map) return Element_Type is
   begin
      return Container.Tree.First.Element;
   end First_Element;

   ---------------
   -- First_Key --
   ---------------

   function First_Key (Container : Map) return Key_Type is
   begin
      return Container.Tree.First.Key;
   end First_Key;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Floor (Container.Tree, Key);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Floor;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   -------------
   -- Include --
   -------------

   procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, Key, New_Item, Position, Inserted);

      if not Inserted then
         Position.Node.Key := Key;
         Position.Node.Element := New_Item;
      end if;
   end Include;

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Key_Ops.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Key_Ops.Generic_Conditional_Insert (Insert_Post);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         Node : constant Node_Access :=
                  new Node_Type'(Parent  => null,
                                 Left    => null,
                                 Right   => null,
                                 Color   => Red,
                                 Key     => Key,
                                 Element => New_Item);
      begin
         return Node;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container.Tree,
         Key,
         Position.Node,
         Inserted);

      Position.Container := Container'Unchecked_Access;
   end Insert;

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, Key, New_Item, Position, Inserted);

      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Key_Ops.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Key_Ops.Generic_Conditional_Insert (Insert_Post);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         Node : Node_Access := new Node_Type;

      begin
         begin
            Node.Key := Key;
         exception
            when others =>
               Free (Node);
               raise;
         end;

         return Node;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container.Tree,
         Key,
         Position.Node,
         Inserted);

      Position.Container := Container'Unchecked_Access;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Tree.Length = 0;
   end Is_Empty;

   ------------------------
   -- Is_Equal_Node_Node --
   ------------------------

   function Is_Equal_Node_Node
     (L, R : Node_Access) return Boolean is
   begin
      return L.Element = R.Element;
   end Is_Equal_Node_Node;

   -------------------------
   -- Is_Greater_Key_Node --
   -------------------------

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean
   is
   begin
      --  k > node same as node < k

      return Right.Key < Left;
   end Is_Greater_Key_Node;

   ----------------------
   -- Is_Less_Key_Node --
   ----------------------

   function Is_Less_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean
   is
   begin
      return Left < Right.Key;
   end Is_Less_Key_Node;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Node_Access);
      pragma Inline (Process_Node);

      procedure Local_Iterate is
         new Tree_Operations.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

   --  Start of processing for Iterate

   begin
      Local_Iterate (Container.Tree);
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Node.Key;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
   begin
      if Container.Tree.Last = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Tree.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Map) return Element_Type is
   begin
      return Container.Tree.Last.Element;
   end Last_Element;

   --------------
   -- Last_Key --
   --------------

   function Last_Key (Container : Map) return Key_Type is
   begin
      return Container.Tree.Last.Key;
   end Last_Key;

   ----------
   -- Left --
   ----------

   function Left (Node : Node_Access) return Node_Access is
   begin
      return Node.Left;
   end Left;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Tree.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Move (Target => Target.Tree, Source => Source.Tree);
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      declare
         Node : constant Node_Access :=
                  Tree_Operations.Next (Position.Node);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Next;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Node_Access) return Node_Access is
   begin
      return Node.Parent;
   end Parent;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      declare
         Node : constant Node_Access :=
                  Tree_Operations.Previous (Position.Node);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      Process (Position.Node.Key, Position.Node.Element);
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container : out Map)
   is
      N : Count_Type'Base;

      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Local_Read is new Tree_Operations.Generic_Read (New_Node);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         Node : Node_Access := new Node_Type;

      begin
         begin
            Key_Type'Read (Stream, Node.Key);
            Element_Type'Read (Stream, Node.Element);
         exception
            when others =>
               Free (Node);
               raise;
         end;

         return Node;
      end New_Node;

   --  Start of processing for Read

   begin
      Clear (Container);
      Count_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      Local_Read (Container.Tree, N);
   end Read;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Node = null then
         raise Constraint_Error;
      end if;

      Node.Key := Key;
      Node.Element := New_Item;
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element (Position : Cursor; By : Element_Type) is
   begin
      Position.Node.Element := By;
   end Replace_Element;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Node_Access);
      pragma Inline (Process_Node);

      procedure Local_Reverse_Iterate is
         new Tree_Operations.Generic_Reverse_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

      --  Start of processing for Reverse_Iterate

   begin
      Local_Reverse_Iterate (Container.Tree);
   end Reverse_Iterate;

   -----------
   -- Right --
   -----------

   function Right (Node : Node_Access) return Node_Access is
   begin
      return Node.Right;
   end Right;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type)
   is
   begin
      Node.Color := Color;
   end Set_Color;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (Node : Node_Access; Left : Node_Access) is
   begin
      Node.Left := Left;
   end Set_Left;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Node : Node_Access; Parent : Node_Access) is
   begin
      Node.Parent := Parent;
   end Set_Parent;


   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (Node : Node_Access; Right : Node_Access) is
   begin
      Node.Right := Right;
   end Set_Right;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in out Element_Type))
   is
   begin
      Process (Position.Node.Key, Position.Node.Element);
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Map)
   is
      procedure Process (Node : Node_Access);
      pragma Inline (Process);

      procedure Iterate is new Tree_Operations.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Node : Node_Access) is
      begin
         Key_Type'Write (Stream, Node.Key);
         Element_Type'Write (Stream, Node.Element);
      end Process;

   --  Start of processing for Write

   begin
      Count_Type'Base'Write (Stream, Container.Tree.Length);
      Iterate (Container.Tree);
   end Write;

end Ada.Containers.Ordered_Maps;
