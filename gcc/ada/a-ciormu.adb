------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               ADA.CONTAINERS.INDEFINITE_ORDERED_MULTISETS                --
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

with Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Set_Operations);

with System;  use type System.Address;

package body Ada.Containers.Indefinite_Ordered_Multisets is

   use Red_Black_Trees;

   type Element_Access is access Element_Type;

   type Node_Type is limited record
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Red_Black_Trees.Color_Type := Red;
      Element : Element_Access;
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

   procedure Free (X : in out Node_Access);

   procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access);

   function Is_Equal_Node_Node (L, R : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Node_Node);

   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean;
   pragma Inline (Is_Greater_Element_Node);

   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean;
   pragma Inline (Is_Less_Element_Node);

   function Is_Less_Node_Node (L, R : Node_Access) return Boolean;
   pragma Inline (Is_Less_Node_Node);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package Tree_Operations is
     new Red_Black_Trees.Generic_Operations
       (Tree_Types => Tree_Types,
        Null_Node  => Node_Access'(null));

   use Tree_Operations;

   procedure Free_Element is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   function Is_Equal is
     new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

   package Set_Ops is
     new Generic_Set_Operations
       (Tree_Operations  => Tree_Operations,
        Insert_With_Hint => Insert_With_Hint,
        Copy_Tree        => Copy_Tree,
        Delete_Tree      => Delete_Tree,
        Is_Less          => Is_Less_Node_Node,
        Free             => Free);

   package Element_Keys is
     new Red_Black_Trees.Generic_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Element_Type,
        Is_Less_Key_Node    => Is_Less_Element_Node,
        Is_Greater_Key_Node => Is_Greater_Element_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left.Node.Element.all < Right.Node.Element.all;
   end "<";

   function "<" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      return Left.Node.Element.all < Right;
   end "<";

   function "<" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      return Left < Right.Node.Element.all;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.Tree, Right.Tree);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      return Right < Left.Node.Element.all;
   end ">";

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      --  L > R same as R < L

      return Right.Node.Element.all < Left.Node.Element.all;
   end ">";

   function ">" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      return Right.Node.Element.all < Left;
   end ">";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Set) is
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

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access :=
               Element_Keys.Ceiling (Container.Tree, Item);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Set) is
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

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Source : Node_Access) return Node_Access is
      X : Element_Access := new Element_Type'(Source.Element.all);

   begin
      return new Node_Type'(Parent  => null,
                            Left    => null,
                            Right   => null,
                            Color   => Source.Color,
                            Element => X);

   exception
      when others =>
         Free_Element (X);
         raise;
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

   procedure Delete (Container : in out Set; Item : Element_Type) is
      Tree : Tree_Type renames Container.Tree;
      Node : Node_Access := Element_Keys.Ceiling (Tree, Item);
      Done : constant Node_Access := Element_Keys.Upper_Bound (Tree, Item);
      X    : Node_Access;

   begin
      if Node = Done then
         raise Constraint_Error;
      end if;

      loop
         X := Node;
         Node := Tree_Operations.Next (Node);
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Free (X);

         exit when Node = Done;
      end loop;
   end Delete;

   procedure Delete (Container : in out Set; Position : in out Cursor) is
   begin
      if Position = No_Element then
         return;
      end if;

      if Position.Container /= Set_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      Delete_Node_Sans_Free (Container.Tree, Position.Node);
      Free (Position.Node);

      Position.Container := null;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Set) is
      Tree : Tree_Type renames Container.Tree;
      X    : Node_Access := Tree.First;

   begin
      if X = null then
         return;
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Tree, X);
      Free (X);
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Set) is
      Tree : Tree_Type renames Container.Tree;
      X    : Node_Access := Tree.Last;

   begin
      if X = null then
         return;
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Tree, X);
      Free (X);
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

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      Set_Ops.Difference (Target.Tree, Source.Tree);
   end Difference;

   function Difference (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      declare
         Tree : constant Tree_Type :=
                  Set_Ops.Difference (Left.Tree, Right.Tree);
      begin
         return (Controlled with Tree);
      end;
   end Difference;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element.all;
   end Element;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      Tree : Tree_Type renames Container.Tree;
      Node : Node_Access := Element_Keys.Ceiling (Tree, Item);
      Done : constant Node_Access := Element_Keys.Upper_Bound (Tree, Item);
      X    : Node_Access;
   begin
      while Node /= Done loop
         X := Node;
         Node := Tree_Operations.Next (Node);
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Free (X);
      end loop;
   end Exclude;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access :=
               Element_Keys.Find (Container.Tree, Item);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      if Container.Tree.First = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Tree.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Set) return Element_Type is
   begin
      return Container.Tree.First.Element.all;
   end First_Element;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access :=
               Element_Keys.Floor (Container.Tree, Item);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Floor;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   begin
      if X /= null then
         Free_Element (X.Element);
         Deallocate (X);
      end if;
   end Free;

   ------------------
   -- Generic_Keys --
   ------------------

   package body Generic_Keys is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean;
      pragma Inline (Is_Less_Key_Node);

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean;
      pragma Inline (Is_Greater_Key_Node);

      --------------------------
      -- Local Instantiations --
      --------------------------

      package Key_Keys is
        new Red_Black_Trees.Generic_Keys
          (Tree_Operations     => Tree_Operations,
           Key_Type            => Key_Type,
           Is_Less_Key_Node    => Is_Less_Key_Node,
           Is_Greater_Key_Node => Is_Greater_Key_Node);

      ---------
      -- "<" --
      ---------

      function "<" (Left : Key_Type; Right : Cursor) return Boolean is
      begin
         return Left < Right.Node.Element.all;
      end "<";

      function "<" (Left : Cursor; Right : Key_Type) return Boolean is
      begin
         return Right > Left.Node.Element.all;
      end "<";

      ---------
      -- ">" --
      ---------

      function ">" (Left : Key_Type; Right : Cursor) return Boolean is
      begin
         return Left > Right.Node.Element.all;
      end ">";

      function ">" (Left : Cursor; Right : Key_Type) return Boolean is
      begin
         return Right < Left.Node.Element.all;
      end ">";

      -------------
      -- Ceiling --
      -------------

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access :=
                  Key_Keys.Ceiling (Container.Tree, Key);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Container'Unchecked_Access, Node);
      end Ceiling;

      ----------------------------
      -- Checked_Update_Element --
      ----------------------------

      procedure Checked_Update_Element
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access procedure (Element : in out Element_Type))
      is
      begin
         if Position.Container = null then
            raise Constraint_Error;
         end if;

         if Position.Container /= Set_Access'(Container'Unchecked_Access) then
            raise Program_Error;
         end if;

         declare
            Old_Key : Key_Type renames Key (Position.Node.Element.all);

         begin
            Process (Position.Node.Element.all);

            if Old_Key < Position.Node.Element.all
              or else Old_Key > Position.Node.Element.all
            then
               null;
            else
               return;
            end if;
         end;

         Delete_Node_Sans_Free (Container.Tree, Position.Node);

         Do_Insert : declare
            Result : Node_Access;

            function New_Node return Node_Access;
            pragma Inline (New_Node);

            procedure Insert_Post is
              new Key_Keys.Generic_Insert_Post (New_Node);

            procedure Insert is
              new Key_Keys.Generic_Unconditional_Insert (Insert_Post);

            --------------
            -- New_Node --
            --------------

            function New_Node return Node_Access is
            begin
               return Position.Node;
            end New_Node;

         --  Start of processing for Do_Insert

         begin
            Insert
              (Tree    => Container.Tree,
               Key     => Key (Position.Node.Element.all),
               Node    => Result);

            pragma Assert (Result = Position.Node);
         end Do_Insert;
      end Checked_Update_Element;

      --------------
      -- Contains --
      --------------

      function Contains (Container : Set; Key : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= No_Element;
      end Contains;

      ------------
      -- Delete --
      ------------

      procedure Delete (Container : in out Set; Key : Key_Type) is
         Tree : Tree_Type renames Container.Tree;
         Node : Node_Access := Key_Keys.Ceiling (Tree, Key);
         Done : constant Node_Access := Key_Keys.Upper_Bound (Tree, Key);
         X    : Node_Access;

      begin
         if Node = Done then
            raise Constraint_Error;
         end if;

         loop
            X := Node;
            Node := Tree_Operations.Next (Node);
            Tree_Operations.Delete_Node_Sans_Free (Tree, X);
            Free (X);

            exit when Node = Done;
         end loop;
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Container : Set; Key : Key_Type) return Element_Type is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);
      begin
         return Node.Element.all;
      end Element;

      -------------
      -- Exclude --
      -------------

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         Tree : Tree_Type renames Container.Tree;
         Node : Node_Access := Key_Keys.Ceiling (Tree, Key);
         Done : constant Node_Access := Key_Keys.Upper_Bound (Tree, Key);
         X    : Node_Access;

      begin
         while Node /= Done loop
            X := Node;
            Node := Tree_Operations.Next (Node);
            Tree_Operations.Delete_Node_Sans_Free (Tree, X);
            Free (X);
         end loop;
      end Exclude;

      ----------
      -- Find --
      ----------

      function Find (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Container'Unchecked_Access, Node);
      end Find;

      -----------
      -- Floor --
      -----------

      function Floor (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access := Key_Keys.Floor (Container.Tree, Key);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Container'Unchecked_Access, Node);
      end Floor;

      -------------------------
      -- Is_Greater_Key_Node --
      -------------------------

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is
      begin
         return Left > Right.Element.all;
      end Is_Greater_Key_Node;

      ----------------------
      -- Is_Less_Key_Node --
      ----------------------

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is
      begin
         return Left < Right.Element.all;
      end Is_Less_Key_Node;

      -------------
      -- Iterate --
      -------------

      procedure Iterate
        (Container : Set;
         Key       : Key_Type;
         Process   : not null access procedure (Position : Cursor))
      is
         procedure Process_Node (Node : Node_Access);
         pragma Inline (Process_Node);

         procedure Local_Iterate is
           new Key_Keys.Generic_Iteration (Process_Node);

         ------------------
         -- Process_Node --
         ------------------

         procedure Process_Node (Node : Node_Access) is
         begin
            Process (Cursor'(Container'Unchecked_Access, Node));
         end Process_Node;

      --  Start of processing for Iterate

      begin
         Local_Iterate (Container.Tree, Key);
      end Iterate;

      ---------
      -- Key --
      ---------

      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Node.Element.all);
      end Key;

      -------------
      -- Replace --
      -------------

      --  In post-madision api: ???

--     procedure Replace
--       (Container : in out Set;
--        Key       : Key_Type;
--        New_Item  : Element_Type)
--     is
--           Node : Node_Access := Key_Keys.Find (Container.Tree, Key);

--        begin
--           if Node = null then
--              raise Constraint_Error;
--           end if;

--           Replace_Node (Container, Node, New_Item);
--        end Replace;

      ---------------------
      -- Reverse_Iterate --
      ---------------------

      procedure Reverse_Iterate
        (Container : Set;
         Key       : Key_Type;
         Process   : not null access procedure (Position : Cursor))
      is
         procedure Process_Node (Node : Node_Access);
         pragma Inline (Process_Node);

         -------------
         -- Iterate --
         -------------

         procedure Local_Reverse_Iterate is
            new Key_Keys.Generic_Reverse_Iteration (Process_Node);

         ------------------
         -- Process_Node --
         ------------------

         procedure Process_Node (Node : Node_Access) is
         begin
            Process (Cursor'(Container'Unchecked_Access, Node));
         end Process_Node;

      --  Start of processing for Reverse_Iterate

      begin
         Local_Reverse_Iterate (Container.Tree, Key);
      end Reverse_Iterate;

   end Generic_Keys;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert (Container : in out Set; New_Item : Element_Type) is
      Position : Cursor;
   begin
      Insert (Container, New_Item, Position);
   end Insert;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor)
   is
      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Unconditional_Insert_Sans_Hint is
        new Element_Keys.Generic_Unconditional_Insert (Insert_Post);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         X : Element_Access := new Element_Type'(New_Item);

      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red,
                               Element => X);

      exception
         when others =>
            Free_Element (X);
            raise;
      end New_Node;

   --  Start of processing for Insert

   begin
      Unconditional_Insert_Sans_Hint
        (Container.Tree,
         New_Item,
         Position.Node);

      Position.Container := Container'Unchecked_Access;
   end Insert;

   ----------------------
   -- Insert_With_Hint --
   ----------------------

   procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access)
   is
      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Element_Keys.Generic_Unconditional_Insert (Insert_Post);

      procedure Local_Insert_With_Hint is
        new Element_Keys.Generic_Unconditional_Insert_With_Hint
          (Insert_Post,
           Insert_Sans_Hint);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         X : Element_Access := new Element_Type'(Src_Node.Element.all);

      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red,
                               Element => X);

      exception
         when others =>
            Free_Element (X);
            raise;
      end New_Node;

   --  Start of processing for Insert_With_Hint

   begin
      Local_Insert_With_Hint
        (Dst_Tree,
         Dst_Hint,
         Src_Node.Element.all,
         Dst_Node);
   end Insert_With_Hint;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Set_Ops.Intersection (Target.Tree, Source.Tree);
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Left;
      end if;

      declare
         Tree : constant Tree_Type :=
                  Set_Ops.Intersection (Left.Tree, Right.Tree);
      begin
         return (Controlled with Tree);
      end;
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Tree.Length = 0;
   end Is_Empty;

   ------------------------
   -- Is_Equal_Node_Node --
   ------------------------

   function Is_Equal_Node_Node (L, R : Node_Access) return Boolean is
   begin
      return L.Element.all = R.Element.all;
   end Is_Equal_Node_Node;

   -----------------------------
   -- Is_Greater_Element_Node --
   -----------------------------

   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean
   is
   begin
      --  e > node same as node < e

      return Right.Element.all < Left;
   end Is_Greater_Element_Node;

   --------------------------
   -- Is_Less_Element_Node --
   --------------------------

   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean
   is
   begin
      return Left < Right.Element.all;
   end Is_Less_Element_Node;

   -----------------------
   -- Is_Less_Node_Node --
   -----------------------

   function Is_Less_Node_Node (L, R : Node_Access) return Boolean is
   begin
      return L.Element.all < R.Element.all;
   end Is_Less_Node_Node;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      return Set_Ops.Is_Subset (Subset => Subset.Tree, Of_Set => Of_Set.Tree);
   end Is_Subset;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Item      : Element_Type;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Node_Access);
      pragma Inline (Process_Node);

      procedure Local_Iterate is
        new Element_Keys.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

   --  Start of processing for Iterate

   begin
      Local_Iterate (Container.Tree, Item);
   end Iterate;

   procedure Iterate
     (Container : Set;
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

   ----------
   -- Last --
   ----------

   function Last (Container : Set) return Cursor is
   begin
      if Container.Tree.Last = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Tree.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Set) return Element_Type is
   begin
      return Container.Tree.Last.Element.all;
   end Last_Element;

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

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Tree.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Move (Target => Target.Tree, Source => Source.Tree);
   end Move;

   ----------
   -- Next --
   ----------

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

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
      if Left'Address = Right'Address then
         return Left.Tree.Length /= 0;
      end if;

      return Set_Ops.Overlap (Left.Tree, Right.Tree);
   end Overlap;

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

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      Process (Position.Node.Element.all);
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container : out Set)
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
            Node.Element := new Element_Type'(Element_Type'Input (Stream));
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

   --  NOTE: from post-madison api???

--   procedure Replace
--     (Container : in out Set;
--      Position  : Cursor;
--      By        : Element_Type)
--   is
--   begin
--      if Position.Container = null then
--         raise Constraint_Error;
--      end if;

--      if Position.Container /= Set_Access'(Container'Unchecked_Access) then
--         raise Program_Error;
--      end if;

--      Replace_Node (Container, Position.Node, By);
--   end Replace;

   ------------------
   -- Replace_Node --
   ------------------

   --  NOTE: from post-madison api???

--   procedure Replace_Node
--     (Container : in out Set;
--      Position  : Node_Access;
--      By        : Element_Type);
--   is
--      Tree : Tree_Type renames Container.Tree;
--      Node : Node_Access := Position;

--   begin
--      if By < Node.Element
--        or else Node.Element < By
--      then
--         null;

--      else
--         begin
--            Node.Element := By;

--         exception
--            when others =>
--               Tree_Operations.Delete_Node_Sans_Free (Tree, Node);
--               Free (Node);
--               raise;
--         end;

--         return;
--      end if;

--      Tree_Operations.Delete_Node_Sans_Free (Tree, Node);

--      begin
--         Node.Element := By;

--      exception
--         when others =>
--            Free (Node);
--            raise;
--      end;

--      declare
--         Result  : Node_Access;
--         Success : Boolean;

--         function New_Node return Node_Access;
--         pragma Inline (New_Node);

--         procedure Insert_Post is
--           new Element_Keys.Generic_Insert_Post (New_Node);

--         procedure Insert is
--           new Element_Keys.Generic_Conditional_Insert (Insert_Post);

--         --------------
--         -- New_Node --
--         --------------
--
--         function New_Node return Node_Access is
--         begin
--            return Node;
--         end New_Node;

--      --  Start of processing for Replace_Node

--      begin
--         Insert
--           (Tree    => Tree,
--            Key     => Node.Element,
--            Node    => Result,
--            Success => Success);

--         if not Success then
--            Free (Node);
--            raise Program_Error;
--         end if;

--         pragma Assert (Result = Node);
--      end;
--   end Replace_Node;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Set;
      Item      : Element_Type;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Node_Access);
      pragma Inline (Process_Node);

      procedure Local_Reverse_Iterate is
        new Element_Keys.Generic_Reverse_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

   --  Start of processing for Reverse_Iterate

   begin
      Local_Reverse_Iterate (Container.Tree, Item);
   end Reverse_Iterate;

   procedure Reverse_Iterate
     (Container : Set;
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

   procedure Set_Color (Node : Node_Access; Color : Color_Type) is
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

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      Set_Ops.Symmetric_Difference (Target.Tree, Source.Tree);
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      declare
         Tree : constant Tree_Type :=
                  Set_Ops.Symmetric_Difference (Left.Tree, Right.Tree);
      begin
         return (Controlled with Tree);
      end;
   end Symmetric_Difference;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Set_Ops.Union (Target.Tree, Source.Tree);
   end Union;

   function Union (Left, Right : Set) return Set is begin
      if Left'Address = Right'Address then
         return Left;
      end if;

      declare
         Tree : constant Tree_Type := Set_Ops.Union (Left.Tree, Right.Tree);
      begin
         return (Controlled with Tree);
      end;
   end Union;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Set)
   is
      procedure Process (Node : Node_Access);
      pragma Inline (Process);

      procedure Iterate is new Tree_Operations.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Node : Node_Access) is
      begin
         Element_Type'Output (Stream, Node.Element.all);
      end Process;

   --  Start of processing for Write

   begin
      Count_Type'Base'Write (Stream, Container.Tree.Length);
      Iterate (Container.Tree);
   end Write;

end Ada.Containers.Indefinite_Ordered_Multisets;
