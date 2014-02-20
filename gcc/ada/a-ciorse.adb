------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.INDEFINITE_ORDERED_SETS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2013, Free Software Foundation, Inc.         --
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

with Ada.Containers.Red_Black_Trees.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Keys);

with Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Set_Operations);

with Ada.Unchecked_Deallocation;

with System; use type System.Address;

package body Ada.Containers.Indefinite_Ordered_Sets is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Color (Node : Node_Access) return Color_Type;
   pragma Inline (Color);

   function Copy_Node (Source : Node_Access) return Node_Access;
   pragma Inline (Copy_Node);

   procedure Free (X : in out Node_Access);

   procedure Insert_Sans_Hint
     (Tree     : in out Tree_Type;
      New_Item : Element_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);

   procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access);

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

   function Left (Node : Node_Access) return Node_Access;
   pragma Inline (Left);

   function Parent (Node : Node_Access) return Node_Access;
   pragma Inline (Parent);

   procedure Replace_Element
     (Tree : in out Tree_Type;
      Node : Node_Access;
      Item : Element_Type);

   function Right (Node : Node_Access) return Node_Access;
   pragma Inline (Right);

   procedure Set_Color (Node : Node_Access; Color : Color_Type);
   pragma Inline (Set_Color);

   procedure Set_Left (Node : Node_Access; Left : Node_Access);
   pragma Inline (Set_Left);

   procedure Set_Parent (Node   : Node_Access; Parent : Node_Access);
   pragma Inline (Set_Parent);

   procedure Set_Right (Node : Node_Access; Right : Node_Access);
   pragma Inline (Set_Right);

   --------------------------
   -- Local Instantiations --
   --------------------------

   procedure Free_Element is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   package Tree_Operations is
     new Red_Black_Trees.Generic_Operations (Tree_Types);

   procedure Delete_Tree is
     new Tree_Operations.Generic_Delete_Tree (Free);

   function Copy_Tree is
     new Tree_Operations.Generic_Copy_Tree (Copy_Node, Delete_Tree);

   use Tree_Operations;

   package Element_Keys is
     new Red_Black_Trees.Generic_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Element_Type,
        Is_Less_Key_Node    => Is_Less_Element_Node,
        Is_Greater_Key_Node => Is_Greater_Element_Node);

   package Set_Ops is
      new Generic_Set_Operations
        (Tree_Operations  => Tree_Operations,
         Insert_With_Hint => Insert_With_Hint,
         Copy_Tree        => Copy_Tree,
         Delete_Tree      => Delete_Tree,
         Is_Less          => Is_Less_Node_Node,
         Free             => Free);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Right.Node = null then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      if Left.Node.Element = null then
         raise Program_Error with "Left cursor is bad";
      end if;

      if Right.Node.Element = null then
         raise Program_Error with "Right cursor is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "bad Left cursor in ""<""");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "bad Right cursor in ""<""");

      return Left.Node.Element.all < Right.Node.Element.all;
   end "<";

   function "<" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Left.Node.Element = null then
         raise Program_Error with "Left cursor is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "bad Left cursor in ""<""");

      return Left.Node.Element.all < Right;
   end "<";

   function "<" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = null then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      if Right.Node.Element = null then
         raise Program_Error with "Right cursor is bad";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "bad Right cursor in ""<""");

      return Left < Right.Node.Element.all;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is

      function Is_Equal_Node_Node (L, R : Node_Access) return Boolean;
      pragma Inline (Is_Equal_Node_Node);

      function Is_Equal is
        new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

      ------------------------
      -- Is_Equal_Node_Node --
      ------------------------

      function Is_Equal_Node_Node (L, R : Node_Access) return Boolean is
      begin
         return L.Element.all = R.Element.all;
      end Is_Equal_Node_Node;

   --  Start of processing for "="

   begin
      return Is_Equal (Left.Tree, Right.Tree);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Right.Node = null then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      if Left.Node.Element = null then
         raise Program_Error with "Left cursor is bad";
      end if;

      if Right.Node.Element = null then
         raise Program_Error with "Right cursor is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "bad Left cursor in "">""");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "bad Right cursor in "">""");

      --  L > R same as R < L

      return Right.Node.Element.all < Left.Node.Element.all;
   end ">";

   function ">" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Left.Node.Element = null then
         raise Program_Error with "Left cursor is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "bad Left cursor in "">""");

      return Right < Left.Node.Element.all;
   end ">";

   function ">" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = null then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      if Right.Node.Element = null then
         raise Program_Error with "Right cursor is bad";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "bad Right cursor in "">""");

      return Right.Node.Element.all < Left;
   end ">";

   ------------
   -- Adjust --
   ------------

   procedure Adjust is new Tree_Operations.Generic_Adjust (Copy_Tree);

   procedure Adjust (Container : in out Set) is
   begin
      Adjust (Container.Tree);
   end Adjust;

   procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            Tree : Tree_Type renames Control.Container.all.Tree;
            B : Natural renames Tree.Busy;
            L : Natural renames Tree.Lock;
         begin
            B := B + 1;
            L := L + 1;
         end;
      end if;
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Target.Clear;
      Target.Union (Source);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access :=
        Element_Keys.Ceiling (Container.Tree, Item);
   begin
      return (if Node = null then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear is
      new Tree_Operations.Generic_Clear (Delete_Tree);

   procedure Clear (Container : in out Set) is
   begin
      Clear (Container.Tree);
   end Clear;

   -----------
   -- Color --
   -----------

   function Color (Node : Node_Access) return Color_Type is
   begin
      return Node.Color;
   end Color;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Set;
      Position  : Cursor) return Constant_Reference_Type
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      pragma Assert
        (Vet (Container.Tree, Position.Node),
         "bad cursor in Constant_Reference");

      declare
         Tree : Tree_Type renames Position.Container.all.Tree;
         B : Natural renames Tree.Busy;
         L : Natural renames Tree.Lock;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => Position.Node.Element.all'Access,
            Control => (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Set) return Set is
   begin
      return Target : Set do
         Target.Assign (Source);
      end return;
   end Copy;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Source : Node_Access) return Node_Access is
      Element : Element_Access := new Element_Type'(Source.Element.all);

   begin
      return new Node_Type'(Parent  => null,
                            Left    => null,
                            Right   => null,
                            Color   => Source.Color,
                            Element => Element);

   exception
      when others =>
         Free_Element (Element);
         raise;
   end Copy_Node;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Set; Position : in out Cursor) is
   begin
      if Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "bad cursor in Delete");

      Tree_Operations.Delete_Node_Sans_Free (Container.Tree, Position.Node);
      Free (Position.Node);
      Position.Container := null;
   end Delete;

   procedure Delete (Container : in out Set; Item : Element_Type) is
      X : Node_Access := Element_Keys.Find (Container.Tree, Item);
   begin
      if X = null then
         raise Constraint_Error with "attempt to delete element not in set";
      else
         Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end if;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Set) is
      Tree : Tree_Type renames Container.Tree;
      X    : Node_Access := Tree.First;
   begin
      if X /= null then
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Free (X);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Set) is
      Tree : Tree_Type renames Container.Tree;
      X    : Node_Access := Tree.Last;
   begin
      if X /= null then
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Free (X);
      end if;
   end Delete_Last;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Set; Source : Set) is
   begin
      Set_Ops.Difference (Target.Tree, Source.Tree);
   end Difference;

   function Difference (Left, Right : Set) return Set is
      Tree : constant Tree_Type := Set_Ops.Difference (Left.Tree, Right.Tree);
   begin
      return Set'(Controlled with Tree);
   end Difference;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "bad cursor in Element");

      return Position.Node.Element.all;
   end Element;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      if Left < Right or else Right < Left then
         return False;
      else
         return True;
      end if;
   end Equivalent_Elements;

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is

      function Is_Equivalent_Node_Node (L, R : Node_Access) return Boolean;
      pragma Inline (Is_Equivalent_Node_Node);

      function Is_Equivalent is
         new Tree_Operations.Generic_Equal (Is_Equivalent_Node_Node);

      -----------------------------
      -- Is_Equivalent_Node_Node --
      -----------------------------

      function Is_Equivalent_Node_Node (L, R : Node_Access) return Boolean is
      begin
         if L.Element.all < R.Element.all then
            return False;
         elsif R.Element.all < L.Element.all then
            return False;
         else
            return True;
         end if;
      end Is_Equivalent_Node_Node;

   --  Start of processing for Equivalent_Sets

   begin
      return Is_Equivalent (Left.Tree, Right.Tree);
   end Equivalent_Sets;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      X : Node_Access := Element_Keys.Find (Container.Tree, Item);
   begin
      if X /= null then
         Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end if;
   end Exclude;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Container /= null then
         declare
            B : Natural renames Object.Container.all.Tree.Busy;
         begin
            B := B - 1;
         end;
      end if;
   end Finalize;

   procedure Finalize (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            Tree : Tree_Type renames Control.Container.all.Tree;
            B : Natural renames Tree.Busy;
            L : Natural renames Tree.Lock;
         begin
            B := B - 1;
            L := L - 1;
         end;

         Control.Container := null;
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access := Element_Keys.Find (Container.Tree, Item);
   begin
      if Node = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Node);
      end if;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      return
        (if Container.Tree.First = null then No_Element
         else Cursor'(Container'Unrestricted_Access, Container.Tree.First));
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Node component is null, this means the iterator object was
      --  constructed without a start expression, in which case the (forward)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  of items (corresponding to Container.First, for a forward iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is non-null, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (forward) partial iteration begins.

      if Object.Node = null then
         return Object.Container.First;
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Set) return Element_Type is
   begin
      if Container.Tree.First = null then
         raise Constraint_Error with "set is empty";
      else
         return Container.Tree.First.Element.all;
      end if;
   end First_Element;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Node_Access := Element_Keys.Floor (Container.Tree, Item);
   begin
      return (if Node = null then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Floor;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   begin
      if X = null then
         return;
      end if;

      X.Parent := X;
      X.Left := X;
      X.Right := X;

      begin
         Free_Element (X.Element);
      exception
         when others =>
            X.Element := null;
            Deallocate (X);
            raise;
      end;

      Deallocate (X);
   end Free;

   ------------------
   -- Generic_Keys --
   ------------------

   package body Generic_Keys is

      -----------------------
      -- Local Subprograms --
      -----------------------

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

      package Key_Keys is
        new Red_Black_Trees.Generic_Keys
          (Tree_Operations     => Tree_Operations,
           Key_Type            => Key_Type,
           Is_Less_Key_Node    => Is_Less_Key_Node,
           Is_Greater_Key_Node => Is_Greater_Key_Node);

      -------------
      -- Ceiling --
      -------------

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access := Key_Keys.Ceiling (Container.Tree, Key);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Ceiling;

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Container : aliased Set;
         Key       : Key_Type) return Constant_Reference_Type
      is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);

      begin
         if Node = null then
            raise Constraint_Error with "Key not in set";
         end if;

         if Node.Element = null then
            raise Program_Error with "Node has no element";
         end if;

         declare
            Tree : Tree_Type renames Container'Unrestricted_Access.all.Tree;
            B : Natural renames Tree.Busy;
            L : Natural renames Tree.Lock;
         begin
            return R : constant Constant_Reference_Type :=
              (Element => Node.Element.all'Access,
               Control => (Controlled with Container'Unrestricted_Access))
            do
               B := B + 1;
               L := L + 1;
            end return;
         end;
      end Constant_Reference;

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
         X : Node_Access := Key_Keys.Find (Container.Tree, Key);

      begin
         if X = null then
            raise Constraint_Error with "attempt to delete key not in set";
         end if;

         Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Container : Set; Key : Key_Type) return Element_Type is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);
      begin
         if Node = null then
            raise Constraint_Error with "key not in set";
         else
            return Node.Element.all;
         end if;
      end Element;

      ---------------------
      -- Equivalent_Keys --
      ---------------------

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
      begin
         if Left < Right or else Right < Left then
            return False;
         else
            return True;
         end if;
      end Equivalent_Keys;

      -------------
      -- Exclude --
      -------------

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         X : Node_Access := Key_Keys.Find (Container.Tree, Key);
      begin
         if X /= null then
            Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
            Free (X);
         end if;
      end Exclude;

      ----------
      -- Find --
      ----------

      function Find (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Find;

      -----------
      -- Floor --
      -----------

      function Floor (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Node_Access := Key_Keys.Floor (Container.Tree, Key);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Floor;

      -------------------------
      -- Is_Greater_Key_Node --
      -------------------------

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean
      is
      begin
         return Key (Right.Element.all) < Left;
      end Is_Greater_Key_Node;

      ----------------------
      -- Is_Less_Key_Node --
      ----------------------

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean
      is
      begin
         return Left < Key (Right.Element.all);
      end Is_Less_Key_Node;

      ---------
      -- Key --
      ---------

      function Key (Position : Cursor) return Key_Type is
      begin
         if Position.Node = null then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         if Position.Node.Element = null then
            raise Program_Error with
              "Position cursor is bad";
         end if;

         pragma Assert (Vet (Position.Container.Tree, Position.Node),
                        "bad cursor in Key");

         return Key (Position.Node.Element.all);
      end Key;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);

      begin
         if Node = null then
            raise Constraint_Error with
              "attempt to replace key not in set";
         end if;

         Replace_Element (Container.Tree, Node, New_Item);
      end Replace;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : not null access Root_Stream_Type'Class;
         Item   : out Reference_Type)
      is
      begin
         raise Program_Error with "attempt to stream reference";
      end Read;

      ------------------------------
      -- Reference_Preserving_Key --
      ------------------------------

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Position  : Cursor) return Reference_Type
      is
      begin
         if Position.Container = null then
            raise Constraint_Error with "Position cursor has no element";
         end if;

         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         if Position.Node.Element = null then
            raise Program_Error with "Node has no element";
         end if;

         pragma Assert
           (Vet (Container.Tree, Position.Node),
            "bad cursor in function Reference_Preserving_Key");

         --  Some form of finalization will be required in order to actually
         --  check that the key-part of the element designated by Position has
         --  not changed.  ???

         return (Element => Position.Node.Element.all'Access);
      end Reference_Preserving_Key;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Key       : Key_Type) return Reference_Type
      is
         Node : constant Node_Access := Key_Keys.Find (Container.Tree, Key);

      begin
         if Node = null then
            raise Constraint_Error with "Key not in set";
         end if;

         if Node.Element = null then
            raise Program_Error with "Node has no element";
         end if;

         --  Some form of finalization will be required in order to actually
         --  check that the key-part of the element designated by Key has not
         --  changed.  ???

         return (Element => Node.Element.all'Access);
      end Reference_Preserving_Key;

      -----------------------------------
      -- Update_Element_Preserving_Key --
      -----------------------------------

      procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access
                        procedure (Element : in out Element_Type))
      is
         Tree : Tree_Type renames Container.Tree;

      begin
         if Position.Node = null then
            raise Constraint_Error with "Position cursor equals No_Element";
         end if;

         if Position.Node.Element = null then
            raise Program_Error with "Position cursor is bad";
         end if;

         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with "Position cursor designates wrong set";
         end if;

         pragma Assert (Vet (Container.Tree, Position.Node),
                        "bad cursor in Update_Element_Preserving_Key");

         declare
            E : Element_Type renames Position.Node.Element.all;
            K : constant Key_Type := Key (E);

            B : Natural renames Tree.Busy;
            L : Natural renames Tree.Lock;

            Eq : Boolean;

         begin
            B := B + 1;
            L := L + 1;

            begin
               Process (E);
               Eq := Equivalent_Keys (K, Key (E));
            exception
               when others =>
                  L := L - 1;
                  B := B - 1;
                  raise;
            end;

            L := L - 1;
            B := B - 1;

            if Eq then
               return;
            end if;
         end;

         declare
            X : Node_Access := Position.Node;
         begin
            Tree_Operations.Delete_Node_Sans_Free (Tree, X);
            Free (X);
         end;

         raise Program_Error with "key was modified";
      end Update_Element_Preserving_Key;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Root_Stream_Type'Class;
         Item   : Reference_Type)
      is
      begin
         raise Program_Error with "attempt to stream reference";
      end Write;

   end Generic_Keys;

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

   procedure Include (Container : in out Set; New_Item  : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;

      X : Element_Access;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         if Container.Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (set is locked)";
         end if;

         declare
            --  The element allocator may need an accessibility check in the
            --  case the actual type is class-wide or has access discriminants
            --  (see RM 4.8(10.1) and AI12-0035).

            pragma Unsuppress (Accessibility_Check);

         begin
            X := Position.Node.Element;
            Position.Node.Element := new Element_Type'(New_Item);
            Free_Element (X);
         end;
      end if;
   end Include;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
   begin
      Insert_Sans_Hint
        (Container.Tree,
         New_Item,
         Position.Node,
         Inserted);

      Position.Container := Container'Unrestricted_Access;
   end Insert;

   procedure Insert (Container : in out Set; New_Item  : Element_Type) is
      Position : Cursor;
      pragma Unreferenced (Position);

      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         raise Constraint_Error with
           "attempt to insert element already in set";
      end if;
   end Insert;

   ----------------------
   -- Insert_Sans_Hint --
   ----------------------

   procedure Insert_Sans_Hint
     (Tree     : in out Tree_Type;
      New_Item : Element_Type;
      Node     : out Node_Access;
      Inserted : out Boolean)
   is
      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Conditional_Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

         Element : Element_Access := new Element_Type'(New_Item);

      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red_Black_Trees.Red,
                               Element => Element);

      exception
         when others =>
            Free_Element (Element);
            raise;
      end New_Node;

   --  Start of processing for Insert_Sans_Hint

   begin
      Conditional_Insert_Sans_Hint
        (Tree,
         New_Item,
         Node,
         Inserted);
   end Insert_Sans_Hint;

   ----------------------
   -- Insert_With_Hint --
   ----------------------

   procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access)
   is
      Success : Boolean;
      pragma Unreferenced (Success);

      function New_Node return Node_Access;

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Insert_With_Hint is
         new Element_Keys.Generic_Conditional_Insert_With_Hint
            (Insert_Post,
             Insert_Sans_Hint);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         Element : Element_Access := new Element_Type'(Src_Node.Element.all);
         Node    : Node_Access;

      begin
         begin
            Node := new Node_Type;
         exception
            when others =>
               Free_Element (Element);
               raise;
         end;

         Node.Element := Element;
         return Node;
      end New_Node;

   --  Start of processing for Insert_With_Hint

   begin
      Insert_With_Hint
        (Dst_Tree,
         Dst_Hint,
         Src_Node.Element.all,
         Dst_Node,
         Success);
   end Insert_With_Hint;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
      Set_Ops.Intersection (Target.Tree, Source.Tree);
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
      Tree : constant Tree_Type :=
        Set_Ops.Intersection (Left.Tree, Right.Tree);
   begin
      return Set'(Controlled with Tree);
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Tree.Length = 0;
   end Is_Empty;

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
      return Set_Ops.Is_Subset (Subset => Subset.Tree, Of_Set => Of_Set.Tree);
   end Is_Subset;

   -------------
   -- Iterate --
   -------------

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
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
      B : Natural renames T.Busy;

   --  Start of processing for Iterate

   begin
      B := B + 1;

      begin
         Local_Iterate (T);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   function Iterate
     (Container : Set)
      return Set_Iterator_Interfaces.Reversible_Iterator'class
   is
      B  : Natural renames Container'Unrestricted_Access.all.Tree.Busy;

   begin
      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is null (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a complete
      --  iterator, meaning that the iteration starts from the (logical)
      --  beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
        Iterator'(Limited_Controlled with
                    Container => Container'Unrestricted_Access,
                    Node      => null)
      do
         B := B + 1;
      end return;
   end Iterate;

   function Iterate
     (Container : Set;
      Start     : Cursor)
      return Set_Iterator_Interfaces.Reversible_Iterator'class
   is
      B  : Natural renames Container'Unrestricted_Access.all.Tree.Busy;

   begin
      --  It was formerly the case that when Start = No_Element, the partial
      --  iterator was defined to behave the same as for a complete iterator,
      --  and iterate over the entire sequence of items. However, those
      --  semantics were unintuitive and arguably error-prone (it is too easy
      --  to accidentally create an endless loop), and so they were changed,
      --  per the ARG meeting in Denver on 2011/11. However, there was no
      --  consensus about what positive meaning this corner case should have,
      --  and so it was decided to simply raise an exception. This does imply,
      --  however, that it is not possible to use a partial iterator to specify
      --  an empty sequence of items.

      if Start = No_Element then
         raise Constraint_Error with
           "Start position for iterator equals No_Element";
      end if;

      if Start.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Start cursor of Iterate designates wrong set";
      end if;

      pragma Assert (Vet (Container.Tree, Start.Node),
                     "Start cursor of Iterate is bad");

      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is non-null (as is the case here), it means that this is a
      --  partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this is
      --  a forward or reverse iteration.

      return It : constant Iterator :=
        (Limited_Controlled with
           Container => Container'Unrestricted_Access,
           Node      => Start.Node)
      do
         B := B + 1;
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : Set) return Cursor is
   begin
      return
        (if Container.Tree.Last = null then No_Element
         else Cursor'(Container'Unrestricted_Access, Container.Tree.Last));
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Node component is null, this means the iterator object was
      --  constructed without a start expression, in which case the (reverse)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is non-null, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (reverse) partial iteration begins.

      if Object.Node = null then
         return Object.Container.Last;
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Set) return Element_Type is
   begin
      if Container.Tree.Last = null then
         raise Constraint_Error with "set is empty";
      else
         return Container.Tree.Last.Element.all;
      end if;
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

   procedure Move is new Tree_Operations.Generic_Move (Clear);

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
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

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "bad cursor in Next");

      declare
         Node : constant Node_Access := Tree_Operations.Next (Position.Node);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Position.Container, Node));
      end;
   end Next;

   function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong set";
      end if;

      return Next (Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
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

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "bad cursor in Previous");

      declare
         Node : constant Node_Access :=
           Tree_Operations.Previous (Position.Node);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Position.Container, Node));
      end;
   end Previous;

   function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Previous designates wrong set";
      end if;

      return Previous (Position);
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position  : Cursor;
      Process   : not null access procedure (Element : Element_Type))
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "bad cursor in Query_Element");

      declare
         T : Tree_Type renames Position.Container.Tree;

         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (Position.Node.Element.all);
         exception
            when others =>
               L := L - 1;
               B := B - 1;
               raise;
         end;

         L := L - 1;
         B := B - 1;
      end;
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Set)
   is
      function Read_Node
        (Stream : not null access Root_Stream_Type'Class) return Node_Access;
      pragma Inline (Read_Node);

      procedure Read is
         new Tree_Operations.Generic_Read (Clear, Read_Node);

      ---------------
      -- Read_Node --
      ---------------

      function Read_Node
        (Stream : not null access Root_Stream_Type'Class) return Node_Access
      is
         Node : Node_Access := new Node_Type;

      begin
         Node.Element := new Element_Type'(Element_Type'Input (Stream));
         return Node;

      exception
         when others =>
            Free (Node);  --  Note that Free deallocates elem too
            raise;
      end Read_Node;

   --  Start of processing for Read

   begin
      Read (Stream, Container.Tree);
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream set cursor";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in out Set; New_Item : Element_Type) is
      Node : constant Node_Access :=
        Element_Keys.Find (Container.Tree, New_Item);

      X : Element_Access;
      pragma Warnings (Off, X);

   begin
      if Node = null then
         raise Constraint_Error with "attempt to replace element not in set";
      end if;

      if Container.Tree.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (set is locked)";
      end if;

      declare
         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         X := Node.Element;
         Node.Element := new Element_Type'(New_Item);
         Free_Element (X);
      end;
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Tree : in out Tree_Type;
      Node : Node_Access;
      Item : Element_Type)
   is
      pragma Assert (Node /= null);
      pragma Assert (Node.Element /= null);

      function New_Node return Node_Access;
      pragma Inline (New_Node);

      procedure Local_Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Local_Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Local_Insert_Post);

      procedure Local_Insert_With_Hint is
        new Element_Keys.Generic_Conditional_Insert_With_Hint
          (Local_Insert_Post,
           Local_Insert_Sans_Hint);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is

         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Node.Element := new Element_Type'(Item);  -- OK if fails
         Node.Color := Red;
         Node.Parent := null;
         Node.Right := null;
         Node.Left := null;
         return Node;
      end New_Node;

      Hint     : Node_Access;
      Result   : Node_Access;
      Inserted : Boolean;
      Compare  : Boolean;

      X : Element_Access := Node.Element;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B : Natural renames Tree.Busy;
      L : Natural renames Tree.Lock;

   --  Start of processing for Replace_Element

   begin
      --  Replace_Element assigns value Item to the element designated by Node,
      --  per certain semantic constraints, described as follows.

      --  If Item is equivalent to the element, then element is replaced and
      --  there's nothing else to do. This is the easy case.

      --  If Item is not equivalent, then the node will (possibly) have to move
      --  to some other place in the tree. This is slighly more complicated,
      --  because we must ensure that Item is not equivalent to some other
      --  element in the tree (in which case, the replacement is not allowed).

      --  Determine whether Item is equivalent to element on the specified
      --  node.

      begin
         B := B + 1;
         L := L + 1;

         Compare := (if Item < Node.Element.all then False
                     elsif Node.Element.all < Item then False
                     else True);

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;

            raise;
      end;

      if Compare then
         --  Item is equivalent to the node's element, so we will not have to
         --  move the node.

         if Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (set is locked)";
         end if;

         declare
            --  The element allocator may need an accessibility check in the
            --  case the actual type is class-wide or has access discriminants
            --  (see RM 4.8(10.1) and AI12-0035).

            pragma Unsuppress (Accessibility_Check);

         begin
            Node.Element := new Element_Type'(Item);
            Free_Element (X);
         end;

         return;
      end if;

      --  The replacement Item is not equivalent to the element on the
      --  specified node, which means that it will need to be re-inserted in a
      --  different position in the tree. We must now determine whether Item is
      --  equivalent to some other element in the tree (which would prohibit
      --  the assignment and hence the move).

      --  Ceiling returns the smallest element equivalent or greater than the
      --  specified Item; if there is no such element, then it returns null.

      Hint := Element_Keys.Ceiling (Tree, Item);

      if Hint /= null then
         begin
            B := B + 1;
            L := L + 1;

            Compare := Item < Hint.Element.all;

            L := L - 1;
            B := B - 1;

         exception
            when others =>
               L := L - 1;
               B := B - 1;

               raise;
         end;

         --  Item >= Hint.Element

         if not Compare then

            --  Ceiling returns an element that is equivalent or greater
            --  than Item. If Item is "not less than" the element, then
            --  by elimination we know that Item is equivalent to the element.

            --  But this means that it is not possible to assign the value of
            --  Item to the specified element (on Node), because a different
            --  element (on Hint) equivalent to Item already exsits. (Were we
            --  to change Node's element value, we would have to move Node, but
            --  we would be unable to move the Node, because its new position
            --  in the tree is already occupied by an equivalent element.)

            raise Program_Error with "attempt to replace existing element";
         end if;

         --  Item is not equivalent to any other element in the tree, so it is
         --  safe to assign the value of Item to Node.Element. This means that
         --  the node will have to move to a different position in the tree
         --  (because its element will have a different value).

         --  The nearest (greater) neighbor of Item is Hint. This will be the
         --  insertion position of Node (because its element will have Item as
         --  its new value).

         --  If Node equals Hint, the relative position of Node does not
         --  change. This allows us to perform an optimization: we need not
         --  remove Node from the tree and then reinsert it with its new value,
         --  because it would only be placed in the exact same position.

         if Hint = Node then
            if Tree.Lock > 0 then
               raise Program_Error with
                 "attempt to tamper with elements (set is locked)";
            end if;

            declare
               --  The element allocator may need an accessibility check in the
               --  case actual type is class-wide or has access discriminants
               --  (see RM 4.8(10.1) and AI12-0035).

               pragma Unsuppress (Accessibility_Check);

            begin
               Node.Element := new Element_Type'(Item);
               Free_Element (X);
            end;

            return;
         end if;
      end if;

      --  If we get here, it is because Item was greater than all elements in
      --  the tree (Hint = null), or because Item was less than some element at
      --  a different place in the tree (Item < Hint.Element.all). In either
      --  case, we remove Node from the tree (without actually deallocating
      --  it), and then insert Item into the tree, onto the same Node (so no
      --  new node is actually allocated).

      Tree_Operations.Delete_Node_Sans_Free (Tree, Node);  -- Checks busy-bit

      Local_Insert_With_Hint
        (Tree     => Tree,
         Position => Hint,
         Key      => Item,
         Node     => Result,
         Inserted => Inserted);

      pragma Assert (Inserted);
      pragma Assert (Result = Node);

      Free_Element (X);
   end Replace_Element;

   procedure Replace_Element
    (Container : in out Set;
     Position  : Cursor;
     New_Item  : Element_Type)
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Position cursor is bad";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "bad cursor in Replace_Element");

      Replace_Element (Container.Tree, Position.Node, New_Item);
   end Replace_Element;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

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
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      T : Tree_Type renames Container.Tree'Unrestricted_Access.all;
      B : Natural renames T.Busy;

   --  Start of processing for Reverse_Iterate

   begin
      B := B + 1;

      begin
         Local_Reverse_Iterate (T);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
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
      Set_Ops.Symmetric_Difference (Target.Tree, Source.Tree);
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
      Tree : constant Tree_Type :=
        Set_Ops.Symmetric_Difference (Left.Tree, Right.Tree);
   begin
      return Set'(Controlled with Tree);
   end Symmetric_Difference;

   ------------
   -- To_Set --
   ------------

   function To_Set (New_Item : Element_Type) return Set is
      Tree     : Tree_Type;
      Node     : Node_Access;
      Inserted : Boolean;
      pragma Unreferenced (Node, Inserted);
   begin
      Insert_Sans_Hint (Tree, New_Item, Node, Inserted);
      return Set'(Controlled with Tree);
   end To_Set;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Set; Source : Set) is
   begin
      Set_Ops.Union (Target.Tree, Source.Tree);
   end Union;

   function Union (Left, Right : Set) return Set is
      Tree : constant Tree_Type := Set_Ops.Union (Left.Tree, Right.Tree);
   begin
      return Set'(Controlled with Tree);
   end Union;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Set)
   is
      procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Access);
      pragma Inline (Write_Node);

      procedure Write is
         new Tree_Operations.Generic_Write (Write_Node);

      ----------------
      -- Write_Node --
      ----------------

      procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Access)
      is
      begin
         Element_Type'Output (Stream, Node.Element.all);
      end Write_Node;

   --  Start of processing for Write

   begin
      Write (Stream, Container.Tree);
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream set cursor";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

end Ada.Containers.Indefinite_Ordered_Sets;
