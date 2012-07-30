------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.INDEFINITE_ORDERED_MAPS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2012, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

with Ada.Containers.Red_Black_Trees.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Keys);

with System; use type System.Address;

package body Ada.Containers.Indefinite_Ordered_Maps is
   pragma Suppress (All_Checks);

   type Iterator is new Limited_Controlled and
     Map_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Map_Access;
      Node      : Node_Access;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

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

   procedure Free (X : in out Node_Access);

   function Is_Equal_Node_Node
     (L, R : Node_Access) return Boolean;
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

   package Tree_Operations is
     new Red_Black_Trees.Generic_Operations (Tree_Types);

   procedure Delete_Tree is
      new Tree_Operations.Generic_Delete_Tree (Free);

   function Copy_Tree is
      new Tree_Operations.Generic_Copy_Tree (Copy_Node, Delete_Tree);

   use Tree_Operations;

   package Key_Ops is
     new Red_Black_Trees.Generic_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Key_Type,
        Is_Less_Key_Node    => Is_Less_Key_Node,
        Is_Greater_Key_Node => Is_Greater_Key_Node);

   procedure Free_Key is
     new Ada.Unchecked_Deallocation (Key_Type, Key_Access);

   procedure Free_Element is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   function Is_Equal is
     new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      if Right.Node = null then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      if Left.Node.Key = null then
         raise Program_Error with "Left cursor in ""<"" is bad";
      end if;

      if Right.Node.Key = null then
         raise Program_Error with "Right cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor in ""<"" is bad");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor in ""<"" is bad");

      return Left.Node.Key.all < Right.Node.Key.all;
   end "<";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      if Left.Node.Key = null then
         raise Program_Error with "Left cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor in ""<"" is bad");

      return Left.Node.Key.all < Right;
   end "<";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = null then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      if Right.Node.Key = null then
         raise Program_Error with "Right cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor in ""<"" is bad");

      return Left < Right.Node.Key.all;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
   begin
      return Is_Equal (Left.Tree, Right.Tree);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      if Right.Node = null then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      if Left.Node.Key = null then
         raise Program_Error with "Left cursor in ""<"" is bad";
      end if;

      if Right.Node.Key = null then
         raise Program_Error with "Right cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor in "">"" is bad");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor in "">"" is bad");

      return Right.Node.Key.all < Left.Node.Key.all;
   end ">";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Left.Node = null then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      if Left.Node.Key = null then
         raise Program_Error with "Left cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor in "">"" is bad");

      return Right < Left.Node.Key.all;
   end ">";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = null then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      if Right.Node.Key = null then
         raise Program_Error with "Right cursor in ""<"" is bad";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor in "">"" is bad");

      return Right.Node.Key.all < Left;
   end ">";

   ------------
   -- Adjust --
   ------------

   procedure Adjust is new Tree_Operations.Generic_Adjust (Copy_Tree);

   procedure Adjust (Container : in out Map) is
   begin
      Adjust (Container.Tree);
   end Adjust;

   procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            T : Tree_Type renames Control.Container.all.Tree;
            B : Natural renames T.Busy;
            L : Natural renames T.Lock;
         begin
            B := B + 1;
            L := L + 1;
         end;
      end if;
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Map; Source : Map) is
      procedure Insert_Item (Node : Node_Access);
      pragma Inline (Insert_Item);

      procedure Insert_Items is
         new Tree_Operations.Generic_Iteration (Insert_Item);

      -----------------
      -- Insert_Item --
      -----------------

      procedure Insert_Item (Node : Node_Access) is
      begin
         Target.Insert (Key => Node.Key.all, New_Item => Node.Element.all);
      end Insert_Item;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Target.Clear;
      Insert_Items (Target.Tree);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Ceiling (Container.Tree, Key);
   begin
      return (if Node = null then No_Element
                else Cursor'(Container'Unrestricted_Access, Node));
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear is new Tree_Operations.Generic_Clear (Delete_Tree);

   procedure Clear (Container : in out Map) is
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
     (Container : aliased Map;
      Position  : Cursor) return Constant_Reference_Type
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong map";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor in Constant_Reference is bad");

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;
      begin
         return R : constant Constant_Reference_Type :=
                      (Element => Position.Node.Element.all'Access,
                       Control =>
                         (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Map;
      Key       : Key_Type) return Constant_Reference_Type
   is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      if Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;
      begin
         return R : constant Constant_Reference_Type :=
                      (Element => Node.Element.all'Access,
                       Control =>
                         (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Map) return Map is
   begin
      return Target : Map do
         Target.Assign (Source);
      end return;
   end Copy;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Source : Node_Access) return Node_Access is
      K : Key_Access := new Key_Type'(Source.Key.all);
      E : Element_Access;

   begin
      E := new Element_Type'(Source.Element.all);

      return new Node_Type'(Parent  => null,
                            Left    => null,
                            Right   => null,
                            Color   => Source.Color,
                            Key     => K,
                            Element => E);
   exception
      when others =>
         Free_Key (K);
         Free_Element (E);
         raise;
   end Copy_Node;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Map;
      Position  : in out Cursor)
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Delete equals No_Element";
      end if;

      if Position.Node.Key = null
        or else Position.Node.Element = null
      then
         raise Program_Error with "Position cursor of Delete is bad";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Delete designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor of Delete is bad");

      Tree_Operations.Delete_Node_Sans_Free (Container.Tree, Position.Node);
      Free (Position.Node);

      Position.Container := null;
   end Delete;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if X = null then
         raise Constraint_Error with "key not in map";
      end if;

      Delete_Node_Sans_Free (Container.Tree, X);
      Free (X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Map) is
      X : Node_Access := Container.Tree.First;
   begin
      if X /= null then
         Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Map) is
      X : Node_Access := Container.Tree.Last;
   begin
      if X /= null then
         Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
         Free (X);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of function Element equals No_Element";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with
           "Position cursor of function Element is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of function Element is bad");

      return Position.Node.Element.all;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      return Node.Element.all;
   end Element;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
   begin
      return (if Left < Right or else Right < Left then False else True);
   end Equivalent_Keys;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      X : Node_Access := Key_Ops.Find (Container.Tree, Key);
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
            T : Tree_Type renames Control.Container.all.Tree;
            B : Natural renames T.Busy;
            L : Natural renames T.Lock;
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

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);
   begin
      return (if Node = null then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
      T : Tree_Type renames Container.Tree;
   begin
      return (if T.First = null then No_Element
              else Cursor'(Container'Unrestricted_Access, T.First));
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Node component is null, this means the iterator object was
      --  constructed without a start expression, in which case the (forward)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  of items (corresponding to Container.First for a forward iterator).

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

   function First_Element (Container : Map) return Element_Type is
      T : Tree_Type renames Container.Tree;
   begin
      if T.First = null then
         raise Constraint_Error with "map is empty";
      else
         return T.First.Element.all;
      end if;
   end First_Element;

   ---------------
   -- First_Key --
   ---------------

   function First_Key (Container : Map) return Key_Type is
      T : Tree_Type renames Container.Tree;
   begin
      if T.First = null then
         raise Constraint_Error with "map is empty";
      else
         return T.First.Key.all;
      end if;
   end First_Key;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Floor (Container.Tree, Key);
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
         Free_Key (X.Key);

      exception
         when others =>
            X.Key := null;

            begin
               Free_Element (X.Element);
            exception
               when others =>
                  X.Element := null;
            end;

            Deallocate (X);
            raise;
      end;

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

      K : Key_Access;
      E : Element_Access;

   begin
      Insert (Container, Key, New_Item, Position, Inserted);

      if not Inserted then
         if Container.Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (map is locked)";
         end if;

         K := Position.Node.Key;
         E := Position.Node.Element;

         Position.Node.Key := new Key_Type'(Key);

         declare
            --  The element allocator may need an accessibility check in the
            --  case the actual type is class-wide or has access discriminants
            --  (see RM 4.8(10.1) and AI12-0035).

            pragma Unsuppress (Accessibility_Check);

         begin
            Position.Node.Element := new Element_Type'(New_Item);

         exception
            when others =>
               Free_Key (K);
               raise;
         end;

         Free_Key (K);
         Free_Element (E);
      end if;
   end Include;

   ------------
   -- Insert --
   ------------

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
         Node : Node_Access := new Node_Type;

         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Node.Key := new Key_Type'(Key);
         Node.Element := new Element_Type'(New_Item);
         return Node;

      exception
         when others =>

            --  On exception, deallocate key and elem. Note that free
            --  deallocates both the key and the elem.

            Free (Node);
            raise;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container.Tree,
         Key,
         Position.Node,
         Inserted);

      Position.Container := Container'Unrestricted_Access;
   end Insert;

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      pragma Unreferenced (Position);

      Inserted : Boolean;

   begin
      Insert (Container, Key, New_Item, Position, Inserted);

      if not Inserted then
         raise Constraint_Error with "key already in map";
      end if;
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

   function Is_Equal_Node_Node (L, R : Node_Access) return Boolean is
   begin
      return (if L.Key.all < R.Key.all then False
              elsif R.Key.all < L.Key.all then False
              else L.Element.all = R.Element.all);
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

      return Right.Key.all < Left;
   end Is_Greater_Key_Node;

   ----------------------
   -- Is_Less_Key_Node --
   ----------------------

   function Is_Less_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean is
   begin
      return Left < Right.Key.all;
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
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      B : Natural renames Container'Unrestricted_Access.all.Tree.Busy;

   --  Start of processing for Iterate

   begin
      B := B + 1;

      begin
         Local_Iterate (Container.Tree);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   function Iterate
     (Container : Map) return Map_Iterator_Interfaces.Reversible_Iterator'Class
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
                    (Limited_Controlled with
                       Container => Container'Unrestricted_Access,
                       Node      => null)
      do
         B := B + 1;
      end return;
   end Iterate;

   function Iterate
     (Container : Map;
      Start     : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
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
           "Start cursor of Iterate designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Start.Node),
                     "Start cursor of Iterate is bad");

      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is non-null (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.

      return It : constant Iterator :=
                    (Limited_Controlled with
                       Container => Container'Unrestricted_Access,
                       Node      => Start.Node)
      do
         B := B + 1;
      end return;
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of function Key equals No_Element";
      end if;

      if Position.Node.Key = null then
         raise Program_Error with
           "Position cursor of function Key is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of function Key is bad");

      return Position.Node.Key.all;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
      T : Tree_Type renames Container.Tree;
   begin
      return (if T.Last = null then No_Element
              else Cursor'(Container'Unrestricted_Access, T.Last));
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

   function Last_Element (Container : Map) return Element_Type is
      T : Tree_Type renames Container.Tree;

   begin
      if T.Last = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.Last.Element.all;
   end Last_Element;

   --------------
   -- Last_Key --
   --------------

   function Last_Key (Container : Map) return Key_Type is
      T : Tree_Type renames Container.Tree;

   begin
      if T.Last = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.Last.Key.all;
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

   procedure Move is new Tree_Operations.Generic_Move (Clear);

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
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

      pragma Assert (Position.Node /= null);
      pragma Assert (Position.Node.Key /= null);
      pragma Assert (Position.Node.Element /= null);
      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Next is bad");

      declare
         Node : constant Node_Access :=
                  Tree_Operations.Next (Position.Node);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Position.Container, Node));
      end;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
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
           "Position cursor of Next designates wrong map";
      end if;

      return Next (Position);
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

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      pragma Assert (Position.Node /= null);
      pragma Assert (Position.Node.Key /= null);
      pragma Assert (Position.Node.Element /= null);
      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Previous is bad");

      declare
         Node : constant Node_Access :=
                  Tree_Operations.Previous (Position.Node);
      begin
         return (if Node = null then No_Element
                 else Cursor'(Position.Container, Node));
      end;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
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
           "Position cursor of Previous designates wrong map";
      end if;

      return Previous (Position);
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type))
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      if Position.Node.Key = null
        or else Position.Node.Element = null
      then
         raise Program_Error with
           "Position cursor of Query_Element is bad";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Query_Element is bad");

      declare
         T : Tree_Type renames Position.Container.Tree;

         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         declare
            K : Key_Type renames Position.Node.Key.all;
            E : Element_Type renames Position.Node.Element.all;

         begin
            Process (K, E);
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
      Container : out Map)
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
         Node.Key := new Key_Type'(Key_Type'Input (Stream));
         Node.Element := new Element_Type'(Element_Type'Input (Stream));
         return Node;
      exception
         when others =>
            Free (Node);  --  Note that Free deallocates key and elem too
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
      raise Program_Error with "attempt to stream map cursor";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Map;
      Position  : Cursor) return Reference_Type
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong map";
      end if;

      if Position.Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor in function Reference is bad");

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;
      begin
         return R : constant Reference_Type :=
                      (Element => Position.Node.Element.all'Access,
                       Control => (Controlled with Position.Container))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Reference;

   function Reference
     (Container : aliased in out Map;
      Key       : Key_Type) return Reference_Type
   is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      if Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;
      begin
         return R : constant Reference_Type :=
                      (Element => Node.Element.all'Access,
                       Control =>
                         (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Reference;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Node : constant Node_Access :=
               Key_Ops.Find (Container.Tree, Key);

      K : Key_Access;
      E : Element_Access;

   begin
      if Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      if Container.Tree.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (map is locked)";
      end if;

      K := Node.Key;
      E := Node.Element;

      Node.Key := new Key_Type'(Key);

      declare
         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Node.Element := new Element_Type'(New_Item);

      exception
         when others =>
            Free_Key (K);
            raise;
      end;

      Free_Key (K);
      Free_Element (E);
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Replace_Element equals No_Element";
      end if;

      if Position.Node.Key = null
        or else Position.Node.Element = null
      then
         raise Program_Error with
           "Position cursor of Replace_Element is bad";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Replace_Element designates wrong map";
      end if;

      if Container.Tree.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (map is locked)";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor of Replace_Element is bad");

      declare
         X : Element_Access := Position.Node.Element;

         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Position.Node.Element := new Element_Type'(New_Item);
         Free_Element (X);
      end;
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
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      B : Natural renames Container.Tree'Unrestricted_Access.all.Busy;

   --  Start of processing for Reverse_Iterate

   begin
      B := B + 1;

      begin
         Local_Reverse_Iterate (Container.Tree);
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

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access procedure (Key     : Key_Type;
                                             Element : in out Element_Type))
   is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Update_Element equals No_Element";
      end if;

      if Position.Node.Key = null
        or else Position.Node.Element = null
      then
         raise Program_Error with
           "Position cursor of Update_Element is bad";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Update_Element designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor of Update_Element is bad");

      declare
         T : Tree_Type renames Position.Container.Tree;

         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         declare
            K : Key_Type renames Position.Node.Key.all;
            E : Element_Type renames Position.Node.Element.all;

         begin
            Process (K, E);

         exception
            when others =>
               L := L - 1;
               B := B - 1;
               raise;
         end;

         L := L - 1;
         B := B - 1;
      end;
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Map)
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
         Key_Type'Output (Stream, Node.Key.all);
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
      raise Program_Error with "attempt to stream map cursor";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

end Ada.Containers.Indefinite_Ordered_Maps;
