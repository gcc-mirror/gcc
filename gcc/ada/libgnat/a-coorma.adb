------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           A D A . C O N T A I N E R S . O R D E R E D _ M A P S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2017, Free Software Foundation, Inc.         --
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

with Ada.Containers.Helpers; use Ada.Containers.Helpers;

with Ada.Containers.Red_Black_Trees.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Keys);

with System; use type System.Address;

package body Ada.Containers.Ordered_Maps is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

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

   function Is_Equal is
     new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      if Checks and then Left.Node = null then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      if Checks and then Right.Node = null then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor of ""<"" is bad");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor of ""<"" is bad");

      return Left.Node.Key < Right.Node.Key;
   end "<";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Checks and then Left.Node = null then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor of ""<"" is bad");

      return Left.Node.Key < Right;
   end "<";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Checks and then Right.Node = null then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor of ""<"" is bad");

      return Left < Right.Node.Key;
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
      if Checks and then Left.Node = null then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      if Checks and then Right.Node = null then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor of "">"" is bad");

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor of "">"" is bad");

      return Right.Node.Key < Left.Node.Key;
   end ">";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Checks and then Left.Node = null then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.Tree, Left.Node),
                     "Left cursor of "">"" is bad");

      return Right < Left.Node.Key;
   end ">";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Checks and then Right.Node = null then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.Tree, Right.Node),
                     "Right cursor of "">"" is bad");

      return Right.Node.Key < Left;
   end ">";

   ------------
   -- Adjust --
   ------------

   procedure Adjust is
      new Tree_Operations.Generic_Adjust (Copy_Tree);

   procedure Adjust (Container : in out Map) is
   begin
      Adjust (Container.Tree);
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
         Target.Insert (Key => Node.Key, New_Item => Node.Element);
      end Insert_Item;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Target.Clear;
      Insert_Items (Source.Tree);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Ceiling (Container.Tree, Key);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Node);
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
      if Checks and then Position.Container = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor in Constant_Reference is bad");

      declare
         T : Tree_Type renames Position.Container.all.Tree;
         TC : constant Tamper_Counts_Access :=
           T.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => Position.Node.Element'Access,
            Control => (Controlled with TC))
         do
            Lock (TC.all);
         end return;
      end;
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Map;
      Key       : Key_Type) return Constant_Reference_Type
   is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Checks and then Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         TC : constant Tamper_Counts_Access :=
           T.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => Node.Element'Access,
            Control => (Controlled with TC))
         do
            Lock (TC.all);
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
      Target : constant Node_Access :=
        new Node_Type'(Color   => Source.Color,
                       Key     => Source.Key,
                       Element => Source.Element,
                       Parent  => null,
                       Left    => null,
                       Right   => null);
   begin
      return Target;
   end Copy_Node;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Position : in out Cursor) is
      Tree : Tree_Type renames Container.Tree;

   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Delete equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Delete designates wrong map";
      end if;

      pragma Assert (Vet (Tree, Position.Node),
                     "Position cursor of Delete is bad");

      Tree_Operations.Delete_Node_Sans_Free (Tree, Position.Node);
      Free (Position.Node);

      Position.Container := null;
   end Delete;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Checks and then X = null then
         raise Constraint_Error with "key not in map";
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Container.Tree, X);
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
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of function Element equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of function Element is bad");

      return Position.Node.Element;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Checks and then Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      return Node.Element;
   end Element;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
   begin
      if Left < Right
        or else Right < Left
      then
         return False;
      else
         return True;
      end if;
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
         Unbusy (Object.Container.Tree.TC);
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
      if T.First = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, T.First);
      end if;
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

   function First_Element (Container : Map) return Element_Type is
      T : Tree_Type renames Container.Tree;
   begin
      if Checks and then T.First = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.First.Element;
   end First_Element;

   ---------------
   -- First_Key --
   ---------------

   function First_Key (Container : Map) return Key_Type is
      T : Tree_Type renames Container.Tree;
   begin
      if Checks and then T.First = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.First.Key;
   end First_Key;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Floor (Container.Tree, Key);
   begin
      if Node = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Node);
      end if;
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

      Deallocate (X);
   end Free;

   ------------------------
   -- Get_Element_Access --
   ------------------------

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access is
   begin
      return Position.Node.Element'Access;
   end Get_Element_Access;

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
         TE_Check (Container.Tree.TC);

         Position.Node.Key := Key;
         Position.Node.Element := New_Item;
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
      begin
         return new Node_Type'(Key     => Key,
                               Element => New_Item,
                               Color   => Red_Black_Trees.Red,
                               Parent  => null,
                               Left    => null,
                               Right   => null);
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

      if Checks and then not Inserted then
         raise Constraint_Error with "key already in map";
      end if;
   end Insert;

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
      begin
         return new Node_Type'(Key     => Key,
                               Element => <>,
                               Color   => Red_Black_Trees.Red,
                               Parent  => null,
                               Left    => null,
                               Right   => null);
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
     (L, R : Node_Access) return Boolean
   is
   begin
      if L.Key < R.Key then
         return False;
      elsif R.Key < L.Key then
         return False;
      else
         return L.Element = R.Element;
      end if;
   end Is_Equal_Node_Node;

   -------------------------
   -- Is_Greater_Key_Node --
   -------------------------

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Access) return Boolean
   is
   begin
      --  Left > Right same as Right < Left

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
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      Busy : With_Busy (Container.Tree.TC'Unrestricted_Access);

   --  Start of processing for Iterate

   begin
      Local_Iterate (Container.Tree);
   end Iterate;

   function Iterate
     (Container : Map) return Map_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is null (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a
      --  complete iterator, meaning that the iteration starts from the
      --  (logical) beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
        (Limited_Controlled with
           Container => Container'Unrestricted_Access,
           Node      => null)
      do
         Busy (Container.Tree.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   function Iterate (Container : Map; Start : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
   is
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

      if Checks and then Start = No_Element then
         raise Constraint_Error with
           "Start position for iterator equals No_Element";
      end if;

      if Checks and then Start.Container /= Container'Unrestricted_Access then
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
         Busy (Container.Tree.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of function Key equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of function Key is bad");

      return Position.Node.Key;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
      T : Tree_Type renames Container.Tree;
   begin
      if T.Last = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, T.Last);
      end if;
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
      if Checks and then T.Last = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.Last.Element;
   end Last_Element;

   --------------
   -- Last_Key --
   --------------

   function Last_Key (Container : Map) return Key_Type is
      T : Tree_Type renames Container.Tree;
   begin
      if Checks and then T.Last = null then
         raise Constraint_Error with "map is empty";
      end if;

      return T.Last.Key;
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

   procedure Move is
      new Tree_Operations.Generic_Move (Clear);

   procedure Move (Target : in out Map; Source : in out Map) is
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

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Next is bad");

      declare
         Node : constant Node_Access := Tree_Operations.Next (Position.Node);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
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

      if Checks and then Position.Container /= Object.Container then
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

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Previous is bad");

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

   function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Previous designates wrong map";
      end if;

      return Previous (Position);
   end Previous;

   ----------------------
   -- Pseudo_Reference --
   ----------------------

   function Pseudo_Reference
     (Container : aliased Map'Class) return Reference_Control_Type
   is
      TC : constant Tamper_Counts_Access :=
        Container.Tree.TC'Unrestricted_Access;
   begin
      return R : constant Reference_Control_Type := (Controlled with TC) do
         Lock (TC.all);
      end return;
   end Pseudo_Reference;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type))
   is
   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.Tree, Position.Node),
                     "Position cursor of Query_Element is bad");

      declare
         T : Tree_Type renames Position.Container.Tree;
         Lock : With_Lock (T.TC'Unrestricted_Access);
         K : Key_Type renames Position.Node.Key;
         E : Element_Type renames Position.Node.Element;
      begin
         Process (K, E);
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
         Key_Type'Read (Stream, Node.Key);
         Element_Type'Read (Stream, Node.Element);
         return Node;
      exception
         when others =>
            Free (Node);
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
      if Checks and then Position.Container = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor in function Reference is bad");

      declare
         T : Tree_Type renames Position.Container.all.Tree;
         TC : constant Tamper_Counts_Access :=
           T.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => Position.Node.Element'Access,
            Control => (Controlled with TC))
         do
            Lock (TC.all);
         end return;
      end;
   end Reference;

   function Reference
     (Container : aliased in out Map;
      Key       : Key_Type) return Reference_Type
   is
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Checks and then Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      declare
         T : Tree_Type renames Container'Unrestricted_Access.all.Tree;
         TC : constant Tamper_Counts_Access :=
           T.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => Node.Element'Access,
            Control => (Controlled with TC))
         do
            Lock (TC.all);
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
      Node : constant Node_Access := Key_Ops.Find (Container.Tree, Key);

   begin
      if Checks and then Node = null then
         raise Constraint_Error with "key not in map";
      end if;

      TE_Check (Container.Tree.TC);

      Node.Key := Key;
      Node.Element := New_Item;
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
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Replace_Element equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Replace_Element designates wrong map";
      end if;

      TE_Check (Container.Tree.TC);

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor of Replace_Element is bad");

      Position.Node.Element := New_Item;
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

      Busy : With_Busy (Container.Tree.TC'Unrestricted_Access);

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
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access procedure (Key     : Key_Type;
                                             Element : in out Element_Type))
   is
   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Update_Element equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Update_Element designates wrong map";
      end if;

      pragma Assert (Vet (Container.Tree, Position.Node),
                     "Position cursor of Update_Element is bad");

      declare
         T : Tree_Type renames Container.Tree;
         Lock : With_Lock (T.TC'Unrestricted_Access);
         K : Key_Type renames Position.Node.Key;
         E : Element_Type renames Position.Node.Element;
      begin
         Process (K, E);
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
         Key_Type'Write (Stream, Node.Key);
         Element_Type'Write (Stream, Node.Element);
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

end Ada.Containers.Ordered_Maps;
