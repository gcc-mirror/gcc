------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . B O U N D E D _ O R D E R E D _ M A P S  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys);

with Ada.Finalization; use Ada.Finalization;

with System; use type System.Address;

package body Ada.Containers.Bounded_Ordered_Maps is

   type Iterator is new Limited_Controlled and
     Map_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Map_Access;
      Node      : Count_Type;
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

   function Color (Node : Node_Type) return Color_Type;
   pragma Inline (Color);

   function Left (Node : Node_Type) return Count_Type;
   pragma Inline (Left);

   function Parent (Node : Node_Type) return Count_Type;
   pragma Inline (Parent);

   function Right (Node : Node_Type) return Count_Type;
   pragma Inline (Right);

   procedure Set_Parent (Node : in out Node_Type; Parent : Count_Type);
   pragma Inline (Set_Parent);

   procedure Set_Left (Node : in out Node_Type; Left : Count_Type);
   pragma Inline (Set_Left);

   procedure Set_Right (Node : in out Node_Type; Right : Count_Type);
   pragma Inline (Set_Right);

   procedure Set_Color (Node : in out Node_Type; Color : Color_Type);
   pragma Inline (Set_Color);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Type) return Boolean;
   pragma Inline (Is_Greater_Key_Node);

   function Is_Less_Key_Node
     (Left  : Key_Type;
      Right : Node_Type) return Boolean;
   pragma Inline (Is_Less_Key_Node);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package Tree_Operations is
      new Red_Black_Trees.Generic_Bounded_Operations (Tree_Types);

   use Tree_Operations;

   package Key_Ops is
     new Red_Black_Trees.Generic_Bounded_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Key_Type,
        Is_Less_Key_Node    => Is_Less_Key_Node,
        Is_Greater_Key_Node => Is_Greater_Key_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = 0 then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      if Right.Node = 0 then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "Left cursor of ""<"" is bad");

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "Right cursor of ""<"" is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

      begin
         return LN.Key < RN.Key;
      end;
   end "<";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Left.Node = 0 then
         raise Constraint_Error with "Left cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "Left cursor of ""<"" is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);

      begin
         return LN.Key < Right;
      end;
   end "<";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = 0 then
         raise Constraint_Error with "Right cursor of ""<"" equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "Right cursor of ""<"" is bad");

      declare
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

      begin
         return Left < RN.Key;
      end;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
      function Is_Equal_Node_Node (L, R : Node_Type) return Boolean;
      pragma Inline (Is_Equal_Node_Node);

      function Is_Equal is
        new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

      ------------------------
      -- Is_Equal_Node_Node --
      ------------------------

      function Is_Equal_Node_Node
        (L, R : Node_Type) return Boolean is
      begin
         if L.Key < R.Key then
            return False;

         elsif R.Key < L.Key then
            return False;

         else
            return L.Element = R.Element;
         end if;
      end Is_Equal_Node_Node;

   --  Start of processing for "="

   begin
      return Is_Equal (Left, Right);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      if Left.Node = 0 then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      if Right.Node = 0 then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "Left cursor of "">"" is bad");

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "Right cursor of "">"" is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

      begin
         return RN.Key < LN.Key;
      end;
   end ">";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Left.Node = 0 then
         raise Constraint_Error with "Left cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "Left cursor of "">"" is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
      begin
         return Right < LN.Key;
      end;
   end ">";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Right.Node = 0 then
         raise Constraint_Error with "Right cursor of "">"" equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "Right cursor of "">"" is bad");

      declare
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

      begin
         return RN.Key < Left;
      end;
   end ">";

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Map; Source : Map) is
      procedure Append_Element (Source_Node : Count_Type);

      procedure Append_Elements is
         new Tree_Operations.Generic_Iteration (Append_Element);

      --------------------
      -- Append_Element --
      --------------------

      procedure Append_Element (Source_Node : Count_Type) is
         SN : Node_Type renames Source.Nodes (Source_Node);

         procedure Set_Element (Node : in out Node_Type);
         pragma Inline (Set_Element);

         function New_Node return Count_Type;
         pragma Inline (New_Node);

         procedure Insert_Post is
            new Key_Ops.Generic_Insert_Post (New_Node);

         procedure Unconditional_Insert_Sans_Hint is
            new Key_Ops.Generic_Unconditional_Insert (Insert_Post);

         procedure Unconditional_Insert_Avec_Hint is
            new Key_Ops.Generic_Unconditional_Insert_With_Hint
              (Insert_Post,
               Unconditional_Insert_Sans_Hint);

         procedure Allocate is
            new Tree_Operations.Generic_Allocate (Set_Element);

         --------------
         -- New_Node --
         --------------

         function New_Node return Count_Type is
            Result : Count_Type;

         begin
            Allocate (Target, Result);
            return Result;
         end New_Node;

         -----------------
         -- Set_Element --
         -----------------

         procedure Set_Element (Node : in out Node_Type) is
         begin
            Node.Key := SN.Key;
            Node.Element := SN.Element;
         end Set_Element;

         Target_Node : Count_Type;

      --  Start of processing for Append_Element

      begin
         Unconditional_Insert_Avec_Hint
           (Tree  => Target,
            Hint  => 0,
            Key   => SN.Key,
            Node  => Target_Node);
      end Append_Element;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Capacity_Error
           with "Target capacity is less than Source length";
      end if;

      Tree_Operations.Clear_Tree (Target);
      Append_Elements (Source);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Ceiling (Container, Key);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Node);
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      Tree_Operations.Clear_Tree (Container);
   end Clear;

   -----------
   -- Color --
   -----------

   function Color (Node : Node_Type) return Color_Type is
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

      pragma Assert (Vet (Container, Position.Node),
                     "Position cursor in Constant_Reference is bad");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
      begin
         return (Element => N.Element'Access);
      end;
   end Constant_Reference;

   function Constant_Reference
     (Container : Map;
      Key       : Key_Type) return Constant_Reference_Type
   is
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if Node = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      declare
         N : Node_Type renames Container.Nodes (Node);
      begin
         return (Element => N.Element'Access);
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

   function Copy (Source : Map; Capacity : Count_Type := 0) return Map is
      C : Count_Type;

   begin
      if Capacity = 0 then
         C := Source.Length;

      elsif Capacity >= Source.Length then
         C := Capacity;

      else
         raise Capacity_Error with "Capacity value too small";
      end if;

      return Target : Map (Capacity => C) do
         Assign (Target => Target, Source => Source);
      end return;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Delete equals No_Element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Delete designates wrong map";
      end if;

      pragma Assert (Vet (Container, Position.Node),
                     "Position cursor of Delete is bad");

      Tree_Operations.Delete_Node_Sans_Free (Container, Position.Node);
      Tree_Operations.Free (Container, Position.Node);

      Position := No_Element;
   end Delete;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if X = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Container, X);
      Tree_Operations.Free (Container, X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Map) is
      X : constant Count_Type := Container.First;

   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container, X);
         Tree_Operations.Free (Container, X);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Map) is
      X : constant Count_Type := Container.Last;

   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container, X);
         Tree_Operations.Free (Container, X);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of function Element equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "Position cursor of function Element is bad");

      return Position.Container.Nodes (Position.Node).Element;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Count_Type := Key_Ops.Find (Container, Key);
   begin
      if Node = 0 then
         raise Constraint_Error with "key not in map";
      else
         return Container.Nodes (Node).Element;
      end if;
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
      X : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container, X);
         Tree_Operations.Free (Container, X);
      end if;
   end Exclude;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Container /= null then
         declare
            B : Natural renames Object.Container.all.Busy;
         begin
            B := B - 1;
         end;
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Find (Container, Key);
   begin
      if Node = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Node);
      end if;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
   begin
      if Container.First = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.First);
      end if;
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Node component is 0, this means the iterator object was
      --  constructed without a start expression, in which case the (forward)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  of items (corresponding to Container.First, for a forward iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is positive, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (forward) partial iteration begins.

      if Object.Node = 0 then
         return Bounded_Ordered_Maps.First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Map) return Element_Type is
   begin
      if Container.First = 0 then
         raise Constraint_Error with "map is empty";
      else
         return Container.Nodes (Container.First).Element;
      end if;
   end First_Element;

   ---------------
   -- First_Key --
   ---------------

   function First_Key (Container : Map) return Key_Type is
   begin
      if Container.First = 0 then
         raise Constraint_Error with "map is empty";
      else
         return Container.Nodes (Container.First).Key;
      end if;
   end First_Key;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Floor (Container, Key);
   begin
      if Node = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Node);
      end if;
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
         if Container.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (map is locked)";
         end if;

         declare
            N : Node_Type renames Container.Nodes (Position.Node);
         begin
            N.Key := Key;
            N.Element := New_Item;
         end;
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
      procedure Assign (Node : in out Node_Type);
      pragma Inline (Assign);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Key_Ops.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Key_Ops.Generic_Conditional_Insert (Insert_Post);

      procedure Allocate is
         new Tree_Operations.Generic_Allocate (Assign);

      ------------
      -- Assign --
      ------------

      procedure Assign (Node : in out Node_Type) is
      begin
         Node.Key := Key;
         Node.Element := New_Item;
      end Assign;

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
         Result : Count_Type;
      begin
         Allocate (Container, Result);
         return Result;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container,
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

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      procedure Assign (Node : in out Node_Type);
      pragma Inline (Assign);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Key_Ops.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Key_Ops.Generic_Conditional_Insert (Insert_Post);

      procedure Allocate is
         new Tree_Operations.Generic_Allocate (Assign);

      ------------
      -- Assign --
      ------------

      procedure Assign (Node : in out Node_Type) is
      begin
         Node.Key := Key;

         --  Were this insertion operation to accept an element parameter, this
         --  is the point where the element value would be used, to update the
         --  element component of the new node. However, this insertion
         --  operation is special, in the sense that it does not accept an
         --  element parameter. Rather, this version of Insert allocates a node
         --  (inserting it among the active nodes of the container in the
         --  normal way, with the node's position being determined by the Key),
         --  and passes back a cursor designating the node. It is then up to
         --  the caller to assign a value to the node's element.

         --  Node.Element := New_Item;
      end Assign;

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
         Result : Count_Type;
      begin
         Allocate (Container, Result);
         return Result;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container,
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
      return Container.Length = 0;
   end Is_Empty;

   -------------------------
   -- Is_Greater_Key_Node --
   -------------------------

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Type) return Boolean
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
      Right : Node_Type) return Boolean
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
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Local_Iterate is
         new Tree_Operations.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Count_Type) is
      begin
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      B : Natural renames Container'Unrestricted_Access.all.Busy;

   --  Start of processing for Iterate

   begin
      B := B + 1;

      begin
         Local_Iterate (Container);
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
      B  : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is 0 (as is the case here), this means the iterator object
      --  was constructed without a start expression. This is a complete
      --  iterator, meaning that the iteration starts from the (logical)
      --  beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
                    (Limited_Controlled with
                       Container => Container'Unrestricted_Access,
                       Node      => 0)
      do
         B := B + 1;
      end return;
   end Iterate;

   function Iterate
     (Container : Map;
      Start     : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
   is
      B  : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      --  Iterator was defined to behave the same as for a complete iterator,
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

      pragma Assert (Vet (Container, Start.Node),
                     "Start cursor of Iterate is bad");

      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is positive (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. (Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.)

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
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of function Key equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "Position cursor of function Key is bad");

      return Position.Container.Nodes (Position.Node).Key;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
   begin
      if Container.Last = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.Last);
      end if;
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Node component is 0, this means the iterator object was
      --  constructed without a start expression, in which case the (reverse)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is positive, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (reverse) partial iteration begins.

      if Object.Node = 0 then
         return Bounded_Ordered_Maps.Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Map) return Element_Type is
   begin
      if Container.Last = 0 then
         raise Constraint_Error with "map is empty";
      else
         return Container.Nodes (Container.Last).Element;
      end if;
   end Last_Element;

   --------------
   -- Last_Key --
   --------------

   function Last_Key (Container : Map) return Key_Type is
   begin
      if Container.Last = 0 then
         raise Constraint_Error with "map is empty";
      else
         return Container.Nodes (Container.Last).Key;
      end if;
   end Last_Key;

   ----------
   -- Left --
   ----------

   function Left (Node : Node_Type) return Count_Type is
   begin
      return Node.Left;
   end Left;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Target.Assign (Source);
      Source.Clear;
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

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "Position cursor of Next is bad");

      declare
         M : Map renames Position.Container.all;

         Node : constant Count_Type :=
                  Tree_Operations.Next (M, Position.Node);

      begin
         if Node = 0 then
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

      if Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong map";
      end if;

      return Next (Position);
   end Next;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Node_Type) return Count_Type is
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

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "Position cursor of Previous is bad");

      declare
         M : Map renames Position.Container.all;

         Node : constant Count_Type :=
                  Tree_Operations.Previous (M, Position.Node);

      begin
         if Node = 0 then
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
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "Position cursor of Query_Element is bad");

      declare
         M : Map renames Position.Container.all;
         N : Node_Type renames M.Nodes (Position.Node);

         B : Natural renames M.Busy;
         L : Natural renames M.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (N.Key, N.Element);
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
      procedure Read_Element (Node : in out Node_Type);
      pragma Inline (Read_Element);

      procedure Allocate is
         new Tree_Operations.Generic_Allocate (Read_Element);

      procedure Read_Elements is
         new Tree_Operations.Generic_Read (Allocate);

      ------------------
      -- Read_Element --
      ------------------

      procedure Read_Element (Node : in out Node_Type) is
      begin
         Key_Type'Read (Stream, Node.Key);
         Element_Type'Read (Stream, Node.Element);
      end Read_Element;

   --  Start of processing for Read

   begin
      Read_Elements (Stream, Container);
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

      pragma Assert (Vet (Container, Position.Node),
                     "Position cursor in function Reference is bad");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
      begin
         return (Element => N.Element'Access);
      end;
   end Reference;

   function Reference
     (Container : aliased in out Map;
      Key       : Key_Type) return Reference_Type
   is
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if Node = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      declare
         N : Node_Type renames Container.Nodes (Node);
      begin
         return (Element => N.Element'Access);
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
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if Node = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (map is locked)";
      end if;

      declare
         N : Node_Type renames Container.Nodes (Node);

      begin
         N.Key := Key;
         N.Element := New_Item;
      end;
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
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Replace_Element equals No_Element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Replace_Element designates wrong map";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (map is locked)";
      end if;

      pragma Assert (Vet (Container, Position.Node),
                     "Position cursor of Replace_Element is bad");

      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Local_Reverse_Iterate is
         new Tree_Operations.Generic_Reverse_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Count_Type) is
      begin
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      B : Natural renames Container'Unrestricted_Access.all.Busy;

   --  Start of processing for Reverse_Iterate

   begin
      B := B + 1;

      begin
         Local_Reverse_Iterate (Container);
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

   function Right (Node : Node_Type) return Count_Type is
   begin
      return Node.Right;
   end Right;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Node  : in out Node_Type;
      Color : Color_Type)
   is
   begin
      Node.Color := Color;
   end Set_Color;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (Node : in out Node_Type; Left : Count_Type) is
   begin
      Node.Left := Left;
   end Set_Left;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Node : in out Node_Type; Parent : Count_Type) is
   begin
      Node.Parent := Parent;
   end Set_Parent;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (Node : in out Node_Type; Right : Count_Type) is
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
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Update_Element equals No_Element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor of Update_Element designates wrong map";
      end if;

      pragma Assert (Vet (Container, Position.Node),
                     "Position cursor of Update_Element is bad");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         B : Natural renames Container.Busy;
         L : Natural renames Container.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (N.Key, N.Element);

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
         Node   : Node_Type);
      pragma Inline (Write_Node);

      procedure Write_Nodes is
         new Tree_Operations.Generic_Write (Write_Node);

      ----------------
      -- Write_Node --
      ----------------

      procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type)
      is
      begin
         Key_Type'Write (Stream, Node.Key);
         Element_Type'Write (Stream, Node.Element);
      end Write_Node;

   --  Start of processing for Write

   begin
      Write_Nodes (Stream, Container);
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

end Ada.Containers.Bounded_Ordered_Maps;
