------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . F O R M A L _ O R D E R E D _ M A P S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2021, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys);

with System; use type System.Address;

package body Ada.Containers.Formal_Ordered_Maps with
  SPARK_Mode => Off
is

   -----------------------------
   -- Node Access Subprograms --
   -----------------------------

   --  These subprograms provide a functional interface to access fields
   --  of a node, and a procedural interface for modifying these values.

   function Color
     (Node : Node_Type) return Ada.Containers.Red_Black_Trees.Color_Type;
   pragma Inline (Color);

   function Left_Son (Node : Node_Type) return Count_Type;
   pragma Inline (Left_Son);

   function Parent (Node : Node_Type) return Count_Type;
   pragma Inline (Parent);

   function Right_Son (Node : Node_Type) return Count_Type;
   pragma Inline (Right_Son);

   procedure Set_Color
     (Node  : in out Node_Type;
      Color : Ada.Containers.Red_Black_Trees.Color_Type);
   pragma Inline (Set_Color);

   procedure Set_Left (Node : in out Node_Type; Left : Count_Type);
   pragma Inline (Set_Left);

   procedure Set_Right (Node : in out Node_Type; Right : Count_Type);
   pragma Inline (Set_Right);

   procedure Set_Parent (Node : in out Node_Type; Parent : Count_Type);
   pragma Inline (Set_Parent);

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  All need comments ???

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (Tree : in out Tree_Types.Tree_Type'Class;
      Node : out Count_Type);

   procedure Free (Tree : in out Map; X : Count_Type);

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
     new Red_Black_Trees.Generic_Bounded_Operations
       (Tree_Types => Tree_Types,
        Left       => Left_Son,
        Right      => Right_Son);

   use Tree_Operations;

   package Key_Ops is
     new Red_Black_Trees.Generic_Bounded_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Key_Type,
        Is_Less_Key_Node    => Is_Less_Key_Node,
        Is_Greater_Key_Node => Is_Greater_Key_Node);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
      Lst   : Count_Type;
      Node  : Count_Type;
      ENode : Count_Type;

   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      if Is_Empty (Left) then
         return True;
      end if;

      Lst := Next (Left.Content, Last (Left).Node);

      Node := First (Left).Node;
      while Node /= Lst loop
         ENode := Find (Right, Left.Content.Nodes (Node).Key).Node;

         if ENode = 0 or else
           Left.Content.Nodes (Node).Element /=
           Right.Content.Nodes (ENode).Element
         then
            return False;
         end if;

         Node := Next (Left.Content, Node);
      end loop;

      return True;
   end "=";

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
         SN : Node_Type renames Source.Content.Nodes (Source_Node);

         procedure Set_Element (Node : in out Node_Type);
         pragma Inline (Set_Element);

         function New_Node return Count_Type;
         pragma Inline (New_Node);

         procedure Insert_Post is new Key_Ops.Generic_Insert_Post (New_Node);

         procedure Unconditional_Insert_Sans_Hint is
           new Key_Ops.Generic_Unconditional_Insert (Insert_Post);

         procedure Unconditional_Insert_Avec_Hint is
           new Key_Ops.Generic_Unconditional_Insert_With_Hint
             (Insert_Post,
              Unconditional_Insert_Sans_Hint);

         procedure Allocate is new Generic_Allocate (Set_Element);

         --------------
         -- New_Node --
         --------------

         function New_Node return Count_Type is
            Result : Count_Type;
         begin
            Allocate (Target.Content, Result);
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
           (Tree  => Target.Content,
            Hint  => 0,
            Key   => SN.Key,
            Node  => Target_Node);
      end Append_Element;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Storage_Error with "not enough capacity";  -- SE or CE? ???
      end if;

      Tree_Operations.Clear_Tree (Target.Content);
      Append_Elements (Source.Content);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Ceiling (Container.Content, Key);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      return (Node => Node);
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      Tree_Operations.Clear_Tree (Container.Content);
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
      Position  : Cursor) return not null access constant Element_Type
   is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "bad cursor in function Constant_Reference");

      return Container.Content.Nodes (Position.Node).Element'Access;
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Map;
      Key       : Key_Type) return not null access constant Element_Type
   is
      Node : constant Node_Access := Find (Container, Key).Node;

   begin
      if Node = 0 then
         raise Constraint_Error with
           "no element available because key not in map";
      end if;

      return Container.Content.Nodes (Node).Element'Access;
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
      Node : Count_Type := 1;
      N    : Count_Type;

   begin
      if 0 < Capacity and then Capacity < Source.Capacity then
         raise Capacity_Error;
      end if;

      return Target : Map (Count_Type'Max (Source.Capacity, Capacity)) do
         if Length (Source) > 0 then
            Target.Content.Length := Source.Content.Length;
            Target.Content.Root := Source.Content.Root;
            Target.Content.First := Source.Content.First;
            Target.Content.Last := Source.Content.Last;
            Target.Content.Free := Source.Content.Free;

            while Node <= Source.Capacity loop
               Target.Content.Nodes (Node).Element :=
                 Source.Content.Nodes (Node).Element;
               Target.Content.Nodes (Node).Key :=
                 Source.Content.Nodes (Node).Key;
               Target.Content.Nodes (Node).Parent :=
                 Source.Content.Nodes (Node).Parent;
               Target.Content.Nodes (Node).Left :=
                 Source.Content.Nodes (Node).Left;
               Target.Content.Nodes (Node).Right :=
                 Source.Content.Nodes (Node).Right;
               Target.Content.Nodes (Node).Color :=
                 Source.Content.Nodes (Node).Color;
               Target.Content.Nodes (Node).Has_Element :=
                 Source.Content.Nodes (Node).Has_Element;
               Node := Node + 1;
            end loop;

            while Node <= Target.Capacity loop
               N := Node;
               Free (Tree => Target, X => N);
               Node := Node + 1;
            end loop;
         end if;
      end return;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Delete has no element";
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "Position cursor of Delete is bad");

      Tree_Operations.Delete_Node_Sans_Free (Container.Content,
                                             Position.Node);
      Free (Container, Position.Node);
      Position := No_Element;
   end Delete;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : constant Node_Access := Key_Ops.Find (Container.Content, Key);

   begin
      if X = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Container.Content, X);
      Free (Container, X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Map) is
      X : constant Node_Access := First (Container).Node;
   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container.Content, X);
         Free (Container, X);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Map) is
      X : constant Node_Access := Last (Container).Node;
   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container.Content, X);
         Free (Container, X);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (Container : Map; Position : Cursor) return Element_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of function Element has no element";
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "Position cursor of function Element is bad");

      return Container.Content.Nodes (Position.Node).Element;

   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Node_Access := Find (Container, Key).Node;

   begin
      if Node = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      return Container.Content.Nodes (Node).Element;
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
      X : constant Node_Access := Key_Ops.Find (Container.Content, Key);
   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container.Content, X);
         Free (Container, X);
      end if;
   end Exclude;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Find (Container.Content, Key);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      return (Node => Node);
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
   begin
      if Length (Container) = 0 then
         return No_Element;
      end if;

      return (Node => Container.Content.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Map) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "map is empty";
      end if;

      return Container.Content.Nodes (First (Container).Node).Element;
   end First_Element;

   ---------------
   -- First_Key --
   ---------------

   function First_Key (Container : Map) return Key_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "map is empty";
      end if;

      return Container.Content.Nodes (First (Container).Node).Key;
   end First_Key;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Floor (Container.Content, Key);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      return (Node => Node);
   end Floor;

   ------------------
   -- Formal_Model --
   ------------------

   package body Formal_Model is

      ----------
      -- Find --
      ----------

      function Find
        (Container : K.Sequence;
         Key       : Key_Type) return Count_Type
      is
      begin
         for I in 1 .. K.Length (Container) loop
            if Equivalent_Keys (Key, K.Get (Container, I)) then
               return I;
            elsif Key < K.Get (Container, I) then
               return 0;
            end if;
         end loop;
         return 0;
      end Find;

      -------------------------
      -- K_Bigger_Than_Range --
      -------------------------

      function K_Bigger_Than_Range
        (Container : K.Sequence;
         Fst       : Positive_Count_Type;
         Lst       : Count_Type;
         Key       : Key_Type) return Boolean
      is
      begin
         for I in Fst .. Lst loop
            if not (K.Get (Container, I) < Key) then
               return False;
            end if;
         end loop;
         return True;
      end K_Bigger_Than_Range;

      ---------------
      -- K_Is_Find --
      ---------------

      function K_Is_Find
        (Container : K.Sequence;
         Key       : Key_Type;
         Position  : Count_Type) return Boolean
      is
      begin
         for I in 1 .. Position - 1 loop
            if Key < K.Get (Container, I) then
               return False;
            end if;
         end loop;

         if Position < K.Length (Container) then
            for I in Position + 1 .. K.Length (Container) loop
               if K.Get (Container, I) < Key then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end K_Is_Find;

      --------------------------
      -- K_Smaller_Than_Range --
      --------------------------

      function K_Smaller_Than_Range
        (Container : K.Sequence;
         Fst       : Positive_Count_Type;
         Lst       : Count_Type;
         Key       : Key_Type) return Boolean
      is
      begin
         for I in Fst .. Lst loop
            if not (Key < K.Get (Container, I)) then
               return False;
            end if;
         end loop;
         return True;
      end K_Smaller_Than_Range;

      ----------
      -- Keys --
      ----------

      function Keys (Container : Map) return K.Sequence is
         Position : Count_Type := Container.Content.First;
         R        : K.Sequence;

      begin
         --  Can't use First, Next or Element here, since they depend on models
         --  for their postconditions.

         while Position /= 0 loop
            R := K.Add (R, Container.Content.Nodes (Position).Key);
            Position := Tree_Operations.Next (Container.Content, Position);
         end loop;

         return R;
      end Keys;

      ----------------------------
      -- Lift_Abstraction_Level --
      ----------------------------

      procedure Lift_Abstraction_Level (Container : Map) is null;

      -----------
      -- Model --
      -----------

      function Model (Container : Map) return M.Map is
         Position : Count_Type := Container.Content.First;
         R        : M.Map;

      begin
         --  Can't use First, Next or Element here, since they depend on models
         --  for their postconditions.

         while Position /= 0 loop
            R :=
              M.Add
                (Container => R,
                 New_Key   => Container.Content.Nodes (Position).Key,
                 New_Item  => Container.Content.Nodes (Position).Element);

            Position := Tree_Operations.Next (Container.Content, Position);
         end loop;

         return R;
      end Model;

      -------------------------
      -- P_Positions_Shifted --
      -------------------------

      function P_Positions_Shifted
        (Small : P.Map;
         Big   : P.Map;
         Cut   : Positive_Count_Type;
         Count : Count_Type := 1) return Boolean
      is
      begin
         for Cu of Small loop
            if not P.Has_Key (Big, Cu) then
               return False;
            end if;
         end loop;

         for Cu of Big loop
            declare
               Pos : constant Positive_Count_Type := P.Get (Big, Cu);

            begin
               if Pos < Cut then
                  if not P.Has_Key (Small, Cu)
                    or else Pos /= P.Get (Small, Cu)
                  then
                     return False;
                  end if;

               elsif Pos >= Cut + Count then
                  if not P.Has_Key (Small, Cu)
                    or else Pos /= P.Get (Small, Cu) + Count
                  then
                     return False;
                  end if;

               else
                  if P.Has_Key (Small, Cu) then
                     return False;
                  end if;
               end if;
            end;
         end loop;

         return True;
      end P_Positions_Shifted;

      ---------------
      -- Positions --
      ---------------

      function Positions (Container : Map) return P.Map is
         I        : Count_Type := 1;
         Position : Count_Type := Container.Content.First;
         R        : P.Map;

      begin
         --  Can't use First, Next or Element here, since they depend on models
         --  for their postconditions.

         while Position /= 0 loop
            R := P.Add (R, (Node => Position), I);
            pragma Assert (P.Length (R) = I);
            Position := Tree_Operations.Next (Container.Content, Position);
            I := I + 1;
         end loop;

         return R;
      end Positions;

   end Formal_Model;

   ----------
   -- Free --
   ----------

   procedure Free
     (Tree : in out Map;
      X  : Count_Type)
   is
   begin
      Tree.Content.Nodes (X).Has_Element := False;
      Tree_Operations.Free (Tree.Content, X);
   end Free;

   ----------------------
   -- Generic_Allocate --
   ----------------------

   procedure Generic_Allocate
     (Tree : in out Tree_Types.Tree_Type'Class;
      Node : out Count_Type)
   is
      procedure Allocate is
        new Tree_Operations.Generic_Allocate (Set_Element);
   begin
      Allocate (Tree, Node);
      Tree.Nodes (Node).Has_Element := True;
   end Generic_Allocate;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : Map; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return False;
      end if;

      return Container.Content.Nodes (Position.Node).Has_Element;
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
         declare
            N : Node_Type renames Container.Content.Nodes (Position.Node);
         begin
            N.Key := Key;
            N.Element := New_Item;
         end;
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
      --  Comment ???

      procedure Insert_Post is
        new Key_Ops.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Key_Ops.Generic_Conditional_Insert (Insert_Post);

      --------------
      -- New_Node --
      --------------

      function New_Node return Node_Access is
         procedure Initialize (Node : in out Node_Type);
         procedure Allocate_Node is new Generic_Allocate (Initialize);

         procedure Initialize (Node : in out Node_Type) is
         begin
            Node.Key := Key;
            Node.Element := New_Item;
         end Initialize;

         X : Node_Access;

      begin
         Allocate_Node (Container.Content, X);
         return X;
      end New_Node;

   --  Start of processing for Insert

   begin
      Insert_Sans_Hint
        (Container.Content,
         Key,
         Position.Node,
         Inserted);
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
         raise Constraint_Error with "key already in map";
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Length (Container) = 0;
   end Is_Empty;

   -------------------------
   -- Is_Greater_Key_Node --
   -------------------------

   function Is_Greater_Key_Node
     (Left  : Key_Type;
      Right : Node_Type) return Boolean
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
      Right : Node_Type) return Boolean
   is
   begin
      return Left < Right.Key;
   end Is_Less_Key_Node;

   ---------
   -- Key --
   ---------

   function Key (Container : Map; Position : Cursor) return Key_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of function Key has no element";
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "Position cursor of function Key is bad");

      return Container.Content.Nodes (Position.Node).Key;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
   begin
      if Length (Container) = 0 then
         return No_Element;
      end if;

      return (Node => Container.Content.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Map) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "map is empty";
      end if;

      return Container.Content.Nodes (Last (Container).Node).Element;
   end Last_Element;

   --------------
   -- Last_Key --
   --------------

   function Last_Key (Container : Map) return Key_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "map is empty";
      end if;

      return Container.Content.Nodes (Last (Container).Node).Key;
   end Last_Key;

   --------------
   -- Left_Son --
   --------------

   function Left_Son (Node : Node_Type) return Count_Type is
   begin
      return Node.Left;
   end Left_Son;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Content.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Map; Source : in out Map) is
      NN : Tree_Types.Nodes_Type renames Source.Content.Nodes;
      X  : Node_Access;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      loop
         X := First (Source).Node;
         exit when X = 0;

         --  Here we insert a copy of the source element into the target, and
         --  then delete the element from the source. Another possibility is
         --  that delete it first (and hang onto its index), then insert it.
         --  ???

         Insert (Target, NN (X).Key, NN (X).Element);  -- optimize???

         Tree_Operations.Delete_Node_Sans_Free (Source.Content, X);
         Formal_Ordered_Maps.Free (Source, X);
      end loop;
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Container : Map; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   function Next (Container : Map; Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "bad cursor in Next");

      return (Node => Tree_Operations.Next (Container.Content, Position.Node));
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

   procedure Previous (Container : Map; Position : in out Cursor) is
   begin
      Position := Previous (Container, Position);
   end Previous;

   function Previous (Container : Map; Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "bad cursor in Previous");

      declare
         Node : constant Count_Type :=
           Tree_Operations.Previous (Container.Content, Position.Node);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Previous;

   --------------
   -- Reference --
   --------------

   function Reference
     (Container : not null access Map;
      Position  : Cursor) return not null access Element_Type
   is
   begin
      if not Has_Element (Container.all, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert
        (Vet (Container.Content, Position.Node),
         "bad cursor in function Reference");

      return Container.Content.Nodes (Position.Node).Element'Access;
   end Reference;

   function Reference
     (Container : not null access Map;
      Key       : Key_Type) return not null access Element_Type
   is
      Node : constant Count_Type := Find (Container.all, Key).Node;

   begin
      if Node = 0 then
         raise Constraint_Error with
           "no element available because key not in map";
      end if;

      return Container.Content.Nodes (Node).Element'Access;
   end Reference;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
   begin
      declare
         Node : constant Node_Access := Key_Ops.Find (Container.Content, Key);

      begin
         if Node = 0 then
            raise Constraint_Error with "key not in map";
         end if;

         declare
            N : Node_Type renames Container.Content.Nodes (Node);
         begin
            N.Key := Key;
            N.Element := New_Item;
         end;
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
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Replace_Element has no element";
      end if;

      pragma Assert (Vet (Container.Content, Position.Node),
                     "Position cursor of Replace_Element is bad");

      Container.Content.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   ---------------
   -- Right_Son --
   ---------------

   function Right_Son (Node : Node_Type) return Count_Type is
   begin
      return Node.Right;
   end Right_Son;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Node  : in out Node_Type; Color : Color_Type) is
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

end Ada.Containers.Formal_Ordered_Maps;
