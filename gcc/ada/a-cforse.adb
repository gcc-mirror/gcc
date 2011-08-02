------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . F O R M A L _ O R D E R E D _ S E T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010, Free Software Foundation, Inc.              --
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

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations);

with System;  use type System.Address;

package body Ada.Containers.Formal_Ordered_Sets is

   ------------------------------
   -- Access to Fields of Node --
   ------------------------------

   --  These subprograms provide functional notation for access to fields
   --  of a node, and procedural notation for modifiying these fields.

   function Color (Node : Node_Type) return Red_Black_Trees.Color_Type;
   pragma Inline (Color);

   function Left_Son (Node : Node_Type) return Count_Type;
   pragma Inline (Left);

   function Parent (Node : Node_Type) return Count_Type;
   pragma Inline (Parent);

   function Right_Son (Node : Node_Type) return Count_Type;
   pragma Inline (Right);

   procedure Set_Color
     (Node  : in out Node_Type;
      Color : Red_Black_Trees.Color_Type);
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

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (Tree : in out Tree_Types.Tree_Type'Class;
      Node : out Count_Type);

   procedure Assign (Target : in out Tree_Types.Tree_Type;
                     Source : Tree_Types.Tree_Type);

   procedure Clear (Container : in out Tree_Types.Tree_Type);

   procedure Free (Tree : in out Tree_Types.Tree_Type; X : Count_Type);

   procedure Insert_Sans_Hint
     (Container : in out Tree_Types.Tree_Type;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean);

   procedure Insert_With_Hint
     (Dst_Set  : in out Tree_Types.Tree_Type;
      Dst_Hint : Count_Type;
      Src_Node : Node_Type;
      Dst_Node : out Count_Type);

   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Type) return Boolean;
   pragma Inline (Is_Greater_Element_Node);

   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Type) return Boolean;
   pragma Inline (Is_Less_Element_Node);

   function Is_Less_Node_Node (L, R : Node_Type) return Boolean;
   pragma Inline (Is_Less_Node_Node);

   generic
      with procedure Process (Node : Count_Type) is <>;
   procedure Iterate_Between (Tree : Tree_Types.Tree_Type;
                              From : Count_Type;
                              To   : Count_Type);

   function Next_Unchecked
     (Container : Set;
      Position  : Count_Type) return Count_Type;

   procedure Replace_Element
     (Tree : in out Tree_Types.Tree_Type;
      Node : Count_Type;
      Item : Element_Type);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package Tree_Operations is
     new Red_Black_Trees.Generic_Bounded_Operations
       (Tree_Types,
        Left      => Left_Son,
        Right     => Right_Son);

   use Tree_Operations;

   package Element_Keys is
     new Red_Black_Trees.Generic_Bounded_Keys
       (Tree_Operations     => Tree_Operations,
        Key_Type            => Element_Type,
        Is_Less_Key_Node    => Is_Less_Element_Node,
        Is_Greater_Key_Node => Is_Greater_Element_Node);

   package Set_Ops is
     new Red_Black_Trees.Generic_Bounded_Set_Operations
       (Tree_Operations  => Tree_Operations,
        Set_Type         => Tree_Types.Tree_Type,
        Assign           => Assign,
        Insert_With_Hint => Insert_With_Hint,
        Is_Less          => Is_Less_Node_Node);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
      Lst   : Count_Type;
      Node  : Count_Type := First (Left).Node;
      ENode : Count_Type;
   begin

      if Length (Left) /= Length (Right) then
         return False;
      end if;

      if Is_Empty (Left) then
         return True;
      end if;

      Lst := Next (Left.Tree.all, Last (Left).Node);
      while Node /= Lst loop
         ENode := Find (Right, Left.Tree.Nodes (Node).Element).Node;
         if ENode = 0 or else
           Left.Tree.Nodes (Node).Element /= Right.Tree.Nodes (ENode).Element
         then
            return False;
         end if;
         Node := Next (Left.Tree.all, Node);
      end loop;

      return True;

   end "=";

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Tree_Types.Tree_Type;
                     Source : Tree_Types.Tree_Type) is
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
           new Element_Keys.Generic_Insert_Post (New_Node);

         procedure Unconditional_Insert_Sans_Hint is
           new Element_Keys.Generic_Unconditional_Insert (Insert_Post);

         procedure Unconditional_Insert_Avec_Hint is
           new Element_Keys.Generic_Unconditional_Insert_With_Hint
             (Insert_Post,
              Unconditional_Insert_Sans_Hint);

         procedure Allocate is
           new Generic_Allocate (Set_Element);

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
            Node.Element := SN.Element;
         end Set_Element;

         Target_Node : Count_Type;

         --  Start of processing for Append_Element

      begin
         Unconditional_Insert_Avec_Hint
           (Tree  => Target,
            Hint  => 0,
            Key   => SN.Element,
            Node  => Target_Node);
      end Append_Element;

      --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error
           with "Target capacity is less than Source length";
      end if;

      Tree_Operations.Clear_Tree (Target);
      Append_Elements (Source);
   end Assign;

   procedure Assign (Target : in out Set; Source : Set) is
      X : Count_Type;
   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Storage_Error with "not enough capacity";  -- SE or CE? ???
      end if;

      if Source.K = Plain then
         Assign (Target => Target.Tree.all, Source => Source.Tree.all);
      else
         declare
            procedure Append_Element (Source_Node : Count_Type);

            procedure Append_Element (Source_Node : Count_Type) is
               SN : Node_Type renames Source.Tree.Nodes (Source_Node);

               procedure Set_Element (Node : in out Node_Type);
               pragma Inline (Set_Element);

               function New_Node return Count_Type;
               pragma Inline (New_Node);

               procedure Insert_Post is
                 new Element_Keys.Generic_Insert_Post (New_Node);

               procedure Unconditional_Insert_Sans_Hint is
                 new Element_Keys.Generic_Unconditional_Insert (Insert_Post);

               procedure Unconditional_Insert_Avec_Hint is
                 new Element_Keys.Generic_Unconditional_Insert_With_Hint
                   (Insert_Post,
                    Unconditional_Insert_Sans_Hint);

               procedure Allocate is
                 new Generic_Allocate (Set_Element);

               --------------
               -- New_Node --
               --------------

               function New_Node return Count_Type is
                  Result : Count_Type;

               begin
                  Allocate (Target.Tree.all, Result);
                  return Result;
               end New_Node;

               -----------------
               -- Set_Element --
               -----------------

               procedure Set_Element (Node : in out Node_Type) is
               begin
                  Node.Element := SN.Element;
               end Set_Element;

               Target_Node : Count_Type;

               --  Start of processing for Append_Element

            begin
               Unconditional_Insert_Avec_Hint
                 (Tree  => Target.Tree.all,
                  Hint  => 0,
                  Key   => SN.Element,
                  Node  => Target_Node);
            end Append_Element;
         begin
            Tree_Operations.Clear_Tree (Target.Tree.all);
            X := Source.First;
            while X /= Next (Source.Tree.all, Source.Last) loop
               Append_Element (X);
               X := Next (Source.Tree.all, X);
            end loop;
         end;
      end if;
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
   begin

      if Container.K = Part then
         if Container.Length = 0 then
            return No_Element;
         end if;

         if Item < Container.Tree.Nodes (Container.First).Element then
            return (Node => Container.First);
         end if;

         if Container.Tree.Nodes (Container.Last).Element < Item then
            return No_Element;
         end if;
      end if;

      declare
         Node : constant Count_Type :=
           Element_Keys.Ceiling (Container.Tree.all, Item);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Tree_Types.Tree_Type) is
   begin
      Tree_Operations.Clear_Tree (Container);
   end Clear;

   procedure Clear (Container : in out Set) is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      Clear (Container.Tree.all);
   end Clear;

   -----------
   -- Color --
   -----------

   function Color (Node : Node_Type) return Red_Black_Trees.Color_Type is
   begin
      return Node.Color;
   end Color;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Set;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Set; Capacity : Count_Type := 0) return Set is
      Node : Count_Type := 1;
      N    : Count_Type;
      Cu   : Cursor;
      Target : Set (Count_Type'Max (Source.Capacity, Capacity));
   begin
      if Length (Source) > 0 then
         Target.Tree.Length := Source.Tree.Length;
         Target.Tree.Root := Source.Tree.Root;
         Target.Tree.First := Source.Tree.First;
         Target.Tree.Last := Source.Tree.Last;
         Target.Tree.Free := Source.Tree.Free;

         while Node <= Source.Capacity loop
            Target.Tree.Nodes (Node).Element :=
              Source.Tree.Nodes (Node).Element;
            Target.Tree.Nodes (Node).Parent :=
              Source.Tree.Nodes (Node).Parent;
            Target.Tree.Nodes (Node).Left :=
              Source.Tree.Nodes (Node).Left;
            Target.Tree.Nodes (Node).Right :=
              Source.Tree.Nodes (Node).Right;
            Target.Tree.Nodes (Node).Color :=
              Source.Tree.Nodes (Node).Color;
            Target.Tree.Nodes (Node).Has_Element :=
              Source.Tree.Nodes (Node).Has_Element;
            Node := Node + 1;
         end loop;

         while Node <= Target.Capacity loop
            N := Node;
            Formal_Ordered_Sets.Free (Tree => Target.Tree.all, X => N);
            Node := Node + 1;
         end loop;

         if Source.K = Part then
            Node := Target.Tree.First;
            while Node /= Source.First loop
               Cu := (Node => Node);
               Node := Next (Target.Tree.all, Node);
               Delete (Target, Cu);
            end loop;

            Node := Next (Target.Tree.all, Source.Last);

            while Node /= 0 loop
               Cu := (Node => Node);
               Node := Next (Target.Tree.all, Node);
               Delete (Target, Cu);
            end loop;
         end if;
         Node := 1;

      end if;
      return Target;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Set; Position : in out Cursor) is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Delete");

      Tree_Operations.Delete_Node_Sans_Free (Container.Tree.all,
                                             Position.Node);
      Formal_Ordered_Sets.Free (Container.Tree.all, Position.Node);
      Position := No_Element;
   end Delete;

   procedure Delete (Container : in out Set; Item : Element_Type) is
      X : constant Count_Type := Element_Keys.Find (Container.Tree.all, Item);

   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if X = 0 then
         raise Constraint_Error with "attempt to delete element not in set";
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Container.Tree.all, X);
      Formal_Ordered_Sets.Free (Container.Tree.all, X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Set) is
      Tree : Tree_Types.Tree_Type renames Container.Tree.all;
      X    : constant Count_Type := Tree.First;

   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Formal_Ordered_Sets.Free (Tree, X);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Set) is
      Tree : Tree_Types.Tree_Type renames Container.Tree.all;
      X    : constant Count_Type := Tree.Last;

   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Tree, X);
         Formal_Ordered_Sets.Free (Tree, X);
      end if;
   end Delete_Last;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Set; Source : Set) is
   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Source.K = Plain then
         Set_Ops.Set_Difference (Target.Tree.all, Source.Tree.all);
      else
         declare
            Tgt : Count_Type := Target.Tree.First;
            Src : Count_Type := Source.First;
         begin
            if Target'Address = Source'Address then
               if Target.Tree.Busy > 0 then
                  raise Program_Error with
                    "attempt to tamper with cursors (container is busy)";
               end if;

               Clear (Target.Tree.all);
               return;
            end if;

            if Source.Length = 0 then
               return;
            end if;

            if Target.Tree.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (container is busy)";
            end if;

            loop
               if Tgt = 0 then
                  return;
               end if;

               if Src = Next (Source.Tree.all, Source.Last) then
                  return;
               end if;

               if Target.Tree.Nodes (Tgt).Element <
                 Source.Tree.Nodes (Src).Element then
                  Tgt := Next (Target.Tree.all, Tgt);

               elsif Source.Tree.Nodes (Src).Element <
                 Target.Tree.Nodes (Tgt).Element then
                  Src := Next (Source.Tree.all, Src);

               else
                  declare
                     X : constant Count_Type := Tgt;
                  begin
                     Tgt := Next (Target.Tree.all, Tgt);
                     Delete_Node_Sans_Free (Target.Tree.all, X);
                     Formal_Ordered_Sets.Free (Target.Tree.all, X);
                  end;

                  Src := Next (Source.Tree.all, Src);
               end if;
            end loop;
         end;
      end if;
   end Difference;

   function Difference (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Length (Left) = 0 then
         return Empty_Set;
      end if;

      if Length (Right) = 0 then
         return Left.Copy;
      end if;

      return S : Set (Length (Left)) do
         if Left.K = Plain and Right.K = Plain then
            Assign (S.Tree.all,
                    Set_Ops.Set_Difference (Left.Tree.all, Right.Tree.all));
         else
            declare
               Tree : Tree_Types.Tree_Type renames S.Tree.all;

               L_Node : Count_Type := First (Left).Node;
               R_Node : Count_Type := First (Right).Node;

               L_Last : constant Count_Type := Next (Left.Tree.all,
                                                     Last (Left).Node);
               R_Last : constant Count_Type := Next (Right.Tree.all,
                                                     Last (Right).Node);

               Dst_Node : Count_Type;

            begin
               loop
                  if L_Node = L_Last then
                     return;
                  end if;

                  if R_Node = R_Last then
                     while L_Node /= L_Last loop
                        Insert_With_Hint
                          (Dst_Set  => Tree,
                           Dst_Hint => 0,
                           Src_Node => Left.Tree.Nodes (L_Node),
                           Dst_Node => Dst_Node);

                        L_Node := Next (Left.Tree.all, L_Node);

                     end loop;

                     return;
                  end if;

                  if Left.Tree.Nodes (L_Node).Element <
                    Right.Tree.Nodes (R_Node).Element then
                     Insert_With_Hint
                       (Dst_Set  => Tree,
                        Dst_Hint => 0,
                        Src_Node => Left.Tree.Nodes (L_Node),
                        Dst_Node => Dst_Node);

                     L_Node := Next (Left.Tree.all, L_Node);

                  elsif Right.Tree.Nodes (R_Node).Element <
                    Left.Tree.Nodes (L_Node).Element then
                     R_Node := Next (Right.Tree.all, R_Node);

                  else
                     L_Node := Next (Left.Tree.all, L_Node);
                     R_Node := Next (Right.Tree.all, R_Node);
                  end if;
               end loop;
            end;
         end if;
      end return;
   end Difference;

   -------------
   -- Element --
   -------------

   function Element (Container : Set; Position : Cursor) return Element_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Element");

      declare
         N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
      begin
         return N (Position.Node).Element;
      end;
   end Element;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      if Left < Right
        or else Right < Left
      then
         return False;
      else
         return True;
      end if;
   end Equivalent_Elements;

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
      function Is_Equivalent_Node_Node
        (L, R        : Node_Type) return Boolean;
      pragma Inline (Is_Equivalent_Node_Node);

      function Is_Equivalent is
        new Tree_Operations.Generic_Equal (Is_Equivalent_Node_Node);

      -----------------------------
      -- Is_Equivalent_Node_Node --
      -----------------------------

      function Is_Equivalent_Node_Node (L, R : Node_Type) return Boolean is
      begin
         if L.Element < R.Element then
            return False;
         elsif R.Element < L.Element then
            return False;
         else
            return True;
         end if;
      end Is_Equivalent_Node_Node;

      --  Start of processing for Equivalent_Sets

   begin
      if Left.K = Plain and Right.K = Plain then
         return Is_Equivalent (Left.Tree.all, Right.Tree.all);
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      if Length (Left) /= Length (Right) then
         return False;
      end if;

      if Length (Left) = 0 then
         return True;
      end if;

      declare
         L_Node : Count_Type;
         R_Node : Count_Type;

         L_Last : constant Count_Type := Next (Left.Tree.all,
                                               Last (Left).Node);
      begin

         L_Node := First (Left).Node;
         R_Node := First (Right).Node;
         while L_Node /= L_Last loop
            if not Is_Equivalent_Node_Node (Left.Tree.Nodes (L_Node),
                                            Right.Tree.Nodes (R_Node)) then
               return False;
            end if;

            L_Node := Next (Left.Tree.all, L_Node);
            R_Node := Next (Right.Tree.all, R_Node);
         end loop;

         return True;
      end;
   end Equivalent_Sets;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      X : constant Count_Type := Element_Keys.Find (Container.Tree.all, Item);

   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container.Tree.all, X);
         Formal_Ordered_Sets.Free (Container.Tree.all, X);
      end if;
   end Exclude;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
   begin

      if Container.K = Part then
         if Container.Length = 0 then
            return No_Element;
         end if;

         if Item < Container.Tree.Nodes (Container.First).Element or
           Container.Tree.Nodes (Container.Last).Element < Item then
            return No_Element;
         end if;
      end if;

      declare
         Node : constant Count_Type :=
           Element_Keys.Find (Container.Tree.all, Item);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      if Length (Container) = 0 then
         return No_Element;
      end if;

      if Container.K = Plain then
         return (Node => Container.Tree.First);
      else
         return (Node => Container.First);
      end if;

   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Set) return Element_Type is
      Fst : constant Count_Type :=  First (Container).Node;
   begin
      if Fst = 0 then
         raise Constraint_Error with "set is empty";
      end if;

      declare
         N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
      begin
         return N (Fst).Element;
      end;
   end First_Element;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Set; Item : Element_Type) return Cursor is
   begin

      if Container.K = Part then
         if Container.Length = 0 then
            return No_Element;
         end if;

         if Item < Container.Tree.Nodes (Container.First).Element then
            return No_Element;
         end if;

         if Container.Tree.Nodes (Container.Last).Element < Item then
            return (Node => Container.Last);
         end if;
      end if;

      declare
         Node : constant Count_Type :=
           Element_Keys.Floor (Container.Tree.all, Item);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Floor;

   ----------
   -- Free --
   ----------

   procedure Free
     (Tree : in out Tree_Types.Tree_Type;
      X  : Count_Type)
   is
   begin
      Tree.Nodes (X).Has_Element := False;
      Tree_Operations.Free (Tree, X);
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

   ------------------
   -- Generic_Keys --
   ------------------

   package body Generic_Keys is

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

      package Key_Keys is
        new Red_Black_Trees.Generic_Bounded_Keys
          (Tree_Operations     => Tree_Operations,
           Key_Type            => Key_Type,
           Is_Less_Key_Node    => Is_Less_Key_Node,
           Is_Greater_Key_Node => Is_Greater_Key_Node);

      -------------
      -- Ceiling --
      -------------

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
      begin

         if Container.K = Part then
            if Container.Length = 0 then
               return No_Element;
            end if;

            if Key < Generic_Keys.Key
              (Container.Tree.Nodes (Container.First).Element) then
               return (Node => Container.First);
            end if;

            if Generic_Keys.Key
              (Container.Tree.Nodes (Container.Last).Element) < Key then
               return No_Element;
            end if;
         end if;

         declare
            Node : constant Count_Type :=
              Key_Keys.Ceiling (Container.Tree.all, Key);

         begin
            if Node = 0 then
               return No_Element;
            end if;

            return (Node => Node);
         end;
      end Ceiling;

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
      begin
         if Container.K /= Plain then
            raise Constraint_Error
              with "Can't modify part of container";
         end if;

         declare
            X : constant Count_Type := Key_Keys.Find (Container.Tree.all, Key);

         begin
            if X = 0 then
               raise Constraint_Error with "attempt to delete key not in set";
            end if;

            Delete_Node_Sans_Free (Container.Tree.all, X);
            Formal_Ordered_Sets.Free (Container.Tree.all, X);
         end;
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Container : Set; Key : Key_Type) return Element_Type is
      begin

         if Container.K = Part then
            if Container.Length = 0 or else
              (Key < Generic_Keys.Key
                 (Container.Tree.Nodes (Container.First).Element) or
                 Generic_Keys.Key
                   (Container.Tree.Nodes (Container.Last).Element) < Key) then
               raise Constraint_Error with "key not in set";
            end if;
         end if;

         declare
            Node : constant Count_Type :=
              Key_Keys.Find (Container.Tree.all, Key);

         begin
            if Node = 0 then
               raise Constraint_Error with "key not in set";
            end if;

            declare
               N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
            begin
               return N (Node).Element;
            end;
         end;
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

      procedure Exclude (Container : in out Set; Key : Key_Type) is
      begin
         if Container.K /= Plain then
            raise Constraint_Error
              with "Can't modify part of container";
         end if;

         declare

            X : constant Count_Type := Key_Keys.Find (Container.Tree.all, Key);

         begin
            if X /= 0 then
               Delete_Node_Sans_Free (Container.Tree.all, X);
               Formal_Ordered_Sets.Free (Container.Tree.all, X);
            end if;
         end;
      end Exclude;

      ----------
      -- Find --
      ----------

      function Find (Container : Set; Key : Key_Type) return Cursor is
      begin

         if Container.K = Part then
            if Container.Length = 0 or else
              (Key < Generic_Keys.Key
                 (Container.Tree.Nodes (Container.First).Element) or
                 Generic_Keys.Key
                   (Container.Tree.Nodes (Container.Last).Element) < Key) then
               return No_Element;
            end if;
         end if;

         declare

            Node : constant Count_Type := Key_Keys.Find (Container.Tree.all,
                                                         Key);

         begin
            if Node = 0 then
               return No_Element;
            end if;

            return (Node => Node);
         end;
      end Find;

      -----------
      -- Floor --
      -----------

      function Floor (Container : Set; Key : Key_Type) return Cursor is
      begin
         if Container.K = Part then
            if Container.Length = 0 or else
              Key < Generic_Keys.Key
                (Container.Tree.Nodes (Container.First).Element) then
               return No_Element;
            end if;

            if Generic_Keys.Key
              (Container.Tree.Nodes (Container.Last).Element) < Key then
               return (Node => Container.Last);
            end if;
         end if;

         declare
            Node : constant Count_Type :=
              Key_Keys.Floor (Container.Tree.all, Key);

         begin
            if Node = 0 then
               return No_Element;
            end if;

            return (Node => Node);
         end;
      end Floor;

      -------------------------
      -- Is_Greater_Key_Node --
      -------------------------

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Type) return Boolean
      is
      begin
         return Key (Right.Element) < Left;
      end Is_Greater_Key_Node;

      ----------------------
      -- Is_Less_Key_Node --
      ----------------------

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Type) return Boolean
      is
      begin
         return Left < Key (Right.Element);
      end Is_Less_Key_Node;

      ---------
      -- Key --
      ---------

      function Key (Container : Set; Position : Cursor) return Key_Type is
      begin
         if not Has_Element (Container, Position) then
            raise Constraint_Error with
              "Position cursor has no element";
         end if;

         pragma Assert (Vet (Container.Tree.all, Position.Node),
                        "bad cursor in Key");

         declare
            N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
         begin
            return Key (N (Position.Node).Element);
         end;
      end Key;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      is
         Node : constant Count_Type := Key_Keys.Find (Container.Tree.all, Key);

      begin
         if Container.K /= Plain then
            raise Constraint_Error
              with "Can't modify part of container";
         end if;

         if not Has_Element (Container, (Node => Node)) then
            raise Constraint_Error with
              "attempt to replace key not in set";
         end if;

         Replace_Element (Container.Tree.all, Node, New_Item);
      end Replace;

      -----------------------------------
      -- Update_Element_Preserving_Key --
      -----------------------------------

      procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access procedure (Element : in out Element_Type))
      is
         Tree : Tree_Types.Tree_Type renames Container.Tree.all;

      begin
         if Container.K /= Plain then
            raise Constraint_Error
              with "Can't modify part of container";
         end if;

         if not Has_Element (Container, Position) then
            raise Constraint_Error with
              "Position cursor has no element";
         end if;

         pragma Assert (Vet (Container.Tree.all, Position.Node),
                        "bad cursor in Update_Element_Preserving_Key");

         declare
            N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;

            E : Element_Type renames N (Position.Node).Element;
            K : constant Key_Type := Key (E);

            B : Natural renames Tree.Busy;
            L : Natural renames Tree.Lock;

         begin
            B := B + 1;
            L := L + 1;

            begin
               Process (E);
            exception
               when others =>
                  L := L - 1;
                  B := B - 1;
                  raise;
            end;

            L := L - 1;
            B := B - 1;

            if Equivalent_Keys (K, Key (E)) then
               return;
            end if;
         end;

         declare
            X : constant Count_Type := Position.Node;
         begin
            Tree_Operations.Delete_Node_Sans_Free (Tree, X);
            Formal_Ordered_Sets.Free (Tree, X);
         end;

         raise Program_Error with "key was modified";
      end Update_Element_Preserving_Key;

   end Generic_Keys;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : Set; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return False;
      end if;

      if not Container.Tree.Nodes (Position.Node).Has_Element then
         return False;
      end if;

      if Container.K = Plain then
         return True;
      end if;

      declare
         Elt : constant Element_Type :=
           Container.Tree.Nodes (Position.Node).Element;
      begin

         if Elt < Container.Tree.Nodes (Container.First).Element or
           Container.Tree.Nodes (Container.Last).Element < Elt then
            return False;
         end if;

         return True;
      end;
   end Has_Element;

   -------------
   -- Include --
   -------------

   procedure Include (Container : in out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         if Container.Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (set is locked)";
         end if;

         declare
            N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
         begin
            N (Position.Node).Element := New_Item;
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
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      Insert_Sans_Hint
        (Container.Tree.all,
         New_Item,
         Position.Node,
         Inserted);

   end Insert;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Position : Cursor;
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
     (Container : in out Tree_Types.Tree_Type;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean)
   is

      procedure Set_Element (Node : in out Node_Type);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Conditional_Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Allocate is
        new Generic_Allocate (Set_Element);

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
         Result : Count_Type;

      begin
         Allocate (Container, Result);
         return Result;
      end New_Node;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element (Node : in out Node_Type) is
      begin
         Node.Element := New_Item;
      end Set_Element;

      --  Start of processing for Insert_Sans_Hint

   begin
      Conditional_Insert_Sans_Hint
        (Container,
         New_Item,
         Node,
         Inserted);
   end Insert_Sans_Hint;

   ----------------------
   -- Insert_With_Hint --
   ----------------------

   procedure Insert_With_Hint
     (Dst_Set  : in out Tree_Types.Tree_Type;
      Dst_Hint : Count_Type;
      Src_Node : Node_Type;
      Dst_Node : out Count_Type)
   is
      Success : Boolean;
      pragma Unreferenced (Success);

      procedure Set_Element (Node : in out Node_Type);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Local_Insert_With_Hint is
        new Element_Keys.Generic_Conditional_Insert_With_Hint
          (Insert_Post,
           Insert_Sans_Hint);

      procedure Allocate is
        new Generic_Allocate (Set_Element);

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
         Result : Count_Type;

      begin
         Allocate (Dst_Set, Result);
         return Result;
      end New_Node;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element (Node : in out Node_Type) is
      begin
         Node.Element := Src_Node.Element;
      end Set_Element;

      --  Start of processing for Insert_With_Hint

   begin
      Local_Insert_With_Hint
        (Dst_Set,
         Dst_Hint,
         Src_Node.Element,
         Dst_Node,
         Success);
   end Insert_With_Hint;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Source.K = Plain then
         Set_Ops.Set_Intersection (Target.Tree.all, Source.Tree.all);
      else
         declare
            Tgt : Count_Type := Target.First;
            Src : Count_Type := Source.First;

            S_Last : constant Count_Type :=
              Next (Source.Tree.all, Source.Last);

         begin
            if Target'Address = Source'Address then
               return;
            end if;

            if Target.Tree.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (container is busy)";
            end if;

            if Source.Length = 0 then
               Clear (Target);
               return;
            end if;

            while Tgt /= 0
              and then Src /= S_Last
            loop
               if Target.Tree.Nodes (Tgt).Element <
                 Source.Tree.Nodes (Src).Element then
                  declare
                     X : constant Count_Type := Tgt;
                  begin
                     Tgt := Next (Target.Tree.all, Tgt);
                     Delete_Node_Sans_Free (Target.Tree.all, X);
                     Formal_Ordered_Sets.Free (Target.Tree.all, X);
                  end;

               elsif Source.Tree.Nodes (Src).Element <
                 Target.Tree.Nodes (Tgt).Element then
                  Src := Next (Source.Tree.all, Src);

               else
                  Tgt := Next (Target.Tree.all, Tgt);
                  Src := Next (Source.Tree.all, Src);
               end if;
            end loop;

            while Tgt /= 0 loop
               declare
                  X : constant Count_Type := Tgt;
               begin
                  Tgt := Next (Target.Tree.all, Tgt);
                  Delete_Node_Sans_Free (Target.Tree.all, X);
                  Formal_Ordered_Sets.Free (Target.Tree.all, X);
               end;
            end loop;
         end;
      end if;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Left.Copy;
      end if;

      return S : Set (Count_Type'Min (Length (Left), Length (Right))) do
         if Left.K = Plain and Right.K = Plain then
            Assign (S.Tree.all, Set_Ops.Set_Intersection
                    (Left.Tree.all, Right.Tree.all));
            return;
         end if;

         if Length (Left) = 0 or Length (Right) = 0 then
            return;
         end if;

         declare

            L_Node : Count_Type := First (Left).Node;
            R_Node : Count_Type := First (Right).Node;

            L_Last : constant Count_Type :=
              Next (Left.Tree.all, Last (Left).Node);
            R_Last : constant Count_Type :=
              Next (Right.Tree.all, Last (Right).Node);

            Dst_Node : Count_Type;

         begin
            loop

               if L_Node = L_Last or R_Node = R_Last then
                  return;
               end if;

               if Left.Tree.Nodes (L_Node).Element <
                 Right.Tree.Nodes (R_Node).Element then
                  L_Node := Next (Left.Tree.all, L_Node);

               elsif Right.Tree.Nodes (R_Node).Element <
                 Left.Tree.Nodes (L_Node).Element then
                  R_Node := Next (Right.Tree.all, R_Node);

               else
                  Insert_With_Hint
                    (Dst_Set  => S.Tree.all,
                     Dst_Hint => 0,
                     Src_Node => Left.Tree.Nodes (L_Node),
                     Dst_Node => Dst_Node);

                  L_Node := Next (Left.Tree.all, L_Node);
                  R_Node := Next (Right.Tree.all, R_Node);
               end if;
            end loop;
         end;
      end return;
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Length (Container) = 0;
   end Is_Empty;

   -----------------------------
   -- Is_Greater_Element_Node --
   -----------------------------

   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Type) return Boolean
   is
   begin
      --  Compute e > node same as node < e

      return Right.Element < Left;
   end Is_Greater_Element_Node;

   --------------------------
   -- Is_Less_Element_Node --
   --------------------------

   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Type) return Boolean
   is
   begin
      return Left < Right.Element;
   end Is_Less_Element_Node;

   -----------------------
   -- Is_Less_Node_Node --
   -----------------------

   function Is_Less_Node_Node (L, R : Node_Type) return Boolean is
   begin
      return L.Element < R.Element;
   end Is_Less_Node_Node;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      if Subset.K = Plain and Of_Set.K = Plain then
         return Set_Ops.Set_Subset (Subset.Tree.all,
                                    Of_Set => Of_Set.Tree.all);
      end if;

      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      if Length (Subset) > Length (Of_Set) then
         return False;
      end if;

      declare
         Subset_Node : Count_Type := First (Subset).Node;
         Set_Node    : Count_Type := First (Of_Set).Node;

         Subset_Last : constant Count_Type :=
           Next (Subset.Tree.all, Last (Subset).Node);
         Set_Last    : constant Count_Type :=
           Next (Of_Set.Tree.all, Last (Of_Set).Node);

      begin
         loop
            if Set_Node = Set_Last then
               return Subset_Node = 0;
            end if;

            if Subset_Node = Subset_Last then
               return True;
            end if;

            if Subset.Tree.Nodes (Subset_Node).Element <
              Of_Set.Tree.Nodes (Set_Node).Element then
               return False;
            end if;

            if Of_Set.Tree.Nodes (Set_Node).Element <
              Subset.Tree.Nodes (Subset_Node).Element then
               Set_Node := Next (Of_Set.Tree.all, Set_Node);
            else
               Set_Node := Next (Of_Set.Tree.all, Set_Node);
               Subset_Node := Next (Subset.Tree.all, Subset_Node);
            end if;
         end loop;
      end;
   end Is_Subset;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Process   :
      not null access procedure (Container : Set; Position : Cursor))
   is
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Local_Iterate is
        new Tree_Operations.Generic_Iteration (Process_Node);

      procedure Local_Iterate_Between is
        new Iterate_Between (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Count_Type) is
      begin
         Process (Container, (Node => Node));
      end Process_Node;

      T : Tree_Types.Tree_Type renames Container.Tree.all;
      B : Natural renames T.Busy;

      --  Start of prccessing for Iterate

   begin
      B := B + 1;

      begin
         if Container.K = Plain then
            Local_Iterate (T);
            return;
         end if;

         if Container.Length = 0 then
            return;
         end if;

         Local_Iterate_Between (T, Container.First, Container.Last);

      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   ---------------------
   -- Iterate_Between --
   ---------------------

   procedure Iterate_Between (Tree : Tree_Types.Tree_Type;
                              From : Count_Type;
                              To   : Count_Type) is

      FElt : constant Element_Type := Tree.Nodes (From).Element;
      TElt : constant Element_Type := Tree.Nodes (To).Element;
      procedure Iterate (P : Count_Type);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (P : Count_Type) is
         X : Count_Type := P;
      begin
         while X /= 0 loop
            if Tree.Nodes (X).Element < FElt then
               X := Tree.Nodes (X).Right;
            elsif TElt < Tree.Nodes (X).Element then
               X := Tree.Nodes (X).Left;
            else
               Iterate (Tree.Nodes (X).Left);
               Process (X);
               X := Tree.Nodes (X).Right;
            end if;
         end loop;
      end Iterate;

   begin
      Iterate (Tree.Root);
   end Iterate_Between;

   ----------
   -- Last --
   ----------

   function Last (Container : Set) return Cursor is
   begin
      if Length (Container) = 0 then
         return No_Element;
      end if;

      if Container.K = Plain then
         return (Node => Container.Tree.Last);
      end if;

      return (Node => Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Set) return Element_Type is
   begin
      if Last (Container).Node = 0 then
         raise Constraint_Error with "set is empty";
      end if;

      declare
         N : Tree_Types.Nodes_Type renames Container.Tree.Nodes;
      begin
         return N (Last (Container).Node).Element;
      end;
   end Last_Element;

   ----------
   -- Left --
   ----------

   function Left (Container : Set; Position : Cursor) return Set is
      Lst : Count_Type;
      Fst : constant Count_Type := First (Container).Node;
      L   : Count_Type := 0;
      C   : Count_Type := Fst;
   begin
      while C /= Position.Node loop
         if C = Last (Container).Node or C = 0 then
            raise Constraint_Error with
              "Position cursor has no element";
         end if;
         Lst := C;
         C := Next (Container.Tree.all, C);
         L := L + 1;
      end loop;
      if L = 0 then
         return (Capacity => Container.Capacity,
                 K        => Part,
                 Tree     => Container.Tree,
                 Length   => 0,
                 First    => 0,
                 Last     => 0);
      else
         return (Capacity => Container.Capacity,
                 K        => Part,
                 Tree     => Container.Tree,
                 Length   => L,
                 First    => Fst,
                 Last     => Lst);
      end if;
   end Left;

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

   function Length (Container : Set) return Count_Type is
   begin
      if Container.K = Plain then
         return Container.Tree.Length;
      else
         return Container.Length;
      end if;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Set; Source : in out Set) is
      S : Tree_Types.Tree_Type renames Source.Tree.all;
      N : Tree_Types.Nodes_Type renames S.Nodes;
      X : Count_Type;

   begin
      if Target.K /= Plain or Source.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      if S.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      Clear (Target);

      loop
         X := S.First;
         exit when X = 0;

         Insert (Target, N (X).Element);  -- optimize???

         Tree_Operations.Delete_Node_Sans_Free (S, X);
         Formal_Ordered_Sets.Free (S, X);
      end loop;
   end Move;

   ----------
   -- Next --
   ----------

   function Next_Unchecked
     (Container : Set;
      Position  : Count_Type) return Count_Type is
   begin

      if Container.K = Part and then
        (Container.Length = 0 or Position = Container.Last) then
         return 0;
      end if;

      return Tree_Operations.Next (Container.Tree.all, Position);
   end Next_Unchecked;

   function Next (Container : Set; Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Next");
      return (Node => Next_Unchecked (Container, Position.Node));
   end Next;

   procedure Next (Container : Set; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
      if Left.K = Plain and Right.K = Plain then
         return Set_Ops.Set_Overlap (Left.Tree.all, Right.Tree.all);
      end if;

      if Length (Left) = 0 or Length (Right) = 0 then
         return False;
      end if;

      declare

         L_Node : Count_Type := First (Left).Node;
         R_Node : Count_Type := First (Right).Node;

         L_Last : constant Count_Type :=
           Next (Left.Tree.all, Last (Left).Node);
         R_Last : constant Count_Type :=
           Next (Right.Tree.all, Last (Right).Node);

      begin
         if Left'Address = Right'Address then
            return True;
         end if;

         loop
            if L_Node = L_Last
              or else R_Node = R_Last
            then
               return False;
            end if;

            if Left.Tree.Nodes (L_Node).Element <
              Right.Tree.Nodes (R_Node).Element then
               L_Node := Next (Left.Tree.all, L_Node);

            elsif Right.Tree.Nodes (R_Node).Element <
              Left.Tree.Nodes (L_Node).Element then
               R_Node := Next (Right.Tree.all, R_Node);

            else
               return True;
            end if;
         end loop;
      end;
   end Overlap;

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

   function Previous (Container : Set; Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Previous");

      if Container.K = Part and then
        (Container.Length = 0 or Position.Node = Container.First) then
         return No_Element;
      end if;

      declare
         Tree : Tree_Types.Tree_Type renames Container.Tree.all;
         Node : constant Count_Type :=
           Tree_Operations.Previous (Tree, Position.Node);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Previous;

   procedure Previous (Container : Set; Position : in out Cursor) is
   begin
      Position := Previous (Container, Position);
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : in out Set;
      Position  : Cursor;
      Process   : not null access procedure (Element : Element_Type))
   is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Query_Element");

      declare
         T : Tree_Types.Tree_Type renames Container.Tree.all;

         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (T.Nodes (Position.Node).Element);
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
      procedure Read_Element (Node : in out Node_Type);
      pragma Inline (Read_Element);

      procedure Allocate is
        new Generic_Allocate (Read_Element);

      procedure Read_Elements is
        new Tree_Operations.Generic_Read (Allocate);

      ------------------
      -- Read_Element --
      ------------------

      procedure Read_Element (Node : in out Node_Type) is
      begin
         Element_Type'Read (Stream, Node.Element);
      end Read_Element;

      --  Start of processing for Read
      Result : Tree_Type_Access;
   begin
      if Container.K /= Plain then
         raise Constraint_Error;
      end if;

      if Container.Tree = null then
         Result := new Tree_Types.Tree_Type (Container.Capacity);
      else
         Result := Container.Tree;
      end if;

      Read_Elements (Stream, Result.all);
      Container.Tree := Result;
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream set cursor";
   end Read;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in out Set; New_Item : Element_Type) is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      declare
         Node : constant Count_Type :=
           Element_Keys.Find (Container.Tree.all, New_Item);

      begin
         if Node = 0 then
            raise Constraint_Error with
              "attempt to replace element not in set";
         end if;

         if Container.Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (set is locked)";
         end if;

         Container.Tree.Nodes (Node).Element := New_Item;
      end;
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Tree : in out Tree_Types.Tree_Type;
      Node : Count_Type;
      Item : Element_Type)
   is
      pragma Assert (Node /= 0);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Local_Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Local_Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Local_Insert_Post);

      procedure Local_Insert_With_Hint is
        new Element_Keys.Generic_Conditional_Insert_With_Hint
          (Local_Insert_Post,
           Local_Insert_Sans_Hint);

      NN : Tree_Types.Nodes_Type renames Tree.Nodes;

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
         N  : Node_Type renames NN (Node);

      begin
         N.Element := Item;
         N.Color := Red;
         N.Parent := 0;
         N.Right := 0;
         N.Left := 0;

         return Node;
      end New_Node;

      Hint      : Count_Type;
      Result    : Count_Type;
      Inserted  : Boolean;

      --  Start of processing for Insert

   begin
      if Item < NN (Node).Element
        or else NN (Node).Element < Item
      then
         null;

      else
         if Tree.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (set is locked)";
         end if;

         NN (Node).Element := Item;
         return;
      end if;

      Hint := Element_Keys.Ceiling (Tree, Item);

      if Hint = 0 then
         null;

      elsif Item < NN (Hint).Element then
         if Hint = Node then
            if Tree.Lock > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (set is locked)";
            end if;

            NN (Node).Element := Item;
            return;
         end if;

      else
         pragma Assert (not (NN (Hint).Element < Item));
         raise Program_Error with "attempt to replace existing element";
      end if;

      Tree_Operations.Delete_Node_Sans_Free (Tree, Node);  -- Checks busy-bit

      Local_Insert_With_Hint
        (Tree     => Tree,
         Position => Hint,
         Key      => Item,
         Node     => Result,
         Inserted => Inserted);

      pragma Assert (Inserted);
      pragma Assert (Result = Node);
   end Replace_Element;

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container.Tree.all, Position.Node),
                     "bad cursor in Replace_Element");

      Replace_Element (Container.Tree.all, Position.Node, New_Item);
   end Replace_Element;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Set;
      Process   :
      not null access procedure (Container : Set; Position : Cursor))
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
         Process (Container, (Node => Node));
      end Process_Node;

      T : Tree_Types.Tree_Type renames Container.Tree.all;
      B : Natural renames T.Busy;

      --  Start of processing for Reverse_Iterate

   begin
      B := B + 1;

      begin
         if Container.K = Plain then
            Local_Reverse_Iterate (T);
            return;
         end if;

         if Container.Length = 0 then
            return;
         end if;

         declare
            Node  : Count_Type := Container.Last;
            First : constant Count_Type :=
              Previous (Container.Tree.all, Container.First);

         begin

            while Node /= First loop
               Process_Node (Node);
               Node := Previous (Container.Tree.all, Node);
            end loop;

         end;

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

   function Right (Container : Set; Position : Cursor) return Set is
      Lst : Count_Type;
      L   : Count_Type := 0;
      C   : Count_Type := Position.Node;
   begin

      if C = 0 then
         return (Capacity => Container.Capacity,
                 K        => Part,
                 Tree     => Container.Tree,
                 Length   => 0,
                 First    => 0,
                 Last     => 0);
      end if;

      if Container.K = Plain then
         Lst := 0;
      else
         Lst := Next (Container.Tree.all, Container.Last);
      end if;

      if C = Lst then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      while C /= Lst loop
         if C = 0 then
            raise Constraint_Error with
              "Position cursor has no element";
         end if;
         C := Next (Container.Tree.all, C);
         L := L + 1;
      end loop;

      return (Capacity => Container.Capacity,
              K        => Part,
              Tree     => Container.Tree,
              Length   => L,
              First    => Position.Node,
              Last     => Last (Container).Node);
   end Right;

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

   procedure Set_Color
     (Node  : in out Node_Type;
      Color : Red_Black_Trees.Color_Type)
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

   ------------------
   -- Strict_Equal --
   ------------------

   function Strict_Equal (Left, Right : Set) return Boolean is
      LNode : Count_Type := First (Left).Node;
      RNode : Count_Type := First (Right).Node;
   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      while LNode = RNode loop
         if LNode = 0 then
            return True;
         end if;

         if Left.Tree.Nodes (LNode).Element /=
           Right.Tree.Nodes (RNode).Element then
            exit;
         end if;

         LNode := Next_Unchecked (Left, LNode);
         RNode := Next_Unchecked (Right, RNode);
      end loop;
      return False;

   end Strict_Equal;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in out Set; Source : Set) is
   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Source.K = Plain then
         Set_Ops.Set_Symmetric_Difference (Target.Tree.all, Source.Tree.all);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      declare

         Tgt : Count_Type := Target.First;
         Src : Count_Type := Source.First;

         SLast : constant Count_Type := Next (Source.Tree.all, Source.Last);

         New_Tgt_Node : Count_Type;

      begin
         if Target.Tree.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;

         if Target'Address = Source'Address then
            Clear (Target);
            return;
         end if;

         loop
            if Tgt = 0 then
               while Src /= SLast loop
                  Insert_With_Hint
                    (Dst_Set  => Target.Tree.all,
                     Dst_Hint => 0,
                     Src_Node => Source.Tree.Nodes (Src),
                     Dst_Node => New_Tgt_Node);

                  Src := Next (Source.Tree.all, Src);
               end loop;

               return;
            end if;

            if Src = SLast then
               return;
            end if;

            if Target.Tree.Nodes (Tgt).Element <
              Source.Tree.Nodes (Src).Element then
               Tgt := Next (Target.Tree.all, Tgt);

            elsif Source.Tree.Nodes (Src).Element <
              Target.Tree.Nodes (Tgt).Element then
               Insert_With_Hint
                 (Dst_Set  => Target.Tree.all,
                  Dst_Hint => Tgt,
                  Src_Node => Source.Tree.Nodes (Src),
                  Dst_Node => New_Tgt_Node);

               Src := Next (Source.Tree.all, Src);

            else
               declare
                  X : constant Count_Type := Tgt;
               begin
                  Tgt := Next (Target.Tree.all, Tgt);
                  Delete_Node_Sans_Free (Target.Tree.all, X);
                  Formal_Ordered_Sets.Free (Target.Tree.all, X);
               end;

               Src := Next (Source.Tree.all, Src);
            end if;
         end loop;
      end;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Length (Right) = 0 then
         return Left.Copy;
      end if;

      if Length (Left) = 0 then
         return Right.Copy;
      end if;

      return S : Set (Length (Left) + Length (Right)) do
         if Left.K = Plain and Right.K = Plain then
            Assign (S.Tree.all,
              Set_Ops.Set_Symmetric_Difference (Left.Tree.all,
                Right.Tree.all));
            return;
         end if;

         declare

            Tree : Tree_Types.Tree_Type renames S.Tree.all;

            L_Node : Count_Type := First (Left).Node;
            R_Node : Count_Type := First (Right).Node;

            L_Last : constant Count_Type :=
              Next (Left.Tree.all, Last (Left).Node);
            R_Last : constant Count_Type :=
              Next (Right.Tree.all, Last (Right).Node);

            Dst_Node : Count_Type;

         begin
            loop
               if L_Node = L_Last then
                  while R_Node /= R_Last loop
                     Insert_With_Hint
                       (Dst_Set  => Tree,
                        Dst_Hint => 0,
                        Src_Node => Right.Tree.Nodes (R_Node),
                        Dst_Node => Dst_Node);

                     R_Node := Next (Right.Tree.all, R_Node);
                  end loop;

                  return;
               end if;

               if R_Node = R_Last then
                  while L_Node /= L_Last  loop
                     Insert_With_Hint
                       (Dst_Set  => Tree,
                        Dst_Hint => 0,
                        Src_Node => Left.Tree.Nodes (L_Node),
                        Dst_Node => Dst_Node);

                     L_Node := Next (Left.Tree.all, L_Node);
                  end loop;

                  return;
               end if;

               if Left.Tree.Nodes (L_Node).Element <
                 Right.Tree.Nodes (R_Node).Element then
                  Insert_With_Hint
                    (Dst_Set  => Tree,
                     Dst_Hint => 0,
                     Src_Node => Left.Tree.Nodes (L_Node),
                     Dst_Node => Dst_Node);

                  L_Node := Next (Left.Tree.all, L_Node);

               elsif Right.Tree.Nodes (R_Node).Element <
                 Left.Tree.Nodes (L_Node).Element then
                  Insert_With_Hint
                    (Dst_Set  => Tree,
                     Dst_Hint => 0,
                     Src_Node => Right.Tree.Nodes (R_Node),
                     Dst_Node => Dst_Node);

                  R_Node := Next (Right.Tree.all, R_Node);

               else
                  L_Node := Next (Left.Tree.all, L_Node);
                  R_Node := Next (Right.Tree.all, R_Node);
               end if;
            end loop;
         end;

      end return;
   end Symmetric_Difference;

   ------------
   -- To_Set --
   ------------

   function To_Set (New_Item : Element_Type) return Set is
      Node     : Count_Type;
      Inserted : Boolean;

   begin
      return S : Set (Capacity => 1) do
         Insert_Sans_Hint (S.Tree.all, New_Item, Node, Inserted);
         pragma Assert (Inserted);
      end return;
   end To_Set;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Set; Source : Set) is
   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Source.K = Plain then
         Set_Ops.Set_Union (Target.Tree.all, Source.Tree.all);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      declare
         Hint : Count_Type := 0;

         procedure Process (Node : Count_Type);
         pragma Inline (Process);

         procedure Iterate is new Iterate_Between (Process);

         -------------
         -- Process --
         -------------

         procedure Process (Node : Count_Type) is
         begin
            Insert_With_Hint
              (Dst_Set  => Target.Tree.all,
               Dst_Hint => Hint,
               Src_Node => Source.Tree.Nodes (Node),
               Dst_Node => Hint);
         end Process;

         --  Start of processing for Union

      begin
         if Target'Address = Source'Address then
            return;
         end if;

         if Target.Tree.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;

         Iterate (Source.Tree.all, Source.First, Source.Last);
      end;
   end Union;

   function Union (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Left.Copy;
      end if;

      if Length (Left) = 0 then
         return Right.Copy;
      end if;

      if Length (Right) = 0 then
         return Left.Copy;
      end if;

      return S : Set (Length (Left) + Length (Right)) do
         S.Assign (Source => Left);
         S.Union (Right);
      end return;
   end Union;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Set)
   is
      procedure Write_Element
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type);
      pragma Inline (Write_Element);

      procedure Write_Elements is
        new Tree_Operations.Generic_Write (Write_Element);

      -------------------
      -- Write_Element --
      -------------------

      procedure Write_Element
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type)
      is
      begin
         Element_Type'Write (Stream, Node.Element);
      end Write_Element;

      --  Start of processing for Write

   begin
      Write_Elements (Stream, Container.Tree.all);
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream set cursor";
   end Write;

end Ada.Containers.Formal_Ordered_Sets;
