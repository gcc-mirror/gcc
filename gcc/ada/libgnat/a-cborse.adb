------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . B O U N D E D _ O R D E R E D _ S E T S  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

with Ada.Containers.Helpers; use Ada.Containers.Helpers;

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations);

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys;
pragma Elaborate_All (Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys);

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations;
pragma Elaborate_All
  (Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations);

with System; use type System.Address;

package body Ada.Containers.Bounded_Ordered_Sets is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   ------------------------------
   -- Access to Fields of Node --
   ------------------------------

   --  These subprograms provide functional notation for access to fields
   --  of a node, and procedural notation for modifying these fields.

   function Color (Node : Node_Type) return Red_Black_Trees.Color_Type;
   pragma Inline (Color);

   function Left (Node : Node_Type) return Count_Type;
   pragma Inline (Left);

   function Parent (Node : Node_Type) return Count_Type;
   pragma Inline (Parent);

   function Right (Node : Node_Type) return Count_Type;
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

   procedure Insert_Sans_Hint
     (Container : in out Set;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean);

   procedure Insert_With_Hint
     (Dst_Set  : in out Set;
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

   procedure Replace_Element
     (Container : in out Set;
      Index     : Count_Type;
      Item      : Element_Type);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package Tree_Operations is
      new Red_Black_Trees.Generic_Bounded_Operations (Tree_Types);

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
         Set_Type         => Set,
         Assign           => Assign,
         Insert_With_Hint => Insert_With_Hint,
         Is_Less          => Is_Less_Node_Node);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Checks and then Right.Node = 0 then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "bad Left cursor in ""<""");

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "bad Right cursor in ""<""");

      declare
         LN : Nodes_Type renames Left.Container.Nodes;
         RN : Nodes_Type renames Right.Container.Nodes;
      begin
         return LN (Left.Node).Element < RN (Right.Node).Element;
      end;
   end "<";

   function "<" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "bad Left cursor in ""<""");

      return Left.Container.Nodes (Left.Node).Element < Right;
   end "<";

   function "<" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      if Checks and then Right.Node = 0 then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "bad Right cursor in ""<""");

      return Left < Right.Container.Nodes (Right.Node).Element;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
      function Is_Equal_Node_Node (L, R : Node_Type) return Boolean;
      pragma Inline (Is_Equal_Node_Node);

      function Is_Equal is
         new Tree_Operations.Generic_Equal (Is_Equal_Node_Node);

      ------------------------
      -- Is_Equal_Node_Node --
      ------------------------

      function Is_Equal_Node_Node (L, R : Node_Type) return Boolean is
      begin
         return L.Element = R.Element;
      end Is_Equal_Node_Node;

   --  Start of processing for Is_Equal

   begin
      return Is_Equal (Left, Right);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      if Checks and then Right.Node = 0 then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "bad Left cursor in "">""");

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "bad Right cursor in "">""");

      --  L > R same as R < L

      declare
         LN : Nodes_Type renames Left.Container.Nodes;
         RN : Nodes_Type renames Right.Container.Nodes;
      begin
         return RN (Right.Node).Element < LN (Left.Node).Element;
      end;
   end ">";

   function ">" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      if Checks and then Right.Node = 0 then
         raise Constraint_Error with "Right cursor equals No_Element";
      end if;

      pragma Assert (Vet (Right.Container.all, Right.Node),
                     "bad Right cursor in "">""");

      return Right.Container.Nodes (Right.Node).Element < Left;
   end ">";

   function ">" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with "Left cursor equals No_Element";
      end if;

      pragma Assert (Vet (Left.Container.all, Left.Node),
                     "bad Left cursor in "">""");

      return Right < Left.Container.Nodes (Left.Node).Element;
   end ">";

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Set; Source : Set) is
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

      if Checks and then Target.Capacity < Source.Length then
         raise Capacity_Error
           with "Target capacity is less than Source length";
      end if;

      Target.Clear;
      Append_Elements (Source);
   end Assign;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Count_Type :=
        Element_Keys.Ceiling (Container, Item);
   begin
      return (if Node = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Ceiling;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Set) is
   begin
      while not Container.Is_Empty loop
         Container.Delete_Last;
      end loop;
   end Clear;

   -----------
   -- Color --
   -----------

   function Color (Node : Node_Type) return Red_Black_Trees.Color_Type is
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
      if Checks and then Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert
        (Vet (Container, Position.Node),
         "bad cursor in Constant_Reference");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => N.Element'Access,
            Control => (Controlled with TC))
         do
            Busy (TC.all);
         end return;
      end;
   end Constant_Reference;

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
      C : constant Count_Type :=
        (if Capacity = 0 then Source.Length
         else Capacity);
   begin
      if Checks and then C < Source.Length then
         raise Capacity_Error with "Capacity too small";
      end if;

      return Target : Set (Capacity => C) do
         Assign (Target => Target, Source => Source);
      end return;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Set; Position : in out Cursor) is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Position cursor designates wrong set";
      end if;

      TC_Check (Container.TC);

      pragma Assert (Vet (Container, Position.Node),
                     "bad cursor in Delete");

      Tree_Operations.Delete_Node_Sans_Free (Container, Position.Node);
      Tree_Operations.Free (Container, Position.Node);

      Position := No_Element;
   end Delete;

   procedure Delete (Container : in out Set; Item : Element_Type) is
      X : constant Count_Type := Element_Keys.Find (Container, Item);

   begin
      Tree_Operations.Delete_Node_Sans_Free (Container, X);

      if Checks and then X = 0 then
         raise Constraint_Error with "attempt to delete element not in set";
      end if;

      Tree_Operations.Free (Container, X);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Set) is
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

   procedure Delete_Last (Container : in out Set) is
      X : constant Count_Type := Container.Last;
   begin
      if X /= 0 then
         Tree_Operations.Delete_Node_Sans_Free (Container, X);
         Tree_Operations.Free (Container, X);
      end if;
   end Delete_Last;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Set; Source : Set)
      renames Set_Ops.Set_Difference;

   function Difference (Left, Right : Set) return Set
      renames Set_Ops.Set_Difference;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "bad cursor in Element");

      return Position.Container.Nodes (Position.Node).Element;
   end Element;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      return (if Left < Right or else Right < Left then False else True);
   end Equivalent_Elements;

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
      function Is_Equivalent_Node_Node (L, R : Node_Type) return Boolean;
      pragma Inline (Is_Equivalent_Node_Node);

      function Is_Equivalent is
         new Tree_Operations.Generic_Equal (Is_Equivalent_Node_Node);

      -----------------------------
      -- Is_Equivalent_Node_Node --
      -----------------------------

      function Is_Equivalent_Node_Node (L, R : Node_Type) return Boolean is
      begin
         return (if L.Element < R.Element then False
                 elsif R.Element < L.Element then False
                 else True);
      end Is_Equivalent_Node_Node;

   --  Start of processing for Equivalent_Sets

   begin
      return Is_Equivalent (Left, Right);
   end Equivalent_Sets;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      X : constant Count_Type := Element_Keys.Find (Container, Item);
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
         Unbusy (Object.Container.TC);
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Count_Type := Element_Keys.Find (Container, Item);
   begin
      return (if Node = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      return (if Container.First = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Container.First));
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
         return Bounded_Ordered_Sets.First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Set) return Element_Type is
   begin
      if Checks and then Container.First = 0 then
         raise Constraint_Error with "set is empty";
      end if;

      return Container.Nodes (Container.First).Element;
   end First_Element;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Set; Item : Element_Type) return Cursor is
      Node : constant Count_Type := Element_Keys.Floor (Container, Item);
   begin
      return (if Node = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Floor;

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
         Node : constant Count_Type :=
           Key_Keys.Ceiling (Container, Key);
      begin
         return (if Node = 0 then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Ceiling;

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Container : aliased Set;
         Key       : Key_Type) return Constant_Reference_Type
      is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Checks and then Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         declare
            N : Node_Type renames Container.Nodes (Node);
            TC : constant Tamper_Counts_Access :=
              Container.TC'Unrestricted_Access;
         begin
            return R : constant Constant_Reference_Type :=
              (Element => N.Element'Access,
               Control => (Controlled with TC))
            do
               Busy (TC.all);
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
         X : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Checks and then X = 0 then
            raise Constraint_Error with "attempt to delete key not in set";
         end if;

         Tree_Operations.Delete_Node_Sans_Free (Container, X);
         Tree_Operations.Free (Container, X);
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Container : Set; Key : Key_Type) return Element_Type is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Checks and then Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         return Container.Nodes (Node).Element;
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

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         X : constant Count_Type := Key_Keys.Find (Container, Key);
      begin
         if X /= 0 then
            Tree_Operations.Delete_Node_Sans_Free (Container, X);
            Tree_Operations.Free (Container, X);
         end if;
      end Exclude;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Control : in out Reference_Control_Type) is
      begin
         if Control.Container /= null then
            Impl.Reference_Control_Type (Control).Finalize;

            if Checks and then not (Key (Control.Pos) = Control.Old_Key.all)
            then
               Delete (Control.Container.all, Key (Control.Pos));
               raise Program_Error;
            end if;

            Control.Container := null;
         end if;
      end Finalize;

      ----------
      -- Find --
      ----------

      function Find (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);
      begin
         return (if Node = 0 then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Find;

      -----------
      -- Floor --
      -----------

      function Floor (Container : Set; Key : Key_Type) return Cursor is
         Node : constant Count_Type := Key_Keys.Floor (Container, Key);
      begin
         return (if Node = 0 then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
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

      function Key (Position : Cursor) return Key_Type is
      begin
         if Checks and then Position.Node = 0 then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         pragma Assert (Vet (Position.Container.all, Position.Node),
                        "bad cursor in Key");

         return Key (Position.Container.Nodes (Position.Node).Element);
      end Key;

      ----------
      -- Read --
      ----------

      procedure  Read
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
         if Checks and then Position.Container = null then
            raise Constraint_Error with "Position cursor has no element";
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert
           (Vet (Container, Position.Node),
            "bad cursor in function Reference_Preserving_Key");

         declare
            N : Node_Type renames Container.Nodes (Position.Node);
         begin
            return R : constant Reference_Type :=
                         (Element => N.Element'Access,
                          Control =>
                            (Controlled with
                              Container.TC'Unrestricted_Access,
                              Container => Container'Access,
                              Pos       => Position,
                              Old_Key   => new Key_Type'(Key (Position))))
            do
               Busy (Container.TC);
            end return;
         end;
      end Reference_Preserving_Key;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Key       : Key_Type) return Reference_Type
      is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Checks and then Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         declare
            N : Node_Type renames Container.Nodes (Node);
         begin
            return R : constant Reference_Type :=
                         (Element => N.Element'Access,
                          Control =>
                            (Controlled with
                              Container.TC'Unrestricted_Access,
                              Container => Container'Access,
                               Pos      => Find (Container, Key),
                               Old_Key  => new Key_Type'(Key)))
            do
               Busy (Container.TC);
            end return;
         end;
      end Reference_Preserving_Key;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Checks and then Node = 0 then
            raise Constraint_Error with
              "attempt to replace key not in set";
         end if;

         Replace_Element (Container, Node, New_Item);
      end Replace;

      -----------------------------------
      -- Update_Element_Preserving_Key --
      -----------------------------------

      procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : Cursor;
         Process   : not null access procedure (Element : in out Element_Type))
      is
      begin
         if Checks and then Position.Node = 0 then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong set";
         end if;

         pragma Assert (Vet (Container, Position.Node),
                        "bad cursor in Update_Element_Preserving_Key");

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            N : Node_Type renames Container.Nodes (Position.Node);
            E : Element_Type renames N.Element;
            K : constant Key_Type := Key (E);
            Lock : With_Lock (Container.TC'Unrestricted_Access);
         begin
            Process (E);
            if Equivalent_Keys (K, Key (E)) then
               return;
            end if;
         end;

         Tree_Operations.Delete_Node_Sans_Free (Container, Position.Node);
         Tree_Operations.Free (Container, Position.Node);

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

   ------------------------
   -- Get_Element_Access --
   ------------------------

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access is
   begin
      return Position.Container.Nodes (Position.Node).Element'Access;
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

   procedure Include (Container : in out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         TE_Check (Container.TC);

         Container.Nodes (Position.Node).Element := New_Item;
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
        (Container,
         New_Item,
         Position.Node,
         Inserted);

      Position.Container := Container'Unrestricted_Access;
   end Insert;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      pragma Unreferenced (Position);

      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if Checks and then not Inserted then
         raise Constraint_Error with
           "attempt to insert element already in set";
      end if;
   end Insert;

   ----------------------
   -- Insert_Sans_Hint --
   ----------------------

   procedure Insert_Sans_Hint
     (Container : in out Set;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean)
   is
      procedure Set_Element (Node : in out Node_Type);
      pragma Inline (Set_Element);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Insert_Post is
        new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Conditional_Insert_Sans_Hint is
        new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Allocate is
         new Tree_Operations.Generic_Allocate (Set_Element);

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
      TC_Check (Container.TC);

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
     (Dst_Set  : in out Set;
      Dst_Hint : Count_Type;
      Src_Node : Node_Type;
      Dst_Node : out Count_Type)
   is
      Success : Boolean;
      pragma Unreferenced (Success);

      procedure Set_Element (Node : in out Node_Type);
      pragma Inline (Set_Element);

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
         new Tree_Operations.Generic_Allocate (Set_Element);

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

   procedure Intersection (Target : in out Set; Source : Set)
      renames Set_Ops.Set_Intersection;

   function Intersection (Left, Right : Set) return Set
      renames Set_Ops.Set_Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
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

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean
      renames Set_Ops.Set_Subset;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
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

      S : Set renames Container'Unrestricted_Access.all;
      Busy : With_Busy (S.TC'Unrestricted_Access);

   --  Start of processing for Iterate

   begin
      Local_Iterate (S);
   end Iterate;

   function Iterate (Container : Set)
     return Set_Iterator_Interfaces.Reversible_Iterator'class
   is
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
        Iterator'(Limited_Controlled with
                    Container => Container'Unrestricted_Access,
                    Node      => 0)
      do
         Busy (Container.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   function Iterate (Container : Set; Start : Cursor)
     return Set_Iterator_Interfaces.Reversible_Iterator'class
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
           "Start cursor of Iterate designates wrong set";
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
        Iterator'(Limited_Controlled with
                    Container => Container'Unrestricted_Access,
                    Node      => Start.Node)
      do
         Busy (Container.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : Set) return Cursor is
   begin
      return (if Container.Last = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Container.Last));
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
         return Bounded_Ordered_Sets.Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Set) return Element_Type is
   begin
      if Checks and then Container.Last = 0 then
         raise Constraint_Error with "set is empty";
      end if;

      return Container.Nodes (Container.Last).Element;
   end Last_Element;

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

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      TC_Check (Source.TC);

      Target.Assign (Source);
      Source.Clear;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "bad cursor in Next");

      declare
         Node : constant Count_Type :=
           Tree_Operations.Next (Position.Container.all, Position.Node);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong set";
      end if;

      return Next (Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean
      renames Set_Ops.Set_Overlap;

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

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "bad cursor in Previous");

      declare
         Node : constant Count_Type :=
           Tree_Operations.Previous (Position.Container.all, Position.Node);
      begin
         return (if Node = 0 then No_Element
                 else Cursor'(Position.Container, Node));
      end;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Previous (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Previous designates wrong set";
      end if;

      return Previous (Position);
   end Previous;

   ----------------------
   -- Pseudo_Reference --
   ----------------------

   function Pseudo_Reference
     (Container : aliased Set'Class) return Reference_Control_Type
   is
      TC : constant Tamper_Counts_Access :=
        Container.TC'Unrestricted_Access;
   begin
      return R : constant Reference_Control_Type := (Controlled with TC) do
         Busy (TC.all);
      end return;
   end Pseudo_Reference;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Position.Container.all, Position.Node),
                     "bad cursor in Query_Element");

      declare
         S : Set renames Position.Container.all;
         Lock : With_Lock (S.TC'Unrestricted_Access);
      begin
         Process (S.Nodes (Position.Node).Element);
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
         new Tree_Operations.Generic_Allocate (Read_Element);

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

   begin
      Read_Elements (Stream, Container);
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
      Node : constant Count_Type := Element_Keys.Find (Container, New_Item);

   begin
      if Checks and then Node = 0 then
         raise Constraint_Error with
           "attempt to replace element not in set";
      end if;

      TE_Check (Container.TC);

      Container.Nodes (Node).Element := New_Item;
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Set;
      Index     : Count_Type;
      Item      : Element_Type)
   is
      pragma Assert (Index /= 0);

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

      Nodes : Nodes_Type renames Container.Nodes;
      Node  : Node_Type renames Nodes (Index);

      --------------
      -- New_Node --
      --------------

      function New_Node return Count_Type is
      begin
         Node.Element := Item;
         Node.Color   := Red_Black_Trees.Red;
         Node.Parent  := 0;
         Node.Right   := 0;
         Node.Left    := 0;
         return Index;
      end New_Node;

      Hint      : Count_Type;
      Result    : Count_Type;
      Inserted  : Boolean;
      Compare   : Boolean;

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

      declare
         Lock : With_Lock (Container.TC'Unrestricted_Access);
      begin
         Compare := (if Item < Node.Element then False
                     elsif Node.Element < Item then False
                     else True);
      end;

      if Compare then

         --  Item is equivalent to the node's element, so we will not have to
         --  move the node.

         TE_Check (Container.TC);

         Node.Element := Item;
         return;
      end if;

      --  The replacement Item is not equivalent to the element on the
      --  specified node, which means that it will need to be re-inserted in a
      --  different position in the tree. We must now determine whether Item is
      --  equivalent to some other element in the tree (which would prohibit
      --  the assignment and hence the move).

      --  Ceiling returns the smallest element equivalent or greater than the
      --  specified Item; if there is no such element, then it returns 0.

      Hint := Element_Keys.Ceiling (Container, Item);

      if Hint /= 0 then  -- Item <= Nodes (Hint).Element
         declare
            Lock : With_Lock (Container.TC'Unrestricted_Access);
         begin
            Compare := Item < Nodes (Hint).Element;
         end;

         --  Item is equivalent to Nodes (Hint).Element

         if Checks and then not Compare then

            --  Ceiling returns an element that is equivalent or greater than
            --  Item. If Item is "not less than" the element, then by
            --  elimination we know that Item is equivalent to the element.

            --  But this means that it is not possible to assign the value of
            --  Item to the specified element (on Node), because a different
            --  element (on Hint) equivalent to Item already exsits. (Were we
            --  to change Node's element value, we would have to move Node, but
            --  we would be unable to move the Node, because its new position
            --  in the tree is already occupied by an equivalent element.)

            raise Program_Error with "attempt to replace existing element";
         end if;

         --  Item is not equivalent to any other element in the tree
         --  (specifically, it is less than Nodes (Hint).Element), so it is
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

         if Hint = Index then
            TE_Check (Container.TC);

            Node.Element := Item;
            return;
         end if;
      end if;

      --  If we get here, it is because Item was greater than all elements in
      --  the tree (Hint = 0), or because Item was less than some element at a
      --  different place in the tree (Item < Nodes (Hint).Element and Hint /=
      --  Index). In either case, we remove Node from the tree and then insert
      --  Item into the tree, onto the same Node.

      Tree_Operations.Delete_Node_Sans_Free (Container, Index);

      Local_Insert_With_Hint
        (Tree     => Container,
         Position => Hint,
         Key      => Item,
         Node     => Result,
         Inserted => Inserted);

      pragma Assert (Inserted);
      pragma Assert (Result = Index);
   end Replace_Element;

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Container, Position.Node),
                     "bad cursor in Replace_Element");

      Replace_Element (Container, Position.Node, New_Item);
   end Replace_Element;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Set;
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

      S : Set renames Container'Unrestricted_Access.all;
      Busy : With_Busy (S.TC'Unrestricted_Access);

   --  Start of processing for Reverse_Iterate

   begin
      Local_Reverse_Iterate (S);
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

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in out Set; Source : Set)
      renames Set_Ops.Set_Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set
      renames Set_Ops.Set_Symmetric_Difference;

   ------------
   -- To_Set --
   ------------

   function To_Set (New_Item : Element_Type) return Set is
      Node     : Count_Type;
      Inserted : Boolean;
   begin
      return S : Set (1) do
         Insert_Sans_Hint (S, New_Item, Node, Inserted);
         pragma Assert (Inserted);
      end return;
   end To_Set;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Set; Source : Set)
      renames Set_Ops.Set_Union;

   function Union (Left, Right : Set) return Set
      renames Set_Ops.Set_Union;

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
      Write_Elements (Stream, Container);
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

end Ada.Containers.Bounded_Ordered_Sets;
