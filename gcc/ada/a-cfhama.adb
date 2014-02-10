------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . F O R M A L _ H A S H E D _ M A P S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2013, Free Software Foundation, Inc.         --
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

with Ada.Containers.Hash_Tables.Generic_Bounded_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Operations);

with Ada.Containers.Hash_Tables.Generic_Bounded_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Keys);

with Ada.Containers.Prime_Numbers; use Ada.Containers.Prime_Numbers;

with System;  use type System.Address;

package body Ada.Containers.Formal_Hashed_Maps is

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  All local subprograms require comments ???

   function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Type) return Boolean;
   pragma Inline (Equivalent_Keys);

   procedure Free
     (HT : in out Map;
      X  : Count_Type);

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (HT   : in out Map;
      Node : out Count_Type);

   function Hash_Node (Node : Node_Type) return Hash_Type;
   pragma Inline (Hash_Node);

   function Next (Node : Node_Type) return Count_Type;
   pragma Inline (Next);

   procedure Set_Next (Node : in out Node_Type; Next : Count_Type);
   pragma Inline (Set_Next);

   function Vet (Container : Map; Position : Cursor) return Boolean;

   --------------------------
   -- Local Instantiations --
   --------------------------

   package HT_Ops is
     new Hash_Tables.Generic_Bounded_Operations
       (HT_Types  => HT_Types,
        Hash_Node => Hash_Node,
        Next      => Next,
        Set_Next  => Set_Next);

   package Key_Ops is
     new Hash_Tables.Generic_Bounded_Keys
       (HT_Types        => HT_Types,
        Next            => Next,
        Set_Next        => Set_Next,
        Key_Type        => Key_Type,
        Hash            => Hash,
        Equivalent_Keys => Equivalent_Keys);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      if Length (Left) = 0 then
         return True;
      end if;

      declare
         Node  : Count_Type;
         ENode : Count_Type;

      begin
         Node := Left.First.Node;
         while Node /= 0 loop
            ENode := Find (Container => Right,
                           Key       => Left.Nodes (Node).Key).Node;

            if ENode = 0 or else
              Right.Nodes (ENode).Element /= Left.Nodes (Node).Element
            then
               return False;
            end if;

            Node := HT_Ops.Next (Left, Node);
         end loop;

         return True;
      end;
   end "=";

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Map; Source : Map) is
      procedure Insert_Element (Source_Node : Count_Type);
      pragma Inline (Insert_Element);

      procedure Insert_Elements is
        new HT_Ops.Generic_Iteration (Insert_Element);

      --------------------
      -- Insert_Element --
      --------------------

      procedure Insert_Element (Source_Node : Count_Type) is
         N : Node_Type renames Source.Nodes (Source_Node);
      begin
         Target.Insert (N.Key, N.Element);
      end Insert_Element;

      --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Constraint_Error with  -- correct exception ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      Insert_Elements (Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Map) return Count_Type is
   begin
      return Container.Nodes'Length;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      HT_Ops.Clear (Container);
   end Clear;

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

   function Copy
     (Source   : Map;
      Capacity : Count_Type := 0) return Map
   is
      C      : constant Count_Type :=
        Count_Type'Max (Capacity, Source.Capacity);
      H      : Hash_Type;
      N      : Count_Type;
      Target : Map (C, Source.Modulus);
      Cu     : Cursor;

   begin
      if 0 < Capacity and then Capacity < Source.Capacity then
         raise Capacity_Error;
      end if;

      Target.Length := Source.Length;
      Target.Free := Source.Free;

      H := 1;
      while H <= Source.Modulus loop
         Target.Buckets (H) := Source.Buckets (H);
         H := H + 1;
      end loop;

      N := 1;
      while N <= Source.Capacity loop
         Target.Nodes (N) := Source.Nodes (N);
         N := N + 1;
      end loop;

      while N <= C loop
         Cu := (Node => N);
         Free (Target, Cu.Node);
         N := N + 1;
      end loop;

      return Target;
   end Copy;

   ---------------------
   -- Default_Modulus --
   ---------------------

   function Default_Modulus (Capacity : Count_Type) return Hash_Type is
   begin
      return To_Prime (Capacity);
   end Default_Modulus;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Count_Type;

   begin
      Key_Ops.Delete_Key_Sans_Free (Container, Key, X);

      if X = 0 then
         raise Constraint_Error with "attempt to delete key not in map";
      end if;

      Free (Container, X);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Delete has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in Delete");

      HT_Ops.Delete_Node_Sans_Free (Container, Position.Node);

      Free (Container, Position.Node);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Count_Type := Find (Container, Key).Node;

   begin
      if Node = 0 then
         raise Constraint_Error with
           "no element available because key not in map";
      end if;

      return Container.Nodes (Node).Element;
   end Element;

   function Element (Container : Map; Position : Cursor) return Element_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Container, Position),
                     "bad cursor in function Element");

      return Container.Nodes (Position.Node).Element;
   end Element;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Type) return Boolean
   is
   begin
      return Equivalent_Keys (Key, Node.Key);
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left   : Map;
      CLeft  : Cursor;
      Right  : Map;
      CRight : Cursor) return Boolean
   is
   begin
      if not Has_Element (Left, CLeft) then
         raise Constraint_Error with
           "Left cursor of Equivalent_Keys has no element";
      end if;

      if not Has_Element (Right, CRight) then
         raise Constraint_Error with
           "Right cursor of Equivalent_Keys has no element";
      end if;

      pragma Assert (Vet (Left, CLeft),
                     "Left cursor of Equivalent_Keys is bad");
      pragma Assert (Vet (Right, CRight),
                     "Right cursor of Equivalent_Keys is bad");

      declare
         LN : Node_Type renames Left.Nodes (CLeft.Node);
         RN : Node_Type renames Right.Nodes (CRight.Node);
      begin
         return Equivalent_Keys (LN.Key, RN.Key);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left  : Map;
      CLeft : Cursor;
      Right : Key_Type) return Boolean
   is
   begin
      if not Has_Element (Left, CLeft) then
         raise Constraint_Error with
           "Left cursor of Equivalent_Keys has no element";
      end if;

      pragma Assert (Vet (Left, CLeft),
                     "Left cursor in Equivalent_Keys is bad");

      declare
         LN : Node_Type renames Left.Nodes (CLeft.Node);
      begin
         return Equivalent_Keys (LN.Key, Right);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left   : Key_Type;
      Right  : Map;
      CRight : Cursor) return Boolean
   is
   begin
      if Has_Element (Right, CRight) then
         raise Constraint_Error with
           "Right cursor of Equivalent_Keys has no element";
      end if;

      pragma Assert (Vet (Right, CRight),
                     "Right cursor of Equivalent_Keys is bad");

      declare
         RN : Node_Type renames Right.Nodes (CRight.Node);

      begin
         return Equivalent_Keys (Left, RN.Key);
      end;
   end Equivalent_Keys;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      X : Count_Type;
   begin
      Key_Ops.Delete_Key_Sans_Free (Container, Key, X);
      Free (Container, X);
   end Exclude;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

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
      Node : constant Count_Type := HT_Ops.First (Container);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      return (Node => Node);
   end First;

   ----------
   -- Free --
   ----------

   procedure Free (HT : in out Map; X : Count_Type) is
   begin
      HT.Nodes (X).Has_Element := False;
      HT_Ops.Free (HT, X);
   end Free;

   ----------------------
   -- Generic_Allocate --
   ----------------------

   procedure Generic_Allocate (HT : in out Map; Node : out Count_Type) is

      procedure Allocate is
        new HT_Ops.Generic_Allocate (Set_Element);

   begin
      Allocate (HT, Node);
      HT.Nodes (Node).Has_Element := True;
   end Generic_Allocate;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : Map; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 or else
        not Container.Nodes (Position.Node).Has_Element then
         return False;
      end if;

      return True;
   end Has_Element;

   ---------------
   -- Hash_Node --
   ---------------

   function Hash_Node (Node : Node_Type) return Hash_Type is
   begin
      return Hash (Node.Key);
   end Hash_Node;

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
      procedure Assign_Key (Node : in out Node_Type);
      pragma Inline (Assign_Key);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Local_Insert is
        new Key_Ops.Generic_Conditional_Insert (New_Node);

      procedure Allocate is
        new Generic_Allocate (Assign_Key);

      -----------------
      --  Assign_Key --
      -----------------

      procedure Assign_Key (Node : in out Node_Type) is
      begin
         Node.Key := Key;
         Node.Element := New_Item;
      end Assign_Key;

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
      Local_Insert (Container, Key, Position.Node, Inserted);
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
         raise Constraint_Error with
           "attempt to insert key already in map";
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Length (Container) = 0;
   end Is_Empty;

   ---------
   -- Key --
   ---------

   function Key (Container : Map; Position : Cursor) return Key_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of function Key has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in function Key");

      return Container.Nodes (Position.Node).Key;
   end Key;

   ----------
   -- Left --
   ----------

   function Left (Container : Map; Position : Cursor) return Map is
      Curs : Cursor;
      C    : Map (Container.Capacity, Container.Modulus) :=
        Copy (Container, Container.Capacity);
      Node : Count_Type;

   begin
      Curs := Position;

      if Curs = No_Element then
         return C;
      end if;

      if not Has_Element (Container, Curs) then
         raise Constraint_Error;
      end if;

      while Curs.Node /= 0 loop
         Node := Curs.Node;
         Delete (C, Curs);
         Curs := Next (Container, (Node => Node));
      end loop;

      return C;
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

   procedure Move
     (Target : in out Map;
      Source : in out Map)
   is
      NN   : HT_Types.Nodes_Type renames Source.Nodes;
      X, Y : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      if Source.Length = 0 then
         return;
      end if;

      X := HT_Ops.First (Source);
      while X /= 0 loop
         Insert (Target, NN (X).Key, NN (X).Element);  -- optimize???

         Y := HT_Ops.Next (Source, X);

         HT_Ops.Delete_Node_Sans_Free (Source, X);
         Free (Source, X);

         X := Y;
      end loop;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Node : Node_Type) return Count_Type is
   begin
      return Node.Next;
   end Next;

   function Next (Container : Map; Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error
           with "Position has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in function Next");

      declare
         Node : constant Count_Type := HT_Ops.Next (Container, Position.Node);

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return (Node => Node);
      end;
   end Next;

   procedure Next (Container : Map; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Map) return Boolean is
      Left_Node  : Count_Type;
      Left_Nodes : Nodes_Type renames Left.Nodes;

   begin
      if Length (Right) = 0 or Length (Left) = 0 then
         return False;
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      Left_Node := First (Left).Node;
      while Left_Node /= 0 loop
         declare
            N : Node_Type renames Left_Nodes (Left_Node);
            E : Key_Type renames N.Key;
         begin
            if Find (Right, E).Node /= 0 then
               return True;
            end if;
         end;

         Left_Node := HT_Ops.Next (Left, Left_Node);
      end loop;

      return False;
   end Overlap;

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
         raise Constraint_Error with
           "attempt to replace key not in map";
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
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Replace_Element has no element";
      end if;

      pragma Assert (Vet (Container, Position),
                     "bad cursor in Replace_Element");

      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Map;
      Capacity  : Count_Type)
   is
   begin
      if Capacity > Container.Capacity then
         raise Capacity_Error with "requested capacity is too large";
      end if;
   end Reserve_Capacity;

   -----------
   -- Right --
   -----------

   function Right (Container : Map; Position : Cursor) return Map is
      Curs : Cursor := First (Container);
      C    : Map (Container.Capacity, Container.Modulus) :=
        Copy (Container, Container.Capacity);
      Node : Count_Type;

   begin
      if Curs = No_Element then
         Clear (C);
         return C;
      end if;

      if Position /= No_Element and not Has_Element (Container, Position) then
         raise Constraint_Error;
      end if;

      while Curs.Node /= Position.Node loop
         Node := Curs.Node;
         Delete (C, Curs);
         Curs := Next (Container, (Node => Node));
      end loop;

      return C;
   end Right;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Node : in out Node_Type; Next : Count_Type) is
   begin
      Node.Next := Next;
   end Set_Next;

   ------------------
   -- Strict_Equal --
   ------------------

   function Strict_Equal (Left, Right : Map) return Boolean is
      CuL : Cursor := First (Left);
      CuR : Cursor := First (Right);

   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      while CuL.Node /= 0 or CuR.Node /= 0 loop
         if CuL.Node /= CuR.Node or else
           (Left.Nodes (CuL.Node).Element /=
              Right.Nodes (CuR.Node).Element or
              Left.Nodes (CuL.Node).Key /=
              Right.Nodes (CuR.Node).Key) then
            return False;
         end if;

         CuL := Next (Left, CuL);
         CuR := Next (Right, CuR);
      end loop;

      return True;
   end Strict_Equal;

   ---------
   -- Vet --
   ---------

   function Vet (Container : Map; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return True;
      end if;

      declare
         X : Count_Type;

      begin
         if Container.Length = 0 then
            return False;
         end if;

         if Container.Capacity = 0 then
            return False;
         end if;

         if Container.Buckets'Length = 0 then
            return False;
         end if;

         if Position.Node > Container.Capacity then
            return False;
         end if;

         if Container.Nodes (Position.Node).Next = Position.Node then
            return False;
         end if;

         X := Container.Buckets
           (Key_Ops.Index (Container, Container.Nodes (Position.Node).Key));

         for J in 1 .. Container.Length loop
            if X = Position.Node then
               return True;
            end if;

            if X = 0 then
               return False;
            end if;

            if X = Container.Nodes (X).Next then

               --  Prevent unnecessary looping

               return False;
            end if;

            X := Container.Nodes (X).Next;
         end loop;

         return False;
      end;
   end Vet;

end Ada.Containers.Formal_Hashed_Maps;
