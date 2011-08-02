------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . F O R M A L _ H A S H E D _ M A P S     --
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

   function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Type) return Boolean;
   pragma Inline (Equivalent_Keys);

   function Find_Between
     (HT   : Hash_Table_Type;
      Key  : Key_Type;
      From : Count_Type;
      To   : Count_Type) return Count_Type;

   procedure Free
     (HT : in out Hash_Table_Type;
      X  : Count_Type);

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (HT   : in out Hash_Table_Type;
      Node : out Count_Type);

   function Hash_Node (Node : Node_Type) return Hash_Type;
   pragma Inline (Hash_Node);

   function Next_Unchecked
     (Container : Map;
      Position  : Cursor) return Cursor;

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
       (HT_Types        => HT_Types,
        Hash_Node       => Hash_Node,
        Next            => Next,
        Set_Next        => Set_Next);

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
         Node  : Count_Type := First (Left).Node;
         ENode : Count_Type;
         Last  : Count_Type;
      begin

         if Left.K = Plain then
            Last := 0;
         else
            Last := HT_Ops.Next (Left.HT.all, Left.Last);
         end if;

         while Node /= Last loop
            ENode := Find (Container => Right,
                           Key       => Left.HT.Nodes (Node).Key).Node;
            if ENode = 0 or else
              Right.HT.Nodes (ENode).Element /= Left.HT.Nodes (Node).Element
            then
               return False;
            end if;

            Node := HT_Ops.Next (Left.HT.all, Node);
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
         N : Node_Type renames Source.HT.Nodes (Source_Node);
      begin
         Target.Insert (N.Key, N.Element);
      end Insert_Element;

      --  Start of processing for Assign

   begin
      if Target.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Length (Source) then
         raise Constraint_Error with  -- correct exception ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);  -- checks busy bits

      case Source.K is
         when Plain =>
            Insert_Elements (Source.HT.all);
         when Part =>
            declare
               N : Count_Type := Source.First;
            begin
               while N /= HT_Ops.Next (Source.HT.all, Source.Last) loop
                  Insert_Element (N);
                  N := HT_Ops.Next (Source.HT.all, N);
               end loop;
            end;
      end case;
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Map) return Count_Type is
   begin
      return Container.HT.Nodes'Length;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      HT_Ops.Clear (Container.HT.all);
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
      H      : Hash_Type := 1;
      N      : Count_Type := 1;
      Target : Map (C, Source.Modulus);
      Cu     : Cursor;
   begin
      if (Source.K = Part and Source.Length = 0) or
        Source.HT.Length = 0 then
         return Target;
      end if;

      Target.HT.Length := Source.HT.Length;
      Target.HT.Free := Source.HT.Free;
      while H <= Source.Modulus loop
         Target.HT.Buckets (H) := Source.HT.Buckets (H);
         H := H + 1;
      end loop;
      while N <= Source.Capacity loop
         Target.HT.Nodes (N) := Source.HT.Nodes (N);
         N := N + 1;
      end loop;
      while N <= C loop
         Cu := (Node => N);
         Free (Target.HT.all, Cu.Node);
         N := N + 1;
      end loop;
      if Source.K = Part then
         N := HT_Ops.First (Target.HT.all);
         while N /= Source.First loop
            Cu := (Node => N);
            N := HT_Ops.Next (Target.HT.all, N);
            Delete (Target, Cu);
         end loop;
         N := HT_Ops.Next (Target.HT.all, Source.Last);
         while N /= 0 loop
            Cu := (Node => N);
            N := HT_Ops.Next (Target.HT.all, N);
            Delete (Target, Cu);
         end loop;
      end if;
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

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      Key_Ops.Delete_Key_Sans_Free (Container.HT.all, Key, X);

      if X = 0 then
         raise Constraint_Error with "attempt to delete key not in map";
      end if;

      Free (Container.HT.all, X);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Delete has no element";
      end if;

      if Container.HT.Busy > 0 then
         raise Program_Error with
           "Delete attempted to tamper with elements (map is busy)";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in Delete");

      HT_Ops.Delete_Node_Sans_Free (Container.HT.all, Position.Node);

      Free (Container.HT.all, Position.Node);
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

      return Container.HT.Nodes (Node).Element;
   end Element;

   function Element (Container : Map; Position : Cursor) return Element_Type is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Container, Position),
                     "bad cursor in function Element");

      return Container.HT.Nodes (Position.Node).Element;
   end Element;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Type) return Boolean is
   begin
      return Equivalent_Keys (Key, Node.Key);
   end Equivalent_Keys;

   function Equivalent_Keys (Left  : Map; CLeft : Cursor;
                             Right : Map; CRight : Cursor)
                             return Boolean is
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
         LT : Hash_Table_Type renames Left.HT.all;
         RT : Hash_Table_Type renames Right.HT.all;

         LN : Node_Type renames LT.Nodes (CLeft.Node);
         RN : Node_Type renames RT.Nodes (CRight.Node);

      begin
         return Equivalent_Keys (LN.Key, RN.Key);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left  : Map;
      CLeft : Cursor;
      Right : Key_Type) return Boolean is
   begin
      if not Has_Element (Left, CLeft) then
         raise Constraint_Error with
           "Left cursor of Equivalent_Keys has no element";
      end if;

      pragma Assert (Vet (Left, CLeft),
                     "Left cursor in Equivalent_Keys is bad");

      declare
         LT : Hash_Table_Type renames Left.HT.all;
         LN : Node_Type renames LT.Nodes (CLeft.Node);

      begin
         return Equivalent_Keys (LN.Key, Right);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left   : Key_Type;
      Right  : Map;
      CRight : Cursor) return Boolean is
   begin
      if Has_Element (Right, CRight) then
         raise Constraint_Error with
           "Right cursor of Equivalent_Keys has no element";
      end if;

      pragma Assert (Vet (Right, CRight),
                     "Right cursor of Equivalent_Keys is bad");

      declare
         RT : Hash_Table_Type renames Right.HT.all;
         RN : Node_Type renames RT.Nodes (CRight.Node);

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

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      Key_Ops.Delete_Key_Sans_Free (Container.HT.all, Key, X);
      Free (Container.HT.all, X);
   end Exclude;

   ----------
   -- Find --
   ----------
   function Find_Between
     (HT   : Hash_Table_Type;
      Key  : Key_Type;
      From : Count_Type;
      To   : Count_Type) return Count_Type is

      Indx      : Hash_Type;
      Indx_From : constant Hash_Type :=
        Key_Ops.Index (HT, HT.Nodes (From).Key);
      Indx_To   : constant Hash_Type :=
        Key_Ops.Index (HT, HT.Nodes (To).Key);
      Node      : Count_Type;
      To_Node   : Count_Type;

   begin

      Indx := Key_Ops.Index (HT, Key);

      if Indx < Indx_From or Indx > Indx_To then
         return 0;
      end if;

      if Indx = Indx_From then
         Node := From;
      else
         Node := HT.Buckets (Indx);
      end if;

      if Indx = Indx_To then
         To_Node := HT.Nodes (To).Next;
      else
         To_Node := 0;
      end if;

      while Node /= To_Node loop
         if Equivalent_Keys (Key, HT.Nodes (Node)) then
            return Node;
         end if;
         Node := HT.Nodes (Node).Next;
      end loop;
      return 0;
   end Find_Between;

   function Find (Container : Map; Key : Key_Type) return Cursor is
   begin
      case Container.K is
         when Plain =>
            declare
               Node : constant Count_Type :=
                        Key_Ops.Find (Container.HT.all, Key);

            begin
               if Node = 0 then
                  return No_Element;
               end if;

               return (Node => Node);
            end;
         when Part =>
            if Container.Length = 0 then
               return No_Element;
            end if;

            return (Node => Find_Between (Container.HT.all, Key,
                    Container.First, Container.Last));
      end case;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
   begin
      case Container.K is
         when Plain =>
            declare
               Node : constant Count_Type := HT_Ops.First (Container.HT.all);

            begin
               if Node = 0 then
                  return No_Element;
               end if;

               return (Node => Node);
            end;
         when Part =>
            declare
               Node : constant Count_Type := Container.First;

            begin
               if Node = 0 then
                  return No_Element;
               end if;

               return (Node => Node);
            end;
      end case;
   end First;

   ----------
   -- Free --
   ----------

   procedure Free
     (HT : in out Hash_Table_Type;
      X  : Count_Type)
   is
   begin
      HT.Nodes (X).Has_Element := False;
      HT_Ops.Free (HT, X);
   end Free;

   ----------------------
   -- Generic_Allocate --
   ----------------------

   procedure Generic_Allocate
     (HT   : in out Hash_Table_Type;
      Node : out Count_Type)
   is

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
        not Container.HT.Nodes (Position.Node).Has_Element then
         return False;
      end if;

      if Container.K = Plain then
         return True;
      end if;

      declare
         Lst_Index : constant Hash_Type :=
                       Key_Ops.Index (Container.HT.all,
                                      Container.HT.Nodes (Container.Last).Key);
         Fst_Index : constant Hash_Type :=
                       Key_Ops.Index (Container.HT.all,
                                     Container.HT.Nodes (Container.First).Key);
         Index     : constant Hash_Type :=
                       Key_Ops.Index (Container.HT.all,
                                      Container.HT.Nodes (Position.Node).Key);
         Lst_Node  : Count_Type;
         Node      : Count_Type;
      begin

         if Index < Fst_Index or Index > Lst_Index then
            return False;
         end if;

         if Index > Fst_Index and Index < Lst_Index then
            return True;
         end if;

         if Index = Fst_Index then
            Node := Container.First;
         else
            Node := Container.HT.Buckets (Index);
         end if;

         if Index = Lst_Index then
            Lst_Node := Container.HT.Nodes (Container.Last).Next;
         else
            Lst_Node := 0;
         end if;

         while Node /= Lst_Node loop
            if Position.Node = Node then
               return True;
            end if;
            Node := HT_Ops.Next (Container.HT.all, Node);
         end loop;

         return False;
      end;
   end Has_Element;

   ---------------
   -- Hash_Node --
   ---------------

   function Hash_Node
     (Node : Node_Type) return Hash_Type is
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
         if Container.HT.Lock > 0 then
            raise Program_Error with
              "Include attempted to tamper with cursors (map is locked)";
         end if;

         declare
            N : Node_Type renames Container.HT.Nodes (Position.Node);
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
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
   begin

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;
      declare
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
            --  Node.Element := New_Item;
         end Assign_Key;

         --------------
         -- New_Node --
         --------------

         function New_Node return Count_Type is
            Result : Count_Type;
         begin
            Allocate (Container.HT.all, Result);
            return Result;
         end New_Node;

         --  Start of processing for Insert

      begin

         Local_Insert (Container.HT.all, Key, Position.Node, Inserted);
      end;
   end Insert;

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
   begin

      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;
      declare
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
            Allocate (Container.HT.all, Result);
            return Result;
         end New_Node;

         --  Start of processing for Insert

      begin

         Local_Insert (Container.HT.all, Key, Position.Node, Inserted);
      end;
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   :
        not null access procedure (Container : Map; Position : Cursor))
   is
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Local_Iterate is new HT_Ops.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Count_Type) is
      begin
         Process (Container, (Node => Node));
      end Process_Node;

      B : Natural renames Container'Unrestricted_Access.HT.Busy;

      --  Start of processing for Iterate

   begin
      B := B + 1;

      begin
         case Container.K is
            when Plain =>
               Local_Iterate (Container.HT.all);
            when Part =>

               if Container.Length = 0 then
                  return;
               end if;

               declare
                  Node : Count_Type := Container.First;
               begin
                  while Node /= Container.HT.Nodes (Container.Last).Next loop
                     Process_Node (Node);
                     Node := HT_Ops.Next (Container.HT.all, Node);
                  end loop;
               end;
         end case;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

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

      return Container.HT.Nodes (Position.Node).Key;
   end Key;

   ----------
   -- Left --
   ----------

   function Left (Container : Map; Position : Cursor) return Map is
      Lst : Count_Type;
      Fst : constant Count_Type := First (Container).Node;
      L   : Count_Type := 0;
      C   : Count_Type := Fst;
   begin
      while C /= Position.Node loop
         if C = 0 or C = Container.Last then
            raise Constraint_Error with
              "Position cursor has no element";
         end if;
         Lst := C;
         C := HT_Ops.Next (Container.HT.all, C);
         L := L + 1;
      end loop;
      if L = 0 then
         return (Capacity => Container.Capacity,
                 Modulus  => Container.Modulus,
                 K        => Part,
                 HT       => Container.HT,
                 Length   => 0,
                 First    => 0,
                 Last     => 0);
      else
         return (Capacity => Container.Capacity,
                 Modulus  => Container.Modulus,
                 K        => Part,
                 HT       => Container.HT,
                 Length   => L,
                 First    => Fst,
                 Last     => Lst);
      end if;
   end Left;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      case Container.K is
         when Plain =>
            return Container.HT.Length;
         when Part =>
            return Container.Length;
      end case;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Map;
      Source : in out Map)
   is
      HT   : HT_Types.Hash_Table_Type renames Source.HT.all;
      NN   : HT_Types.Nodes_Type renames HT.Nodes;
      X, Y : Count_Type;

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

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      Clear (Target);

      if HT.Length = 0 then
         return;
      end if;

      X := HT_Ops.First (HT);
      while X /= 0 loop
         Insert (Target, NN (X).Key, NN (X).Element);  -- optimize???

         Y := HT_Ops.Next (HT, X);

         HT_Ops.Delete_Node_Sans_Free (HT, X);
         Free (HT, X);

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

   function Next_Unchecked
     (Container : Map;
      Position  : Cursor) return Cursor
   is
      HT   : Hash_Table_Type renames Container.HT.all;
      Node : constant Count_Type := HT_Ops.Next (HT, Position.Node);

   begin
      if Node = 0 then
         return No_Element;
      end if;

      if Container.K = Part and then Container.Last = Position.Node then
         return No_Element;
      end if;

      return (Node => Node);
   end Next_Unchecked;

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

      return Next_Unchecked (Container, Position);
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
      Left_Nodes : Nodes_Type renames Left.HT.Nodes;
      To_Node    : Count_Type;
   begin
      if Length (Right) = 0 or Length (Left) = 0 then
         return False;
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      Left_Node := First (Left).Node;

      if Left.K = Plain then
         To_Node := 0;
      else
         To_Node := Left.HT.Nodes (Left.Last).Next;
      end if;

      while Left_Node /= To_Node loop
         declare
            N : Node_Type renames Left_Nodes (Left_Node);
            E : Key_Type renames N.Key;

         begin
            if Find (Right, E).Node /= 0 then
               return True;
            end if;
         end;

         Left_Node := HT_Ops.Next (Left.HT.all, Left_Node);
      end loop;

      return False;
   end Overlap;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : in out Map;
      Position  : Cursor;
      Process   : not null access
        procedure (Key : Key_Type; Element : Element_Type))
   is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Query_Element has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in Query_Element");

      declare
         HT : Hash_Table_Type renames Container.HT.all;
         N  : Node_Type renames HT.Nodes (Position.Node);

         B : Natural renames HT.Busy;
         L : Natural renames HT.Lock;

      begin
         B := B + 1;
         L := L + 1;

         declare
            K : Key_Type renames N.Key;
            E : Element_Type renames N.Element;

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
      function Read_Node (Stream : not null access Root_Stream_Type'Class)
                          return Count_Type;

      procedure Read_Nodes is
        new HT_Ops.Generic_Read (Read_Node);

      ---------------
      -- Read_Node --
      ---------------

      function Read_Node (Stream : not null access Root_Stream_Type'Class)
                          return Count_Type
      is
         procedure Read_Element (Node : in out Node_Type);
         pragma Inline (Read_Element);

         procedure Allocate is
           new Generic_Allocate (Read_Element);

         procedure Read_Element (Node : in out Node_Type) is
         begin
            Element_Type'Read (Stream, Node.Element);
         end Read_Element;

         Node : Count_Type;

         --  Start of processing for Read_Node

      begin
         Allocate (Container.HT.all, Node);
         return Node;
      end Read_Node;

      --  Start of processing for Read
      Result : HT_Access;
   begin
      if Container.K /= Plain then
         raise Constraint_Error;
      end if;

      if Container.HT = null then
         Result := new HT_Types.Hash_Table_Type (Container.Capacity,
                                                 Container.Modulus);
      else
         Result := Container.HT;
      end if;

      Read_Nodes (Stream, Result.all);
      Container.HT := Result;
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

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Node : constant Count_Type := Key_Ops.Find (Container.HT.all, Key);

   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Node = 0 then
         raise Constraint_Error with
           "attempt to replace key not in map";
      end if;

      if Container.HT.Lock > 0 then
         raise Program_Error with
           "Replace attempted to tamper with cursors (map is locked)";
      end if;

      declare
         N : Node_Type renames Container.HT.Nodes (Node);
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
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Replace_Element has no element";
      end if;

      if Container.HT.Lock > 0 then
         raise Program_Error with
           "Replace_Element attempted to tamper with cursors (map is locked)";
      end if;

      pragma Assert (Vet (Container, Position),
                     "bad cursor in Replace_Element");

      Container.HT.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Map;
      Capacity  : Count_Type)
   is
   begin
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if Capacity > Container.Capacity then
         raise Capacity_Error with "requested capacity is too large";
      end if;
   end Reserve_Capacity;

   -----------
   -- Right --
   -----------

   function Right (Container : Map; Position : Cursor) return Map is
      Last : Count_Type;
      Lst  : Count_Type;
      L    : Count_Type := 0;
      C    : Count_Type := Position.Node;
   begin

      if C = 0 then
         return (Capacity => Container.Capacity,
                 Modulus  => Container.Modulus,
                 K        => Part,
                 HT       => Container.HT,
                 Length   => 0,
                 First    => 0,
                 Last     => 0);
      end if;

      if Container.K = Plain then
         Lst := 0;
      else
         Lst := HT_Ops.Next (Container.HT.all, Container.Last);
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
         Last := C;
         C := HT_Ops.Next (Container.HT.all, C);
         L := L + 1;
      end loop;

      return (Capacity => Container.Capacity,
              Modulus  => Container.Modulus,
              K        => Part,
              HT       => Container.HT,
              Length   => L,
              First    => Position.Node,
              Last     => Last);
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
           (Left.HT.Nodes (CuL.Node).Element /=
              Right.HT.Nodes (CuR.Node).Element or
              Left.HT.Nodes (CuL.Node).Key /=
              Right.HT.Nodes (CuR.Node).Key) then
            return False;
         end if;
         CuL := Next_Unchecked (Left, CuL);
         CuR := Next_Unchecked (Right, CuR);
      end loop;

      return True;
   end Strict_Equal;

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
      if Container.K /= Plain then
         raise Constraint_Error
           with "Can't modify part of container";
      end if;

      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor of Update_Element has no element";
      end if;

      pragma Assert (Vet (Container, Position),
                     "bad cursor in Update_Element");

      declare
         HT : Hash_Table_Type renames Container.HT.all;
         B  : Natural renames HT.Busy;
         L  : Natural renames HT.Lock;

      begin
         B := B + 1;
         L := L + 1;

         declare
            N : Node_Type renames HT.Nodes (Position.Node);
            K : Key_Type renames N.Key;
            E : Element_Type renames N.Element;

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

   ---------
   -- Vet --
   ---------

   function Vet (Container : Map; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return True;
      end if;

      declare
         M : HT_Types.Hash_Table_Type renames Container.HT.all;
         X : Count_Type;

      begin
         if M.Length = 0 then
            return False;
         end if;

         if M.Capacity = 0 then
            return False;
         end if;

         if M.Buckets'Length = 0 then
            return False;
         end if;

         if Position.Node > M.Capacity then
            return False;
         end if;

         if M.Nodes (Position.Node).Next = Position.Node then
            return False;
         end if;

         X := M.Buckets (Key_Ops.Index (M, M.Nodes (Position.Node).Key));

         for J in 1 .. M.Length loop
            if X = Position.Node then
               return True;
            end if;

            if X = 0 then
               return False;
            end if;

            if X = M.Nodes (X).Next then  --  to prevent unnecessary looping
               return False;
            end if;

            X := M.Nodes (X).Next;
         end loop;

         return False;
      end;
   end Vet;

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

      procedure Write_Nodes is new HT_Ops.Generic_Write (Write_Node);

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
      Write_Nodes (Stream, Container.HT.all);
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream map cursor";
   end Write;

end Ada.Containers.Formal_Hashed_Maps;
