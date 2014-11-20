------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . B O U N D E D _ H A S H E D _ S E T S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2014, Free Software Foundation, Inc.         --
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

with Ada.Containers.Hash_Tables.Generic_Bounded_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Operations);

with Ada.Containers.Hash_Tables.Generic_Bounded_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Keys);

with Ada.Containers.Prime_Numbers; use Ada.Containers.Prime_Numbers;

with System; use type System.Address;

package body Ada.Containers.Bounded_Hashed_Sets is

   pragma Annotate (CodePeer, Skip_Analysis);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Equivalent_Keys
     (Key  : Element_Type;
      Node : Node_Type) return Boolean;
   pragma Inline (Equivalent_Keys);

   function Hash_Node (Node : Node_Type) return Hash_Type;
   pragma Inline (Hash_Node);

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean);

   function Is_In (HT : Set; Key : Node_Type) return Boolean;
   pragma Inline (Is_In);

   procedure Set_Element (Node : in out Node_Type; Item : Element_Type);
   pragma Inline (Set_Element);

   function Next (Node : Node_Type) return Count_Type;
   pragma Inline (Next);

   procedure Set_Next (Node : in out Node_Type; Next : Count_Type);
   pragma Inline (Set_Next);

   function Vet (Position : Cursor) return Boolean;

   --------------------------
   -- Local Instantiations --
   --------------------------

   package HT_Ops is new Hash_Tables.Generic_Bounded_Operations
     (HT_Types  => HT_Types,
      Hash_Node => Hash_Node,
      Next      => Next,
      Set_Next  => Set_Next);

   package Element_Keys is new Hash_Tables.Generic_Bounded_Keys
     (HT_Types        => HT_Types,
      Next            => Next,
      Set_Next        => Set_Next,
      Key_Type        => Element_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   procedure Replace_Element is
      new Element_Keys.Generic_Replace_Element (Hash_Node, Set_Element);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
      function Find_Equal_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean;
      pragma Inline (Find_Equal_Key);

      function Is_Equal is
        new HT_Ops.Generic_Equal (Find_Equal_Key);

      --------------------
      -- Find_Equal_Key --
      --------------------

      function Find_Equal_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean
      is
         R_Index : constant Hash_Type :=
           Element_Keys.Index (R_HT, L_Node.Element);

         R_Node  : Count_Type := R_HT.Buckets (R_Index);

      begin
         loop
            if R_Node = 0 then
               return False;
            end if;

            if L_Node.Element = R_HT.Nodes (R_Node).Element then
               return True;
            end if;

            R_Node := Next (R_HT.Nodes (R_Node));
         end loop;
      end Find_Equal_Key;

   --  Start of processing for "="

   begin
      return Is_Equal (Left, Right);
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            C : Set renames Control.Container.all;
            B : Natural renames C.Busy;
            L : Natural renames C.Lock;
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
      procedure Insert_Element (Source_Node : Count_Type);

      procedure Insert_Elements is
         new HT_Ops.Generic_Iteration (Insert_Element);

      --------------------
      -- Insert_Element --
      --------------------

      procedure Insert_Element (Source_Node : Count_Type) is
         N : Node_Type renames Source.Nodes (Source_Node);
         X : Count_Type;
         B : Boolean;
      begin
         Insert (Target, N.Element, X, B);
         pragma Assert (B);
      end Insert_Element;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Capacity_Error
           with "Target capacity is less than Source length";
      end if;

      HT_Ops.Clear (Target);
      Insert_Elements (Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Set) return Count_Type is
   begin
      return Container.Capacity;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Set) is
   begin
      HT_Ops.Clear (Container);
   end Clear;

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

      pragma Assert (Vet (Position), "bad cursor in Constant_Reference");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         B : Natural renames Position.Container.Busy;
         L : Natural renames Position.Container.Lock;

      begin
         return R : constant Constant_Reference_Type :=
            (Element => N.Element'Access,
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

   function Copy
     (Source   : Set;
      Capacity : Count_Type := 0;
      Modulus  : Hash_Type := 0) return Set
   is
      C : Count_Type;
      M : Hash_Type;

   begin
      if Capacity = 0 then
         C := Source.Length;
      elsif Capacity >= Source.Length then
         C := Capacity;
      else
         raise Capacity_Error with "Capacity value too small";
      end if;

      if Modulus = 0 then
         M := Default_Modulus (C);
      else
         M := Modulus;
      end if;

      return Target : Set (Capacity => C, Modulus => M) do
         Assign (Target => Target, Source => Source);
      end return;
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

   procedure Delete
     (Container : in out Set;
      Item      : Element_Type)
   is
      X : Count_Type;

   begin
      Element_Keys.Delete_Key_Sans_Free (Container, Item, X);

      if X = 0 then
         raise Constraint_Error with "attempt to delete element not in set";
      end if;

      HT_Ops.Free (Container, X);
   end Delete;

   procedure Delete
     (Container : in out Set;
      Position  : in out Cursor)
   is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor designates wrong set";
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (set is busy)";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");

      HT_Ops.Delete_Node_Sans_Free (Container, Position.Node);
      HT_Ops.Free (Container, Position.Node);

      Position := No_Element;
   end Delete;

   ----------------
   -- Difference --
   ----------------

   procedure Difference
     (Target : in out Set;
      Source : Set)
   is
      Tgt_Node, Src_Node : Count_Type;

      Src : Set renames Source'Unrestricted_Access.all;

      TN : Nodes_Type renames Target.Nodes;
      SN : Nodes_Type renames Source.Nodes;

   begin
      if Target'Address = Source'Address then
         HT_Ops.Clear (Target);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (set is busy)";
      end if;

      if Source.Length < Target.Length then
         Src_Node := HT_Ops.First (Source);
         while Src_Node /= 0 loop
            Tgt_Node := Element_Keys.Find (Target, SN (Src_Node).Element);

            if Tgt_Node /= 0 then
               HT_Ops.Delete_Node_Sans_Free (Target, Tgt_Node);
               HT_Ops.Free (Target, Tgt_Node);
            end if;

            Src_Node := HT_Ops.Next (Src, Src_Node);
         end loop;

      else
         Tgt_Node := HT_Ops.First (Target);
         while Tgt_Node /= 0 loop
            if Is_In (Source, TN (Tgt_Node)) then
               declare
                  X : constant Count_Type := Tgt_Node;
               begin
                  Tgt_Node := HT_Ops.Next (Target, Tgt_Node);
                  HT_Ops.Delete_Node_Sans_Free (Target, X);
                  HT_Ops.Free (Target, X);
               end;

            else
               Tgt_Node := HT_Ops.Next (Target, Tgt_Node);
            end if;
         end loop;
      end if;
   end Difference;

   function Difference (Left, Right : Set) return Set is
   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Left.Length = 0 then
         return Empty_Set;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      return Result : Set (Left.Length, To_Prime (Left.Length)) do
         Iterate_Left : declare
            procedure Process (L_Node : Count_Type);

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (L_Node : Count_Type) is
               N : Node_Type renames Left.Nodes (L_Node);
               X : Count_Type;
               B : Boolean;
            begin
               if not Is_In (Right, N) then
                  Insert (Result, N.Element, X, B);  --  optimize this ???
                  pragma Assert (B);
                  pragma Assert (X > 0);
               end if;
            end Process;

         --  Start of processing for Iterate_Left

         begin
            Iterate (Left);
         end Iterate_Left;
      end return;
   end Difference;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Element");

      declare
         S : Set renames Position.Container.all;
         N : Node_Type renames S.Nodes (Position.Node);
      begin
         return N.Element;
      end;
   end Element;

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
      function Find_Equivalent_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean;
      pragma Inline (Find_Equivalent_Key);

      function Is_Equivalent is
         new HT_Ops.Generic_Equal (Find_Equivalent_Key);

      -------------------------
      -- Find_Equivalent_Key --
      -------------------------

      function Find_Equivalent_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean
      is
         R_Index : constant Hash_Type :=
           Element_Keys.Index (R_HT, L_Node.Element);

         R_Node  : Count_Type := R_HT.Buckets (R_Index);

         RN      : Nodes_Type renames R_HT.Nodes;

      begin
         loop
            if R_Node = 0 then
               return False;
            end if;

            if Equivalent_Elements (L_Node.Element, RN (R_Node).Element) then
               return True;
            end if;

            R_Node := Next (R_HT.Nodes (R_Node));
         end loop;
      end Find_Equivalent_Key;

   --  Start of processing for Equivalent_Sets

   begin
      return Is_Equivalent (Left, Right);
   end Equivalent_Sets;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements (Left, Right : Cursor)
     return Boolean is

   begin
      if Left.Node = 0 then
         raise Constraint_Error with
           "Left cursor of Equivalent_Elements equals No_Element";
      end if;

      if Right.Node = 0 then
         raise Constraint_Error with
           "Right cursor of Equivalent_Elements equals No_Element";
      end if;

      pragma Assert (Vet (Left), "bad Left cursor in Equivalent_Elements");
      pragma Assert (Vet (Right), "bad Right cursor in Equivalent_Elements");

      --  AI05-0022 requires that a container implementation detect element
      --  tampering by a generic actual subprogram. However, the following case
      --  falls outside the scope of that AI. Randy Brukardt explained on the
      --  ARG list on 2013/02/07 that:

      --  (Begin Quote):
      --  But for an operation like "<" [the ordered set analog of
      --  Equivalent_Elements], there is no need to "dereference" a cursor
      --  after the call to the generic formal parameter function, so nothing
      --  bad could happen if tampering is undetected. And the operation can
      --  safely return a result without a problem even if an element is
      --  deleted from the container.
      --  (End Quote).

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
         RN : Node_Type renames Right.Container.Nodes (Right.Node);
      begin
         return Equivalent_Elements (LN.Element, RN.Element);
      end;
   end Equivalent_Elements;

   function Equivalent_Elements
     (Left  : Cursor;
      Right : Element_Type) return Boolean
   is
   begin
      if Left.Node = 0 then
         raise Constraint_Error with
           "Left cursor of Equivalent_Elements equals No_Element";
      end if;

      pragma Assert (Vet (Left), "Left cursor in Equivalent_Elements is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
      begin
         return Equivalent_Elements (LN.Element, Right);
      end;
   end Equivalent_Elements;

   function Equivalent_Elements
     (Left  : Element_Type;
      Right : Cursor) return Boolean
   is
   begin
      if Right.Node = 0 then
         raise Constraint_Error with
           "Right cursor of Equivalent_Elements equals No_Element";
      end if;

      pragma Assert
        (Vet (Right),
         "Right cursor of Equivalent_Elements is bad");

      declare
         RN : Node_Type renames Right.Container.Nodes (Right.Node);
      begin
         return Equivalent_Elements (Left, RN.Element);
      end;
   end Equivalent_Elements;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys
     (Key  : Element_Type;
      Node : Node_Type) return Boolean
   is
   begin
      return Equivalent_Elements (Key, Node.Element);
   end Equivalent_Keys;

   -------------
   -- Exclude --
   -------------

   procedure Exclude
     (Container : in out Set;
      Item      : Element_Type)
   is
      X : Count_Type;
   begin
      Element_Keys.Delete_Key_Sans_Free (Container, Item, X);
      HT_Ops.Free (Container, X);
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

   procedure Finalize (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            C : Set renames Control.Container.all;
            B : Natural renames C.Busy;
            L : Natural renames C.Lock;
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

   function Find
     (Container : Set;
      Item      : Element_Type) return Cursor
   is
      Node : constant Count_Type :=
               Element_Keys.Find (Container'Unrestricted_Access.all, Item);
   begin
      return (if Node = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
      Node : constant Count_Type := HT_Ops.First (Container);
   begin
      return (if Node = 0 then No_Element
              else Cursor'(Container'Unrestricted_Access, Node));
   end First;

   overriding function First (Object : Iterator) return Cursor is
   begin
      return Object.Container.First;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      return Position.Node /= 0;
   end Has_Element;

   ---------------
   -- Hash_Node --
   ---------------

   function Hash_Node (Node : Node_Type) return Hash_Type is
   begin
      return Hash (Node.Element);
   end Hash_Node;

   -------------
   -- Include --
   -------------

   procedure Include
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         if Container.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (set is locked)";
         end if;

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
      Insert (Container, New_Item, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;
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

      if not Inserted then
         raise Constraint_Error with
           "attempt to insert element already in set";
      end if;
   end Insert;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Node      : out Count_Type;
      Inserted  : out Boolean)
   is
      procedure Allocate_Set_Element (Node : in out Node_Type);
      pragma Inline (Allocate_Set_Element);

      function New_Node return Count_Type;
      pragma Inline (New_Node);

      procedure Local_Insert is
        new Element_Keys.Generic_Conditional_Insert (New_Node);

      procedure Allocate is
         new HT_Ops.Generic_Allocate (Allocate_Set_Element);

      ---------------------------
      --  Allocate_Set_Element --
      ---------------------------

      procedure Allocate_Set_Element (Node : in out Node_Type) is
      begin
         Node.Element := New_Item;
      end Allocate_Set_Element;

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
      --  The buckets array length is specified by the user as a discriminant
      --  of the container type, so it is possible for the buckets array to
      --  have a length of zero. We must check for this case specifically, in
      --  order to prevent divide-by-zero errors later, when we compute the
      --  buckets array index value for an element, given its hash value.

      if Container.Buckets'Length = 0 then
         raise Capacity_Error with "No capacity for insertion";
      end if;

      Local_Insert (Container, New_Item, Node, Inserted);
   end Insert;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection
     (Target : in out Set;
      Source : Set)
   is
      Tgt_Node : Count_Type;
      TN       : Nodes_Type renames Target.Nodes;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Length = 0 then
         HT_Ops.Clear (Target);
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (set is busy)";
      end if;

      Tgt_Node := HT_Ops.First (Target);
      while Tgt_Node /= 0 loop
         if Is_In (Source, TN (Tgt_Node)) then
            Tgt_Node := HT_Ops.Next (Target, Tgt_Node);

         else
            declare
               X : constant Count_Type := Tgt_Node;
            begin
               Tgt_Node := HT_Ops.Next (Target, Tgt_Node);
               HT_Ops.Delete_Node_Sans_Free (Target, X);
               HT_Ops.Free (Target, X);
            end;
         end if;
      end loop;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
      C : Count_Type;

   begin
      if Left'Address = Right'Address then
         return Left;
      end if;

      C := Count_Type'Min (Left.Length, Right.Length);

      if C = 0 then
         return Empty_Set;
      end if;

      return Result : Set (C, To_Prime (C)) do
         Iterate_Left : declare
            procedure Process (L_Node : Count_Type);

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (L_Node : Count_Type) is
               N : Node_Type renames Left.Nodes (L_Node);
               X : Count_Type;
               B : Boolean;

            begin
               if Is_In (Right, N) then
                  Insert (Result, N.Element, X, B);  -- optimize ???
                  pragma Assert (B);
                  pragma Assert (X > 0);
               end if;
            end Process;

         --  Start of processing for Iterate_Left

         begin
            Iterate (Left);
         end Iterate_Left;
      end return;
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Is_In --
   -----------

   function Is_In (HT : Set; Key : Node_Type) return Boolean is
   begin
      return Element_Keys.Find (HT'Unrestricted_Access.all, Key.Element) /= 0;
   end Is_In;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
      Subset_Node : Count_Type;
      SN          : Nodes_Type renames Subset.Nodes;

   begin
      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      if Subset.Length > Of_Set.Length then
         return False;
      end if;

      Subset_Node := HT_Ops.First (Subset);
      while Subset_Node /= 0 loop
         if not Is_In (Of_Set, SN (Subset_Node)) then
            return False;
         end if;
         Subset_Node := HT_Ops.Next
                          (Subset'Unrestricted_Access.all, Subset_Node);
      end loop;

      return True;
   end Is_Subset;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process_Node);

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
         Iterate (Container);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   function Iterate (Container : Set)
     return Set_Iterator_Interfaces.Forward_Iterator'Class
   is
      B : Natural renames Container'Unrestricted_Access.all.Busy;
   begin
      B := B + 1;
      return It : constant Iterator :=
        Iterator'(Limited_Controlled with
                    Container => Container'Unrestricted_Access);
   end Iterate;

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

   function Next (Node : Node_Type) return Count_Type is
   begin
      return Node.Next;
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Next");

      declare
         HT   : Set renames Position.Container.all;
         Node : constant Count_Type := HT_Ops.Next (HT, Position.Node);

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

   function Next
     (Object : Iterator;
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
      Left_Node : Count_Type;

   begin
      if Right.Length = 0 then
         return False;
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      Left_Node := HT_Ops.First (Left);
      while Left_Node /= 0 loop
         if Is_In (Right, Left.Nodes (Left_Node)) then
            return True;
         end if;
         Left_Node := HT_Ops.Next (Left'Unrestricted_Access.all, Left_Node);
      end loop;

      return False;
   end Overlap;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         S : Set renames Position.Container.all;
         B : Natural renames S.Busy;
         L : Natural renames S.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (S.Nodes (Position.Node).Element);
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
            new HT_Ops.Generic_Allocate (Read_Element);

         procedure Read_Element (Node : in out Node_Type) is
         begin
            Element_Type'Read (Stream, Node.Element);
         end Read_Element;

         Node : Count_Type;

      --  Start of processing for Read_Node

      begin
         Allocate (Container, Node);
         return Node;
      end Read_Node;

   --  Start of processing for Read

   begin
      Read_Nodes (Stream, Container);
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

   procedure Replace
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Node : constant Count_Type := Element_Keys.Find (Container, New_Item);

   begin
      if Node = 0 then
         raise Constraint_Error with
           "attempt to replace element not in set";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (set is locked)";
      end if;

      Container.Nodes (Node).Element := New_Item;
   end Replace;

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor equals No_Element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      Replace_Element (Container, Position.Node, New_Item);
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Set;
      Capacity  : Count_Type)
   is
   begin
      if Capacity > Container.Capacity then
         raise Capacity_Error with "requested capacity is too large";
      end if;
   end Reserve_Capacity;

   ------------------
   --  Set_Element --
   ------------------

   procedure Set_Element (Node : in out Node_Type; Item : Element_Type) is
   begin
      Node.Element := Item;
   end Set_Element;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Node : in out Node_Type; Next : Count_Type) is
   begin
      Node.Next := Next;
   end Set_Next;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference
     (Target : in out Set;
      Source : Set)
   is
      procedure Process (Source_Node : Count_Type);
      pragma Inline (Process);

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Source_Node : Count_Type) is
         N : Node_Type renames Source.Nodes (Source_Node);
         X : Count_Type;
         B : Boolean;

      begin
         if Is_In (Target, N) then
            Delete (Target, N.Element);
         else
            Insert (Target, N.Element, X, B);
            pragma Assert (B);
         end if;
      end Process;

   --  Start of processing for Symmetric_Difference

   begin
      if Target'Address = Source'Address then
         HT_Ops.Clear (Target);
         return;
      end if;

      if Target.Length = 0 then
         Assign (Target => Target, Source => Source);
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (set is busy)";
      end if;

      Iterate (Source);
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
      C : Count_Type;

   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      if Left.Length = 0 then
         return Right;
      end if;

      C := Left.Length + Right.Length;

      return Result : Set (C, To_Prime (C)) do
         Iterate_Left : declare
            procedure Process (L_Node : Count_Type);

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (L_Node : Count_Type) is
               N : Node_Type renames Left.Nodes (L_Node);
               X : Count_Type;
               B : Boolean;
            begin
               if not Is_In (Right, N) then
                  Insert (Result, N.Element, X, B);
                  pragma Assert (B);
               end if;
            end Process;

         --  Start of processing for Iterate_Left

         begin
            Iterate (Left);
         end Iterate_Left;

         Iterate_Right : declare
            procedure Process (R_Node : Count_Type);

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (R_Node : Count_Type) is
               N : Node_Type renames Right.Nodes (R_Node);
               X : Count_Type;
               B : Boolean;
            begin
               if not Is_In (Left, N) then
                  Insert (Result, N.Element, X, B);
                  pragma Assert (B);
               end if;
            end Process;

         --  Start of processing for Iterate_Right

         begin
            Iterate (Right);
         end Iterate_Right;
      end return;
   end Symmetric_Difference;

   ------------
   -- To_Set --
   ------------

   function To_Set (New_Item : Element_Type) return Set is
      X : Count_Type;
      B : Boolean;
   begin
      return Result : Set (1, 1) do
         Insert (Result, New_Item, X, B);
         pragma Assert (B);
      end return;
   end To_Set;

   -----------
   -- Union --
   -----------

   procedure Union
     (Target : in out Set;
      Source : Set)
   is
      procedure Process (Src_Node : Count_Type);

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Src_Node : Count_Type) is
         N : Node_Type renames Source.Nodes (Src_Node);
         X : Count_Type;
         B : Boolean;
      begin
         Insert (Target, N.Element, X, B);
      end Process;

   --  Start of processing for Union

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (set is busy)";
      end if;

      --  ??? why is this code commented out ???
      --  declare
      --     N : constant Count_Type := Target.Length + Source.Length;
      --  begin
      --     if N > HT_Ops.Capacity (Target.HT) then
      --        HT_Ops.Reserve_Capacity (Target.HT, N);
      --     end if;
      --  end;

      Iterate (Source);
   end Union;

   function Union (Left, Right : Set) return Set is
      C : Count_Type;

   begin
      if Left'Address = Right'Address then
         return Left;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      if Left.Length = 0 then
         return Right;
      end if;

      C := Left.Length + Right.Length;

      return Result : Set (C, To_Prime (C)) do
         Assign (Target => Result, Source => Left);
         Union (Target => Result, Source => Right);
      end return;
   end Union;

   ---------
   -- Vet --
   ---------

   function Vet (Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return Position.Container = null;
      end if;

      if Position.Container = null then
         return False;
      end if;

      declare
         S : Set renames Position.Container.all;
         N : Nodes_Type renames S.Nodes;
         X : Count_Type;

      begin
         if S.Length = 0 then
            return False;
         end if;

         if Position.Node > N'Last then
            return False;
         end if;

         if N (Position.Node).Next = Position.Node then
            return False;
         end if;

         X := S.Buckets (Element_Keys.Checked_Index
                           (S, N (Position.Node).Element));

         for J in 1 .. S.Length loop
            if X = Position.Node then
               return True;
            end if;

            if X = 0 then
               return False;
            end if;

            if X = N (X).Next then  --  to prevent unnecessary looping
               return False;
            end if;

            X := N (X).Next;
         end loop;

         return False;
      end;
   end Vet;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Set)
   is
      procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type);
      pragma Inline (Write_Node);

      procedure Write_Nodes is
         new HT_Ops.Generic_Write (Write_Node);

      ----------------
      -- Write_Node --
      ----------------

      procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type)
      is
      begin
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
      raise Program_Error with "attempt to stream set cursor";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   package body Generic_Keys is

      -----------------------
      -- Local Subprograms --
      -----------------------

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Control : in out Reference_Control_Type) is
      begin
         if Control.Container /= null then
            declare
               B : Natural renames Control.Container.Busy;
               L : Natural renames Control.Container.Lock;
            begin
               B := B + 1;
               L := L + 1;
            end;
         end if;
      end Adjust;

      function Equivalent_Key_Node
        (Key  : Key_Type;
         Node : Node_Type) return Boolean;
      pragma Inline (Equivalent_Key_Node);

      --------------------------
      -- Local Instantiations --
      --------------------------

      package Key_Keys is
         new Hash_Tables.Generic_Bounded_Keys
          (HT_Types        => HT_Types,
           Next            => Next,
           Set_Next        => Set_Next,
           Key_Type        => Key_Type,
           Hash            => Hash,
           Equivalent_Keys => Equivalent_Key_Node);

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Container : aliased Set;
         Key       : Key_Type) return Constant_Reference_Type
      is
         Node : constant Count_Type :=
                  Key_Keys.Find (Container'Unrestricted_Access.all, Key);

      begin
         if Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         declare
            Cur  : Cursor := Find (Container, Key);
            pragma Unmodified (Cur);

            N : Node_Type renames Container.Nodes (Node);
            B : Natural renames Cur.Container.Busy;
            L : Natural renames Cur.Container.Lock;

         begin
            return R : constant Constant_Reference_Type :=
              (Element => N.Element'Access,
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

      function Contains
        (Container : Set;
         Key       : Key_Type) return Boolean
      is
      begin
         return Find (Container, Key) /= No_Element;
      end Contains;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Container : in out Set;
         Key       : Key_Type)
      is
         X : Count_Type;

      begin
         Key_Keys.Delete_Key_Sans_Free (Container, Key, X);

         if X = 0 then
            raise Constraint_Error with "attempt to delete key not in set";
         end if;

         HT_Ops.Free (Container, X);
      end Delete;

      -------------
      -- Element --
      -------------

      function Element
        (Container : Set;
         Key       : Key_Type) return Element_Type
      is
         Node : constant Count_Type :=
                  Key_Keys.Find (Container'Unrestricted_Access.all, Key);

      begin
         if Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         return Container.Nodes (Node).Element;
      end Element;

      -------------------------
      -- Equivalent_Key_Node --
      -------------------------

      function Equivalent_Key_Node
        (Key  : Key_Type;
         Node : Node_Type) return Boolean
      is
      begin
         return Equivalent_Keys (Key, Generic_Keys.Key (Node.Element));
      end Equivalent_Key_Node;

      -------------
      -- Exclude --
      -------------

      procedure Exclude
        (Container : in out Set;
         Key       : Key_Type)
      is
         X : Count_Type;
      begin
         Key_Keys.Delete_Key_Sans_Free (Container, Key, X);
         HT_Ops.Free (Container, X);
      end Exclude;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Control : in out Reference_Control_Type) is
      begin
         if Control.Container /= null then
            declare
               B : Natural renames Control.Container.Busy;
               L : Natural renames Control.Container.Lock;
            begin
               B := B - 1;
               L := L - 1;
            end;

            if Hash (Key (Element (Control.Old_Pos))) /= Control.Old_Hash
            then
               HT_Ops.Delete_Node_At_Index
                 (Control.Container.all, Control.Index, Control.Old_Pos.Node);
               raise Program_Error with "key not preserved in reference";
            end if;

            Control.Container := null;
         end if;
      end Finalize;

      ----------
      -- Find --
      ----------

      function Find
        (Container : Set;
         Key       : Key_Type) return Cursor
      is
         Node : constant Count_Type :=
                  Key_Keys.Find (Container'Unrestricted_Access.all, Key);
      begin
         return (if Node = 0 then No_Element
                 else Cursor'(Container'Unrestricted_Access, Node));
      end Find;

      ---------
      -- Key --
      ---------

      function Key (Position : Cursor) return Key_Type is
      begin
         if Position.Node = 0 then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         pragma Assert (Vet (Position), "bad cursor in function Key");
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
         if Position.Container = null then
            raise Constraint_Error with "Position cursor has no element";
         end if;

         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert
           (Vet (Position),
            "bad cursor in function Reference_Preserving_Key");

         declare
            N : Node_Type renames Container.Nodes (Position.Node);
            B : Natural renames Container.Busy;
            L : Natural renames Container.Lock;

         begin
            return R : constant Reference_Type :=
              (Element  => N.Element'Unrestricted_Access,
                Control =>
                  (Controlled with
                     Container'Unrestricted_Access,
                     Index    => Key_Keys.Index (Container, Key (Position)),
                     Old_Pos  => Position,
                     Old_Hash => Hash (Key (Position))))
         do
               B := B + 1;
               L := L + 1;
            end return;
         end;
      end Reference_Preserving_Key;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Key       : Key_Type) return Reference_Type
      is
         Node : constant Count_Type := Key_Keys.Find (Container, Key);

      begin
         if Node = 0 then
            raise Constraint_Error with "key not in set";
         end if;

         declare
            P : constant Cursor := Find (Container, Key);
            B : Natural renames Container.Busy;
            L : Natural renames Container.Lock;

         begin
            return R : constant Reference_Type :=
              (Element => Container.Nodes (Node).Element'Unrestricted_Access,
               Control =>
                 (Controlled with
                    Container'Unrestricted_Access,
                    Index  => Key_Keys.Index (Container, Key),
                    Old_Pos => P,
                    Old_Hash => Hash (Key)))
            do
               B := B + 1;
               L := L + 1;
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
         if Node = 0 then
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
         Process   : not null access
                       procedure (Element : in out Element_Type))
      is
         Indx : Hash_Type;
         N    : Nodes_Type renames Container.Nodes;

      begin
         if Position.Node = 0 then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Position cursor designates wrong set";
         end if;

         --  ??? why is this code commented out ???
         --  if HT.Buckets = null
         --    or else HT.Buckets'Length = 0
         --    or else HT.Length = 0
         --    or else Position.Node.Next = Position.Node
         --  then
         --     raise Program_Error with
         --        "Position cursor is bad (set is empty)";
         --  end if;

         pragma Assert
           (Vet (Position),
            "bad cursor in Update_Element_Preserving_Key");

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            E : Element_Type renames N (Position.Node).Element;
            K : constant Key_Type := Key (E);

            B : Natural renames Container.Busy;
            L : Natural renames Container.Lock;

            Eq : Boolean;

         begin
            B := B + 1;
            L := L + 1;

            begin
               --  Record bucket now, in case key is changed
               Indx := HT_Ops.Index (Container.Buckets, N (Position.Node));

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

         --  Key was modified, so remove this node from set.

         if Container.Buckets (Indx) = Position.Node then
            Container.Buckets (Indx) := N (Position.Node).Next;

         else
            declare
               Prev : Count_Type := Container.Buckets (Indx);

            begin
               while N (Prev).Next /= Position.Node loop
                  Prev := N (Prev).Next;

                  if Prev = 0 then
                     raise Program_Error with
                       "Position cursor is bad (node not found)";
                  end if;
               end loop;

               N (Prev).Next := N (Position.Node).Next;
            end;
         end if;

         Container.Length := Container.Length - 1;
         HT_Ops.Free (Container, Position.Node);

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

end Ada.Containers.Bounded_Hashed_Sets;
