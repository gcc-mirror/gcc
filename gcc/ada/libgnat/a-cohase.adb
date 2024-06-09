------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           A D A . C O N T A I N E R S . H A S H E D _ S E T S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2024, Free Software Foundation, Inc.         --
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

with Ada.Containers.Hash_Tables.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Operations);

with Ada.Containers.Hash_Tables.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Keys);

with Ada.Containers.Helpers; use Ada.Containers.Helpers;

with Ada.Containers.Prime_Numbers;

with System; use type System.Address;
with System.Put_Images;

package body Ada.Containers.Hashed_Sets with
  SPARK_Mode => Off
is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Assign (Node : Node_Access; Item : Element_Type);
   pragma Inline (Assign);

   function Copy_Node (Source : Node_Access) return Node_Access;
   pragma Inline (Copy_Node);

   function Equivalent_Keys
     (Key  : Element_Type;
      Node : Node_Access) return Boolean;
   pragma Inline (Equivalent_Keys);

   function Find_Equal_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean;

   function Find_Equivalent_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean;

   procedure Free (X : in out Node_Access);

   function Hash_Node (Node : Node_Access) return Hash_Type;
   pragma Inline (Hash_Node);

   procedure Insert
     (HT       : in out Hash_Table_Type;
      New_Item : Element_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);

   function Is_In
     (HT  : aliased in out Hash_Table_Type;
      Key : Node_Access) return Boolean;
   pragma Inline (Is_In);

   function Next (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   function Read_Node (Stream : not null access Root_Stream_Type'Class)
     return Node_Access;
   pragma Inline (Read_Node);

   procedure Set_Next (Node : Node_Access; Next : Node_Access);
   pragma Inline (Set_Next);

   function Vet (Position : Cursor) return Boolean with Inline;

   procedure Write_Node
     (Stream : not null access Root_Stream_Type'Class;
      Node   : Node_Access);
   pragma Inline (Write_Node);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package HT_Ops is new Hash_Tables.Generic_Operations
     (HT_Types  => HT_Types,
      Hash_Node => Hash_Node,
      Next      => Next,
      Set_Next  => Set_Next,
      Copy_Node => Copy_Node,
      Free      => Free);

   package Element_Keys is new Hash_Tables.Generic_Keys
     (HT_Types        => HT_Types,
      Next            => Next,
      Set_Next        => Set_Next,
      Key_Type        => Element_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   function Is_Equal is
      new HT_Ops.Generic_Equal (Find_Equal_Key);

   function Is_Equivalent is
      new HT_Ops.Generic_Equal (Find_Equivalent_Key);

   procedure Read_Nodes is
      new HT_Ops.Generic_Read (Read_Node);

   procedure Replace_Element is
      new Element_Keys.Generic_Replace_Element (Hash_Node, Assign);

   procedure Write_Nodes is
      new HT_Ops.Generic_Write (Write_Node);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Cursor) return Boolean is
   begin
      return
       Left.Container = Right.Container
         and then Left.Node = Right.Node;
   end "=";

   function "=" (Left, Right : Set) return Boolean is
   begin
      return Is_Equal (Left.HT, Right.HT);
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Set) is
   begin
      HT_Ops.Adjust (Container.HT);
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Node : Node_Access; Item : Element_Type) is
   begin
      Node.Element := Item;
   end Assign;

   procedure Assign (Target : in out Set; Source : Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Target.Clear;
      Target.Union (Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Set) return Count_Type is
   begin
      return HT_Ops.Capacity (Container.HT);
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Set) is
   begin
      HT_Ops.Clear (Container.HT);
   end Clear;

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

      pragma Assert (Vet (Position), "bad cursor in Constant_Reference");

      declare
         HT : Hash_Table_Type renames Position.Container.all.HT;
         TC : constant Tamper_Counts_Access :=
           HT.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => Position.Node.Element'Access,
            Control => (Controlled with TC))
         do
            Busy (TC.all);
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
      Capacity : Count_Type := 0) return Set
   is
      C : Count_Type;

   begin
      if Capacity < Source.Length then
         if Checks and then Capacity /= 0 then
            raise Capacity_Error
              with "Requested capacity is less than Source length";
         end if;

         C := Source.Length;
      else
         C := Capacity;
      end if;

      return Target : Set do
         Target.Reserve_Capacity (C);
         Target.Assign (Source);
      end return;
   end Copy;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Source : Node_Access) return Node_Access is
   begin
      return new Node_Type'(Element => Source.Element, Next => null);
   end Copy_Node;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Set;
      Item      : Element_Type)
   is
      X : Node_Access;

   begin
      Element_Keys.Delete_Key_Sans_Free (Container.HT, Item, X);

      if Checks and then X = null then
         raise Constraint_Error with "attempt to delete element not in set";
      end if;

      Free (X);
   end Delete;

   procedure Delete
     (Container : in out Set;
      Position  : in out Cursor)
   is
   begin
      TC_Check (Container.HT.TC);

      if Checks and then Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");

      HT_Ops.Delete_Node_Sans_Free (Container.HT, Position.Node);

      Free (Position.Node);
      Position.Container := null;
      Position.Position := No_Element.Position;
      pragma Assert (Position = No_Element);
   end Delete;

   ----------------
   -- Difference --
   ----------------

   procedure Difference
     (Target : in out Set;
      Source : Set)
   is
      Tgt_Node : Node_Access;
      Src_HT   : Hash_Table_Type renames Source'Unrestricted_Access.HT;

   begin
      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      if Src_HT.Length = 0 then
         return;
      end if;

      TC_Check (Target.HT.TC);

      if Src_HT.Length < Target.HT.Length then
         declare
            Src_Node : Node_Access;

         begin
            Src_Node := HT_Ops.First (Src_HT);
            while Src_Node /= null loop
               Tgt_Node := Element_Keys.Find (Target.HT, Src_Node.Element);

               if Tgt_Node /= null then
                  HT_Ops.Delete_Node_Sans_Free (Target.HT, Tgt_Node);
                  Free (Tgt_Node);
               end if;

               Src_Node := HT_Ops.Next (Src_HT, Src_Node);
            end loop;
         end;

      else
         Tgt_Node := HT_Ops.First (Target.HT);
         while Tgt_Node /= null loop
            if Is_In (Src_HT, Tgt_Node) then
               declare
                  X : Node_Access := Tgt_Node;
               begin
                  Tgt_Node := HT_Ops.Next (Target.HT, Tgt_Node);
                  HT_Ops.Delete_Node_Sans_Free (Target.HT, X);
                  Free (X);
               end;

            else
               Tgt_Node := HT_Ops.Next (Target.HT, Tgt_Node);
            end if;
         end loop;
      end if;
   end Difference;

   function Difference (Left, Right : Set) return Set is
      Left_HT  : Hash_Table_Type renames Left'Unrestricted_Access.HT;
      Right_HT : Hash_Table_Type renames Right'Unrestricted_Access.HT;
      Buckets  : HT_Types.Buckets_Access;
      Length   : Count_Type;

   begin
      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Left_HT.Length = 0 then
         return Empty_Set;
      end if;

      if Right_HT.Length = 0 then
         return Left;
      end if;

      declare
         Size : constant Hash_Type := Prime_Numbers.To_Prime (Left.Length);
      begin
         Buckets := HT_Ops.New_Buckets (Length => Size);
      end;

      Length := 0;

      Iterate_Left : declare
         procedure Process (L_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (L_Node : Node_Access) is
         begin
            if not Is_In (Right_HT, L_Node) then
               declare
                  --  Per AI05-0022, the container implementation is required
                  --  to detect element tampering by a generic actual
                  --  subprogram, hence the use of Checked_Index instead of a
                  --  simple invocation of generic formal Hash.

                  J : constant Hash_Type :=
                    HT_Ops.Checked_Index (Left_HT, Buckets.all, L_Node);

                  Bucket : Node_Access renames Buckets (J);

               begin
                  Bucket := new Node_Type'(L_Node.Element, Bucket);
               end;

               Length := Length + 1;
            end if;
         end Process;

      --  Start of processing for Iterate_Left

      begin
         Iterate (Left_HT);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Left;

      return (Controlled with HT => (Buckets, Length, (Busy => 0, Lock => 0)));
   end Difference;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with "Position cursor equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Element");

      return Position.Node.Element;
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty (Capacity : Count_Type := 1000) return Set is
   begin
      return Result : Set do
         Reserve_Capacity (Result, Capacity);
      end return;
   end Empty;

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
   begin
      return Is_Equivalent (Left.HT, Right.HT);
   end Equivalent_Sets;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements (Left, Right : Cursor)
     return Boolean is
   begin
      if Checks and then Left.Node = null then
         raise Constraint_Error with
           "Left cursor of Equivalent_Elements equals No_Element";
      end if;

      if Checks and then Right.Node = null then
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

      return Equivalent_Elements (Left.Node.Element, Right.Node.Element);
   end Equivalent_Elements;

   function Equivalent_Elements (Left : Cursor; Right : Element_Type)
     return Boolean is
   begin
      if Checks and then Left.Node = null then
         raise Constraint_Error with
           "Left cursor of Equivalent_Elements equals No_Element";
      end if;

      pragma Assert (Vet (Left), "Left cursor in Equivalent_Elements is bad");

      return Equivalent_Elements (Left.Node.Element, Right);
   end Equivalent_Elements;

   function Equivalent_Elements (Left : Element_Type; Right : Cursor)
     return Boolean is
   begin
      if Checks and then Right.Node = null then
         raise Constraint_Error with
           "Right cursor of Equivalent_Elements equals No_Element";
      end if;

      pragma Assert
        (Vet (Right),
         "Right cursor of Equivalent_Elements is bad");

      return Equivalent_Elements (Left, Right.Node.Element);
   end Equivalent_Elements;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Key : Element_Type; Node : Node_Access)
     return Boolean is
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
      X : Node_Access;
   begin
      Element_Keys.Delete_Key_Sans_Free (Container.HT, Item, X);
      Free (X);
   end Exclude;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Set) is
   begin
      HT_Ops.Finalize (Container.HT);
   end Finalize;

   procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Container /= null then
         Unbusy (Object.Container.HT.TC);
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Set;
      Item      : Element_Type) return Cursor
   is
      HT   : Hash_Table_Type renames Container'Unrestricted_Access.HT;
      Node : constant Node_Access := Element_Keys.Find (HT, Item);
   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'
        (Container'Unrestricted_Access, Node, HT_Ops.Index (HT, Node));
   end Find;

   --------------------
   -- Find_Equal_Key --
   --------------------

   function Find_Equal_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean
   is
      R_Index : constant Hash_Type :=
        Element_Keys.Index (R_HT, L_Node.Element);

      R_Node  : Node_Access := R_HT.Buckets (R_Index);

   begin
      loop
         if R_Node = null then
            return False;
         end if;

         if L_Node.Element = R_Node.Element then
            return True;
         end if;

         R_Node := Next (R_Node);
      end loop;
   end Find_Equal_Key;

   -------------------------
   -- Find_Equivalent_Key --
   -------------------------

   function Find_Equivalent_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean
   is
      R_Index : constant Hash_Type :=
        Element_Keys.Index (R_HT, L_Node.Element);

      R_Node  : Node_Access := R_HT.Buckets (R_Index);

   begin
      loop
         if R_Node = null then
            return False;
         end if;

         if Equivalent_Elements (L_Node.Element, R_Node.Element) then
            return True;
         end if;

         R_Node := Next (R_Node);
      end loop;
   end Find_Equivalent_Key;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
      Pos  : Hash_Type;
      Node : constant Node_Access := HT_Ops.First (Container.HT, Pos);
   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Node, Pos);
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      return Object.Container.First;
   end First;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   begin
      if X /= null then
         X.Next := X;     --  detect mischief (in Vet)
         Deallocate (X);
      end if;
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
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      return Position.Node /= null;
   end Has_Element;

   ---------------
   -- Hash_Node --
   ---------------

   function Hash_Node (Node : Node_Access) return Hash_Type is
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
         TE_Check (Container.HT.TC);

         Position.Node.Element := New_Item;
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
      Insert (Container.HT, New_Item, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;

      --  Note that we do not set the Position component of the cursor,
      --  because it may become incorrect on subsequent insertions/deletions
      --  from the container. This will lose some optimizations but prevents
      --  anomalies when the underlying hash-table is expanded or shrunk.
   end Insert;

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;

   begin
      Insert (Container, New_Item, Position, Inserted);

      if Checks and then not Inserted then
         raise Constraint_Error with
           "attempt to insert element already in set";
      end if;
   end Insert;

   procedure Insert
     (HT       : in out Hash_Table_Type;
      New_Item : Element_Type;
      Node     : out Node_Access;
      Inserted : out Boolean)
   is
      function New_Node (Next : Node_Access) return Node_Access;
      pragma Inline (New_Node);

      procedure Local_Insert is
        new Element_Keys.Generic_Conditional_Insert (New_Node);

      --------------
      -- New_Node --
      --------------

      function New_Node (Next : Node_Access) return Node_Access is
      begin
         return new Node_Type'(New_Item, Next);
      end New_Node;

   --  Start of processing for Insert

   begin
      if HT_Ops.Capacity (HT) = 0 then
         HT_Ops.Reserve_Capacity (HT, 1);
      end if;

      TC_Check (HT.TC);

      Local_Insert (HT, New_Item, Node, Inserted);

      if Inserted
        and then HT.Length > HT_Ops.Capacity (HT)
      then
         HT_Ops.Reserve_Capacity (HT, HT.Length);
      end if;
   end Insert;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection
     (Target : in out Set;
      Source : Set)
   is
      Src_HT   : Hash_Table_Type renames Source'Unrestricted_Access.HT;
      Tgt_Node : Node_Access;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.HT.Length = 0 then
         Clear (Target);
         return;
      end if;

      TC_Check (Target.HT.TC);

      Tgt_Node := HT_Ops.First (Target.HT);
      while Tgt_Node /= null loop
         if Is_In (Src_HT, Tgt_Node) then
            Tgt_Node := HT_Ops.Next (Target.HT, Tgt_Node);

         else
            declare
               X : Node_Access := Tgt_Node;
            begin
               Tgt_Node := HT_Ops.Next (Target.HT, Tgt_Node);
               HT_Ops.Delete_Node_Sans_Free (Target.HT, X);
               Free (X);
            end;
         end if;
      end loop;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
      Left_HT  : Hash_Table_Type renames Left'Unrestricted_Access.HT;
      Right_HT : Hash_Table_Type renames Right'Unrestricted_Access.HT;
      Buckets  : HT_Types.Buckets_Access;
      Length   : Count_Type;

   begin
      if Left'Address = Right'Address then
         return Left;
      end if;

      Length := Count_Type'Min (Left.Length, Right.Length);

      if Length = 0 then
         return Empty_Set;
      end if;

      declare
         Size : constant Hash_Type := Prime_Numbers.To_Prime (Length);
      begin
         Buckets := HT_Ops.New_Buckets (Length => Size);
      end;

      Length := 0;

      Iterate_Left : declare
         procedure Process (L_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (L_Node : Node_Access) is
         begin
            if Is_In (Right_HT, L_Node) then
               declare
                  --  Per AI05-0022, the container implementation is required
                  --  to detect element tampering by a generic actual
                  --  subprogram, hence the use of Checked_Index instead of a
                  --  simple invocation of generic formal Hash.

                  J : constant Hash_Type :=
                    HT_Ops.Checked_Index (Left_HT, Buckets.all, L_Node);

                  Bucket : Node_Access renames Buckets (J);

               begin
                  Bucket := new Node_Type'(L_Node.Element, Bucket);
               end;

               Length := Length + 1;
            end if;
         end Process;

      --  Start of processing for Iterate_Left

      begin
         Iterate (Left_HT);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Left;

      return (Controlled with HT => (Buckets, Length, (Busy => 0, Lock => 0)));
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.HT.Length = 0;
   end Is_Empty;

   -----------
   -- Is_In --
   -----------

   function Is_In
     (HT : aliased in out Hash_Table_Type;
      Key : Node_Access) return Boolean
   is
   begin
      return Element_Keys.Find (HT, Key.Element) /= null;
   end Is_In;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
      Subset_HT   : Hash_Table_Type renames Subset'Unrestricted_Access.HT;
      Of_Set_HT   : Hash_Table_Type renames Of_Set'Unrestricted_Access.HT;
      Subset_Node : Node_Access;

   begin
      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      if Subset.Length > Of_Set.Length then
         return False;
      end if;

      Subset_Node := HT_Ops.First (Subset_HT);
      while Subset_Node /= null loop
         if not Is_In (Of_Set_HT, Subset_Node) then
            return False;
         end if;
         Subset_Node := HT_Ops.Next (Subset_HT, Subset_Node);
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
      procedure Process_Node (Node : Node_Access; Position : Hash_Type);
      pragma Inline (Process_Node);

      procedure Iterate is
        new HT_Ops.Generic_Iteration_With_Position (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access; Position : Hash_Type) is
      begin
         Process (Cursor'(Container'Unrestricted_Access, Node, Position));
      end Process_Node;

      Busy : With_Busy (Container.HT.TC'Unrestricted_Access);

   --  Start of processing for Iterate

   begin
      Iterate (Container.HT);
   end Iterate;

   function Iterate
     (Container : Set) return Set_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      Busy (Container.HT.TC'Unrestricted_Access.all);
      return It : constant Iterator :=
         Iterator'(Limited_Controlled with
              Container => Container'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Container : Set) return Count_Type is
   begin
      return Container.HT.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      HT_Ops.Move (Target => Target.HT, Source => Source.HT);
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Node : Node_Access) return Node_Access is
   begin
      return Node.Next;
   end Next;

   function Next (Position : Cursor) return Cursor is
      Node : Node_Access;
      Pos  : Hash_Type;
   begin
      if Position.Node = null then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Next");

      Pos := Position.Position;
      Node := HT_Ops.Next (Position.Container.HT, Position.Node, Pos);

      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Node, Pos);
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

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong set";
      end if;

      return Next (Position);
   end Next;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
      Left_HT   : Hash_Table_Type renames Left'Unrestricted_Access.HT;
      Right_HT  : Hash_Table_Type renames Right'Unrestricted_Access.HT;
      Left_Node : Node_Access;

   begin
      if Right.Length = 0 then
         return False;
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      Left_Node := HT_Ops.First (Left_HT);
      while Left_Node /= null loop
         if Is_In (Right_HT, Left_Node) then
            return True;
         end if;
         Left_Node := HT_Ops.Next (Left_HT, Left_Node);
      end loop;

      return False;
   end Overlap;

   ----------------------
   -- Pseudo_Reference --
   ----------------------

   function Pseudo_Reference
     (Container : aliased Set'Class) return Reference_Control_Type
   is
      TC : constant Tamper_Counts_Access :=
        Container.HT.TC'Unrestricted_Access;
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
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         HT : Hash_Table_Type renames Position.Container.HT;
         Lock : With_Lock (HT.TC'Unrestricted_Access);
      begin
         Process (Position.Node.Element);
      end;
   end Query_Element;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Set)
   is
      First_Time : Boolean := True;
      use System.Put_Images;
   begin
      Array_Before (S);

      for X of V loop
         if First_Time then
            First_Time := False;
         else
            Simple_Array_Between (S);
         end if;

         Element_Type'Put_Image (S, X);
      end loop;

      Array_After (S);
   end Put_Image;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Set)
   is
   begin
      Read_Nodes (Stream, Container.HT);
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

   ---------------
   -- Read_Node --
   ---------------

   function Read_Node (Stream : not null access Root_Stream_Type'Class)
     return Node_Access
   is
      Node : Node_Access := new Node_Type;
   begin
      Element_Type'Read (Stream, Node.Element);
      return Node;
   exception
      when others =>
         Free (Node);
         raise;
   end Read_Node;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Set;
      New_Item  : Element_Type)
   is
      Node : constant Node_Access :=
        Element_Keys.Find (Container.HT, New_Item);

   begin
      TE_Check (Container.HT.TC);

      if Checks and then Node = null then
         raise Constraint_Error with
           "attempt to replace element not in set";
      end if;

      Node.Element := New_Item;
   end Replace;

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong set";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      Replace_Element (Container.HT, Position.Node, New_Item);
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Set;
      Capacity  : Count_Type)
   is
   begin
      HT_Ops.Reserve_Capacity (Container.HT, Capacity);
   end Reserve_Capacity;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Node : Node_Access; Next : Node_Access) is
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
      Tgt_HT : Hash_Table_Type renames Target.HT;
      Src_HT : Hash_Table_Type renames Source.HT'Unrestricted_Access.all;
   begin
      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      TC_Check (Tgt_HT.TC);

      declare
         N : constant Count_Type := Target.Length + Source.Length;
      begin
         if N > HT_Ops.Capacity (Tgt_HT) then
            HT_Ops.Reserve_Capacity (Tgt_HT, N);
         end if;
      end;

      if Target.Length = 0 then
         Iterate_Source_When_Empty_Target : declare
            procedure Process (Src_Node : Node_Access);

            procedure Iterate is new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (Src_Node : Node_Access) is
               E : Element_Type renames Src_Node.Element;
               B : Buckets_Type renames Tgt_HT.Buckets.all;
               J : constant Hash_Type := Hash (E) mod B'Length;
               N : Count_Type renames Tgt_HT.Length;

            begin
               B (J) := new Node_Type'(E, B (J));
               N := N + 1;
            end Process;

            --  Per AI05-0022, the container implementation is required to
            --  detect element tampering by a generic actual subprogram.

            Lock_Tgt : With_Lock (Tgt_HT.TC'Unrestricted_Access);
            Lock_Src : With_Lock (Src_HT.TC'Unrestricted_Access);

         --  Start of processing for Iterate_Source_When_Empty_Target

         begin
            Iterate (Src_HT);
         end Iterate_Source_When_Empty_Target;

      else
         Iterate_Source : declare
            procedure Process (Src_Node : Node_Access);

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);

            -------------
            -- Process --
            -------------

            procedure Process (Src_Node : Node_Access) is
               E : Element_Type renames Src_Node.Element;
               B : Buckets_Type renames Tgt_HT.Buckets.all;
               J : constant Hash_Type := Hash (E) mod B'Length;
               N : Count_Type renames Tgt_HT.Length;

            begin
               if B (J) = null then
                  B (J) := new Node_Type'(E, null);
                  N := N + 1;

               elsif Equivalent_Elements (E, B (J).Element) then
                  declare
                     X : Node_Access := B (J);
                  begin
                     B (J) := B (J).Next;
                     N := N - 1;
                     Free (X);
                  end;

               else
                  declare
                     Prev : Node_Access := B (J);
                     Curr : Node_Access := Prev.Next;

                  begin
                     while Curr /= null loop
                        if Equivalent_Elements (E, Curr.Element) then
                           Prev.Next := Curr.Next;
                           N := N - 1;
                           Free (Curr);
                           return;
                        end if;

                        Prev := Curr;
                        Curr := Prev.Next;
                     end loop;

                     B (J) := new Node_Type'(E, B (J));
                     N := N + 1;
                  end;
               end if;
            end Process;

            --  Per AI05-0022, the container implementation is required to
            --  detect element tampering by a generic actual subprogram.

            Lock_Tgt : With_Lock (Tgt_HT.TC'Unrestricted_Access);
            Lock_Src : With_Lock (Src_HT.TC'Unrestricted_Access);

         --  Start of processing for Iterate_Source

         begin
            Iterate (Src_HT);
         end Iterate_Source;
      end if;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
      Left_HT  : Hash_Table_Type renames Left'Unrestricted_Access.HT;
      Right_HT : Hash_Table_Type renames Right'Unrestricted_Access.HT;
      Buckets  : HT_Types.Buckets_Access;
      Length   : Count_Type;

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

      declare
         Size : constant Hash_Type :=
           Prime_Numbers.To_Prime (Left.Length + Right.Length);
      begin
         Buckets := HT_Ops.New_Buckets (Length => Size);
      end;

      Length := 0;

      Iterate_Left : declare
         procedure Process (L_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (L_Node : Node_Access) is
         begin
            if not Is_In (Right_HT, L_Node) then
               declare
                  E : Element_Type renames L_Node.Element;

                  --  Per AI05-0022, the container implementation is required
                  --  to detect element tampering by a generic actual
                  --  subprogram, hence the use of Checked_Index instead of a
                  --  simple invocation of generic formal Hash.

                  J : constant Hash_Type :=
                    HT_Ops.Checked_Index (Left_HT, Buckets.all, L_Node);

               begin
                  Buckets (J) := new Node_Type'(E, Buckets (J));
                  Length := Length + 1;
               end;
            end if;
         end Process;

      --  Start of processing for Iterate_Left

      begin
         Iterate (Left_HT);

      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Left;

      Iterate_Right : declare
         procedure Process (R_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (R_Node : Node_Access) is
         begin
            if not Is_In (Left_HT, R_Node) then
               declare
                  E : Element_Type renames R_Node.Element;

                  --  Per AI05-0022, the container implementation is required
                  --  to detect element tampering by a generic actual
                  --  subprogram, hence the use of Checked_Index instead of a
                  --  simple invocation of generic formal Hash.

                  J : constant Hash_Type :=
                    HT_Ops.Checked_Index (Right_HT, Buckets.all, R_Node);

               begin
                  Buckets (J) := new Node_Type'(E, Buckets (J));
                  Length := Length + 1;
               end;
            end if;
         end Process;

      --  Start of processing for Iterate_Right

      begin
         Iterate (Right_HT);

      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Right;

      return (Controlled with HT => (Buckets, Length, (Busy => 0, Lock => 0)));
   end Symmetric_Difference;

   ------------
   -- To_Set --
   ------------

   function To_Set (New_Item : Element_Type) return Set is
      HT : Hash_Table_Type;

      Node     : Node_Access;
      Inserted : Boolean;

   begin
      Insert (HT, New_Item, Node, Inserted);
      return Set'(Controlled with HT);
   end To_Set;

   -----------
   -- Union --
   -----------

   procedure Union
     (Target : in out Set;
      Source : Set)
   is
      procedure Process (Src_Node : Node_Access);

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Src_Node : Node_Access) is
         function New_Node (Next : Node_Access) return Node_Access;
         pragma Inline (New_Node);

         procedure Insert is
            new Element_Keys.Generic_Conditional_Insert (New_Node);

         --------------
         -- New_Node --
         --------------

         function New_Node (Next : Node_Access) return Node_Access is
            Node : constant Node_Access :=
              new Node_Type'(Src_Node.Element, Next);
         begin
            return Node;
         end New_Node;

         Tgt_Node : Node_Access;
         Success  : Boolean;

      --  Start of processing for Process

      begin
         Insert (Target.HT, Src_Node.Element, Tgt_Node, Success);
      end Process;

   --  Start of processing for Union

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      TC_Check (Target.HT.TC);

      declare
         N : constant Count_Type := Target.Length + Source.Length;
      begin
         if N > HT_Ops.Capacity (Target.HT) then
            HT_Ops.Reserve_Capacity (Target.HT, N);
         end if;
      end;

      Iterate (Source.HT);
   end Union;

   function Union (Left, Right : Set) return Set is
      Left_HT  : Hash_Table_Type renames Left.HT'Unrestricted_Access.all;
      Right_HT : Hash_Table_Type renames Right.HT'Unrestricted_Access.all;
      Buckets  : HT_Types.Buckets_Access;
      Length   : Count_Type;

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

      declare
         Size : constant Hash_Type :=
           Prime_Numbers.To_Prime (Left.Length + Right.Length);
      begin
         Buckets := HT_Ops.New_Buckets (Length => Size);
      end;

      Iterate_Left : declare
         procedure Process (L_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (L_Node : Node_Access) is
            J : constant Hash_Type :=
              Hash (L_Node.Element) mod Buckets'Length;

         begin
            Buckets (J) := new Node_Type'(L_Node.Element, Buckets (J));
         end Process;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram, hence the use of
         --  Checked_Index instead of a simple invocation of generic formal
         --  Hash.

         Lock_Left : With_Lock (Left_HT.TC'Unrestricted_Access);

      --  Start of processing for Iterate_Left

      begin
         Iterate (Left_HT);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Left;

      Length := Left.Length;

      Iterate_Right : declare
         procedure Process (Src_Node : Node_Access);

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (Src_Node : Node_Access) is
            J : constant Hash_Type :=
              Hash (Src_Node.Element) mod Buckets'Length;

            Tgt_Node : Node_Access := Buckets (J);

         begin
            while Tgt_Node /= null loop
               if Equivalent_Elements (Src_Node.Element, Tgt_Node.Element) then
                  return;
               end if;

               Tgt_Node := Next (Tgt_Node);
            end loop;

            Buckets (J) := new Node_Type'(Src_Node.Element, Buckets (J));
            Length := Length + 1;
         end Process;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram, hence the use of
         --  Checked_Index instead of a simple invocation of generic formal
         --  Hash.

         Lock_Left : With_Lock (Left_HT.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right_HT.TC'Unrestricted_Access);

      --  Start of processing for Iterate_Right

      begin
         Iterate (Right_HT);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end Iterate_Right;

      return (Controlled with HT => (Buckets, Length, (Busy => 0, Lock => 0)));
   end Union;

   ---------
   -- Vet --
   ---------

   function Vet (Position : Cursor) return Boolean is
   begin
      if not Container_Checks'Enabled then
         return True;
      end if;

      if Position.Node = null then
         return Position.Container = null;
      end if;

      if Position.Container = null then
         return False;
      end if;

      if Position.Node.Next = Position.Node then
         return False;
      end if;

      declare
         HT : Hash_Table_Type renames Position.Container.HT;
         X  : Node_Access;

      begin
         if HT.Length = 0 then
            return False;
         end if;

         if HT.Buckets = null
           or else HT.Buckets'Length = 0
         then
            return False;
         end if;

         X := HT.Buckets (Element_Keys.Checked_Index
                            (HT,
                             Position.Node.Element));

         for J in 1 .. HT.Length loop
            if X = Position.Node then
               return True;
            end if;

            if X = null then
               return False;
            end if;

            if X = X.Next then  --  to prevent unnecessary looping
               return False;
            end if;

            X := X.Next;
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
   begin
      Write_Nodes (Stream, Container.HT);
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

   ----------------
   -- Write_Node --
   ----------------

   procedure Write_Node
     (Stream : not null access Root_Stream_Type'Class;
      Node   : Node_Access)
   is
   begin
      Element_Type'Write (Stream, Node.Element);
   end Write_Node;

   --  Ada 2022 features:

   function Has_Element (Container : Set; Position : Cursor) return Boolean is
   begin
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      pragma Assert ((Position.Container = null) = (Position.Node = null),
                     "bad nullity in Has_Element");
      return Position.Container = Container'Unrestricted_Access;
   end Has_Element;

   function Tampering_With_Cursors_Prohibited
     (Container : Set) return Boolean
   is
   begin
      return Is_Busy (Container.HT.TC);
   end Tampering_With_Cursors_Prohibited;

   function Element (Container : Set; Position : Cursor) return Element_Type is
   begin
      if Checks and then not Has_Element (Container, Position) then
         raise Program_Error with "Position for wrong Container";
      end if;

      return Element (Position);
   end Element;

   procedure Query_Element
     (Container : Set;
      Position  : Cursor;
      Process   : not null access procedure (Element : Element_Type)) is
   begin
      if Checks and then not Has_Element (Container, Position) then
         raise Program_Error with "Position for wrong Container";
      end if;

      Query_Element (Position, Process);
   end Query_Element;

   function Next (Container : Set; Position : Cursor) return Cursor is
   begin
      if Checks and then
        not (Position = No_Element or else Has_Element (Container, Position))
      then
         raise Program_Error with "Position for wrong Container";
      end if;

      return Next (Position);
   end Next;

   procedure Next (Container : Set; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   ------------------
   -- Generic_Keys --
   ------------------

   package body Generic_Keys is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Equivalent_Key_Node
        (Key  : Key_Type;
         Node : Node_Access) return Boolean;
      pragma Inline (Equivalent_Key_Node);

      --------------------------
      -- Local Instantiations --
      --------------------------

      package Key_Keys is
         new Hash_Tables.Generic_Keys
          (HT_Types  => HT_Types,
           Next      => Next,
           Set_Next  => Set_Next,
           Key_Type  => Key_Type,
           Hash      => Hash,
           Equivalent_Keys => Equivalent_Key_Node);

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Container : aliased Set;
         Key       : Key_Type) return Constant_Reference_Type
      is
         Position : constant Cursor := Find (Container, Key);

      begin
         if Checks and then Position = No_Element then
            raise Constraint_Error with "Key not in set";
         end if;

         return Constant_Reference (Container, Position);
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
         X : Node_Access;

      begin
         Key_Keys.Delete_Key_Sans_Free (Container.HT, Key, X);

         if Checks and then X = null then
            raise Constraint_Error with "attempt to delete key not in set";
         end if;

         Free (X);
      end Delete;

      -------------
      -- Element --
      -------------

      function Element
        (Container : Set;
         Key       : Key_Type) return Element_Type
      is
         HT   : Hash_Table_Type renames Container'Unrestricted_Access.HT;
         Node : constant Node_Access := Key_Keys.Find (HT, Key);

      begin
         if Checks and then Node = null then
            raise Constraint_Error with "key not in set";
         end if;

         return Node.Element;
      end Element;

      -------------------------
      -- Equivalent_Key_Node --
      -------------------------

      function Equivalent_Key_Node
        (Key  : Key_Type;
         Node : Node_Access) return Boolean
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
         X : Node_Access;
      begin
         Key_Keys.Delete_Key_Sans_Free (Container.HT, Key, X);
         Free (X);
      end Exclude;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Control : in out Reference_Control_Type) is
      begin
         if Control.Container /= null then
            Impl.Reference_Control_Type (Control).Finalize;

            if Checks and then
              Hash (Key (Element (Control.Old_Pos))) /= Control.Old_Hash
            then
               HT_Ops.Delete_Node_At_Index
                 (Control.Container.HT, Control.Index, Control.Old_Pos.Node);
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
         HT   : Hash_Table_Type renames Container'Unrestricted_Access.HT;
         Node : constant Node_Access := Key_Keys.Find (HT, Key);
      begin
         if Node = null then
            return No_Element;
         else
            return Cursor'
              (Container'Unrestricted_Access, Node, HT_Ops.Index (HT, Node));
         end if;
      end Find;

      ---------
      -- Key --
      ---------

      function Key (Position : Cursor) return Key_Type is
      begin
         if Checks and then Position.Node = null then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         pragma Assert (Vet (Position), "bad cursor in function Key");

         return Key (Position.Node.Element);
      end Key;

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
         if Checks and then Position.Container = null then
            raise Constraint_Error with "Position cursor has no element";
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert
           (Vet (Position),
            "bad cursor in function Reference_Preserving_Key");

         declare
            HT : Hash_Table_Type renames Position.Container.all.HT;
         begin
            return R : constant Reference_Type :=
                         (Element => Position.Node.Element'Access,
                          Control =>
                            (Controlled with
                              HT.TC'Unrestricted_Access,
                              Container'Unrestricted_Access,
                              Index    => HT_Ops.Index (HT, Position.Node),
                              Old_Pos  => Position,
                              Old_Hash => Hash (Key (Position))))
            do
               Busy (HT.TC);
            end return;
         end;
      end Reference_Preserving_Key;

      function Reference_Preserving_Key
        (Container : aliased in out Set;
         Key       : Key_Type) return Reference_Type
      is
         Position : constant Cursor := Find (Container, Key);

      begin
         if Checks and then Position = No_Element then
            raise Constraint_Error with "key not in set";
         end if;

         return Reference_Preserving_Key (Container, Position);
      end Reference_Preserving_Key;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      is
         Node : constant Node_Access := Key_Keys.Find (Container.HT, Key);

      begin
         if Checks and then Node = null then
            raise Constraint_Error with
              "attempt to replace key not in set";
         end if;

         Replace_Element (Container.HT, Node, New_Item);
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
         HT   : Hash_Table_Type renames Container.HT;
         Indx : Hash_Type;

      begin
         if Checks and then Position.Node = null then
            raise Constraint_Error with
              "Position cursor equals No_Element";
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong set";
         end if;

         if Checks and then
           (HT.Buckets = null
              or else HT.Buckets'Length = 0
              or else HT.Length = 0
              or else Position.Node.Next = Position.Node)
         then
            raise Program_Error with "Position cursor is bad (set is empty)";
         end if;

         pragma Assert
           (Vet (Position),
            "bad cursor in Update_Element_Preserving_Key");

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            E : Element_Type renames Position.Node.Element;
            K : constant Key_Type := Key (E);
            Lock : With_Lock (HT.TC'Unrestricted_Access);
         begin
            Indx := HT_Ops.Index (HT, Position.Node);
            Process (E);

            if Equivalent_Keys (K, Key (E)) then
               return;
            end if;
         end;

         if HT.Buckets (Indx) = Position.Node then
            HT.Buckets (Indx) := Position.Node.Next;

         else
            declare
               Prev : Node_Access := HT.Buckets (Indx);

            begin
               while Prev.Next /= Position.Node loop
                  Prev := Prev.Next;

                  if Checks and then Prev = null then
                     raise Program_Error with
                       "Position cursor is bad (node not found)";
                  end if;
               end loop;

               Prev.Next := Position.Node.Next;
            end;
         end if;

         HT.Length := HT.Length - 1;

         declare
            X : Node_Access := Position.Node;

         begin
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

end Ada.Containers.Hashed_Sets;
