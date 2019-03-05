------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . B O U N D E D _ H A S H E D _ M A P S    --
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

with Ada.Containers.Hash_Tables.Generic_Bounded_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Operations);

with Ada.Containers.Hash_Tables.Generic_Bounded_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Bounded_Keys);

with Ada.Containers.Helpers; use Ada.Containers.Helpers;

with Ada.Containers.Prime_Numbers; use Ada.Containers.Prime_Numbers;

with System; use type System.Address;

package body Ada.Containers.Bounded_Hashed_Maps is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Equivalent_Key_Node
     (Key  : Key_Type;
      Node : Node_Type) return Boolean;
   pragma Inline (Equivalent_Key_Node);

   function Hash_Node (Node : Node_Type) return Hash_Type;
   pragma Inline (Hash_Node);

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

   package Key_Ops is new Hash_Tables.Generic_Bounded_Keys
     (HT_Types        => HT_Types,
      Next            => Next,
      Set_Next        => Set_Next,
      Key_Type        => Key_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Key_Node);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
      function Find_Equal_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean;

      function Is_Equal is new HT_Ops.Generic_Equal (Find_Equal_Key);

      --------------------
      -- Find_Equal_Key --
      --------------------

      function Find_Equal_Key
        (R_HT   : Hash_Table_Type'Class;
         L_Node : Node_Type) return Boolean
      is
         R_Index : constant Hash_Type := Key_Ops.Index (R_HT, L_Node.Key);
         R_Node  : Count_Type := R_HT.Buckets (R_Index);

      begin
         while R_Node /= 0 loop
            if Equivalent_Keys (L_Node.Key, R_HT.Nodes (R_Node).Key) then
               return L_Node.Element = R_HT.Nodes (R_Node).Element;
            end if;

            R_Node := R_HT.Nodes (R_Node).Next;
         end loop;

         return False;
      end Find_Equal_Key;

   --  Start of processing for "="

   begin
      return Is_Equal (Left, Right);
   end "=";

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Map; Source : Map) is
      procedure Insert_Element (Source_Node : Count_Type);

      procedure Insert_Elements is
         new HT_Ops.Generic_Iteration (Insert_Element);

      --------------------
      -- Insert_Element --
      --------------------

      procedure Insert_Element (Source_Node : Count_Type) is
         N : Node_Type renames Source.Nodes (Source_Node);
         C : Cursor;
         B : Boolean;

      begin
         Insert (Target, N.Key, N.Element, C, B);
         pragma Assert (B);
      end Insert_Element;

   --  Start of processing for Assign

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Checks and then Target.Capacity < Source.Length then
         raise Capacity_Error
           with "Target capacity is less than Source length";
      end if;

      HT_Ops.Clear (Target);
      Insert_Elements (Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Map) return Count_Type is
   begin
      return Container.Capacity;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      HT_Ops.Clear (Container);
   end Clear;

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

      pragma Assert (Vet (Position),
                     "Position cursor in Constant_Reference is bad");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => N.Element'Access,
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
      Node : constant Count_Type :=
               Key_Ops.Find (Container'Unrestricted_Access.all, Key);

   begin
      if Checks and then Node = 0 then
         raise Constraint_Error with "key not in map";
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

   function Copy
     (Source   : Map;
      Capacity : Count_Type := 0;
      Modulus  : Hash_Type := 0) return Map
   is
      C : Count_Type;
      M : Hash_Type;

   begin
      if Capacity = 0 then
         C := Source.Length;

      elsif Capacity >= Source.Length then
         C := Capacity;

      elsif Checks then
         raise Capacity_Error with "Capacity value too small";
      end if;

      if Modulus = 0 then
         M := Default_Modulus (C);
      else
         M := Modulus;
      end if;

      return Target : Map (Capacity => C, Modulus => M) do
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

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Count_Type;

   begin
      Key_Ops.Delete_Key_Sans_Free (Container, Key, X);

      if Checks and then X = 0 then
         raise Constraint_Error with "attempt to delete key not in map";
      end if;

      HT_Ops.Free (Container, X);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Delete equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Delete designates wrong map";
      end if;

      TC_Check (Container.TC);

      pragma Assert (Vet (Position), "bad cursor in Delete");

      HT_Ops.Delete_Node_Sans_Free (Container, Position.Node);
      HT_Ops.Free (Container, Position.Node);

      Position := No_Element;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Node : constant Count_Type :=
               Key_Ops.Find (Container'Unrestricted_Access.all, Key);

   begin
      if Checks and then Node = 0 then
         raise Constraint_Error with
           "no element available because key not in map";
      end if;

      return Container.Nodes (Node).Element;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of function Element equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Element");

      return Position.Container.Nodes (Position.Node).Element;
   end Element;

   -------------------------
   -- Equivalent_Key_Node --
   -------------------------

   function Equivalent_Key_Node
     (Key  : Key_Type;
      Node : Node_Type) return Boolean is
   begin
      return Equivalent_Keys (Key, Node.Key);
   end Equivalent_Key_Node;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : Cursor)
     return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with
           "Left cursor of Equivalent_Keys equals No_Element";
      end if;

      if Checks and then Right.Node = 0 then
         raise Constraint_Error with
           "Right cursor of Equivalent_Keys equals No_Element";
      end if;

      pragma Assert (Vet (Left), "Left cursor of Equivalent_Keys is bad");
      pragma Assert (Vet (Right), "Right cursor of Equivalent_Keys is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

      begin
         return Equivalent_Keys (LN.Key, RN.Key);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      if Checks and then Left.Node = 0 then
         raise Constraint_Error with
           "Left cursor of Equivalent_Keys equals No_Element";
      end if;

      pragma Assert (Vet (Left), "Left cursor in Equivalent_Keys is bad");

      declare
         LN : Node_Type renames Left.Container.Nodes (Left.Node);

      begin
         return Equivalent_Keys (LN.Key, Right);
      end;
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      if Checks and then Right.Node = 0 then
         raise Constraint_Error with
           "Right cursor of Equivalent_Keys equals No_Element";
      end if;

      pragma Assert (Vet (Right), "Right cursor of Equivalent_Keys is bad");

      declare
         RN : Node_Type renames Right.Container.Nodes (Right.Node);

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
      HT_Ops.Free (Container, X);
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

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Count_Type :=
               Key_Ops.Find (Container'Unrestricted_Access.all, Key);
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
      Node : constant Count_Type := HT_Ops.First (Container);
   begin
      if Node = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Node);
      end if;
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      return Object.Container.First;
   end First;

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
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      return Position.Node /= 0;
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
         TE_Check (Container.TC);

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
         new HT_Ops.Generic_Allocate (Assign_Key);

      -----------------
      --  Assign_Key --
      -----------------

      procedure Assign_Key (Node : in out Node_Type) is
         pragma Warnings (Off);
         Default_Initialized_Item : Element_Type;
         pragma Unmodified (Default_Initialized_Item);
         --  Default-initialized element (ok to reference, see below)

      begin
         Node.Key := Key;

         --  There is no explicit element provided, but in an instance the
         --  element type may be a scalar with a Default_Value aspect, or a
         --  composite type with such a scalar component, or components with
         --  default initialization, so insert a possibly initialized element
         --  under the given key.

         Node.Element := Default_Initialized_Item;
         pragma Warnings (On);
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
      --  The buckets array length is specified by the user as a discriminant
      --  of the container type, so it is possible for the buckets array to
      --  have a length of zero. We must check for this case specifically, in
      --  order to prevent divide-by-zero errors later, when we compute the
      --  buckets array index value for a key, given its hash value.

      if Checks and then Container.Buckets'Length = 0 then
         raise Capacity_Error with "No capacity for insertion";
      end if;

      Local_Insert (Container, Key, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;
   end Insert;

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
         new HT_Ops.Generic_Allocate (Assign_Key);

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
      --  The buckets array length is specified by the user as a discriminant
      --  of the container type, so it is possible for the buckets array to
      --  have a length of zero. We must check for this case specifically, in
      --  order to prevent divide-by-zero errors later, when we compute the
      --  buckets array index value for a key, given its hash value.

      if Checks and then Container.Buckets'Length = 0 then
         raise Capacity_Error with "No capacity for insertion";
      end if;

      Local_Insert (Container, Key, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;
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
         raise Constraint_Error with
           "attempt to insert key already in map";
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Count_Type);
      pragma Inline (Process_Node);

      procedure Local_Iterate is new HT_Ops.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Count_Type) is
      begin
         Process (Cursor'(Container'Unrestricted_Access, Node));
      end Process_Node;

      Busy : With_Busy (Container.TC'Unrestricted_Access);

   --  Start of processing for Iterate

   begin
      Local_Iterate (Container);
   end Iterate;

   function Iterate
     (Container : Map) return Map_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      return It : constant Iterator :=
        (Limited_Controlled with
           Container => Container'Unrestricted_Access)
      do
         Busy (Container.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of function Key equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Key");

      return Position.Container.Nodes (Position.Node).Key;
   end Key;

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

   function Next (Node : Node_Type) return Count_Type is
   begin
      return Node.Next;
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Next");

      declare
         M    : Map renames Position.Container.all;
         Node : constant Count_Type := HT_Ops.Next (M, Position.Node);
      begin
         if Node = 0 then
            return No_Element;
         else
            return Cursor'(Position.Container, Node);
         end if;
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

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong map";
      end if;

      return Next (Position);
   end Next;

   ----------------------
   -- Pseudo_Reference --
   ----------------------

   function Pseudo_Reference
     (Container : aliased Map'Class) return Reference_Control_Type
   is
      TC : constant Tamper_Counts_Access :=
        Container.TC'Unrestricted_Access;
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
      Process  : not null access
                   procedure (Key : Key_Type; Element : Element_Type))
   is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Query_Element equals No_Element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         M : Map renames Position.Container.all;
         N : Node_Type renames M.Nodes (Position.Node);
         Lock : With_Lock (M.TC'Unrestricted_Access);
      begin
         Process (N.Key, N.Element);
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
        (Stream : not null access Root_Stream_Type'Class) return Count_Type;
      --  pragma Inline (Read_Node);  ???

      procedure Read_Nodes is new HT_Ops.Generic_Read (Read_Node);

      ---------------
      -- Read_Node --
      ---------------

      function Read_Node
        (Stream : not null access Root_Stream_Type'Class) return Count_Type
      is
         procedure Read_Element (Node : in out Node_Type);
         --  pragma Inline (Read_Element);  ???

         procedure Allocate is
            new HT_Ops.Generic_Allocate (Read_Element);

         procedure Read_Element (Node : in out Node_Type) is
         begin
            Key_Type'Read (Stream, Node.Key);
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

      pragma Assert (Vet (Position),
                     "Position cursor in function Reference is bad");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => N.Element'Access,
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
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if Checks and then Node = 0 then
         raise Constraint_Error with "key not in map";
      end if;

      declare
         N : Node_Type renames Container.Nodes (Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => N.Element'Access,
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
      Node : constant Count_Type := Key_Ops.Find (Container, Key);

   begin
      if Checks and then Node = 0 then
         raise Constraint_Error with
           "attempt to replace key not in map";
      end if;

      TE_Check (Container.TC);

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
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Replace_Element equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Replace_Element designates wrong map";
      end if;

      TE_Check (Position.Container.TC);

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

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
      if Checks and then Capacity > Container.Capacity then
         raise Capacity_Error with "requested capacity is too large";
      end if;
   end Reserve_Capacity;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Node : in out Node_Type; Next : Count_Type) is
   begin
      Node.Next := Next;
   end Set_Next;

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
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor of Update_Element equals No_Element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor of Update_Element designates wrong map";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Update_Element");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         Lock : With_Lock (Container.TC'Unrestricted_Access);
      begin
         Process (N.Key, N.Element);
      end;
   end Update_Element;

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
         M : Map renames Position.Container.all;
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

         X := M.Buckets (Key_Ops.Checked_Index
                          (M, M.Nodes (Position.Node).Key));

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

end Ada.Containers.Bounded_Hashed_Maps;
