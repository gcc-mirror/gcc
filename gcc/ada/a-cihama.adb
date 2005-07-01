------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--               I N D E F I N I T E _ H A S H E D _ M A P S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit has originally being developed by Matthew J Heaney.            --
------------------------------------------------------------------------------

with Ada.Containers.Hash_Tables.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Operations);

with Ada.Containers.Hash_Tables.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Keys);

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Hashed_Maps is

   procedure Free_Key is
      new Ada.Unchecked_Deallocation (Key_Type, Key_Access);

   procedure Free_Element is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Copy_Node (Node : Node_Access) return Node_Access;
   pragma Inline (Copy_Node);

   function Equivalent_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean;
   pragma Inline (Equivalent_Key_Node);

   function Find_Equal_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean;

   procedure Free (X : in out Node_Access);
   --  pragma Inline (Free);

   function Hash_Node (Node : Node_Access) return Hash_Type;
   pragma Inline (Hash_Node);

   function Next (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   function Read_Node
     (Stream : access Root_Stream_Type'Class) return Node_Access;

   procedure Set_Next (Node : Node_Access; Next : Node_Access);
   pragma Inline (Set_Next);

   function Vet (Position : Cursor) return Boolean;

   procedure Write_Node
     (Stream : access Root_Stream_Type'Class;
      Node   : Node_Access);

   --------------------------
   -- Local Instantiations --
   --------------------------

   package HT_Ops is
      new Ada.Containers.Hash_Tables.Generic_Operations
        (HT_Types          => HT_Types,
         Hash_Node         => Hash_Node,
         Next              => Next,
         Set_Next          => Set_Next,
         Copy_Node         => Copy_Node,
         Free              => Free);

   package Key_Ops is
      new Hash_Tables.Generic_Keys
       (HT_Types  => HT_Types,
        Next      => Next,
        Set_Next  => Set_Next,
        Key_Type  => Key_Type,
        Hash      => Hash,
        Equivalent_Keys => Equivalent_Key_Node);

   ---------
   -- "=" --
   ---------

   function Is_Equal is new HT_Ops.Generic_Equal (Find_Equal_Key);

   function "=" (Left, Right : Map) return Boolean is
   begin
      return Is_Equal (Left.HT, Right.HT);
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Map) is
   begin
      HT_Ops.Adjust (Container.HT);
   end Adjust;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Map) return Count_Type is
   begin
      return HT_Ops.Capacity (Container.HT);
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      HT_Ops.Clear (Container.HT);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Node : Node_Access) return Node_Access is
      K : Key_Access := new Key_Type'(Node.Key.all);
      E : Element_Access;

   begin
      E := new Element_Type'(Node.Element.all);
      return new Node_Type'(K, E, null);

   exception
      when others =>
         Free_Key (K);
         Free_Element (E);
         raise;
   end Copy_Node;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map; Key : Key_Type) is
      X : Node_Access;

   begin
      Key_Ops.Delete_Key_Sans_Free (Container.HT, Key, X);

      if X = null then
         raise Constraint_Error;
      end if;

      Free (X);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      if Position.Node = null then
         raise Constraint_Error;
         return;
      end if;

      if Position.Container /= Map_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      pragma Assert (Position.Node.Next /= Position.Node);
      pragma Assert (Position.Node.Key /= null);
      pragma Assert (Position.Node.Element /= null);

      if Container.HT.Busy > 0 then
         raise Program_Error;
      end if;

      HT_Ops.Delete_Node_Sans_Free (Container.HT, Position.Node);

      Free (Position.Node);
      Position.Container := null;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      C : constant Cursor := Find (Container, Key);
   begin
      return C.Node.Element.all;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      pragma Assert (Vet (Position));
      return Position.Node.Element.all;
   end Element;

   -------------------------
   -- Equivalent_Key_Node --
   -------------------------

   function Equivalent_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean
   is
   begin
      return Equivalent_Keys (Key, Node.Key.all);
   end Equivalent_Key_Node;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : Cursor) return Boolean is
   begin
      pragma Assert (Vet (Left));
      pragma Assert (Vet (Right));
      return Equivalent_Keys (Left.Node.Key.all, Right.Node.Key.all);
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left  : Cursor;
      Right : Key_Type) return Boolean
   is
   begin
      pragma Assert (Vet (Left));
      return Equivalent_Keys (Left.Node.Key.all, Right);
   end Equivalent_Keys;

   function Equivalent_Keys
     (Left  : Key_Type;
      Right : Cursor) return Boolean
   is
   begin
      pragma Assert (Vet (Right));
      return Equivalent_Keys (Left, Right.Node.Key.all);
   end Equivalent_Keys;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      X : Node_Access;
   begin
      Key_Ops.Delete_Key_Sans_Free (Container.HT, Key, X);
      Free (X);
   end Exclude;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Map) is
   begin
      HT_Ops.Finalize (Container.HT);
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : constant Node_Access := Key_Ops.Find (Container.HT, Key);

   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end Find;

   --------------------
   -- Find_Equal_Key --
   --------------------

   function Find_Equal_Key
     (R_HT   : Hash_Table_Type;
      L_Node : Node_Access) return Boolean
   is
      R_Index : constant Hash_Type := Key_Ops.Index (R_HT, L_Node.Key.all);
      R_Node  : Node_Access := R_HT.Buckets (R_Index);

   begin
      while R_Node /= null loop
         if Equivalent_Keys (L_Node.Key.all, R_Node.Key.all) then
            return L_Node.Element.all = R_Node.Element.all;
         end if;

         R_Node := R_Node.Next;
      end loop;

      return False;
   end Find_Equal_Key;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
      Node : constant Node_Access := HT_Ops.First (Container.HT);
   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end First;

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

      X.Next := X;  --  detect mischief (in Vet)

      begin
         Free_Key (X.Key);
      exception
         when others =>
            X.Key := null;

            begin
               Free_Element (X.Element);
            exception
               when others =>
                  X.Element := null;
            end;

            Deallocate (X);
            raise;
      end;

      begin
         Free_Element (X.Element);
      exception
         when others =>
            X.Element := null;

            Deallocate (X);
            raise;
      end;

      Deallocate (X);
   end Free;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         pragma Assert (Position.Container = null);
         return False;
      end if;

      pragma Assert (Vet (Position));
      return True;
   end Has_Element;

   ---------------
   -- Hash_Node --
   ---------------

   function Hash_Node (Node : Node_Access) return Hash_Type is
   begin
      return Hash (Node.Key.all);
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

      K : Key_Access;
      E : Element_Access;

   begin
      Insert (Container, Key, New_Item, Position, Inserted);

      if not Inserted then
         if Container.HT.Lock > 0 then
            raise Program_Error;
         end if;

         K := Position.Node.Key;
         E := Position.Node.Element;

         Position.Node.Key := new Key_Type'(Key);

         begin
            Position.Node.Element := new Element_Type'(New_Item);
         exception
            when others =>
               Free_Key (K);
               raise;
         end;

         Free_Key (K);
         Free_Element (E);
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
      function New_Node (Next : Node_Access) return Node_Access;

      procedure Insert is
        new Key_Ops.Generic_Conditional_Insert (New_Node);

      --------------
      -- New_Node --
      --------------

      function New_Node (Next : Node_Access) return Node_Access is
         K  : Key_Access := new Key_Type'(Key);
         E  : Element_Access;
      begin
         E := new Element_Type'(New_Item);
         return new Node_Type'(K, E, Next);
      exception
         when others =>
            Free_Key (K);
            Free_Element (E);
            raise;
      end New_Node;

      HT : Hash_Table_Type renames Container.HT;

   --  Start of processing for Insert

   begin
      if HT.Length >= HT_Ops.Capacity (HT) then
         --  TODO: see note in a-cohama.adb.
         HT_Ops.Reserve_Capacity (HT, HT.Length + 1);
      end if;

      Insert (HT, Key, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;
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
         raise Constraint_Error;
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.HT.Length = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Process_Node (Node : Node_Access);
      pragma Inline (Process_Node);

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process_Node);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

   --  Start of processing Iterate

   begin
      Iterate (Container.HT);
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      pragma Assert (Vet (Position));
      return Position.Node.Key.all;
   end Key;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.HT.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Map;
      Source : in out Map)
   is
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

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = null then
         pragma Assert (Position.Container = null);
         return No_Element;
      end if;

      declare
         pragma Assert (Vet (Position));
         HT   : Hash_Table_Type renames Position.Container.HT;
         Node : constant Node_Access := HT_Ops.Next (HT, Position.Node);

      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Next;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : Element_Type))
   is
      pragma Assert (Vet (Position));

      K : Key_Type renames Position.Node.Key.all;
      E : Element_Type renames Position.Node.Element.all;

      M  : Map renames Position.Container.all;
      HT : Hash_Table_Type renames M.HT'Unrestricted_Access.all;

      B : Natural renames HT.Busy;
      L : Natural renames HT.Lock;

   begin
      B := B + 1;
      L := L + 1;

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
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read_Nodes is new HT_Ops.Generic_Read (Read_Node);

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container : out Map)
   is
   begin
      Read_Nodes (Stream, Container.HT);
   end Read;

   ---------------
   -- Read_Node --
   ---------------

   function Read_Node
     (Stream : access Root_Stream_Type'Class) return Node_Access
   is
      Node : Node_Access := new Node_Type;

   begin
      begin
         Node.Key := new Key_Type'(Key_Type'Input (Stream));
      exception
         when others =>
            Free (Node);
            raise;
      end;

      begin
         Node.Element := new Element_Type'(Element_Type'Input (Stream));
      exception
         when others =>
            Free_Key (Node.Key);
            Free (Node);
            raise;
      end;

      return Node;
   end Read_Node;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Node : constant Node_Access := Key_Ops.Find (Container.HT, Key);

      K : Key_Access;
      E : Element_Access;

   begin
      if Node = null then
         raise Constraint_Error;
      end if;

      if Container.HT.Lock > 0 then
         raise Program_Error;
      end if;

      K := Node.Key;
      E := Node.Element;

      Node.Key := new Key_Type'(Key);

      begin
         Node.Element := new Element_Type'(New_Item);
      exception
         when others =>
            Free_Key (K);
            raise;
      end;

      Free_Key (K);
      Free_Element (E);
   end Replace;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element (Position : Cursor; By : Element_Type) is
      pragma Assert (Vet (Position));
      X : Element_Access := Position.Node.Element;
   begin
      if Position.Container.HT.Lock > 0 then
         raise Program_Error;
      end if;

      Position.Node.Element := new Element_Type'(By);
      Free_Element (X);
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Map;
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

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Key     : Key_Type;
                                            Element : in out Element_Type))
   is
      pragma Assert (Vet (Position));

      K : Key_Type renames Position.Node.Key.all;
      E : Element_Type renames Position.Node.Element.all;

      M  : Map renames Position.Container.all;
      HT : Hash_Table_Type renames M.HT'Unrestricted_Access.all;

      B : Natural renames HT.Busy;
      L : Natural renames HT.Lock;

   begin
      B := B + 1;
      L := L + 1;

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
   end Update_Element;

   ---------
   -- Vet --
   ---------

   function Vet (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         return False;
      end if;

      if Position.Node.Next = Position.Node then
         return False;
      end if;

      if Position.Node.Key = null then
         return False;
      end if;

      if Position.Node.Element = null then
         return False;
      end if;

      declare
         HT : Hash_Table_Type renames Position.Container.HT;
         X  : Node_Access;
      begin
         if HT.Length = 0 then
            return False;
         end if;

         if HT.Buckets = null then
            return False;
         end if;

         X := HT.Buckets (Key_Ops.Index (HT, Position.Node.Key.all));

         for J in 1 .. HT.Length loop
            if X = Position.Node then
               return True;
            end if;

            if X = null then
               return False;
            end if;

            if X = X.Next then -- weird
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

   procedure Write_Nodes is new HT_Ops.Generic_Write (Write_Node);

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Map)
   is
   begin
      Write_Nodes (Stream, Container.HT);
   end Write;

   ----------------
   -- Write_Node --
   ----------------

   procedure Write_Node
     (Stream : access Root_Stream_Type'Class;
      Node   : Node_Access)
   is
   begin
      Key_Type'Output (Stream, Node.Key.all);
      Element_Type'Output (Stream, Node.Element.all);
   end Write_Node;

end Ada.Containers.Indefinite_Hashed_Maps;
