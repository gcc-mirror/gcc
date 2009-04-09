------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.RESTRICTED_DOUBLY_LINKED_LISTS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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

with System;  use type System.Address;

package body Ada.Containers.Restricted_Doubly_Linked_Lists is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate
     (Container : in out List'Class;
      New_Item  : Element_Type;
      New_Node  : out Count_Type);

   procedure Free
     (Container : in out List'Class;
      X         : Count_Type);

   procedure Insert_Internal
     (Container : in out List'Class;
      Before    : Count_Type;
      New_Node  : Count_Type);

   function Vet (Position : Cursor) return Boolean;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      LN : Node_Array renames Left.Nodes;
      RN : Node_Array renames Right.Nodes;

      LI : Count_Type := Left.First;
      RI : Count_Type := Right.First;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      for J in 1 .. Left.Length loop
         if LN (LI).Element /= RN (RI).Element then
            return False;
         end if;

         LI := LN (LI).Next;
         RI := RN (RI).Next;
      end loop;

      return True;
   end "=";

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Container : in out List'Class;
      New_Item  : Element_Type;
      New_Node  : out Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Free >= 0 then
         New_Node := Container.Free;
         N (New_Node).Element := New_Item;
         Container.Free := N (New_Node).Next;

      else
         New_Node := abs Container.Free;
         N (New_Node).Element := New_Item;
         Container.Free := Container.Free - 1;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container, No_Element, New_Item, Count);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out List; Source : List) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error;  -- ???
      end if;

      Clear (Target);

      declare
         N : Node_Array renames Source.Nodes;
         J : Count_Type := Source.First;

      begin
         while J /= 0 loop
            Append (Target, N (J).Element);
            J := N (J).Next;
         end loop;
      end;
   end Assign;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out List) is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Container.Length = 0 then
         pragma Assert (Container.First = 0);
         pragma Assert (Container.Last = 0);
--       pragma Assert (Container.Busy = 0);
--       pragma Assert (Container.Lock = 0);
         return;
      end if;

      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

--    if Container.Busy > 0 then
--      raise Program_Error;
--    end if;

      while Container.Length > 1 loop
         X := Container.First;

         Container.First := N (X).Next;
         N (Container.First).Prev := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;

      X := Container.First;

      Container.First := 0;
      Container.Last := 0;
      Container.Length := 0;

      Free (Container, X);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Position.Node = 0 then
         raise Constraint_Error;
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");

      if Position.Node = Container.First then
         Delete_First (Container, Count);
         Position := No_Element;
         return;
      end if;

      if Count = 0 then
         Position := No_Element;
         return;
      end if;

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

      for Index in 1 .. Count loop
         pragma Assert (Container.Length >= 2);

         X := Position.Node;
         Container.Length := Container.Length - 1;

         if X = Container.Last then
            Position := No_Element;

            Container.Last := N (X).Prev;
            N (Container.Last).Next := 0;

            Free (Container, X);
            return;
         end if;

         Position.Node := N (X).Next;

         N (N (X).Next).Prev := N (X).Prev;
         N (N (X).Prev).Next := N (X).Next;

         Free (Container, X);
      end loop;

      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      for I in 1 .. Count loop
         X := Container.First;
         pragma Assert (N (N (X).Next).Prev = Container.First);

         Container.First := N (X).Next;
         N (Container.First).Prev := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      for I in 1 .. Count loop
         X := Container.Last;
         pragma Assert (N (N (X).Prev).Next = Container.Last);

         Container.Last := N (X).Prev;
         N (Container.Last).Next := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = 0 then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Element");

      declare
         N : Node_Array renames Position.Container.Nodes;
      begin
         return N (Position.Node).Element;
      end;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Nodes : Node_Array renames Container.Nodes;
      Node  : Count_Type := Position.Node;

   begin
      if Node = 0 then
         Node := Container.First;

      else
         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error;
         end if;

         pragma Assert (Vet (Position), "bad cursor in Find");
      end if;

      while Node /= 0 loop
         if Nodes (Node).Element = Item then
            return Cursor'(Container'Unrestricted_Access, Node);
         end if;

         Node := Nodes (Node).Next;
      end loop;

      return No_Element;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = 0 then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Container.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.First = 0 then
         raise Constraint_Error;
      end if;

      return N (Container.First).Element;
   end First_Element;

   ----------
   -- Free --
   ----------

   procedure Free
     (Container : in out List'Class;
      X         : Count_Type)
   is
      pragma Assert (X > 0);
      pragma Assert (X <= Container.Capacity);

      N : Node_Array renames Container.Nodes;

   begin
      N (X).Prev := -1;  -- Node is deallocated (not on active list)

      if Container.Free >= 0 then
         N (X).Next := Container.Free;
         Container.Free := X;

      elsif X + 1 = abs Container.Free then
         N (X).Next := 0;  -- Not strictly necessary, but marginally safer
         Container.Free := Container.Free + 1;

      else
         Container.Free := abs Container.Free;

         if Container.Free > Container.Capacity then
            Container.Free := 0;

         else
            for I in Container.Free .. Container.Capacity - 1 loop
               N (I).Next := I + 1;
            end loop;

            N (Container.Capacity).Next := 0;
         end if;

         N (X).Next := Container.Free;
         Container.Free := X;
      end if;
   end Free;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : List) return Boolean is
         Nodes : Node_Array renames Container.Nodes;
         Node  : Count_Type := Container.First;

      begin
         for I in 2 .. Container.Length loop
            if Nodes (Nodes (Node).Next).Element < Nodes (Node).Element then
               return False;
            end if;

            Node := Nodes (Node).Next;
         end loop;

         return True;
      end Is_Sorted;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out List) is
         N : Node_Array renames Container.Nodes;

         procedure Partition (Pivot, Back : Count_Type);
         procedure Sort (Front, Back : Count_Type);

         ---------------
         -- Partition --
         ---------------

         procedure Partition (Pivot, Back : Count_Type) is
            Node : Count_Type := N (Pivot).Next;

         begin
            while Node /= Back loop
               if N (Node).Element < N (Pivot).Element then
                  declare
                     Prev : constant Count_Type := N (Node).Prev;
                     Next : constant Count_Type := N (Node).Next;

                  begin
                     N (Prev).Next := Next;

                     if Next = 0 then
                        Container.Last := Prev;
                     else
                        N (Next).Prev := Prev;
                     end if;

                     N (Node).Next := Pivot;
                     N (Node).Prev := N (Pivot).Prev;

                     N (Pivot).Prev := Node;

                     if N (Node).Prev = 0 then
                        Container.First := Node;
                     else
                        N (N (Node).Prev).Next := Node;
                     end if;

                     Node := Next;
                  end;

               else
                  Node := N (Node).Next;
               end if;
            end loop;
         end Partition;

         ----------
         -- Sort --
         ----------

         procedure Sort (Front, Back : Count_Type) is
            Pivot : Count_Type;

         begin
            if Front = 0 then
               Pivot := Container.First;
            else
               Pivot := N (Front).Next;
            end if;

            if Pivot /= Back then
               Partition (Pivot, Back);
               Sort (Front, Pivot);
               Sort (Pivot, Back);
            end if;
         end Sort;

      --  Start of processing for Sort

      begin
         if Container.Length <= 1 then
            return;
         end if;

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);

--       if Container.Busy > 0 then
--          raise Program_Error;
--       end if;

         Sort (Front => 0, Back => 0);

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      return Position.Node /= 0;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      J : Count_Type;

   begin
      if Before.Container /= null then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error;
         end if;

         pragma Assert (Vet (Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Container.Capacity - Count then
         raise Constraint_Error;
      end if;

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      Allocate (Container, New_Item, New_Node => J);
      Insert_Internal (Container, Before.Node, New_Node => J);
      Position := Cursor'(Container'Unrestricted_Access, Node => J);

      for Index in 2 .. Count loop
         Allocate (Container, New_Item, New_Node => J);
         Insert_Internal (Container, Before.Node, New_Node => J);
      end loop;
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Position : Cursor;
      pragma Unreferenced (Position);
   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      New_Item : Element_Type;  -- Do we need to reinit node ???
      pragma Warnings (Off, New_Item);

   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   ---------------------
   -- Insert_Internal --
   ---------------------

   procedure Insert_Internal
     (Container : in out List'Class;
      Before    : Count_Type;
      New_Node  : Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Length = 0 then
         pragma Assert (Before = 0);
         pragma Assert (Container.First = 0);
         pragma Assert (Container.Last = 0);

         Container.First := New_Node;
         Container.Last := New_Node;

         N (Container.First).Prev := 0;
         N (Container.Last).Next := 0;

      elsif Before = 0 then
         pragma Assert (N (Container.Last).Next = 0);

         N (Container.Last).Next := New_Node;
         N (New_Node).Prev := Container.Last;

         Container.Last := New_Node;
         N (Container.Last).Next := 0;

      elsif Before = Container.First then
         pragma Assert (N (Container.First).Prev = 0);

         N (Container.First).Prev := New_Node;
         N (New_Node).Next := Container.First;

         Container.First := New_Node;
         N (Container.First).Prev := 0;

      else
         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);

         N (New_Node).Next := Before;
         N (New_Node).Prev := N (Before).Prev;

         N (N (Before).Prev).Next := New_Node;
         N (Before).Prev := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Insert_Internal;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      C : List renames Container'Unrestricted_Access.all;
      N : Node_Array renames C.Nodes;
--    B : Natural renames C.Busy;

      Node  : Count_Type := Container.First;

      Index     : Count_Type := 0;
      Index_Max : constant Count_Type := Container.Length;

   begin
      if Index_Max = 0 then
         pragma Assert (Node = 0);
         return;
      end if;

      loop
         pragma Assert (Node /= 0);

         Process (Cursor'(C'Unchecked_Access, Node));
         pragma Assert (Container.Length = Index_Max);
         pragma Assert (N (Node).Prev /= -1);

         Node := N (Node).Next;
         Index := Index + 1;

         if Index = Index_Max then
            pragma Assert (Node = 0);
            return;
         end if;
      end loop;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = 0 then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Last = 0 then
         raise Constraint_Error;
      end if;

      return N (Container.Last).Element;
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Next");

      declare
         Nodes : Node_Array renames Position.Container.Nodes;
         Node  : constant Count_Type := Nodes (Position.Node).Next;

      begin
         if Node = 0 then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container, First (Container), New_Item, Count);
   end Prepend;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Previous");

      declare
         Nodes : Node_Array renames Position.Container.Nodes;
         Node  : constant Count_Type := Nodes (Position.Node).Prev;
      begin
         if Node = 0 then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Position.Node = 0 then
         raise Constraint_Error;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         C : List renames Position.Container.all'Unrestricted_Access.all;
         N : Node_Type renames C.Nodes (Position.Node);

      begin
         Process (N.Element);
         pragma Assert (N.Prev >= 0);
      end;
   end Query_Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Container = null then
         raise Constraint_Error;
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error;
      end if;

--    if Container.Lock > 0 then
--       raise Program_Error;
--    end if;

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      declare
         N : Node_Array renames Container.Nodes;
      begin
         N (Position.Node).Element := New_Item;
      end;
   end Replace_Element;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out List) is
      N : Node_Array renames Container.Nodes;
      I : Count_Type := Container.First;
      J : Count_Type := Container.Last;

      procedure Swap (L, R : Count_Type);

      ----------
      -- Swap --
      ----------

      procedure Swap (L, R : Count_Type) is
         LN : constant Count_Type := N (L).Next;
         LP : constant Count_Type := N (L).Prev;

         RN : constant Count_Type := N (R).Next;
         RP : constant Count_Type := N (R).Prev;

      begin
         if LP /= 0 then
            N (LP).Next := R;
         end if;

         if RN /= 0 then
            N (RN).Prev := L;
         end if;

         N (L).Next := RN;
         N (R).Prev := LP;

         if LN = R then
            pragma Assert (RP = L);

            N (L).Prev := R;
            N (R).Next := L;

         else
            N (L).Prev := RP;
            N (RP).Next := L;

            N (R).Next := LN;
            N (LN).Prev := R;
         end if;
      end Swap;

   --  Start of processing for Reverse_Elements

   begin
      if Container.Length <= 1 then
         return;
      end if;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      Container.First := J;
      Container.Last := I;
      loop
         Swap (L => I, R => J);

         J := N (J).Next;
         exit when I = J;

         I := N (I).Prev;
         exit when I = J;

         Swap (L => J, R => I);

         I := N (I).Next;
         exit when I = J;

         J := N (J).Prev;
         exit when I = J;
      end loop;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);
   end Reverse_Elements;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      N    : Node_Array renames Container.Nodes;
      Node : Count_Type := Position.Node;

   begin
      if Node = 0 then
         Node := Container.Last;

      else
         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error;
         end if;

         pragma Assert (Vet (Position), "bad cursor in Reverse_Find");
      end if;

      while Node /= 0 loop
         if N (Node).Element = Item then
            return Cursor'(Container'Unrestricted_Access, Node);
         end if;

         Node := N (Node).Prev;
      end loop;

      return No_Element;
   end Reverse_Find;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      C : List renames Container'Unrestricted_Access.all;
      N : Node_Array renames C.Nodes;
--    B : Natural renames C.Busy;

      Node : Count_Type := Container.Last;

      Index     : Count_Type := 0;
      Index_Max : constant Count_Type := Container.Length;

   begin
      if Index_Max = 0 then
         pragma Assert (Node = 0);
         return;
      end if;

      loop
         pragma Assert (Node > 0);

         Process (Cursor'(C'Unchecked_Access, Node));
         pragma Assert (Container.Length = Index_Max);
         pragma Assert (N (Node).Prev /= -1);

         Node := N (Node).Prev;
         Index := Index + 1;

         if Index = Index_Max then
            pragma Assert (Node = 0);
            return;
         end if;
      end loop;
   end Reverse_Iterate;

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : in out Cursor)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Before.Container /= null then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error;
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Position.Node = 0 then
         raise Constraint_Error;
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error;
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Position.Node = Before.Node
        or else N (Position.Node).Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      if Before.Node = 0 then
         pragma Assert (Position.Node /= Container.Last);

         if Position.Node = Container.First then
            Container.First := N (Position.Node).Next;
            N (Container.First).Prev := 0;

         else
            N (N (Position.Node).Prev).Next := N (Position.Node).Next;
            N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
         end if;

         N (Container.Last).Next := Position.Node;
         N (Position.Node).Prev := Container.Last;

         Container.Last := Position.Node;
         N (Container.Last).Next := 0;

         return;
      end if;

      if Before.Node = Container.First then
         pragma Assert (Position.Node /= Container.First);

         if Position.Node = Container.Last then
            Container.Last := N (Position.Node).Prev;
            N (Container.Last).Next := 0;

         else
            N (N (Position.Node).Prev).Next := N (Position.Node).Next;
            N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
         end if;

         N (Container.First).Prev := Position.Node;
         N (Position.Node).Next := Container.First;

         Container.First := Position.Node;
         N (Container.First).Prev := 0;

         return;
      end if;

      if Position.Node = Container.First then
         Container.First := N (Position.Node).Next;
         N (Container.First).Prev := 0;

      elsif Position.Node = Container.Last then
         Container.Last := N (Position.Node).Prev;
         N (Container.Last).Next := 0;

      else
         N (N (Position.Node).Prev).Next := N (Position.Node).Next;
         N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
      end if;

      N (N (Before.Node).Prev).Next := Position.Node;
      N (Position.Node).Prev := N (Before.Node).Prev;

      N (Before.Node).Prev := Position.Node;
      N (Position.Node).Next := Before.Node;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);
   end Splice;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      if I.Node = 0
        or else J.Node = 0
      then
         raise Constraint_Error;
      end if;

      if I.Container /= Container'Unrestricted_Access
        or else J.Container /= Container'Unrestricted_Access
      then
         raise Program_Error;
      end if;

      if I.Node = J.Node then
         return;
      end if;

--    if Container.Lock > 0 then
--       raise Program_Error;
--    end if;

      pragma Assert (Vet (I), "bad I cursor in Swap");
      pragma Assert (Vet (J), "bad J cursor in Swap");

      declare
         N  : Node_Array renames Container.Nodes;

         EI : Element_Type renames N (I.Node).Element;
         EJ : Element_Type renames N (J.Node).Element;

         EI_Copy : constant Element_Type := EI;

      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   ----------------
   -- Swap_Links --
   ----------------

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      if I.Node = 0
        or else J.Node = 0
      then
         raise Constraint_Error;
      end if;

      if I.Container /= Container'Unrestricted_Access
        or else I.Container /= J.Container
      then
         raise Program_Error;
      end if;

      if I.Node = J.Node then
         return;
      end if;

--    if Container.Busy > 0 then
--       raise Program_Error;
--    end if;

      pragma Assert (Vet (I), "bad I cursor in Swap_Links");
      pragma Assert (Vet (J), "bad J cursor in Swap_Links");

      declare
         I_Next : constant Cursor := Next (I);

         J_Copy : Cursor := J;
         pragma Warnings (Off, J_Copy);

      begin
         if I_Next = J then
            Splice (Container, Before => I, Position => J_Copy);

         else
            declare
               J_Next : constant Cursor := Next (J);

               I_Copy : Cursor := I;
               pragma Warnings (Off, I_Copy);

            begin
               if J_Next = I then
                  Splice (Container, Before => J, Position => I_Copy);

               else
                  pragma Assert (Container.Length >= 3);

                  Splice (Container, Before => I_Next, Position => J_Copy);
                  Splice (Container, Before => J_Next, Position => I_Copy);
               end if;
            end;
         end if;
      end;
   end Swap_Links;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out List;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type))
   is
   begin
      if Position.Node = 0 then
         raise Constraint_Error;
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Update_Element");

      declare
         N  : Node_Type renames Container.Nodes (Position.Node);

      begin
         Process (N.Element);
         pragma Assert (N.Prev >= 0);
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
         L : List renames Position.Container.all;
         N : Node_Array renames L.Nodes;

      begin
         if L.Length = 0 then
            return False;
         end if;

         if L.First = 0 then
            return False;
         end if;

         if L.Last = 0 then
            return False;
         end if;

         if Position.Node > L.Capacity then
            return False;
         end if;

         if N (Position.Node).Prev < 0
           or else N (Position.Node).Prev > L.Capacity
         then
            return False;
         end if;

         if N (Position.Node).Next > L.Capacity then
            return False;
         end if;

         if N (L.First).Prev /= 0 then
            return False;
         end if;

         if N (L.Last).Next /= 0 then
            return False;
         end if;

         if N (Position.Node).Prev = 0
           and then Position.Node /= L.First
         then
            return False;
         end if;

         if N (Position.Node).Next = 0
           and then Position.Node /= L.Last
         then
            return False;
         end if;

         if L.Length = 1 then
            return L.First = L.Last;
         end if;

         if L.First = L.Last then
            return False;
         end if;

         if N (L.First).Next = 0 then
            return False;
         end if;

         if N (L.Last).Prev = 0 then
            return False;
         end if;

         if N (N (L.First).Next).Prev /= L.First then
            return False;
         end if;

         if N (N (L.Last).Prev).Next /= L.Last then
            return False;
         end if;

         if L.Length = 2 then
            if N (L.First).Next /= L.Last then
               return False;
            end if;

            if N (L.Last).Prev /= L.First then
               return False;
            end if;

            return True;
         end if;

         if N (L.First).Next = L.Last then
            return False;
         end if;

         if N (L.Last).Prev = L.First then
            return False;
         end if;

         if Position.Node = L.First then
            return True;
         end if;

         if Position.Node = L.Last then
            return True;
         end if;

         if N (Position.Node).Next = 0 then
            return False;
         end if;

         if N (Position.Node).Prev = 0 then
            return False;
         end if;

         if N (N (Position.Node).Next).Prev /= Position.Node then
            return False;
         end if;

         if N (N (Position.Node).Prev).Next /= Position.Node then
            return False;
         end if;

         if L.Length = 3 then
            if N (L.First).Next /= Position.Node then
               return False;
            end if;

            if N (L.Last).Prev /= Position.Node then
               return False;
            end if;
         end if;

         return True;
      end;
   end Vet;

end Ada.Containers.Restricted_Doubly_Linked_Lists;
