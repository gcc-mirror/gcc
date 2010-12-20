------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . C O N T A I N E R S . D O U B L Y _ L I N K E D _ L I S T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2010, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Doubly_Linked_Lists is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free (X : in out Node_Access);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Node_Access;
      New_Node  : Node_Access);

   function Vet (Position : Cursor) return Boolean;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      L : Node_Access := Left.First;
      R : Node_Access := Right.First;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      for J in 1 .. Left.Length loop
         if L.Element /= R.Element then
            return False;
         end if;

         L := L.Next;
         R := R.Next;
      end loop;

      return True;
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out List) is
      Src : Node_Access := Container.First;

   begin
      if Src = null then
         pragma Assert (Container.Last = null);
         pragma Assert (Container.Length = 0);
         pragma Assert (Container.Busy = 0);
         pragma Assert (Container.Lock = 0);
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
      pragma Assert (Container.Length > 0);

      Container.First := null;
      Container.Last := null;
      Container.Length := 0;
      Container.Busy := 0;
      Container.Lock := 0;

      Container.First := new Node_Type'(Src.Element, null, null);
      Container.Last := Container.First;
      Container.Length := 1;

      Src := Src.Next;
      while Src /= null loop
         Container.Last.Next := new Node_Type'(Element => Src.Element,
                                               Prev    => Container.Last,
                                               Next    => null);
         Container.Last := Container.Last.Next;
         Container.Length := Container.Length + 1;

         Src := Src.Next;
      end loop;
   end Adjust;

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

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out List) is
      X : Node_Access;

   begin
      if Container.Length = 0 then
         pragma Assert (Container.First = null);
         pragma Assert (Container.Last = null);
         pragma Assert (Container.Busy = 0);
         pragma Assert (Container.Lock = 0);
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      while Container.Length > 1 loop
         X := Container.First;
         pragma Assert (X.Next.Prev = Container.First);

         Container.First := X.Next;
         Container.First.Prev := null;

         Container.Length := Container.Length - 1;

         Free (X);
      end loop;

      X := Container.First;
      pragma Assert (X = Container.Last);

      Container.First := null;
      Container.Last := null;
      Container.Length := 0;

      pragma Warnings (Off);
      Free (X);
      pragma Warnings (On);
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
      X : Node_Access;

   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");

      if Position.Node = Container.First then
         Delete_First (Container, Count);
         Position := No_Element; --  Post-York behavior
         return;
      end if;

      if Count = 0 then
         Position := No_Element;  --  Post-York behavior
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      for Index in 1 .. Count loop
         X := Position.Node;
         Container.Length := Container.Length - 1;

         if X = Container.Last then
            Position := No_Element;

            Container.Last := X.Prev;
            Container.Last.Next := null;

            Free (X);
            return;
         end if;

         Position.Node := X.Next;

         X.Next.Prev := X.Prev;
         X.Prev.Next := X.Next;

         Free (X);
      end loop;

      Position := No_Element;  --  Post-York behavior
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      X : Node_Access;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      for I in 1 .. Count loop
         X := Container.First;
         pragma Assert (X.Next.Prev = Container.First);

         Container.First := X.Next;
         Container.First.Prev := null;

         Container.Length := Container.Length - 1;

         Free (X);
      end loop;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      X : Node_Access;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      for I in 1 .. Count loop
         X := Container.Last;
         pragma Assert (X.Prev.Next = Container.Last);

         Container.Last := X.Prev;
         Container.Last.Next := null;

         Container.Length := Container.Length - 1;

         Free (X);
      end loop;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Element");

      return Position.Node.Element;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Node : Node_Access := Position.Node;

   begin
      if Node = null then
         Node := Container.First;

      else
         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert (Vet (Position), "bad cursor in Find");
      end if;

      while Node /= null loop
         if Node.Element = Item then
            return Cursor'(Container'Unchecked_Access, Node);
         end if;

         Node := Node.Next;
      end loop;

      return No_Element;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
   begin
      if Container.First = null then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.First.Element;
   end First_Element;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   begin
      X.Prev := X;
      X.Next := X;
      Deallocate (X);
   end Free;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : List) return Boolean is
         Node : Node_Access := Container.First;

      begin
         for I in 2 .. Container.Length loop
            if Node.Next.Element < Node.Element then
               return False;
            end if;

            Node := Node.Next;
         end loop;

         return True;
      end Is_Sorted;

      -----------
      -- Merge --
      -----------

      procedure Merge
        (Target : in out List;
         Source : in out List)
      is
         LI, RI : Cursor;

      begin
         if Target'Address = Source'Address then
            return;
         end if;

         if Target.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors of Target (list is busy)";
         end if;

         if Source.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors of Source (list is busy)";
         end if;

         LI := First (Target);
         RI := First (Source);
         while RI.Node /= null loop
            pragma Assert (RI.Node.Next = null
                             or else not (RI.Node.Next.Element <
                                          RI.Node.Element));

            if LI.Node = null then
               Splice (Target, No_Element, Source);
               return;
            end if;

            pragma Assert (LI.Node.Next = null
                             or else not (LI.Node.Next.Element <
                                          LI.Node.Element));

            if RI.Node.Element < LI.Node.Element then
               declare
                  RJ : Cursor := RI;
                  pragma Warnings (Off, RJ);
               begin
                  RI.Node := RI.Node.Next;
                  Splice (Target, LI, Source, RJ);
               end;

            else
               LI.Node := LI.Node.Next;
            end if;
         end loop;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out List) is

         procedure Partition (Pivot : Node_Access; Back : Node_Access);

         procedure Sort (Front, Back : Node_Access);

         ---------------
         -- Partition --
         ---------------

         procedure Partition (Pivot : Node_Access; Back : Node_Access) is
            Node : Node_Access := Pivot.Next;

         begin
            while Node /= Back loop
               if Node.Element < Pivot.Element then
                  declare
                     Prev : constant Node_Access := Node.Prev;
                     Next : constant Node_Access := Node.Next;

                  begin
                     Prev.Next := Next;

                     if Next = null then
                        Container.Last := Prev;
                     else
                        Next.Prev := Prev;
                     end if;

                     Node.Next := Pivot;
                     Node.Prev := Pivot.Prev;

                     Pivot.Prev := Node;

                     if Node.Prev = null then
                        Container.First := Node;
                     else
                        Node.Prev.Next := Node;
                     end if;

                     Node := Next;
                  end;

               else
                  Node := Node.Next;
               end if;
            end loop;
         end Partition;

         ----------
         -- Sort --
         ----------

         procedure Sort (Front, Back : Node_Access) is
            Pivot : constant Node_Access :=
                      (if Front = null then Container.First else Front.Next);
         begin
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

         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);

         if Container.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (list is busy)";
         end if;

         Sort (Front => null, Back => null);

         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      pragma Assert (Vet (Position), "bad cursor in Has_Element");
      return Position.Node /= null;
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
      New_Node : Node_Access;

   begin
      if Before.Container /= null then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong list";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Count_Type'Last - Count then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      New_Node := new Node_Type'(New_Item, null, null);
      Insert_Internal (Container, Before.Node, New_Node);

      Position := Cursor'(Container'Unchecked_Access, New_Node);

      for J in Count_Type'(2) .. Count loop
         New_Node := new Node_Type'(New_Item, null, null);
         Insert_Internal (Container, Before.Node, New_Node);
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
      New_Node : Node_Access;

   begin
      if Before.Container /= null then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong list";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Count_Type'Last - Count then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      New_Node := new Node_Type;
      Insert_Internal (Container, Before.Node, New_Node);

      Position := Cursor'(Container'Unchecked_Access, New_Node);

      for J in Count_Type'(2) .. Count loop
         New_Node := new Node_Type;
         Insert_Internal (Container, Before.Node, New_Node);
      end loop;
   end Insert;

   ---------------------
   -- Insert_Internal --
   ---------------------

   procedure Insert_Internal
     (Container : in out List;
      Before    : Node_Access;
      New_Node  : Node_Access)
   is
   begin
      if Container.Length = 0 then
         pragma Assert (Before = null);
         pragma Assert (Container.First = null);
         pragma Assert (Container.Last = null);

         Container.First := New_Node;
         Container.Last := New_Node;

      elsif Before = null then
         pragma Assert (Container.Last.Next = null);

         Container.Last.Next := New_Node;
         New_Node.Prev := Container.Last;

         Container.Last := New_Node;

      elsif Before = Container.First then
         pragma Assert (Container.First.Prev = null);

         Container.First.Prev := New_Node;
         New_Node.Next := Container.First;

         Container.First := New_Node;

      else
         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);

         New_Node.Next := Before;
         New_Node.Prev := Before.Prev;

         Before.Prev.Next := New_Node;
         Before.Prev := New_Node;
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
      B : Natural renames C.Busy;

      Node : Node_Access := Container.First;

   begin
      B := B + 1;

      begin
         while Node /= null loop
            Process (Cursor'(Container'Unchecked_Access, Node));
            Node := Node.Next;
         end loop;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
   begin
      if Container.Last = null then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.Last.Element;
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out List;
      Source : in out List)
   is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      Clear (Target);

      Target.First := Source.First;
      Source.First := null;

      Target.Last := Source.Last;
      Source.Last := null;

      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = null then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Next");

      declare
         Next_Node : constant Node_Access := Position.Node.Next;
      begin
         if Next_Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Next_Node);
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
      if Position.Node = null then
         return No_Element;
      end if;

      pragma Assert (Vet (Position), "bad cursor in Previous");

      declare
         Prev_Node : constant Node_Access := Position.Node.Prev;
      begin
         if Prev_Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Prev_Node);
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
      if Position.Node = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         C : List renames Position.Container.all'Unrestricted_Access.all;
         B : Natural renames C.Busy;
         L : Natural renames C.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (Position.Node.Element);
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
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out List)
   is
      N : Count_Type'Base;
      X : Node_Access;

   begin
      Clear (Item);
      Count_Type'Base'Read (Stream, N);

      if N = 0 then
         return;
      end if;

      X := new Node_Type;

      begin
         Element_Type'Read (Stream, X.Element);
      exception
         when others =>
            Free (X);
            raise;
      end;

      Item.First := X;
      Item.Last := X;

      loop
         Item.Length := Item.Length + 1;
         exit when Item.Length = N;

         X := new Node_Type;

         begin
            Element_Type'Read (Stream, X.Element);
         exception
            when others =>
               Free (X);
               raise;
         end;

         X.Prev := Item.Last;
         Item.Last.Next := X;
         Item.Last := X;
      end loop;
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream list cursor";
   end Read;

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
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unchecked_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is locked)";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      Position.Node.Element := New_Item;
   end Replace_Element;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out List) is
      I : Node_Access := Container.First;
      J : Node_Access := Container.Last;

      procedure Swap (L, R : Node_Access);

      ----------
      -- Swap --
      ----------

      procedure Swap (L, R : Node_Access) is
         LN : constant Node_Access := L.Next;
         LP : constant Node_Access := L.Prev;

         RN : constant Node_Access := R.Next;
         RP : constant Node_Access := R.Prev;

      begin
         if LP /= null then
            LP.Next := R;
         end if;

         if RN /= null then
            RN.Prev := L;
         end if;

         L.Next := RN;
         R.Prev := LP;

         if LN = R then
            pragma Assert (RP = L);

            L.Prev := R;
            R.Next := L;

         else
            L.Prev := RP;
            RP.Next := L;

            R.Next := LN;
            LN.Prev := R;
         end if;
      end Swap;

   --  Start of processing for Reverse_Elements

   begin
      if Container.Length <= 1 then
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      Container.First := J;
      Container.Last := I;
      loop
         Swap (L => I, R => J);

         J := J.Next;
         exit when I = J;

         I := I.Prev;
         exit when I = J;

         Swap (L => J, R => I);

         I := I.Next;
         exit when I = J;

         J := J.Prev;
         exit when I = J;
      end loop;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
   end Reverse_Elements;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Node : Node_Access := Position.Node;

   begin
      if Node = null then
         Node := Container.Last;

      else
         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert (Vet (Position), "bad cursor in Reverse_Find");
      end if;

      while Node /= null loop
         if Node.Element = Item then
            return Cursor'(Container'Unchecked_Access, Node);
         end if;

         Node := Node.Prev;
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
      B : Natural renames C.Busy;

      Node : Node_Access := Container.Last;

   begin
      B := B + 1;

      begin
         while Node /= null loop
            Process (Cursor'(Container'Unchecked_Access, Node));
            Node := Node.Prev;
         end loop;

      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Reverse_Iterate;

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List)
   is
   begin
      if Before.Container /= null then
         if Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Splice");
      end if;

      if Target'Address = Source'Address
        or else Source.Length = 0
      then
         return;
      end if;

      pragma Assert (Source.First.Prev = null);
      pragma Assert (Source.Last.Next = null);

      if Target.Length > Count_Type'Last - Source.Length then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Target (list is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      if Target.Length = 0 then
         pragma Assert (Target.First = null);
         pragma Assert (Target.Last = null);
         pragma Assert (Before = No_Element);

         Target.First := Source.First;
         Target.Last := Source.Last;

      elsif Before.Node = null then
         pragma Assert (Target.Last.Next = null);

         Target.Last.Next := Source.First;
         Source.First.Prev := Target.Last;

         Target.Last := Source.Last;

      elsif Before.Node = Target.First then
         pragma Assert (Target.First.Prev = null);

         Source.Last.Next := Target.First;
         Target.First.Prev := Source.Last;

         Target.First := Source.First;

      else
         pragma Assert (Target.Length >= 2);

         Before.Node.Prev.Next := Source.First;
         Source.First.Prev := Before.Node.Prev;

         Before.Node.Prev := Source.Last;
         Source.Last.Next := Before.Node;
      end if;

      Source.First := null;
      Source.Last := null;

      Target.Length := Target.Length + Source.Length;
      Source.Length := 0;
   end Splice;

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   is
   begin
      if Before.Container /= null then
         if Before.Container /= Container'Unchecked_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Position.Node = Before.Node
        or else Position.Node.Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      if Before.Node = null then
         pragma Assert (Position.Node /= Container.Last);

         if Position.Node = Container.First then
            Container.First := Position.Node.Next;
            Container.First.Prev := null;
         else
            Position.Node.Prev.Next := Position.Node.Next;
            Position.Node.Next.Prev := Position.Node.Prev;
         end if;

         Container.Last.Next := Position.Node;
         Position.Node.Prev := Container.Last;

         Container.Last := Position.Node;
         Container.Last.Next := null;

         return;
      end if;

      if Before.Node = Container.First then
         pragma Assert (Position.Node /= Container.First);

         if Position.Node = Container.Last then
            Container.Last := Position.Node.Prev;
            Container.Last.Next := null;
         else
            Position.Node.Prev.Next := Position.Node.Next;
            Position.Node.Next.Prev := Position.Node.Prev;
         end if;

         Container.First.Prev := Position.Node;
         Position.Node.Next := Container.First;

         Container.First := Position.Node;
         Container.First.Prev := null;

         return;
      end if;

      if Position.Node = Container.First then
         Container.First := Position.Node.Next;
         Container.First.Prev := null;

      elsif Position.Node = Container.Last then
         Container.Last := Position.Node.Prev;
         Container.Last.Next := null;

      else
         Position.Node.Prev.Next := Position.Node.Next;
         Position.Node.Next.Prev := Position.Node.Prev;
      end if;

      Before.Node.Prev.Next := Position.Node;
      Position.Node.Prev := Before.Node.Prev;

      Before.Node.Prev := Position.Node;
      Position.Node.Next := Before.Node;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
   end Splice;

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor)
   is
   begin
      if Target'Address = Source'Address then
         Splice (Target, Before, Position);
         return;
      end if;

      if Before.Container /= null then
         if Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Source'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Target.Length = Count_Type'Last then
         raise Constraint_Error with "Target is full";
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Target (list is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      if Position.Node = Source.First then
         Source.First := Position.Node.Next;

         if Position.Node = Source.Last then
            pragma Assert (Source.First = null);
            pragma Assert (Source.Length = 1);
            Source.Last := null;

         else
            Source.First.Prev := null;
         end if;

      elsif Position.Node = Source.Last then
         pragma Assert (Source.Length >= 2);
         Source.Last := Position.Node.Prev;
         Source.Last.Next := null;

      else
         pragma Assert (Source.Length >= 3);
         Position.Node.Prev.Next := Position.Node.Next;
         Position.Node.Next.Prev := Position.Node.Prev;
      end if;

      if Target.Length = 0 then
         pragma Assert (Target.First = null);
         pragma Assert (Target.Last = null);
         pragma Assert (Before = No_Element);

         Target.First := Position.Node;
         Target.Last := Position.Node;

         Target.First.Prev := null;
         Target.Last.Next := null;

      elsif Before.Node = null then
         pragma Assert (Target.Last.Next = null);
         Target.Last.Next := Position.Node;
         Position.Node.Prev := Target.Last;

         Target.Last := Position.Node;
         Target.Last.Next := null;

      elsif Before.Node = Target.First then
         pragma Assert (Target.First.Prev = null);
         Target.First.Prev := Position.Node;
         Position.Node.Next := Target.First;

         Target.First := Position.Node;
         Target.First.Prev := null;

      else
         pragma Assert (Target.Length >= 2);
         Before.Node.Prev.Next := Position.Node;
         Position.Node.Prev := Before.Node.Prev;

         Before.Node.Prev := Position.Node;
         Position.Node.Next := Before.Node;
      end if;

      Target.Length := Target.Length + 1;
      Source.Length := Source.Length - 1;

      Position.Container := Target'Unchecked_Access;
   end Splice;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      if I.Node = null then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = null then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Container /= Container'Unchecked_Access then
         raise Program_Error with "I cursor designates wrong container";
      end if;

      if J.Container /= Container'Unchecked_Access then
         raise Program_Error with "J cursor designates wrong container";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is locked)";
      end if;

      pragma Assert (Vet (I), "bad I cursor in Swap");
      pragma Assert (Vet (J), "bad J cursor in Swap");

      declare
         EI : Element_Type renames I.Node.Element;
         EJ : Element_Type renames J.Node.Element;

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
      if I.Node = null then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = null then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Container /= Container'Unrestricted_Access then
         raise Program_Error with "I cursor designates wrong container";
      end if;

      if J.Container /= Container'Unrestricted_Access then
         raise Program_Error with "J cursor designates wrong container";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is busy)";
      end if;

      pragma Assert (Vet (I), "bad I cursor in Swap_Links");
      pragma Assert (Vet (J), "bad J cursor in Swap_Links");

      declare
         I_Next : constant Cursor := Next (I);

      begin
         if I_Next = J then
            Splice (Container, Before => I, Position => J);

         else
            declare
               J_Next : constant Cursor := Next (J);

            begin
               if J_Next = I then
                  Splice (Container, Before => J, Position => I);

               else
                  pragma Assert (Container.Length >= 3);

                  Splice (Container, Before => I_Next, Position => J);
                  Splice (Container, Before => J_Next, Position => I);
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
      if Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unchecked_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Update_Element");

      declare
         B : Natural renames Container.Busy;
         L : Natural renames Container.Lock;

      begin
         B := B + 1;
         L := L + 1;

         begin
            Process (Position.Node.Element);
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

   function Vet (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         return Position.Container = null;
      end if;

      if Position.Container = null then
         return False;
      end if;

      if Position.Node.Next = Position.Node then
         return False;
      end if;

      if Position.Node.Prev = Position.Node then
         return False;
      end if;

      declare
         L : List renames Position.Container.all;
      begin
         if L.Length = 0 then
            return False;
         end if;

         if L.First = null then
            return False;
         end if;

         if L.Last = null then
            return False;
         end if;

         if L.First.Prev /= null then
            return False;
         end if;

         if L.Last.Next /= null then
            return False;
         end if;

         if Position.Node.Prev = null
           and then Position.Node /= L.First
         then
            return False;
         end if;

         --  If we get here, we know that this disjunction is true:
         --  Position.Node.Prev /= null or else Position.Node = L.First

         if Position.Node.Next = null
           and then Position.Node /= L.Last
         then
            return False;
         end if;

         --  If we get here, we know that this disjunction is true:
         --  Position.Node.Next /= null or else Position.Node = L.Last

         if L.Length = 1 then
            return L.First = L.Last;
         end if;

         if L.First = L.Last then
            return False;
         end if;

         if L.First.Next = null then
            return False;
         end if;

         if L.Last.Prev = null then
            return False;
         end if;

         if L.First.Next.Prev /= L.First then
            return False;
         end if;

         if L.Last.Prev.Next /= L.Last then
            return False;
         end if;

         if L.Length = 2 then
            if L.First.Next /= L.Last then
               return False;
            end if;

            if L.Last.Prev /= L.First then
               return False;
            end if;

            return True;
         end if;

         if L.First.Next = L.Last then
            return False;
         end if;

         if L.Last.Prev = L.First then
            return False;
         end if;

         if Position.Node = L.First then  -- eliminates earlier disjunct
            return True;
         end if;

         --  If we get here, we know, per disjunctive syllogism (modus
         --  tollendo ponens), that this predicate is true:
         --  Position.Node.Prev /= null

         if Position.Node = L.Last then  -- eliminates earlier disjunct
            return True;
         end if;

         --  If we get here, we know, per disjunctive syllogism (modus
         --  tollendo ponens), that this predicate is true:
         --  Position.Node.Next /= null

         if Position.Node.Next.Prev /= Position.Node then
            return False;
         end if;

         if Position.Node.Prev.Next /= Position.Node then
            return False;
         end if;

         if L.Length = 3 then
            if L.First.Next /= Position.Node then
               return False;
            end if;

            if L.Last.Prev /= Position.Node then
               return False;
            end if;
         end if;

         return True;
      end;
   end Vet;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : List)
   is
      Node : Node_Access := Item.First;

   begin
      Count_Type'Base'Write (Stream, Item.Length);

      while Node /= null loop
         Element_Type'Write (Stream, Node.Element);
         Node := Node.Next;
      end loop;
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream list cursor";
   end Write;

end Ada.Containers.Doubly_Linked_Lists;
