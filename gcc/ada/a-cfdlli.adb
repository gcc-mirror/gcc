------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_DOUBLY_LINKED_LISTS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2011, Free Software Foundation, Inc.         --
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

with System;  use type System.Address;
with Ada.Finalization;

package body Ada.Containers.Formal_Doubly_Linked_Lists is

   type Iterator is new Ada.Finalization.Limited_Controlled and
     List_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : List_Access;
      Node      : Count_Type;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate
     (Container : in out List;
      New_Item  : Element_Type;
      New_Node  : out Count_Type);

   procedure Allocate
     (Container : in out List;
      New_Node  : out Count_Type);

   procedure Free
     (Container : in out List;
      X         : Count_Type);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Count_Type;
      New_Node  : Count_Type);

   function Vet (L : List; Position : Cursor) return Boolean;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      LI, RI : Count_Type;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      LI := Left.First;
      RI := Left.First;
      while LI /= 0 loop
         if Left.Nodes (LI).Element /= Right.Nodes (LI).Element then
            return False;
         end if;

         LI := Left.Nodes (LI).Next;
         RI := Right.Nodes (RI).Next;
      end loop;

      return True;
   end "=";

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Container : in out List;
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

   procedure Allocate
     (Container : in out List;
      New_Node  : out Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Free >= 0 then
         New_Node := Container.Free;
         Container.Free := N (New_Node).Next;

      else
         New_Node := abs Container.Free;
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
      N : Node_Array renames Source.Nodes;
      J : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      J := Source.First;
      while J /= 0 loop
         Append (Target, N (J).Element);
         J := N (J).Next;
      end loop;
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
         pragma Assert (Container.Busy = 0);
         pragma Assert (Container.Lock = 0);
         return;
      end if;

      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

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

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : List;
      Capacity : Count_Type := 0) return List
   is
      C : constant Count_Type := Count_Type'Max (Source.Capacity, Capacity);
      N : Count_Type;
      P : List (C);

   begin
      N := 1;
      while N <= Source.Capacity loop
         P.Nodes (N).Prev := Source.Nodes (N).Prev;
         P.Nodes (N).Next := Source.Nodes (N).Next;
         P.Nodes (N).Element := Source.Nodes (N).Element;
         N := N + 1;
      end loop;

      P.Free := Source.Free;
      P.Length := Source.Length;
      P.First := Source.First;
      P.Last := Source.Last;

      if P.Free >= 0 then
         N := Source.Capacity + 1;
         while N <= C loop
            Free (P, N);
            N := N + 1;
         end loop;
      end if;

      return P;
   end Copy;

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
      if not Has_Element (Container => Container,
                          Position  => Position)
      then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in Delete");
      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

      if Position.Node = Container.First then
         Delete_First (Container, Count);
         Position := No_Element;
         return;
      end if;

      if Count = 0 then
         Position := No_Element;
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

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
         pragma Assert (N (Position.Node).Prev >= 0);

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

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

      for J in 1 .. Count loop
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

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

      for J in 1 .. Count loop
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

   function Element
     (Container : List;
      Position  : Cursor) return Element_Type
   is
   begin
      if not Has_Element (Container => Container, Position  => Position) then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      return Container.Nodes (Position.Node).Element;
   end Element;

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

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      From : Count_Type := Position.Node;

   begin
      if From = 0 and Container.Length = 0 then
         return No_Element;
      end if;

      if From = 0 then
         From := Container.First;
      end if;

      if Position.Node /= 0 and then
        not Has_Element (Container, Position)
      then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      while From /= 0 loop
         if Container.Nodes (From).Element = Item then
            return (Node => From);
         end if;

         From := Container.Nodes (From).Next;
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

      return (Node => Container.First);
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Node component is null, this means the iterator object was
      --  constructed without a start expression, in which case the (forward)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  of items (corresponding to Container.First, for a forward iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is non-null, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (forward) partial iteration begins.

      if Object.Node = 0 then
         return First (Object.Container.all);
      else
         return (Node => Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
      F : constant Count_Type := Container.First;
   begin
      if F = 0 then
         raise Constraint_Error with "list is empty";
      else
         return Container.Nodes (F).Element;
      end if;
   end First_Element;

   ----------
   -- Free --
   ----------

   procedure Free
     (Container : in out List;
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
            for J in Container.Free .. Container.Capacity - 1 loop
               N (J).Next := J + 1;
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
         for J in 2 .. Container.Length loop
            if Nodes (Nodes (Node).Next).Element < Nodes (Node).Element then
               return False;
            else
               Node := Nodes (Node).Next;
            end if;
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
         LN : Node_Array renames Target.Nodes;
         RN : Node_Array renames Source.Nodes;
         LI : Cursor;
         RI : Cursor;

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
         while RI.Node /= 0 loop
            pragma Assert (RN (RI.Node).Next = 0
              or else not (RN (RN (RI.Node).Next).Element <
                  RN (RI.Node).Element));

            if LI.Node = 0 then
               Splice (Target, No_Element, Source);
               return;
            end if;

            pragma Assert (LN (LI.Node).Next = 0
              or else not (LN (LN (LI.Node).Next).Element <
                  LN (LI.Node).Element));

            if RN (RI.Node).Element < LN (LI.Node).Element then
               declare
                  RJ : Cursor := RI;
                  pragma Warnings (Off, RJ);
               begin
                  RI.Node := RN (RI.Node).Next;
                  Splice (Target, LI, Source, RJ);
               end;

            else
               LI.Node := LN (LI.Node).Next;
            end if;
         end loop;
      end Merge;

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
            Node : Count_Type;

         begin
            Node := N (Pivot).Next;
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

         if Container.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with elements (list is busy)";
         end if;

         Sort (Front => 0, Back => 0);

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : List; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return False;
      end if;

      return Container.Nodes (Position.Node).Prev /= -1;
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
      if Before.Node /= 0 then
         pragma Assert (Vet (Container, Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Container.Capacity - Count then
         raise Constraint_Error with "new length exceeds capacity";
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

      Allocate (Container, New_Item, New_Node => J);
      Insert_Internal (Container, Before.Node, New_Node => J);
      Position := (Node => J);

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
   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      J : Count_Type;

   begin
      if Before.Node /= 0 then
         pragma Assert (Vet (Container, Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Container.Capacity - Count then
         raise Constraint_Error with "new length exceeds capacity";
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

      Allocate (Container, New_Node => J);
      Insert_Internal (Container, Before.Node, New_Node => J);
      Position := (Node => J);

      for Index in 2 .. Count loop
         Allocate (Container, New_Node => J);
         Insert_Internal (Container, Before.Node, New_Node => J);
      end loop;
   end Insert;

   ---------------------
   -- Insert_Internal --
   ---------------------

   procedure Insert_Internal
     (Container : in out List;
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
      return Length (Container) = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : List;
      Process   :
      not null access procedure (Container : List; Position : Cursor))
   is
      C    : List renames Container'Unrestricted_Access.all;
      B    : Natural renames C.Busy;
      Node : Count_Type;

   begin
      B := B + 1;

      begin
         Node := Container.First;
         while Node /= 0 loop
            Process (Container, (Node => Node));
            Node := Container.Nodes (Node).Next;
         end loop;

      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   function Iterate (Container : List)
     return List_Iterator_Interfaces.Reversible_Iterator'Class
   is
      B : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is null (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a
      --  complete iterator, meaning that the iteration starts from the
      --  (logical) beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
                    Iterator'(Ada.Finalization.Limited_Controlled with
                                Container => Container'Unrestricted_Access,
                                Node      => 0)
      do
         B := B + 1;
      end return;
   end Iterate;

   function Iterate (Container : List; Start : Cursor)
     return List_Iterator_Interfaces.Reversible_Iterator'Class
   is
      B  : Natural renames Container'Unrestricted_Access.all.Busy;

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

      if not Has_Element (Container, Start) then
         raise Constraint_Error with
           "Start position for iterator is not a valid cursor";
      end if;

      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is non-null (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.

      return It : constant Iterator :=
                    Iterator'(Ada.Finalization.Limited_Controlled with
                                Container => Container'Unrestricted_Access,
                                Node      => Start.Node)
      do
         B := B + 1;
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = 0 then
         return No_Element;
      end if;
      return (Node => Container.Last);
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Node component is null, this means the iterator object was
      --  constructed without a start expression, in which case the (reverse)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is non-null, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (reverse) partial iteration begins.

      if Object.Node = 0 then
         return Last (Object.Container.all);
      else
         return (Node => Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
      L : constant Count_Type := Container.Last;
   begin
      if L = 0 then
         raise Constraint_Error with "list is empty";
      else
         return Container.Nodes (L).Element;
      end if;
   end Last_Element;

   ----------
   -- Left --
   ----------

   function Left (Container : List; Position : Cursor) return List is
      Curs : Cursor := Position;
      C    : List (Container.Capacity) := Copy (Container, Container.Capacity);
      Node : Count_Type;

   begin
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
      N : Node_Array renames Source.Nodes;
      X : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      Clear (Target);

      while Source.Length > 1 loop
         pragma Assert (Source.First in 1 .. Source.Capacity);
         pragma Assert (Source.Last /= Source.First);
         pragma Assert (N (Source.First).Prev = 0);
         pragma Assert (N (Source.Last).Next = 0);

         --  Copy first element from Source to Target

         X := Source.First;
         Append (Target, N (X).Element);  -- optimize away???

         --  Unlink first node of Source

         Source.First := N (X).Next;
         N (Source.First).Prev := 0;

         Source.Length := Source.Length - 1;

         --  The representation invariants for Source have been restored. It is
         --  now safe to free the unlinked node, without fear of corrupting the
         --  active links of Source.

         --  Note that the algorithm we use here models similar algorithms used
         --  in the unbounded form of the doubly-linked list container. In that
         --  case, Free is an instantation of Unchecked_Deallocation, which can
         --  fail (because PE will be raised if controlled Finalize fails), so
         --  we must defer the call until the last step. Here in the bounded
         --  form, Free merely links the node we have just "deallocated" onto a
         --  list of inactive nodes, so technically Free cannot fail. However,
         --  for consistency, we handle Free the same way here as we do for the
         --  unbounded form, with the pessimistic assumption that it can fail.

         Free (Source, X);
      end loop;

      if Source.Length = 1 then
         pragma Assert (Source.First in 1 .. Source.Capacity);
         pragma Assert (Source.Last = Source.First);
         pragma Assert (N (Source.First).Prev = 0);
         pragma Assert (N (Source.Last).Next = 0);

         --  Copy element from Source to Target

         X := Source.First;
         Append (Target, N (X).Element);

         --  Unlink node of Source

         Source.First := 0;
         Source.Last := 0;
         Source.Length := 0;

         --  Return the unlinked node to the free store

         Free (Source, X);
      end if;
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Container : List; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   function Next (Container : List; Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Program_Error with "Position cursor has no element";
      end if;

      return (Node => Container.Nodes (Position.Node).Next);
   end Next;

   function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      return Next (Object.Container.all, Position);
   end Next;

   --------------------
   -- Not_No_Element --
   --------------------

   function Not_No_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Not_No_Element;

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

   procedure Previous (Container : List; Position : in out Cursor) is
   begin
      Position := Previous (Container, Position);
   end Previous;

   function Previous (Container : List; Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Program_Error with "Position cursor has no element";
      end if;

      return (Node => Container.Nodes (Position.Node).Prev);
   end Previous;

   function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      return Previous (Object.Container.all, Position);
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : List; Position : Cursor;
      Process   : not null access procedure (Element : Element_Type))
   is
      C : List renames Container'Unrestricted_Access.all;
      B : Natural renames C.Busy;
      L : Natural renames C.Lock;

   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      B := B + 1;
      L := L + 1;

      declare
         N : Node_Type renames C.Nodes (Position.Node);
      begin
         Process (N.Element);
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

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out List)
   is
      N : Count_Type'Base;

   begin
      Clear (Item);

      Count_Type'Base'Read (Stream, N);

      if N < 0 then
         raise Program_Error with "bad list length";
      end if;

      if N = 0 then
         return;
      end if;

      if N > Item.Capacity then
         raise Constraint_Error with "length exceeds capacity";
      end if;

      for J in 1 .. N loop
         Item.Append (Element_Type'Input (Stream));  -- ???
      end loop;
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream list cursor";
   end Read;

   ---------------
   -- Reference --
   ---------------

   function Constant_Reference
     (Container : List;
      Position  : Cursor) return Constant_Reference_Type
   is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      return (Element => Container.Nodes (Position.Node).Element'Access);
   end Constant_Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is locked)";
      end if;

      pragma Assert
        (Vet (Container, Position), "bad cursor in Replace_Element");

      declare
         N : Node_Array renames Container.Nodes;
      begin
         N (Position.Node).Element := New_Item;
      end;

      --  Above is peculiar, why not simply
      --  Container.Nodes (Position.Node).Element := New_Item ???

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

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

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
      CFirst : Count_Type := Position.Node;

   begin
      if CFirst = 0 then
         CFirst := Container.First;
      end if;

      if Container.Length = 0 then
         return No_Element;
      end if;

      while CFirst /= 0 loop
         if Container.Nodes (CFirst).Element = Item then
            return (Node => CFirst);
         end if;
         CFirst := Container.Nodes (CFirst).Prev;
      end loop;

      return No_Element;
   end Reverse_Find;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   :
      not null access procedure (Container : List; Position : Cursor))
   is
      C : List renames Container'Unrestricted_Access.all;
      B : Natural renames C.Busy;

      Node : Count_Type;

   begin
      B := B + 1;

      begin
         Node := Container.Last;
         while Node /= 0 loop
            Process (Container, (Node => Node));
            Node := Container.Nodes (Node).Prev;
         end loop;

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

   function Right (Container : List; Position : Cursor) return List is
      Curs : Cursor := First (Container);
      C    : List (Container.Capacity) := Copy (Container, Container.Capacity);
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

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List)
   is
      SN : Node_Array renames Source.Nodes;

   begin
      if Before.Node /= 0 then
         pragma Assert (Vet (Target, Before), "bad cursor in Splice");
      end if;

      if Target'Address = Source'Address
        or else Source.Length = 0
      then
         return;
      end if;

      pragma Assert (SN (Source.First).Prev = 0);
      pragma Assert (SN (Source.Last).Next = 0);

      if Target.Length > Count_Type'Base'Last - Source.Length then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Target.Length + Source.Length > Target.Capacity then
         raise Constraint_Error;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Target (list is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      loop
         Insert (Target, Before, SN (Source.Last).Element);
         Delete_Last (Source);
         exit when Is_Empty (Source);
      end loop;
   end Splice;

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor)
   is
      Target_Position : Cursor;

   begin
      if Target'Address = Source'Address then
         Splice (Target, Before, Position);
         return;
      end if;

      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Source, Position), "bad Position cursor in Splice");

      if Target.Length >= Target.Capacity then
         raise Constraint_Error;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Target (list is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors of Source (list is busy)";
      end if;

      Insert
        (Container => Target,
         Before    => Before,
         New_Item  => Source.Nodes (Position.Node).Element,
         Position  => Target_Position);

      Delete (Source, Position);
      Position := Target_Position;
   end Splice;

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Before.Node /= 0 then
         pragma Assert
           (Vet (Container, Before), "bad Before cursor in Splice");
      end if;

      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert
        (Vet (Container, Position), "bad Position cursor in Splice");

      if Position.Node = Before.Node
        or else N (Position.Node).Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

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

   ------------------
   -- Strict_Equal --
   ------------------

   function Strict_Equal (Left, Right : List) return Boolean is
      CL : Count_Type := Left.First;
      CR : Count_Type := Right.First;

   begin
      while CL /= 0 or CR /= 0 loop
         if CL /= CR or else
           Left.Nodes (CL).Element /= Right.Nodes (CL).Element
         then
            return False;
         end if;

         CL := Left.Nodes (CL).Next;
         CR := Right.Nodes (CR).Next;
      end loop;

      return True;
   end Strict_Equal;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      if I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = 0 then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (list is locked)";
      end if;

      pragma Assert (Vet (Container, I), "bad I cursor in Swap");
      pragma Assert (Vet (Container, J), "bad J cursor in Swap");

      declare
         NN : Node_Array renames Container.Nodes;
         NI : Node_Type renames NN (I.Node);
         NJ : Node_Type renames NN (J.Node);

         EI_Copy : constant Element_Type := NI.Element;

      begin
         NI.Element := NJ.Element;
         NJ.Element := EI_Copy;
      end;
   end Swap;

   ----------------
   -- Swap_Links --
   ----------------

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor)
   is
      I_Next, J_Next : Cursor;

   begin
      if I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = 0 then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with elements (list is busy)";
      end if;

      pragma Assert (Vet (Container, I), "bad I cursor in Swap_Links");
      pragma Assert (Vet (Container, J), "bad J cursor in Swap_Links");

      I_Next := Next (Container, I);

      if I_Next = J then
         Splice (Container, Before => I, Position => J);

      else
         J_Next := Next (Container, J);

         if J_Next = I then
            Splice (Container, Before => J, Position => I);

         else
            pragma Assert (Container.Length >= 3);
            Splice (Container, Before => I_Next, Position => J);
            Splice (Container, Before => J_Next, Position => I);
         end if;
      end if;
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
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert
        (Vet (Container, Position), "bad cursor in Update_Element");

      declare
         B : Natural renames Container.Busy;
         L : Natural renames Container.Lock;

      begin
         B := B + 1;
         L := L + 1;

         declare
            N : Node_Type renames Container.Nodes (Position.Node);
         begin
            Process (N.Element);
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

   function Vet (L : List; Position : Cursor) return Boolean is
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
   end Vet;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : List)
   is
      N    : Node_Array renames Item.Nodes;
      Node : Count_Type;

   begin
      Count_Type'Base'Write (Stream, Item.Length);

      Node := Item.First;
      while Node /= 0 loop
         Element_Type'Write (Stream, N (Node).Element);
         Node := N (Node).Next;
      end loop;
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream list cursor";
   end Write;

end Ada.Containers.Formal_Doubly_Linked_Lists;
