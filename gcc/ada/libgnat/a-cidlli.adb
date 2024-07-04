------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               ADA.CONTAINERS.INDEFINITE_DOUBLY_LINKED_LISTS              --
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

with Ada.Containers.Stable_Sorting; use Ada.Containers.Stable_Sorting;

with System; use type System.Address;
with System.Put_Images;

package body Ada.Containers.Indefinite_Doubly_Linked_Lists with
  SPARK_Mode => Off
is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free (X : in out Node_Access);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Node_Access;
      New_Node  : Node_Access);

   procedure Splice_Internal
     (Target : in out List;
      Before : Node_Access;
      Source : in out List);

   procedure Splice_Internal
     (Target   : in out List;
      Before   : Node_Access;
      Source   : in out List;
      Position : Node_Access);

   function Vet (Position : Cursor) return Boolean with Inline;
   --  Checks invariants of the cursor and its designated container, as a
   --  simple way of detecting dangling references (see operation Free for a
   --  description of the detection mechanism), returning True if all checks
   --  pass. Invocations of Vet are used here as the argument of pragma Assert,
   --  so the checks are performed only when assertions are enabled.

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      if Left.Length = 0 then
         return True;
      end if;

      declare
         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

         L : Node_Access := Left.First;
         R : Node_Access := Right.First;
      begin
         for J in 1 .. Left.Length loop
            if L.Element.all /= R.Element.all then
               return False;
            end if;

            L := L.Next;
            R := R.Next;
         end loop;
      end;

      return True;
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out List) is
      Src : Node_Access := Container.First;
      Dst : Node_Access;

   begin
      --  If the counts are nonzero, execution is technically erroneous, but
      --  it seems friendly to allow things like concurrent "=" on shared
      --  constants.

      Zero_Counts (Container.TC);

      if Src = null then
         pragma Assert (Container.Last = null);
         pragma Assert (Container.Length = 0);
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
      pragma Assert (Container.Length > 0);

      Container.First := null;
      Container.Last := null;
      Container.Length := 0;

      declare
         Element : Element_Access := new Element_Type'(Src.Element.all);
      begin
         Dst := new Node_Type'(Element, null, null);
      exception
         when others =>
            Free (Element);
            raise;
      end;

      Container.First := Dst;
      Container.Last := Dst;
      Container.Length := 1;

      Src := Src.Next;
      while Src /= null loop
         declare
            Element : Element_Access := new Element_Type'(Src.Element.all);
         begin
            Dst := new Node_Type'(Element, null, Prev => Container.Last);
         exception
            when others =>
               Free (Element);
               raise;
         end;

         Container.Last.Next := Dst;
         Container.Last := Dst;
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
      Count     : Count_Type)
   is
   begin
      Insert (Container, No_Element, New_Item, Count);
   end Append;

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type)
   is
   begin
      Insert (Container, No_Element, New_Item, 1);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out List; Source : List) is
      Node : Node_Access;

   begin
      if Target'Address = Source'Address then
         return;

      else
         Target.Clear;

         Node := Source.First;
         while Node /= null loop
            Target.Append (Node.Element.all);
            Node := Node.Next;
         end loop;
      end if;
   end Assign;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out List) is
      X : Node_Access;
      pragma Warnings (Off, X);

   begin
      if Container.Length = 0 then
         pragma Assert (Container.First = null);
         pragma Assert (Container.Last = null);
         pragma Assert (Container.TC = (Busy => 0, Lock => 0));
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);

      TC_Check (Container.TC);

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

      Free (X);
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased List;
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

      if Checks and then Position.Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Constant_Reference");

      declare
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => Position.Node.Element,
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
     (Container : List;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy (Source : List) return List is
   begin
      return Target : List do
         Target.Assign (Source);
      end return;
   end Copy;

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
      TC_Check (Container.TC);

      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");

      if Position.Node = Container.First then
         Delete_First (Container, Count);
         Position := No_Element;  --  Post-York behavior
         return;
      end if;

      if Count = 0 then
         Position := No_Element;  --  Post-York behavior
         return;
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

      --  Fix this junk comment ???

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

      TC_Check (Container.TC);

      for J in 1 .. Count loop
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

      TC_Check (Container.TC);

      for J in 1 .. Count loop
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
      if Checks and then Position.Node = null then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Element");

      return Position.Node.Element.all;
   end Element;

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
         if Checks and then Node.Element = null then
            raise Program_Error;
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert (Vet (Position), "bad cursor in Find");
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock : With_Lock (Container.TC'Unrestricted_Access);
      begin
         while Node /= null loop
            if Node.Element.all = Item then
               return Cursor'(Container'Unrestricted_Access, Node);
            end if;

            Node := Node.Next;
         end loop;

         return No_Element;
      end;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.First);
      end if;
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

      if Object.Node = null then
         return Indefinite_Doubly_Linked_Lists.First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
   begin
      if Checks and then Container.First = null then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.First.Element.all;
   end First_Element;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   begin
      --  While a node is in use, as an active link in a list, its Previous and
      --  Next components must be null, or designate a different node; this is
      --  a node invariant. For this indefinite list, there is an additional
      --  invariant: that the element access value be non-null. Before actually
      --  deallocating the node, we set the node access value components of the
      --  node to point to the node itself, and set the element access value to
      --  null (by deallocating the node's element), thus falsifying the node
      --  invariant. Subprogram Vet inspects the value of the node components
      --  when interrogating the node, in order to detect whether the cursor's
      --  node access value is dangling.

      --  Note that we have no guarantee that the storage for the node isn't
      --  modified when it is deallocated, but there are other tests that Vet
      --  does if node invariants appear to be satisifed. However, in practice
      --  this simple test works well enough, detecting dangling references
      --  immediately, without needing further interrogation.

      X.Next := X;
      X.Prev := X;

      begin
         Free (X.Element);
      exception
         when others =>
            X.Element := null;
            Deallocate (X);
            raise;
      end;

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
         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         Lock : With_Lock (Container.TC'Unrestricted_Access);

         Node   : Node_Access;
      begin
         Node := Container.First;
         for J in 2 .. Container.Length loop
            if Node.Next.Element.all < Node.Element.all then
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
      begin
         --  The semantics of Merge changed slightly per AI05-0021. It was
         --  originally the case that if Target and Source denoted the same
         --  container object, then the GNAT implementation of Merge did
         --  nothing. However, it was argued that RM05 did not precisely
         --  specify the semantics for this corner case. The decision of the
         --  ARG was that if Target and Source denote the same non-empty
         --  container object, then Program_Error is raised.

         if Source.Is_Empty then
            return;
         end if;

         TC_Check (Target.TC);
         TC_Check (Source.TC);

         if Checks and then Target'Address = Source'Address then
            raise Program_Error with
              "Target and Source denote same non-empty container";
         end if;

         if Checks and then Target.Length > Count_Type'Last - Source.Length
         then
            raise Constraint_Error with "new length exceeds maximum";
         end if;

         declare
            Lock_Target : With_Lock (Target.TC'Unchecked_Access);
            Lock_Source : With_Lock (Source.TC'Unchecked_Access);

            LI, RI, RJ : Node_Access;

         begin
            LI := Target.First;
            RI := Source.First;
            while RI /= null loop
               pragma Assert (RI.Next = null
                               or else not (RI.Next.Element.all <
                                              RI.Element.all));

               if LI = null then
                  Splice_Internal (Target, null, Source);
                  exit;
               end if;

               pragma Assert (LI.Next = null
                               or else not (LI.Next.Element.all <
                                              LI.Element.all));

               if RI.Element.all < LI.Element.all then
                  RJ := RI;
                  RI := RI.Next;
                  Splice_Internal (Target, LI, Source, RJ);

               else
                  LI := LI.Next;
               end if;
            end loop;
         end;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out List) is
      begin
         if Container.Length <= 1 then
            return;
         end if;

         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);

         TC_Check (Container.TC);

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock : With_Lock (Container.TC'Unchecked_Access);

            package Descriptors is new List_Descriptors
              (Node_Ref => Node_Access, Nil => null);
            use Descriptors;

            function Next (N : Node_Access) return Node_Access is (N.Next);
            procedure Set_Next (N : Node_Access; Next : Node_Access)
              with Inline;
            procedure Set_Prev (N : Node_Access; Prev : Node_Access)
              with Inline;
            function "<" (L, R : Node_Access) return Boolean is
              (L.Element.all < R.Element.all);
            procedure Update_Container (List : List_Descriptor) with Inline;

            procedure Set_Next (N : Node_Access; Next : Node_Access) is
            begin
               N.Next := Next;
            end Set_Next;

            procedure Set_Prev (N : Node_Access; Prev : Node_Access) is
            begin
               N.Prev := Prev;
            end Set_Prev;

            procedure Update_Container (List : List_Descriptor) is
            begin
               Container.First  := List.First;
               Container.Last   := List.Last;
               Container.Length := List.Length;
            end Update_Container;

            procedure Sort_List is new Doubly_Linked_List_Sort;
         begin
            Sort_List (List_Descriptor'(First  => Container.First,
                                        Last   => Container.Last,
                                        Length => Container.Length));
         end;

         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);
      end Sort;

   end Generic_Sorting;

   ------------------------
   -- Get_Element_Access --
   ------------------------

   function Get_Element_Access
     (Position : Cursor) return not null Element_Access is
   begin
      return Position.Node.Element;
   end Get_Element_Access;

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
      First_Node : Node_Access;
      New_Node   : Node_Access;

   begin
      TC_Check (Container.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Before cursor designates wrong list";
         end if;

         if Checks and then
           (Before.Node = null or else Before.Node.Element = null)
         then
            raise Program_Error with
              "Before cursor has no element";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Checks and then Container.Length > Count_Type'Last - Count then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      declare
         --  The element allocator may need an accessibility check in the case
         --  the actual type is class-wide or has access discriminants (see
         --  RM 4.8(10.1) and AI12-0035). We don't unsuppress the check on the
         --  allocator in the loop below, because the one in this block would
         --  have failed already.

         pragma Unsuppress (Accessibility_Check);

         Element : Element_Access := new Element_Type'(New_Item);

      begin
         New_Node   := new Node_Type'(Element, null, null);
         First_Node := New_Node;

      exception
         when others =>
            Free (Element);
            raise;
      end;

      Insert_Internal (Container, Before.Node, New_Node);

      for J in 2 .. Count loop
         declare
            Element : Element_Access := new Element_Type'(New_Item);
         begin
            New_Node := new Node_Type'(Element, null, null);
         exception
            when others =>
               Free (Element);
               raise;
         end;

         Insert_Internal (Container, Before.Node, New_Node);
      end loop;

      Position := Cursor'(Container'Unchecked_Access, First_Node);
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
      Busy : With_Busy (Container.TC'Unrestricted_Access);
      Node : Node_Access := Container.First;

   begin
      while Node /= null loop
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Node := Node.Next;
      end loop;
   end Iterate;

   function Iterate
     (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'class
   is
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
                    Iterator'(Limited_Controlled with
                                Container => Container'Unrestricted_Access,
                                Node      => null)
      do
         Busy (Container.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   function Iterate
     (Container : List;
      Start     : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class
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
           "Start cursor of Iterate designates wrong list";
      end if;

      pragma Assert (Vet (Start), "Start cursor of Iterate is bad");

      --  The value of the Node component influences the behavior of the
      --  First and Last selector functions of the iterator object. When
      --  the Node component is non-null (as is the case here), it means
      --  that this is a partial iteration, over a subset of the complete
      --  sequence of items. The iterator object was constructed with
      --  a start expression, indicating the position from which the
      --  iteration begins. Note that the start position has the same value
      --  irrespective of whether this is a forward or reverse iteration.

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

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = null then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.Last);
      end if;
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

      if Object.Node = null then
         return Indefinite_Doubly_Linked_Lists.Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
   begin
      if Checks and then Container.Last = null then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.Last.Element.all;
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

   procedure Move (Target : in out List; Source : in out List) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      TC_Check (Source.TC);

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

      else
         pragma Assert (Vet (Position), "bad cursor in Next");

         declare
            Next_Node : constant Node_Access := Position.Node.Next;
         begin
            if Next_Node = null then
               return No_Element;
            else
               return Cursor'(Position.Container, Next_Node);
            end if;
         end;
      end if;
   end Next;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong list";
      end if;

      return Next (Position);
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

      else
         pragma Assert (Vet (Position), "bad cursor in Previous");

         declare
            Prev_Node : constant Node_Access := Position.Node.Prev;
         begin
            if Prev_Node = null then
               return No_Element;
            else
               return Cursor'(Position.Container, Prev_Node);
            end if;
         end;
      end if;
   end Previous;

   function Previous (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Checks and then Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Previous designates wrong list";
      end if;

      return Previous (Position);
   end Previous;

   ----------------------
   -- Pseudo_Reference --
   ----------------------

   function Pseudo_Reference
     (Container : aliased List'Class) return Reference_Control_Type
   is
      TC : constant Tamper_Counts_Access := Container.TC'Unrestricted_Access;
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
           "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         Lock : With_Lock (Position.Container.TC'Unrestricted_Access);
      begin
         Process (Position.Node.Element.all);
      end;
   end Query_Element;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : List)
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
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out List)
   is
      N   : Count_Type'Base;
      Dst : Node_Access;

   begin
      Clear (Item);

      Count_Type'Base'Read (Stream, N);

      if N = 0 then
         return;
      end if;

      declare
         Element : Element_Access :=
                     new Element_Type'(Element_Type'Input (Stream));
      begin
         Dst := new Node_Type'(Element, null, null);
      exception
         when others =>
            Free (Element);
            raise;
      end;

      Item.First := Dst;
      Item.Last := Dst;
      Item.Length := 1;

      while Item.Length < N loop
         declare
            Element : Element_Access :=
                        new Element_Type'(Element_Type'Input (Stream));
         begin
            Dst := new Node_Type'(Element, Next => null, Prev => Item.Last);
         exception
            when others =>
               Free (Element);
               raise;
         end;

         Item.Last.Next := Dst;
         Item.Last := Dst;
         Item.Length := Item.Length + 1;
      end loop;
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream list cursor";
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
     (Container : aliased in out List;
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

      if Checks and then Position.Node.Element = null then
         raise Program_Error with "Node has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in function Reference");

      declare
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => Position.Node.Element,
            Control => (Controlled with TC))
         do
            Busy (TC.all);
         end return;
      end;
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      TE_Check (Container.TC);

      if Checks and then Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unchecked_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      declare
         --  The element allocator may need an accessibility check in the
         --  case the actual type is class-wide or has access discriminants
         --  (see RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

         X : Element_Access := Position.Node.Element;

      begin
         Position.Node.Element := new Element_Type'(New_Item);
         Free (X);
      end;
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

      TC_Check (Container.TC);

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
         if Checks and then Node.Element = null then
            raise Program_Error with "Position cursor has no element";
         end if;

         if Checks and then Position.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Position cursor designates wrong container";
         end if;

         pragma Assert (Vet (Position), "bad cursor in Reverse_Find");
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock : With_Lock (Container.TC'Unrestricted_Access);
      begin
         while Node /= null loop
            if Node.Element.all = Item then
               return Cursor'(Container'Unrestricted_Access, Node);
            end if;

            Node := Node.Prev;
         end loop;

         return No_Element;
      end;
   end Reverse_Find;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Busy : With_Busy (Container.TC'Unrestricted_Access);
      Node : Node_Access := Container.Last;

   begin
      while Node /= null loop
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Node := Node.Prev;
      end loop;
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
      TC_Check (Target.TC);
      TC_Check (Source.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         if Checks and then
           (Before.Node = null or else Before.Node.Element = null)
         then
            raise Program_Error with
              "Before cursor has no element";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Splice");
      end if;

      if Target'Address = Source'Address or else Source.Length = 0 then
         return;
      end if;

      if Checks and then Target.Length > Count_Type'Last - Source.Length then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      Splice_Internal (Target, Before.Node, Source);
   end Splice;

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   is
   begin
      TC_Check (Container.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Container'Unchecked_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         if Checks and then
           (Before.Node = null or else Before.Node.Element = null)
         then
            raise Program_Error with
              "Before cursor has no element";
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Checks and then Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
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

      TC_Check (Target.TC);
      TC_Check (Source.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         if Checks and then
           (Before.Node = null or else Before.Node.Element = null)
         then
            raise Program_Error with
              "Before cursor has no element";
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Checks and then Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Source'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Checks and then Target.Length = Count_Type'Last then
         raise Constraint_Error with "Target is full";
      end if;

      Splice_Internal (Target, Before.Node, Source, Position.Node);
      Position.Container := Target'Unchecked_Access;
   end Splice;

   ---------------------
   -- Splice_Internal --
   ---------------------

   procedure Splice_Internal
     (Target : in out List;
      Before : Node_Access;
      Source : in out List)
   is
   begin
      --  This implements the corresponding Splice operation, after the
      --  parameters have been vetted, and corner-cases disposed of.

      pragma Assert (Target'Address /= Source'Address);
      pragma Assert (Source.Length > 0);
      pragma Assert (Source.First /= null);
      pragma Assert (Source.First.Prev = null);
      pragma Assert (Source.Last /= null);
      pragma Assert (Source.Last.Next = null);
      pragma Assert (Target.Length <= Count_Type'Last - Source.Length);

      if Target.Length = 0 then
         pragma Assert (Before = null);
         pragma Assert (Target.First = null);
         pragma Assert (Target.Last = null);

         Target.First := Source.First;
         Target.Last := Source.Last;

      elsif Before = null then
         pragma Assert (Target.Last.Next = null);

         Target.Last.Next := Source.First;
         Source.First.Prev := Target.Last;

         Target.Last := Source.Last;

      elsif Before = Target.First then
         pragma Assert (Target.First.Prev = null);

         Source.Last.Next := Target.First;
         Target.First.Prev := Source.Last;

         Target.First := Source.First;

      else
         pragma Assert (Target.Length >= 2);
         Before.Prev.Next := Source.First;
         Source.First.Prev := Before.Prev;

         Before.Prev := Source.Last;
         Source.Last.Next := Before;
      end if;

      Source.First := null;
      Source.Last := null;

      Target.Length := Target.Length + Source.Length;
      Source.Length := 0;
   end Splice_Internal;

   procedure Splice_Internal
     (Target   : in out List;
      Before   : Node_Access;  -- node of Target
      Source   : in out List;
      Position : Node_Access)  -- node of Source
   is
   begin
      --  This implements the corresponding Splice operation, after the
      --  parameters have been vetted.

      pragma Assert (Target'Address /= Source'Address);
      pragma Assert (Target.Length < Count_Type'Last);
      pragma Assert (Source.Length > 0);
      pragma Assert (Source.First /= null);
      pragma Assert (Source.First.Prev = null);
      pragma Assert (Source.Last /= null);
      pragma Assert (Source.Last.Next = null);
      pragma Assert (Position /= null);

      if Position = Source.First then
         Source.First := Position.Next;

         if Position = Source.Last then
            pragma Assert (Source.First = null);
            pragma Assert (Source.Length = 1);
            Source.Last := null;

         else
            Source.First.Prev := null;
         end if;

      elsif Position = Source.Last then
         pragma Assert (Source.Length >= 2);
         Source.Last := Position.Prev;
         Source.Last.Next := null;

      else
         pragma Assert (Source.Length >= 3);
         Position.Prev.Next := Position.Next;
         Position.Next.Prev := Position.Prev;
      end if;

      if Target.Length = 0 then
         pragma Assert (Before = null);
         pragma Assert (Target.First = null);
         pragma Assert (Target.Last = null);

         Target.First := Position;
         Target.Last := Position;

         Target.First.Prev := null;
         Target.Last.Next := null;

      elsif Before = null then
         pragma Assert (Target.Last.Next = null);
         Target.Last.Next := Position;
         Position.Prev := Target.Last;

         Target.Last := Position;
         Target.Last.Next := null;

      elsif Before = Target.First then
         pragma Assert (Target.First.Prev = null);
         Target.First.Prev := Position;
         Position.Next := Target.First;

         Target.First := Position;
         Target.First.Prev := null;

      else
         pragma Assert (Target.Length >= 2);
         Before.Prev.Next := Position;
         Position.Prev := Before.Prev;

         Before.Prev := Position;
         Position.Next := Before;
      end if;

      Target.Length := Target.Length + 1;
      Source.Length := Source.Length - 1;
   end Splice_Internal;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      TE_Check (Container.TC);

      if Checks and then I.Node = null then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if Checks and then J.Node = null then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if Checks and then I.Container /= Container'Unchecked_Access then
         raise Program_Error with "I cursor designates wrong container";
      end if;

      if Checks and then J.Container /= Container'Unchecked_Access then
         raise Program_Error with "J cursor designates wrong container";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      pragma Assert (Vet (I), "bad I cursor in Swap");
      pragma Assert (Vet (J), "bad J cursor in Swap");

      declare
         EI_Copy : constant Element_Access := I.Node.Element;

      begin
         I.Node.Element := J.Node.Element;
         J.Node.Element := EI_Copy;
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
      TC_Check (Container.TC);

      if Checks and then I.Node = null then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if Checks and then J.Node = null then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if Checks and then I.Container /= Container'Unrestricted_Access then
         raise Program_Error with "I cursor designates wrong container";
      end if;

      if Checks and then J.Container /= Container'Unrestricted_Access then
         raise Program_Error with "J cursor designates wrong container";
      end if;

      if I.Node = J.Node then
         return;
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

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
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
      if Checks and then Position.Node = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Node.Element = null then
         raise Program_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unchecked_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Update_Element");

      declare
         Lock : With_Lock (Container.TC'Unchecked_Access);
      begin
         Process (Position.Node.Element.all);
      end;
   end Update_Element;

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

      --  An invariant of a node is that its Previous and Next components can
      --  be null, or designate a different node. Also, its element access
      --  value must be non-null. Operation Free sets the node access value
      --  components of the node to designate the node itself, and the element
      --  access value to null, before actually deallocating the node, thus
      --  deliberately violating the node invariant. This gives us a simple way
      --  to detect a dangling reference to a node.

      if Position.Node.Next = Position.Node then
         return False;
      end if;

      if Position.Node.Prev = Position.Node then
         return False;
      end if;

      if Position.Node.Element = null then
         return False;
      end if;

      --  In practice the tests above will detect most instances of a dangling
      --  reference. If we get here, it means that the invariants of the
      --  designated node are satisfied (they at least appear to be satisfied),
      --  so we perform some more tests, to determine whether invariants of the
      --  designated list are satisfied too.

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

         if Position.Node.Prev = null and then Position.Node /= L.First then
            return False;
         end if;

         if Position.Node.Next = null and then Position.Node /= L.Last then
            return False;
         end if;

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

         if Position.Node = L.First then
            return True;
         end if;

         if Position.Node = L.Last then
            return True;
         end if;

         if Position.Node.Next = null then
            return False;
         end if;

         if Position.Node.Prev = null then
            return False;
         end if;

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
         Element_Type'Output (Stream, Node.Element.all);
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

end Ada.Containers.Indefinite_Doubly_Linked_Lists;
