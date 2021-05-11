------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               ADA.CONTAINERS.BOUNDED_DOUBLY_LINKED_LISTS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
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

with System; use type System.Address;
with System.Put_Images;

package body Ada.Containers.Bounded_Doubly_Linked_Lists with
  SPARK_Mode => Off
is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate
     (Container : in out List;
      New_Item  : Element_Type;
      New_Node  : out Count_Type);

   procedure Allocate
     (Container : in out List;
      Stream    : not null access Root_Stream_Type'Class;
      New_Node  : out Count_Type);

   procedure Free
     (Container : in out List;
      X         : Count_Type);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Count_Type;
      New_Node  : Count_Type);

   procedure Splice_Internal
     (Target : in out List;
      Before : Count_Type;
      Source : in out List);

   procedure Splice_Internal
     (Target  : in out List;
      Before  : Count_Type;
      Source  : in out List;
      Src_Pos : Count_Type;
      Tgt_Pos : out Count_Type);

   function Vet (Position : Cursor) return Boolean;
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

         LN : Node_Array renames Left.Nodes;
         RN : Node_Array renames Right.Nodes;

         LI : Count_Type := Left.First;
         RI : Count_Type := Right.First;
      begin
         for J in 1 .. Left.Length loop
            if LN (LI).Element /= RN (RI).Element then
               return False;
            end if;

            LI := LN (LI).Next;
            RI := RN (RI).Next;
         end loop;
      end;

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

         --  We always perform the assignment first, before we change container
         --  state, in order to defend against exceptions duration assignment.

         N (New_Node).Element := New_Item;
         Container.Free := N (New_Node).Next;

      else
         --  A negative free store value means that the links of the nodes in
         --  the free store have not been initialized. In this case, the nodes
         --  are physically contiguous in the array, starting at the index that
         --  is the absolute value of the Container.Free, and continuing until
         --  the end of the array (Nodes'Last).

         New_Node := abs Container.Free;

         --  As above, we perform this assignment first, before modifying any
         --  container state.

         N (New_Node).Element := New_Item;
         Container.Free := Container.Free - 1;
      end if;
   end Allocate;

   procedure Allocate
     (Container : in out List;
      Stream    : not null access Root_Stream_Type'Class;
      New_Node  : out Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Free >= 0 then
         New_Node := Container.Free;

         --  We always perform the assignment first, before we change container
         --  state, in order to defend against exceptions duration assignment.

         Element_Type'Read (Stream, N (New_Node).Element);
         Container.Free := N (New_Node).Next;

      else
         --  A negative free store value means that the links of the nodes in
         --  the free store have not been initialized. In this case, the nodes
         --  are physically contiguous in the array, starting at the index that
         --  is the absolute value of the Container.Free, and continuing until
         --  the end of the array (Nodes'Last).

         New_Node := abs Container.Free;

         --  As above, we perform this assignment first, before modifying any
         --  container state.

         Element_Type'Read (Stream, N (New_Node).Element);
         Container.Free := Container.Free - 1;
      end if;
   end Allocate;

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
      SN : Node_Array renames Source.Nodes;
      J  : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Checks and then Target.Capacity < Source.Length then
         raise Capacity_Error  -- ???
           with "Target capacity is less than Source length";
      end if;

      Target.Clear;

      J := Source.First;
      while J /= 0 loop
         Target.Append (SN (J).Element);
         J := SN (J).Next;
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
         pragma Assert (Container.TC = (Busy => 0, Lock => 0));
         return;
      end if;

      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

      TC_Check (Container.TC);

      while Container.Length > 1 loop
         X := Container.First;
         pragma Assert (N (N (X).Next).Prev = Container.First);

         Container.First := N (X).Next;
         N (Container.First).Prev := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;

      X := Container.First;
      pragma Assert (X = Container.Last);

      Container.First := 0;
      Container.Last := 0;
      Container.Length := 0;

      Free (Container, X);
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

      pragma Assert (Vet (Position), "bad cursor in Constant_Reference");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => N.Element'Access,
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

   function Copy (Source : List; Capacity : Count_Type := 0) return List is
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

      return Target : List (Capacity => C) do
         Assign (Target => Target, Source => Source);
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
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      TC_Check (Container.TC);

      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Delete");
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
      TC_Check (Container.TC);

      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
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
      TC_Check (Container.TC);

      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
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

   function Element (Position : Cursor) return Element_Type is
   begin
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Element");

      return Position.Container.Nodes (Position.Node).Element;
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty (Capacity : Count_Type := 10) return List is
   begin
      return Result : List (Capacity) do
         null;
      end return;
   end Empty;

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
      Nodes : Node_Array renames Container.Nodes;
      Node  : Count_Type := Position.Node;

   begin
      if Node = 0 then
         Node := Container.First;

      else
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
         while Node /= 0 loop
            if Nodes (Node).Element = Item then
               return Cursor'(Container'Unrestricted_Access, Node);
            end if;

            Node := Nodes (Node).Next;
         end loop;

         return No_Element;
      end;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.First);
      end if;
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Node component is 0, this means the iterator object was
      --  constructed without a start expression, in which case the (forward)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  of items (corresponding to Container.First, for a forward iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is positive, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (forward) partial iteration begins.

      if Object.Node = 0 then
         return Bounded_Doubly_Linked_Lists.First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
   begin
      if Checks and then Container.First = 0 then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.Nodes (Container.First).Element;
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
      pragma Assert (N (X).Prev >= 0);  -- node is active

   begin
      --  The list container actually contains two lists: one for the "active"
      --  nodes that contain elements that have been inserted onto the list,
      --  and another for the "inactive" nodes for the free store.

      --  We desire that merely declaring an object should have only minimal
      --  cost; specially, we want to avoid having to initialize the free
      --  store (to fill in the links), especially if the capacity is large.

      --  The head of the free list is indicated by Container.Free. If its
      --  value is non-negative, then the free store has been initialized in
      --  the "normal" way: Container.Free points to the head of the list of
      --  free (inactive) nodes, and the value 0 means the free list is empty.
      --  Each node on the free list has been initialized to point to the next
      --  free node (via its Next component), and the value 0 means that this
      --  is the last free node.

      --  If Container.Free is negative, then the links on the free store have
      --  not been initialized. In this case the link values are implied: the
      --  free store comprises the components of the node array started with
      --  the absolute value of Container.Free, and continuing until the end of
      --  the array (Nodes'Last).

      --  If the list container is manipulated on one end only (for example if
      --  the container were being used as a stack), then there is no need to
      --  initialize the free store, since the inactive nodes are physically
      --  contiguous (in fact, they lie immediately beyond the logical end
      --  being manipulated). The only time we need to actually initialize the
      --  nodes in the free store is if the node that becomes inactive is not
      --  at the end of the list. The free store would then be discontiguous
      --  and so its nodes would need to be linked in the traditional way.

      --  ???
      --  It might be possible to perform an optimization here. Suppose that
      --  the free store can be represented as having two parts: one comprising
      --  the non-contiguous inactive nodes linked together in the normal way,
      --  and the other comprising the contiguous inactive nodes (that are not
      --  linked together, at the end of the nodes array). This would allow us
      --  to never have to initialize the free store, except in a lazy way as
      --  nodes become inactive.

      --  When an element is deleted from the list container, its node becomes
      --  inactive, and so we set its Prev component to a negative value, to
      --  indicate that it is now inactive. This provides a useful way to
      --  detect a dangling cursor reference (and which is used in Vet).

      N (X).Prev := -1;  -- Node is deallocated (not on active list)

      if Container.Free >= 0 then

         --  The free store has previously been initialized. All we need to
         --  do here is link the newly-free'd node onto the free list.

         N (X).Next := Container.Free;
         Container.Free := X;

      elsif X + 1 = abs Container.Free then

         --  The free store has not been initialized, and the node becoming
         --  inactive immediately precedes the start of the free store. All
         --  we need to do is move the start of the free store back by one.

         --  Note: initializing Next to zero is not strictly necessary but
         --  seems cleaner and marginally safer.

         N (X).Next := 0;
         Container.Free := Container.Free + 1;

      else
         --  The free store has not been initialized, and the node becoming
         --  inactive does not immediately precede the free store. Here we
         --  first initialize the free store (meaning the links are given
         --  values in the traditional way), and then link the newly-free'd
         --  node onto the head of the free store.

         --  ???
         --  See the comments above for an optimization opportunity. If the
         --  next link for a node on the free store is negative, then this
         --  means the remaining nodes on the free store are physically
         --  contiguous, starting as the absolute value of that index value.

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
         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         Lock : With_Lock (Container.TC'Unrestricted_Access);

         Nodes : Node_Array renames Container.Nodes;
         Node  : Count_Type;
      begin
         Node := Container.First;
         for J in 2 .. Container.Length loop
            if Nodes (Nodes (Node).Next).Element < Nodes (Node).Element then
               return False;
            end if;

            Node := Nodes (Node).Next;
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
         TC_Check (Target.TC);
         TC_Check (Source.TC);

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

         if Checks and then Target'Address = Source'Address then
            raise Program_Error with
              "Target and Source denote same non-empty container";
         end if;

         if Checks and then Target.Length > Count_Type'Last - Source.Length
         then
            raise Constraint_Error with "new length exceeds maximum";
         end if;

         if Checks and then Target.Length + Source.Length > Target.Capacity
         then
            raise Capacity_Error with "new length exceeds target capacity";
         end if;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock_Target : With_Lock (Target.TC'Unchecked_Access);
            Lock_Source : With_Lock (Source.TC'Unchecked_Access);

            LN : Node_Array renames Target.Nodes;
            RN : Node_Array renames Source.Nodes;

            LI, LJ, RI, RJ : Count_Type;

         begin
            LI := Target.First;
            RI := Source.First;
            while RI /= 0 loop
               pragma Assert (RN (RI).Next = 0
                                or else not (RN (RN (RI).Next).Element <
                                               RN (RI).Element));

               if LI = 0 then
                  Splice_Internal (Target, 0, Source);
                  exit;
               end if;

               pragma Assert (LN (LI).Next = 0
                                or else not (LN (LN (LI).Next).Element <
                                               LN (LI).Element));

               if RN (RI).Element < LN (LI).Element then
                  RJ := RI;
                  RI := RN (RI).Next;
                  Splice_Internal (Target, LI, Source, RJ, LJ);

               else
                  LI := LN (LI).Next;
               end if;
            end loop;
         end;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out List) is
         N : Node_Array renames Container.Nodes;

         procedure Partition (Pivot, Back : Count_Type);
         --  What does this do ???

         procedure Sort (Front, Back : Count_Type);
         --  Internal procedure, what does it do??? rename it???

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
            Pivot : constant Count_Type :=
              (if Front = 0 then Container.First else N (Front).Next);
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

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);

         TC_Check (Container.TC);

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock : With_Lock (Container.TC'Unchecked_Access);
         begin
            Sort (Front => 0, Back => 0);
         end;

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);
      end Sort;

   end Generic_Sorting;

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
      First_Node : Count_Type;
      New_Node   : Count_Type;

   begin
      TC_Check (Container.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Container'Unrestricted_Access
         then
            raise Program_Error with
              "Before cursor designates wrong list";
         end if;

         pragma Assert (Vet (Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Checks and then Container.Length > Container.Capacity - Count then
         raise Capacity_Error with "capacity exceeded";
      end if;

      Allocate (Container, New_Item, New_Node);
      First_Node := New_Node;
      Insert_Internal (Container, Before.Node, New_Node);

      for Index in Count_Type'(2) .. Count loop
         Allocate (Container, New_Item, New_Node);
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
      pragma Warnings (Off);
      Default_Initialized_Item : Element_Type;
      pragma Unmodified (Default_Initialized_Item);
      --  OK to reference, see below. Note that we need to suppress both the
      --  front end warning and the back end warning. In addition, pragma
      --  Unmodified is needed to suppress the warning ``actual type for
      --  "Element_Type" should be fully initialized type'' on certain
      --  instantiations.

   begin
      --  There is no explicit element provided, but in an instance the element
      --  type may be a scalar with a Default_Value aspect, or a composite
      --  type with such a scalar component, or components with default
      --  initialization, so insert the specified number of possibly
      --  initialized elements at the given position.

      Insert (Container, Before, Default_Initialized_Item, Position, Count);
      pragma Warnings (On);
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
         N (Container.First).Prev := 0;

         Container.Last := New_Node;
         N (Container.Last).Next := 0;

      --  Before = zero means append

      elsif Before = 0 then
         pragma Assert (N (Container.Last).Next = 0);

         N (Container.Last).Next := New_Node;
         N (New_Node).Prev := Container.Last;

         Container.Last := New_Node;
         N (Container.Last).Next := 0;

      --  Before = Container.First means prepend

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
      Busy : With_Busy (Container.TC'Unrestricted_Access);
      Node : Count_Type := Container.First;

   begin
      while Node /= 0 loop
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Node := Container.Nodes (Node).Next;
      end loop;
   end Iterate;

   function Iterate
     (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is 0 (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a
      --  complete iterator, meaning that the iteration starts from the
      --  (logical) beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
                    Iterator'(Limited_Controlled with
                                Container => Container'Unrestricted_Access,
                                Node      => 0)
      do
         Busy (Container.TC'Unrestricted_Access.all);
      end return;
   end Iterate;

   function Iterate
     (Container : List;
      Start     : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'class
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

      --  The value of the Node component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Node
      --  component is positive (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.

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
      if Container.Last = 0 then
         return No_Element;
      else
         return Cursor'(Container'Unrestricted_Access, Container.Last);
      end if;
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Node component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Node component is 0, this means the iterator object was
      --  constructed without a start expression, in which case the (reverse)
      --  iteration starts from the (logical) beginning of the entire sequence
      --  (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items. When
      --  the Node component is positive, the iterator object was constructed
      --  with a start expression, that specifies the position from which the
      --  (reverse) partial iteration begins.

      if Object.Node = 0 then
         return Bounded_Doubly_Linked_Lists.Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Node);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
   begin
      if Checks and then Container.Last = 0 then
         raise Constraint_Error with "list is empty";
      end if;

      return Container.Nodes (Container.Last).Element;
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
      N : Node_Array renames Source.Nodes;
      X : Count_Type;

   begin
      TC_Check (Source.TC);

      if Target'Address = Source'Address then
         return;
      end if;

      if Checks and then Target.Capacity < Source.Length then
         raise Capacity_Error with "Source length exceeds Target capacity";
      end if;

      --  Clear target, note that this checks busy bits of Target

      Clear (Target);

      while Source.Length > 1 loop
         pragma Assert (Source.First in 1 .. Source.Capacity);
         pragma Assert (Source.Last /= Source.First);
         pragma Assert (N (Source.First).Prev = 0);
         pragma Assert (N (Source.Last).Next = 0);

         --  Copy first element from Source to Target

         X := Source.First;
         Append (Target, N (X).Element);

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
         else
            return Cursor'(Position.Container, Node);
         end if;
      end;
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
         else
            return Cursor'(Position.Container, Node);
         end if;
      end;
   end Previous;

   function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
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
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with
           "Position cursor has no element";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Query_Element");

      declare
         Lock : With_Lock (Position.Container.TC'Unrestricted_Access);
         C : List renames Position.Container.all'Unrestricted_Access.all;
         N : Node_Type renames C.Nodes (Position.Node);
      begin
         Process (N.Element);
      end;
   end Query_Element;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (S : in out Ada.Strings.Text_Output.Sink'Class; V : List)
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
      N : Count_Type'Base;
      X : Count_Type;

   begin
      Clear (Item);
      Count_Type'Base'Read (Stream, N);

      if Checks and then N < 0 then
         raise Program_Error with "bad list length (corrupt stream)";
      end if;

      if N = 0 then
         return;
      end if;

      if Checks and then N > Item.Capacity then
         raise Constraint_Error with "length exceeds capacity";
      end if;

      for Idx in 1 .. N loop
         Allocate (Item, Stream, New_Node => X);
         Insert_Internal (Item, Before => 0, New_Node => X);
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

      pragma Assert (Vet (Position), "bad cursor in function Reference");

      declare
         N : Node_Type renames Container.Nodes (Position.Node);
         TC : constant Tamper_Counts_Access :=
           Container.TC'Unrestricted_Access;
      begin
         return R : constant Reference_Type :=
           (Element => N.Element'Access,
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

      pragma Assert (Vet (Position), "bad cursor in Replace_Element");

      Container.Nodes (Position.Node).Element := New_Item;
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

      TC_Check (Container.TC);

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
      Node : Count_Type := Position.Node;

   begin
      if Node = 0 then
         Node := Container.Last;

      else
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
         while Node /= 0 loop
            if Container.Nodes (Node).Element = Item then
               return Cursor'(Container'Unrestricted_Access, Node);
            end if;

            Node := Container.Nodes (Node).Prev;
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
      Node : Count_Type := Container.Last;

   begin
      while Node /= 0 loop
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Node := Container.Nodes (Node).Prev;
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

         pragma Assert (Vet (Before), "bad cursor in Splice");
      end if;

      if Target'Address = Source'Address or else Source.Length = 0 then
         return;
      end if;

      if Checks and then Target.Length > Count_Type'Last - Source.Length then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Checks and then Target.Length + Source.Length > Target.Capacity then
         raise Capacity_Error with "new length exceeds target capacity";
      end if;

      Splice_Internal (Target, Before.Node, Source);
   end Splice;

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   is
      N : Node_Array renames Container.Nodes;

   begin
      TC_Check (Container.TC);

      if Before.Container /= null then
         if Checks and then Before.Container /= Container'Unchecked_Access then
            raise Program_Error with
              "Before cursor designates wrong container";
         end if;

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Position.Node = Before.Node
        or else N (Position.Node).Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

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

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor)
   is
      Target_Position : Count_Type;

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

         pragma Assert (Vet (Before), "bad Before cursor in Splice");
      end if;

      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Source'Unrestricted_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad Position cursor in Splice");

      if Checks and then Target.Length >= Target.Capacity then
         raise Capacity_Error with "Target is full";
      end if;

      Splice_Internal
        (Target  => Target,
         Before  => Before.Node,
         Source  => Source,
         Src_Pos => Position.Node,
         Tgt_Pos => Target_Position);

      Position := Cursor'(Target'Unrestricted_Access, Target_Position);
   end Splice;

   ---------------------
   -- Splice_Internal --
   ---------------------

   procedure Splice_Internal
     (Target : in out List;
      Before : Count_Type;
      Source : in out List)
   is
      N : Node_Array renames Source.Nodes;
      X : Count_Type;

   begin
      --  This implements the corresponding Splice operation, after the
      --  parameters have been vetted, and corner-cases disposed of.

      pragma Assert (Target'Address /= Source'Address);
      pragma Assert (Source.Length > 0);
      pragma Assert (Source.First /= 0);
      pragma Assert (N (Source.First).Prev = 0);
      pragma Assert (Source.Last /= 0);
      pragma Assert (N (Source.Last).Next = 0);
      pragma Assert (Target.Length <= Count_Type'Last - Source.Length);
      pragma Assert (Target.Length + Source.Length <= Target.Capacity);

      while Source.Length > 1 loop
         --  Copy first element of Source onto Target

         Allocate (Target, N (Source.First).Element, New_Node => X);
         Insert_Internal (Target, Before => Before, New_Node => X);

         --  Unlink the first node from Source

         X := Source.First;
         pragma Assert (N (N (X).Next).Prev = X);

         Source.First := N (X).Next;
         N (Source.First).Prev := 0;

         Source.Length := Source.Length - 1;

         --  Return the Source node to its free store

         Free (Source, X);
      end loop;

      --  Copy first (and only remaining) element of Source onto Target

      Allocate (Target, N (Source.First).Element, New_Node => X);
      Insert_Internal (Target, Before => Before, New_Node => X);

      --  Unlink the node from Source

      X := Source.First;
      pragma Assert (X = Source.Last);

      Source.First := 0;
      Source.Last := 0;

      Source.Length := 0;

      --  Return the Source node to its free store

      Free (Source, X);
   end Splice_Internal;

   procedure Splice_Internal
     (Target  : in out List;
      Before  : Count_Type;  -- node of Target
      Source  : in out List;
      Src_Pos : Count_Type;  -- node of Source
      Tgt_Pos : out Count_Type)
   is
      N : Node_Array renames Source.Nodes;

   begin
      --  This implements the corresponding Splice operation, after the
      --  parameters have been vetted, and corner-cases handled.

      pragma Assert (Target'Address /= Source'Address);
      pragma Assert (Target.Length < Target.Capacity);
      pragma Assert (Source.Length > 0);
      pragma Assert (Source.First /= 0);
      pragma Assert (N (Source.First).Prev = 0);
      pragma Assert (Source.Last /= 0);
      pragma Assert (N (Source.Last).Next = 0);
      pragma Assert (Src_Pos /= 0);

      Allocate (Target, N (Src_Pos).Element, New_Node => Tgt_Pos);
      Insert_Internal (Target, Before => Before, New_Node => Tgt_Pos);

      if Source.Length = 1 then
         pragma Assert (Source.First = Source.Last);
         pragma Assert (Src_Pos = Source.First);

         Source.First := 0;
         Source.Last := 0;

      elsif Src_Pos = Source.First then
         pragma Assert (N (N (Src_Pos).Next).Prev = Src_Pos);

         Source.First := N (Src_Pos).Next;
         N (Source.First).Prev := 0;

      elsif Src_Pos = Source.Last then
         pragma Assert (N (N (Src_Pos).Prev).Next = Src_Pos);

         Source.Last := N (Src_Pos).Prev;
         N (Source.Last).Next := 0;

      else
         pragma Assert (Source.Length >= 3);
         pragma Assert (N (N (Src_Pos).Next).Prev = Src_Pos);
         pragma Assert (N (N (Src_Pos).Prev).Next = Src_Pos);

         N (N (Src_Pos).Next).Prev := N (Src_Pos).Prev;
         N (N (Src_Pos).Prev).Next := N (Src_Pos).Next;
      end if;

      Source.Length := Source.Length - 1;
      Free (Source, Src_Pos);
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

      if Checks and then I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if Checks and then J.Node = 0 then
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
         EI : Element_Type renames Container.Nodes (I.Node).Element;
         EJ : Element_Type renames Container.Nodes (J.Node).Element;

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
      TC_Check (Container.TC);

      if Checks and then I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if Checks and then J.Node = 0 then
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
      if Checks and then Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Checks and then Position.Container /= Container'Unchecked_Access then
         raise Program_Error with
           "Position cursor designates wrong container";
      end if;

      pragma Assert (Vet (Position), "bad cursor in Update_Element");

      declare
         Lock : With_Lock (Container.TC'Unchecked_Access);
         N : Node_Type renames Container.Nodes (Position.Node);
      begin
         Process (N.Element);
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

         if L.First = 0 or L.First > L.Capacity then
            return False;
         end if;

         if L.Last = 0 or L.Last > L.Capacity then
            return False;
         end if;

         if N (L.First).Prev /= 0 then
            return False;
         end if;

         if N (L.Last).Next /= 0 then
            return False;
         end if;

         if Position.Node > L.Capacity then
            return False;
         end if;

         --  An invariant of an active node is that its Previous and Next
         --  components are non-negative. Operation Free sets the Previous
         --  component of the node to the value -1 before actually deallocating
         --  the node, to mark the node as inactive. (By "dellocating" we mean
         --  only that the node is linked onto a list of inactive nodes used
         --  for storage.) This marker gives us a simple way to detect a
         --  dangling reference to a node.

         if N (Position.Node).Prev < 0 then  -- see Free
            return False;
         end if;

         if N (Position.Node).Prev > L.Capacity then
            return False;
         end if;

         if N (Position.Node).Next = Position.Node then
            return False;
         end if;

         if N (Position.Node).Prev = Position.Node then
            return False;
         end if;

         if N (Position.Node).Prev = 0
           and then Position.Node /= L.First
         then
            return False;
         end if;

         pragma Assert (N (Position.Node).Prev /= 0
                          or else Position.Node = L.First);

         if N (Position.Node).Next = 0
           and then Position.Node /= L.Last
         then
            return False;
         end if;

         pragma Assert (N (Position.Node).Next /= 0
                          or else Position.Node = L.Last);

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

         --  Eliminate earlier possibility

         if Position.Node = L.First then
            return True;
         end if;

         pragma Assert (N (Position.Node).Prev /= 0);

         --  Eliminate another possibility

         if Position.Node = L.Last then
            return True;
         end if;

         pragma Assert (N (Position.Node).Next /= 0);

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

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : List)
   is
      Node : Count_Type;

   begin
      Count_Type'Base'Write (Stream, Item.Length);

      Node := Item.First;
      while Node /= 0 loop
         Element_Type'Write (Stream, Item.Nodes (Node).Element);
         Node := Item.Nodes (Node).Next;
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

end Ada.Containers.Bounded_Doubly_Linked_Lists;
