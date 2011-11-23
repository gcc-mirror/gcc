------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                   ADA.CONTAINERS.BOUNDED_MULTIWAY_TREES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

with Ada.Finalization; use Ada.Finalization;
with System; use type System.Address;

package body Ada.Containers.Bounded_Multiway_Trees is

   No_Node : constant Count_Type'Base := -1;

   type Iterator is new Limited_Controlled and
     Tree_Iterator_Interfaces.Forward_Iterator with
   record
      Container : Tree_Access;
      Position  : Cursor;
      From_Root : Boolean;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object : Iterator;
      Position : Cursor) return Cursor;

   type Child_Iterator is new Limited_Controlled and
      Tree_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Tree_Access;
      Position  : Cursor;
   end record;

   overriding procedure Finalize (Object : in out Child_Iterator);

   overriding function First (Object : Child_Iterator) return Cursor;

   overriding function Next
     (Object   : Child_Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Child_Iterator;
      Position : Cursor) return Cursor;

   overriding function Last (Object : Child_Iterator) return Cursor;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize_Node (Container : in out Tree; Index : Count_Type);
   procedure Initialize_Root (Container : in out Tree);

   procedure Allocate_Node
     (Container          : in out Tree;
      Initialize_Element : not null access procedure (Index : Count_Type);
      New_Node           : out Count_Type);

   procedure Allocate_Node
     (Container : in out Tree;
      New_Item  : Element_Type;
      New_Node  : out Count_Type);

   procedure Allocate_Node
     (Container : in out Tree;
      New_Node  : out Count_Type);

   procedure Allocate_Node
     (Container : in out Tree;
      Stream    : not null access Root_Stream_Type'Class;
      New_Node  : out Count_Type);

   procedure Deallocate_Node
     (Container : in out Tree;
      X         : Count_Type);

   procedure Deallocate_Children
     (Container : in out Tree;
      Subtree   : Count_Type;
      Count     : in out Count_Type);

   procedure Deallocate_Subtree
     (Container : in out Tree;
      Subtree   : Count_Type;
      Count     : in out Count_Type);

   function Equal_Children
     (Left_Tree     : Tree;
      Left_Subtree  : Count_Type;
      Right_Tree    : Tree;
      Right_Subtree : Count_Type) return Boolean;

   function Equal_Subtree
     (Left_Tree     : Tree;
      Left_Subtree  : Count_Type;
      Right_Tree    : Tree;
      Right_Subtree : Count_Type) return Boolean;

   procedure Iterate_Children
     (Container : Tree;
      Subtree   : Count_Type;
      Process   : not null access procedure (Position : Cursor));

   procedure Iterate_Subtree
     (Container : Tree;
      Subtree   : Count_Type;
      Process   : not null access procedure (Position : Cursor));

   procedure Copy_Children
     (Source        : Tree;
      Source_Parent : Count_Type;
      Target        : in out Tree;
      Target_Parent : Count_Type;
      Count         : in out Count_Type);

   procedure Copy_Subtree
     (Source         : Tree;
      Source_Subtree : Count_Type;
      Target         : in out Tree;
      Target_Parent  : Count_Type;
      Target_Subtree : out Count_Type;
      Count          : in out Count_Type);

   function Find_In_Children
     (Container : Tree;
      Subtree   : Count_Type;
      Item      : Element_Type) return Count_Type;

   function Find_In_Subtree
     (Container : Tree;
      Subtree   : Count_Type;
      Item      : Element_Type) return Count_Type;

   function Child_Count
     (Container : Tree;
      Parent    : Count_Type) return Count_Type;

   function Subtree_Node_Count
     (Container : Tree;
      Subtree   : Count_Type) return Count_Type;

   function Is_Reachable
     (Container : Tree;
      From, To  : Count_Type) return Boolean;

   function Root_Node (Container : Tree) return Count_Type;

   procedure Remove_Subtree
     (Container : in out Tree;
      Subtree   : Count_Type);

   procedure Insert_Subtree_Node
     (Container : in out Tree;
      Subtree   : Count_Type'Base;
      Parent    : Count_Type;
      Before    : Count_Type'Base);

   procedure Insert_Subtree_List
     (Container : in out Tree;
      First     : Count_Type'Base;
      Last      : Count_Type'Base;
      Parent    : Count_Type;
      Before    : Count_Type'Base);

   procedure Splice_Children
     (Container     : in out Tree;
      Target_Parent : Count_Type;
      Before        : Count_Type'Base;
      Source_Parent : Count_Type);

   procedure Splice_Children
     (Target        : in out Tree;
      Target_Parent : Count_Type;
      Before        : Count_Type'Base;
      Source        : in out Tree;
      Source_Parent : Count_Type);

   procedure Splice_Subtree
     (Target   : in out Tree;
      Parent   : Count_Type;
      Before   : Count_Type'Base;
      Source   : in out Tree;
      Position : in out Count_Type);  -- source on input, target on output

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Tree) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Count /= Right.Count then
         return False;
      end if;

      if Left.Count = 0 then
         return True;
      end if;

      return Equal_Children
               (Left_Tree     => Left,
                Left_Subtree  => Root_Node (Left),
                Right_Tree    => Right,
                Right_Subtree => Root_Node (Right));
   end "=";

   -------------------
   -- Allocate_Node --
   -------------------

   procedure Allocate_Node
     (Container          : in out Tree;
      Initialize_Element : not null access procedure (Index : Count_Type);
      New_Node           : out Count_Type)
   is
   begin
      if Container.Free >= 0 then
         New_Node := Container.Free;
         pragma Assert (New_Node in Container.Elements'Range);

         --  We always perform the assignment first, before we change container
         --  state, in order to defend against exceptions duration assignment.

         Initialize_Element (New_Node);

         Container.Free := Container.Nodes (New_Node).Next;

      else
         --  A negative free store value means that the links of the nodes in
         --  the free store have not been initialized. In this case, the nodes
         --  are physically contiguous in the array, starting at the index that
         --  is the absolute value of the Container.Free, and continuing until
         --  the end of the array (Nodes'Last).

         New_Node := abs Container.Free;
         pragma Assert (New_Node in Container.Elements'Range);

         --  As above, we perform this assignment first, before modifying any
         --  container state.

         Initialize_Element (New_Node);

         Container.Free := Container.Free - 1;

         if abs Container.Free > Container.Capacity then
            Container.Free := 0;
         end if;
      end if;

      Initialize_Node (Container, New_Node);
   end Allocate_Node;

   procedure Allocate_Node
     (Container : in out Tree;
      New_Item  : Element_Type;
      New_Node  : out Count_Type)
   is
      procedure Initialize_Element (Index : Count_Type);

      procedure Initialize_Element (Index : Count_Type) is
      begin
         Container.Elements (Index) := New_Item;
      end Initialize_Element;

   begin
      Allocate_Node (Container, Initialize_Element'Access, New_Node);
   end Allocate_Node;

   procedure Allocate_Node
     (Container : in out Tree;
      Stream    : not null access Root_Stream_Type'Class;
      New_Node  : out Count_Type)
   is
      procedure Initialize_Element (Index : Count_Type);

      procedure Initialize_Element (Index : Count_Type) is
      begin
         Element_Type'Read (Stream, Container.Elements (Index));
      end Initialize_Element;

   begin
      Allocate_Node (Container, Initialize_Element'Access, New_Node);
   end Allocate_Node;

   procedure Allocate_Node
     (Container : in out Tree;
      New_Node  : out Count_Type)
   is
      procedure Initialize_Element (Index : Count_Type) is null;
   begin
      Allocate_Node (Container, Initialize_Element'Access, New_Node);
   end Allocate_Node;

   -------------------
   -- Ancestor_Find --
   -------------------

   function Ancestor_Find
     (Position : Cursor;
      Item     : Element_Type) return Cursor
   is
      R, N : Count_Type;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      --  Commented-out pending ruling by ARG.  ???

      --  if Position.Container /= Container'Unrestricted_Access then
      --     raise Program_Error with "Position cursor not in container";
      --  end if;

      --  AI-0136 says to raise PE if Position equals the root node. This does
      --  not seem correct, as this value is just the limiting condition of the
      --  search. For now we omit this check, pending a ruling from the ARG.
      --  ???
      --
      --  if Is_Root (Position) then
      --     raise Program_Error with "Position cursor designates root";
      --  end if;

      R := Root_Node (Position.Container.all);
      N := Position.Node;
      while N /= R loop
         if Position.Container.Elements (N) = Item then
            return Cursor'(Position.Container, N);
         end if;

         N := Position.Container.Nodes (N).Parent;
      end loop;

      return No_Element;
   end Ancestor_Find;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Container : in out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Nodes       : Tree_Node_Array renames Container.Nodes;
      First, Last : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Count = 0 then
         return;
      end if;

      if Container.Count > Container.Capacity - Count then
         raise Constraint_Error
           with "requested count exceeds available storage";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container.Count = 0 then
         Initialize_Root (Container);
      end if;

      Allocate_Node (Container, New_Item, First);
      Nodes (First).Parent := Parent.Node;

      Last := First;
      for J in Count_Type'(2) .. Count loop
         Allocate_Node (Container, New_Item, Nodes (Last).Next);
         Nodes (Nodes (Last).Next).Parent := Parent.Node;
         Nodes (Nodes (Last).Next).Prev := Last;

         Last := Nodes (Last).Next;
      end loop;

      Insert_Subtree_List
        (Container => Container,
         First     => First,
         Last      => Last,
         Parent    => Parent.Node,
         Before    => No_Node);  -- means "insert at end of list"

      Container.Count := Container.Count + Count;
   end Append_Child;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Tree; Source : Tree) is
      Target_Count : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Count then
         raise Capacity_Error  -- ???
           with "Target capacity is less than Source count";
      end if;

      Target.Clear;  -- Checks busy bit

      if Source.Count = 0 then
         return;
      end if;

      Initialize_Root (Target);

      --  Copy_Children returns the number of nodes that it allocates, but it
      --  does this by incrementing the count value passed in, so we must
      --  initialize the count before calling Copy_Children.

      Target_Count := 0;

      Copy_Children
        (Source        => Source,
         Source_Parent => Root_Node (Source),
         Target        => Target,
         Target_Parent => Root_Node (Target),
         Count         => Target_Count);

      pragma Assert (Target_Count = Source.Count);
      Target.Count := Source.Count;
   end Assign;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Parent : Cursor) return Count_Type is
   begin
      if Parent = No_Element then
         return 0;

      elsif Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return 0;

      else
         return Child_Count (Parent.Container.all, Parent.Node);
      end if;
   end Child_Count;

   function Child_Count
     (Container : Tree;
      Parent    : Count_Type) return Count_Type
   is
      NN : Tree_Node_Array renames Container.Nodes;
      CC : Children_Type renames NN (Parent).Children;

      Result : Count_Type;
      Node   : Count_Type'Base;

   begin
      Result := 0;
      Node := CC.First;
      while Node > 0 loop
         Result := Result + 1;
         Node := NN (Node).Next;
      end loop;

      return Result;
   end Child_Count;

   -----------------
   -- Child_Depth --
   -----------------

   function Child_Depth (Parent, Child : Cursor) return Count_Type is
      Result : Count_Type;
      N      : Count_Type'Base;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Child = No_Element then
         raise Constraint_Error with "Child cursor has no element";
      end if;

      if Parent.Container /= Child.Container then
         raise Program_Error with "Parent and Child in different containers";
      end if;

      if Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         pragma Assert (Child = Parent);
         return 0;
      end if;

      Result := 0;
      N := Child.Node;
      while N /= Parent.Node loop
         Result := Result + 1;
         N := Parent.Container.Nodes (N).Parent;

         if N < 0 then
            raise Program_Error with "Parent is not ancestor of Child";
         end if;
      end loop;

      return Result;
   end Child_Depth;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Tree) is
      Container_Count : constant Count_Type := Container.Count;
      Count           : Count_Type;

   begin
      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container_Count = 0 then
         return;
      end if;

      Container.Count := 0;

      --  Deallocate_Children returns the number of nodes that it deallocates,
      --  but it does this by incrementing the count value that is passed in,
      --  so we must first initialize the count return value before calling it.

      Count := 0;

      Deallocate_Children
        (Container => Container,
         Subtree   => Root_Node (Container),
         Count     => Count);

      pragma Assert (Count = Container_Count);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Tree;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : Tree;
      Capacity : Count_Type := 0) return Tree
   is
      C : Count_Type;

   begin
      if Capacity = 0 then
         C := Source.Count;
      elsif Capacity >= Source.Count then
         C := Capacity;
      else
         raise Capacity_Error with "Capacity value too small";
      end if;

      return Target : Tree (Capacity => C) do
         Initialize_Root (Target);

         if Source.Count = 0 then
            return;
         end if;

         Copy_Children
           (Source        => Source,
            Source_Parent => Root_Node (Source),
            Target        => Target,
            Target_Parent => Root_Node (Target),
            Count         => Target.Count);

         pragma Assert (Target.Count = Source.Count);
      end return;
   end Copy;

   -------------------
   -- Copy_Children --
   -------------------

   procedure Copy_Children
     (Source        : Tree;
      Source_Parent : Count_Type;
      Target        : in out Tree;
      Target_Parent : Count_Type;
      Count         : in out Count_Type)
   is
      S_Nodes : Tree_Node_Array renames Source.Nodes;
      S_Node  : Tree_Node_Type renames S_Nodes (Source_Parent);

      T_Nodes : Tree_Node_Array renames Target.Nodes;
      T_Node  : Tree_Node_Type renames T_Nodes (Target_Parent);

      pragma Assert (T_Node.Children.First <= 0);
      pragma Assert (T_Node.Children.Last <= 0);

      T_CC : Children_Type;
      C    : Count_Type'Base;

   begin
      --  We special-case the first allocation, in order to establish the
      --  representation invariants for type Children_Type.

      C := S_Node.Children.First;

      if C <= 0 then  -- source parent has no children
         return;
      end if;

      Copy_Subtree
        (Source         => Source,
         Source_Subtree => C,
         Target         => Target,
         Target_Parent  => Target_Parent,
         Target_Subtree => T_CC.First,
         Count          => Count);

      T_CC.Last := T_CC.First;

      --  The representation invariants for the Children_Type list have been
      --  established, so we can now copy the remaining children of Source.

      C := S_Nodes (C).Next;
      while C > 0 loop
         Copy_Subtree
           (Source         => Source,
            Source_Subtree => C,
            Target         => Target,
            Target_Parent  => Target_Parent,
            Target_Subtree => T_Nodes (T_CC.Last).Next,
            Count          => Count);

         T_Nodes (T_Nodes (T_CC.Last).Next).Prev := T_CC.Last;
         T_CC.Last := T_Nodes (T_CC.Last).Next;

         C := S_Nodes (C).Next;
      end loop;

      --  We add the newly-allocated children to their parent list only after
      --  the allocation has succeeded, in order to preserve invariants of the
      --  parent.

      T_Node.Children := T_CC;
   end Copy_Children;

   ------------------
   -- Copy_Subtree --
   ------------------

   procedure Copy_Subtree
     (Target   : in out Tree;
      Parent   : Cursor;
      Before   : Cursor;
      Source   : Cursor)
   is
      Target_Subtree : Count_Type;
      Target_Count   : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Target'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with "Before cursor not in container";
         end if;

         if Before.Container.Nodes (Before.Node).Parent /= Parent.Node then
            raise Constraint_Error with "Before cursor not child of Parent";
         end if;
      end if;

      if Source = No_Element then
         return;
      end if;

      if Is_Root (Source) then
         raise Constraint_Error with "Source cursor designates root";
      end if;

      if Target.Count = 0 then
         Initialize_Root (Target);
      end if;

      --  Copy_Subtree returns a count of the number of nodes that it
      --  allocates, but it works by incrementing the value that is passed
      --  in. We must therefore initialize the count value before calling
      --  Copy_Subtree.

      Target_Count := 0;

      Copy_Subtree
        (Source         => Source.Container.all,
         Source_Subtree => Source.Node,
         Target         => Target,
         Target_Parent  => Parent.Node,
         Target_Subtree => Target_Subtree,
         Count          => Target_Count);

      Insert_Subtree_Node
        (Container => Target,
         Subtree   => Target_Subtree,
         Parent    => Parent.Node,
         Before    => Before.Node);

      Target.Count := Target.Count + Target_Count;
   end Copy_Subtree;

   procedure Copy_Subtree
     (Source         : Tree;
      Source_Subtree : Count_Type;
      Target         : in out Tree;
      Target_Parent  : Count_Type;
      Target_Subtree : out Count_Type;
      Count          : in out Count_Type)
   is
      T_Nodes : Tree_Node_Array renames Target.Nodes;

   begin
      --  First we allocate the root of the target subtree.

      Allocate_Node
        (Container => Target,
         New_Item  => Source.Elements (Source_Subtree),
         New_Node  => Target_Subtree);

      T_Nodes (Target_Subtree).Parent := Target_Parent;
      Count := Count + 1;

      --  We now have a new subtree (for the Target tree), containing only a
      --  copy of the corresponding element in the Source subtree. Next we copy
      --  the children of the Source subtree as children of the new Target
      --  subtree.

      Copy_Children
        (Source        => Source,
         Source_Parent => Source_Subtree,
         Target        => Target,
         Target_Parent => Target_Subtree,
         Count         => Count);
   end Copy_Subtree;

   -------------------------
   -- Deallocate_Children --
   -------------------------

   procedure Deallocate_Children
     (Container : in out Tree;
      Subtree   : Count_Type;
      Count     : in out Count_Type)
   is
      Nodes : Tree_Node_Array renames Container.Nodes;
      Node  : Tree_Node_Type renames Nodes (Subtree);  -- parent
      CC    : Children_Type renames Node.Children;
      C     : Count_Type'Base;

   begin
      while CC.First > 0 loop
         C := CC.First;
         CC.First := Nodes (C).Next;

         Deallocate_Subtree (Container, C, Count);
      end loop;

      CC.Last := 0;
   end Deallocate_Children;

   ---------------------
   -- Deallocate_Node --
   ---------------------

   procedure Deallocate_Node
     (Container : in out Tree;
      X         : Count_Type)
   is
      NN : Tree_Node_Array renames Container.Nodes;
      pragma Assert (X > 0);
      pragma Assert (X <= NN'Last);

      N : Tree_Node_Type renames NN (X);
      pragma Assert (N.Parent /= X);  -- node is active

   begin
      --  The tree container actually contains two lists: one for the "active"
      --  nodes that contain elements that have been inserted onto the tree,
      --  and another for the "inactive" nodes of the free store, from which
      --  nodes are allocated when a new child is inserted in the tree.

      --  We desire that merely declaring a tree object should have only
      --  minimal cost; specially, we want to avoid having to initialize the
      --  free store (to fill in the links), especially if the capacity of the
      --  tree object is large.

      --  The head of the free list is indicated by Container.Free. If its
      --  value is non-negative, then the free store has been initialized in
      --  the "normal" way: Container.Free points to the head of the list of
      --  free (inactive) nodes, and the value 0 means the free list is
      --  empty. Each node on the free list has been initialized to point to
      --  the next free node (via its Next component), and the value 0 means
      --  that this is the last node of the free list.

      --  If Container.Free is negative, then the links on the free store have
      --  not been initialized. In this case the link values are implied: the
      --  free store comprises the components of the node array started with
      --  the absolute value of Container.Free, and continuing until the end of
      --  the array (Nodes'Last).

      --  We prefer to lazy-init the free store (in fact, we would prefer to
      --  not initialize it at all, because such initialization is an O(n)
      --  operation). The time when we need to actually initialize the nodes in
      --  the free store is when the node that becomes inactive is not at the
      --  end of the active list. The free store would then be discontigous and
      --  so its nodes would need to be linked in the traditional way.

      --  It might be possible to perform an optimization here. Suppose that
      --  the free store can be represented as having two parts: one comprising
      --  the non-contiguous inactive nodes linked together in the normal way,
      --  and the other comprising the contiguous inactive nodes (that are not
      --  linked together, at the end of the nodes array). This would allow us
      --  to never have to initialize the free store, except in a lazy way as
      --  nodes become inactive. ???

      --  When an element is deleted from the list container, its node becomes
      --  inactive, and so we set its Parent and Prev components to an
      --  impossible value (the index of the node itself), to indicate that it
      --  is now inactive. This provides a useful way to detect a dangling
      --  cursor reference.

      N.Parent := X;  -- Node is deallocated (not on active list)
      N.Prev := X;

      if Container.Free >= 0 then
         --  The free store has previously been initialized. All we need to do
         --  here is link the newly-free'd node onto the free list.

         N.Next := Container.Free;
         Container.Free := X;

      elsif X + 1 = abs Container.Free then
         --  The free store has not been initialized, and the node becoming
         --  inactive immediately precedes the start of the free store. All
         --  we need to do is move the start of the free store back by one.

         N.Next := X;  -- Not strictly necessary, but marginally safer
         Container.Free := Container.Free + 1;

      else
         --  The free store has not been initialized, and the node becoming
         --  inactive does not immediately precede the free store. Here we
         --  first initialize the free store (meaning the links are given
         --  values in the traditional way), and then link the newly-free'd
         --  node onto the head of the free store.

         --  See the comments above for an optimization opportunity. If the
         --  next link for a node on the free store is negative, then this
         --  means the remaining nodes on the free store are physically
         --  contiguous, starting at the absolute value of that index value.
         --  ???

         Container.Free := abs Container.Free;

         if Container.Free > Container.Capacity then
            Container.Free := 0;

         else
            for J in Container.Free .. Container.Capacity - 1 loop
               NN (J).Next := J + 1;
            end loop;

            NN (Container.Capacity).Next := 0;
         end if;

         NN (X).Next := Container.Free;
         Container.Free := X;
      end if;
   end Deallocate_Node;

   ------------------------
   -- Deallocate_Subtree --
   ------------------------

   procedure Deallocate_Subtree
     (Container : in out Tree;
      Subtree   : Count_Type;
      Count     : in out Count_Type)
   is
   begin
      Deallocate_Children (Container, Subtree, Count);
      Deallocate_Node (Container, Subtree);
      Count := Count + 1;
   end Deallocate_Subtree;

   ---------------------
   -- Delete_Children --
   ---------------------

   procedure Delete_Children
     (Container : in out Tree;
      Parent    : Cursor)
   is
      Count : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return;
      end if;

      --  Deallocate_Children returns a count of the number of nodes that it
      --  deallocates, but it works by incrementing the value that is passed
      --  in. We must therefore initialize the count value before calling
      --  Deallocate_Children.

      Count := 0;

      Deallocate_Children (Container, Parent.Node, Count);
      pragma Assert (Count <= Container.Count);

      Container.Count := Container.Count - Count;
   end Delete_Children;

   -----------------
   -- Delete_Leaf --
   -----------------

   procedure Delete_Leaf
     (Container : in out Tree;
      Position  : in out Cursor)
   is
      X : Count_Type;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor not in container";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      if not Is_Leaf (Position) then
         raise Constraint_Error with "Position cursor does not designate leaf";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      X := Position.Node;
      Position := No_Element;

      Remove_Subtree (Container, X);
      Container.Count := Container.Count - 1;

      Deallocate_Node (Container, X);
   end Delete_Leaf;

   --------------------
   -- Delete_Subtree --
   --------------------

   procedure Delete_Subtree
     (Container : in out Tree;
      Position  : in out Cursor)
   is
      X     : Count_Type;
      Count : Count_Type;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor not in container";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      X := Position.Node;
      Position := No_Element;

      Remove_Subtree (Container, X);

      --  Deallocate_Subtree returns a count of the number of nodes that it
      --  deallocates, but it works by incrementing the value that is passed
      --  in. We must therefore initialize the count value before calling
      --  Deallocate_Subtree.

      Count := 0;

      Deallocate_Subtree (Container, X, Count);
      pragma Assert (Count <= Container.Count);

      Container.Count := Container.Count - Count;
   end Delete_Subtree;

   -----------
   -- Depth --
   -----------

   function Depth (Position : Cursor) return Count_Type is
      Result : Count_Type;
      N      : Count_Type'Base;

   begin
      if Position = No_Element then
         return 0;
      end if;

      if Is_Root (Position) then
         return 1;
      end if;

      Result := 0;
      N := Position.Node;
      while N >= 0 loop
         N := Position.Container.Nodes (N).Parent;
         Result := Result + 1;
      end loop;

      return Result;
   end Depth;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Node = Root_Node (Position.Container.all) then
         raise Program_Error with "Position cursor designates root";
      end if;

      return Position.Container.Elements (Position.Node);
   end Element;

   --------------------
   -- Equal_Children --
   --------------------

   function Equal_Children
     (Left_Tree     : Tree;
      Left_Subtree  : Count_Type;
      Right_Tree    : Tree;
      Right_Subtree : Count_Type) return Boolean
   is
      L_NN : Tree_Node_Array renames Left_Tree.Nodes;
      R_NN : Tree_Node_Array renames Right_Tree.Nodes;

      Left_Children  : Children_Type renames L_NN (Left_Subtree).Children;
      Right_Children : Children_Type renames R_NN (Right_Subtree).Children;

      L, R : Count_Type'Base;

   begin
      if Child_Count (Left_Tree, Left_Subtree)
        /= Child_Count (Right_Tree, Right_Subtree)
      then
         return False;
      end if;

      L := Left_Children.First;
      R := Right_Children.First;
      while L > 0 loop
         if not Equal_Subtree (Left_Tree, L, Right_Tree, R) then
            return False;
         end if;

         L := L_NN (L).Next;
         R := R_NN (R).Next;
      end loop;

      return True;
   end Equal_Children;

   -------------------
   -- Equal_Subtree --
   -------------------

   function Equal_Subtree
     (Left_Position  : Cursor;
      Right_Position : Cursor) return Boolean
   is
   begin
      if Left_Position = No_Element then
         raise Constraint_Error with "Left cursor has no element";
      end if;

      if Right_Position = No_Element then
         raise Constraint_Error with "Right cursor has no element";
      end if;

      if Left_Position = Right_Position then
         return True;
      end if;

      if Is_Root (Left_Position) then
         if not Is_Root (Right_Position) then
            return False;
         end if;

         if Left_Position.Container.Count = 0 then
            return Right_Position.Container.Count = 0;
         end if;

         if Right_Position.Container.Count = 0 then
            return False;
         end if;

         return Equal_Children
                  (Left_Tree     => Left_Position.Container.all,
                   Left_Subtree  => Left_Position.Node,
                   Right_Tree    => Right_Position.Container.all,
                   Right_Subtree => Right_Position.Node);
      end if;

      if Is_Root (Right_Position) then
         return False;
      end if;

      return Equal_Subtree
               (Left_Tree     => Left_Position.Container.all,
                Left_Subtree  => Left_Position.Node,
                Right_Tree    => Right_Position.Container.all,
                Right_Subtree => Right_Position.Node);
   end Equal_Subtree;

   function Equal_Subtree
     (Left_Tree     : Tree;
      Left_Subtree  : Count_Type;
      Right_Tree    : Tree;
      Right_Subtree : Count_Type) return Boolean
   is
   begin
      if Left_Tree.Elements  (Left_Subtree) /=
         Right_Tree.Elements (Right_Subtree)
      then
         return False;
      end if;

      return Equal_Children
               (Left_Tree     => Left_Tree,
                Left_Subtree  => Left_Subtree,
                Right_Tree    => Right_Tree,
                Right_Subtree => Right_Subtree);
   end Equal_Subtree;

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

   procedure Finalize (Object : in out Child_Iterator) is
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
     (Container : Tree;
      Item      : Element_Type) return Cursor
   is
      Node : Count_Type;

   begin
      if Container.Count = 0 then
         return No_Element;
      end if;

      Node := Find_In_Children (Container, Root_Node (Container), Item);

      if Node = 0 then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Node);
   end Find;

   function First (Object : Iterator) return Cursor is
   begin
      return Object.Position;
   end First;

   function First (Object : Child_Iterator) return Cursor is
      Node : Count_Type'Base;
   begin
      Node := Object.Container.Nodes (Object.Position.Node).Children.First;
      return (Object.Container, Node);
   end First;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Parent : Cursor) return Cursor is
      Node : Count_Type'Base;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return No_Element;
      end if;

      Node := Parent.Container.Nodes (Parent.Node).Children.First;

      if Node <= 0 then
         return No_Element;
      end if;

      return Cursor'(Parent.Container, Node);
   end First_Child;

   -------------------------
   -- First_Child_Element --
   -------------------------

   function First_Child_Element (Parent : Cursor) return Element_Type is
   begin
      return Element (First_Child (Parent));
   end First_Child_Element;

   ----------------------
   -- Find_In_Children --
   ----------------------

   function Find_In_Children
     (Container : Tree;
      Subtree   : Count_Type;
      Item      : Element_Type) return Count_Type
   is
      N      : Count_Type'Base;
      Result : Count_Type;

   begin
      N := Container.Nodes (Subtree).Children.First;
      while N > 0 loop
         Result := Find_In_Subtree (Container, N, Item);

         if Result > 0 then
            return Result;
         end if;

         N := Container.Nodes (N).Next;
      end loop;

      return 0;
   end Find_In_Children;

   ---------------------
   -- Find_In_Subtree --
   ---------------------

   function Find_In_Subtree
     (Position : Cursor;
      Item     : Element_Type) return Cursor
   is
      Result : Count_Type;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      --  Commented-out pending ruling by ARG.  ???

      --  if Position.Container /= Container'Unrestricted_Access then
      --     raise Program_Error with "Position cursor not in container";
      --  end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return No_Element;
      end if;

      if Is_Root (Position) then
         Result := Find_In_Children
                     (Container => Position.Container.all,
                      Subtree   => Position.Node,
                      Item      => Item);

      else
         Result := Find_In_Subtree
                     (Container => Position.Container.all,
                      Subtree   => Position.Node,
                      Item      => Item);
      end if;

      if Result = 0 then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Result);
   end Find_In_Subtree;

   function Find_In_Subtree
     (Container : Tree;
      Subtree   : Count_Type;
      Item      : Element_Type) return Count_Type
   is
   begin
      if Container.Elements (Subtree) = Item then
         return Subtree;
      end if;

      return Find_In_Children (Container, Subtree, Item);
   end Find_In_Subtree;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position = No_Element then
         return False;
      end if;

      return Position.Node /= Root_Node (Position.Container.all);
   end Has_Element;

   ---------------------
   -- Initialize_Node --
   ---------------------

   procedure Initialize_Node
     (Container : in out Tree;
      Index     : Count_Type)
   is
   begin
      Container.Nodes (Index) :=
        (Parent   => No_Node,
         Prev     => 0,
         Next     => 0,
         Children => (others => 0));
   end Initialize_Node;

   ---------------------
   -- Initialize_Root --
   ---------------------

   procedure Initialize_Root (Container : in out Tree) is
   begin
      Initialize_Node (Container, Root_Node (Container));
   end Initialize_Root;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Position : Cursor;
      pragma Unreferenced (Position);

   begin
      Insert_Child (Container, Parent, Before, New_Item, Position, Count);
   end Insert_Child;

   procedure Insert_Child
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Nodes : Tree_Node_Array renames Container.Nodes;
      Last  : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error with "Before cursor not in container";
         end if;

         if Before.Container.Nodes (Before.Node).Parent /= Parent.Node then
            raise Constraint_Error with "Parent cursor not parent of Before";
         end if;
      end if;

      if Count = 0 then
         Position := No_Element;  -- Need ruling from ARG ???
         return;
      end if;

      if Container.Count > Container.Capacity - Count then
         raise Constraint_Error
           with "requested count exceeds available storage";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container.Count = 0 then
         Initialize_Root (Container);
      end if;

      Allocate_Node (Container, New_Item, Position.Node);
      Nodes (Position.Node).Parent := Parent.Node;

      Last := Position.Node;
      for J in Count_Type'(2) .. Count loop
         Allocate_Node (Container, New_Item, Nodes (Last).Next);
         Nodes (Nodes (Last).Next).Parent := Parent.Node;
         Nodes (Nodes (Last).Next).Prev := Last;

         Last := Nodes (Last).Next;
      end loop;

      Insert_Subtree_List
        (Container => Container,
         First     => Position.Node,
         Last      => Last,
         Parent    => Parent.Node,
         Before    => Before.Node);

      Container.Count := Container.Count + Count;

      Position.Container := Parent.Container;
   end Insert_Child;

   procedure Insert_Child
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Nodes : Tree_Node_Array renames Container.Nodes;
      Last  : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error with "Before cursor not in container";
         end if;

         if Before.Container.Nodes (Before.Node).Parent /= Parent.Node then
            raise Constraint_Error with "Parent cursor not parent of Before";
         end if;
      end if;

      if Count = 0 then
         Position := No_Element;  -- Need ruling from ARG  ???
         return;
      end if;

      if Container.Count > Container.Capacity - Count then
         raise Constraint_Error
           with "requested count exceeds available storage";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container.Count = 0 then
         Initialize_Root (Container);
      end if;

      Allocate_Node (Container, Position.Node);
      Nodes (Position.Node).Parent := Parent.Node;

      Last := Position.Node;
      for J in Count_Type'(2) .. Count loop
         Allocate_Node (Container, Nodes (Last).Next);
         Nodes (Nodes (Last).Next).Parent := Parent.Node;
         Nodes (Nodes (Last).Next).Prev := Last;

         Last := Nodes (Last).Next;
      end loop;

      Insert_Subtree_List
        (Container => Container,
         First     => Position.Node,
         Last      => Last,
         Parent    => Parent.Node,
         Before    => Before.Node);

      Container.Count := Container.Count + Count;

      Position.Container := Parent.Container;
   end Insert_Child;

   -------------------------
   -- Insert_Subtree_List --
   -------------------------

   procedure Insert_Subtree_List
     (Container : in out Tree;
      First     : Count_Type'Base;
      Last      : Count_Type'Base;
      Parent    : Count_Type;
      Before    : Count_Type'Base)
   is
      NN : Tree_Node_Array renames Container.Nodes;
      N  : Tree_Node_Type renames NN (Parent);
      CC : Children_Type renames N.Children;

   begin
      --  This is a simple utility operation to insert a list of nodes
      --  (First..Last) as children of Parent. The Before node specifies where
      --  the new children should be inserted relative to existing children.

      if First <= 0 then
         pragma Assert (Last <= 0);
         return;
      end if;

      pragma Assert (Last > 0);
      pragma Assert (Before <= 0 or else NN (Before).Parent = Parent);

      if CC.First <= 0 then  -- no existing children
         CC.First := First;
         NN (CC.First).Prev := 0;
         CC.Last := Last;
         NN (CC.Last).Next := 0;

      elsif Before <= 0 then  -- means "insert after existing nodes"
         NN (CC.Last).Next := First;
         NN (First).Prev := CC.Last;
         CC.Last := Last;
         NN (CC.Last).Next := 0;

      elsif Before = CC.First then
         NN (Last).Next := CC.First;
         NN (CC.First).Prev := Last;
         CC.First := First;
         NN (CC.First).Prev := 0;

      else
         NN (NN (Before).Prev).Next := First;
         NN (First).Prev := NN (Before).Prev;
         NN (Last).Next := Before;
         NN (Before).Prev := Last;
      end if;
   end Insert_Subtree_List;

   -------------------------
   -- Insert_Subtree_Node --
   -------------------------

   procedure Insert_Subtree_Node
     (Container : in out Tree;
      Subtree   : Count_Type'Base;
      Parent    : Count_Type;
      Before    : Count_Type'Base)
   is
   begin
      --  This is a simple wrapper operation to insert a single child into the
      --  Parent's children list.

      Insert_Subtree_List
        (Container => Container,
         First     => Subtree,
         Last      => Subtree,
         Parent    => Parent,
         Before    => Before);
   end Insert_Subtree_Node;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
   begin
      return Container.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Position : Cursor) return Boolean is
   begin
      if Position = No_Element then
         return False;
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return True;
      end if;

      return Position.Container.Nodes (Position.Node).Children.First <= 0;
   end Is_Leaf;

   ------------------
   -- Is_Reachable --
   ------------------

   function Is_Reachable
     (Container : Tree;
      From, To  : Count_Type) return Boolean
   is
      Idx : Count_Type;

   begin
      Idx := From;
      while Idx >= 0 loop
         if Idx = To then
            return True;
         end if;

         Idx := Container.Nodes (Idx).Parent;
      end loop;

      return False;
   end Is_Reachable;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
   begin
      return
        (if Position.Container = null then False
         else Position.Node = Root_Node (Position.Container.all));
   end Is_Root;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor))
   is
      B : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      if Container.Count = 0 then
         return;
      end if;

      B := B + 1;

      Iterate_Children
        (Container => Container,
         Subtree   => Root_Node (Container),
         Process   => Process);

      B := B - 1;

   exception
      when others =>
         B := B - 1;
         raise;
   end Iterate;

   function Iterate (Container : Tree)
     return Tree_Iterator_Interfaces.Forward_Iterator'Class
   is
      B  : Natural renames Container'Unrestricted_Access.all.Busy;
      RC : constant Cursor :=
             (Container'Unrestricted_Access, Root_Node (Container));

   begin
      return It : constant Iterator :=
                    Iterator'(Limited_Controlled with
                                Container => Container'Unrestricted_Access,
                                Position  => First_Child (RC),
                                From_Root => True)
      do
         B := B + 1;
      end return;
   end Iterate;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor))
   is
   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return;
      end if;

      declare
         B  : Natural renames Parent.Container.Busy;
         C  : Count_Type;
         NN : Tree_Node_Array renames Parent.Container.Nodes;

      begin
         B := B + 1;

         C := NN (Parent.Node).Children.First;
         while C > 0 loop
            Process (Cursor'(Parent.Container, Node => C));
            C := NN (C).Next;
         end loop;

         B := B - 1;

      exception
         when others =>
            B := B - 1;
            raise;
      end;
   end Iterate_Children;

   procedure Iterate_Children
     (Container : Tree;
      Subtree   : Count_Type;
      Process   : not null access procedure (Position : Cursor))
   is
      NN : Tree_Node_Array renames Container.Nodes;
      N  : Tree_Node_Type renames NN (Subtree);
      C  : Count_Type;

   begin
      --  This is a helper function to recursively iterate over all the nodes
      --  in a subtree, in depth-first fashion. This particular helper just
      --  visits the children of this subtree, not the root of the subtree
      --  itself. This is useful when starting from the ultimate root of the
      --  entire tree (see Iterate), as that root does not have an element.

      C := N.Children.First;
      while C > 0 loop
         Iterate_Subtree (Container, C, Process);
         C := NN (C).Next;
      end loop;
   end Iterate_Children;

   function Iterate_Children
     (Container : Tree;
      Parent    : Cursor)
      return Tree_Iterator_Interfaces.Reversible_Iterator'Class
   is
      B : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      return It : constant Child_Iterator :=
                    Child_Iterator'(Limited_Controlled with
                                      Container => Parent.Container,
                                      Position  => Parent)
      do
         B := B + 1;
      end return;
   end Iterate_Children;

   ---------------------
   -- Iterate_Subtree --
   ---------------------

   function Iterate_Subtree
     (Position : Cursor)
      return Tree_Iterator_Interfaces.Forward_Iterator'Class
   is
      B : Natural renames Position.Container.all.Busy;

   begin
      return It : constant Iterator :=
                    Iterator'(Limited_Controlled with
                                Container => Position.Container,
                                Position  => Position,
                                From_Root => False)
      do
         B := B + 1;
      end return;
   end Iterate_Subtree;

   procedure Iterate_Subtree
     (Position  : Cursor;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return;
      end if;

      declare
         T : Tree renames Position.Container.all;
         B : Natural renames T.Busy;

      begin
         B := B + 1;

         if Is_Root (Position) then
            Iterate_Children (T, Position.Node, Process);
         else
            Iterate_Subtree (T, Position.Node, Process);
         end if;

         B := B - 1;

      exception
         when others =>
            B := B - 1;
            raise;
      end;
   end Iterate_Subtree;

   procedure Iterate_Subtree
     (Container : Tree;
      Subtree   : Count_Type;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      --  This is a helper function to recursively iterate over all the nodes
      --  in a subtree, in depth-first fashion. It first visits the root of the
      --  subtree, then visits its children.

      Process (Cursor'(Container'Unrestricted_Access, Subtree));
      Iterate_Children (Container, Subtree, Process);
   end Iterate_Subtree;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Child_Iterator) return Cursor is
   begin
      return Last_Child (Object.Position);
   end Last;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (Parent : Cursor) return Cursor is
      Node : Count_Type'Base;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return No_Element;
      end if;

      Node := Parent.Container.Nodes (Parent.Node).Children.Last;

      if Node <= 0 then
         return No_Element;
      end if;

      return Cursor'(Parent.Container, Node);
   end Last_Child;

   ------------------------
   -- Last_Child_Element --
   ------------------------

   function Last_Child_Element (Parent : Cursor) return Element_Type is
   begin
      return Element (Last_Child (Parent));
   end Last_Child_Element;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Tree; Source : in out Tree) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors of Source (tree is busy)";
      end if;

      Target.Assign (Source);
      Source.Clear;
   end Move;

   ----------
   -- Next --
   ----------

   function Next
     (Object : Iterator;
      Position : Cursor) return Cursor
   is
      T  : Tree renames Position.Container.all;
      NN : Tree_Node_Array renames T.Nodes;
      N  : Tree_Node_Type renames NN (Position.Node);

   begin
      if Is_Leaf (Position) then

         --  If sibling is present, return it

         if N.Next /= 0 then
            return (Object.Container, N.Next);

         --  If this is the last sibling, go to sibling of first ancestor that
         --  has a sibling, or terminate.

         else
            declare
               Pos : Count_Type := N.Parent;
               Par : Tree_Node_Type := NN (Pos);

            begin
               while Par.Next = 0 loop
                  Pos := Par.Parent;

                  --  If we are back at the root the iteration is complete

                  if Pos = No_Node then
                     return No_Element;

                  --  If this is a subtree iterator and we are back at the
                  --  starting node, iteration is complete.

                  elsif Pos = Object.Position.Node
                    and then not Object.From_Root
                  then
                     return No_Element;

                  else
                     Par := NN (Pos);
                  end if;
               end loop;

               if Pos = Object.Position.Node
                 and then not Object.From_Root
               then
                  return No_Element;
               end if;

               return (Object.Container, Par.Next);
            end;
         end if;

      --  If an internal node, return its first child

      else
         return (Object.Container, N.Children.First);
      end if;
   end Next;

   function Next
     (Object   : Child_Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Object.Container /= Position.Container then
         raise Program_Error;
      end if;

      return Next_Sibling (Position);
   end Next;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return No_Element;
      end if;

      declare
         T  : Tree renames Position.Container.all;
         NN : Tree_Node_Array renames T.Nodes;
         N  : Tree_Node_Type renames NN (Position.Node);

      begin
         if N.Next <= 0 then
            return No_Element;
         end if;

         return Cursor'(Position.Container, N.Next);
      end;
   end Next_Sibling;

   procedure Next_Sibling (Position : in out Cursor) is
   begin
      Position := Next_Sibling (Position);
   end Next_Sibling;

   ----------------
   -- Node_Count --
   ----------------

   function Node_Count (Container : Tree) return Count_Type is
   begin
      --  Container.Count is the number of nodes we have actually allocated. We
      --  cache the value specifically so this Node_Count operation can execute
      --  in O(1) time, which makes it behave similarly to how the Length
      --  selector function behaves for other containers.
      --
      --  The cached node count value only describes the nodes we have
      --  allocated; the root node itself is not included in that count. The
      --  Node_Count operation returns a value that includes the root node
      --  (because the RM says so), so we must add 1 to our cached value.

      return 1 + Container.Count;
   end Node_Count;

   ------------
   -- Parent --
   ------------

   function Parent (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return No_Element;
      end if;

      declare
         T  : Tree renames Position.Container.all;
         NN : Tree_Node_Array renames T.Nodes;
         N  : Tree_Node_Type renames NN (Position.Node);

      begin
         if N.Parent < 0 then
            pragma Assert (Position.Node = Root_Node (T));
            return No_Element;
         end if;

         return Cursor'(Position.Container, N.Parent);
      end;
   end Parent;

   -------------------
   -- Prepend_Child --
   -------------------

   procedure Prepend_Child
     (Container : in out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Nodes       : Tree_Node_Array renames Container.Nodes;
      First, Last : Count_Type;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Count = 0 then
         return;
      end if;

      if Container.Count > Container.Capacity - Count then
         raise Constraint_Error
           with "requested count exceeds available storage";
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Container.Count = 0 then
         Initialize_Root (Container);
      end if;

      Allocate_Node (Container, New_Item, First);
      Nodes (First).Parent := Parent.Node;

      Last := First;
      for J in Count_Type'(2) .. Count loop
         Allocate_Node (Container, New_Item, Nodes (Last).Next);
         Nodes (Nodes (Last).Next).Parent := Parent.Node;
         Nodes (Nodes (Last).Next).Prev := Last;

         Last := Nodes (Last).Next;
      end loop;

      Insert_Subtree_List
        (Container => Container,
         First     => First,
         Last      => Last,
         Parent    => Parent.Node,
         Before    => Nodes (Parent.Node).Children.First);

      Container.Count := Container.Count + Count;
   end Prepend_Child;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object   : Child_Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Object.Container /= Position.Container then
         raise Program_Error;
      end if;

      return Previous_Sibling (Position);
   end Previous;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return No_Element;
      end if;

      declare
         T  : Tree renames Position.Container.all;
         NN : Tree_Node_Array renames T.Nodes;
         N  : Tree_Node_Type renames NN (Position.Node);

      begin
         if N.Prev <= 0 then
            return No_Element;
         end if;

         return Cursor'(Position.Container, N.Prev);
      end;
   end Previous_Sibling;

   procedure Previous_Sibling (Position : in out Cursor) is
   begin
      Position := Previous_Sibling (Position);
   end Previous_Sibling;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      declare
         T : Tree renames Position.Container.all'Unrestricted_Access.all;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         Process (Element => T.Elements (Position.Node));

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Tree)
   is
      procedure Read_Children (Subtree : Count_Type);

      function Read_Subtree
        (Parent : Count_Type) return Count_Type;

      NN : Tree_Node_Array renames Container.Nodes;

      Total_Count : Count_Type'Base;
      --  Value read from the stream that says how many elements follow

      Read_Count : Count_Type'Base;
      --  Actual number of elements read from the stream

      -------------------
      -- Read_Children --
      -------------------

      procedure Read_Children (Subtree : Count_Type) is
         Count : Count_Type'Base;
         --  number of child subtrees

         CC : Children_Type;

      begin
         Count_Type'Read (Stream, Count);

         if Count < 0 then
            raise Program_Error with "attempt to read from corrupt stream";
         end if;

         if Count = 0 then
            return;
         end if;

         CC.First := Read_Subtree (Parent => Subtree);
         CC.Last := CC.First;

         for J in Count_Type'(2) .. Count loop
            NN (CC.Last).Next := Read_Subtree (Parent => Subtree);
            NN (NN (CC.Last).Next).Prev := CC.Last;
            CC.Last := NN (CC.Last).Next;
         end loop;

         --  Now that the allocation and reads have completed successfully, it
         --  is safe to link the children to their parent.

         NN (Subtree).Children := CC;
      end Read_Children;

      ------------------
      -- Read_Subtree --
      ------------------

      function Read_Subtree
        (Parent : Count_Type) return Count_Type
      is
         Subtree : Count_Type;

      begin
         Allocate_Node (Container, Stream, Subtree);
         Container.Nodes (Subtree).Parent := Parent;

         Read_Count := Read_Count + 1;

         Read_Children (Subtree);

         return Subtree;
      end Read_Subtree;

   --  Start of processing for Read

   begin
      Container.Clear;  -- checks busy bit

      Count_Type'Read (Stream, Total_Count);

      if Total_Count < 0 then
         raise Program_Error with "attempt to read from corrupt stream";
      end if;

      if Total_Count = 0 then
         return;
      end if;

      if Total_Count > Container.Capacity then
         raise Capacity_Error  -- ???
           with "node count in stream exceeds container capacity";
      end if;

      Initialize_Root (Container);

      Read_Count := 0;

      Read_Children (Root_Node (Container));

      if Read_Count /= Total_Count then
         raise Program_Error with "attempt to read from corrupt stream";
      end if;

      Container.Count := Total_Count;
   end Read;

   procedure Read
     (Stream   : not null access Root_Stream_Type'Class;
      Position : out Cursor)
   is
   begin
      raise Program_Error with "attempt to read tree cursor from stream";
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

   function Constant_Reference
     (Container : aliased Tree;
      Position  : Cursor) return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return
        (Element =>
           Position.Container.Elements (Position.Node)'Unchecked_Access);
   end Constant_Reference;

   function Reference
     (Container : aliased Tree;
      Position  : Cursor) return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return
        (Element =>
           Position.Container.Elements (Position.Node)'Unchecked_Access);
   end Reference;

   --------------------
   -- Remove_Subtree --
   --------------------

   procedure Remove_Subtree
     (Container : in out Tree;
      Subtree   : Count_Type)
   is
      NN : Tree_Node_Array renames Container.Nodes;
      N  : Tree_Node_Type renames NN (Subtree);
      CC : Children_Type renames NN (N.Parent).Children;

   begin
      --  This is a utility operation to remove a subtree node from its
      --  parent's list of children.

      if CC.First = Subtree then
         pragma Assert (N.Prev <= 0);

         if CC.Last = Subtree then
            pragma Assert (N.Next <= 0);
            CC.First := 0;
            CC.Last := 0;

         else
            CC.First := N.Next;
            NN (CC.First).Prev := 0;
         end if;

      elsif CC.Last = Subtree then
         pragma Assert (N.Next <= 0);
         CC.Last := N.Prev;
         NN (CC.Last).Next := 0;

      else
         NN (N.Prev).Next := N.Next;
         NN (N.Next).Prev := N.Prev;
      end if;
   end Remove_Subtree;

   ----------------------
   -- Replace_Element --
   ----------------------

   procedure Replace_Element
     (Container : in out Tree;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor not in container";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      if Container.Lock > 0 then
         raise Program_Error
           with "attempt to tamper with elements (tree is locked)";
      end if;

      Container.Elements (Position.Node) := New_Item;
   end Replace_Element;

   ------------------------------
   -- Reverse_Iterate_Children --
   ------------------------------

   procedure Reverse_Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor))
   is
   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container.Count = 0 then
         pragma Assert (Is_Root (Parent));
         return;
      end if;

      declare
         NN : Tree_Node_Array renames Parent.Container.Nodes;
         B  : Natural renames Parent.Container.Busy;
         C  : Count_Type;

      begin
         B := B + 1;

         C := NN (Parent.Node).Children.Last;
         while C > 0 loop
            Process (Cursor'(Parent.Container, Node => C));
            C := NN (C).Prev;
         end loop;

         B := B - 1;

      exception
         when others =>
            B := B - 1;
            raise;
      end;
   end Reverse_Iterate_Children;

   ----------
   -- Root --
   ----------

   function Root (Container : Tree) return Cursor is
   begin
      return (Container'Unrestricted_Access, Root_Node (Container));
   end Root;

   ---------------
   -- Root_Node --
   ---------------

   function Root_Node (Container : Tree) return Count_Type is
      pragma Unreferenced (Container);

   begin
      return 0;
   end Root_Node;

   ---------------------
   -- Splice_Children --
   ---------------------

   procedure Splice_Children
     (Target        : in out Tree;
      Target_Parent : Cursor;
      Before        : Cursor;
      Source        : in out Tree;
      Source_Parent : Cursor)
   is
   begin
      if Target_Parent = No_Element then
         raise Constraint_Error with "Target_Parent cursor has no element";
      end if;

      if Target_Parent.Container /= Target'Unrestricted_Access then
         raise Program_Error
           with "Target_Parent cursor not in Target container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Target'Unrestricted_Access then
            raise Program_Error
              with "Before cursor not in Target container";
         end if;

         if Target.Nodes (Before.Node).Parent /= Target_Parent.Node then
            raise Constraint_Error
              with "Before cursor not child of Target_Parent";
         end if;
      end if;

      if Source_Parent = No_Element then
         raise Constraint_Error with "Source_Parent cursor has no element";
      end if;

      if Source_Parent.Container /= Source'Unrestricted_Access then
         raise Program_Error
           with "Source_Parent cursor not in Source container";
      end if;

      if Source.Count = 0 then
         pragma Assert (Is_Root (Source_Parent));
         return;
      end if;

      if Target'Address = Source'Address then
         if Target_Parent = Source_Parent then
            return;
         end if;

         if Target.Busy > 0 then
            raise Program_Error
              with "attempt to tamper with cursors (Target tree is busy)";
         end if;

         if Is_Reachable (Container => Target,
                          From      => Target_Parent.Node,
                          To        => Source_Parent.Node)
         then
            raise Constraint_Error
              with "Source_Parent is ancestor of Target_Parent";
         end if;

         Splice_Children
           (Container     => Target,
            Target_Parent => Target_Parent.Node,
            Before        => Before.Node,
            Source_Parent => Source_Parent.Node);

         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (Target tree is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (Source tree is busy)";
      end if;

      if Target.Count = 0 then
         Initialize_Root (Target);
      end if;

      Splice_Children
        (Target        => Target,
         Target_Parent => Target_Parent.Node,
         Before        => Before.Node,
         Source        => Source,
         Source_Parent => Source_Parent.Node);
   end Splice_Children;

   procedure Splice_Children
     (Container       : in out Tree;
      Target_Parent   : Cursor;
      Before          : Cursor;
      Source_Parent   : Cursor)
   is
   begin
      if Target_Parent = No_Element then
         raise Constraint_Error with "Target_Parent cursor has no element";
      end if;

      if Target_Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error
           with "Target_Parent cursor not in container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error
              with "Before cursor not in container";
         end if;

         if Container.Nodes (Before.Node).Parent /= Target_Parent.Node then
            raise Constraint_Error
              with "Before cursor not child of Target_Parent";
         end if;
      end if;

      if Source_Parent = No_Element then
         raise Constraint_Error with "Source_Parent cursor has no element";
      end if;

      if Source_Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error
           with "Source_Parent cursor not in container";
      end if;

      if Target_Parent = Source_Parent then
         return;
      end if;

      pragma Assert (Container.Count > 0);

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Is_Reachable (Container => Container,
                       From      => Target_Parent.Node,
                       To        => Source_Parent.Node)
      then
         raise Constraint_Error
           with "Source_Parent is ancestor of Target_Parent";
      end if;

      Splice_Children
        (Container     => Container,
         Target_Parent => Target_Parent.Node,
         Before        => Before.Node,
         Source_Parent => Source_Parent.Node);
   end Splice_Children;

   procedure Splice_Children
     (Container     : in out Tree;
      Target_Parent : Count_Type;
      Before        : Count_Type'Base;
      Source_Parent : Count_Type)
   is
      NN : Tree_Node_Array renames Container.Nodes;
      CC : constant Children_Type := NN (Source_Parent).Children;
      C  : Count_Type'Base;

   begin
      --  This is a utility operation to remove the children from Source parent
      --  and insert them into Target parent.

      NN (Source_Parent).Children := Children_Type'(others => 0);

      --  Fix up the Parent pointers of each child to designate its new Target
      --  parent.

      C := CC.First;
      while C > 0 loop
         NN (C).Parent := Target_Parent;
         C := NN (C).Next;
      end loop;

      Insert_Subtree_List
        (Container => Container,
         First     => CC.First,
         Last      => CC.Last,
         Parent    => Target_Parent,
         Before    => Before);
   end Splice_Children;

   procedure Splice_Children
     (Target        : in out Tree;
      Target_Parent : Count_Type;
      Before        : Count_Type'Base;
      Source        : in out Tree;
      Source_Parent : Count_Type)
   is
      S_NN : Tree_Node_Array renames Source.Nodes;
      S_CC : Children_Type renames S_NN (Source_Parent).Children;

      Target_Count, Source_Count : Count_Type;
      T, S                       : Count_Type'Base;

   begin
      --  This is a utility operation to copy the children from the Source
      --  parent and insert them as children of the Target parent, and then
      --  delete them from the Source. (This is not a true splice operation,
      --  but it is the best we can do in a bounded form.) The Before position
      --  specifies where among the Target parent's exising children the new
      --  children are inserted.

      --  Before we attempt the insertion, we must count the sources nodes in
      --  order to determine whether the target have enough storage
      --  available. Note that calculating this value is an O(n) operation.

      --  Here is an optimization opportunity: iterate of each children the
      --  source explicitly, and keep a running count of the total number of
      --  nodes. Compare the running total to the capacity of the target each
      --  pass through the loop. This is more efficient than summing the counts
      --  of child subtree (which is what Subtree_Node_Count does) and then
      --  comparing that total sum to the target's capacity.  ???

      --  Here is another possibility. We currently treat the splice as an
      --  all-or-nothing proposition: either we can insert all of children of
      --  the source, or we raise exception with modifying the target. The
      --  price for not causing side-effect is an O(n) determination of the
      --  source count. If we are willing to tolerate side-effect, then we
      --  could loop over the children of the source, counting that subtree and
      --  then immediately inserting it in the target. The issue here is that
      --  the test for available storage could fail during some later pass,
      --  after children have already been inserted into target. ???

      Source_Count := Subtree_Node_Count (Source, Source_Parent) - 1;

      if Source_Count = 0 then
         return;
      end if;

      if Target.Count > Target.Capacity - Source_Count then
         raise Capacity_Error  -- ???
           with "Source count exceeds available storage on Target";
      end if;

      --  Copy_Subtree returns a count of the number of nodes it inserts, but
      --  it does this by incrementing the value passed in. Therefore we must
      --  initialize the count before calling Copy_Subtree.

      Target_Count := 0;

      S := S_CC.First;
      while S > 0 loop
         Copy_Subtree
           (Source         => Source,
            Source_Subtree => S,
            Target         => Target,
            Target_Parent  => Target_Parent,
            Target_Subtree => T,
            Count          => Target_Count);

         Insert_Subtree_Node
           (Container => Target,
            Subtree   => T,
            Parent    => Target_Parent,
            Before    => Before);

         S := S_NN (S).Next;
      end loop;

      pragma Assert (Target_Count = Source_Count);
      Target.Count := Target.Count + Target_Count;

      --  As with Copy_Subtree, operation Deallocate_Children returns a count
      --  of the number of nodes it deallocates, but it works by incrementing
      --  the value passed in. We must therefore initialize the count before
      --  calling it.

      Source_Count := 0;

      Deallocate_Children (Source, Source_Parent, Source_Count);
      pragma Assert (Source_Count = Target_Count);

      Source.Count := Source.Count - Source_Count;
   end Splice_Children;

   --------------------
   -- Splice_Subtree --
   --------------------

   procedure Splice_Subtree
     (Target   : in out Tree;
      Parent   : Cursor;
      Before   : Cursor;
      Source   : in out Tree;
      Position : in out Cursor)
   is
   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Target'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in Target container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Target'Unrestricted_Access then
            raise Program_Error with "Before cursor not in Target container";
         end if;

         if Target.Nodes (Before.Node).Parent /= Parent.Node then
            raise Constraint_Error with "Before cursor not child of Parent";
         end if;
      end if;

      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Source'Unrestricted_Access then
         raise Program_Error with "Position cursor not in Source container";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      if Target'Address = Source'Address then
         if Target.Nodes (Position.Node).Parent = Parent.Node then
            if Before = No_Element then
               if Target.Nodes (Position.Node).Next <= 0 then  -- last child
                  return;
               end if;

            elsif Position.Node = Before.Node then
               return;

            elsif Target.Nodes (Position.Node).Next = Before.Node then
               return;
            end if;
         end if;

         if Target.Busy > 0 then
            raise Program_Error
              with "attempt to tamper with cursors (Target tree is busy)";
         end if;

         if Is_Reachable (Container => Target,
                          From      => Parent.Node,
                          To        => Position.Node)
         then
            raise Constraint_Error with "Position is ancestor of Parent";
         end if;

         Remove_Subtree (Target, Position.Node);

         Target.Nodes (Position.Node).Parent := Parent.Node;
         Insert_Subtree_Node (Target, Position.Node, Parent.Node, Before.Node);

         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (Target tree is busy)";
      end if;

      if Source.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (Source tree is busy)";
      end if;

      if Target.Count = 0 then
         Initialize_Root (Target);
      end if;

      Splice_Subtree
        (Target   => Target,
         Parent   => Parent.Node,
         Before   => Before.Node,
         Source   => Source,
         Position => Position.Node);  -- modified during call

      Position.Container := Target'Unrestricted_Access;
   end Splice_Subtree;

   procedure Splice_Subtree
     (Container : in out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      Position  : Cursor)
   is
   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      if Parent.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Parent cursor not in container";
      end if;

      if Before /= No_Element then
         if Before.Container /= Container'Unrestricted_Access then
            raise Program_Error with "Before cursor not in container";
         end if;

         if Container.Nodes (Before.Node).Parent /= Parent.Node then
            raise Constraint_Error with "Before cursor not child of Parent";
         end if;
      end if;

      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor not in container";
      end if;

      if Is_Root (Position) then

         --  Should this be PE instead?  Need ARG confirmation.  ???

         raise Constraint_Error with "Position cursor designates root";
      end if;

      if Container.Nodes (Position.Node).Parent = Parent.Node then
         if Before = No_Element then
            if Container.Nodes (Position.Node).Next <= 0 then  -- last child
               return;
            end if;

         elsif Position.Node = Before.Node then
            return;

         elsif Container.Nodes (Position.Node).Next = Before.Node then
            return;
         end if;
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Is_Reachable (Container => Container,
                       From      => Parent.Node,
                       To        => Position.Node)
      then
         raise Constraint_Error with "Position is ancestor of Parent";
      end if;

      Remove_Subtree (Container, Position.Node);
      Container.Nodes (Position.Node).Parent := Parent.Node;
      Insert_Subtree_Node (Container, Position.Node, Parent.Node, Before.Node);
   end Splice_Subtree;

   procedure Splice_Subtree
     (Target   : in out Tree;
      Parent   : Count_Type;
      Before   : Count_Type'Base;
      Source   : in out Tree;
      Position : in out Count_Type)  -- Source on input, Target on output
   is
      Source_Count : Count_Type := Subtree_Node_Count (Source, Position);
      pragma Assert (Source_Count >= 1);

      Target_Subtree : Count_Type;
      Target_Count   : Count_Type;

   begin
      --  This is a utility operation to do the heavy lifting associated with
      --  splicing a subtree from one tree to another. Note that "splicing"
      --  is a bit of a misnomer here in the case of a bounded tree, because
      --  the elements must be copied from the source to the target.

      if Target.Count > Target.Capacity - Source_Count then
         raise Capacity_Error  -- ???
           with "Source count exceeds available storage on Target";
      end if;

      --  Copy_Subtree returns a count of the number of nodes it inserts, but
      --  it does this by incrementing the value passed in. Therefore we must
      --  initialize the count before calling Copy_Subtree.

      Target_Count := 0;

      Copy_Subtree
        (Source         => Source,
         Source_Subtree => Position,
         Target         => Target,
         Target_Parent  => Parent,
         Target_Subtree => Target_Subtree,
         Count          => Target_Count);

      pragma Assert (Target_Count = Source_Count);

      --  Now link the newly-allocated subtree into the target.

      Insert_Subtree_Node
        (Container => Target,
         Subtree   => Target_Subtree,
         Parent    => Parent,
         Before    => Before);

      Target.Count := Target.Count + Target_Count;

      --  The manipulation of the Target container is complete. Now we remove
      --  the subtree from the Source container.

      Remove_Subtree (Source, Position);  -- unlink the subtree

      --  As with Copy_Subtree, operation Deallocate_Subtree returns a count of
      --  the number of nodes it deallocates, but it works by incrementing the
      --  value passed in. We must therefore initialize the count before
      --  calling it.

      Source_Count := 0;

      Deallocate_Subtree (Source, Position, Source_Count);
      pragma Assert (Source_Count = Target_Count);

      Source.Count := Source.Count - Source_Count;

      Position := Target_Subtree;
   end Splice_Subtree;

   ------------------------
   -- Subtree_Node_Count --
   ------------------------

   function Subtree_Node_Count (Position : Cursor) return Count_Type is
   begin
      if Position = No_Element then
         return 0;
      end if;

      if Position.Container.Count = 0 then
         pragma Assert (Is_Root (Position));
         return 1;
      end if;

      return Subtree_Node_Count (Position.Container.all, Position.Node);
   end Subtree_Node_Count;

   function Subtree_Node_Count
     (Container : Tree;
      Subtree   : Count_Type) return Count_Type
   is
      Result : Count_Type;
      Node   : Count_Type'Base;

   begin
      Result := 1;
      Node := Container.Nodes (Subtree).Children.First;
      while Node > 0 loop
         Result := Result + Subtree_Node_Count (Container, Node);
         Node := Container.Nodes (Node).Next;
      end loop;
      return Result;
   end Subtree_Node_Count;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out Tree;
      I, J      : Cursor)
   is
   begin
      if I = No_Element then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if I.Container /= Container'Unrestricted_Access then
         raise Program_Error with "I cursor not in container";
      end if;

      if Is_Root (I) then
         raise Program_Error with "I cursor designates root";
      end if;

      if I = J then -- make this test sooner???
         return;
      end if;

      if J = No_Element then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if J.Container /= Container'Unrestricted_Access then
         raise Program_Error with "J cursor not in container";
      end if;

      if Is_Root (J) then
         raise Program_Error with "J cursor designates root";
      end if;

      if Container.Lock > 0 then
         raise Program_Error
           with "attempt to tamper with elements (tree is locked)";
      end if;

      declare
         EE : Element_Array renames Container.Elements;
         EI : constant Element_Type := EE (I.Node);

      begin
         EE (I.Node) := EE (J.Node);
         EE (J.Node) := EI;
      end;
   end Swap;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Tree;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type))
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor not in container";
      end if;

      if Is_Root (Position) then
         raise Program_Error with "Position cursor designates root";
      end if;

      declare
         T : Tree renames Position.Container.all'Unrestricted_Access.all;
         B : Natural renames T.Busy;
         L : Natural renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         Process (Element => T.Elements (Position.Node));

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Tree)
   is
      procedure Write_Children (Subtree : Count_Type);
      procedure Write_Subtree (Subtree : Count_Type);

      --------------------
      -- Write_Children --
      --------------------

      procedure Write_Children (Subtree : Count_Type) is
         CC : Children_Type renames Container.Nodes (Subtree).Children;
         C  : Count_Type'Base;

      begin
         Count_Type'Write (Stream, Child_Count (Container, Subtree));

         C := CC.First;
         while C > 0 loop
            Write_Subtree (C);
            C := Container.Nodes (C).Next;
         end loop;
      end Write_Children;

      -------------------
      -- Write_Subtree --
      -------------------

      procedure Write_Subtree (Subtree : Count_Type) is
      begin
         Element_Type'Write (Stream, Container.Elements (Subtree));
         Write_Children (Subtree);
      end Write_Subtree;

   --  Start of processing for Write

   begin
      Count_Type'Write (Stream, Container.Count);

      if Container.Count = 0 then
         return;
      end if;

      Write_Children (Root_Node (Container));
   end Write;

   procedure Write
     (Stream   : not null access Root_Stream_Type'Class;
      Position : Cursor)
   is
   begin
      raise Program_Error with "attempt to write tree cursor to stream";
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

end Ada.Containers.Bounded_Multiway_Trees;
