------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                   ADA.CONTAINERS.INDEFINITE_MULTIWAY_TREES               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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
with System;  use type System.Address;

package body Ada.Containers.Indefinite_Multiway_Trees is

   type Iterator is new Tree_Iterator_Interfaces.Forward_Iterator with
   record
      Container : Tree_Access;
      Position  : Cursor;
      From_Root : Boolean;
   end record;

   type Child_Iterator is new Tree_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Tree_Access;
      Position  : Cursor;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next
     (Object : Iterator;
      Position : Cursor) return Cursor;

   overriding function First (Object : Child_Iterator) return Cursor;
   overriding function Next
     (Object : Child_Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object : Child_Iterator;
      Position : Cursor) return Cursor;

   overriding function Last (Object : Child_Iterator) return Cursor;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Root_Node (Container : Tree) return Tree_Node_Access;

   procedure Free_Element is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Deallocate_Node (X : in out Tree_Node_Access);

   procedure Deallocate_Children
     (Subtree : Tree_Node_Access;
      Count   : in out Count_Type);

   procedure Deallocate_Subtree
     (Subtree : in out Tree_Node_Access;
      Count   : in out Count_Type);

   function Equal_Children
     (Left_Subtree, Right_Subtree : Tree_Node_Access) return Boolean;

   function Equal_Subtree
     (Left_Subtree, Right_Subtree : Tree_Node_Access) return Boolean;

   procedure Iterate_Children
     (Container : Tree_Access;
      Subtree   : Tree_Node_Access;
      Process   : not null access procedure (Position : Cursor));

   procedure Iterate_Subtree
     (Container : Tree_Access;
      Subtree   : Tree_Node_Access;
      Process   : not null access procedure (Position : Cursor));

   procedure Copy_Children
     (Source : Children_Type;
      Parent : Tree_Node_Access;
      Count  : in out Count_Type);

   procedure Copy_Subtree
     (Source : Tree_Node_Access;
      Parent : Tree_Node_Access;
      Target : out Tree_Node_Access;
      Count  : in out Count_Type);

   function Find_In_Children
     (Subtree : Tree_Node_Access;
      Item    : Element_Type) return Tree_Node_Access;

   function Find_In_Subtree
     (Subtree : Tree_Node_Access;
      Item    : Element_Type) return Tree_Node_Access;

   function Child_Count (Children : Children_Type) return Count_Type;

   function Subtree_Node_Count
     (Subtree : Tree_Node_Access) return Count_Type;

   function Is_Reachable (From, To : Tree_Node_Access) return Boolean;

   procedure Remove_Subtree (Subtree : Tree_Node_Access);

   procedure Insert_Subtree_Node
     (Subtree : Tree_Node_Access;
      Parent  : Tree_Node_Access;
      Before  : Tree_Node_Access);

   procedure Insert_Subtree_List
     (First  : Tree_Node_Access;
      Last   : Tree_Node_Access;
      Parent : Tree_Node_Access;
      Before : Tree_Node_Access);

   procedure Splice_Children
     (Target_Parent : Tree_Node_Access;
      Before        : Tree_Node_Access;
      Source_Parent : Tree_Node_Access);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Tree) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Equal_Children (Root_Node (Left), Root_Node (Right));
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Tree) is
      Source       : constant Children_Type := Container.Root.Children;
      Source_Count : constant Count_Type := Container.Count;
      Target_Count : Count_Type;

   begin
      --  We first restore the target container to its default-initialized
      --  state, before we attempt any allocation, to ensure that invariants
      --  are preserved in the event that the allocation fails.

      Container.Root.Children := Children_Type'(others => null);
      Container.Busy := 0;
      Container.Lock := 0;
      Container.Count := 0;

      --  Copy_Children returns a count of the number of nodes that it
      --  allocates, but it works by incrementing the value that is passed in.
      --  We must therefore initialize the count value before calling
      --  Copy_Children.

      Target_Count := 0;

      --  Now we attempt the allocation of subtrees. The invariants are
      --  satisfied even if the allocation fails.

      Copy_Children (Source, Root_Node (Container), Target_Count);
      pragma Assert (Target_Count = Source_Count);

      Container.Count := Source_Count;
   end Adjust;

   -------------------
   -- Ancestor_Find --
   -------------------

   function Ancestor_Find
     (Position : Cursor;
      Item     : Element_Type) return Cursor
   is
      R, N : Tree_Node_Access;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      --  Commented-out pending ARG ruling.  ???

      --  if Position.Container /= Container'Unrestricted_Access then
      --     raise Program_Error with "Position cursor not in container";
      --  end if;

      --  AI-0136 says to raise PE if Position equals the root node. This does
      --  not seem correct, as this value is just the limiting condition of the
      --  search. For now we omit this check pending a ruling from the ARG.???

      --  if Is_Root (Position) then
      --     raise Program_Error with "Position cursor designates root";
      --  end if;

      R := Root_Node (Position.Container.all);
      N := Position.Node;
      while N /= R loop
         if N.Element.all = Item then
            return Cursor'(Position.Container, N);
         end if;

         N := N.Parent;
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
      First, Last : Tree_Node_Access;
      Element     : Element_Access;

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

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      Element := new Element_Type'(New_Item);
      First := new Tree_Node_Type'(Parent  => Parent.Node,
                                   Element => Element,
                                   others  => <>);

      Last := First;

      for J in Count_Type'(2) .. Count loop

         --  Reclaim other nodes if Storage_Error.  ???

         Element := new Element_Type'(New_Item);
         Last.Next := new Tree_Node_Type'(Parent  => Parent.Node,
                                          Prev    => Last,
                                          Element => Element,
                                          others  => <>);

         Last := Last.Next;
      end loop;

      Insert_Subtree_List
        (First  => First,
         Last   => Last,
         Parent => Parent.Node,
         Before => null);  -- null means "insert at end of list"

      --  In order for operation Node_Count to complete in O(1) time, we cache
      --  the count value. Here we increment the total count by the number of
      --  nodes we just inserted.

      Container.Count := Container.Count + Count;
   end Append_Child;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Tree; Source : Tree) is
      Source_Count : constant Count_Type := Source.Count;
      Target_Count : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Target.Clear;  -- checks busy bit

      --  Copy_Children returns the number of nodes that it allocates, but it
      --  does this by incrementing the count value passed in, so we must
      --  initialize the count before calling Copy_Children.

      Target_Count := 0;

      --  Note that Copy_Children inserts the newly-allocated children into
      --  their parent list only after the allocation of all the children has
      --  succeeded. This preserves invariants even if the allocation fails.

      Copy_Children (Source.Root.Children, Root_Node (Target), Target_Count);
      pragma Assert (Target_Count = Source_Count);

      Target.Count := Source_Count;
   end Assign;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Parent : Cursor) return Count_Type is
   begin
      if Parent = No_Element then
         return 0;
      else
         return Child_Count (Parent.Node.Children);
      end if;
   end Child_Count;

   function Child_Count (Children : Children_Type) return Count_Type is
      Result : Count_Type;
      Node   : Tree_Node_Access;

   begin
      Result := 0;
      Node := Children.First;
      while Node /= null loop
         Result := Result + 1;
         Node := Node.Next;
      end loop;

      return Result;
   end Child_Count;

   -----------------
   -- Child_Depth --
   -----------------

   function Child_Depth (Parent, Child : Cursor) return Count_Type is
      Result : Count_Type;
      N      : Tree_Node_Access;

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

      Result := 0;
      N := Child.Node;
      while N /= Parent.Node loop
         Result := Result + 1;
         N := N.Parent;

         if N = null then
            raise Program_Error with "Parent is not ancestor of Child";
         end if;
      end loop;

      return Result;
   end Child_Depth;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Tree) is
      Container_Count : Count_Type;
      Children_Count  : Count_Type;

   begin
      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      --  We first set the container count to 0, in order to preserve
      --  invariants in case the deallocation fails. (This works because
      --  Deallocate_Children immediately removes the children from their
      --  parent, and then does the actual deallocation.)

      Container_Count := Container.Count;
      Container.Count := 0;

      --  Deallocate_Children returns the number of nodes that it deallocates,
      --  but it does this by incrementing the count value that is passed in,
      --  so we must first initialize the count return value before calling it.

      Children_Count := 0;

      --  See comment above. Deallocate_Children immediately removes the
      --  children list from their parent node (here, the root of the tree),
      --  and only after that does it attempt the actual deallocation. So even
      --  if the deallocation fails, the representation invariants

      Deallocate_Children (Root_Node (Container), Children_Count);
      pragma Assert (Children_Count = Container_Count);
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

   function Copy (Source : Tree) return Tree is
   begin
      return Target : Tree do
         Copy_Children
           (Source => Source.Root.Children,
            Parent => Root_Node (Target),
            Count  => Target.Count);

         pragma Assert (Target.Count = Source.Count);
      end return;
   end Copy;

   -------------------
   -- Copy_Children --
   -------------------

   procedure Copy_Children
     (Source : Children_Type;
      Parent : Tree_Node_Access;
      Count  : in out Count_Type)
   is
      pragma Assert (Parent /= null);
      pragma Assert (Parent.Children.First = null);
      pragma Assert (Parent.Children.Last = null);

      CC : Children_Type;
      C  : Tree_Node_Access;

   begin
      --  We special-case the first allocation, in order to establish the
      --  representation invariants for type Children_Type.

      C := Source.First;

      if C = null then
         return;
      end if;

      Copy_Subtree
        (Source => C,
         Parent => Parent,
         Target => CC.First,
         Count  => Count);

      CC.Last := CC.First;

      --  The representation invariants for the Children_Type list have been
      --  established, so we can now copy the remaining children of Source.

      C := C.Next;
      while C /= null loop
         Copy_Subtree
           (Source => C,
            Parent => Parent,
            Target => CC.Last.Next,
            Count  => Count);

         CC.Last.Next.Prev := CC.Last;
         CC.Last := CC.Last.Next;

         C := C.Next;
      end loop;

      --  We add the newly-allocated children to their parent list only after
      --  the allocation has succeeded, in order to preserve invariants of the
      --  parent.

      Parent.Children := CC;
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
      Target_Subtree : Tree_Node_Access;
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

         if Before.Node.Parent /= Parent.Node then
            raise Constraint_Error with "Before cursor not child of Parent";
         end if;
      end if;

      if Source = No_Element then
         return;
      end if;

      if Is_Root (Source) then
         raise Constraint_Error with "Source cursor designates root";
      end if;

      --  Copy_Subtree returns a count of the number of nodes that it
      --  allocates, but it works by incrementing the value that is passed in.
      --  We must therefore initialize the count value before calling
      --  Copy_Subtree.

      Target_Count := 0;

      Copy_Subtree
        (Source => Source.Node,
         Parent => Parent.Node,
         Target => Target_Subtree,
         Count  => Target_Count);

      pragma Assert (Target_Subtree /= null);
      pragma Assert (Target_Subtree.Parent = Parent.Node);
      pragma Assert (Target_Count >= 1);

      Insert_Subtree_Node
        (Subtree => Target_Subtree,
         Parent  => Parent.Node,
         Before  => Before.Node);

      --  In order for operation Node_Count to complete in O(1) time, we cache
      --  the count value. Here we increment the total count by the number of
      --  nodes we just inserted.

      Target.Count := Target.Count + Target_Count;
   end Copy_Subtree;

   procedure Copy_Subtree
     (Source : Tree_Node_Access;
      Parent : Tree_Node_Access;
      Target : out Tree_Node_Access;
      Count  : in out Count_Type)
   is
      E : constant Element_Access := new Element_Type'(Source.Element.all);

   begin
      Target := new Tree_Node_Type'(Element => E,
                                    Parent  => Parent,
                                    others  => <>);

      Count := Count + 1;

      Copy_Children
        (Source => Source.Children,
         Parent => Target,
         Count  => Count);
   end Copy_Subtree;

   -------------------------
   -- Deallocate_Children --
   -------------------------

   procedure Deallocate_Children
     (Subtree : Tree_Node_Access;
      Count   : in out Count_Type)
   is
      pragma Assert (Subtree /= null);

      CC : Children_Type := Subtree.Children;
      C  : Tree_Node_Access;

   begin
      --  We immediately remove the children from their parent, in order to
      --  preserve invariants in case the deallocation fails.

      Subtree.Children := Children_Type'(others => null);

      while CC.First /= null loop
         C := CC.First;
         CC.First := C.Next;

         Deallocate_Subtree (C, Count);
      end loop;
   end Deallocate_Children;

   ---------------------
   -- Deallocate_Node --
   ---------------------

   procedure Deallocate_Node (X : in out Tree_Node_Access) is
      procedure Free_Node is
         new Ada.Unchecked_Deallocation (Tree_Node_Type, Tree_Node_Access);

   --  Start of processing for Deallocate_Node

   begin
      if X /= null then
         Free_Element (X.Element);
         Free_Node (X);
      end if;
   end Deallocate_Node;

   ------------------------
   -- Deallocate_Subtree --
   ------------------------

   procedure Deallocate_Subtree
     (Subtree : in out Tree_Node_Access;
      Count   : in out Count_Type)
   is
   begin
      Deallocate_Children (Subtree, Count);
      Deallocate_Node (Subtree);
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

      --  Deallocate_Children returns a count of the number of nodes
      --  that it deallocates, but it works by incrementing the
      --  value that is passed in. We must therefore initialize
      --  the count value before calling Deallocate_Children.

      Count := 0;

      Deallocate_Children (Parent.Node, Count);
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
      X : Tree_Node_Access;

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

      --  Restore represention invariants before attempting the actual
      --  deallocation.

      Remove_Subtree (X);
      Container.Count := Container.Count - 1;

      --  It is now safe to attempt the deallocation. This leaf node has been
      --  disassociated from the tree, so even if the deallocation fails,
      --  representation invariants will remain satisfied.

      Deallocate_Node (X);
   end Delete_Leaf;

   --------------------
   -- Delete_Subtree --
   --------------------

   procedure Delete_Subtree
     (Container : in out Tree;
      Position  : in out Cursor)
   is
      X     : Tree_Node_Access;
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

      --  Here is one case where a deallocation failure can result in the
      --  violation of a representation invariant. We disassociate the subtree
      --  from the tree now, but we only decrement the total node count after
      --  we attempt the deallocation. However, if the deallocation fails, the
      --  total node count will not get decremented.

      --  One way around this dilemma is to count the nodes in the subtree
      --  before attempt to delete the subtree, but that is an O(n) operation,
      --  so it does not seem worth it.

      --  Perhaps this is much ado about nothing, since the only way
      --  deallocation can fail is if Controlled Finalization fails: this
      --  propagates Program_Error so all bets are off anyway. ???

      Remove_Subtree (X);

      --  Deallocate_Subtree returns a count of the number of nodes that it
      --  deallocates, but it works by incrementing the value that is passed
      --  in. We must therefore initialize the count value before calling
      --  Deallocate_Subtree.

      Count := 0;

      Deallocate_Subtree (X, Count);
      pragma Assert (Count <= Container.Count);

      --  See comments above. We would prefer to do this sooner, but there's no
      --  way to satisfy that goal without an potentially severe execution
      --  penalty.

      Container.Count := Container.Count - Count;
   end Delete_Subtree;

   -----------
   -- Depth --
   -----------

   function Depth (Position : Cursor) return Count_Type is
      Result : Count_Type;
      N      : Tree_Node_Access;

   begin
      Result := 0;
      N := Position.Node;
      while N /= null loop
         N := N.Parent;
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

      return Position.Node.Element.all;
   end Element;

   --------------------
   -- Equal_Children --
   --------------------

   function Equal_Children
     (Left_Subtree  : Tree_Node_Access;
      Right_Subtree : Tree_Node_Access) return Boolean
   is
      Left_Children  : Children_Type renames Left_Subtree.Children;
      Right_Children : Children_Type renames Right_Subtree.Children;

      L, R : Tree_Node_Access;

   begin
      if Child_Count (Left_Children) /= Child_Count (Right_Children) then
         return False;
      end if;

      L := Left_Children.First;
      R := Right_Children.First;
      while L /= null loop
         if not Equal_Subtree (L, R) then
            return False;
         end if;

         L := L.Next;
         R := R.Next;
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

         return Equal_Children (Left_Position.Node, Right_Position.Node);
      end if;

      if Is_Root (Right_Position) then
         return False;
      end if;

      return Equal_Subtree (Left_Position.Node, Right_Position.Node);
   end Equal_Subtree;

   function Equal_Subtree
     (Left_Subtree  : Tree_Node_Access;
      Right_Subtree : Tree_Node_Access) return Boolean
   is
   begin
      if Left_Subtree.Element.all /= Right_Subtree.Element.all then
         return False;
      end if;

      return Equal_Children (Left_Subtree, Right_Subtree);
   end Equal_Subtree;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Tree;
      Item      : Element_Type) return Cursor
   is
      N : constant Tree_Node_Access :=
            Find_In_Children (Root_Node (Container), Item);

   begin
      if N = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, N);
   end Find;

   -----------
   -- First --
   -----------

   function First (Object : Iterator) return Cursor is
   begin
      return Object.Position;
   end First;

   function First (Object : Child_Iterator) return Cursor is
   begin
      return (Object.Container, Object.Position.Node.Children.First);
   end First;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Parent : Cursor) return Cursor is
      Node : Tree_Node_Access;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      Node := Parent.Node.Children.First;

      if Node = null then
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
     (Subtree : Tree_Node_Access;
      Item    : Element_Type) return Tree_Node_Access
   is
      N, Result : Tree_Node_Access;

   begin
      N := Subtree.Children.First;
      while N /= null loop
         Result := Find_In_Subtree (N, Item);

         if Result /= null then
            return Result;
         end if;

         N := N.Next;
      end loop;

      return null;
   end Find_In_Children;

   ---------------------
   -- Find_In_Subtree --
   ---------------------

   function Find_In_Subtree
     (Position : Cursor;
      Item     : Element_Type) return Cursor
   is
      Result : Tree_Node_Access;

   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      --  Commented-out pending ruling from ARG.  ???

      --  if Position.Container /= Container'Unrestricted_Access then
      --     raise Program_Error with "Position cursor not in container";
      --  end if;

      if Is_Root (Position) then
         Result := Find_In_Children (Position.Node, Item);

      else
         Result := Find_In_Subtree (Position.Node, Item);
      end if;

      if Result = null then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Result);
   end Find_In_Subtree;

   function Find_In_Subtree
     (Subtree : Tree_Node_Access;
      Item    : Element_Type) return Tree_Node_Access
   is
   begin
      if Subtree.Element.all = Item then
         return Subtree;
      end if;

      return Find_In_Children (Subtree, Item);
   end Find_In_Subtree;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position = No_Element then
         return False;
      end if;

      return Position.Node.Parent /= null;
   end Has_Element;

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
      Last    : Tree_Node_Access;
      Element : Element_Access;

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

         if Before.Node.Parent /= Parent.Node then
            raise Constraint_Error with "Parent cursor not parent of Before";
         end if;
      end if;

      if Count = 0 then
         Position := No_Element;  -- Need ruling from ARG ???
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      Position.Container := Parent.Container;

      Element := new Element_Type'(New_Item);
      Position.Node := new Tree_Node_Type'(Parent  => Parent.Node,
                                           Element => Element,
                                           others  => <>);

      Last := Position.Node;

      for J in Count_Type'(2) .. Count loop
         --  Reclaim other nodes if Storage_Error.  ???

         Element := new Element_Type'(New_Item);
         Last.Next := new Tree_Node_Type'(Parent  => Parent.Node,
                                          Prev    => Last,
                                          Element => Element,
                                          others  => <>);

         Last := Last.Next;
      end loop;

      Insert_Subtree_List
        (First  => Position.Node,
         Last   => Last,
         Parent => Parent.Node,
         Before => Before.Node);

      --  In order for operation Node_Count to complete in O(1) time, we cache
      --  the count value. Here we increment the total count by the number of
      --  nodes we just inserted.

      Container.Count := Container.Count + Count;
   end Insert_Child;

   -------------------------
   -- Insert_Subtree_List --
   -------------------------

   procedure Insert_Subtree_List
     (First  : Tree_Node_Access;
      Last   : Tree_Node_Access;
      Parent : Tree_Node_Access;
      Before : Tree_Node_Access)
   is
      pragma Assert (Parent /= null);
      C : Children_Type renames Parent.Children;

   begin
      --  This is a simple utility operation to insert a list of nodes (from
      --  First..Last) as children of Parent. The Before node specifies where
      --  the new children should be inserted relative to the existing
      --  children.

      if First = null then
         pragma Assert (Last = null);
         return;
      end if;

      pragma Assert (Last /= null);
      pragma Assert (Before = null or else Before.Parent = Parent);

      if C.First = null then
         C.First := First;
         C.First.Prev := null;
         C.Last := Last;
         C.Last.Next := null;

      elsif Before = null then  -- means "insert after existing nodes"
         C.Last.Next := First;
         First.Prev := C.Last;
         C.Last := Last;
         C.Last.Next := null;

      elsif Before = C.First then
         Last.Next := C.First;
         C.First.Prev := Last;
         C.First := First;
         C.First.Prev := null;

      else
         Before.Prev.Next := First;
         First.Prev := Before.Prev;
         Last.Next := Before;
         Before.Prev := Last;
      end if;
   end Insert_Subtree_List;

   -------------------------
   -- Insert_Subtree_Node --
   -------------------------

   procedure Insert_Subtree_Node
     (Subtree : Tree_Node_Access;
      Parent  : Tree_Node_Access;
      Before  : Tree_Node_Access)
   is
   begin
      --  This is a simple wrapper operation to insert a single child into the
      --  Parent's children list.

      Insert_Subtree_List
        (First  => Subtree,
         Last   => Subtree,
         Parent => Parent,
         Before => Before);
   end Insert_Subtree_Node;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
   begin
      return Container.Root.Children.First = null;
   end Is_Empty;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Position : Cursor) return Boolean is
   begin
      if Position = No_Element then
         return False;
      end if;

      return Position.Node.Children.First = null;
   end Is_Leaf;

   ------------------
   -- Is_Reachable --
   ------------------

   function Is_Reachable (From, To : Tree_Node_Access) return Boolean is
      pragma Assert (From /= null);
      pragma Assert (To /= null);

      N : Tree_Node_Access;

   begin
      N := From;
      while N /= null loop
         if N = To then
            return True;
         end if;

         N := N.Parent;
      end loop;

      return False;
   end Is_Reachable;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      return Position = Root (Position.Container.all);
   end Is_Root;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor))
   is
      T : Tree renames Container'Unrestricted_Access.all;
      B : Integer renames T.Busy;

   begin
      B := B + 1;

      Iterate_Children
        (Container => Container'Unrestricted_Access,
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
      Root_Cursor : constant Cursor :=
                      (Container'Unrestricted_Access, Root_Node (Container));
   begin
      return
        Iterator'(Container'Unrestricted_Access,
                  First_Child (Root_Cursor),
                  From_Root => True);
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

      declare
         B : Integer renames Parent.Container.Busy;
         C : Tree_Node_Access;

      begin
         B := B + 1;

         C := Parent.Node.Children.First;
         while C /= null loop
            Process (Position => Cursor'(Parent.Container, Node => C));
            C := C.Next;
         end loop;

         B := B - 1;

      exception
         when others =>
            B := B - 1;
            raise;
      end;
   end Iterate_Children;

   procedure Iterate_Children
     (Container : Tree_Access;
      Subtree   : Tree_Node_Access;
      Process   : not null access procedure (Position : Cursor))
   is
      Node : Tree_Node_Access;

   begin
      --  This is a helper function to recursively iterate over all the nodes
      --  in a subtree, in depth-first fashion. This particular helper just
      --  visits the children of this subtree, not the root of the subtree node
      --  itself. This is useful when starting from the ultimate root of the
      --  entire tree (see Iterate), as that root does not have an element.

      Node := Subtree.Children.First;
      while Node /= null loop
         Iterate_Subtree (Container, Node, Process);
         Node := Node.Next;
      end loop;
   end Iterate_Children;

   function Iterate_Children
     (Container : Tree;
      Parent    : Cursor)
     return Tree_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
   begin
      return Child_Iterator'(Parent.Container, Parent);
   end Iterate_Children;

   ---------------------
   -- Iterate_Subtree --
   ---------------------

   function Iterate_Subtree
     (Position : Cursor)
      return Tree_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      return Iterator'(Position.Container, Position, From_Root => False);
   end Iterate_Subtree;

   procedure Iterate_Subtree
     (Position  : Cursor;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      declare
         B : Integer renames Position.Container.Busy;

      begin
         B := B + 1;

         if Is_Root (Position) then
            Iterate_Children (Position.Container, Position.Node, Process);
         else
            Iterate_Subtree (Position.Container, Position.Node, Process);
         end if;

         B := B - 1;

      exception
         when others =>
            B := B - 1;
            raise;
      end;
   end Iterate_Subtree;

   procedure Iterate_Subtree
     (Container : Tree_Access;
      Subtree   : Tree_Node_Access;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      --  This is a helper function to recursively iterate over all the nodes
      --  in a subtree, in depth-first fashion. It first visits the root of the
      --  subtree, then visits its children.

      Process (Cursor'(Container, Subtree));
      Iterate_Children (Container, Subtree, Process);
   end Iterate_Subtree;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Child_Iterator) return Cursor is
   begin
      return (Object.Container, Object.Position.Node.Children.Last);
   end Last;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (Parent : Cursor) return Cursor is
      Node : Tree_Node_Access;

   begin
      if Parent = No_Element then
         raise Constraint_Error with "Parent cursor has no element";
      end if;

      Node := Parent.Node.Children.Last;

      if Node = null then
         return No_Element;
      end if;

      return (Parent.Container, Node);
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
      Node : Tree_Node_Access;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors of Source (tree is busy)";
      end if;

      Target.Clear;  -- checks busy bit

      Target.Root.Children := Source.Root.Children;
      Source.Root.Children := Children_Type'(others => null);

      Node := Target.Root.Children.First;
      while Node /= null loop
         Node.Parent := Root_Node (Target);
         Node := Node.Next;
      end loop;

      Target.Count := Source.Count;
      Source.Count := 0;
   end Move;

   ----------
   -- Next --
   ----------

   function Next
     (Object : Iterator;
      Position : Cursor) return Cursor
   is
      T  : Tree renames Position.Container.all;
      N  : constant Tree_Node_Access := Position.Node;

   begin
      if Is_Leaf (Position) then

         --  If sibling is present, return it

         if N.Next /= null then
            return (Object.Container, N.Next);

         --  If this is the last sibling, go to sibling of first ancestor that
         --  has a sibling, or terminate.

         else
            declare
               Par : Tree_Node_Access := N.Parent;

            begin
               while Par.Next = null loop

                  --  If we are back at the root the iteration is complete

                  if Par = Root_Node (T)  then
                     return No_Element;

                  --  If this is a subtree iterator and we are back at the
                  --  starting node, iteration is complete.

                  elsif Par = Object.Position.Node
                    and then not Object.From_Root
                  then
                     return No_Element;

                  else
                     Par := Par.Parent;
                  end if;
               end loop;

               if Par = Object.Position.Node
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
     (Object : Child_Iterator;
      Position : Cursor) return Cursor
   is
      C : constant Tree_Node_Access := Position.Node.Next;

   begin
      if C = null then
         return No_Element;

      else
         return (Object.Container, C);
      end if;
   end Next;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Node.Next = null then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Position.Node.Next);
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

      if Position.Node.Parent = null then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Position.Node.Parent);
   end Parent;

   -------------------
   -- Prepent_Child --
   -------------------

   procedure Prepend_Child
     (Container : in out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      First, Last : Tree_Node_Access;
      Element     : Element_Access;

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

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      Element := new Element_Type'(New_Item);
      First := new Tree_Node_Type'(Parent  => Parent.Node,
                                   Element => Element,
                                   others  => <>);

      Last := First;

      for J in Count_Type'(2) .. Count loop

         --  Reclaim other nodes if Storage_Error.  ???

         Element := new Element_Type'(New_Item);
         Last.Next := new Tree_Node_Type'(Parent  => Parent.Node,
                                          Prev    => Last,
                                          Element => Element,
                                          others  => <>);

         Last := Last.Next;
      end loop;

      Insert_Subtree_List
        (First  => First,
         Last   => Last,
         Parent => Parent.Node,
         Before => Parent.Node.Children.First);

      --  In order for operation Node_Count to complete in O(1) time, we cache
      --  the count value. Here we increment the total count by the number of
      --  nodes we just inserted.

      Container.Count := Container.Count + Count;
   end Prepend_Child;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object : Child_Iterator;
      Position : Cursor) return Cursor
   is
      C : constant Tree_Node_Access := Position.Node.Prev;

   begin
      if C = null then
         return No_Element;

      else
         return (Object.Container, C);
      end if;
   end Previous;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         return No_Element;
      end if;

      if Position.Node.Prev = null then
         return No_Element;
      end if;

      return Cursor'(Position.Container, Position.Node.Prev);
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
         B : Integer renames T.Busy;
         L : Integer renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         Process (Position.Node.Element.all);

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
      procedure Read_Children (Subtree : Tree_Node_Access);

      function Read_Subtree
        (Parent : Tree_Node_Access) return Tree_Node_Access;

      Total_Count : Count_Type'Base;
      --  Value read from the stream that says how many elements follow

      Read_Count : Count_Type'Base;
      --  Actual number of elements read from the stream

      -------------------
      -- Read_Children --
      -------------------

      procedure Read_Children (Subtree : Tree_Node_Access) is
         pragma Assert (Subtree /= null);
         pragma Assert (Subtree.Children.First = null);
         pragma Assert (Subtree.Children.Last = null);

         Count : Count_Type'Base;
         --  Number of child subtrees

         C : Children_Type;

      begin
         Count_Type'Read (Stream, Count);

         if Count < 0 then
            raise Program_Error with "attempt to read from corrupt stream";
         end if;

         if Count = 0 then
            return;
         end if;

         C.First := Read_Subtree (Parent => Subtree);
         C.Last := C.First;

         for J in Count_Type'(2) .. Count loop
            C.Last.Next := Read_Subtree (Parent => Subtree);
            C.Last.Next.Prev := C.Last;
            C.Last := C.Last.Next;
         end loop;

         --  Now that the allocation and reads have completed successfully, it
         --  is safe to link the children to their parent.

         Subtree.Children := C;
      end Read_Children;

      ------------------
      -- Read_Subtree --
      ------------------

      function Read_Subtree
        (Parent : Tree_Node_Access) return Tree_Node_Access
      is
         Element : constant Element_Access :=
                     new Element_Type'(Element_Type'Input (Stream));

         Subtree : constant Tree_Node_Access :=
                     new Tree_Node_Type'
                           (Parent  => Parent,
                            Element => Element,
                            others  => <>);

      begin
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
   begin
      pragma Unreferenced (Container);

      return (Element => Position.Node.Element.all'Unchecked_Access);
   end Constant_Reference;

   function Reference
     (Container : aliased Tree;
      Position  : Cursor) return Reference_Type
   is
   begin
      pragma Unreferenced (Container);

      return (Element => Position.Node.Element.all'Unchecked_Access);
   end Reference;

   --------------------
   -- Remove_Subtree --
   --------------------

   procedure Remove_Subtree (Subtree : Tree_Node_Access) is
      C : Children_Type renames Subtree.Parent.Children;

   begin
      --  This is a utility operation to remove a subtree node from its
      --  parent's list of children.

      if C.First = Subtree then
         pragma Assert (Subtree.Prev = null);

         if C.Last = Subtree then
            pragma Assert (Subtree.Next = null);
            C.First := null;
            C.Last := null;

         else
            C.First := Subtree.Next;
            C.First.Prev := null;
         end if;

      elsif C.Last = Subtree then
         pragma Assert (Subtree.Next = null);
         C.Last := Subtree.Prev;
         C.Last.Next := null;

      else
         Subtree.Prev.Next := Subtree.Next;
         Subtree.Next.Prev := Subtree.Prev;
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
      E, X : Element_Access;

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

      E := new Element_Type'(New_Item);

      X := Position.Node.Element;
      Position.Node.Element := E;

      Free_Element (X);
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

      declare
         B : Integer renames Parent.Container.Busy;
         C : Tree_Node_Access;

      begin
         B := B + 1;

         C := Parent.Node.Children.Last;
         while C /= null loop
            Process (Position => Cursor'(Parent.Container, Node => C));
            C := C.Prev;
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

   function Root_Node (Container : Tree) return Tree_Node_Access is
   begin
      return Container.Root'Unrestricted_Access;
   end Root_Node;

   ---------------------
   -- Splice_Children --
   ---------------------

   procedure Splice_Children
     (Target          : in out Tree;
      Target_Parent   : Cursor;
      Before          : Cursor;
      Source          : in out Tree;
      Source_Parent   : Cursor)
   is
      Count : Count_Type;

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

         if Before.Node.Parent /= Target_Parent.Node then
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

      if Target'Address = Source'Address then
         if Target_Parent = Source_Parent then
            return;
         end if;

         if Target.Busy > 0 then
            raise Program_Error
              with "attempt to tamper with cursors (Target tree is busy)";
         end if;

         if Is_Reachable (From => Target_Parent.Node,
                          To   => Source_Parent.Node)
         then
            raise Constraint_Error
              with "Source_Parent is ancestor of Target_Parent";
         end if;

         Splice_Children
           (Target_Parent => Target_Parent.Node,
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

      --  We cache the count of the nodes we have allocated, so that operation
      --  Node_Count can execute in O(1) time. But that means we must count the
      --  nodes in the subtree we remove from Source and insert into Target, in
      --  order to keep the count accurate.

      Count := Subtree_Node_Count (Source_Parent.Node);
      pragma Assert (Count >= 1);

      Count := Count - 1;  -- because Source_Parent node does not move

      Splice_Children
        (Target_Parent => Target_Parent.Node,
         Before        => Before.Node,
         Source_Parent => Source_Parent.Node);

      Source.Count := Source.Count - Count;
      Target.Count := Target.Count + Count;
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

         if Before.Node.Parent /= Target_Parent.Node then
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

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Is_Reachable (From => Target_Parent.Node,
                       To   => Source_Parent.Node)
      then
         raise Constraint_Error
           with "Source_Parent is ancestor of Target_Parent";
      end if;

      Splice_Children
        (Target_Parent => Target_Parent.Node,
         Before        => Before.Node,
         Source_Parent => Source_Parent.Node);
   end Splice_Children;

   procedure Splice_Children
     (Target_Parent : Tree_Node_Access;
      Before        : Tree_Node_Access;
      Source_Parent : Tree_Node_Access)
   is
      CC : constant Children_Type := Source_Parent.Children;
      C  : Tree_Node_Access;

   begin
      --  This is a utility operation to remove the children from Source parent
      --  and insert them into Target parent.

      Source_Parent.Children := Children_Type'(others => null);

      --  Fix up the Parent pointers of each child to designate its new Target
      --  parent.

      C := CC.First;
      while C /= null loop
         C.Parent := Target_Parent;
         C := C.Next;
      end loop;

      Insert_Subtree_List
        (First  => CC.First,
         Last   => CC.Last,
         Parent => Target_Parent,
         Before => Before);
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
      Subtree_Count : Count_Type;

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

         if Before.Node.Parent /= Parent.Node then
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
         if Position.Node.Parent = Parent.Node then
            if Position.Node = Before.Node then
               return;
            end if;

            if Position.Node.Next = Before.Node then
               return;
            end if;
         end if;

         if Target.Busy > 0 then
            raise Program_Error
              with "attempt to tamper with cursors (Target tree is busy)";
         end if;

         if Is_Reachable (From => Parent.Node, To => Position.Node) then
            raise Constraint_Error with "Position is ancestor of Parent";
         end if;

         Remove_Subtree (Position.Node);

         Position.Node.Parent := Parent.Node;
         Insert_Subtree_Node (Position.Node, Parent.Node, Before.Node);

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

      --  This is an unfortunate feature of this API: we must count the nodes
      --  in the subtree that we remove from the source tree, which is an O(n)
      --  operation. It would have been better if the Tree container did not
      --  have a Node_Count selector; a user that wants the number of nodes in
      --  the tree could simply call Subtree_Node_Count, with the understanding
      --  that such an operation is O(n).
      --
      --  Of course, we could choose to implement the Node_Count selector as an
      --  O(n) operation, which would turn this splice operation into an O(1)
      --  operation. ???

      Subtree_Count := Subtree_Node_Count (Position.Node);
      pragma Assert (Subtree_Count <= Source.Count);

      Remove_Subtree (Position.Node);
      Source.Count := Source.Count - Subtree_Count;

      Position.Node.Parent := Parent.Node;
      Insert_Subtree_Node (Position.Node, Parent.Node, Before.Node);

      Target.Count := Target.Count + Subtree_Count;

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

         if Before.Node.Parent /= Parent.Node then
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

      if Position.Node.Parent = Parent.Node then
         if Position.Node = Before.Node then
            return;
         end if;

         if Position.Node.Next = Before.Node then
            return;
         end if;
      end if;

      if Container.Busy > 0 then
         raise Program_Error
           with "attempt to tamper with cursors (tree is busy)";
      end if;

      if Is_Reachable (From => Parent.Node, To => Position.Node) then
         raise Constraint_Error with "Position is ancestor of Parent";
      end if;

      Remove_Subtree (Position.Node);

      Position.Node.Parent := Parent.Node;
      Insert_Subtree_Node (Position.Node, Parent.Node, Before.Node);
   end Splice_Subtree;

   ------------------------
   -- Subtree_Node_Count --
   ------------------------

   function Subtree_Node_Count (Position : Cursor) return Count_Type is
   begin
      if Position = No_Element then
         return 0;
      end if;

      return Subtree_Node_Count (Position.Node);
   end Subtree_Node_Count;

   function Subtree_Node_Count
     (Subtree : Tree_Node_Access) return Count_Type
   is
      Result : Count_Type;
      Node   : Tree_Node_Access;

   begin
      Result := 1;
      Node := Subtree.Children.First;
      while Node /= null loop
         Result := Result + Subtree_Node_Count (Node);
         Node := Node.Next;
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
         EI : constant Element_Access := I.Node.Element;

      begin
         I.Node.Element := J.Node.Element;
         J.Node.Element := EI;
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
         B : Integer renames T.Busy;
         L : Integer renames T.Lock;

      begin
         B := B + 1;
         L := L + 1;

         Process (Position.Node.Element.all);

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
      procedure Write_Children (Subtree : Tree_Node_Access);
      procedure Write_Subtree (Subtree : Tree_Node_Access);

      --------------------
      -- Write_Children --
      --------------------

      procedure Write_Children (Subtree : Tree_Node_Access) is
         CC : Children_Type renames Subtree.Children;
         C  : Tree_Node_Access;

      begin
         Count_Type'Write (Stream, Child_Count (CC));

         C := CC.First;
         while C /= null loop
            Write_Subtree (C);
            C := C.Next;
         end loop;
      end Write_Children;

      -------------------
      -- Write_Subtree --
      -------------------

      procedure Write_Subtree (Subtree : Tree_Node_Access) is
      begin
         Element_Type'Output (Stream, Subtree.Element.all);
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

end Ada.Containers.Indefinite_Multiway_Trees;
