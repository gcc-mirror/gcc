------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_KEYS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2013, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Red_Black_Trees.Generic_Keys is

   package Ops renames Tree_Operations;

   -------------
   -- Ceiling --
   -------------

   --  AKA Lower_Bound

   function Ceiling (Tree : Tree_Type; Key : Key_Type) return Node_Access is
      B : Natural renames Tree'Unrestricted_Access.Busy;
      L : Natural renames Tree'Unrestricted_Access.Lock;

      Y : Node_Access;
      X : Node_Access;

   begin
      --  If the container is empty, return a result immediately, so that we do
      --  not manipulate the tamper bits unnecessarily.

      if Tree.Root = null then
         return null;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B := B + 1;
      L := L + 1;

      X := Tree.Root;
      while X /= null loop
         if Is_Greater_Key_Node (Key, X) then
            X := Ops.Right (X);
         else
            Y := X;
            X := Ops.Left (X);
         end if;
      end loop;

      B := B - 1;
      L := L - 1;

      return Y;

   exception
      when others =>
         B := B - 1;
         L := L - 1;

         raise;
   end Ceiling;

   ----------
   -- Find --
   ----------

   function Find (Tree : Tree_Type; Key : Key_Type) return Node_Access is
      B : Natural renames Tree'Unrestricted_Access.Busy;
      L : Natural renames Tree'Unrestricted_Access.Lock;

      Y : Node_Access;
      X : Node_Access;

      Result : Node_Access;

   begin
      --  If the container is empty, return a result immediately, so that we do
      --  not manipulate the tamper bits unnecessarily.

      if Tree.Root = null then
         return null;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B := B + 1;
      L := L + 1;

      X := Tree.Root;
      while X /= null loop
         if Is_Greater_Key_Node (Key, X) then
            X := Ops.Right (X);
         else
            Y := X;
            X := Ops.Left (X);
         end if;
      end loop;

      if Y = null then
         Result := null;

      elsif Is_Less_Key_Node (Key, Y) then
         Result := null;

      else
         Result := Y;
      end if;

      B := B - 1;
      L := L - 1;

      return Result;

   exception
      when others =>
         B := B - 1;
         L := L - 1;

         raise;
   end Find;

   -----------
   -- Floor --
   -----------

   function Floor (Tree : Tree_Type; Key : Key_Type) return Node_Access is
      B : Natural renames Tree'Unrestricted_Access.Busy;
      L : Natural renames Tree'Unrestricted_Access.Lock;

      Y : Node_Access;
      X : Node_Access;

   begin
      --  If the container is empty, return a result immediately, so that we do
      --  not manipulate the tamper bits unnecessarily.

      if Tree.Root = null then
         return null;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B := B + 1;
      L := L + 1;

      X := Tree.Root;
      while X /= null loop
         if Is_Less_Key_Node (Key, X) then
            X := Ops.Left (X);
         else
            Y := X;
            X := Ops.Right (X);
         end if;
      end loop;

      B := B - 1;
      L := L - 1;

      return Y;

   exception
      when others =>
         B := B - 1;
         L := L - 1;

         raise;
   end Floor;

   --------------------------------
   -- Generic_Conditional_Insert --
   --------------------------------

   procedure Generic_Conditional_Insert
     (Tree     : in out Tree_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean)
   is
      X : Node_Access;
      Y : Node_Access;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B : Natural renames Tree.Busy;
      L : Natural renames Tree.Lock;

      Compare : Boolean;

   begin
      --  This is a "conditional" insertion, meaning that the insertion request
      --  can "fail" in the sense that no new node is created. If the Key is
      --  equivalent to an existing node, then we return the existing node and
      --  Inserted is set to False. Otherwise, we allocate a new node (via
      --  Insert_Post) and Inserted is set to True.

      --  Note that we are testing for equivalence here, not equality. Key must
      --  be strictly less than its next neighbor, and strictly greater than
      --  its previous neighbor, in order for the conditional insertion to
      --  succeed.

      --  Handle insertion into an empty container as a special case, so that
      --  we do not manipulate the tamper bits unnecessarily.

      if Tree.Root = null then
         Insert_Post (Tree, null, True, Node);
         Inserted := True;
         return;
      end if;

      --  We search the tree to find the nearest neighbor of Key, which is
      --  either the smallest node greater than Key (Inserted is True), or the
      --  largest node less or equivalent to Key (Inserted is False).

      begin
         B := B + 1;
         L := L + 1;

         X := Tree.Root;
         Y := null;
         Inserted := True;
         while X /= null loop
            Y := X;
            Inserted := Is_Less_Key_Node (Key, X);
            X := (if Inserted then Ops.Left (X) else Ops.Right (X));
         end loop;

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;

            raise;
      end;

      if Inserted then

         --  Key is less than Y. If Y is the first node in the tree, then there
         --  are no other nodes that we need to search for, and we insert a new
         --  node into the tree.

         if Y = Tree.First then
            Insert_Post (Tree, Y, True, Node);
            return;
         end if;

         --  Y is the next nearest-neighbor of Key. We know that Key is not
         --  equivalent to Y (because Key is strictly less than Y), so we move
         --  to the previous node, the nearest-neighbor just smaller or
         --  equivalent to Key.

         Node := Ops.Previous (Y);

      else
         --  Y is the previous nearest-neighbor of Key. We know that Key is not
         --  less than Y, which means either that Key is equivalent to Y, or
         --  greater than Y.

         Node := Y;
      end if;

      --  Key is equivalent to or greater than Node. We must resolve which is
      --  the case, to determine whether the conditional insertion succeeds.

      begin
         B := B + 1;
         L := L + 1;

         Compare := Is_Greater_Key_Node (Key, Node);

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;

            raise;
      end;

      if Compare then

         --  Key is strictly greater than Node, which means that Key is not
         --  equivalent to Node. In this case, the insertion succeeds, and we
         --  insert a new node into the tree.

         Insert_Post (Tree, Y, Inserted, Node);
         Inserted := True;
         return;
      end if;

      --  Key is equivalent to Node. This is a conditional insertion, so we do
      --  not insert a new node in this case. We return the existing node and
      --  report that no insertion has occurred.

      Inserted := False;
   end Generic_Conditional_Insert;

   ------------------------------------------
   -- Generic_Conditional_Insert_With_Hint --
   ------------------------------------------

   procedure Generic_Conditional_Insert_With_Hint
     (Tree      : in out Tree_Type;
      Position  : Node_Access;
      Key       : Key_Type;
      Node      : out Node_Access;
      Inserted  : out Boolean)
   is
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B : Natural renames Tree.Busy;
      L : Natural renames Tree.Lock;

      Test    : Node_Access;
      Compare : Boolean;

   begin
      --  The purpose of a hint is to avoid a search from the root of
      --  tree. If we have it hint it means we only need to traverse the
      --  subtree rooted at the hint to find the nearest neighbor. Note
      --  that finding the neighbor means merely walking the tree; this
      --  is not a search and the only comparisons that occur are with
      --  the hint and its neighbor.

      --  Handle insertion into an empty container as a special case, so that
      --  we do not manipulate the tamper bits unnecessarily.

      if Tree.Root = null then
         Insert_Post (Tree, null, True, Node);
         Inserted := True;
         return;
      end if;

      --  If Position is null, this is interpreted to mean that Key is large
      --  relative to the nodes in the tree. If Key is greater than the last
      --  node in the tree, then we're done; otherwise the hint was "wrong" and
      --  we must search.

      if Position = null then  -- largest
         begin
            B := B + 1;
            L := L + 1;

            Compare := Is_Greater_Key_Node (Key, Tree.Last);

            L := L - 1;
            B := B - 1;

         exception
            when others =>
               L := L - 1;
               B := B - 1;

               raise;
         end;

         if Compare then
            Insert_Post (Tree, Tree.Last, False, Node);
            Inserted := True;
         else
            Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
         end if;

         return;
      end if;

      pragma Assert (Tree.Length > 0);

      --  A hint can either name the node that immediately follows Key,
      --  or immediately precedes Key. We first test whether Key is
      --  less than the hint, and if so we compare Key to the node that
      --  precedes the hint. If Key is both less than the hint and
      --  greater than the hint's preceding neighbor, then we're done;
      --  otherwise we must search.

      --  Note also that a hint can either be an anterior node or a leaf
      --  node. A new node is always inserted at the bottom of the tree
      --  (at least prior to rebalancing), becoming the new left or
      --  right child of leaf node (which prior to the insertion must
      --  necessarily be null, since this is a leaf). If the hint names
      --  an anterior node then its neighbor must be a leaf, and so
      --  (here) we insert after the neighbor. If the hint names a leaf
      --  then its neighbor must be anterior and so we insert before the
      --  hint.

      begin
         B := B + 1;
         L := L + 1;

         Compare := Is_Less_Key_Node (Key, Position);

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;

            raise;
      end;

      if Compare then
         Test := Ops.Previous (Position);  -- "before"

         if Test = null then  -- new first node
            Insert_Post (Tree, Tree.First, True, Node);

            Inserted := True;
            return;
         end if;

         begin
            B := B + 1;
            L := L + 1;

            Compare := Is_Greater_Key_Node (Key, Test);

            L := L - 1;
            B := B - 1;

         exception
            when others =>
               L := L - 1;
               B := B - 1;

               raise;
         end;

         if Compare then
            if Ops.Right (Test) = null then
               Insert_Post (Tree, Test, False, Node);
            else
               Insert_Post (Tree, Position, True, Node);
            end if;

            Inserted := True;

         else
            Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
         end if;

         return;
      end if;

      --  We know that Key isn't less than the hint so we try again, this time
      --  to see if it's greater than the hint. If so we compare Key to the
      --  node that follows the hint. If Key is both greater than the hint and
      --  less than the hint's next neighbor, then we're done; otherwise we
      --  must search.

      begin
         B := B + 1;
         L := L + 1;

         Compare := Is_Greater_Key_Node (Key, Position);

         L := L - 1;
         B := B - 1;

      exception
         when others =>
            L := L - 1;
            B := B - 1;

            raise;
      end;

      if Compare then
         Test := Ops.Next (Position);  -- "after"

         if Test = null then  -- new last node
            Insert_Post (Tree, Tree.Last, False, Node);

            Inserted := True;
            return;
         end if;

         begin
            B := B + 1;
            L := L + 1;

            Compare := Is_Less_Key_Node (Key, Test);

            L := L - 1;
            B := B - 1;

         exception
            when others =>
               L := L - 1;
               B := B - 1;

               raise;
         end;

         if Compare then
            if Ops.Right (Position) = null then
               Insert_Post (Tree, Position, False, Node);
            else
               Insert_Post (Tree, Test, True, Node);
            end if;

            Inserted := True;

         else
            Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
         end if;

         return;
      end if;

      --  We know that Key is neither less than the hint nor greater than the
      --  hint, and that's the definition of equivalence. There's nothing else
      --  we need to do, since a search would just reach the same conclusion.

      Node := Position;
      Inserted := False;
   end Generic_Conditional_Insert_With_Hint;

   -------------------------
   -- Generic_Insert_Post --
   -------------------------

   procedure Generic_Insert_Post
     (Tree   : in out Tree_Type;
      Y      : Node_Access;
      Before : Boolean;
      Z      : out Node_Access)
   is
   begin
      if Tree.Length = Count_Type'Last then
         raise Constraint_Error with "too many elements";
      end if;

      if Tree.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Z := New_Node;
      pragma Assert (Z /= null);
      pragma Assert (Ops.Color (Z) = Red);

      if Y = null then
         pragma Assert (Tree.Length = 0);
         pragma Assert (Tree.Root = null);
         pragma Assert (Tree.First = null);
         pragma Assert (Tree.Last = null);

         Tree.Root := Z;
         Tree.First := Z;
         Tree.Last := Z;

      elsif Before then
         pragma Assert (Ops.Left (Y) = null);

         Ops.Set_Left (Y, Z);

         if Y = Tree.First then
            Tree.First := Z;
         end if;

      else
         pragma Assert (Ops.Right (Y) = null);

         Ops.Set_Right (Y, Z);

         if Y = Tree.Last then
            Tree.Last := Z;
         end if;
      end if;

      Ops.Set_Parent (Z, Y);
      Ops.Rebalance_For_Insert (Tree, Z);
      Tree.Length := Tree.Length + 1;
   end Generic_Insert_Post;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type)
   is
      procedure Iterate (Node : Node_Access);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Node : Node_Access) is
         N : Node_Access;
      begin
         N := Node;
         while N /= null loop
            if Is_Less_Key_Node (Key, N) then
               N := Ops.Left (N);
            elsif Is_Greater_Key_Node (Key, N) then
               N := Ops.Right (N);
            else
               Iterate (Ops.Left (N));
               Process (N);
               N := Ops.Right (N);
            end if;
         end loop;
      end Iterate;

   --  Start of processing for Generic_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Iteration;

   -------------------------------
   -- Generic_Reverse_Iteration --
   -------------------------------

   procedure Generic_Reverse_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type)
   is
      procedure Iterate (Node : Node_Access);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Node : Node_Access) is
         N : Node_Access;
      begin
         N := Node;
         while N /= null loop
            if Is_Less_Key_Node (Key, N) then
               N := Ops.Left (N);
            elsif Is_Greater_Key_Node (Key, N) then
               N := Ops.Right (N);
            else
               Iterate (Ops.Right (N));
               Process (N);
               N := Ops.Left (N);
            end if;
         end loop;
      end Iterate;

   --  Start of processing for Generic_Reverse_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Reverse_Iteration;

   ----------------------------------
   -- Generic_Unconditional_Insert --
   ----------------------------------

   procedure Generic_Unconditional_Insert
     (Tree : in out Tree_Type;
      Key  : Key_Type;
      Node : out Node_Access)
   is
      Y : Node_Access;
      X : Node_Access;

      Before : Boolean;

   begin
      Y := null;
      Before := False;

      X := Tree.Root;
      while X /= null loop
         Y := X;
         Before := Is_Less_Key_Node (Key, X);
         X := (if Before then Ops.Left (X) else Ops.Right (X));
      end loop;

      Insert_Post (Tree, Y, Before, Node);
   end Generic_Unconditional_Insert;

   --------------------------------------------
   -- Generic_Unconditional_Insert_With_Hint --
   --------------------------------------------

   procedure Generic_Unconditional_Insert_With_Hint
     (Tree : in out Tree_Type;
      Hint : Node_Access;
      Key  : Key_Type;
      Node : out Node_Access)
   is
   begin
      --  There are fewer constraints for an unconditional insertion
      --  than for a conditional insertion, since we allow duplicate
      --  keys. So instead of having to check (say) whether Key is
      --  (strictly) greater than the hint's previous neighbor, here we
      --  allow Key to be equal to or greater than the previous node.

      --  There is the issue of what to do if Key is equivalent to the
      --  hint. Does the new node get inserted before or after the hint?
      --  We decide that it gets inserted after the hint, reasoning that
      --  this is consistent with behavior for non-hint insertion, which
      --  inserts a new node after existing nodes with equivalent keys.

      --  First we check whether the hint is null, which is interpreted
      --  to mean that Key is large relative to existing nodes.
      --  Following our rule above, if Key is equal to or greater than
      --  the last node, then we insert the new node immediately after
      --  last. (We don't have an operation for testing whether a key is
      --  "equal to or greater than" a node, so we must say instead "not
      --  less than", which is equivalent.)

      if Hint = null then  -- largest
         if Tree.Last = null then
            Insert_Post (Tree, null, False, Node);
         elsif Is_Less_Key_Node (Key, Tree.Last) then
            Unconditional_Insert_Sans_Hint (Tree, Key, Node);
         else
            Insert_Post (Tree, Tree.Last, False, Node);
         end if;

         return;
      end if;

      pragma Assert (Tree.Length > 0);

      --  We decide here whether to insert the new node prior to the
      --  hint. Key could be equivalent to the hint, so in theory we
      --  could write the following test as "not greater than" (same as
      --  "less than or equal to"). If Key were equivalent to the hint,
      --  that would mean that the new node gets inserted before an
      --  equivalent node. That wouldn't break any container invariants,
      --  but our rule above says that new nodes always get inserted
      --  after equivalent nodes. So here we test whether Key is both
      --  less than the hint and equal to or greater than the hint's
      --  previous neighbor, and if so insert it before the hint.

      if Is_Less_Key_Node (Key, Hint) then
         declare
            Before : constant Node_Access := Ops.Previous (Hint);
         begin
            if Before = null then
               Insert_Post (Tree, Hint, True, Node);
            elsif Is_Less_Key_Node (Key, Before) then
               Unconditional_Insert_Sans_Hint (Tree, Key, Node);
            elsif Ops.Right (Before) = null then
               Insert_Post (Tree, Before, False, Node);
            else
               Insert_Post (Tree, Hint, True, Node);
            end if;
         end;

         return;
      end if;

      --  We know that Key isn't less than the hint, so it must be equal
      --  or greater. So we just test whether Key is less than or equal
      --  to (same as "not greater than") the hint's next neighbor, and
      --  if so insert it after the hint.

      declare
         After : constant Node_Access := Ops.Next (Hint);
      begin
         if After = null then
            Insert_Post (Tree, Hint, False, Node);
         elsif Is_Greater_Key_Node (Key, After) then
            Unconditional_Insert_Sans_Hint (Tree, Key, Node);
         elsif Ops.Right (Hint) = null then
            Insert_Post (Tree, Hint, False, Node);
         else
            Insert_Post (Tree, After, True, Node);
         end if;
      end;
   end Generic_Unconditional_Insert_With_Hint;

   -----------------
   -- Upper_Bound --
   -----------------

   function Upper_Bound
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access
   is
      Y : Node_Access;
      X : Node_Access;

   begin
      X := Tree.Root;
      while X /= null loop
         if Is_Less_Key_Node (Key, X) then
            Y := X;
            X := Ops.Left (X);
         else
            X := Ops.Right (X);
         end if;
      end loop;

      return Y;
   end Upper_Bound;

end Ada.Containers.Red_Black_Trees.Generic_Keys;
