------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .     --
--                          G E N E R I C _ K E Y S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

package body Ada.Containers.Red_Black_Trees.Generic_Keys is

   package Ops renames Tree_Operations;

   -------------
   -- Ceiling --
   -------------

   --  AKA Lower_Bound

   function Ceiling (Tree : Tree_Type; Key : Key_Type) return Node_Access is
      Y : Node_Access;
      X : Node_Access;

   begin
      X := Tree.Root;
      while X /= null loop
         if Is_Greater_Key_Node (Key, X) then
            X := Ops.Right (X);
         else
            Y := X;
            X := Ops.Left (X);
         end if;
      end loop;

      return Y;
   end Ceiling;

   ----------
   -- Find --
   ----------

   function Find (Tree : Tree_Type; Key  : Key_Type) return Node_Access is
      Y : Node_Access;
      X : Node_Access;

   begin
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
         return null;
      end if;

      if Is_Less_Key_Node (Key, Y) then
         return null;
      end if;

      return Y;
   end Find;

   -----------
   -- Floor --
   -----------

   function Floor (Tree : Tree_Type; Key  : Key_Type) return Node_Access is
      Y : Node_Access;
      X : Node_Access;

   begin
      X := Tree.Root;
      while X /= null loop
         if Is_Less_Key_Node (Key, X) then
            X := Ops.Left (X);
         else
            Y := X;
            X := Ops.Right (X);
         end if;
      end loop;

      return Y;
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
      Y : Node_Access := null;
      X : Node_Access := Tree.Root;

   begin
      Inserted := True;
      while X /= null loop
         Y := X;
         Inserted := Is_Less_Key_Node (Key, X);

         if Inserted then
            X := Ops.Left (X);
         else
            X := Ops.Right (X);
         end if;
      end loop;

      --  If Inserted is True, then this means either that Tree is
      --  empty, or there was a least one node (strictly) greater than
      --  Key. Otherwise, it means that Key is equal to or greater than
      --  every node.

      if Inserted then
         if Y = Tree.First then
            Insert_Post (Tree, Y, True, Node);
            return;
         end if;

         Node := Ops.Previous (Y);

      else
         Node := Y;
      end if;

      --  Here Node has a value that is less than or equal to Key. We
      --  now have to resolve whether Key is equal to or greater than
      --  Node, which determines whether the insertion succeeds.

      if Is_Greater_Key_Node (Key, Node) then
         Insert_Post (Tree, Y, Inserted, Node);
         Inserted := True;
         return;
      end if;

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
   begin
      --  The purpose of a hint is to avoid a search from the root of
      --  tree. If we have it hint it means we only need to traverse the
      --  subtree rooted at the hint to find the nearest neighbor. Note
      --  that finding the neighbor means merely walking the tree; this
      --  is not a search and the only comparisons that occur are with
      --  the hint and its neighbor.

      --  If Position is null, this is intepreted to mean that Key is
      --  large relative to the nodes in the tree. If the tree is empty,
      --  or Key is greater than the last node in the tree, then we're
      --  done; otherwise the hint was "wrong" and we must search.

      if Position = null then  -- largest
         if Tree.Last = null
           or else Is_Greater_Key_Node (Key, Tree.Last)
         then
            Insert_Post (Tree, Tree.Last, False, Node);
            Inserted := True;
         else
            Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
         end if;

         return;
      end if;

      pragma Assert (Tree.Length > 0);

      --  A hint can either name the node that immediately follows Key,
      --  or immediately precedes Key. We first test whether Key is is
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

      if Is_Less_Key_Node (Key, Position) then
         declare
            Before : constant Node_Access := Ops.Previous (Position);

         begin
            if Before = null then
               Insert_Post (Tree, Tree.First, True, Node);
               Inserted := True;

            elsif Is_Greater_Key_Node (Key, Before) then
               if Ops.Right (Before) = null then
                  Insert_Post (Tree, Before, False, Node);
               else
                  Insert_Post (Tree, Position, True, Node);
               end if;

               Inserted := True;

            else
               Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
            end if;
         end;

         return;
      end if;

      --  We know that Key isn't less than the hint so we try again,
      --  this time to see if it's greater than the hint. If so we
      --  compare Key to the node that follows the hint. If Key is both
      --  greater than the hint and less than the hint's next neighbor,
      --  then we're done; otherwise we must search.

      if Is_Greater_Key_Node (Key, Position) then
         declare
            After : constant Node_Access := Ops.Next (Position);

         begin
            if After = null then
               Insert_Post (Tree, Tree.Last, False, Node);
               Inserted := True;

            elsif Is_Less_Key_Node (Key, After) then
               if Ops.Right (Position) = null then
                  Insert_Post (Tree, Position, False, Node);
               else
                  Insert_Post (Tree, After, True, Node);
               end if;

               Inserted := True;

            else
               Conditional_Insert_Sans_Hint (Tree, Key, Node, Inserted);
            end if;
         end;

         return;
      end if;

      --  We know that Key is neither less than the hint nor greater
      --  than the hint, and that's the definition of equivalence.
      --  There's nothing else we need to do, since a search would just
      --  reach the same conclusion.

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

         if Before then
            X := Ops.Left (X);
         else
            X := Ops.Right (X);
         end if;
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
      --  less than the hint and and equal to or greater than the hint's
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
