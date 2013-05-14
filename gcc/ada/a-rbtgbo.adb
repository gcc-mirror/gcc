------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_BOUNDED_OPERATIONS        --
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

--  The references below to "CLR" refer to the following book, from which
--  several of the algorithms here were adapted:
--     Introduction to Algorithms
--     by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest
--     Publisher: The MIT Press (June 18, 1990)
--     ISBN: 0262031418

with System;  use type System.Address;

package body Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Delete_Fixup (Tree : in out Tree_Type'Class; Node : Count_Type);
   procedure Delete_Swap (Tree : in out Tree_Type'Class; Z, Y : Count_Type);

   procedure Left_Rotate  (Tree : in out Tree_Type'Class; X : Count_Type);
   procedure Right_Rotate (Tree : in out Tree_Type'Class; Y : Count_Type);

   ----------------
   -- Clear_Tree --
   ----------------

   procedure Clear_Tree (Tree : in out Tree_Type'Class) is
   begin
      if Tree.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      --  The lock status (which monitors "element tampering") always implies
      --  that the busy status (which monitors "cursor tampering") is set too;
      --  this is a representation invariant. Thus if the busy bit is not set,
      --  then the lock bit must not be set either.

      pragma Assert (Tree.Lock = 0);

      Tree.First  := 0;
      Tree.Last   := 0;
      Tree.Root   := 0;
      Tree.Length := 0;
      Tree.Free   := -1;
   end Clear_Tree;

   ------------------
   -- Delete_Fixup --
   ------------------

   procedure Delete_Fixup
     (Tree : in out Tree_Type'Class;
      Node : Count_Type)
   is
      --  CLR p. 274

      X : Count_Type;
      W : Count_Type;
      N : Nodes_Type renames Tree.Nodes;

   begin
      X := Node;
      while X /= Tree.Root
        and then Color (N (X)) = Black
      loop
         if X = Left (N (Parent (N (X)))) then
            W :=  Right (N (Parent (N (X))));

            if Color (N (W)) = Red then
               Set_Color (N (W), Black);
               Set_Color (N (Parent (N (X))), Red);
               Left_Rotate (Tree, Parent (N (X)));
               W := Right (N (Parent (N (X))));
            end if;

            if (Left (N (W))  = 0 or else Color (N (Left (N (W)))) = Black)
              and then
               (Right (N (W)) = 0 or else Color (N (Right (N (W)))) = Black)
            then
               Set_Color (N (W), Red);
               X := Parent (N (X));

            else
               if Right (N (W)) = 0
                 or else Color (N (Right (N (W)))) = Black
               then
                  --  As a condition for setting the color of the left child to
                  --  black, the left child access value must be non-null. A
                  --  truth table analysis shows that if we arrive here, that
                  --  condition holds, so there's no need for an explicit test.
                  --  The assertion is here to document what we know is true.

                  pragma Assert (Left (N (W)) /= 0);
                  Set_Color (N (Left (N (W))), Black);

                  Set_Color (N (W), Red);
                  Right_Rotate (Tree, W);
                  W := Right (N (Parent (N (X))));
               end if;

               Set_Color (N (W), Color (N (Parent (N (X)))));
               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Right (N (W))), Black);
               Left_Rotate  (Tree, Parent (N (X)));
               X := Tree.Root;
            end if;

         else
            pragma Assert (X = Right (N (Parent (N (X)))));

            W :=  Left (N (Parent (N (X))));

            if Color (N (W)) = Red then
               Set_Color (N (W), Black);
               Set_Color (N (Parent (N (X))), Red);
               Right_Rotate (Tree, Parent (N (X)));
               W := Left (N (Parent (N (X))));
            end if;

            if (Left (N (W))  = 0 or else Color (N (Left (N (W)))) = Black)
                 and then
               (Right (N (W)) = 0 or else Color (N (Right (N (W)))) = Black)
            then
               Set_Color (N (W), Red);
               X := Parent (N (X));

            else
               if Left (N (W)) = 0
                 or else Color (N (Left (N (W)))) = Black
               then
                  --  As a condition for setting the color of the right child
                  --  to black, the right child access value must be non-null.
                  --  A truth table analysis shows that if we arrive here, that
                  --  condition holds, so there's no need for an explicit test.
                  --  The assertion is here to document what we know is true.

                  pragma Assert (Right (N (W)) /= 0);
                  Set_Color (N (Right (N (W))), Black);

                  Set_Color (N (W), Red);
                  Left_Rotate (Tree, W);
                  W := Left (N (Parent (N (X))));
               end if;

               Set_Color (N (W), Color (N (Parent (N (X)))));
               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Left (N (W))), Black);
               Right_Rotate (Tree, Parent (N (X)));
               X := Tree.Root;
            end if;
         end if;
      end loop;

      Set_Color (N (X), Black);
   end Delete_Fixup;

   ---------------------------
   -- Delete_Node_Sans_Free --
   ---------------------------

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type'Class;
      Node : Count_Type)
   is
      --  CLR p. 273

      X, Y : Count_Type;

      Z : constant Count_Type := Node;
      pragma Assert (Z /= 0);

      N : Nodes_Type renames Tree.Nodes;

   begin
      if Tree.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      pragma Assert (Tree.Length > 0);
      pragma Assert (Tree.Root  /= 0);
      pragma Assert (Tree.First /= 0);
      pragma Assert (Tree.Last  /= 0);
      pragma Assert (Parent (N (Tree.Root)) = 0);

      pragma Assert ((Tree.Length > 1)
                        or else (Tree.First = Tree.Last
                                   and then Tree.First = Tree.Root));

      pragma Assert ((Left (N (Node)) = 0)
                        or else (Parent (N (Left (N (Node)))) = Node));

      pragma Assert ((Right (N (Node)) = 0)
                        or else (Parent (N (Right (N (Node)))) = Node));

      pragma Assert (((Parent (N (Node)) = 0) and then (Tree.Root = Node))
                        or else ((Parent (N (Node)) /= 0) and then
                                  ((Left (N (Parent (N (Node)))) = Node)
                                      or else
                                   (Right (N (Parent (N (Node)))) = Node))));

      if Left (N (Z)) = 0 then
         if Right (N (Z)) = 0 then
            if Z = Tree.First then
               Tree.First := Parent (N (Z));
            end if;

            if Z = Tree.Last then
               Tree.Last := Parent (N (Z));
            end if;

            if Color (N (Z)) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (N (Z)) = 0);
            pragma Assert (Right (N (Z)) = 0);

            if Z = Tree.Root then
               pragma Assert (Tree.Length = 1);
               pragma Assert (Parent (N (Z)) = 0);
               Tree.Root := 0;
            elsif Z = Left (N (Parent (N (Z)))) then
               Set_Left (N (Parent (N (Z))), 0);
            else
               pragma Assert (Z = Right (N (Parent (N (Z)))));
               Set_Right (N (Parent (N (Z))), 0);
            end if;

         else
            pragma Assert (Z /= Tree.Last);

            X := Right (N (Z));

            if Z = Tree.First then
               Tree.First := Min (Tree, X);
            end if;

            if Z = Tree.Root then
               Tree.Root := X;
            elsif Z = Left (N (Parent (N (Z)))) then
               Set_Left (N (Parent (N (Z))), X);
            else
               pragma Assert (Z = Right (N (Parent (N (Z)))));
               Set_Right (N (Parent (N (Z))), X);
            end if;

            Set_Parent (N (X), Parent (N (Z)));

            if Color (N (Z)) = Black then
               Delete_Fixup (Tree, X);
            end if;
         end if;

      elsif Right (N (Z)) = 0 then
         pragma Assert (Z /= Tree.First);

         X := Left (N (Z));

         if Z = Tree.Last then
            Tree.Last := Max (Tree, X);
         end if;

         if Z = Tree.Root then
            Tree.Root := X;
         elsif Z = Left (N (Parent (N (Z)))) then
            Set_Left (N (Parent (N (Z))), X);
         else
            pragma Assert (Z = Right (N (Parent (N (Z)))));
            Set_Right (N (Parent (N (Z))), X);
         end if;

         Set_Parent (N (X), Parent (N (Z)));

         if Color (N (Z)) = Black then
            Delete_Fixup (Tree, X);
         end if;

      else
         pragma Assert (Z /= Tree.First);
         pragma Assert (Z /= Tree.Last);

         Y := Next (Tree, Z);
         pragma Assert (Left (N (Y)) = 0);

         X := Right (N (Y));

         if X = 0 then
            if Y = Left (N (Parent (N (Y)))) then
               pragma Assert (Parent (N (Y)) /= Z);
               Delete_Swap (Tree, Z, Y);
               Set_Left (N (Parent (N (Z))), Z);

            else
               pragma Assert (Y = Right (N (Parent (N (Y)))));
               pragma Assert (Parent (N (Y)) = Z);
               Set_Parent (N (Y), Parent (N (Z)));

               if Z = Tree.Root then
                  Tree.Root := Y;
               elsif Z = Left (N (Parent (N (Z)))) then
                  Set_Left (N (Parent (N (Z))), Y);
               else
                  pragma Assert (Z = Right (N (Parent (N (Z)))));
                  Set_Right (N (Parent (N (Z))), Y);
               end if;

               Set_Left   (N (Y), Left (N (Z)));
               Set_Parent (N (Left (N (Y))), Y);
               Set_Right  (N (Y), Z);

               Set_Parent (N (Z), Y);
               Set_Left   (N (Z), 0);
               Set_Right  (N (Z), 0);

               declare
                  Y_Color : constant Color_Type := Color (N (Y));
               begin
                  Set_Color (N (Y), Color (N (Z)));
                  Set_Color (N (Z), Y_Color);
               end;
            end if;

            if Color (N (Z)) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (N (Z)) = 0);
            pragma Assert (Right (N (Z)) = 0);

            if Z = Right (N (Parent (N (Z)))) then
               Set_Right (N (Parent (N (Z))), 0);
            else
               pragma Assert (Z = Left (N (Parent (N (Z)))));
               Set_Left (N (Parent (N (Z))), 0);
            end if;

         else
            if Y = Left (N (Parent (N (Y)))) then
               pragma Assert (Parent (N (Y)) /= Z);

               Delete_Swap (Tree, Z, Y);

               Set_Left (N (Parent (N (Z))), X);
               Set_Parent (N (X), Parent (N (Z)));

            else
               pragma Assert (Y = Right (N (Parent (N (Y)))));
               pragma Assert (Parent (N (Y)) = Z);

               Set_Parent (N (Y), Parent (N (Z)));

               if Z = Tree.Root then
                  Tree.Root := Y;
               elsif Z = Left (N (Parent (N (Z)))) then
                  Set_Left (N (Parent (N (Z))), Y);
               else
                  pragma Assert (Z = Right (N (Parent (N (Z)))));
                  Set_Right (N (Parent (N (Z))), Y);
               end if;

               Set_Left (N (Y), Left (N (Z)));
               Set_Parent (N (Left (N (Y))), Y);

               declare
                  Y_Color : constant Color_Type := Color (N (Y));
               begin
                  Set_Color (N (Y), Color (N (Z)));
                  Set_Color (N (Z), Y_Color);
               end;
            end if;

            if Color (N (Z)) = Black then
               Delete_Fixup (Tree, X);
            end if;
         end if;
      end if;

      Tree.Length := Tree.Length - 1;
   end Delete_Node_Sans_Free;

   -----------------
   -- Delete_Swap --
   -----------------

   procedure Delete_Swap
     (Tree : in out Tree_Type'Class;
      Z, Y : Count_Type)
   is
      N : Nodes_Type renames Tree.Nodes;

      pragma Assert (Z /= Y);
      pragma Assert (Parent (N (Y)) /= Z);

      Y_Parent : constant Count_Type := Parent (N (Y));
      Y_Color  : constant Color_Type := Color (N (Y));

   begin
      Set_Parent (N (Y), Parent (N (Z)));
      Set_Left   (N (Y), Left   (N (Z)));
      Set_Right  (N (Y), Right  (N (Z)));
      Set_Color  (N (Y), Color  (N (Z)));

      if Tree.Root = Z then
         Tree.Root := Y;
      elsif Right (N (Parent (N (Y)))) = Z then
         Set_Right (N (Parent (N (Y))), Y);
      else
         pragma Assert (Left (N (Parent (N (Y)))) = Z);
         Set_Left (N (Parent (N (Y))), Y);
      end if;

      if Right (N (Y)) /= 0 then
         Set_Parent (N (Right (N (Y))), Y);
      end if;

      if Left (N (Y)) /= 0 then
         Set_Parent (N (Left (N (Y))), Y);
      end if;

      Set_Parent (N (Z), Y_Parent);
      Set_Color  (N (Z), Y_Color);
      Set_Left   (N (Z), 0);
      Set_Right  (N (Z), 0);
   end Delete_Swap;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Tree_Type'Class; X : Count_Type) is
      pragma Assert (X > 0);
      pragma Assert (X <= Tree.Capacity);

      N : Nodes_Type renames Tree.Nodes;
      --  pragma Assert (N (X).Prev >= 0);  -- node is active
      --  Find a way to mark a node as active vs. inactive; we could
      --  use a special value in Color_Type for this.  ???

   begin
      --  The set container actually contains two data structures: a list for
      --  the "active" nodes that contain elements that have been inserted
      --  onto the tree, and another for the "inactive" nodes of the free
      --  store.
      --
      --  We desire that merely declaring an object should have only minimal
      --  cost; specially, we want to avoid having to initialize the free
      --  store (to fill in the links), especially if the capacity is large.
      --
      --  The head of the free list is indicated by Container.Free. If its
      --  value is non-negative, then the free store has been initialized
      --  in the "normal" way: Container.Free points to the head of the list
      --  of free (inactive) nodes, and the value 0 means the free list is
      --  empty. Each node on the free list has been initialized to point
      --  to the next free node (via its Parent component), and the value 0
      --  means that this is the last free node.
      --
      --  If Container.Free is negative, then the links on the free store
      --  have not been initialized. In this case the link values are
      --  implied: the free store comprises the components of the node array
      --  started with the absolute value of Container.Free, and continuing
      --  until the end of the array (Nodes'Last).
      --
      --  ???
      --  It might be possible to perform an optimization here. Suppose that
      --  the free store can be represented as having two parts: one
      --  comprising the non-contiguous inactive nodes linked together
      --  in the normal way, and the other comprising the contiguous
      --  inactive nodes (that are not linked together, at the end of the
      --  nodes array). This would allow us to never have to initialize
      --  the free store, except in a lazy way as nodes become inactive.

      --  When an element is deleted from the list container, its node
      --  becomes inactive, and so we set its Prev component to a negative
      --  value, to indicate that it is now inactive. This provides a useful
      --  way to detect a dangling cursor reference.

      --  The comment above is incorrect; we need some other way to
      --  indicate a node is inactive, for example by using a special
      --  Color_Type value.  ???
      --  N (X).Prev := -1;  -- Node is deallocated (not on active list)

      if Tree.Free >= 0 then
         --  The free store has previously been initialized. All we need to
         --  do here is link the newly-free'd node onto the free list.

         Set_Parent (N (X), Tree.Free);
         Tree.Free := X;

      elsif X + 1 = abs Tree.Free then
         --  The free store has not been initialized, and the node becoming
         --  inactive immediately precedes the start of the free store. All
         --  we need to do is move the start of the free store back by one.

         Tree.Free := Tree.Free + 1;

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

         Tree.Free := abs Tree.Free;

         if Tree.Free > Tree.Capacity then
            Tree.Free := 0;

         else
            for I in Tree.Free .. Tree.Capacity - 1 loop
               Set_Parent (N (I), I + 1);
            end loop;

            Set_Parent (N (Tree.Capacity), 0);
         end if;

         Set_Parent (N (X), Tree.Free);
         Tree.Free := X;
      end if;
   end Free;

   -----------------------
   -- Generic_Allocate --
   -----------------------

   procedure Generic_Allocate
     (Tree : in out Tree_Type'Class;
      Node : out Count_Type)
   is
      N : Nodes_Type renames Tree.Nodes;

   begin
      if Tree.Free >= 0 then
         Node := Tree.Free;

         --  We always perform the assignment first, before we
         --  change container state, in order to defend against
         --  exceptions duration assignment.

         Set_Element (N (Node));
         Tree.Free := Parent (N (Node));

      else
         --  A negative free store value means that the links of the nodes
         --  in the free store have not been initialized. In this case, the
         --  nodes are physically contiguous in the array, starting at the
         --  index that is the absolute value of the Container.Free, and
         --  continuing until the end of the array (Nodes'Last).

         Node := abs Tree.Free;

         --  As above, we perform this assignment first, before modifying
         --  any container state.

         Set_Element (N (Node));
         Tree.Free := Tree.Free - 1;
      end if;

      --  When a node is allocated from the free store, its pointer components
      --  (the links to other nodes in the tree) must also be initialized (to
      --  0, the equivalent of null). This simplifies the post-allocation
      --  handling of nodes inserted into terminal positions.

      Set_Parent (N (Node), Parent => 0);
      Set_Left   (N (Node), Left   => 0);
      Set_Right  (N (Node), Right  => 0);
   end Generic_Allocate;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal (Left, Right : Tree_Type'Class) return Boolean is
      BL : Natural renames Left'Unrestricted_Access.Busy;
      LL : Natural renames Left'Unrestricted_Access.Lock;

      BR : Natural renames Right'Unrestricted_Access.Busy;
      LR : Natural renames Right'Unrestricted_Access.Lock;

      L_Node : Count_Type;
      R_Node : Count_Type;

      Result : Boolean;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      --  If the containers are empty, return a result immediately, so as to
      --  not manipulate the tamper bits unnecessarily.

      if Left.Length = 0 then
         return True;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      BL := BL + 1;
      LL := LL + 1;

      BR := BR + 1;
      LR := LR + 1;

      L_Node := Left.First;
      R_Node := Right.First;
      Result := True;
      while L_Node /= 0 loop
         if not Is_Equal (Left.Nodes (L_Node), Right.Nodes (R_Node)) then
            Result := False;
            exit;
         end if;

         L_Node := Next (Left, L_Node);
         R_Node := Next (Right, R_Node);
      end loop;

      BL := BL - 1;
      LL := LL - 1;

      BR := BR - 1;
      LR := LR - 1;

      return Result;

   exception
      when others =>
         BL := BL - 1;
         LL := LL - 1;

         BR := BR - 1;
         LR := LR - 1;

         raise;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (Tree : Tree_Type'Class) is
      procedure Iterate (P : Count_Type);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (P : Count_Type) is
         X : Count_Type := P;
      begin
         while X /= 0 loop
            Iterate (Left (Tree.Nodes (X)));
            Process (X);
            X := Right (Tree.Nodes (X));
         end loop;
      end Iterate;

   --  Start of processing for Generic_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Iteration;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : in out Tree_Type'Class)
   is
      Len : Count_Type'Base;

      Node, Last_Node : Count_Type;

      N : Nodes_Type renames Tree.Nodes;

   begin
      Clear_Tree (Tree);
      Count_Type'Base'Read (Stream, Len);

      if Len < 0 then
         raise Program_Error with "bad container length (corrupt stream)";
      end if;

      if Len = 0 then
         return;
      end if;

      if Len > Tree.Capacity then
         raise Constraint_Error with "length exceeds capacity";
      end if;

      --  Use Unconditional_Insert_With_Hint here instead ???

      Allocate (Tree, Node);
      pragma Assert (Node /= 0);

      Set_Color (N (Node), Black);

      Tree.Root   := Node;
      Tree.First  := Node;
      Tree.Last   := Node;
      Tree.Length := 1;

      for J in Count_Type range 2 .. Len loop
         Last_Node := Node;
         pragma Assert (Last_Node = Tree.Last);

         Allocate (Tree, Node);
         pragma Assert (Node /= 0);

         Set_Color (N (Node), Red);
         Set_Right (N (Last_Node), Right => Node);
         Tree.Last := Node;
         Set_Parent (N (Node), Parent => Last_Node);

         Rebalance_For_Insert (Tree, Node);
         Tree.Length := Tree.Length + 1;
      end loop;
   end Generic_Read;

   -------------------------------
   -- Generic_Reverse_Iteration --
   -------------------------------

   procedure Generic_Reverse_Iteration (Tree : Tree_Type'Class) is
      procedure Iterate (P : Count_Type);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (P : Count_Type) is
         X : Count_Type := P;
      begin
         while X /= 0 loop
            Iterate (Right (Tree.Nodes (X)));
            Process (X);
            X := Left (Tree.Nodes (X));
         end loop;
      end Iterate;

   --  Start of processing for Generic_Reverse_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Reverse_Iteration;

   -------------------
   -- Generic_Write --
   -------------------

   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : Tree_Type'Class)
   is
      procedure Process (Node : Count_Type);
      pragma Inline (Process);

      procedure Iterate is new Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Node : Count_Type) is
      begin
         Write_Node (Stream, Tree.Nodes (Node));
      end Process;

   --  Start of processing for Generic_Write

   begin
      Count_Type'Base'Write (Stream, Tree.Length);
      Iterate (Tree);
   end Generic_Write;

   -----------------
   -- Left_Rotate --
   -----------------

   procedure Left_Rotate (Tree : in out Tree_Type'Class; X : Count_Type) is
      --  CLR p. 266

      N : Nodes_Type renames Tree.Nodes;

      Y : constant Count_Type := Right (N (X));
      pragma Assert (Y /= 0);

   begin
      Set_Right (N (X), Left (N (Y)));

      if Left (N (Y)) /= 0 then
         Set_Parent (N (Left (N (Y))), X);
      end if;

      Set_Parent (N (Y), Parent (N (X)));

      if X = Tree.Root then
         Tree.Root := Y;
      elsif X = Left (N (Parent (N (X)))) then
         Set_Left (N (Parent (N (X))), Y);
      else
         pragma Assert (X = Right (N (Parent (N (X)))));
         Set_Right (N (Parent (N (X))), Y);
      end if;

      Set_Left   (N (Y), X);
      Set_Parent (N (X), Y);
   end Left_Rotate;

   ---------
   -- Max --
   ---------

   function Max
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type
   is
      --  CLR p. 248

      X : Count_Type := Node;
      Y : Count_Type;

   begin
      loop
         Y := Right (Tree.Nodes (X));

         if Y = 0 then
            return X;
         end if;

         X := Y;
      end loop;
   end Max;

   ---------
   -- Min --
   ---------

   function Min
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type
   is
      --  CLR p. 248

      X : Count_Type := Node;
      Y : Count_Type;

   begin
      loop
         Y := Left (Tree.Nodes (X));

         if Y = 0 then
            return X;
         end if;

         X := Y;
      end loop;
   end Min;

   ----------
   -- Next --
   ----------

   function Next
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type
   is
   begin
      --  CLR p. 249

      if Node = 0 then
         return 0;
      end if;

      if Right (Tree.Nodes (Node)) /= 0 then
         return Min (Tree, Right (Tree.Nodes (Node)));
      end if;

      declare
         X : Count_Type := Node;
         Y : Count_Type := Parent (Tree.Nodes (Node));

      begin
         while Y /= 0
           and then X = Right (Tree.Nodes (Y))
         loop
            X := Y;
            Y := Parent (Tree.Nodes (Y));
         end loop;

         return Y;
      end;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type
   is
   begin
      if Node = 0 then
         return 0;
      end if;

      if Left (Tree.Nodes (Node)) /= 0 then
         return Max (Tree, Left (Tree.Nodes (Node)));
      end if;

      declare
         X : Count_Type := Node;
         Y : Count_Type := Parent (Tree.Nodes (Node));

      begin
         while Y /= 0
           and then X = Left (Tree.Nodes (Y))
         loop
            X := Y;
            Y := Parent (Tree.Nodes (Y));
         end loop;

         return Y;
      end;
   end Previous;

   --------------------------
   -- Rebalance_For_Insert --
   --------------------------

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type'Class;
      Node : Count_Type)
   is
      --  CLR p. 268

      N : Nodes_Type renames Tree.Nodes;

      X : Count_Type := Node;
      pragma Assert (X /= 0);
      pragma Assert (Color (N (X)) = Red);

      Y : Count_Type;

   begin
      while X /= Tree.Root and then Color (N (Parent (N (X)))) = Red loop
         if Parent (N (X)) = Left (N (Parent (N (Parent (N (X)))))) then
            Y := Right (N (Parent (N (Parent (N (X))))));

            if Y /= 0 and then Color (N (Y)) = Red then
               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Y), Black);
               Set_Color (N (Parent (N (Parent (N (X))))), Red);
               X := Parent (N (Parent (N (X))));

            else
               if X = Right (N (Parent (N (X)))) then
                  X := Parent (N (X));
                  Left_Rotate (Tree, X);
               end if;

               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Parent (N (Parent (N (X))))), Red);
               Right_Rotate (Tree, Parent (N (Parent (N (X)))));
            end if;

         else
            pragma Assert (Parent (N (X)) =
                             Right (N (Parent (N (Parent (N (X)))))));

            Y := Left (N (Parent (N (Parent (N (X))))));

            if Y /= 0 and then Color (N (Y)) = Red then
               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Y), Black);
               Set_Color (N (Parent (N (Parent (N (X))))), Red);
               X := Parent (N (Parent (N (X))));

            else
               if X = Left (N (Parent (N (X)))) then
                  X := Parent (N (X));
                  Right_Rotate (Tree, X);
               end if;

               Set_Color (N (Parent (N (X))), Black);
               Set_Color (N (Parent (N (Parent (N (X))))), Red);
               Left_Rotate (Tree, Parent (N (Parent (N (X)))));
            end if;
         end if;
      end loop;

      Set_Color (N (Tree.Root), Black);
   end Rebalance_For_Insert;

   ------------------
   -- Right_Rotate --
   ------------------

   procedure Right_Rotate (Tree : in out Tree_Type'Class; Y : Count_Type) is
      N : Nodes_Type renames Tree.Nodes;

      X : constant Count_Type := Left (N (Y));
      pragma Assert (X /= 0);

   begin
      Set_Left (N (Y), Right (N (X)));

      if Right (N (X)) /= 0 then
         Set_Parent (N (Right (N (X))), Y);
      end if;

      Set_Parent (N (X), Parent (N (Y)));

      if Y = Tree.Root then
         Tree.Root := X;
      elsif Y = Left (N (Parent (N (Y)))) then
         Set_Left (N (Parent (N (Y))), X);
      else
         pragma Assert (Y = Right (N (Parent (N (Y)))));
         Set_Right (N (Parent (N (Y))), X);
      end if;

      Set_Right  (N (X), Y);
      Set_Parent (N (Y), X);
   end Right_Rotate;

   ---------
   -- Vet --
   ---------

   function Vet (Tree : Tree_Type'Class; Index : Count_Type) return Boolean is
      Nodes : Nodes_Type renames Tree.Nodes;
      Node  : Node_Type renames Nodes (Index);

   begin
      if Parent (Node) = Index
        or else Left (Node) = Index
        or else Right (Node) = Index
      then
         return False;
      end if;

      if Tree.Length = 0
        or else Tree.Root = 0
        or else Tree.First = 0
        or else Tree.Last = 0
      then
         return False;
      end if;

      if Parent (Nodes (Tree.Root)) /= 0 then
         return False;
      end if;

      if Left (Nodes (Tree.First)) /= 0 then
         return False;
      end if;

      if Right (Nodes (Tree.Last)) /= 0 then
         return False;
      end if;

      if Tree.Length = 1 then
         if Tree.First /= Tree.Last
           or else Tree.First /= Tree.Root
         then
            return False;
         end if;

         if Index /= Tree.First then
            return False;
         end if;

         if Parent (Node) /= 0
           or else Left (Node) /= 0
           or else Right (Node) /= 0
         then
            return False;
         end if;

         return True;
      end if;

      if Tree.First = Tree.Last then
         return False;
      end if;

      if Tree.Length = 2 then
         if Tree.First /= Tree.Root
           and then Tree.Last /= Tree.Root
         then
            return False;
         end if;

         if Tree.First /= Index
           and then Tree.Last /= Index
         then
            return False;
         end if;
      end if;

      if Left (Node) /= 0
        and then Parent (Nodes (Left (Node))) /= Index
      then
         return False;
      end if;

      if Right (Node) /= 0
        and then Parent (Nodes (Right (Node))) /= Index
      then
         return False;
      end if;

      if Parent (Node) = 0 then
         if Tree.Root /= Index then
            return False;
         end if;

      elsif Left (Nodes (Parent (Node))) /= Index
        and then Right (Nodes (Parent (Node))) /= Index
      then
         return False;
      end if;

      return True;
   end Vet;

end Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;
