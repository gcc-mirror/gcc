------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--             ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_OPERATIONS            --
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

--  The references below to "CLR" refer to the following book, from which
--  several of the algorithms here were adapted:
--     Introduction to Algorithms
--     by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest
--     Publisher: The MIT Press (June 18, 1990)
--     ISBN: 0262031418

with System;  use type System.Address;

package body Ada.Containers.Red_Black_Trees.Generic_Operations is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Delete_Fixup (Tree : in out Tree_Type; Node : Node_Access);

   procedure Delete_Swap (Tree : in out Tree_Type; Z, Y : Node_Access);

   procedure Left_Rotate  (Tree : in out Tree_Type; X : Node_Access);
   procedure Right_Rotate (Tree : in out Tree_Type; Y : Node_Access);

--     ---------------------
--     -- Check_Invariant --
--     ---------------------

--     procedure Check_Invariant (Tree : Tree_Type) is
--        Root : constant Node_Access := Tree.Root;
--
--        function Check (Node : Node_Access) return Natural;
--
--        -----------
--        -- Check --
--        -----------
--
--        function Check (Node : Node_Access) return Natural is
--        begin
--           if Node = null then
--              return 0;
--           end if;
--
--           if Color (Node) = Red then
--              declare
--                 L : constant Node_Access := Left (Node);
--              begin
--                 pragma Assert (L = null or else Color (L) = Black);
--                 null;
--              end;
--
--              declare
--                 R : constant Node_Access := Right (Node);
--              begin
--                 pragma Assert (R = null or else Color (R) = Black);
--                 null;
--              end;
--
--              declare
--                 NL : constant Natural := Check (Left (Node));
--                 NR : constant Natural := Check (Right (Node));
--              begin
--                 pragma Assert (NL = NR);
--                 return NL;
--              end;
--           end if;
--
--           declare
--              NL : constant Natural := Check (Left (Node));
--              NR : constant Natural := Check (Right (Node));
--           begin
--              pragma Assert (NL = NR);
--              return NL + 1;
--           end;
--        end Check;
--
--     --  Start of processing for Check_Invariant
--
--     begin
--        if Root = null then
--           pragma Assert (Tree.First = null);
--           pragma Assert (Tree.Last = null);
--           pragma Assert (Tree.Length = 0);
--           null;
--
--        else
--           pragma Assert (Color (Root) = Black);
--           pragma Assert (Tree.Length > 0);
--           pragma Assert (Tree.Root /= null);
--           pragma Assert (Tree.First /= null);
--           pragma Assert (Tree.Last /= null);
--           pragma Assert (Parent (Tree.Root) = null);
--           pragma Assert ((Tree.Length > 1)
--                             or else (Tree.First = Tree.Last
--                                        and Tree.First = Tree.Root));
--           pragma Assert (Left (Tree.First) = null);
--           pragma Assert (Right (Tree.Last) = null);
--
--           declare
--              L  : constant Node_Access := Left (Root);
--              R  : constant Node_Access := Right (Root);
--              NL : constant Natural := Check (L);
--              NR : constant Natural := Check (R);
--           begin
--              pragma Assert (NL = NR);
--              null;
--           end;
--        end if;
--     end Check_Invariant;

   ------------------
   -- Delete_Fixup --
   ------------------

   procedure Delete_Fixup (Tree : in out Tree_Type; Node : Node_Access) is

      --  CLR p274

      X : Node_Access := Node;
      W : Node_Access;

   begin
      while X /= Tree.Root
        and then Color (X) = Black
      loop
         if X = Left (Parent (X)) then
            W :=  Right (Parent (X));

            if Color (W) = Red then
               Set_Color (W, Black);
               Set_Color (Parent (X), Red);
               Left_Rotate (Tree, Parent (X));
               W := Right (Parent (X));
            end if;

            if (Left (W)  = null or else Color (Left (W)) = Black)
              and then
               (Right (W) = null or else Color (Right (W)) = Black)
            then
               Set_Color (W, Red);
               X := Parent (X);

            else
               if Right (W) = null
                 or else Color (Right (W)) = Black
               then
                  pragma Assert (Left (W) /= null);
                  Set_Color (Left (W), Black);
                  Set_Color (W, Red);
                  Right_Rotate (Tree, W);
                  W := Right (Parent (X));
               end if;

               Set_Color (W, Color (Parent (X)));
               Set_Color (Parent (X), Black);
               Set_Color (Right (W), Black);
               Left_Rotate  (Tree, Parent (X));
               X := Tree.Root;
            end if;

         else
            pragma Assert (X = Right (Parent (X)));

            W :=  Left (Parent (X));

            if Color (W) = Red then
               Set_Color (W, Black);
               Set_Color (Parent (X), Red);
               Right_Rotate (Tree, Parent (X));
               W := Left (Parent (X));
            end if;

            if (Left (W)  = null or else Color (Left (W)) = Black)
                  and then
               (Right (W) = null or else Color (Right (W)) = Black)
            then
               Set_Color (W, Red);
               X := Parent (X);

            else
               if Left (W) = null or else Color (Left (W)) = Black then
                  pragma Assert (Right (W) /= null);
                  Set_Color (Right (W), Black);
                  Set_Color (W, Red);
                  Left_Rotate (Tree, W);
                  W := Left (Parent (X));
               end if;

               Set_Color (W, Color (Parent (X)));
               Set_Color (Parent (X), Black);
               Set_Color (Left (W), Black);
               Right_Rotate (Tree, Parent (X));
               X := Tree.Root;
            end if;
         end if;
      end loop;

      Set_Color (X, Black);
   end Delete_Fixup;

   ---------------------------
   -- Delete_Node_Sans_Free --
   ---------------------------

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type;
      Node : Node_Access)
   is
      --  CLR p273

      X, Y : Node_Access;

      Z : constant Node_Access := Node;
      pragma Assert (Z /= null);

   begin
      if Tree.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

--    pragma Assert (Tree.Length > 0);
--    pragma Assert (Tree.Root /= null);
--    pragma Assert (Tree.First /= null);
--    pragma Assert (Tree.Last /= null);
--    pragma Assert (Parent (Tree.Root) = null);
--    pragma Assert ((Tree.Length > 1)
--                      or else (Tree.First = Tree.Last
--                                 and then Tree.First = Tree.Root));
--    pragma Assert ((Left (Node) = null)
--                      or else (Parent (Left (Node)) = Node));
--    pragma Assert ((Right (Node) = null)
--                      or else (Parent (Right (Node)) = Node));
--    pragma Assert (((Parent (Node) = null) and then (Tree.Root = Node))
--                      or else ((Parent (Node) /= null) and then
--                                ((Left (Parent (Node)) = Node)
--                                   or else (Right (Parent (Node)) = Node))));

      if Left (Z) = null then
         if Right (Z) = null then
            if Z = Tree.First then
               Tree.First := Parent (Z);
            end if;

            if Z = Tree.Last then
               Tree.Last := Parent (Z);
            end if;

            if Color (Z) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (Z) = null);
            pragma Assert (Right (Z) = null);

            if Z = Tree.Root then
               pragma Assert (Tree.Length = 1);
               pragma Assert (Parent (Z) = null);
               Tree.Root := null;
            elsif Z = Left (Parent (Z)) then
               Set_Left (Parent (Z), null);
            else
               pragma Assert (Z = Right (Parent (Z)));
               Set_Right (Parent (Z), null);
            end if;

         else
            pragma Assert (Z /= Tree.Last);

            X := Right (Z);

            if Z = Tree.First then
               Tree.First := Min (X);
            end if;

            if Z = Tree.Root then
               Tree.Root := X;
            elsif Z = Left (Parent (Z)) then
               Set_Left (Parent (Z), X);
            else
               pragma Assert (Z = Right (Parent (Z)));
               Set_Right (Parent (Z), X);
            end if;

            Set_Parent (X, Parent (Z));

            if Color (Z) = Black then
               Delete_Fixup (Tree, X);
            end if;
         end if;

      elsif Right (Z) = null then
         pragma Assert (Z /= Tree.First);

         X := Left (Z);

         if Z = Tree.Last then
            Tree.Last := Max (X);
         end if;

         if Z = Tree.Root then
            Tree.Root := X;
         elsif Z = Left (Parent (Z)) then
            Set_Left (Parent (Z), X);
         else
            pragma Assert (Z = Right (Parent (Z)));
            Set_Right (Parent (Z), X);
         end if;

         Set_Parent (X, Parent (Z));

         if Color (Z) = Black then
            Delete_Fixup (Tree, X);
         end if;

      else
         pragma Assert (Z /= Tree.First);
         pragma Assert (Z /= Tree.Last);

         Y := Next (Z);
         pragma Assert (Left (Y) = null);

         X := Right (Y);

         if X = null then
            if Y = Left (Parent (Y)) then
               pragma Assert (Parent (Y) /= Z);
               Delete_Swap (Tree, Z, Y);
               Set_Left (Parent (Z), Z);

            else
               pragma Assert (Y = Right (Parent (Y)));
               pragma Assert (Parent (Y) = Z);
               Set_Parent (Y, Parent (Z));

               if Z = Tree.Root then
                  Tree.Root := Y;
               elsif Z = Left (Parent (Z)) then
                  Set_Left (Parent (Z), Y);
               else
                  pragma Assert (Z = Right (Parent (Z)));
                  Set_Right (Parent (Z), Y);
               end if;

               Set_Left (Y, Left (Z));
               Set_Parent (Left (Y), Y);
               Set_Right (Y, Z);
               Set_Parent (Z, Y);
               Set_Left (Z, null);
               Set_Right (Z, null);

               declare
                  Y_Color : constant Color_Type := Color (Y);
               begin
                  Set_Color (Y, Color (Z));
                  Set_Color (Z, Y_Color);
               end;
            end if;

            if Color (Z) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (Z) = null);
            pragma Assert (Right (Z) = null);

            if Z = Right (Parent (Z)) then
               Set_Right (Parent (Z), null);
            else
               pragma Assert (Z = Left (Parent (Z)));
               Set_Left (Parent (Z), null);
            end if;

         else
            if Y = Left (Parent (Y)) then
               pragma Assert (Parent (Y) /= Z);

               Delete_Swap (Tree, Z, Y);

               Set_Left (Parent (Z), X);
               Set_Parent (X, Parent (Z));

            else
               pragma Assert (Y = Right (Parent (Y)));
               pragma Assert (Parent (Y) = Z);

               Set_Parent (Y, Parent (Z));

               if Z = Tree.Root then
                  Tree.Root := Y;
               elsif Z = Left (Parent (Z)) then
                  Set_Left (Parent (Z), Y);
               else
                  pragma Assert (Z = Right (Parent (Z)));
                  Set_Right (Parent (Z), Y);
               end if;

               Set_Left (Y, Left (Z));
               Set_Parent (Left (Y), Y);

               declare
                  Y_Color : constant Color_Type := Color (Y);
               begin
                  Set_Color (Y, Color (Z));
                  Set_Color (Z, Y_Color);
               end;
            end if;

            if Color (Z) = Black then
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
     (Tree : in out Tree_Type;
      Z, Y : Node_Access)
   is
      pragma Assert (Z /= Y);
      pragma Assert (Parent (Y) /= Z);

      Y_Parent : constant Node_Access := Parent (Y);
      Y_Color  : constant Color_Type  := Color (Y);

   begin
      Set_Parent (Y, Parent (Z));
      Set_Left (Y, Left (Z));
      Set_Right (Y, Right (Z));
      Set_Color (Y, Color (Z));

      if Tree.Root = Z then
         Tree.Root := Y;
      elsif Right (Parent (Y)) = Z then
         Set_Right (Parent (Y), Y);
      else
         pragma Assert (Left (Parent (Y)) = Z);
         Set_Left (Parent (Y), Y);
      end if;

      if Right (Y) /= null then
         Set_Parent (Right (Y), Y);
      end if;

      if Left (Y) /= null then
         Set_Parent (Left (Y), Y);
      end if;

      Set_Parent (Z, Y_Parent);
      Set_Color (Z, Y_Color);
      Set_Left (Z, null);
      Set_Right (Z, null);
   end Delete_Swap;

   --------------------
   -- Generic_Adjust --
   --------------------

   procedure Generic_Adjust (Tree : in out Tree_Type) is
      N    : constant Count_Type := Tree.Length;
      Root : constant Node_Access := Tree.Root;

   begin
      if N = 0 then
         pragma Assert (Root = null);
         pragma Assert (Tree.Busy = 0);
         pragma Assert (Tree.Lock = 0);
         return;
      end if;

      Tree.Root := null;
      Tree.First := null;
      Tree.Last := null;
      Tree.Length := 0;

      Tree.Root := Copy_Tree (Root);
      Tree.First := Min (Tree.Root);
      Tree.Last := Max (Tree.Root);
      Tree.Length := N;
   end Generic_Adjust;

   -------------------
   -- Generic_Clear --
   -------------------

   procedure Generic_Clear (Tree : in out Tree_Type) is
      Root : Node_Access := Tree.Root;
   begin
      if Tree.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Tree := (First  => null,
               Last   => null,
               Root   => null,
               Length => 0,
               Busy   => 0,
               Lock   => 0);

      Delete_Tree (Root);
   end Generic_Clear;

   -----------------------
   -- Generic_Copy_Tree --
   -----------------------

   function Generic_Copy_Tree (Source_Root : Node_Access) return Node_Access is
      Target_Root : Node_Access := Copy_Node (Source_Root);
      P, X        : Node_Access;

   begin
      if Right (Source_Root) /= null then
         Set_Right
           (Node  => Target_Root,
            Right => Generic_Copy_Tree (Right (Source_Root)));

         Set_Parent
           (Node   => Right (Target_Root),
            Parent => Target_Root);
      end if;

      P := Target_Root;

      X := Left (Source_Root);
      while X /= null loop
         declare
            Y : constant Node_Access := Copy_Node (X);
         begin
            Set_Left (Node => P, Left => Y);
            Set_Parent (Node => Y, Parent => P);

            if Right (X) /= null then
               Set_Right
                 (Node  => Y,
                  Right => Generic_Copy_Tree (Right (X)));

               Set_Parent
                 (Node   => Right (Y),
                  Parent => Y);
            end if;

            P := Y;
            X := Left (X);
         end;
      end loop;

      return Target_Root;
   exception
      when others =>
         Delete_Tree (Target_Root);
         raise;
   end Generic_Copy_Tree;

   -------------------------
   -- Generic_Delete_Tree --
   -------------------------

   procedure Generic_Delete_Tree (X : in out Node_Access) is
      Y : Node_Access;
      pragma Warnings (Off, Y);
   begin
      while X /= null loop
         Y := Right (X);
         Generic_Delete_Tree (Y);
         Y := Left (X);
         Free (X);
         X := Y;
      end loop;
   end Generic_Delete_Tree;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal (Left, Right : Tree_Type) return Boolean is
      L_Node : Node_Access;
      R_Node : Node_Access;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      L_Node := Left.First;
      R_Node := Right.First;
      while L_Node /= null loop
         if not Is_Equal (L_Node, R_Node) then
            return False;
         end if;

         L_Node := Next (L_Node);
         R_Node := Next (R_Node);
      end loop;

      return True;
   end Generic_Equal;

   -----------------------
   -- Generic_Iteration --
   -----------------------

   procedure Generic_Iteration (Tree : Tree_Type) is
      procedure Iterate (P : Node_Access);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (P : Node_Access) is
         X : Node_Access := P;
      begin
         while X /= null loop
            Iterate (Left (X));
            Process (X);
            X := Right (X);
         end loop;
      end Iterate;

   --  Start of processing for Generic_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Iteration;

   ------------------
   -- Generic_Move --
   ------------------

   procedure Generic_Move (Target, Source : in out Tree_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Clear (Target);

      Target := Source;

      Source := (First  => null,
                 Last   => null,
                 Root   => null,
                 Length => 0,
                 Busy   => 0,
                 Lock   => 0);
   end Generic_Move;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : in out Tree_Type)
   is
      N : Count_Type'Base;

      Node, Last_Node : Node_Access;

   begin
      Clear (Tree);

      Count_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      if N = 0 then
         return;
      end if;

      Node := Read_Node (Stream);
      pragma Assert (Node /= null);
      pragma Assert (Color (Node) = Red);

      Set_Color (Node, Black);

      Tree.Root := Node;
      Tree.First := Node;
      Tree.Last := Node;

      Tree.Length := 1;

      for J in Count_Type range 2 .. N loop
         Last_Node := Node;
         pragma Assert (Last_Node = Tree.Last);

         Node := Read_Node (Stream);
         pragma Assert (Node /= null);
         pragma Assert (Color (Node) = Red);

         Set_Right (Node => Last_Node, Right => Node);
         Tree.Last := Node;
         Set_Parent (Node => Node, Parent => Last_Node);
         Rebalance_For_Insert (Tree, Node);
         Tree.Length := Tree.Length + 1;
      end loop;
   end Generic_Read;

   -------------------------------
   -- Generic_Reverse_Iteration --
   -------------------------------

   procedure Generic_Reverse_Iteration (Tree : Tree_Type)
   is
      procedure Iterate (P : Node_Access);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (P : Node_Access) is
         X : Node_Access := P;
      begin
         while X /= null loop
            Iterate (Right (X));
            Process (X);
            X := Left (X);
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
      Tree   : Tree_Type)
   is
      procedure Process (Node : Node_Access);
      pragma Inline (Process);

      procedure Iterate is
         new Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Node : Node_Access) is
      begin
         Write_Node (Stream, Node);
      end Process;

   --  Start of processing for Generic_Write

   begin
      Count_Type'Base'Write (Stream, Tree.Length);
      Iterate (Tree);
   end Generic_Write;

   -----------------
   -- Left_Rotate --
   -----------------

   procedure Left_Rotate (Tree : in out Tree_Type; X : Node_Access) is

      --  CLR p266

      Y : constant Node_Access := Right (X);
      pragma Assert (Y /= null);

   begin
      Set_Right (X, Left (Y));

      if Left (Y) /= null then
         Set_Parent (Left (Y), X);
      end if;

      Set_Parent (Y, Parent (X));

      if X = Tree.Root then
         Tree.Root := Y;
      elsif X = Left (Parent (X)) then
         Set_Left (Parent (X), Y);
      else
         pragma Assert (X = Right (Parent (X)));
         Set_Right (Parent (X), Y);
      end if;

      Set_Left (Y, X);
      Set_Parent (X, Y);
   end Left_Rotate;

   ---------
   -- Max --
   ---------

   function Max (Node : Node_Access) return Node_Access is

      --  CLR p248

      X : Node_Access := Node;
      Y : Node_Access;

   begin
      loop
         Y := Right (X);

         if Y = null then
            return X;
         end if;

         X := Y;
      end loop;
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Node : Node_Access) return Node_Access is

      --  CLR p248

      X : Node_Access := Node;
      Y : Node_Access;

   begin
      loop
         Y := Left (X);

         if Y = null then
            return X;
         end if;

         X := Y;
      end loop;
   end Min;

   ----------
   -- Next --
   ----------

   function Next (Node : Node_Access) return Node_Access is
   begin
      --  CLR p249

      if Node = null then
         return null;
      end if;

      if Right (Node) /= null then
         return Min (Right (Node));
      end if;

      declare
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);

      begin
         while Y /= null
           and then X = Right (Y)
         loop
            X := Y;
            Y := Parent (Y);
         end loop;

         return Y;
      end;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Node : Node_Access) return Node_Access is
   begin
      if Node = null then
         return null;
      end if;

      if Left (Node) /= null then
         return Max (Left (Node));
      end if;

      declare
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);

      begin
         while Y /= null
           and then X = Left (Y)
         loop
            X := Y;
            Y := Parent (Y);
         end loop;

         return Y;
      end;
   end Previous;

   --------------------------
   -- Rebalance_For_Insert --
   --------------------------

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type;
      Node : Node_Access)
   is
      --  CLR p.268

      X : Node_Access := Node;
      pragma Assert (X /= null);
      pragma Assert (Color (X) = Red);

      Y : Node_Access;

   begin
      while X /= Tree.Root and then Color (Parent (X)) = Red loop
         if Parent (X) = Left (Parent (Parent (X))) then
            Y := Right (Parent (Parent (X)));

            if Y /= null and then Color (Y) = Red then
               Set_Color (Parent (X), Black);
               Set_Color (Y, Black);
               Set_Color (Parent (Parent (X)), Red);
               X := Parent (Parent (X));

            else
               if X = Right (Parent (X)) then
                  X := Parent (X);
                  Left_Rotate (Tree, X);
               end if;

               Set_Color (Parent (X), Black);
               Set_Color (Parent (Parent (X)), Red);
               Right_Rotate (Tree, Parent (Parent (X)));
            end if;

         else
            pragma Assert (Parent (X) = Right (Parent (Parent (X))));

            Y := Left (Parent (Parent (X)));

            if Y /= null and then Color (Y) = Red then
               Set_Color (Parent (X), Black);
               Set_Color (Y, Black);
               Set_Color (Parent (Parent (X)), Red);
               X := Parent (Parent (X));

            else
               if X = Left (Parent (X)) then
                  X := Parent (X);
                  Right_Rotate (Tree, X);
               end if;

               Set_Color (Parent (X), Black);
               Set_Color (Parent (Parent (X)), Red);
               Left_Rotate (Tree, Parent (Parent (X)));
            end if;
         end if;
      end loop;

      Set_Color (Tree.Root, Black);
   end Rebalance_For_Insert;

   ------------------
   -- Right_Rotate --
   ------------------

   procedure Right_Rotate (Tree : in out Tree_Type; Y : Node_Access) is
      X : constant Node_Access := Left (Y);
      pragma Assert (X /= null);

   begin
      Set_Left (Y, Right (X));

      if Right (X) /= null then
         Set_Parent (Right (X), Y);
      end if;

      Set_Parent (X, Parent (Y));

      if Y = Tree.Root then
         Tree.Root := X;
      elsif Y = Left (Parent (Y)) then
         Set_Left (Parent (Y), X);
      else
         pragma Assert (Y = Right (Parent (Y)));
         Set_Right (Parent (Y), X);
      end if;

      Set_Right (X, Y);
      Set_Parent (Y, X);
   end Right_Rotate;

   ---------
   -- Vet --
   ---------

   function Vet (Tree : Tree_Type; Node : Node_Access) return Boolean is
   begin
      if Node = null then
         return True;
      end if;

      if Parent (Node) = Node
        or else Left (Node) = Node
        or else Right (Node) = Node
      then
         return False;
      end if;

      if Tree.Length = 0
        or else Tree.Root = null
        or else Tree.First = null
        or else Tree.Last = null
      then
         return False;
      end if;

      if Parent (Tree.Root) /= null then
         return False;
      end if;

      if Left (Tree.First) /= null then
         return False;
      end if;

      if Right (Tree.Last) /= null then
         return False;
      end if;

      if Tree.Length = 1 then
         if Tree.First /= Tree.Last
           or else Tree.First /= Tree.Root
         then
            return False;
         end if;

         if Node /= Tree.First then
            return False;
         end if;

         if Parent (Node) /= null
           or else Left (Node) /= null
           or else Right (Node) /= null
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

         if Tree.First /= Node
           and then Tree.Last /= Node
         then
            return False;
         end if;
      end if;

      if Left (Node) /= null
        and then Parent (Left (Node)) /= Node
      then
         return False;
      end if;

      if Right (Node) /= null
        and then Parent (Right (Node)) /= Node
      then
         return False;
      end if;

      if Parent (Node) = null then
         if Tree.Root /= Node then
            return False;
         end if;

      elsif Left (Parent (Node)) /= Node
        and then Right (Parent (Node)) /= Node
      then
         return False;
      end if;

      return True;
   end Vet;

end Ada.Containers.Red_Black_Trees.Generic_Operations;
