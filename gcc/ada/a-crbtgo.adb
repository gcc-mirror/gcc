------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_OPERATIONS             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

package body Ada.Containers.Red_Black_Trees.Generic_Operations is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Delete_Fixup (Tree : in out Tree_Type; Node : Node_Access);

   procedure Delete_Swap (Tree : in out Tree_Type; Z, Y : Node_Access);

   procedure Left_Rotate  (Tree : in out Tree_Type; X : Node_Access);
   procedure Right_Rotate (Tree : in out Tree_Type; Y : Node_Access);

   ---------------------
   -- Check_Invariant --
   ---------------------

   procedure Check_Invariant (Tree : Tree_Type) is
      Root : constant Node_Access := Tree.Root;

      function Check (Node : Node_Access) return Natural;

      -----------
      -- Check --
      -----------

      function Check (Node : Node_Access) return Natural is
      begin
         if Node = Null_Node then
            return 0;
         end if;

         if Color (Node) = Red then
            declare
               L : constant Node_Access := Left (Node);
            begin
               pragma Assert (L = Null_Node or else Color (L) = Black);
               null;
            end;

            declare
               R : constant Node_Access := Right (Node);
            begin
               pragma Assert (R = Null_Node or else Color (R) = Black);
               null;
            end;

            declare
               NL : constant Natural := Check (Left (Node));
               NR : constant Natural := Check (Right (Node));
            begin
               pragma Assert (NL = NR);
               return NL;
            end;
         end if;

         declare
            NL : constant Natural := Check (Left (Node));
            NR : constant Natural := Check (Right (Node));
         begin
            pragma Assert (NL = NR);
            return NL + 1;
         end;
      end Check;

   --  Start of processing for Check_Invariant

   begin
      if Root = Null_Node then
         pragma Assert (Tree.First = Null_Node);
         pragma Assert (Tree.Last = Null_Node);
         pragma Assert (Tree.Length = 0);
         null;

      else
         pragma Assert (Color (Root) = Black);
         pragma Assert (Tree.Length > 0);
         pragma Assert (Tree.Root /= Null_Node);
         pragma Assert (Tree.First /= Null_Node);
         pragma Assert (Tree.Last /= Null_Node);
         pragma Assert (Parent (Tree.Root) = Null_Node);
         pragma Assert ((Tree.Length > 1)
                           or else (Tree.First = Tree.Last
                                      and Tree.First = Tree.Root));
         pragma Assert (Left (Tree.First) = Null_Node);
         pragma Assert (Right (Tree.Last) = Null_Node);

         declare
            L  : constant Node_Access := Left (Root);
            R  : constant Node_Access := Right (Root);
            NL : constant Natural := Check (L);
            NR : constant Natural := Check (R);
         begin
            pragma Assert (NL = NR);
            null;
         end;
      end if;
   end Check_Invariant;

   ------------------
   -- Delete_Fixup --
   ------------------

   procedure Delete_Fixup (Tree : in out Tree_Type; Node : Node_Access) is

      --  CLR p274 ???

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

            if (Left (W)  = Null_Node or else Color (Left (W)) = Black)
              and then
               (Right (W) = Null_Node or else Color (Right (W)) = Black)
            then
               Set_Color (W, Red);
               X := Parent (X);

            else
               if Right (W) = Null_Node
                 or else Color (Right (W)) = Black
               then
                  if Left (W) /= Null_Node then
                     Set_Color (Left (W), Black);
                  end if;

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

            if (Left (W)  = Null_Node or else Color (Left (W)) = Black)
                  and then
               (Right (W) = Null_Node or else Color (Right (W)) = Black)
            then
               Set_Color (W, Red);
               X := Parent (X);

            else
               if Left (W) = Null_Node or else Color (Left (W)) = Black then
                  if Right (W) /= Null_Node then
                     Set_Color (Right (W), Black);
                  end if;

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
      --  CLR p273 ???

      X, Y : Node_Access;

      Z : constant Node_Access := Node;
      pragma Assert (Z /= Null_Node);

   begin
      pragma Assert (Tree.Length > 0);
      pragma Assert (Tree.Root /= Null_Node);
      pragma Assert (Tree.First /= Null_Node);
      pragma Assert (Tree.Last /= Null_Node);
      pragma Assert (Parent (Tree.Root) = Null_Node);
      pragma Assert ((Tree.Length > 1)
                        or else (Tree.First = Tree.Last
                                   and then Tree.First = Tree.Root));
      pragma Assert ((Left (Node) = Null_Node)
                        or else (Parent (Left (Node)) = Node));
      pragma Assert ((Right (Node) = Null_Node)
                        or else (Parent (Right (Node)) = Node));
      pragma Assert (((Parent (Node) = Null_Node) and then (Tree.Root = Node))
                        or else ((Parent (Node) /= Null_Node) and then
                                  ((Left (Parent (Node)) = Node)
                                     or else (Right (Parent (Node)) = Node))));

      if Left (Z) = Null_Node then
         if Right (Z) = Null_Node then
            if Z = Tree.First then
               Tree.First := Parent (Z);
            end if;

            if Z = Tree.Last then
               Tree.Last := Parent (Z);
            end if;

            if Color (Z) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (Z) = Null_Node);
            pragma Assert (Right (Z) = Null_Node);

            if Z = Tree.Root then
               pragma Assert (Tree.Length = 1);
               pragma Assert (Parent (Z) = Null_Node);
               Tree.Root := Null_Node;
            elsif Z = Left (Parent (Z)) then
               Set_Left (Parent (Z), Null_Node);
            else
               pragma Assert (Z = Right (Parent (Z)));
               Set_Right (Parent (Z), Null_Node);
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

      elsif Right (Z) = Null_Node then
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
         pragma Assert (Left (Y) = Null_Node);

         X := Right (Y);

         if X = Null_Node then
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
               Set_Left (Z, Null_Node);
               Set_Right (Z, Null_Node);

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

            pragma Assert (Left (Z) = Null_Node);
            pragma Assert (Right (Z) = Null_Node);

            if Z = Right (Parent (Z)) then
               Set_Right (Parent (Z), Null_Node);
            else
               pragma Assert (Z = Left (Parent (Z)));
               Set_Left (Parent (Z), Null_Node);
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

      if Right (Y) /= Null_Node then
         Set_Parent (Right (Y), Y);
      end if;

      if Left (Y) /= Null_Node then
         Set_Parent (Left (Y), Y);
      end if;

      Set_Parent (Z, Y_Parent);
      Set_Color (Z, Y_Color);
      Set_Left (Z, Null_Node);
      Set_Right (Z, Null_Node);
   end Delete_Swap;

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal (Left, Right : Tree_Type) return Boolean is
      L_Node : Node_Access;
      R_Node : Node_Access;

   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      L_Node := Left.First;
      R_Node := Right.First;
      while L_Node /= Null_Node loop
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
         while X /= Null_Node loop
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
   -- Generic_Read --
   ------------------

   procedure Generic_Read (Tree : in out Tree_Type; N : Count_Type) is

      pragma Assert (Tree.Length = 0);
      --  Clear and back node reinit was done by caller

      Node, Last_Node : Node_Access;

   begin
      if N = 0 then
         return;
      end if;

      Node := New_Node;
      pragma Assert (Node /= Null_Node);
      pragma Assert (Color (Node) = Red);

      Set_Color (Node, Black);

      Tree.Root := Node;
      Tree.First := Node;
      Tree.Last := Node;

      Tree.Length := 1;

      for J in Count_Type range 2 .. N loop
         Last_Node := Node;
         pragma Assert (Last_Node = Tree.Last);

         Node := New_Node;
         pragma Assert (Node /= Null_Node);
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
         while X /= Null_Node loop
            Iterate (Right (X));
            Process (X);
            X := Left (X);
         end loop;
      end Iterate;

   --  Start of processing for Generic_Reverse_Iteration

   begin
      Iterate (Tree.Root);
   end Generic_Reverse_Iteration;

   -----------------
   -- Left_Rotate --
   -----------------

   procedure Left_Rotate (Tree : in out Tree_Type; X : Node_Access) is

      --  CLR p266 ???

      Y : constant Node_Access := Right (X);
      pragma Assert (Y /= Null_Node);

   begin
      Set_Right (X, Left (Y));

      if Left (Y) /= Null_Node then
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

      --  CLR p248 ???

      X : Node_Access := Node;
      Y : Node_Access;

   begin
      loop
         Y := Right (X);

         if Y = Null_Node then
            return X;
         end if;

         X := Y;
      end loop;
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Node : Node_Access) return Node_Access is

      --  CLR p248 ???

      X : Node_Access := Node;
      Y : Node_Access;

   begin
      loop
         Y := Left (X);

         if Y = Null_Node then
            return X;
         end if;

         X := Y;
      end loop;
   end Min;

   ----------
   -- Move --
   ----------

   procedure Move (Target, Source : in out Tree_Type) is
   begin
      if Target.Length > 0 then
         raise Constraint_Error;
      end if;

      Target := Source;
      Source := (First => Null_Node,
                 Last  => Null_Node,
                 Root  => Null_Node,
                 Length => 0);
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Node : Node_Access) return Node_Access is
   begin
      --  CLR p249 ???

      if Node = Null_Node then
         return Null_Node;
      end if;

      if Right (Node) /= Null_Node then
         return Min (Right (Node));
      end if;

      declare
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);

      begin
         while Y /= Null_Node
           and then X = Right (Y)
         loop
            X := Y;
            Y := Parent (Y);
         end loop;

         --  Why is this code commented out ???

--           if Right (X) /= Y then
--              return Y;
--           else
--              return X;
--           end if;

         return Y;
      end;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Node : Node_Access) return Node_Access is
   begin
      if Node = Null_Node then
         return Null_Node;
      end if;

      if Left (Node) /= Null_Node then
         return Max (Left (Node));
      end if;

      declare
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);

      begin
         while Y /= Null_Node
           and then X = Left (Y)
         loop
            X := Y;
            Y := Parent (Y);
         end loop;

         --  Why is this code commented out ???

--           if Left (X) /= Y then
--              return Y;
--           else
--              return X;
--           end if;

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
      --  CLR p.268 ???

      X : Node_Access := Node;
      pragma Assert (X /= Null_Node);
      pragma Assert (Color (X) = Red);

      Y : Node_Access;

   begin
      while X /= Tree.Root and then Color (Parent (X)) = Red loop
         if Parent (X) = Left (Parent (Parent (X))) then
            Y := Right (Parent (Parent (X)));

            if Y /= Null_Node and then Color (Y) = Red then
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

            if Y /= Null_Node and then Color (Y) = Red then
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
      pragma Assert (X /= Null_Node);

   begin
      Set_Left (Y, Right (X));

      if Right (X) /= Null_Node then
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

end Ada.Containers.Red_Black_Trees.Generic_Operations;
