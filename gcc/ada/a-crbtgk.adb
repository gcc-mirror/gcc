------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .     --
--                          G E N E R I C _ K E Y S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
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

   function Ceiling (Tree : Tree_Type; Key  : Key_Type) return Node_Access is
      Y : Node_Access;
      X : Node_Access := Tree.Root;

   begin
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
      X : Node_Access := Tree.Root;

   begin
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
      X : Node_Access := Tree.Root;

   begin
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
     (Tree    : in out Tree_Type;
      Key     : Key_Type;
      Node    : out Node_Access;
      Success : out Boolean)
   is
      Y : Node_Access := null;
      X : Node_Access := Tree.Root;

   begin
      Success := True;
      while X /= null loop
         Y := X;
         Success := Is_Less_Key_Node (Key, X);

         if Success then
            X := Ops.Left (X);
         else
            X := Ops.Right (X);
         end if;
      end loop;

      Node := Y;

      if Success then
         if Node = Tree.First then
            Insert_Post (Tree, X, Y, Key, Node);
            return;
         end if;

         Node := Ops.Previous (Node);
      end if;

      if Is_Greater_Key_Node (Key, Node) then
         Insert_Post (Tree, X, Y, Key, Node);
         Success := True;
         return;
      end if;

      Success := False;
   end Generic_Conditional_Insert;

   ------------------------------------------
   -- Generic_Conditional_Insert_With_Hint --
   ------------------------------------------

   procedure Generic_Conditional_Insert_With_Hint
     (Tree     : in out Tree_Type;
      Position : Node_Access;
      Key      : Key_Type;
      Node     : out Node_Access;
      Success  : out Boolean)
   is
   begin
      if Position = null then  -- largest
         if Tree.Length > 0
           and then Is_Greater_Key_Node (Key, Tree.Last)
         then
            Insert_Post (Tree, null, Tree.Last, Key, Node);
            Success := True;
         else
            Conditional_Insert_Sans_Hint (Tree, Key, Node, Success);
         end if;

         return;
      end if;

      pragma Assert (Tree.Length > 0);

      if Is_Less_Key_Node (Key, Position) then
         if Position = Tree.First then
            Insert_Post (Tree, Position, Position, Key, Node);
            Success := True;
            return;
         end if;

         declare
            Before : constant Node_Access := Ops.Previous (Position);

         begin
            if Is_Greater_Key_Node (Key, Before) then
               if Ops.Right (Before) = null then
                  Insert_Post (Tree, null, Before, Key, Node);
               else
                  Insert_Post (Tree, Position, Position, Key, Node);
               end if;

               Success := True;

            else
               Conditional_Insert_Sans_Hint (Tree, Key, Node, Success);
            end if;
         end;

         return;
      end if;

      if Is_Greater_Key_Node (Key, Position) then
         if Position = Tree.Last then
            Insert_Post (Tree, null, Tree.Last, Key, Node);
            Success := True;
            return;
         end if;

         declare
            After : constant Node_Access := Ops.Next (Position);

         begin
            if Is_Less_Key_Node (Key, After) then
               if Ops.Right (Position) = null then
                  Insert_Post (Tree, null, Position, Key, Node);
               else
                  Insert_Post (Tree, After, After, Key, Node);
               end if;

               Success := True;

            else
               Conditional_Insert_Sans_Hint (Tree, Key, Node, Success);
            end if;
         end;

         return;
      end if;

      Node := Position;
      Success := False;
   end Generic_Conditional_Insert_With_Hint;

   -------------------------
   -- Generic_Insert_Post --
   -------------------------

   procedure Generic_Insert_Post
     (Tree : in out Tree_Type;
      X, Y : Node_Access;
      Key  : Key_Type;
      Z    : out Node_Access)
   is
      subtype Length_Subtype is Count_Type range 0 .. Count_Type'Last - 1;

      New_Length : constant Count_Type := Length_Subtype'(Tree.Length) + 1;

   begin
      if Tree.Busy > 0 then
         raise Program_Error;
      end if;

      if Y = null
        or else X /= null
        or else Is_Less_Key_Node (Key, Y)
      then
         pragma Assert (Y = null
                          or else Ops.Left (Y) = null);

         --  Delay allocation as long as we can, in order to defend
         --  against exceptions propagated by relational operators.

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

         else
            Ops.Set_Left (Y, Z);

            if Y = Tree.First then
               Tree.First := Z;
            end if;
         end if;

      else
         pragma Assert (Ops.Right (Y) = null);

         --  Delay allocation as long as we can, in order to defend
         --  against exceptions propagated by relational operators.

         Z := New_Node;

         pragma Assert (Z /= null);
         pragma Assert (Ops.Color (Z) = Red);

         Ops.Set_Right (Y, Z);

         if Y = Tree.Last then
            Tree.Last := Z;
         end if;
      end if;

      Ops.Set_Parent (Z, Y);
      Ops.Rebalance_For_Insert (Tree, Z);
      Tree.Length := New_Length;
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
         N : Node_Access := Node;
      begin
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
         N : Node_Access := Node;
      begin
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
      Y : Node_Access := null;
      X : Node_Access := Tree.Root;

   begin
      while X /= null loop
         Y := X;

         if Is_Less_Key_Node (Key, X) then
            X := Ops.Left (X);
         else
            X := Ops.Right (X);
         end if;
      end loop;

      Insert_Post (Tree, X, Y, Key, Node);
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
      --  TODO: verify this algorithm.  It was (quickly) adapted it from the
      --  same algorithm for conditional_with_hint. It may be that the test
      --  Key > Hint should be something like a Key >= Hint, to handle the
      --  case when Hint is The Last Item of A (Contiguous) sequence of
      --  Equivalent Items.  (The Key < Hint Test is probably OK. It is not
      --  clear that you can use Key <= Hint, since new items are always
      --  inserted last in the sequence of equivalent items.) ???

   begin
      if Hint = null then  -- largest
         if Tree.Length > 0
           and then Is_Greater_Key_Node (Key, Tree.Last)
         then
            Insert_Post (Tree, null, Tree.Last, Key, Node);
         else
            Unconditional_Insert_Sans_Hint (Tree, Key, Node);
         end if;

         return;
      end if;

      pragma Assert (Tree.Length > 0);

      if Is_Less_Key_Node (Key, Hint) then
         if Hint = Tree.First then
            Insert_Post (Tree, Hint, Hint, Key, Node);
            return;
         end if;

         declare
            Before : constant Node_Access := Ops.Previous (Hint);
         begin
            if Is_Greater_Key_Node (Key, Before) then
               if Ops.Right (Before) = null then
                  Insert_Post (Tree, null, Before, Key, Node);
               else
                  Insert_Post (Tree, Hint, Hint, Key, Node);
               end if;
            else
               Unconditional_Insert_Sans_Hint (Tree, Key, Node);
            end if;
         end;

         return;
      end if;

      if Is_Greater_Key_Node (Key, Hint) then
         if Hint = Tree.Last then
            Insert_Post (Tree, null, Tree.Last, Key, Node);
            return;
         end if;

         declare
            After : constant Node_Access := Ops.Next (Hint);
         begin
            if Is_Less_Key_Node (Key, After) then
               if Ops.Right (Hint) = null then
                  Insert_Post (Tree, null, Hint, Key, Node);
               else
                  Insert_Post (Tree, After, After, Key, Node);
               end if;
            else
               Unconditional_Insert_Sans_Hint (Tree, Key, Node);
            end if;
         end;

         return;
      end if;

      Unconditional_Insert_Sans_Hint (Tree, Key, Node);
   end Generic_Unconditional_Insert_With_Hint;

   -----------------
   -- Upper_Bound --
   -----------------

   function Upper_Bound
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access
   is
      Y : Node_Access;
      X : Node_Access := Tree.Root;

   begin
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
