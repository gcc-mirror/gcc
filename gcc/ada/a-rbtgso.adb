------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--          ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_SET_OPERATIONS           --
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

package body Ada.Containers.Red_Black_Trees.Generic_Set_Operations is

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Tree_Type; Source : Tree_Type) is
      Tgt : Node_Access := Target.First;
      Src : Node_Access := Source.First;

   begin

      --  NOTE: must be done by client:
      --      if Target'Address = Source'Address then
      --         Clear (Target);
      --         return;
      --      end if;

      loop
         if Tgt = Tree_Operations.Null_Node then
            return;
         end if;

         if Src = Tree_Operations.Null_Node then
            return;
         end if;

         if Is_Less (Tgt, Src) then
            Tgt := Tree_Operations.Next (Tgt);

         elsif Is_Less (Src, Tgt) then
            Src := Tree_Operations.Next (Src);

         else
            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Tree_Operations.Next (Tgt);
               Tree_Operations.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

            Src := Tree_Operations.Next (Src);
         end if;
      end loop;
   end Difference;

   function Difference (Left, Right : Tree_Type) return Tree_Type is
      Tree : Tree_Type := (Length => 0, others => Tree_Operations.Null_Node);

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;

   begin
      --  NOTE: must by done by client:
      --      if Left'Address = Right'Address then
      --         return Empty_Set;
      --      end if;

      loop
         if L_Node = Tree_Operations.Null_Node then
            return Tree;
         end if;

         if R_Node = Tree_Operations.Null_Node then
            while L_Node /= Tree_Operations.Null_Node loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => Tree_Operations.Null_Node,
                  Src_Node => L_Node,
                  Dst_Node => Dst_Node);

               L_Node := Tree_Operations.Next (L_Node);

            end loop;

            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => Tree_Operations.Null_Node,
               Src_Node => L_Node,
               Dst_Node => Dst_Node);

            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            R_Node := Tree_Operations.Next (R_Node);

         else
            L_Node := Tree_Operations.Next (L_Node);
            R_Node := Tree_Operations.Next (R_Node);
         end if;
      end loop;

   exception
      when others =>
         Delete_Tree (Tree.Root);
         raise;
   end Difference;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection
     (Target : in out Tree_Type;
      Source : Tree_Type)
   is
      Tgt : Node_Access := Target.First;
      Src : Node_Access := Source.First;

   begin
      --  NOTE: must be done by caller: ???
      --      if Target'Address = Source'Address then
      --         return;
      --      end if;

      while Tgt /= Tree_Operations.Null_Node
        and then Src /= Tree_Operations.Null_Node
      loop
         if Is_Less (Tgt, Src) then
            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Tree_Operations.Next (Tgt);
               Tree_Operations.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

         elsif Is_Less (Src, Tgt) then
            Src := Tree_Operations.Next (Src);

         else
            Tgt := Tree_Operations.Next (Tgt);
            Src := Tree_Operations.Next (Src);
         end if;
      end loop;
   end Intersection;

   function Intersection (Left, Right : Tree_Type) return Tree_Type is
      Tree : Tree_Type := (Length => 0, others => Tree_Operations.Null_Node);

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;

   begin
      --  NOTE: must be done by caller: ???
      --      if Left'Address = Right'Address then
      --         return Left;
      --      end if;

      loop
         if L_Node = Tree_Operations.Null_Node then
            return Tree;
         end if;

         if R_Node = Tree_Operations.Null_Node then
            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            R_Node := Tree_Operations.Next (R_Node);

         else
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => Tree_Operations.Null_Node,
               Src_Node => L_Node,
               Dst_Node => Dst_Node);

            L_Node := Tree_Operations.Next (L_Node);
            R_Node := Tree_Operations.Next (R_Node);
         end if;
      end loop;

   exception
      when others =>
         Delete_Tree (Tree.Root);
         raise;
   end Intersection;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset
     (Subset : Tree_Type;
      Of_Set : Tree_Type) return Boolean
   is
   begin
      --  NOTE: must by done by caller:
      --      if Subset'Address = Of_Set'Address then
      --         return True;
      --      end if;

      if Subset.Length > Of_Set.Length then
         return False;
      end if;

      declare
         Subset_Node : Node_Access := Subset.First;
         Set_Node : Node_Access := Of_Set.First;

      begin
         loop
            if Set_Node = Tree_Operations.Null_Node then
               return Subset_Node = Tree_Operations.Null_Node;
            end if;

            if Subset_Node = Tree_Operations.Null_Node then
               return True;
            end if;

            if Is_Less (Subset_Node, Set_Node) then
               return False;
            end if;

            if Is_Less (Set_Node, Subset_Node) then
               Set_Node := Tree_Operations.Next (Set_Node);
            else
               Set_Node := Tree_Operations.Next (Set_Node);
               Subset_Node := Tree_Operations.Next (Subset_Node);
            end if;
         end loop;
      end;
   end Is_Subset;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Tree_Type) return Boolean is
      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

   begin
      --  NOTE: must be done by caller: ???
      --      if Left'Address = Right'Address then
      --         return Left.Tree.Length /= 0;
      --      end if;

      loop
         if L_Node = Tree_Operations.Null_Node
           or else R_Node = Tree_Operations.Null_Node
         then
            return False;
         end if;

         if Is_Less (L_Node, R_Node) then
            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            R_Node := Tree_Operations.Next (R_Node);

         else
            return True;
         end if;
      end loop;
   end Overlap;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference
     (Target : in out Tree_Type;
      Source : Tree_Type)
   is
      Tgt : Node_Access := Target.First;
      Src : Node_Access := Source.First;

      New_Tgt_Node : Node_Access;

   begin
      --  NOTE: must by done by client: ???
      --      if Target'Address = Source'Address then
      --         Clear (Target);
      --         return;
      --      end if;

      loop
         if Tgt = Tree_Operations.Null_Node then
            while Src /= Tree_Operations.Null_Node loop
               Insert_With_Hint
                 (Dst_Tree => Target,
                  Dst_Hint => Tree_Operations.Null_Node,
                  Src_Node => Src,
                  Dst_Node => New_Tgt_Node);

               Src := Tree_Operations.Next (Src);
            end loop;

            return;
         end if;

         if Src = Tree_Operations.Null_Node then
            return;
         end if;

         if Is_Less (Tgt, Src) then
            Tgt := Tree_Operations.Next (Tgt);

         elsif Is_Less (Src, Tgt) then
            Insert_With_Hint
              (Dst_Tree => Target,
               Dst_Hint => Tgt,
               Src_Node => Src,
               Dst_Node => New_Tgt_Node);

            Src := Tree_Operations.Next (Src);

         else
            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Tree_Operations.Next (Tgt);
               Tree_Operations.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

            Src := Tree_Operations.Next (Src);
         end if;
      end loop;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Tree_Type) return Tree_Type is
      Tree : Tree_Type := (Length => 0, others => Tree_Operations.Null_Node);

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;

   begin
      --  NOTE: must by done by caller ???
      --      if Left'Address = Right'Address then
      --         return Empty_Set;
      --      end if;

      loop
         if L_Node = Tree_Operations.Null_Node then
            while R_Node /= Tree_Operations.Null_Node loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => Tree_Operations.Null_Node,
                  Src_Node => R_Node,
                  Dst_Node => Dst_Node);
               R_Node := Tree_Operations.Next (R_Node);
            end loop;

            return Tree;
         end if;

         if R_Node = Tree_Operations.Null_Node then
            while L_Node /= Tree_Operations.Null_Node loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => Tree_Operations.Null_Node,
                  Src_Node => L_Node,
                  Dst_Node => Dst_Node);

               L_Node := Tree_Operations.Next (L_Node);
            end loop;

            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => Tree_Operations.Null_Node,
               Src_Node => L_Node,
               Dst_Node => Dst_Node);

            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => Tree_Operations.Null_Node,
               Src_Node => R_Node,
               Dst_Node => Dst_Node);

            R_Node := Tree_Operations.Next (R_Node);

         else
            L_Node := Tree_Operations.Next (L_Node);
            R_Node := Tree_Operations.Next (R_Node);
         end if;
      end loop;

   exception
      when others =>
         Delete_Tree (Tree.Root);
         raise;
   end Symmetric_Difference;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Tree_Type; Source : Tree_Type)
   is
      Hint : Node_Access;

      procedure Process (Node : Node_Access);
      pragma Inline (Process);

      procedure Iterate is new Tree_Operations.Generic_Iteration (Process);

      -------------
      -- Process --
      -------------

      procedure Process (Node : Node_Access) is
      begin
         Insert_With_Hint
           (Dst_Tree => Target,
            Dst_Hint => Hint,
            Src_Node => Node,
            Dst_Node => Hint);
      end Process;

   --  Start of processing for Union

   begin
      --  NOTE: must be done by caller: ???
      --      if Target'Address = Source'Address then
      --         return;
      --      end if;

      Iterate (Source);
   end Union;

   function Union (Left, Right : Tree_Type) return Tree_Type is
      Tree : Tree_Type;

   begin
      --  NOTE: must be done by caller:
      --      if Left'Address = Right'Address then
      --         return Left;
      --      end if;

      declare
         Root : constant Node_Access := Copy_Tree (Left.Root);
      begin
         Tree := (Root   => Root,
                  First  => Tree_Operations.Min (Root),
                  Last   => Tree_Operations.Max (Root),
                  Length => Left.Length);
      end;

      declare
         Hint : Node_Access;

         procedure Process (Node : Node_Access);
         pragma Inline (Process);

         procedure Iterate is
           new Tree_Operations.Generic_Iteration (Process);

         -------------
         -- Process --
         -------------

         procedure Process (Node : Node_Access) is
         begin
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => Hint,
               Src_Node => Node,
               Dst_Node => Hint);
         end Process;

      --  Start of processing for Union

      begin
         Iterate (Right);

      exception
         when others =>
            Delete_Tree (Tree.Root);
            raise;
      end;

      return Tree;
   end Union;

end Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
