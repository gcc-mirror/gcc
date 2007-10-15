------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .      --
--               G E N E R I C _ S E T _ O P E R A T I O N S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2007, Free Software Foundation, Inc.         --
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

with System; use type System.Address;

package body Ada.Containers.Red_Black_Trees.Generic_Set_Operations is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Clear (Tree : in out Tree_Type);

   function Copy (Source : Tree_Type) return Tree_Type;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree : in out Tree_Type) is
      pragma Assert (Tree.Busy = 0);
      pragma Assert (Tree.Lock = 0);

      Root : Node_Access := Tree.Root;
      pragma Warnings (Off, Root);

   begin
      Tree.Root := null;
      Tree.First := null;
      Tree.Last := null;
      Tree.Length := 0;

      Delete_Tree (Root);
   end Clear;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Tree_Type) return Tree_Type is
      Target : Tree_Type;

   begin
      if Source.Length = 0 then
         return Target;
      end if;

      Target.Root := Copy_Tree (Source.Root);
      Target.First := Tree_Operations.Min (Target.Root);
      Target.Last := Tree_Operations.Max (Target.Root);
      Target.Length := Source.Length;

      return Target;
   end Copy;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in out Tree_Type; Source : Tree_Type) is
      Tgt : Node_Access := Target.First;
      Src : Node_Access := Source.First;

   begin
      if Target'Address = Source'Address then
         if Target.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;

         Clear (Target);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      loop
         if Tgt = null then
            return;
         end if;

         if Src = null then
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
      Tree : Tree_Type;

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;
      pragma Warnings (Off, Dst_Node);

   begin
      if Left'Address = Right'Address then
         return Tree;  -- Empty set
      end if;

      if Left.Length = 0 then
         return Tree;  -- Empty set
      end if;

      if Right.Length = 0 then
         return Copy (Left);
      end if;

      loop
         if L_Node = null then
            return Tree;
         end if;

         if R_Node = null then
            while L_Node /= null loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => null,
                  Src_Node => L_Node,
                  Dst_Node => Dst_Node);

               L_Node := Tree_Operations.Next (L_Node);

            end loop;

            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => null,
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
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      if Source.Length = 0 then
         Clear (Target);
         return;
      end if;

      while Tgt /= null
        and then Src /= null
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

      while Tgt /= null loop
         declare
            X : Node_Access := Tgt;
         begin
            Tgt := Tree_Operations.Next (Tgt);
            Tree_Operations.Delete_Node_Sans_Free (Target, X);
            Free (X);
         end;
      end loop;
   end Intersection;

   function Intersection (Left, Right : Tree_Type) return Tree_Type is
      Tree : Tree_Type;

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;
      pragma Warnings (Off, Dst_Node);

   begin
      if Left'Address = Right'Address then
         return Copy (Left);
      end if;

      loop
         if L_Node = null then
            return Tree;
         end if;

         if R_Node = null then
            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            R_Node := Tree_Operations.Next (R_Node);

         else
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => null,
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
      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      if Subset.Length > Of_Set.Length then
         return False;
      end if;

      declare
         Subset_Node : Node_Access := Subset.First;
         Set_Node    : Node_Access := Of_Set.First;

      begin
         loop
            if Set_Node = null then
               return Subset_Node = null;
            end if;

            if Subset_Node = null then
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
      if Left'Address = Right'Address then
         return Left.Length /= 0;
      end if;

      loop
         if L_Node = null
           or else R_Node = null
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
      pragma Warnings (Off, New_Tgt_Node);

   begin
      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      loop
         if Tgt = null then
            while Src /= null loop
               Insert_With_Hint
                 (Dst_Tree => Target,
                  Dst_Hint => null,
                  Src_Node => Src,
                  Dst_Node => New_Tgt_Node);

               Src := Tree_Operations.Next (Src);
            end loop;

            return;
         end if;

         if Src = null then
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
      Tree : Tree_Type;

      L_Node : Node_Access := Left.First;
      R_Node : Node_Access := Right.First;

      Dst_Node : Node_Access;
      pragma Warnings (Off, Dst_Node);

   begin
      if Left'Address = Right'Address then
         return Tree;  -- Empty set
      end if;

      if Right.Length = 0 then
         return Copy (Left);
      end if;

      if Left.Length = 0 then
         return Copy (Right);
      end if;

      loop
         if L_Node = null then
            while R_Node /= null loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => null,
                  Src_Node => R_Node,
                  Dst_Node => Dst_Node);
               R_Node := Tree_Operations.Next (R_Node);
            end loop;

            return Tree;
         end if;

         if R_Node = null then
            while L_Node /= null loop
               Insert_With_Hint
                 (Dst_Tree => Tree,
                  Dst_Hint => null,
                  Src_Node => L_Node,
                  Dst_Node => Dst_Node);

               L_Node := Tree_Operations.Next (L_Node);
            end loop;

            return Tree;
         end if;

         if Is_Less (L_Node, R_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => null,
               Src_Node => L_Node,
               Dst_Node => Dst_Node);

            L_Node := Tree_Operations.Next (L_Node);

         elsif Is_Less (R_Node, L_Node) then
            Insert_With_Hint
              (Dst_Tree => Tree,
               Dst_Hint => null,
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
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      Iterate (Source);
   end Union;

   function Union (Left, Right : Tree_Type) return Tree_Type is
   begin
      if Left'Address = Right'Address then
         return Copy (Left);
      end if;

      if Left.Length = 0 then
         return Copy (Right);
      end if;

      if Right.Length = 0 then
         return Copy (Left);
      end if;

      declare
         Tree : Tree_Type := Copy (Left);

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
         return Tree;

      exception
         when others =>
            Delete_Tree (Tree.Root);
            raise;
      end;

   end Union;

end Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
