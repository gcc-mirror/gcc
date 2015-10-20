------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_SET_OPERATIONS          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2015, Free Software Foundation, Inc.         --
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

with System; use type System.Address;

package body Ada.Containers.Red_Black_Trees.Generic_Set_Operations is

   pragma Warnings (Off, "variable ""Busy*"" is not referenced");
   pragma Warnings (Off, "variable ""Lock*"" is not referenced");
   --  See comment in Ada.Containers.Helpers

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Clear (Tree : in out Tree_Type);

   function Copy (Source : Tree_Type) return Tree_Type;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree : in out Tree_Type) is
      use type Helpers.Tamper_Counts;
      pragma Assert (Tree.TC = (Busy => 0, Lock => 0));

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
      Tgt : Node_Access;
      Src : Node_Access;

      Compare : Integer;

   begin
      if Target'Address = Source'Address then
         TC_Check (Target.TC);

         Clear (Target);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      TC_Check (Target.TC);

      Tgt := Target.First;
      Src := Source.First;
      loop
         if Tgt = null then
            exit;
         end if;

         if Src = null then
            exit;
         end if;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock_Target : With_Lock (Target.TC'Unrestricted_Access);
            Lock_Source : With_Lock (Source.TC'Unrestricted_Access);
         begin
            if Is_Less (Tgt, Src) then
               Compare := -1;
            elsif Is_Less (Src, Tgt) then
               Compare := 1;
            else
               Compare := 0;
            end if;
         end;

         if Compare < 0 then
            Tgt := Tree_Operations.Next (Tgt);

         elsif Compare > 0 then
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
   begin
      if Left'Address = Right'Address then
         return Tree_Type'(others => <>);  -- Empty set
      end if;

      if Left.Length = 0 then
         return Tree_Type'(others => <>);  -- Empty set
      end if;

      if Right.Length = 0 then
         return Copy (Left);
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

         Tree : Tree_Type;

         L_Node : Node_Access;
         R_Node : Node_Access;

         Dst_Node : Node_Access;
         pragma Warnings (Off, Dst_Node);

      begin
         L_Node := Left.First;
         R_Node := Right.First;
         loop
            if L_Node = null then
               exit;
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

               exit;
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

         return Tree;

      exception
         when others =>
            Delete_Tree (Tree.Root);
            raise;
      end;
   end Difference;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection
     (Target : in out Tree_Type;
      Source : Tree_Type)
   is
      Tgt : Node_Access;
      Src : Node_Access;

      Compare : Integer;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      TC_Check (Target.TC);

      if Source.Length = 0 then
         Clear (Target);
         return;
      end if;

      Tgt := Target.First;
      Src := Source.First;
      while Tgt /= null
        and then Src /= null
      loop
         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock_Target : With_Lock (Target.TC'Unrestricted_Access);
            Lock_Source : With_Lock (Source.TC'Unrestricted_Access);
         begin
            if Is_Less (Tgt, Src) then
               Compare := -1;
            elsif Is_Less (Src, Tgt) then
               Compare := 1;
            else
               Compare := 0;
            end if;
         end;

         if Compare < 0 then
            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Tree_Operations.Next (Tgt);
               Tree_Operations.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

         elsif Compare > 0 then
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
   begin
      if Left'Address = Right'Address then
         return Copy (Left);
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

         Tree : Tree_Type;

         L_Node : Node_Access;
         R_Node : Node_Access;

         Dst_Node : Node_Access;
         pragma Warnings (Off, Dst_Node);

      begin
         L_Node := Left.First;
         R_Node := Right.First;
         loop
            if L_Node = null then
               exit;
            end if;

            if R_Node = null then
               exit;
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

         return Tree;

      exception
         when others =>
            Delete_Tree (Tree.Root);
            raise;
      end;
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

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Subset : With_Lock (Subset.TC'Unrestricted_Access);
         Lock_Of_Set : With_Lock (Of_Set.TC'Unrestricted_Access);

         Subset_Node : Node_Access;
         Set_Node    : Node_Access;

      begin
         Subset_Node := Subset.First;
         Set_Node    := Of_Set.First;
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
   begin
      if Left'Address = Right'Address then
         return Left.Length /= 0;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

         L_Node : Node_Access;
         R_Node : Node_Access;
      begin
         L_Node := Left.First;
         R_Node := Right.First;
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
      end;
   end Overlap;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference
     (Target : in out Tree_Type;
      Source : Tree_Type)
   is
      Tgt : Node_Access;
      Src : Node_Access;

      New_Tgt_Node : Node_Access;
      pragma Warnings (Off, New_Tgt_Node);

      Compare : Integer;

   begin
      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      Tgt := Target.First;
      Src := Source.First;
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

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            Lock_Target : With_Lock (Target.TC'Unrestricted_Access);
            Lock_Source : With_Lock (Source.TC'Unrestricted_Access);
         begin
            if Is_Less (Tgt, Src) then
               Compare := -1;
            elsif Is_Less (Src, Tgt) then
               Compare := 1;
            else
               Compare := 0;
            end if;
         end;

         if Compare < 0 then
            Tgt := Tree_Operations.Next (Tgt);

         elsif Compare > 0 then
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
   begin
      if Left'Address = Right'Address then
         return Tree_Type'(others => <>);  -- Empty set
      end if;

      if Right.Length = 0 then
         return Copy (Left);
      end if;

      if Left.Length = 0 then
         return Copy (Right);
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

         Tree : Tree_Type;

         L_Node : Node_Access;
         R_Node : Node_Access;

         Dst_Node : Node_Access;
         pragma Warnings (Off, Dst_Node);

      begin
         L_Node := Left.First;
         R_Node := Right.First;
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

               exit;
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

               exit;
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

         return Tree;

      exception
         when others =>
            Delete_Tree (Tree.Root);
            raise;
      end;
   end Symmetric_Difference;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in out Tree_Type; Source : Tree_Type) is
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
            Dst_Hint => Hint,  -- use node most recently inserted as hint
            Src_Node => Node,
            Dst_Node => Hint);
      end Process;

   --  Start of processing for Union

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock_Source : With_Lock (Source.TC'Unrestricted_Access);
      begin
         Iterate (Source);
      end;
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
         Lock_Left : With_Lock (Left.TC'Unrestricted_Access);
         Lock_Right : With_Lock (Right.TC'Unrestricted_Access);

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
               Dst_Hint => Hint,  -- use node most recently inserted as hint
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
