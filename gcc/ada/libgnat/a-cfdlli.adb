------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_DOUBLY_LINKED_LISTS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2021, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

with System; use type System.Address;

package body Ada.Containers.Formal_Doubly_Linked_Lists with
  SPARK_Mode => Off
is
   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate
     (Container : in out List;
      New_Item  : Element_Type;
      New_Node  : out Count_Type);

   procedure Free (Container : in out List; X : Count_Type);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Count_Type;
      New_Node  : Count_Type);

   function Vet (L : List; Position : Cursor) return Boolean;

   ---------
   -- "=" --
   ---------

   function "=" (Left : List; Right : List) return Boolean is
      LI : Count_Type;
      RI : Count_Type;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      LI := Left.First;
      RI := Left.First;
      while LI /= 0 loop
         if Left.Nodes (LI).Element /= Right.Nodes (LI).Element then
            return False;
         end if;

         LI := Left.Nodes (LI).Next;
         RI := Right.Nodes (RI).Next;
      end loop;

      return True;
   end "=";

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Container : in out List;
      New_Item  : Element_Type;
      New_Node  : out Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Free >= 0 then
         New_Node := Container.Free;
         N (New_Node).Element := New_Item;
         Container.Free := N (New_Node).Next;

      else
         New_Node := abs Container.Free;
         N (New_Node).Element := New_Item;
         Container.Free := Container.Free - 1;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out List; New_Item : Element_Type) is
   begin
      Insert (Container, No_Element, New_Item, 1);
   end Append;

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
   begin
      Insert (Container, No_Element, New_Item, Count);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out List; Source : List) is
      N : Node_Array renames Source.Nodes;
      J : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      J := Source.First;
      while J /= 0 loop
         Append (Target, N (J).Element, 1);
         J := N (J).Next;
      end loop;
   end Assign;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out List) is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Container.Length = 0 then
         pragma Assert (Container.First = 0);
         pragma Assert (Container.Last  = 0);
         return;
      end if;

      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last  >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next  = 0);

      while Container.Length > 1 loop
         X := Container.First;

         Container.First := N (X).Next;
         N (Container.First).Prev := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;

      X := Container.First;

      Container.First := 0;
      Container.Last := 0;
      Container.Length := 0;

      Free (Container, X);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : List;
      Capacity : Count_Type := 0) return List
   is
      C : constant Count_Type := Count_Type'Max (Source.Capacity, Capacity);
      N : Count_Type;
      P : List (C);

   begin
      if 0 < Capacity and then Capacity < Source.Capacity then
         raise Capacity_Error;
      end if;

      N := 1;
      while N <= Source.Capacity loop
         P.Nodes (N).Prev := Source.Nodes (N).Prev;
         P.Nodes (N).Next := Source.Nodes (N).Next;
         P.Nodes (N).Element := Source.Nodes (N).Element;
         N := N + 1;
      end loop;

      P.Free   := Source.Free;
      P.Length := Source.Length;
      P.First  := Source.First;
      P.Last   := Source.Last;

      if P.Free >= 0 then
         N := Source.Capacity + 1;
         while N <= C loop
            Free (P, N);
            N := N + 1;
         end loop;
      end if;

      return P;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out List; Position : in out Cursor) is
   begin
      Delete
        (Container => Container,
         Position  => Position,
         Count     => 1);
   end Delete;

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type)
   is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if not Has_Element (Container => Container,
                          Position  => Position)
      then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Container, Position), "bad cursor in Delete");
      pragma Assert (Container.First >= 1);
      pragma Assert (Container.Last  >= 1);
      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next  = 0);

      if Position.Node = Container.First then
         Delete_First (Container, Count);
         Position := No_Element;
         return;
      end if;

      if Count = 0 then
         Position := No_Element;
         return;
      end if;

      for Index in 1 .. Count loop
         pragma Assert (Container.Length >= 2);

         X := Position.Node;
         Container.Length := Container.Length - 1;

         if X = Container.Last then
            Position := No_Element;

            Container.Last := N (X).Prev;
            N (Container.Last).Next := 0;

            Free (Container, X);
            return;
         end if;

         Position.Node := N (X).Next;
         pragma Assert (N (Position.Node).Prev >= 0);

         N (N (X).Next).Prev := N (X).Prev;
         N (N (X).Prev).Next := N (X).Next;

         Free (Container, X);
      end loop;

      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out List) is
   begin
      Delete_First
        (Container => Container,
         Count     => 1);
   end Delete_First;

   procedure Delete_First (Container : in out List; Count : Count_Type) is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

      for J in 1 .. Count loop
         X := Container.First;
         pragma Assert (N (N (X).Next).Prev = Container.First);

         Container.First := N (X).Next;
         N (Container.First).Prev := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out List) is
   begin
      Delete_Last
        (Container => Container,
         Count     => 1);
   end Delete_Last;

   procedure Delete_Last (Container : in out List; Count : Count_Type) is
      N : Node_Array renames Container.Nodes;
      X : Count_Type;

   begin
      if Count >= Container.Length then
         Clear (Container);
         return;
      end if;

      if Count = 0 then
         return;
      end if;

      for J in 1 .. Count loop
         X := Container.Last;
         pragma Assert (N (N (X).Prev).Next = Container.Last);

         Container.Last := N (X).Prev;
         N (Container.Last).Next := 0;

         Container.Length := Container.Length - 1;

         Free (Container, X);
      end loop;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : List;
      Position  : Cursor) return Element_Type
   is
   begin
      if not Has_Element (Container => Container, Position  => Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      return Container.Nodes (Position.Node).Element;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      From : Count_Type := Position.Node;

   begin
      if From = 0 and Container.Length = 0 then
         return No_Element;
      end if;

      if From = 0 then
         From := Container.First;
      end if;

      if Position.Node /= 0 and then not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      while From /= 0 loop
         if Container.Nodes (From).Element = Item then
            return (Node => From);
         end if;

         From := Container.Nodes (From).Next;
      end loop;

      return No_Element;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = 0 then
         return No_Element;
      end if;

      return (Node => Container.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
      F : constant Count_Type := Container.First;

   begin
      if F = 0 then
         raise Constraint_Error with "list is empty";
      else
         return Container.Nodes (F).Element;
      end if;
   end First_Element;

   ------------------
   -- Formal_Model --
   ------------------

   package body Formal_Model is

      ----------------------------
      -- Lift_Abstraction_Level --
      ----------------------------

      procedure Lift_Abstraction_Level (Container : List) is null;

      -------------------------
      -- M_Elements_In_Union --
      -------------------------

      function M_Elements_In_Union
        (Container : M.Sequence;
         Left      : M.Sequence;
         Right     : M.Sequence) return Boolean
      is
         Elem : Element_Type;

      begin
         for Index in 1 .. M.Length (Container) loop
            Elem := Element (Container, Index);

            if not M.Contains (Left, 1, M.Length (Left), Elem)
               and then not M.Contains (Right, 1, M.Length (Right), Elem)
            then
               return False;
            end if;
         end loop;

         return True;
      end M_Elements_In_Union;

      -------------------------
      -- M_Elements_Included --
      -------------------------

      function M_Elements_Included
        (Left  : M.Sequence;
         L_Fst : Positive_Count_Type := 1;
         L_Lst : Count_Type;
         Right : M.Sequence;
         R_Fst : Positive_Count_Type := 1;
         R_Lst : Count_Type) return Boolean
      is
      begin
         for I in L_Fst .. L_Lst loop
            declare
               Found : Boolean := False;
               J     : Count_Type := R_Fst - 1;

            begin
               while not Found and J < R_Lst loop
                  J := J + 1;
                  if Element (Left, I) = Element (Right, J) then
                     Found := True;
                  end if;
               end loop;

               if not Found then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end M_Elements_Included;

      -------------------------
      -- M_Elements_Reversed --
      -------------------------

      function M_Elements_Reversed
        (Left  : M.Sequence;
         Right : M.Sequence) return Boolean
      is
         L : constant Count_Type := M.Length (Left);

      begin
         if L /= M.Length (Right) then
            return False;
         end if;

         for I in 1 .. L loop
            if Element (Left, I) /= Element (Right, L - I + 1) then
               return False;
            end if;
         end loop;

         return True;
      end M_Elements_Reversed;

      ------------------------
      -- M_Elements_Swapped --
      ------------------------

      function M_Elements_Swapped
        (Left  : M.Sequence;
         Right : M.Sequence;
         X     : Positive_Count_Type;
         Y     : Positive_Count_Type) return Boolean
      is
      begin
         if M.Length (Left) /= M.Length (Right)
           or else Element (Left, X) /= Element (Right, Y)
           or else Element (Left, Y) /= Element (Right, X)
         then
            return False;
         end if;

         for I in 1 .. M.Length (Left) loop
            if I /= X and then I /= Y
              and then Element (Left, I) /= Element (Right, I)
            then
               return False;
            end if;
         end loop;

         return True;
      end M_Elements_Swapped;

      -----------
      -- Model --
      -----------

      function Model (Container : List) return M.Sequence is
         Position : Count_Type := Container.First;
         R        : M.Sequence;

      begin
         --  Can't use First, Next or Element here, since they depend on models
         --  for their postconditions.

         while Position /= 0 loop
            R := M.Add (R, Container.Nodes (Position).Element);
            Position := Container.Nodes (Position).Next;
         end loop;

         return R;
      end Model;

      -----------------------
      -- Mapping_Preserved --
      -----------------------

      function Mapping_Preserved
        (M_Left  : M.Sequence;
         M_Right : M.Sequence;
         P_Left  : P.Map;
         P_Right : P.Map) return Boolean
      is
      begin
         for C of P_Left loop
            if not P.Has_Key (P_Right, C)
              or else P.Get (P_Left,  C) > M.Length (M_Left)
              or else P.Get (P_Right, C) > M.Length (M_Right)
              or else M.Get (M_Left,  P.Get (P_Left,  C)) /=
                      M.Get (M_Right, P.Get (P_Right, C))
            then
               return False;
            end if;
         end loop;

         for C of P_Right loop
            if not P.Has_Key (P_Left, C) then
               return False;
            end if;
         end loop;

         return True;
      end Mapping_Preserved;

      -------------------------
      -- P_Positions_Shifted --
      -------------------------

      function P_Positions_Shifted
        (Small : P.Map;
         Big   : P.Map;
         Cut   : Positive_Count_Type;
         Count : Count_Type := 1) return Boolean
      is
      begin
         for Cu of Small loop
            if not P.Has_Key (Big, Cu) then
               return False;
            end if;
         end loop;

         for Cu of Big loop
            declare
               Pos : constant Positive_Count_Type := P.Get (Big, Cu);

            begin
               if Pos < Cut then
                  if not P.Has_Key (Small, Cu)
                    or else Pos /= P.Get (Small, Cu)
                  then
                     return False;
                  end if;

               elsif Pos >= Cut + Count then
                  if not P.Has_Key (Small, Cu)
                    or else Pos /= P.Get (Small, Cu) + Count
                  then
                     return False;
                  end if;

               else
                  if P.Has_Key (Small, Cu) then
                     return False;
                  end if;
               end if;
            end;
         end loop;

         return True;
      end P_Positions_Shifted;

      -------------------------
      -- P_Positions_Swapped --
      -------------------------

      function P_Positions_Swapped
        (Left  : P.Map;
         Right : P.Map;
         X     : Cursor;
         Y     : Cursor) return Boolean
      is
      begin
         if not P.Has_Key (Left, X)
           or not P.Has_Key (Left, Y)
           or not P.Has_Key (Right, X)
           or not P.Has_Key (Right, Y)
         then
            return False;
         end if;

         if P.Get (Left, X) /= P.Get (Right, Y)
           or P.Get (Left, Y) /= P.Get (Right, X)
         then
            return False;
         end if;

         for C of Left loop
            if not P.Has_Key (Right, C) then
               return False;
            end if;
         end loop;

         for C of Right loop
            if not P.Has_Key (Left, C)
              or else (C /= X
                        and C /= Y
                        and P.Get (Left, C) /= P.Get (Right, C))
            then
               return False;
            end if;
         end loop;

         return True;
      end P_Positions_Swapped;

      ---------------------------
      -- P_Positions_Truncated --
      ---------------------------

      function P_Positions_Truncated
        (Small : P.Map;
         Big   : P.Map;
         Cut   : Positive_Count_Type;
         Count : Count_Type := 1) return Boolean
      is
      begin
         for Cu of Small loop
            if not P.Has_Key (Big, Cu) then
               return False;
            end if;
         end loop;

         for Cu of Big loop
            declare
               Pos : constant Positive_Count_Type := P.Get (Big, Cu);

            begin
               if Pos < Cut then
                  if not P.Has_Key (Small, Cu)
                    or else Pos /= P.Get (Small, Cu)
                  then
                     return False;
                  end if;

               elsif Pos >= Cut + Count then
                  return False;

               elsif P.Has_Key (Small, Cu) then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end P_Positions_Truncated;

      ---------------
      -- Positions --
      ---------------

      function Positions (Container : List) return P.Map is
         I        : Count_Type := 1;
         Position : Count_Type := Container.First;
         R        : P.Map;

      begin
         --  Can't use First, Next or Element here, since they depend on models
         --  for their postconditions.

         while Position /= 0 loop
            R := P.Add (R, (Node => Position), I);
            pragma Assert (P.Length (R) = I);
            Position := Container.Nodes (Position).Next;
            I := I + 1;
         end loop;

         return R;
      end Positions;

   end Formal_Model;

   ----------
   -- Free --
   ----------

   procedure Free (Container : in out List; X : Count_Type) is
      pragma Assert (X > 0);
      pragma Assert (X <= Container.Capacity);

      N : Node_Array renames Container.Nodes;

   begin
      N (X).Prev := -1;  -- Node is deallocated (not on active list)

      if Container.Free >= 0 then
         N (X).Next := Container.Free;
         Container.Free := X;

      elsif X + 1 = abs Container.Free then
         N (X).Next := 0;  -- Not strictly necessary, but marginally safer
         Container.Free := Container.Free + 1;

      else
         Container.Free := abs Container.Free;

         if Container.Free > Container.Capacity then
            Container.Free := 0;

         else
            for J in Container.Free .. Container.Capacity - 1 loop
               N (J).Next := J + 1;
            end loop;

            N (Container.Capacity).Next := 0;
         end if;

         N (X).Next := Container.Free;
         Container.Free := X;
      end if;
   end Free;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting with SPARK_Mode => Off is

      ------------------
      -- Formal_Model --
      ------------------

      package body Formal_Model is

         -----------------------
         -- M_Elements_Sorted --
         -----------------------

         function M_Elements_Sorted (Container : M.Sequence) return Boolean is
         begin
            if M.Length (Container) = 0 then
               return True;
            end if;

            declare
               E1 : Element_Type := Element (Container, 1);

            begin
               for I in 2 .. M.Length (Container) loop
                  declare
                     E2 : constant Element_Type := Element (Container, I);

                  begin
                     if E2 < E1 then
                        return False;
                     end if;

                     E1 := E2;
                  end;
               end loop;
            end;

            return True;
         end M_Elements_Sorted;

      end Formal_Model;

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : List) return Boolean is
         Nodes : Node_Array renames Container.Nodes;
         Node  : Count_Type := Container.First;

      begin
         for J in 2 .. Container.Length loop
            if Nodes (Nodes (Node).Next).Element < Nodes (Node).Element then
               return False;
            else
               Node := Nodes (Node).Next;
            end if;
         end loop;

         return True;
      end Is_Sorted;

      -----------
      -- Merge --
      -----------

      procedure Merge (Target : in out List; Source : in out List) is
         LN : Node_Array renames Target.Nodes;
         RN : Node_Array renames Source.Nodes;
         LI : Cursor;
         RI : Cursor;

      begin
         if Target'Address = Source'Address then
            raise Program_Error with "Target and Source denote same container";
         end if;

         LI := First (Target);
         RI := First (Source);
         while RI.Node /= 0 loop
            pragma Assert
              (RN (RI.Node).Next = 0
                or else not (RN (RN (RI.Node).Next).Element <
                             RN (RI.Node).Element));

            if LI.Node = 0 then
               Splice (Target, No_Element, Source);
               return;
            end if;

            pragma Assert
              (LN (LI.Node).Next = 0
                or else not (LN (LN (LI.Node).Next).Element <
                             LN (LI.Node).Element));

            if RN (RI.Node).Element < LN (LI.Node).Element then
               declare
                  RJ : Cursor := RI;
                  pragma Warnings (Off, RJ);
               begin
                  RI.Node := RN (RI.Node).Next;
                  Splice (Target, LI, Source, RJ);
               end;

            else
               LI.Node := LN (LI.Node).Next;
            end if;
         end loop;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out List) is
         N : Node_Array renames Container.Nodes;

         procedure Partition (Pivot : Count_Type; Back : Count_Type);
         procedure Sort (Front : Count_Type; Back : Count_Type);

         ---------------
         -- Partition --
         ---------------

         procedure Partition (Pivot : Count_Type; Back : Count_Type) is
            Node : Count_Type;

         begin
            Node := N (Pivot).Next;
            while Node /= Back loop
               if N (Node).Element < N (Pivot).Element then
                  declare
                     Prev : constant Count_Type := N (Node).Prev;
                     Next : constant Count_Type := N (Node).Next;

                  begin
                     N (Prev).Next := Next;

                     if Next = 0 then
                        Container.Last := Prev;
                     else
                        N (Next).Prev := Prev;
                     end if;

                     N (Node).Next := Pivot;
                     N (Node).Prev := N (Pivot).Prev;

                     N (Pivot).Prev := Node;

                     if N (Node).Prev = 0 then
                        Container.First := Node;
                     else
                        N (N (Node).Prev).Next := Node;
                     end if;

                     Node := Next;
                  end;

               else
                  Node := N (Node).Next;
               end if;
            end loop;
         end Partition;

         ----------
         -- Sort --
         ----------

         procedure Sort (Front : Count_Type; Back : Count_Type) is
            Pivot : Count_Type;

         begin
            if Front = 0 then
               Pivot := Container.First;
            else
               Pivot := N (Front).Next;
            end if;

            if Pivot /= Back then
               Partition (Pivot, Back);
               Sort (Front, Pivot);
               Sort (Pivot, Back);
            end if;
         end Sort;

      --  Start of processing for Sort

      begin
         if Container.Length <= 1 then
            return;
         end if;

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);

         Sort (Front => 0, Back => 0);

         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : List; Position : Cursor) return Boolean is
   begin
      if Position.Node = 0 then
         return False;
      end if;

      return Container.Nodes (Position.Node).Prev /= -1;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type)
   is
      J : Count_Type;

   begin
      if Before.Node /= 0 then
         pragma Assert (Vet (Container, Before), "bad cursor in Insert");
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      if Container.Length > Container.Capacity - Count then
         raise Constraint_Error with "new length exceeds capacity";
      end if;

      Allocate (Container, New_Item, New_Node => J);
      Insert_Internal (Container, Before.Node, New_Node => J);
      Position := (Node => J);

      for Index in 2 .. Count loop
         Allocate (Container, New_Item, New_Node => J);
         Insert_Internal (Container, Before.Node, New_Node => J);
      end loop;
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor)
   is
   begin
      Insert
        (Container => Container,
         Before    => Before,
         New_Item  => New_Item,
         Position  => Position,
         Count     => 1);
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
      Position : Cursor;

   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type)
   is
      Position : Cursor;

   begin
      Insert (Container, Before, New_Item, Position, 1);
   end Insert;

   ---------------------
   -- Insert_Internal --
   ---------------------

   procedure Insert_Internal
     (Container : in out List;
      Before    : Count_Type;
      New_Node  : Count_Type)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Container.Length = 0 then
         pragma Assert (Before = 0);
         pragma Assert (Container.First = 0);
         pragma Assert (Container.Last = 0);

         Container.First := New_Node;
         Container.Last := New_Node;

         N (Container.First).Prev := 0;
         N (Container.Last).Next := 0;

      elsif Before = 0 then
         pragma Assert (N (Container.Last).Next = 0);

         N (Container.Last).Next := New_Node;
         N (New_Node).Prev := Container.Last;

         Container.Last := New_Node;
         N (Container.Last).Next := 0;

      elsif Before = Container.First then
         pragma Assert (N (Container.First).Prev = 0);

         N (Container.First).Prev := New_Node;
         N (New_Node).Next := Container.First;

         Container.First := New_Node;
         N (Container.First).Prev := 0;

      else
         pragma Assert (N (Container.First).Prev = 0);
         pragma Assert (N (Container.Last).Next = 0);

         N (New_Node).Next := Before;
         N (New_Node).Prev := N (Before).Prev;

         N (N (Before).Prev).Next := New_Node;
         N (Before).Prev := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Insert_Internal;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : List) return Boolean is
   begin
      return Length (Container) = 0;
   end Is_Empty;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = 0 then
         return No_Element;
      end if;

      return (Node => Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
      L : constant Count_Type := Container.Last;

   begin
      if L = 0 then
         raise Constraint_Error with "list is empty";
      else
         return Container.Nodes (L).Element;
      end if;
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out List; Source : in out List) is
      N : Node_Array renames Source.Nodes;
      X : Count_Type;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < Source.Length then
         raise Constraint_Error with  -- ???
           "Source length exceeds Target capacity";
      end if;

      Clear (Target);

      while Source.Length > 1 loop
         pragma Assert (Source.First in 1 .. Source.Capacity);
         pragma Assert (Source.Last /= Source.First);
         pragma Assert (N (Source.First).Prev = 0);
         pragma Assert (N (Source.Last).Next = 0);

         --  Copy first element from Source to Target

         X := Source.First;
         Append (Target, N (X).Element);  -- optimize away???

         --  Unlink first node of Source

         Source.First := N (X).Next;
         N (Source.First).Prev := 0;

         Source.Length := Source.Length - 1;

         --  The representation invariants for Source have been restored. It is
         --  now safe to free the unlinked node, without fear of corrupting the
         --  active links of Source.

         --  Note that the algorithm we use here models similar algorithms used
         --  in the unbounded form of the doubly-linked list container. In that
         --  case, Free is an instantation of Unchecked_Deallocation, which can
         --  fail (because PE will be raised if controlled Finalize fails), so
         --  we must defer the call until the last step. Here in the bounded
         --  form, Free merely links the node we have just "deallocated" onto a
         --  list of inactive nodes, so technically Free cannot fail. However,
         --  for consistency, we handle Free the same way here as we do for the
         --  unbounded form, with the pessimistic assumption that it can fail.

         Free (Source, X);
      end loop;

      if Source.Length = 1 then
         pragma Assert (Source.First in 1 .. Source.Capacity);
         pragma Assert (Source.Last = Source.First);
         pragma Assert (N (Source.First).Prev = 0);
         pragma Assert (N (Source.Last).Next = 0);

         --  Copy element from Source to Target

         X := Source.First;
         Append (Target, N (X).Element);

         --  Unlink node of Source

         Source.First := 0;
         Source.Last := 0;
         Source.Length := 0;

         --  Return the unlinked node to the free store

         Free (Source, X);
      end if;
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Container : List; Position : in out Cursor) is
   begin
      Position := Next (Container, Position);
   end Next;

   function Next (Container : List; Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Program_Error with "Position cursor has no element";
      end if;

      return (Node => Container.Nodes (Position.Node).Next);
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in out List; New_Item : Element_Type) is
   begin
      Insert (Container, First (Container), New_Item, 1);
   end Prepend;

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
   begin
      Insert (Container, First (Container), New_Item, Count);
   end Prepend;

   --------------
   -- Previous --
   --------------

   procedure Previous (Container : List; Position : in out Cursor) is
   begin
      Position := Previous (Container, Position);
   end Previous;

   function Previous (Container : List; Position : Cursor) return Cursor is
   begin
      if Position.Node = 0 then
         return No_Element;
      end if;

      if not Has_Element (Container, Position) then
         raise Program_Error with "Position cursor has no element";
      end if;

      return (Node => Container.Nodes (Position.Node).Prev);
   end Previous;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if not Has_Element (Container, Position) then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert
        (Vet (Container, Position), "bad cursor in Replace_Element");

      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out List) is
      N : Node_Array renames Container.Nodes;
      I : Count_Type := Container.First;
      J : Count_Type := Container.Last;

      procedure Swap (L : Count_Type; R : Count_Type);

      ----------
      -- Swap --
      ----------

      procedure Swap (L : Count_Type; R : Count_Type) is
         LN : constant Count_Type := N (L).Next;
         LP : constant Count_Type := N (L).Prev;

         RN : constant Count_Type := N (R).Next;
         RP : constant Count_Type := N (R).Prev;

      begin
         if LP /= 0 then
            N (LP).Next := R;
         end if;

         if RN /= 0 then
            N (RN).Prev := L;
         end if;

         N (L).Next := RN;
         N (R).Prev := LP;

         if LN = R then
            pragma Assert (RP = L);

            N (L).Prev := R;
            N (R).Next := L;

         else
            N (L).Prev := RP;
            N (RP).Next := L;

            N (R).Next := LN;
            N (LN).Prev := R;
         end if;
      end Swap;

   --  Start of processing for Reverse_Elements

   begin
      if Container.Length <= 1 then
         return;
      end if;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);

      Container.First := J;
      Container.Last  := I;
      loop
         Swap (L => I, R => J);

         J := N (J).Next;
         exit when I = J;

         I := N (I).Prev;
         exit when I = J;

         Swap (L => J, R => I);

         I := N (I).Next;
         exit when I = J;

         J := N (J).Prev;
         exit when I = J;
      end loop;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);
   end Reverse_Elements;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      CFirst : Count_Type := Position.Node;

   begin
      if CFirst = 0 then
         CFirst := Container.Last;
      end if;

      if Container.Length = 0 then
         return No_Element;

      else
         while CFirst /= 0 loop
            if Container.Nodes (CFirst).Element = Item then
               return (Node => CFirst);
            else
               CFirst := Container.Nodes (CFirst).Prev;
            end if;
         end loop;

         return No_Element;
      end if;
   end Reverse_Find;

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List)
   is
      SN : Node_Array renames Source.Nodes;

   begin
      if Target'Address = Source'Address then
         raise Program_Error with "Target and Source denote same container";
      end if;

      if Before.Node /= 0 then
         pragma Assert (Vet (Target, Before), "bad cursor in Splice");
      end if;

      pragma Assert (SN (Source.First).Prev = 0);
      pragma Assert (SN (Source.Last).Next  = 0);

      if Target.Length > Count_Type'Base'Last - Source.Length then
         raise Constraint_Error with "new length exceeds maximum";
      end if;

      if Target.Length + Source.Length > Target.Capacity then
         raise Constraint_Error;
      end if;

      loop
         Insert (Target, Before, SN (Source.Last).Element);
         Delete_Last (Source);
         exit when Is_Empty (Source);
      end loop;
   end Splice;

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor)
   is
      Target_Position : Cursor;

   begin
      if Target'Address = Source'Address then
         raise Program_Error with "Target and Source denote same container";
      end if;

      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert (Vet (Source, Position), "bad Position cursor in Splice");

      if Target.Length >= Target.Capacity then
         raise Constraint_Error;
      end if;

      Insert
        (Container => Target,
         Before    => Before,
         New_Item  => Source.Nodes (Position.Node).Element,
         Position  => Target_Position);

      Delete (Source, Position);
      Position := Target_Position;
   end Splice;

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   is
      N : Node_Array renames Container.Nodes;

   begin
      if Before.Node /= 0 then
         pragma Assert
           (Vet (Container, Before), "bad Before cursor in Splice");
      end if;

      if Position.Node = 0 then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      pragma Assert
        (Vet (Container, Position), "bad Position cursor in Splice");

      if Position.Node = Before.Node
        or else N (Position.Node).Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

      if Before.Node = 0 then
         pragma Assert (Position.Node /= Container.Last);

         if Position.Node = Container.First then
            Container.First := N (Position.Node).Next;
            N (Container.First).Prev := 0;

         else
            N (N (Position.Node).Prev).Next := N (Position.Node).Next;
            N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
         end if;

         N (Container.Last).Next := Position.Node;
         N (Position.Node).Prev := Container.Last;

         Container.Last := Position.Node;
         N (Container.Last).Next := 0;

         return;
      end if;

      if Before.Node = Container.First then
         pragma Assert (Position.Node /= Container.First);

         if Position.Node = Container.Last then
            Container.Last := N (Position.Node).Prev;
            N (Container.Last).Next := 0;

         else
            N (N (Position.Node).Prev).Next := N (Position.Node).Next;
            N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
         end if;

         N (Container.First).Prev := Position.Node;
         N (Position.Node).Next := Container.First;

         Container.First := Position.Node;
         N (Container.First).Prev := 0;

         return;
      end if;

      if Position.Node = Container.First then
         Container.First := N (Position.Node).Next;
         N (Container.First).Prev := 0;

      elsif Position.Node = Container.Last then
         Container.Last := N (Position.Node).Prev;
         N (Container.Last).Next := 0;

      else
         N (N (Position.Node).Prev).Next := N (Position.Node).Next;
         N (N (Position.Node).Next).Prev := N (Position.Node).Prev;
      end if;

      N (N (Before.Node).Prev).Next := Position.Node;
      N (Position.Node).Prev := N (Before.Node).Prev;

      N (Before.Node).Prev := Position.Node;
      N (Position.Node).Next := Before.Node;

      pragma Assert (N (Container.First).Prev = 0);
      pragma Assert (N (Container.Last).Next = 0);
   end Splice;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out List;
      I         : Cursor;
      J         : Cursor)
   is
   begin
      if I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = 0 then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      pragma Assert (Vet (Container, I), "bad I cursor in Swap");
      pragma Assert (Vet (Container, J), "bad J cursor in Swap");

      declare
         NN : Node_Array renames Container.Nodes;
         NI : Node_Type renames NN (I.Node);
         NJ : Node_Type renames NN (J.Node);

         EI_Copy : constant Element_Type := NI.Element;

      begin
         NI.Element := NJ.Element;
         NJ.Element := EI_Copy;
      end;
   end Swap;

   ----------------
   -- Swap_Links --
   ----------------

   procedure Swap_Links
     (Container : in out List;
      I         : Cursor;
      J         : Cursor)
   is
      I_Next : Cursor;
      J_Next : Cursor;

   begin
      if I.Node = 0 then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Node = 0 then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Node = J.Node then
         return;
      end if;

      pragma Assert (Vet (Container, I), "bad I cursor in Swap_Links");
      pragma Assert (Vet (Container, J), "bad J cursor in Swap_Links");

      I_Next := Next (Container, I);

      if I_Next = J then
         Splice (Container, Before => I, Position => J);

      else
         J_Next := Next (Container, J);

         if J_Next = I then
            Splice (Container, Before => J, Position => I);

         else
            pragma Assert (Container.Length >= 3);
            Splice (Container, Before => I_Next, Position => J);
            Splice (Container, Before => J_Next, Position => I);
         end if;
      end if;
   end Swap_Links;

   ---------
   -- Vet --
   ---------

   function Vet (L : List; Position : Cursor) return Boolean is
      N : Node_Array renames L.Nodes;

   begin
      if L.Length = 0 then
         return False;
      end if;

      if L.First = 0 then
         return False;
      end if;

      if L.Last = 0 then
         return False;
      end if;

      if Position.Node > L.Capacity then
         return False;
      end if;

      if N (Position.Node).Prev < 0
        or else N (Position.Node).Prev > L.Capacity
      then
         return False;
      end if;

      if N (Position.Node).Next > L.Capacity then
         return False;
      end if;

      if N (L.First).Prev /= 0 then
         return False;
      end if;

      if N (L.Last).Next /= 0 then
         return False;
      end if;

      if N (Position.Node).Prev = 0 and then Position.Node /= L.First then
         return False;
      end if;

      if N (Position.Node).Next = 0 and then Position.Node /= L.Last then
         return False;
      end if;

      if L.Length = 1 then
         return L.First = L.Last;
      end if;

      if L.First = L.Last then
         return False;
      end if;

      if N (L.First).Next = 0 then
         return False;
      end if;

      if N (L.Last).Prev = 0 then
         return False;
      end if;

      if N (N (L.First).Next).Prev /= L.First then
         return False;
      end if;

      if N (N (L.Last).Prev).Next /= L.Last then
         return False;
      end if;

      if L.Length = 2 then
         if N (L.First).Next /= L.Last then
            return False;
         end if;

         if N (L.Last).Prev /= L.First then
            return False;
         end if;

         return True;
      end if;

      if N (L.First).Next = L.Last then
         return False;
      end if;

      if N (L.Last).Prev = L.First then
         return False;
      end if;

      if Position.Node = L.First then
         return True;
      end if;

      if Position.Node = L.Last then
         return True;
      end if;

      if N (Position.Node).Next = 0 then
         return False;
      end if;

      if N (Position.Node).Prev = 0 then
         return False;
      end if;

      if N (N (Position.Node).Next).Prev /= Position.Node then
         return False;
      end if;

      if N (N (Position.Node).Prev).Next /= Position.Node then
         return False;
      end if;

      if L.Length = 3 then
         if N (L.First).Next /= Position.Node then
            return False;
         end if;

         if N (L.Last).Prev /= Position.Node then
            return False;
         end if;
      end if;

      return True;
   end Vet;

end Ada.Containers.Formal_Doubly_Linked_Lists;
