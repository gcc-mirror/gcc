------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . F O R M A L _ V E C T O R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2019, Free Software Foundation, Inc.         --
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

with Ada.Containers.Generic_Array_Sort;

with System; use type System.Address;

package body Ada.Containers.Formal_Vectors with
  SPARK_Mode => Off
is

   type Int is range System.Min_Int .. System.Max_Int;

   function To_Array_Index (Index : Index_Type'Base) return Count_Type'Base;

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1);

   ---------
   -- "=" --
   ---------

   function "=" (Left : Vector; Right : Vector) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Length (Left) /= Length (Right) then
         return False;
      end if;

      for J in 1 .. Length (Left) loop
         if Left.Elements (J) /= Right.Elements (J) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      if Is_Empty (New_Item) then
         return;
      end if;

      if Container.Last >= Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      end if;

      Insert (Container, Container.Last + 1, New_Item);
   end Append;

   procedure Append (Container : in out Vector; New_Item : Element_Type) is
   begin
      Append (Container, New_Item, 1);
   end Append;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
   begin
      if Count = 0 then
         return;
      end if;

      if Container.Last >= Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      end if;

      Insert (Container, Container.Last + 1, New_Item, Count);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Vector; Source : Vector) is
      LS : constant Capacity_Range := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < LS then
         raise Constraint_Error;
      end if;

      Clear (Target);
      Append (Target, Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Capacity_Range is
   begin
      return Container.Capacity;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Container.Last := No_Index;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find_Index (Container, Item) /= No_Index;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : Vector;
      Capacity : Capacity_Range := 0) return Vector
   is
      LS : constant Capacity_Range := Length (Source);
      C  : Capacity_Range;

   begin
      if Capacity = 0 then
         C := LS;
      elsif Capacity >= LS then
         C := Capacity;
      else
         raise Capacity_Error;
      end if;

      return Target : Vector (C) do
         Target.Elements (1 .. LS) := Source.Elements (1 .. LS);
         Target.Last := Source.Last;
      end return;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Vector; Index : Extended_Index) is
   begin
      Delete (Container, Index, 1);
   end Delete;

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type)
   is
      Old_Last : constant Index_Type'Base := Container.Last;
      Old_Len  : constant Count_Type := Length (Container);
      New_Last : Index_Type'Base;
      Count2   : Count_Type'Base;  -- count of items from Index to Old_Last
      Off      : Count_Type'Base;  -- Index expressed as offset from IT'First

   begin
      --  Delete removes items from the vector, the number of which is the
      --  minimum of the specified Count and the items (if any) that exist from
      --  Index to Container.Last. There are no constraints on the specified
      --  value of Count (it can be larger than what's available at this
      --  position in the vector, for example), but there are constraints on
      --  the allowed values of the Index.

      --  As a precondition on the generic actual Index_Type, the base type
      --  must include Index_Type'Pred (Index_Type'First); this is the value
      --  that Container.Last assumes when the vector is empty. However, we do
      --  not allow that as the value for Index when specifying which items
      --  should be deleted, so we must manually check. (That the user is
      --  allowed to specify the value at all here is a consequence of the
      --  declaration of the Extended_Index subtype, which includes the values
      --  in the base range that immediately precede and immediately follow the
      --  values in the Index_Type.)

      if Index < Index_Type'First then
         raise Constraint_Error with "Index is out of range (too small)";
      end if;

      --  We do allow a value greater than Container.Last to be specified as
      --  the Index, but only if it's immediately greater. This allows the
      --  corner case of deleting no items from the back end of the vector to
      --  be treated as a no-op. (It is assumed that specifying an index value
      --  greater than Last + 1 indicates some deeper flaw in the caller's
      --  algorithm, so that case is treated as a proper error.)

      if Index > Old_Last then
         if Index > Old_Last + 1 then
            raise Constraint_Error with "Index is out of range (too large)";
         end if;

         return;
      end if;

      if Count = 0 then
         return;
      end if;

      --  We first calculate what's available for deletion starting at
      --  Index. Here and elsewhere we use the wider of Index_Type'Base and
      --  Count_Type'Base as the type for intermediate values. (See function
      --  Length for more information.)

      if Count_Type'Base'Last >= Index_Type'Pos (Index_Type'Base'Last) then
         Count2 := Count_Type'Base (Old_Last) - Count_Type'Base (Index) + 1;
      else
         Count2 := Count_Type'Base (Old_Last - Index + 1);
      end if;

      --  If more elements are requested (Count) for deletion than are
      --  available (Count2) for deletion beginning at Index, then everything
      --  from Index is deleted. There are no elements to slide down, and so
      --  all we need to do is set the value of Container.Last.

      if Count >= Count2 then
         Container.Last := Index - 1;
         return;
      end if;

      --  There are some elements aren't being deleted (the requested count was
      --  less than the available count), so we must slide them down to Index.
      --  We first calculate the index values of the respective array slices,
      --  using the wider of Index_Type'Base and Count_Type'Base as the type
      --  for intermediate calculations.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Off := Count_Type'Base (Index - Index_Type'First);
         New_Last := Old_Last - Index_Type'Base (Count);
      else
         Off := Count_Type'Base (Index) - Count_Type'Base (Index_Type'First);
         New_Last := Index_Type'Base (Count_Type'Base (Old_Last) - Count);
      end if;

      --  The array index values for each slice have already been determined,
      --  so we just slide down to Index the elements that weren't deleted.

      declare
         EA  : Elements_Array renames Container.Elements;
         Idx : constant Count_Type := EA'First + Off;
      begin
         EA (Idx .. Old_Len - Count) := EA (Idx + Count .. Old_Len);
         Container.Last := New_Last;
      end;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in out Vector) is
   begin
      Delete_First (Container, 1);
   end Delete_First;

   procedure Delete_First (Container : in out Vector; Count : Count_Type) is
   begin
      if Count = 0 then
         return;

      elsif Count >= Length (Container) then
         Clear (Container);
         return;

      else
         Delete (Container, Index_Type'First, Count);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Vector) is
   begin
      Delete_Last (Container, 1);
   end Delete_Last;

   procedure Delete_Last (Container : in out Vector; Count : Count_Type) is
   begin
      if Count = 0 then
         return;
      end if;

      --  There is no restriction on how large Count can be when deleting
      --  items. If it is equal or greater than the current length, then this
      --  is equivalent to clearing the vector. (In particular, there's no need
      --  for us to actually calculate the new value for Last.)

      --  If the requested count is less than the current length, then we must
      --  calculate the new value for Last. For the type we use the widest of
      --  Index_Type'Base and Count_Type'Base for the intermediate values of
      --  our calculation.  (See the comments in Length for more information.)

      if Count >= Length (Container) then
         Container.Last := No_Index;

      elsif Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Container.Last := Container.Last - Index_Type'Base (Count);

      else
         Container.Last :=
           Index_Type'Base (Count_Type'Base (Container.Last) - Count);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Capacity_Range := Capacity_Range (II);
      begin
         return Container.Elements (I);
      end;
   end Element;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
      K    : Count_Type;
      Last : constant Index_Type := Last_Index (Container);

   begin
      K := Capacity_Range (Int (Index) - Int (No_Index));
      for Indx in Index .. Last loop
         if Container.Elements (K) = Item then
            return Indx;
         end if;

         K := K + 1;
      end loop;

      return No_Index;
   end Find_Index;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      else
         return Container.Elements (1);
      end if;
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   ------------------
   -- Formal_Model --
   ------------------

   package body Formal_Model is

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
         for Index in Index_Type'First .. M.Last (Container) loop
            Elem := Element (Container, Index);

            if not M.Contains (Left, Index_Type'First, M.Last (Left), Elem)
              and then
                not M.Contains (Right, Index_Type'First, M.Last (Right), Elem)
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
         L_Fst : Index_Type := Index_Type'First;
         L_Lst : Extended_Index;
         Right : M.Sequence;
         R_Fst : Index_Type := Index_Type'First;
         R_Lst : Extended_Index) return Boolean
      is
      begin
         for I in L_Fst .. L_Lst loop
            declare
               Found : Boolean := False;
               J     : Extended_Index := R_Fst - 1;

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
         L : constant Index_Type := M.Last (Left);

      begin
         if L /= M.Last (Right) then
            return False;
         end if;

         for I in Index_Type'First .. L loop
            if Element (Left, I) /= Element (Right, L - I + 1)
            then
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
         X     : Index_Type;
         Y     : Index_Type) return Boolean
      is
      begin
         if M.Length (Left) /= M.Length (Right)
           or else Element (Left, X) /= Element (Right, Y)
           or else Element (Left, Y) /= Element (Right, X)
         then
            return False;
         end if;

         for I in Index_Type'First .. M.Last (Left) loop
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

      function Model (Container : Vector) return M.Sequence is
         R : M.Sequence;

      begin
         for Position in 1 .. Length (Container) loop
            R := M.Add (R, Container.Elements (Position));
         end loop;

         return R;
      end Model;

   end Formal_Model;

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
               E1 : Element_Type := Element (Container, Index_Type'First);

            begin
               for I in Index_Type'First + 1 .. M.Last (Container) loop
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

      function Is_Sorted (Container : Vector) return Boolean is
         L : constant Capacity_Range := Length (Container);

      begin
         for J in 1 .. L - 1 loop
            if Container.Elements (J + 1) <
               Container.Elements (J)
            then
               return False;
            end if;
         end loop;

         return True;
      end Is_Sorted;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector) is
         procedure Sort is
           new Generic_Array_Sort
                 (Index_Type   => Array_Index,
                  Element_Type => Element_Type,
                  Array_Type   => Elements_Array,
                  "<"          => "<");

         Len : constant Capacity_Range := Length (Container);

      begin
         if Container.Last <= Index_Type'First then
            return;
         else
            Sort (Container.Elements (1 .. Len));
         end if;
      end Sort;

      -----------
      -- Merge --
      -----------

      procedure Merge (Target : in out Vector; Source : in out Vector) is
         I : Count_Type;
         J : Count_Type;

      begin
         if Target'Address = Source'Address then
            raise Program_Error with "Target and Source denote same container";
         end if;

         if Length (Source) = 0 then
            return;
         end if;

         if Length (Target) = 0 then
            Move (Target => Target, Source => Source);
            return;
         end if;

         I := Length (Target);

         declare
            New_Length : constant Count_Type := I + Length (Source);

         begin
            if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
               Target.Last := No_Index + Index_Type'Base (New_Length);

            else
               Target.Last :=
                 Index_Type'Base (Count_Type'Base (No_Index) + New_Length);
            end if;
         end;

         declare
            TA : Elements_Array renames Target.Elements;
            SA : Elements_Array renames Source.Elements;

         begin
            J := Length (Target);
            while Length (Source) /= 0 loop
               if I = 0 then
                  TA (1 .. J) := SA (1 .. Length (Source));
                  Source.Last := No_Index;
                  exit;
               end if;

               if SA (Length (Source)) < TA (I) then
                  TA (J) := TA (I);
                  I := I - 1;

               else
                  TA (J) := SA (Length (Source));
                  Source.Last := Source.Last - 1;
               end if;

               J := J - 1;
            end loop;
         end;
      end Merge;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
   is
   begin
      return Position in First_Index (Container) .. Last_Index (Container);
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type)
   is
   begin
      Insert (Container, Before, New_Item, 1);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
      J : Count_Type'Base;  -- scratch

   begin
      --  Use Insert_Space to create the "hole" (the destination slice)

      Insert_Space (Container, Before, Count);

      J := To_Array_Index (Before);

      Container.Elements (J .. J - 1 + Count) := (others => New_Item);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   is
      N : constant Count_Type := Length (New_Item);
      B : Count_Type;  -- index Before converted to Count_Type

   begin
      if Container'Address = New_Item'Address then
         raise Program_Error with
           "Container and New_Item denote same container";
      end if;

      --  Use Insert_Space to create the "hole" (the destination slice) into
      --  which we copy the source items.

      Insert_Space (Container, Before, Count => N);

      if N = 0 then

         --  There's nothing else to do here (vetting of parameters was
         --  performed already in Insert_Space), so we simply return.

         return;
      end if;

      B := To_Array_Index (Before);

      Container.Elements (B .. B + N - 1) := New_Item.Elements (1 .. N);
   end Insert;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1)
   is
      Old_Length : constant Count_Type := Length (Container);

      Max_Length : Count_Type'Base;  -- determined from range of Index_Type
      New_Length : Count_Type'Base;  -- sum of current length and Count

      Index : Index_Type'Base;  -- scratch for intermediate values
      J     : Count_Type'Base;  -- scratch

   begin
      --  As a precondition on the generic actual Index_Type, the base type
      --  must include Index_Type'Pred (Index_Type'First); this is the value
      --  that Container.Last assumes when the vector is empty. However, we do
      --  not allow that as the value for Index when specifying where the new
      --  items should be inserted, so we must manually check. (That the user
      --  is allowed to specify the value at all here is a consequence of the
      --  declaration of the Extended_Index subtype, which includes the values
      --  in the base range that immediately precede and immediately follow the
      --  values in the Index_Type.)

      if Before < Index_Type'First then
         raise Constraint_Error with
           "Before index is out of range (too small)";
      end if;

      --  We do allow a value greater than Container.Last to be specified as
      --  the Index, but only if it's immediately greater. This allows for the
      --  case of appending items to the back end of the vector. (It is assumed
      --  that specifying an index value greater than Last + 1 indicates some
      --  deeper flaw in the caller's algorithm, so that case is treated as a
      --  proper error.)

      if Before > Container.Last
        and then Before - 1 > Container.Last
      then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      --  We treat inserting 0 items into the container as a no-op, so we
      --  simply return.

      if Count = 0 then
         return;
      end if;

      --  There are two constraints we need to satisfy. The first constraint is
      --  that a container cannot have more than Count_Type'Last elements, so
      --  we must check the sum of the current length and the insertion count.
      --  Note that the value cannot be simply added because the result may
      --  overflow.

      if Old_Length > Count_Type'Last - Count then
         raise Constraint_Error with "Count is out of range";
      end if;

      --  It is now safe compute the length of the new vector, without fear of
      --  overflow.

      New_Length := Old_Length + Count;

      --  The second constraint is that the new Last index value cannot exceed
      --  Index_Type'Last. In each branch below, we calculate the maximum
      --  length (computed from the range of values in Index_Type), and then
      --  compare the new length to the maximum length. If the new length is
      --  acceptable, then we compute the new last index from that.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then

         --  We have to handle the case when there might be more values in the
         --  range of Index_Type than in the range of Count_Type.

         if Index_Type'First <= 0 then

            --  We know that No_Index (the same as Index_Type'First - 1) is
            --  less than 0, so it is safe to compute the following sum without
            --  fear of overflow.

            Index := No_Index + Index_Type'Base (Count_Type'Last);

            if Index <= Index_Type'Last then

               --  We have determined that range of Index_Type has at least as
               --  many values as in Count_Type, so Count_Type'Last is the
               --  maximum number of items that are allowed.

               Max_Length := Count_Type'Last;

            else
               --  The range of Index_Type has fewer values than in Count_Type,
               --  so the maximum number of items is computed from the range of
               --  the Index_Type.

               Max_Length := Count_Type'Base (Index_Type'Last - No_Index);
            end if;

         else
            --  No_Index is equal or greater than 0, so we can safely compute
            --  the difference without fear of overflow (which we would have to
            --  worry about if No_Index were less than 0, but that case is
            --  handled above).

            if Index_Type'Last - No_Index >= Count_Type'Pos (Count_Type'Last)
            then
               --  We have determined that range of Index_Type has at least as
               --  many values as in Count_Type, so Count_Type'Last is the
               --  maximum number of items that are allowed.

               Max_Length := Count_Type'Last;

            else
               --  The range of Index_Type has fewer values than in Count_Type,
               --  so the maximum number of items is computed from the range of
               --  the Index_Type.

               Max_Length := Count_Type'Base (Index_Type'Last - No_Index);
            end if;
         end if;

      elsif Index_Type'First <= 0 then

         --  We know that No_Index (the same as Index_Type'First - 1) is less
         --  than 0, so it is safe to compute the following sum without fear of
         --  overflow.

         J := Count_Type'Base (No_Index) + Count_Type'Last;

         if J <= Count_Type'Base (Index_Type'Last) then

            --  We have determined that range of Index_Type has at least as
            --  many values as in Count_Type, so Count_Type'Last is the maximum
            --  number of items that are allowed.

            Max_Length := Count_Type'Last;

         else
            --  The range of Index_Type has fewer values than Count_Type does,
            --  so the maximum number of items is computed from the range of
            --  the Index_Type.

            Max_Length :=
              Count_Type'Base (Index_Type'Last) - Count_Type'Base (No_Index);
         end if;

      else
         --  No_Index is equal or greater than 0, so we can safely compute the
         --  difference without fear of overflow (which we would have to worry
         --  about if No_Index were less than 0, but that case is handled
         --  above).

         Max_Length :=
           Count_Type'Base (Index_Type'Last) - Count_Type'Base (No_Index);
      end if;

      --  We have just computed the maximum length (number of items). We must
      --  now compare the requested length to the maximum length, as we do not
      --  allow a vector expand beyond the maximum (because that would create
      --  an internal array with a last index value greater than
      --  Index_Type'Last, with no way to index those elements).

      if New_Length > Max_Length then
         raise Constraint_Error with "Count is out of range";
      end if;

      J := To_Array_Index (Before);

      declare
         EA : Elements_Array renames Container.Elements;

      begin
         if Before <= Container.Last then

            --  The new items are being inserted before some existing
            --  elements, so we must slide the existing elements up to their
            --  new home.

            EA (J + Count .. New_Length) := EA (J .. Old_Length);
         end if;
      end;

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Container.Last := No_Index + Index_Type'Base (New_Length);

      else
         Container.Last :=
           Index_Type'Base (Count_Type'Base (No_Index) + New_Length);
      end if;
   end Insert_Space;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Last_Index (Container) < Index_Type'First;
   end Is_Empty;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      else
         return Container.Elements (Length (Container));
      end if;
   end Last_Element;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Last;
   end Last_Index;

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Capacity_Range is
      L : constant Int := Int (Container.Last);
      F : constant Int := Int (Index_Type'First);
      N : constant Int'Base := L - F + 1;

   begin
      return Capacity_Range (N);
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Vector; Source : in out Vector) is
      LS : constant Capacity_Range := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < LS then
         raise Constraint_Error;
      end if;

      Clear (Target);
      Append (Target, Source);
      Clear (Source);
   end Move;

   ------------
   -- Prepend --
   ------------

   procedure Prepend (Container : in out Vector; New_Item : Vector) is
   begin
      Insert (Container, Index_Type'First, New_Item);
   end Prepend;

   procedure Prepend (Container : in out Vector; New_Item : Element_Type) is
   begin
      Prepend (Container, New_Item, 1);
   end Prepend;

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type)
   is
   begin
      Insert (Container, Index_Type'First, New_Item, Count);
   end Prepend;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Capacity_Range := Capacity_Range (II);

      begin
         Container.Elements (I) := New_Item;
      end;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Capacity_Range)
   is
   begin
      if Capacity > Container.Capacity then
         raise Constraint_Error with "Capacity is out of range";
      end if;
   end Reserve_Capacity;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      if Length (Container) <= 1 then
         return;
      end if;

      declare
         I, J : Capacity_Range;
         E    : Elements_Array renames
                  Container.Elements (1 .. Length (Container));

      begin
         I := 1;
         J := Length (Container);
         while I < J loop
            declare
               EI : constant Element_Type := E (I);

            begin
               E (I) := E (J);
               E (J) := EI;
            end;

            I := I + 1;
            J := J - 1;
         end loop;
      end;
   end Reverse_Elements;

   ------------------------
   -- Reverse_Find_Index --
   ------------------------

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   is
      Last : Index_Type'Base;
      K    : Count_Type'Base;

   begin
      if Index > Last_Index (Container) then
         Last := Last_Index (Container);
      else
         Last := Index;
      end if;

      K := Capacity_Range (Int (Last) - Int (No_Index));
      for Indx in reverse Index_Type'First .. Last loop
         if Container.Elements (K) = Item then
            return Indx;
         end if;

         K := K - 1;
      end loop;

      return No_Index;
   end Reverse_Find_Index;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out Vector;
      I         : Index_Type;
      J         : Index_Type)
   is
   begin
      if I > Container.Last then
         raise Constraint_Error with "I index is out of range";
      end if;

      if J > Container.Last then
         raise Constraint_Error with "J index is out of range";
      end if;

      if I = J then
         return;
      end if;

      declare
         II : constant Int'Base := Int (I) - Int (No_Index);
         JJ : constant Int'Base := Int (J) - Int (No_Index);

         EI : Element_Type renames Container.Elements (Capacity_Range (II));
         EJ : Element_Type renames Container.Elements (Capacity_Range (JJ));

         EI_Copy : constant Element_Type := EI;

      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   --------------------
   -- To_Array_Index --
   --------------------

   function To_Array_Index (Index : Index_Type'Base) return Count_Type'Base is
      Offset : Count_Type'Base;

   begin
      --  We know that
      --    Index >= Index_Type'First
      --  hence we also know that
      --    Index - Index_Type'First >= 0

      --  The issue is that even though 0 is guaranteed to be a value in
      --  the type Index_Type'Base, there's no guarantee that the difference
      --  is a value in that type. To prevent overflow we use the wider
      --  of Count_Type'Base and Index_Type'Base to perform intermediate
      --  calculations.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Offset := Count_Type'Base (Index - Index_Type'First);

      else
         Offset :=
           Count_Type'Base (Index) - Count_Type'Base (Index_Type'First);
      end if;

      --  The array index subtype for all container element arrays always
      --  starts with 1.

      return 1 + Offset;
   end To_Array_Index;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector
     (New_Item : Element_Type;
      Length   : Capacity_Range) return Vector
   is
   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      declare
         First       : constant Int := Int (Index_Type'First);
         Last_As_Int : constant Int'Base := First + Int (Length) - 1;
         Last        : Index_Type;

      begin
         if Last_As_Int > Index_Type'Pos (Index_Type'Last) then
            raise Constraint_Error with "Length is out of range";  -- ???
         end if;

         Last := Index_Type (Last_As_Int);

         return
           (Capacity => Length,
            Last     => Last,
            Elements => (others => New_Item));
      end;
   end To_Vector;

end Ada.Containers.Formal_Vectors;
