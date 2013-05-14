------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . I N D E F I N I T E _ V E C T O R S     --
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

with Ada.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;

with System; use type System.Address;

package body Ada.Containers.Indefinite_Vectors is

   procedure Free is
     new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Vector) return Vector is
      LN   : constant Count_Type := Length (Left);
      RN   : constant Count_Type := Length (Right);
      N    : Count_Type'Base;  -- length of result
      J    : Count_Type'Base;  -- for computing intermediate values
      Last : Index_Type'Base;  -- Last index of result

   begin
      --  We decide that the capacity of the result is the sum of the lengths
      --  of the vector parameters. We could decide to make it larger, but we
      --  have no basis for knowing how much larger, so we just allocate the
      --  minimum amount of storage.

      --  Here we handle the easy cases first, when one of the vector
      --  parameters is empty. (We say "easy" because there's nothing to
      --  compute, that can potentially overflow.)

      if LN = 0 then
         if RN = 0 then
            return Empty_Vector;
         end if;

         declare
            RE : Elements_Array renames
                   Right.Elements.EA (Index_Type'First .. Right.Last);

            Elements : Elements_Access := new Elements_Type (Right.Last);

         begin
            --  Elements of an indefinite vector are allocated, so we cannot
            --  use simple slice assignment to give a value to our result.
            --  Hence we must walk the array of the Right vector, and copy
            --  each source element individually.

            for I in Elements.EA'Range loop
               begin
                  if RE (I) /= null then
                     Elements.EA (I) := new Element_Type'(RE (I).all);
                  end if;

               exception
                  when others =>
                     for J in Index_Type'First .. I - 1 loop
                        Free (Elements.EA (J));
                     end loop;

                     Free (Elements);
                     raise;
               end;
            end loop;

            return (Controlled with Elements, Right.Last, 0, 0);
         end;
      end if;

      if RN = 0 then
         declare
            LE : Elements_Array renames
                   Left.Elements.EA (Index_Type'First .. Left.Last);

            Elements : Elements_Access := new Elements_Type (Left.Last);

         begin
            --  Elements of an indefinite vector are allocated, so we cannot
            --  use simple slice assignment to give a value to our result.
            --  Hence we must walk the array of the Left vector, and copy
            --  each source element individually.

            for I in Elements.EA'Range loop
               begin
                  if LE (I) /= null then
                     Elements.EA (I) := new Element_Type'(LE (I).all);
                  end if;

               exception
                  when others =>
                     for J in Index_Type'First .. I - 1 loop
                        Free (Elements.EA (J));
                     end loop;

                     Free (Elements);
                     raise;
               end;
            end loop;

            return (Controlled with Elements, Left.Last, 0, 0);
         end;
      end if;

      --  Neither of the vector parameters is empty, so we must compute the
      --  length of the result vector and its last index. (This is the harder
      --  case, because our computations must avoid overflow.)

      --  There are two constraints we need to satisfy. The first constraint is
      --  that a container cannot have more than Count_Type'Last elements, so
      --  we must check the sum of the combined lengths. Note that we cannot
      --  simply add the lengths, because of the possibility of overflow.

      if LN > Count_Type'Last - RN then
         raise Constraint_Error with "new length is out of range";
      end if;

      --  It is now safe compute the length of the new vector.

      N := LN + RN;

      --  The second constraint is that the new Last index value cannot
      --  exceed Index_Type'Last. We use the wider of Index_Type'Base and
      --  Count_Type'Base as the type for intermediate values.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then

         --  We perform a two-part test. First we determine whether the
         --  computed Last value lies in the base range of the type, and then
         --  determine whether it lies in the range of the index (sub)type.

         --  Last must satisfy this relation:
         --    First + Length - 1 <= Last
         --  We regroup terms:
         --    First - 1 <= Last - Length
         --  Which can rewrite as:
         --    No_Index <= Last - Length

         if Index_Type'Base'Last - Index_Type'Base (N) < No_Index then
            raise Constraint_Error with "new length is out of range";
         end if;

         --  We now know that the computed value of Last is within the base
         --  range of the type, so it is safe to compute its value:

         Last := No_Index + Index_Type'Base (N);

         --  Finally we test whether the value is within the range of the
         --  generic actual index subtype:

         if Last > Index_Type'Last then
            raise Constraint_Error with "new length is out of range";
         end if;

      elsif Index_Type'First <= 0 then

         --  Here we can compute Last directly, in the normal way. We know that
         --  No_Index is less than 0, so there is no danger of overflow when
         --  adding the (positive) value of length.

         J := Count_Type'Base (No_Index) + N;  -- Last

         if J > Count_Type'Base (Index_Type'Last) then
            raise Constraint_Error with "new length is out of range";
         end if;

         --  We know that the computed value (having type Count_Type) of Last
         --  is within the range of the generic actual index subtype, so it is
         --  safe to convert to Index_Type:

         Last := Index_Type'Base (J);

      else
         --  Here Index_Type'First (and Index_Type'Last) is positive, so we
         --  must test the length indirectly (by working backwards from the
         --  largest possible value of Last), in order to prevent overflow.

         J := Count_Type'Base (Index_Type'Last) - N;  -- No_Index

         if J < Count_Type'Base (No_Index) then
            raise Constraint_Error with "new length is out of range";
         end if;

         --  We have determined that the result length would not create a Last
         --  index value outside of the range of Index_Type, so we can now
         --  safely compute its value.

         Last := Index_Type'Base (Count_Type'Base (No_Index) + N);
      end if;

      declare
         LE : Elements_Array renames
                Left.Elements.EA (Index_Type'First .. Left.Last);
         RE : Elements_Array renames
                Right.Elements.EA (Index_Type'First .. Right.Last);

         Elements : Elements_Access := new Elements_Type (Last);

         I : Index_Type'Base := No_Index;

      begin
         --  Elements of an indefinite vector are allocated, so we cannot use
         --  simple slice assignment to give a value to our result. Hence we
         --  must walk the array of each vector parameter, and copy each source
         --  element individually.

         for LI in LE'Range loop
            I := I + 1;

            begin
               if LE (LI) /= null then
                  Elements.EA (I) := new Element_Type'(LE (LI).all);
               end if;

            exception
               when others =>
                  for J in Index_Type'First .. I - 1 loop
                     Free (Elements.EA (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         for RI in RE'Range loop
            I := I + 1;

            begin
               if RE (RI) /= null then
                  Elements.EA (I) := new Element_Type'(RE (RI).all);
               end if;

            exception
               when others =>
                  for J in Index_Type'First .. I - 1 loop
                     Free (Elements.EA (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left : Vector; Right : Element_Type) return Vector is
   begin
      --  We decide that the capacity of the result is the sum of the lengths
      --  of the parameters. We could decide to make it larger, but we have no
      --  basis for knowing how much larger, so we just allocate the minimum
      --  amount of storage.

      --  Here we handle the easy case first, when the vector parameter (Left)
      --  is empty.

      if Left.Is_Empty then
         declare
            Elements : Elements_Access := new Elements_Type (Index_Type'First);

         begin
            begin
               Elements.EA (Index_Type'First) := new Element_Type'(Right);
            exception
               when others =>
                  Free (Elements);
                  raise;
            end;

            return (Controlled with Elements, Index_Type'First, 0, 0);
         end;
      end if;

      --  The vector parameter is not empty, so we must compute the length of
      --  the result vector and its last index, but in such a way that overflow
      --  is avoided. We must satisfy two constraints: the new length cannot
      --  exceed Count_Type'Last, and the new Last index cannot exceed
      --  Index_Type'Last.

      if Left.Length = Count_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      if Left.Last >= Index_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last : constant Index_Type := Left.Last + 1;

         LE : Elements_Array renames
                 Left.Elements.EA (Index_Type'First .. Left.Last);

         Elements : Elements_Access := new Elements_Type (Last);

      begin
         for I in LE'Range loop
            begin
               if LE (I) /= null then
                  Elements.EA (I) := new Element_Type'(LE (I).all);
               end if;

            exception
               when others =>
                  for J in Index_Type'First .. I - 1 loop
                     Free (Elements.EA (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         begin
            Elements.EA (Last) := new Element_Type'(Right);

         exception
            when others =>
               for J in Index_Type'First .. Last - 1 loop
                  Free (Elements.EA (J));
               end loop;

               Free (Elements);
               raise;
         end;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left : Element_Type; Right : Vector) return Vector is
   begin
      --  We decide that the capacity of the result is the sum of the lengths
      --  of the parameters. We could decide to make it larger, but we have no
      --  basis for knowing how much larger, so we just allocate the minimum
      --  amount of storage.

      --  Here we handle the easy case first, when the vector parameter (Right)
      --  is empty.

      if Right.Is_Empty then
         declare
            Elements : Elements_Access := new Elements_Type (Index_Type'First);

         begin
            begin
               Elements.EA (Index_Type'First) := new Element_Type'(Left);
            exception
               when others =>
                  Free (Elements);
                  raise;
            end;

            return (Controlled with Elements, Index_Type'First, 0, 0);
         end;
      end if;

      --  The vector parameter is not empty, so we must compute the length of
      --  the result vector and its last index, but in such a way that overflow
      --  is avoided. We must satisfy two constraints: the new length cannot
      --  exceed Count_Type'Last, and the new Last index cannot exceed
      --  Index_Type'Last.

      if Right.Length = Count_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      if Right.Last >= Index_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last : constant Index_Type := Right.Last + 1;

         RE : Elements_Array renames
                Right.Elements.EA (Index_Type'First .. Right.Last);

         Elements : Elements_Access := new Elements_Type (Last);

         I : Index_Type'Base := Index_Type'First;

      begin
         begin
            Elements.EA (I) := new Element_Type'(Left);
         exception
            when others =>
               Free (Elements);
               raise;
         end;

         for RI in RE'Range loop
            I := I + 1;

            begin
               if RE (RI) /= null then
                  Elements.EA (I) := new Element_Type'(RE (RI).all);
               end if;

            exception
               when others =>
                  for J in Index_Type'First .. I - 1 loop
                     Free (Elements.EA (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left, Right : Element_Type) return Vector is
   begin
      --  We decide that the capacity of the result is the sum of the lengths
      --  of the parameters. We could decide to make it larger, but we have no
      --  basis for knowing how much larger, so we just allocate the minimum
      --  amount of storage.

      --  We must compute the length of the result vector and its last index,
      --  but in such a way that overflow is avoided. We must satisfy two
      --  constraints: the new length cannot exceed Count_Type'Last (here, we
      --  know that that condition is satisfied), and the new Last index cannot
      --  exceed Index_Type'Last.

      if Index_Type'First >= Index_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last     : constant Index_Type := Index_Type'First + 1;
         Elements : Elements_Access := new Elements_Type (Last);

      begin
         begin
            Elements.EA (Index_Type'First) := new Element_Type'(Left);
         exception
            when others =>
               Free (Elements);
               raise;
         end;

         begin
            Elements.EA (Last) := new Element_Type'(Right);
         exception
            when others =>
               Free (Elements.EA (Index_Type'First));
               Free (Elements);
               raise;
         end;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Vector) return Boolean is
      BL : Natural renames Left'Unrestricted_Access.Busy;
      LL : Natural renames Left'Unrestricted_Access.Lock;

      BR : Natural renames Right'Unrestricted_Access.Busy;
      LR : Natural renames Right'Unrestricted_Access.Lock;

      Result : Boolean;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Last /= Right.Last then
         return False;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      BL := BL + 1;
      LL := LL + 1;

      BR := BR + 1;
      LR := LR + 1;

      Result := True;
      for J in Index_Type'First .. Left.Last loop
         if Left.Elements.EA (J) = null then
            if Right.Elements.EA (J) /= null then
               Result := False;
               exit;
            end if;

         elsif Right.Elements.EA (J) = null then
            Result := False;
            exit;

         elsif Left.Elements.EA (J).all /= Right.Elements.EA (J).all then
            Result := False;
            exit;
         end if;
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
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Vector) is
   begin
      if Container.Last = No_Index then
         Container.Elements := null;
         return;
      end if;

      declare
         L : constant Index_Type := Container.Last;
         E : Elements_Array renames
               Container.Elements.EA (Index_Type'First .. L);

      begin
         Container.Elements := null;
         Container.Last := No_Index;
         Container.Busy := 0;
         Container.Lock := 0;

         Container.Elements := new Elements_Type (L);

         for J in E'Range loop
            if E (J) /= null then
               Container.Elements.EA (J) := new Element_Type'(E (J).all);
            end if;

            Container.Last := J;
         end loop;
      end;
   end Adjust;

   procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            C : Vector renames Control.Container.all;
            B : Natural renames C.Busy;
            L : Natural renames C.Lock;
         begin
            B := B + 1;
            L := L + 1;
         end;
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      if Is_Empty (New_Item) then
         return;
      elsif Container.Last = Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      else
         Insert (Container, Container.Last + 1, New_Item);
      end if;
   end Append;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      if Count = 0 then
         return;
      elsif Container.Last = Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      else
         Insert (Container, Container.Last + 1, New_Item, Count);
      end if;
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Vector; Source : Vector) is
   begin
      if Target'Address = Source'Address then
         return;
      else
         Target.Clear;
         Target.Append (Source);
      end if;
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Count_Type is
   begin
      if Container.Elements = null then
         return 0;
      else
         return Container.Elements.EA'Length;
      end if;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";

      else
         while Container.Last >= Index_Type'First loop
            declare
               X : Element_Access := Container.Elements.EA (Container.Last);
            begin
               Container.Elements.EA (Container.Last) := null;
               Container.Last := Container.Last - 1;
               Free (X);
            end;
         end loop;
      end if;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor) return Constant_Reference_Type
   is
      E : Element_Access;

   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor denotes wrong container";
      end if;

      if Position.Index > Position.Container.Last then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      E := Container.Elements.EA (Position.Index);

      if E = null then
         raise Constraint_Error with "element at Position is empty";
      end if;

      declare
         C : Vector renames Container'Unrestricted_Access.all;
         B : Natural renames C.Busy;
         L : Natural renames C.Lock;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => E.all'Access,
            Control => (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type) return Constant_Reference_Type
   is
      E : Element_Access;

   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      E := Container.Elements.EA (Index);

      if E = null then
         raise Constraint_Error with "element at Index is empty";
      end if;

      declare
         C : Vector renames Container'Unrestricted_Access.all;
         B : Natural renames C.Busy;
         L : Natural renames C.Lock;
      begin
         return R : constant Constant_Reference_Type :=
           (Element => E.all'Access,
            Control => (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Constant_Reference;

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
      Capacity : Count_Type := 0) return Vector
   is
      C : Count_Type;

   begin
      if Capacity = 0 then
         C := Source.Length;

      elsif Capacity >= Source.Length then
         C := Capacity;

      else
         raise Capacity_Error
           with "Requested capacity is less than Source length";
      end if;

      return Target : Vector do
         Target.Reserve_Capacity (C);
         Target.Assign (Source);
      end return;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type := 1)
   is
      Old_Last : constant Index_Type'Base := Container.Last;
      New_Last : Index_Type'Base;
      Count2   : Count_Type'Base;  -- count of items from Index to Old_Last
      J        : Index_Type'Base;  -- first index of items that slide down

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
         else
            return;
         end if;
      end if;

      --  Here and elsewhere we treat deleting 0 items from the container as a
      --  no-op, even when the container is busy, so we simply return.

      if Count = 0 then
         return;
      end if;

      --  The internal elements array isn't guaranteed to exist unless we have
      --  elements, so we handle that case here in order to avoid having to
      --  check it later. (Note that an empty vector can never be busy, so
      --  there's no semantic harm in returning early.)

      if Container.Is_Empty then
         return;
      end if;

      --  The tampering bits exist to prevent an item from being deleted (or
      --  otherwise harmfully manipulated) while it is being visited. Query,
      --  Update, and Iterate increment the busy count on entry, and decrement
      --  the count on exit. Delete checks the count to determine whether it is
      --  being called while the associated callback procedure is executing.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
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

      --  If the number of elements requested (Count) for deletion is equal to
      --  (or greater than) the number of elements available (Count2) for
      --  deletion beginning at Index, then everything from Index to
      --  Container.Last is deleted (this is equivalent to Delete_Last).

      if Count >= Count2 then
         --  Elements in an indefinite vector are allocated, so we must iterate
         --  over the loop and deallocate elements one-at-a-time. We work from
         --  back to front, deleting the last element during each pass, in
         --  order to gracefully handle deallocation failures.

         declare
            EA : Elements_Array renames Container.Elements.EA;

         begin
            while Container.Last >= Index loop
               declare
                  K : constant Index_Type := Container.Last;
                  X : Element_Access := EA (K);

               begin
                  --  We first isolate the element we're deleting, removing it
                  --  from the vector before we attempt to deallocate it, in
                  --  case the deallocation fails.

                  EA (K) := null;
                  Container.Last := K - 1;

                  --  Container invariants have been restored, so it is now
                  --  safe to attempt to deallocate the element.

                  Free (X);
               end;
            end loop;
         end;

         return;
      end if;

      --  There are some elements that aren't being deleted (the requested
      --  count was less than the available count), so we must slide them down
      --  to Index. We first calculate the index values of the respective array
      --  slices, using the wider of Index_Type'Base and Count_Type'Base as the
      --  type for intermediate calculations. For the elements that slide down,
      --  index value New_Last is the last index value of their new home, and
      --  index value J is the first index of their old home.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         New_Last := Old_Last - Index_Type'Base (Count);
         J := Index + Index_Type'Base (Count);
      else
         New_Last := Index_Type'Base (Count_Type'Base (Old_Last) - Count);
         J := Index_Type'Base (Count_Type'Base (Index) + Count);
      end if;

      --  The internal elements array isn't guaranteed to exist unless we have
      --  elements, but we have that guarantee here because we know we have
      --  elements to slide.  The array index values for each slice have
      --  already been determined, so what remains to be done is to first
      --  deallocate the elements that are being deleted, and then slide down
      --  to Index the elements that aren't being deleted.

      declare
         EA : Elements_Array renames Container.Elements.EA;

      begin
         --  Before we can slide down the elements that aren't being deleted,
         --  we need to deallocate the elements that are being deleted.

         for K in Index .. J - 1 loop
            declare
               X : Element_Access := EA (K);

            begin
               --  First we remove the element we're about to deallocate from
               --  the vector, in case the deallocation fails, in order to
               --  preserve representation invariants.

               EA (K) := null;

               --  The element has been removed from the vector, so it is now
               --  safe to attempt to deallocate it.

               Free (X);
            end;
         end loop;

         EA (Index .. New_Last) := EA (J .. Old_Last);
         Container.Last := New_Last;
      end;
   end Delete;

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
      pragma Warnings (Off, Position);

   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";

      elsif Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor denotes wrong container";

      elsif Position.Index > Container.Last then
         raise Program_Error with "Position index is out of range";

      else
         Delete (Container, Position.Index, Count);
         Position := No_Element;
      end if;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out Vector;
      Count     : Count_Type := 1)
   is
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

   procedure Delete_Last
     (Container : in out Vector;
      Count     : Count_Type := 1)
   is
   begin
      --  It is not permitted to delete items while the container is busy (for
      --  example, we're in the middle of a passive iteration). However, we
      --  always treat deleting 0 items as a no-op, even when we're busy, so we
      --  simply return without checking.

      if Count = 0 then
         return;
      end if;

      --  We cannot simply subsume the empty case into the loop below (the loop
      --  would iterate 0 times), because we rename the internal array object
      --  (which is allocated), but an empty vector isn't guaranteed to have
      --  actually allocated an array. (Note that an empty vector can never be
      --  busy, so there's no semantic harm in returning early here.)

      if Container.Is_Empty then
         return;
      end if;

      --  The tampering bits exist to prevent an item from being deleted (or
      --  otherwise harmfully manipulated) while it is being visited. Query,
      --  Update, and Iterate increment the busy count on entry, and decrement
      --  the count on exit. Delete_Last checks the count to determine whether
      --  it is being called while the associated callback procedure is
      --  executing.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
      end if;

      --  Elements in an indefinite vector are allocated, so we must iterate
      --  over the loop and deallocate elements one-at-a-time. We work from
      --  back to front, deleting the last element during each pass, in order
      --  to gracefully handle deallocation failures.

      declare
         E : Elements_Array renames Container.Elements.EA;

      begin
         for Indx in 1 .. Count_Type'Min (Count, Container.Length) loop
            declare
               J : constant Index_Type := Container.Last;
               X : Element_Access := E (J);

            begin
               --  Note that we first isolate the element we're deleting,
               --  removing it from the vector, before we actually deallocate
               --  it, in order to preserve representation invariants even if
               --  the deallocation fails.

               E (J) := null;
               Container.Last := J - 1;

               --  Container invariants have been restored, so it is now safe
               --  to deallocate the element.

               Free (X);
            end;
         end loop;
      end;
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
         EA : constant Element_Access := Container.Elements.EA (Index);
      begin
         if EA = null then
            raise Constraint_Error with "element is empty";
         else
            return EA.all;
         end if;
      end;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Index > Position.Container.Last then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      declare
         EA : constant Element_Access :=
                Position.Container.Elements.EA (Position.Index);
      begin
         if EA = null then
            raise Constraint_Error with "element is empty";
         else
            return EA.all;
         end if;
      end;
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Vector) is
   begin
      Clear (Container);  --  Checks busy-bit

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := null;
         Free (X);
      end;
   end Finalize;

   procedure Finalize (Object : in out Iterator) is
      B : Natural renames Object.Container.Busy;
   begin
      B := B - 1;
   end Finalize;

   procedure Finalize (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            C : Vector renames Control.Container.all;
            B : Natural renames C.Busy;
            L : Natural renames C.Lock;
         begin
            B := B - 1;
            L := L - 1;
         end;

         Control.Container := null;
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
   begin
      if Position.Container /= null then
         if Position.Container /= Container'Unrestricted_Access then
            raise Program_Error with "Position cursor denotes wrong container";
         end if;

         if Position.Index > Container.Last then
            raise Program_Error with "Position index is out of range";
         end if;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         B : Natural renames Container'Unrestricted_Access.Busy;
         L : Natural renames Container'Unrestricted_Access.Lock;

         Result : Index_Type'Base;

      begin
         B := B + 1;
         L := L + 1;

         Result := No_Index;
         for J in Position.Index .. Container.Last loop
            if Container.Elements.EA (J) /= null
              and then Container.Elements.EA (J).all = Item
            then
               Result := J;
               exit;
            end if;
         end loop;

         B := B - 1;
         L := L - 1;

         if Result = No_Index then
            return No_Element;
         else
            return Cursor'(Container'Unrestricted_Access, Result);
         end if;

      exception
         when others =>
            B := B - 1;
            L := L - 1;
            raise;
      end;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
      B : Natural renames Container'Unrestricted_Access.Busy;
      L : Natural renames Container'Unrestricted_Access.Lock;

      Result : Index_Type'Base;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B := B + 1;
      L := L + 1;

      Result := No_Index;
      for Indx in Index .. Container.Last loop
         if Container.Elements.EA (Indx) /= null
           and then Container.Elements.EA (Indx).all = Item
         then
            Result := Indx;
            exit;
         end if;
      end loop;

      B := B - 1;
      L := L - 1;

      return Result;

   exception
      when others =>
         B := B - 1;
         L := L - 1;
         raise;
   end Find_Index;

   -----------
   -- First --
   -----------

   function First (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unrestricted_Access, Index_Type'First);
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Index component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Index component is No_Index, this means the iterator
      --  object was constructed without a start expression, in which case the
      --  (forward) iteration starts from the (logical) beginning of the entire
      --  sequence of items (corresponding to Container.First, for a forward
      --  iterator).

      --  Otherwise, this is iteration over a partial sequence of items.
      --  When the Index component isn't No_Index, the iterator object was
      --  constructed with a start expression, that specifies the position
      --  from which the (forward) partial iteration begins.

      if Object.Index = No_Index then
         return First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Index);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
   begin
      if Container.Last = No_Index then
         raise Constraint_Error with "Container is empty";
      end if;

      declare
         EA : constant Element_Access :=
                Container.Elements.EA (Index_Type'First);
      begin
         if EA = null then
            raise Constraint_Error with "first element is empty";
         else
            return EA.all;
         end if;
      end;
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Is_Less (L, R : Element_Access) return Boolean;
      pragma Inline (Is_Less);

      -------------
      -- Is_Less --
      -------------

      function Is_Less (L, R : Element_Access) return Boolean is
      begin
         if L = null then
            return R /= null;
         elsif R = null then
            return False;
         else
            return L.all < R.all;
         end if;
      end Is_Less;

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         if Container.Last <= Index_Type'First then
            return True;
         end if;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            E : Elements_Array renames Container.Elements.EA;

            B : Natural renames Container'Unrestricted_Access.Busy;
            L : Natural renames Container'Unrestricted_Access.Lock;

            Result : Boolean;

         begin
            B := B + 1;
            L := L + 1;

            Result := True;
            for I in Index_Type'First .. Container.Last - 1 loop
               if Is_Less (E (I + 1), E (I)) then
                  Result := False;
                  exit;
               end if;
            end loop;

            B := B - 1;
            L := L - 1;

            return Result;

         exception
            when others =>
               B := B - 1;
               L := L - 1;
               raise;
         end;
      end Is_Sorted;

      -----------
      -- Merge --
      -----------

      procedure Merge (Target, Source : in out Vector) is
         I, J : Index_Type'Base;

      begin
         --  The semantics of Merge changed slightly per AI05-0021. It was
         --  originally the case that if Target and Source denoted the same
         --  container object, then the GNAT implementation of Merge did
         --  nothing. However, it was argued that RM05 did not precisely
         --  specify the semantics for this corner case. The decision of the
         --  ARG was that if Target and Source denote the same non-empty
         --  container object, then Program_Error is raised.

         if Source.Last < Index_Type'First then  -- Source is empty
            return;
         end if;

         if Target'Address = Source'Address then
            raise Program_Error with
              "Target and Source denote same non-empty container";
         end if;

         if Target.Last < Index_Type'First then  -- Target is empty
            Move (Target => Target, Source => Source);
            return;
         end if;

         if Source.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (vector is busy)";
         end if;

         I := Target.Last;  -- original value (before Set_Length)
         Target.Set_Length (Length (Target) + Length (Source));

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            TA : Elements_Array renames Target.Elements.EA;
            SA : Elements_Array renames Source.Elements.EA;

            TB : Natural renames Target.Busy;
            TL : Natural renames Target.Lock;

            SB : Natural renames Source.Busy;
            SL : Natural renames Source.Lock;

         begin
            TB := TB + 1;
            TL := TL + 1;

            SB := SB + 1;
            SL := SL + 1;

            J := Target.Last;  -- new value (after Set_Length)
            while Source.Last >= Index_Type'First loop
               pragma Assert
                 (Source.Last <= Index_Type'First
                   or else not (Is_Less (SA (Source.Last),
                                         SA (Source.Last - 1))));

               if I < Index_Type'First then
                  declare
                     Src : Elements_Array renames
                             SA (Index_Type'First .. Source.Last);
                  begin
                     TA (Index_Type'First .. J) := Src;
                     Src := (others => null);
                  end;

                  Source.Last := No_Index;
                  exit;
               end if;

               pragma Assert
                 (I <= Index_Type'First
                    or else not (Is_Less (TA (I), TA (I - 1))));

               declare
                  Src : Element_Access renames SA (Source.Last);
                  Tgt : Element_Access renames TA (I);

               begin
                  if Is_Less (Src, Tgt) then
                     Target.Elements.EA (J) := Tgt;
                     Tgt := null;
                     I := I - 1;

                  else
                     Target.Elements.EA (J) := Src;
                     Src := null;
                     Source.Last := Source.Last - 1;
                  end if;
               end;

               J := J - 1;
            end loop;

            TB := TB - 1;
            TL := TL - 1;

            SB := SB - 1;
            SL := SL - 1;

         exception
            when others =>
               TB := TB - 1;
               TL := TL - 1;

               SB := SB - 1;
               SL := SL - 1;

               raise;
         end;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector) is
         procedure Sort is new Generic_Array_Sort
           (Index_Type   => Index_Type,
            Element_Type => Element_Access,
            Array_Type   => Elements_Array,
            "<"          => Is_Less);

      --  Start of processing for Sort

      begin
         if Container.Last <= Index_Type'First then
            return;
         end if;

         --  The exception behavior for the vector container must match that
         --  for the list container, so we check for cursor tampering here
         --  (which will catch more things) instead of for element tampering
         --  (which will catch fewer things). It's true that the elements of
         --  this vector container could be safely moved around while (say) an
         --  iteration is taking place (iteration only increments the busy
         --  counter), and so technically all we would need here is a test for
         --  element tampering (indicated by the lock counter), that's simply
         --  an artifact of our array-based implementation. Logically Sort
         --  requires a check for cursor tampering.

         if Container.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (vector is busy)";
         end if;

         --  Per AI05-0022, the container implementation is required to detect
         --  element tampering by a generic actual subprogram.

         declare
            B : Natural renames Container.Busy;
            L : Natural renames Container.Lock;

         begin
            B := B + 1;
            L := L + 1;

            Sort (Container.Elements.EA (Index_Type'First .. Container.Last));

            B := B - 1;
            L := L - 1;

         exception
            when others =>
               B := B - 1;
               L := L - 1;
               raise;
         end;
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      else
         return Position.Index <= Position.Container.Last;
      end if;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Old_Length : constant Count_Type := Container.Length;

      Max_Length : Count_Type'Base;  -- determined from range of Index_Type
      New_Length : Count_Type'Base;  -- sum of current length and Count
      New_Last   : Index_Type'Base;  -- last index of vector after insertion

      Index : Index_Type'Base;  -- scratch for intermediate values
      J     : Count_Type'Base;  -- scratch

      New_Capacity : Count_Type'Base;  -- length of new, expanded array
      Dst_Last     : Index_Type'Base;  -- last index of new, expanded array
      Dst          : Elements_Access;  -- new, expanded internal array

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
        and then Before > Container.Last + 1
      then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      --  We treat inserting 0 items into the container as a no-op, even when
      --  the container is busy, so we simply return.

      if Count = 0 then
         return;
      end if;

      --  There are two constraints we need to satisfy. The first constraint is
      --  that a container cannot have more than Count_Type'Last elements, so
      --  we must check the sum of the current length and the insertion count.
      --  Note that we cannot simply add these values, because of the
      --  possibility of overflow.

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

            Max_Length := Count_Type'Base (Index_Type'Last - No_Index);
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

      --  New_Last is the last index value of the items in the container after
      --  insertion.  Use the wider of Index_Type'Base and Count_Type'Base to
      --  compute its value from the New_Length.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         New_Last := No_Index + Index_Type'Base (New_Length);
      else
         New_Last := Index_Type'Base (Count_Type'Base (No_Index) + New_Length);
      end if;

      if Container.Elements = null then
         pragma Assert (Container.Last = No_Index);

         --  This is the simplest case, with which we must always begin: we're
         --  inserting items into an empty vector that hasn't allocated an
         --  internal array yet. Note that we don't need to check the busy bit
         --  here, because an empty container cannot be busy.

         --  In an indefinite vector, elements are allocated individually, and
         --  stored as access values on the internal array (the length of which
         --  represents the vector "capacity"), which is separately allocated.

         Container.Elements := new Elements_Type (New_Last);

         --  The element backbone has been successfully allocated, so now we
         --  allocate the elements.

         for Idx in Container.Elements.EA'Range loop

            --  In order to preserve container invariants, we always attempt
            --  the element allocation first, before setting the Last index
            --  value, in case the allocation fails (either because there is no
            --  storage available, or because element initialization fails).

            declare
               --  The element allocator may need an accessibility check in the
               --  case actual type is class-wide or has access discriminants
               --  (see RM 4.8(10.1) and AI12-0035).

               pragma Unsuppress (Accessibility_Check);

            begin
               Container.Elements.EA (Idx) := new Element_Type'(New_Item);
            end;

            --  The allocation of the element succeeded, so it is now safe to
            --  update the Last index, restoring container invariants.

            Container.Last := Idx;
         end loop;

         return;
      end if;

      --  The tampering bits exist to prevent an item from being harmfully
      --  manipulated while it is being visited. Query, Update, and Iterate
      --  increment the busy count on entry, and decrement the count on
      --  exit. Insert checks the count to determine whether it is being called
      --  while the associated callback procedure is executing.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
      end if;

      if New_Length <= Container.Elements.EA'Length then

         --  In this case, we're inserting elements into a vector that has
         --  already allocated an internal array, and the existing array has
         --  enough unused storage for the new items.

         declare
            E : Elements_Array renames Container.Elements.EA;
            K : Index_Type'Base;

         begin
            if Before > Container.Last then

               --  The new items are being appended to the vector, so no
               --  sliding of existing elements is required.

               for Idx in Before .. New_Last loop

                  --  In order to preserve container invariants, we always
                  --  attempt the element allocation first, before setting the
                  --  Last index value, in case the allocation fails (either
                  --  because there is no storage available, or because element
                  --  initialization fails).

                  declare
                     --  The element allocator may need an accessibility check
                     --  in case the actual type is class-wide or has access
                     --  discriminants (see RM 4.8(10.1) and AI12-0035).

                     pragma Unsuppress (Accessibility_Check);

                  begin
                     E (Idx) := new Element_Type'(New_Item);
                  end;

                  --  The allocation of the element succeeded, so it is now
                  --  safe to update the Last index, restoring container
                  --  invariants.

                  Container.Last := Idx;
               end loop;

            else
               --  The new items are being inserted before some existing
               --  elements, so we must slide the existing elements up to their
               --  new home. We use the wider of Index_Type'Base and
               --  Count_Type'Base as the type for intermediate index values.

               if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
                  Index := Before + Index_Type'Base (Count);
               else
                  Index := Index_Type'Base (Count_Type'Base (Before) + Count);
               end if;

               --  The new items are being inserted in the middle of the array,
               --  in the range [Before, Index). Copy the existing elements to
               --  the end of the array, to make room for the new items.

               E (Index .. New_Last) := E (Before .. Container.Last);
               Container.Last := New_Last;

               --  We have copied the existing items up to the end of the
               --  array, to make room for the new items in the middle of
               --  the array.  Now we actually allocate the new items.

               --  Note: initialize K outside loop to make it clear that
               --  K always has a value if the exception handler triggers.

               K := Before;

               declare
                  --  The element allocator may need an accessibility check in
                  --  the case the actual type is class-wide or has access
                  --  discriminants (see RM 4.8(10.1) and AI12-0035).

                  pragma Unsuppress (Accessibility_Check);

               begin
                  while K < Index loop
                     E (K) := new Element_Type'(New_Item);
                     K := K + 1;
                  end loop;

               exception
                  when others =>

                     --  Values in the range [Before, K) were successfully
                     --  allocated, but values in the range [K, Index) are
                     --  stale (these array positions contain copies of the
                     --  old items, that did not get assigned a new item,
                     --  because the allocation failed). We must finish what
                     --  we started by clearing out all of the stale values,
                     --  leaving a "hole" in the middle of the array.

                     E (K .. Index - 1) := (others => null);
                     raise;
               end;
            end if;
         end;

         return;
      end if;

      --  In this case, we're inserting elements into a vector that has already
      --  allocated an internal array, but the existing array does not have
      --  enough storage, so we must allocate a new, longer array. In order to
      --  guarantee that the amortized insertion cost is O(1), we always
      --  allocate an array whose length is some power-of-two factor of the
      --  current array length. (The new array cannot have a length less than
      --  the New_Length of the container, but its last index value cannot be
      --  greater than Index_Type'Last.)

      New_Capacity := Count_Type'Max (1, Container.Elements.EA'Length);
      while New_Capacity < New_Length loop
         if New_Capacity > Count_Type'Last / 2 then
            New_Capacity := Count_Type'Last;
            exit;
         end if;

         New_Capacity := 2 * New_Capacity;
      end loop;

      if New_Capacity > Max_Length then

         --  We have reached the limit of capacity, so no further expansion
         --  will occur. (This is not a problem, as there is never a need to
         --  have more capacity than the maximum container length.)

         New_Capacity := Max_Length;
      end if;

      --  We have computed the length of the new internal array (and this is
      --  what "vector capacity" means), so use that to compute its last index.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Dst_Last := No_Index + Index_Type'Base (New_Capacity);
      else
         Dst_Last :=
           Index_Type'Base (Count_Type'Base (No_Index) + New_Capacity);
      end if;

      --  Now we allocate the new, longer internal array. If the allocation
      --  fails, we have not changed any container state, so no side-effect
      --  will occur as a result of propagating the exception.

      Dst := new Elements_Type (Dst_Last);

      --  We have our new internal array. All that needs to be done now is to
      --  copy the existing items (if any) from the old array (the "source"
      --  array) to the new array (the "destination" array), and then
      --  deallocate the old array.

      declare
         Src : Elements_Access := Container.Elements;

      begin
         Dst.EA (Index_Type'First .. Before - 1) :=
           Src.EA (Index_Type'First .. Before - 1);

         if Before > Container.Last then

            --  The new items are being appended to the vector, so no
            --  sliding of existing elements is required.

            --  We have copied the elements from to the old source array to the
            --  new destination array, so we can now deallocate the old array.

            Container.Elements := Dst;
            Free (Src);

            --  Now we append the new items.

            for Idx in Before .. New_Last loop

               --  In order to preserve container invariants, we always attempt
               --  the element allocation first, before setting the Last index
               --  value, in case the allocation fails (either because there
               --  is no storage available, or because element initialization
               --  fails).

               declare
                  --  The element allocator may need an accessibility check in
                  --  the case the actual type is class-wide or has access
                  --  discriminants (see RM 4.8(10.1) and AI12-0035).

                  pragma Unsuppress (Accessibility_Check);

               begin
                  Dst.EA (Idx) := new Element_Type'(New_Item);
               end;

               --  The allocation of the element succeeded, so it is now safe
               --  to update the Last index, restoring container invariants.

               Container.Last := Idx;
            end loop;

         else
            --  The new items are being inserted before some existing elements,
            --  so we must slide the existing elements up to their new home.

            if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
               Index := Before + Index_Type'Base (Count);
            else
               Index := Index_Type'Base (Count_Type'Base (Before) + Count);
            end if;

            Dst.EA (Index .. New_Last) := Src.EA (Before .. Container.Last);

            --  We have copied the elements from to the old source array to the
            --  new destination array, so we can now deallocate the old array.

            Container.Elements := Dst;
            Container.Last := New_Last;
            Free (Src);

            --  The new array has a range in the middle containing null access
            --  values. Fill in that partition of the array with the new items.

            for Idx in Before .. Index - 1 loop

               --  Note that container invariants have already been satisfied
               --  (in particular, the Last index value of the vector has
               --  already been updated), so if this allocation fails we simply
               --  let it propagate.

               declare
                  --  The element allocator may need an accessibility check in
                  --  the case the actual type is class-wide or has access
                  --  discriminants (see RM 4.8(10.1) and AI12-0035).

                  pragma Unsuppress (Accessibility_Check);

               begin
                  Dst.EA (Idx) := new Element_Type'(New_Item);
               end;
            end loop;
         end if;
      end;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   is
      N : constant Count_Type := Length (New_Item);
      J : Index_Type'Base;

   begin
      --  Use Insert_Space to create the "hole" (the destination slice) into
      --  which we copy the source items.

      Insert_Space (Container, Before, Count => N);

      if N = 0 then

         --  There's nothing else to do here (vetting of parameters was
         --  performed already in Insert_Space), so we simply return.

         return;
      end if;

      if Container'Address /= New_Item'Address then

         --  This is the simple case.  New_Item denotes an object different
         --  from Container, so there's nothing special we need to do to copy
         --  the source items to their destination, because all of the source
         --  items are contiguous.

         declare
            subtype Src_Index_Subtype is Index_Type'Base range
              Index_Type'First .. New_Item.Last;

            Src : Elements_Array renames
                    New_Item.Elements.EA (Src_Index_Subtype);

            Dst : Elements_Array renames Container.Elements.EA;

            Dst_Index : Index_Type'Base;

         begin
            Dst_Index := Before - 1;
            for Src_Index in Src'Range loop
               Dst_Index := Dst_Index + 1;

               if Src (Src_Index) /= null then
                  Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
               end if;
            end loop;
         end;

         return;
      end if;

      --  New_Item denotes the same object as Container, so an insertion has
      --  potentially split the source items.  The first source slice is
      --  [Index_Type'First, Before), and the second source slice is
      --  [J, Container.Last], where index value J is the first index of the
      --  second slice. (J gets computed below, but only after we have
      --  determined that the second source slice is non-empty.) The
      --  destination slice is always the range [Before, J). We perform the
      --  copy in two steps, using each of the two slices of the source items.

      declare
         L : constant Index_Type'Base := Before - 1;

         subtype Src_Index_Subtype is Index_Type'Base range
           Index_Type'First .. L;

         Src : Elements_Array renames
                 Container.Elements.EA (Src_Index_Subtype);

         Dst : Elements_Array renames Container.Elements.EA;

         Dst_Index : Index_Type'Base;

      begin
         --  We first copy the source items that precede the space we
         --  inserted. (If Before equals Index_Type'First, then this first
         --  source slice will be empty, which is harmless.)

         Dst_Index := Before - 1;
         for Src_Index in Src'Range loop
            Dst_Index := Dst_Index + 1;

            if Src (Src_Index) /= null then
               Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
            end if;
         end loop;

         if Src'Length = N then

            --  The new items were effectively appended to the container, so we
            --  have already copied all of the items that need to be copied.
            --  We return early here, even though the source slice below is
            --  empty (so the assignment would be harmless), because we want to
            --  avoid computing J, which will overflow if J is greater than
            --  Index_Type'Base'Last.

            return;
         end if;
      end;

      --  Index value J is the first index of the second source slice. (It is
      --  also 1 greater than the last index of the destination slice.) Note:
      --  avoid computing J if J is greater than Index_Type'Base'Last, in order
      --  to avoid overflow. Prevent that by returning early above, immediately
      --  after copying the first slice of the source, and determining that
      --  this second slice of the source is empty.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         J := Before + Index_Type'Base (N);
      else
         J := Index_Type'Base (Count_Type'Base (Before) + N);
      end if;

      declare
         subtype Src_Index_Subtype is Index_Type'Base range
           J .. Container.Last;

         Src : Elements_Array renames
                 Container.Elements.EA (Src_Index_Subtype);

         Dst : Elements_Array renames Container.Elements.EA;

         Dst_Index : Index_Type'Base;

      begin
         --  We next copy the source items that follow the space we inserted.
         --  Index value Dst_Index is the first index of that portion of the
         --  destination that receives this slice of the source. (For the
         --  reasons given above, this slice is guaranteed to be non-empty.)

         if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
            Dst_Index := J - Index_Type'Base (Src'Length);
         else
            Dst_Index := Index_Type'Base (Count_Type'Base (J) - Src'Length);
         end if;

         for Src_Index in Src'Range loop
            if Src (Src_Index) /= null then
               Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
            end if;

            Dst_Index := Dst_Index + 1;
         end loop;
      end;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Before cursor denotes wrong container";
      end if;

      if Is_Empty (New_Item) then
         return;
      end if;

      if Before.Container = null or else Before.Index > Container.Last then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector;
      Position  : out Cursor)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /=
                   Vector_Access'(Container'Unrestricted_Access)
      then
         raise Program_Error with "Before cursor denotes wrong container";
      end if;

      if Is_Empty (New_Item) then
         if Before.Container = null or else Before.Index > Container.Last then
            Position := No_Element;
         else
            Position := (Container'Unrestricted_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null or else Before.Index > Container.Last then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);

      Position := Cursor'(Container'Unrestricted_Access, Index);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Before cursor denotes wrong container";
      end if;

      if Count = 0 then
         return;
      end if;

      if Before.Container = null or else Before.Index > Container.Last then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Before cursor denotes wrong container";
      end if;

      if Count = 0 then
         if Before.Container = null
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unrestricted_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null or else Before.Index > Container.Last then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);

      Position := (Container'Unrestricted_Access, Index);
   end Insert;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1)
   is
      Old_Length : constant Count_Type := Container.Length;

      Max_Length : Count_Type'Base;  -- determined from range of Index_Type
      New_Length : Count_Type'Base;  -- sum of current length and Count
      New_Last   : Index_Type'Base;  -- last index of vector after insertion

      Index : Index_Type'Base;  -- scratch for intermediate values
      J     : Count_Type'Base;  -- scratch

      New_Capacity : Count_Type'Base;  -- length of new, expanded array
      Dst_Last     : Index_Type'Base;  -- last index of new, expanded array
      Dst          : Elements_Access;  -- new, expanded internal array

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

      if Before > Container.Last and then Before > Container.Last + 1 then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      --  We treat inserting 0 items into the container as a no-op, even when
      --  the container is busy, so we simply return.

      if Count = 0 then
         return;
      end if;

      --  There are two constraints we need to satisfy. The first constraint is
      --  that a container cannot have more than Count_Type'Last elements, so
      --  we must check the sum of the current length and the insertion
      --  count. Note that we cannot simply add these values, because of the
      --  possibility of overflow.

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

            Max_Length := Count_Type'Base (Index_Type'Last - No_Index);
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

      --  New_Last is the last index value of the items in the container after
      --  insertion.  Use the wider of Index_Type'Base and Count_Type'Base to
      --  compute its value from the New_Length.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         New_Last := No_Index + Index_Type'Base (New_Length);
      else
         New_Last := Index_Type'Base (Count_Type'Base (No_Index) + New_Length);
      end if;

      if Container.Elements = null then
         pragma Assert (Container.Last = No_Index);

         --  This is the simplest case, with which we must always begin: we're
         --  inserting items into an empty vector that hasn't allocated an
         --  internal array yet. Note that we don't need to check the busy bit
         --  here, because an empty container cannot be busy.

         --  In an indefinite vector, elements are allocated individually, and
         --  stored as access values on the internal array (the length of which
         --  represents the vector "capacity"), which is separately allocated.
         --  We have no elements here (because we're inserting "space"), so all
         --  we need to do is allocate the backbone.

         Container.Elements := new Elements_Type (New_Last);
         Container.Last := New_Last;

         return;
      end if;

      --  The tampering bits exist to prevent an item from being harmfully
      --  manipulated while it is being visited. Query, Update, and Iterate
      --  increment the busy count on entry, and decrement the count on exit.
      --  Insert checks the count to determine whether it is being called while
      --  the associated callback procedure is executing.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
      end if;

      if New_Length <= Container.Elements.EA'Length then

         --  In this case, we are inserting elements into a vector that has
         --  already allocated an internal array, and the existing array has
         --  enough unused storage for the new items.

         declare
            E : Elements_Array renames Container.Elements.EA;

         begin
            if Before <= Container.Last then

               --  The new space is being inserted before some existing
               --  elements, so we must slide the existing elements up to
               --  their new home. We use the wider of Index_Type'Base and
               --  Count_Type'Base as the type for intermediate index values.

               if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
                  Index := Before + Index_Type'Base (Count);
               else
                  Index := Index_Type'Base (Count_Type'Base (Before) + Count);
               end if;

               E (Index .. New_Last) := E (Before .. Container.Last);
               E (Before .. Index - 1) := (others => null);
            end if;
         end;

         Container.Last := New_Last;
         return;
      end if;

      --  In this case, we're inserting elements into a vector that has already
      --  allocated an internal array, but the existing array does not have
      --  enough storage, so we must allocate a new, longer array. In order to
      --  guarantee that the amortized insertion cost is O(1), we always
      --  allocate an array whose length is some power-of-two factor of the
      --  current array length. (The new array cannot have a length less than
      --  the New_Length of the container, but its last index value cannot be
      --  greater than Index_Type'Last.)

      New_Capacity := Count_Type'Max (1, Container.Elements.EA'Length);
      while New_Capacity < New_Length loop
         if New_Capacity > Count_Type'Last / 2 then
            New_Capacity := Count_Type'Last;
            exit;
         end if;

         New_Capacity := 2 * New_Capacity;
      end loop;

      if New_Capacity > Max_Length then

         --  We have reached the limit of capacity, so no further expansion
         --  will occur. (This is not a problem, as there is never a need to
         --  have more capacity than the maximum container length.)

         New_Capacity := Max_Length;
      end if;

      --  We have computed the length of the new internal array (and this is
      --  what "vector capacity" means), so use that to compute its last index.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
         Dst_Last := No_Index + Index_Type'Base (New_Capacity);
      else
         Dst_Last :=
           Index_Type'Base (Count_Type'Base (No_Index) + New_Capacity);
      end if;

      --  Now we allocate the new, longer internal array. If the allocation
      --  fails, we have not changed any container state, so no side-effect
      --  will occur as a result of propagating the exception.

      Dst := new Elements_Type (Dst_Last);

      --  We have our new internal array. All that needs to be done now is to
      --  copy the existing items (if any) from the old array (the "source"
      --  array) to the new array (the "destination" array), and then
      --  deallocate the old array.

      declare
         Src : Elements_Access := Container.Elements;

      begin
         Dst.EA (Index_Type'First .. Before - 1) :=
           Src.EA (Index_Type'First .. Before - 1);

         if Before <= Container.Last then

            --  The new items are being inserted before some existing elements,
            --  so we must slide the existing elements up to their new home.

            if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then
               Index := Before + Index_Type'Base (Count);
            else
               Index := Index_Type'Base (Count_Type'Base (Before) + Count);
            end if;

            Dst.EA (Index .. New_Last) := Src.EA (Before .. Container.Last);
         end if;

         --  We have copied the elements from to the old, source array to the
         --  new, destination array, so we can now restore invariants, and
         --  deallocate the old array.

         Container.Elements := Dst;
         Container.Last := New_Last;
         Free (Src);
      end;
   end Insert_Space;

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Before cursor denotes wrong container";
      end if;

      if Count = 0 then
         if Before.Container = null or else Before.Index > Container.Last then
            Position := No_Element;
         else
            Position := (Container'Unrestricted_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert_Space (Container, Index, Count);

      Position := Cursor'(Container'Unrestricted_Access, Index);
   end Insert_Space;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Last < Index_Type'First;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor))
   is
      B : Natural renames Container'Unrestricted_Access.all.Busy;

   begin
      B := B + 1;

      begin
         for Indx in Index_Type'First .. Container.Last loop
            Process (Cursor'(Container'Unrestricted_Access, Indx));
         end loop;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   function Iterate (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      V : constant Vector_Access := Container'Unrestricted_Access;
      B : Natural renames V.Busy;

   begin
      --  The value of its Index component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Index
      --  component is No_Index (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a complete
      --  iterator, meaning that the iteration starts from the (logical)
      --  beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator :=
        (Limited_Controlled with
           Container => V,
           Index     => No_Index)
      do
         B := B + 1;
      end return;
   end Iterate;

   function Iterate
     (Container : Vector;
      Start     : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      V : constant Vector_Access := Container'Unrestricted_Access;
      B : Natural renames V.Busy;

   begin
      --  It was formerly the case that when Start = No_Element, the partial
      --  iterator was defined to behave the same as for a complete iterator,
      --  and iterate over the entire sequence of items. However, those
      --  semantics were unintuitive and arguably error-prone (it is too easy
      --  to accidentally create an endless loop), and so they were changed,
      --  per the ARG meeting in Denver on 2011/11. However, there was no
      --  consensus about what positive meaning this corner case should have,
      --  and so it was decided to simply raise an exception. This does imply,
      --  however, that it is not possible to use a partial iterator to specify
      --  an empty sequence of items.

      if Start.Container = null then
         raise Constraint_Error with
           "Start position for iterator equals No_Element";
      end if;

      if Start.Container /= V then
         raise Program_Error with
           "Start cursor of Iterate designates wrong vector";
      end if;

      if Start.Index > V.Last then
         raise Constraint_Error with
           "Start position for iterator equals No_Element";
      end if;

      --  The value of its Index component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Index
      --  component is not No_Index (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.

      return It : constant Iterator :=
        (Limited_Controlled with
           Container => V,
           Index     => Start.Index)
      do
         B := B + 1;
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unrestricted_Access, Container.Last);
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Index component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Index component is No_Index, this means the iterator
      --  object was constructed without a start expression, in which case the
      --  (reverse) iteration starts from the (logical) beginning of the entire
      --  sequence (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items.
      --  When the Index component is not No_Index, the iterator object was
      --  constructed with a start expression, that specifies the position
      --  from which the (reverse) partial iteration begins.

      if Object.Index = No_Index then
         return Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Index);
      end if;
   end Last;

   -----------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      if Container.Last = No_Index then
         raise Constraint_Error with "Container is empty";
      end if;

      declare
         EA : constant Element_Access :=
                Container.Elements.EA (Container.Last);
      begin
         if EA = null then
            raise Constraint_Error with "last element is empty";
         else
            return EA.all;
         end if;
      end;
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

   function Length (Container : Vector) return Count_Type is
      L : constant Index_Type'Base := Container.Last;
      F : constant Index_Type := Index_Type'First;

   begin
      --  The base range of the index type (Index_Type'Base) might not include
      --  all values for length (Count_Type). Contrariwise, the index type
      --  might include values outside the range of length.  Hence we use
      --  whatever type is wider for intermediate values when calculating
      --  length. Note that no matter what the index type is, the maximum
      --  length to which a vector is allowed to grow is always the minimum
      --  of Count_Type'Last and (IT'Last - IT'First + 1).

      --  For example, an Index_Type with range -127 .. 127 is only guaranteed
      --  to have a base range of -128 .. 127, but the corresponding vector
      --  would have lengths in the range 0 .. 255. In this case we would need
      --  to use Count_Type'Base for intermediate values.

      --  Another case would be the index range -2**63 + 1 .. -2**63 + 10. The
      --  vector would have a maximum length of 10, but the index values lie
      --  outside the range of Count_Type (which is only 32 bits). In this
      --  case we would need to use Index_Type'Base for intermediate values.

      if Count_Type'Base'Last >= Index_Type'Pos (Index_Type'Base'Last) then
         return Count_Type'Base (L) - Count_Type'Base (F) + 1;
      else
         return Count_Type (L - F + 1);
      end if;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Vector;
      Source : in out Vector)
   is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (Source is busy)";
      end if;

      Clear (Target);  --  Checks busy-bit

      declare
         Target_Elements : constant Elements_Access := Target.Elements;
      begin
         Target.Elements := Source.Elements;
         Source.Elements := Target_Elements;
      end;

      Target.Last := Source.Last;
      Source.Last := No_Index;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Index < Position.Container.Last then
         return (Position.Container, Position.Index + 1);
      else
         return No_Element;
      end if;
   end Next;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Next designates wrong vector";
      else
         return Next (Position);
      end if;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      elsif Position.Index < Position.Container.Last then
         Position.Index := Position.Index + 1;
      else
         Position := No_Element;
      end if;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in out Vector; New_Item : Vector) is
   begin
      Insert (Container, Index_Type'First, New_Item);
   end Prepend;

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container, Index_Type'First, New_Item, Count);
   end Prepend;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      elsif Position.Index > Index_Type'First then
         Position.Index := Position.Index - 1;
      else
         Position := No_Element;
      end if;
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Index > Index_Type'First then
         return (Position.Container, Position.Index - 1);
      else
         return No_Element;
      end if;
   end Previous;

   function Previous (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Container /= Object.Container then
         raise Program_Error with
           "Position cursor of Previous designates wrong vector";
      else
         return Previous (Position);
      end if;
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : Element_Type))
   is
      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;
      L : Natural renames V.Lock;

   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      if V.Elements.EA (Index) = null then
         raise Constraint_Error with "element is null";
      end if;

      B := B + 1;
      L := L + 1;

      begin
         Process (V.Elements.EA (Index).all);
      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;

      L := L - 1;
      B := B - 1;
   end Query_Element;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      else
         Query_Element (Position.Container.all, Position.Index, Process);
      end if;
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Vector)
   is
      Length : Count_Type'Base;
      Last   : Index_Type'Base := Index_Type'Pred (Index_Type'First);
      B      : Boolean;

   begin
      Clear (Container);

      Count_Type'Base'Read (Stream, Length);

      if Length > Capacity (Container) then
         Reserve_Capacity (Container, Capacity => Length);
      end if;

      for J in Count_Type range 1 .. Length loop
         Last := Last + 1;

         Boolean'Read (Stream, B);

         if B then
            Container.Elements.EA (Last) :=
              new Element_Type'(Element_Type'Input (Stream));
         end if;

         Container.Last := Last;
      end loop;
   end Read;

   procedure Read
     (Stream   : not null access Root_Stream_Type'Class;
      Position : out Cursor)
   is
   begin
      raise Program_Error with "attempt to stream vector cursor";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor) return Reference_Type
   is
      E : Element_Access;

   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor denotes wrong container";
      end if;

      if Position.Index > Position.Container.Last then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      E := Container.Elements.EA (Position.Index);

      if E = null then
         raise Constraint_Error with "element at Position is empty";
      end if;

      declare
         C : Vector renames Container'Unrestricted_Access.all;
         B : Natural renames C.Busy;
         L : Natural renames C.Lock;
      begin
         return R : constant Reference_Type :=
           (Element => E.all'Access,
            Control => (Controlled with Position.Container))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Reference;

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type) return Reference_Type
   is
      E : Element_Access;

   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      E := Container.Elements.EA (Index);

      if E = null then
         raise Constraint_Error with "element at Index is empty";
      end if;

      declare
         C : Vector renames Container'Unrestricted_Access.all;
         B : Natural renames C.Busy;
         L : Natural renames C.Lock;
      begin
         return R : constant Reference_Type :=
           (Element => E.all'Access,
            Control => (Controlled with Container'Unrestricted_Access))
         do
            B := B + 1;
            L := L + 1;
         end return;
      end;
   end Reference;

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

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (vector is locked)";
      end if;

      declare
         X : Element_Access := Container.Elements.EA (Index);

         --  The element allocator may need an accessibility check in the case
         --  where the actual type is class-wide or has access discriminants
         --  (see RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Container.Elements.EA (Index) := new Element_Type'(New_Item);
         Free (X);
      end;
   end Replace_Element;

   procedure Replace_Element
     (Container : in out Vector;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor denotes wrong container";
      end if;

      if Position.Index > Container.Last then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (vector is locked)";
      end if;

      declare
         X : Element_Access := Container.Elements.EA (Position.Index);

         --  The element allocator may need an accessibility check in the case
         --  where the actual type is class-wide or has access discriminants
         --  (see RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Container.Elements.EA (Position.Index) := new Element_Type'(New_Item);
         Free (X);
      end;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Count_Type)
   is
      N : constant Count_Type := Length (Container);

      Index : Count_Type'Base;
      Last  : Index_Type'Base;

   begin
      --  Reserve_Capacity can be used to either expand the storage available
      --  for elements (this would be its typical use, in anticipation of
      --  future insertion), or to trim back storage. In the latter case,
      --  storage can only be trimmed back to the limit of the container
      --  length. Note that Reserve_Capacity neither deletes (active) elements
      --  nor inserts elements; it only affects container capacity, never
      --  container length.

      if Capacity = 0 then

         --  This is a request to trim back storage, to the minimum amount
         --  possible given the current state of the container.

         if N = 0 then

            --  The container is empty, so in this unique case we can
            --  deallocate the entire internal array. Note that an empty
            --  container can never be busy, so there's no need to check the
            --  tampering bits.

            declare
               X : Elements_Access := Container.Elements;

            begin
               --  First we remove the internal array from the container, to
               --  handle the case when the deallocation raises an exception
               --  (although that's unlikely, since this is simply an array of
               --  access values, all of which are null).

               Container.Elements := null;

               --  Container invariants have been restored, so it is now safe
               --  to attempt to deallocate the internal array.

               Free (X);
            end;

         elsif N < Container.Elements.EA'Length then

            --  The container is not empty, and the current length is less than
            --  the current capacity, so there's storage available to trim. In
            --  this case, we allocate a new internal array having a length
            --  that exactly matches the number of items in the
            --  container. (Reserve_Capacity does not delete active elements,
            --  so this is the best we can do with respect to minimizing
            --  storage).

            if Container.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (vector is busy)";
            end if;

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Array renames
                       Container.Elements.EA (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;

            begin
               --  Although we have isolated the old internal array that we're
               --  going to deallocate, we don't deallocate it until we have
               --  successfully allocated a new one. If there is an exception
               --  during allocation (because there is not enough storage), we
               --  let it propagate without causing any side-effect.

               Container.Elements := new Elements_Type'(Container.Last, Src);

               --  We have successfully allocated a new internal array (with a
               --  smaller length than the old one, and containing a copy of
               --  just the active elements in the container), so we can
               --  deallocate the old array.

               Free (X);
            end;
         end if;

         return;
      end if;

      --  Reserve_Capacity can be used to expand the storage available for
      --  elements, but we do not let the capacity grow beyond the number of
      --  values in Index_Type'Range. (Were it otherwise, there would be no way
      --  to refer to the elements with index values greater than
      --  Index_Type'Last, so that storage would be wasted.) Here we compute
      --  the Last index value of the new internal array, in a way that avoids
      --  any possibility of overflow.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then

         --  We perform a two-part test. First we determine whether the
         --  computed Last value lies in the base range of the type, and then
         --  determine whether it lies in the range of the index (sub)type.

         --  Last must satisfy this relation:
         --    First + Length - 1 <= Last
         --  We regroup terms:
         --    First - 1 <= Last - Length
         --  Which can rewrite as:
         --    No_Index <= Last - Length

         if Index_Type'Base'Last - Index_Type'Base (Capacity) < No_Index then
            raise Constraint_Error with "Capacity is out of range";
         end if;

         --  We now know that the computed value of Last is within the base
         --  range of the type, so it is safe to compute its value:

         Last := No_Index + Index_Type'Base (Capacity);

         --  Finally we test whether the value is within the range of the
         --  generic actual index subtype:

         if Last > Index_Type'Last then
            raise Constraint_Error with "Capacity is out of range";
         end if;

      elsif Index_Type'First <= 0 then

         --  Here we can compute Last directly, in the normal way. We know that
         --  No_Index is less than 0, so there is no danger of overflow when
         --  adding the (positive) value of Capacity.

         Index := Count_Type'Base (No_Index) + Capacity;  -- Last

         if Index > Count_Type'Base (Index_Type'Last) then
            raise Constraint_Error with "Capacity is out of range";
         end if;

         --  We know that the computed value (having type Count_Type) of Last
         --  is within the range of the generic actual index subtype, so it is
         --  safe to convert to Index_Type:

         Last := Index_Type'Base (Index);

      else
         --  Here Index_Type'First (and Index_Type'Last) is positive, so we
         --  must test the length indirectly (by working backwards from the
         --  largest possible value of Last), in order to prevent overflow.

         Index := Count_Type'Base (Index_Type'Last) - Capacity;  -- No_Index

         if Index < Count_Type'Base (No_Index) then
            raise Constraint_Error with "Capacity is out of range";
         end if;

         --  We have determined that the value of Capacity would not create a
         --  Last index value outside of the range of Index_Type, so we can now
         --  safely compute its value.

         Last := Index_Type'Base (Count_Type'Base (No_Index) + Capacity);
      end if;

      --  The requested capacity is non-zero, but we don't know yet whether
      --  this is a request for expansion or contraction of storage.

      if Container.Elements = null then

         --  The container is empty (it doesn't even have an internal array),
         --  so this represents a request to allocate storage having the given
         --  capacity.

         Container.Elements := new Elements_Type (Last);
         return;
      end if;

      if Capacity <= N then

         --  This is a request to trim back storage, but only to the limit of
         --  what's already in the container. (Reserve_Capacity never deletes
         --  active elements, it only reclaims excess storage.)

         if N < Container.Elements.EA'Length then

            --  The container is not empty (because the requested capacity is
            --  positive, and less than or equal to the container length), and
            --  the current length is less than the current capacity, so there
            --  is storage available to trim. In this case, we allocate a new
            --  internal array having a length that exactly matches the number
            --  of items in the container.

            if Container.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (vector is busy)";
            end if;

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Array renames
                       Container.Elements.EA (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;

            begin
               --  Although we have isolated the old internal array that we're
               --  going to deallocate, we don't deallocate it until we have
               --  successfully allocated a new one. If there is an exception
               --  during allocation (because there is not enough storage), we
               --  let it propagate without causing any side-effect.

               Container.Elements := new Elements_Type'(Container.Last, Src);

               --  We have successfully allocated a new internal array (with a
               --  smaller length than the old one, and containing a copy of
               --  just the active elements in the container), so it is now
               --  safe to deallocate the old array.

               Free (X);
            end;
         end if;

         return;
      end if;

      --  The requested capacity is larger than the container length (the
      --  number of active elements). Whether this represents a request for
      --  expansion or contraction of the current capacity depends on what the
      --  current capacity is.

      if Capacity = Container.Elements.EA'Length then

         --  The requested capacity matches the existing capacity, so there's
         --  nothing to do here. We treat this case as a no-op, and simply
         --  return without checking the busy bit.

         return;
      end if;

      --  There is a change in the capacity of a non-empty container, so a new
      --  internal array will be allocated. (The length of the new internal
      --  array could be less or greater than the old internal array. We know
      --  only that the length of the new internal array is greater than the
      --  number of active elements in the container.) We must check whether
      --  the container is busy before doing anything else.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
      end if;

      --  We now allocate a new internal array, having a length different from
      --  its current value.

      declare
         X : Elements_Access := Container.Elements;

         subtype Index_Subtype is Index_Type'Base range
           Index_Type'First .. Container.Last;

      begin
         --  We now allocate a new internal array, having a length different
         --  from its current value.

         Container.Elements := new Elements_Type (Last);

         --  We have successfully allocated the new internal array, so now we
         --  move the existing elements from the existing the old internal
         --  array onto the new one. Note that we're just copying access
         --  values, to this should not raise any exceptions.

         Container.Elements.EA (Index_Subtype) := X.EA (Index_Subtype);

         --  We have moved the elements from the old internal array, so now we
         --  can deallocate it.

         Free (X);
      end;
   end Reserve_Capacity;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      if Container.Length <= 1 then
         return;
      end if;

      --  The exception behavior for the vector container must match that for
      --  the list container, so we check for cursor tampering here (which will
      --  catch more things) instead of for element tampering (which will catch
      --  fewer things). It's true that the elements of this vector container
      --  could be safely moved around while (say) an iteration is taking place
      --  (iteration only increments the busy counter), and so technically all
      --  we would need here is a test for element tampering (indicated by the
      --  lock counter), that's simply an artifact of our array-based
      --  implementation. Logically Reverse_Elements requires a check for
      --  cursor tampering.

      if Container.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (vector is busy)";
      end if;

      declare
         I : Index_Type;
         J : Index_Type;
         E : Elements_Array renames Container.Elements.EA;

      begin
         I := Index_Type'First;
         J := Container.Last;
         while I < J loop
            declare
               EI : constant Element_Access := E (I);

            begin
               E (I) := E (J);
               E (J) := EI;
            end;

            I := I + 1;
            J := J - 1;
         end loop;
      end;
   end Reverse_Elements;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Last : Index_Type'Base;

   begin
      if Position.Container /= null
        and then Position.Container /= Container'Unrestricted_Access
      then
         raise Program_Error with "Position cursor denotes wrong container";
      end if;

      if Position.Container = null or else Position.Index > Container.Last then
         Last := Container.Last;
      else
         Last := Position.Index;
      end if;

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         B : Natural renames Container'Unrestricted_Access.Busy;
         L : Natural renames Container'Unrestricted_Access.Lock;

         Result : Index_Type'Base;

      begin
         B := B + 1;
         L := L + 1;

         Result := No_Index;
         for Indx in reverse Index_Type'First .. Last loop
            if Container.Elements.EA (Indx) /= null
              and then Container.Elements.EA (Indx).all = Item
            then
               Result := Indx;
               exit;
            end if;
         end loop;

         B := B - 1;
         L := L - 1;

         if Result = No_Index then
            return No_Element;
         else
            return Cursor'(Container'Unrestricted_Access, Result);
         end if;

      exception
         when others =>
            B := B - 1;
            L := L - 1;
            raise;
      end;
   end Reverse_Find;

   ------------------------
   -- Reverse_Find_Index --
   ------------------------

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   is
      B : Natural renames Container'Unrestricted_Access.Busy;
      L : Natural renames Container'Unrestricted_Access.Lock;

      Last : constant Index_Type'Base :=
        (if Index > Container.Last then Container.Last else Index);

      Result : Index_Type'Base;

   begin
      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      B := B + 1;
      L := L + 1;

      Result := No_Index;
      for Indx in reverse Index_Type'First .. Last loop
         if Container.Elements.EA (Indx) /= null
           and then Container.Elements.EA (Indx).all = Item
         then
            Result := Indx;
            exit;
         end if;
      end loop;

      B := B - 1;
      L := L - 1;

      return Result;

   exception
      when others =>
         B := B - 1;
         L := L - 1;
         raise;
   end Reverse_Find_Index;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor))
   is
      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;

   begin
      B := B + 1;

      begin
         for Indx in reverse Index_Type'First .. Container.Last loop
            Process (Cursor'(Container'Unrestricted_Access, Indx));
         end loop;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Reverse_Iterate;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Container : in out Vector;
      Length    : Count_Type)
   is
      Count : constant Count_Type'Base := Container.Length - Length;

   begin
      --  Set_Length allows the user to set the length explicitly, instead of
      --  implicitly as a side-effect of deletion or insertion. If the
      --  requested length is less than the current length, this is equivalent
      --  to deleting items from the back end of the vector. If the requested
      --  length is greater than the current length, then this is equivalent to
      --  inserting "space" (nonce items) at the end.

      if Count >= 0 then
         Container.Delete_Last (Count);

      elsif Container.Last >= Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";

      else
         Container.Insert_Space (Container.Last + 1, -Count);
      end if;
   end Set_Length;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : in out Vector;
      I, J      : Index_Type)
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

      if Container.Lock > 0 then
         raise Program_Error with
           "attempt to tamper with elements (vector is locked)";
      end if;

      declare
         EI : Element_Access renames Container.Elements.EA (I);
         EJ : Element_Access renames Container.Elements.EA (J);

         EI_Copy : constant Element_Access := EI;

      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   procedure Swap
     (Container : in out Vector;
      I, J      : Cursor)
   is
   begin
      if I.Container = null then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if J.Container = null then
         raise Constraint_Error with "J cursor has no element";
      end if;

      if I.Container /= Container'Unrestricted_Access then
         raise Program_Error with "I cursor denotes wrong container";
      end if;

      if J.Container /= Container'Unrestricted_Access then
         raise Program_Error with "J cursor denotes wrong container";
      end if;

      Swap (Container, I.Index, J.Index);
   end Swap;

   ---------------
   -- To_Cursor --
   ---------------

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index) return Cursor
   is
   begin
      if Index not in Index_Type'First .. Container.Last then
         return No_Element;
      end if;

      return Cursor'(Container'Unrestricted_Access, Index);
   end To_Cursor;

   --------------
   -- To_Index --
   --------------

   function To_Index (Position : Cursor) return Extended_Index is
   begin
      if Position.Container = null then
         return No_Index;
      elsif Position.Index <= Position.Container.Last then
         return Position.Index;
      else
         return No_Index;
      end if;
   end To_Index;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Length : Count_Type) return Vector is
      Index    : Count_Type'Base;
      Last     : Index_Type'Base;
      Elements : Elements_Access;

   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      --  We create a vector object with a capacity that matches the specified
      --  Length, but we do not allow the vector capacity (the length of the
      --  internal array) to exceed the number of values in Index_Type'Range
      --  (otherwise, there would be no way to refer to those components via an
      --  index).  We must therefore check whether the specified Length would
      --  create a Last index value greater than Index_Type'Last.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then

         --  We perform a two-part test. First we determine whether the
         --  computed Last value lies in the base range of the type, and then
         --  determine whether it lies in the range of the index (sub)type.

         --  Last must satisfy this relation:
         --    First + Length - 1 <= Last
         --  We regroup terms:
         --    First - 1 <= Last - Length
         --  Which can rewrite as:
         --    No_Index <= Last - Length

         if Index_Type'Base'Last - Index_Type'Base (Length) < No_Index then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We now know that the computed value of Last is within the base
         --  range of the type, so it is safe to compute its value:

         Last := No_Index + Index_Type'Base (Length);

         --  Finally we test whether the value is within the range of the
         --  generic actual index subtype:

         if Last > Index_Type'Last then
            raise Constraint_Error with "Length is out of range";
         end if;

      elsif Index_Type'First <= 0 then

         --  Here we can compute Last directly, in the normal way. We know that
         --  No_Index is less than 0, so there is no danger of overflow when
         --  adding the (positive) value of Length.

         Index := Count_Type'Base (No_Index) + Length;  -- Last

         if Index > Count_Type'Base (Index_Type'Last) then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We know that the computed value (having type Count_Type) of Last
         --  is within the range of the generic actual index subtype, so it is
         --  safe to convert to Index_Type:

         Last := Index_Type'Base (Index);

      else
         --  Here Index_Type'First (and Index_Type'Last) is positive, so we
         --  must test the length indirectly (by working backwards from the
         --  largest possible value of Last), in order to prevent overflow.

         Index := Count_Type'Base (Index_Type'Last) - Length;  -- No_Index

         if Index < Count_Type'Base (No_Index) then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We have determined that the value of Length would not create a
         --  Last index value outside of the range of Index_Type, so we can now
         --  safely compute its value.

         Last := Index_Type'Base (Count_Type'Base (No_Index) + Length);
      end if;

      Elements := new Elements_Type (Last);

      return Vector'(Controlled with Elements, Last, 0, 0);
   end To_Vector;

   function To_Vector
     (New_Item : Element_Type;
      Length   : Count_Type) return Vector
   is
      Index    : Count_Type'Base;
      Last     : Index_Type'Base;
      Elements : Elements_Access;

   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      --  We create a vector object with a capacity that matches the specified
      --  Length, but we do not allow the vector capacity (the length of the
      --  internal array) to exceed the number of values in Index_Type'Range
      --  (otherwise, there would be no way to refer to those components via an
      --  index). We must therefore check whether the specified Length would
      --  create a Last index value greater than Index_Type'Last.

      if Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last) then

         --  We perform a two-part test. First we determine whether the
         --  computed Last value lies in the base range of the type, and then
         --  determine whether it lies in the range of the index (sub)type.

         --  Last must satisfy this relation:
         --    First + Length - 1 <= Last
         --  We regroup terms:
         --    First - 1 <= Last - Length
         --  Which can rewrite as:
         --    No_Index <= Last - Length

         if Index_Type'Base'Last - Index_Type'Base (Length) < No_Index then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We now know that the computed value of Last is within the base
         --  range of the type, so it is safe to compute its value:

         Last := No_Index + Index_Type'Base (Length);

         --  Finally we test whether the value is within the range of the
         --  generic actual index subtype:

         if Last > Index_Type'Last then
            raise Constraint_Error with "Length is out of range";
         end if;

      elsif Index_Type'First <= 0 then

         --  Here we can compute Last directly, in the normal way. We know that
         --  No_Index is less than 0, so there is no danger of overflow when
         --  adding the (positive) value of Length.

         Index := Count_Type'Base (No_Index) + Length;  -- Last

         if Index > Count_Type'Base (Index_Type'Last) then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We know that the computed value (having type Count_Type) of Last
         --  is within the range of the generic actual index subtype, so it is
         --  safe to convert to Index_Type:

         Last := Index_Type'Base (Index);

      else
         --  Here Index_Type'First (and Index_Type'Last) is positive, so we
         --  must test the length indirectly (by working backwards from the
         --  largest possible value of Last), in order to prevent overflow.

         Index := Count_Type'Base (Index_Type'Last) - Length;  -- No_Index

         if Index < Count_Type'Base (No_Index) then
            raise Constraint_Error with "Length is out of range";
         end if;

         --  We have determined that the value of Length would not create a
         --  Last index value outside of the range of Index_Type, so we can now
         --  safely compute its value.

         Last := Index_Type'Base (Count_Type'Base (No_Index) + Length);
      end if;

      Elements := new Elements_Type (Last);

      --  We use Last as the index of the loop used to populate the internal
      --  array with items. In general, we prefer to initialize the loop index
      --  immediately prior to entering the loop. However, Last is also used in
      --  the exception handler (to reclaim elements that have been allocated,
      --  before propagating the exception), and the initialization of Last
      --  after entering the block containing the handler confuses some static
      --  analysis tools, with respect to whether Last has been properly
      --  initialized when the handler executes. So here we initialize our loop
      --  variable earlier than we prefer, before entering the block, so there
      --  is no ambiguity.

      Last := Index_Type'First;

      declare
         --  The element allocator may need an accessibility check in the case
         --  where the actual type is class-wide or has access discriminants
         --  (see RM 4.8(10.1) and AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         loop
            Elements.EA (Last) := new Element_Type'(New_Item);
            exit when Last = Elements.Last;
            Last := Last + 1;
         end loop;

      exception
         when others =>
            for J in Index_Type'First .. Last - 1 loop
               Free (Elements.EA (J));
            end loop;

            Free (Elements);
            raise;
      end;

      return (Controlled with Elements, Last, 0, 0);
   end To_Vector;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type))
   is
      B : Natural renames Container.Busy;
      L : Natural renames Container.Lock;

   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      if Container.Elements.EA (Index) = null then
         raise Constraint_Error with "element is null";
      end if;

      B := B + 1;
      L := L + 1;

      begin
         Process (Container.Elements.EA (Index).all);
      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;

      L := L - 1;
      B := B - 1;
   end Update_Element;

   procedure Update_Element
     (Container : in out Vector;
      Position  : Cursor;
      Process   : not null access procedure (Element : in out Element_Type))
   is
   begin
      if Position.Container = null then
         raise Constraint_Error with "Position cursor has no element";

      elsif Position.Container /= Container'Unrestricted_Access then
         raise Program_Error with "Position cursor denotes wrong container";

      else
         Update_Element (Container, Position.Index, Process);
      end if;
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Vector)
   is
      N : constant Count_Type := Length (Container);

   begin
      Count_Type'Base'Write (Stream, N);

      if N = 0 then
         return;
      end if;

      declare
         E : Elements_Array renames Container.Elements.EA;

      begin
         for Indx in Index_Type'First .. Container.Last loop
            if E (Indx) = null then
               Boolean'Write (Stream, False);
            else
               Boolean'Write (Stream, True);
               Element_Type'Output (Stream, E (Indx).all);
            end if;
         end loop;
      end;
   end Write;

   procedure Write
     (Stream   : not null access Root_Stream_Type'Class;
      Position : Cursor)
   is
   begin
      raise Program_Error with "attempt to stream vector cursor";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

end Ada.Containers.Indefinite_Vectors;
