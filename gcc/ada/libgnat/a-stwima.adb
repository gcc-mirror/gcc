------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                A D A . S T R I N G S . W I D E _ M A P S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Ada.Strings.Wide_Maps is

   ---------
   -- "-" --
   ---------

   function "-"
     (Left, Right : Wide_Character_Set) return Wide_Character_Set
   is
      LS : constant Wide_Character_Ranges_Access := Left.Set;
      RS : constant Wide_Character_Ranges_Access := Right.Set;

      Result : Wide_Character_Ranges (1 .. LS'Last + RS'Last);
      --  Each range on the right can generate at least one more range in
      --  the result, by splitting one of the left operand ranges.

      N  : Natural := 0;
      R  : Natural := 1;
      L  : Natural := 1;

      Left_Low : Wide_Character;
      --  Left_Low is lowest character of the L'th range not yet dealt with

   begin
      if LS'Last = 0 or else RS'Last = 0 then
         return Left;
      end if;

      Left_Low := LS (L).Low;
      while R <= RS'Last loop

         --  If next right range is below current left range, skip it

         if RS (R).High < Left_Low then
            R := R + 1;

         --  If next right range above current left range, copy remainder
         --  of the left range to the result

         elsif RS (R).Low > LS (L).High then
            N := N + 1;
            Result (N).Low  := Left_Low;
            Result (N).High := LS (L).High;
            L := L + 1;
            exit when L > LS'Last;
            Left_Low := LS (L).Low;

         else
            --  Next right range overlaps bottom of left range

            if RS (R).Low <= Left_Low then

               --  Case of right range complete overlaps left range

               if RS (R).High >= LS (L).High then
                  L := L + 1;
                  exit when L > LS'Last;
                  Left_Low := LS (L).Low;

               --  Case of right range eats lower part of left range

               else
                  Left_Low := Wide_Character'Succ (RS (R).High);
                  R := R + 1;
               end if;

            --  Next right range overlaps some of left range, but not bottom

            else
               N := N + 1;
               Result (N).Low  := Left_Low;
               Result (N).High := Wide_Character'Pred (RS (R).Low);

               --  Case of right range splits left range

               if RS (R).High < LS (L).High then
                  Left_Low := Wide_Character'Succ (RS (R).High);
                  R := R + 1;

               --  Case of right range overlaps top of left range

               else
                  L := L + 1;
                  exit when L > LS'Last;
                  Left_Low := LS (L).Low;
               end if;
            end if;
         end if;
      end loop;

      --  Copy remainder of left ranges to result

      if L <= LS'Last then
         N := N + 1;
         Result (N).Low  := Left_Low;
         Result (N).High := LS (L).High;

         loop
            L := L + 1;
            exit when L > LS'Last;
            N := N + 1;
            Result (N) := LS (L);
         end loop;
      end if;

      return (AF.Controlled with
              Set => new Wide_Character_Ranges'(Result (1 .. N)));
   end "-";

   ---------
   -- "=" --
   ---------

   --  The sorted, discontiguous form is canonical, so equality can be used

   function "=" (Left, Right : Wide_Character_Set) return Boolean is
   begin
      return Left.Set.all = Right.Set.all;
   end "=";

   -----------
   -- "and" --
   -----------

   function "and"
     (Left, Right : Wide_Character_Set) return Wide_Character_Set
   is
      LS : constant Wide_Character_Ranges_Access := Left.Set;
      RS : constant Wide_Character_Ranges_Access := Right.Set;

      Result : Wide_Character_Ranges (1 .. LS'Last + RS'Last);
      N      : Natural := 0;
      L, R   : Natural := 1;

   begin
      --  Loop to search for overlapping character ranges

      while L <= LS'Last and then R <= RS'Last loop

         if LS (L).High < RS (R).Low then
            L := L + 1;

         elsif RS (R).High < LS (L).Low then
            R := R + 1;

         --  Here we have LS (L).High >= RS (R).Low
         --           and RS (R).High >= LS (L).Low
         --  so we have an overlapping range

         else
            N := N + 1;
            Result (N).Low := Wide_Character'Max (LS (L).Low,  RS (R).Low);
            Result (N).High :=
              Wide_Character'Min (LS (L).High, RS (R).High);

            if RS (R).High = LS (L).High then
               L := L + 1;
               R := R + 1;
            elsif RS (R).High < LS (L).High then
               R := R + 1;
            else
               L := L + 1;
            end if;
         end if;
      end loop;

      return (AF.Controlled with
              Set       => new Wide_Character_Ranges'(Result (1 .. N)));
   end "and";

   -----------
   -- "not" --
   -----------

   function "not"
     (Right : Wide_Character_Set) return Wide_Character_Set
   is
      RS : constant Wide_Character_Ranges_Access := Right.Set;

      Result : Wide_Character_Ranges (1 .. RS'Last + 1);
      N      : Natural := 0;

   begin
      if RS'Last = 0 then
         N := 1;
         Result (1) := (Low  => Wide_Character'First,
                        High => Wide_Character'Last);

      else
         if RS (1).Low /= Wide_Character'First then
            N := N + 1;
            Result (N).Low  := Wide_Character'First;
            Result (N).High := Wide_Character'Pred (RS (1).Low);
         end if;

         for K in 1 .. RS'Last - 1 loop
            N := N + 1;
            Result (N).Low  := Wide_Character'Succ (RS (K).High);
            Result (N).High := Wide_Character'Pred (RS (K + 1).Low);
         end loop;

         if RS (RS'Last).High /= Wide_Character'Last then
            N := N + 1;
            Result (N).Low  := Wide_Character'Succ (RS (RS'Last).High);
            Result (N).High := Wide_Character'Last;
         end if;
      end if;

      return (AF.Controlled with
              Set => new Wide_Character_Ranges'(Result (1 .. N)));
   end "not";

   ----------
   -- "or" --
   ----------

   function "or"
     (Left, Right : Wide_Character_Set) return Wide_Character_Set
   is
      LS : constant Wide_Character_Ranges_Access := Left.Set;
      RS : constant Wide_Character_Ranges_Access := Right.Set;

      Result : Wide_Character_Ranges (1 .. LS'Last + RS'Last);
      N      : Natural;
      L, R   : Natural;

   begin
      N := 0;
      L := 1;
      R := 1;

      --  Loop through ranges in output file

      loop
         --  If no left ranges left, copy next right range

         if L > LS'Last then
            exit when R > RS'Last;
            N := N + 1;
            Result (N) := RS (R);
            R := R + 1;

         --  If no right ranges left, copy next left range

         elsif R > RS'Last then
            N := N + 1;
            Result (N) := LS (L);
            L := L + 1;

         else
            --  We have two ranges, choose lower one

            N := N + 1;

            if LS (L).Low <= RS (R).Low then
               Result (N) := LS (L);
               L := L + 1;
            else
               Result (N) := RS (R);
               R := R + 1;
            end if;

            --  Loop to collapse ranges into last range

            loop
               --  Collapse next length range into current result range
               --  if possible.

               if L <= LS'Last
                 and then LS (L).Low <= Wide_Character'Succ (Result (N).High)
               then
                  Result (N).High :=
                    Wide_Character'Max (Result (N).High, LS (L).High);
                  L := L + 1;

               --  Collapse next right range into current result range
               --  if possible

               elsif R <= RS'Last
                 and then RS (R).Low <=
                            Wide_Character'Succ (Result (N).High)
               then
                  Result (N).High :=
                    Wide_Character'Max (Result (N).High, RS (R).High);
                  R := R + 1;

               --  If neither range collapses, then done with this range

               else
                  exit;
               end if;
            end loop;
         end if;
      end loop;

      return (AF.Controlled with
              Set => new Wide_Character_Ranges'(Result (1 .. N)));
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor"
     (Left, Right : Wide_Character_Set) return Wide_Character_Set
   is
   begin
      return (Left or Right) - (Left and Right);
   end "xor";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Wide_Character_Mapping) is
   begin
      Object.Map := new Wide_Character_Mapping_Values'(Object.Map.all);
   end Adjust;

   procedure Adjust (Object : in out Wide_Character_Set) is
   begin
      Object.Set := new Wide_Character_Ranges'(Object.Set.all);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Wide_Character_Mapping) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Wide_Character_Mapping_Values,
         Wide_Character_Mapping_Values_Access);

   begin
      if Object.Map /= Null_Map'Unrestricted_Access then
         Free (Object.Map);
      end if;
   end Finalize;

   procedure Finalize (Object : in out Wide_Character_Set) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Wide_Character_Ranges,
         Wide_Character_Ranges_Access);

   begin
      if Object.Set /= Null_Range'Unrestricted_Access then
         Free (Object.Set);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Wide_Character_Mapping) is
   begin
      Object := Identity;
   end Initialize;

   procedure Initialize (Object : in out Wide_Character_Set) is
   begin
      Object := Null_Set;
   end Initialize;

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Element : Wide_Character;
      Set     : Wide_Character_Set) return Boolean
   is
      L, R, M : Natural;
      SS      : constant Wide_Character_Ranges_Access := Set.Set;

   begin
      L := 1;
      R := SS'Last;

      --  Binary search loop. The invariant is that if Element is in any of
      --  of the constituent ranges it is in one between Set (L) and Set (R).

      loop
         if L > R then
            return False;

         else
            M := (L + R) / 2;

            if Element > SS (M).High then
               L := M + 1;
            elsif Element < SS (M).Low then
               R := M - 1;
            else
               return True;
            end if;
         end if;
      end loop;
   end Is_In;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset
     (Elements : Wide_Character_Set;
      Set      : Wide_Character_Set) return Boolean
   is
      ES : constant Wide_Character_Ranges_Access := Elements.Set;
      SS : constant Wide_Character_Ranges_Access := Set.Set;

      S  : Positive := 1;
      E  : Positive := 1;

   begin
      loop
         --  If no more element ranges, done, and result is true

         if E > ES'Last then
            return True;

         --  If more element ranges, but no more set ranges, result is false

         elsif S > SS'Last then
            return False;

         --  Remove irrelevant set range

         elsif SS (S).High < ES (E).Low then
            S := S + 1;

         --  Get rid of element range that is properly covered by set

         elsif SS (S).Low <= ES (E).Low
            and then ES (E).High <= SS (S).High
         then
            E := E + 1;

         --  Otherwise we have a non-covered element range, result is false

         else
            return False;
         end if;
      end loop;
   end Is_Subset;

   ---------------
   -- To_Domain --
   ---------------

   function To_Domain
     (Map : Wide_Character_Mapping) return Wide_Character_Sequence
   is
   begin
      return Map.Map.Domain;
   end To_Domain;

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping
     (From, To : Wide_Character_Sequence) return Wide_Character_Mapping
   is
      Domain : Wide_Character_Sequence (1 .. From'Length);
      Rangev : Wide_Character_Sequence (1 .. To'Length);
      N      : Natural := 0;

   begin
      if From'Length /= To'Length then
         raise Translation_Error;

      else
         pragma Warnings (Off); -- apparent uninit use of Domain

         for J in From'Range loop
            for M in 1 .. N loop
               if From (J) = Domain (M) then
                  raise Translation_Error;
               elsif From (J) < Domain (M) then
                  Domain (M + 1 .. N + 1) := Domain (M .. N);
                  Rangev (M + 1 .. N + 1) := Rangev (M .. N);
                  Domain (M) := From (J);
                  Rangev (M) := To   (J);
                  goto Continue;
               end if;
            end loop;

            Domain (N + 1) := From (J);
            Rangev (N + 1) := To   (J);

            <<Continue>>
               N := N + 1;
         end loop;

         pragma Warnings (On);

         return (AF.Controlled with
                 Map => new Wide_Character_Mapping_Values'(
                          Length => N,
                          Domain => Domain (1 .. N),
                          Rangev => Rangev (1 .. N)));
      end if;
   end To_Mapping;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Map : Wide_Character_Mapping) return Wide_Character_Sequence
   is
   begin
      return Map.Map.Rangev;
   end To_Range;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges
     (Set : Wide_Character_Set) return Wide_Character_Ranges
   is
   begin
      return Set.Set.all;
   end To_Ranges;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Set : Wide_Character_Set) return Wide_Character_Sequence
   is
      SS    : constant Wide_Character_Ranges_Access := Set.Set;
      N     : Natural := 0;
      Count : Natural := 0;

   begin
      for J in SS'Range loop
         Count :=
           Count + (Wide_Character'Pos (SS (J).High) -
                    Wide_Character'Pos (SS (J).Low) + 1);
      end loop;

      return Result : Wide_String (1 .. Count) do
         for J in SS'Range loop
            for K in SS (J).Low .. SS (J).High loop
               N := N + 1;
               Result (N) := K;
            end loop;
         end loop;
      end return;
   end To_Sequence;

   ------------
   -- To_Set --
   ------------

   --  Case of multiple range input

   function To_Set
     (Ranges : Wide_Character_Ranges) return Wide_Character_Set
   is
      Result : Wide_Character_Ranges (Ranges'Range);
      N      : Natural := 0;
      J      : Natural;

   begin
      --  The output of To_Set is required to be sorted by increasing Low
      --  values, and discontiguous, so first we sort them as we enter them,
      --  using a simple insertion sort.

      pragma Warnings (Off);
      --  Kill bogus warning on Result being uninitialized

      for J in Ranges'Range loop
         for K in 1 .. N loop
            if Ranges (J).Low < Result (K).Low then
               Result (K + 1 .. N + 1) := Result (K .. N);
               Result (K) := Ranges (J);
               goto Continue;
            end if;
         end loop;

         Result (N + 1) := Ranges (J);

         <<Continue>>
            N := N + 1;
      end loop;

      pragma Warnings (On);

      --  Now collapse any contiguous or overlapping ranges

      J := 1;
      while J < N loop
         if Result (J).High < Result (J).Low then
            N := N - 1;
            Result (J .. N) := Result (J + 1 .. N + 1);

         elsif Wide_Character'Succ (Result (J).High) >= Result (J + 1).Low then
            Result (J).High :=
              Wide_Character'Max (Result (J).High, Result (J + 1).High);

            N := N - 1;
            Result (J + 1 .. N) := Result (J + 2 .. N + 1);

         else
            J := J + 1;
         end if;
      end loop;

      if N > 0 and then Result (N).High < Result (N).Low then
         N := N - 1;
      end if;

      return (AF.Controlled with
              Set => new Wide_Character_Ranges'(Result (1 .. N)));
   end To_Set;

   --  Case of single range input

   function To_Set
     (Span : Wide_Character_Range) return Wide_Character_Set
   is
   begin
      if Span.Low > Span.High then
         return Null_Set;
         --  This is safe, because there is no procedure with parameter
         --  Wide_Character_Set of mode "out" or "in out".

      else
         return (AF.Controlled with
                 Set => new Wide_Character_Ranges'(1 => Span));
      end if;
   end To_Set;

   --  Case of wide string input

   function To_Set
     (Sequence : Wide_Character_Sequence) return Wide_Character_Set
   is
      R : Wide_Character_Ranges (1 .. Sequence'Length);

   begin
      for J in R'Range loop
         R (J) := (Sequence (J), Sequence (J));
      end loop;

      return To_Set (R);
   end To_Set;

   --  Case of single wide character input

   function To_Set
     (Singleton : Wide_Character) return Wide_Character_Set
   is
   begin
      return
        (AF.Controlled with
         Set => new Wide_Character_Ranges'(1 => (Singleton, Singleton)));
   end To_Set;

   -----------
   -- Value --
   -----------

   function Value
     (Map     : Wide_Character_Mapping;
      Element : Wide_Character) return Wide_Character
   is
      L, R, M : Natural;

      MV : constant Wide_Character_Mapping_Values_Access := Map.Map;

   begin
      L := 1;
      R := MV.Domain'Last;

      --  Binary search loop

      loop
         --  If not found, identity

         if L > R then
            return Element;

         --  Otherwise do binary divide

         else
            M := (L + R) / 2;

            if Element < MV.Domain (M) then
               R := M - 1;

            elsif Element > MV.Domain (M) then
               L := M + 1;

            else --  Element = MV.Domain (M) then
               return MV.Rangev (M);
            end if;
         end if;
      end loop;
   end Value;

end Ada.Strings.Wide_Maps;
