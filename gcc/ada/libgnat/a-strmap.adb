------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . S T R I N G S . M A P S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  Note: parts of this code are derived from the ADAR.CSH public domain
--  Ada 83 versions of the Appendix C string handling packages. The main
--  differences are that we avoid the use of the minimize function which
--  is bit-by-bit or character-by-character and therefore rather slow.
--  Generally for character sets we favor the full 32-byte representation.

--  Assertions, ghost code and loop invariants in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Assert         => Ignore,
                         Ghost          => Ignore,
                         Loop_Invariant => Ignore);

package body Ada.Strings.Maps
  with SPARK_Mode
is

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Character_Set) return Character_Set is
   begin
      return Left and not Right;
   end "-";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Character_Set) return Boolean is
   begin
      return Character_Set_Internal (Left) = Character_Set_Internal (Right);
   end "=";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set
        (Character_Set_Internal (Left) and Character_Set_Internal (Right));
   end "and";

   -----------
   -- "not" --
   -----------

   function "not" (Right : Character_Set) return Character_Set is
   begin
      return Character_Set (not Character_Set_Internal (Right));
   end "not";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set
        (Character_Set_Internal (Left) or Character_Set_Internal (Right));
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set
        (Character_Set_Internal (Left) xor Character_Set_Internal (Right));
   end "xor";

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Element : Character;
      Set     : Character_Set) return Boolean
   is
      (Set (Element));

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset
     (Elements : Character_Set;
      Set      : Character_Set) return Boolean
   is
   begin
      return (Elements and Set) = Elements;
   end Is_Subset;

   ---------------
   -- To_Domain --
   ---------------

   function To_Domain (Map : Character_Mapping) return Character_Sequence is
      Result : String (1 .. Map'Length) with Relaxed_Initialization;
      J      : Natural;

      type Character_Index is array (Character) of Natural with Ghost;
      Indexes : Character_Index := [others => 0] with Ghost;

   begin
      J := 0;
      for C in Map'Range loop
         if Map (C) /= C then
            J := J + 1;
            Result (J) := C;
            Indexes (C) := J;
         end if;

         pragma Loop_Invariant (if Map = Identity then J = 0);
         pragma Loop_Invariant (J <= Character'Pos (C) + 1);
         pragma Loop_Invariant (for all K in 1 .. J => Result (K)'Initialized);
         pragma Loop_Invariant (for all K in 1 .. J => Result (K) <= C);
         pragma Loop_Invariant
           (SPARK_Proof_Sorted_Character_Sequence (Result (1 .. J)));
         pragma Loop_Invariant
           (for all D in Map'First .. C =>
              (if Map (D) = D then
                 Indexes (D) = 0
               else
                 Indexes (D) in 1 .. J
                   and then Result (Indexes (D)) = D));
         pragma Loop_Invariant
           (for all Char of Result (1 .. J) => Map (Char) /= Char);
      end loop;

      return Result (1 .. J);
   end To_Domain;

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping
     (From, To : Character_Sequence) return Character_Mapping
   is
      Result   : Character_Mapping with Relaxed_Initialization;
      Inserted : Character_Set := Null_Set;
      From_Len : constant Natural := From'Length;
      To_Len   : constant Natural := To'Length;

   begin
      if From_Len /= To_Len then
         raise Strings.Translation_Error;
      end if;

      for Char in Character loop
         Result (Char) := Char;
         pragma Loop_Invariant (Result (Result'First .. Char)'Initialized);
         pragma Loop_Invariant
           (for all C in Result'First .. Char => Result (C) = C);
      end loop;

      for J in From'Range loop
         if Inserted (From (J)) then
            raise Strings.Translation_Error;
         end if;

         Result   (From (J)) := To (J - From'First + To'First);
         Inserted (From (J)) := True;

         pragma Loop_Invariant (Result'Initialized);
         pragma Loop_Invariant
           (for all K in From'First .. J =>
              Result (From (K)) = To (K - From'First + To'First)
                and then Inserted (From (K)));
         pragma Loop_Invariant
           (for all Char in Character =>
              (Inserted (Char) =
                 (for some K in From'First .. J => Char = From (K))));
         pragma Loop_Invariant
           (for all Char in Character =>
              (if not Inserted (Char) then Result (Char) = Char));
         pragma Loop_Invariant
           (if (for all K in From'First .. J =>
                  From (K) = To (J - From'First + To'First))
            then Result = Identity);
      end loop;

      return Result;
   end To_Mapping;

   --------------
   -- To_Range --
   --------------

   function To_Range (Map : Character_Mapping) return Character_Sequence is

      --  Extract from the postcondition of To_Domain the essential properties
      --  that define Seq as the domain of Map.
      function Is_Domain
        (Map : Character_Mapping;
         Seq : Character_Sequence)
         return Boolean
      is
        (Seq'First = 1
           and then
         SPARK_Proof_Sorted_Character_Sequence (Seq)
           and then
         (for all Char in Character =>
            (if (for all X of Seq => X /= Char)
             then Map (Char) = Char))
           and then
         (for all Char of Seq => Map (Char) /= Char))
      with
        Ghost;

      --  Given Map, there is a unique sequence Seq for which
      --  Is_Domain(Map,Seq) holds.
      procedure Lemma_Domain_Unicity
        (Map        : Character_Mapping;
         Seq1, Seq2 : Character_Sequence)
      with
        Ghost,
        Pre  => Is_Domain (Map, Seq1)
          and then Is_Domain (Map, Seq2),
        Post => Seq1 = Seq2;

      --  Isolate the proof that To_Domain(Map) returns a sequence for which
      --  Is_Domain holds.
      procedure Lemma_Is_Domain (Map : Character_Mapping)
      with
        Ghost,
        Post => Is_Domain (Map, To_Domain (Map));

      --  Deduce the alternative expression of sortedness from the one in
      --  SPARK_Proof_Sorted_Character_Sequence which compares consecutive
      --  elements.
      procedure Lemma_Is_Sorted (Seq : Character_Sequence)
      with
        Ghost,
        Pre  => SPARK_Proof_Sorted_Character_Sequence (Seq),
        Post => (for all J in Seq'Range =>
                   (for all K in Seq'Range =>
                      (if J < K then Seq (J) < Seq (K))));

      --------------------------
      -- Lemma_Domain_Unicity --
      --------------------------

      procedure Lemma_Domain_Unicity
        (Map        : Character_Mapping;
         Seq1, Seq2 : Character_Sequence)
      is
         J : Positive := 1;

      begin
         while J <= Seq1'Last
           and then J <= Seq2'Last
           and then Seq1 (J) = Seq2 (J)
         loop
            pragma Loop_Invariant
              (Seq1 (Seq1'First .. J) = Seq2 (Seq2'First .. J));
            pragma Loop_Variant (Increases => J);

            if J = Positive'Last then
               return;
            end if;

            J := J + 1;
         end loop;

         Lemma_Is_Sorted (Seq1);
         Lemma_Is_Sorted (Seq2);

         if J <= Seq1'Last
           and then J <= Seq2'Last
         then
            if Seq1 (J) < Seq2 (J) then
               pragma Assert (for all X of Seq2 => X /= Seq1 (J));
               pragma Assert (Map (Seq1 (J)) = Seq1 (J));
               pragma Assert (False);
            else
               pragma Assert (for all X of Seq1 => X /= Seq2 (J));
               pragma Assert (Map (Seq2 (J)) = Seq2 (J));
               pragma Assert (False);
            end if;

         elsif J <= Seq1'Last then
            pragma Assert (for all X of Seq2 => X /= Seq1 (J));
            pragma Assert (Map (Seq1 (J)) = Seq1 (J));
            pragma Assert (False);

         elsif J <= Seq2'Last then
            pragma Assert (for all X of Seq1 => X /= Seq2 (J));
            pragma Assert (Map (Seq2 (J)) = Seq2 (J));
            pragma Assert (False);
         end if;
      end Lemma_Domain_Unicity;

      ---------------------
      -- Lemma_Is_Domain --
      ---------------------

      procedure Lemma_Is_Domain (Map : Character_Mapping) is
         Ignore : constant Character_Sequence := To_Domain (Map);
      begin
         null;
      end Lemma_Is_Domain;

      ---------------------
      -- Lemma_Is_Sorted --
      ---------------------

      procedure Lemma_Is_Sorted (Seq : Character_Sequence) is
      begin
         for A in Seq'Range loop
            exit when A = Positive'Last;

            for B in A + 1 .. Seq'Last loop
               pragma Loop_Invariant
                 (for all K in A + 1 .. B => Seq (A) < Seq (K));
            end loop;

            pragma Loop_Invariant
              (for all J in Seq'First .. A =>
                 (for all K in Seq'Range =>
                    (if J < K then Seq (J) < Seq (K))));
         end loop;
      end Lemma_Is_Sorted;

      --  Local variables

      Result : String (1 .. Map'Length) with Relaxed_Initialization;
      J      : Natural;

      --  Repeat the computation from To_Domain in ghost code, in order to
      --  prove the relationship between Result and To_Domain(Map).

      Domain : String (1 .. Map'Length) with Ghost, Relaxed_Initialization;
      type Character_Index is array (Character) of Natural with Ghost;
      Indexes : Character_Index := [others => 0] with Ghost;

   --  Start of processing for To_Range

   begin
      J := 0;
      for C in Map'Range loop
         if Map (C) /= C then
            J := J + 1;
            Result (J) := Map (C);
            Domain (J) := C;
            Indexes (C) := J;
         end if;

         --  Repeat the loop invariants from To_Domain regarding Domain and
         --  Indexes. Add similar loop invariants for Result and Indexes.

         pragma Loop_Invariant (J <= Character'Pos (C) + 1);
         pragma Loop_Invariant (Result (1 .. J)'Initialized);
         pragma Loop_Invariant (Domain (1 .. J)'Initialized);
         pragma Loop_Invariant (for all K in 1 .. J => Domain (K) <= C);
         pragma Loop_Invariant
           (SPARK_Proof_Sorted_Character_Sequence (Domain (1 .. J)));
         pragma Loop_Invariant
           (for all D in Map'First .. C =>
              (if Map (D) = D then
                 Indexes (D) = 0
               else
                 Indexes (D) in 1 .. J
                   and then Domain (Indexes (D)) = D
                   and then Result (Indexes (D)) = Map (D)));
         pragma Loop_Invariant
           (for all Char of Domain (1 .. J) => Map (Char) /= Char);
         pragma Loop_Invariant
           (for all K in 1 .. J => Result (K) = Map (Domain (K)));
      end loop;
      pragma Assert (Is_Domain (Map, Domain (1 .. J)));

      --  Show the equality of Domain and To_Domain(Map)

      Lemma_Is_Domain (Map);
      Lemma_Domain_Unicity (Map, Domain (1 .. J), To_Domain (Map));
      pragma Assert
        (for all K in 1 .. J => Domain (K) = To_Domain (Map) (K));
      pragma Assert (To_Domain (Map)'Length = J);

      return Result (1 .. J);
   end To_Range;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges (Set : Character_Set) return Character_Ranges is
      Max_Ranges : Character_Ranges (1 .. Set'Length / 2 + 1)
        with Relaxed_Initialization;
      Range_Num  : Natural;
      C          : Character;
      C_Iter     : Character with Ghost;

   begin
      C := Character'First;
      Range_Num := 0;

      loop
         C_Iter := C;

         --  Skip gap between subsets

         while not Set (C) loop
            pragma Loop_Invariant
              (Character'Pos (C) >= Character'Pos (C'Loop_Entry));
            pragma Loop_Invariant
              (for all Char in C'Loop_Entry .. C => not Set (Char));
            pragma Loop_Variant (Increases => C);
            exit when C = Character'Last;
            C := Character'Succ (C);
         end loop;

         exit when not Set (C);

         Range_Num := Range_Num + 1;
         Max_Ranges (Range_Num).Low := C;

         --  Span a subset

         loop
            pragma Loop_Invariant
              (Character'Pos (C) >= Character'Pos (C'Loop_Entry));
            pragma Loop_Invariant
              (for all Char in C'Loop_Entry .. C =>
                 (if Char /= C then Set (Char)));
            pragma Loop_Variant (Increases => C);
            exit when not Set (C) or else C = Character'Last;
            C := Character'Succ (C);
         end loop;

         if Set (C) then
            Max_Ranges (Range_Num).High := C;
            exit;
         else
            Max_Ranges (Range_Num).High := Character'Pred (C);
         end if;

         pragma Assert
           (for all Char in C_Iter .. C =>
              (Set (Char) =
                 (Char in Max_Ranges (Range_Num).Low ..
                          Max_Ranges (Range_Num).High)));
         pragma Assert
           (for all Char in Character'First .. C_Iter =>
              (if Char /= C_Iter then
                 (Set (Char) =
                    (for some Span of Max_Ranges (1 .. Range_Num - 1) =>
                       Char in Span.Low .. Span.High))));

         pragma Loop_Invariant (2 * Range_Num <= Character'Pos (C) + 1);
         pragma Loop_Invariant (Max_Ranges (1 .. Range_Num)'Initialized);
         pragma Loop_Invariant (not Set (C));
         pragma Loop_Invariant
           (for all Char in Character'First .. C =>
              (Set (Char) =
                 (for some Span of Max_Ranges (1 .. Range_Num) =>
                    Char in Span.Low .. Span.High)));
         pragma Loop_Invariant
           (for all Span of Max_Ranges (1 .. Range_Num) =>
              (for all Char in Span.Low .. Span.High => Set (Char)));
         pragma Loop_Variant (Increases => Range_Num);
      end loop;

      return Max_Ranges (1 .. Range_Num);
   end To_Ranges;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Set : Character_Set) return Character_Sequence is
      Result : String (1 .. Character'Pos (Character'Last) + 1)
        with Relaxed_Initialization;
      Count  : Natural := 0;
   begin
      for Char in Set'Range loop
         if Set (Char) then
            Count := Count + 1;
            Result (Count) := Char;
         end if;

         pragma Loop_Invariant (Count <= Character'Pos (Char) + 1);
         pragma Loop_Invariant (Result (1 .. Count)'Initialized);
         pragma Loop_Invariant (for all K in 1 .. Count => Result (K) <= Char);
         pragma Loop_Invariant
           (SPARK_Proof_Sorted_Character_Sequence (Result (1 .. Count)));
         pragma Loop_Invariant
           (for all C in Set'First .. Char =>
              (Set (C) = (for some X of Result (1 .. Count) => C = X)));
         pragma Loop_Invariant
           (for all Char of Result (1 .. Count) => Is_In (Char, Set));
      end loop;

      return Result (1 .. Count);
   end To_Sequence;

   ------------
   -- To_Set --
   ------------

   function To_Set (Ranges : Character_Ranges) return Character_Set is
      Result : Character_Set := Null_Set;
   begin
      for R in Ranges'Range loop
         for C in Ranges (R).Low .. Ranges (R).High loop
            Result (C) := True;
            pragma Loop_Invariant
              (for all Char in Character =>
                 Result (Char) =
                   ((for some Prev in Ranges'First .. R - 1 =>
                       Char in Ranges (Prev).Low .. Ranges (Prev).High)
                    or else Char in Ranges (R).Low .. C));
         end loop;

         pragma Loop_Invariant
           (for all Char in Character =>
              Result (Char) =
                (for some Prev in Ranges'First .. R =>
                   Char in Ranges (Prev).Low .. Ranges (Prev).High));
      end loop;

      return Result;
   end To_Set;

   function To_Set (Span : Character_Range) return Character_Set is
      Result : Character_Set := Null_Set;
   begin
      for C in Span.Low .. Span.High loop
         Result (C) := True;
         pragma Loop_Invariant
           (for all Char in Character =>
              Result (Char) = (Char in Span.Low .. C));
      end loop;

      return Result;
   end To_Set;

   function To_Set (Sequence : Character_Sequence) return Character_Set is
      Result : Character_Set := Null_Set;
   begin
      for J in Sequence'Range loop
         Result (Sequence (J)) := True;
         pragma Loop_Invariant
           (for all Char in Character =>
              Result (Char) =
                (for some K in Sequence'First .. J => Char = Sequence (K)));
      end loop;

      return Result;
   end To_Set;

   function To_Set (Singleton : Character) return Character_Set is
      Result : Character_Set := Null_Set;
   begin
      Result (Singleton) := True;
      return Result;
   end To_Set;

   -----------
   -- Value --
   -----------

   function Value
     (Map     : Character_Mapping;
      Element : Character) return Character
   is
      (Map (Element));

end Ada.Strings.Maps;
