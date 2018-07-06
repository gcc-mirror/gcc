------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . S T R I N G S . M A P S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

package body Ada.Strings.Maps is

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
   begin
      return Set (Element);
   end Is_In;

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

   function To_Domain (Map : Character_Mapping) return Character_Sequence
   is
      Result : String (1 .. Map'Length);
      J      : Natural;

   begin
      J := 0;
      for C in Map'Range loop
         if Map (C) /= C then
            J := J + 1;
            Result (J) := C;
         end if;
      end loop;

      return Result (1 .. J);
   end To_Domain;

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping
     (From, To : Character_Sequence) return Character_Mapping
   is
      Result   : Character_Mapping;
      Inserted : Character_Set := Null_Set;
      From_Len : constant Natural := From'Length;
      To_Len   : constant Natural := To'Length;

   begin
      if From_Len /= To_Len then
         raise Strings.Translation_Error;
      end if;

      for Char in Character loop
         Result (Char) := Char;
      end loop;

      for J in From'Range loop
         if Inserted (From (J)) then
            raise Strings.Translation_Error;
         end if;

         Result   (From (J)) := To (J - From'First + To'First);
         Inserted (From (J)) := True;
      end loop;

      return Result;
   end To_Mapping;

   --------------
   -- To_Range --
   --------------

   function To_Range (Map : Character_Mapping) return Character_Sequence
   is
      Result : String (1 .. Map'Length);
      J      : Natural;
   begin
      J := 0;
      for C in Map'Range loop
         if Map (C) /= C then
            J := J + 1;
            Result (J) := Map (C);
         end if;
      end loop;

      return Result (1 .. J);
   end To_Range;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges (Set : Character_Set) return Character_Ranges is
      Max_Ranges : Character_Ranges (1 .. Set'Length / 2 + 1);
      Range_Num  : Natural;
      C          : Character;

   begin
      C := Character'First;
      Range_Num := 0;

      loop
         --  Skip gap between subsets

         while not Set (C) loop
            exit when C = Character'Last;
            C := Character'Succ (C);
         end loop;

         exit when not Set (C);

         Range_Num := Range_Num + 1;
         Max_Ranges (Range_Num).Low := C;

         --  Span a subset

         loop
            exit when not Set (C) or else C = Character'Last;
            C := Character'Succ (C);
         end loop;

         if Set (C) then
            Max_Ranges (Range_Num). High := C;
            exit;
         else
            Max_Ranges (Range_Num). High := Character'Pred (C);
         end if;
      end loop;

      return Max_Ranges (1 .. Range_Num);
   end To_Ranges;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Set : Character_Set) return Character_Sequence is
      Result : String (1 .. Character'Pos (Character'Last) + 1);
      Count  : Natural := 0;
   begin
      for Char in Set'Range loop
         if Set (Char) then
            Count := Count + 1;
            Result (Count) := Char;
         end if;
      end loop;

      return Result (1 .. Count);
   end To_Sequence;

   ------------
   -- To_Set --
   ------------

   function To_Set (Ranges : Character_Ranges) return Character_Set is
      Result : Character_Set;
   begin
      for C in Result'Range loop
         Result (C) := False;
      end loop;

      for R in Ranges'Range loop
         for C in Ranges (R).Low .. Ranges (R).High loop
            Result (C) := True;
         end loop;
      end loop;

      return Result;
   end To_Set;

   function To_Set (Span : Character_Range) return Character_Set is
      Result : Character_Set;
   begin
      for C in Result'Range loop
         Result (C) := False;
      end loop;

      for C in Span.Low .. Span.High loop
         Result (C) := True;
      end loop;

      return Result;
   end To_Set;

   function To_Set (Sequence : Character_Sequence) return Character_Set is
      Result : Character_Set := Null_Set;
   begin
      for J in Sequence'Range loop
         Result (Sequence (J)) := True;
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
   begin
      return Map (Element);
   end Value;

end Ada.Strings.Maps;
