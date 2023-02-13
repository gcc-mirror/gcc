------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . S T R I N G S . S E A R C H                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  Note: This code is derived from the ADAR.CSH public domain Ada 83
--  versions of the Appendix C string handling packages (code extracted
--  from Ada.Strings.Fixed). A significant change is that we optimize the
--  case of identity mappings for Count and Index, and also Index_Non_Blank
--  is specialized (rather than using the general Index routine).

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Strings.Maps; use Ada.Strings.Maps;
with System;           use System;

package body Ada.Strings.Search with SPARK_Mode is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean;
   pragma Inline (Belongs);
   --  Determines if the given element is in (Test = Inside) or not in
   --  (Test = Outside) the given character set.

   -------------
   -- Belongs --
   -------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean
   is
      (if Test = Inside then
          Is_In (Element, Set)
      else not (Is_In (Element, Set)));

   -----------
   -- Count --
   -----------

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Isolating the null string case to ensure Source'First, Source'Last in
      --  Positive.

      if Source = "" then
         return 0;
      end if;

      Num := 0;
      Ind := Source'First - 1;

      --  Unmapped case

      if Is_Identity (Mapping) then
         while Ind < Source'Last - PL1 loop
            Ind := Ind + 1;
            if Pattern = Source (Ind .. Ind + PL1) then
               Num := Num + 1;
               Ind := Ind + PL1;
            end if;

            pragma Loop_Invariant (Num <= Ind - (Source'First - 1));
            pragma Loop_Invariant (Ind >= Source'First);
            pragma Loop_Variant (Increases => Ind);
         end loop;

      --  Mapped case

      else
         while Ind < Source'Last - PL1 loop
            Ind := Ind + 1;
            for K in Pattern'Range loop
               if Pattern (K) /= Value (Mapping,
                 Source (Ind + (K - Pattern'First)))
               then
                  pragma Assert (not (Match (Source, Pattern, Mapping, Ind)));
                  goto Cont;
               end if;

               pragma Loop_Invariant
                 (for all J in Pattern'First .. K =>
                    Pattern (J) = Value (Mapping,
                      Source (Ind + (J - Pattern'First))));
            end loop;

            pragma Assert (Match (Source, Pattern, Mapping, Ind));
            Num := Num + 1;
            Ind := Ind + PL1;

            <<Cont>>
            null;
            pragma Loop_Invariant (Num <= Ind - (Source'First - 1));
            pragma Loop_Invariant (Ind >= Source'First);
            pragma Loop_Variant (Increases => Ind);
         end loop;
      end if;

      --  Return result

      return Num;
   end Count;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Isolating the null string case to ensure Source'First, Source'Last in
      --  Positive.

      if Source = "" then
         return 0;
      end if;

      --  Check for null pointer in case checks are off

      if Mapping = null then
         raise Constraint_Error;
      end if;

      Num := 0;
      Ind := Source'First - 1;
      while Ind < Source'Last - PL1 loop
         Ind := Ind + 1;
         for K in Pattern'Range loop
            if Pattern (K) /= Mapping (Source (Ind + (K - Pattern'First))) then
               pragma Assert (not (Match (Source, Pattern, Mapping, Ind)));
               goto Cont;
            end if;

            pragma Loop_Invariant
              (for all J in Pattern'First .. K =>
                 Pattern (J) = Mapping (Source (Ind + (J - Pattern'First))));
         end loop;

         pragma Assert (Match (Source, Pattern, Mapping, Ind));
         Num := Num + 1;
         Ind := Ind + PL1;

      <<Cont>>
         null;
         pragma Loop_Invariant (Num <= Ind - (Source'First - 1));
         pragma Loop_Invariant (Ind >= Source'First);
         pragma Loop_Variant (Increases => Ind);
      end loop;

      return Num;
   end Count;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   is
      N : Natural := 0;

   begin
      for J in Source'Range loop
         pragma Loop_Invariant (N <= J - Source'First);
         if Is_In (Source (J), Set) then
            N := N + 1;
         end if;
      end loop;

      return N;
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      --  AI05-031: Raise Index error if Source non-empty and From not in range

      if Source'Length /= 0 and then From not in Source'Range then
         raise Index_Error;
      end if;

      --  If Source is the empty string, From may still be out of its
      --  range.  The following ensures that in all cases there is no
      --  possible erroneous access to a non-existing character.

      for J in Integer'Max (From, Source'First) .. Source'Last loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            if J < Source'Last then
               for K in J + 1 .. Source'Last loop
                  if not Belongs (Source (K), Set, Test) then
                     Last := K - 1;
                     return;
                  end if;

                  pragma Loop_Invariant
                    (for all L in J .. K =>
                       Belongs (Source (L), Set, Test));
               end loop;
            end if;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;

         pragma Loop_Invariant
           (for all K in Integer'Max (From, Source'First) .. J =>
                not (Belongs (Source (K), Set, Test)));
      end loop;

      --  Here if no token found

      First := From;
      Last  := 0;
   end Find_Token;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      for J in Source'Range loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            if J < Source'Last then
               for K in J + 1 .. Source'Last loop
                  if not Belongs (Source (K), Set, Test) then
                     Last := K - 1;
                     return;
                  end if;

                  pragma Loop_Invariant
                    (for all L in J .. K =>
                       Belongs (Source (L), Set, Test));
               end loop;
            end if;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;

         pragma Loop_Invariant
           (for all K in Source'First .. J =>
              not (Belongs (Source (K), Set, Test)));
      end loop;

      --  Here if no token found

      --  RM 2005 A.4.3 (68/1) specifies that an exception must be raised if
      --  Source'First is not positive and is assigned to First. Formulation
      --  is slightly different in RM 2012, but the intent seems similar, so
      --  we check explicitly for that condition.

      if Source'First not in Positive then
         raise Constraint_Error;

      else
         First := Source'First;
         Last  := 0;
      end if;
   end Find_Token;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  If Pattern is longer than Source, it can't be found

      if Pattern'Length > Source'Length then
         return 0;
      end if;

      --  Forwards case

      if Going = Forward then

         --  Unmapped forward case

         if Is_Identity (Mapping) then
            for Ind in Source'First .. Source'Last - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  pragma Assert (Match (Source, Pattern, Mapping, Ind));
                  return Ind;
               end if;

               pragma Loop_Invariant
                 (for all J in Source'First .. Ind =>
                    not (Match (Source, Pattern, Mapping, J)));
            end loop;

         --  Mapped forward case

         else
            for Ind in Source'First .. Source'Last - PL1 loop
               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping,
                    Source (Ind + (K - Pattern'First)))
                  then
                     goto Cont1;
                  end if;

                  pragma Loop_Invariant
                    (for all J in Pattern'First .. K =>
                       Pattern (J) = Value (Mapping,
                         Source (Ind + (J - Pattern'First))));
               end loop;

               pragma Assert (Match (Source, Pattern, Mapping, Ind));
               return Ind;

               <<Cont1>>
               pragma Loop_Invariant
                 (for all J in Source'First .. Ind =>
                    not (Match (Source, Pattern, Mapping, J)));
               null;
            end loop;
         end if;

      --  Backwards case

      else
         --  Unmapped backward case

         if Is_Identity (Mapping) then
            for Ind in reverse Source'First .. Source'Last - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  pragma Assert (Match (Source, Pattern, Mapping, Ind));
                  return Ind;
               end if;

               pragma Loop_Invariant
                 (for all J in Ind .. Source'Last - PL1 =>
                    not (Match (Source, Pattern, Mapping, J)));
            end loop;

         --  Mapped backward case

         else
            for Ind in reverse Source'First .. Source'Last - PL1 loop
               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping,
                    Source (Ind + (K - Pattern'First)))
                  then
                     goto Cont2;
                  end if;

                  pragma Loop_Invariant
                    (for all J in Pattern'First .. K =>
                       Pattern (J) = Value (Mapping,
                         Source (Ind + (J - Pattern'First))));
               end loop;

               pragma Assert (Match (Source, Pattern, Mapping, Ind));
               return Ind;

               <<Cont2>>
               pragma Loop_Invariant
                 (for all J in Ind .. Source'Last - PL1 =>
                    not (Match (Source, Pattern, Mapping, J)));
               null;
            end loop;
         end if;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Check for null pointer in case checks are off

      if Mapping = null then
         raise Constraint_Error;
      end if;

      --  If Pattern longer than Source it can't be found

      if Pattern'Length > Source'Length then
         return 0;
      end if;

      --  Forwards case

      if Going = Forward then
         for Ind in Source'First .. Source'Last - PL1 loop
            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all
                 (Source (Ind + (K - Pattern'First)))
               then
                  goto Cont1;
               end if;

               pragma Loop_Invariant
                 (for all J in Pattern'First .. K =>
                   Pattern (J) = Mapping (Source (Ind + (J - Pattern'First))));
            end loop;

            pragma Assert (Match (Source, Pattern, Mapping, Ind));
            return Ind;

            <<Cont1>>
            pragma Loop_Invariant
              (for all J in Source'First .. Ind =>
                 not (Match (Source, Pattern, Mapping, J)));
            null;
         end loop;

      --  Backwards case

      else
         for Ind in reverse Source'First .. Source'Last - PL1 loop
            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all
                 (Source (Ind + (K - Pattern'First)))
               then
                  goto Cont2;
               end if;

               pragma Loop_Invariant
                 (for all J in Pattern'First .. K =>
                   Pattern (J) = Mapping (Source (Ind + (J - Pattern'First))));
            end loop;

            return Ind;

            <<Cont2>>
            pragma Loop_Invariant
              (for all J in Ind .. (Source'Last - PL1) =>
                not (Match (Source, Pattern, Mapping, J)));
            null;
         end loop;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   is
   begin
      --  Forwards case

      if Going = Forward then
         for J in Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;

            pragma Loop_Invariant
              (for all C of Source (Source'First .. J) =>
                   not (Belongs (C, Set, Test)));
         end loop;

      --  Backwards case

      else
         for J in reverse Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;

            pragma Loop_Invariant
              (for all C of Source (J .. Source'Last) =>
                   not (Belongs (C, Set, Test)));
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      Result : Natural;
      PL1    : constant Integer := Pattern'Length - 1;
   begin

      --  AI05-056: If source is empty result is always zero

      if Source'Length = 0 then
         return 0;

      elsif Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         Result :=
           Index (Source (From .. Source'Last), Pattern, Forward, Mapping);
         pragma Assert
           (if (for some J in From .. Source'Last - PL1 =>
                 Match (Source, Pattern, Mapping, J))
            then Result in From .. Source'Last - PL1
            else Result = 0);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         Result :=
           Index (Source (Source'First .. From), Pattern, Backward, Mapping);
         pragma Assert
           (if (for some J in Source'First .. From - PL1 =>
                  Match (Source, Pattern, Mapping, J))
            then Result in Source'First .. From - PL1
            else Result = 0);
      end if;

      return Result;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin

      --  AI05-056: If source is empty result is always zero

      if Source'Length = 0 then
         return 0;

      elsif Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return Index
           (Source (From .. Source'Last), Pattern, Forward, Mapping);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return Index
           (Source (Source'First .. From), Pattern, Backward, Mapping);
      end if;
   end Index;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
   begin

      --  AI05-056 : if source is empty result is always 0.

      if Source'Length = 0 then
         return 0;

      elsif Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index (Source (From .. Source'Last), Set, Test, Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index (Source (Source'First .. From), Set, Test, Backward);
      end if;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;

            pragma Loop_Invariant
              (for all C of Source (Source'First .. J) => C = ' ');
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;

            pragma Loop_Invariant
              (for all C of Source (J .. Source'Last) => C = ' ');
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin

      --  For equivalence with Index, if Source is empty the result is 0

      if Source'Length = 0 then
         return 0;
      end if;

      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (From .. Source'Last), Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (Source'First .. From), Backward);
      end if;
   end Index_Non_Blank;

   function Is_Identity
     (Mapping : Maps.Character_Mapping) return Boolean
   with SPARK_Mode => Off
   is
   begin
      return Mapping'Address = Maps.Identity'Address;
   end Is_Identity;

end Ada.Strings.Search;
