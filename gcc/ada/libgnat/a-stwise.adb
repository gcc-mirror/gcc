------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . S T R I N G S . W I D E _ S E A R C H               --
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

with Ada.Strings.Wide_Maps; use Ada.Strings.Wide_Maps;
with System;                use System;

package body Ada.Strings.Wide_Search is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Belongs
     (Element : Wide_Character;
      Set     : Wide_Maps.Wide_Character_Set;
      Test    : Membership) return Boolean;
   pragma Inline (Belongs);
   --  Determines if the given element is in (Test = Inside) or not in
   --  (Test = Outside) the given character set.

   -------------
   -- Belongs --
   -------------

   function Belongs
     (Element : Wide_Character;
      Set     : Wide_Maps.Wide_Character_Set;
      Test    : Membership) return Boolean
   is
   begin
      if Test = Inside then
         return Is_In (Element, Set);
      else
         return not Is_In (Element, Set);
      end if;
   end Belongs;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;
      Cur : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      Num := 0;
      Ind := Source'First;

      --  Unmapped case

      if Mapping'Address = Wide_Maps.Identity'Address then
         while Ind <= Source'Last - PL1 loop
            if Pattern = Source (Ind .. Ind + PL1) then
               Num := Num + 1;
               Ind := Ind + Pattern'Length;
            else
               Ind := Ind + 1;
            end if;
         end loop;

      --  Mapped case

      else
         while Ind <= Source'Last - PL1 loop
            Cur := Ind;
            for K in Pattern'Range loop
               if Pattern (K) /= Value (Mapping, Source (Cur)) then
                  Ind := Ind + 1;
                  goto Cont;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            Num := Num + 1;
            Ind := Ind + Pattern'Length;

         <<Cont>>
            null;
         end loop;
      end if;

      --  Return result

      return Num;
   end Count;

   function Count
     (Source  : Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;
      Cur : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Check for null pointer in case checks are off

      if Mapping = null then
         raise Constraint_Error;
      end if;

      Num := 0;
      Ind := Source'First;
      while Ind <= Source'Last - PL1 loop
         Cur := Ind;
         for K in Pattern'Range loop
            if Pattern (K) /= Mapping (Source (Cur)) then
               Ind := Ind + 1;
               goto Cont;
            else
               Cur := Cur + 1;
            end if;
         end loop;

         Num := Num + 1;
         Ind := Ind + Pattern'Length;

      <<Cont>>
         null;
      end loop;

      return Num;
   end Count;

   function Count
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set) return Natural
   is
      N : Natural := 0;

   begin
      for J in Source'Range loop
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
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      for J in From .. Source'Last loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            for K in J + 1 .. Source'Last loop
               if not Belongs (Source (K), Set, Test) then
                  Last := K - 1;
                  return;
               end if;
            end loop;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;
      end loop;

      --  Here if no token found

      First := From;
      Last  := 0;
   end Find_Token;

   procedure Find_Token
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      for J in Source'Range loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            for K in J + 1 .. Source'Last loop
               if not Belongs (Source (K), Set, Test) then
                  Last := K - 1;
                  return;
               end if;
            end loop;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;
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
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Cur : Natural;

      Ind : Integer;
      --  Index for start of match check. This can be negative if the pattern
      --  length is greater than the string length, which is why this variable
      --  is Integer instead of Natural. In this case, the search loops do not
      --  execute at all, so this Ind value is never used.

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Forwards case

      if Going = Forward then
         Ind := Source'First;

         --  Unmapped forward case

         if Mapping'Address = Wide_Maps.Identity'Address then
            for J in 1 .. Source'Length - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  return Ind;
               else
                  Ind := Ind + 1;
               end if;
            end loop;

         --  Mapped forward case

         else
            for J in 1 .. Source'Length - PL1 loop
               Cur := Ind;

               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping, Source (Cur)) then
                     goto Cont1;
                  else
                     Cur := Cur + 1;
                  end if;
               end loop;

               return Ind;

            <<Cont1>>
               Ind := Ind + 1;
            end loop;
         end if;

      --  Backwards case

      else
         --  Unmapped backward case

         Ind := Source'Last - PL1;

         if Mapping'Address = Wide_Maps.Identity'Address then
            for J in reverse 1 .. Source'Length - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  return Ind;
               else
                  Ind := Ind - 1;
               end if;
            end loop;

         --  Mapped backward case

         else
            for J in reverse 1 .. Source'Length - PL1 loop
               Cur := Ind;

               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping, Source (Cur)) then
                     goto Cont2;
                  else
                     Cur := Cur + 1;
                  end if;
               end loop;

               return Ind;

            <<Cont2>>
               Ind := Ind - 1;
            end loop;
         end if;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Ind : Natural;
      Cur : Natural;

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
         Ind := Source'First;
         for J in 1 .. Source'Length - PL1 loop
            Cur := Ind;

            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all (Source (Cur)) then
                  goto Cont1;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            return Ind;

         <<Cont1>>
            Ind := Ind + 1;
         end loop;

      --  Backwards case

      else
         Ind := Source'Last - PL1;
         for J in reverse 1 .. Source'Length - PL1 loop
            Cur := Ind;

            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all (Source (Cur)) then
                  goto Cont2;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            return Ind;

         <<Cont2>>
            Ind := Ind - 1;
         end loop;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
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
         end loop;

      --  Backwards case

      else
         for J in reverse Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index;

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index (Source (From .. Source'Last), Pattern, Forward, Mapping);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index (Source (Source'First .. From), Pattern, Backward, Mapping);
      end if;
   end Index;

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
   begin
      if Going = Forward then
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
     (Source  : Wide_String;
      Set     : Wide_Maps.Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
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
     (Source : Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= Wide_Space then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= Wide_Space then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
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

end Ada.Strings.Wide_Search;
