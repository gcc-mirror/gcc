------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S T R I N G S . W I D E _ W I D E _ S E A R C H          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;

package body Ada.Strings.Wide_Wide_Search is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Belongs
     (Element : Wide_Wide_Character;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test    : Membership) return Boolean;
   pragma Inline (Belongs);
   --  Determines if the given element is in (Test = Inside) or not in
   --  (Test = Outside) the given character set.

   -------------
   -- Belongs --
   -------------

   function Belongs
     (Element : Wide_Wide_Character;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural
   is
      N : Natural;
      J : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Handle the case of non-identity mappings by creating a mapped
      --  string and making a recursive call using the identity mapping
      --  on this mapped string.

      if Mapping /= Wide_Wide_Maps.Identity then
         declare
            Mapped_Source : Wide_Wide_String (Source'Range);

         begin
            for J in Source'Range loop
               Mapped_Source (J) := Value (Mapping, Source (J));
            end loop;

            return Count (Mapped_Source, Pattern);
         end;
      end if;

      N := 0;
      J := Source'First;

      while J <= Source'Last - (Pattern'Length - 1) loop
         if Source (J .. J + (Pattern'Length - 1)) = Pattern then
            N := N + 1;
            J := J + Pattern'Length;
         else
            J := J + 1;
         end if;
      end loop;

      return N;
   end Count;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
   is
      Mapped_Source : Wide_Wide_String (Source'Range);

   begin
      for J in Source'Range loop
         Mapped_Source (J) := Mapping (Source (J));
      end loop;

      return Count (Mapped_Source, Pattern);
   end Count;

   function Count
     (Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural
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
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
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

            --  Here if J indexes 1st char of token, and all chars
            --  after J are in the token

            Last := Source'Last;
            return;
         end if;
      end loop;

      --  Here if no token found

      First := Source'First;
      Last  := 0;
   end Find_Token;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural
   is
   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Handle the case of non-identity mappings by creating a mapped
      --  string and making a recursive call using the identity mapping
      --  on this mapped string.

      if Mapping /= Identity then
         declare
            Mapped_Source : Wide_Wide_String (Source'Range);

         begin
            for J in Source'Range loop
               Mapped_Source (J) := Value (Mapping, Source (J));
            end loop;

            return Index (Mapped_Source, Pattern, Going);
         end;
      end if;

      if Going = Forward then
         for J in Source'First .. Source'Last - Pattern'Length + 1 loop
            if Pattern = Source (J .. J + Pattern'Length - 1) then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'First .. Source'Last - Pattern'Length + 1 loop
            if Pattern = Source (J .. J + Pattern'Length - 1) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
   is
      Mapped_Source : Wide_Wide_String (Source'Range);

   begin
      for J in Source'Range loop
         Mapped_Source (J) := Mapping (Source (J));
      end loop;

      return Index (Mapped_Source, Pattern, Going);
   end Index;

   function Index
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;

      else -- Going = Backward
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
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
     (Source  : Wide_Wide_String;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
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
     (Source : Wide_Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= Wide_Wide_Space then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= Wide_Wide_Space then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : Wide_Wide_String;
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

end Ada.Strings.Wide_Wide_Search;
