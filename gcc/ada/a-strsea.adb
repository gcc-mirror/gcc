------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . S T R I N G S . S E A R C H                    --
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

--  Note: This code is derived from the ADAR.CSH public domain Ada 83
--  versions of the Appendix C string handling packages (code extracted
--  from Ada.Strings.Fixed). A significant change is that we optimize the
--  case of identity mappings for Count and Index, and also Index_Non_Blank
--  is specialized (rather than using the general Index routine).

with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Ada.Strings.Search is

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
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      N : Natural;
      J : Natural;

      Mapped_Source : String (Source'Range);

   begin
      for J in Source'Range loop
         Mapped_Source (J) := Value (Mapping, Source (J));
      end loop;

      if Pattern = "" then
         raise Pattern_Error;
      end if;

      N := 0;
      J := Source'First;

      while J <= Source'Last - (Pattern'Length - 1) loop
         if Mapped_Source (J .. J + (Pattern'Length - 1)) = Pattern then
            N := N + 1;
            J := J + Pattern'Length;
         else
            J := J + 1;
         end if;
      end loop;

      return N;
   end Count;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
      Mapped_Source : String (Source'Range);
      N             : Natural;
      J             : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  We make sure Access_Check is unsuppressed so that the Mapping.all
      --  call will generate a friendly Constraint_Error if the value for
      --  Mapping is uninitialized (and hence null).

      declare
         pragma Unsuppress (Access_Check);

      begin
         for J in Source'Range loop
            Mapped_Source (J) := Mapping.all (Source (J));
         end loop;
      end;

      N := 0;
      J := Source'First;

      while J <= Source'Last - (Pattern'Length - 1) loop
         if Mapped_Source (J .. J + (Pattern'Length - 1)) = Pattern then
            N := N + 1;
            J := J + Pattern'Length;
         else
            J := J + 1;
         end if;
      end loop;

      return N;
   end Count;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
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
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      Cur_Index     : Natural;
      Mapped_Source : String (Source'Range);

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      for J in Source'Range loop
         Mapped_Source (J) := Value (Mapping, Source (J));
      end loop;

      --  Forwards case

      if Going = Forward then
         for J in 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;

            if Pattern = Mapped_Source
                           (Cur_Index .. Cur_Index + Pattern'Length - 1)
            then
               return Cur_Index;
            end if;
         end loop;

      --  Backwards case

      else
         for J in reverse 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;

            if Pattern = Mapped_Source
                           (Cur_Index .. Cur_Index + Pattern'Length - 1)
            then
               return Cur_Index;
            end if;
         end loop;
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
      Mapped_Source : String (Source'Range);
      Cur_Index     : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  We make sure Access_Check is unsuppressed so that the Mapping.all
      --  call will generate a friendly Constraint_Error if the value for
      --  Mapping is uninitialized (and hence null).

      declare
         pragma Unsuppress (Access_Check);
      begin
         for J in Source'Range loop
            Mapped_Source (J) := Mapping.all (Source (J));
         end loop;
      end;

      --  Forwards case

      if Going = Forward then
         for J in 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;

            if Pattern = Mapped_Source
                           (Cur_Index .. Cur_Index + Pattern'Length - 1)
            then
               return Cur_Index;
            end if;
         end loop;

      --  Backwards case

      else
         for J in reverse 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;

            if Pattern = Mapped_Source
                           (Cur_Index .. Cur_Index + Pattern'Length - 1)
            then
               return Cur_Index;
            end if;
         end loop;
      end if;

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
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
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
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
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
     (Source  : String;
      Set     : Maps.Character_Set;
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
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
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

end Ada.Strings.Search;
