------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             N A M E T . S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008-2021, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.WCh_Cnv; use System.WCh_Cnv;

with GNAT.UTF_32_Spelling_Checker;

package body Namet.Sp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Get_Name_String_UTF_32
     (Id     : Name_Id;
      Result : out UTF_32_String;
      Length : out Natural);
   --  This procedure is similar to Get_Decoded_Name except that the output
   --  is stored in the given Result array as single codes, so in particular
   --  any Uhh, Whhhh, or WWhhhhhhhh sequences are decoded to appear as a
   --  single value in the output. This call does not affect the contents of
   --  either Name_Buffer or Name_Len. The result is in Result (1 .. Length).
   --  The caller must ensure that the result buffer is long enough.

   ----------------------------
   -- Get_Name_String_UTF_32 --
   ----------------------------

   procedure Get_Name_String_UTF_32
     (Id     : Name_Id;
      Result : out UTF_32_String;
      Length : out Natural)
   is
      pragma Assert (Result'First = 1);

      SPtr : Int := Name_Entries.Table (Id).Name_Chars_Index + 1;
      --  Index through characters of name in Name_Chars table. Initial value
      --  points to first character of the name.

      SLen : constant Nat := Nat (Name_Entries.Table (Id).Name_Len);
      --  Length of the name

      SLast : constant Int := SPtr + SLen - 1;
      --  Last index in Name_Chars table for name

      C : Character;
      --  Current character from Name_Chars table

      procedure Store_Hex (N : Natural);
      --  Read and store next N characters starting at SPtr and store result
      --  in next character of Result. Update SPtr past characters read.

      ---------------
      -- Store_Hex --
      ---------------

      procedure Store_Hex (N : Natural) is
         T : UTF_32_Code;
         C : Character;

      begin
         T := 0;
         for J in 1 .. N loop
            C := Name_Chars.Table (SPtr);
            SPtr := SPtr + 1;

            if C in '0' .. '9' then
               T := 16 * T + Character'Pos (C) - Character'Pos ('0');
            else
               pragma Assert (C in 'a' .. 'f');

               T := 16 * T + Character'Pos (C) - (Character'Pos ('a') - 10);
            end if;
         end loop;

         Length := Length + 1;
         pragma Assert (Length <= Result'Length);
         Result (Length) := T;
      end Store_Hex;

   --  Start of processing for Get_Name_String_UTF_32

   begin
      Length := 0;
      while SPtr <= SLast loop
         C := Name_Chars.Table (SPtr);

         --  Uhh encoding

         if C = 'U'
           and then SPtr <= SLast - 2
           and then Name_Chars.Table (SPtr + 1) not in 'A' .. 'Z'
         then
            SPtr := SPtr + 1;
            Store_Hex (2);

         --  Whhhh encoding

         elsif C = 'W'
           and then SPtr <= SLast - 4
           and then Name_Chars.Table (SPtr + 1) not in 'A' .. 'Z'
         then
            SPtr := SPtr + 1;
            Store_Hex (4);

         --  WWhhhhhhhh encoding

         elsif C = 'W'
           and then SPtr <= SLast - 8
           and then Name_Chars.Table (SPtr + 1) = 'W'
         then
            SPtr := SPtr + 2;
            Store_Hex (8);

         --  Q encoding (character literal)

         elsif C = 'Q' and then SPtr < SLast then

            --  Put apostrophes around character

            pragma Assert (Length <= Result'Last - 3);
            Result (Length + 1) := UTF_32_Code'Val (Character'Pos ('''));
            Result (Length + 2) :=
              UTF_32_Code (Get_Char_Code (Name_Chars.Table (SPtr + 1)));
            Result (Length + 3) := UTF_32_Code'Val (Character'Pos ('''));
            SPtr := SPtr + 2;
            Length := Length + 3;

         --  Unencoded case

         else
            SPtr := SPtr + 1;
            Length := Length + 1;
            pragma Assert (Length <= Result'Last);
            Result (Length) := UTF_32_Code (Get_Char_Code (C));
         end if;
      end loop;
   end Get_Name_String_UTF_32;

   ------------------------
   -- Is_Bad_Spelling_Of --
   ------------------------

   function Is_Bad_Spelling_Of (Found, Expect : Name_Id) return Boolean is
      FL : constant Natural := Natural (Length_Of_Name (Found));
      EL : constant Natural := Natural (Length_Of_Name (Expect));
      --  Length of input names

      FB : UTF_32_String (1 .. 2 * FL);
      EB : UTF_32_String (1 .. 2 * EL);
      --  Buffers for results, a factor of 2 is more than enough, the only
      --  sequence which expands is Q (character literal) by 1.5 times.

      FBL : Natural;
      EBL : Natural;
      --  Length of decoded names

   begin
      Get_Name_String_UTF_32 (Found, FB, FBL);
      Get_Name_String_UTF_32 (Expect, EB, EBL);

      --  For an exact match, return False, otherwise check bad spelling. We
      --  need this special test because the library routine returns True for
      --  an exact match.

      if FB (1 .. FBL) = EB (1 .. EBL) then
         return False;
      else
         return
           GNAT.UTF_32_Spelling_Checker.Is_Bad_Spelling_Of
             (FB (1 .. FBL), EB (1 .. EBL));
      end if;
   end Is_Bad_Spelling_Of;

end Namet.Sp;
