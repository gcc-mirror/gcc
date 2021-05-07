------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       ADA.STRINGS.TEXT_OUTPUT.UTILS                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Ada.Strings.Text_Output.Utils is

   procedure Put_Octet (S : in out Sink'Class; Item : Character) with Inline;
   --  Send a single octet to the current Chunk

   procedure Adjust_Column (S : in out Sink'Class) with Inline;
   --  Adjust the column for a non-NL character.

   procedure Put_UTF_8_Outline (S : in out Sink'Class; Item : UTF_8);
   --  Out-of-line portion of Put_UTF_8. This exists solely to make Put_UTF_8
   --  small enough to reasonably inline it.

   procedure Full (S : in out Sink'Class) is
   begin
      pragma Assert (S.Last = S.Chunk_Length);
      Full_Method (S);
      pragma Assert (S.Last = 0);
   end Full;

   procedure Flush (S : in out Sink'Class) is
   begin
      Flush_Method (S);
   end Flush;

   procedure Put_Octet (S : in out Sink'Class; Item : Character) is
   begin
      S.Last := S.Last + 1;
      S.Cur_Chunk.Chars (S.Last) := Item;
      pragma Assert (S.Chunk_Length = S.Cur_Chunk.Chars'Length);
      if S.Last = S.Chunk_Length then
         Full (S);
      end if;
   end Put_Octet;

   procedure Adjust_Column (S : in out Sink'Class) is
   begin
      --  If we're in the first column, indent. This is handled here, rather
      --  than when we see NL, because we don't want spaces in a blank line.
      --  The character we're about to put is not NL; NL is handled in
      --  New_Line. So after indenting, we simply increment the Column.

      if S.Column = 1 then
         Tab_To_Column (S, S.Indentation + 1);
      end if;
      S.Column := S.Column + 1;
   end Adjust_Column;

   procedure Put_7bit (S : in out Sink'Class; Item : Character_7) is
   begin
      Adjust_Column (S);
      Put_Octet (S, Item);
   end Put_7bit;

   procedure Put_7bit_NL (S : in out Sink'Class; Item : Character_7) is
   begin
      if Item = NL then
         New_Line (S);
      else
         Put_7bit (S, Item);
      end if;
   end Put_7bit_NL;

   procedure Put_Character (S : in out Sink'Class; Item : Character) is
   begin
      if Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, Item);
      else
         Put_Wide_Wide_Character (S, To_Wide_Wide (Item));
      end if;
   end Put_Character;

   procedure Put_Wide_Character
     (S : in out Sink'Class; Item : Wide_Character) is
   begin
      if Wide_Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, From_Wide (Item));
      else
         Put_Wide_Wide_Character (S, To_Wide_Wide (Item));
      end if;
   end Put_Wide_Character;

   procedure Put_Wide_Wide_Character
     (S : in out Sink'Class; Item : Wide_Wide_Character) is
   begin
      if Wide_Wide_Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, From_Wide_Wide (Item));
      else
         S.All_7_Bits := False;
         if Wide_Wide_Character'Pos (Item) >= 2**8 then
            S.All_8_Bits := False;
         end if;
         declare
            Temp : constant UTF_8_Lines :=
              UTF_Encoding.Wide_Wide_Strings.Encode ((1 => Item));
         begin
            for X of Temp loop
               pragma Assert (X /= NL);
               Adjust_Column (S);
               Put_Octet (S, X);
            end loop;
         end;
      end if;
   end Put_Wide_Wide_Character;

   procedure Put_UTF_8_Outline (S : in out Sink'Class; Item : UTF_8) is
   begin
      if S.Last + Item'Length = S.Chunk_Length then
         --  Item fits exactly in current chunk

         S.Cur_Chunk.Chars (S.Last + 1 .. S.Last + Item'Length) := Item;
         S.Last := S.Last + Item'Length;
         S.Column := S.Column + Item'Length;
         Full (S);
         --  ???Seems like maybe we shouldn't call Full until we have MORE
         --  characters. But then we can't pass Chunk_Length => 1 to
         --  Create_File to get unbuffered output.
      else
         --  We get here only if Item doesn't fit in the current chunk, which
         --  should be fairly rare. We split Item into Left and Right, where
         --  Left exactly fills the current chunk, and recurse on Left and
         --  Right. Right will fit into the next chunk unless it's very long,
         --  so another level of recursion will be extremely rare.

         declare
            Left_Length : constant Natural := S.Chunk_Length - S.Last;
            Right_First : constant Natural := Item'First + Left_Length;
            Left : UTF_8 renames Item (Item'First .. Right_First - 1);
            Right : UTF_8 renames Item (Right_First .. Item'Last);
            pragma Assert (Left & Right = Item);
         begin
            Put_UTF_8 (S, Left); -- This will call Full.
            Put_UTF_8 (S, Right); -- This might call Full, but probably not.
         end;
      end if;
   end Put_UTF_8_Outline;

   procedure Put_UTF_8 (S : in out Sink'Class; Item : UTF_8) is
   begin
      Adjust_Column (S);

      if S.Last + Item'Length < S.Chunk_Length then
         --  Item fits in current chunk

         S.Cur_Chunk.Chars (S.Last + 1 .. S.Last + Item'Length) := Item;
         S.Last := S.Last + Item'Length;
         S.Column := S.Column + Item'Length;
      else
         Put_UTF_8_Outline (S, Item);
      end if;
   end Put_UTF_8;

   procedure Put_UTF_8_Lines (S : in out Sink'Class; Item : UTF_8_Lines) is
      Line_Start, Index : Integer := Item'First;
      --  Needs to be Integer, because Item'First might be negative for empty
      --  Items.
   begin
      while Index <= Item'Last loop
         if Item (Index) = NL then
            if Index > Line_Start then
               Put_UTF_8 (S, Item (Line_Start .. Index - 1));
            end if;
            New_Line (S);
            Line_Start := Index + 1;
         end if;

         Index := Index + 1;
      end loop;

      if Index > Line_Start then
         Put_UTF_8 (S, Item (Line_Start .. Index - 1));
      end if;
   end Put_UTF_8_Lines;

   procedure Put_String (S : in out Sink'Class; Item : String) is
   begin
      for X of Item loop
         Put_Character (S, X);
      end loop;
   end Put_String;

   procedure Put_Wide_String (S : in out Sink'Class; Item : Wide_String) is
   begin
      for X of Item loop
         Put_Wide_Character (S, X);
      end loop;
   end Put_Wide_String;

   procedure Put_Wide_Wide_String
     (S : in out Sink'Class; Item : Wide_Wide_String) is
   begin
      for X of Item loop
         Put_Wide_Wide_Character (S, X);
      end loop;
   end Put_Wide_Wide_String;

   procedure New_Line (S : in out Sink'Class) is
   begin
      S.Column := 1;
      Put_Octet (S, NL);
   end New_Line;

   function Column (S : Sink'Class) return Positive is (S.Column);

   procedure Tab_To_Column (S : in out Sink'Class; Column : Positive) is
   begin
      if S.Column < Column then
         for X in 1 .. Column - S.Column loop
            Put_Octet (S, ' ');
         end loop;
         S.Column := Column;
      end if;
   end Tab_To_Column;

   procedure Set_Indentation (S : in out Sink'Class; Amount : Natural) is
   begin
      S.Indentation := Amount;
   end Set_Indentation;

   function Indentation (S : Sink'Class) return Natural is (S.Indentation);

   procedure Indent
     (S : in out Sink'Class; Amount : Optional_Indentation := Default)
   is
      By : constant Natural :=
        (if Amount = Default then S.Indent_Amount else Amount);
   begin
      Set_Indentation (S, Indentation (S) + By);
   end Indent;

   procedure Outdent
     (S : in out Sink'Class; Amount : Optional_Indentation := Default)
   is
      By : constant Natural :=
        (if Amount = Default then S.Indent_Amount else Amount);
   begin
      Set_Indentation (S, Indentation (S) - By);
   end Outdent;

end Ada.Strings.Text_Output.Utils;
