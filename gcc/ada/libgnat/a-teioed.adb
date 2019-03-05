------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . T E X T _ I O . E D I T I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Ada.Strings.Fixed;
package body Ada.Text_IO.Editing is

   package Strings renames Ada.Strings;
   package Strings_Fixed renames Ada.Strings.Fixed;
   package Text_IO renames Ada.Text_IO;

   ---------------------
   -- Blank_When_Zero --
   ---------------------

   function Blank_When_Zero (Pic : Picture) return Boolean is
   begin
      return Pic.Contents.Original_BWZ;
   end Blank_When_Zero;

   ------------
   -- Expand --
   ------------

   function Expand (Picture : String) return String is
      Result        : String (1 .. MAX_PICSIZE);
      Picture_Index : Integer := Picture'First;
      Result_Index  : Integer := Result'First;
      Count         : Natural;
      Last          : Integer;

      package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   begin
      if Picture'Length < 1 then
         raise Picture_Error;
      end if;

      if Picture (Picture'First) = '(' then
         raise Picture_Error;
      end if;

      loop
         case Picture (Picture_Index) is
            when '(' =>
               Int_IO.Get
                 (Picture (Picture_Index + 1 .. Picture'Last), Count, Last);

               if Picture (Last + 1) /= ')' then
                  raise Picture_Error;
               end if;

               --  In what follows note that one copy of the repeated character
               --  has already been made, so a count of one is a no-op, and a
               --  count of zero erases a character.

               if Result_Index + Count - 2 > Result'Last then
                  raise Picture_Error;
               end if;

               for J in 2 .. Count loop
                  Result (Result_Index + J - 2) := Picture (Picture_Index - 1);
               end loop;

               Result_Index := Result_Index + Count - 1;

               --  Last + 1 was a ')' throw it away too

               Picture_Index := Last + 2;

            when ')' =>
               raise Picture_Error;

            when others =>
               if Result_Index > Result'Last then
                  raise Picture_Error;
               end if;

               Result (Result_Index) := Picture (Picture_Index);
               Picture_Index := Picture_Index + 1;
               Result_Index := Result_Index + 1;
         end case;

         exit when Picture_Index > Picture'Last;
      end loop;

      return Result (1 .. Result_Index - 1);

   exception
      when others =>
         raise Picture_Error;
   end Expand;

   -------------------
   -- Format_Number --
   -------------------

   function Format_Number
     (Pic                 : Format_Record;
      Number              : String;
      Currency_Symbol     : String;
      Fill_Character      : Character;
      Separator_Character : Character;
      Radix_Point         : Character) return String
   is
      Attrs    : Number_Attributes := Parse_Number_String (Number);
      Position : Integer;
      Rounded  : String := Number;

      Sign_Position : Integer := Pic.Sign_Position; --  may float.

      Answer : String (1 .. Pic.Picture.Length) := Pic.Picture.Expanded;
      Last          : Integer;
      Currency_Pos  : Integer := Pic.Start_Currency;
      In_Currency   : Boolean := False;

      Dollar : Boolean := False;
      --  Overridden immediately if necessary

      Zero : Boolean := True;
      --  Set to False when a non-zero digit is output

   begin

      --  If the picture has fewer decimal places than the number, the image
      --  must be rounded according to the usual rules.

      if Attrs.Has_Fraction then
         declare
            R : constant Integer :=
              (Attrs.End_Of_Fraction - Attrs.Start_Of_Fraction + 1)
                - Pic.Max_Trailing_Digits;
            R_Pos : Integer;

         begin
            if R > 0 then
               R_Pos := Attrs.End_Of_Fraction - R;

               if Rounded (R_Pos + 1) > '4' then

                  if Rounded (R_Pos) = '.' then
                     R_Pos := R_Pos - 1;
                  end if;

                  if Rounded (R_Pos) /= '9' then
                     Rounded (R_Pos) := Character'Succ (Rounded (R_Pos));
                  else
                     Rounded (R_Pos) := '0';
                     R_Pos := R_Pos - 1;

                     while R_Pos > 1 loop
                        if Rounded (R_Pos) = '.' then
                           R_Pos := R_Pos - 1;
                        end if;

                        if Rounded (R_Pos) /= '9' then
                           Rounded (R_Pos) := Character'Succ (Rounded (R_Pos));
                           exit;
                        else
                           Rounded (R_Pos) := '0';
                           R_Pos := R_Pos - 1;
                        end if;
                     end loop;

                     --  The rounding may add a digit in front. Either the
                     --  leading blank or the sign (already captured) can
                     --  be overwritten.

                     if R_Pos = 1 then
                        Rounded (R_Pos) := '1';
                        Attrs.Start_Of_Int := Attrs.Start_Of_Int - 1;
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;

      if Pic.Start_Currency /= Invalid_Position then
         Dollar := Answer (Pic.Start_Currency) = '$';
      end if;

      --  Fix up "direct inserts" outside the playing field. Set up as one
      --  loop to do the beginning, one (reverse) loop to do the end.

      Last := 1;
      loop
         exit when Last = Pic.Start_Float;
         exit when Last = Pic.Radix_Position;
         exit when Answer (Last) = '9';

         case Answer (Last) is
            when '_' =>
               Answer (Last) := Separator_Character;

            when 'b' =>
               Answer (Last) := ' ';

            when others =>
               null;
         end case;

         exit when Last = Answer'Last;

         Last := Last + 1;
      end loop;

      --  Now for the end...

      for J in reverse Last .. Answer'Last loop
         exit when J = Pic.Radix_Position;

         --  Do this test First, Separator_Character can equal Pic.Floater

         if Answer (J) = Pic.Floater then
            exit;
         end if;

         case Answer (J) is
            when '_' =>
               Answer (J) := Separator_Character;

            when 'b' =>
               Answer (J) := ' ';

            when '9' =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      --  Non-floating sign

      if Pic.Start_Currency /= -1
        and then Answer (Pic.Start_Currency) = '#'
        and then Pic.Floater /= '#'
      then
         if Currency_Symbol'Length >
            Pic.End_Currency - Pic.Start_Currency + 1
         then
            raise Picture_Error;

         elsif Currency_Symbol'Length =
            Pic.End_Currency - Pic.Start_Currency + 1
         then
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
              Currency_Symbol;

         elsif Pic.Radix_Position = Invalid_Position
           or else Pic.Start_Currency < Pic.Radix_Position
         then
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
                                                        (others => ' ');
            Answer (Pic.End_Currency - Currency_Symbol'Length + 1 ..
                    Pic.End_Currency) := Currency_Symbol;

         else
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
                                                        (others => ' ');
            Answer (Pic.Start_Currency ..
                    Pic.Start_Currency + Currency_Symbol'Length - 1) :=
                                                        Currency_Symbol;
         end if;
      end if;

      --  Fill in leading digits

      if Attrs.End_Of_Int - Attrs.Start_Of_Int + 1 >
                                                Pic.Max_Leading_Digits
      then
         raise Ada.Text_IO.Layout_Error;
      end if;

      Position :=
        (if Pic.Radix_Position = Invalid_Position
         then Answer'Last
         else Pic.Radix_Position - 1);

      for J in reverse Attrs.Start_Of_Int .. Attrs.End_Of_Int loop
         while Answer (Position) /= '9'
                 and then
               Answer (Position) /= Pic.Floater
         loop
            if Answer (Position) = '_' then
               Answer (Position) := Separator_Character;

            elsif Answer (Position) = 'b' then
               Answer (Position) := ' ';
            end if;

            Position := Position - 1;
         end loop;

         Answer (Position) := Rounded (J);

         if Rounded (J) /= '0' then
            Zero := False;
         end if;

         Position := Position - 1;
      end loop;

      --  Do lead float

      if Pic.Start_Float = Invalid_Position then

         --  No leading floats, but need to change '9' to '0', '_' to
         --  Separator_Character and 'b' to ' '.

         for J in Last .. Position loop

            --  Last set when fixing the "uninteresting" leaders above.
            --  Don't duplicate the work.

            if Answer (J) = '9' then
               Answer (J) := '0';

            elsif Answer (J) = '_' then
               Answer (J) := Separator_Character;

            elsif Answer (J) = 'b' then
               Answer (J) := ' ';
            end if;
         end loop;

      elsif Pic.Floater = '<'
              or else
            Pic.Floater = '+'
              or else
            Pic.Floater = '-'
      then
         for J in Pic.End_Float .. Position loop --  May be null range.
            if Answer (J) = '9' then
               Answer (J) := '0';

            elsif Answer (J) = '_' then
               Answer (J) := Separator_Character;

            elsif Answer (J) = 'b' then
               Answer (J) := ' ';
            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for J in Pic.Start_Float .. Position - 1 loop
            Answer (J) := ' ';
         end loop;

         Answer (Position) := Pic.Floater;
         Sign_Position     := Position;

      elsif Pic.Floater = '$' then

         for J in Pic.End_Float .. Position loop --  May be null range.
            if Answer (J) = '9' then
               Answer (J) := '0';

            elsif Answer (J) = '_' then
               Answer (J) := ' ';    --  no separators before leftmost digit.

            elsif Answer (J) = 'b' then
               Answer (J) := ' ';
            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for J in Pic.Start_Float .. Position - 1 loop
            Answer (J) := ' ';
         end loop;

         Answer (Position) := Pic.Floater;
         Currency_Pos      := Position;

      elsif Pic.Floater = '*' then

         for J in Pic.End_Float .. Position loop --  May be null range.
            if Answer (J) = '9' then
               Answer (J) := '0';

            elsif Answer (J) = '_' then
               Answer (J) := Separator_Character;

            elsif Answer (J) = 'b' then
               Answer (J) := Fill_Character;
            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for J in Pic.Start_Float .. Position loop
            Answer (J) := Fill_Character;
         end loop;

      else
         if Pic.Floater = '#' then
            Currency_Pos := Currency_Symbol'Length;
            In_Currency := True;
         end if;

         for J in reverse Pic.Start_Float .. Position loop
            case Answer (J) is
               when '*' =>
                  Answer (J) := Fill_Character;

               when 'b' | '/' =>
                  if In_Currency and then Currency_Pos > 0 then
                     Answer (J)   := Currency_Symbol (Currency_Pos);
                     Currency_Pos := Currency_Pos - 1;
                  else
                     Answer (J) := ' ';
                  end if;

               when 'Z' | '0' =>
                  Answer (J) := ' ';

               when '9' =>
                  Answer (J) := '0';

               when '.' | 'V' | 'v' | '<' | '$' | '+' | '-' =>
                  null;

               when '#' =>
                  if Currency_Pos = 0 then
                     Answer (J) := ' ';
                  else
                     Answer (J)   := Currency_Symbol (Currency_Pos);
                     Currency_Pos := Currency_Pos - 1;
                  end if;

               when '_' =>
                  case Pic.Floater is
                     when '*' =>
                        Answer (J) := Fill_Character;

                     when 'Z' | 'b' =>
                        Answer (J) := ' ';

                     when '#' =>
                        if Currency_Pos = 0 then
                           Answer (J) := ' ';

                        else
                           Answer (J)   := Currency_Symbol (Currency_Pos);
                           Currency_Pos := Currency_Pos - 1;
                        end if;

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;
         end loop;

         if Pic.Floater = '#' and then Currency_Pos /= 0 then
            raise Ada.Text_IO.Layout_Error;
         end if;
      end if;

      --  Do sign

      if Sign_Position = Invalid_Position then
         if Attrs.Negative then
            raise Ada.Text_IO.Layout_Error;
         end if;

      else
         if Attrs.Negative then
            case Answer (Sign_Position) is
               when 'C' | 'D' | '-' =>
                  null;

               when '+' =>
                  Answer (Sign_Position) := '-';

               when '<' =>
                  Answer (Sign_Position)   := '(';
                  Answer (Pic.Second_Sign) := ')';

               when others =>
                  raise Picture_Error;
            end case;

         else --  positive

            case Answer (Sign_Position) is
               when '-' =>
                  Answer (Sign_Position) := ' ';

               when '<' | 'C' | 'D' =>
                  Answer (Sign_Position)   := ' ';
                  Answer (Pic.Second_Sign) := ' ';

               when '+' =>
                  null;

               when others =>
                  raise Picture_Error;
            end case;
         end if;
      end if;

      --  Fill in trailing digits

      if Pic.Max_Trailing_Digits > 0 then

         if Attrs.Has_Fraction then
            Position := Attrs.Start_Of_Fraction;
            Last     := Pic.Radix_Position + 1;

            for J in Last .. Answer'Last loop
               if Answer (J) = '9' or else Answer (J) = Pic.Floater then
                  Answer (J) := Rounded (Position);

                  if Rounded (Position) /= '0' then
                     Zero := False;
                  end if;

                  Position := Position + 1;
                  Last     := J + 1;

                  --  Used up fraction but remember place in Answer

                  exit when Position > Attrs.End_Of_Fraction;

               elsif Answer (J) = 'b' then
                  Answer (J) := ' ';

               elsif Answer (J) = '_' then
                  Answer (J) := Separator_Character;
               end if;

               Last := J + 1;
            end loop;

            Position := Last;

         else
            Position := Pic.Radix_Position + 1;
         end if;

         --  Now fill remaining 9's with zeros and _ with separators

         Last := Answer'Last;

         for J in Position .. Last loop
            if Answer (J) = '9' then
               Answer (J) := '0';

            elsif Answer (J) = Pic.Floater then
               Answer (J) := '0';

            elsif Answer (J) = '_' then
               Answer (J) := Separator_Character;

            elsif Answer (J) = 'b' then
               Answer (J) := ' ';

            end if;
         end loop;

         Position := Last + 1;

      else
         if Pic.Floater = '#' and then Currency_Pos /= 0 then
            raise Ada.Text_IO.Layout_Error;
         end if;

         --  No trailing digits, but now J may need to stick in a currency
         --  symbol or sign.

         Position :=
           (if Pic.Start_Currency = Invalid_Position
            then Answer'Last + 1
            else Pic.Start_Currency);
      end if;

      for J in Position .. Answer'Last loop
         if Pic.Start_Currency /= Invalid_Position
           and then Answer (Pic.Start_Currency) = '#'
         then
            Currency_Pos := 1;
         end if;

         case Answer (J) is
            when '*' =>
               Answer (J) := Fill_Character;

            when 'b' =>
               if In_Currency then
                  Answer (J) := Currency_Symbol (Currency_Pos);
                  Currency_Pos := Currency_Pos + 1;

                  if Currency_Pos > Currency_Symbol'Length then
                     In_Currency := False;
                  end if;
               end if;

            when '#' =>
               if Currency_Pos > Currency_Symbol'Length then
                  Answer (J) := ' ';

               else
                  In_Currency := True;
                  Answer (J)   := Currency_Symbol (Currency_Pos);
                  Currency_Pos := Currency_Pos + 1;

                  if Currency_Pos > Currency_Symbol'Length then
                     In_Currency := False;
                  end if;
               end if;

            when '_' =>
               Answer (J) := Currency_Symbol (Currency_Pos);
               Currency_Pos := Currency_Pos + 1;

               case Pic.Floater is
                  when '*' =>
                     Answer (J) := Fill_Character;

                  when 'Z' | 'z' =>
                     Answer (J) := ' ';

                  when '#' =>
                     if Currency_Pos > Currency_Symbol'Length then
                        Answer (J) := ' ';
                     else
                        Answer (J)   := Currency_Symbol (Currency_Pos);
                        Currency_Pos := Currency_Pos + 1;
                     end if;

                  when others =>
                     null;
               end case;

            when others =>
               exit;
         end case;
      end loop;

      --  Now get rid of Blank_when_Zero and complete Star fill

      if Zero and then Pic.Blank_When_Zero then

         --  Value is zero, and blank it

         Last := Answer'Last;

         if Dollar then
            Last := Last - 1 + Currency_Symbol'Length;
         end if;

         if Pic.Radix_Position /= Invalid_Position
           and then Answer (Pic.Radix_Position) = 'V'
         then
            Last := Last - 1;
         end if;

         return String'(1 .. Last => ' ');

      elsif Zero and then Pic.Star_Fill then
         Last := Answer'Last;

         if Dollar then
            Last := Last - 1 + Currency_Symbol'Length;
         end if;

         if Pic.Radix_Position /= Invalid_Position then

            if Answer (Pic.Radix_Position) = 'V' then
               Last := Last - 1;

            elsif Dollar then
               if Pic.Radix_Position > Pic.Start_Currency then
                  return String'(1 .. Pic.Radix_Position - 1 => '*') &
                     Radix_Point &
                     String'(Pic.Radix_Position + 1 .. Last => '*');

               else
                  return
                     String'
                     (1 ..
                      Pic.Radix_Position + Currency_Symbol'Length - 2 =>
                         '*') & Radix_Point &
                     String'
                     (Pic.Radix_Position + Currency_Symbol'Length .. Last
                      => '*');
               end if;

            else
               return String'(1 .. Pic.Radix_Position - 1 => '*') &
                  Radix_Point &
                  String'(Pic.Radix_Position + 1 .. Last => '*');
            end if;
         end if;

         return String'(1 .. Last => '*');
      end if;

      --  This was once a simple return statement, now there are nine different
      --  return cases. Not to mention the five above to deal with zeros. Why
      --  not split things out?

      --  Processing the radix and sign expansion separately would require
      --  lots of copying--the string and some of its indexes--without
      --  really simplifying the logic. The cases are:

      --  1) Expand $, replace '.' with Radix_Point
      --  2) No currency expansion, replace '.' with Radix_Point
      --  3) Expand $, radix blanked
      --  4) No currency expansion, radix blanked
      --  5) Elide V
      --  6) Expand $, Elide V
      --  7) Elide V, Expand $ (Two cases depending on order.)
      --  8) No radix, expand $
      --  9) No radix, no currency expansion

      if Pic.Radix_Position /= Invalid_Position then

         if Answer (Pic.Radix_Position) = '.' then
            Answer (Pic.Radix_Position) := Radix_Point;

            if Dollar then

               --  1) Expand $, replace '.' with Radix_Point

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Answer'Last);

            else
               --  2) No currency expansion, replace '.' with Radix_Point

               return Answer;
            end if;

         elsif Answer (Pic.Radix_Position) = ' ' then --  blanked radix.
            if Dollar then

               --  3) Expand $, radix blanked

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                 Answer (Currency_Pos + 1 .. Answer'Last);

            else
               --  4) No expansion, radix blanked

               return Answer;
            end if;

         --  V cases

         else
            if not Dollar then

               --  5) Elide V

               return Answer (1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Answer'Last);

            elsif Currency_Pos < Pic.Radix_Position then

               --  6) Expand $, Elide V

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Answer'Last);

            else
               --  7) Elide V, Expand $

               return Answer (1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Currency_Pos - 1) &
                  Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Answer'Last);
            end if;
         end if;

      elsif Dollar then

         --  8) No radix, expand $

         return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
            Answer (Currency_Pos + 1 .. Answer'Last);

      else
         --  9) No radix, no currency expansion

         return Answer;
      end if;
   end Format_Number;

   -------------------------
   -- Parse_Number_String --
   -------------------------

   function Parse_Number_String (Str : String) return Number_Attributes is
      Answer : Number_Attributes;

   begin
      for J in Str'Range loop
         case Str (J) is
            when ' ' =>
               null; --  ignore

            when '1' .. '9' =>

               --  Decide if this is the start of a number.
               --  If so, figure out which one...

               if Answer.Has_Fraction then
                  Answer.End_Of_Fraction := J;
               else
                  if Answer.Start_Of_Int = Invalid_Position then
                     --  start integer
                     Answer.Start_Of_Int := J;
                  end if;
                  Answer.End_Of_Int := J;
               end if;

            when '0' =>

               --  Only count a zero before the decimal point if it follows a
               --  non-zero digit. After the decimal point, zeros will be
               --  counted if followed by a non-zero digit.

               if not Answer.Has_Fraction then
                  if Answer.Start_Of_Int /= Invalid_Position then
                     Answer.End_Of_Int := J;
                  end if;
               end if;

            when '-' =>

               --  Set negative

               Answer.Negative := True;

            when '.' =>

               --  Close integer, start fraction

               if Answer.Has_Fraction then
                  raise Picture_Error;
               end if;

               --  Two decimal points is a no-no

               Answer.Has_Fraction    := True;
               Answer.End_Of_Fraction := J;

               --  Could leave this at Invalid_Position, but this seems the
               --  right way to indicate a null range...

               Answer.Start_Of_Fraction := J + 1;
               Answer.End_Of_Int        := J - 1;

            when others =>
               raise Picture_Error; -- can this happen? probably not
         end case;
      end loop;

      if Answer.Start_Of_Int = Invalid_Position then
         Answer.Start_Of_Int := Answer.End_Of_Int + 1;
      end if;

      --  No significant (integer) digits needs a null range

      return Answer;
   end Parse_Number_String;

   ----------------
   -- Pic_String --
   ----------------

   --  The following ensures that we return B and not b being careful not
   --  to break things which expect lower case b for blank. See CXF3A02.

   function Pic_String (Pic : Picture) return String is
      Temp : String (1 .. Pic.Contents.Picture.Length) :=
        Pic.Contents.Picture.Expanded;
   begin
      for J in Temp'Range loop
         if Temp (J) = 'b' then
            Temp (J) := 'B';
         end if;
      end loop;

      return Temp;
   end Pic_String;

   ------------------
   -- Precalculate --
   ------------------

   procedure Precalculate  (Pic : in out Format_Record) is
      Debug : constant Boolean := False;
      --  Set True to generate debug output

      Computed_BWZ : Boolean := True;

      type Legality is  (Okay, Reject);

      State : Legality := Reject;
      --  Start in reject, which will reject null strings

      Index : Pic_Index := Pic.Picture.Expanded'First;

      function At_End return Boolean;
      pragma Inline (At_End);

      procedure Set_State (L : Legality);
      pragma Inline (Set_State);

      function Look return Character;
      pragma Inline (Look);

      function Is_Insert return Boolean;
      pragma Inline (Is_Insert);

      procedure Skip;
      pragma Inline (Skip);

      procedure Debug_Start (Name : String);
      pragma Inline (Debug_Start);

      procedure Debug_Integer  (Value : Integer; S : String);
      pragma Inline (Debug_Integer);

      procedure Trailing_Currency;
      procedure Trailing_Bracket;
      procedure Number_Fraction;
      procedure Number_Completion;
      procedure Number_Fraction_Or_Bracket;
      procedure Number_Fraction_Or_Z_Fill;
      procedure Zero_Suppression;
      procedure Floating_Bracket;
      procedure Number_Fraction_Or_Star_Fill;
      procedure Star_Suppression;
      procedure Number_Fraction_Or_Dollar;
      procedure Leading_Dollar;
      procedure Number_Fraction_Or_Pound;
      procedure Leading_Pound;
      procedure Picture;
      procedure Floating_Plus;
      procedure Floating_Minus;
      procedure Picture_Plus;
      procedure Picture_Minus;
      procedure Picture_Bracket;
      procedure Number;
      procedure Optional_RHS_Sign;
      procedure Picture_String;
      procedure Set_Debug;

      ------------
      -- At_End --
      ------------

      function At_End return Boolean is
      begin
         Debug_Start ("At_End");
         return Index > Pic.Picture.Length;
      end At_End;

      --------------
      -- Set_Debug--
      --------------

      --  Needed to have a procedure to pass to pragma Debug

      procedure Set_Debug is
      begin
         --  Uncomment this line and make Debug a variable to enable debug

         --  Debug := True;

         null;
      end Set_Debug;

      -------------------
      -- Debug_Integer --
      -------------------

      procedure Debug_Integer (Value : Integer; S : String) is

      begin
         if Debug and then Value > 0 then
            if Ada.Text_IO.Col > 70 - S'Length then
               Ada.Text_IO.New_Line;
            end if;

            Ada.Text_IO.Put (' ' & S & Integer'Image (Value) & ',');
         end if;
      end Debug_Integer;

      -----------------
      -- Debug_Start --
      -----------------

      procedure Debug_Start (Name : String) is
      begin
         if Debug then
            Ada.Text_IO.Put_Line ("  In " & Name & '.');
         end if;
      end Debug_Start;

      ----------------------
      -- Floating_Bracket --
      ----------------------

      --  Note that Floating_Bracket is only called with an acceptable
      --  prefix. But we don't set Okay, because we must end with a '>'.

      procedure Floating_Bracket is
      begin
         Debug_Start ("Floating_Bracket");

         --  Two different floats not allowed

         if Pic.Floater /= '!' and then Pic.Floater /= '<' then
            raise Picture_Error;

         else
            Pic.Floater := '<';
         end if;

         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;

         --  First bracket wasn't counted...

         Skip; --  known '<'

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Pic.End_Float := Index;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Skip;

               when '9' =>
                  Number_Completion;

               when '$' =>
                  Leading_Dollar;

               when '#' =>
                  Leading_Pound;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Bracket;
                  return;

               when others =>
               return;
            end case;
         end loop;
      end Floating_Bracket;

      --------------------
      -- Floating_Minus --
      --------------------

      procedure Floating_Minus is
      begin
         Debug_Start ("Floating_Minus");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '-' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

               when '9' =>
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip; --  Radix

                  while Is_Insert loop
                     Skip;
                  end loop;

                  if At_End then
                     return;
                  end if;

                  if Look = '-' then
                     loop
                        if At_End then
                           return;
                        end if;

                        case Look is
                           when '-' =>
                              Pic.Max_Trailing_Digits :=
                                Pic.Max_Trailing_Digits + 1;
                              Pic.End_Float := Index;
                              Skip;

                           when '_' | '0' | '/' =>
                              Skip;

                           when 'B' | 'b'  =>
                              Pic.Picture.Expanded (Index) := 'b';
                              Skip;

                           when others =>
                              return;
                        end case;
                     end loop;

                  else
                     Number_Completion;
                  end if;

                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Floating_Minus;

      -------------------
      -- Floating_Plus --
      -------------------

      procedure Floating_Plus is
      begin
         Debug_Start ("Floating_Plus");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '+' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

               when '9' =>
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip; --  Radix

                  while Is_Insert loop
                     Skip;
                  end loop;

                  if At_End then
                     return;
                  end if;

                  if Look = '+' then
                     loop
                        if At_End then
                           return;
                        end if;

                        case Look is
                           when '+' =>
                              Pic.Max_Trailing_Digits :=
                                Pic.Max_Trailing_Digits + 1;
                              Pic.End_Float := Index;
                              Skip;

                           when '_' | '0' | '/' =>
                              Skip;

                           when 'B' | 'b'  =>
                              Pic.Picture.Expanded (Index) := 'b';
                              Skip;

                           when others =>
                              return;
                        end case;
                     end loop;

                  else
                     Number_Completion;
                  end if;

                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Floating_Plus;

      ---------------
      -- Is_Insert --
      ---------------

      function Is_Insert return Boolean is
      begin
         if At_End then
            return False;
         end if;

         case Pic.Picture.Expanded (Index) is
            when '_' | '0' | '/' =>
               return True;

            when 'B' | 'b' =>
               Pic.Picture.Expanded (Index) := 'b'; --  canonical
               return True;

            when others =>
               return False;
         end case;
      end Is_Insert;

      --------------------
      -- Leading_Dollar --
      --------------------

      --  Note that Leading_Dollar can be called in either State. It will set
      --  state to Okay only if a 9 or (second) $ is encountered.

      --  Also notice the tricky bit with State and Zero_Suppression.
      --  Zero_Suppression is Picture_Error if a '$' or a '9' has been
      --  encountered, exactly the cases where State has been set.

      procedure Leading_Dollar is
      begin
         Debug_Start ("Leading_Dollar");

         --  Treat as a floating dollar, and unwind otherwise

         if Pic.Floater /= '!' and then Pic.Floater /= '$' then

            --  Two floats not allowed

            raise Picture_Error;

         else
            Pic.Floater := '$';
         end if;

         Pic.Start_Currency := Index;
         Pic.End_Currency := Index;
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  currency place.

         Skip; --  known '$'

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

                  --  A trailing insertion character is not part of the
                  --  floating currency, so need to look ahead.

                  if Look /= '$' then
                     Pic.End_Float := Pic.End_Float - 1;
                  end if;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  if State = Okay then
                     raise Picture_Error;
                  else
                     --  Overwrite Floater and Start_Float

                     Pic.Floater := 'Z';
                     Pic.Start_Float := Index;
                     Zero_Suppression;
                  end if;

               when '*' =>
                  if State = Okay then
                     raise Picture_Error;
                  else
                     --  Overwrite Floater and Start_Float

                     Pic.Floater := '*';
                     Pic.Start_Float := Index;
                     Star_Suppression;
                  end if;

               when '$' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.End_Currency := Index;
                  Set_State (Okay); Skip;

               when '9' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  A single dollar does not a floating make

                  Number_Completion;
                  return;

               when 'V' | 'v' | '.' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Only one dollar before the sign is okay, but doesn't
                  --  float.

                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Dollar;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Leading_Dollar;

      -------------------
      -- Leading_Pound --
      -------------------

      --  This one is complex.  A Leading_Pound can be fixed or floating,
      --  but in some cases the decision has to be deferred until we leave
      --  this procedure. Also note that Leading_Pound can be called in
      --  either State.

      --  It will set state to Okay only if a 9 or (second) # is encountered

      --  One Last note:  In ambiguous cases, the currency is treated as
      --  floating unless there is only one '#'.

      procedure Leading_Pound is

         Inserts : Boolean := False;
         --  Set to True if a '_', '0', '/', 'B', or 'b' is encountered

         Must_Float : Boolean := False;
         --  Set to true if a '#' occurs after an insert

      begin
         Debug_Start ("Leading_Pound");

         --  Treat as a floating currency. If it isn't, this will be
         --  overwritten later.

         if Pic.Floater /= '!' and then Pic.Floater /= '#' then

            --  Two floats not allowed

            raise Picture_Error;

         else
            Pic.Floater := '#';
         end if;

         Pic.Start_Currency := Index;
         Pic.End_Currency := Index;
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  currency place.

         Pic.Max_Currency_Digits := 1; --  we've seen one.

         Skip; --  known '#'

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Inserts := True;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Pic.End_Float := Index;
                  Inserts := True;
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  if Must_Float then
                     raise Picture_Error;
                  else
                     Pic.Max_Leading_Digits := 0;

                     --  Overwrite Floater and Start_Float

                     Pic.Floater := 'Z';
                     Pic.Start_Float := Index;
                     Zero_Suppression;
                  end if;

               when '*' =>
                  if Must_Float then
                     raise Picture_Error;
                  else
                     Pic.Max_Leading_Digits := 0;

                     --  Overwrite Floater and Start_Float
                     Pic.Floater := '*';
                     Pic.Start_Float := Index;
                     Star_Suppression;
                  end if;

               when '#' =>
                  if Inserts then
                     Must_Float := True;
                  end if;

                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.End_Currency := Index;
                  Set_State (Okay);
                  Skip;

               when '9' =>
                  if State /= Okay then

                     --  A single '#' doesn't float

                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Number_Completion;
                  return;

               when 'V' | 'v' | '.' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Only one pound before the sign is okay, but doesn't
                  --  float.

                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Pound;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Leading_Pound;

      ----------
      -- Look --
      ----------

      function Look return Character is
      begin
         if At_End then
            raise Picture_Error;
         end if;

         return Pic.Picture.Expanded (Index);
      end Look;

      ------------
      -- Number --
      ------------

      procedure Number is
      begin
         Debug_Start ("Number");

         loop
            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay);
                  Skip;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  return;

               when others =>
                  return;
            end case;

            if At_End then
               return;
            end if;

            --  Will return in Okay state if a '9' was seen

         end loop;
      end Number;

      -----------------------
      -- Number_Completion --
      -----------------------

      procedure Number_Completion is
      begin
         Debug_Start ("Number_Completion");

         while not At_End loop
            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay);
                  Skip;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Number_Completion;

      ---------------------
      -- Number_Fraction --
      ---------------------

      procedure Number_Fraction is
      begin
         --  Note that number fraction can be called in either State.
         --  It will set state to Valid only if a 9 is encountered.

         Debug_Start ("Number_Fraction");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Set_State (Okay); Skip;

               when others =>
                  return;
            end case;
         end loop;
      end Number_Fraction;

      --------------------------------
      -- Number_Fraction_Or_Bracket --
      --------------------------------

      procedure Number_Fraction_Or_Bracket is
      begin
         Debug_Start ("Number_Fraction_Or_Bracket");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '<' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Bracket;

      -------------------------------
      -- Number_Fraction_Or_Dollar --
      -------------------------------

      procedure Number_Fraction_Or_Dollar is
      begin
         Debug_Start ("Number_Fraction_Or_Dollar");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '$' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '$' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Dollar;

      ------------------------------
      -- Number_Fraction_Or_Pound --
      ------------------------------

      procedure Number_Fraction_Or_Pound is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '#' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '#' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Pound;

      ----------------------------------
      -- Number_Fraction_Or_Star_Fill --
      ----------------------------------

      procedure Number_Fraction_Or_Star_Fill is
      begin
         Debug_Start ("Number_Fraction_Or_Star_Fill");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '*' =>
                  Pic.Star_Fill := True;
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '*' =>
                           Pic.Star_Fill := True;
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Star_Fill;

      -------------------------------
      -- Number_Fraction_Or_Z_Fill --
      -------------------------------

      procedure Number_Fraction_Or_Z_Fill is
      begin
         Debug_Start ("Number_Fraction_Or_Z_Fill");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when 'Z' | 'z' =>
                           Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Z_Fill;

      -----------------------
      -- Optional_RHS_Sign --
      -----------------------

      procedure Optional_RHS_Sign is
      begin
         Debug_Start ("Optional_RHS_Sign");

         if At_End then
            return;
         end if;

         case Look is
            when '+' | '-' =>
               Pic.Sign_Position := Index;
               Skip;
               return;

            when 'C' | 'c' =>
               Pic.Sign_Position := Index;
               Pic.Picture.Expanded (Index) := 'C';
               Skip;

               if Look = 'R' or else Look = 'r' then
                  Pic.Second_Sign := Index;
                  Pic.Picture.Expanded (Index) := 'R';
                  Skip;

               else
                  raise Picture_Error;
               end if;

               return;

            when 'D' | 'd' =>
               Pic.Sign_Position := Index;
               Pic.Picture.Expanded (Index) := 'D';
               Skip;

               if Look = 'B' or else Look = 'b' then
                  Pic.Second_Sign := Index;
                  Pic.Picture.Expanded (Index) := 'B';
                  Skip;

               else
                  raise Picture_Error;
               end if;

               return;

            when '>' =>
               if Pic.Picture.Expanded (Pic.Sign_Position) = '<' then
                  Pic.Second_Sign := Index;
                  Skip;

               else
                  raise Picture_Error;
               end if;

            when others =>
               return;
         end case;
      end Optional_RHS_Sign;

      -------------
      -- Picture --
      -------------

      --  Note that Picture can be called in either State

      --  It will set state to Valid only if a 9 is encountered or floating
      --  currency is called.

      procedure Picture is
      begin
         Debug_Start ("Picture");

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '$' =>
                  Leading_Dollar;
                  return;

               when '#' =>
                  Leading_Pound;
                  return;

               when '9' =>
                  Computed_BWZ := False;
                  Set_State (Okay);
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Skip;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  Trailing_Currency;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Picture;

      ---------------------
      -- Picture_Bracket --
      ---------------------

      procedure Picture_Bracket is
      begin
         Pic.Sign_Position := Index;
         Debug_Start ("Picture_Bracket");
         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise

         Pic.Floater := '<';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Bracket

         loop
            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Set_State (Okay);  --  "<<>" is enough.
                  Floating_Bracket;
                  Trailing_Currency;
                  Trailing_Bracket;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Trailing_Bracket;
                  Set_State (Okay);
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit

                  Picture;
                  Trailing_Bracket;
                  return;

               when others =>
                  raise Picture_Error;
            end case;
         end loop;
      end Picture_Bracket;

      -------------------
      -- Picture_Minus --
      -------------------

      procedure Picture_Minus is
      begin
         Debug_Start ("Picture_Minus");

         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise

         Pic.Floater := '-';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Minus

         loop
            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '-' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;
                  Set_State (Okay);  --  "-- " is enough.
                  Floating_Minus;
                  Trailing_Currency;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Set_State (Okay);
                  return;

               when 'Z' | 'z' =>

                  --  Can't have Z and a floating sign

                  if State = Okay then
                     Set_State (Reject);
                  end if;

                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency
                  Zero_Suppression;
                  Trailing_Currency;
                  Optional_RHS_Sign;
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit

                  Picture;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Picture_Minus;

      ------------------
      -- Picture_Plus --
      ------------------

      procedure Picture_Plus is
      begin
         Debug_Start ("Picture_Plus");
         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise

         Pic.Floater := '+';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Plus

         loop
            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '+' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;
                  Set_State (Okay);  --  "++" is enough
                  Floating_Plus;
                  Trailing_Currency;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Set_State (Okay);
                  return;

               when 'Z' | 'z' =>
                  if State = Okay then
                     Set_State (Reject);
                  end if;

                  --  Can't have Z and a floating sign

                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  --  '+Z' is acceptable

                  Set_State (Okay);

                  --  Overwrite Floater and Start_Float

                  Pic.Floater := 'Z';
                  Pic.Start_Float := Index;

                  Zero_Suppression;
                  Trailing_Currency;
                  Optional_RHS_Sign;
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit

                  Picture;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Picture_Plus;

      --------------------
      -- Picture_String --
      --------------------

      procedure Picture_String is
      begin
         Debug_Start ("Picture_String");

         while Is_Insert loop
            Skip;
         end loop;

         case Look is
            when '$' | '#' =>
               Picture;
               Optional_RHS_Sign;

            when '+' =>
               Picture_Plus;

            when '-' =>
               Picture_Minus;

            when '<' =>
               Picture_Bracket;

            when 'Z' | 'z' =>
               Pic.Picture.Expanded (Index) := 'Z'; -- consistency
               Zero_Suppression;
               Trailing_Currency;
               Optional_RHS_Sign;

            when '*' =>
               Star_Suppression;
               Trailing_Currency;
               Optional_RHS_Sign;

            when '9' | '.' | 'V' | 'v' =>
               Number;
               Trailing_Currency;
               Optional_RHS_Sign;

            when others =>
               raise Picture_Error;
         end case;

         --  Blank when zero either if the PIC does not contain a '9' or if
         --  requested by the user and no '*'.

         Pic.Blank_When_Zero :=
           (Computed_BWZ or else Pic.Blank_When_Zero)
             and then not Pic.Star_Fill;

         --  Star fill if '*' and no '9'

         Pic.Star_Fill := Pic.Star_Fill and then Computed_BWZ;

         if not At_End then
            Set_State (Reject);
         end if;
      end Picture_String;

      ---------------
      -- Set_State --
      ---------------

      procedure Set_State (L : Legality) is
      begin
         if Debug then
            Ada.Text_IO.Put_Line
              ("  Set state from " & Legality'Image (State)
               & " to " & Legality'Image (L));
         end if;

         State := L;
      end Set_State;

      ----------
      -- Skip --
      ----------

      procedure Skip is
      begin
         if Debug then
            Ada.Text_IO.Put_Line ("  Skip " & Pic.Picture.Expanded (Index));
         end if;

         Index := Index + 1;
      end Skip;

      ----------------------
      -- Star_Suppression --
      ----------------------

      procedure Star_Suppression is
      begin
         Debug_Start ("Star_Suppression");

         if Pic.Floater /= '!' and then Pic.Floater /= '*' then

            --  Two floats not allowed

            raise Picture_Error;

         else
            Pic.Floater := '*';
         end if;

         Pic.Start_Float := Index;
         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
         Set_State (Okay);

         --  Even a single * is a valid picture

         Pic.Star_Fill := True;
         Skip; --  Known *

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '*' =>
                  Pic.End_Float := Index;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay); Skip;

               when '9' =>
                  Set_State (Okay);
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Star_Fill;
                  return;

               when '#' | '$' =>
                  if Pic.Max_Currency_Digits > 0 then
                     raise Picture_Error;
                  end if;

                  --  Cannot have leading and trailing currency

                  Trailing_Currency;
                  Set_State (Okay);
                  return;

               when others =>
                  raise Picture_Error;
            end case;
         end loop;
      end Star_Suppression;

      ----------------------
      -- Trailing_Bracket --
      ----------------------

      procedure Trailing_Bracket is
      begin
         Debug_Start ("Trailing_Bracket");

         if Look = '>' then
            Pic.Second_Sign := Index;
            Skip;
         else
            raise Picture_Error;
         end if;
      end Trailing_Bracket;

      -----------------------
      -- Trailing_Currency --
      -----------------------

      procedure Trailing_Currency is
      begin
         Debug_Start ("Trailing_Currency");

         if At_End then
            return;
         end if;

         if Look = '$' then
            Pic.Start_Currency := Index;
            Pic.End_Currency := Index;
            Skip;

         else
            while not At_End and then Look = '#' loop
               if Pic.Start_Currency = Invalid_Position then
                  Pic.Start_Currency := Index;
               end if;

               Pic.End_Currency := Index;
               Skip;
            end loop;
         end if;

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when others =>
                  return;
            end case;
         end loop;
      end Trailing_Currency;

      ----------------------
      -- Zero_Suppression --
      ----------------------

      procedure Zero_Suppression is
      begin
         Debug_Start ("Zero_Suppression");

         Pic.Floater := 'Z';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
         Pic.Picture.Expanded (Index) := 'Z'; -- consistency

         Skip; --  Known Z

         loop
            --  Even a single Z is a valid picture

            if At_End then
               Set_State (Okay);
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Set_State (Okay);
                  Skip;

               when '9' =>
                  Set_State (Okay);
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Z_Fill;
                  return;

               when '#' | '$' =>
                  Trailing_Currency;
                  Set_State (Okay);
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Zero_Suppression;

   --  Start of processing for Precalculate

   begin
      pragma Debug (Set_Debug);

      Picture_String;

      if Debug then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put (" Picture : """ &
                     Pic.Picture.Expanded (1 .. Pic.Picture.Length) & """,");
         Ada.Text_IO.Put (" Floater : '" & Pic.Floater & "',");
      end if;

      if State = Reject then
         raise Picture_Error;
      end if;

      Debug_Integer (Pic.Radix_Position, "Radix Positon : ");
      Debug_Integer (Pic.Sign_Position, "Sign Positon : ");
      Debug_Integer (Pic.Second_Sign, "Second Sign : ");
      Debug_Integer (Pic.Start_Float, "Start Float : ");
      Debug_Integer (Pic.End_Float, "End Float : ");
      Debug_Integer (Pic.Start_Currency, "Start Currency : ");
      Debug_Integer (Pic.End_Currency, "End Currency : ");
      Debug_Integer (Pic.Max_Leading_Digits, "Max Leading Digits : ");
      Debug_Integer (Pic.Max_Trailing_Digits, "Max Trailing Digits : ");

      if Debug then
         Ada.Text_IO.New_Line;
      end if;

   exception

      when Constraint_Error =>

      --  To deal with special cases like null strings

      raise Picture_Error;
   end Precalculate;

   ----------------
   -- To_Picture --
   ----------------

   function To_Picture
     (Pic_String      : String;
      Blank_When_Zero : Boolean := False) return Picture
   is
      Result : Picture;

   begin
      declare
         Item : constant String := Expand (Pic_String);

      begin
         Result.Contents.Picture         := (Item'Length, Item);
         Result.Contents.Original_BWZ := Blank_When_Zero;
         Result.Contents.Blank_When_Zero := Blank_When_Zero;
         Precalculate (Result.Contents);
         return Result;
      end;

   exception
      when others =>
         raise Picture_Error;
   end To_Picture;

   -----------
   -- Valid --
   -----------

   function Valid
     (Pic_String      : String;
      Blank_When_Zero : Boolean := False) return Boolean
   is
   begin
      declare
         Expanded_Pic : constant String := Expand (Pic_String);
         --  Raises Picture_Error if Item not well-formed

         Format_Rec : Format_Record;

      begin
         Format_Rec.Picture := (Expanded_Pic'Length, Expanded_Pic);
         Format_Rec.Blank_When_Zero := Blank_When_Zero;
         Format_Rec.Original_BWZ := Blank_When_Zero;
         Precalculate (Format_Rec);

         --  False only if Blank_When_Zero is True but the pic string has a '*'

         return not Blank_When_Zero
           or else Strings_Fixed.Index (Expanded_Pic, "*") = 0;
      end;

   exception
      when others => return False;
   end Valid;

   --------------------
   -- Decimal_Output --
   --------------------

   package body Decimal_Output is

      -----------
      -- Image --
      -----------

      function Image
        (Item       : Num;
         Pic        : Picture;
         Currency   : String    := Default_Currency;
         Fill       : Character := Default_Fill;
         Separator  : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark) return String
      is
      begin
         return Format_Number
            (Pic.Contents, Num'Image (Item),
             Currency, Fill, Separator, Radix_Mark);
      end Image;

      ------------
      -- Length --
      ------------

      function Length
        (Pic      : Picture;
         Currency : String := Default_Currency) return Natural
      is
         Picstr     : constant String := Pic_String (Pic);
         V_Adjust   : Integer := 0;
         Cur_Adjust : Integer := 0;

      begin
         --  Check if Picstr has 'V' or '$'

         --  If 'V', then length is 1 less than otherwise

         --  If '$', then length is Currency'Length-1 more than otherwise

         --  This should use the string handling package ???

         for J in Picstr'Range loop
            if Picstr (J) = 'V' then
               V_Adjust := -1;

            elsif Picstr (J) = '$' then
               Cur_Adjust := Currency'Length - 1;
            end if;
         end loop;

         return Picstr'Length - V_Adjust + Cur_Adjust;
      end Length;

      ---------
      -- Put --
      ---------

      procedure Put
        (File       : Text_IO.File_Type;
         Item       : Num;
         Pic        : Picture;
         Currency   : String    := Default_Currency;
         Fill       : Character := Default_Fill;
         Separator  : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
      is
      begin
         Text_IO.Put (File, Image (Item, Pic,
                                   Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put
        (Item       : Num;
         Pic        : Picture;
         Currency   : String    := Default_Currency;
         Fill       : Character := Default_Fill;
         Separator  : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
      is
      begin
         Text_IO.Put (Image (Item, Pic,
                             Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put
        (To         : out String;
         Item       : Num;
         Pic        : Picture;
         Currency   : String    := Default_Currency;
         Fill       : Character := Default_Fill;
         Separator  : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
      is
         Result : constant String :=
           Image (Item, Pic, Currency, Fill, Separator, Radix_Mark);

      begin
         if Result'Length > To'Length then
            raise Ada.Text_IO.Layout_Error;
         else
            Strings_Fixed.Move (Source => Result, Target => To,
                                Justify => Strings.Right);
         end if;
      end Put;

      -----------
      -- Valid --
      -----------

      function Valid
        (Item     : Num;
         Pic      : Picture;
         Currency : String := Default_Currency) return Boolean
      is
      begin
         declare
            Temp : constant String := Image (Item, Pic, Currency);
            pragma Warnings (Off, Temp);
         begin
            return True;
         end;

      exception
         when Ada.Text_IO.Layout_Error => return False;

      end Valid;
   end Decimal_Output;

end Ada.Text_IO.Editing;
