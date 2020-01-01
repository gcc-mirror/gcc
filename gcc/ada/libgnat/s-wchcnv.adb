------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ C N V                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with Interfaces;     use Interfaces;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_Cnv is

   -----------------------------
   -- Char_Sequence_To_UTF_32 --
   -----------------------------

   function Char_Sequence_To_UTF_32
     (C  : Character;
      EM : System.WCh_Con.WC_Encoding_Method) return UTF_32_Code
   is
      B1 : Unsigned_32;
      C1 : Character;
      U  : Unsigned_32;
      W  : Unsigned_32;

      procedure Get_Hex (N : Character);
      --  If N is a hex character, then set B1 to 16 * B1 + character N.
      --  Raise Constraint_Error if character N is not a hex character.

      procedure Get_UTF_Byte;
      pragma Inline (Get_UTF_Byte);
      --  Used to interpret a 2#10xxxxxx# continuation byte in UTF-8 mode.
      --  Reads a byte, and raises CE if the first two bits are not 10.
      --  Otherwise shifts W 6 bits left and or's in the 6 xxxxxx bits.

      -------------
      -- Get_Hex --
      -------------

      procedure Get_Hex (N : Character) is
         B2 : constant Unsigned_32 := Character'Pos (N);
      begin
         if B2 in Character'Pos ('0') .. Character'Pos ('9') then
            B1 := B1 * 16 + B2 - Character'Pos ('0');
         elsif B2 in Character'Pos ('A') .. Character'Pos ('F') then
            B1 := B1 * 16 + B2 - (Character'Pos ('A') - 10);
         elsif B2 in Character'Pos ('a') .. Character'Pos ('f') then
            B1 := B1 * 16 + B2 - (Character'Pos ('a') - 10);
         else
            raise Constraint_Error;
         end if;
      end Get_Hex;

      ------------------
      -- Get_UTF_Byte --
      ------------------

      procedure Get_UTF_Byte is
      begin
         U := Unsigned_32 (Character'Pos (In_Char));

         if (U and 2#11000000#) /= 2#10_000000# then
            raise Constraint_Error;
         end if;

         W := Shift_Left (W, 6) or (U and 2#00111111#);
      end Get_UTF_Byte;

   --  Start of processing for Char_Sequence_To_UTF_32

   begin
      case EM is
         when WCEM_Hex =>
            if C /= ASCII.ESC then
               return Character'Pos (C);

            else
               B1 := 0;
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);

               return UTF_32_Code (B1);
            end if;

         when WCEM_Upper =>
            if C > ASCII.DEL then
               return 256 * Character'Pos (C) + Character'Pos (In_Char);
            else
               return Character'Pos (C);
            end if;

         when WCEM_Shift_JIS =>
            if C > ASCII.DEL then
               return Wide_Character'Pos (Shift_JIS_To_JIS (C, In_Char));
            else
               return Character'Pos (C);
            end if;

         when WCEM_EUC =>
            if C > ASCII.DEL then
               return Wide_Character'Pos (EUC_To_JIS (C, In_Char));
            else
               return Character'Pos (C);
            end if;

         when WCEM_UTF8 =>

            --  Note: for details of UTF8 encoding see RFC 3629

            U := Unsigned_32 (Character'Pos (C));

            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            if (U and 2#10000000#) = 2#00000000# then
               return Character'Pos (C);

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            elsif (U and 2#11100000#) = 2#110_00000# then
               W := U and 2#00011111#;
               Get_UTF_Byte;
               return UTF_32_Code (W);

            --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11110000#) = 2#1110_0000# then
               W := U and 2#00001111#;
               Get_UTF_Byte;
               Get_UTF_Byte;
               return UTF_32_Code (W);

            --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11111000#) = 2#11110_000# then
               W := U and 2#00000111#;

               for K in 1 .. 3 loop
                  Get_UTF_Byte;
               end loop;

               return UTF_32_Code (W);

            --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx

            elsif (U and 2#11111100#) = 2#111110_00# then
               W := U and 2#00000011#;

               for K in 1 .. 4 loop
                  Get_UTF_Byte;
               end loop;

               return UTF_32_Code (W);

            --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11111110#) = 2#1111110_0# then
               W := U and 2#00000001#;

               for K in 1 .. 5 loop
                  Get_UTF_Byte;
               end loop;

               return UTF_32_Code (W);

            else
               raise Constraint_Error;
            end if;

         when WCEM_Brackets =>
            if C /= '[' then
               return Character'Pos (C);
            end if;

            if In_Char /= '"' then
               raise Constraint_Error;
            end if;

            B1 := 0;
            Get_Hex (In_Char);
            Get_Hex (In_Char);

            C1 := In_Char;

            if C1 /= '"' then
               Get_Hex (C1);
               Get_Hex (In_Char);

               C1 := In_Char;

               if C1 /= '"' then
                  Get_Hex (C1);
                  Get_Hex (In_Char);

                  C1 := In_Char;

                  if C1 /= '"' then
                     Get_Hex (C1);
                     Get_Hex (In_Char);

                     if B1 > Unsigned_32 (UTF_32_Code'Last) then
                        raise Constraint_Error;
                     end if;

                     if In_Char /= '"' then
                        raise Constraint_Error;
                     end if;
                  end if;
               end if;
            end if;

            if In_Char /= ']' then
               raise Constraint_Error;
            end if;

            return UTF_32_Code (B1);
      end case;
   end Char_Sequence_To_UTF_32;

   --------------------------------
   -- Char_Sequence_To_Wide_Char --
   --------------------------------

   function Char_Sequence_To_Wide_Char
     (C  : Character;
      EM : System.WCh_Con.WC_Encoding_Method) return Wide_Character
   is
      function Char_Sequence_To_UTF is new Char_Sequence_To_UTF_32 (In_Char);

      U : constant UTF_32_Code := Char_Sequence_To_UTF (C, EM);

   begin
      if U > 16#FFFF# then
         raise Constraint_Error;
      else
         return Wide_Character'Val (U);
      end if;
   end Char_Sequence_To_Wide_Char;

   -----------------------------
   -- UTF_32_To_Char_Sequence --
   -----------------------------

   procedure UTF_32_To_Char_Sequence
     (Val : UTF_32_Code;
      EM  : System.WCh_Con.WC_Encoding_Method)
   is
      Hexc : constant array (UTF_32_Code range 0 .. 15) of Character :=
               "0123456789ABCDEF";

      C1, C2 : Character;
      U      : Unsigned_32;

   begin
      --  Raise CE for invalid UTF_32_Code

      if not Val'Valid then
         raise Constraint_Error;
      end if;

      --  Processing depends on encoding mode

      case EM is
         when WCEM_Hex =>
            if Val < 256 then
               Out_Char (Character'Val (Val));
            elsif Val <= 16#FFFF# then
               Out_Char (ASCII.ESC);
               Out_Char (Hexc (Val / (16**3)));
               Out_Char (Hexc ((Val / (16**2)) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
            else
               raise Constraint_Error;
            end if;

         when WCEM_Upper =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            elsif Val < 16#8000# or else Val > 16#FFFF# then
               raise Constraint_Error;
            else
               Out_Char (Character'Val (Val / 256));
               Out_Char (Character'Val (Val mod 256));
            end if;

         when WCEM_Shift_JIS =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            elsif Val <= 16#FFFF# then
               JIS_To_Shift_JIS (Wide_Character'Val (Val), C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            else
               raise Constraint_Error;
            end if;

         when WCEM_EUC =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            elsif Val <= 16#FFFF# then
               JIS_To_EUC (Wide_Character'Val (Val), C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            else
               raise Constraint_Error;
            end if;

         when WCEM_UTF8 =>

            --  Note: for details of UTF8 encoding see RFC 3629

            U := Unsigned_32 (Val);

            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            if U <= 16#00_007F# then
               Out_Char (Character'Val (U));

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            elsif U <= 16#00_07FF# then
               Out_Char (Character'Val (2#11000000# or Shift_Right (U, 6)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            --  16#00_0800#-16#00_FFFF#: 1110xxxx 10xxxxxx 10xxxxxx

            elsif U <= 16#00_FFFF# then
               Out_Char (Character'Val (2#11100000# or Shift_Right (U, 12)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

            elsif U <= 16#10_FFFF# then
               Out_Char (Character'Val (2#11110000# or Shift_Right (U, 18)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 12)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx

            elsif U <= 16#03FF_FFFF# then
               Out_Char (Character'Val (2#11111000# or Shift_Right (U, 24)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 18)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 12)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx 10xxxxxx

            elsif U <= 16#7FFF_FFFF# then
               Out_Char (Character'Val (2#11111100# or Shift_Right (U, 30)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 24)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 18)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 12)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                          and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            else
               raise Constraint_Error;
            end if;

         when WCEM_Brackets =>

            --  Values in the range 0-255 are directly output. Note that there
            --  is an issue with [ (16#5B#) since this will cause confusion
            --  if the resulting string is interpreted using brackets encoding.

            --  One possibility would be to always output [ as ["5B"] but in
            --  practice this is undesirable, since for example normal use of
            --  Wide_Text_IO for output (much more common than input), really
            --  does want to be able to say something like

            --     Put_Line ("Start of output [first run]");

            --  and have it come out as intended, rather than contaminated by
            --  a ["5B"] sequence in place of the left bracket.

            if Val < 256 then
               Out_Char (Character'Val (Val));

            --  Otherwise use brackets notation for vales greater than 255

            else
               Out_Char ('[');
               Out_Char ('"');

               if Val > 16#FFFF# then
                  if Val > 16#00FF_FFFF# then
                     Out_Char (Hexc (Val / 16 ** 7));
                     Out_Char (Hexc ((Val / 16 ** 6) mod 16));
                  end if;

                  Out_Char (Hexc ((Val / 16 ** 5) mod 16));
                  Out_Char (Hexc ((Val / 16 ** 4) mod 16));
               end if;

               Out_Char (Hexc ((Val / 16 ** 3) mod 16));
               Out_Char (Hexc ((Val / 16 ** 2) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));

               Out_Char ('"');
               Out_Char (']');
            end if;
      end case;
   end UTF_32_To_Char_Sequence;

   --------------------------------
   -- Wide_Char_To_Char_Sequence --
   --------------------------------

   procedure Wide_Char_To_Char_Sequence
     (WC : Wide_Character;
      EM : System.WCh_Con.WC_Encoding_Method)
   is
      procedure UTF_To_Char_Sequence is new UTF_32_To_Char_Sequence (Out_Char);
   begin
      UTF_To_Char_Sequence (Wide_Character'Pos (WC), EM);
   end Wide_Char_To_Char_Sequence;

end System.WCh_Cnv;
