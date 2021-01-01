------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.STRINGS.UTF_ENCODING.CONVERSIONS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2021, Free Software Foundation, Inc.         --
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

package body Ada.Strings.UTF_Encoding.Conversions is
   use Interfaces;

   --  Convert from UTF-8/UTF-16BE/LE to UTF-8/UTF-16BE/LE

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String
   is
   begin
      --  Nothing to do if identical schemes, but for UTF_8 we need to
      --  handle overlong encodings, so need to do the full conversion.

      if Input_Scheme = Output_Scheme
        and then Input_Scheme /= UTF_8
      then
         return Item;

      --  For remaining cases, one or other of the operands is UTF-16BE/LE
      --  encoded, or we have the UTF-8 to UTF-8 case where we must handle
      --  overlong encodings. In all cases,  go through UTF-16 intermediate.

      else
         return Convert (UTF_16_Wide_String'(Convert (Item, Input_Scheme)),
                         Output_Scheme, Output_BOM);
      end if;
   end Convert;

   --  Convert from UTF-8/UTF-16BE/LE to UTF-16

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String
   is
   begin
      if Input_Scheme = UTF_8 then
         return Convert (Item, Output_BOM);
      else
         return To_UTF_16 (Item, Input_Scheme, Output_BOM);
      end if;
   end Convert;

   --  Convert from UTF-8 to UTF-16

   function Convert
     (Item       : UTF_8_String;
      Output_BOM : Boolean := False) return UTF_16_Wide_String
   is
      Result : UTF_16_Wide_String (1 .. Item'Length + 1);
      --  Maximum length of result, including possible BOM

      Len : Natural := 0;
      --  Number of characters stored so far in Result

      Iptr : Natural;
      --  Next character to process in Item

      C : Unsigned_8;
      --  Input UTF-8 code

      R : Unsigned_16;
      --  Output UTF-16 code

      procedure Get_Continuation;
      --  Reads a continuation byte of the form 10xxxxxx, shifts R left by 6
      --  bits, and or's in the xxxxxx to the low order 6 bits. On return Ptr
      --  is incremented. Raises exception if continuation byte does not exist
      --  or is invalid.

      ----------------------
      -- Get_Continuation --
      ----------------------

      procedure Get_Continuation is
      begin
         if Iptr > Item'Last then
            Raise_Encoding_Error (Iptr - 1);

         else
            C := To_Unsigned_8 (Item (Iptr));
            Iptr := Iptr + 1;

            if C < 2#10_000000# or else C > 2#10_111111# then
               Raise_Encoding_Error (Iptr - 1);

            else
               R :=
                 Shift_Left (R, 6) or Unsigned_16 (C and 2#00_111111#);
            end if;
         end if;
      end Get_Continuation;

   --  Start of processing for Convert

   begin
      --  Output BOM if required

      if Output_BOM then
         Len := Len + 1;
         Result (Len) := BOM_16 (1);
      end if;

      --  Skip OK BOM

      Iptr := Item'First;

      if Item'Length >= 3 and then Item (Iptr .. Iptr + 2) = BOM_8 then
         Iptr := Iptr + 3;

      --  Error if bad BOM

      elsif Item'Length >= 2
        and then (Item (Iptr .. Iptr + 1) = BOM_16BE
                    or else
                  Item (Iptr .. Iptr + 1) = BOM_16LE)
      then
         Raise_Encoding_Error (Iptr);

      --  No BOM present

      else
         Iptr := Item'First;
      end if;

      while Iptr <= Item'Last loop
         C := To_Unsigned_8 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#00# .. 16#7F#
         --    UTF-8:  0xxxxxxx
         --    UTF-16: 00000000_0xxxxxxx

         if C <= 16#7F# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (C);

         --  No initial code can be of the form 10xxxxxx. Such codes are used
         --  only for continuations.

         elsif C <= 2#10_111111# then
            Raise_Encoding_Error (Iptr - 1);

         --  Codes in the range 16#80# .. 16#7FF#
         --    UTF-8:  110yyyxx 10xxxxxx
         --    UTF-16: 00000yyy_xxxxxxxx

         elsif C <= 2#110_11111# then
            R := Unsigned_16 (C and 2#000_11111#);
            Get_Continuation;
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (R);

         --  Codes in the range 16#800# .. 16#D7FF or 16#DF01# .. 16#FFFF#
         --    UTF-8:  1110yyyy 10yyyyxx 10xxxxxx
         --    UTF-16: yyyyyyyy_xxxxxxxx

         elsif C <= 2#1110_1111# then
            R := Unsigned_16 (C and 2#0000_1111#);
            Get_Continuation;
            Get_Continuation;
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (R);

            --  Make sure that we don't have a result in the forbidden range
            --  reserved for UTF-16 surrogate characters.

            if R in 16#D800# .. 16#DF00# then
               Raise_Encoding_Error (Iptr - 3);
            end if;

         --  Codes in the range 16#10000# .. 16#10FFFF#
         --    UTF-8:  11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
         --    UTF-16: 110110zz_zzyyyyyy 110111yy_xxxxxxxx
         --    Note: zzzz in the output is input zzzzz - 1

         elsif C <= 2#11110_111# then
            R := Unsigned_16 (C and 2#00000_111#);
            Get_Continuation;

            --  R now has zzzzzyyyy

            --  At this stage, we check for the case where we have an overlong
            --  encoding, and the encoded value in fact lies in the single word
            --  range (16#800# .. 16#D7FF or 16#DF01# .. 16#FFFF#). This means
            --  that the result fits in a single result word.

            if R <= 2#1111# then
               Get_Continuation;
               Get_Continuation;

               --  Make sure we are not in the forbidden surrogate range

               if R in 16#D800# .. 16#DF00# then
                  Raise_Encoding_Error (Iptr - 3);
               end if;

               --  Otherwise output a single UTF-16 value

               Len := Len + 1;
               Result (Len) := Wide_Character'Val (R);

            --  Here for normal case (code value > 16#FFFF and zzzzz non-zero)

            else
               --  Subtract 1 from input zzzzz value to get output zzzz value

               R := R - 2#0000_1_0000#;

               --  R now has zzzzyyyy (zzzz minus one for the output)

               Get_Continuation;

               --  R now has zzzzyy_yyyyyyxx

               Len := Len + 1;
               Result (Len) :=
                 Wide_Character'Val
                   (2#110110_00_0000_0000# or Shift_Right (R, 4));

               R := R and 2#1111#;
               Get_Continuation;
               Len := Len + 1;
               Result (Len) :=
                 Wide_Character'Val (2#110111_00_0000_0000# or R);
            end if;

         --  Any other code is an error

         else
            Raise_Encoding_Error (Iptr - 1);
         end if;
      end loop;

      return Result (1 .. Len);
   end Convert;

   --  Convert from UTF-16 to UTF-8/UTF-16-BE/LE

   function Convert
     (Item          : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String
   is
   begin
      if Output_Scheme = UTF_8 then
         return Convert (Item, Output_BOM);
      else
         return From_UTF_16 (Item, Output_Scheme, Output_BOM);
      end if;
   end Convert;

   --  Convert from UTF-16 to UTF-8

   function Convert
     (Item          : UTF_16_Wide_String;
      Output_BOM    : Boolean := False) return UTF_8_String
   is
      Result : UTF_8_String (1 .. 3 * Item'Length + 3);
      --  Worst case is 3 output codes for each input code + BOM space

      Len : Natural;
      --  Number of result codes stored

      Iptr : Natural;
      --  Pointer to next input character

      C1, C2 : Unsigned_16;

      zzzzz    : Unsigned_16;
      yyyyyyyy : Unsigned_16;
      xxxxxxxx : Unsigned_16;
      --  Components of double length case

   begin
      Iptr := Item'First;

      --  Skip BOM at start of input

      if Item'Length > 0 and then Item (Iptr) = BOM_16 (1) then
         Iptr := Iptr + 1;
      end if;

      --  Generate output BOM if required

      if Output_BOM then
         Result (1 .. 3) := BOM_8;
         Len := 3;
      else
         Len := 0;
      end if;

      --  Loop through input

      while Iptr <= Item'Last loop
         C1 := To_Unsigned_16 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#0000# - 16#007F#
         --    UTF-16: 000000000xxxxxxx
         --    UTF-8:  0xxxxxxx

         if C1 <= 16#007F# then
            Result (Len + 1) := Character'Val (C1);
            Len := Len + 1;

         --  Codes in the range 16#80# - 16#7FF#
         --    UTF-16: 00000yyyxxxxxxxx
         --    UTF-8:  110yyyxx 10xxxxxx

         elsif C1 <= 16#07FF# then
            Result (Len + 1) :=
              Character'Val
                (2#110_00000# or Shift_Right (C1, 6));
            Result (Len + 2) :=
              Character'Val
                (2#10_000000# or (C1 and 2#00_111111#));
            Len := Len + 2;

         --  Codes in the range 16#800# - 16#D7FF# or 16#E000# - 16#FFFF#
         --    UTF-16: yyyyyyyyxxxxxxxx
         --    UTF-8:  1110yyyy 10yyyyxx 10xxxxxx

         elsif C1 <= 16#D7FF# or else C1 >= 16#E000# then
            Result (Len + 1) :=
              Character'Val
                (2#1110_0000# or Shift_Right (C1, 12));
            Result (Len + 2) :=
              Character'Val
                (2#10_000000# or (Shift_Right (C1, 6) and 2#00_111111#));
            Result (Len + 3) :=
              Character'Val
                (2#10_000000# or (C1 and 2#00_111111#));
            Len := Len + 3;

         --  Codes in the range 16#10000# - 16#10FFFF#
         --    UTF-16: 110110zzzzyyyyyy 110111yyxxxxxxxx
         --    UTF-8:  11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
         --    Note: zzzzz in the output is input zzzz + 1

         elsif C1 <= 2#110110_11_11111111# then
            if Iptr > Item'Last then
               Raise_Encoding_Error (Iptr - 1);
            else
               C2 := To_Unsigned_16 (Item (Iptr));
               Iptr := Iptr + 1;
            end if;

            if (C2 and 2#111111_00_00000000#) /= 2#110111_00_00000000# then
               Raise_Encoding_Error (Iptr - 1);
            end if;

            zzzzz    := (Shift_Right (C1, 6) and 2#1111#) + 1;
            yyyyyyyy := ((Shift_Left (C1, 2) and 2#111111_00#)
                            or
                         (Shift_Right (C2, 8) and 2#000000_11#));
            xxxxxxxx := C2 and 2#11111111#;

            Result (Len + 1) :=
              Character'Val
                (2#11110_000# or (Shift_Right (zzzzz, 2)));
            Result (Len + 2) :=
              Character'Val
                (2#10_000000# or Shift_Left (zzzzz and 2#11#, 4)
                              or Shift_Right (yyyyyyyy, 4));
            Result (Len + 3) :=
              Character'Val
                (2#10_000000# or Shift_Left (yyyyyyyy and 2#1111#, 2)
                              or Shift_Right (xxxxxxxx, 6));
            Result (Len + 4) :=
              Character'Val
                (2#10_000000# or (xxxxxxxx and 2#00_111111#));
            Len := Len + 4;

         --  Error if input in 16#DC00# - 16#DFFF# (2nd surrogate with no 1st)

         else
            Raise_Encoding_Error (Iptr - 2);
         end if;
      end loop;

      return Result (1 .. Len);
   end Convert;

end Ada.Strings.UTF_Encoding.Conversions;
