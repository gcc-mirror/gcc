------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . S T R I N G S . U T F _ E N C O D I N G             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2010, Free Software Foundation, Inc.           --
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
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Unchecked_Conversion;

package body Ada.Strings.UTF_Encoding is

   function To_Unsigned_8 is new
     Unchecked_Conversion (Character, Unsigned_8);

   function To_Unsigned_16 is new
     Unchecked_Conversion (Wide_Character, Unsigned_16);

   function To_Unsigned_32 is new
     Unchecked_Conversion (Wide_Wide_Character, Unsigned_32);

   --  Local subprograms

   procedure Raise_Encoding_Error;
   --  Called if an invalid input encoding sequence is found by Decode

   function Decode_UTF_8 (Item : String) return Wide_String;
   --  Equivalent to Decode (Item, UTF_8), but smaller and faster

   function Decode_UTF_8 (Item : String) return Wide_Wide_String;
   --  Equivalent to Decode (Item, UTF_8), but smaller and faster

   function Encode_UTF_8 (Item : Wide_String) return String;
   --  Equivalent to Encode (Item, UTF_8) but smaller and faster

   function Encode_UTF_8 (Item : Wide_Wide_String) return String;
   --  Equivalent to Encode (Item, UTF_8) but smaller and faster

   function Decode_UTF_16 (Item : Wide_String) return Wide_String;
   --  Equivalent to Decode (Item, UTF_16)

   function Decode_UTF_16 (Item : Wide_String) return Wide_Wide_String;
   --  Equivalent to Decode (Item, UTF_16)

   function Encode_UTF_16 (Item : Wide_String) return Wide_String;
   --  Equivalent to Encode (Item, UTF_16)

   function Encode_UTF_16 (Item : Wide_Wide_String) return Wide_String;
   --  Equivalent to Encode (Item, UTF_16)

   ------------
   -- Decode --
   ------------

   --  String input with Wide_String output (short encodings)

   function Decode
     (Item   : String;
      Scheme : Short_Encoding := UTF_8) return Wide_String
   is
   begin
      --  UTF-8 encoding case

      if Scheme = UTF_8 then
         return Decode_UTF_8 (Item);

      --  Case of UTF_16LE or UTF_16BE

      else
         UTF16_XE : declare
            Input_UTF16 : Wide_String (1 .. Item'Length / 2);
            --  UTF_16 input string

            Iptr : Natural;
            --  Pointer to next location to store in Input_UTF16

            Ptr : Natural;
            --  Input string pointer

            H, L : Natural range 0 .. 1;
            --  Offset for high and low order bytes

         begin
            --  In both cases, the input string must be even in length, since
            --  we have two input characters for each input code in UTF_16.

            if Item'Length mod 2 /= 0 then
               Raise_Encoding_Error;
            end if;

            --  We first assemble the UTF_16 string from the input. Set offsets
            --  for the two bytes. For UTF_16LE we have low order/high order.
            --  For UTF_16BE we have high order/low order.

            if Scheme = UTF_16LE then
               L := 0;
               H := 1;
            else
               L := 1;
               H := 0;
            end if;

            --  Loop to convert input to UTF_16 form

            Iptr := 1;
            Ptr := Item'First;
            while Ptr < Item'Last loop
               Input_UTF16 (Iptr) :=
                 Wide_Character'Val
                   (Unsigned_16 (To_Unsigned_8 (Item (Ptr + L)))
                     or
                    Shift_Left
                      (Unsigned_16 (To_Unsigned_8 (Item (Ptr + H))), 8));
               Iptr := Iptr + 1;
               Ptr := Ptr + 2;
            end loop;

            --  Result is obtained by converting this UTF_16 input. Note that
            --  we rely on this nested call to Decode to skip any BOM present.

            return Decode (Input_UTF16);
         end UTF16_XE;
      end if;
   end Decode;

   --  String input with Wide_Wide_String output (short encodings)

   function Decode
     (Item   : String;
      Scheme : Short_Encoding := UTF_8) return Wide_Wide_String
   is
   begin
      --  UTF-8 encoding case

      if Scheme = UTF_8 then
         return Decode_UTF_8 (Item);

      --  Case of UTF_16LE or UTF_16BE

      else
         UTF16_XE : declare
            Input_UTF16 : Wide_String (1 .. Item'Length / 2);
            --  UTF_16 input string

            Iptr : Natural;
            --  Pointer to next location to store in Input_UTF16

            Ptr : Natural;
            --  Input string pointer

            H, L : Integer range 0 .. 1;
            --  Offset for high and low order bytes

         begin
            --  In both cases, the input string must be even in length, since
            --  we have two input characters for each input code in UTF_16.

            if Item'Length mod 2 /= 0 then
               Raise_Encoding_Error;
            end if;

            --  We first assemble the UTF_16 string from the input. Set offsets
            --  for the two bytes. For UTF_16LE we have low order/high order.
            --  For UTF_16BE we have high order/low order.

            if Scheme = UTF_16LE then
               L := 0;
               H := 1;
            else
               L := 1;
               H := 0;
            end if;

            --  Loop to convert input to UTF_16 form

            Ptr := Item'First;
            Iptr := 1;
            while Ptr < Item'Last loop
               Input_UTF16 (Iptr) :=
                 Wide_Character'Val
                   (Unsigned_16 (To_Unsigned_8 (Item (Ptr + L)))
                      or
                    Shift_Left
                      (Unsigned_16 (To_Unsigned_8 (Item (Ptr + H))), 8));
               Iptr := Iptr + 1;
               Ptr := Ptr + 2;
            end loop;

            --  Result is obtained by converting this UTF_16 input. Note that
            --  we rely on this nested call to Decode to skip any BOM present.

            return Decode_UTF_16 (Input_UTF16);
         end UTF16_XE;
      end if;
   end Decode;

   --  Wide_String input with Wide_Wide_String output (long encodings)

   function Decode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String
   is
      pragma Unreferenced (Scheme);
   begin
      return Decode_UTF_16 (Item);
   end Decode;

   --  Wide_String input with Wide_Wide_String output (long encodings)

   function Decode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_Wide_String
   is
      pragma Unreferenced (Scheme);
   begin
      return Decode_UTF_16 (Item);
   end Decode;

   -------------------
   -- Decode_UTF_16 --
   -------------------

   --  Version returning Wide_String result

   function Decode_UTF_16 (Item : Wide_String) return Wide_String is
      Result : Wide_String (1 .. Item'Length);
      --  Result is same length as input (possibly minus 1 if BOM present)

      Len : Natural := 0;
      --  Length of result

      Cod : Unsigned_16;
      J   : Positive;

   begin
      --  Skip UTF-16 BOM at start

      J := Item'First;

      if J <= Item'Last and then Item (J) = BOM_16 (1) then
         J := J + 1;
      end if;

      --  Loop through input characters

      while J <= Item'Last loop
         Cod := To_Unsigned_16 (Item (J));

         --  Codes in the range 16#0000#..16#D7FF# or 16#E000#..16#FFFF#
         --  represent their own value.

         if Cod <= 16#D7FF# or else Cod >= 16#E000# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Cod);

         --  Codes in the range 16#D800#..16#DBFF# represent the first of the
         --  two surrogates used to encode the range 16#01_000#..16#10_FFFF".
         --  Such codes are out of range for 16-bit output.

         --  The remaining case of input in the range 16#DC00#..16#DFFF# must
         --  never occur, since it means we have a second surrogate character
         --  with no corresponding first surrogate.

         --  Thus all remaining codes are invalid

         else
            Raise_Encoding_Error;
         end if;

         J := J + 1;
      end loop;

      return Result (1 .. Len);
   end Decode_UTF_16;

   --  Version returning Wide_Wide_String result

   function Decode_UTF_16 (Item : Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      --  Result cannot be longer than the input string

      Len : Natural := 0;
      --  Length of result

      Cod  : Unsigned_16;
      J    : Positive;
      Rcod : Unsigned_32;

   begin
      --  Skip UTF-16 BOM at start

      J := Item'First;

      if J <= Item'Last and then Item (J) = BOM_16 (1) then
         J := J + 1;
      end if;

      --  Loop through input characters

      while J <= Item'Last loop
         Cod := To_Unsigned_16 (Item (J));

         --  Codes in the range 16#0000#..16#D7FF# or 16#E000#..16#FFFF#
         --  represent their own value.

         if Cod <= 16#D7FF# or else Cod >= 16#E000# then
            Len := Len + 1;
            Result (Len) := Wide_Wide_Character'Val (Cod);

         --  Codes in the range 16#D800#..16#DBFF# represent the first of the
         --  two surrogates used to encode the range 16#01_000#..16#10_FFFF".

         elsif Cod <= 16#DBFF# then
            Rcod := (Unsigned_32 (Cod) - 16#D800#) * 2 ** 10;

            --  Error if at end of string

            if J = Item'Last then
               Raise_Encoding_Error;

            --  Otherwise next character must be valid low order surrogate

            else
               J := J + 1;
               Cod := To_Unsigned_16 (Item (J));

               if Cod < 16#DC00# or else Cod > 16#DFFF# then
                  Raise_Encoding_Error;

               else
                  Rcod := Rcod + (Unsigned_32 (Cod) mod 2 ** 10) + 16#01_0000#;
                  Len := Len + 1;
                  Result (Len) := Wide_Wide_Character'Val (Rcod);
               end if;
            end if;

         --  If input is in the range 16#DC00#..16#DFFF#, we have a second
         --  surrogate character with no corresponding first surrogate.

         else
            Raise_Encoding_Error;
         end if;

         J := J + 1;
      end loop;

      return Result (1 .. Len);
   end Decode_UTF_16;

   ------------------
   -- Decode_UTF_8 --
   ------------------

   --  Version returning Wide_String result

   function Decode_UTF_8 (Item : String) return Wide_String is
      Result : Wide_String (1 .. Item'Length);
      --  Result string (worst case is same length as input)

      Len : Natural := 0;
      --  Length of result stored so far

      Ptr : Natural;
      --  Input string pointer

      C : Unsigned_8;
      R : Unsigned_16;

      procedure Get_Continuation;
      --  Reads a continuation byte of the form 10xxxxxx, shifts R left
      --  by 6 bits, and or's in the xxxxxx to the low order 6 bits. On
      --  return Ptr is incremented. Raises exceptioon if continuation
      --  byte does not exist or is invalid.

      ----------------------
      -- Get_Continuation --
      ----------------------

      procedure Get_Continuation is
      begin
         if Ptr > Item'Last then
            Raise_Encoding_Error;

         else
            C := To_Unsigned_8 (Item (Ptr));
            Ptr := Ptr + 1;

            if C < 2#10_000000# or else C > 2#10_111111# then
               Raise_Encoding_Error;

            else
               R := Shift_Left (R, 6) or
                      Unsigned_16 (C and 2#00_111111#);
            end if;
         end if;
      end Get_Continuation;

   --  Start of processing for Decode_UTF_8

   begin
      Ptr := Item'First;

      --  Skip BOM at start

      if Ptr + 2 <= Item'Last
        and then Item (Ptr .. Ptr + 2) = BOM_8
      then
         Ptr := Ptr + 3;
      end if;

      --  Loop through input characters

      while Ptr <= Item'Last loop
         C := To_Unsigned_8 (Item (Ptr));
         Ptr := Ptr + 1;

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            R := Unsigned_16 (C);

         --  No initial code can be of the form 10xxxxxx. Such codes are used
         --  only for continuations.

         elsif C <= 2#10_111111# then
            Raise_Encoding_Error;

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 2#110_11111# then
            R := Unsigned_16 (C and 2#000_11111#);
            Get_Continuation;

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         elsif C <= 2#1110_1111# then
            R := Unsigned_16 (C and 2#0000_1111#);
            Get_Continuation;
            Get_Continuation;

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         --  Such codes are out of range for Wide_String output

         else
            Raise_Encoding_Error;
         end if;

         Len := Len + 1;
         Result (Len) := Wide_Character'Val (R);
      end loop;

      return Result (1 .. Len);
   end Decode_UTF_8;

   --  Version returning Wide_Wide_String result

   function Decode_UTF_8 (Item : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      --  Result string (worst case is same length as input)

      Len : Natural := 0;
      --  Length of result stored so far

      Ptr : Natural;
      --  Input string pointer

      C : Unsigned_8;
      R : Unsigned_32;

      procedure Get_Continuation;
      --  Reads a continuation byte of the form 10xxxxxx, shifts R left
      --  by 6 bits, and or's in the xxxxxx to the low order 6 bits. On
      --  return Ptr is incremented. Raises exceptioon if continuation
      --  byte does not exist or is invalid.

      ----------------------
      -- Get_Continuation --
      ----------------------

      procedure Get_Continuation is
      begin
         if Ptr > Item'Last then
            raise Encoding_Error with
              "incomplete UTF-8 encoding sequence";

         else
            C := To_Unsigned_8 (Item (Ptr));
            Ptr := Ptr + 1;

            if C < 2#10_000000# or else C > 2#10_111111# then
               Raise_Encoding_Error;

            else
               R := Shift_Left (R, 6) or
                 Unsigned_32 (C and 2#00_111111#);
            end if;
         end if;
      end Get_Continuation;

   --  Start of processing for UTF8_Decode

   begin
      Ptr := Item'First;

      --  Skip BOM at start

      if Ptr + 2 <= Item'Last
        and then Item (Ptr .. Ptr + 2) = BOM_8
      then
         Ptr := Ptr + 3;
      end if;

      --  Loop through input characters

      while Ptr <= Item'Last loop
         C := To_Unsigned_8 (Item (Ptr));
         Ptr := Ptr + 1;

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            R := Unsigned_32 (C);

         --  No initial code can be of the form 10xxxxxx. Such codes are used
         --  only for continuations.

         elsif C <= 2#10_111111# then
            Raise_Encoding_Error;

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 2#110_11111# then
            R := Unsigned_32 (C and 2#000_11111#);
            Get_Continuation;

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         elsif C <= 2#1110_1111# then
            R := Unsigned_32 (C and 2#0000_1111#);
            Get_Continuation;
            Get_Continuation;

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         elsif C <= 2#11110_111# then
            R := Unsigned_32 (C and 2#00000_111#);
            Get_Continuation;
            Get_Continuation;
            Get_Continuation;

         --  Any other code is an error

         else
            Raise_Encoding_Error;
         end if;

         Len := Len + 1;
         Result (Len) := Wide_Wide_Character'Val (R);
      end loop;

      return Result (1 .. Len);
   end Decode_UTF_8;

   ------------
   -- Encode --
   ------------

   --  Version with Wide_String input returning encoded String

   function Encode
     (Item   : Wide_String;
      Scheme : Short_Encoding := UTF_8) return String
   is
   begin
      --  Case of UTF_8

      if Scheme = UTF_8 then
         return Encode_UTF_8 (Item);

      --  Case of UTF_16LE or UTF_16BE

      else
         UTF16XE_Encode : declare
            UTF16_Str : constant Wide_String := Encode_UTF_16 (Item);
            Result    : String (1 .. 2 * UTF16_Str'Last);

            H, L : Integer range -1 .. 0;
            --  Offset for high and low order bytes

            C : Unsigned_16;
            --  One UTF_16 output value

         begin
            --  Set proper byte offsets

            --  Set the byte order for the two bytes of each UTF_16 input code.
            --  For UTF_16LE we have low order/high order. For UTF_16BE we have
            --  high order/low order.

            if Scheme = UTF_16LE then
               L := -1;
               H := 0;
            else
               L := 0;
               H := -1;
            end if;

            --  Now copy the UTF_16 string to the result string

            pragma Warnings (Off);
            for J in 1 .. UTF16_Str'Last loop
               C := To_Unsigned_16 (UTF16_Str (J));
               Result (2 * J + L) := Character'Val (C and 16#FF#);
               Result (2 * J + H) := Character'Val (Shift_Right (C, 8));
            end loop;

            return Result;
         end UTF16XE_Encode;
      end if;
   end Encode;

   --  Version with Wide_Wide_String input returning String

   function Encode
     (Item   : Wide_Wide_String;
      Scheme : Short_Encoding := UTF_8) return String
   is
   begin
      --  Case of UTF_8

      if Scheme = UTF_8 then
         return Encode_UTF_8 (Item);

      --  Case of UTF_16LE or UTF_16BE

      else
         UTF16XE_Encode : declare
            UTF16_Str : constant Wide_String := Encode (Item, UTF_16);
            Result    : String (1 .. 2 * UTF16_Str'Last);

            H, L : Integer range -1 .. 0;
            --  Offset for high and low order bytes

            C : Unsigned_16;
            --  One UTF_16 output value

         begin
            --  Set proper byte offsets

            --  Set the byte order for the two bytes of each UTF_16 input code.
            --  For UTF_16LE we have low order/high order. For UTF_16BE we have
            --  high order/low order.

            if Scheme = UTF_16LE then
               L := -1;
               H := 0;
            else
               L := 0;
               H := -1;
            end if;

            --  Now copy the UTF_16 string to the result string

            for J in 1 .. UTF16_Str'Last loop
               C := To_Unsigned_16 (UTF16_Str (J));
               Result (2 * J + L) := Character'Val (C and 16#FF#);
               Result (2 * J + H) := Character'Val (Shift_Right (C, 8));
            end loop;

            return Result;
         end UTF16XE_Encode;
      end if;
   end Encode;

   --  Wide_String input returning encoded Wide_String (long encodings)

   function Encode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String
   is
      pragma Unreferenced (Scheme);
   begin
      return Encode_UTF_16 (Item);
   end Encode;

   --  Wide_Wide_String input returning Wide_String (long encodings)

   function Encode
     (Item   : Wide_Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String
   is
      pragma Unreferenced (Scheme);
   begin
      return Encode_UTF_16 (Item);
   end Encode;

   -------------------
   -- Encode_UTF_16 --
   -------------------

   --  Wide_String input with UTF-16 encoded Wide_String output

   function Encode_UTF_16 (Item : Wide_String) return Wide_String is
      Result : Wide_String (1 .. Item'Length);
      --  Output is same length as input (we do not add a BOM!)

      Len : Integer := 0;
      --  Length of output string

      Cod : Unsigned_16;

   begin
      --  Loop through input characters encoding them

      for J in Item'Range loop
         Cod := To_Unsigned_16 (Item (J));

         --  Codes in the range 16#0000#..16#D7FF# are output unchanged

         if Cod <= 16#D7FF# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Cod);

         --  Codes in tne range 16#D800#..16#DFFF# should never appear in the
         --  input, since no valid Unicode characters are in this range (which
         --  would conflict with the UTF-16 surrogate encodings).

         elsif Cod <= 16#DFFF# then
            raise Constraint_Error with
              "Wide_Character in range 16#D800# .. 16#DFFF#";

         --  Codes in the range 16#E000#..16#FFFF# are output unchanged

         else
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Cod);
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode_UTF_16;

   --  Wide_Wide_String input with UTF-16 encoded Wide_String output

   function Encode_UTF_16 (Item : Wide_Wide_String) return Wide_String is
      Result : Wide_String (1 .. 2 * Item'Length);
      --  Worst case is each input character generates two output characters

      Len : Integer := 0;
      --  Length of output string

      Cod : Unsigned_32;

   begin
      --  Loop through input characters encoding them

      for J in Item'Range loop
         Cod := To_Unsigned_32 (Item (J));

         --  Codes in the range 16#00_0000#..16#00_D7FF# are output unchanged

         if Cod <= 16#00_D7FF# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Cod);

         --  Codes in tne range 16#00_D800#..16#00_DFFF# should never appear
         --  in the input, since no valid Unicode characters are in this range
         --  (which would conflict with the UTF-16 surrogate encodings).

         elsif Cod <= 16#00_DFFF# then
            raise Constraint_Error with
              "Wide_Wide_Character in range 16#00_D800# .. 16#00_DFFF#";

         --  Codes in the range 16#00_E000#..16#00_FFFF# are output unchanged

         elsif Cod <= 16#00_FFFF# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Cod);

         --  Codes in the range 16#01_0000#..16#10_FFFF# are output using two
         --  surrogate characters. First 16#1_0000# is subtracted from the code
         --  point to give a 20-bit value. This is then split into two separate
         --  10-bit values each of which is represented as a surrogate with the
         --  most significant half placed in the first surrogate. To allow safe
         --  use of simple word-oriented string processing, separate ranges of
         --  values are used for the two surrogates: 16#D800#-16#DBFF# for the
         --  first, most significant surrogate and 16#DC00#-16#DFFF# for the
         --  second, least significant surrogate.

         elsif Cod <= 16#10_FFFF# then
            Cod := Cod - 16#1_0000#;

            Len := Len + 1;
            Result (Len) := Wide_Character'Val (16#D800# + Cod / 2 ** 10);

            Len := Len + 1;
            Result (Len) := Wide_Character'Val (16#DC00# + Cod mod 2 ** 10);

         --  Codes larger than 16#10_FFFF# are invalid

         else
            raise Constraint_Error with
              "Wide_Wide_Character exceeds maximum value of 16#10_FFFF#";
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode_UTF_16;

   ------------------
   -- Encode_UTF_8 --
   ------------------

   --  Wide_String input with UTF_8 encoded String output

   function Encode_UTF_8 (Item : Wide_String) return String is
      Result : String (1 .. 3 * Item'Length);
      --  Worst case is three bytes per input byte

      N : Natural := 0;
      --  Number of output codes stored in Result

      C : Unsigned_16;
      --  Single input character

      procedure Store (C : Unsigned_16);
      pragma Inline (Store);
      --  Store one output code, C is in the range 0 .. 255

      -----------
      -- Store --
      -----------

      procedure Store (C : Unsigned_16) is
      begin
         N := N + 1;
         Result (N) := Character'Val (C);
      end Store;

   --  Start of processing for UTF8_Encode

   begin
      --  Loop through characters of input

      for J in Item'Range loop
         C := To_Unsigned_16 (Item (J));

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            Store (C);

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 16#7FF# then
            Store (2#110_00000# or Shift_Right (C, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         else
            Store (2#1110_0000# or Shift_Right (C, 12));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000#, 6));
            Store (2#10_000000# or (C and 2#00_111111#));
         end if;
      end loop;

      return Result (1 .. N);
   end Encode_UTF_8;

   --  Wide_Wide_String input with UTF_8 encoded String output

   function Encode_UTF_8 (Item : Wide_Wide_String) return String is
      Result : String (1 .. 4 * Item'Length);
      --  Worst case is four bytes per input byte

      N  : Natural := 0;
      --  Number of output codes stored in Result

      C : Unsigned_32;
      --  Single input character

      procedure Store (C : Unsigned_32);
      pragma Inline (Store);
      --  Store one output code (input is in range 0 .. 255)

      -----------
      -- Store --
      -----------

      procedure Store (C : Unsigned_32) is
      begin
         N := N + 1;
         Result (N) := Character'Val (C);
      end Store;

   --  Start of processing for UTF8_Encode

   begin
      --  Loop through characters of input

      for J in Item'Range loop
         C := To_Unsigned_32 (Item (J));

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            Store (C);

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 16#7FF# then
            Store (2#110_00000# or Shift_Right (C, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         elsif C <= 16#FFFF# then
            Store (2#1110_0000# or Shift_Right (C, 12));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000#, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         elsif C <= 16#10_FFFF# then
            Store (2#11110_000# or Shift_Right (C, 18));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000_000000#, 12));
            Store (2#10_000000#
                   or Shift_Right (C and 2#111111_000000#, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes higher than 16#10_FFFF# should not appear

         else
            raise Constraint_Error with
              "out of range invalid value in Encode input";
         end if;
      end loop;

      return Result (1 .. N);
   end Encode_UTF_8;

   --------------
   -- Encoding --
   --------------

   --  Version taking String input

   function Encoding (Item : String) return Encoding_Scheme is
   begin
      if Item'Length >= 2 then
         if Item (Item'First .. Item'First + 1) = BOM_16BE then
            return UTF_16BE;

         elsif Item (Item'First .. Item'First + 1) = BOM_16LE then
            return UTF_16LE;

         elsif Item'Length >= 3
           and then Item (Item'First .. Item'First + 2) = BOM_8
         then
            return UTF_8;
         end if;
      end if;

      return UTF_None;
   end Encoding;

   --  Version taking Wide_String input

   function Encoding (Item : Wide_String) return Encoding_Scheme is
   begin
      if Item'Length >= 1
        and then Item (Item'First .. Item'First) = BOM_16
      then
         return UTF_16;
      else
         return UTF_None;
      end if;
   end Encoding;

   ------------------------
   -- Raise_Encoding_Error --
   ------------------------

   procedure Raise_Encoding_Error is
   begin
      raise Encoding_Error with "invalid input encoding sequence";
   end Raise_Encoding_Error;

end Ada.Strings.UTF_Encoding;
