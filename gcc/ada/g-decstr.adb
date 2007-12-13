------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . D E C O D E _ S T R I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a utility routine for converting from an encoded
--  string to a corresponding Wide_String or Wide_Wide_String value.

with Interfaces; use Interfaces;

with System.WCh_Cnv; use System.WCh_Cnv;
with System.WCh_Con; use System.WCh_Con;

package body GNAT.Decode_String is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Bad;
   pragma No_Return (Bad);
   --  Raise error for bad encoding

   procedure Past_End;
   pragma No_Return (Past_End);
   --  Raise error for off end of string

   ---------
   -- Bad --
   ---------

   procedure Bad is
   begin
      raise Constraint_Error with
        "bad encoding or character out of range";
   end Bad;

   ---------------------------
   -- Decode_Wide_Character --
   ---------------------------

   procedure Decode_Wide_Character
     (Input  : String;
      Ptr    : in out Natural;
      Result : out Wide_Character)
   is
      Char : Wide_Wide_Character;
   begin
      Decode_Wide_Wide_Character (Input, Ptr, Char);

      if Wide_Wide_Character'Pos (Char) > 16#FFFF# then
         Bad;
      else
         Result := Wide_Character'Val (Wide_Wide_Character'Pos (Char));
      end if;
   end Decode_Wide_Character;

   ------------------------
   -- Decode_Wide_String --
   ------------------------

   function Decode_Wide_String (S : String) return Wide_String is
      Result : Wide_String (1 .. S'Length);
      Length : Natural;
   begin
      Decode_Wide_String (S, Result, Length);
      return Result (1 .. Length);
   end Decode_Wide_String;

   procedure Decode_Wide_String
     (S      : String;
      Result : out Wide_String;
      Length : out Natural)
   is
      Ptr : Natural;

   begin
      Ptr := S'First;
      Length := 0;
      while Ptr <= S'Last loop
         if Length >= Result'Last then
            Past_End;
         end if;

         Length := Length + 1;
         Decode_Wide_Character (S, Ptr, Result (Length));
      end loop;
   end Decode_Wide_String;

   --------------------------------
   -- Decode_Wide_Wide_Character --
   --------------------------------

   procedure Decode_Wide_Wide_Character
     (Input  : String;
      Ptr    : in out Natural;
      Result : out Wide_Wide_Character)
   is
      C : Character;

      function In_Char return Character;
      pragma Inline (In_Char);
      --  Function to get one input character

      -------------
      -- In_Char --
      -------------

      function In_Char return Character is
      begin
         if Ptr <= Input'Last then
            Ptr := Ptr + 1;
            return Input (Ptr - 1);
         else
            Past_End;
         end if;
      end In_Char;

   --  Start of processing for Decode_Wide_Wide_Character

   begin
      C := In_Char;

      --  Special fast processing for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then
         UTF8 : declare
            U : Unsigned_32;
            W : Unsigned_32;

            procedure Get_UTF_Byte;
            pragma Inline (Get_UTF_Byte);
            --  Used to interpret 2#10xxxxxx# continuation byte in UTF-8 mode.
            --  Reads a byte, and raises CE if the first two bits are not 10.
            --  Otherwise shifts W 6 bits left and or's in the 6 xxxxxx bits.

            ------------------
            -- Get_UTF_Byte --
            ------------------

            procedure Get_UTF_Byte is
            begin
               U := Unsigned_32 (Character'Pos (In_Char));

               if (U and 2#11000000#) /= 2#10_000000# then
                  Bad;
               end if;

               W := Shift_Left (W, 6) or (U and 2#00111111#);
            end Get_UTF_Byte;

         --  Start of processing for UTF8 case

         begin
            --  Note: for details of UTF8 encoding see RFC 3629

            U := Unsigned_32 (Character'Pos (C));

            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            if (U and 2#10000000#) = 2#00000000# then
               Result := Wide_Wide_Character'Val (Character'Pos (C));

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            elsif (U and 2#11100000#) = 2#110_00000# then
               W := U and 2#00011111#;
               Get_UTF_Byte;
               Result := Wide_Wide_Character'Val (W);

            --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11110000#) = 2#1110_0000# then
               W := U and 2#00001111#;
               Get_UTF_Byte;
               Get_UTF_Byte;
               Result := Wide_Wide_Character'Val (W);

            --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11111000#) = 2#11110_000# then
               W := U and 2#00000111#;

               for K in 1 .. 3 loop
                  Get_UTF_Byte;
               end loop;

               Result := Wide_Wide_Character'Val (W);

            --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx

            elsif (U and 2#11111100#) = 2#111110_00# then
               W := U and 2#00000011#;

               for K in 1 .. 4 loop
                  Get_UTF_Byte;
               end loop;

               Result := Wide_Wide_Character'Val (W);

            --  All other cases are invalid, note that this includes:

            --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx 10xxxxxx

            --  since Wide_Wide_Character does not include code values
            --  greater than 16#03FF_FFFF#.

            else
               Bad;
            end if;
         end UTF8;

      --  All encoding functions other than UTF-8

      else
         Non_UTF8 : declare
            function Char_Sequence_To_UTF is
              new Char_Sequence_To_UTF_32 (In_Char);

         begin
            --  For brackets, must test for specific case of [ not followed by
            --  quotation, where we must not call Char_Sequence_To_UTF, but
            --  instead just return the bracket unchanged.

            if Encoding_Method = WCEM_Brackets
              and then C = '['
              and then (Ptr > Input'Last or else Input (Ptr) /= '"')
            then
               Result := '[';

            --  All other cases including [" with Brackets

            else
               Result :=
                 Wide_Wide_Character'Val
                   (Char_Sequence_To_UTF (C, Encoding_Method));
            end if;
         end Non_UTF8;
      end if;
   end Decode_Wide_Wide_Character;

   -----------------------------
   -- Decode_Wide_Wide_String --
   -----------------------------

   function Decode_Wide_Wide_String (S : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. S'Length);
      Length : Natural;
   begin
      Decode_Wide_Wide_String (S, Result, Length);
      return Result (1 .. Length);
   end Decode_Wide_Wide_String;

   procedure Decode_Wide_Wide_String
     (S      : String;
      Result : out Wide_Wide_String;
      Length : out Natural)
   is
      Ptr : Natural;

   begin
      Ptr := S'First;
      Length := 0;
      while Ptr <= S'Last loop
         if Length >= Result'Last then
            Past_End;
         end if;

         Length := Length + 1;
         Decode_Wide_Wide_Character (S, Ptr, Result (Length));
      end loop;
   end Decode_Wide_Wide_String;

   -------------------------
   -- Next_Wide_Character --
   -------------------------

   procedure Next_Wide_Character (Input : String; Ptr : in out Natural) is
   begin
      if Ptr < Input'First then
         Past_End;
      end if;

      --  Special efficient encoding for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then
         UTF8 : declare
            U : Unsigned_32;

            procedure Getc;
            pragma Inline (Getc);
            --  Gets the character at Input (Ptr) and returns code in U as
            --  Unsigned_32 value. On return Ptr is bumped past the character.

            procedure Skip_UTF_Byte;
            pragma Inline (Skip_UTF_Byte);
            --  Skips past one encoded byte which must be 2#10xxxxxx#

            ----------
            -- Getc --
            ----------

            procedure Getc is
            begin
               if Ptr > Input'Last then
                  Past_End;
               else
                  U := Unsigned_32 (Character'Pos (Input (Ptr)));
                  Ptr := Ptr + 1;
               end if;
            end Getc;

            -------------------
            -- Skip_UTF_Byte --
            -------------------

            procedure Skip_UTF_Byte is
            begin
               Getc;

               if (U and 2#11000000#) /= 2#10_000000# then
                  Bad;
               end if;
            end Skip_UTF_Byte;

         --  Start of processing for UTF-8 case

         begin
            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            Getc;

            if (U and 2#10000000#) = 2#00000000# then
               return;

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            elsif (U and 2#11100000#) = 2#110_00000# then
               Skip_UTF_Byte;

            --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11110000#) = 2#1110_0000# then
               Skip_UTF_Byte;
               Skip_UTF_Byte;

            --  Any other code is invalid, note that this includes:

            --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

            --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx

            --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx 10xxxxxx

            --  since Wide_Character does not allow codes > 16#FFFF#

            else
               Bad;
            end if;
         end UTF8;

      --  Non-UTF-8 cass

      else
         declare
            Discard : Wide_Character;
         begin
            Decode_Wide_Character (Input, Ptr, Discard);
         end;
      end if;
   end Next_Wide_Character;

   ------------------------------
   -- Next_Wide_Wide_Character --
   ------------------------------

   procedure Next_Wide_Wide_Character (Input : String; Ptr : in out Natural) is
   begin
      --  Special efficient encoding for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then
         UTF8 : declare
            U : Unsigned_32;

            procedure Getc;
            pragma Inline (Getc);
            --  Gets the character at Input (Ptr) and returns code in U as
            --  Unsigned_32 value. On return Ptr is bumped past the character.

            procedure Skip_UTF_Byte;
            pragma Inline (Skip_UTF_Byte);
            --  Skips past one encoded byte which must be 2#10xxxxxx#

            ----------
            -- Getc --
            ----------

            procedure Getc is
            begin
               if Ptr > Input'Last then
                  Past_End;
               else
                  U := Unsigned_32 (Character'Pos (Input (Ptr)));
                  Ptr := Ptr + 1;
               end if;
            end Getc;

            -------------------
            -- Skip_UTF_Byte --
            -------------------

            procedure Skip_UTF_Byte is
            begin
               Getc;

               if (U and 2#11000000#) /= 2#10_000000# then
                  Bad;
               end if;
            end Skip_UTF_Byte;

         --  Start of processing for UTF-8 case

         begin
            if Ptr < Input'First then
               Past_End;
            end if;

            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            Getc;

            if (U and 2#10000000#) = 2#00000000# then
               null;

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            elsif (U and 2#11100000#) = 2#110_00000# then
               Skip_UTF_Byte;

            --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11110000#) = 2#1110_0000# then
               Skip_UTF_Byte;
               Skip_UTF_Byte;

            --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

            elsif (U and 2#11111000#) = 2#11110_000# then
               for K in 1 .. 3 loop
                  Skip_UTF_Byte;
               end loop;

            --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx

            elsif (U and 2#11111100#) = 2#111110_00# then
               for K in 1 .. 4 loop
                  Skip_UTF_Byte;
               end loop;

            --  Any other code is invalid, note that this includes:

            --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
            --                               10xxxxxx 10xxxxxx 10xxxxxx

            --  since Wide_Wide_Character does not allow codes > 16#03FF_FFFF#

            else
               Bad;
            end if;
         end UTF8;

      --  Non-UTF-8 cass

      else
         declare
            Discard : Wide_Wide_Character;
         begin
            Decode_Wide_Wide_Character (Input, Ptr, Discard);
         end;
      end if;
   end Next_Wide_Wide_Character;

   --------------
   -- Past_End --
   --------------

   procedure Past_End is
   begin
      raise Constraint_Error with "past end of string";
   end Past_End;

   -------------------------
   -- Prev_Wide_Character --
   -------------------------

   procedure Prev_Wide_Character (Input : String; Ptr : in out Natural) is
   begin
      if Ptr > Input'Last + 1 then
         Past_End;
      end if;

      --  Special efficient encoding for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then
         UTF8 : declare
            U : Unsigned_32;

            procedure Getc;
            pragma Inline (Getc);
            --  Gets the character at Input (Ptr - 1) and returns code in U as
            --  Unsigned_32 value. On return Ptr is decremented by one.

            procedure Skip_UTF_Byte;
            pragma Inline (Skip_UTF_Byte);
            --  Checks that U is 2#10xxxxxx# and then calls Get

            ----------
            -- Getc --
            ----------

            procedure Getc is
            begin
               if Ptr <= Input'First then
                  Past_End;
               else
                  Ptr := Ptr - 1;
                  U := Unsigned_32 (Character'Pos (Input (Ptr)));
               end if;
            end Getc;

            -------------------
            -- Skip_UTF_Byte --
            -------------------

            procedure Skip_UTF_Byte is
            begin
               if (U and 2#11000000#) = 2#10_000000# then
                  Getc;
               else
                  Bad;
               end if;
            end Skip_UTF_Byte;

         --  Start of processing for UTF-8 case

         begin
            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            Getc;

            if (U and 2#10000000#) = 2#00000000# then
               return;

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            else
               Skip_UTF_Byte;

               if (U and 2#11100000#) = 2#110_00000# then
                  return;

               --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

               else
                  Skip_UTF_Byte;

                  if (U and 2#11110000#) = 2#1110_0000# then
                     return;

                     --  Any other code is invalid, note that this includes:

                     --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx
                     --                           10xxxxxx

                     --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx
                     --                               10xxxxxx 10xxxxxx
                     --                               10xxxxxx

                     --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx
                     --                               10xxxxxx 10xxxxxx
                     --                               10xxxxxx 10xxxxxx

                     --  since Wide_Character does not allow codes > 16#FFFF#

                  else
                     Bad;
                  end if;
               end if;
            end if;
         end UTF8;

      --  Special efficient encoding for brackets case

      elsif Encoding_Method = WCEM_Brackets then
         Brackets : declare
            P : Natural;
            S : Natural;

         begin
            --  See if we have "] at end positions

            if Ptr > Input'First + 1
              and then Input (Ptr - 1) = ']'
              and then Input (Ptr - 2) = '"'
            then
               P := Ptr - 2;

               --  Loop back looking for [" at start

               while P >= Ptr - 10 loop
                  if P <= Input'First + 1 then
                     Bad;

                  elsif Input (P - 1) = '"'
                    and then Input (P - 2) = '['
                  then
                     --  Found ["..."], scan forward to check it

                     S := P - 2;
                     P := S;
                     Next_Wide_Character (Input, P);

                     --  OK if at original pointer, else error

                     if P = Ptr then
                        Ptr := S;
                        return;
                     else
                        Bad;
                     end if;
                  end if;

                  P := P - 1;
               end loop;

               --  Falling through loop means more than 8 chars between the
               --  enclosing brackets (or simply a missing left bracket)

               Bad;

            --  Here if no bracket sequence present

            else
               if Ptr = Input'First then
                  Past_End;
               else
                  Ptr := Ptr - 1;
               end if;
            end if;
         end Brackets;

      --  Non-UTF-8/Brackets. These are the inefficient cases where we have to
      --  go to the start of the string and skip forwards till Ptr matches.

      else
         Non_UTF_Brackets : declare
            Discard : Wide_Character;
            PtrS    : Natural;
            PtrP    : Natural;

         begin
            PtrS := Input'First;

            if Ptr <= PtrS then
               Past_End;
            end if;

            loop
               PtrP := PtrS;
               Decode_Wide_Character (Input, PtrS, Discard);

               if PtrS = Ptr then
                  Ptr := PtrP;
                  return;

               elsif PtrS > Ptr then
                  Bad;
               end if;
            end loop;

         exception
            when Constraint_Error =>
               Bad;
         end Non_UTF_Brackets;
      end if;
   end Prev_Wide_Character;

   ------------------------------
   -- Prev_Wide_Wide_Character --
   ------------------------------

   procedure Prev_Wide_Wide_Character (Input : String; Ptr : in out Natural) is
   begin
      if Ptr > Input'Last + 1 then
         Past_End;
      end if;

      --  Special efficient encoding for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then
         UTF8 : declare
            U : Unsigned_32;

            procedure Getc;
            pragma Inline (Getc);
            --  Gets the character at Input (Ptr - 1) and returns code in U as
            --  Unsigned_32 value. On return Ptr is decremented by one.

            procedure Skip_UTF_Byte;
            pragma Inline (Skip_UTF_Byte);
            --  Checks that U is 2#10xxxxxx# and then calls Get

            ----------
            -- Getc --
            ----------

            procedure Getc is
            begin
               if Ptr <= Input'First then
                  Past_End;
               else
                  Ptr := Ptr - 1;
                  U := Unsigned_32 (Character'Pos (Input (Ptr)));
               end if;
            end Getc;

            -------------------
            -- Skip_UTF_Byte --
            -------------------

            procedure Skip_UTF_Byte is
            begin
               if (U and 2#11000000#) = 2#10_000000# then
                  Getc;
               else
                  Bad;
               end if;
            end Skip_UTF_Byte;

         --  Start of processing for UTF-8 case

         begin
            --  16#00_0000#-16#00_007F#: 0xxxxxxx

            Getc;

            if (U and 2#10000000#) = 2#00000000# then
               return;

            --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

            else
               Skip_UTF_Byte;

               if (U and 2#11100000#) = 2#110_00000# then
                  return;

               --  16#00_0800#-16#00_ffff#: 1110xxxx 10xxxxxx 10xxxxxx

               else
                  Skip_UTF_Byte;

                  if (U and 2#11110000#) = 2#1110_0000# then
                     return;

                  --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx
                  --                           10xxxxxx

                  else
                     Skip_UTF_Byte;

                     if (U and 2#11111000#) = 2#11110_000# then
                        return;

                     --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx
                     --                               10xxxxxx 10xxxxxx
                     --                               10xxxxxx

                     else
                        Skip_UTF_Byte;

                        if (U and 2#11111100#) = 2#111110_00# then
                           return;

                        --  Any other code is invalid, note that this includes:

                        --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx
                        --                               10xxxxxx 10xxxxxx
                        --                               10xxxxxx 10xxxxxx

                        --  since Wide_Wide_Character does not allow codes
                        --  greater than 16#03FF_FFFF#

                        else
                           Bad;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end UTF8;

      --  Special efficient encoding for brackets case

      elsif Encoding_Method = WCEM_Brackets then
         Brackets : declare
            P : Natural;
            S : Natural;

         begin
            --  See if we have "] at end positions

            if Ptr > Input'First + 1
              and then Input (Ptr - 1) = ']'
              and then Input (Ptr - 2) = '"'
            then
               P := Ptr - 2;

               --  Loop back looking for [" at start

               while P >= Ptr - 10 loop
                  if P <= Input'First + 1 then
                     Bad;

                  elsif Input (P - 1) = '"'
                    and then Input (P - 2) = '['
                  then
                     --  Found ["..."], scan forward to check it

                     S := P - 2;
                     P := S;
                     Next_Wide_Wide_Character (Input, P);

                     --  OK if at original pointer, else error

                     if P = Ptr then
                        Ptr := S;
                        return;
                     else
                        Bad;
                     end if;
                  end if;

                  P := P - 1;
               end loop;

               --  Falling through loop means more than 8 chars between the
               --  enclosing brackets (or simply a missing left bracket)

               Bad;

            --  Here if no bracket sequence present

            else
               if Ptr = Input'First then
                  Past_End;
               else
                  Ptr := Ptr - 1;
               end if;
            end if;
         end Brackets;

      --  Non-UTF-8/Brackets. These are the inefficient cases where we have to
      --  go to the start of the string and skip forwards till Ptr matches.

      else
         Non_UTF8_Brackets : declare
            Discard : Wide_Wide_Character;
            PtrS    : Natural;
            PtrP    : Natural;

         begin
            PtrS := Input'First;

            if Ptr <= PtrS then
               Past_End;
            end if;

            loop
               PtrP := PtrS;
               Decode_Wide_Wide_Character (Input, PtrS, Discard);

               if PtrS = Ptr then
                  Ptr := PtrP;
                  return;

               elsif PtrS > Ptr then
                  Bad;
               end if;
            end loop;

         exception
             when Constraint_Error =>
               Bad;
         end Non_UTF8_Brackets;
      end if;
   end Prev_Wide_Wide_Character;

   --------------------------
   -- Validate_Wide_String --
   --------------------------

   function Validate_Wide_String (S : String) return Boolean is
      Ptr : Natural;

   begin
      Ptr := S'First;
      while Ptr <= S'Last loop
         Next_Wide_Character (S, Ptr);
      end loop;

      return True;

   exception
      when Constraint_Error =>
         return False;
   end Validate_Wide_String;

   -------------------------------
   -- Validate_Wide_Wide_String --
   -------------------------------

   function Validate_Wide_Wide_String (S : String) return Boolean is
      Ptr : Natural;

   begin
      Ptr := S'First;
      while Ptr <= S'Last loop
         Next_Wide_Wide_Character (S, Ptr);
      end loop;

      return True;

   exception
      when Constraint_Error =>
         return False;
   end Validate_Wide_Wide_String;

end GNAT.Decode_String;
