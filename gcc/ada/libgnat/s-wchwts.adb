------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with System.Case_Util; use System.Case_Util;
with System.WCh_Con;   use System.WCh_Con;
with System.WCh_Cnv;   use System.WCh_Cnv;

package body System.WCh_WtS is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Normalize_String
     (S  : in out String;
      EM : WC_Encoding_Method);
   --  If S does not represent a character literal, then any lower case
   --  characters in S are changed to their upper case counterparts, while
   --  wide characters are unchanged. EM indicates their encoding method.
   --  This is the wide counterpart of System.Val_Util.Normalize_String.

   procedure Store_UTF_32_Character
     (U  : UTF_32_Code;
      S  : out String;
      P  : in out Integer;
      EM : WC_Encoding_Method);
   --  Stores the string representation of the wide or wide wide character
   --  whose code is given as U, starting at S (P + 1). P is incremented to
   --  point to the last character stored. Raises CE if character cannot be
   --  stored using the given encoding method.

   ----------------------
   -- Normalize_String --
   ----------------------

   procedure Normalize_String
     (S  : in out String;
      EM : WC_Encoding_Method)
   is
      procedure Skip_Wide (S : String; P : in out Natural);
      --  On entry S (P) points to an ESC character for a wide character escape
      --  sequence or an upper half character if the encoding method uses the
      --  upper bit, or a left bracket if the brackets encoding method is in
      --  use. On exit, P is bumped past the wide character sequence.

      ---------------
      -- Skip_Wide --
      ---------------

      procedure Skip_Wide (S : String; P : in out Natural) is
         function Skip_Char return Character;
         --  Function to skip one character of wide character escape sequence

         ---------------
         -- Skip_Char --
         ---------------

         function Skip_Char return Character is
         begin
            P := P + 1;
            return S (P - 1);
         end Skip_Char;

         function WC_Skip is new Char_Sequence_To_UTF_32 (Skip_Char);

         Discard : UTF_32_Code;
         pragma Warnings (Off, Discard);

      --  Start of processing for Skip_Wide

      begin
         --  Capture invalid wide characters errors since we are going to
         --  discard the result anyway. We just want to move past it.

         begin
            Discard := WC_Skip (Skip_Char, EM);
         exception
            when Constraint_Error =>
               null;
         end;
      end Skip_Wide;

      F, L, Ptr : Natural;

   begin
      F := S'First;
      L := S'Last;

      --  Case of empty string

      if F > L then
         return;
      end if;

      --  Scan for leading spaces

      while F < L and then S (F) = ' ' loop
         F := F + 1;
      end loop;

      --  Case of no nonspace characters found. Decrease L to ensure L < F
      --  without risking an overflow if F is Integer'Last.

      if S (F) = ' ' then
         L := L - 1;
         return;
      end if;

      --  Scan for trailing spaces

      while S (L) = ' ' loop
         L := L - 1;
      end loop;

      --  Convert to upper case if S is not a character literal

      if S (F) /= ''' then
         Ptr := F;

         while Ptr <= L loop
            --  This mimics the handling of wide characters in a call to
            --  Casing.Set_Casing (All_Upper_Case) in the compiler.

            if S (Ptr) = ASCII.ESC
              or else S (Ptr) = '['
              or else (EM in WC_Upper_Half_Encoding_Method
                         and then Character'Pos (S (Ptr)) >= 16#80#)
            then
               Skip_Wide (S, Ptr);

            else
               S (Ptr) := To_Upper (S (Ptr));
               Ptr := Ptr + 1;
            end if;
         end loop;
      end if;
   end Normalize_String;

   ----------------------------
   -- Store_UTF_32_Character --
   ----------------------------

   procedure Store_UTF_32_Character
     (U  : UTF_32_Code;
      S  : out String;
      P  : in out Integer;
      EM : WC_Encoding_Method)
   is
      procedure Out_Char (C : Character);
      pragma Inline (Out_Char);
      --  Procedure to increment P and store C at S (P)

      procedure Store_Chars is new UTF_32_To_Char_Sequence (Out_Char);

      --------------
      -- Out_Char --
      --------------

      procedure Out_Char (C : Character) is
      begin
         P := P + 1;
         S (P) := C;
      end Out_Char;

   begin
      Store_Chars (U, EM);
   end Store_UTF_32_Character;

   --------------------------------
   -- Enum_Wide_String_To_String --
   --------------------------------

   function Enum_Wide_String_To_String
     (S  : Wide_String;
      EM : WC_Encoding_Method) return String
   is
      Result : String := Wide_String_To_String (S, EM);

   begin
      Normalize_String (Result, EM);
      return Result;
   end Enum_Wide_String_To_String;

   -------------------------------------
   -- Enum_Wide_Wide_String_To_String --
   -------------------------------------

   function Enum_Wide_Wide_String_To_String
     (S  : Wide_Wide_String;
      EM : WC_Encoding_Method) return String
   is
      Result : String := Wide_Wide_String_To_String (S, EM);

   begin
      Normalize_String (Result, EM);
      return Result;
   end Enum_Wide_Wide_String_To_String;

   ---------------------------
   -- Wide_String_To_String --
   ---------------------------

   function Wide_String_To_String
     (S  : Wide_String;
      EM : WC_Encoding_Method) return String
   is
      Max_Chars : constant Natural := WC_Longest_Sequences (EM);

      Result     : String (S'First .. S'First + Max_Chars * S'Length);
      Result_Idx : Natural;

   begin
      Result_Idx := Result'First - 1;

      for S_Idx in S'Range loop
         Store_UTF_32_Character
           (U  => Wide_Character'Pos (S (S_Idx)),
            S  => Result,
            P  => Result_Idx,
            EM => EM);
      end loop;

      return Result (Result'First .. Result_Idx);
   end Wide_String_To_String;

   --------------------------------
   -- Wide_Wide_String_To_String --
   --------------------------------

   function Wide_Wide_String_To_String
     (S  : Wide_Wide_String;
      EM : WC_Encoding_Method) return String
   is
      Max_Chars : constant Natural := WC_Longest_Sequences (EM);

      Result     : String (S'First .. S'First + Max_Chars * S'Length);
      Result_Idx : Natural;

   begin
      Result_Idx := Result'First - 1;

      for S_Idx in S'Range loop
         Store_UTF_32_Character
           (U  => Wide_Wide_Character'Pos (S (S_Idx)),
            S  => Result,
            P  => Result_Idx,
            EM => EM);
      end loop;

      return Result (Result'First .. Result_Idx);
   end Wide_Wide_String_To_String;

end System.WCh_WtS;
