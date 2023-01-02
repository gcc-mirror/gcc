------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with System.WCh_Con; use System.WCh_Con;
with System.WCh_Cnv; use System.WCh_Cnv;

package body System.WCh_WtS is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Store_UTF_32_Character
     (U  : UTF_32_Code;
      S  : out String;
      P  : in out Integer;
      EM : WC_Encoding_Method);
   --  Stores the string representation of the wide or wide wide character
   --  whose code is given as U, starting at S (P + 1). P is incremented to
   --  point to the last character stored. Raises CE if character cannot be
   --  stored using the given encoding method.

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
