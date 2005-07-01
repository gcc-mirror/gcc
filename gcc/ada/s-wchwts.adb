------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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
      R  : String (1 .. 5 * S'Length); -- worst case length!
      RP : Natural;

   begin
      RP := 0;
      for SP in S'Range loop
         Store_UTF_32_Character (Wide_Character'Pos (S (SP)), R, RP, EM);
      end loop;

      return R (1 .. RP);
   end Wide_String_To_String;

   --------------------------------
   -- Wide_Wide_Sring_To_String --
   --------------------------------

   function Wide_Wide_String_To_String
     (S  : Wide_Wide_String;
      EM : WC_Encoding_Method) return String
   is
      R  : String (1 .. 7 * S'Length); -- worst case length!
      RP : Natural;

   begin
      RP := 0;

      for SP in S'Range loop
         Store_UTF_32_Character (Wide_Wide_Character'Pos (S (SP)), R, RP, EM);
      end loop;

      return R (1 .. RP);
   end Wide_Wide_String_To_String;

end System.WCh_WtS;
