------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ S T W                        --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

package body System.WCh_StW is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Get_Next_Code
     (S  : String;
      P  : in out Natural;
      V  : out UTF_32_Code;
      EM : WC_Encoding_Method);
   --  Scans next character starting at S(P) and returns its value in V. On
   --  exit P is updated past the last character read. Raises Constraint_Error
   --  if the string is not well formed. Raises Constraint_Error if the code
   --  value is greater than 16#7FFF_FFFF#. On entry P <= S'Last.

   -------------------
   -- Get_Next_Code --
   -------------------

   procedure Get_Next_Code
     (S  : String;
      P  : in out Natural;
      V  : out UTF_32_Code;
      EM : WC_Encoding_Method)
   is
      function In_Char return Character;
      --  Function to return a character, bumping P, raises Constraint_Error
      --  if P > S'Last on entry.

      function Get_UTF_32 is new Char_Sequence_To_UTF_32 (In_Char);
      --  Function to get next UFT_32 value.

      -------------
      -- In_Char --
      -------------

      function In_Char return Character is
      begin
         if P > S'Last then
            raise Constraint_Error;
         else
            P := P + 1;
            return S (P - 1);
         end if;
      end In_Char;

   begin
      --  Check for wide character encoding

      case EM is
         when WCEM_Hex =>
            if S (P) = ASCII.ESC then
               V := Get_UTF_32 (In_Char, EM);
               return;
            end if;

         when WCEM_Upper | WCEM_Shift_JIS | WCEM_EUC | WCEM_UTF8 =>
            if S (P) >= Character'Val (16#80#) then
               V := Get_UTF_32 (In_Char, EM);
               return;
            end if;

         when WCEM_Brackets =>
            if P + 2 <= S'Last
              and then S (P) = '['
              and then S (P + 1) = '"'
              and then S (P + 2) /= '"'
            then
               V := Get_UTF_32 (In_Char, EM);
               return;
            end if;
      end case;

      --  If it is not a wide character code, just get it

      V := Character'Pos (S (P));
      P := P + 1;
   end Get_Next_Code;

   ---------------------------
   -- String_To_Wide_String --
   ---------------------------

   function String_To_Wide_String
     (S  : String;
      EM : WC_Encoding_Method) return Wide_String
   is
      R  : Wide_String (1 .. S'Length);
      RP : Natural;
      SP : Natural;
      V  : UTF_32_Code;

   begin
      SP := S'First;
      RP := 0;
      while SP <= S'Last loop
         Get_Next_Code (S, SP, V, EM);

         if V > 16#FFFF# then
            raise Constraint_Error;
         end if;

         RP := RP + 1;
         R (RP) := Wide_Character'Val (V);
      end loop;

      return R (1 .. RP);
   end String_To_Wide_String;

   --------------------------------
   -- String_To_Wide_Wide_String --
   --------------------------------

   function String_To_Wide_Wide_String
     (S  : String;
      EM : WC_Encoding_Method) return Wide_Wide_String
   is
      R  : Wide_Wide_String (1 .. S'Length);
      RP : Natural;
      SP : Natural;
      V  : UTF_32_Code;

   begin
      SP := S'First;
      RP := 0;
      while SP <= S'Last loop
         Get_Next_Code (S, SP, V, EM);
         RP := RP + 1;
         R (RP) := Wide_Wide_Character'Val (V);
      end loop;

      return R (1 .. RP);
   end String_To_Wide_Wide_String;

end System.WCh_StW;
