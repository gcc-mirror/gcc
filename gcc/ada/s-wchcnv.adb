------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ C N V                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  This package contains generic subprograms used for converting between
--  sequences of Character and Wide_Character. All access to wide character
--  sequences is isolated in this unit.

with Interfaces;     use Interfaces;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_Cnv is

   --------------------------------
   -- Char_Sequence_To_Wide_Char --
   --------------------------------

   function Char_Sequence_To_Wide_Char
     (C    : Character;
      EM   : WC_Encoding_Method)
      return Wide_Character
   is
      B1 : Integer;
      C1 : Character;
      U  : Unsigned_16;
      W  : Unsigned_16;

      procedure Get_Hex (N : Character);
      --  If N is a hex character, then set B1 to 16 * B1 + character N.
      --  Raise Constraint_Error if character N is not a hex character.

      -------------
      -- Get_Hex --
      -------------

      procedure Get_Hex (N : Character) is
         B2 : constant Integer := Character'Pos (N);

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

   --  Start of processing for Char_Sequence_To_Wide_Char

   begin
      case EM is

         when WCEM_Hex =>
            if C /= ASCII.ESC then
               return Wide_Character'Val (Character'Pos (C));

            else
               B1 := 0;
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);

               return Wide_Character'Val (B1);
            end if;

         when WCEM_Upper =>
            if C > ASCII.DEL then
               return
                 Wide_Character'Val
                   (Integer (256 * Character'Pos (C)) +
                    Character'Pos (In_Char));
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_Shift_JIS =>
            if C > ASCII.DEL then
               return Shift_JIS_To_JIS (C, In_Char);
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_EUC =>
            if C > ASCII.DEL then
               return EUC_To_JIS (C, In_Char);
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_UTF8 =>
            if C > ASCII.DEL then

               --  16#0080#-16#07ff#: 2#110xxxxx# 2#10xxxxxx#
               --  16#0800#-16#ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#

               U := Unsigned_16 (Character'Pos (C));

               if (U and 2#11100000#) = 2#11000000# then
                  W := Shift_Left (U and 2#00011111#, 6);
                  U := Unsigned_16 (Character'Pos (In_Char));

                  if (U and 2#11000000#) /= 2#10000000# then
                     raise Constraint_Error;
                  end if;

                  W := W or (U and 2#00111111#);

               elsif (U and 2#11110000#) = 2#11100000# then
                  W := Shift_Left (U and 2#00001111#, 12);
                  U := Unsigned_16 (Character'Pos (In_Char));

                  if (U and 2#11000000#) /= 2#10000000# then
                     raise Constraint_Error;
                  end if;

                  W := W or Shift_Left (U and 2#00111111#, 6);
                  U := Unsigned_16 (Character'Pos (In_Char));

                  if (U and 2#11000000#) /= 2#10000000# then
                     raise Constraint_Error;
                  end if;

                  W := W or (U and 2#00111111#);

               else
                  raise Constraint_Error;
               end if;

               return Wide_Character'Val (W);

            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_Brackets =>

            if C /= '[' then
               return Wide_Character'Val (Character'Pos (C));
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
                  raise Constraint_Error;
               end if;
            end if;

            if In_Char /= ']' then
               raise Constraint_Error;
            end if;

            return Wide_Character'Val (B1);

      end case;
   end Char_Sequence_To_Wide_Char;

   --------------------------------
   -- Wide_Char_To_Char_Sequence --
   --------------------------------

   procedure Wide_Char_To_Char_Sequence
     (WC : Wide_Character;
      EM : WC_Encoding_Method)
   is
      Val    : constant Natural := Wide_Character'Pos (WC);
      Hexc   : constant array (0 .. 15) of Character := "0123456789ABCDEF";
      C1, C2 : Character;
      U      : Unsigned_16;

   begin
      case EM is

         when WCEM_Hex =>
            if Val < 256 then
               Out_Char (Character'Val (Val));

            else
               Out_Char (ASCII.ESC);
               Out_Char (Hexc (Val / (16**3)));
               Out_Char (Hexc ((Val / (16**2)) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
            end if;

         when WCEM_Upper =>
            if Val < 128 then
               Out_Char (Character'Val (Val));

            elsif Val < 16#8000# then
               raise Constraint_Error;

            else
               Out_Char (Character'Val (Val / 256));
               Out_Char (Character'Val (Val mod 256));
            end if;

         when WCEM_Shift_JIS =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            else
               JIS_To_Shift_JIS (WC, C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            end if;

         when WCEM_EUC =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            else
               JIS_To_EUC (WC, C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            end if;

         when WCEM_UTF8 =>
            U := Unsigned_16 (Val);

            --  16#0000#-16#007f#: 2#0xxxxxxx#
            --  16#0080#-16#07ff#: 2#110xxxxx# 2#10xxxxxx#
            --  16#0800#-16#ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#

            if U < 16#80# then
               Out_Char (Character'Val (U));

            elsif U < 16#0800# then
               Out_Char (Character'Val (2#11000000# or Shift_Right (U, 6)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

            else
               Out_Char (Character'Val (2#11100000# or Shift_Right (U, 12)));
               Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                         and 2#00111111#)));
               Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));
            end if;

         when WCEM_Brackets =>

            if Val < 256 then
               Out_Char (Character'Val (Val));

            else
               Out_Char ('[');
               Out_Char ('"');
               Out_Char (Hexc (Val / (16**3)));
               Out_Char (Hexc ((Val / (16**2)) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
               Out_Char ('"');
               Out_Char (']');
            end if;
      end case;
   end Wide_Char_To_Char_Sequence;

end System.WCh_Cnv;
