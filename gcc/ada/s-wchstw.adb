------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ S T W                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

with Interfaces;     use Interfaces;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_StW is

   ---------------------------
   -- String_To_Wide_String --
   ---------------------------

   function String_To_Wide_String
     (S    : String;
      EM   : WC_Encoding_Method)
      return Wide_String
   is
      R  : Wide_String (1 .. S'Length);
      RP : Natural;
      SP : Natural;
      U1 : Unsigned_16;
      U2 : Unsigned_16;
      U3 : Unsigned_16;
      U  : Unsigned_16;

      Last : constant Natural := S'Last;

      function Get_Hex (C : Character) return Unsigned_16;
      --  Converts character from hex digit to value in range 0-15. The
      --  input must be in 0-9, A-F, or a-f, and no check is needed.

      procedure Get_Hex_4;
      --  Translates four hex characters starting at S (SP) to a single
      --  wide character. Used in WCEM_Hex and WCEM_Brackets mode. SP
      --  is not modified by the call. The resulting wide character value
      --  is stored in R (RP). RP is not modified by the call.

      function Get_Hex (C : Character) return Unsigned_16 is
      begin
         if C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C in 'A' .. 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         else
            return Character'Pos (C) - Character'Pos ('a') + 10;
         end if;
      end Get_Hex;

      procedure Get_Hex_4 is
      begin
         R (RP) := Wide_Character'Val (
            Get_Hex (S (SP + 3)) + 16 *
              (Get_Hex (S (SP + 2)) + 16 *
                (Get_Hex (S (SP + 1)) + 16 *
                  (Get_Hex (S (SP + 0))))));
      end Get_Hex_4;

   --  Start of processing for String_To_Wide_String

   begin
      SP := S'First;
      RP := 0;

      case EM is

         --  ESC-Hex representation

         when WCEM_Hex =>
            while SP <= Last - 4 loop
               RP := RP + 1;

               if S (SP) = ASCII.ESC then
                  SP := SP + 1;
                  Get_Hex_4;
                  SP := SP + 4;
               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

         --  Upper bit shift, internal code = external code

         when WCEM_Upper =>
            while SP < Last loop
               RP := RP + 1;

               if S (SP) >= Character'Val (16#80#) then
                  U1 := Character'Pos (S (SP));
                  U2 := Character'Pos (S (SP + 1));
                  R (RP) := Wide_Character'Val (256 * U1 + U2);
                  SP := SP + 2;
               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

         --  Upper bit shift, shift-JIS

         when WCEM_Shift_JIS =>
            while SP < Last loop
               RP := RP + 1;

               if S (SP) >= Character'Val (16#80#) then
                  R (RP) := Shift_JIS_To_JIS (S (SP), S (SP + 1));
                  SP := SP + 2;
               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

         --  Upper bit shift, EUC

         when WCEM_EUC =>
            while SP < Last loop
               RP := RP + 1;

               if S (SP) >= Character'Val (16#80#) then
                  R (RP) := EUC_To_JIS (S (SP), S (SP + 1));
                  SP := SP + 2;
               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

         --  Upper bit shift, UTF-8

         when WCEM_UTF8 =>
            while SP < Last loop
               RP := RP + 1;

               if S (SP) >= Character'Val (16#80#) then
                  U1 := Character'Pos (S (SP));
                  U2 := Character'Pos (S (SP + 1));

                  U := Shift_Left (U1 and 2#00011111#, 6) +
                         (U2 and 2#00111111#);
                  SP := SP + 2;

                  if U1 >= 2#11100000# then
                     U3 := Character'Pos (S (SP));
                     U := Shift_Left (U, 6) + (U3 and 2#00111111#);
                     SP := SP + 1;
                  end if;

                  R (RP) := Wide_Character'Val (U);

               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

         --  Brackets representation

         when WCEM_Brackets =>
            while SP <= Last - 7 loop
               RP := RP + 1;

               if S (SP) = '['
                 and then S (SP + 1) = '"'
                 and then S (SP + 2) /= '"'
               then
                  SP := SP + 2;
                  Get_Hex_4;
                  SP := SP + 6;

               else
                  R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
                  SP := SP + 1;
               end if;
            end loop;

      end case;

      while SP <= Last loop
         RP := RP + 1;
         R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
         SP := SP + 1;
      end loop;

      return R (1 .. RP);
   end String_To_Wide_String;

end System.WCh_StW;
