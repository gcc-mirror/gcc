------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
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

package body System.WCh_WtS is

   ---------------------------
   -- Wide_String_To_String --
   ---------------------------

   function Wide_String_To_String
     (S    : Wide_String;
      EM   : WC_Encoding_Method)
      return String
   is
      R  : String (1 .. 5 * S'Length); -- worst case length!
      RP : Natural;
      C1 : Character;
      C2 : Character;

   begin
      RP := 0;

      for SP in S'Range loop
         declare
            C   : constant Wide_Character := S (SP);
            CV  : constant Unsigned_16    := Wide_Character'Pos (C);
            Hex : constant array (Unsigned_16 range 0 .. 15) of Character :=
                    "0123456789ABCDEF";

         begin
            if CV <= 127 then
               RP := RP + 1;
               R (RP) := Character'Val (CV);

            else
               case EM is

                  --  Hex ESC sequence encoding

                  when WCEM_Hex =>
                     if CV <= 16#FF# then
                        RP := RP + 1;
                        R (RP) := Character'Val (CV);

                     else
                        R (RP + 1) := ASCII.ESC;
                        R (RP + 2) := Hex (Shift_Right (CV, 12));
                        R (RP + 3) := Hex (Shift_Right (CV, 8)  and 16#000F#);
                        R (RP + 4) := Hex (Shift_Right (CV, 4)  and 16#000F#);
                        R (RP + 5) := Hex (CV                   and 16#000F#);
                        RP := RP + 5;
                     end if;

                  --  Upper bit shift (internal code = external code)

                  when WCEM_Upper =>
                     R (RP + 1) := Character'Val (Shift_Right (CV, 8));
                     R (RP + 2) := Character'Val (CV and 16#FF#);
                     RP := RP + 2;

                  --  Upper bit shift (EUC)

                  when WCEM_EUC =>
                     JIS_To_EUC (C, C1, C2);
                     R (RP + 1) := C1;
                     R (RP + 2) := C2;
                     RP := RP + 2;

                  --  Upper bit shift (Shift-JIS)

                  when WCEM_Shift_JIS =>
                     JIS_To_Shift_JIS (C, C1, C2);
                     R (RP + 1) := C1;
                     R (RP + 2) := C2;
                     RP := RP + 2;

                  --  Upper bit shift (UTF-8)

                  --    16#0080#-16#07ff#: 2#110xxxxx# 2#10xxxxxx#
                  --    16#0800#-16#ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#

                  when WCEM_UTF8 =>
                     if CV < 16#0800# then
                        R (RP + 1) :=
                          Character'Val (2#11000000# or Shift_Right (CV, 6));
                        R (RP + 2) :=
                          Character'Val (2#10000000# or (CV and 2#00111111#));
                        RP := RP + 2;

                     else
                        R (RP + 1) :=
                          Character'Val (2#11100000# or Shift_Right (CV, 12));
                        R (RP + 2) :=
                          Character'Val (2#10000000# or
                                          (Shift_Right (CV, 6) and
                                                               2#00111111#));
                        R (RP + 3) :=
                          Character'Val (2#10000000# or (CV and 2#00111111#));
                        RP := RP + 3;
                     end if;

                  --  Brackets encoding

                  when WCEM_Brackets =>
                     if CV <= 16#FF# then
                        RP := RP + 1;
                        R (RP) := Character'Val (CV);

                     else
                        R (RP + 1) := '[';
                        R (RP + 2) := '"';
                        R (RP + 3) := Hex (Shift_Right (CV, 12));
                        R (RP + 4) := Hex (Shift_Right (CV, 8)  and 16#000F#);
                        R (RP + 5) := Hex (Shift_Right (CV, 4)  and 16#000F#);
                        R (RP + 6) := Hex (CV                   and 16#000F#);
                        R (RP + 7) := '"';
                        R (RP + 8) := ']';
                        RP := RP + 8;
                     end if;

               end case;
            end if;
         end;
      end loop;

      return R (1 .. RP);
   end Wide_String_To_String;

end System.WCh_WtS;
