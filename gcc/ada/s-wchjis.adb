------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ J I S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2005 Free Software Foundation, Inc.         --
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

package body System.WCh_JIS is

   type Byte is mod 256;

   EUC_Hankaku_Kana : constant Byte := 16#8E#;
   --  Prefix byte in EUC for Hankaku Kana (small Katakana). Such characters
   --  in EUC are represented by a prefix byte followed by the code, which
   --  is in the upper half (the corresponding JIS internal code is in the
   --  range 16#0080# - 16#00FF#).

   function EUC_To_JIS (EUC1, EUC2 : Character) return Wide_Character is
      EUC1B  : constant Byte := Character'Pos (EUC1);
      EUC2B  : constant Byte := Character'Pos (EUC2);

   begin
      if EUC2B not in 16#A0# .. 16#FE# then
         raise Constraint_Error;
      end if;

      if EUC1B = EUC_Hankaku_Kana then
         return Wide_Character'Val (EUC2B);

      else
         if EUC1B not in 16#A0# .. 16#FE# then
            raise Constraint_Error;
         else
            return Wide_Character'Val
              (256 * Natural (EUC1B and 16#7F#) + Natural (EUC2B and 16#7F#));
         end if;
      end if;
   end EUC_To_JIS;

   ----------------
   -- JIS_To_EUC --
   ----------------

   procedure JIS_To_EUC
     (J    : in Wide_Character;
      EUC1 : out Character;
      EUC2 : out Character)
   is
      JIS1 : constant Natural := Wide_Character'Pos (J) / 256;
      JIS2 : constant Natural := Wide_Character'Pos (J) rem 256;

   begin
      if JIS1 = 0 then
         EUC1 := Character'Val (EUC_Hankaku_Kana);
         EUC2 := Character'Val (JIS2);

      else
         EUC1 := Character'Val (JIS1 + 16#80#);
         EUC2 := Character'Val (JIS2 + 16#80#);
      end if;
   end JIS_To_EUC;

   ----------------------
   -- JIS_To_Shift_JIS --
   ----------------------

   procedure JIS_To_Shift_JIS
     (J   : in Wide_Character;
      SJ1 : out Character;
      SJ2 : out Character)
   is
      JIS1 : Byte;
      JIS2 : Byte;

   begin
      --  The following is the required algorithm, it's hard to make any
      --  more intelligent comments! This was copied from a public domain
      --  C program called etos.c (author unknown).

      JIS1 := Byte (Natural (Wide_Character'Pos (J) / 256));
      JIS2 := Byte (Natural (Wide_Character'Pos (J) rem 256));

      if JIS1 > 16#5F# then
         JIS1 := JIS1 + 16#80#;
      end if;

      if (JIS1 mod 2) = 0 then
         SJ1 := Character'Val ((JIS1 - 16#30#) / 2 + 16#88#);
         SJ2 := Character'Val (JIS2 + 16#7E#);

      else
         if JIS2 >= 16#60# then
            JIS2 := JIS2 + 16#01#;
         end if;

         SJ1 := Character'Val ((JIS1 - 16#31#) / 2 + 16#89#);
         SJ2 := Character'Val (JIS2 + 16#1F#);
      end if;
   end JIS_To_Shift_JIS;

   ----------------------
   -- Shift_JIS_To_JIS --
   ----------------------

   function Shift_JIS_To_JIS (SJ1, SJ2 : Character) return Wide_Character is
      SJIS1 : Byte;
      SJIS2 : Byte;
      JIS1  : Byte;
      JIS2  : Byte;

   begin
      --  The following is the required algorithm, it's hard to make any
      --  more intelligent comments! This was copied from a public domain
      --  C program called stoj.c written by shige@csk.JUNET.

      SJIS1 := Character'Pos (SJ1);
      SJIS2 := Character'Pos (SJ2);

      if SJIS1 >= 16#E0# then
         SJIS1 := SJIS1 - 16#40#;
      end if;

      if SJIS2 >= 16#9F# then
         JIS1 := (SJIS1 - 16#88#) * 2 + 16#30#;
         JIS2 := SJIS2 - 16#7E#;

      else
         if SJIS2 >= 16#7F# then
            SJIS2 := SJIS2 - 16#01#;
         end if;

         JIS1 := (SJIS1 - 16#89#) * 2 + 16#31#;
         JIS2 := SJIS2 - 16#1F#;
      end if;

      if JIS1 not in 16#20# .. 16#7E#
        or else JIS2 not in 16#20# .. 16#7E#
      then
         raise Constraint_Error;
      else
         return Wide_Character'Val (256 * Natural (JIS1) + Natural (JIS2));
      end if;
   end Shift_JIS_To_JIS;

end System.WCh_JIS;
