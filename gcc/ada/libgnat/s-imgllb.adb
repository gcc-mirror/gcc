------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L B                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with System.Unsigned_Types; use System.Unsigned_Types;

package body System.Img_LLB is

   ---------------------------------------
   -- Set_Image_Based_Long_Long_Integer --
   ---------------------------------------

   procedure Set_Image_Based_Long_Long_Integer
     (V : Long_Long_Integer;
      B : Natural;
      W : Integer;
      S : out String;
      P : in out Natural)
   is
      Start : Natural;

   begin
      --  Positive case can just use the unsigned circuit directly

      if V >= 0 then
         Set_Image_Based_Long_Long_Unsigned
           (Long_Long_Unsigned (V), B, W, S, P);

      --  Negative case has to set a minus sign. Note also that we have to be
      --  careful not to generate overflow with the largest negative number.

      else
         P := P + 1;
         S (P) := ' ';
         Start := P;

         declare
            pragma Suppress (Overflow_Check);
            pragma Suppress (Range_Check);
         begin
            Set_Image_Based_Long_Long_Unsigned
              (Long_Long_Unsigned (-V), B, W - 1, S, P);
         end;

         --  Set minus sign in last leading blank location. Because of the
         --  code above, there must be at least one such location.

         while S (Start + 1) = ' ' loop
            Start := Start + 1;
         end loop;

         S (Start) := '-';
      end if;

   end Set_Image_Based_Long_Long_Integer;

   ----------------------------------------
   -- Set_Image_Based_Long_Long_Unsigned --
   ----------------------------------------

   procedure Set_Image_Based_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      B : Natural;
      W : Integer;
      S : out String;
      P : in out Natural)
   is
      Start : constant Natural := P;
      F, T  : Natural;
      BU    : constant Long_Long_Unsigned := Long_Long_Unsigned (B);
      Hex   : constant array
                (Long_Long_Unsigned range 0 .. 15) of Character :=
                                                         "0123456789ABCDEF";

      procedure Set_Digits (T : Long_Long_Unsigned);
      --  Set digits of absolute value of T

      ----------------
      -- Set_Digits --
      ----------------

      procedure Set_Digits (T : Long_Long_Unsigned) is
      begin
         if T >= BU then
            Set_Digits (T / BU);
            P := P + 1;
            S (P) := Hex (T mod BU);
         else
            P := P + 1;
            S (P) := Hex (T);
         end if;
      end Set_Digits;

   --  Start of processing for Set_Image_Based_Long_Long_Unsigned

   begin

      if B >= 10 then
         P := P + 1;
         S (P) := '1';
      end if;

      P := P + 1;
      S (P) := Character'Val (Character'Pos ('0') + B mod 10);

      P := P + 1;
      S (P) := '#';

      Set_Digits (V);

      P := P + 1;
      S (P) := '#';

      --  Add leading spaces if required by width parameter

      if P - Start < W then
         F := P;
         P := Start + W;
         T := P;

         while F > Start loop
            S (T) := S (F);
            T := T - 1;
            F := F - 1;
         end loop;

         for J in Start + 1 .. T loop
            S (J) := ' ';
         end loop;
      end if;

   end Set_Image_Based_Long_Long_Unsigned;

end System.Img_LLB;
