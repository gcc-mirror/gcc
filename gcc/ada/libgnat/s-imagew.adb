------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ W                        --
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

package body System.Image_W is

   -----------------------------
   -- Set_Image_Width_Integer --
   -----------------------------

   procedure Set_Image_Width_Integer
     (V : Int;
      W : Integer;
      S : out String;
      P : in out Natural)
   is
      Start : Natural;

   begin
      --  Positive case can just use the unsigned circuit directly

      if V >= 0 then
         Set_Image_Width_Unsigned (Uns (V), W, S, P);

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
            Set_Image_Width_Unsigned (Uns (-V), W - 1, S, P);
         end;

         --  Set minus sign in last leading blank location. Because of the
         --  code above, there must be at least one such location.

         while S (Start + 1) = ' ' loop
            Start := Start + 1;
         end loop;

         S (Start) := '-';
      end if;

   end Set_Image_Width_Integer;

   ------------------------------
   -- Set_Image_Width_Unsigned --
   ------------------------------

   procedure Set_Image_Width_Unsigned
     (V : Uns;
      W : Integer;
      S : out String;
      P : in out Natural)
   is
      Start : constant Natural := P + 1;
      Nb_Digits : Natural := 1;
      T : Uns := V;

   begin

      --  First we compute the number of characters needed for representing
      --  the number.
      loop
         T := T / 10;
         exit when T = 0;
         Nb_Digits := Nb_Digits + 1;
      end loop;

      P := Start;

      --  Pad S with spaces up to W reduced by Nb_Digits
      while P < Start + W - Nb_Digits loop
         S (P) := ' ';
         P := P + 1;
      end loop;

      --  We now populate digits from the end of the value to the beginning
      T := V;
      for J in reverse  P .. P + Nb_Digits - 1 loop
         S (J) := Character'Val (T mod 10 + Character'Pos ('0'));
         T := T / 10;
      end loop;

      P := P + Nb_Digits - 1;

   end Set_Image_Width_Unsigned;

end System.Image_W;
