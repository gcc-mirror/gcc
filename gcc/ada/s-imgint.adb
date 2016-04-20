------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ I N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

package body System.Img_Int is

   procedure Set_Digits
     (T : Integer;
      S : in out String;
      P : in out Natural);
   --  Set digits of absolute value of T, which is zero or negative. We work
   --  with the negative of the value so that the largest negative number is
   --  not a special case.

   -------------------
   -- Image_Integer --
   -------------------

   procedure Image_Integer
     (V : Integer;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);

   begin
      if V >= 0 then
         S (1) := ' ';
         P := 1;
      else
         P := 0;
      end if;

      Set_Image_Integer (V, S, P);
   end Image_Integer;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
     (T : Integer;
      S : in out String;
      P : in out Natural)
   is
   begin
      if T <= -10 then
         Set_Digits (T / 10, S, P);
         P := P + 1;
         S (P) := Character'Val (48 - (T rem 10));
      else
         P := P + 1;
         S (P) := Character'Val (48 - T);
      end if;
   end Set_Digits;

   -----------------------
   -- Set_Image_Integer --
   -----------------------

   procedure Set_Image_Integer
     (V : Integer;
      S : in out String;
      P : in out Natural)
   is
   begin
      if V >= 0 then
         Set_Digits (-V, S, P);
      else
         P := P + 1;
         S (P) := '-';
         Set_Digits (V, S, P);
      end if;
   end Set_Image_Integer;

end System.Img_Int;
