------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L U                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

package body System.Img_LLU is

   ------------------------------
   -- Image_Long_Long_Unsigned --
   ------------------------------

   procedure Image_Long_Long_Unsigned
     (V : System.Unsigned_Types.Long_Long_Unsigned;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);
   begin
      S (1) := ' ';
      P := 1;
      Set_Image_Long_Long_Unsigned (V, S, P);
   end Image_Long_Long_Unsigned;

   ----------------------------------
   -- Set_Image_Long_Long_Unsigned --
   ----------------------------------

   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : in out Natural)
   is
   begin
      if V >= 10 then
         Set_Image_Long_Long_Unsigned (V / 10, S, P);
         P := P + 1;
         S (P) := Character'Val (48 + (V rem 10));

      else
         P := P + 1;
         S (P) := Character'Val (48 + V);
      end if;
   end Set_Image_Long_Long_Unsigned;

end System.Img_LLU;
