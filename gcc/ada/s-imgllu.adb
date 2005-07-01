------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L U                        --
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

with System.Unsigned_Types; use System.Unsigned_Types;

package body System.Img_LLU is

   ------------------------------
   -- Image_Long_Long_Unsigned --
   ------------------------------

   function Image_Long_Long_Unsigned
     (V    : Long_Long_Unsigned)
      return String
   is
      P : Natural;
      S : String (1 .. Long_Long_Unsigned'Width);

   begin
      P := 1;
      S (P) := ' ';
      Set_Image_Long_Long_Unsigned (V, S, P);
      return S (1 .. P);
   end Image_Long_Long_Unsigned;

   -----------------------
   -- Set_Image_Long_Long_Unsigned --
   -----------------------

   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : out String;
      P : in out Natural)
   is
      procedure Set_Digits (T : Long_Long_Unsigned);
      --  Set digits of absolute value of T

      procedure Set_Digits (T : Long_Long_Unsigned) is
      begin
         if T >= 10 then
            Set_Digits (T / 10);
            P := P + 1;
            S (P) := Character'Val (48 + (T rem 10));

         else
            P := P + 1;
            S (P) := Character'Val (48 + T);
         end if;
      end Set_Digits;

   --  Start of processing for Set_Image_Long_Long_Unsigned

   begin
      Set_Digits (V);

   end Set_Image_Long_Long_Unsigned;

end System.Img_LLU;
