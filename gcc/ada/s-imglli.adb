------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . I M G _ L L I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

package body System.Img_LLI is

   -----------------------------
   -- Image_Long_Long_Integer --
   -----------------------------

   function Image_Long_Long_Integer (V : Long_Long_Integer) return String is
      P : Natural;
      S : String (1 .. Long_Long_Integer'Width);

   begin
      if V >= 0 then
         P := 1;
         S (P) := ' ';
      else
         P := 0;
      end if;

      Set_Image_Long_Long_Integer (V, S, P);
      return S (1 .. P);
   end Image_Long_Long_Integer;

   ---------------------------------
   -- Set_Image_Long_Long_Integer --
   ---------------------------------

   procedure Set_Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : out String;
      P : in out Natural)
   is
      procedure Set_Digits (T : Long_Long_Integer);
      --  Set digits of absolute value of T, which is zero or negative. We work
      --  with the negative of the value so that the largest negative number is
      --  not a special case.

      procedure Set_Digits (T : Long_Long_Integer) is
      begin
         if T <= -10 then
            Set_Digits (T / 10);
            P := P + 1;
            S (P) := Character'Val (48 - (T rem 10));

         else
            P := P + 1;
            S (P) := Character'Val (48 - T);
         end if;
      end Set_Digits;

   --  Start of processing for Set_Image_Long_Long_Integer

   begin
      if V >= 0 then
         Set_Digits (-V);

      else
         P := P + 1;
         S (P) := '-';
         Set_Digits (V);
      end if;

   end Set_Image_Long_Long_Integer;

end System.Img_LLI;
