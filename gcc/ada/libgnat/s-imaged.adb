------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . I M A G E _ D                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

with System.Image_I;
with System.Img_Util; use System.Img_Util;

package body System.Image_D is

   package Image_I is new System.Image_I (Int);

   procedure Set_Image_Integer
     (V : Int;
      S : in out String;
      P : in out Natural)
     renames Image_I.Set_Image_Integer;

   -------------------
   -- Image_Decimal --
   -------------------

   procedure Image_Decimal
     (V     : Int;
      S     : in out String;
      P     : out Natural;
      Scale : Integer)
   is
      pragma Assert (S'First = 1);
   begin
      --  Add space at start for non-negative numbers

      if V >= 0 then
         S (1) := ' ';
         P := 1;
      else
         P := 0;
      end if;

      Set_Image_Decimal (V, S, P, Scale, 1, Integer'Max (1, Scale), 0);
   end Image_Decimal;

   -----------------------
   -- Set_Image_Decimal --
   -----------------------

   procedure Set_Image_Decimal
     (V     : Int;
      S     : in out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
   is
      Maxdigs : constant Natural := Int'Width;
      --  Maximum length needed for Image of an Int

      Digs  : String (1 .. Maxdigs);
      Ndigs : Natural;
      --  Buffer for the image of the integer value

   begin
      --  Set the first character like Image

      if V >= 0 then
         Digs (1) := ' ';
         Ndigs := 1;
      else
         Ndigs := 0;
      end if;

      Set_Image_Integer (V, Digs, Ndigs);

      pragma Assert (1 <= Ndigs and then Ndigs <= Maxdigs);

      Set_Decimal_Digits (Digs, Ndigs, S, P, Scale, Fore, Aft, Exp);
   end Set_Image_Decimal;

end System.Image_D;
