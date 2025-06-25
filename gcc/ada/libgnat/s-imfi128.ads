------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . I M G _ F I X E D _ 1 2 8                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains the routines for supporting the Image attribute for
--  ordinary fixed point types up to 128-bit small and mantissa.

with Interfaces;
with System.Arith_128;
with System.Image_F;

package System.Img_Fixed_128 is

   subtype Int128 is Interfaces.Integer_128;

   package Impl is new Image_F (Int128, Arith_128.Scaled_Divide128);

   procedure Image_Fixed128
     (V    : Int128;
      S    : in out String;
      P    : out Natural;
      Num  : Int128;
      Den  : Int128;
      For0 : Natural;
      Aft0 : Natural)
     renames Impl.Image_Fixed;

   procedure Set_Image_Fixed128
     (V    : Int128;
      S    : in out String;
      P    : in out Natural;
      Num  : Int128;
      Den  : Int128;
      For0 : Natural;
      Aft0 : Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
     renames Impl.Set_Image_Fixed;

end System.Img_Fixed_128;
