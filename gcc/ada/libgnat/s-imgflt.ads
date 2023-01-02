------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ F L T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2021-2023, Free Software Foundation, Inc.       --
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

--  This package contains routines for the Image attribute of floating point
--  types based on Float, also used for Float_IO output.

with System.Image_R;
with System.Img_Uns;
with System.Powten_Flt;
with System.Unsigned_Types;

package System.Img_Flt is

   package Impl is new Image_R
     (Float,
      System.Powten_Flt.Maxpow,
      System.Powten_Flt.Powten'Address,
      Unsigned_Types.Unsigned,
      System.Img_Uns.Set_Image_Unsigned);

   procedure Image_Float
     (V    : Float;
      S    : in out String;
      P    : out Natural;
      Digs : Natural)
     renames Impl.Image_Floating_Point;

   procedure Set_Image_Float
     (V    : Float;
      S    : in out String;
      P    : in out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
     renames Impl.Set_Image_Real;

end System.Img_Flt;
