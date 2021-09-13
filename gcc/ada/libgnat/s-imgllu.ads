------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
--  modular integer types larger than Unsigned, and also for conversion
--  operations required in Text_IO.Modular_IO for such types.

with System.Image_U;
with System.Unsigned_Types;

package System.Img_LLU is
   pragma Pure;

   subtype Long_Long_Unsigned is Unsigned_Types.Long_Long_Unsigned;

   package Impl is new Image_U (Long_Long_Unsigned);

   procedure Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : out Natural)
     renames Impl.Image_Unsigned;

   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : in out Natural)
     renames Impl.Set_Image_Unsigned;

end System.Img_LLU;
