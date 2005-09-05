------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L U                        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains the routines for supporting the Image attribute for
--  unsigned (modular) integer types larger than Size Unsigned'Size, and also
--  for conversion operations required in Text_IO.Modular_IO for such types.

with System.Unsigned_Types;

package System.Img_LLU is
   pragma Pure;

   function Image_Long_Long_Unsigned
     (V :    System.Unsigned_Types.Long_Long_Unsigned)
      return String;
   --  Computes Long_Long_Unsigned'Image (V) and returns the result.

   procedure Set_Image_Long_Long_Unsigned
     (V : System.Unsigned_Types.Long_Long_Unsigned;
      S : out String;
      P : in out Natural);
   --  Sets the image of V starting at S (P + 1) with no leading spaces (i.e.
   --  Text_IO format where Width = 0), starting at S (P + 1), updating P
   --  to point to the last character stored. The caller promises that the
   --  buffer is large enough and no check is made for this (Constraint_Error
   --  will not be necessarily raised if this is violated since it is perfectly
   --  valid to compile this unit with checks off).

end System.Img_LLU;
