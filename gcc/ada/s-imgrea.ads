------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      S Y S T E M . I M G _ R E A L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  Image for fixed and float types (also used for Float_IO/Fixed_IO output)

package System.Img_Real is
pragma Preelaborate (Img_Real);

   function Image_Ordinary_Fixed_Point
     (V    : Long_Long_Float;
      Aft  : Natural)
      return String;
   --  Computes the image of V and returns the result according to the rules
   --  for image for fixed-point types (RM 3.5(34)), where Aft is the value of
   --  the Aft attribute for the fixed-point type. This function is used only
   --  for ordinary fixed point (see package System.Img_Dec for handling of
   --  decimal fixed-point).

   function Image_Floating_Point
     (V    : Long_Long_Float;
      Digs : Natural)
      return String;
   --  Computes the image of V and returns the result according to the rules
   --  for image for foating-point types (RM 3.5(33)), where Digs is the value
   --  of the Digits attribute for the floating-point type.

   procedure Set_Image_Real
     (V    : Long_Long_Float;
      S    : out String;
      P    : in out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);
   --  Sets the image of V starting at S (P + 1), updating P to point to the
   --  last character stored, the caller promises that the buffer is large
   --  enough and no check is made for this. Constraint_Error will not
   --  necessarily be raised if this is violated, since it is perfectly valid
   --  to compile this unit with checks off). The Fore, Aft and Exp values
   --  can be set to any valid values for the case of use from Text_IO.

end System.Img_Real;
