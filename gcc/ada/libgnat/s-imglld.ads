------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L D                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  Image for decimal fixed types where the size of the corresponding integer
--  type does exceeds Integer'Size (also used for Text_IO.Decimal_IO output)

package System.Img_LLD is
   pragma Pure;

   procedure Image_Long_Long_Decimal
     (V     : Long_Long_Integer;
      S     : in out String;
      P     : out Natural;
      Scale : Integer);
   --  Computes fixed_type'Image (V), where V is the integer value (in units of
   --  delta) of a decimal type whose Scale is as given and store the result in
   --  S (P + 1 .. L), updating P to the value of L. The image is given by the
   --  rules in RM 3.5(34) for fixed-point type image functions. The caller
   --  guarantees that S is long enough to hold the result. S need not have a
   --  lower bound of 1.

   procedure Set_Image_Long_Long_Decimal
     (V     : Long_Long_Integer;
      S     : in out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural);
   --  Sets the image of V, where V is the integer value (in units of delta)
   --  of a decimal type with the given Scale, starting at S (P + 1), updating
   --  P to point to the last character stored, the caller promises that the
   --  buffer is large enough and no check is made for this. Constraint_Error
   --  will not necessarily be raised if this requirement is violated, since
   --  it is perfectly valid to compile this unit with checks off. The Fore,
   --  Aft and Exp values can be set to any valid values for the case of use
   --  by Text_IO.Decimal_IO. Note that there is no leading space stored.

end System.Img_LLD;
