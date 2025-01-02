------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . I M G _ D E C I M A L _ 3 2                 --
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

--  This package provides the subprograms supporting the ``Image`` attribute
--  and ``Ada.Text_IO.Decimal_IO`` conversions routines for decimal fixed point
--  types up to 32-bit mantissa.

with Interfaces;
with System.Image_D;

package System.Img_Decimal_32 is

   subtype Int32 is Interfaces.Integer_32;
   subtype Uns32 is Interfaces.Unsigned_32;

   package Impl is new Image_D (Int32, Uns32);

   procedure Image_Decimal32
     (V     : Int32;
      S     : in out String;
      P     : out Natural;
      Scale : Integer)
     renames Impl.Image_Decimal;
   --  Computes fixed_type'Image (V), where V is the integer value (in units of
   --  delta) of a decimal type whose Scale is as given and stores the result
   --  S (1 .. P), updating P on return. The result is computed according to
   --  the rules for image for fixed-point types (RM 4.10(14)). The caller
   --  guarantees that S is long enough to hold the result and has a lower
   --  bound of 1.

   procedure Set_Image_Decimal32
     (V     : Int32;
      S     : in out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
     renames Impl.Set_Image_Decimal;
   --  Sets the image of V, where V is the integer value (in units of delta)
   --  of a decimal type with the specified Scale, starting at S (P + 1) and
   --  updating P to point to the last character stored, the caller promises
   --  that the buffer is large enough and no check is made. Constraint_Error
   --  will not necessarily be raised if this requirement is violated, since
   --  it is perfectly valid to compile this unit with checks off. The Fore,
   --  Aft and Exp values can be set to any valid values for the case of use
   --  by Text_IO.Decimal_IO.

end System.Img_Decimal_32;
