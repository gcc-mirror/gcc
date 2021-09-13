------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ R                        --
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

--  This package contains routines for the Image attribute of real types, and
--  is also for Float_IO/Fixed_IO output.

generic

   type Num is digits <>;

   Maxpow : Positive;

   Powten_Address : System.Address;

   type Uns is mod <>;

   with procedure Set_Image_Unsigned
     (V : Uns;
      S : in out String;
      P : in out Natural);

package System.Image_R is
   pragma Pure;

   procedure Image_Fixed_Point
     (V   : Num;
      S   : in out String;
      P   : out Natural;
      Aft : Natural);
   --  Computes fixed_type'Image (V) and returns the result in S (1 .. P)
   --  updating P on return. The result is computed according to the rules for
   --  image for fixed-point types (RM 3.5(34)), where Aft is the value of the
   --  Aft attribute for the fixed-point type. The caller guarantees that S is
   --  long enough to hold the result and has a lower bound of 1.
   --
   --  Note: this procedure should NOT be called with V = -0.0 or V = +/-Inf.

   procedure Image_Floating_Point
     (V    : Num;
      S    : in out String;
      P    : out Natural;
      Digs : Natural);
   --  Computes Uns'Image (V) and returns the result in S (1 .. P) updating P
   --  on return. The result is computed according to the rules for image for
   --  floating-point types (RM 3.5(33)), where Digs is the value of the Digits
   --  attribute for the floating-point type. The caller guarantees that S is
   --  long enough to hold the result and has a lower bound of 1.

   procedure Set_Image_Real
     (V    : Num;
      S    : in out String;
      P    : in out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);
   --  Sets the image of V starting at S (P + 1), updating P to point to the
   --  last character stored, the caller promises that the buffer is large
   --  enough and no check is made for this. Constraint_Error will not
   --  necessarily be raised if this is violated, since it is perfectly valid
   --  to compile this unit with checks off). The Fore, Aft and Exp values
   --  can be set to any valid values for the case of use from Text_IO. Note
   --  that no space is stored at the start for non-negative values.

end System.Image_R;
