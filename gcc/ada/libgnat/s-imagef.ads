------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ F                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2024, Free Software Foundation, Inc.       --
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
--  and ``Ada.Text_IO.Fixed_IO`` conversions routines for for ordinary fixed
--  point types whose Small is the ratio of two Int values.

generic

   type Int is range <>;
   type Uns is mod <>;

   with procedure Scaled_Divide
          (X, Y, Z : Int;
           Q, R : out Int;
           Round : Boolean);

package System.Image_F is

   procedure Image_Fixed
     (V    : Int;
      S    : in out String;
      P    : out Natural;
      Num  : Int;
      Den  : Int;
      For0 : Natural;
      Aft0 : Natural);
   --  Computes fixed_type'Image (V), where V is the integer value (in units of
   --  small) of an ordinary fixed point type with small Num/Den, and stores
   --  the result in S (1 .. P), updating P on return. The result is computed
   --  according to the rules for image for fixed-point types (RM 4.10(14)).
   --  For0 and Aft0 are the values of the Fore and Aft attributes for the
   --  fixed point type whose mantissa type is Int and whose small is Num/Den.
   --  This function is used only for fixed point whose Small is the ratio of
   --  two Int values (see package System.Image_R for the handling of other
   --  ordinary fixed-point types). The caller guarantees that S is long enough
   --  to hold the result and has a lower bound of 1.

   procedure Set_Image_Fixed
     (V    : Int;
      S    : in out String;
      P    : in out Natural;
      Num  : Int;
      Den  : Int;
      For0 : Natural;
      Aft0 : Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);
   --  Sets the image of V, where V is the integer value (in units of small)
   --  of a fixed point type with small Num/Den, starting at S (P + 1) and
   --  updating P to point to the last character stored, the caller promises
   --  that the buffer is large enough and no check is made. Constraint_Error
   --  will not necessarily be raised if this requirement is violated, since
   --  it is perfectly valid to compile this unit with checks off. For0 and
   --  Aft0 are the values of the Fore and Aft attributes for the fixed point
   --  type whose mantissa type is Int and whose small is Num/Den. The Fore,
   --  Aft and Exp can be set to any valid values for use by Text_IO.Fixed_IO.

end System.Image_F;
