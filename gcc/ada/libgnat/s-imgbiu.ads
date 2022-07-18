------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ B I U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Contains the routine for computing the image in based format of signed and
--  unsigned integers up to ``Integer`` for use by ``Ada.Text_IO.Integer_IO``
--  and ``Ada.Text_IO.Modular_IO``.

with System.Image_B;
with System.Unsigned_Types;

package System.Img_BIU is
   pragma Pure;

   subtype Unsigned is Unsigned_Types.Unsigned;

   package Impl is new Image_B (Integer, Unsigned);

   procedure Set_Image_Based_Integer
     (V : Integer;
      B : Natural;
      W : Integer;
      S : out String;
      P : in out Natural)
     renames Impl.Set_Image_Based_Integer;
   --  Sets the signed image of V in based format, using base value B (2..16)
   --  starting at S (P + 1), updating P to point to the last character stored.
   --  The image includes a leading minus sign if necessary, but no leading
   --  spaces unless W is positive, in which case leading spaces are output if
   --  necessary to ensure that the output string is no less than W characters
   --  long. The caller promises that the buffer is large enough and no check
   --  is made for this. Constraint_Error will not necessarily be raised if
   --  this is violated, since it is perfectly valid to compile this unit with
   --  checks off.

   procedure Set_Image_Based_Unsigned
     (V : Unsigned;
      B : Natural;
      W : Integer;
      S : out String;
      P : in out Natural)
     renames Impl.Set_Image_Based_Unsigned;
   --  Sets the unsigned image of V in based format, using base value B (2..16)
   --  starting at S (P + 1), updating P to point to the last character stored.
   --  The image includes no leading spaces unless W is positive, in which case
   --  leading spaces are output if necessary to ensure that the output string
   --  is no less than W characters long. The caller promises that the buffer
   --  is large enough and no check is made for this. Constraint_Error will not
   --  necessarily be raised if this is violated, since it is perfectly valid
   --  to compile this unit with checks off).

end System.Img_BIU;
