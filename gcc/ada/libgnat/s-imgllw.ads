------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L W                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

--  Contains the routine for computing the image  of signed and unsigned
--  integers whose size > Integer'Size for use by Text_IO.Integer_IO,
--  Text_IO.Modular_IO.

with System.Unsigned_Types;

package System.Img_LLW is
   pragma Pure;

   procedure Set_Image_Width_Long_Long_Integer
     (V : Long_Long_Integer;
      W : Integer;
      S : out String;
      P : in out Natural);
   --  Sets the signed image of V in decimal format, starting at S (P + 1),
   --  updating P to point to the last character stored. The image includes
   --  a leading minus sign if necessary, but no leading spaces unless W is
   --  positive, in which case leading spaces are output if necessary to ensure
   --  that the output string is no less than W characters long. The caller
   --  promises that the buffer is large enough and no check is made for this.
   --  Constraint_Error will not necessarily be raised if this is violated,
   --  since it is perfectly valid to compile this unit with checks off.

   procedure Set_Image_Width_Long_Long_Unsigned
     (V : System.Unsigned_Types.Long_Long_Unsigned;
      W : Integer;
      S : out String;
      P : in out Natural);
   --  Sets the unsigned image of V in decimal format, starting at S (P + 1),
   --  updating P to point to the last character stored. The image includes no
   --  leading spaces unless W is positive, in which case leading spaces are
   --  output if necessary to ensure that the output string is no less than
   --  W characters long. The caller promises that the buffer is large enough
   --  and no check is made for this. Constraint_Error will not necessarily be
   --  raised if this is violated, since it is perfectly valid to compile this
   --  unit with checks off.

end System.Img_LLW;
