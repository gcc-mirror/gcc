------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . I M G _ L L W                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2000 Free Software Foundation, Inc.         --
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

--  Contains the routine for computing the image  of signed and unsigned
--  integers whose size > Integer'Size for use by Text_IO.Integer_IO,
--  Text_IO.Modular_IO.

with System.Unsigned_Types;

package System.Img_LLW is
pragma Pure (Img_LLW);

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
