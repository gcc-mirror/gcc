------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . I M G _ L L D                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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

--  Image for decimal fixed types where the size of the corresponding integer
--  type does exceeds Integer'Size (also used for Text_IO.Decimal_IO output)

package System.Img_LLD is
pragma Preelaborate (Img_LLD);

   function Image_Long_Long_Decimal
     (V     : Long_Long_Integer;
      Scale : Integer)
      return  String;
   --  Compute 'Image of V, the integer value (in units of delta) of a decimal
   --  type whose Scale is as given and returns the result. The image is given
   --  by the rules in RM 3.5(34) for fixed-point type image functions.

   procedure Set_Image_Long_Long_Decimal
     (V     : Long_Long_Integer;
      S     : out String;
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
   --  by Text_IO.Decimal_IO.

end System.Img_LLD;
