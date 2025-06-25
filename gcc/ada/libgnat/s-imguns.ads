------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ U N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
--  and ``Ada.Text_IO.Modular_IO`` conversions routines for modular integer
--  types up to size ``Unsigned'Size``.

with System.Image_U;
with System.Unsigned_Types;

package System.Img_Uns
  with SPARK_Mode
is
   subtype Unsigned is Unsigned_Types.Unsigned;

   package Impl is new Image_U (Uns => Unsigned);

   procedure Image_Unsigned
     (V : Unsigned;
      S : in out String;
      P : out Natural)
     renames Impl.Image_Unsigned;
   --  Computes Unsigned'Image (``V``) and stores the result in
   --  ``S`` (1 .. ``P``) setting the resulting value of ``P``. The caller
   --  guarantees that ``S`` is long enough to hold the result, and that
   --  ``S``'First is 1.
   --
   --  The subprogram writes the leading blank in ``S`` and calls
   --  *Set_Image_Unsigned*.

   procedure Set_Image_Unsigned
     (V : Unsigned;
      S : in out String;
      P : in out Natural)
     renames Impl.Set_Image_Unsigned;
   --  Stores the image of ``V`` in ``S`` starting at ``S`` (``P`` + 1), ``P``
   --  is updated to point to the last character stored. The value stored is
   --  identical to the value of Unsigned'Image (``V``) except that no leading
   --  space is stored. The caller guarantees that ``S`` is long enough to hold
   --  the result. ``S`` need not have a lower bound of 1.
   --
   --  This subprogram uses recursion: if the value is equal or greater than
   --  10, recurse with the value divided by 10. Then add the digit for the
   --  remainder.

end System.Img_Uns;
