------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L I                        --
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

--  This package provides the subprograms supporting the ``Image`` attribute
--  and ``Ada.Text_IO.Integer_IO`` conversions routines for signed integer
--  types larger than Size ``Integer'Size``.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

with System.Image_I;
with System.Unsigned_Types;
with System.Vs_LLI;
with System.Vs_LLU;

package System.Img_LLI
  with SPARK_Mode
is
   subtype Long_Long_Unsigned is Unsigned_Types.Long_Long_Unsigned;

   package Impl is new Image_I
     (Int    => Long_Long_Integer,
      Uns    => Long_Long_Unsigned,
      U_Spec => System.Vs_LLU.Spec,
      I_Spec => System.Vs_LLI.Spec);

   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in out String;
      P : out Natural)
     renames Impl.Image_Integer;
   --  Computes Long_Long_Integer'Image (``V``) and stores the result in
   --  ``S`` (1 .. ``P``) setting the resulting value of ``P``. The caller
   --  guarantees that ``S`` is long enough to hold the result, and that
   --  ``S``'First is 1.
   --
   --  If ``V`` is not negative, the subprogram writes the leading blank in
   --  ``S``. It then calls *Set_Image_Long_Long_Integer*.

   procedure Set_Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in out String;
      P : in out Natural)
     renames Impl.Set_Image_Integer;
   --  Stores the image of ``V`` in ``S`` starting at ``S`` (``P`` + 1),
   --  ``P`` is updated to point to the last character stored. The value
   --  stored is identical to the value of Long_Long_Integer'Image (``V``)
   --  except that no leading space is stored when ``V`` is non-negative. The
   --  caller guarantees that ``S`` is long enough to hold the result. ``S``
   --  need not have a lower bound of 1.
   --
   --  If ``V`` is negative, the subprogram writes the leading '-' character,
   --  otherwise work with -``V`` (always work with negative value to avoid
   --  overflow: the largest negative number is not a special case).
   --
   --  This subprogram then uses recursion: if the value is equal or less than
   --  -10, recurse with the value divided by 10. Then add the digit for the
   --  remainder.

end System.Img_LLI;
