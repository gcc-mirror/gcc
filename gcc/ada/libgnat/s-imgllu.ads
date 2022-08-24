------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

--  This package contains the routines for supporting the Image attribute for
--  modular integer types larger than Unsigned, and also for conversion
--  operations required in Text_IO.Modular_IO for such types.

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

with System.Image_U;
with System.Unsigned_Types;
with System.Val_LLU;
with System.Wid_LLU;

package System.Img_LLU
  with SPARK_Mode
is
   subtype Long_Long_Unsigned is Unsigned_Types.Long_Long_Unsigned;

   package Impl is new Image_U
     (Uns                                => Long_Long_Unsigned,
      Uns_Option                         => Val_LLU.Impl.Uns_Option,
      Unsigned_Width_Ghost               =>
         Wid_LLU.Width_Long_Long_Unsigned (0, Long_Long_Unsigned'Last),
      Only_Decimal_Ghost                 => Val_LLU.Impl.Only_Decimal_Ghost,
      Hexa_To_Unsigned_Ghost             =>
         Val_LLU.Impl.Hexa_To_Unsigned_Ghost,
      Wrap_Option                        => Val_LLU.Impl.Wrap_Option,
      Scan_Based_Number_Ghost            =>
         Val_LLU.Impl.Scan_Based_Number_Ghost,
      Is_Unsigned_Ghost                  => Val_LLU.Impl.Is_Unsigned_Ghost,
      Value_Unsigned                     => Val_LLU.Impl.Value_Unsigned,
      Prove_Iter_Scan_Based_Number_Ghost =>
         Val_LLU.Impl.Prove_Iter_Scan_Based_Number_Ghost,
      Prove_Scan_Only_Decimal_Ghost      =>
         Val_LLU.Impl.Prove_Scan_Only_Decimal_Ghost);

   procedure Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : out Natural)
     renames Impl.Image_Unsigned;

   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : in out Natural)
     renames Impl.Set_Image_Unsigned;

end System.Img_LLU;
