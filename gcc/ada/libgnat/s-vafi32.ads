------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . V A L _ F I X E D _ 3 2                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

--  This package contains routines for scanning values for decimal fixed point
--  types up to 32-bit small and mantissa, for use in Text_IO.Decimal_IO, and
--  the Value attribute for such decimal types.

with Interfaces;
with System.Arith_32;
with System.Value_F;

package System.Val_Fixed_32 is
   pragma Preelaborate;

   subtype Int32 is Interfaces.Integer_32;
   subtype Uns32 is Interfaces.Unsigned_32;

   package Impl is new Value_F (Int32, Uns32, Arith_32.Scaled_Divide32);

   function Scan_Fixed32
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Num : Int32;
      Den : Int32) return Int32
     renames Impl.Scan_Fixed;

   function Value_Fixed32
     (Str : String; Num : Int32; Den : Int32) return Int32
     renames Impl.Value_Fixed;

end System.Val_Fixed_32;
