------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ F L T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

--  This package contains routines for scanning real values for floating point
--  type Float, for use in Text_IO.Float_IO and the Value attribute.

with System.Powten_Flt;
with System.Unsigned_Types;
with System.Val_Real;

package System.Val_Flt is
   pragma Preelaborate;

   package Impl is new Val_Real
     (Float,
      System.Powten_Flt.Maxpow,
      System.Powten_Flt.Powfive'Address,
      System.Null_Address,
      System.Null_Address,
      System.Null_Address,
      Unsigned_Types.Unsigned);

   function Scan_Float
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Float
     renames Impl.Scan_Real;

   function Value_Float (Str : String) return Float
     renames Impl.Value_Real;

end System.Val_Flt;
