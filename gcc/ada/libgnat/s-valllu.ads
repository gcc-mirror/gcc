------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package contains routines for scanning modular Long_Long_Unsigned
--  values for use in Text_IO.Modular_IO, and the Value attribute.

with System.Unsigned_Types;
with System.Value_U;

package System.Val_LLU is
   pragma Preelaborate;

   subtype Long_Long_Unsigned is Unsigned_Types.Long_Long_Unsigned;

   package Impl is new Value_U (Long_Long_Unsigned);

   function Scan_Raw_Long_Long_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Unsigned
     renames Impl.Scan_Raw_Unsigned;

   function Scan_Long_Long_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Unsigned
     renames Impl.Scan_Unsigned;

   function Value_Long_Long_Unsigned
     (Str : String) return Long_Long_Unsigned
     renames Impl.Value_Unsigned;

end System.Val_LLU;
