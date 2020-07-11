------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ D E C                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

with System.Val_Real; use System.Val_Real;

package body System.Val_Dec is

   ------------------
   -- Scan_Decimal --
   ------------------

   --  For decimal types where Size < Integer'Size, it is fine to use
   --  the floating-point circuit, since it certainly has sufficient
   --  precision for any reasonable hardware, and we just don't support
   --  things on junk hardware.

   function Scan_Decimal
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Integer
   is
      Val : Long_Long_Float;
   begin
      Val := Scan_Real (Str, Ptr, Max);
      return Integer (Val * 10.0 ** Scale);
   end Scan_Decimal;

   -------------------
   -- Value_Decimal --
   -------------------

   --  Again, we use the real circuit for this purpose

   function Value_Decimal (Str : String; Scale : Integer) return Integer is
   begin
      return Integer (Value_Real (Str) * 10.0 ** Scale);
   end Value_Decimal;

end System.Val_Dec;
