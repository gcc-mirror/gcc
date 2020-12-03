------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
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

with System.Float_Control;
with System.Unsigned_Types; use System.Unsigned_Types;
with System.Val_Util;       use System.Val_Util;
with System.Value_R;

package body System.Val_Real is

   package Impl is new Value_R (Long_Long_Unsigned, Floating => True);

   function Integer_to_Real
     (Str   : String;
      Val   : Long_Long_Unsigned;
      Base  : Unsigned;
      Scale : Integer;
      Minus : Boolean) return Long_Long_Float;
   --  Convert the real value from integer to real representation

   ---------------------
   -- Integer_to_Real --
   ---------------------

   function Integer_to_Real
     (Str   : String;
      Val   : Long_Long_Unsigned;
      Base  : Unsigned;
      Scale : Integer;
      Minus : Boolean) return Long_Long_Float
   is
      pragma Unsuppress (Range_Check);

      R_Val : Long_Long_Float;

   begin
      --  We call the floating-point processor reset routine so we can be sure
      --  that the processor is properly set for conversions. This is notably
      --  needed on Windows, where calls to the operating system randomly reset
      --  the processor into 64-bit mode.

      System.Float_Control.Reset;

      --  Compute the final value

      R_Val := Long_Long_Float (Val) * Long_Long_Float (Base) ** Scale;

      --  Finally deal with initial minus sign, note that this processing is
      --  done even if Uval is zero, so that -0.0 is correctly interpreted.

      return (if Minus then -R_Val else R_Val);

   exception
      when Constraint_Error => Bad_Value (Str);
   end Integer_to_Real;

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
      (Str : String;
       Ptr : not null access Integer;
       Max : Integer)
      return Long_Long_Float
   is
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Long_Long_Unsigned;

   begin
      Val := Impl.Scan_Raw_Real (Str, Ptr, Max, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Minus);
   end Scan_Real;

   ----------------
   -- Value_Real --
   ----------------

   function Value_Real (Str : String) return Long_Long_Float is
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Long_Long_Unsigned;

   begin
      Val := Impl.Value_Raw_Real (Str, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Minus);
   end Value_Real;

end System.Val_Real;
