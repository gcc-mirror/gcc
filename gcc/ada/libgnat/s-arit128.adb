------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . A R I T H _ 1 2 8                      --
--                                                                          --
--                                 B o d y                                  --
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

with System.Arith_Double;

package body System.Arith_128 is

   subtype Uns128 is Interfaces.Unsigned_128;
   subtype Uns64  is Interfaces.Unsigned_64;

   use Interfaces;

   package Impl is new Arith_Double (Int128, Uns128, Uns64);

   function Add_With_Ovflo_Check128 (X, Y : Int128) return Int128
     renames Impl.Add_With_Ovflo_Check;

   function Subtract_With_Ovflo_Check128 (X, Y : Int128) return Int128
     renames Impl.Subtract_With_Ovflo_Check;

   function Multiply_With_Ovflo_Check128 (X, Y : Int128) return Int128
     renames Impl.Multiply_With_Ovflo_Check;

   procedure Scaled_Divide128
     (X, Y, Z : Int128;
      Q, R    : out Int128;
      Round   : Boolean)
     renames Impl.Scaled_Divide;

   procedure Double_Divide128
     (X, Y, Z : Int128;
      Q, R    : out Int128;
      Round   : Boolean)
     renames Impl.Double_Divide;

end System.Arith_128;
