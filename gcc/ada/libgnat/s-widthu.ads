------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W I D T H _ U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  Compute Width attribute for non-static type derived from a modular integer
--  type. The arguments Lo, Hi are the bounds of the type.

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

generic

   type Uns is mod <>;

package System.Width_U
  with Pure
is
   package BI_Ghost renames Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
   subtype Big_Integer is BI_Ghost.Big_Integer with Ghost;
   subtype Big_Natural is BI_Ghost.Big_Natural with Ghost;
   subtype Big_Positive is BI_Ghost.Big_Positive with Ghost;
   use type BI_Ghost.Big_Integer;

   package Unsigned_Conversion is
     new BI_Ghost.Unsigned_Conversions (Int => Uns);

   function Big (Arg : Uns) return Big_Integer renames
     Unsigned_Conversion.To_Big_Integer;

   Big_10 : constant Big_Integer := Big (Uns'(10)) with Ghost;

   --  Maximum value of exponent for 10 that fits in Uns'Base
   function Max_Log10 return Natural is
     (case Uns'Base'Size is
        when 8   => 2,
        when 16  => 4,
        when 32  => 9,
        when 64  => 19,
        when 128 => 38,
        when others => raise Program_Error)
   with Ghost;

   function Width (Lo, Hi : Uns) return Natural
   with
     Post =>
       (declare
          W : constant Natural := System.Width_U.Width'Result;
        begin
          (if Lo > Hi then W = 0
           else W > 0
             and then W <= Max_Log10 + 2
             and then Big (Lo) < Big_10 ** (W - 1)
             and then Big (Hi) < Big_10 ** (W - 1)));

end System.Width_U;
