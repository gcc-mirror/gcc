------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . N U M E R I C S . A U X . L O N G _ L O N G _ F L O A T      --
--                                                                          --
--                                 S p e c                                  --
--                    (Instrinsic Version, Long Long Float)                 --
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

--  This package provides the basic computational interface for the
--  generic elementary functions. With the intrinsic version, the
--  compiler can use its knowledge of the functions to select the most
--  suitable implementation. It is thus quite portable. These
--  interfaces are suitable for cases in which Long Long Float and C's
--  long double share the same representation.

with Ada.Numerics.Aux_Linker_Options;
pragma Warnings (Off, Ada.Numerics.Aux_Linker_Options);

package Ada.Numerics.Aux_Long_Long_Float is
   pragma Pure;

   subtype T is Long_Long_Float;

   --  We import these functions as intrinsics. Note that we label them
   --  all as pure functions, because indeed all of them are in fact pure.

   function Sin (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "sinl";

   function Cos (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "cosl";

   function Tan (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "tanl";

   function Exp (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "expl";

   function Sqrt (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "sqrtl";

   function Log (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "logl";

   function Acos (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "acosl";

   function Asin (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "asinl";

   function Atan (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "atanl";

   function Sinh (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "sinhl";

   function Cosh (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "coshl";

   function Tanh (X : T) return T with
     Import, Convention => Intrinsic, External_Name => "tanhl";

   function Pow (X, Y : T) return T with
     Import, Convention => Intrinsic, External_Name => "powl";

end Ada.Numerics.Aux_Long_Long_Float;
