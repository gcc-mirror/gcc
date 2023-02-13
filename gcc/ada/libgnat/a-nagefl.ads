------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       A D A . N U M E R I C S . A U X _ G E N E R I C _ F L O A T        --
--                                                                          --
--                                 S p e c                                  --
--                            (Generic Wrapper)                             --
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

--  This package provides the basic computational interface for the generic
--  elementary functions.

--  This version here delegates to interfaces that typically import as
--  intrinsics the expected math functions.

with Ada.Numerics.Aux_Long_Long_Float;
with Ada.Numerics.Aux_Long_Float;
with Ada.Numerics.Aux_Float;
with Ada.Numerics.Aux_Short_Float;

generic
   type T is digits <>;
package Ada.Numerics.Aux_Generic_Float is
   pragma Pure;

   package LLF renames Aux_Long_Long_Float;
   package LF renames Aux_Long_Float;
   package F renames Aux_Float;
   package SF renames Aux_Short_Float;

   function Sin (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Sin (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Sin (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Sin (F.T (X)))
         else T'Base (SF.Sin (SF.T (X))));

   function Cos (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Cos (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Cos (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Cos (F.T (X)))
         else T'Base (SF.Cos (SF.T (X))));

   function Tan (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Tan (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Tan (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Tan (F.T (X)))
         else T'Base (SF.Tan (SF.T (X))));

   function Exp (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Exp (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Exp (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Exp (F.T (X)))
         else T'Base (SF.Exp (SF.T (X))));

   function Sqrt (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Sqrt (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Sqrt (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Sqrt (F.T (X)))
         else T'Base (SF.Sqrt (SF.T (X))));

   function Log (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Log (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Log (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Log (F.T (X)))
         else T'Base (SF.Log (SF.T (X))));

   function Acos (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Acos (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Acos (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Acos (F.T (X)))
         else T'Base (SF.Acos (SF.T (X))));

   function Asin (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Asin (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Asin (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Asin (F.T (X)))
         else T'Base (SF.Asin (SF.T (X))));

   function Atan (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Atan (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Atan (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Atan (F.T (X)))
         else T'Base (SF.Atan (SF.T (X))));

   function Sinh (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Sinh (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Sinh (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Sinh (F.T (X)))
         else T'Base (SF.Sinh (SF.T (X))));

   function Cosh (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Cosh (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Cosh (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Cosh (F.T (X)))
         else T'Base (SF.Cosh (SF.T (X))));

   function Tanh (X : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Tanh (LLF.T (X)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Tanh (LF.T (X)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Tanh (F.T (X)))
         else T'Base (SF.Tanh (SF.T (X))));

   function Pow (X, Y : T'Base) return T'Base
   is (if T'Base'Digits > LF.T'Digits
         then T'Base (LLF.Pow (LLF.T (X), LLF.T (Y)))
         elsif T'Base'Digits > F.T'Digits
         then T'Base (LF.Pow (LF.T (X), LF.T (Y)))
         elsif T'Base'Digits > SF.T'Digits
         then T'Base (F.Pow (F.T (X), F.T (Y)))
         else T'Base (SF.Pow (SF.T (X), SF.T (Y))));

end Ada.Numerics.Aux_Generic_Float;
