------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               System.Atomic_Operations.Integer_Arithmetic                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2019-2025, Free Software Foundation, Inc.  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

generic
   type Atomic_Type is range <> with Atomic;
package System.Atomic_Operations.Integer_Arithmetic
  with Pure
is
   procedure Atomic_Add
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) with Convention => Intrinsic;

   procedure Atomic_Subtract
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) with Convention => Intrinsic;

   function Atomic_Fetch_And_Add
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type with Convention => Intrinsic;

   function Atomic_Fetch_And_Subtract
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type with Convention => Intrinsic;

   function Is_Lock_Free
     (Item : aliased Atomic_Type) return Boolean with Convention => Intrinsic;

private
   pragma Inline_Always (Atomic_Add);
   pragma Inline_Always (Atomic_Subtract);
   pragma Inline_Always (Atomic_Fetch_And_Add);
   pragma Inline_Always (Atomic_Fetch_And_Subtract);
   pragma Inline_Always (Is_Lock_Free);
end System.Atomic_Operations.Integer_Arithmetic;
