------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  System.Atomic_Operations.Test_And_Set                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2019-2020, Free Software Foundation, Inc.  --
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

package System.Atomic_Operations.Test_And_Set
  with Pure
is
   type Test_And_Set_Flag is mod 2 ** 8
     with Atomic, Default_Value => 0, Size => 8;

   function Atomic_Test_And_Set
     (Item : aliased in out Test_And_Set_Flag) return Boolean
     with Convention => Intrinsic;

   procedure Atomic_Clear
     (Item : aliased in out Test_And_Set_Flag)
     with Convention => Intrinsic;

   function Is_Lock_Free
     (Item : aliased Test_And_Set_Flag) return Boolean
     with Convention => Intrinsic;

private
   pragma Inline_Always (Atomic_Test_And_Set);
   pragma Inline_Always (Atomic_Clear);
   pragma Inline_Always (Is_Lock_Free);
end System.Atomic_Operations.Test_And_Set;
