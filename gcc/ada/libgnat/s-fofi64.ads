------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . F O R E _ F I X E D _ 6 4                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains the routine used for the 'Fore attribute for ordinary
--  fixed point types up to 64-bit small and mantissa.

with Interfaces;
with System.Arith_64;
with System.Fore_F;

package System.Fore_Fixed_64 is
   pragma Pure;

   subtype Int64 is Interfaces.Integer_64;

   package Impl is new Fore_F (Int64, Arith_64.Scaled_Divide64);

   function Fore_Fixed64
     (Lo, Hi, Num, Den : Int64; Scale : Integer) return Natural
     renames Impl.Fore_Fixed;

end System.Fore_Fixed_64;
