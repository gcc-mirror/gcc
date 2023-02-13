------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 1998-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software;  you can redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the AARCH64 VxWorks version of this package

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate (System.VxWorks);

   package IC renames Interfaces.C;

   --  Floating point context record. ARM version

   type ulong128 is array (0 .. 1) of IC.unsigned_long;
   FP_SGPR_NUM_REGS : constant := 32;
   type Fpr_Sgpr_Array is array (1 .. FP_SGPR_NUM_REGS) of ulong128;

   --  The record definition below matches what arch/arm/fppArmLib.h says

   type FP_CONTEXT is record
      fpcr     : IC.unsigned_long;  --  Floating-point Control Register
      fpsr     : IC.unsigned_long;  --  Floating-point Status Register
      vfp_gpr  : Fpr_Sgpr_Array;
   end record;

   for FP_CONTEXT'Alignment use 16;  --  128 bits
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;
   --  Number of entries in hardware interrupt vector table

end System.VxWorks;
