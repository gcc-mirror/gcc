------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 1998-2017, Free Software Foundation, Inc.         --
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

--  This is the ARM VxWorks version of this package

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate (System.VxWorks);

   package IC renames Interfaces.C;

   --  Floating point context record. ARM version

   FP_SGPR_NUM_REGS : constant := 32;
   type Fpr_Sgpr_Array is array (1 .. FP_SGPR_NUM_REGS) of IC.unsigned;

   --  The record definition below matches what arch/arm/fppArmLib.h says

   type FP_CONTEXT is record
      fpsid    : IC.unsigned;  --  system ID register
      fpscr    : IC.unsigned;  --  status and control register
      fpexc    : IC.unsigned;  --  exception register
      fpinst   : IC.unsigned;  --  instruction register
      fpinst2  : IC.unsigned;  --  instruction register 2
      mfvfr0   : IC.unsigned;  --  media and VFP feature Register 0
      mfvfr1   : IC.unsigned;  --  media and VFP feature Register 1
      pad      : IC.unsigned;
      vfp_gpr  : Fpr_Sgpr_Array;
   end record;

   for FP_CONTEXT'Alignment use 4;
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;
   --  Number of entries in hardware interrupt vector table

end System.VxWorks;
