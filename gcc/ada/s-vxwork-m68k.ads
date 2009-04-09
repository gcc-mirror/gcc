------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 1998-2009, Free Software Foundation, Inc.         --
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

--  This is the M68K VxWorks version of this package

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate;

   package IC renames Interfaces.C;

   --  Floating point context record. 68K version

   FP_NUM_DREGS        : constant := 8;
   FP_STATE_FRAME_SIZE : constant := 216;

   type DOUBLEX is array (1 .. 12) of Interfaces.Unsigned_8;
   pragma Pack (DOUBLEX);
   for DOUBLEX'Size use 12 * 8;

   type DOUBLEX_Array is array (1 .. FP_NUM_DREGS) of DOUBLEX;
   pragma Pack (DOUBLEX_Array);
   for DOUBLEX_Array'Size use FP_NUM_DREGS * 12 * 8;

   type FPREG_SET is record
      fpcr  : IC.int;
      fpsr  : IC.int;
      fpiar : IC.int;
      fpx   : DOUBLEX_Array;
   end record;

   type Fp_State_Frame_Array is array (1 .. FP_STATE_FRAME_SIZE) of IC.char;
   pragma Pack (Fp_State_Frame_Array);
   for Fp_State_Frame_Array'Size use 8 * FP_STATE_FRAME_SIZE;

   type FP_CONTEXT is record
      fpRegSet   : FPREG_SET;
      stateFrame : Fp_State_Frame_Array;
   end record;
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;
   --  Number of entries in the hardware interrupt vector table

end System.VxWorks;
