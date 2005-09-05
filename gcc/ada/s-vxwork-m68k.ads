------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--            Copyright (C) 1998-2005 Free Software Foundation, Inc.        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the M68K VxWorks version of this package.

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
