------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1998-2001 Free Software Foundation              --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the M68K VxWorks version of this package.

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate (System.VxWorks);

   package IC renames Interfaces.C;

   --  Define enough of a Wind Task Control Block in order to
   --  obtain the inherited priority.  When porting this to
   --  different versions of VxWorks (this is based on 5.3[.1]),
   --  be sure to look at the definition for WIND_TCB located
   --  in $WIND_BASE/target/h/taskLib.h

   type Wind_Fill_1 is array (0 .. 16#3F#) of IC.unsigned_char;
   type Wind_Fill_2 is array (16#48# .. 16#107#) of IC.unsigned_char;

   type Wind_TCB is record
      Fill_1          : Wind_Fill_1; -- 0x00 - 0x3f
      Priority        : IC.int;  -- 0x40 - 0x43, current (inherited) priority
      Normal_Priority : IC.int;  -- 0x44 - 0x47, base priority
      Fill_2          : Wind_Fill_2; -- 0x48 - 0x107
      spare1          : Address;  -- 0x108 - 0x10b
      spare2          : Address;  -- 0x10c - 0x10f
      spare3          : Address;  -- 0x110 - 0x113
      spare4          : Address;  -- 0x114 - 0x117
   end record;
   type Wind_TCB_Ptr is access Wind_TCB;

   --  Floating point context record.  68K version

   FP_NUM_DREGS : constant := 8;
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

   --  VxWorks 5.3 and 5.4 version
   type TASK_DESC is record
      td_id           : IC.int;   --  task id
      td_name         : Address;  --  name of task
      td_priority     : IC.int;   --  task priority
      td_status       : IC.int;   --  task status
      td_options      : IC.int;   --  task option bits (see below)
      td_entry        : Address;  --  original entry point of task
      td_sp           : Address;  --  saved stack pointer
      td_pStackBase   : Address;  --  the bottom of the stack
      td_pStackLimit  : Address;  --  the effective end of the stack
      td_pStackEnd    : Address;  --  the actual end of the stack
      td_stackSize    : IC.int;   --  size of stack in bytes
      td_stackCurrent : IC.int;   --  current stack usage in bytes
      td_stackHigh    : IC.int;   --  maximum stack usage in bytes
      td_stackMargin  : IC.int;   --  current stack margin in bytes
      td_errorStatus  : IC.int;   --  most recent task error status
      td_delay        : IC.int;   --  delay/timeout ticks
   end record;
   pragma Convention (C, TASK_DESC);

end System.VxWorks;
