------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                             $Revision: 1.1.16.1 $
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

--  This is the Alpha VxWorks version of this package.

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate (System.VxWorks);

   package IC renames Interfaces.C;

   --  Define enough of a Wind Task Control Block in order to
   --  obtain the inherited priority.  When porting this to
   --  different versions of VxWorks (this is based on 5.3[.1]),
   --  be sure to look at the definition for WIND_TCB located
   --  in $WIND_BASE/target/h/taskLib.h

   type Wind_Fill_1 is array (0 .. 16#77#) of IC.unsigned_char;
   type Wind_Fill_2 is array (16#80# .. 16#1c7#) of IC.unsigned_char;
   type Wind_Fill_3 is array (16#1d8# .. 16#777#) of IC.unsigned_char;

   type Wind_TCB is record
      Fill_1          : Wind_Fill_1;  -- 0x00 - 0x77
      Priority        : IC.int;  -- 0x78 - 0x7b, current (inherited) priority
      Normal_Priority : IC.int;  -- 0x7c - 0x7f, base priority
      Fill_2          : Wind_Fill_2;  -- 0x80 - 0x1c7
      spare1          : Address;  -- 0x1c8 - 0x1cb
      spare2          : Address;  -- 0x1cc - 0x1cf
      spare3          : Address;  -- 0x1d0 - 0x1d3
      spare4          : Address;  -- 0x1d4 - 0x1d7

      --  Fill_3 is much smaller on the board runtime, but the larger size
      --  below keeps this record compatible with vxsim.

      Fill_3          : Wind_Fill_3;     -- 0x1d8 - 0x777
   end record;
   type Wind_TCB_Ptr is access Wind_TCB;


   --  Floating point context record.  Alpha version

   FP_NUM_DREGS : constant := 32;
   type Fpx_Array is array (1 .. FP_NUM_DREGS) of IC.double;

   type FP_CONTEXT is record
      fpx :   Fpx_Array;
      fpcsr : IC.long;
   end record;
   pragma Convention (C, FP_CONTEXT);

   --  Number of entries in hardware interrupt vector table.  Value of
   --  0 disables hardware interrupt handling until it can be tested
   Num_HW_Interrupts : constant := 0;

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
