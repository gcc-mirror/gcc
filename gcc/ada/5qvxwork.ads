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
--             Copyright (C) 1998 - 2001 Free Software Foundation           --
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

--  This is the PPC VxWorks 6.0 version of this package. A different version
--  is used for VxWorks 5.x

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate (System.VxWorks);

   package IC renames Interfaces.C;

   --  Define enough of a Wind Task Control Block in order to
   --  obtain the inherited priority.  When porting this to
   --  different versions of VxWorks (this is based on 6.0),
   --  be sure to look at the definition for WIND_TCB located
   --  in $WIND_BASE/target/h/taskLib.h

   type Wind_Fill_1 is array (0 .. 16#6B#) of IC.unsigned_char;
   type Wind_Fill_2 is array (16#74# .. 16#10F#) of IC.unsigned_char;

   type Wind_TCB is record
      Fill_1          : Wind_Fill_1; -- 0x00 - 0x6b
      Priority        : IC.int;  -- 0x6c - 0x6f, current (inherited) priority
      Normal_Priority : IC.int;  -- 0x70 - 0x73, base priority
      Fill_2          : Wind_Fill_2; -- 0x74 - 0x10f
      spare1          : Address;  -- 0x110 - 0x113
      spare2          : Address;  -- 0x114 - 0x117
      spare3          : Address;  -- 0x118 - 0x11b
      spare4          : Address;  -- 0x11c - 0x11f
   end record;
   type Wind_TCB_Ptr is access Wind_TCB;

   --  Floating point context record.  PPC version

   FP_NUM_DREGS : constant := 32;
   type Fpr_Array is array (1 .. FP_NUM_DREGS) of IC.double;

   type FP_CONTEXT is record
      fpr :   Fpr_Array;
      fpcsr : IC.int;
      pad :   IC.int;
   end record;
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;

   --  For VxWorks 6.0
   type TASK_DESC is record
      td_id           : IC.int;   --  task id
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

      td_PExcStkBase  : Address;  --  exception stack base
      td_PExcStkPtr   : Address;  --  exception stack pointer
      td_ExcStkHigh   : IC.int;   --  exception stack max usage
      td_ExcStkMgn    : IC.int;   --  exception stack margin

      td_errorStatus  : IC.int;   --  most recent task error status
      td_delay        : IC.int;   --  delay/timeout ticks

      td_PdId         : Address;  --  task's home protection domain
      td_name         : Address;  --  name of task
   end record;

   pragma Convention (C, TASK_DESC);

end System.VxWorks;
