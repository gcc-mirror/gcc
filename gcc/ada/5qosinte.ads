------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision$
--                                                                          --
--             Copyright (C) 1991-2001 Florida State University             --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  RT GNU/Linux version.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;

package System.OS_Interface is

   pragma Preelaborate;

   subtype int            is Interfaces.C.int;
   subtype unsigned_long  is Interfaces.C.unsigned_long;

   --  RT GNU/Linux kernel threads should not use the
   --  OS signal interfaces.

   Max_Interrupt : constant := 2;
   type Signal is new int range 0 .. Max_Interrupt;
   type sigset_t is new Integer;

   ----------
   -- Time --
   ----------

   RT_TICKS_PER_SEC : constant := 1193180;
   --  the amount of time units in one second.

   RT_TIME_END : constant := 16#7fffFfffFfffFfff#;

   type RTIME is range -2 ** 63 .. 2 ** 63 - 1;
   --  the introduction of type RTIME is due to the fact that RT-GNU/Linux
   --  uses this type to represent time. In RT-GNU/Linux, it's a long long
   --  integer that takes 64 bits for storage

   -------------------------
   -- Priority Scheduling --
   -------------------------

   RT_LOWEST_PRIORITY : constant System.Any_Priority :=
     System.Any_Priority'First;
   --  for the lowest priority task in RT-GNU/Linux. By the design, this
   --  task is the regular GNU/Linux kernel.

   RT_TASK_MAGIC : constant := 16#754d2774#;
   --  a special constant used as a label for a task that has been created

   ----------------------------
   -- RT constants and types --
   ----------------------------

   SFIF : Integer;
   pragma Import (C, SFIF, "SFIF");
   --  Interrupt emulation flag used by RT-GNU/Linux. If it's 0, the regular
   --  GNU/Linux kernel is preempted. Otherwise, the regular Linux kernel is
   --  running

   GFP_ATOMIC : constant := 16#1#;
   GFP_KERNEL : constant := 16#3#;
   --  constants to indicate the priority of a call to kmalloc.
   --  GFP_KERNEL is used in the current implementation to allocate
   --  stack space for a task. Since GFP_ATOMIC has higher priority,
   --  if necessary, replace GFP_KERNEL with GFP_ATOMIC

   type Rt_Task_States is (RT_TASK_READY, RT_TASK_DELAYED, RT_TASK_DORMANT);

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;

   --  ??? need to define a type for references to (IDs of)
   --  RT GNU/Linux lock objects, and implement the lock objects.

   subtype Thread_Id is System.Address;

   -------------------------------
   -- Useful imported functions --
   -------------------------------

   -------------------------------------
   -- Functions from GNU/Linux kernel --
   -------------------------------------

   function Kmalloc (size : Integer; Priority : Integer) return System.Address;
   pragma Import (C, Kmalloc, "kmalloc");

   procedure Kfree (Ptr : System.Address);
   pragma Import (C, Kfree, "kfree");

   procedure Printk (Msg : String);
   pragma Import (C, Printk, "printk");

   ---------------------
   -- RT time related --
   ---------------------

   function Rt_Get_Time return RTIME;
   pragma Import (C, Rt_Get_Time, "rt_get_time");

   function Rt_Request_Timer (Fn : System.Address) return Integer;
   procedure Rt_Request_Timer (Fn : System.Address);
   pragma Import (C, Rt_Request_Timer, "rt_request_timer");

   procedure Rt_Free_Timer;
   pragma Import (C, Rt_Free_Timer, "rt_free_timer");

   procedure Rt_Set_Timer (T : RTIME);
   pragma Import (C, Rt_Set_Timer, "rt_set_timer");

   procedure Rt_No_Timer;
   pragma Import (C, Rt_No_Timer, "rt_no_timer");

   ---------------------
   -- RT FIFO related --
   ---------------------

   function Rtf_Create (Fifo : Integer; Size : Integer) return Integer;
   pragma Import (C, Rtf_Create, "rtf_create");

   function Rtf_Destroy (Fifo : Integer) return Integer;
   pragma Import (C, Rtf_Destroy, "rtf_destroy");

   function Rtf_Resize (Minor : Integer; Size : Integer) return Integer;
   pragma Import (C, Rtf_Resize, "rtf_resize");

   function Rtf_Put
     (Fifo  : Integer;
      Buf   : System.Address;
      Count : Integer) return Integer;
   pragma Import (C, Rtf_Put, "rtf_put");

   function Rtf_Get
     (Fifo  : Integer;
      Buf   : System.Address;
      Count : Integer) return Integer;
   pragma Import (C, Rtf_Get, "rtf_get");

   function Rtf_Create_Handler
     (Fifo    : Integer;
      Handler : System.Address) return Integer;
   pragma Import (C, Rtf_Create_Handler, "rtf_create_handler");

private
   type Require_Body;
end System.OS_Interface;
