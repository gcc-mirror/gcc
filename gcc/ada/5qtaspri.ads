------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--            Copyright (C) 1991-2001, Florida State University             --
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

--  RT_GNU/Linux version

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.OS_Interface;

package System.Task_Primitives is

   type Lock is limited private;
   --  Used for implementation of protected objects.

   type Lock_Ptr is limited private;

   type RTS_Lock is limited private;
   --  Used inside the runtime system. The difference between Lock and the
   --  RTS_Lock is that the later one serves only as a semaphore so that do
   --  not check for ceiling violations.
   type RTS_Lock_Ptr is limited private;

   type Task_Body_Access is access procedure;
   --  Pointer to the task body's entry point (or possibly a wrapper
   --  declared local to the GNARL).

   type Private_Data is limited private;
   --  Any information that the GNULLI needs maintained on a per-task
   --  basis.  A component of this type is guaranteed to be included
   --  in the Ada_Task_Control_Block.

private

   type RT_GNU_Linux_Lock is record
      Ceiling_Priority     : System.Any_Priority;
      Pre_Locking_Priority : System.Any_Priority;
      --  Used to store the task's active priority before it
      --  acquires the lock

      Owner : System.Address;
      --  This is really a Task_ID, but we can't use that type
      --  here because this System.Tasking is "with"
      --  the current package -- a circularity.
   end record;

   type Lock is new RT_GNU_Linux_Lock;
   type RTS_Lock is new RT_GNU_Linux_Lock;

   type RTS_Lock_Ptr is access all RTS_Lock;
   type Lock_Ptr is access all Lock;

   type Private_Data is record
      Stack : System.Address;
      --  A stack space needed for the task. the space is allocated
      --  when the task is being created and is deallocated when
      --  the TCB for the task is finalized

      Uses_Fp : Integer;
      --  A flag to indicate whether the task is going to use floating-
      --  point unit. It's set to 1, indicating FP unit is always going
      --  to be used. The reason is that it is in this private record and
      --  necessary operation has to be provided for a user to call so as
      --  to change its value

      Magic : Integer;
      --  A special value is going to be stored in it when a task is
      --  created. The value is RT_TASK_MAGIC (16#754d2774#) as declared
      --  in System.OS_Interface

      State : System.OS_Interface.Rt_Task_States;
      --  Shows whether the task is RT_TASK_READY, RT_TASK_DELAYED or
      --  RT_TASK_DORMANT to support suspend, wait, wakeup.

      Stack_Bottom : System.Address;

      Active_Priority  : System.Any_Priority;
      --  Active priority of the task

      Period : System.OS_Interface.RTIME;
      --  Intended originally to store the period of the task, but not used
      --  in the current implementation

      Resume_Time : System.OS_Interface.RTIME;
      --  Store the time the task has to be awakened

      Next : System.Address;
      --  This really is a Task_ID, used to link the Available_TCBs.

      Succ : System.Address;
      pragma Volatile (Succ);
      Pred : System.Address;
      pragma Volatile (Pred);
      --  These really are Task_ID, used to implement a circular doubly
      --  linked list for task queue

      L : aliased RTS_Lock;

      Outer_Lock : RTS_Lock_Ptr := null;
      --  Used to track which Lock the task is holding is the outermost
      --  one in order to implement priority setting and inheritance
   end record;

   --  ????  May need to use pragma Atomic or Volatile on some
   --  components; may also need to specify aliased for some.
end System.Task_Primitives;
