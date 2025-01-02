------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  This is a Solaris version of this package

--  This package provides low-level support for most tasking features

with Ada.Unchecked_Conversion;

with System.OS_Interface;
with System.OS_Locks;

package System.Task_Primitives is
   pragma Preelaborate;

   type Lock is limited private;
   type Lock_Ptr is access all Lock;
   --  Should be used for implementation of protected objects

   function To_Lock_Ptr is
     new Ada.Unchecked_Conversion (OS_Locks.RTS_Lock_Ptr, Lock_Ptr);
   function To_RTS_Lock_Ptr is
     new Ada.Unchecked_Conversion (Lock_Ptr, OS_Locks.RTS_Lock_Ptr);

   type Suspension_Object is limited private;
   --  Should be used for the implementation of Ada.Synchronous_Task_Control

   type Task_Body_Access is access procedure;
   --  Pointer to the task body's entry point (or possibly a wrapper
   --  declared local to the GNARL).

   type Private_Data is limited private;
   --  Any information that the GNULLI needs maintained on a per-task basis.
   --  A component of this type is guaranteed to be included in the
   --  Ada_Task_Control_Block.

   subtype Task_Address is System.Address;
   Task_Address_Size : constant := Standard'Address_Size;
   --  Type used for task addresses and its size

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

private

   type Lock is new OS_Locks.RTS_Lock;

   type Suspension_Object is record
      State : Boolean;
      pragma Atomic (State);
      --  Boolean that indicates whether the object is open. This field is
      --  marked Atomic to ensure that we can read its value without locking
      --  the access to the Suspension_Object.

      Waiting : Boolean;
      --  Flag showing if there is a task already suspended on this object

      L : aliased System.OS_Interface.mutex_t;
      --  Protection for ensuring mutual exclusion on the Suspension_Object

      CV : aliased System.OS_Interface.cond_t;
      --  Condition variable used to queue threads until condition is signaled
   end record;

   --  Note that task support on gdb relies on the fact that the first two
   --  fields of Private_Data are Thread and LWP.

   type Private_Data is limited record
      Thread : aliased System.OS_Interface.thread_t;
      --  This component is written to once before concurrent access to it is
      --  possible, and then remains constant. The place where it is written to
      --  depends on how the enclosing ATCB comes into existence:
      --
      --  1. For the environment task, the component is set in
      --     System.Task_Primitive.Operations.Initialize.
      --  2. For foreign threads, it happens in
      --     System.Task_Primitives.Operations.Register_Foreign_Thread.
      --  3. For others tasks, it's in
      --     System.Task_Primitives.Operations.Create_Task.

      LWP : System.OS_Interface.lwpid_t;
      --  The LWP id of the thread. Set by self in Enter_Task

      CV : aliased System.OS_Interface.cond_t;
      --  Condition variable used to queue threads until condition is signaled

      L  : aliased System.OS_Locks.RTS_Lock;
      --  Protection for all components is lock L

      Active_Priority : System.Any_Priority := System.Any_Priority'First;
      --  Simulated active priority, used iff Priority_Ceiling_Support is True

      Locking : Lock_Ptr;
      Locks   : Lock_Ptr;
      Wakeups : Natural := 0;
   end record;

end System.Task_Primitives;
