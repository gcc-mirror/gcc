------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

with Ada.Unchecked_Conversion;

with System.OS_Interface;

package System.Task_Primitives is
   pragma Preelaborate;

   type Lock is limited private;
   type Lock_Ptr is access all Lock;
   --  Should be used for implementation of protected objects

   type RTS_Lock is limited private;
   type RTS_Lock_Ptr is access all RTS_Lock;
   --  Should be used inside the runtime system. The difference between Lock
   --  and the RTS_Lock is that the later one serves only as a semaphore so
   --  that do not check for ceiling violations.

   function To_Lock_Ptr is
     new Ada.Unchecked_Conversion (RTS_Lock_Ptr, Lock_Ptr);

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

   type Private_Task_Serial_Number is mod 2 ** Long_Long_Integer'Size;
   --  Used to give each task a unique serial number

   type Base_Lock is new System.OS_Interface.mutex_t;

   type Owner_Int is new Integer;
   for Owner_Int'Alignment use Standard'Maximum_Alignment;

   type Owner_ID is access all Owner_Int;

   function To_Owner_ID is
     new Ada.Unchecked_Conversion (System.Address, Owner_ID);

   type Lock is record
      L              : aliased Base_Lock;
      Ceiling        : System.Any_Priority := System.Any_Priority'First;
      Saved_Priority : System.Any_Priority := System.Any_Priority'First;
      Owner          : Owner_ID;
      Next           : Lock_Ptr;
      Level          : Private_Task_Serial_Number := 0;
      Buddy          : Owner_ID;
      Frozen         : Boolean := False;
   end record;

   type RTS_Lock is new Lock;

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
      pragma Atomic (Thread);
      --  Thread field may be updated by two different threads of control.
      --  (See, Enter_Task and Create_Task in s-taprop.adb). They put the same
      --  value (thr_self value). We do not want to use lock on those
      --  operations and the only thing we have to make sure is that they are
      --  updated in atomic fashion.

      LWP : System.OS_Interface.lwpid_t;
      --  The LWP id of the thread. Set by self in Enter_Task

      CV : aliased System.OS_Interface.cond_t;
      L  : aliased RTS_Lock;
      --  Protection for all components is lock L

      Active_Priority : System.Any_Priority := System.Any_Priority'First;
      --  Simulated active priority, used iff Priority_Ceiling_Support is True

      Locking : Lock_Ptr;
      Locks   : Lock_Ptr;
      Wakeups : Natural := 0;
   end record;

end System.Task_Primitives;
