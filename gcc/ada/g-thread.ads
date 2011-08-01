------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . T H R E A D S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2010, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for creation or registration of foreign
--  threads for use as Ada tasks. In order to execute general Ada code, the
--  run-time system must know about all tasks. This package allows foreign
--  code, e.g. a C program, to create a thread that the Ada run-time knows
--  about, or to register the current thread.

--  For some implementations of GNAT Pro, the registration of foreign threads
--  is automatic. However, in such implementations, if the Ada program has no
--  tasks at all and no tasking constructs other than delay, then by default
--  the non-tasking version of the Ada run-time will be loaded. If foreign
--  threads are present, it is important to ensure that the tasking version
--  of the Ada run time is loaded. This may be achieved by adding "with
--  GNAT.Threads" to any unit in the partition.

with System;
with Ada.Task_Identification;

package GNAT.Threads is

   type Void_Ptr is access all Integer;

   function Create_Thread
     (Code : System.Address;  -- pointer
      Parm : Void_Ptr;        -- pointer
      Size : Natural;         -- int
      Prio : Integer)         -- int
      return System.Address;
   pragma Export (C, Create_Thread, "__gnat_create_thread");
   --  Creates a thread with the given (Size) stack size in bytes, and
   --  the given (Prio) priority. The task will execute a call to the
   --  procedure whose address is given by Code. This procedure has
   --  the prototype
   --
   --    void thread_code (void *id, void *parm);
   --
   --  where id is the id of the created task, and parm is the parameter
   --  passed to Create_Thread. The called procedure is the body of the
   --  code for the task, the task will be automatically terminated when
   --  the procedure returns.
   --
   --  This function returns the Ada Id of the created task that can then be
   --  used as a parameter to the procedures below.
   --
   --  C declaration:
   --
   --  extern void *__gnat_create_thread
   --    (void (*code)(void *, void *), void *parm, int size, int prio);

   function Register_Thread return System.Address;
   pragma Export (C, Register_Thread, "__gnat_register_thread");
   --  Create an Ada task Id for the current thread if needed.
   --  If the thread could not be registered, System.Null_Address is returned.
   --
   --  This function returns the Ada Id of the current task that can then be
   --  used as a parameter to the procedures below.
   --
   --  C declaration:
   --
   --  extern void *__gnat_register_thread ();
   --
   --  Here is a typical usage of the Register/Unregister_Thread procedures:
   --
   --  void thread_body ()
   --  {
   --    void *task_id = __gnat_register_thread ();
   --    ... thread body ...
   --    __gnat_unregister_thread ();
   --  }

   procedure Unregister_Thread;
   pragma Export (C, Unregister_Thread, "__gnat_unregister_thread");
   --  Unregister the current task from the GNAT run time and destroy the
   --  memory allocated for its task id.
   --
   --  C declaration:
   --
   --  extern void __gnat_unregister_thread ();

   procedure Unregister_Thread_Id (Thread : System.Address);
   pragma Export (C, Unregister_Thread_Id, "__gnat_unregister_thread_id");
   --  Unregister the task associated with Thread from the GNAT run time and
   --  destroy the memory allocated for its task id.
   --  If no task id is associated with Thread, do nothing.
   --
   --  C declaration:
   --
   --  extern void __gnat_unregister_thread_id (pthread_t *thread);

   procedure Destroy_Thread (Id : System.Address);
   pragma Export (C, Destroy_Thread, "__gnat_destroy_thread");
   --  This procedure may be used to prematurely abort the created thread.
   --  The value Id is the value that was passed to the thread code procedure
   --  at activation time.
   --
   --  C declaration:
   --
   --  extern void __gnat_destroy_thread (void *id);

   procedure Get_Thread (Id : System.Address; Thread : System.Address);
   pragma Export (C, Get_Thread, "__gnat_get_thread");
   --  This procedure is used to retrieve the thread id of a given task.
   --  The value Id is the value that was passed to the thread code procedure
   --  at activation time.
   --  Thread is a pointer to a thread id that will be updated by this
   --  procedure.
   --
   --  C declaration:
   --
   --  extern void __gnat_get_thread (void *id, pthread_t *thread);

   function To_Task_Id
     (Id : System.Address)
      return Ada.Task_Identification.Task_Id;
   --  Ada interface only.
   --  Given a low level Id, as returned by Create_Thread, return a Task_Id,
   --  so that operations in Ada.Task_Identification can be used.

end GNAT.Threads;
