------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         G N A T . T H R E A D S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for creation of foreign threads for
--  use as Ada tasks. In order to execute general Ada code, the run-time
--  system must know about all tasks. This package allows foreign code,
--  e.g. a C program, to create a thread that the Ada run-time knows about.

with System;

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

end GNAT.Threads;
