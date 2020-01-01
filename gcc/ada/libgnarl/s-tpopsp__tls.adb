------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--         Copyright (C) 1992-2020, Free Software Foundation, Inc.          --
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

--  This is a version of this package using TLS and where foreign threads are
--  recognized.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB : aliased Task_Id := null;
   pragma Thread_Local_Storage (ATCB);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
   begin
      ATCB := Environment_Task;
   end Initialize;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return ATCB /= null;
   end Is_Valid_Task;

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_Id) is
   begin
      ATCB := Self_Id;
   end Set;

   ----------
   -- Self --
   ----------

   --  To make Ada tasks and C threads interoperate better, we have added some
   --  functionality to Self. Suppose a C main program (with threads) calls an
   --  Ada procedure and the Ada procedure calls the tasking runtime system.
   --  Eventually, a call will be made to self. Since the call is not coming
   --  from an Ada task, there will be no corresponding ATCB.

   --  What we do in Self is to catch references that do not come from
   --  recognized Ada tasks, and create an ATCB for the calling thread.

   --  The new ATCB will be "detached" from the normal Ada task master
   --  hierarchy, much like the existing implicitly created signal-server
   --  tasks.

   function Self return Task_Id is
      Result : constant Task_Id := ATCB;
   begin
      if Result /= null then
         return Result;
      else
         --  If the value is Null then it is a non-Ada task

         return Register_Foreign_Thread;
      end if;
   end Self;

end Specific;
