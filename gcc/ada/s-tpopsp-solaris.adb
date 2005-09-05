------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--    S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S .   --
--                             S P E C I F I C                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--         Copyright (C) 1992-2005, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a version for Solaris native threads

separate (System.Task_Primitives.Operations)
package body Specific is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      pragma Unreferenced (Environment_Task);
      Result : Interfaces.C.int;
   begin
      Result := thr_keycreate (ATCB_Key'Access, System.Null_Address);
      pragma Assert (Result = 0);
   end Initialize;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
      Unknown_Task : aliased System.Address;
      Result       : Interfaces.C.int;
   begin
      Result := thr_getspecific (ATCB_Key, Unknown_Task'Unchecked_Access);
      pragma Assert (Result = 0);
      return Unknown_Task /= System.Null_Address;
   end Is_Valid_Task;

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_Id) is
      Result : Interfaces.C.int;
   begin
      Result := thr_setspecific (ATCB_Key, To_Address (Self_Id));
      pragma Assert (Result = 0);
   end Set;

   ----------
   -- Self --
   ----------

   --  To make Ada tasks and C threads interoperate better, we have
   --  added some functionality to Self. Suppose a C main program
   --  (with threads) calls an Ada procedure and the Ada procedure
   --  calls the tasking run-time system. Eventually, a call will be
   --  made to self. Since the call is not coming from an Ada task,
   --  there will be no corresponding ATCB.

   --  What we do in Self is to catch references that do not come
   --  from recognized Ada tasks, and create an ATCB for the calling
   --  thread.

   --  The new ATCB will be "detached" from the normal Ada task
   --  master hierarchy, much like the existing implicitly created
   --  signal-server tasks.

   function Self return Task_Id is
      Result  : Interfaces.C.int;
      Self_Id : aliased System.Address;
   begin
      Result := thr_getspecific (ATCB_Key, Self_Id'Unchecked_Access);
      pragma Assert (Result = 0);

      if Self_Id = System.Null_Address then
         return Register_Foreign_Thread;
      else
         return To_Task_Id (Self_Id);
      end if;
   end Self;

end Specific;
