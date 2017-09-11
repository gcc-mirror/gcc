------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1992-2017, Free Software Foundation, Inc.          --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a VxWorks version of this package using Thread_Local_Storage
--  support (VxWorks 6.6 and higher). The implementation is based on __threads
--  support.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB : aliased Task_Id := null;
   --  Ada Task_Id associated with a thread
   pragma Thread_Local_Storage (ATCB);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return ATCB /= Null_Task;
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

   function Self return Task_Id is
   begin
      return ATCB;
   end Self;

end Specific;
