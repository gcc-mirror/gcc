------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--             SYSTEM.TASK_PRIMITIVES.OPERATIONS.ATCB_ALLOCATION            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 2011-2021, Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;

separate (System.Task_Primitives.Operations)
package body ATCB_Allocation is

   ---------------
   -- Free_ATCB --
   ---------------

   procedure Free_ATCB (T : Task_Id) is
      Tmp     : Task_Id := T;
      Is_Self : constant Boolean := T = Self;

      procedure Free is new
        Ada.Unchecked_Deallocation (Ada_Task_Control_Block, Task_Id);

   begin
      if Is_Self then
         declare
            Local_ATCB : aliased Ada_Task_Control_Block (0);
            --  Create a dummy ATCB and initialize it minimally so that "Free"
            --  can still call Self and Defer/Undefer_Abort after Tmp is freed
            --  by the underlying memory management library.

         begin
            Local_ATCB.Common.LL.Thread        := T.Common.LL.Thread;
            Local_ATCB.Common.Current_Priority := T.Common.Current_Priority;

            Specific.Set (Local_ATCB'Unchecked_Access);
            Free (Tmp);

            --  Note: it is assumed here that for all platforms, Specific.Set
            --  deletes the task specific information if passed a null value.

            Specific.Set (null);
         end;

      else
         Free (Tmp);
      end if;
   end Free_ATCB;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_Id is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

end ATCB_Allocation;
