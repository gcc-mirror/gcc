------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . T H R E A D S . I N I T I A L I Z A T I O N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks version of this package; to use this implementation,
--  the task hook libraries should be included in the VxWorks kernel.

with System.Secondary_Stack;
with Interfaces.C;
with Unchecked_Conversion;

package body System.Threads.Initialization is

   use Interfaces.C;

   package SSS renames System.Secondary_Stack;

   procedure Initialize_Task_Hooks;
   --  Register the appropriate hooks (Register and Reset_TSD) to the
   --  underlying OS, so that they will be called when a task is created
   --  or reset.

   Current_ATSD : aliased System.Address;
   pragma Import (C, Current_ATSD, "__gnat_current_atsd");

   ---------------------------
   -- Initialize_Task_Hooks --
   ---------------------------

   procedure Initialize_Task_Hooks is separate;
   --  Separate, as these hooks are different for AE653 and VxWorks 5.5.

   --------------
   -- Register --
   --------------

   function Register (T : OSI.Thread_Id) return OSI.STATUS is
      TSD : ATSD_Access := new ATSD;
      Result : OSI.STATUS;
   begin
      --  It cannot be assumed that the caller of this routine has a ATSD;
      --  so neither this procedure nor the procedures that it calls should
      --  raise or handle exceptions,  or make use of a secondary stack.

      if OSI.taskIdVerify (T) = OSI.ERROR
        or else OSI.taskVarGet (T, Current_ATSD'Access) /= OSI.ERROR
      then
         return OSI.ERROR;
      end if;

      Result := OSI.taskVarAdd (T, Current_ATSD'Access);
      pragma Assert (Result /= -1);
      Result := OSI.taskVarSet (T, Current_ATSD'Access, TSD.all'Address);
      pragma Assert (Result /= -1);
      TSD.Sec_Stack_Addr := SSS.SS_Create;
      SSS.SS_Init (TSD.Sec_Stack_Addr);
      return Result;
   end Register;

   ---------------
   -- Reset_TSD --
   ---------------

   function Reset_TSD (T : OSI.Thread_Id) return OSI.STATUS is
      TSD_Ptr : int;
      function To_Address is new Unchecked_Conversion
        (Interfaces.C.int, ATSD_Access);
   begin
      TSD_Ptr := OSI.taskVarGet (T, Current_ATSD'Access);
      pragma Assert (TSD_Ptr /= OSI.ERROR);

      --  Just reset the secondary stack pointer.  The implementation here
      --  assumes that the fixed secondary stack implementation is used.
      --  If not, there will be a memory leak (along with allocation, which
      --  is prohibited for ARINC processes once the system enters "normal"
      --  mode).

      SSS.SS_Init (To_Address (TSD_Ptr).Sec_Stack_Addr);
      return OSI.OK;
   end Reset_TSD;

begin
   Initialize_Task_Hooks;
end System.Threads.Initialization;
