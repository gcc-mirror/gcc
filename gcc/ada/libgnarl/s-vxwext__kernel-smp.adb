------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2008-2024, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This package provides vxworks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks 6 SMP kernel version of this package

package body System.VxWorks.Ext is

   IERR : constant := -1;

   --------------
   -- Int_Lock --
   --------------

   function Int_Lock return int is
   begin
      return IERR;
   end Int_Lock;

   ----------------
   -- Int_Unlock --
   ----------------

   procedure Int_Unlock (Old : int) is
      pragma Unreferenced (Old);
   begin
      null;
   end Int_Unlock;

   ---------------
   -- semDelete --
   ---------------

   function semDelete (Sem : SEM_ID) return STATUS is
      function Os_Sem_Delete (Sem : SEM_ID) return STATUS;
      pragma Import (C, Os_Sem_Delete, "semDelete");
   begin
      return Os_Sem_Delete (Sem);
   end semDelete;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int
   is
      function Set_Affinity (tid : t_id; CPU : int) return int;
      pragma Import (C, Set_Affinity, "__gnat_set_affinity");
   begin
      return Set_Affinity (tid, CPU);
   end taskCpuAffinitySet;

   -------------------------
   -- taskMaskAffinitySet --
   -------------------------

   function taskMaskAffinitySet (tid : t_id; CPU_Set : unsigned) return int is
      function Set_Affinity (tid : t_id; CPU_Set : unsigned) return int;
      pragma Import (C, Set_Affinity, "__gnat_set_affinity_mask");
   begin
      return Set_Affinity (tid, CPU_Set);
   end taskMaskAffinitySet;

   ---------------
   -- Task_Cont --
   ---------------

   function Task_Cont (tid : t_id) return STATUS is
      function taskCont (tid : t_id) return STATUS;
      pragma Import (C, taskCont, "taskCont");
   begin
      return taskCont (tid);
   end Task_Cont;

   ---------------
   -- Task_Stop --
   ---------------

   function Task_Stop (tid : t_id) return STATUS is
      function taskStop (tid : t_id) return STATUS;
      pragma Import (C, taskStop, "taskStop");
   begin
      return taskStop (tid);
   end Task_Stop;

end System.VxWorks.Ext;
