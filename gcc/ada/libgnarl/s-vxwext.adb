------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--           Copyright (C) 2009-2021, Free Software Foundation, Inc.        --
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

--  This is the VxWorks 5 and VxWorks MILS version of this package

package body System.VxWorks.Ext is

   IERR : constant := -1;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int is
      pragma Unreferenced (tid, CPU);
   begin
      return IERR;
   end taskCpuAffinitySet;

   -------------------------
   -- taskMaskAffinitySet --
   -------------------------

   function taskMaskAffinitySet (tid : t_id; CPU_Set : unsigned) return int is
      pragma Unreferenced (tid, CPU_Set);
   begin
      return IERR;
   end taskMaskAffinitySet;

end System.VxWorks.Ext;
