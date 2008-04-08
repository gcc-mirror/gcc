------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
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
------------------------------------------------------------------------------

--  This package provides vxworks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks 5 version of this package

with Interfaces.C;

package System.VxWorks.Ext is
   pragma Preelaborate;

   type t_id is new Long_Integer;
   subtype int is Interfaces.C.int;

   function Task_Cont (tid : t_id) return int;
   pragma Import (C, Task_Cont, "taskResume");

   function Task_Stop (tid : t_id) return int;
   pragma Import (C, Task_Stop, "taskSuspend");

   function Int_Lock return int;
   pragma Import (C, Int_Lock, "intLock");

   function Int_Unlock return int;
   pragma Import (C, Int_Unlock, "intUnlock");

   function kill (pid : t_id; sig : int) return int;
   pragma Import (C, kill, "kill");

   function Set_Time_Slice (ticks : int) return int;
   pragma Import (C, Set_Time_Slice, "kernelTimeSlice");

   function getpid return t_id;
   pragma Import (C, getpid, "taskIdSelf");

end System.VxWorks.Ext;
