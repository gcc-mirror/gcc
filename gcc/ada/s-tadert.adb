------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--          S Y S T E M . T A S K I N G . A S Y N C _ D E L A Y S .         --
--                          E N Q U E U E _ R T                             --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time;
with Ada.Real_Time.Delays;
with System.Task_Primitives.Operations;
with System.Tasking.Initialization;

function System.Tasking.Async_Delays.Enqueue_RT
  (T : in Ada.Real_Time.Time;
   D : Delay_Block_Access) return Boolean
is
   use type Ada.Real_Time.Time;  -- for "=" operator
begin
   if T <= Ada.Real_Time.Clock then
      D.Timed_Out := True;
      System.Task_Primitives.Operations.Yield;
      return False;
   end if;

   System.Tasking.Initialization.Defer_Abort
     (System.Task_Primitives.Operations.Self);
   Time_Enqueue (Ada.Real_Time.Delays.To_Duration (T), D);
   return True;
end System.Tasking.Async_Delays.Enqueue_RT;
