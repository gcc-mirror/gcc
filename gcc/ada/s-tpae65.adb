------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . T A S K _ P R I M I T I V E S . A E _ 6 5 3      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--       Copyright (C) 2002-2003, Free Software Foundation, Inc.            --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  Export certain tasking-related routines for use by Interfaces.Vthreads

with Interfaces.C;
package body System.Task_Primitives.Ae_653 is

   -------------------
   -- ATCB_Key_Addr --
   -------------------

   function ATCB_Key_Addr return Address_Access is
      Key_Addr : Address_Access;
      pragma Import (Ada, Key_Addr, "__gnat_ATCB_key_addr");
      --  Done this way to minimize impact on other targets. This
      --  implementation is temporary, and specific to AE653
   begin
      return Key_Addr;
   end ATCB_Key_Addr;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority
     (T : System.Tasking.Task_ID;
      Prio : System.Priority)
   is
   begin
      T.Common.Current_Priority := Prio;
   end Set_Current_Priority;

   ---------------------
   -- Set_Task_Thread --
   ---------------------

   procedure Set_Task_Thread
     (T : System.Tasking.Task_ID;
      Thread : System.OS_Interface.Thread_Id)
   is
      use System.OS_Interface;
      use System.Tasking;
      use type Interfaces.C.int;
      Result : STATUS;
   begin
      T.Common.LL.Thread := Thread;
      if taskVarGet (Thread, ATCB_Key_Addr) = ERROR then
         Result := taskVarAdd (Thread, ATCB_Key_Addr);
         pragma Assert (Result = OK);
      end if;

      Result := taskVarSet (Thread, ATCB_Key_Addr, To_Address (T));
      pragma Assert (Result = OK);
   end Set_Task_Thread;

end System.Task_Primitives.Ae_653;
