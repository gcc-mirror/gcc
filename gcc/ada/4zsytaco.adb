------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--            Copyright (C) 1992-2001 Free Software Foundation, Inc.        --
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

with Interfaces.C;
package body Ada.Synchronous_Task_Control is
   use System.OS_Interface;
   use type Interfaces.C.int;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
      St     : STATUS;
      Result : Boolean := False;

   begin
      --  Determine state by attempting to take the semaphore with
      --  a 0 timeout value.  Status = OK indicates the semaphore was
      --  full, so reset it to the full state.

      St := semTake (S.Sema, NO_WAIT);

      if St = OK then
         --  Took the semaphore. Reset semaphore state to FULL
         Result := True;
         St := semGive (S.Sema);
      end if;

      return Result;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
      St : STATUS;
   begin
      --  Need to get the semaphore into the "empty" state.
      --  On return, this task will have made the semaphore
      --  empty (St = OK) or have left it empty.
      St := semTake (S.Sema, NO_WAIT);
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      St : STATUS;
   begin
      St := semGive (S.Sema);
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      St : STATUS;

      --  Declare local exception so the mutex can still be reset
      --  to full if Program_Error is raised

      Task_Already_Pending : exception;
   begin
      --  Determine whether another task is pending on the suspension
      --  object. Should never be called from an ISR. Therefore semTake can
      --  be called on the mutex
      St := semTake (S.Mutex, NO_WAIT);

      if St = OK then
         --  Wait for suspension object

         St := semTake (S.Sema, WAIT_FOREVER);
         St := semGive (S.Mutex);

      else
         --  Another task is pending on the suspension object

         raise Task_Already_Pending;
      end if;
   exception
      when Task_Already_Pending =>
         raise Program_Error;
      when others =>
         St := semGive (S.Mutex);
         raise;
   end Suspend_Until_True;

   procedure Initialize (S : in out Suspension_Object) is
   begin
      S.Sema := semBCreate (SEM_Q_FIFO, SEM_EMPTY);

      --  Use simpler binary semaphore instead of VxWorks
      --  mutual exclusion semaphore, because we don't need
      --  the fancier semantics and their overhead.

      S.Mutex := semBCreate (SEM_Q_FIFO, SEM_FULL);
   end Initialize;

   procedure Finalize (S : in out Suspension_Object) is
      St : STATUS;
   begin
      St := semDelete (S.Sema);
      St := semDelete (S.Mutex);
   end Finalize;

end Ada.Synchronous_Task_Control;
