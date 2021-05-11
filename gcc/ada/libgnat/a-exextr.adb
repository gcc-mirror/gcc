------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      ADA.EXCEPTIONS.EXCEPTION_TRACES                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

pragma Warnings (Off);
with Ada.Exceptions.Last_Chance_Handler;
pragma Warnings (On);
--  Bring last chance handler into closure

separate (Ada.Exceptions)
package body Exception_Traces is

   Nline : constant String := String'(1 => ASCII.LF);
   --  Convenient shortcut

   type Exception_Action is access procedure (E : Exception_Occurrence);
   pragma Favor_Top_Level (Exception_Action);

   Global_Action : Exception_Action := null;
   pragma Atomic (Global_Action);
   pragma Export
     (Ada, Global_Action, "__gnat_exception_actions_global_action");
   --  Global action, executed whenever an exception is raised.  Changing the
   --  export name must be coordinated with code in g-excact.adb.

   Global_Unhandled_Action : Exception_Action := null;
   pragma Atomic (Global_Unhandled_Action);
   pragma Export
     (Ada, Global_Unhandled_Action,
      "__gnat_exception_actions_global_unhandled_action");
   --  Global action, executed whenever an unhandled exception is raised.
   --  Changing the export name must be coordinated with code in g-excact.adb.

   Raise_Hook_Initialized : Boolean := False;
   pragma Export
     (Ada, Raise_Hook_Initialized, "__gnat_exception_actions_initialized");

   procedure Last_Chance_Handler (Except : Exception_Occurrence);
   pragma Import (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);
   --  Users can replace the default version of this routine,
   --  Ada.Exceptions.Last_Chance_Handler.

   function To_Action is new Ada.Unchecked_Conversion
     (Raise_Action, Exception_Action);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean);
   --  Factorizes the common processing for Notify_Handled_Exception and
   --  Notify_Unhandled_Exception. Is_Unhandled is set to True only in the
   --  latter case because Notify_Handled_Exception may be called for an
   --  actually unhandled occurrence in the Front-End-SJLJ case.

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean) is
      --  Save actions locally to avoid any race condition that would
      --  reset them to null.
      Action           : constant Exception_Action := Global_Action;
      Unhandled_Action : constant Exception_Action := Global_Unhandled_Action;

   begin
      --  Output the exception information required by the Exception_Trace
      --  configuration. Take care not to output information about internal
      --  exceptions.

      if not Excep.Id.Not_Handled_By_Others
        and then
          (Exception_Trace = Every_Raise
            or else
              (Is_Unhandled
                and then
                  (Exception_Trace = Unhandled_Raise
                    or else Exception_Trace = Unhandled_Raise_In_Main)))
      then
         --  Exception trace messages need to be protected when several tasks
         --  can issue them at the same time.

         Lock_Task.all;
         To_Stderr (Nline);

         if Exception_Trace /= Unhandled_Raise_In_Main then
            if Is_Unhandled then
               To_Stderr ("Unhandled ");
            end if;

            To_Stderr ("Exception raised");
            To_Stderr (Nline);
         end if;

         To_Stderr (Exception_Information (Excep.all));
         Unlock_Task.all;
      end if;

      --  Call the user-specific actions
      --  ??? We should presumably look at the reraise status here.

      if Raise_Hook_Initialized
        and then Exception_Data_Ptr (Excep.Id).Raise_Hook /= null
      then
         To_Action (Exception_Data_Ptr (Excep.Id).Raise_Hook) (Excep.all);
      end if;

      if Is_Unhandled and Unhandled_Action /= null then
         Unhandled_Action (Excep.all);
      end if;

      if Action /= null then
         Action (Excep.all);
      end if;
   end Notify_Exception;

   ------------------------------
   -- Notify_Handled_Exception --
   ------------------------------

   procedure Notify_Handled_Exception (Excep : EOA) is
   begin
      Notify_Exception (Excep, Is_Unhandled => False);
   end Notify_Handled_Exception;

   --------------------------------
   -- Notify_Unhandled_Exception --
   --------------------------------

   procedure Notify_Unhandled_Exception (Excep : EOA) is
   begin
      --  Check whether there is any termination handler to be executed for
      --  the environment task, and execute it if needed. Here we handle both
      --  the Abnormal and Unhandled_Exception task termination. Normal
      --  task termination routine is executed elsewhere (either in the
      --  Task_Wrapper or in the Adafinal routine for the environment task).

      Task_Termination_Handler.all (Excep.all);

      Notify_Exception (Excep, Is_Unhandled => True);
      Debug_Unhandled_Exception (SSL.Exception_Data_Ptr (Excep.Id));
   end Notify_Unhandled_Exception;

   -----------------------------------
   -- Unhandled_Exception_Terminate --
   -----------------------------------

   procedure Unhandled_Exception_Terminate (Excep : EOA) is
      Occ : Exception_Occurrence;
      --  This occurrence will be used to display a message after finalization.
      --  It is necessary to save a copy here, or else the designated value
      --  could be overwritten if an exception is raised during finalization
      --  (even if that exception is caught). The occurrence is saved on the
      --  stack to avoid dynamic allocation (if this exception is due to lack
      --  of space in the heap, we therefore avoid a second failure). We assume
      --  that there is enough room on the stack however.

   begin
      Save_Occurrence (Occ, Excep.all);
      Last_Chance_Handler (Occ);
   end Unhandled_Exception_Terminate;

   ------------------------------------
   -- Handling GNAT.Exception_Traces --
   ------------------------------------

   --  The bulk of exception traces output is centralized in Notify_Exception,
   --  for both the Handled and Unhandled cases. Extra task specific output is
   --  triggered in the task wrapper for unhandled occurrences in tasks. It is
   --  not performed in this unit to avoid dependencies on the tasking units
   --  here.

   --  We used to rely on the output performed by Unhanded_Exception_Terminate
   --  for the case of an unhandled occurrence in the environment thread, and
   --  the task wrapper was responsible for the whole output in the tasking
   --  case.

   --  This initial scheme had a drawback: the output from Terminate only
   --  occurs after finalization is done, which means possibly never if some
   --  tasks keep hanging around.

   --  The first "presumably obvious" fix consists in moving the Terminate
   --  output before the finalization. It has not been retained because it
   --  introduces annoying changes in output orders when the finalization
   --  itself issues outputs, this also in "regular" cases not resorting to
   --  Exception_Traces.

   --  Today's solution has the advantage of simplicity and better isolates
   --  the Exception_Traces machinery.

end Exception_Traces;
