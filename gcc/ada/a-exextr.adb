------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      ADA.EXCEPTIONS.EXCEPTION_TRACES                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

with Unchecked_Conversion;

pragma Warnings (Off);
with Ada.Exceptions.Last_Chance_Handler;
pragma Warnings (On);
--  Bring last chance handler into closure

separate (Ada.Exceptions)
package body Exception_Traces is

   Nline : constant String := String'(1 => ASCII.LF);
   --  Convenient shortcut

   type Exception_Action is access procedure (E : Exception_Occurrence);
   Global_Action : Exception_Action := null;
   pragma Export
     (Ada, Global_Action, "__gnat_exception_actions_global_action");
   --  Global action, executed whenever an exception is raised.  Changing the
   --  export name must be coordinated with code in g-excact.adb.

   Raise_Hook_Initialized : Boolean := False;
   pragma Export
     (Ada, Raise_Hook_Initialized, "__gnat_exception_actions_initialized");

   procedure Last_Chance_Handler
     (Except :  Exception_Occurrence);
   pragma Import (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);
   --  Users can replace the default version of this routine,
   --  Ada.Exceptions.Last_Chance_Handler.

   function To_Action is new Unchecked_Conversion
     (Raise_Action, Exception_Action);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean);
   --  Factorizes the common processing for Notify_Handled_Exception and
   --  Notify_Unhandled_Exception. Is_Unhandled is set to True only in the
   --  latter case because Notify_Handled_Exception may be called for an
   --  actually unhandled occurrence in the Front-End-SJLJ case.

   ---------------------------------
   -- Debugger Interface Routines --
   ---------------------------------

   --  The routines here are null routines that normally have no effect.
   --  They are provided for the debugger to place breakpoints on their
   --  entry points to get control on an exception.

   procedure Unhandled_Exception;
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  Hook for GDB to support "break exception unhandled".

   --  For "break exception", GDB uses __gnat_raise_nodefer_with_msg, which
   --  is not in this section because it fullfills other purposes than a mere
   --  debugger interface.

   --------------------------------
   -- Import Run-Time C Routines --
   --------------------------------

   --  The purpose of the following pragma Import is to ensure that we
   --  generate appropriate subprogram descriptors for all C routines in
   --  the standard GNAT library that can raise exceptions. This ensures
   --  that the exception propagation can properly find these routines

   pragma Propagate_Exceptions;

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean) is
   begin
      --  Output the exception information required by the Exception_Trace
      --  configuration. Take care not to output information about internal
      --  exceptions.

      --  ??? In the Front-End ZCX case, the traceback entries we have at this
      --  point only include the ones we stored while walking up the stack *up
      --  to the handler*. All the frames above the subprogram in which the
      --  handler is found are missing.

      if not Excep.Id.Not_Handled_By_Others
        and then
        (Exception_Trace = Every_Raise
         or else (Exception_Trace = Unhandled_Raise and then Is_Unhandled))
      then
         To_Stderr (Nline);

         if Is_Unhandled then
            To_Stderr ("Unhandled ");
         end if;

         To_Stderr ("Exception raised");
         To_Stderr (Nline);
         To_Stderr (Tailored_Exception_Information (Excep.all));
      end if;

      --  Call the user-specific actions
      --  ??? We should presumably look at the reraise status here.

      if Raise_Hook_Initialized
        and then Exception_Data_Ptr (Excep.Id).Raise_Hook /= null
      then
         To_Action (Exception_Data_Ptr (Excep.Id).Raise_Hook) (Excep.all);
      end if;

      if Global_Action /= null then
         Global_Action (Excep.all);
      end if;
   end Notify_Exception;

   ------------------------------
   -- Notify_Handled_Exception --
   ------------------------------

   procedure Notify_Handled_Exception is
   begin
      Notify_Exception (Get_Current_Excep.all, Is_Unhandled => False);
   end Notify_Handled_Exception;

   --------------------------------
   -- Notify_Unhandled_Exception --
   --------------------------------

   procedure Notify_Unhandled_Exception is
   begin
      Notify_Exception (Get_Current_Excep.all, Is_Unhandled => True);
      Unhandled_Exception;
   end Notify_Unhandled_Exception;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

   -----------------------------------
   -- Unhandled_Exception_Terminate --
   -----------------------------------

   procedure Unhandled_Exception_Terminate is
      Excep : constant EOA := Save_Occurrence (Get_Current_Excep.all.all);
      --  This occurrence will be used to display a message after finalization.
      --  It is necessary to save a copy here, or else the designated value
      --  could be overwritten if an exception is raised during finalization
      --  (even if that exception is caught).

   begin
      Last_Chance_Handler (Excep.all);
   end Unhandled_Exception_Terminate;


   ------------------------------------
   -- Handling GNAT.Exception_Traces --
   ------------------------------------

   --  The bulk of exception traces output is centralized in Notify_Exception,
   --  for both the Handled and Unhandled cases. Extra task specific output is
   --  triggered in the task wrapper for unhandled occurrences in tasks. It is
   --  not performed in this unit to avoid dragging dependencies against the
   --  tasking units here.

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

   --  It currently outputs the information about unhandled exceptions twice
   --  in the environment thread, once in the notification routine and once in
   --  the termination routine. Avoiding the second output is possible but so
   --  far has been considered undesirable. It would mean changing the order
   --  of outputs between the two runs with or without exception traces, while
   --  it seems preferrable to only have additional outputs in the former
   --  case.

end Exception_Traces;
