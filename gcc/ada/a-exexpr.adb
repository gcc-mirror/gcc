------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     ADA.EXCEPTIONS.EXCEPTION_PROPAGATION                 --
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

with Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off);
--  Since several constructs give warnings in 3.14a1, including unreferenced
--  variables and pragma Unreferenced itself.

separate (Ada.Exceptions)
package body Exception_Propagation is

   ------------------------------------------------
   -- Entities to interface with the GCC runtime --
   ------------------------------------------------

   --  These come from "C++ ABI for Itanium: Exception handling", which is
   --  the reference for GCC. They are used only when we are relying on
   --  back-end tables for exception propagation, which in turn is currenly
   --  only the case for Zero_Cost_Exceptions in GNAT5.

   --  Return codes from the GCC runtime functions used to propagate
   --  an exception.

   type Unwind_Reason_Code is
     (URC_NO_REASON,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_PHASE2_ERROR,
      URC_PHASE1_ERROR,
      URC_NORMAL_STOP,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND);

   pragma Unreferenced
     (URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_PHASE2_ERROR,
      URC_PHASE1_ERROR,
      URC_NORMAL_STOP,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND);

   pragma Convention (C, Unwind_Reason_Code);

   --  Phase identifiers

   type Unwind_Action is
     (UA_SEARCH_PHASE,
      UA_CLEANUP_PHASE,
      UA_HANDLER_FRAME,
      UA_FORCE_UNWIND);

   for Unwind_Action use
      (UA_SEARCH_PHASE  => 1,
       UA_CLEANUP_PHASE => 2,
       UA_HANDLER_FRAME => 4,
       UA_FORCE_UNWIND  => 8);

   pragma Convention (C, Unwind_Action);

   --  Mandatory common header for any exception object handled by the
   --  GCC unwinding runtime.

   subtype Exception_Class is Interfaces.Unsigned_64;

   GNAT_Exception_Class : constant Exception_Class := 16#474e552d41646100#;
   --  "GNU-Ada\0"

   type Unwind_Exception is record
      Class    : Exception_Class := GNAT_Exception_Class;
      Cleanup  : System.Address  := System.Null_Address;
      Private1 : Integer;
      Private2 : Integer;
   end record;

   pragma Convention (C, Unwind_Exception);

   for Unwind_Exception'Alignment use Standard'Maximum_Alignment;
   --  The C++ ABI mandates the common exception header to be at least
   --  doubleword aligned, and the libGCC implementation actually makes it
   --  maximally aligned (see unwind.h). We need to match this because:

   --  1/ We pass pointers to such headers down to the underlying
   --     libGCC unwinder,

   --  and

   --  2/ The GNAT_GCC_Exception record below starts with this common
   --     common header and has a C counterpart which needs to be laid
   --     out identically in raise.c. If the alignment of the C and Ada
   --     common headers mismatch, their size may also differ, and the
   --     layouts may not match anymore.

   ---------------------------------------------------------------
   --  GNAT specific entities to deal with the GCC eh circuitry --
   ---------------------------------------------------------------

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime. This structure shall match the
   --  one in raise.c and is currently experimental as it might be merged
   --  with the GNAT runtime definition some day.

   type GNAT_GCC_Exception is record
      Header : Unwind_Exception;
      --  ABI Exception header first.

      Id : Exception_Id;
      --  GNAT Exception identifier. This is used by the personality
      --  routine to determine if the context it examines contains a
      --  handler for the exception beeing propagated.

      Handled_By_Others : Boolean;
      --  Is this exception handled by "when others" ? This is used by the
      --  personality routine to determine if an "others" handler in the
      --  context it examines may catch the exception beeing propagated.

      N_Cleanups_To_Trigger : Integer;
      --  Number of cleanup only frames encountered in SEARCH phase.
      --  This is used to control the forced unwinding triggered when
      --  no handler has been found.

      Next_Exception : EOA;
      --  Used to create a linked list of exception occurrences.
   end record;

   pragma Convention (C, GNAT_GCC_Exception);

   type GNAT_GCC_Exception_Access is access all GNAT_GCC_Exception;

   function To_GNAT_GCC_Exception is new
     Unchecked_Conversion (System.Address, GNAT_GCC_Exception_Access);

   procedure Free is new Unchecked_Deallocation
     (GNAT_GCC_Exception, GNAT_GCC_Exception_Access);

   procedure Free is new Unchecked_Deallocation
     (Exception_Occurrence, EOA);

   function Remove
     (Top   : EOA;
      Excep : GNAT_GCC_Exception_Access)
      return  Boolean;
   --  Remove Excep from the stack starting at Top.
   --  Return True if Excep was found and removed, false otherwise.

   --  Hooks called when entering/leaving an exception handler for a given
   --  occurrence, aimed at handling the stack of active occurrences. The
   --  calls are generated by gigi in tree_transform/N_Exception_Handler.

   procedure Begin_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   procedure End_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : access GNAT_GCC_Exception;
      UW_Context   : System.Address;
      UW_Argument  : System.Address)
      return         Unwind_Reason_Code;
   --  Hook called at each step of the forced unwinding we perform to
   --  trigger cleanups found during the propagation of an unhandled
   --  exception.

   --  GCC runtime functions used. These are C non-void functions, actually,
   --  but we ignore the return values. See raise.c as to why we are using
   --  __gnat stubs for these.

   procedure Unwind_RaiseException
     (UW_Exception : access GNAT_GCC_Exception);
   pragma Import (C, Unwind_RaiseException, "__gnat_Unwind_RaiseException");

   procedure Unwind_ForcedUnwind
     (UW_Exception : access GNAT_GCC_Exception;
      UW_Handler   : System.Address;
      UW_Argument  : System.Address);
   pragma Import (C, Unwind_ForcedUnwind, "__gnat_Unwind_ForcedUnwind");

   ------------
   -- Remove --
   ------------

   function Remove
     (Top   : EOA;
      Excep : GNAT_GCC_Exception_Access)
      return  Boolean
   is
      Prev          : GNAT_GCC_Exception_Access := null;
      Iter          : EOA := Top;
      GCC_Exception : GNAT_GCC_Exception_Access;

   begin
      --  Pop stack

      loop
         pragma Assert (Iter.Private_Data /= System.Null_Address);

         GCC_Exception := To_GNAT_GCC_Exception (Iter.Private_Data);

         if GCC_Exception = Excep then
            if Prev = null then

               --  Special case for the top of the stack: shift the contents
               --  of the next item to the top, since top is at a fixed
               --  location and can't be changed.

               Iter := GCC_Exception.Next_Exception;

               if Iter = null then

                  --  Stack is now empty

                  Top.Private_Data := System.Null_Address;

               else
                  Save_Occurrence_And_Private (Top.all, Iter.all);
                  Free (Iter);
               end if;

            else
               Prev.Next_Exception := GCC_Exception.Next_Exception;
               Free (Iter);
            end if;

            Free (GCC_Exception);

            return True;
         end if;

         exit when GCC_Exception.Next_Exception = null;

         Prev := GCC_Exception;
         Iter := GCC_Exception.Next_Exception;
      end loop;

      return False;
   end Remove;

   ---------------------------
   -- CleanupUnwind_Handler --
   ---------------------------

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : access GNAT_GCC_Exception;
      UW_Context   : System.Address;
      UW_Argument  : System.Address)
      return         Unwind_Reason_Code
   is
   begin
      --  Terminate as soon as we know there is nothing more to run. The
      --  count is maintained by the personality routine.

      if UW_Exception.N_Cleanups_To_Trigger = 0 then
         Unhandled_Exception_Terminate;
      end if;

      --  We know there is at least one cleanup further up. Return so that it
      --  is searched and entered, after which Unwind_Resume will be called
      --  and this hook will gain control (with an updated count) again.

      return URC_NO_REASON;
   end CleanupUnwind_Handler;

   ---------------------
   -- Setup_Exception --
   ---------------------

   --  Push the current exception occurrence on the stack before overriding it.

   procedure Setup_Exception
     (Excep    : EOA;
      Current  : EOA;
      Reraised : Boolean := False)
   is
      Top           : constant EOA := Current;
      Next          : EOA;
      GCC_Exception : GNAT_GCC_Exception_Access;

      --  Note that we make no use of the Reraised indication at this point.

      --  The information is still passed around just in case of future needs,
      --  since we've already switched between using/not-using it a number of
      --  times.

   begin
      --  If the current exception is not live, the stack is empty and there
      --  is nothing to do. Note that the stack always appears empty for
      --  mechanisms that do not require one. For the mechanism we implement
      --  in this unit, the initial Private_Data allocation for an occurrence
      --  is issued by Propagate_Exception.

      if Top.Private_Data = System.Null_Address then
         return;
      end if;

      --  Shift the contents of the Top of the stack in a freshly allocated
      --  entry, which leaves the room in the fixed Top entry available for the
      --  occurrence about to be propagated.

      Next := new Exception_Occurrence;
      Save_Occurrence_And_Private (Next.all, Top.all);

      --  Allocate Private_Data for the occurrence about to be propagated
      --  and link everything together.

      GCC_Exception := new GNAT_GCC_Exception;
      GCC_Exception.Next_Exception := Next;

      Top.Private_Data := GCC_Exception.all'Address;

   end Setup_Exception;

   -------------------
   -- Begin_Handler --
   -------------------

   procedure Begin_Handler (GCC_Exception : GNAT_GCC_Exception_Access) is
   begin
      --  Every necessary operation related to the occurrence stack has
      --  already been performed by Propagate_Exception. This hook remains for
      --  potential future necessity in optimizing the overall scheme, as well
      --  a useful debugging tool.
      null;
   end Begin_Handler;

   -----------------
   -- End_Handler --
   -----------------

   procedure End_Handler (GCC_Exception : GNAT_GCC_Exception_Access) is
      Removed : Boolean;

   begin
      Removed := Remove (Get_Current_Excep.all, GCC_Exception);
      pragma Assert (Removed);
   end End_Handler;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   --  Build an object suitable for the libgcc processing and call
   --  Unwind_RaiseException to actually throw, taking care of handling
   --  the two phase scheme it implements.

   procedure Propagate_Exception (From_Signal_Handler : Boolean) is
      Excep         : EOA := Get_Current_Excep.all;
      GCC_Exception : GNAT_GCC_Exception_Access;

   begin
      if Excep.Private_Data = System.Null_Address then
         GCC_Exception := new GNAT_GCC_Exception;
         Excep.Private_Data := GCC_Exception.all'Address;
      else
         GCC_Exception := To_GNAT_GCC_Exception (Excep.Private_Data);
      end if;

      --  Fill in the useful flags for the personality routine called for each
      --  frame via Unwind_RaiseException below.

      GCC_Exception.Id := Excep.Id;
      GCC_Exception.Handled_By_Others := not Excep.Id.Not_Handled_By_Others;
      GCC_Exception.N_Cleanups_To_Trigger := 0;

      --  Compute the backtrace for this occurrence if the corresponding
      --  binder option has been set. Call_Chain takes care of the reraise
      --  case.

      --  ??? Using Call_Chain here means we are going to walk up the stack
      --  once only for backtracing purposes before doing it again for the
      --  propagation per se.

      --  The first inspection is much lighter, though, as it only requires
      --  partial unwinding of each frame. Additionally, although we could use
      --  the personality routine to record the addresses while propagating,
      --  this method has two drawbacks:

      --  1) the trace is incomplete if the exception is handled since we
      --  don't walk past the frame with the handler,

      --    and

      --  2) we would miss the frames for which our personality routine is not
      --  called, e.g. if C or C++ calls are on the way.

      Call_Chain (Excep);

      --  Perform a standard raise first. If a regular handler is found, it
      --  will be entered after all the intermediate cleanups have run. If
      --  there is no regular handler, control will get back to after the
      --  call, with N_Cleanups_To_Trigger set to the number of frames with
      --  cleanups found on the way up, and none of these already run.

      Unwind_RaiseException (GCC_Exception);

      --  If we get here we know the exception is not handled, as otherwise
      --  Unwind_RaiseException arranges for the handler to be entered. Take
      --  the necessary steps to enable the debugger to gain control while the
      --  stack is still intact.

      Notify_Unhandled_Exception;

      --  Now, if cleanups have been found, run a forced unwind to trigger
      --  them. Control should not resume there, as the unwinding hook calls
      --  Unhandled_Exception_Terminate as soon as the last cleanup has been
      --  triggered.

      if GCC_Exception.N_Cleanups_To_Trigger /= 0 then
         Unwind_ForcedUnwind (GCC_Exception,
                              CleanupUnwind_Handler'Address,
                              System.Null_Address);
      end if;

      --  We get here when there is no handler or cleanup to be run at
      --  all. The debugger has been notified before the second step above.

      Unhandled_Exception_Terminate;
   end Propagate_Exception;

   -----------
   -- Notes --
   -----------

   --  The current model implemented for the stack of occurrences is a
   --  simplification of previous attempts, which all prooved to be flawed or
   --  would have needed significant additional circuitry to be made to work
   --  correctly.

   --  We now represent every propagation by a new entry on the stack, which
   --  means that an exception occurrence may appear more than once (e.g. when
   --  it is reraised during the course of its own handler).

   --  This may seem overcostly compared to the C++ model as implemented in
   --  the g++ v3 libstd. This is actually understandable when one considers
   --  the extra variations of possible run-time configurations induced by the
   --  freedom offered by the Save_Occurrence/Reraise_Occurrence public
   --  interface.

   --  The basic point is that arranging for an occurrence to always appear at
   --  most once on the stack requires a way to determine if a given occurence
   --  is already there, which is not as easy as it might seem.

   --  An attempt was made to use the Private_Data pointer for this purpose.
   --  It did not work because:

   --  1/ The Private_Data has to be saved by Save_Occurrence to be usable
   --     as a key in case of a later reraise,

   --  2/ There is no easy way to synchronize End_Handler for an occurrence
   --     and the data attached to potential copies, so these copies may end
   --     up pointing to stale data. Moreover ...

   --  3/ The same address may be reused for different occurrences, which
   --     defeats the idea of using it as a key.

   --  The example below illustrates:

   --  Saved_CE : Exception_Occurrence;
   --
   --  begin
   --    raise Constraint_Error;
   --  exception
   --    when CE: others =>
   --      Save_Occurrence (Saved_CE, CE);      <= Saved_CE.PDA = CE.PDA
   --  end;
   --
   --                                           <= Saved_CE.PDA is stale (!)
   --
   --  begin
   --    raise Program_Error;                   <= Saved_CE.PDA = PE.PDA (!!)
   --  exception
   --    when others =>
   --      Reraise_Occurrence (Saved_CE);
   --  end;

   --  Not releasing the Private_Data via End_Handler could be an option,
   --  but making this to work while still avoiding memory leaks is far
   --  from trivial.

   --  The current scheme has the advantage of beeing simple, and induces
   --  extra costs only in reraise cases which is acceptable.

end Exception_Propagation;
