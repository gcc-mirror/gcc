------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  A D A . E X C E P T I O N S . E X C E P T I O N _ P R O P A G A T I O N --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This is the version using the GCC EH mechanism

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Storage_Elements;  use System.Storage_Elements;

separate (Ada.Exceptions)
package body Exception_Propagation is

   ------------------------------------------------
   -- Entities to interface with the GCC runtime --
   ------------------------------------------------

   --  These come from "C++ ABI for Itanium: Exception handling", which is
   --  the reference for GCC. They are used only when we are relying on
   --  back-end tables for exception propagation, which in turn is currently
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

   type Exception_Class is mod 2 ** 64;

   GNAT_Exception_Class : constant Exception_Class := 16#474e552d41646100#;
   --  "GNU-Ada\0"

   type Unwind_Word is mod 2 ** System.Word_Size;
   for Unwind_Word'Size use System.Word_Size;
   --  Map the corresponding C type used in Unwind_Exception below

   type Unwind_Exception is record
      Class    : Exception_Class := GNAT_Exception_Class;
      Cleanup  : System.Address  := System.Null_Address;
      Private1 : Unwind_Word;
      Private2 : Unwind_Word;
   end record;
   --  Map the GCC struct used for exception handling

   for Unwind_Exception'Alignment use Standard'Maximum_Alignment;
   --  The C++ ABI mandates the common exception header to be at least
   --  doubleword aligned, and the libGCC implementation actually makes it
   --  maximally aligned (see unwind.h). See additional comments on the
   --  alignment below.

   --------------------------------------------------------------
   -- GNAT Specific Entities To Deal With The GCC EH Circuitry --
   --------------------------------------------------------------

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime.

   type GNAT_GCC_Exception is record
      Header : Unwind_Exception;
      --  ABI Exception header first

      Id : Exception_Id;
      --  GNAT Exception identifier.  This is filled by Propagate_Exception
      --  and then used by the personality routine to determine if the context
      --  it examines contains a handler for the exception being propagated.

      N_Cleanups_To_Trigger : Integer;
      --  Number of cleanup only frames encountered in SEARCH phase.  This is
      --  initialized to 0 by Propagate_Exception and maintained by the
      --  personality routine to control a forced unwinding phase triggering
      --  all the cleanups before calling Unhandled_Exception_Terminate when
      --  an exception is not handled.

      Next_Exception : EOA;
      --  Used to create a linked list of exception occurrences
   end record;

   pragma Convention (C, GNAT_GCC_Exception);

   --  There is a subtle issue with the common header alignment, since the C
   --  version is aligned on BIGGEST_ALIGNMENT, the Ada version is aligned on
   --  Standard'Maximum_Alignment, and those two values don't quite represent
   --  the same concepts and so may be decoupled someday. One typical reason
   --  is that BIGGEST_ALIGNMENT may be larger than what the underlying system
   --  allocator guarantees, and there are extra costs involved in allocating
   --  objects aligned to such factors.

   --  To deal with the potential alignment differences between the C and Ada
   --  representations, the Ada part of the whole structure is only accessed
   --  by the personality routine through the accessors declared below.  Ada
   --  specific fields are thus always accessed through consistent layout, and
   --  we expect the actual alignment to always be large enough to avoid traps
   --  from the C accesses to the common header. Besides, accessors alleviate
   --  the need for a C struct whole counterpart, both painful and error-prone
   --  to maintain anyway.

   type GNAT_GCC_Exception_Access is access all GNAT_GCC_Exception;

   function To_GNAT_GCC_Exception is new
     Unchecked_Conversion (System.Address, GNAT_GCC_Exception_Access);

   procedure Free is new Unchecked_Deallocation
     (GNAT_GCC_Exception, GNAT_GCC_Exception_Access);

   procedure Free is new Unchecked_Deallocation
     (Exception_Occurrence, EOA);

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null access GNAT_GCC_Exception;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code;
   --  Hook called at each step of the forced unwinding we perform to
   --  trigger cleanups found during the propagation of an unhandled
   --  exception.

   --  GCC runtime functions used. These are C non-void functions, actually,
   --  but we ignore the return values. See raise.c as to why we are using
   --  __gnat stubs for these.

   procedure Unwind_RaiseException
     (UW_Exception : not null access GNAT_GCC_Exception);
   pragma Import (C, Unwind_RaiseException, "__gnat_Unwind_RaiseException");

   procedure Unwind_ForcedUnwind
     (UW_Exception : not null access GNAT_GCC_Exception;
      UW_Handler   : System.Address;
      UW_Argument  : System.Address);
   pragma Import (C, Unwind_ForcedUnwind, "__gnat_Unwind_ForcedUnwind");

   ------------------------------------------------------------------
   -- Occurrence Stack Management Facilities for the GCC-EH Scheme --
   ------------------------------------------------------------------

   function Remove
     (Top   : EOA;
      Excep : GNAT_GCC_Exception_Access) return Boolean;
   --  Remove Excep from the stack starting at Top.
   --  Return True if Excep was found and removed, false otherwise.

   --  Hooks called when entering/leaving an exception handler for a given
   --  occurrence, aimed at handling the stack of active occurrences. The
   --  calls are generated by gigi in tree_transform/N_Exception_Handler.

   procedure Begin_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   procedure End_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

   Setup_Key : constant := 16#DEAD#;
   --  To handle the case of a task "transferring" an exception occurrence to
   --  another task, for instance via Exceptional_Complete_Rendezvous, we need
   --  to be able to identify occurrences which have been Setup and not yet
   --  Propagated. We hijack one of the common header fields for that purpose,
   --  setting it to a special key value during the setup process, clearing it
   --  at the very beginning of the propagation phase, and expecting it never
   --  to be reset to the special value later on. A 16-bit value is used rather
   --  than a 32-bit value for static compatibility with 16-bit targets such as
   --  AAMP (where type Unwind_Word will be 16 bits).

   function Is_Setup_And_Not_Propagated (E : EOA) return Boolean;

   procedure Set_Setup_And_Not_Propagated (E : EOA);
   procedure Clear_Setup_And_Not_Propagated (E : EOA);

   procedure Save_Occurrence_And_Private
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence);
   --  Copy all the components of Source to Target as well as the
   --  Private_Data pointer.

   --------------------------------------------------------------------
   -- Accessors to Basic Components of a GNAT Exception Data Pointer --
   --------------------------------------------------------------------

   --  As of today, these are only used by the C implementation of the GCC
   --  propagation personality routine to avoid having to rely on a C
   --  counterpart of the whole exception_data structure, which is both
   --  painful and error prone. These subprograms could be moved to a more
   --  widely visible location if need be.

   function Is_Handled_By_Others (E : Exception_Data_Ptr) return Boolean;
   pragma Export (C, Is_Handled_By_Others, "__gnat_is_handled_by_others");
   pragma Warnings (Off, Is_Handled_By_Others);

   function Language_For (E : Exception_Data_Ptr) return Character;
   pragma Export (C, Language_For, "__gnat_language_for");

   function Import_Code_For (E : Exception_Data_Ptr) return Exception_Code;
   pragma Export (C, Import_Code_For, "__gnat_import_code_for");

   function EID_For (GNAT_Exception : GNAT_GCC_Exception_Access)
     return Exception_Id;
   pragma Export (C, EID_For, "__gnat_eid_for");

   procedure Adjust_N_Cleanups_For
     (GNAT_Exception : GNAT_GCC_Exception_Access;
      Adjustment     : Integer);
   pragma Export (C, Adjust_N_Cleanups_For, "__gnat_adjust_n_cleanups_for");

   ---------------------------------------------------------------------------
   -- Objects to materialize "others" and "all others" in the GCC EH tables --
   ---------------------------------------------------------------------------

   --  Currently, these only have their address taken and compared so there is
   --  no real point having whole exception data blocks allocated. In any case
   --  the types should match what gigi and the personality routine expect.
   --  The initial value is an arbitrary value that will not exceed the range
   --  of Integer on 16-bit targets (such as AAMP).

   Others_Value : constant Integer := 16#7FFF#;
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : constant Integer := 16#7FFF#;
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   ------------
   -- Remove --
   ------------

   function Remove
     (Top   : EOA;
      Excep : GNAT_GCC_Exception_Access) return Boolean
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
      UW_Exception : not null access GNAT_GCC_Exception;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code
   is
      pragma Unreferenced
        (UW_Version, UW_Phases, UW_Eclass, UW_Context, UW_Argument);

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

   ---------------------------------
   -- Is_Setup_And_Not_Propagated --
   ---------------------------------

   function Is_Setup_And_Not_Propagated (E : EOA) return Boolean is
      GCC_E : constant GNAT_GCC_Exception_Access :=
                To_GNAT_GCC_Exception (E.Private_Data);
   begin
      return GCC_E /= null and then GCC_E.Header.Private1 = Setup_Key;
   end Is_Setup_And_Not_Propagated;

   ------------------------------------
   -- Clear_Setup_And_Not_Propagated --
   ------------------------------------

   procedure Clear_Setup_And_Not_Propagated (E : EOA) is
      GCC_E : constant GNAT_GCC_Exception_Access :=
                To_GNAT_GCC_Exception (E.Private_Data);
   begin
      pragma Assert (GCC_E /= null);
      GCC_E.Header.Private1 := 0;
   end Clear_Setup_And_Not_Propagated;

   ----------------------------------
   -- Set_Setup_And_Not_Propagated --
   ----------------------------------

   procedure Set_Setup_And_Not_Propagated (E : EOA) is
      GCC_E : constant GNAT_GCC_Exception_Access :=
                To_GNAT_GCC_Exception (E.Private_Data);
   begin
      pragma Assert (GCC_E /= null);
      GCC_E.Header.Private1 := Setup_Key;
   end Set_Setup_And_Not_Propagated;

   --------------------------------
   -- Save_Occurrence_And_Private --
   --------------------------------

   procedure Save_Occurrence_And_Private
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      Save_Occurrence_No_Private (Target, Source);
      Target.Private_Data := Source.Private_Data;
   end Save_Occurrence_And_Private;

   ---------------------
   -- Setup_Exception --
   ---------------------

   --  In the GCC-EH implementation of the propagation scheme, this
   --  subprogram should be understood as: Setup the exception occurrence
   --  stack headed at Current for a forthcoming raise of Excep.

   procedure Setup_Exception
     (Excep    : EOA;
      Current  : EOA;
      Reraised : Boolean := False)
   is
      Top           : constant EOA := Current;
      Next          : EOA;
      GCC_Exception : GNAT_GCC_Exception_Access;

   begin
      --  The exception Excep is soon to be propagated, and the
      --  storage used for that will be the occurrence statically allocated
      --  for the current thread. This storage might currently be used for a
      --  still active occurrence, so we need to push it on the thread's
      --  occurrence stack (headed at that static occurrence) before it gets
      --  clobbered.

      --  What we do here is to trigger this push when need be, and allocate a
      --  Private_Data block for the forthcoming Propagation.

      --  Some tasking rendez-vous attempts lead to an occurrence transfer
      --  from the server to the client (see Exceptional_Complete_Rendezvous).
      --  In those cases Setup is called twice for the very same occurrence
      --  before it gets propagated: once from the server, because this is
      --  where the occurrence contents is elaborated and known, and then
      --  once from the client when it detects the case and actually raises
      --  the exception in its own context.

      --  The Is_Setup_And_Not_Propagated predicate tells us when we are in
      --  the second call to Setup for a Transferred occurrence, and there is
      --  nothing to be done here in this situation. This predicate cannot be
      --  True if we are dealing with a Reraise, and we may even be called
      --  with a raw uninitialized Excep occurrence in this case so we should
      --  not check anyway. Observe the front-end expansion for a "raise;" to
      --  see that happening. We get a local occurrence and a direct call to
      --  Save_Occurrence without the intermediate init-proc call.

      if not Reraised and then Is_Setup_And_Not_Propagated (Excep) then
         return;
      end if;

      --  Allocate what will be the Private_Data block for the exception
      --  to be propagated.

      GCC_Exception := new GNAT_GCC_Exception;

      --  If the Top of the occurrence stack is not currently used for an
      --  active exception (the stack is empty) we just need to setup the
      --  Private_Data pointer.

      --  Otherwise, we also need to shift the contents of the Top of the
      --  stack in a freshly allocated entry and link everything together.

      if Top.Private_Data /= System.Null_Address then
         Next := new Exception_Occurrence;
         Save_Occurrence_And_Private (Next.all, Top.all);

         GCC_Exception.Next_Exception := Next;
         Top.Private_Data := GCC_Exception.all'Address;
      end if;

      Top.Private_Data := GCC_Exception.all'Address;

      Set_Setup_And_Not_Propagated (Top);
   end Setup_Exception;

   -------------------
   -- Begin_Handler --
   -------------------

   procedure Begin_Handler (GCC_Exception : GNAT_GCC_Exception_Access) is
      pragma Unreferenced (GCC_Exception);

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

   procedure Propagate_Exception
     (E                   : Exception_Id;
      From_Signal_Handler : Boolean)
   is
      pragma Inspection_Point (E);
      pragma Unreferenced (From_Signal_Handler);

      Excep         : constant EOA := Get_Current_Excep.all;
      GCC_Exception : GNAT_GCC_Exception_Access;

   begin
      pragma Assert (Excep.Private_Data /= System.Null_Address);

      --  Retrieve the Private_Data for this occurrence and set the useful
      --  flags for the personality routine, which will be called for each
      --  frame via Unwind_RaiseException below.

      GCC_Exception := To_GNAT_GCC_Exception (Excep.Private_Data);

      Clear_Setup_And_Not_Propagated (Excep);

      GCC_Exception.Id := Excep.Id;
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

      --  We get here when there is no handler or cleanup to be run at all.
      --  The debugger has been notified before the second step above.

      Unhandled_Exception_Terminate;
   end Propagate_Exception;

   ---------------------------
   -- Adjust_N_Cleanups_For --
   ---------------------------

   procedure Adjust_N_Cleanups_For
     (GNAT_Exception : GNAT_GCC_Exception_Access;
      Adjustment     : Integer)
   is
   begin
      GNAT_Exception.N_Cleanups_To_Trigger :=
        GNAT_Exception.N_Cleanups_To_Trigger + Adjustment;
   end Adjust_N_Cleanups_For;

   -------------
   -- EID_For --
   -------------

   function EID_For
     (GNAT_Exception : GNAT_GCC_Exception_Access) return Exception_Id
   is
   begin
      return GNAT_Exception.Id;
   end EID_For;

   ---------------------
   -- Import_Code_For --
   ---------------------

   function Import_Code_For
     (E : SSL.Exception_Data_Ptr) return Exception_Code
   is
   begin
      return E.all.Import_Code;
   end Import_Code_For;

   --------------------------
   -- Is_Handled_By_Others --
   --------------------------

   function Is_Handled_By_Others (E : SSL.Exception_Data_Ptr) return Boolean is
   begin
      return not E.all.Not_Handled_By_Others;
   end Is_Handled_By_Others;

   ------------------
   -- Language_For --
   ------------------

   function Language_For (E : SSL.Exception_Data_Ptr) return Character is
   begin
      return E.all.Lang;
   end Language_For;

   -----------
   -- Notes --
   -----------

   --  The current model implemented for the stack of occurrences is a
   --  simplification of previous attempts, which all proved to be flawed or
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
   --  most once on the stack requires a way to determine if a given occurrence
   --  is already there, which is not as easy as it might seem.

   --  An attempt was made to use the Private_Data pointer for this purpose.
   --  It did not work because:

   --  1) The Private_Data has to be saved by Save_Occurrence to be usable
   --     as a key in case of a later reraise,

   --  2) There is no easy way to synchronize End_Handler for an occurrence
   --     and the data attached to potential copies, so these copies may end
   --     up pointing to stale data. Moreover ...

   --  3) The same address may be reused for different occurrences, which
   --     defeats the idea of using it as a key.

   --  The example below illustrates:

   --  Saved_CE : Exception_Occurrence;

   --  begin
   --    raise Constraint_Error;
   --  exception
   --    when CE: others =>
   --      Save_Occurrence (Saved_CE, CE);      <= Saved_CE.PDA = CE.PDA
   --  end;

   --                                           <= Saved_CE.PDA is stale (!)

   --  begin
   --    raise Program_Error;                   <= Saved_CE.PDA = PE.PDA (!!)
   --  exception
   --    when others =>
   --      Reraise_Occurrence (Saved_CE);
   --  end;

   --  Not releasing the Private_Data via End_Handler could be an option,
   --  but making this to work while still avoiding memory leaks is far
   --  from trivial.

   --  The current scheme has the advantage of being simple, and induces
   --  extra costs only in reraise cases which is acceptable.

end Exception_Propagation;
