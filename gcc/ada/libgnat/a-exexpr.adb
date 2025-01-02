------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  A D A . E X C E P T I O N S . E X C E P T I O N _ P R O P A G A T I O N --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This is the version using the GCC EH mechanism, which could rely on
--  different underlying unwinding engines, for example DWARF or ARM unwind
--  info based. Here is a sketch of the most prominent data structures
--  involved:

--      (s-excmac.ads)
--      GNAT_GCC_Exception:
--      *-----------------------------------*
--  o-->|          (s-excmac.ads)           |
--  |   | Header : <gcc occurrence type>    |
--  |   |   - Class                         |
--  |   |   ...                             |    Constraint_Error:
--  |   |-----------------------------------*    Program_Error:
--  |   |              (a-except.ads)       |    Foreign_Exception:
--  |   | Occurrence : Exception_Occurrence |
--  |   |                                   |    (s-stalib. ads)
--  |   |   - Id : Exception_Id  --------------> Exception_Data
--  o------ - Machine_Occurrence            |   *------------------------*
--      |   - Msg                           |   | Not_Handled_By_Others  |
--      |   - Traceback                     |   | Lang                   |
--      |   ...                             |   | Foreign_Data --o       |
--      *-----------------------------------*   | Full_Name      |       |
--        ||                                    | ...            |       |
--        ||          foreign rtti blob         *----------------|-------*
--        ||          *---------------*                          |
--        ||          |   ...   ...   |<-------------------------o
--        ||          *---------------*
--        ||
--     Setup_Current_Excep()
--        ||
--        ||   Latch into ATCB or
--        ||   environment Current Exception Buffer:
--        ||
--        vv
--     <> : Exception_Occurrence
--     *---------------------------*
--     | ...  ...  ... ... ... ... * --- Get_Current_Excep() ---->
--     *---------------------------*

--  On "raise" events, the runtime allocates a new GNAT_GCC_Exception
--  instance and eventually calls into libgcc's Unwind_RaiseException.
--  This part handles the object through the header part only.

--  During execution, Get_Current_Excep provides a pointer to the
--  Exception_Occurrence being raised or last raised by the current task.

--  This is actually the address of a statically allocated
--  Exception_Occurrence attached to the current ATCB or to the environment
--  thread into which an occurrence being raised is synchronized at critical
--  points during the raise process, via Setup_Current_Excep.

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Storage_Elements;   use System.Storage_Elements;
with System.Exceptions.Machine; use System.Exceptions.Machine;

separate (Ada.Exceptions)
package body Exception_Propagation is

   use Exception_Traces;

   type bool is new Boolean;
   pragma Convention (C, bool);

   Foreign_Exception : aliased System.Standard_Library.Exception_Data;
   pragma Import (Ada, Foreign_Exception,
                  "system__exceptions__foreign_exception");
   --  Id for foreign exceptions

   --------------------------------------------------------------
   -- GNAT Specific Entities To Deal With The GCC EH Circuitry --
   --------------------------------------------------------------

   --  Phase identifiers (Unwind Actions)

   type Unwind_Action is new Integer;
   pragma Convention (C, Unwind_Action);

   UA_SEARCH_PHASE  : constant Unwind_Action := 1;
   UA_CLEANUP_PHASE : constant Unwind_Action := 2;
   UA_HANDLER_FRAME : constant Unwind_Action := 4;
   UA_FORCE_UNWIND  : constant Unwind_Action := 8;
   UA_END_OF_STACK  : constant Unwind_Action := 16;  --  GCC extension

   pragma Unreferenced
     (UA_HANDLER_FRAME,
      UA_FORCE_UNWIND,
      UA_END_OF_STACK);

   procedure GNAT_GCC_Exception_Cleanup
     (Reason : Unwind_Reason_Code;
      Excep  : not null GNAT_GCC_Exception_Access);
   pragma Convention (C, GNAT_GCC_Exception_Cleanup);
   --  Procedure called when a GNAT GCC exception is free.

   procedure Propagate_GCC_Exception
     (GCC_Exception : not null GCC_Exception_Access);
   pragma No_Return (Propagate_GCC_Exception);
   --  Propagate a GCC exception

   procedure Reraise_GCC_Exception
     (GCC_Exception : not null GCC_Exception_Access);
   pragma No_Return (Reraise_GCC_Exception);
   pragma Export (C, Reraise_GCC_Exception, "__gnat_reraise_zcx");
   --  Called to implement raise without exception, ie reraise. Called
   --  directly from gigi.

   function Setup_Current_Excep
     (GCC_Exception : not null GCC_Exception_Access;
      Phase : Unwind_Action;
      Id : Exception_Id) return EOA;
   pragma Export (C, Setup_Current_Excep, "__gnat_setup_current_excep");
   --  Acknowledge GCC_Exception as the current exception object being
   --  raised, which could be an Ada or a foreign exception object.  Return
   --  a pointer to the embedded Ada occurrence for an Ada exception object,
   --  to the current exception buffer otherwise.
   --
   --  Synchronize the current exception buffer as needed for possible
   --  accesses through Get_Current_Except.all afterwards, depending on the
   --  Phase bits, received either from the personality routine, from a
   --  forced_unwind cleanup handler, or just before the start of propagation
   --  for an Ada exception (Phase 0 in this case).

   procedure Unhandled_Except_Handler
     (GCC_Exception : not null GCC_Exception_Access);
   pragma No_Return (Unhandled_Except_Handler);
   pragma Export (C, Unhandled_Except_Handler,
                  "__gnat_unhandled_except_handler");
   --  Called for handle unhandled exceptions, ie the last chance handler
   --  on platforms (such as SEH) that never returns after throwing an
   --  exception. Called directly by gigi.

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null GCC_Exception_Access;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code;
   pragma Import (C, CleanupUnwind_Handler,
                  "__gnat_cleanupunwind_handler");
   --  Hook called at each step of the forced unwinding we perform to trigger
   --  cleanups found during the propagation of an unhandled exception.

   --  GCC runtime functions used. These are C non-void functions, actually,
   --  but we ignore the return values. See raise.c as to why we are using
   --  __gnat stubs for these.

   procedure Unwind_RaiseException
     (UW_Exception : not null GCC_Exception_Access);
   pragma Import (C, Unwind_RaiseException, "__gnat_Unwind_RaiseException");

   procedure Unwind_ForcedUnwind
     (UW_Exception : not null GCC_Exception_Access;
      UW_Handler   : System.Address;
      UW_Argument  : System.Address);
   pragma Import (C, Unwind_ForcedUnwind, "__gnat_Unwind_ForcedUnwind");

   procedure Set_Exception_Parameter
     (Excep         : EOA;
      GCC_Exception : not null GCC_Exception_Access);
   pragma Export
     (C, Set_Exception_Parameter, "__gnat_set_exception_parameter");
   --  Called inserted by gigi to set the exception choice parameter from the
   --  gcc occurrence.

   procedure Set_Foreign_Occurrence
     (Excep : EOA;
      Mo    : System.Address;
      Id    : Exception_Id := Foreign_Exception'Access);
   --  Utility routine to initialize occurrence Excep from a foreign exception
   --  whose machine occurrence is Mo. The message is empty, the backtrace
   --  is empty too and the exception identity is Id.

   --  Hooks called when entering/leaving an exception handler for a
   --  given occurrence.  The calls are generated by gigi in
   --  Exception_Handler_to_gnu_gcc.

   --  Begin_Handler_v1, called when entering an exception handler,
   --  claims responsibility for the handler to release the
   --  GCC_Exception occurrence.  End_Handler_v1, called when
   --  leaving the handler, releases the occurrence, unless the
   --  occurrence is propagating further up, or the handler is
   --  dynamically nested in the context of another handler that
   --  claimed responsibility for releasing that occurrence.

   --  Responsibility is claimed by changing the Cleanup field to
   --  Claimed_Cleanup, which enables claimed exceptions to be
   --  recognized, and avoids accidental releases even by foreign
   --  handlers.

   function Begin_Handler_v1
     (GCC_Exception : not null GCC_Exception_Access)
     return System.Address;
   pragma Export (C, Begin_Handler_v1, "__gnat_begin_handler_v1");
   --  Called when entering an exception handler.  Claim
   --  responsibility for releasing GCC_Exception, by setting the
   --  cleanup/release function to Claimed_Cleanup, and return the
   --  address of the previous cleanup/release function.

   procedure End_Handler_v1
     (GCC_Exception : not null GCC_Exception_Access;
      Saved_Cleanup : System.Address;
      Propagating_Exception : GCC_Exception_Access);
   pragma Export (C, End_Handler_v1, "__gnat_end_handler_v1");
   --  Called when leaving an exception handler.  Restore the
   --  Saved_Cleanup in the GCC_Exception occurrence, and then release
   --  it, unless it remains claimed by an enclosing handler, or
   --  GCC_Exception and Propagating_Exception are the same
   --  occurrence.  Propagating_Exception could be either an
   --  occurrence (re)raised within the handler of GCC_Exception, when
   --  we're executing as an exceptional cleanup, or null, if we're
   --  completing the handler of GCC_Exception normally.

   procedure Claimed_Cleanup
     (Reason : Unwind_Reason_Code;
      GCC_Exception : not null GCC_Exception_Access);
   pragma Export (C, Claimed_Cleanup, "__gnat_claimed_cleanup");
   --  A do-nothing placeholder installed as GCC_Exception.Cleanup
   --  while handling GCC_Exception, to claim responsibility for
   --  releasing it, and to stop it from being accidentally released.

   --  The following are version 0 implementations of the version 1
   --  hooks above.  They remain in place for compatibility with the
   --  output of compilers that still use version 0, such as those
   --  used during bootstrap.  They are interoperable with the v1
   --  hooks, except that the older versions may malfunction when
   --  handling foreign exceptions passed to Reraise_Occurrence.

   procedure Begin_Handler (GCC_Exception : not null GCC_Exception_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");
   --  Called when entering an exception handler translated by an old
   --  compiler.  It does nothing.

   procedure End_Handler (GCC_Exception : GCC_Exception_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");
   --  Called when leaving an exception handler translated by an old
   --  compiler.  It releases GCC_Exception, unless it is null.  It is
   --  only ever null when the handler has a 'raise;' translated by a
   --  v0-using compiler.  The artificial handler variable passed to
   --  End_Handler was set to null to tell End_Handler to refrain from
   --  releasing the reraised exception.  In v1 safer ways are used to
   --  accomplish that.

   --------------------------------------------------------------------
   -- Accessors to Basic Components of a GNAT Exception Data Pointer --
   --------------------------------------------------------------------

   --  As of today, these are only used by the C implementation of the GCC
   --  propagation personality routine to avoid having to rely on a C
   --  counterpart of the whole exception_data structure, which is both
   --  painful and error prone. These subprograms could be moved to a more
   --  widely visible location if need be.

   function Is_Handled_By_Others (E : Exception_Data_Ptr) return bool;
   pragma Export (C, Is_Handled_By_Others, "__gnat_is_handled_by_others");

   function Language_For (E : Exception_Data_Ptr) return Character;
   pragma Export (C, Language_For, "__gnat_language_for");

   function Foreign_Data_For (E : Exception_Data_Ptr) return Address;
   pragma Export (C, Foreign_Data_For, "__gnat_foreign_data_for");

   function EID_For (GNAT_Exception : not null GNAT_GCC_Exception_Access)
     return Exception_Id;
   pragma Export (C, EID_For, "__gnat_eid_for");

   ---------------------------------------------------------------------------
   -- Objects to materialize "others" and "all others" in the GCC EH tables --
   ---------------------------------------------------------------------------

   --  Currently, these only have their address taken and compared so there is
   --  no real point having whole exception data blocks allocated. Note that
   --  there are corresponding declarations in gigi (trans.c) which must be
   --  kept properly synchronized.

   Others_Value : constant Character := 'O';
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : constant Character := 'A';
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   Unhandled_Others_Value : constant Character := 'U';
   pragma Export (C, Unhandled_Others_Value, "__gnat_unhandled_others_value");
   --  Special choice (emitted by gigi) to catch and notify unhandled
   --  exceptions on targets which always handle exceptions (such as SEH).
   --  The handler will simply call Unhandled_Except_Handler.

   -------------------------
   -- Allocate_Occurrence --
   -------------------------

   function Allocate_Occurrence return EOA is
      Res : GNAT_GCC_Exception_Access;

   begin
      Res := New_Occurrence;
      Res.Header.Cleanup := GNAT_GCC_Exception_Cleanup'Address;
      Res.Occurrence.Machine_Occurrence := Res.all'Address;

      return Res.Occurrence'Access;
   end Allocate_Occurrence;

   --------------------------------
   -- GNAT_GCC_Exception_Cleanup --
   --------------------------------

   procedure GNAT_GCC_Exception_Cleanup
     (Reason : Unwind_Reason_Code;
      Excep  : not null GNAT_GCC_Exception_Access)
   is
      pragma Unreferenced (Reason);

      procedure Free is new Unchecked_Deallocation
        (GNAT_GCC_Exception, GNAT_GCC_Exception_Access);

      Copy : GNAT_GCC_Exception_Access := Excep;

   begin
      --  Simply free the memory

      Free (Copy);
   end GNAT_GCC_Exception_Cleanup;

   ----------------------------
   -- Set_Foreign_Occurrence --
   ----------------------------

   procedure Set_Foreign_Occurrence
     (Excep : EOA;
      Mo    : System.Address;
      Id    : Exception_Id := Foreign_Exception'Access) is
   begin
      Excep.all := (
        Id                 => Id,
        Machine_Occurrence => Mo,
        Msg                => <>,
        Msg_Length         => 0,
        Exception_Raised   => True,
        Pid                => Local_Partition_ID,
        Num_Tracebacks     => 0,
        Tracebacks         => <>);
   end Set_Foreign_Occurrence;

   -------------------------
   -- Setup_Current_Excep --
   -------------------------

   function Setup_Current_Excep
     (GCC_Exception : not null GCC_Exception_Access;
      Phase : Unwind_Action;
      Id : Exception_Id) return EOA
   is
      Excep : constant EOA := Get_Current_Excep.all;

   begin

      if GCC_Exception.Class = GNAT_Exception_Class then

         --  Ada exception : latch the occurrence data in the Current
         --  Exception Buffer if needed and return a pointer to the original
         --  Ada exception object. This particular object was specifically
         --  allocated for this raise and is thus more precise than the fixed
         --  Current Exception Buffer address.

         declare
            GNAT_Occurrence : constant GNAT_GCC_Exception_Access :=
                                To_GNAT_GCC_Exception (GCC_Exception);
         begin

            --  When reaching here during SEARCH_PHASE, no need to
            --  replicate the copy performed at the propagation start.

            if Phase /= UA_SEARCH_PHASE then
               Excep.all := GNAT_Occurrence.Occurrence;
            end if;
            return GNAT_Occurrence.Occurrence'Access;
         end;

      else

         --  Foreign exception (caught by Ada handler, reaching here from
         --  personality routine) : The original exception object doesn't hold
         --  an Ada occurrence info.  Set the foreign data pointer in the
         --  Current Exception Buffer and return the address of the latter.

         if Id = null then
            Set_Foreign_Occurrence (Excep, GCC_Exception.all'Address);
         else
            Set_Foreign_Occurrence (Excep, GCC_Exception.all'Address, Id);
         end if;

         return Excep;
      end if;
   end Setup_Current_Excep;

   ----------------------
   -- Begin_Handler_v1 --
   ----------------------

   function Begin_Handler_v1
     (GCC_Exception : not null GCC_Exception_Access)
     return System.Address is
      Saved_Cleanup : constant System.Address := GCC_Exception.Cleanup;
   begin
      --  Claim responsibility for releasing this exception, and stop
      --  others from releasing it.
      GCC_Exception.Cleanup := Claimed_Cleanup'Address;
      return Saved_Cleanup;
   end Begin_Handler_v1;

   --------------------
   -- End_Handler_v1 --
   --------------------

   procedure End_Handler_v1
     (GCC_Exception : not null GCC_Exception_Access;
      Saved_Cleanup : System.Address;
      Propagating_Exception : GCC_Exception_Access) is
   begin
      GCC_Exception.Cleanup := Saved_Cleanup;
      --  Restore the Saved_Cleanup, so that it is either used to
      --  release GCC_Exception below, or transferred to the next
      --  handler of the Propagating_Exception occurrence.  The
      --  following test ensures that an occurrence is only released
      --  once, even after reraises.
      --
      --  The idea is that the GCC_Exception is not to be released
      --  unless it had an unclaimed Cleanup when the handler started
      --  (see Begin_Handler_v1 above), but if we propagate across its
      --  handler a reraise of the same exception, we transfer to the
      --  Propagating_Exception the responsibility for running the
      --  Saved_Cleanup when its handler completes.
      --
      --  This ownership transfer mechanism ensures safety, as in
      --  single release and no dangling pointers, because there is no
      --  way to hold on to the Machine_Occurrence of an
      --  Exception_Occurrence: the only situations in which another
      --  Exception_Occurrence gets the same Machine_Occurrence are
      --  through Reraise_Occurrence, and plain reraise, and so we
      --  have the following possibilities:
      --
      --  - Reraise_Occurrence is handled within the running handler,
      --  and so when completing the dynamically nested handler, we
      --  must NOT release the exception.  A Claimed_Cleanup upon
      --  entry of the nested handler, installed when entering the
      --  enclosing handler, ensures the exception will not be
      --  released by the nested handler, but rather by the enclosing
      --  handler.
      --
      --  - Reraise_Occurrence/reraise escapes the running handler,
      --  and we run as an exceptional cleanup for GCC_Exception.  The
      --  Saved_Cleanup was reinstalled, but since we're propagating
      --  the same machine occurrence, we do not release it.  Instead,
      --  we transfer responsibility for releasing it to the eventual
      --  handler of the propagating exception.
      --
      --  - An unrelated exception propagates through the running
      --  handler.  We restored GCC_Exception.Saved_Cleanup above.
      --  Since we're propagating a different exception, we proceed to
      --  release GCC_Exception, unless Saved_Cleanup was
      --  Claimed_Cleanup, because then we know we're not in the
      --  outermost handler for GCC_Exception.
      --
      --  - The handler completes normally, so it reinstalls the
      --  Saved_Cleanup and runs it, unless it was Claimed_Cleanup.
      --  If Saved_Cleanup is null, Unwind_DeleteException (currently)
      --  has no effect, so we could skip it, but if it is ever
      --  changed to do more in this case, we're ready for that,
      --  calling it exactly once.
      if Saved_Cleanup /= Claimed_Cleanup'Address
        and then
        Propagating_Exception /= GCC_Exception
      then
         declare
            Current : constant EOA := Get_Current_Excep.all;
            Cur_Occ : constant GCC_Exception_Access :=
              To_GCC_Exception (Current.Machine_Occurrence);
         begin
            --  If we are releasing the Machine_Occurrence of the current
            --  exception, reset the access to it, so that it is no
            --  longer accessible.
            if Cur_Occ = GCC_Exception then
               Current.Machine_Occurrence := System.Null_Address;
            end if;
         end;
         Unwind_DeleteException (GCC_Exception);
      end if;
   end End_Handler_v1;

   ---------------------
   -- Claimed_Cleanup --
   ---------------------

   procedure Claimed_Cleanup
     (Reason : Unwind_Reason_Code;
      GCC_Exception : not null GCC_Exception_Access) is
      pragma Unreferenced (Reason);
      pragma Unreferenced (GCC_Exception);
   begin
      --  This procedure should never run.  If it does, it's either a
      --  version 0 handler or a foreign handler, attempting to
      --  release an exception while a version 1 handler that claimed
      --  responsibility for releasing the exception remains still
      --  active.  This placeholder stops GCC_Exception from being
      --  released by them.

      --  We could get away with just Null_Address instead, with
      --  nearly the same effect, but with this placeholder we can
      --  detect and report unexpected releases, and we can tell apart
      --  a GCC_Exception without a Cleanup, from one with another
      --  active handler, so as to still call Unwind_DeleteException
      --  exactly once: currently, Unwind_DeleteException does nothing
      --  when the Cleanup is null, but should it ever be changed to
      --  do more, we'll still be safe.
      null;
   end Claimed_Cleanup;

   -------------------
   -- Begin_Handler --
   -------------------

   procedure Begin_Handler (GCC_Exception : not null GCC_Exception_Access) is
      pragma Unreferenced (GCC_Exception);
   begin
      null;
   end Begin_Handler;

   -----------------
   -- End_Handler --
   -----------------

   procedure End_Handler (GCC_Exception : GCC_Exception_Access) is
   begin
      if GCC_Exception /= null then

         --  The exception might have been reraised, in this case the cleanup
         --  mustn't be called.

         Unwind_DeleteException (GCC_Exception);
      end if;
   end End_Handler;

   -----------------------------
   -- Reraise_GCC_Exception --
   -----------------------------

   procedure Reraise_GCC_Exception
     (GCC_Exception : not null GCC_Exception_Access)
   is
   begin
      --  Simply propagate it

      Propagate_GCC_Exception (GCC_Exception);
   end Reraise_GCC_Exception;

   -----------------------------
   -- Propagate_GCC_Exception --
   -----------------------------

   --  Call Unwind_RaiseException to actually throw, taking care of handling
   --  the two phase scheme it implements.

   procedure Propagate_GCC_Exception
     (GCC_Exception : not null GCC_Exception_Access)
   is
      --  Acknowledge the current exception info now, before unwinding
      --  starts so it is available even from C++ handlers involved before
      --  our personality routine.

      Excep : constant EOA :=
        Setup_Current_Excep (GCC_Exception, Phase => 0, Id => null);

   begin
      --  Perform a standard raise first. If a regular handler is found, it
      --  will be entered after all the intermediate cleanups have run. If
      --  there is no regular handler, it will return.

      Unwind_RaiseException (GCC_Exception);

      --  If we get here we know the exception is not handled, as otherwise
      --  Unwind_RaiseException arranges for the handler to be entered. Take
      --  the necessary steps to enable the debugger to gain control while the
      --  stack is still intact.

      Notify_Unhandled_Exception (Excep);

      --  Now, un a forced unwind to trigger cleanups. Control should not
      --  resume there, if there are cleanups and in any cases as the
      --  unwinding hook calls Unhandled_Exception_Terminate when end of
      --  stack is reached.

      Unwind_ForcedUnwind
        (GCC_Exception,
         CleanupUnwind_Handler'Address,
         System.Null_Address);

      --  We get here in case of error. The debugger has been notified before
      --  the second step above.

      Unhandled_Except_Handler (GCC_Exception);
   end Propagate_GCC_Exception;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception (Excep : Exception_Occurrence) is
   begin
      Propagate_GCC_Exception (To_GCC_Exception (Excep.Machine_Occurrence));
   end Propagate_Exception;

   -----------------------------
   -- Set_Exception_Parameter --
   -----------------------------

   procedure Set_Exception_Parameter
     (Excep         : EOA;
      GCC_Exception : not null GCC_Exception_Access)
   is
   begin
      --  Setup the exception occurrence

      if GCC_Exception.Class = GNAT_Exception_Class then

         --  From the GCC exception

         declare
            GNAT_Occurrence : constant GNAT_GCC_Exception_Access :=
                                To_GNAT_GCC_Exception (GCC_Exception);
         begin
            Save_Occurrence (Excep.all, GNAT_Occurrence.Occurrence);
         end;

      else
         --  A default one.  Take the Id from the exception object
         --  created by Setup_Current_Excep.

         Set_Foreign_Occurrence (Excep, GCC_Exception.all'Address,
                                 Get_Current_Excep.all.Id);

      end if;
   end Set_Exception_Parameter;

   ------------------------------
   -- Unhandled_Except_Handler --
   ------------------------------

   procedure Unhandled_Except_Handler
     (GCC_Exception : not null GCC_Exception_Access)
   is
      Excep : EOA;
   begin
      Excep := Setup_Current_Excep (GCC_Exception, Phase => UA_CLEANUP_PHASE,
                                    Id => null);
      Unhandled_Exception_Terminate (Excep);
   end Unhandled_Except_Handler;

   -------------
   -- EID_For --
   -------------

   function EID_For
     (GNAT_Exception : not null GNAT_GCC_Exception_Access) return Exception_Id
   is
   begin
      return GNAT_Exception.Occurrence.Id;
   end EID_For;

   ----------------------
   -- Foreign_Data_For --
   ----------------------

   function Foreign_Data_For (E : Exception_Data_Ptr) return Address is
   begin
      return E.Foreign_Data;
   end Foreign_Data_For;

   --------------------------
   -- Is_Handled_By_Others --
   --------------------------

   function Is_Handled_By_Others (E : Exception_Data_Ptr) return bool is
   begin
      return not bool (E.all.Not_Handled_By_Others);
   end Is_Handled_By_Others;

   ------------------
   -- Language_For --
   ------------------

   function Language_For (E : Exception_Data_Ptr) return Character is
   begin
      return E.all.Lang;
   end Language_For;

end Exception_Propagation;
