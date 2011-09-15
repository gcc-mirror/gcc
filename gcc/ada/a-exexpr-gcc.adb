------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  A D A . E X C E P T I O N S . E X C E P T I O N _ P R O P A G A T I O N --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

   type Unwind_Action is new Integer;
   pragma Convention (C, Unwind_Action);

   UA_SEARCH_PHASE  : constant Unwind_Action := 1;
   UA_CLEANUP_PHASE : constant Unwind_Action := 2;
   UA_HANDLER_FRAME : constant Unwind_Action := 4;
   UA_FORCE_UNWIND  : constant Unwind_Action := 8;
   UA_END_OF_STACK  : constant Unwind_Action := 16;  --  GCC extension ?

   pragma Unreferenced
     (UA_SEARCH_PHASE,
      UA_CLEANUP_PHASE,
      UA_HANDLER_FRAME,
      UA_FORCE_UNWIND);

   --  Mandatory common header for any exception object handled by the
   --  GCC unwinding runtime.

   type Exception_Class is mod 2 ** 64;

   GNAT_Exception_Class : constant Exception_Class := 16#474e552d41646100#;
   --  "GNU-Ada\0"

   type Unwind_Word is mod 2 ** System.Word_Size;
   for Unwind_Word'Size use System.Word_Size;
   --  Map the corresponding C type used in Unwind_Exception below

   type Unwind_Exception is record
      Class    : Exception_Class;
      Cleanup  : System.Address;
      Private1 : Unwind_Word;
      Private2 : Unwind_Word;
   end record;
   pragma Convention (C, Unwind_Exception);
   --  Map the GCC struct used for exception handling

   for Unwind_Exception'Alignment use Standard'Maximum_Alignment;
   --  The C++ ABI mandates the common exception header to be at least
   --  doubleword aligned, and the libGCC implementation actually makes it
   --  maximally aligned (see unwind.h). See additional comments on the
   --  alignment below.

   type GCC_Exception_Access is access all Unwind_Exception;
   --  Pointer to a GCC exception. Do not use convention C as on VMS this
   --  would imply the use of 32-bits pointers.

   procedure Unwind_DeleteException (Excp : not null GCC_Exception_Access);
   pragma Import (C, Unwind_DeleteException, "_Unwind_DeleteException");
   --  Procedure to free any GCC exception

   Foreign_Exception : aliased System.Standard_Library.Exception_Data;
   pragma Import (Ada, Foreign_Exception,
                  "system__exceptions__foreign_exception");
   --  Id for foreign exceptions

   --------------------------------------------------------------
   -- GNAT Specific Entities To Deal With The GCC EH Circuitry --
   --------------------------------------------------------------

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime.

   type GNAT_GCC_Exception is record
      Header : Unwind_Exception;
      --  ABI Exception header first

      Occurrence : Exception_Occurrence;
      --  The Ada occurrence
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

   function To_GCC_Exception is new
     Unchecked_Conversion (GNAT_GCC_Exception_Access, GCC_Exception_Access);

   function To_GNAT_GCC_Exception is new
     Unchecked_Conversion (GCC_Exception_Access, GNAT_GCC_Exception_Access);

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
   --  Called to implement raise without exception, ie reraise.  Called
   --  directly from gigi.

   procedure Setup_Current_Excep
     (GCC_Exception : not null GCC_Exception_Access);
   pragma Export (C, Setup_Current_Excep, "__gnat_setup_current_excep");
   --  Write Get_Current_Excep.all from GCC_Exception

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null GCC_Exception_Access;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code;
   --  Hook called at each step of the forced unwinding we perform to
   --  trigger cleanups found during the propagation of an unhandled
   --  exception.

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

   --  Hooks called when entering/leaving an exception handler for a given
   --  occurrence, aimed at handling the stack of active occurrences. The
   --  calls are generated by gigi in tree_transform/N_Exception_Handler.

   procedure Begin_Handler (GCC_Exception : not null GCC_Exception_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   procedure End_Handler (GCC_Exception : GCC_Exception_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

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

   function EID_For (GNAT_Exception : not null GNAT_GCC_Exception_Access)
     return Exception_Id;
   pragma Export (C, EID_For, "__gnat_eid_for");

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

   ---------------------------
   -- CleanupUnwind_Handler --
   ---------------------------

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null GCC_Exception_Access;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code
   is
      pragma Unreferenced (UW_Version, UW_Eclass, UW_Context, UW_Argument);

   begin
      --  Terminate when the end of the stack is reached

      if UW_Phases >= UA_END_OF_STACK then
         Setup_Current_Excep (UW_Exception);
         Unhandled_Exception_Terminate;
      end if;

      --  We know there is at least one cleanup further up. Return so that it
      --  is searched and entered, after which Unwind_Resume will be called
      --  and this hook will gain control again.

      return URC_NO_REASON;
   end CleanupUnwind_Handler;

   -------------------------
   -- Setup_Current_Excep --
   -------------------------

   procedure Setup_Current_Excep
     (GCC_Exception : not null GCC_Exception_Access)
   is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      --  Setup the exception occurrence

      if GCC_Exception.Class = GNAT_Exception_Class then

         --  From the GCC exception

         declare
            GNAT_Occurrence : constant GNAT_GCC_Exception_Access :=
                                To_GNAT_GCC_Exception (GCC_Exception);
         begin
            Excep.all := GNAT_Occurrence.Occurrence;
         end;
      else

         --  A default one

         Excep.Id := Foreign_Exception'Access;
         Excep.Msg_Length := 0;
         Excep.Exception_Raised := True;
         Excep.Pid := Local_Partition_ID;
         Excep.Num_Tracebacks := 0;
      end if;
   end Setup_Current_Excep;

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
   begin
      --  Perform a standard raise first. If a regular handler is found, it
      --  will be entered after all the intermediate cleanups have run. If
      --  there is no regular handler, it will return.

      Unwind_RaiseException (GCC_Exception);

      --  If we get here we know the exception is not handled, as otherwise
      --  Unwind_RaiseException arranges for the handler to be entered. Take
      --  the necessary steps to enable the debugger to gain control while the
      --  stack is still intact.

      Setup_Current_Excep (GCC_Exception);
      Notify_Unhandled_Exception;

      --  Now, un a forced unwind to trigger cleanups. Control should not
      --  resume there, if there are cleanups and in any cases as the
      --  unwinding hook calls Unhandled_Exception_Terminate when end of
      --  stack is reached.

      Unwind_ForcedUnwind (GCC_Exception,
                           CleanupUnwind_Handler'Address,
                           System.Null_Address);

      --  We get here in case of error. The debugger has been notified before
      --  the second step above.

      Setup_Current_Excep (GCC_Exception);
      Unhandled_Exception_Terminate;
   end Propagate_GCC_Exception;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   --  Build an object suitable for the libgcc processing and call
   --  Unwind_RaiseException to actually do the raise, taking care of
   --  handling the two phase scheme it implements.

   procedure Propagate_Exception is
      Excep         : constant EOA := Get_Current_Excep.all;
      GCC_Exception : GNAT_GCC_Exception_Access;

   begin
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

      --  Allocate the GCC exception

      GCC_Exception :=
        new GNAT_GCC_Exception'
          (Header     => (Class => GNAT_Exception_Class,
                          Cleanup => GNAT_GCC_Exception_Cleanup'Address,
                          Private1 => 0,
                          Private2 => 0),
           Occurrence => Excep.all);

      --  Propagate it

      Propagate_GCC_Exception (To_GCC_Exception (GCC_Exception));
   end Propagate_Exception;

   -------------
   -- EID_For --
   -------------

   function EID_For
     (GNAT_Exception : not null GNAT_GCC_Exception_Access) return Exception_Id
   is
   begin
      return GNAT_Exception.Occurrence.Id;
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

end Exception_Propagation;
