------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with System.Exception_Tables.

with Ada.Unchecked_Deallocation;

with GNAT.Heap_Sort_A;        use GNAT.Heap_Sort_A;

with System;                  use System;
with System.Exception_Table;  use System.Exception_Table;
with System.Exceptions;       use System.Exceptions;
with System.Standard_Library; use System.Standard_Library;
with System.Storage_Elements; use System.Storage_Elements;
with System.Soft_Links;       use System.Soft_Links;
with System.Machine_State_Operations; use System.Machine_State_Operations;
with System.Traceback;

with Unchecked_Conversion;

package body Ada.Exceptions is

   procedure builtin_longjmp (buffer : Address; Flag : Integer);
   pragma No_Return (builtin_longjmp);
   pragma Import (C, builtin_longjmp, "_gnat_builtin_longjmp");

   pragma Suppress (All_Checks);
   --  We definitely do not want exceptions occurring within this unit, or
   --  we are in big trouble. If an exceptional situation does occur, better
   --  that it not be raised, since raising it can cause confusing chaos.

   type Subprogram_Descriptor_List_Ptr is
     access all Subprogram_Descriptor_List;

   Subprogram_Descriptors : Subprogram_Descriptor_List_Ptr;
   --  This location is initialized by Register_Exceptions to point to a
   --  list of pointers to procedure descriptors, sorted into ascending
   --  order of PC addresses.
   --
   --  Note that SDP_Table_Build is called *before* this unit (or any
   --  other unit) is elaborated. That's important, because exceptions can
   --  and do occur during elaboration of units, and must be handled during
   --  elaboration. This means that we are counting on the fact that the
   --  initialization of Subprogram_Descriptors to null is done by the
   --  load process and NOT by an explicit assignment during elaboration.

   Num_Subprogram_Descriptors : Natural;
   --  Number of subprogram descriptors, the useful descriptors are stored
   --  in Subprogram_Descriptors (1 .. Num_Subprogram_Descriptors). There
   --  can be unused entries at the end of the array due to elimination of
   --  duplicated entries (which can arise from use of pragma Import).

   Exception_Tracebacks : Integer;
   pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
   --  Boolean indicating whether tracebacks should be stored in exception
   --  occurrences.

   Zero_Cost_Exceptions : Integer;
   pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
   --  Boolean indicating if we are handling exceptions using a zero cost
   --  mechanism.
   --
   --  ??? We currently have two alternatives for this scheme : one using
   --  front-end tables and one using back-end tables. The former is known to
   --  only work for GNAT3 and the latter is known to only work for GNAT5.
   --  Both are present in this implementation and it would be good to have
   --  separate bodies at some point.
   --
   --  Note that although we currently do not support it, the GCC3 back-end
   --  tables are also potentially useable for setjmp/longjmp processing.

   Nline : constant String := String' (1 => ASCII.LF);
   --  Convenient shortcut

   ------------------------------------------------
   -- Entities to interface with the GCC runtime --
   ------------------------------------------------

   --  These come from "C++ ABI for Itanium : Exception handling", which is
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

   --  ??? pragma Unreferenced is unknown until 3.15, so we need to disable
   --  warnings around it to fix the bootstrap path.

   pragma Warnings (Off);
   pragma Unreferenced
     (URC_NO_REASON,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_PHASE2_ERROR,
      URC_PHASE1_ERROR,
      URC_NORMAL_STOP,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND);
   pragma Warnings (On);

   pragma Convention (C, Unwind_Reason_Code);

   --  Mandatory common header for any exception object handled by the
   --  GCC unwinding runtime.

   subtype Exception_Class is String (1 .. 8);

   GNAT_Exception_Class : constant Exception_Class
     := "GNU" & ASCII.NUL & "Ada" & ASCII.NUL;

   type Unwind_Exception is record
      Class    : Exception_Class := GNAT_Exception_Class;
      Cleanup  : System.Address  := System.Null_Address;
      Private1 : Integer;
      Private2 : Integer;
   end record;

   pragma Convention (C, Unwind_Exception);

   for Unwind_Exception'Alignment use Standard'Maximum_Alignment;

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime. This structure shall match the
   --  one in raise.c and is currently experimental as it might be merged
   --  with the GNAT runtime definition some day.

   type GNAT_GCC_Exception is record
      Header : Unwind_Exception;
      --  Exception header first, as required by the ABI.

      Id : Exception_Id;
      --  Usual Exception identifier

      Handled_By_Others : Boolean;
      --  Is this exception handled by "when others" ?

      Has_Cleanup : Boolean;
      --  Did we see any at-end handler while walking up the stack
      --  searching for a handler ? This is used to determine if we
      --  start the propagation again after having tried once without
      --  finding a true handler for the exception.

      Select_Cleanups : Boolean;
      --  Do we consider at-end handlers as legitimate handlers for the
      --  exception ? This is used to control the propagation process
      --  as described in Raise_Current_Excep.
   end record;

   pragma Convention (C, GNAT_GCC_Exception);

   --  GCC runtime functions used

   function Unwind_RaiseException
     (E    : access GNAT_GCC_Exception)
      return Unwind_Reason_Code;
   pragma Import (C, Unwind_RaiseException, "__gnat_Unwind_RaiseException");

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Note: the exported subprograms in this package body are called directly
   --  from C clients using the given external name, even though they are not
   --  technically visible in the Ada sense.

   procedure AAA;
   --  Mark start of procedures in this unit

   procedure ZZZ;
   --  Mark end of procedures in this package

   function Address_Image (A : System.Address) return String;
   --  Returns at string of the form 0xhhhhhhhhh for 32-bit addresses
   --  or 0xhhhhhhhhhhhhhhhh for 64-bit addresses. Hex characters are
   --  in lower case.

   procedure Call_Chain (Excep : EOA);
   --  Store up to Max_Tracebacks in Excep, corresponding to the current
   --  call chain.

   procedure Free
     is new Ada.Unchecked_Deallocation
       (Subprogram_Descriptor_List, Subprogram_Descriptor_List_Ptr);

   procedure Process_Raise_Exception
     (E                   : Exception_Id;
      From_Signal_Handler : Boolean);
   pragma Inline (Process_Raise_Exception);
   pragma No_Return (Process_Raise_Exception);
   --  This is the lowest level raise routine. It raises the exception
   --  referenced by Current_Excep.all in the TSD, without deferring abort
   --  (the caller must ensure that abort is deferred on entry).
   --
   --  This is actually the common implementation for Raise_Current_Excep and
   --  Raise_From_Signal_Handler, with a couple of operations inhibited when
   --  called from the latter. The origin of the call is indicated by the
   --  From_Signal_Handler argument.
   --
   --  The Inline pragma is there for efficiency reasons.

   procedure Propagate_Exception_With_FE_Support (Mstate : Machine_State);
   pragma No_Return (Propagate_Exception_With_FE_Support);
   --  This procedure propagates the exception represented by the occurrence
   --  referenced by Current_Excep in the TSD for the current task. M is the
   --  initial machine state, representing the site of the exception raise
   --  operation.
   --
   --  The procedure searches the front end exception tables for an applicable
   --  handler, calling Pop_Frame as needed. If and when it locates an
   --  applicable handler, Enter_Handler is called to actually enter this
   --  handler. If the search is unable to locate an applicable handler,
   --  execution is terminated by calling Unhandled_Exception_Terminate.

   procedure Propagate_Exception_With_GCC_Support (Mstate : Machine_State);
   pragma No_Return (Propagate_Exception_With_GCC_Support);
   --  This procedure propagates the exception represented by the occurrence
   --  referenced by Current_Excep in the TSD for the current task. M is the
   --  initial machine state, representing the site of the exception raise
   --  operation. It is currently not used and is there for the purpose of
   --  interface consistency against Propagate_Exception_With_FE_Support.
   --
   --  The procedure builds an object suitable for the libgcc processing and
   --  calls Unwind_RaiseException to actually throw, taking care of handling
   --  the two phase scheme it implements.

   procedure Raise_Current_Excep (E : Exception_Id);
   pragma No_Return (Raise_Current_Excep);
   pragma Export (C, Raise_Current_Excep, "__gnat_raise_nodefer_with_msg");
   --  This is a simple wrapper to Process_Raise_Exception setting the
   --  From_Signal_Handler argument to False.
   --
   --  This external name for Raise_Current_Excep is historical, and probably
   --  should be changed but for now we keep it, because gdb and gigi know
   --  about it.

   procedure Raise_Exception_No_Defer
      (E : Exception_Id; Message : String := "");
   pragma Export (Ada, Raise_Exception_No_Defer,
     "ada__exceptions__raise_exception_no_defer");
   pragma No_Return (Raise_Exception_No_Defer);
   --  Similar to Raise_Exception, but with no abort deferral

   procedure Raise_With_Msg (E : Exception_Id);
   pragma No_Return (Raise_With_Msg);
   pragma Export (C, Raise_With_Msg, "__gnat_raise_with_msg");
   --  Raises an exception with given exception id value. A message
   --  is associated with the raise, and has already been stored in the
   --  exception occurrence referenced by the Current_Excep in the TSD.
   --  Abort is deferred before the raise call.

   procedure Raise_With_Location
     (E : Exception_Id;
      F : Big_String_Ptr;
      L : Integer);
   pragma No_Return (Raise_With_Location);
   --  Raise an exception with given exception id value. A filename and line
   --  number is associated with the raise and is stored in the exception
   --  occurrence.

   procedure Raise_With_Location_And_Msg
     (E : Exception_Id;
      F : Big_String_Ptr;
      L : Integer;
      M : Big_String_Ptr);
   pragma No_Return (Raise_With_Location_And_Msg);
   --  Raise an exception with given exception id value. A filename and line
   --  number is associated with the raise and is stored in the exception
   --  occurrence and in addition a string message M is appended to this.

   procedure Raise_Constraint_Error
     (File : Big_String_Ptr;
      Line : Integer);
   pragma No_Return (Raise_Constraint_Error);
   pragma Export
     (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error with file:line information

   procedure Raise_Constraint_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr);
   pragma No_Return (Raise_Constraint_Error_Msg);
   pragma Export
     (C, Raise_Constraint_Error_Msg, "__gnat_raise_constraint_error_msg");
   --  Raise constraint error with file:line + msg information

   procedure Raise_Program_Error
     (File : Big_String_Ptr;
      Line : Integer);
   pragma No_Return (Raise_Program_Error);
   pragma Export
     (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise program error with file:line information

   procedure Raise_Program_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr);
   pragma No_Return (Raise_Program_Error_Msg);
   pragma Export
     (C, Raise_Program_Error_Msg, "__gnat_raise_program_error_msg");
   --  Raise program error with file:line + msg information

   procedure Raise_Storage_Error
     (File : Big_String_Ptr;
      Line : Integer);
   pragma No_Return (Raise_Storage_Error);
   pragma Export
     (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error with file:line information

   procedure Raise_Storage_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr);
   pragma No_Return (Raise_Storage_Error_Msg);
   pragma Export
     (C, Raise_Storage_Error_Msg, "__gnat_raise_storage_error_msg");
   --  Raise storage error with file:line + reason msg information

   --  The exception raising process and the automatic tracing mechanism rely
   --  on some careful use of flags attached to the exception occurrence. The
   --  graph below illustrates the relations between the Raise_ subprograms
   --  and identifies the points where basic flags such as Exception_Raised
   --  are initialized.
   --
   --  (i) signs indicate the flags initialization points. R stands for Raise,
   --  W for With, and E for Exception.
   --
   --                   R_No_Msg    R_E   R_Pe  R_Ce  R_Se
   --                       |        |     |     |     |
   --                       +--+  +--+     +---+ | +---+
   --                          |  |            | | |
   --     R_E_No_Defer(i)    R_W_Msg(i)       R_W_Loc      R_W_C_Msg
   --           |               |              |   |        |    |
   --           +------------+  |  +-----------+   +--+  +--+    |
   --                        |  |  |                  |  |       |
   --                        |  |  |              Set_E_C_Msg(i) |
   --                        |  |  |                             |
   --                        |  |  |  +--------------------------+
   --                        |  |  |  |
   --                   Raise_Current_Excep

   procedure Reraise;
   pragma No_Return (Reraise);
   pragma Export (C, Reraise, "__gnat_reraise");
   --  Reraises the exception referenced by the Current_Excep field of
   --  the TSD (all fields of this exception occurrence are set). Abort
   --  is deferred before the reraise operation.

   function SDP_Table_Sort_Lt (Op1, Op2 : Natural) return Boolean;
   --  Used in call to sort SDP table (SDP_Table_Build), compares two elements

   procedure SDP_Table_Sort_Move (From : Natural; To : Natural);
   --  Used in call to sort SDP table (SDP_Table_Build), moves one element

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg1 : Big_String_Ptr;
      Line : Integer        := 0;
      Msg2 : Big_String_Ptr := null);
   --  This routine is called to setup the exception referenced by the
   --  Current_Excep field in the TSD to contain the indicated Id value
   --  and message. Msg1 is a null terminated string which is generated
   --  as the exception message. If line is non-zero, then a colon and
   --  the decimal representation of this integer is appended to the
   --  message. When Msg2 is non-null, a space and this additional null
   --  terminated string is added to the message.

   procedure To_Stderr (S : String);
   pragma Export (Ada, To_Stderr, "__gnat_to_stderr");
   --  Little routine to output string to stderr that is also used
   --  in the tasking run time.

   procedure Unhandled_Exception_Terminate;
   pragma No_Return (Unhandled_Exception_Terminate);
   --  This procedure is called to terminate execution following an unhandled
   --  exception. The exception information, including traceback if available
   --  is output, and execution is then terminated. Note that at the point
   --  where this routine is called, the stack has typically been destroyed

   ---------------------------------
   -- Debugger Interface Routines --
   ---------------------------------

   --  The routines here are null routines that normally have no effect.
   --  they are provided for the debugger to place breakpoints on their
   --  entry points to get control on an exception.

   procedure Notify_Exception
     (Id        : Exception_Id;
      Handler   : Code_Loc;
      Is_Others : Boolean);
   pragma Export (C, Notify_Exception, "__gnat_notify_exception");
   --  This routine is called whenever an exception is signalled. The Id
   --  parameter is the Exception_Id of the exception being raised. The
   --  second parameter Handler is Null_Loc if the exception is unhandled,
   --  and is otherwise the entry point of the handler that will handle
   --  the exception. Is_Others is True if the handler is an others handler
   --  and False otherwise. In the unhandled exception case, if possible
   --  (and certainly if zero cost exception handling is active), the
   --  stack is still intact when this procedure is called. Note that this
   --  routine is entered before any finalization handlers are entered if
   --  the exception is unhandled by a "real" exception handler.

   procedure Unhandled_Exception;
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  This routine is called in addition to Notify_Exception in the
   --  unhandled exception case. The fact that there are two routines
   --  which are somewhat redundant is historical. Notify_Exception
   --  certainly is complete enough, but GDB still uses this routine.

   -----------------------------
   -- Run-Time Check Routines --
   -----------------------------

   --  These routines are called from the runtime to raise a specific
   --  exception with a reason message attached. The parameters are
   --  the file name and line number in each case. The names are keyed
   --  to the codes defined in Types.ads and a-types.h (for example,
   --  the name Rcheck_05 refers to the Reason whose Pos code is 5).

   procedure Rcheck_00 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_01 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_02 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_03 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_04 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_05 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_06 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_07 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_08 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_09 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_10 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_11 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_12 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_13 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_14 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_15 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_16 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_17 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_18 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_19 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_20 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_21 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_22 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_23 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_24 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_25 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_26 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_27 (File : Big_String_Ptr; Line : Integer);
   procedure Rcheck_28 (File : Big_String_Ptr; Line : Integer);

   pragma Export (C, Rcheck_00, "__gnat_rcheck_00");
   pragma Export (C, Rcheck_01, "__gnat_rcheck_01");
   pragma Export (C, Rcheck_02, "__gnat_rcheck_02");
   pragma Export (C, Rcheck_03, "__gnat_rcheck_03");
   pragma Export (C, Rcheck_04, "__gnat_rcheck_04");
   pragma Export (C, Rcheck_05, "__gnat_rcheck_05");
   pragma Export (C, Rcheck_06, "__gnat_rcheck_06");
   pragma Export (C, Rcheck_07, "__gnat_rcheck_07");
   pragma Export (C, Rcheck_08, "__gnat_rcheck_08");
   pragma Export (C, Rcheck_09, "__gnat_rcheck_09");
   pragma Export (C, Rcheck_10, "__gnat_rcheck_10");
   pragma Export (C, Rcheck_11, "__gnat_rcheck_11");
   pragma Export (C, Rcheck_12, "__gnat_rcheck_12");
   pragma Export (C, Rcheck_13, "__gnat_rcheck_13");
   pragma Export (C, Rcheck_14, "__gnat_rcheck_14");
   pragma Export (C, Rcheck_15, "__gnat_rcheck_15");
   pragma Export (C, Rcheck_16, "__gnat_rcheck_16");
   pragma Export (C, Rcheck_17, "__gnat_rcheck_17");
   pragma Export (C, Rcheck_18, "__gnat_rcheck_18");
   pragma Export (C, Rcheck_19, "__gnat_rcheck_19");
   pragma Export (C, Rcheck_20, "__gnat_rcheck_20");
   pragma Export (C, Rcheck_21, "__gnat_rcheck_21");
   pragma Export (C, Rcheck_22, "__gnat_rcheck_22");
   pragma Export (C, Rcheck_23, "__gnat_rcheck_23");
   pragma Export (C, Rcheck_24, "__gnat_rcheck_24");
   pragma Export (C, Rcheck_25, "__gnat_rcheck_25");
   pragma Export (C, Rcheck_26, "__gnat_rcheck_26");
   pragma Export (C, Rcheck_27, "__gnat_rcheck_27");
   pragma Export (C, Rcheck_28, "__gnat_rcheck_28");

   ---------------------------------------------
   -- Reason Strings for Run-Time Check Calls --
   ---------------------------------------------

   --  These strings are null-terminated and are used by Rcheck_nn. The
   --  strings correspond to the definitions for Types.RT_Exception_Code.

   use ASCII;

   Rmsg_00 : constant String := "access check failed"              & NUL;
   Rmsg_01 : constant String := "access parameter is null"         & NUL;
   Rmsg_02 : constant String := "discriminant check failed"        & NUL;
   Rmsg_03 : constant String := "divide by zero"                   & NUL;
   Rmsg_04 : constant String := "explicit raise"                   & NUL;
   Rmsg_05 : constant String := "index check failed"               & NUL;
   Rmsg_06 : constant String := "invalid data"                     & NUL;
   Rmsg_07 : constant String := "length check failed"              & NUL;
   Rmsg_08 : constant String := "overflow check failed"            & NUL;
   Rmsg_09 : constant String := "partition check failed"           & NUL;
   Rmsg_10 : constant String := "range check failed"               & NUL;
   Rmsg_11 : constant String := "tag check failed"                 & NUL;
   Rmsg_12 : constant String := "access before elaboration"        & NUL;
   Rmsg_13 : constant String := "accessibility check failed"       & NUL;
   Rmsg_14 : constant String := "all guards closed"                & NUL;
   Rmsg_15 : constant String := "duplicated entry address"         & NUL;
   Rmsg_16 : constant String := "explicit raise"                   & NUL;
   Rmsg_17 : constant String := "finalize raised exception"        & NUL;
   Rmsg_18 : constant String := "invalid data"                     & NUL;
   Rmsg_19 : constant String := "misaligned address value"         & NUL;
   Rmsg_20 : constant String := "missing return"                   & NUL;
   Rmsg_21 : constant String := "potentially blocking operation"   & NUL;
   Rmsg_22 : constant String := "stubbed subprogram called"        & NUL;
   Rmsg_23 : constant String := "unchecked union restriction"      & NUL;
   Rmsg_24 : constant String := "empty storage pool"               & NUL;
   Rmsg_25 : constant String := "explicit raise"                   & NUL;
   Rmsg_26 : constant String := "infinite recursion"               & NUL;
   Rmsg_27 : constant String := "object too large"                 & NUL;
   Rmsg_28 : constant String := "restriction violation"            & NUL;

   --------------------------------------
   -- Calls to Run-Time Check Routines --
   --------------------------------------

   procedure Rcheck_00 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_00'Address));
   end Rcheck_00;

   procedure Rcheck_01 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_01'Address));
   end Rcheck_01;

   procedure Rcheck_02 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_02'Address));
   end Rcheck_02;

   procedure Rcheck_03 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_03'Address));
   end Rcheck_03;

   procedure Rcheck_04 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_04'Address));
   end Rcheck_04;

   procedure Rcheck_05 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_05'Address));
   end Rcheck_05;

   procedure Rcheck_06 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_06'Address));
   end Rcheck_06;

   procedure Rcheck_07 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_07'Address));
   end Rcheck_07;

   procedure Rcheck_08 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_08'Address));
   end Rcheck_08;

   procedure Rcheck_09 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_09'Address));
   end Rcheck_09;

   procedure Rcheck_10 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_10'Address));
   end Rcheck_10;

   procedure Rcheck_11 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Constraint_Error_Msg (File, Line, To_Ptr (Rmsg_11'Address));
   end Rcheck_11;

   procedure Rcheck_12 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_12'Address));
   end Rcheck_12;

   procedure Rcheck_13 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_13'Address));
   end Rcheck_13;

   procedure Rcheck_14 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_14'Address));
   end Rcheck_14;

   procedure Rcheck_15 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_15'Address));
   end Rcheck_15;

   procedure Rcheck_16 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_16'Address));
   end Rcheck_16;

   procedure Rcheck_17 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_17'Address));
   end Rcheck_17;

   procedure Rcheck_18 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_18'Address));
   end Rcheck_18;

   procedure Rcheck_19 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_19'Address));
   end Rcheck_19;

   procedure Rcheck_20 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_20'Address));
   end Rcheck_20;

   procedure Rcheck_21 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_21'Address));
   end Rcheck_21;

   procedure Rcheck_22 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Program_Error_Msg (File, Line, To_Ptr (Rmsg_22'Address));
   end Rcheck_22;

   procedure Rcheck_23 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_23'Address));
   end Rcheck_23;

   procedure Rcheck_24 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_24'Address));
   end Rcheck_24;

   procedure Rcheck_25 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_25'Address));
   end Rcheck_25;

   procedure Rcheck_26 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_26'Address));
   end Rcheck_26;

   procedure Rcheck_27 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_27'Address));
   end Rcheck_27;

   procedure Rcheck_28 (File : Big_String_Ptr; Line : Integer) is
   begin
      Raise_Storage_Error_Msg (File, Line, To_Ptr (Rmsg_28'Address));
   end Rcheck_28;

   ---------------------------------------
   -- Exception backtracing subprograms --
   ---------------------------------------

   --  What is automatically output when exception tracing is on basically
   --  corresponds to the usual exception information, but with the call
   --  chain backtrace possibly tailored by a backtrace decorator. Modifying
   --  Exception_Information itself is not a good idea because the decorated
   --  output is completely out of control and would break all our code
   --  related to the streaming of exceptions.
   --
   --  We then provide an alternative function to Exception_Information to
   --  compute the possibly tailored output, which is equivalent if no
   --  decorator is currently set :

   function Tailored_Exception_Information
     (X    : Exception_Occurrence)
      return String;
   --  Exception information to be output in the case of automatic tracing
   --  requested through GNAT.Exception_Traces.
   --
   --  This is the same as Exception_Information if no backtrace decorator
   --  is currently in place. Otherwise, this is Exception_Information with
   --  the call chain raw addresses replaced by the result of a call to the
   --  current decorator provided with the call chain addresses.

   pragma Export
     (Ada, Tailored_Exception_Information,
      "__gnat_tailored_exception_information");
   --  This function is used within this package but also from within
   --  System.Tasking.Stages.
   --
   --  The output of Exception_Information and Tailored_Exception_Information
   --  share a common part which was formerly built using local procedures
   --  within Exception_Information. These procedures have been extracted from
   --  their original place to be available to Tailored_Exception_Information
   --  also.
   --
   --  Each of these procedures appends some input to an information string
   --  currently being built. The Ptr argument represents the last position
   --  in this string at which a character has been written.

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural);
   --  Append the image of N at the end of the provided information string

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural);
   --  Append a LF at the end of the provided information string

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural);
   --  Append a string at the end of the provided information string

   --  To build Exception_Information and Tailored_Exception_Information,
   --  we then use three intermediate functions :

   function Basic_Exception_Information
     (X    : Exception_Occurrence)
      return String;
   --  Returns the basic exception information string associated with a
   --  given exception occurrence. This is the common part shared by both
   --  Exception_Information and Tailored_Exception_Infomation.

   function Basic_Exception_Traceback
     (X    : Exception_Occurrence)
      return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurrence in its most basic form, that is as a raw sequence
   --  of hexadecimal binary addresses.

   function Tailored_Exception_Traceback
     (X    : Exception_Occurrence)
      return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurrence, either in its basic form if no decorator is
   --  in place, or as formatted by the decorator otherwise.

   --  The overall organization of the exception information related code
   --  is summarized below :
   --
   --           Exception_Information
   --                    |
   --            +-------+--------+
   --            |                |
   --     Basic_Exc_Info & Basic_Exc_Tback
   --
   --
   --       Tailored_Exception_Information
   --                    |
   --         +----------+----------+
   --         |                     |
   --  Basic_Exc_Info    &  Tailored_Exc_Tback
   --                               |
   --                   +-----------+------------+
   --                   |                        |
   --            Basic_Exc_Tback    Or    Tback_Decorator
   --          if no decorator set           otherwise

   ----------------------------------------------
   -- Run-Time Exception Notification Routines --
   ----------------------------------------------

   --  The notification routines described above are low level "handles" for
   --  the debugger but what needs to be done at the notification points
   --  always involves more than just calling one of these routines. The
   --  routines below provide a common run-time interface for this purpose,
   --  with variations depending on the handled/not handled status of the
   --  occurrence. They are exported to be usable by the Ada exception
   --  handling personality routine when the GCC 3 mechanism is used.

   procedure Notify_Handled_Exception
     (Handler    : Code_Loc;
      Is_Others  : Boolean;
      Low_Notify : Boolean);
   pragma Export (C, Notify_Handled_Exception,
      "__gnat_notify_handled_exception");
   --  Routine to call when a handled occurrence is about to be propagated.
   --  Low_Notify might be set to false to skip the low level debugger
   --  notification, which is useful when the information it requires is
   --  not available, like in the SJLJ case.

   procedure Notify_Unhandled_Exception (Id : Exception_Id);
   pragma Export (C, Notify_Unhandled_Exception,
     "__gnat_notify_unhandled_exception");
   --  Routine to call when an unhandled occurrence is about to be propagated.

   --------------------------------
   -- Import Run-Time C Routines --
   --------------------------------

   --  The purpose of the following pragma Imports is to ensure that we
   --  generate appropriate subprogram descriptors for all C routines in
   --  the standard GNAT library that can raise exceptions. This ensures
   --  that the exception propagation can properly find these routines

   pragma Warnings (Off);        -- so old compiler does not complain
   pragma Propagate_Exceptions;

   procedure Unhandled_Terminate;
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

   -----------------------
   -- Polling Interface --
   -----------------------

   type Unsigned is mod 2 ** 32;

   Counter : Unsigned := 0;
   --  This counter is provided for convenience. It can be used in Poll to
   --  perform periodic but not systematic operations.

   procedure Poll is separate;
   --  The actual polling routine is separate, so that it can easily
   --  be replaced with a target dependent version.

   ---------
   -- AAA --
   ---------

   --  This dummy procedure gives us the start of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keep all the
   --  procedures in their original order!

   procedure AAA is
   begin
      null;
   end AAA;

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (A : Address) return String is
      S : String (1 .. 18);
      P : Natural;
      N : Integer_Address;

      H : constant array (Integer range 0 .. 15) of Character :=
                                                         "0123456789abcdef";
   begin
      P := S'Last;
      N := To_Integer (A);
      while N /= 0 loop
         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
      end loop;

      S (P - 1) := '0';
      S (P) := 'x';
      return S (P - 1 .. S'Last);
   end Address_Image;

   ---------------------
   -- Append_Info_Nat --
   ---------------------

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if N > 9 then
         Append_Info_Nat (N / 10, Info, Ptr);
      end if;

      Ptr := Ptr + 1;
      Info (Ptr) := Character'Val (Character'Pos ('0') + N mod 10);
   end Append_Info_Nat;

   --------------------
   -- Append_Info_NL --
   --------------------

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Ptr := Ptr + 1;
      Info (Ptr) := ASCII.LF;
   end Append_Info_NL;

   ------------------------
   -- Append_Info_String --
   ------------------------

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Info (Ptr + 1 .. Ptr + S'Length) := S;
      Ptr := Ptr + S'Length;
   end Append_Info_String;

   ---------------------------------
   -- Basic_Exception_Information --
   ---------------------------------

   function Basic_Exception_Information
     (X    : Exception_Occurrence)
      return String
   is
      Name : constant String  := Exception_Name (X);
      Msg  : constant String  := Exception_Message (X);
      --  Exception name and message that are going to be included in the
      --  information to return, if not empty.

      Name_Len : constant Natural := Name'Length;
      Msg_Len  : constant Natural := Msg'Length;
      --  Length of these strings, useful to compute the size of the string
      --  we have to allocate for the complete result as well as in the body
      --  of this procedure.

      Info_Maxlen : constant Natural := 50 + Name_Len + Msg_Len;
      --  Maximum length of the information string we will build, with :
      --
      --  50 =    16 + 2   for the text associated with the name
      --        +  9 + 2   for the text associated with the message
      --        +  5 + 2   for the text associated with the pid
      --        + 14       for the text image of the pid itself and a margin.
      --
      --  This is indeed a maximum since some data may not appear at all if
      --  not relevant. For example, nothing related to the exception message
      --  will be there if this message is empty.
      --
      --  WARNING : Do not forget to update these numbers if anything
      --  involved in the computation changes.

      Info : String (1 .. Info_Maxlen);
      --  Information string we are going to build, containing the common
      --  part shared by Exc_Info and Tailored_Exc_Info.

      Ptr  : Natural := 0;

   begin
      --  Output exception name and message except for _ABORT_SIGNAL, where
      --  these two lines are omitted (see discussion above).

      if Name (1) /= '_' then
         Append_Info_String ("Exception name: ", Info, Ptr);
         Append_Info_String (Name, Info, Ptr);
         Append_Info_NL (Info, Ptr);

         if Msg_Len /= 0 then
            Append_Info_String ("Message: ", Info, Ptr);
            Append_Info_String (Msg, Info, Ptr);
            Append_Info_NL (Info, Ptr);
         end if;
      end if;

      --  Output PID line if non-zero

      if X.Pid /= 0 then
         Append_Info_String ("PID: ", Info, Ptr);
         Append_Info_Nat (X.Pid, Info, Ptr);
         Append_Info_NL (Info, Ptr);
      end if;

      return Info (1 .. Ptr);
   end Basic_Exception_Information;

   -------------------------------
   -- Basic_Exception_Traceback --
   -------------------------------

   function Basic_Exception_Traceback
     (X    : Exception_Occurrence)
      return String
   is
      Info_Maxlen : constant Natural := 35 + X.Num_Tracebacks * 19;
      --  Maximum length of the information string we are building, with :
      --  33 = 31 + 4      for the text before and after the traceback, and
      --  19 =  2 + 16 + 1 for each address ("0x" + HHHH + " ")
      --
      --  WARNING : Do not forget to update these numbers if anything
      --  involved in the computation changes.

      Info : String (1 .. Info_Maxlen);
      --  Information string we are going to build, containing an image
      --  of the call chain associated with the exception occurrence in its
      --  most basic form, that is as a sequence of binary addresses.

      Ptr  : Natural := 0;

   begin
      if X.Num_Tracebacks > 0 then
         Append_Info_String ("Call stack traceback locations:", Info, Ptr);
         Append_Info_NL (Info, Ptr);

         for J in 1 .. X.Num_Tracebacks loop
            Append_Info_String (Address_Image (X.Tracebacks (J)), Info, Ptr);
            exit when J = X.Num_Tracebacks;
            Append_Info_String (" ", Info, Ptr);
         end loop;

         Append_Info_NL (Info, Ptr);
      end if;

      return Info (1 .. Ptr);
   end Basic_Exception_Traceback;

   -----------------
   -- Break_Start --
   -----------------

   procedure Break_Start is
   begin
      null;
   end Break_Start;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Excep : EOA) is
   begin
      if Excep.Num_Tracebacks /= 0 then
         --  This is a reraise, no need to store a new (wrong) chain.
         return;
      end if;

      System.Traceback.Call_Chain
        (Excep.Tracebacks'Address,
         Max_Tracebacks,
         Excep.Num_Tracebacks,
         AAA'Address,
         ZZZ'Address);
   end Call_Chain;

   ------------------------------
   -- Current_Target_Exception --
   ------------------------------

   function Current_Target_Exception return Exception_Occurrence is
   begin
      return Null_Occurrence;
   end Current_Target_Exception;

   -------------------
   -- EId_To_String --
   -------------------

   function EId_To_String (X : Exception_Id) return String is
   begin
      if X = Null_Id then
         return "";
      else
         return Exception_Name (X);
      end if;
   end EId_To_String;

   ------------------
   -- EO_To_String --
   ------------------

   --  We use the null string to represent the null occurrence, otherwise
   --  we output the Exception_Information string for the occurrence.

   function EO_To_String (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         return "";
      else
         return Exception_Information (X);
      end if;
   end EO_To_String;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X    : Exception_Occurrence)
      return Exception_Id
   is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      else
         return X.Id;
      end if;
   end Exception_Identity;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   --  The format of the string is:

   --    Exception_Name: nnnnn
   --    Message: mmmmm
   --    PID: ppp
   --    Call stack traceback locations:
   --    0xhhhh 0xhhhh 0xhhhh ... 0xhhh

   --  where

   --    nnnn is the fully qualified name of the exception in all upper
   --    case letters. This line is always present.

   --    mmmm is the message (this line present only if message is non-null)

   --    ppp is the Process Id value as a decimal integer (this line is
   --    present only if the Process Id is non-zero). Currently we are
   --    not making use of this field.

   --    The Call stack traceback locations line and the following values
   --    are present only if at least one traceback location was recorded.
   --    the values are given in C style format, with lower case letters
   --    for a-f, and only as many digits present as are necessary.

   --  The line terminator sequence at the end of each line, including the
   --  last line is a CR-LF sequence (16#0D# followed by 16#0A#).

   --  The Exception_Name and Message lines are omitted in the abort
   --  signal case, since this is not really an exception, and the only
   --  use of this routine is internal for printing termination output.

   --  WARNING: if the format of the generated string is changed, please note
   --  that an equivalent modification to the routine String_To_EO must be
   --  made to preserve proper functioning of the stream attributes.

   function Exception_Information (X : Exception_Occurrence) return String is

      --  This information is now built using the circuitry introduced in
      --  association with the support of traceback decorators, as the
      --  catenation of the exception basic information and the call chain
      --  backtrace in its basic form.

      Basic_Info : constant String  := Basic_Exception_Information (X);
      Tback_Info : constant String  := Basic_Exception_Traceback (X);

      Basic_Len  : constant Natural := Basic_Info'Length;
      Tback_Len  : constant Natural := Tback_Info'Length;

      Info : String (1 .. Basic_Len + Tback_Len);
      Ptr  : Natural := 0;

   begin
      Append_Info_String (Basic_Info, Info, Ptr);
      Append_Info_String (Tback_Info, Info, Ptr);

      return Info;
   end Exception_Information;

   -----------------------
   -- Exception_Message --
   -----------------------

   function Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Msg (1 .. X.Msg_Length);
   end Exception_Message;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (Id : Exception_Id) return String is
   begin
      if Id = null then
         raise Constraint_Error;
      end if;

      return Id.Full_Name.all (1 .. Id.Name_Length - 1);
   end Exception_Name;

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   ---------------------------
   -- Exception_Name_Simple --
   ---------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String is
      Name : constant String := Exception_Name (X);
      P    : Natural;

   begin
      P := Name'Length;
      while P > 1 loop
         exit when Name (P - 1) = '.';
         P := P - 1;
      end loop;

      return Name (P .. Name'Length);
   end Exception_Name_Simple;

   -----------------------------
   -- Process_Raise_Exception --
   -----------------------------

   procedure Process_Raise_Exception
     (E                   : Exception_Id;
      From_Signal_Handler : Boolean)
   is
      pragma Inspection_Point (E);
      --  This is so the debugger can reliably inspect the parameter

      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;
      Mstate_Ptr  : constant Machine_State :=
                      Machine_State (Get_Machine_State_Addr.all);
      Excep       : EOA := Get_Current_Excep.all;

   begin
      --  WARNING : There should be no exception handler for this body
      --  because this would cause gigi to prepend a setup for a new
      --  jmpbuf to the sequence of statements. We would then always get
      --  this new buf in Jumpbuf_Ptr instead of the one for the exception
      --  we are handling, which would completely break the whole design
      --  of this procedure.

      --  Processing varies between zero cost and setjmp/lonjmp processing.

      if Zero_Cost_Exceptions /= 0 then

         --  Use the front-end tables to propagate if we have them, otherwise
         --  resort to the GCC back-end alternative. The backtrace for the
         --  occurrence is stored while walking up the stack, and thus stops
         --  in the handler's frame if there is one. Notifications are also
         --  not performed here since it is not yet known if the exception is
         --  handled.

         --  Set the machine state unless we are raising from a signal handler
         --  since it has already been set properly in that case.

         if not From_Signal_Handler then
            Set_Machine_State (Mstate_Ptr);
         end if;

         if Subprogram_Descriptors /= null then
            Propagate_Exception_With_FE_Support (Mstate_Ptr);
         else
            Propagate_Exception_With_GCC_Support (Mstate_Ptr);
         end if;

      else

         --  Compute the backtrace for this occurrence if the corresponding
         --  binder option has been set and we are not raising from a signal
         --  handler. Call_Chain takes care of the reraise case.

         if not From_Signal_Handler
           and then Exception_Tracebacks /= 0
         then
            Call_Chain (Excep);
         end if;

         --  If the jump buffer pointer is non-null, transfer control using
         --  it. Otherwise announce an unhandled exception (note that this
         --  means that we have no finalizations to do other than at the outer
         --  level). Perform the necessary notification tasks in both cases.

         if Jumpbuf_Ptr /= Null_Address then

            if not Excep.Exception_Raised then
               Excep.Exception_Raised := True;
               Notify_Handled_Exception (Null_Loc, False, False);

               --  The low level debugger notification is skipped from the
               --  call above because we do not have the necessary information
               --  to "feed" it properly.

            end if;

            builtin_longjmp (Jumpbuf_Ptr, 1);

         else
            Notify_Unhandled_Exception (E);
            Unhandled_Exception_Terminate;
         end if;
      end if;

   end Process_Raise_Exception;

   -----------------------------------------
   -- Propagate_Exception_With_FE_Support --
   -----------------------------------------

   procedure Propagate_Exception_With_FE_Support (Mstate : Machine_State) is
      Excep  : constant EOA := Get_Current_Excep.all;
      Loc    : Code_Loc;
      Lo, Hi : Natural;
      Pdesc  : Natural;
      Hrec   : Handler_Record_Ptr;
      Info   : Subprogram_Info_Type;

      type Machine_State_Record is
        new Storage_Array (1 .. Machine_State_Length);
      for Machine_State_Record'Alignment use Standard'Maximum_Alignment;

      procedure Duplicate_Machine_State (Dest, Src : Machine_State);
      --  Copy Src into Dest, assuming that a Machine_State is pointing to
      --  an area of Machine_State_Length bytes.

      procedure Duplicate_Machine_State (Dest, Src : Machine_State) is
         type Machine_State_Record_Access is access Machine_State_Record;
         function To_MSR is new Unchecked_Conversion
           (Machine_State, Machine_State_Record_Access);

      begin
         To_MSR (Dest).all := To_MSR (Src).all;
      end Duplicate_Machine_State;

      --  Data for handling the finalization handler case. A simple approach
      --  in this routine would simply to unwind stack frames till we find a
      --  handler and then enter it. But this is undesirable in the case where
      --  we have only finalization handlers, and no "real" handler, i.e. a
      --  case where we have an unhandled exception.

      --  In this case we prefer to signal unhandled exception with the stack
      --  intact, and entering finalization handlers would destroy the stack
      --  state. To deal with this, as we unwind the stack, we note the first
      --  finalization handler, and remember it in the following variables.
      --  We then continue to unwind. If and when we find a "real", i.e. non-
      --  finalization handler, then we use these variables to pass control to
      --  the finalization handler.

      FH_Found : Boolean := False;
      --  Set when a finalization handler is found

      FH_Mstate : aliased Machine_State_Record;
      --  Records the machine state for the finalization handler

      FH_Handler : Code_Loc := Null_Address;
      --  Record handler address for finalization handler

      FH_Num_Trb : Natural := 0;
      --  Save number of tracebacks for finalization handler

   begin
      --  Loop through stack frames as exception propagates

      Main_Loop : loop
         Loc := Get_Code_Loc (Mstate);
         exit Main_Loop when Loc = Null_Loc;

         --  Record location unless it is inside this unit. Note: this
         --  test should really say Code_Address, but Address is the same
         --  as Code_Address for unnested subprograms, and Code_Address
         --  would cause a bootstrap problem

         if Loc < AAA'Address or else Loc > ZZZ'Address then

            --  Record location unless we already recorded max tracebacks

            if Excep.Num_Tracebacks /= Max_Tracebacks then

               --  Do not record location if it is the return point from
               --  a reraise call from within a cleanup handler

               if not Excep.Cleanup_Flag then
                  Excep.Num_Tracebacks := Excep.Num_Tracebacks + 1;
                  Excep.Tracebacks (Excep.Num_Tracebacks) := Loc;

               --  For reraise call from cleanup handler, skip entry and
               --  clear the flag so that we will start to record again

               else
                  Excep.Cleanup_Flag := False;
               end if;
            end if;
         end if;

         --  Do binary search on procedure table

         Lo := 1;
         Hi := Num_Subprogram_Descriptors;

         --  Binary search loop

         loop
            Pdesc := (Lo + Hi) / 2;

            --  Note that Loc is expected to be the procedure's call point
            --  and not the return point.

            if Loc < Subprogram_Descriptors (Pdesc).Code then
               Hi := Pdesc - 1;

            elsif Pdesc < Num_Subprogram_Descriptors
              and then Loc > Subprogram_Descriptors (Pdesc + 1).Code
            then
               Lo := Pdesc + 1;

            else
               exit;
            end if;

            --  This happens when the current Loc is completely outside of
            --  the range of the program, which usually means that we reached
            --  the top level frame (e.g __start). In this case we have an
            --  unhandled exception.

            exit Main_Loop when Hi < Lo;
         end loop;

         --  Come here with Subprogram_Descriptors (Pdesc) referencing the
         --  procedure descriptor that applies to this PC value. Now do a
         --  serial search to see if any handler is applicable to this PC
         --  value, and to the exception that we are propagating

         for J in 1 .. Subprogram_Descriptors (Pdesc).Num_Handlers loop
            Hrec := Subprogram_Descriptors (Pdesc).Handler_Records (J);

            if Loc >= Hrec.Lo and then Loc < Hrec.Hi then

               --  PC range is applicable, see if handler is for this exception

               --  First test for case of "all others" (finalization) handler.
               --  We do not enter such a handler until we are sure there is
               --  a real handler further up the stack.

               if Hrec.Id = All_Others_Id then

                  --  If this is the first finalization handler, then
                  --  save the machine state so we can enter it later
                  --  without having to repeat the search.

                  if not FH_Found then
                     FH_Found   := True;
                     Duplicate_Machine_State
                       (Machine_State (FH_Mstate'Address), Mstate);
                     FH_Handler := Hrec.Handler;
                     FH_Num_Trb := Excep.Num_Tracebacks;
                  end if;

               --  Normal (non-finalization exception with matching Id)

               elsif Excep.Id = Hrec.Id
                 or else (Hrec.Id = Others_Id
                            and not Excep.Id.Not_Handled_By_Others)
               then
                  --  Perform the necessary notification tasks.

                  Notify_Handled_Exception
                    (Hrec.Handler, Hrec.Id = Others_Id, True);

                  --  If we already encountered a finalization handler, then
                  --  reset the context to that handler, and enter it.

                  if FH_Found then
                     Excep.Num_Tracebacks := FH_Num_Trb;
                     Excep.Cleanup_Flag   := True;

                     Enter_Handler
                       (Machine_State (FH_Mstate'Address), FH_Handler);

                  --  If we have not encountered a finalization handler,
                  --  then enter the current handler.

                  else
                     Enter_Handler (Mstate, Hrec.Handler);
                  end if;
               end if;
            end if;
         end loop;

         Info := Subprogram_Descriptors (Pdesc).Subprogram_Info;
         exit Main_Loop when Info = No_Info;
         Pop_Frame (Mstate, Info);
      end loop Main_Loop;

      --  Fall through if no "real" exception handler found. First thing is to
      --  perform the necessary notification tasks with the stack intact.

      Notify_Unhandled_Exception (Excep.Id);

      --  If there were finalization handlers, then enter the top one.
      --  Just because there is no handler does not mean we don't have
      --  to still execute all finalizations and cleanups before
      --  terminating. Note that the process of calling cleanups
      --  does not disturb the back trace stack, since he same
      --  exception occurrence gets reraised, and new traceback
      --  entries added as we go along.

      if FH_Found then
         Excep.Num_Tracebacks := FH_Num_Trb;
         Excep.Cleanup_Flag   := True;
         Enter_Handler (Machine_State (FH_Mstate'Address), FH_Handler);
      end if;

      --  If no cleanups, then this is the real unhandled termination

      Unhandled_Exception_Terminate;

   end Propagate_Exception_With_FE_Support;

   ------------------------------------------
   -- Propagate_Exception_With_GCC_Support --
   ------------------------------------------

   procedure Propagate_Exception_With_GCC_Support (Mstate : Machine_State) is
      Excep          : EOA := Get_Current_Excep.all;
      This_Exception : aliased GNAT_GCC_Exception;
      Status         : Unwind_Reason_Code;

   begin
      --  ??? Nothing is currently done for backtracing purposes. We could
      --  have used the personality routine to record the addresses while
      --  walking up the stack, but this method has two drawbacks : 1/ the
      --  trace is incomplete if the exception is handled since we don't walk
      --  up the frame with the handler, and 2/ we will miss frames if the
      --  exception propagates through frames for which our personality
      --  routine is not called (e.g. if C or C++ frames are on the way).

      --  Fill in the useful flags for the personality routine called for each
      --  frame via the call to Unwind_RaiseException below.

      This_Exception.Id := Excep.Id;
      This_Exception.Handled_By_Others := not Excep.Id.Not_Handled_By_Others;
      This_Exception.Has_Cleanup := False;

      --  We are looking for a regular handler first. If there is one, either
      --  it or the first at-end handler before it will be entered. If there
      --  is none, control will normally get back to after the call, with
      --  Has_Cleanup set to true if at least one at-end handler has been
      --  found while walking up the stack.

      This_Exception.Select_Cleanups := False;

      Status := Unwind_RaiseException (This_Exception'Access);

      --  If we get here we know the exception is not handled, as otherwise
      --  Unwind_RaiseException arranges for a handler to be entered. We might
      --  have met cleanups handlers, though, requiring to start again with
      --  the Select_Cleanups flag set to True.

      --  Before restarting for cleanups, take the necessary steps to enable
      --  the debugger to gain control while the stack is still intact. Flag
      --  the occurrence as raised to avoid notifying again in case cleanup
      --  handlers are entered later.

      if not Excep.Exception_Raised then
         Excep.Exception_Raised := True;
         Notify_Unhandled_Exception (Excep.Id);
      end if;

      --  Now raise again selecting cleanups as true handlers. Only do this if
      --  we know at least one such handler exists since otherwise we would
      --  perform a complete stack upwalk for nothing.

      if This_Exception.Has_Cleanup then
         This_Exception.Select_Cleanups := True;
         Status := Unwind_RaiseException (This_Exception'Access);

         --  The first cleanup found is entered. It performs its job, raises
         --  the initial exception again, and the flow goes back to the first
         --  step above with the stack in a different state.
      end if;

      --  We get here when there is no handler to be run at all. The debugger
      --  has been notified before the second step above.

      Unhandled_Exception_Terminate;

   end Propagate_Exception_With_GCC_Support;

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error
     (File : Big_String_Ptr;
      Line : Integer)
   is
   begin
      Raise_With_Location (Constraint_Error_Def'Access, File, Line);
   end Raise_Constraint_Error;

   --------------------------------
   -- Raise_Constraint_Error_Msg --
   --------------------------------

   procedure Raise_Constraint_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr)
   is
   begin
      Raise_With_Location_And_Msg
        (Constraint_Error_Def'Access, File, Line, Msg);
   end Raise_Constraint_Error_Msg;

   -------------------------
   -- Raise_Current_Excep --
   -------------------------

   procedure Raise_Current_Excep (E : Exception_Id) is
   begin
      Process_Raise_Exception (E => E, From_Signal_Handler => False);
   end Raise_Current_Excep;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : Exception_Id;
      Message : String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      if E /= null then
         Excep.Msg_Length := Len;
         Excep.Msg (1 .. Len) := Message (1 .. Len);
         Raise_With_Msg (E);
      end if;
   end Raise_Exception;

   ----------------------------
   -- Raise_Exception_Always --
   ----------------------------

   procedure Raise_Exception_Always
     (E       : Exception_Id;
      Message : String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);

      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Msg_Length := Len;
      Excep.Msg (1 .. Len) := Message (1 .. Len);
      Raise_With_Msg (E);
   end Raise_Exception_Always;

   -------------------------------
   -- Raise_From_Signal_Handler --
   -------------------------------

   procedure Raise_From_Signal_Handler
     (E : Exception_Id;
      M : Big_String_Ptr)
   is
   begin
      Set_Exception_C_Msg (E, M);
      Abort_Defer.all;
      Process_Raise_Exception (E => E, From_Signal_Handler => True);
   end Raise_From_Signal_Handler;

   ------------------
   -- Raise_No_Msg --
   ------------------

   procedure Raise_No_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Msg_Length := 0;
      Raise_With_Msg (E);
   end Raise_No_Msg;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error
     (File : Big_String_Ptr;
      Line : Integer)
   is
   begin
      Raise_With_Location (Program_Error_Def'Access, File, Line);
   end Raise_Program_Error;

   -----------------------------
   -- Raise_Program_Error_Msg --
   -----------------------------

   procedure Raise_Program_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr)
   is
   begin
      Raise_With_Location_And_Msg
        (Program_Error_Def'Access, File, Line, Msg);
   end Raise_Program_Error_Msg;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error
     (File : Big_String_Ptr;
      Line : Integer)
   is
   begin
      Raise_With_Location (Storage_Error_Def'Access, File, Line);
   end Raise_Storage_Error;

   -----------------------------
   -- Raise_Storage_Error_Msg --
   -----------------------------

   procedure Raise_Storage_Error_Msg
     (File : Big_String_Ptr;
      Line : Integer;
      Msg  : Big_String_Ptr)
   is
   begin
      Raise_With_Location_And_Msg
        (Storage_Error_Def'Access, File, Line, Msg);
   end Raise_Storage_Error_Msg;

   ----------------------
   -- Raise_With_C_Msg --
   ----------------------

   procedure Raise_With_C_Msg
     (E : Exception_Id;
      M : Big_String_Ptr)
   is
   begin
      Set_Exception_C_Msg (E, M);
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_C_Msg;

   -------------------------
   -- Raise_With_Location --
   -------------------------

   procedure Raise_With_Location
     (E : Exception_Id;
      F : Big_String_Ptr;
      L : Integer)
   is
   begin
      Set_Exception_C_Msg (E, F, L);
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Location;

   ---------------------------------
   -- Raise_With_Location_And_Msg --
   ---------------------------------

   procedure Raise_With_Location_And_Msg
     (E : Exception_Id;
      F : Big_String_Ptr;
      L : Integer;
      M : Big_String_Ptr)
   is
   begin
      Set_Exception_C_Msg (E, F, L, M);
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Location_And_Msg;

   --------------------
   -- Raise_With_Msg --
   --------------------

   procedure Raise_With_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Exception_Raised := False;
      Excep.Id               := E;
      Excep.Num_Tracebacks   := 0;
      Excep.Cleanup_Flag     := False;
      Excep.Pid              := Local_Partition_ID;
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Msg;

   -------------
   -- Reraise --
   -------------

   procedure Reraise is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Abort_Defer.all;
      Raise_Current_Excep (Excep.Id);
   end Reraise;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= null then
         Abort_Defer.all;
         Save_Occurrence (Get_Current_Excep.all.all, X);
         Raise_Current_Excep (X.Id);
      end if;
   end Reraise_Occurrence;

   -------------------------------
   -- Reraise_Occurrence_Always --
   -------------------------------

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence) is
   begin
      Abort_Defer.all;
      Save_Occurrence (Get_Current_Excep.all.all, X);
      Raise_Current_Excep (X.Id);
   end Reraise_Occurrence_Always;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence) is
   begin
      Save_Occurrence (Get_Current_Excep.all.all, X);
      Raise_Current_Excep (X.Id);
   end Reraise_Occurrence_No_Defer;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      Target.Id             := Source.Id;
      Target.Msg_Length     := Source.Msg_Length;
      Target.Num_Tracebacks := Source.Num_Tracebacks;
      Target.Pid            := Source.Pid;
      Target.Cleanup_Flag   := Source.Cleanup_Flag;

      Target.Msg (1 .. Target.Msg_Length) :=
        Source.Msg (1 .. Target.Msg_Length);

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   function Save_Occurrence
     (Source : Exception_Occurrence)
      return   EOA
   is
      Target : EOA := new Exception_Occurrence;

   begin
      Save_Occurrence (Target.all, Source);
      return Target;
   end Save_Occurrence;

   ---------------------
   -- SDP_Table_Build --
   ---------------------

   procedure SDP_Table_Build
     (SDP_Addresses   : System.Address;
      SDP_Count       : Natural;
      Elab_Addresses  : System.Address;
      Elab_Addr_Count : Natural)
   is
      type SDLP_Array is array (1 .. SDP_Count) of Subprogram_Descriptors_Ptr;
      type SDLP_Array_Ptr is access all SDLP_Array;

      function To_SDLP_Array_Ptr is new Unchecked_Conversion
        (System.Address, SDLP_Array_Ptr);

      T : constant SDLP_Array_Ptr := To_SDLP_Array_Ptr (SDP_Addresses);

      type Elab_Array is array (1 .. Elab_Addr_Count) of Code_Loc;
      type Elab_Array_Ptr is access all Elab_Array;

      function To_Elab_Array_Ptr is new Unchecked_Conversion
        (System.Address, Elab_Array_Ptr);

      EA : constant Elab_Array_Ptr := To_Elab_Array_Ptr (Elab_Addresses);

      Ndes : Natural;
      Previous_Subprogram_Descriptors : Subprogram_Descriptor_List_Ptr;

   begin
      --  If first call, then initialize count of subprogram descriptors

      if Subprogram_Descriptors = null then
         Num_Subprogram_Descriptors := 0;
      end if;

      --  First count number of subprogram descriptors. This count includes
      --  entries with duplicated code addresses (resulting from Import).

      Ndes := Num_Subprogram_Descriptors + Elab_Addr_Count;
      for J in T'Range loop
         Ndes := Ndes + T (J).Count;
      end loop;

      --  Now, allocate the new table (extra zero'th element is for sort call)
      --  after having saved the previous one

      Previous_Subprogram_Descriptors := Subprogram_Descriptors;
      Subprogram_Descriptors := new Subprogram_Descriptor_List (0 .. Ndes);

      --  If there was a previous Subprogram_Descriptors table, copy it back
      --  into the new one being built. Then free the memory used for the
      --  previous table.

      for J in 1 .. Num_Subprogram_Descriptors loop
         Subprogram_Descriptors (J) := Previous_Subprogram_Descriptors (J);
      end loop;

      Free (Previous_Subprogram_Descriptors);

      --  Next, append the elaboration routine addresses, building dummy
      --  SDP's for them as we go through the list.

      Ndes := Num_Subprogram_Descriptors;
      for J in EA'Range loop
         Ndes := Ndes + 1;
         Subprogram_Descriptors (Ndes) := new Subprogram_Descriptor_0;

         Subprogram_Descriptors (Ndes).all :=
           Subprogram_Descriptor'
             (Num_Handlers    => 0,
              Code            => Fetch_Code (EA (J)),
              Subprogram_Info => EA (J),
              Handler_Records => (1 .. 0 => null));
      end loop;

      --  Now copy in pointers to SDP addresses of application subprograms

      for J in T'Range loop
         for K in 1 .. T (J).Count loop
            Ndes := Ndes + 1;
            Subprogram_Descriptors (Ndes) := T (J).SDesc (K);
            Subprogram_Descriptors (Ndes).Code :=
              Fetch_Code (T (J).SDesc (K).Code);
         end loop;
      end loop;

      --  Now we need to sort the table into ascending PC order

      Sort (Ndes, SDP_Table_Sort_Move'Access, SDP_Table_Sort_Lt'Access);

      --  Now eliminate duplicate entries. Note that in the case where
      --  entries have duplicate code addresses, the code for the Lt
      --  routine ensures that the interesting one (i.e. the one with
      --  handler entries if there are any) comes first.

      Num_Subprogram_Descriptors := 1;

      for J in 2 .. Ndes loop
         if Subprogram_Descriptors (J).Code /=
            Subprogram_Descriptors (Num_Subprogram_Descriptors).Code
         then
            Num_Subprogram_Descriptors := Num_Subprogram_Descriptors + 1;
            Subprogram_Descriptors (Num_Subprogram_Descriptors) :=
              Subprogram_Descriptors (J);
         end if;
      end loop;

   end SDP_Table_Build;

   -----------------------
   -- SDP_Table_Sort_Lt --
   -----------------------

   function SDP_Table_Sort_Lt (Op1, Op2 : Natural) return Boolean is
      SDC1 : constant Code_Loc := Subprogram_Descriptors (Op1).Code;
      SDC2 : constant Code_Loc := Subprogram_Descriptors (Op2).Code;

   begin
      if SDC1 < SDC2 then
         return True;

      elsif SDC1 > SDC2 then
         return False;

      --  For two descriptors for the same procedure, we want the more
      --  interesting one first. A descriptor with an exception handler
      --  is more interesting than one without. This happens if the less
      --  interesting one came from a pragma Import.

      else
         return Subprogram_Descriptors (Op1).Num_Handlers /= 0
           and then Subprogram_Descriptors (Op2).Num_Handlers = 0;
      end if;
   end SDP_Table_Sort_Lt;

   --------------------------
   -- SDP_Table_Sort_Move --
   --------------------------

   procedure SDP_Table_Sort_Move (From : Natural; To : Natural) is
   begin
      Subprogram_Descriptors (To) := Subprogram_Descriptors (From);
   end SDP_Table_Sort_Move;

   -------------------------
   -- Set_Exception_C_Msg --
   -------------------------

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg1 : Big_String_Ptr;
      Line : Integer        := 0;
      Msg2 : Big_String_Ptr := null)
   is
      Excep  : constant EOA := Get_Current_Excep.all;
      Val    : Integer := Line;
      Remind : Integer;
      Size   : Integer := 1;
      Ptr    : Natural;

   begin
      Excep.Exception_Raised := False;
      Excep.Id               := Id;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;
      Excep.Msg_Length       := 0;
      Excep.Cleanup_Flag     := False;

      while Msg1 (Excep.Msg_Length + 1) /= ASCII.NUL
        and then Excep.Msg_Length < Exception_Msg_Max_Length
      loop
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := Msg1 (Excep.Msg_Length);
      end loop;

      --  Append line number if present

      if Line > 0 then

         --  Compute the number of needed characters

         while Val > 0 loop
            Val := Val / 10;
            Size := Size + 1;
         end loop;

         --  If enough characters are available, put the line number

         if Excep.Msg_Length <= Exception_Msg_Max_Length - Size then
            Excep.Msg (Excep.Msg_Length + 1) := ':';
            Excep.Msg_Length := Excep.Msg_Length + Size;
            Val := Line;
            Size := 0;

            while Val > 0 loop
               Remind := Val rem 10;
               Val := Val / 10;
               Excep.Msg (Excep.Msg_Length - Size) :=
                 Character'Val (Remind + Character'Pos ('0'));
               Size := Size + 1;
            end loop;
         end if;
      end if;

      --  Append second message if present

      if Msg2 /= null
        and then Excep.Msg_Length + 1 < Exception_Msg_Max_Length
      then
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := ' ';

         Ptr := 1;
         while Msg2 (Ptr) /= ASCII.NUL
           and then Excep.Msg_Length < Exception_Msg_Max_Length
         loop
            Excep.Msg_Length := Excep.Msg_Length + 1;
            Excep.Msg (Excep.Msg_Length) := Msg2 (Ptr);
            Ptr := Ptr + 1;
         end loop;
      end if;
   end Set_Exception_C_Msg;

   -------------------
   -- String_To_EId --
   -------------------

   function String_To_EId (S : String) return Exception_Id is
   begin
      if S = "" then
         return Null_Id;
      else
         return Exception_Id (Internal_Exception (S));
      end if;
   end String_To_EId;

   ------------------
   -- String_To_EO --
   ------------------

   function String_To_EO (S : String) return Exception_Occurrence is
      From : Natural;
      To   : Integer;

      X : Exception_Occurrence;
      --  This is the exception occurrence we will create

      procedure Bad_EO;
      pragma No_Return (Bad_EO);
      --  Signal bad exception occurrence string

      procedure Next_String;
      --  On entry, To points to last character of previous line of the
      --  message, terminated by LF. On return, From .. To are set to
      --  specify the next string, or From > To if there are no more lines.

      procedure Bad_EO is
      begin
         Raise_Exception
           (Program_Error'Identity,
            "bad exception occurrence in stream input");
      end Bad_EO;

      procedure Next_String is
      begin
         From := To + 2;

         if From < S'Last then
            To := From + 1;

            while To < S'Last - 1 loop
               if To >= S'Last then
                  Bad_EO;
               elsif S (To + 1) = ASCII.LF then
                  exit;
               else
                  To := To + 1;
               end if;
            end loop;
         end if;
      end Next_String;

   --  Start of processing for String_To_EO

   begin
      if S = "" then
         return Null_Occurrence;

      else
         X.Cleanup_Flag := False;

         To := S'First - 2;
         Next_String;

         if S (From .. From + 15) /= "Exception name: " then
            Bad_EO;
         end if;

         X.Id := Exception_Id (Internal_Exception (S (From + 16 .. To)));

         Next_String;

         if From <= To and then S (From) = 'M' then
            if S (From .. From + 8) /= "Message: " then
               Bad_EO;
            end if;

            X.Msg_Length := To - From - 8;
            X.Msg (1 .. X.Msg_Length) := S (From + 9 .. To);
            Next_String;

         else
            X.Msg_Length := 0;
         end if;

         X.Pid := 0;

         if From <= To and then S (From) = 'P' then
            if S (From .. From + 3) /= "PID:" then
               Bad_EO;
            end if;

            From := From + 5; -- skip past PID: space

            while From <= To loop
               X.Pid := X.Pid * 10 +
                          (Character'Pos (S (From)) - Character'Pos ('0'));
               From := From + 1;
            end loop;

            Next_String;
         end if;

         X.Num_Tracebacks := 0;

         if From <= To then
            if S (From .. To) /= "Call stack traceback locations:" then
               Bad_EO;
            end if;

            Next_String;
            loop
               exit when From > To;

               declare
                  Ch : Character;
                  C  : Integer_Address;
                  N  : Integer_Address;

               begin
                  if S (From) /= '0'
                    or else S (From + 1) /= 'x'
                  then
                     Bad_EO;
                  else
                     From := From + 2;
                  end if;

                  C := 0;
                  while From <= To loop
                     Ch := S (From);

                     if Ch in '0' .. '9' then
                        N :=
                          Character'Pos (S (From)) - Character'Pos ('0');

                     elsif Ch in 'a' .. 'f' then
                        N :=
                          Character'Pos (S (From)) - Character'Pos ('a') + 10;

                     elsif Ch = ' ' then
                        From := From + 1;
                        exit;

                     else
                        Bad_EO;
                     end if;

                     C := C * 16 + N;

                     From := From + 1;
                  end loop;

                  if X.Num_Tracebacks = Max_Tracebacks then
                     Bad_EO;
                  end if;

                  X.Num_Tracebacks := X.Num_Tracebacks + 1;
                  X.Tracebacks (X.Num_Tracebacks) := To_Address (C);
               end;
            end loop;
         end if;

         --  If an exception was converted to a string, it must have
         --  already been raised, so flag it accordingly and we are done.

         X.Exception_Raised := True;
         return X;
      end if;
   end String_To_EO;

   ----------------------------------
   -- Tailored_Exception_Traceback --
   ----------------------------------

   function Tailored_Exception_Traceback
     (X    : Exception_Occurrence)
      return String
   is
      --  We indeed reference the decorator *wrapper* from here and not the
      --  decorator itself. The purpose of the local variable Wrapper is to
      --  prevent a potential crash by race condition in the code below. The
      --  atomicity of this assignment is enforced by pragma Atomic in
      --  System.Soft_Links.

      --  The potential race condition here, if no local variable was used,
      --  relates to the test upon the wrapper's value and the call, which
      --  are not performed atomically. With the local variable, potential
      --  changes of the wrapper's global value between the test and the
      --  call become inoffensive.

      Wrapper : constant Traceback_Decorator_Wrapper_Call :=
                  Traceback_Decorator_Wrapper;

   begin
      if Wrapper = null then
         return Basic_Exception_Traceback (X);
      else
         return Wrapper.all (X.Tracebacks'Address, X.Num_Tracebacks);
      end if;
   end Tailored_Exception_Traceback;

   ------------------------------------
   -- Tailored_Exception_Information --
   ------------------------------------

   function Tailored_Exception_Information
     (X    : Exception_Occurrence)
      return String
   is
      --  The tailored exception information is simply the basic information
      --  associated with the tailored call chain backtrace.

      Basic_Info : constant String  := Basic_Exception_Information (X);
      Tback_Info : constant String  := Tailored_Exception_Traceback (X);

      Basic_Len  : constant Natural := Basic_Info'Length;
      Tback_Len  : constant Natural := Tback_Info'Length;

      Info : String (1 .. Basic_Len + Tback_Len);
      Ptr  : Natural := 0;

   begin
      Append_Info_String (Basic_Info, Info, Ptr);
      Append_Info_String (Tback_Info, Info, Ptr);

      return Info;
   end Tailored_Exception_Information;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception
     (Id        : Exception_Id;
      Handler   : Code_Loc;
      Is_Others : Boolean)
   is
   begin
      null;
   end Notify_Exception;

   ------------------------------
   -- Notify_Handled_Exception --
   ------------------------------

   procedure Notify_Handled_Exception
     (Handler    : Code_Loc;
      Is_Others  : Boolean;
      Low_Notify : Boolean)
   is
      Excep  : constant EOA := Get_Current_Excep.all;

   begin
      --  Notify the debugger that we have found a handler and are about to
      --  propagate an exception, but only if specifically told to do so.

      if Low_Notify then
         Notify_Exception (Excep.Id, Handler, Is_Others);
      end if;

      --  Output some exception information if necessary, as specified by
      --  GNAT.Exception_Traces. Take care not to output information about
      --  internal exceptions.
      --
      --  ??? In the ZCX case, the traceback entries we have at this point
      --  only include the ones we stored while walking up the stack *up to
      --  the handler*. All the frames above the subprogram in which the
      --  handler is found are missing.

      if Exception_Trace = Every_Raise
        and then not Excep.Id.Not_Handled_By_Others
      then
         To_Stderr (Nline);
         To_Stderr ("Exception raised");
         To_Stderr (Nline);
         To_Stderr (Tailored_Exception_Information (Excep.all));
      end if;

   end Notify_Handled_Exception;

   ------------------------------
   -- Notify_Handled_Exception --
   ------------------------------

   procedure Notify_Unhandled_Exception (Id : Exception_Id) is
   begin
      --  Simply perform the two necessary low level notification calls.

      Unhandled_Exception;
      Notify_Exception (Id, Null_Loc, False);

   end Notify_Unhandled_Exception;

   -----------------------------------
   -- Unhandled_Exception_Terminate --
   -----------------------------------

   adafinal_Called : Boolean := False;
   --  Used to prevent recursive call to adafinal in the event that
   --  adafinal processing itself raises an unhandled exception.

   type FILEs is new System.Address;
   type int is new Integer;

   procedure Unhandled_Exception_Terminate is

      Excep : constant EOA := Save_Occurrence (Get_Current_Excep.all.all);
      --  This occurrence will be used to display a message after finalization.
      --  It is necessary to save a copy here, or else the designated value
      --  could be overwritten if an exception is raised during finalization
      --  (even if that exception is caught).

      Msg : constant String := Exception_Message (Excep.all);

   --  Start of processing for Unhandled_Exception_Terminate

   begin
      --  First call adafinal

      if not adafinal_Called then
         adafinal_Called := True;
         System.Soft_Links.Adafinal.all;
      end if;

      --  Check for special case of raising _ABORT_SIGNAL, which is not
      --  really an exception at all. We recognize this by the fact that
      --  it is the only exception whose name starts with underscore.

      if Exception_Name (Excep.all) (1) = '_' then
         To_Stderr (Nline);
         To_Stderr ("Execution terminated by abort of environment task");
         To_Stderr (Nline);

      --  If no tracebacks, we print the unhandled exception in the old style
      --  (i.e. the style used before ZCX was implemented). We do this to
      --  retain compatibility, especially with the nightly scripts, but
      --  this can be removed at some point ???

      elsif Excep.Num_Tracebacks = 0 then
         To_Stderr (Nline);
         To_Stderr ("raised ");
         To_Stderr (Exception_Name (Excep.all));

         if Msg'Length /= 0 then
            To_Stderr (" : ");
            To_Stderr (Msg);
         end if;

         To_Stderr (Nline);

      --  New style, zero cost exception case

      else
         --  Tailored_Exception_Information is also called here so that the
         --  backtrace decorator gets called if it has been set. This is
         --  currently required because some paths in Raise_Current_Excep
         --  do not go through the calls that display this information.
         --
         --  Note also that with the current scheme in Raise_Current_Excep
         --  we can have this whole information output twice, typically when
         --  some handler is found on the call chain but none deals with the
         --  occurrence or if this occurrence gets reraised up to here.

         To_Stderr (Nline);
         To_Stderr ("Execution terminated by unhandled exception");
         To_Stderr (Nline);
         To_Stderr (Tailored_Exception_Information (Excep.all));
      end if;

      --  Perform system dependent shutdown code

      declare
         procedure Unhandled_Terminate;
         pragma No_Return (Unhandled_Terminate);
         pragma Import
           (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

      begin
         Unhandled_Terminate;
      end;

   end Unhandled_Exception_Terminate;

   ------------------------------
   -- Raise_Exception_No_Defer --
   ------------------------------

   procedure Raise_Exception_No_Defer
     (E       : Exception_Id;
      Message : String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);

      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Exception_Raised := False;
      Excep.Msg_Length       := Len;
      Excep.Msg (1 .. Len)   := Message (1 .. Len);
      Excep.Id               := E;
      Excep.Num_Tracebacks   := 0;
      Excep.Cleanup_Flag     := False;
      Excep.Pid              := Local_Partition_ID;

      --  DO NOT CALL Abort_Defer.all; !!!!

      Raise_Current_Excep (E);
   end Raise_Exception_No_Defer;

   ---------------
   -- To_Stderr --
   ---------------

   procedure To_Stderr (S : String) is
      procedure put_char_stderr (C : int);
      pragma Import (C, put_char_stderr, "put_char_stderr");

   begin
      for J in 1 .. S'Length loop
         if S (J) /= ASCII.CR then
            put_char_stderr (Character'Pos (S (J)));
         end if;
      end loop;
   end To_Stderr;

   ---------
   -- ZZZ --
   ---------

   --  This dummy procedure gives us the end of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keeps all the
   --  procedures in their original order!

   procedure ZZZ is
   begin
      null;
   end ZZZ;

begin
   --  Allocate the Non-Tasking Machine_State

   Set_Machine_State_Addr_NT (System.Address (Allocate_Machine_State));
end Ada.Exceptions;
