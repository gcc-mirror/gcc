------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This version of Ada.Exceptions fully supports both Ada 95 and Ada 2005.
--  It is used in all situations except for the build of the compiler and
--  other basic tools. For these latter builds, we use an Ada 95-only version.

--  The reason for this splitting off of a separate version is that bootstrap
--  compilers often will be used that do not support Ada 2005 features, and
--  Ada.Exceptions is part of the compiler sources.

pragma Style_Checks (All_Checks);
--  No subprogram ordering check, due to logical grouping

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with System.Exception_Tables.

with System;                  use System;
with System.Exceptions;       use System.Exceptions;
with System.Exceptions_Debug; use System.Exceptions_Debug;
with System.Standard_Library; use System.Standard_Library;
with System.Soft_Links;       use System.Soft_Links;
with System.WCh_Con;          use System.WCh_Con;
with System.WCh_StW;          use System.WCh_StW;

package body Ada.Exceptions is

   pragma Suppress (All_Checks);
   --  We definitely do not want exceptions occurring within this unit, or
   --  we are in big trouble. If an exceptional situation does occur, better
   --  that it not be raised, since raising it can cause confusing chaos.

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Note: the exported subprograms in this package body are called directly
   --  from C clients using the given external name, even though they are not
   --  technically visible in the Ada sense.

   function Code_Address_For_AAA return System.Address;
   function Code_Address_For_ZZZ return System.Address;
   --  Return start and end of procedures in this package
   --
   --  These procedures are used to provide exclusion bounds in
   --  calls to Call_Chain at exception raise points from this unit. The
   --  purpose is to arrange for the exception tracebacks not to include
   --  frames from routines involved in the raise process, as these are
   --  meaningless from the user's standpoint.
   --
   --  For these bounds to be meaningful, we need to ensure that the object
   --  code for the routines involved in processing a raise is located after
   --  the object code Code_Address_For_AAA and before the object code
   --  Code_Address_For_ZZZ. This will indeed be the case as long as the
   --  following rules are respected:
   --
   --  1) The bodies of the subprograms involved in processing a raise
   --     are located after the body of Code_Address_For_AAA and before the
   --     body of Code_Address_For_ZZZ.
   --
   --  2) No pragma Inline applies to any of these subprograms, as this
   --     could delay the corresponding assembly output until the end of
   --     the unit.

   procedure Call_Chain (Excep : EOA);
   --  Store up to Max_Tracebacks in Excep, corresponding to the current
   --  call chain.

   function Image (Index : Integer) return String;
   --  Return string image corresponding to Index

   procedure To_Stderr (S : String);
   pragma Export (Ada, To_Stderr, "__gnat_to_stderr");
   --  Little routine to output string to stderr that is also used
   --  in the tasking run time.

   procedure To_Stderr (C : Character);
   pragma Inline (To_Stderr);
   pragma Export (Ada, To_Stderr, "__gnat_to_stderr_char");
   --  Little routine to output a character to stderr, used by some of
   --  the separate units below.

   package Exception_Data is

      ---------------------------------
      -- Exception messages routines --
      ---------------------------------

      procedure Set_Exception_C_Msg
        (Excep  : EOA;
         Id     : Exception_Id;
         Msg1   : System.Address;
         Line   : Integer        := 0;
         Column : Integer        := 0;
         Msg2   : System.Address := System.Null_Address);
      --  This routine is called to setup the exception referenced by X
      --  to contain the indicated Id value and message. Msg1 is a null
      --  terminated string which is generated as the exception message. If
      --  line is non-zero, then a colon and the decimal representation of
      --  this integer is appended to the message. Ditto for Column. When Msg2
      --  is non-null, a space and this additional null terminated string is
      --  added to the message.

      procedure Set_Exception_Msg
        (Excep   : EOA;
         Id      : Exception_Id;
         Message : String);
      --  This routine is called to setup the exception referenced by X
      --  to contain the indicated Id value and message. Message is a string
      --  which is generated as the exception message.

      --------------------------------------
      -- Exception information subprogram --
      --------------------------------------

      function Exception_Information (X : Exception_Occurrence) return String;
      --  The format of the exception information is as follows:
      --
      --    Exception_Name: <exception name> (as in Exception_Name)
      --    Message: <message> (only if Exception_Message is empty)
      --    PID=nnnn (only if != 0)
      --    Call stack traceback locations:  (only if at least one location)
      --    <0xyyyyyyyy 0xyyyyyyyy ...>      (is recorded)
      --
      --  The lines are separated by a ASCII.LF character.
      --  The nnnn is the partition Id given as decimal digits.
      --  The 0x... line represents traceback program counter locations, in
      --  execution order with the first one being the exception location. It
      --  is present only
      --
      --  The Exception_Name and Message lines are omitted in the abort
      --  signal case, since this is not really an exception.

      --  !! If the format of the generated string is changed, please note
      --  !! that an equivalent modification to the routine String_To_EO must
      --  !! be made to preserve proper functioning of the stream attributes.

      ---------------------------------------
      -- Exception backtracing subprograms --
      ---------------------------------------

      --  What is automatically output when exception tracing is on is the
      --  usual exception information with the call chain backtrace possibly
      --  tailored by a backtrace decorator. Modifying Exception_Information
      --  itself is not a good idea because the decorated output is completely
      --  out of control and would break all our code related to the streaming
      --  of exceptions.  We then provide an alternative function to compute
      --  the possibly tailored output, which is equivalent if no decorator is
      --  currently set:

      function Tailored_Exception_Information
        (X : Exception_Occurrence) return String;
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
      --  This is currently used by System.Tasking.Stages

   end Exception_Data;

   package Exception_Traces is

      use Exception_Data;
      --  Imports Tailored_Exception_Information

      ----------------------------------------------
      -- Run-Time Exception Notification Routines --
      ----------------------------------------------

      --  These subprograms provide a common run-time interface to trigger the
      --  actions required when an exception is about to be propagated (e.g.
      --  user specified actions or output of exception information). They are
      --  exported to be usable by the Ada exception handling personality
      --  routine when the GCC 3 mechanism is used.

      procedure Notify_Handled_Exception (Excep : EOA);
      pragma Export
        (C, Notify_Handled_Exception, "__gnat_notify_handled_exception");
      --  This routine is called for a handled occurrence is about to be
      --  propagated.

      procedure Notify_Unhandled_Exception (Excep : EOA);
      pragma Export
        (C, Notify_Unhandled_Exception, "__gnat_notify_unhandled_exception");
      --  This routine is called when an unhandled occurrence is about to be
      --  propagated.

      procedure Unhandled_Exception_Terminate (Excep : EOA);
      pragma No_Return (Unhandled_Exception_Terminate);
      --  This procedure is called to terminate execution following an
      --  unhandled exception. The exception information, including
      --  traceback if available is output, and execution is then
      --  terminated. Note that at the point where this routine is
      --  called, the stack has typically been destroyed.

   end Exception_Traces;

   package Exception_Propagation is

      ------------------------------------
      -- Exception propagation routines --
      ------------------------------------

      function Allocate_Occurrence return EOA;
      --  Allocate an exception occurence (as well as the machine occurence)

      procedure Propagate_Exception (Excep : EOA);
      pragma No_Return (Propagate_Exception);
      --  This procedure propagates the exception represented by Excep

   end Exception_Propagation;

   package Stream_Attributes is

      --------------------------------
      -- Stream attributes routines --
      --------------------------------

      function EId_To_String (X : Exception_Id) return String;
      function String_To_EId (S : String) return Exception_Id;
      --  Functions for implementing Exception_Id stream attributes

      function EO_To_String (X : Exception_Occurrence) return String;
      function String_To_EO (S : String) return Exception_Occurrence;
      --  Functions for implementing Exception_Occurrence stream
      --  attributes

   end Stream_Attributes;

   procedure Complete_Occurrence (X : EOA);
   --  Finish building the occurrence: save the call chain and notify the
   --  debugger.

   procedure Complete_And_Propagate_Occurrence (X : EOA);
   pragma No_Return (Complete_And_Propagate_Occurrence);
   --  This is a simple wrapper to Complete_Occurrence and
   --  Exception_Propagation.Propagate_Exception.

   function Create_Occurrence_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address) return EOA;
   --  Create and build an exception occurrence using exception id E and
   --  nul-terminated message M.

   function Create_Machine_Occurrence_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address) return System.Address;
   pragma Export (C, Create_Machine_Occurrence_From_Signal_Handler,
                  "__gnat_create_machine_occurrence_from_signal_handler");
   --  Create and build an exception occurrence using exception id E and
   --  nul-terminated message M. Return the machine occurrence.

   procedure Raise_Exception_No_Defer
     (E       : Exception_Id;
      Message : String := "");
   pragma Export
    (Ada, Raise_Exception_No_Defer,
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

   procedure Raise_With_Location_And_Msg
     (E : Exception_Id;
      F : System.Address;
      L : Integer;
      C : Integer := 0;
      M : System.Address := System.Null_Address);
   pragma No_Return (Raise_With_Location_And_Msg);
   --  Raise an exception with given exception id value. A filename and line
   --  number is associated with the raise and is stored in the exception
   --  occurrence and in addition a column and a string message M may be
   --  appended to this (if not null/0).

   procedure Raise_Constraint_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Constraint_Error);
   pragma Export
     (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error with file:line information

   procedure Raise_Constraint_Error_Msg
     (File   : System.Address;
      Line   : Integer;
      Column : Integer;
      Msg    : System.Address);
   pragma No_Return (Raise_Constraint_Error_Msg);
   pragma Export
     (C, Raise_Constraint_Error_Msg, "__gnat_raise_constraint_error_msg");
   --  Raise constraint error with file:line:col + msg information

   procedure Raise_Program_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Program_Error);
   pragma Export
     (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise program error with file:line information

   procedure Raise_Program_Error_Msg
     (File : System.Address;
      Line : Integer;
      Msg  : System.Address);
   pragma No_Return (Raise_Program_Error_Msg);
   pragma Export
     (C, Raise_Program_Error_Msg, "__gnat_raise_program_error_msg");
   --  Raise program error with file:line + msg information

   procedure Raise_Storage_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Storage_Error);
   pragma Export
     (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error with file:line information

   procedure Raise_Storage_Error_Msg
     (File : System.Address;
      Line : Integer;
      Msg  : System.Address);
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
   --     R_E_No_Defer(i)    R_W_Msg(i)       R_W_Loc
   --           |               |              |   |
   --           +------------+  |  +-----------+   +--+
   --                        |  |  |                  |
   --                        |  |  |             Set_E_C_Msg(i)
   --                        |  |  |
   --            Complete_And_Propagate_Occurrence

   procedure Reraise;
   pragma No_Return (Reraise);
   pragma Export (C, Reraise, "__gnat_reraise");
   --  Reraises the exception referenced by the Current_Excep field of
   --  the TSD (all fields of this exception occurrence are set). Abort
   --  is deferred before the reraise operation.
   --  Called from System.Tasking.RendezVous.Exceptional_Complete_RendezVous

   procedure Transfer_Occurrence
     (Target : Exception_Occurrence_Access;
      Source : Exception_Occurrence);
   pragma Export (C, Transfer_Occurrence, "__gnat_transfer_occurrence");
   --  Called from s-tasren.adb:Local_Complete_RendezVous and
   --  s-tpobop.adb:Exceptional_Complete_Entry_Body to setup Target from
   --  Source as an exception to be propagated in the caller task. Target is
   --  expected to be a pointer to the fixed TSD occurrence for this task.

   -----------------------------
   -- Run-Time Check Routines --
   -----------------------------

   --  These routines raise a specific exception with a reason message
   --  attached. The parameters are the file name and line number in each
   --  case. The names are defined by Exp_Ch11.Get_RT_Exception_Name.

   procedure Rcheck_CE_Access_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Access_Parameter
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Discriminant_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Divide_By_Zero
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Index_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Invalid_Data
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Length_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Exception_Id
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Not_Allowed
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Overflow_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Partition_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Range_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Tag_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Access_Before_Elaboration
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Accessibility_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Address_Of_Intrinsic
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Aliased_Parameters
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_All_Guards_Closed
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Bad_Predicated_Generic_Type
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Current_Task_In_Entry_Body
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Duplicated_Entry_Address
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Implicit_Return
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Misaligned_Address_Value
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Missing_Return
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Overlaid_Controlled_Object
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Potentially_Blocking_Operation
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Stubbed_Subprogram_Called
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Unchecked_Union_Restriction
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Non_Transportable_Actual
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Empty_Storage_Pool
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Infinite_Recursion
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Object_Too_Large
     (File : System.Address; Line : Integer);

   procedure Rcheck_CE_Access_Check_Ext
     (File : System.Address; Line, Column : Integer);
   procedure Rcheck_CE_Index_Check_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer);
   procedure Rcheck_CE_Invalid_Data_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer);
   procedure Rcheck_CE_Range_Check_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer);

   procedure Rcheck_PE_Finalize_Raised_Exception
     (File : System.Address; Line : Integer);
   --  This routine is separated out because it has quite different behavior
   --  from the others. This is the "finalize/adjust raised exception". This
   --  subprogram is always called with abort deferred, unlike all other
   --  Rcheck_* routines, it needs to call Raise_Exception_No_Defer.

   pragma Export (C, Rcheck_CE_Access_Check,
                  "__gnat_rcheck_CE_Access_Check");
   pragma Export (C, Rcheck_CE_Null_Access_Parameter,
                  "__gnat_rcheck_CE_Null_Access_Parameter");
   pragma Export (C, Rcheck_CE_Discriminant_Check,
                  "__gnat_rcheck_CE_Discriminant_Check");
   pragma Export (C, Rcheck_CE_Divide_By_Zero,
                  "__gnat_rcheck_CE_Divide_By_Zero");
   pragma Export (C, Rcheck_CE_Explicit_Raise,
                  "__gnat_rcheck_CE_Explicit_Raise");
   pragma Export (C, Rcheck_CE_Index_Check,
                  "__gnat_rcheck_CE_Index_Check");
   pragma Export (C, Rcheck_CE_Invalid_Data,
                  "__gnat_rcheck_CE_Invalid_Data");
   pragma Export (C, Rcheck_CE_Length_Check,
                  "__gnat_rcheck_CE_Length_Check");
   pragma Export (C, Rcheck_CE_Null_Exception_Id,
                  "__gnat_rcheck_CE_Null_Exception_Id");
   pragma Export (C, Rcheck_CE_Null_Not_Allowed,
                  "__gnat_rcheck_CE_Null_Not_Allowed");
   pragma Export (C, Rcheck_CE_Overflow_Check,
                  "__gnat_rcheck_CE_Overflow_Check");
   pragma Export (C, Rcheck_CE_Partition_Check,
                  "__gnat_rcheck_CE_Partition_Check");
   pragma Export (C, Rcheck_CE_Range_Check,
                  "__gnat_rcheck_CE_Range_Check");
   pragma Export (C, Rcheck_CE_Tag_Check,
                  "__gnat_rcheck_CE_Tag_Check");
   pragma Export (C, Rcheck_PE_Access_Before_Elaboration,
                  "__gnat_rcheck_PE_Access_Before_Elaboration");
   pragma Export (C, Rcheck_PE_Accessibility_Check,
                  "__gnat_rcheck_PE_Accessibility_Check");
   pragma Export (C, Rcheck_PE_Address_Of_Intrinsic,
                  "__gnat_rcheck_PE_Address_Of_Intrinsic");
   pragma Export (C, Rcheck_PE_Aliased_Parameters,
                  "__gnat_rcheck_PE_Aliased_Parameters");
   pragma Export (C, Rcheck_PE_All_Guards_Closed,
                  "__gnat_rcheck_PE_All_Guards_Closed");
   pragma Export (C, Rcheck_PE_Bad_Predicated_Generic_Type,
                  "__gnat_rcheck_PE_Bad_Predicated_Generic_Type");
   pragma Export (C, Rcheck_PE_Current_Task_In_Entry_Body,
                  "__gnat_rcheck_PE_Current_Task_In_Entry_Body");
   pragma Export (C, Rcheck_PE_Duplicated_Entry_Address,
                  "__gnat_rcheck_PE_Duplicated_Entry_Address");
   pragma Export (C, Rcheck_PE_Explicit_Raise,
                  "__gnat_rcheck_PE_Explicit_Raise");
   pragma Export (C, Rcheck_PE_Finalize_Raised_Exception,
                  "__gnat_rcheck_PE_Finalize_Raised_Exception");
   pragma Export (C, Rcheck_PE_Implicit_Return,
                  "__gnat_rcheck_PE_Implicit_Return");
   pragma Export (C, Rcheck_PE_Misaligned_Address_Value,
                  "__gnat_rcheck_PE_Misaligned_Address_Value");
   pragma Export (C, Rcheck_PE_Missing_Return,
                  "__gnat_rcheck_PE_Missing_Return");
   pragma Export (C, Rcheck_PE_Overlaid_Controlled_Object,
                  "__gnat_rcheck_PE_Overlaid_Controlled_Object");
   pragma Export (C, Rcheck_PE_Potentially_Blocking_Operation,
                  "__gnat_rcheck_PE_Potentially_Blocking_Operation");
   pragma Export (C, Rcheck_PE_Stubbed_Subprogram_Called,
                  "__gnat_rcheck_PE_Stubbed_Subprogram_Called");
   pragma Export (C, Rcheck_PE_Unchecked_Union_Restriction,
                  "__gnat_rcheck_PE_Unchecked_Union_Restriction");
   pragma Export (C, Rcheck_PE_Non_Transportable_Actual,
                  "__gnat_rcheck_PE_Non_Transportable_Actual");
   pragma Export (C, Rcheck_SE_Empty_Storage_Pool,
                  "__gnat_rcheck_SE_Empty_Storage_Pool");
   pragma Export (C, Rcheck_SE_Explicit_Raise,
                  "__gnat_rcheck_SE_Explicit_Raise");
   pragma Export (C, Rcheck_SE_Infinite_Recursion,
                  "__gnat_rcheck_SE_Infinite_Recursion");
   pragma Export (C, Rcheck_SE_Object_Too_Large,
                  "__gnat_rcheck_SE_Object_Too_Large");

   pragma Export (C, Rcheck_CE_Access_Check_Ext,
                  "__gnat_rcheck_CE_Access_Check_ext");
   pragma Export (C, Rcheck_CE_Index_Check_Ext,
                  "__gnat_rcheck_CE_Index_Check_ext");
   pragma Export (C, Rcheck_CE_Invalid_Data_Ext,
                  "__gnat_rcheck_CE_Invalid_Data_ext");
   pragma Export (C, Rcheck_CE_Range_Check_Ext,
                  "__gnat_rcheck_CE_Range_Check_ext");

   --  None of these procedures ever returns (they raise an exception!). By
   --  using pragma No_Return, we ensure that any junk code after the call,
   --  such as normal return epilog stuff, can be eliminated).

   pragma No_Return (Rcheck_CE_Access_Check);
   pragma No_Return (Rcheck_CE_Null_Access_Parameter);
   pragma No_Return (Rcheck_CE_Discriminant_Check);
   pragma No_Return (Rcheck_CE_Divide_By_Zero);
   pragma No_Return (Rcheck_CE_Explicit_Raise);
   pragma No_Return (Rcheck_CE_Index_Check);
   pragma No_Return (Rcheck_CE_Invalid_Data);
   pragma No_Return (Rcheck_CE_Length_Check);
   pragma No_Return (Rcheck_CE_Null_Exception_Id);
   pragma No_Return (Rcheck_CE_Null_Not_Allowed);
   pragma No_Return (Rcheck_CE_Overflow_Check);
   pragma No_Return (Rcheck_CE_Partition_Check);
   pragma No_Return (Rcheck_CE_Range_Check);
   pragma No_Return (Rcheck_CE_Tag_Check);
   pragma No_Return (Rcheck_PE_Access_Before_Elaboration);
   pragma No_Return (Rcheck_PE_Accessibility_Check);
   pragma No_Return (Rcheck_PE_Address_Of_Intrinsic);
   pragma No_Return (Rcheck_PE_Aliased_Parameters);
   pragma No_Return (Rcheck_PE_All_Guards_Closed);
   pragma No_Return (Rcheck_PE_Bad_Predicated_Generic_Type);
   pragma No_Return (Rcheck_PE_Current_Task_In_Entry_Body);
   pragma No_Return (Rcheck_PE_Duplicated_Entry_Address);
   pragma No_Return (Rcheck_PE_Explicit_Raise);
   pragma No_Return (Rcheck_PE_Implicit_Return);
   pragma No_Return (Rcheck_PE_Misaligned_Address_Value);
   pragma No_Return (Rcheck_PE_Missing_Return);
   pragma No_Return (Rcheck_PE_Overlaid_Controlled_Object);
   pragma No_Return (Rcheck_PE_Potentially_Blocking_Operation);
   pragma No_Return (Rcheck_PE_Stubbed_Subprogram_Called);
   pragma No_Return (Rcheck_PE_Unchecked_Union_Restriction);
   pragma No_Return (Rcheck_PE_Non_Transportable_Actual);
   pragma No_Return (Rcheck_PE_Finalize_Raised_Exception);
   pragma No_Return (Rcheck_SE_Empty_Storage_Pool);
   pragma No_Return (Rcheck_SE_Explicit_Raise);
   pragma No_Return (Rcheck_SE_Infinite_Recursion);
   pragma No_Return (Rcheck_SE_Object_Too_Large);

   pragma No_Return (Rcheck_CE_Access_Check_Ext);
   pragma No_Return (Rcheck_CE_Index_Check_Ext);
   pragma No_Return (Rcheck_CE_Invalid_Data_Ext);
   pragma No_Return (Rcheck_CE_Range_Check_Ext);

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
   Rmsg_08 : constant String := "null Exception_Id"                & NUL;
   Rmsg_09 : constant String := "null-exclusion check failed"      & NUL;
   Rmsg_10 : constant String := "overflow check failed"            & NUL;
   Rmsg_11 : constant String := "partition check failed"           & NUL;
   Rmsg_12 : constant String := "range check failed"               & NUL;
   Rmsg_13 : constant String := "tag check failed"                 & NUL;
   Rmsg_14 : constant String := "access before elaboration"        & NUL;
   Rmsg_15 : constant String := "accessibility check failed"       & NUL;
   Rmsg_16 : constant String := "attempt to take address of"       &
                                " intrinsic subprogram"            & NUL;
   Rmsg_17 : constant String := "aliased parameters"               & NUL;
   Rmsg_18 : constant String := "all guards closed"                & NUL;
   Rmsg_19 : constant String := "improper use of generic subtype"  &
                                " with predicate"                  & NUL;
   Rmsg_20 : constant String := "Current_Task referenced in entry" &
                                " body"                            & NUL;
   Rmsg_21 : constant String := "duplicated entry address"         & NUL;
   Rmsg_22 : constant String := "explicit raise"                   & NUL;
   Rmsg_23 : constant String := "finalize/adjust raised exception" & NUL;
   Rmsg_24 : constant String := "implicit return with No_Return"   & NUL;
   Rmsg_25 : constant String := "misaligned address value"         & NUL;
   Rmsg_26 : constant String := "missing return"                   & NUL;
   Rmsg_27 : constant String := "overlaid controlled object"       & NUL;
   Rmsg_28 : constant String := "potentially blocking operation"   & NUL;
   Rmsg_29 : constant String := "stubbed subprogram called"        & NUL;
   Rmsg_30 : constant String := "unchecked union restriction"      & NUL;
   Rmsg_31 : constant String := "actual/returned class-wide"       &
                                " value not transportable"         & NUL;
   Rmsg_32 : constant String := "empty storage pool"               & NUL;
   Rmsg_33 : constant String := "explicit raise"                   & NUL;
   Rmsg_34 : constant String := "infinite recursion"               & NUL;
   Rmsg_35 : constant String := "object too large"                 & NUL;

   -----------------------
   -- Polling Interface --
   -----------------------

   type Unsigned is mod 2 ** 32;

   Counter : Unsigned := 0;
   pragma Warnings (Off, Counter);
   --  This counter is provided for convenience. It can be used in Poll to
   --  perform periodic but not systematic operations.

   procedure Poll is separate;
   --  The actual polling routine is separate, so that it can easily
   --  be replaced with a target dependent version.

   --------------------------
   -- Code_Address_For_AAA --
   --------------------------

   --  This function gives us the start of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keep all the
   --  procedures in their original order!

   function Code_Address_For_AAA return System.Address is
   begin
      --  We are using a label instead of merely using
      --  Code_Address_For_AAA'Address because on some platforms the latter
      --  does not yield the address we want, but the address of a stub or of
      --  a descriptor instead. This is the case at least on Alpha-VMS and
      --  PA-HPUX.

      <<Start_Of_AAA>>
      return Start_Of_AAA'Address;
   end Code_Address_For_AAA;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Excep : EOA) is separate;
   --  The actual Call_Chain routine is separate, so that it can easily
   --  be dummied out when no exception traceback information is needed.

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

   function EId_To_String (X : Exception_Id) return String
     renames Stream_Attributes.EId_To_String;

   ------------------
   -- EO_To_String --
   ------------------

   --  We use the null string to represent the null occurrence, otherwise
   --  we output the Exception_Information string for the occurrence.

   function EO_To_String (X : Exception_Occurrence) return String
     renames Stream_Attributes.EO_To_String;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X : Exception_Occurrence) return Exception_Id
   is
   begin
      --  Note that the following test used to be here for the original
      --  Ada 95 semantics, but these were modified by AI-241 to require
      --  returning Null_Id instead of raising Constraint_Error.

      --  if X.Id = Null_Id then
      --     raise Constraint_Error;
      --  end if;

      return X.Id;
   end Exception_Identity;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return Exception_Data.Exception_Information (X);
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

      return To_Ptr (Id.Full_Name) (1 .. Id.Name_Length - 1);
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

      --  Return result making sure lower bound is 1

      declare
         subtype Rname is String (1 .. Name'Length - P + 1);
      begin
         return Rname (Name (P .. Name'Length));
      end;
   end Exception_Name_Simple;

   --------------------
   -- Exception_Data --
   --------------------

   package body Exception_Data is separate;
   --  This package can be easily dummied out if we do not want the
   --  basic support for exception messages (such as in Ada 83).

   ---------------------------
   -- Exception_Propagation --
   ---------------------------

   package body Exception_Propagation is separate;
   --  Depending on the actual exception mechanism used (front-end or
   --  back-end based), the implementation will differ, which is why this
   --  package is separated.

   ----------------------
   -- Exception_Traces --
   ----------------------

   package body Exception_Traces is separate;
   --  Depending on the underlying support for IO the implementation
   --  will differ. Moreover we would like to dummy out this package
   --  in case we do not want any exception tracing support. This is
   --  why this package is separated.

   --------------------------------------
   -- Get_Exception_Machine_Occurrence --
   --------------------------------------

   function Get_Exception_Machine_Occurrence
     (X : Exception_Occurrence) return System.Address
   is
   begin
      return X.Machine_Occurrence;
   end Get_Exception_Machine_Occurrence;

   -----------
   -- Image --
   -----------

   function Image (Index : Integer) return String is
      Result : constant String := Integer'Image (Index);
   begin
      if Result (1) = ' ' then
         return Result (2 .. Result'Last);
      else
         return Result;
      end if;
   end Image;

   -----------------------
   -- Stream Attributes --
   -----------------------

   package body Stream_Attributes is separate;
   --  This package can be easily dummied out if we do not want the
   --  support for streaming Exception_Ids and Exception_Occurrences.

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error (File : System.Address; Line : Integer) is
   begin
      Raise_With_Location_And_Msg (Constraint_Error_Def'Access, File, Line);
   end Raise_Constraint_Error;

   --------------------------------
   -- Raise_Constraint_Error_Msg --
   --------------------------------

   procedure Raise_Constraint_Error_Msg
     (File   : System.Address;
      Line   : Integer;
      Column : Integer;
      Msg    : System.Address)
   is
   begin
      Raise_With_Location_And_Msg
        (Constraint_Error_Def'Access, File, Line, Column, Msg);
   end Raise_Constraint_Error_Msg;

   -------------------------
   -- Complete_Occurrence --
   -------------------------

   procedure Complete_Occurrence (X : EOA) is
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

      Call_Chain (X);

      --  Notify the debugger
      Debug_Raise_Exception (E => SSL.Exception_Data_Ptr (X.Id));
   end Complete_Occurrence;

   ---------------------------------------
   -- Complete_And_Propagate_Occurrence --
   ---------------------------------------

   procedure Complete_And_Propagate_Occurrence (X : EOA) is
   begin
      Complete_Occurrence (X);
      Exception_Propagation.Propagate_Exception (X);
   end Complete_And_Propagate_Occurrence;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : Exception_Id;
      Message : String := "")
   is
      EF : Exception_Id := E;
   begin
      --  Raise CE if E = Null_ID (AI-446)

      if E = null then
         EF := Constraint_Error'Identity;
      end if;

      --  Go ahead and raise appropriate exception

      Raise_Exception_Always (EF, Message);
   end Raise_Exception;

   ----------------------------
   -- Raise_Exception_Always --
   ----------------------------

   procedure Raise_Exception_Always
     (E       : Exception_Id;
      Message : String := "")
   is
      X : constant EOA := Exception_Propagation.Allocate_Occurrence;

   begin
      Exception_Data.Set_Exception_Msg (X, E, Message);

      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Complete_And_Propagate_Occurrence (X);
   end Raise_Exception_Always;

   ------------------------------
   -- Raise_Exception_No_Defer --
   ------------------------------

   procedure Raise_Exception_No_Defer
     (E       : Exception_Id;
      Message : String := "")
   is
      X : constant EOA := Exception_Propagation.Allocate_Occurrence;
   begin
      Exception_Data.Set_Exception_Msg (X, E, Message);

      --  Do not call Abort_Defer.all, as specified by the spec

      Complete_And_Propagate_Occurrence (X);
   end Raise_Exception_No_Defer;

   -------------------------------------
   -- Raise_From_Controlled_Operation --
   -------------------------------------

   procedure Raise_From_Controlled_Operation
     (X : Ada.Exceptions.Exception_Occurrence)
   is
      Prefix             : constant String := "adjust/finalize raised ";
      Orig_Msg           : constant String := Exception_Message (X);
      Orig_Prefix_Length : constant Natural :=
        Integer'Min (Prefix'Length, Orig_Msg'Length);
      Orig_Prefix        : String renames Orig_Msg
        (Orig_Msg'First ..
         Orig_Msg'First + Orig_Prefix_Length - 1);
   begin
      --  Message already has the proper prefix, just re-raise

      if Orig_Prefix = Prefix then
         Raise_Exception_No_Defer
           (E       => Program_Error'Identity,
            Message => Orig_Msg);

      else
         declare
            New_Msg  : constant String := Prefix & Exception_Name (X);

         begin
            --  No message present, just provide our own

            if Orig_Msg = "" then
               Raise_Exception_No_Defer
                 (E       => Program_Error'Identity,
                  Message => New_Msg);

            --  Message present, add informational prefix

            else
               Raise_Exception_No_Defer
                 (E       => Program_Error'Identity,
                  Message => New_Msg & ": " & Orig_Msg);
            end if;
         end;
      end if;
   end Raise_From_Controlled_Operation;

   -------------------------------------------
   -- Create_Occurrence_From_Signal_Handler --
   -------------------------------------------

   function Create_Occurrence_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address) return EOA
   is
      X : constant EOA := Exception_Propagation.Allocate_Occurrence;

   begin
      Exception_Data.Set_Exception_C_Msg (X, E, M);

      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Complete_Occurrence (X);
      return X;
   end Create_Occurrence_From_Signal_Handler;

   ---------------------------------------------------
   -- Create_Machine_Occurrence_From_Signal_Handler --
   ---------------------------------------------------

   function Create_Machine_Occurrence_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address) return System.Address
   is
   begin
      return Create_Occurrence_From_Signal_Handler (E, M).Machine_Occurrence;
   end Create_Machine_Occurrence_From_Signal_Handler;

   -------------------------------
   -- Raise_From_Signal_Handler --
   -------------------------------

   procedure Raise_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address)
   is
   begin
      Exception_Propagation.Propagate_Exception
        (Create_Occurrence_From_Signal_Handler (E, M));
   end Raise_From_Signal_Handler;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error
     (File : System.Address;
      Line : Integer)
   is
   begin
      Raise_With_Location_And_Msg (Program_Error_Def'Access, File, Line);
   end Raise_Program_Error;

   -----------------------------
   -- Raise_Program_Error_Msg --
   -----------------------------

   procedure Raise_Program_Error_Msg
     (File : System.Address;
      Line : Integer;
      Msg  : System.Address)
   is
   begin
      Raise_With_Location_And_Msg
        (Program_Error_Def'Access, File, Line, M => Msg);
   end Raise_Program_Error_Msg;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error
     (File : System.Address;
      Line : Integer)
   is
   begin
      Raise_With_Location_And_Msg (Storage_Error_Def'Access, File, Line);
   end Raise_Storage_Error;

   -----------------------------
   -- Raise_Storage_Error_Msg --
   -----------------------------

   procedure Raise_Storage_Error_Msg
     (File : System.Address;
      Line : Integer;
      Msg  : System.Address)
   is
   begin
      Raise_With_Location_And_Msg
        (Storage_Error_Def'Access, File, Line, M => Msg);
   end Raise_Storage_Error_Msg;

   ---------------------------------
   -- Raise_With_Location_And_Msg --
   ---------------------------------

   procedure Raise_With_Location_And_Msg
     (E : Exception_Id;
      F : System.Address;
      L : Integer;
      C : Integer := 0;
      M : System.Address := System.Null_Address)
   is
      X : constant EOA := Exception_Propagation.Allocate_Occurrence;
   begin
      Exception_Data.Set_Exception_C_Msg (X, E, F, L, C, M);

      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Complete_And_Propagate_Occurrence (X);
   end Raise_With_Location_And_Msg;

   --------------------
   -- Raise_With_Msg --
   --------------------

   procedure Raise_With_Msg (E : Exception_Id) is
      Excep : constant EOA := Exception_Propagation.Allocate_Occurrence;
      Ex    : constant Exception_Occurrence_Access := Get_Current_Excep.all;
   begin
      Excep.Exception_Raised := False;
      Excep.Id               := E;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;

      --  Copy the message from the current exception
      --  Change the interface to be called with an occurrence ???

      Excep.Msg_Length                  := Ex.Msg_Length;
      Excep.Msg (1 .. Excep.Msg_Length) := Ex.Msg (1 .. Ex.Msg_Length);

      --  The following is a common pattern, should be abstracted
      --  into a procedure call ???

      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Complete_And_Propagate_Occurrence (Excep);
   end Raise_With_Msg;

   --------------------------------------
   -- Calls to Run-Time Check Routines --
   --------------------------------------

   procedure Rcheck_CE_Access_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_00'Address);
   end Rcheck_CE_Access_Check;

   procedure Rcheck_CE_Null_Access_Parameter
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_01'Address);
   end Rcheck_CE_Null_Access_Parameter;

   procedure Rcheck_CE_Discriminant_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_02'Address);
   end Rcheck_CE_Discriminant_Check;

   procedure Rcheck_CE_Divide_By_Zero
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_03'Address);
   end Rcheck_CE_Divide_By_Zero;

   procedure Rcheck_CE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_04'Address);
   end Rcheck_CE_Explicit_Raise;

   procedure Rcheck_CE_Index_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_05'Address);
   end Rcheck_CE_Index_Check;

   procedure Rcheck_CE_Invalid_Data
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_06'Address);
   end Rcheck_CE_Invalid_Data;

   procedure Rcheck_CE_Length_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_07'Address);
   end Rcheck_CE_Length_Check;

   procedure Rcheck_CE_Null_Exception_Id
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_08'Address);
   end Rcheck_CE_Null_Exception_Id;

   procedure Rcheck_CE_Null_Not_Allowed
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_09'Address);
   end Rcheck_CE_Null_Not_Allowed;

   procedure Rcheck_CE_Overflow_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_10'Address);
   end Rcheck_CE_Overflow_Check;

   procedure Rcheck_CE_Partition_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_11'Address);
   end Rcheck_CE_Partition_Check;

   procedure Rcheck_CE_Range_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_12'Address);
   end Rcheck_CE_Range_Check;

   procedure Rcheck_CE_Tag_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, 0, Rmsg_13'Address);
   end Rcheck_CE_Tag_Check;

   procedure Rcheck_PE_Access_Before_Elaboration
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_14'Address);
   end Rcheck_PE_Access_Before_Elaboration;

   procedure Rcheck_PE_Accessibility_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_15'Address);
   end Rcheck_PE_Accessibility_Check;

   procedure Rcheck_PE_Address_Of_Intrinsic
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_16'Address);
   end Rcheck_PE_Address_Of_Intrinsic;

   procedure Rcheck_PE_Aliased_Parameters
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_17'Address);
   end Rcheck_PE_Aliased_Parameters;

   procedure Rcheck_PE_All_Guards_Closed
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_18'Address);
   end Rcheck_PE_All_Guards_Closed;

   procedure Rcheck_PE_Bad_Predicated_Generic_Type
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_19'Address);
   end Rcheck_PE_Bad_Predicated_Generic_Type;

   procedure Rcheck_PE_Current_Task_In_Entry_Body
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_20'Address);
   end Rcheck_PE_Current_Task_In_Entry_Body;

   procedure Rcheck_PE_Duplicated_Entry_Address
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_21'Address);
   end Rcheck_PE_Duplicated_Entry_Address;

   procedure Rcheck_PE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_22'Address);
   end Rcheck_PE_Explicit_Raise;

   procedure Rcheck_PE_Implicit_Return
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_24'Address);
   end Rcheck_PE_Implicit_Return;

   procedure Rcheck_PE_Misaligned_Address_Value
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_25'Address);
   end Rcheck_PE_Misaligned_Address_Value;

   procedure Rcheck_PE_Missing_Return
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_26'Address);
   end Rcheck_PE_Missing_Return;

   procedure Rcheck_PE_Overlaid_Controlled_Object
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_27'Address);
   end Rcheck_PE_Overlaid_Controlled_Object;

   procedure Rcheck_PE_Potentially_Blocking_Operation
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_28'Address);
   end Rcheck_PE_Potentially_Blocking_Operation;

   procedure Rcheck_PE_Stubbed_Subprogram_Called
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_29'Address);
   end Rcheck_PE_Stubbed_Subprogram_Called;

   procedure Rcheck_PE_Unchecked_Union_Restriction
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_30'Address);
   end Rcheck_PE_Unchecked_Union_Restriction;

   procedure Rcheck_PE_Non_Transportable_Actual
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error_Msg (File, Line, Rmsg_31'Address);
   end Rcheck_PE_Non_Transportable_Actual;

   procedure Rcheck_SE_Empty_Storage_Pool
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error_Msg (File, Line, Rmsg_32'Address);
   end Rcheck_SE_Empty_Storage_Pool;

   procedure Rcheck_SE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error_Msg (File, Line, Rmsg_33'Address);
   end Rcheck_SE_Explicit_Raise;

   procedure Rcheck_SE_Infinite_Recursion
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error_Msg (File, Line, Rmsg_34'Address);
   end Rcheck_SE_Infinite_Recursion;

   procedure Rcheck_SE_Object_Too_Large
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error_Msg (File, Line, Rmsg_35'Address);
   end Rcheck_SE_Object_Too_Large;

   procedure Rcheck_CE_Access_Check_Ext
     (File : System.Address; Line, Column : Integer)
   is
   begin
      Raise_Constraint_Error_Msg (File, Line, Column, Rmsg_00'Address);
   end Rcheck_CE_Access_Check_Ext;

   procedure Rcheck_CE_Index_Check_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer)
   is
      Msg : constant String :=
        Rmsg_05 (Rmsg_05'First .. Rmsg_05'Last - 1) & ASCII.LF &
        "index " & Image (Index) & " not in " & Image (First) &
        ".." & Image (Last) & ASCII.NUL;
   begin
      Raise_Constraint_Error_Msg (File, Line, Column, Msg'Address);
   end Rcheck_CE_Index_Check_Ext;

   procedure Rcheck_CE_Invalid_Data_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer)
   is
      Msg : constant String :=
        Rmsg_06 (Rmsg_06'First .. Rmsg_06'Last - 1) & ASCII.LF &
        "value " & Image (Index) & " not in " & Image (First) &
        ".." & Image (Last) & ASCII.NUL;
   begin
      Raise_Constraint_Error_Msg (File, Line, Column, Msg'Address);
   end Rcheck_CE_Invalid_Data_Ext;

   procedure Rcheck_CE_Range_Check_Ext
     (File : System.Address; Line, Column, Index, First, Last : Integer)
   is
      Msg : constant String :=
        Rmsg_12 (Rmsg_12'First .. Rmsg_12'Last - 1) & ASCII.LF &
        "value " & Image (Index) & " not in " & Image (First) &
        ".." & Image (Last) & ASCII.NUL;
   begin
      Raise_Constraint_Error_Msg (File, Line, Column, Msg'Address);
   end Rcheck_CE_Range_Check_Ext;

   procedure Rcheck_PE_Finalize_Raised_Exception
     (File : System.Address; Line : Integer)
   is
      X : constant EOA := Exception_Propagation.Allocate_Occurrence;

   begin
      --  This is "finalize/adjust raised exception". This subprogram is always
      --  called with abort deferred, unlike all other Rcheck_* routines, it
      --  needs to call Raise_Exception_No_Defer.

      --  This is consistent with Raise_From_Controlled_Operation

      Exception_Data.Set_Exception_C_Msg
        (X, Program_Error_Def'Access, File, Line, 0, Rmsg_23'Address);
      Complete_And_Propagate_Occurrence (X);
   end Rcheck_PE_Finalize_Raised_Exception;

   -------------
   -- Reraise --
   -------------

   procedure Reraise is
      Excep    : constant EOA := Exception_Propagation.Allocate_Occurrence;
      Saved_MO : constant System.Address := Excep.Machine_Occurrence;
   begin
      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Save_Occurrence (Excep.all, Get_Current_Excep.all.all);
      Excep.Machine_Occurrence := Saved_MO;
      Complete_And_Propagate_Occurrence (Excep);
   end Reraise;

   --------------------------------------
   -- Reraise_Library_Exception_If_Any --
   --------------------------------------

   procedure Reraise_Library_Exception_If_Any is
      LE : Exception_Occurrence;
   begin
      if Library_Exception_Set then
         LE := Library_Exception;
         if LE.Id = Null_Id then
            Raise_Exception_No_Defer
              (E       => Program_Error'Identity,
               Message => "finalize/adjust raised exception");
         else
            Raise_From_Controlled_Operation (LE);
         end if;
      end if;
   end Reraise_Library_Exception_If_Any;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id = null then
         return;
      end if;

      Reraise_Occurrence_Always (X);
   end Reraise_Occurrence;

   -------------------------------
   -- Reraise_Occurrence_Always --
   -------------------------------

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence) is
   begin
      if not ZCX_By_Default then
         Abort_Defer.all;
      end if;

      Reraise_Occurrence_No_Defer (X);
   end Reraise_Occurrence_Always;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence) is
      Excep    : constant EOA := Exception_Propagation.Allocate_Occurrence;
      Saved_MO : constant System.Address := Excep.Machine_Occurrence;
   begin
      Save_Occurrence (Excep.all, X);
      Excep.Machine_Occurrence := Saved_MO;
      Complete_And_Propagate_Occurrence (Excep);
   end Reraise_Occurrence_No_Defer;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      --  As the machine occurrence might be a data that must be finalized
      --  (outside any Ada mechanism), do not copy it

      Target.Id                 := Source.Id;
      Target.Machine_Occurrence := System.Null_Address;
      Target.Msg_Length         := Source.Msg_Length;
      Target.Num_Tracebacks     := Source.Num_Tracebacks;
      Target.Pid                := Source.Pid;

      Target.Msg (1 .. Target.Msg_Length) :=
        Source.Msg (1 .. Target.Msg_Length);

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   function Save_Occurrence (Source : Exception_Occurrence) return EOA is
      Target : constant EOA := new Exception_Occurrence;
   begin
      Save_Occurrence (Target.all, Source);
      return Target;
   end Save_Occurrence;

   -------------------
   -- String_To_EId --
   -------------------

   function String_To_EId (S : String) return Exception_Id
     renames Stream_Attributes.String_To_EId;

   ------------------
   -- String_To_EO --
   ------------------

   function String_To_EO (S : String) return Exception_Occurrence
     renames Stream_Attributes.String_To_EO;

   ---------------
   -- To_Stderr --
   ---------------

   procedure To_Stderr (C : Character) is
      type int is new Integer;

      procedure put_char_stderr (C : int);
      pragma Import (C, put_char_stderr, "put_char_stderr");

   begin
      put_char_stderr (Character'Pos (C));
   end To_Stderr;

   procedure To_Stderr (S : String) is
   begin
      for J in S'Range loop
         if S (J) /= ASCII.CR then
            To_Stderr (S (J));
         end if;
      end loop;
   end To_Stderr;

   -------------------------
   -- Transfer_Occurrence --
   -------------------------

   procedure Transfer_Occurrence
     (Target : Exception_Occurrence_Access;
      Source : Exception_Occurrence)
   is
   begin
      Save_Occurrence (Target.all, Source);
   end Transfer_Occurrence;

   ------------------------
   -- Triggered_By_Abort --
   ------------------------

   function Triggered_By_Abort return Boolean is
      Ex : constant Exception_Occurrence_Access := Get_Current_Excep.all;

   begin
      return Ex /= null
        and then Exception_Identity (Ex.all) = Standard'Abort_Signal'Identity;
   end Triggered_By_Abort;

   -------------------------
   -- Wide_Exception_Name --
   -------------------------

   WC_Encoding : Character;
   pragma Import (C, WC_Encoding, "__gl_wc_encoding");
   --  Encoding method for source, as exported by binder

   function Wide_Exception_Name
     (Id : Exception_Id) return Wide_String
   is
      S : constant String := Exception_Name (Id);
      W : Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Exception_Name;

   function Wide_Exception_Name
     (X : Exception_Occurrence) return Wide_String
   is
      S : constant String := Exception_Name (X);
      W : Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Exception_Name;

   ----------------------------
   -- Wide_Wide_Exception_Name --
   -----------------------------

   function Wide_Wide_Exception_Name
     (Id : Exception_Id) return Wide_Wide_String
   is
      S : constant String := Exception_Name (Id);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Wide_Exception_Name;

   function Wide_Wide_Exception_Name
     (X : Exception_Occurrence) return Wide_Wide_String
   is
      S : constant String := Exception_Name (X);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Wide_Exception_Name;

   --------------------------
   -- Code_Address_For_ZZZ --
   --------------------------

   --  This function gives us the end of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keeps all the
   --  procedures in their original order!

   function Code_Address_For_ZZZ return System.Address is
   begin
      <<Start_Of_ZZZ>>
      return Start_Of_ZZZ'Address;
   end Code_Address_For_ZZZ;

end Ada.Exceptions;
