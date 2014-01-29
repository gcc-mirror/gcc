------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with ourself.

with System;
with System.Parameters;
with System.Standard_Library;
with System.Traceback_Entries;

package Ada.Exceptions is
   pragma Preelaborate_05;
   --  In accordance with Ada 2005 AI-362.

   type Exception_Id is private;
   pragma Preelaborable_Initialization (Exception_Id);

   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;
   pragma Preelaborable_Initialization (Exception_Occurrence);

   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name (Id : Exception_Id) return String;

   function Exception_Name (X : Exception_Occurrence) return String;

   function Wide_Exception_Name
     (Id : Exception_Id) return Wide_String;
   pragma Ada_05 (Wide_Exception_Name);

   function Wide_Exception_Name
     (X : Exception_Occurrence) return Wide_String;
   pragma Ada_05 (Wide_Exception_Name);

   function Wide_Wide_Exception_Name
     (Id : Exception_Id) return Wide_Wide_String;
   pragma Ada_05 (Wide_Wide_Exception_Name);

   function Wide_Wide_Exception_Name
     (X : Exception_Occurrence) return Wide_Wide_String;
   pragma Ada_05 (Wide_Wide_Exception_Name);

   procedure Raise_Exception (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception);
   --  Note: In accordance with AI-466, CE is raised if E = Null_Id

   function Exception_Message (X : Exception_Occurrence) return String;

   procedure Reraise_Occurrence (X : Exception_Occurrence);
   --  Note: it would be really nice to give a pragma No_Return for this
   --  procedure, but it would be wrong, since Reraise_Occurrence does return
   --  if the argument is the null exception occurrence. See also procedure
   --  Reraise_Occurrence_Always in the private part of this package.

   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;

   function Exception_Information (X : Exception_Occurrence) return String;
   --  The format of the exception information is as follows:
   --
   --    exception name (as in Exception_Name)
   --    message (or a null line if no message)
   --    PID=nnnn
   --    0xyyyyyyyy 0xyyyyyyyy ...
   --
   --  The lines are separated by a ASCII.LF character
   --
   --  The nnnn is the partition Id given as decimal digits
   --
   --  The 0x... line represents traceback program counter locations,
   --  in order with the first one being the exception location.

   --  Note on ordering: the compiler uses the Save_Occurrence procedure, but
   --  not the function from Rtsfind, so it is important that the procedure
   --  come first, since Rtsfind finds the first matching entity.

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence);

   function Save_Occurrence
     (Source : Exception_Occurrence)
      return   Exception_Occurrence_Access;

   --  Ada 2005 (AI-438): The language revision introduces the following
   --  subprograms and attribute definitions. We do not provide them
   --  explicitly. instead, the corresponding stream attributes are made
   --  available through a pragma Stream_Convert in the private part.

   --  procedure Read_Exception_Occurrence
   --    (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
   --     Item   : out Exception_Occurrence);

   --  procedure Write_Exception_Occurrence
   --    (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
   --     Item   : Exception_Occurrence);

   --  for Exception_Occurrence'Read use Read_Exception_Occurrence;
   --  for Exception_Occurrence'Write use Write_Exception_Occurrence;

private
   package SSL renames System.Standard_Library;
   package SP renames System.Parameters;

   subtype EOA is Exception_Occurrence_Access;

   Exception_Msg_Max_Length : constant := SP.Default_Exception_Msg_Max_Length;

   ------------------
   -- Exception_Id --
   ------------------

   subtype Code_Loc is System.Address;
   --  Code location used in building exception tables and for call addresses
   --  when propagating an exception. Values of this type are created by using
   --  Label'Address or extracted from machine states using Get_Code_Loc.

   Null_Loc : constant Code_Loc := System.Null_Address;
   --  Null code location, used to flag outer level frame

   type Exception_Id is new SSL.Exception_Data_Ptr;

   function EId_To_String (X : Exception_Id) return String;
   function String_To_EId (S : String) return Exception_Id;
   pragma Stream_Convert (Exception_Id, String_To_EId, EId_To_String);
   --  Functions for implementing Exception_Id stream attributes

   Null_Id : constant Exception_Id := null;

   -------------------------
   -- Private Subprograms --
   -------------------------

   function Current_Target_Exception return Exception_Occurrence;
   pragma Export
     (Ada, Current_Target_Exception,
      "__gnat_current_target_exception");
   --  This routine should return the current raised exception on targets which
   --  have built-in exception handling such as the Java Virtual Machine. For
   --  other targets this routine is simply ignored. Currently, only JGNAT
   --  uses this. See 4jexcept.ads for details. The pragma Export allows this
   --  routine to be accessed elsewhere in the run-time, even though it is in
   --  the private part of this package (it is not allowed to be in the visible
   --  part, since this is set by the reference manual).

   function Exception_Name_Simple (X : Exception_Occurrence) return String;
   --  Like Exception_Name, but returns the simple non-qualified name of the
   --  exception. This is used to implement the Exception_Name function in
   --  Current_Exceptions (the DEC compatible unit). It is called from the
   --  compiler generated code (using Rtsfind, which does not respect the
   --  private barrier, so we can place this function in the private part
   --  where the compiler can find it, but the spec is unchanged.)

   procedure Raise_Exception_Always (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception_Always);
   pragma Export (Ada, Raise_Exception_Always, "__gnat_raise_exception");
   --  This differs from Raise_Exception only in that the caller has determined
   --  that for sure the parameter E is not null, and that therefore no check
   --  for Null_Id is required. The expander converts Raise_Exception calls to
   --  Raise_Exception_Always if it can determine this is the case. The Export
   --  allows this routine to be accessed from Pure units.

   procedure Raise_From_Signal_Handler
     (E : Exception_Id;
      M : System.Address);
   pragma Export
     (Ada, Raise_From_Signal_Handler,
           "ada__exceptions__raise_from_signal_handler");
   pragma No_Return (Raise_From_Signal_Handler);
   --  This routine is used to raise an exception from a signal handler. The
   --  signal handler has already stored the machine state (i.e. the state that
   --  corresponds to the location at which the signal was raised). E is the
   --  Exception_Id specifying what exception is being raised, and M is a
   --  pointer to a null-terminated string which is the message to be raised.
   --  Note that this routine never returns, so it is permissible to simply
   --  jump to this routine, rather than call it. This may be appropriate for
   --  systems where the right way to get out of signal handler is to alter the
   --  PC value in the machine state or in some other way ask the operating
   --  system to return here rather than to the original location.

   procedure Raise_From_Controlled_Operation
     (X : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Raise_From_Controlled_Operation);
   pragma Export
     (Ada, Raise_From_Controlled_Operation,
           "__gnat_raise_from_controlled_operation");
   --  Raise Program_Error, providing information about X (an exception raised
   --  during a controlled operation) in the exception message.

   procedure Reraise_Library_Exception_If_Any;
   pragma Export
     (Ada, Reraise_Library_Exception_If_Any,
           "__gnat_reraise_library_exception_if_any");
   --  If there was an exception raised during library-level finalization,
   --  reraise the exception.

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_Always);
   --  This differs from Raise_Occurrence only in that the caller guarantees
   --  that for sure the parameter X is not the null occurrence, and that
   --  therefore this procedure cannot return. The expander uses this routine
   --  in the translation of a raise statement with no parameter (reraise).

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_No_Defer);
   --  Exactly like Reraise_Occurrence, except that abort is not deferred
   --  before the call and the parameter X is known not to be the null
   --  occurrence. This is used in generated code when it is known that abort
   --  is already deferred.

   function Triggered_By_Abort return Boolean;
   --  Determine whether the current exception (if it exists) is an instance of
   --  Standard'Abort_Signal.

   -----------------------
   -- Polling Interface --
   -----------------------

   --  The GNAT compiler has an option to generate polling calls to the Poll
   --  routine in this package. Specifying the -gnatP option for a compilation
   --  causes a call to Ada.Exceptions.Poll to be generated on every subprogram
   --  entry and on every iteration of a loop, thus avoiding the possibility of
   --  a case of unbounded time between calls.

   --  This polling interface may be used for instrumentation or debugging
   --  purposes (e.g. implementing watchpoints in software or in the debugger).

   --  In the GNAT technology itself, this interface is used to implement
   --  immediate asynchronous transfer of control and immediate abort on
   --  targets which do not provide for one thread interrupting another.

   --  Note: this used to be in a separate unit called System.Poll, but that
   --  caused horrible circular elaboration problems between System.Poll and
   --  Ada.Exceptions.

   procedure Poll;
   --  Check for asynchronous abort. Note that we do not inline the body.
   --  This makes the interface more useful for debugging purposes.

   --------------------------
   -- Exception_Occurrence --
   --------------------------

   package TBE renames System.Traceback_Entries;

   Max_Tracebacks : constant := 50;
   --  Maximum number of trace backs stored in exception occurrence

   type Tracebacks_Array is array (1 .. Max_Tracebacks) of TBE.Traceback_Entry;
   --  Traceback array stored in exception occurrence

   type Exception_Occurrence is record
      Id : Exception_Id;
      --  Exception_Identity for this exception occurrence

      Machine_Occurrence : System.Address;
      --  The underlying machine occurrence. For GCC, this corresponds to the
      --  _Unwind_Exception structure address.

      Msg_Length : Natural := 0;
      --  Length of message (zero = no message)

      Msg : String (1 .. Exception_Msg_Max_Length);
      --  Characters of message

      Exception_Raised : Boolean := False;
      --  Set to true to indicate that this exception occurrence has actually
      --  been raised. When an exception occurrence is first created, this is
      --  set to False, then when it is processed by Raise_Current_Exception,
      --  it is set to True. If Raise_Current_Exception is used to raise an
      --  exception for which this flag is already True, then it knows that
      --  it is dealing with the reraise case (which is useful to distinguish
      --  for exception tracing purposes).

      Pid : Natural := 0;
      --  Partition_Id for partition raising exception

      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      --  Number of traceback entries stored

      Tracebacks : Tracebacks_Array;
      --  Stored tracebacks (in Tracebacks (1 .. Num_Tracebacks))
   end record;

   function "=" (Left, Right : Exception_Occurrence) return Boolean
     is abstract;
   --  Don't allow comparison on exception occurrences, we should not need
   --  this, and it would not work right, because of the Msg and Tracebacks
   --  fields which have unused entries not copied by Save_Occurrence.

   function Get_Exception_Machine_Occurrence
     (X : Exception_Occurrence) return System.Address;
   pragma Export (Ada, Get_Exception_Machine_Occurrence,
                  "__gnat_get_exception_machine_occurrence");
   --  Get the machine occurrence corresponding to an exception occurrence.
   --  It is Null_Address if there is no machine occurrence (in runtimes that
   --  doesn't use GCC mechanism) or if it has been lost (Save_Occurrence
   --  doesn't save the machine occurrence).

   function EO_To_String (X : Exception_Occurrence) return String;
   function String_To_EO (S : String) return Exception_Occurrence;
   pragma Stream_Convert (Exception_Occurrence, String_To_EO, EO_To_String);
   --  Functions for implementing Exception_Occurrence stream attributes

   Null_Occurrence : constant Exception_Occurrence := (
     Id                 => null,
     Machine_Occurrence => System.Null_Address,
     Msg_Length         => 0,
     Msg                => (others => ' '),
     Exception_Raised   => False,
     Pid                => 0,
     Num_Tracebacks     => 0,
     Tracebacks         => (others => TBE.Null_TB_Entry));

end Ada.Exceptions;
