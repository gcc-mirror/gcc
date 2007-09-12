------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This version of Ada.Exceptions is a full Ada 95 version. It omits Ada 2005
--  features such as the additional definitions of Exception_Name returning
--  Wide_[Wide_]String.

--  It is used for building the compiler and the basic tools, since these
--  builds may be done with bootstrap compilers that cannot handle these
--  additions. The full version of Ada.Exceptions can be found in the files
--  a-except-2005.ads/adb, and is used for all other builds where full Ada
--  2005 functionality is required. in particular, it is used for building
--  run times on all targets.

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with ourself.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

with System;
with System.Parameters;
with System.Standard_Library;
with System.Traceback_Entries;

package Ada.Exceptions is
   pragma Warnings (Off);
   pragma Preelaborate_05;
   pragma Warnings (On);
   --  We make this preelaborable in Ada 2005 mode. If we did not do this, then
   --  run time units used by the compiler (e.g. s-soflin.ads) would run
   --  into trouble. Conformance is not an issue, since this version is used
   --  only by the compiler.

   type Exception_Id is private;

   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;

   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name (X : Exception_Occurrence) return String;
   --  Same as Exception_Name (Exception_Identity (X))

   function Exception_Name (Id : Exception_Id) return String;

   procedure Raise_Exception (E : Exception_Id; Message : String := "");
   --  Note: it would be really nice to give a pragma No_Return for this
   --  procedure, but it would be wrong, since Raise_Exception does return if
   --  given the null exception in Ada 95 mode. However we do special case the
   --  name in the test in the compiler for issuing a warning for a missing
   --  return after this call. Program_Error seems reasonable enough in such a
   --  case. See also the routine Raise_Exception_Always in the private part.

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
   --  The nnnn is the partition Id given as decimal digits.
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
   --  This routine should return the current raised exception on targets
   --  which have built-in exception handling such as the Java Virtual
   --  Machine. For other targets this routine is simply ignored. Currently,
   --  only JGNAT uses this. See 4jexcept.ads for details. The pragma Export
   --  allows this routine to be accessed elsewhere in the run-time, even
   --  though it is in the private part of this package (it is not allowed
   --  to be in the visible part, since this is set by the reference manual).

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
   --  that for sure the parameter E is not null, and that therefore the call
   --  to this procedure cannot return. The expander converts Raise_Exception
   --  calls to Raise_Exception_Always if it can determine this is the case.
   --  The Export allows this routine to be accessed from Pure units.

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
   --  Raise Program_Error, proviving information about X (an exception
   --  raised during a controlled operation) in the exception message.

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
   --  occurrence. This is used in generated code when it is known that
   --  abort is already deferred.

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
   --  immediate aynschronous transfer of control and immediate abort on
   --  targets which do not provide for one thread interrupting another.

   --  Note: this used to be in a separate unit called System.Poll, but that
   --  caused horrible circular elaboration problems between System.Poll and
   --  Ada.Exceptions. One way of solving such circularities is unification!

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
      --  WARNING System.System.Finalization_Implementation.Finalize_List
      --  relies on the fact that this field is always first in the exception
      --  occurrence

      Msg_Length : Natural := 0;
      --  Length of message (zero = no message)

      Msg : String (1 .. Exception_Msg_Max_Length);
      --  Characters of message

      Cleanup_Flag : Boolean := False;
      --  The cleanup flag is normally False, it is set True for an exception
      --  occurrence passed to a cleanup routine, and will still be set True
      --  when the cleanup routine does a Reraise_Occurrence call using this
      --  exception occurrence. This is used to avoid recording a bogus trace
      --  back entry from this reraise call.

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

      Private_Data : System.Address := System.Null_Address;
      --  Field used by low level exception mechanism to store specific data.
      --  Currently used by the GCC exception mechanism to store a pointer to
      --  a GNAT_GCC_Exception.
   end record;

   function "=" (Left, Right : Exception_Occurrence) return Boolean
     is abstract;
   --  Don't allow comparison on exception occurrences, we should not need
   --  this, and it would not work right, because of the Msg and Tracebacks
   --  fields which have unused entries not copied by Save_Occurrence.

   function EO_To_String (X : Exception_Occurrence) return String;
   function String_To_EO (S : String) return Exception_Occurrence;
   pragma Stream_Convert (Exception_Occurrence, String_To_EO, EO_To_String);
   --  Functions for implementing Exception_Occurrence stream attributes

   Null_Occurrence : constant Exception_Occurrence := (
     Id               => null,
     Msg_Length       => 0,
     Msg              => (others => ' '),
     Cleanup_Flag     => False,
     Exception_Raised => False,
     Pid              => 0,
     Num_Tracebacks   => 0,
     Tracebacks       => (others => TBE.Null_TB_Entry),
     Private_Data     => System.Null_Address);

end Ada.Exceptions;
