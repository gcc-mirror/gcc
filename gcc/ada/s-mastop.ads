------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      SYSTEM.MACHINE_STATE_OPERATIONS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with System.Exception_Tables.

with System.Storage_Elements;
with System.Exceptions;

package System.Machine_State_Operations is

   subtype Code_Loc is System.Address;
   --  Code location used in building exception tables and for call
   --  addresses when propagating an exception (also traceback table)
   --  Values of this type are created by using Label'Address or
   --  extracted from machine states using Get_Code_Loc.

   type Machine_State is new System.Address;
   --  The table based exception handling approach (see a-except.adb) isolates
   --  the target dependent aspects using an abstract data type interface
   --  to the type Machine_State, which is represented as a System.Address
   --  value (presumably implemented as a pointer to an appropriate record
   --  structure).

   function Machine_State_Length return System.Storage_Elements.Storage_Offset;
   --  Function to determine the length of the Storage_Array needed to hold
   --  a machine state. The machine state will always be maximally aligned.
   --  The value returned is a constant that will be used to allocate space
   --  for a machine state value.

   function Allocate_Machine_State return Machine_State;
   --  Allocate the required space for a Machine_State

   procedure Free_Machine_State (M : in out Machine_State);
   --  Free the dynamic memory taken by Machine_State

   --  The initial value of type Machine_State is created by the low level
   --  routine that actually raises an exception using the special builtin
   --  _builtin_machine_state. This value will typically encode the value
   --  of the program counter, and relevant registers. The following
   --  operations are defined on Machine_State values:

   function Get_Code_Loc (M : Machine_State) return Code_Loc;
   --  This function extracts the program counter value from a machine
   --  state, which the caller uses for searching the exception tables,
   --  and also for recording entries in the traceback table. The call
   --  returns a value of Null_Loc if the machine state represents the
   --  outer level, or some other frame for which no information can be
   --  provided.

   procedure Pop_Frame
     (M    : Machine_State;
      Info : System.Exceptions.Subprogram_Info_Type);
   --  This procedure pops the machine state M so that it represents the
   --  call point, as though the current subprogram had returned. It
   --  changes only the value referenced by M, and does not affect
   --  the current stack environment.
   --
   --  The Info parameter represents information generated by the backend
   --  (see description of Subprogram_Info node in sinfo.ads). This
   --  information is stored as static data during compilation. The
   --  caller then passes this information to Pop_Frame, which will
   --  use it to determine what must be changed in the machine state
   --  (e.g. which save-over-call registers must be restored, and from
   --  where on the stack frame they must be restored).
   --
   --  A value of No_Info for Info means either that the backend provided
   --  no information for current frame, or that the current frame is an
   --  other language frame for which no information exists, or that this
   --  is an outer level subprogram. In any case, Pop_Frame sets the code
   --  location to Null_Address when it pops past such a frame, and this
   --  is taken as an indication that the exception is unhandled.

   --  Note: at the current time, Info, if present is always a copy of
   --  the entry point of the procedure, as found by searching the
   --  subprogram table. For the case where a procedure is indeed in
   --  the table (either it is an Ada procedure, or a foreign procedure
   --  which is registered using pragma Propagate_Exceptions), then the
   --  entry point information will indeed be correct. It may well be
   --  possible for Pop_Frame to avoid using the Info parameter (for
   --  example if it consults auxiliary Dwarf tables to do its job).
   --  This is desirable if it can be done, because it means that it
   --  will work fine to propagate exceptions through unregistered
   --  foreign procedures. What will happen is that the search in the
   --  Ada subprogram table will find a junk entry. Even if this junk
   --  entry has an exception table, none of them will apply to the
   --  current location, so they will be ignored, and then Pop_Frame
   --  will be called to pop the frame. The Info parameter for this
   --  call will be junk, but if it is not used that does not matter.
   --  Note that the address recorded in the traceback table is of
   --  the exception location, so the traceback will be correct even
   --  in this case.

   procedure Enter_Handler
     (M       : Machine_State;
      Handler : System.Exceptions.Handler_Loc);
   --  When Propagate_Handler locates an applicable exception handler, it
   --  calls Enter_Handler, passing it two parameters. The first is the
   --  machine state that corresponds to what is required for entry to
   --  the handler, as computed by repeated Pop_Frame calls to reach the
   --  handler to be entered. The second is the code location for the
   --  handler itself which is the address of the label at the start of
   --  the handler code.
   --
   --  Note: The machine state M is likely stored on the part of the
   --  stack that will be popped by the call, so care must be taken
   --  not to pop the stack until the Machine_State is entirely read.
   --  The value passed as Handler was obtained from elaboration of
   --  an N_Handler_Loc node by the backend.

   function Fetch_Code (Loc : Code_Loc) return Code_Loc;
   --  Some architectures (notably VMS) use a descriptor to describe
   --  a subprogram address. This function computes the actual starting
   --  address of the code from Loc.
   --  Do not add pragma Inline, see 9116-002.
   --  ??? This function will go away when 'Code_Address is fixed on VMS.

   procedure Set_Machine_State (M : Machine_State);
   --  This routine sets M from the current machine state. It is called
   --  when an exception is initially signalled to initialize the state.

   procedure Set_Signal_Machine_State
     (M       : Machine_State;
      Context : System.Address);
   --  This routine sets M from the machine state that corresponds to the
   --  point in the code where a signal was raised. The parameter Context
   --  is a pointer to a structure created by the operating system when a
   --  signal is raised, and made available to the signal handler. The
   --  format of this context block, and the manner in which it is made
   --  available to the handler, are implementation dependent.

end System.Machine_State_Operations;
