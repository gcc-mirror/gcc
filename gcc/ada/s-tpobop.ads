------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--    S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .     --
--                            O P E R A T I O N S                           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains all the extended primitives related to
--  Protected_Objects with entries.
--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the simple routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Entries.
--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Exceptions;
--  used for Exception_Id

with System.Tasking.Protected_Objects.Entries;

package System.Tasking.Protected_Objects.Operations is
   pragma Elaborate_Body;

   type Communication_Block is private;
   --  Objects of this type are passed between GNARL calls to allow RTS
   --  information to be preserved.

   procedure Protected_Entry_Call
     (Object             : Entries.Protection_Entries_Access;
      E                  : Protected_Entry_Index;
      Uninterpreted_Data : System.Address;
      Mode               : Call_Modes;
      Block              : out Communication_Block);
   --  Make a protected entry call to the specified object.
   --  Pend a protected entry call on the protected object represented
   --  by Object. A pended call is not queued; it may be executed immediately
   --  or queued, depending on the state of the entry barrier.
   --
   --    E
   --      The index representing the entry to be called.
   --
   --    Uninterpreted_Data
   --      This will be returned by Next_Entry_Call when this call is serviced.
   --      It can be used by the compiler to pass information between the
   --      caller and the server, in particular entry parameters.
   --
   --    Mode
   --      The kind of call to be pended
   --
   --    Block
   --      Information passed between runtime calls by the compiler

   procedure Timed_Protected_Entry_Call
     (Object                : Entries.Protection_Entries_Access;
      E                     : Protected_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Entry_Call_Successful : out Boolean);
      --  Same as the Protected_Entry_Call but with time-out specified.
      --  This routines is used when we do not use ATC mechanism to implement
      --  timed entry calls.

   procedure Service_Entries (Object : Entries.Protection_Entries_Access);
   pragma Inline (Service_Entries);

   procedure PO_Service_Entries
     (Self_ID : Task_ID;
      Object  : Entries.Protection_Entries_Access);
   --  Service all entry queues of the specified object, executing the
   --  corresponding bodies of any queued entry calls that are waiting
   --  on True barriers. This is used when the state of a protected
   --  object may have changed, in particular after the execution of
   --  the statement sequence of a protected procedure.
   --  Note that servicing an entry may change the value of one or more
   --  barriers, so this routine keeps checking barriers until all of
   --  them are closed.
   --
   --  This must be called with abortion deferred and with the corresponding
   --  object locked.

   procedure Complete_Entry_Body (Object : Entries.Protection_Entries_Access);
   --  Called from within an entry body procedure, indicates that the
   --  corresponding entry call has been serviced.

   procedure Exceptional_Complete_Entry_Body
     (Object : Entries.Protection_Entries_Access;
      Ex     : Ada.Exceptions.Exception_Id);
   --  Perform all of the functions of Complete_Entry_Body. In addition,
   --  report in Ex the exception whose propagation terminated the entry
   --  body to the runtime system.

   procedure Cancel_Protected_Entry_Call (Block : in out Communication_Block);
   --  Attempt to cancel the most recent protected entry call. If the call is
   --  not queued abortably, wait until it is or until it has completed.
   --  If the call is actually cancelled, the called object will be
   --  locked on return from this call. Get_Cancelled (Block) can be
   --  used to determine if the cancellation took place; there
   --  may be entries needing service in this case.
   --
   --  Block passes information between this and other runtime calls.

   function Enqueued (Block : Communication_Block) return Boolean;
   --  Returns True if the Protected_Entry_Call which returned the
   --  specified Block object was queued; False otherwise.

   function Cancelled (Block : Communication_Block) return Boolean;
   --  Returns True if the Protected_Entry_Call which returned the
   --  specified Block object was cancelled, False otherwise.

   procedure Requeue_Protected_Entry
     (Object     : Entries.Protection_Entries_Access;
      New_Object : Entries.Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean);
   --  If Object = New_Object, queue the protected entry call on Object
   --   currently being serviced on the queue corresponding to the entry
   --   represented by E.
   --
   --  If Object /= New_Object, transfer the call to New_Object.E,
   --   executing or queuing it as appropriate.
   --
   --  With_Abort---True if the call is to be queued abortably, false
   --   otherwise.

   procedure Requeue_Task_To_Protected_Entry
     (New_Object : Entries.Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean);
   --  Transfer task entry call currently being serviced to entry E
   --   on New_Object.
   --
   --  With_Abort---True if the call is to be queued abortably, false
   --   otherwise.

   function Protected_Count
     (Object : Entries.Protection_Entries'Class;
      E      : Protected_Entry_Index)
      return   Natural;
   --  Return the number of entry calls to E on Object.

   function Protected_Entry_Caller
     (Object : Entries.Protection_Entries'Class) return Task_ID;
   --  Return value of E'Caller, where E is the protected entry currently
   --  being handled. This will only work if called from within an entry
   --  body, as required by the LRM (C.7.1(14)).

   --  For internal use only:

   procedure PO_Do_Or_Queue
     (Self_ID    : Task_ID;
      Object     : Entries.Protection_Entries_Access;
      Entry_Call : Entry_Call_Link;
      With_Abort : Boolean);
   --  This procedure either executes or queues an entry call, depending
   --  on the status of the corresponding barrier. It assumes that abortion
   --  is deferred and that the specified object is locked.

private
   type Communication_Block is record
      Self      : Task_ID;
      Enqueued  : Boolean := True;
      Cancelled : Boolean := False;
   end record;
   pragma Volatile (Communication_Block);

   --  ?????
   --  The Communication_Block seems to be a relic.
   --  At the moment, the compiler seems to be generating
   --  unnecessary conditional code based on this block.
   --  See the code generated for async. select with task entry
   --  call for another way of solving this.

end System.Tasking.Protected_Objects.Operations;
