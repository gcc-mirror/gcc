------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . R E N D E Z V O U S            --
--                                                                          --
--                                  S p e c                                 --
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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Exceptions;
--  Used for, Exception_Id

with System.Tasking.Protected_Objects.Entries;
--  used for Protection_Entries

package System.Tasking.Rendezvous is

   package STPE renames System.Tasking.Protected_Objects.Entries;

   procedure Task_Entry_Call
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean);
   --  General entry call used to implement ATC or conditional entry calls.
   --  Compiler interface only. Do not call from within the RTS.
   --  Acceptor is the ID of the acceptor task.
   --  E is the entry index requested.
   --  Uninterpreted_Data represents the parameters of the entry. It is
   --  constructed by the compiler for the caller and the callee; therefore,
   --  the run time never needs to decode this data.
   --  Mode can be either Asynchronous_Call (ATC) or Conditional_Call.
   --  Rendezvous_Successful is set to True on return if the call was serviced.

   procedure Timed_Task_Entry_Call
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Rendezvous_Successful : out Boolean);
   --  Timed entry call without using ATC.
   --  Compiler interface only. Do not call from within the RTS.
   --  See Task_Entry_Call for details on Acceptor, E and Uninterpreted_Data.
   --  Timeout is the value of the time out.
   --  Mode determines whether the delay is relative or absolute.

   procedure Call_Simple
     (Acceptor           : Task_ID;
      E                  : Task_Entry_Index;
      Uninterpreted_Data : System.Address);
   --  Simple entry call.
   --  Compiler interface only. Do not call from within the RTS.
   --
   --  source:
   --     T.E1 (Params);
   --
   --  expansion:
   --    declare
   --       P : parms := (parm1, parm2, parm3);
   --       X : Task_Entry_Index := 1;
   --    begin
   --       Call_Simple (t._task_id, X, P'Address);
   --       parm1 := P.param1;
   --       parm2 := P.param2;
   --       ...
   --    end;

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean);
   --  Cancel pending asynchronous task entry call.
   --  Compiler interface only. Do not call from within the RTS.
   --  See Exp_Ch9.Expand_N_Asynchronous_Select for code expansion.

   procedure Requeue_Task_Entry
     (Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean);
   --  Requeue from a task entry to a task entry.
   --  Compiler interface only. Do not call from within the RTS.
   --  The code generation for task entry requeues is different from that for
   --  protected entry requeues. There is a "goto" that skips around the call
   --  to Complete_Rendezvous, so that Requeue_Task_Entry must also do the work
   --  of Complete_Rendezvous. The difference is that it does not report that
   --  the call's State = Done.
   --
   --  source:
   --     accept e1 do
   --       ...A...
   --       requeue e2;
   --       ...B...
   --     end e1;
   --
   --  expansion:
   --     A62b : address;
   --     L61b : label
   --     begin
   --        accept_call (1, A62b);
   --        ...A...
   --        requeue_task_entry (tTV!(t)._task_id, 2, false);
   --        goto L61b;
   --        ...B...
   --        complete_rendezvous;
   --        <<L61b>>
   --     exception
   --        when others =>
   --           exceptional_complete_rendezvous (current_exception);
   --     end;

   procedure Requeue_Protected_To_Task_Entry
     (Object     : STPE.Protection_Entries_Access;
      Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean);
   --  Requeue from a protected entry to a task entry.
   --  Compiler interface only. Do not call from within the RTS.
   --
   --  source:
   --     entry e2 when b is
   --     begin
   --        b := false;
   --        ...A...
   --        requeue t.e2;
   --     end e2;
   --
   --  expansion:
   --     procedure rPT__E14b (O : address; P : address; E :
   --       protected_entry_index) is
   --        type rTVP is access rTV;
   --        freeze rTVP []
   --        _object : rTVP := rTVP!(O);
   --     begin
   --        declare
   --           rR : protection renames _object._object;
   --           vP : integer renames _object.v;
   --           bP : boolean renames _object.b;
   --        begin
   --           b := false;
   --           ...A...
   --           requeue_protected_to_task_entry (rR'unchecked_access, tTV!(t).
   --             _task_id, 2, false);
   --           return;
   --        end;
   --        complete_entry_body (_object._object'unchecked_access, objectF =>
   --          0);
   --        return;
   --     exception
   --        when others =>
   --           abort_undefer.all;
   --           exceptional_complete_entry_body (_object._object'
   --             unchecked_access, current_exception, objectF => 0);
   --           return;
   --     end rPT__E14b;

   procedure Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Index              : out Select_Index);
   --  Implement select statement.
   --  Compiler interface only. Do not call from within the RTS.
   --  See comments on Accept_Call.
   --
   --  source:
   --     select accept e1 do
   --           ...A...
   --        end e1;
   --        ...B...
   --     or accept e2;
   --        ...C...
   --     end select;
   --
   --  expansion:
   --     A32b : address;
   --     declare
   --        A37b : T36b;
   --        A37b (1) := (null_body => false, s => 1);
   --        A37b (2) := (null_body => true, s => 2);
   --        S0 : aliased T36b := accept_list'A37b;
   --        J1 : select_index := 0;
   --        procedure e1A is
   --        begin
   --           abort_undefer.all;
   --           ...A...
   --           <<L31b>>
   --           complete_rendezvous;
   --        exception
   --           when all others =>
   --              exceptional_complete_rendezvous (get_gnat_exception);
   --        end e1A;
   --     begin
   --        selective_wait (S0'unchecked_access, simple_mode, A32b, J1);
   --        case J1 is
   --           when 0 =>
   --              goto L3;
   --           when 1 =>
   --              e1A;
   --              goto L1;
   --           when 2 =>
   --              goto L2;
   --           when others =>
   --              goto L3;
   --        end case;
   --        <<L1>>
   --        ...B...
   --        goto L3;
   --        <<L2>>
   --        ...C...
   --        goto L3;
   --        <<L3>>
   --     end;

   procedure Timed_Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Timeout            : Duration;
      Mode               : Delay_Modes;
      Index              : out Select_Index);
   --  Selective wait with timeout without using ATC.
   --  Compiler interface only. Do not call from within the RTS.

   procedure Accept_Call
     (E                  : Task_Entry_Index;
      Uninterpreted_Data : out System.Address);
   --  Accept an entry call.
   --  Compiler interface only. Do not call from within the RTS.
   --
   --  source:
   --              accept E do  ...A... end E;
   --  expansion:
   --              A27b : address;
   --              L26b : label
   --              begin
   --                 accept_call (1, A27b);
   --                 ...A...
   --                 complete_rendezvous;
   --              <<L26b>>
   --              exception
   --              when all others =>
   --                 exceptional_complete_rendezvous (get_gnat_exception);
   --              end;
   --
   --  The handler for Abort_Signal (*all* others) is to handle the case when
   --  the acceptor is aborted between Accept_Call and the corresponding
   --  Complete_Rendezvous call. We need to wake up the caller in this case.
   --
   --   See also Selective_Wait

   procedure Accept_Trivial (E : Task_Entry_Index);
   --  Accept an entry call that has no parameters and no body.
   --  Compiler interface only. Do not call from within the RTS.
   --  This should only be called when there is no accept body, or the accept
   --  body is empty.
   --
   --  source:
   --               accept E;
   --  expansion:
   --               accept_trivial (1);
   --
   --  The compiler is also able to recognize the following and
   --  translate it the same way.
   --
   --     accept E do null; end E;

   function Task_Count (E : Task_Entry_Index) return Natural;
   --  Return number of tasks waiting on the entry E (of current task)
   --  Compiler interface only. Do not call from within the RTS.

   function Callable (T : Task_ID) return Boolean;
   --  Return T'Callable
   --  Compiler interface. Do not call from within the RTS, except for body of
   --  Ada.Task_Identification.

   type Task_Entry_Nesting_Depth is new Task_Entry_Index
     range 0 .. Max_Task_Entry;

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_ID;
   --  Return E'Caller. This will only work if called from within an
   --  accept statement that is handling E, as required by the LRM (C.7.1(14)).
   --  Compiler interface only. Do not call from within the RTS.

   procedure Complete_Rendezvous;
   --  Called by acceptor to wake up caller

   procedure Exceptional_Complete_Rendezvous
     (Ex : Ada.Exceptions.Exception_Id);
   --  Called by acceptor to mark the end of the current rendezvous and
   --  propagate an exception to the caller.

   --  For internal use only:

   function Task_Do_Or_Queue
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link;
      With_Abort : Boolean) return Boolean;
   --  Call this only with abort deferred and holding no locks, except
   --  the global RTS lock when Single_Lock is True which must be owned.
   --  Returns False iff the call cannot be served or queued, as is the
   --  case if the caller is not callable; i.e., a False return value
   --  indicates that Tasking_Error should be raised.
   --  Either initiate the entry call, such that the accepting task is
   --  free to execute the rendezvous, queue the call on the acceptor's
   --  queue, or cancel the call. Conditional calls that cannot be
   --  accepted immediately are cancelled.

end System.Tasking.Rendezvous;
