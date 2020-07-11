------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASKING.PROTECTED_OBJECTS.SINGLE_ENTRY              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an optimized version of Protected_Objects.Operations
--  and Protected_Objects.Entries making the following assumptions:

--    PO have only one entry
--    There is only one caller at a time (No_Entry_Queue)
--    There is no dynamic priority support (No_Dynamic_Priorities)
--    No Abort Statements
--      (No_Abort_Statements, Max_Asynchronous_Select_Nesting => 0)
--    PO are at library level
--    None of the tasks will terminate (no need for finalization)

--  This interface is intended to be used in the Ravenscar profile, the
--  compiler is responsible for ensuring that the conditions mentioned above
--  are respected, except for the No_Entry_Queue restriction that is checked
--  dynamically in this package, since the check cannot be performed at compile
--  time, and is relatively cheap (see body).

--  This package is part of the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls
--  (aka GNARLI, GNU Ada Run-time Library Interface)

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes
--  in exp_ch9.adb and possibly exp_ch7.adb

package System.Tasking.Protected_Objects.Single_Entry is
   pragma Elaborate_Body;

   ---------------------------------
   -- Compiler Interface (GNARLI) --
   ---------------------------------

   --  The compiler will expand in the GNAT tree the following construct:

   --  protected PO is
   --     entry E;
   --     procedure P;
   --  private
   --     Open : Boolean := False;
   --  end PO;

   --  protected body PO is
   --     entry E when Open is
   --        ...variable declarations...
   --     begin
   --        ...B...
   --     end E;

   --     procedure P is
   --        ...variable declarations...
   --     begin
   --        ...C...
   --     end P;
   --  end PO;

   --  as follows:

   --  protected type poT is
   --     entry e;
   --     procedure p;
   --  private
   --     open : boolean := false;
   --  end poT;
   --  type poTV is limited record
   --     open : boolean := false;
   --     _object : aliased protection_entry;
   --  end record;
   --  procedure poPT__E1s (O : address; P : address; E :
   --    protected_entry_index);
   --  function poPT__B2s (O : address; E : protected_entry_index) return
   --    boolean;
   --  procedure poPT__pN (_object : in out poTV);
   --  procedure poPT__pP (_object : in out poTV);
   --  poTA : aliased entry_body := (
   --     barrier => poPT__B2s'unrestricted_access,
   --     action => poPT__E1s'unrestricted_access);
   --  freeze poTV [
   --     procedure poTVIP (_init : in out poTV) is
   --     begin
   --        _init.open := false;
   --        object-init-proc (_init._object);
   --        initialize_protection_entry (_init._object'unchecked_access,
   --          unspecified_priority, _init'address, poTA'
   --          unrestricted_access);
   --        return;
   --     end poTVIP;
   --  ]
   --  po : poT;
   --  poTVIP (poTV!(po));

   --  function poPT__B2s (O : address; E : protected_entry_index) return
   --    boolean is
   --     type poTVP is access poTV;
   --     _object : poTVP := poTVP!(O);
   --     poR : protection_entry renames _object._object;
   --     openP : boolean renames _object.open;
   --  begin
   --     return open;
   --  end poPT__B2s;

   --  procedure poPT__E1s (O : address; P : address; E :
   --    protected_entry_index) is
   --     type poTVP is access poTV;
   --     _object : poTVP := poTVP!(O);
   --  begin
   --     B1b : declare
   --        poR : protection_entry renames _object._object;
   --        openP : boolean renames _object.open;
   --        ...variable declarations...
   --     begin
   --        ...B...
   --     end B1b;
   --     complete_single_entry_body (_object._object'unchecked_access);
   --     return;
   --  exception
   --     when all others =>
   --        exceptional_complete_single_entry_body (_object._object'
   --          unchecked_access, get_gnat_exception);
   --        return;
   --  end poPT__E1s;

   --  procedure poPT__pN (_object : in out poTV) is
   --     poR : protection_entry renames _object._object;
   --     openP : boolean renames _object.open;
   --     ...variable declarations...
   --  begin
   --     ...C...
   --     return;
   --  end poPT__pN;

   --  procedure poPT__pP (_object : in out poTV) is
   --     procedure _clean is
   --     begin
   --        service_entry (_object._object'unchecked_access);
   --        return;
   --     end _clean;
   --  begin
   --     lock_entry (_object._object'unchecked_access);
   --     B5b : begin
   --        poPT__pN (_object);
   --     at end
   --        _clean;
   --     end B5b;
   --     return;
   --  end poPT__pP;

   type Protection_Entry is limited private;
   --  This type contains the GNARL state of a protected object. The
   --  application-defined portion of the state (i.e. private objects)
   --  is maintained by the compiler-generated code.

   type Protection_Entry_Access is access all Protection_Entry;

   type Entry_Body_Access is access constant Entry_Body;
   --  Access to barrier and action function of an entry

   procedure Initialize_Protection_Entry
     (Object           : Protection_Entry_Access;
      Ceiling_Priority : Integer;
      Compiler_Info    : System.Address;
      Entry_Body       : Entry_Body_Access);
   --  Initialize the Object parameter so that it can be used by the run time
   --  to keep track of the runtime state of a protected object.

   procedure Lock_Entry (Object : Protection_Entry_Access);
   --  Lock a protected object for write access. Upon return, the caller owns
   --  the lock to this object, and no other call to Lock or Lock_Read_Only
   --  with the same argument will return until the corresponding call to
   --  Unlock has been made by the caller.

   procedure Lock_Read_Only_Entry
     (Object : Protection_Entry_Access);
   --  Lock a protected object for read access. Upon return, the caller owns
   --  the lock for read access, and no other calls to Lock with the same
   --  argument will return until the corresponding call to Unlock has been
   --  made by the caller. Other calls to Lock_Read_Only may (but need not)
   --  return before the call to Unlock, and the corresponding callers will
   --  also own the lock for read access.

   procedure Unlock_Entry (Object : Protection_Entry_Access);
   --  Relinquish ownership of the lock for the object represented by the
   --  Object parameter. If this ownership was for write access, or if it was
   --  for read access where there are no other read access locks outstanding,
   --  one (or more, in the case of Lock_Read_Only) of the tasks waiting on
   --  this lock (if any) will be given the lock and allowed to return from
   --  the Lock or Lock_Read_Only call.

   procedure Service_Entry (Object : Protection_Entry_Access);
   --  Service the entry queue of the specified object, executing the
   --  corresponding body of any queued entry call that is waiting on True
   --  barrier. This is used when the state of a protected object may have
   --  changed, in particular after the execution of the statement sequence
   --  of a protected procedure.
   --
   --  This must be called with abort deferred and with the corresponding
   --  object locked. Object is unlocked on return.

   procedure Protected_Single_Entry_Call
     (Object              : Protection_Entry_Access;
      Uninterpreted_Data  : System.Address);
   --  Make a protected entry call to the specified object
   --
   --  Pends a protected entry call on the protected object represented by
   --  Object. A pended call is not queued; it may be executed immediately
   --  or queued, depending on the state of the entry barrier.
   --
   --    Uninterpreted_Data
   --      This will be returned by Next_Entry_Call when this call is serviced.
   --      It can be used by the compiler to pass information between the
   --      caller and the server, in particular entry parameters.

   procedure Exceptional_Complete_Single_Entry_Body
     (Object : Protection_Entry_Access;
      Ex     : Ada.Exceptions.Exception_Id);
   --  Perform all of the functions of Complete_Entry_Body. In addition, report
   --  in Ex the exception whose propagation terminated the entry body to the
   --  runtime system.

   function Protected_Count_Entry (Object : Protection_Entry) return Natural;
   --  Return the number of entry calls on Object (0 or 1)

   function Protected_Single_Entry_Caller
     (Object : Protection_Entry) return Task_Id;
   --  Return value of E'Caller, where E is the protected entry currently being
   --  handled. This will only work if called from within an entry body, as
   --  required by the LRM (C.7.1(14)).

private
   type Protection_Entry is record
      Common : aliased Protection;
      --  State of the protected object. This part is common to any protected
      --  object, including those without entries.

      Compiler_Info : System.Address;
      --  Pointer to compiler-generated record representing protected object

      Call_In_Progress : Entry_Call_Link;
      --  Pointer to the entry call being executed (if any)

      Entry_Body : Entry_Body_Access;
      --  Pointer to executable code for the entry body of the protected type

      Entry_Queue : Entry_Call_Link;
      --  Place to store the waiting entry call (if any)
   end record;

end System.Tasking.Protected_Objects.Single_Entry;
