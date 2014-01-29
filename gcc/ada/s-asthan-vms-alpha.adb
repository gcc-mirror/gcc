------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                  S Y S T E M . A S T _ H A N D L I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2013, Free Software Foundation, Inc.         --
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

--  This is the OpenVMS/Alpha version

with System; use System;

with System.IO;

with System.Machine_Code;
with System.Parameters;
with System.Storage_Elements;

with System.Tasking;
with System.Tasking.Rendezvous;
with System.Tasking.Initialization;
with System.Tasking.Utilities;

with System.Task_Primitives;
with System.Task_Primitives.Operations;
with System.Task_Primitives.Operations.DEC;

with Ada.Finalization;
with Ada.Task_Attributes;

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body System.AST_Handling is

   package ATID renames Ada.Task_Identification;

   package SP   renames System.Parameters;
   package ST   renames System.Tasking;
   package STR  renames System.Tasking.Rendezvous;
   package STI  renames System.Tasking.Initialization;
   package STU  renames System.Tasking.Utilities;

   package SSE  renames System.Storage_Elements;
   package STPO renames System.Task_Primitives.Operations;
   package STPOD renames System.Task_Primitives.Operations.DEC;

   AST_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other AST tasks.  It is only used by Lock_AST and
   --  Unlock_AST.

   procedure Lock_AST (Self_ID : ST.Task_Id);
   --  Locks out other AST tasks. Preceding a section of code by Lock_AST and
   --  following it by Unlock_AST creates a critical region.

   procedure Unlock_AST (Self_ID : ST.Task_Id);
   --  Releases lock previously set by call to Lock_AST.
   --  All nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   --------------
   -- Lock_AST --
   --------------

   procedure Lock_AST (Self_ID : ST.Task_Id) is
   begin
      STI.Defer_Abort_Nestable (Self_ID);
      STPO.Write_Lock (AST_Lock'Access, Global_Lock => True);
   end Lock_AST;

   ----------------
   -- Unlock_AST --
   ----------------

   procedure Unlock_AST (Self_ID : ST.Task_Id) is
   begin
      STPO.Unlock (AST_Lock'Access, Global_Lock => True);
      STI.Undefer_Abort_Nestable (Self_ID);
   end Unlock_AST;

   ---------------------------------
   -- AST_Handler Data Structures --
   ---------------------------------

   --  As noted in the private part of the spec of System.Aux_DEC, the
   --  AST_Handler type is simply a pointer to a procedure that takes
   --  a single 64bit parameter. The following is a local copy
   --  of that definition.

   --  We need our own copy because we need to get our hands on this
   --  and we cannot see the private part of System.Aux_DEC. We don't
   --  want to be a child of Aux_Dec because of complications resulting
   --  from the use of pragma Extend_System. We will use unchecked
   --  conversions between the two versions of the declarations.

   type AST_Handler is access procedure (Param : Long_Integer);

   --  However, this declaration is somewhat misleading, since the values
   --  referenced by AST_Handler values (all produced in this package by
   --  calls to Create_AST_Handler) are highly stylized.

   --  The first point is that in VMS/Alpha, procedure pointers do not in
   --  fact point to code, but rather to a 48-byte procedure descriptor.
   --  So a value of type AST_Handler is in fact a pointer to one of these
   --  48-byte descriptors.

   type Descriptor_Type is new SSE.Storage_Array (1 .. 48);
   for  Descriptor_Type'Alignment use Standard'Maximum_Alignment;

   type Descriptor_Ref is access all Descriptor_Type;

   --  Normally, there is only one such descriptor for a given procedure, but
   --  it works fine to make a copy of the single allocated descriptor, and
   --  use the copy itself, and we take advantage of this in the design here.
   --  The idea is that AST_Handler values will all point to a record with the
   --  following structure:

   --  Note: When we say it works fine, there is one delicate point, which
   --  is that the code for the AST procedure itself requires the original
   --  descriptor address.  We handle this by saving the original descriptor
   --  address in this structure and restoring in Process_AST.

   type AST_Handler_Data is record
      Descriptor              : Descriptor_Type;
      Original_Descriptor_Ref : Descriptor_Ref;
      Taskid                  : ATID.Task_Id;
      Entryno                 : Natural;
   end record;

   type AST_Handler_Data_Ref is access all AST_Handler_Data;

   function To_AST_Handler is new Ada.Unchecked_Conversion
     (AST_Handler_Data_Ref, System.Aux_DEC.AST_Handler);

   --  Each time Create_AST_Handler is called, a new value of this record
   --  type is created, containing a copy of the procedure descriptor for
   --  the routine used to handle all AST's (Process_AST), and the Task_Id
   --  and entry number parameters identifying the task entry involved.

   --  The AST_Handler value returned is a pointer to this record. Since
   --  the record starts with the procedure descriptor, it can be used
   --  by the system in the normal way to call the procedure. But now
   --  when the procedure gets control, it can determine the address of
   --  the procedure descriptor used to call it (since the ABI specifies
   --  that this is left sitting in register r27 on entry), and then use
   --  that address to retrieve the Task_Id and entry number so that it
   --  knows on which entry to queue the AST request.

   --  The next issue is where are these records placed. Since we intend
   --  to pass pointers to these records to asynchronous system service
   --  routines, they have to be on the heap, which means we have to worry
   --  about when to allocate them and deallocate them.

   --  We solve this problem by introducing a task attribute that points to
   --  a vector, indexed by the entry number, of AST_Handler_Data records
   --  for a given task. The pointer itself is a controlled object allowing
   --  us to write a finalization routine that frees the referenced vector.

   --  An entry in this vector is either initialized (Entryno non-zero) and
   --  can be used for any subsequent reference to the same entry, or it is
   --  unused, marked by the Entryno value being zero.

   type AST_Handler_Vector is array (Natural range <>) of AST_Handler_Data;
   type AST_Handler_Vector_Ref is access all AST_Handler_Vector;

   type AST_Vector_Ptr is new Ada.Finalization.Controlled with record
      Vector : AST_Handler_Vector_Ref;
   end record;

   procedure Finalize (Obj : in out AST_Vector_Ptr);
   --  Override Finalize so that the AST Vector gets freed.

   procedure Finalize (Obj : in out AST_Vector_Ptr) is
      procedure Free is new
       Ada.Unchecked_Deallocation (AST_Handler_Vector, AST_Handler_Vector_Ref);
   begin
      if Obj.Vector /= null then
         Free (Obj.Vector);
      end if;
   end Finalize;

   AST_Vector_Init : AST_Vector_Ptr;
   --  Initial value, treated as constant, Vector will be null

   package AST_Attribute is new Ada.Task_Attributes
     (Attribute     => AST_Vector_Ptr,
      Initial_Value => AST_Vector_Init);

   use AST_Attribute;

   -----------------------
   -- AST Service Queue --
   -----------------------

   --  The following global data structures are used to queue pending
   --  AST requests. When an AST is signalled, the AST service routine
   --  Process_AST is called, and it makes an entry in this structure.

   type AST_Instance is record
      Taskid  : ATID.Task_Id;
      Entryno : Natural;
      Param   : Long_Integer;
   end record;
   --  The Taskid and Entryno indicate the entry on which this AST is to
   --  be queued, and Param is the parameter provided from the AST itself.

   AST_Service_Queue_Size  : constant := 256;
   AST_Service_Queue_Limit : constant := 250;
   type AST_Service_Queue_Index is mod AST_Service_Queue_Size;
   --  Index used to refer to entries in the circular buffer which holds
   --  active AST_Instance values. The upper bound reflects the maximum
   --  number of AST instances that can be stored in the buffer. Since
   --  these entries are immediately serviced by the high priority server
   --  task that does the actual entry queuing, it is very unusual to have
   --  any significant number of entries simultaneously queued.

   AST_Service_Queue : array (AST_Service_Queue_Index) of AST_Instance;
   pragma Volatile_Components (AST_Service_Queue);
   --  The circular buffer used to store active AST requests

   AST_Service_Queue_Put : AST_Service_Queue_Index := 0;
   AST_Service_Queue_Get : AST_Service_Queue_Index := 0;
   pragma Atomic (AST_Service_Queue_Put);
   pragma Atomic (AST_Service_Queue_Get);
   --  These two variables point to the next slots in the AST_Service_Queue
   --  to be used for putting a new entry in and taking an entry out. This
   --  is a circular buffer, so these pointers wrap around. If the two values
   --  are equal the buffer is currently empty. The pointers are atomic to
   --  ensure proper synchronization between the single producer (namely the
   --  Process_AST procedure), and the single consumer (the AST_Service_Task).

   --------------------------------
   -- AST Server Task Structures --
   --------------------------------

   --  The basic approach is that when an AST comes in, a call is made to
   --  the Process_AST procedure. It queues the request in the service queue
   --  and then wakes up an AST server task to perform the actual call to the
   --  required entry. We use this intermediate server task, since the AST
   --  procedure itself cannot wait to return, and we need some caller for
   --  the rendezvous so that we can use the normal rendezvous mechanism.

   --  It would work to have only one AST server task, but then we would lose
   --  all overlap in AST processing, and furthermore, we could get priority
   --  inversion effects resulting in starvation of AST requests.

   --  We therefore maintain a small pool of AST server tasks. We adjust
   --  the size of the pool dynamically to reflect traffic, so that we have
   --  a sufficient number of server tasks to avoid starvation.

   Max_AST_Servers : constant Natural := 16;
   --  Maximum number of AST server tasks that can be allocated

   Num_AST_Servers : Natural := 0;
   --  Number of AST server tasks currently active

   Num_Waiting_AST_Servers : Natural := 0;
   --  This is the number of AST server tasks that are either waiting for
   --  work, or just about to go to sleep and wait for work.

   Is_Waiting : array (1 .. Max_AST_Servers) of Boolean := (others => False);
   --  An array of flags showing which AST server tasks are currently waiting

   AST_Task_Ids : array (1 .. Max_AST_Servers) of ST.Task_Id;
   --  Task Id's of allocated AST server tasks

   task type AST_Server_Task (Num : Natural) is
      pragma Priority (Priority'Last);
   end AST_Server_Task;
   --  Declaration for AST server task. This task has no entries, it is
   --  controlled by sleep and wakeup calls at the task primitives level.

   type AST_Server_Task_Ptr is access all AST_Server_Task;
   --  Type used to allocate server tasks

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate_New_AST_Server;
   --  Allocate an additional AST server task

   procedure Process_AST (Param : Long_Integer);
   --  This is the central routine for processing all AST's, it is referenced
   --  as the code address of all created AST_Handler values. See detailed
   --  description in body to understand how it works to have a single such
   --  procedure for all AST's even though it does not get any indication of
   --  the entry involved passed as an explicit parameter. The single explicit
   --  parameter Param is the parameter passed by the system with the AST.

   -----------------------------
   -- Allocate_New_AST_Server --
   -----------------------------

   procedure Allocate_New_AST_Server is
      Dummy : AST_Server_Task_Ptr;
      pragma Unreferenced (Dummy);

   begin
      if Num_AST_Servers = Max_AST_Servers then
         return;

      else
         --  Note: it is safe to increment Num_AST_Servers immediately, since
         --  no one will try to activate this task until it indicates that it
         --  is sleeping by setting its entry in Is_Waiting to True.

         Num_AST_Servers := Num_AST_Servers + 1;
         Dummy := new AST_Server_Task (Num_AST_Servers);
      end if;
   end Allocate_New_AST_Server;

   ---------------------
   -- AST_Server_Task --
   ---------------------

   task body AST_Server_Task is
      Taskid  : ATID.Task_Id;
      Entryno : Natural;
      Param   : aliased Long_Integer;
      Self_Id : constant ST.Task_Id := ST.Self;

      pragma Volatile (Param);

   begin
      --  By making this task independent of master, when the environment
      --  task is finalizing, the AST_Server_Task will be notified that it
      --  should terminate.

      STU.Make_Independent;

      --  Record our task Id for access by Process_AST

      AST_Task_Ids (Num) := Self_Id;

      --  Note: this entire task operates with the main task lock set, except
      --  when it is sleeping waiting for work, or busy doing a rendezvous
      --  with an AST server. This lock protects the data structures that
      --  are shared by multiple instances of the server task.

      Lock_AST (Self_Id);

      --  This is the main infinite loop of the task. We go to sleep and
      --  wait to be woken up by Process_AST when there is some work to do.

      loop
         Num_Waiting_AST_Servers := Num_Waiting_AST_Servers + 1;

         Unlock_AST (Self_Id);

         STI.Defer_Abort (Self_Id);

         if SP.Single_Lock then
            STPO.Lock_RTS;
         end if;

         STPO.Write_Lock (Self_Id);

         Is_Waiting (Num) := True;

         Self_Id.Common.State := ST.AST_Server_Sleep;
         STPO.Sleep (Self_Id, ST.AST_Server_Sleep);
         Self_Id.Common.State := ST.Runnable;

         STPO.Unlock (Self_Id);

         if SP.Single_Lock then
            STPO.Unlock_RTS;
         end if;

         --  If the process is finalizing, Undefer_Abort will simply end
         --  this task.

         STI.Undefer_Abort (Self_Id);

         --  We are awake, there is something to do

         Lock_AST (Self_Id);
         Num_Waiting_AST_Servers := Num_Waiting_AST_Servers - 1;

         --  Loop here to service outstanding requests. We are always
         --  locked on entry to this loop.

         while AST_Service_Queue_Get /= AST_Service_Queue_Put loop
            Taskid  := AST_Service_Queue (AST_Service_Queue_Get).Taskid;
            Entryno := AST_Service_Queue (AST_Service_Queue_Get).Entryno;
            Param   := AST_Service_Queue (AST_Service_Queue_Get).Param;

            AST_Service_Queue_Get := AST_Service_Queue_Get + 1;

            --  This is a manual expansion of the normal call simple code

            declare
               type AA is access all Long_Integer;
               P : AA := Param'Unrestricted_Access;

               function To_ST_Task_Id is new Ada.Unchecked_Conversion
                 (ATID.Task_Id, ST.Task_Id);

            begin
               Unlock_AST (Self_Id);
               STR.Call_Simple
                 (Acceptor           => To_ST_Task_Id (Taskid),
                  E                  => ST.Task_Entry_Index (Entryno),
                  Uninterpreted_Data => P'Address);

            exception
               when E : others =>
                  System.IO.Put_Line ("%Debugging event");
                  System.IO.Put_Line (Exception_Name (E) &
                    " raised when trying to deliver an AST.");

                  if Exception_Message (E)'Length /= 0 then
                     System.IO.Put_Line (Exception_Message (E));
                  end if;

                  System.IO.Put_Line ("Task type is " & "Receiver_Type");
                  System.IO.Put_Line ("Task id is " & ATID.Image (Taskid));
            end;

            Lock_AST (Self_Id);
         end loop;
      end loop;
   end AST_Server_Task;

   ------------------------
   -- Create_AST_Handler --
   ------------------------

   function Create_AST_Handler
     (Taskid  : ATID.Task_Id;
      Entryno : Natural) return System.Aux_DEC.AST_Handler
   is
      Attr_Ref : Attribute_Handle;

      Process_AST_Ptr : constant AST_Handler := Process_AST'Access;
      --  Reference to standard procedure descriptor for Process_AST

      pragma Warnings (Off, "*alignment*");
      --  Suppress harmless warnings about alignment.
      --  Should explain why this warning is harmless ???

      function To_Descriptor_Ref is new Ada.Unchecked_Conversion
        (AST_Handler, Descriptor_Ref);

      Original_Descriptor_Ref : constant Descriptor_Ref :=
                                  To_Descriptor_Ref (Process_AST_Ptr);

      pragma Warnings (On, "*alignment*");

   begin
      if ATID.Is_Terminated (Taskid) then
         raise Program_Error;
      end if;

      Attr_Ref := Reference (Taskid);

      --  Allocate another server if supply is getting low

      if Num_Waiting_AST_Servers < 2 then
         Allocate_New_AST_Server;
      end if;

      --  No point in creating more if we have zillions waiting to
      --  be serviced.

      while AST_Service_Queue_Put - AST_Service_Queue_Get
         > AST_Service_Queue_Limit
      loop
         delay 0.01;
      end loop;

      --  If no AST vector allocated, or the one we have is too short, then
      --  allocate one of right size and initialize all entries except the
      --  one we will use to unused. Note that the assignment automatically
      --  frees the old allocated table if there is one.

      if Attr_Ref.Vector = null
        or else Attr_Ref.Vector'Length < Entryno
      then
         Attr_Ref.Vector := new AST_Handler_Vector (1 .. Entryno);

         for E in 1 .. Entryno loop
            Attr_Ref.Vector (E).Descriptor :=
              Original_Descriptor_Ref.all;
            Attr_Ref.Vector (E).Original_Descriptor_Ref :=
              Original_Descriptor_Ref;
            Attr_Ref.Vector (E).Taskid  := Taskid;
            Attr_Ref.Vector (E).Entryno := E;
         end loop;
      end if;

      return To_AST_Handler (Attr_Ref.Vector (Entryno)'Unrestricted_Access);
   end Create_AST_Handler;

   ----------------------------
   -- Expand_AST_Packet_Pool --
   ----------------------------

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : Natural;
      Actual_Number     : out Natural;
      Total_Number      : out Natural)
   is
      pragma Unreferenced (Requested_Packets);
   begin
      --  The AST implementation of GNAT does not permit dynamic expansion
      --  of the pool, so we simply add no entries and return the total. If
      --  it is necessary to expand the allocation, then this package body
      --  must be recompiled with a larger value for AST_Service_Queue_Size.

      Actual_Number := 0;
      Total_Number := AST_Service_Queue_Size;
   end Expand_AST_Packet_Pool;

   -----------------
   -- Process_AST --
   -----------------

   procedure Process_AST (Param : Long_Integer) is

      Handler_Data_Ptr : AST_Handler_Data_Ref;
      --  This variable is set to the address of the descriptor through
      --  which Process_AST is called. Since the descriptor is part of
      --  an AST_Handler value, this is also the address of this value,
      --  from which we can obtain the task and entry number information.

      function To_Address is new Ada.Unchecked_Conversion
        (ST.Task_Id, System.Task_Primitives.Task_Address);

   begin
      System.Machine_Code.Asm
        (Template => "addq $27,0,%0",
         Outputs  => AST_Handler_Data_Ref'Asm_Output ("=r", Handler_Data_Ptr),
         Volatile => True);

      System.Machine_Code.Asm
        (Template => "ldq $27,%0",
         Inputs  => Descriptor_Ref'Asm_Input
           ("m", Handler_Data_Ptr.Original_Descriptor_Ref),
         Volatile => True);

      AST_Service_Queue (AST_Service_Queue_Put) := AST_Instance'
        (Taskid  => Handler_Data_Ptr.Taskid,
         Entryno => Handler_Data_Ptr.Entryno,
         Param   => Param);

      --  OpenVMS Programming Concepts manual, chapter 8.2.3:
      --  "Implicit synchronization can be achieved for data that is shared
      --   for write by using only AST routines to write the data, since only
      --   one AST can be running at any one time."

      --  This subprogram runs at AST level so is guaranteed to be
      --  called sequentially at a given access level.

      AST_Service_Queue_Put := AST_Service_Queue_Put + 1;

      --  Need to wake up processing task. If there is no waiting server
      --  then we have temporarily run out, but things should still be
      --  OK, since one of the active ones will eventually pick up the
      --  service request queued in the AST_Service_Queue.

      for J in 1 .. Num_AST_Servers loop
         if Is_Waiting (J) then
            Is_Waiting (J) := False;

            --  Sleeps are handled by ASTs on VMS, so don't call Wakeup

            STPOD.Interrupt_AST_Handler (To_Address (AST_Task_Ids (J)));
            exit;
         end if;
      end loop;
   end Process_AST;

begin
   STPO.Initialize_Lock (AST_Lock'Access, STPO.Global_Task_Level);
end System.AST_Handling;
