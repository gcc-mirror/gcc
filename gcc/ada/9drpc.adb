------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S Y S T E M . R P C                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  Version for ???

with Unchecked_Deallocation;
with Ada.Streams;

with System.RPC.Net_Trace;
with System.RPC.Garlic;
with System.RPC.Streams;
pragma Elaborate (System.RPC.Garlic);

package body System.RPC is

   --  ??? general note: the debugging calls are very heavy, especially
   --  those that create exception handlers in every procedure. Do we
   --  really still need all this stuff?

   use type Ada.Streams.Stream_Element_Count;
   use type Ada.Streams.Stream_Element_Offset;

   use type Garlic.Protocol_Access;
   use type Garlic.Lock_Method;

   Max_Of_Message_Id : constant := 127;

   subtype Message_Id_Type is
     Integer range -Max_Of_Message_Id .. Max_Of_Message_Id;
   --  A message id is either a request id or reply id. A message id is
   --  provided with a message to a receiving stub which uses the opposite
   --  as a reply id. A message id helps to retrieve to which task is
   --  addressed a reply. When the environment task receives a message, the
   --  message id is extracted : a positive message id stands for a call, a
   --  negative message id stands for a reply. A null message id stands for
   --  an asynchronous request.

   subtype Request_Id_Type is Message_Id_Type range 1 .. Max_Of_Message_Id;
   --  When a message id is positive, it is a request

   type Message_Length_Per_Request is array (Request_Id_Type)
      of Ada.Streams.Stream_Element_Count;

   Header_Size : Ada.Streams.Stream_Element_Count :=
                   Streams.Get_Integer_Initial_Size +
                     Streams.Get_SEC_Initial_Size;
   --  Initial size needed for frequently used header streams

   Stream_Error : exception;
   --  Occurs when a read procedure is executed on an empty stream
   --  or when a write procedure is executed on a full stream

   Partition_RPC_Receiver : RPC_Receiver;
   --  Cache the RPC_Recevier passed by Establish_RPC_Receiver

   type Anonymous_Task_Node;

   type Anonymous_Task_Node_Access is access Anonymous_Task_Node;
   --  Types we need to construct a singly linked list of anonymous tasks
   --  This pool is maintained to avoid a task creation each time a RPC
   --  occurs - to be cont'd

   task type Anonymous_Task_Type (Self : Anonymous_Task_Node_Access) is

      entry Start
         (Message_Id   : in Message_Id_Type;
          Partition    : in Partition_ID;
          Params_Size  : in Ada.Streams.Stream_Element_Count;
          Result_Size  : in Ada.Streams.Stream_Element_Count;
          Protocol     : in Garlic.Protocol_Access);
      --  This entry provides an anonymous task a remote call to perform.
      --  This task calls for a Request id is provided to construct the
      --  reply id by using -Request. Partition is used to send the reply
      --  message. Params_Size is the size of the calling stub Params stream.
      --  Then Protocol (used by the environment task previously) allows
      --  extraction of the message following the header (The header is
      --  extracted by the environment task)
      --  Note: grammar in above is obscure??? needs cleanup

   end Anonymous_Task_Type;

   type Anonymous_Task_Access is access Anonymous_Task_Type;

   type Anonymous_Task_List is record
      Head     : Anonymous_Task_Node_Access;
      Tail     : Anonymous_Task_Node_Access;
   end record;

   type Anonymous_Task_Node is record
      Element : Anonymous_Task_Access;
      Next    : Anonymous_Task_Node_Access;
   end record;
   --  Types we need to construct a singly linked list of anonymous tasks.
   --  This pool is maintained to avoid a task creation each time a RPC occurs.

   protected Garbage_Collector is

      procedure Allocate
         (Item : out Anonymous_Task_Node_Access);
      --  Anonymous task pool management : if there is an anonymous task
      --  left, use it. Otherwise, allocate a new one

      procedure Deallocate
         (Item : in out Anonymous_Task_Node_Access);
      --  Anonymous task pool management : queue this task in the pool
      --  of inactive anonymous tasks.

   private

      Anonymous_List : Anonymous_Task_Node_Access;
      --  The list root of inactive anonymous tasks

   end Garbage_Collector;

   task Dispatcher is

      entry New_Request (Request : out Request_Id_Type);
      --  To get a new request

      entry Wait_On (Request_Id_Type)
        (Length : out Ada.Streams.Stream_Element_Count);
      --  To block the calling stub when it waits for a reply
      --  When it is resumed, we provide the size of the reply

      entry Wake_Up
        (Request : in Request_Id_Type;
         Length  : in Ada.Streams.Stream_Element_Count);
      --  To wake up the calling stub when the environnement task has
      --  received a reply for this request

   end Dispatcher;

   task Environnement is

      entry Start;
      --  Receive no message until Partition_Receiver is set
      --  Establish_RPC_Receiver decides when the environment task
      --  is allowed to start

   end Environnement;

   protected Partition_Receiver is

      entry Is_Set;
      --  Blocks if the Partition_RPC_Receiver has not been set

      procedure Set;
      --  Done by Establish_RPC_Receiver when Partition_RPC_Receiver
      --  is known

   private

      Was_Set : Boolean := False;
      --  True when Partition_RPC_Receiver has been set

   end Partition_Receiver;
   --  Anonymous tasks have to wait for the Partition_RPC_Receiver
   --  to be established

   type Debug_Level is
      (D_Elaborate,        --  About the elaboration of this package
       D_Communication,    --  About calls to Send and Receive
       D_Debug,            --  Verbose
       D_Exception);       --  Exception handler
   --  Debugging levels

   package Debugging is new System.RPC.Net_Trace (Debug_Level, "RPC : ");
   --  Debugging package

   procedure D
     (Flag : in Debug_Level; Info : in String) renames Debugging.Debug;
   --  Shortcut

   ------------------------
   -- Partition_Receiver --
   ------------------------

   protected body Partition_Receiver is

      -------------------------------
      -- Partition_Receiver.Is_Set --
      -------------------------------

      entry Is_Set when Was_Set is
      begin
         null;
      end Is_Set;

      ----------------------------
      -- Partition_Receiver.Set --
      ----------------------------

      procedure Set is
      begin
         Was_Set := True;
      end Set;

   end Partition_Receiver;

   ---------------
   -- Head_Node --
   ---------------

   procedure Head_Node
     (Index  : out Packet_Node_Access;
      Stream : Params_Stream_Type)
   is
   begin
      Index := Stream.Extra.Head;

   exception
      when others =>
         D (D_Exception, "exception in Head_Node");
         raise;
   end Head_Node;

   ---------------
   -- Tail_Node --
   ---------------

   procedure Tail_Node
     (Index  : out Packet_Node_Access;
      Stream : Params_Stream_Type)
   is
   begin
      Index := Stream.Extra.Tail;

   exception
      when others =>
         D (D_Exception, "exception in Tail_Node");
         raise;
   end Tail_Node;

   ---------------
   -- Null_Node --
   ---------------

   function Null_Node (Index : in Packet_Node_Access) return Boolean is
   begin
      return Index = null;

   exception
      when others =>
         D (D_Exception, "exception in Null_Node");
         raise;
   end Null_Node;

   ----------------------
   -- Delete_Head_Node --
   ----------------------

   procedure Delete_Head_Node (Stream : in out Params_Stream_Type) is

      procedure Free is
        new Unchecked_Deallocation
        (Packet_Node, Packet_Node_Access);

      Next_Node : Packet_Node_Access := Stream.Extra.Head.Next;

   begin
      --  Delete head node and free memory usage

      Free (Stream.Extra.Head);
      Stream.Extra.Head := Next_Node;

      --  If the extra storage is empty, update tail as well

      if Stream.Extra.Head = null then
         Stream.Extra.Tail := null;
      end if;

   exception
      when others =>
         D (D_Exception, "exception in Delete_Head_Node");
         raise;
   end Delete_Head_Node;

   ---------------
   -- Next_Node --
   ---------------

   procedure Next_Node (Node : in out Packet_Node_Access) is
   begin
      --  Node is set to the next node
      --  If not possible, Stream_Error is raised

      if Node = null then
         raise Stream_Error;
      else
         Node := Node.Next;
      end if;

   exception
      when others =>
         D (D_Exception, "exception in Next_Node");
         raise;
   end Next_Node;

   ---------------------
   -- Append_New_Node --
   ---------------------

   procedure Append_New_Node (Stream : in out Params_Stream_Type) is
      Index : Packet_Node_Access;

   begin
      --  Set Index to the end of the linked list

      Tail_Node (Index, Stream);

      if Null_Node (Index) then

         --  The list is empty : set head as well

         Stream.Extra.Head := new Packet_Node;
         Stream.Extra.Tail := Stream.Extra.Head;

      else
         --  The list is not empty : link new node with tail

         Stream.Extra.Tail.Next := new Packet_Node;
         Stream.Extra.Tail := Stream.Extra.Tail.Next;

      end if;

   exception
      when others =>
         D (D_Exception, "exception in Append_New_Node");
         raise;
   end Append_New_Node;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
     renames System.RPC.Streams.Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array)
     renames System.RPC.Streams.Write;

   -----------------------
   -- Garbage_Collector --
   -----------------------

   protected body Garbage_Collector is

      --------------------------------
      -- Garbage_Collector.Allocate --
      --------------------------------

      procedure Allocate (Item : out Anonymous_Task_Node_Access) is
         New_Anonymous_Task_Node : Anonymous_Task_Node_Access;
         Anonymous_Task          : Anonymous_Task_Access;

      begin
         --  If the list is empty, allocate a new anonymous task
         --  Otherwise, reuse the first queued anonymous task

         if Anonymous_List = null then

            --  Create a new anonymous task
            --  Provide this new task with its id to allow it
            --  to enqueue itself into the free anonymous task list
            --  with the function Deallocate

            New_Anonymous_Task_Node := new Anonymous_Task_Node;
            Anonymous_Task :=
             new Anonymous_Task_Type (New_Anonymous_Task_Node);
            New_Anonymous_Task_Node.all := (Anonymous_Task, null);

         else
            --  Extract one task from the list
            --  Set the Next field to null to avoid possible bugs

            New_Anonymous_Task_Node  := Anonymous_List;
            Anonymous_List := Anonymous_List.Next;
            New_Anonymous_Task_Node.Next := null;

         end if;

         --  Item is an out parameter

         Item := New_Anonymous_Task_Node;

      exception
         when others =>
            D (D_Exception, "exception in Allocate (Anonymous Task)");
            raise;
      end Allocate;

      ----------------------------------
      -- Garbage_Collector.Deallocate --
      ----------------------------------

      procedure Deallocate (Item : in out Anonymous_Task_Node_Access) is
      begin
         --  Enqueue the task in the free list

         Item.Next := Anonymous_List;
         Anonymous_List := Item;

      exception
         when others =>
            D (D_Exception, "exception in Deallocate (Anonymous Task)");
            raise;
      end Deallocate;

   end Garbage_Collector;

   ------------
   -- Do_RPC --
   ------------

   procedure Do_RPC
     (Partition  : Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type)
   is
      Protocol   : Protocol_Access;
      Request    : Request_Id_Type;
      Header     : aliased Params_Stream_Type (Header_Size);
      R_Length   : Ada.Streams.Stream_Element_Count;

   begin
      --  Parameters order :
      --       Opcode   (provided and used by garlic)
      --   (1) Size     (provided by s-rpc and used by garlic)
      --                (size of (2)+(3)+(4)+(5))
      --   (2) Request  (provided by calling stub (resp receiving stub) and
      --                 used by anonymous task (resp Do_RPC))
      --                *** ZERO IF APC ***
      --   (3) Res.len. (provided by calling stubs and used by anonymous task)
      --                *** ZERO IF APC ***
      --   (4) Receiver (provided by calling stubs and used by anonymous task)
      --   (5) Params   (provided by calling stubs and used by anonymous task)

      --  The call is a remote call or a local call. A local call occurs
      --  when the pragma All_Calls_Remote has been specified. Do_RPC is
      --  called and the execution has to be performed in the PCS

      if Partition /= Garlic.Get_My_Partition_ID then

         --  Get a request id to be resumed when the reply arrives

         Dispatcher.New_Request (Request);

         --  Build header = request (2) + result.initial_size (3)

         D (D_Debug, "Do_RPC - Build header");
         Streams.Allocate (Header);
         Streams.Integer_Write_Attribute            --  (2)
           (Header'Access, Request);
         System.RPC.Streams.SEC_Write_Attribute     --  (3)
           (Header'Access, Result.Initial_Size);

         --  Get a protocol method to communicate with the remote partition
         --  and give the message size

         D (D_Communication,
            "Do_RPC - Lookup for protocol to talk to partition" &
            Partition_ID'Image (Partition));
         Garlic.Initiate_Send
           (Partition,
            Streams.Get_Stream_Size (Header'Access) +
            Streams.Get_Stream_Size (Params), --  (1)
            Protocol,
            Garlic.Remote_Call);

         --  Send the header by using the protocol method

         D (D_Communication, "Do_RPC - Send Header to partition" &
            Partition_ID'Image (Partition));
         Garlic.Send
           (Protocol.all,
            Partition,
            Header'Access);                         --  (2) + (3)

         --  The header is deallocated

         Streams.Deallocate (Header);

         --  Send Params from Do_RPC

         D (D_Communication, "Do_RPC - Send Params to partition" &
            Partition_ID'Image (Partition));
         Garlic.Send
           (Protocol.all,
            Partition,
            Params);                                --  (4) + (5)

         --  Let Garlic know we have nothing else to send

         Garlic.Complete_Send
           (Protocol.all,
            Partition);
         D (D_Debug, "Do_RPC - Suspend");

         --  Wait for a reply and get the reply message length

         Dispatcher.Wait_On (Request) (R_Length);
         D (D_Debug, "Do_RPC - Resume");

         declare
            New_Result : aliased Params_Stream_Type (R_Length);
         begin
            --  Adjust the Result stream size right now to be able to load
            --  the stream in one receive call. Create a temporary resutl
            --  that will be substituted to Do_RPC one

            Streams.Allocate (New_Result);

            --  Receive the reply message from receiving stub

            D (D_Communication, "Do_RPC - Receive Result from partition" &
            Partition_ID'Image (Partition));
            Garlic.Receive
              (Protocol.all,
               Partition,
               New_Result'Access);

            --  Let Garlic know we have nothing else to receive

            Garlic.Complete_Receive
              (Protocol.all,
               Partition);

            --  Update calling stub Result stream

            D (D_Debug, "Do_RPC - Reconstruct Result");
            Streams.Deallocate (Result.all);
            Result.Initial := New_Result.Initial;
            Streams.Dump ("|||", Result.all);

         end;

      else
         --  Do RPC locally and first wait for Partition_RPC_Receiver to be
         --  set

         Partition_Receiver.Is_Set;
         D (D_Debug, "Do_RPC - Locally");
         Partition_RPC_Receiver.all (Params, Result);

      end if;

   exception
      when others =>
         D (D_Exception, "exception in Do_RPC");
         raise;
   end Do_RPC;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition  : Partition_ID;
      Params     : access Params_Stream_Type)
   is
      Message_Id : Message_Id_Type := 0;
      Protocol   : Protocol_Access;
      Header     : aliased Params_Stream_Type (Header_Size);

   begin
      --  For more informations, see above
      --  Request = 0 as we are not waiting for a reply message
      --  Result length = 0 as we don't expect a result at all

      if Partition /= Garlic.Get_My_Partition_ID then

         --  Build header = request (2) + result.initial_size (3)
         --  As we have an APC, the request id is null to indicate
         --  to the receiving stub that we do not expect a reply
         --  This comes from 0 = -0

         D (D_Debug, "Do_APC - Build Header");
         Streams.Allocate (Header);
         Streams.Integer_Write_Attribute
           (Header'Access, Integer (Message_Id));
         Streams.SEC_Write_Attribute
           (Header'Access, 0);

         --  Get a protocol method to communicate with the remote partition
         --  and give the message size

         D (D_Communication,
            "Do_APC - Lookup for protocol to talk to partition" &
            Partition_ID'Image (Partition));
         Garlic.Initiate_Send
           (Partition,
            Streams.Get_Stream_Size (Header'Access) +
            Streams.Get_Stream_Size (Params),
            Protocol,
            Garlic.Remote_Call);

         --  Send the header by using the protocol method

         D (D_Communication, "Do_APC - Send Header to partition" &
            Partition_ID'Image (Partition));
         Garlic.Send
           (Protocol.all,
            Partition,
            Header'Access);

         --  The header is deallocated

         Streams.Deallocate (Header);

         --  Send Params from Do_APC

         D (D_Communication, "Do_APC - Send Params to partition" &
            Partition_ID'Image (Partition));
         Garlic.Send
           (Protocol.all,
            Partition,
            Params);

         --  Let Garlic know we have nothing else to send

         Garlic.Complete_Send
           (Protocol.all,
            Partition);
      else

         declare
            Result   : aliased Params_Stream_Type (0);
         begin
            --  Result is here a dummy parameter
            --  No reason to deallocate as it is not allocated at all

            Partition_Receiver.Is_Set;
            D (D_Debug, "Do_APC - Locally");
            Partition_RPC_Receiver.all (Params, Result'Access);

         end;

      end if;

   exception
      when others =>
         D (D_Exception, "exception in Do_APC");
         raise;
   end Do_APC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : in Partition_ID;
      Receiver  : in RPC_Receiver)
   is
   begin
      --  Set Partition_RPC_Receiver and allow RPC mechanism

      Partition_RPC_Receiver := Receiver;
      Partition_Receiver.Set;
      D (D_Elaborate, "Partition_Receiver is set");

   exception
      when others =>
         D (D_Exception, "exception in Establish_RPC_Receiver");
         raise;
   end Establish_RPC_Receiver;

   ----------------
   -- Dispatcher --
   ----------------

   task body Dispatcher is
      Last_Request : Request_Id_Type := Request_Id_Type'First;
      Current_Rqst : Request_Id_Type := Request_Id_Type'First;
      Current_Size : Ada.Streams.Stream_Element_Count;

   begin
      loop
         --  Three services:

         --    New_Request to get an entry in Dispatcher table

         --    Wait_On for Do_RPC calls

         --    Wake_Up called by environment task when a Do_RPC receives
         --    the result of its remote call

         select
            accept New_Request (Request : out Request_Id_Type) do
               Request := Last_Request;

               --  << TODO >>
               --  ??? Avaibility check

               if Last_Request = Request_Id_Type'Last then
                  Last_Request := Request_Id_Type'First;
               else
                  Last_Request := Last_Request + 1;
               end if;

            end New_Request;

         or
            accept Wake_Up
              (Request : Request_Id_Type;
               Length  : Ada.Streams.Stream_Element_Count)
            do
               --  The environment reads the header and has been notified
               --  of the reply id and the size of the result message

               Current_Rqst := Request;
               Current_Size := Length;

            end Wake_Up;

            --  << TODO >>
            --  ??? Must be select with delay for aborted tasks

            select

               accept Wait_On (Current_Rqst)
                 (Length  : out Ada.Streams.Stream_Element_Count)
               do
                  Length := Current_Size;
               end Wait_On;

            or
               --  To free the Dispatcher when a task is aborted

               delay 1.0;

            end select;

         or
            terminate;
         end select;

      end loop;

   exception
      when others =>
         D (D_Exception, "exception in Dispatcher body");
         raise;
   end Dispatcher;

   -------------------------
   -- Anonymous_Task_Type --
   -------------------------

   task body Anonymous_Task_Type is
      Whoami       : Anonymous_Task_Node_Access := Self;
      C_Message_Id : Message_Id_Type;                  --  Current Message Id
      C_Partition  : Partition_ID;                     --  Current Partition
      Params_S     : Ada.Streams.Stream_Element_Count; --  Params message size
      Result_S     : Ada.Streams.Stream_Element_Count; --  Result message size
      C_Protocol   : Protocol_Access;                  --  Current Protocol

   begin
      loop
         --  Get a new RPC to execute

         select
            accept Start
              (Message_Id   : in Message_Id_Type;
               Partition    : in Partition_ID;
               Params_Size  : in Ada.Streams.Stream_Element_Count;
               Result_Size  : in Ada.Streams.Stream_Element_Count;
               Protocol     : in Protocol_Access)
            do
               C_Message_Id := Message_Id;
               C_Partition  := Partition;
               Params_S     := Params_Size;
               Result_S     := Result_Size;
               C_Protocol   := Protocol;
            end Start;
         or
            terminate;
         end select;

         declare
            Params : aliased Params_Stream_Type (Params_S);
            Result : aliased Params_Stream_Type (Result_S);
            Header : aliased Params_Stream_Type (Header_Size);

         begin
            --  We reconstruct all the client context : Params and Result
            --  with the SAME size, then we receive Params from calling stub

            D (D_Communication,
               "Anonymous Task - Receive Params from partition" &
               Partition_ID'Image (C_Partition));
            Garlic.Receive
               (C_Protocol.all,
                C_Partition,
                Params'Access);

            --  Let Garlic know we don't receive anymore

            Garlic.Complete_Receive
               (C_Protocol.all,
                C_Partition);

            --  Check that Partition_RPC_Receiver has been set

            Partition_Receiver.Is_Set;

            --  Do it locally

            D (D_Debug,
               "Anonymous Task - Perform Partition_RPC_Receiver for request" &
               Message_Id_Type'Image (C_Message_Id));
            Partition_RPC_Receiver (Params'Access, Result'Access);

            --  If this was a RPC we send the result back
            --  Otherwise, do nothing else than deallocation

            if C_Message_Id /= 0 then

               --  Build Header = -C_Message_Id + Result Size
               --  Provide the request id to the env task of the calling
               --  stub partition We get the real result stream size : the
               --  calling stub (in Do_RPC) updates its size to this one

               D (D_Debug, "Anonymous Task - Build Header");
               Streams.Allocate (Header);
               Streams.Integer_Write_Attribute
                 (Header'Access, Integer (-C_Message_Id));
               Streams.SEC_Write_Attribute
                 (Header'Access,
                  Streams.Get_Stream_Size (Result'Access));

               --  Get a protocol method to comunicate with the remote
               --  partition and give the message size

               D (D_Communication,
                  "Anonymous Task - Lookup for protocol talk to partition" &
                  Partition_ID'Image (C_Partition));
               Garlic.Initiate_Send
                 (C_Partition,
                  Streams.Get_Stream_Size (Header'Access) +
                  Streams.Get_Stream_Size (Result'Access),
                  C_Protocol,
                  Garlic.Remote_Call);

               --  Send the header by using the protocol method

               D (D_Communication,
                  "Anonymous Task - Send Header to partition" &
                  Partition_ID'Image (C_Partition));
               Garlic.Send
                 (C_Protocol.all,
                  C_Partition,
                  Header'Access);

               --  Send Result toDo_RPC

               D (D_Communication,
                  "Anonymous Task - Send Result to partition" &
                  Partition_ID'Image (C_Partition));
               Garlic.Send
                 (C_Protocol.all,
                  C_Partition,
                  Result'Access);

               --  Let Garlic know we don't send anymore

               Garlic.Complete_Send
                 (C_Protocol.all,
                  C_Partition);
               Streams.Deallocate (Header);
            end if;

            Streams.Deallocate (Params);
            Streams.Deallocate (Result);
         end;

         --  Enqueue into the anonymous task free list : become inactive

         Garbage_Collector.Deallocate (Whoami);

      end loop;

   exception
      when others =>
         D (D_Exception, "exception in Anonymous_Task_Type body");
         raise;
   end Anonymous_Task_Type;

   -----------------
   -- Environment --
   -----------------

   task body Environnement is
      Partition    : Partition_ID;
      Message_Size : Ada.Streams.Stream_Element_Count;
      Result_Size  : Ada.Streams.Stream_Element_Count;
      Message_Id   : Message_Id_Type;
      Header       : aliased Params_Stream_Type (Header_Size);
      Protocol     : Protocol_Access;
      Anonymous    : Anonymous_Task_Node_Access;

   begin
      --  Wait the Partition_RPC_Receiver to be set

      accept Start;
      D (D_Elaborate, "Environment task elaborated");

      loop
         --  We receive first a fixed size message : the header
         --  Header = Message Id + Message Size

         Streams.Allocate (Header);

         --  Garlic provides the size of the received message and the
         --  protocol to use to communicate with the calling partition

         Garlic.Initiate_Receive
           (Partition,
            Message_Size,
            Protocol,
            Garlic.Remote_Call);
         D (D_Communication,
            "Environment task - Receive protocol to talk to active partition" &
            Partition_ID'Image (Partition));

         --  Extract the header to route the message either to
         --  an anonymous task (Message Id > 0 <=> Request Id)
         --  or to a waiting task (Message Id < 0 <=> Reply Id)

         D (D_Communication,
            "Environment task - Receive Header from partition" &
            Partition_ID'Image (Partition));
         Garlic.Receive
           (Protocol.all,
            Partition,
            Header'Access);

         --  Evaluate the remaining size of the message

         Message_Size := Message_Size -
            Streams.Get_Stream_Size (Header'Access);

         --  Extract from header : message id and message size

         Streams.Integer_Read_Attribute (Header'Access, Message_Id);
         Streams.SEC_Read_Attribute (Header'Access, Result_Size);

         if Streams.Get_Stream_Size (Header'Access) /= 0 then

            --  If there are stream elements left in the header ???

            D (D_Exception, "Header is not empty");
            raise Program_Error;

         end if;

         if Message_Id < 0 then

            --  The message was sent by a receiving stub : wake up the
            --  calling task - We have a reply there

            D (D_Debug, "Environment Task - Receive Reply from partition" &
               Partition_ID'Image (Partition));
            Dispatcher.Wake_Up (-Message_Id, Result_Size);

         else
            --  The message was send by a calling stub : get an anonymous
            --  task to perform the job

            D (D_Debug, "Environment Task - Receive Request from partition" &
               Partition_ID'Image (Partition));
            Garbage_Collector.Allocate (Anonymous);

            --  We substracted the size of the header from the size of the
            --  global message in order to provide immediatly Params size

            Anonymous.Element.Start
              (Message_Id,
               Partition,
               Message_Size,
               Result_Size,
               Protocol);

         end if;

         --  Deallocate header : unnecessary - WARNING

         Streams.Deallocate (Header);

      end loop;

   exception
      when others =>
         D (D_Exception, "exception in Environment");
         raise;
   end Environnement;

begin
   --  Set debugging information

   Debugging.Set_Environment_Variable ("RPC");
   Debugging.Set_Debugging_Name ("D", D_Debug);
   Debugging.Set_Debugging_Name ("E", D_Exception);
   Debugging.Set_Debugging_Name ("C", D_Communication);
   Debugging.Set_Debugging_Name ("Z", D_Elaborate);
   D (D_Elaborate, "To be elaborated");

   --  When this body is elaborated we should ensure that RCI name server
   --  has been already elaborated : this means that Establish_RPC_Receiver
   --  has already been called and that Partition_RPC_Receiver is set

   Environnement.Start;
   D (D_Elaborate, "ELABORATED");

end System.RPC;
