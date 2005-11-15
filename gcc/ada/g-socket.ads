------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . S O C K E T S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2005, AdaCore                     --
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

--  This package provides an interface to the sockets communication facility
--  provided on many operating systems. This is implemented on the following
--  platforms:

--     All native ports, with restrictions as follows

--       Multicast is available only on systems which provide support for this
--       feature, so it is not available if Multicast is not supported, or not
--       installed. In particular Multicast is not available with the Windows
--       version.

--       The VMS implementation has implemented using the DECC RTL Socket API,
--       and is thus subject to limitations in the implementation of this API.

--     VxWorks cross ports fully implement this package

--     This package is not yet implemented on LynxOS or other cross ports

with Ada.Exceptions;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

with System;

package GNAT.Sockets is

   --  Sockets are designed to provide a consistent communication facility
   --  between applications. This package provides an Ada-like interface
   --  similar to that proposed as part of the BSD socket layer.

   --  GNAT.Sockets has been designed with several ideas in mind

   --  This is a system independent interface. Therefore, we try as much as
   --  possible to mask system incompatibilities. Some functionalities are not
   --  available because there are not fully supported on some systems.

   --  This is a thick binding. For instance, a major effort has been done to
   --  avoid using memory addresses or untyped ints. We preferred to define
   --  streams and enumeration types. Errors are not returned as returned
   --  values but as exceptions.

   --  This package provides a POSIX-compliant interface (between two
   --  different implementations of the same routine, we adopt the one closest
   --  to the POSIX specification). For instance, using select(), the
   --  notification of an asynchronous connect failure is delivered in the
   --  write socket set (POSIX) instead of the exception socket set (NT).

   --  Here is a typical example of what you can do:

   --  with GNAT.Sockets; use GNAT.Sockets;

   --  with Ada.Text_IO;
   --  with Ada.Exceptions; use Ada.Exceptions;

   --  procedure PingPong is

   --     Group : constant String := "239.255.128.128";
   --     --  Multicast group: administratively scoped IP address

   --     task Pong is
   --        entry Start;
   --        entry Stop;
   --     end Pong;

   --     task body Pong is
   --        Address  : Sock_Addr_Type;
   --        Server   : Socket_Type;
   --        Socket   : Socket_Type;
   --        Channel  : Stream_Access;

   --     begin
   --        accept Start;
   --
   --        --  Get an Internet address of a host (here the local host name).
   --        --  Note that a host can have several addresses. Here we get
   --        --  the first one which is supposed to be the official one.

   --        Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);

   --        --  Get a socket address that is an Internet address and a port

   --        Address.Port := 5876;

   --        --  The first step is to create a socket. Once created, this
   --        --  socket must be associated to with an address. Usually only a
   --        --  server (Pong here) needs to bind an address explicitly. Most
   --        --  of the time clients can skip this step because the socket
   --        --  routines will bind an arbitrary address to an unbound socket.

   --        Create_Socket (Server);

   --        --  Allow reuse of local addresses

   --        Set_Socket_Option
   --          (Server,
   --           Socket_Level,
   --           (Reuse_Address, True));

   --        Bind_Socket (Server, Address);

   --        --  A server marks a socket as willing to receive connect events

   --        Listen_Socket (Server);

   --        --  Once a server calls Listen_Socket, incoming connects events
   --        --  can be accepted. The returned Socket is a new socket that
   --        --  represents the server side of the connection. Server remains
   --        --  available to receive further connections.

   --        Accept_Socket (Server, Socket, Address);

   --        --  Return a stream associated to the connected socket

   --        Channel := Stream (Socket);

   --        --  Force Pong to block

   --        delay 0.2;

   --        --  Receive and print message from client Ping

   --        declare
   --           Message : String := String'Input (Channel);

   --        begin
   --           Ada.Text_IO.Put_Line (Message);

   --           --  Send same message back to client Ping

   --           String'Output (Channel, Message);
   --        end;

   --        Close_Socket (Server);
   --        Close_Socket (Socket);

   --        --  Part of the multicast example

   --        --  Create a datagram socket to send connectionless, unreliable
   --        --  messages of a fixed maximum length.

   --        Create_Socket (Socket, Family_Inet, Socket_Datagram);

   --        --  Allow reuse of local addresses

   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));

   --        --  Join a multicast group

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));

   --        --  Controls the live time of the datagram to avoid it being
   --        --  looped forever due to routing errors. Routers decrement
   --        --  the TTL of every datagram as it traverses from one network
   --        --  to another and when its value reaches 0 the packet is
   --        --  dropped. Default is 1.

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_TTL, 1));

   --        --  Want the data you send to be looped back to your host

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_Loop, True));

   --        --  If this socket is intended to receive messages, bind it
   --        --  to a given socket address.

   --        Address.Addr := Any_Inet_Addr;
   --        Address.Port := 55505;

   --        Bind_Socket (Socket, Address);

   --        --  If this socket is intended to send messages, provide the
   --        --  receiver socket address.

   --        Address.Addr := Inet_Addr (Group);
   --        Address.Port := 55506;

   --        Channel := Stream (Socket, Address);

   --        --  Receive and print message from client Ping

   --        declare
   --           Message : String := String'Input (Channel);

   --        begin
   --           --  Get the address of the sender

   --           Address := Get_Address (Channel);
   --           Ada.Text_IO.Put_Line (Message & " from " & Image (Address));

   --           --  Send same message back to client Ping

   --           String'Output (Channel, Message);
   --        end;

   --        Close_Socket (Socket);

   --        accept Stop;

   --     exception when E : others =>
   --        Ada.Text_IO.Put_Line
   --          (Exception_Name (E) & ": " & Exception_Message (E));
   --     end Pong;

   --     task Ping is
   --        entry Start;
   --        entry Stop;
   --     end Ping;

   --     task body Ping is
   --        Address  : Sock_Addr_Type;
   --        Socket   : Socket_Type;
   --        Channel  : Stream_Access;

   --     begin
   --        accept Start;

   --        --  See comments in Ping section for the first steps

   --        Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   --        Address.Port := 5876;
   --        Create_Socket (Socket);

   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));

   --        --  Force Pong to block

   --        delay 0.2;

   --        --  If the client's socket is not bound, Connect_Socket will
   --        --  bind to an unused address. The client uses Connect_Socket to
   --        --  create a logical connection between the client's socket and
   --        --  a server's socket returned by Accept_Socket.

   --        Connect_Socket (Socket, Address);

   --        Channel := Stream (Socket);

   --        --  Send message to server Pong

   --        String'Output (Channel, "Hello world");

   --        --  Force Ping to block

   --        delay 0.2;

   --        --  Receive and print message from server Pong

   --        Ada.Text_IO.Put_Line (String'Input (Channel));
   --        Close_Socket (Socket);

   --        --  Part of multicast example. Code similar to Pong's one

   --        Create_Socket (Socket, Family_Inet, Socket_Datagram);

   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_TTL, 1));

   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_Loop, True));

   --        Address.Addr := Any_Inet_Addr;
   --        Address.Port := 55506;

   --        Bind_Socket (Socket, Address);

   --        Address.Addr := Inet_Addr (Group);
   --        Address.Port := 55505;

   --        Channel := Stream (Socket, Address);

   --        --  Send message to server Pong

   --        String'Output (Channel, "Hello world");

   --        --  Receive and print message from server Pong

   --        declare
   --           Message : String := String'Input (Channel);

   --        begin
   --           Address := Get_Address (Channel);
   --           Ada.Text_IO.Put_Line (Message & " from " & Image (Address));
   --        end;

   --        Close_Socket (Socket);

   --        accept Stop;

   --     exception when E : others =>
   --        Ada.Text_IO.Put_Line
   --          (Exception_Name (E) & ": " & Exception_Message (E));
   --     end Ping;

   --  begin
   --     --  Indicate whether the thread library provides process
   --     --  blocking IO. Basically, if you are not using FSU threads
   --     --  the default is ok.

   --     Initialize (Process_Blocking_IO => False);
   --     Ping.Start;
   --     Pong.Start;
   --     Ping.Stop;
   --     Pong.Stop;
   --     Finalize;
   --  end PingPong;

   procedure Initialize (Process_Blocking_IO : Boolean := False);
   --  Initialize must be called before using any other socket routines. The
   --  Process_Blocking_IO parameter indicates whether the thread library
   --  provides process-blocking or thread-blocking input/output operations.
   --  In the former case (typically with FSU threads) GNAT.Sockets should be
   --  initialized with a value of True to provide task-blocking IO through an
   --  emulation mechanism. Only the first call to Initialize is taken into
   --  account (further calls will be ignored). Note that with the default
   --  value of Process_Blocking_IO, this operation is a no-op on UNIX
   --  platforms, but applications should make sure to call it if portability
   --  is expected: some platforms (such as Windows) require initialization
   --  before any other socket operations.

   procedure Finalize;
   --  After Finalize is called it is not possible to use any routines
   --  exported in by this package. This procedure is idempotent.

   type Socket_Type is private;
   --  Sockets are used to implement a reliable bi-directional point-to-point,
   --  stream-based connections between hosts. No_Socket provides a special
   --  value to denote uninitialized sockets.

   No_Socket : constant Socket_Type;

   Socket_Error : exception;
   --  There is only one exception in this package to deal with an error during
   --  a socket routine. Once raised, its message contains a string describing
   --  the error code.

   function Image (Socket : Socket_Type) return String;
   --  Return a printable string for Socket

   function To_C (Socket : Socket_Type) return Integer;
   --  Return a file descriptor to be used by external subprograms. This is
   --  useful for C functions that are not yet interfaced in this package.

   type Family_Type is (Family_Inet, Family_Inet6);
   --  Address family (or protocol family) identifies the communication domain
   --  and groups protocols with similar address formats. IPv6 will soon be
   --  supported.

   type Mode_Type is (Socket_Stream, Socket_Datagram);
   --  Stream sockets provide connection-oriented byte streams. Datagram
   --  sockets support unreliable connectionless message based communication.

   type Shutmode_Type is (Shut_Read, Shut_Write, Shut_Read_Write);
   --  When a process closes a socket, the policy is to retain any data queued
   --  until either a delivery or a timeout expiration (in this case, the data
   --  are discarded). A finer control is available through shutdown. With
   --  Shut_Read, no more data can be received from the socket. With_Write, no
   --  more data can be transmitted. Neither transmission nor reception can be
   --  performed with Shut_Read_Write.

   type Port_Type is new Natural;
   --  Classical port definition. No_Port provides a special value to
   --  denote uninitialized port. Any_Port provides a special value
   --  enabling all ports.

   Any_Port : constant Port_Type;
   No_Port  : constant Port_Type;

   type Inet_Addr_Type (Family : Family_Type := Family_Inet) is private;
   --  An Internet address depends on an address family (IPv4 contains 4
   --  octets and Ipv6 contains 16 octets). Any_Inet_Addr is a special value
   --  treated like a wildcard enabling all addresses. No_Inet_Addr provides a
   --  special value to denote uninitialized inet addresses.

   Any_Inet_Addr       : constant Inet_Addr_Type;
   No_Inet_Addr        : constant Inet_Addr_Type;
   Broadcast_Inet_Addr : constant Inet_Addr_Type;

   type Sock_Addr_Type (Family : Family_Type := Family_Inet) is record
      Addr : Inet_Addr_Type (Family);
      Port : Port_Type;
   end record;
   --  Socket addresses fully define a socket connection with protocol family,
   --  an Internet address and a port. No_Sock_Addr provides a special value
   --  for uninitialized socket addresses.

   No_Sock_Addr : constant Sock_Addr_Type;

   function Image (Value : Inet_Addr_Type) return String;
   --  Return an image of an Internet address. IPv4 notation consists in 4
   --  octets in decimal format separated by dots. IPv6 notation consists in
   --  16 octets in hexadecimal format separated by colons (and possibly
   --  dots).

   function Image (Value : Sock_Addr_Type) return String;
   --  Return inet address image and port image separated by a colon

   function Inet_Addr (Image : String) return Inet_Addr_Type;
   --  Convert address image from numbers-and-dots notation into an
   --  inet address.

   --  Host entries provide complete information on a given host: the official
   --  name, an array of alternative names or aliases and array of network
   --  addresses.

   type Host_Entry_Type
     (Aliases_Length, Addresses_Length : Natural) is private;

   function Official_Name (E : Host_Entry_Type) return String;
   --  Return official name in host entry

   function Aliases_Length (E : Host_Entry_Type) return Natural;
   --  Return number of aliases in host entry

   function Addresses_Length (E : Host_Entry_Type) return Natural;
   --  Return number of addresses in host entry

   function Aliases
     (E : Host_Entry_Type;
      N : Positive := 1) return String;
   --  Return N'th aliases in host entry. The first index is 1

   function Addresses
     (E : Host_Entry_Type;
      N : Positive := 1) return Inet_Addr_Type;
   --  Return N'th addresses in host entry. The first index is 1

   Host_Error : exception;
   --  Exception raised by the two following procedures. Once raised, its
   --  message contains a string describing the error code. This exception is
   --  raised when an host entry cannot be retrieved.

   function Get_Host_By_Address
     (Address : Inet_Addr_Type;
      Family  : Family_Type := Family_Inet) return Host_Entry_Type;
   --  Return host entry structure for the given Inet address. Note that no
   --  result will be returned if there is no mapping of this IP address to a
   --  host name in the system tables (host database, DNS or otherwise).

   function Get_Host_By_Name
     (Name : String) return Host_Entry_Type;
   --  Return host entry structure for the given host name. Here name is
   --  either a host name, or an IP address. If Name is an IP address, this is
   --  equivalent to Get_Host_By_Address (Inet_Addr (Name)).

   function Host_Name return String;
   --  Return the name of the current host

   type Service_Entry_Type (Aliases_Length : Natural) is private;
   --  Service entries provide complete information on a given service: the
   --  official name, an array of alternative names or aliases and the port
   --  number.

   function Official_Name (S : Service_Entry_Type) return String;
   --  Return official name in service entry

   function Port_Number (S : Service_Entry_Type) return Port_Type;
   --  Return port number in service entry

   function Protocol_Name (S : Service_Entry_Type) return String;
   --  Return Protocol in service entry (usually UDP or TCP)

   function Aliases_Length (S : Service_Entry_Type) return Natural;
   --  Return number of aliases in service entry

   function Aliases
     (S : Service_Entry_Type;
      N : Positive := 1) return String;
   --  Return N'th aliases in service entry (the first index is 1)

   function Get_Service_By_Name
     (Name     : String;
      Protocol : String) return Service_Entry_Type;
   --  Return service entry structure for the given service name

   function Get_Service_By_Port
     (Port     : Port_Type;
      Protocol : String) return Service_Entry_Type;
   --  Return service entry structure for the given service port number

   Service_Error : exception;
   --  Comment required ???

   --  Errors are described by an enumeration type. There is only one
   --  exception Socket_Error in this package to deal with an error during a
   --  socket routine. Once raised, its message contains the error code
   --  between brackets and a string describing the error code.

   --  The name of the enumeration constant documents the error condition

   type Error_Type is
     (Success,
      Permission_Denied,
      Address_Already_In_Use,
      Cannot_Assign_Requested_Address,
      Address_Family_Not_Supported_By_Protocol,
      Operation_Already_In_Progress,
      Bad_File_Descriptor,
      Software_Caused_Connection_Abort,
      Connection_Refused,
      Connection_Reset_By_Peer,
      Destination_Address_Required,
      Bad_Address,
      Host_Is_Down,
      No_Route_To_Host,
      Operation_Now_In_Progress,
      Interrupted_System_Call,
      Invalid_Argument,
      Input_Output_Error,
      Transport_Endpoint_Already_Connected,
      Too_Many_Symbolic_Links,
      Too_Many_Open_Files,
      Message_Too_Long,
      File_Name_Too_Long,
      Network_Is_Down,
      Network_Dropped_Connection_Because_Of_Reset,
      Network_Is_Unreachable,
      No_Buffer_Space_Available,
      Protocol_Not_Available,
      Transport_Endpoint_Not_Connected,
      Socket_Operation_On_Non_Socket,
      Operation_Not_Supported,
      Protocol_Family_Not_Supported,
      Protocol_Not_Supported,
      Protocol_Wrong_Type_For_Socket,
      Cannot_Send_After_Transport_Endpoint_Shutdown,
      Socket_Type_Not_Supported,
      Connection_Timed_Out,
      Too_Many_References,
      Resource_Temporarily_Unavailable,
      Unknown_Host,
      Host_Name_Lookup_Failure,
      Non_Recoverable_Error,
      Unknown_Server_Error,
      Cannot_Resolve_Error);

   --  Timeval_Duration is a subtype of Standard.Duration because the full
   --  range of Standard.Duration cannot be represented in the equivalent C
   --  structure. Moreover, negative values are not allowed to avoid system
   --  incompatibilities.

   Immediate : constant := 0.0;
   Forever   : constant := Duration (Integer'Last) * 1.0;

   subtype Timeval_Duration is Duration range Immediate .. Forever;

   --  Get_Socket_Options and Set_Socket_Options manipulate options associated
   --  with a socket. Options may exist at multiple protocol levels in the
   --  communication stack. Socket_Level is the uppermost socket level.

   type Level_Type is (
     Socket_Level,
     IP_Protocol_For_IP_Level,
     IP_Protocol_For_UDP_Level,
     IP_Protocol_For_TCP_Level);

   --  There are several options available to manipulate sockets. Each option
   --  has a name and several values available. Most of the time, the value is
   --  a boolean to enable or disable this option.

   type Option_Name is (
     Keep_Alive,       -- Enable sending of keep-alive messages
     Reuse_Address,    -- Allow bind to reuse local address
     Broadcast,        -- Enable datagram sockets to recv/send broadcasts
     Send_Buffer,      -- Set/get the maximum socket send buffer in bytes
     Receive_Buffer,   -- Set/get the maximum socket recv buffer in bytes
     Linger,           -- Shutdown wait for msg to be sent or timeout occur
     Error,            -- Get and clear the pending socket error
     No_Delay,         -- Do not delay send to coalesce packets (TCP_NODELAY)
     Add_Membership,   -- Join a multicast group
     Drop_Membership,  -- Leave a multicast group
     Multicast_If,     -- Set default outgoing interface for multicast packets
     Multicast_TTL,    -- Indicate the time-to-live of sent multicast packets
     Multicast_Loop,   -- Sent multicast packets are looped to local socket
     Send_Timeout,     -- Set timeout value for output
     Receive_Timeout); -- Set timeout value for input

   type Option_Type (Name : Option_Name := Keep_Alive) is record
      case Name is
         when Keep_Alive      |
              Reuse_Address   |
              Broadcast       |
              Linger          |
              No_Delay        |
              Multicast_Loop  =>
            Enabled : Boolean;

            case Name is
               when Linger    =>
                  Seconds : Natural;
               when others    =>
                  null;
            end case;

         when Send_Buffer     |
              Receive_Buffer  =>
            Size : Natural;

         when Error           =>
            Error : Error_Type;

         when Add_Membership  |
              Drop_Membership =>
            Multicast_Address : Inet_Addr_Type;
            Local_Interface   : Inet_Addr_Type;

         when Multicast_If    =>
            Outgoing_If : Inet_Addr_Type;

         when Multicast_TTL   =>
            Time_To_Live : Natural;

         when Send_Timeout |
              Receive_Timeout =>
            Timeout : Timeval_Duration;

      end case;
   end record;

   --  There are several controls available to manipulate sockets. Each option
   --  has a name and several values available. These controls differ from the
   --  socket options in that they are not specific to sockets but are
   --  available for any device.

   type Request_Name is (
      Non_Blocking_IO,  --  Cause a caller not to wait on blocking operations.
      N_Bytes_To_Read); --  Return the number of bytes available to read

   type Request_Type (Name : Request_Name := Non_Blocking_IO) is record
      case Name is
         when Non_Blocking_IO =>
            Enabled : Boolean;

         when N_Bytes_To_Read =>
            Size : Natural;

      end case;
   end record;

   --  A request flag allows to specify the type of message transmissions or
   --  receptions. A request flag can be combination of zero or more
   --  predefined request flags.

   type Request_Flag_Type is private;

   No_Request_Flag : constant Request_Flag_Type;
   --  This flag corresponds to the normal execution of an operation

   Process_Out_Of_Band_Data : constant Request_Flag_Type;
   --  This flag requests that the receive or send function operates on
   --  out-of-band data when the socket supports this notion (e.g.
   --  Socket_Stream).

   Peek_At_Incoming_Data : constant Request_Flag_Type;
   --  This flag causes the receive operation to return data from the
   --  beginning of the receive queue without removing that data from the
   --  queue. A subsequent receive call will return the same data.

   Wait_For_A_Full_Reception : constant Request_Flag_Type;
   --  This flag requests that the operation block until the full request is
   --  satisfied. However, the call may still return less data than requested
   --  if a signal is caught, an error or disconnect occurs, or the next data
   --  to be received is of a different type than that returned. Note that
   --  this flag depends on support in the underlying sockets implementation,
   --  and is not supported under Windows.

   Send_End_Of_Record : constant Request_Flag_Type;
   --  This flag indicates that the entire message has been sent and so this
   --  terminates the record.

   function "+" (L, R : Request_Flag_Type) return Request_Flag_Type;
   --  Combine flag L with flag R

   type Stream_Element_Reference is access all Ada.Streams.Stream_Element;

   type Vector_Element is record
      Base   : Stream_Element_Reference;
      Length : Ada.Streams.Stream_Element_Count;
   end record;

   type Vector_Type is array (Integer range <>) of Vector_Element;

   procedure Create_Socket
     (Socket : out Socket_Type;
      Family : Family_Type := Family_Inet;
      Mode   : Mode_Type   := Socket_Stream);
   --  Create an endpoint for communication. Raises Socket_Error on error

   procedure Accept_Socket
     (Server  : Socket_Type;
      Socket  : out Socket_Type;
      Address : out Sock_Addr_Type);
   --  Extracts the first connection request on the queue of pending
   --  connections, creates a new connected socket with mostly the same
   --  properties as Server, and allocates a new socket. The returned Address
   --  is filled in with the address of the connection. Raises Socket_Error on
   --  error.

   procedure Bind_Socket
     (Socket  : Socket_Type;
      Address : Sock_Addr_Type);
   --  Once a socket is created, assign a local address to it. Raise
   --  Socket_Error on error.

   procedure Close_Socket (Socket : Socket_Type);
   --  Close a socket and more specifically a non-connected socket

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : in out Sock_Addr_Type);
   --  Make a connection to another socket which has the address of
   --  Server. Raises Socket_Error on error.

   procedure Control_Socket
     (Socket  : Socket_Type;
      Request : in out Request_Type);
   --  Obtain or set parameter values that control the socket. This control
   --  differs from the socket options in that they are not specific to
   --  sockets but are available for any device.

   function Get_Peer_Name (Socket : Socket_Type) return Sock_Addr_Type;
   --  Return the peer or remote socket address of a socket. Raise
   --  Socket_Error on error.

   function Get_Socket_Name (Socket : Socket_Type) return Sock_Addr_Type;
   --  Return the local or current socket address of a socket. Return
   --  No_Sock_Addr on error (for instance, socket closed or not locally
   --  bound).

   function Get_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Name   : Option_Name) return Option_Type;
   --  Get the options associated with a socket. Raises Socket_Error
   --  on error.

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Positive := 15);
   --  To accept connections, a socket is first created with Create_Socket,
   --  a willingness to accept incoming connections and a queue Length for
   --  incoming connections are specified. Raise Socket_Error on error.

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Flags  : Request_Flag_Type := No_Request_Flag);
   --  Receive message from Socket. Last is the index value such that Item
   --  (Last) is the last character assigned. Note that Last is set to
   --  Item'First - 1 when the socket has been closed by peer. This is not an
   --  error and no exception is raised. Flags allows to control the
   --  reception. Raise Socket_Error on error.

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      From   : out Sock_Addr_Type;
      Flags  : Request_Flag_Type := No_Request_Flag);
   --  Receive message from Socket. If Socket is not connection-oriented, the
   --  source address From of the message is filled in. Last is the index
   --  value such that Item (Last) is the last character assigned. Flags
   --  allows to control the reception. Raises Socket_Error on error.

   procedure Receive_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Receive data from a socket and scatter it into the set of vector
   --  elements Vector. Count is set to the count of received stream elements.

   function Resolve_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return Error_Type;
   --  When Socket_Error or Host_Error are raised, the exception message
   --  contains the error code between brackets and a string describing the
   --  error code. Resolve_Error extracts the error code from an exception
   --  message and translate it into an enumeration value.

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Flags  : Request_Flag_Type := No_Request_Flag);
   --  Transmit a message to another socket. Note that Last is set to
   --  Item'First-1 when socket has been closed by peer. This is not
   --  considered an error and no exception is raised. Flags allows to control
   --  the transmission. Raises Socket_Error on any other error condition.

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : Sock_Addr_Type;
      Flags  : Request_Flag_Type := No_Request_Flag);
   --  Transmit a message to another socket. The address is given by To. Flags
   --  allows to control the transmission. Raises Socket_Error on error.

   procedure Send_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Transmit data gathered from the set of vector elements Vector to a
   --  socket. Count is set to the count of transmitted stream elements.

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Option : Option_Type);
   --  Manipulate socket options. Raises Socket_Error on error

   procedure Shutdown_Socket
     (Socket : Socket_Type;
      How    : Shutmode_Type := Shut_Read_Write);
   --  Shutdown a connected socket. If How is Shut_Read, further receives will
   --  be disallowed. If How is Shut_Write, further sends will be disallowed.
   --  If how is Shut_Read_Write, further sends and receives will be
   --  disallowed.

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
   --  Same interface as Ada.Streams.Stream_IO

   function Stream
     (Socket : Socket_Type) return Stream_Access;
   --  Create a stream associated with a stream-based socket that is
   --  already connected.

   function Stream
     (Socket  : Socket_Type;
      Send_To : Sock_Addr_Type) return Stream_Access;
   --  Create a stream associated with a datagram-based socket that is already
   --  bound. Send_To is the socket address to which messages are being sent.

   function Get_Address
     (Stream : Stream_Access) return Sock_Addr_Type;
   --  Return the socket address from which the last message was received

   procedure Free is new Ada.Unchecked_Deallocation
     (Ada.Streams.Root_Stream_Type'Class, Stream_Access);
   --  Destroy a stream created by one of the Stream functions above,
   --  releasing the corresponding resources. The user is responsible for
   --  calling this subprogram when the stream is not needed anymore.

   type Socket_Set_Type is limited private;
   --  This type allows to manipulate sets of sockets. It allows to wait for
   --  events on multiple endpoints at one time. This is an access type on a
   --  system dependent structure. To avoid memory leaks it is highly
   --  recommended to clean the access value with procedure Empty.

   procedure Clear (Item : in out Socket_Set_Type; Socket : Socket_Type);
   --  Remove Socket from Item

   procedure Copy  (Source : Socket_Set_Type; Target : in out Socket_Set_Type);
   --  Copy Source into Target as Socket_Set_Type is limited private

   procedure Empty (Item : in out Socket_Set_Type);
   --  Remove all Sockets from Item and deallocate internal data

   procedure Get (Item : in out Socket_Set_Type; Socket : out Socket_Type);
   --  Extract a Socket from socket set Item. Socket is set to
   --  No_Socket when the set is empty.

   function Is_Empty
     (Item : Socket_Set_Type) return Boolean;
   --  Return True iff Item is empty

   function Is_Set
     (Item   : Socket_Set_Type;
      Socket : Socket_Type) return Boolean;
   --  Return True iff Socket is present in Item

   procedure Set (Item : in out Socket_Set_Type; Socket : Socket_Type);
   --  Insert Socket into Item

   --  The select(2) system call waits for events to occur on any of a set of
   --  file descriptors. Usually, three independent sets of descriptors are
   --  watched (read, write  and exception). A timeout gives an upper bound
   --  on the amount of time elapsed before select returns. This function
   --  blocks until an event occurs. On some platforms, the select(2) system
   --  can block the full process (not just the calling thread).
   --
   --  Check_Selector provides the very same behaviour. The only difference is
   --  that it does not watch for exception events. Note that on some
   --  platforms it is kept process blocking on purpose. The timeout parameter
   --  allows the user to have the behaviour he wants. Abort_Selector allows
   --  to abort safely a Check_Selector that is blocked forever. A special
   --  file descriptor is opened by Create_Selector and included in each call
   --  to Check_Selector. Abort_Selector causes an event to occur on this
   --  descriptor in order to unblock Check_Selector. The user must call
   --  Close_Selector to discard this special file. A reason to abort a select
   --  operation is typically to add a socket in one of the socket sets when
   --  the timeout is set to forever.

   type Selector_Type is limited private;
   type Selector_Access is access all Selector_Type;

   subtype Selector_Duration is Timeval_Duration;

   procedure Create_Selector (Selector : out Selector_Type);
   --  Create a new selector

   procedure Close_Selector (Selector : in out Selector_Type);
   --  Close Selector and all internal descriptors associated

   type Selector_Status is (Completed, Expired, Aborted);

   procedure Check_Selector
     (Selector     : in out Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever);
   --  Return when one Socket in R_Socket_Set has some data to be read or if
   --  one Socket in W_Socket_Set is ready to transmit some data. In these
   --  cases Status is set to Completed and sockets that are ready are set in
   --  R_Socket_Set or W_Socket_Set. Status is set to Expired if no socket was
   --  ready after a Timeout expiration. Status is set to Aborted if an abort
   --  signal has been received while checking socket status. As this
   --  procedure returns when Timeout occurs, it is a design choice to keep
   --  this procedure process blocking. Note that a Timeout of 0.0 returns
   --  immediately. Also note that two different Socket_Set_Type objects must
   --  be passed as R_Socket_Set and W_Socket_Set (even if they denote the
   --  same set of Sockets), or some event may be lost.
   --  Socket_Error is raised when the select(2) system call returns an
   --  error condition, or when a read error occurs on the signalling socket
   --  used for the implementation of Abort_Selector.

   procedure Check_Selector
     (Selector     : in out Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      E_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever);
   --  This refined version of Check_Selector allows to watch for exception
   --  events (that is notifications of out-of-band transmission and
   --  reception). As above, all of R_Socket_Set, W_Socket_Set and
   --  E_Socket_Set must be different objects.

   procedure Abort_Selector (Selector : Selector_Type);
   --  Send an abort signal to the selector

private

   type Socket_Type is new Integer;
   No_Socket : constant Socket_Type := -1;

   type Selector_Type is limited record
      R_Sig_Socket : Socket_Type;
      W_Sig_Socket : Socket_Type;
   end record;

   pragma Volatile (Selector_Type);

   --  The two signalling sockets are used to abort a select operation

   subtype Socket_Set_Access is System.Address;
   No_Socket_Set : constant Socket_Set_Access := System.Null_Address;

   type Socket_Set_Type is record
      Last : Socket_Type       := No_Socket;
      Set  : Socket_Set_Access := No_Socket_Set;
   end record;

   subtype Inet_Addr_Comp_Type is Natural range 0 .. 255;
   --  Octet for Internet address

   type Inet_Addr_VN_Type is array (Natural range <>) of Inet_Addr_Comp_Type;

   subtype Inet_Addr_V4_Type is Inet_Addr_VN_Type (1 ..  4);
   subtype Inet_Addr_V6_Type is Inet_Addr_VN_Type (1 .. 16);

   type Inet_Addr_Type (Family : Family_Type := Family_Inet) is record
      case Family is
         when Family_Inet =>
            Sin_V4 : Inet_Addr_V4_Type := (others => 0);

         when Family_Inet6 =>
            Sin_V6 : Inet_Addr_V6_Type := (others => 0);
      end case;
   end record;

   Any_Port : constant Port_Type := 0;
   No_Port  : constant Port_Type := 0;

   Any_Inet_Addr       : constant Inet_Addr_Type :=
                           (Family_Inet, (others => 0));
   No_Inet_Addr        : constant Inet_Addr_Type :=
                           (Family_Inet, (others => 0));
   Broadcast_Inet_Addr : constant Inet_Addr_Type :=
                           (Family_Inet, (others => 255));

   No_Sock_Addr : constant Sock_Addr_Type := (Family_Inet, No_Inet_Addr, 0);

   Max_Name_Length : constant := 64;
   --  The constant MAXHOSTNAMELEN is usually set to 64

   subtype Name_Index is Natural range 1 .. Max_Name_Length;

   type Name_Type
     (Length : Name_Index := Max_Name_Length)
   is record
      Name : String (1 .. Length);
   end record;
   --  We need fixed strings to avoid access types in host entry type

   type Name_Array is array (Natural range <>) of Name_Type;
   type Inet_Addr_Array is array (Natural range <>) of Inet_Addr_Type;

   type Host_Entry_Type (Aliases_Length, Addresses_Length : Natural) is record
      Official  : Name_Type;
      Aliases   : Name_Array (1 .. Aliases_Length);
      Addresses : Inet_Addr_Array (1 .. Addresses_Length);
   end record;

   type Service_Entry_Type (Aliases_Length : Natural) is record
      Official  : Name_Type;
      Aliases   : Name_Array (1 .. Aliases_Length);
      Port      : Port_Type;
      Protocol  : Name_Type;
   end record;

   type Request_Flag_Type is mod 2 ** 8;
   No_Request_Flag           : constant Request_Flag_Type := 0;
   Process_Out_Of_Band_Data  : constant Request_Flag_Type := 1;
   Peek_At_Incoming_Data     : constant Request_Flag_Type := 2;
   Wait_For_A_Full_Reception : constant Request_Flag_Type := 4;
   Send_End_Of_Record        : constant Request_Flag_Type := 8;

end GNAT.Sockets;
