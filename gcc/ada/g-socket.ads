------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . S O C K E T S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 2001 Ada Core Technologies, Inc.              --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an interface to the sockets communication facility
--  provided on many operating systems. Currently this is implemented on all
--  native GNAT ports except for VMS. It is not yet implemented for any of
--  the cross-ports (e.g. it is not available for VxWorks or LynxOS).
--  Another restriction is that there is no multicast support under Windows
--  or under any system on which the multicast support is not available or
--  installed.

with Ada.Exceptions;
with Ada.Streams;

package GNAT.Sockets is

   --  Sockets are designed to provide a consistent communication
   --  facility between applications. This package provides an
   --  Ada-like interface similar to the one proposed as part of the
   --  BSD socket layer. This is a system independent thick binding.
   --  Here is a typical example of what you can do.

   --  with GNAT.Sockets; use GNAT.Sockets;
   --
   --  with Ada.Text_IO;
   --  with Ada.Exceptions; use Ada.Exceptions;
   --
   --  procedure PingPong is
   --
   --     Group : constant String := "239.255.128.128";
   --     --  Multicast groupe: administratively scoped IP address
   --
   --     task Pong is
   --        entry Start;
   --        entry Stop;
   --     end Pong;
   --
   --     task body Pong is
   --        Address  : Sock_Addr_Type;
   --        Server   : Socket_Type;
   --        Socket   : Socket_Type;
   --        Channel  : Stream_Access;
   --
   --     begin
   --        accept Start;
   --
   --        --  Get an Internet address of a host (here "localhost").
   --        --  Note that a host can have several addresses. Here we get
   --        --  the first one which is supposed to be the official one.
   --
   --        Address.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
   --
   --        --  Get a socket address that is an Internet address and a port
   --
   --        Address.Port := 5432;
   --
   --        --  The first step is to create a socket. Once created, this
   --        --  socket must be associated to with an address. Usually only a
   --        --  server (Pong here) needs to bind an address explicitly.
   --        --  Most of the time clients can skip this step because the
   --        --  socket routines will bind an arbitrary address to an unbound
   --        --  socket.
   --
   --        Create_Socket (Server);
   --
   --        --  Allow reuse of local addresses.
   --
   --        Set_Socket_Option
   --          (Server,
   --           Socket_Level,
   --           (Reuse_Address, True));
   --
   --        Bind_Socket (Server, Address);
   --
   --        --  A server marks a socket as willing to receive connect events.
   --
   --        Listen_Socket (Server);
   --
   --        --  Once a server calls Listen_Socket, incoming connects events
   --        --  can be accepted. The returned Socket is a new socket that
   --        --  represents the server side of the connection. Server remains
   --        --  available to receive further connections.
   --
   --        Accept_Socket (Server, Socket, Address);
   --
   --        --  Return a stream associated to the connected socket.
   --
   --        Channel := Stream (Socket);
   --
   --        --  Force Pong to block
   --
   --        delay 0.2;
   --
   --        --  Receive and print message from client Ping.
   --
   --        declare
   --           Message : String := String'Input (Channel);
   --
   --        begin
   --           Ada.Text_IO.Put_Line (Message);
   --
   --           --  Send same message to server Pong.
   --
   --           String'Output (Channel, Message);
   --        end;
   --
   --        Close_Socket (Server);
   --        Close_Socket (Socket);
   --
   --        --  Part of the multicast example
   --
   --        --  Create a datagram socket to send connectionless, unreliable
   --        --  messages of a fixed maximum length.
   --
   --        Create_Socket (Socket, Family_Inet, Socket_Datagram);
   --
   --        --  Allow reuse of local addresses.
   --
   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));
   --
   --        --  Join a multicast group.
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));
   --
   --        --  Controls the live time of the datagram to avoid it being
   --        --  looped forever due to routing errors. Routers decrement
   --        --  the TTL of every datagram as it traverses from one network
   --        --  to another and when its value reaches 0 the packet is
   --        --  dropped. Default is 1.
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_TTL, 1));
   --
   --        --  Want the data you send to be looped back to your host.
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_Loop, True));
   --
   --        --  If this socket is intended to receive messages, bind it to a
   --        --  given socket address.
   --
   --        Address.Addr := Any_Inet_Addr;
   --        Address.Port := 55505;
   --
   --        Bind_Socket (Socket, Address);
   --
   --        --  If this socket is intended to send messages, provide the
   --        --  receiver socket address.
   --
   --        Address.Addr := Inet_Addr (Group);
   --        Address.Port := 55506;
   --
   --        Channel := Stream (Socket, Address);
   --
   --        --  Receive and print message from client Ping.
   --
   --        declare
   --           Message : String := String'Input (Channel);
   --
   --        begin
   --
   --           --  Get the address of the sender.
   --
   --           Address := Get_Address (Channel);
   --           Ada.Text_IO.Put_Line (Message & " from " & Image (Address));
   --
   --           --  Send same message to server Pong.
   --
   --           String'Output (Channel, Message);
   --        end;
   --
   --        Close_Socket (Socket);
   --
   --        accept Stop;
   --
   --     exception when E : others =>
   --        Ada.Text_IO.Put_Line
   --          (Exception_Name (E) & ": " & Exception_Message (E));
   --     end Pong;
   --
   --     task Ping is
   --        entry Start;
   --        entry Stop;
   --     end Ping;
   --
   --     task body Ping is
   --        Address  : Sock_Addr_Type;
   --        Socket   : Socket_Type;
   --        Channel  : Stream_Access;
   --
   --     begin
   --        accept Start;
   --
   --        --  See comments in Ping section for the first steps.
   --
   --        Address.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
   --        Address.Port := 5432;
   --        Create_Socket (Socket);
   --
   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));
   --
   --        --  Force Pong to block
   --
   --        delay 0.2;
   --
   --        --  If the client's socket is not bound, Connect_Socket will
   --        --  bind to an unused address. The client uses Connect_Socket to
   --        --  create a logical connection between the client's socket and
   --        --  a server's socket returned by Accept_Socket.
   --
   --        Connect_Socket (Socket, Address);
   --
   --        Channel := Stream (Socket);
   --
   --        --  Send message to server Pong.
   --
   --        String'Output (Channel, "Hello world");
   --
   --        --  Force Ping to block
   --
   --        delay 0.2;
   --
   --        --  Receive and print message from server Pong.
   --
   --        Ada.Text_IO.Put_Line (String'Input (Channel));
   --        Close_Socket (Socket);
   --
   --        --  Part of multicast example. Code similar to Pong's one.
   --
   --        Create_Socket (Socket, Family_Inet, Socket_Datagram);
   --
   --        Set_Socket_Option
   --          (Socket,
   --           Socket_Level,
   --           (Reuse_Address, True));
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_TTL, 1));
   --
   --        Set_Socket_Option
   --          (Socket,
   --           IP_Protocol_For_IP_Level,
   --           (Multicast_Loop, True));
   --
   --        Address.Addr := Any_Inet_Addr;
   --        Address.Port := 55506;
   --
   --        Bind_Socket (Socket, Address);
   --
   --        Address.Addr := Inet_Addr (Group);
   --        Address.Port := 55505;
   --
   --        Channel := Stream (Socket, Address);
   --
   --        --  Send message to server Pong.
   --
   --        String'Output (Channel, "Hello world");
   --
   --        --  Receive and print message from server Pong.
   --
   --        declare
   --           Message : String := String'Input (Channel);
   --
   --        begin
   --           Address := Get_Address (Channel);
   --           Ada.Text_IO.Put_Line (Message & " from " & Image (Address));
   --        end;
   --
   --        Close_Socket (Socket);
   --
   --        accept Stop;
   --
   --     exception when E : others =>
   --        Ada.Text_IO.Put_Line
   --          (Exception_Name (E) & ": " & Exception_Message (E));
   --     end Ping;
   --
   --  begin
   --     --  Indicate whether the thread library provides process
   --     --  blocking IO. Basically, if you are not using FSU threads
   --     --  the default is ok.
   --
   --     Initialize (Process_Blocking_IO => False);
   --     Ping.Start;
   --     Pong.Start;
   --     Ping.Stop;
   --     Pong.Stop;
   --     Finalize;
   --  end PingPong;

   procedure Initialize (Process_Blocking_IO : Boolean := False);
   --  Initialize must be called before using any socket routines. If
   --  the thread library provides process blocking IO - basically
   --  with FSU threads - GNAT.Sockets should be initialized with a
   --  value of True to simulate thread blocking IO. Further calls to
   --  Initialize will be ignored.

   procedure Finalize;
   --  After Finalize is called it is not possible to use any routines
   --  exported in by this package. This procedure is idempotent.

   type Socket_Type is private;
   --  Sockets are used to implement a reliable bi-directional
   --  point-to-point, stream-based connections between
   --  hosts. No_Socket provides a special value to denote
   --  uninitialized sockets.

   No_Socket : constant Socket_Type;

   Socket_Error : exception;
   --  There is only one exception in this package to deal with an
   --  error during a socket routine. Once raised, its message
   --  contains a string describing the error code.

   function Image (Socket : Socket_Type) return String;
   --  Return a printable string for Socket

   function To_C (Socket : Socket_Type) return Integer;
   --  Return a file descriptor to be used by external subprograms
   --  especially the C functions that are not yet interfaced in this
   --  package.

   type Family_Type is (Family_Inet, Family_Inet6);
   --  Address family (or protocol family) identifies the
   --  communication domain and groups protocols with similar address
   --  formats. IPv6 will soon be supported.

   type Mode_Type is (Socket_Stream, Socket_Datagram);
   --  Stream sockets provide connection-oriented byte
   --  streams. Datagram sockets support unreliable connectionless
   --  message based communication.

   type Shutmode_Type is (Shut_Read, Shut_Write, Shut_Read_Write);
   --  When a process closes a socket, the policy is to retain any
   --  data queued until either a delivery or a timeout expiration (in
   --  this case, the data are discarded). A finer control is
   --  available through shutdown. With Shut_Read, no more data can be
   --  received from the socket. With_Write, no more data can be
   --  transmitted. Neither transmission nor reception can be
   --  performed with Shut_Read_Write.

   type Port_Type is new Natural;
   --  Classical port definition. No_Port provides a special value to
   --  denote uninitialized port. Any_Port provides a special value
   --  enabling all ports.

   Any_Port : constant Port_Type;
   No_Port  : constant Port_Type;

   type Inet_Addr_Type (Family : Family_Type := Family_Inet) is private;
   --  An Internet address depends on an address family (IPv4 contains
   --  4 octets and Ipv6 contains 16 octets). Any_Inet_Address is a
   --  special value treated like a wildcard enabling all addresses.
   --  No_Inet_Addr provides a special value to denote uninitialized
   --  inet addresses.

   Any_Inet_Addr : constant Inet_Addr_Type;
   No_Inet_Addr  : constant Inet_Addr_Type;

   type Sock_Addr_Type (Family : Family_Type := Family_Inet) is record
      Addr : Inet_Addr_Type (Family);
      Port : Port_Type;
   end record;
   --  Socket addresses fully define a socket connection with a
   --  protocol family, an Internet address and a port. No_Sock_Addr
   --  provides a special value for uninitialized socket addresses.

   No_Sock_Addr : constant Sock_Addr_Type;

   function Image (Value : Inet_Addr_Type) return String;
   --  Return an image of an Internet address. IPv4 notation consists
   --  in 4 octets in decimal format separated by dots. IPv6 notation
   --  consists in 16 octets in hexadecimal format separated by
   --  colons (and possibly dots).

   function Image (Value : Sock_Addr_Type) return String;
   --  Return inet address image and port image separated by a colon.

   function Inet_Addr (Image : String) return Inet_Addr_Type;
   --  Convert address image from numbers-and-dots notation into an
   --  inet address.

   --  Host entries provide a complete information on a given host:
   --  the official name, an array of alternative names or aliases and
   --  array of network addresses.

   type Host_Entry_Type
     (Aliases_Length, Addresses_Length : Natural) is private;

   function Official_Name (E : Host_Entry_Type) return String;
   --  Return official name in host entry

   function Aliases_Length (E : Host_Entry_Type) return Natural;
   --  Return number of aliases in host entry

   function Addresses_Length (E : Host_Entry_Type) return Natural;
   --  Return number of addresses in host entry

   function Aliases
     (E    : Host_Entry_Type;
      N    : Positive := 1)
      return String;
   --  Return N'th aliases in host entry. The first index is 1.

   function Addresses
     (E    : Host_Entry_Type;
      N    : Positive := 1)
      return Inet_Addr_Type;
   --  Return N'th addresses in host entry. The first index is 1.

   Host_Error : exception;
   --  Exception raised by the two following procedures. Once raised,
   --  its message contains a string describing the error code. This
   --  exception is raised when an host entry can not be retrieved.

   function Get_Host_By_Address
     (Address : Inet_Addr_Type;
      Family  : Family_Type := Family_Inet)
      return    Host_Entry_Type;
   --  Return host entry structure for the given inet address

   function Get_Host_By_Name
     (Name : String)
      return Host_Entry_Type;
   --  Return host entry structure for the given host name

   function Host_Name return String;
   --  Return the name of the current host

   --  Errors are described by an enumeration type. There is only one
   --  exception Socket_Error in this package to deal with an error
   --  during a socket routine. Once raised, its message contains the
   --  error code between brackets and a string describing the error
   --  code.

   type Error_Type is
     (Permission_Denied,
      Address_Already_In_Use,
      Cannot_Assign_Requested_Address,
      Address_Family_Not_Supported_By_Protocol,
      Operation_Already_In_Progress,
      Bad_File_Descriptor,
      Connection_Refused,
      Bad_Address,
      Operation_Now_In_Progress,
      Interrupted_System_Call,
      Invalid_Argument,
      Input_Output_Error,
      Transport_Endpoint_Already_Connected,
      Message_Too_Long,
      Network_Is_Unreachable,
      No_Buffer_Space_Available,
      Protocol_Not_Available,
      Transport_Endpoint_Not_Connected,
      Operation_Not_Supported,
      Protocol_Not_Supported,
      Socket_Type_Not_Supported,
      Connection_Timed_Out,
      Resource_Temporarily_Unavailable,
      Unknown_Host,
      Host_Name_Lookup_Failure,
      No_Address_Associated_With_Name,
      Unknown_Server_Error,
      Cannot_Resolve_Error);

   --  Get_Socket_Options and Set_Socket_Options manipulate options
   --  associated with a socket. Options may exist at multiple
   --  protocol levels in the communication stack. Socket_Level is the
   --  uppermost socket level.

   type Level_Type is (
     Socket_Level,
     IP_Protocol_For_IP_Level,
     IP_Protocol_For_UDP_Level,
     IP_Protocol_For_TCP_Level);

   --  There are several options available to manipulate sockets. Each
   --  option has a name and several values available. Most of the
   --  time, the value is a boolean to enable or disable this option.

   type Option_Name is (
     Keep_Alive,      -- Enable sending of keep-alive messages
     Reuse_Address,   -- Allow bind to reuse local address
     Broadcast,       -- Enable datagram sockets to recv/send broadcast packets
     Send_Buffer,     -- Set/get the maximum socket send buffer in bytes
     Receive_Buffer,  -- Set/get the maximum socket recv buffer in bytes
     Linger,          -- Shutdown wait for msg to be sent or timeout occur
     Error,           -- Get and clear the pending socket error
     No_Delay,        -- Do not delay send to coalesce packets (TCP_NODELAY)
     Add_Membership,  -- Join a multicast group
     Drop_Membership, -- Leave a multicast group
     Multicast_TTL,   -- Indicates the time-to-live of sent multicast packets
     Multicast_Loop); -- Sent multicast packets are looped to the local socket

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
            Multiaddr : Inet_Addr_Type;
            Interface : Inet_Addr_Type;

         when Multicast_TTL   =>
            Time_To_Live : Natural;

      end case;
   end record;

   --  There are several controls available to manipulate
   --  sockets. Each option has a name and several values available.
   --  These controls differ from the socket options in that they are
   --  not specific to sockets but are available for any device.

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

   procedure Create_Socket
     (Socket : out Socket_Type;
      Family : Family_Type := Family_Inet;
      Mode   : Mode_Type   := Socket_Stream);
   --  Create an endpoint for communication. Raise Socket_Error on error.

   procedure Accept_Socket
     (Server  : Socket_Type;
      Socket  : out Socket_Type;
      Address : out Sock_Addr_Type);
   --  Extract the first connection request on the queue of pending
   --  connections, creates a new connected socket with mostly the
   --  same properties as Server, and allocates a new socket. The
   --  returned Address is filled in with the address of the
   --  connection. Raise Socket_Error on error.

   procedure Bind_Socket
     (Socket  : Socket_Type;
      Address : Sock_Addr_Type);
   --  Once a socket is created, assign a local address to it. Raise
   --  Socket_Error on error.

   procedure Close_Socket (Socket : Socket_Type);
   --  Close a socket and more specifically a non-connected socket.

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : in out Sock_Addr_Type);
   --  Make a connection to another socket which has the address of
   --  Server. Raise Socket_Error on error.

   procedure Control_Socket
     (Socket  : Socket_Type;
      Request : in out Request_Type);
   --  Obtain or set parameter values that control the socket. This
   --  control differs from the socket options in that they are not
   --  specific to sockets but are avaiable for any device.

   function Get_Peer_Name (Socket : Socket_Type) return Sock_Addr_Type;
   --  Return the peer or remote socket address of a socket. Raise
   --  Socket_Error on error.

   function Get_Socket_Name (Socket : Socket_Type) return Sock_Addr_Type;
   --  Return the local or current socket address of a socket. Raise
   --  Socket_Error on error.

   function Get_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Name   : Option_Name)
      return   Option_Type;
   --  Get the options associated with a socket. Raise Socket_Error on
   --  error.

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Positive := 15);
   --  To accept connections, a socket is first created with
   --  Create_Socket, a willingness to accept incoming connections and
   --  a queue Length for incoming connections are specified. Raise
   --  Socket_Error on error.

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive message from Socket. Last is the index value such that
   --  Item (Last) is the last character assigned. Note that Last is
   --  set to Item'First - 1 when the socket has been closed by
   --  peer. This is not an error and no exception is raised. Raise
   --  Socket_Error on error.

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      From   : out Sock_Addr_Type);
   --  Receive message from Socket. If Socket is not
   --  connection-oriented, the source address From of the message is
   --  filled in. Last is the index value such that Item (Last) is the
   --  last character assigned. Raise Socket_Error on error.

   function Resolve_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
     return        Error_Type;
   --  When Socket_Error or Host_Error are raised, the exception
   --  message contains the error code between brackets and a string
   --  describing the error code. Resolve_Error extracts the error
   --  code from an exception message and translate it into an
   --  enumeration value.

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Transmit a message to another socket. Note that Last is set to
   --  Item'First when socket has been closed by peer. This is not an
   --  error and no exception is raised. Raise Socket_Error on error;

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : Sock_Addr_Type);
   --  Transmit a message to another socket. The address is given by
   --  To. Raise Socket_Error on error;

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Option : Option_Type);
   --  Manipulate socket options. Raise Socket_Error on error.

   procedure Shutdown_Socket
     (Socket : Socket_Type;
      How    : Shutmode_Type := Shut_Read_Write);
   --  Shutdown a connected socket. If How is Shut_Read, further
   --  receives will be disallowed. If How is Shut_Write, further
   --  sends will be disallowed. If how is Shut_Read_Write, further
   --  sends and receives will be disallowed.

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
   --  Same interface as Ada.Streams.Stream_IO

   function Stream
     (Socket : Socket_Type)
      return   Stream_Access;
   --  Associate a stream with a stream-based socket that is already
   --  connected.

   function Stream
     (Socket  : Socket_Type;
      Send_To : Sock_Addr_Type)
      return    Stream_Access;
   --  Associate a stream with a datagram-based socket that is already
   --  bound. Send_To is the socket address to which messages are
   --  being sent.

   function Get_Address
     (Stream : Stream_Access)
     return Sock_Addr_Type;
   --  Return the socket address from which the last message was
   --  received.

   type Socket_Set_Type is private;
   --  This type allows to manipulate sets of sockets. It allows to
   --  wait for events on multiple endpoints at one time. This is an
   --  access type on a system dependent structure. To avoid memory
   --  leaks it is highly recommended to clean the access value with
   --  procedure Empty.

   procedure Clear (Item : in out Socket_Set_Type; Socket : Socket_Type);
   --  Remove Socket from Item

   procedure Set   (Item : in out Socket_Set_Type; Socket : Socket_Type);
   --  Insert Socket into Item

   procedure Empty (Item : in out Socket_Set_Type);
   --  Remove all Sockets from Item and deallocate internal data

   function Is_Empty
     (Item : Socket_Set_Type)
      return  Boolean;
   --  Return True if Item is empty

   function Is_Set
     (Item   : Socket_Set_Type;
      Socket : Socket_Type)
      return   Boolean;
   --  Return True if Socket is present in Item

   --  C select() waits for a number of file descriptors to change
   --  status. Usually, three independent sets of descriptors are
   --  watched (read, write and exception). A timeout gives an upper
   --  bound on the amount of time elapsed before select returns.
   --  This function blocks until an event occurs. On some platforms,
   --  C select can block the full process.
   --
   --  Check_Selector provides the very same behaviour. The only
   --  difference is that it does not watch for exception events. Note
   --  that on some platforms it is kept process blocking in purpose.
   --  The timeout parameter allows the user to have the behaviour he
   --  wants. Abort_Selector allows to abort safely a Check_Selector
   --  that is blocked forever. A special file descriptor is opened by
   --  Create_Selector and included in each call to
   --  Check_Selector. Abort_Selector causes an event to occur on this
   --  descriptor in order to unblock Check_Selector. The user must
   --  call Close_Selector to discard this special file. A reason to
   --  abort a select operation is typically to add a socket in one of
   --  the socket sets when the timeout is set to forever.

   Forever : constant Duration;

   type Selector_Type is limited private;
   type Selector_Access is access all Selector_Type;

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
      Timeout      : Duration := Forever);
   --  Return when one Socket in R_Socket_Set has some data to be read
   --  or if one Socket in W_Socket_Set is ready to receive some
   --  data. In these cases Status is set to Completed and sockets
   --  that are ready are set in R_Socket_Set or W_Socket_Set. Status
   --  is set to Expired if no socket was ready after a Timeout
   --  expiration. Status is set to Aborted if an abort signal as been
   --  received while checking socket status. As this procedure
   --  returns when Timeout occurs, it is a design choice to keep this
   --  procedure process blocking. Note that a Timeout of 0.0 returns
   --  immediatly.

   procedure Abort_Selector (Selector : Selector_Type);
   --  Send an abort signal to the selector.

private

   type Socket_Type is new Integer;
   No_Socket : constant Socket_Type := -1;

   Forever : constant Duration := Duration'Last;

   type Selector_Type is limited record
      R_Sig_Socket : Socket_Type;
      W_Sig_Socket : Socket_Type;
      In_Progress  : Boolean := False;
   end record;
   --  The two signalling sockets are used to abort a select
   --  operation.

   type Socket_Set_Record;
   type Socket_Set_Type is access all Socket_Set_Record;

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

   Any_Inet_Addr : constant Inet_Addr_Type := (Family_Inet, (others => 0));
   No_Inet_Addr  : constant Inet_Addr_Type := (Family_Inet, (others => 0));

   No_Sock_Addr  : constant Sock_Addr_Type := (Family_Inet, No_Inet_Addr, 0);

   Max_Host_Name_Length : constant := 64;
   --  The constant MAXHOSTNAMELEN is usually set to 64

   subtype Host_Name_Index is Natural range 1 .. Max_Host_Name_Length;

   type Host_Name_Type
     (Length : Host_Name_Index := Max_Host_Name_Length)
   is record
      Name : String (1 .. Length);
   end record;
   --  We need fixed strings to avoid access types in host entry type

   type Host_Name_Array is array (Natural range <>) of Host_Name_Type;
   type Inet_Addr_Array is array (Natural range <>) of Inet_Addr_Type;

   type Host_Entry_Type (Aliases_Length, Addresses_Length : Natural) is record
      Official  : Host_Name_Type;
      Aliases   : Host_Name_Array (1 .. Aliases_Length);
      Addresses : Inet_Addr_Array (1 .. Addresses_Length);
   end record;

end GNAT.Sockets;
