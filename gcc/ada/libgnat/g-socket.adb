------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . S O C K E T S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2024, AdaCore                     --
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

with Ada.Containers.Generic_Array_Sort;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Finalization;
with Ada.Streams;              use Ada.Streams;
with Ada.Unchecked_Conversion;

with GNAT.Sockets.Thin;        use GNAT.Sockets.Thin;
with GNAT.Sockets.Thin_Common; use GNAT.Sockets.Thin_Common;

with GNAT.Sockets.Linker_Options;
pragma Warnings (Off, GNAT.Sockets.Linker_Options);
--  Need to include pragma Linker_Options which is platform dependent

with GNAT.Sockets.Poll;

with System;               use System;
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;
with System.Task_Lock;

package body GNAT.Sockets is

   package C renames Interfaces.C;

   type IPV6_Mreq is record
      ipv6mr_multiaddr : In6_Addr;
      ipv6mr_interface : C.unsigned;
   end record with Convention => C;
   --  Record to Add/Drop_Membership for multicast in IPv6

   ENOERROR : constant := 0;

   Netdb_Buffer_Size : constant := SOSC.Need_Netdb_Buffer * 1024;
   Need_Netdb_Lock   : constant Boolean := SOSC.Need_Netdb_Lock /= 0;
   --  The network database functions gethostbyname, gethostbyaddr,
   --  getservbyname and getservbyport can either be guaranteed task safe by
   --  the operating system, or else return data through a user-provided buffer
   --  to ensure concurrent uses do not interfere.

   --  Correspondence tables

   Levels : constant array (Level_Type) of C.int :=
              [Socket_Level               => SOSC.SOL_SOCKET,
               IP_Protocol_For_IP_Level   => SOSC.IPPROTO_IP,
               IP_Protocol_For_IPv6_Level => SOSC.IPPROTO_IPV6,
               IP_Protocol_For_UDP_Level  => SOSC.IPPROTO_UDP,
               IP_Protocol_For_TCP_Level  => SOSC.IPPROTO_TCP,
               IP_Protocol_For_ICMP_Level => SOSC.IPPROTO_ICMP,
               IP_Protocol_For_IGMP_Level => SOSC.IPPROTO_IGMP,
               IP_Protocol_For_RAW_Level  => SOSC.IPPROTO_RAW];

   Modes : constant array (Mode_Type) of C.int :=
             [Socket_Stream   => SOSC.SOCK_STREAM,
              Socket_Datagram => SOSC.SOCK_DGRAM,
              Socket_Raw      => SOSC.SOCK_RAW];

   Shutmodes : constant array (Shutmode_Type) of C.int :=
                 [Shut_Read       => SOSC.SHUT_RD,
                  Shut_Write      => SOSC.SHUT_WR,
                  Shut_Read_Write => SOSC.SHUT_RDWR];

   Requests : constant array (Request_Name) of SOSC.IOCTL_Req_T :=
                [Non_Blocking_IO => SOSC.FIONBIO,
                 N_Bytes_To_Read => SOSC.FIONREAD];

   Options : constant array (Specific_Option_Name) of C.int :=
               [Keep_Alive          => SOSC.SO_KEEPALIVE,
                Keep_Alive_Count    => SOSC.TCP_KEEPCNT,
                Keep_Alive_Idle     => SOSC.TCP_KEEPIDLE,
                Keep_Alive_Interval => SOSC.TCP_KEEPINTVL,
                Reuse_Address       => SOSC.SO_REUSEADDR,
                Broadcast           => SOSC.SO_BROADCAST,
                Send_Buffer         => SOSC.SO_SNDBUF,
                Receive_Buffer      => SOSC.SO_RCVBUF,
                Linger              => SOSC.SO_LINGER,
                Error               => SOSC.SO_ERROR,
                No_Delay            => SOSC.TCP_NODELAY,
                Add_Membership_V4   => SOSC.IP_ADD_MEMBERSHIP,
                Drop_Membership_V4  => SOSC.IP_DROP_MEMBERSHIP,
                Multicast_If_V4     => SOSC.IP_MULTICAST_IF,
                Multicast_Loop_V4   => SOSC.IP_MULTICAST_LOOP,
                Receive_Packet_Info => SOSC.IP_PKTINFO,
                Multicast_TTL       => SOSC.IP_MULTICAST_TTL,
                Add_Membership_V6   => SOSC.IPV6_ADD_MEMBERSHIP,
                Drop_Membership_V6  => SOSC.IPV6_DROP_MEMBERSHIP,
                Multicast_If_V6     => SOSC.IPV6_MULTICAST_IF,
                Multicast_Loop_V6   => SOSC.IPV6_MULTICAST_LOOP,
                Multicast_Hops      => SOSC.IPV6_MULTICAST_HOPS,
                IPv6_Only           => SOSC.IPV6_V6ONLY,
                Send_Timeout        => SOSC.SO_SNDTIMEO,
                Receive_Timeout     => SOSC.SO_RCVTIMEO,
                Busy_Polling        => SOSC.SO_BUSY_POLL,
                Bind_To_Device      => SOSC.SO_BINDTODEVICE];
   --  ??? Note: for OpenSolaris, Receive_Packet_Info should be IP_RECVPKTINFO,
   --  but for Linux compatibility this constant is the same as IP_PKTINFO.

   Flags : constant array (0 .. 3) of C.int :=
             [0 => SOSC.MSG_OOB,     --  Process_Out_Of_Band_Data
              1 => SOSC.MSG_PEEK,    --  Peek_At_Incoming_Data
              2 => SOSC.MSG_WAITALL, --  Wait_For_A_Full_Reception
              3 => SOSC.MSG_EOR];    --  Send_End_Of_Record

   Socket_Error_Id : constant Exception_Id := Socket_Error'Identity;
   Host_Error_Id   : constant Exception_Id := Host_Error'Identity;

   type In_Addr_Union (Family : Family_Inet_4_6) is record
      case Family is
         when Family_Inet =>
            In4 : In_Addr;
         when Family_Inet6 =>
            In6 : In6_Addr;
      end case;
   end record with Unchecked_Union;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Resolve_Error
     (Error_Value : Integer;
      From_Errno  : Boolean := True) return Error_Type;
   --  Associate an enumeration value (error_type) to an error value (errno).
   --  From_Errno prevents from mixing h_errno with errno.

   function To_Name   (N  : String) return Name_Type;
   function To_String (HN : Name_Type) return String;
   --  Conversion functions

   function To_Int (F : Request_Flag_Type) return C.int;
   --  Return the int value corresponding to the specified flags combination

   function Set_Forced_Flags (F : C.int) return C.int;
   --  Return F with the bits from SOSC.MSG_Forced_Flags forced set

   procedure Netdb_Lock;
   pragma Inline (Netdb_Lock);
   procedure Netdb_Unlock;
   pragma Inline (Netdb_Unlock);
   --  Lock/unlock operation used to protect netdb access for platforms that
   --  require such protection.

   function To_Host_Entry (E : Hostent_Access) return Host_Entry_Type;
   --  Conversion function

   function To_Service_Entry (E : Servent_Access) return Service_Entry_Type;
   --  Conversion function

   function Value (S : System.Address) return String;
   --  Same as Interfaces.C.Strings.Value but taking a System.Address

   function To_Timeval (Val : Timeval_Duration) return Timeval;
   --  Separate Val in seconds and microseconds

   function To_Duration (Val : Timeval) return Timeval_Duration;
   --  Reconstruct a Duration value from a Timeval record (seconds and
   --  microseconds).

   function Dedot (Value : String) return String
   is (if Value /= "" and then Value (Value'Last) = '.'
       then Value (Value'First .. Value'Last - 1)
       else Value);
   --  Removes dot at the end of error message

   procedure Raise_Host_Error (H_Error : Integer; Name : String)
   with No_Return;
   --  Raise Host_Error exception with message describing error code (note
   --  hstrerror seems to be obsolete) from h_errno. Name is the name
   --  or address that was being looked up.

   procedure Raise_GAI_Error (RC : C.int; Name : String)
   with No_Return;
   --  Raise Host_Error with exception message in case of errors in
   --  getaddrinfo and getnameinfo.

   function Is_Windows return Boolean with Inline;
   --  Returns True on Windows platform

   procedure Narrow (Item : in out Socket_Set_Type);
   --  Update Last as it may be greater than the real last socket

   procedure Check_For_Fd_Set (Fd : Socket_Type);
   pragma Inline (Check_For_Fd_Set);
   --  Raise Constraint_Error if Fd is less than 0 or greater than or equal to
   --  FD_SETSIZE, on platforms where fd_set is a bitmap.

   function Connect_Socket
     (Socket : Socket_Type;
      Server : Sock_Addr_Type) return C.int;
   pragma Inline (Connect_Socket);
   --  Underlying implementation for the Connect_Socket procedures

   --  Types needed for Datagram_Socket_Stream_Type

   type Datagram_Socket_Stream_Type is new Root_Stream_Type with record
      Socket : Socket_Type;
      To     : Sock_Addr_Type;
      From   : Sock_Addr_Type;
   end record;

   type Datagram_Socket_Stream_Access is
     access all Datagram_Socket_Stream_Type;

   procedure Read
     (Stream : in out Datagram_Socket_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Datagram_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   --  Types needed for Stream_Socket_Stream_Type

   type Stream_Socket_Stream_Type is new Root_Stream_Type with record
      Socket : Socket_Type;
   end record;

   type Stream_Socket_Stream_Access is
     access all Stream_Socket_Stream_Type;

   procedure Read
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Wait_On_Socket
     (Socket   : Socket_Type;
      Event    : Poll.Wait_Event_Set;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status);
   --  Common code for variants of socket operations supporting a timeout:
   --  block in Poll.Wait on Socket for at most the indicated timeout.
   --  Event parameter defines what the Poll.Wait is waiting for.

   type Sockets_Library_Controller is new Ada.Finalization.Limited_Controlled
     with null record;
   --  This type is used to generate automatic calls to Initialize and Finalize
   --  during the elaboration and finalization of this package. A single object
   --  of this type must exist at library level.

   function Err_Code_Image (E : Integer) return String;
   --  Return the value of E surrounded with brackets

   procedure Initialize (X : in out Sockets_Library_Controller);
   procedure Finalize   (X : in out Sockets_Library_Controller);

   procedure Normalize_Empty_Socket_Set (S : in out Socket_Set_Type);
   --  If S is the empty set (detected by Last = No_Socket), make sure its
   --  fd_set component is actually cleared. Note that the case where it is
   --  not can occur for an uninitialized Socket_Set_Type object.

   function Is_Open (S : Selector_Type) return Boolean;
   --  Return True for an "open" Selector_Type object, i.e. one for which
   --  Create_Selector has been called and Close_Selector has not been called,
   --  or the null selector.

   function Create_Address
     (Family : Family_Inet_4_6; Bytes : Inet_Addr_Bytes) return Inet_Addr_Type
     with Inline;
   --  Creates address from family and Inet_Addr_Bytes array

   function Get_Bytes (Addr : Inet_Addr_Type) return Inet_Addr_Bytes
     with Inline;
   --  Extract bytes from address

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Request_Flag_Type) return Request_Flag_Type is
   begin
      return L or R;
   end "+";

   --------------------
   -- Abort_Selector --
   --------------------

   procedure Abort_Selector (Selector : Selector_Type) is
      Res : C.int;

   begin
      if not Is_Open (Selector) then
         raise Program_Error with "closed selector";

      elsif Selector.Is_Null then
         raise Program_Error with "null selector";

      end if;

      --  Send one byte to unblock select system call

      Res := Signalling_Fds.Write (C.int (Selector.W_Sig_Socket));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Abort_Selector;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Server  : Socket_Type;
      Socket  : out Socket_Type;
      Address : out Sock_Addr_Type)
   is
      Res : C.int;
      Sin : aliased Sockaddr;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Res := C_Accept (C.int (Server), Sin'Address, Len'Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Socket := Socket_Type (Res);
      Address := Get_Address (Sin, Len);
   end Accept_Socket;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Server   : Socket_Type;
      Socket   : out Socket_Type;
      Address  : out Sock_Addr_Type;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status)
   is
   begin
      if Selector /= null and then not Is_Open (Selector.all) then
         raise Program_Error with "closed selector";
      end if;

      --  Wait for socket to become available for reading

      Wait_On_Socket
        (Socket   => Server,
         Event    => Poll.Input_Event,
         Timeout  => Timeout,
         Selector => Selector,
         Status   => Status);

      --  Accept connection if available

      if Status = Completed then
         Accept_Socket (Server, Socket, Address);
      else
         Socket := No_Socket;
      end if;
   end Accept_Socket;

   ---------------
   -- Addresses --
   ---------------

   function Addresses
     (E : Host_Entry_Type;
      N : Positive := 1) return Inet_Addr_Type
   is
   begin
      return E.Addresses (N);
   end Addresses;

   ----------------------
   -- Addresses_Length --
   ----------------------

   function Addresses_Length (E : Host_Entry_Type) return Natural is
   begin
      return E.Addresses_Length;
   end Addresses_Length;

   -------------
   -- Aliases --
   -------------

   function Aliases
     (E : Host_Entry_Type;
      N : Positive := 1) return String
   is
   begin
      return To_String (E.Aliases (N));
   end Aliases;

   -------------
   -- Aliases --
   -------------

   function Aliases
     (S : Service_Entry_Type;
      N : Positive := 1) return String
   is
   begin
      return To_String (S.Aliases (N));
   end Aliases;

   --------------------
   -- Aliases_Length --
   --------------------

   function Aliases_Length (E : Host_Entry_Type) return Natural is
   begin
      return E.Aliases_Length;
   end Aliases_Length;

   --------------------
   -- Aliases_Length --
   --------------------

   function Aliases_Length (S : Service_Entry_Type) return Natural is
   begin
      return S.Aliases_Length;
   end Aliases_Length;

   -----------------
   -- Bind_Socket --
   -----------------

   procedure Bind_Socket
     (Socket  : Socket_Type;
      Address : Sock_Addr_Type)
   is
      Res : C.int;
      Sin : aliased Sockaddr;
      Len : C.int;

   begin
      Set_Address (Sin'Unchecked_Access, Address, Len);

      Res := C_Bind (C.int (Socket), Sin'Address, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Bind_Socket;

   ----------------------
   -- Check_For_Fd_Set --
   ----------------------

   procedure Check_For_Fd_Set (Fd : Socket_Type) is
   begin
      --  On Windows, fd_set is a FD_SETSIZE array of socket ids:
      --  no check required. Warnings suppressed because condition
      --  is known at compile time.

      if Is_Windows then

         return;

      --  On other platforms, fd_set is an FD_SETSIZE bitmap: check
      --  that Fd is within range (otherwise behavior is undefined).

      elsif Fd < 0 or else Fd >= SOSC.FD_SETSIZE then
         raise Constraint_Error
           with "invalid value for socket set: " & Image (Fd);
      end if;
   end Check_For_Fd_Set;

   --------------------
   -- Check_Selector --
   --------------------

   procedure Check_Selector
     (Selector     : Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever)
   is
      E_Socket_Set : Socket_Set_Type;
   begin
      Check_Selector
        (Selector, R_Socket_Set, W_Socket_Set, E_Socket_Set, Status, Timeout);
   end Check_Selector;

   procedure Check_Selector
     (Selector     : Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      E_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever)
   is
      Res  : C.int;
      Last : C.int;
      RSig : Socket_Type := No_Socket;
      TVal : aliased Timeval;
      TPtr : Timeval_Access;

   begin
      if not Is_Open (Selector) then
         raise Program_Error with "closed selector";
      end if;

      Status := Completed;

      --  No timeout or Forever is indicated by a null timeval pointer

      if Timeout = Forever then
         TPtr := null;
      else
         TVal := To_Timeval (Timeout);
         TPtr := TVal'Unchecked_Access;
      end if;

      --  Add read signalling socket, if present

      if not Selector.Is_Null then
         RSig := Selector.R_Sig_Socket;
         Set (R_Socket_Set, RSig);
      end if;

      Last := C.int'Max (C.int'Max (C.int (R_Socket_Set.Last),
                                    C.int (W_Socket_Set.Last)),
                                    C.int (E_Socket_Set.Last));

      --  Zero out fd_set for empty Socket_Set_Type objects

      Normalize_Empty_Socket_Set (R_Socket_Set);
      Normalize_Empty_Socket_Set (W_Socket_Set);
      Normalize_Empty_Socket_Set (E_Socket_Set);

      Res :=
        C_Select
         (Last + 1,
          R_Socket_Set.Set'Access,
          W_Socket_Set.Set'Access,
          E_Socket_Set.Set'Access,
          TPtr);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      --  If Select was resumed because of read signalling socket, read this
      --  data and remove socket from set.

      if RSig /= No_Socket and then Is_Set (R_Socket_Set, RSig) then
         Clear (R_Socket_Set, RSig);

         Res := Signalling_Fds.Read (C.int (RSig));

         if Res = Failure then
            Raise_Socket_Error (Socket_Errno);
         end if;

         Status := Aborted;

      elsif Res = 0 then
         Status := Expired;
      end if;

      --  Update socket sets in regard to their new contents

      Narrow (R_Socket_Set);
      Narrow (W_Socket_Set);
      Narrow (E_Socket_Set);
   end Check_Selector;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Item   : in out Socket_Set_Type;
      Socket : Socket_Type)
   is
      Last : aliased C.int := C.int (Item.Last);

   begin
      Check_For_Fd_Set (Socket);

      if Item.Last /= No_Socket then
         Remove_Socket_From_Set (Item.Set'Access, C.int (Socket));
         Last_Socket_In_Set (Item.Set'Access, Last'Unchecked_Access);
         Item.Last := Socket_Type (Last);
      end if;
   end Clear;

   --------------------
   -- Close_Selector --
   --------------------

   procedure Close_Selector (Selector : in out Selector_Type) is
   begin
      --  Nothing to do if selector already in closed state

      if Selector.Is_Null or else not Is_Open (Selector) then
         return;
      end if;

      --  Close the signalling file descriptors used internally for the
      --  implementation of Abort_Selector.

      Signalling_Fds.Close (C.int (Selector.R_Sig_Socket));
      Signalling_Fds.Close (C.int (Selector.W_Sig_Socket));

      --  Reset R_Sig_Socket and W_Sig_Socket to No_Socket to ensure that any
      --  (erroneous) subsequent attempt to use this selector properly fails.

      Selector.R_Sig_Socket := No_Socket;
      Selector.W_Sig_Socket := No_Socket;
   end Close_Selector;

   ------------------
   -- Close_Socket --
   ------------------

   procedure Close_Socket (Socket : Socket_Type) is
      Res : C.int;

   begin
      Res := C_Close (C.int (Socket));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Close_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   function Connect_Socket
     (Socket : Socket_Type;
      Server : Sock_Addr_Type) return C.int
   is
      Sin : aliased Sockaddr;
      Len : C.int;
   begin
      Set_Address (Sin'Unchecked_Access, Server, Len);

      return C_Connect (C.int (Socket), Sin'Address, Len);
   end Connect_Socket;

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : Sock_Addr_Type)
   is
   begin
      if Connect_Socket (Socket, Server) = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Connect_Socket;

   procedure Connect_Socket
     (Socket   : Socket_Type;
      Server   : Sock_Addr_Type;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status)
   is
      Req : Request_Type;
      --  Used to set Socket to non-blocking I/O

      Conn_Err : aliased Integer;
      --  Error status of the socket after completion of select(2)

      Res           : C.int;
      Conn_Err_Size : aliased C.int := Conn_Err'Size / 8;
      --  For getsockopt(2) call

   begin
      if Selector /= null and then not Is_Open (Selector.all) then
         raise Program_Error with "closed selector";
      end if;

      --  Set the socket to non-blocking I/O

      Req := (Name => Non_Blocking_IO, Enabled => True);
      Control_Socket (Socket, Request => Req);

      --  Start operation (non-blocking), will return Failure with errno set
      --  to EINPROGRESS.

      Res := Connect_Socket (Socket, Server);
      if Res = Failure then
         Conn_Err := Socket_Errno;
         if Conn_Err /= SOSC.EINPROGRESS then
            Raise_Socket_Error (Conn_Err);
         end if;
      end if;

      --  Wait for socket to become available for writing (unless the Timeout
      --  is zero, in which case we consider that it has already expired, and
      --  we do not need to wait at all).

      if Timeout = 0.0 then
         Status := Expired;

      else
         Wait_On_Socket
           (Socket   => Socket,
            Event    => Poll.Output_Event,
            Timeout  => Timeout,
            Selector => Selector,
            Status   => Status);
      end if;

      --  Check error condition (the asynchronous connect may have terminated
      --  with an error, e.g. ECONNREFUSED) if select(2) completed.

      if Status = Completed then
         Res := C_Getsockopt
           (C.int (Socket), SOSC.SOL_SOCKET, SOSC.SO_ERROR,
            Conn_Err'Address, Conn_Err_Size'Access);

         if Res /= 0 then
            Conn_Err := Socket_Errno;
         end if;

      else
         Conn_Err := 0;
      end if;

      --  Reset the socket to blocking I/O

      Req := (Name => Non_Blocking_IO, Enabled => False);
      Control_Socket (Socket, Request => Req);

      --  Report error condition if any

      if Conn_Err /= 0 then
         Raise_Socket_Error (Conn_Err);
      end if;
   end Connect_Socket;

   --------------------
   -- Control_Socket --
   --------------------

   procedure Control_Socket
     (Socket  : Socket_Type;
      Request : in out Request_Type)
   is
      Arg : aliased C.int;
      Res : C.int;

   begin
      case Request.Name is
         when Non_Blocking_IO =>
            Arg := C.int (Boolean'Pos (Request.Enabled));

         when N_Bytes_To_Read =>
            null;
      end case;

      Res := Socket_Ioctl
               (C.int (Socket), Requests (Request.Name), Arg'Unchecked_Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      case Request.Name is
         when Non_Blocking_IO =>
            null;

         when N_Bytes_To_Read =>
            Request.Size := Natural (Arg);
      end case;
   end Control_Socket;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : Socket_Set_Type;
      Target : out Socket_Set_Type)
   is
   begin
      Target := Source;
   end Copy;

   ---------------------
   -- Create_Selector --
   ---------------------

   procedure Create_Selector (Selector : out Selector_Type) is
      Two_Fds : aliased Fd_Pair;
      Res     : C.int;

   begin
      if Is_Open (Selector) then
         --  Raise exception to prevent socket descriptor leak

         raise Program_Error with "selector already open";
      end if;

      --  We open two signalling file descriptors. One of them is used to send
      --  data to the other, which is included in a C_Select socket set. The
      --  communication is used to force a call to C_Select to complete, and
      --  the waiting task to resume its execution.

      Res := Signalling_Fds.Create (Two_Fds'Access);
      pragma Annotate (CodePeer, Modified, Two_Fds);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Selector.R_Sig_Socket := Socket_Type (Two_Fds (Read_End));
      Selector.W_Sig_Socket := Socket_Type (Two_Fds (Write_End));
   end Create_Selector;

   -------------------
   -- Create_Socket --
   -------------------

   procedure Create_Socket
     (Socket : out Socket_Type;
      Family : Family_Type := Family_Inet;
      Mode   : Mode_Type   := Socket_Stream;
      Level  : Level_Type  := IP_Protocol_For_IP_Level)
   is
      Res : C.int;

   begin
      Res := C_Socket (Families (Family), Modes (Mode), Levels (Level));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Socket := Socket_Type (Res);
   end Create_Socket;

   ------------------------
   -- Create_Socket_Pair --
   ------------------------

   procedure Create_Socket_Pair
     (Left   : out Socket_Type;
      Right  : out Socket_Type;
      Family : Family_Type := Family_Unspec;
      Mode   : Mode_Type   := Socket_Stream;
      Level  : Level_Type  := IP_Protocol_For_IP_Level)
   is
      Res  : C.int;
      Pair : aliased Thin_Common.Fd_Pair;

   begin
      Res := C_Socketpair
        ((if Family = Family_Unspec then Default_Socket_Pair_Family
          else Families (Family)),
         Modes (Mode), Levels (Level), Pair'Access);
      pragma Annotate (CodePeer, Modified, Pair);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Left  := Socket_Type (Pair (Pair'First));
      Right := Socket_Type (Pair (Pair'Last));
   end Create_Socket_Pair;

   -----------
   -- Empty --
   -----------

   procedure Empty (Item : out Socket_Set_Type) is
   begin
      Reset_Socket_Set (Item.Set'Access);
      Item.Last := No_Socket;
   end Empty;

   --------------------
   -- Err_Code_Image --
   --------------------

   function Err_Code_Image (E : Integer) return String is
      Msg : String := E'Img & "] ";
   begin
      Msg (Msg'First) := '[';
      return Msg;
   end Err_Code_Image;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Sockets_Library_Controller) is
      pragma Unreferenced (X);

   begin
      --  Finalization operation for the GNAT.Sockets package

      Thin.Finalize;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      --  This is a dummy placeholder for an obsolete API.
      --  The real finalization actions are in Initialize primitive operation
      --  of Sockets_Library_Controller.

      null;
   end Finalize;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item   : in out Socket_Set_Type;
      Socket : out Socket_Type)
   is
      S : aliased C.int;
      L : aliased C.int := C.int (Item.Last);

   begin
      if Item.Last /= No_Socket then
         Get_Socket_From_Set
           (Item.Set'Access, Last => L'Access, Socket => S'Access);
         pragma Annotate (CodePeer, Modified, L);
         pragma Annotate (CodePeer, Modified, S);

         Item.Last := Socket_Type (L);
         Socket    := Socket_Type (S);

      else
         Socket := No_Socket;
      end if;
   end Get;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (Stream : not null Stream_Access) return Sock_Addr_Type
   is
   begin
      if Stream.all in Datagram_Socket_Stream_Type then
         return Datagram_Socket_Stream_Type (Stream.all).From;
      else
         return Get_Peer_Name (Stream_Socket_Stream_Type (Stream.all).Socket);
      end if;
   end Get_Address;

   ---------------------
   -- Raise_GAI_Error --
   ---------------------

   procedure Raise_GAI_Error (RC : C.int; Name : String) is
   begin
      if RC = SOSC.EAI_SYSTEM then
         declare
            Errcode : constant Integer := Socket_Errno;
         begin
            raise Host_Error with Err_Code_Image (Errcode)
              & Dedot (Socket_Error_Message (Errcode)) & ": " & Name;
         end;
      else
         raise Host_Error with Err_Code_Image (Integer (RC))
           & Dedot (CS.Value (C_GAI_Strerror (RC))) & ": " & Name;
      end if;
   end Raise_GAI_Error;

   ----------------------
   -- Get_Address_Info --
   ----------------------

   function Get_Address_Info
     (Host         : String;
      Service      : String;
      Family       : Family_Type := Family_Unspec;
      Mode         : Mode_Type   := Socket_Stream;
      Level        : Level_Type  := IP_Protocol_For_IP_Level;
      Numeric_Host : Boolean     := False;
      Passive      : Boolean     := False;
      Unknown      : access procedure
        (Family, Mode, Level, Length : Integer) := null)
      return Address_Info_Array
   is
      A : aliased Addrinfo_Access;
      N : aliased C.char_array := C.To_C (Host);
      S : aliased C.char_array := C.To_C (if Service = "" then "0"
                                          else Service);
      Hints : aliased constant Addrinfo :=
        (ai_family   => Families (Family),
         ai_socktype => Modes (Mode),
         ai_protocol => Levels (Level),
         ai_flags    => (if Numeric_Host then SOSC.AI_NUMERICHOST else 0) +
                        (if Passive then SOSC.AI_PASSIVE else 0),
         ai_addrlen  => 0,
         others      => <>);

      R     : C.int;
      Iter  : Addrinfo_Access;

      function To_Array return Address_Info_Array;
      --  Convert taken from OS addrinfo list A into Address_Info_Array

      --------------
      -- To_Array --
      --------------

      function To_Array return Address_Info_Array is
         procedure Unsupported;
         --  Calls Unknown callback if defiend

         -----------------
         -- Unsupported --
         -----------------

         procedure Unsupported is
         begin
            if Unknown /= null then
               Unknown
                 (Integer (Iter.ai_family),
                  Integer (Iter.ai_socktype),
                  Integer (Iter.ai_protocol),
                  Integer (Iter.ai_addrlen));
            end if;
         end Unsupported;

         Found  : Boolean;
         Result : Address_Info_Array (1 .. 8);

      --  Start of processing for To_Array

      begin
         for J in Result'Range loop
            Look_For_Supported : loop
               if Iter = null then
                  pragma Warnings
                    (Off, "may be referenced before it has a value");

                  return Result (1 .. J - 1);

                  pragma Warnings
                    (On, "may be referenced before it has a value");
               end if;

               Result (J).Addr :=
                 Get_Address (Iter.ai_addr.all, C.int (Iter.ai_addrlen));

               if Result (J).Addr.Family = Family_Unspec then
                  Unsupported;
               else
                  Found := False;
                  for M in Modes'Range loop
                     if Modes (M) = Iter.ai_socktype then
                        Result (J).Mode := M;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if Found then
                     for L in Levels'Range loop
                        if Levels (L) = Iter.ai_protocol then
                           Result (J).Level := L;
                           exit;
                        end if;
                     end loop;

                     exit Look_For_Supported;
                  else
                     Unsupported;
                  end if;
               end if;

               Iter := Iter.ai_next;
            end loop Look_For_Supported;

            Iter := Iter.ai_next;
         end loop;

         return Result & To_Array;
      end To_Array;

   --  Start of processing for Get_Address_Info

   begin
      R := C_Getaddrinfo
        (Node    => (if Host = "" then null else N'Unchecked_Access),
         Service => S'Unchecked_Access,
         Hints   => Hints'Unchecked_Access,
         Res     => A'Access);

      if R /= 0 then
         Raise_GAI_Error
           (R, Host & (if Service = "" then "" else ':' & Service));
      end if;

      Iter := A;

      return Result : constant Address_Info_Array := To_Array do
         C_Freeaddrinfo (A);
      end return;
   end Get_Address_Info;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Addr_Info : in out Address_Info_Array;
      Compare   : access function (Left, Right : Address_Info) return Boolean)
   is
      function Comp (Left, Right : Address_Info) return Boolean is
         (Compare (Left, Right));
      procedure Sorter is new Ada.Containers.Generic_Array_Sort
        (Positive, Address_Info, Address_Info_Array, Comp);
   begin
      Sorter (Addr_Info);
   end Sort;

   ------------------------
   -- IPv6_TCP_Preferred --
   ------------------------

   function IPv6_TCP_Preferred (Left, Right : Address_Info) return Boolean is
   begin
      pragma Assert (Family_Inet < Family_Inet6);
      --  To be sure that Family_Type enumeration has appropriate elements
      --  order

      if Left.Addr.Family /= Right.Addr.Family then
         return Left.Addr.Family > Right.Addr.Family;
      end if;

      pragma Assert (Socket_Stream < Socket_Datagram);
      --  To be sure that Mode_Type enumeration has appropriate elements order

      return Left.Mode < Right.Mode;
   end IPv6_TCP_Preferred;

   -------------------
   -- Get_Name_Info --
   -------------------

   function Get_Name_Info
     (Addr         : Sock_Addr_Type;
      Numeric_Host : Boolean := False;
      Numeric_Serv : Boolean := False) return Host_Service
   is
      SA  : aliased Sockaddr;
      H   : aliased C.char_array := [1 .. SOSC.NI_MAXHOST => C.nul];
      S   : aliased C.char_array := [1 .. SOSC.NI_MAXSERV => C.nul];
      RC  : C.int;
      Len : C.int;
   begin
      Set_Address (SA'Unchecked_Access, Addr, Len);

      RC := C_Getnameinfo
        (SA'Unchecked_Access, socklen_t (Len),
         H'Unchecked_Access, H'Length,
         S'Unchecked_Access, S'Length,
         (if Numeric_Host then SOSC.NI_NUMERICHOST else 0) +
             (if Numeric_Serv then SOSC.NI_NUMERICSERV else 0));

      if RC /= 0 then
         Raise_GAI_Error (RC, Image (Addr));
      end if;

      declare
         HR : constant String := C.To_Ada (H);
         SR : constant String := C.To_Ada (S);
      begin
         return (HR'Length, SR'Length, HR, SR);
      end;
   end Get_Name_Info;

   -------------------------
   -- Get_Host_By_Address --
   -------------------------

   function Get_Host_By_Address
     (Address : Inet_Addr_Type;
      Family  : Family_Type := Family_Inet) return Host_Entry_Type
   is
      pragma Unreferenced (Family);

      HA     : aliased In_Addr_Union (Address.Family);
      Buflen : constant C.size_t := Netdb_Buffer_Size;
      Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
      Res    : aliased Hostent;
      Err    : aliased C.int;

   begin
      case Address.Family is
         when Family_Inet =>
            HA.In4 := To_In_Addr (Address);
         when Family_Inet6 =>
            HA.In6 := To_In6_Addr (Address);
      end case;

      Netdb_Lock;

      if C_Gethostbyaddr
        (HA'Address,
         (case Address.Family is
             when Family_Inet => HA.In4'Size,
             when Family_Inet6 => HA.In6'Size) / 8,
         Families (Address.Family),
         Res'Access, Buf'Address, Buflen, Err'Access) /= 0
      then
         Netdb_Unlock;
         Raise_Host_Error (Integer (Err), Image (Address));
      end if;

      begin
         return H : constant Host_Entry_Type :=
                      To_Host_Entry (Res'Unchecked_Access)
         do
            Netdb_Unlock;
         end return;
      exception
         when others =>
            Netdb_Unlock;
            raise;
      end;
   end Get_Host_By_Address;

   ----------------------
   -- Get_Host_By_Name --
   ----------------------

   function Get_Host_By_Name (Name : String) return Host_Entry_Type is
   begin
      --  If the given name actually is the string representation of
      --  an IP address, use Get_Host_By_Address instead.

      if Is_IPv4_Address (Name) or else Is_IPv6_Address (Name) then
         return Get_Host_By_Address (Inet_Addr (Name));
      end if;

      declare
         HN     : constant C.char_array := C.To_C (Name);
         Buflen : constant C.size_t := Netdb_Buffer_Size;
         Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
         Res    : aliased Hostent;
         Err    : aliased C.int;

      begin
         Netdb_Lock;

         if C_Gethostbyname
           (HN, Res'Access, Buf'Address, Buflen, Err'Access) /= 0
         then
            Netdb_Unlock;
            Raise_Host_Error (Integer (Err), Name);
         end if;

         return H : constant Host_Entry_Type :=
                      To_Host_Entry (Res'Unchecked_Access)
         do
            Netdb_Unlock;
         end return;
      end;
   end Get_Host_By_Name;

   -------------------
   -- Get_Peer_Name --
   -------------------

   function Get_Peer_Name (Socket : Socket_Type) return Sock_Addr_Type is
      Sin : aliased Sockaddr;
      Len : aliased C.int := Sin'Size / 8;
   begin
      if C_Getpeername (C.int (Socket), Sin'Address, Len'Access) = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      return Get_Address (Sin, Len);
   end Get_Peer_Name;

   -------------------------
   -- Get_Service_By_Name --
   -------------------------

   function Get_Service_By_Name
     (Name     : String;
      Protocol : String) return Service_Entry_Type
   is
      SN     : constant C.char_array := C.To_C (Name);
      SP     : constant C.char_array := C.To_C (Protocol);
      Buflen : constant C.size_t := Netdb_Buffer_Size;
      Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
      Res    : aliased Servent;

   begin
      Netdb_Lock;

      if C_Getservbyname (SN, SP, Res'Access, Buf'Address, Buflen) /= 0 then
         Netdb_Unlock;
         raise Service_Error with "Service not found";
      end if;

      --  Translate from the C format to the API format

      return S : constant Service_Entry_Type :=
                   To_Service_Entry (Res'Unchecked_Access)
      do
         Netdb_Unlock;
      end return;
   end Get_Service_By_Name;

   -------------------------
   -- Get_Service_By_Port --
   -------------------------

   function Get_Service_By_Port
     (Port     : Port_Type;
      Protocol : String) return Service_Entry_Type
   is
      SP     : constant C.char_array := C.To_C (Protocol);
      Buflen : constant C.size_t := Netdb_Buffer_Size;
      Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
      Res    : aliased Servent;

   begin
      Netdb_Lock;

      if C_Getservbyport
        (C.int (Short_To_Network (C.unsigned_short (Port))), SP,
         Res'Access, Buf'Address, Buflen) /= 0
      then
         Netdb_Unlock;
         raise Service_Error with "Service not found";
      end if;

      --  Translate from the C format to the API format

      return S : constant Service_Entry_Type :=
                   To_Service_Entry (Res'Unchecked_Access)
      do
         Netdb_Unlock;
      end return;
   end Get_Service_By_Port;

   ---------------------
   -- Get_Socket_Name --
   ---------------------

   function Get_Socket_Name
     (Socket : Socket_Type) return Sock_Addr_Type
   is
      Sin : aliased Sockaddr;
      Len : aliased C.int := Sin'Size / 8;
      Res : C.int;
   begin
      Res := C_Getsockname (C.int (Socket), Sin'Address, Len'Access);

      if Res = Failure then
         return No_Sock_Addr;
      end if;

      return Get_Address (Sin, Len);
   end Get_Socket_Name;

   -----------------------
   -- Get_Socket_Option --
   -----------------------

   function Get_Socket_Option
     (Socket  : Socket_Type;
      Level   : Level_Type;
      Name    : Option_Name;
      Optname : Interfaces.C.int := -1) return Option_Type
   is
      use type C.unsigned;
      use type C.unsigned_char;

      --  SOSC.IF_NAMESIZE may be not defined, ensure that we have at least
      --  a valid range for VS declared below.
      NS  : constant Interfaces.C.size_t :=
              (if SOSC.IF_NAMESIZE = -1 then 256 else SOSC.IF_NAMESIZE);
      V8  : aliased Two_Ints;
      V4  : aliased C.int;
      U4  : aliased C.unsigned;
      V1  : aliased C.unsigned_char;
      VS  : aliased C.char_array (1 .. NS); -- for devices name
      VT  : aliased Timeval;
      Len : aliased C.int;
      Add : System.Address;
      Res : C.int;
      Opt : Option_Type (Name);
      Onm : Interfaces.C.int;
   begin
      if Name in Specific_Option_Name then
         Onm := Options (Name);

      elsif Optname = -1 then
         raise Socket_Error with "optname must be specified";

      else
         Onm := Optname;
      end if;

      case Name is
         when Multicast_TTL
            | Receive_Packet_Info
         =>
            Len := V1'Size / 8;
            Add := V1'Address;

         when Broadcast
            | Busy_Polling
            | Error
            | Generic_Option
            | Keep_Alive
            | Keep_Alive_Count
            | Keep_Alive_Idle
            | Keep_Alive_Interval
            | Multicast_If_V4
            | Multicast_If_V6
            | Multicast_Loop_V4
            | Multicast_Loop_V6
            | Multicast_Hops
            | No_Delay
            | Receive_Buffer
            | Reuse_Address
            | Send_Buffer
            | IPv6_Only
         =>
            Len := V4'Size / 8;
            Add := V4'Address;

         when Receive_Timeout
            | Send_Timeout
         =>
            --  The standard argument for SO_RCVTIMEO and SO_SNDTIMEO is a
            --  struct timeval, but on Windows it is a milliseconds count in
            --  a DWORD.

            if Is_Windows then
               Len := U4'Size / 8;
               Add := U4'Address;
            else
               Len := VT'Size / 8;
               Add := VT'Address;
            end if;

         when Add_Membership_V4
            | Add_Membership_V6
            | Drop_Membership_V4
            | Drop_Membership_V6
         =>
            raise Socket_Error with
              "Add/Drop membership valid only for Set_Socket_Option";

         when Linger
         =>
            Len := V8'Size / 8;
            Add := V8'Address;

         when Bind_To_Device
         =>
            Len := VS'Length;
            Add := VS'Address;
      end case;

      Res :=
        C_Getsockopt
          (C.int (Socket),
           Levels (Level),
           Onm,
           Add, Len'Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      case Name is
         when Generic_Option =>
            Opt.Optname := Onm;
            Opt.Optval  := V4;

         when Broadcast
            | Keep_Alive
            | No_Delay
            | Reuse_Address
            | Multicast_Loop_V4
            | Multicast_Loop_V6
            | IPv6_Only
         =>
            Opt.Enabled := (V4 /= 0);

         when Keep_Alive_Count =>
            Opt.Count := Natural (V4);

         when Keep_Alive_Idle =>
            Opt.Idle_Seconds := Natural (V4);

         when Keep_Alive_Interval =>
            Opt.Interval_Seconds := Natural (V4);

         when Busy_Polling =>
            Opt.Microseconds := Natural (V4);

         when Linger =>
            Opt.Enabled := (V8 (V8'First) /= 0);
            Opt.Seconds := Natural (V8 (V8'Last));

         when Receive_Buffer
            | Send_Buffer
         =>
            Opt.Size := Natural (V4);

         when Error =>
            Opt.Error := Resolve_Error (Integer (V4));

         when Add_Membership_V4
            | Add_Membership_V6
            | Drop_Membership_V4
            | Drop_Membership_V6
         =>
            --  No way to be here. Exception raised in the first case Name
            --  expression.
            null;

         when Multicast_If_V4 =>
            To_Inet_Addr (To_In_Addr (V4), Opt.Outgoing_If);

         when Multicast_If_V6 =>
            Opt.Outgoing_If_Index := Natural (V4);

         when Multicast_TTL =>
            Opt.Time_To_Live := Integer (V1);

         when Multicast_Hops =>
            Opt.Hop_Limit := Integer (V4);

         when Receive_Packet_Info
         =>
            Opt.Enabled := (V1 /= 0);

         when Receive_Timeout
            | Send_Timeout
         =>
            if Is_Windows then
               if U4 = 0 then
                  Opt.Timeout := 0.0;

               else
                  if Minus_500ms_Windows_Timeout then
                     --  Timeout is in milliseconds, actual value is 500 ms +
                     --  returned value (unless it is 0).

                     U4 := U4 + 500;
                  end if;

                  Opt.Timeout := Duration (U4) / 1000;
               end if;

            else
               Opt.Timeout := To_Duration (VT);
            end if;

         when Bind_To_Device =>
            Opt.Device := ASU.To_Unbounded_String (C.To_Ada (VS));
      end case;

      return Opt;
   end Get_Socket_Option;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
      Name : aliased C.char_array (1 .. 64);
      Res  : C.int;

   begin
      Res := C_Gethostname (Name'Address, Name'Length);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      return C.To_Ada (Name);
   end Host_Name;

   -----------
   -- Image --
   -----------

   function Image (Value : Inet_Addr_Type) return String is
      use type CS.char_array_access;
      Size : constant socklen_t :=
        (case Value.Family is
            when Family_Inet   => 4 * Value.Sin_V4'Length,
            when Family_Inet6  => 6 * 5 + 4 * 4);
            --  1234:1234:1234:1234:1234:1234:123.123.123.123
      Dst : aliased C.char_array := [1 .. C.size_t (Size) => C.nul];
      Ia  : aliased In_Addr_Union (Value.Family);
   begin
      case Value.Family is
         when Family_Inet6 =>
            Ia.In6 := To_In6_Addr (Value);
         when Family_Inet =>
            Ia.In4 := To_In_Addr (Value);
      end case;

      if Inet_Ntop
        (Families (Value.Family), Ia'Address,
         Dst'Unchecked_Access, Size) = null
      then
         Raise_Socket_Error (Socket_Errno);
      end if;

      return C.To_Ada (Dst);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Sock_Addr_Type) return String is
      function Ipv6_Brackets (S : String) return String is
        (if Value.Family = Family_Inet6 then "[" & S & "]" else S);
   begin
      case Value.Family is
         when Family_Unix =>
            if ASU.Length (Value.Name) > 0
              and then ASU.Element (Value.Name, 1) = ASCII.NUL
            then
               return '@' & ASU.Slice (Value.Name, 2, ASU.Length (Value.Name));
            else
               return ASU.To_String (Value.Name);
            end if;

         when Family_Inet_4_6 =>
            declare
               Port : constant String := Value.Port'Img;
            begin
               return Ipv6_Brackets (Image (Value.Addr)) & ':'
                 & Port (2 .. Port'Last);
            end;

         when Family_Unspec =>
            return "";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Socket : Socket_Type) return String is
   begin
      return Socket'Img;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Socket_Set_Type) return String is
      Socket_Set : Socket_Set_Type := Item;

   begin
      declare
         Last_Img : constant String := Socket_Set.Last'Img;
         Buffer   : String
                      (1 .. (Integer (Socket_Set.Last) + 1) * Last_Img'Length);
         Index    : Positive := 1;
         Socket   : Socket_Type;

      begin
         while not Is_Empty (Socket_Set) loop
            Get (Socket_Set, Socket);

            declare
               Socket_Img : constant String := Socket'Img;
            begin
               Buffer (Index .. Index + Socket_Img'Length - 1) := Socket_Img;
               Index := Index + Socket_Img'Length;
            end;
         end loop;

         return "[" & Last_Img & "]" & Buffer (1 .. Index - 1);
      end;
   end Image;

   ---------------
   -- Inet_Addr --
   ---------------

   function Inet_Addr (Image : String) return Inet_Addr_Type is
      use Interfaces.C;

      Img    : aliased char_array := To_C (Image);
      Res    : C.int;
      Result : Inet_Addr_Type;
      IPv6   : constant Boolean := Is_IPv6_Address (Image);
      Ia     : aliased In_Addr_Union
                 (if IPv6 then Family_Inet6 else Family_Inet);
   begin
      --  Special case for an empty Image as on some platforms (e.g. Windows)
      --  calling Inet_Addr("") will not return an error.

      if Image = "" then
         Raise_Socket_Error (SOSC.EINVAL);
      end if;

      Res := Inet_Pton
        ((if IPv6 then SOSC.AF_INET6 else SOSC.AF_INET), Img'Address,
         Ia'Address);

      if Res < 0 then
         Raise_Socket_Error (Socket_Errno);

      elsif Res = 0 then
         Raise_Socket_Error (SOSC.EINVAL);
      end if;

      if IPv6 then
         To_Inet_Addr (Ia.In6, Result);
      else
         To_Inet_Addr (Ia.In4, Result);
      end if;

      return Result;
   end Inet_Addr;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (X : in out Sockets_Library_Controller) is
      pragma Unreferenced (X);

   begin
      Thin.Initialize;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Process_Blocking_IO : Boolean) is
      Expected : constant Boolean := not SOSC.Thread_Blocking_IO;

   begin
      if Process_Blocking_IO /= Expected then
         raise Socket_Error with
           "incorrect Process_Blocking_IO setting, expected " & Expected'Img;
      end if;

      --  This is a dummy placeholder for an obsolete API

      --  Real initialization actions are in Initialize primitive operation
      --  of Sockets_Library_Controller.

      null;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  This is a dummy placeholder for an obsolete API

      --  Real initialization actions are in Initialize primitive operation
      --  of Sockets_Library_Controller.

      null;
   end Initialize;

   ----------------
   -- Is_Windows --
   ----------------

   function Is_Windows return Boolean is
      use SOSC;
   begin
      return Target_OS = Windows;
   end Is_Windows;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Socket_Set_Type) return Boolean is
   begin
      return Item.Last = No_Socket;
   end Is_Empty;

   ---------------------
   -- Is_IPv6_Address --
   ---------------------

   function Is_IPv6_Address (Name : String) return Boolean is
      Prev_Colon   : Natural := 0;
      Double_Colon : Boolean := False;
      Colons       : Natural := 0;
   begin
      for J in Name'Range loop
         if Name (J) = ':' then
            Colons := Colons + 1;

            if Prev_Colon > 0 and then J = Prev_Colon + 1 then
               if Double_Colon then
                  --  Only one double colon allowed
                  return False;
               end if;

               Double_Colon := True;

            elsif J = Name'Last then
               --  Single colon at the end is not allowed
               return False;
            end if;

            Prev_Colon := J;

         elsif Prev_Colon = Name'First then
            --  Single colon at start is not allowed
            return False;

         elsif Name (J) = '.' then
            return Prev_Colon > 0
              and then Is_IPv4_Address (Name (Prev_Colon + 1 .. Name'Last));

         elsif Name (J) not in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' then
            return False;

         end if;
      end loop;

      return Colons in 2 .. 8;
   end Is_IPv6_Address;

   ---------------------
   -- Is_IPv4_Address --
   ---------------------

   function Is_IPv4_Address (Name : String) return Boolean is
      Dots : Natural := 0;

   begin
      --  Perform a cursory check for a dotted quad: we must have 1 to 3 dots,
      --  and there must be at least one digit around each.

      for J in Name'Range loop
         if Name (J) = '.' then

            --  Check that the dot is not in first or last position, and that
            --  it is followed by a digit. Note that we already know that it is
            --  preceded by a digit, or we would have returned earlier on.

            if J in Name'First + 1 .. Name'Last - 1
              and then Name (J + 1) in '0' .. '9'
            then
               Dots := Dots + 1;

            --  Definitely not a proper dotted quad

            else
               return False;
            end if;

         elsif Name (J) not in '0' .. '9' then
            return False;
         end if;
      end loop;

      return Dots in 1 .. 3;
   end Is_IPv4_Address;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (S : Selector_Type) return Boolean is
   begin
      if S.Is_Null then
         return True;

      else
         --  Either both controlling socket descriptors are valid (case of an
         --  open selector) or neither (case of a closed selector).

         pragma Assert ((S.R_Sig_Socket /= No_Socket)
                          =
                        (S.W_Sig_Socket /= No_Socket));

         return S.R_Sig_Socket /= No_Socket;
      end if;
   end Is_Open;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Item   : Socket_Set_Type;
      Socket : Socket_Type) return Boolean
   is
   begin
      Check_For_Fd_Set (Socket);

      return Item.Last /= No_Socket
        and then Socket <= Item.Last
        and then Is_Socket_In_Set (Item.Set'Access, C.int (Socket)) /= 0;
   end Is_Set;

   -------------------
   -- Listen_Socket --
   -------------------

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Natural := 15)
   is
      Res : constant C.int := C_Listen (C.int (Socket), C.int (Length));
   begin
      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Listen_Socket;

   ------------
   -- Narrow --
   ------------

   procedure Narrow (Item : in out Socket_Set_Type) is
      Last : aliased C.int := C.int (Item.Last);
   begin
      if Item.Last /= No_Socket then
         Last_Socket_In_Set (Item.Set'Access, Last'Unchecked_Access);
         Item.Last := Socket_Type (Last);
      end if;
   end Narrow;

   ----------------
   -- Netdb_Lock --
   ----------------

   procedure Netdb_Lock is
   begin
      if Need_Netdb_Lock then
         System.Task_Lock.Lock;
      end if;
   end Netdb_Lock;

   ------------------
   -- Netdb_Unlock --
   ------------------

   procedure Netdb_Unlock is
   begin
      if Need_Netdb_Lock then
         System.Task_Lock.Unlock;
      end if;
   end Netdb_Unlock;

   ----------------------------
   -- Network_Socket_Address --
   ----------------------------

   function Network_Socket_Address
     (Addr : Inet_Addr_Type; Port : Port_Type) return Sock_Addr_Type is
   begin
      return Result : Sock_Addr_Type (Addr.Family) do
         Result.Addr := Addr;
         Result.Port := Port;
      end return;
   end Network_Socket_Address;

   --------------------------------
   -- Normalize_Empty_Socket_Set --
   --------------------------------

   procedure Normalize_Empty_Socket_Set (S : in out Socket_Set_Type) is
   begin
      if S.Last = No_Socket then
         Reset_Socket_Set (S.Set'Access);
      end if;
   end Normalize_Empty_Socket_Set;

   -------------------
   -- Official_Name --
   -------------------

   function Official_Name (E : Host_Entry_Type) return String is
   begin
      return To_String (E.Official);
   end Official_Name;

   -------------------
   -- Official_Name --
   -------------------

   function Official_Name (S : Service_Entry_Type) return String is
   begin
      return To_String (S.Official);
   end Official_Name;

   --------------------
   -- Wait_On_Socket --
   --------------------

   procedure Wait_On_Socket
     (Socket   : Socket_Type;
      Event    : Poll.Wait_Event_Set;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status)
   is
      Fd_Set : Poll.Set := Poll.To_Set (Socket, Event, 2);
      --  Socket itself and second place for signaling socket if necessary

      Count : Natural;
      Index : Natural := 0;

   begin
      --  Add signaling socket if selector defined

      if Selector /= null then
         Poll.Append (Fd_Set, Selector.R_Sig_Socket, Poll.Input_Event);
      end if;

      Poll.Wait (Fd_Set, Timeout, Count);

      if Count = 0 then
         Status := Expired;
      else
         Poll.Next (Fd_Set, Index);
         Status := (if Index = 1 then Completed else Aborted);
      end if;
   end Wait_On_Socket;

   -----------------
   -- Port_Number --
   -----------------

   function Port_Number (S : Service_Entry_Type) return Port_Type is
   begin
      return S.Port;
   end Port_Number;

   -------------------
   -- Protocol_Name --
   -------------------

   function Protocol_Name (S : Service_Entry_Type) return String is
   begin
      return To_String (S.Protocol);
   end Protocol_Name;

   ----------------------
   -- Raise_Host_Error --
   ----------------------

   procedure Raise_Host_Error (H_Error : Integer; Name : String) is
   begin
      raise Host_Error with
        Err_Code_Image (H_Error)
          & Dedot (Host_Error_Messages.Host_Error_Message (H_Error))
          & ": " & Name;
   end Raise_Host_Error;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error (Error : Integer) is
   begin
      raise Socket_Error with
        Err_Code_Image (Error) & Socket_Error_Message (Error);
   end Raise_Socket_Error;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Datagram_Socket_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Receive_Socket
        (Stream.Socket,
         Item,
         Last,
         Stream.From);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      First : Ada.Streams.Stream_Element_Offset          := Item'First;
      Index : Ada.Streams.Stream_Element_Offset          := First - 1;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      loop
         Receive_Socket (Stream.Socket, Item (First .. Max), Index);
         Last  := Index;

         --  Exit when all or zero data received. Zero means that the socket
         --  peer is closed.

         exit when Index < First or else Index = Max;

         First := Index + 1;
      end loop;
   end Read;

   --------------------
   -- Receive_Socket --
   --------------------

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      Res : C.int;

   begin
      Res :=
        C_Recv (C.int (Socket), Item'Address, Item'Length, To_Int (Flags));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Last_Index (First => Item'First, Count => size_t (Res));
   end Receive_Socket;

   --------------------
   -- Receive_Socket --
   --------------------

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      From   : out Sock_Addr_Type;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      Res : C.int;
      Sin : aliased Sockaddr;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Res :=
        C_Recvfrom
          (C.int (Socket),
           Item'Address,
           Item'Length,
           To_Int (Flags),
           Sin'Address,
           Len'Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Last_Index (First => Item'First, Count => size_t (Res));

      From := Get_Address (Sin, Len);
   end Receive_Socket;

   --------------------
   -- Receive_Vector --
   --------------------

   procedure Receive_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      Res : ssize_t;

      Msg : Msghdr :=
              (Msg_Name       => System.Null_Address,
               Msg_Namelen    => 0,
               Msg_Iov        => Vector'Address,

               --  recvmsg(2) returns EMSGSIZE on Linux (and probably on other
               --  platforms) when the supplied vector is longer than IOV_MAX,
               --  so use minimum of the two lengths.

               Msg_Iovlen     => SOSC.Msg_Iovlen_T'Min
                                   (Vector'Length, SOSC.IOV_MAX),

               Msg_Control    => System.Null_Address,
               Msg_Controllen => 0,
               Msg_Flags      => 0);

   begin
      Res :=
        C_Recvmsg
          (C.int (Socket),
           Msg'Address,
           To_Int (Flags));

      if Res = ssize_t (Failure) then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Count := Ada.Streams.Stream_Element_Count (Res);
   end Receive_Vector;

   -------------------
   -- Resolve_Error --
   -------------------

   function Resolve_Error
     (Error_Value : Integer;
      From_Errno  : Boolean := True) return Error_Type
   is
      use GNAT.Sockets.SOSC;

   begin
      if not From_Errno then
         case Error_Value is
            when SOSC.HOST_NOT_FOUND => return Unknown_Host;
            when SOSC.TRY_AGAIN      => return Host_Name_Lookup_Failure;
            when SOSC.NO_RECOVERY    => return Non_Recoverable_Error;
            when SOSC.NO_DATA        => return Unknown_Server_Error;
            when others              => return Cannot_Resolve_Error;
         end case;
      end if;

      --  Special case: EAGAIN may be the same value as EWOULDBLOCK, so we
      --  can't include it in the case statement below.

      pragma Warnings (Off);
      --  Condition "EAGAIN /= EWOULDBLOCK" is known at compile time

      if EAGAIN /= EWOULDBLOCK and then Error_Value = EAGAIN then
         return Resource_Temporarily_Unavailable;
      end if;

      --  This is not a case statement because if a particular error
      --  number constant is not defined, s-oscons-tmplt.c defines
      --  it to -1.  If multiple constants are not defined, they
      --  would each be -1 and result in a "duplicate value in case" error.
      --
      --  But we have to leave warnings off because the compiler is also
      --  smart enough to note that when two errnos have the same value,
      --  the second if condition is useless.
      if Error_Value = ENOERROR then
         return Success;
      elsif Error_Value = EACCES then
         return Permission_Denied;
      elsif Error_Value = EADDRINUSE then
         return Address_Already_In_Use;
      elsif Error_Value = EADDRNOTAVAIL then
         return Cannot_Assign_Requested_Address;
      elsif Error_Value = EAFNOSUPPORT then
         return Address_Family_Not_Supported_By_Protocol;
      elsif Error_Value = EALREADY then
         return Operation_Already_In_Progress;
      elsif Error_Value = EBADF then
         return Bad_File_Descriptor;
      elsif Error_Value = ECONNABORTED then
         return Software_Caused_Connection_Abort;
      elsif Error_Value = ECONNREFUSED then
         return Connection_Refused;
      elsif Error_Value = ECONNRESET then
         return Connection_Reset_By_Peer;
      elsif Error_Value = EDESTADDRREQ then
         return Destination_Address_Required;
      elsif Error_Value = EFAULT then
         return Bad_Address;
      elsif Error_Value = EHOSTDOWN then
         return Host_Is_Down;
      elsif Error_Value = EHOSTUNREACH then
         return No_Route_To_Host;
      elsif Error_Value = EINPROGRESS then
         return Operation_Now_In_Progress;
      elsif Error_Value = EINTR then
         return Interrupted_System_Call;
      elsif Error_Value = EINVAL then
         return Invalid_Argument;
      elsif Error_Value = EIO then
         return Input_Output_Error;
      elsif Error_Value = EISCONN then
         return Transport_Endpoint_Already_Connected;
      elsif Error_Value = ELOOP then
         return Too_Many_Symbolic_Links;
      elsif Error_Value = EMFILE then
         return Too_Many_Open_Files;
      elsif Error_Value = EMSGSIZE then
         return Message_Too_Long;
      elsif Error_Value = ENAMETOOLONG then
         return File_Name_Too_Long;
      elsif Error_Value = ENETDOWN then
         return Network_Is_Down;
      elsif Error_Value = ENETRESET then
         return Network_Dropped_Connection_Because_Of_Reset;
      elsif Error_Value = ENETUNREACH then
         return Network_Is_Unreachable;
      elsif Error_Value = ENOBUFS then
         return No_Buffer_Space_Available;
      elsif Error_Value = ENOPROTOOPT then
         return Protocol_Not_Available;
      elsif Error_Value = ENOTCONN then
         return Transport_Endpoint_Not_Connected;
      elsif Error_Value = ENOTSOCK then
         return Socket_Operation_On_Non_Socket;
      elsif Error_Value = EOPNOTSUPP then
         return Operation_Not_Supported;
      elsif Error_Value = EPFNOSUPPORT then
         return Protocol_Family_Not_Supported;
      elsif Error_Value = EPIPE then
         return Broken_Pipe;
      elsif Error_Value = EPROTONOSUPPORT then
         return Protocol_Not_Supported;
      elsif Error_Value = EPROTOTYPE then
         return Protocol_Wrong_Type_For_Socket;
      elsif Error_Value = ESHUTDOWN then
         return Cannot_Send_After_Transport_Endpoint_Shutdown;
      elsif Error_Value = ESOCKTNOSUPPORT then
         return Socket_Type_Not_Supported;
      elsif Error_Value = ETIMEDOUT then
         return Connection_Timed_Out;
      elsif Error_Value = ETOOMANYREFS then
         return Too_Many_References;
      elsif Error_Value = EWOULDBLOCK then
         return Resource_Temporarily_Unavailable;
      else
         return Cannot_Resolve_Error;
      end if;
      pragma Warnings (On);

   end Resolve_Error;

   -----------------------
   -- Resolve_Exception --
   -----------------------

   function Resolve_Exception
     (Occurrence : Exception_Occurrence) return Error_Type
   is
      Id    : constant Exception_Id := Exception_Identity (Occurrence);
      Msg   : constant String       := Exception_Message (Occurrence);
      First : Natural;
      Last  : Natural;
      Val   : Integer;

   begin
      First := Msg'First;
      while First <= Msg'Last
        and then Msg (First) not in '0' .. '9'
      loop
         First := First + 1;
      end loop;

      if First > Msg'Last then
         return Cannot_Resolve_Error;
      end if;

      Last := First;
      while Last < Msg'Last
        and then Msg (Last + 1) in '0' .. '9'
      loop
         Last := Last + 1;
      end loop;

      Val := Integer'Value (Msg (First .. Last));

      if Id = Socket_Error_Id then
         return Resolve_Error (Val);

      elsif Id = Host_Error_Id then
         return Resolve_Error (Val, False);

      else
         return Cannot_Resolve_Error;
      end if;
   end Resolve_Exception;

   -----------------
   -- Send_Socket --
   -----------------

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
   begin
      Send_Socket (Socket, Item, Last, To => null, Flags => Flags);
   end Send_Socket;

   -----------------
   -- Send_Socket --
   -----------------

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : Sock_Addr_Type;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
   begin
      Send_Socket
        (Socket, Item, Last, To => To'Unrestricted_Access, Flags => Flags);
   end Send_Socket;

   -----------------
   -- Send_Socket --
   -----------------

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : access Sock_Addr_Type;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      Res  : C.int;

      Sin  : aliased Sockaddr;
      C_To : System.Address;
      Len  : C.int;

   begin
      if To /= null then
         Set_Address (Sin'Unchecked_Access, To.all, Len);
         C_To := Sin'Address;

      else
         C_To := System.Null_Address;
         Len := 0;
      end if;

      Res := C_Sendto
        (C.int (Socket),
         Item'Address,
         Item'Length,
         Set_Forced_Flags (To_Int (Flags)),
         C_To,
         Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Last_Index (First => Item'First, Count => size_t (Res));
   end Send_Socket;

   -----------------
   -- Send_Vector --
   -----------------

   procedure Send_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      use Interfaces.C;

      Res            : ssize_t;
      Iov_Count      : SOSC.Msg_Iovlen_T;
      This_Iov_Count : SOSC.Msg_Iovlen_T;
      Msg            : Msghdr;

   begin
      Count := 0;
      Iov_Count := 0;
      while Iov_Count < Vector'Length loop

         pragma Warnings (Off);
         --  Following test may be compile time known on some targets

         This_Iov_Count :=
           (if Vector'Length - Iov_Count > SOSC.IOV_MAX
            then SOSC.IOV_MAX
            else Vector'Length - Iov_Count);

         pragma Warnings (On);

         Msg :=
           (Msg_Name       => System.Null_Address,
            Msg_Namelen    => 0,
            Msg_Iov        => Vector
                                (Vector'First + Integer (Iov_Count))'Address,
            Msg_Iovlen     => This_Iov_Count,
            Msg_Control    => System.Null_Address,
            Msg_Controllen => 0,
            Msg_Flags      => 0);

         Res :=
           C_Sendmsg
             (C.int (Socket),
              Msg'Address,
              Set_Forced_Flags (To_Int (Flags)));

         if Res = ssize_t (Failure) then
            Raise_Socket_Error (Socket_Errno);
         end if;

         Count := Count + Ada.Streams.Stream_Element_Count (Res);
         Iov_Count := Iov_Count + This_Iov_Count;
      end loop;
   end Send_Vector;

   ---------
   -- Set --
   ---------

   procedure Set (Item : in out Socket_Set_Type; Socket : Socket_Type) is
   begin
      Check_For_Fd_Set (Socket);

      if Item.Last = No_Socket then

         --  Uninitialized socket set, make sure it is properly zeroed out

         Reset_Socket_Set (Item.Set'Access);
         Item.Last := Socket;

      elsif Item.Last < Socket then
         Item.Last := Socket;
      end if;

      Insert_Socket_In_Set (Item.Set'Access, C.int (Socket));
   end Set;

   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (Socket        : Socket_Type;
      Close_On_Exec : Boolean;
      Status        : out Boolean)
   is
      function C_Set_Close_On_Exec
        (Socket : Socket_Type; Close_On_Exec : C.int) return C.int;
      pragma Import (C, C_Set_Close_On_Exec, "__gnat_set_close_on_exec");
   begin
      Status := C_Set_Close_On_Exec (Socket, Boolean'Pos (Close_On_Exec)) = 0;
   end Set_Close_On_Exec;

   ----------------------
   -- Set_Forced_Flags --
   ----------------------

   function Set_Forced_Flags (F : C.int) return C.int is
      use type C.unsigned;
      function To_unsigned is
        new Ada.Unchecked_Conversion (C.int, C.unsigned);
      function To_int is
        new Ada.Unchecked_Conversion (C.unsigned, C.int);
   begin
      return To_int (To_unsigned (F) or SOSC.MSG_Forced_Flags);
   end Set_Forced_Flags;

   -----------------------
   -- Set_Socket_Option --
   -----------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type;
      Option : Option_Type)
   is
      use type C.unsigned;

      MR  : aliased IPV6_Mreq;
      V8  : aliased Two_Ints;
      V4  : aliased C.int;
      U4  : aliased C.unsigned;
      V1  : aliased C.unsigned_char;
      VS  : aliased C.char_array
              (1 .. (if Option.Name = Bind_To_Device
                     then C.size_t (ASU.Length (Option.Device) + 1)
                     else 0));
      VT  : aliased Timeval;
      Len : C.int;
      Add : System.Address := Null_Address;
      Res : C.int;
      Onm : C.int;

   begin
      case Option.Name is
         when Generic_Option =>
            V4  := Option.Optval;
            Len := V4'Size / 8;
            Add := V4'Address;

         when Broadcast
            | Keep_Alive
            | No_Delay
            | Reuse_Address
            | Multicast_Loop_V4
            | Multicast_Loop_V6
            | IPv6_Only
         =>
            V4  := C.int (Boolean'Pos (Option.Enabled));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Keep_Alive_Count =>
            V4  := C.int (Option.Count);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Keep_Alive_Idle =>
            V4  := C.int (Option.Idle_Seconds);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Keep_Alive_Interval =>
            V4  := C.int (Option.Interval_Seconds);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Busy_Polling =>
            V4  := C.int (Option.Microseconds);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Linger =>
            V8 (V8'First) := C.int (Boolean'Pos (Option.Enabled));
            V8 (V8'Last)  := C.int (Option.Seconds);
            Len := V8'Size / 8;
            Add := V8'Address;

         when Receive_Buffer
            | Send_Buffer
         =>
            V4  := C.int (Option.Size);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Error =>
            V4  := C.int (Boolean'Pos (True));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Add_Membership_V4
            | Drop_Membership_V4
         =>
            V8 (V8'First) := To_Int (To_In_Addr (Option.Multicast_Address));
            V8 (V8'Last)  := To_Int (To_In_Addr (Option.Local_Interface));
            Len := V8'Size / 8;
            Add := V8'Address;

         when Add_Membership_V6
            | Drop_Membership_V6 =>
            MR.ipv6mr_multiaddr := To_In6_Addr (Option.Multicast_Address);
            MR.ipv6mr_interface := C.unsigned (Option.Interface_Index);
            Len := MR'Size / 8;
            Add := MR'Address;

         when Multicast_If_V4 =>
            V4  := To_Int (To_In_Addr (Option.Outgoing_If));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Multicast_If_V6 =>
            V4  := C.int (Option.Outgoing_If_Index);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Multicast_TTL =>
            V1  := C.unsigned_char (Option.Time_To_Live);
            Len := V1'Size / 8;
            Add := V1'Address;

         when Multicast_Hops =>
            V4  := C.int (Option.Hop_Limit);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Receive_Packet_Info
         =>
            V1  := C.unsigned_char (Boolean'Pos (Option.Enabled));
            Len := V1'Size / 8;
            Add := V1'Address;

         when Receive_Timeout
            | Send_Timeout
         =>
            if Is_Windows then

               --  On Windows, the timeout is a DWORD in milliseconds

               Len := U4'Size / 8;
               Add := U4'Address;

               U4 := C.unsigned (Option.Timeout * 1000);

               if Option.Timeout > 0.0 and then U4 = 0 then
                  --  Avoid round to zero. Zero timeout mean unlimited
                  U4 := 1;
               end if;

               --  Old windows versions actual timeout is 500 ms + the given
               --  value (unless it is 0).

               if Minus_500ms_Windows_Timeout then
                  if U4 > 500 then
                     U4 := U4 - 500;

                  elsif U4 > 0 then
                     U4 := 1;
                  end if;
               end if;

            else
               VT  := To_Timeval (Option.Timeout);
               Len := VT'Size / 8;
               Add := VT'Address;
            end if;

         when Bind_To_Device =>
            VS := C.To_C (ASU.To_String (Option.Device));
            Len := C.int (VS'Length);
            Add := VS'Address;
      end case;

      if Option.Name in Specific_Option_Name then
         Onm := Options (Option.Name);

      elsif Option.Optname = -1 then
         raise Socket_Error with "optname must be specified";

      else
         Onm := Option.Optname;
      end if;

      Res := C_Setsockopt
        (C.int (Socket),
         Levels (Level),
         Onm,
         Add, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Set_Socket_Option;

   ---------------------
   -- Shutdown_Socket --
   ---------------------

   procedure Shutdown_Socket
     (Socket : Socket_Type;
      How    : Shutmode_Type := Shut_Read_Write)
   is
      Res : C.int;

   begin
      Res := C_Shutdown (C.int (Socket), Shutmodes (How));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Shutdown_Socket;

   ------------
   -- Stream --
   ------------

   function Stream
     (Socket  : Socket_Type;
      Send_To : Sock_Addr_Type) return Stream_Access
   is
      S : Datagram_Socket_Stream_Access;

   begin
      S        := new Datagram_Socket_Stream_Type;
      S.Socket := Socket;
      S.To     := Send_To;
      S.From   := Get_Socket_Name (Socket);
      return Stream_Access (S);
   end Stream;

   ------------
   -- Stream --
   ------------

   function Stream (Socket : Socket_Type) return Stream_Access is
      S : Stream_Socket_Stream_Access;
   begin
      S := new Stream_Socket_Stream_Type;
      S.Socket := Socket;
      return Stream_Access (S);
   end Stream;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Fd : Integer) return Socket_Type is
   begin
      return Socket_Type (Fd);
   end To_Ada;

   ----------
   -- To_C --
   ----------

   function To_C (Socket : Socket_Type) return Integer is
   begin
      return Integer (Socket);
   end To_C;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (Val : Timeval) return Timeval_Duration is
      Max_D : constant Long_Long_Integer := Long_Long_Integer (Forever - 0.5);
      Tv_sec_64 : constant Boolean := SOSC.SIZEOF_tv_sec = 8;
      --  Need to separate this condition into the constant declaration to
      --  avoid GNAT warning about "always true" or "always false".
   begin
      if Tv_sec_64 then
         --  Check for possible Duration overflow when Tv_Sec field is 64 bit
         --  integer.

         if Val.Tv_Sec > time_t (Max_D)
             or else
           (Val.Tv_Sec = time_t (Max_D)
              and then
            Val.Tv_Usec > suseconds_t ((Forever - Duration (Max_D)) * 1E6))
         then
            return Forever;
         end if;
      end if;

      return Duration (Val.Tv_Sec) + Duration (Val.Tv_Usec) * 1.0E-6;
   end To_Duration;

   -------------------
   -- To_Host_Entry --
   -------------------

   function To_Host_Entry (E : Hostent_Access) return Host_Entry_Type is
      Aliases_Count, Addresses_Count : Natural;

      Family : constant Family_Type :=
                 (case Hostent_H_Addrtype (E) is
                     when SOSC.AF_INET  => Family_Inet,
                     when SOSC.AF_INET6 => Family_Inet6,
                     when others        => Family_Unspec);

      Addr_Len : constant C.size_t := C.size_t (Hostent_H_Length (E));

   begin
      if Family = Family_Unspec then
         Raise_Socket_Error (SOSC.EPFNOSUPPORT);
      end if;

      Aliases_Count := 0;
      while Hostent_H_Alias (E, C.int (Aliases_Count)) /= Null_Address loop
         Aliases_Count := Aliases_Count + 1;
      end loop;

      Addresses_Count := 0;
      while Hostent_H_Addr (E, C.int (Addresses_Count)) /= Null_Address loop
         Addresses_Count := Addresses_Count + 1;
      end loop;

      return Result : Host_Entry_Type
                        (Aliases_Length   => Aliases_Count,
                         Addresses_Length => Addresses_Count)
      do
         Result.Official := To_Name (Value (Hostent_H_Name (E)));

         for J in Result.Aliases'Range loop
            Result.Aliases (J) :=
              To_Name (Value (Hostent_H_Alias
                                (E, C.int (J - Result.Aliases'First))));
         end loop;

         for J in Result.Addresses'Range loop
            declare
               Ia : In_Addr_Union (Family);

               --  Hostent_H_Addr (E, <index>) may return an address that is
               --  not correctly aligned for In_Addr, so we need to use
               --  an intermediate copy operation on a type with an alignment
               --  of 1 to recover the value.

               subtype Addr_Buf_T is C.char_array (1 .. Addr_Len);
               Unaligned_Addr : Addr_Buf_T;
               for Unaligned_Addr'Address
                 use Hostent_H_Addr (E, C.int (J - Result.Addresses'First));
               pragma Import (Ada, Unaligned_Addr);

               Aligned_Addr : Addr_Buf_T;
               for Aligned_Addr'Address use Ia'Address;
               pragma Import (Ada, Aligned_Addr);

            begin
               Aligned_Addr := Unaligned_Addr;
               if Family = Family_Inet6 then
                  To_Inet_Addr (Ia.In6, Result.Addresses (J));
               else
                  To_Inet_Addr (Ia.In4, Result.Addresses (J));
               end if;
            end;
         end loop;
      end return;
   end To_Host_Entry;

   ------------
   -- To_Int --
   ------------

   function To_Int (F : Request_Flag_Type) return C.int is
      Current : Request_Flag_Type := F;
      Result  : C.int := 0;

   begin
      for J in Flags'Range loop
         exit when Current = 0;

         if Current mod 2 /= 0 then
            if Flags (J) = -1 then
               pragma Annotate
                 (CodePeer, False_Positive,
                  "test always false", "self fulfilling prophecy");

               Raise_Socket_Error (SOSC.EOPNOTSUPP);
            end if;

            Result := Result + Flags (J);
         end if;

         Current := Current / 2;
      end loop;

      return Result;
   end To_Int;

   -------------
   -- To_Name --
   -------------

   function To_Name (N : String) return Name_Type is
   begin
      return Name_Type'(N'Length, N);
   end To_Name;

   ----------------------
   -- To_Service_Entry --
   ----------------------

   function To_Service_Entry (E : Servent_Access) return Service_Entry_Type is
      Aliases_Count : Natural;

   begin
      Aliases_Count := 0;
      while Servent_S_Alias (E, C.int (Aliases_Count)) /= Null_Address loop
         Aliases_Count := Aliases_Count + 1;
      end loop;

      return Result : Service_Entry_Type (Aliases_Length   => Aliases_Count) do
         Result.Official := To_Name (Value (Servent_S_Name (E)));

         for J in Result.Aliases'Range loop
            Result.Aliases (J) :=
              To_Name (Value (Servent_S_Alias
                                (E, C.int (J - Result.Aliases'First))));
         end loop;

         Result.Protocol := To_Name (Value (Servent_S_Proto (E)));
         Result.Port :=
           Port_Type (Network_To_Short (Servent_S_Port (E)));
      end return;
   end To_Service_Entry;

   ---------------
   -- To_String --
   ---------------

   function To_String (HN : Name_Type) return String is
   begin
      return HN.Name (1 .. HN.Length);
   end To_String;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval (Val : Timeval_Duration) return Timeval is
      S  : time_t;
      uS : suseconds_t;

   begin
      --  If zero, set result as zero (otherwise it gets rounded down to -1)

      if Val = 0.0 then
         S  := 0;
         uS := 0;

      --  Normal case where we do round down

      else
         S  := time_t (Val - 0.5);
         uS := suseconds_t (1_000_000 * (Val - Selector_Duration (S)) - 0.5);

         if uS = -1 then
            --  It happen on integer duration
            uS := 0;
         end if;
      end if;

      return (S, uS);
   end To_Timeval;

   -----------
   -- Value --
   -----------

   function Value (S : System.Address) return String is
      Str : String (1 .. Positive'Last);
      for Str'Address use S;
      pragma Import (Ada, Str);

      Terminator : Positive := Str'First;

   begin
      while Str (Terminator) /= ASCII.NUL loop
         Terminator := Terminator + 1;
      end loop;

      return Str (1 .. Terminator - 1);
   end Value;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Datagram_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      Last : Stream_Element_Offset;

   begin
      Send_Socket
        (Stream.Socket,
         Item,
         Last,
         Stream.To);

      --  It is an error if not all of the data has been sent

      if Last /= Item'Last then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      First : Ada.Streams.Stream_Element_Offset;
      Index : Ada.Streams.Stream_Element_Offset;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      First := Item'First;
      Index := First - 1;
      while First <= Max loop
         Send_Socket (Stream.Socket, Item (First .. Max), Index, null);

         --  Exit when all or zero data sent. Zero means that the socket has
         --  been closed by peer.

         exit when Index < First or else Index = Max;

         First := Index + 1;
      end loop;

      --  For an empty array, we have First > Max, and hence Index >= Max (no
      --  error, the loop above is never executed). After a successful send,
      --  Index = Max. The only remaining case, Index < Max, is therefore
      --  always an actual send failure.

      if Index < Max then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Write;

   Sockets_Library_Controller_Object : Sockets_Library_Controller;
   pragma Unreferenced (Sockets_Library_Controller_Object);
   --  The elaboration and finalization of this object perform the required
   --  initialization and cleanup actions for the sockets library.

   --------------------
   -- Create_Address --
   --------------------

   function Create_Address
     (Family : Family_Inet_4_6; Bytes : Inet_Addr_Bytes) return Inet_Addr_Type
   is
     (case Family is
         when Family_Inet => (Family_Inet, Bytes),
         when Family_Inet6 => (Family_Inet6, Bytes));

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes (Addr : Inet_Addr_Type) return Inet_Addr_Bytes is
     (case Addr.Family is
         when Family_Inet => Addr.Sin_V4,
         when Family_Inet6 => Addr.Sin_V6);

   ----------
   -- Mask --
   ----------

   function Mask
     (Family : Family_Inet_4_6;
      Length : Natural;
      Host   : Boolean := False) return Inet_Addr_Type
   is
      Addr_Len : constant Natural := Inet_Addr_Bytes_Length (Family);
   begin
      if Length > 8 * Addr_Len then
         raise Constraint_Error with
           "invalid mask length for address family " & Family'Img;
      end if;

      declare
         B    : Inet_Addr_Bytes (1 ..  Addr_Len);
         Part : Inet_Addr_Comp_Type;
      begin
         for J in 1 .. Length / 8 loop
            B (J) := (if Host then 0 else 255);
         end loop;

         if Length < 8 * Addr_Len then
            Part := 2 ** (8 - Length mod 8) - 1;
            B (Length / 8 + 1) := (if Host then Part else not Part);

            for J in Length / 8 + 2 .. B'Last loop
               B (J) := (if Host then 255 else 0);
            end loop;
         end if;

         return Create_Address (Family, B);
      end;
   end Mask;

   -------------------------
   -- Unix_Socket_Address --
   -------------------------

   function Unix_Socket_Address (Addr : String) return Sock_Addr_Type is
   begin
      return Sock_Addr_Type'(Family_Unix, ASU.To_Unbounded_String (Addr));
   end Unix_Socket_Address;

   -----------
   -- "and" --
   -----------

   function "and" (Addr, Mask : Inet_Addr_Type) return Inet_Addr_Type is
   begin
      if Addr.Family /= Mask.Family then
         raise Constraint_Error with "incompatible address families";
      end if;

      declare
         A : constant Inet_Addr_Bytes := Get_Bytes (Addr);
         M : constant Inet_Addr_Bytes := Get_Bytes (Mask);
         R : Inet_Addr_Bytes (A'Range);

      begin
         for J in A'Range loop
            R (J) := A (J) and M (J);
         end loop;
         return Create_Address (Addr.Family, R);
      end;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Net, Host : Inet_Addr_Type) return Inet_Addr_Type is
   begin
      if Net.Family /= Host.Family then
         raise Constraint_Error with "incompatible address families";
      end if;

      declare
         N : constant Inet_Addr_Bytes := Get_Bytes (Net);
         H : constant Inet_Addr_Bytes := Get_Bytes (Host);
         R : Inet_Addr_Bytes (N'Range);

      begin
         for J in N'Range loop
            R (J) := N (J) or H (J);
         end loop;
         return Create_Address (Net.Family, R);
      end;
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (Mask : Inet_Addr_Type) return Inet_Addr_Type is
      M : constant Inet_Addr_Bytes := Get_Bytes (Mask);
      R : Inet_Addr_Bytes (M'Range);
   begin
      for J in R'Range loop
         R (J) := not M (J);
      end loop;
      return Create_Address (Mask.Family, R);
   end "not";

end GNAT.Sockets;
