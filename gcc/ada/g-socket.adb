------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . S O C K E T S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Streams;              use Ada.Streams;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with GNAT.Sockets.Thin_Common; use GNAT.Sockets.Thin_Common;
with GNAT.Sockets.Thin;        use GNAT.Sockets.Thin;

with GNAT.Sockets.Linker_Options;
pragma Warnings (Off, GNAT.Sockets.Linker_Options);
--  Need to include pragma Linker_Options which is platform dependent

with System;               use System;
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;
with System.Task_Lock;

package body GNAT.Sockets is

   package C renames Interfaces.C;

   use type C.int;

   ENOERROR : constant := 0;

   Netdb_Buffer_Size : constant := SOSC.Need_Netdb_Buffer * 1024;
   Need_Netdb_Lock   : constant Boolean := SOSC.Need_Netdb_Lock /= 0;
   --  The network database functions gethostbyname, gethostbyaddr,
   --  getservbyname and getservbyport can either be guaranteed task safe by
   --  the operating system, or else return data through a user-provided buffer
   --  to ensure concurrent uses do not interfere.

   --  Correspondence tables

   Levels : constant array (Level_Type) of C.int :=
              (Socket_Level              => SOSC.SOL_SOCKET,
               IP_Protocol_For_IP_Level  => SOSC.IPPROTO_IP,
               IP_Protocol_For_UDP_Level => SOSC.IPPROTO_UDP,
               IP_Protocol_For_TCP_Level => SOSC.IPPROTO_TCP);

   Modes : constant array (Mode_Type) of C.int :=
             (Socket_Stream   => SOSC.SOCK_STREAM,
              Socket_Datagram => SOSC.SOCK_DGRAM);

   Shutmodes : constant array (Shutmode_Type) of C.int :=
                 (Shut_Read       => SOSC.SHUT_RD,
                  Shut_Write      => SOSC.SHUT_WR,
                  Shut_Read_Write => SOSC.SHUT_RDWR);

   Requests : constant array (Request_Name) of SOSC.IOCTL_Req_T :=
                (Non_Blocking_IO => SOSC.FIONBIO,
                 N_Bytes_To_Read => SOSC.FIONREAD);

   Options : constant array (Option_Name) of C.int :=
               (Keep_Alive          => SOSC.SO_KEEPALIVE,
                Reuse_Address       => SOSC.SO_REUSEADDR,
                Broadcast           => SOSC.SO_BROADCAST,
                Send_Buffer         => SOSC.SO_SNDBUF,
                Receive_Buffer      => SOSC.SO_RCVBUF,
                Linger              => SOSC.SO_LINGER,
                Error               => SOSC.SO_ERROR,
                No_Delay            => SOSC.TCP_NODELAY,
                Add_Membership      => SOSC.IP_ADD_MEMBERSHIP,
                Drop_Membership     => SOSC.IP_DROP_MEMBERSHIP,
                Multicast_If        => SOSC.IP_MULTICAST_IF,
                Multicast_TTL       => SOSC.IP_MULTICAST_TTL,
                Multicast_Loop      => SOSC.IP_MULTICAST_LOOP,
                Receive_Packet_Info => SOSC.IP_PKTINFO,
                Send_Timeout        => SOSC.SO_SNDTIMEO,
                Receive_Timeout     => SOSC.SO_RCVTIMEO);
   --  ??? Note: for OpenSolaris, Receive_Packet_Info should be IP_RECVPKTINFO,
   --  but for Linux compatibility this constant is the same as IP_PKTINFO.

   Flags : constant array (0 .. 3) of C.int :=
             (0 => SOSC.MSG_OOB,     --  Process_Out_Of_Band_Data
              1 => SOSC.MSG_PEEK,    --  Peek_At_Incoming_Data
              2 => SOSC.MSG_WAITALL, --  Wait_For_A_Full_Reception
              3 => SOSC.MSG_EOR);    --  Send_End_Of_Record

   Socket_Error_Id : constant Exception_Id := Socket_Error'Identity;
   Host_Error_Id   : constant Exception_Id := Host_Error'Identity;

   Hex_To_Char : constant String (1 .. 16) := "0123456789ABCDEF";
   --  Use to print in hexadecimal format

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

   function Short_To_Network
     (S : C.unsigned_short) return C.unsigned_short;
   pragma Inline (Short_To_Network);
   --  Convert a port number into a network port number

   function Network_To_Short
     (S : C.unsigned_short) return C.unsigned_short
   renames Short_To_Network;
   --  Symmetric operation

   function Image
     (Val :  Inet_Addr_VN_Type;
      Hex :  Boolean := False) return String;
   --  Output an array of inet address components in hex or decimal mode

   function Is_IP_Address (Name : String) return Boolean;
   --  Return true when Name is an IP address in standard dot notation

   procedure Netdb_Lock;
   pragma Inline (Netdb_Lock);
   procedure Netdb_Unlock;
   pragma Inline (Netdb_Unlock);
   --  Lock/unlock operation used to protect netdb access for platforms that
   --  require such protection.

   function To_In_Addr (Addr : Inet_Addr_Type) return In_Addr;
   procedure To_Inet_Addr
     (Addr   : In_Addr;
      Result : out Inet_Addr_Type);
   --  Conversion functions

   function To_Host_Entry (E : Hostent_Access) return Host_Entry_Type;
   --  Conversion function

   function To_Service_Entry (E : Servent_Access) return Service_Entry_Type;
   --  Conversion function

   function Value (S : System.Address) return String;
   --  Same as Interfaces.C.Strings.Value but taking a System.Address (on VMS,
   --  chars_ptr is a 32-bit pointer, and here we need a 64-bit version).

   function To_Timeval (Val : Timeval_Duration) return Timeval;
   --  Separate Val in seconds and microseconds

   function To_Duration (Val : Timeval) return Timeval_Duration;
   --  Reconstruct a Duration value from a Timeval record (seconds and
   --  microseconds).

   procedure Raise_Socket_Error (Error : Integer);
   --  Raise Socket_Error with an exception message describing the error code
   --  from errno.

   procedure Raise_Host_Error (H_Error : Integer);
   --  Raise Host_Error exception with message describing error code (note
   --  hstrerror seems to be obsolete) from h_errno.

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

   procedure Stream_Write
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : access Sock_Addr_Type);
   --  Common implementation for the Write operation of Datagram_Socket_Stream_
   --  Type and Stream_Socket_Stream_Type.

   procedure Wait_On_Socket
     (Socket   : Socket_Type;
      For_Read : Boolean;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status);
   --  Common code for variants of socket operations supporting a timeout:
   --  block in Check_Selector on Socket for at most the indicated timeout.
   --  If For_Read is True, Socket is added to the read set for this call, else
   --  it is added to the write set. If no selector is provided, a local one is
   --  created for this call and destroyed prior to returning.

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
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Res := C_Accept (C.int (Server), Sin'Address, Len'Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Socket := Socket_Type (Res);

      To_Inet_Addr (Sin.Sin_Addr, Address.Addr);
      Address.Port := Port_Type (Network_To_Short (Sin.Sin_Port));
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
        (Socket    => Server,
         For_Read  => True,
         Timeout   => Timeout,
         Selector  => Selector,
         Status    => Status);

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
      Sin : aliased Sockaddr_In;
      Len : constant C.int := Sin'Size / 8;
      --  This assumes that Address.Family = Family_Inet???

   begin
      if Address.Family = Family_Inet6 then
         raise Socket_Error with "IPv6 not supported";
      end if;

      Set_Family  (Sin.Sin_Family, Address.Family);
      Set_Address (Sin'Unchecked_Access, To_In_Addr (Address.Addr));
      Set_Port
        (Sin'Unchecked_Access,
         Short_To_Network (C.unsigned_short (Address.Port)));

      Res := C_Bind (C.int (Socket), Sin'Address, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Bind_Socket;

   ----------------------
   -- Check_For_Fd_Set --
   ----------------------

   procedure Check_For_Fd_Set (Fd : Socket_Type) is
      use SOSC;

   begin
      --  On Windows, fd_set is a FD_SETSIZE array of socket ids:
      --  no check required. Warnings suppressed because condition
      --  is known at compile time.

      if Target_OS = Windows then

         return;

      --  On other platforms, fd_set is an FD_SETSIZE bitmap: check
      --  that Fd is within range (otherwise behaviour is undefined).

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
      Sin : aliased Sockaddr_In;
      Len : constant C.int := Sin'Size / 8;

   begin
      if Server.Family = Family_Inet6 then
         raise Socket_Error with "IPv6 not supported";
      end if;

      Set_Family  (Sin.Sin_Family, Server.Family);
      Set_Address (Sin'Unchecked_Access, To_In_Addr (Server.Addr));
      Set_Port
        (Sin'Unchecked_Access,
         Short_To_Network (C.unsigned_short (Server.Port)));

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
            For_Read => False,
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
      Mode   : Mode_Type   := Socket_Stream)
   is
      Res : C.int;

   begin
      Res := C_Socket (Families (Family), Modes (Mode), 0);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Socket := Socket_Type (Res);
   end Create_Socket;

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

   -------------------------
   -- Get_Host_By_Address --
   -------------------------

   function Get_Host_By_Address
     (Address : Inet_Addr_Type;
      Family  : Family_Type := Family_Inet) return Host_Entry_Type
   is
      pragma Unreferenced (Family);

      HA     : aliased In_Addr := To_In_Addr (Address);
      Buflen : constant C.int := Netdb_Buffer_Size;
      Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
      Res    : aliased Hostent;
      Err    : aliased C.int;

   begin
      Netdb_Lock;

      if C_Gethostbyaddr (HA'Address, HA'Size / 8, SOSC.AF_INET,
                             Res'Access, Buf'Address, Buflen, Err'Access) /= 0
      then
         Netdb_Unlock;
         Raise_Host_Error (Integer (Err));
      end if;

      return H : constant Host_Entry_Type :=
                   To_Host_Entry (Res'Unchecked_Access)
      do
         Netdb_Unlock;
      end return;
   end Get_Host_By_Address;

   ----------------------
   -- Get_Host_By_Name --
   ----------------------

   function Get_Host_By_Name (Name : String) return Host_Entry_Type is
   begin
      --  Detect IP address name and redirect to Inet_Addr

      if Is_IP_Address (Name) then
         return Get_Host_By_Address (Inet_Addr (Name));
      end if;

      declare
         HN     : constant C.char_array := C.To_C (Name);
         Buflen : constant C.int := Netdb_Buffer_Size;
         Buf    : aliased C.char_array (1 .. Netdb_Buffer_Size);
         Res    : aliased Hostent;
         Err    : aliased C.int;

      begin
         Netdb_Lock;

         if C_Gethostbyname
           (HN, Res'Access, Buf'Address, Buflen, Err'Access) /= 0
         then
            Netdb_Unlock;
            Raise_Host_Error (Integer (Err));
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
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;
      Res : Sock_Addr_Type (Family_Inet);

   begin
      if C_Getpeername (C.int (Socket), Sin'Address, Len'Access) = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      To_Inet_Addr (Sin.Sin_Addr, Res.Addr);
      Res.Port := Port_Type (Network_To_Short (Sin.Sin_Port));

      return Res;
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
      Buflen : constant C.int := Netdb_Buffer_Size;
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
      Buflen : constant C.int := Netdb_Buffer_Size;
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
      Sin  : aliased Sockaddr_In;
      Len  : aliased C.int := Sin'Size / 8;
      Res  : C.int;
      Addr : Sock_Addr_Type := No_Sock_Addr;

   begin
      Res := C_Getsockname (C.int (Socket), Sin'Address, Len'Access);

      if Res /= Failure then
         To_Inet_Addr (Sin.Sin_Addr, Addr.Addr);
         Addr.Port := Port_Type (Network_To_Short (Sin.Sin_Port));
      end if;

      return Addr;
   end Get_Socket_Name;

   -----------------------
   -- Get_Socket_Option --
   -----------------------

   function Get_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Name   : Option_Name) return Option_Type
   is
      use SOSC;
      use type C.unsigned_char;

      V8  : aliased Two_Ints;
      V4  : aliased C.int;
      V1  : aliased C.unsigned_char;
      VT  : aliased Timeval;
      Len : aliased C.int;
      Add : System.Address;
      Res : C.int;
      Opt : Option_Type (Name);

   begin
      case Name is
         when Multicast_Loop      |
              Multicast_TTL       |
              Receive_Packet_Info =>
            Len := V1'Size / 8;
            Add := V1'Address;

         when Keep_Alive      |
              Reuse_Address   |
              Broadcast       |
              No_Delay        |
              Send_Buffer     |
              Receive_Buffer  |
              Multicast_If    |
              Error           =>
            Len := V4'Size / 8;
            Add := V4'Address;

         when Send_Timeout    |
              Receive_Timeout =>

            --  The standard argument for SO_RCVTIMEO and SO_SNDTIMEO is a
            --  struct timeval, but on Windows it is a milliseconds count in
            --  a DWORD.

            if Target_OS = Windows then
               Len := V4'Size / 8;
               Add := V4'Address;

            else
               Len := VT'Size / 8;
               Add := VT'Address;
            end if;

         when Linger          |
              Add_Membership  |
              Drop_Membership =>
            Len := V8'Size / 8;
            Add := V8'Address;

      end case;

      Res :=
        C_Getsockopt
          (C.int (Socket),
           Levels (Level),
           Options (Name),
           Add, Len'Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      case Name is
         when Keep_Alive      |
              Reuse_Address   |
              Broadcast       |
              No_Delay        =>
            Opt.Enabled := (V4 /= 0);

         when Linger          =>
            Opt.Enabled := (V8 (V8'First) /= 0);
            Opt.Seconds := Natural (V8 (V8'Last));

         when Send_Buffer     |
              Receive_Buffer  =>
            Opt.Size := Natural (V4);

         when Error           =>
            Opt.Error := Resolve_Error (Integer (V4));

         when Add_Membership  |
              Drop_Membership =>
            To_Inet_Addr (To_In_Addr (V8 (V8'First)), Opt.Multicast_Address);
            To_Inet_Addr (To_In_Addr (V8 (V8'Last)), Opt.Local_Interface);

         when Multicast_If    =>
            To_Inet_Addr (To_In_Addr (V4), Opt.Outgoing_If);

         when Multicast_TTL   =>
            Opt.Time_To_Live := Integer (V1);

         when Multicast_Loop      |
              Receive_Packet_Info =>
            Opt.Enabled := (V1 /= 0);

         when Send_Timeout    |
              Receive_Timeout =>

            if Target_OS = Windows then

               --  Timeout is in milliseconds, actual value is 500 ms +
               --  returned value (unless it is 0).

               if V4 = 0 then
                  Opt.Timeout := 0.0;
               else
                  Opt.Timeout := Natural (V4) * 0.001 + 0.500;
               end if;

            else
               Opt.Timeout := To_Duration (VT);
            end if;
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

   function Image
     (Val : Inet_Addr_VN_Type;
      Hex : Boolean := False) return String
   is
      --  The largest Inet_Addr_Comp_Type image occurs with IPv4. It
      --  has at most a length of 3 plus one '.' character.

      Buffer    : String (1 .. 4 * Val'Length);
      Length    : Natural := 1;
      Separator : Character;

      procedure Img10 (V : Inet_Addr_Comp_Type);
      --  Append to Buffer image of V in decimal format

      procedure Img16 (V : Inet_Addr_Comp_Type);
      --  Append to Buffer image of V in hexadecimal format

      -----------
      -- Img10 --
      -----------

      procedure Img10 (V : Inet_Addr_Comp_Type) is
         Img : constant String := V'Img;
         Len : constant Natural := Img'Length - 1;
      begin
         Buffer (Length .. Length + Len - 1) := Img (2 .. Img'Last);
         Length := Length + Len;
      end Img10;

      -----------
      -- Img16 --
      -----------

      procedure Img16 (V : Inet_Addr_Comp_Type) is
      begin
         Buffer (Length)     := Hex_To_Char (Natural (V / 16) + 1);
         Buffer (Length + 1) := Hex_To_Char (Natural (V mod 16) + 1);
         Length := Length + 2;
      end Img16;

   --  Start of processing for Image

   begin
      Separator := (if Hex then ':' else '.');

      for J in Val'Range loop
         if Hex then
            Img16 (Val (J));
         else
            Img10 (Val (J));
         end if;

         if J /= Val'Last then
            Buffer (Length) := Separator;
            Length := Length + 1;
         end if;
      end loop;

      return Buffer (1 .. Length - 1);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Inet_Addr_Type) return String is
   begin
      if Value.Family = Family_Inet then
         return Image (Inet_Addr_VN_Type (Value.Sin_V4), Hex => False);
      else
         return Image (Inet_Addr_VN_Type (Value.Sin_V6), Hex => True);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Sock_Addr_Type) return String is
      Port : constant String := Value.Port'Img;
   begin
      return Image (Value.Addr) & ':' & Port (2 .. Port'Last);
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
      use Interfaces.C.Strings;

      Img    : aliased char_array := To_C (Image);
      Addr   : aliased C.int;
      Res    : C.int;
      Result : Inet_Addr_Type;

   begin
      --  Special case for an empty Image as on some platforms (e.g. Windows)
      --  calling Inet_Addr("") will not return an error.

      if Image = "" then
         Raise_Socket_Error (SOSC.EINVAL);
      end if;

      Res := Inet_Pton (SOSC.AF_INET, Img'Address, Addr'Address);

      if Res < 0 then
         Raise_Socket_Error (Socket_Errno);

      elsif Res = 0 then
         Raise_Socket_Error (SOSC.EINVAL);
      end if;

      To_Inet_Addr (To_In_Addr (Addr), Result);
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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Socket_Set_Type) return Boolean is
   begin
      return Item.Last = No_Socket;
   end Is_Empty;

   -------------------
   -- Is_IP_Address --
   -------------------

   function Is_IP_Address (Name : String) return Boolean is
   begin
      for J in Name'Range loop
         if Name (J) /= '.'
           and then Name (J) not in '0' .. '9'
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_IP_Address;

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
      For_Read : Boolean;
      Timeout  : Selector_Duration;
      Selector : access Selector_Type := null;
      Status   : out Selector_Status)
   is
      type Local_Selector_Access is access Selector_Type;
      for Local_Selector_Access'Storage_Size use Selector_Type'Size;

      S : Selector_Access;
      --  Selector to use for waiting

      R_Fd_Set : Socket_Set_Type;
      W_Fd_Set : Socket_Set_Type;

   begin
      --  Create selector if not provided by the user

      if Selector = null then
         declare
            Local_S : constant Local_Selector_Access := new Selector_Type;
         begin
            S := Local_S.all'Unchecked_Access;
            Create_Selector (S.all);
         end;

      else
         S := Selector.all'Access;
      end if;

      if For_Read then
         Set (R_Fd_Set, Socket);
      else
         Set (W_Fd_Set, Socket);
      end if;

      Check_Selector (S.all, R_Fd_Set, W_Fd_Set, Status, Timeout);

      if Selector = null then
         Close_Selector (S.all);
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

   procedure Raise_Host_Error (H_Error : Integer) is
   begin
      raise Host_Error with
        Err_Code_Image (H_Error)
        & C.Strings.Value (Host_Error_Messages.Host_Error_Message (H_Error));
   end Raise_Host_Error;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error (Error : Integer) is
      use type C.Strings.chars_ptr;
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
      First : Ada.Streams.Stream_Element_Offset          := Item'First;
      Index : Ada.Streams.Stream_Element_Offset          := First - 1;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      loop
         Receive_Socket
           (Stream.Socket,
            Item (First .. Max),
            Index,
            Stream.From);

         Last := Index;

         --  Exit when all or zero data received. Zero means that the socket
         --  peer is closed.

         exit when Index < First or else Index = Max;

         First := Index + 1;
      end loop;
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
      Sin : aliased Sockaddr_In;
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

      To_Inet_Addr (Sin.Sin_Addr, From.Addr);
      From.Port := Port_Type (Network_To_Short (Sin.Sin_Port));
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

      Sin  : aliased Sockaddr_In;
      C_To : System.Address;
      Len  : C.int;

   begin
      if To /= null then
         Set_Family  (Sin.Sin_Family, To.Family);
         Set_Address (Sin'Unchecked_Access, To_In_Addr (To.Addr));
         Set_Port
           (Sin'Unchecked_Access,
            Short_To_Network (C.unsigned_short (To.Port)));
         C_To := Sin'Address;
         Len := Sin'Size / 8;

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
      use SOSC;
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
      Level  : Level_Type := Socket_Level;
      Option : Option_Type)
   is
      use SOSC;

      V8  : aliased Two_Ints;
      V4  : aliased C.int;
      V1  : aliased C.unsigned_char;
      VT  : aliased Timeval;
      Len : C.int;
      Add : System.Address := Null_Address;
      Res : C.int;

   begin
      case Option.Name is
         when Keep_Alive      |
              Reuse_Address   |
              Broadcast       |
              No_Delay        =>
            V4  := C.int (Boolean'Pos (Option.Enabled));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Linger          =>
            V8 (V8'First) := C.int (Boolean'Pos (Option.Enabled));
            V8 (V8'Last)  := C.int (Option.Seconds);
            Len := V8'Size / 8;
            Add := V8'Address;

         when Send_Buffer     |
              Receive_Buffer  =>
            V4  := C.int (Option.Size);
            Len := V4'Size / 8;
            Add := V4'Address;

         when Error           =>
            V4  := C.int (Boolean'Pos (True));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Add_Membership  |
              Drop_Membership =>
            V8 (V8'First) := To_Int (To_In_Addr (Option.Multicast_Address));
            V8 (V8'Last)  := To_Int (To_In_Addr (Option.Local_Interface));
            Len := V8'Size / 8;
            Add := V8'Address;

         when Multicast_If    =>
            V4  := To_Int (To_In_Addr (Option.Outgoing_If));
            Len := V4'Size / 8;
            Add := V4'Address;

         when Multicast_TTL   =>
            V1  := C.unsigned_char (Option.Time_To_Live);
            Len := V1'Size / 8;
            Add := V1'Address;

         when Multicast_Loop      |
              Receive_Packet_Info =>
            V1  := C.unsigned_char (Boolean'Pos (Option.Enabled));
            Len := V1'Size / 8;
            Add := V1'Address;

         when Send_Timeout    |
              Receive_Timeout =>

            if Target_OS = Windows then

               --  On Windows, the timeout is a DWORD in milliseconds, and
               --  the actual timeout is 500 ms + the given value (unless it
               --  is 0).

               V4 := C.int (Option.Timeout / 0.001);

               if V4 > 500 then
                  V4 := V4 - 500;

               elsif V4 > 0 then
                  V4 := 1;
               end if;

               Len := V4'Size / 8;
               Add := V4'Address;

            else
               VT  := To_Timeval (Option.Timeout);
               Len := VT'Size / 8;
               Add := VT'Address;
            end if;

      end case;

      Res := C_Setsockopt
        (C.int (Socket),
         Levels (Level),
         Options (Option.Name),
         Add, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Set_Socket_Option;

   ----------------------
   -- Short_To_Network --
   ----------------------

   function Short_To_Network (S : C.unsigned_short) return C.unsigned_short is
      use type C.unsigned_short;

   begin
      --  Big-endian case. No conversion needed. On these platforms, htons()
      --  defaults to a null procedure.

      if Default_Bit_Order = High_Order_First then
         return S;

      --  Little-endian case. We must swap the high and low bytes of this
      --  short to make the port number network compliant.

      else
         return (S / 256) + (S mod 256) * 256;
      end if;
   end Short_To_Network;

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

   ------------------
   -- Stream_Write --
   ------------------

   procedure Stream_Write
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : access Sock_Addr_Type)
   is
      First : Ada.Streams.Stream_Element_Offset;
      Index : Ada.Streams.Stream_Element_Offset;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      First := Item'First;
      Index := First - 1;
      while First <= Max loop
         Send_Socket (Socket, Item (First .. Max), Index, To);

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
   end Stream_Write;

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
   begin
      return Natural (Val.Tv_Sec) * 1.0 + Natural (Val.Tv_Usec) * 1.0E-6;
   end To_Duration;

   -------------------
   -- To_Host_Entry --
   -------------------

   function To_Host_Entry (E : Hostent_Access) return Host_Entry_Type is
      use type C.size_t;
      use C.Strings;

      Aliases_Count, Addresses_Count : Natural;

      --  H_Length is not used because it is currently only ever set to 4, as
      --  H_Addrtype is always AF_INET.

   begin
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
               Addr : In_Addr;

               --  Hostent_H_Addr (E, <index>) may return an address that is
               --  not correctly aligned for In_Addr, so we need to use
               --  an intermediate copy operation on a type with an alignemnt
               --  of 1 to recover the value.

               subtype Addr_Buf_T is C.char_array (1 .. Addr'Size / 8);
               Unaligned_Addr : Addr_Buf_T;
               for Unaligned_Addr'Address
                 use Hostent_H_Addr (E, C.int (J - Result.Addresses'First));
               pragma Import (Ada, Unaligned_Addr);

               Aligned_Addr : Addr_Buf_T;
               for Aligned_Addr'Address use Addr'Address;
               pragma Import (Ada, Aligned_Addr);

            begin
               Aligned_Addr := Unaligned_Addr;
               To_Inet_Addr (Addr, Result.Addresses (J));
            end;
         end loop;
      end return;
   end To_Host_Entry;

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In_Addr (Addr : Inet_Addr_Type) return In_Addr is
   begin
      if Addr.Family = Family_Inet then
         return (S_B1 => C.unsigned_char (Addr.Sin_V4 (1)),
                 S_B2 => C.unsigned_char (Addr.Sin_V4 (2)),
                 S_B3 => C.unsigned_char (Addr.Sin_V4 (3)),
                 S_B4 => C.unsigned_char (Addr.Sin_V4 (4)));
      end if;

      raise Socket_Error with "IPv6 not supported";
   end To_In_Addr;

   ------------------
   -- To_Inet_Addr --
   ------------------

   procedure To_Inet_Addr
     (Addr   : In_Addr;
      Result : out Inet_Addr_Type) is
   begin
      Result.Sin_V4 (1) := Inet_Addr_Comp_Type (Addr.S_B1);
      Result.Sin_V4 (2) := Inet_Addr_Comp_Type (Addr.S_B2);
      Result.Sin_V4 (3) := Inet_Addr_Comp_Type (Addr.S_B3);
      Result.Sin_V4 (4) := Inet_Addr_Comp_Type (Addr.S_B4);
   end To_Inet_Addr;

   ------------
   -- To_Int --
   ------------

   function To_Int (F : Request_Flag_Type) return C.int
   is
      Current : Request_Flag_Type := F;
      Result  : C.int := 0;

   begin
      for J in Flags'Range loop
         exit when Current = 0;

         if Current mod 2 /= 0 then
            if Flags (J) = -1 then
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
      use C.Strings;
      use type C.size_t;

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
         uS := suseconds_t (1_000_000 * (Val - Selector_Duration (S)));
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
   begin
      Stream_Write (Stream.Socket, Item, To => Stream.To'Unrestricted_Access);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      Stream_Write (Stream.Socket, Item, To => null);
   end Write;

   Sockets_Library_Controller_Object : Sockets_Library_Controller;
   pragma Unreferenced (Sockets_Library_Controller_Object);
   --  The elaboration and finalization of this object perform the required
   --  initialization and cleanup actions for the sockets library.

end GNAT.Sockets;
