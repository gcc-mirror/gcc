------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . S O C K E T S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2001-2004 Ada Core Technologies, Inc.            --
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

with Ada.Streams;                use Ada.Streams;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Sockets.Constants;
with GNAT.Sockets.Thin;          use GNAT.Sockets.Thin;
with GNAT.Task_Lock;

with GNAT.Sockets.Linker_Options;
pragma Warnings (Off, GNAT.Sockets.Linker_Options);
--  Need to include pragma Linker_Options which is platform dependent

with System; use System;

package body GNAT.Sockets is

   use type C.int, System.Address;

   Finalized   : Boolean := False;
   Initialized : Boolean := False;

   ENOERROR : constant := 0;

   --  Correspondance tables

   Families : constant array (Family_Type) of C.int :=
     (Family_Inet  => Constants.AF_INET,
      Family_Inet6 => Constants.AF_INET6);

   Levels : constant array (Level_Type) of C.int :=
     (Socket_Level              => Constants.SOL_SOCKET,
      IP_Protocol_For_IP_Level  => Constants.IPPROTO_IP,
      IP_Protocol_For_UDP_Level => Constants.IPPROTO_UDP,
      IP_Protocol_For_TCP_Level => Constants.IPPROTO_TCP);

   Modes : constant array (Mode_Type) of C.int :=
     (Socket_Stream   => Constants.SOCK_STREAM,
      Socket_Datagram => Constants.SOCK_DGRAM);

   Shutmodes : constant array (Shutmode_Type) of C.int :=
     (Shut_Read       => Constants.SHUT_RD,
      Shut_Write      => Constants.SHUT_WR,
      Shut_Read_Write => Constants.SHUT_RDWR);

   Requests : constant array (Request_Name) of C.int :=
     (Non_Blocking_IO => Constants.FIONBIO,
      N_Bytes_To_Read => Constants.FIONREAD);

   Options : constant array (Option_Name) of C.int :=
     (Keep_Alive      => Constants.SO_KEEPALIVE,
      Reuse_Address   => Constants.SO_REUSEADDR,
      Broadcast       => Constants.SO_BROADCAST,
      Send_Buffer     => Constants.SO_SNDBUF,
      Receive_Buffer  => Constants.SO_RCVBUF,
      Linger          => Constants.SO_LINGER,
      Error           => Constants.SO_ERROR,
      No_Delay        => Constants.TCP_NODELAY,
      Add_Membership  => Constants.IP_ADD_MEMBERSHIP,
      Drop_Membership => Constants.IP_DROP_MEMBERSHIP,
      Multicast_TTL   => Constants.IP_MULTICAST_TTL,
      Multicast_Loop  => Constants.IP_MULTICAST_LOOP);

   Flags : constant array (0 .. 3) of C.int :=
            (0 => Constants.MSG_OOB,     --  Process_Out_Of_Band_Data
             1 => Constants.MSG_PEEK,    --  Peek_At_Incoming_Data
             2 => Constants.MSG_WAITALL, --  Wait_For_A_Full_Reception
             3 => Constants.MSG_EOR);    --  Send_End_Of_Record

   Socket_Error_Id : constant Exception_Id := Socket_Error'Identity;
   Host_Error_Id   : constant Exception_Id := Host_Error'Identity;

   Hex_To_Char : constant String (1 .. 16) := "0123456789ABCDEF";
   --  Use to print in hexadecimal format

   function To_In_Addr is new Ada.Unchecked_Conversion (C.int, In_Addr);
   function To_Int     is new Ada.Unchecked_Conversion (In_Addr, C.int);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Resolve_Error
     (Error_Value : Integer;
      From_Errno  : Boolean := True) return Error_Type;
   --  Associate an enumeration value (error_type) to en error value
   --  (errno). From_Errno prevents from mixing h_errno with errno.

   function To_Name   (N  : String) return Name_Type;
   function To_String (HN : Name_Type) return String;
   --  Conversion functions

   function To_Int (F : Request_Flag_Type) return C.int;
   --  Return the int value corresponding to the specified flags combination

   function Set_Forced_Flags (F : C.int) return C.int;
   --  Return F with the bits from Constants.MSG_Forced_Flags forced set

   function Short_To_Network
     (S : C.unsigned_short) return C.unsigned_short;
   pragma Inline (Short_To_Network);
   --  Convert a port number into a network port number

   function Network_To_Short
     (S : C.unsigned_short) return C.unsigned_short
   renames Short_To_Network;
   --  Symetric operation

   function Image
     (Val :  Inet_Addr_VN_Type;
      Hex :  Boolean := False) return String;
   --  Output an array of inet address components either in
   --  hexadecimal or in decimal mode.

   function Is_IP_Address (Name : String) return Boolean;
   --  Return true when Name is an IP address in standard dot notation.

   function To_In_Addr (Addr : Inet_Addr_Type) return Thin.In_Addr;
   function To_Inet_Addr (Addr : In_Addr) return Inet_Addr_Type;
   --  Conversion functions

   function To_Host_Entry (E : Hostent) return Host_Entry_Type;
   --  Conversion function

   function To_Service_Entry (E : Servent) return Service_Entry_Type;
   --  Conversion function

   function To_Timeval (Val : Selector_Duration) return Timeval;
   --  Separate Val in seconds and microseconds

   procedure Raise_Socket_Error (Error : Integer);
   --  Raise Socket_Error with an exception message describing
   --  the error code.

   procedure Raise_Host_Error (Error : Integer);
   --  Raise Host_Error exception with message describing error code
   --  (note hstrerror seems to be obsolete).

   procedure Narrow (Item : in out Socket_Set_Type);
   --  Update Last as it may be greater than the real last socket

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
      Buf : aliased Character := ASCII.NUL;
      Res : C.int;

   begin
      --  Send an empty array to unblock C select system call

      Res := C_Send (C.int (Selector.W_Sig_Socket), Buf'Address, 1,
                     Constants.MSG_Forced_Flags);
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

      Address.Addr := To_Inet_Addr (Sin.Sin_Addr);
      Address.Port := Port_Type (Network_To_Short (Sin.Sin_Port));
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

   begin
      if Address.Family = Family_Inet6 then
         raise Socket_Error;
      end if;

      Set_Length (Sin'Unchecked_Access, Len);
      Set_Family (Sin'Unchecked_Access, Families (Address.Family));
      Set_Port
        (Sin'Unchecked_Access,
         Short_To_Network (C.unsigned_short (Address.Port)));

      Res := C_Bind (C.int (Socket), Sin'Address, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
   end Bind_Socket;

   --------------------
   -- Check_Selector --
   --------------------

   procedure Check_Selector
     (Selector     : in out Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever)
   is
      E_Socket_Set : Socket_Set_Type; --  (No_Socket, No_Socket_Set)
   begin
      Check_Selector
        (Selector, R_Socket_Set, W_Socket_Set, E_Socket_Set, Status, Timeout);
   end Check_Selector;

   procedure Check_Selector
     (Selector     : in out Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      E_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : Selector_Duration := Forever)
   is
      Res  : C.int;
      Last : C.int;
      RSet : Socket_Set_Type;
      WSet : Socket_Set_Type;
      ESet : Socket_Set_Type;
      TVal : aliased Timeval;
      TPtr : Timeval_Access;

   begin
      Status := Completed;

      --  No timeout or Forever is indicated by a null timeval pointer

      if Timeout = Forever then
         TPtr := null;
      else
         TVal := To_Timeval (Timeout);
         TPtr := TVal'Unchecked_Access;
      end if;

      --  Copy R_Socket_Set in RSet and add read signalling socket

      RSet := (Set  => New_Socket_Set (R_Socket_Set.Set),
               Last => R_Socket_Set.Last);
      Set (RSet, Selector.R_Sig_Socket);

      --  Copy W_Socket_Set in WSet

      WSet := (Set  => New_Socket_Set (W_Socket_Set.Set),
               Last => W_Socket_Set.Last);

      --  Copy E_Socket_Set in ESet

      ESet := (Set  => New_Socket_Set (E_Socket_Set.Set),
               Last => E_Socket_Set.Last);

      Last := C.int'Max (C.int'Max (C.int (RSet.Last),
                                    C.int (WSet.Last)),
                                    C.int (ESet.Last));

      Res :=
        C_Select
         (Last + 1,
          RSet.Set,
          WSet.Set,
          ESet.Set,
          TPtr);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      --  If Select was resumed because of read signalling socket,
      --  read this data and remove socket from set.

      if Is_Set (RSet, Selector.R_Sig_Socket) then
         Clear (RSet, Selector.R_Sig_Socket);

         declare
            Buf : Character;

         begin
            Res := C_Recv (C.int (Selector.R_Sig_Socket), Buf'Address, 1, 0);

            if Res = Failure then
               Raise_Socket_Error (Socket_Errno);
            end if;
         end;

         Status := Aborted;

      elsif Res = 0 then
         Status := Expired;
      end if;

      --  Update RSet, WSet and ESet in regard to their new socket
      --  sets.

      Narrow (RSet);
      Narrow (WSet);
      Narrow (ESet);

      --  Reset RSet as it should be if R_Sig_Socket was not added

      if Is_Empty (RSet) then
         Empty (RSet);
      end if;

      if Is_Empty (WSet) then
         Empty (WSet);
      end if;

      if Is_Empty (ESet) then
         Empty (ESet);
      end if;

      --  Deliver RSet, WSet and ESet

      Empty (R_Socket_Set);
      R_Socket_Set := RSet;

      Empty (W_Socket_Set);
      W_Socket_Set := WSet;

      Empty (E_Socket_Set);
      E_Socket_Set := ESet;
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
      if Item.Last /= No_Socket then
         Remove_Socket_From_Set (Item.Set, C.int (Socket));
         Last_Socket_In_Set (Item.Set, Last'Unchecked_Access);
         Item.Last := Socket_Type (Last);
      end if;
   end Clear;

   --------------------
   -- Close_Selector --
   --------------------

   --  Comments needed below ???
   --  Why are exceptions ignored ???

   procedure Close_Selector (Selector : in out Selector_Type) is
   begin
      begin
         Close_Socket (Selector.R_Sig_Socket);

      exception
         when Socket_Error =>
            null;
      end;

      begin
         Close_Socket (Selector.W_Sig_Socket);

      exception
         when Socket_Error =>
            null;
      end;
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

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : in out Sock_Addr_Type)
   is
      Res : C.int;
      Sin : aliased Sockaddr_In;
      Len : constant C.int := Sin'Size / 8;

   begin
      if Server.Family = Family_Inet6 then
         raise Socket_Error;
      end if;

      Set_Length (Sin'Unchecked_Access, Len);
      Set_Family (Sin'Unchecked_Access, Families (Server.Family));
      Set_Address (Sin'Unchecked_Access, To_In_Addr (Server.Addr));
      Set_Port
        (Sin'Unchecked_Access,
         Short_To_Network (C.unsigned_short (Server.Port)));

      Res := C_Connect (C.int (Socket), Sin'Address, Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
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

      Res := C_Ioctl
        (C.int (Socket),
         Requests (Request.Name),
         Arg'Unchecked_Access);

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
      Target : in out Socket_Set_Type)
   is
   begin
      Empty (Target);
      if Source.Last /= No_Socket then
         Target.Set  := New_Socket_Set (Source.Set);
         Target.Last := Source.Last;
      end if;
   end Copy;

   ---------------------
   -- Create_Selector --
   ---------------------

   procedure Create_Selector (Selector : out Selector_Type) is
      S0  : C.int;
      S1  : C.int;
      S2  : C.int;
      Res : C.int;
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;
      Err : Integer;

   begin
      --  We open two signalling sockets. One of them is used to
      --  send data to the other, which is included in a C_Select
      --  socket set. The communication is used to force the call
      --  to C_Select to complete, and the waiting task to resume
      --  its execution.

      --  Create a listening socket

      S0 := C_Socket (Constants.AF_INET, Constants.SOCK_STREAM, 0);
      if S0 = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      --  Sin is already correctly initialized. Bind the socket to any
      --  unused port.

      Res := C_Bind (S0, Sin'Address, Len);
      if Res = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Raise_Socket_Error (Err);
      end if;

      --  Get the port used by the socket

      Res := C_Getsockname (S0, Sin'Address, Len'Access);

      if Res = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Raise_Socket_Error (Err);
      end if;

      Res := C_Listen (S0, 2);

      if Res = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Raise_Socket_Error (Err);
      end if;

      S1 := C_Socket (Constants.AF_INET, Constants.SOCK_STREAM, 0);

      if S1 = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Raise_Socket_Error (Err);
      end if;

      --  Use INADDR_LOOPBACK

      Sin.Sin_Addr.S_B1 := 127;
      Sin.Sin_Addr.S_B2 := 0;
      Sin.Sin_Addr.S_B3 := 0;
      Sin.Sin_Addr.S_B4 := 1;

      --  Do a connect and accept the connection

      Res := C_Connect (S1, Sin'Address, Len);

      if Res = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Res := C_Close (S1);
         Raise_Socket_Error (Err);
      end if;

      S2 := C_Accept (S0, Sin'Address, Len'Access);

      if S2 = Failure then
         Err := Socket_Errno;
         Res := C_Close (S0);
         Res := C_Close (S1);
         Raise_Socket_Error (Err);
      end if;

      Res := C_Close (S0);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Selector.R_Sig_Socket := Socket_Type (S1);
      Selector.W_Sig_Socket := Socket_Type (S2);
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

   procedure Empty  (Item : in out Socket_Set_Type) is
   begin
      if Item.Set /= No_Socket_Set then
         Free_Socket_Set (Item.Set);
         Item.Set := No_Socket_Set;
      end if;

      Item.Last := No_Socket;
   end Empty;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if not Finalized
        and then Initialized
      then
         Finalized := True;
         Thin.Finalize;
      end if;
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
           (Item.Set, L'Unchecked_Access, S'Unchecked_Access);
         Item.Last := Socket_Type (L);
         Socket    := Socket_Type (S);
      else
         Socket := No_Socket;
      end if;
   end Get;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Stream : Stream_Access) return Sock_Addr_Type is
   begin
      if Stream = null then
         raise Socket_Error;

      elsif Stream.all in Datagram_Socket_Stream_Type then
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

      HA  : aliased In_Addr := To_In_Addr (Address);
      Res : Hostent_Access;
      Err : Integer;

   begin
      --  This C function is not always thread-safe. Protect against
      --  concurrent access.

      Task_Lock.Lock;
      Res := C_Gethostbyaddr (HA'Address, HA'Size / 8, Constants.AF_INET);

      if Res = null then
         Err := Socket_Errno;
         Task_Lock.Unlock;
         Raise_Host_Error (Err);
      end if;

      --  Translate from the C format to the API format

      declare
         HE : constant Host_Entry_Type := To_Host_Entry (Res.all);

      begin
         Task_Lock.Unlock;
         return HE;
      end;
   end Get_Host_By_Address;

   ----------------------
   -- Get_Host_By_Name --
   ----------------------

   function Get_Host_By_Name (Name : String) return Host_Entry_Type is
      HN  : constant C.char_array := C.To_C (Name);
      Res : Hostent_Access;
      Err : Integer;

   begin
      --  Detect IP address name and redirect to Inet_Addr

      if Is_IP_Address (Name) then
         return Get_Host_By_Address (Inet_Addr (Name));
      end if;

      --  This C function is not always thread-safe. Protect against
      --  concurrent access.

      Task_Lock.Lock;
      Res := C_Gethostbyname (HN);

      if Res = null then
         Err := Socket_Errno;
         Task_Lock.Unlock;
         Raise_Host_Error (Err);
      end if;

      --  Translate from the C format to the API format

      declare
         HE : constant Host_Entry_Type := To_Host_Entry (Res.all);

      begin
         Task_Lock.Unlock;
         return HE;
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

      Res.Addr := To_Inet_Addr (Sin.Sin_Addr);
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
      SN  : constant C.char_array := C.To_C (Name);
      SP  : constant C.char_array := C.To_C (Protocol);
      Res : Servent_Access;

   begin
      --  This C function is not always thread-safe. Protect against
      --  concurrent access.

      Task_Lock.Lock;
      Res := C_Getservbyname (SN, SP);

      if Res = null then
         Task_Lock.Unlock;
         Ada.Exceptions.Raise_Exception
           (Service_Error'Identity, "Service not found");
      end if;

      --  Translate from the C format to the API format

      declare
         SE : constant Service_Entry_Type := To_Service_Entry (Res.all);

      begin
         Task_Lock.Unlock;
         return SE;
      end;
   end Get_Service_By_Name;

   -------------------------
   -- Get_Service_By_Port --
   -------------------------

   function Get_Service_By_Port
     (Port     : Port_Type;
      Protocol : String) return Service_Entry_Type
   is
      SP  : constant C.char_array := C.To_C (Protocol);
      Res : Servent_Access;

   begin
      --  This C function is not always thread-safe. Protect against
      --  concurrent access.

      Task_Lock.Lock;
      Res := C_Getservbyport
        (C.int (Short_To_Network (C.unsigned_short (Port))), SP);

      if Res = null then
         Task_Lock.Unlock;
         Ada.Exceptions.Raise_Exception
           (Service_Error'Identity, "Service not found");
      end if;

      --  Translate from the C format to the API format

      declare
         SE : constant Service_Entry_Type := To_Service_Entry (Res.all);

      begin
         Task_Lock.Unlock;
         return SE;
      end;
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
         Addr.Addr := To_Inet_Addr (Sin.Sin_Addr);
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
      use type C.unsigned_char;

      V8  : aliased Two_Int;
      V4  : aliased C.int;
      V1  : aliased C.unsigned_char;
      Len : aliased C.int;
      Add : System.Address;
      Res : C.int;
      Opt : Option_Type (Name);

   begin
      case Name is
         when Multicast_Loop  |
              Multicast_TTL   =>
            Len := V1'Size / 8;
            Add := V1'Address;

         when Keep_Alive      |
              Reuse_Address   |
              Broadcast       |
              No_Delay        |
              Send_Buffer     |
              Receive_Buffer  |
              Error           =>
            Len := V4'Size / 8;
            Add := V4'Address;

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
           Add, Len'Unchecked_Access);

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
            Opt.Multicast_Address := To_Inet_Addr (To_In_Addr (V8 (V8'First)));
            Opt.Local_Interface   := To_Inet_Addr (To_In_Addr (V8 (V8'Last)));

         when Multicast_TTL   =>
            Opt.Time_To_Live := Integer (V1);

         when Multicast_Loop  =>
            Opt.Enabled := (V1 /= 0);

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
      if Hex then
         Separator := ':';
      else
         Separator := '.';
      end if;

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

   ---------------
   -- Inet_Addr --
   ---------------

   function Inet_Addr (Image : String) return Inet_Addr_Type is
      use Interfaces.C.Strings;

      Img : chars_ptr := New_String (Image);
      Res : C.int;
      Err : Integer;

   begin
      Res := C_Inet_Addr (Img);
      Err := Errno;
      Free (Img);

      if Res = Failure then
         Raise_Socket_Error (Err);
      end if;

      return To_Inet_Addr (To_In_Addr (Res));
   end Inet_Addr;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Process_Blocking_IO : Boolean := False) is
   begin
      if not Initialized then
         Initialized := True;
         Thin.Initialize (Process_Blocking_IO);
      end if;
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

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Item   : Socket_Set_Type;
      Socket : Socket_Type) return Boolean
   is
   begin
      return Item.Last /= No_Socket
        and then Socket <= Item.Last
        and then Is_Socket_In_Set (Item.Set, C.int (Socket)) /= 0;
   end Is_Set;

   -------------------
   -- Listen_Socket --
   -------------------

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Positive := 15)
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
      if Item.Set /= No_Socket_Set then
         Last_Socket_In_Set (Item.Set, Last'Unchecked_Access);
         Item.Last := Socket_Type (Last);
      end if;
   end Narrow;

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

   procedure Raise_Host_Error (Error : Integer) is

      function Host_Error_Message return String;
      --  We do not use a C function like strerror because hstrerror
      --  that would correspond seems to be obsolete. Return
      --  appropriate string for error value.

      ------------------------
      -- Host_Error_Message --
      ------------------------

      function Host_Error_Message return String is
      begin
         case Error is
            when Constants.HOST_NOT_FOUND => return "Host not found";
            when Constants.TRY_AGAIN      => return "Try again";
            when Constants.NO_RECOVERY    => return "No recovery";
            when Constants.NO_DATA        => return "No address";
            when others                   => return "Unknown error";
         end case;
      end Host_Error_Message;

   --  Start of processing for Raise_Host_Error

   begin
      Ada.Exceptions.Raise_Exception (Host_Error'Identity, Host_Error_Message);
   end Raise_Host_Error;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error (Error : Integer) is
      use type C.Strings.chars_ptr;

      function Image (E : Integer) return String;

      -----------
      -- Image --
      -----------

      function Image (E : Integer) return String is
         Msg : String := E'Img & "] ";
      begin
         Msg (Msg'First) := '[';
         return Msg;
      end Image;

   --  Start of processing for Raise_Socket_Error

   begin
      Ada.Exceptions.Raise_Exception
        (Socket_Error'Identity,
         Image (Error) & C.Strings.Value (Socket_Error_Message (Error)));
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

         Last  := Index;

         --  Exit when all or zero data received. Zero means that
         --  the socket peer is closed.

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

         --  Exit when all or zero data received. Zero means that
         --  the socket peer is closed.

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
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;

   begin
      Res := C_Recv
        (C.int (Socket),
         Item (Item'First)'Address,
         Item'Length,
         To_Int (Flags));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
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
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Res :=
        C_Recvfrom
          (C.int (Socket),
           Item (Item'First)'Address,
           Item'Length,
           To_Int (Flags),
           Sin'Unchecked_Access,
           Len'Unchecked_Access);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);

      From.Addr := To_Inet_Addr (Sin.Sin_Addr);
      From.Port := Port_Type (Network_To_Short (Sin.Sin_Port));
   end Receive_Socket;

   -------------------
   -- Resolve_Error --
   -------------------

   function Resolve_Error
     (Error_Value : Integer;
      From_Errno  : Boolean := True) return Error_Type
   is
      use GNAT.Sockets.Constants;

   begin
      if not From_Errno then
         case Error_Value is
            when Constants.HOST_NOT_FOUND => return Unknown_Host;
            when Constants.TRY_AGAIN      => return Host_Name_Lookup_Failure;
            when Constants.NO_RECOVERY    =>
               return Non_Recoverable_Error;
            when Constants.NO_DATA        => return Unknown_Server_Error;
            when others                   => return Cannot_Resolve_Error;
         end case;
      end if;

      case Error_Value is
         when ENOERROR        => return Success;
         when EACCES          => return Permission_Denied;
         when EADDRINUSE      => return Address_Already_In_Use;
         when EADDRNOTAVAIL   => return Cannot_Assign_Requested_Address;
         when EAFNOSUPPORT    =>
            return Address_Family_Not_Supported_By_Protocol;
         when EALREADY        => return Operation_Already_In_Progress;
         when EBADF           => return Bad_File_Descriptor;
         when ECONNABORTED    => return Software_Caused_Connection_Abort;
         when ECONNREFUSED    => return Connection_Refused;
         when ECONNRESET      => return Connection_Reset_By_Peer;
         when EDESTADDRREQ    => return Destination_Address_Required;
         when EFAULT          => return Bad_Address;
         when EHOSTDOWN       => return Host_Is_Down;
         when EHOSTUNREACH    => return No_Route_To_Host;
         when EINPROGRESS     => return Operation_Now_In_Progress;
         when EINTR           => return Interrupted_System_Call;
         when EINVAL          => return Invalid_Argument;
         when EIO             => return Input_Output_Error;
         when EISCONN         => return Transport_Endpoint_Already_Connected;
         when ELOOP           => return Too_Many_Symbolic_Links;
         when EMFILE          => return Too_Many_Open_Files;
         when EMSGSIZE        => return Message_Too_Long;
         when ENAMETOOLONG    => return File_Name_Too_Long;
         when ENETDOWN        => return Network_Is_Down;
         when ENETRESET       =>
            return Network_Dropped_Connection_Because_Of_Reset;
         when ENETUNREACH     => return Network_Is_Unreachable;
         when ENOBUFS         => return No_Buffer_Space_Available;
         when ENOPROTOOPT     => return Protocol_Not_Available;
         when ENOTCONN        => return Transport_Endpoint_Not_Connected;
         when ENOTSOCK        => return Socket_Operation_On_Non_Socket;
         when EOPNOTSUPP      => return Operation_Not_Supported;
         when EPFNOSUPPORT    => return Protocol_Family_Not_Supported;
         when EPROTONOSUPPORT => return Protocol_Not_Supported;
         when EPROTOTYPE      => return Protocol_Wrong_Type_For_Socket;
         when ESHUTDOWN       =>
            return Cannot_Send_After_Transport_Endpoint_Shutdown;
         when ESOCKTNOSUPPORT => return Socket_Type_Not_Supported;
         when ETIMEDOUT       => return Connection_Timed_Out;
         when ETOOMANYREFS    => return Too_Many_References;
         when EWOULDBLOCK     => return Resource_Temporarily_Unavailable;
         when others          => null;
      end case;

      return Cannot_Resolve_Error;
   end Resolve_Error;

   -----------------------
   -- Resolve_Exception --
   -----------------------

   function Resolve_Exception
     (Occurrence : Exception_Occurrence) return Error_Type
   is
      Id    : constant Exception_Id := Exception_Identity (Occurrence);
      Msg   : constant String       := Exception_Message (Occurrence);
      First : Natural               := Msg'First;
      Last  : Natural;
      Val   : Integer;

   begin
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

   --------------------
   -- Receive_Vector --
   --------------------

   procedure Receive_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count)
   is
      Res : C.int;

   begin
      Res :=
        C_Readv
          (C.int (Socket),
           Vector (Vector'First)'Address,
           Vector'Length);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Count := Ada.Streams.Stream_Element_Count (Res);
   end Receive_Vector;

   -----------------
   -- Send_Socket --
   -----------------

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Flags  : Request_Flag_Type := No_Request_Flag)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;

   begin
      Res :=
        C_Send
          (C.int (Socket),
           Item (Item'First)'Address,
           Item'Length,
           Set_Forced_Flags (To_Int (Flags)));

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
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
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Sin : aliased Sockaddr_In;
      Len : constant C.int := Sin'Size / 8;

   begin
      Set_Length (Sin'Unchecked_Access, Len);
      Set_Family (Sin'Unchecked_Access, Families (To.Family));
      Set_Address (Sin'Unchecked_Access, To_In_Addr (To.Addr));
      Set_Port
        (Sin'Unchecked_Access,
         Short_To_Network (C.unsigned_short (To.Port)));

      Res := C_Sendto
        (C.int (Socket),
         Item (Item'First)'Address,
         Item'Length,
         Set_Forced_Flags (To_Int (Flags)),
         Sin'Unchecked_Access,
         Len);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Last := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Send_Socket;

   -----------------
   -- Send_Vector --
   -----------------

   procedure Send_Vector
     (Socket : Socket_Type;
      Vector : Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count)
   is
      Res : C.int;

   begin
      Res :=
        C_Writev
          (C.int (Socket),
           Vector (Vector'First)'Address,
           Vector'Length);

      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;

      Count := Ada.Streams.Stream_Element_Count (Res);
   end Send_Vector;

   ---------
   -- Set --
   ---------

   procedure Set (Item : in out Socket_Set_Type; Socket : Socket_Type) is
   begin
      if Item.Set = No_Socket_Set then
         Item.Set  := New_Socket_Set (No_Socket_Set);
         Item.Last := Socket;

      elsif Item.Last < Socket then
         Item.Last := Socket;
      end if;

      Insert_Socket_In_Set (Item.Set, C.int (Socket));
   end Set;

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
      return To_int (To_unsigned (F) or Constants.MSG_Forced_Flags);
   end Set_Forced_Flags;

   -----------------------
   -- Set_Socket_Option --
   -----------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Level  : Level_Type := Socket_Level;
      Option : Option_Type)
   is
      V8  : aliased Two_Int;
      V4  : aliased C.int;
      V1  : aliased C.unsigned_char;
      Len : aliased C.int;
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

         when Multicast_TTL   =>
            V1  := C.unsigned_char (Option.Time_To_Live);
            Len := V1'Size / 8;
            Add := V1'Address;

         when Multicast_Loop  =>
            V1  := C.unsigned_char (Boolean'Pos (Option.Enabled));
            Len := V1'Size / 8;
            Add := V1'Address;

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
      --  Big-endian case. No conversion needed. On these platforms,
      --  htons() defaults to a null procedure.

      pragma Warnings (Off);
      --  Since the test can generate "always True/False" warning

      if Default_Bit_Order = High_Order_First then
         return S;

         pragma Warnings (On);

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

   ----------
   -- To_C --
   ----------

   function To_C (Socket : Socket_Type) return Integer is
   begin
      return Integer (Socket);
   end To_C;

   -------------------
   -- To_Host_Entry --
   -------------------

   function To_Host_Entry (E : Hostent) return Host_Entry_Type is
      use type C.size_t;

      Official : constant String :=
                  C.Strings.Value (E.H_Name);

      Aliases : constant Chars_Ptr_Array :=
                  Chars_Ptr_Pointers.Value (E.H_Aliases);
      --  H_Aliases points to a list of name aliases. The list is
      --  terminated by a NULL pointer.

      Addresses : constant In_Addr_Access_Array :=
                    In_Addr_Access_Pointers.Value (E.H_Addr_List);
      --  H_Addr_List points to a list of binary addresses (in network
      --  byte order). The list is terminated by a NULL pointer.
      --
      --  H_Length is not used because it is currently only set to 4.
      --  H_Addrtype is always AF_INET

      Result : Host_Entry_Type
                 (Aliases_Length   => Aliases'Length - 1,
                  Addresses_Length => Addresses'Length - 1);
      --  The last element is a null pointer

      Source : C.size_t;
      Target : Natural;

   begin
      Result.Official := To_Name (Official);

      Source := Aliases'First;
      Target := Result.Aliases'First;
      while Target <= Result.Aliases_Length loop
         Result.Aliases (Target) :=
           To_Name (C.Strings.Value (Aliases (Source)));
         Source := Source + 1;
         Target := Target + 1;
      end loop;

      Source := Addresses'First;
      Target := Result.Addresses'First;
      while Target <= Result.Addresses_Length loop
         Result.Addresses (Target) :=
           To_Inet_Addr (Addresses (Source).all);
         Source := Source + 1;
         Target := Target + 1;
      end loop;

      return Result;
   end To_Host_Entry;

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In_Addr (Addr : Inet_Addr_Type) return Thin.In_Addr is
   begin
      if Addr.Family = Family_Inet then
         return (S_B1 => C.unsigned_char (Addr.Sin_V4 (1)),
                 S_B2 => C.unsigned_char (Addr.Sin_V4 (2)),
                 S_B3 => C.unsigned_char (Addr.Sin_V4 (3)),
                 S_B4 => C.unsigned_char (Addr.Sin_V4 (4)));
      end if;

      raise Socket_Error;
   end To_In_Addr;

   ------------------
   -- To_Inet_Addr --
   ------------------

   function To_Inet_Addr
     (Addr : In_Addr) return Inet_Addr_Type
   is
      Result : Inet_Addr_Type;
   begin
      Result.Sin_V4 (1) := Inet_Addr_Comp_Type (Addr.S_B1);
      Result.Sin_V4 (2) := Inet_Addr_Comp_Type (Addr.S_B2);
      Result.Sin_V4 (3) := Inet_Addr_Comp_Type (Addr.S_B3);
      Result.Sin_V4 (4) := Inet_Addr_Comp_Type (Addr.S_B4);
      return Result;
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
               Raise_Socket_Error (Constants.EOPNOTSUPP);
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

   function To_Service_Entry (E : Servent) return Service_Entry_Type is
      use type C.size_t;

      Official : constant String :=
                  C.Strings.Value (E.S_Name);

      Aliases : constant Chars_Ptr_Array :=
                  Chars_Ptr_Pointers.Value (E.S_Aliases);
      --  S_Aliases points to a list of name aliases. The list is
      --  terminated by a NULL pointer.

      Protocol : constant String :=
                   C.Strings.Value (E.S_Proto);

      Result   : Service_Entry_Type
        (Aliases_Length   => Aliases'Length - 1);
      --  The last element is a null pointer

      Source : C.size_t;
      Target : Natural;

   begin
      Result.Official := To_Name (Official);

      Source := Aliases'First;
      Target := Result.Aliases'First;
      while Target <= Result.Aliases_Length loop
         Result.Aliases (Target) :=
           To_Name (C.Strings.Value (Aliases (Source)));
         Source := Source + 1;
         Target := Target + 1;
      end loop;

      Result.Port :=
        Port_Type (Network_To_Short (C.unsigned_short (E.S_Port)));

      Result.Protocol := To_Name (Protocol);

      return Result;
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

   function To_Timeval (Val : Selector_Duration) return Timeval is
      S  : Timeval_Unit;
      MS : Timeval_Unit;

   begin
      --  If zero, set result as zero (otherwise it gets rounded down to -1)

      if Val = 0.0 then
         S  := 0;
         MS := 0;

      --  Normal case where we do round down

      else
         S  := Timeval_Unit (Val - 0.5);
         MS := Timeval_Unit (1_000_000 * (Val - Selector_Duration (S)));
      end if;

      return (S, MS);
   end To_Timeval;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Datagram_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      First : Ada.Streams.Stream_Element_Offset          := Item'First;
      Index : Ada.Streams.Stream_Element_Offset          := First - 1;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      loop
         Send_Socket
           (Stream.Socket,
            Item (First .. Max),
            Index,
            Stream.To);

         --  Exit when all or zero data sent. Zero means that the
         --  socket has been closed by peer.

         exit when Index < First or else Index = Max;

         First := Index + 1;
      end loop;

      if Index /= Max then
         raise Socket_Error;
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Stream_Socket_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      First : Ada.Streams.Stream_Element_Offset          := Item'First;
      Index : Ada.Streams.Stream_Element_Offset          := First - 1;
      Max   : constant Ada.Streams.Stream_Element_Offset := Item'Last;

   begin
      loop
         Send_Socket (Stream.Socket, Item (First .. Max), Index);

         --  Exit when all or zero data sent. Zero means that the
         --  socket has been closed by peer.

         exit when Index < First or else Index = Max;

         First := Index + 1;
      end loop;

      if Index /= Max then
         raise Socket_Error;
      end if;
   end Write;

end GNAT.Sockets;
