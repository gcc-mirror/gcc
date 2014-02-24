------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
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

--  This package provides a target dependent thin interface to the sockets
--  layer for use by the GNAT.Sockets package (g-socket.ads). This package
--  should not be directly with'ed by an applications program.

--  This version is for NT

with Ada.Streams;             use Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body GNAT.Sockets.Thin is

   use type C.unsigned;
   use type C.int;

   WSAData_Dummy : array (1 .. 512) of C.int;

   WS_Version : constant := 16#0202#;
   --  Winsock 2.2

   Initialized : Boolean := False;

   function Standard_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int;
   pragma Import (Stdcall, Standard_Connect, "connect");

   function Standard_Select
     (Nfds      : C.int;
      Readfds   : access Fd_Set;
      Writefds  : access Fd_Set;
      Exceptfds : access Fd_Set;
      Timeout   : Timeval_Access) return C.int;
   pragma Import (Stdcall, Standard_Select, "select");

   type Error_Type is
     (N_EINTR,
      N_EBADF,
      N_EACCES,
      N_EFAULT,
      N_EINVAL,
      N_EMFILE,
      N_EWOULDBLOCK,
      N_EINPROGRESS,
      N_EALREADY,
      N_ENOTSOCK,
      N_EDESTADDRREQ,
      N_EMSGSIZE,
      N_EPROTOTYPE,
      N_ENOPROTOOPT,
      N_EPROTONOSUPPORT,
      N_ESOCKTNOSUPPORT,
      N_EOPNOTSUPP,
      N_EPFNOSUPPORT,
      N_EAFNOSUPPORT,
      N_EADDRINUSE,
      N_EADDRNOTAVAIL,
      N_ENETDOWN,
      N_ENETUNREACH,
      N_ENETRESET,
      N_ECONNABORTED,
      N_ECONNRESET,
      N_ENOBUFS,
      N_EISCONN,
      N_ENOTCONN,
      N_ESHUTDOWN,
      N_ETOOMANYREFS,
      N_ETIMEDOUT,
      N_ECONNREFUSED,
      N_ELOOP,
      N_ENAMETOOLONG,
      N_EHOSTDOWN,
      N_EHOSTUNREACH,
      N_WSASYSNOTREADY,
      N_WSAVERNOTSUPPORTED,
      N_WSANOTINITIALISED,
      N_WSAEDISCON,
      N_HOST_NOT_FOUND,
      N_TRY_AGAIN,
      N_NO_RECOVERY,
      N_NO_DATA,
      N_OTHERS);

   Error_Messages : constant array (Error_Type) of chars_ptr :=
     (N_EINTR =>
        New_String ("Interrupted system call"),
      N_EBADF =>
        New_String ("Bad file number"),
      N_EACCES =>
        New_String ("Permission denied"),
      N_EFAULT =>
        New_String ("Bad address"),
      N_EINVAL =>
        New_String ("Invalid argument"),
      N_EMFILE =>
        New_String ("Too many open files"),
      N_EWOULDBLOCK =>
        New_String ("Operation would block"),
      N_EINPROGRESS =>
        New_String ("Operation now in progress. This error is "
                    & "returned if any Windows Sockets API "
                    & "function is called while a blocking "
                    & "function is in progress"),
      N_EALREADY =>
        New_String ("Operation already in progress"),
      N_ENOTSOCK =>
        New_String ("Socket operation on nonsocket"),
      N_EDESTADDRREQ =>
        New_String ("Destination address required"),
      N_EMSGSIZE =>
        New_String ("Message too long"),
      N_EPROTOTYPE =>
        New_String ("Protocol wrong type for socket"),
      N_ENOPROTOOPT =>
        New_String ("Protocol not available"),
      N_EPROTONOSUPPORT =>
        New_String ("Protocol not supported"),
      N_ESOCKTNOSUPPORT =>
        New_String ("Socket type not supported"),
      N_EOPNOTSUPP =>
        New_String ("Operation not supported on socket"),
      N_EPFNOSUPPORT =>
        New_String ("Protocol family not supported"),
      N_EAFNOSUPPORT =>
        New_String ("Address family not supported by protocol family"),
      N_EADDRINUSE =>
        New_String ("Address already in use"),
      N_EADDRNOTAVAIL =>
        New_String ("Cannot assign requested address"),
      N_ENETDOWN =>
        New_String ("Network is down. This error may be "
                    & "reported at any time if the Windows "
                    & "Sockets implementation detects an "
                    & "underlying failure"),
      N_ENETUNREACH =>
        New_String ("Network is unreachable"),
      N_ENETRESET =>
        New_String ("Network dropped connection on reset"),
      N_ECONNABORTED =>
        New_String ("Software caused connection abort"),
      N_ECONNRESET =>
        New_String ("Connection reset by peer"),
      N_ENOBUFS =>
        New_String ("No buffer space available"),
      N_EISCONN  =>
        New_String ("Socket is already connected"),
      N_ENOTCONN =>
        New_String ("Socket is not connected"),
      N_ESHUTDOWN =>
        New_String ("Cannot send after socket shutdown"),
      N_ETOOMANYREFS =>
        New_String ("Too many references: cannot splice"),
      N_ETIMEDOUT =>
        New_String ("Connection timed out"),
      N_ECONNREFUSED =>
        New_String ("Connection refused"),
      N_ELOOP =>
        New_String ("Too many levels of symbolic links"),
      N_ENAMETOOLONG =>
        New_String ("File name too long"),
      N_EHOSTDOWN =>
        New_String ("Host is down"),
      N_EHOSTUNREACH =>
        New_String ("No route to host"),
      N_WSASYSNOTREADY =>
        New_String ("Returned by WSAStartup(), indicating that "
                    & "the network subsystem is unusable"),
      N_WSAVERNOTSUPPORTED =>
        New_String ("Returned by WSAStartup(), indicating that "
                    & "the Windows Sockets DLL cannot support "
                    & "this application"),
      N_WSANOTINITIALISED =>
        New_String ("Winsock not initialized. This message is "
                    & "returned by any function except WSAStartup(), "
                    & "indicating that a successful WSAStartup() has "
                    & "not yet been performed"),
      N_WSAEDISCON =>
        New_String ("Disconnected"),
      N_HOST_NOT_FOUND =>
        New_String ("Host not found. This message indicates "
                    & "that the key (name, address, and so on) was not found"),
      N_TRY_AGAIN =>
        New_String ("Nonauthoritative host not found. This error may "
                    & "suggest that the name service itself is not "
                    & "functioning"),
      N_NO_RECOVERY =>
        New_String ("Nonrecoverable error. This error may suggest that the "
                    & "name service itself is not functioning"),
      N_NO_DATA =>
        New_String ("Valid name, no data record of requested type. "
                    & "This error indicates that the key (name, address, "
                    & "and so on) was not found."),
      N_OTHERS =>
        New_String ("Unknown system error"));

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int
   is
      Res : C.int;

   begin
      Res := Standard_Connect (S, Name, Namelen);

      if Res = -1 then
         if Socket_Errno = SOSC.EWOULDBLOCK then
            Set_Socket_Errno (SOSC.EINPROGRESS);
         end if;
      end if;

      return Res;
   end C_Connect;

   ------------------
   -- Socket_Ioctl --
   ------------------

   function Socket_Ioctl
     (S   : C.int;
      Req : SOSC.IOCTL_Req_T;
      Arg : access C.int) return C.int
   is
   begin
      return C_Ioctl (S, Req, Arg);
   end Socket_Ioctl;

   ---------------
   -- C_Recvmsg --
   ---------------

   function C_Recvmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t
   is
      use type C.size_t;

      Fill  : constant Boolean :=
                SOSC.MSG_WAITALL /= -1
                  and then (C.unsigned (Flags) and SOSC.MSG_WAITALL) /= 0;
      --  Is the MSG_WAITALL flag set? If so we need to fully fill all vectors

      Res   : C.int;
      Count : C.int := 0;

      MH : Msghdr;
      for MH'Address use Msg;

      Iovec : array (0 .. MH.Msg_Iovlen - 1) of Vector_Element;
      for Iovec'Address use MH.Msg_Iov;
      pragma Import (Ada, Iovec);

      Iov_Index     : Integer;
      Current_Iovec : Vector_Element;

      function To_Access is new Ada.Unchecked_Conversion
                                  (System.Address, Stream_Element_Reference);
      pragma Warnings (Off, Stream_Element_Reference);

      Req : Request_Type (Name => N_Bytes_To_Read);

   begin
      --  Windows does not provide an implementation of recvmsg(). The spec for
      --  WSARecvMsg() is incompatible with the data types we define, and is
      --  available starting with Windows Vista and Server 2008 only. So,
      --  we use C_Recv instead.

      --  Check how much data are available

      Control_Socket (Socket_Type (S), Req);

      --  Fill the vectors

      Iov_Index := -1;
      Current_Iovec := (Base => null, Length => 0);

      loop
         if Current_Iovec.Length = 0 then
            Iov_Index := Iov_Index + 1;
            exit when Iov_Index > Integer (Iovec'Last);
            Current_Iovec := Iovec (SOSC.Msg_Iovlen_T (Iov_Index));
         end if;

         Res :=
           C_Recv
            (S,
             Current_Iovec.Base.all'Address,
             C.int (Current_Iovec.Length),
             Flags);

         if Res < 0 then
            return System.CRTL.ssize_t (Res);

         elsif Res = 0 and then not Fill then
            exit;

         else
            pragma Assert (Stream_Element_Count (Res) <= Current_Iovec.Length);

            Count := Count + Res;
            Current_Iovec.Length :=
              Current_Iovec.Length - Stream_Element_Count (Res);
            Current_Iovec.Base :=
              To_Access (Current_Iovec.Base.all'Address
                + Storage_Offset (Res));

            --  If all the data that was initially available read, do not
            --  attempt to receive more, since this might block, or merge data
            --  from successive datagrams for a datagram-oriented socket. We
            --  still try to receive more if we need to fill all vectors
            --  (MSG_WAITALL flag is set).

            exit when Natural (Count) >= Req.Size
              and then

                --  Either we are not in fill mode

                (not Fill

                  --  Or else last vector filled

                  or else (Interfaces.C.size_t (Iov_Index) = Iovec'Last
                            and then Current_Iovec.Length = 0));
         end if;
      end loop;

      return System.CRTL.ssize_t (Count);
   end C_Recvmsg;

   --------------
   -- C_Select --
   --------------

   function C_Select
     (Nfds      : C.int;
      Readfds   : access Fd_Set;
      Writefds  : access Fd_Set;
      Exceptfds : access Fd_Set;
      Timeout   : Timeval_Access) return C.int
   is
      pragma Warnings (Off, Exceptfds);

      Original_WFS : aliased constant Fd_Set := Writefds.all;

      Res  : C.int;
      S    : aliased C.int;
      Last : aliased C.int;

   begin
      --  Asynchronous connection failures are notified in the exception fd
      --  set instead of the write fd set. To ensure POSIX compatibility, copy
      --  write fd set into exception fd set. Once select() returns, check any
      --  socket present in the exception fd set and peek at incoming
      --  out-of-band data. If the test is not successful, and the socket is
      --  present in the initial write fd set, then move the socket from the
      --  exception fd set to the write fd set.

      if Writefds /= No_Fd_Set_Access then

         --  Add any socket present in write fd set into exception fd set

         declare
            WFS : aliased Fd_Set := Writefds.all;
         begin
            Last := Nfds - 1;
            loop
               Get_Socket_From_Set
                 (WFS'Access, S'Unchecked_Access, Last'Unchecked_Access);
               exit when S = -1;
               Insert_Socket_In_Set (Exceptfds, S);
            end loop;
         end;
      end if;

      Res := Standard_Select (Nfds, Readfds, Writefds, Exceptfds, Timeout);

      if Exceptfds /= No_Fd_Set_Access then
         declare
            EFSC    : aliased Fd_Set := Exceptfds.all;
            Flag    : constant C.int := SOSC.MSG_PEEK + SOSC.MSG_OOB;
            Buffer  : Character;
            Length  : C.int;
            Fromlen : aliased C.int;

         begin
            Last := Nfds - 1;
            loop
               Get_Socket_From_Set
                 (EFSC'Access, S'Unchecked_Access, Last'Unchecked_Access);

               --  No more sockets in EFSC

               exit when S = -1;

               --  Check out-of-band data

               Length :=
                 C_Recvfrom
                  (S, Buffer'Address, 1, Flag,
                   From    => System.Null_Address,
                   Fromlen => Fromlen'Unchecked_Access);
               --  Is Fromlen necessary if From is Null_Address???

               --  If the signal is not an out-of-band data, then it
               --  is a connection failure notification.

               if Length = -1 then
                  Remove_Socket_From_Set (Exceptfds, S);

                  --  If S is present in the initial write fd set, move it from
                  --  exception fd set back to write fd set. Otherwise, ignore
                  --  this event since the user is not watching for it.

                  if Writefds /= No_Fd_Set_Access
                    and then (Is_Socket_In_Set (Original_WFS'Access, S) /= 0)
                  then
                     Insert_Socket_In_Set (Writefds, S);
                  end if;
               end if;
            end loop;
         end;
      end if;
      return Res;
   end C_Select;

   ---------------
   -- C_Sendmsg --
   ---------------

   function C_Sendmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t
   is
      use type C.size_t;

      Res   : C.int;
      Count : C.int := 0;

      MH : Msghdr;
      for MH'Address use Msg;

      Iovec : array (0 .. MH.Msg_Iovlen - 1) of Vector_Element;
      for Iovec'Address use MH.Msg_Iov;
      pragma Import (Ada, Iovec);

   begin
      --  Windows does not provide an implementation of sendmsg(). The spec for
      --  WSASendMsg() is incompatible with the data types we define, and is
      --  available starting with Windows Vista and Server 2008 only. So
      --  use C_Sendto instead.

      for J in Iovec'Range loop
         Res :=
           C_Sendto
            (S,
             Iovec (J).Base.all'Address,
             C.int (Iovec (J).Length),
             Flags => Flags,
             To    => MH.Msg_Name,
             Tolen => C.int (MH.Msg_Namelen));

         if Res < 0 then
            return System.CRTL.ssize_t (Res);
         else
            Count := Count + Res;
         end if;

         --  Exit now if the buffer is not fully transmitted

         exit when Stream_Element_Count (Res) < Iovec (J).Length;
      end loop;

      return System.CRTL.ssize_t (Count);
   end C_Sendmsg;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Initialized then
         WSACleanup;
         Initialized := False;
      end if;
   end Finalize;

   -------------------------
   -- Host_Error_Messages --
   -------------------------

   package body Host_Error_Messages is

      --  On Windows, socket and host errors share the same code space, and
      --  error messages are provided by Socket_Error_Message, so the default
      --  separate body for Host_Error_Messages is not used in this case.

      function Host_Error_Message (H_Errno : Integer) return String
         renames Socket_Error_Message;

   end Host_Error_Messages;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Return_Value : Interfaces.C.int;
   begin
      if not Initialized then
         Return_Value := WSAStartup (WS_Version, WSAData_Dummy'Address);
         pragma Assert (Return_Value = 0);
         Initialized := True;
      end if;
   end Initialize;

   --------------------
   -- Signalling_Fds --
   --------------------

   package body Signalling_Fds is separate;

   --------------------------
   -- Socket_Error_Message --
   --------------------------

   function Socket_Error_Message (Errno : Integer) return String is
      use GNAT.Sockets.SOSC;

      Errm : C.Strings.chars_ptr;

   begin
      case Errno is
         when EINTR =>           Errm := Error_Messages (N_EINTR);
         when EBADF =>           Errm := Error_Messages (N_EBADF);
         when EACCES =>          Errm := Error_Messages (N_EACCES);
         when EFAULT =>          Errm := Error_Messages (N_EFAULT);
         when EINVAL =>          Errm := Error_Messages (N_EINVAL);
         when EMFILE =>          Errm := Error_Messages (N_EMFILE);
         when EWOULDBLOCK =>     Errm := Error_Messages (N_EWOULDBLOCK);
         when EINPROGRESS =>     Errm := Error_Messages (N_EINPROGRESS);
         when EALREADY =>        Errm := Error_Messages (N_EALREADY);
         when ENOTSOCK =>        Errm := Error_Messages (N_ENOTSOCK);
         when EDESTADDRREQ =>    Errm := Error_Messages (N_EDESTADDRREQ);
         when EMSGSIZE =>        Errm := Error_Messages (N_EMSGSIZE);
         when EPROTOTYPE =>      Errm := Error_Messages (N_EPROTOTYPE);
         when ENOPROTOOPT =>     Errm := Error_Messages (N_ENOPROTOOPT);
         when EPROTONOSUPPORT => Errm := Error_Messages (N_EPROTONOSUPPORT);
         when ESOCKTNOSUPPORT => Errm := Error_Messages (N_ESOCKTNOSUPPORT);
         when EOPNOTSUPP =>      Errm := Error_Messages (N_EOPNOTSUPP);
         when EPFNOSUPPORT =>    Errm := Error_Messages (N_EPFNOSUPPORT);
         when EAFNOSUPPORT =>    Errm := Error_Messages (N_EAFNOSUPPORT);
         when EADDRINUSE =>      Errm := Error_Messages (N_EADDRINUSE);
         when EADDRNOTAVAIL =>   Errm := Error_Messages (N_EADDRNOTAVAIL);
         when ENETDOWN =>        Errm := Error_Messages (N_ENETDOWN);
         when ENETUNREACH =>     Errm := Error_Messages (N_ENETUNREACH);
         when ENETRESET =>       Errm := Error_Messages (N_ENETRESET);
         when ECONNABORTED =>    Errm := Error_Messages (N_ECONNABORTED);
         when ECONNRESET =>      Errm := Error_Messages (N_ECONNRESET);
         when ENOBUFS =>         Errm := Error_Messages (N_ENOBUFS);
         when EISCONN =>         Errm := Error_Messages (N_EISCONN);
         when ENOTCONN =>        Errm := Error_Messages (N_ENOTCONN);
         when ESHUTDOWN =>       Errm := Error_Messages (N_ESHUTDOWN);
         when ETOOMANYREFS =>    Errm := Error_Messages (N_ETOOMANYREFS);
         when ETIMEDOUT =>       Errm := Error_Messages (N_ETIMEDOUT);
         when ECONNREFUSED =>    Errm := Error_Messages (N_ECONNREFUSED);
         when ELOOP =>           Errm := Error_Messages (N_ELOOP);
         when ENAMETOOLONG =>    Errm := Error_Messages (N_ENAMETOOLONG);
         when EHOSTDOWN =>       Errm := Error_Messages (N_EHOSTDOWN);
         when EHOSTUNREACH =>    Errm := Error_Messages (N_EHOSTUNREACH);

         --  Windows-specific error codes

         when WSASYSNOTREADY =>  Errm := Error_Messages (N_WSASYSNOTREADY);
         when WSAVERNOTSUPPORTED =>
            Errm := Error_Messages (N_WSAVERNOTSUPPORTED);
         when WSANOTINITIALISED =>
            Errm := Error_Messages (N_WSANOTINITIALISED);
         when WSAEDISCON =>
            Errm := Error_Messages (N_WSAEDISCON);

         --  h_errno values

         when HOST_NOT_FOUND =>  Errm := Error_Messages (N_HOST_NOT_FOUND);
         when TRY_AGAIN =>       Errm := Error_Messages (N_TRY_AGAIN);
         when NO_RECOVERY =>     Errm := Error_Messages (N_NO_RECOVERY);
         when NO_DATA =>         Errm := Error_Messages (N_NO_DATA);

         when others =>          Errm := Error_Messages (N_OTHERS);
      end case;

      return Value (Errm);
   end Socket_Error_Message;

end GNAT.Sockets.Thin;
