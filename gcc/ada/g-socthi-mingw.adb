------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2005 AdaCore                      --
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

--  This package provides a target dependent thin interface to the sockets
--  layer for use by the GNAT.Sockets package (g-socket.ads). This package
--  should not be directly with'ed by an applications program.

--  This version is for NT.

with GNAT.Sockets.Constants; use GNAT.Sockets.Constants;
with Interfaces.C.Strings;   use Interfaces.C.Strings;

with System; use System;

package body GNAT.Sockets.Thin is

   use type C.unsigned;

   WSAData_Dummy : array (1 .. 512) of C.int;

   WS_Version  : constant := 16#0101#;
   Initialized : Boolean := False;

   SYSNOTREADY          : constant := 10091;
   VERNOTSUPPORTED      : constant := 10092;
   NOTINITIALISED       : constant := 10093;
   EDISCON              : constant := 10101;

   function Standard_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int)
      return    C.int;
   pragma Import (Stdcall, Standard_Connect, "connect");

   function Standard_Select
     (Nfds      : C.int;
      Readfds   : Fd_Set_Access;
      Writefds  : Fd_Set_Access;
      Exceptfds : Fd_Set_Access;
      Timeout   : Timeval_Access)
      return      C.int;
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
      N_SYSNOTREADY,
      N_VERNOTSUPPORTED,
      N_NOTINITIALISED,
      N_EDISCON,
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
      N_SYSNOTREADY =>
        New_String ("Returned by WSAStartup(), indicating that "
                    & "the network subsystem is unusable"),
      N_VERNOTSUPPORTED =>
        New_String ("Returned by WSAStartup(), indicating that "
                    & "the Windows Sockets DLL cannot support "
                    & "this application"),
      N_NOTINITIALISED =>
        New_String ("Winsock not initialized. This message is "
                    & "returned by any function except WSAStartup(), "
                    & "indicating that a successful WSAStartup() has "
                    & "not yet been performed"),
      N_EDISCON =>
        New_String ("Disconnect"),
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
      Namelen : C.int)
      return    C.int
   is
      Res : C.int;

   begin
      Res := Standard_Connect (S, Name, Namelen);

      if Res = -1 then
         if Socket_Errno = EWOULDBLOCK then
            Set_Socket_Errno (EINPROGRESS);
         end if;
      end if;

      return Res;
   end C_Connect;

   -------------
   -- C_Readv --
   -------------

   function C_Readv
     (Socket : C.int;
      Iov    : System.Address;
      Iovcnt : C.int)
      return  C.int
   is
      Res : C.int;
      Count : C.int := 0;

      Iovec : array (0 .. Iovcnt - 1) of Vector_Element;
      for Iovec'Address use Iov;
      pragma Import (Ada, Iovec);

   begin
      for J in Iovec'Range loop
         Res := C_Recv
           (Socket,
            Iovec (J).Base.all'Address,
            C.int (Iovec (J).Length),
            0);

         if Res < 0 then
            return Res;
         else
            Count := Count + Res;
         end if;
      end loop;
      return Count;
   end C_Readv;

   --------------
   -- C_Select --
   --------------

   function C_Select
     (Nfds      : C.int;
      Readfds   : Fd_Set_Access;
      Writefds  : Fd_Set_Access;
      Exceptfds : Fd_Set_Access;
      Timeout   : Timeval_Access)
      return      C.int
   is
      pragma Warnings (Off, Exceptfds);

      RFS  : constant Fd_Set_Access := Readfds;
      WFS  : constant Fd_Set_Access := Writefds;
      WFSC : Fd_Set_Access := No_Fd_Set;
      EFS  : Fd_Set_Access := Exceptfds;
      Res  : C.int;
      S    : aliased C.int;
      Last : aliased C.int;

   begin
      --  Asynchronous connection failures are notified in the
      --  exception fd set instead of the write fd set. To ensure
      --  POSIX compatitibility, copy write fd set into exception fd
      --  set. Once select() returns, check any socket present in the
      --  exception fd set and peek at incoming out-of-band data. If
      --  the test is not successful, and the socket is present in
      --  the initial write fd set, then move the socket from the
      --  exception fd set to the write fd set.

      if WFS /= No_Fd_Set then
         --  Add any socket present in write fd set into exception fd set

         if EFS = No_Fd_Set then
            EFS := New_Socket_Set (WFS);

         else
            WFSC := New_Socket_Set (WFS);

            Last := Nfds - 1;
            loop
               Get_Socket_From_Set
                 (WFSC, S'Unchecked_Access, Last'Unchecked_Access);
               exit when S = -1;
               Insert_Socket_In_Set (EFS, S);
            end loop;

            Free_Socket_Set (WFSC);
         end if;

         --  Keep a copy of write fd set

         WFSC := New_Socket_Set (WFS);
      end if;

      Res := Standard_Select (Nfds, RFS, WFS, EFS, Timeout);

      if EFS /= No_Fd_Set then
         declare
            EFSC    : constant Fd_Set_Access := New_Socket_Set (EFS);
            Flag    : constant C.int := MSG_PEEK + MSG_OOB;
            Buffer  : Character;
            Length  : C.int;
            Fromlen : aliased C.int;

         begin
            Last := Nfds - 1;
            loop
               Get_Socket_From_Set
                 (EFSC, S'Unchecked_Access, Last'Unchecked_Access);

               --  No more sockets in EFSC

               exit when S = -1;

               --  Check out-of-band data

               Length := C_Recvfrom
                 (S, Buffer'Address, 1, Flag,
                  null, Fromlen'Unchecked_Access);

               --  If the signal is not an out-of-band data, then it
               --  is a connection failure notification.

               if Length = -1 then
                  Remove_Socket_From_Set (EFS, S);

                  --  If S is present in the initial write fd set,
                  --  move it from exception fd set back to write fd
                  --  set. Otherwise, ignore this event since the user
                  --  is not watching for it.

                  if WFSC /= No_Fd_Set
                    and then (Is_Socket_In_Set (WFSC, S) /= 0)
                  then
                     Insert_Socket_In_Set (WFS, S);
                  end if;
               end if;
            end loop;

            Free_Socket_Set (EFSC);
         end;

         if Exceptfds = No_Fd_Set then
            Free_Socket_Set (EFS);
         end if;
      end if;

      --  Free any copy of write fd set

      if WFSC /= No_Fd_Set then
         Free_Socket_Set (WFSC);
      end if;

      return Res;
   end C_Select;

   -----------------
   -- C_Inet_Addr --
   -----------------

   function C_Inet_Addr
     (Cp : C.Strings.chars_ptr) return C.int
   is
      use type C.unsigned_long;

      function Internal_Inet_Addr
        (Cp : C.Strings.chars_ptr) return C.unsigned_long;
      pragma Import (Stdcall, Internal_Inet_Addr, "inet_addr");

      Res : C.unsigned_long;
   begin
      Res := Internal_Inet_Addr (Cp);

      if Res = C.unsigned_long'Last then
         --  This value is returned in case of error
         return -1;
      else
         return C.int (Internal_Inet_Addr (Cp));
      end if;
   end C_Inet_Addr;

   --------------
   -- C_Writev --
   --------------

   function C_Writev
     (Socket : C.int;
      Iov    : System.Address;
      Iovcnt : C.int)
      return   C.int
   is
      Res : C.int;
      Count : C.int := 0;

      Iovec : array (0 .. Iovcnt - 1) of Vector_Element;
      for Iovec'Address use Iov;
      pragma Import (Ada, Iovec);

   begin
      for J in Iovec'Range loop
         Res := C_Send
           (Socket,
            Iovec (J).Base.all'Address,
            C.int (Iovec (J).Length),
            0);

         if Res < 0 then
            return Res;
         else
            Count := Count + Res;
         end if;
      end loop;
      return Count;
   end C_Writev;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Process_Blocking_IO : Boolean := False) is
      pragma Unreferenced (Process_Blocking_IO);

      Return_Value : Interfaces.C.int;

   begin
      if not Initialized then
         Return_Value := WSAStartup (WS_Version, WSAData_Dummy'Address);
         pragma Assert (Interfaces.C."=" (Return_Value, 0));
         Initialized := True;
      end if;
   end Initialize;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Sin     : Sockaddr_In_Access;
      Address : In_Addr)
   is
   begin
      Sin.Sin_Addr := Address;
   end Set_Address;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
     (Sin    : Sockaddr_In_Access;
      Family : C.int)
   is
   begin
      Sin.Sin_Family := C.unsigned_short (Family);
   end Set_Family;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Sin : Sockaddr_In_Access;
      Len : C.int)
   is
      pragma Unreferenced (Sin);
      pragma Unreferenced (Len);

   begin
      null;
   end Set_Length;

   --------------
   -- Set_Port --
   --------------

   procedure Set_Port
     (Sin  : Sockaddr_In_Access;
      Port : C.unsigned_short)
   is
   begin
      Sin.Sin_Port := Port;
   end Set_Port;

   --------------------------
   -- Socket_Error_Message --
   --------------------------

   function Socket_Error_Message
     (Errno : Integer)
     return  C.Strings.chars_ptr
   is
      use GNAT.Sockets.Constants;

   begin
      case Errno is
         when EINTR =>           return Error_Messages (N_EINTR);
         when EBADF =>           return Error_Messages (N_EBADF);
         when EACCES =>          return Error_Messages (N_EACCES);
         when EFAULT =>          return Error_Messages (N_EFAULT);
         when EINVAL =>          return Error_Messages (N_EINVAL);
         when EMFILE =>          return Error_Messages (N_EMFILE);
         when EWOULDBLOCK =>     return Error_Messages (N_EWOULDBLOCK);
         when EINPROGRESS =>     return Error_Messages (N_EINPROGRESS);
         when EALREADY =>        return Error_Messages (N_EALREADY);
         when ENOTSOCK =>        return Error_Messages (N_ENOTSOCK);
         when EDESTADDRREQ =>    return Error_Messages (N_EDESTADDRREQ);
         when EMSGSIZE =>        return Error_Messages (N_EMSGSIZE);
         when EPROTOTYPE =>      return Error_Messages (N_EPROTOTYPE);
         when ENOPROTOOPT =>     return Error_Messages (N_ENOPROTOOPT);
         when EPROTONOSUPPORT => return Error_Messages (N_EPROTONOSUPPORT);
         when ESOCKTNOSUPPORT => return Error_Messages (N_ESOCKTNOSUPPORT);
         when EOPNOTSUPP =>      return Error_Messages (N_EOPNOTSUPP);
         when EPFNOSUPPORT =>    return Error_Messages (N_EPFNOSUPPORT);
         when EAFNOSUPPORT =>    return Error_Messages (N_EAFNOSUPPORT);
         when EADDRINUSE =>      return Error_Messages (N_EADDRINUSE);
         when EADDRNOTAVAIL =>   return Error_Messages (N_EADDRNOTAVAIL);
         when ENETDOWN =>        return Error_Messages (N_ENETDOWN);
         when ENETUNREACH =>     return Error_Messages (N_ENETUNREACH);
         when ENETRESET =>       return Error_Messages (N_ENETRESET);
         when ECONNABORTED =>    return Error_Messages (N_ECONNABORTED);
         when ECONNRESET =>      return Error_Messages (N_ECONNRESET);
         when ENOBUFS =>         return Error_Messages (N_ENOBUFS);
         when EISCONN =>         return Error_Messages (N_EISCONN);
         when ENOTCONN =>        return Error_Messages (N_ENOTCONN);
         when ESHUTDOWN =>       return Error_Messages (N_ESHUTDOWN);
         when ETOOMANYREFS =>    return Error_Messages (N_ETOOMANYREFS);
         when ETIMEDOUT =>       return Error_Messages (N_ETIMEDOUT);
         when ECONNREFUSED =>    return Error_Messages (N_ECONNREFUSED);
         when ELOOP =>           return Error_Messages (N_ELOOP);
         when ENAMETOOLONG =>    return Error_Messages (N_ENAMETOOLONG);
         when EHOSTDOWN =>       return Error_Messages (N_EHOSTDOWN);
         when EHOSTUNREACH =>    return Error_Messages (N_EHOSTUNREACH);
         when SYSNOTREADY =>     return Error_Messages (N_SYSNOTREADY);
         when VERNOTSUPPORTED => return Error_Messages (N_VERNOTSUPPORTED);
         when NOTINITIALISED =>  return Error_Messages (N_NOTINITIALISED);
         when EDISCON =>         return Error_Messages (N_EDISCON);
         when HOST_NOT_FOUND =>  return Error_Messages (N_HOST_NOT_FOUND);
         when TRY_AGAIN =>       return Error_Messages (N_TRY_AGAIN);
         when NO_RECOVERY =>     return Error_Messages (N_NO_RECOVERY);
         when NO_DATA =>         return Error_Messages (N_NO_DATA);
         when others =>          return Error_Messages (N_OTHERS);
      end case;
   end Socket_Error_Message;

end GNAT.Sockets.Thin;
