------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2001-2003 Ada Core Technologies, Inc.         --
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

--  This package provides a target dependent thin interface to the sockets
--  layer for use by the GNAT.Sockets package (g-socket.ads). This package
--  should not be directly with'ed by an applications program.

--  This version is for NT.

with GNAT.Sockets.Constants; use GNAT.Sockets.Constants;

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

      RFS  : Fd_Set_Access := Readfds;
      WFS  : Fd_Set_Access := Writefds;
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
      --  the test is not successfull and if the socket is present in
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
            EFSC    : Fd_Set_Access := New_Socket_Set (EFS);
            Buffer  : Character;
            Length  : C.int;
            Flag    : C.int := MSG_PEEK + MSG_OOB;
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
                    and then Is_Socket_In_Set (WFSC, S)
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

   function Socket_Error_Message (Errno : Integer) return String is
      use GNAT.Sockets.Constants;

   begin
      case Errno is
         when EINTR =>
            return "Interrupted system call";

         when EBADF =>
            return "Bad file number";

         when EACCES =>
            return "Permission denied";

         when EFAULT =>
            return "Bad address";

         when EINVAL =>
            return "Invalid argument";

         when EMFILE =>
            return "Too many open files";

         when EWOULDBLOCK =>
            return "Operation would block";

         when EINPROGRESS =>
            return "Operation now in progress. This error is "
              & "returned if any Windows Sockets API "
              & "function is called while a blocking "
              & "function is in progress";

         when EALREADY =>
            return "Operation already in progress";

         when ENOTSOCK =>
            return "Socket operation on nonsocket";

         when EDESTADDRREQ =>
            return "Destination address required";

         when EMSGSIZE =>
            return "Message too long";

         when EPROTOTYPE =>
            return "Protocol wrong type for socket";

         when ENOPROTOOPT =>
            return "Protocol not available";

         when EPROTONOSUPPORT =>
            return "Protocol not supported";

         when ESOCKTNOSUPPORT =>
            return "Socket type not supported";

         when EOPNOTSUPP =>
            return "Operation not supported on socket";

         when EPFNOSUPPORT =>
            return "Protocol family not supported";

         when EAFNOSUPPORT =>
            return "Address family not supported by protocol family";

         when EADDRINUSE =>
            return "Address already in use";

         when EADDRNOTAVAIL =>
            return "Cannot assign requested address";

         when ENETDOWN =>
            return "Network is down. This error may be "
              & "reported at any time if the Windows "
              & "Sockets implementation detects an "
              & "underlying failure";

         when ENETUNREACH =>
            return "Network is unreachable";

         when ENETRESET =>
            return "Network dropped connection on reset";

         when ECONNABORTED =>
            return "Software caused connection abort";

         when ECONNRESET =>
            return "Connection reset by peer";

         when ENOBUFS =>
            return "No buffer space available";

         when EISCONN  =>
            return "Socket is already connected";

         when ENOTCONN =>
            return "Socket is not connected";

         when ESHUTDOWN =>
            return "Cannot send after socket shutdown";

         when ETOOMANYREFS =>
            return "Too many references: cannot splice";

         when ETIMEDOUT =>
            return "Connection timed out";

         when ECONNREFUSED =>
            return "Connection refused";

         when ELOOP =>
            return "Too many levels of symbolic links";

         when ENAMETOOLONG =>
            return "File name too long";

         when EHOSTDOWN =>
            return "Host is down";

         when EHOSTUNREACH =>
            return "No route to host";

         when SYSNOTREADY =>
            return "Returned by WSAStartup(), indicating that "
              & "the network subsystem is unusable";

         when VERNOTSUPPORTED =>
            return "Returned by WSAStartup(), indicating that "
              & "the Windows Sockets DLL cannot support this application";

         when NOTINITIALISED =>
            return "Winsock not initialized. This message is "
              & "returned by any function except WSAStartup(), "
              & "indicating that a successful WSAStartup() has "
              & "not yet been performed";

         when EDISCON =>
            return "Disconnect";

         when HOST_NOT_FOUND =>
            return "Host not found. This message indicates "
              & "that the key (name, address, and so on) was not found";

         when TRY_AGAIN =>
            return "Nonauthoritative host not found. This error may "
              & "suggest that the name service itself is not functioning";

         when NO_RECOVERY =>
            return "Nonrecoverable error. This error may suggest that the "
              & "name service itself is not functioning";

         when NO_DATA =>
            return "Valid name, no data record of requested type. "
              & "This error indicates that the key (name, address, "
              & "and so on) was not found.";

         when others =>
            return "Unknown system error";

      end case;
   end Socket_Error_Message;

end GNAT.Sockets.Thin;
