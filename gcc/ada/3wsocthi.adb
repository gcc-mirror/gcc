------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
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

--  This version is for NT.

package body GNAT.Sockets.Thin is

   use type C.unsigned;

   WSAData_Dummy : array (1 .. 512) of C.int;

   WS_Version  : constant := 16#0101#;
   Initialized : Boolean := False;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Item   : in out Fd_Set;
      Socket : C.int)
   is
   begin
      for J in 1 .. Item.fd_count loop
         if Item.fd_array (J) = Socket then
            Item.fd_array (J .. Item.fd_count - 1) :=
              Item.fd_array (J + 1 .. Item.fd_count);
            Item.fd_count := Item.fd_count - 1;
            exit;
         end if;
      end loop;
   end Clear;

   -----------
   -- Empty --
   -----------

   procedure Empty  (Item : in out Fd_Set) is
   begin
      Item := Null_Fd_Set;
   end Empty;

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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Fd_Set) return Boolean is
   begin
      return Item.fd_count = 0;
   end Is_Empty;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Item : Fd_Set; Socket : C.int) return Boolean is
   begin
      for J in 1 .. Item.fd_count loop
         if Item.fd_array (J) = Socket then
            return True;
         end if;
      end loop;

      return False;
   end Is_Set;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Process_Blocking_IO : Boolean := False) is
      Return_Value : Interfaces.C.int;

   begin
      if not Initialized then
         Return_Value := WSAStartup (WS_Version, WSAData_Dummy'Address);
         pragma Assert (Interfaces.C."=" (Return_Value, 0));
         Initialized := True;
      end if;
   end Initialize;

   ---------
   -- Max --
   ---------

   function Max (Item : Fd_Set) return C.int is
      L : C.int := 0;

   begin
      for J in 1 .. Item.fd_count loop
         if Item.fd_array (J) > L then
            L := Item.fd_array (J);
         end if;
      end loop;

      return L;
   end Max;

   ---------
   -- Set --
   ---------

   procedure Set (Item : in out Fd_Set; Socket : in C.int) is
   begin
      Item.fd_count := Item.fd_count + 1;
      Item.fd_array (Item.fd_count) := Socket;
   end Set;

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
