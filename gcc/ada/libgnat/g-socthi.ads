------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

--  This is the default version

with Interfaces.C;

with GNAT.OS_Lib;
with GNAT.Sockets.Thin_Common;

with System;
with System.CRTL;

package GNAT.Sockets.Thin is

   --  This package is intended for hosts implementing BSD sockets with a
   --  standard interface. It will be used as a default for all the platforms
   --  that do not have a specific version of this file.

   use Thin_Common;

   package C renames Interfaces.C;

   use type System.CRTL.ssize_t;

   function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;
   --  Returns last socket error number

   function Socket_Error_Message (Errno : Integer) return String;
   --  Returns the error message string for the error number Errno. If Errno is
   --  not known, returns "Unknown system error".

   function Host_Errno return Integer;
   pragma Import (C, Host_Errno, "__gnat_get_h_errno");
   --  Returns last host error number

   package Host_Error_Messages is

      function Host_Error_Message (H_Errno : Integer) return String;
      --  Returns the error message string for the host error number H_Errno.
      --  If H_Errno is not known, returns "Unknown system error".

   end Host_Error_Messages;

   --------------------------------
   -- Standard library functions --
   --------------------------------

   function C_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : not null access C.int) return C.int;

   function C_Bind
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int;

   function C_Close
     (Fd : C.int) return C.int;

   function C_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int;

   function C_Gethostname
     (Name    : System.Address;
      Namelen : C.int) return C.int;

   function C_Getpeername
     (S       : C.int;
      Name    : System.Address;
      Namelen : not null access C.int) return C.int;

   function C_Getsockname
     (S       : C.int;
      Name    : System.Address;
      Namelen : not null access C.int) return C.int;

   function C_Getsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : System.Address;
      Optlen  : not null access C.int) return C.int;

   function Socket_Ioctl
     (S   : C.int;
      Req : SOSC.IOCTL_Req_T;
      Arg : access C.int) return C.int;

   function C_Listen
     (S       : C.int;
      Backlog : C.int) return C.int;

   function C_Recv
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int) return C.int;

   function C_Recvfrom
     (S       : C.int;
      Msg     : System.Address;
      Len     : C.int;
      Flags   : C.int;
      From    : System.Address;
      Fromlen : not null access C.int) return C.int;

   function C_Recvmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t;

   function C_Select
     (Nfds      : C.int;
      Readfds   : access Fd_Set;
      Writefds  : access Fd_Set;
      Exceptfds : access Fd_Set;
      Timeout   : Timeval_Access) return C.int;

   function C_Sendmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t;

   function C_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : System.Address;
      Tolen : C.int) return C.int;

   function C_Setsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : System.Address;
      Optlen  : C.int) return C.int;

   function C_Shutdown
     (S   : C.int;
      How : C.int) return C.int;

   function C_Socket
     (Domain   : C.int;
      Typ      : C.int;
      Protocol : C.int) return C.int;

   function C_System
     (Command : System.Address) return C.int;

   -------------------------------------------------------
   -- Signalling file descriptors for selector abortion --
   -------------------------------------------------------

   package Signalling_Fds is

      function Create (Fds : not null access Fd_Pair) return C.int;
      pragma Convention (C, Create);
      --  Create a pair of connected descriptors suitable for use with C_Select
      --  (used for signalling in Selector objects).

      function Read (Rsig : C.int) return C.int;
      pragma Convention (C, Read);
      --  Read one byte of data from rsig, the read end of a pair of signalling
      --  fds created by Create_Signalling_Fds.

      function Write (Wsig : C.int) return C.int;
      pragma Convention (C, Write);
      --  Write one byte of data to wsig, the write end of a pair of signalling
      --  fds created by Create_Signalling_Fds.

      procedure Close (Sig : C.int);
      pragma Convention (C, Close);
      --  Close one end of a pair of signalling fds (ignoring any error)

   end Signalling_Fds;

   -------------------------------------------
   -- Nonreentrant network databases access --
   -------------------------------------------

   --  The following are used only on systems that have nonreentrant
   --  getXXXbyYYY functions, and do NOT have corresponding getXXXbyYYY_
   --  functions. Currently, LynxOS is the only such system.

   function Nonreentrant_Gethostbyname
     (Name : C.char_array) return Hostent_Access;

   function Nonreentrant_Gethostbyaddr
     (Addr      : System.Address;
      Addr_Len  : C.int;
      Addr_Type : C.int) return Hostent_Access;

   function Nonreentrant_Getservbyname
     (Name  : C.char_array;
      Proto : C.char_array) return Servent_Access;

   function Nonreentrant_Getservbyport
     (Port  : C.int;
      Proto : C.char_array) return Servent_Access;

   procedure Initialize;
   procedure Finalize;

private
   pragma Import (C, C_Bind, "bind");
   pragma Import (C, C_Close, "close");
   pragma Import (C, C_Gethostname, "gethostname");
   pragma Import (C, C_Getpeername, "getpeername");
   pragma Import (C, C_Getsockname, "getsockname");
   pragma Import (C, C_Getsockopt, "getsockopt");
   pragma Import (C, C_Listen, "listen");
   pragma Import (C, C_Select, "select");
   pragma Import (C, C_Setsockopt, "setsockopt");
   pragma Import (C, C_Shutdown, "shutdown");
   pragma Import (C, C_System, "system");

   pragma Import (C, Nonreentrant_Gethostbyname, "gethostbyname");
   pragma Import (C, Nonreentrant_Gethostbyaddr, "gethostbyaddr");
   pragma Import (C, Nonreentrant_Getservbyname, "getservbyname");
   pragma Import (C, Nonreentrant_Getservbyport, "getservbyport");

end GNAT.Sockets.Thin;
