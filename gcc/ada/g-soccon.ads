------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G N A T . S O C K E T S . C O N S T A N T S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2005, Free Software Foundation, Inc.         --
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

--  This package provides target dependent definitions of constant for use
--  by the GNAT.Sockets package (g-socket.ads). This package should not be
--  directly with'ed by an applications program.

--  WARNING! This file is a default version that must be replaced for
--  each platform by running gen-soccon.c which automatically generates
--  the appropriate target specific values.

--  The values below were computed from a i686-pc-linux-gnu environment,
--  but are for illustration purposes only. As noted above, part of a port
--  to a new target is to replace this file appropriately.

package GNAT.Sockets.Constants is

   --------------
   -- Families --
   --------------

   AF_INET            : constant :=            2; --  IPv4 address family
   AF_INET6           : constant :=           10; --  IPv6 address family

   -----------
   -- Modes --
   -----------

   SOCK_STREAM        : constant :=            1; --  Stream socket
   SOCK_DGRAM         : constant :=            2; --  Datagram socket

   -------------------
   -- Socket errors --
   -------------------

   EACCES             : constant :=           13; --  Permission denied
   EADDRINUSE         : constant :=           98; --  Address already in use
   EADDRNOTAVAIL      : constant :=           99; --  Cannot assign address
   EAFNOSUPPORT       : constant :=           97; --  Addr family not supported
   EALREADY           : constant :=          114; --  Operation in progress
   EBADF              : constant :=            9; --  Bad file descriptor
   ECONNABORTED       : constant :=          103; --  Connection aborted
   ECONNREFUSED       : constant :=          111; --  Connection refused
   ECONNRESET         : constant :=          104; --  Connection reset by peer
   EDESTADDRREQ       : constant :=           89; --  Destination addr required
   EFAULT             : constant :=           14; --  Bad address
   EHOSTDOWN          : constant :=          112; --  Host is down
   EHOSTUNREACH       : constant :=          113; --  No route to host
   EINPROGRESS        : constant :=          115; --  Operation now in progress
   EINTR              : constant :=            4; --  Interrupted system call
   EINVAL             : constant :=           22; --  Invalid argument
   EIO                : constant :=            5; --  Input output error
   EISCONN            : constant :=          106; --  Socket already connected
   ELOOP              : constant :=           40; --  Too many symbolic lynks
   EMFILE             : constant :=           24; --  Too many open files
   EMSGSIZE           : constant :=           90; --  Message too long
   ENAMETOOLONG       : constant :=           36; --  Name too long
   ENETDOWN           : constant :=          100; --  Network is down
   ENETRESET          : constant :=          102; --  Disconn. on network reset
   ENETUNREACH        : constant :=          101; --  Network is unreachable
   ENOBUFS            : constant :=          105; --  No buffer space available
   ENOPROTOOPT        : constant :=           92; --  Protocol not available
   ENOTCONN           : constant :=          107; --  Socket not connected
   ENOTSOCK           : constant :=           88; --  Operation on non socket
   EOPNOTSUPP         : constant :=           95; --  Operation not supported
   EPFNOSUPPORT       : constant :=           96; --  Unknown protocol family
   EPROTONOSUPPORT    : constant :=           93; --  Unknown protocol
   EPROTOTYPE         : constant :=           91; --  Unknown protocol type
   ESHUTDOWN          : constant :=          108; --  Cannot send once shutdown
   ESOCKTNOSUPPORT    : constant :=           94; --  Socket type not supported
   ETIMEDOUT          : constant :=          110; --  Connection timed out
   ETOOMANYREFS       : constant :=          109; --  Too many references
   EWOULDBLOCK        : constant :=           11; --  Operation would block

   -----------------
   -- Host errors --
   -----------------

   HOST_NOT_FOUND     : constant :=            1; --  Unknown host
   TRY_AGAIN          : constant :=            2; --  Host name lookup failure
   NO_DATA            : constant :=            4; --  No data record for name
   NO_RECOVERY        : constant :=            3; --  Non recoverable errors

   -------------------
   -- Control flags --
   -------------------

   FIONBIO            : constant :=        21537; --  Set/clear non-blocking io
   FIONREAD           : constant :=        21531; --  How many bytes to read

   --------------------
   -- Shutdown modes --
   --------------------

   SHUT_RD            : constant :=            0; --  No more recv
   SHUT_WR            : constant :=            1; --  No more send
   SHUT_RDWR          : constant :=            2; --  No more recv/send

   ---------------------
   -- Protocol levels --
   ---------------------

   SOL_SOCKET         : constant :=            1; --  Options for socket level
   IPPROTO_IP         : constant :=            0; --  Dummy protocol for IP
   IPPROTO_UDP        : constant :=           17; --  UDP
   IPPROTO_TCP        : constant :=            6; --  TCP

   -------------------
   -- Request flags --
   -------------------

   MSG_OOB            : constant :=            1; --  Process out-of-band data
   MSG_PEEK           : constant :=            2; --  Peek at incoming data
   MSG_EOR            : constant :=          128; --  Send end of record
   MSG_WAITALL        : constant :=          256; --  Wait for full reception
   MSG_NOSIGNAL       : constant :=        16384; --  No SIGPIPE on send
   MSG_Forced_Flags   : constant := MSG_NOSIGNAL;
   --  Flags set on all send(2) calls

   --------------------
   -- Socket options --
   --------------------

   TCP_NODELAY        : constant :=            1; --  Do not coalesce packets
   SO_REUSEADDR       : constant :=            2; --  Bind reuse local address
   SO_KEEPALIVE       : constant :=            9; --  Enable keep-alive msgs
   SO_LINGER          : constant :=           13; --  Defer close to flush data
   SO_BROADCAST       : constant :=            6; --  Can send broadcast msgs
   SO_SNDBUF          : constant :=            7; --  Set/get send buffer size
   SO_RCVBUF          : constant :=            8; --  Set/get recv buffer size
   SO_SNDTIMEO        : constant :=           21; --  Emission timeout
   SO_RCVTIMEO        : constant :=           20; --  Reception timeout
   SO_ERROR           : constant :=            4; --  Get/clear error status
   IP_MULTICAST_IF    : constant :=           32; --  Set/get mcast interface
   IP_MULTICAST_TTL   : constant :=           33; --  Set/get multicast TTL
   IP_MULTICAST_LOOP  : constant :=           34; --  Set/get mcast loopback
   IP_ADD_MEMBERSHIP  : constant :=           35; --  Join a multicast group
   IP_DROP_MEMBERSHIP : constant :=           36; --  Leave a multicast group

   -------------------
   -- System limits --
   -------------------

   IOV_MAX            : constant :=   2147483647; --  Maximum writev iovcnt

   ----------------------
   -- Type definitions --
   ----------------------

   --  Sizes (in bytes) of the components of struct timeval

   SIZEOF_tv_sec      : constant :=            4; --  tv_sec
   SIZEOF_tv_usec     : constant :=            4; --  tv_usec

end GNAT.Sockets.Constants;
