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

--  This is the version for ia64-hp-hpux11.23
--  This file is generated automatically, do not modify it by hand! Instead,
--  make changes to gen-soccon.c and re-run it on each target.

package GNAT.Sockets.Constants is

   --------------
   -- Families --
   --------------

   AF_INET            : constant :=           2; --  IPv4 address family
   AF_INET6           : constant :=          22; --  IPv6 address family

   -----------
   -- Modes --
   -----------

   SOCK_STREAM        : constant :=           1; --  Stream socket
   SOCK_DGRAM         : constant :=           2; --  Datagram socket

   -------------------
   -- Socket errors --
   -------------------

   EACCES             : constant :=          13; --  Permission denied
   EADDRINUSE         : constant :=         226; --  Address already in use
   EADDRNOTAVAIL      : constant :=         227; --  Cannot assign address
   EAFNOSUPPORT       : constant :=         225; --  Addr family not supported
   EALREADY           : constant :=         244; --  Operation in progress
   EBADF              : constant :=           9; --  Bad file descriptor
   ECONNABORTED       : constant :=         231; --  Connection aborted
   ECONNREFUSED       : constant :=         239; --  Connection refused
   ECONNRESET         : constant :=         232; --  Connection reset by peer
   EDESTADDRREQ       : constant :=         217; --  Destination addr required
   EFAULT             : constant :=          14; --  Bad address
   EHOSTDOWN          : constant :=         241; --  Host is down
   EHOSTUNREACH       : constant :=         242; --  No route to host
   EINPROGRESS        : constant :=         245; --  Operation now in progress
   EINTR              : constant :=           4; --  Interrupted system call
   EINVAL             : constant :=          22; --  Invalid argument
   EIO                : constant :=           5; --  Input output error
   EISCONN            : constant :=         234; --  Socket already connected
   ELOOP              : constant :=         249; --  Too many symbolic lynks
   EMFILE             : constant :=          24; --  Too many open files
   EMSGSIZE           : constant :=         218; --  Message too long
   ENAMETOOLONG       : constant :=         248; --  Name too long
   ENETDOWN           : constant :=         228; --  Network is down
   ENETRESET          : constant :=         230; --  Disconn. on network reset
   ENETUNREACH        : constant :=         229; --  Network is unreachable
   ENOBUFS            : constant :=         233; --  No buffer space available
   ENOPROTOOPT        : constant :=         220; --  Protocol not available
   ENOTCONN           : constant :=         235; --  Socket not connected
   ENOTSOCK           : constant :=         216; --  Operation on non socket
   EOPNOTSUPP         : constant :=         223; --  Operation not supported
   EPFNOSUPPORT       : constant :=         224; --  Unknown protocol family
   EPROTONOSUPPORT    : constant :=         221; --  Unknown protocol
   EPROTOTYPE         : constant :=         219; --  Unknown protocol type
   ESHUTDOWN          : constant :=         236; --  Cannot send once shutdown
   ESOCKTNOSUPPORT    : constant :=         222; --  Socket type not supported
   ETIMEDOUT          : constant :=         238; --  Connection timed out
   ETOOMANYREFS       : constant :=         237; --  Too many references
   EWOULDBLOCK        : constant :=         246; --  Operation would block

   -----------------
   -- Host errors --
   -----------------

   HOST_NOT_FOUND     : constant :=           1; --  Unknown host
   TRY_AGAIN          : constant :=           2; --  Host name lookup failure
   NO_DATA            : constant :=           4; --  No data record for name
   NO_RECOVERY        : constant :=           3; --  Non recoverable errors

   -------------------
   -- Control flags --
   -------------------

   FIONBIO            : constant := -2147195266; --  Set/clear non-blocking io
   FIONREAD           : constant :=  1074030207; --  How many bytes to read

   --------------------
   -- Shutdown modes --
   --------------------

   SHUT_RD            : constant :=           0; --  No more recv
   SHUT_WR            : constant :=           1; --  No more send
   SHUT_RDWR          : constant :=           2; --  No more recv/send

   ---------------------
   -- Protocol levels --
   ---------------------

   SOL_SOCKET         : constant :=       65535; --  Options for socket level
   IPPROTO_IP         : constant :=           0; --  Dummy protocol for IP
   IPPROTO_UDP        : constant :=          17; --  UDP
   IPPROTO_TCP        : constant :=           6; --  TCP

   -------------------
   -- Request flags --
   -------------------

   MSG_OOB            : constant :=           1; --  Process out-of-band data
   MSG_PEEK           : constant :=           2; --  Peek at incoming data
   MSG_EOR            : constant :=           8; --  Send end of record
   MSG_WAITALL        : constant :=          64; --  Wait for full reception
   MSG_NOSIGNAL       : constant :=          -1; --  No SIGPIPE on send
   MSG_Forced_Flags   : constant :=           0;
   --  Flags set on all send(2) calls

   --------------------
   -- Socket options --
   --------------------

   TCP_NODELAY        : constant :=           1; --  Do not coalesce packets
   SO_REUSEADDR       : constant :=           4; --  Bind reuse local address
   SO_KEEPALIVE       : constant :=           8; --  Enable keep-alive msgs
   SO_LINGER          : constant :=         128; --  Defer close to flush data
   SO_BROADCAST       : constant :=          32; --  Can send broadcast msgs
   SO_SNDBUF          : constant :=        4097; --  Set/get send buffer size
   SO_RCVBUF          : constant :=        4098; --  Set/get recv buffer size
   SO_SNDTIMEO        : constant :=        4101; --  Emission timeout
   SO_RCVTIMEO        : constant :=        4102; --  Reception timeout
   SO_ERROR           : constant :=        4103; --  Get/clear error status
   IP_MULTICAST_IF    : constant :=           2; --  Set/get mcast interface
   IP_MULTICAST_TTL   : constant :=           3; --  Set/get multicast TTL
   IP_MULTICAST_LOOP  : constant :=           4; --  Set/get mcast loopback
   IP_ADD_MEMBERSHIP  : constant :=           5; --  Join a multicast group
   IP_DROP_MEMBERSHIP : constant :=           6; --  Leave a multicast group

   -------------------
   -- System limits --
   -------------------

   IOV_MAX            : constant :=          16; --  Maximum writev iovcnt

   ----------------------
   -- Type definitions --
   ----------------------

   --  Sizes (in bytes) of the components of struct timeval

   SIZEOF_tv_sec      : constant :=           8; --  tv_sec
   SIZEOF_tv_usec     : constant :=           8; --  tv_usec

end GNAT.Sockets.Constants;
