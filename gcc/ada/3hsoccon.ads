------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G N A T . S O C K E T S . C O N S T A N T S                --
--                                                                          --
--                                 S p e c                                  --
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

--  This is the version for HP/UX

package GNAT.Sockets.Constants is

   --  Families

   AF_INET              : constant :=                2;
   AF_INET6             : constant :=               -1;

   --  Modes

   SOCK_STREAM          : constant :=                1;
   SOCK_DGRAM           : constant :=                2;

   --  Socket Errors

   EBADF                : constant :=                9;
   ENOTSOCK             : constant :=              216;
   ENOTCONN             : constant :=              235;
   ENOBUFS              : constant :=              233;
   EOPNOTSUPP           : constant :=              223;
   EFAULT               : constant :=               14;
   EWOULDBLOCK          : constant :=              246;
   EADDRNOTAVAIL        : constant :=              227;
   EMSGSIZE             : constant :=              218;
   EADDRINUSE           : constant :=              226;
   EINVAL               : constant :=               22;
   EACCES               : constant :=               13;
   EAFNOSUPPORT         : constant :=              225;
   EISCONN              : constant :=              234;
   ETIMEDOUT            : constant :=              238;
   ECONNREFUSED         : constant :=              239;
   ENETUNREACH          : constant :=              229;
   EALREADY             : constant :=              244;
   EINPROGRESS          : constant :=              245;
   ENOPROTOOPT          : constant :=              220;
   EPROTONOSUPPORT      : constant :=              221;
   EINTR                : constant :=                4;
   EIO                  : constant :=                5;
   ESOCKTNOSUPPORT      : constant :=              222;

   --  Host Errors

   HOST_NOT_FOUND       : constant :=                1;
   TRY_AGAIN            : constant :=                2;
   NO_ADDRESS           : constant :=                4;
   NO_RECOVERY          : constant :=                3;

   --  Control Flags

   FIONBIO              : constant :=      -2147195266;
   FIONREAD             : constant :=       1074030207;

   --  Shutdown Modes

   SHUT_RD              : constant :=                0;
   SHUT_WR              : constant :=                1;
   SHUT_RDWR            : constant :=                2;

   --  Protocol Levels

   SOL_SOCKET           : constant :=            65535;
   IPPROTO_IP           : constant :=                0;
   IPPROTO_UDP          : constant :=               17;
   IPPROTO_TCP          : constant :=                6;

   --  Socket Options

   TCP_NODELAY          : constant :=                1;
   SO_SNDBUF            : constant :=             4097;
   SO_RCVBUF            : constant :=             4098;
   SO_REUSEADDR         : constant :=                4;
   SO_KEEPALIVE         : constant :=                8;
   SO_LINGER            : constant :=              128;
   SO_ERROR             : constant :=             4103;
   SO_BROADCAST         : constant :=               32;
   IP_ADD_MEMBERSHIP    : constant :=                5;
   IP_DROP_MEMBERSHIP   : constant :=                6;
   IP_MULTICAST_TTL     : constant :=                3;
   IP_MULTICAST_LOOP    : constant :=                4;
end GNAT.Sockets.Constants;
