------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G N A T . S O C K E T S . C O N S T A N T S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
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

--  This is the version for GNU/Linux

package GNAT.Sockets.Constants is

   --  Families

   AF_INET              : constant :=                2;
   AF_INET6             : constant :=               10;

   --  Modes

   SOCK_STREAM          : constant :=                1;
   SOCK_DGRAM           : constant :=                2;

   --  Socket Errors

   EBADF                : constant :=                9;
   ENOTSOCK             : constant :=               88;
   ENOTCONN             : constant :=              107;
   ENOBUFS              : constant :=              105;
   EOPNOTSUPP           : constant :=               95;
   EFAULT               : constant :=               14;
   EWOULDBLOCK          : constant :=               11;
   EADDRNOTAVAIL        : constant :=               99;
   EMSGSIZE             : constant :=               90;
   EADDRINUSE           : constant :=               98;
   EINVAL               : constant :=               22;
   EACCES               : constant :=               13;
   EAFNOSUPPORT         : constant :=               97;
   EISCONN              : constant :=              106;
   ETIMEDOUT            : constant :=              110;
   ECONNREFUSED         : constant :=              111;
   ENETUNREACH          : constant :=              101;
   EALREADY             : constant :=              114;
   EINPROGRESS          : constant :=              115;
   ENOPROTOOPT          : constant :=               92;
   EPROTONOSUPPORT      : constant :=               93;
   EINTR                : constant :=                4;
   EIO                  : constant :=                5;
   ESOCKTNOSUPPORT      : constant :=               94;

   --  Host Errors

   HOST_NOT_FOUND       : constant :=                1;
   TRY_AGAIN            : constant :=                2;
   NO_ADDRESS           : constant :=                4;
   NO_RECOVERY          : constant :=                3;

   --  Control Flags

   FIONBIO              : constant :=            21537;
   FIONREAD             : constant :=            21531;

   --  Shutdown Modes

   SHUT_RD              : constant :=                0;
   SHUT_WR              : constant :=                1;
   SHUT_RDWR            : constant :=                2;

   --  Protocol Levels

   SOL_SOCKET           : constant :=                1;
   IPPROTO_IP           : constant :=                0;
   IPPROTO_UDP          : constant :=               17;
   IPPROTO_TCP          : constant :=                6;

   --  Socket Options

   TCP_NODELAY          : constant :=                1;
   SO_SNDBUF            : constant :=                7;
   SO_RCVBUF            : constant :=                8;
   SO_REUSEADDR         : constant :=                2;
   SO_KEEPALIVE         : constant :=                9;
   SO_LINGER            : constant :=               13;
   SO_ERROR             : constant :=                4;
   SO_BROADCAST         : constant :=                6;
   IP_ADD_MEMBERSHIP    : constant :=               35;
   IP_DROP_MEMBERSHIP   : constant :=               36;
   IP_MULTICAST_TTL     : constant :=               33;
   IP_MULTICAST_LOOP    : constant :=               34;
end GNAT.Sockets.Constants;
