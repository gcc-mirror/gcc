------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G N A T . S O C K E T S . C O N S T A N T S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.11 $
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

--  This is the version for MINGW32 NT

package GNAT.Sockets.Constants is

   --  Families

   AF_INET              : constant :=                2;
   AF_INET6             : constant :=                3;

   --  Modes

   SOCK_STREAM          : constant :=                1;
   SOCK_DGRAM           : constant :=                2;

   --  Socket Errors

   EINTR                : constant :=            10004;
   EBADF                : constant :=            10009;
   EACCES               : constant :=            10013;
   EFAULT               : constant :=            10014;
   EINVAL               : constant :=            10022;
   EMFILE               : constant :=            10024;
   EWOULDBLOCK          : constant :=            10035;
   EINPROGRESS          : constant :=            10036;
   EALREADY             : constant :=            10037;
   ENOTSOCK             : constant :=            10038;
   EDESTADDRREQ         : constant :=            10039;
   EMSGSIZE             : constant :=            10040;
   EPROTOTYPE           : constant :=            10041;
   ENOPROTOOPT          : constant :=            10042;
   EPROTONOSUPPORT      : constant :=            10043;
   ESOCKTNOSUPPORT      : constant :=            10044;
   EOPNOTSUPP           : constant :=            10045;
   EPFNOSUPPORT         : constant :=            10046;
   EAFNOSUPPORT         : constant :=            10047;
   EADDRINUSE           : constant :=            10048;
   EADDRNOTAVAIL        : constant :=            10049;
   ENETDOWN             : constant :=            10050;
   ENETUNREACH          : constant :=            10051;
   ENETRESET            : constant :=            10052;
   ECONNABORTED         : constant :=            10053;
   ECONNRESET           : constant :=            10054;
   ENOBUFS              : constant :=            10055;
   EISCONN              : constant :=            10056;
   ENOTCONN             : constant :=            10057;
   ESHUTDOWN            : constant :=            10058;
   ETOOMANYREFS         : constant :=            10059;
   ETIMEDOUT            : constant :=            10060;
   ECONNREFUSED         : constant :=            10061;
   ELOOP                : constant :=            10062;
   ENAMETOOLONG         : constant :=            10063;
   EHOSTDOWN            : constant :=            10064;
   EHOSTUNREACH         : constant :=            10065;
   SYSNOTREADY          : constant :=            10091;
   VERNOTSUPPORTED      : constant :=            10092;
   NOTINITIALISED       : constant :=            10093;
   EDISCON              : constant :=            10101;

   --  Host Errors

   HOST_NOT_FOUND       : constant :=            11001;
   TRY_AGAIN            : constant :=            11002;
   NO_RECOVERY          : constant :=            11003;
   NO_ADDRESS           : constant :=            11004;
   NO_DATA              : constant :=            11004;

   EIO                  : constant :=            10101;

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
