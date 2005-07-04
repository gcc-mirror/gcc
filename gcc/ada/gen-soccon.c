/****************************************************************************
 *                                                                          *
 *                          GNAT SYSTEM UTILITIES                           *
 *                                                                          *
 *                           G E N - S O C C O N                            *
 *                                                                          *
 *            Copyright (C) 2004-2005 Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This program generates g-soccon.ads */

#include <stdio.h>
#include <string.h>

#include "gsocket.h"

#ifdef __MINGW32__
#include <winsock2.h>
#else
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <netdb.h>
#endif

struct line {
  char *text;
  char *value;
  char *comment;
  struct line *next;
};

struct line *first = NULL, *last = NULL;

#define TXT(_text) add_line(_text, NULL, NULL);
/* Plain text */

#define _NL TXT("")
/* Empty line */

#define itoad(n) f_itoa ("%d", n)
#define itoax(n) f_itoa ("16#%08x#", n)

#define CND(name,comment) add_line(#name, itoad (name), comment);
/* Constant (decimal) */

#define CNX(name,comment) add_line(#name, itoax (name), comment);
/* Constant (hexadecimal) */

#define CN_(name,comment) add_line(#name, name, comment);
/* Constant (generic) */

void output (void);
/* Generate output spec */

char *f_itoa (char *, int);
/* int to string */

void add_line (char *, char*, char*);

int
main (void) {

TXT("------------------------------------------------------------------------------")
TXT("--                                                                          --")
TXT("--                         GNAT COMPILER COMPONENTS                         --")
TXT("--                                                                          --")
TXT("--               G N A T . S O C K E T S . C O N S T A N T S                --")
TXT("--                                                                          --")
TXT("--                                 S p e c                                  --")
TXT("--                                                                          --")
TXT("--          Copyright (C) 2000-2005 Free Software Foundation, Inc.          --")
TXT("--                                                                          --")
TXT("-- GNAT is free software;  you can  redistribute it  and/or modify it under --")
TXT("-- terms of the  GNU General Public License as published  by the Free Soft- --")
TXT("-- ware  Foundation;  either version 2,  or (at your option) any later ver- --")
TXT("-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --")
TXT("-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --")
TXT("-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --")
TXT("-- for  more details.  You should have  received  a copy of the GNU General --")
TXT("-- Public License  distributed with GNAT;  see file COPYING.  If not, write --")
TXT("-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --")
TXT("-- Boston, MA 02110-1301, USA.                                              --")
TXT("--                                                                          --")
TXT("-- As a special exception,  if other files  instantiate  generics from this --")
TXT("-- unit, or you link  this unit with other files  to produce an executable, --")
TXT("-- this  unit  does not  by itself cause  the resulting  executable  to  be --")
TXT("-- covered  by the  GNU  General  Public  License.  This exception does not --")
TXT("-- however invalidate  any other reasons why  the executable file  might be --")
TXT("-- covered by the  GNU Public License.                                      --")
TXT("--                                                                          --")
TXT("-- GNAT was originally developed  by the GNAT team at  New York University. --")
TXT("-- Extensive contributions were provided by Ada Core Technologies Inc.      --")
TXT("--                                                                          --")
TXT("------------------------------------------------------------------------------")
_NL
TXT("--  This package provides target dependent definitions of constant for use")
TXT("--  by the GNAT.Sockets package (g-socket.ads). This package should not be")
TXT("--  directly with'ed by an applications program.")
_NL
TXT("--  This is the version for " TARGET)
TXT("--  This file is generated automatically, do not modify it by hand! Instead,")
TXT("--  make changes to gen-soccon.c and re-run it on each target.")
_NL
TXT("package GNAT.Sockets.Constants is")
_NL
TXT("   --------------")
TXT("   -- Families --")
TXT("   --------------")
_NL

#ifndef AF_INET
#define AF_INET -1
#endif
CND(AF_INET, "IPv4 address family")

#ifndef AF_INET6
#define AF_INET6 -1
#endif
CND(AF_INET6, "IPv6 address family")
_NL
TXT("   -----------")
TXT("   -- Modes --")
TXT("   -----------")
_NL

#ifndef SOCK_STREAM
#define SOCK_STREAM -1
#endif
CND(SOCK_STREAM, "Stream socket")

#ifndef SOCK_DGRAM
#define SOCK_DGRAM -1
#endif
CND(SOCK_DGRAM, "Datagram socket")
_NL
TXT("   -------------------")
TXT("   -- Socket errors --")
TXT("   -------------------")
_NL

#ifndef EACCES
#define EACCES -1
#endif
CND(EACCES, "Permission denied")

#ifndef EADDRINUSE
#define EADDRINUSE -1
#endif
CND(EADDRINUSE, "Address already in use")

#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL -1
#endif
CND(EADDRNOTAVAIL, "Cannot assign address")

#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT -1
#endif
CND(EAFNOSUPPORT, "Addr family not supported")

#ifndef EALREADY
#define EALREADY -1
#endif
CND(EALREADY, "Operation in progress")

#ifndef EBADF
#define EBADF -1
#endif
CND(EBADF, "Bad file descriptor")

#ifndef ECONNABORTED
#define ECONNABORTED -1
#endif
CND(ECONNABORTED, "Connection aborted")

#ifndef ECONNREFUSED
#define ECONNREFUSED -1
#endif
CND(ECONNREFUSED, "Connection refused")

#ifndef ECONNRESET
#define ECONNRESET -1
#endif
CND(ECONNRESET, "Connection reset by peer")

#ifndef EDESTADDRREQ
#define EDESTADDRREQ -1
#endif
CND(EDESTADDRREQ, "Destination addr required")

#ifndef EFAULT
#define EFAULT -1
#endif
CND(EFAULT, "Bad address")

#ifndef EHOSTDOWN
#define EHOSTDOWN -1
#endif
CND(EHOSTDOWN, "Host is down")

#ifndef EHOSTUNREACH
#define EHOSTUNREACH -1
#endif
CND(EHOSTUNREACH, "No route to host")

#ifndef EINPROGRESS
#define EINPROGRESS -1
#endif
CND(EINPROGRESS, "Operation now in progress")

#ifndef EINTR
#define EINTR -1
#endif
CND(EINTR, "Interrupted system call")

#ifndef EINVAL
#define EINVAL -1
#endif
CND(EINVAL, "Invalid argument")

#ifndef EIO
#define EIO -1
#endif
CND(EIO, "Input output error")

#ifndef EISCONN
#define EISCONN -1
#endif
CND(EISCONN, "Socket already connected")

#ifndef ELOOP
#define ELOOP -1
#endif
CND(ELOOP, "Too many symbolic lynks")

#ifndef EMFILE
#define EMFILE -1
#endif
CND(EMFILE, "Too many open files")

#ifndef EMSGSIZE
#define EMSGSIZE -1
#endif
CND(EMSGSIZE, "Message too long")

#ifndef ENAMETOOLONG
#define ENAMETOOLONG -1
#endif
CND(ENAMETOOLONG, "Name too long")

#ifndef ENETDOWN
#define ENETDOWN -1
#endif
CND(ENETDOWN, "Network is down")

#ifndef ENETRESET
#define ENETRESET -1
#endif
CND(ENETRESET, "Disconn. on network reset")

#ifndef ENETUNREACH
#define ENETUNREACH -1
#endif
CND(ENETUNREACH, "Network is unreachable")

#ifndef ENOBUFS
#define ENOBUFS -1
#endif
CND(ENOBUFS, "No buffer space available")

#ifndef ENOPROTOOPT
#define ENOPROTOOPT -1
#endif
CND(ENOPROTOOPT, "Protocol not available")

#ifndef ENOTCONN
#define ENOTCONN -1
#endif
CND(ENOTCONN, "Socket not connected")

#ifndef ENOTSOCK
#define ENOTSOCK -1
#endif
CND(ENOTSOCK, "Operation on non socket")

#ifndef EOPNOTSUPP
#define EOPNOTSUPP -1
#endif
CND(EOPNOTSUPP, "Operation not supported")

#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT -1
#endif
CND(EPFNOSUPPORT, "Unknown protocol family")

#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT -1
#endif
CND(EPROTONOSUPPORT, "Unknown protocol")

#ifndef EPROTOTYPE
#define EPROTOTYPE -1
#endif
CND(EPROTOTYPE, "Unknown protocol type")

#ifndef ESHUTDOWN
#define ESHUTDOWN -1
#endif
CND(ESHUTDOWN, "Cannot send once shutdown")

#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT -1
#endif
CND(ESOCKTNOSUPPORT, "Socket type not supported")

#ifndef ETIMEDOUT
#define ETIMEDOUT -1
#endif
CND(ETIMEDOUT, "Connection timed out")

#ifndef ETOOMANYREFS
#define ETOOMANYREFS -1
#endif
CND(ETOOMANYREFS, "Too many references")

#ifndef EWOULDBLOCK
#define EWOULDBLOCK -1
#endif
CND(EWOULDBLOCK, "Operation would block")
_NL
TXT("   -----------------")
TXT("   -- Host errors --")
TXT("   -----------------")
_NL

#ifndef HOST_NOT_FOUND
#define HOST_NOT_FOUND -1
#endif
CND(HOST_NOT_FOUND, "Unknown host")

#ifndef TRY_AGAIN
#define TRY_AGAIN -1
#endif
CND(TRY_AGAIN, "Host name lookup failure")

#ifndef NO_DATA
#define NO_DATA -1
#endif
CND(NO_DATA, "No data record for name")

#ifndef NO_RECOVERY
#define NO_RECOVERY -1
#endif
CND(NO_RECOVERY, "Non recoverable errors")
_NL
TXT("   -------------------")
TXT("   -- Control flags --")
TXT("   -------------------")
_NL

#ifndef FIONBIO
#define FIONBIO -1
#endif
CNX(FIONBIO, "Set/clear non-blocking io")

#ifndef FIONREAD
#define FIONREAD -1
#endif
CNX(FIONREAD, "How many bytes to read")
_NL
TXT("   --------------------")
TXT("   -- Shutdown modes --")
TXT("   --------------------")
_NL

#ifndef SHUT_RD
#define SHUT_RD -1
#endif
CND(SHUT_RD, "No more recv")

#ifndef SHUT_WR
#define SHUT_WR -1
#endif
CND(SHUT_WR, "No more send")

#ifndef SHUT_RDWR
#define SHUT_RDWR -1
#endif
CND(SHUT_RDWR, "No more recv/send")
_NL
TXT("   ---------------------")
TXT("   -- Protocol levels --")
TXT("   ---------------------")
_NL

#ifndef SOL_SOCKET
#define SOL_SOCKET -1
#endif
CND(SOL_SOCKET, "Options for socket level")

#ifndef IPPROTO_IP
#define IPPROTO_IP -1
#endif
CND(IPPROTO_IP, "Dummy protocol for IP")

#ifndef IPPROTO_UDP
#define IPPROTO_UDP -1
#endif
CND(IPPROTO_UDP, "UDP")

#ifndef IPPROTO_TCP
#define IPPROTO_TCP -1
#endif
CND(IPPROTO_TCP, "TCP")
_NL
TXT("   -------------------")
TXT("   -- Request flags --")
TXT("   -------------------")
_NL

#ifndef MSG_OOB
#define MSG_OOB -1
#endif
CND(MSG_OOB, "Process out-of-band data")

#ifndef MSG_PEEK
#define MSG_PEEK -1
#endif
CND(MSG_PEEK, "Peek at incoming data")

#ifndef MSG_EOR
#define MSG_EOR -1
#endif
CND(MSG_EOR, "Send end of record")

#ifndef MSG_WAITALL
#define MSG_WAITALL -1
#endif
CND(MSG_WAITALL, "Wait for full reception")

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL -1
#endif
CND(MSG_NOSIGNAL, "No SIGPIPE on send")

#ifdef __linux__
# define MSG_Forced_Flags "MSG_NOSIGNAL"
#else
# define MSG_Forced_Flags "0"
#endif
CN_(MSG_Forced_Flags, "")
TXT("   --  Flags set on all send(2) calls")

_NL
TXT("   --------------------")
TXT("   -- Socket options --")
TXT("   --------------------")
_NL

#ifndef TCP_NODELAY
#define TCP_NODELAY -1
#endif
CND(TCP_NODELAY, "Do not coalesce packets")

#ifndef SO_REUSEADDR
#define SO_REUSEADDR -1
#endif
CND(SO_REUSEADDR, "Bind reuse local address")

#ifndef SO_KEEPALIVE
#define SO_KEEPALIVE -1
#endif
CND(SO_KEEPALIVE, "Enable keep-alive msgs")

#ifndef SO_LINGER
#define SO_LINGER -1
#endif
CND(SO_LINGER, "Defer close to flush data")

#ifndef SO_BROADCAST
#define SO_BROADCAST -1
#endif
CND(SO_BROADCAST, "Can send broadcast msgs")

#ifndef SO_SNDBUF
#define SO_SNDBUF -1
#endif
CND(SO_SNDBUF, "Set/get send buffer size")

#ifndef SO_RCVBUF
#define SO_RCVBUF -1
#endif
CND(SO_RCVBUF, "Set/get recv buffer size")

#ifndef SO_SNDTIMEO
#define SO_SNDTIMEO -1
#endif
CND(SO_SNDTIMEO, "Emission timeout")

#ifndef SO_RCVTIMEO
#define SO_RCVTIMEO -1
#endif
CND(SO_RCVTIMEO, "Reception timeout")

#ifndef SO_ERROR
#define SO_ERROR -1
#endif
CND(SO_ERROR, "Get/clear error status")

#ifndef IP_MULTICAST_IF
#define IP_MULTICAST_IF -1
#endif
CND(IP_MULTICAST_IF, "Set/get mcast interface")

#ifndef IP_MULTICAST_TTL
#define IP_MULTICAST_TTL -1
#endif
CND(IP_MULTICAST_TTL, "Set/get multicast TTL")

#ifndef IP_MULTICAST_LOOP
#define IP_MULTICAST_LOOP -1
#endif
CND(IP_MULTICAST_LOOP, "Set/get mcast loopback")

#ifndef IP_ADD_MEMBERSHIP
#define IP_ADD_MEMBERSHIP -1
#endif
CND(IP_ADD_MEMBERSHIP, "Join a multicast group")

#ifndef IP_DROP_MEMBERSHIP
#define IP_DROP_MEMBERSHIP -1
#endif
CND(IP_DROP_MEMBERSHIP, "Leave a multicast group")

_NL
TXT("end GNAT.Sockets.Constants;")

  output ();
  return 0;
}

void
output (void) {
  int text_max = 0, value_max = 0, l;
  struct line *p;
  char fmt[64];
#define UPD_MAX(x) do { \
  l = strlen (p->x); \
  if (l > x ## _max) x ## _max = l; \
} while (0)

  for (p = first; p != NULL; p = p->next) {
    if (p->value != NULL) {
      UPD_MAX(text);
      UPD_MAX(value);
    }
  }
  sprintf (fmt, "   %%-%ds : constant := %%%ds;%%s%%s\n",
    text_max, value_max);

  for (p = first; p != NULL; p = p->next) {
    if (p->value == NULL) {
      printf ("%s\n", p->text);
    } else {
      char *comment_sep = (strlen (p->comment) > 0)
                          ? " --  " : "";
      printf (fmt, p->text, p->value, comment_sep, p->comment);
    }
  }
}

char *
f_itoa (char *fmt, int n) {
  char buf[32];
  sprintf (buf, fmt, n);
  return strdup (buf);
}

void
add_line (char *_text, char *_value, char *_comment) {
  struct line *l = (struct line *) malloc (sizeof (struct line));
  l->text = _text;
  l->value = _value;
  l->comment = _comment;
  l->next = NULL;
  if (last == NULL)
    first = last = l;
  else {
    last->next = l;
    last = l;
  }
}
