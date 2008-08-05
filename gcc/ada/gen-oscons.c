/****************************************************************************
 *                                                                          *
 *                          GNAT SYSTEM UTILITIES                           *
 *                                                                          *
 *                            G E N - O S C O N                             *
 *                                                                          *
 *          Copyright (C) 2004-2008, Free Software Foundation, Inc.         *
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

/* This program generates s-oscons.ads */

/*
 * To build using DEC C:
 *
 * CC/DEFINE="TARGET=""OpenVMS""" gen-oscon
 * LINK gen-oscon
 * RUN gen-oscon
 *
 * Note: OpenVMS versions older than 8.3 provide an incorrect value in
 * the DEC C header files for MSG_WAITALL. To generate the VMS version
 * of s-oscons.ads, gen-oscon should be run on an 8.3 or later machine.
 */

#ifndef TARGET
# error Please define TARGET
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <fcntl.h>

#if !defined (__vxworks) && !defined (__VMS)
# include <termios.h>
#endif

#include "gsocket.h"

typedef enum { NUM, TXT } kind_t;

struct line {
  char *text;
  char *value;
  char *comment;
  kind_t kind;
  struct line *next;
};

struct line *first = NULL, *last = NULL;

#define TXT(_text) add_line(_text, NULL, NULL, TXT);
/* Plain text */

#define _NL TXT("")
/* Empty line */

#define itoad(n) f_itoa ("%d", (n))
#define itoax(n) f_itoa ("16#%08x#", (n))

#define CND(name,comment) add_line(#name, itoad (name), comment, NUM);
/* Constant (decimal) */

#define CNX(name,comment) add_line(#name, itoax (name), comment, NUM);
/* Constant (hexadecimal) */

#define CN_(name,comment) add_line(#name, name, comment, TXT);
/* Constant (generic) */

#define STR(p) STR1(p)
#define STR1(p) #p

void output (void);
/* Generate output spec */

char *f_itoa (char *, int);
/* int to string */

void add_line (char *, char*, char*, kind_t);

#ifdef __MINGW32__
unsigned int _CRT_fmode = _O_BINARY;
#endif

int
main (void) {

TXT("------------------------------------------------------------------------------")
TXT("--                                                                          --")
TXT("--                         GNAT COMPILER COMPONENTS                         --")
TXT("--                                                                          --")
TXT("--                  S Y S T E M . O S _ C O N S T A N T S                   --")
TXT("--                                                                          --")
TXT("--                                 S p e c                                  --")
TXT("--                                                                          --")
TXT("--          Copyright (C) 2000-2008, Free Software Foundation, Inc.         --")
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
TXT("--  by the GNAT runtime library. This package should not be directly with'd")
TXT("--  by an application program.")
_NL
TXT("--  This is the version for " TARGET)
TXT("--  This file is generated automatically, do not modify it by hand! Instead,")
TXT("--  make changes to gen-oscon.c and re-run it on each target.")
_NL
TXT("with Interfaces.C;")
TXT("package System.OS_Constants is")
_NL
TXT("   pragma Pure;")
_NL
TXT("   ---------------------")
TXT("   -- File open modes --")
TXT("   ---------------------")
_NL

#ifndef O_RDWR
# define O_RDWR -1
#endif
CND(O_RDWR, "Read/write")

#ifndef O_NOCTTY
# define O_NOCTTY -1
#endif
CND(O_NOCTTY, "Don't change ctrl tty")

#ifndef O_NDELAY
# define O_NDELAY -1
#endif
CND(O_NDELAY, "Nonblocking")

_NL
TXT("   ----------------------")
TXT("   -- Fcntl operations --")
TXT("   ----------------------")
_NL

#ifndef F_GETFL
# define F_GETFL -1
#endif
CND(F_GETFL, "Get flags")

#ifndef F_SETFL
# define F_SETFL -1
#endif
CND(F_SETFL, "Set flags")

_NL
TXT("   -----------------")
TXT("   -- Fcntl flags --")
TXT("   -----------------")
_NL

#ifndef FNDELAY
# define FNDELAY -1
#endif
CND(FNDELAY, "Nonblocking")

#if !defined(__vxworks) && !defined(__VMS)

_NL
TXT("   ----------------------")
TXT("   -- Termical control --")
TXT("   ----------------------")
_NL

#ifndef TCSANOW
# define TCSANOW -1
#endif
CND(TCSANOW, "Immediate")

#ifndef TCIFLUSH
# define TCIFLUSH -1
#endif
CND(TCIFLUSH, "Flush input")

#ifndef CLOCAL
# define CLOCAL -1
#endif
CND(CLOCAL, "Local")

#ifndef CRTSCTS
# define CRTSCTS -1
#endif
CND(CRTSCTS, "Hardware flow control")

#ifndef CREAD
# define CREAD -1
#endif
CND(CREAD, "Read")

#ifndef CS5
# define CS5 -1
#endif
CND(CS5, "5 data bits")

#ifndef CS6
# define CS6 -1
#endif
CND(CS6, "6 data bits")

#ifndef CS7
# define CS7 -1
#endif
CND(CS7, "7 data bits")

#ifndef CS8
# define CS8 -1
#endif
CND(CS8, "8 data bits")

#ifndef CSTOPB
# define CSTOPB -1
#endif
CND(CSTOPB, "2 stop bits")

#ifndef PARENB
# define PARENB -1
#endif
CND(PARENB, "Parity enable")

#ifndef PARODD
# define PARODD -1
#endif
CND(PARODD, "Parity odd")

#ifndef B0
# define B0 -1
#endif
CND(B0, "0 bps")

#ifndef B50
# define B50 -1
#endif
CND(B50, "50 bps")

#ifndef B75
# define B75 -1
#endif
CND(B75, "75 bps")

#ifndef B110
# define B110 -1
#endif
CND(B110, "110 bps")

#ifndef B134
# define B134 -1
#endif
CND(B134, "134 bps")

#ifndef B150
# define B150 -1
#endif
CND(B150, "150 bps")

#ifndef B200
# define B200 -1
#endif
CND(B200, "200 bps")

#ifndef B300
# define B300 -1
#endif
CND(B300, "300 bps")

#ifndef B600
# define B600 -1
#endif
CND(B600, "600 bps")

#ifndef B1200
# define B1200 -1
#endif
CND(B1200, "1200 bps")

#ifndef B1800
# define B1800 -1
#endif
CND(B1800, "1800 bps")

#ifndef B2400
# define B2400 -1
#endif
CND(B2400, "2400 bps")

#ifndef B4800
# define B4800 -1
#endif
CND(B4800, "4800 bps")

#ifndef B9600
# define B9600 -1
#endif
CND(B9600, "9600 bps")

#ifndef B19200
# define B19200 -1
#endif
CND(B19200, "19200 bps")

#ifndef B38400
# define B38400 -1
#endif
CND(B38400, "38400 bps")

#ifndef B57600
# define B57600 -1
#endif
CND(B57600, "57600 bps")

#ifndef B115200
# define B115200 -1
#endif
CND(B115200, "115200 bps")

#ifndef B230400
# define B230400 -1
#endif
CND(B230400, "230400 bps")

#ifndef B460800
# define B460800 -1
#endif
CND(B460800, "460800 bps")

#ifndef B500000
# define B500000 -1
#endif
CND(B500000, "500000 bps")

#ifndef B576000
# define B576000 -1
#endif
CND(B576000, "576000 bps")

#ifndef B921600
# define B921600 -1
#endif
CND(B921600, "921600 bps")

#ifndef B1000000
# define B1000000 -1
#endif
CND(B1000000, "1000000 bps")

#ifndef B1152000
# define B1152000 -1
#endif
CND(B1152000, "1152000 bps")

#ifndef B1500000
# define B1500000 -1
#endif
CND(B1500000, "1500000 bps")

#ifndef B2000000
# define B2000000 -1
#endif
CND(B2000000, "2000000 bps")

#ifndef B2500000
# define B2500000 -1
#endif
CND(B2500000, "2500000 bps")

#ifndef B3000000
# define B3000000 -1
#endif
CND(B3000000, "3000000 bps")

#ifndef B3500000
# define B3500000 -1
#endif
CND(B3500000, "3500000 bps")

#ifndef B4000000
# define B4000000 -1
#endif
CND(B4000000, "4000000 bps")

_NL
TXT("   ---------------------------------")
TXT("   -- Terminal control characters --")
TXT("   ---------------------------------")
_NL

#ifndef VINTR
# define VINTR -1
#endif
CND(VINTR, "Interrupt")

#ifndef VQUIT
# define VQUIT -1
#endif
CND(VQUIT, "Quit")

#ifndef VERASE
# define VERASE -1
#endif
CND(VERASE, "Erase")

#ifndef VKILL
# define VKILL -1
#endif
CND(VKILL, "Kill")

#ifndef VEOF
# define VEOF -1
#endif
CND(VEOF, "EOF")

#ifndef VTIME
# define VTIME -1
#endif
CND(VTIME, "Read timeout")

#ifndef VMIN
# define VMIN -1
#endif
CND(VMIN, "Read min chars")

#ifndef VSWTC
# define VSWTC -1
#endif
CND(VSWTC, "Switch")

#ifndef VSTART
# define VSTART -1
#endif
CND(VSTART, "Flow control start")

#ifndef VSTOP
# define VSTOP -1
#endif
CND(VSTOP, "Flow control stop")

#ifndef VSUSP
# define VSUSP -1
#endif
CND(VSUSP, "Suspend")

#ifndef VEOL
# define VEOL -1
#endif
CND(VEOL, "EOL")

#ifndef VREPRINT
# define VREPRINT -1
#endif
CND(VREPRINT, "Reprint unread")

#ifndef VDISCARD
# define VDISCARD -1
#endif
CND(VDISCARD, "Discard pending")

#ifndef VWERASE
# define VWERASE -1
#endif
CND(VWERASE, "Word erase")

#ifndef VLNEXT
# define VLNEXT -1
#endif
CND(VLNEXT, "Literal next")

#ifndef VEOL2
# define VEOL2 -1
#endif
CND(VEOL2, "Alternative EOL")

#endif /* !defined(__vxworks) && !defined(__VMS) */

_NL
TXT("   --------------")
TXT("   -- Families --")
TXT("   --------------")
_NL

#ifndef AF_INET
# define AF_INET -1
#endif
CND(AF_INET, "IPv4 address family")

#ifndef AF_INET6
# define AF_INET6 -1
#else
# define HAVE_AF_INET6 1
#endif
CND(AF_INET6, "IPv6 address family")
_NL
TXT("   ------------------")
TXT("   -- Socket modes --")
TXT("   ------------------")
_NL

#ifndef SOCK_STREAM
# define SOCK_STREAM -1
#endif
CND(SOCK_STREAM, "Stream socket")

#ifndef SOCK_DGRAM
# define SOCK_DGRAM -1
#endif
CND(SOCK_DGRAM, "Datagram socket")
_NL
TXT("   ------------------")
TXT("   -- Errno values --")
TXT("   ------------------")
_NL

#ifndef EACCES
# define EACCES -1
#endif
CND(EACCES, "Permission denied")

#ifndef EADDRINUSE
# define EADDRINUSE -1
#endif
CND(EADDRINUSE, "Address already in use")

#ifndef EADDRNOTAVAIL
# define EADDRNOTAVAIL -1
#endif
CND(EADDRNOTAVAIL, "Cannot assign address")

#ifndef EAFNOSUPPORT
# define EAFNOSUPPORT -1
#endif
CND(EAFNOSUPPORT, "Addr family not supported")

#ifndef EAGAIN
# define EAGAIN -1
#endif
CND(EAGAIN, "Try again")

#ifndef EALREADY
# define EALREADY -1
#endif
CND(EALREADY, "Operation in progress")

#ifndef EBADF
# define EBADF -1
#endif
CND(EBADF, "Bad file descriptor")

#ifndef ECONNABORTED
# define ECONNABORTED -1
#endif
CND(ECONNABORTED, "Connection aborted")

#ifndef ECONNREFUSED
# define ECONNREFUSED -1
#endif
CND(ECONNREFUSED, "Connection refused")

#ifndef ECONNRESET
# define ECONNRESET -1
#endif
CND(ECONNRESET, "Connection reset by peer")

#ifndef EDESTADDRREQ
# define EDESTADDRREQ -1
#endif
CND(EDESTADDRREQ, "Destination addr required")

#ifndef EFAULT
# define EFAULT -1
#endif
CND(EFAULT, "Bad address")

#ifndef EHOSTDOWN
# define EHOSTDOWN -1
#endif
CND(EHOSTDOWN, "Host is down")

#ifndef EHOSTUNREACH
# define EHOSTUNREACH -1
#endif
CND(EHOSTUNREACH, "No route to host")

#ifndef EINPROGRESS
# define EINPROGRESS -1
#endif
CND(EINPROGRESS, "Operation now in progress")

#ifndef EINTR
# define EINTR -1
#endif
CND(EINTR, "Interrupted system call")

#ifndef EINVAL
# define EINVAL -1
#endif
CND(EINVAL, "Invalid argument")

#ifndef EIO
# define EIO -1
#endif
CND(EIO, "Input output error")

#ifndef EISCONN
# define EISCONN -1
#endif
CND(EISCONN, "Socket already connected")

#ifndef ELOOP
# define ELOOP -1
#endif
CND(ELOOP, "Too many symbolic links")

#ifndef EMFILE
# define EMFILE -1
#endif
CND(EMFILE, "Too many open files")

#ifndef EMSGSIZE
# define EMSGSIZE -1
#endif
CND(EMSGSIZE, "Message too long")

#ifndef ENAMETOOLONG
# define ENAMETOOLONG -1
#endif
CND(ENAMETOOLONG, "Name too long")

#ifndef ENETDOWN
# define ENETDOWN -1
#endif
CND(ENETDOWN, "Network is down")

#ifndef ENETRESET
# define ENETRESET -1
#endif
CND(ENETRESET, "Disconn. on network reset")

#ifndef ENETUNREACH
# define ENETUNREACH -1
#endif
CND(ENETUNREACH, "Network is unreachable")

#ifndef ENOBUFS
# define ENOBUFS -1
#endif
CND(ENOBUFS, "No buffer space available")

#ifndef ENOENT
# define ENOENT -1
#endif
CND(ENOENT, "File not found")

#ifndef ENOMEM
# define ENOMEM -1
#endif
CND(ENOMEM, "Out of memory")

#ifndef ENOPROTOOPT
# define ENOPROTOOPT -1
#endif
CND(ENOPROTOOPT, "Protocol not available")

#ifndef ENOTCONN
# define ENOTCONN -1
#endif
CND(ENOTCONN, "Socket not connected")

#ifndef ENOTSOCK
# define ENOTSOCK -1
#endif
CND(ENOTSOCK, "Operation on non socket")

#ifndef EOPNOTSUPP
# define EOPNOTSUPP -1
#endif
CND(EOPNOTSUPP, "Operation not supported")

#ifndef EPFNOSUPPORT
# define EPFNOSUPPORT -1
#endif
CND(EPFNOSUPPORT, "Unknown protocol family")

#ifndef EPROTONOSUPPORT
# define EPROTONOSUPPORT -1
#endif
CND(EPROTONOSUPPORT, "Unknown protocol")

#ifndef EPROTOTYPE
# define EPROTOTYPE -1
#endif
CND(EPROTOTYPE, "Unknown protocol type")

#ifndef ESHUTDOWN
# define ESHUTDOWN -1
#endif
CND(ESHUTDOWN, "Cannot send once shutdown")

#ifndef ESOCKTNOSUPPORT
# define ESOCKTNOSUPPORT -1
#endif
CND(ESOCKTNOSUPPORT, "Socket type not supported")

#ifndef ETIMEDOUT
# define ETIMEDOUT -1
#endif
CND(ETIMEDOUT, "Connection timed out")

#ifndef ETOOMANYREFS
# define ETOOMANYREFS -1
#endif
CND(ETOOMANYREFS, "Too many references")

#ifndef EWOULDBLOCK
# define EWOULDBLOCK -1
#endif
CND(EWOULDBLOCK, "Operation would block")
_NL
TXT("   -----------------")
TXT("   -- Host errors --")
TXT("   -----------------")
_NL

#ifndef HOST_NOT_FOUND
# define HOST_NOT_FOUND -1
#endif
CND(HOST_NOT_FOUND, "Unknown host")

#ifndef TRY_AGAIN
# define TRY_AGAIN -1
#endif
CND(TRY_AGAIN, "Host name lookup failure")

#ifndef NO_DATA
# define NO_DATA -1
#endif
CND(NO_DATA, "No data record for name")

#ifndef NO_RECOVERY
# define NO_RECOVERY -1
#endif
CND(NO_RECOVERY, "Non recoverable errors")
_NL
TXT("   ----------------------")
TXT("   -- Ioctl operatings --")
TXT("   ----------------------")
_NL

#ifndef FIONBIO
# define FIONBIO -1
#endif
CND(FIONBIO, "Set/clear non-blocking io")

#ifndef FIONREAD
# define FIONREAD -1
#endif
CND(FIONREAD, "How many bytes to read")
_NL
TXT("   --------------------")
TXT("   -- Shutdown modes --")
TXT("   --------------------")
_NL

#ifndef SHUT_RD
# define SHUT_RD -1
#endif
CND(SHUT_RD, "No more recv")

#ifndef SHUT_WR
# define SHUT_WR -1
#endif
CND(SHUT_WR, "No more send")

#ifndef SHUT_RDWR
# define SHUT_RDWR -1
#endif
CND(SHUT_RDWR, "No more recv/send")
_NL
TXT("   ---------------------")
TXT("   -- Protocol levels --")
TXT("   ---------------------")
_NL

#ifndef SOL_SOCKET
# define SOL_SOCKET -1
#endif
CND(SOL_SOCKET, "Options for socket level")

#ifndef IPPROTO_IP
# define IPPROTO_IP -1
#endif
CND(IPPROTO_IP, "Dummy protocol for IP")

#ifndef IPPROTO_UDP
# define IPPROTO_UDP -1
#endif
CND(IPPROTO_UDP, "UDP")

#ifndef IPPROTO_TCP
# define IPPROTO_TCP -1
#endif
CND(IPPROTO_TCP, "TCP")
_NL
TXT("   -------------------")
TXT("   -- Request flags --")
TXT("   -------------------")
_NL

#ifndef MSG_OOB
# define MSG_OOB -1
#endif
CND(MSG_OOB, "Process out-of-band data")

#ifndef MSG_PEEK
# define MSG_PEEK -1
#endif
CND(MSG_PEEK, "Peek at incoming data")

#ifndef MSG_EOR
# define MSG_EOR -1
#endif
CND(MSG_EOR, "Send end of record")

#ifndef MSG_WAITALL
# define MSG_WAITALL -1
#endif
CND(MSG_WAITALL, "Wait for full reception")

#ifndef MSG_NOSIGNAL
# define MSG_NOSIGNAL -1
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
# define TCP_NODELAY -1
#endif
CND(TCP_NODELAY, "Do not coalesce packets")

#ifndef SO_REUSEADDR
# define SO_REUSEADDR -1
#endif
CND(SO_REUSEADDR, "Bind reuse local address")

#ifndef SO_REUSEPORT
# define SO_REUSEPORT -1
#endif
CND(SO_REUSEPORT, "Bind reuse port number")

#ifndef SO_KEEPALIVE
# define SO_KEEPALIVE -1
#endif
CND(SO_KEEPALIVE, "Enable keep-alive msgs")

#ifndef SO_LINGER
# define SO_LINGER -1
#endif
CND(SO_LINGER, "Defer close to flush data")

#ifndef SO_BROADCAST
# define SO_BROADCAST -1
#endif
CND(SO_BROADCAST, "Can send broadcast msgs")

#ifndef SO_SNDBUF
# define SO_SNDBUF -1
#endif
CND(SO_SNDBUF, "Set/get send buffer size")

#ifndef SO_RCVBUF
# define SO_RCVBUF -1
#endif
CND(SO_RCVBUF, "Set/get recv buffer size")

#ifndef SO_SNDTIMEO
# define SO_SNDTIMEO -1
#endif
CND(SO_SNDTIMEO, "Emission timeout")

#ifndef SO_RCVTIMEO
# define SO_RCVTIMEO -1
#endif
CND(SO_RCVTIMEO, "Reception timeout")

#ifndef SO_ERROR
# define SO_ERROR -1
#endif
CND(SO_ERROR, "Get/clear error status")

#ifndef IP_MULTICAST_IF
# define IP_MULTICAST_IF -1
#endif
CND(IP_MULTICAST_IF, "Set/get mcast interface")

#ifndef IP_MULTICAST_TTL
# define IP_MULTICAST_TTL -1
#endif
CND(IP_MULTICAST_TTL, "Set/get multicast TTL")

#ifndef IP_MULTICAST_LOOP
# define IP_MULTICAST_LOOP -1
#endif
CND(IP_MULTICAST_LOOP, "Set/get mcast loopback")

#ifndef IP_ADD_MEMBERSHIP
# define IP_ADD_MEMBERSHIP -1
#endif
CND(IP_ADD_MEMBERSHIP, "Join a multicast group")

#ifndef IP_DROP_MEMBERSHIP
# define IP_DROP_MEMBERSHIP -1
#endif
CND(IP_DROP_MEMBERSHIP, "Leave a multicast group")

#ifndef IP_PKTINFO
# define IP_PKTINFO -1
#endif
CND(IP_PKTINFO, "Get datagram info")

_NL
TXT("   -------------------")
TXT("   -- System limits --")
TXT("   -------------------")
_NL

#ifndef IOV_MAX
# define IOV_MAX INT_MAX
#endif
CND(IOV_MAX, "Maximum writev iovcnt")

_NL
TXT("   ----------------------")
TXT("   -- Type definitions --")
TXT("   ----------------------")
_NL

{
  struct timeval tv;
TXT("   --  Sizes (in bytes) of the components of struct timeval")
_NL
#define SIZEOF_tv_sec (sizeof tv.tv_sec)
CND(SIZEOF_tv_sec, "tv_sec")
#define SIZEOF_tv_usec (sizeof tv.tv_usec)
CND(SIZEOF_tv_usec, "tv_usec")
}
_NL
TXT("   --  Sizes of protocol specific address types (for sockaddr.sa_len)")
_NL
#define SIZEOF_sockaddr_in (sizeof (struct sockaddr_in))
CND(SIZEOF_sockaddr_in, "struct sockaddr_in")
#ifdef HAVE_AF_INET6
# define SIZEOF_sockaddr_in6 (sizeof (struct sockaddr_in6))
#else
# define SIZEOF_sockaddr_in6 0
#endif
CND(SIZEOF_sockaddr_in6, "struct sockaddr_in6")
_NL
TXT("   --  Size of file descriptor sets")
_NL
#define SIZEOF_fd_set (sizeof (fd_set))
CND(SIZEOF_fd_set, "fd_set");
_NL
TXT("   --  Fields of struct hostent")
_NL
#ifdef __MINGW32__
# define h_addrtype_t "short"
# define h_length_t   "short"
#else
# define h_addrtype_t "int"
# define h_length_t   "int"
#endif
TXT("   subtype H_Addrtype_T is Interfaces.C." h_addrtype_t ";")
TXT("   subtype H_Length_T   is Interfaces.C." h_length_t ";")
_NL
TXT("   ----------------------------------------")
TXT("   -- Properties of supported interfaces --")
TXT("   ----------------------------------------")
_NL

CND(Need_Netdb_Buffer, "Need buffer for Netdb ops")
CND(Has_Sockaddr_Len,  "Sockaddr has sa_len field")
_NL
TXT("   Thread_Blocking_IO : constant Boolean := True;")
TXT("   --  Set False for contexts where socket i/o are process blocking")

#ifdef __vxworks
_NL
TXT("   --------------------------------")
TXT("   -- VxWorks-specific constants --")
TXT("   --------------------------------")
_NL
TXT("   --  These constants may be used only within the VxWorks version of")
TXT("   --  GNAT.Sockets.Thin.")
_NL

CND(OK,    "VxWorks generic success")
CND(ERROR, "VxWorks generic error")
#endif

#ifdef __MINGW32__
_NL
TXT("   ------------------------------")
TXT("   -- MinGW-specific constants --")
TXT("   ------------------------------")
_NL
TXT("   --  These constants may be used only within the MinGW version of")
TXT("   --  GNAT.Sockets.Thin.")
_NL

CND(WSASYSNOTREADY,     "System not ready")
CND(WSAVERNOTSUPPORTED, "Version not supported")
CND(WSANOTINITIALISED,  "Winsock not initialized")
CND(WSAEDISCON,         "Disconnected")
#endif

_NL
TXT("end System.OS_Constants;")

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
      if (p->kind == NUM)
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
  char buf[32], *ret;
  sprintf (buf, fmt, n);
  ret = malloc (strlen (buf) + 1);
  if (ret != NULL)
    strcpy (ret, buf);
  return ret;
}

void
add_line (char *_text, char *_value, char *_comment, kind_t _kind) {
  struct line *l = (struct line *) malloc (sizeof (struct line));

  l->text    = _text;
  l->value   = _value;
  l->comment = _comment;
  l->kind    = _kind;
  l->next    = NULL;

  if (last == NULL)
    first = last = l;
  else {
    last->next = l;
    last = l;
  }
}
