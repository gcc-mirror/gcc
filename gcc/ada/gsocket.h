/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              G S O C K E T                               *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *         Copyright (C) 2004-2005, Free Software Foundation, Inc.          *
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
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED 1
/* For HP-UX */
#endif

#ifndef BSD_COMP
#define BSD_COMP 1
/* For Solaris */
#endif

#ifndef _ALL_SOURCE
#define _ALL_SOURCE 1
/* For AIX */
#endif

#ifndef _OSF_SOURCE
#define _OSF_SOURCE 1
/* For Tru64 */
#endif

#include <limits.h>

#if defined(__vxworks)
#include <vxWorks.h>
#include <ioLib.h>
#include <hostLib.h>
#include <resolvLib.h>
#define SHUT_RD		0
#define SHUT_WR		1
#define SHUT_RDWR	2

#elif defined (WINNT)
#define FD_SETSIZE 1024
#include <windows.h>

#ifdef __MINGW32__
#include <winsock2.h>
#include <ws2tcpip.h>

#define EACCES		WSAEACCES
#define EADDRINUSE	WSAEADDRINUSE
#define EADDRNOTAVAIL	WSAEADDRNOTAVAIL
#define EAFNOSUPPORT	WSAEAFNOSUPPORT
#define EALREADY	WSAEALREADY
#define EBADF		WSAEBADF
#define ECONNABORTED	WSAECONNABORTED
#define ECONNREFUSED	WSAECONNREFUSED
#define ECONNRESET	WSAECONNRESET
#define EDESTADDRREQ	WSAEDESTADDRREQ
#define EFAULT		WSAEFAULT
#define EHOSTDOWN	WSAEHOSTDOWN
#define EHOSTUNREACH	WSAEHOSTUNREACH
#define EINPROGRESS	WSAEINPROGRESS
#define EINTR		WSAEINTR
#define EINVAL		WSAEINVAL
#define EIO		WSAEDISCON
#define EISCONN		WSAEISCONN
#define ELOOP		WSAELOOP
#define EMFILE		WSAEMFILE
#define EMSGSIZE	WSAEMSGSIZE
#define ENAMETOOLONG	WSAENAMETOOLONG
#define ENETDOWN	WSAENETDOWN
#define ENETRESET	WSAENETRESET
#define ENETUNREACH	WSAENETUNREACH
#define ENOBUFS		WSAENOBUFS
#define ENOPROTOOPT	WSAENOPROTOOPT
#define ENOTCONN	WSAENOTCONN
#define ENOTSOCK	WSAENOTSOCK
#define EOPNOTSUPP	WSAEOPNOTSUPP
#define EPFNOSUPPORT	WSAEPFNOSUPPORT
#define EPROTONOSUPPORT	WSAEPROTONOSUPPORT
#define ENOTSOCK	WSAENOTSOCK
#define EOPNOTSUPP	WSAEOPNOTSUPP
#define EPFNOSUPPORT	WSAEPFNOSUPPORT
#define EPROTONOSUPPORT	WSAEPROTONOSUPPORT
#define EPROTOTYPE	WSAEPROTOTYPE
#define ESHUTDOWN	WSAESHUTDOWN
#define ESOCKTNOSUPPORT	WSAESOCKTNOSUPPORT
#define ETIMEDOUT	WSAETIMEDOUT
#define ETOOMANYREFS	WSAETOOMANYREFS
#define EWOULDBLOCK	WSAEWOULDBLOCK
#define SHUT_RD		SD_RECEIVE
#define SHUT_WR		SD_SEND
#define SHUT_RDWR	SD_BOTH

#endif

#elif defined(VMS)
#define FD_SETSIZE 4096
#ifndef IN_RTS
/* These DEC C headers are not available when building with GCC */
#include <in.h>
#include <tcp.h>
#include <ioctl.h>
#include <netdb.h>
#endif

#endif

#ifndef __MINGW32__
#include <errno.h>
#endif

#ifdef __vxworks
#include <sys/times.h>
#else
#include <sys/time.h>
#endif

#if !(defined (VMS) || defined (__MINGW32__) || defined(__rtems__))
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <netdb.h>
#endif
