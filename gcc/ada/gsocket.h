/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              G S O C K E T                               *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *         Copyright (C) 2004-2008, Free Software Foundation, Inc.          *
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

/*
 * RTEMS has these .h files but not until you have built RTEMS.  When
 * IN_RTS, you only have the .h files in the newlib C library.
 * Because this file is also included from gen-soccon.c which is built
 * to run on RTEMS (not IN_RTS), we must distinguish between IN_RTS
 * and using this file to compile gen-soccon.
 */
#if !(defined (VMS) || defined (__MINGW32__) || \
      (defined(__rtems__) && defined(IN_RTS)))
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <netdb.h>
#endif

/*
 * Handling of gethostbyname, gethostbyaddr, getservbyname and getservbyport
 * =========================================================================
 *
 * The default implementation of GNAT.Sockets.Thin requires that these
 * operations be either thread safe, or that a reentrant version getXXXbyYYY_r
 * be provided. In both cases, socket.c provides a __gnat_safe_getXXXbyYYY
 * function with the same signature as getXXXbyYYY_r. If the operating
 * system version of getXXXbyYYY is thread safe, the provided auxiliary
 * buffer argument is unused and ignored.
 *
 * Target specific versions of GNAT.Sockets.Thin for platforms that can't
 * fulfill these requirements must provide their own protection mechanism
 * in Safe_GetXXXbyYYY, and if they require GNAT.Sockets to provide a buffer
 * to this effect, then we need to set Need_Netdb_Buffer here (case of
 * VxWorks and VMS).
 */

#if defined (_AIX) || defined (__FreeBSD__) || defined (__hpux__) || defined (__osf__) || defined (_WIN32) || defined (__APPLE__)
# define HAVE_THREAD_SAFE_GETxxxBYyyy 1
#elif defined (sgi) || defined (linux) || (defined (sun) && defined (__SVR4) && !defined (__vxworks))
# define HAVE_GETxxxBYyyy_R 1
#endif

#if defined (HAVE_GETxxxBYyyy_R) || !defined (HAVE_THREAD_SAFE_GETxxxBYyyy)
# define Need_Netdb_Buffer 1
#else
# define Need_Netdb_Buffer 0
#endif
