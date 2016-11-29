/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              G S O C K E T                               *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *         Copyright (C) 2004-2015, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#if defined(VTHREADS) || defined(__PikeOS__) || defined(__DJGPP__)

/* Sockets not supported on these platforms.  */
#undef HAVE_SOCKETS

#else

#define HAVE_SOCKETS

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

/** No system header may be included prior to this point since on some targets
 ** we need to redefine FD_SETSIZE.
 **/

/* Target-specific includes and definitions */

#if defined(__vxworks)
#include <vxWorks.h>
#include <ioLib.h>
#include <hostLib.h>
#define SHUT_RD		0
#define SHUT_WR		1
#define SHUT_RDWR	2

#elif defined (WINNT)
#define FD_SETSIZE 1024

#ifdef __MINGW32__
#include <winsock2.h>
#include <ws2tcpip.h>

#undef  EACCES
#define EACCES          WSAEACCES
#undef  EADDRINUSE
#define EADDRINUSE      WSAEADDRINUSE
#undef  EADDRNOTAVAIL
#define EADDRNOTAVAIL   WSAEADDRNOTAVAIL
#undef  EAFNOSUPPORT
#define EAFNOSUPPORT    WSAEAFNOSUPPORT
#undef  EALREADY
#define EALREADY        WSAEALREADY
#undef  EBADF
#define EBADF           WSAEBADF
#undef  ECONNABORTED
#define ECONNABORTED    WSAECONNABORTED
#undef  ECONNREFUSED
#define ECONNREFUSED    WSAECONNREFUSED
#undef  ECONNRESET
#define ECONNRESET      WSAECONNRESET
#undef  EDESTADDRREQ
#define EDESTADDRREQ    WSAEDESTADDRREQ
#undef  EFAULT
#define EFAULT          WSAEFAULT
#undef  EHOSTDOWN
#define EHOSTDOWN       WSAEHOSTDOWN
#undef  EHOSTUNREACH
#define EHOSTUNREACH    WSAEHOSTUNREACH
#undef  EINPROGRESS
#define EINPROGRESS     WSAEINPROGRESS
#undef  EINTR
#define EINTR           WSAEINTR
#undef  EINVAL
#define EINVAL          WSAEINVAL
#undef  EIO
#define EIO             WSAEDISCON
#undef  EISCONN
#define EISCONN         WSAEISCONN
#undef  ELOOP
#define ELOOP           WSAELOOP
#undef  EMFILE
#define EMFILE          WSAEMFILE
#undef  EMSGSIZE
#define EMSGSIZE        WSAEMSGSIZE
#undef  ENAMETOOLONG
#define ENAMETOOLONG    WSAENAMETOOLONG
#undef  ENETDOWN
#define ENETDOWN        WSAENETDOWN
#undef  ENETRESET
#define ENETRESET       WSAENETRESET
#undef  ENETUNREACH
#define ENETUNREACH     WSAENETUNREACH
#undef  ENOBUFS
#define ENOBUFS         WSAENOBUFS
#undef  ENOPROTOOPT
#define ENOPROTOOPT     WSAENOPROTOOPT
#undef  ENOTCONN
#define ENOTCONN        WSAENOTCONN
#undef  ENOTSOCK
#define ENOTSOCK        WSAENOTSOCK
#undef  EOPNOTSUPP
#define EOPNOTSUPP      WSAEOPNOTSUPP
#undef  EPFNOSUPPORT
#define EPFNOSUPPORT    WSAEPFNOSUPPORT
#undef  EPROTONOSUPPORT
#define EPROTONOSUPPORT WSAEPROTONOSUPPORT
#undef  EPROTOTYPE
#define EPROTOTYPE      WSAEPROTOTYPE
#undef  ESHUTDOWN
#define ESHUTDOWN       WSAESHUTDOWN
#undef  ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT
#undef  ETIMEDOUT
#define ETIMEDOUT       WSAETIMEDOUT
#undef  ETOOMANYREFS
#define ETOOMANYREFS    WSAETOOMANYREFS
#undef  EWOULDBLOCK
#define EWOULDBLOCK     WSAEWOULDBLOCK

#define SHUT_RD		SD_RECEIVE
#define SHUT_WR		SD_SEND
#define SHUT_RDWR	SD_BOTH

#endif

#include <windows.h>

#elif defined(VMS)
/* Allow a large number of fds for select.  */
#define FD_SETSIZE 4096
#ifndef IN_RTS
/* These DEC C headers are not available when building with GCC.  Order is
   important.  */
#include <time.h>
#include <tcp.h>
#include <in.h>
#include <ioctl.h>
#include <netdb.h>
#endif

#endif

#include <limits.h>
#include <errno.h>

#if defined (__vxworks) && ! defined (__RTP__)
#include <sys/times.h>
#else
#include <sys/time.h>
#endif

#if defined(__rtems__)
#include <unistd.h>
/* Required, for read(), write(), and close() */
#endif

/*
 * RTEMS has these .h files but not until you have built and installed RTEMS.
 * When building a C/C++ toolset, you also build the newlib C library, so the
 * build procedure for an RTEMS GNAT toolset requires that you build a C/C++
 * toolset, then build and install RTEMS with --enable-multilib, and finally
 * build the Ada part of the toolset.
 */
#if !(defined (VMS) || defined (__MINGW32__))
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <netdb.h>
#endif

#ifdef __ANDROID__
#include <unistd.h>
#include <sys/select.h>
#endif

#if defined (_AIX) || defined (__FreeBSD__) || defined (__hpux__) || \
    defined (_WIN32) || defined (__APPLE__) || defined (__ANDROID__) || \
    defined (__DragonFly__) || defined (__NetBSD__) || defined (__OpenBSD__)
# define HAVE_THREAD_SAFE_GETxxxBYyyy 1

#elif defined (__linux__) || defined (__GLIBC__) || \
     (defined (__sun__) && !defined (__vxworks)) || \
      defined (__rtems__)
# define HAVE_GETxxxBYyyy_R 1
#endif

/*
 * Properties of the unerlying NetDB library:
 *   Need_Netdb_Buffer __gnat_getXXXbyYYY expects a caller-supplied buffer
 *   Need_Netdb_Lock   __gnat_getXXXbyYYY expects the caller to ensure
 *                     mutual exclusion
 *
 * See "Handling of gethostbyname, gethostbyaddr, getservbyname and
 * getservbyport" in socket.c for details.
 */

#if defined (HAVE_GETxxxBYyyy_R)
# define Need_Netdb_Buffer 1
# define Need_Netdb_Lock 0

#else
# define Need_Netdb_Buffer 0
# if !defined (HAVE_THREAD_SAFE_GETxxxBYyyy)
#  define Need_Netdb_Lock 1
# else
#  define Need_Netdb_Lock 0
# endif
#endif

#if defined (__FreeBSD__) || defined (__vxworks) || defined(__rtems__) \
 || defined (__DragonFly__) || defined (__NetBSD__) || defined (__OpenBSD__)
# define Has_Sockaddr_Len 1
#else
# define Has_Sockaddr_Len 0
#endif

#if !(defined (__vxworks) || defined (_WIN32) || defined (__hpux__) || defined (VMS))
# define HAVE_INET_PTON
#endif

#endif /* defined(VTHREADS) */
