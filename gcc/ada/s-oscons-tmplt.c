/*
------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . O S _ C O N S T A N T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks ("M32766");
--  Allow long lines

*/

/**
 **  This template file is used while building the GNAT runtime library to
 **  generate package System.OS_Constants (s-oscons.ads).
 **
 **  The generation process is:
 **  1. the platform-independent extraction tool xoscons is built with the
 **     base native compiler
 **  2. this template is processed by the cross C compiler to produce
 **     a list of constant values
 **  3. the comments in this template and the list of values are processed
 **     by xoscons to generate s-oscons.ads.
 **
 **  Any comment occurring in this file whose start and end markers are on
 **  a line by themselves (see above) is copied verbatim to s-oscons.ads.
 **  All other comments are ignored. Note that the build process first passes
 **  this file through the C preprocessor, so comments that occur in a section
 **  that is conditioned by a #if directive will be copied to the output only
 **  when it applies.
 **
 **  Two methods are supported to generate the list of constant values,
 **  s-oscons-tmpl.s.
 **
 **  The default one assumes that the template can be compiled by the newly-
 **  built cross compiler. It uses markup produced in the (pseudo-)assembly
 **  listing:
 **
 **     xgcc -DTARGET=\"$target\" -C -E s-oscons-tmplt.c > s-oscons-tmplt.i
 **     xgcc -S s-oscons-tmplt.i
 **     xoscons
 **
 **  Alternatively, if s-oscons-tmplt.c must be compiled with a proprietary
 **  compiler (e.g. the native DEC CC on OpenVMS), the NATIVE macro should
 **  be defined, and the resulting program executed:
 **
 **  $ CC/DEFINE=("TARGET=""OpenVMS""",NATIVE)
 **      /PREPROCESS_ONLY /COMMENTS=AS_IS s-oscons-tmplt
 **  $ CC/DEFINE=("TARGET=""OpenVMS""",NATIVE) s-oscons-tmplt
 **  $ LINK s-oscons-tmplt
 **  $ DEFINE/USER SYS$OUTPUT s-oscons-tmplt.s
 **  $ RUN s-oscons-tmplt
 **  $ RUN xoscons
 **/

/* Feature macro definitions */

/**
 ** Note: we deliberately do not define _POSIX_SOURCE / _POSIX_C_SOURCE
 ** unconditionally, as on many platforms these macros actually disable
 ** a number of non-POSIX but useful/required features.
 **/

#if defined (__linux__) || defined (__ANDROID__)

/* Define _XOPEN_SOURCE to get IOV_MAX */
# if !defined (_XOPEN_SOURCE)
#  define _XOPEN_SOURCE 500
# endif

/* Define _BSD_SOURCE to get CRTSCTS */
# define _BSD_SOURCE

#endif /* defined (__linux__) */

/* Include gsocket.h before any system header so it can redefine FD_SETSIZE */

#include "gsocket.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <fcntl.h>
#include <time.h>

#if ! (defined (__vxworks) || defined (__MINGW32__))
# define HAVE_TERMIOS
#endif

#if defined (__vxworks)

/**
 ** For VxWorks, always include vxWorks.h (gsocket.h provides it only for
 ** the case of runtime libraries that support sockets). Note: this must
 ** be done before including adaint.h.
 **/

# include <vxWorks.h>
#endif

#include "adaint.h"

#ifdef DUMMY

# if defined (TARGET)
#   error TARGET may not be defined when generating the dummy version
# else
#   define TARGET "batch runtime compilation (dummy values)"
# endif

# if !(defined (HAVE_SOCKETS) && defined (HAVE_TERMIOS))
#   error Features missing on platform
# endif

# define NATIVE

#endif /* DUMMY */

#ifndef TARGET
# error Please define TARGET
#endif

#ifndef HAVE_SOCKETS
# include <errno.h>
#endif

#ifdef HAVE_TERMIOS
# include <termios.h>
#endif

#ifdef __APPLE__
# include <_types.h>
#endif

#if defined (__linux__) || defined (__ANDROID__) || defined (__QNX__) \
  || defined (__rtems__)
# include <pthread.h>
# include <signal.h>
#endif

#if defined(__MINGW32__) || defined(__CYGWIN__)
# include <windef.h>
# include <winbase.h>
#endif

#ifdef NATIVE
#include <stdio.h>

#ifdef DUMMY
int counter = 0;
# define _VAL(x) counter++
#else
# define _VAL(x) x
#endif

#define CND(name,comment) \
  printf ("\n->CND:$%d:" #name ":$%d:" comment, __LINE__, ((int) _VAL (name)));

#define CNU(name,comment) \
  printf ("\n->CNU:$%d:" #name ":$%u:" comment, __LINE__, ((unsigned int) _VAL (name)));

#define CNS(name,comment) \
  printf ("\n->CNS:$%d:" #name ":" name ":" comment, __LINE__);

#define C(sname,type,value,comment)\
  printf ("\n->C:$%d:" sname ":" #type ":" value ":" comment, __LINE__);

#define SUB(sname)\
  printf ("\n->SUB:$%d:" #sname ":" sname, __LINE__);

#define TXT(text) \
  printf ("\n->TXT:$%d:" text, __LINE__);

#else

#define CND(name, comment) \
  asm volatile("\n->CND:%0:" #name ":%1:" comment \
  : : "i" (__LINE__), "i" ((int) name));
/* Decimal constant in the range of type "int" */

#define CNU(name, comment) \
  asm volatile("\n->CNU:%0:" #name ":%1:" comment \
  : : "i" (__LINE__), "i" ((int) name));
/* Decimal constant in the range of type "unsigned int" (note, assembler
 * always wants a signed int, we convert back in xoscons).
 */

#define CNS(name, comment) \
  asm volatile("\n->CNS:%0:" #name ":" name ":" comment \
  : : "i" (__LINE__));
/* General expression named number */

#define C(sname, type, value, comment) \
  asm volatile("\n->C:%0:" sname ":" #type ":" value ":" comment \
  : : "i" (__LINE__));
/* Typed constant */

#define SUB(sname) \
  asm volatile("\n->SUB:%0:" #sname ":" sname \
  : : "i" (__LINE__));
/* Subtype */

#define TXT(text) \
  asm volatile("\n->TXT:%0:" text \
  : : "i" (__LINE__));
/* Freeform text */

#endif /* NATIVE */

#define CST(name,comment) C(#name,String,name,comment)
/* String constant */

#define STR(x) STR1(x)
#define STR1(x) #x

#ifdef __MINGW32__
unsigned int _CRT_fmode = _O_BINARY;
#endif

int
main (void) {

/*
--  This package provides target dependent definitions of constant for use
--  by the GNAT runtime library. This package should not be directly with'd
--  by an application program.

--  This file is generated automatically, do not modify it by hand! Instead,
--  make changes to s-oscons-tmplt.c and rebuild the GNAT runtime library.
*/

/**
 ** Do not change the format of the line below without also updating the
 ** MaRTE Makefile.
 **/
TXT("--  This is the version for " TARGET)
TXT("")
TXT("with Interfaces.C;")
#if defined (__MINGW32__)
# define TARGET_OS "Windows"
# define Serial_Port_Descriptor "System.Win32.HANDLE"
TXT("with System.Win32;")
#else
# define TARGET_OS "Other_OS"
# define Serial_Port_Descriptor "Interfaces.C.int"
#endif

/*
package System.OS_Constants is

   pragma Pure;
*/

/**
 **  General constants (all platforms)
 **/

/*

   ---------------------------------
   -- General platform parameters --
   ---------------------------------

   type OS_Type is (Windows, Other_OS);
*/
C("Target_OS", OS_Type, TARGET_OS, "")
/*
   pragma Warnings (Off, Target_OS);
   --  Suppress warnings on Target_OS since it is in general tested for
   --  equality with a constant value to implement conditional compilation,
   --  which normally generates a constant condition warning.

*/
#define Target_Name TARGET
CST(Target_Name, "")

/**
 ** Note: the name of the following constant is recognized specially by
 **  xoscons (case sensitive).
 **/
#define SIZEOF_unsigned_int sizeof (unsigned int)
CND(SIZEOF_unsigned_int, "Size of unsigned int")

SUB(Serial_Port_Descriptor)

/*

   -------------------
   -- System limits --
   -------------------

*/

#ifndef IOV_MAX
# define IOV_MAX INT_MAX
#endif
CND(IOV_MAX, "Maximum writev iovcnt")

/* NAME_MAX is used to compute the allocation size for a struct dirent
 * passed to readdir() / readdir_r(). However on some systems it is not
 * defined, as it is technically a filesystem dependent property that
 * we should retrieve through pathconf(). In any case, we do not need a
 * precise value but only an upper limit.
 */
#ifndef NAME_MAX
# ifdef MAXNAMELEN
   /* Solaris has no NAME_MAX but defines MAXNAMELEN */
#  define NAME_MAX MAXNAMELEN
# elif defined(PATH_MAX)
   /* PATH_MAX (maximum length of a full path name) is a safe fall back */
#  define NAME_MAX PATH_MAX
# elif defined(FILENAME_MAX)
   /* Similarly FILENAME_MAX can provide a safe fall back */
#  define NAME_MAX FILENAME_MAX
# else
   /* Hardcode a reasonably large value as a last chance fallback */
#  define NAME_MAX 1024
# endif
#endif
CND(NAME_MAX, "Maximum file name length")

/*

   ---------------------
   -- File open modes --
   ---------------------

*/

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

/*

   ----------------------
   -- Fcntl operations --
   ----------------------

*/

#ifndef F_GETFL
# define F_GETFL -1
#endif
CND(F_GETFL, "Get flags")

#ifndef F_SETFL
# define F_SETFL -1
#endif
CND(F_SETFL, "Set flags")

/*

   -----------------
   -- Fcntl flags --
   -----------------

*/

#ifndef FNDELAY
# define FNDELAY -1
#endif
CND(FNDELAY, "Nonblocking")

/*

   ----------------------
   -- Ioctl operations --
   ----------------------

*/

/* ioctl(2) requests are "int" in UNIX, but "unsigned long" on FreeBSD */

#if defined (__FreeBSD__) || defined (__DragonFly__)
# define CNI CNU
# define IOCTL_Req_T "Interfaces.C.unsigned"
#else
# define CNI CND
# define IOCTL_Req_T "Interfaces.C.int"
#endif

SUB(IOCTL_Req_T)

#ifndef FIONBIO
# define FIONBIO -1
#endif
CNI(FIONBIO, "Set/clear non-blocking io")

#ifndef FIONREAD
# define FIONREAD -1
#endif
CNI(FIONREAD, "How many bytes to read")

/*

   ------------------
   -- Errno values --
   ------------------

   --  The following constants are defined from <errno.h>

*/
#ifndef EAGAIN
# define EAGAIN -1
#endif
CND(EAGAIN, "Try again")

#ifndef ENOENT
# define ENOENT -1
#endif
CND(ENOENT, "File not found")

#ifndef ENOMEM
# define ENOMEM -1
#endif
CND(ENOMEM, "Out of memory")

#ifdef __MINGW32__
/*

   --  The following constants are defined from <winsock2.h> (WSA*)

*/

/**
 **  For sockets-related errno values on Windows, gsocket.h redefines
 **  Exxx as WSAExxx.
 **/

#endif

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

#ifndef EPIPE
# define EPIPE -1
#endif
CND(EPIPE, "Broken pipe")

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

#ifndef ERANGE
# define ERANGE -1
#endif
CND(ERANGE, "Result too large")

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

#ifndef E2BIG
# define E2BIG -1
#endif
CND(E2BIG, "Argument list too long")

#ifndef EILSEQ
# define EILSEQ -1
#endif
CND(EILSEQ, "Illegal byte sequence")

/**
 **  Terminal/serial I/O constants
 **/

#if defined(HAVE_TERMIOS) || defined(__MINGW32__)
/*

   ----------------------
   -- Terminal control --
   ----------------------

*/
#endif

#ifdef HAVE_TERMIOS

#ifndef TCSANOW
# define TCSANOW -1
#endif
CND(TCSANOW, "Immediate")

#ifndef TCIFLUSH
# define TCIFLUSH -1
#endif
CND(TCIFLUSH, "Flush input")

#ifndef IXON
# define IXON -1
#endif
CNU(IXON, "Output sw flow control")

#ifndef CLOCAL
# define CLOCAL -1
#endif
CNU(CLOCAL, "Local")

#ifndef CRTSCTS
# define CRTSCTS -1
#endif
CNU(CRTSCTS, "Output hw flow control")

#ifndef CREAD
# define CREAD -1
#endif
CNU(CREAD, "Read")

#ifndef CS5
# define CS5 -1
#endif
CNU(CS5, "5 data bits")

#ifndef CS6
# define CS6 -1
#endif
CNU(CS6, "6 data bits")

#ifndef CS7
# define CS7 -1
#endif
CNU(CS7, "7 data bits")

#ifndef CS8
# define CS8 -1
#endif
CNU(CS8, "8 data bits")

#ifndef CSTOPB
# define CSTOPB -1
#endif
CNU(CSTOPB, "2 stop bits")

#ifndef PARENB
# define PARENB -1
#endif
CNU(PARENB, "Parity enable")

#ifndef PARODD
# define PARODD -1
#endif
CNU(PARODD, "Parity odd")

#ifndef B0
# define B0 -1
#endif
CNU(B0, "0 bps")

#ifndef B50
# define B50 -1
#endif
CNU(B50, "50 bps")

#ifndef B75
# define B75 -1
#endif
CNU(B75, "75 bps")

#ifndef B110
# define B110 -1
#endif
CNU(B110, "110 bps")

#ifndef B134
# define B134 -1
#endif
CNU(B134, "134 bps")

#ifndef B150
# define B150 -1
#endif
CNU(B150, "150 bps")

#ifndef B200
# define B200 -1
#endif
CNU(B200, "200 bps")

#ifndef B300
# define B300 -1
#endif
CNU(B300, "300 bps")

#ifndef B600
# define B600 -1
#endif
CNU(B600, "600 bps")

#ifndef B1200
# define B1200 -1
#endif
CNU(B1200, "1200 bps")

#ifndef B1800
# define B1800 -1
#endif
CNU(B1800, "1800 bps")

#ifndef B2400
# define B2400 -1
#endif
CNU(B2400, "2400 bps")

#ifndef B4800
# define B4800 -1
#endif
CNU(B4800, "4800 bps")

#ifndef B9600
# define B9600 -1
#endif
CNU(B9600, "9600 bps")

#ifndef B19200
# define B19200 -1
#endif
CNU(B19200, "19200 bps")

#ifndef B38400
# define B38400 -1
#endif
CNU(B38400, "38400 bps")

#ifndef B57600
# define B57600 -1
#endif
CNU(B57600, "57600 bps")

#ifndef B115200
# define B115200 -1
#endif
CNU(B115200, "115200 bps")

#ifndef B230400
# define B230400 -1
#endif
CNU(B230400, "230400 bps")

#ifndef B460800
# define B460800 -1
#endif
CNU(B460800, "460800 bps")

#ifndef B500000
# define B500000 -1
#endif
CNU(B500000, "500000 bps")

#ifndef B576000
# define B576000 -1
#endif
CNU(B576000, "576000 bps")

#ifndef B921600
# define B921600 -1
#endif
CNU(B921600, "921600 bps")

#ifndef B1000000
# define B1000000 -1
#endif
CNU(B1000000, "1000000 bps")

#ifndef B1152000
# define B1152000 -1
#endif
CNU(B1152000, "1152000 bps")

#ifndef B1500000
# define B1500000 -1
#endif
CNU(B1500000, "1500000 bps")

#ifndef B2000000
# define B2000000 -1
#endif
CNU(B2000000, "2000000 bps")

#ifndef B2500000
# define B2500000 -1
#endif
CNU(B2500000, "2500000 bps")

#ifndef B3000000
# define B3000000 -1
#endif
CNU(B3000000, "3000000 bps")

#ifndef B3500000
# define B3500000 -1
#endif
CNU(B3500000, "3500000 bps")

#ifndef B4000000
# define B4000000 -1
#endif
CNU(B4000000, "4000000 bps")

/*

   ---------------------------------
   -- Terminal control characters --
   ---------------------------------

*/

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

#endif /* HAVE_TERMIOS */

#if defined(__MINGW32__) || defined(__CYGWIN__)
CNU(DTR_CONTROL_ENABLE, "Enable DTR flow ctrl")
CNU(RTS_CONTROL_ENABLE, "Enable RTS flow ctrl")
#endif

/*

   -----------------------------
   -- Pseudo terminal library --
   -----------------------------

*/

#if defined (__FreeBSD__) || defined (__linux__) || defined (__DragonFly__)
# define PTY_Library "-lutil"
#else
# define PTY_Library ""
#endif
CST(PTY_Library, "for g-exptty")

/**
 **  Sockets constants
 **/

#ifdef HAVE_SOCKETS

/*

   --------------
   -- Families --
   --------------

*/

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

#ifndef AF_UNIX
# define AF_UNIX -1
#endif
CND(AF_UNIX, "Local unix family")

#ifndef AF_UNSPEC
# define AF_UNSPEC -1
#else
# define HAVE_AF_UNSPEC 1
#endif
CND(AF_UNSPEC, "Unspecified address family")

/*

   -----------------------------
   -- addrinfo fields offsets --
   -----------------------------

*/

#ifdef AI_CANONNAME
  const struct addrinfo ai;

#define AI_FLAGS_OFFSET ((void *)&ai.ai_flags - (void *)&ai)
#define AI_FAMILY_OFFSET ((void *)&ai.ai_family - (void *)&ai)
#define AI_SOCKTYPE_OFFSET ((void *)&ai.ai_socktype - (void *)&ai)
#define AI_PROTOCOL_OFFSET ((void *)&ai.ai_protocol - (void *)&ai)
#define AI_ADDRLEN_OFFSET ((void *)&ai.ai_addrlen - (void *)&ai)
#define AI_ADDR_OFFSET ((void *)&ai.ai_addr - (void *)&ai)
#define AI_CANONNAME_OFFSET ((void *)&ai.ai_canonname - (void *)&ai)
#define AI_NEXT_OFFSET ((void *)&ai.ai_next - (void *)&ai)

#else

#define AI_FLAGS_OFFSET 0
#define AI_FAMILY_OFFSET 4
#define AI_SOCKTYPE_OFFSET 8
#define AI_PROTOCOL_OFFSET 12
#define AI_ADDRLEN_OFFSET 16
#define AI_CANONNAME_OFFSET 24
#define AI_ADDR_OFFSET 32
#define AI_NEXT_OFFSET 40

#endif

CND(AI_FLAGS_OFFSET,     "Offset of ai_flags in addrinfo");
CND(AI_FAMILY_OFFSET,    "Offset of ai_family in addrinfo");
CND(AI_SOCKTYPE_OFFSET,  "Offset of ai_socktype in addrinfo");
CND(AI_PROTOCOL_OFFSET,  "Offset of ai_protocol in addrinfo");
CND(AI_ADDRLEN_OFFSET,   "Offset of ai_addrlen in addrinfo");
CND(AI_ADDR_OFFSET,      "Offset of ai_addr in addrinfo");
CND(AI_CANONNAME_OFFSET, "Offset of ai_canonname in addrinfo");
CND(AI_NEXT_OFFSET,      "Offset of ai_next in addrinfo");

/*

   ---------------------------------------
   -- getaddrinfo getnameinfo constants --
   ---------------------------------------

*/

#ifndef AI_PASSIVE
# define AI_PASSIVE -1
#endif
CND(AI_PASSIVE, "NULL nodename for accepting")

#ifndef AI_CANONNAME
# define AI_CANONNAME -1
#endif
CND(AI_CANONNAME, "Get the host official name")

#ifndef AI_NUMERICSERV
# define AI_NUMERICSERV -1
#endif
CND(AI_NUMERICSERV, "Service is a numeric string")

#ifndef AI_NUMERICHOST
# define AI_NUMERICHOST -1
#endif
CND(AI_NUMERICHOST, "Node is a numeric IP address")

#ifndef AI_ADDRCONFIG
# define AI_ADDRCONFIG -1
#endif
CND(AI_ADDRCONFIG, "Returns addresses for only locally configured families")

#ifndef AI_V4MAPPED
# define AI_V4MAPPED -1
#endif
CND(AI_V4MAPPED, "Returns IPv4 mapped to IPv6")

#ifndef AI_ALL
# define AI_ALL -1
#endif
CND(AI_ALL, "Change AI_V4MAPPED behavior for unavailavle IPv6 addresses")

#ifndef NI_NAMEREQD
# define NI_NAMEREQD -1
#endif
CND(NI_NAMEREQD, "Error if the hostname cannot be determined")

#ifndef NI_DGRAM
# define NI_DGRAM -1
#endif
CND(NI_DGRAM, "Service is datagram")

#ifndef NI_NOFQDN
# define NI_NOFQDN -1
#endif
CND(NI_NOFQDN, "Return only the hostname part for local hosts")

#ifndef NI_NUMERICSERV
# define NI_NUMERICSERV -1
#endif
CND(NI_NUMERICSERV, "Numeric form of the service")

#ifndef NI_NUMERICHOST
# define NI_NUMERICHOST -1
#endif
CND(NI_NUMERICHOST, "Numeric form of the hostname")

#ifndef NI_MAXHOST
# define NI_MAXHOST -1
#endif
CND(NI_MAXHOST, "Maximum size of hostname")

#ifndef NI_MAXSERV
# define NI_MAXSERV -1
#endif
CND(NI_MAXSERV, "Maximum size of service name")

#ifndef EAI_SYSTEM
# define EAI_SYSTEM -1
#endif
CND(EAI_SYSTEM, "Check errno for details")

/*

   ------------------
   -- Socket modes --
   ------------------

*/

#ifndef SOCK_STREAM
# define SOCK_STREAM -1
#endif
CND(SOCK_STREAM, "Stream socket")

#ifndef SOCK_DGRAM
# define SOCK_DGRAM -1
#endif
CND(SOCK_DGRAM, "Datagram socket")

#ifndef SOCK_RAW
# define SOCK_RAW -1
#endif
CND(SOCK_RAW, "Raw socket")

/*

   -----------------
   -- Host errors --
   -----------------

*/

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

/*

   --------------------
   -- Shutdown modes --
   --------------------

*/

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

/*

   ---------------------
   -- Protocol levels --
   ---------------------

*/

#ifndef SOL_SOCKET
# define SOL_SOCKET -1
#endif
CND(SOL_SOCKET, "Options for socket level")

#ifndef IPPROTO_IP
# define IPPROTO_IP -1
#endif
CND(IPPROTO_IP, "Dummy protocol for IP")

#ifndef IPPROTO_IPV6
# define IPPROTO_IPV6 -1
#endif
CND(IPPROTO_IPV6, "IPv6 socket option level")

#ifndef IPPROTO_UDP
# define IPPROTO_UDP -1
#endif
CND(IPPROTO_UDP, "UDP")

#ifndef IPPROTO_TCP
# define IPPROTO_TCP -1
#endif
CND(IPPROTO_TCP, "TCP")

#ifndef IPPROTO_ICMP
# define IPPROTO_ICMP -1
#endif
CND(IPPROTO_ICMP, "Internet Control Message Protocol")

#ifndef IPPROTO_IGMP
# define IPPROTO_IGMP -1
#endif
CND(IPPROTO_IGMP, "Internet Group Management Protocol")

#ifndef IPPROTO_IPIP
# define IPPROTO_IPIP -1
#endif
CND(IPPROTO_IPIP, "IPIP tunnels (older KA9Q tunnels use 94)")

#ifndef IPPROTO_EGP
# define IPPROTO_EGP -1
#endif
CND(IPPROTO_EGP, "Exterior Gateway Protocol")

#ifndef IPPROTO_PUP
# define IPPROTO_PUP -1
#endif
CND(IPPROTO_PUP, "PUP protocol")

#ifndef IPPROTO_IDP
# define IPPROTO_IDP -1
#endif
CND(IPPROTO_IDP, "XNS IDP protocol")

#ifndef IPPROTO_TP
# define IPPROTO_TP -1
#endif
CND(IPPROTO_TP, "SO Transport Protocol Class 4")

#ifndef IPPROTO_DCCP
# define IPPROTO_DCCP -1
#endif
CND(IPPROTO_DCCP, "Datagram Congestion Control Protocol")

#ifndef IPPROTO_RSVP
# define IPPROTO_RSVP -1
#endif
CND(IPPROTO_RSVP, "Reservation Protocol")

#ifndef IPPROTO_GRE
# define IPPROTO_GRE -1
#endif
CND(IPPROTO_GRE, "General Routing Encapsulation")

#ifndef IPPROTO_ESP
# define IPPROTO_ESP -1
#endif
CND(IPPROTO_ESP, "encapsulating security payload")

#ifndef IPPROTO_AH
# define IPPROTO_AH -1
#endif
CND(IPPROTO_AH, "authentication header")

#ifndef IPPROTO_MTP
# define IPPROTO_MTP -1
#endif
CND(IPPROTO_MTP, "Multicast Transport Protocol")

#ifndef IPPROTO_BEETPH
# define IPPROTO_BEETPH -1
#endif
CND(IPPROTO_BEETPH, "IP option pseudo header for BEET")

#ifndef IPPROTO_ENCAP
# define IPPROTO_ENCAP -1
#endif
CND(IPPROTO_ENCAP, "Encapsulation Header")

#ifndef IPPROTO_PIM
# define IPPROTO_PIM -1
#endif
CND(IPPROTO_PIM, "Protocol Independent Multicast")

#ifndef IPPROTO_COMP
# define IPPROTO_COMP -1
#endif
CND(IPPROTO_COMP, "Compression Header Protocol")

#ifndef IPPROTO_SCTP
# define IPPROTO_SCTP -1
#endif
CND(IPPROTO_SCTP, "Stream Control Transmission Protocol")

#ifndef IPPROTO_UDPLITE
# define IPPROTO_UDPLITE -1
#endif
CND(IPPROTO_UDPLITE, "UDP-Lite protocol")

#ifndef IPPROTO_MPLS
# define IPPROTO_MPLS -1
#endif
CND(IPPROTO_MPLS, "MPLS in IP")

#ifndef IPPROTO_RAW
# define IPPROTO_RAW -1
#endif
CND(IPPROTO_RAW, "Raw IP packets")

/*

   -------------------
   -- Request flags --
   -------------------

*/

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
#ifdef __MINWGW32__
/* The value of MSG_WAITALL is 8.  Nevertheless winsock.h doesn't
   define it, but it is still usable as we link to winsock2 API.  */
# define MSG_WAITALL (1 << 3)
#else
# define MSG_WAITALL -1
#endif
#endif
CND(MSG_WAITALL, "Wait for full reception")

#ifndef MSG_NOSIGNAL
# define MSG_NOSIGNAL -1
#endif
CND(MSG_NOSIGNAL, "No SIGPIPE on send")

#if defined (__linux__) || defined (__ANDROID__) || defined (__QNX__)
# define MSG_Forced_Flags "MSG_NOSIGNAL"
#else
# define MSG_Forced_Flags "0"
#endif
CNS(MSG_Forced_Flags, "")
/*
   --  Flags set on all send(2) calls
*/

/*

   --------------------
   -- Socket options --
   --------------------

*/

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

#ifndef SO_BUSY_POLL
# define SO_BUSY_POLL -1
#endif
CND(SO_BUSY_POLL, "Busy polling")

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

#ifndef IP_RECVERR
# define IP_RECVERR -1
#endif
CND(IP_RECVERR, "Extended reliable error message passing")

#ifndef IPV6_ADDRFORM
# define IPV6_ADDRFORM -1
#endif
CND(IPV6_ADDRFORM, "Turn IPv6 socket into different address family")

#ifndef IPV6_ADD_MEMBERSHIP
# define IPV6_ADD_MEMBERSHIP -1
#endif
CND(IPV6_ADD_MEMBERSHIP, "Join IPv6 multicast group")

#ifndef IPV6_DROP_MEMBERSHIP
# define IPV6_DROP_MEMBERSHIP -1
#endif
CND(IPV6_DROP_MEMBERSHIP, "Leave IPv6 multicast group")

#ifndef IPV6_MTU
# define IPV6_MTU -1
#endif
CND(IPV6_MTU, "Set/get MTU used for the socket")

#ifndef IPV6_MTU_DISCOVER
# define IPV6_MTU_DISCOVER -1
#endif
CND(IPV6_MTU_DISCOVER, "Control path-MTU discovery on the socket")

#ifndef IPV6_MULTICAST_HOPS
# define IPV6_MULTICAST_HOPS -1
#endif
CND(IPV6_MULTICAST_HOPS, "Set the multicast hop limit for the socket")

#ifndef IPV6_MULTICAST_IF
# define IPV6_MULTICAST_IF -1
#endif
CND(IPV6_MULTICAST_IF, "Set/get IPv6 mcast interface")

#ifndef IPV6_MULTICAST_LOOP
# define IPV6_MULTICAST_LOOP -1
#endif
CND(IPV6_MULTICAST_LOOP, "Set/get mcast loopback")

#ifndef IPV6_RECVPKTINFO
# define IPV6_RECVPKTINFO -1
#endif
CND(IPV6_RECVPKTINFO, "Set delivery of the IPV6_PKTINFO")

#ifndef IPV6_PKTINFO
# define IPV6_PKTINFO -1
#endif
CND(IPV6_PKTINFO, "Get IPv6datagram info")

#ifndef IPV6_RTHDR
# define IPV6_RTHDR -1
#endif
CND(IPV6_RTHDR, "Set the routing header delivery")

#ifndef IPV6_AUTHHDR
# define IPV6_AUTHHDR -1
#endif
CND(IPV6_AUTHHDR, "Set the authentication header delivery")

#ifndef IPV6_DSTOPTS
# define IPV6_DSTOPTS -1
#endif
CND(IPV6_DSTOPTS, "Set the destination options delivery")

#ifndef IPV6_HOPOPTS
# define IPV6_HOPOPTS -1
#endif
CND(IPV6_HOPOPTS, "Set the hop options delivery")

#ifndef IPV6_FLOWINFO
# define IPV6_FLOWINFO -1
#endif
CND(IPV6_FLOWINFO, "Set the flow ID delivery")

#ifndef IPV6_HOPLIMIT
# define IPV6_HOPLIMIT -1
#endif
CND(IPV6_HOPLIMIT, "Set the hop count of the packet delivery")

#ifndef IPV6_RECVERR
# define IPV6_RECVERR -1
#endif
CND(IPV6_RECVERR, "Extended reliable error message passing")

#ifndef IPV6_ROUTER_ALERT
# define IPV6_ROUTER_ALERT -1
#endif
CND(IPV6_ROUTER_ALERT, "Pass forwarded router alert hop-by-hop option")

#ifndef IPV6_UNICAST_HOPS
# define IPV6_UNICAST_HOPS -1
#endif
CND(IPV6_UNICAST_HOPS, "Set the unicast hop limit")

#ifndef IPV6_V6ONLY
# define IPV6_V6ONLY -1
#endif
CND(IPV6_V6ONLY, "Restricted to IPv6 communications only")

/*

   ----------------------
   -- Type definitions --
   ----------------------

*/

{
  struct timeval tv;
/*
   --  Sizes (in bytes) of the components of struct timeval
*/
#define SIZEOF_tv_sec (sizeof tv.tv_sec)
CND(SIZEOF_tv_sec, "tv_sec")
#define SIZEOF_tv_usec (sizeof tv.tv_usec)
CND(SIZEOF_tv_usec, "tv_usec")
/*

   --  Maximum allowed value for tv_sec
*/

/**
 ** On Solaris, field tv_sec in struct timeval has an undocumented
 ** hard-wired limit of 100 million.
 ** On IA64 HP-UX the limit is 2**31 - 1.
 **/
#if defined (__sun__)
# define MAX_tv_sec "100_000_000"

#elif defined (__hpux__)
# define MAX_tv_sec "16#7fffffff#"

#else
# define MAX_tv_sec "2 ** (SIZEOF_tv_sec * 8 - 1) - 1"
#endif
CNS(MAX_tv_sec, "")
}
/*

   --  Sizes of various data types
*/

#define SIZEOF_sockaddr_in (sizeof (struct sockaddr_in))
CND(SIZEOF_sockaddr_in, "struct sockaddr_in")
#ifdef HAVE_AF_INET6
# define SIZEOF_sockaddr_in6 (sizeof (struct sockaddr_in6))
#else
# define SIZEOF_sockaddr_in6 0
#endif
CND(SIZEOF_sockaddr_in6, "struct sockaddr_in6")

/**
 ** The sockaddr_un structure is not defined in MINGW C headers
 ** but Windows supports it from build 17063.
 **/
#if defined(__MINGW32__)
struct sockaddr_un {
  ADDRESS_FAMILY sun_family;    /* AF_UNIX */
  char           sun_path[108]; /* Pathname */
};
#endif
#define SIZEOF_sockaddr_un (sizeof (struct sockaddr_un))
CND(SIZEOF_sockaddr_un, "struct sockaddr_un")

#define SIZEOF_fd_set (sizeof (fd_set))
CND(SIZEOF_fd_set, "fd_set")
CND(FD_SETSIZE, "Max fd value")

#define SIZEOF_struct_hostent (sizeof (struct hostent))
CND(SIZEOF_struct_hostent, "struct hostent")

#define SIZEOF_struct_servent (sizeof (struct servent))
CND(SIZEOF_struct_servent, "struct servent")

#if defined (__linux__) || defined (__ANDROID__) || defined (__QNX__)
#define SIZEOF_sigset (sizeof (sigset_t))
CND(SIZEOF_sigset, "sigset")
#endif

#if defined(_WIN32) || defined(__vxworks)
#define SIZEOF_socklen_t sizeof (size_t)
#else
#define SIZEOF_socklen_t sizeof (socklen_t)
#endif
CND(SIZEOF_socklen_t, "Size of socklen_t");

#ifndef IF_NAMESIZE
#ifdef IF_MAX_STRING_SIZE
#define IF_NAMESIZE IF_MAX_STRING_SIZE
#else
#define IF_NAMESIZE -1
#endif
#endif
CND(IF_NAMESIZE, "Max size of interface name with 0 terminator");

/*

   --  Fields of struct msghdr
*/

#if defined (__sun__) || defined (__hpux__)
# define Msg_Iovlen_T "Interfaces.C.int"
#else
# define Msg_Iovlen_T "Interfaces.C.size_t"
#endif

SUB(Msg_Iovlen_T)

/*

   ----------------------------------------
   -- Properties of supported interfaces --
   ----------------------------------------

*/

CND(Need_Netdb_Buffer, "Need buffer for Netdb ops")
CND(Need_Netdb_Lock,   "Need lock for Netdb ops")
CND(Has_Sockaddr_Len,  "Sockaddr has sa_len field")

/**
 ** Do not change the format of the line below without also updating the
 ** MaRTE Makefile.
 **/
C("Thread_Blocking_IO", Boolean, "True", "")
/*
   --  Set False for contexts where socket i/o are process blocking

*/

#ifdef HAVE_INET_PTON
# define Inet_Pton_Linkname "inet_pton"
#else
# define Inet_Pton_Linkname "__gnat_inet_pton"
#endif
CST(Inet_Pton_Linkname, "")

#ifdef HAVE_INET_NTOP
# define Inet_Ntop_Linkname "inet_ntop"
#else
# define Inet_Ntop_Linkname "__gnat_inet_ntop"
#endif
CST(Inet_Ntop_Linkname, "")

#endif /* HAVE_SOCKETS */

/*

   ---------------------
   -- Threads support --
   ---------------------

   --  Clock identifier definitions

*/

/* Note: On HP-UX, CLOCK_REALTIME is an enum, not a macro. */

#if !(defined(CLOCK_REALTIME) || defined (__hpux__))
# define CLOCK_REALTIME (-1)
#endif
CND(CLOCK_REALTIME, "System realtime clock")

#ifdef CLOCK_MONOTONIC
CND(CLOCK_MONOTONIC, "System monotonic clock")
#endif

#ifdef CLOCK_FASTEST
CND(CLOCK_FASTEST, "Fastest clock")
#endif

#ifndef CLOCK_THREAD_CPUTIME_ID
# define CLOCK_THREAD_CPUTIME_ID -1
#endif
CND(CLOCK_THREAD_CPUTIME_ID, "Thread CPU clock")

#if defined(__linux__) || defined(__FreeBSD__) \
 || (defined(_AIX) && defined(_AIXVERSION_530)) \
 || defined(__DragonFly__) || defined(__QNX__)
/** On these platforms use system provided monotonic clock instead of
 ** the default CLOCK_REALTIME. We then need to set up cond var attributes
 ** appropriately (see thread.c).
 **
 ** Note that AIX 5.2 does not support CLOCK_MONOTONIC timestamps for
 ** pthread_cond_timedwait (and does not have pthread_condattr_setclock),
 ** hence the conditionalization on AIX version above). _AIXVERSION_530
 ** is defined in AIX 5.3 and more recent versions.
 **/
# define CLOCK_RT_Ada "CLOCK_MONOTONIC"

#else
/* By default use CLOCK_REALTIME */
# define CLOCK_RT_Ada "CLOCK_REALTIME"
#endif

#ifdef CLOCK_RT_Ada
CNS(CLOCK_RT_Ada, "")
#endif

#if defined (__APPLE__) || defined (__linux__) || defined (__ANDROID__) \
  || defined (__QNX__) || defined (__rtems__) || defined (DUMMY)
/*

   --  Sizes of pthread data types
*/

#if defined (__APPLE__) || defined (DUMMY)
/*
   --  (on Darwin, these are just placeholders)
*/
#define PTHREAD_SIZE            __PTHREAD_SIZE__
#define PTHREAD_ATTR_SIZE       __PTHREAD_ATTR_SIZE__
#define PTHREAD_MUTEXATTR_SIZE  __PTHREAD_MUTEXATTR_SIZE__
#define PTHREAD_MUTEX_SIZE      __PTHREAD_MUTEX_SIZE__
#define PTHREAD_CONDATTR_SIZE   __PTHREAD_CONDATTR_SIZE__
#define PTHREAD_COND_SIZE       __PTHREAD_COND_SIZE__
#define PTHREAD_RWLOCKATTR_SIZE __PTHREAD_RWLOCKATTR_SIZE__
#define PTHREAD_RWLOCK_SIZE     __PTHREAD_RWLOCK_SIZE__
#define PTHREAD_ONCE_SIZE       __PTHREAD_ONCE_SIZE__
#else
#define PTHREAD_SIZE            (sizeof (pthread_t))
#define PTHREAD_ATTR_SIZE       (sizeof (pthread_attr_t))
#define PTHREAD_MUTEXATTR_SIZE  (sizeof (pthread_mutexattr_t))
#define PTHREAD_MUTEX_SIZE      (sizeof (pthread_mutex_t))
#define PTHREAD_CONDATTR_SIZE   (sizeof (pthread_condattr_t))
#define PTHREAD_COND_SIZE       (sizeof (pthread_cond_t))
#define PTHREAD_RWLOCKATTR_SIZE (sizeof (pthread_rwlockattr_t))
#define PTHREAD_RWLOCK_SIZE     (sizeof (pthread_rwlock_t))
#define PTHREAD_ONCE_SIZE       (sizeof (pthread_once_t))
#endif
/*

*/
CND(PTHREAD_SIZE,            "pthread_t")
CND(PTHREAD_ATTR_SIZE,       "pthread_attr_t")
CND(PTHREAD_MUTEXATTR_SIZE,  "pthread_mutexattr_t")
CND(PTHREAD_MUTEX_SIZE,      "pthread_mutex_t")
CND(PTHREAD_CONDATTR_SIZE,   "pthread_condattr_t")
CND(PTHREAD_COND_SIZE,       "pthread_cond_t")
CND(PTHREAD_RWLOCKATTR_SIZE, "pthread_rwlockattr_t")
CND(PTHREAD_RWLOCK_SIZE,     "pthread_rwlock_t")
CND(PTHREAD_ONCE_SIZE,       "pthread_once_t")

#endif /* __APPLE__ || __linux__ || __ANDROID__ || __rtems__ */

/*

   --------------------------------
   -- File and directory support --
   --------------------------------

*/

/**
 ** Note: this constant can be used in the GNAT runtime library. In compiler
 ** units on the other hand, System.OS_Constants is not available, so we
 ** declare an Ada constant (Osint.File_Attributes_Size) independently, which
 ** is at least as large as sizeof (struct file_attributes), and we have an
 ** assertion at initialization of Osint checking that the size is indeed at
 ** least sufficient.
 **/
#define SIZEOF_struct_file_attributes (sizeof (struct file_attributes))
CND(SIZEOF_struct_file_attributes, "struct file_attributes")

/**
 ** Maximal size of buffer for struct dirent. Note: Since POSIX.1 does not
 ** specify the size of the d_name field, and other nonstandard fields may
 ** precede that field within the dirent structure, we must make a conservative
 ** computation.
 **/
{
  struct dirent dent;
#define SIZEOF_struct_dirent_alloc \
  ((char*) &dent.d_name - (char*) &dent) + NAME_MAX + 1
CND(SIZEOF_struct_dirent_alloc, "struct dirent allocation")
}

/**
 **  System-specific constants follow
 **  Each section should be activated if compiling for the corresponding
 **  platform *or* generating the dummy version for runtime test compilation.
 **/

#if defined (__vxworks) || defined (DUMMY)

/*

   --------------------------------
   -- VxWorks-specific constants --
   --------------------------------

   --  These constants may be used only within the VxWorks version of
   --  GNAT.Sockets.Thin.
*/

CND(OK,    "VxWorks generic success")
CND(ERROR, "VxWorks generic error")

#endif /* __vxworks */

#if defined (__MINGW32__) || defined (DUMMY)
/*

   ------------------------------
   -- MinGW-specific constants --
   ------------------------------

   --  These constants may be used only within the MinGW version of
   --  GNAT.Sockets.Thin.
*/

CND(WSASYSNOTREADY,     "System not ready")
CND(WSAVERNOTSUPPORTED, "Version not supported")
CND(WSANOTINITIALISED,  "Winsock not initialized")
CND(WSAEDISCON,         "Disconnected")

#endif /* __MINGW32__ */

/**
 ** End of constants definitions
 **/

#ifdef NATIVE
   putchar ('\n');
#endif

/*

end System.OS_Constants;
*/
}
