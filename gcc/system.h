/* system.h - Get common system includes and various definitions and
   declarations based on autoconf macros.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef __GCC_SYSTEM_H__
#define __GCC_SYSTEM_H__

/* This is the location of the online document giving information how
   to report bugs. If you change this string, also check for strings
   not under control of the preprocessor.  */
#define GCCBUGURL "<URL:http://www.gnu.org/software/gcc/bugs.html>"

/* We must include stdarg.h/varargs.h before stdio.h. */
#ifdef ANSI_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>

/* Define a generic NULL if one hasn't already been defined.  */
#ifndef NULL
#define NULL 0
#endif

/* The compiler is not a multi-threaded application and therefore we
   do not have to use the locking functions.

   NEED_DECLARATION_PUTC_UNLOCKED actually indicates whether or not
   the IO code is multi-thread safe by default.  If it is not declared,
   then do not worry about using the _unlocked functions.
   
   fputs_unlocked is an extension and needs to be prototyped specially.  */

#if defined HAVE_PUTC_UNLOCKED && !defined NEED_DECLARATION_PUTC_UNLOCKED
# undef putc
# define putc(C, Stream) putc_unlocked (C, Stream)
#endif
#if defined HAVE_FPUTC_UNLOCKED && !defined NEED_DECLARATION_PUTC_UNLOCKED
# undef fputc
# define fputc(C, Stream) fputc_unlocked (C, Stream)
#endif
#if defined HAVE_FPUTS_UNLOCKED && !defined NEED_DECLARATION_PUTC_UNLOCKED
# undef fputs
# define fputs(String, Stream) fputs_unlocked (String, Stream)
# ifdef NEED_DECLARATION_FPUTS_UNLOCKED
extern int fputs_unlocked PROTO ((const char *, FILE *));
# endif
#endif

#include <ctype.h>

/* Jim Meyering writes:

   "... Some ctype macros are valid only for character codes that
   isascii says are ASCII (SGI's IRIX-4.0.5 is one such system --when
   using /bin/cc or gcc but without giving an ansi option).  So, all
   ctype uses should be through macros like ISPRINT...  If
   STDC_HEADERS is defined, then autoconf has verified that the ctype
   macros don't need to be guarded with references to isascii. ...
   Defining isascii to 1 should let any compiler worth its salt
   eliminate the && through constant folding."

   Bruno Haible adds:

   "... Furthermore, isupper(c) etc. have an undefined result if c is
   outside the range -1 <= c <= 255. One is tempted to write isupper(c)
   with c being of type `char', but this is wrong if c is an 8-bit
   character >= 128 which gets sign-extended to a negative value.
   The macro ISUPPER protects against this as well."  */

#if defined (STDC_HEADERS) || (!defined (isascii) && !defined (HAVE_ISASCII))
# define IN_CTYPE_DOMAIN(c) 1
#else
# define IN_CTYPE_DOMAIN(c) isascii(c)
#endif

#ifdef isblank
# define ISBLANK(c) (IN_CTYPE_DOMAIN (c) && isblank (c))
#else
# define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#endif
#ifdef isgraph
# define ISGRAPH(c) (IN_CTYPE_DOMAIN (c) && isgraph (c))
#else
# define ISGRAPH(c) (IN_CTYPE_DOMAIN (c) && isprint (c) && !isspace (c))
#endif

#define ISPRINT(c) (IN_CTYPE_DOMAIN (c) && isprint (c))
#define ISALNUM(c) (IN_CTYPE_DOMAIN (c) && isalnum (c))
#define ISALPHA(c) (IN_CTYPE_DOMAIN (c) && isalpha (c))
#define ISCNTRL(c) (IN_CTYPE_DOMAIN (c) && iscntrl (c))
#define ISLOWER(c) (IN_CTYPE_DOMAIN (c) && islower (c))
#define ISPUNCT(c) (IN_CTYPE_DOMAIN (c) && ispunct (c))
#define ISSPACE(c) (IN_CTYPE_DOMAIN (c) && isspace (c))
#define ISUPPER(c) (IN_CTYPE_DOMAIN (c) && isupper (c))
#define ISXDIGIT(c) (IN_CTYPE_DOMAIN (c) && isxdigit (c))
#define ISDIGIT_LOCALE(c) (IN_CTYPE_DOMAIN (c) && isdigit (c))

/* ISDIGIT differs from ISDIGIT_LOCALE, as follows:
   - Its arg may be any int or unsigned int; it need not be an unsigned char.
   - It's guaranteed to evaluate its argument exactly once.
   - It's typically faster.
   Posix 1003.2-1992 section 2.5.2.1 page 50 lines 1556-1558 says that
   only '0' through '9' are digits.  Prefer ISDIGIT to ISDIGIT_LOCALE unless
   it's important to use the locale's definition of `digit' even when the
   host does not conform to Posix.  */
#define ISDIGIT(c) ((unsigned) (c) - '0' <= 9)


#include <sys/types.h>
#include <errno.h>

#ifndef errno
extern int errno;
#endif

#ifdef STRING_WITH_STRINGS
# include <string.h>
# include <strings.h>
#else
# ifdef HAVE_STRING_H
#  include <string.h>
# else
#  ifdef HAVE_STRINGS_H
#   include <strings.h>
#  endif
# endif
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif

#if HAVE_LIMITS_H
# include <limits.h>
#endif

/* Find HOST_WIDEST_INT and set its bit size, type and print macros.
   It will be the largest integer mode supported by the host which may
   (or may not) be larger than HOST_WIDE_INT.  This must appear after
   <limits.h> since we only use `long long' if its bigger than a
   `long' and also if it is supported by macros in limits.h.  For old
   hosts which don't have a limits.h (and thus won't include it in
   stage2 cause we don't rerun configure) we assume gcc supports long
   long.)  Note, you won't get these defined if you don't include
   {ht}config.h before this file to set the HOST_BITS_PER_* macros. */

#ifndef HOST_WIDEST_INT
# if defined (HOST_BITS_PER_LONG) && defined (HOST_BITS_PER_LONGLONG)
#  if (HOST_BITS_PER_LONGLONG > HOST_BITS_PER_LONG) && (defined (LONG_LONG_MAX) || defined (LONGLONG_MAX) || defined (LLONG_MAX) || defined (__GNUC__))
#   define HOST_BITS_PER_WIDEST_INT HOST_BITS_PER_LONGLONG
#   define HOST_WIDEST_INT long long
#   define HOST_WIDEST_INT_PRINT_DEC "%lld"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED "%llu"
#   define HOST_WIDEST_INT_PRINT_HEX "0x%llx"
#  else
#   define HOST_BITS_PER_WIDEST_INT HOST_BITS_PER_LONG
#   define HOST_WIDEST_INT long
#   define HOST_WIDEST_INT_PRINT_DEC "%ld"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED "%lu"
#   define HOST_WIDEST_INT_PRINT_HEX "0x%lx"
#  endif /*(long long>long) && (LONG_LONG_MAX||LONGLONG_MAX||LLONG_MAX||GNUC)*/
# endif /* defined(HOST_BITS_PER_LONG) && defined(HOST_BITS_PER_LONGLONG) */
#endif /* ! HOST_WIDEST_INT */

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  ifdef HAVE_TIME_H
#   include <time.h>
#  endif
# endif
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# ifdef HAVE_SYS_FILE_H
#  include <sys/file.h>
# endif
#endif

#ifndef SEEK_SET
# define SEEK_SET 0
# define SEEK_CUR 1
# define SEEK_END 2
#endif
#ifndef F_OK
# define F_OK 0
# define X_OK 1
# define W_OK 2
# define R_OK 4
#endif
#ifndef O_RDONLY
# define O_RDONLY 0
#endif
#ifndef O_WRONLY
# define O_WRONLY 1
#endif

/* Some systems define these in, e.g., param.h.  We undefine these names
   here to avoid the warnings.  We prefer to use our definitions since we
   know they are correct.  */

#undef MIN
#undef MAX
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef WIFSIGNALED
#define WIFSIGNALED(S) (((S) & 0xff) != 0 && ((S) & 0xff) != 0x7f)
#endif
#ifndef WTERMSIG
#define WTERMSIG(S) ((S) & 0x7f)
#endif
#ifndef WIFEXITED
#define WIFEXITED(S) (((S) & 0xff) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(S) (((S) & 0xff00) >> 8)
#endif
#ifndef WSTOPSIG
#define WSTOPSIG WEXITSTATUS
#endif



#ifndef bcopy
# ifdef HAVE_BCOPY
#  ifdef NEED_DECLARATION_BCOPY
extern void bcopy ();
#  endif
# else /* ! HAVE_BCOPY */
#  define bcopy(src,dst,len) memmove((dst),(src),(len))
# endif
#endif

#ifndef bcmp
# ifdef HAVE_BCMP
#  ifdef NEED_DECLARATION_BCMP
extern int bcmp ();
#  endif
# else /* ! HAVE_BCMP */
#  define bcmp(left,right,len) memcmp ((left),(right),(len))
# endif
#endif

#ifndef bzero
# ifdef HAVE_BZERO
#  ifdef NEED_DECLARATION_BZERO
extern void bzero ();
#  endif
# else /* ! HAVE_BZERO */
#  define bzero(dst,len) memset ((dst),0,(len))
# endif
#endif

#ifndef index
# ifdef HAVE_INDEX
#  ifdef NEED_DECLARATION_INDEX
extern char *index ();
#  endif
# else /* ! HAVE_INDEX */
#  define index strchr
# endif
#endif

#ifndef rindex
# ifdef HAVE_RINDEX
#  ifdef NEED_DECLARATION_RINDEX
extern char *rindex ();
#  endif
# else /* ! HAVE_RINDEX */
#  define rindex strrchr
# endif
#endif

#ifdef NEED_DECLARATION_ATOF
extern double atof ();
#endif

#ifdef NEED_DECLARATION_ATOL
extern long atol();
#endif

#ifdef NEED_DECLARATION_FREE
extern void free ();
#endif

#ifdef NEED_DECLARATION_GETCWD
extern char *getcwd ();
#endif

#ifdef NEED_DECLARATION_GETENV
extern char *getenv ();
#endif

#ifdef NEED_DECLARATION_GETWD
extern char *getwd ();
#endif

#ifdef NEED_DECLARATION_SBRK
extern char *sbrk ();
#endif

#ifdef NEED_DECLARATION_STRSTR
extern char *strstr ();
#endif

#ifdef HAVE_STRERROR
# ifdef NEED_DECLARATION_STRERROR
#  ifndef strerror
extern char *strerror ();
#  endif
# endif
#else /* ! HAVE_STRERROR */
extern int sys_nerr;
extern char *sys_errlist[];
#endif /* HAVE_STRERROR */

#ifdef HAVE_STRSIGNAL
# ifdef NEED_DECLARATION_STRSIGNAL
#  ifndef strsignal
extern char * strsignal ();
#  endif
# endif
#else /* ! HAVE_STRSIGNAL */
# ifndef SYS_SIGLIST_DECLARED
#  ifndef NO_SYS_SIGLIST
extern char * sys_siglist[];
#  endif
# endif
#endif /* HAVE_STRSIGNAL */

#ifdef HAVE_GETRLIMIT
# ifdef NEED_DECLARATION_GETRLIMIT
#  ifndef getrlimit
extern int getrlimit ();
#  endif
# endif
#endif

#ifdef HAVE_SETRLIMIT
# ifdef NEED_DECLARATION_SETRLIMIT
#  ifndef setrlimit
extern int setrlimit ();
#  endif
# endif
#endif

/* HAVE_VOLATILE only refers to the stage1 compiler.  We also check
   __STDC__ and assume gcc sets it and has volatile in stage >=2. */
#if !defined(HAVE_VOLATILE) && !defined(__STDC__) && !defined(volatile)
#define volatile
#endif

/* Redefine abort to report an internal error w/o coredump, and reporting the
   location of the error in the source file.
   Some files undefine abort again, so we must prototype the real thing
   for their sake.  */
#ifdef NEED_DECLARATION_ABORT
extern void abort ();
#endif
extern void fatal PVPROTO((const char *, ...)) ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;

#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
#define abort() fatal ("Internal compiler error at %s:%d\n", \
		       trim_filename (__FILE__), __LINE__)
#else
#define abort() fatal ("Internal compiler error in `%s', at %s:%d\n"	\
  "Please submit a full bug report.\n"	\
  "See %s for instructions.", \
  __PRETTY_FUNCTION__, trim_filename (__FILE__), __LINE__, GCCBUGURL)
#endif /* recent gcc */

/* trim_filename is in toplev.c.  Define a stub macro for files that
   don't link toplev.c.  toplev.h will reset it to the real version.  */
#define trim_filename(x) (x)

/* Define a STRINGIFY macro that's right for ANSI or traditional C.
   HAVE_CPP_STRINGIFY only refers to the stage1 compiler.  Assume that
   (non-traditional) gcc used in stage2 or later has this feature.

   Note: if the argument passed to STRINGIFY is itself a macro, eg
   #define foo bar, STRINGIFY(foo) will produce "foo", not "bar".
   Although the __STDC__ case could be made to expand this via a layer
   of indirection, the traditional C case can not do so.  Therefore
   this behavior is not supported. */
#ifndef STRINGIFY
# if defined(HAVE_CPP_STRINGIFY) || (defined(__GNUC__) && defined(__STDC__))
#  define STRINGIFY(STRING) #STRING
# else
#  define STRINGIFY(STRING) "STRING"
# endif
#endif /* ! STRINGIFY */

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

/* Test if something is a normal file.  */
#ifndef S_ISREG
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

/* Test if something is a directory.  */
#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

/* Test if something is a character special file.  */
#ifndef S_ISCHR
#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#endif

/* Test if something is a socket.  */
#ifndef S_ISSOCK
# ifdef S_IFSOCK
#   define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
# else
#   define S_ISSOCK(m) 0
# endif
#endif

/* Test if something is a FIFO.  */
#ifndef S_ISFIFO
# ifdef S_IFIFO
#  define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
# else
#  define S_ISFIFO(m) 0
# endif
#endif

/* Approximate O_NONBLOCK.  */
#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif

/* Approximate O_NOCTTY.  */
#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif

/* Define well known filenos if the system does not define them.  */
#ifndef STDIN_FILENO
# define STDIN_FILENO   0
#endif
#ifndef STDOUT_FILENO
# define STDOUT_FILENO  1
#endif
#ifndef STDERR_FILENO
# define STDERR_FILENO  2
#endif

/* Some systems have mkdir that takes a single argument. */
#ifdef MKDIR_TAKES_ONE_ARG
# define mkdir(a,b) mkdir(a)
#endif

/* Get libiberty declarations. */
#include "libiberty.h"

#endif /* __GCC_SYSTEM_H__ */
