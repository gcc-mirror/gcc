/* Get common system includes and various definitions and declarations based
   on autoconf macros.
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
extern int fputs_unlocked PARAMS ((const char *, FILE *));
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

#if defined (STDC_HEADERS) || (!defined (isascii) && !defined (HAVE_ISASCII)) || defined(HOST_EBCDIC)
# define IN_CTYPE_DOMAIN(c) 1
#else
# define IN_CTYPE_DOMAIN(c) isascii(c)
#endif

/* The ctype functions are often implemented as macros which do
   lookups in arrays using the parameter as the offset.  If the ctype
   function parameter is a char, then gcc will (appropriately) warn
   that a "subscript has type char".  Using a (signed) char as a subscript
   is bad because you may get negative offsets and thus it is not 8-bit
   safe.  The CTYPE_CONV macro ensures that the parameter is cast to an
   unsigned char when a char is passed in.  When an int is passed in, the
   parameter is left alone so we don't lose EOF.
*/

#define CTYPE_CONV(CH) \
  (sizeof(CH) == sizeof(unsigned char) ? (int)(unsigned char)(CH) : (int)(CH))


/* WARNING!  The argument to the ctype replacement macros below is
   evaluated more than once so it must not have side effects!  */

#ifdef isblank
# define ISBLANK(c) (IN_CTYPE_DOMAIN (c) && isblank (CTYPE_CONV(c)))
#else
# define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#endif
#ifdef isgraph
# define ISGRAPH(c) (IN_CTYPE_DOMAIN (c) && isgraph (CTYPE_CONV(c)))
#else
# define ISGRAPH(c) (IN_CTYPE_DOMAIN (c) && isprint (CTYPE_CONV(c)) && !isspace (CTYPE_CONV(c)))
#endif

#define ISPRINT(c) (IN_CTYPE_DOMAIN (c) && isprint (CTYPE_CONV(c)))
#define ISALNUM(c) (IN_CTYPE_DOMAIN (c) && isalnum (CTYPE_CONV(c)))
#define ISALPHA(c) (IN_CTYPE_DOMAIN (c) && isalpha (CTYPE_CONV(c)))
#define ISCNTRL(c) (IN_CTYPE_DOMAIN (c) && iscntrl (CTYPE_CONV(c)))
#define ISLOWER(c) (IN_CTYPE_DOMAIN (c) && islower (CTYPE_CONV(c)))
#define ISPUNCT(c) (IN_CTYPE_DOMAIN (c) && ispunct (CTYPE_CONV(c)))
#define ISSPACE(c) (IN_CTYPE_DOMAIN (c) && isspace (CTYPE_CONV(c)))
#define ISUPPER(c) (IN_CTYPE_DOMAIN (c) && isupper (CTYPE_CONV(c)))
#define ISXDIGIT(c) (IN_CTYPE_DOMAIN (c) && isxdigit (CTYPE_CONV(c)))
#define ISDIGIT_LOCALE(c) (IN_CTYPE_DOMAIN (c) && isdigit (CTYPE_CONV(c)))

#if STDC_HEADERS
# define TOLOWER(c) (tolower (CTYPE_CONV(c)))
# define TOUPPER(c) (toupper (CTYPE_CONV(c)))
#else
# define TOLOWER(c) (ISUPPER (c) ? tolower (CTYPE_CONV(c)) : (c))
# define TOUPPER(c) (ISLOWER (c) ? toupper (CTYPE_CONV(c)) : (c))
#endif

/* ISDIGIT differs from ISDIGIT_LOCALE, as follows:
   - Its arg may be any int or unsigned int; it need not be an unsigned char.
   - It's guaranteed to evaluate its argument exactly once.
   - It's typically faster.
   Posix 1003.2-1992 section 2.5.2.1 page 50 lines 1556-1558 says that
   only '0' through '9' are digits.  Prefer ISDIGIT to ISDIGIT_LOCALE unless
   it's important to use the locale's definition of `digit' even when the
   host does not conform to Posix.  */
#define ISDIGIT(c) ((unsigned) (c) - '0' <= 9)

/* Define a default escape character; its different for EBCDIC.  */
#ifndef TARGET_ESC
#define TARGET_ESC 033
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

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
# ifdef USE_C_ALLOCA
/* Note that systems that use glibc have a <stdlib.h> that includes
   <alloca.h> that defines alloca, so let USE_C_ALLOCA override this. */
# undef alloca
#endif
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

/* Returns the least number N such that N * Y >= X.  */
#define CEIL(x,y) (((x) + (y) - 1) / (y))

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
extern void bcopy PARAMS ((const PTR, PTR, size_t));
#  endif
# else /* ! HAVE_BCOPY */
#  define bcopy(src,dst,len) memmove((dst),(src),(len))
# endif
#endif

#ifndef bcmp
# ifdef HAVE_BCMP
#  ifdef NEED_DECLARATION_BCMP
extern int bcmp PARAMS ((const PTR, const PTR, size_t));
#  endif
# else /* ! HAVE_BCMP */
#  define bcmp(left,right,len) memcmp ((left),(right),(len))
# endif
#endif

#ifndef bzero
# ifdef HAVE_BZERO
#  ifdef NEED_DECLARATION_BZERO
extern void bzero PARAMS ((PTR, size_t));
#  endif
# else /* ! HAVE_BZERO */
#  define bzero(dst,len) memset ((dst),0,(len))
# endif
#endif

#ifndef index
# ifdef HAVE_INDEX
#  ifdef NEED_DECLARATION_INDEX
extern char *index PARAMS ((const char *, int));
#  endif
# else /* ! HAVE_INDEX */
#  define index strchr
# endif
#endif

#ifndef rindex
# ifdef HAVE_RINDEX
#  ifdef NEED_DECLARATION_RINDEX
extern char *rindex PARAMS ((const char *, int));
#  endif
# else /* ! HAVE_RINDEX */
#  define rindex strrchr
# endif
#endif

#ifdef NEED_DECLARATION_ATOF
extern double atof PARAMS ((const char *));
#endif

#ifdef NEED_DECLARATION_ATOL
extern long atol PARAMS ((const char *));
#endif

#ifdef NEED_DECLARATION_FREE
extern void free PARAMS ((PTR));
#endif

#ifdef NEED_DECLARATION_GETCWD
extern char *getcwd PARAMS ((char *, size_t));
#endif

#ifdef NEED_DECLARATION_GETENV
extern char *getenv PARAMS ((const char *));
#endif

#ifdef NEED_DECLARATION_GETWD
extern char *getwd PARAMS ((char *));
#endif

#ifdef NEED_DECLARATION_SBRK
extern PTR sbrk PARAMS ((int));
#endif

#ifdef NEED_DECLARATION_STRSTR
extern char *strstr PARAMS ((const char *, const char *));
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef NEED_DECLARATION_MALLOC
extern PTR malloc PARAMS ((size_t));
#endif

#ifdef NEED_DECLARATION_CALLOC
extern PTR calloc PARAMS ((size_t, size_t));
#endif

#ifdef NEED_DECLARATION_REALLOC
extern PTR realloc PARAMS ((PTR, size_t));
#endif

/* If the system doesn't provide strsignal, we get it defined in
   libiberty but no declaration is supplied. */
#ifdef NEED_DECLARATION_STRSIGNAL
# ifndef strsignal
extern const char *strsignal PARAMS ((int));
# endif
#endif

#ifdef HAVE_GETRLIMIT
# ifdef NEED_DECLARATION_GETRLIMIT
#  ifndef getrlimit
#   ifdef ANSI_PROTOTYPES
struct rlimit;
#   endif
extern int getrlimit PARAMS ((int, struct rlimit *));
#  endif
# endif
#endif

#ifdef HAVE_SETRLIMIT
# ifdef NEED_DECLARATION_SETRLIMIT
#  ifndef setrlimit
#   ifdef ANSI_PROTOTYPES
struct rlimit;
#   endif
extern int setrlimit PARAMS ((int, const struct rlimit *));
#  endif
# endif
#endif

/* HAVE_VOLATILE only refers to the stage1 compiler.  We also check
   __STDC__ and assume gcc sets it and has volatile in stage >=2. */
#if !defined(HAVE_VOLATILE) && !defined(__STDC__) && !defined(volatile)
#define volatile
#endif

#ifdef NEED_DECLARATION_ABORT
extern void abort PARAMS ((void));
#endif

/* Define a STRINGIFY macro that's right for ANSI or traditional C.
   Note: if the argument passed to STRINGIFY is itself a macro, eg
   #define foo bar, STRINGIFY(foo) will produce "foo", not "bar".
   Although the __STDC__ case could be made to expand this via a layer
   of indirection, the traditional C case can not do so.  Therefore
   this behavior is not supported. */
#ifndef STRINGIFY
# ifdef HAVE_STRINGIZE
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

/* Provide a way to print an address via printf.  */
#ifndef HOST_PTR_PRINTF
# ifdef HAVE_PRINTF_PTR
#  define HOST_PTR_PRINTF "%p"
# else
#  define HOST_PTR_PRINTF \
    (sizeof (int) == sizeof (char *) ? "%x" \
     : sizeof (long) == sizeof (char *) ? "%lx" : "%llx")
# endif
#endif /* ! HOST_PTR_PRINTF */

/* By default, colon separates directories in a path.  */
#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Define IS_DIR_SEPARATOR.  */
#ifndef DIR_SEPARATOR_2
# define IS_DIR_SEPARATOR(ch) ((ch) == DIR_SEPARATOR)
#else /* DIR_SEPARATOR_2 */
# define IS_DIR_SEPARATOR(ch) \
	(((ch) == DIR_SEPARATOR) || ((ch) == DIR_SEPARATOR_2))
#endif /* DIR_SEPARATOR_2 */

/* Get libiberty declarations. */
#include "libiberty.h"

/* Enumerated bitfields are safe to use unless we've been explictly told
 * otherwise or if they are signed. */
 
#define USE_ENUM_BITFIELDS __GNUC__ || (!ONLY_INT_FIELDS && ENUM_BTIFIELDS_ARE_UNSIGNED)

#if USE_ENUM_BITFIELDS
#define ENUM_BITFIELD(TYPE) enum TYPE
#else
#define ENUM_BITFIELD(TYPE) unsigned int
#endif


#endif /* __GCC_SYSTEM_H__ */
