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

#ifndef va_copy
# ifdef __va_copy
#   define va_copy(d,s)  __va_copy((d),(s))
# else
#   define va_copy(d,s)  ((d) = (s))
# endif
#endif

#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif

#include <stdio.h>

/* Define a generic NULL if one hasn't already been defined.  */
#ifndef NULL
#define NULL 0
#endif

/* The compiler is not a multi-threaded application and therefore we
   do not have to use the locking functions.

   HAVE_DECL_PUTC_UNLOCKED actually indicates whether or not the IO
   code is multi-thread safe by default.  If it is set to 0, then do
   not worry about using the _unlocked functions.
   
   fputs_unlocked is an extension and needs to be prototyped specially.  */

#if defined HAVE_PUTC_UNLOCKED && (defined (HAVE_DECL_PUTC_UNLOCKED) && HAVE_DECL_PUTC_UNLOCKED)
# undef putc
# define putc(C, Stream) putc_unlocked (C, Stream)
#endif
#if defined HAVE_FPUTC_UNLOCKED && (defined (HAVE_DECL_PUTC_UNLOCKED) && HAVE_DECL_PUTC_UNLOCKED)
# undef fputc
# define fputc(C, Stream) fputc_unlocked (C, Stream)
#endif
#if defined HAVE_FPUTS_UNLOCKED && (defined (HAVE_DECL_PUTC_UNLOCKED) && HAVE_DECL_PUTC_UNLOCKED)
# undef fputs
# define fputs(String, Stream) fputs_unlocked (String, Stream)
# if defined (HAVE_DECL_FPUTS_UNLOCKED) && !HAVE_DECL_FPUTS_UNLOCKED
extern int fputs_unlocked PARAMS ((const char *, FILE *));
# endif
#endif

/* There are an extraordinary number of issues with <ctype.h>.
   The last straw is that it varies with the locale.  Use libiberty's
   replacement instead.  */
#include <safe-ctype.h>

/* Define a default escape character; it's different for EBCDIC.  */
#ifndef TARGET_ESC
#define TARGET_ESC 033
#endif

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

/* Infrastructure for defining missing _MAX and _MIN macros.  Note that
   macros defined with these cannot be used in #if.  */

/* The extra casts work around common compiler bugs.  */
#define INTTYPE_SIGNED(t) (! ((t) 0 < (t) -1))
/* The outer cast is needed to work around a bug in Cray C 5.0.3.0.
   It is necessary at least when t == time_t.  */
#define INTTYPE_MINIMUM(t) ((t) (INTTYPE_SIGNED (t) \
                             ? ~ (t) 0 << (sizeof(t) * CHAR_BIT - 1) : (t) 0))
#define INTTYPE_MAXIMUM(t) ((t) (~ (t) 0 - INTTYPE_MINIMUM (t)))

/* Use that infrastructure to provide a few constants.  */
#ifndef UCHAR_MAX
# define UCHAR_MAX INTTYPE_MAXIMUM (unsigned char)
#endif

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

/* The HAVE_DECL_* macros are three-state, undefined, 0 or 1.  If they
   are defined to 0 then we must provide the relevant declaration
   here.  These checks will be in the undefined state while configure
   is running so be careful to test "defined (HAVE_DECL_*)".  */

#ifndef bcopy
# ifdef HAVE_BCOPY
#  if defined (HAVE_DECL_BCOPY) && !HAVE_DECL_BCOPY
extern void bcopy PARAMS ((const PTR, PTR, size_t));
#  endif
# else /* ! HAVE_BCOPY */
#  define bcopy(src,dst,len) memmove((dst),(src),(len))
# endif
#endif

#if defined (HAVE_DECL_ATOF) && !HAVE_DECL_ATOF
extern double atof PARAMS ((const char *));
#endif

#if defined (HAVE_DECL_ATOL) && !HAVE_DECL_ATOL
extern long atol PARAMS ((const char *));
#endif

#if defined (HAVE_DECL_FREE) && !HAVE_DECL_FREE
extern void free PARAMS ((PTR));
#endif

#if defined (HAVE_DECL_GETCWD) && !HAVE_DECL_GETCWD
extern char *getcwd PARAMS ((char *, size_t));
#endif

#if defined (HAVE_DECL_GETENV) && !HAVE_DECL_GETENV
extern char *getenv PARAMS ((const char *));
#endif

#if defined (HAVE_DECL_GETOPT) && !HAVE_DECL_GETOPT
extern int getopt PARAMS ((int, char * const *, const char *));
#endif

#if defined (HAVE_DECL_GETWD) && !HAVE_DECL_GETWD
extern char *getwd PARAMS ((char *));
#endif

#if defined (HAVE_DECL_SBRK) && !HAVE_DECL_SBRK
extern PTR sbrk PARAMS ((int));
#endif

#if defined (HAVE_DECL_STRSTR) && !HAVE_DECL_STRSTR
extern char *strstr PARAMS ((const char *, const char *));
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#if defined (HAVE_DECL_MALLOC) && !HAVE_DECL_MALLOC
extern PTR malloc PARAMS ((size_t));
#endif

#if defined (HAVE_DECL_CALLOC) && !HAVE_DECL_CALLOC
extern PTR calloc PARAMS ((size_t, size_t));
#endif

#if defined (HAVE_DECL_REALLOC) && !HAVE_DECL_REALLOC
extern PTR realloc PARAMS ((PTR, size_t));
#endif

/* If the system doesn't provide strsignal, we get it defined in
   libiberty but no declaration is supplied. */
#ifndef HAVE_STRSIGNAL
# ifndef strsignal
extern const char *strsignal PARAMS ((int));
# endif
#endif

#ifdef HAVE_GETRLIMIT
# if defined (HAVE_DECL_GETRLIMIT) && !HAVE_DECL_GETRLIMIT
#  ifndef getrlimit
#   ifdef ANSI_PROTOTYPES
struct rlimit;
#   endif
extern int getrlimit PARAMS ((int, struct rlimit *));
#  endif
# endif
#endif

#ifdef HAVE_SETRLIMIT
# if defined (HAVE_DECL_SETRLIMIT) && !HAVE_DECL_SETRLIMIT
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

#if defined (HAVE_DECL_ABORT) && !HAVE_DECL_ABORT
extern void abort PARAMS ((void));
#endif

/* 1 if we have C99 designated initializers.  */
#if !defined(HAVE_DESIGNATED_INITIALIZERS)
#define HAVE_DESIGNATED_INITIALIZERS \
  ((GCC_VERSION >= 2007) || (__STDC_VERSION__ >= 199901L))
#endif

/* 1 if we have _Bool.  */
#ifndef HAVE__BOOL
# define HAVE__BOOL \
   ((GCC_VERSION >= 3000) || (__STDC_VERSION__ >= 199901L))
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

/* Test if something is a block special file.  */
#ifndef S_ISBLK
#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
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
# define IS_DIR_SEPARATOR(CH) ((CH) == DIR_SEPARATOR)
#else /* DIR_SEPARATOR_2 */
# define IS_DIR_SEPARATOR(CH) \
	(((CH) == DIR_SEPARATOR) || ((CH) == DIR_SEPARATOR_2))
#endif /* DIR_SEPARATOR_2 */

/* Say how to test for an absolute pathname.  On Unix systems, this is if
   it starts with a leading slash or a '$', the latter meaning the value of
   an environment variable is to be used.  On machien with DOS-based
   file systems, it is also absolute if it starts with a drive identifier.  */
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
#define IS_ABSOLUTE_PATHNAME(STR) \
  (IS_DIR_SEPARATOR ((STR)[0]) || (STR)[0] == '$' \
   || ((STR)[0] != '\0' && (STR)[1] == ':' && IS_DIR_SEPARATOR ((STR)[2])))
#else
#define IS_ABSOLUTE_PATHNAME(STR) \
  (IS_DIR_SEPARATOR ((STR)[0]) || (STR)[0] == '$')
#endif

/* Get libiberty declarations. */
#include "libiberty.h"

/* Make sure that ONLY_INT_FIELDS has an integral value.  */
#ifdef ONLY_INT_FIELDS
#undef ONLY_INT_FIELDS
#define ONLY_INT_FIELDS 1
#else
#define ONLY_INT_FIELDS 0
#endif 

/* Provide a default for the HOST_BIT_BUCKET.
   This suffices for POSIX-like hosts.  */

#ifndef HOST_BIT_BUCKET
#define HOST_BIT_BUCKET "/dev/null"
#endif

/* Enumerated bitfields are safe to use unless we've been explictly told
   otherwise or if they are signed. */
 
#define USE_ENUM_BITFIELDS (__GNUC__ || (!ONLY_INT_FIELDS && ENUM_BITFIELDS_ARE_UNSIGNED))

#if USE_ENUM_BITFIELDS
#define ENUM_BITFIELD(TYPE) enum TYPE
#else
#define ENUM_BITFIELD(TYPE) unsigned int
#endif

#ifndef offsetof
#define offsetof(TYPE, MEMBER)	((size_t) &((TYPE *)0)->MEMBER)
#endif

/* Traditional C cannot initialize union members of structs.  Provide
   a macro which expands appropriately to handle it.  This only works
   if you intend to initalize the union member to zero since it relies
   on default initialization to zero in the traditional C case.  */
#ifdef __STDC__
#define UNION_INIT_ZERO , {0}
#else
#define UNION_INIT_ZERO
#endif

/* GCC now gives implicit declaration warnings for undeclared builtins.  */
#if defined(__GNUC__) && defined (__SIZE_TYPE__)
extern void *alloca (__SIZE_TYPE__);
#endif

/* Various error reporting routines want to use __FUNCTION__.  */
#if (GCC_VERSION < 2007)
#ifndef __FUNCTION__
#define __FUNCTION__ "?"
#endif /* ! __FUNCTION__ */
#endif

/* Provide some sort of boolean type.  We use stdbool.h if it's
  available.  This is dead last because various system headers might
  mess us up.  */
#undef bool
#undef true
#undef false
#undef TRUE
#undef FALSE

#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#else
# if !HAVE__BOOL
typedef char _Bool;
# endif
# define bool _Bool
# define true 1
# define false 0
#endif

#define TRUE true
#define FALSE false

#endif /* __GCC_SYSTEM_H__ */
