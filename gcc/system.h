/* system.h - Get common system includes and various definitions and
   declarations based on autoconf macros.
   Copyright (C) 1998 Free Software Foundation, Inc.

 */

#ifndef __GCC_SYSTEM_H__
#define __GCC_SYSTEM_H__

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



#ifndef bcopy
# ifdef HAVE_BCOPY
#  ifdef NEED_DECLARATION_BCOPY
extern void bcopy ();
#  endif
# else /* ! HAVE_BCOPY */
#  define bcopy(src,dst,len) memcpy ((dst),(src),(len))
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
   location of the error in the source file.  */
#ifndef abort
#ifndef __STDC__
#ifndef __GNUC__
#ifndef USE_SYSTEM_ABORT
#define USE_SYSTEM_ABORT
#endif /* !USE_SYSTEM_ABORT */
#endif /* !__GNUC__ */
#endif /* !__STDC__ */

#ifdef USE_SYSTEM_ABORT
# ifdef NEED_DECLARATION_ABORT
extern void abort ();
# endif
#else
#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
#define abort()								\
(fprintf (stderr,							\
	  "%s:%d: Internal compiler error\n", __FILE__, __LINE__),	\
 exit (FATAL_EXIT_CODE))

#else
#define abort()								\
(fprintf (stderr,							\
	  "%s:%d: Internal compiler error in function %s\n",		\
	  __FILE__, __LINE__, __PRETTY_FUNCTION__),			\
 exit (FATAL_EXIT_CODE))

#endif /* recent gcc */
#endif /* USE_SYSTEM_ABORT */
#endif /* !abort */


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


/* These macros are here in preparation for the use of gettext in egcs.  */
#define _(String) String
#define N_(String) String

#endif /* __GCC_SYSTEM_H__ */
