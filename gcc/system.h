/* system.h - Get common system includes and various definitions and
   declarations based on autoconf macros.
   Copyright (C) 1998 Free Software Foundation, Inc.

 */

#ifndef __GCC_SYSTEM_H__
#define __GCC_SYSTEM_H__

#include <stdio.h>
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
#include <sys/stat.h>
#include <errno.h>

#ifndef errno
extern int errno;
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
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
# include <sys/time.h>
# else
#  include <time.h>
#endif
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# include <sys/file.h>
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



#ifndef bcopy
# ifdef HAVE_BCOPY
#  ifdef NEED_DECLARATION_BCOPY
void bcopy ();
#  endif
# else /* ! HAVE_BCOPY */
#  define bcopy(src,dst,len) memcpy ((dst),(src),(len))
# endif
#endif

#ifndef bcmp
# ifdef HAVE_BCMP
#  ifdef NEED_DECLARATION_BCMP
void bcmp ();
#  endif
# else /* ! HAVE_BCMP */
#  define bcmp(left,right,len) memcmp ((left),(right),(len))
# endif
#endif

#ifndef bzero
# ifdef HAVE_BZERO
#  ifdef NEED_DECLARATION_BZERO
void bzero ();
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

#ifdef NEED_DECLARATION_FREE
extern void free ();
#endif

#endif /* __GCC_SYSTEM_H__ */
