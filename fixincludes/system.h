/* Get common system includes and various definitions and declarations based
   on autoconf macros.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

This file is part of libcpp (aka cpplib).

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#ifndef FIXINC_SYSTEM_H
#define FIXINC_SYSTEM_H

/* We must include stdarg.h before stdio.h.  */
#include <stdarg.h>

#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif

#include <stdio.h>

/* Define a generic NULL if one hasn't already been defined.  */
#ifndef NULL
#define NULL 0
#endif

/* fixincludes is not a multi-threaded application and therefore we
   do not have to use the locking functions.  In fact, using the locking
   functions can cause the compiler to be significantly slower under
   I/O bound conditions (such as -g -O0 on very large source files).

   HAVE_DECL_PUTC_UNLOCKED actually indicates whether or not the stdio
   code is multi-thread safe by default.  If it is set to 0, then do
   not worry about using the _unlocked functions.

   fputs_unlocked, fwrite_unlocked, and fprintf_unlocked are
   extensions and need to be prototyped by hand (since we do not
   define _GNU_SOURCE).  */

#if defined HAVE_DECL_PUTC_UNLOCKED && HAVE_DECL_PUTC_UNLOCKED

# ifdef HAVE_PUTC_UNLOCKED
#  undef putc
#  define putc(C, Stream) putc_unlocked (C, Stream)
# endif
# ifdef HAVE_FPUTC_UNLOCKED
#  undef fputc
#  define fputc(C, Stream) fputc_unlocked (C, Stream)
# endif

# ifdef HAVE_FPUTS_UNLOCKED
#  undef fputs
#  define fputs(String, Stream) fputs_unlocked (String, Stream)
#  if defined (HAVE_DECL_FPUTS_UNLOCKED) && !HAVE_DECL_FPUTS_UNLOCKED
extern int fputs_unlocked (const char *, FILE *);
#  endif
# endif
# ifdef HAVE_FWRITE_UNLOCKED
#  undef fwrite
#  define fwrite(Ptr, Size, N, Stream) fwrite_unlocked (Ptr, Size, N, Stream)
#  if defined (HAVE_DECL_FWRITE_UNLOCKED) && !HAVE_DECL_FWRITE_UNLOCKED
extern int fwrite_unlocked (const void *, size_t, size_t, FILE *);
#  endif
# endif
# ifdef HAVE_FPRINTF_UNLOCKED
#  undef fprintf
/* We can't use a function-like macro here because we don't know if
   we have varargs macros.  */
#  define fprintf fprintf_unlocked
#  if defined (HAVE_DECL_FPRINTF_UNLOCKED) && !HAVE_DECL_FPRINTF_UNLOCKED
extern int fprintf_unlocked (FILE *, const char *, ...);
#  endif
# endif

#endif

/* ??? Glibc's fwrite/fread_unlocked macros cause
   "warning: signed and unsigned type in conditional expression".  */
#undef fread_unlocked
#undef fwrite_unlocked

#include <sys/types.h>
#include <errno.h>

#if !defined (errno) && defined (HAVE_DECL_ERRNO) && !HAVE_DECL_ERRNO
extern int errno;
#endif

/* Some of glibc's string inlines cause warnings.  Plus we'd rather
   rely on (and therefore test) GCC's string builtins.  */
#define __NO_STRING_INLINES

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

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# ifdef HAVE_SYS_FILE_H
#  include <sys/file.h>
# endif
#endif

/* The HAVE_DECL_* macros are three-state, undefined, 0 or 1.  If they
   are defined to 0 then we must provide the relevant declaration
   here.  These checks will be in the undefined state while configure
   is running so be careful to test "defined (HAVE_DECL_*)".  */

#if defined (HAVE_DECL_ABORT) && !HAVE_DECL_ABORT
extern void abort (void);
#endif

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

/* Test if something is a normal file.  */
#ifndef S_ISREG
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

/* Filename handling macros.  */
#include "filenames.h"

/* Get libiberty declarations.  */
#include "libiberty.h"
#include "safe-ctype.h"

#endif /* ! FIXINC_SYSTEM_H */
