/* Configuration for GNU C-compiler for openVMS/Alpha.
   Copyright (C)  1996 Free Software Foundation, Inc.
   Contributed by Klaus Kaempf (kkaempf@progis.de).

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

/* If compiling with DECC, need to fix problem with <stdio.h>
   which defines a macro called FILE_TYPE that breaks "tree.h".
   Fortunately it uses #ifndef to suppress multiple inclusions.
   Three possible cases:
        1) <stdio.h> has already been included -- ours will be no-op;
        2) <stdio.h> will be included after us -- "theirs" will be no-op;
        3) <stdio.h> isn't needed -- including it here shouldn't hurt.
   In all three cases, the problem macro will be removed here.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __DECC
#undef FILE_TYPE
#endif

#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG 32

#define HOST_WIDE_INT long long
#define HOST_BITS_PER_WIDE_INT 64

#undef SUCCESS_EXIT_CODE
#define SUCCESS_EXIT_CODE 1
#undef FATAL_EXIT_CODE
#define FATAL_EXIT_CODE (44 | 0x10000000)  /* Abort, and no DCL message.  */

/* A couple of conditionals for execution machine are controlled here.  */
#ifndef VMS
#define VMS
#endif

/* Define a local equivalent (sort of) for unlink */
#define unlink remove
#define NEED_ATEXIT

#define NO_SYS_PARAMS_H		/* Don't have <sys/params.h> */
#define NO_STAB_H		/* Don't have <stab.h> */
#define USE_C_ALLOCA		/* Using alloca.c */

#define HAVE_FCNTL_H 1
#define HAVE_STDLIB_H 1
#define HAVE_UNISTD_H 1
#define HAVE_STRING_H 1
#define STDC_HEADERS 1

/* Use ANSI/SYSV style byte manipulation routines instead of BSD ones.  */

#define bcopy(s,d,n)	memcpy((d),(s),(n))
#define bzero(d,n)	memset((d),0,(n))
#define bcmp(l,r,n)	memcmp((l),(r),(n))

#define index	strchr
#define rindex	strrchr

#if __STDC__
extern void *alloca (size_t);
#else
extern char *alloca (unsigned int);
#endif

#define OBJECT_SUFFIX ".obj"
#define EXECUTABLE_SUFFIX ".exe"
#define DIR_SEPARATOR ']'
#define PATH_SEPARATOR ','
