/* Configuration for GNU C-compiler for BeOS host.
   Copyright (C) 1997-99, 2000 Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com), based on xm-rs6000.h
   by Richard Kenner (kenner@vlsi1.ultra.nyu.edu).

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

#include <i386/xm-i386.h>

/* Arguments to use with `exit'.  */

#define	SUCCESS_EXIT_CODE	0
#define	FATAL_EXIT_CODE		33

/* Include <sys/wait.h> to define the exit status access macros.  */
#ifndef inhibit_libc
#include <sys/wait.h>
#endif

#define	ONLY_INT_FIELDS

/* use ANSI/SYSV style byte manipulation routines instead of BSD ones */

#undef bcopy
#define bcopy(s,d,n)	memmove((d),(s),(n))
#undef bzero
#define bzero(d,n)	memset((d),0,(n))
#undef bcmp
#define bcmp(l,r,n)	memcmp((l),(r),(n))
#undef index
#define index		strchr
#undef rindex
#define rindex		strrchr

/* BeOS is closer to USG than BSD */

#define USG

/* Define various things that the BeOS host has. */

#ifndef HAVE_VPRINTF
#define HAVE_VPRINTF
#endif
#ifndef HAVE_PUTENV
#define HAVE_PUTENV
#endif
#define HAVE_RENAME

#define STDC_HEADERS 1
