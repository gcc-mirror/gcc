/* ANSI and traditional C compatibility macros.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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

/* This file mimics some of the support provided by include/ansidecl.h
   in binutils and gdb releases.
   ??? Over time the two should be merged into one.  */

#ifndef	ANSIDECL_H
#define	ANSIDECL_H

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#ifndef VPROTO
#ifdef __STDC__
#define PVPROTO(ARGS)		ARGS
#define VPROTO(ARGS)            ARGS
#define VA_START(va_list,var)  va_start(va_list,var)
#else
#define PVPROTO(ARGS)		()
#define VPROTO(ARGS)            (va_alist) va_dcl
#define VA_START(va_list,var)  va_start(va_list)
#endif
#endif

/* Define a generic NULL if one hasn't already been defined.  */

#ifndef NULL
#define NULL 0
#endif

#ifndef GENERIC_PTR
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define GENERIC_PTR void *
#else
#define GENERIC_PTR char *
#endif
#endif

#ifndef NULL_PTR
#define NULL_PTR ((GENERIC_PTR) 0)
#endif

#ifdef __STDC__

#define	PTR void *

#else

#define	PTR char *
#ifndef const
#define const
#endif

#endif /* ! __STDC__ */

/* We don't have autoconf for libgcc2.c since it's a target, so don't
   define these functions, which aren't used there anyway.  */

#ifndef IN_LIBGCC2

#ifndef HAVE_BCOPY
#define bcopy(src,dst,len) memcpy ((dst),(src),(len))
#endif

#ifndef HAVE_BZERO
#define bzero(dst,len) memset ((dst),0,(len))
#endif

#ifndef HAVE_BCMP
#define bcmp(left,right,len) memcmp ((left),(right),(len))
#endif

#ifndef HAVE_RINDEX
#define rindex strrchr
#endif

#ifndef HAVE_INDEX
#define index strchr
#endif

#endif /* IN_LIBGCC2 */

#endif /* ANSIDECL_H */
