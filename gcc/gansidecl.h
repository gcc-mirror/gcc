/* ANSI and traditional C compatibility macros.
   Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

#ifndef	__GANSIDECL_H__
#define	__GANSIDECL_H__

#include "ansidecl.h"

/* Autoconf will possibly define the `inline' or `const' keywords as
   macros, however this is only valid for the stage1 compiler.  If we
   detect a modern version of gcc, unconditionally reset the values.
   This makes sure the right thing happens in stage2 and later.  We
   need to do this very early; i.e. before any systems header files or
   gcc header files in case they use these keywords.  Otherwise
   conflicts might occur. */
#if (GCC_VERSION >= 2007)
# undef const
# undef inline
# define inline __inline__  /* Modern gcc can use `__inline__' freely. */
# ifndef HAVE_LONG_DOUBLE
#  define HAVE_LONG_DOUBLE 1
# endif
/* Assume that (non-traditional) gcc used in stage2 or later has the
   stringize feature.  */
# if !defined (HAVE_STRINGIZE) && __STDC__
#  define HAVE_STRINGIZE 1
# endif /* ! HAVE_STRINGIZE && __STDC__ */
#endif /* GCC >= 2.7 */

#ifndef NULL_PTR
#define NULL_PTR ((PTR) 0)
#endif

#endif /* __GANSIDECL_H__ */
