/* Definitions for Intel 386 running MOSS
   Copyright (C) 1996, 2001 Free Software Foundation, Inc.
   Contributed by Bryan Ford <baford@cs.utah.edu>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_OS_CPP_BUILTINS /* config.gcc includes i386/linux.h.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("moss");		\
	builtin_assert ("system=posix");	\
	if (flag_pic)				\
	  {					\
	    builtin_define ("__PIC__");		\
	    builtin_define ("__pic__");		\
	  }					\
    }						\
  while (0)

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "crt0.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC  "crtn.o%s"

#undef	LINK_SPEC

