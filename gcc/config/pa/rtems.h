/* Definitions of target machine for GNU compiler, for PRO.
   Copyright (C) 1997, 2000, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel@OARcorp.com).

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

/* Specify predefined symbols in preprocessor.  */

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	if (c_language != clk_cplusplus		\
	    && !flag_iso)			\
	  {					\
	    builtin_define ("hppa");		\
	    builtin_define_std ("PWB");		\
	  }					\
	builtin_define ("__rtems__");		\
	builtin_assert ("system=rtems");	\
    }						\
  while (0)

#undef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p: -lc}%{pg: -lc} -N"
