/* Operating system specific defines to be used when targeting GCC for
   Interix
   Copyright (C) 1994, 1995, 1999 Free Software Foundation, Inc.
   Donn Terry, Softway Systems, Inc. (donn@softway.com)
   Modified from code
      Contributed by Douglas B. Rupp (drupp@cs.washington.edu).

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

#define TARGET_MEM_FUNCTIONS

/* POSIX/Uni-thread only for now.  Look at the winnt version
for windows/multi thread */

/* We need multiple -lc -lcpsx because they mutually refer;
   that should go away someday */

#undef LIB_SPEC
#define LIB_SPEC "\
 %{!shared:%{!dynamic:-lc -lcpsx -lc -lcpsx %$INTERIX_ROOT/usr/lib/psxdll.a \
 %$INTERIX_ROOT/usr/lib/psxdll2.a \
 }} \
 %{!G:%{!dynamic:-lc -lcpsx -lc -lcpsx %$INTERIX_ROOT/usr/lib/psxdll.a \
 %$INTERIX_ROOT/usr/lib/psxdll2.a \
 }} \
 %{dynamic:-lc %$INTERIX_ROOT/usr/lib/psxdll.a \
 %$INTERIX_ROOT/usr/lib/psxdll2.a \
 } \
 %{v}"

#undef LINK_SPEC
#define LINK_SPEC "%{!shared:-stack 0x400000,0x10000} \
 		   -subsystem posix \
		   %{g} \
		   %{dynamic:-Bdynamic} \
		   %{static:-Bstatic} \
		   %{shared:--shared -Bdynamic} \
		   %{G:--shared -Bdynamic} \
		   %{symbolic:--shared -Bsymbolic -Bdynamic} \
		   %{soname*:--soname %*} \
   		   %{rpath*:--rpath %*} \
		   "

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}} %{shared:crti%O%s}"

#undef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)					\
 ((DEFAULT_WORD_SWITCH_TAKES_ARG (STR)					\
 || !strcmp(STR, "rpath"))						\
  && strcmp (STR, "Tdata") && strcmp (STR, "Ttext")			\
  && strcmp (STR, "Tbss"))


#if 0	
/* don't do this until we can sort out the default path issues. MK */
#undef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX ""

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX ""

#undef TOOLDIR_BASE_PREFIX
#define TOOLDIR_BASE_PREFIX ""

#endif /* 0 */

#undef STDC_VALUE
#define STDC_VALUE 0

#define HANDLE_SYSV_PRAGMA
#undef HANDLE_PRAGMA_WEAK  /* until the link format can handle it */

/* Names to predefine in the preprocessor for this target machine.  */

#define DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG


/* TARGET_DEFAULT from configure */

#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT
