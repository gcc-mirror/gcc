/* Definitions of target machine for GNU compiler,
   for PowerPC NetBSD systems.
   Copyright 2001 Free Software Foundation, Inc.

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

/* Under NetBSD, the normal location of the various *crt*.o files is
   the /usr/lib directory [from config/netbsd.h].  */

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/lib/"

/* FIXME: Should this macro be treated the same as for the other
   spec's?  */
/* NOTE: -dc and -dp are equivalent yet NetBSD's CC passes both both!
   NetBSD's CC also passes -O1 but we can skip that.  NetBSD explictly
   sets ``-e _start'', since LD knows this, skip it.  */

#undef LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "\
%{shared:-shared} \
%{!shared: %{static:-dc -dp -static}} \
%{!shared: %{!static:-dc -dp}} \
"

/* Override the defaults.  */
#undef LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_netbsd)"

#undef STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_netbsd)"

#undef ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_netbsd)"

#undef LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_netbsd)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_netbsd)"

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_netbsd)"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC NetBSD/ELF)");

/* Use STABS debugging information by default.  DWARF2 makes a mess of
   the 1.5.2 linker.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
