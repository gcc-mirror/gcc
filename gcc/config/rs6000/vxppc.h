/* Definitions of target machine for GNU compiler.  Vxworks PowerPC version.
   Copyright (C) 1996 Free Software Foundation, Inc.

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

/* This file just exists to give specs for the PowerPC running on VxWorks.  */

#include "rs6000/sysv4.h"

/* ??? This file redefines CPP_SPEC which is wrong.  It should instead define
   one of the extra specs that gets included in CPP_SPEC.  For instance,
   CPP_OS_DEFAULT_SPEC.  The mrelocatable line was copied from CPP_SYSV_SPEC.
   There is probably other stuff missing.  */

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_sysv) %(cpp_endian) %(cpp_cpu) \
%{mads: %(cpp_os_ads) } \
%{myellowknife: %(cpp_os_yellowknife) } \
%{mmvme: %(cpp_os_mvme) } \
%{msim: %(cpp_os_sim) } \
%{mcall-linux: %(cpp_os_linux) } \
%{mcall-solaris: %(cpp_os_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(cpp_os_default) }}}}}} \
%{!DCPU=*: \
  %{!mcpu*: -DCPU=PPC603} \
  %{mcpu=powerpc: -DCPU=PPC603} \
  %{mcpu=403: -DCPU=PPC403} \
  %{mcpu=601: -DCPU=PPC601} \
  %{mcpu=603: -DCPU=PPC603} \
  %{mcpu=604: -DCPU=PPC604}}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__vxworks -Asystem(vxworks) -Asystem(embedded) \
-Acpu(powerpc) -Amachine(powerpc)"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  make linker just
   combine objects. */

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

/* We use stabs-in-elf for debugging */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
