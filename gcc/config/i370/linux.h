/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993, 1995, 1996, 1997, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for Linux/390 by Linas Vepstas (linas@linas.org)

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


#define TARGET_VERSION fprintf (stderr, " (i370 GNU/Linux with ELF)");

/* Specify that we're generating code for a Linux port to 370 */

#define TARGET_ELF_ABI
#define LINUX_DEFAULT_ELF


/* hack alert define to get dbx/gdb/dwarf to compile  */
/* problem is that host float format is not target float format.  */
/* define REAL_ARITHMETIC for software emulation of float to
 * int conversion.  This seems to have somethings to do with 
 * cross-compiling ...  */
#define REAL_ARITHMETIC

/* Include system common definitions */
/* TODO: convert include to ${tm_file} list in config.gcc.  */
#include "i370/i370.h"

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-DGCC -Dgcc -D__ELF__ -Dunix -D__gnu_linux__ -Dlinux -Asystem=posix -Acpu=i370 -Amachine=i370"

/* Options for this target machine.  */

#define LIBGCC_SPEC "libgcc.a%s"

#ifdef SOME_FUTURE_DAY
 
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_sysv) %(cpp_endian_big) \
%{mcall-linux: %(cpp_os_linux) } \
%{!mcall-linux: %(cpp_os_default) }"

#define LIB_SPEC "\
%{mcall-linux: %(lib_linux) } \
%{!mcall-linux:%(lib_default) }"

#define STARTFILE_SPEC "\
%{mcall-linux: %(startfile_linux) } \
%{!mcall-linux: %(startfile_default) }"

#define ENDFILE_SPEC "\
%{mcall-linux: %(endfile_linux) } \
%{!mcall-linux: %(endfile_default) }"

/* GNU/Linux support.  */
#ifndef LIB_LINUX_SPEC
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } %{!mnewlib: -lc }"
#endif

#ifndef STARTFILE_LINUX_SPEC
#define STARTFILE_LINUX_SPEC "\
%{!shared: %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}} \
%{mnewlib: ecrti.o%s} \
%{!mnewlib: crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}}"
#endif

#ifndef ENDFILE_LINUX_SPEC
#define ENDFILE_LINUX_SPEC "\
%{mnewlib: ecrtn.o%s} \
%{!mnewlib: %{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s}"
#endif

#ifndef LINK_START_LINUX_SPEC
#define LINK_START_LINUX_SPEC "-Ttext 0x10000"
#endif

#ifndef LINK_OS_LINUX_SPEC
#define LINK_OS_LINUX_SPEC ""
#endif

#ifndef CPP_OS_LINUX_SPEC
#define CPP_OS_LINUX_SPEC "-D__unix__ -D__gnu_linux__ -D__linux__ \
%{!ansi: -Dunix -Dlinux } \
-Asystem=unix -Asystem=linux"
#endif

#ifndef CPP_OS_LINUX_SPEC
#define CPP_OS_LINUX_SPEC ""
#endif


/* Define any extra SPECS that the compiler needs to generate.  */
#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS                                           \
  { "lib_linux",                LIB_LINUX_SPEC },                       \
  { "lib_default",              LIB_DEFAULT_SPEC },                     \
  { "startfile_linux",          STARTFILE_LINUX_SPEC },                 \
  { "startfile_default",        STARTFILE_DEFAULT_SPEC },               \
  { "endfile_linux",            ENDFILE_LINUX_SPEC },                   \
  { "endfile_default",          ENDFILE_DEFAULT_SPEC },                 \
  { "link_shlib",               LINK_SHLIB_SPEC },                      \
  { "link_target",              LINK_TARGET_SPEC },                     \
  { "link_start",               LINK_START_SPEC },                      \
  { "link_start_linux",         LINK_START_LINUX_SPEC },                \
  { "link_os",                  LINK_OS_SPEC },                         \
  { "link_os_linux",            LINK_OS_LINUX_SPEC },                   \
  { "link_os_default",          LINK_OS_DEFAULT_SPEC },                 \
  { "cpp_endian_big",           CPP_ENDIAN_BIG_SPEC },                  \
  { "cpp_os_linux",             CPP_OS_LINUX_SPEC },                    \
  { "cpp_os_default",           CPP_OS_DEFAULT_SPEC },

#endif /* SOME_FUTURE_DAY */
