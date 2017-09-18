/* Target macros for arc*-*-linux targets.

   Copyright (C) 2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Enable DWARF 2 exceptions.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS ();	\
    }						\
  while (0)

#define GLIBC_DYNAMIC_LINKER   "/lib/ld-linux.so.2"
#define UCLIBC_DYNAMIC_LINKER  "/lib/ld-uClibc.so.0"

/* Note that the default is to link against dynamic libraries, if they are
   available.  Override with -static.  */
#undef LINK_SPEC
#define LINK_SPEC "%{h*} \
  %{static:-Bstatic} \
  %{shared:-shared} \
  %{symbolic:-Bsymbolic} \
  %{!static: \
    %{rdynamic:-export-dynamic} \
    %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}} \
  -X \
  %{mbig-endian:-EB} %{EB} %{EL} \
  %{!z:-z max-page-size=0x2000 -z common-page-size=0x2000} \
  %{mcpu=nps400:-marclinux_nps; :-marclinux}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, ANDROID_STARTFILE_SPEC)

#undef ENDFILE_SPEC
#define ENDFILE_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_ENDFILE_SPEC, ANDROID_ENDFILE_SPEC)

#undef LIB_SPEC
#define LIB_SPEC  \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* No SDATA default for linux.  */
#undef TARGET_SDATA_DEFAULT
#define TARGET_SDATA_DEFAULT 0

/* We have medium calls.  */
#undef TARGET_MMEDIUM_CALLS_DEFAULT
#define TARGET_MMEDIUM_CALLS_DEFAULT 1

/* We do not have any MULTILIB_OPTIONS specified, so there are no
   MULTILIB_DEFAULTS.  */
#undef  MULTILIB_DEFAULTS

/* Linux toolchains use r25 as the thread pointer register.  */
#undef TARGET_ARC_TP_REGNO_DEFAULT
#define TARGET_ARC_TP_REGNO_DEFAULT 25

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{pthread:-D_REENTRANT} \
"

/* Indexed loads are default off.  */
#undef TARGET_INDEXED_LOADS_DEFAULT
#define TARGET_INDEXED_LOADS_DEFAULT 0

/* Pre/post modify with register displacement are default off.  */
#undef TARGET_AUTO_MODIFY_REG_DEFAULT
#define TARGET_AUTO_MODIFY_REG_DEFAULT 0
