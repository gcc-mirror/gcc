/* Definitions for Intel 386 running Linux-based GNU systems with ELF format.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich.

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

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()               \
  do                                           \
    {                                          \
      GNU_USER_TARGET_OS_CPP_BUILTINS();       \
      ANDROID_TARGET_OS_CPP_BUILTINS();	       \
    }                                          \
  while (0)

#undef CC1_SPEC
#define CC1_SPEC \
  LINUX_OR_ANDROID_CC (GNU_USER_TARGET_CC1_SPEC, \
		       GNU_USER_TARGET_CC1_SPEC " " ANDROID_CC1_SPEC)

#undef	LINK_SPEC
#define LINK_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_LINK_SPEC, \
		       GNU_USER_TARGET_LINK_SPEC " " ANDROID_LINK_SPEC)

#undef  LIB_SPEC
#define LIB_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_LIB_SPEC, \
		    GNU_USER_TARGET_NO_PTHREADS_LIB_SPEC " " ANDROID_LIB_SPEC)

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, \
		       ANDROID_STARTFILE_SPEC)

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_MATHFILE_SPEC " " \
		       GNU_USER_TARGET_ENDFILE_SPEC,	 \
		       GNU_USER_TARGET_MATHFILE_SPEC " " \
		       ANDROID_ENDFILE_SPEC)

#ifndef LIBMPX_LIBS
#define LIBMPX_LIBS "\
 %:include(libmpx.spec)%(link_libmpx)"
#endif

#ifndef LINK_MPX
#if defined (HAVE_LD_BNDPLT_SUPPORT)
#define LINK_MPX "-z bndplt "
#else
#define LINK_MPX \
  "%nGCC was configured with a linker with no '-z bndplt' support. " \
  "It significantly reduces MPX coverage for dynamic codes. " \
  "It is strongly recommended to use GCC properly configured for MPX."
#endif
#endif

#ifndef MPX_SPEC
#ifdef SPEC_64
#define MPX_SPEC "\
 %{mmpx:%{fcheck-pointer-bounds:%{!static:%{" SPEC_64 ":" LINK_MPX "}}}}"
#else
#define MPX_SPEC ""
#endif
#endif

#ifdef HAVE_LD_PUSHPOPSTATE_SUPPORT
#define MPX_LD_AS_NEEDED_GUARD_PUSH "--push-state --no-as-needed"
#define MPX_LD_AS_NEEDED_GUARD_POP "--pop-state"
#else
#define MPX_LD_AS_NEEDED_GUARD_PUSH ""
#define MPX_LD_AS_NEEDED_GUARD_POP ""
#endif

#ifndef LIBMPX_SPEC
#if defined(HAVE_LD_STATIC_DYNAMIC)
#define LIBMPX_SPEC "\
%{mmpx:%{fcheck-pointer-bounds:\
    %{static:--whole-archive -lmpx --no-whole-archive" LIBMPX_LIBS "}\
    %{!static:%{static-libmpx:" LD_STATIC_OPTION " --whole-archive}\
    %{!static-libmpx:" MPX_LD_AS_NEEDED_GUARD_PUSH "} -lmpx \
    %{!static-libmpx:" MPX_LD_AS_NEEDED_GUARD_POP "} \
    %{static-libmpx:--no-whole-archive " LD_DYNAMIC_OPTION \
    LIBMPX_LIBS "}}}}"
#else
#define LIBMPX_SPEC "\
%{mmpx:%{fcheck-pointer-bounds:-lmpx" LIBMPX_LIBS "}}"
#endif
#endif

#ifndef LIBMPXWRAPPERS_SPEC
#if defined(HAVE_LD_STATIC_DYNAMIC)
#define LIBMPXWRAPPERS_SPEC "\
%{mmpx:%{fcheck-pointer-bounds:%{!fno-chkp-use-wrappers:\
    %{static:-lmpxwrappers}\
    %{!static:%{static-libmpxwrappers:" LD_STATIC_OPTION "}\
    -lmpxwrappers %{static-libmpxwrappers: "\
    LD_DYNAMIC_OPTION "}}}}}"
#else
#define LIBMPXWRAPPERS_SPEC "\
%{mmpx:%{fcheck-pointer-bounds:{!fno-chkp-use-wrappers:-lmpxwrappers}}}"
#endif
#endif

#ifndef CHKP_SPEC
#define CHKP_SPEC "\
%{!nostdlib:%{!nodefaultlibs:" LIBMPX_SPEC LIBMPXWRAPPERS_SPEC "}}" MPX_SPEC
#endif

extern void file_end_indicate_exec_stack_and_cet (void);

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END file_end_indicate_exec_stack_and_cet
