/* Definitions for SH running NetBSD using ELF
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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

/* Run-time Target Specification.  */

/* Extra specs needed for NetBSD SuperH ELF targets.  */

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "netbsd_entry_point", NETBSD_ENTRY_POINT },


#define TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
      NETBSD_OS_CPP_BUILTINS_ELF();					\
      builtin_define ("__NO_LEADING_UNDERSCORES__");			\
    }									\
  while (0)

/* Provide a LINK_SPEC appropriate for a NetBSD/sh ELF target.
   We use the SH_LINK_SPEC from sh/sh.h, and define the appropriate
   SUBTARGET_LINK_SPEC that pulls in what we need from a generic
   NetBSD ELF LINK_SPEC.  */

/* LINK_EMUL_PREFIX from sh/elf.h */
#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "_nbsd"

#undef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC NETBSD_LINK_SPEC_ELF

#undef LINK_SPEC
#define LINK_SPEC SH_LINK_SPEC

#define NETBSD_ENTRY_POINT "__start"

/* Provide a CPP_SPEC appropriate for NetBSD.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC NETBSD_CPP_SPEC

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | TARGET_ENDIAN_DEFAULT)

/* Define because we use the label and we do not need them.  */
#define NO_PROFILE_COUNTERS 1
 
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM,LABELNO)				\
do									\
  {									\
        fprintf((STREAM), "\tmov.l\t%sLP%d,r1\n",			\
                LOCAL_LABEL_PREFIX, (LABELNO));				\
        fprintf((STREAM), "\tmova\t%sLP%dr,r0\n",			\
                LOCAL_LABEL_PREFIX, (LABELNO));				\
        fprintf((STREAM), "\tjmp\t@r1\n");				\
        fprintf((STREAM), "\tnop\n");					\
        fprintf((STREAM), "\t.align\t2\n");				\
        fprintf((STREAM), "%sLP%d:\t.long\t__mcount\n",			\
                LOCAL_LABEL_PREFIX, (LABELNO));				\
        fprintf((STREAM), "%sLP%dr:\n", LOCAL_LABEL_PREFIX, (LABELNO));	\
  }									\
while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
  do									\
    {									\
      /* Set -musermode if it hasn't been specified.  */		\
      if (OPTION_SET_P (TARGET_USERMODE) == 0)			\
	TARGET_USERMODE = true;						\
    }									\
  while (0)
