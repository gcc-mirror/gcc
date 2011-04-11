/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

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


/* After the first "Node:" comment comes all preprocessor directives and
   attached declarations described in the info files, the "Using and
   Porting GCC" manual (uapgcc), in the same order as found in the "Target
   macros" section in the gcc-2.9x CVS edition of 2000-03-17.  FIXME: Not
   really, but needs an update anyway.

   There is no generic copy-of-uapgcc comment, you'll have to see uapgcc
   for that.  If applicable, there is a CRIS-specific comment.  The order
   of macro definitions follow the order in the manual.  Every section in
   the manual (node in the info pages) has an introductory `Node:
   <subchapter>' comment.  If no macros are defined for a section, only
   the section-comment is present.  */

/* This file defines the macros for cris-axis-linux-gnu that are not
   covered by cris.h, elfos.h and (config/)linux.h.  */

/* Make sure we have a valid TARGET_CPU_DEFAULT, so we can assume it
   and take shortcuts below.  */
#ifndef TARGET_CPU_DEFAULT
#error "TARGET_CPU_DEFAULT not defined"
#elif (TARGET_CPU_DEFAULT+0) != 10 && (TARGET_CPU_DEFAULT+0) != 32
#error "TARGET_CPU_DEFAULT must be 10 or 32, or this file be updated"
#endif

/* Node: Instruction Output */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Node: Driver */
/* These macros are CRIS-specific, but used in target driver macros.  */

#undef CRIS_CPP_SUBTARGET_SPEC
#if TARGET_CPU_DEFAULT == 32
# define CRIS_CPP_SUBTARGET_SPEC \
  "%{pthread:-D_REENTRANT}\
   %{!march=*:%{!mcpu=*:-D__arch_v32 -D__CRIS_arch_version=32}}"
#else
# define CRIS_CPP_SUBTARGET_SPEC \
  "%{pthread:-D_REENTRANT}\
   %{!march=*:%{!mcpu=*:-D__arch_v10 -D__CRIS_arch_version=10}}"
#endif

#undef CRIS_CC1_SUBTARGET_SPEC
#if TARGET_CPU_DEFAULT == 32
# define CRIS_CC1_SUBTARGET_SPEC \
 "%{!march=*:%{!mcpu=*:-march=v32}}"
#define CRIS_SUBTARGET_DEFAULT_ARCH MASK_AVOID_GOTPLT
#else
# define CRIS_CC1_SUBTARGET_SPEC \
 "%{!march=*:%{!mcpu=*:-march=v10}}"
#define CRIS_SUBTARGET_DEFAULT_ARCH 0
#endif

#undef CRIS_ASM_SUBTARGET_SPEC
#if TARGET_CPU_DEFAULT == 32
# define CRIS_ASM_SUBTARGET_SPEC \
 "--em=criself \
  %{!march=*:%{!mcpu=*:--march=v32}} \
  %{!fleading-underscore:--no-underscore}\
  %{fPIC|fpic|fPIE|fpie: --pic}"
#else
# define CRIS_ASM_SUBTARGET_SPEC \
 "--em=criself \
  %{!march=*:%{!mcpu=*:--march=v10}} \
  %{!fleading-underscore:--no-underscore}\
  %{fPIC|fpic|fPIE|fpie: --pic}"
#endif

/* Previously controlled by target_flags.  */
#undef TARGET_LINUX
#define TARGET_LINUX 1

#undef CRIS_SUBTARGET_DEFAULT
#define CRIS_SUBTARGET_DEFAULT			\
  (MASK_SVINTO					\
   + MASK_ETRAX4_ADD				\
   + MASK_ALIGN_BY_32				\
   + CRIS_SUBTARGET_DEFAULT_ARCH)

#undef CRIS_DEFAULT_CPU_VERSION
#define CRIS_DEFAULT_CPU_VERSION CRIS_CPU_NG

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"

#undef CRIS_LINK_SUBTARGET_SPEC
#define CRIS_LINK_SUBTARGET_SPEC \
 "-mcrislinux\
  %{shared} %{static}\
  %{symbolic:-Bdynamic} %{static:-Bstatic}\
  %{!shared:%{!static:\
              %{rdynamic:-export-dynamic}\
              -dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}\
  %{!r:%{O2|O3: --gc-sections}}"


/* Node: Run-time Target */

/* For the cris-*-linux* subtarget.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
      if (flag_leading_underscore <= 0)		\
	builtin_define ("__NO_UNDERSCORES__");	\
    }						\
  while (0)

/* Node: Type Layout */
     
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Node: Sections */

/* GNU/Linux has crti and crtn and does not need the
   CRT_CALL_STATIC_FUNCTION trick in cris.h.  */
#undef CRT_CALL_STATIC_FUNCTION

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
