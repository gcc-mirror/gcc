/* Definitions of target machine for GCC,
   for ARM with targetting the VXWorks run time environment. 
   Copyright (C) 1999, 2000, 2003, 2004, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

   Contributed by: Mike Stump <mrs@wrs.com>
   Brought up to date by CodeSourcery, LLC.
   
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


#define TARGET_OS_CPP_BUILTINS()		\
  do {						\
    if (TARGET_BIG_END)				\
      builtin_define ("ARMEB");			\
    else					\
      builtin_define ("ARMEL");			\
						\
    if (arm_arch_xscale)			\
      builtin_define ("CPU=XSCALE");		\
    else if (arm_arch5)				\
      builtin_define ("CPU=ARMARCH5");		\
    else if (arm_arch4)				\
      {						\
	if (thumb_code)				\
	  builtin_define ("CPU=ARMARCH4_T");	\
	else					\
	  builtin_define ("CPU=ARMARCH4");	\
      }						\
    VXWORKS_OS_CPP_BUILTINS ();			\
  } while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

/* Subsume the arm/elf.h definition, and add RTP hooks.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "-D__ELF__" VXWORKS_ADDITIONAL_CPP_SPEC

#undef  CC1_SPEC
#define CC1_SPEC							\
"%{tstrongarm:-mlittle-endian -mcpu=strongarm ;				\
   t4:        -mlittle-endian -march=armv4 ;				\
   t4be:      -mbig-endian -march=armv4 ;				\
   t4t:       -mthumb -mthumb-interwork -mlittle-endian -march=armv4t ;	\
   t4tbe:     -mthumb -mthumb-interwork -mbig-endian -march=armv4t ;	\
   t5:        -mlittle-endian -march=armv5 ;				\
   t5be:      -mbig-endian -march=armv5 ;				\
   t5t:       -mthumb -mthumb-interwork -mlittle-endian -march=armv5 ;	\
   t5tbe:     -mthumb -mthumb-interwork -mbig-endian -march=armv5 ;	\
   txscale:   -mlittle-endian -mcpu=xscale ;				\
   txscalebe: -mbig-endian -mcpu=xscale ;				\
            : -march=armv4}"

/* Pass -EB for big-endian targets.  */
#define VXWORKS_ENDIAN_SPEC \
  "%{mbig-endian|t4be|t4tbe|t5be|t5tbe|txscalebe:-EB}"

#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC VXWORKS_ENDIAN_SPEC

#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC " " VXWORKS_ENDIAN_SPEC

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC

#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

#undef TARGET_VERSION
#define TARGET_VERSION fputs (" (ARM/VxWorks)", stderr);

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

#define FPUTYPE_DEFAULT "vfp"

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER

/* We want to be compatible with a version of "2.96" at one point in
   the past before this macro was changed.  */
#undef DEFAULT_STRUCTURE_SIZE_BOUNDARY
#define DEFAULT_STRUCTURE_SIZE_BOUNDARY 8

/* The kernel loader does not allow relocations to overflow, so we
   cannot allow arbitrary relocation addends in kernel modules or RTP
   executables.  Also, the dynamic loader uses the resolved relocation
   value to distinguish references to the text and data segments, so we
   cannot allow arbitrary offsets for shared libraries either.  */
#undef ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P
#define ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P 1

#undef TARGET_DEFAULT_WORD_RELOCATIONS
#define TARGET_DEFAULT_WORD_RELOCATIONS 1
