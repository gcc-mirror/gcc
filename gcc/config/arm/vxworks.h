/* Definitions of target machine for GCC,
   for ARM with targeting the VXWorks run time environment. 
   Copyright (C) 1999-2018 Free Software Foundation, Inc.

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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* TARGET_OS_CPP_BUILTINS, down to BPABI if defined.  */

#if defined (TARGET_BPABI_CPP_BUILTINS)
#define MAYBE_TARGET_BPABI_CPP_BUILTINS TARGET_BPABI_CPP_BUILTINS
#else
#define MAYBE_TARGET_BPABI_CPP_BUILTINS()
#endif

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do {						\
    if (TARGET_BIG_END)				\
      builtin_define ("ARMEB");			\
    else					\
      builtin_define ("ARMEL");			\
						\
    if (arm_arch_xscale)			\
      builtin_define ("CPU=XSCALE");		\
    else if (arm_arch7)				\
      {						\
	if (!arm_arch_notm)			\
	  builtin_define ("CPU=ARMARCH7M");	\
	else if (TARGET_THUMB)			\
	  builtin_define ("CPU=ARMARCH7_T2");	\
	else					\
	  builtin_define ("CPU=ARMARCH7");	\
      }						\
    else if (arm_arch6)				\
      {						\
	if (TARGET_THUMB)			\
	  builtin_define ("CPU=ARMARCH6_T");	\
	else					\
	  builtin_define ("CPU=ARMARCH6");	\
      }						\
    else if (arm_arch5t)				\
	builtin_define ("CPU=ARMARCH5_T");	\
    else if (arm_arch4)				\
      {						\
	if (TARGET_THUMB)			\
	  builtin_define ("CPU=ARMARCH4_T");	\
	else					\
	  builtin_define ("CPU=ARMARCH4");	\
      }						\
    VXWORKS_OS_CPP_BUILTINS ();			\
    MAYBE_TARGET_BPABI_CPP_BUILTINS ();		\
  } while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

/* Subsume the arm/elf.h definition, and add RTP hooks.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "-D__ELF__" VXWORKS_ADDITIONAL_CPP_SPEC

/* .text.hot and .text.unlikely sections are badly handled by the
   VxWorks kernel mode loader for ARM style exceptions.  */

#if ARM_UNWIND_INFO
#define EXTRA_CC1_SPEC "%{!mrtp:-fno-reorder-functions}"
#else
#define EXTRA_CC1_SPEC
#endif

#undef  CC1_SPEC
#define CC1_SPEC "" EXTRA_CC1_SPEC

/* Translate an explicit -mbig-endian as an explicit -EB to assembler
   and linker, and pass abi options matching the target expectations
   or command-line requests.  */
#define VXWORKS_ENDIAN_SPEC "%{mbig-endian:-EB}"

#if defined (TARGET_BPABI_CPP_BUILTINS)
#define MAYBE_ASM_ABI_SPEC \
  "%{mabi=apcs-gnu|mabi=atpcs:-meabi=gnu;:-meabi=5}" TARGET_FIX_V4BX_SPEC
#else
#define MAYBE_ASM_ABI_SPEC
#endif

#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC MAYBE_ASM_ABI_SPEC " " VXWORKS_ENDIAN_SPEC

#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC " " VXWORKS_ENDIAN_SPEC

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC

#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

/* For exceptions, pre VX7 uses DWARF2 info, VX7 uses ARM unwinding.  */
#undef  DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO (!TARGET_VXWORKS7)

#undef ARM_TARGET2_DWARF_FORMAT
#define ARM_TARGET2_DWARF_FORMAT \
  (TARGET_VXWORKS_RTP ? (DW_EH_PE_pcrel | DW_EH_PE_indirect) : DW_EH_PE_absptr)

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

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

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* This platform supports the probing method of stack checking (RTP mode).
   8K is reserved in the stack to propagate exceptions in case of overflow.  */
#define STACK_CHECK_PROTECT 8192

/* Unless overridded by the target options, the default is little-endian.  */
#define TARGET_ENDIAN_DEFAULT 0
