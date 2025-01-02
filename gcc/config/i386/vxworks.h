/* IA32 VxWorks target definitions for GNU compiler.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Updated by CodeSourcery, LLC.

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

/* VxWorks after 7 SR0600 use the ELF ABI and the system environment is llvm
   based.  Earlier versions have GNU based environment components and use the
   same ABI as Solaris 2.  */

#if TARGET_VXWORKS7

#undef VXWORKS_PERSONALITY
#define VXWORKS_PERSONALITY "llvm"

#else

#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
	(MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_VECT8_RETURNS)

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_LP64 ? "long int" : "int")

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_LP64 ? "long unsigned int" : "unsigned int")

/* We cannot use PC-relative accesses for VxWorks PIC because there is no
   fixed gap between segments.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT

#if TARGET_64BIT_DEFAULT
#undef VXWORKS_SYSCALL_LIBS_RTP
#define VXWORKS_SYSCALL_LIBS_RTP "-lsyscall"
#endif

#endif

/* Provide our target specific DEBUGGER_REGNO.  VxWorks relies on
   the SVR4 numbering.  */

#undef DEBUGGER_REGNO
#define DEBUGGER_REGNO(n) \
  (TARGET_64BIT ? debugger64_register_map[n] : svr4_debugger_register_map[n])

/* CPU macro definitions, ordered to account for VxWorks 7 not
   supporting CPUs older than PENTIUM4 since SR0650.  */

#define VX_CPUDEF(CPU) builtin_define(VX_CPU_PREFIX "CPU=" #CPU)
#define VX_CPUVDEF(CPU) builtin_define(VX_CPU_PREFIX "CPU_VARIANT=" #CPU)

#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      VXWORKS_OS_CPP_BUILTINS ();			\
      if (TARGET_64BIT)					\
	VX_CPUDEF (X86_64);				\
      else if (TARGET_CPU_P (PENTIUM4))			\
	{						\
	  VX_CPUDEF (PENTIUM4);				\
	  VX_CPUVDEF (PENTIUM4);			\
	}						\
      else if (TARGET_CPU_P (CORE2))			\
	VX_CPUDEF (CORE2);				\
      else if (TARGET_CPU_P (NEHALEM))			\
	VX_CPUDEF (NEHALEM);				\
      else if (TARGET_CPU_P (SANDYBRIDGE))		\
	VX_CPUDEF (SANDYBRIDGE);			\
      else if (TARGET_CPU_P (HASWELL))			\
	VX_CPUDEF (HASWELL);				\
      else if (TARGET_CPU_P (SILVERMONT))		\
	VX_CPUDEF (SILVERMONT);				\
      else if (TARGET_CPU_P (SKYLAKE) || TARGET_CPU_P (SKYLAKE_AVX512)) \
	VX_CPUDEF (SKYLAKE);				\
      else if (TARGET_CPU_P (GOLDMONT))			\
	VX_CPUDEF (GOLDMONT);				\
      else if (TARGET_VXWORKS7)				\
	VX_CPUDEF (PENTIUM4);				\
      else if (TARGET_CPU_P (I386))			\
	VX_CPUDEF (I80386);				\
      else if (TARGET_CPU_P (I486))			\
	VX_CPUDEF (I80486);				\
      else if (TARGET_CPU_P (PENTIUM))			\
	{						\
	  VX_CPUDEF (PENTIUM);				\
	  VX_CPUVDEF (PENTIUM);				\
	}						\
      else if (TARGET_CPU_P (PENTIUMPRO))		\
	{						\
	  VX_CPUDEF (PENTIUM2);				\
	  VX_CPUVDEF (PENTIUMPRO);			\
	}						\
      else						\
	VX_CPUDEF (I80386);				\
    }							\
  while (0)

#undef  CPP_SPEC
#define CPP_SPEC VXWORKS_ADDITIONAL_CPP_SPEC
#undef  CC1_SPEC
#define CC1_SPEC VXWORKS_CC1_SPEC
#undef  LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC
#undef  LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES EXTRA_SUBTARGET_SWITCHES

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

/* No _mcount profiling on VxWorks.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE,LABELNO) VXWORKS_FUNCTION_PROFILER(FILE,LABELNO)

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* This platform supports the probing method of stack checking (RTP mode).
   8K is reserved in the stack to propagate exceptions in case of overflow.
   On 64-bit targets, we double that size.  */

#define STACK_CHECK_PROTECT (TARGET_64BIT_DEFAULT ? 16 * 1024 : 8 * 1024)
