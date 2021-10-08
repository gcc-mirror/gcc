/* Definitions of target machine for GNU compiler.  Vxworks PowerPC version.
   Copyright (C) 1996-2021 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* The port comes in two very different flavors at this stage:

   - For 653 (AE) and regular versions prior to VxWorks 7, the port
     comes with its own set of definitions, matching a system compiler
     configured this way as well as the corresponding run-time
     environment.  This is essentially an eabi system, so changes to
     eabi.h should usually be reflected here.

   - Starting with VxWorks 7 (post SR600), the system environment
     was made extremely similar to GNU/Linux and this toolchain is
     built on top of the corresponding header files.  */

/*-------------------------------------------------------------*/
/* Common definitions first.                                   */
/*-------------------------------------------------------------*/

/* CPP predefined macros.  */

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__ELF__");		\
      if (!TARGET_VXWORKS7)			\
	builtin_define ("__EABI__");		\
						\
      /* CPU macros, based on what the system compilers do.  */	\
      if (!TARGET_VXWORKS7)			\
	{					\
	  builtin_define ("__ppc");		\
	  /* Namespace violation below, but the system headers \
	     really depend heavily on this.  */	\
	  builtin_define ("CPU_FAMILY=PPC");	\
						\
	  /* __PPC__ isn't actually emitted by the system compiler \
	     prior to vx7 but has been advertised by us for ages.  */	\
	  builtin_define ("__PPC__");		\
	}					\
      else					\
	{					\
	  builtin_define ("__PPC__");		\
	  builtin_define ("__powerpc__");	\
	  if (TARGET_64BIT)			\
	    {					\
	      builtin_define ("__PPC64__");	\
	      builtin_define ("__powerpc64__");	\
	    }					\
	  else					\
	    {					\
	      builtin_define ("__PPC");		\
	      builtin_define ("__powerpc");	\
	    }					\
						\
	  /* __ppc isn't emitted by the system compiler \
	     any more but a few system headers still depend \
	     on it, as well as on __ppc__.  */	\
	  builtin_define ("__ppc");		\
	  builtin_define ("__ppc__");		\
	}					\
						\
      /* Asserts for #cpu and #machine.  */	\
      if (TARGET_64BIT)				\
	{					\
	  builtin_assert ("cpu=powerpc64");     \
	  builtin_assert ("machine=powerpc64"); \
	}					\
      else 					\
	{					\
	  builtin_assert ("cpu=powerpc");	\
	  builtin_assert ("machine=powerpc");   \
	}					\
						\
      /* PowerPC VxWorks specificities.  */	\
      if (!TARGET_SOFT_FLOAT)			\
	{					\
	  builtin_define ("__hardfp");		\
	  builtin_define ("_WRS_HARDWARE_FP");  \
	}                                       \
						\
      /* Common VxWorks and port items.  */	\
      VXWORKS_OS_CPP_BUILTINS ();		\
      TARGET_OS_SYSV_CPP_BUILTINS ();		\
    }		\
  while (0)


#define VX_CPUDEF(CPUID) \
  ":-D" VX_CPU_PREFIX "CPU=" VX_CPU_PREFIX #CPUID

#define VX_MCPU(CPU,CPUID) \
  "mcpu=" #CPU VX_CPUDEF(CPUID)

#undef CPP_SPEC
#define CPP_SPEC			\
  "%{!D" VX_CPU_PREFIX "CPU=*:%{"	\
  VX_MCPU(403, PPC403)   ";"		\
  VX_MCPU(405, PPC405)   ";"		\
  VX_MCPU(440, PPC440)   ";"		\
  VX_MCPU(464, PPC464)   ";"		\
  VX_MCPU(476, PPC476)   ";"		\
  VX_MCPU(603, PPC603)   ";"		\
  VX_MCPU(604, PPC604)   ";"		\
  VX_MCPU(860, PPC860)   ";"		\
  VX_MCPU(e6500, PPCE6500)  ";"		\
  VX_MCPU(8540, PPC85XX) ";"		\
  VX_MCPU(8548, PPC85XX) ";"		\
  VX_CPUDEF(PPC604)			\
  "}}"					\
  VXWORKS_ADDITIONAL_CPP_SPEC

/* FIXME: The only reason we allow no -mcpu switch at all is because
   config-ml.in insists on a "." multilib.  */

#undef  LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

/* No _mcount profiling on VxWorks.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE,LABELNO) VXWORKS_FUNCTION_PROFILER(FILE,LABELNO)

/* Nor sdata, for kernel mode.  We use this in
   SUBSUBTARGET_INITIALIZE_OPTIONS, after rs6000_rtp has been initialized.  */
#undef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE (TARGET_VXWORKS_RTP ? 8 : 0)

#undef SUB3TARGET_OVERRIDE_OPTIONS
#define SUB3TARGET_OVERRIDE_OPTIONS           \
  do {                                          \
  if (!OPTION_SET_P (g_switch_value))     \
    g_switch_value = SDATA_DEFAULT_SIZE;        \
  VXWORKS_OVERRIDE_OPTIONS;                     \
  } while (0)

/* The stack pointer need not be moved while checking the stack.  */
#undef STACK_CHECK_MOVING_SP

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* Room needed to allow exception propagation, from what experiments
   and low level observations taught us ...  */
#define STACK_CHECK_PROTECT (TARGET_64BIT ? 16 * 1024 : 12 * 1024)

/* Leverage linker relaxation for RTPs.  This helps 32bit programs
   referring to kernel services too far away for short calls, is more
   precise than -mlongcall and can be overriden with -Wl,--no-relax.  */
#define VXWORKS_RELAX_LINK_SPEC "%{mrtp:--relax}"

/*-------------------------------------------------------------*/
/* Pre-VxWorks7 configuration.                                 */
/*-------------------------------------------------------------*/

#if !TARGET_VXWORKS7

#undef RS6000_STARTING_FRAME_OFFSET
#define RS6000_STARTING_FRAME_OFFSET					\
  (cfun->calls_alloca							\
   ? RS6000_ALIGN (crtl->outgoing_args_size + RS6000_SAVE_AREA, 16)	\
   : (RS6000_ALIGN (crtl->outgoing_args_size, 16) + RS6000_SAVE_AREA))

#undef STACK_DYNAMIC_OFFSET
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
   RS6000_ALIGN (crtl->outgoing_args_size.to_constant ()		\
		 + STACK_POINTER_OFFSET, 16)

/* Enforce 16-byte alignment for the stack pointer, to permit general
   compliance with e.g. Altivec instructions requirements.  Make sure
   this isn't overruled by the EABI constraints.  */

#undef  STACK_BOUNDARY
#define STACK_BOUNDARY (16*BITS_PER_UNIT)

#undef  PREFERRED_STACK_BOUNDARY
#define PREFERRED_STACK_BOUNDARY STACK_BOUNDARY

#undef  ABI_STACK_BOUNDARY

#undef  STARTFILE_PREFIX_SPEC
#define STARTFILE_PREFIX_SPEC						\
 "%{mrtp:%{!shared:%:getenv(WIND_BASE /target/lib/usr/lib/ppc/PPC32/common)}}"

/* For aggregates passing, use the same, consistent ABI as Linux.  */
#define AGGREGATE_PADDING_FIXED 0
#define AGGREGATES_PAD_UPWARD_ALWAYS 0

#undef ASM_SPEC
#define ASM_SPEC \
"%(asm_cpu) \
 %{,assembler|,assembler-with-cpp: %{mregnames} %{mno-regnames}} \
 %{mrelocatable} %{mrelocatable-lib} %{" FPIC_SPEC ":-K PIC} -mbig"

#undef CC1_SPEC
#define CC1_SPEC VXWORKS_CC1_SPEC " \
  %{G*} %{mno-sdata:-msdata=none} %{msdata:-msdata=default}      \
 %{mlittle|mlittle-endian:-mstrict-align}"

#undef  LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC " " VXWORKS_RELAX_LINK_SPEC

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_EABI | MASK_STRICT_ALIGN)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC604

/* Only big endian PPC is supported by VxWorks.  */
#undef BYTES_BIG_ENDIAN
#define BYTES_BIG_ENDIAN 1

#undef WORDS_BIG_ENDIAN
#define WORDS_BIG_ENDIAN 1

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS /* none needed */

#else /* TARGET_VXWORKS7 */

/*-------------------------------------------------------------*/
/* Post-VxWorks7 (SR600) configuration.                        */
/*-------------------------------------------------------------*/

/* VxWorks does not use local symbols for the function entry point.  */
#undef DOT_SYMBOLS
#define DOT_SYMBOLS 0

#undef LINK_OS_VXWORKS_SPEC
#define LINK_OS_VXWORKS_SPEC \
  " %{!mrtp:-r} %{mrtp:-q -static} %{!Xbind-lazy:-z now}"

#undef LINK_OS_EXTRA_SPEC32
#define LINK_OS_EXTRA_SPEC32 LINK_OS_VXWORKS_SPEC " " VXWORKS_RELAX_LINK_SPEC

#undef LINK_OS_EXTRA_SPEC64
#define LINK_OS_EXTRA_SPEC64 LINK_OS_VXWORKS_SPEC

/* linux64.h enables this, not supported in vxWorks.  */
#undef TARGET_FLOAT128_ENABLE_TYPE
#define TARGET_FLOAT128_ENABLE_TYPE 0

#endif /* TARGET_VXWORKS7 */

