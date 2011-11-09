/* Configuration file for ARM BPABI targets.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC   

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Use the AAPCS ABI by default.  */
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS

/* Assume that AAPCS ABIs should adhere to the full BPABI.  */ 
#define TARGET_BPABI (TARGET_AAPCS_BASED)

/* BPABI targets use EABI frame unwinding tables.  */
#undef ARM_UNWIND_INFO
#define ARM_UNWIND_INFO 1

/* Section 4.1 of the AAPCS requires the use of VFP format.  */
#undef  FPUTYPE_DEFAULT
#define FPUTYPE_DEFAULT "vfp"

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT MASK_BIG_END
#else
#define TARGET_ENDIAN_DEFAULT 0
#endif

/* EABI targets should enable interworking by default.  */
#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_INTERWORK | TARGET_ENDIAN_DEFAULT)

/* The ARM BPABI functions return a boolean; they use no special
   calling convention.  */
#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) TARGET_BPABI

/* The BPABI integer comparison routines return { -1, 0, 1 }.  */
#define TARGET_LIB_INT_CMP_BIASED !TARGET_BPABI

#define TARGET_FIX_V4BX_SPEC " %{mcpu=arm8|mcpu=arm810|mcpu=strongarm*"\
  "|march=armv4|mcpu=fa526|mcpu=fa626:--fix-v4bx}"

#define BE8_LINK_SPEC \
  " %{mbig-endian:%{march=armv7-a|mcpu=cortex-a5	\
   |mcpu=cortex-a7					\
   |mcpu=cortex-a8|mcpu=cortex-a9|mcpu=cortex-a15	\
   |mcpu=generic-armv7-a				\
   |march=armv7-m|mcpu=cortex-m3			\
   |march=armv7e-m|mcpu=cortex-m4			\
   |march=armv6-m|mcpu=cortex-m0			\
   :%{!r:--be8}}}"

/* Tell the assembler to build BPABI binaries.  */
#undef  SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC \
  "%{mabi=apcs-gnu|mabi=atpcs:-meabi=gnu;:-meabi=5}" TARGET_FIX_V4BX_SPEC

#ifndef SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC ""
#endif

/* The generic link spec in elf.h does not support shared libraries.  */
#define BPABI_LINK_SPEC \
  "%{mbig-endian:-EB} %{mlittle-endian:-EL} "		\
  "%{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic} "	\
  "-X" SUBTARGET_EXTRA_LINK_SPEC TARGET_FIX_V4BX_SPEC BE8_LINK_SPEC

#undef  LINK_SPEC
#define LINK_SPEC BPABI_LINK_SPEC

/* The BPABI requires that we always use an out-of-line implementation
   of RTTI comparison, even if the target supports weak symbols,
   because the same object file might be used on a target that does
   not support merging symbols across DLL boundaries.  This macro is
   broken out separately so that it can be used within
   TARGET_OS_CPP_BUILTINS in configuration files for systems based on
   the BPABI.  */
#define TARGET_BPABI_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__GXX_TYPEINFO_EQUALITY_INLINE=0");	\
    }							\
  while (false)

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() \
  TARGET_BPABI_CPP_BUILTINS()

/* The BPABI specifies the use of .{init,fini}_array.  Therefore, we
   do not want GCC to put anything into the .{init,fini} sections.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#define INIT_ARRAY_SECTION_ASM_OP ARM_EABI_CTORS_SECTION_OP
#define FINI_ARRAY_SECTION_ASM_OP ARM_EABI_DTORS_SECTION_OP

/* The legacy _mcount implementation assumes r11 points to a
    4-word APCS frame.  This is generally not true for EABI targets,
    particularly not in Thumb mode.  We assume the mcount
    implementation does not require a counter variable (No Counter).
    Note that __gnu_mcount_nc will be entered with a misaligned stack.
    This is OK because it uses a special calling convention anyway.  */

#undef  NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS 1
#undef  ARM_FUNCTION_PROFILER
#define ARM_FUNCTION_PROFILER(STREAM, LABELNO)  			\
{									\
  fprintf (STREAM, "\tpush\t{lr}\n");					\
  fprintf (STREAM, "\tbl\t__gnu_mcount_nc\n");				\
}

#undef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED 0

/* __gnu_mcount_nc restores the original LR value before returning.  Ensure
   that there is no unnecessary hook set up.  */
#undef PROFILE_HOOK
