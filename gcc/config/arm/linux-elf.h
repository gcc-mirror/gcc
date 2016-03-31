/* Definitions for ARM running Linux-based GNU systems using ELF
   Copyright (C) 1993-2016 Free Software Foundation, Inc.
   Contributed by Philip Blundell <philb@gnu.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

/* Run-time Target Specification.  */
#undef  TARGET_DEFAULT_FLOAT_ABI
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_HARD

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT    MASK_BIG_END
#define TARGET_ENDIAN_OPTION     "mbig-endian"
#define TARGET_LINKER_EMULATION  "armelfb_linux"
#else
#define TARGET_ENDIAN_DEFAULT    0
#define TARGET_ENDIAN_OPTION     "mlittle-endian"
#define TARGET_LINKER_EMULATION  "armelf_linux"
#endif

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (TARGET_ENDIAN_DEFAULT)

#define SUBTARGET_CPU_DEFAULT TARGET_CPU_arm6

#define SUBTARGET_EXTRA_LINK_SPEC " -m " TARGET_LINKER_EMULATION " -p"

/* We do not have any MULTILIB_OPTIONS specified, so there are no
   MULTILIB_DEFAULTS.  */
#undef  MULTILIB_DEFAULTS

/* Now we define the strings used to build the spec file.  */
#undef  LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

#define LIBGCC_SPEC "%{mfloat-abi=soft*:-lfloat} -lgcc"

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux.so.2"

#define LINUX_TARGET_LINK_SPEC  "%{h*} \
   %{static:-Bstatic} \
   %{shared:-shared} \
   %{symbolic:-Bsymbolic} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}} \
   -X \
   %{mbig-endian:-EB} %{mlittle-endian:-EL}" \
   SUBTARGET_EXTRA_LINK_SPEC

#undef  LINK_SPEC
#define LINK_SPEC LINUX_TARGET_LINK_SPEC

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#undef  FPUTYPE_DEFAULT
#define FPUTYPE_DEFAULT "vfp"

/* Call the function profiler with a given profile label.  */
#undef  ARM_FUNCTION_PROFILER
#define ARM_FUNCTION_PROFILER(STREAM, LABELNO)  			\
{									\
  fprintf (STREAM, "\tbl\tmcount%s\n",					\
	   (TARGET_ARM && NEED_PLT_RELOC) ? "(PLT)" : "");		\
}

/* The GNU/Linux profiler clobbers the link register.  Make sure the
   prologue knows to save it.  */
#define PROFILE_HOOK(X)						\
  emit_clobber (gen_rtx_REG (SImode, LR_REGNUM))

/* The GNU/Linux profiler needs a frame pointer.  */
#define SUBTARGET_FRAME_POINTER_REQUIRED crtl->profile

/* Add .note.GNU-stack.  */
#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK	1

/* Uninitialized common symbols in non-PIE executables, even with
   strong definitions in dependent shared libraries, will resolve
   to COPY relocated symbol in the executable.  See PR65780.  */
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P default_binds_local_p_2

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1
