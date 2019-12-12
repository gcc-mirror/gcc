/* Definitions of target machine for GNU compiler, FreeBSD/arm version.
   Copyright (C) 2002-2019 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC FBSD_CPP_SPEC

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS					\
  { "subtarget_extra_asm_spec",	SUBTARGET_EXTRA_ASM_SPEC },	\
  { "subtarget_asm_float_spec", SUBTARGET_ASM_FLOAT_SPEC }, 	\
  { "fbsd_dynamic_linker", FBSD_DYNAMIC_LINKER }

#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC \
  "%{mabi=apcs-gnu|mabi=atpcs:-meabi=gnu;:-meabi=5} " TARGET_FIX_V4BX_SPEC " \
  %{" FPIE_OR_FPIC_SPEC ":-k}"

#undef SUBTARGET_ASM_FLOAT_SPEC
#ifdef TARGET_FREEBSD_ARM_HARD_FLOAT
/* Default to full vfp if we build for arm*hf.  */
#define SUBTARGET_ASM_FLOAT_SPEC "%{!mfpu=*:-mfpu=vfp}"
#else
#define SUBTARGET_ASM_FLOAT_SPEC "%{!mfpu=*:-mfpu=softvfp}"
#endif

#undef	LINK_SPEC
#define LINK_SPEC "							\
  %{p:%nconsider using `-pg' instead of `-p' with gprof (1)}		\
  %{v:-V}								\
  %{assert*} %{R*} %{rpath*} %{defsym*}					\
  %{shared:-Bshareable %{h*} %{soname*}}				\
  %{!shared:								\
    %{!static:								\
      %{rdynamic:-export-dynamic}					\
      %{!dynamic-linker:-dynamic-linker %(fbsd_dynamic_linker) }}	\
    %{static:-Bstatic}}							\
  %{!static:--hash-style=both --enable-new-dtags}			\
  %{symbolic:-Bsymbolic}						\
  -X %{mbig-endian:-EB} %{mlittle-endian:-EL}"

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT    MASK_BIG_END
#define TARGET_ENDIAN_OPTION     "mbig-endian"
#define TARGET_LINKER_EMULATION  "armelfb_fbsd"
#else
#define TARGET_ENDIAN_DEFAULT    0
#define TARGET_ENDIAN_OPTION     "mlittle-endian"
#define TARGET_LINKER_EMULATION  "armelf_fbsd"
#endif

#undef	SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m " TARGET_LINKER_EMULATION " -p"

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() 		\
  do						\
    {						\
	FBSD_TARGET_OS_CPP_BUILTINS ();		\
	TARGET_BPABI_CPP_BUILTINS ();		\
    }						\
  while (false)

/* We default to a soft-float ABI so that binaries can run on all
   target hardware.  */
#undef TARGET_DEFAULT_FLOAT_ABI
#ifdef TARGET_FREEBSD_ARM_HARD_FLOAT
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_HARD
#else
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_SOFT
#endif

#undef ARM_DEFAULT_ABI

/* AACPS_LINUX has access to kernel atomic ops while we don't.
   But AACPS defaults to short_enums.  */
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS_LINUX

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_INTERWORK | TARGET_ENDIAN_DEFAULT)

/* We do not have any MULTILIB_OPTIONS specified, so there are no
   MULTILIB_DEFAULTS.  */
#undef  MULTILIB_DEFAULTS

/*  Use the AAPCS type for wchar_t, override the one from config/freebsd.h.  */
#undef  WCHAR_TYPE
#define WCHAR_TYPE  "unsigned int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* FreeBSD 10 does not support unaligned access for armv6 and up.
   Unaligned access support was added in FreeBSD 11.  */
#if FBSD_MAJOR < 11
#define SUBTARGET_OVERRIDE_INTERNAL_OPTIONS				\
do {									\
    if (opts_set->x_unaligned_access == 1)				\
        warning (0, "target OS does not support unaligned accesses");	\
    if (opts->x_unaligned_access)					\
	opts->x_unaligned_access = 0;					\
} while (0)
#endif

#undef MAX_SYNC_LIBFUNC_SIZE
#define MAX_SYNC_LIBFUNC_SIZE 4 /* UNITS_PER_WORD not defined yet.  */

/* FreeBSD does its profiling differently to the Acorn compiler.  We
   don't need a word following the mcount call; and to skip it
   requires either an assembly stub or use of fomit-frame-pointer when
   compiling the profiling functions.  Since we break Acorn CC
   compatibility below a little more won't hurt.  */

#undef ARM_FUNCTION_PROFILER
#define ARM_FUNCTION_PROFILER(STREAM,LABELNO)		\
{							\
  asm_fprintf (STREAM, "\tmov\t%Rip, %Rlr\n");		\
  asm_fprintf (STREAM, "\tbl\t__mcount%s\n",		\
	       (TARGET_ARM && NEED_PLT_RELOC)		\
	       ? "(PLT)" : "");				\
}

/* Clear the instruction cache from `BEG' to `END'.  This makes a
   call to the ARM_SYNC_ICACHE architecture specific syscall.  */
#define CLEAR_INSN_CACHE(BEG, END)					\
do									\
  {									\
    extern int sysarch (int number, void *args);			\
    struct								\
      {									\
	unsigned int addr;						\
	int          len;						\
      } s;								\
    s.addr = (unsigned int) (BEG);					\
    s.len = (END) - (BEG);						\
    (void) sysarch (0, &s);						\
  }									\
while (0)

/* This is how we tell the assembler that two symbols have the same value.  */
#define ASM_OUTPUT_DEF(FILE, NAME1, NAME2) \
  do					   \
    {					   \
     assemble_name (FILE, NAME1); 	   \
     fputs (" = ", FILE);		   \
     assemble_name (FILE, NAME2);	   \
     fputc ('\n', FILE);		   \
    }					   \
  while (0)

/* Add  .note.GNU-stack.  */
#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#define ARM_TARGET2_DWARF_FORMAT (DW_EH_PE_pcrel | DW_EH_PE_indirect)

