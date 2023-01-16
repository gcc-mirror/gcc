/* Definitions of target machine for GNU compiler for Renesas / SuperH SH.
   Copyright (C) 1993-2023 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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

#ifndef GCC_SH_H
#define GCC_SH_H

#include "config/vxworks-dummy.h"

/* Unfortunately, insn-attrtab.cc doesn't include insn-codes.h.  We can't
   include it here, because bconfig.h is also included by gencodes.cc .  */
/* ??? No longer true.  */
extern int code_for_indirect_jump_scratch;

#define TARGET_CPU_CPP_BUILTINS() sh_cpu_cpp_builtins (pfile)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  */

#ifndef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED 0
#endif


/* Nonzero if this is an ELF target - compile time only */
#define TARGET_ELF 0

/* Nonzero if we should generate code using type 2E insns.  */
#define TARGET_SH2E (TARGET_SH2 && TARGET_SH_E)

/* Nonzero if we should generate code using type 2A insns.  */
#define TARGET_SH2A TARGET_HARD_SH2A
/* Nonzero if we should generate code using type 2A SF insns.  */
#define TARGET_SH2A_SINGLE (TARGET_SH2A && TARGET_SH2E)
/* Nonzero if we should generate code using type 2A DF insns.  */
#define TARGET_SH2A_DOUBLE (TARGET_HARD_SH2A_DOUBLE && TARGET_SH2A)

/* Nonzero if we should generate code using type 3E insns.  */
#define TARGET_SH3E (TARGET_SH3 && TARGET_SH_E)

/* Nonzero if we schedule for a superscalar implementation.  */
#define TARGET_SUPERSCALAR (TARGET_HARD_SH4 || TARGET_SH2A)

/* Nonzero if a double-precision FPU is available.  */
#define TARGET_FPU_DOUBLE (TARGET_SH4 || TARGET_SH2A_DOUBLE)

/* Nonzero if an FPU is available.  */
#define TARGET_FPU_ANY (TARGET_SH2E || TARGET_FPU_DOUBLE)

/* Nonzero if we're generating code for SH4a, unless the use of the
   FPU is disabled (which makes it compatible with SH4al-dsp).  */
#define TARGET_SH4A_FP (TARGET_SH4A && TARGET_FPU_ANY)

/* True if the FPU is a SH4-300 variant.  */
#define TARGET_FPU_SH4_300 (TARGET_FPU_ANY && TARGET_SH4_300)

/* This is not used by the SH2E calling convention  */
#define TARGET_VARARGS_PRETEND_ARGS(FUN_DECL) \
  (! TARGET_SH2E \
   && ! (TARGET_HITACHI || sh_attr_renesas_p (FUN_DECL)))

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT SELECT_SH1
#define SUPPORT_SH1 1
#define SUPPORT_SH2E 1
#define SUPPORT_SH4 1
#define SUPPORT_SH4_SINGLE 1
#define SUPPORT_SH2A 1
#define SUPPORT_SH2A_SINGLE 1
#endif

#define TARGET_DIVIDE_CALL_DIV1 (sh_div_strategy == SH_DIV_CALL_DIV1)
#define TARGET_DIVIDE_CALL_FP (sh_div_strategy == SH_DIV_CALL_FP)
#define TARGET_DIVIDE_CALL_TABLE (sh_div_strategy == SH_DIV_CALL_TABLE)

#define SELECT_SH1		 (MASK_SH1)
#define SELECT_SH2		 (MASK_SH2 | SELECT_SH1)
#define SELECT_SH2E		 (MASK_SH_E | MASK_SH2 | MASK_SH1 \
				  | MASK_FPU_SINGLE)
#define SELECT_SH2A		 (MASK_SH_E | MASK_HARD_SH2A \
				  | MASK_HARD_SH2A_DOUBLE \
				  | MASK_SH2 | MASK_SH1)
#define SELECT_SH2A_NOFPU	 (MASK_HARD_SH2A | MASK_SH2 | MASK_SH1)
#define SELECT_SH2A_SINGLE_ONLY  (MASK_SH_E | MASK_HARD_SH2A | MASK_SH2 \
				  | MASK_SH1 | MASK_FPU_SINGLE \
				  | MASK_FPU_SINGLE_ONLY)
#define SELECT_SH2A_SINGLE	 (MASK_SH_E | MASK_HARD_SH2A \
				  | MASK_FPU_SINGLE | MASK_HARD_SH2A_DOUBLE \
				  | MASK_SH2 | MASK_SH1)
#define SELECT_SH3		 (MASK_SH3 | SELECT_SH2)
#define SELECT_SH3E		 (MASK_SH_E | MASK_FPU_SINGLE | SELECT_SH3)
#define SELECT_SH4_NOFPU	 (MASK_HARD_SH4 | SELECT_SH3)
#define SELECT_SH4_SINGLE_ONLY	 (MASK_HARD_SH4 | SELECT_SH3E \
				  | MASK_FPU_SINGLE_ONLY)
#define SELECT_SH4		 (MASK_SH4 | MASK_SH_E | MASK_HARD_SH4 \
				  | SELECT_SH3)
#define SELECT_SH4_SINGLE	 (MASK_FPU_SINGLE | SELECT_SH4)
#define SELECT_SH4A_NOFPU	 (MASK_SH4A | SELECT_SH4_NOFPU)
#define SELECT_SH4A_SINGLE_ONLY  (MASK_SH4A | SELECT_SH4_SINGLE_ONLY)
#define SELECT_SH4A		 (MASK_SH4A | SELECT_SH4)
#define SELECT_SH4A_SINGLE	 (MASK_SH4A | SELECT_SH4_SINGLE)

#if SUPPORT_SH1
#define SUPPORT_SH2 1
#endif
#if SUPPORT_SH2
#define SUPPORT_SH3 1
#define SUPPORT_SH2A_NOFPU 1
#endif
#if SUPPORT_SH3
#define SUPPORT_SH4_NOFPU 1
#endif
#if SUPPORT_SH4_NOFPU
#define SUPPORT_SH4A_NOFPU 1
#define SUPPORT_SH4AL 1
#endif

#if SUPPORT_SH2E
#define SUPPORT_SH3E 1
#define SUPPORT_SH2A_SINGLE_ONLY 1
#endif
#if SUPPORT_SH3E
#define SUPPORT_SH4_SINGLE_ONLY 1
#endif
#if SUPPORT_SH4_SINGLE_ONLY
#define SUPPORT_SH4A_SINGLE_ONLY 1
#endif

#if SUPPORT_SH4
#define SUPPORT_SH4A 1
#endif

#if SUPPORT_SH4_SINGLE
#define SUPPORT_SH4A_SINGLE 1
#endif

/* Reset all target-selection flags.  */
#define MASK_ARCH (MASK_SH1 | MASK_SH2 | MASK_SH3 | MASK_SH_E | MASK_SH4 \
		   | MASK_HARD_SH2A | MASK_HARD_SH2A_DOUBLE | MASK_SH4A \
		   | MASK_HARD_SH4 | MASK_FPU_SINGLE \
		   | MASK_FPU_SINGLE_ONLY)

/* This defaults us to big-endian.  */
#ifndef TARGET_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT 0
#endif

#ifndef TARGET_OPT_DEFAULT
#define TARGET_OPT_DEFAULT  0
#endif

#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | TARGET_ENDIAN_DEFAULT | TARGET_OPT_DEFAULT)

#ifndef SH_MULTILIB_CPU_DEFAULT
#define SH_MULTILIB_CPU_DEFAULT "m1"
#endif

#if TARGET_ENDIAN_DEFAULT
#define MULTILIB_DEFAULTS { "ml", SH_MULTILIB_CPU_DEFAULT }
#else
#define MULTILIB_DEFAULTS { "mb", SH_MULTILIB_CPU_DEFAULT }
#endif

#define CPP_SPEC " %(subtarget_cpp_spec) "

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#define EXTRA_SPECS						\
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },			\
  { "link_emul_prefix", LINK_EMUL_PREFIX },			\
  { "link_default_cpu_emul", LINK_DEFAULT_CPU_EMUL },		\
  { "subtarget_link_emul_suffix", SUBTARGET_LINK_EMUL_SUFFIX },	\
  { "subtarget_link_spec", SUBTARGET_LINK_SPEC },		\
  { "subtarget_asm_endian_spec", SUBTARGET_ASM_ENDIAN_SPEC },	\
  { "subtarget_asm_relax_spec", SUBTARGET_ASM_RELAX_SPEC },	\
  { "subtarget_asm_isa_spec", SUBTARGET_ASM_ISA_SPEC },		\
  { "subtarget_asm_spec", SUBTARGET_ASM_SPEC },			\
  SUBTARGET_EXTRA_SPECS

#if TARGET_CPU_DEFAULT & MASK_HARD_SH4
#define SUBTARGET_ASM_RELAX_SPEC "%{!m1:%{!m2:%{!m3*:-isa=sh4-up}}}"
#else
#define SUBTARGET_ASM_RELAX_SPEC "%{m4*:-isa=sh4-up}"
#endif

/* Define which ISA type to pass to the assembler.
   For SH4 we pass SH4A to allow using some instructions that are available
   on some SH4 variants, but officially are part of the SH4A ISA.  */
#define SH_ASM_SPEC \
 "%(subtarget_asm_endian_spec) %{mrelax:-relax %(subtarget_asm_relax_spec)} \
%(subtarget_asm_isa_spec) %(subtarget_asm_spec) \
%{m1:--isa=sh} \
%{m2:--isa=sh2} \
%{m2e:--isa=sh2e} \
%{m3:--isa=sh3} \
%{m3e:--isa=sh3e} \
%{m4:--isa=sh4a} \
%{m4-single:--isa=sh4a} \
%{m4-single-only:--isa=sh4a} \
%{m4-nofpu:--isa=sh4a-nofpu} \
%{m4a:--isa=sh4a} \
%{m4a-single:--isa=sh4a} \
%{m4a-single-only:--isa=sh4a} \
%{m4a-nofpu:--isa=sh4a-nofpu} \
%{m2a:--isa=sh2a} \
%{m2a-single:--isa=sh2a} \
%{m2a-single-only:--isa=sh2a} \
%{m2a-nofpu:--isa=sh2a-nofpu} \
%{m4al:-dsp}"

#define ASM_SPEC SH_ASM_SPEC

#ifndef SUBTARGET_ASM_ENDIAN_SPEC
#if TARGET_ENDIAN_DEFAULT == MASK_LITTLE_ENDIAN
#define SUBTARGET_ASM_ENDIAN_SPEC "%{mb:-big} %{!mb:-little}"
#else
#define SUBTARGET_ASM_ENDIAN_SPEC "%{ml:-little} %{!ml:-big}"
#endif
#endif

#if STRICT_NOFPU == 1
/* Strict nofpu means that the compiler should tell the assembler
   to reject FPU instructions. E.g. from ASM inserts.  */
#if TARGET_CPU_DEFAULT & MASK_HARD_SH4 && !(TARGET_CPU_DEFAULT & MASK_SH_E)
#define SUBTARGET_ASM_ISA_SPEC "%{!m1:%{!m2:%{!m3*:%{m4-nofpu|!m4*:-isa=sh4-nofpu}}}}"
#else

#define SUBTARGET_ASM_ISA_SPEC \
 "%{m4-nofpu:-isa=sh4-nofpu} " ASM_ISA_DEFAULT_SPEC
#endif
#else /* ! STRICT_NOFPU */
#define SUBTARGET_ASM_ISA_SPEC ASM_ISA_DEFAULT_SPEC
#endif

#ifndef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "%{mfdpic:--fdpic}"
#endif

#if TARGET_ENDIAN_DEFAULT == MASK_LITTLE_ENDIAN
#define LINK_EMUL_PREFIX "sh%{!mb:l}"
#else
#define LINK_EMUL_PREFIX "sh%{ml:l}"
#endif

#define LINK_DEFAULT_CPU_EMUL ""
#define ASM_ISA_DEFAULT_SPEC ""

#define SUBTARGET_LINK_EMUL_SUFFIX "%{mfdpic:_fd}"
#define SUBTARGET_LINK_SPEC ""

/* Go via SH_LINK_SPEC to avoid code replication.  */
#define LINK_SPEC SH_LINK_SPEC

#define SH_LINK_SPEC "\
-m %(link_emul_prefix)\
%{!m1:%{!m2:%{!m3*:%{!m4*:%(link_default_cpu_emul)}}}}\
%(subtarget_link_emul_suffix) \
%{mrelax:-relax} %(subtarget_link_spec)"

#ifndef SH_DIV_STR_FOR_SIZE
#define SH_DIV_STR_FOR_SIZE "call"
#endif

/* SH2A does not support little-endian.  Catch such combinations
   taking into account the default configuration.  */
#if TARGET_ENDIAN_DEFAULT == MASK_BIG_ENDIAN
#define IS_LITTLE_ENDIAN_OPTION "%{ml:"
#else
#define IS_LITTLE_ENDIAN_OPTION "%{!mb:"
#endif
 
#if TARGET_CPU_DEFAULT & MASK_HARD_SH2A
#define UNSUPPORTED_SH2A IS_LITTLE_ENDIAN_OPTION \
"%{m2a*|!m1:%{!m2*:%{!m3*:%{!m4*:%eSH2a does not support little-endian}}}}}"
#else
#define UNSUPPORTED_SH2A IS_LITTLE_ENDIAN_OPTION \
"%{m2a*:%eSH2a does not support little-endian}}"
#endif

#ifdef FDPIC_DEFAULT
#define FDPIC_SELF_SPECS "%{!mno-fdpic:-mfdpic}"
#else
#define FDPIC_SELF_SPECS
#endif

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS UNSUPPORTED_SH2A SUBTARGET_DRIVER_SELF_SPECS \
  FDPIC_SELF_SPECS

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS

#define ASSEMBLER_DIALECT assembler_dialect

extern int assembler_dialect;

enum sh_divide_strategy_e {
  /* SH1 .. SH4 strategies.  Because of the small number of registers
     available, the compiler uses knowledge of the actual set of registers
     being clobbered by the different functions called.  */
  SH_DIV_CALL_DIV1, /* No FPU, medium size, highest latency.  */
  SH_DIV_CALL_FP,     /* FPU needed, small size, high latency.  */
  SH_DIV_CALL_TABLE,  /* No FPU, large size, medium latency. */
  SH_DIV_INTRINSIC
};

extern enum sh_divide_strategy_e sh_div_strategy;

#ifndef SH_DIV_STRATEGY_DEFAULT
#define SH_DIV_STRATEGY_DEFAULT SH_DIV_CALL_DIV1
#endif

#ifdef __cplusplus

/* Atomic model.  */
struct sh_atomic_model
{
  enum enum_type
  {
    none = 0,
    soft_gusa,
    hard_llcs,
    soft_tcb,
    soft_imask,

    num_models
  };

  /*  If strict is set, disallow mixing of different models, as it would
      happen on SH4A.  */
  bool strict;
  enum_type type;

  /* Name string as it was specified on the command line.  */
  const char* name;

  /* Name string as it is used in C/C++ defines.  */
  const char* cdef_name;

  /* GBR offset variable for TCB model.  */
  int tcb_gbr_offset;
};

extern const sh_atomic_model& selected_atomic_model (void);

/* Shortcuts to check the currently selected atomic model.  */
#define TARGET_ATOMIC_ANY \
  (selected_atomic_model ().type != sh_atomic_model::none)

#define TARGET_ATOMIC_STRICT \
  (selected_atomic_model ().strict)

#define TARGET_ATOMIC_SOFT_GUSA \
  (selected_atomic_model ().type == sh_atomic_model::soft_gusa)

#define TARGET_ATOMIC_HARD_LLCS \
  (selected_atomic_model ().type == sh_atomic_model::hard_llcs)

#define TARGET_ATOMIC_SOFT_TCB \
  (selected_atomic_model ().type == sh_atomic_model::soft_tcb)

#define TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX \
  GEN_INT (selected_atomic_model ().tcb_gbr_offset)

#define TARGET_ATOMIC_SOFT_IMASK \
  (selected_atomic_model ().type == sh_atomic_model::soft_imask)

#endif // __cplusplus

#define SUBTARGET_OVERRIDE_OPTIONS (void) 0


/* Target machine storage layout.  */

#define TARGET_BIG_ENDIAN (!TARGET_LITTLE_ENDIAN)

#define SH_REG_MSW_OFFSET (TARGET_LITTLE_ENDIAN ? 1 : 0)
#define SH_REG_LSW_OFFSET (TARGET_LITTLE_ENDIAN ? 0 : 1)

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN TARGET_BIG_ENDIAN

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN TARGET_BIG_ENDIAN

#define MAX_BITS_PER_WORD 64

/* Width in bits of an `int'.  We want just 32-bits, even if words are
   longer.  */
#define INT_TYPE_SIZE 32

/* Width in bits of a `long'.  */
#define LONG_TYPE_SIZE (32)

/* Width in bits of a `long long'.  */
#define LONG_LONG_TYPE_SIZE 64

/* Width in bits of a `long double'.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD	(4)
#define MIN_UNITS_PER_WORD 4

/* Scaling factor for Dwarf data offsets for CFI information.
   The dwarf2out.cc default would use -UNITS_PER_WORD.  */
#define DWARF_CIE_DATA_ALIGNMENT -4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE  (32)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY  	(32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  BIGGEST_ALIGNMENT

/* The log (base 2) of the cache line size, in bytes.  Processors prior to
   SH2 have no actual cache, but they fetch code in chunks of 4 bytes.
   The SH2/3 have 16 byte cache lines, and the SH4 has a 32 byte cache line */
#define CACHE_LOG (TARGET_HARD_SH4 ? 5 : TARGET_SH2 ? 4 : 2)

/* ABI given & required minimum allocation boundary (in *bits*) for the
   code of a function.  */
#define FUNCTION_BOUNDARY (16)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  (TARGET_ALIGN_DOUBLE ? 64 : 32)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT (32)

/* get_mode_alignment assumes complex values are always held in multiple
   registers, but that is not the case on the SH; CQImode and CHImode are
   held in a single integer register.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  ((GET_MODE_CLASS (TYPE_MODE (TYPE)) == MODE_COMPLEX_INT \
    || GET_MODE_CLASS (TYPE_MODE (TYPE)) == MODE_COMPLEX_FLOAT) \
   ? (unsigned) MIN (BIGGEST_ALIGNMENT, \
		     GET_MODE_BITSIZE (as_a <fixed_size_mode> \
				       (TYPE_MODE (TYPE)))) \
   : (unsigned) DATA_ALIGNMENT(TYPE, ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Number of bits which any structure or union's size must be a
   multiple of.  Each structure or union's size is rounded up to a
   multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY (TARGET_PADSTRUCT ? 32 : 8)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* If LABEL_AFTER_BARRIER demands an alignment, return its base 2 logarithm.  */
#define LABEL_ALIGN_AFTER_BARRIER(LABEL_AFTER_BARRIER) \
  barrier_align (LABEL_AFTER_BARRIER)

#define LOOP_ALIGN(A_LABEL) sh_loop_align (A_LABEL)

#define LABEL_ALIGN(A_LABEL) \
(									\
  (PREV_INSN (A_LABEL)							\
   && NONJUMP_INSN_P (PREV_INSN (A_LABEL))				\
   && GET_CODE (PATTERN (PREV_INSN (A_LABEL))) == UNSPEC_VOLATILE	\
   && XINT (PATTERN (PREV_INSN (A_LABEL)), 1) == UNSPECV_ALIGN)		\
   /* explicit alignment insn in constant tables.  */			\
  ? INTVAL (XVECEXP (PATTERN (PREV_INSN (A_LABEL)), 0, 0))		\
  : 0)

/* Jump tables must be 32 bit aligned, no matter the size of the element.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 2

/* The base two logarithm of the known minimum alignment of an insn length.  */
#define INSN_LENGTH_ALIGNMENT(A_INSN)		\
  (NONJUMP_INSN_P (A_INSN)			\
   ? 1						\
   : JUMP_P (A_INSN) || CALL_P (A_INSN)		\
   ? 1						\
   : CACHE_LOG)

/* Standard register usage.  */

/* Register allocation for the Renesas calling convention:

	r0		arg return
	r1..r3		scratch
	r4..r7		args in
	r8..r13		call saved
	r14		frame pointer/call saved
	r15		stack pointer
	ap		arg pointer (doesn't really exist, always eliminated)
	pr		subroutine return address
	t		t bit
	mach		multiply/accumulate result, high part
	macl		multiply/accumulate result, low part.
	fpul		fp/int communication register
	rap		return address pointer register
	fr0		fp arg return
	fr1..fr3	scratch floating point registers
	fr4..fr11	fp args in
	fr12..fr15	call saved floating point registers  */

#define MAX_REGISTER_NAME_LENGTH 6
extern char sh_register_names[][MAX_REGISTER_NAME_LENGTH + 1];

#define SH_REGISTER_NAMES_INITIALIZER					\
{									\
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7", 	\
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",	\
  "r16",  "r17",  "r18",  "r19",  "r20",  "r21",  "r22",  "r23",	\
  "r24",  "r25",  "r26",  "r27",  "r28",  "r29",  "r30",  "r31",	\
  "r32",  "r33",  "r34",  "r35",  "r36",  "r37",  "r38",  "r39", 	\
  "r40",  "r41",  "r42",  "r43",  "r44",  "r45",  "r46",  "r47",	\
  "r48",  "r49",  "r50",  "r51",  "r52",  "r53",  "r54",  "r55",	\
  "r56",  "r57",  "r58",  "r59",  "r60",  "r61",  "r62",  "r63",	\
  "fr0",  "fr1",  "fr2",  "fr3",  "fr4",  "fr5",  "fr6",  "fr7", 	\
  "fr8",  "fr9",  "fr10", "fr11", "fr12", "fr13", "fr14", "fr15",	\
  "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23",	\
  "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31",	\
  "fr32", "fr33", "fr34", "fr35", "fr36", "fr37", "fr38", "fr39", 	\
  "fr40", "fr41", "fr42", "fr43", "fr44", "fr45", "fr46", "fr47",	\
  "fr48", "fr49", "fr50", "fr51", "fr52", "fr53", "fr54", "fr55",	\
  "fr56", "fr57", "fr58", "fr59", "fr60", "fr61", "fr62", "fr63",	\
  "tr0",  "tr1",  "tr2",  "tr3",  "tr4",  "tr5",  "tr6",  "tr7", 	\
  "xd0",  "xd2",  "xd4",  "xd6",  "xd8",  "xd10", "xd12", "xd14",	\
  "gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr",	\
  "rap",  "sfp", "fpscr0", "fpscr1"					\
}

#define REGNAMES_ARR_INDEX_1(index) \
  (sh_register_names[index])
#define REGNAMES_ARR_INDEX_2(index) \
  REGNAMES_ARR_INDEX_1 ((index)), REGNAMES_ARR_INDEX_1 ((index)+1)
#define REGNAMES_ARR_INDEX_4(index) \
  REGNAMES_ARR_INDEX_2 ((index)), REGNAMES_ARR_INDEX_2 ((index)+2)
#define REGNAMES_ARR_INDEX_8(index) \
  REGNAMES_ARR_INDEX_4 ((index)), REGNAMES_ARR_INDEX_4 ((index)+4)
#define REGNAMES_ARR_INDEX_16(index) \
  REGNAMES_ARR_INDEX_8 ((index)), REGNAMES_ARR_INDEX_8 ((index)+8)
#define REGNAMES_ARR_INDEX_32(index) \
  REGNAMES_ARR_INDEX_16 ((index)), REGNAMES_ARR_INDEX_16 ((index)+16)
#define REGNAMES_ARR_INDEX_64(index) \
  REGNAMES_ARR_INDEX_32 ((index)), REGNAMES_ARR_INDEX_32 ((index)+32)

#define REGISTER_NAMES \
{ \
  REGNAMES_ARR_INDEX_64 (0), \
  REGNAMES_ARR_INDEX_64 (64), \
  REGNAMES_ARR_INDEX_8 (128), \
  REGNAMES_ARR_INDEX_8 (136), \
  REGNAMES_ARR_INDEX_8 (144), \
  REGNAMES_ARR_INDEX_4 (152) \
}

#define ADDREGNAMES_SIZE 32
#define MAX_ADDITIONAL_REGISTER_NAME_LENGTH 4
extern char sh_additional_register_names[ADDREGNAMES_SIZE] \
  [MAX_ADDITIONAL_REGISTER_NAME_LENGTH + 1];

#define SH_ADDITIONAL_REGISTER_NAMES_INITIALIZER			\
{									\
  "dr0",  "dr2",  "dr4",  "dr6",  "dr8",  "dr10", "dr12", "dr14",	\
  "dr16", "dr18", "dr20", "dr22", "dr24", "dr26", "dr28", "dr30",	\
  "dr32", "dr34", "dr36", "dr38", "dr40", "dr42", "dr44", "dr46",	\
  "dr48", "dr50", "dr52", "dr54", "dr56", "dr58", "dr60", "dr62"	\
}

#define ADDREGNAMES_REGNO(index) \
  ((index < 32) ? (FIRST_FP_REG + (index) * 2) \
   : (-1))

#define ADDREGNAMES_ARR_INDEX_1(index) \
  { (sh_additional_register_names[index]), ADDREGNAMES_REGNO (index) }
#define ADDREGNAMES_ARR_INDEX_2(index) \
  ADDREGNAMES_ARR_INDEX_1 ((index)), ADDREGNAMES_ARR_INDEX_1 ((index)+1)
#define ADDREGNAMES_ARR_INDEX_4(index) \
  ADDREGNAMES_ARR_INDEX_2 ((index)), ADDREGNAMES_ARR_INDEX_2 ((index)+2)
#define ADDREGNAMES_ARR_INDEX_8(index) \
  ADDREGNAMES_ARR_INDEX_4 ((index)), ADDREGNAMES_ARR_INDEX_4 ((index)+4)
#define ADDREGNAMES_ARR_INDEX_16(index) \
  ADDREGNAMES_ARR_INDEX_8 ((index)), ADDREGNAMES_ARR_INDEX_8 ((index)+8)
#define ADDREGNAMES_ARR_INDEX_32(index) \
  ADDREGNAMES_ARR_INDEX_16 ((index)), ADDREGNAMES_ARR_INDEX_16 ((index)+16)

#define ADDITIONAL_REGISTER_NAMES \
{					\
  ADDREGNAMES_ARR_INDEX_32 (0)		\
}

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

/* There are many other relevant definitions in sh.md's md_constants.  */

#define FIRST_GENERAL_REG R0_REG
#define LAST_GENERAL_REG (FIRST_GENERAL_REG + (15))
#define FIRST_FP_REG DR0_REG
#define LAST_FP_REG  (FIRST_FP_REG + (TARGET_SH2E ? 15 : -1))
#define FIRST_XD_REG XD0_REG
#define LAST_XD_REG  (FIRST_XD_REG + ((TARGET_SH4 && TARGET_FMOVD) ? 7 : -1))

/* Registers that can be accessed through bank0 or bank1 depending on sr.md.  */
#define FIRST_BANKED_REG R0_REG
#define LAST_BANKED_REG R7_REG

#define BANKED_REGISTER_P(REGNO) \
  IN_RANGE ((REGNO), \
	    (unsigned HOST_WIDE_INT) FIRST_BANKED_REG, \
	    (unsigned HOST_WIDE_INT) LAST_BANKED_REG)

#define GENERAL_REGISTER_P(REGNO) \
  IN_RANGE ((REGNO), \
	    (unsigned HOST_WIDE_INT) FIRST_GENERAL_REG, \
	    (unsigned HOST_WIDE_INT) LAST_GENERAL_REG)

#define GENERAL_OR_AP_REGISTER_P(REGNO) \
  (GENERAL_REGISTER_P (REGNO) || ((REGNO) == AP_REG) \
   || ((REGNO) == FRAME_POINTER_REGNUM))

#define FP_REGISTER_P(REGNO) \
  ((int) (REGNO) >= FIRST_FP_REG && (int) (REGNO) <= LAST_FP_REG)

#define XD_REGISTER_P(REGNO) \
  ((int) (REGNO) >= FIRST_XD_REG && (int) (REGNO) <= LAST_XD_REG)

#define FP_OR_XD_REGISTER_P(REGNO) \
  (FP_REGISTER_P (REGNO) || XD_REGISTER_P (REGNO))

#define FP_ANY_REGISTER_P(REGNO) \
  (FP_REGISTER_P (REGNO) || XD_REGISTER_P (REGNO) || (REGNO) == FPUL_REG)

#define SPECIAL_REGISTER_P(REGNO) \
  ((REGNO) == GBR_REG || (REGNO) == T_REG \
   || (REGNO) == MACH_REG || (REGNO) == MACL_REG \
   || (REGNO) == FPSCR_MODES_REG || (REGNO) == FPSCR_STAT_REG)

#define VALID_REGISTER_P(REGNO) \
  (GENERAL_REGISTER_P (REGNO) || FP_REGISTER_P (REGNO) \
   || XD_REGISTER_P (REGNO) \
   || (REGNO) == AP_REG || (REGNO) == RAP_REG \
   || (REGNO) == FRAME_POINTER_REGNUM \
   || ((SPECIAL_REGISTER_P (REGNO) || (REGNO) == PR_REG)) \
   || (TARGET_SH2E && (REGNO) == FPUL_REG))

/* The mode that should be generally used to store a register by
   itself in the stack, or to load it back.  */
#define REGISTER_NATURAL_MODE(REGNO) \
  (FP_REGISTER_P (REGNO) ? E_SFmode \
   : XD_REGISTER_P (REGNO) ? E_DFmode : E_SImode)


#define FIRST_PSEUDO_REGISTER 156

/* Don't count soft frame pointer.  */
#define DWARF_FRAME_REGISTERS (153)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   Mach register is fixed 'cause it's only 10 bits wide for SH1.
   It is 32 bits wide for SH2.  */
#define FIXED_REGISTERS							\
{									\
/* Regular registers.  */						\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      1,		\
  /* r16 is reserved, r18 is the former pr.  */				\
  1,      0,      0,      0,      0,      0,      0,      0,		\
  /* r24 is reserved for the OS; r25, for the assembler or linker.  */	\
  /* r26 is a global variable data pointer; r27 is for constants.  */	\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      1,		\
/* FP registers.  */							\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* Branch target registers.  */						\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* XD registers.  */							\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/*"gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr", */	\
  1,      1,      1,      1,      1,      1,      0,      1,		\
/*"rap",  "sfp","fpscr0","fpscr1"  */					\
  1,      1,      1,      1,						\
}

/* CALL_REALLY_USED_REGISTERS is used as a default setting, which is then
   overridden by -fcall-saved-* and -fcall-used-* options and then by
   TARGET_CONDITIONAL_REGISTER_USAGE.  There we might want to make a
   register call-used, yet fixed, like PIC_OFFSET_TABLE_REGNUM.  */
#define CALL_REALLY_USED_REGISTERS 					\
{									\
/* Regular registers.  */						\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  /* R8 and R9 are call-clobbered on SH5, but not on earlier SH ABIs.	\
     Only the lower 32bits of R10-R14 are guaranteed to be preserved	\
     across SH5 function calls.  */					\
  0,      0,      0,      0,      0,      0,      0,      1,		\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      1,      1,      1,      1,		\
/* FP registers.  */							\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* Branch target registers.  */						\
  1,      1,      1,      1,      1,      0,      0,      0,		\
/* XD registers.  */							\
  1,      1,      1,      1,      1,      1,      0,      0,		\
/*"gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr", */	\
  0,      1,      1,      1,      1,      1,      1,      1,		\
/*"rap",  "sfp","fpscr0","fpscr1"  */					\
  1,      1,      0,      0,						\
}

/* Specify the modes required to caller save a given hard regno.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE)	\
  sh_hard_regno_caller_save_mode ((REGNO), (NREGS), (MODE))

/* A C expression that is nonzero if hard register NEW_REG can be
   considered for use as a rename register for OLD_REG register */
#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
   sh_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
/* #define PC_REGNUM		15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	SP_REG

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM	FP_REG

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	153

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM RAP_REG

/* Register to hold the addressing base for position independent
   code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM	(flag_pic ? PIC_REG : INVALID_REGNUM)

/* For FDPIC, the FDPIC register is call-clobbered (otherwise PLT
   entries would need to handle saving and restoring it).  */
#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED TARGET_FDPIC

#define GOT_SYMBOL_NAME "*_GLOBAL_OFFSET_TABLE_"

/* Definitions for register eliminations.

   We have three registers that can be eliminated on the SH.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.
   Third, there is the return address pointer, which can also be replaced
   with either the stack or the frame pointer.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   If you add any registers here that are not actually hard registers,
   and that have any alternative of elimination that doesn't always
   apply, you need to amend calc_live_regs to exclude it, because
   reload spills all eliminable registers where it sees an
   can_eliminate == 0 entry, thus making them 'live' .
   If you add any hard registers that can be eliminated in different
   ways, you have to patch reload to spill them only when all alternatives
   of elimination fail.  */
#define ELIMINABLE_REGS						\
{{ HARD_FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = initial_elimination_offset ((FROM), (TO))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	AP_REG

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM	(3)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the TARGET_RETURN_IN_MEMORY
   target hook.  */
#define DEFAULT_PCC_STRUCT_RETURN 0


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.

   The SH has two sorts of general registers, R0 and the rest.  R0 can
   be used as the destination of some of the arithmetic ops. There are
   also some special purpose registers; the T bit register, the
   Procedure Return Register and the Multiply Accumulate Registers.

   Place GENERAL_REGS after FPUL_REGS so that it will be preferred by
   reg_class_subunion.  We don't want to have an actual union class
   of these, because it would only be used when both classes are calculated
   to give the same cost, but there is only one FPUL register.
   Besides, regclass fails to notice the different REGISTER_MOVE_COSTS
   applying to the actual instruction alternative considered.  E.g., the
   y/r alternative of movsi_ie is considered to have no more cost that
   the r/r alternative, which is patently untrue.  */
enum reg_class
{
  NO_REGS,
  R0_REGS,
  PR_REGS,
  T_REGS,
  MAC_REGS,
  FPUL_REGS,
  SIBCALL_REGS,
  NON_SP_REGS,
  GENERAL_REGS,
  FP0_REGS,
  FP_REGS,
  DF_REGS,
  FPSCR_REGS,
  GENERAL_FP_REGS,
  GENERAL_DF_REGS,
  TARGET_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES	\
{			\
  "NO_REGS",		\
  "R0_REGS",		\
  "PR_REGS",		\
  "T_REGS",		\
  "MAC_REGS",		\
  "FPUL_REGS",		\
  "SIBCALL_REGS",	\
  "NON_SP_REGS",	\
  "GENERAL_REGS",	\
  "FP0_REGS",		\
  "FP_REGS",		\
  "DF_REGS",		\
  "FPSCR_REGS",		\
  "GENERAL_FP_REGS",	\
  "GENERAL_DF_REGS",	\
  "TARGET_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */
#define REG_CLASS_CONTENTS						\
{									\
/* NO_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* R0_REGS:  */								\
  { 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* PR_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00040000 },	\
/* T_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00080000 },	\
/* MAC_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00300000 },	\
/* FPUL_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00400000 },	\
/* SIBCALL_REGS: Initialized in TARGET_CONDITIONAL_REGISTER_USAGE.  */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* NON_SP_REGS:  */							\
  { 0xffff7fff, 0xffffffff, 0x00000000, 0x00000000, 0x03020000 },	\
/* GENERAL_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0x00000000, 0x00000000, 0x03020000 },	\
/* FP0_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000 },	\
/* FP_REGS:  */								\
  { 0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x00000000 },	\
/* DF_REGS:  */								\
  { 0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x0000ff00 },	\
/* FPSCR_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00800000 },	\
/* GENERAL_FP_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x03020000 },	\
/* GENERAL_DF_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x0302ff00 },	\
/* TARGET_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x000000ff },	\
/* ALL_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x0fffffff },	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
extern enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[(REGNO)]

/* When this hook returns true for MODE, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers.  */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P \
  sh_small_register_classes_for_mode_p

/* The order in which register should be allocated.  */
/* Sometimes FP0_REGS becomes the preferred class of a floating point pseudo,
   and GENERAL_FP_REGS the alternate class.  Since FP0 is likely to be
   spilled or used otherwise, we better have the FP_REGS allocated first.  */
#define REG_ALLOC_ORDER \
  {/* Caller-saved FPRs */ \
    65, 66, 67, 68, 69, 70, 71, 64, \
    72, 73, 74, 75, 80, 81, 82, 83, \
    84, 85, 86, 87, 88, 89, 90, 91, \
    92, 93, 94, 95, 96, 97, 98, 99, \
   /* Callee-saved FPRs */ \
    76, 77, 78, 79,100,101,102,103, \
   104,105,106,107,108,109,110,111, \
   112,113,114,115,116,117,118,119, \
   120,121,122,123,124,125,126,127, \
   136,137,138,139,140,141,142,143, \
   /* FPSCR */ 151, \
   /* Caller-saved GPRs (except 8/9 on SH1-4) */ \
     1,  2,  3,  7,  6,  5,  4,  0, \
     8,  9, 17, 19, 20, 21, 22, 23, \
    36, 37, 38, 39, 40, 41, 42, 43, \
    60, 61, 62, \
   /* SH1-4 callee-saved saved GPRs / SH5 partially-saved GPRs */ \
    10, 11, 12, 13, 14, 18, \
    /* SH5 callee-saved GPRs */ \
    28, 29, 30, 31, 32, 33, 34, 35, \
    44, 45, 46, 47, 48, 49, 50, 51, \
    52, 53, 54, 55, 56, 57, 58, 59, \
   /* FPUL */ 150, \
   /* Fixed registers */ \
    15, 16, 24, 25, 26, 27, 63,144, \
   145,146,147,148,149,152,153,154,155  }

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS R0_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Defines for sh.md and constraints.md.  */

#define CONST_OK_FOR_I08(VALUE) (((HOST_WIDE_INT)(VALUE))>= -128 \
				 && ((HOST_WIDE_INT)(VALUE)) <= 127)

#define CONST_OK_FOR_K08(VALUE) (((HOST_WIDE_INT)(VALUE))>= 0 \
				 && ((HOST_WIDE_INT)(VALUE)) <= 255)

#define ZERO_EXTRACT_ANDMASK(EXTRACT_SZ_RTX, EXTRACT_POS_RTX)\
  (((1 << INTVAL (EXTRACT_SZ_RTX)) - 1) << INTVAL (EXTRACT_POS_RTX))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   If TARGET_SHMEDIA, we need two FP registers per word.
   Otherwise we will need at most one register per word.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define the number of registers that can hold parameters.
   These macros are used only in other macro definitions below.  */
#define NPARM_REGS(MODE) \
  (TARGET_FPU_ANY && (MODE) == SFmode \
   ? 8 \
   : TARGET_FPU_DOUBLE \
     && (GET_MODE_CLASS (MODE) == MODE_FLOAT \
	 || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
   ? 8 \
   : 4)

#define FIRST_PARM_REG (FIRST_GENERAL_REG + 4)
#define FIRST_RET_REG  (FIRST_GENERAL_REG + 0)

#define FIRST_FP_PARM_REG (FIRST_FP_REG + 4)
#define FIRST_FP_RET_REG FIRST_FP_REG

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/*  Define this macro to nonzero if the addresses of local variable slots
    are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* Don't define PUSH_ROUNDING, since the hardware doesn't do this.
   When PUSH_ROUNDING is not defined, PARM_BOUNDARY will cause gcc to
   do correct alignment.  */
#if 0
#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)
#endif

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of bytes of arguments automatically popped when
   calling a subroutine.
   CUM is the accumulated argument list.  */
#define CALL_POPS_ARGS(CUM) (0)

/* Some subroutine macros specific to this machine.  */

#define BASE_RETURN_VALUE_REG(MODE) \
  ((TARGET_FPU_ANY && ((MODE) == SFmode))		\
   ? FIRST_FP_RET_REG					\
   : TARGET_FPU_ANY && (MODE) == SCmode			\
   ? FIRST_FP_RET_REG					\
   : (TARGET_FPU_DOUBLE					\
      && ((MODE) == DFmode || (MODE) == SFmode		\
	  || (MODE) == DCmode || (MODE) == SCmode ))	\
   ? FIRST_FP_RET_REG					\
   : FIRST_RET_REG)

#define BASE_ARG_REG(MODE) \
  ((TARGET_SH2E && ((MODE) == SFmode))			\
   ? FIRST_FP_PARM_REG					\
   : TARGET_FPU_DOUBLE					\
     && (GET_MODE_CLASS (MODE) == MODE_FLOAT		\
	 || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)\
   ? FIRST_FP_PARM_REG					\
   : FIRST_PARM_REG)

/* 1 if N is a possible register number for function argument passing.  */
/* ??? There are some callers that pass REGNO as int, and others that pass
   it as unsigned.  We get warnings unless we do casts everywhere.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (((unsigned) (REGNO) >= (unsigned) FIRST_PARM_REG			\
    && (unsigned) (REGNO) < (unsigned) (FIRST_PARM_REG + NPARM_REGS (SImode)))\
   || (TARGET_FPU_ANY							\
       && (unsigned) (REGNO) >= (unsigned) FIRST_FP_PARM_REG		\
       && (unsigned) (REGNO) < (unsigned) (FIRST_FP_PARM_REG		\
					   + NPARM_REGS (SFmode))))

#ifdef __cplusplus

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SH, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */

enum sh_arg_class { SH_ARG_INT = 0, SH_ARG_FLOAT = 1 };

struct sh_args
{
  /* How many SH_ARG_INT and how many SH_ARG_FLOAT args there are.  */
  int arg_count[2];

  bool force_mem;

  /* Nonzero if a prototype is available for the function.  */
  bool prototype_p;

  /* The number of an odd floating-point register, that should be used
     for the next argument of type float.  */
  int free_single_fp_reg;

  /* Whether we're processing an outgoing function call.  */
  bool outgoing;

  /* This is set to nonzero when the call in question must use the Renesas ABI,
     even without the -mrenesas option.  */
  bool renesas_abi;
};

typedef sh_args CUMULATIVE_ARGS;

/* Set when processing a function with interrupt attribute.  */
extern bool current_function_interrupt;

#endif // __cplusplus

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SH, the offset always starts at 0: the first parm reg is always
   the same reg for a given argument class.

   For TARGET_HITACHI, the structure value pointer is passed in memory.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  sh_init_cumulative_args (& (CUM), (FNTYPE), (LIBNAME), (FNDECL),\
			   (N_NAMED_ARGS), VOIDmode)

#define INIT_CUMULATIVE_LIBCALL_ARGS(CUM, MODE, LIBNAME) \
  sh_init_cumulative_args (& (CUM), NULL_TREE, (LIBNAME), NULL_TREE, 0, (MODE))

/* By accident we got stuck with passing SCmode on SH4 little endian
   in two registers that are nominally successive - which is different from
   two single SFmode values, where we take endianness translation into
   account.  That does not work at all if an odd number of registers is
   already in use, so that got fixed, but library functions are still more
   likely to use complex numbers without mixing them with SFmode arguments
   (which in C would have to be structures), so for the sake of ABI
   compatibility the way SCmode values are passed when an even number of
   FP registers is in use remains different from a pair of SFmode values for
   now.
   I.e.:
   foo (double); a: fr5,fr4
   foo (float a, float b); a: fr5 b: fr4
   foo (__complex float a); a.real fr4 a.imag: fr5 - for consistency,
			    this should be the other way round...
   foo (float a, __complex float b); a: fr5 b.real: fr4 b.imag: fr7  */
#define FUNCTION_ARG_SCmode_WART 1

/* Minimum alignment for an argument to be passed by callee-copy
   reference.  We need such arguments to be aligned to 8 byte
   boundaries, because they'll be loaded using quad loads.  */
#define SH_MIN_ALIGN_FOR_CALLEE_COPY (8 * BITS_PER_UNIT)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.  */

/* Call the function profiler with a given profile label.
   We use two .aligns, so as to make sure that both the .long is aligned
   on a 4 byte boundary, and that the .long is a fixed distance (2 bytes)
   from the trapa instruction.  */
#define FUNCTION_PROFILER(STREAM,LABELNO)			\
{								\
  fprintf((STREAM), "\t.align\t2\n");				\
  fprintf((STREAM), "\ttrapa\t#33\n");				\
  fprintf((STREAM), "\t.align\t2\n");				\
  asm_fprintf((STREAM), "\t.long\t%LLP%d\n", (LABELNO));	\
}

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */
#define PROFILE_BEFORE_PROLOGUE

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

/*
   On the SH, the trampoline looks like
   2 0002 D202			mov.l	l2,r2
   1 0000 D301			mov.l	l1,r3
   3 0004 422B			jmp	@r2
   4 0006 0009			nop
   5 0008 00000000 	l1:  	.long   area
   6 000c 00000000 	l2:	.long   function  */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE (TARGET_FDPIC ? 32 : 16)

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT \
  ((CACHE_LOG < 3 \
    || (optimize_size && ! (TARGET_HARD_SH4))) ? 32 \
   : 64)

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, so we
   can ignore COUNT.  */
#define RETURN_ADDR_RTX(COUNT, FRAME)	\
  (((COUNT) == 0) ? sh_get_pr_initial_val () : NULL_RTX)

/* A C expression whose value is RTL representing the location of the
   incoming return address at the beginning of any function, before the
   prologue.  This RTL is either a REG, indicating that the return
   value is saved in REG, or a MEM representing a location in
   the stack.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, PR_REG)

/* Addressing modes, and classification of registers for them.  */
#define HAVE_POST_INCREMENT  TARGET_SH1
#define HAVE_PRE_DECREMENT   TARGET_SH1

#define USE_LOAD_POST_INCREMENT(mode) TARGET_SH1
#define USE_LOAD_PRE_DECREMENT(mode) TARGET_SH2A
#define USE_STORE_POST_INCREMENT(mode) TARGET_SH2A
#define USE_STORE_PRE_DECREMENT(mode) TARGET_SH1

/* If a memory clear move would take CLEAR_RATIO or more simple
   move-instruction pairs, we will do a setmem instead.  */

#define CLEAR_RATIO(speed) ((speed) ? 15 : 3)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.cc during register
   allocation.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
  (GENERAL_OR_AP_REGISTER_P (REGNO) \
   || GENERAL_OR_AP_REGISTER_P (reg_renumber[(REGNO)]))
#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) == R0_REG || (unsigned) reg_renumber[(REGNO)] == R0_REG)

/* True if SYMBOL + OFFSET constants must refer to something within
   SYMBOL's section.  */
#define SH_OFFSETS_MUST_BE_WITHIN_SECTIONS_P TARGET_FDPIC

/* Maximum number of registers that can appear in a valid memory
   address.  */
#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X)	(GET_CODE (X) == LABEL_REF)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   The suitable hard regs are always accepted and all pseudo regs
   are also accepted if STRICT is not set.  */

/* Nonzero if X is a reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X, STRICT)			\
  (GENERAL_OR_AP_REGISTER_P (REGNO (X))			\
   || (!STRICT && REGNO (X) >= FIRST_PSEUDO_REGISTER))

/* Nonzero if X is a reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X, STRICT)			\
  ((REGNO (X) == R0_REG)				\
   || (!STRICT && REGNO (X) >= FIRST_PSEUDO_REGISTER))

/* Nonzero if X/OFFSET is a reg that can be used as an index.  */
#define SUBREG_OK_FOR_INDEX_P(X, OFFSET, STRICT)	\
  ((REGNO (X) == R0_REG && OFFSET == 0)			\
   || (!STRICT && REGNO (X) >= FIRST_PSEUDO_REGISTER))

/* Macros for extra constraints.  */

#define IS_PC_RELATIVE_LOAD_ADDR_P(OP)					\
  ((GET_CODE ((OP)) == LABEL_REF)					\
   || (GET_CODE ((OP)) == CONST						\
       && GET_CODE (XEXP ((OP), 0)) == PLUS				\
       && GET_CODE (XEXP (XEXP ((OP), 0), 0)) == LABEL_REF		\
       && CONST_INT_P (XEXP (XEXP ((OP), 0), 1))))

#define IS_NON_EXPLICIT_CONSTANT_P(OP)					\
  (CONSTANT_P (OP)							\
   && !CONST_INT_P (OP)							\
   && GET_CODE (OP) != CONST_DOUBLE					\
   && (!flag_pic							\
       || (LEGITIMATE_PIC_OPERAND_P (OP)				\
	   && !PIC_ADDR_P (OP)						\
	   && GET_CODE (OP) != LABEL_REF)))

#define GOT_ENTRY_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_GOT)

#define GOTPLT_ENTRY_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_GOTPLT)

#define UNSPEC_GOTOFF_P(OP) \
  (GET_CODE (OP) == UNSPEC && XINT ((OP), 1) == UNSPEC_GOTOFF)

#define GOTOFF_P(OP) \
  (GET_CODE (OP) == CONST \
   && (UNSPEC_GOTOFF_P (XEXP ((OP), 0)) \
       || (GET_CODE (XEXP ((OP), 0)) == PLUS \
	   && UNSPEC_GOTOFF_P (XEXP (XEXP ((OP), 0), 0)) \
	   && CONST_INT_P (XEXP (XEXP ((OP), 0), 1)))))

#define PIC_ADDR_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_PIC)

#define PCREL_SYMOFF_P(OP) \
  (GET_CODE (OP) == CONST \
   && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_PCREL_SYMOFF)

#define NON_PIC_REFERENCE_P(OP) \
  (GET_CODE (OP) == LABEL_REF || GET_CODE (OP) == SYMBOL_REF \
   || (GET_CODE (OP) == CONST \
       && (GET_CODE (XEXP ((OP), 0)) == LABEL_REF \
	   || GET_CODE (XEXP ((OP), 0)) == SYMBOL_REF)) \
   || (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == PLUS \
       && (GET_CODE (XEXP (XEXP ((OP), 0), 0)) == SYMBOL_REF \
	   || GET_CODE (XEXP (XEXP ((OP), 0), 0)) == LABEL_REF) \
       && CONST_INT_P (XEXP (XEXP ((OP), 0), 1))))

#define PIC_REFERENCE_P(OP) \
  (GOT_ENTRY_P (OP) || GOTPLT_ENTRY_P (OP) \
   || GOTOFF_P (OP) || PIC_ADDR_P (OP))

#define MAYBE_BASE_REGISTER_RTX_P(X, STRICT)			\
  ((REG_P (X) && REG_OK_FOR_BASE_P (X, STRICT))	\
   || (GET_CODE (X) == SUBREG					\
       && REG_P (SUBREG_REG (X))			\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X), STRICT)))

/* Since this must be r0, which is a single register class, we must check
   SUBREGs more carefully, to be sure that we don't accept one that extends
   outside the class.  */
#define MAYBE_INDEX_REGISTER_RTX_P(X, STRICT)				\
  ((REG_P (X) && REG_OK_FOR_INDEX_P (X, STRICT))	\
   || (GET_CODE (X) == SUBREG					\
       && REG_P (SUBREG_REG (X))		\
       && SUBREG_OK_FOR_INDEX_P (SUBREG_REG (X), SUBREG_BYTE (X), STRICT)))

#ifdef REG_OK_STRICT
#define BASE_REGISTER_RTX_P(X) MAYBE_BASE_REGISTER_RTX_P(X, true)
#define INDEX_REGISTER_RTX_P(X) MAYBE_INDEX_REGISTER_RTX_P(X, true)
#else
#define BASE_REGISTER_RTX_P(X) MAYBE_BASE_REGISTER_RTX_P(X, false)
#define INDEX_REGISTER_RTX_P(X) MAYBE_INDEX_REGISTER_RTX_P(X, false)
#endif


/* A C compound statement that attempts to replace X, which is an address
   that needs reloading, with a valid memory address for an operand of
   mode MODE.  WIN is a C statement label elsewhere in the code.  */
#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	\
  do {									\
    if (sh_legitimize_reload_address (&(X), (MODE), (OPNUM), (TYPE)))	\
      goto WIN;								\
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE ((! optimize || TARGET_BIGTABLE) ? SImode : HImode)

#define CASE_VECTOR_SHORTEN_MODE(MIN_OFFSET, MAX_OFFSET, BODY) \
((MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 127 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, QImode) \
 : (MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 255 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 1, QImode) \
 : (MIN_OFFSET) >= -32768 && (MAX_OFFSET) <= 32767 ? HImode \
 : SImode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

/* Define it here, so that it doesn't get bumped to 64-bits on SHmedia.  */
#define FLOAT_TYPE_SIZE 32

/* Since the SH2e has only `float' support, it is desirable to make all
   floating point types equivalent to `float'.  */
#define DOUBLE_TYPE_SIZE (TARGET_FPU_SINGLE_ONLY ? 32 : 64)

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  1

/* The type of size_t unsigned int.  */
#define SIZE_TYPE ("unsigned int")

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE ("int")

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

#define SH_ELF_WCHAR_TYPE "long int"

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX (4)

/* Maximum value possibly taken by MOVE_MAX.  Must be defined whenever
   MOVE_MAX is not a compile-time constant.  */
#define MAX_MOVE_MAX 8

/* Max number of bytes we want move_by_pieces to be able to copy
   efficiently.  */
#define MOVE_MAX_PIECES (TARGET_SH4 ? 8 : 4)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ((MODE) != SImode ? SIGN_EXTEND : UNKNOWN)

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* Nonzero if access to memory by bytes is no faster than for words.  */
#define SLOW_BYTE_ACCESS 1

/* Nonzero if the target supports dynamic shift instructions
   like shad and shld.  */
#define TARGET_DYNSHIFT (TARGET_SH3 || TARGET_SH2A)

/* The cost of using the dynamic shift insns (shad, shld) are the same
   if they are available.  If they are not available a library function will
   be emitted instead, which is more expensive.  */
#define SH_DYNAMIC_SHIFT_COST (TARGET_DYNSHIFT ? 1 : 20)

/* Defining SHIFT_COUNT_TRUNCATED tells the combine pass that code like
   (X << (Y % 32)) for register X, Y is equivalent to (X << Y).
   This is not generally true when hardware dynamic shifts (shad, shld) are
   used, because they check the sign bit _before_ the modulo op.  The sign
   bit determines whether it is a left shift or a right shift:
     if (Y < 0)
       return X << (Y & 31);
     else
       return X >> (-Y) & 31);
 
   The dynamic shift library routines in lib1funcs.S do not use the sign bit
   like the hardware dynamic shifts and truncate the shift count to 31.
   We define SHIFT_COUNT_TRUNCATED to 0 and express the implied shift count
   truncation in the library function call patterns, as this gives slightly
   more compact code.  */
#define SHIFT_COUNT_TRUNCATED (0)

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/*#define NO_FUNCTION_CSE 1*/

/* The machine modes of pointers and functions.  */
#define Pmode  (SImode)
#define FUNCTION_MODE  Pmode

/* The multiply insn on the SH1 and the divide insns on the SH1 and SH2
   are actually function calls with some special constraints on arguments
   and register usage.

   These macros tell reorg that the references to arguments and
   register clobbers for insns of type sfunc do not appear to happen
   until after the millicode call.  This allows reorg to put insns
   which set the argument registers into the delay slot of the millicode
   call -- thus they act more like traditional CALL_INSNs.

   get_attr_is_sfunc will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */

#define INSN_SETS_ARE_DELAYED(X) 		\
  ((NONJUMP_INSN_P (X)				\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))

#define INSN_REFERENCES_ARE_DELAYED(X) 		\
  ((NONJUMP_INSN_P (X)				\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))


/* Position Independent Code.  */

/* We can't directly access anything that contains a symbol,
   nor can we indirect via the constant pool.  */
#define LEGITIMATE_PIC_OPERAND_P(X)				\
	((! nonpic_symbol_mentioned_p (X)			\
	  && (GET_CODE (X) != SYMBOL_REF			\
	      || ! CONSTANT_POOL_ADDRESS_P (X)			\
	      || ! nonpic_symbol_mentioned_p (get_pool_constant (X)))))

#define SYMBOLIC_CONST_P(X)	\
((GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == LABEL_REF)	\
  && nonpic_symbol_mentioned_p (X))

/* Compute extra cost of moving data between one register class
   and another.  */

/* If SECONDARY*_RELOAD_CLASS says something about the src/dst pair, regclass
   uses this information.  Hence, the general register <-> floating point
   register information here is not used for SFmode.  */
#define REGCLASS_HAS_GENERAL_REG(CLASS) \
  ((CLASS) == GENERAL_REGS || (CLASS) == R0_REGS || (CLASS) == NON_SP_REGS \
    || ((CLASS) == SIBCALL_REGS))

#define REGCLASS_HAS_FP_REG(CLASS) \
  ((CLASS) == FP0_REGS || (CLASS) == FP_REGS \
   || (CLASS) == DF_REGS)

/* ??? Perhaps make MEMORY_MOVE_COST depend on compiler option?  This
   would be so that people with slow memory systems could generate
   different code that does fewer memory accesses.  */

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */
#define BRANCH_COST(speed_p, predictable_p) sh_branch_cost

/* Assembler output control.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START "!"

#define ASM_APP_ON  		""
#define ASM_APP_OFF  		""
#define FILE_ASM_OP 		"\t.file\n"
#define SET_ASM_OP		"\t.set\t"

/* How to change between sections.  */
#define TEXT_SECTION_ASM_OP	"\t.text"
#define DATA_SECTION_ASM_OP	"\t.data"

#if defined CRT_BEGIN || defined CRT_END
/* Arrange for TEXT_SECTION_ASM_OP to be a compile-time constant.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.text"
#endif

#ifndef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#endif

#ifndef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)
#endif

/* Define this so that jump tables go in same section as the current function,
   which could be text or it could be a user defined section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY			\
{						\
  typedef void (*pfunc) (void);			\
  extern pfunc __ctors[];			\
  extern pfunc __ctors_end[];			\
  pfunc *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  typedef void (*pfunc) (void);			\
  extern pfunc __dtors[];			\
  extern pfunc __dtors_end[];			\
  pfunc *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}

#define ASM_OUTPUT_REG_PUSH(file, v) \
{							\
  fprintf ((file), "\tmov.l\tr%d,@-r15\n", (v));	\
}

#define ASM_OUTPUT_REG_POP(file, v) \
{							\
  fprintf ((file), "\tmov.l\t@r15+,r%d\n", (v));	\
}

/* Debugger register number for a given compiler register number.  */
/* GDB has FPUL at 23 and FP0 at 25, so we must add one to all FP registers
   to match gdb.  */
/* expand_builtin_init_dwarf_reg_sizes uses this to test if a
   register exists, so we should return -1 for invalid register numbers.  */
#define DEBUGGER_REGNO(REGNO) SH_DEBUGGER_REGNO (REGNO)

#define SH_DEBUGGER_REGNO(REGNO) \
  (IN_RANGE ((REGNO), \
	     (unsigned HOST_WIDE_INT) FIRST_GENERAL_REG, \
	     FIRST_GENERAL_REG + 15U) \
   ? ((unsigned) (REGNO) - FIRST_GENERAL_REG) \
   : ((int) (REGNO) >= FIRST_FP_REG \
     && ((int) (REGNO) \
	 <= (FIRST_FP_REG + (TARGET_SH2E ? 15 : -1)))) \
   ? ((unsigned) (REGNO) - FIRST_FP_REG + 25) \
   : XD_REGISTER_P (REGNO) \
   ? ((unsigned) (REGNO) - FIRST_XD_REG + 87) \
   : (REGNO) == PR_REG \
   ? (17) \
   : (REGNO) == GBR_REG \
   ? (18) \
   : (REGNO) == MACH_REG \
   ? (20) \
   : (REGNO) == MACL_REG \
   ? (21) \
   : (REGNO) == T_REG \
   ? (22) \
   : (REGNO) == FPUL_REG \
   ? (23) \
   : (REGNO) == FPSCR_REG \
   ? (24) \
   : (unsigned) -1)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf ((FILE), "\t.align %d\n", (LOG))

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* #define ASM_OUTPUT_CASE_END(STREAM,NUM,TABLE)  */

/* Output a relative address table.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)			\
  switch (GET_MODE (BODY))						\
    {									\
    case E_SImode:							\
      asm_fprintf ((STREAM), "\t.long\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case E_HImode:							\
      asm_fprintf ((STREAM), "\t.word\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case E_QImode:							\
      asm_fprintf ((STREAM), "\t.byte\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    default:								\
      break;								\
    }

/* Output an absolute table element.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE) \
  do {									\
    if (! optimize || TARGET_BIGTABLE)					\
      asm_fprintf ((STREAM), "\t.long\t%LL%d\n", (VALUE)); 		\
    else								\
      asm_fprintf ((STREAM), "\t.word\t%LL%d\n", (VALUE));		\
  } while (0)

/* A C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.
   The contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler output
   by changing the contents of the vector.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  final_prescan_insn ((INSN), (OPVEC), (NOPERANDS))

/* Which processor to schedule for.  The elements of the enumeration must
   match exactly the cpu attribute in the sh.md file.  */
enum processor_type {
  PROCESSOR_SH1,
  PROCESSOR_SH2,
  PROCESSOR_SH2E,
  PROCESSOR_SH2A,
  PROCESSOR_SH3,
  PROCESSOR_SH3E,
  PROCESSOR_SH4,
  PROCESSOR_SH4A
};

#define sh_cpu_attr ((enum attr_cpu)sh_cpu)
extern enum processor_type sh_cpu;

enum mdep_reorg_phase_e
{
  SH_BEFORE_MDEP_REORG,
  SH_INSERT_USES_LABELS,
  SH_SHORTEN_BRANCHES0,
  SH_FIXUP_PCLOAD,
  SH_SHORTEN_BRANCHES1,
  SH_AFTER_MDEP_REORG
};

extern enum mdep_reorg_phase_e mdep_reorg_phase;

/* Handle Renesas compiler's pragmas.  */
#define REGISTER_TARGET_PRAGMAS() do {					\
  c_register_pragma (0, "interrupt", sh_pr_interrupt);			\
  c_register_pragma (0, "trapa", sh_pr_trapa);				\
  c_register_pragma (0, "nosave_low_regs", sh_pr_nosave_low_regs);	\
} while (0)

extern tree sh_deferred_function_attributes;
extern tree *sh_deferred_function_attributes_tail;



/* Instructions with unfilled delay slots take up an
   extra two bytes for the nop in the delay slot.
   sh-dsp parallel processing insns are four bytes long.  */
#define ADJUST_INSN_LENGTH(X, LENGTH)				\
  (LENGTH) += sh_insn_length_adjustment (X);

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Leaving the unsignedp unchanged gives better code than always setting it
   to 0.  This is despite the fact that we have only signed char and short
   load instructions.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  if (GET_MODE_CLASS (MODE) == MODE_INT			\
      && GET_MODE_SIZE (MODE) < 4/* ! UNITS_PER_WORD */)\
    (UNSIGNEDP) = ((MODE) == SImode ? 0 : (UNSIGNEDP)),	(MODE) = SImode;

#define MAX_FIXED_MODE_SIZE (64)

/* Better to allocate once the maximum space for outgoing args in the
   prologue rather than duplicate around each call.  */
#define ACCUMULATE_OUTGOING_ARGS TARGET_ACCUMULATE_OUTGOING_ARGS

#define NUM_MODES_FOR_MODE_SWITCHING { FP_MODE_NONE }

#define OPTIMIZE_MODE_SWITCHING(ENTITY) (TARGET_FPU_DOUBLE)

#define ACTUAL_NORMAL_MODE(ENTITY) \
  (TARGET_FPU_SINGLE ? FP_MODE_SINGLE : FP_MODE_DOUBLE)

#define NORMAL_MODE(ENTITY) \
  (sh_cfun_interrupt_handler_p () \
   ? (TARGET_FMOVD ? FP_MODE_DOUBLE : FP_MODE_NONE) \
   : ACTUAL_NORMAL_MODE (ENTITY))

#define EPILOGUE_USES(REGNO) (TARGET_FPU_ANY && REGNO == FPSCR_REG)

#define DWARF_FRAME_RETURN_COLUMN (DWARF_FRAME_REGNUM (PR_REG))

#define EH_RETURN_DATA_REGNO(N)	((N) < 4 ? (N) + 4U : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_REGNO STATIC_CHAIN_REGNUM
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, EH_RETURN_STACKADJ_REGNO)

/* We have to distinguish between code and data, so that we apply
   datalabel where and only where appropriate.  Use sdataN for data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  ((TARGET_FDPIC \
    ? ((GLOBAL) ? DW_EH_PE_indirect | DW_EH_PE_datarel : DW_EH_PE_pcrel) \
    : ((flag_pic && (GLOBAL) ? DW_EH_PE_indirect : 0) \
       | (flag_pic ? DW_EH_PE_pcrel : DW_EH_PE_absptr))) \
   | ((CODE) ? 0 : DW_EH_PE_sdata4))

/* Handle special EH pointer encodings.  Absolute, pc-relative, and
   indirect are handled automatically.  */
#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE) \
  do { \
    if (((ENCODING) & 0xf) != DW_EH_PE_sdata4 \
	&& ((ENCODING) & 0xf) != DW_EH_PE_sdata8) \
      { \
	gcc_assert (GET_CODE (ADDR) == SYMBOL_REF); \
	SYMBOL_REF_FLAGS (ADDR) |= SYMBOL_FLAG_FUNCTION; \
	if (0) goto DONE; \
      } \
    if (TARGET_FDPIC \
	&& ((ENCODING) & 0xf0) == (DW_EH_PE_indirect | DW_EH_PE_datarel)) \
      { \
	fputs ("\t.ualong ", FILE); \
	output_addr_const (FILE, ADDR); \
	if (GET_CODE (ADDR) == SYMBOL_REF && SYMBOL_REF_FUNCTION_P (ADDR)) \
	  fputs ("@GOTFUNCDESC", FILE); \
	else \
	  fputs ("@GOT", FILE); \
	goto DONE; \
      } \
  } while (0)

#if (defined CRT_BEGIN || defined CRT_END)
/* SH constant pool breaks the devices in crtstuff.c to control section
   in where code resides.  We have to write it as asm code.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
   asm (SECTION_OP "\n\
	mov.l	1f,r1\n\
	mova	2f,r0\n\
	braf	r1\n\
	lds	r0,pr\n\
0:	.p2align 2\n\
1:	.long	" USER_LABEL_PREFIX #FUNC " - 0b\n\
2:\n" TEXT_SECTION_ASM_OP);
#endif /* (defined CRT_BEGIN || defined CRT_END) */

#endif /* ! GCC_SH_H */
