/* Definitions of target machine for GNU compiler for IA-32.
   Copyright (C) 1988, 1992, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

/* The purpose of this file is to define the characteristics of the i386,
   independent of assembler syntax or operating system.

   Three other files build on this one to describe a specific assembler syntax:
   bsd386.h, att386.h, and sun386.h.

   The actual tm.h file for a particular system should include
   this file, and then the file for the appropriate assembler syntax.

   Many macros that specify assembler syntax are omitted entirely from
   this file because they really belong in the files for particular
   assemblers.  These include RP, IP, LPREFIX, PUT_OP_SIZE, USE_STAR,
   ADDR_BEG, ADDR_END, PRINT_IREG, PRINT_SCALE, PRINT_B_I_S, and many
   that start with ASM_ or end in ASM_OP.  */

/* Stubs for half-pic support if not OSF/1 reference platform.  */

#ifndef HALF_PIC_P
#define HALF_PIC_P() 0
#define HALF_PIC_NUMBER_PTRS 0
#define HALF_PIC_NUMBER_REFS 0
#define HALF_PIC_ENCODE(DECL)
#define HALF_PIC_DECLARE(NAME)
#define HALF_PIC_INIT()	error ("half-pic init called on systems that don't support it.")
#define HALF_PIC_ADDRESS_P(X) 0
#define HALF_PIC_PTR(X) X
#define HALF_PIC_FINISH(STREAM)
#endif

/* Define the specific costs for a given cpu */

struct processor_costs {
  int add;			/* cost of an add instruction */
  int lea;			/* cost of a lea instruction */
  int shift_var;		/* variable shift costs */
  int shift_const;		/* constant shift costs */
  int mult_init;		/* cost of starting a multiply */
  int mult_bit;			/* cost of multiply per each bit set */
  int divide;			/* cost of a divide/mod */
  int large_insn;		/* insns larger than this cost more */
  int move_ratio;		/* The threshold of number of scalar memory-to-memory
				   move insns.  */
  int movzbl_load;		/* cost of loading using movzbl */
  int int_load[3];		/* cost of loading integer registers
				   in QImode, HImode and SImode relative
				   to reg-reg move (2).  */
  int int_store[3];		/* cost of storing integer register
				   in QImode, HImode and SImode */
  int fp_move;			/* cost of reg,reg fld/fst */
  int fp_load[3];		/* cost of loading FP register
				   in SFmode, DFmode and XFmode */
  int fp_store[3];		/* cost of storing FP register
				   in SFmode, DFmode and XFmode */
};

extern struct processor_costs *ix86_cost;

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* configure can arrange to make this 2, to force a 486.  */

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

/* Masks for the -m switches */
#define MASK_80387		0x00000001	/* Hardware floating point */
#define MASK_RTD		0x00000002	/* Use ret that pops args */
#define MASK_ALIGN_DOUBLE	0x00000004	/* align doubles to 2 word boundary */
#define MASK_SVR3_SHLIB		0x00000008	/* Uninit locals into bss */
#define MASK_IEEE_FP		0x00000010	/* IEEE fp comparisons */
#define MASK_FLOAT_RETURNS	0x00000020	/* Return float in st(0) */
#define MASK_NO_FANCY_MATH_387	0x00000040	/* Disable sin, cos, sqrt */
#define MASK_OMIT_LEAF_FRAME_POINTER 0x080      /* omit leaf frame pointers */
#define MASK_STACK_PROBE	0x00000100	/* Enable stack probing */
#define MASK_NO_ALIGN_STROPS	0x00001000	/* Enable aligning of string ops. */
#define MASK_INLINE_ALL_STROPS	0x00002000	/* Inline stringops in all cases */
#define MASK_NO_PUSH_ARGS	0x00004000	/* Use push instructions */
#define MASK_ACCUMULATE_OUTGOING_ARGS 0x00008000/* Accumulate outgoing args */

/* Temporary codegen switches */
#define MASK_INTEL_SYNTAX	0x00000200
#define MASK_DEBUG_ARG		0x00000400	/* function_arg */   
#define MASK_DEBUG_ADDR		0x00000800	/* GO_IF_LEGITIMATE_ADDRESS */

/* Use the floating point instructions */
#define TARGET_80387 (target_flags & MASK_80387)

/* Compile using ret insn that pops args.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */  
#define TARGET_RTD (target_flags & MASK_RTD)

/* Align doubles to a two word boundary.  This breaks compatibility with
   the published ABI's for structures containing doubles, but produces
   faster code on the pentium.  */
#define TARGET_ALIGN_DOUBLE (target_flags & MASK_ALIGN_DOUBLE)

/* Use push instructions to save outgoing args.  */
#define TARGET_PUSH_ARGS (!(target_flags & MASK_NO_PUSH_ARGS))

/* Accumulate stack adjustments to prologue/epilogue.  */
#define TARGET_ACCUMULATE_OUTGOING_ARGS \
 (target_flags & MASK_ACCUMULATE_OUTGOING_ARGS)

/* Put uninitialized locals into bss, not data.
   Meaningful only on svr3.  */
#define TARGET_SVR3_SHLIB (target_flags & MASK_SVR3_SHLIB)

/* Use IEEE floating point comparisons.  These handle correctly the cases
   where the result of a comparison is unordered.  Normally SIGFPE is
   generated in such cases, in which case this isn't needed.  */
#define TARGET_IEEE_FP (target_flags & MASK_IEEE_FP)

/* Functions that return a floating point value may return that value
   in the 387 FPU or in 386 integer registers.  If set, this flag causes
   the 387 to be used, which is compatible with most calling conventions. */
#define TARGET_FLOAT_RETURNS_IN_80387 (target_flags & MASK_FLOAT_RETURNS)

/* Disable generation of FP sin, cos and sqrt operations for 387.
   This is because FreeBSD lacks these in the math-emulator-code */
#define TARGET_NO_FANCY_MATH_387 (target_flags & MASK_NO_FANCY_MATH_387)

/* Don't create frame pointers for leaf functions */
#define TARGET_OMIT_LEAF_FRAME_POINTER \
  (target_flags & MASK_OMIT_LEAF_FRAME_POINTER)

/* Debug GO_IF_LEGITIMATE_ADDRESS */
#define TARGET_DEBUG_ADDR (target_flags & MASK_DEBUG_ADDR)

/* Debug FUNCTION_ARG macros */
#define TARGET_DEBUG_ARG (target_flags & MASK_DEBUG_ARG)

#define TARGET_386 (ix86_cpu == PROCESSOR_I386)
#define TARGET_486 (ix86_cpu == PROCESSOR_I486)
#define TARGET_PENTIUM (ix86_cpu == PROCESSOR_PENTIUM)
#define TARGET_PENTIUMPRO (ix86_cpu == PROCESSOR_PENTIUMPRO)
#define TARGET_K6 (ix86_cpu == PROCESSOR_K6)
#define TARGET_ATHLON (ix86_cpu == PROCESSOR_ATHLON)

#define CPUMASK (1 << ix86_cpu)
extern const int x86_use_leave, x86_push_memory, x86_zero_extend_with_and;
extern const int x86_use_bit_test, x86_cmove, x86_deep_branch;
extern const int x86_unroll_strlen, x86_use_q_reg, x86_use_any_reg;
extern const int x86_double_with_add, x86_partial_reg_stall, x86_movx;
extern const int x86_use_loop, x86_use_fiop, x86_use_mov0;
extern const int x86_use_cltd, x86_read_modify_write;
extern const int x86_read_modify, x86_split_long_moves;
extern const int x86_promote_QImode, x86_single_stringop;
extern const int x86_himode_math, x86_qimode_math, x86_promote_qi_regs;
extern const int x86_promote_hi_regs;

#define TARGET_USE_LEAVE (x86_use_leave & CPUMASK)
#define TARGET_PUSH_MEMORY (x86_push_memory & CPUMASK)
#define TARGET_ZERO_EXTEND_WITH_AND (x86_zero_extend_with_and & CPUMASK)
#define TARGET_USE_BIT_TEST (x86_use_bit_test & CPUMASK)
#define TARGET_UNROLL_STRLEN (x86_unroll_strlen & CPUMASK)
#define TARGET_USE_Q_REG (x86_use_q_reg & CPUMASK)
#define TARGET_USE_ANY_REG (x86_use_any_reg & CPUMASK)
#define TARGET_CMOVE (x86_cmove & (1 << ix86_arch))
#define TARGET_DEEP_BRANCH_PREDICTION (x86_deep_branch & CPUMASK)
#define TARGET_DOUBLE_WITH_ADD (x86_double_with_add & CPUMASK)
#define TARGET_USE_SAHF (x86_use_sahf & CPUMASK)
#define TARGET_MOVX (x86_movx & CPUMASK)
#define TARGET_PARTIAL_REG_STALL (x86_partial_reg_stall & CPUMASK)
#define TARGET_USE_LOOP (x86_use_loop & CPUMASK)
#define TARGET_USE_FIOP (x86_use_fiop & CPUMASK)
#define TARGET_USE_MOV0 (x86_use_mov0 & CPUMASK)
#define TARGET_USE_CLTD (x86_use_cltd & CPUMASK)
#define TARGET_SPLIT_LONG_MOVES (x86_split_long_moves & CPUMASK)
#define TARGET_READ_MODIFY_WRITE (x86_read_modify_write & CPUMASK)
#define TARGET_READ_MODIFY (x86_read_modify & CPUMASK)
#define TARGET_PROMOTE_QImode (x86_promote_QImode & CPUMASK)
#define TARGET_SINGLE_STRINGOP (x86_single_stringop & CPUMASK)
#define TARGET_QIMODE_MATH (x86_qimode_math & CPUMASK)
#define TARGET_HIMODE_MATH (x86_himode_math & CPUMASK)
#define TARGET_PROMOTE_QI_REGS (x86_promote_qi_regs & CPUMASK)
#define TARGET_PROMOTE_HI_REGS (x86_promote_hi_regs & CPUMASK)

#define TARGET_STACK_PROBE (target_flags & MASK_STACK_PROBE)

#define TARGET_ALIGN_STRINGOPS (!(target_flags & MASK_NO_ALIGN_STROPS))
#define TARGET_INLINE_ALL_STRINGOPS (target_flags & MASK_INLINE_ALL_STROPS)

#define ASSEMBLER_DIALECT ((target_flags & MASK_INTEL_SYNTAX) != 0)

#define TARGET_SWITCHES							      \
{ { "80387",			 MASK_80387, "Use hardware fp" },	      \
  { "no-80387",			-MASK_80387, "Do not use hardware fp" },      \
  { "hard-float",		 MASK_80387, "Use hardware fp" },	      \
  { "soft-float",		-MASK_80387, "Do not use hardware fp" },      \
  { "no-soft-float",		 MASK_80387, "Use hardware fp" },	      \
  { "386",			 0, "Same as -mcpu=i386" },		      \
  { "486",			 0, "Same as -mcpu=i486" },		      \
  { "pentium",			 0, "Same as -mcpu=pentium" },		      \
  { "pentiumpro",		 0, "Same as -mcpu=pentiumpro" },	      \
  { "rtd",			 MASK_RTD, "Alternate calling convention" },  \
  { "no-rtd",			-MASK_RTD, "Use normal calling convention" }, \
  { "align-double",		 MASK_ALIGN_DOUBLE,			      \
    "Align some doubles on dword boundary" },				      \
  { "no-align-double",		-MASK_ALIGN_DOUBLE,			      \
    "Align doubles on word boundary" },					      \
  { "svr3-shlib",		 MASK_SVR3_SHLIB,			      \
    "Uninitialized locals in .bss"  },					      \
  { "no-svr3-shlib",		-MASK_SVR3_SHLIB,			      \
    "Uninitialized locals in .data" },					      \
  { "ieee-fp",			 MASK_IEEE_FP,				      \
    "Use IEEE math for fp comparisons" },				      \
  { "no-ieee-fp",		-MASK_IEEE_FP,				      \
    "Do not use IEEE math for fp comparisons" },			      \
  { "fp-ret-in-387",		 MASK_FLOAT_RETURNS,			      \
    "Return values of functions in FPU registers" },			      \
  { "no-fp-ret-in-387",		-MASK_FLOAT_RETURNS ,			      \
    "Do not return values of functions in FPU registers"},		      \
  { "no-fancy-math-387",	 MASK_NO_FANCY_MATH_387,		      \
    "Do not generate sin, cos, sqrt for FPU" },				      \
  { "fancy-math-387",		-MASK_NO_FANCY_MATH_387,		      \
     "Generate sin, cos, sqrt for FPU"},				      \
  { "omit-leaf-frame-pointer",	 MASK_OMIT_LEAF_FRAME_POINTER,		      \
    "Omit the frame pointer in leaf functions" },			      \
  { "no-omit-leaf-frame-pointer",-MASK_OMIT_LEAF_FRAME_POINTER, "" },	      \
  { "debug-addr",		 MASK_DEBUG_ADDR, 0 /* undocumented */ },     \
  { "no-debug-addr",		-MASK_DEBUG_ADDR, 0 /* undocumented */ },     \
  { "debug-arg",		 MASK_DEBUG_ARG, 0 /* undocumented */ },      \
  { "no-debug-arg",		-MASK_DEBUG_ARG, 0 /* undocumented */ },      \
  { "stack-arg-probe",		 MASK_STACK_PROBE, "Enable stack probing" },  \
  { "no-stack-arg-probe",	-MASK_STACK_PROBE, "" },		      \
  { "windows",			0, 0 /* undocumented */ },		      \
  { "dll",			0,  0 /* undocumented */ },		      \
  { "intel-syntax",		MASK_INTEL_SYNTAX,			      \
    "Emit Intel syntax assembler opcodes" },				      \
  { "no-intel-syntax",		-MASK_INTEL_SYNTAX, "" },		      \
  { "align-stringops",		-MASK_NO_ALIGN_STROPS,			      \
    "Align destination of the string operations" },			      \
  { "no-align-stringops",	 MASK_NO_ALIGN_STROPS,			      \
    "Do not align destination of the string operations" },		      \
  { "inline-all-stringops",	 MASK_INLINE_ALL_STROPS,		      \
    "Inline all known string operations" },				      \
  { "no-inline-all-stringops",	-MASK_INLINE_ALL_STROPS,		      \
    "Do not inline all known string operations" },			      \
  { "push-args",		-MASK_NO_PUSH_ARGS,			      \
    "Use push instructions to save outgoing arguments" },		      \
  { "no-push-args",		MASK_NO_PUSH_ARGS,			      \
    "Do not use push instructions to save outgoing arguments" },	      \
  { "accumulate-outgoing-args",	MASK_ACCUMULATE_OUTGOING_ARGS,		      \
    "Use push instructions to save outgoing arguments" },		      \
  { "no-accumulate-outgoing-args",-MASK_ACCUMULATE_OUTGOING_ARGS,	      \
    "Do not use push instructions to save outgoing arguments" },	      \
  SUBTARGET_SWITCHES							      \
  { "", TARGET_DEFAULT, 0 }}

/* Which processor to schedule for. The cpu attribute defines a list that
   mirrors this list, so changes to i386.md must be made at the same time.  */

enum processor_type
{
  PROCESSOR_I386,			/* 80386 */
  PROCESSOR_I486,			/* 80486DX, 80486SX, 80486DX[24] */
  PROCESSOR_PENTIUM,
  PROCESSOR_PENTIUMPRO,
  PROCESSOR_K6,
  PROCESSOR_ATHLON,
  PROCESSOR_max
};

extern enum processor_type ix86_cpu;

extern int ix86_arch;

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.  The
   variable, type `char *', is set to the variable part of the given
   option if the fixed part matches.  The actual option name is made
   by appending `-m' to the specified name.  */
#define TARGET_OPTIONS						\
{ { "cpu=",		&ix86_cpu_string,			\
    "Schedule code for given CPU"},				\
  { "arch=",		&ix86_arch_string,			\
    "Generate code for given CPU"},				\
  { "reg-alloc=",	&ix86_reg_alloc_order,			\
    "Control allocation order of integer registers" },		\
  { "regparm=",		&ix86_regparm_string,			\
    "Number of registers used to pass integer arguments" },	\
  { "align-loops=",	&ix86_align_loops_string,		\
    "Loop code aligned to this power of 2" },			\
  { "align-jumps=",	&ix86_align_jumps_string,		\
    "Jump targets are aligned to this power of 2" },		\
  { "align-functions=",	&ix86_align_funcs_string,		\
    "Function starts are aligned to this power of 2" },		\
  { "preferred-stack-boundary=",				\
    &ix86_preferred_stack_boundary_string,			\
    "Attempt to keep stack aligned to this power of 2" },	\
  { "branch-cost=",	&ix86_branch_cost_string,		\
    "Branches are this expensive (1-5, arbitrary units)" },	\
  SUBTARGET_OPTIONS						\
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS override_options ()

/* These are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES
#define SUBTARGET_OPTIONS

/* Define this to change the optimizations performed by default.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE) optimization_options(LEVEL,SIZE)

/* Specs for the compiler proper */

#ifndef CC1_CPU_SPEC
#define CC1_CPU_SPEC "\
%{!mcpu*: \
%{m386:-mcpu=i386} \
%{m486:-mcpu=i486} \
%{mpentium:-mcpu=pentium} \
%{mpentiumpro:-mcpu=pentiumpro}}"
#endif

#ifndef CPP_CPU_DEFAULT_SPEC
#if TARGET_CPU_DEFAULT == 1
#define CPP_CPU_DEFAULT_SPEC "-D__tune_i486__"
#endif
#if TARGET_CPU_DEFAULT == 2
#define CPP_CPU_DEFAULT_SPEC "-D__tune_pentium__"
#endif
#if TARGET_CPU_DEFAULT == 3
#define CPP_CPU_DEFAULT_SPEC "-D__tune_pentiumpro__"
#endif
#if TARGET_CPU_DEFAULT == 4
#define CPP_CPU_DEFAULT_SPEC "-D__tune_k6__"
#endif
#if TARGET_CPU_DEFAULT == 5
#define CPP_CPU_DEFAULT_SPEC "-D__tune_athlon__"
#endif
#ifndef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC "-D__tune_i386__"
#endif
#endif /* CPP_CPU_DEFAULT_SPEC */

#ifndef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
-Acpu(i386) -Amachine(i386) \
%{!ansi:-Di386} -D__i386 -D__i386__ \
%{march=i386:%{!mcpu*:-D__tune_i386__ }}\
%{march=i486:-D__i486 -D__i486__ %{!mcpu*:-D__tune_i486__ }}\
%{march=pentium|march=i586:-D__pentium -D__pentium__ \
  %{!mcpu*:-D__tune_pentium__ }}\
%{march=pentiumpro|march=i686:-D__pentiumpro -D__pentiumpro__ \
  %{!mcpu*:-D__tune_pentiumpro__ }}\
%{march=k6:-D__k6 -D__k6__ %{!mcpu*:-D__tune_k6__ }}\
%{march=athlon:-D__athlon -D__athlon__ %{!mcpu*:-D__tune_athlon__ }}\
%{m386|mcpu=i386:-D__tune_i386__ }\
%{m486|mcpu=i486:-D__tune_i486__ }\
%{mpentium|mcpu=pentium|mcpu=i586:-D__tune_pentium__ }\
%{mpentiumpro|mcpu=pentiumpro|mcpu=i686:-D__tune_pentiumpro__ }\
%{mcpu=k6:-D__tune_k6__ }\
%{mcpu=athlon:-D__tune_athlon__ }\
%{!march*:%{!mcpu*:%{!m386:%{!m486:%{!mpentium*:%(cpp_cpu_default)}}}}}"
#endif

#ifndef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) "
#endif

/* This macro defines names of additional specifications to put in the
   specs that can be used in various specifications like CC1_SPEC.  Its
   definition is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#define EXTRA_SPECS							\
  { "cpp_cpu_default",	CPP_CPU_DEFAULT_SPEC },				\
  { "cpp_cpu",	CPP_CPU_SPEC },						\
  { "cc1_cpu",  CC1_CPU_SPEC },						\
  SUBTARGET_EXTRA_SPECS

/* target machine storage layout */

/* Define for XFmode extended real floating point support.
   This will automatically cause REAL_ARITHMETIC to be defined.  */
#define LONG_DOUBLE_TYPE_SIZE 96

/* Define if you don't want extended real, but do want to use the
   software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/* #define REAL_ARITHMETIC */

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the 80386.  */

#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the 80386.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* Not true for 80386 */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 80386, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* Boundary (in *bits*) on which the stack pointer preferrs to be
   aligned; the compiler cannot rely on having this alignment.  */
#define PREFERRED_STACK_BOUNDARY ix86_preferred_stack_boundary

/* Allocation boundary for the code of a function. */
#define FUNCTION_BOUNDARY \
   (1 << ((ix86_align_funcs >= 0 ? ix86_align_funcs : -ix86_align_funcs) + 3))

/* Alignment of field after `int : 0' in a structure. */

#define EMPTY_FIELD_BOUNDARY 32

/* Minimum size in bits of the largest boundary to which any
   and all fundamental data types supported by the hardware
   might need to be aligned. No data type wants to be aligned
   rounder than this.
   
   Pentium+ preferrs DFmode values to be alignmed to 64 bit boundary
   and Pentium Pro XFmode values at 128 bit boundaries.  */

#define BIGGEST_ALIGNMENT 128

/* The published ABIs say that doubles should be aligned on word
   boundaries, so lower the aligment for structure fields unless
   -malign-double is set.  */
/* BIGGEST_FIELD_ALIGNMENT is also used in libobjc, where it must be
   constant.  Use the smaller value in that context.  */
#ifndef IN_TARGET_LIBS
#define BIGGEST_FIELD_ALIGNMENT (TARGET_ALIGN_DOUBLE ? 64 : 32)
#else
#define BIGGEST_FIELD_ALIGNMENT 32
#endif

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  (TREE_CODE (EXP) == REAL_CST						\
    ? ((TYPE_MODE (TREE_TYPE (EXP)) == DFmode && (ALIGN) < 64)		\
	? 64								\
   	: (TYPE_MODE (TREE_TYPE (EXP)) == XFmode && (ALIGN) < 128)	\
	? 128								\
	: (ALIGN))							\
    : TREE_CODE (EXP) == STRING_CST					\
    ? ((TREE_STRING_LENGTH (EXP) >= 31 && (ALIGN) < 256)		\
	? 256								\
	: (ALIGN))							\
    : (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((AGGREGATE_TYPE_P (TYPE)						\
    && TYPE_SIZE (TYPE)							\
    && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST			\
    && (TREE_INT_CST_LOW (TYPE_SIZE (TYPE)) >= 256			\
	|| TREE_INT_CST_HIGH (TYPE_SIZE (TYPE))) && (ALIGN) < 256)	\
    ? 256								\
    : TREE_CODE (TYPE) == ARRAY_TYPE					\
    ? ((TYPE_MODE (TREE_TYPE (TYPE)) == DFmode && (ALIGN) < 64)	\
	? 64								\
   	: (TYPE_MODE (TREE_TYPE (TYPE)) == XFmode && (ALIGN) < 128)	\
	? 128								\
	: (ALIGN))							\
    : TREE_CODE (TYPE) == COMPLEX_TYPE					\
    ? ((TYPE_MODE (TYPE) == DCmode && (ALIGN) < 64)			\
	? 64								\
   	: (TYPE_MODE (TYPE) == XCmode && (ALIGN) < 128)			\
	? 128								\
	: (ALIGN))							\
    : ((TREE_CODE (TYPE) == RECORD_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == QUAL_UNION_TYPE)				\
	&& TYPE_FIELDS (TYPE))						\
    ? ((DECL_MODE (TYPE_FIELDS (TYPE)) == DFmode && (ALIGN) < 64)	\
	? 64								\
	: (DECL_MODE (TYPE_FIELDS (TYPE)) == XFmode && (ALIGN) < 128)	\
	? 128								\
	: (ALIGN))							\
    : TREE_CODE (TYPE) == REAL_TYPE					\
    ? ((TYPE_MODE (TYPE) == DFmode && (ALIGN) < 64)			\
	? 64								\
   	: (TYPE_MODE (TYPE) == XFmode && (ALIGN) < 128)			\
	? 128								\
	: (ALIGN))							\
    : (ALIGN))

/* If defined, a C expression to compute the alignment for a local
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  */

#define LOCAL_ALIGNMENT(TYPE, ALIGN)					\
  (TREE_CODE (TYPE) == ARRAY_TYPE					\
    ? ((TYPE_MODE (TREE_TYPE (TYPE)) == DFmode && (ALIGN) < 64)		\
	? 64								\
   	: (TYPE_MODE (TREE_TYPE (TYPE)) == XFmode && (ALIGN) < 128)	\
	? 128								\
	: (ALIGN))							\
    : TREE_CODE (TYPE) == COMPLEX_TYPE					\
    ? ((TYPE_MODE (TYPE) == DCmode && (ALIGN) < 64)			\
	? 64								\
   	: (TYPE_MODE (TYPE) == XCmode && (ALIGN) < 128)			\
	? 128								\
	: (ALIGN))							\
    : ((TREE_CODE (TYPE) == RECORD_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == QUAL_UNION_TYPE)				\
	&& TYPE_FIELDS (TYPE))						\
    ? ((DECL_MODE (TYPE_FIELDS (TYPE)) == DFmode && (ALIGN) < 64)	\
	? 64								\
	: (DECL_MODE (TYPE_FIELDS (TYPE)) == XFmode && (ALIGN) < 128)	\
	? 128								\
	: (ALIGN))							\
    : TREE_CODE (TYPE) == REAL_TYPE					\
    ? ((TYPE_MODE (TYPE) == DFmode && (ALIGN) < 64)			\
	? 64								\
   	: (TYPE_MODE (TYPE) == XFmode && (ALIGN) < 128)			\
	? 128								\
	: (ALIGN))							\
    : (ALIGN))

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* If bit field type is int, don't let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have bitfield insns.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Align loop starts for optimal branching.  */
#define LOOP_ALIGN(LABEL) \
	(ix86_align_loops < 0 ? -ix86_align_loops : ix86_align_loops)
#define LOOP_ALIGN_MAX_SKIP \
	(ix86_align_loops < -3 ? (1<<(-ix86_align_loops-1))-1 : 0)

/* This is how to align an instruction for optimal branching.  */
#define LABEL_ALIGN_AFTER_BARRIER(LABEL) \
	(ix86_align_jumps < 0 ? -ix86_align_jumps : ix86_align_jumps)
#define LABEL_ALIGN_AFTER_BARRIER_MAX_SKIP \
	(ix86_align_jumps < -3 ? (1<<(-ix86_align_jumps-1))-1 : 0)

/* Standard register usage.  */

/* This processor has special stack-like registers.  See reg-stack.c
   for details. */

#define STACK_REGS
#define IS_STACK_MODE(mode) (mode==DFmode || mode==SFmode || mode==XFmode)

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   In the 80386 we give the 8 general purpose registers the numbers 0-7.
   We number the floating point registers 8-15.
   Note that registers 0-7 can be accessed as a  short or int,
   while only 0-3 may be used with byte `mov' instructions.

   Reg 16 does not correspond to any hardware register, but instead
   appears in the RTL as an argument pointer prior to reload, and is
   eliminated during reloading in favor of either the stack or frame
   pointer. */

#define FIRST_PSEUDO_REGISTER 21

/* Number of hardware registers that go into the DWARF-2 unwind info.
   If not defined, equals FIRST_PSEUDO_REGISTER.  */

#define DWARF_FRAME_REGISTERS 17

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the 80386, the stack pointer is such, as is the arg pointer. */
#define FIXED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg,flags,fpsr, dir*/ \
{  0, 0, 0, 0, 0, 0, 0, 1, 0,  0,  0,  0,  0,  0,  0,  0,  1,    0,   0,   0,  \
/*frame									    */ \
   1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg,flags,fpsr, dir*/ \
{  1, 1, 1, 0, 0, 0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1,    1,   1,   1,  \
/*frame									    */ \
   1}

/* Order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.  List frame pointer
   late and fixed registers last.  Note that, in general, we prefer
   registers listed in CALL_USED_REGISTERS, keeping the others
   available for storage of persistent values.

   Three different versions of REG_ALLOC_ORDER have been tried:

   If the order is edx, ecx, eax, ... it produces a slightly faster compiler,
   but slower code on simple functions returning values in eax.

   If the order is eax, ecx, edx, ... it causes reload to abort when compiling
   perl 4.036 due to not being able to create a DImode register (to hold a 2
   word union).

   If the order is eax, edx, ecx, ... it produces better code for simple
   functions, and a slightly slower compiler.  Users complained about the code
   generated by allocating edx first, so restore the 'natural' order of things. */

#define REG_ALLOC_ORDER \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg,cc,fpsr, dir*/ \
{  0, 1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16,17,  18,  19,  \
/*frame									 */ \
  20}

/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

/* Macro to conditionally modify fixed_regs/call_used_regs.  */
#define CONDITIONAL_REGISTER_USAGE			\
  {							\
    if (flag_pic)					\
      {							\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
      }							\
    if (! TARGET_80387 && ! TARGET_FLOAT_RETURNS_IN_80387) \
      { 						\
	int i; 						\
	HARD_REG_SET x;					\
        COPY_HARD_REG_SET (x, reg_class_contents[(int)FLOAT_REGS]); \
        for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ )	\
         if (TEST_HARD_REG_BIT (x, i)) 			\
	  fixed_regs[i] = call_used_regs[i] = 1; 	\
      }							\
  }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   Actually there are no two word move instructions for consecutive 
   registers.  And only registers 0-3 may have mov byte instructions
   applied to them.
   */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (FP_REGNO_P (REGNO) ? 1 \
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)				\
  /* Flags and only flags can only hold CCmode values.  */	\
  (CC_REGNO_P (REGNO)						\
   ? GET_MODE_CLASS (MODE) == MODE_CC				\
   : GET_MODE_CLASS (MODE) == MODE_CC ? 0			\
   /* FP regs can only hold floating point; make it clear they	\
      cannot hold TFmode floats.  */				\
   : FP_REGNO_P (REGNO)						\
   ? ((GET_MODE_CLASS (MODE) == MODE_FLOAT			\
       || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)		\
      && GET_MODE_UNIT_SIZE (MODE) <= (LONG_DOUBLE_TYPE_SIZE == 96 ? 12 : 8))\
   : (REGNO) < 4 ? 1						\
   /* Other regs cannot do byte accesses.  */			\
   : (MODE) != QImode ? 1					\
   : reload_in_progress || reload_completed			\
     || !TARGET_PARTIAL_REG_STALL)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2)				\
  ((MODE1) == (MODE2)						\
   || ((MODE1) == SImode && (MODE2) == HImode)			\
   || ((MODE1) == HImode && (MODE2) == SImode))

/* Specify the modes required to caller save a given hard regno.
   We do this on i386 to prevent flags from being saved at all.  */

#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS)		\
  (CC_REGNO_P (REGNO) ? VOIDmode				\
   : choose_hard_reg_mode ((REGNO), (NREGS)))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* on the 386 the pc register is %eip, and is not usable as a general
   register.  The ordinary mov instructions won't work */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 7

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 6

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 20

/* First floating point reg */
#define FIRST_FLOAT_REG 8

/* First & last stack-like regs */
#define FIRST_STACK_REG FIRST_FLOAT_REG
#define LAST_STACK_REG (FIRST_FLOAT_REG + 7)

#define FLAGS_REG 17
#define FPSR_REG 18
#define DIRFLAG_REG 19

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED (TARGET_OMIT_LEAF_FRAME_POINTER && !leaf_function_p ()) 	

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 16

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 2

/* Register to hold the addressing base for position independent
   code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM 3

/* Register in which address to store a structure value
   arrives in the function.  On the 386, the prologue
   copies this from the stack to register %eax.  */
#define STRUCT_VALUE_INCOMING 0

/* Place in which caller passes the structure value address.
   0 means push the value on the stack like an argument.  */
#define STRUCT_VALUE 0

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value
   says to return the function value in memory, just as large
   structures are always returned.  Here TYPE will be a C expression
   of type `tree', representing the data type of the value.

   Note that values of mode `BLKmode' must be explicitly handled by
   this macro.  Also, the option `-fpcc-struct-return' takes effect
   regardless of this macro.  On most systems, it is possible to
   leave the macro undefined; this causes a default definition to be
   used, whose value is the constant 1 for `BLKmode' values, and 0
   otherwise.

   Do not use this macro to indicate that structures and unions
   should always be returned in memory.  You should instead use
   `DEFAULT_PCC_STRUCT_RETURN' to indicate this.  */

#define RETURN_IN_MEMORY(TYPE) \
  ((TYPE_MODE (TYPE) == BLKmode) || int_size_in_bytes (TYPE) > 12)


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

   It might seem that class BREG is unnecessary, since no useful 386
   opcode needs reg %ebx.  But some systems pass args to the OS in ebx,
   and the "b" register constraint is useful in asms for syscalls.

   The flags and fpsr registers are in no class.  */

enum reg_class
{
  NO_REGS,
  AREG, DREG, CREG, BREG, SIREG, DIREG,
  AD_REGS,			/* %eax/%edx for DImode */
  Q_REGS,			/* %eax %ebx %ecx %edx */
  NON_Q_REGS,			/* %esi %edi %ebp %esp */
  INDEX_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp */
  GENERAL_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp %esp */
  FP_TOP_REG, FP_SECOND_REG,	/* %st(0) %st(1) */
  FLOAT_REGS,
  FLOAT_INT_REGS,		/* FLOAT_REGS and GENERAL_REGS.  */
  ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define FLOAT_CLASS_P(CLASS) (reg_class_subset_p (CLASS, FLOAT_REGS))

#define Q_CLASS_P(CLASS) (reg_class_subset_p (CLASS, Q_REGS))

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{  "NO_REGS",				\
   "AREG", "DREG", "CREG", "BREG",	\
   "SIREG", "DIREG",			\
   "AD_REGS",				\
   "Q_REGS", "NON_Q_REGS",		\
   "INDEX_REGS",			\
   "GENERAL_REGS",			\
   "FP_TOP_REG", "FP_SECOND_REG",	\
   "FLOAT_REGS",			\
   "FLOAT_INT_REGS",			\
   "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS					\
{      {0},							\
     {0x1}, {0x2}, {0x4}, {0x8},/* AREG, DREG, CREG, BREG */	\
    {0x10},   {0x20},		/* SIREG, DIREG */		\
     {0x3},			/* AD_REGS */			\
     {0xf},			/* Q_REGS */			\
{0x1100f0},			/* NON_Q_REGS */		\
    {0x7f},			/* INDEX_REGS */		\
{0x1100ff},			/* GENERAL_REGS */		\
  {0x0100}, {0x0200},		/* FP_TOP_REG, FP_SECOND_REG */	\
  {0xff00},			/* FLOAT_REGS */		\
{0x11ffff},			/* FLOAT_INT_REGS */		\
{0x17ffff}							\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) (regclass_map[REGNO])

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers. */

#define SMALL_REGISTER_CLASSES 1

#define QI_REG_P(X) \
  (REG_P (X) && REGNO (X) < 4)
#define NON_QI_REG_P(X) \
  (REG_P (X) && REGNO (X) >= 4 && REGNO (X) < FIRST_PSEUDO_REGISTER)

#define FP_REG_P(X) (REG_P (X) && FP_REGNO_P (REGNO (X)))
#define FP_REGNO_P(n) ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG)
  
#define STACK_REG_P(xop) (REG_P (xop) &&		       	\
			  REGNO (xop) >= FIRST_STACK_REG &&	\
			  REGNO (xop) <= LAST_STACK_REG)

#define NON_STACK_REG_P(xop) (REG_P (xop) && ! STACK_REG_P (xop))

#define STACK_TOP_P(xop) (REG_P (xop) && REGNO (xop) == FIRST_STACK_REG)

#define CC_REG_P(X) (REG_P (X) && CC_REGNO_P (REGNO (X)))
#define CC_REGNO_P(X) ((X) == FLAGS_REG || (X) == FPSR_REG)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS INDEX_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)	\
  ((C) == 'r' ? GENERAL_REGS :					\
   (C) == 'q' ? Q_REGS :					\
   (C) == 'f' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FLOAT_REGS					\
		 : NO_REGS) :					\
   (C) == 't' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FP_TOP_REG					\
		 : NO_REGS) :					\
   (C) == 'u' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FP_SECOND_REG				\
		 : NO_REGS) :					\
   (C) == 'a' ? AREG :						\
   (C) == 'b' ? BREG :						\
   (C) == 'c' ? CREG :						\
   (C) == 'd' ? DREG :						\
   (C) == 'A' ? AD_REGS :					\
   (C) == 'D' ? DIREG :						\
   (C) == 'S' ? SIREG : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   I is for non-DImode shifts.
   J is for DImode shifts.
   K is for signed imm8 operands.
   L is for andsi as zero-extending move.
   M is for shifts that can be executed by the "lea" opcode.
   */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (VALUE) >= 0 && (VALUE) <= 31			\
   : (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 63			\
   : (C) == 'K' ? (VALUE) >= -128 && (VALUE) <= 127		\
   : (C) == 'L' ? (VALUE) == 0xff || (VALUE) == 0xffff		\
   : (C) == 'M' ? (VALUE) >= 0 && (VALUE) <= 3			\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  We allow constants even if
   TARGET_387 isn't set, because the stack register converter may need to
   load 0.0 into the function value register.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? standard_80387_constant_p (VALUE) : 0)

/* Place additional restrictions on the register class to use when it
   is necessary to be able to hold a value of mode MODE in a reload
   register for which class CLASS would ordinarily be used. */

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  ((MODE) == QImode && ((CLASS) == ALL_REGS || (CLASS) == GENERAL_REGS) \
   ? Q_REGS : (CLASS))

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   On the 80386 series, we prevent floating constants from being
   reloaded into floating registers (since no move-insn can do that)
   and we ensure that QImodes aren't reloaded into the esi or edi reg.  */

/* Put float CONST_DOUBLE in the constant pool instead of fp regs.
   QImode must go into class Q_REGS.
   Narrow ALL_REGS to GENERAL_REGS.  This supports allowing movsf and
   movdf to do mem-to-mem moves through integer regs. */

#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
  (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != VOIDmode		\
   ? (standard_80387_constant_p (X)					\
      ? CLASS								\
      : (reg_class_subset_p (CLASS, FLOAT_REGS) 			\
	 ? NO_REGS							\
	 : reg_class_subset_p (CLASS, GENERAL_REGS) ? CLASS : GENERAL_REGS)) \
   : GET_MODE (X) == QImode && ! reg_class_subset_p (CLASS, Q_REGS) ? Q_REGS \
   : (CLASS))

/* If we are copying between general and FP registers, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE) \
  (FLOAT_CLASS_P (CLASS1) != FLOAT_CLASS_P (CLASS2))

/* QImode spills from non-QI registers need a scratch.  This does not
   happen often -- the only example so far requires an uninitialized 
   pseudo.  */

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,OUT) \
  ((CLASS) == GENERAL_REGS && (MODE) == QImode ? Q_REGS : NO_REGS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 80386, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 (FLOAT_CLASS_P (CLASS) ? 1 :		\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one
   register and zero otherwise.  On most machines, this default
   should be used.  Only define this macro to some other expression
   if pseudo allocated by `local-alloc.c' end up in memory because
   their hard registers were needed for spill registers.  If this
   macro returns nonzero for those classes, those pseudos will only
   be allocated by `global.c', which knows how to reallocate the
   pseudo to another register.  If there would not be another
   register available for reallocation, you should not change the
   definition of this macro since the only effect of such a
   definition would be to slow down register allocation.  */

#define CLASS_LIKELY_SPILLED_P(CLASS)					\
  (((CLASS) == AREG)							\
   || ((CLASS) == DREG)							\
   || ((CLASS) == CREG)							\
   || ((CLASS) == BREG)							\
   || ((CLASS) == AD_REGS)						\
   || ((CLASS) == SIREG)						\
   || ((CLASS) == DIREG))

/* A C statement that adds to CLOBBERS any hard regs the port wishes
   to automatically clobber for all asms. 

   We do this in the new i386 backend to maintain source compatibility
   with the old cc0-based compiler.  */

#define MD_ASM_CLOBBERS(CLOBBERS)					      \
  do {									      \
    (CLOBBERS) = tree_cons (NULL_TREE, build_string (5, "flags"), (CLOBBERS));\
    (CLOBBERS) = tree_cons (NULL_TREE, build_string (4, "fpsr"), (CLOBBERS)); \
    (CLOBBERS) = tree_cons (NULL_TREE, build_string (7, "dirflag"), (CLOBBERS)); \
  } while (0)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On 386 pushw decrements by exactly 2 no matter what the position was.
   On the 386 there is no pushb; we use pushw instead, and this
   has the effect of rounding up to 2.  */

#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & (-2))

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the stack
   frame size by this amount.  */

#define ACCUMULATE_OUTGOING_ARGS TARGET_ACCUMULATE_OUTGOING_ARGS

/* If defined, a C expression whose value is nonzero when we want to use PUSH
   instructions to pass outgoing arguments.  */

#define PUSH_ARGS (TARGET_PUSH_ARGS && !ACCUMULATE_OUTGOING_ARGS)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  (ix86_return_pops_args (FUNDECL, FUNTYPE, SIZE))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  \
   gen_rtx_REG (TYPE_MODE (VALTYPE), \
		VALUE_REGNO (TYPE_MODE (VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE, VALUE_REGNO (MODE))

/* Define the size of the result block used for communication between
   untyped_call and untyped_return.  The block contains a DImode value
   followed by the block used by fnsave and frstor.  */

#define APPLY_RESULT_SIZE (8+108)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) ((N) >= 0 && (N) < REGPARM_MAX)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */

typedef struct ix86_args {
  int words;			/* # words passed so far */
  int nregs;			/* # registers available for passing */
  int regno;			/* next available register number */
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  (init_cumulative_args (&CUM, FNTYPE, LIBNAME))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (function_arg_advance (&CUM, MODE, TYPE, NAMED))

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  (function_arg (&CUM, MODE, TYPE, NAMED))

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* If PIC, we cannot optimize sibling calls to global functions
   because the PLT requires %ebx live.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) \
  (DECL && (! flag_pic || ! TREE_PUBLIC (DECL)))

/* This macro is invoked just before the start of a function.
   It is used here to output code for -fpic that will load the
   return address into %ebx.  */

#undef ASM_OUTPUT_FUNCTION_PREFIX
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, FNNAME) \
  asm_output_function_prefix (FILE, FNNAME)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal\t%sP%d@GOTOFF(%%ebx),%%edx\n",		\
	       LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall\t*_mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmovl\t$%sP%d,%%edx\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall\t_mcount\n");				\
    }									\
}


/* There are three profiling modes for basic blocks available.
   The modes are selected at compile time by using the options
   -a or -ax of the gnu compiler.
   The variable `profile_block_flag' will be set according to the
   selected option.

   profile_block_flag == 0, no option used:

      No profiling done.

   profile_block_flag == 1, -a option used.

      Count frequency of execution of every basic block.

   profile_block_flag == 2, -ax option used.

      Generate code to allow several different profiling modes at run time. 
      Available modes are:
             Produce a trace of all basic blocks.
             Count frequency of jump instructions executed.
      In every mode it is possible to start profiling upon entering
      certain functions and to disable profiling of some other functions.

    The result of basic-block profiling will be written to a file `bb.out'.
    If the -ax option is used parameters for the profiling will be read
    from file `bb.in'.

*/

/* The following macro shall output assembler code to FILE
   to initialize basic-block profiling.  */

#undef	FUNCTION_BLOCK_PROFILER
#define FUNCTION_BLOCK_PROFILER(FILE, BLOCK_OR_LABEL) \
	ix86_output_function_block_profiler (FILE, BLOCK_OR_LABEL)

/* The following macro shall output assembler code to FILE
   to increment a counter associated with basic block number BLOCKNO.  */

#define BLOCK_PROFILER(FILE, BLOCKNO) \
	ix86_output_block_profiler (FILE, BLOCKNO)

/* The following macro shall output rtl for the epilogue
   to indicate a return from function during basic-block profiling.

   If profiling_block_flag == 2:

	Output assembler code to call function `__bb_trace_ret'.

	Note that function `__bb_trace_ret' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE
	and MACHINE_STATE_RESTORE. The last two macros will be
	used in the function `__bb_trace_ret', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE.

   else if profiling_block_flag != 0:

	The macro will not be used, so it need not distinguish
	these cases.
*/

#define FUNCTION_BLOCK_PROFILER_EXIT			\
emit_call_insn (gen_call (gen_rtx_MEM (Pmode,		\
  gen_rtx_SYMBOL_REF (VOIDmode, "__bb_trace_ret")),	\
  const0_rtx))

/* The function `__bb_trace_func' is called in every basic block
   and is not allowed to change the machine state. Saving (restoring)
   the state can either be done in the BLOCK_PROFILER macro,
   before calling function (rsp. after returning from function)
   `__bb_trace_func', or it can be done inside the function by
   defining the macros:

	MACHINE_STATE_SAVE(ID)
	MACHINE_STATE_RESTORE(ID)

   In the latter case care must be taken, that the prologue code
   of function `__bb_trace_func' does not already change the
   state prior to saving it with MACHINE_STATE_SAVE.

   The parameter `ID' is a string identifying a unique macro use.

   On the i386 the initialization code at the begin of
   function `__bb_trace_func' contains a `sub' instruction
   therefore we handle save and restore of the flag register 
   in the BLOCK_PROFILER macro.

   Note that ebx, esi, and edi are callee-save, so we don't have to
   preserve them explicitly.  */

#define MACHINE_STATE_SAVE(ID)					\
do {								\
  register int eax_ __asm__("eax");				\
  register int ecx_ __asm__("ecx");				\
  register int edx_ __asm__("edx");				\
  __asm__ __volatile__ ("\
push{l} %0\n\t\
push{l} %1\n\t\
push{l} %2"							\
	: : "r"(eax_), "r"(ecx_), "r"(edx_));			\
} while (0);

#define MACHINE_STATE_RESTORE(ID)				\
do {								\
  register int eax_ __asm__("eax");				\
  register int ecx_ __asm__("ecx");				\
  register int edx_ __asm__("edx");				\
  __asm__ __volatile__ ("\
pop{l} %2\n\t\
pop{l} %1\n\t\
pop{l} %0"							\
	: "=r"(eax_), "=r"(ecx_), "=r"(edx_));			\
} while (0);

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
/* Note on the 386 it might be more efficient not to define this since 
   we have to restore it ourselves from the frame pointer, in order to
   use pop */

#define EXIT_IGNORE_STACK 1

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the 386, the trampoline contains two instructions:
     mov #STATIC,ecx
     jmp FUNCTION
   The trampoline is generated entirely at runtime.  The operand of JMP
   is the address of FUNCTION relative to the instruction following the
   JMP (which is 5 bytes long).  */

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 10

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  /* Compute offset from the end of the jmp to the target function.  */	\
  rtx disp = expand_binop (SImode, sub_optab, FNADDR,			\
			   plus_constant (TRAMP, 10),			\
			   NULL_RTX, 1, OPTAB_DIRECT);			\
  emit_move_insn (gen_rtx_MEM (QImode, TRAMP), GEN_INT (0xb9));		\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 1)), CXT); \
  emit_move_insn (gen_rtx_MEM (QImode, plus_constant (TRAMP, 5)), GEN_INT (0xe9));\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 6)), disp); \
}

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have three registers that can be eliminated on the i386.  First, the
   hard frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument and frame pointer register can
   always be eliminated; They are replaced with either the stack or frame pointer. */

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}	\

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  Frame pointer elimination is automatically handled.

   All other eliminations are valid.  */

#define CAN_ELIMINATE(FROM, TO) \
  ((TO) == STACK_POINTER_REGNUM ? ! frame_pointer_needed : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  (OFFSET) = ix86_initial_elimination_offset (FROM, TO)

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) < STACK_POINTER_REGNUM \
   || (unsigned) reg_renumber[REGNO] < STACK_POINTER_REGNUM)

#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) <= STACK_POINTER_REGNUM \
   || (REGNO) == ARG_POINTER_REGNUM \
   || (REGNO) == FRAME_POINTER_REGNUM \
   || (unsigned) reg_renumber[REGNO] <= STACK_POINTER_REGNUM)

#define REGNO_OK_FOR_SIREG_P(REGNO) ((REGNO) == 4 || reg_renumber[REGNO] == 4)
#define REGNO_OK_FOR_DIREG_P(REGNO) ((REGNO) == 5 || reg_renumber[REGNO] == 5)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */


/* Non strict versions, pseudos are ok */
#define REG_OK_FOR_INDEX_NONSTRICT_P(X)					\
  (REGNO (X) < STACK_POINTER_REGNUM					\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_BASE_NONSTRICT_P(X)					\
  (REGNO (X) <= STACK_POINTER_REGNUM					\
   || REGNO (X) == ARG_POINTER_REGNUM					\
   || REGNO (X) == FRAME_POINTER_REGNUM \
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_STRREG_NONSTRICT_P(X)				\
  (REGNO (X) == 4 || REGNO (X) == 5 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Strict versions, hard registers only */
#define REG_OK_FOR_INDEX_STRICT_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_STRICT_P(X)  REGNO_OK_FOR_BASE_P (REGNO (X))
#define REG_OK_FOR_STRREG_STRICT_P(X)					\
  (REGNO_OK_FOR_DIREG_P (REGNO (X)) || REGNO_OK_FOR_SIREG_P (REGNO (X)))

#ifndef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_NONSTRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_NONSTRICT_P(X)
#define REG_OK_FOR_STRREG_P(X) REG_OK_FOR_STRREG_NONSTRICT_P(X)

#else
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_STRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_STRICT_P(X)
#define REG_OK_FOR_STRREG_P(X) REG_OK_FOR_STRREG_STRICT_P(X)
#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is usually machine-independent.

   See legitimize_pic_address in i386.c for details as to what
   constitutes a legitimate address when -fpic is used.  */

#define MAX_REGS_PER_ADDRESS 2

#define CONSTANT_ADDRESS_P(X)					\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (legitimate_address_p (MODE, X, 1))				\
    goto ADDR;								\
}

#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (legitimate_address_p (MODE, X, 0))				\
    goto ADDR;								\
}

#endif

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.c for details.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  (X) = legitimize_address (X, OLDX, MODE);				\
  if (memory_address_p (MODE, X))					\
    goto WIN;								\
}

#define REWRITE_ADDRESS(x) rewrite_address(x)

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and 
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_PIC_OPERAND_P(X)		\
  (! SYMBOLIC_CONST (X)				\
   || legitimate_pic_address_disp_p (X))

#define SYMBOLIC_CONST(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 80386, only postdecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == POST_DEC) goto LABEL

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On i386, if using PIC, mark a SYMBOL_REF for a non-global symbol
   so that we may access it directly in the GOT.  */

#define ENCODE_SECTION_INFO(DECL)				\
do								\
  {								\
    if (flag_pic)						\
      {								\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'	\
		   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));	\
								\
	if (GET_CODE (rtl) == MEM)				\
	  {							\
	    if (TARGET_DEBUG_ADDR				\
		&& TREE_CODE_CLASS (TREE_CODE (DECL)) == 'd')	\
	      {							\
		fprintf (stderr, "Encode %s, public = %d\n",	\
			 IDENTIFIER_POINTER (DECL_NAME (DECL)),	\
			 TREE_PUBLIC (DECL));			\
	      }							\
	    							\
	    SYMBOL_REF_FLAG (XEXP (rtl, 0))			\
	      = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'	\
		 || ! TREE_PUBLIC (DECL));			\
	  }							\
      }								\
  }								\
while (0)

/* The `FINALIZE_PIC' macro serves as a hook to emit these special
   codes once the function is being compiled into assembly code, but
   not before.  (It is not done before, because in the case of
   compiling an inline function, it would lead to multiple PIC
   prologues being included in functions which used inline functions
   and were compiled to assembly language.)  */

#define FINALIZE_PIC							\
do									\
  {									\
    current_function_uses_pic_offset_table |= profile_flag | profile_block_flag; \
  }									\
while (0)


/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, NAME, ARGS) \
  (ix86_valid_decl_attribute_p (DECL, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, NAME, ARGS) \
  (ix86_valid_type_attribute_p (TYPE, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is zero if the attributes on
   TYPE1 and TYPE2 are incompatible, one if they are compatible, and
   two if they are nearly compatible (which causes a warning to be
   generated).  */

#define COMP_TYPE_ATTRIBUTES(TYPE1, TYPE2) \
  (ix86_comp_type_attributes (TYPE1, TYPE2))

/* If defined, a C statement that assigns default attributes to newly
   defined TYPE.  */

/* #define SET_DEFAULT_TYPE_ATTRIBUTES (TYPE) */

/* Max number of args passed in registers.  If this is more than 3, we will
   have problems with ebx (register #4), since it is a caller save register and
   is also used as the pic register in ELF.  So for now, don't allow more than
   3 registers to be passed in registers.  */

#define REGPARM_MAX 3


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Specify the tree operation to be used to convert reals to integers.
   This should be changed to take advantage of fist --wfs ??
 */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a movstr or libcall instead.
   Increasing the value will always make code faster, but eventually
   incurs high cost in increased code size.

   If you don't define this, a reasonable default is used.  */

#define MOVE_RATIO (optimize_size ? 3 : ix86_cost->move_ratio)

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* On i386, shifts do truncate the count.  But bit opcodes don't. */

/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.
   (The 386 can't easily push less than an int.)  */

#define PROMOTE_PROTOTYPES 1

/* A macro to update M and UNSIGNEDP when an object whose type is
   TYPE and which has the specified mode and signedness is to be
   stored in a register.  This macro is only called when TYPE is a
   scalar type.

   On i386 it is sometimes usefull to promote HImode and QImode
   quantities to SImode.  The choice depends on target type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) 		\
  if (((MODE) == HImode && TARGET_PROMOTE_HI_REGS)	\
      || ((MODE) == QImode && TARGET_PROMOTE_QI_REGS))	\
    (MODE) = SImode;

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* A part of a C `switch' statement that describes the relative costs
   of constant RTL expressions.  It must contain `case' labels for
   expression codes `const_int', `const', `symbol_ref', `label_ref'
   and `const_double'.  Each case must ultimately reach a `return'
   statement to return the relative cost of the use of that kind of
   constant value in an expression.  The cost may depend on the
   precise value of the constant, which is available for examination
   in X, and the rtx code of the expression in which it is contained,
   found in OUTER_CODE.
  
   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    return (unsigned) INTVAL (RTX) < 256 ? 0 : 1;		\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return flag_pic && SYMBOLIC_CONST (RTX) ? 2 : 1;		\
								\
  case CONST_DOUBLE:						\
    {								\
      int code;							\
      if (GET_MODE (RTX) == VOIDmode)				\
	return 2;						\
								\
      code = standard_80387_constant_p (RTX);			\
      return code == 1 ? 0 :					\
	     code == 2 ? 1 :					\
			 2;					\
    }

/* Delete the definition here when TOPLEVEL_COSTS_N_INSNS gets added to cse.c */
#define TOPLEVEL_COSTS_N_INSNS(N) \
  do { total = COSTS_N_INSNS (N); goto egress_rtx_costs; } while (0)

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)					\
  case ASHIFT:								\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT				\
	&& GET_MODE (XEXP (X, 0)) == SImode)				\
      {									\
	HOST_WIDE_INT value = INTVAL (XEXP (X, 1));			\
	if (value == 1)							\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->add);			\
	if (value == 2 || value == 3)					\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->lea);			\
      }									\
    /* fall through */							\
		  							\
  case ROTATE:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
  case ROTATERT:							\
    if (GET_MODE (XEXP (X, 0)) == DImode)				\
      {									\
	if (GET_CODE (XEXP (X, 1)) == CONST_INT)			\
	  {								\
	    if (INTVAL (XEXP (X, 1)) > 32)				\
	      TOPLEVEL_COSTS_N_INSNS(ix86_cost->shift_const + 2);	\
	    else							\
	      TOPLEVEL_COSTS_N_INSNS(ix86_cost->shift_const * 2);	\
	  }								\
	else								\
	  {								\
	    if (GET_CODE (XEXP (X, 1)) == AND)				\
	      TOPLEVEL_COSTS_N_INSNS(ix86_cost->shift_var * 2);		\
	    else							\
	      TOPLEVEL_COSTS_N_INSNS(ix86_cost->shift_var * 6 + 2);	\
	  }								\
      }									\
    else								\
      {									\
	if (GET_CODE (XEXP (X, 1)) == CONST_INT)			\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->shift_const);		\
	else								\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->shift_var);		\
      }									\
    break;								\
									\
  case MULT:								\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT)				\
      {									\
	unsigned HOST_WIDE_INT value = INTVAL (XEXP (X, 1));		\
	int nbits = 0;							\
									\
	if (value == 2)							\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->add);			\
	if (value == 4 || value == 8)					\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->lea);			\
									\
	while (value != 0)						\
	  {								\
	    nbits++;							\
	    value >>= 1;						\
	  } 								\
									\
	if (nbits == 1)							\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->shift_const);		\
	else								\
	  TOPLEVEL_COSTS_N_INSNS (ix86_cost->mult_init			\
			          + nbits * ix86_cost->mult_bit);	\
      }									\
    else			/* This is arbitrary */			\
      TOPLEVEL_COSTS_N_INSNS (ix86_cost->mult_init			\
			      + 7 * ix86_cost->mult_bit);		\
									\
  case DIV:								\
  case UDIV:								\
  case MOD:								\
  case UMOD:								\
    TOPLEVEL_COSTS_N_INSNS (ix86_cost->divide);				\
									\
  case PLUS:								\
    if (GET_CODE (XEXP (X, 0)) == PLUS					\
	&& GET_CODE (XEXP (XEXP (X, 0), 0)) == MULT			\
	&& GET_CODE (XEXP (XEXP (XEXP (X, 0), 0), 1)) == CONST_INT	\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT)				\
      {									\
	HOST_WIDE_INT val = INTVAL (XEXP (XEXP (XEXP (X, 0), 0), 1));	\
	if (val == 2 || val == 4 || val == 8)				\
	  {								\
            return (COSTS_N_INSNS (ix86_cost->lea)			\
		    + rtx_cost (XEXP (XEXP (X, 0), 1), OUTER_CODE)	\
		    + rtx_cost (XEXP (XEXP (XEXP (X, 0), 0), 0), OUTER_CODE) \
		    + rtx_cost (XEXP (X, 1), OUTER_CODE));		\
	  }								\
      }									\
    else if (GET_CODE (XEXP (X, 0)) == MULT				\
	     && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT)		\
      {									\
	HOST_WIDE_INT val = INTVAL (XEXP (XEXP (X, 0), 1));		\
	if (val == 2 || val == 4 || val == 8)				\
	  {								\
	    return (COSTS_N_INSNS (ix86_cost->lea)			\
		    + rtx_cost (XEXP (XEXP (X, 0), 0), OUTER_CODE)	\
		    + rtx_cost (XEXP (X, 1), OUTER_CODE));		\
	  }								\
      }									\
    else if (GET_CODE (XEXP (X, 0)) == PLUS)				\
      {									\
	return (COSTS_N_INSNS (ix86_cost->lea)				\
		+ rtx_cost (XEXP (XEXP (X, 0), 0), OUTER_CODE)		\
		+ rtx_cost (XEXP (XEXP (X, 0), 1), OUTER_CODE)		\
		+ rtx_cost (XEXP (X, 1), OUTER_CODE));			\
      }									\
									\
    /* fall through */							\
  case AND:								\
  case IOR:								\
  case XOR:								\
  case MINUS:								\
    if (GET_MODE (X) == DImode)						\
      return (COSTS_N_INSNS (ix86_cost->add) * 2			\
	      + (rtx_cost (XEXP (X, 0), OUTER_CODE)			\
	         << (GET_MODE (XEXP (X, 0)) != DImode))			\
	      + (rtx_cost (XEXP (X, 1), OUTER_CODE)			\
 	         << (GET_MODE (XEXP (X, 1)) != DImode)));		\
									\
    /* fall through */							\
  case NEG:								\
  case NOT:								\
    if (GET_MODE (X) == DImode)						\
      TOPLEVEL_COSTS_N_INSNS (ix86_cost->add * 2);			\
    TOPLEVEL_COSTS_N_INSNS (ix86_cost->add);				\
									\
  egress_rtx_costs:							\
    break;


/* An expression giving the cost of an addressing mode that contains
   ADDRESS.  If not defined, the cost is computed from the ADDRESS
   expression and the `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation
   of the true cost of the addressing mode.  However, on RISC
   machines, all instructions normally have the same length and
   execution time.  Hence all addresses will have equal costs.

   In cases where more than one form of an address is known, the form
   with the lowest cost will be used.  If multiple forms have the
   same, lowest, cost, the one that is the most complex will be used.

   For example, suppose an address that is equal to the sum of a
   register and a constant is used twice in the same basic block.
   When this macro is not defined, the address will be computed in a
   register and memory references will be indirect through that
   register.  On machines where the cost of the addressing mode
   containing the sum is no higher than that of a simple indirect
   reference, this will produce an additional instruction and
   possibly require an additional register.  Proper specification of
   this macro eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the cost
   is not relevant and can be any value; invalid addresses need not be
   assigned a different cost.

   On machines where an address involving more than one register is as
   cheap as an address computation involving only one register,
   defining `ADDRESS_COST' to reflect this can cause two registers to
   be live over a region of code where only one would have been if
   `ADDRESS_COST' were not defined in that manner.  This effect should
   be considered in the definition of this macro.  Equivalent costs
   should probably only be given to addresses with different numbers
   of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as a
   constant.

   For i386, it is better to use a complex address than let gcc copy
   the address into a reg and make a new pseudo.  But not if the address
   requires to two regs - that would mean more pseudos with longer
   lifetimes.  */

#define ADDRESS_COST(RTX) \
  ((CONSTANT_P (RTX)						\
    || (GET_CODE (RTX) == PLUS && CONSTANT_P (XEXP (RTX, 1))	\
	&& REG_P (XEXP (RTX, 0)))) ? 0				\
   : REG_P (RTX) ? 1						\
   : 2)

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  The classes are expressed using the enumeration values
   such as `GENERAL_REGS'.  A value of 2 is the default; other values are
   interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   On the i386, copying between floating-point and fixed-point
   registers is done trough memory.  
 
   Integer -> fp moves are noticeably slower than the opposite direction
   because of the partial memory stall they cause.  Give it an
   arbitary high cost.
 */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)				\
  ((FLOAT_CLASS_P (CLASS1) && ! FLOAT_CLASS_P (CLASS2))			\
   ? (MEMORY_MOVE_COST (DFmode, CLASS1, 0)				\
     + MEMORY_MOVE_COST (DFmode, CLASS2, 1))				\
   : (! FLOAT_CLASS_P (CLASS1) && FLOAT_CLASS_P (CLASS2)) ? 10 : 2)

/* A C expression for the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.  
 
   Model also increased moving costs of QImode registers in non
   Q_REGS classes.
 */

#define MEMORY_MOVE_COST(MODE,CLASS,IN)					\
  (FLOAT_CLASS_P (CLASS)						\
   ? (GET_MODE_SIZE (MODE)==4						\
      ? (IN ? ix86_cost->fp_load[0] : ix86_cost->fp_store[0])		\
      : (GET_MODE_SIZE (MODE)==8					\
	 ? (IN ? ix86_cost->fp_load[1] : ix86_cost->fp_store[1])	\
	 : (IN ? ix86_cost->fp_load[2] : ix86_cost->fp_store[2])))	\
   : (GET_MODE_SIZE (MODE)==1						\
      ? (IN ? (Q_CLASS_P (CLASS) ? ix86_cost->int_load[0]		\
				 : ix86_cost->movzbl_load)		\
	    : (Q_CLASS_P (CLASS) ? ix86_cost->int_store[0]		\
				 : ix86_cost->int_store[0] + 4))	\
      : (GET_MODE_SIZE (MODE)==2					\
	 ? (IN ? ix86_cost->int_load[1] : ix86_cost->int_store[1])	\
	 : ((IN ? ix86_cost->int_load[2] : ix86_cost->int_store[2])	\
	    * GET_MODE_SIZE (MODE) / 4))))

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */

#define BRANCH_COST ix86_branch_cost

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory, i.e., if such access
   require more than one instruction or if there is no difference in
   cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by
   finding the smallest containing object; when it is defined, a
   fullword load will be used if alignment permits.  Unless bytes
   accesses are faster than word accesses, using word accesses is
   preferable since it may eliminate subsequent memory access if
   subsequent accesses occur to other fields in the same word of the
   structure, but to different bytes.  */

#define SLOW_BYTE_ACCESS 0

/* Nonzero if access to memory by shorts is slow and undesirable.  */
#define SLOW_SHORT_ACCESS 0

/* Define this macro if zero-extension (of a `char' or `short' to an
   `int') can be done faster if the destination is a register that is
   known to be zero.

   If you define this macro, you must have instruction patterns that
   recognize RTL structures like this:

          (set (strict_low_part (subreg:QI (reg:SI ...) 0)) ...)

   and likewise for `HImode'.  */

/* #define SLOW_ZERO_EXTEND */

/* Define this macro to be the value 1 if unaligned accesses have a
   cost many times greater than aligned accesses, for example if they
   are emulated in a trap handler.

   When this macro is non-zero, the compiler will act as if
   `STRICT_ALIGNMENT' were non-zero when generating code for block
   moves.  This can cause significantly more instructions to be
   produced.  Therefore, do not set this macro non-zero if unaligned
   accesses only add a cycle or two to the time for a memory access.

   If the value of this macro is always zero, it need not be defined.  */

/* #define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) 0 */

/* Define this macro to inhibit strength reduction of memory
   addresses.  (On some machines, such strength reduction seems to do
   harm rather than good.)  */

/* #define DONT_REDUCE_ADDR */

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.

   Desirable on the 386 because a CALL with a constant address is
   faster than one with a register address.  */

#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call
   itself with an explicit address than to call an address kept in a
   register.  */

#define NO_RECURSIVE_FUNCTION_CSE

/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.  */

#define ADJUST_COST(insn,link,dep_insn,cost) \
  (cost) = ix86_adjust_cost(insn, link, dep_insn, cost)

#define ISSUE_RATE \
  ix86_issue_rate ()

#define MD_SCHED_INIT(DUMP, SCHED_VERBOSE) \
  ix86_sched_init (DUMP, SCHED_VERBOSE)

#define MD_SCHED_REORDER(DUMP, SCHED_VERBOSE, READY, N_READY, CLOCK, CIM) \
  (CIM) = ix86_sched_reorder (DUMP, SCHED_VERBOSE, READY, N_READY, CLOCK)

#define MD_SCHED_VARIABLE_ISSUE(DUMP, SCHED_VERBOSE, INSN, CAN_ISSUE_MORE) \
  ((CAN_ISSUE_MORE) =							   \
   ix86_variable_issue (DUMP, SCHED_VERBOSE, INSN, CAN_ISSUE_MORE))

/* Add any extra modes needed to represent the condition code.

   For the i386, we need separate modes when floating-point
   equality comparisons are being done. 

   Add CCNO to indicate No Overflow, which is often also includes
   No Carry.  This is typically used on the output of logicals,
   and is only valid in comparisons against zero.

   Add CCZ to indicate that only the Zero flag is valid.  */

#define EXTRA_CC_MODES \
	CC(CCNOmode, "CCNO") \
	CC(CCZmode, "CCZ") \
	CC(CCFPmode, "CCFP") \
	CC(CCFPUmode, "CCFPU")

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.

   For floating-point equality comparisons, CCFPEQmode should be used.
   VOIDmode should be used in all other cases.

   For integer comparisons against zero, reduce to CCNOmode or CCZmode if
   possible, to allow for more combinations.  */

#define SELECT_CC_MODE(OP,X,Y)				\
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
   ? (OP) == EQ || (OP) == NE ? CCFPUmode : CCFPmode	\
   : (OP) == LE || (OP) == GT ? CCmode			\
   : (Y) != const0_rtx ? CCmode				\
   : (OP) == EQ || (OP) == NE ? CCZmode : CCNOmode)

/* Control the assembler format that we output, to the extent
   this does not vary between assemblers.  */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above). */

/* In order to refer to the first 8 regs as 32 bit regs prefix an "e"
   For non floating point regs, the following are the HImode names.

   For float regs, the stack top is sometimes referred to as "%st(0)"
   instead of just "%st".  PRINT_REG handles this with the "y" code.  */

#define HI_REGISTER_NAMES						\
{"ax","dx","cx","bx","si","di","bp","sp",				\
 "st","st(1)","st(2)","st(3)","st(4)","st(5)","st(6)","st(7)","",	\
 "flags","fpsr", "dirflag", "frame" }

#define REGISTER_NAMES HI_REGISTER_NAMES

/* Table of additional register names to use in user input.  */

#define ADDITIONAL_REGISTER_NAMES \
{ { "eax", 0 }, { "edx", 1 }, { "ecx", 2 }, { "ebx", 3 },	\
  { "esi", 4 }, { "edi", 5 }, { "ebp", 6 }, { "esp", 7 },	\
  { "al", 0 }, { "dl", 1 }, { "cl", 2 }, { "bl", 3 },		\
  { "ah", 0 }, { "dh", 1 }, { "ch", 2 }, { "bh", 3 } }

/* Note we are omitting these since currently I don't know how
to get gcc to use these, since they want the same but different
number as al, and ax.
*/

/* note the last four are not really qi_registers, but
   the md will have to never output movb into one of them
   only a movw .  There is no movb into the last four regs */

#define QI_REGISTER_NAMES \
{"al", "dl", "cl", "bl", "si", "di", "bp", "sp",}

/* These parallel the array above, and can be used to access bits 8:15
   of regs 0 through 3. */

#define QI_HIGH_REGISTER_NAMES \
{"ah", "dh", "ch", "bh", }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(n)  dbx_register_map[n]

extern int const dbx_register_map[FIRST_PSEUDO_REGISTER];
extern int const svr4_dbx_register_map[FIRST_PSEUDO_REGISTER];

/* Before the prologue, RA is at 0(%esp).  */
#define INCOMING_RETURN_ADDR_RTX \
  gen_rtx_MEM (VOIDmode, gen_rtx_REG (VOIDmode, STACK_POINTER_REGNUM))
 
/* After the prologue, RA is at -4(AP) in the current frame.  */
#define RETURN_ADDR_RTX(COUNT, FRAME)					\
  ((COUNT) == 0								\
   ? gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, -4))\
   : gen_rtx_MEM (Pmode, plus_constant (FRAME, 4)))

/* PC is dbx register 8; let's use that column for RA. */
#define DWARF_FRAME_RETURN_COLUMN 	8

/* Before the prologue, the top of the frame is at 4(%esp).  */
#define INCOMING_FRAME_SP_OFFSET 4

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  (assemble_name (FILE, NAME), fputs (":\n", FILE))

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
do { long l[2];								\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
     fprintf (FILE, "%s\t0x%lx,0x%lx\n", ASM_LONG, l[0], l[1]);		\
   } while (0)

/* This is how to output a `long double' extended real constant. */

#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  		\
do { long l[3];						\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);	\
     fprintf (FILE, "%s\t0x%lx,0x%lx,0x%lx\n", ASM_LONG, l[0], l[1], l[2]); \
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     fprintf ((FILE), "%s\t0x%lx\n", ASM_LONG, l);	\
   } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "%s\t", ASM_LONG),		\
  output_addr_const (FILE,(VALUE)),		\
  putc('\n',FILE))

/* Likewise for `char' and `short' constants.  */
/* is this supposed to do align too?? */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "%s\t", ASM_SHORT),		\
  output_addr_const (FILE,(VALUE)),		\
  putc('\n',FILE))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "%s\t", ASM_BYTE_OP),		\
  output_addr_const (FILE, (VALUE)),		\
  putc ('\n', FILE))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  asm_fprintf ((FILE), "%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  asm_fprintf (FILE, "\tpush{l}\t%%e%s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  asm_fprintf (FILE, "\tpop{l}\t%%e%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
     */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "%s %s%d\n", ASM_LONG, LPREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.
   We don't use these on the 386 yet, because the ATT assembler can't do
   forward reference the differences.  
 */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t%s\t%s%d-%s%d\n",ASM_LONG, LPREFIX, VALUE, LPREFIX, REL)

/* A C statement that outputs an address constant appropriate to 
   for DWARF debugging.  */

#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE,X) \
  i386_dwarf_output_addr_const((FILE),(X))

/* Either simplify a location expression, or return the original.  */

#define ASM_SIMPLIFY_DWARF_ADDR(X) \
  i386_simplify_dwarf_addr(X)

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN ""
#define ASM_CLOSE_PAREN ""

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   The CODE z takes the size of operand from the following digit, and
   outputs b,w,or l respectively.

   On the 80386, we use several such letters:
   f -- float insn (print a CONST_DOUBLE as a float rather than in hex).
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   P -- if PIC, print an @PLT suffix.
   X -- don't print any sort of PIC '@' suffix for a symbol.
   s -- ??? something to do with double shifts.  not actually used, afaik.
   C -- print a conditional move suffix corresponding to the op code.
   c -- likewise, but reverse the condition.
   F,f -- likewise, but for floating-point.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '*')

/* Print the name of a register based on its machine mode and number.
   If CODE is 'w', pretend the mode is HImode.
   If CODE is 'b', pretend the mode is QImode.
   If CODE is 'k', pretend the mode is SImode.
   If CODE is 'h', pretend the reg is the `high' byte register.
   If CODE is 'y', print "st(0)" instead of "st", if the reg is stack op. */

#define PRINT_REG(X, CODE, FILE)  \
  print_reg (X, CODE, FILE)

#define PRINT_OPERAND(FILE, X, CODE)  \
  print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
  print_operand_address (FILE, ADDR)

/* Print the name of a register for based on its machine mode and number.
   This macro is used to print debugging output.
   This macro is different from PRINT_REG in that it may be used in
   programs that are not linked with aux-output.o.  */

#define DEBUG_PRINT_REG(X, CODE, FILE)			\
  do { static const char * const hi_name[] = HI_REGISTER_NAMES;	\
       static const char * const qi_name[] = QI_REGISTER_NAMES;	\
       fprintf (FILE, "%d ", REGNO (X));		\
       if (REGNO (X) == FLAGS_REG)			\
	 { fputs ("flags", FILE); break; }		\
       if (REGNO (X) == DIRFLAG_REG)			\
	 { fputs ("dirflag", FILE); break; }		\
       if (REGNO (X) == FPSR_REG)			\
	 { fputs ("fpsr", FILE); break; }		\
       if (REGNO (X) == ARG_POINTER_REGNUM)		\
	 { fputs ("argp", FILE); break; }		\
       if (REGNO (X) == FRAME_POINTER_REGNUM)		\
	 { fputs ("frame", FILE); break; }		\
       if (STACK_TOP_P (X))				\
	 { fputs ("st(0)", FILE); break; }		\
       if (FP_REG_P (X))				\
	 { fputs (hi_name[REGNO(X)], FILE); break; }	\
       switch (GET_MODE_SIZE (GET_MODE (X)))		\
	 {						\
	 default:					\
	   fputs ("e", FILE);				\
	 case 2:					\
	   fputs (hi_name[REGNO (X)], FILE);		\
	   break;					\
	 case 1:					\
	   fputs (qi_name[REGNO (X)], FILE);		\
	   break;					\
	 }						\
     } while (0)

/* Routines in libgcc that return floats must return them in an fp reg,
   just as other functions do which return such values.
   These macros make that happen.  */

#define FLOAT_VALUE_TYPE float
#define INTIFY(FLOATVAL) FLOATVAL

/* a letter which is not needed by the normal asm syntax, which
   we can use for operand syntax in the extended asm */

#define ASM_OPERAND_LETTER '#'
#define RET return ""
#define AT_SP(mode) (gen_rtx_MEM ((mode), stack_pointer_rtx))

/* Define the codes that are matched by predicates in i386.c.  */

#define PREDICATE_CODES							\
  {"symbolic_operand", {SYMBOL_REF, LABEL_REF, CONST}},			\
  {"aligned_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,	\
		       LABEL_REF, SUBREG, REG, MEM}},			\
  {"pic_symbolic_operand", {CONST}},					\
  {"call_insn_operand", {MEM}},						\
  {"constant_call_address_operand", {MEM}},				\
  {"const0_operand", {CONST_INT, CONST_DOUBLE}},			\
  {"const1_operand", {CONST_INT}},					\
  {"const248_operand", {CONST_INT}},					\
  {"incdec_operand", {CONST_INT}},					\
  {"reg_no_sp_operand", {SUBREG, REG}},					\
  {"general_no_elim_operand", {CONST_INT, CONST_DOUBLE, CONST,		\
			SYMBOL_REF, LABEL_REF, SUBREG, REG, MEM}},	\
  {"nonmemory_no_elim_operand", {CONST_INT, REG, SUBREG}},		\
  {"q_regs_operand", {SUBREG, REG}},					\
  {"non_q_regs_operand", {SUBREG, REG}},				\
  {"no_comparison_operator", {EQ, NE, LT, GE, LTU, GTU, LEU, GEU}},	\
  {"fcmov_comparison_operator", {EQ, NE, LTU, GTU, LEU, GEU}},		\
  {"cmp_fp_expander_operand", {CONST_DOUBLE, SUBREG, REG, MEM}},	\
  {"ext_register_operand", {SUBREG, REG}},				\
  {"binary_fp_operator", {PLUS, MINUS, MULT, DIV}},			\
  {"mult_operator", {MULT}},						\
  {"div_operator", {DIV}},						\
  {"arith_or_logical_operator", {PLUS, MULT, AND, IOR, XOR, SMIN, SMAX, \
				 UMIN, UMAX, COMPARE, MINUS, DIV, MOD,	\
				 UDIV, UMOD, ASHIFT, ROTATE, ASHIFTRT,	\
				 LSHIFTRT, ROTATERT}},			\
  {"promotable_binary_operator", {PLUS, MULT, AND, IOR, XOR, ASHIFT}},	\
  {"memory_displacement_operand", {MEM}},				\
  {"cmpsi_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,	\
		     LABEL_REF, SUBREG, REG, MEM, AND}},		\
  {"long_memory_operand", {MEM}},

/* A list of predicates that do special things with modes, and so
   should not elicit warnings for VOIDmode match_operand.  */

#define SPECIAL_MODE_PREDICATES \
  "ext_register_operand",

/* Variables in i386.c */
extern const char *ix86_cpu_string;		/* for -mcpu=<xxx> */
extern const char *ix86_arch_string;		/* for -march=<xxx> */
extern const char *ix86_reg_alloc_order;	/* register allocation order */
extern const char *ix86_regparm_string;		/* # registers to use to pass args */
extern const char *ix86_align_loops_string;	/* power of two alignment for loops */
extern const char *ix86_align_jumps_string;	/* power of two alignment for non-loop jumps */
extern const char *ix86_align_funcs_string;	/* power of two alignment for functions */
extern const char *ix86_preferred_stack_boundary_string;/* power of two alignment for stack boundary */
extern const char *ix86_branch_cost_string;	/* values 1-5: see jump.c */
extern int ix86_regparm;			/* ix86_regparm_string as a number */
extern int ix86_align_loops;			/* power of two alignment for loops */
extern int ix86_align_jumps;			/* power of two alignment for non-loop jumps */
extern int ix86_align_funcs;			/* power of two alignment for functions */
extern int ix86_preferred_stack_boundary;	/* preferred stack boundary alignment in bits */
extern int ix86_branch_cost;			/* values 1-5: see jump.c */
extern const char * const hi_reg_name[];	/* names for 16 bit regs */
extern const char * const qi_reg_name[];	/* names for 8 bit regs (low) */
extern const char * const qi_high_reg_name[];	/* names for 8 bit regs (high) */
extern enum reg_class const regclass_map[];	/* smalled class containing REGNO */
extern struct rtx_def *ix86_compare_op0;	/* operand 0 for comparisons */
extern struct rtx_def *ix86_compare_op1;	/* operand 1 for comparisons */

/*
Local variables:
version-control: t
End:
*/
