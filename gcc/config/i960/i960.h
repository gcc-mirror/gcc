/* Definitions of target machine for GNU compiler, for Intel 80960
   Copyright (C) 1992, 1993, 1995, 1996, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

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
Boston, MA 02111-1307, USA.  */

/* Note that some other tm.h files may include this one and then override
   many of the definitions that relate to assembler syntax.  */

#define MULTILIB_DEFAULTS { "mnumerics" }

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES "-Di960 -Di80960 -DI960 -DI80960 -Acpu(i960) -Amachine(i960)"

/* Name to predefine in the preprocessor for processor variations.  */
#define	CPP_SPEC "%{mic*:-D__i960\
			%{mka:-D__i960KA}%{mkb:-D__i960KB}\
			%{mja:-D__i960JA}%{mjd:-D__i960JD}%{mjf:-D__i960JF}\
			%{mrp:-D__i960RP}\
			%{msa:-D__i960SA}%{msb:-D__i960SB}\
			%{mmc:-D__i960MC}\
			%{mca:-D__i960CA}%{mcc:-D__i960CC}\
			%{mcf:-D__i960CF}}\
	%{mka:-D__i960KA__ -D__i960_KA__}\
	%{mkb:-D__i960KB__ -D__i960_KB__}\
	%{msa:-D__i960SA__ -D__i960_SA__}\
	%{msb:-D__i960SB__ -D__i960_SB__}\
	%{mmc:-D__i960MC__ -D__i960_MC__}\
	%{mca:-D__i960CA__ -D__i960_CA__}\
	%{mcc:-D__i960CC__ -D__i960_CC__}\
	%{mcf:-D__i960CF__ -D__i960_CF__}\
	%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:\
		%{!mcc:%{!mcf:-D__i960_KB -D__i960KB__ %{mic*:-D__i960KB}}}}}}}}}\
	%{mlong-double-64:-D__LONG_DOUBLE_64__}"

/* -mic* options make characters signed by default.  */
/* Use #if rather than ?: because MIPS C compiler rejects ?: in
   initializers.  */
#if DEFAULT_SIGNED_CHAR
#define SIGNED_CHAR_SPEC "%{funsigned-char:-D__CHAR_UNSIGNED__}"
#else
#define SIGNED_CHAR_SPEC "%{!fsigned-char:%{!mic*:-D__CHAR_UNSIGNED__}}"
#endif

/* Specs for the compiler, to handle processor variations. 
   If the user gives an explicit -gstabs or -gcoff option, then do not
   try to add an implicit one, as this will fail.  */
#define CC1_SPEC \
	"%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:%{!mcc:%{!mcf:%{!mja:%{!mjd:%{!mjf:%{!mrp:-mka}}}}}}}}}}}}\
	 %{!gs*:%{!gc*:%{mbout:%{g*:-gstabs}}\
		       %{mcoff:%{g*:-gcoff}}\
		       %{!mbout:%{!mcoff:%{g*:-gstabs}}}}}"

/* Specs for the assembler, to handle processor variations.
   For compatibility with Intel's gnu960 tool chain, pass -A options to
   the assembler.  */
#define ASM_SPEC \
	"%{mka:-AKA}%{mkb:-AKB}%{msa:-ASA}%{msb:-ASB}\
	%{mmc:-AMC}%{mca:-ACA}%{mcc:-ACC}%{mcf:-ACF}\
        %{mja:-AJX}%{mjd:-AJX}%{mjf:-AJX}%{mrp:-AJX}\
	%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:%{!mcc:%{!mcf:%{!mja:%{!mjd:%{!mjf:%{!mrp:-AKB}}}}}}}}}}}}\
	%{mlink-relax:-linkrelax}"

/* Specs for the linker, to handle processor variations.
   For compatibility with Intel's gnu960 tool chain, pass -F and -A options
   to the linker.  */
#define LINK_SPEC \
	"%{mka:-AKA}%{mkb:-AKB}%{msa:-ASA}%{msb:-ASB}\
	%{mmc:-AMC}%{mca:-ACA}%{mcc:-ACC}%{mcf:-ACF}\
        %{mja:-AJX}%{mjd:-AJX}%{mjf:-AJX}%{mrp:-AJX}\
	%{mbout:-Fbout}%{mcoff:-Fcoff}\
	%{mlink-relax:-relax}"

/* Specs for the libraries to link with, to handle processor variations.
   Compatible with Intel's gnu960 tool chain.  */
#define LIB_SPEC "%{!nostdlib:-lcg %{p:-lprof}%{pg:-lgprof}\
	  %{mka:-lfpg}%{msa:-lfpg}%{mca:-lfpg}%{mcf:-lfpg} -lgnu}"

/* Defining the macro shows we can debug even without a frame pointer.
   Actually, we can debug without FP.  But defining the macro results in
   that -O means FP elimination.  Addressing through sp requires
   negative offset and more one word addressing in the most cases
   (offsets except for 0-4095 require one more word).  Therefore we've
   not defined the macro. */
/*#define CAN_DEBUG_WITHOUT_FP*/

/* Do leaf procedure and tail call optimizations for -O2 and higher.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)	\
{						\
  if ((LEVEL) >= 2)				\
    {						\
      target_flags |= TARGET_FLAG_LEAFPROC;	\
      target_flags |= TARGET_FLAG_TAILCALL;	\
    }						\
}

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr," (intel 80960)");

/* Generate DBX debugging information.  */
#define DBX_DEBUGGING_INFO

/* Generate SDB style debugging information.  */
#define SDB_DEBUGGING_INFO
#define EXTENDED_SDB_BASIC_TYPES

/* Generate DBX_DEBUGGING_INFO by default.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Redefine this to print in hex.  No value adjustment is necessary
   anymore.  */
#define PUT_SDB_TYPE(A) \
  fprintf (asm_out_file, "\t.type\t0x%x;", A)

/* Handle pragmas for compatibility with Intel's compilers.  */
#define HANDLE_PRAGMA(GET, UNGET, NAME) process_pragma (GET, UNGET, NAME)

/* Run-time compilation parameters selecting different hardware subsets.  */

/* 960 architecture with floating-point.  */
#define TARGET_FLAG_NUMERICS	0x01
#define TARGET_NUMERICS		(target_flags & TARGET_FLAG_NUMERICS)

/* 960 architecture with memory management.  */
/* ??? Not used currently.  */
#define	TARGET_FLAG_PROTECTED	0x02
#define	TARGET_PROTECTED	(target_flags & TARGET_FLAG_PROTECTED)

/* The following three are mainly used to provide a little sanity checking
   against the -mARCH flags given. The Jx series, for the purposes of
   gcc, is a Kx with a data cache. */

/* Nonzero if we should generate code for the KA and similar processors.
   No FPU, no microcode instructions.  */
#define TARGET_FLAG_K_SERIES	0x04
#define TARGET_K_SERIES		(target_flags & TARGET_FLAG_K_SERIES)

/* Nonzero if we should generate code for the MC processor.
   Not really different from KB for our purposes.  */
#define	TARGET_FLAG_MC		0x08
#define TARGET_MC 		(target_flags & TARGET_FLAG_MC)

/* Nonzero if we should generate code for the CA processor.
   Enables different optimization strategies.  */
#define	TARGET_FLAG_C_SERIES	0x10
#define	TARGET_C_SERIES 	(target_flags & TARGET_FLAG_C_SERIES)

/* Nonzero if we should generate leaf-procedures when we find them.
   You may not want to do this because leaf-proc entries are
   slower when not entered via BAL - this would be true when
   a linker not supporting the optimization is used.  */
#define	TARGET_FLAG_LEAFPROC	0x20
#define	TARGET_LEAFPROC		(target_flags & TARGET_FLAG_LEAFPROC)

/* Nonzero if we should perform tail-call optimizations when we find them.
   You may not want to do this because the detection of cases where
   this is not valid is not totally complete.  */
#define	TARGET_FLAG_TAILCALL	0x40
#define	TARGET_TAILCALL		(target_flags & TARGET_FLAG_TAILCALL)

/* Nonzero if use of a complex addressing mode is a win on this implementation.
   Complex addressing modes are probably not worthwhile on the K-series,
   but they definitely are on the C-series.  */
#define	TARGET_FLAG_COMPLEX_ADDR 0x80
#define	TARGET_COMPLEX_ADDR	(target_flags & TARGET_FLAG_COMPLEX_ADDR)

/* Align code to 8 byte boundaries for faster fetching.  */
#define	TARGET_FLAG_CODE_ALIGN	0x100
#define	TARGET_CODE_ALIGN	(target_flags  & TARGET_FLAG_CODE_ALIGN)

/* Append branch prediction suffixes to branch opcodes.  */
/* ??? Not used currently.  */
#define	TARGET_FLAG_BRANCH_PREDICT 0x200
#define	TARGET_BRANCH_PREDICT	(target_flags  & TARGET_FLAG_BRANCH_PREDICT)

/* Forces prototype and return promotions.  */
/* ??? This does not work.  */
#define	TARGET_FLAG_CLEAN_LINKAGE 0x400
#define	TARGET_CLEAN_LINKAGE	(target_flags & TARGET_FLAG_CLEAN_LINKAGE)

/* For compatibility with iC960 v3.0.  */
#define	TARGET_FLAG_IC_COMPAT3_0 0x800 
#define	TARGET_IC_COMPAT3_0	(target_flags & TARGET_FLAG_IC_COMPAT3_0)

/* For compatibility with iC960 v2.0.  */
#define	TARGET_FLAG_IC_COMPAT2_0 0x1000
#define	TARGET_IC_COMPAT2_0	(target_flags & TARGET_FLAG_IC_COMPAT2_0)

/* If no unaligned accesses are to be permitted.  */
#define	TARGET_FLAG_STRICT_ALIGN 0x2000
#define	TARGET_STRICT_ALIGN	(target_flags & TARGET_FLAG_STRICT_ALIGN)

/* For compatibility with iC960 assembler.  */
#define	TARGET_FLAG_ASM_COMPAT	0x4000
#define	TARGET_ASM_COMPAT	(target_flags & TARGET_FLAG_ASM_COMPAT)

/* For compatibility with the gcc960 v1.2 compiler.  Use the old structure
   alignment rules.  Also, turns on STRICT_ALIGNMENT.  */
#define TARGET_FLAG_OLD_ALIGN	0x8000
#define TARGET_OLD_ALIGN	(target_flags & TARGET_FLAG_OLD_ALIGN)

/* Nonzero if long doubles are to be 64 bits.  Useful for soft-float targets
   if 80 bit long double support is missing.  */
#define TARGET_FLAG_LONG_DOUBLE_64	0x10000
#define TARGET_LONG_DOUBLE_64	(target_flags & TARGET_FLAG_LONG_DOUBLE_64)

extern int target_flags;

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

/* ??? Not all ten of these architecture variations actually exist, but I
   am not sure which are real and which aren't.  */

#define TARGET_SWITCHES  \
  { {"sa", (TARGET_FLAG_K_SERIES|TARGET_FLAG_COMPLEX_ADDR),		\
       "Generate SA code"},						\
    {"sb", (TARGET_FLAG_NUMERICS|TARGET_FLAG_K_SERIES|			\
			TARGET_FLAG_COMPLEX_ADDR),			\
       "Generate SB code"},						\
/*  {"sc", (TARGET_FLAG_NUMERICS|TARGET_FLAG_PROTECTED|			\
			TARGET_FLAG_MC|TARGET_FLAG_COMPLEX_ADDR),	\
       "Generate SC code"}, */						\
    {"ka", (TARGET_FLAG_K_SERIES|TARGET_FLAG_COMPLEX_ADDR),		\
       "Generate KA code"},						\
    {"kb", (TARGET_FLAG_NUMERICS|TARGET_FLAG_K_SERIES|			\
			TARGET_FLAG_COMPLEX_ADDR),			\
       "Generate KB code"},						\
/*  {"kc", (TARGET_FLAG_NUMERICS|TARGET_FLAG_PROTECTED|			\
			TARGET_FLAG_MC|TARGET_FLAG_COMPLEX_ADDR),	\
       "Generate KC code"}, */						\
    {"ja", (TARGET_FLAG_K_SERIES|TARGET_FLAG_COMPLEX_ADDR),		\
       "Generate JA code"},						\
    {"jd", (TARGET_FLAG_K_SERIES|TARGET_FLAG_COMPLEX_ADDR),		\
       "Generate JD code"},						\
    {"jf", (TARGET_FLAG_NUMERICS|TARGET_FLAG_K_SERIES|			\
			TARGET_FLAG_COMPLEX_ADDR),			\
       "Generate JF code"},						\
    {"rp", (TARGET_FLAG_K_SERIES|TARGET_FLAG_COMPLEX_ADDR),		\
       "generate RP code"},						\
    {"mc", (TARGET_FLAG_NUMERICS|TARGET_FLAG_PROTECTED|			\
			TARGET_FLAG_MC|TARGET_FLAG_COMPLEX_ADDR),	\
       "Generate MC code"},						\
    {"ca", (TARGET_FLAG_C_SERIES|TARGET_FLAG_BRANCH_PREDICT|		\
			TARGET_FLAG_CODE_ALIGN|TARGET_FLAG_COMPLEX_ADDR),\
       "Generate CA code"},						\
/*  {"cb", (TARGET_FLAG_NUMERICS|TARGET_FLAG_C_SERIES|			\
			TARGET_FLAG_BRANCH_PREDICT|TARGET_FLAG_CODE_ALIGN),\
       "Generate CB code"},						\
    {"cc", (TARGET_FLAG_NUMERICS|TARGET_FLAG_PROTECTED|			\
			TARGET_FLAG_C_SERIES|TARGET_FLAG_BRANCH_PREDICT|\
			TARGET_FLAG_CODE_ALIGN),			\
       "Generate CC code"}, */						\
    {"cf", (TARGET_FLAG_C_SERIES|TARGET_FLAG_BRANCH_PREDICT|		\
			TARGET_FLAG_CODE_ALIGN|TARGET_FLAG_COMPLEX_ADDR),\
       "Generate CF code"},						\
    {"numerics", (TARGET_FLAG_NUMERICS),				\
       "Use hardware floating point instructions"},			\
    {"soft-float", -(TARGET_FLAG_NUMERICS),				\
       "Use software floating point"},					\
    {"leaf-procedures", TARGET_FLAG_LEAFPROC,				\
       "Use alternate leaf function entries"},				\
    {"no-leaf-procedures", -(TARGET_FLAG_LEAFPROC),			\
       "Do not use alternate leaf function entries"},			\
    {"tail-call", TARGET_FLAG_TAILCALL,					\
       "Perform tail call optimization"},				\
    {"no-tail-call", -(TARGET_FLAG_TAILCALL),				\
       "Do not perform tail call optimization"},			\
    {"complex-addr", TARGET_FLAG_COMPLEX_ADDR, 				\
       "Use complex addressing modes"},					\
    {"no-complex-addr", -(TARGET_FLAG_COMPLEX_ADDR),			\
       "Do not use complex addressing modes"},				\
    {"code-align", TARGET_FLAG_CODE_ALIGN,				\
       "Align code to 8 byte boundary"},				\
    {"no-code-align", -(TARGET_FLAG_CODE_ALIGN),			\
       "Do not align code to 8 byte boundary"},				\
/*  {"clean-linkage", (TARGET_FLAG_CLEAN_LINKAGE),			\
       "Force use of prototypes"},					\
    {"no-clean-linkage", -(TARGET_FLAG_CLEAN_LINKAGE),			\
       "Do not force use of prototypes"}, */				\
    {"ic-compat", TARGET_FLAG_IC_COMPAT2_0,				\
       "Enable compatibility with iC960 v2.0"},				\
    {"ic2.0-compat", TARGET_FLAG_IC_COMPAT2_0,				\
       "Enable compatibility with iC960 v2.0"},				\
    {"ic3.0-compat", TARGET_FLAG_IC_COMPAT3_0,				\
       "Enable compatibility with iC960 v3.0"},				\
    {"asm-compat", TARGET_FLAG_ASM_COMPAT,				\
       "Enable compatibility with ic960 assembler"},			\
    {"intel-asm", TARGET_FLAG_ASM_COMPAT,				\
       "Enable compatibility with ic960 assembler"},			\
    {"strict-align", TARGET_FLAG_STRICT_ALIGN,				\
       "Do not permit unaligned accesses"},				\
    {"no-strict-align", -(TARGET_FLAG_STRICT_ALIGN),			\
       "Permit unaligned accesses"},					\
    {"old-align", (TARGET_FLAG_OLD_ALIGN|TARGET_FLAG_STRICT_ALIGN),	\
       "Layout types like Intel's v1.3 gcc"},				\
    {"no-old-align", -(TARGET_FLAG_OLD_ALIGN|TARGET_FLAG_STRICT_ALIGN),	\
       "Do not layout types like Intel's v1.3 gcc"},			\
    {"long-double-64", TARGET_FLAG_LONG_DOUBLE_64,			\
       "Use 64 bit long doubles"},					\
    {"link-relax", 0,							\
       "Enable linker relaxation"},					\
    {"no-link-relax", 0,						\
       "Do not enable linker relaxation"},				\
    SUBTARGET_SWITCHES                                                  \
    { "", TARGET_DEFAULT,						\
	NULL}}

/* This are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES

/* Override conflicting target switch options.
   Doesn't actually detect if more than one -mARCH option is given, but
   does handle the case of two blatantly conflicting -mARCH options.  */
#define OVERRIDE_OPTIONS					\
{								\
  if (TARGET_K_SERIES && TARGET_C_SERIES)			\
    {								\
      warning ("conflicting architectures defined - using C series"); \
      target_flags &= ~TARGET_FLAG_K_SERIES;			\
    }								\
  if (TARGET_K_SERIES && TARGET_MC)				\
    {								\
      warning ("conflicting architectures defined - using K series"); \
      target_flags &= ~TARGET_FLAG_MC;				\
    }								\
  if (TARGET_C_SERIES && TARGET_MC)				\
    {								\
      warning ("conflicting architectures defined - using C series");\
      target_flags &= ~TARGET_FLAG_MC;				\
    }								\
  if (TARGET_IC_COMPAT3_0)					\
    {								\
      flag_short_enums = 1;					\
      flag_signed_char = 1;					\
      target_flags |= TARGET_FLAG_CLEAN_LINKAGE;		\
      if (TARGET_IC_COMPAT2_0)					\
	{							\
	  warning ("iC2.0 and iC3.0 are incompatible - using iC3.0"); \
	  target_flags &= ~TARGET_FLAG_IC_COMPAT2_0;		\
	}							\
    }								\
  if (TARGET_IC_COMPAT2_0)					\
    {								\
      flag_signed_char = 1;					\
      target_flags |= TARGET_FLAG_CLEAN_LINKAGE;		\
    }								\
  /* ??? See the LONG_DOUBLE_TYPE_SIZE definition below.  */	\
  if (TARGET_LONG_DOUBLE_64)					\
    warning ("The -mlong-double-64 option does not work yet.");\
  i960_initialize ();						\
}

/* Don't enable anything by default.  The user is expected to supply a -mARCH
   option.  If none is given, then -mka is added by CC1_SPEC.  */
#define TARGET_DEFAULT 0

/* Target machine storage layout.  */

/* Define for cross-compilation from a host with a different float format
   or endianness, as well as to support 80 bit long doubles on the i960.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.
   The i960 case be either big endian or little endian.  We only support
   little endian, which is the most common.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT 8

/* Bitfields cannot cross word boundaries.  */
#define BITFIELD_NBYTES_LIMITED 1

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.  See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Width in bits of a long double.  Define to 96, and let
   ROUND_TYPE_ALIGN adjust the alignment for speed. */
#define	LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_64 ? 64 : 96)

/* ??? This must be a constant, because real.c and real.h test it with #if.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 96

/* Define this to set long double type size to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined(__LONG_DOUBLE_64__)
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 96
#endif

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 128

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* This makes zero-length anonymous fields lay the next field
   at a word boundary.  It also makes the whole struct have
   at least word alignment if there are any bitfields at all.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* No data type wants to be aligned rounder than this.
   Extended precision floats gets 4-word alignment.  */
#define BIGGEST_ALIGNMENT 128

/* Define this if move instructions will actually fail to work
   when given unaligned data.
   80960 will work even with unaligned data, but it is slow.  */
#define STRICT_ALIGNMENT TARGET_STRICT_ALIGN

/* Specify alignment for string literals (which might be higher than the
   base type's minimal alignment requirement.  This allows strings to be
   aligned on word boundaries, and optimizes calls to the str* and mem*
   library functions.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN) \
  (TREE_CODE (EXP) == STRING_CST	\
   && i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP))) > (ALIGN) \
   ? i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP)))	    \
   : (ALIGN))

/* Make XFmode floating point quantities be 128 bit aligned.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  (TREE_CODE (TYPE) == ARRAY_TYPE					\
   && TYPE_MODE (TREE_TYPE (TYPE)) == XFmode				\
   && (ALIGN) < 128 ? 128 : (ALIGN))

/* Macros to determine size of aggregates (structures and unions
   in C).  Normally, these may be defined to simply return the maximum
   alignment and simple rounded-up size, but on some machines (like
   the i960), the total size of a structure is based on a non-trivial
   rounding method.  */

#define ROUND_TYPE_ALIGN(TYPE, COMPUTED, SPECIFIED)		\
  ((TREE_CODE (TYPE) == REAL_TYPE && TYPE_MODE (TYPE) == XFmode)	   \
   ? 128  /* Put 80 bit floating point elements on 128 bit boundaries.  */ \
   : ((!TARGET_OLD_ALIGN && !TYPE_PACKED (TYPE)				   \
       && TREE_CODE (TYPE) == RECORD_TYPE)				   \
      ? i960_round_align (MAX ((COMPUTED), (SPECIFIED)), TYPE_SIZE (TYPE)) \
      : MAX ((COMPUTED), (SPECIFIED))))

#define ROUND_TYPE_SIZE(TYPE, COMPUTED, SPECIFIED)		\
  ((TREE_CODE (TYPE) == REAL_TYPE && TYPE_MODE (TYPE) == XFmode)	\
   ? bitsize_int (128) : round_up (COMPUTED, SPECIFIED))
#define ROUND_TYPE_SIZE_UNIT(TYPE, COMPUTED, SPECIFIED)		\
  ((TREE_CODE (TYPE) == REAL_TYPE && TYPE_MODE (TYPE) == XFmode)	\
   ? bitsize_int (16) : round_up (COMPUTED, SPECIFIED))


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Registers 0-15 are the global registers (g0-g15).
   Registers 16-31 are the local registers (r0-r15).
   Register 32-35 are the fp registers (fp0-fp3).
   Register 36 is the condition code register.
   Register 37 is unused.  */

#define FIRST_PSEUDO_REGISTER 38

/* 1 for registers that have pervasive standard uses and are not available
   for the register allocator.  On 80960, this includes the frame pointer
   (g15), the previous FP (r0), the stack pointer (r1), the return
   instruction pointer (r2), and the argument pointer (g14).  */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* On the 80960, note that:
	g0..g3 are used for return values,
	g0..g7 may always be used for parameters,
	g8..g11 may be used for parameters, but are preserved if they aren't,
	g12 is the static chain if needed, otherwise is preserved
	g13 is the struct return ptr if used, or temp, but may be trashed,
	g14 is the leaf return ptr or the arg block ptr otherwise zero,
		must be reset to zero before returning if it was used,
	g15 is the frame pointer,
	r0 is the previous FP,
	r1 is the stack pointer,
	r2 is the return instruction pointer,
	r3-r15 are always available,
	r3 is clobbered by calls in functions that use the arg pointer
	r4-r11 may be clobbered by the mcount call when profiling
	r4-r15 if otherwise unused may be used for preserving global registers
	fp0..fp3 are never available.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 1, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1}

/* If no fp unit, make all of the fp registers fixed so that they can't
   be used.  */
#define	CONDITIONAL_REGISTER_USAGE	\
  if (! TARGET_NUMERICS) {						\
     fixed_regs[32] = fixed_regs[33] = fixed_regs[34] = fixed_regs[35] = 1;\
  }									\

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On 80960, ordinary registers hold 32 bits worth, but can be ganged
   together to hold double or extended precision floating point numbers,
   and the floating point registers hold any size floating point number */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) < 32							\
   ? (((MODE) == VOIDmode)					\
      ? 1 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)) \
   : ((REGNO) < FIRST_PSEUDO_REGISTER) ? 1 : 0)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On 80960, the cpu registers can hold any mode but the float registers
   can only hold SFmode, DFmode, or XFmode.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok ((REGNO), (MODE))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2) || GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* 80960 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 17

/* Actual top-of-stack address is same as
   the contents of the stack pointer register.  */
#define STACK_POINTER_OFFSET (-current_function_outgoing_args_size)

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 15

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
/* ??? It isn't clear to me why this is here.  Perhaps because of a bug (since
   fixed) in the definition of INITIAL_FRAME_POINTER_OFFSET which would have
   caused this to fail.  */
#define FRAME_POINTER_REQUIRED (! leaf_function_p ())

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.. */

#define ELIMINABLE_REGS	 {{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.  */
#define CAN_ELIMINATE(FROM, TO) 1

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.

   Since the stack grows upward on the i960, this must be a negative number.
   This includes the 64 byte hardware register save area and the size of
   the frame.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do { (OFFSET) = - (64 + compute_frame_size (get_frame_size ())); } while (0)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14

/* Register in which static-chain is passed to a function.
   On i960, we use g12.  We can't use any local register, because we need
   a register that can be set before a call or before a jump.  */
#define STATIC_CHAIN_REGNUM 12
 
/* Functions which return large structures get the address
   to place the wanted value at in g13.  */

#define STRUCT_VALUE_REGNUM 13 

/* The order in which to allocate registers.  */

#define	REG_ALLOC_ORDER	\
{  4, 5, 6, 7, 0, 1, 2, 3, 13,	 /* g4, g5, g6, g7, g0, g1, g2, g3, g13  */ \
  20, 21, 22, 23, 24, 25, 26, 27,/* r4, r5, r6, r7, r8, r9, r10, r11  */    \
  28, 29, 30, 31, 19, 8, 9, 10,	 /* r12, r13, r14, r15, r3, g8, g9, g10  */ \
  11, 12,			 /* g11, g12  */			    \
  32, 33, 34, 35,		 /* fp0, fp1, fp2, fp3  */		    \
  /* We can't actually allocate these.  */				    \
  16, 17, 18, 14, 15, 36, 37}	 /* r0, r1, r2, g14, g15, cc  */

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
   class that represents their union.  */
   
/* The 80960 has four kinds of registers, global, local, floating point,
   and condition code.  The cc register is never allocated, so no class
   needs to be defined for it.  */

enum reg_class { NO_REGS, GLOBAL_REGS, LOCAL_REGS, LOCAL_OR_GLOBAL_REGS,
  FP_REGS, ALL_REGS, LIM_REG_CLASSES };

/* 'r' includes floating point registers if TARGET_NUMERICS.  'd' never
   does.  */
#define	GENERAL_REGS	((TARGET_NUMERICS) ? ALL_REGS : LOCAL_OR_GLOBAL_REGS)

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES							\
{ "NO_REGS", "GLOBAL_REGS", "LOCAL_REGS", "LOCAL_OR_GLOBAL_REGS",	\
  "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS	\
{ {0, 0}, {0x0ffff, 0}, {0xffff0000, 0}, {-1,0}, {0, -1}, {-1,-1}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)	\
  ((REGNO) < 16 ? GLOBAL_REGS	\
   : (REGNO) < 32 ? LOCAL_REGS	\
   : (REGNO) < 36 ? FP_REGS	\
   : NO_REGS)

/* The class value for index registers, and the one for base regs.
   There is currently no difference between base and index registers on the
   i960, but this distinction may one day be useful.  */
#define INDEX_REG_CLASS LOCAL_OR_GLOBAL_REGS
#define BASE_REG_CLASS LOCAL_OR_GLOBAL_REGS

/* Get reg_class from a letter such as appears in the machine description.
   'f' is a floating point register (fp0..fp3)
   'l' is a local register (r0-r15)
   'b' is a global register (g0-g15)
   'd' is any local or global register
   'r' or 'g' are pre-defined to the class GENERAL_REGS.  */
/* 'l' and 'b' are probably never used.  Note that 'd' and 'r' are *not*
   the same thing, since 'r' may include the fp registers.  */
#define REG_CLASS_FROM_LETTER(C) \
  (((C) == 'f') && (TARGET_NUMERICS) ? FP_REGS : ((C) == 'l' ? LOCAL_REGS : \
    (C) == 'b' ? GLOBAL_REGS : ((C) == 'd' ? LOCAL_OR_GLOBAL_REGS : NO_REGS)))

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For 80960:
	'I' is used for literal values 0..31
   	'J' means literal 0
	'K' means 0..-31.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  				\
  ((C) == 'I' ? (((unsigned) (VALUE)) <= 31)				\
   : (C) == 'J' ? ((VALUE) == 0)					\
   : (C) == 'K' ? ((VALUE) >= -31 && (VALUE) <= 0)			\
   : (C) == 'M' ? ((VALUE) >= -32 && (VALUE) <= 0)			\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.
   For the 80960, G is 0.0 and H is 1.0.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((TARGET_NUMERICS) &&							\
   (((C) == 'G' && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
    || ((C) == 'H' && ((VALUE) == CONST1_RTX (GET_MODE (VALUE))))))

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

/* On 960, can't load constant into floating-point reg except
   0.0 or 1.0.

   Any hard reg is ok as a src operand of a reload insn.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (GET_CODE (X) == REG && REGNO (X) < FIRST_PSEUDO_REGISTER	\
   ? (CLASS)							\
   : ((CLASS) == FP_REGS && CONSTANT_P (X)			\
      && (X) != CONST0_RTX (DFmode) && (X) != CONST1_RTX (DFmode)\
      && (X) != CONST0_RTX (SFmode) && (X) != CONST1_RTX (SFmode)\
      ? NO_REGS							\
      : (CLASS) == ALL_REGS ? LOCAL_OR_GLOBAL_REGS : (CLASS)))

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class (CLASS, MODE, IN)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On 80960, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
  ((CLASS) == FP_REGS ? 1 : HARD_REGNO_NREGS (0, (MODE)))

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.

   The i960 has a 64 byte register save area, plus possibly some extra
   bytes allocated for varargs functions.  */
#define STARTING_FRAME_OFFSET 64

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On 80960, don't define this because there are no push insns.  */
/* #define PUSH_ROUNDING(BYTES) BYTES */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* When a parameter is passed in a register, no stack space is
   allocated for it.  However, when args are passed in the
   stack, space is allocated for every register parameter.  */
#define MAYBE_REG_PARM_STACK_SPACE 48
#define FINAL_REG_PARM_STACK_SPACE(CONST_SIZE, VAR_SIZE)	\
  i960_final_reg_parm_stack_space (CONST_SIZE, VAR_SIZE);
#define REG_PARM_STACK_SPACE(DECL) i960_reg_parm_stack_space (DECL)
#define OUTGOING_REG_PARM_STACK_SPACE

/* Keep the stack pointer constant throughout the function.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) gen_rtx_REG ((MODE), 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On 80960, returns are in g0..g3 */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* 1 if N is a possible register number for function argument passing.
   On 80960, parameters are passed in g0..g11 */

#define FUNCTION_ARG_REGNO_P(N) ((N) < 12)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL) \
  i960_setup_incoming_varargs(&CUM,MODE,TYPE,&PRETEND_SIZE,NO_RTL)

/* Define the `__builtin_va_list' type for the ABI.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = i960_build_va_list ()

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  i960_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  i960_va_arg (valist, type)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On 80960, this is two integers, which count the number of register
   parameters and the number of stack parameters seen so far.  */

struct cum_args { int ca_nregparms; int ca_nstackparms; };

#define CUMULATIVE_ARGS struct cum_args

/* Define the number of registers that can hold parameters.
   This macro is used only in macro definitions below and/or i960.c.  */
#define NPARM_REGS 12

/* Define how to round to the next parameter boundary.
   This macro is used only in macro definitions below and/or i960.c.  */
#define ROUND_PARM(X, MULTIPLE_OF)	\
  ((((X) + (MULTIPLE_OF) - 1) / (MULTIPLE_OF)) * MULTIPLE_OF)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On 80960, the offset always starts at 0; the first parm reg is g0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  ((CUM).ca_nregparms = 0, (CUM).ca_nstackparms = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   CUM should be advanced to align with the data type accessed and
   also the size of that data type in # of regs.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  i960_function_arg_advance(&CUM, MODE, TYPE, NAMED)

/* Indicate the alignment boundary for an argument of the specified mode and
   type.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  (((TYPE) != 0)							\
   ? ((TYPE_ALIGN (TYPE) <= PARM_BOUNDARY)				\
      ? PARM_BOUNDARY							\
      : TYPE_ALIGN (TYPE))						\
   : ((GET_MODE_ALIGNMENT (MODE) <= PARM_BOUNDARY)			\
      ? PARM_BOUNDARY							\
      : GET_MODE_ALIGNMENT (MODE)))

/* Determine where to put an argument to a function.
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

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)	\
  i960_function_arg(&CUM, MODE, TYPE, NAMED)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(TYPE, FUNC) \
  gen_rtx_REG (TYPE_MODE (TYPE), 0)

/* Force aggregates and objects larger than 16 bytes to be returned in memory,
   since we only have 4 registers available for return values.  */

#define RETURN_IN_MEMORY(TYPE) \
  (TYPE_MODE (TYPE) == BLKmode || int_size_in_bytes (TYPE) > 16)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   This never happens on 80960.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* Output the label for a function definition.
  This handles leaf functions and a few other things for the i960.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
  i960_function_name_declare (FILE, NAME, DECL)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) i960_function_prologue ((FILE), (SIZE))

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	\
  output_function_profiler ((FILE), (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define	EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) i960_function_epilogue (FILE, SIZE)

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
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_FP_P(REGNO) \
  ((REGNO) < 36 || (unsigned) reg_renumber[REGNO] < 36)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the 960, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REGNO (X) >= 32 && REGNO (X) < 36)

/* Maximum number of registers that can appear in a valid memory address.  */
#define	MAX_REGS_PER_ADDRESS 2

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* LEGITIMATE_CONSTANT_P is nonzero if the constant value X
   is a legitimate general operand.
   It is given that X satisfies CONSTANT_P.

   Anything but a CONST_DOUBLE can be made to work, excepting 0.0 and 1.0.

   ??? This probably should be defined to 1.  */

#define LEGITIMATE_CONSTANT_P(X) \
  ((GET_CODE (X) != CONST_DOUBLE) || fp_literal ((X), GET_MODE (X)))

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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_INDEX_P_STRICT(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P_STRICT(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

	On 80960, legitimate addresses are:
		base				ld	(g0),r0
		disp	(12 or 32 bit)		ld	foo,r0
		base + index			ld	(g0)[g1*1],r0
		base + displ			ld	0xf00(g0),r0
		base + index*scale + displ	ld	0xf00(g0)[g1*4],r0
		index*scale + base		ld	(g0)[g1*4],r0
		index*scale + displ		ld	0xf00[g1*4],r0
		index*scale			ld	[g1*4],r0
		index + base + displ		ld	0xf00(g0)[g1*1],r0

	In each case, scale can be 1, 2, 4, 8, or 16.  */

/* Returns 1 if the scale factor of an index term is valid. */
#define SCALE_TERM_P(X)							\
  (GET_CODE (X) == CONST_INT						\
   && (INTVAL (X) == 1 || INTVAL (X) == 2 || INTVAL (X) == 4 		\
       || INTVAL(X) == 8 || INTVAL (X) == 16))


#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) \
  { if (legitimate_address_p (MODE, X, 1)) goto ADDR; }
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) \
  { if (legitimate_address_p (MODE, X, 0)) goto ADDR; }
#endif

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On 80960, convert non-canonical addresses to canonical form.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)	\
{ rtx orig_x = (X);				\
  (X) = legitimize_address (X, OLDX, MODE);	\
  if ((X) != orig_x && memory_address_p (MODE, X)) \
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 960 this is never true.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Allow and ignore #sccs directives.  */
#define	SCCS_DIRECTIVE

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is no faster than for words.
   Value changed to 1 after reports of poor bitfield code with g++.
   Indications are that code is usually as good, sometimes better. */   

#define SLOW_BYTE_ACCESS 1

/* Force sizeof(bool) == 1 to maintain binary compatibility; otherwise, the
   change in SLOW_BYTE_ACCESS would have changed it to 4.  */

#define BOOL_TYPE_SIZE CHAR_TYPE_SIZE

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* Specify the widest mode that BLKmode objects can be promoted to */
#define	MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TImode)

/* These global variables are used to pass information between
   cc setter and cc user at insn emit time.  */

extern struct rtx_def *i960_compare_op0, *i960_compare_op1;

/* Add any extra modes needed to represent the condition code.

   Also, signed and unsigned comparisons are distinguished, as
   are operations which are compatible with chkbit insns.  */
#define EXTRA_CC_MODES		\
    CC(CC_UNSmode, "CC_UNS")	\
    CC(CC_CHKmode, "CC_CHK")

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CC_NOOVmode should be used when the first operand is a
   PLUS, MINUS, or NEG.  CCmode should be used when no special processing is
   needed.  */
#define SELECT_CC_MODE(OP,X,Y) select_cc_mode (OP, X)

/* A function address in a call instruction is a byte address
   (for indexing purposes) so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Use memcpy, etc. instead of bcopy.  */

#ifndef WIND_RIVER
#define	TARGET_MEM_FUNCTIONS	1
#endif

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

/* Constants that can be (non-ldconst) insn operands are cost 0.  Constants
   that can be non-ldconst operands in rare cases are cost 1.  Other constants
   have higher costs.  */

/* Must check for OUTER_CODE of SET for power2_operand, because
   reload_cse_move2add calls us with OUTER_CODE of PLUS to decide when
   to replace set with add.  */

#define CONST_COSTS(RTX, CODE, OUTER_CODE)				\
  case CONST_INT:							\
    if ((INTVAL (RTX) >= 0 && INTVAL (RTX) < 32)			\
	|| (OUTER_CODE == SET && power2_operand (RTX, VOIDmode)))	\
      return 0; 							\
    else if (INTVAL (RTX) >= -31 && INTVAL (RTX) < 0)			\
      return 1;								\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return (TARGET_C_SERIES ? 6 : 8);					\
  case CONST_DOUBLE:							\
    if ((RTX) == CONST0_RTX (DFmode) || (RTX) == CONST0_RTX (SFmode)	\
	|| (RTX) == CONST1_RTX (DFmode) || (RTX) == CONST1_RTX (SFmode))\
      return 1;								\
    return 12;

/* The i960 offers addressing modes which are "as cheap as a register".
   See i960.c (or gcc.texinfo) for details.  */

#define ADDRESS_COST(RTX) \
  (GET_CODE (RTX) == REG ? 1 : i960_address_cost (RTX))

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(file)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES {						\
	"g0", "g1", "g2",  "g3",  "g4",  "g5",  "g6",  "g7",		\
	"g8", "g9", "g10", "g11", "g12", "g13", "g14", "fp",		\
	"pfp","sp", "rip", "r3",  "r4",  "r5",  "r6",  "r7",		\
	"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
	"fp0","fp1","fp2", "fp3", "cc", "fake" }

/* How to renumber registers for dbx and gdb.
   In the 960 encoding, g0..g15 are registers 16..31.  */

#define DBX_REGISTER_NUMBER(REGNO)					\
  (((REGNO) < 16) ? (REGNO) + 16					\
   : (((REGNO) > 31) ? (REGNO) : (REGNO) - 16))

/* Don't emit dbx records longer than this.  This is an arbitrary value.  */
#define DBX_CONTIN_LENGTH 1500

/* This is how to output a note to DBX telling it the line number
   to which the following sequence of instructions corresponds. */

#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)			\
{ if (write_symbols == SDB_DEBUG) {				\
    fprintf ((FILE), "\t.ln	%d\n",				\
	     (sdb_begin_function_line				\
	      ? (LINE) - sdb_begin_function_line : 1));		\
  } else if (write_symbols == DBX_DEBUG) {			\
	fprintf((FILE),"\t.stabd	68,0,%d\n",(LINE));	\
  } }

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)		\
{ fputs ("\t.globl ", FILE);			\
  assemble_name (FILE, NAME);			\
  fputs ("\n", FILE); }

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `long double'
   constant.  */

#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE) i960_output_long_double(FILE, VALUE)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  i960_output_double(FILE, VALUE)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  i960_output_float(FILE, VALUE)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.short "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)	\
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tst\t%s,(sp)\n\taddo\t4,sp,sp\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tsubo\t4,sp,sp\n\tld\t(sp),%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

/* For common objects, output unpadded size... gld960 & lnk960 both
   have code to align each common object at link time.  Also, if size
   is 0, treat this as a declaration, not a definition - i.e.,
   do nothing at all.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
{ if ((SIZE) != 0)					\
    {							\
      fputs (".globl ", (FILE)),			\
      assemble_name ((FILE), (NAME)),			\
      fputs ("\n.comm ", (FILE)),			\
      assemble_name ((FILE), (NAME)),			\
      fprintf ((FILE), ",%d\n", (SIZE));		\
    }							\
}

/* This says how to output an assembler line to define a local common symbol.
   Output unpadded size, with request to linker to align as requested.
   0 size should not be possible here.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)  \
( fputs (".bss\t", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d,%d\n", (SIZE),		\
	   (floor_log2 ((ALIGN) / BITS_PER_UNIT))))

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
  do {								\
    fputs (".globl ", (FILE));					\
    assemble_name ((FILE), (NAME));				\
    fputs ("\n", (FILE));					\
    ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);		\
  } while (0)

/* Output text for an #ident directive.  */
#define	ASM_OUTPUT_IDENT(FILE, STR)  fprintf(FILE, "\t# %s\n", STR);

/* Align code to 8 byte boundary if TARGET_CODE_ALIGN is true.  */

#define	LABEL_ALIGN_AFTER_BARRIER(LABEL) (TARGET_CODE_ALIGN ? 3 : 0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
	( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
	  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO) \
{ fprintf (FILE, "\tld	LPBX0,g12\n");			\
  fprintf (FILE, "\tcmpobne	0,g12,LPY%d\n",LABELNO);\
  fprintf (FILE, "\tlda	LPBX0,g12\n");			\
  fprintf (FILE, "\tcall	___bb_init_func\n");	\
  fprintf (FILE, "LPY%d:\n",LABELNO); }

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO) \
{ int blockn = (BLOCKNO);				\
  fprintf (FILE, "\tld	LPBX2+%d,g12\n", 4 * blockn);	\
  fprintf (FILE, "\taddo	g12,1,g12\n");		\
  fprintf (FILE, "\tst	g12,LPBX2+%d\n", 4 * blockn); }

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  \
  i960_print_operand (FILE, X, CODE);

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)	\
  i960_print_operand_addr (FILE, ADDR)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the i960, the trampoline contains three instructions:
     ldconst _function, r4
     ldconst static addr, g12
     jump (r4)  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x8C203000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x00000000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x8CE03000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x00000000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x84212000));	\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 4)), FNADDR); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 12)), CXT); \
}

/* Generate RTL to flush the register windows so as to make arbitrary frames
   available.  */
#define SETUP_FRAME_ADDRESSES()		\
  emit_insn (gen_flush_register_windows ())

#define BUILTIN_SETJMP_FRAME_VALUE hard_frame_pointer_rtx

#if 0
/* Promote char and short arguments to ints, when want compatibility with
   the iC960 compilers.  */

/* ??? In order for this to work, all users would need to be changed
   to test the value of the macro at run time.  */
#define PROMOTE_PROTOTYPES	TARGET_CLEAN_LINKAGE
/* ??? This does not exist.  */
#define PROMOTE_RETURN		TARGET_CLEAN_LINKAGE
#endif

/* Instruction type definitions.  Used to alternate instructions types for
   better performance on the C series chips.  */

enum insn_types { I_TYPE_REG, I_TYPE_MEM, I_TYPE_CTRL };

/* Holds the insn type of the last insn output to the assembly file.  */

extern enum insn_types i960_last_insn_type;

/* Parse opcodes, and set the insn last insn type based on them.  */

#define ASM_OUTPUT_OPCODE(FILE, INSN)	i960_scan_opcode (INSN)

/* Table listing what rtl codes each predicate in i960.c will accept.  */

#define PREDICATE_CODES \
  {"fpmove_src_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,	\
			  LABEL_REF, SUBREG, REG, MEM}},		\
  {"arith_operand", {SUBREG, REG, CONST_INT}},				\
  {"logic_operand", {SUBREG, REG, CONST_INT}},				\
  {"fp_arith_operand", {SUBREG, REG, CONST_DOUBLE}},			\
  {"signed_arith_operand", {SUBREG, REG, CONST_INT}},			\
  {"literal", {CONST_INT}},						\
  {"fp_literal_one", {CONST_DOUBLE}},					\
  {"fp_literal_double", {CONST_DOUBLE}},				\
  {"fp_literal", {CONST_DOUBLE}},					\
  {"signed_literal", {CONST_INT}},					\
  {"symbolic_memory_operand", {SUBREG, MEM}},				\
  {"eq_or_neq", {EQ, NE}},						\
  {"arith32_operand", {SUBREG, REG, LABEL_REF, SYMBOL_REF, CONST_INT,	\
		       CONST_DOUBLE, CONST}},				\
  {"power2_operand", {CONST_INT}},					\
  {"cmplpower2_operand", {CONST_INT}},

/* Defined in reload.c, and used in insn-recog.c.  */

extern int rtx_equal_function_value_matters;

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	\
do {									\
  int d = (DELTA);							\
  if (d < 0 && d > -32)							\
    fprintf (FILE, "\tsubo %d,g0,g0\n", -d);				\
  else if (d > 0 && d < 32)						\
    fprintf (FILE, "\taddo %d,g0,g0\n", d);				\
  else									\
    {									\
      fprintf (FILE, "\tldconst %d,r5\n", d);				\
      fprintf (FILE, "\taddo r5,g0,g0\n");				\
    }									\
  fprintf (FILE, "\tbx ");						\
  assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	\
  fprintf (FILE, "\n");							\
} while (0);
