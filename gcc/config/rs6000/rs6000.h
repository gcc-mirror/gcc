/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-D_IBMR2 -D_POWER -D_AIX -D_AIX32 \
-Asystem(unix) -Asystem(aix) -Acpu(rs6000) -Amachine(rs6000)"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION ;

/* Tell the assembler to assume that all undefined names are external.

   Don't do this until the fixed IBM assembler is more generally available.
   When this becomes permanently defined, the ASM_OUTPUT_EXTERNAL,
   ASM_OUTPUT_EXTERNAL_LIBCALL, and RS6000_OUTPUT_BASENAME macros will no
   longer be needed.  Also, the extern declaration of mcount in ASM_FILE_START
   will no longer be needed.  */

/* #define ASM_SPEC "-u" */

/* Define appropriate architecture macros for preprocessor depending on
   target switches.  */

#define CPP_SPEC "\
%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{mno-power: %{!mpowerpc*: -D_ARCH_COM}} \
  %{!mno-power: %{!mpower2: -D_ARCH_PWR}}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=mpc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=ppc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=mpc603: -D_ARCH_PPC} \
%{mcpu=ppc603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=mpc604: -D_ARCH_PPC} \
%{mcpu=ppc604: -D_ARCH_PPC}"

/* Define the options for the binder: Start text at 512, align all segments
   to 512 bytes, and warn if there is text relocation.

   The -bhalt:4 option supposedly changes the level at which ld will abort,
   but it also suppresses warnings about multiply defined symbols and is
   used by the AIX cc command.  So we use it here.

   -bnodelcsect undoes a poor choice of default relating to multiply-defined
   csects.  See AIX documentation for more information about this.  */

#define LINK_SPEC "-T512 -H512 %{!r:-btextro} -bhalt:4 -bnodelcsect\
   %{static:-bnso -bI:/lib/syscalls.exp} %{g*:-bexport:/usr/lib/libg.exp}"

/* Profiled library versions are used by linking with special directories.  */
#define LIB_SPEC "%{pg:-L/lib/profiled -L/usr/lib/profiled}\
   %{p:-L/lib/profiled -L/usr/lib/profiled} %{g*:-lg} -lc"

/* gcc must do the search itself to find libgcc.a, not use -l.  */
#define LINK_LIBGCC_SPECIAL_1

/* Don't turn -B into -L if the argument specifies a relative file name.  */
#define RELATIVE_PREFIX_NOT_LINKDIR

/* Architecture type.  */

extern int target_flags;

/* Use POWER architecture instructions and MQ register.  */
#define MASK_POWER		0x01

/* Use POWER2 extensions to POWER architecture.  */
#define MASK_POWER2		0x02

/* Use PowerPC architecture instructions.  */
#define MASK_POWERPC		0x04

/* Use PowerPC General Purpose group optional instructions, e.g. fsqrt.  */
#define MASK_PPC_GPOPT		0x08

/* Use PowerPC Graphics group optional instructions, e.g. fsel.  */
#define MASK_PPC_GFXOPT		0x10

/* Use PowerPC-64 architecture instructions.  */
#define MASK_POWERPC64		0x20

/* Use revised mnemonic names defined for PowerPC architecture.  */
#define MASK_NEW_MNEMONICS	0x40

/* Disable placing fp constants in the TOC; can be turned on when the
   TOC overflows.  */
#define MASK_NO_FP_IN_TOC	0x80

/* Disable placing symbol+offset constants in the TOC; can be turned on when
   the TOC overflows.  */
#define MASK_NO_SUM_IN_TOC	0x100

/* Output only one TOC entry per module.  Normally linking fails if
   there are more than 16K unique variables/constants in an executable.  With
   this option, linking fails only if there are more than 16K modules, or
   if there are more than 16K unique variables/constant in a single module.

   This is at the cost of having 2 extra loads and one extra store per
   function, and one less allocatable register.  */
#define MASK_MINIMAL_TOC	0x200

#define TARGET_POWER			(target_flags & MASK_POWER)
#define TARGET_POWER2			(target_flags & MASK_POWER2)
#define TARGET_POWERPC			(target_flags & MASK_POWERPC)
#define TARGET_PPC_GPOPT		(target_flags & MASK_PPC_GPOPT)
#define TARGET_PPC_GFXOPT		(target_flags & MASK_PPC_GFXOPT)
#define TARGET_POWERPC64		(target_flags & MASK_POWERPC64)
#define TARGET_NEW_MNEMONICS		(target_flags & MASK_NEW_MNEMONICS)
#define TARGET_NO_FP_IN_TOC		(target_flags & MASK_NO_FP_IN_TOC)
#define TARGET_NO_SUM_IN_TOC		(target_flags & MASK_NO_SUM_IN_TOC)
#define TARGET_MINIMAL_TOC		(target_flags & MASK_MINIMAL_TOC)

/* Run-time compilation parameters selecting different hardware subsets.

   Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES						\
 {{"power",		MASK_POWER},				\
  {"power2",		MASK_POWER | MASK_POWER2},		\
  {"no-power2",		- MASK_POWER2},				\
  {"no-power",		- (MASK_POWER | MASK_POWER2)},		\
  {"powerpc",		MASK_POWERPC},				\
  {"no-powerpc",	- (MASK_POWERPC | MASK_PPC_GPOPT 	\
			   | MASK_PPC_GFXOPT | MASK_POWERPC64)}, \
  {"powerpc-gpopt",	MASK_POWERPC | MASK_PPC_GPOPT},		\
  {"no-powerpc-gpopt",	- MASK_PPC_GPOPT},			\
  {"powerpc-gfxopt",	MASK_POWERPC | MASK_PPC_GFXOPT},	\
  {"no-powerpc-gfxopt",	- MASK_PPC_GFXOPT},			\
  {"new-mnemonics",	MASK_NEW_MNEMONICS},			\
  {"old-mnemonics",	-MASK_NEW_MNEMONICS},			\
  {"full-toc",		- (MASK_NO_FP_IN_TOC | MASK_NO_SUM_IN_TOC \
			   | MASK_MINIMAL_TOC)},		\
  {"fp-in-toc",		- MASK_NO_FP_IN_TOC},			\
  {"no-fp-in-toc",	MASK_NO_FP_IN_TOC},			\
  {"sum-in-toc",	- MASK_NO_SUM_IN_TOC},			\
  {"no-sum-in-toc",	MASK_NO_SUM_IN_TOC},			\
  {"minimal-toc",	MASK_MINIMAL_TOC},			\
  {"minimal-toc",	- (MASK_NO_FP_IN_TOC | MASK_NO_SUM_IN_TOC)}, \
  {"no-minimal-toc",	- MASK_MINIMAL_TOC},			\
  {"",			TARGET_DEFAULT}}

#define TARGET_DEFAULT MASK_POWER

/* Processor type.  */
enum processor_type
 {PROCESSOR_RIOS1,
  PROCESSOR_RIOS2,
  PROCESSOR_PPC601,
  PROCESSOR_PPC603,
  PROCESSOR_PPC604,
  PROCESSOR_PPC620};

extern enum processor_type rs6000_cpu;

/* Recast the processor type to the cpu attribute.  */
#define rs6000_cpu_attr ((enum attr_cpu)rs6000_cpu)

/* Define generic processor types based upon current deployment.  */
#define PROCESSOR_COMMON  PROCESSOR_PPC601
#define PROCESSOR_POWER   PROCESSOR_RIOS1
#define PROCESSOR_POWERPC PROCESSOR_PPC601

/* Define the default processor.  This is overridden by other tm.h files.  */
#define PROCESSOR_DEFAULT PROCESSOR_RIOS1

/* Specify the dialect of assembler to use.  New mnemonics is dialect one
   and the old mnemonics are dialect zero.  */
#define ASSEMBLER_DIALECT TARGET_NEW_MNEMONICS ? 1 : 0

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.
   The variable, type `char *', is set to the variable part of the
   given option if the fixed part matches.  The actual option name
   is made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

	extern char *m88k_short_data;
	#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

#define TARGET_OPTIONS		\
{ {"cpu=", &rs6000_cpu_string}}

extern char *rs6000_cpu_string;

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   On the RS/6000 this is used to define the target cpu type.  */

#define OVERRIDE_OPTIONS rs6000_override_options ()

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* target machine storage layout */

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT	\
      && GET_MODE_SIZE (MODE) < 4)  	\
    (MODE) = SImode;

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */
/* That is true on RS/6000. */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on RS/6000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered. 

   For RS/6000 we can decide arbitrarily since there are no machine
   instructions for them.  Might as well be consistent with bits and bytes. */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
#define WCHAR_TYPE "short unsigned int"

/* Width of wchar_t in bits.  */
#define WCHAR_TYPE_SIZE 16

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   RS/6000 has 32 fixed-point registers, 32 floating-point registers,
   an MQ register, a count register, a link register, and 8 condition
   register fields, which we view here as separate registers.

   In addition, the difference between the frame and argument pointers is
   a function of the number of registers saved, so we need to have a
   register for AP that will later be eliminated in favor of SP or FP.
   This is a normal register, but it is fixed.  */

#define FIRST_PSEUDO_REGISTER 76

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On RS/6000, r1 is used for the stack and r2 is used as the TOC pointer.  

   cr5 is not supposed to be used.  */

#define FIXED_REGISTERS  \
  {0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS  \
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1}

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
	fp0		(not saved or used for anything)
	fp13 - fp2	(not saved; incoming fp arg registers)
	fp1		(not saved; return value)
 	fp31 - fp14	(saved; order given to save least number)
	cr1, cr6, cr7	(not saved or special)
	cr0		(not saved, but used for arithmetic operations)
	cr2, cr3, cr4	(saved)
        r0		(not saved; cannot be base reg)
	r9		(not saved; best for TImode)
	r11, r10, r8-r4	(not saved; highest used first to make less conflict)
	r3     		(not saved; return value register)
	r31 - r13	(saved; order given to save least number)
	r12		(not saved; if used for DImode or DFmode would use r13)
	mq		(not saved; best to use it if we can)
	ctr		(not saved; when we have the choice ctr is better)
	lr		(saved)
        cr5, r1, r2, ap	(fixed)  */

#define REG_ALLOC_ORDER					\
  {32, 							\
   45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34,	\
   33,							\
   63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51,	\
   50, 49, 48, 47, 46, 					\
   69, 74, 75, 68, 70, 71, 72,				\
   0,							\
   9, 11, 10, 8, 7, 6, 5, 4,				\
   3,							\
   31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19,	\
   18, 17, 16, 15, 14, 13, 12,				\
   64, 66, 65, 						\
   73, 1, 2, 67}

/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= 32 && (N) <= 63)

/* True if register is a condition register.  */
#define CR_REGNO_P(N) ((N) >= 68 && (N) <= 75)

/* True if register is an integer register.  */
#define INT_REGNO_P(N) ((N) <= 31 || (N) == 67)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On RS/6000, ordinary registers hold 32 bits worth;
   a single floating point register holds 64 bits worth.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (FP_REGNO_P (REGNO)			\
   ? ((GET_MODE_SIZE (MODE) + 2 * UNITS_PER_WORD - 1) / (2 * UNITS_PER_WORD)) \
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   For POWER and PowerPC, the GPRs can hold any mode, but the float
   registers only can hold floating modes and DImode, and CR register only
   can hold CC modes.  We cannot put TImode anywhere except general
   register and it must be able to fit within the register set. */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (FP_REGNO_P (REGNO) ?						\
   (GET_MODE_CLASS (MODE) == MODE_FLOAT				\
    || (GET_MODE_CLASS (MODE) == MODE_INT			\
	&& GET_MODE_SIZE (MODE) == 2 * UNITS_PER_WORD))		\
   : CR_REGNO_P (REGNO) ? GET_MODE_CLASS (MODE) == MODE_CC	\
   : ! INT_REGNO_P (REGNO) ? (GET_MODE_CLASS (MODE) == MODE_INT	\
			      && GET_MODE_SIZE (MODE) <= UNITS_PER_WORD) \
   : 1)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
   ? GET_MODE_CLASS (MODE2) == MODE_FLOAT	\
   : GET_MODE_CLASS (MODE2) == MODE_FLOAT	\
   ? GET_MODE_CLASS (MODE1) == MODE_FLOAT	\
   : GET_MODE_CLASS (MODE1) == MODE_CC		\
   ? GET_MODE_CLASS (MODE2) == MODE_CC		\
   : GET_MODE_CLASS (MODE2) == MODE_CC		\
   ? GET_MODE_CLASS (MODE1) == MODE_CC		\
   : 1)

/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.

   On the RS/6000, copying between floating-point and fixed-point
   registers is expensive.  */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)			\
  ((CLASS1) == FLOAT_REGS && (CLASS2) == FLOAT_REGS ? 2		\
   : (CLASS1) == FLOAT_REGS && (CLASS2) != FLOAT_REGS ? 10	\
   : (CLASS1) != FLOAT_REGS && (CLASS2) == FLOAT_REGS ? 10	\
   : (((CLASS1) == SPECIAL_REGS || (CLASS1) == MQ_REGS		\
       || (CLASS1) == LINK_REGS || (CLASS1) == CTR_REGS)	\
      && ((CLASS2) == SPECIAL_REGS || (CLASS2) == MQ_REGS	\
	  || (CLASS2) == LINK_REGS || (CLASS2) == CTR_REGS)) ? 10 \
   : 2)

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory.

   On the RS/6000, bump this up a bit.  */

#define MEMORY_MOVE_COST(MODE)		\
  ((GET_MODE_CLASS (MODE) == MODE_FLOAT	\
    && (rs6000_cpu == PROCESSOR_RIOS1 || rs6000_cpu == PROCESSOR_PPC601) \
    ? 3 : 2) \
   + 4)

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Set this to 3 on the RS/6000 since that is roughly the average cost of an
   unscheduled conditional branch.  */

#define BRANCH_COST 3

/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  On the RS/6000, ignore the cost of anti- and
   output-dependencies.  In fact, output dependencies on the CR do have
   a cost, but it is probably not worthwhile to track it.  */

#define ADJUST_COST(INSN,LINK,DEP_INSN,COST)				\
  (COST) = rs6000_adjust_cost (INSN,LINK,DEP_INSN,COST)

/* Define this macro to change register usage conditional on target flags.
   Set MQ register fixed (already call_used) if not POWER architecture
   (RIOS1, RIOS2, RSC, and PPC601) so that it will not be allocated.  */

#define CONDITIONAL_REGISTER_USAGE					\
    if (!TARGET_POWER)							\
	fixed_regs[64] = 1;

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* RS/6000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 31

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 67

/* Place to put static chain when calling a function that requires it.  */
#define STATIC_CHAIN_REGNUM 11

/* Place that structure value return address is placed.

   On the RS/6000, it is passed as an extra parameter.  */
#define STRUCT_VALUE	0

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
   
/* The RS/6000 has three types of registers, fixed-point, floating-point,
   and condition registers, plus three special registers, MQ, CTR, and the
   link register.

   However, r0 is special in that it cannot be used as a base register.
   So make a class for registers valid as base registers.

   Also, cr0 is the only condition code register that can be used in
   arithmetic insns, so make a separate class for it. */

enum reg_class { NO_REGS, BASE_REGS, GENERAL_REGS, FLOAT_REGS,
  NON_SPECIAL_REGS, MQ_REGS, LINK_REGS, CTR_REGS, LINK_OR_CTR_REGS,
  SPECIAL_REGS, SPEC_OR_GEN_REGS, CR0_REGS, CR_REGS, NON_FLOAT_REGS,
  ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES					 	\
  { "NO_REGS", "BASE_REGS", "GENERAL_REGS", "FLOAT_REGS",	\
    "NON_SPECIAL_REGS", "MQ_REGS", "LINK_REGS", "CTR_REGS",	\
    "LINK_OR_CTR_REGS", "SPECIAL_REGS", "SPEC_OR_GEN_REGS",	\
    "CR0_REGS", "CR_REGS", "NON_FLOAT_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS				\
  { {0, 0, 0}, {0xfffffffe, 0, 8}, {~0, 0, 8},		\
    {0, ~0, 0}, {~0, ~0, 8}, {0, 0, 1}, {0, 0, 2},	\
    {0, 0, 4}, {0, 0, 6}, {0, 0, 7}, {~0, 0, 15},	\
    {0, 0, 16}, {0, 0, 0xff0}, {~0, 0, 0xffff},		\
    {~0, ~0, 0xffff} }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)	\
 ((REGNO) == 0 ? GENERAL_REGS	\
  : (REGNO) < 32 ? BASE_REGS	\
  : FP_REGNO_P (REGNO) ? FLOAT_REGS \
  : (REGNO) == 68 ? CR0_REGS	\
  : CR_REGNO_P (REGNO) ? CR_REGS \
  : (REGNO) == 64 ? MQ_REGS	\
  : (REGNO) == 65 ? LINK_REGS	\
  : (REGNO) == 66 ? CTR_REGS	\
  : (REGNO) == 67 ? BASE_REGS	\
  : NO_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS BASE_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FLOAT_REGS	\
   : (C) == 'b' ? BASE_REGS	\
   : (C) == 'h' ? SPECIAL_REGS	\
   : (C) == 'q' ? MQ_REGS	\
   : (C) == 'c' ? CTR_REGS	\
   : (C) == 'l' ? LINK_REGS	\
   : (C) == 'x' ? CR0_REGS	\
   : (C) == 'y' ? CR_REGS	\
   : NO_REGS)

/* The letters I, J, K, L, M, N, and P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is signed 16-bit constants 
   `J' is a constant with only the high-order 16 bits non-zero
   `K' is a constant with only the low-order 16 bits non-zero
   `L' is a constant that can be placed into a mask operand
   `M' is a constant that is greater than 31
   `N' is a constant that is an exact power of two
   `O' is the constant zero
   `P' is a constant whose negation is a signed 16-bit constant */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
   ( (C) == 'I' ? (unsigned) ((VALUE) + 0x8000) < 0x10000	\
   : (C) == 'J' ? ((VALUE) & 0xffff) == 0			\
   : (C) == 'K' ? ((VALUE) & 0xffff0000) == 0			\
   : (C) == 'L' ? mask_constant (VALUE)				\
   : (C) == 'M' ? (VALUE) > 31					\
   : (C) == 'N' ? exact_log2 (VALUE) >= 0			\
   : (C) == 'O' ? (VALUE) == 0					\
   : (C) == 'P' ? (unsigned) ((- (VALUE)) + 0x8000) < 0x1000	\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.

   We flag for special constants when we can copy the constant into
   a general register in two insns for DF and one insn for SF.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? easy_fp_constant (VALUE, GET_MODE (VALUE)) : 0)

/* Optional extra constraints for this machine.

   For the RS/6000, `Q' means that this is a memory operand that is just
   an offset from a register.  */

#define EXTRA_CONSTRAINT(OP, C)						\
  ((C) == 'Q' ? GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG	\
   : (C) == 'R' ? GET_CODE (OP) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (OP)\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class. 

   On the RS/6000, we have to return NO_REGS when we want to reload a
   floating-point CONST_DOUBLE to force it to be copied to memory.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)	\
  ((GET_CODE (X) == CONST_DOUBLE			\
    && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)	\
   ? NO_REGS : (CLASS))
   
/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class (CLASS, MODE, IN)

/* If we are copying between FP registers and anything else, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE) \
 ((CLASS1) != (CLASS2) && ((CLASS1) == FLOAT_REGS || (CLASS2) == FLOAT_REGS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On RS/6000, this is the size of MODE in words,
   except in the FP regs, where a single reg is enough for two words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FLOAT_REGS			\
  ? ((GET_MODE_SIZE (MODE) + 2 * UNITS_PER_WORD - 1) / (2 * UNITS_PER_WORD)) \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the size of the object.  */

#define CLASS_CANNOT_CHANGE_SIZE	FLOAT_REGS

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.

   On the RS/6000, we grow upwards, from the area after the outgoing
   arguments.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated. 

   On the RS/6000, the frame pointer is the same as the stack pointer,
   except for dynamic allocations.  So we start after the fixed area and
   outgoing parameter area.  */

#define STARTING_FRAME_OFFSET (current_function_outgoing_args_size + 24)

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On RS/6000, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   On the RS/6000, we define the argument pointer to the start of the fixed
   area.  */
#define FIRST_PARM_OFFSET(FNDECL) 24

/* Define this if stack space is still allocated for a parameter passed
   in a register.  The value is the number of bytes allocated to this
   area.  */
#define REG_PARM_STACK_SPACE(FNDECL)	32

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* This is the difference between the logical top of stack and the actual sp.

   For the RS/6000, sp points past the fixed area. */
#define STACK_POINTER_OFFSET 24

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.

   On RS/6000 an integer value is in r3 and a floating-point value is in 
   fp1.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)	\
  gen_rtx (REG, TYPE_MODE (VALTYPE),	\
	   TREE_CODE (VALTYPE) == REAL_TYPE ? 33 : 3)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)		\
  gen_rtx (REG, MODE, GET_MODE_CLASS (MODE) == MODE_FLOAT ? 33 : 3)

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.

   For the RS/6000, any structure or union type is returned in memory.  */

#define RETURN_IN_MEMORY(TYPE) \
  (TYPE_MODE (TYPE) == BLKmode)

/* 1 if N is a possible register number for a function value
   as seen by the caller.

   On RS/6000, this is r3 and fp1.  */

#define FUNCTION_VALUE_REGNO_P(N)  ((N) == 3 || ((N) == 33))

/* 1 if N is a possible register number for function argument passing.
   On RS/6000, these are r3-r10 and fp1-fp13.  */

#define FUNCTION_ARG_REGNO_P(N)	\
  (((N) <= 10 && (N) >= 3) || ((N) >= 33 && (N) <= 45))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the RS/6000, this is a structure.  The first element is the number of
   total argument words, the second is used to store the next
   floating-point register number, and the third says how many more args we
   have prototype types for.  */

struct rs6000_args {int words, fregno, nargs_prototype; };
#define CUMULATIVE_ARGS struct rs6000_args

/* Define intermediate macro to compute the size (in registers) of an argument
   for the RS/6000.  */

#define RS6000_ARG_SIZE(MODE, TYPE, NAMED)				\
(! (NAMED) ? 0								\
 : (MODE) != BLKmode							\
 ? (GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD 	\
 : (int_size_in_bytes (TYPE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
  (CUM).words = 0,				\
  (CUM).fregno = 33,				\
  (CUM).nargs_prototype = (FNTYPE && TYPE_ARG_TYPES (FNTYPE)		\
			   ? (list_length (TYPE_ARG_TYPES (FNTYPE)) - 1 \
			      + (TYPE_MODE (TREE_TYPE (FNTYPE)) == BLKmode \
				 || RETURN_IN_MEMORY (TREE_TYPE (FNTYPE)))) \
			   : 0)

/* Similar, but when scanning the definition of a procedure.  We always
   set NARGS_PROTOTYPE large so we never return an EXPR_LIST.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM,FNTYPE,IGNORE) \
  (CUM).words = 0,				\
  (CUM).fregno = 33,				\
  (CUM).nargs_prototype = 1000

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
{ (CUM).nargs_prototype--;				\
  if (NAMED)						\
    {							\
      (CUM).words += RS6000_ARG_SIZE (MODE, TYPE, NAMED); \
      if (GET_MODE_CLASS (MODE) == MODE_FLOAT)		\
	(CUM).fregno++;					\
    }							\
}

/* Non-zero if we can use a floating-point register to pass this arg.  */
#define USE_FP_FOR_ARG_P(CUM,MODE,TYPE)	\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT && (CUM).fregno < 46)

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
    (otherwise it is an extra parameter matching an ellipsis).

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  The first 13 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when TYPE is zero) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support EXPR_LIST anyway.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				\
  (! (NAMED) ? 0							\
   : ((TYPE) != 0 && TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST) ? 0	\
   : USE_FP_FOR_ARG_P (CUM, MODE, TYPE)					\
   ? ((CUM).nargs_prototype > 0 || (TYPE) == 0				\
      ? gen_rtx (REG, MODE, (CUM).fregno)				\
      : ((CUM).words < 8						\
	 ? gen_rtx (EXPR_LIST, VOIDmode,				\
		    gen_rtx (REG, (MODE), 3 + (CUM).words),		\
		    gen_rtx (REG, (MODE), (CUM).fregno))		\
	 : gen_rtx (EXPR_LIST, VOIDmode, 0,				\
		    gen_rtx (REG, (MODE), (CUM).fregno))))		\
   : (CUM).words < 8 ? gen_rtx(REG, (MODE), 3 + (CUM).words) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)		\
  (! (NAMED) ? 0							\
   : USE_FP_FOR_ARG_P (CUM, MODE, TYPE) && (CUM).nargs_prototype >= 0 ? 0 \
   : (((CUM).words < 8							\
       && 8 < ((CUM).words + RS6000_ARG_SIZE (MODE, TYPE, NAMED)))	\
      ? 8 - (CUM).words : 0))

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM).words < 8)							\
    {									\
      int first_reg_offset = (CUM).words;				\
									\
      if (MUST_PASS_IN_STACK (MODE, TYPE))				\
	first_reg_offset += RS6000_ARG_SIZE (TYPE_MODE (TYPE), TYPE, 1); \
									\
      if (first_reg_offset > 8)						\
	first_reg_offset = 8;						\
									\
      if (! (NO_RTL) && first_reg_offset != 8)				\
	move_block_from_reg						\
	  (3 + first_reg_offset,					\
	   gen_rtx (MEM, BLKmode,					\
		    plus_constant (virtual_incoming_args_rtx,		\
				   first_reg_offset * 4)),		\
	   8 - first_reg_offset, (8 - first_reg_offset) * UNITS_PER_WORD); \
      PRETEND_SIZE = (8 - first_reg_offset) * UNITS_PER_WORD;		\
    }									\
}

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) output_prolog (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	\
  output_function_profiler ((FILE), (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter. No definition is equivalent to
   always zero.

   On the RS/6000, this is non-zero because we can restore the stack from
   its backpointer, which we maintain.  */
#define EXIT_IGNORE_STACK	1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) output_epilog (FILE, SIZE)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.

   On the RS/6000, this is not code at all, but merely a data area,
   since that is the way all functions are called.  The first word is
   the address of the function, the second word is the TOC pointer (r2),
   and the third word is the static chain value.  */

#define TRAMPOLINE_TEMPLATE(FILE) { fprintf (FILE, "\t.long 0, 0, 0\n"); }

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE    12

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)		\
{								\
  emit_move_insn (gen_rtx (MEM, SImode,				\
			   memory_address (SImode, (ADDR))),	\
		  gen_rtx (MEM, SImode,				\
			   memory_address (SImode, (FNADDR))));	\
  emit_move_insn (gen_rtx (MEM, SImode,				\
			   memory_address (SImode,		\
					   plus_constant ((ADDR), 4))), \
		  gen_rtx (MEM, SImode,				\
			   memory_address (SImode,		\
					   plus_constant ((FNADDR), 4)))); \
  emit_move_insn (gen_rtx (MEM, SImode,				\
			   memory_address (SImode,		\
					   plus_constant ((ADDR), 8))), \
		  force_reg (SImode, (CXT)));			\
}

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the RS/6000.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.

   In addition, we use the elimination mechanism to see if r30 is needed
   Initially we assume that it isn't.  If it is, we spill it.  This is done
   by making it an eliminable register.  We replace it with itself so that
   if it isn't needed, then existing uses won't be modified.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { 30, 30} }

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the RS/6000, if frame pointer elimination is being done, we would like
   to convert ap into fp, not sp.

   We need r30 if -mmininal-toc was specified, and there are constant pool
   references.  */

#define CAN_ELIMINATE(FROM, TO)					\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM	\
  ? ! frame_pointer_needed					\
  : (FROM) == 30 ? ! TARGET_MINIMAL_TOC || get_pool_size () == 0 \
  : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  int total_stack_size = (rs6000_sa_size () + get_frame_size ()		\
			  + current_function_outgoing_args_size);	\
									\
  total_stack_size = (total_stack_size + 7) & ~7;			\
									\
 if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
    {									\
      if (rs6000_pushes_stack ())					\
	(OFFSET) = 0;							\
      else								\
	(OFFSET) = - total_stack_size;					\
    }									\
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM) \
      (OFFSET) = total_stack_size;					\
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
    {									\
      if (rs6000_pushes_stack ())					\
	(OFFSET) = total_stack_size;					\
      else								\
	(OFFSET) = 0;							\
    }									\
  else if ((FROM) == 30)						\
    (OFFSET) = 0;							\
  else									\
    abort ();								\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

#define HAVE_PRE_DECREMENT
#define HAVE_PRE_INCREMENT

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? (REGNO) <= 31 || (REGNO) == 67				\
 : (reg_renumber[REGNO] >= 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67)))

#define REGNO_OK_FOR_BASE_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? ((REGNO) > 0 && (REGNO) <= 31) || (REGNO) == 67		\
 : (reg_renumber[REGNO] > 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the RS/6000, all integer constants are acceptable, most won't be valid
   for particular insns, though.  Only easy FP constants are
   acceptable.  */

#define LEGITIMATE_CONSTANT_P(X)				\
  (GET_CODE (X) != CONST_DOUBLE || GET_MODE (X) == VOIDmode	\
   || easy_fp_constant (X, GET_MODE (X)))

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
#define REG_OK_FOR_INDEX_P(X)			\
  (REGNO (X) <= 31 || REGNO (X) == 67 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)					 \
  (REGNO (X) > 0 && REG_OK_FOR_INDEX_P (X))

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

   On the RS/6000, there are four valid address: a SYMBOL_REF that
   refers to a constant pool entry of an address (or the sum of it
   plus a constant), a short (16-bit signed) constant plus a register,
   the sum of two registers, or a register indirect, possibly with an
   auto-increment.  For DFmode and DImode with an constant plus register,
   we must ensure that both words are addressable.  */

#define LEGITIMATE_CONSTANT_POOL_BASE_P(X)				\
  (GET_CODE (X) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (X)		\
   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (X)))

#define LEGITIMATE_CONSTANT_POOL_ADDRESS_P(X)				\
  (LEGITIMATE_CONSTANT_POOL_BASE_P (X)					\
   || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS		\
       && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT			\
       && LEGITIMATE_CONSTANT_POOL_BASE_P (XEXP (XEXP (X, 0), 0))))

#define LEGITIMATE_ADDRESS_INTEGER_P(X,OFFSET)				\
 (GET_CODE (X) == CONST_INT						\
  && (unsigned) (INTVAL (X) + (OFFSET) + 0x8000) < 0x10000)

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE,X)		\
 (GET_CODE (X) == PLUS					\
  && GET_CODE (XEXP (X, 0)) == REG			\
  && REG_OK_FOR_BASE_P (XEXP (X, 0))			\
  && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 0)	\
  && (((MODE) != DFmode && (MODE) != DImode)		\
      || LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 4)))

#define LEGITIMATE_INDEXED_ADDRESS_P(X)		\
 (GET_CODE (X) == PLUS				\
  && GET_CODE (XEXP (X, 0)) == REG		\
  && GET_CODE (XEXP (X, 1)) == REG		\
  && ((REG_OK_FOR_BASE_P (XEXP (X, 0))		\
       && REG_OK_FOR_INDEX_P (XEXP (X, 1)))	\
      || (REG_OK_FOR_BASE_P (XEXP (X, 1))	\
	  && REG_OK_FOR_INDEX_P (XEXP (X, 0)))))

#define LEGITIMATE_INDIRECT_ADDRESS_P(X)	\
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (LEGITIMATE_INDIRECT_ADDRESS_P (X))		\
    goto ADDR;						\
  if (GET_CODE (X) == PRE_INC				\
      && LEGITIMATE_INDIRECT_ADDRESS_P (XEXP (X, 0)))	\
    goto ADDR;						\
  if (GET_CODE (X) == PRE_DEC				\
      && LEGITIMATE_INDIRECT_ADDRESS_P (XEXP (X, 0)))	\
    goto ADDR;						\
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (X))		\
    goto ADDR;						\
  if (LEGITIMATE_OFFSET_ADDRESS_P (MODE, X))		\
    goto ADDR;						\
  if ((MODE) != DImode && (MODE) != TImode		\
      && LEGITIMATE_INDEXED_ADDRESS_P (X))		\
    goto ADDR;						\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   On RS/6000, first check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the 
   possibility of bit 16 being a one.

   Then check for the sum of a register and something not constant, try to
   load the other things into a register and return the sum.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{ if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG	\
    && GET_CODE (XEXP (X, 1)) == CONST_INT			\
    && (unsigned) (INTVAL (XEXP (X, 1)) + 0x8000) >= 0x10000)	\
    { int high_int, low_int;					\
      high_int = INTVAL (XEXP (X, 1)) >> 16;			\
      low_int = INTVAL (XEXP (X, 1)) & 0xffff;			\
      if (low_int & 0x8000)					\
	high_int += 1, low_int |= 0xffff0000;			\
      (X) = gen_rtx (PLUS, SImode,				\
		     force_operand				\
		     	(gen_rtx (PLUS, SImode, XEXP (X, 0), \
				  gen_rtx (CONST_INT, VOIDmode, \
						      high_int << 16)), 0),\
		     gen_rtx (CONST_INT, VOIDmode, low_int));	\
      goto WIN;							\
    }								\
  else if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG \
	   && GET_CODE (XEXP (X, 1)) != CONST_INT		\
	   && (MODE) != DImode && (MODE) != TImode) 		\
    {								\
      (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		     force_reg (SImode, force_operand (XEXP (X, 1), 0))); \
      goto WIN;							\
    }								\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the RS/6000 this is true if the address is valid with a zero offset
   but not with an offset of four (this means it cannot be used as an
   address for DImode or DFmode) or is a pre-increment or decrement.  Since
   we know it is valid, we just check for an address that is not valid with
   an offset of four.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)		\
{ if (GET_CODE (ADDR) == PLUS					\
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (ADDR, 1), 0)	\
      && ! LEGITIMATE_ADDRESS_INTEGER_P (XEXP (ADDR, 1), 4))	\
    goto LABEL;							\
  if (GET_CODE (ADDR) == PRE_INC)				\
    goto LABEL;							\
  if (GET_CODE (ADDR) == PRE_DEC)				\
    goto LABEL;							\
}

/* Define this if some processing needs to be done immediately before
   emitting code for an insn.  */

/* #define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS) */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */

/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Nonzero if access to memory by bytes is no faster than for words.
   Also non-zero if doing byte operations (specifically shifts) in registers
   is undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* The RS/6000 uses the XCOFF format.  */

#define XCOFF_DEBUGGING_INFO

/* Define if the object format being used is COFF or a superset.  */
#define OBJECT_FORMAT_COFF

/* Define the magic numbers that we recognize as COFF.  */

#define MY_ISCOFF(magic) \
  ((magic) == U802WRMAGIC || (magic) == U802ROMAGIC || (magic) == U802TOCMAGIC)

/* This is the only version of nm that collect2 can work with.  */
#define REAL_NM_FILE_NAME "/usr/ucb/nm"

/* We don't have GAS for the RS/6000 yet, so don't write out special
   .stabs in cc1plus.  */
   
#define FASCIST_ASSEMBLER

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* Mode of a function address in a call instruction (for indexing purposes).

   Doesn't matter on RS/6000.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.

   The sle and sre instructions which allow SHIFT_COUNT_TRUNCATED
   have been dropped from the PowerPC architecture.  */

#define SHIFT_COUNT_TRUNCATED TARGET_POWER ? 1 : 0

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   On the RS/6000, if it is legal in the insn, it is free.  So this
   always returns 0.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
  case CONST_DOUBLE:						\
    return 0;

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MULT:						\
  switch (rs6000_cpu)					\
    {							\
    case PROCESSOR_RIOS1:				\
      return (GET_CODE (XEXP (X, 1)) != CONST_INT	\
	      ? COSTS_N_INSNS (5)			\
	      : INTVAL (XEXP (X, 1)) >= -256 && INTVAL (XEXP (X, 1)) <= 255 \
	      ? COSTS_N_INSNS (3) : COSTS_N_INSNS (4));	\
    case PROCESSOR_RIOS2:				\
      return COSTS_N_INSNS (2);				\
    case PROCESSOR_PPC601:				\
    case PROCESSOR_PPC603:				\
      return COSTS_N_INSNS (5);				\
    case PROCESSOR_PPC604:				\
    case PROCESSOR_PPC620:				\
      return COSTS_N_INSNS (4);				\
    }							\
  case DIV:						\
  case MOD:						\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& exact_log2 (INTVAL (XEXP (X, 1))) >= 0)	\
      return COSTS_N_INSNS (2);				\
    /* otherwise fall through to normal divide.  */	\
  case UDIV:						\
  case UMOD:						\
  switch (rs6000_cpu)					\
    {							\
    case PROCESSOR_RIOS1:				\
      return COSTS_N_INSNS (19);			\
    case PROCESSOR_RIOS2:				\
      return COSTS_N_INSNS (13);			\
    case PROCESSOR_PPC601:				\
      return COSTS_N_INSNS (36);			\
    case PROCESSOR_PPC603:				\
      return COSTS_N_INSNS (37);			\
    case PROCESSOR_PPC604:				\
    case PROCESSOR_PPC620:				\
      return COSTS_N_INSNS (20);			\
    }							\
  case MEM:						\
    /* MEM should be slightly more expensive than (plus (reg) (const)) */ \
    return 5;

/* Compute the cost of an address.  This is meant to approximate the size
   and/or execution delay of an insn using that address.  If the cost is
   approximated by the RTL complexity, including CONST_COSTS above, as
   is usually the case for CISC machines, this macro should not be defined.
   For aggressively RISCy machines, only one insn format is allowed, so
   this macro should be a constant.  The value of this macro only matters
   for valid addresses.

   For the RS/6000, everything is cost 0.  */

#define ADDRESS_COST(RTX) 0

/* Adjust the length of an INSN.  LENGTH is the currently-computed length and
   should be adjusted to reflect any required changes.  This macro is used when
   there is some systematic length adjustment required that would be difficult
   to express in the length attribute.  */

/* #define ADJUST_INSN_LENGTH(X,LENGTH) */

/* Add any extra modes needed to represent the condition code.

   For the RS/6000, we need separate modes when unsigned (logical) comparisons
   are being done and we need a separate mode for floating-point.  We also
   use a mode for the case when we are comparing the results of two
   comparisons.  */

#define EXTRA_CC_MODES CCUNSmode, CCFPmode, CCEQmode

/* Define the names for the modes specified above.  */
#define EXTRA_CC_NAMES "CCUNS", "CCFP", "CCEQ"

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CCUNSmode should be used for unsigned comparisons.
   CCEQmode should be used when we are doing an inequality comparison on
   the result of a comparison. CCmode should be used in all other cases.  */

#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? CCFPmode	\
   : (OP) == GTU || (OP) == LTU || (OP) == GEU || (OP) == LEU ? CCUNSmode \
   : (((OP) == EQ || (OP) == NE) && GET_RTX_CLASS (GET_CODE (X)) == '<'   \
      ? CCEQmode : CCmode))

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *rs6000_compare_op0, *rs6000_compare_op1;
extern int rs6000_compare_fp_p;

/* Set to non-zero by "fix" operation to indicate that itrunc and
   uitrunc must be defined.  */

extern int rs6000_trunc_used;

/* Function names to call to do floating point truncation.  */

#define RS6000_ITRUNC "itrunc"
#define RS6000_UITRUNC "uitrunc"

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.

   Initialize the section names for the RS/6000 at this point.

   Specify filename to assembler.

   We want to go into the TOC section so at least one .toc will be emitted.
   Also, in order to output proper .bs/.es pairs, we need at least one static
   [RW] section emitted.

   We then switch back to text to force the gcc2_compiled. label and the space
   allocated after it (when profiling) into the text section.  

   Finally, declare mcount when profiling to make the assembler happy.  */

#define ASM_FILE_START(FILE)					\
{								\
  rs6000_gen_section_name (&xcoff_bss_section_name,		\
			   main_input_filename, ".bss_");	\
  rs6000_gen_section_name (&xcoff_private_data_section_name,	\
			   main_input_filename, ".rw_");	\
  rs6000_gen_section_name (&xcoff_read_only_section_name,	\
			   main_input_filename, ".ro_");	\
								\
  output_file_directive (FILE, main_input_filename);		\
  toc_section ();						\
  if (write_symbols != NO_DEBUG)				\
    private_data_section ();					\
  text_section ();						\
  if (profile_flag)						\
    fprintf (FILE, "\t.extern .mcount\n");			\
}

/* Output at end of assembler file.

   On the RS/6000, referencing data should automatically pull in text.  */

#define ASM_FILE_END(FILE)					\
{								\
  text_section ();						\
  fprintf (FILE, "_section_.text:\n");				\
  data_section ();						\
  fprintf (FILE, "\t.long _section_.text\n");			\
}

/* We define this to prevent the name mangler from putting dollar signs into
   function names.  */

#define NO_DOLLAR_IN_LABEL

/* We define this to 0 so that gcc will never accept a dollar sign in a
   variable name.  This is needed because the AIX assembler will not accept
   dollar signs.  */

#define DOLLARS_IN_IDENTIFIERS 0

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Define the extra sections we need.  We define three: one is the read-only
   data section which is used for constants.  This is a csect whose name is
   derived from the name of the input file.  The second is for initialized
   global variables.  This is a csect whose name is that of the variable.
   The third is the TOC.  */

#define EXTRA_SECTIONS \
   read_only_data, private_data, read_only_private_data, toc, bss

/* Define the name of our readonly data section.  */

#define READONLY_DATA_SECTION read_only_data_section

/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  */

#define ENCODE_SECTION_INFO(DECL)  \
  if (TREE_CODE (DECL) == FUNCTION_DECL			\
      && (TREE_ASM_WRITTEN (DECL) || ! TREE_PUBLIC (DECL))) \
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;

/* Indicate that jump tables go in the text section.  */

#define JUMP_TABLES_IN_TEXT_SECTION

/* Define the routines to implement these extra sections.  */

#define EXTRA_SECTION_FUNCTIONS				\
							\
void							\
read_only_data_section ()				\
{							\
  if (in_section != read_only_data)			\
    {							\
      fprintf (asm_out_file, ".csect %s[RO]\n",		\
	       xcoff_read_only_section_name);		\
      in_section = read_only_data;			\
    }							\
}							\
							\
void							\
private_data_section ()					\
{							\
  if (in_section != private_data)			\
    {							\
      fprintf (asm_out_file, ".csect %s[RW]\n",		\
	       xcoff_private_data_section_name);	\
							\
      in_section = private_data;			\
    }							\
}							\
							\
void							\
read_only_private_data_section ()			\
{							\
  if (in_section != read_only_private_data)		\
    {							\
      fprintf (asm_out_file, ".csect %s[RO]\n",		\
	       xcoff_private_data_section_name);	\
      in_section = read_only_private_data;		\
    }							\
}							\
							\
void							\
toc_section ()						\
{							\
  if (TARGET_MINIMAL_TOC)				\
    {							\
      static int toc_initialized = 0;			\
							\
      /* toc_section is always called at least once from ASM_FILE_START, \
	 so this is guaranteed to always be defined once and only once   \
	 in each file.  */						 \
      if (! toc_initialized)				\
	{						\
	  fprintf (asm_out_file, ".toc\nLCTOC..0:\n");	\
	  fprintf (asm_out_file, "\t.tc toc_table[TC],toc_table[RW]\n"); \
	  toc_initialized = 1;				\
	}						\
							\
      if (in_section != toc)				\
	fprintf (asm_out_file, ".csect toc_table[RW]\n"); \
    }							\
  else							\
    {							\
      if (in_section != toc)				\
        fprintf (asm_out_file, ".toc\n");		\
    }							\
  in_section = toc;					\
}

/* This macro produces the initial definition of a function name.
   On the RS/6000, we need to place an extra '.' in the function name and
   output the function descriptor.  

   The csect for the function will have already been created by the
   `text_section' call previously done.  We do have to go back to that
   csect, however.  */

/* ??? What do the 16 and 044 in the .function line really mean?  */

#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)		\
{ if (TREE_PUBLIC (DECL))					\
    {								\
      fprintf (FILE, "\t.globl .");				\
      RS6000_OUTPUT_BASENAME (FILE, NAME);			\
      fprintf (FILE, "\n");					\
    }								\
  else								\
    {								\
      fprintf (FILE, "\t.lglobl .");				\
      RS6000_OUTPUT_BASENAME (FILE, NAME);			\
      fprintf (FILE, "\n");					\
    }								\
  fprintf (FILE, ".csect ");					\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fprintf (FILE, "[DS]\n");					\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fprintf (FILE, ":\n");					\
  fprintf (FILE, "\t.long .");					\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fprintf (FILE, ", TOC[tc0], 0\n");				\
  fprintf (FILE, ".csect .text[PR]\n.");				\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fprintf (FILE, ":\n");					\
  if (write_symbols == XCOFF_DEBUG)				\
    xcoffout_declare_function (FILE, DECL, NAME);		\
}

/* Return non-zero if this entry is to be written into the constant pool
   in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF or a CONST
   containing one of them.  If -mfp-in-toc (the default), we also do
   this for floating-point constants.  We actually can only do this
   if the FP formats of the target and host machines are the same, but
   we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X)			\
  (GET_CODE (X) == SYMBOL_REF					\
   || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
       && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)	\
   || GET_CODE (X) == LABEL_REF					\
   || (! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)		\
       && GET_CODE (X) == CONST_DOUBLE				\
       && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
       && BITS_PER_WORD == HOST_BITS_PER_INT))

/* Select section for constant in constant pool.

   On RS/6000, all constants are in the private read-only data area.
   However, if this is being placed in the TOC it must be output as a
   toc entry.  */

#define SELECT_RTX_SECTION(MODE, X)		\
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    toc_section ();				\
  else						\
    read_only_private_data_section ();		\
}

/* Macro to output a special constant pool entry.  Go to WIN if we output
   it.  Otherwise, it is written the usual way.

   On the RS/6000, toc entries are handled this way.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, WIN)  \
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    {						\
      output_toc (FILE, X, LABELNO);		\
      goto WIN;					\
    }						\
}

/* Select the section for an initialized data object.

   On the RS/6000, we have a special section for all variables except those
   that are static.  */

#define SELECT_SECTION(EXP,RELOC)			\
{							\
  if ((TREE_CODE (EXP) == STRING_CST			\
       && !flag_writable_strings)			\
      || (TREE_READONLY (EXP) && ! TREE_THIS_VOLATILE (EXP) \
	  && DECL_INITIAL (EXP)				\
	  && (DECL_INITIAL (EXP) == error_mark_node	\
	      || TREE_CONSTANT (DECL_INITIAL (EXP)))	\
	  && ! (RELOC)))				\
    {							\
      if (TREE_PUBLIC (EXP))				\
        read_only_data_section ();			\
      else						\
        read_only_private_data_section ();		\
    }							\
  else							\
    {							\
      if (TREE_PUBLIC (EXP))				\
        data_section ();				\
      else						\
        private_data_section ();			\
    }							\
}

/* This outputs NAME to FILE up to the first null or '['.  */

#define RS6000_OUTPUT_BASENAME(FILE, NAME)	\
  if ((NAME)[0] == '*' || (NAME)[strlen (NAME) - 1] != ']') \
    assemble_name (FILE, NAME);  		\
  else						\
    {						\
      int _len = strlen (NAME);			\
      char *_p = alloca (_len + 1);		\
						\
      strcpy (_p, NAME);			\
      _p[_len - 4] = '\0';			\
      assemble_name (FILE, _p);			\
    }

/* Output something to declare an external symbol to the assembler.  Most
   assemblers don't need this.  

   If we haven't already, add "[RW]" (or "[DS]" for a function) to the
   name.  Normally we write this out along with the name.  In the few cases
   where we can't, it gets stripped off.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
{ rtx _symref = XEXP (DECL_RTL (DECL), 0);	\
  if ((TREE_CODE (DECL) == VAR_DECL		\
       || TREE_CODE (DECL) == FUNCTION_DECL)	\
      && (NAME)[0] != '*'			\
      && (NAME)[strlen (NAME) - 1] != ']')	\
    {						\
      char *_name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5); \
      strcpy (_name, XSTR (_symref, 0));	\
      strcat (_name, TREE_CODE (DECL) == FUNCTION_DECL ? "[DS]" : "[RW]"); \
      XSTR (_symref, 0) = _name;		\
    }						\
  fprintf (FILE, "\t.extern ");			\
  assemble_name (FILE, XSTR (_symref, 0));	\
  if (TREE_CODE (DECL) == FUNCTION_DECL)	\
    {						\
      fprintf (FILE, "\n\t.extern .");		\
      RS6000_OUTPUT_BASENAME (FILE, XSTR (_symref, 0));	\
    }						\
  fprintf (FILE, "\n");				\
}

/* Similar, but for libcall.  We only have to worry about the function name,
   not that of the descriptor. */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
{ fprintf (FILE, "\t.extern .");		\
  assemble_name (FILE, XSTR (FUN, 0));		\
  fprintf (FILE, "\n");				\
}

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before instructions.  */

#define TEXT_SECTION_ASM_OP ".csect .text[PR]"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".csect .data[RW]"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
 {"0", "1", "2", "3", "4", "5", "6", "7", 		\
  "8", "9", "10", "11", "12", "13", "14", "15",		\
  "16", "17", "18", "19", "20", "21", "22", "23",	\
  "24", "25", "26", "27", "28", "29", "30", "31",	\
  "0", "1", "2", "3", "4", "5", "6", "7",		\
  "8", "9", "10", "11", "12", "13", "14", "15",		\
  "16", "17", "18", "19", "20", "21", "22", "23",	\
  "24", "25", "26", "27", "28", "29", "30", "31",	\
  "mq", "lr", "ctr", "ap",				\
  "0", "1", "2", "3", "4", "5", "6", "7" }

/* Table of additional register names to use in user input.  */

#define ADDITIONAL_REGISTER_NAMES \
 {"r0",    0, "r1",    1, "r2",    2, "r3",    3,	\
  "r4",    4, "r5",    5, "r6",    6, "r7",    7,	\
  "r8",    8, "r9",    9, "r10",  10, "r11",  11,	\
  "r12",  12, "r13",  13, "r14",  14, "r15",  15,	\
  "r16",  16, "r17",  17, "r18",  18, "r19",  19,	\
  "r20",  20, "r21",  21, "r22",  22, "r23",  23,	\
  "r24",  24, "r25",  25, "r26",  26, "r27",  27,	\
  "r28",  28, "r29",  29, "r30",  30, "r31",  31,	\
  "fr0",  32, "fr1",  33, "fr2",  34, "fr3",  35,	\
  "fr4",  36, "fr5",  37, "fr6",  38, "fr7",  39,	\
  "fr8",  40, "fr9",  41, "fr10", 42, "fr11", 43,	\
  "fr12", 44, "fr13", 45, "fr14", 46, "fr15", 47,	\
  "fr16", 48, "fr17", 49, "fr18", 50, "fr19", 51,	\
  "fr20", 52, "fr21", 53, "fr22", 54, "fr23", 55,	\
  "fr24", 56, "fr25", 57, "fr26", 58, "fr27", 59,	\
  "fr28", 60, "fr29", 61, "fr30", 62, "fr31", 63,	\
  /* no additional names for: mq, lr, ctr, ap */	\
  "cr0",  68, "cr1",  69, "cr2",  70, "cr3",  71,	\
  "cr4",  72, "cr5",  73, "cr6",  74, "cr7",  75,	\
  "cc",   68 }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Text to write out after a CALL that may be replaced by glue code by
   the loader.  This depends on the AIX version.  */
#define RS6000_CALL_GLUE "cror 31,31,31"

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { RS6000_OUTPUT_BASENAME (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE);	\
       RS6000_OUTPUT_BASENAME (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s..%d:\n", PREFIX, NUM)

/* This is how to output an internal label prefix.  rs6000.c uses this
   when generating traceback tables.  */

#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, "%s..", PREFIX)

/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed. */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "%s..%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t[2];							\
	REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n",		\
		t[0] & 0xffffffff, t[1] & 0xffffffff);			\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", str);			\
	fprintf (FILE, "\t.double 0d%s\n", str);			\
      }									\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t;								\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);		\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);			\
	fprintf (FILE, "\t.float 0d%s\n", str);				\
      }									\
  }

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
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

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an assembler line to define N characters starting
   at P to FILE.  */

#define ASM_OUTPUT_ASCII(FILE, P, N)  output_ascii ((FILE), (P), (N))

/* This is how to output code to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  asm_fprintf (FILE, "\{tstu|stwu} %s,-4(r1)\n", reg_names[REGNO]);

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  asm_fprintf (FILE, "\t{l|lwz} %s,0(r1)\n\t{ai|addic} r1,r1,4\n",  \
    reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute. 
   (RS/6000 does not use such vectors, but we must define this macro
   anyway.)   */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)		\
  do { char buf[100];					\
       fprintf (FILE, "\t.long ");			\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", VALUE);	\
       assemble_name (FILE, buf);			\
       fprintf (FILE, "\n");				\
     } while (0)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)	\
  do { char buf[100];					\
       fprintf (FILE, "\t.long ");			\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", VALUE);	\
       assemble_name (FILE, buf);			\
       fprintf (FILE, "-");				\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", REL);	\
       assemble_name (FILE, buf);			\
       fprintf (FILE, "\n");				\
     } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs (".comm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ",%d\n", (SIZE)); } while (0)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE,ROUNDED)	\
  do { fputs (".lcomm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ",%d,%s\n", (SIZE), xcoff_bss_section_name); \
     } while (0)

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
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Define which CODE values are valid.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)  ((CODE) == '.' || (CODE) == '*')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Define the codes that are matched by predicates in rs6000.c.  */

#define PREDICATE_CODES \
  {"short_cint_operand", {CONST_INT}},				\
  {"u_short_cint_operand", {CONST_INT}},			\
  {"non_short_cint_operand", {CONST_INT}},			\
  {"gpc_reg_operand", {SUBREG, REG}},				\
  {"cc_reg_operand", {SUBREG, REG}},				\
  {"reg_or_short_operand", {SUBREG, REG, CONST_INT}},		\
  {"reg_or_neg_short_operand", {SUBREG, REG, CONST_INT}},	\
  {"reg_or_u_short_operand", {SUBREG, REG, CONST_INT}},		\
  {"reg_or_cint_operand", {SUBREG, REG, CONST_INT}},		\
  {"easy_fp_constant", {CONST_DOUBLE}},				\
  {"reg_or_mem_operand", {SUBREG, MEM, REG}},			\
  {"fp_reg_or_mem_operand", {SUBREG, MEM, REG}},		\
  {"mem_or_easy_const_operand", {SUBREG, MEM, CONST_DOUBLE}},	\
  {"add_operand", {SUBREG, REG, CONST_INT}},			\
  {"non_add_cint_operand", {CONST_INT}},			\
  {"and_operand", {SUBREG, REG, CONST_INT}},			\
  {"non_and_cint_operand", {CONST_INT}},			\
  {"logical_operand", {SUBREG, REG, CONST_INT}},		\
  {"non_logical_cint_operand", {CONST_INT}},			\
  {"mask_operand", {CONST_INT}},				\
  {"call_operand", {SYMBOL_REF, REG}},				\
  {"current_file_function_operand", {SYMBOL_REF}},		\
  {"input_operand", {SUBREG, MEM, REG, CONST_INT, SYMBOL_REF}},	\
  {"load_multiple_operation", {PARALLEL}},			\
  {"store_multiple_operation", {PARALLEL}},			\
  {"branch_comparison_operator", {EQ, NE, LE, LT, GE,		\
				  GT, LEU, LTU, GEU, GTU}},	\
  {"scc_comparison_operator", {EQ, NE, LE, LT, GE,		\
			       GT, LEU, LTU, GEU, GTU}},
