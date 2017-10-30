/* Definitions of target machine for GNU compiler,
   for Motorola M*CORE Processor.
   Copyright (C) 1993-2017 Free Software Foundation, Inc.

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

#ifndef GCC_MCORE_H
#define GCC_MCORE_H

/* RBE: need to move these elsewhere.  */
#undef	LIKE_PPC_ABI 
#define	MCORE_STRUCT_ARGS
/* RBE: end of "move elsewhere".  */

/* Run-time Target Specification.  */
#define TARGET_MCORE

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

#define TARGET_CPU_CPP_BUILTINS()					  \
  do									  \
    {									  \
      builtin_define ("__mcore__");					  \
      builtin_define ("__MCORE__");					  \
      if (TARGET_LITTLE_END)						  \
        builtin_define ("__MCORELE__");					  \
      else								  \
        builtin_define ("__MCOREBE__");					  \
      if (TARGET_M340)							  \
        builtin_define ("__M340__");					  \
      else								  \
        builtin_define ("__M210__");					  \
    }									  \
  while (0)

#undef  CPP_SPEC
#define CPP_SPEC "%{m210:%{mlittle-endian:%ethe m210 does not have little endian support}}"

/* We don't have a -lg library, so don't put it in the list.  */
#undef	LIB_SPEC
#define LIB_SPEC "%{!shared: %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

#undef	ASM_SPEC
#define	ASM_SPEC "%{mbig-endian:-EB} %{m210:-cpu=210 -EB}"

#undef  LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} %{m210:-EB} -X"

#define TARGET_DEFAULT	\
  (MASK_HARDLIT		\
   | MASK_DIV		\
   | MASK_RELAX_IMM	\
   | MASK_M340		\
   | MASK_LITTLE_END)

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mlittle-endian", "m340" }
#endif

/* The ability to have 4 byte alignment is being suppressed for now.
   If this ability is reenabled, you must disable the definition below
   *and* edit t-mcore to enable multilibs for 4 byte alignment code.  */
#undef TARGET_8ALIGN
#define TARGET_8ALIGN 1

extern char * mcore_current_function_name;
 
/* The MCore ABI says that bitfields are unsigned by default.  */
#define CC1_SPEC "-funsigned-bitfields"

/* Target machine storage Layout.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  	\
  if (GET_MODE_CLASS (MODE) == MODE_INT         \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {						\
      (MODE) = SImode;				\
      (UNSIGNEDP) = 1;				\
    }

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (! TARGET_LITTLE_END)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (! TARGET_LITTLE_END)

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD	4

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY  	32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  (TARGET_8ALIGN ? 64 : 32)

/* Largest increment in UNITS we allow the stack to grow in a single operation.  */
#define STACK_UNITS_MAXSTEP  4096

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY  ((TARGET_OVERALIGN_FUNC) ? 32 : 16)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  (TARGET_8ALIGN ? 64 : 32)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS	1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))
     
/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Register allocation for our first guess 

	r0		stack pointer
	r1		scratch, target reg for xtrb?
	r2-r7		arguments.
	r8-r14		call saved
	r15		link register
	ap		arg pointer (doesn't really exist, always eliminated)
	c               c bit
	fp		frame pointer (doesn't really exist, always eliminated)
	x19		two control registers.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   MCore has 16 integer registers and 2 control registers + the arg
   pointer.  */

#define FIRST_PSEUDO_REGISTER 20

#define R1_REG  1	/* Where literals are forced.  */
#define LK_REG	15	/* Overloaded on general register.  */
#define AP_REG  16	/* Fake arg pointer register.  */
/* RBE: mcore.md depends on CC_REG being set to 17.  */
#define CC_REG	17	/* Can't name it C_REG.  */
#define FP_REG  18	/* Fake frame pointer register.  */

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */


#undef PC_REGNUM /* Define this if the program counter is overloaded on a register.  */
#define STACK_POINTER_REGNUM 0 /* Register to use for pushing function arguments.  */
#define FRAME_POINTER_REGNUM 8 /* When we need FP, use r8.  */

/* The assembler's names for the registers.  RFP need not always be used as
   the Real framepointer; it can also be used as a normal general register.
   Note that the name `fp' is horribly misleading since `fp' is in fact only
   the argument-and-return-context pointer.  */
#define REGISTER_NAMES  				\
{				                   	\
  "sp", "r1", "r2",  "r3",  "r4",  "r5",  "r6",  "r7", 	\
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",	\
  "apvirtual",  "c", "fpvirtual", "x19" \
}

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS  \
 /*  r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 ap  c  fp x19 */ \
   { 1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* RBE: r15 {link register} not available across calls,
   But we don't mark it that way here....  */
#define CALL_USED_REGISTERS \
 /*  r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 ap  c   fp x19 */ \
   { 1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1, 1}

/* The order in which register should be allocated.  */
#define REG_ALLOC_ORDER  \
 /* r7  r6  r5  r4  r3  r2  r15 r14 r13 r12 r11 r10  r9  r8  r1  r0  ap  c   fp x19*/ \
  {  7,  6,  5,  4,  3,  2,  15, 14, 13, 12, 11, 10,  9,  8,  1,  0, 16, 17, 18, 19}

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the MCore.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.  */

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	16

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM	1

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM},}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = mcore_initial_elimination_offset (FROM, TO)

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

/* The MCore has only general registers. There are
   also some special purpose registers: the T bit register, the
   procedure Link and the Count Registers.  */
enum reg_class
{
  NO_REGS,
  ONLYR1_REGS,
  LRW_REGS,
  GENERAL_REGS,
  C_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES


/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES  \
{			\
  "NO_REGS",		\
  "ONLYR1_REGS",	\
  "LRW_REGS",		\
  "GENERAL_REGS",	\
  "C_REGS",		\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

/* ??? STACK_POINTER_REGNUM should be excluded from LRW_REGS.  */
#define REG_CLASS_CONTENTS      	\
{					\
  {0x000000},  /* NO_REGS       */	\
  {0x000002},  /* ONLYR1_REGS   */	\
  {0x007FFE},  /* LRW_REGS      */	\
  {0x01FFFF},  /* GENERAL_REGS  */	\
  {0x020000},  /* C_REGS        */	\
  {0x0FFFFF}   /* ALL_REGS      */	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern const enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) ((REGNO) < FIRST_PSEUDO_REGISTER ? regno_reg_class[REGNO] : NO_REGS)

/* When this hook returns true for MODE, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers.  */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true
 
/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  NO_REGS
#define BASE_REG_CLASS	 GENERAL_REGS

/* Convenience wrappers around insn_const_int_ok_for_constraint.  */
#define CONST_OK_FOR_I(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_I)
#define CONST_OK_FOR_J(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_J)
#define CONST_OK_FOR_L(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_L)
#define CONST_OK_FOR_K(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_K)
#define CONST_OK_FOR_M(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_M)
#define CONST_OK_FOR_N(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_N)
#define CONST_OK_FOR_O(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_O)
#define CONST_OK_FOR_P(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_P)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) mcore_reload_class (X, CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X) \
  mcore_secondary_reload_class (CLASS, MODE, X)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS. 

   On MCore this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
     (ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Stack layout; function entry, exit and calling.  */

/* Define the number of register that can hold parameters.
   These two macros are used only in other macro definitions below.  */
#define NPARM_REGS 6
#define FIRST_PARM_REG 2
#define FIRST_RET_REG 2

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  mcore_function_value (VALTYPE, FUNC)

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, FIRST_RET_REG)

/* 1 if N is a possible register number for a function value.
   On the MCore, only r4 can return results.  */
#define FUNCTION_VALUE_REGNO_P(REGNO)  ((REGNO) == FIRST_RET_REG)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(REGNO)  \
  ((REGNO) >= FIRST_PARM_REG && (REGNO) < (NPARM_REGS + FIRST_PARM_REG))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On MCore, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */
#define CUMULATIVE_ARGS  int

#define ROUND_ADVANCE(SIZE)	\
  ((SIZE + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round a register number up to a proper boundary for an arg of mode 
   MODE. 
   
   We round to an even reg for things larger than a word.  */
#define ROUND_REG(X, MODE) 				\
  ((TARGET_8ALIGN 					\
   && GET_MODE_UNIT_SIZE ((MODE)) > UNITS_PER_WORD) 	\
   ? ((X) + ((X) & 1)) : (X))


/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On MCore, the offset always starts at 0: the first parm reg is always
   the same reg.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

/* Call the function profiler with a given profile label.  */
#define FUNCTION_PROFILER(STREAM,LABELNO)		\
{							\
  fprintf (STREAM, "	trap	1\n");			\
  fprintf (STREAM, "	.align	2\n");			\
  fprintf (STREAM, "	.long	LP%d\n", (LABELNO));	\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  12

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT  32

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) < AP_REG || (unsigned) reg_renumber[(REGNO)] < AP_REG)

#define REGNO_OK_FOR_INDEX_P(REGNO)   0

/* Maximum number of registers that can appear in a valid memory 
   address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) 	 (GET_CODE (X) == LABEL_REF)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  0

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS TARGET_SLOW_BYTES

/* Shift counts are truncated to 6-bits (0 to 63) instead of the expected
   5-bits, so we can not define SHIFT_COUNT_TRUNCATED to true for this
   target.  */
#define SHIFT_COUNT_TRUNCATED 0

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/* Why is this defined??? -- dac */
#define NO_FUNCTION_CSE 1

/* The machine modes of pointers and functions.  */
#define Pmode          SImode
#define FUNCTION_MODE  Pmode

/* Compute extra cost of moving data between one register class
   and another.  All register moves are cheap.  */
#define REGISTER_MOVE_COST(MODE, SRCCLASS, DSTCLASS) 2

#define WORD_REGISTER_OPERATIONS 1

/* Assembler output control.  */
#define ASM_COMMENT_START "\t//"

#define ASM_APP_ON	"// inline asm begin\n"
#define ASM_APP_OFF	"// inline asm end\n"

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

/* Switch into a generic section.  */
#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  mcore_asm_named_section

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (SImode, LK_REG)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubi\t %s,%d\n\tstw\t %s,(%s)\n",	\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT),		\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM])

/* Length in instructions of the code output by ASM_OUTPUT_REG_PUSH.  */
#define REG_PUSH_LENGTH 2

/* This is how to output an insn to pop a register from the stack.  */
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tldw\t %s,(%s)\n\taddi\t %s,%d\n",	\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT))

  
/* Output a reference to a label.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  \
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, \
	   (* targetm.strip_name_encoding) (NAME))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align\t%d\n", LOG)

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

#define MULTIPLE_SYMBOL_SPACES 1

#define SUPPORTS_ONE_ONLY 1

/* A pair of macros to output things for the callgraph data.
   VALUE means (to the tools that reads this info later):
  	0 a call from src to dst
  	1 the call is special (e.g. dst is "unknown" or "alloca")
  	2 the call is special (e.g., the src is a table instead of routine)
  
   Frame sizes are augmented with timestamps to help later tools 
   differentiate between static entities with same names in different
   files.  */
extern long mcore_current_compilation_timestamp;
#define	ASM_OUTPUT_CG_NODE(FILE,SRCNAME,VALUE)				\
  do									\
    {									\
      if (mcore_current_compilation_timestamp == 0)			\
        mcore_current_compilation_timestamp = time (0);			\
      fprintf ((FILE),"\t.equ\t__$frame$size$_%s_$_%08lx,%d\n",		\
             (SRCNAME), mcore_current_compilation_timestamp, (VALUE));	\
    }									\
  while (0)

#define	ASM_OUTPUT_CG_EDGE(FILE,SRCNAME,DSTNAME,VALUE)		\
  do								\
    {								\
      fprintf ((FILE),"\t.equ\t__$function$call$_%s_$_%s,%d\n",	\
             (SRCNAME), (DSTNAME), (VALUE));			\
    }								\
  while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.export\t"

/* The prefix to add to user-visible assembler symbols.  */
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*.%s%ld", PREFIX, (long) NUM)

/* Jump tables must be 32 bit aligned.  */
#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(STREAM,PREFIX,NUM,TABLE) \
  fprintf (STREAM, "\t.align 2\n.%s%d:\n", PREFIX, NUM);

/* Output a relative address. Not needed since jump tables are absolute
   but we must define it anyway.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  \
  fputs ("- - - ASM_OUTPUT_ADDR_DIFF_ELT called!\n", STREAM)

/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  \
    fprintf (STREAM, "\t.long\t.L%d\n", VALUE)

/* Output various types of constants.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.fill %d, 1\n", (int)(SIZE))

/* This says how to output an assembler line
   to define a global common symbol, with alignment information.  */
/* XXX - for now we ignore the alignment.  */     
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      if (mcore_dllexport_name_p (NAME))			\
	MCORE_EXPORT_NAME (FILE, NAME)				\
      if (! mcore_dllimport_name_p (NAME))			\
        {							\
          fputs ("\t.comm\t", FILE);				\
          assemble_name (FILE, NAME);				\
          fprintf (FILE, ",%lu\n", (unsigned long)(SIZE));	\
        }							\
    }								\
  while (0)

/* This says how to output an assembler line
   to define a local common symbol....  */
#undef  ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  (fputs ("\t.lcomm\t", FILE),				\
  assemble_name (FILE, NAME),				\
  fprintf (FILE, ",%d\n", (int)SIZE))

/* ... and how to define a local common symbol whose alignment
   we wish to specify.  ALIGN comes in as bits, we have to turn
   it into bytes.  */
#undef  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      fputs ("\t.bss\t", (FILE));					\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%d,%d\n", (int)(SIZE), (ALIGN) / BITS_PER_UNIT);\
    }									\
  while (0)

#endif /* ! GCC_MCORE_H */
