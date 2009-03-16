/* Definitions of target machine for GNU compiler for picoChip
   Copyright (C) 2001, 2008 Free Software Foundation, Inc.

   Contributed by picoChip Designs Ltd. (http://www.picochip.com)
   Maintained by Daniel Towner (daniel.towner@picochip.com) and
   Hariharan Sandanagobalane (hariharan@picochip.com).

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
along with GCC; see the file COPYING3.  If not, see
<http://www.gnu.org/licenses/>. */

/* Which type of DFA scheduling to use - schedule for speed (VLIW), or
   schedule for space.  When scheduling for space, attempt to schedule
   into stall cycles, but don't pack instructions. */

enum picochip_dfa_type
{
  DFA_TYPE_NONE,
  DFA_TYPE_SPACE,
  DFA_TYPE_SPEED
};

extern enum picochip_dfa_type picochip_schedule_type;

/* Controlling the Compilation Driver */

/* Pass through the save-temps command option. */
#define LINK_SPEC " %{save-temps:--save-temps}"

/* This is an embedded processor, and only supports a cut-down version of
 * the standard C library. */
#define LIB_SPEC "-lpicoC"

/* The start file is automatically provided by the linker. */
#define STARTFILE_SPEC ""

/* Run-time Target Specification  */

/* Define some additional pre-processor macros. */
#define TARGET_CPU_CPP_BUILTINS()                       \
  do                                                    \
    {                                                   \
      builtin_define ("NO_TRAMPOLINES");                \
      builtin_define ("PICOCHIP");                      \
      builtin_define ("__PICOCHIP__");                      \
    }                                                   \
  while (0)

/* Translate requests for particular AEs into their respective ISA
   options. Note that byte access is enabled by default. */
#define TARGET_OPTION_TRANSLATE_TABLE			      \
  { "-mae=ANY",   "-mmul-type=none -mno-byte-access" },	      \
  { "-mae=ANY2",  "-mmul-type=none -mno-byte-access" },	      \
  { "-mae=ANY3",  "-mmul-type=none" },			      \
  { "-mae=STAN",  "-mmul-type=none -mno-byte-access" },	      \
  { "-mae=STAN2", "-mmul-type=mac -mno-byte-access" },	      \
  { "-mae=STAN3", "-mmul-type=mac " },			      \
  { "-mae=MAC",   "-mmul-type=mac -mno-byte-access" },	      \
  { "-mae=MUL",   "-mmul-type=mul" },			      \
  { "-mae=MEM",   "-mmul-type=mul" },			      \
  { "-mae=MEM2",  "-mmul-type=mul" },			      \
  { "-mae=CTRL",  "-mmul-type=mul" },			      \
  { "-mae=CTRL2", "-mmul-type=mul" }

/* Specify the default options, so that the multilib build doesn't
   need to provide special cases for the defaults. */
#define MULTILIB_DEFAULTS \
  { "mmul-type=mul", "mbyte-access"}

#define TARGET_HAS_BYTE_ACCESS (picochip_has_byte_access)
#define TARGET_HAS_MUL_UNIT (picochip_has_mul_unit)
#define TARGET_HAS_MAC_UNIT (picochip_has_mac_unit)
#define TARGET_HAS_MULTIPLY (picochip_has_mac_unit || picochip_has_mul_unit)

/* Allow some options to be overriden.  In particular, the 2nd
   scheduling pass option is switched off, and a machine dependent
   reorganisation ensures that it is run later on, after the second
   jump optimisation. */
#define OVERRIDE_OPTIONS picochip_override_options()

#define CAN_DEBUG_WITHOUT_FP 1

#define TARGET_VERSION fprintf(stderr, "(picoChip)");

/* Storage Layout */

/* picoChip processors are 16-bit machines, little endian. */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#define BITS_PER_UNIT 8

#define BITS_PER_WORD 16
#define UNITS_PER_WORD (BITS_PER_WORD / BITS_PER_UNIT)

#define POINTER_SIZE BITS_PER_WORD

/* Promote those modes that are smaller than an int, to int mode.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  ((GET_MODE_CLASS (MODE) == MODE_INT			\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)		\
      ? (MODE) = HImode : 0)

/* All parameters are at least this aligned.  Parameters are passed
   one-per-register. */
#define PARM_BOUNDARY BITS_PER_WORD

/* The main stack pointer is guaranteed to be aligned to the most
   strict data alignment. */
#define STACK_BOUNDARY 32

/* Function entry point is byte aligned. */
#define FUNCTION_BOUNDARY 8

/* This is the biggest alignment that can be allowed on this machine.
   Since the STANs have only 256 byte memory, it doesnt make sense
   to have alignments greater than 32 bytes. Hence the value */
#define MAX_OFILE_ALIGNMENT 32*8

/* The strictest data object alignment, which repesents a register pair. */
#define BIGGEST_ALIGNMENT 32

/* The hardware doesn't allow unaligned memory access.  */
#define STRICT_ALIGNMENT 1

/* We want the 'unix' style bitfield packing algorithm.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Support up to 64-bit integers. */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (DImode)

/* We don't support floating point, but give it a sensible definition. */
#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* Layout of Source Language Data Types.  */

#define INT_TYPE_SIZE BITS_PER_WORD

/* The normal sizes for C scalar data. */
#define CHAR_TYPE_SIZE 8
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

/* We don't support the following data types, but still give them
   sensible values.  */
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 32
#define LONG_DOUBLE_TYPE_SIZE 32

/* Plain `char' is a signed type, since the hardware sign-extends
   bytes when loading them from memory into a register. */
#define DEFAULT_SIGNED_CHAR 1

/* Note that the names of the types used in the following macros must
   be precisely the same as those defined internally in gcc.  For
   example, `unsigned short' wouldn't work as a type string, since gcc
   doesn't define any type with this exact string.  The correct string
   to use is `short unsigned int'. */

#define SIZE_TYPE "unsigned int"

#define PTRDIFF_TYPE "int"

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

#define WINT_TYPE "unsigned int"

/* Register Usage  */

/* Picochip has 16 16-bit registers, a condition code register and an
   (inaccessible) instruction pointer.  One of these registers (r15) is
   special, and is either used to load a constant anywhere a register
   can normally be used, or is used to specify a dummy destination
   (e.g., when setting condition flags).  We also define some pseudo
   registers to represent condition codes, the frame pointer and the
   argument pointer.  The latter two are eliminated wherever possible.

   Pairs of general registers may be combined to form 32-bit registers.

   The picoChip registers are as follows:

   0..1 - function return value
   0..5 - first 6 function parameters
   6..11 - General purpose
   12 - link register
   13 - stack pointer
   14 - specialized pointer
   15 - long constant or /dev/null
   (16) acc0
   (17) pseudo condition code
   (18) pseudo frame pointer
   (19) pseudo arg pointer

   Registers 0..6, 12, 13, 14, 15 are caller save
   Registers 0..12, 14 are available to the register allocator.

   In addition, the DSP variant of the ISA allows extra accumulator
   registers to be accessed.  These are special purpose registers,
   which are not currently used by the compiler.

  */

/* Basic Characteristics of Registers  */

/* We have 16 hard registers plus 3 pseudo hard registers and an accumulator.  */
#define FIRST_PSEUDO_REGISTER 20

/* The first non-hard register.  Only used internally by the picoChip port. */
#define FIRST_NONHARD_REGISTER 18

/* Cannot use SP, CST, CC, FP, AP */
#define FIXED_REGISTERS {0,0,0,0,0,0,0,0, 0,0,0,0,0,1,0,1, 1,1,1,1}

/* Those that are clobbered by a function call (includes pseudo-regs) */
#define CALL_USED_REGISTERS {1,1,1,1,1,1,0,0, 0,0,0,0,1,1,0,1, 1,1,1,1}
#define CALL_REALLY_USED_REGISTERS {1,1,1,1,1,1,0,0, 0,0,0,0,1,1,0,0, 0,1,0,0}

/* Define the number of the picoChip link and condition psuedo registers. */
#define LINK_REGNUM 12
#define CC_REGNUM 17
#define ACC_REGNUM 16

/* Order of Allocation of Registers  */

/* The registers are allocated starting with the caller-clobbered
   registers, in reverse order.  The registers are then listed in an
   order which means that they are efficiently saved in pairs (i.e.,
   one 32-bit store can be used instead of two 16-bit stores to save
   the registers into the stack). The exception to this is the use of
   r14 (AP) register, which also appears early on.  This is because the
   AP register can be used to encode memory operations more
   efficiently than other registers.  Some code can be made more
   compact as a result. */
   /* My current feeling is that r14 should go to the end and maybe even r12.
   It seems like the overhead of store/load that will occur since we cant
   pair anything up with r14 will be higher than the advantage of smaller
   encoding.
   Also r12 is put towards the end for leaf functions. Since leaf functions
   do not have any calls, the prologue/epilogue for them wouldnt save up/
   restore its value. So, it doesnt make sense for us to use it in the middle,
   if we can avoid it. */
#define REG_ALLOC_ORDER {5,4,3,2,1,0,12,6,7,8,9,10,11,14,16,0,0,0,0,0}
#define LEAF_REG_ALLOC_ORDER {5,4,3,2,1,0,6,7,8,9,10,11,14,12,16,0,0,0,0,0}

/* We can dynamically change the REG_ALLOC_ORDER using the following hook.
   It would be desirable to change it for leaf functions so we can put
   r12 at the end of this list.*/
#define ORDER_REGS_FOR_LOCAL_ALLOC picochip_order_regs_for_local_alloc ()

/* How Values Fit in Registers  */

/* Number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE) picochip_regno_nregs((REGNO), (MODE))

/* Is it ok to place MODE in REGNO?  Require that the register number
   be aligned. */
#define HARD_REGNO_MODE_OK(REGNO, MODE)	picochip_hard_regno_mode_ok(REGNO, MODE)

#define MODES_TIEABLE_P(MODE1,MODE2) 1

/* Don't copy the cc register ('cos you can't put it back).  */
#define AVOID_CCMODE_COPIES 1

/* Register Classes */

enum reg_class
{
  NO_REGS,			/* no registers in set */
  FRAME_REGS,			/* registers with a long offset  */
  PTR_REGS,			/* registers without an offset  */
  CONST_REGS,			/* registers for long constants  */
  NULL_REGS,			/* registers which ignore writes  */
  CC_REGS,			/* condition code registers  */
  ACC_REGS,			/* Accumulator registers  */
  TWIN_REGS,			/* registers which can be paired */
  GR_REGS,			/* general purpose registers */
  ALL_REGS,			/* all registers */
  LIM_REG_CLASSES,		/* max value + 1 */

  /* Some aliases  */
  GENERAL_REGS = GR_REGS
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* The following macro defines cover classes for Integrated Register
   Allocator.  Cover classes is a set of non-intersected register
   classes covering all hard registers used for register allocation
   purpose.  Any move between two registers of a cover class should be
   cheaper than load or store of the registers.  The macro value is
   array of register classes with LIM_REG_CLASSES used as the end
   marker.  */

#define IRA_COVER_CLASSES 						\
{									\
  GR_REGS, LIM_REG_CLASSES						\
}


/* The names of the register classes  */
#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "FRAME_REGS",								\
  "PTR_REGS",								\
  "CONST_REGS",								\
  "NULL_REGS", 								\
  "CC_REGS",								\
  "ACC_REGS",								\
  "TWIN_REGS",								\
  "GR_REGS",								\
  "ALL_REGS"								\
}

/* Each reg class is an array of 32-bit integers.  Each array must be
   long enough to store one bit for every pseudo register. Thus in the
   following code, each array only stores one 32-bit value. */
#define REG_CLASS_CONTENTS						\
{									\
  {0x00000000}, /* no registers */					\
  {0x00002000},	/* frame */						\
  {0x00004000},	/* pointer  */						\
  {0x00008000}, /* const */						\
  {0x00008000},	/* null  */						\
  {0x00020000}, /* cc */						\
  {0x00010000}, /* acc0 */						\
  {0x00000FFF},	/* twin */						\
  {0x000CFFFF},	/* general registers - includes pseudo-arg */    	\
  {0x000FFFFF}	/* all registers - includes pseudo-arg */               \
}

/* The earliest register class containing the given register.  */
extern const enum reg_class picochip_regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) picochip_regno_reg_class[REGNO]

/* Any register can be a base pointer.  */
#define BASE_REG_CLASS GR_REGS

/* Any register can be an index.  */
#define INDEX_REG_CLASS GR_REGS

#define REGNO_OK_FOR_BASE_P(REGNO) 					\
  (REGNO_REG_CLASS (REGNO) != CC_REGS && REGNO_REG_CLASS (REGNO) != ACC_REGS)

#define REGNO_OK_FOR_INDEX_P(REGNO) 0

#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

#define CLASS_MAX_NREGS(CLASS, MODE) picochip_class_max_nregs(CLASS, MODE)


/* Stack Layout and Calling Conventions  */

#define STACK_GROWS_DOWNWARD 1

/* The frame pointer points to the outgoing argument area, so the
   locals are above that.  */
#define STARTING_FRAME_OFFSET 0

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Specify where the return address lives before entry to the
   prologue.  This is required to enable DWARF debug information to be
   generated. */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, LINK_REGNUM)

#define RETURN_ADDR_RTX(count,frameaddr) picochip_return_addr_rtx(count,frameaddr)

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (LINK_REGNUM)

/* Registers that Address the Stack Frame  */

#define STACK_POINTER_REGNUM 13
#define FRAME_POINTER_REGNUM 18
#define ARG_POINTER_REGNUM   19

/* Static chain is used to pass the local variables of the enclosing function.
   The static chain is passed in memory. The first long-word location
   beneath the stack pointer is used. In the presence of pretend
   arguments, which are written into that location, this mechanism
   complicates matters. */

/* Location seen by the caller. */
#define STATIC_CHAIN							\
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, -2 * UNITS_PER_WORD))

/* Location seen by the callee. */
#define STATIC_CHAIN_INCOMING						\
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, 0))

/* Eliminating Frame Pointer and Arg Pointer.  The frame and argument
   pointers are eliminated wherever possible, by replacing them with
   offsets from the stack pointer. */

/* We want to get rid of the frame pointer.  */
#define FRAME_POINTER_REQUIRED 0

#define ELIMINABLE_REGS 						\
  {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
   {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

#define CAN_ELIMINATE(FROM, TO) 1

#define INITIAL_ELIMINATION_OFFSET(FROM,TO,OFFSET) \
  OFFSET = initial_elimination_offset(FROM, TO);

#define ACCUMULATE_OUTGOING_ARGS 1

#define PUSH_ARGS 0

/* Functions don't pop their args.  */
#define RETURN_POPS_ARGS(FNDECL, FNTYPE, STACK) 0

/* Passing Arguments in Registers  */

/* Store the offset of the next argument. */
#define CUMULATIVE_ARGS unsigned

/* Decide how function arguments are handled. */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  picochip_function_arg (CUM, MODE, TYPE, NAMED)

/* Incoming arguments are always the same as normal arguments, except
   for a function which uses variadic arguments, in which case all
   arguments are effectively passed on the stack. */
#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  picochip_incoming_function_arg(CUM, MODE, TYPE, NAMED)

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT,N_NAMED_ARGS) \
  ((CUM) = 0)

#define FUNCTION_ARG_ADVANCE(CUM,MODE,TYPE,NAMED) \
  (CUM) = picochip_arg_advance (CUM, MODE, TYPE, NAMED)

/* Originally this used TYPE_ALIGN to determine the
   alignment.  Unfortunately, this fails in some cases, because the
   type is unknown (e.g., libcall's). Instead, use GET_MODE_ALIGNMENT
   since the mode is always present. */
#define FUNCTION_ARG_BOUNDARY(MODE,TYPE) \
  picochip_get_function_arg_boundary(MODE)

/* The first 6 registers can hold parameters.  */
#define FUNCTION_ARG_REGNO_P(REGNO) ((REGNO) < 6)

/* How Scalar Function Values are Returned
   Do we need this?? */
#define FUNCTION_VALUE(VALTYPE,FUNC) picochip_function_value(VALTYPE, FUNC, 0)

#define LIBCALL_VALUE(MODE) (gen_rtx_REG (MODE, 0))

/* Results are in register zero.  If an SImode register is returned,
   reg0 will suffice to mean R[0:1]. */
#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == 0)

/* Don't automatically pass struct's in memory - use the
 * RETURN_IN_MEMORY macro to determine when structs are returned in
 * memory, and when in registers. */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Function Entry and Exit  */

/* The epilogue doesn't clobber anything.  */
#define EPILOGUE_USES(REGNO) 0

/* Generating Code for Profiling.  No profiling implemented  */

#define FUNCTION_PROFILER(FILE,LABELNO)

/* Trampolines for Nested Functions  */

/* No trampolines.  */
#define TRAMPOLINE_SIZE 0
#define INITIALIZE_TRAMPOLINE(ADDR,FNADDR,CHAIN)

/* Addressing Modes  */

#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

#define MAX_REGS_PER_ADDRESS 1

#ifdef REG_OK_STRICT

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 			\
 if (picochip_legitimate_address_p (MODE, X, 1)) goto LABEL;

#else /* REG_OK_STRICT */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 			\
  if (picochip_legitimate_address_p (MODE, X, 0)) goto LABEL;

#endif /* !REG_OK_STRICT */

/* extern struct rtx_def *picochip_legitimize_address */
/* 	PARAMS ((struct rtx_def *, struct rtx_def *, int)); */
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN);

/* Legitimize reload address tries machine dependent means of
   reloading addresses.  There seems to be a strange error in gcc,
   which necessitates this macro.  Consider:

     set (reg A) (symbol_ref)
     set (reg B) (plus (reg A) (const_int))	
			
   A symbol_ref is a valid constant, so the symbol_ref is propagated
   into the second instruction to generate the instruction:

     set (reg B) (plus (symbol_ref) (const_int))

   This is an invalid address, and find_reloads_address correctly
   determines this.  However, that function doesn't generate a valid
   replacement for the now invalid address, and the invalid address is
   output into the assembly language.  To fix the problem without
   changing gcc itself, the following macro tests when such an invalid
   address has been computed, and wraps it up inside a constant rtx.  A
   constant rtx can be correctly reloaded by the function, and hence
   correct code is generated. */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	     \
if (picochip_symbol_offset(X)) { X = gen_rtx_CONST(MODE, X); }

/* There are no mode dependent addresses.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL) do {} while (0)

/* Nonzero if the constant rtx X is a legitimate general operand.  X
   satisfies CONSTANT_P.  */

#define LEGITIMATE_CONSTANT_P(X) 1


/* Condition Code Status  */

#define CC_STATUS_MDEP unsigned
#define CC_STATUS_MDEP_INIT (cc_status.mdep = 0)

/* Describing Relative Costs of Operations  */

/* Bytes are no faster than words.  */
#define SLOW_BYTE_ACCESS 1

/* The assembler is often able to optimise function call branches, so
   don't try to CSE them in the compiler. This was the thinking before.
   But now, we realise that the benefits from CSE would mostly outweigh
   the disadvantages. */
#define NO_FUNCTION_CSE


/* Dividing the Output into Sections  */

#define TEXT_SECTION_ASM_OP ".section .text\n"
#define DATA_SECTION_ASM_OP ".section .data\n"
#define BSS_SECTION_ASM_OP ".section .bss\n"
/* picoChip is Harvard (separate data/instruction memories), so
   read-only data must go into the data section. */
#define READONLY_DATA_SECTION_ASM_OP ".section .data\n"

/* Defining the Output Assembler Language  */

/* The Overall Framework of an Assembler File  */

#define ASM_FILE_COMMENT "// "

#define ASM_APP_ON "// High-level ASM start\n"
#define ASM_APP_OFF "// High-level ASM end\n"

#define ASM_OUTPUT_IDENT(STREAM,STRING) fprintf(STREAM, ".ident %s\n", STRING)

/* Output of Data  */

#define ASM_OUTPUT_ASCII(FILE, PTR, LEN) picochip_output_ascii(FILE, PTR, LEN);

/* Output of Uninitialized Variables  */
#define ASM_OUTPUT_ALIGNED_COMMON(FILE,NAME,SIZE,ALIGN) \
  picochip_output_aligned_common(FILE, NAME, SIZE, ALIGN)

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE,NAME,SIZE,ALIGN) \
  picochip_output_aligned_local(FILE, NAME, SIZE, ALIGN)

/* Output and Generation of Labels  */

#define ASM_OUTPUT_LABEL(STREAM,NAME) \
  do { picochip_output_label(STREAM, NAME); } while (0);

#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  { picochip_output_labelref(STREAM, NAME); }

/* Format must match that of picochip_output_label. */
#define ASM_GENERATE_INTERNAL_LABEL(STRING,PREFIX,NUM) \
 picochip_generate_internal_label(STRING,PREFIX,(long)NUM)

#define ASM_WEAKEN_LABEL(STREAM,NAME) picochip_weaken_label(STREAM,NAME);

/* Store in OUTPUT a string (made with alloca) containing an
   assembler-name for a local static variable named NAME.  LABELNO is
   an integer which is different for each call.  The assembler can't
   use periods to generate the name, so we use a ___ separator
   instead. */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 15),    \
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* Macros Controlling Initialization Routines  */

/* By defining this, the main function won't try to call `__main'. */
#define HAS_INIT_SECTION

/* Output of Assembler Instructions  */

#define REGISTER_NAMES							\
{"R0",  "R1",  "R2",  "R3",						\
 "R4",  "R5",  "R6",  "R7",						\
 "R8",  "R9",  "R10", "R11",						\
 "R12", "FP", "R14", "R15",						\
 "acc0", "pseudoCC", "pseudoFP", "pseudoAP"}

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "R0",	 0},							\
  { "R1",	 1},							\
  { "R2",	 2},							\
  { "R3",	 3},							\
  { "R4",	 4},							\
  { "R5",	 5},							\
  { "R6",	 6},							\
  { "R7",	 7},							\
  { "R8",	 8},							\
  { "R9",	 9},							\
  { "R10",	10},							\
  { "R11",	11},							\
  { "R12",	12},							\
  { "FP",	13},							\
  { "R14",	14},							\
  { "R15",	15},							\
  { "acc0",	16},							\
  { "sp",	12}, /* ABI stack pointer */				\
  { "ln",	13}, /* arch link register */				\
  { "ptr",	14}, /* arch constant pointer */			\
  { "rc",	15}, /* arch constant register */			\
  { "rz",	15}, /* arch zero */					\
}

/* Final prescan insn is called just before an instruction is
   output.  In our case, we use this to detect the VLIW slot to which
   the instruction has been assigned, preparatory to generating the
   VLIW output in ASM_OUTPUT_OPCODE. */
#define FINAL_PRESCAN_INSN(insn, operand, nop) \
  picochip_final_prescan_insn (insn, operand,nop)

#define ASM_OUTPUT_OPCODE(FILE,PTR) \
  { PTR = picochip_asm_output_opcode(FILE, PTR); }

#define PRINT_OPERAND(STREAM,X,CODE) \
  picochip_print_operand(STREAM, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(code) \
  (((code) == '|') || ((code) == '#') || ((code) == '>'))

#define PRINT_OPERAND_ADDRESS(STREAM,X) \
  picochip_print_operand_address(STREAM,X)

/* Output of Dispatch Tables  */

/* Initialise a data memory location to an absolute code label.  Used
   for building switch statement jump tables.  Note - the format of the
   label must match that of the function picochip_output_label. */
#define ASM_OUTPUT_ADDR_VEC_ELT(stream, value) \
  fprintf (stream, ".initWord _L%d\n", value);

/* Assembler Commands for Alignment  */

#define ASM_OUTPUT_SKIP(STREAM,BYTES) \
  fprintf(STREAM, ".skip %u\n", BYTES);
#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
  fprintf(STREAM, ".align %u\n", 1 << POWER);

/* The elaborator doesn't output zero bytes in the text section. */
#define ASM_NO_SKIP_IN_TEXT 1

/* Controlling Debugging Information Format  */

/* Macros Affecting All Debugging Formats  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_FRAME_INFO 1

/* Generate .file/.loc directives, so that the assembler generates the
   line table. */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* Miscellaneous Parameters  */

#define CASE_VECTOR_MODE HImode
#define WORD_REGISTER_OPERATIONS
#define LOAD_EXTEND_OP(MODE) ((MODE) == QImode ? SIGN_EXTEND : ZERO_EXTEND)
#define MOVE_MAX 4
#define SHIFT_COUNT_TRUNCATED 1
#define Pmode HImode
#define FUNCTION_MODE QImode
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC) 1

#define ASM_LONG ":TODO:.word\t"

/* Define builtins for selected special-purpose instructions. */
enum picochip_builtins
{
  PICOCHIP_BUILTIN_SBC,
  PICOCHIP_BUILTIN_PUT,
  PICOCHIP_BUILTIN_GET,
  PICOCHIP_BUILTIN_TESTPORT,
  PICOCHIP_BUILTIN_COPYSW,
  PICOCHIP_BUILTIN_ADDS,
  PICOCHIP_BUILTIN_SUBS,
  PICOCHIP_BUILTIN_BREV,
  PICOCHIP_BUILTIN_BYTESWAP,
  PICOCHIP_BUILTIN_GET_ARRAY,
  PICOCHIP_BUILTIN_PUT_ARRAY,
  PICOCHIP_BUILTIN_TESTPORT_ARRAY,
  PICOCHIP_BUILTIN_ASRI,
  PICOCHIP_BUILTIN_HALT
};

#define NO_DOT_IN_LABEL 1

/* The assembler does support LEB128, despite the auto-configure test
   not detecting this. */
#define HAVE_AS_LEB128 1

/* The End */
