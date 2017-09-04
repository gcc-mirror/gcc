/* Target Definitions for R8C/M16C/M32C
   Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

#ifndef GCC_M32C_H
#define GCC_M32C_H

/* Controlling the Compilation Driver, `gcc'.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

/* There are four CPU series we support, but they basically break down
   into two families - the R8C/M16C families, with 16-bit address
   registers and one set of opcodes, and the M32CM/M32C group, with
   24-bit address registers and a different set of opcodes.  The
   assembler doesn't care except for which opcode set is needed; the
   big difference is in the memory maps, which we cover in
   LIB_SPEC.  */

#undef  ASM_SPEC
#define ASM_SPEC "\
%{mcpu=r8c:--m16c} \
%{mcpu=m16c:--m16c} \
%{mcpu=m32cm:--m32c} \
%{mcpu=m32c:--m32c} "

/* The default is R8C hardware.  We support a simulator, which has its
   own libgloss and link map, plus one default link map for each chip
   family.  Most of the logic here is making sure we do the right
   thing when no CPU is specified, which defaults to R8C.  */
#undef  LIB_SPEC
#define LIB_SPEC "-( -lc %{msim:-lsim}%{!msim:-lnosys} -) \
%{msim:%{!T*: %{mcpu=m32cm:%Tsim24.ld}%{mcpu=m32c:%Tsim24.ld} \
       %{!mcpu=m32cm:%{!mcpu=m32c:%Tsim16.ld}}}} \
%{!T*:%{!msim: %{mcpu=m16c:%Tm16c.ld} \
	       %{mcpu=m32cm:%Tm32cm.ld} \
	       %{mcpu=m32c:%Tm32c.ld} \
	       %{!mcpu=m16c:%{!mcpu=m32cm:%{!mcpu=m32c:%Tr8c.ld}}}}} \
"

/* Run-time Target Specification */

/* Nothing unusual here.  */
#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_assert ("cpu=m32c"); \
    builtin_assert ("machine=m32c"); \
    builtin_define ("__m32c__=1"); \
    if (TARGET_R8C) \
      builtin_define ("__r8c_cpu__=1"); \
    if (TARGET_M16C) \
      builtin_define ("__m16c_cpu__=1"); \
    if (TARGET_M32CM) \
      builtin_define ("__m32cm_cpu__=1"); \
    if (TARGET_M32C) \
      builtin_define ("__m32c_cpu__=1"); \
  }

/* The pragma handlers need to know if we've started processing
   functions yet, as the memregs pragma should only be given at the
   beginning of the file.  This variable starts off TRUE and later
   becomes FALSE.  */
extern int ok_to_change_target_memregs;

/* TARGET_CPU is a multi-way option set in m32c.opt.  While we could
   use enums or defines for this, this and m32c.opt are the only
   places that know (or care) what values are being used.  */
#define TARGET_R8C	(target_cpu == 'r')
#define TARGET_M16C	(target_cpu == '6')
#define TARGET_M32CM	(target_cpu == 'm')
#define TARGET_M32C	(target_cpu == '3')

/* Address register sizes.  Warning: these are used all over the place
   to select between the two CPU families in general.  */
#define TARGET_A16	(TARGET_R8C || TARGET_M16C)
#define TARGET_A24	(TARGET_M32CM || TARGET_M32C)

/* Defining data structures for per-function information */

typedef struct GTY (()) machine_function
{
  /* How much we adjust the stack when returning from an exception
     handler.  */
  rtx eh_stack_adjust;

  /* TRUE if the current function is an interrupt handler.  */
  int is_interrupt;

  /* TRUE if the current function is a leaf function.  Currently, this
     only affects saving $a0 in interrupt functions.  */
  int is_leaf;

  /* Bitmask that keeps track of which registers are used in an
     interrupt function, so we know which ones need to be saved and
     restored.  */
  int intr_pushm;
  /* Likewise, one element for each memreg that needs to be saved.  */
  char intr_pushmem[16];

  /* TRUE if the current function can use a simple RTS to return, instead
     of the longer ENTER/EXIT pair.  */
  int use_rts;
}
machine_function;

#define INIT_EXPANDERS m32c_init_expanders ()

/* Storage Layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* We can do QI, HI, and SI operations pretty much equally well, but
   GCC expects us to have a "native" format, so we pick the one that
   matches "int".  Pointers are 16 bits for R8C/M16C (when TARGET_A16
   is true) and 24 bits for M32CM/M32C (when TARGET_A24 is true), but
   24-bit pointers are stored in 32-bit words.  */
#define UNITS_PER_WORD 2
#define POINTER_SIZE (TARGET_A16 ? 16 : 32)
#define POINTERS_EXTEND_UNSIGNED 1
/* We have a problem with libgcc2.  It only defines two versions of
   each function, one for "int" and one for "long long".  Ie it assumes
   that "sizeof (int) == sizeof (long)".  For the M32C this is not true
   and we need a third set of functions.  We explicitly define
   LIBGCC2_UNITS_PER_WORD here so that it is clear that we are expecting
   to get the SI and DI versions from the libgcc2.c sources, and we
   provide our own set of HI functions in m32c-lib2.c, which is why this
   definition is surrounded by #ifndef..#endif.  */
#ifndef LIBGCC2_UNITS_PER_WORD
#define LIBGCC2_UNITS_PER_WORD 4
#endif

/* These match the alignment enforced by the two types of stack operations.  */
#define PARM_BOUNDARY (TARGET_A16 ? 8 : 16)
#define STACK_BOUNDARY (TARGET_A16 ? 8 : 16)

/* We do this because we care more about space than about speed.  For
   the chips with 16-bit busses, we could set these to 16 if
   desired.  */
#define FUNCTION_BOUNDARY 8
#define BIGGEST_ALIGNMENT 8

/* Since we have a maximum structure alignment of 8 there
   is no need to enforce any alignment of bitfield types.  */
#undef  PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS 0

#define STRICT_ALIGNMENT 0
#define SLOW_BYTE_ACCESS 1

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 16
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_A16 ? "int" : "long int")

#undef UINTPTR_TYPE
#define UINTPTR_TYPE (TARGET_A16 ? "unsigned int" : "long unsigned int")

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* REGISTER USAGE */

/* Register Basics */

/* Register layout:

        [r0h][r0l]  $r0  (16 bits, or two 8-bit halves)
        [--------]  $r2  (16 bits)
        [r1h][r1l]  $r1  (16 bits, or two 8-bit halves)
        [--------]  $r3  (16 bits)
   [---][--------]  $a0  (might be 24 bits)
   [---][--------]  $a1  (might be 24 bits)
   [---][--------]  $sb  (might be 24 bits)
   [---][--------]  $fb  (might be 24 bits)
   [---][--------]  $sp  (might be 24 bits)
   [-------------]  $pc  (20 or 24 bits)
             [---]  $flg (CPU flags)
   [---][--------]  $argp (virtual)
        [--------]  $mem0 (all 16 bits)
          . . .
        [--------]  $mem14
*/

#define FIRST_PSEUDO_REGISTER   20

/* Note that these two tables are modified based on which CPU family
   you select; see m32c_conditional_register_usage for details.  */

/* r0 r2 r1 r3 - a0 a1 sb fb - sp pc flg argp - mem0..mem14 */
#define FIXED_REGISTERS     { 0, 0, 0, 0, \
			      0, 0, 1, 0, \
			      1, 1, 0, 1, \
			      0, 0, 0, 0, 0, 0, 0, 0 }
#define CALL_USED_REGISTERS { 1, 1, 1, 1, \
			      1, 1, 1, 0, \
			      1, 1, 1, 1, \
			      1, 1, 1, 1, 1, 1, 1, 1 }

/* The *_REGNO theme matches m32c.md and most register number
   arguments; the PC_REGNUM is the odd one out.  */
#ifndef PC_REGNO
#define PC_REGNO 9
#endif
#define PC_REGNUM PC_REGNO

/* Order of Allocation of Registers */

#define REG_ALLOC_ORDER { \
	0, 1, 2, 3, 4, 5, /* r0..r3, a0, a1 */ \
        12, 13, 14, 15, 16, 17, 18, 19, /* mem0..mem7 */	\
	6, 7, 8, 9, 10, 11 /* sb, fb, sp, pc, flg, ap */ }

/* How Values Fit in Registers */

#define HARD_REGNO_NREGS(R,M) m32c_hard_regno_nregs (R, M)
#define MODES_TIEABLE_P(M1,M2) m32c_modes_tieable_p (M1, M2)
#define AVOID_CCMODE_COPIES

/* Register Classes */

/* Most registers are special purpose in some form or another, so this
   table is pretty big.  Class names are used for constraints also;
   for example the HL_REGS class (HL below) is "Rhl" in the md files.
   See m32c_reg_class_from_constraint for the mapping.  There's some
   duplication so that we can better isolate the reason for using
   constraints in the md files from the actual registers used; for
   example we may want to exclude a1a0 from SI_REGS in the future,
   without precluding their use as HImode registers.  */

/* m7654 - m3210 - argp flg pc sp - fb sb a1 a0 - r3 r1 r2 r0 */
/*       mmPAR */
#define REG_CLASS_CONTENTS \
{ { 0x00000000 }, /* NO */\
  { 0x00000100 }, /* SP  - sp */\
  { 0x00000080 }, /* FB  - fb */\
  { 0x00000040 }, /* SB  - sb */\
  { 0x000001c0 }, /* CR  - sb fb sp */\
  { 0x00000001 }, /* R0  - r0 */\
  { 0x00000004 }, /* R1  - r1 */\
  { 0x00000002 }, /* R2  - r2 */\
  { 0x00000008 }, /* R3  - r3 */\
  { 0x00000003 }, /* R02 - r0r2 */\
  { 0x0000000c }, /* R13 - r1r3 */\
  { 0x00000005 }, /* HL  - r0 r1 */\
  { 0x0000000a }, /* R23 - r2 r3 */\
  { 0x0000000f }, /* R03 - r0r2 r1r3 */\
  { 0x00000010 }, /* A0  - a0 */\
  { 0x00000020 }, /* A1  - a1 */\
  { 0x00000030 }, /* A   - a0 a1 */\
  { 0x000000f0 }, /* AD  - a0 a1 sb fp */\
  { 0x000001f0 }, /* PS  - a0 a1 sb fp sp */\
  { 0x00000033 }, /* R02A  - r0r2 a0 a1 */ \
  { 0x0000003f }, /* RA  - r0 r1 r2 r3 a0 a1 */\
  { 0x0000007f }, /* GENERAL */\
  { 0x00000400 }, /* FLG */\
  { 0x000001ff }, /* HC  - r0l r1 r2 r3 a0 a1 sb fb sp */\
  { 0x000ff000 }, /* MEM */\
  { 0x000ff003 }, /* R02_A_MEM */\
  { 0x000ff005 }, /* A_HL_MEM */\
  { 0x000ff00c }, /* R1_R3_A_MEM */\
  { 0x000ff00f }, /* R03_MEM */\
  { 0x000ff03f }, /* A_HI_MEM */\
  { 0x000ff0ff }, /* A_AD_CR_MEM_SI */\
  { 0x000ff5ff }, /* ALL */\
}

#define QI_REGS HL_REGS
#define HI_REGS RA_REGS
#define SI_REGS R03_REGS
#define DI_REGS R03_REGS

enum reg_class
{
  NO_REGS,
  SP_REGS,
  FB_REGS,
  SB_REGS,
  CR_REGS,
  R0_REGS,
  R1_REGS,
  R2_REGS,
  R3_REGS,
  R02_REGS,
  R13_REGS,
  HL_REGS,
  R23_REGS,
  R03_REGS,
  A0_REGS,
  A1_REGS,
  A_REGS,
  AD_REGS,
  PS_REGS,
  R02A_REGS,
  RA_REGS,
  GENERAL_REGS,
  FLG_REGS,
  HC_REGS,
  MEM_REGS,
  R02_A_MEM_REGS,
  A_HL_MEM_REGS,
  R1_R3_A_MEM_REGS,
  R03_MEM_REGS,
  A_HI_MEM_REGS,
  A_AD_CR_MEM_SI_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
"NO_REGS", \
"SP_REGS", \
"FB_REGS", \
"SB_REGS", \
"CR_REGS", \
"R0_REGS", \
"R1_REGS", \
"R2_REGS", \
"R3_REGS", \
"R02_REGS", \
"R13_REGS", \
"HL_REGS", \
"R23_REGS", \
"R03_REGS", \
"A0_REGS", \
"A1_REGS", \
"A_REGS", \
"AD_REGS", \
"PS_REGS", \
"R02A_REGS", \
"RA_REGS", \
"GENERAL_REGS", \
"FLG_REGS", \
"HC_REGS", \
"MEM_REGS", \
"R02_A_MEM_REGS", \
"A_HL_MEM_REGS", \
"R1_R3_A_MEM_REGS", \
"R03_MEM_REGS", \
"A_HI_MEM_REGS", \
"A_AD_CR_MEM_SI_REGS", \
"ALL_REGS", \
}

#define REGNO_REG_CLASS(R) m32c_regno_reg_class (R)

/* We support simple displacements off address registers, nothing else.  */
#define BASE_REG_CLASS A_REGS
#define INDEX_REG_CLASS NO_REGS

/* We primarily use the new "long" constraint names, with the initial
   letter classifying the constraint type and following letters
   specifying which.  The types are:

   I - integer values
   R - register classes
   S - memory references (M was used)
   A - addresses (currently unused)
*/

#define REGNO_OK_FOR_BASE_P(NUM) m32c_regno_ok_for_base_p (NUM)
#define REGNO_OK_FOR_INDEX_P(NUM) 0

#define LIMIT_RELOAD_CLASS(MODE,CLASS) \
  (enum reg_class) m32c_limit_reload_class (MODE, CLASS)

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,X) \
  (enum reg_class) m32c_secondary_reload_class (CLASS, MODE, X)

#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true

#define CANNOT_CHANGE_MODE_CLASS(F,T,C) m32c_cannot_change_mode_class(F,T,C)

/* STACK AND CALLING */

/* Frame Layout */

/* Standard push/pop stack, no surprises here.  */

#define STACK_GROWS_DOWNWARD 1
#define STACK_PUSH_CODE PRE_DEC
#define FRAME_GROWS_DOWNWARD 1

#define STARTING_FRAME_OFFSET 0
#define FIRST_PARM_OFFSET(F) 0

#define RETURN_ADDR_RTX(COUNT,FA) m32c_return_addr_rtx (COUNT)

#define INCOMING_RETURN_ADDR_RTX m32c_incoming_return_addr_rtx()
#define INCOMING_FRAME_SP_OFFSET (TARGET_A24 ? 4 : 3)

/* Exception Handling Support */

#define EH_RETURN_DATA_REGNO(N) m32c_eh_return_data_regno (N)
#define EH_RETURN_STACKADJ_RTX m32c_eh_return_stackadj_rtx ()

/* Registers That Address the Stack Frame */

#ifndef FP_REGNO
#define FP_REGNO 7
#endif
#ifndef SP_REGNO
#define SP_REGNO 8
#endif
#define AP_REGNO 11

#define STACK_POINTER_REGNUM	SP_REGNO
#define FRAME_POINTER_REGNUM	FP_REGNO
#define ARG_POINTER_REGNUM	AP_REGNO

/* The static chain must be pointer-capable.  */
#define STATIC_CHAIN_REGNUM A0_REGNO

#define DWARF_FRAME_REGISTERS 20
#define DWARF_FRAME_REGNUM(N) m32c_dwarf_frame_regnum (N)
#define DBX_REGISTER_NUMBER(N) m32c_dwarf_frame_regnum (N)

#undef ASM_PREFERRED_EH_DATA_FORMAT
/* This is the same as the default in practice, except that by making
   it explicit we tell binutils what size pointers to use.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  (TARGET_A16 ? DW_EH_PE_udata2 : DW_EH_PE_udata4)

/* Eliminating Frame Pointer and Arg Pointer */

#define ELIMINABLE_REGS \
  {{AP_REGNO, SP_REGNO}, \
   {AP_REGNO, FB_REGNO}, \
   {FB_REGNO, SP_REGNO}}

#define INITIAL_ELIMINATION_OFFSET(FROM,TO,VAR) \
	(VAR) = m32c_initial_elimination_offset(FROM,TO)

/* Passing Function Arguments on the Stack */

#define PUSH_ARGS 1
#define PUSH_ROUNDING(N) m32c_push_rounding (N)
#define CALL_POPS_ARGS(C) 0

/* Passing Arguments in Registers */

typedef struct m32c_cumulative_args
{
  /* For address of return value buffer (structures are returned by
     passing the address of a buffer as an invisible first argument.
     This identifies it).  If set, the current parameter will be put
     on the stack, regardless of type.  */
  int force_mem;
  /* First parm is 1, parm 0 is hidden pointer for returning
     aggregates.  */
  int parm_num;
} m32c_cumulative_args;

#define CUMULATIVE_ARGS m32c_cumulative_args
#define INIT_CUMULATIVE_ARGS(CA,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
	m32c_init_cumulative_args (&(CA),FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS)
#define FUNCTION_ARG_REGNO_P(r) m32c_function_arg_regno_p (r)

/* How Large Values Are Returned */

#define DEFAULT_PCC_STRUCT_RETURN 1

/* Function Entry and Exit */

#define EXIT_IGNORE_STACK 0
#define EPILOGUE_USES(REGNO) m32c_epilogue_uses(REGNO)
#define EH_USES(REGNO) 0	/* FIXME */

/* Generating Code for Profiling */

#define FUNCTION_PROFILER(FILE,LABELNO)

/* Implementing the Varargs Macros */

/* Trampolines for Nested Functions */

#define TRAMPOLINE_SIZE m32c_trampoline_size ()
#define TRAMPOLINE_ALIGNMENT m32c_trampoline_alignment ()

/* Addressing Modes */

#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1
#define MAX_REGS_PER_ADDRESS 1

/* This is passed to the macros below, so that they can be implemented
   in m32c.c.  */
#ifdef REG_OK_STRICT
#define REG_OK_STRICT_V 1
#else
#define REG_OK_STRICT_V 0
#endif

#define REG_OK_FOR_BASE_P(X) m32c_reg_ok_for_base_p (X, REG_OK_STRICT_V)
#define REG_OK_FOR_INDEX_P(X) 0

/* #define FIND_BASE_TERM(X) when we do unspecs for symrefs */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN) \
	if (m32c_legitimize_reload_address(&(X),MODE,OPNUM,TYPE,IND_LEVELS)) \
	  goto WIN;

/* Address spaces.  */
#define ADDR_SPACE_FAR	1


/* Condition Code Status */

#define REVERSIBLE_CC_MODE(MODE) 1

/* Dividing the Output into Sections (Texts, Data, ...) */

#define TEXT_SECTION_ASM_OP ".text"
#define DATA_SECTION_ASM_OP ".data"
#define BSS_SECTION_ASM_OP ".bss"

#define CTOR_LIST_BEGIN
#define CTOR_LIST_END
#define DTOR_LIST_BEGIN
#define DTOR_LIST_END
#define CTORS_SECTION_ASM_OP "\t.section\t.init_array,\"aw\",%init_array"
#define DTORS_SECTION_ASM_OP "\t.section\t.fini_array,\"aw\",%fini_array"
#define INIT_ARRAY_SECTION_ASM_OP "\t.section\t.init_array,\"aw\",%init_array"
#define FINI_ARRAY_SECTION_ASM_OP "\t.section\t.fini_array,\"aw\",%fini_array"

/* The Overall Framework of an Assembler File */

#define ASM_COMMENT_START ";"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Output of Assembler Instructions */

#define REGISTER_NAMES {	\
  "r0", "r2", "r1", "r3", \
  "a0", "a1", "sb", "fb", "sp", \
  "pc", "flg", "argp", \
  "mem0",  "mem2",  "mem4",  "mem6",  "mem8",  "mem10",  "mem12",  "mem14", \
}

#define ADDITIONAL_REGISTER_NAMES { \
  {"r0l", 0}, \
  {"r1l", 2}, \
  {"r0r2", 0}, \
  {"r1r3", 2}, \
  {"a0a1", 4}, \
  {"r0r2r1r3", 0} }

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#define ASM_OUTPUT_REG_PUSH(S,R) m32c_output_reg_push (S, R)
#define ASM_OUTPUT_REG_POP(S,R) m32c_output_reg_pop (S, R)

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
	m32c_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 1)

#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
	m32c_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 0)


/* Output of Dispatch Tables */

#define ASM_OUTPUT_ADDR_VEC_ELT(S,V) \
	fprintf (S, "\t.word L%d\n", V)

/* Assembler Commands for Exception Regions */

#define DWARF_CIE_DATA_ALIGNMENT -1

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
	fprintf (STREAM, "\t.p2align\t%d\n", POWER);

/* Controlling Debugging Information Format */

#define DWARF2_ADDR_SIZE	4

/* Miscellaneous Parameters */

#define HAS_LONG_COND_BRANCH false
#define HAS_LONG_UNCOND_BRANCH true
#define CASE_VECTOR_MODE SImode
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND

#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

#define STORE_FLAG_VALUE 1

/* 16- or 24-bit pointers */
#define Pmode (TARGET_A16 ? HImode : PSImode)
#define FUNCTION_MODE QImode

#define REGISTER_TARGET_PRAGMAS() m32c_register_pragmas()

#endif
