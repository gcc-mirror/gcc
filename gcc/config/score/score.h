/* score.h for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Sunnorth.

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

#include "score-conv.h"

#undef CC1_SPEC
#define CC1_SPEC                 "%{!mel:-meb} %{mel:-mel } \
%{!mscore*:-mscore7}    \
%{mscore7:-mscore7}     \
%{mscore7d:-mscore7d}   \
%{G*}"

#undef ASM_SPEC
#define ASM_SPEC                 "%{!mel:-EB} %{mel:-EL} \
%{!mscore*:-march=score7}         \
%{mscore7:-march=score7}          \
%{mscore7d:-march=score7}         \
%{march=score7:-march=score7}     \
%{march=score7d:-march=score7}    \
%{G*}"

#undef LINK_SPEC
#define LINK_SPEC                "%{!mel:-EB} %{mel:-EL} \
%{!mscore*:-mscore7_elf}          \
%{mscore7:-mscore7_elf}           \
%{mscore7d:-mscore7_elf}          \
%{march=score7:-mscore7_elf}      \
%{march=score7d:-mscore7_elf}     \
%{G*}"

/* Run-time Target Specification.  */
#define TARGET_CPU_CPP_BUILTINS()               \
  do {                                          \
    builtin_define ("SUNPLUS");                 \
    builtin_define ("__SCORE__");               \
    builtin_define ("__score__");               \
    if (TARGET_LITTLE_ENDIAN)                   \
      builtin_define ("__scorele__");           \
    else                                        \
      builtin_define ("__scorebe__");           \
    if (TARGET_SCORE7)                          \
      builtin_define ("__score7__");            \
    if (TARGET_SCORE7D)                         \
      builtin_define ("__score7d__");           \
  } while (0)

#define TARGET_DEFAULT         0

#define SCORE_GCC_VERSION      "1.6"

/* Target machine storage layout.  */
#define BITS_BIG_ENDIAN        0
#define BYTES_BIG_ENDIAN       (TARGET_LITTLE_ENDIAN == 0)
#define WORDS_BIG_ENDIAN       (TARGET_LITTLE_ENDIAN == 0)

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD                 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)     \
  if (GET_MODE_CLASS (MODE) == MODE_INT         \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    (MODE) = SImode;

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY                  BITS_PER_WORD
#define STACK_BOUNDARY                 BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY              BITS_PER_WORD

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT              LONG_DOUBLE_TYPE_SIZE

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)                                      \
  ((((ALIGN) < BITS_PER_WORD)                                            \
    && (TREE_CODE (TYPE) == ARRAY_TYPE                                   \
        || TREE_CODE (TYPE) == UNION_TYPE                                \
        || TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  EXP is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)                                  \
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)   \
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment for a local
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)                                    \
  ((TREE_CODE (TYPE) == ARRAY_TYPE                                      \
    && TYPE_MODE (TREE_TYPE (TYPE)) == QImode                           \
    && (ALIGN) < BITS_PER_WORD) ? BITS_PER_WORD : (ALIGN))

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY           32

/* All accesses must be aligned.  */
#define STRICT_ALIGNMENT               1

/* Score requires that structure alignment is affected by bitfields.  */
#define PCC_BITFIELD_TYPE_MATTERS      1

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE            LONG_DOUBLE_TYPE_SIZE

/* Layout of Data Type.  */
/* Set the sizes of the core types.  */
#define INT_TYPE_SIZE                   32
#define SHORT_TYPE_SIZE                 16
#define LONG_TYPE_SIZE                  32
#define LONG_LONG_TYPE_SIZE             64
#define CHAR_TYPE_SIZE                  8
#define FLOAT_TYPE_SIZE                 32
#define DOUBLE_TYPE_SIZE                64
#define LONG_DOUBLE_TYPE_SIZE           64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR             1

/* Default definitions for size_t and ptrdiff_t.  */
#define SIZE_TYPE                       "unsigned int"

#define UINTPTR_TYPE			"long unsigned int"

/* Register Usage

   S+core have:
   - 32 integer registers
   - 16 control registers (cond)
   - 16 special registers (ceh/cel/cnt/lcr/scr/arg/fp)
   - 32 coprocessors 1 registers
   - 32 coprocessors 2 registers
   - 32 coprocessors 3 registers.  */
#define FIRST_PSEUDO_REGISTER           160

/* By default, fix the kernel registers (r30 and r31), the global
   pointer (r28) and the stack pointer (r0).  This can change
   depending on the command-line options.

   Regarding coprocessor registers: without evidence to the contrary,
   it's best to assume that each coprocessor register has a unique
   use.  This can be overridden, in, e.g., TARGET_OPTION_OVERRIDE or
   TARGET_CONDITIONAL_REGISTER_USAGE should the assumption be inappropriate
   for a particular target.  */

/* Control Registers, use mfcr/mtcr insn
    32        cr0         PSR
    33        cr1         Condition
    34        cr2         ECR
    35        cr3         EXCPVec
    36        cr4         CCR
    37        cr5         EPC
    38        cr6         EMA
    39        cr7         TLBLock
    40        cr8         TLBPT
    41        cr8         PEADDR
    42        cr10        TLBRPT
    43        cr11        PEVN
    44        cr12        PECTX
    45        cr13
    46        cr14
    47        cr15

    Custom Engine Register, use mfce/mtce
    48        CEH        CEH
    49        CEL        CEL

    Special-Purpose Register, use mfsr/mtsr
    50        sr0        CNT
    51        sr1        LCR
    52        sr2        SCR

    53        ARG_POINTER_REGNUM
    54        FRAME_POINTER_REGNUM
    but Control register have 32 registers, cr16-cr31.  */
#define FIXED_REGISTERS                                  \
{                                                        \
  /* General Purpose Registers  */                       \
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,        \
  /* Control Registers  */                               \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CEH/ CEL/ CNT/ LCR/ SCR / ARG_POINTER_REGNUM/ FRAME_POINTER_REGNUM */\
  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 1 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 2 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 3 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
}

#define CALL_USED_REGISTERS                              \
{                                                        \
  /* General purpose register  */                        \
  1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,        \
  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* Control Registers  */                               \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 1 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 2 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  /* CP 3 Registers  */                                  \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,        \
}

#define REG_ALLOC_ORDER                                                   \
{   0,  1,  6,  7,  8,  9, 10, 11,  4,  5, 22, 23, 24, 25, 26, 27,        \
   12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 28, 29, 30, 31,  2,  3,        \
   32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,        \
   48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,        \
   64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,        \
   80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,        \
   96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111,        \
  112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,        \
  128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,        \
  144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159        }

/* Macro to conditionally modify fixed_regs/call_used_regs.  */
#define PIC_OFFSET_TABLE_REGNUM          29

#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Return true if REGNO is suitable for holding a quantity of type MODE.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) score_hard_regno_mode_ok (REGNO, MODE)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)                             \
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT                          \
    || GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)              \
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT                       \
       || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* Register Classes.  */
/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.  */
enum reg_class
{
  NO_REGS,
  G16_REGS,    /* r0 ~ r15 */
  G32_REGS,    /* r0 ~ r31 */
  T32_REGS,    /* r8 ~ r11 | r22 ~ r27 */

  HI_REG,      /* hi                 */
  LO_REG,      /* lo                 */
  CE_REGS,     /* hi + lo            */

  CN_REG,      /* cnt                */
  LC_REG,      /* lcb                */
  SC_REG,      /* scb                */
  SP_REGS,     /* cnt + lcb + scb    */

  CR_REGS,     /* cr0 - cr15         */

  CP1_REGS,    /* cp1                */
  CP2_REGS,    /* cp2                */
  CP3_REGS,    /* cp3                */
  CPA_REGS,    /* cp1 + cp2 + cp3    */

  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES                  ((int) LIM_REG_CLASSES)

#define GENERAL_REGS                   G32_REGS

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES           \
{                                 \
  "NO_REGS",                      \
  "G16_REGS",                     \
  "G32_REGS",                     \
  "T32_REGS",                     \
                                  \
  "HI_REG",                       \
  "LO_REG",                       \
  "CE_REGS",                      \
                                  \
  "CN_REG",                       \
  "LC_REG",                       \
  "SC_REG",                       \
  "SP_REGS",                      \
                                  \
  "CR_REGS",                      \
                                  \
  "CP1_REGS",                     \
  "CP2_REGS",                     \
  "CP3_REGS",                     \
  "CPA_REGS",                     \
                                  \
  "ALL_REGS",                     \
}

/* Define which registers fit in which classes.  */
#define REG_CLASS_CONTENTS                                        \
{                                                                 \
  /* NO_REGS/G16/G32/T32  */                                      \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x0000ffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x0fc00f00, 0x00000000, 0x00000000, 0x00000000, 0x00000000},  \
  /* HI/LO/CE  */                                                 \
  { 0x00000000, 0x00010000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x00020000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x00030000, 0x00000000, 0x00000000, 0x00000000},  \
  /* CN/LC/SC/SP/CR  */                                           \
  { 0x00000000, 0x00040000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x00080000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x00100000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x001c0000, 0x00000000, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x0000ffff, 0x00000000, 0x00000000, 0x00000000},  \
  /* CP1/CP2/CP3/CPA  */                                          \
  { 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000},  \
  { 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000},  \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff},  \
  { 0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0xffffffff},  \
  /* ALL_REGS  */                                                 \
  { 0xffffffff, 0x001fffff, 0xffffffff, 0xffffffff, 0xffffffff},  \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
#define REGNO_REG_CLASS(REGNO) (enum reg_class) score_reg_class (REGNO)

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */
#define BASE_REG_CLASS                 G16_REGS

/* The class value for index registers.  */
#define INDEX_REG_CLASS                NO_REGS

extern enum reg_class score_char_to_class[256];
#define REG_CLASS_FROM_LETTER(C)       score_char_to_class[(unsigned char) (C)]

/* Addressing modes, and classification of registers for them.  */
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  score_regno_mode_ok_for_base_p (REGNO, 1)

#define REGNO_OK_FOR_INDEX_P(NUM)       0

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  score_preferred_reload_class (X, CLASS)

/* If we need to load shorts byte-at-a-time, then we need a scratch.  */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
  score_secondary_reload_class (CLASS, MODE, X)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
  score_secondary_reload_class (CLASS, MODE, X)

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS)    \
  (GET_MODE_SIZE (FROM) != GET_MODE_SIZE (TO)        \
   ? reg_classes_intersect_p (HI_REG, (CLASS)) : 0)


/* Basic Stack Layout.  */
/* Stack layout; function entry, exit and calling.  */
#define STACK_GROWS_DOWNWARD

#define STACK_PUSH_CODE                 PRE_DEC
#define STACK_POP_CODE                  POST_INC

/* The offset of the first local variable from the beginning of the frame.
   See compute_frame_size for details about the frame layout.  */
#define STARTING_FRAME_OFFSET           crtl->outgoing_args_size

/* The argument pointer always points to the first argument.  */
#define FIRST_PARM_OFFSET(FUNDECL)      0

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */
#define RETURN_ADDR_RTX(count, frame)   score_return_addr (count, frame)

/* Pick up the return address upon entry to a procedure.  */
#define INCOMING_RETURN_ADDR_RTX        gen_rtx_REG (VOIDmode, RA_REGNUM)

/* Exception handling Support.  */
/* Use r0 to r3 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? (N) + ARG_REG_FIRST : INVALID_REGNUM)

/* The register that holds the return address in exception handlers.  */
#define EH_RETURN_STACKADJ_RTX          gen_rtx_REG (Pmode, EH_REGNUM)
#define EH_RETURN_HANDLER_RTX  		gen_rtx_REG (SImode, 30)

/* Registers That Address the Stack Frame.  */
/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM            SP_REGNUM

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define FRAME_POINTER_REGNUM            53

/*  we use r2 as the frame pointer.  */
#define HARD_FRAME_POINTER_REGNUM       FP_REGNUM

#define ARG_POINTER_REGNUM              54

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM             23

/* Elimination Frame Pointer and Arg Pointer  */

#define ELIMINABLE_REGS                                \
  {{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},        \
   { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},   \
   { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},      \
   { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = score_initial_elimination_offset ((FROM), (TO))

/* Passing Function Arguments on the Stack.  */
/* Allocate stack space for arguments at the beginning of each function.  */
#define ACCUMULATE_OUTGOING_ARGS        1

/* reserve stack space for all argument registers.  */
#define REG_PARM_STACK_SPACE(FNDECL)    UNITS_PER_WORD

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers.
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in
   `crtl->outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

/* Passing Arguments in Registers  */
/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the
   type `int' suffices and can hold the number of bytes of argument so far.  */
typedef struct score_args
{
  unsigned int arg_number;             /* how many arguments have been seen  */
  unsigned int num_gprs;               /* number of gprs in use  */
  unsigned int stack_words;            /* number of words in stack  */
} score_args_t;

#define CUMULATIVE_ARGS                score_args_t

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, n_named_args) \
  score_init_cumulative_args (&CUM, FNTYPE, LIBNAME)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  When FP registers
   are 32 bits, we can't directly reference the odd numbered ones.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  REG_CONTAIN (REGNO, ARG_REG_FIRST, ARG_REG_NUM)

/* How Scalar Function Values Are Returned.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  score_function_value ((VALTYPE), (FUNC), VOIDmode)

#define LIBCALL_VALUE(MODE)  score_function_value (NULL_TREE, NULL, (MODE))

/* 1 if N is a possible register number for a function value.  */
#define FUNCTION_VALUE_REGNO_P(REGNO)   ((REGNO) == (ARG_REG_FIRST))

#define PIC_FUNCTION_ADDR_REGNUM        (GP_REG_FIRST + 25)

/* How Large Values Are Returned.  */
#define STRUCT_VALUE                    0

/* Function Entry and Exit  */
/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK               1

/* Generating Code for Profiling  */
/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#define FUNCTION_PROFILER(FILE, LABELNO)                              \
  do {                                                                \
    if (TARGET_SCORE7)                                                \
      {                                                               \
        fprintf (FILE, " .set r1  \n");                               \
        fprintf (FILE, " mv   r%d,r%d \n", AT_REGNUM, RA_REGNUM);     \
        fprintf (FILE, " subi r%d, %d \n", STACK_POINTER_REGNUM, 8);  \
        fprintf (FILE, " jl   _mcount \n");                           \
        fprintf (FILE, " .set nor1 \n");                              \
      }                                                               \
  } while (0)

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_INSNS                6

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE                (24 + GET_MODE_SIZE (ptr_mode) * 2)

#define HAVE_PRE_INCREMENT              1
#define HAVE_PRE_DECREMENT              1
#define HAVE_POST_INCREMENT             1
#define HAVE_POST_DECREMENT             1
#define HAVE_PRE_MODIFY_DISP            1
#define HAVE_POST_MODIFY_DISP           1
#define HAVE_PRE_MODIFY_REG             0
#define HAVE_POST_MODIFY_REG            0

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS            1

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */
#ifndef REG_OK_STRICT
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  score_regno_mode_ok_for_base_p (REGNO (X), 0)
#else
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  score_regno_mode_ok_for_base_p (REGNO (X), 1)
#endif

#define REG_OK_FOR_INDEX_P(X) 0

/* Condition Code Status.  */
#define SELECT_CC_MODE(OP, X, Y)        score_select_cc_mode (OP, X, Y)

/* Return nonzero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE)        1

/* Describing Relative Costs of Operations  */
/* Compute extra cost of moving data between one register class and another.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO) \
  score_register_move_cost (MODE, FROM, TO)

/* Moves to and from memory are quite expensive */
#define MEMORY_MOVE_COST(MODE, CLASS, TO_P) \
  (4 + memory_move_secondary_cost ((MODE), (CLASS), (TO_P)))

/* Try to generate sequences that don't involve branches.  */
#define BRANCH_COST(speed_p, predictable_p) 2

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS                1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE                 1

/* Dividing the Output into Sections (Texts, Data, ...).  */
/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP             "\t.text"
#define DATA_SECTION_ASM_OP             "\t.data"
#define SDATA_SECTION_ASM_OP            "\t.sdata"

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP    "\t.rdata"

/* The Overall Framework of an Assembler File  */
/* How to start an assembler comment.
   The leading space is important.  */
#define ASM_COMMENT_START               "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON                     "#APP\n\t.set volatile\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF                     "#NO_APP\n\t.set optimize\n"

/* Output of Uninitialized Variables.  */
/* This says how to define a global common symbol.  */
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(STREAM, DECL, NAME, SIZE, ALIGN)     \
  do {                                                                      \
    fputs ("\n\t.comm\t", STREAM);                                          \
    assemble_name (STREAM, NAME);                                           \
    fprintf (STREAM, " , " HOST_WIDE_INT_PRINT_UNSIGNED ", %u\n",           \
             SIZE, ALIGN / BITS_PER_UNIT);                                  \
  } while (0)

/* This says how to define a local common symbol (i.e., not visible to
   linker).  */
#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)                 \
  do {                                                                      \
    fputs ("\n\t.lcomm\t", STREAM);                                         \
    assemble_name (STREAM, NAME);                                           \
    fprintf (STREAM, " , " HOST_WIDE_INT_PRINT_UNSIGNED ", %u\n",           \
             SIZE, ALIGN / BITS_PER_UNIT);                                  \
  } while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP                   "\t.globl\t"

/* Output and Generation of Labels  */
/* This is how to declare a function name.  The actual work of
   emitting the label is moved to function_prologue, so that we can
   get the line number correctly emitted before the .ent directive,
   and after any .file directives.  Define as empty so that the function
   is not declared before the .ent directive elsewhere.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)   \
  do {                                                \
    assemble_name (STREAM, NAME);                     \
    fprintf (STREAM, ":\n");                          \
  } while (0)

/* This says how to output an external.  It would be possible not to
   output anything and let undefined symbol become external. However
   the assembler uses length information on externals to allocate in
   data/sdata bss/sbss, thereby saving exec time.  */
#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(STREAM, DECL, NAME) \
  score_output_external (STREAM, DECL, NAME)

/* This handles the magic '..CURRENT_FUNCTION' symbol, which means
   'the start of the function that this code is output in'.  */
#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  fprintf ((STREAM), "%s", (NAME))

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.  */
#define LOCAL_LABEL_PREFIX              (TARGET_SCORE7 ? "." : "$")

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long) (NUM))

/* Output of Assembler Instructions.  */
#define REGISTER_NAMES                                                    \
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",                         \
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",                   \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",                 \
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",                 \
                                                                          \
  "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",                 \
  "cr8", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15",           \
                                                                          \
  "ceh", "cel", "sr0", "sr1", "sr2", "_arg", "_frame", "",                \
  "cr24", "cr25", "cr26", "cr27", "cr28", "cr29", "cr30", "cr31",         \
                                                                          \
  "c1r0", "c1r1", "c1r2", "c1r3", "c1r4", "c1r5", "c1r6", "c1r7",         \
  "c1r8", "c1r9", "c1r10", "c1r11", "c1r12", "c1r13", "c1r14", "c1r15",   \
  "c1r16", "c1r17", "c1r18", "c1r19", "c1r20", "c1r21", "c1r22", "c1r23", \
  "c1r24", "c1r25", "c1r26", "c1r27", "c1r28", "c1r29", "c1r30", "c1r31", \
                                                                          \
  "c2r0", "c2r1", "c2r2", "c2r3", "c2r4", "c2r5", "c2r6", "c2r7",         \
  "c2r8", "c2r9", "c2r10", "c2r11", "c2r12", "c2r13", "c2r14", "c2r15",   \
  "c2r16", "c2r17", "c2r18", "c2r19", "c2r20", "c2r21", "c2r22", "c2r23", \
  "c2r24", "c2r25", "c2r26", "c2r27", "c2r28", "c2r29", "c2r30", "c2r31", \
                                                                          \
  "c3r0", "c3r1", "c3r2", "c3r3", "c3r4", "c3r5", "c3r6", "c3r7",         \
  "c3r8", "c3r9", "c3r10", "c3r11", "c3r12", "c3r13", "c3r14", "c3r15",   \
  "c3r16", "c3r17", "c3r18", "c3r19", "c3r20", "c3r21", "c3r22", "c3r23", \
  "c3r24", "c3r25", "c3r26", "c3r27", "c3r28", "c3r29", "c3r30", "c3r31", \
}

/* Print operand X (an rtx) in assembler syntax to file FILE.  */
#define PRINT_OPERAND(STREAM, X, CODE)  score_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
#define PRINT_OPERAND_PUNCT_VALID_P(C)  ((C) == '[' || (C) == ']')

/* Print a memory address as an operand to reference that memory location.  */
#define PRINT_OPERAND_ADDRESS(STREAM, X) \
  score_print_operand_address (STREAM, X)

/* By default on the S+core, external symbols do not have an underscore
   prepended.  */
#define USER_LABEL_PREFIX        ""

/* This is how to output an insn to push a register on the stack.  */
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)           \
  do {                                               \
    if (TARGET_SCORE7)                               \
        fprintf (STREAM, "\tpush! %s,[%s]\n",        \
                 reg_names[REGNO],                   \
                 reg_names[STACK_POINTER_REGNUM]);   \
  } while (0)

/* This is how to output an insn to pop a register from the stack.  */
#define ASM_OUTPUT_REG_POP(STREAM, REGNO)            \
  do {                                               \
    if (TARGET_SCORE7)                               \
      fprintf (STREAM, "\tpop! %s,[%s]\n",           \
               reg_names[REGNO],                     \
               reg_names[STACK_POINTER_REGNUM]);     \
  } while (0)

/* Output of Dispatch Tables.  */
/* This is how to output an element of a case-vector.  We can make the
   entries PC-relative in GP-relative when .gp(d)word is supported.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)                \
  do {                                                                    \
    if (TARGET_SCORE7)                                                    \
      if (flag_pic)                                                       \
        fprintf (STREAM, "\t.gpword %sL%d\n", LOCAL_LABEL_PREFIX, VALUE); \
      else                                                                \
        fprintf (STREAM, "\t.word %sL%d\n", LOCAL_LABEL_PREFIX, VALUE);   \
  } while (0)

/* Jump table alignment is explicit in ASM_OUTPUT_CASE_LABEL.  */
#define ADDR_VEC_ALIGN(JUMPTABLE) (GET_MODE (PATTERN (JUMPTABLE)) == SImode ? 2 \
                                   : GET_MODE (PATTERN (JUMPTABLE)) == HImode ? 1 : 0)

/* This is how to output a label which precedes a jumptable.  Since
   Score3 instructions are 2 bytes, we may need explicit alignment here.  */
#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE)             \
  do {                                                                  \
      if ((TARGET_SCORE7) && GET_MODE (PATTERN (JUMPTABLE)) == SImode)  \
        ASM_OUTPUT_ALIGN (FILE, 2);                                     \
      (*targetm.asm_out.internal_label) (FILE, PREFIX, NUM);            \
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE                SImode

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  fprintf (STREAM, "\t.word %sL%d\n", LOCAL_LABEL_PREFIX, VALUE)

/* Assembler Commands for Exception Regions  */
/* Since the S+core is encoded in the least-significant bit
   of the address, mask it off return addresses for purposes of
   finding exception handling regions.  */
#define MASK_RETURN_ADDR               constm1_rtx

/* Assembler Commands for Alignment  */
/* This is how to output an assembler line to advance the location
   counter by SIZE bytes.  */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, SIZE) \
  fprintf (STREAM, "\t.space\t"HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(STREAM, LOG) \
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

/* Macros Affecting All Debugging Formats.  */
#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE         DWARF2_DEBUG
#endif

/* Specific Options for DBX Output.  */
#define DBX_DEBUGGING_INFO              1

/* By default, turn on GDB extensions.  */
#define DEFAULT_GDB_EXTENSIONS          1

#define DBX_CONTIN_LENGTH               0

/* File Names in DBX Format.  */
#define DWARF2_DEBUGGING_INFO           1

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN       3

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/*  All references are zero extended.  */
#define LOAD_EXTEND_OP(MODE)            ZERO_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX                        4

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED           1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode                           SImode

/* Give call MEMs SImode since it is the "most permissive" mode
   for 32-bit targets.  */
#define FUNCTION_MODE                   Pmode

struct GTY ((chain_next ("%h.next"))) extern_list
{
  struct extern_list *next;             /* next external  */
  const char *name;                     /* name of the external  */
  int size;                             /* size in bytes  */
};

extern GTY (()) struct extern_list      *extern_head;
