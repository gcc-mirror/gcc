/* Definitions of target machine for GNU compiler for picoJava
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* Contributed by Steve Chamberlain of Transmeta (sac@pobox.com).  */


#define TARGET_VERSION  fputs ("(picoJava)", stderr);

/* We support two different default configurations.  */
#undef ASM_SPEC
#ifdef TARGET_LITTLE_ENDIAN_DEFAULT
#define CPP_SPEC        "%{mb:-D__BIG_ENDIAN__ }%{!mb:-D__LITTLE_ENDIAN__ }" 
#define ASM_SPEC        "%{mb:-mb }%{!mb:-ml }"
#else
#define CPP_SPEC        "%{ml:-D__LITTLE_ENDIAN__ }%{!ml:-D__BIG_ENDIAN__}"
#define ASM_SPEC        "%{ml:-ml } %{!ml:-mb }"
#endif

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -D__pj__ -Asystem(posix)"
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

#define LITTLE_ENDIAN_BIT (1<<0)
#define EXTENSIONS_BIT    (1<<1)
#define PJ_TEST_BIT       (1<<2)
#define REORG_BIT         (1<<3)

/* Nonzero if generating code for a little endian pico java.  */

#define TARGET_LITTLE_ENDIAN     (target_flags & LITTLE_ENDIAN_BIT)

/* Nonzero to turn on internal tests.  */

#define TARGET_TEST              (target_flags & PJ_TEST_BIT)

/* Nonzero to turn on picoJava extensions.  */

#define TARGET_TM_EXTENSIONS     (target_flags & EXTENSIONS_BIT)

/* Nonzero to turn on the reorganization pass.  */

#define TARGET_REORG             (target_flags & REORG_BIT)

#ifdef TARGET_LITTLE_ENDIAN_DEFAULT
#define TARGET_DEFAULT  (LITTLE_ENDIAN_BIT|EXTENSIONS_BIT|REORG_BIT)
#else
#define TARGET_DEFAULT  REORG_BIT
#endif

#define TARGET_SWITCHES  \
{ {"l",         LITTLE_ENDIAN_BIT, "Generate little endian data"           }, \
  {"b",        -LITTLE_ENDIAN_BIT, "Generate big endian data"              }, \
  {"t",         PJ_TEST_BIT,       "Turn on maintainer testing code"       }, \
  {"ext",       EXTENSIONS_BIT,    "Enable Transmeta picoJava extensions"  }, \
  {"no-ext",   -EXTENSIONS_BIT,    "Disable Transmeta picoJava extensions" }, \
  {"no-reorg", -REORG_BIT,         "Disable reorganization pass"           }, \
  {"",          TARGET_DEFAULT,    0 }}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  

   We take this chance to register the global variables with the garbage
   collector. */

#define OVERRIDE_OPTIONS                                                \
 do {                                                                   \
   ggc_add_rtx_root (&pj_cmp_op0, 1);                                   \
   ggc_add_rtx_root (&pj_cmp_op1, 1);                                   \
 } while (0)

/* Define this to change the optimizations performed by default.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)                                \
 do {                                                                   \
   if (optimize)                                                        \
       flag_force_addr = 1;                                             \
 } while (0)

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined(TARGET_LITTLE_ENDIAN_DEFAULT)
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#endif

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT  8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD  32
#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD  4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE  32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY   32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY  8

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST       \
    && (ALIGN) < FASTEST_ALIGNMENT)     \
    ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)             \
  (TREE_CODE (TYPE) == ARRAY_TYPE               \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode    \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1


/* Standard register usage.  */

/* Enumerate the hardware registers.  */

enum
{
  R0_REG,       R1_REG,         R2_REG,         R3_REG, 
  R4_REG,       R5_REG,         R6_REG,         R7_REG, 
  R8_REG,       R9_REG,         R10_REG,        R11_REG,
  R12_REG,      R13_REG,        R14_REG,        R15_REG,

  R16_REG,      R17_REG,        R18_REG,        R19_REG,
  R20_REG,      R21_REG,        R22_REG,        R23_REG,
  R24_REG,      R25_REG,        R26_REG,        R27_REG,
  R28_REG,      R29_REG,        R30_REG,        R31_REG,

  I0_REG,       I1_REG,         I2_REG,         I3_REG,  
  I4_REG,       I5_REG,         I6_REG,         I7_REG,  
  I8_REG,       I9_REG,         I10_REG,        I11_REG, 
  I12_REG,      I13_REG,        I14_REG,        I15_REG, 

  I16_REG,      I17_REG,        I18_REG,        I19_REG, 
  I20_REG,      I21_REG,        I22_REG,        I23_REG, 
  I24_REG,      I25_REG,        I26_REG,        I27_REG, 
  I28_REG,      I29_REG,        I30_REG,        ISC_REG, 

  G0_REG,       G1_REG,         G2_REG,         G3_REG,  
  G4_REG,       G5_REG,         G6_REG,         G7_REG,  
  VARS_REG,     OPTOP_REG,      SC_REG,         PC_REG,  
  TICKS_REG,    SLOW_REG,       VA_REG,         D3_REG, 

  D4_REG,       D5_REG,         D6_REG,         D7_REG,
  Q0_REG,       Q1_REG,         Q2_REG,         Q3_REG,
  P0_REG,       P1_REG,         P2_REG,         P3_REG,
  P4_REG,       P5_REG,         P6_REG,         P7_REG,

  O0_REG,       O1_REG,         O2_REG,         O3_REG, 
  O4_REG,       O5_REG,         O6_REG,         O7_REG, 
  O8_REG,       O9_REG,         O10_REG,        O11_REG,
  O12_REG,      O13_REG,        O14_REG,        O15_REG,

  O16_REG,      O17_REG,        O18_REG,        O19_REG,
  O20_REG,      O21_REG,        O22_REG,        O23_REG,
  O24_REG,      O25_REG,        O26_REG,        O27_REG,
  O28_REG,      O29_REG,        O30_REG,        OSC_REG,
  
  LAST_O_REG=OSC_REG,
  LAST_R_REG=R31_REG,
  LAST_I_REG=ISC_REG,
  LAST_S_REG=P7_REG,

};

/* Useful predicates.  */

#define STACK_REGNO_P(REGNO) 		\
	(((unsigned) (REGNO)) <= LAST_I_REG)

#define OUTGOING_REGNO_P(REGNO) 	\
  	(((REGNO) >= O0_REG) && ((REGNO) <= LAST_O_REG))

#define INCOMING_REGNO_P(REGNO)   	\
	(((REGNO) >= I0_REG) && ((REGNO) <= LAST_I_REG))

#define STACK_REG_RTX_P(RTX)    	\
	(GET_CODE (RTX) == REG && STACK_REGNO_P (REGNO (RTX)))

#define OUTGOING_REG_RTX_P(RTX) 	\
	(GET_CODE (RTX) == REG && OUTGOING_REGNO_P (REGNO (RTX)))

#define OPTOP_REG_RTX_P(RTX) 		\
	(GET_CODE (RTX) == REG && REGNO (RTX) == OPTOP_REG)

#define FIRST_PSEUDO_REGISTER 128

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS                                                 \
 {                                                                      \
   0,0,0,0, 0,0,0,0,  /* r0 .. r7  */                                   \
   0,0,0,0, 0,0,0,0,  /* r8 .. r15 */                                   \
   0,0,0,0, 0,0,0,0,  /* r16.. r23 */                                   \
   0,0,0,0, 0,0,0,0,  /* r24.. r31 */                                   \
                                                                        \
   0,0,0,0, 0,0,0,0,  /* i0 .. i7  */                                   \
   0,0,0,0, 0,0,0,0,  /* i8 .. i15 */                                   \
   0,0,0,0, 0,0,0,0,  /* i16.. i23 */                                   \
   0,0,0,0, 0,0,0,0,  /* i24.. i31 */                                   \
                                                                        \
   1,0,0,1, 1,1,1,1,  /* g0 .. g7  */                                   \
   1,1,1,1, 1,1,1,1,  /* vars, optop, sc, pc, ticks, slow, va, sgo */   \
   1,1,1,1, 1,1,1,1,  /* d4 d5 d6 ap p0 p1 p2 p3 */                     \
   1,1,1,1, 1,1,1,1,  /* q1 .. q7 */                                    \
                                                                        \
   0,0,0,0, 0,0,0,0,  /* o0 .. o7  */                                   \
   0,0,0,0, 0,0,0,0,  /* o8 .. o15 */                                   \
   0,0,0,0, 0,0,0,0,  /* o16.. o23 */                                   \
   0,0,0,0, 0,0,0,0 } /* o24.. o31 */                                   
                                                                          

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  

   We pretend that some standard registers are call clobbered so the
   exception handler code has somewhere to play.  */

#define CALL_USED_REGISTERS                                             \
 {                                                                      \
   0,0,0,0, 0,0,0,0,  /* r0 ..r7  */                                    \
   0,0,0,0, 0,0,0,0,  /* r8 ..r15 */                                    \
   0,0,0,0, 1,1,1,1,  /* r16..r23 */                                    \
   1,1,1,1, 1,1,1,1,  /* r24..r31 */                                    \
                                                                        \
   0,0,0,0, 0,0,0,0,  /* i0 ..i7  */                                    \
   0,0,0,0, 0,0,0,0,  /* i8 ..i15 */                                    \
   0,0,0,0, 0,0,0,0,  /* i16..i23 */                                    \
   0,0,0,0, 0,0,0,0,  /* i24..i31 */                                    \
                                                                        \
   1,1,1,1, 0,0,0,0,  /* g0 ..g7  */                                    \
   1,1,1,1, 1,1,1,1,  /* vars, optop, sc, pc, ticls, slow, va, sgo */   \
   1,1,1,1, 1,1,1,1,  /* d4 d5 d6 ap p0..p3*/                           \
   1,1,1,1, 1,1,1,1,  /* q0..q7  */                                     \
                                                                        \
   1,1,1,1, 1,1,1,1,  /* o0 ..o7  */                                    \
   1,1,1,1, 1,1,1,1,  /* o8 ..o15 */                                    \
   1,1,1,1, 1,1,1,1,  /* o16..o23 */                                    \
   1,1,1,1, 1,1,1,1 } /* o24..o31 */                                    
  
/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.

   We can allow any mode in the general register or the result
   register.  It's only safe to put up to 4 bytes values elsewhere.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)                               \
   (((REGNO) <= LAST_R_REG || (REGNO) == G1_REG || GET_MODE_SIZE(MODE) <= 4 ) && !OUTGOING_REGNO_P(REGNO))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
#define PC_REGNUM               PC_REG

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM    G0_REG

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM    R31_REG

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM      R30_REG

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM     G1_REG

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.  */
#define FRAME_POINTER_REQUIRED  0

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

#define ELIMINABLE_REGS                                                 \
   { { VA_REG,                          STACK_POINTER_REGNUM    },      \
     { FRAME_POINTER_REGNUM,            STACK_POINTER_REGNUM    },      \
     { ARG_POINTER_REGNUM,              STACK_POINTER_REGNUM    } }    
     
/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) 1

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET =  (((FROM) == FRAME_POINTER_REGNUM) ? get_frame_size() : 0)

/* For picoJava we have to save 12 bytes of information for a non local 
   jump.  */

#define STACK_SAVEAREA_MODE(x) ((x)==SAVE_NONLOCAL ? XFmode : Pmode)

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */
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
  ((TYPE_MODE (TYPE) == BLKmode) || int_size_in_bytes (TYPE) > 8)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
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
   class that represents their union.  */

enum reg_class
{
  NO_REGS,
  OUT_REGS,         /* Registers for passing outgoing parameters.  */
  STD_REGS,         /* Standard registers, on opstack.  */
  ARG_REGS,         /* Incoming argument registers.  */
  SRC_REGS,         /* All registers valid as a source.  */
  DST_REGS,         /* All registers valid as a destination.  */
  ALL_REGS,
  LIM_REG_CLASSES,
};

#define GENERAL_REGS SRC_REGS
#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump files.  */
#define REG_CLASS_NAMES   \
{                         \
  "NO_REGS",              \
  "OUT_REGS",             \
  "STD_REGS",             \
  "ARG_REGS",             \
  "SRC_REGS",             \
  "DST_REGS",             \
  "ALL_REGS",             \
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS                                              \
{                                                                       \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* NO_REGS  */    \
  { 0x00000000, 0x00000000, 0x00000000, 0xffffffff }, /* OUT_REGS */    \
  { 0xffffffff, 0x00000000, 0x00000000, 0x00000000 }, /* STD_REGS */    \
  { 0x00000000, 0xffffffff, 0x00000000, 0x00000000 }, /* ARG_REGS */    \
  { 0xffffffff, 0xffffffff, 0x000fff0f, 0x00000000 }, /* SRC_REGS */    \
  { 0xffffffff, 0xffffffff, 0x000fff0f, 0xffffffff }, /* DST_REGS */    \
  { 0xffffffff, 0xffffffff, 0x000fff0f, 0xffffffff }, /* ALL_REGS */    \
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)                  \
     ( ((REGNO) <= LAST_R_REG) ? STD_REGS       \
     : ((REGNO) <= LAST_I_REG) ? ARG_REGS       \
     : ((REGNO) <= LAST_S_REG) ? SRC_REGS       \
     : OUT_REGS)
        
/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  GENERAL_REGS
#define BASE_REG_CLASS   GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine
   description.  */

#define REG_CLASS_FROM_LETTER(C) \
   ( (C) == 'S' ? SRC_REGS \
   : (C) == 'D' ? DST_REGS \
   : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   I: arithmetic operand -127..128, as used in inc.
   K: 0.
   */

#define CONST_OK_FOR_I(VALUE) \
 (((HOST_WIDE_INT)(VALUE))>= -128 && ((HOST_WIDE_INT)(VALUE)) <= 127)

#define CONST_OK_FOR_K(VALUE) ((VALUE)==0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)         \
     ((C) == 'I' ? CONST_OK_FOR_I (VALUE)       \
    : (C) == 'K' ? CONST_OK_FOR_K (VALUE)       \
    : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   With picoJava this is the size of MODE in words.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
     ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   For picoJava, something that isn't an incoming argument or a normal
   register is going to be very hard to get at. */

#define CLASS_LIKELY_SPILLED_P(X) ((X) != STD_REGS && (X) != ARG_REGS)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */

#define STACK_GROWS_DOWNWARD 1

/* Define this macro if successive arguments to a function occupy
   decreasing addresses on the stack.  */

#define ARGS_GROW_DOWNWARD 1

/*  Define this macro if the addresses of local variable slots are at
    negative offsets from the frame pointer.  */

#define FRAME_GROWS_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */

#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */

/* Don't define PUSH_ROUNDING, since the hardware doesn't do this.
   When PUSH_ROUNDING is not defined, PARM_BOUNDARY will cause gcc to
   do correct alignment.  */

#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)

/* Offset of first parameter from the argument pointer register value.  */

#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0 

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
   gen_rtx_REG (TYPE_MODE (VALTYPE), G1_REG)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N)  \
  ((N) == G1_REG)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE, G1_REG)

/* Define this macro to be a nonzero value if the location where a
   function argument is passed depends on whether or not it is a
   named argument.  */

#define STRICT_ARGUMENT_NAMING 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   For picoJava this is a struct which remembers the number of
   arguments named, the total number of words passed and an adjustment
   factor to use if accessing a double word argument with a single
   word memop.  See the comments at the head pj.c for more information */

#define ARGS_IN_REGS 32

struct pj_args
{
  int named_words;
  int total_words;
  int arg_count;
  int arg_adjust[ARGS_IN_REGS];
};

#define CUMULATIVE_ARGS  struct pj_args

#define FUNCTION_INCOMING_ARG(asf,pmode,passtyped,named) \
    pj_function_incoming_arg(&asf,pmode,passtyped,named)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.
 */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  (CUM).named_words = 0;                                     \
  (CUM).total_words = 0;                                     \
  (CUM).arg_count = 0;

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.

   picoJava only ever sends scalars as arguments.  Aggregates are sent
   by reference.  */

#define PJ_ARG_WORDS(MODE)  \
   ((GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)                    \
{                                                                       \
  (CUM).total_words += PJ_ARG_WORDS (MODE);                             \
  if (NAMED)                                                            \
    (CUM).named_words += PJ_ARG_WORDS (MODE);                           \
  (CUM).arg_count++;                                                    \
}

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
    (otherwise it is an extra parameter matching an ellipsis).

   For picoJava scalar arguments are normally in registers.  */


#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)                            \
    ( ((CUM).total_words + PJ_ARG_WORDS (MODE) < ARGS_IN_REGS)          \
       ? gen_rtx (REG, MODE, O0_REG + (CUM).total_words)                \
       : NULL_RTX)


/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */

/* All aggregates and arguments larger than 8 bytes are passed this way.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
 (TYPE && (AGGREGATE_TYPE_P (TYPE) || int_size_in_bytes (TYPE) > 8))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* Trampoline support.  */

/* A picoJava trampoline looks like:

 0000 11DEAD            sipush %lo16(static)
 0003 EDDEAD            sethi  %hi16(static)
 0006 FF7D              write_global1
 0008 11DEAD            sipush %lo16(fn)
 000b EDDEAD            sethi  %hi16(fn)
 000e FF60              write_pc
*/ 

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  16

/* Alignment required for a trampoline in bits .  */
#define TRAMPOLINE_ALIGNMENT  32

#define TRAMPOLINE_TEMPLATE(FILE) \
  fprintf (FILE, "\tsipush 0xdead\n"); \
  fprintf (FILE, "\tsethi  0xdead\n"); \
  fprintf (FILE, "\twrite_global1\n"); \
  fprintf (FILE, "\tsipush 0xdead\n"); \
  fprintf (FILE, "\tsethi  0xdead\n"); \
  fprintf (FILE, "\twrite_pc\n"); 

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)                             \
{                                                                             \
  static int off[4] = { 1, 0, 4, 3 };                                         \
  int i;                                                                      \
                                                                              \
  /* Move the FNADDR and CXT into the instruction stream. Do this byte        \
     by byte to make sure it works for either endianness.  */                 \
                                                                              \
  for (i = 0; i < 4; i++)                                                     \
    emit_move_insn                                                            \
      (gen_rtx_MEM (QImode,                                                   \
                    plus_constant (tramp, off[i] + 1)),                       \
       gen_rtx_TRUNCATE (QImode,                                              \
                         expand_shift (RSHIFT_EXPR, SImode,                   \
                                       CXT, size_int (i * 8), 0, 1)));        \
                                                                              \
  for (i = 0; i < 4; i++)                                                     \
    emit_move_insn                                                            \
      (gen_rtx_MEM (QImode,                                                   \
                    plus_constant (tramp, off[i] + 9)),                       \
       gen_rtx_TRUNCATE (QImode,                                              \
                         expand_shift (RSHIFT_EXPR, SImode,                   \
                                       FNADDR, size_int (i * 8), 0, 1)));     \
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)                        \
 fprintf (FILE, "\tsipush %%lo16(.LP%d)\n", (LABELNO));         \
 fprintf (FILE, "\tsethi  %%hi16(.LP%d)\n", (LABELNO));         \
 fprintf (FILE, "\tsipush %%lo16(_mcount)\n");                  \
 fprintf (FILE, "\tsethi  %%hi16(_mcount)\n");                  \
 fprintf (FILE, "\ticonst_3\n");                                \
 fprintf (FILE, "\tcall\n");                                    


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT  1
#define HAVE_PRE_INCREMENT   1
#define HAVE_POST_DECREMENT  1
#define HAVE_PRE_DECREMENT   1

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

/* Any register is OK for a base or an index.  As is something that has
   been spilled to memory.  */

#define REGNO_OK_FOR_BASE_P(REGNO) 1
#define REGNO_OK_FOR_INDEX_P(REGNO) 1

/* Maximum number of registers that can appear in a valid memory
   address.  

   Arbitarily limited to 20.  */

#define MAX_REGS_PER_ADDRESS 20

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)                                   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF      \
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) == CONST_DOUBLE ? (pj_standard_float_constant (X)!=0) : 1)

/* Letters in the range `Q' through `U' in a register constraint string
   may be defined in a machine-dependent fashion to stand for arbitrary
   operand types.

   For picoJava, `S' handles a source operand. */

#define EXTRA_CONSTRAINT(OP, C)                         \
   ((C) == 'S' ? pj_source_operand (OP, GET_MODE (OP)) : 0)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and
   check its validity for a certain class.  */

#define REG_OK_FOR_BASE_P(X) 1
#define REG_OK_FOR_INDEX_P(x) 0


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  

   We may have arbitrarily complex addressing modes, but we get better
   cse of address expressions if we generate code with simple
   addressing modes and clean up redundant register operations later
   in the machine dependent reorg pass.  */

#define SRC_REG_P(X) \
 (REG_P(X) && !OUTGOING_REG_RTX_P (X))

#define SIMPLE_ADDRESS(X) \
 (SRC_REG_P(X) || CONSTANT_ADDRESS_P(X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)                          \
   if (SIMPLE_ADDRESS(X)) goto LABEL;                                     \
   if ((GET_CODE (X) == POST_INC                                          \
       || GET_CODE (X) == PRE_INC                                         \
       || GET_CODE (X) == POST_DEC                                        \
       || GET_CODE (X) == PRE_DEC) && SRC_REG_P(XEXP (X, 0))) goto LABEL; \

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)                        \
{                                                                       \
  if (GET_CODE(ADDR) == PRE_DEC || GET_CODE(ADDR) == POST_INC           \
      || GET_CODE(ADDR) == PRE_INC || GET_CODE(ADDR) == POST_DEC)       \
    goto LABEL;                                                         \
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.  */

#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR  FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR  TRUNC_DIV_EXPR

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  1

/* The type of size_t unsigned int.  */
#define SIZE_TYPE "unsigned int"

/* Don't cse the address of the function being compiled.  */

#define NO_RECURSIVE_FUNCTION_CSE (!optimize_size)

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */

#define MOVE_MAX 4

/* Max number of bytes we want move_by_pieces to be able to copy
   efficiently.  */

#define MOVE_MAX_PIECES 4

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
/*#define WORD_REGISTER_OPERATIONS*/

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */

#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */

#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Define this if zero-extension is slow (more than one real
   instruction).  */

/* #define SLOW_ZERO_EXTEND  */

/* Nonzero if access to memory by bytes is no faster than for words.  */
#define SLOW_BYTE_ACCESS 1

#define INT_TYPE_SIZE           32

/* A C expression that is nonzero if on this machine the number of
   bits actually used for the count of a shift operation is equal to the
   number of bits needed to represent the size of the object being
   shifted.  */

#define SHIFT_COUNT_TRUNCATED 1

/* All integers have the same format so truncation is easy.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */

#define NO_FUNCTION_CSE (!optimize_size)

/* Chars and shorts should be passed as ints.  */

#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions.  */

#define Pmode  SImode
#define FUNCTION_MODE  Pmode


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

#define CONST_COSTS(RTX,CODE,OUTER_CODE)                        \
  case CONST_INT:                                               \
    return INTVAL (RTX) >= -1 && INTVAL (RTX) <= 5 ?  1         \
         : INTVAL (RTX) >= -32768 && INTVAL (RTX) <= 32767 ? 2  \
         : 3;                                                   \
  case CONST:                                                   \
  case LABEL_REF:                                               \
  case SYMBOL_REF:                                              \
    return 3;                                                   \
  case CONST_DOUBLE:                                            \
   return pj_standard_float_constant (RTX) ? 1 : 4;             \

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained. */

#define RTX_COSTS(X,CODE,OUTER_CODE)                                    \
  case MULT:                                                            \
    if (GET_CODE (XEXP (X, 1)) == CONST_INT)                            \
      {                                                                 \
        unsigned HOST_WIDE_INT value = INTVAL (XEXP (X, 1));            \
        int nbits = 0;                                                  \
                                                                        \
        while (value != 0)                                              \
          {                                                             \
            nbits++;                                                    \
            value >>= 1;                                                \
          }                                                             \
                                                                        \
          total = COSTS_N_INSNS (nbits);                                \
      }                                                                 \
    else                                                                \
      total = COSTS_N_INSNS (10);                                       \
    break;

/* Compute extra cost of moving data between one register class and
   another.  */

#define REGISTER_MOVE_COST(SRC_CLASS, DST_CLASS)                \
      ((SRC_CLASS == STD_REGS || SRC_CLASS == ARG_REGS)?  2 : 10)


/* Assembler output control.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START "!"

/* The text to go at the start of the assembler file.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)                                                 \
  fprintf (FILE,"\t.file\t\"%s\"\n", main_input_filename);                   \
  fprintf (FILE,"\t! %s\n", TARGET_LITTLE_ENDIAN ? ".little" : ".big");      \
  fprintf (FILE,"\t.align 4\n");

#define ASM_LONG ".long"
#define ASM_APP_ON              ""
#define ASM_APP_OFF             ""
#define FILE_ASM_OP             "\t.file\n"

#define SET_ASM_OP              ".set"

/* How to change between sections.  */

#define TEXT_SECTION_ASM_OP             "\t.text"
#define DATA_SECTION_ASM_OP             "\t.data"

/* This special macro is used to output the asm pseduo op which allows
   the linker to fixup broken calling conentions.  */

#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, FNNAME)                        \
do { fputs (current_function_varargs || current_function_stdarg         \
            ? "\t.varargs_words_needed\t" : "\t.words_needed\t",        \
            FILE);                                                      \
     assemble_name (FILE, FNNAME);                                      \
     fprintf (FILE, ", %d\n", current_function_args_info.named_words);  \
   } while (0)

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized G data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */

#define BSS_SECTION_ASM_OP      ".section\t.bss"

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used
   in place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   Try to use function `asm_output_aligned_bss' defined in file
   `varasm.c' when defining this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)


/* Define this so that jump tables go in same section as the current function,
   which could be text or it could be a user defined section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* The assembler's names for the registers.  */

#define REGISTER_NAMES                                          \
{                                                               \
   "r0", "r1",  "r2",  "r3",  "r4",  "r5",  "r6", "r7",         \
   "r8", "r9",  "r10", "r11", "r12", "r13", "r14","r15",        \
   "r16","r17", "r18", "r19", "r20", "r21", "r22","r23",        \
   "r24","r25", "r26", "r27", "r28", "r29", "r30","r31",        \
                                                                \
   "i0", "i1",  "i2",  "i3",  "i4",  "i5",  "i6", "i7",         \
   "i8", "i9",  "i10", "i11", "i12", "i13", "i14","i15",        \
   "i16","i17", "i18", "i19", "i20", "i21", "i22","i23",        \
   "i24","i25", "i26", "i27", "i28", "i29", "i30","i31",        \
                                                                \
   "global0",   "global1",    "global2",    "global3",          \
   "global4",   "global5",    "global6",    "global7",          \
   "vars",      "optop",      "sc",         "pc",               \
   "ticks",     "slow",       "va",         "d3",               \
   "d4",        "d5",         "d6",         "ap",               \
   "p0",        "p1",         "p2",         "p3",               \
   "q0", "q1",  "q2",  "q3",  "q4",  "q5",  "q6", "q7",         \
                                                                \
   "o0", "o1",  "o2",  "o3",  "o4",  "o5",  "o6", "o7",         \
   "o8", "o9",  "o10", "o11", "o12", "o13", "o14","o15",        \
   "o16","o17", "o18", "o19", "o20", "o21", "o22","o23",        \
   "o24","o25", "o26", "o27", "o28", "o29", "o30","o31"}        \


/* Output a label definition.  */

#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name ((FILE), (NAME)); fputs (":\n", (FILE)); } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)      	\
  if ((LOG) != 0)                       	\
    fprintf ((FILE), "\t.align %d\n", (LOG))

/* Output a globalising directive for a label.  */

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)        \
  (fprintf ((STREAM), "\t.global\t"),           \
   assemble_name ((STREAM), (NAME)),            \
   fputc ('\n', (STREAM)))

/* After an opcode has been printed, there's nothing on the line any
   more.  */

#define ASM_OUTPUT_OPCODE(STREAM, P)  		\
   pj_stuff_on_line = 0;

/* The prefix to add to user-visible assembler symbols.  */

//#define USER_LABEL_PREFIX ""

/* The prefix to add to an internally generated label.  */

//#define LOCAL_LABEL_PREFIX ""

/* Construct a private name.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)     \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),     \
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output a relative address table.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)                 \
      asm_fprintf ((STREAM), "\t.long\t.L%d-.L%di\n", (VALUE),(REL));

#define ADDR_VEC_ALIGN(VEC) 0

/* Output various types of constants.  */

/* This is how to output an assembler line defining a `double'.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)                   \
do { char dstr[30];                                     \
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);    \
     fprintf ((FILE), "\t.double %s\n", dstr);          \
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)                    \
do { char dstr[30];                                     \
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);    \
     fprintf ((FILE), "\t.float %s\n", dstr);           \
   } while (0)

#define ASM_OUTPUT_INT(STREAM, EXP)             \
  (fprintf ((STREAM), "\t.long\t"),             \
   output_addr_const ((STREAM), (EXP)),         \
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_SHORT(STREAM, EXP)   \
  (fprintf ((STREAM), "\t.short\t"),    \
   output_addr_const ((STREAM), (EXP)), \
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_CHAR(STREAM, EXP)            \
  (fprintf ((STREAM), "\t.byte\t"),             \
   output_addr_const ((STREAM), (EXP)),         \
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_BYTE(STREAM, VALUE)          \
  fprintf ((STREAM), "\t.byte\t%d\n", (VALUE))  

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)    \
( fputs ("\t.comm ", (FILE)),                   	\
  assemble_name ((FILE), (NAME)),               	\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)     \
( fputs ("\t.lcomm ", (FILE)),                          \
  assemble_name ((FILE), (NAME)),                       \
  fprintf ((FILE), ",%d\n", (SIZE)))

/* The assembler's parentheses characters.  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* We don't want the default switch handling.  */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#undef ASM_OUTPUT_CASE_LABEL

/* Target characters.  */
#define TARGET_BELL     007
#define TARGET_BS       010
#define TARGET_TAB      011
#define TARGET_NEWLINE  012
#define TARGET_VT       013
#define TARGET_FF       014
#define TARGET_CR       015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or star or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(STREAM, X, CODE)  pj_print_operand ((STREAM), (X), (CODE))

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(STREAM,X)  output_addr_const (STREAM, X)

/* Punctuation valid for print_operand.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '*')


/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Since picoJava doesn't have unsigned compares, prefer signed
   arithmetic.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)             \
 if (GET_MODE_CLASS (MODE) == MODE_INT                  \
     && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)          \
     {                                                  \
       (MODE) = SImode;                                 \
       (UNSIGNEDP) = 0;                                 \
      }

/* Defining PROMOTE_FUNCTION_ARGS eliminates some unnecessary zero/sign
   extensions applied to char/short functions arguments.  Defining
   PROMOTE_FUNCTION_RETURN does the same for function returns.  */
#define PROMOTE_FUNCTION_ARGS

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT


/* We can debug without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP 

/* How to renumber registers for dbx and gdb.  */
extern short pj_debugreg_renumber_vec[];

#define DBX_REGISTER_NUMBER(REG) (pj_debugreg_renumber_vec[REG])

#define DONT_USE_BUILTIN_SETJMP

/* We prefer to use dwarf2. */
#undef  PREFERRED_DEBUGGING_TYPE 
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_UNWIND_INFO 1
#define DWARF_LINE_MIN_INSTR_LENGTH 1


/* varargs and stdarg builtins.  */

#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg)                      \
do {                                                                          \
   tree t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,                   \
                   make_tree (ptr_type_node, gen_rtx_REG (Pmode, VA_REG)));   \
   TREE_SIDE_EFFECTS (t) = 1;                                                 \
   expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);                      \
    } while (0)


#define EXPAND_BUILTIN_VA_ARG(valist, type)                             \
   pj_expand_builtin_va_arg(valist, type)

#define EXPAND_BUILTIN_NEXT_ARG(OFFSET)                                 \
   gen_rtx_MEM (Pmode,                                                  \
                plus_constant (gen_rtx_REG (SImode, VARS_REG),          \
                               (INTVAL (offset) + 1) * -4));     

/* Before the prologue, the return address is just above optop.  */
#define INCOMING_RETURN_ADDR_RTX  \
  plus_constant (gen_rtx_REG (Pmode, OPTOP_REG), 4)

/* Use thunks for vtables.  */
#define DEFAULT_VTABLE_THUNKS 1

/* Rewrite the rtl to use take advantage of the opstack.  */
#define MACHINE_DEPENDENT_REORG(INSNS) pj_machine_dependent_reorg(INSNS)


/* Define the codes that are matched by predicates in pj.c.  */
#define PREDICATE_CODES 						 \
  {"pj_dest_operand",                 {SUBREG, REG, MEM,}},              \
  {"pj_signed_comparison_operator",   {EQ, NE, LE, LT, GE, GT}},         \
  {"pj_unsigned_comparison_operator", {LEU, LTU, GEU, GTU}},             \
  {"pj_source_operand",               {CONST_INT, CONST_DOUBLE, CONST,   \
                                       SYMBOL_REF, LABEL_REF, SUBREG,    \
                                       REG, MEM}},

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS
