/* Definitions of target machine for GNU compiler.  AT&T DSP1600.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Michael Collison (collison@isisinc.net).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern const char *low_reg_names[];
extern const char *text_seg_name;
extern const char *rsect_text;
extern const char *data_seg_name;
extern const char *rsect_data;
extern const char *bss_seg_name;
extern const char *rsect_bss;
extern const char *const_seg_name;
extern const char *rsect_const;
extern const char *chip_name;
extern const char *save_chip_name;
extern GTY(()) rtx dsp16xx_compare_op0;
extern GTY(()) rtx dsp16xx_compare_op1;
extern GTY(()) rtx dsp16xx_addhf3_libcall;
extern GTY(()) rtx dsp16xx_subhf3_libcall;
extern GTY(()) rtx dsp16xx_mulhf3_libcall;
extern GTY(()) rtx dsp16xx_divhf3_libcall;
extern GTY(()) rtx dsp16xx_cmphf3_libcall;
extern GTY(()) rtx dsp16xx_fixhfhi2_libcall;
extern GTY(()) rtx dsp16xx_floathihf2_libcall;
extern GTY(()) rtx dsp16xx_neghf2_libcall;
extern GTY(()) rtx dsp16xx_mulhi3_libcall;
extern GTY(()) rtx dsp16xx_udivqi3_libcall;
extern GTY(()) rtx dsp16xx_udivhi3_libcall;
extern GTY(()) rtx dsp16xx_divqi3_libcall;
extern GTY(()) rtx dsp16xx_divhi3_libcall;
extern GTY(()) rtx dsp16xx_modqi3_libcall;
extern GTY(()) rtx dsp16xx_modhi3_libcall;
extern GTY(()) rtx dsp16xx_umodqi3_libcall;
extern GTY(()) rtx dsp16xx_umodhi3_libcall;

extern GTY(()) rtx dsp16xx_ashrhi3_libcall;
extern GTY(()) rtx dsp16xx_ashlhi3_libcall;
extern GTY(()) rtx dsp16xx_lshrhi3_libcall;

/* RUN-TIME TARGET SPECIFICATION */
#define DSP16XX   1

/* Name of the AT&T assembler */

#define ASM_PROG "as1600"

/* Name of the AT&T linker */

#define LD_PROG "ld1600"

/* Define which switches take word arguments */
#define WORD_SWITCH_TAKES_ARG(STR)              \
  (!strcmp (STR, "ifile") ? 1 :                 \
   0)

#undef  CC1_SPEC
#define CC1_SPEC       "%{!O*:-O}"

/* Define this as a spec to call the AT&T assembler */

#define CROSS_ASM_SPEC   "%{!S:as1600 %a %i\n }"

/* Define this as a spec to call the AT&T linker */

#define CROSS_LINK_SPEC  "%{!c:%{!M:%{!MM:%{!E:%{!S:ld1600 %l %X %{o*} %{m} \
			%{r} %{s} %{t} %{u*} %{x}\
			%{!A:%{!nostdlib:%{!nostartfiles:%S}}} %{static:}\
			%{L*} %D %o %{!nostdlib:-le1600 %L -le1600}\
			%{!A:%{!nostdlib:%{!nostartfiles:%E}}}\n }}}}}"

/* Nothing complicated here, just link with libc.a under normal
   circumstances */
#define LIB_SPEC "-lc"

/* Specify the startup file to link with.  */
#define STARTFILE_SPEC "%{mmap1:m1_crt0.o%s}  \
%{mmap2:m2_crt0.o%s}                          \
%{mmap3:m3_crt0.o%s}                          \
%{mmap4:m4_crt0.o%s}                          \
%{!mmap*: %{!ifile*: m4_crt0.o%s} %{ifile*:     \
%ea -ifile option requires a -map option}}"

/* Specify the end file to link with */

#define ENDFILE_SPEC "%{mmap1:m1_crtn.o%s}  \
%{mmap2:m2_crtn.o%s}                          \
%{mmap3:m3_crtn.o%s}                          \
%{mmap4:m4_crtn.o%s}                          \
%{!mmap*: %{!ifile*: m4_crtn.o%s} %{ifile*:     \
%ea -ifile option requires a -map option}}"


/* Tell gcc where to look for the startfile */
/*#define STANDARD_STARTFILE_PREFIX   "/d1600/lib"*/

/* Tell gcc where to look for it's executables */
/*#define STANDARD_EXEC_PREFIX  "/d1600/bin"*/

/* Command line options to the AT&T assembler */
#define ASM_SPEC  "%{V} %{v:%{!V:-V}} %{g*:-g}"

/* Command line options for the AT&T linker */

#define LINK_SPEC "%{V} %{v:%{!V:-V}} %{minit:-i}  \
%{!ifile*:%{mmap1:m1_deflt.if%s}         \
          %{mmap2:m2_deflt.if%s}         \
          %{mmap3:m3_deflt.if%s}         \
          %{mmap4:m4_deflt.if%s}         \
          %{!mmap*:m4_deflt.if%s}}       \
%{ifile*:%*} %{r}"

/* Include path is determined from the environment variable */
#define INCLUDE_DEFAULTS     \
{                            \
  { 0, 0, 0, 0, 0 }          \
}

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("dsp1600");		\
      builtin_define_std ("DSP1600");		\
    }						\
  while (0)

#ifdef __MSDOS__
# define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("MSDOS");		\
    }						\
  while (0)
#else
# define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("dsp1610");		\
      builtin_define_std ("DSP1610");		\
    }						\
  while (0)
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

#define MASK_REGPARM         0x00000001    /* Pass parameters in registers */
#define MASK_NEAR_CALL       0x00000002    /* The call is on the same 4k page */
#define MASK_NEAR_JUMP       0x00000004    /* The jump is on the same 4k page */
#define MASK_BMU             0x00000008    /* Use the 'bmu' shift instructions */
#define MASK_MAP1            0x00000040    /* Link with map1 */
#define MASK_MAP2            0x00000080    /* Link with map2 */
#define MASK_MAP3            0x00000100    /* Link with map3 */
#define MASK_MAP4            0x00000200    /* Link with map4 */
#define MASK_YBASE_HIGH      0x00000400    /* The ybase register window starts high */
#define MASK_INIT	     0x00000800    /* Have the linker generate tables to
					      initialize data at startup */
#define MASK_RESERVE_YBASE   0x00002000    /* Reserved the ybase registers */
#define MASK_DEBUG           0x00004000	   /* Debugging turned on*/
#define MASK_SAVE_TEMPS      0x00008000    /* Save temps. option seen */

/* Compile passing first two args in regs 0 and 1.
   This exists only to test compiler features that will
   be needed for RISC chips.  It is not usable
   and is not intended to be usable on this cpu.  */
#define TARGET_REGPARM   (target_flags & MASK_REGPARM)

/* The call is on the same 4k page, so instead of loading
   the 'pt' register and branching, we can branch directly */

#define TARGET_NEAR_CALL (target_flags & MASK_NEAR_CALL)

/* The jump is on the same 4k page, so instead of loading
   the 'pt' register and branching, we can branch directly */

#define TARGET_NEAR_JUMP (target_flags & MASK_NEAR_JUMP)

/* Generate shift instructions to use the 1610 Bit Manipulation
   Unit.  */
#define TARGET_BMU (target_flags & MASK_BMU)

#define TARGET_YBASE_HIGH (target_flags & MASK_YBASE_HIGH)

/* Direct the linker to output extra info for initialized data */
#define TARGET_MASK_INIT (target_flags & MASK_INIT)

#define TARGET_INLINE_MULT (target_flags & MASK_INLINE_MULT)

/* Reserve the ybase registers *(0) - *(31) */
#define TARGET_RESERVE_YBASE (target_flags & MASK_RESERVE_YBASE)

/* We turn this option on internally after seeing "-g" */
#define TARGET_DEBUG            (target_flags & MASK_DEBUG)

/* We turn this option on internally after seeing "-save-temps */
#define TARGET_SAVE_TEMPS       (target_flags & MASK_SAVE_TEMPS)


/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */


#define TARGET_SWITCHES                                           \
  {				                                  \
    { "regparm",       MASK_REGPARM,                              \
      N_("Pass parameters in registers (default)") },             \
    { "no-regparm",   -MASK_REGPARM,	                          \
      N_("Don't pass parameters in registers") },                 \
    { "near-call",     MASK_NEAR_JUMP,                            \
      N_("Generate code for near calls") },                       \
    { "no-near-call", -MASK_NEAR_CALL,                            \
      N_("Don't generate code for near calls") },                 \
    { "near-jump",     MASK_NEAR_JUMP,                            \
      N_("Generate code for near jumps") },                       \
    { "no-near-jump", -MASK_NEAR_JUMP,                            \
      N_("Don't generate code for near jumps") },                 \
    { "bmu",           MASK_BMU,                                  \
      N_("Generate code for a bit-manipulation unit") },          \
    { "no-bmu",       -MASK_BMU,                                  \
      N_("Don't generate code for a bit-manipulation unit") },    \
    { "map1",          MASK_MAP1,                                 \
      N_("Generate code for memory map1") },                      \
    { "map2",          MASK_MAP2,                                 \
      N_("Generate code for memory map2") },                      \
    { "map3",          MASK_MAP3,                                 \
      N_("Generate code for memory map3") },                      \
    { "map4",          MASK_MAP4,                                 \
      N_("Generate code for memory map4") },                      \
    { "init",          MASK_INIT,                                 \
      N_("Ouput extra code for initialized data") },              \
    { "reserve-ybase", MASK_RESERVE_YBASE,                        \
      N_("Don't let reg. allocator use ybase registers") },       \
    { "debug",         MASK_DEBUG,                                \
      N_("Output extra debug info in Luxworks environment") },    \
    { "save-temporaries",    MASK_SAVE_TEMPS,                     \
      N_("Save temp. files in Luxworks environment") },           \
    { "",              TARGET_DEFAULT, ""}                        \
  }

/* Default target_flags if no switches are specified */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT  MASK_REGPARM|MASK_YBASE_HIGH
#endif

#define TARGET_OPTIONS						\
{								\
  { "text=",	&text_seg_name,				        \
    N_("Specify alternate name for text section"), 0},          \
  { "data=",	&data_seg_name,				        \
    N_("Specify alternate name for data section"), 0},          \
  { "bss=",	&bss_seg_name,				        \
    N_("Specify alternate name for bss section"), 0},           \
  { "const=",   &const_seg_name,                                \
    N_("Specify alternate name for constant section"), 0},      \
  { "chip=",    &chip_name,                                     \
    N_("Specify alternate name for dsp16xx chip"), 0},          \
}

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.
  
   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS override_options ()

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)              \
{                                                     \
    if (LEVEL >= 2)                                   \
    {                                                 \
        /* The dsp16xx family has so few registers    \
         * that running the first instruction         \
         * scheduling is bad for reg. allocation      \
         * since it increases lifetimes of pseudos.   \
         * So turn of first scheduling pass.          \
         */                                           \
        flag_schedule_insns          = FALSE;         \
    }                                                 \
}

/* STORAGE LAYOUT */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
 */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.
   We define big-endian, but since the 1600 series cannot address bytes
   it does not matter.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is numbered.
   For the 1600 we can decide arbitrarily since there are no machine instructions for them.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 16

/* Maximum number of bits in a word.  */
#define MAX_BITS_PER_WORD 16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 1

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 16

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Biggest alignment that any data type can require on this machine, in bits.  */
#define BIGGEST_ALIGNMENT 16

/* Biggest alignment that any structure field can require on this machine, in bits */
#define BIGGEST_FIELD_ALIGNMENT 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* Number of bits which any structure or union's size must be a multiple of. Each structure
   or union's size is rounded up to a multiple of this */
#define STRUCTURE_SIZE_BOUNDARY 16

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT  1

/* An integer expression for the size in bits of the largest integer machine mode that
   should actually be used. All integer machine modes of this size or smaller can be
   used for structures and unions with the appropriate sizes.  */
#define MAX_FIXED_MODE_SIZE 32

/* LAYOUT OF SOURCE LANGUAGE DATA TYPES */

#define SHORT_TYPE_SIZE        16
#define INT_TYPE_SIZE          16
#define LONG_TYPE_SIZE         32
#define LONG_LONG_TYPE_SIZE    32
#define FLOAT_TYPE_SIZE        32
#define DOUBLE_TYPE_SIZE       32
#define LONG_DOUBLE_TYPE_SIZE  32

/* An expression whose value is 1 or 0, according to whether the type char should be
   signed or unsigned by default.  */

#define DEFAULT_SIGNED_CHAR 1

/* A C expression to determine whether to give an enum type only as many bytes
   as it takes to represent the range of possible values of that type. A nonzero
   value means to do that; a zero value means all enum types should be allocated
   like int.  */

#define DEFAULT_SHORT_ENUMS 0

/* A C expression for a string describing the name of the data type to use for
   size values.  */

#define SIZE_TYPE    "unsigned int"

/* A C expression for a string describing the name of the data type to use for the
   result of subtracting two pointers */

#define PTRDIFF_TYPE "int"


/* REGISTER USAGE.  */

#define ALL_16_BIT_REGISTERS  1

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to FIRST_PSEUDO_REGISTER-1 */

#define FIRST_PSEUDO_REGISTER (REG_YBASE31 + 1)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   The registers are laid out as follows:

   {a0,a0l,a1,a1l,x,y,yl,p,pl} - Data Arithmetic Unit
   {r0,r1,r2,r3,j,k,ybase} - Y Space Address Arithmetic Unit
   {pt} - X Space Address Arithmetic Unit
   {ar0,ar1,ar2,ar3} - Bit Manipulation UNit
   {pr} - Return Address Register

   We reserve r2 for the Stack Pointer.
   We specify r3 for the Frame Pointer but allow the compiler
   to omit it when possible since we have so few pointer registers.  */

#define REG_A0     0
#define REG_A0L    1
#define REG_A1     2
#define REG_A1L    3 
#define REG_X      4
#define REG_Y      5
#define REG_YL     6
#define REG_PROD   7
#define REG_PRODL  8
#define REG_R0     9
#define REG_R1     10
#define REG_R2     11
#define REG_R3     12
#define REG_J      13
#define REG_K      14
#define REG_YBASE  15
#define REG_PT     16
#define REG_AR0    17
#define REG_AR1    18
#define REG_AR2    19
#define REG_AR3    20
#define REG_C0     21
#define REG_C1     22
#define REG_C2     23
#define REG_PR     24
#define REG_RB     25
#define REG_YBASE0 26
#define REG_YBASE1 27
#define REG_YBASE2 28
#define REG_YBASE3 29
#define REG_YBASE4 30
#define REG_YBASE5 31
#define REG_YBASE6 32
#define REG_YBASE7 33
#define REG_YBASE8 34
#define REG_YBASE9 35
#define REG_YBASE10 36
#define REG_YBASE11 37
#define REG_YBASE12 38
#define REG_YBASE13 39
#define REG_YBASE14 40
#define REG_YBASE15 41
#define REG_YBASE16 42
#define REG_YBASE17 43
#define REG_YBASE18 44
#define REG_YBASE19 45
#define REG_YBASE20 46
#define REG_YBASE21 47
#define REG_YBASE22 48
#define REG_YBASE23 49
#define REG_YBASE24 50
#define REG_YBASE25 51
#define REG_YBASE26 52
#define REG_YBASE27 53
#define REG_YBASE28 54
#define REG_YBASE29 55
#define REG_YBASE30 56
#define REG_YBASE31 57

/* Do we have an accumulator register? */
#define IS_ACCUM_REG(REGNO) IN_RANGE ((REGNO), REG_A0, REG_A1L)
#define IS_ACCUM_LOW_REG(REGNO) ((REGNO) == REG_A0L || (REGNO) == REG_A1L)

/* Do we have a virtual ybase register */
#define IS_YBASE_REGISTER_WINDOW(REGNO) ((REGNO) >= REG_YBASE0 && (REGNO) <= REG_YBASE31)

#define IS_YBASE_ELIGIBLE_REG(REGNO) (IS_ACCUM_REG (REGNO) || IS_ADDRESS_REGISTER(REGNO) \
                                      || REGNO == REG_X || REGNO == REG_Y || REGNO == REG_YL \
                                      || REGNO == REG_PROD || REGNO == REG_PRODL)

#define IS_ADDRESS_REGISTER(REGNO) ((REGNO) >= REG_R0 && (REGNO) <= REG_R3)

#define FIXED_REGISTERS     \
{0, 0, 0, 0, 0, 0, 0, 0, 0, \
 0, 0, 0, 1, 0, 0, 1,       \
 1,                         \
 0, 0, 0, 0,                \
 1, 1, 1,                   \
 1, 0,                      \
 0, 0, 0, 0, 0, 0, 0, 0,    \
 0, 0, 0, 0, 0, 0, 0, 0,    \
 0, 0, 0, 0, 0, 0, 0, 0,    \
 0, 0, 0, 0, 0, 0, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   On the 1610 'a0' holds return values from functions. 'r0' holds
   structure-value addresses.

   In addition we don't save either j, k, ybase or any of the
   bit manipulation registers.  */


#define CALL_USED_REGISTERS			\
{1, 1, 1, 1, 0, 1, 1, 1, 1,	/* 0-8 */	\
 1, 0, 0, 1, 1, 1, 1,		/* 9-15 */	\
 1,                             /* 16 */	\
 0, 0, 1, 1,			/* 17-20 */	\
 1, 1, 1,			/* 21-23 */	\
 1, 1,				/* 24-25 */	\
 0, 0, 0, 0, 0, 0, 0, 0,	/* 26-33 */	\
 0, 0, 0, 0, 0, 0, 0, 0,	/* 34-41 */	\
 0, 0, 0, 0, 0, 0, 0, 0,	/* 42-49 */	\
 0, 0, 0, 0, 0, 0, 0, 0}	/* 50-57 */

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
 */

#if 0
#define REG_ALLOC_ORDER					\
{ REG_R0, REG_R1, REG_R2, REG_PROD, REG_Y, REG_X,       \
  REG_PRODL, REG_YL, REG_AR0, REG_AR1,                  \
  REG_RB, REG_A0, REG_A1, REG_A0L,                      \
  REG_A1L, REG_AR2, REG_AR3,                            \
  REG_YBASE, REG_J, REG_K, REG_PR, REG_PT, REG_C0,      \
  REG_C1, REG_C2, REG_R3,				\
  REG_YBASE0, REG_YBASE1, REG_YBASE2, REG_YBASE3,       \
  REG_YBASE4, REG_YBASE5, REG_YBASE6, REG_YBASE7,       \
  REG_YBASE8, REG_YBASE9, REG_YBASE10, REG_YBASE11,     \
  REG_YBASE12, REG_YBASE13, REG_YBASE14, REG_YBASE15,   \
  REG_YBASE16, REG_YBASE17, REG_YBASE18, REG_YBASE19,   \
  REG_YBASE20, REG_YBASE21, REG_YBASE22, REG_YBASE23,   \
  REG_YBASE24, REG_YBASE25, REG_YBASE26, REG_YBASE27,   \
  REG_YBASE28, REG_YBASE29, REG_YBASE30, REG_YBASE31 }
#else
#define REG_ALLOC_ORDER                                 \
{                                                       \
  REG_A0, REG_A0L, REG_A1, REG_A1L, REG_Y, REG_YL,      \
  REG_PROD,                                             \
  REG_PRODL, REG_R0, REG_J, REG_K, REG_AR2, REG_AR3,    \
  REG_X, REG_R1, REG_R2, REG_RB, REG_AR0, REG_AR1,      \
  REG_YBASE0, REG_YBASE1, REG_YBASE2, REG_YBASE3,       \
  REG_YBASE4, REG_YBASE5, REG_YBASE6, REG_YBASE7,       \
  REG_YBASE8, REG_YBASE9, REG_YBASE10, REG_YBASE11,     \
  REG_YBASE12, REG_YBASE13, REG_YBASE14, REG_YBASE15,   \
  REG_YBASE16, REG_YBASE17, REG_YBASE18, REG_YBASE19,   \
  REG_YBASE20, REG_YBASE21, REG_YBASE22, REG_YBASE23,   \
  REG_YBASE24, REG_YBASE25, REG_YBASE26, REG_YBASE27,   \
  REG_YBASE28, REG_YBASE29, REG_YBASE30, REG_YBASE31,   \
  REG_R3, REG_YBASE, REG_PT, REG_C0, REG_C1, REG_C2,    \
  REG_PR }
#endif
/* Zero or more C statements that may conditionally modify two
   variables `fixed_regs' and `call_used_regs' (both of type `char
   []') after they have been initialized from the two preceding
   macros.

   This is necessary in case the fixed or call-clobbered registers
   depend on target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target
   flags, you may indicate this to GCC by using this macro to modify
   `fixed_regs' and `call_used_regs' to 1 for each of the registers in
   the classes which should not be used by GCC.  Also define the macro
   `REG_CLASS_FROM_LETTER' to return `NO_REGS' if it is called with a
   letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all
   of the insn patterns whose constraints permit this class are
   controlled by target switches, then GCC will automatically avoid
   using these registers when the target switches are opposed to
   them.)  If the user tells us there is no BMU, we can't use
   ar0-ar3 for register allocation */

#define CONDITIONAL_REGISTER_USAGE					\
do									\
  {									\
    if (!TARGET_BMU)						        \
      {									\
	int regno;							\
									\
	for (regno = REG_AR0; regno <= REG_AR3; regno++)	        \
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
    if (TARGET_RESERVE_YBASE)						\
      {									\
	int regno;							\
									\
	for (regno = REG_YBASE0; regno <= REG_YBASE31; regno++)	        \
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
  }									\
while (0)

/* Determine which register classes are very likely used by spill registers.
   local-alloc.c won't allocate pseudos that have these classes as their
   preferred class unless they are "preferred or nothing".  */

#define CLASS_LIKELY_SPILLED_P(CLASS) \
 ((CLASS) != ALL_REGS && (CLASS) != YBASE_VIRT_REGS)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)                                 \
  (GET_MODE_SIZE(MODE))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok(REGNO, MODE)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)		     \
  (((MODE1) == (MODE2)) ||                           \
   (GET_MODE_CLASS((MODE1)) == MODE_FLOAT)           \
    == (GET_MODE_CLASS((MODE2)) == MODE_FLOAT))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* DSP1600 pc isn't overloaded on a register.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  
   This is r3 in our case */
#define STACK_POINTER_REGNUM  REG_R3

/* Base register for access to local variables of the function.
   This is r2 in our case   */
#define FRAME_POINTER_REGNUM  REG_R2

/* We can debug without the frame pointer */
#define CAN_DEBUG_WITHOUT_FP 1

/* The 1610 saves the return address in this register */
#define RETURN_ADDRESS_REGNUM REG_PR

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  */

#define STATIC_CHAIN_REGNUM 4

/* Register in which address to store a structure value
   is passed to a function.  This is 'r0' in our case */
#define STRUCT_VALUE_REGNUM   REG_R0

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
    A0H_REG,
    A0L_REG,
    A0_REG,
    A1H_REG,
    ACCUM_HIGH_REGS,
    A1L_REG,
    ACCUM_LOW_REGS, 
    A1_REG,
    ACCUM_REGS, 
    X_REG, 
    X_OR_ACCUM_LOW_REGS,
    X_OR_ACCUM_REGS,
    YH_REG,
    YH_OR_ACCUM_HIGH_REGS,
    X_OR_YH_REGS,
    YL_REG,
    YL_OR_ACCUM_LOW_REGS,
    X_OR_YL_REGS,
    X_OR_Y_REGS,
    Y_REG,
    ACCUM_OR_Y_REGS,
    PH_REG,
    X_OR_PH_REGS, 
    PL_REG, 
    PL_OR_ACCUM_LOW_REGS,
    X_OR_PL_REGS,
    YL_OR_PL_OR_ACCUM_LOW_REGS,
    P_REG,
    ACCUM_OR_P_REGS,
    YL_OR_P_REGS,
    ACCUM_LOW_OR_YL_OR_P_REGS,
    Y_OR_P_REGS,
    ACCUM_Y_OR_P_REGS, 
    NO_FRAME_Y_ADDR_REGS,
    Y_ADDR_REGS, 
    ACCUM_LOW_OR_Y_ADDR_REGS,
    ACCUM_OR_Y_ADDR_REGS,
    X_OR_Y_ADDR_REGS,
    Y_OR_Y_ADDR_REGS,
    P_OR_Y_ADDR_REGS,
    NON_HIGH_YBASE_ELIGIBLE_REGS,
    YBASE_ELIGIBLE_REGS,
    J_REG,
    J_OR_DAU_16_BIT_REGS,
    BMU_REGS, 
    NOHIGH_NON_ADDR_REGS,
    NON_ADDR_REGS,
    SLOW_MEM_LOAD_REGS,
    NOHIGH_NON_YBASE_REGS,
    NO_ACCUM_NON_YBASE_REGS,
    NON_YBASE_REGS,
    YBASE_VIRT_REGS,
    ACCUM_LOW_OR_YBASE_REGS,
    ACCUM_OR_YBASE_REGS,
    X_OR_YBASE_REGS,
    Y_OR_YBASE_REGS,
    ACCUM_LOW_YL_PL_OR_YBASE_REGS,
    P_OR_YBASE_REGS,
    ACCUM_Y_P_OR_YBASE_REGS,
    Y_ADDR_OR_YBASE_REGS,
    YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS,
    YBASE_OR_YBASE_ELIGIBLE_REGS,
    NO_HIGH_ALL_REGS,
    ALL_REGS, 
    LIM_REG_CLASSES 
};

/* GENERAL_REGS must be the name of a register class */
#define GENERAL_REGS ALL_REGS

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES        \
{                              \
    "NO_REGS",                 \
    "A0H_REG",                 \
    "A0L_REG",                 \
    "A0_REG",                  \
    "A1H_REG",                 \
    "ACCUM_HIGH_REGS",         \
    "A1L_REG",                 \
    "ACCUM_LOW_REGS",          \
    "A1_REG",                  \
    "ACCUM_REGS",              \
    "X_REG",                   \
    "X_OR_ACCUM_LOW_REGS",     \
    "X_OR_ACCUM_REGS",         \
    "YH_REG",                  \
    "YH_OR_ACCUM_HIGH_REGS",   \
    "X_OR_YH_REGS",            \
    "YL_REG",                  \
    "YL_OR_ACCUM_LOW_REGS",    \
    "X_OR_YL_REGS",            \
    "X_OR_Y_REGS",             \
    "Y_REG",                   \
    "ACCUM_OR_Y_REGS",         \
    "PH_REG",                  \
    "X_OR_PH_REGS",            \
    "PL_REG",                  \
    "PL_OR_ACCUM_LOW_REGS",    \
    "X_OR_PL_REGS",            \
    "PL_OR_YL_OR_ACCUM_LOW_REGS", \
    "P_REG",                   \
    "ACCUM_OR_P_REGS",         \
    "YL_OR_P_REGS",            \
    "ACCUM_LOW_OR_YL_OR_P_REGS", \
    "Y_OR_P_REGS",             \
    "ACCUM_Y_OR_P_REGS",       \
    "NO_FRAME_Y_ADDR_REGS",      \
    "Y_ADDR_REGS",               \
    "ACCUM_LOW_OR_Y_ADDR_REGS",  \
    "ACCUM_OR_Y_ADDR_REGS",    \
    "X_OR_Y_ADDR_REGS",        \
    "Y_OR_Y_ADDR_REGS",        \
    "P_OR_Y_ADDR_REGS",        \
    "NON_HIGH_YBASE_ELIGIBLE_REGS", \
    "YBASE_ELIGIBLE_REGS",     \
    "J_REG",                   \
    "J_OR_DAU_16_BIT_REGS",    \
    "BMU_REGS",                \
    "NOHIGH_NON_ADDR_REGS",    \
    "NON_ADDR_REGS",           \
    "SLOW_MEM_LOAD_REGS",      \
    "NOHIGH_NON_YBASE_REGS",   \
    "NO_ACCUM_NON_YBASE_REGS", \
    "NON_YBASE_REGS",          \
    "YBASE_VIRT_REGS",         \
    "ACCUM_LOW_OR_YBASE_REGS", \
    "ACCUM_OR_YBASE_REGS",     \
    "X_OR_YBASE_REGS",         \
    "Y_OR_YBASE_REGS",         \
    "ACCUM_LOW_YL_PL_OR_YBASE_REGS", \
    "P_OR_YBASE_REGS",         \
    "ACCUM_Y_P_OR_YBASE_REGS", \
    "Y_ADDR_OR_YBASE_REGS",    \
    "YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS", \
    "YBASE_OR_YBASE_ELIGIBLE_REGS", \
    "NO_HIGH_ALL_REGS",        \
    "ALL_REGS"                 \
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS   \
{                            \
    {0x00000000,  0x00000000},      /* no reg */                             \
    {0x00000001,  0x00000000},      /* a0h */                                \
    {0x00000002,  0x00000000},      /* a0l */                                \
    {0x00000003,  0x00000000},      /* a0h:a0l */                            \
    {0x00000004,  0x00000000},      /* a1h */                                \
    {0x00000005,  0x00000000},      /* accum high */                         \
    {0x00000008,  0x00000000},      /* a1l */                                \
    {0x0000000A,  0x00000000},      /* accum low */                          \
    {0x0000000c,  0x00000000},      /* a1h:a1l */                            \
    {0x0000000f,  0x00000000},      /* accum regs */                         \
    {0x00000010,  0x00000000},      /* x reg */                              \
    {0x0000001A,  0x00000000},      /* x & accum_low_regs */                 \
    {0x0000001f,  0x00000000},      /* x & accum regs */                     \
    {0x00000020,  0x00000000},      /* y high */                             \
    {0x00000025,  0x00000000},      /* yh, accum high */                     \
    {0x00000030,  0x00000000},      /* x & yh */                             \
    {0x00000040,  0x00000000},      /* y low */                              \
    {0x0000004A,  0x00000000},      /* y low, accum_low */                   \
    {0x00000050,  0x00000000},      /* x & yl */                             \
    {0x00000060,  0x00000000},      /* yl:yh */                              \
    {0x00000070,  0x00000000},      /* x, yh,a nd yl */                      \
    {0x0000006F,  0x00000000},      /* accum, y */                           \
    {0x00000080,  0x00000000},      /* p high */                             \
    {0x00000090,  0x00000000},      /* x & ph */                             \
    {0x00000100,  0x00000000},      /* p low */                              \
    {0x0000010A,  0x00000000},      /* p_low and accum_low */                \
    {0x00000110,  0x00000000},      /* x & pl */                             \
    {0x0000014A,  0x00000000},      /* pl,yl,a1l,a0l */                      \
    {0x00000180,  0x00000000},      /* pl:ph */                              \
    {0x0000018F,  0x00000000},      /* accum, p */                           \
    {0x000001C0,  0x00000000},      /* pl:ph and yl */                       \
    {0x000001CA,  0x00000000},      /* pl:ph, yl, a0l, a1l */                \
    {0x000001E0,  0x00000000},      /* y or p */                             \
    {0x000001EF,  0x00000000},      /* accum, y or p */                      \
    {0x00000E00,  0x00000000},      /* r0-r2 */                              \
    {0x00001E00,  0x00000000},      /* r0-r3 */                              \
    {0x00001E0A,  0x00000000},      /* r0-r3, accum_low */                   \
    {0x00001E0F,  0x00000000},      /* accum,r0-r3 */                        \
    {0x00001E10,  0x00000000},      /* x,r0-r3 */                            \
    {0x00001E60,  0x00000000},      /* y,r0-r3 */                            \
    {0x00001F80,  0x00000000},      /* p,r0-r3 */                            \
    {0x00001FDA,  0x00000000},      /* ph:pl, r0-r3, x,a0l,a1l */            \
    {0x00001fff,  0x00000000},      /* accum,x,y,p,r0-r3 */                  \
    {0x00002000,  0x00000000},      /* j */                                  \
    {0x00002025,  0x00000000},      /* j, yh, a1h, a0h */                    \
    {0x001E0000,  0x00000000},      /* ar0-ar3 */                            \
    {0x03FFE1DA,  0x00000000},      /* non_addr except yh,a0h,a1h */         \
    {0x03FFE1FF,  0x00000000},      /* non_addr regs */                      \
    {0x03FFFF8F,  0x00000000},      /* non ybase except yh, yl, and x */     \
    {0x03FFFFDA,  0x00000000},      /* non ybase regs except yh,a0h,a1h */   \
    {0x03FFFFF0,  0x00000000},      /* non ybase except a0,a0l,a1,a1l */     \
    {0x03FFFFFF,  0x00000000},      /* non ybase regs */                     \
    {0xFC000000,  0x03FFFFFF},      /* virt ybase regs */                    \
    {0xFC00000A,  0x03FFFFFF},      /* accum_low, virt ybase regs */         \
    {0xFC00000F,  0x03FFFFFF},      /* accum, virt ybase regs */             \
    {0xFC000010,  0x03FFFFFF},      /* x,virt ybase regs */                  \
    {0xFC000060,  0x03FFFFFF},      /* y,virt ybase regs */                  \
    {0xFC00014A,  0x03FFFFFF},      /* accum_low, yl, pl, ybase */           \
    {0xFC000180,  0x03FFFFFF},      /* p,virt ybase regs */                  \
    {0xFC0001EF,  0x03FFFFFF},      /* accum,y,p,ybase regs */               \
    {0xFC001E00,  0x03FFFFFF},      /* r0-r3, ybase regs */                  \
    {0xFC001FDA,  0x03FFFFFF},      /* r0-r3, pl:ph,yl,x,a1l,a0l */          \
    {0xFC001FFF,  0x03FFFFFF},      /* virt ybase, ybase eligible regs */    \
    {0xFCFFFFDA,  0x03FFFFFF},      /* all regs except yh,a0h,a1h */         \
    {0xFFFFFFFF,  0x03FFFFFF}       /* all regs */                           \
}


/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) regno_reg_class(REGNO)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS  Y_ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  dsp16xx_reg_class_from_letter(C)

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)  \
   secondary_reload_class(CLASS, MODE, X)

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */

#define SMALL_REGISTER_CLASSES 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

/* A C expression which is nonzero if register REGNO is suitable for use
   as a base register in operand addresses. It may be either a suitable
   hard register or a pseudo register that has been allocated such a
   hard register. 

  On the 1610 the Y address pointers can be used as a base registers */
#define REGNO_OK_FOR_BASE_P(REGNO) \
(((REGNO) >= REG_R0 && (REGNO) < REG_R3 + 1) || ((unsigned) reg_renumber[REGNO] >= REG_R0  \
                                   && (unsigned) reg_renumber[REGNO] < REG_R3 + 1))

#define REGNO_OK_FOR_YBASE_P(REGNO) \
  (((REGNO) == REG_YBASE) || ((unsigned) reg_renumber[REGNO] == REG_YBASE))

#define REGNO_OK_FOR_INDEX_P(REGNO)  0

#ifdef ALL_16_BIT_REGISTERS
#define IS_32_BIT_REG(REGNO)  0
#else
#define IS_32_BIT_REG(REGNO)     \
  ((REGNO) == REG_A0 || (REGNO) == REG_A1 || (REGNO) == REG_Y || (REGNO) == REG_PROD)
#endif

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   Also, we must ensure that a PLUS is reloaded either
   into an accumulator or an address register.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)	 preferred_reload_class (X, CLASS)

/*   A C expression that places additional restrictions on the register
     class to use when it is necessary to be able to hold a value of
     mode MODE in a reload register for which class CLASS would
     ordinarily be used.

     Unlike `PREFERRED_RELOAD_CLASS', this macro should be used when
     there are certain modes that simply can't go in certain reload
     classes.

     The value is a register class; perhaps CLASS, or perhaps another,
     smaller class.

     Don't define this macro unless the target machine has limitations
     which require the macro to do something nontrivial.  */

#if 0
#define LIMIT_RELOAD_CLASS(MODE, CLASS) dsp16xx_limit_reload_class (MODE, CLASS)
#endif

/* A C expression for the maximum number of consecutive registers of class CLASS
   needed to hold a value of mode MODE */
#define CLASS_MAX_NREGS(CLASS, MODE)                                \
    class_max_nregs(CLASS, MODE)

/* The letters 'I' through 'P' in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the 16xx, the following constraints are used:
   'I' requires a non-negative 16-bit value.
   'J' requires a non-negative 9-bit value
   'K' requires a constant 0 operand.
   'L' constant for use in add or sub from low 16-bits
   'M' 32-bit value -- low 16-bits zero
   'N' constant for use incrementing or decrementing an address register
   'O' constant for use with and'ing only high 16-bit
   'P' constant for use with and'ing only low 16-bit
 */

#define SMALL_INT(X) (SMALL_INTVAL (INTVAL (X)))
#define SMALL_INTVAL(I) ((unsigned) (I) < 0x10000)
#define SHORT_IMMEDIATE(X)  (SHORT_INTVAL (INTVAL(X)))
#define SHORT_INTVAL(I)     ((unsigned) (I) < 0x100)
#define ADD_LOW_16(I)       ((I) >= 0 && (I) <= 32767)
#define ADD_HIGH_16(I)      (((I) & 0x0000ffff) == 0)
#define AND_LOW_16(I)       ((I) >= 0 && (I) <= 32767)
#define AND_HIGH_16(I)      (((I) & 0x0000ffff) == 0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)                           \
   ((C) == 'I' ? (SMALL_INTVAL(VALUE))                            \
    : (C) == 'J' ? (SHORT_INTVAL(VALUE))                          \
    : (C) == 'K' ? ((VALUE) == 0)                                 \
    : (C) == 'L' ? ((VALUE) >= 0 && (VALUE) <= 32767)             \
    : (C) == 'M' ? (((VALUE) & 0x0000ffff) == 0)                  \
    : (C) == 'N' ? ((VALUE) == -1 || (VALUE) == 1                 \
                    || (VALUE) == -2 || (VALUE) == 2)             \
    : (C) == 'O' ? (((VALUE) & 0xffff0000) == 0xffff0000)         \
    : (C) == 'P' ? (((VALUE) & 0x0000ffff) == 0xffff)             \
    : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)   1

/* Optional extra constraints for this machine */
#define EXTRA_CONSTRAINT(OP,C)                                    \
  ((C) == 'R' ? symbolic_address_p (OP)                           \
   : 0)

/* DESCRIBING STACK LAYOUT AND CALLING CONVENTIONS */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

#define ARGS_GROW_DOWNWARD

/* We use post decrement on the 1600 because there isn't
   a pre-decrement addressing mode. This means that we
   assume the stack pointer always points at the next
   FREE location on the stack.  */
#define STACK_PUSH_CODE POST_INC

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET  0

/* Offset from the stack pointer register to the first
   location at which outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET (0)

struct dsp16xx_frame_info
{
  unsigned long total_size;	/* # bytes that the entire frame takes up */
  unsigned long var_size;	/* # bytes that variables take up */
  unsigned long args_size;	/* # bytes that outgoing arguments take up */
  unsigned long extra_size;	/* # bytes of extra gunk */
  unsigned int  reg_size;	/* # bytes needed to store regs */
  long		fp_save_offset;	/* offset from vfp to store registers */
  unsigned long sp_save_offset;	/* offset from new sp to store registers */
  int		pr_save_offset;	/* offset to saved PR */
  int		initialized;	/* != 0 if frame size already calculated */
  int		num_regs;	/* number of registers saved */
  int           function_makes_calls;  /* Does the function make calls */
};

extern struct dsp16xx_frame_info current_frame_info;

#define RETURN_ADDR_OFF current_frame_info.pr_save_offset

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* #define PUSH_ROUNDING(BYTES) ((BYTES)) */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   'current_function_outgoing_args_size'. No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.

   It is not proper to define both 'PUSH_ROUNDING' and
   'ACCUMULATE_OUTGOING_ARGS'.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer
   register value.  */

#define FIRST_PARM_OFFSET(FNDECL)   (0)

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0. On the 1610 all function return their values
   in a0 (i.e. the upper 16 bits). If the return value is 32-bits the
   entire register is significant.  */

#define VALUE_REGNO(MODE)  (REG_Y)

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), VALUE_REGNO(TYPE_MODE(VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, VALUE_REGNO(MODE))

/* 1 if N is a possible register number for a function value.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == REG_Y)


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

/* On the 1610 all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in a0, a1.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  dsp16xx_function_arg (CUM, MODE, TYPE, NAMED)

/* Define the first register to be used for argument passing */
#define FIRST_REG_FOR_FUNCTION_ARG REG_Y

/* Define the profitability of saving registers around calls.
   NOTE: For now we turn this off because of a bug in the
   caller-saves code and also because i'm not sure it is helpful
   on the 1610.  */

#define CALLER_SAVE_PROFITABLE(REFS,CALLS) 0

/* This indicates that an argument is to be passed with an invisible reference
   (i.e., a pointer to the object is passed).

   On the dsp16xx, we do this if it must be passed on the stack.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)	\
  (MUST_PASS_IN_STACK (MODE, TYPE))

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)  (0)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  dsp16xx_function_arg_advance (&CUM, MODE,TYPE, NAMED)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N)   \
  ((N) == REG_Y || (N) == REG_YL || (N) == REG_PROD || (N) == REG_PRODL)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)        \
  internal_error ("profiling not implemented yet")

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK  (0)

#define TRAMPOLINE_TEMPLATE(FILE) \
  internal_error ("trampolines not yet implemented");

/* Length in units of the trampoline for entering a nested function.
   This is a dummy value  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
  internal_error ("trampolines not yet implemented");

/* A C expression which is nonzero if a function must have and use a
   frame pointer. If its value is nonzero the functions will have a
   frame pointer.  */
#define FRAME_POINTER_REQUIRED  (current_function_calls_alloca)

/* A C statement to store in the variable 'DEPTH' the difference
   between the frame pointer and the stack pointer values immediately
   after the function prologue.  */
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH)                     \
{  (DEPTH) = initial_frame_pointer_offset();	                \
}

/* IMPLICIT CALLS TO LIBRARY ROUTINES */

#define ADDHF3_LIBCALL      "__Emulate_addhf3"
#define SUBHF3_LIBCALL      "__Emulate_subhf3"
#define MULHF3_LIBCALL      "__Emulate_mulhf3"
#define DIVHF3_LIBCALL      "__Emulate_divhf3"
#define CMPHF3_LIBCALL      "__Emulate_cmphf3"
#define FIXHFHI2_LIBCALL    "__Emulate_fixhfhi2"
#define FLOATHIHF2_LIBCALL  "__Emulate_floathihf2"
#define NEGHF2_LIBCALL      "__Emulate_neghf2"

#define UMULHI3_LIBCALL     "__Emulate_umulhi3"
#define MULHI3_LIBCALL      "__Emulate_mulhi3"
#define UDIVQI3_LIBCALL     "__Emulate_udivqi3"
#define UDIVHI3_LIBCALL     "__Emulate_udivhi3"
#define DIVQI3_LIBCALL      "__Emulate_divqi3"
#define DIVHI3_LIBCALL      "__Emulate_divhi3"
#define MODQI3_LIBCALL      "__Emulate_modqi3"
#define MODHI3_LIBCALL      "__Emulate_modhi3"
#define UMODQI3_LIBCALL     "__Emulate_umodqi3"
#define UMODHI3_LIBCALL     "__Emulate_umodhi3"
#define ASHRHI3_LIBCALL     "__Emulate_ashrhi3"
#define LSHRHI3_LIBCALL     "__Emulate_lshrhi3"
#define ASHLHI3_LIBCALL     "__Emulate_ashlhi3"
#define LSHLHI3_LIBCALL     "__Emulate_lshlhi3"   /* NOT USED */

/* Define this macro if calls to the ANSI C library functions memcpy and
   memset should be generated instead of the BSD function bcopy & bzero.  */
#define TARGET_MEM_FUNCTIONS


/* ADDRESSING MODES */

/* The 1610 has post-increment and decrement, but no pre-modify */
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

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
#define REG_OK_FOR_INDEX_P(X)  0

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)    \
    ((REGNO (X) >= REG_R0 && REGNO (X) < REG_R3 + 1 )          \
       || (REGNO (X) >= FIRST_PSEUDO_REGISTER))

/* Nonzero if X is the 'ybase' register */
#define REG_OK_FOR_YBASE_P(X)   \
  (REGNO(X) == REG_YBASE || (REGNO (X) >= FIRST_PSEUDO_REGISTER))
#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

/* Nonzero if X is the 'ybase' register */
#define REG_OK_FOR_YBASE_P(X) REGNO_OK_FOR_YBASE_P (REGNO(X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the 1610, the actual legitimate addresses must be N (N must fit in
   5 bits), *rn (register indirect), *rn++, or *rn-- */

#define INT_FITS_5_BITS(I)    ((unsigned long) (I) < 0x20)
#define INT_FITS_16_BITS(I)   ((unsigned long) (I) < 0x10000)
#define YBASE_CONST_OFFSET(I)       ((I) >= -31 && (I) <= 0)
#define YBASE_OFFSET(X)       (GET_CODE (X) == CONST_INT && YBASE_CONST_OFFSET (INTVAL(X)))

#define FITS_16_BITS(X)       (GET_CODE (X) == CONST_INT && INT_FITS_16_BITS(INTVAL(X)))
#define FITS_5_BITS(X)        (GET_CODE (X) == CONST_INT && INT_FITS_5_BITS(INTVAL(X)))
#define ILLEGAL_HIMODE_ADDR(MODE, CONST)  ((MODE) == HImode && CONST == -31)

#define INDIRECTABLE_ADDRESS_P(X)                            \
    ((GET_CODE(X) == REG && REG_OK_FOR_BASE_P(X))            \
  || ((GET_CODE(X) == POST_DEC || GET_CODE(X) == POST_INC)   \
       && REG_P(XEXP(X,0)) && REG_OK_FOR_BASE_P(XEXP(X,0)))  \
  || (GET_CODE(X) == CONST_INT && (unsigned long) (X) < 0x20))


#define INDEXABLE_ADDRESS_P(X,MODE)                                 \
   ((GET_CODE(X) == PLUS && GET_CODE (XEXP (X,0)) == REG &&         \
     XEXP(X,0) == stack_pointer_rtx && YBASE_OFFSET(XEXP(X,1)) &&   \
     !ILLEGAL_HIMODE_ADDR(MODE, INTVAL(XEXP(X,1)))) ||              \
    (GET_CODE(X) == PLUS && GET_CODE (XEXP (X,1)) == REG &&         \
     XEXP(X,1) == stack_pointer_rtx && YBASE_OFFSET(XEXP(X,0)) &&  \
     !ILLEGAL_HIMODE_ADDR(MODE, INTVAL(XEXP(X,0)))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)                     \
{							            \
    if (INDIRECTABLE_ADDRESS_P(X))                                  \
        goto ADDR;                                                  \
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

   For the 1610, we need not do anything.  However, if we don't,
   `memory_address' will try lots of things to get a valid address, most of
   which will result in dead code and extra pseudos.  So we make the address
   valid here.

   This is easy:  The only valid addresses are an offset from a register
   and we know the address isn't valid.  So just call either `force_operand'
   or `force_reg' unless this is a (plus (reg ...) (const_int 0)).  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{ if (GET_CODE (X) == PLUS && XEXP (X, 1) == const0_rtx)	\
    X = XEXP (x, 0);						\
  if (GET_CODE (X) == MULT || GET_CODE (X) == PLUS)		\
    X = force_operand (X, 0);					\
  else								\
    X = force_reg (Pmode, X);					\
  goto WIN;							\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 1610, only postdecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == POST_DEC) goto LABEL

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
#define LEGITIMATE_CONSTANT_P(X) (1)


/* CONDITION CODE INFORMATION */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
   notice_update_cc( (EXP) )

/* DESCRIBING RELATIVE COSTS OF OPERATIONS */

/* A c expression for the cost of moving data from a register in
   class FROM to one in class TO. The classes are expressed using
   the enumeration values such as GENERAL_REGS. A value of 2 is
   the default.  */
#define REGISTER_MOVE_COST(MODE,FROM,TO)  dsp16xx_register_move_cost (FROM, TO)

/* A C expression for the cost of moving data of mode MODE between
   a register and memory. A value of 2 is the default.  */
#define MEMORY_MOVE_COST(MODE,CLASS,IN)                          \
  (GET_MODE_CLASS(MODE) == MODE_INT && MODE == QImode ? 12       \
   : 16)

/* A C expression for the cost of a branch instruction. A value of
   1 is the default; */
#define BRANCH_COST 1


/* Define this because otherwise gcc will try to put the function address
   in any old pseudo register. We can only use pt.  */
#define NO_FUNCTION_CSE

/* Define this macro as a C expression which is nonzero if accessing less
   than a word of memory (i.e a char or short) is no faster than accessing
   a word of memory, i.e if such access require more than one instruction
   or if there is no difference in cost between byte and (aligned) word
   loads.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if unaligned accesses have a cost many times greater than
   aligned accesses, for example if they are emulated in a trap handler */
/* define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) */


/* DIVIDING THE OUTPUT IN SECTIONS */
/* Output before read-only data.  */

#define DEFAULT_TEXT_SEG_NAME ".text"
#define TEXT_SECTION_ASM_OP  rsect_text

/* Output before constants and strings */
#define DEFAULT_CONST_SEG_NAME  ".const"
#define READONLY_DATA_SECTION_ASM_OP rsect_const

/* Output before writable data.  */
#define DEFAULT_DATA_SEG_NAME ".data"
#define DATA_SECTION_ASM_OP  rsect_data

#define DEFAULT_BSS_SEG_NAME ".bss"
#define BSS_SECTION_ASM_OP rsect_bss

/* We will default to using 1610 if the user doesn't
   specify it.  */
#define DEFAULT_CHIP_NAME "1610"

/* THE OVERALL FRAMEWORK OF AN ASSEMBLER FILE */

/* A C string constant describing how to begin a comment in the target
   assembler language.  */
#define ASM_COMMENT_START ""
#define ASM_COMMENT_END ""

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* OUTPUT OF DATA */

/* This is how we output a 'c' character string. For the 16xx
   assembler we have to do it one letter at a time */

#define ASCII_LENGTH 10

#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    const unsigned char *_hide_p = (const unsigned char *) (MYSTRING);	      \
    int _hide_thissize = (MYLENGTH);					      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      const unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int i;								      \
									      \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  register int c = p[i];					      \
	  								      \
	  if (i % ASCII_LENGTH == 0) \
	    fprintf (asm_out_file, "\tint ");				      \
	    								\
	  if (c >= ' ' && c < 0177 && c != '\'')			      \
	  {								      \
	    putc ('\'', asm_out_file);					      \
	    putc (c, asm_out_file);					      \
	    putc ('\'', asm_out_file);					      \
	  }								      \
	  else								      \
	    {								      \
	      fprintf (asm_out_file, "%d", c);			              \
	      /* After an octal-escape, if a digit follows,		      \
		 terminate one string constant and start another.	      \
		 The VAX assembler fails to stop reading the escape	      \
		 after three digits, so this is the only way we		      \
		 can get it to parse the data properly.  		      \
	      if (i < thissize - 1 && ISDIGIT (p[i + 1]))		      \
		fprintf (asm_out_file, "\'\n\tint \'");		              \
		*/ \
	  }								      \
	  /* if: \
	     we are not at the last char (i != thissize -1) \
	     and (we are not at a line break multiple  \
	     but i == 0) (it will be the very first time) \
	     then put out a comma to extend. \
	   */ \
	  if ((i != thissize - 1) && ((i + 1) % ASCII_LENGTH))	      \
	    fprintf(asm_out_file, ",");	 	                      \
	  if (!((i + 1) % ASCII_LENGTH)) \
	    fprintf (asm_out_file, "\n");			      \
	}								      \
      fprintf (asm_out_file, "\n");					      \
    }									      \
  }									      \
  while (0)

#define ASM_PN_FORMAT "*L%s_%lu"

/* OUTPUT OF UNINITIALIZED VARIABLES */

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
  asm_output_common (FILE, NAME, SIZE, ROUNDED);

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
  asm_output_local (FILE, NAME, SIZE, ROUNDED);

/* OUTPUT AND GENERATION OF LABELS */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global "

/* A C statement to output to the stdio stream any text necessary
   for declaring the name of an external symbol named name which
   is referenced in this compilation but not defined.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)   \
{					\
	fprintf (FILE, ".extern ");	\
	assemble_name (FILE, NAME);	\
	fprintf (FILE, "\n");		\
}
/* A C statement to output on stream an assembler pseudo-op to
   declare a library function named external.  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)    \
{						\
	fprintf (FILE, ".extern ");		\
	assemble_name (FILE, XSTR (FUN, 0));	\
	fprintf (FILE, "\n");			\
}

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%lu", PREFIX, (unsigned long)(NUM))


/* OUTPUT OF ASSEMBLER INSTRUCTIONS */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"a0", "a0l", "a1", "a1l", "x", "y", "yl", "p", "pl",  \
 "r0", "r1", "r2",  "r3", "j", "k", "ybase", "pt",     \
 "ar0", "ar1", "ar2", "ar3",                           \
 "c0", "c1", "c2", "pr", "rb",                         \
 "*(0)", "*(1)", "*(2)", "*(3)", "*(4)", "*(5)",       \
 "*(6)", "*(7)", "*(8)", "*(9)", "*(10)", "*(11)",     \
 "*(12)", "*(13)", "*(14)", "*(15)", "*(16)", "*(17)", \
 "*(18)", "*(19)", "*(20)", "*(21)", "*(22)", "*(23)", \
 "*(24)", "*(25)", "*(26)", "*(27)", "*(28)", "*(29)", \
 "*(30)", "*(31)" }

#define HIMODE_REGISTER_NAMES \
{"a0", "a0", "a1", "a1", "x", "y", "y", "p", "p",  \
 "r0", "r1", "r2",  "r3", "j", "k", "ybase", "pt",     \
 "ar0", "ar1", "ar2", "ar3",                           \
 "c0", "c1", "c2", "pr", "rb",                         \
 "*(0)", "*(1)", "*(2)", "*(3)", "*(4)", "*(5)",       \
 "*(6)", "*(7)", "*(8)", "*(9)", "*(10)", "*(11)",     \
 "*(12)", "*(13)", "*(14)", "*(15)", "*(16)", "*(17)", \
 "*(18)", "*(19)", "*(20)", "*(21)", "*(22)", "*(23)", \
 "*(24)", "*(25)", "*(26)", "*(27)", "*(28)", "*(29)", \
 "*(30)", "*(31)" }

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)  0

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.
   
   DSP1610 extensions for operand codes:

   %H - print lower 16 bits of constant
   %U - print upper 16 bits of constant
   %w - print low half of register (e.g 'a0l')
   %u - print upper half of register (e.g 'a0')
   %b - print high half of accumulator for F3 ALU instructions
   %h - print constant in decimal   */

#define PRINT_OPERAND(FILE, X, CODE) print_operand(FILE, X, CODE)


/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address (FILE, ADDR)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code since it is used only for profiling  */
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)    \
  internal_error ("profiling not implemented yet");

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code since it is used only for profiling  */
#define ASM_OUTPUT_REG_POP(FILE,REGNO)     \
  internal_error ("profiling not implemented yet"); 

/* OUTPUT OF DISPATCH TABLES */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\tint L%d-L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\tint L%d\n", VALUE)

/* ASSEMBLER COMMANDS FOR ALIGNMENT */

/* This is how to output an assembler line that says to advance 
   the location counter to a multiple of 2**LOG bytes. We should
   not have to do any alignment since the 1610 is a word machine.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)

/* Define this macro if ASM_OUTPUT_SKIP should not be used in the text section
   because it fails to put zero1 in the bytes that are skipped.  */
#define ASM_NO_SKIP_IN_TEXT 1

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t%d * int 0\n", (int)(SIZE))

/* CONTROLLING DEBUGGING INFORMATION FORMAT */

#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define ASM_OUTPUT_DEF(asm_out_file, LABEL1, LABEL2) \
         do {						\
	 fprintf (asm_out_file, ".alias " ); \
         ASM_OUTPUT_LABELREF(asm_out_file, LABEL1);  \
	 fprintf (asm_out_file, "=" ); \
         ASM_OUTPUT_LABELREF(asm_out_file, LABEL2); \
	 fprintf (asm_out_file, "\n" );			\
	 } while (0)


/* MISCELLANEOUS PARAMETERS */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE QImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 1

/* Defining this macro causes the compiler to omit a sign-extend, zero-extend,
   or bitwise 'and' instruction that truncates the count of a shift operation
   to a width equal to the number of bits needed to represent the size of the
   object being shifted. Do not define this macro unless the truncation applies
   to both shift operations and bit-field operations (if any).  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES 1

/* An alias for the machine mode used for pointers */
#define Pmode  QImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE  QImode

#if !defined(__DATE__)
#define TARGET_VERSION fprintf (stderr, " (%s)", VERSION_INFO1)
#else
#define TARGET_VERSION fprintf (stderr, " (%s, %s)", VERSION_INFO1, __DATE__)
#endif

#define VERSION_INFO1 "Lucent DSP16xx C Cross Compiler, version 1.3.0b"


/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Define this so gcc does not output a call to __main, since we
   are not currently supporting c++.  */
#define INIT_SECTION_ASM_OP  1

