/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
   Copyright (C) 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
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
Boston, MA 02111-1307, USA.  */


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */


/* Names to predefine in the preprocessor for this target machine.  */

/* See sun3.h, sun2.h, isi.h for different CPP_PREDEFINES.  */

/* Print subsidiary information on the compiler version in use.  */
#ifdef MOTOROLA
#define TARGET_VERSION fprintf (stderr, " (68k, Motorola syntax)");
#else
#define TARGET_VERSION fprintf (stderr, " (68k, MIT syntax)");
#endif

/* Define SUPPORT_SUN_FPA to include support for generating code for
   the Sun Floating Point Accelerator, an optional product for Sun 3
   machines.  By default, it is not defined.  Avoid defining it unless
   you need to output code for the Sun3+FPA architecture, as it has the
   effect of slowing down the register set operations in hard-reg-set.h
   (total number of registers will exceed number of bits in a long,
   if defined, causing the set operations to expand to loops).
   SUPPORT_SUN_FPA is typically defined in sun3.h.  */

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Compile for a 68020 (not a 68000 or 68010).  */
#define MASK_68020	1
#define TARGET_68020 (target_flags & MASK_68020)

/* Compile 68881 insns for floating point (not library calls).  */
#define MASK_68881	2
#define TARGET_68881 (target_flags & MASK_68881)

/* Compile using 68020 bitfield insns.  */
#define MASK_BITFIELD	4
#define TARGET_BITFIELD (target_flags & MASK_BITFIELD)

/* Compile using rtd insn calling sequence.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */
#define MASK_RTD	8
#define TARGET_RTD (target_flags & MASK_RTD)

/* Compile passing first two args in regs 0 and 1.
   This exists only to test compiler features that will
   be needed for RISC chips.  It is not usable
   and is not intended to be usable on this cpu.  */
#define MASK_REGPARM	16
#define TARGET_REGPARM (target_flags & MASK_REGPARM)

/* Compile with 16-bit `int'.  */
#define MASK_SHORT	32
#define TARGET_SHORT (target_flags & MASK_SHORT)

/* Compile with special insns for Sun FPA.  */
#define MASK_FPA	64
#define TARGET_FPA (target_flags & MASK_FPA)

/* Compile (actually, link) for Sun SKY board.  */
#define MASK_SKY	128
#define TARGET_SKY (target_flags & MASK_SKY)

/* Optimize for 68040, but still allow execution on 68020
   (-m68020-40 or -m68040).
   The 68040 will execute all 68030 and 68881/2 instructions, but some
   of them must be emulated in software by the OS.  When TARGET_68040 is
   turned on, these instructions won't be used.  This code will still
   run on a 68030 and 68881/2. */
#define MASK_68040	256
#define TARGET_68040 (target_flags & MASK_68040)

/* Use the 68040-only fp instructions (-m68040 or -m68060).  */
#define MASK_68040_ONLY	512
#define TARGET_68040_ONLY (target_flags & MASK_68040_ONLY)

/* Optimize for 68060, but still allow execution on 68020
   (-m68020-60 or -m68060).
   The 68060 will execute all 68030 and 68881/2 instructions, but some
   of them must be emulated in software by the OS.  When TARGET_68060 is
   turned on, these instructions won't be used.  This code will still
   run on a 68030 and 68881/2. */
#define MASK_68060	1024
#define TARGET_68060 (target_flags & MASK_68060)

/* Compile for mcf5200 */
#define MASK_5200	2048
#define TARGET_5200 (target_flags & MASK_5200)

/* Align ints to a word boundary.  This breaks compatibility with the 
   published ABI's for structures containing ints, but produces faster
   code on cpus with 32 bit busses (020, 030, 040, 060, CPU32+, coldfire).
   It's required for coldfire cpus without a misalignment module.  */
#define MASK_ALIGN_INT	4096
#define TARGET_ALIGN_INT (target_flags & MASK_ALIGN_INT)

/* Compile for a CPU32 */
	/* A 68020 without bitfields is a good heuristic for a CPU32 */
#define TARGET_CPU32	(TARGET_68020 && !TARGET_BITFIELD)

/* Use PC-relative addressing modes (without using a global offset table).
   The m68000 supports 16-bit PC-relative addressing.
   The m68020 supports 32-bit PC-relative addressing
   (using outer displacements).

   Under this model, all SYMBOL_REFs (and CONSTs) and LABEL_REFs are
   treated as all containing an implicit PC-relative component, and hence
   cannot be used directly as addresses for memory writes.  See the comments
   in m68k.c for more information.  */
#define MASK_PCREL	8192
#define TARGET_PCREL	(target_flags & MASK_PCREL)

/* Relax strict alignment. */
#define MASK_NO_STRICT_ALIGNMENT 16384
#define TARGET_STRICT_ALIGNMENT  (~target_flags & MASK_NO_STRICT_ALIGNMENT)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { { "68020", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY)},	\
    { "c68020", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY)},	\
    { "68020", (MASK_68020|MASK_BITFIELD)},				\
    { "c68020", (MASK_68020|MASK_BITFIELD)},				\
    { "68000", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY	\
		|MASK_68020|MASK_BITFIELD|MASK_68881)},			\
    { "c68000", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY	\
		|MASK_68020|MASK_BITFIELD|MASK_68881)},			\
    { "bitfield", MASK_BITFIELD},					\
    { "nobitfield", - MASK_BITFIELD},					\
    { "rtd", MASK_RTD},							\
    { "nortd", - MASK_RTD},						\
    { "short", MASK_SHORT},						\
    { "noshort", - MASK_SHORT},						\
    { "fpa", -(MASK_SKY|MASK_68040_ONLY|MASK_68881)},			\
    { "fpa", MASK_FPA},							\
    { "nofpa", - MASK_FPA},						\
    { "sky", -(MASK_FPA|MASK_68040_ONLY|MASK_68881)},			\
    { "sky", MASK_SKY},							\
    { "nosky", - MASK_SKY},						\
    { "68881", - (MASK_FPA|MASK_SKY)},					\
    { "68881", MASK_68881},						\
    { "soft-float", - (MASK_FPA|MASK_SKY|MASK_68040_ONLY|MASK_68881)},	\
    { "68020-40", -(MASK_5200|MASK_68060|MASK_68040_ONLY)},		\
    { "68020-40", (MASK_BITFIELD|MASK_68881|MASK_68020|MASK_68040)},	\
    { "68020-60", -(MASK_5200|MASK_68040_ONLY)},			\
    { "68020-60", (MASK_BITFIELD|MASK_68881|MASK_68020|MASK_68040	\
		   |MASK_68060)},					\
    { "68030", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY)},	\
    { "68030", (MASK_68020|MASK_BITFIELD)},				\
    { "68040", - (MASK_5200|MASK_68060)},				\
    { "68040", (MASK_68020|MASK_68881|MASK_BITFIELD			\
		|MASK_68040_ONLY|MASK_68040)},				\
    { "68060", - (MASK_5200|MASK_68040)},				\
    { "68060", (MASK_68020|MASK_68881|MASK_BITFIELD			\
		|MASK_68040_ONLY|MASK_68060)},				\
    { "5200", - (MASK_68060|MASK_68040|MASK_68040_ONLY|MASK_68020	\
		|MASK_BITFIELD|MASK_68881)},				\
    { "5200", (MASK_5200)},						\
    { "68851", 0},							\
    { "no-68851", 0},							\
    { "68302", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY	\
		  |MASK_68020|MASK_BITFIELD|MASK_68881)},		\
    { "68332", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY	\
		  |MASK_BITFIELD|MASK_68881)},				\
    { "68332", MASK_68020},						\
    { "cpu32", - (MASK_5200|MASK_68060|MASK_68040|MASK_68040_ONLY	\
		  |MASK_BITFIELD|MASK_68881)},				\
    { "cpu32", MASK_68020},						\
    { "align-int", MASK_ALIGN_INT },					\
    { "no-align-int", -MASK_ALIGN_INT },				\
    { "pcrel", MASK_PCREL},						\
    { "strict-align", -MASK_NO_STRICT_ALIGNMENT},			\
    { "no-strict-align", MASK_NO_STRICT_ALIGNMENT},			\
    SUBTARGET_SWITCHES							\
    { "", TARGET_DEFAULT}}
/* TARGET_DEFAULT is defined in sun*.h and isi.h, etc.  */

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.  The
   variable, type `char *', is set to the variable part of the given
   option if the fixed part matches.  The actual option name is made
   by appending `-m' to the specified name.  */
#define TARGET_OPTIONS							\
{ { "align-loops=",	&m68k_align_loops_string },			\
  { "align-jumps=",	&m68k_align_jumps_string },			\
  { "align-functions=",	&m68k_align_funcs_string },			\
  SUBTARGET_OPTIONS							\
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS		\
{					\
  override_options();			\
  if (! TARGET_68020 && flag_pic == 2)	\
    error("-fPIC is not currently supported on the 68000 or 68010\n");	\
  if (TARGET_PCREL && flag_pic == 0)	\
    flag_pic = 1;			\
  SUBTARGET_OVERRIDE_OPTIONS;		\
}

/* These are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES
#define SUBTARGET_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS

/* target machine storage layout */

/* Define for XFmode extended real floating point support.
   This will automatically cause REAL_ARITHMETIC to be defined.  */
#define LONG_DOUBLE_TYPE_SIZE 96

/* Define if you don't want extended real, but do want to use the
   software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/* #define REAL_ARITHMETIC */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is true for 68020 insns such as bfins and bfexts.
   We make it true always by avoiding using the single-bit insns
   except in special cases with constant bit numbers.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the 68000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* For 68000 we can decide arbitrarily
   since there are no machine instructions for them.
   So let's be consistent.  */
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

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_SHORT ? 16 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY (1 << (m68k_align_funcs + 3))

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this. 
   Most published ABIs say that ints should be aligned on 16 bit
   boundaries, but cpus with 32 bit busses get better performance
   aligned on 32 bit boundaries.  Coldfires without a misalignment
   module require 32 bit alignment. */
#define BIGGEST_ALIGNMENT (TARGET_ALIGN_INT ? 32 : 16)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT (TARGET_STRICT_ALIGNMENT)

/* Maximum power of 2 that code can be aligned to.  */
#define MAX_CODE_ALIGN	2			/* 4 byte alignment */

/* Align loop starts for optimal branching.  */
#define LOOP_ALIGN(LABEL) (m68k_align_loops)

/* This is how to align an instruction for optimal branching. */
#define LABEL_ALIGN_AFTER_BARRIER(LABEL) (m68k_align_jumps)

#define SELECT_RTX_SECTION(MODE, X)					\
{									\
  if (!flag_pic)							\
    readonly_data_section();						\
  else if (LEGITIMATE_PIC_OPERAND_P (X))				\
    readonly_data_section();						\
  else									\
    data_section();							\
}

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).  */

#define INT_TYPE_SIZE (TARGET_SHORT ? 16 : 32)

/* Define these to avoid dependence on meaning of `int'.
   Note that WCHAR_TYPE_SIZE is used in cexp.y,
   where TARGET_SHORT is not available.  */

#define WCHAR_TYPE "long int"
#define WCHAR_TYPE_SIZE 32

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the 68000, we give the data registers numbers 0-7,
   the address registers numbers 010-017,
   and the 68881 floating point registers numbers 020-027.  */
#ifndef SUPPORT_SUN_FPA
#define FIRST_PSEUDO_REGISTER 24
#else
#define FIRST_PSEUDO_REGISTER 56
#endif

/* This defines the register which is used to hold the offset table for PIC. */
#define PIC_OFFSET_TABLE_REGNUM 13

#ifndef SUPPORT_SUN_FPA

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the 68000, only the stack pointer is such.  */

#define FIXED_REGISTERS        \
 {/* Data registers.  */       \
  0, 0, 0, 0, 0, 0, 0, 0,      \
                               \
  /* Address registers.  */    \
  0, 0, 0, 0, 0, 0, 0, 1,      \
                               \
  /* Floating point registers  \
     (if available).  */       \
  0, 0, 0, 0, 0, 0, 0, 0 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
 {1, 1, 0, 0, 0, 0, 0, 0,   \
  1, 1, 0, 0, 0, 0, 0, 1,   \
  1, 1, 0, 0, 0, 0, 0, 0 }

#else /* SUPPORT_SUN_FPA */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the 68000, only the stack pointer is such.  */

/* fpa0 is also reserved so that it can be used to move data back and
   forth between high fpa regs and everything else. */

#define FIXED_REGISTERS        \
 {/* Data registers.  */       \
  0, 0, 0, 0, 0, 0, 0, 0,      \
                               \
  /* Address registers.  */    \
  0, 0, 0, 0, 0, 0, 0, 1,      \
                               \
  /* Floating point registers  \
     (if available).  */       \
  0, 0, 0, 0, 0, 0, 0, 0,      \
                               \
  /* Sun3 FPA registers.  */   \
  1, 0, 0, 0, 0, 0, 0, 0,      \
  0, 0, 0, 0, 0, 0, 0, 0,      \
  0, 0, 0, 0, 0, 0, 0, 0,      \
  0, 0, 0, 0, 0, 0, 0, 0 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
 {1, 1, 0, 0, 0, 0, 0, 0, \
  1, 1, 0, 0, 0, 0, 0, 1, \
  1, 1, 0, 0, 0, 0, 0, 0, \
  /* FPA registers.  */   \
  1, 1, 1, 1, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0  }

#endif /* defined SUPPORT_SUN_FPA */


/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compiler's mitts completely off it.  We don't bother to zero it out
   of register classes.  */

#ifdef SUPPORT_SUN_FPA

#define CONDITIONAL_REGISTER_USAGE \
{ 						\
  int i; 					\
  HARD_REG_SET x; 				\
  if (! TARGET_FPA)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FPA_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
  if (! TARGET_68881)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FP_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
  if (flag_pic)					\
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]		\
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;\
}
#else
#define CONDITIONAL_REGISTER_USAGE \
{ 						\
  int i; 					\
  HARD_REG_SET x; 				\
  if (! TARGET_68881)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FP_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
  if (flag_pic)					\
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]		\
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;\
}

#endif /* defined SUPPORT_SUN_FPA */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the 68000, ordinary registers hold 32 bits worth;
   for the 68881 registers, a single register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 16 ? GET_MODE_NUNITS (MODE)	\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

#ifndef SUPPORT_SUN_FPA

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 68000, the cpu registers can hold any mode but the 68881 registers
   can hold only SFmode or DFmode.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((REGNO) < 16					\
    && !((REGNO) < 8 && (REGNO) + GET_MODE_SIZE (MODE) / 4 > 8))	\
   || ((REGNO) >= 16 && (REGNO) < 24				        \
       && (GET_MODE_CLASS (MODE) == MODE_FLOAT		\
	   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)		\
       && GET_MODE_UNIT_SIZE (MODE) <= 12))

#else /* defined SUPPORT_SUN_FPA */

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 68000, the cpu registers can hold any mode but the 68881 registers
   can hold only SFmode or DFmode.  However, the Sun FPA register can
   (apparently) hold whatever you feel like putting in them.
   If using the fpa, don't put a double in d7/a0.  */

/* ??? This is confused.  The check to prohibit d7/a0 overlaps should always
   be enabled regardless of whether TARGET_FPA is specified.  It isn't clear
   what the other d/a register checks are for.  Every check using REGNO
   actually needs to use a range, e.g. 24>=X<56 not <56.  There is probably
   no one using this code anymore.  
   This code used to be used to suppress register usage for the 68881 by
   saying that the 68881 registers couldn't hold values of any mode if there
   was no 68881.  This was wrong, because reload (etc.) will still try
   to save and restore call-saved registers during, for instance, non-local
   goto.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
(((REGNO) < 16								\
  && !(TARGET_FPA							\
       && GET_MODE_CLASS ((MODE)) != MODE_INT				\
       && GET_MODE_UNIT_SIZE ((MODE)) > 4				\
       && (REGNO) < 8 && (REGNO) + GET_MODE_SIZE ((MODE)) / 4 > 8	\
       && (REGNO) % (GET_MODE_UNIT_SIZE ((MODE)) / 4) != 0))		\
 || ((REGNO) >= 16 && (REGNO) < 24					\
     ? ((GET_MODE_CLASS (MODE) == MODE_FLOAT				\
	 || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)		\
	&& GET_MODE_UNIT_SIZE (MODE) <= 12)				\
     : ((REGNO) < 56 ? TARGET_FPA && GET_MODE_UNIT_SIZE (MODE) <= 8 : 0)))

#endif /* defined SUPPORT_SUN_FPA */

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (! TARGET_68881					\
   || ((GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
	|| GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)	\
       == (GET_MODE_CLASS (MODE2) == MODE_FLOAT		\
	   || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT)))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* m68000 pc isn't overloaded on a register.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 14

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 8

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 9

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

/* The 68000 has three kinds of registers, so eight classes would be
   a complete set.  One of them is not needed.  */

#ifndef SUPPORT_SUN_FPA

enum reg_class {
  NO_REGS, DATA_REGS,
  ADDR_REGS, FP_REGS,
  GENERAL_REGS, DATA_OR_FP_REGS,
  ADDR_OR_FP_REGS, ALL_REGS,
  LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 { "NO_REGS", "DATA_REGS",              \
   "ADDR_REGS", "FP_REGS",              \
   "GENERAL_REGS", "DATA_OR_FP_REGS",   \
   "ADDR_OR_FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{					\
  {0x00000000},  /* NO_REGS */		\
  {0x000000ff},  /* DATA_REGS */	\
  {0x0000ff00},  /* ADDR_REGS */	\
  {0x00ff0000},  /* FP_REGS */		\
  {0x0000ffff},  /* GENERAL_REGS */	\
  {0x00ff00ff},  /* DATA_OR_FP_REGS */	\
  {0x00ffff00},  /* ADDR_OR_FP_REGS */	\
  {0x00ffffff},  /* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) (((REGNO)>>3)+1)

#else /* defined SUPPORT_SUN_FPA */

/*
 * Notes on final choices:
 *
 *   1) Didn't feel any need to union-ize LOW_FPA_REGS with anything
 * else.
 *   2) Removed all unions that involve address registers with
 * floating point registers (left in unions of address and data with
 * floating point).
 *   3) Defined GENERAL_REGS as ADDR_OR_DATA_REGS.
 *   4) Defined ALL_REGS as FPA_OR_FP_OR_GENERAL_REGS.
 *   4) Left in everything else.
 */
enum reg_class { NO_REGS, LO_FPA_REGS, FPA_REGS, FP_REGS,
  FP_OR_FPA_REGS, DATA_REGS, DATA_OR_FPA_REGS, DATA_OR_FP_REGS,
  DATA_OR_FP_OR_FPA_REGS, ADDR_REGS, GENERAL_REGS,
  GENERAL_OR_FPA_REGS, GENERAL_OR_FP_REGS, ALL_REGS,
  LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 { "NO_REGS", "LO_FPA_REGS", "FPA_REGS", "FP_REGS",  \
   "FP_OR_FPA_REGS", "DATA_REGS", "DATA_OR_FPA_REGS", "DATA_OR_FP_REGS",  \
   "DATA_OR_FP_OR_FPA_REGS", "ADDR_REGS", "GENERAL_REGS",  \
   "GENERAL_OR_FPA_REGS", "GENERAL_OR_FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{							\
 {0, 0},			/* NO_REGS */		\
 {0xff000000, 0x000000ff},	/* LO_FPA_REGS */	\
 {0xff000000, 0x00ffffff},	/* FPA_REGS */		\
 {0x00ff0000, 0x00000000},	/* FP_REGS */		\
 {0xffff0000, 0x00ffffff},	/* FP_OR_FPA_REGS */	\
 {0x000000ff, 0x00000000},	/* DATA_REGS */		\
 {0xff0000ff, 0x00ffffff},	/* DATA_OR_FPA_REGS */	\
 {0x00ff00ff, 0x00000000},	/* DATA_OR_FP_REGS */	\
 {0xffff00ff, 0x00ffffff},	/* DATA_OR_FP_OR_FPA_REGS */\
 {0x0000ff00, 0x00000000},	/* ADDR_REGS */		\
 {0x0000ffff, 0x00000000},	/* GENERAL_REGS */	\
 {0xff00ffff, 0x00ffffff},	/* GENERAL_OR_FPA_REGS */\
 {0x00ffffff, 0x00000000},	/* GENERAL_OR_FP_REGS */\
 {0xffffffff, 0x00ffffff},	/* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) (regno_reg_class[(REGNO)>>3])

#endif /* SUPPORT_SUN_FPA */

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.
   We do a trick here to modify the effective constraints on the
   machine description; we zorch the constraint letters that aren't
   appropriate for a specific target.  This allows us to guarantee
   that a specific kind of register will not be used for a given target
   without fiddling with the register classes above. */

#ifndef SUPPORT_SUN_FPA

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'a' ? ADDR_REGS :			\
   ((C) == 'd' ? DATA_REGS :			\
    ((C) == 'f' ? (TARGET_68881 ? FP_REGS :	\
		   NO_REGS) :			\
     NO_REGS)))

#else /* defined SUPPORT_SUN_FPA */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'a' ? ADDR_REGS :			\
   ((C) == 'd' ? DATA_REGS :			\
    ((C) == 'f' ? (TARGET_68881 ? FP_REGS :	\
		   NO_REGS) :			\
     ((C) == 'x' ? (TARGET_FPA ? FPA_REGS :	\
		    NO_REGS) :			\
      ((C) == 'y' ? (TARGET_FPA ? LO_FPA_REGS :	\
		     NO_REGS) :			\
       NO_REGS)))))

#endif /* defined SUPPORT_SUN_FPA */

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the 68000, `I' is used for the range 1 to 8
   allowed as immediate shift counts and in addq.
   `J' is used for the range of signed numbers that fit in 16 bits.
   `K' is for numbers that moveq can't handle.
   `L' is for range -8 to -1, range of values that can be added with subq.
   `M' is for numbers that moveq+notb can't handle.
   'N' is for range 24 to 31, rotatert:SI 8 to 1 expressed as rotate.
   'O' is for 16 (for rotate using swap).
   'P' is for range 8 to 15, rotatert:HI 8 to 1 expressed as rotate.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'I' ? (VALUE) > 0 && (VALUE) <= 8 : \
   (C) == 'J' ? (VALUE) >= -0x8000 && (VALUE) <= 0x7FFF : \
   (C) == 'K' ? (VALUE) < -0x80 || (VALUE) >= 0x80 : \
   (C) == 'L' ? (VALUE) < 0 && (VALUE) >= -8 : \
   (C) == 'M' ? (VALUE) < -0x100 || (VALUE) >= 0x100 : \
   (C) == 'N' ? (VALUE) >= 24 && (VALUE) <= 31 : \
   (C) == 'O' ? (VALUE) == 16 : \
   (C) == 'P' ? (VALUE) >= 8 && (VALUE) <= 15 : 0)

/*
 * A small bit of explanation:
 * "G" defines all of the floating constants that are *NOT* 68881
 * constants.  this is so 68881 constants get reloaded and the
 * fpmovecr is used.  "H" defines *only* the class of constants that
 * the fpa can use, because these can be gotten at in any fpa
 * instruction and there is no need to force reloads.
 */
#ifndef SUPPORT_SUN_FPA
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? ! (TARGET_68881 && standard_68881_constant_p (VALUE)) : 0 )
#else /* defined SUPPORT_SUN_FPA */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? ! (TARGET_68881 && standard_68881_constant_p (VALUE)) : \
   (C) == 'H' ? (TARGET_FPA && standard_sun_fpa_constant_p (VALUE)) : 0)
#endif /* defined SUPPORT_SUN_FPA */

/* A C expression that defines the optional machine-dependent constraint
   letters that can be used to segregate specific types of operands,  
   usually memory references, for the target machine.  It should return 1 if
   VALUE corresponds to the operand type represented by the constraint letter
   C.  If C is not defined as an extra constraint, the value returned should 
   be 0 regardless of VALUE.  */

/* Letters in the range `Q' through `U' may be defined in a
   machine-dependent fashion to stand for arbitrary operand types. 
   The machine description macro `EXTRA_CONSTRAINT' is passed the
   operand as its first argument and the constraint letter as its
   second operand.

   `Q' means address register indirect addressing mode.
   `S' is for operands that satisfy 'm' when -mpcrel is in effect.
   `T' is for operands that satisfy 's' when -mpcrel is not in effect.  */

#define EXTRA_CONSTRAINT(OP,CODE)			\
  (((CODE) == 'S')					\
   ? (TARGET_PCREL					\
      && GET_CODE (OP) == MEM				\
      && (GET_CODE (XEXP (OP, 0)) == SYMBOL_REF		\
	  || GET_CODE (XEXP (OP, 0)) == LABEL_REF	\
	  || GET_CODE (XEXP (OP, 0)) == CONST))		\
   : 							\
  (((CODE) == 'T')					\
   ? ( !TARGET_PCREL 					\
      && (GET_CODE (OP) == SYMBOL_REF			\
	  || GET_CODE (OP) == LABEL_REF			\
	  || GET_CODE (OP) == CONST))			\
   :							\
  (((CODE) == 'Q')					\
   ? (GET_CODE (OP) == MEM 				\
      && GET_CODE (XEXP (OP, 0)) == REG)		\
   :							\
   0)))

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   On the 68000 series, use a data reg if possible when the
   value is a constant in the range where moveq could be used
   and we ensure that QImodes are reloaded into data regs.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  \
  ((GET_CODE (X) == CONST_INT			\
    && (unsigned) (INTVAL (X) + 0x80) < 0x100	\
    && (CLASS) != ADDR_REGS)			\
   ? DATA_REGS					\
   : (GET_MODE (X) == QImode && (CLASS) != ADDR_REGS) \
   ? DATA_REGS					\
   : (GET_CODE (X) == CONST_DOUBLE					\
      && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
   ? (TARGET_68881 && (CLASS == FP_REGS || CLASS == DATA_OR_FP_REGS)	\
      ? FP_REGS : NO_REGS)						\
   : (TARGET_PCREL				\
      && (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == CONST \
	  || GET_CODE (X) == LABEL_REF))	\
   ? ADDR_REGS					\
   : (CLASS))

/* Force QImode output reloads from subregs to be allocated to data regs,
   since QImode stores from address regs are not supported.  We make the
   assumption that if the class is not ADDR_REGS, then it must be a superset
   of DATA_REGS.  */

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  (((MODE) == QImode && (CLASS) != ADDR_REGS)	\
   ? DATA_REGS					\
   : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 68000, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#ifndef SUPPORT_SUN_FPA

#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS ? 1 \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Moves between fp regs and other regs are two insns.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2)		\
  (((CLASS1) == FP_REGS && (CLASS2) != FP_REGS)	        \
    || ((CLASS2) == FP_REGS && (CLASS1) != FP_REGS)	\
    ? 4 : 2)

#else /* defined SUPPORT_SUN_FPA */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS || (CLASS) == FPA_REGS || (CLASS) == LO_FPA_REGS ? 1 \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Moves between fp regs and other regs are two insns.  */
/* Likewise for high fpa regs and other regs.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2)		\
  ((((CLASS1) == FP_REGS && (CLASS2) != FP_REGS)	\
    || ((CLASS2) == FP_REGS && (CLASS1) != FP_REGS)	\
    || ((CLASS1) == FPA_REGS && (CLASS2) != FPA_REGS)	\
    || ((CLASS2) == FPA_REGS && (CLASS1) != FPA_REGS))	\
   ? 4 : 2)

#endif /* define SUPPORT_SUN_FPA */

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Nonzero if we need to generate stack-probe insns.
   On most systems they are not needed.
   When they are needed, define this as the stack offset to probe at.  */
#define NEED_PROBE 0

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
   On the 68000, sp@- in a byte insn really pushes a word.
   On the 5200 (coldfire), sp@- in a byte insn pushes just a byte.  */
#define PUSH_ROUNDING(BYTES) (TARGET_5200 ? BYTES : ((BYTES) + 1) & ~1)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 8

/* Offset of the CFA from the argument pointer register value.  */
#define ARG_POINTER_CFA_OFFSET 8

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 68000, the RTS insn cannot pop anything.
   On the 68010, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)   \
  ((TARGET_RTD && (!(FUNDECL) || TREE_CODE (FUNDECL) != IDENTIFIER_NODE)	\
    && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	|| (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	    == void_type_node)))				\
   ? (SIZE) : 0)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the 68000 the return value is in D0 regardless.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the 68000 the return value is in D0 regardless.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, 0)

/* 1 if N is a possible register number for a function value.
   On the 68000, d0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#define NEEDS_UNTYPED_CALL 0

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for function argument passing.
   On the 68000, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the m68k, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the m68k, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
 ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

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

/* On the 68000 all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in d0, d1.
   *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8) ? gen_rtx_REG ((MODE), (CUM) / 4) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8					\
  && 8 < ((CUM) + ((MODE) == BLKmode				\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM) / 4 : 0)

/* Generate the assembly code for function entry. */
#define FUNCTION_PROLOGUE(FILE, SIZE) output_function_prologue(FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  asm_fprintf (FILE, "\tlea %LLP%d,%Ra0\n\tjsr mcount\n", (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, BLOCK_OR_LABEL)	\
do							\
  {							\
    switch (profile_block_flag)				\
      {							\
      case 2:						\
        asm_fprintf (FILE, "\tpea %d\n\tpea %LLPBX0\n\tjsr %U__bb_init_trace_func\n\taddql %I8,%Rsp\n", \
                           (BLOCK_OR_LABEL)); \
        break;						\
							\
      default:						\
        asm_fprintf (FILE, "\ttstl %LLPBX0\n\tbne %LLPI%d\n\tpea %LLPBX0\n\tjsr %U__bb_init_func\n\taddql %I4,%Rsp\n%LLPI%d:\n", \
                           (BLOCK_OR_LABEL), (BLOCK_OR_LABEL)); \
        break;						\
      }							\
  }							\
while(0)

/* Output assembler code to FILE to increment the counter for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)	\
do							\
  {							\
    switch (profile_block_flag)				\
      {							\
      case 2:						\
        asm_fprintf (FILE, "\tmovel %Ra1,%Rsp@-\n\tlea ___bb,%Ra1\n\tmovel %I%d,%Ra1@(0)\n\tmovel %I%LLPBX0,%Ra1@(4)\n\tmovel %Rsp@+,%Ra1\n\tjsr %U__bb_trace_func\n", \
                           BLOCKNO);			\
        break;						\
							\
      default:						\
        asm_fprintf (FILE, "\taddql %I1,%LLPBX2+%d\n", 4 * BLOCKNO); \
        break;						\
      }							\
  }							\
while(0)

/* Output assembler code to FILE to indicate return from 
   a function during basic block profiling. */

#define FUNCTION_BLOCK_PROFILER_EXIT(FILE)		\
  asm_fprintf (FILE, "\tjsr %U__bb_trace_ret\n");

/* Save all registers which may be clobbered by a function call.
   MACHINE_STATE_SAVE and MACHINE_STATE_RESTORE are target-code macros,
   used in libgcc2.c.  They may not refer to TARGET_* macros !!! */
#if defined (__mc68010__) || defined(mc68010) \
	|| defined(__mc68020__) || defined(mc68020) \
	|| defined(__mc68030__) || defined(mc68030) \
	|| defined(__mc68040__) || defined(mc68040) \
	|| defined(__mcpu32__) || defined(mcpu32)
#define MACHINE_STATE_m68010_up
#endif

#ifdef MOTOROLA
#if defined(__mcf5200__)
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("sub.l 20,%sp");		\
      asm ("movm.l &0x0303,4(%sp)");	\
      asm ("move.w %ccr,%d0");		\
      asm ("movm.l &0x0001,(%sp)");	\
    }
#else /* !__mcf5200__ */
#if defined(MACHINE_STATE_m68010_up)
#ifdef __HPUX_ASM__
/* HPUX assembler does not accept %ccr.  */
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("move.w %cc,-(%sp)");	\
      asm ("movm.l &0xc0c0,-(%sp)");	\
    }
#else /* ! __HPUX_ASM__ */
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("move.w %ccr,-(%sp)");	\
      asm ("movm.l &0xc0c0,-(%sp)");	\
    }
#endif /* __HPUX_ASM__ */
#else /* !MACHINE_STATE_m68010_up */
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("move.w %sr,-(%sp)");	\
      asm ("movm.l &0xc0c0,-(%sp)");	\
    }
#endif /* MACHINE_STATE_m68010_up */
#endif /* __mcf5200__ */
#else /* !MOTOROLA */
#if defined(__mcf5200__)
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("subl %#20,%/sp" : );	\
      asm ("movml %/d0/%/d1/%/a0/%/a1,%/sp@(4)" : ); \
      asm ("movew %/cc,%/d0" : );	\
      asm ("movml %/d0,%/sp@" : );	\
    }
#else /* !__mcf5200__ */
#if defined(MACHINE_STATE_m68010_up)
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("movew %/cc,%/sp@-" : );	\
      asm ("moveml %/d0/%/d1/%/a0/%/a1,%/sp@-" : ); \
    }
#else /* !MACHINE_STATE_m68010_up */
#define MACHINE_STATE_SAVE(id)		\
    {					\
      asm ("movew %/sr,%/sp@-" : );	\
      asm ("moveml %/d0/%/d1/%/a0/%/a1,%/sp@-" : ); \
    }
#endif /* MACHINE_STATE_m68010_up */
#endif /* __mcf5200__ */
#endif /* MOTOROLA */

/* Restore all registers saved by MACHINE_STATE_SAVE. */

#ifdef MOTOROLA
#if defined(__mcf5200__)
#define MACHINE_STATE_RESTORE(id)	\
    {					\
      asm ("movm.l (%sp),&0x0001");	\
      asm ("move.w %d0,%ccr");		\
      asm ("movm.l 4(%sp),&0x0303");	\
      asm ("add.l 20,%sp");		\
    }
#else /* !__mcf5200__ */
#ifdef __HPUX_ASM__
/* HPUX assembler does not accept %ccr.  */
#define MACHINE_STATE_RESTORE(id)	\
    {					\
      asm ("movm.l (%sp)+,&0x0303");	\
      asm ("move.w (%sp)+,%cc");	\
    }
#else /* ! __HPUX_ASM__ */
#define MACHINE_STATE_RESTORE(id)	\
    {					\
      asm ("movm.l (%sp)+,&0x0303");	\
      asm ("move.w (%sp)+,%ccr");	\
    }
#endif /* __HPUX_ASM__ */
#endif /* __mcf5200__ */
#else /* !MOTOROLA */
#if defined(__mcf5200__)
#define MACHINE_STATE_RESTORE(id)	\
    {					\
      asm ("movml %/sp@,%/d0" : );	\
      asm ("movew %/d0,%/cc" : );	\
      asm ("movml %/sp@(4),%/d0/%/d1/%/a0/%/a1" : ); \
      asm ("addl %#20,%/sp" : );	\
    }
#else /* !__mcf5200__ */
#define MACHINE_STATE_RESTORE(id)	\
    {					\
      asm ("moveml %/sp@+,%/d0/%/d1/%/a0/%/a1" : ); \
      asm ("movew %/sp@+,%/cc" : );	\
    }
#endif /* __mcf5200__ */
#endif /* MOTOROLA */

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Generate the assembly code for function exit. */
#define FUNCTION_EPILOGUE(FILE, SIZE) output_function_epilogue (FILE, SIZE)
  
/* This is a hook for other tm files to change.  */
/* #define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE) */

/* Determine if the epilogue should be output as RTL.
   You should override this if you define FUNCTION_EXTRA_EPILOGUE.  */
#define USE_RETURN_INSN use_return_insn ()

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.

   On the 68k, if we have a frame, we must add one word to its length
   to allow for the place that a6 is stored when we do have a frame pointer.
   Otherwise, we would need to compute the offset from the frame pointer
   of a local variable as a function of frame_pointer_needed, which
   is hard.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH)			\
{ int regno;							\
  int offset = -4;						\
  for (regno = 16; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 12;						\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 4;						\
  if (flag_pic && current_function_uses_pic_offset_table)	\
    offset += 4;						\
  (DEPTH) = (offset + ((get_frame_size () + 3) & -4)		\
	     + (get_frame_size () == 0 ? 0 : 4));		\
}

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the 68k, the trampoline looks like this:
     movl #STATIC,a0
     jmp  FUNCTION

   WARNING: Targets that may run on 68040+ cpus must arrange for
   the instruction cache to be flushed.  Previous incarnations of
   the m68k trampoline code attempted to get around this by either
   using an out-of-line transfer function or pc-relative data, but
   the fact remains that the code to jump to the transfer function
   or the code to load the pc-relative data needs to be flushed
   just as much as the "variable" portion of the trampoline.  
   Recognizing that a cache flush is going to be required anyway,
   dispense with such notions and build a smaller trampoline.  */

/* Since more instructions are required to move a template into
   place than to create it on the spot, don't use a template.  */

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 12

/* Alignment required for a trampoline in bits.  */

#define TRAMPOLINE_ALIGNMENT 16

/* Targets redefine this to invoke code to either flush the cache,
   or enable stack execution (or both).  */

#ifndef FINALIZE_TRAMPOLINE
#define FINALIZE_TRAMPOLINE(TRAMP)
#endif

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   We generate a two-instructions program at address TRAMP :
	movea.l &CXT,%a0
	jmp FNADDR					*/

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (HImode, TRAMP), GEN_INT(0x207C));	\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 2)), CXT); \
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (TRAMP, 6)),	\
		  GEN_INT(0x4EF9));					\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 8)), FNADDR); \
  FINALIZE_TRAMPOLINE(TRAMP);						\
}

/* This is the library routine that is used
   to transfer control from the trampoline
   to the actual nested function.
   It is defined for backward compatibility,
   for linking with object code that used the old
   trampoline definition.  */

/* A colon is used with no explicit operands
   to cause the template string to be scanned for %-constructs.  */
/* The function name __transfer_from_trampoline is not actually used.
   The function definition just permits use of "asm with operands"
   (though the operand list is empty).  */
#define TRANSFER_FROM_TRAMPOLINE				\
void								\
__transfer_from_trampoline ()					\
{								\
  register char *a0 asm ("%a0");				\
  asm (GLOBAL_ASM_OP " ___trampoline");				\
  asm ("___trampoline:");					\
  asm volatile ("move%.l %0,%@" : : "m" (a0[22]));		\
  asm volatile ("move%.l %1,%0" : "=a" (a0) : "m" (a0[18]));	\
  asm ("rts":);							\
}

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
/* #define HAVE_POST_DECREMENT 0 */

#define HAVE_PRE_DECREMENT 1
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)
#define REGNO_OK_FOR_BASE_P(REGNO) \
(((REGNO) ^ 010) < 8 || (unsigned) (reg_renumber[REGNO] ^ 010) < 8)
#define REGNO_OK_FOR_DATA_P(REGNO) \
((REGNO) < 8 || (unsigned) reg_renumber[REGNO] < 8)
#define REGNO_OK_FOR_FP_P(REGNO) \
(((REGNO) ^ 020) < 8 || (unsigned) (reg_renumber[REGNO] ^ 020) < 8)
#ifdef SUPPORT_SUN_FPA
#define REGNO_OK_FOR_FPA_P(REGNO) \
(((REGNO) >= 24 && (REGNO) < 56) || (reg_renumber[REGNO] >= 24 && reg_renumber[REGNO] < 56))
#endif

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the 68000, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is a data register.  */

#define DATA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_DATA_P (REGNO (X)))

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* 1 if X is an address register  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

#ifdef SUPPORT_SUN_FPA
/* 1 if X is a register in the Sun FPA.  */
#define FPA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FPA_P (REGNO (X)))
#else
/* Answer must be no if we don't have an FPA.  */
#define FPA_REG_P(X) 0
#endif

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and 
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   PCREL_GENERAL_OPERAND_OK makes reload accept addresses that are
   accepted by insn predicates, but which would otherwise fail the
   `general_operand' test.  */

#ifndef REG_OK_STRICT
#define PCREL_GENERAL_OPERAND_OK 0
#else
#define PCREL_GENERAL_OPERAND_OK (TARGET_PCREL)
#endif

#define LEGITIMATE_PIC_OPERAND_P(X)	\
  ((! symbolic_operand (X, VOIDmode)				\
    && ! (GET_CODE (X) == CONST_DOUBLE && CONST_DOUBLE_MEM (X)	\
	  && GET_CODE (CONST_DOUBLE_MEM (X)) == MEM		\
	  && symbolic_operand (XEXP (CONST_DOUBLE_MEM (X), 0),	\
			       VOIDmode)))			\
   || (GET_CODE (X) == SYMBOL_REF && SYMBOL_REF_FLAG (X))	\
   || PCREL_GENERAL_OPERAND_OK)

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
#define REG_OK_FOR_INDEX_P(X) ((REGNO (X) ^ 020) >= 8)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) ((REGNO (X) & ~027) != 0)

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

   When generating PIC, an address involving a SYMBOL_REF is legitimate
   if and only if it is the sum of pic_offset_table_rtx and the SYMBOL_REF.
   We use LEGITIMATE_PIC_OPERAND_P to throw out the illegitimate addresses,
   and we explicitly check for the sum of pic_offset_table_rtx and a SYMBOL_REF.

   Likewise for a LABEL_REF when generating PIC.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.  */

/* Allow SUBREG everywhere we allow REG.  This results in better code.  It
   also makes function inlining work when inline functions are called with
   arguments that are SUBREGs.  */

#define LEGITIMATE_BASE_REG_P(X)   \
  ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))	\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define INDIRECTABLE_1_ADDRESS_P(X)  \
  ((CONSTANT_ADDRESS_P (X) && (!flag_pic || LEGITIMATE_PIC_OPERAND_P (X))) \
   || LEGITIMATE_BASE_REG_P (X)						\
   || ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_INC)		\
       && LEGITIMATE_BASE_REG_P (XEXP (X, 0)))				\
   || (GET_CODE (X) == PLUS						\
       && LEGITIMATE_BASE_REG_P (XEXP (X, 0))				\
       && GET_CODE (XEXP (X, 1)) == CONST_INT				\
       && (TARGET_68020							\
	   || ((unsigned) INTVAL (XEXP (X, 1)) + 0x8000) < 0x10000))	\
   || (GET_CODE (X) == PLUS && XEXP (X, 0) == pic_offset_table_rtx 	\
       && flag_pic && GET_CODE (XEXP (X, 1)) == SYMBOL_REF)		\
   || (GET_CODE (X) == PLUS && XEXP (X, 0) == pic_offset_table_rtx 	\
       && flag_pic && GET_CODE (XEXP (X, 1)) == LABEL_REF))

#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)  \
{ if (INDIRECTABLE_1_ADDRESS_P (X)) goto ADDR; }

/* Only labels on dispatch tables are valid for indexing from.  */
#define GO_IF_INDEXABLE_BASE(X, ADDR)				\
{ rtx temp;							\
  if (GET_CODE (X) == LABEL_REF					\
      && (temp = next_nonnote_insn (XEXP (X, 0))) != 0		\
      && GET_CODE (temp) == JUMP_INSN				\
      && (GET_CODE (PATTERN (temp)) == ADDR_VEC			\
	  || GET_CODE (PATTERN (temp)) == ADDR_DIFF_VEC))	\
    goto ADDR;							\
  if (LEGITIMATE_BASE_REG_P (X)) goto ADDR; }

#define GO_IF_INDEXING(X, ADDR)	\
{ if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 0)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 1), ADDR); }			\
  if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 1)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 0), ADDR); } }

#define GO_IF_INDEXED_ADDRESS(X, ADDR)	 \
{ GO_IF_INDEXING (X, ADDR);						\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 1)) == CONST_INT				\
	  && (TARGET_68020 || (unsigned) INTVAL (XEXP (X, 1)) + 0x80 < 0x100))		\
	{ rtx go_temp = XEXP (X, 0); GO_IF_INDEXING (go_temp, ADDR); }	\
      if (GET_CODE (XEXP (X, 0)) == CONST_INT				\
	  && (TARGET_68020 || (unsigned) INTVAL (XEXP (X, 0)) + 0x80 < 0x100))		\
	{ rtx go_temp = XEXP (X, 1); GO_IF_INDEXING (go_temp, ADDR); } } }

/* coldfire/5200 does not allow HImode index registers.  */
#define LEGITIMATE_INDEX_REG_P(X)   \
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))	\
   || (! TARGET_5200					\
       && GET_CODE (X) == SIGN_EXTEND			\
       && GET_CODE (XEXP (X, 0)) == REG			\
       && GET_MODE (XEXP (X, 0)) == HImode		\
       && REG_OK_FOR_INDEX_P (XEXP (X, 0)))		\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && REG_OK_FOR_INDEX_P (SUBREG_REG (X))))

#define LEGITIMATE_INDEX_P(X)   \
   (LEGITIMATE_INDEX_REG_P (X)				\
    || ((TARGET_68020 || TARGET_5200) && GET_CODE (X) == MULT \
	&& LEGITIMATE_INDEX_REG_P (XEXP (X, 0))		\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& (INTVAL (XEXP (X, 1)) == 2			\
	    || INTVAL (XEXP (X, 1)) == 4		\
	    || (INTVAL (XEXP (X, 1)) == 8 && !TARGET_5200))))

/* If pic, we accept INDEX+LABEL, which is what do_tablejump makes.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{ GO_IF_NONINDEXED_ADDRESS (X, ADDR);					\
  GO_IF_INDEXED_ADDRESS (X, ADDR);					\
  if (flag_pic && MODE == CASE_VECTOR_MODE && GET_CODE (X) == PLUS	\
      && LEGITIMATE_INDEX_P (XEXP (X, 0))				\
      && GET_CODE (XEXP (X, 1)) == LABEL_REF)				\
    goto ADDR; }

/* Don't call memory_address_noforce for the address to fetch
   the switch offset.  This address is ok as it stands (see above),
   but memory_address_noforce would alter it.  */
#define PIC_CASE_VECTOR_ADDRESS(index) index

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 68000, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in an address reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in an address reg.  */

#define COPY_ONCE(Y) if (!copied) { Y = copy_rtx (Y); copied = ch = 1; }
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   \
{ register int ch = (X) != (OLDX);					\
  if (GET_CODE (X) == PLUS)						\
    { int copied = 0;							\
      if (GET_CODE (XEXP (X, 0)) == MULT)				\
	{ COPY_ONCE (X); XEXP (X, 0) = force_operand (XEXP (X, 0), 0);}	\
      if (GET_CODE (XEXP (X, 1)) == MULT)				\
	{ COPY_ONCE (X); XEXP (X, 1) = force_operand (XEXP (X, 1), 0);}	\
      if (ch && GET_CODE (XEXP (X, 1)) == REG				\
	  && GET_CODE (XEXP (X, 0)) == REG)				\
	goto WIN;							\
      if (ch) { GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN); }		\
      if (GET_CODE (XEXP (X, 0)) == REG					\
	       || (GET_CODE (XEXP (X, 0)) == SIGN_EXTEND		\
		   && GET_CODE (XEXP (XEXP (X, 0), 0)) == REG		\
		   && GET_MODE (XEXP (XEXP (X, 0), 0)) == HImode))	\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 1), 0);		\
	  emit_move_insn (temp, val);					\
	  COPY_ONCE (X);						\
	  XEXP (X, 1) = temp;						\
	  goto WIN; }							\
      else if (GET_CODE (XEXP (X, 1)) == REG				\
	       || (GET_CODE (XEXP (X, 1)) == SIGN_EXTEND		\
		   && GET_CODE (XEXP (XEXP (X, 1), 0)) == REG		\
		   && GET_MODE (XEXP (XEXP (X, 1), 0)) == HImode))	\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 0), 0);		\
	  emit_move_insn (temp, val);					\
	  COPY_ONCE (X);						\
	  XEXP (X, 0) = temp;						\
	  goto WIN; }}}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 68000, only predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC) goto LABEL

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Don't cse the address of the function being compiled.  */
#define NO_RECURSIVE_FUNCTION_CSE

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE -1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    /* Constant zero is super cheap due to clr instruction.  */	\
    if (RTX == const0_rtx) return 0;				\
    /* if ((OUTER_CODE) == SET) */				\
      return const_int_cost(RTX);				\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Compute the cost of various arithmetic operations.
   These are vaguely right for a 68020.  */
/* The costs for long multiply have been adjusted to
   work properly in synth_mult on the 68020,
   relative to an average of the time for add and the time for shift,
   taking away a little more because sometimes move insns are needed.  */
/* div?.w is relatively cheaper on 68000 counted in COSTS_N_INSNS terms.  */
#define MULL_COST (TARGET_68060 ? 2 : TARGET_68040 ? 5 : 13)
#define MULW_COST (TARGET_68060 ? 2 : TARGET_68040 ? 3 : TARGET_68020 ? 8 : 5)
#define DIVW_COST (TARGET_68020 ? 27 : 12)

#define RTX_COSTS(X,CODE,OUTER_CODE)				\
  case PLUS:							\
    /* An lea costs about three times as much as a simple add.  */  \
    if (GET_MODE (X) == SImode					\
	&& GET_CODE (XEXP (X, 1)) == REG			\
	&& GET_CODE (XEXP (X, 0)) == MULT			\
	&& GET_CODE (XEXP (XEXP (X, 0), 0)) == REG		\
	&& GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT	\
	&& (INTVAL (XEXP (XEXP (X, 0), 1)) == 2			\
	    || INTVAL (XEXP (XEXP (X, 0), 1)) == 4		\
	    || INTVAL (XEXP (XEXP (X, 0), 1)) == 8))		\
      return COSTS_N_INSNS (3);	 /* lea an@(dx:l:i),am */	\
    break;							\
  case ASHIFT:							\
  case ASHIFTRT:						\
  case LSHIFTRT:						\
    if (TARGET_68060)						\
      return COSTS_N_INSNS(1);					\
    if (! TARGET_68020)							\
      {									\
	if (GET_CODE (XEXP (X, 1)) == CONST_INT)			\
	  {								\
	    if (INTVAL (XEXP (X, 1)) < 16)				\
	      return COSTS_N_INSNS (2) + INTVAL (XEXP (X, 1)) / 2;	\
	    else							\
	      /* We're using clrw + swap for these cases.  */		\
	      return COSTS_N_INSNS (4) + (INTVAL (XEXP (X, 1)) - 16) / 2; \
	  }								\
	return COSTS_N_INSNS (10); /* worst case */			\
      }									\
    /* A shift by a big integer takes an extra instruction.  */ \
    if (GET_CODE (XEXP (X, 1)) == CONST_INT			\
	&& (INTVAL (XEXP (X, 1)) == 16))			\
      return COSTS_N_INSNS (2);	 /* clrw;swap */		\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT			\
	&& !(INTVAL (XEXP (X, 1)) > 0				\
	     && INTVAL (XEXP (X, 1)) <= 8))			\
      return COSTS_N_INSNS (3);	 /* lsr #i,dn */		\
    break;							\
  case MULT:							\
    if ((GET_CODE (XEXP (X, 0)) == ZERO_EXTEND			\
	 || GET_CODE (XEXP (X, 0)) == SIGN_EXTEND)		\
	&& GET_MODE (X) == SImode)				\
      return COSTS_N_INSNS (MULW_COST);				\
    if (GET_MODE (X) == QImode || GET_MODE (X) == HImode)	\
      return COSTS_N_INSNS (MULW_COST);				\
    else							\
      return COSTS_N_INSNS (MULL_COST);				\
  case DIV:							\
  case UDIV:							\
  case MOD:							\
  case UMOD:							\
    if (GET_MODE (X) == QImode || GET_MODE (X) == HImode)	\
      return COSTS_N_INSNS (DIVW_COST); /* div.w */		\
    return COSTS_N_INSNS (43);	 /* div.l */

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if the cc value is actually in the 68881, so a floating point
   conditional branch must be output.  */
#define CC_IN_68881 04000

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register fail to
   set the cc's.  However, in some cases these instructions can make it
   possibly invalid to use the saved cc's.  In those cases we clear out
   some or all of the saved cc's so they won't be used.  */

#define NOTICE_UPDATE_CC(EXP,INSN) notice_update_cc (EXP, INSN)

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
{ if (cc_prev_status.flags & CC_IN_68881)			\
    return FLOAT;						\
  if (cc_prev_status.flags & CC_NO_OVERFLOW)			\
    return NO_OV;						\
  return NORMAL; }

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)	\
  fprintf (FILE, "#NO_APP\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Here are four prefixes that are used by asm_fprintf to
   facilitate customization for alternate assembler syntaxes.
   Machines with no likelihood of an alternate syntax need not
   define these and need not use asm_fprintf.  */

/* The prefix for register names.  Note that REGISTER_NAMES
   is supposed to include this prefix.  */

#define REGISTER_PREFIX ""

/* The prefix for local labels.  You should be able to define this as
   an empty string, or any arbitrary string (such as ".", ".L%", etc)
   without having to make any other changes to account for the specific
   definition.  Note it is a string literal, not interpreted by printf
   and friends. */

#define LOCAL_LABEL_PREFIX ""

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* The prefix for immediate operands.  */

#define IMMEDIATE_PREFIX "#"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#ifndef SUPPORT_SUN_FPA

#define REGISTER_NAMES \
{"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",	\
 "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp",	\
 "fp0", "fp1", "fp2", "fp3", "fp4", "fp5", "fp6", "fp7" }

#else /* SUPPORTED_SUN_FPA */

#define REGISTER_NAMES \
{"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",	\
 "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp",	\
 "fp0", "fp1", "fp2", "fp3", "fp4", "fp5", "fp6", "fp7", \
 "fpa0", "fpa1", "fpa2", "fpa3", "fpa4", "fpa5", "fpa6", "fpa7", \
 "fpa8", "fpa9", "fpa10", "fpa11", "fpa12", "fpa13", "fpa14", "fpa15", \
 "fpa16", "fpa17", "fpa18", "fpa19", "fpa20", "fpa21", "fpa22", "fpa23", \
 "fpa24", "fpa25", "fpa26", "fpa27", "fpa28", "fpa29", "fpa30", "fpa31" }

#endif /* defined SUPPORT_SUN_FPA */

/* How to renumber registers for dbx and gdb.
   On the Sun-3, the floating point registers have numbers
   18 to 25, not 16 to 23 as they do in the compiler.  */

#define DBX_REGISTER_NUMBER(REGNO) ((REGNO) < 16 ? (REGNO) : (REGNO) + 2)

/* Before the prologue, RA is at 0(%sp).  */
#define INCOMING_RETURN_ADDR_RTX \
  gen_rtx_MEM (VOIDmode, gen_rtx_REG (VOIDmode, STACK_POINTER_REGNUM))

/* We must not use the DBX register numbers for the DWARF 2 CFA column
   numbers because that maps to numbers beyond FIRST_PSEUDO_REGISTER.
   Instead use the identity mapping.  */
#define DWARF_FRAME_REGNUM(REG) REG

/* Before the prologue, the top of the frame is at 4(%sp).  */
#define INCOMING_FRAME_SP_OFFSET 4

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define GLOBAL_ASM_OP ".globl"
#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fprintf (FILE, "%s ", GLOBAL_ASM_OP);		\
       assemble_name (FILE, NAME);			\
       fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  asm_fprintf (FILE, "%0U%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  asm_fprintf (FILE, "%0L%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%s%d", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* This is how to output a `long double' extended real constant. */
  
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  				\
do { long l[3];								\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     fprintf (FILE, "\t.long 0x%lx,0x%lx,0x%lx\n", l[0], l[1], l[2]);	\
   } while (0)
  
/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
  do { char dstr[30];						\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);		\
       fprintf (FILE, "\t.double 0r%s\n", dstr);		\
     } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     fprintf (FILE, "\t.long 0x%lx\n", l);		\
   } while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  asm_fprintf (FILE, "\tmovel %s,%Rsp@-\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  asm_fprintf (FILE, "\tmovel %Rsp@+,%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
   (The 68000 does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\t.long %LL%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  asm_fprintf (FILE, "\t.word %LL%d-%LL%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

/* We don't have a way to align to more than a two-byte boundary, so do the
   best we can and don't complain.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) >= 1)			\
    fprintf (FILE, "\t.even\n");

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

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

/* Output a float value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */

#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
 do {								\
      if (CODE == 'f')						\
        {							\
          char dstr[30];					\
          REAL_VALUE_TO_DECIMAL (VALUE, "%.9g", dstr);		\
          asm_fprintf ((FILE), "%I0r%s", dstr);			\
        }							\
      else							\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          asm_fprintf ((FILE), "%I0x%lx", l);			\
        }							\
     } while (0)

/* Output a double value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
      asm_fprintf (FILE, "%I0r%s", dstr);				\
    } while (0)

/* Note, long double immediate operands are not actually
   generated by m68k.md.  */
#define ASM_OUTPUT_LONG_DOUBLE_OPERAND(FILE,VALUE)			\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
      asm_fprintf (FILE, "%I0r%s", dstr);				\
    } while (0)

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the 68000, we use several CODE characters:
   '.' for dot needed in Motorola-style opcode names.
   '-' for an operand pushing on the stack:
       sp@-, -(sp) or -(%sp) depending on the style of syntax.
   '+' for an operand pushing on the stack:
       sp@+, (sp)+ or (%sp)+ depending on the style of syntax.
   '@' for a reference to the top word on the stack:
       sp@, (sp) or (%sp) depending on the style of syntax.
   '#' for an immediate operand prefix (# in MIT and Motorola syntax
       but & in SGS syntax).
   '!' for the fpcr register (used in some float-to-fixed conversions).
   '$' for the letter `s' in an op code, but only on the 68040.
   '&' for the letter `d' in an op code, but only on the 68040.
   '/' for register prefix needed by longlong.h.

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'o' for operands to go directly to output_operand_address (bypassing
       print_operand_address--used only for SYMBOL_REFs under TARGET_PCREL)
   'w' for FPA insn (print a CONST_DOUBLE as a SunFPA constant rather
       than directly).  Second part of 'y' below.
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'y' for a FPA insn (print pair of registers as rx:ry).  This also outputs
       CONST_DOUBLE's as SunFPA constant RAM registers if
       possible, so it should not be used except for the SunFPA. */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '.' || (CODE) == '#' || (CODE) == '-'			\
   || (CODE) == '+' || (CODE) == '@' || (CODE) == '!'			\
   || (CODE) == '$' || (CODE) == '&' || (CODE) == '/')

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   See m68k.c for the m68k specific codes.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Variables in m68k.c */
extern const char *m68k_align_loops_string;
extern const char *m68k_align_jumps_string;
extern const char *m68k_align_funcs_string;
extern int m68k_align_loops;
extern int m68k_align_jumps;
extern int m68k_align_funcs;
extern int m68k_last_compare_had_fp_operands;


/*
Local variables:
version-control: t
End:
*/
