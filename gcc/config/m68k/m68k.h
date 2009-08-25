/* Definitions of target machine for GCC for Motorola 680x0/ColdFire.
   Copyright (C) 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* We need to have MOTOROLA always defined (either 0 or 1) because we use
   if-statements and ?: on it.  This way we have compile-time error checking
   for both the MOTOROLA and MIT code paths.  We do rely on the host compiler
   to optimize away all constant tests.  */
#if MOTOROLA  /* Use the Motorola assembly syntax.  */
# define TARGET_VERSION fprintf (stderr, " (68k, Motorola syntax)")
#else
# define MOTOROLA 0  /* Use the MIT assembly syntax.  */
# define TARGET_VERSION fprintf (stderr, " (68k, MIT syntax)")
#endif

/* Handle --with-cpu default option from configure script.  */
#define OPTION_DEFAULT_SPECS						\
  { "cpu",   "%{!mc68000:%{!m68000:%{!m68302:%{!m68010:%{!mc68020:%{!m68020:\
%{!m68030:%{!m68040:%{!m68020-40:%{!m68020-60:%{!m68060:%{!mcpu32:\
%{!m68332:%{!m5200:%{!m5206e:%{!m528x:%{!m5307:%{!m5407:%{!mcfv4e:\
%{!mcpu=*:%{!march=*:-%(VALUE)}}}}}}}}}}}}}}}}}}}}}" },

/* Pass flags to gas indicating which type of processor we have.  This
   can be simplified when we can rely on the assembler supporting .cpu
   and .arch directives.  */

#define ASM_CPU_SPEC "\
%{m68851}%{mno-68851} %{m68881}%{mno-68881} %{msoft-float:-mno-float} \
%{m68000}%{m68302}%{mc68000}%{m68010}%{m68020}%{mc68020}%{m68030}\
%{m68040}%{m68020-40:-m68040}%{m68020-60:-m68040}\
%{m68060}%{mcpu32}%{m68332}%{m5200}%{m5206e}%{m528x}%{m5307}%{m5407}%{mcfv4e}\
%{mcpu=*:-mcpu=%*}%{march=*:-march=%*}\
"
#define ASM_PCREL_SPEC "%{fPIC|fpic|mpcrel:--pcrel} \
 %{msep-data|mid-shared-library:--pcrel} \
"

#define ASM_SPEC "%(asm_cpu_spec) %(asm_pcrel_spec)"

#define EXTRA_SPECS					\
  { "asm_cpu_spec", ASM_CPU_SPEC },			\
  { "asm_pcrel_spec", ASM_PCREL_SPEC },			\
  SUBTARGET_EXTRA_SPECS

#define SUBTARGET_EXTRA_SPECS

/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */

#define TARGET_CPU_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_define ("__m68k__");					\
      builtin_define_std ("mc68000");					\
      /* The other mc680x0 macros have traditionally been derived	\
	 from the tuning setting.  For example, -m68020-60 defines	\
	 m68060, even though it generates pure 68020 code.  */		\
      switch (m68k_tune)						\
	{								\
	case u68010:							\
	  builtin_define_std ("mc68010");				\
	  break;							\
									\
	case u68020:							\
	  builtin_define_std ("mc68020");				\
	  break;							\
									\
	case u68030:							\
	  builtin_define_std ("mc68030");				\
	  break;							\
									\
	case u68040:							\
	  builtin_define_std ("mc68040");				\
	  break;							\
									\
	case u68060:							\
	  builtin_define_std ("mc68060");				\
	  break;							\
									\
	case u68020_60:							\
	  builtin_define_std ("mc68060");				\
	  /* Fall through.  */						\
	case u68020_40:							\
	  builtin_define_std ("mc68040");				\
	  builtin_define_std ("mc68030");				\
	  builtin_define_std ("mc68020");				\
	  break;							\
									\
	case ucpu32:							\
	  builtin_define_std ("mc68332");				\
	  builtin_define_std ("mcpu32");				\
	  builtin_define_std ("mc68020");				\
	  break;							\
									\
	case ucfv1:							\
	  builtin_define ("__mcfv1__");					\
	  break;							\
									\
	case ucfv2:							\
	  builtin_define ("__mcfv2__");					\
	  break;							\
									\
    	case ucfv3:							\
	  builtin_define ("__mcfv3__");					\
	  break;							\
									\
	case ucfv4:							\
	  builtin_define ("__mcfv4__");					\
	  break;							\
									\
	case ucfv4e:							\
	  builtin_define ("__mcfv4e__");				\
	  break;							\
									\
	case ucfv5:							\
	  builtin_define ("__mcfv5__");					\
	  break;							\
									\
	default:							\
	  break;							\
	}								\
									\
      if (TARGET_68881)							\
	builtin_define ("__HAVE_68881__");				\
									\
      if (TARGET_COLDFIRE)						\
	{								\
	  const char *tmp;						\
	  								\
	  tmp = m68k_cpp_cpu_ident ("cf");			   	\
	  if (tmp)							\
	    builtin_define (tmp);					\
	  tmp = m68k_cpp_cpu_family ("cf");				\
	  if (tmp)							\
	    builtin_define (tmp);					\
	  builtin_define ("__mcoldfire__");				\
									\
	  if (TARGET_ISAC)						\
	    builtin_define ("__mcfisac__");				\
	  else if (TARGET_ISAB)						\
	    {								\
	      builtin_define ("__mcfisab__");				\
	      /* ISA_B: Legacy 5407 defines.  */			\
	      builtin_define ("__mcf5400__");				\
	      builtin_define ("__mcf5407__");				\
	    }								\
	  else if (TARGET_ISAAPLUS)					\
	    {								\
	      builtin_define ("__mcfisaaplus__");			\
	      /* ISA_A+: legacy defines.  */				\
	      builtin_define ("__mcf528x__");				\
	      builtin_define ("__mcf5200__");				\
	    }								\
	  else 								\
	    {								\
	      builtin_define ("__mcfisaa__");				\
	      /* ISA_A: legacy defines.  */				\
	      switch (m68k_tune)					\
		{							\
		case ucfv2:						\
		  builtin_define ("__mcf5200__");			\
		  break;						\
									\
		case ucfv3:						\
		  builtin_define ("__mcf5307__");			\
		  builtin_define ("__mcf5300__");			\
		  break;						\
									\
		default:						\
		  break;						\
		}							\
    	    }								\
	}								\
									\
      if (TARGET_COLDFIRE_FPU)						\
	builtin_define ("__mcffpu__");					\
									\
      if (TARGET_CF_HWDIV)						\
	builtin_define ("__mcfhwdiv__");				\
									\
      if (TARGET_FIDOA)							\
	builtin_define ("__mfido__");					\
									\
      builtin_assert ("cpu=m68k");					\
      builtin_assert ("machine=m68k");					\
    }									\
  while (0)

/* Classify the groups of pseudo-ops used to assemble QI, HI and SI
   quantities.  */
#define INT_OP_STANDARD	0	/* .byte, .short, .long */
#define INT_OP_DOT_WORD	1	/* .byte, .word, .long */
#define INT_OP_NO_DOT   2	/* byte, short, long */
#define INT_OP_DC	3	/* dc.b, dc.w, dc.l */

/* Set the default.  */
#define INT_OP_GROUP INT_OP_DOT_WORD

/* Bit values used by m68k-devices.def to identify processor capabilities.  */
#define FL_BITFIELD  (1 << 0)    /* Support bitfield instructions.  */
#define FL_68881     (1 << 1)    /* (Default) support for 68881/2.  */
#define FL_COLDFIRE  (1 << 2)    /* ColdFire processor.  */
#define FL_CF_HWDIV  (1 << 3)    /* ColdFire hardware divide supported.  */
#define FL_CF_MAC    (1 << 4)    /* ColdFire MAC unit supported.  */
#define FL_CF_EMAC   (1 << 5)    /* ColdFire eMAC unit supported.  */
#define FL_CF_EMAC_B (1 << 6)    /* ColdFire eMAC-B unit supported.  */
#define FL_CF_USP    (1 << 7)    /* ColdFire User Stack Pointer supported.  */
#define FL_CF_FPU    (1 << 8)    /* ColdFire FPU supported.  */
#define FL_ISA_68000 (1 << 9)
#define FL_ISA_68010 (1 << 10)
#define FL_ISA_68020 (1 << 11)
#define FL_ISA_68040 (1 << 12)
#define FL_ISA_A     (1 << 13)
#define FL_ISA_APLUS (1 << 14)
#define FL_ISA_B     (1 << 15)
#define FL_ISA_C     (1 << 16)
#define FL_FIDOA     (1 << 17)
#define FL_MMU 	     0   /* Used by multilib machinery.  */
#define FL_UCLINUX   0   /* Used by multilib machinery.  */

#define TARGET_68010		((m68k_cpu_flags & FL_ISA_68010) != 0)
#define TARGET_68020		((m68k_cpu_flags & FL_ISA_68020) != 0)
#define TARGET_68040		((m68k_cpu_flags & FL_ISA_68040) != 0)
#define TARGET_COLDFIRE		((m68k_cpu_flags & FL_COLDFIRE) != 0)
#define TARGET_COLDFIRE_FPU	(m68k_fpu == FPUTYPE_COLDFIRE)
#define TARGET_68881		(m68k_fpu == FPUTYPE_68881)
#define TARGET_FIDOA		((m68k_cpu_flags & FL_FIDOA) != 0)

/* Size (in bytes) of FPU registers.  */
#define TARGET_FP_REG_SIZE	(TARGET_COLDFIRE ? 8 : 12)

#define TARGET_ISAAPLUS		((m68k_cpu_flags & FL_ISA_APLUS) != 0)
#define TARGET_ISAB		((m68k_cpu_flags & FL_ISA_B) != 0)
#define TARGET_ISAC		((m68k_cpu_flags & FL_ISA_C) != 0)

/* Some instructions are common to more than one ISA.  */
#define ISA_HAS_MVS_MVZ	(TARGET_ISAB || TARGET_ISAC)
#define ISA_HAS_FF1	(TARGET_ISAAPLUS || TARGET_ISAC)

#define TUNE_68000	(m68k_tune == u68000)
#define TUNE_68010	(m68k_tune == u68010)
#define TUNE_68000_10	(TUNE_68000 || TUNE_68010)
#define TUNE_68030	(m68k_tune == u68030 \
			 || m68k_tune == u68020_40 \
			 || m68k_tune == u68020_60)
#define TUNE_68040	(m68k_tune == u68040 \
			 || m68k_tune == u68020_40 \
			 || m68k_tune == u68020_60)
#define TUNE_68060	(m68k_tune == u68060 || m68k_tune == u68020_60)
#define TUNE_68040_60	(TUNE_68040 || TUNE_68060)
#define TUNE_CPU32	(m68k_tune == ucpu32)
#define TUNE_CFV1       (m68k_tune == ucfv1)
#define TUNE_CFV2	(m68k_tune == ucfv2)
#define TUNE_CFV3       (m68k_tune == ucfv3)
#define TUNE_CFV4       (m68k_tune == ucfv4 || m68k_tune == ucfv4e)

#define TUNE_MAC	((m68k_tune_flags & FL_CF_MAC) != 0)
#define TUNE_EMAC	((m68k_tune_flags & FL_CF_EMAC) != 0)

#define OVERRIDE_OPTIONS   override_options()

/* These are meant to be redefined in the host dependent files */
#define SUBTARGET_OVERRIDE_OPTIONS

/* target machine storage layout */

/* "long double" is the same as "double" on ColdFire and fido
   targets.  */

#define LONG_DOUBLE_TYPE_SIZE			\
  ((TARGET_COLDFIRE || TARGET_FIDOA) ? 64 : 80)

/* We need to know the size of long double at compile-time in libgcc2.  */

#if defined(__mcoldfire__) || defined(__mfido__)
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 80
#endif

/* Set the value of FLT_EVAL_METHOD in float.h.  When using 68040 fp
   instructions, we get proper intermediate rounding, otherwise we
   get extended precision results.  */
#define TARGET_FLT_EVAL_METHOD ((TARGET_68040 || ! TARGET_68881) ? 0 : 2)

#define BITS_BIG_ENDIAN 1
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

#define UNITS_PER_WORD 4

#define PARM_BOUNDARY (TARGET_SHORT ? 16 : 32)
#define STACK_BOUNDARY 16
#define FUNCTION_BOUNDARY 16
#define EMPTY_FIELD_BOUNDARY 16
/* ColdFire and fido strongly prefer a 32-bit aligned stack.  */
#define PREFERRED_STACK_BOUNDARY \
  ((TARGET_COLDFIRE || TARGET_FIDOA) ? 32 : 16)

/* No data type wants to be aligned rounder than this.
   Most published ABIs say that ints should be aligned on 16-bit
   boundaries, but CPUs with 32-bit busses get better performance
   aligned on 32-bit boundaries.  */
#define BIGGEST_ALIGNMENT (TARGET_ALIGN_INT ? 32 : 16)

#define STRICT_ALIGNMENT (TARGET_STRICT_ALIGNMENT)
#define M68K_HONOR_TARGET_STRICT_ALIGNMENT 1

#define DWARF_CIE_DATA_ALIGNMENT -2

#define INT_TYPE_SIZE (TARGET_SHORT ? 16 : 32)

/* Define these to avoid dependence on meaning of `int'.  */
#define WCHAR_TYPE "long int"
#define WCHAR_TYPE_SIZE 32

/* Maximum number of library IDs we permit with -mid-shared-library.  */
#define MAX_LIBRARY_ID 255


/* Standard register usage.  */

/* For the m68k, we give the data registers numbers 0-7,
   the address registers numbers 010-017 (8-15),
   and the 68881 floating point registers numbers 020-027 (16-23).
   We also have a fake `arg-pointer' register 030 (24) used for
   register elimination.  */
#define FIRST_PSEUDO_REGISTER 25

/* All m68k targets (except AmigaOS) use %a5 as the PIC register  */
#define PIC_OFFSET_TABLE_REGNUM				\
  (!flag_pic ? INVALID_REGNUM				\
   : reload_completed ? REGNO (pic_offset_table_rtx)	\
   : PIC_REG)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the m68k, only the stack pointer is such.
   Our fake arg-pointer is obviously fixed as well.  */
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
  /* Arg pointer.  */          \
  1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS     \
 {/* Data registers.  */        \
  1, 1, 0, 0, 0, 0, 0, 0,       \
                                \
  /* Address registers.  */     \
  1, 1, 0, 0, 0, 0, 0, 1,       \
                                \
  /* Floating point registers   \
     (if available).  */        \
  1, 1, 0, 0, 0, 0, 0, 0,       \
                                \
  /* Arg pointer.  */           \
  1 }

#define REG_ALLOC_ORDER		\
{ /* d0/d1/a0/a1 */		\
  0, 1, 8, 9,			\
  /* d2-d7 */			\
  2, 3, 4, 5, 6, 7,		\
  /* a2-a7/arg */		\
  10, 11, 12, 13, 14, 15, 24,	\
  /* fp0-fp7 */			\
  16, 17, 18, 19, 20, 21, 22, 23\
}


/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compiler's mitts completely off it.  We don't bother to zero it out
   of register classes.  */
#define CONDITIONAL_REGISTER_USAGE				\
{								\
  int i;							\
  HARD_REG_SET x;						\
  if (!TARGET_HARD_FLOAT)					\
    {								\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FP_REGS]);	\
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)		\
        if (TEST_HARD_REG_BIT (x, i))				\
	  fixed_regs[i] = call_used_regs[i] = 1;		\
    }								\
  if (flag_pic)							\
    fixed_regs[PIC_REG] = call_used_regs[PIC_REG] = 1;		\
}

/* On the m68k, ordinary registers hold 32 bits worth;
   for the 68881 registers, a single register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 16 ? GET_MODE_NUNITS (MODE)	\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* A C expression that is nonzero if hard register NEW_REG can be
   considered for use as a rename register for OLD_REG register.  */

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  m68k_hard_regno_rename_ok (OLD_REG, NEW_REG)

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  m68k_regno_mode_ok ((REGNO), (MODE))

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X) \
  m68k_secondary_reload_class (CLASS, MODE, X)

#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (! TARGET_HARD_FLOAT					\
   || ((GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
	|| GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)	\
       == (GET_MODE_CLASS (MODE2) == MODE_FLOAT		\
	   || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT)))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

#define STACK_POINTER_REGNUM SP_REG

/* Most m68k targets use %a6 as a frame pointer.  The AmigaOS
   ABI uses %a6 for shared library calls, therefore the frame
   pointer is shifted to %a5 on this target.  */
#define FRAME_POINTER_REGNUM A6_REG

/* Base register for access to arguments of the function.
 * This isn't a hardware register. It will be eliminated to the
 * stack pointer or frame pointer.
 */
#define ARG_POINTER_REGNUM 24

#define STATIC_CHAIN_REGNUM A0_REG
#define M68K_STATIC_CHAIN_REG_NAME REGISTER_PREFIX "a0"

/* Register in which address to store a structure value
   is passed to a function.  */
#define M68K_STRUCT_VALUE_REGNUM A1_REG



/* The m68k has three kinds of registers, so eight classes would be
   a complete set.  One of them is not needed.  */
enum reg_class {
  NO_REGS, DATA_REGS,
  ADDR_REGS, FP_REGS,
  GENERAL_REGS, DATA_OR_FP_REGS,
  ADDR_OR_FP_REGS, ALL_REGS,
  LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
 { "NO_REGS", "DATA_REGS",              \
   "ADDR_REGS", "FP_REGS",              \
   "GENERAL_REGS", "DATA_OR_FP_REGS",   \
   "ADDR_OR_FP_REGS", "ALL_REGS" }

#define REG_CLASS_CONTENTS \
{					\
  {0x00000000},  /* NO_REGS */		\
  {0x000000ff},  /* DATA_REGS */	\
  {0x0100ff00},  /* ADDR_REGS */	\
  {0x00ff0000},  /* FP_REGS */		\
  {0x0100ffff},  /* GENERAL_REGS */	\
  {0x00ff00ff},  /* DATA_OR_FP_REGS */	\
  {0x01ffff00},  /* ADDR_OR_FP_REGS */	\
  {0x01ffffff},  /* ALL_REGS */		\
}

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) (regno_reg_class[(REGNO)])
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS ADDR_REGS

#define PREFERRED_RELOAD_CLASS(X,CLASS) \
  m68k_preferred_reload_class (X, CLASS)

/* On the m68k, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS ? 1 \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Moves between fp regs and other regs are two insns.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)	\
  ((((CLASS1) == FP_REGS) != ((CLASS2) == FP_REGS)) ? 4 : 2)

#define IRA_COVER_CLASSES						\
{									\
  ALL_REGS, LIM_REG_CLASSES						\
}

/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1
#define STARTING_FRAME_OFFSET 0

/* On the 680x0, sp@- in a byte insn really pushes a word.
   On the ColdFire, sp@- in a byte insn pushes just a byte.  */
#define PUSH_ROUNDING(BYTES) (TARGET_COLDFIRE ? BYTES : ((BYTES) + 1) & ~1)

#define FIRST_PARM_OFFSET(FNDECL) 8

/* On the 68000, the RTS insn cannot pop anything.
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

/* On the m68k the return value defaults to D0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), D0_REG)

/* On the m68k the return value defaults to D0.  */
#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, D0_REG)

/* On the m68k, D0 is usually the only register used.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == D0_REG)

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.
   XXX This macro is m68k specific and used only for m68kemb.h.  */
#define NEEDS_UNTYPED_CALL 0

/* On the m68k, all arguments are usually pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(N) 0

/* On the m68k, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */
#define CUMULATIVE_ARGS int

/* On the m68k, the offset starts at 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM) = 0)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

/* On the m68k all args are always pushed.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  asm_fprintf (FILE, "\tlea %LLP%d,%Ra0\n\tjsr mcount\n", (LABELNO))

#define EXIT_IGNORE_STACK 1

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   On the m68k, the trampoline looks like this:
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
   dispense with such notions and build a smaller trampoline.

   Since more instructions are required to move a template into
   place than to create it on the spot, don't use a template.  */

#define TRAMPOLINE_SIZE 12
#define TRAMPOLINE_ALIGNMENT 16

/* Targets redefine this to invoke code to either flush the cache,
   or enable stack execution (or both).  */
#ifndef FINALIZE_TRAMPOLINE
#define FINALIZE_TRAMPOLINE(TRAMP)
#endif

/* We generate a two-instructions program at address TRAMP :
	movea.l &CXT,%a0
	jmp FNADDR  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (HImode, TRAMP),				\
		  GEN_INT(0x207C + ((STATIC_CHAIN_REGNUM-8) << 9)));	\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 2)), CXT); \
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (TRAMP, 6)),	\
		  GEN_INT(0x4EF9));					\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 8)), FNADDR); \
  FINALIZE_TRAMPOLINE(TRAMP);						\
}

/* This is the library routine that is used to transfer control from the
   trampoline to the actual nested function.  It is defined for backward
   compatibility, for linking with object code that used the old trampoline
   definition.

   A colon is used with no explicit operands to cause the template string
   to be scanned for %-constructs.

   The function name __transfer_from_trampoline is not actually used.
   The function definition just permits use of "asm with operands"
   (though the operand list is empty).  */
#define TRANSFER_FROM_TRAMPOLINE				\
void								\
__transfer_from_trampoline ()					\
{								\
  register char *a0 asm (M68K_STATIC_CHAIN_REG_NAME);		\
  asm (GLOBAL_ASM_OP "___trampoline");				\
  asm ("___trampoline:");					\
  asm volatile ("move%.l %0,%@" : : "m" (a0[22]));		\
  asm volatile ("move%.l %1,%0" : "=a" (a0) : "m" (a0[18]));	\
  asm ("rts":);							\
}

/* There are two registers that can always be eliminated on the m68k.
   The frame pointer and the arg pointer can be replaced by either the
   hard frame pointer or to the stack pointer, depending upon the
   circumstances.  The hard frame pointer is not used before reload and
   so it is not eligible for elimination.  */
#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },		\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM },		\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  (OFFSET) = m68k_initial_elimination_offset(FROM, TO)

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_DECREMENT 1

/* Macros to check register numbers against specific register classes.  */

/* True for data registers, D0 through D7.  */
#define DATA_REGNO_P(REGNO)	IN_RANGE (REGNO, 0, 7)

/* True for address registers, A0 through A7.  */
#define ADDRESS_REGNO_P(REGNO)	IN_RANGE (REGNO, 8, 15)

/* True for integer registers, D0 through D7 and A0 through A7.  */
#define INT_REGNO_P(REGNO)	IN_RANGE (REGNO, 0, 15)

/* True for floating point registers, FP0 through FP7.  */
#define FP_REGNO_P(REGNO)	IN_RANGE (REGNO, 16, 23)

#define REGNO_OK_FOR_INDEX_P(REGNO)			\
  (INT_REGNO_P (REGNO)					\
   || INT_REGNO_P (reg_renumber[REGNO]))

#define REGNO_OK_FOR_BASE_P(REGNO)			\
  (ADDRESS_REGNO_P (REGNO)				\
   || ADDRESS_REGNO_P (reg_renumber[REGNO]))

#define REGNO_OK_FOR_INDEX_NONSTRICT_P(REGNO)		\
  (INT_REGNO_P (REGNO)					\
   || REGNO == ARG_POINTER_REGNUM			\
   || REGNO >= FIRST_PSEUDO_REGISTER)

#define REGNO_OK_FOR_BASE_NONSTRICT_P(REGNO)		\
  (ADDRESS_REGNO_P (REGNO)				\
   || REGNO == ARG_POINTER_REGNUM			\
   || REGNO >= FIRST_PSEUDO_REGISTER)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the m68k, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is a data register.  */
#define DATA_REG_P(X)	(REG_P (X) && DATA_REGNO_P (REGNO (X)))

/* 1 if X is an fp register.  */
#define FP_REG_P(X)	(REG_P (X) && FP_REGNO_P (REGNO (X)))

/* 1 if X is an address register  */
#define ADDRESS_REG_P(X) (REG_P (X) && ADDRESS_REGNO_P (REGNO (X)))

/* True if SYMBOL + OFFSET constants must refer to something within
   SYMBOL's section.  */
#ifndef M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P
#define M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P 0
#endif

#define MAX_REGS_PER_ADDRESS 2

#define CONSTANT_ADDRESS_P(X)						\
  ((GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
    || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
    || GET_CODE (X) == HIGH)						\
   && LEGITIMATE_CONSTANT_P (X))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
#define LEGITIMATE_CONSTANT_P(X)				\
  (GET_MODE (X) != XFmode					\
   && !m68k_illegitimate_symbolic_constant_p (X))

#ifndef REG_OK_STRICT
#define REG_STRICT_P 0
#else
#define REG_STRICT_P 1
#endif

#define LEGITIMATE_PIC_OPERAND_P(X)				\
  (!symbolic_operand (X, VOIDmode)				\
   || (TARGET_PCREL && REG_STRICT_P)				\
   || m68k_tls_reference_p (X, true))

#define REG_OK_FOR_BASE_P(X) \
  m68k_legitimate_base_reg_p (X, REG_STRICT_P)

#define REG_OK_FOR_INDEX_P(X) \
  m68k_legitimate_index_reg_p (X, REG_STRICT_P)


/* This address is OK as it stands.  */
#define PIC_CASE_VECTOR_ADDRESS(index) index
#define CASE_VECTOR_MODE HImode
#define CASE_VECTOR_PC_RELATIVE 1

#define DEFAULT_SIGNED_CHAR 1
#define MOVE_MAX 4
#define SLOW_BYTE_ACCESS 0

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* The ColdFire FF1 instruction returns 32 for zero. */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 32, 1)

#define STORE_FLAG_VALUE (-1)

#define Pmode SImode
#define FUNCTION_MODE QImode


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if the cc value is actually in the 68881, so a floating point
   conditional branch must be output.  */
#define CC_IN_68881 04000

/* On the 68000, all the insns to store in an address register fail to
   set the cc's.  However, in some cases these instructions can make it
   possibly invalid to use the saved cc's.  In those cases we clear out
   some or all of the saved cc's so they won't be used.  */
#define NOTICE_UPDATE_CC(EXP,INSN) notice_update_cc (EXP, INSN)

/* The shift instructions always clear the overflow bit.  */
#define CC_OVERFLOW_UNUSABLE 01000

/* The shift instructions use the carry bit in a way not compatible with
   conditional branches.  conditions.h uses CC_NO_OVERFLOW for this purpose.
   Rename it to something more understandable.  */
#define CC_NO_CARRY CC_NO_OVERFLOW

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
do { if (cc_prev_status.flags & CC_IN_68881)			\
    return FLOAT;						\
  if (cc_prev_status.flags & CC_NO_OVERFLOW)			\
    return NO_OV;						\
  return NORMAL; } while (0)

/* Control the assembler format that we output.  */

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"
#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"
#define GLOBAL_ASM_OP "\t.globl\t"
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX ""
#define USER_LABEL_PREFIX "_"
#define IMMEDIATE_PREFIX "#"

#define REGISTER_NAMES \
{REGISTER_PREFIX"d0", REGISTER_PREFIX"d1", REGISTER_PREFIX"d2",	\
 REGISTER_PREFIX"d3", REGISTER_PREFIX"d4", REGISTER_PREFIX"d5",	\
 REGISTER_PREFIX"d6", REGISTER_PREFIX"d7",			\
 REGISTER_PREFIX"a0", REGISTER_PREFIX"a1", REGISTER_PREFIX"a2", \
 REGISTER_PREFIX"a3", REGISTER_PREFIX"a4", REGISTER_PREFIX"a5", \
 REGISTER_PREFIX"a6", REGISTER_PREFIX"sp",			\
 REGISTER_PREFIX"fp0", REGISTER_PREFIX"fp1", REGISTER_PREFIX"fp2", \
 REGISTER_PREFIX"fp3", REGISTER_PREFIX"fp4", REGISTER_PREFIX"fp5", \
 REGISTER_PREFIX"fp6", REGISTER_PREFIX"fp7", REGISTER_PREFIX"argptr" }

#define M68K_FP_REG_NAME REGISTER_PREFIX"fp"

/* Return a register name by index, handling %fp nicely.
   We don't replace %fp for targets that don't map it to %a6
   since it may confuse GAS.  */
#define M68K_REGNAME(r) ( \
  ((FRAME_POINTER_REGNUM == A6_REG) \
    && ((r) == FRAME_POINTER_REGNUM) \
    && frame_pointer_needed) ? \
    M68K_FP_REG_NAME : reg_names[(r)])

/* On the Sun-3, the floating point registers have numbers
   18 to 25, not 16 to 23 as they do in the compiler.  */
#define DBX_REGISTER_NUMBER(REGNO) ((REGNO) < 16 ? (REGNO) : (REGNO) + 2)

/* Before the prologue, RA is at 0(%sp).  */
#define INCOMING_RETURN_ADDR_RTX \
  gen_rtx_MEM (VOIDmode, gen_rtx_REG (VOIDmode, STACK_POINTER_REGNUM))

/* After the prologue, RA is at 4(AP) in the current frame.  */
#define RETURN_ADDR_RTX(COUNT, FRAME)					   \
  ((COUNT) == 0								   \
   ? gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, UNITS_PER_WORD)) \
   : gen_rtx_MEM (Pmode, plus_constant (FRAME, UNITS_PER_WORD)))

/* We must not use the DBX register numbers for the DWARF 2 CFA column
   numbers because that maps to numbers beyond FIRST_PSEUDO_REGISTER.
   Instead use the identity mapping.  */
#define DWARF_FRAME_REGNUM(REG) \
  (INT_REGNO_P (REG) || FP_REGNO_P (REG) ? (REG) : INVALID_REGNUM)

/* The return column was originally 24, but gcc used 25 for a while too.
   Define both registers 24 and 25 as Pmode ones and use 24 in our own
   unwind information.  */
#define DWARF_FRAME_REGISTERS 25
#define DWARF_FRAME_RETURN_COLUMN 24
#define DWARF_ALT_FRAME_RETURN_COLUMN 25

/* Before the prologue, the top of the frame is at 4(%sp).  */
#define INCOMING_FRAME_SP_OFFSET 4

/* All registers are live on exit from an interrupt routine.  */
#define EPILOGUE_USES(REGNO)					\
  (reload_completed						\
   && (m68k_get_function_kind (current_function_decl)	\
       == m68k_fk_interrupt_handler))

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 2 ? (N) : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, A0_REG)
#define EH_RETURN_HANDLER_RTX					    \
  gen_rtx_MEM (Pmode,						    \
	       gen_rtx_PLUS (Pmode, arg_pointer_rtx,		    \
			     plus_constant (EH_RETURN_STACKADJ_RTX, \
					    UNITS_PER_WORD)))

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   TARGET_ID_SHARED_LIBRARY and TARGET_SEP_DATA are designed to support
   a read-only text segment without imposing a fixed gap between the
   text and data segments.  As a result, the text segment cannot refer
   to anything in the data segment, even in PC-relative form.  Because
   .eh_frame refers to both code and data, it follows that .eh_frame
   must be in the data segment itself, and that the offset between
   .eh_frame and code will not be a link-time constant.

   In theory, we could create a read-only .eh_frame by using DW_EH_PE_pcrel
   | DW_EH_PE_indirect for all code references.  However, gcc currently
   handles indirect references using a per-TU constant pool.  This means
   that if a function and its eh_frame are removed by the linker, the
   eh_frame's indirect references to the removed function will not be
   removed, leading to an unresolved symbol error.

   It isn't clear that any -msep-data or -mid-shared-library target
   would benefit from a read-only .eh_frame anyway.  In particular,
   no known target that supports these options has a feature like
   PT_GNU_RELRO.  Without any such feature to motivate them, indirect
   references would be unnecessary bloat, so we simply use an absolute
   pointer for code and global references.  We still use pc-relative
   references to data, as this avoids a relocation.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)			   \
  (flag_pic								   \
   && !((TARGET_ID_SHARED_LIBRARY || TARGET_SEP_DATA)			   \
	&& ((GLOBAL) || (CODE)))					   \
   ? ((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4 \
   : DW_EH_PE_absptr)

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  asm_fprintf (FILE, "%U%s", NAME)

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%s%ld", LOCAL_LABEL_PREFIX, PREFIX, (long)(NUM))

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)			\
  asm_fprintf (FILE, (MOTOROLA				\
		      ? "\tmove.l %s,-(%Rsp)\n"		\
		      : "\tmovel %s,%Rsp@-\n"),		\
	       reg_names[REGNO])

#define ASM_OUTPUT_REG_POP(FILE,REGNO)			\
  asm_fprintf (FILE, (MOTOROLA				\
		      ? "\tmove.l (%Rsp)+,%s\n"		\
		      : "\tmovel %Rsp@+,%s\n"),		\
	       reg_names[REGNO])

/* The m68k does not use absolute case-vectors, but we must define this macro
   anyway.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\t.long %LL%d\n", VALUE)

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  asm_fprintf (FILE, "\t.word %LL%d-%LL%d\n", VALUE, REL)

/* We don't have a way to align to more than a two-byte boundary, so do the
   best we can and don't complain.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) >= 1)			\
    fprintf (FILE, "\t.even\n");

#ifdef HAVE_GAS_BALIGN_AND_P2ALIGN
/* Use "move.l %a4,%a4" to advance within code.  */
#define ASM_OUTPUT_ALIGN_WITH_NOP(FILE,LOG)			\
  if ((LOG) > 0)						\
    fprintf ((FILE), "\t.balignw %u,0x284c\n", 1 << (LOG));
#endif

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (int)(SIZE))

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (int)(ROUNDED)))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (int)(ROUNDED)))

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  m68k_final_prescan_insn (INSN, OPVEC, NOPERANDS)

/* On the 68000, we use several CODE characters:
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
   '?' for m68k_library_id_string

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '.' || (CODE) == '#' || (CODE) == '-'			\
   || (CODE) == '+' || (CODE) == '@' || (CODE) == '!'			\
   || (CODE) == '$' || (CODE) == '&' || (CODE) == '/' || (CODE) == '?')


/* See m68k.c for the m68k specific codes.  */
#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

#define OUTPUT_ADDR_CONST_EXTRA(FILE, X, FAIL)		\
do {							\
  if (! m68k_output_addr_const_extra (FILE, (X)))	\
    goto FAIL;						\
} while (0);

/* Values used in the MICROARCH argument to M68K_DEVICE.  */
enum uarch_type
{
  u68000,
  u68010,
  u68020,
  u68020_40,
  u68020_60,
  u68030,
  u68040,
  u68060,
  ucpu32,
  ucfv1,
  ucfv2,
  ucfv3,
  ucfv4,
  ucfv4e,
  ucfv5,
  unk_arch
};

/* An enumeration of all supported target devices.  */
enum target_device
{
#define M68K_DEVICE(NAME,ENUM_VALUE,FAMILY,MULTILIB,MICROARCH,ISA,FLAGS) \
  ENUM_VALUE,
#include "m68k-devices.def"
#undef M68K_DEVICE
  unk_device
};

enum fpu_type
{
  FPUTYPE_NONE,
  FPUTYPE_68881,
  FPUTYPE_COLDFIRE
};

enum m68k_function_kind
{
  m68k_fk_normal_function,
  m68k_fk_interrupt_handler,
  m68k_fk_interrupt_thread
};

/* Variables in m68k.c; see there for details.  */
extern const char *m68k_library_id_string;
extern enum target_device m68k_cpu;
extern enum uarch_type m68k_tune;
extern enum fpu_type m68k_fpu;
extern unsigned int m68k_cpu_flags;
extern unsigned int m68k_tune_flags;
extern const char *m68k_symbolic_call;
extern const char *m68k_symbolic_jump;

enum M68K_SYMBOLIC_CALL { M68K_SYMBOLIC_CALL_NONE, M68K_SYMBOLIC_CALL_JSR,
			  M68K_SYMBOLIC_CALL_BSR_C, M68K_SYMBOLIC_CALL_BSR_P };

extern enum M68K_SYMBOLIC_CALL m68k_symbolic_call_var;

/* ??? HOST_WIDE_INT is not being defined for auto-generated files.
   Workaround that.  */
#ifdef HOST_WIDE_INT
typedef enum { MOVL, SWAP, NEGW, NOTW, NOTB, MOVQ, MVS, MVZ }
  M68K_CONST_METHOD;

extern M68K_CONST_METHOD m68k_const_method (HOST_WIDE_INT);
#endif

extern void m68k_emit_move_double (rtx [2]);

extern int m68k_sched_address_bypass_p (rtx, rtx);
extern int m68k_sched_indexed_address_bypass_p (rtx, rtx);

#define CPU_UNITS_QUERY 1
