/* Subroutines used for code generation on IA-32.
   Copyright (C) 1988, 92, 94-99, 2000 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. */

#include <setjmp.h>
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "toplev.h"
#include "basic-block.h"
#include "ggc.h"

/* True when we want to do pushes before allocating stack to get better
   scheduling.

   Saving registers first is win in the most cases except for LEAVE
   instruction.  Macro is 0 iff we will use LEAVE.  */

#define SAVED_REGS_FIRST \
  (!frame_pointer_needed || (!TARGET_USE_LEAVE && !optimize_size))


#ifdef EXTRA_CONSTRAINT
/* If EXTRA_CONSTRAINT is defined, then the 'S'
   constraint in REG_CLASS_FROM_LETTER will no longer work, and various
   asm statements that need 'S' for class SIREG will break.  */
 error EXTRA_CONSTRAINT conflicts with S constraint letter
/* The previous line used to be #error, but some compilers barf
   even if the conditional was untrue.  */
#endif

#ifndef CHECK_STACK_LIMIT
#define CHECK_STACK_LIMIT -1
#endif

/* Processor costs (relative to an add) */
struct processor_costs i386_cost = {	/* 386 specific costs */
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  3,					/* variable shift costs */
  2,					/* constant shift costs */
  6,					/* cost of starting a multiply */
  1,					/* cost of multiply per each bit set */
  23,					/* cost of a divide/mod */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {8, 8, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {8, 8, 8}				/* cost of loading integer registers */
};

struct processor_costs i486_cost = {	/* 486 specific costs */
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  3,					/* variable shift costs */
  2,					/* constant shift costs */
  12,					/* cost of starting a multiply */
  1,					/* cost of multiply per each bit set */
  40,					/* cost of a divide/mod */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {8, 8, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {8, 8, 8}				/* cost of loading integer registers */
};

struct processor_costs pentium_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  4,					/* variable shift costs */
  1,					/* constant shift costs */
  11,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  25,					/* cost of a divide/mod */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  6,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 6}				/* cost of loading integer registers */
};

struct processor_costs pentiumpro_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  1,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  17,					/* cost of a divide/mod */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  2,					/* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 2, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 6}				/* cost of loading integer registers */
};

struct processor_costs k6_cost = {
  1,					/* cost of an add instruction */
  2,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  3,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  18,					/* cost of a divide/mod */
  8,					/* "large" insn */
  4,					/* MOVE_RATIO */
  3,					/* cost for loading QImode using movzbl */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 3, 2},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {6, 6, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 4}				/* cost of loading integer registers */
};

struct processor_costs athlon_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  5,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  19,					/* cost of a divide/mod */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2). */
  {2, 3, 2},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {6, 6, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 4}				/* cost of loading integer registers */
};

struct processor_costs *ix86_cost = &pentium_cost;

/* Processor feature/optimization bitmasks.  */
#define m_386 (1<<PROCESSOR_I386)
#define m_486 (1<<PROCESSOR_I486)
#define m_PENT (1<<PROCESSOR_PENTIUM)
#define m_PPRO (1<<PROCESSOR_PENTIUMPRO)
#define m_K6  (1<<PROCESSOR_K6)
#define m_ATHLON  (1<<PROCESSOR_ATHLON)

const int x86_use_leave = m_386 | m_K6 | m_ATHLON;
const int x86_push_memory = m_386 | m_K6 | m_ATHLON;
const int x86_zero_extend_with_and = m_486 | m_PENT;
const int x86_movx = m_ATHLON /* m_386 | m_PPRO | m_K6 */;
const int x86_double_with_add = ~m_386;
const int x86_use_bit_test = m_386;
const int x86_unroll_strlen = m_486 | m_PENT | m_PPRO | m_ATHLON | m_K6;
const int x86_use_q_reg = m_PENT | m_PPRO | m_K6;
const int x86_use_any_reg = m_486;
const int x86_cmove = m_PPRO | m_ATHLON;
const int x86_deep_branch = m_PPRO | m_K6 | m_ATHLON;
const int x86_use_sahf = m_PPRO | m_K6 | m_ATHLON;
const int x86_partial_reg_stall = m_PPRO;
const int x86_use_loop = m_K6;
const int x86_use_fiop = ~(m_PPRO | m_ATHLON | m_PENT);
const int x86_use_mov0 = m_K6;
const int x86_use_cltd = ~(m_PENT | m_K6);
const int x86_read_modify_write = ~m_PENT;
const int x86_read_modify = ~(m_PENT | m_PPRO);
const int x86_split_long_moves = m_PPRO;
const int x86_promote_QImode = m_K6 | m_PENT | m_386 | m_486;
const int x86_single_stringop = m_386;

#define AT_BP(mode) (gen_rtx_MEM ((mode), hard_frame_pointer_rtx))

const char * const hi_reg_name[] = HI_REGISTER_NAMES;
const char * const qi_reg_name[] = QI_REGISTER_NAMES;
const char * const qi_high_reg_name[] = QI_HIGH_REGISTER_NAMES;

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS in i386.h. */

enum reg_class const regclass_map[FIRST_PSEUDO_REGISTER] =
{
  /* ax, dx, cx, bx */
  AREG, DREG, CREG, BREG,
  /* si, di, bp, sp */
  SIREG, DIREG, NON_Q_REGS, NON_Q_REGS,
  /* FP registers */
  FP_TOP_REG, FP_SECOND_REG, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,
  /* arg pointer */
  NON_Q_REGS,
  /* flags, fpsr, dirflag, frame */
  NO_REGS, NO_REGS, NO_REGS, NON_Q_REGS
};

/* The "default" register map.  */

int const dbx_register_map[FIRST_PSEUDO_REGISTER] = 
{
  0, 2, 1, 3, 6, 7, 4, 5,		/* general regs */
  12, 13, 14, 15, 16, 17, 18, 19,	/* fp regs */
  -1, -1, -1, -1,			/* arg, flags, fpsr, dir */
};

/* Define the register numbers to be used in Dwarf debugging information.
   The SVR4 reference port C compiler uses the following register numbers
   in its Dwarf output code:
	0 for %eax (gcc regno = 0)
	1 for %ecx (gcc regno = 2)
	2 for %edx (gcc regno = 1)
	3 for %ebx (gcc regno = 3)
	4 for %esp (gcc regno = 7)
	5 for %ebp (gcc regno = 6)
	6 for %esi (gcc regno = 4)
	7 for %edi (gcc regno = 5)
   The following three DWARF register numbers are never generated by
   the SVR4 C compiler or by the GNU compilers, but SDB on x86/svr4
   believes these numbers have these meanings.
	8  for %eip    (no gcc equivalent)
	9  for %eflags (gcc regno = 17)
	10 for %trapno (no gcc equivalent)
   It is not at all clear how we should number the FP stack registers
   for the x86 architecture.  If the version of SDB on x86/svr4 were
   a bit less brain dead with respect to floating-point then we would
   have a precedent to follow with respect to DWARF register numbers
   for x86 FP registers, but the SDB on x86/svr4 is so completely
   broken with respect to FP registers that it is hardly worth thinking
   of it as something to strive for compatibility with.
   The version of x86/svr4 SDB I have at the moment does (partially)
   seem to believe that DWARF register number 11 is associated with
   the x86 register %st(0), but that's about all.  Higher DWARF
   register numbers don't seem to be associated with anything in
   particular, and even for DWARF regno 11, SDB only seems to under-
   stand that it should say that a variable lives in %st(0) (when
   asked via an `=' command) if we said it was in DWARF regno 11,
   but SDB still prints garbage when asked for the value of the
   variable in question (via a `/' command).
   (Also note that the labels SDB prints for various FP stack regs
   when doing an `x' command are all wrong.)
   Note that these problems generally don't affect the native SVR4
   C compiler because it doesn't allow the use of -O with -g and
   because when it is *not* optimizing, it allocates a memory
   location for each floating-point variable, and the memory
   location is what gets described in the DWARF AT_location
   attribute for the variable in question.
   Regardless of the severe mental illness of the x86/svr4 SDB, we
   do something sensible here and we use the following DWARF
   register numbers.  Note that these are all stack-top-relative
   numbers.
	11 for %st(0) (gcc regno = 8)
	12 for %st(1) (gcc regno = 9)
	13 for %st(2) (gcc regno = 10)
	14 for %st(3) (gcc regno = 11)
	15 for %st(4) (gcc regno = 12)
	16 for %st(5) (gcc regno = 13)
	17 for %st(6) (gcc regno = 14)
	18 for %st(7) (gcc regno = 15)
*/
int const svr4_dbx_register_map[FIRST_PSEUDO_REGISTER] = 
{
  0, 2, 1, 3, 6, 7, 5, 4,		/* general regs */
  11, 12, 13, 14, 15, 16, 17, 18,	/* fp regs */
  -1, 9, -1, -1,			/* arg, flags, fpsr, dir */
};



/* Test and compare insns in i386.md store the information needed to
   generate branch and scc insns here.  */

struct rtx_def *ix86_compare_op0 = NULL_RTX;
struct rtx_def *ix86_compare_op1 = NULL_RTX;

#define MAX_386_STACK_LOCALS 2

/* Define the structure for the machine field in struct function.  */
struct machine_function
{
  rtx stack_locals[(int) MAX_MACHINE_MODE][MAX_386_STACK_LOCALS];
};

#define ix86_stack_locals (cfun->machine->stack_locals)

/* which cpu are we scheduling for */
enum processor_type ix86_cpu;

/* which instruction set architecture to use.  */
int ix86_arch;

/* Strings to hold which cpu and instruction set architecture  to use.  */
const char *ix86_cpu_string;		/* for -mcpu=<xxx> */
const char *ix86_arch_string;		/* for -march=<xxx> */

/* Register allocation order */
const char *ix86_reg_alloc_order;
static char regs_allocated[FIRST_PSEUDO_REGISTER];

/* # of registers to use to pass arguments. */
const char *ix86_regparm_string;

/* ix86_regparm_string as a number */
int ix86_regparm;

/* Alignment to use for loops and jumps:  */

/* Power of two alignment for loops. */
const char *ix86_align_loops_string;

/* Power of two alignment for non-loop jumps. */
const char *ix86_align_jumps_string;

/* Power of two alignment for stack boundary in bytes.  */
const char *ix86_preferred_stack_boundary_string;

/* Preferred alignment for stack boundary in bits.  */
int ix86_preferred_stack_boundary;

/* Values 1-5: see jump.c */
int ix86_branch_cost;
const char *ix86_branch_cost_string;

/* Power of two alignment for functions. */
int ix86_align_funcs;
const char *ix86_align_funcs_string;

/* Power of two alignment for loops. */
int ix86_align_loops;

/* Power of two alignment for non-loop jumps. */
int ix86_align_jumps;

static void output_pic_addr_const PARAMS ((FILE *, rtx, int));
static void put_condition_code PARAMS ((enum rtx_code, enum machine_mode,
				       int, int, FILE *));
static enum rtx_code unsigned_comparison PARAMS ((enum rtx_code code));
static rtx ix86_expand_int_compare PARAMS ((enum rtx_code, rtx, rtx));
static rtx ix86_expand_fp_compare PARAMS ((enum rtx_code, rtx, rtx, int));
static rtx ix86_expand_compare PARAMS ((enum rtx_code, int));
static rtx gen_push PARAMS ((rtx));
static int memory_address_length PARAMS ((rtx addr));
static int ix86_flags_dependant PARAMS ((rtx, rtx, enum attr_type));
static int ix86_agi_dependant PARAMS ((rtx, rtx, enum attr_type));
static int ix86_safe_length PARAMS ((rtx));
static enum attr_memory ix86_safe_memory PARAMS ((rtx));
static enum attr_pent_pair ix86_safe_pent_pair PARAMS ((rtx));
static enum attr_ppro_uops ix86_safe_ppro_uops PARAMS ((rtx));
static void ix86_dump_ppro_packet PARAMS ((FILE *));
static void ix86_reorder_insn PARAMS ((rtx *, rtx *));
static rtx * ix86_pent_find_pair PARAMS ((rtx *, rtx *, enum attr_pent_pair,
					 rtx));
static void ix86_init_machine_status PARAMS ((struct function *));
static void ix86_mark_machine_status PARAMS ((struct function *));
static void ix86_split_to_parts PARAMS ((rtx, rtx *, enum machine_mode));
static int ix86_safe_length_prefix PARAMS ((rtx));
static HOST_WIDE_INT ix86_compute_frame_size PARAMS((HOST_WIDE_INT,
						     int *, int *, int *));
static int ix86_nsaved_regs PARAMS((void));
static void ix86_emit_save_regs PARAMS((void));
static void ix86_emit_restore_regs PARAMS((void));
static void ix86_emit_epilogue_esp_adjustment PARAMS((int));

struct ix86_address
{
  rtx base, index, disp;
  HOST_WIDE_INT scale;
};

static int ix86_decompose_address PARAMS ((rtx, struct ix86_address *));

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options ()
{
  /* Comes from final.c -- no real reason to change it.  */
#define MAX_CODE_ALIGN 16

  static struct ptt
    {
      struct processor_costs *cost;	/* Processor costs */
      int target_enable;		/* Target flags to enable.  */
      int target_disable;		/* Target flags to disable.  */
      int align_loop;			/* Default alignments.  */
      int align_jump;
      int align_func;
      int branch_cost;
    }
  const processor_target_table[PROCESSOR_max] = 
    {
      {&i386_cost, 0, 0, 2, 2, 2, 1},
      {&i486_cost, 0, 0, 4, 4, 4, 1},
      {&pentium_cost, 0, 0, -4, -4, -4, 1},
      {&pentiumpro_cost, 0, 0, 4, -4, 4, 1},
      {&k6_cost, 0, 0, -5, -5, 4, 1},
      {&athlon_cost, 0, 0, 4, -4, 4, 1}
    };

  static struct pta
    {
      const char *name;		/* processor name or nickname. */
      enum processor_type processor;
    }
  const processor_alias_table[] = 
    {
      {"i386", PROCESSOR_I386},
      {"i486", PROCESSOR_I486},
      {"i586", PROCESSOR_PENTIUM},
      {"pentium", PROCESSOR_PENTIUM},
      {"i686", PROCESSOR_PENTIUMPRO},
      {"pentiumpro", PROCESSOR_PENTIUMPRO},
      {"k6", PROCESSOR_K6},
      {"athlon", PROCESSOR_ATHLON},
    };

  int const pta_size = sizeof(processor_alias_table)/sizeof(struct pta);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  ix86_arch = PROCESSOR_I386;
  ix86_cpu = (enum processor_type) TARGET_CPU_DEFAULT;

  if (ix86_arch_string != 0)
    {
      int i;
      for (i = 0; i < pta_size; i++)
	if (! strcmp (ix86_arch_string, processor_alias_table[i].name))
	  {
	    ix86_arch = processor_alias_table[i].processor;
	    /* Default cpu tuning to the architecture.  */
	    ix86_cpu = ix86_arch;
	    break;
	  }
      if (i == pta_size)
	error ("bad value (%s) for -march= switch", ix86_arch_string);
    }

  if (ix86_cpu_string != 0)
    {
      int i;
      for (i = 0; i < pta_size; i++)
	if (! strcmp (ix86_cpu_string, processor_alias_table[i].name))
	  {
	    ix86_cpu = processor_alias_table[i].processor;
	    break;
	  }
      if (i == pta_size)
	error ("bad value (%s) for -mcpu= switch", ix86_cpu_string);
    }

  ix86_cost = processor_target_table[ix86_cpu].cost;
  target_flags |= processor_target_table[ix86_cpu].target_enable;
  target_flags &= ~processor_target_table[ix86_cpu].target_disable;

  /* Arrange to set up i386_stack_locals for all functions.  */
  init_machine_status = ix86_init_machine_status;
  mark_machine_status = ix86_mark_machine_status;

  /* Validate registers in register allocation order.  */
  if (ix86_reg_alloc_order)
    {
      int i, ch;
      for (i = 0; (ch = ix86_reg_alloc_order[i]) != '\0'; i++)
	{
	  int regno = 0;

	  switch (ch)
	    {
	    case 'a':	regno = 0;	break;
	    case 'd':	regno = 1;	break;
	    case 'c':	regno = 2;	break;
	    case 'b':	regno = 3;	break;
	    case 'S':	regno = 4;	break;
	    case 'D':	regno = 5;	break;
	    case 'B':	regno = 6;	break;

	    default:	fatal ("Register '%c' is unknown", ch);
	    }

	  if (regs_allocated[regno])
	    fatal ("Register '%c' already specified in allocation order", ch);

	  regs_allocated[regno] = 1;
	}
    }

  /* Validate -mregparm= value. */
  if (ix86_regparm_string)
    {
      ix86_regparm = atoi (ix86_regparm_string);
      if (ix86_regparm < 0 || ix86_regparm > REGPARM_MAX)
	fatal ("-mregparm=%d is not between 0 and %d",
	       ix86_regparm, REGPARM_MAX);
    }

  /* Validate -malign-loops= value, or provide default.  */
  ix86_align_loops = processor_target_table[ix86_cpu].align_loop;
  if (ix86_align_loops_string)
    {
      ix86_align_loops = atoi (ix86_align_loops_string);
      if (ix86_align_loops < 0 || ix86_align_loops > MAX_CODE_ALIGN)
	fatal ("-malign-loops=%d is not between 0 and %d",
	       ix86_align_loops, MAX_CODE_ALIGN);
    }

  /* Validate -malign-jumps= value, or provide default.  */
  ix86_align_jumps = processor_target_table[ix86_cpu].align_jump;
  if (ix86_align_jumps_string)
    {
      ix86_align_jumps = atoi (ix86_align_jumps_string);
      if (ix86_align_jumps < 0 || ix86_align_jumps > MAX_CODE_ALIGN)
	fatal ("-malign-jumps=%d is not between 0 and %d",
	       ix86_align_jumps, MAX_CODE_ALIGN);
    }

  /* Validate -malign-functions= value, or provide default. */
  ix86_align_funcs = processor_target_table[ix86_cpu].align_func;
  if (ix86_align_funcs_string)
    {
      ix86_align_funcs = atoi (ix86_align_funcs_string);
      if (ix86_align_funcs < 0 || ix86_align_funcs > MAX_CODE_ALIGN)
	fatal ("-malign-functions=%d is not between 0 and %d",
	       ix86_align_funcs, MAX_CODE_ALIGN);
    }

  /* Validate -mpreferred-stack-boundary= value, or provide default.
     The default of 128 bits is for Pentium III's SSE __m128.  */
  ix86_preferred_stack_boundary = 128;
  if (ix86_preferred_stack_boundary_string)
    {
      int i = atoi (ix86_preferred_stack_boundary_string);
      if (i < 2 || i > 31)
	fatal ("-mpreferred-stack-boundary=%d is not between 2 and 31", i);
      ix86_preferred_stack_boundary = (1 << i) * BITS_PER_UNIT;
    }

  /* Validate -mbranch-cost= value, or provide default. */
  ix86_branch_cost = processor_target_table[ix86_cpu].branch_cost;
  if (ix86_branch_cost_string)
    {
      ix86_branch_cost = atoi (ix86_branch_cost_string);
      if (ix86_branch_cost < 0 || ix86_branch_cost > 5)
	fatal ("-mbranch-cost=%d is not between 0 and 5",
	       ix86_branch_cost);
    }

  /* Keep nonleaf frame pointers.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER)
    flag_omit_frame_pointer = 1;

  /* If we're doing fast math, we don't care about comparison order
     wrt NaNs.  This lets us use a shorter comparison sequence.  */
  if (flag_fast_math)
    target_flags &= ~MASK_IEEE_FP;

  /* If we're planning on using `loop', use it.  */
  if (TARGET_USE_LOOP && optimize)
    flag_branch_on_count_reg = 1;
}

/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */

void
order_regs_for_local_alloc ()
{
  int i, ch, order;

  /* User specified the register allocation order.  */

  if (ix86_reg_alloc_order)
    {
      for (i = order = 0; (ch = ix86_reg_alloc_order[i]) != '\0'; i++)
	{
	  int regno = 0;

	  switch (ch)
	    {
	    case 'a':	regno = 0;	break;
	    case 'd':	regno = 1;	break;
	    case 'c':	regno = 2;	break;
	    case 'b':	regno = 3;	break;
	    case 'S':	regno = 4;	break;
	    case 'D':	regno = 5;	break;
	    case 'B':	regno = 6;	break;
	    }

	  reg_alloc_order[order++] = regno;
	}

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
	  if (! regs_allocated[i])
	    reg_alloc_order[order++] = i;
	}
    }

  /* If user did not specify a register allocation order, use natural order. */
  else
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	reg_alloc_order[i] = i;
    }
}

void
optimization_options (level, size)
     int level;
     int size ATTRIBUTE_UNUSED;
{
  /* For -O2 and beyond, turn off -fschedule-insns by default.  It tends to
     make the problem with not enough registers even worse.  */
#ifdef INSN_SCHEDULING
  if (level > 1)
    flag_schedule_insns = 0;
#endif
}

/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for DECL.  The attributes in ATTRIBUTES have previously been
   assigned to DECL.  */

int
ix86_valid_decl_attribute_p (decl, attributes, identifier, args)
     tree decl ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier ATTRIBUTE_UNUSED;
     tree args ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for TYPE.  The attributes in ATTRIBUTES have previously been
   assigned to TYPE.  */

int
ix86_valid_type_attribute_p (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != METHOD_TYPE
      && TREE_CODE (type) != FIELD_DECL
      && TREE_CODE (type) != TYPE_DECL)
    return 0;

  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  if (is_attribute_p ("stdcall", identifier))
    return (args == NULL_TREE);

  /* Cdecl attribute says the callee is a normal C declaration. */
  if (is_attribute_p ("cdecl", identifier))
    return (args == NULL_TREE);

  /* Regparm attribute specifies how many integer arguments are to be
     passed in registers. */
  if (is_attribute_p ("regparm", identifier))
    {
      tree cst;

      if (! args || TREE_CODE (args) != TREE_LIST
	  || TREE_CHAIN (args) != NULL_TREE
	  || TREE_VALUE (args) == NULL_TREE)
	return 0;

      cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != INTEGER_CST)
	return 0;

      if (TREE_INT_CST_HIGH (cst) != 0
	  || TREE_INT_CST_LOW (cst) < 0
	  || TREE_INT_CST_LOW (cst) > REGPARM_MAX)
	return 0;

      return 1;
    }

  return 0;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

int
ix86_comp_type_attributes (type1, type2)
     tree type1;
     tree type2;
{
  /* Check for mismatch of non-default calling convention. */
  const char *rtdstr = TARGET_RTD ? "cdecl" : "stdcall";

  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched return types (cdecl vs stdcall).  */
  if (!lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type1))
      != !lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type2)))
    return 0;
  return 1;
}

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

int
ix86_return_pops_args (fundecl, funtype, size)
     tree fundecl;
     tree funtype;
     int size;
{
  int rtd = TARGET_RTD && (!fundecl || TREE_CODE (fundecl) != IDENTIFIER_NODE);

    /* Cdecl functions override -mrtd, and never pop the stack. */
  if (! lookup_attribute ("cdecl", TYPE_ATTRIBUTES (funtype))) {

    /* Stdcall functions will pop the stack if not variable args. */
    if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (funtype)))
      rtd = 1;

    if (rtd
        && (TYPE_ARG_TYPES (funtype) == NULL_TREE
	    || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype)))
		== void_type_node)))
      return size;
  }

  /* Lose any fake structure return argument.  */
  if (aggregate_value_p (TREE_TYPE (funtype)))
    return GET_MODE_SIZE (Pmode);

    return 0;
}

/* Argument support functions.  */

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;	/* Argument info to initialize */
     tree fntype;		/* tree ptr for function decl */
     rtx libname;		/* SYMBOL_REF of library name or 0 */
{
  static CUMULATIVE_ARGS zero_cum;
  tree param, next_param;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args (");
      if (fntype)
	fprintf (stderr, "fntype code = %s, ret code = %s",
		 tree_code_name[(int) TREE_CODE (fntype)],
		 tree_code_name[(int) TREE_CODE (TREE_TYPE (fntype))]);
      else
	fprintf (stderr, "no fntype");

      if (libname)
	fprintf (stderr, ", libname = %s", XSTR (libname, 0));
    }

  *cum = zero_cum;

  /* Set up the number of registers to use for passing arguments.  */
  cum->nregs = ix86_regparm;
  if (fntype)
    {
      tree attr = lookup_attribute ("regparm", TYPE_ATTRIBUTES (fntype));

      if (attr)
	cum->nregs = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr)));
    }

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  If there are variable arguments, then
     we won't pass anything in registers */

  if (cum->nregs)
    {
      for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
	   param != 0; param = next_param)
	{
	  next_param = TREE_CHAIN (param);
	  if (next_param == 0 && TREE_VALUE (param) != void_type_node)
	    cum->nregs = 0;
	}
    }

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, ", nregs=%d )\n", cum->nregs);

  return;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_adv (sz=%d, wds=%2d, nregs=%d, mode=%s, named=%d)\n\n",
	     words, cum->words, cum->nregs, GET_MODE_NAME (mode), named);

  cum->words += words;
  cum->nregs -= words;
  cum->regno += words;

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->regno = 0;
    }

  return;
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
    (otherwise it is an extra parameter matching an ellipsis).  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  rtx ret   = NULL_RTX;
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  switch (mode)
    {
      /* For now, pass fp/complex values on the stack. */
    default:
      break;

    case BLKmode:
    case DImode:
    case SImode:
    case HImode:
    case QImode:
      if (words <= cum->nregs)
	ret = gen_rtx_REG (mode, cum->regno);
      break;
    }

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr,
	       "function_arg (size=%d, wds=%2d, nregs=%d, mode=%4s, named=%d",
	       words, cum->words, cum->nregs, GET_MODE_NAME (mode), named);

      if (ret)
	fprintf (stderr, ", reg=%%e%s", reg_names[ REGNO(ret) ]);
      else
	fprintf (stderr, ", stack");

      fprintf (stderr, " )\n");
    }

  return ret;
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF
	  || (GET_CODE (op) == UNSPEC
	      && XINT (op, 1) >= 6
	      && XINT (op, 1) <= 7))
	return 1;
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return 0;

      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      /* Only @GOTOFF gets offsets.  */
      if (GET_CODE (op) != UNSPEC
	  || XINT (op, 1) != 7)
	return 0;

      op = XVECEXP (op, 0, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      return 0;

    default:
      return 0;
    }
}

/* Return true if the operand contains a @GOT or @GOTOFF reference.  */

int
pic_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST)
    {
      op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC)
	return 1;
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return 0;
      op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC)
	return 1;
    }
  return 0;
}

/* Test for a valid operand for a call instruction.  Don't allow the
   arg pointer register or virtual regs since they may decay into
   reg + const, which the patterns can't handle.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);

  /* Disallow indirect through a virtual register.  This leads to
     compiler aborts when trying to eliminate them.  */
  if (GET_CODE (op) == REG
      && (op == arg_pointer_rtx
	  || op == frame_pointer_rtx
	  || (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      && REGNO (op) <= LAST_VIRTUAL_REGISTER)))
    return 0;

  /* Disallow `call 1234'.  Due to varying assembler lameness this
     gets either rejected or translated to `call .+1234'.  */
  if (GET_CODE (op) == CONST_INT)
    return 0;

  /* Otherwise we can allow any general_operand in the address.  */
  return general_operand (op, Pmode);
}

/* Like call_insn_operand but allow (mem (symbol_ref ...)) even if pic.  */

int
expander_call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM
      && GET_CODE (XEXP (op, 0)) == SYMBOL_REF)
    return 1;

  return call_insn_operand (op, mode);
}

int
constant_call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == MEM && CONSTANT_ADDRESS_P (XEXP (op, 0));
}

/* Match exactly zero and one.  */

int 
const0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}

int 
const1_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return op == const1_rtx;
}

/* Match 2, 4, or 8.  Used for leal multiplicands.  */

int
const248_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8));
}

/* True if this is a constant appropriate for an increment or decremenmt.  */

int
incdec_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (op == const1_rtx || op == constm1_rtx)
    return 1;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  if (mode == SImode && INTVAL (op) == (HOST_WIDE_INT) 0xffffffff)
    return 1;
  if (mode == HImode && INTVAL (op) == (HOST_WIDE_INT) 0xffff)
    return 1;
  if (mode == QImode && INTVAL (op) == (HOST_WIDE_INT) 0xff)
    return 1;
  return 0;
}

/* Return false if this is the stack pointer, or any other fake 
   register eliminable to the stack pointer.  Otherwise, this is
   a register operand.

   This is used to prevent esp from being used as an index reg.
   Which would only happen in pathological cases.  */

int
reg_no_sp_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == stack_pointer_rtx || t == arg_pointer_rtx || t == frame_pointer_rtx)
    return 0;

  return register_operand (op, mode);
}

/* Return false if this is any eliminable register.  Otherwise
   general_operand.  */

int
general_no_elim_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == arg_pointer_rtx || t == frame_pointer_rtx
      || t == virtual_incoming_args_rtx || t == virtual_stack_vars_rtx
      || t == virtual_stack_dynamic_rtx)
    return 0;

  return general_operand (op, mode);
}

/* Return false if this is any eliminable register.  Otherwise
   register_operand or const_int.  */

int
nonmemory_no_elim_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == arg_pointer_rtx || t == frame_pointer_rtx
      || t == virtual_incoming_args_rtx || t == virtual_stack_vars_rtx
      || t == virtual_stack_dynamic_rtx)
    return 0;

  return GET_CODE (op) == CONST_INT || register_operand (op, mode);
}

/* Return true if op is a Q_REGS class register.  */

int
q_regs_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return QI_REG_P (op);
}     

/* Return true if op is a NON_Q_REGS class register.  */

int
non_q_regs_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return NON_QI_REG_P (op);
}     

/* Return 1 if OP is a comparison operator that can use the condition code
   generated by a logical operation, which characteristicly does not set
   overflow or carry.  To be used with CCNOmode.  */

int
no_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && GET_RTX_CLASS (GET_CODE (op)) == '<'
	  && GET_CODE (op) != LE
	  && GET_CODE (op) != GT);
}

/* Return 1 if OP is a comparison operator that can be issued by fcmov.  */

int
fcmov_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && GET_RTX_CLASS (GET_CODE (op)) == '<'
	  && GET_CODE (op) == unsigned_condition (GET_CODE (op)));
}

/* Return 1 if OP is a binary operator that can be promoted to wider mode.  */

int
promotable_binary_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case MULT:
      /* Modern CPUs have same latency for HImode and SImode multiply,
         but 386 and 486 do HImode multiply faster.  */
      return ix86_cpu > PROCESSOR_I486;
    case PLUS:
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
      return 1;
    default:
      return 0;
    }
}

/* Nearly general operand, but accept any const_double, since we wish
   to be able to drop them into memory rather than have them get pulled
   into registers.  */

int
cmp_fp_expander_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == CONST_DOUBLE)
    return 1;
  return general_operand (op, mode);
}

/* Match an SI or HImode register for a zero_extract.  */

int
ext_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_MODE (op) != SImode && GET_MODE (op) != HImode)
    return 0;
  return register_operand (op, VOIDmode);
}

/* Return 1 if this is a valid binary floating-point operation.
   OP is the expression matched, and MODE is its mode. */

int
binary_fp_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      return GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT;

    default:
      return 0;
    }
}

int
mult_operator(op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == MULT;
}

int
div_operator(op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == DIV;
}

int
arith_or_logical_operator (op, mode)
      rtx op;
      enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
          && (GET_RTX_CLASS (GET_CODE (op)) == 'c'
              || GET_RTX_CLASS (GET_CODE (op)) == '2'));
}

/* Returns 1 if OP is memory operand with a displacement.  */

int
memory_displacement_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  struct ix86_address parts;

  if (! memory_operand (op, mode))
    return 0;

  if (! ix86_decompose_address (XEXP (op, 0), &parts))
    abort ();

  return parts.disp != NULL_RTX;
}

/* To avoid problems when jump re-emits comparisons like testqi_ext_0,
   re-recognize the operand to avoid a copy_to_mode_reg that will fail.

   ??? It seems likely that this will only work because cmpsi is an
   expander, and no actual insns use this.  */

int
cmpsi_operand (op, mode)
      rtx op;
      enum machine_mode mode;
{
  if (general_operand (op, mode))
    return 1;

  if (GET_CODE (op) == AND
      && GET_MODE (op) == SImode
      && GET_CODE (XEXP (op, 0)) == ZERO_EXTRACT
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (XEXP (op, 0), 2)) == CONST_INT
      && INTVAL (XEXP (XEXP (op, 0), 1)) == 8
      && INTVAL (XEXP (XEXP (op, 0), 2)) == 8
      && GET_CODE (XEXP (op, 1)) == CONST_INT)
    return 1;

  return 0;
}

/* Returns 1 if OP is memory operand that can not be represented by the
   modRM array.  */

int
long_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (! memory_operand (op, mode))
    return 0;

  return memory_address_length (op) != 0;
}

/* Return nonzero if the rtx is known aligned.  */

int
aligned_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  struct ix86_address parts;

  if (!general_operand (op, mode))
    return 0;

  /* Registers and immediate operands are always "aligned". */
  if (GET_CODE (op) != MEM)
    return 1;

  /* Don't even try to do any aligned optimizations with volatiles. */
  if (MEM_VOLATILE_P (op))
    return 0;

  op = XEXP (op, 0);

  /* Pushes and pops are only valid on the stack pointer.  */
  if (GET_CODE (op) == PRE_DEC
      || GET_CODE (op) == POST_INC)
    return 1;

  /* Decode the address.  */
  if (! ix86_decompose_address (op, &parts))
    abort ();

  /* Look for some component that isn't known to be aligned.  */
  if (parts.index)
    {
      if (parts.scale < 4
	  && REGNO_POINTER_ALIGN (REGNO (parts.index)) < 4)
	return 0;
    }
  if (parts.base)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.base)) < 4)
	return 0;
    }
  if (parts.disp)
    {
      if (GET_CODE (parts.disp) != CONST_INT
	  || (INTVAL (parts.disp) & 3) != 0)
	return 0;
    }

  /* Didn't find one -- this must be an aligned address.  */
  return 1;
}

/* Return true if the constant is something that can be loaded with
   a special instruction.  Only handle 0.0 and 1.0; others are less
   worthwhile.  */

int
standard_80387_constant_p (x)
     rtx x;
{
  if (GET_CODE (x) != CONST_DOUBLE)
    return -1;

#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  {
    REAL_VALUE_TYPE d;
    jmp_buf handler;
    int is0, is1;

    if (setjmp (handler))
      return 0;

    set_float_handler (handler);
    REAL_VALUE_FROM_CONST_DOUBLE (d, x);
    is0 = REAL_VALUES_EQUAL (d, dconst0) && !REAL_VALUE_MINUS_ZERO (d);
    is1 = REAL_VALUES_EQUAL (d, dconst1);
    set_float_handler (NULL_PTR);

    if (is0)
      return 1;

    if (is1)
      return 2;

    /* Note that on the 80387, other constants, such as pi,
       are much slower to load as standard constants
       than to load from doubles in memory!  */
    /* ??? Not true on K6: all constants are equal cost.  */
  }
#endif

  return 0;
}

/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (op)
     rtx op;
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Return 1 if it is appropriate to emit `ret' instructions in the
   body of a function.  Do this only if the epilogue is simple, needing a
   couple of insns.  Prior to reloading, we can't tell how many registers
   must be saved, so return 0 then.  Return 0 if there is no frame
   marker to de-allocate.

   If NON_SAVING_SETJMP is defined and true, then it is not possible
   for the epilogue to be simple, so return 0.  This is a special case
   since NON_SAVING_SETJMP will not cause regs_ever_live to change
   until final, but jump_optimize may need to know sooner if a
   `return' is OK.  */

int
ix86_can_use_return_insn_p ()
{
#ifdef NON_SAVING_SETJMP
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    return 0;
#endif

  if (! reload_completed)
    return 0;

  return ix86_nsaved_regs () == 0 || ! frame_pointer_needed;
}

static char *pic_label_name;
static int pic_label_output;
static char *global_offset_table_name;

/* This function generates code for -fpic that loads %ebx with
   the return address of the caller and then returns.  */

void
asm_output_function_prefix (file, name)
     FILE *file;
     char *name ATTRIBUTE_UNUSED;
{
  rtx xops[2];
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);
  xops[0] = pic_offset_table_rtx;
  xops[1] = stack_pointer_rtx;

  /* Deep branch prediction favors having a return for every call. */
  if (pic_reg_used && TARGET_DEEP_BRANCH_PREDICTION)
    {
      if (!pic_label_output)
	{
	  /* This used to call ASM_DECLARE_FUNCTION_NAME() but since it's an
	     internal (non-global) label that's being emitted, it didn't make
	     sense to have .type information for local labels.   This caused
	     the SCO OpenServer 5.0.4 ELF assembler grief (why are you giving
  	     me debug info for a label that you're declaring non-global?) this
	     was changed to call ASM_OUTPUT_LABEL() instead. */

	  ASM_OUTPUT_LABEL (file, pic_label_name); 

	  xops[1] = gen_rtx_MEM (SImode, xops[1]);
	  output_asm_insn ("mov{l}\t{%1, %0|%0, %1}", xops);
	  output_asm_insn ("ret", xops);

	  pic_label_output = 1;
	}
    }
}

void
load_pic_register ()
{
  rtx gotsym, pclab;

  if (global_offset_table_name == NULL)
    {
      global_offset_table_name = 
	ggc_alloc_string ("_GLOBAL_OFFSET_TABLE_", 21);
      ggc_add_string_root (&global_offset_table_name, 1);
    }
  gotsym = gen_rtx_SYMBOL_REF (Pmode, global_offset_table_name);

  if (TARGET_DEEP_BRANCH_PREDICTION)
    {
      if (pic_label_name == NULL) 
	{
	  pic_label_name = ggc_alloc_string (NULL, 32);
	  ggc_add_string_root (&pic_label_name, 1);
	  ASM_GENERATE_INTERNAL_LABEL (pic_label_name, "LPR", 0);
	}
      pclab = gen_rtx_MEM (QImode, gen_rtx_SYMBOL_REF (Pmode, pic_label_name));
    }
  else
    {
      pclab = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
    }

  emit_insn (gen_prologue_get_pc (pic_offset_table_rtx, pclab));

  if (! TARGET_DEEP_BRANCH_PREDICTION)
    emit_insn (gen_popsi1 (pic_offset_table_rtx));

  emit_insn (gen_prologue_set_got (pic_offset_table_rtx, gotsym, pclab));
}

/* Generate an SImode "push" pattern for input ARG.  */

static rtx
gen_push (arg)
     rtx arg;
{
  return gen_rtx_SET (VOIDmode,
		      gen_rtx_MEM (SImode,
				   gen_rtx_PRE_DEC (SImode,
						    stack_pointer_rtx)),
		      arg);
}

/* Return number of registers to be saved on the stack.  */

static int
ix86_nsaved_regs ()
{
  int nregs = 0;
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);
  int limit = (frame_pointer_needed
	       ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  int regno;

  for (regno = limit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      {
	nregs ++;
      }
  return nregs;
}

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

HOST_WIDE_INT
ix86_initial_elimination_offset (from, to)
     int from;
     int to;
{
  int padding1;
  int nregs;

  /* Stack grows downward:
    
     [arguments]
						<- ARG_POINTER
     saved pc

     saved frame pointer if frame_pointer_needed
						<- HARD_FRAME_POINTER
     [saved regs if SAVED_REGS_FIRST]

     [padding1]   \
		   |				<- FRAME_POINTER
     [frame]	   > tsize
		   |
     [padding2]   /

     [saved regs if !SAVED_REGS_FIRST]
     						<- STACK_POINTER
    */

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    /* Skip saved PC and previous frame pointer.
       Executed only when frame_pointer_needed.  */
    return 8;
  else if (from == FRAME_POINTER_REGNUM
	   && to == HARD_FRAME_POINTER_REGNUM)
    {
      ix86_compute_frame_size (get_frame_size (), &nregs, &padding1, (int *)0);
      if (SAVED_REGS_FIRST)
	padding1 += nregs * UNITS_PER_WORD;
      return -padding1;
    }
  else
    {
      /* ARG_POINTER or FRAME_POINTER to STACK_POINTER elimination.  */
      int frame_size = frame_pointer_needed ? 8 : 4;
      HOST_WIDE_INT tsize = ix86_compute_frame_size (get_frame_size (),
						     &nregs, &padding1, (int *)0);


      if (to != STACK_POINTER_REGNUM)
	abort ();
      else if (from == ARG_POINTER_REGNUM)
	return tsize + nregs * UNITS_PER_WORD + frame_size;
      else if (from != FRAME_POINTER_REGNUM)
	abort ();
      else if (SAVED_REGS_FIRST)
	return tsize - padding1;
      else
	return tsize + nregs * UNITS_PER_WORD - padding1;
    }
}

/* Compute the size of local storage taking into consideration the
   desired stack alignment which is to be maintained.  Also determine
   the number of registers saved below the local storage.  
 
   PADDING1 returns padding before stack frame and PADDING2 returns
   padding after stack frame;
 */

static HOST_WIDE_INT
ix86_compute_frame_size (size, nregs_on_stack, rpadding1, rpadding2)
     HOST_WIDE_INT size;
     int *nregs_on_stack;
     int *rpadding1;
     int *rpadding2;
{
  int nregs;
  int padding1 = 0;
  int padding2 = 0;
  HOST_WIDE_INT total_size;
  int stack_alignment_needed = cfun->stack_alignment_needed / BITS_PER_UNIT;

  nregs = ix86_nsaved_regs ();
  total_size = size;

#ifdef PREFERRED_STACK_BOUNDARY
  {
    int offset;
    int preferred_alignment = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;

    offset = frame_pointer_needed ? 8 : 4;

    /* When frame is not empty we ought to have recorded the alignment.  */
    if (size && !stack_alignment_needed)
      abort ();

    if (stack_alignment_needed < 4)
      stack_alignment_needed = 4;

    if (stack_alignment_needed > preferred_alignment)
      abort ();

    if (SAVED_REGS_FIRST)
      offset += nregs * UNITS_PER_WORD;
    else
      total_size += nregs * UNITS_PER_WORD;

    total_size += offset;

    /* Align start of frame for local function.  */
    if (size > 0)
      {
        padding1 = ((offset + stack_alignment_needed - 1)
		    & -stack_alignment_needed) - offset;
        total_size += padding1;
      }

    /* Align stack boundary. */
    if (!current_function_is_leaf)
      padding2 = ((total_size + preferred_alignment - 1)
		  & -preferred_alignment) - total_size;
  }
#endif

  if (nregs_on_stack)
    *nregs_on_stack = nregs;
  if (rpadding1)
    *rpadding1 = padding1;
  if (rpadding2)
    *rpadding2 = padding2;

  return size + padding1 + padding2;
}

/* Emit code to save registers in the prologue.  */

static void
ix86_emit_save_regs ()
{
  register int regno;
  int limit;
  rtx insn;
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);
  limit = (frame_pointer_needed
	   ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);

  for (regno = limit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && !call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      {
	insn = emit_insn (gen_push (gen_rtx_REG (SImode, regno)));
	RTX_FRAME_RELATED_P (insn) = 1;
      }
}

/* Expand the prologue into a bunch of separate insns. */

void
ix86_expand_prologue ()
{
  HOST_WIDE_INT tsize = ix86_compute_frame_size (get_frame_size (), (int *)0, (int *)0,
						 (int *)0);
  rtx insn;
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);

  /* Note: AT&T enter does NOT have reversed args.  Enter is probably
     slower on all targets.  Also sdb doesn't like it.  */

  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_push (hard_frame_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;

      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (SAVED_REGS_FIRST)
    ix86_emit_save_regs ();

  if (tsize == 0)
    ;
  else if (! TARGET_STACK_PROBE || tsize < CHECK_STACK_LIMIT)
    {
      if (frame_pointer_needed)
	insn = emit_insn (gen_prologue_allocate_stack (stack_pointer_rtx,
						       stack_pointer_rtx,
						       GEN_INT (-tsize),
						       hard_frame_pointer_rtx));
      else
        insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				      GEN_INT (-tsize)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      /* ??? Is this only valid for Win32?  */

      rtx arg0, sym;

      arg0 = gen_rtx_REG (SImode, 0);
      emit_move_insn (arg0, GEN_INT (tsize));

      sym = gen_rtx_MEM (FUNCTION_MODE,
			 gen_rtx_SYMBOL_REF (Pmode, "_alloca"));
      insn = emit_call_insn (gen_call (sym, const0_rtx));

      CALL_INSN_FUNCTION_USAGE (insn)
	= gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_USE (VOIDmode, arg0),
			     CALL_INSN_FUNCTION_USAGE (insn));
    }

  if (!SAVED_REGS_FIRST)
    ix86_emit_save_regs ();

#ifdef SUBTARGET_PROLOGUE
  SUBTARGET_PROLOGUE;
#endif  

  if (pic_reg_used)
    load_pic_register ();

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  However, if -fpic, the above call will have
     done that.  */
  if ((profile_flag || profile_block_flag) && ! pic_reg_used)
    emit_insn (gen_blockage ());
}

/* Emit code to pop all registers from stack.  */

static void
ix86_emit_restore_regs ()
{
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);
  int limit = (frame_pointer_needed
	       ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  int regno;

  for (regno = 0; regno < limit; regno++)
    if ((regs_ever_live[regno] && !call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      {
	emit_insn (gen_popsi1 (gen_rtx_REG (SImode, regno)));
      }
}

/* Emit code to add TSIZE to esp value.  Use POP instruction when
   profitable.  */

static void
ix86_emit_epilogue_esp_adjustment (tsize)
     int tsize;
{
  /* Intel's docs say that for 4 or 8 bytes of stack frame one should
     use `pop' and not `add'.  */
  int use_pop = tsize == 4;
  rtx edx = 0, ecx;

  /* Use two pops only for the Pentium processors.  */
  if (tsize == 8 && !TARGET_386 && !TARGET_486)
    {
      rtx retval = current_function_return_rtx;

      edx = gen_rtx_REG (SImode, 1);

      /* This case is a bit more complex.  Since we cannot pop into
         %ecx twice we need a second register.  But this is only
         available if the return value is not of DImode in which
         case the %edx register is not available.  */
      use_pop = (retval == NULL
		 || !reg_overlap_mentioned_p (edx, retval));
    }

  if (use_pop)
    {
      ecx = gen_rtx_REG (SImode, 2);

      /* We have to prevent the two pops here from being scheduled.
         GCC otherwise would try in some situation to put other
         instructions in between them which has a bad effect.  */
      emit_insn (gen_blockage ());
      emit_insn (gen_popsi1 (ecx));
      if (tsize == 8)
	emit_insn (gen_popsi1 (edx));
    }
  else
    {
      /* If there is no frame pointer, we must still release the frame. */
      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			     GEN_INT (tsize)));
    }
}

/* Restore function stack, frame, and registers. */

void
ix86_expand_epilogue ()
{
  int regno;
  int nregs;
  int limit;
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);
  int sp_valid = !frame_pointer_needed || current_function_sp_is_unchanging;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT tsize = ix86_compute_frame_size (get_frame_size (), &nregs, (int *)0,
						 (int *)0);

  /* SP is often unreliable so we may have to go off the frame pointer. */

  offset = -(tsize + nregs * UNITS_PER_WORD);

  if (SAVED_REGS_FIRST)
    {
      if (!sp_valid)
        {
	  if (nregs)
	    emit_insn (gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				    gen_rtx_PLUS (SImode, hard_frame_pointer_rtx,
						  GEN_INT (- nregs * UNITS_PER_WORD))));
	  else
	    emit_insn (gen_epilogue_deallocate_stack (stack_pointer_rtx,
						   hard_frame_pointer_rtx));
	}
      else if (tsize)
	ix86_emit_epilogue_esp_adjustment (tsize);
      ix86_emit_restore_regs ();
    }

  /* If we're only restoring one register and sp is not valid then
     using a move instruction to restore the register since it's
     less work than reloading sp and popping the register.  Otherwise,
     restore sp (if necessary) and pop the registers. */

  else if (nregs > 1 || sp_valid)
    {
      if (!sp_valid)
	{
	  rtx addr_offset;
	  addr_offset = adj_offsettable_operand (AT_BP (QImode), offset);
	  addr_offset = XEXP (addr_offset, 0);

	  emit_insn (gen_rtx_SET (VOIDmode, stack_pointer_rtx, addr_offset));
	}

      ix86_emit_restore_regs ();
    }
  else
    {
      limit = (frame_pointer_needed
	       ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
      for (regno = 0; regno < limit; regno++)
	if ((regs_ever_live[regno] && ! call_used_regs[regno])
	    || (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
	  {
	    emit_move_insn (gen_rtx_REG (SImode, regno),
			    adj_offsettable_operand (AT_BP (Pmode), offset));
	    offset += 4;
	  }
    }

  if (frame_pointer_needed)
    {
      /* If not an i386, mov & pop is faster than "leave". */
      if (TARGET_USE_LEAVE || optimize_size)
	emit_insn (gen_leave ());
      else
	{
	  if (!SAVED_REGS_FIRST)
	    emit_insn (gen_epilogue_deallocate_stack (stack_pointer_rtx,
						   hard_frame_pointer_rtx));
	  emit_insn (gen_popsi1 (hard_frame_pointer_rtx));
	}
    }
  else if (!SAVED_REGS_FIRST && tsize)
    ix86_emit_epilogue_esp_adjustment (tsize);

#ifdef FUNCTION_BLOCK_PROFILER_EXIT
  if (profile_block_flag == 2)
    {
      FUNCTION_BLOCK_PROFILER_EXIT;
    }
#endif

  if (current_function_pops_args && current_function_args_size)
    {
      rtx popc = GEN_INT (current_function_pops_args);

      /* i386 can only pop 32K bytes (maybe 64K?  Is it signed?).  If
	 asked to pop more, pop return address, do explicit add, and jump
	 indirectly to the caller. */

      if (current_function_pops_args >= 32768)
	{
	  rtx ecx = gen_rtx_REG (SImode, 2);

	  emit_insn (gen_popsi1 (ecx));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, popc));
	  emit_indirect_jump (ecx);
	}
      else
	emit_jump_insn (gen_return_pop_internal (popc));
    }
  else
    emit_jump_insn (gen_return_internal ());
}

/* Extract the parts of an RTL expression that is a valid memory address
   for an instruction.  Return false if the structure of the address is
   grossly off.  */

static int
ix86_decompose_address (addr, out)
     register rtx addr;
     struct ix86_address *out;
{
  rtx base = NULL_RTX;
  rtx index = NULL_RTX;
  rtx disp = NULL_RTX;
  HOST_WIDE_INT scale = 1;
  rtx scale_rtx = NULL_RTX;

  if (GET_CODE (addr) == REG || GET_CODE (addr) == SUBREG)
    base = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      enum rtx_code code0 = GET_CODE (op0);
      enum rtx_code code1 = GET_CODE (op1);

      if (code0 == REG || code0 == SUBREG)
	{
	  if (code1 == REG || code1 == SUBREG)
	    index = op0, base = op1;	/* index + base */
	  else
	    base = op0, disp = op1;	/* base + displacement */
	}
      else if (code0 == MULT)
	{
	  index = XEXP (op0, 0);
	  scale_rtx = XEXP (op0, 1);
	  if (code1 == REG || code1 == SUBREG)
	    base = op1;			/* index*scale + base */
	  else
	    disp = op1;			/* index*scale + disp */
	}
      else if (code0 == PLUS && GET_CODE (XEXP (op0, 0)) == MULT)
	{
	  index = XEXP (XEXP (op0, 0), 0);	/* index*scale + base + disp */
	  scale_rtx = XEXP (XEXP (op0, 0), 1);
	  base = XEXP (op0, 1);
	  disp = op1;
	}
      else if (code0 == PLUS)
	{
	  index = XEXP (op0, 0);	/* index + base + disp */
	  base = XEXP (op0, 1);
	  disp = op1;
	}
      else
	return FALSE;
    }
  else if (GET_CODE (addr) == MULT)
    {
      index = XEXP (addr, 0);		/* index*scale */
      scale_rtx = XEXP (addr, 1);
    }
  else if (GET_CODE (addr) == ASHIFT)
    {
      rtx tmp;

      /* We're called for lea too, which implements ashift on occasion.  */
      index = XEXP (addr, 0);
      tmp = XEXP (addr, 1);
      if (GET_CODE (tmp) != CONST_INT)
	return FALSE;
      scale = INTVAL (tmp);
      if ((unsigned HOST_WIDE_INT) scale > 3)
	return FALSE;
      scale = 1 << scale;
    }
  else
    disp = addr;			/* displacement */

  /* Extract the integral value of scale.  */
  if (scale_rtx)
    {
      if (GET_CODE (scale_rtx) != CONST_INT)
	return FALSE;
      scale = INTVAL (scale_rtx);
    }

  /* Allow arg pointer and stack pointer as index if there is not scaling */
  if (base && index && scale == 1
      && (index == arg_pointer_rtx || index == frame_pointer_rtx
          || index == stack_pointer_rtx))
    {
      rtx tmp = base;
      base = index;
      index = tmp;
    }

  /* Special case: %ebp cannot be encoded as a base without a displacement.  */
  if ((base == hard_frame_pointer_rtx
       || base == frame_pointer_rtx
       || base == arg_pointer_rtx) && !disp)
    disp = const0_rtx;

  /* Special case: on K6, [%esi] makes the instruction vector decoded.
     Avoid this by transforming to [%esi+0].  */
  if (ix86_cpu == PROCESSOR_K6 && !optimize_size
      && base && !index && !disp
      && REG_P (base)
      && REGNO_REG_CLASS (REGNO (base)) == SIREG)
    disp = const0_rtx;

  /* Special case: encode reg+reg instead of reg*2.  */
  if (!base && index && scale && scale == 2)
    base = index, scale = 1;
  
  /* Special case: scaling cannot be encoded without base or displacement.  */
  if (!base && !disp && index && scale != 1)
    disp = const0_rtx;

  out->base = base;
  out->index = index;
  out->disp = disp;
  out->scale = scale;

  return TRUE;
}

/* Determine if a given CONST RTX is a valid memory displacement
   in PIC mode.  */
   
int
legitimate_pic_address_disp_p (disp)
     register rtx disp;
{
  if (GET_CODE (disp) != CONST)
    return 0;
  disp = XEXP (disp, 0);

  if (GET_CODE (disp) == PLUS)
    {
      if (GET_CODE (XEXP (disp, 1)) != CONST_INT)
	return 0;
      disp = XEXP (disp, 0);
    }

  if (GET_CODE (disp) != UNSPEC
      || XVECLEN (disp, 0) != 1)
    return 0;

  /* Must be @GOT or @GOTOFF.  */
  if (XINT (disp, 1) != 6
      && XINT (disp, 1) != 7)
    return 0;

  if (GET_CODE (XVECEXP (disp, 0, 0)) != SYMBOL_REF
      && GET_CODE (XVECEXP (disp, 0, 0)) != LABEL_REF)
    return 0;

  return 1;
}

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a valid
   memory address for an instruction.  The MODE argument is the machine mode
   for the MEM expression that wants to use this address.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

int
legitimate_address_p (mode, addr, strict)
     enum machine_mode mode;
     register rtx addr;
     int strict;
{
  struct ix86_address parts;
  rtx base, index, disp;
  HOST_WIDE_INT scale;
  const char *reason = NULL;
  rtx reason_rtx = NULL_RTX;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\n======\nGO_IF_LEGITIMATE_ADDRESS, mode = %s, strict = %d\n",
	       GET_MODE_NAME (mode), strict);
      debug_rtx (addr);
    }

  if (! ix86_decompose_address (addr, &parts))
    {
      reason = "decomposition failed";
      goto error;
    }

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  /* Validate base register.

     Don't allow SUBREG's here, it can lead to spill failures when the base
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */

  if (base)
    {
      reason_rtx = base;

      if (GET_CODE (base) != REG)
	{
	  reason = "base is not a register";
	  goto error;
	}

      if (GET_MODE (base) != Pmode)
	{
	  reason = "base is not in Pmode";
	  goto error;
	}

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (base))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (base)))
	{
	  reason = "base is not valid";
	  goto error;
	}
    }

  /* Validate index register.

     Don't allow SUBREG's here, it can lead to spill failures when the index
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */

  if (index)
    {
      reason_rtx = index;

      if (GET_CODE (index) != REG)
	{
	  reason = "index is not a register";
	  goto error;
	}

      if (GET_MODE (index) != Pmode)
	{
	  reason = "index is not in Pmode";
	  goto error;
	}

      if ((strict && ! REG_OK_FOR_INDEX_STRICT_P (index))
	  || (! strict && ! REG_OK_FOR_INDEX_NONSTRICT_P (index)))
	{
	  reason = "index is not valid";
	  goto error;
	}
    }

  /* Validate scale factor.  */
  if (scale != 1)
    {
      reason_rtx = GEN_INT (scale);
      if (!index)
	{
	  reason = "scale without index";
	  goto error;
	}

      if (scale != 2 && scale != 4 && scale != 8)
	{
	  reason = "scale is not a valid multiplier";
	  goto error;
	}
    }

  /* Validate displacement.  */
  if (disp)
    {
      reason_rtx = disp;

      if (!CONSTANT_ADDRESS_P (disp))
	{
	  reason = "displacement is not constant";
	  goto error;
	}

      if (GET_CODE (disp) == CONST_DOUBLE)
	{
	  reason = "displacement is a const_double";
	  goto error;
	}

      if (flag_pic && SYMBOLIC_CONST (disp))
	{
	  if (! legitimate_pic_address_disp_p (disp))
	    {
	      reason = "displacement is an invalid pic construct";
	      goto error;
	    }

	  /* Verify that a symbolic pic displacement includes 
	     the pic_offset_table_rtx register.  */
	  if (base != pic_offset_table_rtx
	      && (index != pic_offset_table_rtx || scale != 1))
	    {
	      reason = "pic displacement against invalid base";
	      goto error;
	    }
	}
      else if (HALF_PIC_P ())
	{
	  if (! HALF_PIC_ADDRESS_P (disp)
	      || (base != NULL_RTX || index != NULL_RTX))
	    {
	      reason = "displacement is an invalid half-pic reference";
	      goto error;
	    }
	}
    }

  /* Everything looks valid.  */
  if (TARGET_DEBUG_ADDR)
    fprintf (stderr, "Success.\n");
  return TRUE;

error:
  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "Error: %s\n", reason);
      debug_rtx (reason_rtx);
    }
  return FALSE;
}

/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are two types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references, constant pool addresses, and code labels
      compute the address as an offset from the GOT, whose base is in
      the PIC reg.  Static data objects have SYMBOL_REF_FLAG set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   GO_IF_LEGITIMATE_ADDRESS rejects symbolic references unless the PIC
   reg also appears in the address.  */

rtx
legitimize_pic_address (orig, reg)
     rtx orig;
     rtx reg;
{
  rtx addr = orig;
  rtx new = orig;
  rtx base;

  if (GET_CODE (addr) == LABEL_REF
      || (GET_CODE (addr) == SYMBOL_REF
	  && (CONSTANT_POOL_ADDRESS_P (addr)
	      || SYMBOL_REF_FLAG (addr))))
    {
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      current_function_uses_pic_offset_table = 1;
      new = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, addr), 7);
      new = gen_rtx_CONST (VOIDmode, new);
      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

      if (reg != 0)
	{
	  emit_move_insn (reg, new);
	  new = reg;
	}
    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      /* This symbol must be referenced via a load from the
	 Global Offset Table (@GOT). */

      current_function_uses_pic_offset_table = 1;
      new = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, addr), 6);
      new = gen_rtx_CONST (VOIDmode, new);
      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);
      new = gen_rtx_MEM (Pmode, new);
      RTX_UNCHANGING_P (new) = 1;

      if (reg == 0)
	reg = gen_reg_rtx (Pmode);
      emit_move_insn (reg, new);
      new = reg;
    }      
  else
    {
      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) == UNSPEC)
	    {
	      /* Check that the unspec is one of the ones we generate?  */
	    }
	  else if (GET_CODE (addr) != PLUS)
	    abort ();
	}
      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant offset from a @GOTOFF
	     symbol reference.  */
	  if ((GET_CODE (op0) == LABEL_REF
	       || (GET_CODE (op0) == SYMBOL_REF
		   && (CONSTANT_POOL_ADDRESS_P (op0)
		       || SYMBOL_REF_FLAG (op0))))
	      && GET_CODE (op1) == CONST_INT)
	    {
	      current_function_uses_pic_offset_table = 1;
	      new = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, op0), 7);
	      new = gen_rtx_PLUS (VOIDmode, new, op1);
	      new = gen_rtx_CONST (VOIDmode, new);
	      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

	      if (reg != 0)
		{
		  emit_move_insn (reg, new);
		  new = reg;
		}
	    }
	  else
	    {
	      base = legitimize_pic_address (XEXP (addr, 0), reg);
	      new  = legitimize_pic_address (XEXP (addr, 1),
					     base == reg ? NULL_RTX : reg);

	      if (GET_CODE (new) == CONST_INT)
		new = plus_constant (base, INTVAL (new));
	      else
		{
		  if (GET_CODE (new) == PLUS && CONSTANT_P (XEXP (new, 1)))
		    {
		      base = gen_rtx_PLUS (Pmode, base, XEXP (new, 0));
		      new = XEXP (new, 1);
		    }
		  new = gen_rtx_PLUS (Pmode, base, new);
		}
	    }
	}
    }
  return new;
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

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.c for details.  */

rtx
legitimize_address (x, oldx, mode)
     register rtx x;
     register rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  int changed = 0;
  unsigned log;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n==========\nLEGITIMIZE_ADDRESS, mode = %s\n",
	       GET_MODE_NAME (mode));
      debug_rtx (x);
    }

  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

  /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
  if (GET_CODE (x) == ASHIFT
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (log = (unsigned)exact_log2 (INTVAL (XEXP (x, 1)))) < 4)
    {
      changed = 1;
      x = gen_rtx_MULT (Pmode, force_reg (Pmode, XEXP (x, 0)),
			GEN_INT (1 << log));
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Canonicalize shifts by 0, 1, 2, 3 into multiply. */

      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (log = (unsigned)exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 0) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 0), 0)),
				      GEN_INT (1 << log));
	}

      if (GET_CODE (XEXP (x, 1)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && (log = (unsigned)exact_log2 (INTVAL (XEXP (XEXP (x, 1), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 1) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 1), 0)),
				      GEN_INT (1 << log));
	}

      /* Put multiply first if it isn't already. */
      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  rtx tmp = XEXP (x, 0);
	  XEXP (x, 0) = XEXP (x, 1);
	  XEXP (x, 1) = tmp;
	  changed = 1;
	}

      /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
	 created by virtual register instantiation, register elimination, and
	 similar optimizations.  */
      if (GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == PLUS)
	{
	  changed = 1;
	  x = gen_rtx_PLUS (Pmode,
			    gen_rtx_PLUS (Pmode, XEXP (x, 0),
					  XEXP (XEXP (x, 1), 0)),
			    XEXP (XEXP (x, 1), 1));
	}

      /* Canonicalize
	 (plus (plus (mult (reg) (const)) (plus (reg) (const))) const)
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  */
      else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == PLUS
	       && CONSTANT_P (XEXP (x, 1)))
	{
	  rtx constant;
	  rtx other = NULL_RTX;

	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      constant = XEXP (x, 1);
	      other = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    }
	  else if (GET_CODE (XEXP (XEXP (XEXP (x, 0), 1), 1)) == CONST_INT)
	    {
	      constant = XEXP (XEXP (XEXP (x, 0), 1), 1);
	      other = XEXP (x, 1);
	    }
	  else
	    constant = 0;

	  if (constant)
	    {
	      changed = 1;
	      x = gen_rtx_PLUS (Pmode,
				gen_rtx_PLUS (Pmode, XEXP (XEXP (x, 0), 0),
					      XEXP (XEXP (XEXP (x, 0), 1), 0)),
				plus_constant (other, INTVAL (constant)));
	    }
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 0) = force_operand (XEXP (x, 0), 0);
	}

      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 1) = force_operand (XEXP (x, 1), 0);
	}

      if (changed
	  && GET_CODE (XEXP (x, 1)) == REG
	  && GET_CODE (XEXP (x, 0)) == REG)
	return x;

      if (flag_pic && SYMBOLIC_CONST (XEXP (x, 1)))
	{
	  changed = 1;
	  x = legitimize_pic_address (x, 0);
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 1) = temp;
	  return x;
	}

      else if (GET_CODE (XEXP (x, 1)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 0) = temp;
	  return x;
	}
    }

  return x;
}

/* Print an integer constant expression in assembler syntax.  Addition
   and subtraction are the only arithmetic that may appear in these
   expressions.  FILE is the stdio stream to write to, X is the rtx, and
   CODE is the operand print code from the output string.  */

static void
output_pic_addr_const (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  char buf[256];

  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	abort ();
      break;

    case SYMBOL_REF:
      assemble_name (file, XSTR (x, 0));
      if (code == 'P' && ! SYMBOL_REF_FLAG (x))
	fputs ("@PLT", file);
      break;

    case LABEL_REF:
      x = XEXP (x, 0);
      /* FALLTHRU */
    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (asm_out_file, buf);
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is <32 bits and positive.  */
	  if (CONST_DOUBLE_HIGH (x) || CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, "0x%lx%08lx",
		     (unsigned long) CONST_DOUBLE_HIGH (x),
		     (unsigned long) CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear first.  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  output_pic_addr_const (file, XEXP (x, 0), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  output_pic_addr_const (file, XEXP (x, 1), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 0), code);
	}
      else
	abort ();
      break;

    case MINUS:
      putc (ASSEMBLER_DIALECT ? '(' : '[', file);
      output_pic_addr_const (file, XEXP (x, 0), code);
      putc ('-', file);
      output_pic_addr_const (file, XEXP (x, 1), code);
      putc (ASSEMBLER_DIALECT ? ')' : ']', file);
      break;

     case UNSPEC:
       if (XVECLEN (x, 0) != 1)
 	abort ();
       output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
       switch (XINT (x, 1))
 	{
 	case 6:
 	  fputs ("@GOT", file);
 	  break;
 	case 7:
 	  fputs ("@GOTOFF", file);
 	  break;
 	case 8:
 	  fputs ("@PLT", file);
 	  break;
 	default:
 	  output_operand_lossage ("invalid UNSPEC as operand");
 	  break;
 	}
       break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* This is called from dwarfout.c via ASM_OUTPUT_DWARF_ADDR_CONST.  
   We need to handle our special PIC relocations.  */

void 
i386_dwarf_output_addr_const (file, x)
     FILE *file;
     rtx x;
{
  fprintf (file, "\t%s\t", INT_ASM_OP);
  if (flag_pic)
    output_pic_addr_const (file, x, '\0');
  else
    output_addr_const (file, x);
  fputc ('\n', file);
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler losage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.  */

rtx
i386_simplify_dwarf_addr (orig_x)
     rtx orig_x;
{
  rtx x = orig_x;

  if (GET_CODE (x) != PLUS
      || GET_CODE (XEXP (x, 0)) != REG
      || GET_CODE (XEXP (x, 1)) != CONST)
    return orig_x;

  x = XEXP (XEXP (x, 1), 0);
  if (GET_CODE (x) == UNSPEC
      && XINT (x, 1) == 7)
    return XVECEXP (x, 0, 0);

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == UNSPEC
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && XINT (XEXP (x, 0), 1) == 7)
    return gen_rtx_PLUS (VOIDmode, XVECEXP (XEXP (x, 0), 0, 0), XEXP (x, 1));

  return orig_x;
}

static void
put_condition_code (code, mode, reverse, fp, file)
     enum rtx_code code;
     enum machine_mode mode;
     int reverse, fp;
     FILE *file;
{
  const char *suffix;

  if (reverse)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:
      suffix = "e";
      break;
    case NE:
      suffix = "ne";
      break;
    case GT:
      if (mode == CCNOmode)
	abort ();
      suffix = "g";
      break;
    case GTU:
      /* ??? Use "nbe" instead of "a" for fcmov losage on some assemblers.
	 Those same assemblers have the same but opposite losage on cmov.  */
      suffix = fp ? "nbe" : "a";
      break;
    case LT:
      if (mode == CCNOmode)
	suffix = "s";
      else
	suffix = "l";
      break;
    case LTU:
      suffix = "b";
      break;
    case GE:
      if (mode == CCNOmode)
	suffix = "ns";
      else
	suffix = "ge";
      break;
    case GEU:
      /* ??? As above.  */
      suffix = fp ? "nb" : "ae";
      break;
    case LE:
      if (mode == CCNOmode)
	abort ();
      suffix = "le";
      break;
    case LEU:
      suffix = "be";
      break;
    default:
      abort ();
    }
  fputs (suffix, file);
}

void
print_reg (x, code, file)
     rtx x;
     int code;
     FILE *file;
{
  if (REGNO (x) == ARG_POINTER_REGNUM
      || REGNO (x) == FRAME_POINTER_REGNUM
      || REGNO (x) == FLAGS_REG
      || REGNO (x) == FPSR_REG)
    abort ();

  if (ASSEMBLER_DIALECT == 0 || USER_LABEL_PREFIX[0] == 0)
    putc ('%', file);

  if (code == 'w')
    code = 2;
  else if (code == 'b')
    code = 1;
  else if (code == 'k')
    code = 4;
  else if (code == 'y')
    code = 3;
  else if (code == 'h')
    code = 0;
  else
    code = GET_MODE_SIZE (GET_MODE (x));

  switch (code)
    {
    case 3:
      if (STACK_TOP_P (x))
	{
	  fputs ("st(0)", file);
	  break;
	}
      /* FALLTHRU */
    case 4:
    case 8:
    case 12:
      if (! FP_REG_P (x))
	putc ('e', file);
      /* FALLTHRU */
    case 2:
      fputs (hi_reg_name[REGNO (x)], file);
      break;
    case 1:
      fputs (qi_reg_name[REGNO (x)], file);
      break;
    case 0:
      fputs (qi_high_reg_name[REGNO (x)], file);
      break;
    default:
      abort ();
    }
}

/* Meaning of CODE:
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
   C -- print opcode suffix for set/cmov insn.
   c -- like C, but print reversed condition
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   s -- print a shift double count, followed by the assemblers argument
	delimiter.
   b -- print the QImode name of the register for the indicated operand.
	%b0 would print %al if operands[0] is reg 0.
   w --  likewise, print the HImode name of the register.
   k --  likewise, print the SImode name of the register.
   h --  print the QImode name for a "high" register, either ah, bh, ch or dh.
   y --  print "st(0)" instead of "st" as a register.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code)
    {
      switch (code)
	{
	case '*':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('*', file);
	  return;

	case 'L':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('l', file);
	  return;

	case 'W':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('w', file);
	  return;

	case 'B':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('b', file);
	  return;

	case 'Q':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('l', file);
	  return;

	case 'S':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('s', file);
	  return;

	case 'T':
	  if (ASSEMBLER_DIALECT == 0)
	    putc ('t', file);
	  return;

	case 'z':
	  /* 387 opcodes don't get size suffixes if the operands are
	     registers. */

	  if (STACK_REG_P (x))
	    return;

	  /* Intel syntax has no truck with instruction suffixes.  */
	  if (ASSEMBLER_DIALECT != 0)
	    return;

	  /* this is the size of op from size of operand */
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 1:
	      putc ('b', file);
	      return;

	    case 2:
	      putc ('w', file);
	      return;

	    case 4:
	      if (GET_MODE (x) == SFmode)
		{
		  putc ('s', file);
		  return;
		}
	      else
		putc ('l', file);
	      return;

	    case 12:
	      putc ('t', file);
	      return;

	    case 8:
	      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
		{
#ifdef GAS_MNEMONICS
		  putc ('q', file);
#else
		  putc ('l', file);
		  putc ('l', file);
#endif
		}
	      else
	        putc ('l', file);
	      return;
	    }

	case 'b':
	case 'w':
	case 'k':
	case 'h':
	case 'y':
	case 'X':
	case 'P':
	  break;

	case 's':
	  if (GET_CODE (x) == CONST_INT || ! SHIFT_DOUBLE_OMITS_COUNT)
	    {
	      PRINT_OPERAND (file, x, 0);
	      putc (',', file);
	    }
	  return;

	case 'C':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 0, file);
	  return;
	case 'F':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 1, file);
	  return;

	  /* Like above, but reverse condition */
	case 'c':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 0, file);
	  return;
	case 'f':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 1, file);
	  return;

	default:
	  {
	    char str[50];
	    sprintf (str, "invalid operand code `%c'", code);
	    output_operand_lossage (str);
	  }
	}
    }

  if (GET_CODE (x) == REG)
    {
      PRINT_REG (x, code, file);
    }

  else if (GET_CODE (x) == MEM)
    {
      /* No `byte ptr' prefix for call instructions.  */
      if (ASSEMBLER_DIALECT != 0 && code != 'X' && code != 'P')
	{
	  const char * size;
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 1: size = "BYTE"; break;
	    case 2: size = "WORD"; break;
	    case 4: size = "DWORD"; break;
	    case 8: size = "QWORD"; break;
	    case 12: size = "XWORD"; break;
	    default:
	      abort ();
	    }
	  fputs (size, file);
	  fputs (" PTR ", file);
	}

      x = XEXP (x, 0);
      if (flag_pic && CONSTANT_ADDRESS_P (x))
	output_pic_addr_const (file, x, code);
      else
	output_address (x);
    }

  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      REAL_VALUE_TYPE r;
      long l;

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_TARGET_SINGLE (r, l);

      if (ASSEMBLER_DIALECT == 0)
	putc ('$', file);
      fprintf (file, "0x%lx", l);
    }

 /* These float cases don't actually occur as immediate operands. */
 else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      REAL_VALUE_TYPE r;
      char dstr[30];

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_DECIMAL (r, "%.22e", dstr);
      fprintf (file, "%s", dstr);
    }

  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == XFmode)
    {
      REAL_VALUE_TYPE r;
      char dstr[30];

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_DECIMAL (r, "%.22e", dstr);
      fprintf (file, "%s", dstr);
    }
  else
    {
      if (code != 'P')
	{
	  if (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
	    {
	      if (ASSEMBLER_DIALECT == 0)
		putc ('$', file);
	    }
	  else if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF
		   || GET_CODE (x) == LABEL_REF)
	    {
	      if (ASSEMBLER_DIALECT == 0)
		putc ('$', file);
	      else
		fputs ("OFFSET FLAT:", file);
	    }
	}
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      else if (flag_pic)
	output_pic_addr_const (file, x, code);
      else
	output_addr_const (file, x);
    }
}

/* Print a memory operand whose address is ADDR.  */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  struct ix86_address parts;
  rtx base, index, disp;
  int scale;

  if (! ix86_decompose_address (addr, &parts))
    abort ();

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  if (!base && !index)
    {
      /* Displacement only requires special attention.  */

      if (GET_CODE (disp) == CONST_INT)
	{
	  if (ASSEMBLER_DIALECT != 0)
	    fputs ("ds:", file);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (addr));
	}
      else if (flag_pic)
	output_pic_addr_const (file, addr, 0);
      else
	output_addr_const (file, addr);
    }
  else
    {
      if (ASSEMBLER_DIALECT == 0)
	{
	  if (disp)
	    {
	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else
		output_addr_const (file, disp);
	    }

	  putc ('(', file);
	  if (base)
	    PRINT_REG (base, 0, file);
	  if (index)
	    {
	      putc (',', file);
	      PRINT_REG (index, 0, file);
	      if (scale != 1)
		fprintf (file, ",%d", scale);
	    }
	  putc (')', file);
	}
      else
	{
	  rtx offset = NULL_RTX;

	  if (disp)
	    {
	      /* Pull out the offset of a symbol; print any symbol itself.  */
	      if (GET_CODE (disp) == CONST
		  && GET_CODE (XEXP (disp, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT)
		{
		  offset = XEXP (XEXP (disp, 0), 1);
		  disp = gen_rtx_CONST (VOIDmode,
					XEXP (XEXP (disp, 0), 0));
		}

	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else if (GET_CODE (disp) == CONST_INT)
		offset = disp;
	      else
		output_addr_const (file, disp);
	    }

	  putc ('[', file);
	  if (base)
	    {
	      PRINT_REG (base, 0, file);
	      if (offset)
		{
		  if (INTVAL (offset) >= 0)
		    putc ('+', file);
		  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
		}
	    }
	  else if (offset)
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
	  else
	    putc ('0', file);

	  if (index)
	    {
	      putc ('+', file);
	      PRINT_REG (index, 0, file);
	      if (scale != 1)
		fprintf (file, "*%d", scale);
	    }
	  putc (']', file);
	}
    }
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands". */

void
split_di (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      rtx op = operands[num];
      if (CONSTANT_P (op))
	split_double (op, &lo_half[num], &hi_half[num]);
      else if (! reload_completed)
	{
	  lo_half[num] = gen_lowpart (SImode, op);
	  hi_half[num] = gen_highpart (SImode, op);
	}
      else if (GET_CODE (op) == REG)
	{
	  lo_half[num] = gen_rtx_REG (SImode, REGNO (op));
	  hi_half[num] = gen_rtx_REG (SImode, REGNO (op) + 1);
	}
      else if (offsettable_memref_p (op))
	{
	  rtx lo_addr = XEXP (op, 0);
	  rtx hi_addr = XEXP (adj_offsettable_operand (op, 4), 0);
	  lo_half[num] = change_address (op, SImode, lo_addr);
	  hi_half[num] = change_address (op, SImode, hi_addr);
	}
      else
	abort ();
    }
}

/* Output code to perform a 387 binary operation in INSN, one of PLUS,
   MINUS, MULT or DIV.  OPERANDS are the insn operands, where operands[3]
   is the expression of the binary operation.  The output may either be
   emitted here, or returned to the caller, like all output_* functions.

   There is no guarantee that the operands are the same mode, as they
   might be within FLOAT or FLOAT_EXTEND expressions. */

const char *
output_387_binary_op (insn, operands)
     rtx insn;
     rtx *operands;
{
  static char buf[100];
  rtx temp;
  const char *p;

  switch (GET_CODE (operands[3]))
    {
    case PLUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fiadd";
      else
	p = "fadd";
      break;

    case MINUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fisub";
      else
	p = "fsub";
      break;

    case MULT:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fimul";
      else
	p = "fmul";
      break;

    case DIV:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fidiv";
      else
	p = "fdiv";
      break;

    default:
      abort ();
    }

  strcpy (buf, p);

  switch (GET_CODE (operands[3]))
    {
    case MULT:
    case PLUS:
      if (REG_P (operands[2]) && REGNO (operands[0]) == REGNO (operands[2]))
	{
	  temp = operands[2];
	  operands[2] = operands[1];
	  operands[1] = temp;
	}

      if (GET_CODE (operands[2]) == MEM)
	{
	  p = "%z2\t%2";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
	  if (STACK_TOP_P (operands[0]))
	    p = "p\t{%0,%2|%2, %0}";
	  else
	    p = "p\t{%2,%0|%0, %2}";
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	p = "\t{%y2,%0|%0, %y2}";
      else
	p = "\t{%2,%0|%0, %2}";
      break;

    case MINUS:
    case DIV:
      if (GET_CODE (operands[1]) == MEM)
	{
	  p = "r%z1\t%1";
	  break;
	}

      if (GET_CODE (operands[2]) == MEM)
	{
	  p = "%z2\t%2";
	  break;
	}

      if (! STACK_REG_P (operands[1]) || ! STACK_REG_P (operands[2]))
	abort ();

      /* Note that the Unixware assembler, and the AT&T assembler before
	 that, are confusingly not reversed from Intel syntax in this
	 area.  */
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
	  if (STACK_TOP_P (operands[0]))
	    p = "p\t%0,%2";
	  else
	    p = "rp\t%2,%0";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	{
	  if (STACK_TOP_P (operands[0]))
	    p = "rp\t%0,%1";
	  else
	    p = "p\t%1,%0";
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	{
	  if (STACK_TOP_P (operands[1]))
	    p = "\t%y2,%0";
	  else
	    p = "r\t%y1,%0";
	  break;
	}
      else if (STACK_TOP_P (operands[1]))
	p = "\t%1,%0";
      else
	p = "r\t%2,%0";
      break;

    default:
      abort ();
    }

  strcat (buf, p);
  return buf;
}

/* Output code for INSN to convert a float to a signed int.  OPERANDS
   are the insn operands.  The output may be [SD]Imode and the input
   operand may be [SDX]Fmode.  */

const char *
output_fix_trunc (insn, operands)
     rtx insn;
     rtx *operands;
{
  int stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;
  int dimode_p = GET_MODE (operands[0]) == DImode;
  rtx xops[4];

  /* Jump through a hoop or two for DImode, since the hardware has no
     non-popping instruction.  We used to do this a different way, but
     that was somewhat fragile and broke with post-reload splitters.  */
  if (dimode_p && !stack_top_dies)
    output_asm_insn ("fld\t%y1", operands);

  if (! STACK_TOP_P (operands[1]))
    abort ();

  xops[0] = GEN_INT (12);
  xops[1] = adj_offsettable_operand (operands[2], 1);
  xops[1] = change_address (xops[1], QImode, NULL_RTX);

  xops[2] = operands[0];
  if (GET_CODE (operands[0]) != MEM)
    xops[2] = operands[3];

  output_asm_insn ("fnstcw\t%2", operands);
  output_asm_insn ("mov{l}\t{%2, %4|%4, %2}", operands);
  output_asm_insn ("mov{b}\t{%0, %1|%1, %0}", xops);
  output_asm_insn ("fldcw\t%2", operands);
  output_asm_insn ("mov{l}\t{%4, %2|%2, %4}", operands);

  if (stack_top_dies || dimode_p)
    output_asm_insn ("fistp%z2\t%2", xops);
  else
    output_asm_insn ("fist%z2\t%2", xops);

  output_asm_insn ("fldcw\t%2", operands);

  if (GET_CODE (operands[0]) != MEM)
    {
      if (dimode_p)
	{
	  split_di (operands+0, 1, xops+0, xops+1);
	  split_di (operands+3, 1, xops+2, xops+3);
	  output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);
	  output_asm_insn ("mov{l}\t{%3, %1|%1, %3}", xops);
	}
      else
	output_asm_insn ("mov{l}\t{%3,%0|%0, %3}", operands);
    }

  return "";
}

/* Output code for INSN to compare OPERANDS.  EFLAGS_P is 1 when fcomi
   should be used and 2 when fnstsw should be used.  UNORDERED_P is true
   when fucom should be used.  */

const char *
output_fp_compare (insn, operands, eflags_p, unordered_p)
     rtx insn;
     rtx *operands;
     int eflags_p, unordered_p;
{
  int stack_top_dies;
  rtx cmp_op0 = operands[0];
  rtx cmp_op1 = operands[1];

  if (eflags_p == 2)
    {
      cmp_op0 = cmp_op1;
      cmp_op1 = operands[2];
    }

  if (! STACK_TOP_P (cmp_op0))
    abort ();

  stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;

  if (STACK_REG_P (cmp_op1)
      && stack_top_dies
      && find_regno_note (insn, REG_DEAD, REGNO (cmp_op1))
      && REGNO (cmp_op1) != FIRST_STACK_REG)
    {
      /* If both the top of the 387 stack dies, and the other operand
	 is also a stack register that dies, then this must be a
	 `fcompp' float compare */

      if (eflags_p == 1)
	{
	  /* There is no double popping fcomi variant.  Fortunately,
	     eflags is immune from the fstp's cc clobbering.  */
	  if (unordered_p)
	    output_asm_insn ("fucomip\t{%y1, %0|%0, %y1}", operands);
	  else
	    output_asm_insn ("fcomip\t{%y1, %0|%0, %y1}", operands);
	  return "fstp\t%y0";
	}
      else
	{
	  if (eflags_p == 2)
	    {
	      if (unordered_p)
		return "fucompp\n\tfnstsw\t%0";
	      else
		return "fcompp\n\tfnstsw\t%0";
	    }
	  else
	    {
	      if (unordered_p)
		return "fucompp";
	      else
		return "fcompp";
	    }
	}
    }
  else
    {
      /* Encoded here as eflags_p | intmode | unordered_p | stack_top_dies.  */

      static const char * const alt[24] = 
      {
	"fcom%z1\t%y1",
	"fcomp%z1\t%y1",
	"fucom%z1\t%y1",
	"fucomp%z1\t%y1",
	
	"ficom%z1\t%y1",
	"ficomp%z1\t%y1",
	NULL,
	NULL,

	"fcomi\t{%y1, %0|%0, %y1}",
	"fcomip\t{%y1, %0|%0, %y1}",
	"fucomi\t{%y1, %0|%0, %y1}",
	"fucomip\t{%y1, %0|%0, %y1}",

	NULL,
	NULL,
	NULL,
	NULL,

	"fcom%z2\t%y2\n\tfnstsw\t%0",
	"fcomp%z2\t%y2\n\tfnstsw\t%0",
	"fucom%z2\t%y2\n\tfnstsw\t%0",
	"fucomp%z2\t%y2\n\tfnstsw\t%0",
	
	"ficom%z2\t%y2\n\tfnstsw\t%0",
	"ficomp%z2\t%y2\n\tfnstsw\t%0",
	NULL,
	NULL
      };

      int mask;
      const char *ret;

      mask  = eflags_p << 3;
      mask |= (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT) << 2;
      mask |= unordered_p << 1;
      mask |= stack_top_dies;

      if (mask >= 24)
	abort ();
      ret = alt[mask];
      if (ret == NULL)
	abort ();

      return ret;
    }
}

/* Output assembler code to FILE to initialize basic-block profiling.

   If profile_block_flag == 2

	Output code to call the subroutine `__bb_init_trace_func'
	and pass two parameters to it. The first parameter is
	the address of a block allocated in the object module.
	The second parameter is the number of the first basic block
	of the function.

	The name of the block is a local symbol made with this statement:
	
	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	The number of the first basic block of the function is
	passed to the macro in BLOCK_OR_LABEL.

	If described in a virtual assembler language the code to be
	output looks like:

		parameter1 <- LPBX0
		parameter2 <- BLOCK_OR_LABEL
		call __bb_init_trace_func

    else if profile_block_flag != 0

	Output code to call the subroutine `__bb_init_func'
	and pass one single parameter to it, which is the same
	as the first parameter to `__bb_init_trace_func'.

	The first word of this parameter is a flag which will be nonzero if
	the object module has already been initialized.  So test this word
	first, and do not call `__bb_init_func' if the flag is nonzero.
	Note: When profile_block_flag == 2 the test need not be done
	but `__bb_init_trace_func' *must* be called.

	BLOCK_OR_LABEL may be used to generate a label number as a
	branch destination in case `__bb_init_func' will not be called.

	If described in a virtual assembler language the code to be
	output looks like:

		cmp (LPBX0),0
		jne local_label
		parameter1 <- LPBX0
		call __bb_init_func
	      local_label:
*/

void
ix86_output_function_block_profiler (file, block_or_label)
     FILE *file;
     int block_or_label;
{
  static int num_func = 0;
  rtx xops[8];
  char block_table[80], false_label[80];

  ASM_GENERATE_INTERNAL_LABEL (block_table, "LPBX", 0);

  xops[1] = gen_rtx_SYMBOL_REF (VOIDmode, block_table);
  xops[5] = stack_pointer_rtx;
  xops[7] = gen_rtx_REG (Pmode, 0); /* eax */

  CONSTANT_POOL_ADDRESS_P (xops[1]) = TRUE;

  switch (profile_block_flag)
    {
    case 2:
      xops[2] = GEN_INT (block_or_label);
      xops[3] = gen_rtx_MEM (Pmode,
		     gen_rtx_SYMBOL_REF (VOIDmode, "__bb_init_trace_func"));
      xops[6] = GEN_INT (8);

      output_asm_insn ("push{l}\t%2", xops);
      if (!flag_pic)
	output_asm_insn ("push{l}\t%1", xops);
      else
	{
	  output_asm_insn ("lea{l}\t{%a1, %7|%7, %a1}", xops);
	  output_asm_insn ("push{l}\t%7", xops);
	}
      output_asm_insn ("call\t%P3", xops);
      output_asm_insn ("add{l}\t{%6, %5|%5, %6}", xops);
      break;

    default:
      ASM_GENERATE_INTERNAL_LABEL (false_label, "LPBZ", num_func);

      xops[0] = const0_rtx;
      xops[2] = gen_rtx_MEM (Pmode,
			     gen_rtx_SYMBOL_REF (VOIDmode, false_label));
      xops[3] = gen_rtx_MEM (Pmode,
			     gen_rtx_SYMBOL_REF (VOIDmode, "__bb_init_func"));
      xops[4] = gen_rtx_MEM (Pmode, xops[1]);
      xops[6] = GEN_INT (4);

      CONSTANT_POOL_ADDRESS_P (xops[2]) = TRUE;

      output_asm_insn ("cmp{l}\t{%0, %4|%4, %0}", xops);
      output_asm_insn ("jne\t%2", xops);

      if (!flag_pic)
	output_asm_insn ("push{l}\t%1", xops);
      else
	{
	  output_asm_insn ("lea{l}\t{%a1, %7|%7, %a2}", xops);
	  output_asm_insn ("push{l}\t%7", xops);
	}
      output_asm_insn ("call\t%P3", xops);
      output_asm_insn ("add{l}\t{%6, %5|%5, %6}", xops);
      ASM_OUTPUT_INTERNAL_LABEL (file, "LPBZ", num_func);
      num_func++;
      break;
    }
}

/* Output assembler code to FILE to increment a counter associated
   with basic block number BLOCKNO.

   If profile_block_flag == 2

	Output code to initialize the global structure `__bb' and
	call the function `__bb_trace_func' which will increment the
	counter.

	`__bb' consists of two words. In the first word the number
	of the basic block has to be stored. In the second word
	the address of a block allocated in the object module 
	has to be stored.

	The basic block number is given by BLOCKNO.

	The address of the block is given by the label created with 

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	by FUNCTION_BLOCK_PROFILER.

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	If described in a virtual assembler language the code to be
	output looks like:

		move BLOCKNO -> (__bb)
		move LPBX0 -> (__bb+4)
		call __bb_trace_func

	Note that function `__bb_trace_func' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE
	and MACHINE_STATE_RESTORE. The last two macros will be
	used in the function `__bb_trace_func', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE.

   else if profile_block_flag != 0

	Output code to increment the counter directly.
	Basic blocks are numbered separately from zero within each
	compiled object module. The count associated with block number
	BLOCKNO is at index BLOCKNO in an array of words; the name of 
	this array is a local symbol made with this statement:

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 2);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result. 

	If described in a virtual assembler language the code to be
	output looks like:

		inc (LPBX2+4*BLOCKNO)
*/

void
ix86_output_block_profiler (file, blockno)
     FILE *file ATTRIBUTE_UNUSED;
     int blockno;
{
  rtx xops[8], cnt_rtx;
  char counts[80];
  char *block_table = counts;

  switch (profile_block_flag)
    {
    case 2:
      ASM_GENERATE_INTERNAL_LABEL (block_table, "LPBX", 0);

      xops[1] = gen_rtx_SYMBOL_REF (VOIDmode, block_table);
      xops[2] = GEN_INT (blockno);
      xops[3] = gen_rtx_MEM (Pmode,
			     gen_rtx_SYMBOL_REF (VOIDmode, "__bb_trace_func"));
      xops[4] = gen_rtx_SYMBOL_REF (VOIDmode, "__bb");
      xops[5] = plus_constant (xops[4], 4);
      xops[0] = gen_rtx_MEM (SImode, xops[4]);
      xops[6] = gen_rtx_MEM (SImode, xops[5]);

      CONSTANT_POOL_ADDRESS_P (xops[1]) = TRUE;

      output_asm_insn ("pushf", xops);
      output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);
      if (flag_pic)
	{
	  xops[7] = gen_rtx_REG (Pmode, 0); /* eax */
	  output_asm_insn ("push{l}\t%7", xops);
	  output_asm_insn ("lea{l}\t{%a1, %7|%7, %a1}", xops);
	  output_asm_insn ("mov{l}\t{%7, %6|%6, %7}", xops);
	  output_asm_insn ("pop{l}\t%7", xops);
	}
      else
	output_asm_insn ("mov{l}\t{%1, %6|%6, %1}", xops);
      output_asm_insn ("call\t%P3", xops);
      output_asm_insn ("popf", xops);

      break;

    default:
      ASM_GENERATE_INTERNAL_LABEL (counts, "LPBX", 2);
      cnt_rtx = gen_rtx_SYMBOL_REF (VOIDmode, counts);
      SYMBOL_REF_FLAG (cnt_rtx) = TRUE;

      if (blockno)
	cnt_rtx = plus_constant (cnt_rtx, blockno*4);

      if (flag_pic)
	cnt_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, cnt_rtx);

      xops[0] = gen_rtx_MEM (SImode, cnt_rtx);
      output_asm_insn ("inc{l}\t%0", xops);

      break;
    }
}

void
ix86_expand_move (mode, operands)
     enum machine_mode mode;
     rtx operands[];
{
  int strict = (reload_in_progress || reload_completed);
  rtx insn;

  if (flag_pic && mode == Pmode && symbolic_operand (operands[1], Pmode))
    {
      /* Emit insns to move operands[1] into operands[0].  */

      if (GET_CODE (operands[0]) == MEM)
	operands[1] = force_reg (Pmode, operands[1]);
      else
	{
	  rtx temp = operands[0];
	  if (GET_CODE (temp) != REG)
	    temp = gen_reg_rtx (Pmode);
	  temp = legitimize_pic_address (operands[1], temp);
	  if (temp == operands[0])
	    return;
	  operands[1] = temp;
	}
    }
  else
    {
      if (GET_CODE (operands[0]) == MEM
	  && (GET_MODE (operands[0]) == QImode
	      || !push_operand (operands[0], mode))
	  && GET_CODE (operands[1]) == MEM)
	operands[1] = force_reg (mode, operands[1]);

      if (push_operand (operands[0], mode)
	  && ! general_no_elim_operand (operands[1], mode))
	operands[1] = copy_to_mode_reg (mode, operands[1]);

      if (FLOAT_MODE_P (mode))
	{
	  /* If we are loading a floating point constant to a register,
	     force the value to memory now, since we'll get better code
	     out the back end.  */

	  if (strict)
	    ;
	  else if (GET_CODE (operands[1]) == CONST_DOUBLE
		   && register_operand (operands[0], mode))
	    operands[1] = validize_mem (force_const_mem (mode, operands[1]));
	}
    }

  insn = gen_rtx_SET (VOIDmode, operands[0], operands[1]);

  emit_insn (insn);
}

/* Attempt to expand a binary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 3 separate
   memory references (one output, two input) in a single insn.  */

void
ix86_expand_binary_operator (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode;
     rtx operands[];
{
  int matching_memory;
  rtx src1, src2, dst, op, clob;

  dst = operands[0];
  src1 = operands[1];
  src2 = operands[2];

  /* Recognize <var1> = <value> <op> <var1> for commutative operators */
  if (GET_RTX_CLASS (code) == 'c'
      && (rtx_equal_p (dst, src2)
	  || immediate_operand (src1, mode)))
    {
      rtx temp = src1;
      src1 = src2;
      src2 = temp;
    }

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  matching_memory = 0;
  if (GET_CODE (dst) == MEM)
    {
      if (rtx_equal_p (dst, src1))
	matching_memory = 1;
      else if (GET_RTX_CLASS (code) == 'c'
	       && rtx_equal_p (dst, src2))
	matching_memory = 2;
      else
	dst = gen_reg_rtx (mode);
    }
  
  /* Both source operands cannot be in memory.  */
  if (GET_CODE (src1) == MEM && GET_CODE (src2) == MEM)
    {
      if (matching_memory != 2)
	src2 = force_reg (mode, src2);
      else
	src1 = force_reg (mode, src1);
    }

  /* If the operation is not commutable, source 1 cannot be a constant
     or non-matching memory.  */
  if ((CONSTANT_P (src1) 
       || (!matching_memory && GET_CODE (src1) == MEM))
      && GET_RTX_CLASS (code) != 'c')
    src1 = force_reg (mode, src1);
    
  /* If optimizing, copy to regs to improve CSE */
  if (optimize && !reload_in_progress && !reload_completed)
    {
      if (GET_CODE (dst) == MEM)
	dst = gen_reg_rtx (mode);
      if (GET_CODE (src1) == MEM)
	src1 = force_reg (mode, src1);
      if (GET_CODE (src2) == MEM)
	src2 = force_reg (mode, src2);
    }

  /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_ee (code, mode, src1, src2));
  if (reload_in_progress)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  We can only do this with PLUS.  */
      if (code != PLUS)
	abort ();
      emit_insn (op);
    }
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Return TRUE or FALSE depending on whether the binary operator meets the
   appropriate constraints.  */

int
ix86_binary_operator_ok (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx operands[3];
{
  /* Both source operands cannot be in memory.  */
  if (GET_CODE (operands[1]) == MEM && GET_CODE (operands[2]) == MEM)
    return 0;
  /* If the operation is not commutable, source 1 cannot be a constant.  */
  if (CONSTANT_P (operands[1]) && GET_RTX_CLASS (code) != 'c')
    return 0;
  /* If the destination is memory, we must have a matching source operand.  */
  if (GET_CODE (operands[0]) == MEM
      && ! (rtx_equal_p (operands[0], operands[1])
	    || (GET_RTX_CLASS (code) == 'c'
		&& rtx_equal_p (operands[0], operands[2]))))
    return 0;
  /* If the operation is not commutable and the source 1 is memory, we must
     have a matching destionation.  */
  if (GET_CODE (operands[1]) == MEM
      && GET_RTX_CLASS (code) != 'c'
      && ! rtx_equal_p (operands[0], operands[1]))
    return 0;
  return 1;
}

/* Attempt to expand a unary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 2 separate
   memory references (one output, one input) in a single insn.  */

void
ix86_expand_unary_operator (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode;
     rtx operands[];
{
  int matching_memory;
  rtx src, dst, op, clob;

  dst = operands[0];
  src = operands[1];

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  matching_memory = 0;
  if (GET_CODE (dst) == MEM)
    {
      if (rtx_equal_p (dst, src))
	matching_memory = 1;
      else
	dst = gen_reg_rtx (mode);
    }

  /* When source operand is memory, destination must match.  */
  if (!matching_memory && GET_CODE (src) == MEM)
    src = force_reg (mode, src);
  
  /* If optimizing, copy to regs to improve CSE */
  if (optimize && !reload_in_progress && !reload_completed)
    {
      if (GET_CODE (dst) == MEM)
	dst = gen_reg_rtx (mode);
      if (GET_CODE (src) == MEM)
	src = force_reg (mode, src);
    }

  /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_e (code, mode, src));
  if (reload_in_progress || code == NOT)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  */
      if (code != NOT)
        abort ();
      emit_insn (op);
    }
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Return TRUE or FALSE depending on whether the unary operator meets the
   appropriate constraints.  */

int
ix86_unary_operator_ok (code, mode, operands)
     enum rtx_code code ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx operands[2] ATTRIBUTE_UNUSED;
{
  /* If one of operands is memory, source and destination must match.  */
  if ((GET_CODE (operands[0]) == MEM
       || GET_CODE (operands[1]) == MEM)
      && ! rtx_equal_p (operands[0], operands[1]))
    return FALSE;
  return TRUE;
}

/* Produce an unsigned comparison for a given signed comparison.  */

static enum rtx_code
unsigned_comparison (code)
     enum rtx_code code;
{
  switch (code)
    {
    case GT:
      code = GTU;
      break;
    case LT:
      code = LTU;
      break;
    case GE:
      code = GEU;
      break;
    case LE:
      code = LEU;
      break;
    case EQ:
    case NE:
    case LEU:
    case LTU:
    case GEU:
    case GTU:
      break;
    default:
      abort ();
    }
  return code;
}

/* Generate insn patterns to do an integer compare of OPERANDS.  */

static rtx
ix86_expand_int_compare (code, op0, op1)
     enum rtx_code code;
     rtx op0, op1;
{
  enum machine_mode cmpmode;
  rtx tmp, flags;

  cmpmode = SELECT_CC_MODE (code, op0, op1);
  flags = gen_rtx_REG (cmpmode, FLAGS_REG);

  /* This is very simple, but making the interface the same as in the
     FP case makes the rest of the code easier.  */
  tmp = gen_rtx_COMPARE (cmpmode, op0, op1);
  emit_insn (gen_rtx_SET (VOIDmode, flags, tmp));

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode, flags, const0_rtx);
}

/* Generate insn patterns to do a floating point compare of OPERANDS.
   If UNORDERED, allow for unordered compares.  */

static rtx
ix86_expand_fp_compare (code, op0, op1, unordered)
     enum rtx_code code;
     rtx op0, op1;
     int unordered;
{
  enum machine_mode fpcmp_mode;
  enum machine_mode intcmp_mode;
  rtx tmp;

  /* When not doing IEEE compliant compares, disable unordered.  */
  if (! TARGET_IEEE_FP)
    unordered = 0;
  fpcmp_mode = unordered ? CCFPUmode : CCFPmode;

  /* ??? If we knew whether invalid-operand exceptions were masked,
     we could rely on fcom to raise an exception and take care of
     NaNs.  But we don't.  We could know this from c9x math bits.  */
  if (TARGET_IEEE_FP)
    unordered = 1;

  /* All of the unordered compare instructions only work on registers.
     The same is true of the XFmode compare instructions.  */
  if (unordered || GET_MODE (op0) == XFmode)
    {
      op0 = force_reg (GET_MODE (op0), op0);
      op1 = force_reg (GET_MODE (op1), op1);
    }
  else
    {
      /* %%% We only allow op1 in memory; op0 must be st(0).  So swap
	 things around if they appear profitable, otherwise force op0
	 into a register.  */

      if (standard_80387_constant_p (op0) == 0
	  || (GET_CODE (op0) == MEM
	      && ! (standard_80387_constant_p (op1) == 0
		    || GET_CODE (op1) == MEM)))
	{
	  rtx tmp;
	  tmp = op0, op0 = op1, op1 = tmp;
	  code = swap_condition (code);
	}

      if (GET_CODE (op0) != REG)
	op0 = force_reg (GET_MODE (op0), op0);

      if (CONSTANT_P (op1))
	{
	  if (standard_80387_constant_p (op1))
	    op1 = force_reg (GET_MODE (op1), op1);
	  else
	    op1 = validize_mem (force_const_mem (GET_MODE (op1), op1));
	}
    }

  /* %%% fcomi is probably always faster, even when dealing with memory,
     since compare-and-branch would be three insns instead of four.  */
  if (TARGET_CMOVE && !unordered)
    {
      if (GET_CODE (op0) != REG)
	op0 = force_reg (GET_MODE (op0), op0);
      if (GET_CODE (op1) != REG)
	op1 = force_reg (GET_MODE (op1), op1);

      tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
      tmp = gen_rtx_SET (VOIDmode, gen_rtx_REG (fpcmp_mode, FLAGS_REG), tmp);
      emit_insn (tmp);

      /* The FP codes work out to act like unsigned.  */
      code = unsigned_comparison (code);
      intcmp_mode = fpcmp_mode;
    }
  else
    {
      /* Sadness wrt reg-stack pops killing fpsr -- gotta get fnstsw first.  */

      rtx tmp2;
      tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
      tmp2 = gen_rtx_UNSPEC (HImode, gen_rtvec (1, tmp), 9);
      tmp = gen_reg_rtx (HImode);
      emit_insn (gen_rtx_SET (VOIDmode, tmp, tmp2));

      if (! unordered)
	{
	  /* We have two options here -- use sahf, or testing bits of ah
	     directly.  On PPRO, they are equivalent, sahf being one byte
	     smaller.  On Pentium, sahf is non-pairable while test is UV
	     pairable.  */

	  if (TARGET_USE_SAHF || optimize_size)
	    {
	    do_sahf:

	      /* The FP codes work out to act like unsigned.  */
	      code = unsigned_comparison (code);
	      emit_insn (gen_x86_sahf_1 (tmp));
	      intcmp_mode = CCmode;
	    }
	  else
	    {
	      /*
	       * The numbers below correspond to the bits of the FPSW in AH.
	       * C3, C2, and C0 are in bits 0x40, 0x4, and 0x01 respectively.
	       *
	       *    cmp    C3 C2 C0
	       *    >      0  0  0
	       *    <      0  0  1
	       *    =      1  0  0
	       *    un     1  1  1
	       */

	      int mask;

	      switch (code)
		{
		case GT:
		  mask = 0x41;
		  code = EQ;
		  break;
		case LT:
		  mask = 0x01;
		  code = NE;
		  break;
		case GE:
		  /* We'd have to use `xorb 1,ah; andb 0x41,ah', so it's 
		     faster in all cases to just fall back on sahf. */
		  goto do_sahf;
		case LE:
		  mask = 0x41;
		  code = NE;
		  break;
		case EQ:
		  mask = 0x40;
		  code = NE;
		  break;
		case NE:
		  mask = 0x40;
		  code = EQ;
		  break;
		default:
		  abort ();
		}

	      emit_insn (gen_testqi_ext_0 (tmp, GEN_INT (mask)));
	      intcmp_mode = CCNOmode;
	    }
	}
      else
	{
	  /* In the unordered case, we have to check C2 for NaN's, which
	     doesn't happen to work out to anything nice combination-wise.
	     So do some bit twiddling on the value we've got in AH to come
	     up with an appropriate set of condition codes.  */

	  intcmp_mode = CCNOmode;
	  switch (code)
	    {
	    case GT:
	      emit_insn (gen_testqi_ext_0 (tmp, GEN_INT (0x45)));
	      code = EQ;
	      break;
	    case LT:
	      emit_insn (gen_andqi_ext_0 (tmp, tmp, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (tmp, GEN_INT (0x01)));
	      intcmp_mode = CCmode;
	      code = EQ;
	      break;
	    case GE:
	      emit_insn (gen_testqi_ext_0 (tmp, GEN_INT (0x05)));
	      code = EQ;
	      break;
	    case LE:
	      emit_insn (gen_andqi_ext_0 (tmp, tmp, GEN_INT (0x45)));
	      emit_insn (gen_addqi_ext_1 (tmp, tmp, constm1_rtx));
	      emit_insn (gen_cmpqi_ext_3 (tmp, GEN_INT (0x40)));
	      intcmp_mode = CCmode;
	      code = LTU;
	      break;
	    case EQ:
	      emit_insn (gen_andqi_ext_0 (tmp, tmp, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (tmp, GEN_INT (0x40)));
	      intcmp_mode = CCmode;
	      code = EQ;
	      break;
	    case NE:
	      emit_insn (gen_andqi_ext_0 (tmp, tmp, GEN_INT (0x45)));
	      emit_insn (gen_xorqi_cc_ext_1 (tmp, tmp, GEN_INT (0x40)));
	      code = NE;
	      break;
	    default:
	      abort ();
	    }
	}
    }

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode,
			 gen_rtx_REG (intcmp_mode, FLAGS_REG),
			 const0_rtx);
}

static rtx
ix86_expand_compare (code, unordered)
     enum rtx_code code;
     int unordered;
{
  rtx op0, op1, ret;
  op0 = ix86_compare_op0;
  op1 = ix86_compare_op1;

  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    ret = ix86_expand_fp_compare (code, op0, op1, unordered);
  else
    ret = ix86_expand_int_compare (code, op0, op1);

  return ret;
}

void
ix86_expand_branch (code, unordered, label)
     enum rtx_code code;
     int unordered;
     rtx label;
{
  rtx tmp, lo[2], hi[2], label2;
  enum rtx_code code1, code2, code3;

  if (GET_MODE (ix86_compare_op0) != DImode)
    {
      tmp = ix86_expand_compare (code, unordered);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
      return;
    }

  /* Expand DImode branch into multiple compare+branch.  */

  if (CONSTANT_P (ix86_compare_op0) && ! CONSTANT_P (ix86_compare_op1))
    {
      tmp = ix86_compare_op0;
      ix86_compare_op0 = ix86_compare_op1;
      ix86_compare_op1 = tmp;
      code = swap_condition (code);
    }
  split_di (&ix86_compare_op0, 1, lo+0, hi+0);
  split_di (&ix86_compare_op1, 1, lo+1, hi+1);

  /* When comparing for equality, we can use (hi0^hi1)|(lo0^lo1) to avoid
     two branches.  This costs one extra insn, so disable when optimizing
     for size.  */

  if ((code == EQ || code == NE)
      && (!optimize_size
	  || hi[1] == const0_rtx || lo[1] == const0_rtx))
    {
      rtx xor0, xor1;

      xor1 = hi[0];
      if (hi[1] != const0_rtx)
	{
	  xor1 = expand_binop (SImode, xor_optab, xor1, hi[1],
			       NULL_RTX, 0, OPTAB_WIDEN);
	}

      xor0 = lo[0];
      if (lo[1] != const0_rtx)
	{
	  xor0 = expand_binop (SImode, xor_optab, xor0, lo[1],
			       NULL_RTX, 0, OPTAB_WIDEN);
	}

      tmp = expand_binop (SImode, ior_optab, xor1, xor0,
			  NULL_RTX, 0, OPTAB_WIDEN);

      ix86_compare_op0 = tmp;
      ix86_compare_op1 = const0_rtx;
      ix86_expand_branch (code, unordered, label);
      return;
    }

  /* Otherwise, if we are doing less-than, op1 is a constant and the
     low word is zero, then we can just examine the high word.  */

  if (GET_CODE (hi[1]) == CONST_INT && lo[1] == const0_rtx
      && (code == LT || code == LTU))
    {
      ix86_compare_op0 = hi[0];
      ix86_compare_op1 = hi[1];
      ix86_expand_branch (code, unordered, label);
      return;
    }

  /* Otherwise, we need two or three jumps.  */

  label2 = gen_label_rtx ();

  code1 = code;
  code2 = swap_condition (code);
  code3 = unsigned_condition (code);

  switch (code)
    {
    case LT: case GT: case LTU: case GTU:
      break;

    case LE:   code1 = LT;  code2 = GT;  break;
    case GE:   code1 = GT;  code2 = LT;  break;
    case LEU:  code1 = LTU; code2 = GTU; break;
    case GEU:  code1 = GTU; code2 = LTU; break;

    case EQ:   code1 = NIL; code2 = NE;  break;
    case NE:   code2 = NIL; break;

    default:
      abort ();
    }

  /*
   * a < b =>
   *    if (hi(a) < hi(b)) goto true;
   *    if (hi(a) > hi(b)) goto false;
   *    if (lo(a) < lo(b)) goto true;
   *  false:
   */

  ix86_compare_op0 = hi[0];
  ix86_compare_op1 = hi[1];

  if (code1 != NIL)
    ix86_expand_branch (code1, unordered, label);
  if (code2 != NIL)
    ix86_expand_branch (code2, unordered, label2);

  ix86_compare_op0 = lo[0];
  ix86_compare_op1 = lo[1];
  ix86_expand_branch (code3, unordered, label);

  if (code2 != NIL)
    emit_label (label2);
}

int
ix86_expand_setcc (code, unordered, dest)
     enum rtx_code code;
     int unordered;
     rtx dest;
{
  rtx ret, tmp;
  int type;

  if (GET_MODE (ix86_compare_op0) == DImode)
    return 0; /* FAIL */

  /* Three modes of generation:
     0 -- destination does not overlap compare sources:
          clear dest first, emit strict_low_part setcc.
     1 -- destination does overlap compare sources:
          emit subreg setcc, zero extend.
     2 -- destination is in QImode:
          emit setcc only.
  */

  type = 0;

  if (GET_MODE (dest) == QImode)
    type = 2;
  else if (reg_overlap_mentioned_p (dest, ix86_compare_op0)
	   || reg_overlap_mentioned_p (dest, ix86_compare_op1))
    type = 1;

  if (type == 0)
    emit_move_insn (dest, const0_rtx);

  ret = ix86_expand_compare (code, unordered);
  PUT_MODE (ret, QImode);

  tmp = dest;
  if (type == 0)
    {
      tmp = gen_lowpart (QImode, dest);
      tmp = gen_rtx_STRICT_LOW_PART (VOIDmode, tmp);
    }
  else if (type == 1)
    {
      if (!cse_not_expected)
	tmp = gen_reg_rtx (QImode);
      else
        tmp = gen_lowpart (QImode, dest);
    }

  emit_insn (gen_rtx_SET (VOIDmode, tmp, ret));

  if (type == 1)
    {
      rtx clob;

      tmp = gen_rtx_ZERO_EXTEND (GET_MODE (dest), tmp);
      tmp = gen_rtx_SET (VOIDmode, dest, tmp);
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
      emit_insn (tmp);
    }

  return 1; /* DONE */
}

int
ix86_expand_int_movcc (operands)
     rtx operands[];
{
  enum rtx_code code = GET_CODE (operands[1]), compare_code;
  rtx compare_seq, compare_op;

  /* When the compare code is not LTU or GEU, we can not use sbbl case.
     In case comparsion is done with immediate, we can convert it to LTU or
     GEU by altering the integer.  */

  if ((code == LEU || code == GTU)
      && GET_CODE (ix86_compare_op1) == CONST_INT
      && GET_MODE (operands[0]) != HImode
      && (unsigned int)INTVAL (ix86_compare_op1) != 0xffffffff
      && GET_CODE (operands[2]) == CONST_INT 
      && GET_CODE (operands[3]) == CONST_INT)
    {
      if (code == LEU)
	code = LTU;
      else
	code = GEU;
      ix86_compare_op1 = GEN_INT (INTVAL (ix86_compare_op1) + 1);
    }
  start_sequence ();
  compare_op = ix86_expand_compare (code, code == EQ || code == NE);
  compare_seq = gen_sequence ();
  end_sequence ();

  compare_code = GET_CODE (compare_op);

  /* Don't attempt mode expansion here -- if we had to expand 5 or 6
     HImode insns, we'd be swallowed in word prefix ops.  */

  if (GET_MODE (operands[0]) != HImode
      && GET_CODE (operands[2]) == CONST_INT 
      && GET_CODE (operands[3]) == CONST_INT)
    {
      rtx out = operands[0];
      HOST_WIDE_INT ct = INTVAL (operands[2]);
      HOST_WIDE_INT cf = INTVAL (operands[3]);
      HOST_WIDE_INT diff;

      if (compare_code == LTU || compare_code == GEU)
	{

	  /* Detect overlap between destination and compare sources.  */
	  rtx tmp = out;

	  /* To simplify rest of code, restrict to the GEU case. */
	  if (compare_code == LTU)
	    {
	      int tmp = ct;
	      ct = cf;
	      cf = tmp;
	      compare_code = reverse_condition (compare_code);
	      code = reverse_condition (code);
	    }
	  diff = ct - cf;

	  if (reg_overlap_mentioned_p (out, ix86_compare_op0)
	      || reg_overlap_mentioned_p (out, ix86_compare_op1))
	    tmp = gen_reg_rtx (SImode);

	  emit_insn (compare_seq);
	  emit_insn (gen_x86_movsicc_0_m1 (tmp));

	  if (diff == 1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * [addl dest, ct]
	       *
	       * Size 5 - 8.
	       */
	      if (ct)
	        emit_insn (gen_addsi3 (out, out, GEN_INT (ct)));
	    }
	  else if (cf == -1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * orl $ct, dest
	       *
	       * Size 8.
	       */
	      emit_insn (gen_iorsi3 (out, out, GEN_INT (ct)));
	    }
	  else if (diff == -1 && ct)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * xorl $-1, dest
	       * [addl dest, cf]
	       *
	       * Size 8 - 11.
	       */
	      emit_insn (gen_one_cmplsi2 (tmp, tmp));
	      if (cf)
	        emit_insn (gen_addsi3 (out, out, GEN_INT (cf)));
	    }
	  else
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * andl cf - ct, dest
	       * [addl dest, ct]
	       *
	       * Size 8 - 11.
	       */
	      emit_insn (gen_andsi3 (out, out, GEN_INT (cf - ct)));
	      if (ct)
	        emit_insn (gen_addsi3 (out, out, GEN_INT (ct)));
	    }

	  if (tmp != out)
	    emit_move_insn (out, tmp);

	  return 1; /* DONE */
	}

      diff = ct - cf;
      if (diff < 0)
	{
	  HOST_WIDE_INT tmp;
	  tmp = ct, ct = cf, cf = tmp;
	  diff = -diff;
	  compare_code = reverse_condition (compare_code);
	  code = reverse_condition (code);
	}
      if (diff == 1 || diff == 2 || diff == 4 || diff == 8
	  || diff == 3 || diff == 5 || diff == 9)
	{
	  /*
	   * xorl dest,dest
	   * cmpl op1,op2
	   * setcc dest
	   * lea cf(dest*(ct-cf)),dest
	   *
	   * Size 14.
	   *
	   * This also catches the degenerate setcc-only case.
	   */

	  rtx tmp;
	  int nops;

	  out = emit_store_flag (out, code, ix86_compare_op0,
				 ix86_compare_op1, VOIDmode, 0, 1);

	  nops = 0;
	  if (diff == 1)
	      tmp = out;
	  else
	    {
	      tmp = gen_rtx_MULT (SImode, out, GEN_INT (diff & ~1));
	      nops++;
	      if (diff & 1)
		{
		  tmp = gen_rtx_PLUS (SImode, tmp, out);
		  nops++;
		}
	    }
	  if (cf != 0)
	    {
	      tmp = gen_rtx_PLUS (SImode, tmp, GEN_INT (cf));
	      nops++;
	    }
	  if (tmp != out)
	    {
	      if (nops == 0)
		emit_move_insn (out, tmp);
	      else if (nops == 1)
		{
		  rtx clob;

		  clob = gen_rtx_REG (CCmode, FLAGS_REG);
		  clob = gen_rtx_CLOBBER (VOIDmode, clob);

		  tmp = gen_rtx_SET (VOIDmode, out, tmp);
		  tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
		  emit_insn (tmp);
		}
	      else
		emit_insn (gen_rtx_SET (VOIDmode, out, tmp));
	    }
	  if (out != operands[0])
	    emit_move_insn (operands[0], out);

	  return 1; /* DONE */
	}

      /*
       * General case:			Jumpful:
       *   xorl dest,dest		cmpl op1, op2
       *   cmpl op1, op2		movl ct, dest
       *   setcc dest			jcc 1f
       *   decl dest			movl cf, dest
       *   andl (cf-ct),dest		1:
       *   addl ct,dest
       * 
       * Size 20.			Size 14.
       *
       * This is reasonably steep, but branch mispredict costs are
       * high on modern cpus, so consider failing only if optimizing
       * for space.
       *
       * %%% Parameterize branch_cost on the tuning architecture, then
       * use that.  The 80386 couldn't care less about mispredicts.
       */

      if (!optimize_size && !TARGET_CMOVE)
	{
	  if (ct == 0)
	    {
	      ct = cf;
	      cf = 0;
	      compare_code = reverse_condition (compare_code);
	      code = reverse_condition (code);
	    }

	  out = emit_store_flag (out, code, ix86_compare_op0,
				 ix86_compare_op1, VOIDmode, 0, 1);

	  emit_insn (gen_addsi3 (out, out, constm1_rtx));
	  emit_insn (gen_andsi3 (out, out, GEN_INT (cf-ct)));
	  if (ct != 0)
	    emit_insn (gen_addsi3 (out, out, GEN_INT (ct)));
	  if (out != operands[0])
	    emit_move_insn (operands[0], out);

	  return 1; /* DONE */
	}
    }

  if (!TARGET_CMOVE)
    {
      /* Try a few things more with specific constants and a variable.  */

      optab op;
      rtx var, orig_out, out, tmp;

      if (optimize_size)
	return 0; /* FAIL */

      /* If one of the two operands is an interesting constant, load a 
	 constant with the above and mask it in with a logical operation.  */
	 
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  var = operands[3];
	  if (INTVAL (operands[2]) == 0)
	    operands[3] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[2]) == -1)
	    operands[3] = const0_rtx, op = ior_optab;
	  else
	    return 0; /* FAIL */
	}
      else if (GET_CODE (operands[3]) == CONST_INT)
	{
	  var = operands[2];
	  if (INTVAL (operands[3]) == 0)
	    operands[2] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[3]) == -1)
	    operands[2] = const0_rtx, op = ior_optab;
	  else
	    return 0; /* FAIL */
	}
      else
        return 0; /* FAIL */

      orig_out = operands[0];
      tmp = gen_reg_rtx (GET_MODE (orig_out));
      operands[0] = tmp;

      /* Recurse to get the constant loaded.  */
      if (ix86_expand_int_movcc (operands) == 0)
        return 0; /* FAIL */

      /* Mask in the interesting variable.  */
      out = expand_binop (GET_MODE (orig_out), op, var, tmp, orig_out, 0,
			  OPTAB_WIDEN);
      if (out != orig_out)
	emit_move_insn (orig_out, out);

      return 1; /* DONE */
    }

  /*
   * For comparison with above,
   *
   * movl cf,dest
   * movl ct,tmp
   * cmpl op1,op2
   * cmovcc tmp,dest
   *
   * Size 15.
   */

  if (! nonimmediate_operand (operands[2], GET_MODE (operands[0])))
    operands[2] = force_reg (GET_MODE (operands[0]), operands[2]);
  if (! nonimmediate_operand (operands[3], GET_MODE (operands[0])))
    operands[3] = force_reg (GET_MODE (operands[0]), operands[3]);

  emit_insn (compare_seq);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
						compare_op, operands[2],
						operands[3])));

  return 1; /* DONE */
}

int
ix86_expand_fp_movcc (operands)
     rtx operands[];
{
  enum rtx_code code;
  enum machine_mode mode;
  rtx tmp;

  /* The floating point conditional move instructions don't directly
     support conditions resulting from a signed integer comparison. */

  code = GET_CODE (operands[1]);
  switch (code)
    {
    case LT:
    case LE:
    case GE:
    case GT:
      tmp = gen_reg_rtx (QImode);
      ix86_expand_setcc (code, 0, tmp);
      code = NE;
      ix86_compare_op0 = tmp;
      ix86_compare_op1 = const0_rtx;
      break;

    default:
      break;
    }

  mode = SELECT_CC_MODE (code, ix86_compare_op0, ix86_compare_op1);
  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_REG (mode, FLAGS_REG),
			  gen_rtx_COMPARE (mode,
					   ix86_compare_op0,
					   ix86_compare_op1)));
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				gen_rtx_fmt_ee (code, VOIDmode,
						gen_rtx_REG (mode, FLAGS_REG),
						const0_rtx),
				operands[2],
				operands[3])));

  return 1;
}

/* Split operands 0 and 1 into SImode parts.  Similar to split_di, but
   works for floating pointer parameters and nonoffsetable memories.
   For pushes, it returns just stack offsets; the values will be saved
   in the right order.  Maximally three parts are generated.  */

static void
ix86_split_to_parts (operand, parts, mode)
     rtx operand;
     rtx *parts;
     enum machine_mode mode;
{
  int size = GET_MODE_SIZE (mode) / 4;

  if (size < 2 || size > 3)
    abort ();

  /* Optimize constant pool reference to immediates.  This is used by fp moves,
     that force all constants to memory to allow combining.  */

  if (GET_CODE (operand) == MEM
      && GET_CODE (XEXP (operand, 0)) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (XEXP (operand, 0)))
    operand = get_pool_constant (XEXP (operand, 0));

  if (GET_CODE (operand) == MEM && !offsettable_memref_p (operand))
    {
      /* The only non-offsetable memories we handle are pushes.  */
      if (! push_operand (operand, VOIDmode))
	abort ();

      PUT_MODE (operand, SImode);
      parts[0] = parts[1] = parts[2] = operand;
    }
  else
    {
      if (mode == DImode)
	split_di (&operand, 1, &parts[0], &parts[1]);
      else
	{
	  if (REG_P (operand))
	    {
	      if (!reload_completed)
		abort ();
	      parts[0] = gen_rtx_REG (SImode, REGNO (operand) + 0);
	      parts[1] = gen_rtx_REG (SImode, REGNO (operand) + 1);
	      if (size == 3)
		parts[2] = gen_rtx_REG (SImode, REGNO (operand) + 2);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      PUT_MODE (operand, SImode);
	      parts[0] = operand;
	      parts[1] = adj_offsettable_operand (operand, 4);
	      if (size == 3)
		parts[2] = adj_offsettable_operand (operand, 8);
	    }
	  else if (GET_CODE (operand) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r;
	      long l[3];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	      switch (mode)
		{
		case XFmode:
		  REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
		  parts[2] = GEN_INT (l[2]);
		  break;
		case DFmode:
		  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
		  break;
		default:
		  abort ();
		}
	      parts[1] = GEN_INT (l[1]);
	      parts[0] = GEN_INT (l[0]);
	    }
	  else
	    abort ();
	}
    }

  return;
}

/* Emit insns to perform a move or push of DI, DF, and XF values.
   Return false when normal moves are needed; true when all required
   insns have been emitted.  Operands 2-4 contain the input values
   int the correct order; operands 5-7 contain the output values.  */

int 
ix86_split_long_move (operands1)
     rtx operands1[];
{
  rtx part[2][3];
  rtx operands[2];
  int size = GET_MODE_SIZE (GET_MODE (operands1[0])) / 4;
  int push = 0;
  int collisions = 0;

  /* Make our own copy to avoid clobbering the operands.  */
  operands[0] = copy_rtx (operands1[0]);
  operands[1] = copy_rtx (operands1[1]);

  if (size < 2 || size > 3)
    abort ();

  /* The only non-offsettable memory we handle is push.  */
  if (push_operand (operands[0], VOIDmode))
    push = 1;
  else if (GET_CODE (operands[0]) == MEM
	   && ! offsettable_memref_p (operands[0]))
    abort ();

  ix86_split_to_parts (operands[0], part[0], GET_MODE (operands1[0]));
  ix86_split_to_parts (operands[1], part[1], GET_MODE (operands1[0]));

  /* When emitting push, take care for source operands on the stack.  */
  if (push && GET_CODE (operands[1]) == MEM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    {
      if (size == 3)
	part[1][1] = part[1][2];
      part[1][0] = part[1][1];
    }

  /* We need to do copy in the right order in case an address register 
     of the source overlaps the destination.  */
  if (REG_P (part[0][0]) && GET_CODE (part[1][0]) == MEM)
    {
      if (reg_overlap_mentioned_p (part[0][0], XEXP (part[1][0], 0)))
	collisions++;
      if (reg_overlap_mentioned_p (part[0][1], XEXP (part[1][0], 0)))
	collisions++;
      if (size == 3
	  && reg_overlap_mentioned_p (part[0][2], XEXP (part[1][0], 0)))
	collisions++;

      /* Collision in the middle part can be handled by reordering.  */
      if (collisions == 1 && size == 3
	  && reg_overlap_mentioned_p (part[0][1], XEXP (part[1][0], 0)))
	{
	  rtx tmp;
	  tmp = part[0][1]; part[0][1] = part[0][2]; part[0][2] = tmp;
	  tmp = part[1][1]; part[1][1] = part[1][2]; part[1][2] = tmp;
	}

      /* If there are more collisions, we can't handle it by reordering.
	 Do an lea to the last part and use only one colliding move.  */
      else if (collisions > 1)
	{
	  collisions = 1;
	  emit_insn (gen_rtx_SET (VOIDmode, part[0][size - 1],
				  XEXP (part[1][0], 0)));
	  part[1][0] = change_address (part[1][0], SImode, part[0][size - 1]);
	  part[1][1] = adj_offsettable_operand (part[1][0], 4);
	  if (size == 3)
	    part[1][2] = adj_offsettable_operand (part[1][0], 8);
	}
    }

  if (push)
    {
      if (size == 3)
	emit_insn (gen_push (part[1][2]));
      emit_insn (gen_push (part[1][1]));
      emit_insn (gen_push (part[1][0]));
      return 1;
    }

  /* Choose correct order to not overwrite the source before it is copied.  */
  if ((REG_P (part[0][0])
       && REG_P (part[1][1])
       && (REGNO (part[0][0]) == REGNO (part[1][1])
	   || (size == 3
	       && REGNO (part[0][0]) == REGNO (part[1][2]))))
      || (collisions > 0
	  && reg_overlap_mentioned_p (part[0][0], XEXP (part[1][0], 0))))
    {
      if (size == 3)
	{
	  operands1[2] = part[0][2];
	  operands1[3] = part[0][1];
	  operands1[4] = part[0][0];
	  operands1[5] = part[1][2];
	  operands1[6] = part[1][1];
	  operands1[7] = part[1][0];
	}
      else
	{
	  operands1[2] = part[0][1];
	  operands1[3] = part[0][0];
	  operands1[5] = part[1][1];
	  operands1[6] = part[1][0];
	}
    }
  else
    {
      if (size == 3)
	{
	  operands1[2] = part[0][0];
	  operands1[3] = part[0][1];
	  operands1[4] = part[0][2];
	  operands1[5] = part[1][0];
	  operands1[6] = part[1][1];
	  operands1[7] = part[1][2];
	}
      else
	{
	  operands1[2] = part[0][0];
	  operands1[3] = part[0][1];
	  operands1[5] = part[1][0];
	  operands1[6] = part[1][1];
	}
    }

  return 0;
}

void
ix86_split_ashldi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (high[0], low[1]);
	  emit_move_insn (low[0], const0_rtx);

	  if (count > 32)
	    emit_insn (gen_ashlsi3 (high[0], high[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shld_1 (high[0], low[0], GEN_INT (count)));
	  emit_insn (gen_ashlsi3 (low[0], low[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shld_1 (high[0], low[0], operands[2]));
      emit_insn (gen_ashlsi3 (low[0], low[0], operands[2]));

      if (TARGET_CMOVE && (! reload_completed || scratch))
	{
	  if (! reload_completed)
	    scratch = force_reg (SImode, const0_rtx);
	  else
	    emit_move_insn (scratch, const0_rtx);

	  emit_insn (gen_x86_shift_adj_1 (high[0], low[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (high[0], low[0], operands[2]));
    }
}

void
ix86_split_ashrdi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (low[0], high[1]);

	  if (! reload_completed)
	    emit_insn (gen_ashrsi3 (high[0], low[0], GEN_INT (31)));
	  else
	    {
	      emit_move_insn (high[0], low[0]);
	      emit_insn (gen_ashrsi3 (high[0], high[0], GEN_INT (31)));
	    }

	  if (count > 32)
	    emit_insn (gen_ashrsi3 (low[0], low[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shrd_1 (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_ashrsi3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shrd_1 (low[0], high[0], operands[2]));
      emit_insn (gen_ashrsi3 (high[0], high[0], operands[2]));

      if (TARGET_CMOVE && (!reload_completed || scratch))
	{
	  if (! reload_completed)
	    scratch = gen_reg_rtx (SImode);
	  emit_move_insn (scratch, high[0]);
	  emit_insn (gen_ashrsi3 (scratch, scratch, GEN_INT (31)));
	  emit_insn (gen_x86_shift_adj_1 (low[0], high[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_3 (low[0], high[0], operands[2]));
    }
}

void
ix86_split_lshrdi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (low[0], high[1]);
	  emit_move_insn (high[0], const0_rtx);

	  if (count > 32)
	    emit_insn (gen_lshrsi3 (low[0], low[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shrd_1 (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_lshrsi3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shrd_1 (low[0], high[0], operands[2]));
      emit_insn (gen_lshrsi3 (high[0], high[0], operands[2]));

      /* Heh.  By reversing the arguments, we can reuse this pattern.  */
      if (TARGET_CMOVE && (! reload_completed || scratch))
	{
	  if (! reload_completed)
	    scratch = force_reg (SImode, const0_rtx);
	  else
	    emit_move_insn (scratch, const0_rtx);

	  emit_insn (gen_x86_shift_adj_1 (low[0], high[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (low[0], high[0], operands[2]));
    }
}

/* Expand the appropriate insns for doing strlen if not just doing
   repnz; scasb

   out = result, initialized with the start address
   align_rtx = alignment of the address.
   scratch = scratch register, initialized with the startaddress when
   	not aligned, otherwise undefined

   This is just the body. It needs the initialisations mentioned above and
   some address computing at the end.  These things are done in i386.md.  */

void
ix86_expand_strlensi_unroll_1 (out, align_rtx, scratch)
     rtx out, align_rtx, scratch;
{
  int align;
  rtx tmp;
  rtx align_2_label = NULL_RTX;
  rtx align_3_label = NULL_RTX;
  rtx align_4_label = gen_label_rtx ();
  rtx end_0_label = gen_label_rtx ();
  rtx mem;
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx tmpreg = gen_reg_rtx (SImode);

  align = 0;
  if (GET_CODE (align_rtx) == CONST_INT)
    align = INTVAL (align_rtx);

  /* Loop to check 1..3 bytes for null to get an aligned pointer.  */

  /* Is there a known alignment and is it less than 4?  */
  if (align < 4)
    {
      /* Is there a known alignment and is it not 2? */
      if (align != 2)
	{
	  align_3_label = gen_label_rtx (); /* Label when aligned to 3-byte */
	  align_2_label = gen_label_rtx (); /* Label when aligned to 2-byte */

	  /* Leave just the 3 lower bits.  */
	  align_rtx = expand_binop (SImode, and_optab, scratch, GEN_INT (3),
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_insn (gen_cmpsi_0 (align_rtx, const0_rtx));

	  tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
	  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				      gen_rtx_LABEL_REF (VOIDmode,
							 align_4_label),
				      pc_rtx);
	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));

	  emit_insn (gen_cmpsi_1 (align_rtx, GEN_INT (2)));

	  tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
	  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				      gen_rtx_LABEL_REF (VOIDmode,
							 align_2_label),
				      pc_rtx);
	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));

	  tmp = gen_rtx_GTU (VOIDmode, flags, const0_rtx);
	  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				      gen_rtx_LABEL_REF (VOIDmode,
							 align_3_label),
				      pc_rtx);
	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
	}
      else
        {
	  /* Since the alignment is 2, we have to check 2 or 0 bytes;
	     check if is aligned to 4 - byte.  */

	  align_rtx = expand_binop (SImode, and_optab, scratch, GEN_INT (2),
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_insn (gen_cmpsi_0 (align_rtx, const0_rtx));

	  tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
	  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				      gen_rtx_LABEL_REF (VOIDmode,
							 align_4_label),
				      pc_rtx);
	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
        }

      mem = gen_rtx_MEM (QImode, out);

      /* Now compare the bytes.  */

      /* Compare the first n unaligned byte on a byte per byte basis. */
      emit_insn (gen_cmpqi_0 (mem, const0_rtx));

      tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				  gen_rtx_LABEL_REF (VOIDmode, end_0_label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));

      /* Increment the address. */
      emit_insn (gen_addsi3 (out, out, const1_rtx));

      /* Not needed with an alignment of 2 */
      if (align != 2)
	{
	  emit_label (align_2_label);

	  emit_insn (gen_cmpqi_0 (mem, const0_rtx));

	  tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
	  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				      gen_rtx_LABEL_REF (VOIDmode,
							 end_0_label),
				      pc_rtx);
	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));

	  emit_insn (gen_addsi3 (out, out, const1_rtx));

	  emit_label (align_3_label);
	}

      emit_insn (gen_cmpqi_0 (mem, const0_rtx));

      tmp = gen_rtx_EQ (VOIDmode, flags, const0_rtx);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp, 
				  gen_rtx_LABEL_REF (VOIDmode, end_0_label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));

      emit_insn (gen_addsi3 (out, out, const1_rtx));
    }

  /* Generate loop to check 4 bytes at a time.  It is not a good idea to
     align this loop.  It gives only huge programs, but does not help to
     speed up.  */
  emit_label (align_4_label);

  mem = gen_rtx_MEM (SImode, out);
  emit_move_insn (scratch, mem);
  emit_insn (gen_addsi3 (out, out, GEN_INT (4)));

  /* This formula yields a nonzero result iff one of the bytes is zero.
     This saves three branches inside loop and many cycles.  */

  emit_insn (gen_addsi3 (tmpreg, scratch, GEN_INT (-0x01010101)));
  emit_insn (gen_one_cmplsi2 (scratch, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg, GEN_INT (0x80808080)));
  emit_cmp_and_jump_insns (tmpreg, const0_rtx, EQ, 0, SImode, 1, 0, align_4_label);

  if (TARGET_CMOVE)
    {
       rtx reg = gen_reg_rtx (SImode);
       emit_move_insn (reg, tmpreg);
       emit_insn (gen_lshrsi3 (reg, reg, GEN_INT (16)));

       /* If zero is not in the first two bytes, move two bytes forward. */
       emit_insn (gen_testsi_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (VOIDmode, tmpreg,
			       gen_rtx_IF_THEN_ELSE (SImode, tmp,
				       		     reg, 
				       		     tmpreg)));
       /* Emit lea manually to avoid clobbering of flags.  */
       emit_insn (gen_rtx_SET (SImode, reg,
			       gen_rtx_PLUS (SImode, out, GEN_INT (2))));

       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (VOIDmode, out,
			       gen_rtx_IF_THEN_ELSE (SImode, tmp,
				       		     reg,
				       		     out)));

    }
  else
    {
       rtx end_2_label = gen_label_rtx ();
       /* Is zero in the first two bytes? */

       emit_insn (gen_testsi_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_NE (VOIDmode, tmp, const0_rtx);
       tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
                            gen_rtx_LABEL_REF (VOIDmode, end_2_label),
                            pc_rtx);
       tmp = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
       JUMP_LABEL (tmp) = end_2_label;

       /* Not in the first two.  Move two bytes forward. */
       emit_insn (gen_lshrsi3 (tmpreg, tmpreg, GEN_INT (16)));
       emit_insn (gen_addsi3 (out, out, GEN_INT (2)));

       emit_label (end_2_label);

    }

  /* Avoid branch in fixing the byte. */
  tmpreg = gen_lowpart (QImode, tmpreg);
  emit_insn (gen_addqi3_cc (tmpreg, tmpreg, tmpreg));
  emit_insn (gen_subsi3_carry (out, out, GEN_INT (3)));

  emit_label (end_0_label);
}

/* Clear stack slot assignments remembered from previous functions.
   This is called from INIT_EXPANDERS once before RTL is emitted for each
   function.  */

static void
ix86_init_machine_status (p)
     struct function *p;
{
  enum machine_mode mode;
  int n;
  p->machine
    = (struct machine_function *) xmalloc (sizeof (struct machine_function));

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    for (n = 0; n < MAX_386_STACK_LOCALS; n++)
      ix86_stack_locals[(int) mode][n] = NULL_RTX;
}

/* Mark machine specific bits of P for GC.  */
static void
ix86_mark_machine_status (p)
     struct function *p;
{
  enum machine_mode mode;
  int n;

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    for (n = 0; n < MAX_386_STACK_LOCALS; n++)
      ggc_mark_rtx (p->machine->stack_locals[(int) mode][n]);
}

/* Return a MEM corresponding to a stack slot with mode MODE.
   Allocate a new slot if necessary.

   The RTL for a function can have several slots available: N is
   which slot to use.  */

rtx
assign_386_stack_local (mode, n)
     enum machine_mode mode;
     int n;
{
  if (n < 0 || n >= MAX_386_STACK_LOCALS)
    abort ();

  if (ix86_stack_locals[(int) mode][n] == NULL_RTX)
    ix86_stack_locals[(int) mode][n]
      = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);

  return ix86_stack_locals[(int) mode][n];
}

/* Calculate the length of the memory address in the instruction
   encoding.  Does not include the one-byte modrm, opcode, or prefix.  */

static int
memory_address_length (addr)
     rtx addr;
{
  struct ix86_address parts;
  rtx base, index, disp;
  int len;

  if (GET_CODE (addr) == PRE_DEC
      || GET_CODE (addr) == POST_INC)
    return 0;

  if (! ix86_decompose_address (addr, &parts))
    abort ();

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  len = 0;

  /* Register Indirect.  */
  if (base && !index && !disp)
    {
      /* Special cases: ebp and esp need the two-byte modrm form.  */
      if (addr == stack_pointer_rtx
	  || addr == arg_pointer_rtx
	  || addr == frame_pointer_rtx
	  || addr == hard_frame_pointer_rtx)
	len = 1;
    }

  /* Direct Addressing.  */
  else if (disp && !base && !index)
    len = 4;

  else
    {
      /* Find the length of the displacement constant.  */
      if (disp)
	{
	  if (GET_CODE (disp) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (disp), 'K'))
	    len = 1;
	  else
	    len = 4;
	}

      /* An index requires the two-byte modrm form.  */
      if (index)
	len += 1;
    }

  return len;
}

int
ix86_attr_length_default (insn)
     rtx insn;
{
  enum attr_type type;
  int len = 0, i;

  type = get_attr_type (insn);
  extract_insn (insn);
  switch (type)
    {
    case TYPE_INCDEC:
    case TYPE_SETCC:
    case TYPE_ICMOV:
    case TYPE_FMOV:
    case TYPE_FOP:
    case TYPE_FCMP:
    case TYPE_FOP1:
    case TYPE_FMUL:
    case TYPE_FDIV:
    case TYPE_FSGN:
    case TYPE_FPSPC:
    case TYPE_FCMOV:
    case TYPE_IBR:
      break;
    case TYPE_STR:
    case TYPE_CLD:
      len = 0;

    case TYPE_ALU1:
    case TYPE_NEGNOT:
    case TYPE_ALU:
    case TYPE_ICMP:
    case TYPE_IMOVX:
    case TYPE_ISHIFT:
    case TYPE_IMUL:
    case TYPE_IDIV:
    case TYPE_PUSH:
    case TYPE_POP:
      for (i = recog_data.n_operands - 1; i >= 0; --i)
        if (CONSTANT_P (recog_data.operand[i]))
	  {
	    if (GET_CODE (recog_data.operand[i]) == CONST_INT
		&& CONST_OK_FOR_LETTER_P (INTVAL (recog_data.operand[i]), 'K'))
	      len += 1;
	    else
	      len += GET_MODE_SIZE (GET_MODE (recog_data.operand[0]));
	  }
      break;

    case TYPE_IMOV:
      if (CONSTANT_P (recog_data.operand[1]))
        len += GET_MODE_SIZE (GET_MODE (recog_data.operand[0]));
      break;

    case TYPE_CALL:
      if (constant_call_address_operand (recog_data.operand[0],
					 GET_MODE (recog_data.operand[0])))
	return 5;
      break;

    case TYPE_CALLV:
      if (constant_call_address_operand (recog_data.operand[1],
					 GET_MODE (recog_data.operand[1])))
	return 5;
      break;

    case TYPE_LEA:
      {
        /* Irritatingly, single_set doesn't work with REG_UNUSED present,
	   as we'll get from running life_analysis during reg-stack when
	   not optimizing.  */
        rtx set = PATTERN (insn);
        if (GET_CODE (set) == SET)
	  ;
	else if (GET_CODE (set) == PARALLEL
		 && XVECLEN (set, 0) == 2
		 && GET_CODE (XVECEXP (set, 0, 0)) == SET
		 && GET_CODE (XVECEXP (set, 0, 1)) == CLOBBER)
	  set = XVECEXP (set, 0, 0);
	else
	  abort ();

	len += memory_address_length (SET_SRC (set));
	goto just_opcode;
      }

    case TYPE_OTHER: 
    case TYPE_MULTI:
      return 15;

    case TYPE_FXCH:
      if (STACK_TOP_P (recog_data.operand[0]))
	return 2 + (REGNO (recog_data.operand[1]) != FIRST_STACK_REG + 1);
      else
	return 2 + (REGNO (recog_data.operand[0]) != FIRST_STACK_REG + 1);

    default:
      abort ();
    }

  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (GET_CODE (recog_data.operand[i]) == MEM)
      {
	len += memory_address_length (XEXP (recog_data.operand[i], 0));
	break;
      }

just_opcode:
  len += get_attr_length_opcode (insn);
  len += get_attr_length_prefix (insn);

  return len;
}

/* Return the maximum number of instructions a cpu can issue.  */

int
ix86_issue_rate ()
{
  switch (ix86_cpu)
    {
    case PROCESSOR_PENTIUM:
    case PROCESSOR_K6:
      return 2;

    case PROCESSOR_PENTIUMPRO:
      return 3;

    default:
      return 1;
    }
}

/* A subroutine of ix86_adjust_cost -- return true iff INSN reads flags set
   by DEP_INSN and nothing set by DEP_INSN.  */

static int
ix86_flags_dependant (insn, dep_insn, insn_type)
     rtx insn, dep_insn;
     enum attr_type insn_type;
{
  rtx set, set2;

  /* Simplify the test for uninteresting insns.  */
  if (insn_type != TYPE_SETCC
      && insn_type != TYPE_ICMOV
      && insn_type != TYPE_FCMOV
      && insn_type != TYPE_IBR)
    return 0;

  if ((set = single_set (dep_insn)) != 0)
    {
      set = SET_DEST (set);
      set2 = NULL_RTX;
    }
  else if (GET_CODE (PATTERN (dep_insn)) == PARALLEL
	   && XVECLEN (PATTERN (dep_insn), 0) == 2
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 1)) == SET)
    {
      set = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
      set2 = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
    }
  else
    return 0;

  if (GET_CODE (set) != REG || REGNO (set) != FLAGS_REG)
    return 0;

  /* This test is true if the dependant insn reads the flags but
     not any other potentially set register.  */
  if (!reg_overlap_mentioned_p (set, PATTERN (insn)))
    return 0;

  if (set2 && reg_overlap_mentioned_p (set2, PATTERN (insn)))
    return 0;

  return 1;
}

/* A subroutine of ix86_adjust_cost -- return true iff INSN has a memory
   address with operands set by DEP_INSN.  */

static int
ix86_agi_dependant (insn, dep_insn, insn_type)
     rtx insn, dep_insn;
     enum attr_type insn_type;
{
  rtx addr;

  if (insn_type == TYPE_LEA)
    addr = SET_SRC (single_set (insn));
  else
    {
      int i;
      extract_insn (insn);
      for (i = recog_data.n_operands - 1; i >= 0; --i)
	if (GET_CODE (recog_data.operand[i]) == MEM)
	  {
	    addr = XEXP (recog_data.operand[i], 0);
	    goto found;
	  }
      return 0;
    found:;
    }

  return modified_in_p (addr, dep_insn);
}

int
ix86_adjust_cost (insn, link, dep_insn, cost)
     rtx insn, link, dep_insn;
     int cost;
{
  enum attr_type insn_type, dep_insn_type;
  rtx set, set2;
  int dep_insn_code_number;

  /* Anti and output depenancies have zero cost on all CPUs.  */
  if (REG_NOTE_KIND (link) != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  /* Prologue and epilogue allocators have false dependency on ebp.
     This results in one cycle extra stall on Pentium prologue scheduling, so
     handle this important case manually. */

  if ((dep_insn_code_number == CODE_FOR_prologue_allocate_stack
       || dep_insn_code_number == CODE_FOR_epilogue_deallocate_stack)
      && !reg_mentioned_p (stack_pointer_rtx, insn))
    return 0;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  switch (ix86_cpu)
    {
    case PROCESSOR_PENTIUM:
      /* Address Generation Interlock adds a cycle of latency.  */
      if (ix86_agi_dependant (insn, dep_insn, insn_type))
	cost += 1;

      /* ??? Compares pair with jump/setcc.  */
      if (ix86_flags_dependant (insn, dep_insn, insn_type))
	cost = 0;

      /* Floating point stores require value to be ready one cycle ealier.  */
      if (insn_type == TYPE_FMOV 
	  && get_attr_memory (insn) == MEMORY_STORE
	  && !ix86_agi_dependant (insn, dep_insn, insn_type))
	cost += 1;
      break;

    case PROCESSOR_PENTIUMPRO:
      /* Since we can't represent delayed latencies of load+operation, 
	 increase the cost here for non-imov insns.  */
      if (dep_insn_type != TYPE_IMOV
	  && dep_insn_type != TYPE_FMOV
	  && get_attr_memory (dep_insn) == MEMORY_LOAD)
	cost += 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* There is one cycle extra latency between an FP op and a store.  */
      if (insn_type == TYPE_FMOV
	  && (set = single_set (dep_insn)) != NULL_RTX
	  && (set2 = single_set (insn)) != NULL_RTX
	  && rtx_equal_p (SET_DEST (set), SET_SRC (set2))
	  && GET_CODE (SET_DEST (set2)) == MEM)
	cost += 1;
      break;

    case PROCESSOR_K6:
      /* The esp dependency is resolved before the instruction is really
         finished.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 1;

      /* Since we can't represent delayed latencies of load+operation, 
	 increase the cost here for non-imov insns.  */
      if (get_attr_memory (dep_insn) == MEMORY_LOAD)
	cost += (dep_insn_type != TYPE_IMOV) ? 2 : 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;
      break;

    case PROCESSOR_ATHLON:
      /* Address Generation Interlock cause problems on the Athlon CPU because
         the loads and stores are done in order so once one load or store has
	 to wait, others must too, so penalize the AGIs slightly by one cycle.
	 We might experiment with this value later.  */
      if (ix86_agi_dependant (insn, dep_insn, insn_type))
	cost += 1;

      /* Since we can't represent delayed latencies of load+operation, 
	 increase the cost here for non-imov insns.  */
      if (dep_insn_type != TYPE_IMOV
	  && dep_insn_type != TYPE_FMOV
	  && get_attr_memory (dep_insn) == MEMORY_LOAD)
	cost += 2;
    default:
      break;
    }

  return cost;
}

static union
{
  struct ppro_sched_data
  {
    rtx decode[3];
    int issued_this_cycle;
  } ppro;
} ix86_sched_data;

static int
ix86_safe_length (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_length(insn);
  else
    return 128;
}

static int
ix86_safe_length_prefix (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_length(insn);
  else
    return 0;
}

static enum attr_memory
ix86_safe_memory (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_memory(insn);
  else
    return MEMORY_UNKNOWN;
}

static enum attr_pent_pair
ix86_safe_pent_pair (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_pent_pair(insn);
  else
    return PENT_PAIR_NP;
}

static enum attr_ppro_uops
ix86_safe_ppro_uops (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_ppro_uops (insn);
  else
    return PPRO_UOPS_MANY;
}

static void
ix86_dump_ppro_packet (dump)
     FILE *dump;
{
  if (ix86_sched_data.ppro.decode[0])
    {
      fprintf (dump, "PPRO packet: %d",
	       INSN_UID (ix86_sched_data.ppro.decode[0]));
      if (ix86_sched_data.ppro.decode[1])
	fprintf (dump, " %d", INSN_UID (ix86_sched_data.ppro.decode[1]));
      if (ix86_sched_data.ppro.decode[2])
	fprintf (dump, " %d", INSN_UID (ix86_sched_data.ppro.decode[2]));
      fputc ('\n', dump);
    }
}

/* We're beginning a new block.  Initialize data structures as necessary.  */

void
ix86_sched_init (dump, sched_verbose)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
{
  memset (&ix86_sched_data, 0, sizeof (ix86_sched_data));
}

/* Shift INSN to SLOT, and shift everything else down.  */

static void
ix86_reorder_insn (insnp, slot)
     rtx *insnp, *slot;
{
  if (insnp != slot)
    {
      rtx insn = *insnp;
      do 
	insnp[0] = insnp[1];
      while (++insnp != slot);
      *insnp = insn;
    }
}

/* Find an instruction with given pairability and minimal amount of cycles
   lost by the fact that the CPU waits for both pipelines to finish before
   reading next instructions.  Also take care that both instructions together
   can not exceed 7 bytes.  */

static rtx *
ix86_pent_find_pair (e_ready, ready, type, first)
     rtx *e_ready;
     rtx *ready;
     enum attr_pent_pair type;
     rtx first;
{
  int mincycles, cycles;
  enum attr_pent_pair tmp;
  enum attr_memory memory;
  rtx *insnp, *bestinsnp = NULL;

  if (ix86_safe_length (first) > 7 + ix86_safe_length_prefix (first))
    return NULL;

  memory = ix86_safe_memory (first);
  cycles = result_ready_cost (first);
  mincycles = INT_MAX;

  for (insnp = e_ready; insnp >= ready && mincycles; --insnp)
    if ((tmp = ix86_safe_pent_pair (*insnp)) == type
	&& ix86_safe_length (*insnp) <= 7 + ix86_safe_length_prefix (*insnp))
      {
	enum attr_memory second_memory;
	int secondcycles, currentcycles;

	second_memory = ix86_safe_memory (*insnp);
	secondcycles = result_ready_cost (*insnp);
	currentcycles = abs (cycles - secondcycles);

	if (secondcycles >= 1 && cycles >= 1)
	  {
	    /* Two read/modify/write instructions together takes two
	       cycles longer.  */
	    if (memory == MEMORY_BOTH && second_memory == MEMORY_BOTH)
	      currentcycles += 2;
  
	    /* Read modify/write instruction followed by read/modify
	       takes one cycle longer.  */
	    if (memory == MEMORY_BOTH && second_memory == MEMORY_LOAD
	        && tmp != PENT_PAIR_UV
	        && ix86_safe_pent_pair (first) != PENT_PAIR_UV)
	      currentcycles += 1;
	  }
	if (currentcycles < mincycles)
	  bestinsnp = insnp, mincycles = currentcycles;
      }

  return bestinsnp;
}

/* Subroutines of ix86_sched_reorder.  */

void
ix86_sched_reorder_pentium (ready, e_ready)
     rtx *ready;
     rtx *e_ready;
{
  enum attr_pent_pair pair1, pair2;
  rtx *insnp;

  /* This wouldn't be necessary if Haifa knew that static insn ordering
     is important to which pipe an insn is issued to.  So we have to make
     some minor rearrangements.  */

  pair1 = ix86_safe_pent_pair (*e_ready);

  /* If the first insn is non-pairable, let it be.  */
  if (pair1 == PENT_PAIR_NP)
    return;

  pair2 = PENT_PAIR_NP;
  insnp = 0;

  /* If the first insn is UV or PV pairable, search for a PU
     insn to go with.  */
  if (pair1 == PENT_PAIR_UV || pair1 == PENT_PAIR_PV)
    {
      insnp = ix86_pent_find_pair (e_ready-1, ready,
				   PENT_PAIR_PU, *e_ready);
      if (insnp)
	pair2 = PENT_PAIR_PU;
    }

  /* If the first insn is PU or UV pairable, search for a PV
     insn to go with.  */
  if (pair2 == PENT_PAIR_NP
      && (pair1 == PENT_PAIR_PU || pair1 == PENT_PAIR_UV))
    {
      insnp = ix86_pent_find_pair (e_ready-1, ready,
				   PENT_PAIR_PV, *e_ready);
      if (insnp)
	pair2 = PENT_PAIR_PV;
    }

  /* If the first insn is pairable, search for a UV
     insn to go with.  */
  if (pair2 == PENT_PAIR_NP)
    {
      insnp = ix86_pent_find_pair (e_ready-1, ready,
				   PENT_PAIR_UV, *e_ready);
      if (insnp)
	pair2 = PENT_PAIR_UV;
    }

  if (pair2 == PENT_PAIR_NP)
    return;

  /* Found something!  Decide if we need to swap the order.  */
  if (pair1 == PENT_PAIR_PV || pair2 == PENT_PAIR_PU
      || (pair1 == PENT_PAIR_UV && pair2 == PENT_PAIR_UV
	  && ix86_safe_memory (*e_ready) == MEMORY_BOTH
	  && ix86_safe_memory (*insnp) == MEMORY_LOAD))
    ix86_reorder_insn (insnp, e_ready);
  else
    ix86_reorder_insn (insnp, e_ready - 1);
}

void
ix86_sched_reorder_ppro (ready, e_ready)
     rtx *ready;
     rtx *e_ready;
{
  rtx decode[3];
  enum attr_ppro_uops cur_uops;
  int issued_this_cycle;
  rtx *insnp;
  int i;

  /* At this point .ppro.decode contains the state of the three 
     decoders from last "cycle".  That is, those insns that were
     actually independent.  But here we're scheduling for the 
     decoder, and we may find things that are decodable in the
     same cycle.  */

  memcpy (decode, ix86_sched_data.ppro.decode, sizeof(decode));
  issued_this_cycle = 0;

  insnp = e_ready;
  cur_uops = ix86_safe_ppro_uops (*insnp);

  /* If the decoders are empty, and we've a complex insn at the
     head of the priority queue, let it issue without complaint.  */
  if (decode[0] == NULL)
    {
      if (cur_uops == PPRO_UOPS_MANY)
	{
	  decode[0] = *insnp;
	  goto ppro_done;
	}

      /* Otherwise, search for a 2-4 uop unsn to issue.  */
      while (cur_uops != PPRO_UOPS_FEW)
	{
	  if (insnp == ready)
	    break;
	  cur_uops = ix86_safe_ppro_uops (*--insnp);
	}

      /* If so, move it to the head of the line.  */
      if (cur_uops == PPRO_UOPS_FEW)
	ix86_reorder_insn (insnp, e_ready);

      /* Issue the head of the queue.  */
      issued_this_cycle = 1;
      decode[0] = *e_ready--;
    }

  /* Look for simple insns to fill in the other two slots.  */
  for (i = 1; i < 3; ++i)
    if (decode[i] == NULL)
      {
	if (ready >= e_ready)
	  goto ppro_done;

	insnp = e_ready;
	cur_uops = ix86_safe_ppro_uops (*insnp);
	while (cur_uops != PPRO_UOPS_ONE)
	  {
	    if (insnp == ready)
	      break;
	    cur_uops = ix86_safe_ppro_uops (*--insnp);
	  }

	/* Found one.  Move it to the head of the queue and issue it.  */
	if (cur_uops == PPRO_UOPS_ONE)
	  {
	    ix86_reorder_insn (insnp, e_ready);
	    decode[i] = *e_ready--;
	    issued_this_cycle++;
	    continue;
	  }

	/* ??? Didn't find one.  Ideally, here we would do a lazy split
	   of 2-uop insns, issue one and queue the other.  */
      }

 ppro_done:
  if (issued_this_cycle == 0)
    issued_this_cycle = 1;
  ix86_sched_data.ppro.issued_this_cycle = issued_this_cycle;
}

  
/* We are about to being issuing insns for this clock cycle.  
   Override the default sort algorithm to better slot instructions.  */
int
ix86_sched_reorder (dump, sched_verbose, ready, n_ready, clock_var)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     rtx *ready;
     int n_ready;
     int clock_var ATTRIBUTE_UNUSED;
{
  rtx *e_ready = ready + n_ready - 1;

  if (n_ready < 2)
    goto out;

  switch (ix86_cpu)
    {
    default:
      break;

    case PROCESSOR_PENTIUM:
      ix86_sched_reorder_pentium (ready, e_ready);
      break;

    case PROCESSOR_PENTIUMPRO:
      ix86_sched_reorder_ppro (ready, e_ready);
      break;
    }

out:
  return ix86_issue_rate ();
}

/* We are about to issue INSN.  Return the number of insns left on the
   ready queue that can be issued this cycle.  */

int
ix86_variable_issue (dump, sched_verbose, insn, can_issue_more)
     FILE *dump;
     int sched_verbose;
     rtx insn;
     int can_issue_more;
{
  int i;
  switch (ix86_cpu)
    {
    default:
      return can_issue_more - 1;

    case PROCESSOR_PENTIUMPRO:
      {
	enum attr_ppro_uops uops = ix86_safe_ppro_uops (insn);

	if (uops == PPRO_UOPS_MANY)
	  {
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = insn;
	    ix86_sched_data.ppro.decode[1] = NULL;
	    ix86_sched_data.ppro.decode[2] = NULL;
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = NULL;
	  }
	else if (uops == PPRO_UOPS_FEW)
	  {
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = insn;
	    ix86_sched_data.ppro.decode[1] = NULL;
	    ix86_sched_data.ppro.decode[2] = NULL;
	  }
	else
	  {
	    for (i = 0; i < 3; ++i)
	      if (ix86_sched_data.ppro.decode[i] == NULL)
		{
		  ix86_sched_data.ppro.decode[i] = insn;
		  break;
		}
	    if (i == 3)
	      abort ();
	    if (i == 2)
	      {
	        if (sched_verbose)
	          ix86_dump_ppro_packet (dump);
		ix86_sched_data.ppro.decode[0] = NULL;
		ix86_sched_data.ppro.decode[1] = NULL;
		ix86_sched_data.ppro.decode[2] = NULL;
	      }
	  }
      }
      return --ix86_sched_data.ppro.issued_this_cycle;
    }
}
