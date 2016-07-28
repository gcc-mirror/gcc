/* Subroutines for insn-output.c for Matsushita MN10300 series
   Copyright (C) 1996-2016 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "output.h"
#include "insn-attr.h"
#include "reload.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "cfgrtl.h"
#include "dumpfile.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* This is used in the am33_2.0-linux-gnu port, in which global symbol
   names are not prefixed by underscores, to tell whether to prefix a
   label with a plus sign or not, so that the assembler can tell
   symbol names from register names.  */
int mn10300_protect_label;

/* Selected processor type for tuning.  */
enum processor_type mn10300_tune_cpu = PROCESSOR_DEFAULT;

#define CC_FLAG_Z	1
#define CC_FLAG_N	2
#define CC_FLAG_C	4
#define CC_FLAG_V	8

static int cc_flags_for_mode(machine_mode);
static int cc_flags_for_code(enum rtx_code);

/* Implement TARGET_OPTION_OVERRIDE.  */
static void
mn10300_option_override (void)
{
  if (TARGET_AM33)
    target_flags &= ~MASK_MULT_BUG;
  else
    {
      /* Disable scheduling for the MN10300 as we do
	 not have timing information available for it.  */
      flag_schedule_insns = 0;
      flag_schedule_insns_after_reload = 0;

      /* Force enable splitting of wide types, as otherwise it is trivial
	 to run out of registers.  Indeed, this works so well that register
	 allocation problems are now more common *without* optimization,
	 when this flag is not enabled by default.  */
      flag_split_wide_types = 1;
    }

  if (mn10300_tune_string)
    {
      if (strcasecmp (mn10300_tune_string, "mn10300") == 0)
	mn10300_tune_cpu = PROCESSOR_MN10300;
      else if (strcasecmp (mn10300_tune_string, "am33") == 0)
	mn10300_tune_cpu = PROCESSOR_AM33;
      else if (strcasecmp (mn10300_tune_string, "am33-2") == 0)
	mn10300_tune_cpu = PROCESSOR_AM33_2;
      else if (strcasecmp (mn10300_tune_string, "am34") == 0)
	mn10300_tune_cpu = PROCESSOR_AM34;
      else
	error ("-mtune= expects mn10300, am33, am33-2, or am34");
    }
}

static void
mn10300_file_start (void)
{
  default_file_start ();

  if (TARGET_AM33_2)
    fprintf (asm_out_file, "\t.am33_2\n");
  else if (TARGET_AM33)
    fprintf (asm_out_file, "\t.am33\n");
}

/* Note: This list must match the liw_op attribute in mn10300.md.  */

static const char *liw_op_names[] =
{
  "add", "cmp", "sub", "mov",
  "and", "or", "xor",
  "asr", "lsr", "asl",
  "none", "max"
};

/* Print operand X using operand code CODE to assembly language output file
   FILE.  */

void
mn10300_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'W':
      {
	unsigned int liw_op = UINTVAL (x);

	gcc_assert (TARGET_ALLOW_LIW);
	gcc_assert (liw_op < LIW_OP_MAX);
	fputs (liw_op_names[liw_op], file);
	break;
      }

    case 'b':
    case 'B':
      {
	enum rtx_code cmp = GET_CODE (x);
	machine_mode mode = GET_MODE (XEXP (x, 0));
	const char *str;
	int have_flags;

	if (code == 'B')
	  cmp = reverse_condition (cmp);
	have_flags = cc_flags_for_mode (mode);

	switch (cmp)
	  {
	  case NE:
	    str = "ne";
	    break;
	  case EQ:
	    str = "eq";
	    break;
	  case GE:
	    /* bge is smaller than bnc.  */
	    str = (have_flags & CC_FLAG_V ? "ge" : "nc");
	    break;
	  case LT:
	    str = (have_flags & CC_FLAG_V ? "lt" : "ns");
	    break;
	  case GT:
	    str = "gt";
	    break;
	  case LE:
	    str = "le";
	    break;
	  case GEU:
	    str = "cc";
	    break;
	  case GTU:
	    str = "hi";
	    break;
	  case LEU:
	    str = "ls";
	    break;
	  case LTU:
	    str = "cs";
	    break;
	  case ORDERED:
	    str = "lge";
	    break;
	  case UNORDERED:
	    str = "uo";
	    break;
	  case LTGT:
	    str = "lg";
	    break;
	  case UNEQ:
	    str = "ue";
	    break;
	  case UNGE:
	    str = "uge";
	    break;
	  case UNGT:
	    str = "ug";
	    break;
	  case UNLE:
	    str = "ule";
	    break;
	  case UNLT:
	    str = "ul";
	    break;
	  default:
	    gcc_unreachable ();
	  }

	gcc_checking_assert ((cc_flags_for_code (cmp) & ~have_flags) == 0);
	fputs (str, file);
      }
      break;

    case 'C':
      /* This is used for the operand to a call instruction;
	 if it's a REG, enclose it in parens, else output
	 the operand normally.  */
      if (REG_P (x))
	{
	  fputc ('(', file);
	  mn10300_print_operand (file, x, 0);
	  fputc (')', file);
	}
      else
	mn10300_print_operand (file, x, 0);
      break;

    case 'D':
      switch (GET_CODE (x))
	{
	case MEM:
	  fputc ('(', file);
	  output_address (GET_MODE (x), XEXP (x, 0));
	  fputc (')', file);
	  break;

	case REG:
	  fprintf (file, "fd%d", REGNO (x) - 18);
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

      /* These are the least significant word in a 64bit value.  */
    case 'L':
      switch (GET_CODE (x))
	{
	case MEM:
	  fputc ('(', file);
	  output_address (GET_MODE (x), XEXP (x, 0));
	  fputc (')', file);
	  break;

	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	case SUBREG:
	  fprintf (file, "%s", reg_names[subreg_regno (x)]);
	  break;

	case CONST_DOUBLE:
	  {
	    long val[2];

	    switch (GET_MODE (x))
	      {
	      case DFmode:
		REAL_VALUE_TO_TARGET_DOUBLE
		  (*CONST_DOUBLE_REAL_VALUE (x), val);
		fprintf (file, "0x%lx", val[0]);
		break;;
	      case SFmode:
		REAL_VALUE_TO_TARGET_SINGLE
		  (*CONST_DOUBLE_REAL_VALUE (x), val[0]);
		fprintf (file, "0x%lx", val[0]);
		break;;
	      case VOIDmode:
	      case DImode:
		mn10300_print_operand_address (file,
					       GEN_INT (CONST_DOUBLE_LOW (x)));
		break;
	      default:
		break;
	      }
	    break;
	  }

	case CONST_INT:
	  {
	    rtx low, high;
	    split_double (x, &low, &high);
	    fprintf (file, "%ld", (long)INTVAL (low));
	    break;
	    }

	default:
	  gcc_unreachable ();
	}
      break;

      /* Similarly, but for the most significant word.  */
    case 'H':
      switch (GET_CODE (x))
	{
	case MEM:
	  fputc ('(', file);
	  x = adjust_address (x, SImode, 4);
	  output_address (GET_MODE (x), XEXP (x, 0));
	  fputc (')', file);
	  break;

	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x) + 1]);
	  break;

	case SUBREG:
	  fprintf (file, "%s", reg_names[subreg_regno (x) + 1]);
	  break;

	case CONST_DOUBLE:
	  {
	    long val[2];

	    switch (GET_MODE (x))
	      {
	      case DFmode:
		REAL_VALUE_TO_TARGET_DOUBLE
		  (*CONST_DOUBLE_REAL_VALUE (x), val);
		fprintf (file, "0x%lx", val[1]);
		break;;
	      case SFmode:
		gcc_unreachable ();
	      case VOIDmode:
	      case DImode:
		mn10300_print_operand_address (file,
					       GEN_INT (CONST_DOUBLE_HIGH (x)));
		break;
	      default:
		break;
	      }
	    break;
	  }

	case CONST_INT:
	  {
	    rtx low, high;
	    split_double (x, &low, &high);
	    fprintf (file, "%ld", (long)INTVAL (high));
	    break;
	  }

	default:
	  gcc_unreachable ();
	}
      break;

    case 'A':
      fputc ('(', file);
      if (REG_P (XEXP (x, 0)))
	output_address (VOIDmode, gen_rtx_PLUS (SImode,
						XEXP (x, 0), const0_rtx));
      else
	output_address (VOIDmode, XEXP (x, 0));
      fputc (')', file);
      break;

    case 'N':
      gcc_assert (INTVAL (x) >= -128 && INTVAL (x) <= 255);
      fprintf (file, "%d", (int)((~INTVAL (x)) & 0xff));
      break;

    case 'U':
      gcc_assert (INTVAL (x) >= -128 && INTVAL (x) <= 255);
      fprintf (file, "%d", (int)(INTVAL (x) & 0xff));
      break;

      /* For shift counts.  The hardware ignores the upper bits of
	 any immediate, but the assembler will flag an out of range
	 shift count as an error.  So we mask off the high bits
	 of the immediate here.  */
    case 'S':
      if (CONST_INT_P (x))
	{
	  fprintf (file, "%d", (int)(INTVAL (x) & 0x1f));
	  break;
	}
      /* FALL THROUGH */

    default:
      switch (GET_CODE (x))
	{
	case MEM:
	  fputc ('(', file);
	  output_address (GET_MODE (x), XEXP (x, 0));
	  fputc (')', file);
	  break;

	case PLUS:
	  output_address (VOIDmode, x);
	  break;

	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	case SUBREG:
	  fprintf (file, "%s", reg_names[subreg_regno (x)]);
	  break;

	  /* This will only be single precision....  */
	case CONST_DOUBLE:
	  {
	    unsigned long val;

	    REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), val);
	    fprintf (file, "0x%lx", val);
	    break;
	  }

	case CONST_INT:
	case SYMBOL_REF:
	case CONST:
	case LABEL_REF:
	case CODE_LABEL:
	case UNSPEC:
	  mn10300_print_operand_address (file, x);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    }
}

/* Output assembly language output for the address ADDR to FILE.  */

void
mn10300_print_operand_address (FILE *file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case POST_INC:
      mn10300_print_operand (file, XEXP (addr, 0), 0);
      fputc ('+', file);
      break;

    case POST_MODIFY:
      mn10300_print_operand (file, XEXP (addr, 0), 0);
      fputc ('+', file);
      fputc (',', file);
      mn10300_print_operand (file, XEXP (addr, 1), 0);
      break;

    case REG:
      mn10300_print_operand (file, addr, 0);
      break;
    case PLUS:
      {
	rtx base = XEXP (addr, 0);
	rtx index = XEXP (addr, 1);
	
	if (REG_P (index) && !REG_OK_FOR_INDEX_P (index))
	  {
	    rtx x = base;
	    base = index;
	    index = x;

	    gcc_assert (REG_P (index) && REG_OK_FOR_INDEX_P (index));
	  }
	gcc_assert (REG_OK_FOR_BASE_P (base));

	mn10300_print_operand (file, index, 0);
	fputc (',', file);
	mn10300_print_operand (file, base, 0);
	break;
      }
    case SYMBOL_REF:
      output_addr_const (file, addr);
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.

   Used for PIC-specific UNSPECs.  */

static bool
mn10300_asm_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_PIC:
	  /* GLOBAL_OFFSET_TABLE or local symbols, no suffix.  */
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  break;
	case UNSPEC_GOT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_GOTOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_PLT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@PLT", file);
	  break;
	case UNSPEC_GOTSYM_OFF:
	  assemble_name (file, GOT_SYMBOL_NAME);
	  fputs ("-(", file);
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("-.)", file);
	  break;
	default:
	  return false;
	}
      return true;
    }
  else
    return false;
}

/* Count the number of FP registers that have to be saved.  */
static int
fp_regs_to_save (void)
{
  int i, n = 0;

  if (! TARGET_AM33_2)
    return 0;

  for (i = FIRST_FP_REGNUM; i <= LAST_FP_REGNUM; ++i)
    if (df_regs_ever_live_p (i) && ! call_really_used_regs[i])
      ++n;

  return n;
}

/* Print a set of registers in the format required by "movm" and "ret".
   Register K is saved if bit K of MASK is set.  The data and address
   registers can be stored individually, but the extended registers cannot.
   We assume that the mask already takes that into account.  For instance,
   bits 14 to 17 must have the same value.  */

void
mn10300_print_reg_list (FILE *file, int mask)
{
  int need_comma;
  int i;

  need_comma = 0;
  fputc ('[', file);

  for (i = 0; i < FIRST_EXTENDED_REGNUM; i++)
    if ((mask & (1 << i)) != 0)
      {
	if (need_comma)
	  fputc (',', file);
	fputs (reg_names [i], file);
	need_comma = 1;
      }

  if ((mask & 0x3c000) != 0)
    {
      gcc_assert ((mask & 0x3c000) == 0x3c000);
      if (need_comma)
	fputc (',', file);
      fputs ("exreg1", file);
      need_comma = 1;
    }

  fputc (']', file);
}

/* If the MDR register is never clobbered, we can use the RETF instruction
   which takes the address from the MDR register.  This is 3 cycles faster
   than having to load the address from the stack.  */

bool
mn10300_can_use_retf_insn (void)
{
  /* Don't bother if we're not optimizing.  In this case we won't
     have proper access to df_regs_ever_live_p.  */
  if (!optimize)
    return false;

  /* EH returns alter the saved return address; MDR is not current.  */
  if (crtl->calls_eh_return)
    return false;

  /* Obviously not if MDR is ever clobbered.  */
  if (df_regs_ever_live_p (MDR_REG))
    return false;

  /* ??? Careful not to use this during expand_epilogue etc.  */
  gcc_assert (!in_sequence_p ());
  return leaf_function_p ();
}

bool
mn10300_can_use_rets_insn (void)
{
  return !mn10300_initial_offset (ARG_POINTER_REGNUM, STACK_POINTER_REGNUM);
}

/* Returns the set of live, callee-saved registers as a bitmask.  The
   callee-saved extended registers cannot be stored individually, so
   all of them will be included in the mask if any one of them is used.
   Also returns the number of bytes in the registers in the mask if
   BYTES_SAVED is not NULL.  */

unsigned int
mn10300_get_live_callee_saved_regs (unsigned int * bytes_saved)
{
  int mask;
  int i;
  unsigned int count;

  count = mask = 0;
  for (i = 0; i <= LAST_EXTENDED_REGNUM; i++)
    if (df_regs_ever_live_p (i) && ! call_really_used_regs[i])
      {
	mask |= (1 << i);
	++ count;
      }

  if ((mask & 0x3c000) != 0)
    {
      for (i = 0x04000; i < 0x40000; i <<= 1)
	if ((mask & i) == 0)
	  ++ count;
      
      mask |= 0x3c000;
    }

  if (bytes_saved)
    * bytes_saved = count * UNITS_PER_WORD;

  return mask;
}

static rtx
F (rtx r)
{
  RTX_FRAME_RELATED_P (r) = 1;
  return r;
}

/* Generate an instruction that pushes several registers onto the stack.
   Register K will be saved if bit K in MASK is set.  The function does
   nothing if MASK is zero.

   To be compatible with the "movm" instruction, the lowest-numbered
   register must be stored in the lowest slot.  If MASK is the set
   { R1,...,RN }, where R1...RN are ordered least first, the generated
   instruction will have the form:

       (parallel
         (set (reg:SI 9) (plus:SI (reg:SI 9) (const_int -N*4)))
	 (set (mem:SI (plus:SI (reg:SI 9)
	                       (const_int -1*4)))
	      (reg:SI RN))
	 ...
	 (set (mem:SI (plus:SI (reg:SI 9)
	                       (const_int -N*4)))
	      (reg:SI R1))) */

static void
mn10300_gen_multiple_store (unsigned int mask)
{
  /* The order in which registers are stored, from SP-4 through SP-N*4.  */
  static const unsigned int store_order[8] = {
    /* e2, e3: never saved */
    FIRST_EXTENDED_REGNUM + 4,
    FIRST_EXTENDED_REGNUM + 5,
    FIRST_EXTENDED_REGNUM + 6,
    FIRST_EXTENDED_REGNUM + 7,
    /* e0, e1, mdrq, mcrh, mcrl, mcvf: never saved. */
    FIRST_DATA_REGNUM + 2,
    FIRST_DATA_REGNUM + 3,
    FIRST_ADDRESS_REGNUM + 2,
    FIRST_ADDRESS_REGNUM + 3,
    /* d0, d1, a0, a1, mdr, lir, lar: never saved.  */
  };

  rtx x, elts[9];
  unsigned int i;
  int count;

  if (mask == 0)
    return;

  for (i = count = 0; i < ARRAY_SIZE(store_order); ++i)
    {
      unsigned regno = store_order[i];

      if (((mask >> regno) & 1) == 0)
	continue;

      ++count;
      x = plus_constant (Pmode, stack_pointer_rtx, count * -4);
      x = gen_frame_mem (SImode, x);
      x = gen_rtx_SET (x, gen_rtx_REG (SImode, regno));
      elts[count] = F(x);

      /* Remove the register from the mask so that... */
      mask &= ~(1u << regno);
    }

  /* ... we can make sure that we didn't try to use a register
     not listed in the store order.  */
  gcc_assert (mask == 0);

  /* Create the instruction that updates the stack pointer.  */
  x = plus_constant (Pmode, stack_pointer_rtx, count * -4);
  x = gen_rtx_SET (stack_pointer_rtx, x);
  elts[0] = F(x);

  /* We need one PARALLEL element to update the stack pointer and
     an additional element for each register that is stored.  */
  x = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (count + 1, elts));
  F (emit_insn (x));
}

static inline unsigned int
popcount (unsigned int mask)
{
  unsigned int count = 0;
  
  while (mask)
    {
      ++ count;
      mask &= ~ (mask & - mask);
    }
  return count;
}

void
mn10300_expand_prologue (void)
{
  HOST_WIDE_INT size = mn10300_frame_size ();
  unsigned int mask;

  mask = mn10300_get_live_callee_saved_regs (NULL);
  /* If we use any of the callee-saved registers, save them now.  */
  mn10300_gen_multiple_store (mask);

  if (flag_stack_usage_info)
    current_function_static_stack_size = size + popcount (mask) * 4;

  if (TARGET_AM33_2 && fp_regs_to_save ())
    {
      int num_regs_to_save = fp_regs_to_save (), i;
      HOST_WIDE_INT xsize;
      enum
      {
	save_sp_merge,
	save_sp_no_merge,
	save_sp_partial_merge,
	save_a0_merge,
	save_a0_no_merge
      } strategy;
      unsigned int strategy_size = (unsigned)-1, this_strategy_size;
      rtx reg;

      if (flag_stack_usage_info)
	current_function_static_stack_size += num_regs_to_save * 4;

      /* We have several different strategies to save FP registers.
	 We can store them using SP offsets, which is beneficial if
	 there are just a few registers to save, or we can use `a0' in
	 post-increment mode (`a0' is the only call-clobbered address
	 register that is never used to pass information to a
	 function).  Furthermore, if we don't need a frame pointer, we
	 can merge the two SP adds into a single one, but this isn't
	 always beneficial; sometimes we can just split the two adds
	 so that we don't exceed a 16-bit constant size.  The code
	 below will select which strategy to use, so as to generate
	 smallest code.  Ties are broken in favor or shorter sequences
	 (in terms of number of instructions).  */

#define SIZE_ADD_AX(S) ((((S) >= (1 << 15)) || ((S) < -(1 << 15))) ? 6 \
			: (((S) >= (1 << 7)) || ((S) < -(1 << 7))) ? 4 : 2)
#define SIZE_ADD_SP(S) ((((S) >= (1 << 15)) || ((S) < -(1 << 15))) ? 6 \
			: (((S) >= (1 << 7)) || ((S) < -(1 << 7))) ? 4 : 3)

/* We add 0 * (S) in two places to promote to the type of S,
   so that all arms of the conditional have the same type.  */
#define SIZE_FMOV_LIMIT(S,N,L,SIZE1,SIZE2,ELSE) \
  (((S) >= (L)) ? 0 * (S) + (SIZE1) * (N) \
   : ((S) + 4 * (N) >= (L)) ? (((L) - (S)) / 4 * (SIZE2) \
			       + ((S) + 4 * (N) - (L)) / 4 * (SIZE1)) \
   : 0 * (S) + (ELSE))
#define SIZE_FMOV_SP_(S,N) \
  (SIZE_FMOV_LIMIT ((S), (N), (1 << 24), 7, 6, \
                   SIZE_FMOV_LIMIT ((S), (N), (1 << 8), 6, 4, \
				    (S) ? 4 * (N) : 3 + 4 * ((N) - 1))))
#define SIZE_FMOV_SP(S,N) (SIZE_FMOV_SP_ ((unsigned HOST_WIDE_INT)(S), (N)))

      /* Consider alternative save_sp_merge only if we don't need the
	 frame pointer and size is nonzero.  */
      if (! frame_pointer_needed && size)
	{
	  /* Insn: add -(size + 4 * num_regs_to_save), sp.  */
	  this_strategy_size = SIZE_ADD_SP (-(size + 4 * num_regs_to_save));
	  /* Insn: fmov fs#, (##, sp), for each fs# to be saved.  */
	  this_strategy_size += SIZE_FMOV_SP (size, num_regs_to_save);

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = save_sp_merge;
	      strategy_size = this_strategy_size;
	    }
	}

      /* Consider alternative save_sp_no_merge unconditionally.  */
      /* Insn: add -4 * num_regs_to_save, sp.  */
      this_strategy_size = SIZE_ADD_SP (-4 * num_regs_to_save);
      /* Insn: fmov fs#, (##, sp), for each fs# to be saved.  */
      this_strategy_size += SIZE_FMOV_SP (0, num_regs_to_save);
      if (size)
	{
	  /* Insn: add -size, sp.  */
	  this_strategy_size += SIZE_ADD_SP (-size);
	}

      if (this_strategy_size < strategy_size)
	{
	  strategy = save_sp_no_merge;
	  strategy_size = this_strategy_size;
	}

      /* Consider alternative save_sp_partial_merge only if we don't
	 need a frame pointer and size is reasonably large.  */
      if (! frame_pointer_needed && size + 4 * num_regs_to_save > 128)
	{
	  /* Insn: add -128, sp.  */
	  this_strategy_size = SIZE_ADD_SP (-128);
	  /* Insn: fmov fs#, (##, sp), for each fs# to be saved.  */
	  this_strategy_size += SIZE_FMOV_SP (128 - 4 * num_regs_to_save,
					      num_regs_to_save);
	  if (size)
	    {
	      /* Insn: add 128-size, sp.  */
	      this_strategy_size += SIZE_ADD_SP (128 - size);
	    }

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = save_sp_partial_merge;
	      strategy_size = this_strategy_size;
	    }
	}

      /* Consider alternative save_a0_merge only if we don't need a
	 frame pointer, size is nonzero and the user hasn't
	 changed the calling conventions of a0.  */
      if (! frame_pointer_needed && size
	  && call_really_used_regs [FIRST_ADDRESS_REGNUM]
	  && ! fixed_regs[FIRST_ADDRESS_REGNUM])
	{
	  /* Insn: add -(size + 4 * num_regs_to_save), sp.  */
	  this_strategy_size = SIZE_ADD_SP (-(size + 4 * num_regs_to_save));
	  /* Insn: mov sp, a0.  */
	  this_strategy_size++;
	  if (size)
	    {
	      /* Insn: add size, a0.  */
	      this_strategy_size += SIZE_ADD_AX (size);
	    }
	  /* Insn: fmov fs#, (a0+), for each fs# to be saved.  */
	  this_strategy_size += 3 * num_regs_to_save;

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = save_a0_merge;
	      strategy_size = this_strategy_size;
	    }
	}

      /* Consider alternative save_a0_no_merge if the user hasn't
	 changed the calling conventions of a0.  */
      if (call_really_used_regs [FIRST_ADDRESS_REGNUM]
	  && ! fixed_regs[FIRST_ADDRESS_REGNUM])
	{
	  /* Insn: add -4 * num_regs_to_save, sp.  */
	  this_strategy_size = SIZE_ADD_SP (-4 * num_regs_to_save);
	  /* Insn: mov sp, a0.  */
	  this_strategy_size++;
	  /* Insn: fmov fs#, (a0+), for each fs# to be saved.  */
	  this_strategy_size += 3 * num_regs_to_save;
	  if (size)
	    {
	      /* Insn: add -size, sp.  */
	      this_strategy_size += SIZE_ADD_SP (-size);
	    }

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = save_a0_no_merge;
	      strategy_size = this_strategy_size;
	    }
	}

      /* Emit the initial SP add, common to all strategies.  */
      switch (strategy)
	{
	case save_sp_no_merge:
	case save_a0_no_merge:
	  F (emit_insn (gen_addsi3 (stack_pointer_rtx,
				    stack_pointer_rtx,
				    GEN_INT (-4 * num_regs_to_save))));
	  xsize = 0;
	  break;

	case save_sp_partial_merge:
	  F (emit_insn (gen_addsi3 (stack_pointer_rtx,
				    stack_pointer_rtx,
				    GEN_INT (-128))));
	  xsize = 128 - 4 * num_regs_to_save;
	  size -= xsize;
	  break;

	case save_sp_merge:
	case save_a0_merge:
	  F (emit_insn (gen_addsi3 (stack_pointer_rtx,
				    stack_pointer_rtx,
				    GEN_INT (-(size + 4 * num_regs_to_save)))));
	  /* We'll have to adjust FP register saves according to the
	     frame size.  */
	  xsize = size;
	  /* Since we've already created the stack frame, don't do it
	     again at the end of the function.  */
	  size = 0;
	  break;

	default:
	  gcc_unreachable ();
	}

      /* Now prepare register a0, if we have decided to use it.  */
      switch (strategy)
	{
	case save_sp_merge:
	case save_sp_no_merge:
	case save_sp_partial_merge:
	  reg = 0;
	  break;

	case save_a0_merge:
	case save_a0_no_merge:
	  reg = gen_rtx_REG (SImode, FIRST_ADDRESS_REGNUM);
	  F (emit_insn (gen_movsi (reg, stack_pointer_rtx)));
	  if (xsize)
	    F (emit_insn (gen_addsi3 (reg, reg, GEN_INT (xsize))));
	  reg = gen_rtx_POST_INC (SImode, reg);
	  break;

	default:
	  gcc_unreachable ();
	}

      /* Now actually save the FP registers.  */
      for (i = FIRST_FP_REGNUM; i <= LAST_FP_REGNUM; ++i)
	if (df_regs_ever_live_p (i) && ! call_really_used_regs [i])
	  {
	    rtx addr;

	    if (reg)
	      addr = reg;
	    else
	      {
		/* If we aren't using `a0', use an SP offset.  */
		if (xsize)
		  {
		    addr = gen_rtx_PLUS (SImode,
					 stack_pointer_rtx,
					 GEN_INT (xsize));
		  }
		else
		  addr = stack_pointer_rtx;

		xsize += 4;
	      }

	    F (emit_insn (gen_movsf (gen_rtx_MEM (SFmode, addr),
				     gen_rtx_REG (SFmode, i))));
	  }
    }

  /* Now put the frame pointer into the frame pointer register.  */
  if (frame_pointer_needed)
    F (emit_move_insn (frame_pointer_rtx, stack_pointer_rtx));

  /* Allocate stack for this frame.  */
  if (size)
    F (emit_insn (gen_addsi3 (stack_pointer_rtx,
			      stack_pointer_rtx,
			      GEN_INT (-size))));

  if (flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
    emit_insn (gen_load_pic ());
}

void
mn10300_expand_epilogue (void)
{
  HOST_WIDE_INT size = mn10300_frame_size ();
  unsigned int reg_save_bytes;

  mn10300_get_live_callee_saved_regs (& reg_save_bytes);

  if (TARGET_AM33_2 && fp_regs_to_save ())
    {
      int num_regs_to_save = fp_regs_to_save (), i;
      rtx reg = 0;

      /* We have several options to restore FP registers.  We could
	 load them from SP offsets, but, if there are enough FP
	 registers to restore, we win if we use a post-increment
	 addressing mode.  */

      /* If we have a frame pointer, it's the best option, because we
	 already know it has the value we want.  */
      if (frame_pointer_needed)
	reg = gen_rtx_REG (SImode, FRAME_POINTER_REGNUM);
      /* Otherwise, we may use `a1', since it's call-clobbered and
	 it's never used for return values.  But only do so if it's
	 smaller than using SP offsets.  */
      else
	{
	  enum { restore_sp_post_adjust,
		 restore_sp_pre_adjust,
		 restore_sp_partial_adjust,
		 restore_a1 } strategy;
	  unsigned int this_strategy_size, strategy_size = (unsigned)-1;

	  /* Consider using sp offsets before adjusting sp.  */
	  /* Insn: fmov (##,sp),fs#, for each fs# to be restored.  */
	  this_strategy_size = SIZE_FMOV_SP (size, num_regs_to_save);
	  /* If size is too large, we'll have to adjust SP with an
		 add.  */
	  if (size + 4 * num_regs_to_save + reg_save_bytes > 255)
	    {
	      /* Insn: add size + 4 * num_regs_to_save, sp.  */
	      this_strategy_size += SIZE_ADD_SP (size + 4 * num_regs_to_save);
	    }
	  /* If we don't have to restore any non-FP registers,
		 we'll be able to save one byte by using rets.  */
	  if (! reg_save_bytes)
	    this_strategy_size--;

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = restore_sp_post_adjust;
	      strategy_size = this_strategy_size;
	    }

	  /* Consider using sp offsets after adjusting sp.  */
	  /* Insn: add size, sp.  */
	  this_strategy_size = SIZE_ADD_SP (size);
	  /* Insn: fmov (##,sp),fs#, for each fs# to be restored.  */
	  this_strategy_size += SIZE_FMOV_SP (0, num_regs_to_save);
	  /* We're going to use ret to release the FP registers
		 save area, so, no savings.  */

	  if (this_strategy_size < strategy_size)
	    {
	      strategy = restore_sp_pre_adjust;
	      strategy_size = this_strategy_size;
	    }

	  /* Consider using sp offsets after partially adjusting sp.
	     When size is close to 32Kb, we may be able to adjust SP
	     with an imm16 add instruction while still using fmov
	     (d8,sp).  */
	  if (size + 4 * num_regs_to_save + reg_save_bytes > 255)
	    {
	      /* Insn: add size + 4 * num_regs_to_save
				+ reg_save_bytes - 252,sp.  */
	      this_strategy_size = SIZE_ADD_SP (size + 4 * num_regs_to_save
						+ (int) reg_save_bytes - 252);
	      /* Insn: fmov (##,sp),fs#, fo each fs# to be restored.  */
	      this_strategy_size += SIZE_FMOV_SP (252 - reg_save_bytes
						  - 4 * num_regs_to_save,
						  num_regs_to_save);
	      /* We're going to use ret to release the FP registers
		 save area, so, no savings.  */

	      if (this_strategy_size < strategy_size)
		{
		  strategy = restore_sp_partial_adjust;
		  strategy_size = this_strategy_size;
		}
	    }

	  /* Consider using a1 in post-increment mode, as long as the
	     user hasn't changed the calling conventions of a1.  */
	  if (call_really_used_regs [FIRST_ADDRESS_REGNUM + 1]
	      && ! fixed_regs[FIRST_ADDRESS_REGNUM+1])
	    {
	      /* Insn: mov sp,a1.  */
	      this_strategy_size = 1;
	      if (size)
		{
		  /* Insn: add size,a1.  */
		  this_strategy_size += SIZE_ADD_AX (size);
		}
	      /* Insn: fmov (a1+),fs#, for each fs# to be restored.  */
	      this_strategy_size += 3 * num_regs_to_save;
	      /* If size is large enough, we may be able to save a
		 couple of bytes.  */
	      if (size + 4 * num_regs_to_save + reg_save_bytes > 255)
		{
		  /* Insn: mov a1,sp.  */
		  this_strategy_size += 2;
		}
	      /* If we don't have to restore any non-FP registers,
		 we'll be able to save one byte by using rets.  */
	      if (! reg_save_bytes)
		this_strategy_size--;

	      if (this_strategy_size < strategy_size)
		{
		  strategy = restore_a1;
		  strategy_size = this_strategy_size;
		}
	    }

	  switch (strategy)
	    {
	    case restore_sp_post_adjust:
	      break;

	    case restore_sp_pre_adjust:
	      emit_insn (gen_addsi3 (stack_pointer_rtx,
				     stack_pointer_rtx,
				     GEN_INT (size)));
	      size = 0;
	      break;

	    case restore_sp_partial_adjust:
	      emit_insn (gen_addsi3 (stack_pointer_rtx,
				     stack_pointer_rtx,
				     GEN_INT (size + 4 * num_regs_to_save
					      + reg_save_bytes - 252)));
	      size = 252 - reg_save_bytes - 4 * num_regs_to_save;
	      break;

	    case restore_a1:
	      reg = gen_rtx_REG (SImode, FIRST_ADDRESS_REGNUM + 1);
	      emit_insn (gen_movsi (reg, stack_pointer_rtx));
	      if (size)
		emit_insn (gen_addsi3 (reg, reg, GEN_INT (size)));
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      /* Adjust the selected register, if any, for post-increment.  */
      if (reg)
	reg = gen_rtx_POST_INC (SImode, reg);

      for (i = FIRST_FP_REGNUM; i <= LAST_FP_REGNUM; ++i)
	if (df_regs_ever_live_p (i) && ! call_really_used_regs [i])
	  {
	    rtx addr;

	    if (reg)
	      addr = reg;
	    else if (size)
	      {
		/* If we aren't using a post-increment register, use an
		   SP offset.  */
		addr = gen_rtx_PLUS (SImode,
				     stack_pointer_rtx,
				     GEN_INT (size));
	      }
	    else
	      addr = stack_pointer_rtx;

	    size += 4;

	    emit_insn (gen_movsf (gen_rtx_REG (SFmode, i),
				  gen_rtx_MEM (SFmode, addr)));
	  }

      /* If we were using the restore_a1 strategy and the number of
	 bytes to be released won't fit in the `ret' byte, copy `a1'
	 to `sp', to avoid having to use `add' to adjust it.  */
      if (! frame_pointer_needed && reg && size + reg_save_bytes > 255)
	{
	  emit_move_insn (stack_pointer_rtx, XEXP (reg, 0));
	  size = 0;
	}
    }

  /* Maybe cut back the stack, except for the register save area.

     If the frame pointer exists, then use the frame pointer to
     cut back the stack.

     If the stack size + register save area is more than 255 bytes,
     then the stack must be cut back here since the size + register
     save size is too big for a ret/retf instruction.

     Else leave it alone, it will be cut back as part of the
     ret/retf instruction, or there wasn't any stack to begin with.

     Under no circumstances should the register save area be
     deallocated here, that would leave a window where an interrupt
     could occur and trash the register save area.  */
  if (frame_pointer_needed)
    {
      emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
      size = 0;
    }
  else if (size + reg_save_bytes > 255)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT (size)));
      size = 0;
    }

  /* Adjust the stack and restore callee-saved registers, if any.  */
  if (mn10300_can_use_rets_insn ())
    emit_jump_insn (ret_rtx);
  else
    emit_jump_insn (gen_return_ret (GEN_INT (size + reg_save_bytes)));
}

/* Recognize the PARALLEL rtx generated by mn10300_gen_multiple_store().
   This function is for MATCH_PARALLEL and so assumes OP is known to be
   parallel.  If OP is a multiple store, return a mask indicating which
   registers it saves.  Return 0 otherwise.  */

unsigned int
mn10300_store_multiple_regs (rtx op)
{
  int count;
  int mask;
  int i;
  unsigned int last;
  rtx elt;

  count = XVECLEN (op, 0);
  if (count < 2)
    return 0;

  /* Check that first instruction has the form (set (sp) (plus A B)) */
  elt = XVECEXP (op, 0, 0);
  if (GET_CODE (elt) != SET
      || (! REG_P (SET_DEST (elt)))
      || REGNO (SET_DEST (elt)) != STACK_POINTER_REGNUM
      || GET_CODE (SET_SRC (elt)) != PLUS)
    return 0;

  /* Check that A is the stack pointer and B is the expected stack size.
     For OP to match, each subsequent instruction should push a word onto
     the stack.  We therefore expect the first instruction to create
     COUNT-1 stack slots.  */
  elt = SET_SRC (elt);
  if ((! REG_P (XEXP (elt, 0)))
      || REGNO (XEXP (elt, 0)) != STACK_POINTER_REGNUM
      || (! CONST_INT_P (XEXP (elt, 1)))
      || INTVAL (XEXP (elt, 1)) != -(count - 1) * 4)
    return 0;

  mask = 0;
  for (i = 1; i < count; i++)
    {
      /* Check that element i is a (set (mem M) R).  */
      /* ??? Validate the register order a-la mn10300_gen_multiple_store.
	 Remember: the ordering is *not* monotonic.  */
      elt = XVECEXP (op, 0, i);
      if (GET_CODE (elt) != SET
	  || (! MEM_P (SET_DEST (elt)))
	  || (! REG_P (SET_SRC (elt))))
	return 0;

      /* Remember which registers are to be saved.  */
      last = REGNO (SET_SRC (elt));
      mask |= (1 << last);

      /* Check that M has the form (plus (sp) (const_int -I*4)) */
      elt = XEXP (SET_DEST (elt), 0);
      if (GET_CODE (elt) != PLUS
	  || (! REG_P (XEXP (elt, 0)))
	  || REGNO (XEXP (elt, 0)) != STACK_POINTER_REGNUM
	  || (! CONST_INT_P (XEXP (elt, 1)))
	  || INTVAL (XEXP (elt, 1)) != -i * 4)
	return 0;
    }

  /* All or none of the callee-saved extended registers must be in the set.  */
  if ((mask & 0x3c000) != 0
      && (mask & 0x3c000) != 0x3c000)
    return 0;

  return mask;
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.  */

static reg_class_t
mn10300_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (x == stack_pointer_rtx && rclass != SP_REGS)
    return (TARGET_AM33 ? GENERAL_REGS : ADDRESS_REGS);
  else if (MEM_P (x)
	   || (REG_P (x) 
	       && !HARD_REGISTER_P (x))
	   || (GET_CODE (x) == SUBREG
	       && REG_P (SUBREG_REG (x))
	       && !HARD_REGISTER_P (SUBREG_REG (x))))
    return LIMIT_RELOAD_CLASS (GET_MODE (x), rclass);
  else
    return rclass;
}

/* Implement TARGET_PREFERRED_OUTPUT_RELOAD_CLASS.  */

static reg_class_t
mn10300_preferred_output_reload_class (rtx x, reg_class_t rclass)
{
  if (x == stack_pointer_rtx && rclass != SP_REGS)
    return (TARGET_AM33 ? GENERAL_REGS : ADDRESS_REGS);
  return rclass;
}

/* Implement TARGET_SECONDARY_RELOAD.  */

static reg_class_t
mn10300_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
			  machine_mode mode, secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;
  enum reg_class xclass = NO_REGS;
  unsigned int xregno = INVALID_REGNUM;

  if (REG_P (x))
    {
      xregno = REGNO (x);
      if (xregno >= FIRST_PSEUDO_REGISTER)
	xregno = true_regnum (x);
      if (xregno != INVALID_REGNUM)
	xclass = REGNO_REG_CLASS (xregno);
    }

  if (!TARGET_AM33)
    {
      /* Memory load/stores less than a full word wide can't have an
         address or stack pointer destination.  They must use a data
         register as an intermediate register.  */
      if (rclass != DATA_REGS
	  && (mode == QImode || mode == HImode)
	  && xclass == NO_REGS)
	return DATA_REGS;

      /* We can only move SP to/from an address register.  */
      if (in_p
	  && rclass == SP_REGS
	  && xclass != ADDRESS_REGS)
	return ADDRESS_REGS;
      if (!in_p
	  && xclass == SP_REGS
	  && rclass != ADDRESS_REGS
	  && rclass != SP_OR_ADDRESS_REGS)
	return ADDRESS_REGS;
    }

  /* We can't directly load sp + const_int into a register;
     we must use an address register as an scratch.  */
  if (in_p
      && rclass != SP_REGS
      && rclass != SP_OR_ADDRESS_REGS
      && rclass != SP_OR_GENERAL_REGS
      && GET_CODE (x) == PLUS
      && (XEXP (x, 0) == stack_pointer_rtx
	  || XEXP (x, 1) == stack_pointer_rtx))
    {
      sri->icode = CODE_FOR_reload_plus_sp_const;
      return NO_REGS;
    }

  /* We can only move MDR to/from a data register.  */
  if (rclass == MDR_REGS && xclass != DATA_REGS)
    return DATA_REGS;
  if (xclass == MDR_REGS && rclass != DATA_REGS)
    return DATA_REGS;

  /* We can't load/store an FP register from a constant address.  */
  if (TARGET_AM33_2
      && (rclass == FP_REGS || xclass == FP_REGS)
      && (xclass == NO_REGS || rclass == NO_REGS))
    {
      rtx addr = NULL;

      if (xregno >= FIRST_PSEUDO_REGISTER && xregno != INVALID_REGNUM)
	{
	  addr = reg_equiv_mem (xregno);
	  if (addr)
	    addr = XEXP (addr, 0);
	}
      else if (MEM_P (x))
	addr = XEXP (x, 0);

      if (addr && CONSTANT_ADDRESS_P (addr))
	return GENERAL_REGS;
    }
  /* Otherwise assume no secondary reloads are needed.  */
  return NO_REGS;
}

int
mn10300_frame_size (void)
{
  /* size includes the fixed stack space needed for function calls.  */
  int size = get_frame_size () + crtl->outgoing_args_size;

  /* And space for the return pointer.  */
  size += crtl->outgoing_args_size ? 4 : 0;

  return size;
}

int
mn10300_initial_offset (int from, int to)
{
  int diff = 0;

  gcc_assert (from == ARG_POINTER_REGNUM || from == FRAME_POINTER_REGNUM);
  gcc_assert (to == FRAME_POINTER_REGNUM || to == STACK_POINTER_REGNUM);

  if (to == STACK_POINTER_REGNUM)
    diff = mn10300_frame_size ();

  /* The difference between the argument pointer and the frame pointer
     is the size of the callee register save area.  */
  if (from == ARG_POINTER_REGNUM)
    {
      unsigned int reg_save_bytes;

      mn10300_get_live_callee_saved_regs (& reg_save_bytes);
      diff += reg_save_bytes;
      diff += 4 * fp_regs_to_save ();
    }

  return diff;
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
mn10300_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  /* Return values > 8 bytes in length in memory.  */
  return (int_size_in_bytes (type) > 8
	  || int_size_in_bytes (type) == 0
	  || TYPE_MODE (type) == BLKmode);
}

/* Flush the argument registers to the stack for a stdarg function;
   return the new argument pointer.  */
static rtx
mn10300_builtin_saveregs (void)
{
  rtx offset, mem;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!stdarg_p (fntype))
                ? UNITS_PER_WORD : 0);
  alias_set_type set = get_varargs_alias_set ();

  if (argadj)
    offset = plus_constant (Pmode, crtl->args.arg_offset_rtx, argadj);
  else
    offset = crtl->args.arg_offset_rtx;

  mem = gen_rtx_MEM (SImode, crtl->args.internal_arg_pointer);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 0));

  mem = gen_rtx_MEM (SImode,
		     plus_constant (Pmode,
				    crtl->args.internal_arg_pointer, 4));
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 1));

  return copy_to_reg (expand_binop (Pmode, add_optab,
				    crtl->args.internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

static void
mn10300_va_start (tree valist, rtx nextarg)
{
  nextarg = expand_builtin_saveregs ();
  std_expand_builtin_va_start (valist, nextarg);
}

/* Return true when a parameter should be passed by reference.  */

static bool
mn10300_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			   machine_mode mode, const_tree type,
			   bool named ATTRIBUTE_UNUSED)
{
  unsigned HOST_WIDE_INT size;

  if (type)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  return (size > 8 || size == 0);
}

/* Return an RTX to represent where a value with mode MODE will be returned
   from a function.  If the result is NULL_RTX, the argument is pushed.  */

static rtx
mn10300_function_arg (cumulative_args_t cum_v, machine_mode mode,
		      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  rtx result = NULL_RTX;
  int size;

  /* We only support using 2 data registers as argument registers.  */
  int nregs = 2;

  /* Figure out the size of the object to be passed.  */
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  cum->nbytes = (cum->nbytes + 3) & ~3;

  /* Don't pass this arg via a register if all the argument registers
     are used up.  */
  if (cum->nbytes > nregs * UNITS_PER_WORD)
    return result;

  /* Don't pass this arg via a register if it would be split between
     registers and memory.  */
  if (type == NULL_TREE
      && cum->nbytes + size > nregs * UNITS_PER_WORD)
    return result;

  switch (cum->nbytes / UNITS_PER_WORD)
    {
    case 0:
      result = gen_rtx_REG (mode, FIRST_ARGUMENT_REGNUM);
      break;
    case 1:
      result = gen_rtx_REG (mode, FIRST_ARGUMENT_REGNUM + 1);
      break;
    default:
      break;
    }

  return result;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
mn10300_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  cum->nbytes += (mode != BLKmode
		  ? (GET_MODE_SIZE (mode) + 3) & ~3
		  : (int_size_in_bytes (type) + 3) & ~3);
}

/* Return the number of bytes of registers to use for an argument passed
   partially in registers and partially in memory.  */

static int
mn10300_arg_partial_bytes (cumulative_args_t cum_v, machine_mode mode,
			   tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int size;

  /* We only support using 2 data registers as argument registers.  */
  int nregs = 2;

  /* Figure out the size of the object to be passed.  */
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  cum->nbytes = (cum->nbytes + 3) & ~3;

  /* Don't pass this arg via a register if all the argument registers
     are used up.  */
  if (cum->nbytes > nregs * UNITS_PER_WORD)
    return 0;

  if (cum->nbytes + size <= nregs * UNITS_PER_WORD)
    return 0;

  /* Don't pass this arg via a register if it would be split between
     registers and memory.  */
  if (type == NULL_TREE
      && cum->nbytes + size > nregs * UNITS_PER_WORD)
    return 0;

  return nregs * UNITS_PER_WORD - cum->nbytes;
}

/* Return the location of the function's value.  This will be either
   $d0 for integer functions, $a0 for pointers, or a PARALLEL of both
   $d0 and $a0 if the -mreturn-pointer-on-do flag is set.  Note that
   we only return the PARALLEL for outgoing values; we do not want
   callers relying on this extra copy.  */

static rtx
mn10300_function_value (const_tree valtype,
			const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
			bool outgoing)
{
  rtx rv;
  machine_mode mode = TYPE_MODE (valtype);

  if (! POINTER_TYPE_P (valtype))
    return gen_rtx_REG (mode, FIRST_DATA_REGNUM);
  else if (! TARGET_PTR_A0D0 || ! outgoing
	   || cfun->returns_struct)
    return gen_rtx_REG (mode, FIRST_ADDRESS_REGNUM);

  rv = gen_rtx_PARALLEL (mode, rtvec_alloc (2));
  XVECEXP (rv, 0, 0)
    = gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_REG (mode, FIRST_ADDRESS_REGNUM),
			 GEN_INT (0));

  XVECEXP (rv, 0, 1)
    = gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_REG (mode, FIRST_DATA_REGNUM),
			 GEN_INT (0));
  return rv;
}

/* Implements TARGET_LIBCALL_VALUE.  */

static rtx
mn10300_libcall_value (machine_mode mode,
		       const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, FIRST_DATA_REGNUM);
}

/* Implements FUNCTION_VALUE_REGNO_P.  */

bool
mn10300_function_value_regno_p (const unsigned int regno)
{
 return (regno == FIRST_DATA_REGNUM || regno == FIRST_ADDRESS_REGNUM);
}

/* Output an addition operation.  */

const char *
mn10300_output_add (rtx operands[3], bool need_flags)
{
  rtx dest, src1, src2;
  unsigned int dest_regnum, src1_regnum, src2_regnum;
  enum reg_class src1_class, src2_class, dest_class;

  dest = operands[0];
  src1 = operands[1];
  src2 = operands[2];

  dest_regnum = true_regnum (dest);
  src1_regnum = true_regnum (src1);

  dest_class = REGNO_REG_CLASS (dest_regnum);
  src1_class = REGNO_REG_CLASS (src1_regnum);

  if (CONST_INT_P (src2))
    {
      gcc_assert (dest_regnum == src1_regnum);

      if (src2 == const1_rtx && !need_flags)
	return "inc %0";
      if (INTVAL (src2) == 4 && !need_flags && dest_class != DATA_REGS)
        return "inc4 %0";

      gcc_assert (!need_flags || dest_class != SP_REGS);
      return "add %2,%0";
    }
  else if (CONSTANT_P (src2))
    return "add %2,%0";

  src2_regnum = true_regnum (src2);
  src2_class = REGNO_REG_CLASS (src2_regnum);
      
  if (dest_regnum == src1_regnum)
    return "add %2,%0";
  if (dest_regnum == src2_regnum)
    return "add %1,%0";

  /* The rest of the cases are reg = reg+reg.  For AM33, we can implement
     this directly, as below, but when optimizing for space we can sometimes
     do better by using a mov+add.  For MN103, we claimed that we could
     implement a three-operand add because the various move and add insns
     change sizes across register classes, and we can often do better than
     reload in choosing which operand to move.  */
  if (TARGET_AM33 && optimize_insn_for_speed_p ())
    return "add %2,%1,%0";

  /* Catch cases where no extended register was used.  */
  if (src1_class != EXTENDED_REGS
      && src2_class != EXTENDED_REGS
      && dest_class != EXTENDED_REGS)
    {
      /* We have to copy one of the sources into the destination, then
         add the other source to the destination.

         Carefully select which source to copy to the destination; a
         naive implementation will waste a byte when the source classes
         are different and the destination is an address register.
         Selecting the lowest cost register copy will optimize this
         sequence.  */
      if (src1_class == dest_class)
        return "mov %1,%0\n\tadd %2,%0";
      else
	return "mov %2,%0\n\tadd %1,%0";
    }

  /* At least one register is an extended register.  */

  /* The three operand add instruction on the am33 is a win iff the
     output register is an extended register, or if both source
     registers are extended registers.  */
  if (dest_class == EXTENDED_REGS || src1_class == src2_class)
    return "add %2,%1,%0";

  /* It is better to copy one of the sources to the destination, then
     perform a 2 address add.  The destination in this case must be
     an address or data register and one of the sources must be an
     extended register and the remaining source must not be an extended
     register.

     The best code for this case is to copy the extended reg to the
     destination, then emit a two address add.  */
  if (src1_class == EXTENDED_REGS)
    return "mov %1,%0\n\tadd %2,%0";
  else
    return "mov %2,%0\n\tadd %1,%0";
}

/* Return 1 if X contains a symbolic expression.  We know these
   expressions will have one of a few well defined forms, so
   we need only check those forms.  */

int
mn10300_symbolic_operand (rtx op,
			  machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
               || GET_CODE (XEXP (op, 0)) == LABEL_REF)
              && CONST_INT_P (XEXP (op, 1)));
    default:
      return 0;
    }
}

/* Try machine dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   Normally it is always safe for this macro to do nothing.  It exists to
   recognize opportunities to optimize the output.

   But on a few ports with segmented architectures and indexed addressing
   (mn10300, hppa) it is used to rewrite certain problematical addresses.  */

static rtx
mn10300_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			    machine_mode mode ATTRIBUTE_UNUSED)
{
  if (flag_pic && ! mn10300_legitimate_pic_operand_p (x))
    x = mn10300_legitimize_pic_address (oldx, NULL_RTX);

  /* Uh-oh.  We might have an address for x[n-100000].  This needs
     special handling to avoid creating an indexed memory address
     with x-100000 as the base.  */
  if (GET_CODE (x) == PLUS
      && mn10300_symbolic_operand (XEXP (x, 1), VOIDmode))
    {
      /* Ugly.  We modify things here so that the address offset specified
         by the index expression is computed first, then added to x to form
         the entire address.  */

      rtx regx1, regy1, regy2, y;

      /* Strip off any CONST.  */
      y = XEXP (x, 1);
      if (GET_CODE (y) == CONST)
        y = XEXP (y, 0);

      if (GET_CODE (y) == PLUS || GET_CODE (y) == MINUS)
	{
	  regx1 = force_reg (Pmode, force_operand (XEXP (x, 0), 0));
	  regy1 = force_reg (Pmode, force_operand (XEXP (y, 0), 0));
	  regy2 = force_reg (Pmode, force_operand (XEXP (y, 1), 0));
	  regx1 = force_reg (Pmode,
			     gen_rtx_fmt_ee (GET_CODE (y), Pmode, regx1,
					     regy2));
	  return force_reg (Pmode, gen_rtx_PLUS (Pmode, regx1, regy1));
	}
    }
  return x;
}

/* Convert a non-PIC address in `orig' to a PIC address using @GOT or
   @GOTOFF in `reg'.  */

rtx
mn10300_legitimize_pic_address (rtx orig, rtx reg)
{
  rtx x;

  if (GET_CODE (orig) == LABEL_REF
      || (GET_CODE (orig) == SYMBOL_REF
	  && (CONSTANT_POOL_ADDRESS_P (orig)
	      || ! MN10300_GLOBAL_P (orig))))
    {
      if (reg == NULL)
	reg = gen_reg_rtx (Pmode);

      x = gen_rtx_UNSPEC (SImode, gen_rtvec (1, orig), UNSPEC_GOTOFF);
      x = gen_rtx_CONST (SImode, x);
      emit_move_insn (reg, x);

      x = emit_insn (gen_addsi3 (reg, reg, pic_offset_table_rtx));
    }
  else if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == NULL)
	reg = gen_reg_rtx (Pmode);

      x = gen_rtx_UNSPEC (SImode, gen_rtvec (1, orig), UNSPEC_GOT);
      x = gen_rtx_CONST (SImode, x);
      x = gen_rtx_PLUS (SImode, pic_offset_table_rtx, x);
      x = gen_const_mem (SImode, x);

      x = emit_move_insn (reg, x);
    }
  else
    return orig;

  set_unique_reg_note (x, REG_EQUAL, orig);
  return reg;
}

/* Return zero if X references a SYMBOL_REF or LABEL_REF whose symbol
   isn't protected by a PIC unspec; nonzero otherwise.  */

int
mn10300_legitimate_pic_operand_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    return 0;

  if (GET_CODE (x) == UNSPEC
      && (XINT (x, 1) == UNSPEC_PIC
	  || XINT (x, 1) == UNSPEC_GOT
	  || XINT (x, 1) == UNSPEC_GOTOFF
	  || XINT (x, 1) == UNSPEC_PLT
	  || XINT (x, 1) == UNSPEC_GOTSYM_OFF))
      return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (! mn10300_legitimate_pic_operand_p (XVECEXP (x, i, j)))
	      return 0;
	}
      else if (fmt[i] == 'e'
	       && ! mn10300_legitimate_pic_operand_p (XEXP (x, i)))
	return 0;
    }

  return 1;
}

/* Return TRUE if the address X, taken from a (MEM:MODE X) rtx, is
   legitimate, and FALSE otherwise.

   On the mn10300, the value in the address register must be
   in the same memory space/segment as the effective address.

   This is problematical for reload since it does not understand
   that base+index != index+base in a memory reference.

   Note it is still possible to use reg+reg addressing modes,
   it's just much more difficult.  For a discussion of a possible
   workaround and solution, see the comments in pa.c before the
   function record_unscaled_index_insn_codes.  */

static bool
mn10300_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  rtx base, index;

  if (CONSTANT_ADDRESS_P (x))
    return !flag_pic || mn10300_legitimate_pic_operand_p (x);

  if (RTX_OK_FOR_BASE_P (x, strict))
    return true;

  if (TARGET_AM33 && (mode == SImode || mode == SFmode || mode == HImode))
    {
      if (GET_CODE (x) == POST_INC)
	return RTX_OK_FOR_BASE_P (XEXP (x, 0), strict);
      if (GET_CODE (x) == POST_MODIFY)
	return (RTX_OK_FOR_BASE_P (XEXP (x, 0), strict)
		&& CONSTANT_ADDRESS_P (XEXP (x, 1)));
    }

  if (GET_CODE (x) != PLUS)
    return false;

  base = XEXP (x, 0);
  index = XEXP (x, 1);

  if (!REG_P (base))
    return false;
  if (REG_P (index))
    {
      /* ??? Without AM33 generalized (Ri,Rn) addressing, reg+reg
	 addressing is hard to satisfy.  */
      if (!TARGET_AM33)
	return false;

      return (REGNO_GENERAL_P (REGNO (base), strict)
	      && REGNO_GENERAL_P (REGNO (index), strict));
    }

  if (!REGNO_STRICT_OK_FOR_BASE_P (REGNO (base), strict))
    return false;

  if (CONST_INT_P (index))
    return IN_RANGE (INTVAL (index), -1 - 0x7fffffff, 0x7fffffff);

  if (CONSTANT_ADDRESS_P (index))
    return !flag_pic || mn10300_legitimate_pic_operand_p (index);

  return false;
}

bool
mn10300_regno_in_class_p (unsigned regno, int rclass, bool strict)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
	return true;
      if (!reg_renumber)
	return false;
      regno = reg_renumber[regno];
      if (regno == INVALID_REGNUM)
	return false;
    }
  return TEST_HARD_REG_BIT (reg_class_contents[rclass], regno);
}

rtx
mn10300_legitimize_reload_address (rtx x,
				   machine_mode mode ATTRIBUTE_UNUSED,
				   int opnum, int type,
				   int ind_levels ATTRIBUTE_UNUSED)
{
  bool any_change = false;

  /* See above re disabling reg+reg addressing for MN103.  */
  if (!TARGET_AM33)
    return NULL_RTX;

  if (GET_CODE (x) != PLUS)
    return NULL_RTX;

  if (XEXP (x, 0) == stack_pointer_rtx)
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   GENERAL_REGS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      any_change = true;
    }
  if (XEXP (x, 1) == stack_pointer_rtx)
    {
      push_reload (XEXP (x, 1), NULL_RTX, &XEXP (x, 1), NULL,
		   GENERAL_REGS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      any_change = true;
    }

  return any_change ? x : NULL_RTX;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  Returns TRUE if X is a valid
   constant.  Note that some "constants" aren't valid, such as TLS
   symbols and unconverted GOT-based references, so we eliminate
   those here.  */

static bool
mn10300_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (! CONST_INT_P (XEXP (x, 1)))
	    return false;
	  x = XEXP (x, 0);
	}

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (x) == UNSPEC)
	{
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_PIC:
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_PLT:
	      return true;
	    default:
	      return false;
	    }
	}

      /* We must have drilled down to a symbol.  */
      if (! mn10300_symbolic_operand (x, Pmode))
	return false;
      break;

    default:
      break;
    }

  return true;
}

/* Undo pic address legitimization for the benefit of debug info.  */

static rtx
mn10300_delegitimize_address (rtx orig_x)
{
  rtx x = orig_x, ret, addend = NULL;
  bool need_mem;

  if (MEM_P (x))
    x = XEXP (x, 0);
  if (GET_CODE (x) != PLUS || GET_MODE (x) != Pmode)
    return orig_x;

  if (XEXP (x, 0) == pic_offset_table_rtx)
    ;
  /* With the REG+REG addressing of AM33, var-tracking can re-assemble
     some odd-looking "addresses" that were never valid in the first place.
     We need to look harder to avoid warnings being emitted.  */
  else if (GET_CODE (XEXP (x, 0)) == PLUS)
    {
      rtx x0 = XEXP (x, 0);
      rtx x00 = XEXP (x0, 0);
      rtx x01 = XEXP (x0, 1);

      if (x00 == pic_offset_table_rtx)
	addend = x01;
      else if (x01 == pic_offset_table_rtx)
	addend = x00;
      else
	return orig_x;

    }
  else
    return orig_x;
  x = XEXP (x, 1);

  if (GET_CODE (x) != CONST)
    return orig_x;
  x = XEXP (x, 0);
  if (GET_CODE (x) != UNSPEC)
    return orig_x;

  ret = XVECEXP (x, 0, 0);
  if (XINT (x, 1) == UNSPEC_GOTOFF)
    need_mem = false;
  else if (XINT (x, 1) == UNSPEC_GOT)
    need_mem = true;
  else
    return orig_x;

  gcc_assert (GET_CODE (ret) == SYMBOL_REF);
  if (need_mem != MEM_P (orig_x))
    return orig_x;
  if (need_mem && addend)
    return orig_x;
  if (addend)
    ret = gen_rtx_PLUS (Pmode, addend, ret);
  return ret;
}

/* For addresses, costs are relative to "MOV (Rm),Rn".  For AM33 this is
   the 3-byte fully general instruction; for MN103 this is the 2-byte form
   with an address register.  */

static int
mn10300_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
		      addr_space_t as ATTRIBUTE_UNUSED, bool speed)
{
  HOST_WIDE_INT i;
  rtx base, index;

  switch (GET_CODE (x))
    {
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* We assume all of these require a 32-bit constant, even though
	 some symbol and label references can be relaxed.  */
      return speed ? 1 : 4;

    case REG:
    case SUBREG:
    case POST_INC:
      return 0;

    case POST_MODIFY:
      /* Assume any symbolic offset is a 32-bit constant.  */
      i = (CONST_INT_P (XEXP (x, 1)) ? INTVAL (XEXP (x, 1)) : 0x12345678);
      if (IN_RANGE (i, -128, 127))
	return speed ? 0 : 1;
      if (speed)
	return 1;
      if (IN_RANGE (i, -0x800000, 0x7fffff))
	return 3;
      return 4;

    case PLUS:
      base = XEXP (x, 0);
      index = XEXP (x, 1);
      if (register_operand (index, SImode))
	{
	  /* Attempt to minimize the number of registers in the address.
	     This is similar to what other ports do.  */
	  if (register_operand (base, SImode))
	    return 1;

	  base = XEXP (x, 1);
	  index = XEXP (x, 0);
	}

      /* Assume any symbolic offset is a 32-bit constant.  */
      i = (CONST_INT_P (XEXP (x, 1)) ? INTVAL (XEXP (x, 1)) : 0x12345678);
      if (IN_RANGE (i, -128, 127))
	return speed ? 0 : 1;
      if (IN_RANGE (i, -32768, 32767))
	return speed ? 0 : 2;
      return speed ? 2 : 6;

    default:
      return rtx_cost (x, Pmode, MEM, 0, speed);
    }
}

/* Implement the TARGET_REGISTER_MOVE_COST hook.

   Recall that the base value of 2 is required by assumptions elsewhere
   in the body of the compiler, and that cost 2 is special-cased as an
   early exit from reload meaning no work is required.  */

static int
mn10300_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			    reg_class_t ifrom, reg_class_t ito)
{
  enum reg_class from = (enum reg_class) ifrom;
  enum reg_class to = (enum reg_class) ito;
  enum reg_class scratch, test;

  /* Simplify the following code by unifying the fp register classes.  */
  if (to == FP_ACC_REGS)
    to = FP_REGS;
  if (from == FP_ACC_REGS)
    from = FP_REGS;

  /* Diagnose invalid moves by costing them as two moves.  */

  scratch = NO_REGS;
  test = from;
  if (to == SP_REGS)
    scratch = (TARGET_AM33 ? GENERAL_REGS : ADDRESS_REGS);
  else if (to == MDR_REGS)
    scratch = DATA_REGS;
  else if (to == FP_REGS && to != from)
    scratch = GENERAL_REGS;
  else
    {
      test = to;
      if (from == SP_REGS)
	scratch = (TARGET_AM33 ? GENERAL_REGS : ADDRESS_REGS);
      else if (from == MDR_REGS)
	scratch = DATA_REGS;
      else if (from == FP_REGS && to != from)
	scratch = GENERAL_REGS;
    }
  if (scratch != NO_REGS && !reg_class_subset_p (test, scratch))
    return (mn10300_register_move_cost (VOIDmode, from, scratch)
	    + mn10300_register_move_cost (VOIDmode, scratch, to));

  /* From here on, all we need consider are legal combinations.  */

  if (optimize_size)
    {
      /* The scale here is bytes * 2.  */

      if (from == to && (to == ADDRESS_REGS || to == DATA_REGS))
	return 2;

      if (from == SP_REGS)
	return (to == ADDRESS_REGS ? 2 : 6);

      /* For MN103, all remaining legal moves are two bytes.  */
      if (TARGET_AM33)
	return 4;

      if (to == SP_REGS)
	return (from == ADDRESS_REGS ? 4 : 6);

      if ((from == ADDRESS_REGS || from == DATA_REGS)
	   && (to == ADDRESS_REGS || to == DATA_REGS))
	return 4;

      if (to == EXTENDED_REGS)
	return (to == from ? 6 : 4);

      /* What's left are SP_REGS, FP_REGS, or combinations of the above.  */
      return 6;
    }
  else
    {
      /* The scale here is cycles * 2.  */

      if (to == FP_REGS)
	return 8;
      if (from == FP_REGS)
	return 4;

      /* All legal moves between integral registers are single cycle.  */
      return 2;
    }
}

/* Implement the TARGET_MEMORY_MOVE_COST hook.

   Given lack of the form of the address, this must be speed-relative,
   though we should never be less expensive than a size-relative register
   move cost above.  This is not a problem.  */

static int
mn10300_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED, 
			  reg_class_t iclass, bool in ATTRIBUTE_UNUSED)
{
  enum reg_class rclass = (enum reg_class) iclass;

  if (rclass == FP_REGS)
    return 8;
  return 6;
}

/* Implement the TARGET_RTX_COSTS hook.

   Speed-relative costs are relative to COSTS_N_INSNS, which is intended
   to represent cycles.  Size-relative costs are in bytes.  */

static bool
mn10300_rtx_costs (rtx x, machine_mode mode, int outer_code,
		   int opno ATTRIBUTE_UNUSED, int *ptotal, bool speed)
{
  /* This value is used for SYMBOL_REF etc where we want to pretend
     we have a full 32-bit constant.  */
  HOST_WIDE_INT i = 0x12345678;
  int total;
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      i = INTVAL (x);
    do_int_costs:
      if (speed)
	{
	  if (outer_code == SET)
	    {
	      /* 16-bit integer loads have latency 1, 32-bit loads 2.  */
	      if (IN_RANGE (i, -32768, 32767))
		total = COSTS_N_INSNS (1);
	      else
		total = COSTS_N_INSNS (2);
	    }
	  else
	    {
	      /* 16-bit integer operands don't affect latency;
		 24-bit and 32-bit operands add a cycle.  */
	      if (IN_RANGE (i, -32768, 32767))
		total = 0;
	      else
		total = COSTS_N_INSNS (1);
	    }
	}
      else
	{
	  if (outer_code == SET)
	    {
	      if (i == 0)
		total = 1;
	      else if (IN_RANGE (i, -128, 127))
		total = 2;
	      else if (IN_RANGE (i, -32768, 32767))
		total = 3;
	      else
		total = 6;
	    }
	  else
	    {
	      /* Reference here is ADD An,Dn, vs ADD imm,Dn.  */
	      if (IN_RANGE (i, -128, 127))
		total = 0;
	      else if (IN_RANGE (i, -32768, 32767))
		total = 2;
	      else if (TARGET_AM33 && IN_RANGE (i, -0x01000000, 0x00ffffff))
		total = 3;
	      else
		total = 4;
	    }
	}
      goto alldone;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      /* We assume all of these require a 32-bit constant, even though
	 some symbol and label references can be relaxed.  */
      goto do_int_costs;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_PIC:
	case UNSPEC_GOT:
	case UNSPEC_GOTOFF:
	case UNSPEC_PLT:
	case UNSPEC_GOTSYM_OFF:
	  /* The PIC unspecs also resolve to a 32-bit constant.  */
	  goto do_int_costs;

	default:
	  /* Assume any non-listed unspec is some sort of arithmetic.  */
	  goto do_arith_costs;
	}

    case PLUS:
      /* Notice the size difference of INC and INC4.  */
      if (!speed && outer_code == SET && CONST_INT_P (XEXP (x, 1)))
	{
	  i = INTVAL (XEXP (x, 1));
	  if (i == 1 || i == 4)
	    {
	      total = 1 + rtx_cost (XEXP (x, 0), mode, PLUS, 0, speed);
	      goto alldone;
	    }
	}
      goto do_arith_costs;
	
    case MINUS:
    case AND:
    case IOR:
    case XOR:
    case NOT:
    case NEG:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
    case COMPARE:
    case BSWAP:
    case CLZ:
    do_arith_costs:
      total = (speed ? COSTS_N_INSNS (1) : 2);
      break;

    case ASHIFT:
      /* Notice the size difference of ASL2 and variants.  */
      if (!speed && CONST_INT_P (XEXP (x, 1)))
	switch (INTVAL (XEXP (x, 1)))
	  {
	  case 1:
	  case 2:
	    total = 1;
	    goto alldone;
	  case 3:
	  case 4:
	    total = 2;
	    goto alldone;
	  }
      /* FALLTHRU */

    case ASHIFTRT:
    case LSHIFTRT:
      total = (speed ? COSTS_N_INSNS (1) : 3);
      goto alldone;

    case MULT:
      total = (speed ? COSTS_N_INSNS (3) : 2);
      break;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      total = (speed ? COSTS_N_INSNS (39)
		/* Include space to load+retrieve MDR.  */
		: code == MOD || code == UMOD ? 6 : 4);
      break;

    case MEM:
      total = mn10300_address_cost (XEXP (x, 0), mode,
				    MEM_ADDR_SPACE (x), speed);
      if (speed)
	total = COSTS_N_INSNS (2 + total);
      goto alldone;

    default:
      /* Probably not implemented.  Assume external call.  */
      total = (speed ? COSTS_N_INSNS (10) : 7);
      break;
    }

  *ptotal = total;
  return false;

 alldone:
  *ptotal = total;
  return true;
}

/* If using PIC, mark a SYMBOL_REF for a non-global symbol so that we
   may access it using GOTOFF instead of GOT.  */

static void
mn10300_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx symbol;

  default_encode_section_info (decl, rtl, first);

  if (! MEM_P (rtl))
    return;

  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  if (flag_pic)
    SYMBOL_REF_FLAG (symbol) = (*targetm.binds_local_p) (decl);
}

/* Dispatch tables on the mn10300 are extremely expensive in terms of code
   and readonly data size.  So we crank up the case threshold value to
   encourage a series of if/else comparisons to implement many small switch
   statements.  In theory, this value could be increased much more if we
   were solely optimizing for space, but we keep it "reasonable" to avoid
   serious code efficiency lossage.  */

static unsigned int
mn10300_case_values_threshold (void)
{
  return 6;
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
mn10300_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx mem, disp, fnaddr = XEXP (DECL_RTL (fndecl), 0);

  /* This is a strict alignment target, which means that we play
     some games to make sure that the locations at which we need
     to store <chain> and <disp> wind up at aligned addresses.

	0x28 0x00			add 0,d0
	          0xfc 0xdd		mov chain,a1
        <chain>
	0xf8 0xed 0x00			btst 0,d1
	               0xdc		jmp fnaddr
	<disp>

     Note that the two extra insns are effectively nops; they 
     clobber the flags but do not affect the contents of D0 or D1.  */

  disp = expand_binop (SImode, sub_optab, fnaddr,
		       plus_constant (Pmode, XEXP (m_tramp, 0), 11),
		       NULL_RTX, 1, OPTAB_DIRECT);

  mem = adjust_address (m_tramp, SImode, 0);
  emit_move_insn (mem, gen_int_mode (0xddfc0028, SImode));
  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 8);
  emit_move_insn (mem, gen_int_mode (0xdc00edf8, SImode));
  mem = adjust_address (m_tramp, SImode, 12);
  emit_move_insn (mem, disp);
}

/* Output the assembler code for a C++ thunk function.
   THUNK_DECL is the declaration for the thunk function itself, FUNCTION
   is the decl for the target function.  DELTA is an immediate constant
   offset to be added to the THIS parameter.  If VCALL_OFFSET is nonzero
   the word at the adjusted address *(*THIS' + VCALL_OFFSET) should be
   additionally added to THIS.  Finally jump to the entry point of
   FUNCTION.  */

static void
mn10300_asm_output_mi_thunk (FILE *        file,
			     tree          thunk_fndecl ATTRIBUTE_UNUSED,
			     HOST_WIDE_INT delta,
			     HOST_WIDE_INT vcall_offset,
			     tree          function)
{
  const char * _this;

  /* Get the register holding the THIS parameter.  Handle the case
     where there is a hidden first argument for a returned structure.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    _this = reg_names [FIRST_ARGUMENT_REGNUM + 1];
  else
    _this = reg_names [FIRST_ARGUMENT_REGNUM];

  fprintf (file, "\t%s Thunk Entry Point:\n", ASM_COMMENT_START);

  if (delta)
    fprintf (file, "\tadd %d, %s\n", (int) delta, _this);

  if (vcall_offset)
    {
      const char * scratch = reg_names [FIRST_ADDRESS_REGNUM + 1];

      fprintf (file, "\tmov %s, %s\n", _this, scratch);
      fprintf (file, "\tmov (%s), %s\n", scratch, scratch);
      fprintf (file, "\tadd %d, %s\n", (int) vcall_offset, scratch);
      fprintf (file, "\tmov (%s), %s\n", scratch, scratch);
      fprintf (file, "\tadd %s, %s\n", scratch, _this);
    }

  fputs ("\tjmp ", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  putc ('\n', file);
}

/* Return true if mn10300_output_mi_thunk would be able to output the
   assembler code for the thunk function specified by the arguments
   it is passed, and false otherwise.  */

static bool
mn10300_can_output_mi_thunk (const_tree    thunk_fndecl ATTRIBUTE_UNUSED,
			     HOST_WIDE_INT delta        ATTRIBUTE_UNUSED,
			     HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			     const_tree    function     ATTRIBUTE_UNUSED)
{
  return true;
}

bool
mn10300_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (REGNO_REG_CLASS (regno) == FP_REGS
      || REGNO_REG_CLASS (regno) == FP_ACC_REGS)
    /* Do not store integer values in FP registers.  */
    return GET_MODE_CLASS (mode) == MODE_FLOAT && ((regno & 1) == 0);

  if (! TARGET_AM33 && REGNO_REG_CLASS (regno) == EXTENDED_REGS)
    return false;

  if (((regno) & 1) == 0 || GET_MODE_SIZE (mode) == 4)
    return true;

  if (REGNO_REG_CLASS (regno) == DATA_REGS
      || (TARGET_AM33 && REGNO_REG_CLASS (regno) == ADDRESS_REGS)
      || REGNO_REG_CLASS (regno) == EXTENDED_REGS)
    return GET_MODE_SIZE (mode) <= 4;
  
  return false;
}

bool
mn10300_modes_tieable (machine_mode mode1, machine_mode mode2)
{
  if (GET_MODE_CLASS (mode1) == MODE_FLOAT
      && GET_MODE_CLASS (mode2) != MODE_FLOAT)
    return false;

  if (GET_MODE_CLASS (mode2) == MODE_FLOAT
      && GET_MODE_CLASS (mode1) != MODE_FLOAT)
    return false;

  if (TARGET_AM33
      || mode1 == mode2
      || (GET_MODE_SIZE (mode1) <= 4 && GET_MODE_SIZE (mode2) <= 4))
    return true;

  return false;
}

static int
cc_flags_for_mode (machine_mode mode)
{
  switch (mode)
    {
    case CCmode:
      return CC_FLAG_Z | CC_FLAG_N | CC_FLAG_C | CC_FLAG_V;
    case CCZNCmode:
      return CC_FLAG_Z | CC_FLAG_N | CC_FLAG_C;
    case CCZNmode:
      return CC_FLAG_Z | CC_FLAG_N;
    case CC_FLOATmode:
      return -1;
    default:
      gcc_unreachable ();
    }
}

static int
cc_flags_for_code (enum rtx_code code)
{
  switch (code)
    {
    case EQ:	/* Z */
    case NE:	/* ~Z */
      return CC_FLAG_Z;

    case LT:	/* N */
    case GE:	/* ~N */
      return CC_FLAG_N;
      break;

    case GT:    /* ~(Z|(N^V)) */
    case LE:    /* Z|(N^V) */
      return CC_FLAG_Z | CC_FLAG_N | CC_FLAG_V;

    case GEU:	/* ~C */
    case LTU:	/* C */
      return CC_FLAG_C;

    case GTU:	/* ~(C | Z) */
    case LEU:	/* C | Z */
      return CC_FLAG_Z | CC_FLAG_C;

    case ORDERED:
    case UNORDERED:
    case LTGT:
    case UNEQ:
    case UNGE:
    case UNGT:
    case UNLE:
    case UNLT:
      return -1;

    default:
      gcc_unreachable ();
    }
}

machine_mode
mn10300_select_cc_mode (enum rtx_code code, rtx x, rtx y ATTRIBUTE_UNUSED)
{
  int req;

  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CC_FLOATmode;

  req = cc_flags_for_code (code);

  if (req & CC_FLAG_V)
    return CCmode;
  if (req & CC_FLAG_C)
    return CCZNCmode;
  return CCZNmode;
}

static inline bool
set_is_load_p (rtx set)
{
  return MEM_P (SET_SRC (set));
}

static inline bool
set_is_store_p (rtx set)
{
  return MEM_P (SET_DEST (set));
}

/* Update scheduling costs for situations that cannot be
   described using the attributes and DFA machinery.
   DEP is the insn being scheduled.
   INSN is the previous insn.
   COST is the current cycle cost for DEP.  */

static int
mn10300_adjust_sched_cost (rtx_insn *insn, int dep_type, rtx_insn *dep,
			   int cost, unsigned int)
{
  rtx insn_set;
  rtx dep_set;
  int timings;

  if (!TARGET_AM33)
    return 1;

  /* We are only interested in pairs of SET. */
  insn_set = single_set (insn);
  if (!insn_set)
    return cost;

  dep_set = single_set (dep);
  if (!dep_set)
    return cost;

  /* For the AM34 a load instruction that follows a
     store instruction incurs an extra cycle of delay.  */
  if (mn10300_tune_cpu == PROCESSOR_AM34
      && set_is_load_p (dep_set)
      && set_is_store_p (insn_set))
    cost += 1;

  /* For the AM34 a non-store, non-branch FPU insn that follows
     another FPU insn incurs a one cycle throughput increase.  */
  else if (mn10300_tune_cpu == PROCESSOR_AM34
      && ! set_is_store_p (insn_set)
      && ! JUMP_P (insn)
      && GET_MODE_CLASS (GET_MODE (SET_SRC (dep_set))) == MODE_FLOAT
      && GET_MODE_CLASS (GET_MODE (SET_SRC (insn_set))) == MODE_FLOAT)
    cost += 1;

  /*  Resolve the conflict described in section 1-7-4 of
      Chapter 3 of the MN103E Series Instruction Manual
      where it says:

        "When the preceding instruction is a CPU load or
	 store instruction, a following FPU instruction
	 cannot be executed until the CPU completes the
	 latency period even though there are no register
	 or flag dependencies between them."  */

  /* Only the AM33-2 (and later) CPUs have FPU instructions.  */
  if (! TARGET_AM33_2)
    return cost;

  /* If a data dependence already exists then the cost is correct.  */
  if (dep_type == 0)
    return cost;

  /* Check that the instruction about to scheduled is an FPU instruction.  */
  if (GET_MODE_CLASS (GET_MODE (SET_SRC (dep_set))) != MODE_FLOAT)
    return cost;

  /* Now check to see if the previous instruction is a load or store.  */
  if (! set_is_load_p (insn_set) && ! set_is_store_p (insn_set))
    return cost;

  /* XXX: Verify: The text of 1-7-4 implies that the restriction
     only applies when an INTEGER load/store precedes an FPU
     instruction, but is this true ?  For now we assume that it is.  */
  if (GET_MODE_CLASS (GET_MODE (SET_SRC (insn_set))) != MODE_INT)
    return cost;

  /* Extract the latency value from the timings attribute.  */
  timings = get_attr_timings (insn);
  return timings < 100 ? (timings % 10) : (timings % 100);
}

static void
mn10300_conditional_register_usage (void)
{
  unsigned int i;

  if (!TARGET_AM33)
    {
      for (i = FIRST_EXTENDED_REGNUM;
	   i <= LAST_EXTENDED_REGNUM; i++)
	fixed_regs[i] = call_used_regs[i] = 1;
    }
  if (!TARGET_AM33_2)
    {
      for (i = FIRST_FP_REGNUM;
	   i <= LAST_FP_REGNUM; i++)
	fixed_regs[i] = call_used_regs[i] = 1;
    }
  if (flag_pic)
    fixed_regs[PIC_OFFSET_TABLE_REGNUM] =
    call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
}

/* Worker function for TARGET_MD_ASM_ADJUST.
   We do this in the mn10300 backend to maintain source compatibility
   with the old cc0-based compiler.  */

static rtx_insn *
mn10300_md_asm_adjust (vec<rtx> &/*outputs*/, vec<rtx> &/*inputs*/,
		       vec<const char *> &/*constraints*/,
		       vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs)
{
  clobbers.safe_push (gen_rtx_REG (CCmode, CC_REG));
  SET_HARD_REG_BIT (clobbered_regs, CC_REG);
  return NULL;
}

/* A helper function for splitting cbranch patterns after reload.  */

void
mn10300_split_cbranch (machine_mode cmp_mode, rtx cmp_op, rtx label_ref)
{
  rtx flags, x;

  flags = gen_rtx_REG (cmp_mode, CC_REG);
  x = gen_rtx_COMPARE (cmp_mode, XEXP (cmp_op, 0), XEXP (cmp_op, 1));
  x = gen_rtx_SET (flags, x);
  emit_insn (x);

  x = gen_rtx_fmt_ee (GET_CODE (cmp_op), VOIDmode, flags, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label_ref, pc_rtx);
  x = gen_rtx_SET (pc_rtx, x);
  emit_jump_insn (x);
}

/* A helper function for matching parallels that set the flags.  */

bool
mn10300_match_ccmode (rtx insn, machine_mode cc_mode)
{
  rtx op1, flags;
  machine_mode flags_mode;

  gcc_checking_assert (XVECLEN (PATTERN (insn), 0) == 2);

  op1 = XVECEXP (PATTERN (insn), 0, 1);
  gcc_checking_assert (GET_CODE (SET_SRC (op1)) == COMPARE);

  flags = SET_DEST (op1);
  flags_mode = GET_MODE (flags);

  if (GET_MODE (SET_SRC (op1)) != flags_mode)
    return false;
  if (GET_MODE_CLASS (flags_mode) != MODE_CC)
    return false;

  /* Ensure that the mode of FLAGS is compatible with CC_MODE.  */
  if (cc_flags_for_mode (flags_mode) & ~cc_flags_for_mode (cc_mode))
    return false;

  return true;
}

/* This function is used to help split:
   
     (set (reg) (and (reg) (int)))
     
   into:
   
     (set (reg) (shift (reg) (int))
     (set (reg) (shift (reg) (int))
     
   where the shitfs will be shorter than the "and" insn.

   It returns the number of bits that should be shifted.  A positive
   values means that the low bits are to be cleared (and hence the
   shifts should be right followed by left) whereas a negative value
   means that the high bits are to be cleared (left followed by right).
   Zero is returned when it would not be economical to split the AND.  */

int
mn10300_split_and_operand_count (rtx op)
{
  HOST_WIDE_INT val = INTVAL (op);
  int count;

  if (val < 0)
    {
      /* High bit is set, look for bits clear at the bottom.  */
      count = exact_log2 (-val);
      if (count < 0)
	return 0;
      /* This is only size win if we can use the asl2 insn.  Otherwise we
	 would be replacing 1 6-byte insn with 2 3-byte insns.  */
      if (count > (optimize_insn_for_speed_p () ? 2 : 4))
	return 0;
      return count;
    }
  else
    {
      /* High bit is clear, look for bits set at the bottom.  */
      count = exact_log2 (val + 1);
      count = 32 - count;
      /* Again, this is only a size win with asl2.  */
      if (count > (optimize_insn_for_speed_p () ? 2 : 4))
	return 0;
      return -count;
    }
}

struct liw_data
{
  enum attr_liw slot;
  enum attr_liw_op op;
  rtx dest;
  rtx src;
};

/* Decide if the given insn is a candidate for LIW bundling.  If it is then
   extract the operands and LIW attributes from the insn and use them to fill
   in the liw_data structure.  Return true upon success or false if the insn
   cannot be bundled.  */

static bool
extract_bundle (rtx_insn *insn, struct liw_data * pdata)
{
  bool allow_consts = true;
  rtx p;

  gcc_assert (pdata != NULL);

  if (insn == NULL)
    return false;
  /* Make sure that we are dealing with a simple SET insn.  */
  p = single_set (insn);
  if (p == NULL_RTX)
    return false;

  /* Make sure that it could go into one of the LIW pipelines.  */
  pdata->slot = get_attr_liw (insn);
  if (pdata->slot == LIW_BOTH)
    return false;

  pdata->op = get_attr_liw_op (insn);

  switch (pdata->op)
    {
    case LIW_OP_MOV:
      pdata->dest = SET_DEST (p);
      pdata->src = SET_SRC (p);
      break;
    case LIW_OP_CMP:
      pdata->dest = XEXP (SET_SRC (p), 0);
      pdata->src = XEXP (SET_SRC (p), 1);
      break;
    case LIW_OP_NONE:
      return false;
    case LIW_OP_AND:
    case LIW_OP_OR:
    case LIW_OP_XOR:
      /* The AND, OR and XOR long instruction words only accept register arguments.  */
      allow_consts = false;
      /* Fall through.  */
    default:
      pdata->dest = SET_DEST (p);
      pdata->src = XEXP (SET_SRC (p), 1);
      break;
    }

  if (! REG_P (pdata->dest))
    return false;

  if (REG_P (pdata->src))
    return true;

  return allow_consts && satisfies_constraint_O (pdata->src);
}

/* Make sure that it is OK to execute LIW1 and LIW2 in parallel.  GCC generated
   the instructions with the assumption that LIW1 would be executed before LIW2
   so we must check for overlaps between their sources and destinations.  */

static bool
check_liw_constraints (struct liw_data * pliw1, struct liw_data * pliw2)
{
  /* Check for slot conflicts.  */
  if (pliw2->slot == pliw1->slot && pliw1->slot != LIW_EITHER)
    return false;

  /* If either operation is a compare, then "dest" is really an input; the real
     destination is CC_REG.  So these instructions need different checks.  */

  /* Changing "CMP ; OP" into "CMP | OP" is OK because the comparison will
     check its values prior to any changes made by OP.  */
  if (pliw1->op == LIW_OP_CMP)
    {
      /* Two sequential comparisons means dead code, which ought to 
         have been eliminated given that bundling only happens with
         optimization.  We cannot bundle them in any case.  */
      gcc_assert (pliw1->op != pliw2->op);
      return true;
    }

  /* Changing "OP ; CMP" into "OP | CMP" does not work if the value being compared
     is the destination of OP, as the CMP will look at the old value, not the new
     one.  */
  if (pliw2->op == LIW_OP_CMP)
    {
      if (REGNO (pliw2->dest) == REGNO (pliw1->dest))
	return false;

      if (REG_P (pliw2->src))
	return REGNO (pliw2->src) != REGNO (pliw1->dest);

      return true;
    }

  /* Changing "OP1 ; OP2" into "OP1 | OP2" does not work if they both write to the
     same destination register.  */
  if (REGNO (pliw2->dest) == REGNO (pliw1->dest))
    return false;

  /* Changing "OP1 ; OP2" into "OP1 | OP2" generally does not work if the destination
     of OP1 is the source of OP2.  The exception is when OP1 is a MOVE instruction when
     we can replace the source in OP2 with the source of OP1.  */
  if (REG_P (pliw2->src) && REGNO (pliw2->src) == REGNO (pliw1->dest))
    {
      if (pliw1->op == LIW_OP_MOV && REG_P (pliw1->src))
	{
	  if (! REG_P (pliw1->src)
	      && (pliw2->op == LIW_OP_AND
		  || pliw2->op == LIW_OP_OR
		  || pliw2->op == LIW_OP_XOR))
	    return false;
		  
	  pliw2->src = pliw1->src;
	  return true;
	}
      return false;
    }

  /* Everything else is OK.  */
  return true;
}

/* Combine pairs of insns into LIW bundles.  */

static void
mn10300_bundle_liw (void)
{
  rtx_insn *r;

  for (r = get_insns (); r != NULL; r = next_nonnote_nondebug_insn (r))
    {
      rtx_insn *insn1, *insn2;
      struct liw_data liw1, liw2;

      insn1 = r;
      if (! extract_bundle (insn1, & liw1))
	continue;

      insn2 = next_nonnote_nondebug_insn (insn1);
      if (! extract_bundle (insn2, & liw2))
	continue;

      /* Check for source/destination overlap.  */
      if (! check_liw_constraints (& liw1, & liw2))
	continue;

      if (liw1.slot == LIW_OP2 || liw2.slot == LIW_OP1)
	{
	  struct liw_data temp;
	  
	  temp = liw1;
	  liw1 = liw2;
	  liw2 = temp;
	}

      delete_insn (insn2);

      rtx insn2_pat;
      if (liw1.op == LIW_OP_CMP)
	insn2_pat = gen_cmp_liw (liw2.dest, liw2.src, liw1.dest, liw1.src,
				 GEN_INT (liw2.op));
      else if (liw2.op == LIW_OP_CMP)
	insn2_pat = gen_liw_cmp (liw1.dest, liw1.src, liw2.dest, liw2.src,
				 GEN_INT (liw1.op));
      else
	insn2_pat = gen_liw (liw1.dest, liw2.dest, liw1.src, liw2.src,
			     GEN_INT (liw1.op), GEN_INT (liw2.op));

      insn2 = emit_insn_after (insn2_pat, insn1);
      delete_insn (insn1);
      r = insn2;
    }
}

#define DUMP(reason, insn)			\
  do						\
    {						\
      if (dump_file)				\
	{					\
	  fprintf (dump_file, reason "\n");	\
	  if (insn != NULL_RTX)			\
	    print_rtl_single (dump_file, insn);	\
	  fprintf(dump_file, "\n");		\
	}					\
    }						\
  while (0)

/* Replace the BRANCH insn with a Lcc insn that goes to LABEL.
   Insert a SETLB insn just before LABEL.  */

static void
mn10300_insert_setlb_lcc (rtx label, rtx branch)
{
  rtx lcc, comparison, cmp_reg;

  if (LABEL_NUSES (label) > 1)
    {
      rtx_insn *insn;

      /* This label is used both as an entry point to the loop
	 and as a loop-back point for the loop.  We need to separate
	 these two functions so that the SETLB happens upon entry,
	 but the loop-back does not go to the SETLB instruction.  */
      DUMP ("Inserting SETLB insn after:", label);
      insn = emit_insn_after (gen_setlb (), label);
      label = gen_label_rtx ();
      emit_label_after (label, insn);
      DUMP ("Created new loop-back label:", label);
    }
  else
    {
      DUMP ("Inserting SETLB insn before:", label);
      emit_insn_before (gen_setlb (), label);
    }

  comparison = XEXP (SET_SRC (PATTERN (branch)), 0);
  cmp_reg = XEXP (comparison, 0);
  gcc_assert (REG_P (cmp_reg));

  /* If the comparison has not already been split out of the branch
     then do so now.  */
  gcc_assert (REGNO (cmp_reg) == CC_REG);

  if (GET_MODE (cmp_reg) == CC_FLOATmode)
    lcc = gen_FLcc (comparison, label);
  else
    lcc = gen_Lcc (comparison, label);    

  rtx_insn *jump = emit_jump_insn_before (lcc, branch);
  mark_jump_label (XVECEXP (lcc, 0, 0), jump, 0);
  JUMP_LABEL (jump) = label;
  DUMP ("Replacing branch insn...", branch);
  DUMP ("... with Lcc insn:", jump);
  delete_insn (branch);
}

static bool
mn10300_block_contains_call (basic_block block)
{
  rtx_insn *insn;

  FOR_BB_INSNS (block, insn)
    if (CALL_P (insn))
      return true;

  return false;
}

static bool
mn10300_loop_contains_call_insn (loop_p loop)
{
  basic_block * bbs;
  bool result = false;
  unsigned int i;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    if (mn10300_block_contains_call (bbs[i]))
      {
	result = true;
	break;
      }

  free (bbs);
  return result;
}

static void
mn10300_scan_for_setlb_lcc (void)
{
  loop_p loop;

  DUMP ("Looking for loops that can use the SETLB insn", NULL_RTX);

  df_analyze ();
  compute_bb_for_insn ();

  /* Find the loops.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  /* FIXME: For now we only investigate innermost loops.  In practice however
     if an inner loop is not suitable for use with the SETLB/Lcc insns, it may
     be the case that its parent loop is suitable.  Thus we should check all
     loops, but work from the innermost outwards.  */
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    {
      const char * reason = NULL;

      /* Check to see if we can modify this loop.  If we cannot
	 then set 'reason' to describe why it could not be done.  */
      if (loop->latch == NULL)
	reason = "it contains multiple latches";
      else if (loop->header != loop->latch)
	/* FIXME: We could handle loops that span multiple blocks,
	   but this requires a lot more work tracking down the branches
	   that need altering, so for now keep things simple.  */
	reason = "the loop spans multiple blocks";
      else if (mn10300_loop_contains_call_insn (loop))
	reason = "it contains CALL insns";
      else
	{
	  rtx_insn *branch = BB_END (loop->latch);

	  gcc_assert (JUMP_P (branch));
	  if (single_set (branch) == NULL_RTX || ! any_condjump_p (branch))
	    /* We cannot optimize tablejumps and the like.  */
	    /* FIXME: We could handle unconditional jumps.  */
	    reason = "it is not a simple loop";
	  else
	    {
	      rtx_insn *label;

	      if (dump_file)
		flow_loop_dump (loop, dump_file, NULL, 0);

	      label = BB_HEAD (loop->header);
	      gcc_assert (LABEL_P (label));

	      mn10300_insert_setlb_lcc (label, branch);
	    }
	}

      if (dump_file && reason != NULL)
	fprintf (dump_file, "Loop starting with insn %d is not suitable because %s\n",
		 INSN_UID (BB_HEAD (loop->header)),
		 reason);
    }

  loop_optimizer_finalize ();

  df_finish_pass (false);  

  DUMP ("SETLB scan complete", NULL_RTX);
}

static void
mn10300_reorg (void)
{
  /* These are optimizations, so only run them if optimizing.  */
  if (TARGET_AM33 && (optimize > 0 || optimize_size))
    {
      if (TARGET_ALLOW_SETLB)
	mn10300_scan_for_setlb_lcc ();

      if (TARGET_ALLOW_LIW)
	mn10300_bundle_liw ();
    }
}

/* Initialize the GCC target structure.  */

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG mn10300_reorg

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS mn10300_legitimize_address

#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST  mn10300_address_cost
#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST  mn10300_register_move_cost
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST  mn10300_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS mn10300_rtx_costs

#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START mn10300_file_start
#undef  TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA mn10300_asm_output_addr_const_extra

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE mn10300_option_override

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO mn10300_encode_section_info

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY mn10300_return_in_memory
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE mn10300_pass_by_reference
#undef  TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES mn10300_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG mn10300_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE mn10300_function_arg_advance

#undef  TARGET_EXPAND_BUILTIN_SAVEREGS
#define TARGET_EXPAND_BUILTIN_SAVEREGS mn10300_builtin_saveregs
#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START mn10300_va_start

#undef  TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD mn10300_case_values_threshold

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	mn10300_legitimate_address_p
#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS	mn10300_delegitimize_address
#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P	mn10300_legitimate_constant_p

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS mn10300_preferred_reload_class
#undef  TARGET_PREFERRED_OUTPUT_RELOAD_CLASS
#define TARGET_PREFERRED_OUTPUT_RELOAD_CLASS \
  mn10300_preferred_output_reload_class
#undef  TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD  mn10300_secondary_reload

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT mn10300_trampoline_init

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE mn10300_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE mn10300_libcall_value

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK      mn10300_asm_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK  mn10300_can_output_mi_thunk

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST mn10300_adjust_sched_cost

#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE mn10300_conditional_register_usage

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST mn10300_md_asm_adjust

#undef  TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM  CC_REG

struct gcc_target targetm = TARGET_INITIALIZER;
