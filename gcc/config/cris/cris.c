/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "recog.h"
#include "tm_p.h"
#include "debug.h"
#include "output.h"
#include "target.h"
#include "target-def.h"
#include "ggc.h"

/* Usable when we have an amount to add or subtract, and want the
   optimal size of the insn.  */
#define ADDITIVE_SIZE_MODIFIER(size) \
 ((size) <= 63 ? "q" : (size) <= 255 ? "u.b" : (size) <= 65535 ? "u.w" : ".d")

#define ASSERT_PLT_UNSPEC(x)					\
  do								\
    {								\
      if (XEXP (x, 1) != NULL_RTX				\
	  || (GET_CODE (XVECEXP (x, 0, 0)) != SYMBOL_REF	\
	      && GET_CODE (XVECEXP (x, 0, 0)) != LABEL_REF))	\
	abort ();						\
    } while (0)

#define LOSE_AND_RETURN(msgid, x)			\
  do						\
    {						\
      cris_operand_lossage (msgid, x);		\
      return;					\
    } while (0)

/* Per-function machine data.  */
struct machine_function GTY(())
 {
   int needs_return_address_on_stack;
 };

/* This little fix suppresses the 'u' or 's' when '%e' in assembly
   pattern.  */
static char cris_output_insn_is_bound = 0;

/* This one suppresses printing out the "rPIC+" in
   "rPIC+sym:GOTOFF+offset" when doing PIC.  For a PLT symbol, it
   suppresses outputting it as [rPIC+sym:GOTPLT] and outputs similarly
   just the "sym:GOTOFF" part.  */
static int cris_pic_sympart_only = 0;

/* Fix for reg_overlap_mentioned_p.  */
static int cris_reg_overlap_mentioned_p PARAMS ((rtx, rtx));

static void cris_print_base PARAMS ((rtx, FILE *));

static void cris_print_index PARAMS ((rtx, FILE *));

static struct machine_function * cris_init_machine_status PARAMS ((void));

static int cris_initial_frame_pointer_offset PARAMS ((void));

static int saved_regs_mentioned PARAMS ((rtx));

static void cris_target_asm_function_prologue
  PARAMS ((FILE *, HOST_WIDE_INT));

static void cris_target_asm_function_epilogue
  PARAMS ((FILE *, HOST_WIDE_INT));

static void cris_encode_section_info PARAMS ((tree, int));
static void cris_operand_lossage PARAMS ((const char *, rtx));

static void cris_asm_output_mi_thunk
  PARAMS ((FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT, tree));


/* The function cris_target_asm_function_epilogue puts the last insn to
   output here.  It always fits; there won't be a symbol operand.  Used in
   delay_slots_for_epilogue and function_epilogue.  */
static char save_last[80];

/* This is the argument from the "-max-stack-stackframe=" option.  */
const char *cris_max_stackframe_str;

/* This is the argument from the "-march=" option.  */
const char *cris_cpu_str;

/* This is the argument from the "-mtune=" option.  */
const char *cris_tune_str;

/* This is the argument from the "-melinux-stacksize=" option.  */
const char *cris_elinux_stacksize_str;

/* This is the parsed result of the "-max-stack-stackframe=" option.  If
   it (still) is zero, then there was no such option given.  */
int cris_max_stackframe = 0;

/* This is the parsed result of the "-march=" option, if given.  */
int cris_cpu_version = CRIS_DEFAULT_CPU_VERSION;

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.dword\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"

/* We need to define these, since the 2byte, 4byte, 8byte op:s are only
   available in ELF.  These "normal" pseudos do not have any alignment
   constraints or side-effects.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP

#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP

#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE cris_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE cris_target_asm_function_epilogue

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO cris_encode_section_info

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK cris_asm_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

struct gcc_target targetm = TARGET_INITIALIZER;

/* Predicate functions.  */

/* This checks a part of an address, the one that is not a plain register
   for an addressing mode using BDAP.
   Allowed operands is either:
   a) a register
   b) a CONST operand (but not a symbol when generating PIC)
   c) a [r] or [r+] in SImode, or sign-extend from HI or QI.  */

int
cris_bdap_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);

  if (mode != SImode && (mode != VOIDmode || GET_MODE (op) != VOIDmode))
    return 0;

  /* Just return whether this is a simple register or constant.  */
  if (register_operand (op, mode)
      || (CONSTANT_P (op) && !(flag_pic && cris_symbol (op))))
    return 1;

  /* Is it a [r] or possibly a [r+]?  */
  if (code == MEM)
    {
      rtx tem = XEXP (op, 0);

      if (mode == SImode
	  && (register_operand (tem, SImode)
	      || (GET_CODE (tem) == POST_INC
		  && register_operand (XEXP (tem, 0), SImode))))
	return 1;
      else
	return 0;
    }

  /* Perhaps a sign-extended mem: [r].(b|w) or [r+].(b|w)?  */
  if (code == SIGN_EXTEND)
    {
      rtx tem = XEXP (op, 0);

      if (GET_CODE (tem) != MEM)
	return 0;

      tem = XEXP (tem, 0);
      if (mode == SImode
	  && (register_operand (tem, SImode)
	      || (GET_CODE (tem) == POST_INC
		  && register_operand (XEXP (tem, 0), SImode))))
	return 1;
      else
	return 0;
    }

  return 0;
}

/* This is similar to cris_bdap_operand:
   It checks a part of an address, the one that is not a plain register
   for an addressing mode using BDAP *or* BIAP.
   Allowed operands is either:
   a) a register
   b) a CONST operand (but not a symbol when generating PIC)
   c) a mult of (1, 2 or 4) and a register
   d) a [r] or [r+] in SImode, or sign-extend from HI or QI.  */

int
cris_bdap_biap_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);
  rtx reg;
  rtx val;

  /* Check for bdap operand.  */
  if (cris_bdap_operand (op, mode))
    return 1;

  if (mode != SImode && (mode != VOIDmode || GET_MODE (op) != VOIDmode))
    return 0;

  /* Check that we're looking at a BIAP operand.  */
  if (code != MULT)
    return 0;

  /* Canonicalize register and multiplicand.  */
  if (GET_CODE (XEXP (op, 0)) == CONST_INT)
    {
      val = XEXP (op, 0);
      reg = XEXP (op, 1);
    }
  else
    {
      val = XEXP (op, 1);
      reg = XEXP (op, 0);
    }

  /* Check that the operands are correct after canonicalization.  */
  if (! register_operand (reg, SImode) || GET_CODE (val) != CONST_INT)
    return 0;

  /* Check that the multiplicand has a valid value.  */
  if ((code == MULT
       && (INTVAL (val) == 1 || INTVAL (val) == 2 || INTVAL (val) == 4)))
    return 1;

  return 0;
}

/* Check if MODE is same as mode for X, and X is PLUS, MINUS, IOR or
   AND or UMIN.  */

int
cris_orthogonal_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == PLUS || code == MINUS
	      || code == IOR || code == AND || code == UMIN));
}

/* Check if MODE is same as mode for X, and X is PLUS, IOR or AND or
   UMIN.  */

int
cris_commutative_orth_op (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode &&
	  (code == PLUS
	   || code == IOR || code == AND || code == UMIN));
}

/* Check if MODE is same as mode for X, and X is PLUS or MINUS or UMIN.  */

int
cris_operand_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == PLUS || code == MINUS || code == UMIN));
}

/* Check if MODE is same as mode for X, and X is PLUS or MINUS.  */

int
cris_additive_operand_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == PLUS || code == MINUS));
}

/* Check to see if MODE is same as mode for X, and X is SIGN_EXTEND or
   ZERO_EXTEND.  */

int
cris_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return
    (GET_MODE (x) == mode && (code == SIGN_EXTEND || code == ZERO_EXTEND));
}

/* Check to see if MODE is same as mode for X, and X is PLUS or BOUND.  */

int
cris_plus_or_bound_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return
    (GET_MODE (x) == mode && (code == UMIN || code == PLUS));
}

/* Since with -fPIC, not all symbols are valid PIC symbols or indeed
   general_operands, we have to have a predicate that matches it for the
   "movsi" expander.  */

int
cris_general_operand_or_symbol (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return general_operand (op, mode)
    || (CONSTANT_P (op) && cris_symbol (op));
}

/* Since a PIC symbol without a GOT entry is not a general_operand, we
   have to have a predicate that matches it.  We use this in the expanded
   "movsi" anonymous pattern for PIC symbols.  */

int
cris_general_operand_or_gotless_symbol (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return general_operand (op, mode)
    || (CONSTANT_P (op) && cris_gotless_symbol (op));
}

/* Since a PLT symbol is not a general_operand, we have to have a
   predicate that matches it when we need it.  We use this in the expanded
   "call" and "call_value" anonymous patterns.  */

int
cris_general_operand_or_plt_symbol (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return general_operand (op, mode)
    || (GET_CODE (op) == CONST
	&& GET_CODE (XEXP (op, 0)) == UNSPEC
	&& !TARGET_AVOID_GOTPLT);
}

/* This matches a (MEM (general_operand)) or
   (MEM (cris_general_operand_or_symbol)).  The second one isn't a valid
   memory_operand, so we need this predicate to recognize call
   destinations before we change them to a PLT operand (by wrapping in
   UNSPEC 0).  */

int
cris_mem_call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx xmem;

  if (GET_CODE (op) != MEM)
    return 0;

  if (memory_operand (op, mode))
    return 1;

  xmem = XEXP (op, 0);

  return cris_general_operand_or_symbol (xmem, GET_MODE (op));
}

/* The CONDITIONAL_REGISTER_USAGE worker.   */

void
cris_conditional_register_usage ()
{
  /* FIXME: This isn't nice.  We should be able to use that register for
     something else if the PIC table isn't needed.  */
  if (flag_pic)
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
}

/* Return current_function_uses_pic_offset_table.  For use in cris.md,
   since some generated files do not include function.h.  */

int
cris_cfun_uses_pic_table ()
{
  return current_function_uses_pic_offset_table;
}

/* Given an rtx, return the text string corresponding to the CODE of X.
   Intended for use in the assembly language output section of a
   define_insn.  */

const char *
cris_op_str (x)
     rtx x;
{
  cris_output_insn_is_bound = 0;
  switch (GET_CODE (x))
    {
    case PLUS:
      return "add";
      break;

    case MINUS:
      return "sub";
      break;

    case MULT:
      return "mul";
      break;

    case DIV:
      return "div";
      break;

    case AND:
      return "and";
      break;

    case IOR:
      return "or";
      break;

    case XOR:
      return "xor";
      break;

    case NOT:
      return "not";
      break;

    case ASHIFT:
      return "lsl";
      break;

    case LSHIFTRT:
      return "lsr";
      break;

    case ASHIFTRT:
      return "asr";
      break;

    case UMIN:
      /* Used to control the sign/zero-extend character for the 'e' modifier.
	 BOUND has none.  */
      cris_output_insn_is_bound = 1;
      return "bound";
      break;

    default:
      return "Unknown operator";
      break;
  }
}

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

static void
cris_operand_lossage (msgid, op)
     const char *msgid;
     rtx op;
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* Print an index part of an address to file.  */

static void
cris_print_index (index, file)
     rtx index;
     FILE * file;
{
  rtx inner = XEXP (index, 0);

  /* Make the index "additive" unless we'll output a negative number, in
     which case the sign character is free (as in free beer).  */
  if (GET_CODE (index) != CONST_INT || INTVAL (index) >= 0)
    putc ('+', file);

  if (REG_P (index))
    fprintf (file, "$%s.b", reg_names[REGNO (index)]);
  else if (CONSTANT_P (index))
    cris_output_addr_const (file, index);
  else if (GET_CODE (index) == MULT)
    {
      fprintf (file, "$%s.",
	       reg_names[REGNO (XEXP (index, 0))]);

      putc (INTVAL (XEXP (index, 1)) == 2 ? 'w' : 'd', file);
    }
  else if (GET_CODE (index) == SIGN_EXTEND &&
	   GET_CODE (inner) == MEM)
    {
      rtx inner_inner = XEXP (inner, 0);

      if (GET_CODE (inner_inner) == POST_INC)
	{
	  fprintf (file, "[$%s+].",
		   reg_names[REGNO (XEXP (inner_inner, 0))]);
	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
      else
	{
	  fprintf (file, "[$%s].", reg_names[REGNO (inner_inner)]);

	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
    }
  else if (GET_CODE (index) == MEM)
    {
      if (GET_CODE (inner) == POST_INC)
	fprintf (file, "[$%s+].d", reg_names[REGNO (XEXP (inner, 0))]);
      else
	fprintf (file, "[$%s].d", reg_names[REGNO (inner)]);
    }
  else
    cris_operand_lossage ("unexpected index-type in cris_print_index",
			  index);
}

/* Print a base rtx of an address to file.  */

static void
cris_print_base (base, file)
     rtx base;
     FILE *file;
{
  if (REG_P (base))
    fprintf (file, "$%s", reg_names[REGNO (base)]);
  else if (GET_CODE (base) == POST_INC)
    fprintf (file, "$%s+", reg_names[REGNO (XEXP (base, 0))]);
  else
    cris_operand_lossage ("unexpected base-type in cris_print_base",
			  base);
}

/* Usable as a guard in expressions.  */

int
cris_fatal (arg)
     char *arg;
{
  internal_error (arg);

  /* We'll never get here; this is just to appease compilers.  */
  return 0;
}

/* Textual function prologue.  */

static void
cris_target_asm_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int regno;

  /* Shorten the used name for readability.  */
  int cfoa_size = current_function_outgoing_args_size;
  int last_movem_reg = -1;
  int doing_dwarf = dwarf2out_do_frame ();
  int framesize;
  int faked_args_size = 0;
  int cfa_write_offset = 0;
  char *cfa_label = NULL;
  int return_address_on_stack
    = regs_ever_live[CRIS_SRP_REGNUM]
    || cfun->machine->needs_return_address_on_stack != 0;

  /* Don't do anything if no prologues or epilogues are wanted.  */
  if (!TARGET_PROLOGUE_EPILOGUE)
    return;

  if (size < 0)
    abort ();

  /* Align the size to what's best for the CPU model.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  if (current_function_pretend_args_size)
    {
      int pretend = current_function_pretend_args_size;
      for (regno = CRIS_FIRST_ARG_REG + CRIS_MAX_ARGS_IN_REGS - 1;
	   pretend > 0;
	   regno--, pretend -= 4)
	{
	  fprintf (file, "\tpush $%s\n", reg_names[regno]);
	  faked_args_size += 4;
	}
    }

  framesize = faked_args_size;

  if (doing_dwarf)
    {
      /* FIXME: Slightly redundant calculation, as we do the same in
	 pieces below.  This offset must be the total adjustment of the
	 stack-pointer.  We can then def_cfa call at the end of this
	 function with the current implementation of execute_cfa_insn, but
	 that wouldn't really be clean.  */

      int cfa_offset
	= faked_args_size
	+ (return_address_on_stack ? 4 : 0)
	+ (frame_pointer_needed ? 4 : 0);

      int cfa_reg;

      if (frame_pointer_needed)
	cfa_reg = FRAME_POINTER_REGNUM;
      else
	{
	  cfa_reg = STACK_POINTER_REGNUM;
	  cfa_offset += cris_initial_frame_pointer_offset ();
	}

      cfa_label = dwarf2out_cfi_label ();
      dwarf2out_def_cfa (cfa_label, cfa_reg, cfa_offset);

      cfa_write_offset = - faked_args_size - 4;
    }

  /* Save SRP if not a leaf function.  */
  if (return_address_on_stack)
    {
      fprintf (file, "\tPush $srp\n");
      framesize += 4;

      if (doing_dwarf)
	{
	  dwarf2out_return_save (cfa_label, cfa_write_offset);
	  cfa_write_offset -= 4;
	}
    }

  /* Set up frame pointer if needed.  */
  if (frame_pointer_needed)
    {
      fprintf (file, "\tpush $%s\n\tmove.d $sp,$%s\n",
	       reg_names[FRAME_POINTER_REGNUM],
	       reg_names[FRAME_POINTER_REGNUM]);
      framesize += 4;

      if (doing_dwarf)
	{
	  dwarf2out_reg_save (cfa_label, FRAME_POINTER_REGNUM,
			      cfa_write_offset);
	  cfa_write_offset -= 4;
	}
    }

  /* Local vars are located above saved regs.  */
  cfa_write_offset -= size;

  /* Get a contiguous sequence of registers, starting with r0, that need
     to be saved.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if ((((regs_ever_live[regno]
	     && !call_used_regs[regno])
	    || (regno == (int) PIC_OFFSET_TABLE_REGNUM
		&& (current_function_uses_pic_offset_table
		    /* It is saved anyway, if there would be a gap.  */
		    || (flag_pic
			&& regs_ever_live[regno + 1]
			&& !call_used_regs[regno + 1]))))
	   && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
	   && regno != CRIS_SRP_REGNUM)
	  || (current_function_calls_eh_return
	      && (regno == EH_RETURN_DATA_REGNO (0)
		  || regno == EH_RETURN_DATA_REGNO (1)
		  || regno == EH_RETURN_DATA_REGNO (2)
		  || regno == EH_RETURN_DATA_REGNO (3))))
	{
	  /* Check if movem may be used for registers so far.  */
	  if (regno == last_movem_reg + 1)
	    /* Yes, update next expected register.  */
	    last_movem_reg++;
	  else
	    {
	      /* We cannot use movem for all registers.  We have to flush
		 any movem:ed registers we got so far.  */
	      if (last_movem_reg != -1)
		{
		  /* It is a win to use a side-effect assignment for
		     64 <= size <= 128.  But side-effect on movem was
		     not usable for CRIS v0..3.  Also only do it if
		     side-effects insns are allowed.  */
		  if ((last_movem_reg + 1) * 4 + size >= 64
		      && (last_movem_reg + 1) * 4 + size <= 128
		      && cris_cpu_version >= CRIS_CPU_SVINTO
		      && TARGET_SIDE_EFFECT_PREFIXES)
		    fprintf (file, "\tmovem $%s,[$sp=$sp-%d]\n",
			     reg_names[last_movem_reg],
			     (last_movem_reg + 1) * 4 + size);
		  else
		    {
		      /* Avoid printing multiple subsequent sub:s for sp.  */
		      fprintf (file, "\tsub%s %d,$sp\n",
			       ADDITIVE_SIZE_MODIFIER ((last_movem_reg + 1)
						       * 4 + size),
			       (last_movem_reg + 1) * 4 + size);

		      fprintf (file, "\tmovem $%s,[$sp]\n",
			       reg_names[last_movem_reg]);
		    }

		  framesize += (last_movem_reg + 1) * 4 + size;

		  if (TARGET_PDEBUG)
		    fprintf (file, "; frame %d, #regs %d, bytes %d args %d\n",
			     size,
			     last_movem_reg + 1,
			     (last_movem_reg + 1) * 4,
			     current_function_args_size);

		  last_movem_reg = -1;
		  size = 0;
		}
	      else if (size > 0)
		{
		  /* Local vars on stack, but there are no movem:s.
		     Just allocate space.  */
		  fprintf (file, "\tSub%s %d,$sp\n",
			   ADDITIVE_SIZE_MODIFIER (size),
			   size);
		  framesize += size;
		  size = 0;
		}

	      fprintf (file, "\tPush $%s\n", reg_names[regno]);
	      framesize += 4;
	    }

	  if (doing_dwarf)
	    {
	      /* Registers are stored lowest numbered at highest address,
		 which matches the loop order; we just need to update the
		 write-offset.  */
	      dwarf2out_reg_save (cfa_label, regno, cfa_write_offset);
	      cfa_write_offset -= 4;
	    }
	}
    }

  /* Check after, if we can movem all registers.  This is the normal
     case.  */
  if (last_movem_reg != -1)
    {
      /* Side-effect assignment on movem was not supported for CRIS v0..3,
	 and don't do it if we're asked not to.

	 The movem is already accounted for, for unwind.  */

      if ((last_movem_reg + 1) * 4 + size >= 64
	  && (last_movem_reg + 1) * 4 + size <= 128
	  && cris_cpu_version >= CRIS_CPU_SVINTO
	  && TARGET_SIDE_EFFECT_PREFIXES)
	fprintf (file, "\tmovem $%s,[$sp=$sp-%d]\n",
		 reg_names[last_movem_reg],
		 (last_movem_reg+1) * 4 + size);
      else
	{
	  /* Avoid printing multiple subsequent sub:s for sp.  FIXME:
	     Clean up the conditional expression.  */
	  fprintf (file, "\tsub%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER ((last_movem_reg + 1) * 4 + size),
		   (last_movem_reg + 1) * 4 + size);
	  /* To be compatible with v0..v3 means we do not use an assignment
	     addressing mode with movem.  We normally don't need that
	     anyway.  It would only be slightly more efficient for 64..128
	     bytes frame size.  */
	  fprintf (file, "\tmovem $%s,[$sp]\n", reg_names[last_movem_reg]);
	}

      framesize += (last_movem_reg + 1) * 4 + size;

      if (TARGET_PDEBUG)
	fprintf (file, "; frame %d, #regs %d, bytes %d args %d\n",
		 size,
		 last_movem_reg + 1,
		 (last_movem_reg + 1) * 4,
		 current_function_args_size);

      /* We have to put outgoing argument space after regs.  */
      if (cfoa_size)
	{
	  /* This does not need to be accounted for, for unwind.  */

	  fprintf (file, "\tSub%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER (cfoa_size),
		   cfoa_size);
	  framesize += cfoa_size;
	}
    }
  else if ((size + cfoa_size) > 0)
    {
      /* This does not need to be accounted for, for unwind.  */

      /* Local vars on stack, and we could not use movem.  Add a sub here.  */
      fprintf (file, "\tSub%s %d,$sp\n",
	       ADDITIVE_SIZE_MODIFIER (size + cfoa_size),
	       cfoa_size + size);
      framesize += size + cfoa_size;
    }

  /* Set up the PIC register.  */
  if (current_function_uses_pic_offset_table)
    fprintf (file, "\tmove.d $pc,$%s\n\tsub.d .:GOTOFF,$%s\n",
	     reg_names[PIC_OFFSET_TABLE_REGNUM],
	     reg_names[PIC_OFFSET_TABLE_REGNUM]);

  if (TARGET_PDEBUG)
    fprintf (file,
	     "; parm #%d @ %d; frame %d, FP-SP is %d; leaf: %s%s; fp %s, outg: %d arg %d\n",
	     CRIS_MAX_ARGS_IN_REGS + 1, FIRST_PARM_OFFSET (0),
	     get_frame_size (),
	     cris_initial_frame_pointer_offset (),
	     leaf_function_p () ? "yes" : "no",
	     return_address_on_stack ? "no" :"yes",
	     frame_pointer_needed ? "yes" : "no",
	     cfoa_size, current_function_args_size);

  if (cris_max_stackframe && framesize > cris_max_stackframe)
    warning ("stackframe too big: %d bytes", framesize);
}

/* Return nonzero if there are regs mentioned in the insn that are not all
   in the call_used regs.  This is part of the decision whether an insn
   can be put in the epilogue.  */

static int
saved_regs_mentioned (x)
     rtx x;
{
  int i;
  const char *fmt;
  RTX_CODE code;

  /* Mainly stolen from refers_to_regno_p in rtlanal.c.  */

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
      i = REGNO (x);
      return !call_used_regs[i];

    case SUBREG:
      /* If this is a SUBREG of a hard reg, we can see exactly which
	 registers are being modified.  Otherwise, handle normally.  */
      i = REGNO (SUBREG_REG (x));
      return !call_used_regs[i];

    default:
      ;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (saved_regs_mentioned (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (saved_regs_mentioned (XEXP (x, i)))
	      return 1;
	}
    }

  return 0;
}

/* Figure out if the insn may be put in the epilogue.  */

int
cris_eligible_for_epilogue_delay (insn)
     rtx insn;
{
  /* First of all, it must be as slottable as for a delayed branch insn.  */
  if (get_attr_slottable (insn) != SLOTTABLE_YES)
    return 0;

  /* It must not refer to the stack pointer (may be valid for some cases
     that I can't think of).  */
  if (reg_mentioned_p (stack_pointer_rtx, PATTERN (insn)))
    return 0;

  /* The frame pointer will be restored in the epilogue, before the
     "ret", so it can't be referred to.  */
  if (frame_pointer_needed
      && reg_mentioned_p (frame_pointer_rtx, PATTERN (insn)))
    return 0;

  /* All saved regs are restored before the delayed insn.
     This means that we cannot have any instructions that mention the
     registers that are restored by the epilogue.  */
  if (saved_regs_mentioned (PATTERN (insn)))
    return 0;

  /* It seems to be ok.  */
  return 1;
}

/* Return the number of delay-slots in the epilogue: return 1 if it
   contains "ret", else 0.  */

int
cris_delay_slots_for_epilogue ()
{
  /* Check if we use a return insn, which we only do for leaf functions.
     Else there is no slot to fill.  */
  if (regs_ever_live[CRIS_SRP_REGNUM]
      || cfun->machine->needs_return_address_on_stack != 0)
    return 0;

  /* By calling function_epilogue with the same parameters as from gcc
     we can get info about if the epilogue can fill the delay-slot by itself.
     If it is filled from the epilogue, then the corresponding string
     is in save_last.
      This depends on that the "size" argument to function_epilogue
     always is get_frame_size.
     FIXME:  Kludgy.  At least make it a separate function that is not
     misnamed or abuses the stream parameter.  */
  cris_target_asm_function_epilogue (NULL, get_frame_size ());

  if (*save_last)
    return 1;
  return 0;
}

/* Textual function epilogue.  When file is NULL, it serves doubly as
   a test for whether the epilogue can fill any "ret" delay-slots by
   itself by storing the delay insn in save_last.  */

static void
cris_target_asm_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int regno;
  int last_movem_reg = -1;
  rtx insn = get_last_insn ();
  int argspace_offset = current_function_outgoing_args_size;
  int pretend =	 current_function_pretend_args_size;
  int return_address_on_stack
    = regs_ever_live[CRIS_SRP_REGNUM]
    || cfun->machine->needs_return_address_on_stack != 0;

  save_last[0] = 0;

  if (file && !TARGET_PROLOGUE_EPILOGUE)
    return;

  if (TARGET_PDEBUG && file)
    fprintf (file, ";;\n");

  /* Align byte count of stack frame.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  /* If the last insn was a BARRIER, we don't have to write any code,
     then all returns were covered by "return" insns.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn
      && (GET_CODE (insn) == BARRIER
	  /* We must make sure that the insn really is a "return" and
	     not a conditional branch.  Try to match the return exactly,
	     and if it doesn't match, assume it is a conditional branch
	     (and output an epilogue).  */
	  || (GET_CODE (insn) == JUMP_INSN
	      && GET_CODE (PATTERN (insn)) == RETURN)))
    {
      if (TARGET_PDEBUG && file)
	fprintf (file, ";;;;;\n");
      return;
    }

  /* Check how many saved regs we can movem.  They start at r0 and must
     be contiguous.  */
  for (regno = 0;
       regno < FIRST_PSEUDO_REGISTER;
       regno++)
    if ((((regs_ever_live[regno]
	   && !call_used_regs[regno])
	  || (regno == (int) PIC_OFFSET_TABLE_REGNUM
	      && (current_function_uses_pic_offset_table
		  /* It is saved anyway, if there would be a gap.  */
		  || (flag_pic
		      && regs_ever_live[regno + 1]
		      && !call_used_regs[regno + 1]))))
	 && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regno != CRIS_SRP_REGNUM)
	|| (current_function_calls_eh_return
	    && (regno == EH_RETURN_DATA_REGNO (0)
		|| regno == EH_RETURN_DATA_REGNO (1)
		|| regno == EH_RETURN_DATA_REGNO (2)
		|| regno == EH_RETURN_DATA_REGNO (3))))

      {
	if (regno == last_movem_reg + 1)
	  last_movem_reg++;
	else
	  break;
      }

  for (regno = FIRST_PSEUDO_REGISTER - 1;
       regno > last_movem_reg;
       regno--)
    if ((((regs_ever_live[regno]
	   && !call_used_regs[regno])
	  || (regno == (int) PIC_OFFSET_TABLE_REGNUM
	      && (current_function_uses_pic_offset_table
		  /* It is saved anyway, if there would be a gap.  */
		  || (flag_pic
		      && regs_ever_live[regno + 1]
		      && !call_used_regs[regno + 1]))))
	 && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regno != CRIS_SRP_REGNUM)
	|| (current_function_calls_eh_return
	    && (regno == EH_RETURN_DATA_REGNO (0)
		|| regno == EH_RETURN_DATA_REGNO (1)
		|| regno == EH_RETURN_DATA_REGNO (2)
		|| regno == EH_RETURN_DATA_REGNO (3))))
      {
	if (argspace_offset)
	  {
	    /* There is an area for outgoing parameters located before
	       the saved registers.  We have to adjust for that.  */
	    if (file)
	      fprintf (file, "\tAdd%s %d,$sp\n",
		       ADDITIVE_SIZE_MODIFIER (argspace_offset),
		       argspace_offset);

	    /* Make sure we only do this once.  */
	    argspace_offset = 0;
	  }

	/* Flush previous non-movem:ed registers.  */
	if (*save_last && file)
	  fprintf (file, save_last);
	sprintf (save_last, "\tPop $%s\n", reg_names[regno]);
      }

  if (last_movem_reg != -1)
    {
      if (argspace_offset)
	{
	  /* Adjust for the outgoing parameters area, if that's not
	     handled yet.  */
	  if (*save_last && file)
	    {
	      fprintf (file, save_last);
	      *save_last = 0;
	    }

	  if (file)
	    fprintf (file, "\tAdd%s %d,$sp\n",
		     ADDITIVE_SIZE_MODIFIER (argspace_offset),
		     argspace_offset);
	  argspace_offset = 0;
	}
      /* Flush previous non-movem:ed registers.  */
      else if (*save_last && file)
	fprintf (file, save_last);
      sprintf (save_last, "\tmovem [$sp+],$%s\n", reg_names[last_movem_reg]);
    }

  /* Restore frame pointer if necessary.  */
  if (frame_pointer_needed)
    {
      if (*save_last && file)
	fprintf (file, save_last);

      if (file)
	fprintf (file, "\tmove.d $%s,$sp\n",
		 reg_names[FRAME_POINTER_REGNUM]);
      sprintf (save_last, "\tPop $%s\n",
	       reg_names[FRAME_POINTER_REGNUM]);
    }
  else
    {
      /* If there was no frame-pointer to restore sp from, we must
	 explicitly deallocate local variables.  */

      /* Handle space for outgoing parameters that hasn't been handled
	 yet.  */
      size += argspace_offset;

      if (size)
	{
	  if (*save_last && file)
	    fprintf (file, save_last);

	  sprintf (save_last, "\tadd%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER (size), size);
	}

      /* If the size was not in the range for a "quick", we must flush
	 it here.  */
      if (size > 63)
	{
	  if (file)
	    fprintf (file, save_last);
	  *save_last = 0;
	}
    }

  /* If this function has no pushed register parameters
     (stdargs/varargs), and if it is not a leaf function, then we can
     just jump-return here.  */
  if (return_address_on_stack && pretend == 0)
    {
      if (*save_last && file)
	fprintf (file, save_last);
      *save_last = 0;

      if (file)
	{
	  if (current_function_calls_eh_return)
	    {
	      /* The installed EH-return address is in *this* frame, so we
		 need to pop it before we return.  */
	      fprintf (file, "\tpop $srp\n");
	      fprintf (file, "\tret\n");
	      fprintf (file, "\tadd.d $%s,$sp\n", reg_names[CRIS_STACKADJ_REG]);
	    }
	  else
	    fprintf (file, "\tJump [$sp+]\n");

	  /* Do a sanity check to avoid generating invalid code.  */
	  if (current_function_epilogue_delay_list)
	    internal_error ("allocated but unused delay list in epilogue");
	}
      return;
    }

  /* Rather than add current_function_calls_eh_return conditions
     everywhere in the following code (and not be able to test it
     thoroughly), assert the assumption that all usage of
     __builtin_eh_return are handled above.  */
  if (current_function_calls_eh_return)
    internal_error ("unexpected function type needing stack adjustment for\
 __builtin_eh_return");

  /* If we pushed some register parameters, then adjust the stack for
     them.  */
  if (pretend)
    {
      /* Since srp is stored on the way, we need to restore it first.  */
      if (return_address_on_stack)
	{
	  if (*save_last && file)
	    fprintf (file, save_last);
	  *save_last = 0;

	  if (file)
	    fprintf (file, "\tpop $srp\n");
	}

      if (*save_last && file)
	fprintf (file, save_last);

      sprintf (save_last, "\tadd%s %d,$sp\n",
	       ADDITIVE_SIZE_MODIFIER (pretend), pretend);
    }

  /* Here's where we have a delay-slot we need to fill.  */
  if (file && current_function_epilogue_delay_list)
    {
      /* If gcc has allocated an insn for the epilogue delay slot, but
	 things were arranged so we now thought we could do it
	 ourselves, don't forget to flush that insn.  */
      if (*save_last)
	fprintf (file, save_last);

      fprintf (file, "\tRet\n");

      /* Output the delay-slot-insn the mandated way.  */
      final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
		       file, 1, -2, 1);
    }
  else if (file)
    {
      fprintf (file, "\tRet\n");

      /* If the GCC did not do it, we have to use whatever insn we have,
	 or a nop.  */
      if (*save_last)
	fprintf (file, save_last);
      else
	fprintf (file, "\tnOp\n");
    }
}

/* The PRINT_OPERAND worker.  */

void
cris_print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  rtx operand = x;

  /* Size-strings corresponding to MULT expressions.  */
  static const char *const mults[] = { "BAD:0", ".b", ".w", "BAD:3", ".d" };

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 'b':
      /* Print the unsigned supplied integer as if it was signed
	 and < 0, i.e print 255 or 65535 as -1, 254, 65534 as -2, etc.  */
      if (GET_CODE (x) != CONST_INT
	  || ! CONST_OK_FOR_LETTER_P (INTVAL (x), 'O'))
	LOSE_AND_RETURN ("invalid operand for 'b' modifier", x);
      fprintf (file, "%d", INTVAL (x)| (INTVAL (x) <= 255 ? ~255 : ~65535));
      return;

    case 'x':
      /* Print assembler code for operator.  */
      fprintf (file, "%s", cris_op_str (operand));
      return;

    case 'v':
      /* Print the operand without the PIC register.  */
      if (! flag_pic || ! CONSTANT_P (x) || ! cris_gotless_symbol (x))
	LOSE_AND_RETURN ("invalid operand for 'v' modifier", x);
      cris_pic_sympart_only++;
      cris_output_addr_const (file, x);
      cris_pic_sympart_only--;
      return;

    case 'P':
      /* Print the PIC register.  Applied to a GOT-less PIC symbol for
         sanity.  */
      if (! flag_pic || ! CONSTANT_P (x) || ! cris_gotless_symbol (x))
	LOSE_AND_RETURN ("invalid operand for 'P' modifier", x);
      fprintf (file, "$%s", reg_names [PIC_OFFSET_TABLE_REGNUM]);
      return;

    case 'p':
      /* Adjust a power of two to its log2.  */
      if (GET_CODE (x) != CONST_INT || exact_log2 (INTVAL (x)) < 0 )
	LOSE_AND_RETURN ("invalid operand for 'p' modifier", x);
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      return;

    case 's':
      /* For an integer, print 'b' or 'w' if <= 255 or <= 65535
	 respectively.  This modifier also terminates the inhibiting
         effects of the 'x' modifier.  */
      cris_output_insn_is_bound = 0;
      if (GET_MODE (x) == VOIDmode && GET_CODE (x) == CONST_INT)
	{
	  if (INTVAL (x) >= 0)
	    {
	      if (INTVAL (x) <= 255)
		putc ('b', file);
	      else if (INTVAL (x) <= 65535)
		putc ('w', file);
	      else
		putc ('d', file);
	    }
	  else
	    putc ('d', file);
	  return;
	}

      /* For a non-integer, print the size of the operand.  */
      putc ((GET_MODE (x) == SImode || GET_MODE (x) == SFmode)
	    ? 'd' : GET_MODE (x) == HImode ? 'w'
	    : GET_MODE (x) == QImode ? 'b'
	    /* If none of the above, emit an erroneous size letter.  */
	    : 'X',
	    file);
      return;

    case 'z':
      /* Const_int: print b for -127 <= x <= 255,
	 w for -32768 <= x <= 65535, else abort.  */
      if (GET_CODE (x) != CONST_INT
	  || INTVAL (x) < -32768 || INTVAL (x) > 65535)
	LOSE_AND_RETURN ("invalid operand for 'z' modifier", x);
      putc (INTVAL (x) >= -128 && INTVAL (x) <= 255 ? 'b' : 'w', file);
      return;

    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.
	 This method stolen from the sparc files.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;

    case 'H':
      /* Print high (most significant) part of something.  */
      switch (GET_CODE (operand))
	{
	case CONST_INT:
	  if (HOST_BITS_PER_WIDE_INT == 32)
	    /* Sign-extension from a normal int to a long long.  */
	    fprintf (file, INTVAL (operand) < 0 ? "-1" : "0");
	  else
	    fprintf (file, "0x%x", (unsigned int)(INTVAL (x) >> 31 >> 1));
	  return;

	case CONST_DOUBLE:
	  /* High part of a long long constant.  */
	  if (GET_MODE (operand) == VOIDmode)
	    {
	      fprintf (file, "0x%x", CONST_DOUBLE_HIGH (x));
	      return;
	    }
	  else
	    LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);

	case REG:
	  /* Print reg + 1.  Check that there's not an attempt to print
	     high-parts of registers like stack-pointer or higher.  */
	  if (REGNO (operand) > STACK_POINTER_REGNUM - 2)
	    LOSE_AND_RETURN ("bad register", operand);
	  fprintf (file, "$%s", reg_names[REGNO (operand) + 1]);
	  return;

	case MEM:
	  /* Adjust memory address to high part.  */
	  {
	    rtx adj_mem = operand;
	    int size
	      = GET_MODE_BITSIZE (GET_MODE (operand)) / BITS_PER_UNIT;

	    /* Adjust so we can use two SImode in DImode.
	       Calling adj_offsettable_operand will make sure it is an
	       offsettable address.  Don't do this for a postincrement
	       though; it should remain as it was.  */
	    if (GET_CODE (XEXP (adj_mem, 0)) != POST_INC)
	      adj_mem
		= adjust_address (adj_mem, GET_MODE (adj_mem), size / 2);

	    output_address (XEXP (adj_mem, 0));
	    return;
	  }

	default:
	  LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);
	}

    case 'L':
      /* Strip the MEM expression.  */
      operand = XEXP (operand, 0);
      break;

    case 'e':
      /* Print 's' if operand is SIGN_EXTEND or 'u' if ZERO_EXTEND unless
	 cris_output_insn_is_bound is nonzero.  */
      if (GET_CODE (operand) != SIGN_EXTEND
	  && GET_CODE (operand) != ZERO_EXTEND
	  && GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'e' modifier", x);

      if (cris_output_insn_is_bound)
	{
	  cris_output_insn_is_bound = 0;
	  return;
	}

      putc (GET_CODE (operand) == SIGN_EXTEND
	    || (GET_CODE (operand) == CONST_INT && INTVAL (operand) < 0)
	    ? 's' : 'u', file);
      return;

    case 'm':
      /* Print the size letter of the inner element.  We can do it by
	 calling ourselves with the 's' modifier.  */
      if (GET_CODE (operand) != SIGN_EXTEND && GET_CODE (operand) != ZERO_EXTEND)
	LOSE_AND_RETURN ("invalid operand for 'm' modifier", x);
      cris_print_operand (file, XEXP (operand, 0), 's');
      return;

    case 'M':
      /* Print the least significant part of operand.  */
      if (GET_CODE (operand) == CONST_DOUBLE)
	{
	  fprintf (file, "0x%x", CONST_DOUBLE_LOW (x));
	  return;
	}
      else if (HOST_BITS_PER_WIDE_INT > 32 && GET_CODE (operand) == CONST_INT)
	{
	  fprintf (file, "0x%x",
		   INTVAL (x) & ((unsigned int) 0x7fffffff * 2 + 1));
	  return;
	}
      /* Otherwise the least significant part equals the normal part,
	 so handle it normally.  */
      break;

    case 'A':
      /* When emitting an add for the high part of a DImode constant, we
	 want to use addq for 0 and adds.w for -1.  */
      if (GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'A' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "adds.w" : "addq");
      return;

    case 'D':
      /* When emitting an sub for the high part of a DImode constant, we
	 want to use subq for 0 and subs.w for -1.  */
      if (GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'D' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "subs.w" : "subq");
      return;

    case 'S':
      /* Print the operand as the index-part of an address.
	 Easiest way out is to use cris_print_index.  */
      cris_print_index (operand, file);
      return;

    case 'T':
      /* Print the size letter for an operand to a MULT, which must be a
	 const_int with a suitable value.  */
      if (GET_CODE (operand) != CONST_INT || INTVAL (operand) > 4)
	LOSE_AND_RETURN ("invalid operand for 'T' modifier", x);
      fprintf (file, "%s", mults[INTVAL (operand)]);
      return;

    case 0:
      /* No code, print as usual.  */
      break;

    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      if (REGNO (operand) > 15)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "$%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (XEXP (operand, 0));
      return;

    case CONST_DOUBLE:
      if (GET_MODE (operand) == VOIDmode)
	/* A long long constant.  */
	output_addr_const (file, operand);
      else
	{
	  /* Only single precision is allowed as plain operands the
	     moment.  FIXME:  REAL_VALUE_FROM_CONST_DOUBLE isn't
	     documented.  */
	  REAL_VALUE_TYPE r;
	  long l;

	  /* FIXME:  Perhaps check overflow of the "single".  */
	  REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	  REAL_VALUE_TO_TARGET_SINGLE (r, l);

	  fprintf (file, "0x%lx", l);
	}
      return;

    case UNSPEC:
      ASSERT_PLT_UNSPEC (operand);
      /* Fall through.  */

    case CONST:
      cris_output_addr_const (file, operand);
      return;

    case MULT:
    case ASHIFT:
      {
	/* For a (MULT (reg X) const_int) we output "rX.S".  */
	int i = GET_CODE (XEXP (operand, 1)) == CONST_INT
	  ? INTVAL (XEXP (operand, 1)) : INTVAL (XEXP (operand, 0));
	rtx reg = GET_CODE (XEXP (operand, 1)) == CONST_INT
	  ? XEXP (operand, 0) : XEXP (operand, 1);

	if (GET_CODE (reg) != REG
	    || (GET_CODE (XEXP (operand, 0)) != CONST_INT
		&& GET_CODE (XEXP (operand, 1)) != CONST_INT))
	  LOSE_AND_RETURN ("unexpected multiplicative operand", x);

	cris_print_base (reg, file);
	fprintf (file, ".%c",
		 i == 0 || (i == 1 && GET_CODE (operand) == MULT) ? 'b'
		 : i == 4 ? 'd'
		 : (i == 2 && GET_CODE (operand) == MULT) || i == 1 ? 'w'
		 : 'd');
	return;
      }

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  cris_output_addr_const (file, operand);
	  return;
	}

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
cris_print_operand_address (file, x)
     FILE *file;
     rtx x;
{
  /* All these were inside MEM:s so output indirection characters.  */
  putc ('[', file);

  if (CONSTANT_ADDRESS_P (x))
    cris_output_addr_const (file, x);
  else if (BASE_OR_AUTOINCR_P (x))
    cris_print_base (x, file);
  else if (GET_CODE (x) == PLUS)
    {
      rtx x1, x2;

      x1 = XEXP (x, 0);
      x2 = XEXP (x, 1);
      if (BASE_P (x1))
	{
	  cris_print_base (x1, file);
	  cris_print_index (x2, file);
	}
      else if (BASE_P (x2))
	{
	  cris_print_base (x2, file);
	  cris_print_index (x1, file);
	}
      else
	LOSE_AND_RETURN ("unrecognized address", x);
    }
  else if (GET_CODE (x) == MEM)
    {
      /* A DIP.  Output more indirection characters.  */
      putc ('[', file);
      cris_print_base (XEXP (x, 0), file);
      putc (']', file);
    }
  else
    LOSE_AND_RETURN ("unrecognized address", x);

  putc (']', file);
}

/* The RETURN_ADDR_RTX worker.
   We mark that the return address is used, either by EH or
   __builtin_return_address, for use by the function prologue and
   epilogue.  FIXME: This isn't optimal; we just use the mark in the
   prologue and epilogue to say that the return address is to be stored
   in the stack frame.  We could return SRP for leaf-functions and use the
   initial-value machinery.  */

rtx
cris_return_addr_rtx (count, frameaddr)
     int count;
     rtx frameaddr ATTRIBUTE_UNUSED;
{
  cfun->machine->needs_return_address_on_stack = 1;

  /* The return-address is stored just above the saved frame-pointer (if
     present).  Apparently we can't eliminate from the frame-pointer in
     that direction, so use the incoming args (maybe pretended) pointer.  */
  return count == 0
    ? gen_rtx_MEM (Pmode, plus_constant (virtual_incoming_args_rtx, -4))
    : NULL_RTX;
}

/* This used to be the INITIAL_FRAME_POINTER_OFFSET worker; now only
   handles FP -> SP elimination offset.  */

static int
cris_initial_frame_pointer_offset ()
{
  int regno;

  /* Initial offset is 0 if we don't have a frame pointer.  */
  int offs = 0;

  /* And 4 for each register pushed.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((((regs_ever_live[regno]
	   && !call_used_regs[regno])
	  || (regno == (int) PIC_OFFSET_TABLE_REGNUM
	      && (current_function_uses_pic_offset_table
		  /* It is saved anyway, if there would be a gap.  */
		  || (flag_pic
		      && regs_ever_live[regno + 1]
		      && !call_used_regs[regno + 1]))))
	 && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regno != CRIS_SRP_REGNUM)
	|| (current_function_calls_eh_return
	    && (regno == EH_RETURN_DATA_REGNO (0)
		|| regno == EH_RETURN_DATA_REGNO (1)
		|| regno == EH_RETURN_DATA_REGNO (2)
		|| regno == EH_RETURN_DATA_REGNO (3))))
      offs += 4;

  /* And then, last, we add the locals allocated.  */
  offs += get_frame_size ();

  /* And more; the accumulated args size.  */
  offs += current_function_outgoing_args_size;

  /* Then round it off, in case we use aligned stack.  */
  if (TARGET_STACK_ALIGN)
    offs = TARGET_ALIGN_BY_32 ? (offs + 3) & ~3 : (offs + 1) & ~1;

  return offs;
}

/* The INITIAL_ELIMINATION_OFFSET worker.
   Calculate the difference between imaginary registers such as frame
   pointer and the stack pointer.  Used to eliminate the frame pointer
   and imaginary arg pointer.  */

int
cris_initial_elimination_offset (fromreg, toreg)
     int fromreg;
     int toreg;
{
  int fp_sp_offset
    = cris_initial_frame_pointer_offset ();

  /* We should be able to use regs_ever_live and related prologue
     information here, or alpha should not as well.  */
  int return_address_on_stack
    = regs_ever_live[CRIS_SRP_REGNUM]
    || cfun->machine->needs_return_address_on_stack != 0;

  /* Here we act as if the frame-pointer is needed.  */
  int ap_fp_offset = 4 + (return_address_on_stack ? 4 : 0);

  if (fromreg == ARG_POINTER_REGNUM
      && toreg == FRAME_POINTER_REGNUM)
    return ap_fp_offset;

  /* Between the frame pointer and the stack are only "normal" stack
     variables and saved registers.  */
  if (fromreg == FRAME_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return fp_sp_offset;

  /* We need to balance out the frame pointer here.  */
  if (fromreg == ARG_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return ap_fp_offset + fp_sp_offset - 4;

  abort ();
}

/*  This function looks into the pattern to see how this insn affects
    condition codes.

    Used when to eliminate test insns before a condition-code user,
    such as a "scc" insn or a conditional branch.  This includes
    checking if the entities that cc was updated by, are changed by the
    operation.

    Currently a jumble of the old peek-inside-the-insn and the newer
    check-cc-attribute methods.  */

void
cris_notice_update_cc (exp, insn)
     rtx exp;
     rtx insn;
{
  /* Check if user specified "-mcc-init" as a bug-workaround.  FIXME:
     TARGET_CCINIT does not work; we must set CC_REVERSED as below.
     Several test-cases will otherwise fail, for example
     gcc.c-torture/execute/20000217-1.c -O0 and -O1.  */
  if (TARGET_CCINIT)
    {
      CC_STATUS_INIT;
      return;
    }

  /* Slowly, we're converting to using attributes to control the setting
     of condition-code status.  */
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Even if it is "none", a setting may clobber a previous
	 cc-value, so check.  */
      if (GET_CODE (exp) == SET)
	{
	  if (cc_status.value1
	      && cris_reg_overlap_mentioned_p (SET_DEST (exp),
					     cc_status.value1))
	    cc_status.value1 = 0;

	  if (cc_status.value2
	      && cris_reg_overlap_mentioned_p (SET_DEST (exp),
					     cc_status.value2))
	    cc_status.value2 = 0;
	}
      return;

    case CC_CLOBBER:
      CC_STATUS_INIT;
      break;

    case CC_NORMAL:
      /* Which means, for:
	 (set (cc0) (...)):
	 CC is (...).

	 (set (reg) (...)):
	 CC is (reg) and (...) - unless (...) is 0, then CC does not change.
	 CC_NO_OVERFLOW unless (...) is reg or mem.

	 (set (mem) (...)):
	 CC does not change.

	 (set (pc) (...)):
	 CC does not change.

	 (parallel
	  (set (reg1) (mem (bdap/biap)))
	  (set (reg2) (bdap/biap))):
	 CC is (reg1) and (mem (reg2))

	 (parallel
	  (set (mem (bdap/biap)) (reg1)) [or 0]
	  (set (reg2) (bdap/biap))):
	 CC does not change.

	 (where reg and mem includes strict_low_parts variants thereof)

	 For all others, assume CC is clobbered.
	 Note that we do not have to care about setting CC_NO_OVERFLOW,
	 since the overflow flag is set to 0 (i.e. right) for
	 instructions where it does not have any sane sense, but where
	 other flags have meanings.  (This includes shifts; the carry is
	 not set by them).

	 Note that there are other parallel constructs we could match,
	 but we don't do that yet.  */

      if (GET_CODE (exp) == SET)
	{
	  /* FIXME: Check when this happens.  It looks like we should
	     actually do a CC_STATUS_INIT here to be safe.  */
	  if (SET_DEST (exp) == pc_rtx)
	    return;

	  /* Record CC0 changes, so we do not have to output multiple
	     test insns.  */
	  if (SET_DEST (exp) == cc0_rtx)
	    {
	      cc_status.value1 = SET_SRC (exp);
	      cc_status.value2 = 0;

	      /* Handle flags for the special btstq on one bit.  */
	      if (GET_CODE (SET_SRC (exp)) == ZERO_EXTRACT
		  && XEXP (SET_SRC (exp), 1) == const1_rtx)
		{
		  if (GET_CODE (XEXP (SET_SRC (exp), 0)) == CONST_INT)
		    /* Using cmpq.  */
		    cc_status.flags = CC_INVERTED;
		  else
		    /* A one-bit btstq.  */
		    cc_status.flags = CC_Z_IN_NOT_N;
		}
	      else
		cc_status.flags = 0;

	      if (GET_CODE (SET_SRC (exp)) == COMPARE)
		{
		  if (!REG_P (XEXP (SET_SRC (exp), 0))
		      && XEXP (SET_SRC (exp), 1) != const0_rtx)
		    /* For some reason gcc will not canonicalize compare
		       operations, reversing the sign by itself if
		       operands are in wrong order.  */
		    /* (But NOT inverted; eq is still eq.) */
		    cc_status.flags = CC_REVERSED;

		  /* This seems to be overlooked by gcc.  FIXME: Check again.
		     FIXME:  Is it really safe?  */
		  cc_status.value2
		    = gen_rtx_MINUS (GET_MODE (SET_SRC (exp)),
				     XEXP (SET_SRC (exp), 0),
				     XEXP (SET_SRC (exp), 1));
		}
	      return;
	    }
	  else if (REG_P (SET_DEST (exp))
		   || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		       && REG_P (XEXP (SET_DEST (exp), 0))))
	    {
	      /* A register is set; normally CC is set to show that no
		 test insn is needed.  Catch the exceptions.  */

	      /* If not to cc0, then no "set"s in non-natural mode give
		 ok cc0...  */
	      if (GET_MODE_SIZE (GET_MODE (SET_DEST (exp))) > UNITS_PER_WORD
		  || GET_MODE_CLASS (GET_MODE (SET_DEST (exp))) == MODE_FLOAT)
		{
		  /* ... except add:s and sub:s in DImode.  */
		  if (GET_MODE (SET_DEST (exp)) == DImode
		      && (GET_CODE (SET_SRC (exp)) == PLUS
			  || GET_CODE (SET_SRC (exp)) == MINUS))
		    {
		      cc_status.flags = 0;
		      cc_status.value1 = SET_DEST (exp);
		      cc_status.value2 = SET_SRC (exp);

		      if (cris_reg_overlap_mentioned_p (cc_status.value1,
							cc_status.value2))
			cc_status.value2 = 0;

		      /* Add and sub may set V, which gets us
			 unoptimizable results in "gt" and "le" condition
			 codes.  */
		      cc_status.flags |= CC_NO_OVERFLOW;

		      return;
		    }
		}
	      else if (SET_SRC (exp) == const0_rtx)
		{
		  /* There's no CC0 change when clearing a register or
		     memory.  Just check for overlap.  */
		  if ((cc_status.value1
		       && cris_reg_overlap_mentioned_p (SET_DEST (exp),
							cc_status.value1)))
		    cc_status.value1 = 0;

		  if ((cc_status.value2
		       && cris_reg_overlap_mentioned_p (SET_DEST (exp),
							cc_status.value2)))
		    cc_status.value2 = 0;

		  return;
		}
	      else
		{
		  cc_status.flags = 0;
		  cc_status.value1 = SET_DEST (exp);
		  cc_status.value2 = SET_SRC (exp);

		  if (cris_reg_overlap_mentioned_p (cc_status.value1,
						    cc_status.value2))
		    cc_status.value2 = 0;

		  /* Some operations may set V, which gets us
		     unoptimizable results in "gt" and "le" condition
		     codes.  */
		  if (GET_CODE (SET_SRC (exp)) == PLUS
		      || GET_CODE (SET_SRC (exp)) == MINUS
		      || GET_CODE (SET_SRC (exp)) == NEG)
		    cc_status.flags |= CC_NO_OVERFLOW;

		  return;
		}
	    }
	  else if (GET_CODE (SET_DEST (exp)) == MEM
		   || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		       && GET_CODE (XEXP (SET_DEST (exp), 0)) == MEM))
	    {
	      /* When SET to MEM, then CC is not changed (except for
		 overlap).  */
	      if ((cc_status.value1
		   && cris_reg_overlap_mentioned_p (SET_DEST (exp),
						    cc_status.value1)))
		cc_status.value1 = 0;

	      if ((cc_status.value2
		   && cris_reg_overlap_mentioned_p (SET_DEST (exp),
						    cc_status.value2)))
		cc_status.value2 = 0;

	      return;
	    }
	}
      else if (GET_CODE (exp) == PARALLEL)
	{
	  if (GET_CODE (XVECEXP (exp, 0, 0)) == SET
	      && GET_CODE (XVECEXP (exp, 0, 1)) == SET
	      && REG_P (XEXP (XVECEXP (exp, 0, 1), 0)))
	    {
	      if (REG_P (XEXP (XVECEXP (exp, 0, 0), 0))
		  && GET_CODE (XEXP (XVECEXP (exp, 0, 0), 1)) == MEM)
		{
		  /* For "move.S [rx=ry+o],rz", say CC reflects
		     value1=rz and value2=[rx] */
		  cc_status.value1 = XEXP (XVECEXP (exp, 0, 0), 0);
		  cc_status.value2
		    = gen_rtx_MEM (GET_MODE (XEXP (XVECEXP (exp, 0, 0), 0)),
				   XEXP (XVECEXP (exp, 0, 1), 0));
		  cc_status.flags = 0;

		  /* Huh?  A side-effect cannot change the destination
		     register.  */
		  if (cris_reg_overlap_mentioned_p (cc_status.value1,
						    cc_status.value2))
		    internal_error ("internal error: sideeffect-insn affecting main effect");
		  return;
		}
	      else if ((REG_P (XEXP (XVECEXP (exp, 0, 0), 1))
			|| XEXP (XVECEXP (exp, 0, 0), 1) == const0_rtx)
		       && GET_CODE (XEXP (XVECEXP (exp, 0, 0), 0)) == MEM)
		{
		  /* For "move.S rz,[rx=ry+o]" and "clear.S [rx=ry+o]",
		     say flags are not changed, except for overlap.  */
		  if (cc_status.value1
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 0), 0),
						       cc_status.value1))
		    cc_status.value1 = 0;

		  if (cc_status.value1
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 1), 0),
						       cc_status.value1))
		    cc_status.value1 = 0;

		  if (cc_status.value2
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 0), 0),
						       cc_status.value2))
		    cc_status.value2 = 0;

		  if (cc_status.value2
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 1), 0),
						       cc_status.value2))
		    cc_status.value2 = 0;

		  return;
		}
	    }
	}
      break;

    default:
      /* Unknown cc_attr value.  */
      abort ();
    }

  CC_STATUS_INIT;
}

/* Return != 0 if the return sequence for the current function is short,
   like "ret" or "jump [sp+]".  Prior to reloading, we can't tell how
   many registers must be saved, so return 0 then.  */

int
cris_simple_epilogue ()
{
  int regno;
  int reglimit = STACK_POINTER_REGNUM;
  int lastreg = -1;

  if (! reload_completed
      || frame_pointer_needed
      || get_frame_size () != 0
      || current_function_pretend_args_size
      || current_function_args_size
      || current_function_outgoing_args_size
      || current_function_calls_eh_return

      /* If we're not supposed to emit prologue and epilogue, we must
	 not emit return-type instructions.  */
      || !TARGET_PROLOGUE_EPILOGUE)
    return 0;

  /* We allow a "movem [sp+],rN" to sit in front if the "jump [sp+]" or
     in the delay-slot of the "ret".  */
  for (regno = 0; regno < reglimit; regno++)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == (int) PIC_OFFSET_TABLE_REGNUM
	    && (current_function_uses_pic_offset_table
		/* It is saved anyway, if there would be a gap.  */
		|| (flag_pic
		    && regs_ever_live[regno + 1]
		    && !call_used_regs[regno + 1]))))
      {
	if (lastreg != regno - 1)
	  return 0;
	lastreg = regno;
      }

  return 1;
}

/* The ADDRESS_COST worker.  */

int
cris_address_cost (x)
     rtx x;
{
  /* The metric to use for the cost-macros is unclear.
     The metric used here is (the number of cycles needed) / 2,
     where we consider equal a cycle for a word of code and a cycle to
     read memory.  */

  /* The cheapest addressing modes get 0, since nothing extra is needed.  */
  if (BASE_OR_AUTOINCR_P (x))
    return 0;

  /* An indirect mem must be a DIP.  This means two bytes extra for code,
     and 4 bytes extra for memory read, i.e.  (2 + 4) / 2.  */
  if (GET_CODE (x) == MEM)
    return (2 + 4) / 2;

  /* Assume (2 + 4) / 2 for a single constant; a dword, since it needs
     an extra DIP prefix and 4 bytes of constant in most cases.
     For PIC and a symbol with a GOT entry, we double the cost since we
     add a [rPIC+...] offset.  A GOT-less symbol uses a BDAP prefix
     equivalent to the DIP prefix for non-PIC, hence the same cost.  */
  if (CONSTANT_P (x))
    return flag_pic && cris_got_symbol (x) ? 2 * (2 + 4) / 2 : (2 + 4) / 2;

  /* Handle BIAP and BDAP prefixes.  */
  if (GET_CODE (x) == PLUS)
    {
      rtx tem1 = XEXP (x, 0);
      rtx tem2 = XEXP (x, 1);

    /* A BIAP is 2 extra bytes for the prefix insn, nothing more.  We
       recognize the typical MULT which is always in tem1 because of
       insn canonicalization.  */
    if ((GET_CODE (tem1) == MULT && BIAP_INDEX_P (tem1))
	|| REG_P (tem1))
      return 2 / 2;

    /* A BDAP (quick) is 2 extra bytes.  Any constant operand to the
       PLUS is always found in tem2.  */
    if (GET_CODE (tem2) == CONST_INT
	&& INTVAL (tem2) < 128 && INTVAL (tem2) >= -128)
      return 2 / 2;

    /* A BDAP -32768 .. 32767 is like BDAP quick, but with 2 extra
       bytes.  */
    if (GET_CODE (tem2) == CONST_INT
	&& CONST_OK_FOR_LETTER_P (INTVAL (tem2), 'L'))
      return (2 + 2) / 2;

    /* A BDAP with some other constant is 2 bytes extra.  */
    if (CONSTANT_P (tem2))
      return (2 + 2 + 2) / 2;

    /* BDAP with something indirect should have a higher cost than
       BIAP with register.   FIXME: Should it cost like a MEM or more?  */
    /* Don't need to check it, it's the only one left.
       FIXME:  There was a REG test missing, perhaps there are others.
       Think more.  */
    return (2 + 2 + 2) / 2;
  }

  /* What else?  Return a high cost.  It matters only for valid
     addressing modes.  */
  return 10;
}

/* Check various objections to the side-effect.  Used in the test-part
   of an anonymous insn describing an insn with a possible side-effect.
   Returns nonzero if the implied side-effect is ok.

   code     : PLUS or MULT
   ops	    : An array of rtx:es. lreg, rreg, rval,
	      The variables multop and other_op are indexes into this,
	      or -1 if they are not applicable.
   lreg     : The register that gets assigned in the side-effect.
   rreg     : One register in the side-effect expression
   rval     : The other register, or an int.
   multop   : An integer to multiply rval with.
   other_op : One of the entities of the main effect,
	      whose mode we must consider.  */

int
cris_side_effect_mode_ok (code, ops, lreg, rreg, rval, multop, other_op)
     enum rtx_code code;
     rtx *ops;
     int lreg, rreg, rval, multop, other_op;
{
  /* Find what value to multiply with, for rx =ry + rz * n.  */
  int mult = multop < 0 ? 1 : INTVAL (ops[multop]);

  rtx reg_rtx = ops[rreg];
  rtx val_rtx = ops[rval];

  /* The operands may be swapped.  Canonicalize them in reg_rtx and
     val_rtx, where reg_rtx always is a reg (for this constraint to
     match).  */
  if (! BASE_P (reg_rtx))
    reg_rtx = val_rtx, val_rtx = ops[rreg];

  /* Don't forget to check that reg_rtx really is a reg.  If it isn't,
     we have no business.  */
  if (! BASE_P (reg_rtx))
    return 0;

  /* Don't do this when -mno-split.  */
  if (!TARGET_SIDE_EFFECT_PREFIXES)
    return 0;

  /* The mult expression may be hidden in lreg.  FIXME:  Add more
     commentary about that.  */
  if (GET_CODE (val_rtx) == MULT)
    {
      mult = INTVAL (XEXP (val_rtx, 1));
      val_rtx = XEXP (val_rtx, 0);
      code = MULT;
    }

  /* First check the "other operand".  */
  if (other_op >= 0)
    {
      if (GET_MODE_SIZE (GET_MODE (ops[other_op])) > UNITS_PER_WORD)
	return 0;

      /* Check if the lvalue register is the same as the "other
	 operand".  If so, the result is undefined and we shouldn't do
	 this.  FIXME:  Check again.  */
      if ((BASE_P (ops[lreg])
	   && BASE_P (ops[other_op])
	   && REGNO (ops[lreg]) == REGNO (ops[other_op]))
	  || rtx_equal_p (ops[other_op], ops[lreg]))
      return 0;
    }

  /* Do not accept frame_pointer_rtx as any operand.  */
  if (ops[lreg] == frame_pointer_rtx || ops[rreg] == frame_pointer_rtx
      || ops[rval] == frame_pointer_rtx
      || (other_op >= 0 && ops[other_op] == frame_pointer_rtx))
    return 0;

  if (code == PLUS
      && ! BASE_P (val_rtx))
    {

      /* Do not allow rx = rx + n if a normal add or sub with same size
	 would do.  */
      if (rtx_equal_p (ops[lreg], reg_rtx)
	  && GET_CODE (val_rtx) == CONST_INT
	  && (INTVAL (val_rtx) <= 63 && INTVAL (val_rtx) >= -63))
	return 0;

      /* Check allowed cases, like [r(+)?].[bwd] and const.
	 A symbol is not allowed with PIC.  */
      if (CONSTANT_P (val_rtx))
	return flag_pic == 0 || cris_symbol (val_rtx) == 0;

      if (GET_CODE (val_rtx) == MEM
	  && BASE_OR_AUTOINCR_P (XEXP (val_rtx, 0)))
	return 1;

      if (GET_CODE (val_rtx) == SIGN_EXTEND
	  && GET_CODE (XEXP (val_rtx, 0)) == MEM
	  && BASE_OR_AUTOINCR_P (XEXP (XEXP (val_rtx, 0), 0)))
	return 1;

      /* If we got here, it's not a valid addressing mode.  */
      return 0;
    }
  else if (code == MULT
	   || (code == PLUS && BASE_P (val_rtx)))
    {
      /* Do not allow rx = rx + ry.S, since it doesn't give better code.  */
      if (rtx_equal_p (ops[lreg], reg_rtx)
	  || (mult == 1 && rtx_equal_p (ops[lreg], val_rtx)))
	return 0;

      /* Do not allow bad multiply-values.  */
      if (mult != 1 && mult != 2 && mult != 4)
	return 0;

      /* Only allow  r + ...  */
      if (! BASE_P (reg_rtx))
	return 0;

      /* If we got here, all seems ok.
	 (All checks need to be done above).  */
      return 1;
    }

  /* If we get here, the caller got its initial tests wrong.  */
  internal_error ("internal error: cris_side_effect_mode_ok with bad operands");
}

/* The function reg_overlap_mentioned_p in CVS (still as of 2001-05-16)
   does not handle the case where the IN operand is strict_low_part; it
   does handle it for X.  Test-case in Axis-20010516.  This function takes
   care of that for THIS port.  FIXME: strict_low_part is going away
   anyway.  */

static int
cris_reg_overlap_mentioned_p (x, in)
     rtx x, in;
{
  /* The function reg_overlap_mentioned now handles when X is
     strict_low_part, but not when IN is a STRICT_LOW_PART.  */
  if (GET_CODE (in) == STRICT_LOW_PART)
    in = XEXP (in, 0);

  return reg_overlap_mentioned_p (x, in);
}

/* The TARGET_ASM_NAMED_SECTION worker.
   We just dispatch to the functions for ELF and a.out.  */

void
cris_target_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  if (! TARGET_ELF)
    default_no_named_section (name, flags);
  else
    default_elf_asm_named_section (name, flags);
}

/* The LEGITIMATE_PIC_OPERAND_P worker.  */

int
cris_legitimate_pic_operand (x)
     rtx x;
{
  /* The PIC representation of a symbol with a GOT entry will be (for
     example; relocations differ):
      sym => [rPIC+sym:GOT]
     and for a GOT-less symbol it will be (for example, relocation differ):
      sym => rPIC+sym:GOTOFF
     so only a symbol with a GOT is by itself a valid operand, and it
     can't be a sum of a symbol and an offset.  */
  return ! cris_symbol (x) || cris_got_symbol (x);
}

/* Return nonzero if there's a SYMBOL_REF or LABEL_REF hiding inside this
   CONSTANT_P.  */

int
cris_symbol (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case UNSPEC:
      /* A PLT reference.  */
      ASSERT_PLT_UNSPEC (x);
      return 1;

    case CONST:
      return cris_symbol (XEXP (x, 0));

    case PLUS:
    case MINUS:
      return cris_symbol (XEXP (x, 0)) || cris_symbol (XEXP (x, 1));

    case CONST_INT:
    case CONST_DOUBLE:
    case CONSTANT_P_RTX:
      return 0;

    default:
      fatal_insn ("unrecognized supposed constant", x);
    }

  return 1;
}

/* Return nonzero if there's a SYMBOL_REF or LABEL_REF hiding inside this
   CONSTANT_P, and the symbol does not need a GOT entry.  Also set
   current_function_uses_pic_offset_table if we're generating PIC and ever
   see something that would need one.  */

int
cris_gotless_symbol (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case UNSPEC:
      ASSERT_PLT_UNSPEC (x);
      return 1;

    case SYMBOL_REF:
      if (flag_pic && cfun != NULL)
	current_function_uses_pic_offset_table = 1;
      return SYMBOL_REF_FLAG (x);

    case LABEL_REF:
      /* We don't set current_function_uses_pic_offset_table for
	 LABEL_REF:s in here, since they are almost always originating
	 from some branch.  The only time it does not come from a label is
	 when GCC does something like __builtin_setjmp.  Then we get the
	 LABEL_REF from the movsi expander, so we mark it there as a
	 special case.  */
      return 1;

    case CONST:
      return cris_gotless_symbol (XEXP (x, 0));

    case PLUS:
    case MINUS:
      {
	int x0 = cris_gotless_symbol (XEXP (x, 0)) != 0;
	int x1 = cris_gotless_symbol (XEXP (x, 1)) != 0;

	/* One and only one of them must be a local symbol.  Neither must
	   be some other, more general kind of symbol.  */
	return
	  (x0 ^ x1)
	  && ! (x0 == 0 && cris_symbol (XEXP (x, 0)))
	  && ! (x1 == 0 && cris_symbol (XEXP (x, 1)));
      }

    case CONST_INT:
    case CONST_DOUBLE:
    case CONSTANT_P_RTX:
      return 0;

    default:
      fatal_insn ("unrecognized supposed constant", x);
    }

  return 1;
}

/* Return nonzero if there's a SYMBOL_REF or LABEL_REF hiding inside this
   CONSTANT_P, and the symbol needs a GOT entry.  */

int
cris_got_symbol (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case UNSPEC:
      ASSERT_PLT_UNSPEC (x);
      return 0;

    case SYMBOL_REF:
      if (flag_pic && cfun != NULL)
	current_function_uses_pic_offset_table = 1;
      return ! SYMBOL_REF_FLAG (x);

    case CONST:
      return cris_got_symbol (XEXP (x, 0));

    case LABEL_REF:
      /* A LABEL_REF is never visible as a symbol outside the local
         function.  */
    case PLUS:
    case MINUS:
      /* Nope, can't access the GOT for "symbol + offset".  */
      return 0;

    case CONST_INT:
    case CONST_DOUBLE:
    case CONSTANT_P_RTX:
      return 0;

    default:
      fatal_insn ("unrecognized supposed constant in cris_global_pic_symbol",
		  x);
    }

  return 1;
}

/* The OVERRIDE_OPTIONS worker.
   As is the norm, this also parses -mfoo=bar type parameters.  */

void
cris_override_options ()
{
  if (cris_max_stackframe_str)
    {
      cris_max_stackframe = atoi (cris_max_stackframe_str);

      /* Do some sanity checking.  */
      if (cris_max_stackframe < 0 || cris_max_stackframe > 0x20000000)
	internal_error ("-max-stackframe=%d is not usable, not between 0 and %d",
			cris_max_stackframe, 0x20000000);
    }

  /* Let "-metrax4" and "-metrax100" change the cpu version.  */
  if (TARGET_SVINTO && cris_cpu_version < CRIS_CPU_SVINTO)
    cris_cpu_version = CRIS_CPU_SVINTO;
  else if (TARGET_ETRAX4_ADD && cris_cpu_version < CRIS_CPU_ETRAX4)
    cris_cpu_version = CRIS_CPU_ETRAX4;

  /* Parse -march=... and its synonym, the deprecated -mcpu=...  */
  if (cris_cpu_str)
    {
      cris_cpu_version
	= (*cris_cpu_str == 'v' ? atoi (cris_cpu_str + 1) : -1);

      if (strcmp ("etrax4", cris_cpu_str) == 0)
	cris_cpu_version = 3;

      if (strcmp ("svinto", cris_cpu_str) == 0
	  || strcmp ("etrax100", cris_cpu_str) == 0)
	cris_cpu_version = 8;

      if (strcmp ("ng", cris_cpu_str) == 0
	  || strcmp ("etrax100lx", cris_cpu_str) == 0)
	cris_cpu_version = 10;

      if (cris_cpu_version < 0 || cris_cpu_version > 10)
	error ("unknown CRIS version specification in -march= or -mcpu= : %s",
	       cris_cpu_str);

      /* Set the target flags.  */
      if (cris_cpu_version >= CRIS_CPU_ETRAX4)
	target_flags |= TARGET_MASK_ETRAX4_ADD;

      /* If this is Svinto or higher, align for 32 bit accesses.  */
      if (cris_cpu_version >= CRIS_CPU_SVINTO)
	target_flags
	  |= (TARGET_MASK_SVINTO | TARGET_MASK_ALIGN_BY_32
	      | TARGET_MASK_STACK_ALIGN | TARGET_MASK_CONST_ALIGN
	      | TARGET_MASK_DATA_ALIGN);

      /* Note that we do not add new flags when it can be completely
	 described with a macro that uses -mcpu=X.  So
	 TARGET_HAS_MUL_INSNS is (cris_cpu_version >= CRIS_CPU_NG).  */
    }

  if (cris_tune_str)
    {
      int cris_tune
	= (*cris_tune_str == 'v' ? atoi (cris_tune_str + 1) : -1);

      if (strcmp ("etrax4", cris_tune_str) == 0)
	cris_tune = 3;

      if (strcmp ("svinto", cris_tune_str) == 0
	  || strcmp ("etrax100", cris_tune_str) == 0)
	cris_tune = 8;

      if (strcmp ("ng", cris_tune_str) == 0
	  || strcmp ("etrax100lx", cris_tune_str) == 0)
	cris_tune = 10;

      if (cris_tune < 0 || cris_tune > 10)
	error ("unknown CRIS cpu version specification in -mtune= : %s",
	       cris_tune_str);

      if (cris_tune >= CRIS_CPU_SVINTO)
	/* We have currently nothing more to tune than alignment for
	   memory accesses.  */
	target_flags
	  |= (TARGET_MASK_STACK_ALIGN | TARGET_MASK_CONST_ALIGN
	      | TARGET_MASK_DATA_ALIGN | TARGET_MASK_ALIGN_BY_32);
    }

  if (flag_pic)
    {
      /* Use error rather than warning, so invalid use is easily
	 detectable.  Still change to the values we expect, to avoid
	 further errors.  */
      if (! TARGET_LINUX)
	{
	  error ("-fPIC and -fpic are not supported in this configuration");
	  flag_pic = 0;
	}

      /* Turn off function CSE.  We need to have the addresses reach the
	 call expanders to get PLT-marked, as they could otherwise be
	 compared against zero directly or indirectly.  After visiting the
	 call expanders they will then be cse:ed, as the call expanders
	 force_reg the addresses, effectively forcing flag_no_function_cse
	 to 0.  */
      flag_no_function_cse = 1;
    }

  if ((write_symbols == DWARF_DEBUG
       || write_symbols == DWARF2_DEBUG) && ! TARGET_ELF)
    {
      warning ("that particular -g option is invalid with -maout and -melinux");
      write_symbols = DBX_DEBUG;
    }

  /* Set the per-function-data initializer.  */
  init_machine_status = cris_init_machine_status;
}

/* The TARGET_ASM_OUTPUT_MI_THUNK worker.  */

static void
cris_asm_output_mi_thunk (stream, thunkdecl, delta, vcall_offset, funcdecl)
     FILE *stream;
     tree thunkdecl ATTRIBUTE_UNUSED;
     HOST_WIDE_INT delta;
     HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED;
     tree funcdecl;
{
  if (delta > 0)
    {
      fprintf (stream, "\tadd%s ", ADDITIVE_SIZE_MODIFIER (delta));
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, delta);
      fprintf (stream, ",$%s\n", reg_names[CRIS_FIRST_ARG_REG]);
    }
  else if (delta < 0)
    {
      fprintf (stream, "\tsub%s ", ADDITIVE_SIZE_MODIFIER (-delta));
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, -delta);
      fprintf (stream, ",$%s\n", reg_names[CRIS_FIRST_ARG_REG]);
    }

  if (flag_pic)
    {
      const char *name = XSTR (XEXP (DECL_RTL (funcdecl), 0), 0);

      name = (* targetm.strip_name_encoding) (name);
      fprintf (stream, "add.d ");
      assemble_name (stream, name);
      fprintf (stream, "%s,$pc\n", CRIS_PLT_PCOFFSET_SUFFIX);
    }
  else
    {
      fprintf (stream, "jump ");
      assemble_name (stream, XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
      fprintf (stream, "\n");
    }
}

/* The EXPAND_BUILTIN_VA_ARG worker.  This is modified from the
   "standard" implementation of va_arg: read the value from the current
   address and increment by the size of one or two registers.  The
   important difference for CRIS is that if the type is
   pass-by-reference, then perform an indirection.  */

rtx
cris_expand_builtin_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree addr_tree, t;
  rtx addr;
  tree passed_size = size_zero_node;
  tree type_size = NULL;
  tree size3 = size_int (3);
  tree size4 = size_int (4);
  tree size8 = size_int (8);
  tree rounded_size;

  /* Get AP.  */
  addr_tree = valist;

  if (type == error_mark_node
      || (type_size = TYPE_SIZE_UNIT (TYPE_MAIN_VARIANT (type))) == NULL
      || TREE_OVERFLOW (type_size))
    /* Presumably an error; the size isn't computable.  A message has
       supposedly been emitted elsewhere.  */
    rounded_size = size_zero_node;
  else
    rounded_size
      = fold (build (MULT_EXPR, sizetype,
		     fold (build (TRUNC_DIV_EXPR, sizetype,
				  fold (build (PLUS_EXPR, sizetype,
					       type_size, size3)),
				  size4)),
		     size4));

  if (!integer_zerop (rounded_size))
    {
      /* Check if the type is passed by value or by reference.  Values up
	 to 8 bytes are passed by-value, padded to register-size (4
	 bytes).  Larger values and varying-size types are passed
	 by reference.  */
      passed_size
	= (!really_constant_p (type_size)
	   ? size4
	   : fold (build (COND_EXPR, sizetype,
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       size8)),
			  size4,
			  rounded_size)));

      addr_tree
	= (!really_constant_p (type_size)
	   ? build1 (INDIRECT_REF, build_pointer_type (type), addr_tree)
	   : fold (build (COND_EXPR, TREE_TYPE (addr_tree),
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       size8)),
			  build1 (INDIRECT_REF, build_pointer_type (type),
				  addr_tree),
			  addr_tree)));
    }

  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  if (!integer_zerop (rounded_size))
    {
      /* Compute new value for AP.  */
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
		 build (PLUS_EXPR, TREE_TYPE (valist), valist,
			passed_size));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return addr;
}

/* The INIT_EXPANDERS worker sets the per-function-data initializer and
   mark functions.  */

void
cris_init_expanders ()
{
  /* Nothing here at the moment.  */
}

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
cris_init_machine_status ()
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

/* Split a 2 word move (DI or presumably DF) into component parts.
   Originally a copy of gen_split_move_double in m32r.c.  */

rtx
cris_split_movdx (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

  /* We used to have to handle (SUBREG (MEM)) here, but that should no
     longer happen; after reload there are no SUBREGs any more, and we're
     only called after reload.  */
  if (GET_CODE (dest) == SUBREG || GET_CODE (src) == SUBREG)
    abort ();

  start_sequence ();
  if (GET_CODE (dest) == REG)
    {
      int dregno = REGNO (dest);

      /* Reg-to-reg copy.  */
      if (GET_CODE (src) == REG)
	{
	  int sregno = REGNO (src);

	  int reverse = (dregno == sregno + 1);

	  /* We normally copy the low-numbered register first.  However, if
	     the first register operand 0 is the same as the second register of
	     operand 1, we must copy in the opposite order.  */
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, reverse, TRUE, mode),
				  operand_subword (src, reverse, TRUE, mode)));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, !reverse, TRUE, mode),
				  operand_subword (src, !reverse, TRUE, mode)));
	}
      /* Constant-to-reg copy.  */
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
	{
	  rtx words[2];
	  split_double (src, &words[0], &words[1]);
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 0, TRUE, mode),
				  words[0]));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 1, TRUE, mode),
				  words[1]));
	}
      /* Mem-to-reg copy.  */
      else if (GET_CODE (src) == MEM)
	{
	  /* If the high-address word is used in the address, we must load it
	     last.  Otherwise, load it first.  */
	  rtx addr = XEXP (src, 0);
	  int reverse
	    = (refers_to_regno_p (dregno, dregno + 1, addr, NULL) != 0);

	  /* The original code imples that we can't do
	     move.x [rN+],rM  move.x [rN],rM+1
	     when rN is dead, because of REG_NOTES damage.  That is
	     consistent with what I've seen, so don't try it.

             We have two different cases here; if the addr is POST_INC,
             just pass it through, otherwise add constants.  */

          if (GET_CODE (addr) == POST_INC)
	    {
	      emit_insn (gen_rtx_SET (VOIDmode,
				      operand_subword (dest, 0, TRUE, mode),
				      change_address (src, SImode, addr)));
	      emit_insn (gen_rtx_SET (VOIDmode,
				      operand_subword (dest, 1, TRUE, mode),
				      change_address (src, SImode, addr)));
	    }
	  else
	    {
	      /* Make sure we don't get any other addresses with
		 embedded postincrements.  They should be stopped in
		 GO_IF_LEGITIMATE_ADDRESS, but we're here for your
		 safety.  */
	      if (side_effects_p (addr))
		fatal_insn ("unexpected side-effects in address", addr);

	      emit_insn (gen_rtx_SET
			 (VOIDmode,
			  operand_subword (dest, reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (addr,
					  reverse * UNITS_PER_WORD))));
	      emit_insn (gen_rtx_SET
			 (VOIDmode,
			  operand_subword (dest, ! reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (addr,
					  (! reverse) *
					  UNITS_PER_WORD))));
	    }
	}
      else
	abort ();
    }
  /* Reg-to-mem copy or clear mem.  */
  else if (GET_CODE (dest) == MEM
	   && (GET_CODE (src) == REG
	       || src == const0_rtx
	       || src == CONST0_RTX (DFmode)))
    {
      rtx addr = XEXP (dest, 0);

      if (GET_CODE (addr) == POST_INC)
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  change_address (dest, SImode, addr),
				  operand_subword (src, 0, TRUE, mode)));
	  emit_insn (gen_rtx_SET (VOIDmode,
				  change_address (dest, SImode, addr),
				  operand_subword (src, 1, TRUE, mode)));
	}
      else
	{
	  /* Make sure we don't get any other addresses with embedded
	     postincrements.  They should be stopped in
	     GO_IF_LEGITIMATE_ADDRESS, but we're here for your safety.  */
	  if (side_effects_p (addr))
	    fatal_insn ("unexpected side-effects in address", addr);

	  emit_insn (gen_rtx_SET
		     (VOIDmode,
		      change_address (dest, SImode, addr),
		      operand_subword (src, 0, TRUE, mode)));

	  emit_insn (gen_rtx_SET
		     (VOIDmode,
		      change_address (dest, SImode,
				      plus_constant (addr,
						     UNITS_PER_WORD)),
		      operand_subword (src, 1, TRUE, mode)));
	}
    }

  else
    abort ();

  val = get_insns ();
  end_sequence ();
  return val;
}

/* This is in essence a copy of output_addr_const altered to output
   symbolic operands as PIC.

   FIXME: Add hooks similar to ASM_OUTPUT_SYMBOL_REF to get this effect in
   the "real" output_addr_const.  All we need is one for LABEL_REF (and
   one for CODE_LABEL?).  */

void
cris_output_addr_const (file, x)
     FILE *file;
     rtx x;
{
  int is_plt = 0;

restart:
  switch (GET_CODE (x))
    {
    case UNSPEC:
      ASSERT_PLT_UNSPEC (x);
      x = XVECEXP (x, 0, 0);
      is_plt = 1;

      /* Fall through.  */
    case SYMBOL_REF:
      if (flag_pic)
	{
	  const char *origstr = XSTR (x, 0);
	  const char *str;

	  str = (* targetm.strip_name_encoding) (origstr);

	  if (is_plt)
	    {
	      if (cris_pic_sympart_only)
		{
		  assemble_name (file, str);
		  fprintf (file, ":PLTG");
		}
	      else
		{
		  if (TARGET_AVOID_GOTPLT)
		    /* We shouldn't get here.  */
		    abort ();

		  fprintf (file, "[$%s+", reg_names [PIC_OFFSET_TABLE_REGNUM]);
		  assemble_name (file, XSTR (x, 0));

		  if (flag_pic == 1)
		    fprintf (file, ":GOTPLT16]");
		  else
		    fprintf (file, ":GOTPLT]");
		}
	    }
	  else if (cris_gotless_symbol (x))
	    {
	      if (! cris_pic_sympart_only)
		fprintf (file, "$%s+", reg_names [PIC_OFFSET_TABLE_REGNUM]);
	      assemble_name (file, str);
	      fprintf (file, ":GOTOFF");
	    }
	  else if (cris_got_symbol (x))
	    {
	      if (cris_pic_sympart_only)
		abort ();
	      fprintf (file, "[$%s+", reg_names [PIC_OFFSET_TABLE_REGNUM]);
	      assemble_name (file, XSTR (x, 0));

	      if (flag_pic == 1)
		fprintf (file, ":GOT16]");
	      else
		fprintf (file, ":GOT]");
	    }
	  else
	    LOSE_AND_RETURN ("unexpected PIC symbol", x);

	  /* Sanity check.  */
	  if (! current_function_uses_pic_offset_table)
	    output_operand_lossage ("PIC register isn't set up");
	}
      else
	assemble_name (file, XSTR (x, 0));
      break;

    case LABEL_REF:
      /* If we get one of those here, it should be dressed as PIC.  Branch
	 labels are normally output with the 'l' specifier, which means it
	 will go directly to output_asm_label and not end up here.  */
      if (GET_CODE (XEXP (x, 0)) != CODE_LABEL
	  && (GET_CODE (XEXP (x, 0)) != NOTE
	      || NOTE_LINE_NUMBER (XEXP (x, 0)) != NOTE_INSN_DELETED_LABEL))
	fatal_insn ("unexpected address expression", x);

      if (flag_pic)
	{
	  if (cris_gotless_symbol (x))
	    {
	      if (! cris_pic_sympart_only)
		fprintf (file, "$%s+", reg_names [PIC_OFFSET_TABLE_REGNUM]);
	      cris_output_addr_const (file, XEXP (x, 0));

	      fprintf (file, ":GOTOFF");
	    }
	  else
	    /* Labels are never marked as global symbols.  */
	    fatal_insn ("unexpected PIC symbol", x);

	  /* Sanity check.  */
	  if (! current_function_uses_pic_offset_table)
	    internal_error ("emitting PIC operand, but PIC register isn't set up");
	  break;
	}

      output_addr_const (file, x);
      break;

    case NOTE:
      if (NOTE_LINE_NUMBER (x) != NOTE_INSN_DELETED_LABEL)
	fatal_insn ("unexpected NOTE as addr_const:", x);
    case CODE_LABEL:
    case CONST_INT:
    case CONST_DOUBLE:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
      output_addr_const (file, x);
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      cris_output_addr_const (file, XEXP (x, 0));
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  cris_output_addr_const (file, XEXP (x, 1));
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_addr_const (file, XEXP (x, 0));
	}
      else
	{
	  cris_output_addr_const (file, XEXP (x, 0));
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT
	      || INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  cris_output_addr_const (file, XEXP (x, 1));
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      cris_output_addr_const (file, XEXP (x, 0));
      fprintf (file, "-");
      if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	   && INTVAL (XEXP (x, 1)) < 0)
	  || GET_CODE (XEXP (x, 1)) != CONST_INT)
	{
	  fprintf (file, "%s", targetm.asm_out.open_paren);
	  cris_output_addr_const (file, XEXP (x, 1));
	  fprintf (file, "%s", targetm.asm_out.close_paren);
	}
      else
	output_addr_const (file, XEXP (x, 1));
      break;

    default:
      LOSE_AND_RETURN ("unexpected address expression", x);
    }
}

/* Code-in whether we can get away without a GOT entry (needed for
   externally visible objects but not for functions) into
   SYMBOL_REF_FLAG and add the PLT suffix for global functions.  */

static void
cris_encode_section_info (exp, first)
     tree exp;
     int first ATTRIBUTE_UNUSED;
{
  if (flag_pic)
    {
      rtx rtl = DECL_P (exp) ? DECL_RTL (exp) : TREE_CST_RTL (exp);

      if (GET_CODE (rtl) == MEM && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF)
	SYMBOL_REF_FLAG (XEXP (rtl, 0)) = (*targetm.binds_local_p) (exp);
    }
}

#if 0
/* Various small functions to replace macros.  Only called from a
   debugger.  They might collide with gcc functions or system functions,
   so only emit them when '#if 1' above.  */

enum rtx_code Get_code PARAMS ((rtx));

enum rtx_code
Get_code (x)
     rtx x;
{
  return GET_CODE (x);
}

const char *Get_mode PARAMS ((rtx));

const char *
Get_mode (x)
     rtx x;
{
  return GET_MODE_NAME (GET_MODE (x));
}

rtx Xexp PARAMS ((rtx, int));

rtx
Xexp (x, n)
     rtx x;
     int n;
{
  return XEXP (x, n);
}

rtx Xvecexp PARAMS ((rtx, int, int));

rtx
Xvecexp (x, n, m)
     rtx x;
     int n;
{
  return XVECEXP (x, n, m);
}

int Get_rtx_len PARAMS ((rtx));

int
Get_rtx_len (x)
     rtx x;
{
  return GET_RTX_LENGTH (GET_CODE (x));
}

/* Use upper-case to distinguish from local variables that are sometimes
   called next_insn and prev_insn.  */

rtx Next_insn PARAMS ((rtx));

rtx
Next_insn (insn)
     rtx insn;
{
  return NEXT_INSN (insn);
}

rtx Prev_insn PARAMS ((rtx));

rtx
Prev_insn (insn)
     rtx insn;
{
  return PREV_INSN (insn);
}
#endif

#include "gt-cris.h"

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
