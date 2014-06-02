/* Xstormy16 target functions.
   Copyright (C) 1997-2014 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "obstack.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "df.h"
#include "reload.h"
#include "builtins.h"

static rtx emit_addhi3_postreload (rtx, rtx, rtx);
static void xstormy16_asm_out_constructor (rtx, int);
static void xstormy16_asm_out_destructor (rtx, int);
static void xstormy16_asm_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
					   HOST_WIDE_INT, tree);

static void xstormy16_init_builtins (void);
static rtx xstormy16_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
static bool xstormy16_rtx_costs (rtx, int, int, int, int *, bool);
static int xstormy16_address_cost (rtx, enum machine_mode, addr_space_t, bool);
static bool xstormy16_return_in_memory (const_tree, const_tree);

static GTY(()) section *bss100_section;

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
xstormy16_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		     int opno ATTRIBUTE_UNUSED, int *total,
		     bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) < 16 && INTVAL (x) >= 0)
        *total = COSTS_N_INSNS (1) / 2;
      else if (INTVAL (x) < 256 && INTVAL (x) >= 0)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (2);
      return true;

    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      *total = COSTS_N_INSNS (2);
      return true;

    case MULT:
      *total = COSTS_N_INSNS (35 + 6);
      return true;
    case DIV:
      *total = COSTS_N_INSNS (51 - 6);
      return true;

    default:
      return false;
    }
}

static int
xstormy16_address_cost (rtx x, enum machine_mode mode ATTRIBUTE_UNUSED,
			addr_space_t as ATTRIBUTE_UNUSED,
			bool speed ATTRIBUTE_UNUSED)
{
  return (CONST_INT_P (x) ? 2
	  : GET_CODE (x) == PLUS ? 7
	  : 5);
}

/* Worker function for TARGET_MEMORY_MOVE_COST.  */

static int
xstormy16_memory_move_cost (enum machine_mode mode, reg_class_t rclass,
			    bool in)
{
  return (5 + memory_move_secondary_cost (mode, rclass, in));
}

/* Branches are handled as follows:

   1. HImode compare-and-branches.  The machine supports these
      natively, so the appropriate pattern is emitted directly.

   2. SImode EQ and NE.  These are emitted as pairs of HImode
      compare-and-branches.

   3. SImode LT, GE, LTU and GEU.  These are emitted as a sequence
      of a SImode subtract followed by a branch (not a compare-and-branch),
      like this:
      sub
      sbc
      blt

   4. SImode GT, LE, GTU, LEU.  These are emitted as a sequence like:
      sub
      sbc
      blt
      or
      bne.  */

/* Emit a branch of kind CODE to location LOC.  */

void
xstormy16_emit_cbranch (enum rtx_code code, rtx op0, rtx op1, rtx loc)
{
  rtx condition_rtx, loc_ref, branch, cy_clobber;
  rtvec vec;
  enum machine_mode mode;

  mode = GET_MODE (op0);
  gcc_assert (mode == HImode || mode == SImode);

  if (mode == SImode
      && (code == GT || code == LE || code == GTU || code == LEU))
    {
      int unsigned_p = (code == GTU || code == LEU);
      int gt_p = (code == GT || code == GTU);
      rtx lab = NULL_RTX;

      if (gt_p)
	lab = gen_label_rtx ();
      xstormy16_emit_cbranch (unsigned_p ? LTU : LT, op0, op1, gt_p ? lab : loc);
      /* This should be generated as a comparison against the temporary
	 created by the previous insn, but reload can't handle that.  */
      xstormy16_emit_cbranch (gt_p ? NE : EQ, op0, op1, loc);
      if (gt_p)
	emit_label (lab);
      return;
    }
  else if (mode == SImode
	   && (code == NE || code == EQ)
	   && op1 != const0_rtx)
    {
      rtx op0_word, op1_word;
      rtx lab = NULL_RTX;
      int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
      int i;

      if (code == EQ)
	lab = gen_label_rtx ();

      for (i = 0; i < num_words - 1; i++)
	{
	  op0_word = simplify_gen_subreg (word_mode, op0, mode,
					  i * UNITS_PER_WORD);
	  op1_word = simplify_gen_subreg (word_mode, op1, mode,
					  i * UNITS_PER_WORD);
	  xstormy16_emit_cbranch (NE, op0_word, op1_word, code == EQ ? lab : loc);
	}
      op0_word = simplify_gen_subreg (word_mode, op0, mode,
				      i * UNITS_PER_WORD);
      op1_word = simplify_gen_subreg (word_mode, op1, mode,
				      i * UNITS_PER_WORD);
      xstormy16_emit_cbranch (code, op0_word, op1_word, loc);

      if (code == EQ)
	emit_label (lab);
      return;
    }

  /* We can't allow reload to try to generate any reload after a branch,
     so when some register must match we must make the temporary ourselves.  */
  if (mode != HImode)
    {
      rtx tmp;
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, op0);
      op0 = tmp;
    }

  condition_rtx = gen_rtx_fmt_ee (code, mode, op0, op1);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, loc);
  branch = gen_rtx_SET (VOIDmode, pc_rtx,
			gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx,
					      loc_ref, pc_rtx));

  cy_clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, CARRY_REGNUM));

  if (mode == HImode)
    vec = gen_rtvec (2, branch, cy_clobber);
  else if (code == NE || code == EQ)
    vec = gen_rtvec (2, branch, gen_rtx_CLOBBER (VOIDmode, op0));
  else
    {
      rtx sub;
#if 0
      sub = gen_rtx_SET (VOIDmode, op0, gen_rtx_MINUS (SImode, op0, op1));
#else
      sub = gen_rtx_CLOBBER (SImode, op0);
#endif
      vec = gen_rtvec (3, branch, sub, cy_clobber);
    }

  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, vec));
}

/* Take a SImode conditional branch, one of GT/LE/GTU/LEU, and split
   the arithmetic operation.  Most of the work is done by
   xstormy16_expand_arith.  */

void
xstormy16_split_cbranch (enum machine_mode mode, rtx label, rtx comparison,
			 rtx dest)
{
  rtx op0 = XEXP (comparison, 0);
  rtx op1 = XEXP (comparison, 1);
  rtx seq, last_insn;
  rtx compare;

  start_sequence ();
  xstormy16_expand_arith (mode, COMPARE, dest, op0, op1);
  seq = get_insns ();
  end_sequence ();

  gcc_assert (INSN_P (seq));

  last_insn = seq;
  while (NEXT_INSN (last_insn) != NULL_RTX)
    last_insn = NEXT_INSN (last_insn);

  compare = SET_SRC (XVECEXP (PATTERN (last_insn), 0, 0));
  PUT_CODE (XEXP (compare, 0), GET_CODE (comparison));
  XEXP (compare, 1) = gen_rtx_LABEL_REF (VOIDmode, label);
  emit_insn (seq);
}


/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.

   OP is the conditional expression, or NULL for branch-always.

   REVERSED is nonzero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
xstormy16_output_cbranch_hi (rtx op, const char *label, int reversed, rtx insn)
{
  static char string[64];
  int need_longbranch = (op != NULL_RTX
			 ? get_attr_length (insn) == 8
			 : get_attr_length (insn) == 4);
  int really_reversed = reversed ^ need_longbranch;
  const char *ccode;
  const char *templ;
  const char *operands;
  enum rtx_code code;

  if (! op)
    {
      if (need_longbranch)
	ccode = "jmpf";
      else
	ccode = "br";
      sprintf (string, "%s %s", ccode, label);
      return string;
    }

  code = GET_CODE (op);

  if (! REG_P (XEXP (op, 0)))
    {
      code = swap_condition (code);
      operands = "%3,%2";
    }
  else
      operands = "%2,%3";

  /* Work out which way this really branches.  */
  if (really_reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:   ccode = "z";   break;
    case NE:   ccode = "nz";  break;
    case GE:   ccode = "ge";  break;
    case LT:   ccode = "lt";  break;
    case GT:   ccode = "gt";  break;
    case LE:   ccode = "le";  break;
    case GEU:  ccode = "nc";  break;
    case LTU:  ccode = "c";   break;
    case GTU:  ccode = "hi";  break;
    case LEU:  ccode = "ls";  break;

    default:
      gcc_unreachable ();
    }

  if (need_longbranch)
    templ = "b%s %s,.+8 | jmpf %s";
  else
    templ = "b%s %s,%s";
  sprintf (string, templ, ccode, operands, label);

  return string;
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label, but suitable for the tail of a
   SImode branch.

   OP is the conditional expression (OP is never NULL_RTX).

   REVERSED is nonzero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
xstormy16_output_cbranch_si (rtx op, const char *label, int reversed, rtx insn)
{
  static char string[64];
  int need_longbranch = get_attr_length (insn) >= 8;
  int really_reversed = reversed ^ need_longbranch;
  const char *ccode;
  const char *templ;
  char prevop[16];
  enum rtx_code code;

  code = GET_CODE (op);

  /* Work out which way this really branches.  */
  if (really_reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:   ccode = "z";   break;
    case NE:   ccode = "nz";  break;
    case GE:   ccode = "ge";  break;
    case LT:   ccode = "lt";  break;
    case GEU:  ccode = "nc";  break;
    case LTU:  ccode = "c";   break;

      /* The missing codes above should never be generated.  */
    default:
      gcc_unreachable ();
    }

  switch (code)
    {
    case EQ: case NE:
      {
	int regnum;

	gcc_assert (REG_P (XEXP (op, 0)));

	regnum = REGNO (XEXP (op, 0));
	sprintf (prevop, "or %s,%s", reg_names[regnum], reg_names[regnum+1]);
      }
      break;

    case GE: case LT: case GEU: case LTU:
      strcpy (prevop, "sbc %2,%3");
      break;

    default:
      gcc_unreachable ();
    }

  if (need_longbranch)
    templ = "%s | b%s .+6 | jmpf %s";
  else
    templ = "%s | b%s %s";
  sprintf (string, templ, prevop, ccode, label);

  return string;
}

/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the SPARC when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   RCLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register RCLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of RCLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required..  These patterns, which will normally be implemented with a
   `define_expand', should be similar to the `movM' patterns, except that
   operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   RCLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

enum reg_class
xstormy16_secondary_reload_class (enum reg_class rclass,
				  enum machine_mode mode ATTRIBUTE_UNUSED,
				  rtx x)
{
  /* This chip has the interesting property that only the first eight
     registers can be moved to/from memory.  */
  if ((MEM_P (x)
       || ((GET_CODE (x) == SUBREG || REG_P (x))
	   && (true_regnum (x) == -1
	       || true_regnum (x) >= FIRST_PSEUDO_REGISTER)))
      && ! reg_class_subset_p (rclass, EIGHT_REGS))
    return EIGHT_REGS;

  return NO_REGS;
}

/* Worker function for TARGET_PREFERRED_RELOAD_CLASS
   and TARGET_PREFERRED_OUTPUT_RELOAD_CLASS.  */

static reg_class_t
xstormy16_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (rclass == GENERAL_REGS && MEM_P (x))
    return EIGHT_REGS;

  return rclass;
}

/* Predicate for symbols and addresses that reflect special 8-bit
   addressing.  */

int
xstormy16_below100_symbol (rtx x,
			   enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    x = XEXP (x, 0);

  if (GET_CODE (x) == SYMBOL_REF)
    return (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_XSTORMY16_BELOW100) != 0;

  if (CONST_INT_P (x))
    {
      HOST_WIDE_INT i = INTVAL (x);

      if ((i >= 0x0000 && i <= 0x00ff)
	  || (i >= 0x7f00 && i <= 0x7fff))
	return 1;
    }
  return 0;
}

/* Likewise, but only for non-volatile MEMs, for patterns where the
   MEM will get split into smaller sized accesses.  */

int
xstormy16_splittable_below100_operand (rtx x, enum machine_mode mode)
{
  if (MEM_P (x) && MEM_VOLATILE_P (x))
    return 0;
  return xstormy16_below100_operand (x, mode);
}

/* Expand an 8-bit IOR.  This either detects the one case we can
   actually do, or uses a 16-bit IOR.  */

void
xstormy16_expand_iorqi3 (rtx *operands)
{
  rtx in, out, outsub, val;

  out = operands[0];
  in = operands[1];
  val = operands[2];

  if (xstormy16_onebit_set_operand (val, QImode))
    {
      if (!xstormy16_below100_or_register (in, QImode))
	in = copy_to_mode_reg (QImode, in);
      if (!xstormy16_below100_or_register (out, QImode))
	out = gen_reg_rtx (QImode);
      emit_insn (gen_iorqi3_internal (out, in, val));
      if (out != operands[0])
	emit_move_insn (operands[0], out);
      return;
    }

  if (! REG_P (in))
    in = copy_to_mode_reg (QImode, in);

  if (! REG_P (val) && ! CONST_INT_P (val))
    val = copy_to_mode_reg (QImode, val);

  if (! REG_P (out))
    out = gen_reg_rtx (QImode);

  in = simplify_gen_subreg (HImode, in, QImode, 0);
  outsub = simplify_gen_subreg (HImode, out, QImode, 0);

  if (! CONST_INT_P (val))
    val = simplify_gen_subreg (HImode, val, QImode, 0);

  emit_insn (gen_iorhi3 (outsub, in, val));

  if (out != operands[0])
    emit_move_insn (operands[0], out);
}

/* Expand an 8-bit AND.  This either detects the one case we can
   actually do, or uses a 16-bit AND.  */

void
xstormy16_expand_andqi3 (rtx *operands)
{
  rtx in, out, outsub, val;

  out = operands[0];
  in = operands[1];
  val = operands[2];

  if (xstormy16_onebit_clr_operand (val, QImode))
    {
      if (!xstormy16_below100_or_register (in, QImode))
	in = copy_to_mode_reg (QImode, in);
      if (!xstormy16_below100_or_register (out, QImode))
	out = gen_reg_rtx (QImode);
      emit_insn (gen_andqi3_internal (out, in, val));
      if (out != operands[0])
	emit_move_insn (operands[0], out);
      return;
    }

  if (! REG_P (in))
    in = copy_to_mode_reg (QImode, in);

  if (! REG_P (val) && ! CONST_INT_P (val))
    val = copy_to_mode_reg (QImode, val);

  if (! REG_P (out))
    out = gen_reg_rtx (QImode);

  in = simplify_gen_subreg (HImode, in, QImode, 0);
  outsub = simplify_gen_subreg (HImode, out, QImode, 0);

  if (! CONST_INT_P (val))
    val = simplify_gen_subreg (HImode, val, QImode, 0);

  emit_insn (gen_andhi3 (outsub, in, val));

  if (out != operands[0])
    emit_move_insn (operands[0], out);
}

#define LEGITIMATE_ADDRESS_INTEGER_P(X, OFFSET)				\
  (CONST_INT_P (X)							\
  && (unsigned HOST_WIDE_INT) (INTVAL (X) + (OFFSET) + 2048) < 4096)

#define LEGITIMATE_ADDRESS_CONST_INT_P(X, OFFSET)			 \
 (CONST_INT_P (X)							 \
  && INTVAL (X) + (OFFSET) >= 0						 \
  && INTVAL (X) + (OFFSET) < 0x8000					 \
  && (INTVAL (X) + (OFFSET) < 0x100 || INTVAL (X) + (OFFSET) >= 0x7F00))

bool
xstormy16_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
				rtx x, bool strict)
{
  if (LEGITIMATE_ADDRESS_CONST_INT_P (x, 0))
    return true;

  if (GET_CODE (x) == PLUS
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 0))
    {
      x = XEXP (x, 0);
      /* PR 31232: Do not allow INT+INT as an address.  */
      if (CONST_INT_P (x))
	return false;
    }

  if ((GET_CODE (x) == PRE_MODIFY && CONST_INT_P (XEXP (XEXP (x, 1), 1)))
      || GET_CODE (x) == POST_INC
      || GET_CODE (x) == PRE_DEC)
    x = XEXP (x, 0);

  if (REG_P (x)
      && REGNO_OK_FOR_BASE_P (REGNO (x))
      && (! strict || REGNO (x) < FIRST_PSEUDO_REGISTER))
    return true;

  if (xstormy16_below100_symbol (x, mode))
    return true;

  return false;
}

/* Worker function for TARGET_MODE_DEPENDENT_ADDRESS_P.

   On this chip, this is true if the address is valid with an offset
   of 0 but not of 6, because in that case it cannot be used as an
   address for DImode or DFmode, or if the address is a post-increment
   or pre-decrement address.  */

static bool
xstormy16_mode_dependent_address_p (const_rtx x,
				    addr_space_t as ATTRIBUTE_UNUSED)
{
  if (LEGITIMATE_ADDRESS_CONST_INT_P (x, 0)
      && ! LEGITIMATE_ADDRESS_CONST_INT_P (x, 6))
    return true;

  if (GET_CODE (x) == PLUS
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 0)
      && ! LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 6))
    return true;

  /* Auto-increment addresses are now treated generically in recog.c.  */
  return false;
}

int
short_memory_operand (rtx x, enum machine_mode mode)
{
  if (! memory_operand (x, mode))
    return 0;
  return (GET_CODE (XEXP (x, 0)) != PLUS);
}

/* Splitter for the 'move' patterns, for modes not directly implemented
   by hardware.  Emit insns to copy a value of mode MODE from SRC to
   DEST.

   This function is only called when reload_completed.  */

void
xstormy16_split_move (enum machine_mode mode, rtx dest, rtx src)
{
  int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
  int direction, end, i;
  int src_modifies = 0;
  int dest_modifies = 0;
  int src_volatile = 0;
  int dest_volatile = 0;
  rtx mem_operand;
  rtx auto_inc_reg_rtx = NULL_RTX;

  /* Check initial conditions.  */
  gcc_assert (reload_completed
	      && mode != QImode && mode != HImode
	      && nonimmediate_operand (dest, mode)
	      && general_operand (src, mode));

  /* This case is not supported below, and shouldn't be generated.  */
  gcc_assert (! MEM_P (dest) || ! MEM_P (src));

  /* This case is very very bad after reload, so trap it now.  */
  gcc_assert (GET_CODE (dest) != SUBREG && GET_CODE (src) != SUBREG);

  /* The general idea is to copy by words, offsetting the source and
     destination.  Normally the least-significant word will be copied
     first, but for pre-dec operations it's better to copy the
     most-significant word first.  Only one operand can be a pre-dec
     or post-inc operand.

     It's also possible that the copy overlaps so that the direction
     must be reversed.  */
  direction = 1;

  if (MEM_P (dest))
    {
      mem_operand = XEXP (dest, 0);
      dest_modifies = side_effects_p (mem_operand);
      if (auto_inc_p (mem_operand))
        auto_inc_reg_rtx = XEXP (mem_operand, 0);
      dest_volatile = MEM_VOLATILE_P (dest);
      if (dest_volatile)
	{
	  dest = copy_rtx (dest);
	  MEM_VOLATILE_P (dest) = 0;
	}
    }
  else if (MEM_P (src))
    {
      mem_operand = XEXP (src, 0);
      src_modifies = side_effects_p (mem_operand);
      if (auto_inc_p (mem_operand))
        auto_inc_reg_rtx = XEXP (mem_operand, 0);
      src_volatile = MEM_VOLATILE_P (src);
      if (src_volatile)
	{
	  src = copy_rtx (src);
	  MEM_VOLATILE_P (src) = 0;
	}
    }
  else
    mem_operand = NULL_RTX;

  if (mem_operand == NULL_RTX)
    {
      if (REG_P (src)
	  && REG_P (dest)
	  && reg_overlap_mentioned_p (dest, src)
	  && REGNO (dest) > REGNO (src))
	direction = -1;
    }
  else if (GET_CODE (mem_operand) == PRE_DEC
      || (GET_CODE (mem_operand) == PLUS
	  && GET_CODE (XEXP (mem_operand, 0)) == PRE_DEC))
    direction = -1;
  else if (MEM_P (src) && reg_overlap_mentioned_p (dest, src))
    {
      int regno;

      gcc_assert (REG_P (dest));
      regno = REGNO (dest);

      gcc_assert (refers_to_regno_p (regno, regno + num_words,
				     mem_operand, 0));

      if (refers_to_regno_p (regno, regno + 1, mem_operand, 0))
	direction = -1;
      else if (refers_to_regno_p (regno + num_words - 1, regno + num_words,
				  mem_operand, 0))
	direction = 1;
      else
	/* This means something like
	   (set (reg:DI r0) (mem:DI (reg:HI r1)))
	   which we'd need to support by doing the set of the second word
	   last.  */
	gcc_unreachable ();
    }

  end = direction < 0 ? -1 : num_words;
  for (i = direction < 0 ? num_words - 1 : 0; i != end; i += direction)
    {
      rtx w_src, w_dest, insn;

      if (src_modifies)
	w_src = gen_rtx_MEM (word_mode, mem_operand);
      else
	w_src = simplify_gen_subreg (word_mode, src, mode, i * UNITS_PER_WORD);
      if (src_volatile)
	MEM_VOLATILE_P (w_src) = 1;
      if (dest_modifies)
	w_dest = gen_rtx_MEM (word_mode, mem_operand);
      else
	w_dest = simplify_gen_subreg (word_mode, dest, mode,
				      i * UNITS_PER_WORD);
      if (dest_volatile)
	MEM_VOLATILE_P (w_dest) = 1;

      /* The simplify_subreg calls must always be able to simplify.  */
      gcc_assert (GET_CODE (w_src) != SUBREG
		  && GET_CODE (w_dest) != SUBREG);

      insn = emit_insn (gen_rtx_SET (VOIDmode, w_dest, w_src));
      if (auto_inc_reg_rtx)
        REG_NOTES (insn) = alloc_EXPR_LIST (REG_INC,
                                            auto_inc_reg_rtx,
					    REG_NOTES (insn));
    }
}

/* Expander for the 'move' patterns.  Emit insns to copy a value of
   mode MODE from SRC to DEST.  */

void
xstormy16_expand_move (enum machine_mode mode, rtx dest, rtx src)
{
  if (MEM_P (dest) && (GET_CODE (XEXP (dest, 0)) == PRE_MODIFY))
    {
      rtx pmv      = XEXP (dest, 0);
      rtx dest_reg = XEXP (pmv, 0);
      rtx dest_mod = XEXP (pmv, 1);
      rtx set      = gen_rtx_SET (Pmode, dest_reg, dest_mod);
      rtx clobber  = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, CARRY_REGNUM));

      dest = gen_rtx_MEM (mode, dest_reg);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
    }
  else if (MEM_P (src) && (GET_CODE (XEXP (src, 0)) == PRE_MODIFY))
    {
      rtx pmv     = XEXP (src, 0);
      rtx src_reg = XEXP (pmv, 0);
      rtx src_mod = XEXP (pmv, 1);
      rtx set     = gen_rtx_SET (Pmode, src_reg, src_mod);
      rtx clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, CARRY_REGNUM));

      src = gen_rtx_MEM (mode, src_reg);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
    }

  /* There are only limited immediate-to-memory move instructions.  */
  if (! reload_in_progress
      && ! reload_completed
      && MEM_P (dest)
      && (! CONST_INT_P (XEXP (dest, 0))
	  || ! xstormy16_legitimate_address_p (mode, XEXP (dest, 0), 0))
      && ! xstormy16_below100_operand (dest, mode)
      && ! REG_P (src)
      && GET_CODE (src) != SUBREG)
    src = copy_to_mode_reg (mode, src);

  /* Don't emit something we would immediately split.  */
  if (reload_completed
      && mode != HImode && mode != QImode)
    {
      xstormy16_split_move (mode, dest, src);
      return;
    }

  emit_insn (gen_rtx_SET (VOIDmode, dest, src));
}

/* Stack Layout:

   The stack is laid out as follows:

SP->
FP->	Local variables
	Register save area (up to 4 words)
	Argument register save area for stdarg (NUM_ARGUMENT_REGISTERS words)

AP->	Return address (two words)
	9th procedure parameter word
	10th procedure parameter word
	...
	last procedure parameter word

  The frame pointer location is tuned to make it most likely that all
  parameters and local variables can be accessed using a load-indexed
  instruction.  */

/* A structure to describe the layout.  */
struct xstormy16_stack_layout
{
  /* Size of the topmost three items on the stack.  */
  int locals_size;
  int register_save_size;
  int stdarg_save_size;
  /* Sum of the above items.  */
  int frame_size;
  /* Various offsets.  */
  int first_local_minus_ap;
  int sp_minus_fp;
  int fp_minus_ap;
};

/* Does REGNO need to be saved?  */
#define REG_NEEDS_SAVE(REGNUM, IFUN)					\
  ((df_regs_ever_live_p (REGNUM) && ! call_used_regs[REGNUM])		\
   || (IFUN && ! fixed_regs[REGNUM] && call_used_regs[REGNUM]		\
       && (REGNUM != CARRY_REGNUM)					\
       && (df_regs_ever_live_p (REGNUM) || ! crtl->is_leaf)))

/* Compute the stack layout.  */

struct xstormy16_stack_layout
xstormy16_compute_stack_layout (void)
{
  struct xstormy16_stack_layout layout;
  int regno;
  const int ifun = xstormy16_interrupt_function_p ();

  layout.locals_size = get_frame_size ();

  layout.register_save_size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (REG_NEEDS_SAVE (regno, ifun))
      layout.register_save_size += UNITS_PER_WORD;

  if (cfun->stdarg)
    layout.stdarg_save_size = NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD;
  else
    layout.stdarg_save_size = 0;

  layout.frame_size = (layout.locals_size
		       + layout.register_save_size
		       + layout.stdarg_save_size);

  if (crtl->args.size <= 2048 && crtl->args.size != -1)
    {
      if (layout.frame_size - INCOMING_FRAME_SP_OFFSET
	  + crtl->args.size <= 2048)
	layout.fp_minus_ap = layout.frame_size - INCOMING_FRAME_SP_OFFSET;
      else
	layout.fp_minus_ap = 2048 - crtl->args.size;
    }
  else
    layout.fp_minus_ap = (layout.stdarg_save_size
			  + layout.register_save_size
			  - INCOMING_FRAME_SP_OFFSET);
  layout.sp_minus_fp = (layout.frame_size - INCOMING_FRAME_SP_OFFSET
			- layout.fp_minus_ap);
  layout.first_local_minus_ap = layout.sp_minus_fp - layout.locals_size;
  return layout;
}

/* Worker function for TARGET_CAN_ELIMINATE.  */

static bool
xstormy16_can_eliminate (const int from, const int to)
{
  return (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM
          ? ! frame_pointer_needed
          : true);
}

/* Determine how all the special registers get eliminated.  */

int
xstormy16_initial_elimination_offset (int from, int to)
{
  struct xstormy16_stack_layout layout;
  int result;

  layout = xstormy16_compute_stack_layout ();

  if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    result = layout.sp_minus_fp - layout.locals_size;
  else if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    result = - layout.locals_size;
  else if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    result = - layout.fp_minus_ap;
  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    result = - (layout.sp_minus_fp + layout.fp_minus_ap);
  else
    gcc_unreachable ();

  return result;
}

static rtx
emit_addhi3_postreload (rtx dest, rtx src0, rtx src1)
{
  rtx set, clobber, insn;

  set = gen_rtx_SET (VOIDmode, dest, gen_rtx_PLUS (HImode, src0, src1));
  clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, CARRY_REGNUM));
  insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
  return insn;
}

/* Called after register allocation to add any instructions needed for
   the prologue.  Using a prologue insn is favored compared to putting
   all of the instructions in the TARGET_ASM_FUNCTION_PROLOGUE macro,
   since it allows the scheduler to intermix instructions with the
   saves of the caller saved registers.  In some cases, it might be
   necessary to emit a barrier instruction as the last insn to prevent
   such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.  */

void
xstormy16_expand_prologue (void)
{
  struct xstormy16_stack_layout layout;
  int regno;
  rtx insn;
  rtx mem_push_rtx;
  const int ifun = xstormy16_interrupt_function_p ();

  mem_push_rtx = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
  mem_push_rtx = gen_rtx_MEM (HImode, mem_push_rtx);

  layout = xstormy16_compute_stack_layout ();

  if (layout.locals_size >= 32768)
    error ("local variable memory requirements exceed capacity");

  if (flag_stack_usage_info)
    current_function_static_stack_size = layout.frame_size;

  /* Save the argument registers if necessary.  */
  if (layout.stdarg_save_size)
    for (regno = FIRST_ARGUMENT_REGISTER;
	 regno < FIRST_ARGUMENT_REGISTER + NUM_ARGUMENT_REGISTERS;
	 regno++)
      {
	rtx dwarf;
	rtx reg = gen_rtx_REG (HImode, regno);

	insn = emit_move_insn (mem_push_rtx, reg);
	RTX_FRAME_RELATED_P (insn) = 1;

	dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (2));

	XVECEXP (dwarf, 0, 0) = gen_rtx_SET (VOIDmode,
					     gen_rtx_MEM (Pmode, stack_pointer_rtx),
					     reg);
	XVECEXP (dwarf, 0, 1) = gen_rtx_SET (Pmode, stack_pointer_rtx,
					     plus_constant (Pmode,
							    stack_pointer_rtx,
							    GET_MODE_SIZE (Pmode)));
	add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 0)) = 1;
	RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 1)) = 1;
      }

  /* Push each of the registers to save.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (REG_NEEDS_SAVE (regno, ifun))
      {
	rtx dwarf;
	rtx reg = gen_rtx_REG (HImode, regno);

	insn = emit_move_insn (mem_push_rtx, reg);
	RTX_FRAME_RELATED_P (insn) = 1;

	dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (2));

	XVECEXP (dwarf, 0, 0) = gen_rtx_SET (VOIDmode,
					     gen_rtx_MEM (Pmode, stack_pointer_rtx),
					     reg);
	XVECEXP (dwarf, 0, 1) = gen_rtx_SET (Pmode, stack_pointer_rtx,
					     plus_constant (Pmode,
							    stack_pointer_rtx,
							    GET_MODE_SIZE (Pmode)));
	add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 0)) = 1;
	RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 1)) = 1;
      }

  /* It's just possible that the SP here might be what we need for
     the new FP...  */
  if (frame_pointer_needed && layout.sp_minus_fp == layout.locals_size)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Allocate space for local variables.  */
  if (layout.locals_size)
    {
      insn = emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (layout.locals_size));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Set up the frame pointer, if required.  */
  if (frame_pointer_needed && layout.sp_minus_fp != layout.locals_size)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;

      if (layout.sp_minus_fp)
	{
	  insn = emit_addhi3_postreload (hard_frame_pointer_rtx,
					 hard_frame_pointer_rtx,
					 GEN_INT (- layout.sp_minus_fp));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Do we need an epilogue at all?  */

int
direct_return (void)
{
  return (reload_completed
	  && xstormy16_compute_stack_layout ().frame_size == 0
	  && ! xstormy16_interrupt_function_p ());
}

/* Called after register allocation to add any instructions needed for
   the epilogue.  Using an epilogue insn is favored compared to putting
   all of the instructions in the TARGET_ASM_FUNCTION_PROLOGUE macro,
   since it allows the scheduler to intermix instructions with the
   saves of the caller saved registers.  In some cases, it might be
   necessary to emit a barrier instruction as the last insn to prevent
   such scheduling.  */

void
xstormy16_expand_epilogue (void)
{
  struct xstormy16_stack_layout layout;
  rtx mem_pop_rtx;
  int regno;
  const int ifun = xstormy16_interrupt_function_p ();

  mem_pop_rtx = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
  mem_pop_rtx = gen_rtx_MEM (HImode, mem_pop_rtx);

  layout = xstormy16_compute_stack_layout ();

  /* Pop the stack for the locals.  */
  if (layout.locals_size)
    {
      if (frame_pointer_needed && layout.sp_minus_fp == layout.locals_size)
	emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      else
	emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (- layout.locals_size));
    }

  /* Restore any call-saved registers.  */
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (REG_NEEDS_SAVE (regno, ifun))
      emit_move_insn (gen_rtx_REG (HImode, regno), mem_pop_rtx);

  /* Pop the stack for the stdarg save area.  */
  if (layout.stdarg_save_size)
    emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (- layout.stdarg_save_size));

  /* Return.  */
  if (ifun)
    emit_jump_insn (gen_return_internal_interrupt ());
  else
    emit_jump_insn (gen_return_internal ());
}

int
xstormy16_epilogue_uses (int regno)
{
  if (reload_completed && call_used_regs[regno])
    {
      const int ifun = xstormy16_interrupt_function_p ();
      return REG_NEEDS_SAVE (regno, ifun);
    }
  return 0;
}

void
xstormy16_function_profiler (void)
{
  sorry ("function_profiler support");
}

/* Update CUM to advance past an argument in the argument list.  The
   values MODE, TYPE and NAMED describe that argument.  Once this is
   done, the variable CUM is suitable for analyzing the *following*
   argument with `TARGET_FUNCTION_ARG', etc.

   This function need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount of
   stack space used for arguments without any special help.  However,
   it makes life easier for xstormy16_build_va_list if it does update
   the word count.  */

static void
xstormy16_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
				const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  /* If an argument would otherwise be passed partially in registers,
     and partially on the stack, the whole of it is passed on the
     stack.  */
  if (*cum < NUM_ARGUMENT_REGISTERS
      && *cum + XSTORMY16_WORD_SIZE (type, mode) > NUM_ARGUMENT_REGISTERS)
    *cum = NUM_ARGUMENT_REGISTERS;

  *cum += XSTORMY16_WORD_SIZE (type, mode);
}

static rtx
xstormy16_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
			const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (mode == VOIDmode)
    return const0_rtx;
  if (targetm.calls.must_pass_in_stack (mode, type)
      || *cum + XSTORMY16_WORD_SIZE (type, mode) > NUM_ARGUMENT_REGISTERS)
    return NULL_RTX;
  return gen_rtx_REG (mode, *cum + FIRST_ARGUMENT_REGISTER);
}

/* Build the va_list type.

   For this chip, va_list is a record containing a counter and a pointer.
   The counter is of type 'int' and indicates how many bytes
   have been used to date.  The pointer indicates the stack position
   for arguments that have not been passed in registers.
   To keep the layout nice, the pointer is first in the structure.  */

static tree
xstormy16_build_builtin_va_list (void)
{
  tree f_1, f_2, record, type_decl;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION,
			  TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_1 = build_decl (BUILTINS_LOCATION,
		    FIELD_DECL, get_identifier ("base"),
		      ptr_type_node);
  f_2 = build_decl (BUILTINS_LOCATION,
		    FIELD_DECL, get_identifier ("count"),
		      unsigned_type_node);

  DECL_FIELD_CONTEXT (f_1) = record;
  DECL_FIELD_CONTEXT (f_2) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_1;
  DECL_CHAIN (f_1) = f_2;

  layout_type (record);

  return record;
}

/* Implement the stdarg/varargs va_start macro.  STDARG_P is nonzero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  */

static void
xstormy16_expand_builtin_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  tree f_base, f_count;
  tree base, count;
  tree t,u;

  if (xstormy16_interrupt_function_p ())
    error ("cannot use va_start in interrupt function");

  f_base = TYPE_FIELDS (va_list_type_node);
  f_count = DECL_CHAIN (f_base);

  base = build3 (COMPONENT_REF, TREE_TYPE (f_base), valist, f_base, NULL_TREE);
  count = build3 (COMPONENT_REF, TREE_TYPE (f_count), valist, f_count,
		  NULL_TREE);

  t = make_tree (TREE_TYPE (base), virtual_incoming_args_rtx);
  u = build_int_cst (NULL_TREE, - INCOMING_FRAME_SP_OFFSET);
  u = fold_convert (TREE_TYPE (count), u);
  t = fold_build_pointer_plus (t, u);
  t = build2 (MODIFY_EXPR, TREE_TYPE (base), base, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build2 (MODIFY_EXPR, TREE_TYPE (count), count,
	      build_int_cst (NULL_TREE,
			     crtl->args.info * UNITS_PER_WORD));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable
   of type va_list as a tree, TYPE is the type passed to va_arg.
   Note:  This algorithm is documented in stormy-abi.  */

static tree
xstormy16_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
				gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree f_base, f_count;
  tree base, count;
  tree count_tmp, addr, t;
  tree lab_gotaddr, lab_fromstack;
  int size, size_of_reg_args, must_stack;
  tree size_tree;

  f_base = TYPE_FIELDS (va_list_type_node);
  f_count = DECL_CHAIN (f_base);

  base = build3 (COMPONENT_REF, TREE_TYPE (f_base), valist, f_base, NULL_TREE);
  count = build3 (COMPONENT_REF, TREE_TYPE (f_count), valist, f_count,
		  NULL_TREE);

  must_stack = targetm.calls.must_pass_in_stack (TYPE_MODE (type), type);
  size_tree = round_up (size_in_bytes (type), UNITS_PER_WORD);
  gimplify_expr (&size_tree, pre_p, NULL, is_gimple_val, fb_rvalue);

  size_of_reg_args = NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD;

  count_tmp = get_initialized_tmp_var (count, pre_p, NULL);
  lab_gotaddr = create_artificial_label (UNKNOWN_LOCATION);
  lab_fromstack = create_artificial_label (UNKNOWN_LOCATION);
  addr = create_tmp_var (ptr_type_node, NULL);

  if (!must_stack)
    {
      tree r;

      t = fold_convert (TREE_TYPE (count), size_tree);
      t = build2 (PLUS_EXPR, TREE_TYPE (count), count_tmp, t);
      r = fold_convert (TREE_TYPE (count), size_int (size_of_reg_args));
      t = build2 (GT_EXPR, boolean_type_node, t, r);
      t = build3 (COND_EXPR, void_type_node, t,
		  build1 (GOTO_EXPR, void_type_node, lab_fromstack),
		  NULL_TREE);
      gimplify_and_add (t, pre_p);

      t = fold_build_pointer_plus (base, count_tmp);
      gimplify_assign (addr, t, pre_p);

      t = build1 (GOTO_EXPR, void_type_node, lab_gotaddr);
      gimplify_and_add (t, pre_p);

      t = build1 (LABEL_EXPR, void_type_node, lab_fromstack);
      gimplify_and_add (t, pre_p);
    }

  /* Arguments larger than a word might need to skip over some
     registers, since arguments are either passed entirely in
     registers or entirely on the stack.  */
  size = PUSH_ROUNDING (int_size_in_bytes (type));
  if (size > 2 || size < 0 || must_stack)
    {
      tree r, u;

      r = size_int (NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD);
      u = build2 (MODIFY_EXPR, TREE_TYPE (count_tmp), count_tmp, r);

      t = fold_convert (TREE_TYPE (count), r);
      t = build2 (GE_EXPR, boolean_type_node, count_tmp, t);
      t = build3 (COND_EXPR, void_type_node, t, NULL_TREE, u);
      gimplify_and_add (t, pre_p);
    }

  t = size_int (NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD
		+ INCOMING_FRAME_SP_OFFSET);
  t = fold_convert (TREE_TYPE (count), t);
  t = build2 (MINUS_EXPR, TREE_TYPE (count), count_tmp, t);
  t = build2 (PLUS_EXPR, TREE_TYPE (count), t,
	      fold_convert (TREE_TYPE (count), size_tree));
  t = fold_convert (TREE_TYPE (t), fold (t));
  t = fold_build1 (NEGATE_EXPR, TREE_TYPE (t), t);
  t = fold_build_pointer_plus (base, t);
  gimplify_assign (addr, t, pre_p);

  t = build1 (LABEL_EXPR, void_type_node, lab_gotaddr);
  gimplify_and_add (t, pre_p);

  t = fold_convert (TREE_TYPE (count), size_tree);
  t = build2 (PLUS_EXPR, TREE_TYPE (count), count_tmp, t);
  gimplify_assign (count, t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);
  return build_va_arg_indirect_ref (addr);
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
xstormy16_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx temp = gen_reg_rtx (HImode);
  rtx reg_fnaddr = gen_reg_rtx (HImode);
  rtx reg_addr, reg_addr_mem;

  reg_addr = copy_to_reg (XEXP (m_tramp, 0));
  reg_addr_mem = adjust_automodify_address (m_tramp, HImode, reg_addr, 0);

  emit_move_insn (temp, GEN_INT (0x3130 | STATIC_CHAIN_REGNUM));
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  reg_addr_mem = adjust_automodify_address (reg_addr_mem, VOIDmode, NULL, 2);

  emit_move_insn (temp, static_chain);
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  reg_addr_mem = adjust_automodify_address (reg_addr_mem, VOIDmode, NULL, 2);

  emit_move_insn (reg_fnaddr, XEXP (DECL_RTL (fndecl), 0));
  emit_move_insn (temp, reg_fnaddr);
  emit_insn (gen_andhi3 (temp, temp, GEN_INT (0xFF)));
  emit_insn (gen_iorhi3 (temp, temp, GEN_INT (0x0200)));
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  reg_addr_mem = adjust_automodify_address (reg_addr_mem, VOIDmode, NULL, 2);

  emit_insn (gen_lshrhi3 (reg_fnaddr, reg_fnaddr, GEN_INT (8)));
  emit_move_insn (reg_addr_mem, reg_fnaddr);
}

/* Worker function for TARGET_FUNCTION_VALUE.  */

static rtx
xstormy16_function_value (const_tree valtype,
			  const_tree func ATTRIBUTE_UNUSED,
			  bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode;
  mode = TYPE_MODE (valtype);
  PROMOTE_MODE (mode, 0, valtype);
  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* Worker function for TARGET_LIBCALL_VALUE.  */

static rtx
xstormy16_libcall_value (enum machine_mode mode,
			 const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
xstormy16_function_value_regno_p (const unsigned int regno)
{
  return (regno == RETURN_VALUE_REGNUM);
}

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if @var{function} had been called directly
   with the adjusted first argument.  This macro is responsible for
   emitting all of the code for a thunk function;
   TARGET_ASM_FUNCTION_PROLOGUE and TARGET_ASM_FUNCTION_EPILOGUE are
   not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.  */

static void
xstormy16_asm_output_mi_thunk (FILE *file,
			       tree thunk_fndecl ATTRIBUTE_UNUSED,
			       HOST_WIDE_INT delta,
			       HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			       tree function)
{
  int regnum = FIRST_ARGUMENT_REGISTER;

  /* There might be a hidden first argument for a returned structure.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    regnum += 1;

  fprintf (file, "\tadd %s,#0x%x\n", reg_names[regnum], (int) delta & 0xFFFF);
  fputs ("\tjmpf ", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  putc ('\n', file);
}

/* The purpose of this function is to override the default behavior of
   BSS objects.  Normally, they go into .bss or .sbss via ".common"
   directives, but we need to override that and put them in
   .bss_below100.  We can't just use a section override (like we do
   for .data_below100), because that makes them initialized rather
   than uninitialized.  */

void
xstormy16_asm_output_aligned_common (FILE *stream,
				     tree decl,
				     const char *name,
				     int size,
				     int align,
				     int global)
{
  rtx mem = decl == NULL_TREE ? NULL_RTX : DECL_RTL (decl);
  rtx symbol;

  if (mem != NULL_RTX
      && MEM_P (mem)
      && GET_CODE (symbol = XEXP (mem, 0)) == SYMBOL_REF
      && SYMBOL_REF_FLAGS (symbol) & SYMBOL_FLAG_XSTORMY16_BELOW100)
    {
      const char *name2;
      int p2align = 0;

      switch_to_section (bss100_section);

      while (align > 8)
	{
	  align /= 2;
	  p2align ++;
	}

      name2 = default_strip_name_encoding (name);
      if (global)
	fprintf (stream, "\t.globl\t%s\n", name2);
      if (p2align)
	fprintf (stream, "\t.p2align %d\n", p2align);
      fprintf (stream, "\t.type\t%s, @object\n", name2);
      fprintf (stream, "\t.size\t%s, %d\n", name2, size);
      fprintf (stream, "%s:\n\t.space\t%d\n", name2, size);
      return;
    }

  if (!global)
    {
      fprintf (stream, "\t.local\t");
      assemble_name (stream, name);
      fprintf (stream, "\n");
    }
  fprintf (stream, "\t.comm\t");
  assemble_name (stream, name);
  fprintf (stream, ",%u,%u\n", size, align / BITS_PER_UNIT);
}

/* Implement TARGET_ASM_INIT_SECTIONS.  */

static void
xstormy16_asm_init_sections (void)
{
  bss100_section
    = get_unnamed_section (SECTION_WRITE | SECTION_BSS,
			   output_section_asm_op,
			   "\t.section \".bss_below100\",\"aw\",@nobits");
}

/* Mark symbols with the "below100" attribute so that we can use the
   special addressing modes for them.  */

static void
xstormy16_encode_section_info (tree decl, rtx r, int first)
{
  default_encode_section_info (decl, r, first);

   if (TREE_CODE (decl) == VAR_DECL
      && (lookup_attribute ("below100", DECL_ATTRIBUTES (decl))
	  || lookup_attribute ("BELOW100", DECL_ATTRIBUTES (decl))))
    {
      rtx symbol = XEXP (r, 0);

      gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
      SYMBOL_REF_FLAGS (symbol) |= SYMBOL_FLAG_XSTORMY16_BELOW100;
    }
}

#undef  TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR  xstormy16_asm_out_constructor
#undef  TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR   xstormy16_asm_out_destructor

/* Output constructors and destructors.  Just like
   default_named_section_asm_out_* but don't set the sections writable.  */

static void
xstormy16_asm_out_destructor (rtx symbol, int priority)
{
  const char *section = ".dtors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.  */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".dtors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  switch_to_section (get_section (section, 0, NULL));
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

static void
xstormy16_asm_out_constructor (rtx symbol, int priority)
{
  const char *section = ".ctors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.  */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".ctors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  switch_to_section (get_section (section, 0, NULL));
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

/* Worker function for TARGET_PRINT_OPERAND_ADDRESS.

   Print a memory address as an operand to reference that memory location.  */

static void
xstormy16_print_operand_address (FILE *file, rtx address)
{
  HOST_WIDE_INT offset;
  int pre_dec, post_inc;

  /* There are a few easy cases.  */
  if (CONST_INT_P (address))
    {
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (address) & 0xFFFF);
      return;
    }

  if (CONSTANT_P (address) || LABEL_P (address))
    {
      output_addr_const (file, address);
      return;
    }

  /* Otherwise, it's hopefully something of the form
     (plus:HI (pre_dec:HI (reg:HI ...)) (const_int ...)).  */
  if (GET_CODE (address) == PLUS)
    {
      gcc_assert (CONST_INT_P (XEXP (address, 1)));
      offset = INTVAL (XEXP (address, 1));
      address = XEXP (address, 0);
    }
  else
    offset = 0;

  pre_dec = (GET_CODE (address) == PRE_DEC);
  post_inc = (GET_CODE (address) == POST_INC);
  if (pre_dec || post_inc)
    address = XEXP (address, 0);

  gcc_assert (REG_P (address));

  fputc ('(', file);
  if (pre_dec)
    fputs ("--", file);
  fputs (reg_names [REGNO (address)], file);
  if (post_inc)
    fputs ("++", file);
  if (offset != 0)
    fprintf (file, "," HOST_WIDE_INT_PRINT_DEC, offset);
  fputc (')', file);
}

/* Worker function for TARGET_PRINT_OPERAND.

   Print an operand to an assembler instruction.  */

static void
xstormy16_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'B':
	/* There is either one bit set, or one bit clear, in X.
	   Print it preceded by '#'.  */
      {
	static int bits_set[8] = { 0, 1, 1, 2, 1, 2, 2, 3 };
	HOST_WIDE_INT xx = 1;
	HOST_WIDE_INT l;

	if (CONST_INT_P (x))
	  xx = INTVAL (x);
	else
	  output_operand_lossage ("'B' operand is not constant");

	/* GCC sign-extends masks with the MSB set, so we have to
	   detect all the cases that differ only in sign extension
	   beyond the bits we care about.  Normally, the predicates
	   and constraints ensure that we have the right values.  This
	   works correctly for valid masks.  */
	if (bits_set[xx & 7] <= 1)
	  {
	    /* Remove sign extension bits.  */
	    if ((~xx & ~(HOST_WIDE_INT)0xff) == 0)
	      xx &= 0xff;
	    else if ((~xx & ~(HOST_WIDE_INT)0xffff) == 0)
	      xx &= 0xffff;
	    l = exact_log2 (xx);
	  }
	else
	  {
	    /* Add sign extension bits.  */
	    if ((xx & ~(HOST_WIDE_INT)0xff) == 0)
	      xx |= ~(HOST_WIDE_INT)0xff;
	    else if ((xx & ~(HOST_WIDE_INT)0xffff) == 0)
	      xx |= ~(HOST_WIDE_INT)0xffff;
	    l = exact_log2 (~xx);
	  }

	if (l == -1)
	  output_operand_lossage ("'B' operand has multiple bits set");

	fprintf (file, IMMEDIATE_PREFIX HOST_WIDE_INT_PRINT_DEC, l);
	return;
      }

    case 'C':
      /* Print the symbol without a surrounding @fptr().  */
      if (GET_CODE (x) == SYMBOL_REF)
	assemble_name (file, XSTR (x, 0));
      else if (LABEL_P (x))
	output_asm_label (x);
      else
	xstormy16_print_operand_address (file, x);
      return;

    case 'o':
    case 'O':
      /* Print the immediate operand less one, preceded by '#'.
         For 'O', negate it first.  */
      {
	HOST_WIDE_INT xx = 0;

	if (CONST_INT_P (x))
	  xx = INTVAL (x);
	else
	  output_operand_lossage ("'o' operand is not constant");

	if (code == 'O')
	  xx = -xx;

	fprintf (file, IMMEDIATE_PREFIX HOST_WIDE_INT_PRINT_DEC, xx - 1);
	return;
      }

    case 'b':
      /* Print the shift mask for bp/bn.  */
      {
	HOST_WIDE_INT xx = 1;
	HOST_WIDE_INT l;

	if (CONST_INT_P (x))
	  xx = INTVAL (x);
	else
	  output_operand_lossage ("'B' operand is not constant");

	l = 7 - xx;

	fputs (IMMEDIATE_PREFIX, file);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, l);
	return;
      }

    case 0:
      /* Handled below.  */
      break;

    default:
      output_operand_lossage ("xstormy16_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      xstormy16_print_operand_address (file, XEXP (x, 0));
      break;

    default:
      /* Some kind of constant or label; an immediate operand,
         so prefix it with '#' for the assembler.  */
      fputs (IMMEDIATE_PREFIX, file);
      output_addr_const (file, x);
      break;
    }

  return;
}

/* Expander for the `casesi' pattern.
   INDEX is the index of the switch statement.
   LOWER_BOUND is a CONST_INT that is the value of INDEX corresponding
     to the first table entry.
   RANGE is the number of table entries.
   TABLE is an ADDR_VEC that is the jump table.
   DEFAULT_LABEL is the address to branch to if INDEX is outside the
     range LOWER_BOUND to LOWER_BOUND + RANGE - 1.  */

void
xstormy16_expand_casesi (rtx index, rtx lower_bound, rtx range,
			 rtx table, rtx default_label)
{
  HOST_WIDE_INT range_i = INTVAL (range);
  rtx int_index;

  /* This code uses 'br', so it can deal only with tables of size up to
     8192 entries.  */
  if (range_i >= 8192)
    sorry ("switch statement of size %lu entries too large",
	   (unsigned long) range_i);

  index = expand_binop (SImode, sub_optab, index, lower_bound, NULL_RTX, 0,
			OPTAB_LIB_WIDEN);
  emit_cmp_and_jump_insns (index, range, GTU, NULL_RTX, SImode, 1,
			   default_label);
  int_index = gen_lowpart_common (HImode, index);
  emit_insn (gen_ashlhi3 (int_index, int_index, const2_rtx));
  emit_jump_insn (gen_tablejump_pcrel (int_index, table));
}

/* Output an ADDR_VEC.  It is output as a sequence of 'jmpf'
   instructions, without label or alignment or any other special
   constructs.  We know that the previous instruction will be the
   `tablejump_pcrel' output above.

   TODO: it might be nice to output 'br' instructions if they could
   all reach.  */

void
xstormy16_output_addr_vec (FILE *file, rtx label ATTRIBUTE_UNUSED, rtx table)
{
  int vlen, idx;

  switch_to_section (current_function_section ());

  vlen = XVECLEN (table, 0);
  for (idx = 0; idx < vlen; idx++)
    {
      fputs ("\tjmpf ", file);
      output_asm_label (XEXP (XVECEXP (table, 0, idx), 0));
      fputc ('\n', file);
    }
}

/* Expander for the `call' patterns.
   RETVAL is the RTL for the return register or NULL for void functions.
   DEST is the function to call, expressed as a MEM.
   COUNTER is ignored.  */

void
xstormy16_expand_call (rtx retval, rtx dest, rtx counter)
{
  rtx call, temp;
  enum machine_mode mode;

  gcc_assert (MEM_P (dest));
  dest = XEXP (dest, 0);

  if (! CONSTANT_P (dest) && ! REG_P (dest))
    dest = force_reg (Pmode, dest);

  if (retval == NULL)
    mode = VOIDmode;
  else
    mode = GET_MODE (retval);

  call = gen_rtx_CALL (mode, gen_rtx_MEM (FUNCTION_MODE, dest),
		       counter);
  if (retval)
    call = gen_rtx_SET (VOIDmode, retval, call);

  if (! CONSTANT_P (dest))
    {
      temp = gen_reg_rtx (HImode);
      emit_move_insn (temp, const0_rtx);
    }
  else
    temp = const0_rtx;

  call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, call,
						gen_rtx_USE (VOIDmode, temp)));
  emit_call_insn (call);
}

/* Expanders for multiword computational operations.  */

/* Expander for arithmetic operations; emit insns to compute

   (set DEST (CODE:MODE SRC0 SRC1))

   When CODE is COMPARE, a branch template is generated
   (this saves duplicating code in xstormy16_split_cbranch).  */

void
xstormy16_expand_arith (enum machine_mode mode, enum rtx_code code,
			rtx dest, rtx src0, rtx src1)
{
  int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
  int i;
  int firstloop = 1;

  if (code == NEG)
    emit_move_insn (src0, const0_rtx);

  for (i = 0; i < num_words; i++)
    {
      rtx w_src0, w_src1, w_dest;
      rtx insn;

      w_src0 = simplify_gen_subreg (word_mode, src0, mode,
				    i * UNITS_PER_WORD);
      w_src1 = simplify_gen_subreg (word_mode, src1, mode, i * UNITS_PER_WORD);
      w_dest = simplify_gen_subreg (word_mode, dest, mode, i * UNITS_PER_WORD);

      switch (code)
	{
	case PLUS:
	  if (firstloop
	      && CONST_INT_P (w_src1)
	      && INTVAL (w_src1) == 0)
	    continue;

	  if (firstloop)
	    insn = gen_addchi4 (w_dest, w_src0, w_src1);
	  else
	    insn = gen_addchi5 (w_dest, w_src0, w_src1);
	  break;

	case NEG:
	case MINUS:
	case COMPARE:
	  if (code == COMPARE && i == num_words - 1)
	    {
	      rtx branch, sub, clobber, sub_1;

	      sub_1 = gen_rtx_MINUS (HImode, w_src0,
				     gen_rtx_ZERO_EXTEND (HImode, gen_rtx_REG (BImode, CARRY_REGNUM)));
	      sub = gen_rtx_SET (VOIDmode, w_dest,
				 gen_rtx_MINUS (HImode, sub_1, w_src1));
	      clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, CARRY_REGNUM));
	      branch = gen_rtx_SET (VOIDmode, pc_rtx,
				    gen_rtx_IF_THEN_ELSE (VOIDmode,
							  gen_rtx_EQ (HImode,
								      sub_1,
								      w_src1),
							  pc_rtx,
							  pc_rtx));
	      insn = gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (3, branch, sub, clobber));
	    }
	  else if (firstloop
		   && code != COMPARE
		   && CONST_INT_P (w_src1)
		   && INTVAL (w_src1) == 0)
	    continue;
	  else if (firstloop)
	    insn = gen_subchi4 (w_dest, w_src0, w_src1);
	  else
	    insn = gen_subchi5 (w_dest, w_src0, w_src1);
	  break;

	case IOR:
	case XOR:
	case AND:
	  if (CONST_INT_P (w_src1)
	      && INTVAL (w_src1) == -(code == AND))
	    continue;

	  insn = gen_rtx_SET (VOIDmode, w_dest, gen_rtx_fmt_ee (code, mode,
								w_src0, w_src1));
	  break;

	case NOT:
	  insn = gen_rtx_SET (VOIDmode, w_dest, gen_rtx_NOT (mode, w_src0));
	  break;

	default:
	  gcc_unreachable ();
	}

      firstloop = 0;
      emit (insn);
    }

  /* If we emit nothing, try_split() will think we failed.  So emit
     something that does nothing and can be optimized away.  */
  if (firstloop)
    emit (gen_nop ());
}

/* The shift operations are split at output time for constant values;
   variable-width shifts get handed off to a library routine.

   Generate an output string to do (set X (CODE:MODE X SIZE_R))
   SIZE_R will be a CONST_INT, X will be a hard register.  */

const char *
xstormy16_output_shift (enum machine_mode mode, enum rtx_code code,
			rtx x, rtx size_r, rtx temp)
{
  HOST_WIDE_INT size;
  const char *r0, *r1, *rt;
  static char r[64];

  gcc_assert (CONST_INT_P (size_r)
	      && REG_P (x)
	      && mode == SImode);

  size = INTVAL (size_r) & (GET_MODE_BITSIZE (mode) - 1);

  if (size == 0)
    return "";

  r0 = reg_names [REGNO (x)];
  r1 = reg_names [REGNO (x) + 1];

  /* For shifts of size 1, we can use the rotate instructions.  */
  if (size == 1)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "shl %s,#1 | rlc %s,#1", r0, r1);
	  break;
	case ASHIFTRT:
	  sprintf (r, "asr %s,#1 | rrc %s,#1", r1, r0);
	  break;
	case LSHIFTRT:
	  sprintf (r, "shr %s,#1 | rrc %s,#1", r1, r0);
	  break;
	default:
	  gcc_unreachable ();
	}
      return r;
    }

  /* For large shifts, there are easy special cases.  */
  if (size == 16)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "mov %s,%s | mov %s,#0", r1, r0, r0);
	  break;
	case ASHIFTRT:
	  sprintf (r, "mov %s,%s | asr %s,#15", r0, r1, r1);
	  break;
	case LSHIFTRT:
	  sprintf (r, "mov %s,%s | mov %s,#0", r0, r1, r1);
	  break;
	default:
	  gcc_unreachable ();
	}
      return r;
    }
  if (size > 16)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "mov %s,%s | mov %s,#0 | shl %s,#%d",
		   r1, r0, r0, r1, (int) size - 16);
	  break;
	case ASHIFTRT:
	  sprintf (r, "mov %s,%s | asr %s,#15 | asr %s,#%d",
		   r0, r1, r1, r0, (int) size - 16);
	  break;
	case LSHIFTRT:
	  sprintf (r, "mov %s,%s | mov %s,#0 | shr %s,#%d",
		   r0, r1, r1, r0, (int) size - 16);
	  break;
	default:
	  gcc_unreachable ();
	}
      return r;
    }

  /* For the rest, we have to do more work.  In particular, we
     need a temporary.  */
  rt = reg_names [REGNO (temp)];
  switch (code)
    {
    case ASHIFT:
      sprintf (r,
	       "mov %s,%s | shl %s,#%d | shl %s,#%d | shr %s,#%d | or %s,%s",
	       rt, r0, r0, (int) size, r1, (int) size, rt, (int) (16 - size),
	       r1, rt);
      break;
    case ASHIFTRT:
      sprintf (r,
	       "mov %s,%s | asr %s,#%d | shr %s,#%d | shl %s,#%d | or %s,%s",
	       rt, r1, r1, (int) size, r0, (int) size, rt, (int) (16 - size),
	       r0, rt);
      break;
    case LSHIFTRT:
      sprintf (r,
	       "mov %s,%s | shr %s,#%d | shr %s,#%d | shl %s,#%d | or %s,%s",
	       rt, r1, r1, (int) size, r0, (int) size, rt, (int) (16 - size),
	       r0, rt);
      break;
    default:
      gcc_unreachable ();
    }
  return r;
}

/* Attribute handling.  */

/* Return nonzero if the function is an interrupt function.  */

int
xstormy16_interrupt_function_p (void)
{
  tree attributes;

  /* The dwarf2 mechanism asks for INCOMING_FRAME_SP_OFFSET before
     any functions are declared, which is demonstrably wrong, but
     it is worked around here.  FIXME.  */
  if (!cfun)
    return 0;

  attributes = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  return lookup_attribute ("interrupt", attributes) != NULL_TREE;
}

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE  xstormy16_attribute_table

static tree xstormy16_handle_interrupt_attribute
  (tree *, tree, tree, int, bool *);
static tree xstormy16_handle_below100_attribute
  (tree *, tree, tree, int, bool *);

static const struct attribute_spec xstormy16_attribute_table[] =
{
  /* name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
     affects_type_identity.  */
  { "interrupt", 0, 0, false, true,  true,
    xstormy16_handle_interrupt_attribute , false },
  { "BELOW100",  0, 0, false, false, false,
    xstormy16_handle_below100_attribute, false },
  { "below100",  0, 0, false, false, false,
    xstormy16_handle_below100_attribute, false },
  { NULL,        0, 0, false, false, false, NULL, false }
};

/* Handle an "interrupt" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
xstormy16_handle_interrupt_attribute (tree *node, tree name,
				      tree args ATTRIBUTE_UNUSED,
				      int flags ATTRIBUTE_UNUSED,
				      bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "below" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
xstormy16_handle_below100_attribute (tree *node,
				     tree name ATTRIBUTE_UNUSED,
				     tree args ATTRIBUTE_UNUSED,
				     int flags ATTRIBUTE_UNUSED,
				     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != VAR_DECL
      && TREE_CODE (*node) != POINTER_TYPE
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes,
	       "%<__BELOW100__%> attribute only applies to variables");
      *no_add_attrs = true;
    }
  else if (args == NULL_TREE && TREE_CODE (*node) == VAR_DECL)
    {
      if (! (TREE_PUBLIC (*node) || TREE_STATIC (*node)))
	{
	  warning (OPT_Wattributes, "__BELOW100__ attribute not allowed "
		   "with auto storage class");
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS   xstormy16_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN  xstormy16_expand_builtin

static struct
{
  const char * name;
  int          md_code;
  const char * arg_ops;   /* 0..9, t for temp register, r for return value.  */
  const char * arg_types; /* s=short,l=long, upper case for unsigned.  */
}
  s16builtins[] =
{
  { "__sdivlh", CODE_FOR_sdivlh, "rt01", "sls" },
  { "__smodlh", CODE_FOR_sdivlh, "tr01", "sls" },
  { "__udivlh", CODE_FOR_udivlh, "rt01", "SLS" },
  { "__umodlh", CODE_FOR_udivlh, "tr01", "SLS" },
  { NULL, 0, NULL, NULL }
};

static void
xstormy16_init_builtins (void)
{
  tree args[2], ret_type, arg = NULL_TREE, ftype;
  int i, a, n_args;

  ret_type = void_type_node;

  for (i = 0; s16builtins[i].name; i++)
    {
      n_args = strlen (s16builtins[i].arg_types) - 1;

      gcc_assert (n_args <= (int) ARRAY_SIZE (args));

      for (a = n_args - 1; a >= 0; a--)
	args[a] = NULL_TREE;

      for (a = n_args; a >= 0; a--)
	{
	  switch (s16builtins[i].arg_types[a])
	    {
	    case 's': arg = short_integer_type_node; break;
	    case 'S': arg = short_unsigned_type_node; break;
	    case 'l': arg = long_integer_type_node; break;
	    case 'L': arg = long_unsigned_type_node; break;
	    default: gcc_unreachable ();
	    }
	  if (a == 0)
	    ret_type = arg;
	  else
	    args[a-1] = arg;
	}
      ftype = build_function_type_list (ret_type, args[0], args[1], NULL_TREE);
      add_builtin_function (s16builtins[i].name, ftype,
			    i, BUILT_IN_MD, NULL, NULL_TREE);
    }
}

static rtx
xstormy16_expand_builtin (tree exp, rtx target,
			  rtx subtarget ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
			  int ignore ATTRIBUTE_UNUSED)
{
  rtx op[10], args[10], pat, copyto[10], retval = 0;
  tree fndecl, argtree;
  int i, a, o, code;

  fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  argtree = TREE_OPERAND (exp, 1);
  i = DECL_FUNCTION_CODE (fndecl);
  code = s16builtins[i].md_code;

  for (a = 0; a < 10 && argtree; a++)
    {
      args[a] = expand_normal (TREE_VALUE (argtree));
      argtree = TREE_CHAIN (argtree);
    }

  for (o = 0; s16builtins[i].arg_ops[o]; o++)
    {
      char ao = s16builtins[i].arg_ops[o];
      char c = insn_data[code].operand[o].constraint[0];
      enum machine_mode omode;

      copyto[o] = 0;

      omode = (enum machine_mode) insn_data[code].operand[o].mode;
      if (ao == 'r')
	op[o] = target ? target : gen_reg_rtx (omode);
      else if (ao == 't')
	op[o] = gen_reg_rtx (omode);
      else
	op[o] = args[(int) hex_value (ao)];

      if (! (*insn_data[code].operand[o].predicate) (op[o], GET_MODE (op[o])))
	{
	  if (c == '+' || c == '=')
	    {
	      copyto[o] = op[o];
	      op[o] = gen_reg_rtx (omode);
	    }
	  else
	    op[o] = copy_to_mode_reg (omode, op[o]);
	}

      if (ao == 'r')
	retval = op[o];
    }

  pat = GEN_FCN (code) (op[0], op[1], op[2], op[3], op[4],
			op[5], op[6], op[7], op[8], op[9]);
  emit_insn (pat);

  for (o = 0; s16builtins[i].arg_ops[o]; o++)
    if (copyto[o])
      {
	emit_move_insn (copyto[o], op[o]);
	if (op[o] == retval)
	  retval = copyto[o];
      }

  return retval;
}

/* Look for combinations of insns that can be converted to BN or BP
   opcodes.  This is, unfortunately, too complex to do with MD
   patterns.  */

static void
combine_bnp (rtx insn)
{
  int insn_code, regno, need_extend;
  unsigned int mask;
  rtx cond, reg, and_insn, load, qireg, mem;
  enum machine_mode load_mode = QImode;
  enum machine_mode and_mode = QImode;
  rtx shift = NULL_RTX;

  insn_code = recog_memoized (insn);
  if (insn_code != CODE_FOR_cbranchhi
      && insn_code != CODE_FOR_cbranchhi_neg)
    return;

  cond = XVECEXP (PATTERN (insn), 0, 0); /* set */
  cond = XEXP (cond, 1); /* if */
  cond = XEXP (cond, 0); /* cond */
  switch (GET_CODE (cond))
    {
    case NE:
    case EQ:
      need_extend = 0;
      break;
    case LT:
    case GE:
      need_extend = 1;
      break;
    default:
      return;
    }

  reg = XEXP (cond, 0);
  if (! REG_P (reg))
    return;
  regno = REGNO (reg);
  if (XEXP (cond, 1) != const0_rtx)
    return;
  if (! find_regno_note (insn, REG_DEAD, regno))
    return;
  qireg = gen_rtx_REG (QImode, regno);

  if (need_extend)
    {
      /* LT and GE conditionals should have a sign extend before
	 them.  */
      for (and_insn = prev_real_insn (insn);
	   and_insn != NULL_RTX;
	   and_insn = prev_real_insn (and_insn))
	{
	  int and_code = recog_memoized (and_insn);

	  if (and_code == CODE_FOR_extendqihi2
	      && rtx_equal_p (SET_DEST (PATTERN (and_insn)), reg)
	      && rtx_equal_p (XEXP (SET_SRC (PATTERN (and_insn)), 0), qireg))
	    break;

	  if (and_code == CODE_FOR_movhi_internal
	      && rtx_equal_p (SET_DEST (PATTERN (and_insn)), reg))
	    {
	      /* This is for testing bit 15.  */
	      and_insn = insn;
	      break;
	    }

	  if (reg_mentioned_p (reg, and_insn))
	    return;

	  if (! NOTE_P (and_insn) && ! NONJUMP_INSN_P (and_insn))
	    return;
	}
    }
  else
    {
      /* EQ and NE conditionals have an AND before them.  */
      for (and_insn = prev_real_insn (insn);
	   and_insn != NULL_RTX;
	   and_insn = prev_real_insn (and_insn))
	{
	  if (recog_memoized (and_insn) == CODE_FOR_andhi3
	      && rtx_equal_p (SET_DEST (PATTERN (and_insn)), reg)
	      && rtx_equal_p (XEXP (SET_SRC (PATTERN (and_insn)), 0), reg))
	    break;

	  if (reg_mentioned_p (reg, and_insn))
	    return;

	  if (! NOTE_P (and_insn) && ! NONJUMP_INSN_P (and_insn))
	    return;
	}

      if (and_insn)
	{
	  /* Some mis-optimizations by GCC can generate a RIGHT-SHIFT
	     followed by an AND like this:

               (parallel [(set (reg:HI r7) (lshiftrt:HI (reg:HI r7) (const_int 3)))
                          (clobber (reg:BI carry))]

               (set (reg:HI r7) (and:HI (reg:HI r7) (const_int 1)))

	     Attempt to detect this here.  */
	  for (shift = prev_real_insn (and_insn); shift;
	       shift = prev_real_insn (shift))
	    {
	      if (recog_memoized (shift) == CODE_FOR_lshrhi3
		  && rtx_equal_p (SET_DEST (XVECEXP (PATTERN (shift), 0, 0)), reg)
		  && rtx_equal_p (XEXP (SET_SRC (XVECEXP (PATTERN (shift), 0, 0)), 0), reg))
		break;

	      if (reg_mentioned_p (reg, shift)
		  || (! NOTE_P (shift) && ! NONJUMP_INSN_P (shift)))
		{
		  shift = NULL_RTX;
		  break;
		}
	    }
	}
    }

  if (and_insn == NULL_RTX)
    return;

  for (load = shift ? prev_real_insn (shift) : prev_real_insn (and_insn);
       load;
       load = prev_real_insn (load))
    {
      int load_code = recog_memoized (load);

      if (load_code == CODE_FOR_movhi_internal
	  && rtx_equal_p (SET_DEST (PATTERN (load)), reg)
	  && xstormy16_below100_operand (SET_SRC (PATTERN (load)), HImode)
	  && ! MEM_VOLATILE_P (SET_SRC (PATTERN (load))))
	{
	  load_mode = HImode;
	  break;
	}

      if (load_code == CODE_FOR_movqi_internal
	  && rtx_equal_p (SET_DEST (PATTERN (load)), qireg)
	  && xstormy16_below100_operand (SET_SRC (PATTERN (load)), QImode))
	{
	  load_mode = QImode;
	  break;
	}

      if (load_code == CODE_FOR_zero_extendqihi2
	  && rtx_equal_p (SET_DEST (PATTERN (load)), reg)
	  && xstormy16_below100_operand (XEXP (SET_SRC (PATTERN (load)), 0), QImode))
	{
	  load_mode = QImode;
	  and_mode = HImode;
	  break;
	}

      if (reg_mentioned_p (reg, load))
	return;

      if (! NOTE_P (load) && ! NONJUMP_INSN_P (load))
	return;
    }
  if (!load)
    return;

  mem = SET_SRC (PATTERN (load));

  if (need_extend)
    {
      mask = (load_mode == HImode) ? 0x8000 : 0x80;

      /* If the mem includes a zero-extend operation and we are
	 going to generate a sign-extend operation then move the
	 mem inside the zero-extend.  */
      if (GET_CODE (mem) == ZERO_EXTEND)
	mem = XEXP (mem, 0);
    }
  else
    {
      if (!xstormy16_onebit_set_operand (XEXP (SET_SRC (PATTERN (and_insn)), 1),
					 load_mode))
	return;

      mask = (int) INTVAL (XEXP (SET_SRC (PATTERN (and_insn)), 1));

      if (shift)
	mask <<= INTVAL (XEXP (SET_SRC (XVECEXP (PATTERN (shift), 0, 0)), 1));
    }

  if (load_mode == HImode)
    {
      rtx addr = XEXP (mem, 0);

      if (! (mask & 0xff))
	{
	  addr = plus_constant (Pmode, addr, 1);
	  mask >>= 8;
	}
      mem = gen_rtx_MEM (QImode, addr);
    }

  if (need_extend)
    XEXP (cond, 0) = gen_rtx_SIGN_EXTEND (HImode, mem);
  else
    XEXP (cond, 0) = gen_rtx_AND (and_mode, mem, GEN_INT (mask));

  INSN_CODE (insn) = -1;
  delete_insn (load);

  if (and_insn != insn)
    delete_insn (and_insn);

  if (shift != NULL_RTX)
    delete_insn (shift);
}

static void
xstormy16_reorg (void)
{
  rtx insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (! JUMP_P (insn))
	continue;
      combine_bnp (insn);
    }
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
xstormy16_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > UNITS_PER_WORD * NUM_ARGUMENT_REGISTERS);
}

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO xstormy16_encode_section_info

/* Select_section doesn't handle .bss_below100.  */
#undef  TARGET_HAVE_SWITCHABLE_BSS_SECTIONS
#define TARGET_HAVE_SWITCHABLE_BSS_SECTIONS false

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK xstormy16_asm_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND xstormy16_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS xstormy16_print_operand_address

#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST xstormy16_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS xstormy16_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST xstormy16_address_cost

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST xstormy16_build_builtin_va_list
#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START xstormy16_expand_builtin_va_start
#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR xstormy16_gimplify_va_arg_expr

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG xstormy16_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE xstormy16_function_arg_advance

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY xstormy16_return_in_memory
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE xstormy16_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE xstormy16_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P xstormy16_function_value_regno_p

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG xstormy16_reorg

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS xstormy16_preferred_reload_class
#undef  TARGET_PREFERRED_OUTPUT_RELOAD_CLASS
#define TARGET_PREFERRED_OUTPUT_RELOAD_CLASS xstormy16_preferred_reload_class

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	xstormy16_legitimate_address_p
#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P xstormy16_mode_dependent_address_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE xstormy16_can_eliminate

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT xstormy16_trampoline_init

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-stormy16.h"
