/* Code for RTL register eliminations.
   Copyright (C) 2010-2018 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */

/* Eliminable registers (like a soft argument or frame pointer) are
   widely used in RTL.  These eliminable registers should be replaced
   by real hard registers (like the stack pointer or hard frame
   pointer) plus some offset.  The offsets usually change whenever the
   stack is expanded.  We know the final offsets only at the very end
   of LRA.

   Within LRA, we usually keep the RTL in such a state that the
   eliminable registers can be replaced by just the corresponding hard
   register (without any offset).  To achieve this we should add the
   initial elimination offset at the beginning of LRA and update the
   offsets whenever the stack is expanded.  We need to do this before
   every constraint pass because the choice of offset often affects
   whether a particular address or memory constraint is satisfied.

   We keep RTL code at most time in such state that the virtual
   registers can be changed by just the corresponding hard registers
   (with zero offsets) and we have the right RTL code.	To achieve this
   we should add initial offset at the beginning of LRA work and update
   offsets after each stack expanding.	But actually we update virtual
   registers to the same virtual registers + corresponding offsets
   before every constraint pass because it affects constraint
   satisfaction (e.g. an address displacement became too big for some
   target).

   The final change of eliminable registers to the corresponding hard
   registers are done at the very end of LRA when there were no change
   in offsets anymore:

		     fp + 42	 =>	sp + 42

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "output.h"
#include "rtl-error.h"
#include "lra-int.h"

/* This structure is used to record information about hard register
   eliminations.  */
struct lra_elim_table
{
  /* Hard register number to be eliminated.  */
  int from;
  /* Hard register number used as replacement.	*/
  int to;
  /* Difference between values of the two hard registers above on
     previous iteration.  */
  poly_int64 previous_offset;
  /* Difference between the values on the current iteration.  */
  poly_int64 offset;
  /* Nonzero if this elimination can be done.  */
  bool can_eliminate;
  /* CAN_ELIMINATE since the last check.  */
  bool prev_can_eliminate;
  /* REG rtx for the register to be eliminated.	 We cannot simply
     compare the number since we might then spuriously replace a hard
     register corresponding to a pseudo assigned to the reg to be
     eliminated.  */
  rtx from_rtx;
  /* REG rtx for the replacement.  */
  rtx to_rtx;
};

/* The elimination table.  Each array entry describes one possible way
   of eliminating a register in favor of another.  If there is more
   than one way of eliminating a particular register, the most
   preferred should be specified first.	 */
static struct lra_elim_table *reg_eliminate = 0;

/* This is an intermediate structure to initialize the table.  It has
   exactly the members provided by ELIMINABLE_REGS.  */
static const struct elim_table_1
{
  const int from;
  const int to;
} reg_eliminate_1[] =

  ELIMINABLE_REGS;

#define NUM_ELIMINABLE_REGS ARRAY_SIZE (reg_eliminate_1)

/* Print info about elimination table to file F.  */
static void
print_elim_table (FILE *f)
{
  struct lra_elim_table *ep;

  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      fprintf (f, "%s eliminate %d to %d (offset=",
	       ep->can_eliminate ? "Can" : "Can't", ep->from, ep->to);
      print_dec (ep->offset, f);
      fprintf (f, ", prev_offset=");
      print_dec (ep->previous_offset, f);
      fprintf (f, ")\n");
    }
}

/* Print info about elimination table to stderr.  */
void
lra_debug_elim_table (void)
{
  print_elim_table (stderr);
}

/* Setup possibility of elimination in elimination table element EP to
   VALUE.  Setup FRAME_POINTER_NEEDED if elimination from frame
   pointer to stack pointer is not possible anymore.  */
static void
setup_can_eliminate (struct lra_elim_table *ep, bool value)
{
  ep->can_eliminate = ep->prev_can_eliminate = value;
  if (! value
      && ep->from == FRAME_POINTER_REGNUM && ep->to == STACK_POINTER_REGNUM)
    frame_pointer_needed = 1;
  if (!frame_pointer_needed)
    REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = 0;
}

/* Map: eliminable "from" register -> its current elimination,
   or NULL if none.  The elimination table may contain more than
   one elimination for the same hard register, but this map specifies
   the one that we are currently using.  */
static struct lra_elim_table *elimination_map[FIRST_PSEUDO_REGISTER];

/* When an eliminable hard register becomes not eliminable, we use the
   following special structure to restore original offsets for the
   register.  */
static struct lra_elim_table self_elim_table;

/* Offsets should be used to restore original offsets for eliminable
   hard register which just became not eliminable.  Zero,
   otherwise.  */
static poly_int64_pod self_elim_offsets[FIRST_PSEUDO_REGISTER];

/* Map: hard regno -> RTL presentation.	 RTL presentations of all
   potentially eliminable hard registers are stored in the map.	 */
static rtx eliminable_reg_rtx[FIRST_PSEUDO_REGISTER];

/* Set up ELIMINATION_MAP of the currently used eliminations.  */
static void
setup_elimination_map (void)
{
  int i;
  struct lra_elim_table *ep;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    elimination_map[i] = NULL;
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    if (ep->can_eliminate && elimination_map[ep->from] == NULL)
      elimination_map[ep->from] = ep;
}



/* Compute the sum of X and Y, making canonicalizations assumed in an
   address, namely: sum constant integers, surround the sum of two
   constants with a CONST, put the constant as the second operand, and
   group the constant on the outermost sum.

   This routine assumes both inputs are already in canonical form.  */
static rtx
form_sum (rtx x, rtx y)
{
  machine_mode mode = GET_MODE (x);
  poly_int64 offset;

  if (mode == VOIDmode)
    mode = GET_MODE (y);

  if (mode == VOIDmode)
    mode = Pmode;

  if (poly_int_rtx_p (x, &offset))
    return plus_constant (mode, y, offset);
  else if (poly_int_rtx_p (y, &offset))
    return plus_constant (mode, x, offset);
  else if (CONSTANT_P (x))
    std::swap (x, y);

  if (GET_CODE (x) == PLUS && CONSTANT_P (XEXP (x, 1)))
    return form_sum (XEXP (x, 0), form_sum (XEXP (x, 1), y));

  /* Note that if the operands of Y are specified in the opposite
     order in the recursive calls below, infinite recursion will
     occur.  */
  if (GET_CODE (y) == PLUS && CONSTANT_P (XEXP (y, 1)))
    return form_sum (form_sum (x, XEXP (y, 0)), XEXP (y, 1));

  /* If both constant, encapsulate sum.	 Otherwise, just form sum.  A
     constant will have been placed second.  */
  if (CONSTANT_P (x) && CONSTANT_P (y))
    {
      if (GET_CODE (x) == CONST)
	x = XEXP (x, 0);
      if (GET_CODE (y) == CONST)
	y = XEXP (y, 0);

      return gen_rtx_CONST (VOIDmode, gen_rtx_PLUS (mode, x, y));
    }

  return gen_rtx_PLUS (mode, x, y);
}

/* Return the current substitution hard register of the elimination of
   HARD_REGNO.	If HARD_REGNO is not eliminable, return itself.	 */
int
lra_get_elimination_hard_regno (int hard_regno)
{
  struct lra_elim_table *ep;

  if (hard_regno < 0 || hard_regno >= FIRST_PSEUDO_REGISTER)
    return hard_regno;
  if ((ep = elimination_map[hard_regno]) == NULL)
    return hard_regno;
  return ep->to;
}

/* Return elimination which will be used for hard reg REG, NULL
   otherwise.  */
static struct lra_elim_table *
get_elimination (rtx reg)
{
  int hard_regno;
  struct lra_elim_table *ep;

  lra_assert (REG_P (reg));
  if ((hard_regno = REGNO (reg)) < 0 || hard_regno >= FIRST_PSEUDO_REGISTER)
    return NULL;
  if ((ep = elimination_map[hard_regno]) != NULL)
    return ep->from_rtx != reg ? NULL : ep;
  poly_int64 offset = self_elim_offsets[hard_regno];
  if (known_eq (offset, 0))
    return NULL;
  /* This is an iteration to restore offsets just after HARD_REGNO
     stopped to be eliminable.	*/
  self_elim_table.from = self_elim_table.to = hard_regno;
  self_elim_table.from_rtx
    = self_elim_table.to_rtx
    = eliminable_reg_rtx[hard_regno];
  lra_assert (self_elim_table.from_rtx != NULL);
  self_elim_table.offset = offset;
  return &self_elim_table;
}

/* Transform (subreg (plus reg const)) to (plus (subreg reg) const)
   when it is possible.  Return X or the transformation result if the
   transformation is done.  */
static rtx
move_plus_up (rtx x)
{
  rtx subreg_reg;
  machine_mode x_mode, subreg_reg_mode;
  
  if (GET_CODE (x) != SUBREG || !subreg_lowpart_p (x))
    return x;
  subreg_reg = SUBREG_REG (x);
  x_mode = GET_MODE (x);
  subreg_reg_mode = GET_MODE (subreg_reg);
  if (!paradoxical_subreg_p (x)
      && GET_CODE (subreg_reg) == PLUS
      && CONSTANT_P (XEXP (subreg_reg, 1))
      && GET_MODE_CLASS (x_mode) == MODE_INT
      && GET_MODE_CLASS (subreg_reg_mode) == MODE_INT)
    {
      rtx cst = simplify_subreg (x_mode, XEXP (subreg_reg, 1), subreg_reg_mode,
				 subreg_lowpart_offset (x_mode,
							subreg_reg_mode));
      if (cst && CONSTANT_P (cst))
	return gen_rtx_PLUS (x_mode, lowpart_subreg (x_mode,
						     XEXP (subreg_reg, 0),
						     subreg_reg_mode), cst);
    }
  return x;
}

/* Scan X and replace any eliminable registers (such as fp) with a
   replacement (such as sp) if SUBST_P, plus an offset.  The offset is
   a change in the offset between the eliminable register and its
   substitution if UPDATE_P, or the full offset if FULL_P, or
   otherwise zero.  If FULL_P, we also use the SP offsets for
   elimination to SP.  If UPDATE_P, use UPDATE_SP_OFFSET for updating
   offsets of register elimnable to SP.  If UPDATE_SP_OFFSET is
   non-zero, don't use difference of the offset and the previous
   offset.

   MEM_MODE is the mode of an enclosing MEM.  We need this to know how
   much to adjust a register for, e.g., PRE_DEC.  Also, if we are
   inside a MEM, we are allowed to replace a sum of a hard register
   and the constant zero with the hard register, which we cannot do
   outside a MEM.  In addition, we need to record the fact that a
   hard register is referenced outside a MEM.

   If we make full substitution to SP for non-null INSN, add the insn
   sp offset.  */
rtx
lra_eliminate_regs_1 (rtx_insn *insn, rtx x, machine_mode mem_mode,
		      bool subst_p, bool update_p,
		      poly_int64 update_sp_offset, bool full_p)
{
  enum rtx_code code = GET_CODE (x);
  struct lra_elim_table *ep;
  rtx new_rtx;
  int i, j;
  const char *fmt;
  int copied = 0;

  lra_assert (!update_p || !full_p);
  lra_assert (known_eq (update_sp_offset, 0)
	      || (!subst_p && update_p && !full_p));
  if (! current_function_decl)
    return x;

  switch (code)
    {
    CASE_CONST_ANY:
    case CONST:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case RETURN:
      return x;

    case REG:
      /* First handle the case where we encounter a bare hard register
	 that is eliminable.  Replace it with a PLUS.  */
      if ((ep = get_elimination (x)) != NULL)
	{
	  rtx to = subst_p ? ep->to_rtx : ep->from_rtx;

	  if (maybe_ne (update_sp_offset, 0))
	    {
	      if (ep->to_rtx == stack_pointer_rtx)
		return plus_constant (Pmode, to, update_sp_offset);
	      return to;
	    }
	  else if (update_p)
	    return plus_constant (Pmode, to, ep->offset - ep->previous_offset);
	  else if (full_p)
	    return plus_constant (Pmode, to,
				  ep->offset
				  - (insn != NULL_RTX
				     && ep->to_rtx == stack_pointer_rtx
				     ? lra_get_insn_recog_data (insn)->sp_offset
				     : 0));
	  else
	    return to;
	}
      return x;

    case PLUS:
      /* If this is the sum of an eliminable register and a constant, rework
	 the sum.  */
      if (REG_P (XEXP (x, 0)) && CONSTANT_P (XEXP (x, 1)))
	{
	  if ((ep = get_elimination (XEXP (x, 0))) != NULL)
	    {
	      poly_int64 offset, curr_offset;
	      rtx to = subst_p ? ep->to_rtx : ep->from_rtx;

	      if (! update_p && ! full_p)
		return gen_rtx_PLUS (Pmode, to, XEXP (x, 1));
	      
	      if (maybe_ne (update_sp_offset, 0))
		offset = ep->to_rtx == stack_pointer_rtx ? update_sp_offset : 0;
	      else
		offset = (update_p
			  ? ep->offset - ep->previous_offset : ep->offset);
	      if (full_p && insn != NULL_RTX && ep->to_rtx == stack_pointer_rtx)
		offset -= lra_get_insn_recog_data (insn)->sp_offset;
	      if (poly_int_rtx_p (XEXP (x, 1), &curr_offset)
		  && known_eq (curr_offset, -offset))
		return to;
	      else
		return gen_rtx_PLUS (Pmode, to,
				     plus_constant (Pmode,
						    XEXP (x, 1), offset));
	    }

	  /* If the hard register is not eliminable, we are done since
	     the other operand is a constant.  */
	  return x;
	}

      /* If this is part of an address, we want to bring any constant
	 to the outermost PLUS.  We will do this by doing hard
	 register replacement in our operands and seeing if a constant
	 shows up in one of them.

	 Note that there is no risk of modifying the structure of the
	 insn, since we only get called for its operands, thus we are
	 either modifying the address inside a MEM, or something like
	 an address operand of a load-address insn.  */

      {
	rtx new0 = lra_eliminate_regs_1 (insn, XEXP (x, 0), mem_mode,
					 subst_p, update_p,
					 update_sp_offset, full_p);
	rtx new1 = lra_eliminate_regs_1 (insn, XEXP (x, 1), mem_mode,
					 subst_p, update_p,
					 update_sp_offset, full_p);

	new0 = move_plus_up (new0);
	new1 = move_plus_up (new1);
	if (new0 != XEXP (x, 0) || new1 != XEXP (x, 1))
	  return form_sum (new0, new1);
      }
      return x;

    case MULT:
      /* If this is the product of an eliminable hard register and a
	 constant, apply the distribute law and move the constant out
	 so that we have (plus (mult ..) ..).  This is needed in order
	 to keep load-address insns valid.  This case is pathological.
	 We ignore the possibility of overflow here.  */
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1))
	  && (ep = get_elimination (XEXP (x, 0))) != NULL)
	{
	  rtx to = subst_p ? ep->to_rtx : ep->from_rtx;

	  if (maybe_ne (update_sp_offset, 0))
	    {
	      if (ep->to_rtx == stack_pointer_rtx)
		return plus_constant (Pmode,
				      gen_rtx_MULT (Pmode, to, XEXP (x, 1)),
				      update_sp_offset * INTVAL (XEXP (x, 1)));
	      return gen_rtx_MULT (Pmode, to, XEXP (x, 1));
	    }
	  else if (update_p)
	    return plus_constant (Pmode,
				  gen_rtx_MULT (Pmode, to, XEXP (x, 1)),
				  (ep->offset - ep->previous_offset)
				  * INTVAL (XEXP (x, 1)));
	  else if (full_p)
	    {
	      poly_int64 offset = ep->offset;

	      if (insn != NULL_RTX && ep->to_rtx == stack_pointer_rtx)
		offset -= lra_get_insn_recog_data (insn)->sp_offset;
	      return
		plus_constant (Pmode,
			       gen_rtx_MULT (Pmode, to, XEXP (x, 1)),
			       offset * INTVAL (XEXP (x, 1)));
	    }
	  else
	    return gen_rtx_MULT (Pmode, to, XEXP (x, 1));
	}

      /* fall through */

    case CALL:
    case COMPARE:
    /* See comments before PLUS about handling MINUS.  */
    case MINUS:
    case DIV:	   case UDIV:
    case MOD:	   case UMOD:
    case AND:	   case IOR:	  case XOR:
    case ROTATERT: case ROTATE:
    case ASHIFTRT: case LSHIFTRT: case ASHIFT:
    case NE:	   case EQ:
    case GE:	   case GT:	  case GEU:    case GTU:
    case LE:	   case LT:	  case LEU:    case LTU:
      {
	rtx new0 = lra_eliminate_regs_1 (insn, XEXP (x, 0), mem_mode,
					 subst_p, update_p, 
					 update_sp_offset, full_p);
	rtx new1 = XEXP (x, 1)
		   ? lra_eliminate_regs_1 (insn, XEXP (x, 1), mem_mode,
					   subst_p, update_p,
					   update_sp_offset, full_p) : 0;

	if (new0 != XEXP (x, 0) || new1 != XEXP (x, 1))
	  return gen_rtx_fmt_ee (code, GET_MODE (x), new0, new1);
      }
      return x;

    case EXPR_LIST:
      /* If we have something in XEXP (x, 0), the usual case,
	 eliminate it.	*/
      if (XEXP (x, 0))
	{
	  new_rtx = lra_eliminate_regs_1 (insn, XEXP (x, 0), mem_mode,
					  subst_p, update_p,
					  update_sp_offset, full_p);
	  if (new_rtx != XEXP (x, 0))
	    {
	      /* If this is a REG_DEAD note, it is not valid anymore.
		 Using the eliminated version could result in creating a
		 REG_DEAD note for the stack or frame pointer.	*/
	      if (REG_NOTE_KIND (x) == REG_DEAD)
		return (XEXP (x, 1)
			? lra_eliminate_regs_1 (insn, XEXP (x, 1), mem_mode,
						subst_p, update_p,
						update_sp_offset, full_p)
			: NULL_RTX);

	      x = alloc_reg_note (REG_NOTE_KIND (x), new_rtx, XEXP (x, 1));
	    }
	}

      /* fall through */

    case INSN_LIST:
    case INT_LIST:
      /* Now do eliminations in the rest of the chain.	If this was
	 an EXPR_LIST, this might result in allocating more memory than is
	 strictly needed, but it simplifies the code.  */
      if (XEXP (x, 1))
	{
	  new_rtx = lra_eliminate_regs_1 (insn, XEXP (x, 1), mem_mode,
					  subst_p, update_p,
					  update_sp_offset, full_p);
	  if (new_rtx != XEXP (x, 1))
	    return
	      gen_rtx_fmt_ee (GET_CODE (x), GET_MODE (x),
			      XEXP (x, 0), new_rtx);
	}
      return x;

    case PRE_INC:
    case POST_INC:
    case PRE_DEC:
    case POST_DEC:
      /* We do not support elimination of a register that is modified.
	 elimination_effects has already make sure that this does not
	 happen.  */
      return x;

    case PRE_MODIFY:
    case POST_MODIFY:
      /* We do not support elimination of a hard register that is
	 modified.  LRA has already make sure that this does not
	 happen. The only remaining case we need to consider here is
	 that the increment value may be an eliminable register.  */
      if (GET_CODE (XEXP (x, 1)) == PLUS
	  && XEXP (XEXP (x, 1), 0) == XEXP (x, 0))
	{
	  rtx new_rtx = lra_eliminate_regs_1 (insn, XEXP (XEXP (x, 1), 1),
					      mem_mode, subst_p, update_p,
					      update_sp_offset, full_p);

	  if (new_rtx != XEXP (XEXP (x, 1), 1))
	    return gen_rtx_fmt_ee (code, GET_MODE (x), XEXP (x, 0),
				   gen_rtx_PLUS (GET_MODE (x),
						 XEXP (x, 0), new_rtx));
	}
      return x;

    case STRICT_LOW_PART:
    case NEG:	       case NOT:
    case SIGN_EXTEND:  case ZERO_EXTEND:
    case TRUNCATE:     case FLOAT_EXTEND: case FLOAT_TRUNCATE:
    case FLOAT:	       case FIX:
    case UNSIGNED_FIX: case UNSIGNED_FLOAT:
    case ABS:
    case SQRT:
    case FFS:
    case CLZ:
    case CTZ:
    case POPCOUNT:
    case PARITY:
    case BSWAP:
      new_rtx = lra_eliminate_regs_1 (insn, XEXP (x, 0), mem_mode,
				      subst_p, update_p,
				      update_sp_offset, full_p);
      if (new_rtx != XEXP (x, 0))
	return gen_rtx_fmt_e (code, GET_MODE (x), new_rtx);
      return x;

    case SUBREG:
      new_rtx = lra_eliminate_regs_1 (insn, SUBREG_REG (x), mem_mode,
				      subst_p, update_p,
				      update_sp_offset, full_p);

      if (new_rtx != SUBREG_REG (x))
	{
	  if (MEM_P (new_rtx) && !paradoxical_subreg_p (x))
	    {
	      SUBREG_REG (x) = new_rtx;
	      alter_subreg (&x, false);
	      return x;
	    }
	  else if (! subst_p)
	    {
	      /* LRA can transform subregs itself.  So don't call
		 simplify_gen_subreg until LRA transformations are
		 finished.  Function simplify_gen_subreg can do
		 non-trivial transformations (like truncation) which
		 might make LRA work to fail.  */
	      SUBREG_REG (x) = new_rtx;
	      return x;
	    }
	  else
	    return simplify_gen_subreg (GET_MODE (x), new_rtx,
					GET_MODE (new_rtx), SUBREG_BYTE (x));
	}

      return x;

    case MEM:
      /* Our only special processing is to pass the mode of the MEM to our
	 recursive call and copy the flags.  While we are here, handle this
	 case more efficiently.	 */
      return
	replace_equiv_address_nv
	(x,
	 lra_eliminate_regs_1 (insn, XEXP (x, 0), GET_MODE (x),
			       subst_p, update_p, update_sp_offset, full_p));

    case USE:
      /* Handle insn_list USE that a call to a pure function may generate.  */
      new_rtx = lra_eliminate_regs_1 (insn, XEXP (x, 0), VOIDmode,
				      subst_p, update_p, update_sp_offset, full_p);
      if (new_rtx != XEXP (x, 0))
	return gen_rtx_USE (GET_MODE (x), new_rtx);
      return x;

    case CLOBBER:
    case CLOBBER_HIGH:
    case SET:
      gcc_unreachable ();

    default:
      break;
    }

  /* Process each of our operands recursively.	If any have changed, make a
     copy of the rtx.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	{
	  new_rtx = lra_eliminate_regs_1 (insn, XEXP (x, i), mem_mode,
					  subst_p, update_p,
					  update_sp_offset, full_p);
	  if (new_rtx != XEXP (x, i) && ! copied)
	    {
	      x = shallow_copy_rtx (x);
	      copied = 1;
	    }
	  XEXP (x, i) = new_rtx;
	}
      else if (*fmt == 'E')
	{
	  int copied_vec = 0;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      new_rtx = lra_eliminate_regs_1 (insn, XVECEXP (x, i, j), mem_mode,
					      subst_p, update_p,
					      update_sp_offset, full_p);
	      if (new_rtx != XVECEXP (x, i, j) && ! copied_vec)
		{
		  rtvec new_v = gen_rtvec_v (XVECLEN (x, i),
					     XVEC (x, i)->elem);
		  if (! copied)
		    {
		      x = shallow_copy_rtx (x);
		      copied = 1;
		    }
		  XVEC (x, i) = new_v;
		  copied_vec = 1;
		}
	      XVECEXP (x, i, j) = new_rtx;
	    }
	}
    }

  return x;
}

/* This function is used externally in subsequent passes of GCC.  It
   always does a full elimination of X.	 */
rtx
lra_eliminate_regs (rtx x, machine_mode mem_mode,
		    rtx insn ATTRIBUTE_UNUSED)
{
  return lra_eliminate_regs_1 (NULL, x, mem_mode, true, false, 0, true);
}

/* Stack pointer offset before the current insn relative to one at the
   func start.  RTL insns can change SP explicitly.  We keep the
   changes from one insn to another through this variable.  */
static poly_int64 curr_sp_change;

/* Scan rtx X for references to elimination source or target registers
   in contexts that would prevent the elimination from happening.
   Update the table of eliminables to reflect the changed state.
   MEM_MODE is the mode of an enclosing MEM rtx, or VOIDmode if not
   within a MEM.  */
static void
mark_not_eliminable (rtx x, machine_mode mem_mode)
{
  enum rtx_code code = GET_CODE (x);
  struct lra_elim_table *ep;
  int i, j;
  const char *fmt;
  poly_int64 offset = 0;

  switch (code)
    {
    case PRE_INC:
    case POST_INC:
    case PRE_DEC:
    case POST_DEC:
    case POST_MODIFY:
    case PRE_MODIFY:
      if (XEXP (x, 0) == stack_pointer_rtx
	  && ((code != PRE_MODIFY && code != POST_MODIFY)
	      || (GET_CODE (XEXP (x, 1)) == PLUS
		  && XEXP (x, 0) == XEXP (XEXP (x, 1), 0)
		  && poly_int_rtx_p (XEXP (XEXP (x, 1), 1), &offset))))
	{
	  poly_int64 size = GET_MODE_SIZE (mem_mode);
	  
#ifdef PUSH_ROUNDING
	  /* If more bytes than MEM_MODE are pushed, account for
	     them.  */
	  size = PUSH_ROUNDING (size);
#endif
	  if (code == PRE_DEC || code == POST_DEC)
	    curr_sp_change -= size;
	  else if (code == PRE_INC || code == POST_INC)
	    curr_sp_change += size;
	  else if (code == PRE_MODIFY || code == POST_MODIFY)
	    curr_sp_change += offset;
	}
      else if (REG_P (XEXP (x, 0))
	       && REGNO (XEXP (x, 0)) >= FIRST_PSEUDO_REGISTER)
	{
	  /* If we modify the source of an elimination rule, disable
	     it.  Do the same if it is the destination and not the
	     hard frame register.  */
	  for (ep = reg_eliminate;
	       ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->from_rtx == XEXP (x, 0)
		|| (ep->to_rtx == XEXP (x, 0)
		    && ep->to_rtx != hard_frame_pointer_rtx))
	      setup_can_eliminate (ep, false);
	}
      return;

    case USE:
      if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER)
	/* If using a hard register that is the source of an eliminate
	   we still think can be performed, note it cannot be
	   performed since we don't know how this hard register is
	   used.  */
	for (ep = reg_eliminate;
	     ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	     ep++)
	  if (ep->from_rtx == XEXP (x, 0)
	      && ep->to_rtx != hard_frame_pointer_rtx)
	    setup_can_eliminate (ep, false);
      return;

    case CLOBBER:
      if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER)
	/* If clobbering a hard register that is the replacement
	   register for an elimination we still think can be
	   performed, note that it cannot be performed.	 Otherwise, we
	   need not be concerned about it.  */
	for (ep = reg_eliminate;
	     ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	     ep++)
	  if (ep->to_rtx == XEXP (x, 0)
	      && ep->to_rtx != hard_frame_pointer_rtx)
	    setup_can_eliminate (ep, false);
      return;

    case CLOBBER_HIGH:
      gcc_assert (REG_P (XEXP (x, 0)));
      gcc_assert (REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER);
      for (ep = reg_eliminate;
	   ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	   ep++)
	if (reg_is_clobbered_by_clobber_high (ep->to_rtx, XEXP (x, 0)))
	  setup_can_eliminate (ep, false);
      return;

    case SET:
      if (SET_DEST (x) == stack_pointer_rtx
	  && GET_CODE (SET_SRC (x)) == PLUS
	  && XEXP (SET_SRC (x), 0) == SET_DEST (x)
	  && poly_int_rtx_p (XEXP (SET_SRC (x), 1), &offset))
	{
	  curr_sp_change += offset;
	  return;
	}
      if (! REG_P (SET_DEST (x))
	  || REGNO (SET_DEST (x)) >= FIRST_PSEUDO_REGISTER)
	mark_not_eliminable (SET_DEST (x), mem_mode);
      else
	{
	  /* See if this is setting the replacement hard register for
	     an elimination.
	     
	     If DEST is the hard frame pointer, we do nothing because
	     we assume that all assignments to the frame pointer are
	     for non-local gotos and are being done at a time when
	     they are valid and do not disturb anything else.  Some
	     machines want to eliminate a fake argument pointer (or
	     even a fake frame pointer) with either the real frame
	     pointer or the stack pointer.  Assignments to the hard
	     frame pointer must not prevent this elimination.  */
	  for (ep = reg_eliminate;
	       ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->to_rtx == SET_DEST (x)
		&& SET_DEST (x) != hard_frame_pointer_rtx)
	      setup_can_eliminate (ep, false);
	}
      
      mark_not_eliminable (SET_SRC (x), mem_mode);
      return;

    case MEM:
      /* Our only special processing is to pass the mode of the MEM to
	 our recursive call.  */
      mark_not_eliminable (XEXP (x, 0), GET_MODE (x));
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	mark_not_eliminable (XEXP (x, i), mem_mode);
      else if (*fmt == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  mark_not_eliminable (XVECEXP (x, i, j), mem_mode);
    }
}



#ifdef HARD_FRAME_POINTER_REGNUM

/* Search INSN's reg notes to see whether the destination is equal to
   WHAT + C for some constant C.  Return true if so, storing C in
   *OFFSET_OUT and removing the reg note.  */
static bool
remove_reg_equal_offset_note (rtx_insn *insn, rtx what, poly_int64 *offset_out)
{
  rtx link, *link_loc;

  for (link_loc = &REG_NOTES (insn);
       (link = *link_loc) != NULL_RTX;
       link_loc = &XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_EQUAL
	&& GET_CODE (XEXP (link, 0)) == PLUS
	&& XEXP (XEXP (link, 0), 0) == what
	&& poly_int_rtx_p (XEXP (XEXP (link, 0), 1), offset_out))
      {
	*link_loc = XEXP (link, 1);
	return true;
      }
  return false;
}

#endif

/* Scan INSN and eliminate all eliminable hard registers in it.

   If REPLACE_P is true, do the replacement destructively.  Also
   delete the insn as dead it if it is setting an eliminable register.

   If REPLACE_P is false, just update the offsets while keeping the
   base register the same.  If FIRST_P, use the sp offset for
   elimination to sp.  Otherwise, use UPDATE_SP_OFFSET for this.  If
   UPDATE_SP_OFFSET is non-zero, don't use difference of the offset
   and the previous offset.  Attach the note about used elimination
   for insns setting frame pointer to update elimination easy (without
   parsing already generated elimination insns to find offset
   previously used) in future.  */

void
eliminate_regs_in_insn (rtx_insn *insn, bool replace_p, bool first_p,
			poly_int64 update_sp_offset)
{
  int icode = recog_memoized (insn);
  rtx old_set = single_set (insn);
  bool validate_p;
  int i;
  rtx substed_operand[MAX_RECOG_OPERANDS];
  rtx orig_operand[MAX_RECOG_OPERANDS];
  struct lra_elim_table *ep;
  rtx plus_src, plus_cst_src;
  lra_insn_recog_data_t id;
  struct lra_static_insn_data *static_id;

  if (icode < 0 && asm_noperands (PATTERN (insn)) < 0 && ! DEBUG_INSN_P (insn))
    {
      lra_assert (GET_CODE (PATTERN (insn)) == USE
		  || GET_CODE (PATTERN (insn)) == CLOBBER
		  || GET_CODE (PATTERN (insn)) == ASM_INPUT);
      return;
    }

  /* Check for setting an eliminable register.	*/
  if (old_set != 0 && REG_P (SET_DEST (old_set))
      && (ep = get_elimination (SET_DEST (old_set))) != NULL)
    {
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if (ep->from_rtx == SET_DEST (old_set) && ep->can_eliminate)
	  {
	    bool delete_p = replace_p;
	    
#ifdef HARD_FRAME_POINTER_REGNUM
	    if (ep->from == FRAME_POINTER_REGNUM
		&& ep->to == HARD_FRAME_POINTER_REGNUM)
	      /* If this is setting the frame pointer register to the
		 hardware frame pointer register and this is an
		 elimination that will be done (tested above), this
		 insn is really adjusting the frame pointer downward
		 to compensate for the adjustment done before a
		 nonlocal goto.  */
	      {
		rtx src = SET_SRC (old_set);
		poly_int64 offset = 0;

		/* We should never process such insn with non-zero
		   UPDATE_SP_OFFSET.  */
		lra_assert (known_eq (update_sp_offset, 0));
		
		if (remove_reg_equal_offset_note (insn, ep->to_rtx, &offset)
		    || strip_offset (src, &offset) == ep->to_rtx)
		  {
		    if (replace_p)
		      {
			SET_DEST (old_set) = ep->to_rtx;
			lra_update_insn_recog_data (insn);
			return;
		      }
		    offset -= (ep->offset - ep->previous_offset);
		    src = plus_constant (Pmode, ep->to_rtx, offset);
		    
		    /* First see if this insn remains valid when we
		       make the change.  If not, keep the INSN_CODE
		       the same and let the constraint pass fit it
		       up.  */
		    validate_change (insn, &SET_SRC (old_set), src, 1);
		    validate_change (insn, &SET_DEST (old_set),
				     ep->from_rtx, 1);
		    if (! apply_change_group ())
		      {
			SET_SRC (old_set) = src;
			SET_DEST (old_set) = ep->from_rtx;
		      }
		    lra_update_insn_recog_data (insn);
		    /* Add offset note for future updates.  */
		    add_reg_note (insn, REG_EQUAL, copy_rtx (src));
		    return;
		  }
	      }
#endif
	    
	    /* This insn isn't serving a useful purpose.  We delete it
	       when REPLACE is set.  */
	    if (delete_p)
	      lra_delete_dead_insn (insn);
	    return;
	  }
    }

  /* We allow one special case which happens to work on all machines we
     currently support: a single set with the source or a REG_EQUAL
     note being a PLUS of an eliminable register and a constant.  */
  plus_src = plus_cst_src = 0;
  poly_int64 offset = 0;
  if (old_set && REG_P (SET_DEST (old_set)))
    {
      if (GET_CODE (SET_SRC (old_set)) == PLUS)
	plus_src = SET_SRC (old_set);
      /* First see if the source is of the form (plus (...) CST).  */
      if (plus_src && poly_int_rtx_p (XEXP (plus_src, 1), &offset))
	plus_cst_src = plus_src;
      /* Check that the first operand of the PLUS is a hard reg or
	 the lowpart subreg of one.  */
      if (plus_cst_src)
	{
	  rtx reg = XEXP (plus_cst_src, 0);

	  if (GET_CODE (reg) == SUBREG && subreg_lowpart_p (reg))
	    reg = SUBREG_REG (reg);

	  if (!REG_P (reg) || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
	    plus_cst_src = 0;
	}
    }
  if (plus_cst_src)
    {
      rtx reg = XEXP (plus_cst_src, 0);

      if (GET_CODE (reg) == SUBREG)
	reg = SUBREG_REG (reg);

      if (REG_P (reg) && (ep = get_elimination (reg)) != NULL)
	{
	  rtx to_rtx = replace_p ? ep->to_rtx : ep->from_rtx;

	  if (! replace_p)
	    {
	      if (known_eq (update_sp_offset, 0))
		offset += (ep->offset - ep->previous_offset);
	      if (ep->to_rtx == stack_pointer_rtx)
		{
		  if (first_p)
		    offset -= lra_get_insn_recog_data (insn)->sp_offset;
		  else
		    offset += update_sp_offset;
		}
	      offset = trunc_int_for_mode (offset, GET_MODE (plus_cst_src));
	    }

	  if (GET_CODE (XEXP (plus_cst_src, 0)) == SUBREG)
	    to_rtx = gen_lowpart (GET_MODE (XEXP (plus_cst_src, 0)), to_rtx);
	  /* If we have a nonzero offset, and the source is already a
	     simple REG, the following transformation would increase
	     the cost of the insn by replacing a simple REG with (plus
	     (reg sp) CST).  So try only when we already had a PLUS
	     before.  */
	  if (known_eq (offset, 0) || plus_src)
	    {
	      rtx new_src = plus_constant (GET_MODE (to_rtx), to_rtx, offset);

	      old_set = single_set (insn);

	      /* First see if this insn remains valid when we make the
		 change.  If not, try to replace the whole pattern
		 with a simple set (this may help if the original insn
		 was a PARALLEL that was only recognized as single_set
		 due to REG_UNUSED notes).  If this isn't valid
		 either, keep the INSN_CODE the same and let the
		 constraint pass fix it up.  */
	      if (! validate_change (insn, &SET_SRC (old_set), new_src, 0))
		{
		  rtx new_pat = gen_rtx_SET (SET_DEST (old_set), new_src);

		  if (! validate_change (insn, &PATTERN (insn), new_pat, 0))
		    SET_SRC (old_set) = new_src;
		}
	      lra_update_insn_recog_data (insn);
	      /* This can't have an effect on elimination offsets, so skip
		 right to the end.  */
	      return;
	    }
	}
    }

  /* Eliminate all eliminable registers occurring in operands that
     can be handled by the constraint pass.  */
  id = lra_get_insn_recog_data (insn);
  static_id = id->insn_static_data;
  validate_p = false;
  for (i = 0; i < static_id->n_operands; i++)
    {
      orig_operand[i] = *id->operand_loc[i];
      substed_operand[i] = *id->operand_loc[i];

      /* For an asm statement, every operand is eliminable.  */
      if (icode < 0 || insn_data[icode].operand[i].eliminable)
	{
	  /* Check for setting a hard register that we know about.  */
	  if (static_id->operand[i].type != OP_IN
	      && REG_P (orig_operand[i]))
	    {
	      /* If we are assigning to a hard register that can be
		 eliminated, it must be as part of a PARALLEL, since
		 the code above handles single SETs.  This reg can not
		 be longer eliminated -- it is forced by
		 mark_not_eliminable.  */
	      for (ep = reg_eliminate;
		   ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
		   ep++)
		lra_assert (ep->from_rtx != orig_operand[i]
			    || ! ep->can_eliminate);
	    }

	  /* Companion to the above plus substitution, we can allow
	     invariants as the source of a plain move.	*/
	  substed_operand[i]
	    = lra_eliminate_regs_1 (insn, *id->operand_loc[i], VOIDmode,
				    replace_p, ! replace_p && ! first_p,
				    update_sp_offset, first_p);
	  if (substed_operand[i] != orig_operand[i])
	    validate_p = true;
	}
    }

  if (! validate_p)
    return;

  /* Substitute the operands; the new values are in the substed_operand
     array.  */
  for (i = 0; i < static_id->n_operands; i++)
    *id->operand_loc[i] = substed_operand[i];
  for (i = 0; i < static_id->n_dups; i++)
    *id->dup_loc[i] = substed_operand[(int) static_id->dup_num[i]];

  /* If we had a move insn but now we don't, re-recognize it.
     This will cause spurious re-recognition if the old move had a
     PARALLEL since the new one still will, but we can't call
     single_set without having put new body into the insn and the
     re-recognition won't hurt in this rare case.  */
  id = lra_update_insn_recog_data (insn);
  static_id = id->insn_static_data;
}

/* Spill pseudos which are assigned to hard registers in SET.  Add
   affected insns for processing in the subsequent constraint
   pass.  */
static void
spill_pseudos (HARD_REG_SET set)
{
  int i;
  bitmap_head to_process;
  rtx_insn *insn;

  if (hard_reg_set_empty_p (set))
    return;
  if (lra_dump_file != NULL)
    {
      fprintf (lra_dump_file, "	   Spilling non-eliminable hard regs:");
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (set, i))
	  fprintf (lra_dump_file, " %d", i);
      fprintf (lra_dump_file, "\n");
    }
  bitmap_initialize (&to_process, &reg_obstack);
  for (i = FIRST_PSEUDO_REGISTER; i < max_reg_num (); i++)
    if (lra_reg_info[i].nrefs != 0 && reg_renumber[i] >= 0
	&& overlaps_hard_reg_set_p (set,
				    PSEUDO_REGNO_MODE (i), reg_renumber[i]))
      {
	if (lra_dump_file != NULL)
	  fprintf (lra_dump_file, "	 Spilling r%d(%d)\n",
		   i, reg_renumber[i]);
	reg_renumber[i] = -1;
	bitmap_ior_into (&to_process, &lra_reg_info[i].insn_bitmap);
      }
  IOR_HARD_REG_SET (lra_no_alloc_regs, set);
  for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
    if (bitmap_bit_p (&to_process, INSN_UID (insn)))
      {
	lra_push_insn (insn);
	lra_set_used_insn_alternative (insn, LRA_UNKNOWN_ALT);
      }
  bitmap_clear (&to_process);
}

/* Update all offsets and possibility for elimination on eliminable
   registers.  Spill pseudos assigned to registers which are
   uneliminable, update LRA_NO_ALLOC_REGS and ELIMINABLE_REG_SET.  Add
   insns to INSNS_WITH_CHANGED_OFFSETS containing eliminable hard
   registers whose offsets should be changed.  Return true if any
   elimination offset changed.  */
static bool
update_reg_eliminate (bitmap insns_with_changed_offsets)
{
  bool prev, result;
  struct lra_elim_table *ep, *ep1;
  HARD_REG_SET temp_hard_reg_set;

  targetm.compute_frame_layout ();

  /* Clear self elimination offsets.  */
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    self_elim_offsets[ep->from] = 0;
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      /* If it is a currently used elimination: update the previous
	 offset.  */
      if (elimination_map[ep->from] == ep)
	ep->previous_offset = ep->offset;

      prev = ep->prev_can_eliminate;
      setup_can_eliminate (ep, targetm.can_eliminate (ep->from, ep->to));
      if (ep->can_eliminate && ! prev)
	{
	  /* It is possible that not eliminable register becomes
	     eliminable because we took other reasons into account to
	     set up eliminable regs in the initial set up.  Just
	     ignore new eliminable registers.  */
	  setup_can_eliminate (ep, false);
	  continue;
	}
      if (ep->can_eliminate != prev && elimination_map[ep->from] == ep)
	{
	  /* We cannot use this elimination anymore -- find another
	     one.  */
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file,
		     "	Elimination %d to %d is not possible anymore\n",
		     ep->from, ep->to);
	  /* If after processing RTL we decides that SP can be used as
	     a result of elimination, it can not be changed.  */
	  gcc_assert ((ep->to_rtx != stack_pointer_rtx)
		      || (ep->from < FIRST_PSEUDO_REGISTER
			  && fixed_regs [ep->from]));
	  /* Mark that is not eliminable anymore.  */
	  elimination_map[ep->from] = NULL;
	  for (ep1 = ep + 1; ep1 < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep1++)
	    if (ep1->can_eliminate && ep1->from == ep->from)
	      break;
	  if (ep1 < &reg_eliminate[NUM_ELIMINABLE_REGS])
	    {
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file, "    Using elimination %d to %d now\n",
			 ep1->from, ep1->to);
	      lra_assert (known_eq (ep1->previous_offset, 0));
	      ep1->previous_offset = ep->offset;
	    }
	  else
	    {
	      /* There is no elimination anymore just use the hard
		 register `from' itself.  Setup self elimination
		 offset to restore the original offset values.	*/
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file, "    %d is not eliminable at all\n",
			 ep->from);
	      self_elim_offsets[ep->from] = -ep->offset;
	      if (maybe_ne (ep->offset, 0))
		bitmap_ior_into (insns_with_changed_offsets,
				 &lra_reg_info[ep->from].insn_bitmap);
	    }
	}

      INITIAL_ELIMINATION_OFFSET (ep->from, ep->to, ep->offset);
    }
  setup_elimination_map ();
  result = false;
  CLEAR_HARD_REG_SET (temp_hard_reg_set);
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    if (elimination_map[ep->from] == NULL)
      add_to_hard_reg_set (&temp_hard_reg_set, Pmode, ep->from);
    else if (elimination_map[ep->from] == ep)
      {
	/* Prevent the hard register into which we eliminate from
	   the usage for pseudos.  */
        if (ep->from != ep->to)
	  add_to_hard_reg_set (&temp_hard_reg_set, Pmode, ep->to);
	if (maybe_ne (ep->previous_offset, ep->offset))
	  {
	    bitmap_ior_into (insns_with_changed_offsets,
			     &lra_reg_info[ep->from].insn_bitmap);

	    /* Update offset when the eliminate offset have been
	       changed.  */
	    lra_update_reg_val_offset (lra_reg_info[ep->from].val,
				       ep->offset - ep->previous_offset);
	    result = true;
	  }
      }
  IOR_HARD_REG_SET (lra_no_alloc_regs, temp_hard_reg_set);
  AND_COMPL_HARD_REG_SET (eliminable_regset, temp_hard_reg_set);
  spill_pseudos (temp_hard_reg_set);
  return result;
}

/* Initialize the table of hard registers to eliminate.
   Pre-condition: global flag frame_pointer_needed has been set before
   calling this function.  */
static void
init_elim_table (void)
{
  struct lra_elim_table *ep;
  bool value_p;
  const struct elim_table_1 *ep1;

  if (!reg_eliminate)
    reg_eliminate = XCNEWVEC (struct lra_elim_table, NUM_ELIMINABLE_REGS);

  memset (self_elim_offsets, 0, sizeof (self_elim_offsets));
  /* Initiate member values which will be never changed.  */
  self_elim_table.can_eliminate = self_elim_table.prev_can_eliminate = true;
  self_elim_table.previous_offset = 0;

  for (ep = reg_eliminate, ep1 = reg_eliminate_1;
       ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++, ep1++)
    {
      ep->offset = ep->previous_offset = 0;
      ep->from = ep1->from;
      ep->to = ep1->to;
      value_p = (targetm.can_eliminate (ep->from, ep->to)
		 && ! (ep->to == STACK_POINTER_REGNUM
		       && frame_pointer_needed
		       && (! SUPPORTS_STACK_ALIGNMENT
			   || ! stack_realign_fp)));
      setup_can_eliminate (ep, value_p);
    }

  /* Build the FROM and TO REG rtx's.  Note that code in gen_rtx_REG
     will cause, e.g., gen_rtx_REG (Pmode, STACK_POINTER_REGNUM) to
     equal stack_pointer_rtx.  We depend on this. Threfore we switch
     off that we are in LRA temporarily.  */
  lra_in_progress = 0;
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      ep->from_rtx = gen_rtx_REG (Pmode, ep->from);
      ep->to_rtx = gen_rtx_REG (Pmode, ep->to);
      eliminable_reg_rtx[ep->from] = ep->from_rtx;
    }
  lra_in_progress = 1;
}

/* Function for initialization of elimination once per function.  It
   sets up sp offset for each insn.  */
static void
init_elimination (void)
{
  bool stop_to_sp_elimination_p;
  basic_block bb;
  rtx_insn *insn;
  struct lra_elim_table *ep;

  init_elim_table ();
  FOR_EACH_BB_FN (bb, cfun)
    {
      curr_sp_change = 0;
      stop_to_sp_elimination_p = false;
      FOR_BB_INSNS (bb, insn)
	if (INSN_P (insn))
	  {
	    lra_get_insn_recog_data (insn)->sp_offset = curr_sp_change;
	    if (NONDEBUG_INSN_P (insn))
	      {
		mark_not_eliminable (PATTERN (insn), VOIDmode);
		if (maybe_ne (curr_sp_change, 0)
		    && find_reg_note (insn, REG_LABEL_OPERAND, NULL_RTX))
		  stop_to_sp_elimination_p = true;
	      }
	  }
      if (! frame_pointer_needed
	  && (maybe_ne (curr_sp_change, 0) || stop_to_sp_elimination_p)
	  && bb->succs && bb->succs->length () != 0)
	for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	  if (ep->to == STACK_POINTER_REGNUM)
	    setup_can_eliminate (ep, false);
    }
  setup_elimination_map ();
}

/* Eliminate hard reg given by its location LOC.  */
void
lra_eliminate_reg_if_possible (rtx *loc)
{
  int regno;
  struct lra_elim_table *ep;

  lra_assert (REG_P (*loc));
  if ((regno = REGNO (*loc)) >= FIRST_PSEUDO_REGISTER
      || ! TEST_HARD_REG_BIT (lra_no_alloc_regs, regno))
    return;
  if ((ep = get_elimination (*loc)) != NULL)
    *loc = ep->to_rtx;
}

/* Do (final if FINAL_P or first if FIRST_P) elimination in INSN.  Add
   the insn for subsequent processing in the constraint pass, update
   the insn info.  */
static void
process_insn_for_elimination (rtx_insn *insn, bool final_p, bool first_p)
{
  eliminate_regs_in_insn (insn, final_p, first_p, 0);
  if (! final_p)
    {
      /* Check that insn changed its code.  This is a case when a move
	 insn becomes an add insn and we do not want to process the
	 insn as a move anymore.  */
      int icode = recog (PATTERN (insn), insn, 0);

      if (icode >= 0 && icode != INSN_CODE (insn))
	{
	  INSN_CODE (insn) = icode;
	  lra_update_insn_recog_data (insn);
	}
      lra_update_insn_regno_info (insn);
      lra_push_insn (insn);
      lra_set_used_insn_alternative (insn, LRA_UNKNOWN_ALT);
    }
}

/* Entry function to do final elimination if FINAL_P or to update
   elimination register offsets (FIRST_P if we are doing it the first
   time).  */
void
lra_eliminate (bool final_p, bool first_p)
{
  unsigned int uid;
  bitmap_head insns_with_changed_offsets;
  bitmap_iterator bi;
  struct lra_elim_table *ep;

  gcc_assert (! final_p || ! first_p);

  timevar_push (TV_LRA_ELIMINATE);

  if (first_p)
    init_elimination ();

  bitmap_initialize (&insns_with_changed_offsets, &reg_obstack);
  if (final_p)
    {
      if (flag_checking)
	{
	  update_reg_eliminate (&insns_with_changed_offsets);
	  gcc_assert (bitmap_empty_p (&insns_with_changed_offsets));
	}
      /* We change eliminable hard registers in insns so we should do
	 this for all insns containing any eliminable hard
	 register.  */
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if (elimination_map[ep->from] != NULL)
	  bitmap_ior_into (&insns_with_changed_offsets,
			   &lra_reg_info[ep->from].insn_bitmap);
    }
  else if (! update_reg_eliminate (&insns_with_changed_offsets))
    goto lra_eliminate_done;
  if (lra_dump_file != NULL)
    {
      fprintf (lra_dump_file, "New elimination table:\n");
      print_elim_table (lra_dump_file);
    }
  EXECUTE_IF_SET_IN_BITMAP (&insns_with_changed_offsets, 0, uid, bi)
    /* A dead insn can be deleted in process_insn_for_elimination.  */
    if (lra_insn_recog_data[uid] != NULL)
      process_insn_for_elimination (lra_insn_recog_data[uid]->insn,
				    final_p, first_p);
  bitmap_clear (&insns_with_changed_offsets);

lra_eliminate_done:
  timevar_pop (TV_LRA_ELIMINATE);
}
