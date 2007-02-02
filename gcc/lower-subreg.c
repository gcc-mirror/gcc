/* Decompose multiword subregs.
   Copyright (C) 2007 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>
		  Ian Lance Taylor <iant@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "machmode.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "timevar.h"
#include "flags.h"
#include "insn-config.h"
#include "obstack.h"
#include "basic-block.h"
#include "recog.h"
#include "bitmap.h"
#include "expr.h"
#include "regs.h"
#include "tree-pass.h"

#ifdef STACK_GROWS_DOWNWARD
# undef STACK_GROWS_DOWNWARD
# define STACK_GROWS_DOWNWARD 1
#else
# define STACK_GROWS_DOWNWARD 0
#endif

DEF_VEC_P (bitmap);
DEF_VEC_ALLOC_P (bitmap,heap);

/* Decompose multi-word pseudo-registers into individual
   pseudo-registers when possible.  This is possible when all the uses
   of a multi-word register are via SUBREG, or are copies of the
   register to another location.  Breaking apart the register permits
   more CSE and permits better register allocation.  */

/* Bit N in this bitmap is set if regno N is used in a context in
   which we can decompose it.  */
static bitmap decomposable_context;

/* Bit N in this bitmap is set if regno N is used in a context in
   which it can not be decomposed.  */
static bitmap non_decomposable_context;

/* Bit N in the bitmap in element M of this array is set if there is a
   copy from reg M to reg N.  */
static VEC(bitmap,heap) *reg_copy_graph;

/* If INSN is a single set between two objects, return the single set.
   Such an insn can always be decomposed.  INSN should have been
   passed to recog and extract_insn before this is called.  */

static rtx
simple_move (rtx insn)
{
  rtx x;
  rtx set;
  enum machine_mode mode;

  if (recog_data.n_operands != 2)
    return NULL_RTX;

  set = single_set (insn);
  if (!set)
    return NULL_RTX;

  x = SET_DEST (set);
  if (x != recog_data.operand[0] && x != recog_data.operand[1])
    return NULL_RTX;
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (!OBJECT_P (x))
    return NULL_RTX;
  if (MEM_P (x)
      && (MEM_VOLATILE_P (x)
	  || mode_dependent_address_p (XEXP (x, 0))))
    return NULL_RTX;

  x = SET_SRC (set);
  if (x != recog_data.operand[0] && x != recog_data.operand[1])
    return NULL_RTX;
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (!OBJECT_P (x) && GET_CODE (x) != ASM_OPERANDS)
    return NULL_RTX;
  if (MEM_P (x)
      && (MEM_VOLATILE_P (x)
	  || mode_dependent_address_p (XEXP (x, 0))))
    return NULL_RTX;

  /* We try to decompose in integer modes, to avoid generating
     inefficient code copying between integer and floating point
     registers.  That means that we can't decompose if this is a
     non-integer mode for which there is no integer mode of the same
     size.  */
  mode = GET_MODE (SET_SRC (set));
  if (!SCALAR_INT_MODE_P (mode)
      && (mode_for_size (GET_MODE_SIZE (mode) * BITS_PER_UNIT, MODE_INT, 0)
	  == BLKmode))
    return NULL_RTX;

  return set;
}

/* If SET is a copy from one multi-word pseudo-register to another,
   record that in reg_copy_graph.  Return whether it is such a
   copy.  */

static bool
find_pseudo_copy (rtx set)
{
  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);
  unsigned int rd, rs;
  bitmap b;

  if (!REG_P (dest) || !REG_P (src))
    return false;

  rd = REGNO (dest);
  rs = REGNO (src);
  if (HARD_REGISTER_NUM_P (rd) || HARD_REGISTER_NUM_P (rs))
    return false;

  if (GET_MODE_SIZE (GET_MODE (dest)) <= UNITS_PER_WORD)
    return false;

  b = VEC_index (bitmap, reg_copy_graph, rs);
  if (b == NULL)
    {
      b = BITMAP_ALLOC (NULL);
      VEC_replace (bitmap, reg_copy_graph, rs, b);
    }

  bitmap_set_bit (b, rd);

  return true;
}

/* Look through the registers in DECOMPOSABLE_CONTEXT.  For each case
   where they are copied to another register, add the register to
   which they are copied to DECOMPOSABLE_CONTEXT.  Use
   NON_DECOMPOSABLE_CONTEXT to limit this--we don't bother to track
   copies of registers which are in NON_DECOMPOSABLE_CONTEXT.  */

static void
propagate_pseudo_copies (void)
{
  bitmap queue, propagate;

  queue = BITMAP_ALLOC (NULL);
  propagate = BITMAP_ALLOC (NULL);

  bitmap_copy (queue, decomposable_context);
  do
    {
      bitmap_iterator iter;
      unsigned int i;

      bitmap_clear (propagate);

      EXECUTE_IF_SET_IN_BITMAP (queue, 0, i, iter)
	{
	  bitmap b = VEC_index (bitmap, reg_copy_graph, i);
	  if (b)
	    bitmap_ior_and_compl_into (propagate, b, non_decomposable_context);
	}

      bitmap_and_compl (queue, propagate, decomposable_context);
      bitmap_ior_into (decomposable_context, propagate);
    }
  while (!bitmap_empty_p (queue));

  BITMAP_FREE (queue);
  BITMAP_FREE (propagate);
}

/* A pointer to one of these values is passed to
   find_decomposable_subregs via for_each_rtx.  */

enum classify_move_insn
{
  /* Not a simple move from one location to another.  */
  NOT_SIMPLE_MOVE,
  /* A simple move from one pseudo-register to another with no
     REG_RETVAL note.  */
  SIMPLE_PSEUDO_REG_MOVE,
  /* A simple move involving a non-pseudo-register, or from one
     pseudo-register to another with a REG_RETVAL note.  */
  SIMPLE_MOVE
};

/* This is called via for_each_rtx.  If we find a SUBREG which we
   could use to decompose a pseudo-register, set a bit in
   DECOMPOSABLE_CONTEXT.  If we find an unadorned register which is
   not a simple pseudo-register copy, DATA will point at the type of
   move, and we set a bit in DECOMPOSABLE_CONTEXT or
   NON_DECOMPOSABLE_CONTEXT as appropriate.  */

static int
find_decomposable_subregs (rtx *px, void *data)
{
  enum classify_move_insn *pcmi = (enum classify_move_insn *) data;
  rtx x = *px;

  if (x == NULL_RTX)
    return 0;

  if (GET_CODE (x) == SUBREG)
    {
      rtx inner = SUBREG_REG (x);
      unsigned int regno, outer_size, inner_size, outer_words, inner_words;

      if (!REG_P (inner))
	return 0;

      regno = REGNO (inner);
      if (HARD_REGISTER_NUM_P (regno))
	return -1;

      outer_size = GET_MODE_SIZE (GET_MODE (x));
      inner_size = GET_MODE_SIZE (GET_MODE (inner));
      outer_words = (outer_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      inner_words = (inner_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

      /* We only try to decompose single word subregs of multi-word
	 registers.  When we find one, we return -1 to avoid iterating
	 over the inner register.

	 ??? This doesn't allow, e.g., DImode subregs of TImode values
	 on 32-bit targets.  We would need to record the way the
	 pseudo-register was used, and only decompose if all the uses
	 were the same number and size of pieces.  Hopefully this
	 doesn't happen much.  */

      if (outer_words == 1 && inner_words > 1)
	{
	  bitmap_set_bit (decomposable_context, regno);
	  return -1;
	}
    }
  else if (GET_CODE (x) == REG)
    {
      unsigned int regno;

      /* We will see an outer SUBREG before we see the inner REG, so
	 when we see a plain REG here it means a direct reference to
	 the register.

	 If this is not a simple copy from one location to another,
	 then we can not decompose this register.  If this is a simple
	 copy from one pseudo-register to another, with no REG_RETVAL
	 note, and the mode is right, then we mark the register as
	 decomposable.  Otherwise we don't say anything about this
	 register--it could be decomposed, but whether that would be
	 profitable depends upon how it is used elsewhere.

	 We only set bits in the bitmap for multi-word
	 pseudo-registers, since those are the only ones we care about
	 and it keeps the size of the bitmaps down.  */

      regno = REGNO (x);
      if (!HARD_REGISTER_NUM_P (regno)
	  && GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	{
	  switch (*pcmi)
	    {
	    case NOT_SIMPLE_MOVE:
	      bitmap_set_bit (non_decomposable_context, regno);
	      break;
	    case SIMPLE_PSEUDO_REG_MOVE:
	      if (MODES_TIEABLE_P (GET_MODE (x), word_mode))
		bitmap_set_bit (decomposable_context, regno);
	      break;
	    case SIMPLE_MOVE:
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
    }

  return 0;
}

/* Decompose REGNO into word-sized components.  We smash the REG node
   in place.  This ensures that (1) something goes wrong quickly if we
   fail to make some replacement, and (2) the debug information inside
   the symbol table is automatically kept up to date.  */

static void
decompose_register (unsigned int regno)
{
  rtx reg;
  unsigned int words, i;
  rtvec v;

  reg = regno_reg_rtx[regno];

  regno_reg_rtx[regno] = NULL_RTX;
  clear_reg_info_regno (regno);

  words = GET_MODE_SIZE (GET_MODE (reg));
  words = (words + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  v = rtvec_alloc (words);
  for (i = 0; i < words; ++i)
    RTVEC_ELT (v, i) = gen_reg_rtx_offset (reg, word_mode, i * UNITS_PER_WORD);

  PUT_CODE (reg, CONCATN);
  XVEC (reg, 0) = v;

  if (dump_file)
    {
      fprintf (dump_file, "; Splitting reg %u ->", regno);
      for (i = 0; i < words; ++i)
	fprintf (dump_file, " %u", REGNO (XVECEXP (reg, 0, i)));
      fputc ('\n', dump_file);
    }
}

/* Get a SUBREG of a CONCATN.  */

static rtx
simplify_subreg_concatn (enum machine_mode outermode, rtx op,
			 unsigned int byte)
{
  unsigned int inner_size;
  enum machine_mode innermode;
  rtx part;
  unsigned int final_offset;

  gcc_assert (GET_CODE (op) == CONCATN);
  gcc_assert (byte % GET_MODE_SIZE (outermode) == 0);

  innermode = GET_MODE (op);
  gcc_assert (byte < GET_MODE_SIZE (innermode));
  gcc_assert (GET_MODE_SIZE (outermode) <= GET_MODE_SIZE (innermode));

  inner_size = GET_MODE_SIZE (innermode) / XVECLEN (op, 0);
  part = XVECEXP (op, 0, byte / inner_size);
  final_offset = byte % inner_size;
  if (final_offset + GET_MODE_SIZE (outermode) > inner_size)
    return NULL_RTX;

  return simplify_gen_subreg (outermode, part, GET_MODE (part), final_offset);
}

/* Wrapper around simplify_gen_subreg which handles CONCATN.  */

static rtx
simplify_gen_subreg_concatn (enum machine_mode outermode, rtx op,
			     enum machine_mode innermode, unsigned int byte)
{
  rtx ret;

  /* We have to handle generating a SUBREG of a SUBREG of a CONCATN.
     If OP is a SUBREG of a CONCATN, then it must be a simple mode
     change with the same size and offset 0, or it must extract a
     part.  We shouldn't see anything else here.  */
  if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == CONCATN)
    {
      rtx op2;

      if ((GET_MODE_SIZE (GET_MODE (op))
	   == GET_MODE_SIZE (GET_MODE (SUBREG_REG (op))))
	  && SUBREG_BYTE (op) == 0)
	return simplify_gen_subreg_concatn (outermode, SUBREG_REG (op),
					    GET_MODE (SUBREG_REG (op)), byte);

      op2 = simplify_subreg_concatn (GET_MODE (op), SUBREG_REG (op),
				     SUBREG_BYTE (op));
      if (op2 == NULL_RTX)
	{
	  /* We don't handle paradoxical subregs here.  */
	  gcc_assert (GET_MODE_SIZE (outermode)
		      <= GET_MODE_SIZE (GET_MODE (op)));
	  gcc_assert (GET_MODE_SIZE (GET_MODE (op))
		      <= GET_MODE_SIZE (GET_MODE (SUBREG_REG (op))));
	  op2 = simplify_subreg_concatn (outermode, SUBREG_REG (op),
					 byte + SUBREG_BYTE (op));
	  gcc_assert (op2 != NULL_RTX);
	  return op2;
	}

      op = op2;
      gcc_assert (op != NULL_RTX);
      gcc_assert (innermode == GET_MODE (op));
    }

  if (GET_CODE (op) == CONCATN)
    return simplify_subreg_concatn (outermode, op, byte);

  ret = simplify_gen_subreg (outermode, op, innermode, byte);

  /* If we see an insn like (set (reg:DI) (subreg:DI (reg:SI) 0)) then
     resolve_simple_move will ask for the high part of the paradoxical
     subreg, which does not have a value.  Just return a zero.  */
  if (ret == NULL_RTX
      && GET_CODE (op) == SUBREG
      && SUBREG_BYTE (op) == 0
      && (GET_MODE_SIZE (innermode)
	  > GET_MODE_SIZE (GET_MODE (SUBREG_REG (op)))))
    return CONST0_RTX (outermode);

  gcc_assert (ret != NULL_RTX);
  return ret;
}

/* Return whether we should resolve X into the registers into which it
   was decomposed.  */

static bool
resolve_reg_p (rtx x)
{
  return GET_CODE (x) == CONCATN;
}

/* Return whether X is a SUBREG of a register which we need to
   resolve.  */

static bool
resolve_subreg_p (rtx x)
{
  if (GET_CODE (x) != SUBREG)
    return false;
  return resolve_reg_p (SUBREG_REG (x));
}

/* This is called via for_each_rtx.  Look for SUBREGs which need to be
   decomposed.  */

static int
resolve_subreg_use (rtx *px, void *data)
{
  rtx insn = (rtx) data;
  rtx x = *px;

  if (x == NULL_RTX)
    return 0;

  if (resolve_subreg_p (x))
    {
      x = simplify_subreg_concatn (GET_MODE (x), SUBREG_REG (x),
				   SUBREG_BYTE (x));

      /* It is possible for a note to contain a reference which we can
	 decompose.  In this case, return 1 to the caller to indicate
	 that the note must be removed.  */
      if (!x)
	{
	  gcc_assert(!insn);
	  return 1;
	}

      validate_change (insn, px, x, 1);
      return -1;
    }

  if (resolve_reg_p (x))
    {
      /* Return 1 to the caller to indicate that we found a direct
	 reference to a register which is being decomposed.  This can
	 happen inside notes.  */
      gcc_assert (!insn);
      return 1;
    }

  return 0;
}

/* If there is a REG_LIBCALL note on OLD_START, move it to NEW_START,
   and link the corresponding REG_RETVAL note to NEW_START.  */

static void
move_libcall_note (rtx old_start, rtx new_start)
{
  rtx note0, note1, end;

  note0 = find_reg_note (old_start, REG_LIBCALL, NULL);
  if (note0 == NULL_RTX)
    return;

  remove_note (old_start, note0);
  end = XEXP (note0, 0);
  note1 = find_reg_note (end, REG_RETVAL, NULL);

  XEXP (note0, 1) = REG_NOTES (new_start);
  REG_NOTES (new_start) = note0;
  XEXP (note1, 0) = new_start;
}

/* Remove any REG_RETVAL note, the corresponding REG_LIBCALL note, and
   any markers for a no-conflict block.  We have decomposed the
   registers so the non-conflict is now obvious.  */

static void
remove_retval_note (rtx insn1)
{
  rtx note0, insn0, note1, insn;

  note1 = find_reg_note (insn1, REG_RETVAL, NULL);
  if (note1 == NULL_RTX)
    return;

  insn0 = XEXP (note1, 0);
  note0 = find_reg_note (insn0, REG_LIBCALL, NULL);

  remove_note (insn0, note0);
  remove_note (insn1, note1);

  for (insn = insn0; insn != insn1; insn = NEXT_INSN (insn))
    {
      while (1)
	{
	  rtx note;

	  note = find_reg_note (insn, REG_NO_CONFLICT, NULL);
	  if (note == NULL_RTX)
	    break;
	  remove_note (insn, note);
	}
    }
}

/* Resolve any decomposed registers which appear in register notes on
   INSN.  */

static void
resolve_reg_notes (rtx insn)
{
  rtx *pnote, note;

  note = find_reg_equal_equiv_note (insn);
  if (note)
    {
      if (for_each_rtx (&XEXP (note, 0), resolve_subreg_use, NULL))
	{
	  remove_note (insn, note);
	  remove_retval_note (insn);
	}
    }

  pnote = &REG_NOTES (insn);
  while (*pnote != NULL_RTX)
    {
      bool delete = false;

      note = *pnote;
      switch (REG_NOTE_KIND (note))
	{
	case REG_NO_CONFLICT:
	  if (resolve_reg_p (XEXP (note, 0)))
	    delete = true;
	  break;

	default:
	  break;
	}

      if (delete)
	*pnote = XEXP (note, 1);
      else
	pnote = &XEXP (note, 1);
    }
}

/* Return whether X can not be decomposed into subwords.  */

static bool
cannot_decompose_p (rtx x)
{
  if (REG_P (x))
    {
      unsigned int regno = REGNO (x);

      if (HARD_REGISTER_NUM_P (regno))
	return !validate_subreg (word_mode, GET_MODE (x), x, UNITS_PER_WORD);
      else
	return bitmap_bit_p (non_decomposable_context, regno);
    }

  return false;
}

/* Decompose the registers used in a simple move SET within INSN.  If
   we don't change anything, return INSN, otherwise return the start
   of the sequence of moves.  */

static rtx
resolve_simple_move (rtx set, rtx insn)
{
  rtx src, dest, real_dest, insns;
  enum machine_mode orig_mode, dest_mode;
  unsigned int words;
  bool pushing;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  orig_mode = GET_MODE (dest);

  words = (GET_MODE_SIZE (orig_mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  if (words <= 1)
    return insn;

  start_sequence ();

  /* We have to handle copying from a SUBREG of a decomposed reg where
     the SUBREG is larger than word size.  Rather than assume that we
     can take a word_mode SUBREG of the destination, we copy to a new
     register and then copy that to the destination.  */

  real_dest = NULL_RTX;

  if (GET_CODE (src) == SUBREG
      && resolve_reg_p (SUBREG_REG (src))
      && (SUBREG_BYTE (src) != 0
	  || (GET_MODE_SIZE (orig_mode)
	      != GET_MODE_SIZE (GET_MODE (SUBREG_REG (src))))))
    {
      real_dest = dest;
      dest = gen_reg_rtx (orig_mode);
      if (REG_P (real_dest))
	REG_ATTRS (dest) = REG_ATTRS (real_dest);
    }

  /* Similarly if we are copying to a SUBREG of a decomposed reg where
     the SUBREG is larger than word size.  */

  if (GET_CODE (dest) == SUBREG
      && resolve_reg_p (SUBREG_REG (dest))
      && (SUBREG_BYTE (dest) != 0
	  || (GET_MODE_SIZE (orig_mode)
	      != GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest))))))
    {
      rtx reg, minsn, smove;

      reg = gen_reg_rtx (orig_mode);
      minsn = emit_move_insn (reg, src);
      smove = single_set (minsn);
      gcc_assert (smove != NULL_RTX);
      resolve_simple_move (smove, minsn);
      src = reg;
    }

  /* If we didn't have any big SUBREGS of decomposed registers, and
     neither side of the move is a register we are decomposing, then
     we don't have to do anything here.  */

  if (src == SET_SRC (set)
      && dest == SET_DEST (set)
      && !resolve_reg_p (src)
      && !resolve_subreg_p (src)
      && !resolve_reg_p (dest)
      && !resolve_subreg_p (dest))
    {
      end_sequence ();
      return insn;
    }

  /* If SRC is a register which we can't decompose, or has side
     effects, we need to move via a temporary register.  */

  if (cannot_decompose_p (src)
      || side_effects_p (src)
      || GET_CODE (src) == ASM_OPERANDS)
    {
      rtx reg;

      reg = gen_reg_rtx (orig_mode);
      emit_move_insn (reg, src);
      src = reg;
    }

  /* If DEST is a register which we can't decompose, or has side
     effects, we need to first move to a temporary register.  We
     handle the common case of pushing an operand directly.  We also
     go through a temporary register if it holds a floating point
     value.  This gives us better code on systems which can't move
     data easily between integer and floating point registers.  */

  dest_mode = orig_mode;
  pushing = push_operand (dest, dest_mode);
  if (cannot_decompose_p (dest)
      || (side_effects_p (dest) && !pushing)
      || (!SCALAR_INT_MODE_P (dest_mode)
	  && !resolve_reg_p (dest)
	  && !resolve_subreg_p (dest)))
    {
      if (real_dest == NULL_RTX)
	real_dest = dest;
      if (!SCALAR_INT_MODE_P (dest_mode))
	{
	  dest_mode = mode_for_size (GET_MODE_SIZE (dest_mode) * BITS_PER_UNIT,
				     MODE_INT, 0);
	  gcc_assert (dest_mode != BLKmode);
	}
      dest = gen_reg_rtx (dest_mode);
      if (REG_P (real_dest))
	REG_ATTRS (dest) = REG_ATTRS (real_dest);
    }

  if (pushing)
    {
      unsigned int i, j, jinc;

      gcc_assert (GET_MODE_SIZE (orig_mode) % UNITS_PER_WORD == 0);
      gcc_assert (GET_CODE (XEXP (dest, 0)) != PRE_MODIFY);
      gcc_assert (GET_CODE (XEXP (dest, 0)) != POST_MODIFY);

      if (WORDS_BIG_ENDIAN == STACK_GROWS_DOWNWARD)
	{
	  j = 0;
	  jinc = 1;
	}
      else
	{
	  j = words - 1;
	  jinc = -1;
	}

      for (i = 0; i < words; ++i, j += jinc)
	{
	  rtx temp;

	  temp = copy_rtx (XEXP (dest, 0));
	  temp = adjust_automodify_address_nv (dest, word_mode, temp,
					       j * UNITS_PER_WORD);
	  emit_move_insn (temp,
			  simplify_gen_subreg_concatn (word_mode, src,
						       orig_mode,
						       j * UNITS_PER_WORD));
	}
    }
  else
    {
      unsigned int i;

      if (REG_P (dest) && !HARD_REGISTER_NUM_P (REGNO (dest)))
	emit_insn (gen_rtx_CLOBBER (VOIDmode, dest));

      for (i = 0; i < words; ++i)
	emit_move_insn (simplify_gen_subreg_concatn (word_mode, dest,
						     dest_mode,
						     i * UNITS_PER_WORD),
			simplify_gen_subreg_concatn (word_mode, src,
						     orig_mode,
						     i * UNITS_PER_WORD));
    }

  if (real_dest != NULL_RTX)
    {
      rtx mdest, minsn, smove;

      if (dest_mode == orig_mode)
	mdest = dest;
      else
	mdest = simplify_gen_subreg (orig_mode, dest, GET_MODE (dest), 0);
      minsn = emit_move_insn (real_dest, mdest);

      smove = single_set (minsn);
      gcc_assert (smove != NULL_RTX);

      resolve_simple_move (smove, minsn);
    }

  insns = get_insns ();
  end_sequence ();

  emit_insn_before (insns, insn);

  move_libcall_note (insn, insns);
  remove_retval_note (insn);
  delete_insn (insn);

  return insns;
}

/* Change a CLOBBER of a decomposed register into a CLOBBER of the
   component registers.  Return whether we changed something.  */

static bool
resolve_clobber (rtx pat, rtx insn)
{
  rtx reg;
  enum machine_mode orig_mode;
  unsigned int words, i;

  reg = XEXP (pat, 0);
  if (!resolve_reg_p (reg) && !resolve_subreg_p (reg))
    return false;

  orig_mode = GET_MODE (reg);
  words = GET_MODE_SIZE (orig_mode);
  words = (words + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  XEXP (pat, 0) = simplify_gen_subreg_concatn (word_mode, reg, orig_mode, 0);
  for (i = words - 1; i > 0; --i)
    {
      rtx x;

      x = simplify_gen_subreg_concatn (word_mode, reg, orig_mode,
				       i * UNITS_PER_WORD);
      x = gen_rtx_CLOBBER (VOIDmode, x);
      emit_insn_after (x, insn);
    }

  return true;
}

/* A USE of a decomposed register is no longer meaningful.  Return
   whether we changed something.  */

static bool
resolve_use (rtx pat, rtx insn)
{
  if (resolve_reg_p (XEXP (pat, 0)) || resolve_subreg_p (XEXP (pat, 0)))
    {
      delete_insn (insn);
      return true;
    }
  return false;
}

/* Look for registers which are always accessed via word-sized SUBREGs
   or via copies.  Decompose these registers into several word-sized
   pseudo-registers.  */

static void
decompose_multiword_subregs (bool update_life)
{
  unsigned int max;
  basic_block bb;

  max = max_reg_num ();

  /* First see if there are any multi-word pseudo-registers.  If there
     aren't, there is nothing we can do.  This should speed up this
     pass in the normal case, since it should be faster than scanning
     all the insns.  */
  {
    unsigned int i;

    for (i = FIRST_PSEUDO_REGISTER; i < max; ++i)
      {
	if (regno_reg_rtx[i] != NULL
	    && GET_MODE_SIZE (GET_MODE (regno_reg_rtx[i])) > UNITS_PER_WORD)
	  break;
      }
    if (i == max)
      return;
  }

  /* FIXME: When the dataflow branch is merged, we can change this
     code to look for each multi-word pseudo-register and to find each
     insn which sets or uses that register.  That should be faster
     than scanning all the insns.  */

  decomposable_context = BITMAP_ALLOC (NULL);
  non_decomposable_context = BITMAP_ALLOC (NULL);

  reg_copy_graph = VEC_alloc (bitmap, heap, max);
  VEC_safe_grow (bitmap, heap, reg_copy_graph, max);
  memset (VEC_address (bitmap, reg_copy_graph), 0, sizeof (bitmap) * max);

  FOR_EACH_BB (bb)
    {
      rtx insn;

      FOR_BB_INSNS (bb, insn)
	{
	  rtx set;
	  enum classify_move_insn cmi;
	  int i, n;

	  if (!INSN_P (insn)
	      || GET_CODE (PATTERN (insn)) == CLOBBER
	      || GET_CODE (PATTERN (insn)) == USE)
	    continue;

	  recog_memoized (insn);
	  extract_insn (insn);

	  set = simple_move (insn);

	  if (!set)
	    cmi = NOT_SIMPLE_MOVE;
	  else
	    {
	      bool retval;

	      retval = find_reg_note (insn, REG_RETVAL, NULL_RTX) != NULL_RTX;

	      if (find_pseudo_copy (set) && !retval)
		cmi = SIMPLE_PSEUDO_REG_MOVE;
	      else if (retval
		       && REG_P (SET_SRC (set))
		       && HARD_REGISTER_P (SET_SRC (set)))
		{
		  rtx note;

		  /* We don't want to decompose an assignment which
		     copies the value returned by a libcall to a
		     pseudo-register.  Doing that will lose the RETVAL
		     note with no real gain.  */
		  cmi = NOT_SIMPLE_MOVE;

		  /* If we have a RETVAL note, there should be an
		     EQUAL note.  We don't want to decompose any
		     registers which that EQUAL note refers to
		     directly.  If we do, we will no longer know the
		     value of the libcall.  */
		  note = find_reg_equal_equiv_note (insn);
		  if (note != NULL_RTX)
		    for_each_rtx (&XEXP (note, 0), find_decomposable_subregs,
				  &cmi);
		}
	      else
		cmi = SIMPLE_MOVE;
	    }

	  n = recog_data.n_operands;
	  for (i = 0; i < n; ++i)
	    {
	      for_each_rtx (&recog_data.operand[i],
			    find_decomposable_subregs,
			    &cmi);

	      /* We handle ASM_OPERANDS as a special case to support
		 things like x86 rdtsc which returns a DImode value.
		 We can decompose the output, which will certainly be
		 operand 0, but not the inputs.  */

	      if (cmi == SIMPLE_MOVE
		  && GET_CODE (SET_SRC (set)) == ASM_OPERANDS)
		{
		  gcc_assert (i == 0);
		  cmi = NOT_SIMPLE_MOVE;
		}
	    }
	}
    }

  bitmap_and_compl_into (decomposable_context, non_decomposable_context);
  if (!bitmap_empty_p (decomposable_context))
    {
      int hold_no_new_pseudos = no_new_pseudos;
      int max_regno = max_reg_num ();
      sbitmap blocks;
      bitmap_iterator iter;
      unsigned int regno;

      propagate_pseudo_copies ();

      no_new_pseudos = 0;
      blocks = sbitmap_alloc (last_basic_block);
      sbitmap_zero (blocks);

      EXECUTE_IF_SET_IN_BITMAP (decomposable_context, 0, regno, iter)
	decompose_register (regno);

      FOR_EACH_BB (bb)
	{
	  rtx insn;

	  FOR_BB_INSNS (bb, insn)
	    {
	      rtx next, pat;
	      bool changed;

	      if (!INSN_P (insn))
		continue;

	      next = NEXT_INSN (insn);
	      changed = false;

	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == CLOBBER)
		{
		  if (resolve_clobber (pat, insn))
		    changed = true;
		}
	      else if (GET_CODE (pat) == USE)
		{
		  if (resolve_use (pat, insn))
		    changed = true;
		}
	      else
		{
		  rtx set;
		  int i;

		  recog_memoized (insn);
		  extract_insn (insn);

		  set = simple_move (insn);
		  if (set)
		    {
		      rtx orig_insn = insn;

		      insn = resolve_simple_move (set, insn);
		      if (insn != orig_insn)
			{
			  changed = true;

			  recog_memoized (insn);
			  extract_insn (insn);
			}
		    }

		  for (i = recog_data.n_operands - 1; i >= 0; --i)
		    for_each_rtx (recog_data.operand_loc[i],
				  resolve_subreg_use,
				  insn);

		  resolve_reg_notes (insn);

		  if (num_validated_changes () > 0)
		    {
		      for (i = recog_data.n_dups - 1; i >= 0; --i)
			{
			  rtx *pl = recog_data.dup_loc[i];
			  int dup_num = recog_data.dup_num[i];
			  rtx *px = recog_data.operand_loc[dup_num];

			  validate_change (insn, pl, *px, 1);
			}

		      i = apply_change_group ();
		      gcc_assert (i);

		      changed = true;
		    }
		}

	      if (changed)
		{
		  SET_BIT (blocks, bb->index);
		  reg_scan_update (insn, next, max_regno);
		}
	    }
	}

      no_new_pseudos = hold_no_new_pseudos;

      if (update_life)
	update_life_info (blocks, UPDATE_LIFE_GLOBAL_RM_NOTES,
			  PROP_DEATH_NOTES);

      sbitmap_free (blocks);
    }

  {
    unsigned int i;
    bitmap b;

    for (i = 0; VEC_iterate (bitmap, reg_copy_graph, i, b); ++i)
      if (b)
	BITMAP_FREE (b);
  }

  VEC_free (bitmap, heap, reg_copy_graph);  

  BITMAP_FREE (decomposable_context);
  BITMAP_FREE (non_decomposable_context);
}

/* Gate function for lower subreg pass.  */

static bool
gate_handle_lower_subreg (void)
{
  return flag_split_wide_types != 0;
}

/* Implement first lower subreg pass.  */

static unsigned int
rest_of_handle_lower_subreg (void)
{
  decompose_multiword_subregs (false);
  return 0;
}

/* Implement second lower subreg pass.  */

static unsigned int
rest_of_handle_lower_subreg2 (void)
{
  decompose_multiword_subregs (true);
  return 0;
}

struct tree_opt_pass pass_lower_subreg =
{
  "subreg",	                        /* name */
  gate_handle_lower_subreg,             /* gate */
  rest_of_handle_lower_subreg,          /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOWER_SUBREG,                      /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'u'                                   /* letter */
};

struct tree_opt_pass pass_lower_subreg2 =
{
  "subreg2",	                        /* name */
  gate_handle_lower_subreg,             /* gate */
  rest_of_handle_lower_subreg2,          /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOWER_SUBREG,                      /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'U'                                   /* letter */
};
