/* Decompose multiword subregs.
   Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>
		  Ian Lance Taylor <iant@google.com>

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
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
#include "dce.h"
#include "expr.h"
#include "except.h"
#include "regs.h"
#include "tree-pass.h"
#include "df.h"

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

/* Bit N in this bitmap is set if regno N is used in a subreg
   which changes the mode but not the size.  This typically happens
   when the register accessed as a floating-point value; we want to
   avoid generating accesses to its subwords in integer modes.  */
static bitmap subreg_context;

/* Bit N in the bitmap in element M of this array is set if there is a
   copy from reg M to reg N.  */
static VEC(bitmap,heap) *reg_copy_graph;

/* Return whether X is a simple object which we can take a word_mode
   subreg of.  */

static bool
simple_move_operand (rtx x)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  if (!OBJECT_P (x))
    return false;

  if (GET_CODE (x) == LABEL_REF
      || GET_CODE (x) == SYMBOL_REF
      || GET_CODE (x) == HIGH
      || GET_CODE (x) == CONST)
    return false;

  if (MEM_P (x)
      && (MEM_VOLATILE_P (x)
	  || mode_dependent_address_p (XEXP (x, 0))))
    return false;

  return true;
}

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
  if (!simple_move_operand (x))
    return NULL_RTX;

  x = SET_SRC (set);
  if (x != recog_data.operand[0] && x != recog_data.operand[1])
    return NULL_RTX;
  /* For the src we can handle ASM_OPERANDS, and it is beneficial for
     things like x86 rdtsc which returns a DImode value.  */
  if (GET_CODE (x) != ASM_OPERANDS
      && !simple_move_operand (x))
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

  /* Reject PARTIAL_INT modes.  They are used for processor specific
     purposes and it's probably best not to tamper with them.  */
  if (GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
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
  /* A simple move from one pseudo-register to another.  */
  SIMPLE_PSEUDO_REG_MOVE,
  /* A simple move involving a non-pseudo-register.  */
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

      /* If this is a cast from one mode to another, where the modes
	 have the same size, and they are not tieable, then mark this
	 register as non-decomposable.  If we decompose it we are
	 likely to mess up whatever the backend is trying to do.  */
      if (outer_words > 1
	  && outer_size == inner_size
	  && !MODES_TIEABLE_P (GET_MODE (x), GET_MODE (inner)))
	{
	  bitmap_set_bit (non_decomposable_context, regno);
	  bitmap_set_bit (subreg_context, regno);
	  return -1;
	}
    }
  else if (REG_P (x))
    {
      unsigned int regno;

      /* We will see an outer SUBREG before we see the inner REG, so
	 when we see a plain REG here it means a direct reference to
	 the register.

	 If this is not a simple copy from one location to another,
	 then we can not decompose this register.  If this is a simple
	 copy from one pseudo-register to another, and the mode is right
	 then we mark the register as decomposable.
	 Otherwise we don't say anything about this register --
	 it could be decomposed, but whether that would be
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
  else if (MEM_P (x))
    {
      enum classify_move_insn cmi_mem = NOT_SIMPLE_MOVE;

      /* Any registers used in a MEM do not participate in a
	 SIMPLE_MOVE or SIMPLE_PSEUDO_REG_MOVE.  Do our own recursion
	 here, and return -1 to block the parent's recursion.  */
      for_each_rtx (&XEXP (x, 0), find_decomposable_subregs, &cmi_mem);
      return -1;
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
  enum machine_mode innermode, partmode;
  rtx part;
  unsigned int final_offset;

  gcc_assert (GET_CODE (op) == CONCATN);
  gcc_assert (byte % GET_MODE_SIZE (outermode) == 0);

  innermode = GET_MODE (op);
  gcc_assert (byte < GET_MODE_SIZE (innermode));
  gcc_assert (GET_MODE_SIZE (outermode) <= GET_MODE_SIZE (innermode));

  inner_size = GET_MODE_SIZE (innermode) / XVECLEN (op, 0);
  part = XVECEXP (op, 0, byte / inner_size);
  partmode = GET_MODE (part);

  /* VECTOR_CSTs in debug expressions are expanded into CONCATN instead of
     regular CONST_VECTORs.  They have vector or integer modes, depending
     on the capabilities of the target.  Cope with them.  */
  if (partmode == VOIDmode && VECTOR_MODE_P (innermode))
    partmode = GET_MODE_INNER (innermode);
  else if (partmode == VOIDmode)
    {
      enum mode_class mclass = GET_MODE_CLASS (innermode);
      partmode = mode_for_size (inner_size * BITS_PER_UNIT, mclass, 0);
    }

  final_offset = byte % inner_size;
  if (final_offset + GET_MODE_SIZE (outermode) > inner_size)
    return NULL_RTX;

  return simplify_gen_subreg (outermode, part, partmode, final_offset);
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
	  gcc_assert (!insn);
	  return 1;
	}

      validate_change (insn, px, x, 1);
      return -1;
    }

  if (resolve_reg_p (x))
    {
      /* Return 1 to the caller to indicate that we found a direct
	 reference to a register which is being decomposed.  This can
	 happen inside notes, multiword shift or zero-extend
	 instructions.  */
      return 1;
    }

  return 0;
}

/* This is called via for_each_rtx.  Look for SUBREGs which can be
   decomposed and decomposed REGs that need copying.  */

static int
adjust_decomposed_uses (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  if (x == NULL_RTX)
    return 0;

  if (resolve_subreg_p (x))
    {
      x = simplify_subreg_concatn (GET_MODE (x), SUBREG_REG (x),
				   SUBREG_BYTE (x));

      if (x)
	*px = x;
      else
	x = copy_rtx (*px);
    }

  if (resolve_reg_p (x))
    *px = copy_rtx (x);

  return 0;
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
      int old_count = num_validated_changes ();
      if (for_each_rtx (&XEXP (note, 0), resolve_subreg_use, NULL))
	remove_note (insn, note);
      else
	if (old_count != num_validated_changes ())
	  df_notes_rescan (insn);
    }

  pnote = &REG_NOTES (insn);
  while (*pnote != NULL_RTX)
    {
      bool del = false;

      note = *pnote;
      switch (REG_NOTE_KIND (note))
	{
	case REG_DEAD:
	case REG_UNUSED:
	  if (resolve_reg_p (XEXP (note, 0)))
	    del = true;
	  break;

	default:
	  break;
	}

      if (del)
	*pnote = XEXP (note, 1);
      else
	pnote = &XEXP (note, 1);
    }
}

/* Return whether X can be decomposed into subwords.  */

static bool
can_decompose_p (rtx x)
{
  if (REG_P (x))
    {
      unsigned int regno = REGNO (x);

      if (HARD_REGISTER_NUM_P (regno))
	return (validate_subreg (word_mode, GET_MODE (x), x, UNITS_PER_WORD)
		&& HARD_REGNO_MODE_OK (regno, word_mode));
      else
	return !bitmap_bit_p (subreg_context, regno);
    }

  return true;
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

  /* It's possible for the code to use a subreg of a decomposed
     register while forming an address.  We need to handle that before
     passing the address to emit_move_insn.  We pass NULL_RTX as the
     insn parameter to resolve_subreg_use because we can not validate
     the insn yet.  */
  if (MEM_P (src) || MEM_P (dest))
    {
      int acg;

      if (MEM_P (src))
	for_each_rtx (&XEXP (src, 0), resolve_subreg_use, NULL_RTX);
      if (MEM_P (dest))
	for_each_rtx (&XEXP (dest, 0), resolve_subreg_use, NULL_RTX);
      acg = apply_change_group ();
      gcc_assert (acg);
    }

  /* If SRC is a register which we can't decompose, or has side
     effects, we need to move via a temporary register.  */

  if (!can_decompose_p (src)
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
  if (!can_decompose_p (dest)
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
	emit_clobber (dest);

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

  copy_reg_eh_region_note_forward (insn, insns, NULL_RTX);

  emit_insn_before (insns, insn);

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
  int ret;

  reg = XEXP (pat, 0);
  if (!resolve_reg_p (reg) && !resolve_subreg_p (reg))
    return false;

  orig_mode = GET_MODE (reg);
  words = GET_MODE_SIZE (orig_mode);
  words = (words + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  ret = validate_change (NULL_RTX, &XEXP (pat, 0),
			 simplify_gen_subreg_concatn (word_mode, reg,
						      orig_mode, 0),
			 0);
  df_insn_rescan (insn);
  gcc_assert (ret != 0);

  for (i = words - 1; i > 0; --i)
    {
      rtx x;

      x = simplify_gen_subreg_concatn (word_mode, reg, orig_mode,
				       i * UNITS_PER_WORD);
      x = gen_rtx_CLOBBER (VOIDmode, x);
      emit_insn_after (x, insn);
    }

  resolve_reg_notes (insn);

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

  resolve_reg_notes (insn);

  return false;
}

/* A VAR_LOCATION can be simplified.  */

static void
resolve_debug (rtx insn)
{
  for_each_rtx (&PATTERN (insn), adjust_decomposed_uses, NULL_RTX);

  df_insn_rescan (insn);

  resolve_reg_notes (insn);
}

/* Checks if INSN is a decomposable multiword-shift or zero-extend and
   sets the decomposable_context bitmap accordingly.  A non-zero value
   is returned if a decomposable insn has been found.  */

static int
find_decomposable_shift_zext (rtx insn)
{
  rtx set;
  rtx op;
  rtx op_operand;

  set = single_set (insn);
  if (!set)
    return 0;

  op = SET_SRC (set);
  if (GET_CODE (op) != ASHIFT
      && GET_CODE (op) != LSHIFTRT
      && GET_CODE (op) != ZERO_EXTEND)
    return 0;

  op_operand = XEXP (op, 0);
  if (!REG_P (SET_DEST (set)) || !REG_P (op_operand)
      || HARD_REGISTER_NUM_P (REGNO (SET_DEST (set)))
      || HARD_REGISTER_NUM_P (REGNO (op_operand))
      || !SCALAR_INT_MODE_P (GET_MODE (op)))
    return 0;

  if (GET_CODE (op) == ZERO_EXTEND)
    {
      if (GET_MODE (op_operand) != word_mode
	  || GET_MODE_BITSIZE (GET_MODE (op)) != 2 * BITS_PER_WORD)
	return 0;
    }
  else /* left or right shift */
    {
      if (!CONST_INT_P (XEXP (op, 1))
	  || INTVAL (XEXP (op, 1)) < BITS_PER_WORD
	  || GET_MODE_BITSIZE (GET_MODE (op_operand)) != 2 * BITS_PER_WORD)
	return 0;
    }

  bitmap_set_bit (decomposable_context, REGNO (SET_DEST (set)));

  if (GET_CODE (op) != ZERO_EXTEND)
    bitmap_set_bit (decomposable_context, REGNO (op_operand));

  return 1;
}

/* Decompose a more than word wide shift (in INSN) of a multiword
   pseudo or a multiword zero-extend of a wordmode pseudo into a move
   and 'set to zero' insn.  Return a pointer to the new insn when a
   replacement was done.  */

static rtx
resolve_shift_zext (rtx insn)
{
  rtx set;
  rtx op;
  rtx op_operand;
  rtx insns;
  rtx src_reg, dest_reg, dest_zero;
  int src_reg_num, dest_reg_num, offset1, offset2, src_offset;

  set = single_set (insn);
  if (!set)
    return NULL_RTX;

  op = SET_SRC (set);
  if (GET_CODE (op) != ASHIFT
      && GET_CODE (op) != LSHIFTRT
      && GET_CODE (op) != ZERO_EXTEND)
    return NULL_RTX;

  op_operand = XEXP (op, 0);

  if (!resolve_reg_p (SET_DEST (set)) && !resolve_reg_p (op_operand))
    return NULL_RTX;

  /* src_reg_num is the number of the word mode register which we
     are operating on.  For a left shift and a zero_extend on little
     endian machines this is register 0.  */
  src_reg_num = GET_CODE (op) == LSHIFTRT ? 1 : 0;

  if (WORDS_BIG_ENDIAN
      && GET_MODE_SIZE (GET_MODE (op_operand)) > UNITS_PER_WORD)
    src_reg_num = 1 - src_reg_num;

  if (GET_CODE (op) == ZERO_EXTEND)
    dest_reg_num = WORDS_BIG_ENDIAN ? 1 : 0;
  else
    dest_reg_num = 1 - src_reg_num;

  offset1 = UNITS_PER_WORD * dest_reg_num;
  offset2 = UNITS_PER_WORD * (1 - dest_reg_num);
  src_offset = UNITS_PER_WORD * src_reg_num;

  start_sequence ();

  dest_reg = simplify_gen_subreg_concatn (word_mode, SET_DEST (set),
                                          GET_MODE (SET_DEST (set)),
                                          offset1);
  dest_zero = simplify_gen_subreg_concatn (word_mode, SET_DEST (set),
                                           GET_MODE (SET_DEST (set)),
                                           offset2);
  src_reg = simplify_gen_subreg_concatn (word_mode, op_operand,
                                         GET_MODE (op_operand),
                                         src_offset);
  if (GET_CODE (op) != ZERO_EXTEND)
    {
      int shift_count = INTVAL (XEXP (op, 1));
      if (shift_count > BITS_PER_WORD)
	src_reg = expand_shift (GET_CODE (op) == ASHIFT ?
				LSHIFT_EXPR : RSHIFT_EXPR,
				word_mode, src_reg,
				shift_count - BITS_PER_WORD,
				dest_reg, 1);
    }

  if (dest_reg != src_reg)
    emit_move_insn (dest_reg, src_reg);
  emit_move_insn (dest_zero, CONST0_RTX (word_mode));
  insns = get_insns ();

  end_sequence ();

  emit_insn_before (insns, insn);

  if (dump_file)
    {
      rtx in;
      fprintf (dump_file, "; Replacing insn: %d with insns: ", INSN_UID (insn));
      for (in = insns; in != insn; in = NEXT_INSN (in))
	fprintf (dump_file, "%d ", INSN_UID (in));
      fprintf (dump_file, "\n");
    }

  delete_insn (insn);
  return insns;
}

/* Look for registers which are always accessed via word-sized SUBREGs
   or via copies.  Decompose these registers into several word-sized
   pseudo-registers.  */

static void
decompose_multiword_subregs (void)
{
  unsigned int max;
  basic_block bb;

  if (df)
    df_set_flags (DF_DEFER_INSN_RESCAN);

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

  if (df)
    run_word_dce ();

  /* FIXME: When the dataflow branch is merged, we can change this
     code to look for each multi-word pseudo-register and to find each
     insn which sets or uses that register.  That should be faster
     than scanning all the insns.  */

  decomposable_context = BITMAP_ALLOC (NULL);
  non_decomposable_context = BITMAP_ALLOC (NULL);
  subreg_context = BITMAP_ALLOC (NULL);

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

	  if (find_decomposable_shift_zext (insn))
	    continue;

	  recog_memoized (insn);
	  extract_insn (insn);

	  set = simple_move (insn);

	  if (!set)
	    cmi = NOT_SIMPLE_MOVE;
	  else
	    {
	      if (find_pseudo_copy (set))
		cmi = SIMPLE_PSEUDO_REG_MOVE;
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
      sbitmap sub_blocks;
      unsigned int i;
      sbitmap_iterator sbi;
      bitmap_iterator iter;
      unsigned int regno;

      propagate_pseudo_copies ();

      sub_blocks = sbitmap_alloc (last_basic_block);
      sbitmap_zero (sub_blocks);

      EXECUTE_IF_SET_IN_BITMAP (decomposable_context, 0, regno, iter)
	decompose_register (regno);

      FOR_EACH_BB (bb)
	{
	  rtx insn;

	  FOR_BB_INSNS (bb, insn)
	    {
	      rtx pat;

	      if (!INSN_P (insn))
		continue;

	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == CLOBBER)
		resolve_clobber (pat, insn);
	      else if (GET_CODE (pat) == USE)
		resolve_use (pat, insn);
	      else if (DEBUG_INSN_P (insn))
		resolve_debug (insn);
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
		      bool cfi = control_flow_insn_p (insn);

		      /* We can end up splitting loads to multi-word pseudos
			 into separate loads to machine word size pseudos.
			 When this happens, we first had one load that can
			 throw, and after resolve_simple_move we'll have a
			 bunch of loads (at least two).  All those loads may
			 trap if we can have non-call exceptions, so they
			 all will end the current basic block.  We split the
			 block after the outer loop over all insns, but we
			 make sure here that we will be able to split the
			 basic block and still produce the correct control
			 flow graph for it.  */
		      gcc_assert (!cfi
				  || (cfun->can_throw_non_call_exceptions
				      && can_throw_internal (insn)));

		      insn = resolve_simple_move (set, insn);
		      if (insn != orig_insn)
			{
			  recog_memoized (insn);
			  extract_insn (insn);

			  if (cfi)
			    SET_BIT (sub_blocks, bb->index);
			}
		    }
		  else
		    {
		      rtx decomposed_shift;

		      decomposed_shift = resolve_shift_zext (insn);
		      if (decomposed_shift != NULL_RTX)
			{
			  insn = decomposed_shift;
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

			  validate_unshare_change (insn, pl, *px, 1);
			}

		      i = apply_change_group ();
		      gcc_assert (i);
		    }
		}
	    }
	}

      /* If we had insns to split that caused control flow insns in the middle
	 of a basic block, split those blocks now.  Note that we only handle
	 the case where splitting a load has caused multiple possibly trapping
	 loads to appear.  */
      EXECUTE_IF_SET_IN_SBITMAP (sub_blocks, 0, i, sbi)
	{
	  rtx insn, end;
	  edge fallthru;

	  bb = BASIC_BLOCK (i);
	  insn = BB_HEAD (bb);
	  end = BB_END (bb);

	  while (insn != end)
	    {
	      if (control_flow_insn_p (insn))
		{
		  /* Split the block after insn.  There will be a fallthru
		     edge, which is OK so we keep it.  We have to create the
		     exception edges ourselves.  */
		  fallthru = split_block (bb, insn);
		  rtl_make_eh_edge (NULL, bb, BB_END (bb));
		  bb = fallthru->dest;
		  insn = BB_HEAD (bb);
		}
	      else
	        insn = NEXT_INSN (insn);
	    }
	}

      sbitmap_free (sub_blocks);
    }

  {
    unsigned int i;
    bitmap b;

    FOR_EACH_VEC_ELT (bitmap, reg_copy_graph, i, b)
      if (b)
	BITMAP_FREE (b);
  }

  VEC_free (bitmap, heap, reg_copy_graph);

  BITMAP_FREE (decomposable_context);
  BITMAP_FREE (non_decomposable_context);
  BITMAP_FREE (subreg_context);
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
  decompose_multiword_subregs ();
  return 0;
}

/* Implement second lower subreg pass.  */

static unsigned int
rest_of_handle_lower_subreg2 (void)
{
  decompose_multiword_subregs ();
  return 0;
}

struct rtl_opt_pass pass_lower_subreg =
{
 {
  RTL_PASS,
  "subreg1",	                        /* name */
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
  TODO_ggc_collect |
  TODO_verify_flow                      /* todo_flags_finish */
 }
};

struct rtl_opt_pass pass_lower_subreg2 =
{
 {
  RTL_PASS,
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
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect |
  TODO_verify_flow                      /* todo_flags_finish */
 }
};
