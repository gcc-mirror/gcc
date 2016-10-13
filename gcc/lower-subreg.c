/* Decompose multiword subregs.
   Copyright (C) 2007-2016 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "cfgbuild.h"
#include "dce.h"
#include "expr.h"
#include "tree-pass.h"
#include "lower-subreg.h"
#include "rtl-iter.h"


/* Decompose multi-word pseudo-registers into individual
   pseudo-registers when possible and profitable.  This is possible
   when all the uses of a multi-word register are via SUBREG, or are
   copies of the register to another location.  Breaking apart the
   register permits more CSE and permits better register allocation.
   This is profitable if the machine does not have move instructions
   to do this.

   This pass only splits moves with modes that are wider than
   word_mode and ASHIFTs, LSHIFTRTs, ASHIFTRTs and ZERO_EXTENDs with
   integer modes that are twice the width of word_mode.  The latter
   could be generalized if there was a need to do this, but the trend in
   architectures is to not need this.

   There are two useful preprocessor defines for use by maintainers:

   #define LOG_COSTS 1

   if you wish to see the actual cost estimates that are being used
   for each mode wider than word mode and the cost estimates for zero
   extension and the shifts.   This can be useful when port maintainers
   are tuning insn rtx costs.

   #define FORCE_LOWERING 1

   if you wish to test the pass with all the transformation forced on.
   This can be useful for finding bugs in the transformations.  */

#define LOG_COSTS 0
#define FORCE_LOWERING 0

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
static vec<bitmap> reg_copy_graph;

struct target_lower_subreg default_target_lower_subreg;
#if SWITCHABLE_TARGET
struct target_lower_subreg *this_target_lower_subreg
  = &default_target_lower_subreg;
#endif

#define twice_word_mode \
  this_target_lower_subreg->x_twice_word_mode
#define choices \
  this_target_lower_subreg->x_choices

/* RTXes used while computing costs.  */
struct cost_rtxes {
  /* Source and target registers.  */
  rtx source;
  rtx target;

  /* A twice_word_mode ZERO_EXTEND of SOURCE.  */
  rtx zext;

  /* A shift of SOURCE.  */
  rtx shift;

  /* A SET of TARGET.  */
  rtx set;
};

/* Return the cost of a CODE shift in mode MODE by OP1 bits, using the
   rtxes in RTXES.  SPEED_P selects between the speed and size cost.  */

static int
shift_cost (bool speed_p, struct cost_rtxes *rtxes, enum rtx_code code,
	    machine_mode mode, int op1)
{
  PUT_CODE (rtxes->shift, code);
  PUT_MODE (rtxes->shift, mode);
  PUT_MODE (rtxes->source, mode);
  XEXP (rtxes->shift, 1) = GEN_INT (op1);
  return set_src_cost (rtxes->shift, mode, speed_p);
}

/* For each X in the range [0, BITS_PER_WORD), set SPLITTING[X]
   to true if it is profitable to split a double-word CODE shift
   of X + BITS_PER_WORD bits.  SPEED_P says whether we are testing
   for speed or size profitability.

   Use the rtxes in RTXES to calculate costs.  WORD_MOVE_ZERO_COST is
   the cost of moving zero into a word-mode register.  WORD_MOVE_COST
   is the cost of moving between word registers.  */

static void
compute_splitting_shift (bool speed_p, struct cost_rtxes *rtxes,
			 bool *splitting, enum rtx_code code,
			 int word_move_zero_cost, int word_move_cost)
{
  int wide_cost, narrow_cost, upper_cost, i;

  for (i = 0; i < BITS_PER_WORD; i++)
    {
      wide_cost = shift_cost (speed_p, rtxes, code, twice_word_mode,
			      i + BITS_PER_WORD);
      if (i == 0)
	narrow_cost = word_move_cost;
      else
	narrow_cost = shift_cost (speed_p, rtxes, code, word_mode, i);

      if (code != ASHIFTRT)
	upper_cost = word_move_zero_cost;
      else if (i == BITS_PER_WORD - 1)
	upper_cost = word_move_cost;
      else
	upper_cost = shift_cost (speed_p, rtxes, code, word_mode,
				 BITS_PER_WORD - 1);

      if (LOG_COSTS)
	fprintf (stderr, "%s %s by %d: original cost %d, split cost %d + %d\n",
		 GET_MODE_NAME (twice_word_mode), GET_RTX_NAME (code),
		 i + BITS_PER_WORD, wide_cost, narrow_cost, upper_cost);

      if (FORCE_LOWERING || wide_cost >= narrow_cost + upper_cost)
	splitting[i] = true;
    }
}

/* Compute what we should do when optimizing for speed or size; SPEED_P
   selects which.  Use RTXES for computing costs.  */

static void
compute_costs (bool speed_p, struct cost_rtxes *rtxes)
{
  unsigned int i;
  int word_move_zero_cost, word_move_cost;

  PUT_MODE (rtxes->target, word_mode);
  SET_SRC (rtxes->set) = CONST0_RTX (word_mode);
  word_move_zero_cost = set_rtx_cost (rtxes->set, speed_p);

  SET_SRC (rtxes->set) = rtxes->source;
  word_move_cost = set_rtx_cost (rtxes->set, speed_p);

  if (LOG_COSTS)
    fprintf (stderr, "%s move: from zero cost %d, from reg cost %d\n",
	     GET_MODE_NAME (word_mode), word_move_zero_cost, word_move_cost);

  for (i = 0; i < MAX_MACHINE_MODE; i++)
    {
      machine_mode mode = (machine_mode) i;
      int factor = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      if (factor > 1)
	{
	  int mode_move_cost;

	  PUT_MODE (rtxes->target, mode);
	  PUT_MODE (rtxes->source, mode);
	  mode_move_cost = set_rtx_cost (rtxes->set, speed_p);

	  if (LOG_COSTS)
	    fprintf (stderr, "%s move: original cost %d, split cost %d * %d\n",
		     GET_MODE_NAME (mode), mode_move_cost,
		     word_move_cost, factor);

	  if (FORCE_LOWERING || mode_move_cost >= word_move_cost * factor)
	    {
	      choices[speed_p].move_modes_to_split[i] = true;
	      choices[speed_p].something_to_do = true;
	    }
	}
    }

  /* For the moves and shifts, the only case that is checked is one
     where the mode of the target is an integer mode twice the width
     of the word_mode.

     If it is not profitable to split a double word move then do not
     even consider the shifts or the zero extension.  */
  if (choices[speed_p].move_modes_to_split[(int) twice_word_mode])
    {
      int zext_cost;

      /* The only case here to check to see if moving the upper part with a
	 zero is cheaper than doing the zext itself.  */
      PUT_MODE (rtxes->source, word_mode);
      zext_cost = set_src_cost (rtxes->zext, twice_word_mode, speed_p);

      if (LOG_COSTS)
	fprintf (stderr, "%s %s: original cost %d, split cost %d + %d\n",
		 GET_MODE_NAME (twice_word_mode), GET_RTX_NAME (ZERO_EXTEND),
		 zext_cost, word_move_cost, word_move_zero_cost);

      if (FORCE_LOWERING || zext_cost >= word_move_cost + word_move_zero_cost)
	choices[speed_p].splitting_zext = true;

      compute_splitting_shift (speed_p, rtxes,
			       choices[speed_p].splitting_ashift, ASHIFT,
			       word_move_zero_cost, word_move_cost);
      compute_splitting_shift (speed_p, rtxes,
			       choices[speed_p].splitting_lshiftrt, LSHIFTRT,
			       word_move_zero_cost, word_move_cost);
      compute_splitting_shift (speed_p, rtxes,
			       choices[speed_p].splitting_ashiftrt, ASHIFTRT,
			       word_move_zero_cost, word_move_cost);
    }
}

/* Do one-per-target initialisation.  This involves determining
   which operations on the machine are profitable.  If none are found,
   then the pass just returns when called.  */

void
init_lower_subreg (void)
{
  struct cost_rtxes rtxes;

  memset (this_target_lower_subreg, 0, sizeof (*this_target_lower_subreg));

  twice_word_mode = GET_MODE_2XWIDER_MODE (word_mode);

  rtxes.target = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);
  rtxes.source = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 2);
  rtxes.set = gen_rtx_SET (rtxes.target, rtxes.source);
  rtxes.zext = gen_rtx_ZERO_EXTEND (twice_word_mode, rtxes.source);
  rtxes.shift = gen_rtx_ASHIFT (twice_word_mode, rtxes.source, const0_rtx);

  if (LOG_COSTS)
    fprintf (stderr, "\nSize costs\n==========\n\n");
  compute_costs (false, &rtxes);

  if (LOG_COSTS)
    fprintf (stderr, "\nSpeed costs\n===========\n\n");
  compute_costs (true, &rtxes);
}

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
	  || mode_dependent_address_p (XEXP (x, 0), MEM_ADDR_SPACE (x))))
    return false;

  return true;
}

/* If INSN is a single set between two objects that we want to split,
   return the single set.  SPEED_P says whether we are optimizing
   INSN for speed or size.

   INSN should have been passed to recog and extract_insn before this
   is called.  */

static rtx
simple_move (rtx_insn *insn, bool speed_p)
{
  rtx x;
  rtx set;
  machine_mode mode;

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
  mode = GET_MODE (SET_DEST (set));
  if (!SCALAR_INT_MODE_P (mode)
      && (mode_for_size (GET_MODE_SIZE (mode) * BITS_PER_UNIT, MODE_INT, 0)
	  == BLKmode))
    return NULL_RTX;

  /* Reject PARTIAL_INT modes.  They are used for processor specific
     purposes and it's probably best not to tamper with them.  */
  if (GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
    return NULL_RTX;

  if (!choices[speed_p].move_modes_to_split[(int) mode])
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

  b = reg_copy_graph[rs];
  if (b == NULL)
    {
      b = BITMAP_ALLOC (NULL);
      reg_copy_graph[rs] = b;
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
	  bitmap b = reg_copy_graph[i];
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
   find_decomposable_subregs.  */

enum classify_move_insn
{
  /* Not a simple move from one location to another.  */
  NOT_SIMPLE_MOVE,
  /* A simple move we want to decompose.  */
  DECOMPOSABLE_SIMPLE_MOVE,
  /* Any other simple move.  */
  SIMPLE_MOVE
};

/* If we find a SUBREG in *LOC which we could use to decompose a
   pseudo-register, set a bit in DECOMPOSABLE_CONTEXT.  If we find an
   unadorned register which is not a simple pseudo-register copy,
   DATA will point at the type of move, and we set a bit in
   DECOMPOSABLE_CONTEXT or NON_DECOMPOSABLE_CONTEXT as appropriate.  */

static void
find_decomposable_subregs (rtx *loc, enum classify_move_insn *pcmi)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, *loc, NONCONST)
    {
      rtx x = *iter;
      if (GET_CODE (x) == SUBREG)
	{
	  rtx inner = SUBREG_REG (x);
	  unsigned int regno, outer_size, inner_size, outer_words, inner_words;

	  if (!REG_P (inner))
	    continue;

	  regno = REGNO (inner);
	  if (HARD_REGISTER_NUM_P (regno))
	    {
	      iter.skip_subrtxes ();
	      continue;
	    }

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
	      iter.skip_subrtxes ();
	      continue;
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
	      iter.skip_subrtxes ();
	      continue;
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
	     copy we want to decompose, and the mode is right,
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
		case DECOMPOSABLE_SIMPLE_MOVE:
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
	     SIMPLE_MOVE or DECOMPOSABLE_SIMPLE_MOVE.  Do our own recursion
	     here, and return -1 to block the parent's recursion.  */
	  find_decomposable_subregs (&XEXP (x, 0), &cmi_mem);
	  iter.skip_subrtxes ();
	}
    }
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
simplify_subreg_concatn (machine_mode outermode, rtx op,
			 unsigned int byte)
{
  unsigned int inner_size;
  machine_mode innermode, partmode;
  rtx part;
  unsigned int final_offset;

  gcc_assert (GET_CODE (op) == CONCATN);
  gcc_assert (byte % GET_MODE_SIZE (outermode) == 0);

  innermode = GET_MODE (op);
  gcc_assert (byte < GET_MODE_SIZE (innermode));
  if (GET_MODE_SIZE (outermode) > GET_MODE_SIZE (innermode))
    return NULL_RTX;

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
simplify_gen_subreg_concatn (machine_mode outermode, rtx op,
			     machine_mode innermode, unsigned int byte)
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

/* Look for SUBREGs in *LOC which need to be decomposed.  */

static bool
resolve_subreg_use (rtx *loc, rtx insn)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, loc, NONCONST)
    {
      rtx *loc = *iter;
      rtx x = *loc;
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
	      return true;
	    }

	  validate_change (insn, loc, x, 1);
	  iter.skip_subrtxes ();
	}
      else if (resolve_reg_p (x))
	/* Return 1 to the caller to indicate that we found a direct
	   reference to a register which is being decomposed.  This can
	   happen inside notes, multiword shift or zero-extend
	   instructions.  */
	return true;
    }

  return false;
}

/* Resolve any decomposed registers which appear in register notes on
   INSN.  */

static void
resolve_reg_notes (rtx_insn *insn)
{
  rtx *pnote, note;

  note = find_reg_equal_equiv_note (insn);
  if (note)
    {
      int old_count = num_validated_changes ();
      if (resolve_subreg_use (&XEXP (note, 0), NULL_RTX))
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
	{
	  unsigned int byte, num_bytes;

	  num_bytes = GET_MODE_SIZE (GET_MODE (x));
	  for (byte = 0; byte < num_bytes; byte += UNITS_PER_WORD)
	    if (simplify_subreg_regno (regno, GET_MODE (x), byte, word_mode) < 0)
	      return false;
	  return true;
	}
      else
	return !bitmap_bit_p (subreg_context, regno);
    }

  return true;
}

/* Decompose the registers used in a simple move SET within INSN.  If
   we don't change anything, return INSN, otherwise return the start
   of the sequence of moves.  */

static rtx_insn *
resolve_simple_move (rtx set, rtx_insn *insn)
{
  rtx src, dest, real_dest;
  rtx_insn *insns;
  machine_mode orig_mode, dest_mode;
  unsigned int words;
  bool pushing;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  orig_mode = GET_MODE (dest);

  words = (GET_MODE_SIZE (orig_mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  gcc_assert (words > 1);

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
      rtx reg, smove;
      rtx_insn *minsn;

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
	resolve_subreg_use (&XEXP (src, 0), NULL_RTX);
      if (MEM_P (dest))
	resolve_subreg_use (&XEXP (dest, 0), NULL_RTX);
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

      if (AUTO_INC_DEC)
	{
	  rtx move = emit_move_insn (reg, src);
	  if (MEM_P (src))
	    {
	      rtx note = find_reg_note (insn, REG_INC, NULL_RTX);
	      if (note)
		add_reg_note (move, REG_INC, XEXP (note, 0));
	    }
	}
      else
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
      rtx mdest, smove;
      rtx_insn *minsn;

      if (dest_mode == orig_mode)
	mdest = dest;
      else
	mdest = simplify_gen_subreg (orig_mode, dest, GET_MODE (dest), 0);
      minsn = emit_move_insn (real_dest, mdest);

  if (AUTO_INC_DEC && MEM_P (real_dest)
      && !(resolve_reg_p (real_dest) || resolve_subreg_p (real_dest)))
    {
      rtx note = find_reg_note (insn, REG_INC, NULL_RTX);
      if (note)
	add_reg_note (minsn, REG_INC, XEXP (note, 0));
    }

      smove = single_set (minsn);
      gcc_assert (smove != NULL_RTX);

      resolve_simple_move (smove, minsn);
    }

  insns = get_insns ();
  end_sequence ();

  copy_reg_eh_region_note_forward (insn, insns, NULL_RTX);

  emit_insn_before (insns, insn);

  /* If we get here via self-recursion, then INSN is not yet in the insns
     chain and delete_insn will fail.  We only want to remove INSN from the
     current sequence.  See PR56738.  */
  if (in_sequence_p ())
    remove_insn (insn);
  else
    delete_insn (insn);

  return insns;
}

/* Change a CLOBBER of a decomposed register into a CLOBBER of the
   component registers.  Return whether we changed something.  */

static bool
resolve_clobber (rtx pat, rtx_insn *insn)
{
  rtx reg;
  machine_mode orig_mode;
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
resolve_use (rtx pat, rtx_insn *insn)
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
resolve_debug (rtx_insn *insn)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, &PATTERN (insn), NONCONST)
    {
      rtx *loc = *iter;
      rtx x = *loc;
      if (resolve_subreg_p (x))
	{
	  x = simplify_subreg_concatn (GET_MODE (x), SUBREG_REG (x),
				       SUBREG_BYTE (x));

	  if (x)
	    *loc = x;
	  else
	    x = copy_rtx (*loc);
	}
      if (resolve_reg_p (x))
	*loc = copy_rtx (x);
    }

  df_insn_rescan (insn);

  resolve_reg_notes (insn);
}

/* Check if INSN is a decomposable multiword-shift or zero-extend and
   set the decomposable_context bitmap accordingly.  SPEED_P is true
   if we are optimizing INSN for speed rather than size.  Return true
   if INSN is decomposable.  */

static bool
find_decomposable_shift_zext (rtx_insn *insn, bool speed_p)
{
  rtx set;
  rtx op;
  rtx op_operand;

  set = single_set (insn);
  if (!set)
    return false;

  op = SET_SRC (set);
  if (GET_CODE (op) != ASHIFT
      && GET_CODE (op) != LSHIFTRT
      && GET_CODE (op) != ASHIFTRT
      && GET_CODE (op) != ZERO_EXTEND)
    return false;

  op_operand = XEXP (op, 0);
  if (!REG_P (SET_DEST (set)) || !REG_P (op_operand)
      || HARD_REGISTER_NUM_P (REGNO (SET_DEST (set)))
      || HARD_REGISTER_NUM_P (REGNO (op_operand))
      || GET_MODE (op) != twice_word_mode)
    return false;

  if (GET_CODE (op) == ZERO_EXTEND)
    {
      if (GET_MODE (op_operand) != word_mode
	  || !choices[speed_p].splitting_zext)
	return false;
    }
  else /* left or right shift */
    {
      bool *splitting = (GET_CODE (op) == ASHIFT
			 ? choices[speed_p].splitting_ashift
			 : GET_CODE (op) == ASHIFTRT
			 ? choices[speed_p].splitting_ashiftrt
			 : choices[speed_p].splitting_lshiftrt);
      if (!CONST_INT_P (XEXP (op, 1))
	  || !IN_RANGE (INTVAL (XEXP (op, 1)), BITS_PER_WORD,
			2 * BITS_PER_WORD - 1)
	  || !splitting[INTVAL (XEXP (op, 1)) - BITS_PER_WORD])
	return false;

      bitmap_set_bit (decomposable_context, REGNO (op_operand));
    }

  bitmap_set_bit (decomposable_context, REGNO (SET_DEST (set)));

  return true;
}

/* Decompose a more than word wide shift (in INSN) of a multiword
   pseudo or a multiword zero-extend of a wordmode pseudo into a move
   and 'set to zero' insn.  Return a pointer to the new insn when a
   replacement was done.  */

static rtx_insn *
resolve_shift_zext (rtx_insn *insn)
{
  rtx set;
  rtx op;
  rtx op_operand;
  rtx_insn *insns;
  rtx src_reg, dest_reg, dest_upper, upper_src = NULL_RTX;
  int src_reg_num, dest_reg_num, offset1, offset2, src_offset;

  set = single_set (insn);
  if (!set)
    return NULL;

  op = SET_SRC (set);
  if (GET_CODE (op) != ASHIFT
      && GET_CODE (op) != LSHIFTRT
      && GET_CODE (op) != ASHIFTRT
      && GET_CODE (op) != ZERO_EXTEND)
    return NULL;

  op_operand = XEXP (op, 0);

  /* We can tear this operation apart only if the regs were already
     torn apart.  */
  if (!resolve_reg_p (SET_DEST (set)) && !resolve_reg_p (op_operand))
    return NULL;

  /* src_reg_num is the number of the word mode register which we
     are operating on.  For a left shift and a zero_extend on little
     endian machines this is register 0.  */
  src_reg_num = (GET_CODE (op) == LSHIFTRT || GET_CODE (op) == ASHIFTRT)
		? 1 : 0;

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
  dest_upper = simplify_gen_subreg_concatn (word_mode, SET_DEST (set),
					    GET_MODE (SET_DEST (set)),
					    offset2);
  src_reg = simplify_gen_subreg_concatn (word_mode, op_operand,
                                         GET_MODE (op_operand),
                                         src_offset);
  if (GET_CODE (op) == ASHIFTRT
      && INTVAL (XEXP (op, 1)) != 2 * BITS_PER_WORD - 1)
    upper_src = expand_shift (RSHIFT_EXPR, word_mode, copy_rtx (src_reg),
			      BITS_PER_WORD - 1, NULL_RTX, 0);

  if (GET_CODE (op) != ZERO_EXTEND)
    {
      int shift_count = INTVAL (XEXP (op, 1));
      if (shift_count > BITS_PER_WORD)
	src_reg = expand_shift (GET_CODE (op) == ASHIFT ?
				LSHIFT_EXPR : RSHIFT_EXPR,
				word_mode, src_reg,
				shift_count - BITS_PER_WORD,
				dest_reg, GET_CODE (op) != ASHIFTRT);
    }

  if (dest_reg != src_reg)
    emit_move_insn (dest_reg, src_reg);
  if (GET_CODE (op) != ASHIFTRT)
    emit_move_insn (dest_upper, CONST0_RTX (word_mode));
  else if (INTVAL (XEXP (op, 1)) == 2 * BITS_PER_WORD - 1)
    emit_move_insn (dest_upper, copy_rtx (src_reg));
  else
    emit_move_insn (dest_upper, upper_src);
  insns = get_insns ();

  end_sequence ();

  emit_insn_before (insns, insn);

  if (dump_file)
    {
      rtx_insn *in;
      fprintf (dump_file, "; Replacing insn: %d with insns: ", INSN_UID (insn));
      for (in = insns; in != insn; in = NEXT_INSN (in))
	fprintf (dump_file, "%d ", INSN_UID (in));
      fprintf (dump_file, "\n");
    }

  delete_insn (insn);
  return insns;
}

/* Print to dump_file a description of what we're doing with shift code CODE.
   SPLITTING[X] is true if we are splitting shifts by X + BITS_PER_WORD.  */

static void
dump_shift_choices (enum rtx_code code, bool *splitting)
{
  int i;
  const char *sep;

  fprintf (dump_file,
	   "  Splitting mode %s for %s lowering with shift amounts = ",
	   GET_MODE_NAME (twice_word_mode), GET_RTX_NAME (code));
  sep = "";
  for (i = 0; i < BITS_PER_WORD; i++)
    if (splitting[i])
      {
	fprintf (dump_file, "%s%d", sep, i + BITS_PER_WORD);
	sep = ",";
      }
  fprintf (dump_file, "\n");
}

/* Print to dump_file a description of what we're doing when optimizing
   for speed or size; SPEED_P says which.  DESCRIPTION is a description
   of the SPEED_P choice.  */

static void
dump_choices (bool speed_p, const char *description)
{
  unsigned int i;

  fprintf (dump_file, "Choices when optimizing for %s:\n", description);

  for (i = 0; i < MAX_MACHINE_MODE; i++)
    if (GET_MODE_SIZE ((machine_mode) i) > UNITS_PER_WORD)
      fprintf (dump_file, "  %s mode %s for copy lowering.\n",
	       choices[speed_p].move_modes_to_split[i]
	       ? "Splitting"
	       : "Skipping",
	       GET_MODE_NAME ((machine_mode) i));

  fprintf (dump_file, "  %s mode %s for zero_extend lowering.\n",
	   choices[speed_p].splitting_zext ? "Splitting" : "Skipping",
	   GET_MODE_NAME (twice_word_mode));

  dump_shift_choices (ASHIFT, choices[speed_p].splitting_ashift);
  dump_shift_choices (LSHIFTRT, choices[speed_p].splitting_lshiftrt);
  dump_shift_choices (ASHIFTRT, choices[speed_p].splitting_ashiftrt);
  fprintf (dump_file, "\n");
}

/* Look for registers which are always accessed via word-sized SUBREGs
   or -if DECOMPOSE_COPIES is true- via copies.  Decompose these
   registers into several word-sized pseudo-registers.  */

static void
decompose_multiword_subregs (bool decompose_copies)
{
  unsigned int max;
  basic_block bb;
  bool speed_p;

  if (dump_file)
    {
      dump_choices (false, "size");
      dump_choices (true, "speed");
    }

  /* Check if this target even has any modes to consider lowering.   */
  if (!choices[false].something_to_do && !choices[true].something_to_do)
    {
      if (dump_file)
	fprintf (dump_file, "Nothing to do!\n");
      return;
    }

  max = max_reg_num ();

  /* First see if there are any multi-word pseudo-registers.  If there
     aren't, there is nothing we can do.  This should speed up this
     pass in the normal case, since it should be faster than scanning
     all the insns.  */
  {
    unsigned int i;
    bool useful_modes_seen = false;

    for (i = FIRST_PSEUDO_REGISTER; i < max; ++i)
      if (regno_reg_rtx[i] != NULL)
	{
	  machine_mode mode = GET_MODE (regno_reg_rtx[i]);
	  if (choices[false].move_modes_to_split[(int) mode]
	      || choices[true].move_modes_to_split[(int) mode])
	    {
	      useful_modes_seen = true;
	      break;
	    }
	}

    if (!useful_modes_seen)
      {
	if (dump_file)
	  fprintf (dump_file, "Nothing to lower in this function.\n");
	return;
      }
  }

  if (df)
    {
      df_set_flags (DF_DEFER_INSN_RESCAN);
      run_word_dce ();
    }

  /* FIXME: It may be possible to change this code to look for each
     multi-word pseudo-register and to find each insn which sets or
     uses that register.  That should be faster than scanning all the
     insns.  */

  decomposable_context = BITMAP_ALLOC (NULL);
  non_decomposable_context = BITMAP_ALLOC (NULL);
  subreg_context = BITMAP_ALLOC (NULL);

  reg_copy_graph.create (max);
  reg_copy_graph.safe_grow_cleared (max);
  memset (reg_copy_graph.address (), 0, sizeof (bitmap) * max);

  speed_p = optimize_function_for_speed_p (cfun);
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

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

	  if (find_decomposable_shift_zext (insn, speed_p))
	    continue;

	  extract_insn (insn);

	  set = simple_move (insn, speed_p);

	  if (!set)
	    cmi = NOT_SIMPLE_MOVE;
	  else
	    {
	      /* We mark pseudo-to-pseudo copies as decomposable during the
		 second pass only.  The first pass is so early that there is
		 good chance such moves will be optimized away completely by
		 subsequent optimizations anyway.

		 However, we call find_pseudo_copy even during the first pass
		 so as to properly set up the reg_copy_graph.  */
	      if (find_pseudo_copy (set))
		cmi = decompose_copies? DECOMPOSABLE_SIMPLE_MOVE : SIMPLE_MOVE;
	      else
		cmi = SIMPLE_MOVE;
	    }

	  n = recog_data.n_operands;
	  for (i = 0; i < n; ++i)
	    {
	      find_decomposable_subregs (&recog_data.operand[i], &cmi);

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
      unsigned int i;
      sbitmap_iterator sbi;
      bitmap_iterator iter;
      unsigned int regno;

      propagate_pseudo_copies ();

      auto_sbitmap sub_blocks (last_basic_block_for_fn (cfun));
      bitmap_clear (sub_blocks);

      EXECUTE_IF_SET_IN_BITMAP (decomposable_context, 0, regno, iter)
	decompose_register (regno);

      FOR_EACH_BB_FN (bb, cfun)
	{
	  rtx_insn *insn;

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

		  set = simple_move (insn, speed_p);
		  if (set)
		    {
		      rtx_insn *orig_insn = insn;
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
			    bitmap_set_bit (sub_blocks, bb->index);
			}
		    }
		  else
		    {
		      rtx_insn *decomposed_shift;

		      decomposed_shift = resolve_shift_zext (insn);
		      if (decomposed_shift != NULL_RTX)
			{
			  insn = decomposed_shift;
			  recog_memoized (insn);
			  extract_insn (insn);
			}
		    }

		  for (i = recog_data.n_operands - 1; i >= 0; --i)
		    resolve_subreg_use (recog_data.operand_loc[i], insn);

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
      EXECUTE_IF_SET_IN_BITMAP (sub_blocks, 0, i, sbi)
	{
	  rtx_insn *insn, *end;
	  edge fallthru;

	  bb = BASIC_BLOCK_FOR_FN (cfun, i);
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
    }

  {
    unsigned int i;
    bitmap b;

    FOR_EACH_VEC_ELT (reg_copy_graph, i, b)
      if (b)
	BITMAP_FREE (b);
  }

  reg_copy_graph.release ();

  BITMAP_FREE (decomposable_context);
  BITMAP_FREE (non_decomposable_context);
  BITMAP_FREE (subreg_context);
}

/* Implement first lower subreg pass.  */

namespace {

const pass_data pass_data_lower_subreg =
{
  RTL_PASS, /* type */
  "subreg1", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_LOWER_SUBREG, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_subreg : public rtl_opt_pass
{
public:
  pass_lower_subreg (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_lower_subreg, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_split_wide_types != 0; }
  virtual unsigned int execute (function *)
    {
      decompose_multiword_subregs (false);
      return 0;
    }

}; // class pass_lower_subreg

} // anon namespace

rtl_opt_pass *
make_pass_lower_subreg (gcc::context *ctxt)
{
  return new pass_lower_subreg (ctxt);
}

/* Implement second lower subreg pass.  */

namespace {

const pass_data pass_data_lower_subreg2 =
{
  RTL_PASS, /* type */
  "subreg2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_LOWER_SUBREG, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_lower_subreg2 : public rtl_opt_pass
{
public:
  pass_lower_subreg2 (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_lower_subreg2, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_split_wide_types != 0; }
  virtual unsigned int execute (function *)
    {
      decompose_multiword_subregs (true);
      return 0;
    }

}; // class pass_lower_subreg2

} // anon namespace

rtl_opt_pass *
make_pass_lower_subreg2 (gcc::context *ctxt)
{
  return new pass_lower_subreg2 (ctxt);
}
