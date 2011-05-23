/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-config.h"
#include "insn-attr.h"
/* Include expr.h after insn-config.h so we get HAVE_conditional_move.  */
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "recog.h"
#include "reload.h"
#include "output.h"
#include "typeclass.h"
#include "toplev.h"
#include "langhooks.h"
#include "intl.h"
#include "tm_p.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree-flow.h"
#include "target.h"
#include "timevar.h"
#include "df.h"
#include "diagnostic.h"
#include "ssaexpand.h"
#include "target-globals.h"

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.

   They should if the stack and args grow in opposite directions, but
   only if we have push insns.  */

#ifdef PUSH_ROUNDING

#ifndef PUSH_ARGS_REVERSED
#if defined (STACK_GROWS_DOWNWARD) != defined (ARGS_GROW_DOWNWARD)
#define PUSH_ARGS_REVERSED	/* If it's last to first.  */
#endif
#endif

#endif

#ifndef STACK_PUSH_CODE
#ifdef STACK_GROWS_DOWNWARD
#define STACK_PUSH_CODE PRE_DEC
#else
#define STACK_PUSH_CODE PRE_INC
#endif
#endif


/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
int cse_not_expected;

/* This structure is used by move_by_pieces to describe the move to
   be performed.  */
struct move_by_pieces_d
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  rtx from;
  rtx from_addr;
  int autinc_from;
  int explicit_inc_from;
  unsigned HOST_WIDE_INT len;
  HOST_WIDE_INT offset;
  int reverse;
};

/* This structure is used by store_by_pieces to describe the clear to
   be performed.  */

struct store_by_pieces_d
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  unsigned HOST_WIDE_INT len;
  HOST_WIDE_INT offset;
  rtx (*constfun) (void *, HOST_WIDE_INT, enum machine_mode);
  void *constfundata;
  int reverse;
};

static unsigned HOST_WIDE_INT move_by_pieces_ninsns (unsigned HOST_WIDE_INT,
						     unsigned int,
						     unsigned int);
static void move_by_pieces_1 (rtx (*) (rtx, ...), enum machine_mode,
			      struct move_by_pieces_d *);
static bool block_move_libcall_safe_for_call_parm (void);
static bool emit_block_move_via_movmem (rtx, rtx, rtx, unsigned, unsigned, HOST_WIDE_INT);
static tree emit_block_move_libcall_fn (int);
static void emit_block_move_via_loop (rtx, rtx, rtx, unsigned);
static rtx clear_by_pieces_1 (void *, HOST_WIDE_INT, enum machine_mode);
static void clear_by_pieces (rtx, unsigned HOST_WIDE_INT, unsigned int);
static void store_by_pieces_1 (struct store_by_pieces_d *, unsigned int);
static void store_by_pieces_2 (rtx (*) (rtx, ...), enum machine_mode,
			       struct store_by_pieces_d *);
static tree clear_storage_libcall_fn (int);
static rtx compress_float_constant (rtx, rtx);
static rtx get_subtarget (rtx);
static void store_constructor_field (rtx, unsigned HOST_WIDE_INT,
				     HOST_WIDE_INT, enum machine_mode,
				     tree, tree, int, alias_set_type);
static void store_constructor (tree, rtx, int, HOST_WIDE_INT);
static rtx store_field (rtx, HOST_WIDE_INT, HOST_WIDE_INT, enum machine_mode,
			tree, tree, alias_set_type, bool);

static unsigned HOST_WIDE_INT highest_pow2_factor_for_target (const_tree, const_tree);

static int is_aligning_offset (const_tree, const_tree);
static void expand_operands (tree, tree, rtx, rtx*, rtx*,
			     enum expand_modifier);
static rtx reduce_to_bit_field_precision (rtx, rtx, tree);
static rtx do_store_flag (sepops, rtx, enum machine_mode);
#ifdef PUSH_ROUNDING
static void emit_single_push_insn (enum machine_mode, rtx, tree);
#endif
static void do_tablejump (rtx, enum machine_mode, rtx, rtx, rtx);
static rtx const_vector_from_tree (tree);
static void write_complex_part (rtx, rtx, bool);

/* This macro is used to determine whether move_by_pieces should be called
   to perform a structure copy.  */
#ifndef MOVE_BY_PIECES_P
#define MOVE_BY_PIECES_P(SIZE, ALIGN) \
  (move_by_pieces_ninsns (SIZE, ALIGN, MOVE_MAX_PIECES + 1) \
   < (unsigned int) MOVE_RATIO (optimize_insn_for_speed_p ()))
#endif

/* This macro is used to determine whether clear_by_pieces should be
   called to clear storage.  */
#ifndef CLEAR_BY_PIECES_P
#define CLEAR_BY_PIECES_P(SIZE, ALIGN) \
  (move_by_pieces_ninsns (SIZE, ALIGN, STORE_MAX_PIECES + 1) \
   < (unsigned int) CLEAR_RATIO (optimize_insn_for_speed_p ()))
#endif

/* This macro is used to determine whether store_by_pieces should be
   called to "memset" storage with byte values other than zero.  */
#ifndef SET_BY_PIECES_P
#define SET_BY_PIECES_P(SIZE, ALIGN) \
  (move_by_pieces_ninsns (SIZE, ALIGN, STORE_MAX_PIECES + 1) \
   < (unsigned int) SET_RATIO (optimize_insn_for_speed_p ()))
#endif

/* This macro is used to determine whether store_by_pieces should be
   called to "memcpy" storage when the source is a constant string.  */
#ifndef STORE_BY_PIECES_P
#define STORE_BY_PIECES_P(SIZE, ALIGN) \
  (move_by_pieces_ninsns (SIZE, ALIGN, STORE_MAX_PIECES + 1) \
   < (unsigned int) MOVE_RATIO (optimize_insn_for_speed_p ()))
#endif

/* SLOW_UNALIGNED_ACCESS is nonzero if unaligned accesses are very slow.  */

#ifndef SLOW_UNALIGNED_ACCESS
#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) STRICT_ALIGNMENT
#endif

/* This is run to set up which modes can be used
   directly in memory and to initialize the block move optab.  It is run
   at the beginning of compilation and when the target is reinitialized.  */

void
init_expr_target (void)
{
  rtx insn, pat;
  enum machine_mode mode;
  int num_clobbers;
  rtx mem, mem1;
  rtx reg;

  /* Try indexing by frame ptr and try by stack ptr.
     It is known that on the Convex the stack ptr isn't a valid index.
     With luck, one or the other is valid on any machine.  */
  mem = gen_rtx_MEM (VOIDmode, stack_pointer_rtx);
  mem1 = gen_rtx_MEM (VOIDmode, frame_pointer_rtx);

  /* A scratch register we can modify in-place below to avoid
     useless RTL allocations.  */
  reg = gen_rtx_REG (VOIDmode, -1);

  insn = rtx_alloc (INSN);
  pat = gen_rtx_SET (VOIDmode, NULL_RTX, NULL_RTX);
  PATTERN (insn) = pat;

  for (mode = VOIDmode; (int) mode < NUM_MACHINE_MODES;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      int regno;

      direct_load[(int) mode] = direct_store[(int) mode] = 0;
      PUT_MODE (mem, mode);
      PUT_MODE (mem1, mode);
      PUT_MODE (reg, mode);

      /* See if there is some register that can be used in this mode and
	 directly loaded or stored from memory.  */

      if (mode != VOIDmode && mode != BLKmode)
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER
	     && (direct_load[(int) mode] == 0 || direct_store[(int) mode] == 0);
	     regno++)
	  {
	    if (! HARD_REGNO_MODE_OK (regno, mode))
	      continue;

	    SET_REGNO (reg, regno);

	    SET_SRC (pat) = mem;
	    SET_DEST (pat) = reg;
	    if (recog (pat, insn, &num_clobbers) >= 0)
	      direct_load[(int) mode] = 1;

	    SET_SRC (pat) = mem1;
	    SET_DEST (pat) = reg;
	    if (recog (pat, insn, &num_clobbers) >= 0)
	      direct_load[(int) mode] = 1;

	    SET_SRC (pat) = reg;
	    SET_DEST (pat) = mem;
	    if (recog (pat, insn, &num_clobbers) >= 0)
	      direct_store[(int) mode] = 1;

	    SET_SRC (pat) = reg;
	    SET_DEST (pat) = mem1;
	    if (recog (pat, insn, &num_clobbers) >= 0)
	      direct_store[(int) mode] = 1;
	  }
    }

  mem = gen_rtx_MEM (VOIDmode, gen_rtx_raw_REG (Pmode, 10000));

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      enum machine_mode srcmode;
      for (srcmode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); srcmode != mode;
	   srcmode = GET_MODE_WIDER_MODE (srcmode))
	{
	  enum insn_code ic;

	  ic = can_extend_p (mode, srcmode, 0);
	  if (ic == CODE_FOR_nothing)
	    continue;

	  PUT_MODE (mem, srcmode);

	  if ((*insn_data[ic].operand[1].predicate) (mem, srcmode))
	    float_extend_from_mem[mode][srcmode] = true;
	}
    }
}

/* This is run at the start of compiling a function.  */

void
init_expr (void)
{
  memset (&crtl->expr, 0, sizeof (crtl->expr));
}

/* Copy data from FROM to TO, where the machine modes are not the same.
   Both modes may be integer, or both may be floating, or both may be
   fixed-point.
   UNSIGNEDP should be nonzero if FROM is an unsigned type.
   This causes zero-extension instead of sign-extension.  */

void
convert_move (rtx to, rtx from, int unsignedp)
{
  enum machine_mode to_mode = GET_MODE (to);
  enum machine_mode from_mode = GET_MODE (from);
  int to_real = SCALAR_FLOAT_MODE_P (to_mode);
  int from_real = SCALAR_FLOAT_MODE_P (from_mode);
  enum insn_code code;
  rtx libcall;

  /* rtx code for making an equivalent value.  */
  enum rtx_code equiv_code = (unsignedp < 0 ? UNKNOWN
			      : (unsignedp ? ZERO_EXTEND : SIGN_EXTEND));


  gcc_assert (to_real == from_real);
  gcc_assert (to_mode != BLKmode);
  gcc_assert (from_mode != BLKmode);

  /* If the source and destination are already the same, then there's
     nothing to do.  */
  if (to == from)
    return;

  /* If FROM is a SUBREG that indicates that we have already done at least
     the required extension, strip it.  We don't handle such SUBREGs as
     TO here.  */

  if (GET_CODE (from) == SUBREG && SUBREG_PROMOTED_VAR_P (from)
      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (from)))
	  >= GET_MODE_SIZE (to_mode))
      && SUBREG_PROMOTED_UNSIGNED_P (from) == unsignedp)
    from = gen_lowpart (to_mode, from), from_mode = to_mode;

  gcc_assert (GET_CODE (to) != SUBREG || !SUBREG_PROMOTED_VAR_P (to));

  if (to_mode == from_mode
      || (from_mode == VOIDmode && CONSTANT_P (from)))
    {
      emit_move_insn (to, from);
      return;
    }

  if (VECTOR_MODE_P (to_mode) || VECTOR_MODE_P (from_mode))
    {
      gcc_assert (GET_MODE_BITSIZE (from_mode) == GET_MODE_BITSIZE (to_mode));

      if (VECTOR_MODE_P (to_mode))
	from = simplify_gen_subreg (to_mode, from, GET_MODE (from), 0);
      else
	to = simplify_gen_subreg (from_mode, to, GET_MODE (to), 0);

      emit_move_insn (to, from);
      return;
    }

  if (GET_CODE (to) == CONCAT && GET_CODE (from) == CONCAT)
    {
      convert_move (XEXP (to, 0), XEXP (from, 0), unsignedp);
      convert_move (XEXP (to, 1), XEXP (from, 1), unsignedp);
      return;
    }

  if (to_real)
    {
      rtx value, insns;
      convert_optab tab;

      gcc_assert ((GET_MODE_PRECISION (from_mode)
		   != GET_MODE_PRECISION (to_mode))
		  || (DECIMAL_FLOAT_MODE_P (from_mode)
		      != DECIMAL_FLOAT_MODE_P (to_mode)));

      if (GET_MODE_PRECISION (from_mode) == GET_MODE_PRECISION (to_mode))
	/* Conversion between decimal float and binary float, same size.  */
	tab = DECIMAL_FLOAT_MODE_P (from_mode) ? trunc_optab : sext_optab;
      else if (GET_MODE_PRECISION (from_mode) < GET_MODE_PRECISION (to_mode))
	tab = sext_optab;
      else
	tab = trunc_optab;

      /* Try converting directly if the insn is supported.  */

      code = convert_optab_handler (tab, to_mode, from_mode);
      if (code != CODE_FOR_nothing)
	{
	  emit_unop_insn (code, to, from,
			  tab == sext_optab ? FLOAT_EXTEND : FLOAT_TRUNCATE);
	  return;
	}

      /* Otherwise use a libcall.  */
      libcall = convert_optab_libfunc (tab, to_mode, from_mode);

      /* Is this conversion implemented yet?  */
      gcc_assert (libcall);

      start_sequence ();
      value = emit_library_call_value (libcall, NULL_RTX, LCT_CONST, to_mode,
				       1, from, from_mode);
      insns = get_insns ();
      end_sequence ();
      emit_libcall_block (insns, to, value,
			  tab == trunc_optab ? gen_rtx_FLOAT_TRUNCATE (to_mode,
								       from)
			  : gen_rtx_FLOAT_EXTEND (to_mode, from));
      return;
    }

  /* Handle pointer conversion.  */			/* SPEE 900220.  */
  /* Targets are expected to provide conversion insns between PxImode and
     xImode for all MODE_PARTIAL_INT modes they use, but no others.  */
  if (GET_MODE_CLASS (to_mode) == MODE_PARTIAL_INT)
    {
      enum machine_mode full_mode
	= smallest_mode_for_size (GET_MODE_BITSIZE (to_mode), MODE_INT);

      gcc_assert (convert_optab_handler (trunc_optab, to_mode, full_mode)
		  != CODE_FOR_nothing);

      if (full_mode != from_mode)
	from = convert_to_mode (full_mode, from, unsignedp);
      emit_unop_insn (convert_optab_handler (trunc_optab, to_mode, full_mode),
		      to, from, UNKNOWN);
      return;
    }
  if (GET_MODE_CLASS (from_mode) == MODE_PARTIAL_INT)
    {
      rtx new_from;
      enum machine_mode full_mode
	= smallest_mode_for_size (GET_MODE_BITSIZE (from_mode), MODE_INT);

      gcc_assert (convert_optab_handler (sext_optab, full_mode, from_mode)
		  != CODE_FOR_nothing);

      if (to_mode == full_mode)
	{
	  emit_unop_insn (convert_optab_handler (sext_optab, full_mode,
						 from_mode),
			  to, from, UNKNOWN);
	  return;
	}

      new_from = gen_reg_rtx (full_mode);
      emit_unop_insn (convert_optab_handler (sext_optab, full_mode, from_mode),
		      new_from, from, UNKNOWN);

      /* else proceed to integer conversions below.  */
      from_mode = full_mode;
      from = new_from;
    }

   /* Make sure both are fixed-point modes or both are not.  */
   gcc_assert (ALL_SCALAR_FIXED_POINT_MODE_P (from_mode) ==
	       ALL_SCALAR_FIXED_POINT_MODE_P (to_mode));
   if (ALL_SCALAR_FIXED_POINT_MODE_P (from_mode))
    {
      /* If we widen from_mode to to_mode and they are in the same class,
	 we won't saturate the result.
	 Otherwise, always saturate the result to play safe.  */
      if (GET_MODE_CLASS (from_mode) == GET_MODE_CLASS (to_mode)
	  && GET_MODE_SIZE (from_mode) < GET_MODE_SIZE (to_mode))
	expand_fixed_convert (to, from, 0, 0);
      else
	expand_fixed_convert (to, from, 0, 1);
      return;
    }

  /* Now both modes are integers.  */

  /* Handle expanding beyond a word.  */
  if (GET_MODE_BITSIZE (from_mode) < GET_MODE_BITSIZE (to_mode)
      && GET_MODE_BITSIZE (to_mode) > BITS_PER_WORD)
    {
      rtx insns;
      rtx lowpart;
      rtx fill_value;
      rtx lowfrom;
      int i;
      enum machine_mode lowpart_mode;
      int nwords = CEIL (GET_MODE_SIZE (to_mode), UNITS_PER_WORD);

      /* Try converting directly if the insn is supported.  */
      if ((code = can_extend_p (to_mode, from_mode, unsignedp))
	  != CODE_FOR_nothing)
	{
	  /* If FROM is a SUBREG, put it into a register.  Do this
	     so that we always generate the same set of insns for
	     better cse'ing; if an intermediate assignment occurred,
	     we won't be doing the operation directly on the SUBREG.  */
	  if (optimize > 0 && GET_CODE (from) == SUBREG)
	    from = force_reg (from_mode, from);
	  emit_unop_insn (code, to, from, equiv_code);
	  return;
	}
      /* Next, try converting via full word.  */
      else if (GET_MODE_BITSIZE (from_mode) < BITS_PER_WORD
	       && ((code = can_extend_p (to_mode, word_mode, unsignedp))
		   != CODE_FOR_nothing))
	{
	  rtx word_to = gen_reg_rtx (word_mode);
	  if (REG_P (to))
	    {
	      if (reg_overlap_mentioned_p (to, from))
		from = force_reg (from_mode, from);
	      emit_clobber (to);
	    }
	  convert_move (word_to, from, unsignedp);
	  emit_unop_insn (code, to, word_to, equiv_code);
	  return;
	}

      /* No special multiword conversion insn; do it by hand.  */
      start_sequence ();

      /* Since we will turn this into a no conflict block, we must ensure
	 that the source does not overlap the target.  */

      if (reg_overlap_mentioned_p (to, from))
	from = force_reg (from_mode, from);

      /* Get a copy of FROM widened to a word, if necessary.  */
      if (GET_MODE_BITSIZE (from_mode) < BITS_PER_WORD)
	lowpart_mode = word_mode;
      else
	lowpart_mode = from_mode;

      lowfrom = convert_to_mode (lowpart_mode, from, unsignedp);

      lowpart = gen_lowpart (lowpart_mode, to);
      emit_move_insn (lowpart, lowfrom);

      /* Compute the value to put in each remaining word.  */
      if (unsignedp)
	fill_value = const0_rtx;
      else
	fill_value = emit_store_flag (gen_reg_rtx (word_mode),
				      LT, lowfrom, const0_rtx,
				      VOIDmode, 0, -1);

      /* Fill the remaining words.  */
      for (i = GET_MODE_SIZE (lowpart_mode) / UNITS_PER_WORD; i < nwords; i++)
	{
	  int index = (WORDS_BIG_ENDIAN ? nwords - i - 1 : i);
	  rtx subword = operand_subword (to, index, 1, to_mode);

	  gcc_assert (subword);

	  if (fill_value != subword)
	    emit_move_insn (subword, fill_value);
	}

      insns = get_insns ();
      end_sequence ();

      emit_insn (insns);
      return;
    }

  /* Truncating multi-word to a word or less.  */
  if (GET_MODE_BITSIZE (from_mode) > BITS_PER_WORD
      && GET_MODE_BITSIZE (to_mode) <= BITS_PER_WORD)
    {
      if (!((MEM_P (from)
	     && ! MEM_VOLATILE_P (from)
	     && direct_load[(int) to_mode]
	     && ! mode_dependent_address_p (XEXP (from, 0)))
	    || REG_P (from)
	    || GET_CODE (from) == SUBREG))
	from = force_reg (from_mode, from);
      convert_move (to, gen_lowpart (word_mode, from), 0);
      return;
    }

  /* Now follow all the conversions between integers
     no more than a word long.  */

  /* For truncation, usually we can just refer to FROM in a narrower mode.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode)
      && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (to_mode),
				GET_MODE_BITSIZE (from_mode)))
    {
      if (!((MEM_P (from)
	     && ! MEM_VOLATILE_P (from)
	     && direct_load[(int) to_mode]
	     && ! mode_dependent_address_p (XEXP (from, 0)))
	    || REG_P (from)
	    || GET_CODE (from) == SUBREG))
	from = force_reg (from_mode, from);
      if (REG_P (from) && REGNO (from) < FIRST_PSEUDO_REGISTER
	  && ! HARD_REGNO_MODE_OK (REGNO (from), to_mode))
	from = copy_to_reg (from);
      emit_move_insn (to, gen_lowpart (to_mode, from));
      return;
    }

  /* Handle extension.  */
  if (GET_MODE_BITSIZE (to_mode) > GET_MODE_BITSIZE (from_mode))
    {
      /* Convert directly if that works.  */
      if ((code = can_extend_p (to_mode, from_mode, unsignedp))
	  != CODE_FOR_nothing)
	{
	  emit_unop_insn (code, to, from, equiv_code);
	  return;
	}
      else
	{
	  enum machine_mode intermediate;
	  rtx tmp;
	  tree shift_amount;

	  /* Search for a mode to convert via.  */
	  for (intermediate = from_mode; intermediate != VOIDmode;
	       intermediate = GET_MODE_WIDER_MODE (intermediate))
	    if (((can_extend_p (to_mode, intermediate, unsignedp)
		  != CODE_FOR_nothing)
		 || (GET_MODE_SIZE (to_mode) < GET_MODE_SIZE (intermediate)
		     && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (to_mode),
					       GET_MODE_BITSIZE (intermediate))))
		&& (can_extend_p (intermediate, from_mode, unsignedp)
		    != CODE_FOR_nothing))
	      {
		convert_move (to, convert_to_mode (intermediate, from,
						   unsignedp), unsignedp);
		return;
	      }

	  /* No suitable intermediate mode.
	     Generate what we need with	shifts.  */
	  shift_amount = build_int_cst (NULL_TREE,
					GET_MODE_BITSIZE (to_mode)
					- GET_MODE_BITSIZE (from_mode));
	  from = gen_lowpart (to_mode, force_reg (from_mode, from));
	  tmp = expand_shift (LSHIFT_EXPR, to_mode, from, shift_amount,
			      to, unsignedp);
	  tmp = expand_shift (RSHIFT_EXPR, to_mode, tmp, shift_amount,
			      to, unsignedp);
	  if (tmp != to)
	    emit_move_insn (to, tmp);
	  return;
	}
    }

  /* Support special truncate insns for certain modes.  */
  if (convert_optab_handler (trunc_optab, to_mode,
			     from_mode) != CODE_FOR_nothing)
    {
      emit_unop_insn (convert_optab_handler (trunc_optab, to_mode, from_mode),
		      to, from, UNKNOWN);
      return;
    }

  /* Handle truncation of volatile memrefs, and so on;
     the things that couldn't be truncated directly,
     and for which there was no special instruction.

     ??? Code above formerly short-circuited this, for most integer
     mode pairs, with a force_reg in from_mode followed by a recursive
     call to this routine.  Appears always to have been wrong.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode))
    {
      rtx temp = force_reg (to_mode, gen_lowpart (to_mode, from));
      emit_move_insn (to, temp);
      return;
    }

  /* Mode combination is not recognized.  */
  gcc_unreachable ();
}

/* Return an rtx for a value that would result
   from converting X to mode MODE.
   Both X and MODE may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.
   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.  */

rtx
convert_to_mode (enum machine_mode mode, rtx x, int unsignedp)
{
  return convert_modes (mode, VOIDmode, x, unsignedp);
}

/* Return an rtx for a value that would result
   from converting X from mode OLDMODE to mode MODE.
   Both modes may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.

   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.

   You can give VOIDmode for OLDMODE, if you are sure X has a nonvoid mode.  */

rtx
convert_modes (enum machine_mode mode, enum machine_mode oldmode, rtx x, int unsignedp)
{
  rtx temp;

  /* If FROM is a SUBREG that indicates that we have already done at least
     the required extension, strip it.  */

  if (GET_CODE (x) == SUBREG && SUBREG_PROMOTED_VAR_P (x)
      && GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))) >= GET_MODE_SIZE (mode)
      && SUBREG_PROMOTED_UNSIGNED_P (x) == unsignedp)
    x = gen_lowpart (mode, x);

  if (GET_MODE (x) != VOIDmode)
    oldmode = GET_MODE (x);

  if (mode == oldmode)
    return x;

  /* There is one case that we must handle specially: If we are converting
     a CONST_INT into a mode whose size is twice HOST_BITS_PER_WIDE_INT and
     we are to interpret the constant as unsigned, gen_lowpart will do
     the wrong if the constant appears negative.  What we want to do is
     make the high-order word of the constant zero, not all ones.  */

  if (unsignedp && GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_BITSIZE (mode) == 2 * HOST_BITS_PER_WIDE_INT
      && CONST_INT_P (x) && INTVAL (x) < 0)
    {
      double_int val = uhwi_to_double_int (INTVAL (x));

      /* We need to zero extend VAL.  */
      if (oldmode != VOIDmode)
	val = double_int_zext (val, GET_MODE_BITSIZE (oldmode));

      return immed_double_int_const (val, mode);
    }

  /* We can do this with a gen_lowpart if both desired and current modes
     are integer, and this is either a constant integer, a register, or a
     non-volatile MEM.  Except for the constant case where MODE is no
     wider than HOST_BITS_PER_WIDE_INT, we must be narrowing the operand.  */

  if ((CONST_INT_P (x)
       && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
      || (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_CLASS (oldmode) == MODE_INT
	  && (GET_CODE (x) == CONST_DOUBLE
	      || (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (oldmode)
		  && ((MEM_P (x) && ! MEM_VOLATILE_P (x)
		       && direct_load[(int) mode])
		      || (REG_P (x)
			  && (! HARD_REGISTER_P (x)
			      || HARD_REGNO_MODE_OK (REGNO (x), mode))
			  && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (mode),
						    GET_MODE_BITSIZE (GET_MODE (x)))))))))
    {
      /* ?? If we don't know OLDMODE, we have to assume here that
	 X does not need sign- or zero-extension.   This may not be
	 the case, but it's the best we can do.  */
      if (CONST_INT_P (x) && oldmode != VOIDmode
	  && GET_MODE_SIZE (mode) > GET_MODE_SIZE (oldmode))
	{
	  HOST_WIDE_INT val = INTVAL (x);
	  int width = GET_MODE_BITSIZE (oldmode);

	  /* We must sign or zero-extend in this case.  Start by
	     zero-extending, then sign extend if we need to.  */
	  val &= ((HOST_WIDE_INT) 1 << width) - 1;
	  if (! unsignedp
	      && (val & ((HOST_WIDE_INT) 1 << (width - 1))))
	    val |= (HOST_WIDE_INT) (-1) << width;

	  return gen_int_mode (val, mode);
	}

      return gen_lowpart (mode, x);
    }

  /* Converting from integer constant into mode is always equivalent to an
     subreg operation.  */
  if (VECTOR_MODE_P (mode) && GET_MODE (x) == VOIDmode)
    {
      gcc_assert (GET_MODE_BITSIZE (mode) == GET_MODE_BITSIZE (oldmode));
      return simplify_gen_subreg (mode, x, oldmode, 0);
    }

  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

/* Return the largest alignment we can use for doing a move (or store)
   of MAX_PIECES.  ALIGN is the largest alignment we could use.  */

static unsigned int
alignment_for_piecewise_move (unsigned int max_pieces, unsigned int align)
{
  enum machine_mode tmode;

  tmode = mode_for_size (max_pieces * BITS_PER_UNIT, MODE_INT, 1);
  if (align >= GET_MODE_ALIGNMENT (tmode))
    align = GET_MODE_ALIGNMENT (tmode);
  else
    {
      enum machine_mode tmode, xmode;

      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT), xmode = tmode;
	   tmode != VOIDmode;
	   xmode = tmode, tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) > max_pieces
	    || SLOW_UNALIGNED_ACCESS (tmode, align))
	  break;

      align = MAX (align, GET_MODE_ALIGNMENT (xmode));
    }

  return align;
}

/* Return the widest integer mode no wider than SIZE.  If no such mode
   can be found, return VOIDmode.  */

static enum machine_mode
widest_int_mode_for_size (unsigned int size)
{
  enum machine_mode tmode, mode = VOIDmode;

  for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
    if (GET_MODE_SIZE (tmode) < size)
      mode = tmode;

  return mode;
}

/* STORE_MAX_PIECES is the number of bytes at a time that we can
   store efficiently.  Due to internal GCC limitations, this is
   MOVE_MAX_PIECES limited by the number of bytes GCC can represent
   for an immediate constant.  */

#define STORE_MAX_PIECES  MIN (MOVE_MAX_PIECES, 2 * sizeof (HOST_WIDE_INT))

/* Determine whether the LEN bytes can be moved by using several move
   instructions.  Return nonzero if a call to move_by_pieces should
   succeed.  */

int
can_move_by_pieces (unsigned HOST_WIDE_INT len,
		    unsigned int align ATTRIBUTE_UNUSED)
{
  return MOVE_BY_PIECES_P (len, align);
}

/* Generate several move instructions to copy LEN bytes from block FROM to
   block TO.  (These are MEM rtx's with BLKmode).

   If PUSH_ROUNDING is defined and TO is NULL, emit_single_push_insn is
   used to push FROM to the stack.

   ALIGN is maximum stack alignment we can assume.

   If ENDP is 0 return to, if ENDP is 1 return memory at the end ala
   mempcpy, and if ENDP is 2 return memory the end minus one byte ala
   stpcpy.  */

rtx
move_by_pieces (rtx to, rtx from, unsigned HOST_WIDE_INT len,
		unsigned int align, int endp)
{
  struct move_by_pieces_d data;
  enum machine_mode to_addr_mode, from_addr_mode
    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (from));
  rtx to_addr, from_addr = XEXP (from, 0);
  unsigned int max_size = MOVE_MAX_PIECES + 1;
  enum insn_code icode;

  align = MIN (to ? MEM_ALIGN (to) : align, MEM_ALIGN (from));

  data.offset = 0;
  data.from_addr = from_addr;
  if (to)
    {
      to_addr_mode = targetm.addr_space.address_mode (MEM_ADDR_SPACE (to));
      to_addr = XEXP (to, 0);
      data.to = to;
      data.autinc_to
	= (GET_CODE (to_addr) == PRE_INC || GET_CODE (to_addr) == PRE_DEC
	   || GET_CODE (to_addr) == POST_INC || GET_CODE (to_addr) == POST_DEC);
      data.reverse
	= (GET_CODE (to_addr) == PRE_DEC || GET_CODE (to_addr) == POST_DEC);
    }
  else
    {
      to_addr_mode = VOIDmode;
      to_addr = NULL_RTX;
      data.to = NULL_RTX;
      data.autinc_to = 1;
#ifdef STACK_GROWS_DOWNWARD
      data.reverse = 1;
#else
      data.reverse = 0;
#endif
    }
  data.to_addr = to_addr;
  data.from = from;
  data.autinc_from
    = (GET_CODE (from_addr) == PRE_INC || GET_CODE (from_addr) == PRE_DEC
       || GET_CODE (from_addr) == POST_INC
       || GET_CODE (from_addr) == POST_DEC);

  data.explicit_inc_from = 0;
  data.explicit_inc_to = 0;
  if (data.reverse) data.offset = len;
  data.len = len;

  /* If copying requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!(data.autinc_from && data.autinc_to)
      && move_by_pieces_ninsns (len, align, max_size) > 2)
    {
      /* Find the mode of the largest move...
	 MODE might not be used depending on the definitions of the
	 USE_* macros below.  */
      enum machine_mode mode ATTRIBUTE_UNUSED
	= widest_int_mode_for_size (max_size);

      if (USE_LOAD_PRE_DECREMENT (mode) && data.reverse && ! data.autinc_from)
	{
	  data.from_addr = copy_to_mode_reg (from_addr_mode,
					     plus_constant (from_addr, len));
	  data.autinc_from = 1;
	  data.explicit_inc_from = -1;
	}
      if (USE_LOAD_POST_INCREMENT (mode) && ! data.autinc_from)
	{
	  data.from_addr = copy_to_mode_reg (from_addr_mode, from_addr);
	  data.autinc_from = 1;
	  data.explicit_inc_from = 1;
	}
      if (!data.autinc_from && CONSTANT_P (from_addr))
	data.from_addr = copy_to_mode_reg (from_addr_mode, from_addr);
      if (USE_STORE_PRE_DECREMENT (mode) && data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_to_mode_reg (to_addr_mode,
					   plus_constant (to_addr, len));
	  data.autinc_to = 1;
	  data.explicit_inc_to = -1;
	}
      if (USE_STORE_POST_INCREMENT (mode) && ! data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_to_mode_reg (to_addr_mode, to_addr);
	  data.autinc_to = 1;
	  data.explicit_inc_to = 1;
	}
      if (!data.autinc_to && CONSTANT_P (to_addr))
	data.to_addr = copy_to_mode_reg (to_addr_mode, to_addr);
    }

  align = alignment_for_piecewise_move (MOVE_MAX_PIECES, align);

  /* First move what we can in the largest integer mode, then go to
     successively smaller modes.  */

  while (max_size > 1)
    {
      enum machine_mode mode = widest_int_mode_for_size (max_size);

      if (mode == VOIDmode)
	break;

      icode = optab_handler (mov_optab, mode);
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	move_by_pieces_1 (GEN_FCN (icode), mode, &data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  gcc_assert (!data.len);

  if (endp)
    {
      rtx to1;

      gcc_assert (!data.reverse);
      if (data.autinc_to)
	{
	  if (endp == 2)
	    {
	      if (HAVE_POST_INCREMENT && data.explicit_inc_to > 0)
		emit_insn (gen_add2_insn (data.to_addr, constm1_rtx));
	      else
		data.to_addr = copy_to_mode_reg (to_addr_mode,
						 plus_constant (data.to_addr,
								-1));
	    }
	  to1 = adjust_automodify_address (data.to, QImode, data.to_addr,
					   data.offset);
	}
      else
	{
	  if (endp == 2)
	    --data.offset;
	  to1 = adjust_address (data.to, QImode, data.offset);
	}
      return to1;
    }
  else
    return data.to;
}

/* Return number of insns required to move L bytes by pieces.
   ALIGN (in bits) is maximum alignment we can assume.  */

static unsigned HOST_WIDE_INT
move_by_pieces_ninsns (unsigned HOST_WIDE_INT l, unsigned int align,
		       unsigned int max_size)
{
  unsigned HOST_WIDE_INT n_insns = 0;

  align = alignment_for_piecewise_move (MOVE_MAX_PIECES, align);

  while (max_size > 1)
    {
      enum machine_mode mode;
      enum insn_code icode;

      mode = widest_int_mode_for_size (max_size);

      if (mode == VOIDmode)
	break;

      icode = optab_handler (mov_optab, mode);
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	n_insns += l / GET_MODE_SIZE (mode), l %= GET_MODE_SIZE (mode);

      max_size = GET_MODE_SIZE (mode);
    }

  gcc_assert (!l);
  return n_insns;
}

/* Subroutine of move_by_pieces.  Move as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
move_by_pieces_1 (rtx (*genfun) (rtx, ...), enum machine_mode mode,
		  struct move_by_pieces_d *data)
{
  unsigned int size = GET_MODE_SIZE (mode);
  rtx to1 = NULL_RTX, from1;

  while (data->len >= size)
    {
      if (data->reverse)
	data->offset -= size;

      if (data->to)
	{
	  if (data->autinc_to)
	    to1 = adjust_automodify_address (data->to, mode, data->to_addr,
					     data->offset);
	  else
	    to1 = adjust_address (data->to, mode, data->offset);
	}

      if (data->autinc_from)
	from1 = adjust_automodify_address (data->from, mode, data->from_addr,
					   data->offset);
      else
	from1 = adjust_address (data->from, mode, data->offset);

      if (HAVE_PRE_DECREMENT && data->explicit_inc_to < 0)
	emit_insn (gen_add2_insn (data->to_addr,
				  GEN_INT (-(HOST_WIDE_INT)size)));
      if (HAVE_PRE_DECREMENT && data->explicit_inc_from < 0)
	emit_insn (gen_add2_insn (data->from_addr,
				  GEN_INT (-(HOST_WIDE_INT)size)));

      if (data->to)
	emit_insn ((*genfun) (to1, from1));
      else
	{
#ifdef PUSH_ROUNDING
	  emit_single_push_insn (mode, from1, NULL);
#else
	  gcc_unreachable ();
#endif
	}

      if (HAVE_POST_INCREMENT && data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (size)));
      if (HAVE_POST_INCREMENT && data->explicit_inc_from > 0)
	emit_insn (gen_add2_insn (data->from_addr, GEN_INT (size)));

      if (! data->reverse)
	data->offset += size;

      data->len -= size;
    }
}

/* Emit code to move a block Y to a block X.  This may be done with
   string-move instructions, with multiple scalar move instructions,
   or with a library call.

   Both X and Y must be MEM rtx's (perhaps inside VOLATILE) with mode BLKmode.
   SIZE is an rtx that says how long they are.
   ALIGN is the maximum alignment we can assume they have.
   METHOD describes what kind of copy this is, and what mechanisms may be used.

   Return the address of the new block, if memcpy is called and returns it,
   0 otherwise.  */

rtx
emit_block_move_hints (rtx x, rtx y, rtx size, enum block_op_methods method,
		       unsigned int expected_align, HOST_WIDE_INT expected_size)
{
  bool may_use_call;
  rtx retval = 0;
  unsigned int align;

  gcc_assert (size);
  if (CONST_INT_P (size)
      && INTVAL (size) == 0)
    return 0;

  switch (method)
    {
    case BLOCK_OP_NORMAL:
    case BLOCK_OP_TAILCALL:
      may_use_call = true;
      break;

    case BLOCK_OP_CALL_PARM:
      may_use_call = block_move_libcall_safe_for_call_parm ();

      /* Make inhibit_defer_pop nonzero around the library call
	 to force it to pop the arguments right away.  */
      NO_DEFER_POP;
      break;

    case BLOCK_OP_NO_LIBCALL:
      may_use_call = false;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (MEM_P (x) && MEM_P (y));
  align = MIN (MEM_ALIGN (x), MEM_ALIGN (y));
  gcc_assert (align >= BITS_PER_UNIT);

  /* Make sure we've got BLKmode addresses; store_one_arg can decide that
     block copy is more efficient for other large modes, e.g. DCmode.  */
  x = adjust_address (x, BLKmode, 0);
  y = adjust_address (y, BLKmode, 0);

  /* Set MEM_SIZE as appropriate for this block copy.  The main place this
     can be incorrect is coming from __builtin_memcpy.  */
  if (CONST_INT_P (size))
    {
      x = shallow_copy_rtx (x);
      y = shallow_copy_rtx (y);
      set_mem_size (x, size);
      set_mem_size (y, size);
    }

  if (CONST_INT_P (size) && MOVE_BY_PIECES_P (INTVAL (size), align))
    move_by_pieces (x, y, INTVAL (size), align, 0);
  else if (emit_block_move_via_movmem (x, y, size, align,
				       expected_align, expected_size))
    ;
  else if (may_use_call
	   && ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (x))
	   && ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (y)))
    retval = emit_block_move_via_libcall (x, y, size,
					  method == BLOCK_OP_TAILCALL);
  else
    emit_block_move_via_loop (x, y, size, align);

  if (method == BLOCK_OP_CALL_PARM)
    OK_DEFER_POP;

  return retval;
}

rtx
emit_block_move (rtx x, rtx y, rtx size, enum block_op_methods method)
{
  return emit_block_move_hints (x, y, size, method, 0, -1);
}

/* A subroutine of emit_block_move.  Returns true if calling the
   block move libcall will not clobber any parameters which may have
   already been placed on the stack.  */

static bool
block_move_libcall_safe_for_call_parm (void)
{
#if defined (REG_PARM_STACK_SPACE)
  tree fn;
#endif

  /* If arguments are pushed on the stack, then they're safe.  */
  if (PUSH_ARGS)
    return true;

  /* If registers go on the stack anyway, any argument is sure to clobber
     an outgoing argument.  */
#if defined (REG_PARM_STACK_SPACE)
  fn = emit_block_move_libcall_fn (false);
  /* Avoid set but not used warning if *REG_PARM_STACK_SPACE doesn't
     depend on its argument.  */
  (void) fn;
  if (OUTGOING_REG_PARM_STACK_SPACE ((!fn ? NULL_TREE : TREE_TYPE (fn)))
      && REG_PARM_STACK_SPACE (fn) != 0)
    return false;
#endif

  /* If any argument goes in memory, then it might clobber an outgoing
     argument.  */
  {
    CUMULATIVE_ARGS args_so_far;
    tree fn, arg;

    fn = emit_block_move_libcall_fn (false);
    INIT_CUMULATIVE_ARGS (args_so_far, TREE_TYPE (fn), NULL_RTX, 0, 3);

    arg = TYPE_ARG_TYPES (TREE_TYPE (fn));
    for ( ; arg != void_list_node ; arg = TREE_CHAIN (arg))
      {
	enum machine_mode mode = TYPE_MODE (TREE_VALUE (arg));
	rtx tmp = targetm.calls.function_arg (&args_so_far, mode,
					      NULL_TREE, true);
	if (!tmp || !REG_P (tmp))
	  return false;
	if (targetm.calls.arg_partial_bytes (&args_so_far, mode, NULL, 1))
	  return false;
	targetm.calls.function_arg_advance (&args_so_far, mode,
					    NULL_TREE, true);
      }
  }
  return true;
}

/* A subroutine of emit_block_move.  Expand a movmem pattern;
   return true if successful.  */

static bool
emit_block_move_via_movmem (rtx x, rtx y, rtx size, unsigned int align,
			    unsigned int expected_align, HOST_WIDE_INT expected_size)
{
  rtx opalign = GEN_INT (align / BITS_PER_UNIT);
  int save_volatile_ok = volatile_ok;
  enum machine_mode mode;

  if (expected_align < align)
    expected_align = align;

  /* Since this is a move insn, we don't care about volatility.  */
  volatile_ok = 1;

  /* Try the most limited insn first, because there's no point
     including more than one in the machine description unless
     the more limited one has some advantage.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      enum insn_code code = direct_optab_handler (movmem_optab, mode);
      insn_operand_predicate_fn pred;

      if (code != CODE_FOR_nothing
	  /* We don't need MODE to be narrower than BITS_PER_HOST_WIDE_INT
	     here because if SIZE is less than the mode mask, as it is
	     returned by the macro, it will definitely be less than the
	     actual mode mask.  */
	  && ((CONST_INT_P (size)
	       && ((unsigned HOST_WIDE_INT) INTVAL (size)
		   <= (GET_MODE_MASK (mode) >> 1)))
	      || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
	  && ((pred = insn_data[(int) code].operand[0].predicate) == 0
	      || (*pred) (x, BLKmode))
	  && ((pred = insn_data[(int) code].operand[1].predicate) == 0
	      || (*pred) (y, BLKmode))
	  && ((pred = insn_data[(int) code].operand[3].predicate) == 0
	      || (*pred) (opalign, VOIDmode)))
	{
	  rtx op2;
	  rtx last = get_last_insn ();
	  rtx pat;

	  op2 = convert_to_mode (mode, size, 1);
	  pred = insn_data[(int) code].operand[2].predicate;
	  if (pred != 0 && ! (*pred) (op2, mode))
	    op2 = copy_to_mode_reg (mode, op2);

	  /* ??? When called via emit_block_move_for_call, it'd be
	     nice if there were some way to inform the backend, so
	     that it doesn't fail the expansion because it thinks
	     emitting the libcall would be more efficient.  */

	  if (insn_data[(int) code].n_operands == 4)
	    pat = GEN_FCN ((int) code) (x, y, op2, opalign);
	  else
	    pat = GEN_FCN ((int) code) (x, y, op2, opalign,
					GEN_INT (expected_align
						 / BITS_PER_UNIT),
					GEN_INT (expected_size));
	  if (pat)
	    {
	      emit_insn (pat);
	      volatile_ok = save_volatile_ok;
	      return true;
	    }
	  else
	    delete_insns_since (last);
	}
    }

  volatile_ok = save_volatile_ok;
  return false;
}

/* A subroutine of emit_block_move.  Expand a call to memcpy.
   Return the return value from memcpy, 0 otherwise.  */

rtx
emit_block_move_via_libcall (rtx dst, rtx src, rtx size, bool tailcall)
{
  rtx dst_addr, src_addr;
  tree call_expr, fn, src_tree, dst_tree, size_tree;
  enum machine_mode size_mode;
  rtx retval;

  /* Emit code to copy the addresses of DST and SRC and SIZE into new
     pseudos.  We can then place those new pseudos into a VAR_DECL and
     use them later.  */

  dst_addr = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  src_addr = copy_to_mode_reg (Pmode, XEXP (src, 0));

  dst_addr = convert_memory_address (ptr_mode, dst_addr);
  src_addr = convert_memory_address (ptr_mode, src_addr);

  dst_tree = make_tree (ptr_type_node, dst_addr);
  src_tree = make_tree (ptr_type_node, src_addr);

  size_mode = TYPE_MODE (sizetype);

  size = convert_to_mode (size_mode, size, 1);
  size = copy_to_mode_reg (size_mode, size);

  /* It is incorrect to use the libcall calling conventions to call
     memcpy in this context.  This could be a user call to memcpy and
     the user may wish to examine the return value from memcpy.  For
     targets where libcalls and normal calls have different conventions
     for returning pointers, we could end up generating incorrect code.  */

  size_tree = make_tree (sizetype, size);

  fn = emit_block_move_libcall_fn (true);
  call_expr = build_call_expr (fn, 3, dst_tree, src_tree, size_tree);
  CALL_EXPR_TAILCALL (call_expr) = tailcall;

  retval = expand_normal (call_expr);

  return retval;
}

/* A subroutine of emit_block_move_via_libcall.  Create the tree node
   for the function we use for block copies.  The first time FOR_CALL
   is true, we call assemble_external.  */

static GTY(()) tree block_move_fn;

void
init_block_move_fn (const char *asmspec)
{
  if (!block_move_fn)
    {
      tree args, fn;

      fn = get_identifier ("memcpy");
      args = build_function_type_list (ptr_type_node, ptr_type_node,
				       const_ptr_type_node, sizetype,
				       NULL_TREE);

      fn = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, fn, args);
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      TREE_NOTHROW (fn) = 1;
      DECL_VISIBILITY (fn) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (fn) = 1;

      block_move_fn = fn;
    }

  if (asmspec)
    set_user_assembler_name (block_move_fn, asmspec);
}

static tree
emit_block_move_libcall_fn (int for_call)
{
  static bool emitted_extern;

  if (!block_move_fn)
    init_block_move_fn (NULL);

  if (for_call && !emitted_extern)
    {
      emitted_extern = true;
      make_decl_rtl (block_move_fn);
      assemble_external (block_move_fn);
    }

  return block_move_fn;
}

/* A subroutine of emit_block_move.  Copy the data via an explicit
   loop.  This is used only when libcalls are forbidden.  */
/* ??? It'd be nice to copy in hunks larger than QImode.  */

static void
emit_block_move_via_loop (rtx x, rtx y, rtx size,
			  unsigned int align ATTRIBUTE_UNUSED)
{
  rtx cmp_label, top_label, iter, x_addr, y_addr, tmp;
  enum machine_mode x_addr_mode
    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (x));
  enum machine_mode y_addr_mode
    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (y));
  enum machine_mode iter_mode;

  iter_mode = GET_MODE (size);
  if (iter_mode == VOIDmode)
    iter_mode = word_mode;

  top_label = gen_label_rtx ();
  cmp_label = gen_label_rtx ();
  iter = gen_reg_rtx (iter_mode);

  emit_move_insn (iter, const0_rtx);

  x_addr = force_operand (XEXP (x, 0), NULL_RTX);
  y_addr = force_operand (XEXP (y, 0), NULL_RTX);
  do_pending_stack_adjust ();

  emit_jump (cmp_label);
  emit_label (top_label);

  tmp = convert_modes (x_addr_mode, iter_mode, iter, true);
  x_addr = gen_rtx_PLUS (x_addr_mode, x_addr, tmp);

  if (x_addr_mode != y_addr_mode)
    tmp = convert_modes (y_addr_mode, iter_mode, iter, true);
  y_addr = gen_rtx_PLUS (y_addr_mode, y_addr, tmp);

  x = change_address (x, QImode, x_addr);
  y = change_address (y, QImode, y_addr);

  emit_move_insn (x, y);

  tmp = expand_simple_binop (iter_mode, PLUS, iter, const1_rtx, iter,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != iter)
    emit_move_insn (iter, tmp);

  emit_label (cmp_label);

  emit_cmp_and_jump_insns (iter, size, LT, NULL_RTX, iter_mode,
			   true, top_label);
}

/* Copy all or part of a value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

void
move_block_to_reg (int regno, rtx x, int nregs, enum machine_mode mode)
{
  int i;
#ifdef HAVE_load_multiple
  rtx pat;
  rtx last;
#endif

  if (nregs == 0)
    return;

  if (CONSTANT_P (x) && ! LEGITIMATE_CONSTANT_P (x))
    x = validize_mem (force_const_mem (mode, x));

  /* See if the machine can do this with a load multiple insn.  */
#ifdef HAVE_load_multiple
  if (HAVE_load_multiple)
    {
      last = get_last_insn ();
      pat = gen_load_multiple (gen_rtx_REG (word_mode, regno), x,
			       GEN_INT (nregs));
      if (pat)
	{
	  emit_insn (pat);
	  return;
	}
      else
	delete_insns_since (last);
    }
#endif

  for (i = 0; i < nregs; i++)
    emit_move_insn (gen_rtx_REG (word_mode, regno + i),
		    operand_subword_force (x, i, mode));
}

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

void
move_block_from_reg (int regno, rtx x, int nregs)
{
  int i;

  if (nregs == 0)
    return;

  /* See if the machine can do this with a store multiple insn.  */
#ifdef HAVE_store_multiple
  if (HAVE_store_multiple)
    {
      rtx last = get_last_insn ();
      rtx pat = gen_store_multiple (x, gen_rtx_REG (word_mode, regno),
				    GEN_INT (nregs));
      if (pat)
	{
	  emit_insn (pat);
	  return;
	}
      else
	delete_insns_since (last);
    }
#endif

  for (i = 0; i < nregs; i++)
    {
      rtx tem = operand_subword (x, i, 1, BLKmode);

      gcc_assert (tem);

      emit_move_insn (tem, gen_rtx_REG (word_mode, regno + i));
    }
}

/* Generate a PARALLEL rtx for a new non-consecutive group of registers from
   ORIG, where ORIG is a non-consecutive group of registers represented by
   a PARALLEL.  The clone is identical to the original except in that the
   original set of registers is replaced by a new set of pseudo registers.
   The new set has the same modes as the original set.  */

rtx
gen_group_rtx (rtx orig)
{
  int i, length;
  rtx *tmps;

  gcc_assert (GET_CODE (orig) == PARALLEL);

  length = XVECLEN (orig, 0);
  tmps = XALLOCAVEC (rtx, length);

  /* Skip a NULL entry in first slot.  */
  i = XEXP (XVECEXP (orig, 0, 0), 0) ? 0 : 1;

  if (i)
    tmps[0] = 0;

  for (; i < length; i++)
    {
      enum machine_mode mode = GET_MODE (XEXP (XVECEXP (orig, 0, i), 0));
      rtx offset = XEXP (XVECEXP (orig, 0, i), 1);

      tmps[i] = gen_rtx_EXPR_LIST (VOIDmode, gen_reg_rtx (mode), offset);
    }

  return gen_rtx_PARALLEL (GET_MODE (orig), gen_rtvec_v (length, tmps));
}

/* A subroutine of emit_group_load.  Arguments as for emit_group_load,
   except that values are placed in TMPS[i], and must later be moved
   into corresponding XEXP (XVECEXP (DST, 0, i), 0) element.  */

static void
emit_group_load_1 (rtx *tmps, rtx dst, rtx orig_src, tree type, int ssize)
{
  rtx src;
  int start, i;
  enum machine_mode m = GET_MODE (orig_src);

  gcc_assert (GET_CODE (dst) == PARALLEL);

  if (m != VOIDmode
      && !SCALAR_INT_MODE_P (m)
      && !MEM_P (orig_src)
      && GET_CODE (orig_src) != CONCAT)
    {
      enum machine_mode imode = int_mode_for_mode (GET_MODE (orig_src));
      if (imode == BLKmode)
	src = assign_stack_temp (GET_MODE (orig_src), ssize, 0);
      else
	src = gen_reg_rtx (imode);
      if (imode != BLKmode)
	src = gen_lowpart (GET_MODE (orig_src), src);
      emit_move_insn (src, orig_src);
      /* ...and back again.  */
      if (imode != BLKmode)
	src = gen_lowpart (imode, src);
      emit_group_load_1 (tmps, dst, src, type, ssize);
      return;
    }

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (dst, 0, 0), 0))
    start = 0;
  else
    start = 1;

  /* Process the pieces.  */
  for (i = start; i < XVECLEN (dst, 0); i++)
    {
      enum machine_mode mode = GET_MODE (XEXP (XVECEXP (dst, 0, i), 0));
      HOST_WIDE_INT bytepos = INTVAL (XEXP (XVECEXP (dst, 0, i), 1));
      unsigned int bytelen = GET_MODE_SIZE (mode);
      int shift = 0;

      /* Handle trailing fragments that run over the size of the struct.  */
      if (ssize >= 0 && bytepos + (HOST_WIDE_INT) bytelen > ssize)
	{
	  /* Arrange to shift the fragment to where it belongs.
	     extract_bit_field loads to the lsb of the reg.  */
	  if (
#ifdef BLOCK_REG_PADDING
	      BLOCK_REG_PADDING (GET_MODE (orig_src), type, i == start)
	      == (BYTES_BIG_ENDIAN ? upward : downward)
#else
	      BYTES_BIG_ENDIAN
#endif
	      )
	    shift = (bytelen - (ssize - bytepos)) * BITS_PER_UNIT;
	  bytelen = ssize - bytepos;
	  gcc_assert (bytelen > 0);
	}

      /* If we won't be loading directly from memory, protect the real source
	 from strange tricks we might play; but make sure that the source can
	 be loaded directly into the destination.  */
      src = orig_src;
      if (!MEM_P (orig_src)
	  && (!CONSTANT_P (orig_src)
	      || (GET_MODE (orig_src) != mode
		  && GET_MODE (orig_src) != VOIDmode)))
	{
	  if (GET_MODE (orig_src) == VOIDmode)
	    src = gen_reg_rtx (mode);
	  else
	    src = gen_reg_rtx (GET_MODE (orig_src));

	  emit_move_insn (src, orig_src);
	}

      /* Optimize the access just a bit.  */
      if (MEM_P (src)
	  && (! SLOW_UNALIGNED_ACCESS (mode, MEM_ALIGN (src))
	      || MEM_ALIGN (src) >= GET_MODE_ALIGNMENT (mode))
	  && bytepos * BITS_PER_UNIT % GET_MODE_ALIGNMENT (mode) == 0
	  && bytelen == GET_MODE_SIZE (mode))
	{
	  tmps[i] = gen_reg_rtx (mode);
	  emit_move_insn (tmps[i], adjust_address (src, mode, bytepos));
	}
      else if (COMPLEX_MODE_P (mode)
	       && GET_MODE (src) == mode
	       && bytelen == GET_MODE_SIZE (mode))
	/* Let emit_move_complex do the bulk of the work.  */
	tmps[i] = src;
      else if (GET_CODE (src) == CONCAT)
	{
	  unsigned int slen = GET_MODE_SIZE (GET_MODE (src));
	  unsigned int slen0 = GET_MODE_SIZE (GET_MODE (XEXP (src, 0)));

	  if ((bytepos == 0 && bytelen == slen0)
	      || (bytepos != 0 && bytepos + bytelen <= slen))
	    {
	      /* The following assumes that the concatenated objects all
		 have the same size.  In this case, a simple calculation
		 can be used to determine the object and the bit field
		 to be extracted.  */
	      tmps[i] = XEXP (src, bytepos / slen0);
	      if (! CONSTANT_P (tmps[i])
		  && (!REG_P (tmps[i]) || GET_MODE (tmps[i]) != mode))
		tmps[i] = extract_bit_field (tmps[i], bytelen * BITS_PER_UNIT,
					     (bytepos % slen0) * BITS_PER_UNIT,
					     1, false, NULL_RTX, mode, mode);
	    }
	  else
	    {
	      rtx mem;

	      gcc_assert (!bytepos);
	      mem = assign_stack_temp (GET_MODE (src), slen, 0);
	      emit_move_insn (mem, src);
	      tmps[i] = extract_bit_field (mem, bytelen * BITS_PER_UNIT,
					   0, 1, false, NULL_RTX, mode, mode);
	    }
	}
      /* FIXME: A SIMD parallel will eventually lead to a subreg of a
	 SIMD register, which is currently broken.  While we get GCC
	 to emit proper RTL for these cases, let's dump to memory.  */
      else if (VECTOR_MODE_P (GET_MODE (dst))
	       && REG_P (src))
	{
	  int slen = GET_MODE_SIZE (GET_MODE (src));
	  rtx mem;

	  mem = assign_stack_temp (GET_MODE (src), slen, 0);
	  emit_move_insn (mem, src);
	  tmps[i] = adjust_address (mem, mode, (int) bytepos);
	}
      else if (CONSTANT_P (src) && GET_MODE (dst) != BLKmode
               && XVECLEN (dst, 0) > 1)
        tmps[i] = simplify_gen_subreg (mode, src, GET_MODE(dst), bytepos);
      else if (CONSTANT_P (src))
	{
	  HOST_WIDE_INT len = (HOST_WIDE_INT) bytelen;

	  if (len == ssize)
	    tmps[i] = src;
	  else
	    {
	      rtx first, second;

	      gcc_assert (2 * len == ssize);
	      split_double (src, &first, &second);
	      if (i)
		tmps[i] = second;
	      else
		tmps[i] = first;
	    }
	}
      else if (REG_P (src) && GET_MODE (src) == mode)
	tmps[i] = src;
      else
	tmps[i] = extract_bit_field (src, bytelen * BITS_PER_UNIT,
				     bytepos * BITS_PER_UNIT, 1, false, NULL_RTX,
				     mode, mode);

      if (shift)
	tmps[i] = expand_shift (LSHIFT_EXPR, mode, tmps[i],
				build_int_cst (NULL_TREE, shift), tmps[i], 0);
    }
}

/* Emit code to move a block SRC of type TYPE to a block DST,
   where DST is non-consecutive registers represented by a PARALLEL.
   SSIZE represents the total size of block ORIG_SRC in bytes, or -1
   if not known.  */

void
emit_group_load (rtx dst, rtx src, tree type, int ssize)
{
  rtx *tmps;
  int i;

  tmps = XALLOCAVEC (rtx, XVECLEN (dst, 0));
  emit_group_load_1 (tmps, dst, src, type, ssize);

  /* Copy the extracted pieces into the proper (probable) hard regs.  */
  for (i = 0; i < XVECLEN (dst, 0); i++)
    {
      rtx d = XEXP (XVECEXP (dst, 0, i), 0);
      if (d == NULL)
	continue;
      emit_move_insn (d, tmps[i]);
    }
}

/* Similar, but load SRC into new pseudos in a format that looks like
   PARALLEL.  This can later be fed to emit_group_move to get things
   in the right place.  */

rtx
emit_group_load_into_temps (rtx parallel, rtx src, tree type, int ssize)
{
  rtvec vec;
  int i;

  vec = rtvec_alloc (XVECLEN (parallel, 0));
  emit_group_load_1 (&RTVEC_ELT (vec, 0), parallel, src, type, ssize);

  /* Convert the vector to look just like the original PARALLEL, except
     with the computed values.  */
  for (i = 0; i < XVECLEN (parallel, 0); i++)
    {
      rtx e = XVECEXP (parallel, 0, i);
      rtx d = XEXP (e, 0);

      if (d)
	{
	  d = force_reg (GET_MODE (d), RTVEC_ELT (vec, i));
	  e = alloc_EXPR_LIST (REG_NOTE_KIND (e), d, XEXP (e, 1));
	}
      RTVEC_ELT (vec, i) = e;
    }

  return gen_rtx_PARALLEL (GET_MODE (parallel), vec);
}

/* Emit code to move a block SRC to block DST, where SRC and DST are
   non-consecutive groups of registers, each represented by a PARALLEL.  */

void
emit_group_move (rtx dst, rtx src)
{
  int i;

  gcc_assert (GET_CODE (src) == PARALLEL
	      && GET_CODE (dst) == PARALLEL
	      && XVECLEN (src, 0) == XVECLEN (dst, 0));

  /* Skip first entry if NULL.  */
  for (i = XEXP (XVECEXP (src, 0, 0), 0) ? 0 : 1; i < XVECLEN (src, 0); i++)
    emit_move_insn (XEXP (XVECEXP (dst, 0, i), 0),
		    XEXP (XVECEXP (src, 0, i), 0));
}

/* Move a group of registers represented by a PARALLEL into pseudos.  */

rtx
emit_group_move_into_temps (rtx src)
{
  rtvec vec = rtvec_alloc (XVECLEN (src, 0));
  int i;

  for (i = 0; i < XVECLEN (src, 0); i++)
    {
      rtx e = XVECEXP (src, 0, i);
      rtx d = XEXP (e, 0);

      if (d)
	e = alloc_EXPR_LIST (REG_NOTE_KIND (e), copy_to_reg (d), XEXP (e, 1));
      RTVEC_ELT (vec, i) = e;
    }

  return gen_rtx_PARALLEL (GET_MODE (src), vec);
}

/* Emit code to move a block SRC to a block ORIG_DST of type TYPE,
   where SRC is non-consecutive registers represented by a PARALLEL.
   SSIZE represents the total size of block ORIG_DST, or -1 if not
   known.  */

void
emit_group_store (rtx orig_dst, rtx src, tree type ATTRIBUTE_UNUSED, int ssize)
{
  rtx *tmps, dst;
  int start, finish, i;
  enum machine_mode m = GET_MODE (orig_dst);

  gcc_assert (GET_CODE (src) == PARALLEL);

  if (!SCALAR_INT_MODE_P (m)
      && !MEM_P (orig_dst) && GET_CODE (orig_dst) != CONCAT)
    {
      enum machine_mode imode = int_mode_for_mode (GET_MODE (orig_dst));
      if (imode == BLKmode)
        dst = assign_stack_temp (GET_MODE (orig_dst), ssize, 0);
      else
        dst = gen_reg_rtx (imode);
      emit_group_store (dst, src, type, ssize);
      if (imode != BLKmode)
        dst = gen_lowpart (GET_MODE (orig_dst), dst);
      emit_move_insn (orig_dst, dst);
      return;
    }

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (src, 0, 0), 0))
    start = 0;
  else
    start = 1;
  finish = XVECLEN (src, 0);

  tmps = XALLOCAVEC (rtx, finish);

  /* Copy the (probable) hard regs into pseudos.  */
  for (i = start; i < finish; i++)
    {
      rtx reg = XEXP (XVECEXP (src, 0, i), 0);
      if (!REG_P (reg) || REGNO (reg) < FIRST_PSEUDO_REGISTER)
	{
	  tmps[i] = gen_reg_rtx (GET_MODE (reg));
	  emit_move_insn (tmps[i], reg);
	}
      else
	tmps[i] = reg;
    }

  /* If we won't be storing directly into memory, protect the real destination
     from strange tricks we might play.  */
  dst = orig_dst;
  if (GET_CODE (dst) == PARALLEL)
    {
      rtx temp;

      /* We can get a PARALLEL dst if there is a conditional expression in
	 a return statement.  In that case, the dst and src are the same,
	 so no action is necessary.  */
      if (rtx_equal_p (dst, src))
	return;

      /* It is unclear if we can ever reach here, but we may as well handle
	 it.  Allocate a temporary, and split this into a store/load to/from
	 the temporary.  */

      temp = assign_stack_temp (GET_MODE (dst), ssize, 0);
      emit_group_store (temp, src, type, ssize);
      emit_group_load (dst, temp, type, ssize);
      return;
    }
  else if (!MEM_P (dst) && GET_CODE (dst) != CONCAT)
    {
      enum machine_mode outer = GET_MODE (dst);
      enum machine_mode inner;
      HOST_WIDE_INT bytepos;
      bool done = false;
      rtx temp;

      if (!REG_P (dst) || REGNO (dst) < FIRST_PSEUDO_REGISTER)
	dst = gen_reg_rtx (outer);

      /* Make life a bit easier for combine.  */
      /* If the first element of the vector is the low part
	 of the destination mode, use a paradoxical subreg to
	 initialize the destination.  */
      if (start < finish)
	{
	  inner = GET_MODE (tmps[start]);
	  bytepos = subreg_lowpart_offset (inner, outer);
	  if (INTVAL (XEXP (XVECEXP (src, 0, start), 1)) == bytepos)
	    {
	      temp = simplify_gen_subreg (outer, tmps[start],
					  inner, 0);
	      if (temp)
		{
		  emit_move_insn (dst, temp);
		  done = true;
		  start++;
		}
	    }
	}

      /* If the first element wasn't the low part, try the last.  */
      if (!done
	  && start < finish - 1)
	{
	  inner = GET_MODE (tmps[finish - 1]);
	  bytepos = subreg_lowpart_offset (inner, outer);
	  if (INTVAL (XEXP (XVECEXP (src, 0, finish - 1), 1)) == bytepos)
	    {
	      temp = simplify_gen_subreg (outer, tmps[finish - 1],
					  inner, 0);
	      if (temp)
		{
		  emit_move_insn (dst, temp);
		  done = true;
		  finish--;
		}
	    }
	}

      /* Otherwise, simply initialize the result to zero.  */
      if (!done)
        emit_move_insn (dst, CONST0_RTX (outer));
    }

  /* Process the pieces.  */
  for (i = start; i < finish; i++)
    {
      HOST_WIDE_INT bytepos = INTVAL (XEXP (XVECEXP (src, 0, i), 1));
      enum machine_mode mode = GET_MODE (tmps[i]);
      unsigned int bytelen = GET_MODE_SIZE (mode);
      unsigned int adj_bytelen = bytelen;
      rtx dest = dst;

      /* Handle trailing fragments that run over the size of the struct.  */
      if (ssize >= 0 && bytepos + (HOST_WIDE_INT) bytelen > ssize)
	adj_bytelen = ssize - bytepos;

      if (GET_CODE (dst) == CONCAT)
	{
	  if (bytepos + adj_bytelen
	      <= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0))))
	    dest = XEXP (dst, 0);
	  else if (bytepos >= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0))))
	    {
	      bytepos -= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0)));
	      dest = XEXP (dst, 1);
	    }
	  else
	    {
	      enum machine_mode dest_mode = GET_MODE (dest);
	      enum machine_mode tmp_mode = GET_MODE (tmps[i]);

	      gcc_assert (bytepos == 0 && XVECLEN (src, 0));

	      if (GET_MODE_ALIGNMENT (dest_mode)
		  >= GET_MODE_ALIGNMENT (tmp_mode))
		{
		  dest = assign_stack_temp (dest_mode,
					    GET_MODE_SIZE (dest_mode),
					    0);
		  emit_move_insn (adjust_address (dest,
						  tmp_mode,
						  bytepos),
				  tmps[i]);
		  dst = dest;
		}
	      else
		{
		  dest = assign_stack_temp (tmp_mode,
					    GET_MODE_SIZE (tmp_mode),
					    0);
		  emit_move_insn (dest, tmps[i]);
		  dst = adjust_address (dest, dest_mode, bytepos);
		}
	      break;
	    }
	}

      if (ssize >= 0 && bytepos + (HOST_WIDE_INT) bytelen > ssize)
	{
	  /* store_bit_field always takes its value from the lsb.
	     Move the fragment to the lsb if it's not already there.  */
	  if (
#ifdef BLOCK_REG_PADDING
	      BLOCK_REG_PADDING (GET_MODE (orig_dst), type, i == start)
	      == (BYTES_BIG_ENDIAN ? upward : downward)
#else
	      BYTES_BIG_ENDIAN
#endif
	      )
	    {
	      int shift = (bytelen - (ssize - bytepos)) * BITS_PER_UNIT;
	      tmps[i] = expand_shift (RSHIFT_EXPR, mode, tmps[i],
				      build_int_cst (NULL_TREE, shift),
				      tmps[i], 0);
	    }
	  bytelen = adj_bytelen;
	}

      /* Optimize the access just a bit.  */
      if (MEM_P (dest)
	  && (! SLOW_UNALIGNED_ACCESS (mode, MEM_ALIGN (dest))
	      || MEM_ALIGN (dest) >= GET_MODE_ALIGNMENT (mode))
	  && bytepos * BITS_PER_UNIT % GET_MODE_ALIGNMENT (mode) == 0
	  && bytelen == GET_MODE_SIZE (mode))
	emit_move_insn (adjust_address (dest, mode, bytepos), tmps[i]);
      else
	store_bit_field (dest, bytelen * BITS_PER_UNIT, bytepos * BITS_PER_UNIT,
			 mode, tmps[i]);
    }

  /* Copy from the pseudo into the (probable) hard reg.  */
  if (orig_dst != dst)
    emit_move_insn (orig_dst, dst);
}

/* Generate code to copy a BLKmode object of TYPE out of a
   set of registers starting with SRCREG into TGTBLK.  If TGTBLK
   is null, a stack temporary is created.  TGTBLK is returned.

   The purpose of this routine is to handle functions that return
   BLKmode structures in registers.  Some machines (the PA for example)
   want to return all small structures in registers regardless of the
   structure's alignment.  */

rtx
copy_blkmode_from_reg (rtx tgtblk, rtx srcreg, tree type)
{
  unsigned HOST_WIDE_INT bytes = int_size_in_bytes (type);
  rtx src = NULL, dst = NULL;
  unsigned HOST_WIDE_INT bitsize = MIN (TYPE_ALIGN (type), BITS_PER_WORD);
  unsigned HOST_WIDE_INT bitpos, xbitpos, padding_correction = 0;
  enum machine_mode copy_mode;

  if (tgtblk == 0)
    {
      tgtblk = assign_temp (build_qualified_type (type,
						  (TYPE_QUALS (type)
						   | TYPE_QUAL_CONST)),
			    0, 1, 1);
      preserve_temp_slots (tgtblk);
    }

  /* This code assumes srcreg is at least a full word.  If it isn't, copy it
     into a new pseudo which is a full word.  */

  if (GET_MODE (srcreg) != BLKmode
      && GET_MODE_SIZE (GET_MODE (srcreg)) < UNITS_PER_WORD)
    srcreg = convert_to_mode (word_mode, srcreg, TYPE_UNSIGNED (type));

  /* If the structure doesn't take up a whole number of words, see whether
     SRCREG is padded on the left or on the right.  If it's on the left,
     set PADDING_CORRECTION to the number of bits to skip.

     In most ABIs, the structure will be returned at the least end of
     the register, which translates to right padding on little-endian
     targets and left padding on big-endian targets.  The opposite
     holds if the structure is returned at the most significant
     end of the register.  */
  if (bytes % UNITS_PER_WORD != 0
      && (targetm.calls.return_in_msb (type)
	  ? !BYTES_BIG_ENDIAN
	  : BYTES_BIG_ENDIAN))
    padding_correction
      = (BITS_PER_WORD - ((bytes % UNITS_PER_WORD) * BITS_PER_UNIT));

  /* Copy the structure BITSIZE bits at a time.  If the target lives in
     memory, take care of not reading/writing past its end by selecting
     a copy mode suited to BITSIZE.  This should always be possible given
     how it is computed.

     We could probably emit more efficient code for machines which do not use
     strict alignment, but it doesn't seem worth the effort at the current
     time.  */

  copy_mode = word_mode;
  if (MEM_P (tgtblk))
    {
      enum machine_mode mem_mode = mode_for_size (bitsize, MODE_INT, 1);
      if (mem_mode != BLKmode)
	copy_mode = mem_mode;
    }

  for (bitpos = 0, xbitpos = padding_correction;
       bitpos < bytes * BITS_PER_UNIT;
       bitpos += bitsize, xbitpos += bitsize)
    {
      /* We need a new source operand each time xbitpos is on a
	 word boundary and when xbitpos == padding_correction
	 (the first time through).  */
      if (xbitpos % BITS_PER_WORD == 0
	  || xbitpos == padding_correction)
	src = operand_subword_force (srcreg, xbitpos / BITS_PER_WORD,
				     GET_MODE (srcreg));

      /* We need a new destination operand each time bitpos is on
	 a word boundary.  */
      if (bitpos % BITS_PER_WORD == 0)
	dst = operand_subword (tgtblk, bitpos / BITS_PER_WORD, 1, BLKmode);

      /* Use xbitpos for the source extraction (right justified) and
	 bitpos for the destination store (left justified).  */
      store_bit_field (dst, bitsize, bitpos % BITS_PER_WORD, copy_mode,
		       extract_bit_field (src, bitsize,
					  xbitpos % BITS_PER_WORD, 1, false,
					  NULL_RTX, copy_mode, copy_mode));
    }

  return tgtblk;
}

/* Add a USE expression for REG to the (possibly empty) list pointed
   to by CALL_FUSAGE.  REG must denote a hard register.  */

void
use_reg (rtx *call_fusage, rtx reg)
{
  gcc_assert (REG_P (reg) && REGNO (reg) < FIRST_PSEUDO_REGISTER);

  *call_fusage
    = gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_USE (VOIDmode, reg), *call_fusage);
}

/* Add USE expressions to *CALL_FUSAGE for each of NREGS consecutive regs,
   starting at REGNO.  All of these registers must be hard registers.  */

void
use_regs (rtx *call_fusage, int regno, int nregs)
{
  int i;

  gcc_assert (regno + nregs <= FIRST_PSEUDO_REGISTER);

  for (i = 0; i < nregs; i++)
    use_reg (call_fusage, regno_reg_rtx[regno + i]);
}

/* Add USE expressions to *CALL_FUSAGE for each REG contained in the
   PARALLEL REGS.  This is for calls that pass values in multiple
   non-contiguous locations.  The Irix 6 ABI has examples of this.  */

void
use_group_regs (rtx *call_fusage, rtx regs)
{
  int i;

  for (i = 0; i < XVECLEN (regs, 0); i++)
    {
      rtx reg = XEXP (XVECEXP (regs, 0, i), 0);

      /* A NULL entry means the parameter goes both on the stack and in
	 registers.  This can also be a MEM for targets that pass values
	 partially on the stack and partially in registers.  */
      if (reg != 0 && REG_P (reg))
	use_reg (call_fusage, reg);
    }
}

/* Return the defining gimple statement for SSA_NAME NAME if it is an
   assigment and the code of the expresion on the RHS is CODE.  Return
   NULL otherwise.  */

static gimple
get_def_for_expr (tree name, enum tree_code code)
{
  gimple def_stmt;

  if (TREE_CODE (name) != SSA_NAME)
    return NULL;

  def_stmt = get_gimple_for_ssa_name (name);
  if (!def_stmt
      || gimple_assign_rhs_code (def_stmt) != code)
    return NULL;

  return def_stmt;
}


/* Determine whether the LEN bytes generated by CONSTFUN can be
   stored to memory using several move instructions.  CONSTFUNDATA is
   a pointer which will be passed as argument in every CONSTFUN call.
   ALIGN is maximum alignment we can assume.  MEMSETP is true if this is
   a memset operation and false if it's a copy of a constant string.
   Return nonzero if a call to store_by_pieces should succeed.  */

int
can_store_by_pieces (unsigned HOST_WIDE_INT len,
		     rtx (*constfun) (void *, HOST_WIDE_INT, enum machine_mode),
		     void *constfundata, unsigned int align, bool memsetp)
{
  unsigned HOST_WIDE_INT l;
  unsigned int max_size;
  HOST_WIDE_INT offset = 0;
  enum machine_mode mode;
  enum insn_code icode;
  int reverse;
  /* cst is set but not used if LEGITIMATE_CONSTANT doesn't use it.  */
  rtx cst ATTRIBUTE_UNUSED;

  if (len == 0)
    return 1;

  if (! (memsetp
	 ? SET_BY_PIECES_P (len, align)
	 : STORE_BY_PIECES_P (len, align)))
    return 0;

  align = alignment_for_piecewise_move (STORE_MAX_PIECES, align);

  /* We would first store what we can in the largest integer mode, then go to
     successively smaller modes.  */

  for (reverse = 0;
       reverse <= (HAVE_PRE_DECREMENT || HAVE_POST_DECREMENT);
       reverse++)
    {
      l = len;
      max_size = STORE_MAX_PIECES + 1;
      while (max_size > 1)
	{
	  mode = widest_int_mode_for_size (max_size);

	  if (mode == VOIDmode)
	    break;

	  icode = optab_handler (mov_optab, mode);
	  if (icode != CODE_FOR_nothing
	      && align >= GET_MODE_ALIGNMENT (mode))
	    {
	      unsigned int size = GET_MODE_SIZE (mode);

	      while (l >= size)
		{
		  if (reverse)
		    offset -= size;

		  cst = (*constfun) (constfundata, offset, mode);
		  if (!LEGITIMATE_CONSTANT_P (cst))
		    return 0;

		  if (!reverse)
		    offset += size;

		  l -= size;
		}
	    }

	  max_size = GET_MODE_SIZE (mode);
	}

      /* The code above should have handled everything.  */
      gcc_assert (!l);
    }

  return 1;
}

/* Generate several move instructions to store LEN bytes generated by
   CONSTFUN to block TO.  (A MEM rtx with BLKmode).  CONSTFUNDATA is a
   pointer which will be passed as argument in every CONSTFUN call.
   ALIGN is maximum alignment we can assume.  MEMSETP is true if this is
   a memset operation and false if it's a copy of a constant string.
   If ENDP is 0 return to, if ENDP is 1 return memory at the end ala
   mempcpy, and if ENDP is 2 return memory the end minus one byte ala
   stpcpy.  */

rtx
store_by_pieces (rtx to, unsigned HOST_WIDE_INT len,
		 rtx (*constfun) (void *, HOST_WIDE_INT, enum machine_mode),
		 void *constfundata, unsigned int align, bool memsetp, int endp)
{
  enum machine_mode to_addr_mode
    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (to));
  struct store_by_pieces_d data;

  if (len == 0)
    {
      gcc_assert (endp != 2);
      return to;
    }

  gcc_assert (memsetp
	      ? SET_BY_PIECES_P (len, align)
	      : STORE_BY_PIECES_P (len, align));
  data.constfun = constfun;
  data.constfundata = constfundata;
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
  if (endp)
    {
      rtx to1;

      gcc_assert (!data.reverse);
      if (data.autinc_to)
	{
	  if (endp == 2)
	    {
	      if (HAVE_POST_INCREMENT && data.explicit_inc_to > 0)
		emit_insn (gen_add2_insn (data.to_addr, constm1_rtx));
	      else
		data.to_addr = copy_to_mode_reg (to_addr_mode,
						 plus_constant (data.to_addr,
								-1));
	    }
	  to1 = adjust_automodify_address (data.to, QImode, data.to_addr,
					   data.offset);
	}
      else
	{
	  if (endp == 2)
	    --data.offset;
	  to1 = adjust_address (data.to, QImode, data.offset);
	}
      return to1;
    }
  else
    return data.to;
}

/* Generate several move instructions to clear LEN bytes of block TO.  (A MEM
   rtx with BLKmode).  ALIGN is maximum alignment we can assume.  */

static void
clear_by_pieces (rtx to, unsigned HOST_WIDE_INT len, unsigned int align)
{
  struct store_by_pieces_d data;

  if (len == 0)
    return;

  data.constfun = clear_by_pieces_1;
  data.constfundata = NULL;
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
}

/* Callback routine for clear_by_pieces.
   Return const0_rtx unconditionally.  */

static rtx
clear_by_pieces_1 (void *data ATTRIBUTE_UNUSED,
		   HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
		   enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return const0_rtx;
}

/* Subroutine of clear_by_pieces and store_by_pieces.
   Generate several move instructions to store LEN bytes of block TO.  (A MEM
   rtx with BLKmode).  ALIGN is maximum alignment we can assume.  */

static void
store_by_pieces_1 (struct store_by_pieces_d *data ATTRIBUTE_UNUSED,
		   unsigned int align ATTRIBUTE_UNUSED)
{
  enum machine_mode to_addr_mode
    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (data->to));
  rtx to_addr = XEXP (data->to, 0);
  unsigned int max_size = STORE_MAX_PIECES + 1;
  enum insn_code icode;

  data->offset = 0;
  data->to_addr = to_addr;
  data->autinc_to
    = (GET_CODE (to_addr) == PRE_INC || GET_CODE (to_addr) == PRE_DEC
       || GET_CODE (to_addr) == POST_INC || GET_CODE (to_addr) == POST_DEC);

  data->explicit_inc_to = 0;
  data->reverse
    = (GET_CODE (to_addr) == PRE_DEC || GET_CODE (to_addr) == POST_DEC);
  if (data->reverse)
    data->offset = data->len;

  /* If storing requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!data->autinc_to
      && move_by_pieces_ninsns (data->len, align, max_size) > 2)
    {
      /* Determine the main mode we'll be using.
	 MODE might not be used depending on the definitions of the
	 USE_* macros below.  */
      enum machine_mode mode ATTRIBUTE_UNUSED
	= widest_int_mode_for_size (max_size);

      if (USE_STORE_PRE_DECREMENT (mode) && data->reverse && ! data->autinc_to)
	{
	  data->to_addr = copy_to_mode_reg (to_addr_mode,
					    plus_constant (to_addr, data->len));
	  data->autinc_to = 1;
	  data->explicit_inc_to = -1;
	}

      if (USE_STORE_POST_INCREMENT (mode) && ! data->reverse
	  && ! data->autinc_to)
	{
	  data->to_addr = copy_to_mode_reg (to_addr_mode, to_addr);
	  data->autinc_to = 1;
	  data->explicit_inc_to = 1;
	}

      if ( !data->autinc_to && CONSTANT_P (to_addr))
	data->to_addr = copy_to_mode_reg (to_addr_mode, to_addr);
    }

  align = alignment_for_piecewise_move (STORE_MAX_PIECES, align);

  /* First store what we can in the largest integer mode, then go to
     successively smaller modes.  */

  while (max_size > 1)
    {
      enum machine_mode mode = widest_int_mode_for_size (max_size);

      if (mode == VOIDmode)
	break;

      icode = optab_handler (mov_optab, mode);
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	store_by_pieces_2 (GEN_FCN (icode), mode, data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  gcc_assert (!data->len);
}

/* Subroutine of store_by_pieces_1.  Store as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
store_by_pieces_2 (rtx (*genfun) (rtx, ...), enum machine_mode mode,
		   struct store_by_pieces_d *data)
{
  unsigned int size = GET_MODE_SIZE (mode);
  rtx to1, cst;

  while (data->len >= size)
    {
      if (data->reverse)
	data->offset -= size;

      if (data->autinc_to)
	to1 = adjust_automodify_address (data->to, mode, data->to_addr,
					 data->offset);
      else
	to1 = adjust_address (data->to, mode, data->offset);

      if (HAVE_PRE_DECREMENT && data->explicit_inc_to < 0)
	emit_insn (gen_add2_insn (data->to_addr,
				  GEN_INT (-(HOST_WIDE_INT) size)));

      cst = (*data->constfun) (data->constfundata, data->offset, mode);
      emit_insn ((*genfun) (to1, cst));

      if (HAVE_POST_INCREMENT && data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (size)));

      if (! data->reverse)
	data->offset += size;

      data->len -= size;
    }
}

/* Write zeros through the storage of OBJECT.  If OBJECT has BLKmode, SIZE is
   its length in bytes.  */

rtx
clear_storage_hints (rtx object, rtx size, enum block_op_methods method,
		     unsigned int expected_align, HOST_WIDE_INT expected_size)
{
  enum machine_mode mode = GET_MODE (object);
  unsigned int align;

  gcc_assert (method == BLOCK_OP_NORMAL || method == BLOCK_OP_TAILCALL);

  /* If OBJECT is not BLKmode and SIZE is the same size as its mode,
     just move a zero.  Otherwise, do this a piece at a time.  */
  if (mode != BLKmode
      && CONST_INT_P (size)
      && INTVAL (size) == (HOST_WIDE_INT) GET_MODE_SIZE (mode))
    {
      rtx zero = CONST0_RTX (mode);
      if (zero != NULL)
	{
	  emit_move_insn (object, zero);
	  return NULL;
	}

      if (COMPLEX_MODE_P (mode))
	{
	  zero = CONST0_RTX (GET_MODE_INNER (mode));
	  if (zero != NULL)
	    {
	      write_complex_part (object, zero, 0);
	      write_complex_part (object, zero, 1);
	      return NULL;
	    }
	}
    }

  if (size == const0_rtx)
    return NULL;

  align = MEM_ALIGN (object);

  if (CONST_INT_P (size)
      && CLEAR_BY_PIECES_P (INTVAL (size), align))
    clear_by_pieces (object, INTVAL (size), align);
  else if (set_storage_via_setmem (object, size, const0_rtx, align,
				   expected_align, expected_size))
    ;
  else if (ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (object)))
    return set_storage_via_libcall (object, size, const0_rtx,
				    method == BLOCK_OP_TAILCALL);
  else
    gcc_unreachable ();

  return NULL;
}

rtx
clear_storage (rtx object, rtx size, enum block_op_methods method)
{
  return clear_storage_hints (object, size, method, 0, -1);
}


/* A subroutine of clear_storage.  Expand a call to memset.
   Return the return value of memset, 0 otherwise.  */

rtx
set_storage_via_libcall (rtx object, rtx size, rtx val, bool tailcall)
{
  tree call_expr, fn, object_tree, size_tree, val_tree;
  enum machine_mode size_mode;
  rtx retval;

  /* Emit code to copy OBJECT and SIZE into new pseudos.  We can then
     place those into new pseudos into a VAR_DECL and use them later.  */

  object = copy_to_mode_reg (Pmode, XEXP (object, 0));

  size_mode = TYPE_MODE (sizetype);
  size = convert_to_mode (size_mode, size, 1);
  size = copy_to_mode_reg (size_mode, size);

  /* It is incorrect to use the libcall calling conventions to call
     memset in this context.  This could be a user call to memset and
     the user may wish to examine the return value from memset.  For
     targets where libcalls and normal calls have different conventions
     for returning pointers, we could end up generating incorrect code.  */

  object_tree = make_tree (ptr_type_node, object);
  if (!CONST_INT_P (val))
    val = convert_to_mode (TYPE_MODE (integer_type_node), val, 1);
  size_tree = make_tree (sizetype, size);
  val_tree = make_tree (integer_type_node, val);

  fn = clear_storage_libcall_fn (true);
  call_expr = build_call_expr (fn, 3, object_tree, val_tree, size_tree);
  CALL_EXPR_TAILCALL (call_expr) = tailcall;

  retval = expand_normal (call_expr);

  return retval;
}

/* A subroutine of set_storage_via_libcall.  Create the tree node
   for the function we use for block clears.  The first time FOR_CALL
   is true, we call assemble_external.  */

tree block_clear_fn;

void
init_block_clear_fn (const char *asmspec)
{
  if (!block_clear_fn)
    {
      tree fn, args;

      fn = get_identifier ("memset");
      args = build_function_type_list (ptr_type_node, ptr_type_node,
				       integer_type_node, sizetype,
				       NULL_TREE);

      fn = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, fn, args);
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      TREE_NOTHROW (fn) = 1;
      DECL_VISIBILITY (fn) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (fn) = 1;

      block_clear_fn = fn;
    }

  if (asmspec)
    set_user_assembler_name (block_clear_fn, asmspec);
}

static tree
clear_storage_libcall_fn (int for_call)
{
  static bool emitted_extern;

  if (!block_clear_fn)
    init_block_clear_fn (NULL);

  if (for_call && !emitted_extern)
    {
      emitted_extern = true;
      make_decl_rtl (block_clear_fn);
      assemble_external (block_clear_fn);
    }

  return block_clear_fn;
}

/* Expand a setmem pattern; return true if successful.  */

bool
set_storage_via_setmem (rtx object, rtx size, rtx val, unsigned int align,
			unsigned int expected_align, HOST_WIDE_INT expected_size)
{
  /* Try the most limited insn first, because there's no point
     including more than one in the machine description unless
     the more limited one has some advantage.  */

  rtx opalign = GEN_INT (align / BITS_PER_UNIT);
  enum machine_mode mode;

  if (expected_align < align)
    expected_align = align;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      enum insn_code code = direct_optab_handler (setmem_optab, mode);
      insn_operand_predicate_fn pred;

      if (code != CODE_FOR_nothing
	  /* We don't need MODE to be narrower than
	     BITS_PER_HOST_WIDE_INT here because if SIZE is less than
	     the mode mask, as it is returned by the macro, it will
	     definitely be less than the actual mode mask.  */
	  && ((CONST_INT_P (size)
	       && ((unsigned HOST_WIDE_INT) INTVAL (size)
		   <= (GET_MODE_MASK (mode) >> 1)))
	      || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
	  && ((pred = insn_data[(int) code].operand[0].predicate) == 0
	      || (*pred) (object, BLKmode))
	  && ((pred = insn_data[(int) code].operand[3].predicate) == 0
	      || (*pred) (opalign, VOIDmode)))
	{
	  rtx opsize, opchar;
	  enum machine_mode char_mode;
	  rtx last = get_last_insn ();
	  rtx pat;

	  opsize = convert_to_mode (mode, size, 1);
	  pred = insn_data[(int) code].operand[1].predicate;
	  if (pred != 0 && ! (*pred) (opsize, mode))
	    opsize = copy_to_mode_reg (mode, opsize);

	  opchar = val;
	  char_mode = insn_data[(int) code].operand[2].mode;
	  if (char_mode != VOIDmode)
	    {
	      opchar = convert_to_mode (char_mode, opchar, 1);
	      pred = insn_data[(int) code].operand[2].predicate;
	      if (pred != 0 && ! (*pred) (opchar, char_mode))
		opchar = copy_to_mode_reg (char_mode, opchar);
	    }

	  if (insn_data[(int) code].n_operands == 4)
	    pat = GEN_FCN ((int) code) (object, opsize, opchar, opalign);
	  else
	    pat = GEN_FCN ((int) code) (object, opsize, opchar, opalign,
					GEN_INT (expected_align
						 / BITS_PER_UNIT),
					GEN_INT (expected_size));
	  if (pat)
	    {
	      emit_insn (pat);
	      return true;
	    }
	  else
	    delete_insns_since (last);
	}
    }

  return false;
}


/* Write to one of the components of the complex value CPLX.  Write VAL to
   the real part if IMAG_P is false, and the imaginary part if its true.  */

static void
write_complex_part (rtx cplx, rtx val, bool imag_p)
{
  enum machine_mode cmode;
  enum machine_mode imode;
  unsigned ibitsize;

  if (GET_CODE (cplx) == CONCAT)
    {
      emit_move_insn (XEXP (cplx, imag_p), val);
      return;
    }

  cmode = GET_MODE (cplx);
  imode = GET_MODE_INNER (cmode);
  ibitsize = GET_MODE_BITSIZE (imode);

  /* For MEMs simplify_gen_subreg may generate an invalid new address
     because, e.g., the original address is considered mode-dependent
     by the target, which restricts simplify_subreg from invoking
     adjust_address_nv.  Instead of preparing fallback support for an
     invalid address, we call adjust_address_nv directly.  */
  if (MEM_P (cplx))
    {
      emit_move_insn (adjust_address_nv (cplx, imode,
					 imag_p ? GET_MODE_SIZE (imode) : 0),
		      val);
      return;
    }

  /* If the sub-object is at least word sized, then we know that subregging
     will work.  This special case is important, since store_bit_field
     wants to operate on integer modes, and there's rarely an OImode to
     correspond to TCmode.  */
  if (ibitsize >= BITS_PER_WORD
      /* For hard regs we have exact predicates.  Assume we can split
	 the original object if it spans an even number of hard regs.
	 This special case is important for SCmode on 64-bit platforms
	 where the natural size of floating-point regs is 32-bit.  */
      || (REG_P (cplx)
	  && REGNO (cplx) < FIRST_PSEUDO_REGISTER
	  && hard_regno_nregs[REGNO (cplx)][cmode] % 2 == 0))
    {
      rtx part = simplify_gen_subreg (imode, cplx, cmode,
				      imag_p ? GET_MODE_SIZE (imode) : 0);
      if (part)
        {
	  emit_move_insn (part, val);
	  return;
	}
      else
	/* simplify_gen_subreg may fail for sub-word MEMs.  */
	gcc_assert (MEM_P (cplx) && ibitsize < BITS_PER_WORD);
    }

  store_bit_field (cplx, ibitsize, imag_p ? ibitsize : 0, imode, val);
}

/* Extract one of the components of the complex value CPLX.  Extract the
   real part if IMAG_P is false, and the imaginary part if it's true.  */

static rtx
read_complex_part (rtx cplx, bool imag_p)
{
  enum machine_mode cmode, imode;
  unsigned ibitsize;

  if (GET_CODE (cplx) == CONCAT)
    return XEXP (cplx, imag_p);

  cmode = GET_MODE (cplx);
  imode = GET_MODE_INNER (cmode);
  ibitsize = GET_MODE_BITSIZE (imode);

  /* Special case reads from complex constants that got spilled to memory.  */
  if (MEM_P (cplx) && GET_CODE (XEXP (cplx, 0)) == SYMBOL_REF)
    {
      tree decl = SYMBOL_REF_DECL (XEXP (cplx, 0));
      if (decl && TREE_CODE (decl) == COMPLEX_CST)
	{
	  tree part = imag_p ? TREE_IMAGPART (decl) : TREE_REALPART (decl);
	  if (CONSTANT_CLASS_P (part))
	    return expand_expr (part, NULL_RTX, imode, EXPAND_NORMAL);
	}
    }

  /* For MEMs simplify_gen_subreg may generate an invalid new address
     because, e.g., the original address is considered mode-dependent
     by the target, which restricts simplify_subreg from invoking
     adjust_address_nv.  Instead of preparing fallback support for an
     invalid address, we call adjust_address_nv directly.  */
  if (MEM_P (cplx))
    return adjust_address_nv (cplx, imode,
			      imag_p ? GET_MODE_SIZE (imode) : 0);

  /* If the sub-object is at least word sized, then we know that subregging
     will work.  This special case is important, since extract_bit_field
     wants to operate on integer modes, and there's rarely an OImode to
     correspond to TCmode.  */
  if (ibitsize >= BITS_PER_WORD
      /* For hard regs we have exact predicates.  Assume we can split
	 the original object if it spans an even number of hard regs.
	 This special case is important for SCmode on 64-bit platforms
	 where the natural size of floating-point regs is 32-bit.  */
      || (REG_P (cplx)
	  && REGNO (cplx) < FIRST_PSEUDO_REGISTER
	  && hard_regno_nregs[REGNO (cplx)][cmode] % 2 == 0))
    {
      rtx ret = simplify_gen_subreg (imode, cplx, cmode,
				     imag_p ? GET_MODE_SIZE (imode) : 0);
      if (ret)
        return ret;
      else
	/* simplify_gen_subreg may fail for sub-word MEMs.  */
	gcc_assert (MEM_P (cplx) && ibitsize < BITS_PER_WORD);
    }

  return extract_bit_field (cplx, ibitsize, imag_p ? ibitsize : 0,
			    true, false, NULL_RTX, imode, imode);
}

/* A subroutine of emit_move_insn_1.  Yet another lowpart generator.
   NEW_MODE and OLD_MODE are the same size.  Return NULL if X cannot be
   represented in NEW_MODE.  If FORCE is true, this will never happen, as
   we'll force-create a SUBREG if needed.  */

static rtx
emit_move_change_mode (enum machine_mode new_mode,
		       enum machine_mode old_mode, rtx x, bool force)
{
  rtx ret;

  if (push_operand (x, GET_MODE (x)))
    {
      ret = gen_rtx_MEM (new_mode, XEXP (x, 0));
      MEM_COPY_ATTRIBUTES (ret, x);
    }
  else if (MEM_P (x))
    {
      /* We don't have to worry about changing the address since the
	 size in bytes is supposed to be the same.  */
      if (reload_in_progress)
	{
	  /* Copy the MEM to change the mode and move any
	     substitutions from the old MEM to the new one.  */
	  ret = adjust_address_nv (x, new_mode, 0);
	  copy_replacements (x, ret);
	}
      else
	ret = adjust_address (x, new_mode, 0);
    }
  else
    {
      /* Note that we do want simplify_subreg's behavior of validating
	 that the new mode is ok for a hard register.  If we were to use
	 simplify_gen_subreg, we would create the subreg, but would
	 probably run into the target not being able to implement it.  */
      /* Except, of course, when FORCE is true, when this is exactly what
	 we want.  Which is needed for CCmodes on some targets.  */
      if (force)
	ret = simplify_gen_subreg (new_mode, x, old_mode, 0);
      else
	ret = simplify_subreg (new_mode, x, old_mode, 0);
    }

  return ret;
}

/* A subroutine of emit_move_insn_1.  Generate a move from Y into X using
   an integer mode of the same size as MODE.  Returns the instruction
   emitted, or NULL if such a move could not be generated.  */

static rtx
emit_move_via_integer (enum machine_mode mode, rtx x, rtx y, bool force)
{
  enum machine_mode imode;
  enum insn_code code;

  /* There must exist a mode of the exact size we require.  */
  imode = int_mode_for_mode (mode);
  if (imode == BLKmode)
    return NULL_RTX;

  /* The target must support moves in this mode.  */
  code = optab_handler (mov_optab, imode);
  if (code == CODE_FOR_nothing)
    return NULL_RTX;

  x = emit_move_change_mode (imode, mode, x, force);
  if (x == NULL_RTX)
    return NULL_RTX;
  y = emit_move_change_mode (imode, mode, y, force);
  if (y == NULL_RTX)
    return NULL_RTX;
  return emit_insn (GEN_FCN (code) (x, y));
}

/* A subroutine of emit_move_insn_1.  X is a push_operand in MODE.
   Return an equivalent MEM that does not use an auto-increment.  */

static rtx
emit_move_resolve_push (enum machine_mode mode, rtx x)
{
  enum rtx_code code = GET_CODE (XEXP (x, 0));
  HOST_WIDE_INT adjust;
  rtx temp;

  adjust = GET_MODE_SIZE (mode);
#ifdef PUSH_ROUNDING
  adjust = PUSH_ROUNDING (adjust);
#endif
  if (code == PRE_DEC || code == POST_DEC)
    adjust = -adjust;
  else if (code == PRE_MODIFY || code == POST_MODIFY)
    {
      rtx expr = XEXP (XEXP (x, 0), 1);
      HOST_WIDE_INT val;

      gcc_assert (GET_CODE (expr) == PLUS || GET_CODE (expr) == MINUS);
      gcc_assert (CONST_INT_P (XEXP (expr, 1)));
      val = INTVAL (XEXP (expr, 1));
      if (GET_CODE (expr) == MINUS)
	val = -val;
      gcc_assert (adjust == val || adjust == -val);
      adjust = val;
    }

  /* Do not use anti_adjust_stack, since we don't want to update
     stack_pointer_delta.  */
  temp = expand_simple_binop (Pmode, PLUS, stack_pointer_rtx,
			      GEN_INT (adjust), stack_pointer_rtx,
			      0, OPTAB_LIB_WIDEN);
  if (temp != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, temp);

  switch (code)
    {
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
      temp = stack_pointer_rtx;
      break;
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      temp = plus_constant (stack_pointer_rtx, -adjust);
      break;
    default:
      gcc_unreachable ();
    }

  return replace_equiv_address (x, temp);
}

/* A subroutine of emit_move_complex.  Generate a move from Y into X.
   X is known to satisfy push_operand, and MODE is known to be complex.
   Returns the last instruction emitted.  */

rtx
emit_move_complex_push (enum machine_mode mode, rtx x, rtx y)
{
  enum machine_mode submode = GET_MODE_INNER (mode);
  bool imag_first;

#ifdef PUSH_ROUNDING
  unsigned int submodesize = GET_MODE_SIZE (submode);

  /* In case we output to the stack, but the size is smaller than the
     machine can push exactly, we need to use move instructions.  */
  if (PUSH_ROUNDING (submodesize) != submodesize)
    {
      x = emit_move_resolve_push (mode, x);
      return emit_move_insn (x, y);
    }
#endif

  /* Note that the real part always precedes the imag part in memory
     regardless of machine's endianness.  */
  switch (GET_CODE (XEXP (x, 0)))
    {
    case PRE_DEC:
    case POST_DEC:
      imag_first = true;
      break;
    case PRE_INC:
    case POST_INC:
      imag_first = false;
      break;
    default:
      gcc_unreachable ();
    }

  emit_move_insn (gen_rtx_MEM (submode, XEXP (x, 0)),
		  read_complex_part (y, imag_first));
  return emit_move_insn (gen_rtx_MEM (submode, XEXP (x, 0)),
			 read_complex_part (y, !imag_first));
}

/* A subroutine of emit_move_complex.  Perform the move from Y to X
   via two moves of the parts.  Returns the last instruction emitted.  */

rtx
emit_move_complex_parts (rtx x, rtx y)
{
  /* Show the output dies here.  This is necessary for SUBREGs
     of pseudos since we cannot track their lifetimes correctly;
     hard regs shouldn't appear here except as return values.  */
  if (!reload_completed && !reload_in_progress
      && REG_P (x) && !reg_overlap_mentioned_p (x, y))
    emit_clobber (x);

  write_complex_part (x, read_complex_part (y, false), false);
  write_complex_part (x, read_complex_part (y, true), true);

  return get_last_insn ();
}

/* A subroutine of emit_move_insn_1.  Generate a move from Y into X.
   MODE is known to be complex.  Returns the last instruction emitted.  */

static rtx
emit_move_complex (enum machine_mode mode, rtx x, rtx y)
{
  bool try_int;

  /* Need to take special care for pushes, to maintain proper ordering
     of the data, and possibly extra padding.  */
  if (push_operand (x, mode))
    return emit_move_complex_push (mode, x, y);

  /* See if we can coerce the target into moving both values at once.  */

  /* Move floating point as parts.  */
  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      && optab_handler (mov_optab, GET_MODE_INNER (mode)) != CODE_FOR_nothing)
    try_int = false;
  /* Not possible if the values are inherently not adjacent.  */
  else if (GET_CODE (x) == CONCAT || GET_CODE (y) == CONCAT)
    try_int = false;
  /* Is possible if both are registers (or subregs of registers).  */
  else if (register_operand (x, mode) && register_operand (y, mode))
    try_int = true;
  /* If one of the operands is a memory, and alignment constraints
     are friendly enough, we may be able to do combined memory operations.
     We do not attempt this if Y is a constant because that combination is
     usually better with the by-parts thing below.  */
  else if ((MEM_P (x) ? !CONSTANT_P (y) : MEM_P (y))
	   && (!STRICT_ALIGNMENT
	       || get_mode_alignment (mode) == BIGGEST_ALIGNMENT))
    try_int = true;
  else
    try_int = false;

  if (try_int)
    {
      rtx ret;

      /* For memory to memory moves, optimal behavior can be had with the
	 existing block move logic.  */
      if (MEM_P (x) && MEM_P (y))
	{
	  emit_block_move (x, y, GEN_INT (GET_MODE_SIZE (mode)),
			   BLOCK_OP_NO_LIBCALL);
	  return get_last_insn ();
	}

      ret = emit_move_via_integer (mode, x, y, true);
      if (ret)
	return ret;
    }

  return emit_move_complex_parts (x, y);
}

/* A subroutine of emit_move_insn_1.  Generate a move from Y into X.
   MODE is known to be MODE_CC.  Returns the last instruction emitted.  */

static rtx
emit_move_ccmode (enum machine_mode mode, rtx x, rtx y)
{
  rtx ret;

  /* Assume all MODE_CC modes are equivalent; if we have movcc, use it.  */
  if (mode != CCmode)
    {
      enum insn_code code = optab_handler (mov_optab, CCmode);
      if (code != CODE_FOR_nothing)
	{
	  x = emit_move_change_mode (CCmode, mode, x, true);
	  y = emit_move_change_mode (CCmode, mode, y, true);
	  return emit_insn (GEN_FCN (code) (x, y));
	}
    }

  /* Otherwise, find the MODE_INT mode of the same width.  */
  ret = emit_move_via_integer (mode, x, y, false);
  gcc_assert (ret != NULL);
  return ret;
}

/* Return true if word I of OP lies entirely in the
   undefined bits of a paradoxical subreg.  */

static bool
undefined_operand_subword_p (const_rtx op, int i)
{
  enum machine_mode innermode, innermostmode;
  int offset;
  if (GET_CODE (op) != SUBREG)
    return false;
  innermode = GET_MODE (op);
  innermostmode = GET_MODE (SUBREG_REG (op));
  offset = i * UNITS_PER_WORD + SUBREG_BYTE (op);
  /* The SUBREG_BYTE represents offset, as if the value were stored in
     memory, except for a paradoxical subreg where we define
     SUBREG_BYTE to be 0; undo this exception as in
     simplify_subreg.  */
  if (SUBREG_BYTE (op) == 0
      && GET_MODE_SIZE (innermostmode) < GET_MODE_SIZE (innermode))
    {
      int difference = (GET_MODE_SIZE (innermostmode) - GET_MODE_SIZE (innermode));
      if (WORDS_BIG_ENDIAN)
	offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset += difference % UNITS_PER_WORD;
    }
  if (offset >= GET_MODE_SIZE (innermostmode)
      || offset <= -GET_MODE_SIZE (word_mode))
    return true;
  return false;
}

/* A subroutine of emit_move_insn_1.  Generate a move from Y into X.
   MODE is any multi-word or full-word mode that lacks a move_insn
   pattern.  Note that you will get better code if you define such
   patterns, even if they must turn into multiple assembler instructions.  */

static rtx
emit_move_multi_word (enum machine_mode mode, rtx x, rtx y)
{
  rtx last_insn = 0;
  rtx seq, inner;
  bool need_clobber;
  int i;

  gcc_assert (GET_MODE_SIZE (mode) >= UNITS_PER_WORD);

  /* If X is a push on the stack, do the push now and replace
     X with a reference to the stack pointer.  */
  if (push_operand (x, mode))
    x = emit_move_resolve_push (mode, x);

  /* If we are in reload, see if either operand is a MEM whose address
     is scheduled for replacement.  */
  if (reload_in_progress && MEM_P (x)
      && (inner = find_replacement (&XEXP (x, 0))) != XEXP (x, 0))
    x = replace_equiv_address_nv (x, inner);
  if (reload_in_progress && MEM_P (y)
      && (inner = find_replacement (&XEXP (y, 0))) != XEXP (y, 0))
    y = replace_equiv_address_nv (y, inner);

  start_sequence ();

  need_clobber = false;
  for (i = 0;
       i < (GET_MODE_SIZE (mode) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
       i++)
    {
      rtx xpart = operand_subword (x, i, 1, mode);
      rtx ypart;

      /* Do not generate code for a move if it would come entirely
	 from the undefined bits of a paradoxical subreg.  */
      if (undefined_operand_subword_p (y, i))
	continue;

      ypart = operand_subword (y, i, 1, mode);

      /* If we can't get a part of Y, put Y into memory if it is a
	 constant.  Otherwise, force it into a register.  Then we must
	 be able to get a part of Y.  */
      if (ypart == 0 && CONSTANT_P (y))
	{
	  y = use_anchored_address (force_const_mem (mode, y));
	  ypart = operand_subword (y, i, 1, mode);
	}
      else if (ypart == 0)
	ypart = operand_subword_force (y, i, mode);

      gcc_assert (xpart && ypart);

      need_clobber |= (GET_CODE (xpart) == SUBREG);

      last_insn = emit_move_insn (xpart, ypart);
    }

  seq = get_insns ();
  end_sequence ();

  /* Show the output dies here.  This is necessary for SUBREGs
     of pseudos since we cannot track their lifetimes correctly;
     hard regs shouldn't appear here except as return values.
     We never want to emit such a clobber after reload.  */
  if (x != y
      && ! (reload_in_progress || reload_completed)
      && need_clobber != 0)
    emit_clobber (x);

  emit_insn (seq);

  return last_insn;
}

/* Low level part of emit_move_insn.
   Called just like emit_move_insn, but assumes X and Y
   are basically valid.  */

rtx
emit_move_insn_1 (rtx x, rtx y)
{
  enum machine_mode mode = GET_MODE (x);
  enum insn_code code;

  gcc_assert ((unsigned int) mode < (unsigned int) MAX_MACHINE_MODE);

  code = optab_handler (mov_optab, mode);
  if (code != CODE_FOR_nothing)
    return emit_insn (GEN_FCN (code) (x, y));

  /* Expand complex moves by moving real part and imag part.  */
  if (COMPLEX_MODE_P (mode))
    return emit_move_complex (mode, x, y);

  if (GET_MODE_CLASS (mode) == MODE_DECIMAL_FLOAT
      || ALL_FIXED_POINT_MODE_P (mode))
    {
      rtx result = emit_move_via_integer (mode, x, y, true);

      /* If we can't find an integer mode, use multi words.  */
      if (result)
	return result;
      else
	return emit_move_multi_word (mode, x, y);
    }

  if (GET_MODE_CLASS (mode) == MODE_CC)
    return emit_move_ccmode (mode, x, y);

  /* Try using a move pattern for the corresponding integer mode.  This is
     only safe when simplify_subreg can convert MODE constants into integer
     constants.  At present, it can only do this reliably if the value
     fits within a HOST_WIDE_INT.  */
  if (!CONSTANT_P (y) || GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      rtx ret = emit_move_via_integer (mode, x, y, false);
      if (ret)
	return ret;
    }

  return emit_move_multi_word (mode, x, y);
}

/* Generate code to copy Y into X.
   Both Y and X must have the same mode, except that
   Y can be a constant with VOIDmode.
   This mode cannot be BLKmode; use emit_block_move for that.

   Return the last instruction emitted.  */

rtx
emit_move_insn (rtx x, rtx y)
{
  enum machine_mode mode = GET_MODE (x);
  rtx y_cst = NULL_RTX;
  rtx last_insn, set;

  gcc_assert (mode != BLKmode
	      && (GET_MODE (y) == mode || GET_MODE (y) == VOIDmode));

  if (CONSTANT_P (y))
    {
      if (optimize
	  && SCALAR_FLOAT_MODE_P (GET_MODE (x))
	  && (last_insn = compress_float_constant (x, y)))
	return last_insn;

      y_cst = y;

      if (!LEGITIMATE_CONSTANT_P (y))
	{
	  y = force_const_mem (mode, y);

	  /* If the target's cannot_force_const_mem prevented the spill,
	     assume that the target's move expanders will also take care
	     of the non-legitimate constant.  */
	  if (!y)
	    y = y_cst;
	  else
	    y = use_anchored_address (y);
	}
    }

  /* If X or Y are memory references, verify that their addresses are valid
     for the machine.  */
  if (MEM_P (x)
      && (! memory_address_addr_space_p (GET_MODE (x), XEXP (x, 0),
					 MEM_ADDR_SPACE (x))
	  && ! push_operand (x, GET_MODE (x))))
    x = validize_mem (x);

  if (MEM_P (y)
      && ! memory_address_addr_space_p (GET_MODE (y), XEXP (y, 0),
					MEM_ADDR_SPACE (y)))
    y = validize_mem (y);

  gcc_assert (mode != BLKmode);

  last_insn = emit_move_insn_1 (x, y);

  if (y_cst && REG_P (x)
      && (set = single_set (last_insn)) != NULL_RTX
      && SET_DEST (set) == x
      && ! rtx_equal_p (y_cst, SET_SRC (set)))
    set_unique_reg_note (last_insn, REG_EQUAL, copy_rtx (y_cst));

  return last_insn;
}

/* If Y is representable exactly in a narrower mode, and the target can
   perform the extension directly from constant or memory, then emit the
   move as an extension.  */

static rtx
compress_float_constant (rtx x, rtx y)
{
  enum machine_mode dstmode = GET_MODE (x);
  enum machine_mode orig_srcmode = GET_MODE (y);
  enum machine_mode srcmode;
  REAL_VALUE_TYPE r;
  int oldcost, newcost;
  bool speed = optimize_insn_for_speed_p ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, y);

  if (LEGITIMATE_CONSTANT_P (y))
    oldcost = rtx_cost (y, SET, speed);
  else
    oldcost = rtx_cost (force_const_mem (dstmode, y), SET, speed);

  for (srcmode = GET_CLASS_NARROWEST_MODE (GET_MODE_CLASS (orig_srcmode));
       srcmode != orig_srcmode;
       srcmode = GET_MODE_WIDER_MODE (srcmode))
    {
      enum insn_code ic;
      rtx trunc_y, last_insn;

      /* Skip if the target can't extend this way.  */
      ic = can_extend_p (dstmode, srcmode, 0);
      if (ic == CODE_FOR_nothing)
	continue;

      /* Skip if the narrowed value isn't exact.  */
      if (! exact_real_truncate (srcmode, &r))
	continue;

      trunc_y = CONST_DOUBLE_FROM_REAL_VALUE (r, srcmode);

      if (LEGITIMATE_CONSTANT_P (trunc_y))
	{
	  /* Skip if the target needs extra instructions to perform
	     the extension.  */
	  if (! (*insn_data[ic].operand[1].predicate) (trunc_y, srcmode))
	    continue;
	  /* This is valid, but may not be cheaper than the original. */
	  newcost = rtx_cost (gen_rtx_FLOAT_EXTEND (dstmode, trunc_y), SET, speed);
	  if (oldcost < newcost)
	    continue;
	}
      else if (float_extend_from_mem[dstmode][srcmode])
	{
	  trunc_y = force_const_mem (srcmode, trunc_y);
	  /* This is valid, but may not be cheaper than the original. */
	  newcost = rtx_cost (gen_rtx_FLOAT_EXTEND (dstmode, trunc_y), SET, speed);
	  if (oldcost < newcost)
	    continue;
	  trunc_y = validize_mem (trunc_y);
	}
      else
	continue;

      /* For CSE's benefit, force the compressed constant pool entry
	 into a new pseudo.  This constant may be used in different modes,
	 and if not, combine will put things back together for us.  */
      trunc_y = force_reg (srcmode, trunc_y);
      emit_unop_insn (ic, x, trunc_y, UNKNOWN);
      last_insn = get_last_insn ();

      if (REG_P (x))
	set_unique_reg_note (last_insn, REG_EQUAL, y);

      return last_insn;
    }

  return NULL_RTX;
}

/* Pushing data onto the stack.  */

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.
   The value may be virtual_outgoing_args_rtx.

   EXTRA is the number of bytes of padding to push in addition to SIZE.
   BELOW nonzero means this padding comes at low addresses;
   otherwise, the padding comes at high addresses.  */

rtx
push_block (rtx size, int extra, int below)
{
  rtx temp;

  size = convert_modes (Pmode, ptr_mode, size, 1);
  if (CONSTANT_P (size))
    anti_adjust_stack (plus_constant (size, extra));
  else if (REG_P (size) && extra == 0)
    anti_adjust_stack (size);
  else
    {
      temp = copy_to_mode_reg (Pmode, size);
      if (extra != 0)
	temp = expand_binop (Pmode, add_optab, temp, GEN_INT (extra),
			     temp, 0, OPTAB_LIB_WIDEN);
      anti_adjust_stack (temp);
    }

#ifndef STACK_GROWS_DOWNWARD
  if (0)
#else
  if (1)
#endif
    {
      temp = virtual_outgoing_args_rtx;
      if (extra != 0 && below)
	temp = plus_constant (temp, extra);
    }
  else
    {
      if (CONST_INT_P (size))
	temp = plus_constant (virtual_outgoing_args_rtx,
			      -INTVAL (size) - (below ? 0 : extra));
      else if (extra != 0 && !below)
	temp = gen_rtx_PLUS (Pmode, virtual_outgoing_args_rtx,
			     negate_rtx (Pmode, plus_constant (size, extra)));
      else
	temp = gen_rtx_PLUS (Pmode, virtual_outgoing_args_rtx,
			     negate_rtx (Pmode, size));
    }

  return memory_address (GET_CLASS_NARROWEST_MODE (MODE_INT), temp);
}

#ifdef PUSH_ROUNDING

/* Emit single push insn.  */

static void
emit_single_push_insn (enum machine_mode mode, rtx x, tree type)
{
  rtx dest_addr;
  unsigned rounded_size = PUSH_ROUNDING (GET_MODE_SIZE (mode));
  rtx dest;
  enum insn_code icode;
  insn_operand_predicate_fn pred;

  stack_pointer_delta += PUSH_ROUNDING (GET_MODE_SIZE (mode));
  /* If there is push pattern, use it.  Otherwise try old way of throwing
     MEM representing push operation to move expander.  */
  icode = optab_handler (push_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      if (((pred = insn_data[(int) icode].operand[0].predicate)
	   && !((*pred) (x, mode))))
	x = force_reg (mode, x);
      emit_insn (GEN_FCN (icode) (x));
      return;
    }
  if (GET_MODE_SIZE (mode) == rounded_size)
    dest_addr = gen_rtx_fmt_e (STACK_PUSH_CODE, Pmode, stack_pointer_rtx);
  /* If we are to pad downward, adjust the stack pointer first and
     then store X into the stack location using an offset.  This is
     because emit_move_insn does not know how to pad; it does not have
     access to type.  */
  else if (FUNCTION_ARG_PADDING (mode, type) == downward)
    {
      unsigned padding_size = rounded_size - GET_MODE_SIZE (mode);
      HOST_WIDE_INT offset;

      emit_move_insn (stack_pointer_rtx,
		      expand_binop (Pmode,
#ifdef STACK_GROWS_DOWNWARD
				    sub_optab,
#else
				    add_optab,
#endif
				    stack_pointer_rtx,
				    GEN_INT (rounded_size),
				    NULL_RTX, 0, OPTAB_LIB_WIDEN));

      offset = (HOST_WIDE_INT) padding_size;
#ifdef STACK_GROWS_DOWNWARD
      if (STACK_PUSH_CODE == POST_DEC)
	/* We have already decremented the stack pointer, so get the
	   previous value.  */
	offset += (HOST_WIDE_INT) rounded_size;
#else
      if (STACK_PUSH_CODE == POST_INC)
	/* We have already incremented the stack pointer, so get the
	   previous value.  */
	offset -= (HOST_WIDE_INT) rounded_size;
#endif
      dest_addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (offset));
    }
  else
    {
#ifdef STACK_GROWS_DOWNWARD
      /* ??? This seems wrong if STACK_PUSH_CODE == POST_DEC.  */
      dest_addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				GEN_INT (-(HOST_WIDE_INT) rounded_size));
#else
      /* ??? This seems wrong if STACK_PUSH_CODE == POST_INC.  */
      dest_addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				GEN_INT (rounded_size));
#endif
      dest_addr = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx, dest_addr);
    }

  dest = gen_rtx_MEM (mode, dest_addr);

  if (type != 0)
    {
      set_mem_attributes (dest, type, 1);

      if (flag_optimize_sibling_calls)
	/* Function incoming arguments may overlap with sibling call
	   outgoing arguments and we cannot allow reordering of reads
	   from function arguments with stores to outgoing arguments
	   of sibling calls.  */
	set_mem_alias_set (dest, 0);
    }
  emit_move_insn (dest, x);
}
#endif

/* Generate code to push X onto the stack, assuming it has mode MODE and
   type TYPE.
   MODE is redundant except when X is a CONST_INT (since they don't
   carry mode info).
   SIZE is an rtx for the size of data to be copied (in bytes),
   needed only if X is BLKmode.

   ALIGN (in bits) is maximum alignment we can assume.

   If PARTIAL and REG are both nonzero, then copy that many of the first
   bytes of X into registers starting with REG, and push the rest of X.
   The amount of space pushed is decreased by PARTIAL bytes.
   REG must be a hard register in this case.
   If REG is zero but PARTIAL is not, take any all others actions for an
   argument partially in registers, but do not actually load any
   registers.

   EXTRA is the amount in bytes of extra space to leave next to this arg.
   This is ignored if an argument block has already been allocated.

   On a machine that lacks real push insns, ARGS_ADDR is the address of
   the bottom of the argument block for this call.  We use indexing off there
   to store the arg.  On machines with push insns, ARGS_ADDR is 0 when a
   argument block has not been preallocated.

   ARGS_SO_FAR is the size of args previously pushed for this call.

   REG_PARM_STACK_SPACE is nonzero if functions require stack space
   for arguments passed in registers.  If nonzero, it will be the number
   of bytes required.  */

void
emit_push_insn (rtx x, enum machine_mode mode, tree type, rtx size,
		unsigned int align, int partial, rtx reg, int extra,
		rtx args_addr, rtx args_so_far, int reg_parm_stack_space,
		rtx alignment_pad)
{
  rtx xinner;
  enum direction stack_direction
#ifdef STACK_GROWS_DOWNWARD
    = downward;
#else
    = upward;
#endif

  /* Decide where to pad the argument: `downward' for below,
     `upward' for above, or `none' for don't pad it.
     Default is below for small data on big-endian machines; else above.  */
  enum direction where_pad = FUNCTION_ARG_PADDING (mode, type);

  /* Invert direction if stack is post-decrement.
     FIXME: why?  */
  if (STACK_PUSH_CODE == POST_DEC)
    if (where_pad != none)
      where_pad = (where_pad == downward ? upward : downward);

  xinner = x;

  if (mode == BLKmode
      || (STRICT_ALIGNMENT && align < GET_MODE_ALIGNMENT (mode)))
    {
      /* Copy a block into the stack, entirely or partially.  */

      rtx temp;
      int used;
      int offset;
      int skip;

      offset = partial % (PARM_BOUNDARY / BITS_PER_UNIT);
      used = partial - offset;

      if (mode != BLKmode)
	{
	  /* A value is to be stored in an insufficiently aligned
	     stack slot; copy via a suitably aligned slot if
	     necessary.  */
	  size = GEN_INT (GET_MODE_SIZE (mode));
	  if (!MEM_P (xinner))
	    {
	      temp = assign_temp (type, 0, 1, 1);
	      emit_move_insn (temp, xinner);
	      xinner = temp;
	    }
	}

      gcc_assert (size);

      /* USED is now the # of bytes we need not copy to the stack
	 because registers will take care of them.  */

      if (partial != 0)
	xinner = adjust_address (xinner, BLKmode, used);

      /* If the partial register-part of the arg counts in its stack size,
	 skip the part of stack space corresponding to the registers.
	 Otherwise, start copying to the beginning of the stack space,
	 by setting SKIP to 0.  */
      skip = (reg_parm_stack_space == 0) ? 0 : used;

#ifdef PUSH_ROUNDING
      /* Do it with several push insns if that doesn't take lots of insns
	 and if there is no difficulty with push insns that skip bytes
	 on the stack for alignment purposes.  */
      if (args_addr == 0
	  && PUSH_ARGS
	  && CONST_INT_P (size)
	  && skip == 0
	  && MEM_ALIGN (xinner) >= align
	  && (MOVE_BY_PIECES_P ((unsigned) INTVAL (size) - used, align))
	  /* Here we avoid the case of a structure whose weak alignment
	     forces many pushes of a small amount of data,
	     and such small pushes do rounding that causes trouble.  */
	  && ((! SLOW_UNALIGNED_ACCESS (word_mode, align))
	      || align >= BIGGEST_ALIGNMENT
	      || (PUSH_ROUNDING (align / BITS_PER_UNIT)
		  == (align / BITS_PER_UNIT)))
	  && (HOST_WIDE_INT) PUSH_ROUNDING (INTVAL (size)) == INTVAL (size))
	{
	  /* Push padding now if padding above and stack grows down,
	     or if padding below and stack grows up.
	     But if space already allocated, this has already been done.  */
	  if (extra && args_addr == 0
	      && where_pad != none && where_pad != stack_direction)
	    anti_adjust_stack (GEN_INT (extra));

	  move_by_pieces (NULL, xinner, INTVAL (size) - used, align, 0);
	}
      else
#endif /* PUSH_ROUNDING  */
	{
	  rtx target;

	  /* Otherwise make space on the stack and copy the data
	     to the address of that space.  */

	  /* Deduct words put into registers from the size we must copy.  */
	  if (partial != 0)
	    {
	      if (CONST_INT_P (size))
		size = GEN_INT (INTVAL (size) - used);
	      else
		size = expand_binop (GET_MODE (size), sub_optab, size,
				     GEN_INT (used), NULL_RTX, 0,
				     OPTAB_LIB_WIDEN);
	    }

	  /* Get the address of the stack space.
	     In this case, we do not deal with EXTRA separately.
	     A single stack adjust will do.  */
	  if (! args_addr)
	    {
	      temp = push_block (size, extra, where_pad == downward);
	      extra = 0;
	    }
	  else if (CONST_INT_P (args_so_far))
	    temp = memory_address (BLKmode,
				   plus_constant (args_addr,
						  skip + INTVAL (args_so_far)));
	  else
	    temp = memory_address (BLKmode,
				   plus_constant (gen_rtx_PLUS (Pmode,
								args_addr,
								args_so_far),
						  skip));

	  if (!ACCUMULATE_OUTGOING_ARGS)
	    {
	      /* If the source is referenced relative to the stack pointer,
		 copy it to another register to stabilize it.  We do not need
		 to do this if we know that we won't be changing sp.  */

	      if (reg_mentioned_p (virtual_stack_dynamic_rtx, temp)
		  || reg_mentioned_p (virtual_outgoing_args_rtx, temp))
		temp = copy_to_reg (temp);
	    }

	  target = gen_rtx_MEM (BLKmode, temp);

	  /* We do *not* set_mem_attributes here, because incoming arguments
	     may overlap with sibling call outgoing arguments and we cannot
	     allow reordering of reads from function arguments with stores
	     to outgoing arguments of sibling calls.  We do, however, want
	     to record the alignment of the stack slot.  */
	  /* ALIGN may well be better aligned than TYPE, e.g. due to
	     PARM_BOUNDARY.  Assume the caller isn't lying.  */
	  set_mem_align (target, align);

	  emit_block_move (target, xinner, size, BLOCK_OP_CALL_PARM);
	}
    }
  else if (partial > 0)
    {
      /* Scalar partly in registers.  */

      int size = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      int i;
      int not_stack;
      /* # bytes of start of argument
	 that we must make space for but need not store.  */
      int offset = partial % (PARM_BOUNDARY / BITS_PER_UNIT);
      int args_offset = INTVAL (args_so_far);
      int skip;

      /* Push padding now if padding above and stack grows down,
	 or if padding below and stack grows up.
	 But if space already allocated, this has already been done.  */
      if (extra && args_addr == 0
	  && where_pad != none && where_pad != stack_direction)
	anti_adjust_stack (GEN_INT (extra));

      /* If we make space by pushing it, we might as well push
	 the real data.  Otherwise, we can leave OFFSET nonzero
	 and leave the space uninitialized.  */
      if (args_addr == 0)
	offset = 0;

      /* Now NOT_STACK gets the number of words that we don't need to
	 allocate on the stack.  Convert OFFSET to words too.  */
      not_stack = (partial - offset) / UNITS_PER_WORD;
      offset /= UNITS_PER_WORD;

      /* If the partial register-part of the arg counts in its stack size,
	 skip the part of stack space corresponding to the registers.
	 Otherwise, start copying to the beginning of the stack space,
	 by setting SKIP to 0.  */
      skip = (reg_parm_stack_space == 0) ? 0 : not_stack;

      if (CONSTANT_P (x) && ! LEGITIMATE_CONSTANT_P (x))
	x = validize_mem (force_const_mem (mode, x));

      /* If X is a hard register in a non-integer mode, copy it into a pseudo;
	 SUBREGs of such registers are not allowed.  */
      if ((REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER
	   && GET_MODE_CLASS (GET_MODE (x)) != MODE_INT))
	x = copy_to_reg (x);

      /* Loop over all the words allocated on the stack for this arg.  */
      /* We can do it by words, because any scalar bigger than a word
	 has a size a multiple of a word.  */
#ifndef PUSH_ARGS_REVERSED
      for (i = not_stack; i < size; i++)
#else
      for (i = size - 1; i >= not_stack; i--)
#endif
	if (i >= not_stack + offset)
	  emit_push_insn (operand_subword_force (x, i, mode),
			  word_mode, NULL_TREE, NULL_RTX, align, 0, NULL_RTX,
			  0, args_addr,
			  GEN_INT (args_offset + ((i - not_stack + skip)
						  * UNITS_PER_WORD)),
			  reg_parm_stack_space, alignment_pad);
    }
  else
    {
      rtx addr;
      rtx dest;

      /* Push padding now if padding above and stack grows down,
	 or if padding below and stack grows up.
	 But if space already allocated, this has already been done.  */
      if (extra && args_addr == 0
	  && where_pad != none && where_pad != stack_direction)
	anti_adjust_stack (GEN_INT (extra));

#ifdef PUSH_ROUNDING
      if (args_addr == 0 && PUSH_ARGS)
	emit_single_push_insn (mode, x, type);
      else
#endif
	{
	  if (CONST_INT_P (args_so_far))
	    addr
	      = memory_address (mode,
				plus_constant (args_addr,
					       INTVAL (args_so_far)));
	  else
	    addr = memory_address (mode, gen_rtx_PLUS (Pmode, args_addr,
						       args_so_far));
	  dest = gen_rtx_MEM (mode, addr);

	  /* We do *not* set_mem_attributes here, because incoming arguments
	     may overlap with sibling call outgoing arguments and we cannot
	     allow reordering of reads from function arguments with stores
	     to outgoing arguments of sibling calls.  We do, however, want
	     to record the alignment of the stack slot.  */
	  /* ALIGN may well be better aligned than TYPE, e.g. due to
	     PARM_BOUNDARY.  Assume the caller isn't lying.  */
	  set_mem_align (dest, align);

	  emit_move_insn (dest, x);
	}
    }

  /* If part should go in registers, copy that part
     into the appropriate registers.  Do this now, at the end,
     since mem-to-mem copies above may do function calls.  */
  if (partial > 0 && reg != 0)
    {
      /* Handle calls that pass values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      if (GET_CODE (reg) == PARALLEL)
	emit_group_load (reg, x, type, -1);
      else
	{
	  gcc_assert (partial % UNITS_PER_WORD == 0);
	  move_block_to_reg (REGNO (reg), x, partial / UNITS_PER_WORD, mode);
	}
    }

  if (extra && args_addr == 0 && where_pad == stack_direction)
    anti_adjust_stack (GEN_INT (extra));

  if (alignment_pad && args_addr == 0)
    anti_adjust_stack (alignment_pad);
}

/* Return X if X can be used as a subtarget in a sequence of arithmetic
   operations.  */

static rtx
get_subtarget (rtx x)
{
  return (optimize
          || x == 0
	   /* Only registers can be subtargets.  */
	   || !REG_P (x)
	   /* Don't use hard regs to avoid extending their life.  */
	   || REGNO (x) < FIRST_PSEUDO_REGISTER
	  ? 0 : x);
}

/* A subroutine of expand_assignment.  Optimize FIELD op= VAL, where
   FIELD is a bitfield.  Returns true if the optimization was successful,
   and there's nothing else to do.  */

static bool
optimize_bitfield_assignment_op (unsigned HOST_WIDE_INT bitsize,
				 unsigned HOST_WIDE_INT bitpos,
				 enum machine_mode mode1, rtx str_rtx,
				 tree to, tree src)
{
  enum machine_mode str_mode = GET_MODE (str_rtx);
  unsigned int str_bitsize = GET_MODE_BITSIZE (str_mode);
  tree op0, op1;
  rtx value, result;
  optab binop;

  if (mode1 != VOIDmode
      || bitsize >= BITS_PER_WORD
      || str_bitsize > BITS_PER_WORD
      || TREE_SIDE_EFFECTS (to)
      || TREE_THIS_VOLATILE (to))
    return false;

  STRIP_NOPS (src);
  if (!BINARY_CLASS_P (src)
      || TREE_CODE (TREE_TYPE (src)) != INTEGER_TYPE)
    return false;

  op0 = TREE_OPERAND (src, 0);
  op1 = TREE_OPERAND (src, 1);
  STRIP_NOPS (op0);

  if (!operand_equal_p (to, op0, 0))
    return false;

  if (MEM_P (str_rtx))
    {
      unsigned HOST_WIDE_INT offset1;

      if (str_bitsize == 0 || str_bitsize > BITS_PER_WORD)
	str_mode = word_mode;
      str_mode = get_best_mode (bitsize, bitpos,
				MEM_ALIGN (str_rtx), str_mode, 0);
      if (str_mode == VOIDmode)
	return false;
      str_bitsize = GET_MODE_BITSIZE (str_mode);

      offset1 = bitpos;
      bitpos %= str_bitsize;
      offset1 = (offset1 - bitpos) / BITS_PER_UNIT;
      str_rtx = adjust_address (str_rtx, str_mode, offset1);
    }
  else if (!REG_P (str_rtx) && GET_CODE (str_rtx) != SUBREG)
    return false;

  /* If the bit field covers the whole REG/MEM, store_field
     will likely generate better code.  */
  if (bitsize >= str_bitsize)
    return false;

  /* We can't handle fields split across multiple entities.  */
  if (bitpos + bitsize > str_bitsize)
    return false;

  if (BYTES_BIG_ENDIAN)
    bitpos = str_bitsize - bitpos - bitsize;

  switch (TREE_CODE (src))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      /* For now, just optimize the case of the topmost bitfield
	 where we don't need to do any masking and also
	 1 bit bitfields where xor can be used.
	 We might win by one instruction for the other bitfields
	 too if insv/extv instructions aren't used, so that
	 can be added later.  */
      if (bitpos + bitsize != str_bitsize
	  && (bitsize != 1 || TREE_CODE (op1) != INTEGER_CST))
	break;

      value = expand_expr (op1, NULL_RTX, str_mode, EXPAND_NORMAL);
      value = convert_modes (str_mode,
			     TYPE_MODE (TREE_TYPE (op1)), value,
			     TYPE_UNSIGNED (TREE_TYPE (op1)));

      /* We may be accessing data outside the field, which means
	 we can alias adjacent data.  */
      if (MEM_P (str_rtx))
	{
	  str_rtx = shallow_copy_rtx (str_rtx);
	  set_mem_alias_set (str_rtx, 0);
	  set_mem_expr (str_rtx, 0);
	}

      binop = TREE_CODE (src) == PLUS_EXPR ? add_optab : sub_optab;
      if (bitsize == 1 && bitpos + bitsize != str_bitsize)
	{
	  value = expand_and (str_mode, value, const1_rtx, NULL);
	  binop = xor_optab;
	}
      value = expand_shift (LSHIFT_EXPR, str_mode, value,
			    build_int_cst (NULL_TREE, bitpos),
			    NULL_RTX, 1);
      result = expand_binop (str_mode, binop, str_rtx,
			     value, str_rtx, 1, OPTAB_WIDEN);
      if (result != str_rtx)
	emit_move_insn (str_rtx, result);
      return true;

    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST)
	break;
      value = expand_expr (op1, NULL_RTX, GET_MODE (str_rtx), EXPAND_NORMAL);
      value = convert_modes (GET_MODE (str_rtx),
			     TYPE_MODE (TREE_TYPE (op1)), value,
			     TYPE_UNSIGNED (TREE_TYPE (op1)));

      /* We may be accessing data outside the field, which means
	 we can alias adjacent data.  */
      if (MEM_P (str_rtx))
	{
	  str_rtx = shallow_copy_rtx (str_rtx);
	  set_mem_alias_set (str_rtx, 0);
	  set_mem_expr (str_rtx, 0);
	}

      binop = TREE_CODE (src) == BIT_IOR_EXPR ? ior_optab : xor_optab;
      if (bitpos + bitsize != GET_MODE_BITSIZE (GET_MODE (str_rtx)))
	{
	  rtx mask = GEN_INT (((unsigned HOST_WIDE_INT) 1 << bitsize)
			      - 1);
	  value = expand_and (GET_MODE (str_rtx), value, mask,
			      NULL_RTX);
	}
      value = expand_shift (LSHIFT_EXPR, GET_MODE (str_rtx), value,
			    build_int_cst (NULL_TREE, bitpos),
			    NULL_RTX, 1);
      result = expand_binop (GET_MODE (str_rtx), binop, str_rtx,
			     value, str_rtx, 1, OPTAB_WIDEN);
      if (result != str_rtx)
	emit_move_insn (str_rtx, result);
      return true;

    default:
      break;
    }

  return false;
}


/* Expand an assignment that stores the value of FROM into TO.  If NONTEMPORAL
   is true, try generating a nontemporal store.  */

void
expand_assignment (tree to, tree from, bool nontemporal)
{
  rtx to_rtx = 0;
  rtx result;
  enum machine_mode mode;
  int align, icode;

  /* Don't crash if the lhs of the assignment was erroneous.  */
  if (TREE_CODE (to) == ERROR_MARK)
    {
      result = expand_normal (from);
      return;
    }

  /* Optimize away no-op moves without side-effects.  */
  if (operand_equal_p (to, from, 0))
    return;

  mode = TYPE_MODE (TREE_TYPE (to));
  if ((TREE_CODE (to) == MEM_REF
       || TREE_CODE (to) == TARGET_MEM_REF)
      && mode != BLKmode
      && ((align = MAX (TYPE_ALIGN (TREE_TYPE (to)),
			get_object_alignment (to, BIGGEST_ALIGNMENT)))
	  < (signed) GET_MODE_ALIGNMENT (mode))
      && ((icode = optab_handler (movmisalign_optab, mode))
	  != CODE_FOR_nothing))
    {
      enum machine_mode address_mode, op_mode1;
      rtx insn, reg, op0, mem;

      reg = expand_expr (from, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      reg = force_not_mem (reg);

      if (TREE_CODE (to) == MEM_REF)
	{
	  addr_space_t as
	      = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (to, 1))));
	  tree base = TREE_OPERAND (to, 0);
	  address_mode = targetm.addr_space.address_mode (as);
	  op0 = expand_expr (base, NULL_RTX, VOIDmode, EXPAND_NORMAL);
	  op0 = convert_memory_address_addr_space (address_mode, op0, as);
	  if (!integer_zerop (TREE_OPERAND (to, 1)))
	    {
	      rtx off
		  = immed_double_int_const (mem_ref_offset (to), address_mode);
	      op0 = simplify_gen_binary (PLUS, address_mode, op0, off);
	    }
	  op0 = memory_address_addr_space (mode, op0, as);
	  mem = gen_rtx_MEM (mode, op0);
	  set_mem_attributes (mem, to, 0);
	  set_mem_addr_space (mem, as);
	}
      else if (TREE_CODE (to) == TARGET_MEM_REF)
	{
	  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (to));
	  struct mem_address addr;

	  get_address_description (to, &addr);
	  op0 = addr_for_mem_ref (&addr, as, true);
	  op0 = memory_address_addr_space (mode, op0, as);
	  mem = gen_rtx_MEM (mode, op0);
	  set_mem_attributes (mem, to, 0);
	  set_mem_addr_space (mem, as);
	}
      else
	gcc_unreachable ();
      if (TREE_THIS_VOLATILE (to))
	MEM_VOLATILE_P (mem) = 1;

      op_mode1 = insn_data[icode].operand[1].mode;
      if (! (*insn_data[icode].operand[1].predicate) (reg, op_mode1)
	  && op_mode1 != VOIDmode)
	reg = copy_to_mode_reg (op_mode1, reg);

      insn = GEN_FCN (icode) (mem, reg);
      /* The movmisalign<mode> pattern cannot fail, else the assignment would
         silently be omitted.  */
      gcc_assert (insn != NULL_RTX);
      emit_insn (insn);
      return;
    }

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.
     Assignment of an array element at a constant index, and assignment of
     an array element in an unaligned packed structure field, has the same
     problem.  */
  if (handled_component_p (to)
      /* ???  We only need to handle MEM_REF here if the access is not
         a full access of the base object.  */
      || (TREE_CODE (to) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (to, 0)) == ADDR_EXPR)
      || TREE_CODE (TREE_TYPE (to)) == ARRAY_TYPE)
    {
      enum machine_mode mode1;
      HOST_WIDE_INT bitsize, bitpos;
      tree offset;
      int unsignedp;
      int volatilep = 0;
      tree tem;

      push_temp_slots ();
      tem = get_inner_reference (to, &bitsize, &bitpos, &offset, &mode1,
				 &unsignedp, &volatilep, true);

      /* If we are going to use store_bit_field and extract_bit_field,
	 make sure to_rtx will be safe for multiple use.  */

      to_rtx = expand_normal (tem);

      /* If the bitfield is volatile, we want to access it in the
	 field's mode, not the computed mode.
	 If a MEM has VOIDmode (external with incomplete type),
	 use BLKmode for it instead.  */
      if (MEM_P (to_rtx))
	{
	  if (volatilep && flag_strict_volatile_bitfields > 0)
	    to_rtx = adjust_address (to_rtx, mode1, 0);
	  else if (GET_MODE (to_rtx) == VOIDmode)
	    to_rtx = adjust_address (to_rtx, BLKmode, 0);
	}
 
      if (offset != 0)
	{
	  enum machine_mode address_mode;
	  rtx offset_rtx;

	  if (!MEM_P (to_rtx))
	    {
	      /* We can get constant negative offsets into arrays with broken
		 user code.  Translate this to a trap instead of ICEing.  */
	      gcc_assert (TREE_CODE (offset) == INTEGER_CST);
	      expand_builtin_trap ();
	      to_rtx = gen_rtx_MEM (BLKmode, const0_rtx);
	    }

	  offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, EXPAND_SUM);
	  address_mode
	    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (to_rtx));
	  if (GET_MODE (offset_rtx) != address_mode)
	    offset_rtx = convert_to_mode (address_mode, offset_rtx, 0);

	  /* A constant address in TO_RTX can have VOIDmode, we must not try
	     to call force_reg for that case.  Avoid that case.  */
	  if (MEM_P (to_rtx)
	      && GET_MODE (to_rtx) == BLKmode
	      && GET_MODE (XEXP (to_rtx, 0)) != VOIDmode
	      && bitsize > 0
	      && (bitpos % bitsize) == 0
	      && (bitsize % GET_MODE_ALIGNMENT (mode1)) == 0
	      && MEM_ALIGN (to_rtx) == GET_MODE_ALIGNMENT (mode1))
	    {
	      to_rtx = adjust_address (to_rtx, mode1, bitpos / BITS_PER_UNIT);
	      bitpos = 0;
	    }

	  to_rtx = offset_address (to_rtx, offset_rtx,
				   highest_pow2_factor_for_target (to,
				   				   offset));
	}

      /* No action is needed if the target is not a memory and the field
	 lies completely outside that target.  This can occur if the source
	 code contains an out-of-bounds access to a small array.  */
      if (!MEM_P (to_rtx)
	  && GET_MODE (to_rtx) != BLKmode
	  && (unsigned HOST_WIDE_INT) bitpos
	     >= GET_MODE_BITSIZE (GET_MODE (to_rtx)))
	{
	  expand_normal (from);
	  result = NULL;
	}
      /* Handle expand_expr of a complex value returning a CONCAT.  */
      else if (GET_CODE (to_rtx) == CONCAT)
	{
	  unsigned short mode_bitsize = GET_MODE_BITSIZE (GET_MODE (to_rtx));
	  if (COMPLEX_MODE_P (TYPE_MODE (TREE_TYPE (from)))
	      && bitpos == 0
	      && bitsize == mode_bitsize)
	    result = store_expr (from, to_rtx, false, nontemporal);
	  else if (bitsize == mode_bitsize / 2
		   && (bitpos == 0 || bitpos == mode_bitsize / 2))
	    result = store_expr (from, XEXP (to_rtx, bitpos != 0), false,
				 nontemporal);
	  else if (bitpos + bitsize <= mode_bitsize / 2)
	    result = store_field (XEXP (to_rtx, 0), bitsize, bitpos,
				  mode1, from, TREE_TYPE (tem),
				  get_alias_set (to), nontemporal);
	  else if (bitpos >= mode_bitsize / 2)
	    result = store_field (XEXP (to_rtx, 1), bitsize,
				  bitpos - mode_bitsize / 2, mode1, from,
				  TREE_TYPE (tem), get_alias_set (to),
				  nontemporal);
	  else if (bitpos == 0 && bitsize == mode_bitsize)
	    {
	      rtx from_rtx;
	      result = expand_normal (from);
	      from_rtx = simplify_gen_subreg (GET_MODE (to_rtx), result,
					      TYPE_MODE (TREE_TYPE (from)), 0);
	      emit_move_insn (XEXP (to_rtx, 0),
			      read_complex_part (from_rtx, false));
	      emit_move_insn (XEXP (to_rtx, 1),
			      read_complex_part (from_rtx, true));
	    }
	  else
	    {
	      rtx temp = assign_stack_temp (GET_MODE (to_rtx),
					    GET_MODE_SIZE (GET_MODE (to_rtx)),
					    0);
	      write_complex_part (temp, XEXP (to_rtx, 0), false);
	      write_complex_part (temp, XEXP (to_rtx, 1), true);
	      result = store_field (temp, bitsize, bitpos, mode1, from,
				    TREE_TYPE (tem), get_alias_set (to),
				    nontemporal);
	      emit_move_insn (XEXP (to_rtx, 0), read_complex_part (temp, false));
	      emit_move_insn (XEXP (to_rtx, 1), read_complex_part (temp, true));
	    }
	}
      else
	{
	  if (MEM_P (to_rtx))
	    {
	      /* If the field is at offset zero, we could have been given the
		 DECL_RTX of the parent struct.  Don't munge it.  */
	      to_rtx = shallow_copy_rtx (to_rtx);

	      set_mem_attributes_minus_bitpos (to_rtx, to, 0, bitpos);

	      /* Deal with volatile and readonly fields.  The former is only
		 done for MEM.  Also set MEM_KEEP_ALIAS_SET_P if needed.  */
	      if (volatilep)
		MEM_VOLATILE_P (to_rtx) = 1;
	      if (component_uses_parent_alias_set (to))
		MEM_KEEP_ALIAS_SET_P (to_rtx) = 1;
	    }

	  if (optimize_bitfield_assignment_op (bitsize, bitpos, mode1,
					       to_rtx, to, from))
	    result = NULL;
	  else
	    result = store_field (to_rtx, bitsize, bitpos, mode1, from,
				  TREE_TYPE (tem), get_alias_set (to),
				  nontemporal);
	}

      if (result)
	preserve_temp_slots (result);
      free_temp_slots ();
      pop_temp_slots ();
      return;
    }

  /* If the rhs is a function call and its value is not an aggregate,
     call the function before we start to compute the lhs.
     This is needed for correct code for cases such as
     val = setjmp (buf) on machines where reference to val
     requires loading up part of an address in a separate insn.

     Don't do this if TO is a VAR_DECL or PARM_DECL whose DECL_RTL is REG
     since it might be a promoted variable where the zero- or sign- extension
     needs to be done.  Handling this in the normal way is safe because no
     computation is done before the call.  The same is true for SSA names.  */
  if (TREE_CODE (from) == CALL_EXPR && ! aggregate_value_p (from, from)
      && COMPLETE_TYPE_P (TREE_TYPE (from))
      && TREE_CODE (TYPE_SIZE (TREE_TYPE (from))) == INTEGER_CST
      && ! (((TREE_CODE (to) == VAR_DECL || TREE_CODE (to) == PARM_DECL)
	     && REG_P (DECL_RTL (to)))
	    || TREE_CODE (to) == SSA_NAME))
    {
      rtx value;

      push_temp_slots ();
      value = expand_normal (from);
      if (to_rtx == 0)
	to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_WRITE);

      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      if (GET_CODE (to_rtx) == PARALLEL)
	emit_group_load (to_rtx, value, TREE_TYPE (from),
			 int_size_in_bytes (TREE_TYPE (from)));
      else if (GET_MODE (to_rtx) == BLKmode)
	emit_block_move (to_rtx, value, expr_size (from), BLOCK_OP_NORMAL);
      else
	{
	  if (POINTER_TYPE_P (TREE_TYPE (to)))
	    value = convert_memory_address_addr_space
		      (GET_MODE (to_rtx), value,
		       TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (to))));

	  emit_move_insn (to_rtx, value);
	}
      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return;
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_WRITE);

  /* Don't move directly into a return register.  */
  if (TREE_CODE (to) == RESULT_DECL
      && (REG_P (to_rtx) || GET_CODE (to_rtx) == PARALLEL))
    {
      rtx temp;

      push_temp_slots ();
      temp = expand_expr (from, NULL_RTX, GET_MODE (to_rtx), EXPAND_NORMAL);

      if (GET_CODE (to_rtx) == PARALLEL)
	emit_group_load (to_rtx, temp, TREE_TYPE (from),
			 int_size_in_bytes (TREE_TYPE (from)));
      else
	emit_move_insn (to_rtx, temp);

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return;
    }

  /* In case we are returning the contents of an object which overlaps
     the place the value is being stored, use a safe function when copying
     a value through a pointer into a structure value return block.  */
  if (TREE_CODE (to) == RESULT_DECL
      && TREE_CODE (from) == INDIRECT_REF
      && ADDR_SPACE_GENERIC_P
	   (TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (from, 0)))))
      && refs_may_alias_p (to, from)
      && cfun->returns_struct
      && !cfun->returns_pcc_struct)
    {
      rtx from_rtx, size;

      push_temp_slots ();
      size = expr_size (from);
      from_rtx = expand_normal (from);

      emit_library_call (memmove_libfunc, LCT_NORMAL,
			 VOIDmode, 3, XEXP (to_rtx, 0), Pmode,
			 XEXP (from_rtx, 0), Pmode,
			 convert_to_mode (TYPE_MODE (sizetype),
					  size, TYPE_UNSIGNED (sizetype)),
			 TYPE_MODE (sizetype));

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return;
    }

  /* Compute FROM and store the value in the rtx we got.  */

  push_temp_slots ();
  result = store_expr (from, to_rtx, 0, nontemporal);
  preserve_temp_slots (result);
  free_temp_slots ();
  pop_temp_slots ();
  return;
}

/* Emits nontemporal store insn that moves FROM to TO.  Returns true if this
   succeeded, false otherwise.  */

bool
emit_storent_insn (rtx to, rtx from)
{
  enum machine_mode mode = GET_MODE (to), imode;
  enum insn_code code = optab_handler (storent_optab, mode);
  rtx pattern;

  if (code == CODE_FOR_nothing)
    return false;

  imode = insn_data[code].operand[0].mode;
  if (!insn_data[code].operand[0].predicate (to, imode))
    return false;

  imode = insn_data[code].operand[1].mode;
  if (!insn_data[code].operand[1].predicate (from, imode))
    {
      from = copy_to_mode_reg (imode, from);
      if (!insn_data[code].operand[1].predicate (from, imode))
	return false;
    }

  pattern = GEN_FCN (code) (to, from);
  if (pattern == NULL_RTX)
    return false;

  emit_insn (pattern);
  return true;
}

/* Generate code for computing expression EXP,
   and storing the value into TARGET.

   If the mode is BLKmode then we may return TARGET itself.
   It turns out that in BLKmode it doesn't cause a problem.
   because C has no operators that could combine two different
   assignments into the same BLKmode object with different values
   with no sequence point.  Will other languages need this to
   be more thorough?

   If CALL_PARAM_P is nonzero, this is a store into a call param on the
   stack, and block moves may need to be treated specially.

   If NONTEMPORAL is true, try using a nontemporal store instruction.  */

rtx
store_expr (tree exp, rtx target, int call_param_p, bool nontemporal)
{
  rtx temp;
  rtx alt_rtl = NULL_RTX;
  location_t loc = EXPR_LOCATION (exp);

  if (VOID_TYPE_P (TREE_TYPE (exp)))
    {
      /* C++ can generate ?: expressions with a throw expression in one
	 branch and an rvalue in the other. Here, we resolve attempts to
	 store the throw expression's nonexistent result.  */
      gcc_assert (!call_param_p);
      expand_expr (exp, const0_rtx, VOIDmode, EXPAND_NORMAL);
      return NULL_RTX;
    }
  if (TREE_CODE (exp) == COMPOUND_EXPR)
    {
      /* Perform first part of compound expression, then assign from second
	 part.  */
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode,
		   call_param_p ? EXPAND_STACK_PARM : EXPAND_NORMAL);
      return store_expr (TREE_OPERAND (exp, 1), target, call_param_p,
			 nontemporal);
    }
  else if (TREE_CODE (exp) == COND_EXPR && GET_MODE (target) == BLKmode)
    {
      /* For conditional expression, get safe form of the target.  Then
	 test the condition, doing the appropriate assignment on either
	 side.  This avoids the creation of unnecessary temporaries.
	 For non-BLKmode, it is more efficient not to do this.  */

      rtx lab1 = gen_label_rtx (), lab2 = gen_label_rtx ();

      do_pending_stack_adjust ();
      NO_DEFER_POP;
      jumpifnot (TREE_OPERAND (exp, 0), lab1, -1);
      store_expr (TREE_OPERAND (exp, 1), target, call_param_p,
		  nontemporal);
      emit_jump_insn (gen_jump (lab2));
      emit_barrier ();
      emit_label (lab1);
      store_expr (TREE_OPERAND (exp, 2), target, call_param_p,
		  nontemporal);
      emit_label (lab2);
      OK_DEFER_POP;

      return NULL_RTX;
    }
  else if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is a scalar in a register that is stored in a wider mode
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    {
      rtx inner_target = 0;

      /* We can do the conversion inside EXP, which will often result
	 in some optimizations.  Do the conversion in two steps: first
	 change the signedness, if needed, then the extend.  But don't
	 do this if the type of EXP is a subtype of something else
	 since then the conversion might involve more than just
	 converting modes.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (exp))
	  && TREE_TYPE (TREE_TYPE (exp)) == 0
	  && GET_MODE_PRECISION (GET_MODE (target))
	     == TYPE_PRECISION (TREE_TYPE (exp)))
	{
	  if (TYPE_UNSIGNED (TREE_TYPE (exp))
	      != SUBREG_PROMOTED_UNSIGNED_P (target))
	    {
	      /* Some types, e.g. Fortran's logical*4, won't have a signed
		 version, so use the mode instead.  */
	      tree ntype
		= (signed_or_unsigned_type_for
		   (SUBREG_PROMOTED_UNSIGNED_P (target), TREE_TYPE (exp)));
	      if (ntype == NULL)
		ntype = lang_hooks.types.type_for_mode
		  (TYPE_MODE (TREE_TYPE (exp)),
		   SUBREG_PROMOTED_UNSIGNED_P (target));

	      exp = fold_convert_loc (loc, ntype, exp);
	    }

	  exp = fold_convert_loc (loc, lang_hooks.types.type_for_mode
				  (GET_MODE (SUBREG_REG (target)),
				   SUBREG_PROMOTED_UNSIGNED_P (target)),
				  exp);

	  inner_target = SUBREG_REG (target);
	}

      temp = expand_expr (exp, inner_target, VOIDmode,
			  call_param_p ? EXPAND_STACK_PARM : EXPAND_NORMAL);

      /* If TEMP is a VOIDmode constant, use convert_modes to make
	 sure that we properly convert it.  */
      if (CONSTANT_P (temp) && GET_MODE (temp) == VOIDmode)
	{
	  temp = convert_modes (GET_MODE (target), TYPE_MODE (TREE_TYPE (exp)),
				temp, SUBREG_PROMOTED_UNSIGNED_P (target));
	  temp = convert_modes (GET_MODE (SUBREG_REG (target)),
			        GET_MODE (target), temp,
			        SUBREG_PROMOTED_UNSIGNED_P (target));
	}

      convert_move (SUBREG_REG (target), temp,
		    SUBREG_PROMOTED_UNSIGNED_P (target));

      return NULL_RTX;
    }
  else if ((TREE_CODE (exp) == STRING_CST
	    || (TREE_CODE (exp) == MEM_REF
		&& TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
		&& TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		   == STRING_CST
		&& integer_zerop (TREE_OPERAND (exp, 1))))
	   && !nontemporal && !call_param_p
	   && MEM_P (target))
    {
      /* Optimize initialization of an array with a STRING_CST.  */
      HOST_WIDE_INT exp_len, str_copy_len;
      rtx dest_mem;
      tree str = TREE_CODE (exp) == STRING_CST
		 ? exp : TREE_OPERAND (TREE_OPERAND (exp, 0), 0);

      exp_len = int_expr_size (exp);
      if (exp_len <= 0)
	goto normal_expr;

      if (TREE_STRING_LENGTH (str) <= 0)
	goto normal_expr;

      str_copy_len = strlen (TREE_STRING_POINTER (str));
      if (str_copy_len < TREE_STRING_LENGTH (str) - 1)
	goto normal_expr;

      str_copy_len = TREE_STRING_LENGTH (str);
      if ((STORE_MAX_PIECES & (STORE_MAX_PIECES - 1)) == 0
	  && TREE_STRING_POINTER (str)[TREE_STRING_LENGTH (str) - 1] == '\0')
	{
	  str_copy_len += STORE_MAX_PIECES - 1;
	  str_copy_len &= ~(STORE_MAX_PIECES - 1);
	}
      str_copy_len = MIN (str_copy_len, exp_len);
      if (!can_store_by_pieces (str_copy_len, builtin_strncpy_read_str,
				CONST_CAST (char *, TREE_STRING_POINTER (str)),
				MEM_ALIGN (target), false))
	goto normal_expr;

      dest_mem = target;

      dest_mem = store_by_pieces (dest_mem,
				  str_copy_len, builtin_strncpy_read_str,
				  CONST_CAST (char *,
					      TREE_STRING_POINTER (str)),
				  MEM_ALIGN (target), false,
				  exp_len > str_copy_len ? 1 : 0);
      if (exp_len > str_copy_len)
	clear_storage (adjust_address (dest_mem, BLKmode, 0),
		       GEN_INT (exp_len - str_copy_len),
		       BLOCK_OP_NORMAL);
      return NULL_RTX;
    }
  else
    {
      rtx tmp_target;

  normal_expr:
      /* If we want to use a nontemporal store, force the value to
	 register first.  */
      tmp_target = nontemporal ? NULL_RTX : target;
      temp = expand_expr_real (exp, tmp_target, GET_MODE (target),
			       (call_param_p
				? EXPAND_STACK_PARM : EXPAND_NORMAL),
			       &alt_rtl);
    }

  /* If TEMP is a VOIDmode constant and the mode of the type of EXP is not
     the same as that of TARGET, adjust the constant.  This is needed, for
     example, in case it is a CONST_DOUBLE and we want only a word-sized
     value.  */
  if (CONSTANT_P (temp) && GET_MODE (temp) == VOIDmode
      && TREE_CODE (exp) != ERROR_MARK
      && GET_MODE (target) != TYPE_MODE (TREE_TYPE (exp)))
    temp = convert_modes (GET_MODE (target), TYPE_MODE (TREE_TYPE (exp)),
			  temp, TYPE_UNSIGNED (TREE_TYPE (exp)));

  /* If value was not generated in the target, store it there.
     Convert the value to TARGET's type first if necessary and emit the
     pending incrementations that have been queued when expanding EXP.
     Note that we cannot emit the whole queue blindly because this will
     effectively disable the POST_INC optimization later.

     If TEMP and TARGET compare equal according to rtx_equal_p, but
     one or both of them are volatile memory refs, we have to distinguish
     two cases:
     - expand_expr has used TARGET.  In this case, we must not generate
       another copy.  This can be detected by TARGET being equal according
       to == .
     - expand_expr has not used TARGET - that means that the source just
       happens to have the same RTX form.  Since temp will have been created
       by expand_expr, it will compare unequal according to == .
       We must generate a copy in this case, to reach the correct number
       of volatile memory references.  */

  if ((! rtx_equal_p (temp, target)
       || (temp != target && (side_effects_p (temp)
			      || side_effects_p (target))))
      && TREE_CODE (exp) != ERROR_MARK
      /* If store_expr stores a DECL whose DECL_RTL(exp) == TARGET,
	 but TARGET is not valid memory reference, TEMP will differ
	 from TARGET although it is really the same location.  */
      && !(alt_rtl
	   && rtx_equal_p (alt_rtl, target)
	   && !side_effects_p (alt_rtl)
	   && !side_effects_p (target))
      /* If there's nothing to copy, don't bother.  Don't call
	 expr_size unless necessary, because some front-ends (C++)
	 expr_size-hook must not be given objects that are not
	 supposed to be bit-copied or bit-initialized.  */
      && expr_size (exp) != const0_rtx)
    {
      if (GET_MODE (temp) != GET_MODE (target)
	  && GET_MODE (temp) != VOIDmode)
	{
	  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (exp));
	  if (GET_MODE (target) == BLKmode
	      && GET_MODE (temp) == BLKmode)
	    emit_block_move (target, temp, expr_size (exp),
			     (call_param_p
			      ? BLOCK_OP_CALL_PARM
			      : BLOCK_OP_NORMAL));
	  else if (GET_MODE (target) == BLKmode)
	    store_bit_field (target, INTVAL (expr_size (exp)) * BITS_PER_UNIT,
			     0, GET_MODE (temp), temp);
	  else
	    convert_move (target, temp, unsignedp);
	}

      else if (GET_MODE (temp) == BLKmode && TREE_CODE (exp) == STRING_CST)
	{
	  /* Handle copying a string constant into an array.  The string
	     constant may be shorter than the array.  So copy just the string's
	     actual length, and clear the rest.  First get the size of the data
	     type of the string, which is actually the size of the target.  */
	  rtx size = expr_size (exp);

	  if (CONST_INT_P (size)
	      && INTVAL (size) < TREE_STRING_LENGTH (exp))
	    emit_block_move (target, temp, size,
			     (call_param_p
			      ? BLOCK_OP_CALL_PARM : BLOCK_OP_NORMAL));
	  else
	    {
	      enum machine_mode pointer_mode
		= targetm.addr_space.pointer_mode (MEM_ADDR_SPACE (target));
	      enum machine_mode address_mode
		= targetm.addr_space.address_mode (MEM_ADDR_SPACE (target));

	      /* Compute the size of the data to copy from the string.  */
	      tree copy_size
		= size_binop_loc (loc, MIN_EXPR,
				  make_tree (sizetype, size),
				  size_int (TREE_STRING_LENGTH (exp)));
	      rtx copy_size_rtx
		= expand_expr (copy_size, NULL_RTX, VOIDmode,
			       (call_param_p
				? EXPAND_STACK_PARM : EXPAND_NORMAL));
	      rtx label = 0;

	      /* Copy that much.  */
	      copy_size_rtx = convert_to_mode (pointer_mode, copy_size_rtx,
					       TYPE_UNSIGNED (sizetype));
	      emit_block_move (target, temp, copy_size_rtx,
			       (call_param_p
				? BLOCK_OP_CALL_PARM : BLOCK_OP_NORMAL));

	      /* Figure out how much is left in TARGET that we have to clear.
		 Do all calculations in pointer_mode.  */
	      if (CONST_INT_P (copy_size_rtx))
		{
		  size = plus_constant (size, -INTVAL (copy_size_rtx));
		  target = adjust_address (target, BLKmode,
					   INTVAL (copy_size_rtx));
		}
	      else
		{
		  size = expand_binop (TYPE_MODE (sizetype), sub_optab, size,
				       copy_size_rtx, NULL_RTX, 0,
				       OPTAB_LIB_WIDEN);

		  if (GET_MODE (copy_size_rtx) != address_mode)
		    copy_size_rtx = convert_to_mode (address_mode,
						     copy_size_rtx,
						     TYPE_UNSIGNED (sizetype));

		  target = offset_address (target, copy_size_rtx,
					   highest_pow2_factor (copy_size));
		  label = gen_label_rtx ();
		  emit_cmp_and_jump_insns (size, const0_rtx, LT, NULL_RTX,
					   GET_MODE (size), 0, label);
		}

	      if (size != const0_rtx)
		clear_storage (target, size, BLOCK_OP_NORMAL);

	      if (label)
		emit_label (label);
	    }
	}
      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      else if (GET_CODE (target) == PARALLEL)
	emit_group_load (target, temp, TREE_TYPE (exp),
			 int_size_in_bytes (TREE_TYPE (exp)));
      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp),
			 (call_param_p
			  ? BLOCK_OP_CALL_PARM : BLOCK_OP_NORMAL));
      else if (nontemporal
	       && emit_storent_insn (target, temp))
	/* If we managed to emit a nontemporal store, there is nothing else to
	   do.  */
	;
      else
	{
	  temp = force_operand (temp, target);
	  if (temp != target)
	    emit_move_insn (target, temp);
	}
    }

  return NULL_RTX;
}

/* Helper for categorize_ctor_elements.  Identical interface.  */

static bool
categorize_ctor_elements_1 (const_tree ctor, HOST_WIDE_INT *p_nz_elts,
			    HOST_WIDE_INT *p_elt_count,
			    bool *p_must_clear)
{
  unsigned HOST_WIDE_INT idx;
  HOST_WIDE_INT nz_elts, elt_count;
  tree value, purpose;

  /* Whether CTOR is a valid constant initializer, in accordance with what
     initializer_constant_valid_p does.  If inferred from the constructor
     elements, true until proven otherwise.  */
  bool const_from_elts_p = constructor_static_from_elts_p (ctor);
  bool const_p = const_from_elts_p ? true : TREE_STATIC (ctor);

  nz_elts = 0;
  elt_count = 0;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), idx, purpose, value)
    {
      HOST_WIDE_INT mult = 1;

      if (TREE_CODE (purpose) == RANGE_EXPR)
	{
	  tree lo_index = TREE_OPERAND (purpose, 0);
	  tree hi_index = TREE_OPERAND (purpose, 1);

	  if (host_integerp (lo_index, 1) && host_integerp (hi_index, 1))
	    mult = (tree_low_cst (hi_index, 1)
		    - tree_low_cst (lo_index, 1) + 1);
	}

      switch (TREE_CODE (value))
	{
	case CONSTRUCTOR:
	  {
	    HOST_WIDE_INT nz = 0, ic = 0;

	    bool const_elt_p
	      = categorize_ctor_elements_1 (value, &nz, &ic, p_must_clear);

	    nz_elts += mult * nz;
 	    elt_count += mult * ic;

	    if (const_from_elts_p && const_p)
	      const_p = const_elt_p;
	  }
	  break;

	case INTEGER_CST:
	case REAL_CST:
	case FIXED_CST:
	  if (!initializer_zerop (value))
	    nz_elts += mult;
	  elt_count += mult;
	  break;

	case STRING_CST:
	  nz_elts += mult * TREE_STRING_LENGTH (value);
	  elt_count += mult * TREE_STRING_LENGTH (value);
	  break;

	case COMPLEX_CST:
	  if (!initializer_zerop (TREE_REALPART (value)))
	    nz_elts += mult;
	  if (!initializer_zerop (TREE_IMAGPART (value)))
	    nz_elts += mult;
	  elt_count += mult;
	  break;

	case VECTOR_CST:
	  {
	    tree v;
	    for (v = TREE_VECTOR_CST_ELTS (value); v; v = TREE_CHAIN (v))
	      {
		if (!initializer_zerop (TREE_VALUE (v)))
		  nz_elts += mult;
		elt_count += mult;
	      }
	  }
	  break;

	default:
	  {
	    HOST_WIDE_INT tc = count_type_elements (TREE_TYPE (value), true);
	    if (tc < 1)
	      tc = 1;
	    nz_elts += mult * tc;
	    elt_count += mult * tc;

	    if (const_from_elts_p && const_p)
	      const_p = initializer_constant_valid_p (value, TREE_TYPE (value))
			!= NULL_TREE;
	  }
	  break;
	}
    }

  if (!*p_must_clear
      && (TREE_CODE (TREE_TYPE (ctor)) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (ctor)) == QUAL_UNION_TYPE))
    {
      tree init_sub_type;
      bool clear_this = true;

      if (!VEC_empty (constructor_elt, CONSTRUCTOR_ELTS (ctor)))
	{
	  /* We don't expect more than one element of the union to be
	     initialized.  Not sure what we should do otherwise... */
          gcc_assert (VEC_length (constructor_elt, CONSTRUCTOR_ELTS (ctor))
		      == 1);

          init_sub_type = TREE_TYPE (VEC_index (constructor_elt,
						CONSTRUCTOR_ELTS (ctor),
						0)->value);

	  /* ??? We could look at each element of the union, and find the
	     largest element.  Which would avoid comparing the size of the
	     initialized element against any tail padding in the union.
	     Doesn't seem worth the effort...  */
	  if (simple_cst_equal (TYPE_SIZE (TREE_TYPE (ctor)),
				TYPE_SIZE (init_sub_type)) == 1)
	    {
	      /* And now we have to find out if the element itself is fully
		 constructed.  E.g. for union { struct { int a, b; } s; } u
		 = { .s = { .a = 1 } }.  */
	      if (elt_count == count_type_elements (init_sub_type, false))
		clear_this = false;
	    }
	}

      *p_must_clear = clear_this;
    }

  *p_nz_elts += nz_elts;
  *p_elt_count += elt_count;

  return const_p;
}

/* Examine CTOR to discover:
   * how many scalar fields are set to nonzero values,
     and place it in *P_NZ_ELTS;
   * how many scalar fields in total are in CTOR,
     and place it in *P_ELT_COUNT.
   * if a type is a union, and the initializer from the constructor
     is not the largest element in the union, then set *p_must_clear.

   Return whether or not CTOR is a valid static constant initializer, the same
   as "initializer_constant_valid_p (CTOR, TREE_TYPE (CTOR)) != 0".  */

bool
categorize_ctor_elements (const_tree ctor, HOST_WIDE_INT *p_nz_elts,
			  HOST_WIDE_INT *p_elt_count,
			  bool *p_must_clear)
{
  *p_nz_elts = 0;
  *p_elt_count = 0;
  *p_must_clear = false;

  return
    categorize_ctor_elements_1 (ctor, p_nz_elts, p_elt_count, p_must_clear);
}

/* Count the number of scalars in TYPE.  Return -1 on overflow or
   variable-sized.  If ALLOW_FLEXARR is true, don't count flexible
   array member at the end of the structure.  */

HOST_WIDE_INT
count_type_elements (const_tree type, bool allow_flexarr)
{
  const HOST_WIDE_INT max = ~((HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT-1));
  switch (TREE_CODE (type))
    {
    case ARRAY_TYPE:
      {
	tree telts = array_type_nelts (type);
	if (telts && host_integerp (telts, 1))
	  {
	    HOST_WIDE_INT n = tree_low_cst (telts, 1) + 1;
	    HOST_WIDE_INT m = count_type_elements (TREE_TYPE (type), false);
	    if (n == 0)
	      return 0;
	    else if (max / n > m)
	      return n * m;
	  }
	return -1;
      }

    case RECORD_TYPE:
      {
	HOST_WIDE_INT n = 0, t;
	tree f;

	for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
	  if (TREE_CODE (f) == FIELD_DECL)
	    {
	      t = count_type_elements (TREE_TYPE (f), false);
	      if (t < 0)
		{
		  /* Check for structures with flexible array member.  */
		  tree tf = TREE_TYPE (f);
		  if (allow_flexarr
		      && DECL_CHAIN (f) == NULL
		      && TREE_CODE (tf) == ARRAY_TYPE
		      && TYPE_DOMAIN (tf)
		      && TYPE_MIN_VALUE (TYPE_DOMAIN (tf))
		      && integer_zerop (TYPE_MIN_VALUE (TYPE_DOMAIN (tf)))
		      && !TYPE_MAX_VALUE (TYPE_DOMAIN (tf))
		      && int_size_in_bytes (type) >= 0)
		    break;

		  return -1;
		}
	      n += t;
	    }

	return n;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      return -1;

    case COMPLEX_TYPE:
      return 2;

    case VECTOR_TYPE:
      return TYPE_VECTOR_SUBPARTS (type);

    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case OFFSET_TYPE:
    case REFERENCE_TYPE:
      return 1;

    case ERROR_MARK:
      return 0;

    case VOID_TYPE:
    case METHOD_TYPE:
    case FUNCTION_TYPE:
    case LANG_TYPE:
    default:
      gcc_unreachable ();
    }
}

/* Return 1 if EXP contains mostly (3/4)  zeros.  */

static int
mostly_zeros_p (const_tree exp)
{
  if (TREE_CODE (exp) == CONSTRUCTOR)

    {
      HOST_WIDE_INT nz_elts, count, elts;
      bool must_clear;

      categorize_ctor_elements (exp, &nz_elts, &count, &must_clear);
      if (must_clear)
	return 1;

      elts = count_type_elements (TREE_TYPE (exp), false);

      return nz_elts < elts / 4;
    }

  return initializer_zerop (exp);
}

/* Return 1 if EXP contains all zeros.  */

static int
all_zeros_p (const_tree exp)
{
  if (TREE_CODE (exp) == CONSTRUCTOR)

    {
      HOST_WIDE_INT nz_elts, count;
      bool must_clear;

      categorize_ctor_elements (exp, &nz_elts, &count, &must_clear);
      return nz_elts == 0;
    }

  return initializer_zerop (exp);
}

/* Helper function for store_constructor.
   TARGET, BITSIZE, BITPOS, MODE, EXP are as for store_field.
   TYPE is the type of the CONSTRUCTOR, not the element type.
   CLEARED is as for store_constructor.
   ALIAS_SET is the alias set to use for any stores.

   This provides a recursive shortcut back to store_constructor when it isn't
   necessary to go through store_field.  This is so that we can pass through
   the cleared field to let store_constructor know that we may not have to
   clear a substructure if the outer structure has already been cleared.  */

static void
store_constructor_field (rtx target, unsigned HOST_WIDE_INT bitsize,
			 HOST_WIDE_INT bitpos, enum machine_mode mode,
			 tree exp, tree type, int cleared,
			 alias_set_type alias_set)
{
  if (TREE_CODE (exp) == CONSTRUCTOR
      /* We can only call store_constructor recursively if the size and
	 bit position are on a byte boundary.  */
      && bitpos % BITS_PER_UNIT == 0
      && (bitsize > 0 && bitsize % BITS_PER_UNIT == 0)
      /* If we have a nonzero bitpos for a register target, then we just
	 let store_field do the bitfield handling.  This is unlikely to
	 generate unnecessary clear instructions anyways.  */
      && (bitpos == 0 || MEM_P (target)))
    {
      if (MEM_P (target))
	target
	  = adjust_address (target,
			    GET_MODE (target) == BLKmode
			    || 0 != (bitpos
				     % GET_MODE_ALIGNMENT (GET_MODE (target)))
			    ? BLKmode : VOIDmode, bitpos / BITS_PER_UNIT);


      /* Update the alias set, if required.  */
      if (MEM_P (target) && ! MEM_KEEP_ALIAS_SET_P (target)
	  && MEM_ALIAS_SET (target) != 0)
	{
	  target = copy_rtx (target);
	  set_mem_alias_set (target, alias_set);
	}

      store_constructor (exp, target, cleared, bitsize / BITS_PER_UNIT);
    }
  else
    store_field (target, bitsize, bitpos, mode, exp, type, alias_set, false);
}

/* Store the value of constructor EXP into the rtx TARGET.
   TARGET is either a REG or a MEM; we know it cannot conflict, since
   safe_from_p has been called.
   CLEARED is true if TARGET is known to have been zero'd.
   SIZE is the number of bytes of TARGET we are allowed to modify: this
   may not be the same as the size of EXP if we are assigning to a field
   which has been packed to exclude padding bits.  */

static void
store_constructor (tree exp, rtx target, int cleared, HOST_WIDE_INT size)
{
  tree type = TREE_TYPE (exp);
#ifdef WORD_REGISTER_OPERATIONS
  HOST_WIDE_INT exp_size = int_size_in_bytes (type);
#endif

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	unsigned HOST_WIDE_INT idx;
	tree field, value;

	/* If size is zero or the target is already cleared, do nothing.  */
	if (size == 0 || cleared)
	  cleared = 1;
	/* We either clear the aggregate or indicate the value is dead.  */
	else if ((TREE_CODE (type) == UNION_TYPE
		  || TREE_CODE (type) == QUAL_UNION_TYPE)
		 && ! CONSTRUCTOR_ELTS (exp))
	  /* If the constructor is empty, clear the union.  */
	  {
	    clear_storage (target, expr_size (exp), BLOCK_OP_NORMAL);
	    cleared = 1;
	  }

	/* If we are building a static constructor into a register,
	   set the initial value as zero so we can fold the value into
	   a constant.  But if more than one register is involved,
	   this probably loses.  */
	else if (REG_P (target) && TREE_STATIC (exp)
		 && GET_MODE_SIZE (GET_MODE (target)) <= UNITS_PER_WORD)
	  {
	    emit_move_insn (target, CONST0_RTX (GET_MODE (target)));
	    cleared = 1;
	  }

        /* If the constructor has fewer fields than the structure or
	   if we are initializing the structure to mostly zeros, clear
	   the whole structure first.  Don't do this if TARGET is a
	   register whose mode size isn't equal to SIZE since
	   clear_storage can't handle this case.  */
	else if (size > 0
		 && (((int)VEC_length (constructor_elt, CONSTRUCTOR_ELTS (exp))
		      != fields_length (type))
		     || mostly_zeros_p (exp))
		 && (!REG_P (target)
		     || ((HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (target))
			 == size)))
	  {
	    clear_storage (target, GEN_INT (size), BLOCK_OP_NORMAL);
	    cleared = 1;
	  }

	if (REG_P (target) && !cleared)
	  emit_clobber (target);

	/* Store each element of the constructor into the
	   corresponding field of TARGET.  */
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (exp), idx, field, value)
	  {
	    enum machine_mode mode;
	    HOST_WIDE_INT bitsize;
	    HOST_WIDE_INT bitpos = 0;
	    tree offset;
	    rtx to_rtx = target;

	    /* Just ignore missing fields.  We cleared the whole
	       structure, above, if any fields are missing.  */
	    if (field == 0)
	      continue;

	    if (cleared && initializer_zerop (value))
	      continue;

	    if (host_integerp (DECL_SIZE (field), 1))
	      bitsize = tree_low_cst (DECL_SIZE (field), 1);
	    else
	      bitsize = -1;

	    mode = DECL_MODE (field);
	    if (DECL_BIT_FIELD (field))
	      mode = VOIDmode;

	    offset = DECL_FIELD_OFFSET (field);
	    if (host_integerp (offset, 0)
		&& host_integerp (bit_position (field), 0))
	      {
		bitpos = int_bit_position (field);
		offset = 0;
	      }
	    else
	      bitpos = tree_low_cst (DECL_FIELD_BIT_OFFSET (field), 0);

	    if (offset)
	      {
	        enum machine_mode address_mode;
		rtx offset_rtx;

		offset
		  = SUBSTITUTE_PLACEHOLDER_IN_EXPR (offset,
						    make_tree (TREE_TYPE (exp),
							       target));

		offset_rtx = expand_normal (offset);
		gcc_assert (MEM_P (to_rtx));

		address_mode
		  = targetm.addr_space.address_mode (MEM_ADDR_SPACE (to_rtx));
		if (GET_MODE (offset_rtx) != address_mode)
		  offset_rtx = convert_to_mode (address_mode, offset_rtx, 0);

		to_rtx = offset_address (to_rtx, offset_rtx,
					 highest_pow2_factor (offset));
	      }

#ifdef WORD_REGISTER_OPERATIONS
	    /* If this initializes a field that is smaller than a
	       word, at the start of a word, try to widen it to a full
	       word.  This special case allows us to output C++ member
	       function initializations in a form that the optimizers
	       can understand.  */
	    if (REG_P (target)
		&& bitsize < BITS_PER_WORD
		&& bitpos % BITS_PER_WORD == 0
		&& GET_MODE_CLASS (mode) == MODE_INT
		&& TREE_CODE (value) == INTEGER_CST
		&& exp_size >= 0
		&& bitpos + BITS_PER_WORD <= exp_size * BITS_PER_UNIT)
	      {
		tree type = TREE_TYPE (value);

		if (TYPE_PRECISION (type) < BITS_PER_WORD)
		  {
		    type = lang_hooks.types.type_for_size
		      (BITS_PER_WORD, TYPE_UNSIGNED (type));
		    value = fold_convert (type, value);
		  }

		if (BYTES_BIG_ENDIAN)
		  value
		   = fold_build2 (LSHIFT_EXPR, type, value,
				   build_int_cst (type,
						  BITS_PER_WORD - bitsize));
		bitsize = BITS_PER_WORD;
		mode = word_mode;
	      }
#endif

	    if (MEM_P (to_rtx) && !MEM_KEEP_ALIAS_SET_P (to_rtx)
		&& DECL_NONADDRESSABLE_P (field))
	      {
		to_rtx = copy_rtx (to_rtx);
		MEM_KEEP_ALIAS_SET_P (to_rtx) = 1;
	      }

	    store_constructor_field (to_rtx, bitsize, bitpos, mode,
				     value, type, cleared,
				     get_alias_set (TREE_TYPE (field)));
	  }
	break;
      }
    case ARRAY_TYPE:
      {
	tree value, index;
	unsigned HOST_WIDE_INT i;
	int need_to_clear;
	tree domain;
	tree elttype = TREE_TYPE (type);
	int const_bounds_p;
	HOST_WIDE_INT minelt = 0;
	HOST_WIDE_INT maxelt = 0;

	domain = TYPE_DOMAIN (type);
	const_bounds_p = (TYPE_MIN_VALUE (domain)
			  && TYPE_MAX_VALUE (domain)
			  && host_integerp (TYPE_MIN_VALUE (domain), 0)
			  && host_integerp (TYPE_MAX_VALUE (domain), 0));

	/* If we have constant bounds for the range of the type, get them.  */
	if (const_bounds_p)
	  {
	    minelt = tree_low_cst (TYPE_MIN_VALUE (domain), 0);
	    maxelt = tree_low_cst (TYPE_MAX_VALUE (domain), 0);
	  }

	/* If the constructor has fewer elements than the array, clear
           the whole array first.  Similarly if this is static
           constructor of a non-BLKmode object.  */
	if (cleared)
	  need_to_clear = 0;
	else if (REG_P (target) && TREE_STATIC (exp))
	  need_to_clear = 1;
	else
	  {
	    unsigned HOST_WIDE_INT idx;
	    tree index, value;
	    HOST_WIDE_INT count = 0, zero_count = 0;
	    need_to_clear = ! const_bounds_p;

	    /* This loop is a more accurate version of the loop in
	       mostly_zeros_p (it handles RANGE_EXPR in an index).  It
	       is also needed to check for missing elements.  */
	    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (exp), idx, index, value)
	      {
		HOST_WIDE_INT this_node_count;

		if (need_to_clear)
		  break;

		if (index != NULL_TREE && TREE_CODE (index) == RANGE_EXPR)
		  {
		    tree lo_index = TREE_OPERAND (index, 0);
		    tree hi_index = TREE_OPERAND (index, 1);

		    if (! host_integerp (lo_index, 1)
			|| ! host_integerp (hi_index, 1))
		      {
			need_to_clear = 1;
			break;
		      }

		    this_node_count = (tree_low_cst (hi_index, 1)
				       - tree_low_cst (lo_index, 1) + 1);
		  }
		else
		  this_node_count = 1;

		count += this_node_count;
		if (mostly_zeros_p (value))
		  zero_count += this_node_count;
	      }

	    /* Clear the entire array first if there are any missing
	       elements, or if the incidence of zero elements is >=
	       75%.  */
	    if (! need_to_clear
		&& (count < maxelt - minelt + 1
		    || 4 * zero_count >= 3 * count))
	      need_to_clear = 1;
	  }

	if (need_to_clear && size > 0)
	  {
	    if (REG_P (target))
	      emit_move_insn (target,  CONST0_RTX (GET_MODE (target)));
	    else
	      clear_storage (target, GEN_INT (size), BLOCK_OP_NORMAL);
	    cleared = 1;
	  }

	if (!cleared && REG_P (target))
	  /* Inform later passes that the old value is dead.  */
	  emit_clobber (target);

	/* Store each element of the constructor into the
	   corresponding element of TARGET, determined by counting the
	   elements.  */
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (exp), i, index, value)
	  {
	    enum machine_mode mode;
	    HOST_WIDE_INT bitsize;
	    HOST_WIDE_INT bitpos;
	    rtx xtarget = target;

	    if (cleared && initializer_zerop (value))
	      continue;

	    mode = TYPE_MODE (elttype);
	    if (mode == BLKmode)
	      bitsize = (host_integerp (TYPE_SIZE (elttype), 1)
			 ? tree_low_cst (TYPE_SIZE (elttype), 1)
			 : -1);
	    else
	      bitsize = GET_MODE_BITSIZE (mode);

	    if (index != NULL_TREE && TREE_CODE (index) == RANGE_EXPR)
	      {
		tree lo_index = TREE_OPERAND (index, 0);
		tree hi_index = TREE_OPERAND (index, 1);
		rtx index_r, pos_rtx;
		HOST_WIDE_INT lo, hi, count;
		tree position;

		/* If the range is constant and "small", unroll the loop.  */
		if (const_bounds_p
		    && host_integerp (lo_index, 0)
		    && host_integerp (hi_index, 0)
		    && (lo = tree_low_cst (lo_index, 0),
			hi = tree_low_cst (hi_index, 0),
			count = hi - lo + 1,
			(!MEM_P (target)
			 || count <= 2
			 || (host_integerp (TYPE_SIZE (elttype), 1)
			     && (tree_low_cst (TYPE_SIZE (elttype), 1) * count
				 <= 40 * 8)))))
		  {
		    lo -= minelt;  hi -= minelt;
		    for (; lo <= hi; lo++)
		      {
			bitpos = lo * tree_low_cst (TYPE_SIZE (elttype), 0);

			if (MEM_P (target)
			    && !MEM_KEEP_ALIAS_SET_P (target)
			    && TREE_CODE (type) == ARRAY_TYPE
			    && TYPE_NONALIASED_COMPONENT (type))
			  {
			    target = copy_rtx (target);
			    MEM_KEEP_ALIAS_SET_P (target) = 1;
			  }

			store_constructor_field
			  (target, bitsize, bitpos, mode, value, type, cleared,
			   get_alias_set (elttype));
		      }
		  }
		else
		  {
		    rtx loop_start = gen_label_rtx ();
		    rtx loop_end = gen_label_rtx ();
		    tree exit_cond;

		    expand_normal (hi_index);

		    index = build_decl (EXPR_LOCATION (exp),
					VAR_DECL, NULL_TREE, domain);
		    index_r = gen_reg_rtx (promote_decl_mode (index, NULL));
		    SET_DECL_RTL (index, index_r);
		    store_expr (lo_index, index_r, 0, false);

		    /* Build the head of the loop.  */
		    do_pending_stack_adjust ();
		    emit_label (loop_start);

		    /* Assign value to element index.  */
		    position =
		      fold_convert (ssizetype,
				    fold_build2 (MINUS_EXPR,
						 TREE_TYPE (index),
						 index,
						 TYPE_MIN_VALUE (domain)));

		    position =
			size_binop (MULT_EXPR, position,
				    fold_convert (ssizetype,
						  TYPE_SIZE_UNIT (elttype)));

		    pos_rtx = expand_normal (position);
		    xtarget = offset_address (target, pos_rtx,
					      highest_pow2_factor (position));
		    xtarget = adjust_address (xtarget, mode, 0);
		    if (TREE_CODE (value) == CONSTRUCTOR)
		      store_constructor (value, xtarget, cleared,
					 bitsize / BITS_PER_UNIT);
		    else
		      store_expr (value, xtarget, 0, false);

		    /* Generate a conditional jump to exit the loop.  */
		    exit_cond = build2 (LT_EXPR, integer_type_node,
					index, hi_index);
		    jumpif (exit_cond, loop_end, -1);

		    /* Update the loop counter, and jump to the head of
		       the loop.  */
		    expand_assignment (index,
				       build2 (PLUS_EXPR, TREE_TYPE (index),
					       index, integer_one_node),
				       false);

		    emit_jump (loop_start);

		    /* Build the end of the loop.  */
		    emit_label (loop_end);
		  }
	      }
	    else if ((index != 0 && ! host_integerp (index, 0))
		     || ! host_integerp (TYPE_SIZE (elttype), 1))
	      {
		tree position;

		if (index == 0)
		  index = ssize_int (1);

		if (minelt)
		  index = fold_convert (ssizetype,
					fold_build2 (MINUS_EXPR,
						     TREE_TYPE (index),
						     index,
						     TYPE_MIN_VALUE (domain)));

		position =
		  size_binop (MULT_EXPR, index,
			      fold_convert (ssizetype,
					    TYPE_SIZE_UNIT (elttype)));
		xtarget = offset_address (target,
					  expand_normal (position),
					  highest_pow2_factor (position));
		xtarget = adjust_address (xtarget, mode, 0);
		store_expr (value, xtarget, 0, false);
	      }
	    else
	      {
		if (index != 0)
		  bitpos = ((tree_low_cst (index, 0) - minelt)
			    * tree_low_cst (TYPE_SIZE (elttype), 1));
		else
		  bitpos = (i * tree_low_cst (TYPE_SIZE (elttype), 1));

		if (MEM_P (target) && !MEM_KEEP_ALIAS_SET_P (target)
		    && TREE_CODE (type) == ARRAY_TYPE
		    && TYPE_NONALIASED_COMPONENT (type))
		  {
		    target = copy_rtx (target);
		    MEM_KEEP_ALIAS_SET_P (target) = 1;
		  }
		store_constructor_field (target, bitsize, bitpos, mode, value,
					 type, cleared, get_alias_set (elttype));
	      }
	  }
	break;
      }

    case VECTOR_TYPE:
      {
	unsigned HOST_WIDE_INT idx;
	constructor_elt *ce;
	int i;
	int need_to_clear;
	int icode = 0;
	tree elttype = TREE_TYPE (type);
	int elt_size = tree_low_cst (TYPE_SIZE (elttype), 1);
	enum machine_mode eltmode = TYPE_MODE (elttype);
	HOST_WIDE_INT bitsize;
	HOST_WIDE_INT bitpos;
	rtvec vector = NULL;
	unsigned n_elts;
	alias_set_type alias;

	gcc_assert (eltmode != BLKmode);

	n_elts = TYPE_VECTOR_SUBPARTS (type);
	if (REG_P (target) && VECTOR_MODE_P (GET_MODE (target)))
	  {
	    enum machine_mode mode = GET_MODE (target);

	    icode = (int) optab_handler (vec_init_optab, mode);
	    if (icode != CODE_FOR_nothing)
	      {
		unsigned int i;

		vector = rtvec_alloc (n_elts);
		for (i = 0; i < n_elts; i++)
		  RTVEC_ELT (vector, i) = CONST0_RTX (GET_MODE_INNER (mode));
	      }
	  }

	/* If the constructor has fewer elements than the vector,
	   clear the whole array first.  Similarly if this is static
	   constructor of a non-BLKmode object.  */
	if (cleared)
	  need_to_clear = 0;
	else if (REG_P (target) && TREE_STATIC (exp))
	  need_to_clear = 1;
	else
	  {
	    unsigned HOST_WIDE_INT count = 0, zero_count = 0;
	    tree value;

	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, value)
	      {
		int n_elts_here = tree_low_cst
		  (int_const_binop (TRUNC_DIV_EXPR,
				    TYPE_SIZE (TREE_TYPE (value)),
				    TYPE_SIZE (elttype), 0), 1);

		count += n_elts_here;
		if (mostly_zeros_p (value))
		  zero_count += n_elts_here;
	      }

	    /* Clear the entire vector first if there are any missing elements,
	       or if the incidence of zero elements is >= 75%.  */
	    need_to_clear = (count < n_elts || 4 * zero_count >= 3 * count);
	  }

	if (need_to_clear && size > 0 && !vector)
	  {
	    if (REG_P (target))
	      emit_move_insn (target, CONST0_RTX (GET_MODE (target)));
	    else
	      clear_storage (target, GEN_INT (size), BLOCK_OP_NORMAL);
	    cleared = 1;
	  }

	/* Inform later passes that the old value is dead.  */
	if (!cleared && !vector && REG_P (target))
	  emit_move_insn (target, CONST0_RTX (GET_MODE (target)));

        if (MEM_P (target))
	  alias = MEM_ALIAS_SET (target);
	else
	  alias = get_alias_set (elttype);

        /* Store each element of the constructor into the corresponding
	   element of TARGET, determined by counting the elements.  */
	for (idx = 0, i = 0;
	     VEC_iterate (constructor_elt, CONSTRUCTOR_ELTS (exp), idx, ce);
	     idx++, i += bitsize / elt_size)
	  {
	    HOST_WIDE_INT eltpos;
	    tree value = ce->value;

	    bitsize = tree_low_cst (TYPE_SIZE (TREE_TYPE (value)), 1);
	    if (cleared && initializer_zerop (value))
	      continue;

	    if (ce->index)
	      eltpos = tree_low_cst (ce->index, 1);
	    else
	      eltpos = i;

	    if (vector)
	      {
	        /* Vector CONSTRUCTORs should only be built from smaller
		   vectors in the case of BLKmode vectors.  */
		gcc_assert (TREE_CODE (TREE_TYPE (value)) != VECTOR_TYPE);
		RTVEC_ELT (vector, eltpos)
		  = expand_normal (value);
	      }
	    else
	      {
		enum machine_mode value_mode =
		  TREE_CODE (TREE_TYPE (value)) == VECTOR_TYPE
		  ? TYPE_MODE (TREE_TYPE (value))
		  : eltmode;
		bitpos = eltpos * elt_size;
		store_constructor_field (target, bitsize, bitpos,
					 value_mode, value, type,
					 cleared, alias);
	      }
	  }

	if (vector)
	  emit_insn (GEN_FCN (icode)
		     (target,
		      gen_rtx_PARALLEL (GET_MODE (target), vector)));
	break;
      }

    default:
      gcc_unreachable ();
    }
}

/* Store the value of EXP (an expression tree)
   into a subfield of TARGET which has mode MODE and occupies
   BITSIZE bits, starting BITPOS bits from the start of TARGET.
   If MODE is VOIDmode, it means that we are storing into a bit-field.

   Always return const0_rtx unless we have something particular to
   return.

   TYPE is the type of the underlying object,

   ALIAS_SET is the alias set for the destination.  This value will
   (in general) be different from that for TARGET, since TARGET is a
   reference to the containing structure.

   If NONTEMPORAL is true, try generating a nontemporal store.  */

static rtx
store_field (rtx target, HOST_WIDE_INT bitsize, HOST_WIDE_INT bitpos,
	     enum machine_mode mode, tree exp, tree type,
	     alias_set_type alias_set, bool nontemporal)
{
  if (TREE_CODE (exp) == ERROR_MARK)
    return const0_rtx;

  /* If we have nothing to store, do nothing unless the expression has
     side-effects.  */
  if (bitsize == 0)
    return expand_expr (exp, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* If we are storing into an unaligned field of an aligned union that is
     in a register, we may have the mode of TARGET being an integer mode but
     MODE == BLKmode.  In that case, get an aligned object whose size and
     alignment are the same as TARGET and store TARGET into it (we can avoid
     the store if the field being stored is the entire width of TARGET).  Then
     call ourselves recursively to store the field into a BLKmode version of
     that object.  Finally, load from the object into TARGET.  This is not
     very efficient in general, but should only be slightly more expensive
     than the otherwise-required unaligned accesses.  Perhaps this can be
     cleaned up later.  It's tempting to make OBJECT readonly, but it's set
     twice, once with emit_move_insn and once via store_field.  */

  if (mode == BLKmode
      && (REG_P (target) || GET_CODE (target) == SUBREG))
    {
      rtx object = assign_temp (type, 0, 1, 1);
      rtx blk_object = adjust_address (object, BLKmode, 0);

      if (bitsize != (HOST_WIDE_INT) GET_MODE_BITSIZE (GET_MODE (target)))
	emit_move_insn (object, target);

      store_field (blk_object, bitsize, bitpos, mode, exp, type, alias_set,
		   nontemporal);

      emit_move_insn (target, object);

      /* We want to return the BLKmode version of the data.  */
      return blk_object;
    }

  if (GET_CODE (target) == CONCAT)
    {
      /* We're storing into a struct containing a single __complex.  */

      gcc_assert (!bitpos);
      return store_expr (exp, target, 0, nontemporal);
    }

  /* If the structure is in a register or if the component
     is a bit field, we cannot use addressing to access it.
     Use bit-field techniques or SUBREG to store in it.  */

  if (mode == VOIDmode
      || (mode != BLKmode && ! direct_store[(int) mode]
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
      || REG_P (target)
      || GET_CODE (target) == SUBREG
      /* If the field isn't aligned enough to store as an ordinary memref,
	 store it as a bit field.  */
      || (mode != BLKmode
	  && ((((MEM_ALIGN (target) < GET_MODE_ALIGNMENT (mode))
		|| bitpos % GET_MODE_ALIGNMENT (mode))
	       && SLOW_UNALIGNED_ACCESS (mode, MEM_ALIGN (target)))
	      || (bitpos % BITS_PER_UNIT != 0)))
      /* If the RHS and field are a constant size and the size of the
	 RHS isn't the same size as the bitfield, we must use bitfield
	 operations.  */
      || (bitsize >= 0
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST
	  && compare_tree_int (TYPE_SIZE (TREE_TYPE (exp)), bitsize) != 0)
      /* If we are expanding a MEM_REF of a non-BLKmode non-addressable
         decl we must use bitfield operations.  */
      || (bitsize >= 0
	  && TREE_CODE (exp) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	  && DECL_P (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	  && !TREE_ADDRESSABLE (TREE_OPERAND (TREE_OPERAND (exp, 0),0 ))
	  && DECL_MODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) != BLKmode))
    {
      rtx temp;
      gimple nop_def;

      /* If EXP is a NOP_EXPR of precision less than its mode, then that
	 implies a mask operation.  If the precision is the same size as
	 the field we're storing into, that mask is redundant.  This is
	 particularly common with bit field assignments generated by the
	 C front end.  */
      nop_def = get_def_for_expr (exp, NOP_EXPR);
      if (nop_def)
	{
	  tree type = TREE_TYPE (exp);
	  if (INTEGRAL_TYPE_P (type)
	      && TYPE_PRECISION (type) < GET_MODE_BITSIZE (TYPE_MODE (type))
	      && bitsize == TYPE_PRECISION (type))
	    {
	      tree op = gimple_assign_rhs1 (nop_def);
	      type = TREE_TYPE (op);
	      if (INTEGRAL_TYPE_P (type) && TYPE_PRECISION (type) >= bitsize)
		exp = op;
	    }
	}

      temp = expand_normal (exp);

      /* If BITSIZE is narrower than the size of the type of EXP
	 we will be narrowing TEMP.  Normally, what's wanted are the
	 low-order bits.  However, if EXP's type is a record and this is
	 big-endian machine, we want the upper BITSIZE bits.  */
      if (BYTES_BIG_ENDIAN && GET_MODE_CLASS (GET_MODE (temp)) == MODE_INT
	  && bitsize < (HOST_WIDE_INT) GET_MODE_BITSIZE (GET_MODE (temp))
	  && TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	temp = expand_shift (RSHIFT_EXPR, GET_MODE (temp), temp,
			     size_int (GET_MODE_BITSIZE (GET_MODE (temp))
				       - bitsize),
			     NULL_RTX, 1);

      /* Unless MODE is VOIDmode or BLKmode, convert TEMP to
	 MODE.  */
      if (mode != VOIDmode && mode != BLKmode
	  && mode != TYPE_MODE (TREE_TYPE (exp)))
	temp = convert_modes (mode, TYPE_MODE (TREE_TYPE (exp)), temp, 1);

      /* If the modes of TEMP and TARGET are both BLKmode, both
	 must be in memory and BITPOS must be aligned on a byte
	 boundary.  If so, we simply do a block copy.  Likewise
	 for a BLKmode-like TARGET.  */
      if (GET_MODE (temp) == BLKmode
	  && (GET_MODE (target) == BLKmode
	      || (MEM_P (target)
		  && GET_MODE_CLASS (GET_MODE (target)) == MODE_INT
		  && (bitpos % BITS_PER_UNIT) == 0
		  && (bitsize % BITS_PER_UNIT) == 0)))
	{
	  gcc_assert (MEM_P (target) && MEM_P (temp)
		      && (bitpos % BITS_PER_UNIT) == 0);

	  target = adjust_address (target, VOIDmode, bitpos / BITS_PER_UNIT);
	  emit_block_move (target, temp,
			   GEN_INT ((bitsize + BITS_PER_UNIT - 1)
				    / BITS_PER_UNIT),
			   BLOCK_OP_NORMAL);

	  return const0_rtx;
	}

      /* Store the value in the bitfield.  */
      store_bit_field (target, bitsize, bitpos, mode, temp);

      return const0_rtx;
    }
  else
    {
      /* Now build a reference to just the desired component.  */
      rtx to_rtx = adjust_address (target, mode, bitpos / BITS_PER_UNIT);

      if (to_rtx == target)
	to_rtx = copy_rtx (to_rtx);

      if (!MEM_SCALAR_P (to_rtx))
	MEM_IN_STRUCT_P (to_rtx) = 1;
      if (!MEM_KEEP_ALIAS_SET_P (to_rtx) && MEM_ALIAS_SET (to_rtx) != 0)
	set_mem_alias_set (to_rtx, alias_set);

      return store_expr (exp, to_rtx, 0, nontemporal);
    }
}

/* Given an expression EXP that may be a COMPONENT_REF, a BIT_FIELD_REF,
   an ARRAY_REF, or an ARRAY_RANGE_REF, look for nested operations of these
   codes and find the ultimate containing object, which we return.

   We set *PBITSIZE to the size in bits that we want, *PBITPOS to the
   bit position, and *PUNSIGNEDP to the signedness of the field.
   If the position of the field is variable, we store a tree
   giving the variable offset (in units) in *POFFSET.
   This offset is in addition to the bit position.
   If the position is not variable, we store 0 in *POFFSET.

   If any of the extraction expressions is volatile,
   we store 1 in *PVOLATILEP.  Otherwise we don't change that.

   If the field is a non-BLKmode bit-field, *PMODE is set to VOIDmode.
   Otherwise, it is a mode that can be used to access the field.

   If the field describes a variable-sized object, *PMODE is set to
   BLKmode and *PBITSIZE is set to -1.  An access cannot be made in
   this case, but the address of the object can be found.

   If KEEP_ALIGNING is true and the target is STRICT_ALIGNMENT, we don't
   look through nodes that serve as markers of a greater alignment than
   the one that can be deduced from the expression.  These nodes make it
   possible for front-ends to prevent temporaries from being created by
   the middle-end on alignment considerations.  For that purpose, the
   normal operating mode at high-level is to always pass FALSE so that
   the ultimate containing object is really returned; moreover, the
   associated predicate handled_component_p will always return TRUE
   on these nodes, thus indicating that they are essentially handled
   by get_inner_reference.  TRUE should only be passed when the caller
   is scanning the expression in order to build another representation
   and specifically knows how to handle these nodes; as such, this is
   the normal operating mode in the RTL expanders.  */

tree
get_inner_reference (tree exp, HOST_WIDE_INT *pbitsize,
		     HOST_WIDE_INT *pbitpos, tree *poffset,
		     enum machine_mode *pmode, int *punsignedp,
		     int *pvolatilep, bool keep_aligning)
{
  tree size_tree = 0;
  enum machine_mode mode = VOIDmode;
  bool blkmode_bitfield = false;
  tree offset = size_zero_node;
  double_int bit_offset = double_int_zero;

  /* First get the mode, signedness, and size.  We do this from just the
     outermost expression.  */
  *pbitsize = -1;
  if (TREE_CODE (exp) == COMPONENT_REF)
    {
      tree field = TREE_OPERAND (exp, 1);
      size_tree = DECL_SIZE (field);
      if (!DECL_BIT_FIELD (field))
	mode = DECL_MODE (field);
      else if (DECL_MODE (field) == BLKmode)
	blkmode_bitfield = true;
      else if (TREE_THIS_VOLATILE (exp)
	       && flag_strict_volatile_bitfields > 0)
	/* Volatile bitfields should be accessed in the mode of the
	     field's type, not the mode computed based on the bit
	     size.  */
	mode = TYPE_MODE (DECL_BIT_FIELD_TYPE (field));

      *punsignedp = DECL_UNSIGNED (field);
    }
  else if (TREE_CODE (exp) == BIT_FIELD_REF)
    {
      size_tree = TREE_OPERAND (exp, 1);
      *punsignedp = (! INTEGRAL_TYPE_P (TREE_TYPE (exp))
		     || TYPE_UNSIGNED (TREE_TYPE (exp)));

      /* For vector types, with the correct size of access, use the mode of
	 inner type.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == VECTOR_TYPE
	  && TREE_TYPE (exp) == TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0)))
	  && tree_int_cst_equal (size_tree, TYPE_SIZE (TREE_TYPE (exp))))
        mode = TYPE_MODE (TREE_TYPE (exp));
    }
  else
    {
      mode = TYPE_MODE (TREE_TYPE (exp));
      *punsignedp = TYPE_UNSIGNED (TREE_TYPE (exp));

      if (mode == BLKmode)
	size_tree = TYPE_SIZE (TREE_TYPE (exp));
      else
	*pbitsize = GET_MODE_BITSIZE (mode);
    }

  if (size_tree != 0)
    {
      if (! host_integerp (size_tree, 1))
	mode = BLKmode, *pbitsize = -1;
      else
	*pbitsize = tree_low_cst (size_tree, 1);
    }

  /* Compute cumulative bit-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */
  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case BIT_FIELD_REF:
	  bit_offset
	    = double_int_add (bit_offset,
			      tree_to_double_int (TREE_OPERAND (exp, 2)));
	  break;

	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    tree this_offset = component_ref_field_offset (exp);

	    /* If this field hasn't been filled in yet, don't go past it.
	       This should only happen when folding expressions made during
	       type construction.  */
	    if (this_offset == 0)
	      break;

	    offset = size_binop (PLUS_EXPR, offset, this_offset);
	    bit_offset = double_int_add (bit_offset,
					 tree_to_double_int
					   (DECL_FIELD_BIT_OFFSET (field)));

	    /* ??? Right now we don't do anything with DECL_OFFSET_ALIGN.  */
	  }
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree index = TREE_OPERAND (exp, 1);
	    tree low_bound = array_ref_low_bound (exp);
	    tree unit_size = array_ref_element_size (exp);

	    /* We assume all arrays have sizes that are a multiple of a byte.
	       First subtract the lower bound, if any, in the type of the
	       index, then convert to sizetype and multiply by the size of
	       the array element.  */
	    if (! integer_zerop (low_bound))
	      index = fold_build2 (MINUS_EXPR, TREE_TYPE (index),
				   index, low_bound);

	    offset = size_binop (PLUS_EXPR, offset,
			         size_binop (MULT_EXPR,
					     fold_convert (sizetype, index),
					     unit_size));
	  }
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  bit_offset = double_int_add (bit_offset,
				       uhwi_to_double_int (*pbitsize));
	  break;

	case VIEW_CONVERT_EXPR:
	  if (keep_aligning && STRICT_ALIGNMENT
	      && (TYPE_ALIGN (TREE_TYPE (exp))
	       > TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0))))
	      && (TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0)))
		  < BIGGEST_ALIGNMENT)
	      && (TYPE_ALIGN_OK (TREE_TYPE (exp))
		  || TYPE_ALIGN_OK (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	    goto done;
	  break;

	case MEM_REF:
	  /* Hand back the decl for MEM[&decl, off].  */
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR)
	    {
	      tree off = TREE_OPERAND (exp, 1);
	      if (!integer_zerop (off))
		{
		  double_int boff, coff = mem_ref_offset (exp);
		  boff = double_int_lshift (coff,
					    BITS_PER_UNIT == 8
					    ? 3 : exact_log2 (BITS_PER_UNIT),
					    HOST_BITS_PER_DOUBLE_INT, true);
		  bit_offset = double_int_add (bit_offset, boff);
		}
	      exp = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	    }
	  goto done;

	default:
	  goto done;
	}

      /* If any reference in the chain is volatile, the effect is volatile.  */
      if (TREE_THIS_VOLATILE (exp))
	*pvolatilep = 1;

      exp = TREE_OPERAND (exp, 0);
    }
 done:

  /* If OFFSET is constant, see if we can return the whole thing as a
     constant bit position.  Make sure to handle overflow during
     this conversion.  */
  if (host_integerp (offset, 0))
    {
      double_int tem = double_int_lshift (tree_to_double_int (offset),
					  BITS_PER_UNIT == 8
					  ? 3 : exact_log2 (BITS_PER_UNIT),
					  HOST_BITS_PER_DOUBLE_INT, true);
      tem = double_int_add (tem, bit_offset);
      if (double_int_fits_in_shwi_p (tem))
	{
	  *pbitpos = double_int_to_shwi (tem);
	  *poffset = offset = NULL_TREE;
	}
    }

  /* Otherwise, split it up.  */
  if (offset)
    {
      *pbitpos = double_int_to_shwi (bit_offset);
      *poffset = offset;
    }

  /* We can use BLKmode for a byte-aligned BLKmode bitfield.  */
  if (mode == VOIDmode
      && blkmode_bitfield
      && (*pbitpos % BITS_PER_UNIT) == 0
      && (*pbitsize % BITS_PER_UNIT) == 0)
    *pmode = BLKmode;
  else
    *pmode = mode;

  return exp;
}

/* Given an expression EXP that may be a COMPONENT_REF, an ARRAY_REF or an
   ARRAY_RANGE_REF, look for whether EXP or any nested component-refs within
   EXP is marked as PACKED.  */

bool
contains_packed_reference (const_tree exp)
{
  bool packed_p = false;

  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    packed_p = DECL_PACKED (field)
		       || TYPE_PACKED (TREE_TYPE (field))
		       || TYPE_PACKED (TREE_TYPE (exp));
	    if (packed_p)
	      goto done;
	  }
	  break;

	case BIT_FIELD_REF:
	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  break;

	default:
	  goto done;
	}
      exp = TREE_OPERAND (exp, 0);
    }
 done:
  return packed_p;
}

/* Return a tree of sizetype representing the size, in bytes, of the element
   of EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_element_size (tree exp)
{
  tree aligned_size = TREE_OPERAND (exp, 3);
  tree elmt_type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  location_t loc = EXPR_LOCATION (exp);

  /* If a size was specified in the ARRAY_REF, it's the size measured
     in alignment units of the element type.  So multiply by that value.  */
  if (aligned_size)
    {
      /* ??? tree_ssa_useless_type_conversion will eliminate casts to
	 sizetype from another type of the same width and signedness.  */
      if (TREE_TYPE (aligned_size) != sizetype)
	aligned_size = fold_convert_loc (loc, sizetype, aligned_size);
      return size_binop_loc (loc, MULT_EXPR, aligned_size,
			     size_int (TYPE_ALIGN_UNIT (elmt_type)));
    }

  /* Otherwise, take the size from that of the element type.  Substitute
     any PLACEHOLDER_EXPR that we have.  */
  else
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (elmt_type), exp);
}

/* Return a tree representing the lower bound of the array mentioned in
   EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_low_bound (tree exp)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (exp, 0)));

  /* If a lower bound is specified in EXP, use it.  */
  if (TREE_OPERAND (exp, 2))
    return TREE_OPERAND (exp, 2);

  /* Otherwise, if there is a domain type and it has a lower bound, use it,
     substituting for a PLACEHOLDER_EXPR as needed.  */
  if (domain_type && TYPE_MIN_VALUE (domain_type))
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_MIN_VALUE (domain_type), exp);

  /* Otherwise, return a zero of the appropriate type.  */
  return build_int_cst (TREE_TYPE (TREE_OPERAND (exp, 1)), 0);
}

/* Return a tree representing the upper bound of the array mentioned in
   EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_up_bound (tree exp)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (exp, 0)));

  /* If there is a domain type and it has an upper bound, use it, substituting
     for a PLACEHOLDER_EXPR as needed.  */
  if (domain_type && TYPE_MAX_VALUE (domain_type))
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_MAX_VALUE (domain_type), exp);

  /* Otherwise fail.  */
  return NULL_TREE;
}

/* Return a tree representing the offset, in bytes, of the field referenced
   by EXP.  This does not include any offset in DECL_FIELD_BIT_OFFSET.  */

tree
component_ref_field_offset (tree exp)
{
  tree aligned_offset = TREE_OPERAND (exp, 2);
  tree field = TREE_OPERAND (exp, 1);
  location_t loc = EXPR_LOCATION (exp);

  /* If an offset was specified in the COMPONENT_REF, it's the offset measured
     in units of DECL_OFFSET_ALIGN / BITS_PER_UNIT.  So multiply by that
     value.  */
  if (aligned_offset)
    {
      /* ??? tree_ssa_useless_type_conversion will eliminate casts to
	 sizetype from another type of the same width and signedness.  */
      if (TREE_TYPE (aligned_offset) != sizetype)
	aligned_offset = fold_convert_loc (loc, sizetype, aligned_offset);
      return size_binop_loc (loc, MULT_EXPR, aligned_offset,
			     size_int (DECL_OFFSET_ALIGN (field)
				       / BITS_PER_UNIT));
    }

  /* Otherwise, take the offset from that of the field.  Substitute
     any PLACEHOLDER_EXPR that we have.  */
  else
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (DECL_FIELD_OFFSET (field), exp);
}

/* Alignment in bits the TARGET of an assignment may be assumed to have.  */

static unsigned HOST_WIDE_INT
target_align (const_tree target)
{
  /* We might have a chain of nested references with intermediate misaligning
     bitfields components, so need to recurse to find out.  */

  unsigned HOST_WIDE_INT this_align, outer_align;

  switch (TREE_CODE (target))
    {
    case BIT_FIELD_REF:
      return 1;

    case COMPONENT_REF:
      this_align = DECL_ALIGN (TREE_OPERAND (target, 1));
      outer_align = target_align (TREE_OPERAND (target, 0));
      return MIN (this_align, outer_align);

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      this_align = TYPE_ALIGN (TREE_TYPE (target));
      outer_align = target_align (TREE_OPERAND (target, 0));
      return MIN (this_align, outer_align);

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      this_align = TYPE_ALIGN (TREE_TYPE (target));
      outer_align = target_align (TREE_OPERAND (target, 0));
      return MAX (this_align, outer_align);

    default:
      return TYPE_ALIGN (TREE_TYPE (target));
    }
}


/* Given an rtx VALUE that may contain additions and multiplications, return
   an equivalent value that just refers to a register, memory, or constant.
   This is done by generating instructions to perform the arithmetic and
   returning a pseudo-register containing the value.

   The returned value may be a REG, SUBREG, MEM or constant.  */

rtx
force_operand (rtx value, rtx target)
{
  rtx op1, op2;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = get_subtarget (target);
  enum rtx_code code = GET_CODE (value);

  /* Check for subreg applied to an expression produced by loop optimizer.  */
  if (code == SUBREG
      && !REG_P (SUBREG_REG (value))
      && !MEM_P (SUBREG_REG (value)))
    {
      value
	= simplify_gen_subreg (GET_MODE (value),
			       force_reg (GET_MODE (SUBREG_REG (value)),
					  force_operand (SUBREG_REG (value),
							 NULL_RTX)),
			       GET_MODE (SUBREG_REG (value)),
			       SUBREG_BYTE (value));
      code = GET_CODE (value);
    }

  /* Check for a PIC address load.  */
  if ((code == PLUS || code == MINUS)
      && XEXP (value, 0) == pic_offset_table_rtx
      && (GET_CODE (XEXP (value, 1)) == SYMBOL_REF
	  || GET_CODE (XEXP (value, 1)) == LABEL_REF
	  || GET_CODE (XEXP (value, 1)) == CONST))
    {
      if (!subtarget)
	subtarget = gen_reg_rtx (GET_MODE (value));
      emit_move_insn (subtarget, value);
      return subtarget;
    }

  if (ARITHMETIC_P (value))
    {
      op2 = XEXP (value, 1);
      if (!CONSTANT_P (op2) && !(REG_P (op2) && op2 != subtarget))
	subtarget = 0;
      if (code == MINUS && CONST_INT_P (op2))
	{
	  code = PLUS;
	  op2 = negate_rtx (GET_MODE (value), op2);
	}

      /* Check for an addition with OP2 a constant integer and our first
         operand a PLUS of a virtual register and something else.  In that
         case, we want to emit the sum of the virtual register and the
         constant first and then add the other value.  This allows virtual
         register instantiation to simply modify the constant rather than
         creating another one around this addition.  */
      if (code == PLUS && CONST_INT_P (op2)
	  && GET_CODE (XEXP (value, 0)) == PLUS
	  && REG_P (XEXP (XEXP (value, 0), 0))
	  && REGNO (XEXP (XEXP (value, 0), 0)) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (XEXP (XEXP (value, 0), 0)) <= LAST_VIRTUAL_REGISTER)
	{
	  rtx temp = expand_simple_binop (GET_MODE (value), code,
					  XEXP (XEXP (value, 0), 0), op2,
					  subtarget, 0, OPTAB_LIB_WIDEN);
	  return expand_simple_binop (GET_MODE (value), code, temp,
				      force_operand (XEXP (XEXP (value,
								 0), 1), 0),
				      target, 0, OPTAB_LIB_WIDEN);
	}

      op1 = force_operand (XEXP (value, 0), subtarget);
      op2 = force_operand (op2, NULL_RTX);
      switch (code)
	{
	case MULT:
	  return expand_mult (GET_MODE (value), op1, op2, target, 1);
	case DIV:
	  if (!INTEGRAL_MODE_P (GET_MODE (value)))
	    return expand_simple_binop (GET_MODE (value), code, op1, op2,
					target, 1, OPTAB_LIB_WIDEN);
	  else
	    return expand_divmod (0,
				  FLOAT_MODE_P (GET_MODE (value))
				  ? RDIV_EXPR : TRUNC_DIV_EXPR,
				  GET_MODE (value), op1, op2, target, 0);
	case MOD:
	  return expand_divmod (1, TRUNC_MOD_EXPR, GET_MODE (value), op1, op2,
				target, 0);
	case UDIV:
	  return expand_divmod (0, TRUNC_DIV_EXPR, GET_MODE (value), op1, op2,
				target, 1);
	case UMOD:
	  return expand_divmod (1, TRUNC_MOD_EXPR, GET_MODE (value), op1, op2,
				target, 1);
	case ASHIFTRT:
	  return expand_simple_binop (GET_MODE (value), code, op1, op2,
				      target, 0, OPTAB_LIB_WIDEN);
	default:
	  return expand_simple_binop (GET_MODE (value), code, op1, op2,
				      target, 1, OPTAB_LIB_WIDEN);
	}
    }
  if (UNARY_P (value))
    {
      if (!target)
	target = gen_reg_rtx (GET_MODE (value));
      op1 = force_operand (XEXP (value, 0), NULL_RTX);
      switch (code)
	{
	case ZERO_EXTEND:
	case SIGN_EXTEND:
	case TRUNCATE:
	case FLOAT_EXTEND:
	case FLOAT_TRUNCATE:
	  convert_move (target, op1, code == ZERO_EXTEND);
	  return target;

	case FIX:
	case UNSIGNED_FIX:
	  expand_fix (target, op1, code == UNSIGNED_FIX);
	  return target;

	case FLOAT:
	case UNSIGNED_FLOAT:
	  expand_float (target, op1, code == UNSIGNED_FLOAT);
	  return target;

	default:
	  return expand_simple_unop (GET_MODE (value), code, op1, target, 0);
	}
    }

#ifdef INSN_SCHEDULING
  /* On machines that have insn scheduling, we want all memory reference to be
     explicit, so we need to deal with such paradoxical SUBREGs.  */
  if (GET_CODE (value) == SUBREG && MEM_P (SUBREG_REG (value))
      && (GET_MODE_SIZE (GET_MODE (value))
	  > GET_MODE_SIZE (GET_MODE (SUBREG_REG (value)))))
    value
      = simplify_gen_subreg (GET_MODE (value),
			     force_reg (GET_MODE (SUBREG_REG (value)),
					force_operand (SUBREG_REG (value),
						       NULL_RTX)),
			     GET_MODE (SUBREG_REG (value)),
			     SUBREG_BYTE (value));
#endif

  return value;
}

/* Subroutine of expand_expr: return nonzero iff there is no way that
   EXP can reference X, which is being modified.  TOP_P is nonzero if this
   call is going to be used to determine whether we need a temporary
   for EXP, as opposed to a recursive call to this function.

   It is always safe for this routine to return zero since it merely
   searches for optimization opportunities.  */

int
safe_from_p (const_rtx x, tree exp, int top_p)
{
  rtx exp_rtl = 0;
  int i, nops;

  if (x == 0
      /* If EXP has varying size, we MUST use a target since we currently
	 have no way of allocating temporaries of variable size
	 (except for arrays that have TYPE_ARRAY_MAX_SIZE set).
	 So we assume here that something at a higher level has prevented a
	 clash.  This is somewhat bogus, but the best we can do.  Only
	 do this when X is BLKmode and when we are at the top level.  */
      || (top_p && TREE_TYPE (exp) != 0 && COMPLETE_TYPE_P (TREE_TYPE (exp))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST
	  && (TREE_CODE (TREE_TYPE (exp)) != ARRAY_TYPE
	      || TYPE_ARRAY_MAX_SIZE (TREE_TYPE (exp)) == NULL_TREE
	      || TREE_CODE (TYPE_ARRAY_MAX_SIZE (TREE_TYPE (exp)))
	      != INTEGER_CST)
	  && GET_MODE (x) == BLKmode)
      /* If X is in the outgoing argument area, it is always safe.  */
      || (MEM_P (x)
	  && (XEXP (x, 0) == virtual_outgoing_args_rtx
	      || (GET_CODE (XEXP (x, 0)) == PLUS
		  && XEXP (XEXP (x, 0), 0) == virtual_outgoing_args_rtx))))
    return 1;

  /* If this is a subreg of a hard register, declare it unsafe, otherwise,
     find the underlying pseudo.  */
  if (GET_CODE (x) == SUBREG)
    {
      x = SUBREG_REG (x);
      if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER)
	return 0;
    }

  /* Now look at our tree code and possibly recurse.  */
  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case tcc_declaration:
      exp_rtl = DECL_RTL_IF_SET (exp);
      break;

    case tcc_constant:
      return 1;

    case tcc_exceptional:
      if (TREE_CODE (exp) == TREE_LIST)
	{
	  while (1)
	    {
	      if (TREE_VALUE (exp) && !safe_from_p (x, TREE_VALUE (exp), 0))
		return 0;
	      exp = TREE_CHAIN (exp);
	      if (!exp)
		return 1;
	      if (TREE_CODE (exp) != TREE_LIST)
		return safe_from_p (x, exp, 0);
	    }
	}
      else if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  constructor_elt *ce;
	  unsigned HOST_WIDE_INT idx;

	  FOR_EACH_VEC_ELT (constructor_elt, CONSTRUCTOR_ELTS (exp), idx, ce)
	    if ((ce->index != NULL_TREE && !safe_from_p (x, ce->index, 0))
		|| !safe_from_p (x, ce->value, 0))
	      return 0;
	  return 1;
	}
      else if (TREE_CODE (exp) == ERROR_MARK)
	return 1;	/* An already-visited SAVE_EXPR? */
      else
	return 0;

    case tcc_statement:
      /* The only case we look at here is the DECL_INITIAL inside a
	 DECL_EXPR.  */
      return (TREE_CODE (exp) != DECL_EXPR
	      || TREE_CODE (DECL_EXPR_DECL (exp)) != VAR_DECL
	      || !DECL_INITIAL (DECL_EXPR_DECL (exp))
	      || safe_from_p (x, DECL_INITIAL (DECL_EXPR_DECL (exp)), 0));

    case tcc_binary:
    case tcc_comparison:
      if (!safe_from_p (x, TREE_OPERAND (exp, 1), 0))
	return 0;
      /* Fall through.  */

    case tcc_unary:
      return safe_from_p (x, TREE_OPERAND (exp, 0), 0);

    case tcc_expression:
    case tcc_reference:
    case tcc_vl_exp:
      /* Now do code-specific tests.  EXP_RTL is set to any rtx we find in
	 the expression.  If it is set, we conflict iff we are that rtx or
	 both are in memory.  Otherwise, we check all operands of the
	 expression recursively.  */

      switch (TREE_CODE (exp))
	{
	case ADDR_EXPR:
	  /* If the operand is static or we are static, we can't conflict.
	     Likewise if we don't conflict with the operand at all.  */
	  if (staticp (TREE_OPERAND (exp, 0))
	      || TREE_STATIC (exp)
	      || safe_from_p (x, TREE_OPERAND (exp, 0), 0))
	    return 1;

	  /* Otherwise, the only way this can conflict is if we are taking
	     the address of a DECL a that address if part of X, which is
	     very rare.  */
	  exp = TREE_OPERAND (exp, 0);
	  if (DECL_P (exp))
	    {
	      if (!DECL_RTL_SET_P (exp)
		  || !MEM_P (DECL_RTL (exp)))
		return 0;
	      else
		exp_rtl = XEXP (DECL_RTL (exp), 0);
	    }
	  break;

	case MEM_REF:
	  if (MEM_P (x)
	      && alias_sets_conflict_p (MEM_ALIAS_SET (x),
					get_alias_set (exp)))
	    return 0;
	  break;

	case CALL_EXPR:
	  /* Assume that the call will clobber all hard registers and
	     all of memory.  */
	  if ((REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER)
	      || MEM_P (x))
	    return 0;
	  break;

	case WITH_CLEANUP_EXPR:
	case CLEANUP_POINT_EXPR:
	  /* Lowered by gimplify.c.  */
	  gcc_unreachable ();

	case SAVE_EXPR:
	  return safe_from_p (x, TREE_OPERAND (exp, 0), 0);

	default:
	  break;
	}

      /* If we have an rtx, we do not need to scan our operands.  */
      if (exp_rtl)
	break;

      nops = TREE_OPERAND_LENGTH (exp);
      for (i = 0; i < nops; i++)
	if (TREE_OPERAND (exp, i) != 0
	    && ! safe_from_p (x, TREE_OPERAND (exp, i), 0))
	  return 0;

      break;

    case tcc_type:
      /* Should never get a type here.  */
      gcc_unreachable ();
    }

  /* If we have an rtl, find any enclosed object.  Then see if we conflict
     with it.  */
  if (exp_rtl)
    {
      if (GET_CODE (exp_rtl) == SUBREG)
	{
	  exp_rtl = SUBREG_REG (exp_rtl);
	  if (REG_P (exp_rtl)
	      && REGNO (exp_rtl) < FIRST_PSEUDO_REGISTER)
	    return 0;
	}

      /* If the rtl is X, then it is not safe.  Otherwise, it is unless both
	 are memory and they conflict.  */
      return ! (rtx_equal_p (x, exp_rtl)
		|| (MEM_P (x) && MEM_P (exp_rtl)
		    && true_dependence (exp_rtl, VOIDmode, x,
					rtx_addr_varies_p)));
    }

  /* If we reach here, it is safe.  */
  return 1;
}


/* Return the highest power of two that EXP is known to be a multiple of.
   This is used in updating alignment of MEMs in array references.  */

unsigned HOST_WIDE_INT
highest_pow2_factor (const_tree exp)
{
  unsigned HOST_WIDE_INT c0, c1;

  switch (TREE_CODE (exp))
    {
    case INTEGER_CST:
      /* We can find the lowest bit that's a one.  If the low
	 HOST_BITS_PER_WIDE_INT bits are zero, return BIGGEST_ALIGNMENT.
	 We need to handle this case since we can find it in a COND_EXPR,
	 a MIN_EXPR, or a MAX_EXPR.  If the constant overflows, we have an
	 erroneous program, so return BIGGEST_ALIGNMENT to avoid any
	 later ICE.  */
      if (TREE_OVERFLOW (exp))
	return BIGGEST_ALIGNMENT;
      else
	{
	  /* Note: tree_low_cst is intentionally not used here,
	     we don't care about the upper bits.  */
	  c0 = TREE_INT_CST_LOW (exp);
	  c0 &= -c0;
	  return c0 ? c0 : BIGGEST_ALIGNMENT;
	}
      break;

    case PLUS_EXPR:  case MINUS_EXPR:  case MIN_EXPR:  case MAX_EXPR:
      c0 = highest_pow2_factor (TREE_OPERAND (exp, 0));
      c1 = highest_pow2_factor (TREE_OPERAND (exp, 1));
      return MIN (c0, c1);

    case MULT_EXPR:
      c0 = highest_pow2_factor (TREE_OPERAND (exp, 0));
      c1 = highest_pow2_factor (TREE_OPERAND (exp, 1));
      return c0 * c1;

    case ROUND_DIV_EXPR:  case TRUNC_DIV_EXPR:  case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
      if (integer_pow2p (TREE_OPERAND (exp, 1))
	  && host_integerp (TREE_OPERAND (exp, 1), 1))
	{
	  c0 = highest_pow2_factor (TREE_OPERAND (exp, 0));
	  c1 = tree_low_cst (TREE_OPERAND (exp, 1), 1);
	  return MAX (1, c0 / c1);
	}
      break;

    case BIT_AND_EXPR:
      /* The highest power of two of a bit-and expression is the maximum of
	 that of its operands.  We typically get here for a complex LHS and
	 a constant negative power of two on the RHS to force an explicit
	 alignment, so don't bother looking at the LHS.  */
      return highest_pow2_factor (TREE_OPERAND (exp, 1));

    CASE_CONVERT:
    case SAVE_EXPR:
      return highest_pow2_factor (TREE_OPERAND (exp, 0));

    case COMPOUND_EXPR:
      return highest_pow2_factor (TREE_OPERAND (exp, 1));

    case COND_EXPR:
      c0 = highest_pow2_factor (TREE_OPERAND (exp, 1));
      c1 = highest_pow2_factor (TREE_OPERAND (exp, 2));
      return MIN (c0, c1);

    default:
      break;
    }

  return 1;
}

/* Similar, except that the alignment requirements of TARGET are
   taken into account.  Assume it is at least as aligned as its
   type, unless it is a COMPONENT_REF in which case the layout of
   the structure gives the alignment.  */

static unsigned HOST_WIDE_INT
highest_pow2_factor_for_target (const_tree target, const_tree exp)
{
  unsigned HOST_WIDE_INT talign = target_align (target) / BITS_PER_UNIT;
  unsigned HOST_WIDE_INT factor = highest_pow2_factor (exp);

  return MAX (factor, talign);
}

/* Subroutine of expand_expr.  Expand the two operands of a binary
   expression EXP0 and EXP1 placing the results in OP0 and OP1.
   The value may be stored in TARGET if TARGET is nonzero.  The
   MODIFIER argument is as documented by expand_expr.  */

static void
expand_operands (tree exp0, tree exp1, rtx target, rtx *op0, rtx *op1,
		 enum expand_modifier modifier)
{
  if (! safe_from_p (target, exp1, 1))
    target = 0;
  if (operand_equal_p (exp0, exp1, 0))
    {
      *op0 = expand_expr (exp0, target, VOIDmode, modifier);
      *op1 = copy_rtx (*op0);
    }
  else
    {
      /* If we need to preserve evaluation order, copy exp0 into its own
	 temporary variable so that it can't be clobbered by exp1.  */
      if (flag_evaluation_order && TREE_SIDE_EFFECTS (exp1))
	exp0 = save_expr (exp0);
      *op0 = expand_expr (exp0, target, VOIDmode, modifier);
      *op1 = expand_expr (exp1, NULL_RTX, VOIDmode, modifier);
    }
}


/* Return a MEM that contains constant EXP.  DEFER is as for
   output_constant_def and MODIFIER is as for expand_expr.  */

static rtx
expand_expr_constant (tree exp, int defer, enum expand_modifier modifier)
{
  rtx mem;

  mem = output_constant_def (exp, defer);
  if (modifier != EXPAND_INITIALIZER)
    mem = use_anchored_address (mem);
  return mem;
}

/* A subroutine of expand_expr_addr_expr.  Evaluate the address of EXP.
   The TARGET, TMODE and MODIFIER arguments are as for expand_expr.  */

static rtx
expand_expr_addr_expr_1 (tree exp, rtx target, enum machine_mode tmode,
		         enum expand_modifier modifier, addr_space_t as)
{
  rtx result, subtarget;
  tree inner, offset;
  HOST_WIDE_INT bitsize, bitpos;
  int volatilep, unsignedp;
  enum machine_mode mode1;

  /* If we are taking the address of a constant and are at the top level,
     we have to use output_constant_def since we can't call force_const_mem
     at top level.  */
  /* ??? This should be considered a front-end bug.  We should not be
     generating ADDR_EXPR of something that isn't an LVALUE.  The only
     exception here is STRING_CST.  */
  if (CONSTANT_CLASS_P (exp))
    return XEXP (expand_expr_constant (exp, 0, modifier), 0);

  /* Everything must be something allowed by is_gimple_addressable.  */
  switch (TREE_CODE (exp))
    {
    case INDIRECT_REF:
      /* This case will happen via recursion for &a->b.  */
      return expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);

    case MEM_REF:
      {
	tree tem = TREE_OPERAND (exp, 0);
	if (!integer_zerop (TREE_OPERAND (exp, 1)))
	  tem = build2 (POINTER_PLUS_EXPR, TREE_TYPE (TREE_OPERAND (exp, 1)),
			tem,
			double_int_to_tree (sizetype, mem_ref_offset (exp)));
	return expand_expr (tem, target, tmode, modifier);
      }

    case CONST_DECL:
      /* Expand the initializer like constants above.  */
      return XEXP (expand_expr_constant (DECL_INITIAL (exp), 0, modifier), 0);

    case REALPART_EXPR:
      /* The real part of the complex number is always first, therefore
	 the address is the same as the address of the parent object.  */
      offset = 0;
      bitpos = 0;
      inner = TREE_OPERAND (exp, 0);
      break;

    case IMAGPART_EXPR:
      /* The imaginary part of the complex number is always second.
	 The expression is therefore always offset by the size of the
	 scalar type.  */
      offset = 0;
      bitpos = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (exp)));
      inner = TREE_OPERAND (exp, 0);
      break;

    default:
      /* If the object is a DECL, then expand it for its rtl.  Don't bypass
	 expand_expr, as that can have various side effects; LABEL_DECLs for
	 example, may not have their DECL_RTL set yet.  Expand the rtl of
	 CONSTRUCTORs too, which should yield a memory reference for the
	 constructor's contents.  Assume language specific tree nodes can
	 be expanded in some interesting way.  */
      gcc_assert (TREE_CODE (exp) < LAST_AND_UNUSED_TREE_CODE);
      if (DECL_P (exp)
	  || TREE_CODE (exp) == CONSTRUCTOR
	  || TREE_CODE (exp) == COMPOUND_LITERAL_EXPR)
	{
	  result = expand_expr (exp, target, tmode,
				modifier == EXPAND_INITIALIZER
				? EXPAND_INITIALIZER : EXPAND_CONST_ADDRESS);

	  /* If the DECL isn't in memory, then the DECL wasn't properly
	     marked TREE_ADDRESSABLE, which will be either a front-end
	     or a tree optimizer bug.  */
	  gcc_assert (MEM_P (result));
	  result = XEXP (result, 0);

	  /* ??? Is this needed anymore?  */
	  if (DECL_P (exp) && !TREE_USED (exp) == 0)
	    {
	      assemble_external (exp);
	      TREE_USED (exp) = 1;
	    }

	  if (modifier != EXPAND_INITIALIZER
	      && modifier != EXPAND_CONST_ADDRESS)
	    result = force_operand (result, target);
	  return result;
	}

      /* Pass FALSE as the last argument to get_inner_reference although
	 we are expanding to RTL.  The rationale is that we know how to
	 handle "aligning nodes" here: we can just bypass them because
	 they won't change the final object whose address will be returned
	 (they actually exist only for that purpose).  */
      inner = get_inner_reference (exp, &bitsize, &bitpos, &offset,
				   &mode1, &unsignedp, &volatilep, false);
      break;
    }

  /* We must have made progress.  */
  gcc_assert (inner != exp);

  subtarget = offset || bitpos ? NULL_RTX : target;
  /* For VIEW_CONVERT_EXPR, where the outer alignment is bigger than
     inner alignment, force the inner to be sufficiently aligned.  */
  if (CONSTANT_CLASS_P (inner)
      && TYPE_ALIGN (TREE_TYPE (inner)) < TYPE_ALIGN (TREE_TYPE (exp)))
    {
      inner = copy_node (inner);
      TREE_TYPE (inner) = copy_node (TREE_TYPE (inner));
      TYPE_ALIGN (TREE_TYPE (inner)) = TYPE_ALIGN (TREE_TYPE (exp));
      TYPE_USER_ALIGN (TREE_TYPE (inner)) = 1;
    }
  result = expand_expr_addr_expr_1 (inner, subtarget, tmode, modifier, as);

  if (offset)
    {
      rtx tmp;

      if (modifier != EXPAND_NORMAL)
	result = force_operand (result, NULL);
      tmp = expand_expr (offset, NULL_RTX, tmode,
			 modifier == EXPAND_INITIALIZER
			  ? EXPAND_INITIALIZER : EXPAND_NORMAL);

      result = convert_memory_address_addr_space (tmode, result, as);
      tmp = convert_memory_address_addr_space (tmode, tmp, as);

      if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	result = simplify_gen_binary (PLUS, tmode, result, tmp);
      else
	{
	  subtarget = bitpos ? NULL_RTX : target;
	  result = expand_simple_binop (tmode, PLUS, result, tmp, subtarget,
					1, OPTAB_LIB_WIDEN);
	}
    }

  if (bitpos)
    {
      /* Someone beforehand should have rejected taking the address
	 of such an object.  */
      gcc_assert ((bitpos % BITS_PER_UNIT) == 0);

      result = plus_constant (result, bitpos / BITS_PER_UNIT);
      if (modifier < EXPAND_SUM)
	result = force_operand (result, target);
    }

  return result;
}

/* A subroutine of expand_expr.  Evaluate EXP, which is an ADDR_EXPR.
   The TARGET, TMODE and MODIFIER arguments are as for expand_expr.  */

static rtx
expand_expr_addr_expr (tree exp, rtx target, enum machine_mode tmode,
		       enum expand_modifier modifier)
{
  addr_space_t as = ADDR_SPACE_GENERIC;
  enum machine_mode address_mode = Pmode;
  enum machine_mode pointer_mode = ptr_mode;
  enum machine_mode rmode;
  rtx result;

  /* Target mode of VOIDmode says "whatever's natural".  */
  if (tmode == VOIDmode)
    tmode = TYPE_MODE (TREE_TYPE (exp));

  if (POINTER_TYPE_P (TREE_TYPE (exp)))
    {
      as = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (exp)));
      address_mode = targetm.addr_space.address_mode (as);
      pointer_mode = targetm.addr_space.pointer_mode (as);
    }

  /* We can get called with some Weird Things if the user does silliness
     like "(short) &a".  In that case, convert_memory_address won't do
     the right thing, so ignore the given target mode.  */
  if (tmode != address_mode && tmode != pointer_mode)
    tmode = address_mode;

  result = expand_expr_addr_expr_1 (TREE_OPERAND (exp, 0), target,
				    tmode, modifier, as);

  /* Despite expand_expr claims concerning ignoring TMODE when not
     strictly convenient, stuff breaks if we don't honor it.  Note
     that combined with the above, we only do this for pointer modes.  */
  rmode = GET_MODE (result);
  if (rmode == VOIDmode)
    rmode = tmode;
  if (rmode != tmode)
    result = convert_memory_address_addr_space (tmode, result, as);

  return result;
}

/* Generate code for computing CONSTRUCTOR EXP.
   An rtx for the computed value is returned.  If AVOID_TEMP_MEM
   is TRUE, instead of creating a temporary variable in memory
   NULL is returned and the caller needs to handle it differently.  */

static rtx
expand_constructor (tree exp, rtx target, enum expand_modifier modifier,
		    bool avoid_temp_mem)
{
  tree type = TREE_TYPE (exp);
  enum machine_mode mode = TYPE_MODE (type);

  /* Try to avoid creating a temporary at all.  This is possible
     if all of the initializer is zero.
     FIXME: try to handle all [0..255] initializers we can handle
     with memset.  */
  if (TREE_STATIC (exp)
      && !TREE_ADDRESSABLE (exp)
      && target != 0 && mode == BLKmode
      && all_zeros_p (exp))
    {
      clear_storage (target, expr_size (exp), BLOCK_OP_NORMAL);
      return target;
    }

  /* All elts simple constants => refer to a constant in memory.  But
     if this is a non-BLKmode mode, let it store a field at a time
     since that should make a CONST_INT or CONST_DOUBLE when we
     fold.  Likewise, if we have a target we can use, it is best to
     store directly into the target unless the type is large enough
     that memcpy will be used.  If we are making an initializer and
     all operands are constant, put it in memory as well.

     FIXME: Avoid trying to fill vector constructors piece-meal.
     Output them with output_constant_def below unless we're sure
     they're zeros.  This should go away when vector initializers
     are treated like VECTOR_CST instead of arrays.  */
  if ((TREE_STATIC (exp)
       && ((mode == BLKmode
	    && ! (target != 0 && safe_from_p (target, exp, 1)))
		  || TREE_ADDRESSABLE (exp)
		  || (host_integerp (TYPE_SIZE_UNIT (type), 1)
		      && (! MOVE_BY_PIECES_P
				     (tree_low_cst (TYPE_SIZE_UNIT (type), 1),
				      TYPE_ALIGN (type)))
		      && ! mostly_zeros_p (exp))))
      || ((modifier == EXPAND_INITIALIZER || modifier == EXPAND_CONST_ADDRESS)
	  && TREE_CONSTANT (exp)))
    {
      rtx constructor;

      if (avoid_temp_mem)
	return NULL_RTX;

      constructor = expand_expr_constant (exp, 1, modifier);

      if (modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_INITIALIZER
	  && modifier != EXPAND_SUM)
	constructor = validize_mem (constructor);

      return constructor;
    }

  /* Handle calls that pass values in multiple non-contiguous
     locations.  The Irix 6 ABI has examples of this.  */
  if (target == 0 || ! safe_from_p (target, exp, 1)
      || GET_CODE (target) == PARALLEL || modifier == EXPAND_STACK_PARM)
    {
      if (avoid_temp_mem)
	return NULL_RTX;

      target
	= assign_temp (build_qualified_type (type, (TYPE_QUALS (type)
						    | (TREE_READONLY (exp)
						       * TYPE_QUAL_CONST))),
		       0, TREE_ADDRESSABLE (exp), 1);
    }

  store_constructor (exp, target, 0, int_expr_size (exp));
  return target;
}


/* expand_expr: generate code for computing expression EXP.
   An rtx for the computed value is returned.  The value is never null.
   In the case of a void EXP, const0_rtx is returned.

   The value may be stored in TARGET if TARGET is nonzero.
   TARGET is just a suggestion; callers must assume that
   the rtx returned may not be the same as TARGET.

   If TARGET is CONST0_RTX, it means that the value will be ignored.

   If TMODE is not VOIDmode, it suggests generating the
   result in mode TMODE.  But this is done only when convenient.
   Otherwise, TMODE is ignored and the value generated in its natural mode.
   TMODE is just a suggestion; callers must assume that
   the rtx returned may not have mode TMODE.

   Note that TARGET may have neither TMODE nor MODE.  In that case, it
   probably will not be used.

   If MODIFIER is EXPAND_SUM then when EXP is an addition
   we can return an rtx of the form (MULT (REG ...) (CONST_INT ...))
   or a nest of (PLUS ...) and (MINUS ...) where the terms are
   products as above, or REG or MEM, or constant.
   Ordinarily in such cases we would output mul or add instructions
   and then return a pseudo reg containing the sum.

   EXPAND_INITIALIZER is much like EXPAND_SUM except that
   it also marks a label as absolutely required (it can't be dead).
   It also makes a ZERO_EXTEND or SIGN_EXTEND instead of emitting extend insns.
   This is used for outputting expressions used in initializers.

   EXPAND_CONST_ADDRESS says that it is okay to return a MEM
   with a constant address even if that address is not normally legitimate.
   EXPAND_INITIALIZER and EXPAND_SUM also have this effect.

   EXPAND_STACK_PARM is used when expanding to a TARGET on the stack for
   a call parameter.  Such targets require special care as we haven't yet
   marked TARGET so that it's safe from being trashed by libcalls.  We
   don't want to use TARGET for anything but the final result;
   Intermediate values must go elsewhere.   Additionally, calls to
   emit_block_move will be flagged with BLOCK_OP_CALL_PARM.

   If EXP is a VAR_DECL whose DECL_RTL was a MEM with an invalid
   address, and ALT_RTL is non-NULL, then *ALT_RTL is set to the
   DECL_RTL of the VAR_DECL.  *ALT_RTL is also set if EXP is a
   COMPOUND_EXPR whose second argument is such a VAR_DECL, and so on
   recursively.  */

rtx
expand_expr_real (tree exp, rtx target, enum machine_mode tmode,
		  enum expand_modifier modifier, rtx *alt_rtl)
{
  rtx ret;

  /* Handle ERROR_MARK before anybody tries to access its type.  */
  if (TREE_CODE (exp) == ERROR_MARK
      || (TREE_CODE (TREE_TYPE (exp)) == ERROR_MARK))
    {
      ret = CONST0_RTX (tmode);
      return ret ? ret : const0_rtx;
    }

  /* If this is an expression of some kind and it has an associated line
     number, then emit the line number before expanding the expression.

     We need to save and restore the file and line information so that
     errors discovered during expansion are emitted with the right
     information.  It would be better of the diagnostic routines
     used the file/line information embedded in the tree nodes rather
     than globals.  */
  if (cfun && EXPR_HAS_LOCATION (exp))
    {
      location_t saved_location = input_location;
      location_t saved_curr_loc = get_curr_insn_source_location ();
      tree saved_block = get_curr_insn_block ();
      input_location = EXPR_LOCATION (exp);
      set_curr_insn_source_location (input_location);

      /* Record where the insns produced belong.  */
      set_curr_insn_block (TREE_BLOCK (exp));

      ret = expand_expr_real_1 (exp, target, tmode, modifier, alt_rtl);

      input_location = saved_location;
      set_curr_insn_block (saved_block);
      set_curr_insn_source_location (saved_curr_loc);
    }
  else
    {
      ret = expand_expr_real_1 (exp, target, tmode, modifier, alt_rtl);
    }

  return ret;
}

rtx
expand_expr_real_2 (sepops ops, rtx target, enum machine_mode tmode,
		    enum expand_modifier modifier)
{
  rtx op0, op1, op2, temp;
  tree type;
  int unsignedp;
  enum machine_mode mode;
  enum tree_code code = ops->code;
  optab this_optab;
  rtx subtarget, original_target;
  int ignore;
  bool reduce_bit_field;
  location_t loc = ops->location;
  tree treeop0, treeop1, treeop2;
#define REDUCE_BIT_FIELD(expr)	(reduce_bit_field			  \
				 ? reduce_to_bit_field_precision ((expr), \
								  target, \
								  type)	  \
				 : (expr))

  type = ops->type;
  mode = TYPE_MODE (type);
  unsignedp = TYPE_UNSIGNED (type);

  treeop0 = ops->op0;
  treeop1 = ops->op1;
  treeop2 = ops->op2;

  /* We should be called only on simple (binary or unary) expressions,
     exactly those that are valid in gimple expressions that aren't
     GIMPLE_SINGLE_RHS (or invalid).  */
  gcc_assert (get_gimple_rhs_class (code) == GIMPLE_UNARY_RHS
	      || get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS
	      || get_gimple_rhs_class (code) == GIMPLE_TERNARY_RHS);

  ignore = (target == const0_rtx
	    || ((CONVERT_EXPR_CODE_P (code)
		 || code == COND_EXPR || code == VIEW_CONVERT_EXPR)
		&& TREE_CODE (type) == VOID_TYPE));

  /* We should be called only if we need the result.  */
  gcc_assert (!ignore);

  /* An operation in what may be a bit-field type needs the
     result to be reduced to the precision of the bit-field type,
     which is narrower than that of the type's mode.  */
  reduce_bit_field = (TREE_CODE (type) == INTEGER_TYPE
		      && GET_MODE_PRECISION (mode) > TYPE_PRECISION (type));

  if (reduce_bit_field && modifier == EXPAND_STACK_PARM)
    target = 0;

  /* Use subtarget as the target for operand 0 of a binary operation.  */
  subtarget = get_subtarget (target);
  original_target = target;

  switch (code)
    {
    case NON_LVALUE_EXPR:
    case PAREN_EXPR:
    CASE_CONVERT:
      if (treeop0 == error_mark_node)
	return const0_rtx;

      if (TREE_CODE (type) == UNION_TYPE)
	{
	  tree valtype = TREE_TYPE (treeop0);

	  /* If both input and output are BLKmode, this conversion isn't doing
	     anything except possibly changing memory attribute.  */
	  if (mode == BLKmode && TYPE_MODE (valtype) == BLKmode)
	    {
	      rtx result = expand_expr (treeop0, target, tmode,
					modifier);

	      result = copy_rtx (result);
	      set_mem_attributes (result, type, 0);
	      return result;
	    }

	  if (target == 0)
	    {
	      if (TYPE_MODE (type) != BLKmode)
		target = gen_reg_rtx (TYPE_MODE (type));
	      else
		target = assign_temp (type, 0, 1, 1);
	    }

	  if (MEM_P (target))
	    /* Store data into beginning of memory target.  */
	    store_expr (treeop0,
			adjust_address (target, TYPE_MODE (valtype), 0),
			modifier == EXPAND_STACK_PARM,
			false);

	  else
	    {
	      gcc_assert (REG_P (target));

	      /* Store this field into a union of the proper type.  */
	      store_field (target,
			   MIN ((int_size_in_bytes (TREE_TYPE
						    (treeop0))
				 * BITS_PER_UNIT),
				(HOST_WIDE_INT) GET_MODE_BITSIZE (mode)),
			   0, TYPE_MODE (valtype), treeop0,
			   type, 0, false);
	    }

	  /* Return the entire union.  */
	  return target;
	}

      if (mode == TYPE_MODE (TREE_TYPE (treeop0)))
	{
	  op0 = expand_expr (treeop0, target, VOIDmode,
			     modifier);

	  /* If the signedness of the conversion differs and OP0 is
	     a promoted SUBREG, clear that indication since we now
	     have to do the proper extension.  */
	  if (TYPE_UNSIGNED (TREE_TYPE (treeop0)) != unsignedp
	      && GET_CODE (op0) == SUBREG)
	    SUBREG_PROMOTED_VAR_P (op0) = 0;

	  return REDUCE_BIT_FIELD (op0);
	}

      op0 = expand_expr (treeop0, NULL_RTX, mode,
			 modifier == EXPAND_SUM ? EXPAND_NORMAL : modifier);
      if (GET_MODE (op0) == mode)
	;

      /* If OP0 is a constant, just convert it into the proper mode.  */
      else if (CONSTANT_P (op0))
	{
	  tree inner_type = TREE_TYPE (treeop0);
	  enum machine_mode inner_mode = TYPE_MODE (inner_type);

	  if (modifier == EXPAND_INITIALIZER)
	    op0 = simplify_gen_subreg (mode, op0, inner_mode,
				       subreg_lowpart_offset (mode,
							      inner_mode));
	  else
	    op0=  convert_modes (mode, inner_mode, op0,
				 TYPE_UNSIGNED (inner_type));
	}

      else if (modifier == EXPAND_INITIALIZER)
	op0 = gen_rtx_fmt_e (unsignedp ? ZERO_EXTEND : SIGN_EXTEND, mode, op0);

      else if (target == 0)
	op0 = convert_to_mode (mode, op0,
			       TYPE_UNSIGNED (TREE_TYPE
					      (treeop0)));
      else
	{
	  convert_move (target, op0,
			TYPE_UNSIGNED (TREE_TYPE (treeop0)));
	  op0 = target;
	}

      return REDUCE_BIT_FIELD (op0);

    case ADDR_SPACE_CONVERT_EXPR:
      {
	tree treeop0_type = TREE_TYPE (treeop0);
	addr_space_t as_to;
	addr_space_t as_from;

	gcc_assert (POINTER_TYPE_P (type));
	gcc_assert (POINTER_TYPE_P (treeop0_type));

	as_to = TYPE_ADDR_SPACE (TREE_TYPE (type));
	as_from = TYPE_ADDR_SPACE (TREE_TYPE (treeop0_type));

        /* Conversions between pointers to the same address space should
	   have been implemented via CONVERT_EXPR / NOP_EXPR.  */
	gcc_assert (as_to != as_from);

        /* Ask target code to handle conversion between pointers
	   to overlapping address spaces.  */
	if (targetm.addr_space.subset_p (as_to, as_from)
	    || targetm.addr_space.subset_p (as_from, as_to))
	  {
	    op0 = expand_expr (treeop0, NULL_RTX, VOIDmode, modifier);
	    op0 = targetm.addr_space.convert (op0, treeop0_type, type);
	    gcc_assert (op0);
	    return op0;
	  }

	/* For disjoint address spaces, converting anything but
	   a null pointer invokes undefined behaviour.  We simply
	   always return a null pointer here.  */
	return CONST0_RTX (mode);
      }

    case POINTER_PLUS_EXPR:
      /* Even though the sizetype mode and the pointer's mode can be different
         expand is able to handle this correctly and get the correct result out
         of the PLUS_EXPR code.  */
      /* Make sure to sign-extend the sizetype offset in a POINTER_PLUS_EXPR
         if sizetype precision is smaller than pointer precision.  */
      if (TYPE_PRECISION (sizetype) < TYPE_PRECISION (type))
	treeop1 = fold_convert_loc (loc, type,
				    fold_convert_loc (loc, ssizetype,
						      treeop1));
    case PLUS_EXPR:
      /* If we are adding a constant, a VAR_DECL that is sp, fp, or ap, and
	 something else, make sure we add the register to the constant and
	 then to the other thing.  This case can occur during strength
	 reduction and doing it this way will produce better code if the
	 frame pointer or argument pointer is eliminated.

	 fold-const.c will ensure that the constant is always in the inner
	 PLUS_EXPR, so the only case we need to do anything about is if
	 sp, ap, or fp is our second argument, in which case we must swap
	 the innermost first argument and our second argument.  */

      if (TREE_CODE (treeop0) == PLUS_EXPR
	  && TREE_CODE (TREE_OPERAND (treeop0, 1)) == INTEGER_CST
	  && TREE_CODE (treeop1) == VAR_DECL
	  && (DECL_RTL (treeop1) == frame_pointer_rtx
	      || DECL_RTL (treeop1) == stack_pointer_rtx
	      || DECL_RTL (treeop1) == arg_pointer_rtx))
	{
	  tree t = treeop1;

	  treeop1 = TREE_OPERAND (treeop0, 0);
	  TREE_OPERAND (treeop0, 0) = t;
	}

      /* If the result is to be ptr_mode and we are adding an integer to
	 something, we might be forming a constant.  So try to use
	 plus_constant.  If it produces a sum and we can't accept it,
	 use force_operand.  This allows P = &ARR[const] to generate
	 efficient code on machines where a SYMBOL_REF is not a valid
	 address.

	 If this is an EXPAND_SUM call, always return the sum.  */
      if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER
	  || (mode == ptr_mode && (unsignedp || ! flag_trapv)))
	{
	  if (modifier == EXPAND_STACK_PARM)
	    target = 0;
	  if (TREE_CODE (treeop0) == INTEGER_CST
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	      && TREE_CONSTANT (treeop1))
	    {
	      rtx constant_part;

	      op1 = expand_expr (treeop1, subtarget, VOIDmode,
				 EXPAND_SUM);
	      /* Use immed_double_const to ensure that the constant is
		 truncated according to the mode of OP1, then sign extended
		 to a HOST_WIDE_INT.  Using the constant directly can result
		 in non-canonical RTL in a 64x32 cross compile.  */
	      constant_part
		= immed_double_const (TREE_INT_CST_LOW (treeop0),
				      (HOST_WIDE_INT) 0,
				      TYPE_MODE (TREE_TYPE (treeop1)));
	      op1 = plus_constant (op1, INTVAL (constant_part));
	      if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
		op1 = force_operand (op1, target);
	      return REDUCE_BIT_FIELD (op1);
	    }

	  else if (TREE_CODE (treeop1) == INTEGER_CST
		   && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
		   && TREE_CONSTANT (treeop0))
	    {
	      rtx constant_part;

	      op0 = expand_expr (treeop0, subtarget, VOIDmode,
				 (modifier == EXPAND_INITIALIZER
				 ? EXPAND_INITIALIZER : EXPAND_SUM));
	      if (! CONSTANT_P (op0))
		{
		  op1 = expand_expr (treeop1, NULL_RTX,
				     VOIDmode, modifier);
		  /* Return a PLUS if modifier says it's OK.  */
		  if (modifier == EXPAND_SUM
		      || modifier == EXPAND_INITIALIZER)
		    return simplify_gen_binary (PLUS, mode, op0, op1);
		  goto binop2;
		}
	      /* Use immed_double_const to ensure that the constant is
		 truncated according to the mode of OP1, then sign extended
		 to a HOST_WIDE_INT.  Using the constant directly can result
		 in non-canonical RTL in a 64x32 cross compile.  */
	      constant_part
		= immed_double_const (TREE_INT_CST_LOW (treeop1),
				      (HOST_WIDE_INT) 0,
				      TYPE_MODE (TREE_TYPE (treeop0)));
	      op0 = plus_constant (op0, INTVAL (constant_part));
	      if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
		op0 = force_operand (op0, target);
	      return REDUCE_BIT_FIELD (op0);
	    }
	}

      /* Use TER to expand pointer addition of a negated value
	 as pointer subtraction.  */
      if ((POINTER_TYPE_P (TREE_TYPE (treeop0))
	   || (TREE_CODE (TREE_TYPE (treeop0)) == VECTOR_TYPE
	       && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (treeop0)))))
	  && TREE_CODE (treeop1) == SSA_NAME
	  && TYPE_MODE (TREE_TYPE (treeop0))
	     == TYPE_MODE (TREE_TYPE (treeop1)))
	{
	  gimple def = get_def_for_expr (treeop1, NEGATE_EXPR);
	  if (def)
	    {
	      treeop1 = gimple_assign_rhs1 (def);
	      code = MINUS_EXPR;
	      goto do_minus;
	    }
	}

      /* No sense saving up arithmetic to be done
	 if it's all in the wrong mode to form part of an address.
	 And force_operand won't know whether to sign-extend or
	 zero-extend.  */
      if ((modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	  || mode != ptr_mode)
	{
	  expand_operands (treeop0, treeop1,
			   subtarget, &op0, &op1, EXPAND_NORMAL);
	  if (op0 == const0_rtx)
	    return op1;
	  if (op1 == const0_rtx)
	    return op0;
	  goto binop2;
	}

      expand_operands (treeop0, treeop1,
		       subtarget, &op0, &op1, modifier);
      return REDUCE_BIT_FIELD (simplify_gen_binary (PLUS, mode, op0, op1));

    case MINUS_EXPR:
    do_minus:
      /* For initializers, we are allowed to return a MINUS of two
	 symbolic constants.  Here we handle all cases when both operands
	 are constant.  */
      /* Handle difference of two symbolic constants,
	 for the sake of an initializer.  */
      if ((modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  && really_constant_p (treeop0)
	  && really_constant_p (treeop1))
	{
	  expand_operands (treeop0, treeop1,
			   NULL_RTX, &op0, &op1, modifier);

	  /* If the last operand is a CONST_INT, use plus_constant of
	     the negated constant.  Else make the MINUS.  */
	  if (CONST_INT_P (op1))
	    return REDUCE_BIT_FIELD (plus_constant (op0, - INTVAL (op1)));
	  else
	    return REDUCE_BIT_FIELD (gen_rtx_MINUS (mode, op0, op1));
	}

      /* No sense saving up arithmetic to be done
	 if it's all in the wrong mode to form part of an address.
	 And force_operand won't know whether to sign-extend or
	 zero-extend.  */
      if ((modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	  || mode != ptr_mode)
	goto binop;

      expand_operands (treeop0, treeop1,
		       subtarget, &op0, &op1, modifier);

      /* Convert A - const to A + (-const).  */
      if (CONST_INT_P (op1))
	{
	  op1 = negate_rtx (mode, op1);
	  return REDUCE_BIT_FIELD (simplify_gen_binary (PLUS, mode, op0, op1));
	}

      goto binop2;

    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
      expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);
      op2 = expand_normal (treeop2);
      target = expand_widen_pattern_expr (ops, op0, op1, op2,
					  target, unsignedp);
      return target;

    case WIDEN_MULT_EXPR:
      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (treeop0) == INTEGER_CST)
	{
	  tree t1 = treeop0;
	  treeop0 = treeop1;
	  treeop1 = t1;
	}

      /* First, check if we have a multiplication of one signed and one
	 unsigned operand.  */
      if (TREE_CODE (treeop1) != INTEGER_CST
	  && (TYPE_UNSIGNED (TREE_TYPE (treeop0))
	      != TYPE_UNSIGNED (TREE_TYPE (treeop1))))
	{
	  enum machine_mode innermode = TYPE_MODE (TREE_TYPE (treeop0));
	  this_optab = usmul_widen_optab;
	  if (mode == GET_MODE_2XWIDER_MODE (innermode))
	    {
	      if (optab_handler (this_optab, mode) != CODE_FOR_nothing)
		{
		  if (TYPE_UNSIGNED (TREE_TYPE (treeop0)))
		    expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1,
				     EXPAND_NORMAL);
		  else
		    expand_operands (treeop0, treeop1, NULL_RTX, &op1, &op0,
				     EXPAND_NORMAL);
		  goto binop3;
		}
	    }
	}
      /* Check for a multiplication with matching signedness.  */
      else if ((TREE_CODE (treeop1) == INTEGER_CST
		&& int_fits_type_p (treeop1, TREE_TYPE (treeop0)))
	       || (TYPE_UNSIGNED (TREE_TYPE (treeop1))
		   == TYPE_UNSIGNED (TREE_TYPE (treeop0))))
	{
	  tree op0type = TREE_TYPE (treeop0);
	  enum machine_mode innermode = TYPE_MODE (op0type);
	  bool zextend_p = TYPE_UNSIGNED (op0type);
	  optab other_optab = zextend_p ? smul_widen_optab : umul_widen_optab;
	  this_optab = zextend_p ? umul_widen_optab : smul_widen_optab;

	  if (mode == GET_MODE_2XWIDER_MODE (innermode)
	      && TREE_CODE (treeop0) != INTEGER_CST)
	    {
	      if (optab_handler (this_optab, mode) != CODE_FOR_nothing)
		{
		  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1,
				   EXPAND_NORMAL);
		  temp = expand_widening_mult (mode, op0, op1, target,
					       unsignedp, this_optab);
		  return REDUCE_BIT_FIELD (temp);
		}
	      if (optab_handler (other_optab, mode) != CODE_FOR_nothing
		  && innermode == word_mode)
		{
		  rtx htem, hipart;
		  op0 = expand_normal (treeop0);
		  if (TREE_CODE (treeop1) == INTEGER_CST)
		    op1 = convert_modes (innermode, mode,
					 expand_normal (treeop1), unsignedp);
		  else
		    op1 = expand_normal (treeop1);
		  temp = expand_binop (mode, other_optab, op0, op1, target,
				       unsignedp, OPTAB_LIB_WIDEN);
		  hipart = gen_highpart (innermode, temp);
		  htem = expand_mult_highpart_adjust (innermode, hipart,
						      op0, op1, hipart,
						      zextend_p);
		  if (htem != hipart)
		    emit_move_insn (hipart, htem);
		  return REDUCE_BIT_FIELD (temp);
		}
	    }
	}
      treeop0 = fold_build1 (CONVERT_EXPR, type, treeop0);
      treeop1 = fold_build1 (CONVERT_EXPR, type, treeop1);
      expand_operands (treeop0, treeop1, subtarget, &op0, &op1, EXPAND_NORMAL);
      return REDUCE_BIT_FIELD (expand_mult (mode, op0, op1, target, unsignedp));

    case FMA_EXPR:
      {
	optab opt = fma_optab;
	gimple def0, def2;

	/* If there is no insn for FMA, emit it as __builtin_fma{,f,l}
	   call.  */
	if (optab_handler (fma_optab, mode) == CODE_FOR_nothing)
	  {
	    tree fn = mathfn_built_in (TREE_TYPE (treeop0), BUILT_IN_FMA);
	    tree call_expr;

	    gcc_assert (fn != NULL_TREE);
	    call_expr = build_call_expr (fn, 3, treeop0, treeop1, treeop2);
	    return expand_builtin (call_expr, target, subtarget, mode, false);
	  }

	def0 = get_def_for_expr (treeop0, NEGATE_EXPR);
	def2 = get_def_for_expr (treeop2, NEGATE_EXPR);

	op0 = op2 = NULL;

	if (def0 && def2
	    && optab_handler (fnms_optab, mode) != CODE_FOR_nothing)
	  {
	    opt = fnms_optab;
	    op0 = expand_normal (gimple_assign_rhs1 (def0));
	    op2 = expand_normal (gimple_assign_rhs1 (def2));
	  }
	else if (def0
		 && optab_handler (fnma_optab, mode) != CODE_FOR_nothing)
	  {
	    opt = fnma_optab;
	    op0 = expand_normal (gimple_assign_rhs1 (def0));
	  }
	else if (def2
		 && optab_handler (fms_optab, mode) != CODE_FOR_nothing)
	  {
	    opt = fms_optab;
	    op2 = expand_normal (gimple_assign_rhs1 (def2));
	  }

	if (op0 == NULL)
	  op0 = expand_expr (treeop0, subtarget, VOIDmode, EXPAND_NORMAL);
	if (op2 == NULL)
	  op2 = expand_normal (treeop2);
	op1 = expand_normal (treeop1);

	return expand_ternary_op (TYPE_MODE (type), opt,
				  op0, op1, op2, target, 0);
      }

    case MULT_EXPR:
      /* If this is a fixed-point operation, then we cannot use the code
	 below because "expand_mult" doesn't support sat/no-sat fixed-point
         multiplications.   */
      if (ALL_FIXED_POINT_MODE_P (mode))
	goto binop;

      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (treeop0) == INTEGER_CST)
	{
	  tree t1 = treeop0;
	  treeop0 = treeop1;
	  treeop1 = t1;
	}

      /* Attempt to return something suitable for generating an
	 indexed address, for machines that support that.  */

      if (modifier == EXPAND_SUM && mode == ptr_mode
	  && host_integerp (treeop1, 0))
	{
	  tree exp1 = treeop1;

	  op0 = expand_expr (treeop0, subtarget, VOIDmode,
			     EXPAND_SUM);

	  if (!REG_P (op0))
	    op0 = force_operand (op0, NULL_RTX);
	  if (!REG_P (op0))
	    op0 = copy_to_mode_reg (mode, op0);

	  return REDUCE_BIT_FIELD (gen_rtx_MULT (mode, op0,
			       gen_int_mode (tree_low_cst (exp1, 0),
					     TYPE_MODE (TREE_TYPE (exp1)))));
	}

      if (modifier == EXPAND_STACK_PARM)
	target = 0;

      expand_operands (treeop0, treeop1, subtarget, &op0, &op1, EXPAND_NORMAL);
      return REDUCE_BIT_FIELD (expand_mult (mode, op0, op1, target, unsignedp));

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* If this is a fixed-point operation, then we cannot use the code
	 below because "expand_divmod" doesn't support sat/no-sat fixed-point
         divisions.   */
      if (ALL_FIXED_POINT_MODE_P (mode))
	goto binop;

      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      /* Possible optimization: compute the dividend with EXPAND_SUM
	 then if the divisor is constant can optimize the case
	 where some terms of the dividend have coeffs divisible by it.  */
      expand_operands (treeop0, treeop1,
		       subtarget, &op0, &op1, EXPAND_NORMAL);
      return expand_divmod (0, code, mode, op0, op1, target, unsignedp);

    case RDIV_EXPR:
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      expand_operands (treeop0, treeop1,
		       subtarget, &op0, &op1, EXPAND_NORMAL);
      return expand_divmod (1, code, mode, op0, op1, target, unsignedp);

    case FIXED_CONVERT_EXPR:
      op0 = expand_normal (treeop0);
      if (target == 0 || modifier == EXPAND_STACK_PARM)
	target = gen_reg_rtx (mode);

      if ((TREE_CODE (TREE_TYPE (treeop0)) == INTEGER_TYPE
	   && TYPE_UNSIGNED (TREE_TYPE (treeop0)))
          || (TREE_CODE (type) == INTEGER_TYPE && TYPE_UNSIGNED (type)))
	expand_fixed_convert (target, op0, 1, TYPE_SATURATING (type));
      else
	expand_fixed_convert (target, op0, 0, TYPE_SATURATING (type));
      return target;

    case FIX_TRUNC_EXPR:
      op0 = expand_normal (treeop0);
      if (target == 0 || modifier == EXPAND_STACK_PARM)
	target = gen_reg_rtx (mode);
      expand_fix (target, op0, unsignedp);
      return target;

    case FLOAT_EXPR:
      op0 = expand_normal (treeop0);
      if (target == 0 || modifier == EXPAND_STACK_PARM)
	target = gen_reg_rtx (mode);
      /* expand_float can't figure out what to do if FROM has VOIDmode.
	 So give it the correct mode.  With -O, cse will optimize this.  */
      if (GET_MODE (op0) == VOIDmode)
	op0 = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (treeop0)),
				op0);
      expand_float (target, op0,
		    TYPE_UNSIGNED (TREE_TYPE (treeop0)));
      return target;

    case NEGATE_EXPR:
      op0 = expand_expr (treeop0, subtarget,
			 VOIDmode, EXPAND_NORMAL);
      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      temp = expand_unop (mode,
      			  optab_for_tree_code (NEGATE_EXPR, type,
					       optab_default),
			  op0, target, 0);
      gcc_assert (temp);
      return REDUCE_BIT_FIELD (temp);

    case ABS_EXPR:
      op0 = expand_expr (treeop0, subtarget,
			 VOIDmode, EXPAND_NORMAL);
      if (modifier == EXPAND_STACK_PARM)
	target = 0;

      /* ABS_EXPR is not valid for complex arguments.  */
      gcc_assert (GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
		  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT);

      /* Unsigned abs is simply the operand.  Testing here means we don't
	 risk generating incorrect code below.  */
      if (TYPE_UNSIGNED (type))
	return op0;

      return expand_abs (mode, op0, target, unsignedp,
			 safe_from_p (target, treeop0, 1));

    case MAX_EXPR:
    case MIN_EXPR:
      target = original_target;
      if (target == 0
	  || modifier == EXPAND_STACK_PARM
	  || (MEM_P (target) && MEM_VOLATILE_P (target))
	  || GET_MODE (target) != mode
	  || (REG_P (target)
	      && REGNO (target) < FIRST_PSEUDO_REGISTER))
	target = gen_reg_rtx (mode);
      expand_operands (treeop0, treeop1,
		       target, &op0, &op1, EXPAND_NORMAL);

      /* First try to do it with a special MIN or MAX instruction.
	 If that does not win, use a conditional jump to select the proper
	 value.  */
      this_optab = optab_for_tree_code (code, type, optab_default);
      temp = expand_binop (mode, this_optab, op0, op1, target, unsignedp,
			   OPTAB_WIDEN);
      if (temp != 0)
	return temp;

      /* At this point, a MEM target is no longer useful; we will get better
	 code without it.  */

      if (! REG_P (target))
	target = gen_reg_rtx (mode);

      /* If op1 was placed in target, swap op0 and op1.  */
      if (target != op0 && target == op1)
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}

      /* We generate better code and avoid problems with op1 mentioning
	 target by forcing op1 into a pseudo if it isn't a constant.  */
      if (! CONSTANT_P (op1))
	op1 = force_reg (mode, op1);

      {
	enum rtx_code comparison_code;
	rtx cmpop1 = op1;

	if (code == MAX_EXPR)
	  comparison_code = unsignedp ? GEU : GE;
	else
	  comparison_code = unsignedp ? LEU : LE;

	/* Canonicalize to comparisons against 0.  */
	if (op1 == const1_rtx)
	  {
	    /* Converting (a >= 1 ? a : 1) into (a > 0 ? a : 1)
	       or (a != 0 ? a : 1) for unsigned.
	       For MIN we are safe converting (a <= 1 ? a : 1)
	       into (a <= 0 ? a : 1)  */
	    cmpop1 = const0_rtx;
	    if (code == MAX_EXPR)
	      comparison_code = unsignedp ? NE : GT;
	  }
	if (op1 == constm1_rtx && !unsignedp)
	  {
	    /* Converting (a >= -1 ? a : -1) into (a >= 0 ? a : -1)
	       and (a <= -1 ? a : -1) into (a < 0 ? a : -1) */
	    cmpop1 = const0_rtx;
	    if (code == MIN_EXPR)
	      comparison_code = LT;
	  }
#ifdef HAVE_conditional_move
	/* Use a conditional move if possible.  */
	if (can_conditionally_move_p (mode))
	  {
	    rtx insn;

	    /* ??? Same problem as in expmed.c: emit_conditional_move
	       forces a stack adjustment via compare_from_rtx, and we
	       lose the stack adjustment if the sequence we are about
	       to create is discarded.  */
	    do_pending_stack_adjust ();

	    start_sequence ();

	    /* Try to emit the conditional move.  */
	    insn = emit_conditional_move (target, comparison_code,
					  op0, cmpop1, mode,
					  op0, op1, mode,
					  unsignedp);

	    /* If we could do the conditional move, emit the sequence,
	       and return.  */
	    if (insn)
	      {
		rtx seq = get_insns ();
		end_sequence ();
		emit_insn (seq);
		return target;
	      }

	    /* Otherwise discard the sequence and fall back to code with
	       branches.  */
	    end_sequence ();
	  }
#endif
	if (target != op0)
	  emit_move_insn (target, op0);

	temp = gen_label_rtx ();
	do_compare_rtx_and_jump (target, cmpop1, comparison_code,
				 unsignedp, mode, NULL_RTX, NULL_RTX, temp,
				 -1);
      }
      emit_move_insn (target, op1);
      emit_label (temp);
      return target;

    case BIT_NOT_EXPR:
      op0 = expand_expr (treeop0, subtarget,
			 VOIDmode, EXPAND_NORMAL);
      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      temp = expand_unop (mode, one_cmpl_optab, op0, target, 1);
      gcc_assert (temp);
      return temp;

      /* ??? Can optimize bitwise operations with one arg constant.
	 Can optimize (a bitwise1 n) bitwise2 (a bitwise3 b)
	 and (a bitwise1 b) bitwise2 b (etc)
	 but that is probably not worth while.  */

      /* BIT_AND_EXPR is for bitwise anding.  TRUTH_AND_EXPR is for anding two
	 boolean values when we want in all cases to compute both of them.  In
	 general it is fastest to do TRUTH_AND_EXPR by computing both operands
	 as actual zero-or-1 values and then bitwise anding.  In cases where
	 there cannot be any side effects, better code would be made by
	 treating TRUTH_AND_EXPR like TRUTH_ANDIF_EXPR; but the question is
	 how to recognize those cases.  */

    case TRUTH_AND_EXPR:
      code = BIT_AND_EXPR;
    case BIT_AND_EXPR:
      goto binop;

    case TRUTH_OR_EXPR:
      code = BIT_IOR_EXPR;
    case BIT_IOR_EXPR:
      goto binop;

    case TRUTH_XOR_EXPR:
      code = BIT_XOR_EXPR;
    case BIT_XOR_EXPR:
      goto binop;

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      gcc_assert (VECTOR_MODE_P (TYPE_MODE (type))
		  || (GET_MODE_PRECISION (TYPE_MODE (type))
		      == TYPE_PRECISION (type)));
      /* fall through */

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      /* If this is a fixed-point operation, then we cannot use the code
	 below because "expand_shift" doesn't support sat/no-sat fixed-point
         shifts.   */
      if (ALL_FIXED_POINT_MODE_P (mode))
	goto binop;

      if (! safe_from_p (subtarget, treeop1, 1))
	subtarget = 0;
      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      op0 = expand_expr (treeop0, subtarget,
			 VOIDmode, EXPAND_NORMAL);
      temp = expand_shift (code, mode, op0, treeop1, target,
			   unsignedp);
      if (code == LSHIFT_EXPR)
	temp = REDUCE_BIT_FIELD (temp);
      return temp;

      /* Could determine the answer when only additive constants differ.  Also,
	 the addition of one can be handled by changing the condition.  */
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
      temp = do_store_flag (ops,
			    modifier != EXPAND_STACK_PARM ? target : NULL_RTX,
			    tmode != VOIDmode ? tmode : mode);
      if (temp)
	return temp;

      /* Use a compare and a jump for BLKmode comparisons, or for function
	 type comparisons is HAVE_canonicalize_funcptr_for_compare.  */

      if ((target == 0
	   || modifier == EXPAND_STACK_PARM
	   || ! safe_from_p (target, treeop0, 1)
	   || ! safe_from_p (target, treeop1, 1)
	   /* Make sure we don't have a hard reg (such as function's return
	      value) live across basic blocks, if not optimizing.  */
	   || (!optimize && REG_P (target)
	       && REGNO (target) < FIRST_PSEUDO_REGISTER)))
	target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

      emit_move_insn (target, const0_rtx);

      op1 = gen_label_rtx ();
      jumpifnot_1 (code, treeop0, treeop1, op1, -1);

      if (TYPE_PRECISION (type) == 1 && !TYPE_UNSIGNED (type))
	emit_move_insn (target, constm1_rtx);
      else
	emit_move_insn (target, const1_rtx);

      emit_label (op1);
      return target;

    case TRUTH_NOT_EXPR:
      if (modifier == EXPAND_STACK_PARM)
	target = 0;
      op0 = expand_expr (treeop0, target,
			 VOIDmode, EXPAND_NORMAL);
      /* The parser is careful to generate TRUTH_NOT_EXPR
	 only with operands that are always zero or one.  */
      temp = expand_binop (mode, xor_optab, op0, const1_rtx,
			   target, 1, OPTAB_LIB_WIDEN);
      gcc_assert (temp);
      return temp;

    case COMPLEX_EXPR:
      /* Get the rtx code of the operands.  */
      op0 = expand_normal (treeop0);
      op1 = expand_normal (treeop1);

      if (!target)
	target = gen_reg_rtx (TYPE_MODE (type));

      /* Move the real (op0) and imaginary (op1) parts to their location.  */
      write_complex_part (target, op0, false);
      write_complex_part (target, op1, true);

      return target;

    case WIDEN_SUM_EXPR:
      {
        tree oprnd0 = treeop0;
        tree oprnd1 = treeop1;

        expand_operands (oprnd0, oprnd1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);
        target = expand_widen_pattern_expr (ops, op0, NULL_RTX, op1,
                                            target, unsignedp);
        return target;
      }

    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
      {
        op0 = expand_normal (treeop0);
        this_optab = optab_for_tree_code (code, type, optab_default);
        temp = expand_unop (mode, this_optab, op0, target, unsignedp);
        gcc_assert (temp);
        return temp;
      }

    case VEC_EXTRACT_EVEN_EXPR:
    case VEC_EXTRACT_ODD_EXPR:
      {
        expand_operands (treeop0,  treeop1,
                         NULL_RTX, &op0, &op1, EXPAND_NORMAL);
        this_optab = optab_for_tree_code (code, type, optab_default);
        temp = expand_binop (mode, this_optab, op0, op1, target, unsignedp,
                             OPTAB_WIDEN);
        gcc_assert (temp);
        return temp;
      }

    case VEC_INTERLEAVE_HIGH_EXPR:
    case VEC_INTERLEAVE_LOW_EXPR:
      {
        expand_operands (treeop0,  treeop1,
                         NULL_RTX, &op0, &op1, EXPAND_NORMAL);
        this_optab = optab_for_tree_code (code, type, optab_default);
        temp = expand_binop (mode, this_optab, op0, op1, target, unsignedp,
                             OPTAB_WIDEN);
        gcc_assert (temp);
        return temp;
      }

    case VEC_LSHIFT_EXPR:
    case VEC_RSHIFT_EXPR:
      {
	target = expand_vec_shift_expr (ops, target);
	return target;
      }

    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
      {
	op0 = expand_normal (treeop0);
	this_optab = optab_for_tree_code (code, type, optab_default);
	temp = expand_widen_pattern_expr (ops, op0, NULL_RTX, NULL_RTX,
					  target, unsignedp);
	gcc_assert (temp);
	return temp;
      }

    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
      {
	op0 = expand_normal (treeop0);
	/* The signedness is determined from input operand.  */
	this_optab = optab_for_tree_code (code,
					  TREE_TYPE (treeop0),
					  optab_default);
	temp = expand_widen_pattern_expr
	  (ops, op0, NULL_RTX, NULL_RTX,
	   target, TYPE_UNSIGNED (TREE_TYPE (treeop0)));

	gcc_assert (temp);
	return temp;
      }

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
      {
	tree oprnd0 = treeop0;
	tree oprnd1 = treeop1;

	expand_operands (oprnd0, oprnd1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);
	target = expand_widen_pattern_expr (ops, op0, op1, NULL_RTX,
					    target, unsignedp);
	gcc_assert (target);
	return target;
      }

    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
      mode = TYPE_MODE (TREE_TYPE (treeop0));
      goto binop;

    default:
      gcc_unreachable ();
    }

  /* Here to do an ordinary binary operator.  */
 binop:
  expand_operands (treeop0, treeop1,
		   subtarget, &op0, &op1, EXPAND_NORMAL);
 binop2:
  this_optab = optab_for_tree_code (code, type, optab_default);
 binop3:
  if (modifier == EXPAND_STACK_PARM)
    target = 0;
  temp = expand_binop (mode, this_optab, op0, op1, target,
		       unsignedp, OPTAB_LIB_WIDEN);
  gcc_assert (temp);
  return REDUCE_BIT_FIELD (temp);
}
#undef REDUCE_BIT_FIELD

rtx
expand_expr_real_1 (tree exp, rtx target, enum machine_mode tmode,
		    enum expand_modifier modifier, rtx *alt_rtl)
{
  rtx op0, op1, temp, decl_rtl;
  tree type;
  int unsignedp;
  enum machine_mode mode;
  enum tree_code code = TREE_CODE (exp);
  optab this_optab;
  rtx subtarget, original_target;
  int ignore;
  tree context;
  bool reduce_bit_field;
  location_t loc = EXPR_LOCATION (exp);
  struct separate_ops ops;
  tree treeop0, treeop1, treeop2;
  tree ssa_name = NULL_TREE;
  gimple g;

  type = TREE_TYPE (exp);
  mode = TYPE_MODE (type);
  unsignedp = TYPE_UNSIGNED (type);

  treeop0 = treeop1 = treeop2 = NULL_TREE;
  if (!VL_EXP_CLASS_P (exp))
    switch (TREE_CODE_LENGTH (code))
      {
	default:
	case 3: treeop2 = TREE_OPERAND (exp, 2);
	case 2: treeop1 = TREE_OPERAND (exp, 1);
	case 1: treeop0 = TREE_OPERAND (exp, 0);
	case 0: break;
      }
  ops.code = code;
  ops.type = type;
  ops.op0 = treeop0;
  ops.op1 = treeop1;
  ops.op2 = treeop2;
  ops.location = loc;

  ignore = (target == const0_rtx
	    || ((CONVERT_EXPR_CODE_P (code)
		 || code == COND_EXPR || code == VIEW_CONVERT_EXPR)
		&& TREE_CODE (type) == VOID_TYPE));

  /* An operation in what may be a bit-field type needs the
     result to be reduced to the precision of the bit-field type,
     which is narrower than that of the type's mode.  */
  reduce_bit_field = (!ignore
		      && TREE_CODE (type) == INTEGER_TYPE
		      && GET_MODE_PRECISION (mode) > TYPE_PRECISION (type));

  /* If we are going to ignore this result, we need only do something
     if there is a side-effect somewhere in the expression.  If there
     is, short-circuit the most common cases here.  Note that we must
     not call expand_expr with anything but const0_rtx in case this
     is an initial expansion of a size that contains a PLACEHOLDER_EXPR.  */

  if (ignore)
    {
      if (! TREE_SIDE_EFFECTS (exp))
	return const0_rtx;

      /* Ensure we reference a volatile object even if value is ignored, but
	 don't do this if all we are doing is taking its address.  */
      if (TREE_THIS_VOLATILE (exp)
	  && TREE_CODE (exp) != FUNCTION_DECL
	  && mode != VOIDmode && mode != BLKmode
	  && modifier != EXPAND_CONST_ADDRESS)
	{
	  temp = expand_expr (exp, NULL_RTX, VOIDmode, modifier);
	  if (MEM_P (temp))
	    temp = copy_to_reg (temp);
	  return const0_rtx;
	}

      if (TREE_CODE_CLASS (code) == tcc_unary
	  || code == COMPONENT_REF || code == INDIRECT_REF)
	return expand_expr (treeop0, const0_rtx, VOIDmode,
			    modifier);

      else if (TREE_CODE_CLASS (code) == tcc_binary
	       || TREE_CODE_CLASS (code) == tcc_comparison
	       || code == ARRAY_REF || code == ARRAY_RANGE_REF)
	{
	  expand_expr (treeop0, const0_rtx, VOIDmode, modifier);
	  expand_expr (treeop1, const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}
      else if (code == BIT_FIELD_REF)
	{
	  expand_expr (treeop0, const0_rtx, VOIDmode, modifier);
	  expand_expr (treeop1, const0_rtx, VOIDmode, modifier);
	  expand_expr (treeop2, const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}

      target = 0;
    }

  if (reduce_bit_field && modifier == EXPAND_STACK_PARM)
    target = 0;

  /* Use subtarget as the target for operand 0 of a binary operation.  */
  subtarget = get_subtarget (target);
  original_target = target;

  switch (code)
    {
    case LABEL_DECL:
      {
	tree function = decl_function_context (exp);

	temp = label_rtx (exp);
	temp = gen_rtx_LABEL_REF (Pmode, temp);

	if (function != current_function_decl
	    && function != 0)
	  LABEL_REF_NONLOCAL_P (temp) = 1;

	temp = gen_rtx_MEM (FUNCTION_MODE, temp);
	return temp;
      }

    case SSA_NAME:
      /* ??? ivopts calls expander, without any preparation from
         out-of-ssa.  So fake instructions as if this was an access to the
	 base variable.  This unnecessarily allocates a pseudo, see how we can
	 reuse it, if partition base vars have it set already.  */
      if (!currently_expanding_to_rtl)
	return expand_expr_real_1 (SSA_NAME_VAR (exp), target, tmode, modifier,
				   NULL);

      g = get_gimple_for_ssa_name (exp);
      /* For EXPAND_INITIALIZER try harder to get something simpler.  */
      if (g == NULL
	  && modifier == EXPAND_INITIALIZER
	  && !SSA_NAME_IS_DEFAULT_DEF (exp)
	  && (optimize || DECL_IGNORED_P (SSA_NAME_VAR (exp)))
	  && stmt_is_replaceable_p (SSA_NAME_DEF_STMT (exp)))
	g = SSA_NAME_DEF_STMT (exp);
      if (g)
	return expand_expr_real (gimple_assign_rhs_to_tree (g), target, tmode,
				 modifier, NULL);

      ssa_name = exp;
      decl_rtl = get_rtx_for_ssa_name (ssa_name);
      exp = SSA_NAME_VAR (ssa_name);
      goto expand_decl_rtl;

    case PARM_DECL:
    case VAR_DECL:
      /* If a static var's type was incomplete when the decl was written,
	 but the type is complete now, lay out the decl now.  */
      if (DECL_SIZE (exp) == 0
	  && COMPLETE_OR_UNBOUND_ARRAY_TYPE_P (TREE_TYPE (exp))
	  && (TREE_STATIC (exp) || DECL_EXTERNAL (exp)))
	layout_decl (exp, 0);

      /* ... fall through ...  */

    case FUNCTION_DECL:
    case RESULT_DECL:
      decl_rtl = DECL_RTL (exp);
    expand_decl_rtl:
      gcc_assert (decl_rtl);
      decl_rtl = copy_rtx (decl_rtl);
      /* Record writes to register variables.  */
      if (modifier == EXPAND_WRITE && REG_P (decl_rtl)
	  && REGNO (decl_rtl) < FIRST_PSEUDO_REGISTER)
	{
	    int i = REGNO (decl_rtl);
	    int nregs = hard_regno_nregs[i][GET_MODE (decl_rtl)];
	    while (nregs)
	      {
		SET_HARD_REG_BIT (crtl->asm_clobbers, i);
		i++;
		nregs--;
	      }
	}

      /* Ensure variable marked as used even if it doesn't go through
	 a parser.  If it hasn't be used yet, write out an external
	 definition.  */
      if (! TREE_USED (exp))
	{
	  assemble_external (exp);
	  TREE_USED (exp) = 1;
	}

      /* Show we haven't gotten RTL for this yet.  */
      temp = 0;

      /* Variables inherited from containing functions should have
	 been lowered by this point.  */
      context = decl_function_context (exp);
      gcc_assert (!context
		  || context == current_function_decl
		  || TREE_STATIC (exp)
		  || DECL_EXTERNAL (exp)
		  /* ??? C++ creates functions that are not TREE_STATIC.  */
		  || TREE_CODE (exp) == FUNCTION_DECL);

      /* This is the case of an array whose size is to be determined
	 from its initializer, while the initializer is still being parsed.
	 See expand_decl.  */

      if (MEM_P (decl_rtl) && REG_P (XEXP (decl_rtl, 0)))
	temp = validize_mem (decl_rtl);

      /* If DECL_RTL is memory, we are in the normal case and the
	 address is not valid, get the address into a register.  */

      else if (MEM_P (decl_rtl) && modifier != EXPAND_INITIALIZER)
	{
	  if (alt_rtl)
	    *alt_rtl = decl_rtl;
	  decl_rtl = use_anchored_address (decl_rtl);
	  if (modifier != EXPAND_CONST_ADDRESS
	      && modifier != EXPAND_SUM
	      && !memory_address_addr_space_p (DECL_MODE (exp),
					       XEXP (decl_rtl, 0),
					       MEM_ADDR_SPACE (decl_rtl)))
	    temp = replace_equiv_address (decl_rtl,
					  copy_rtx (XEXP (decl_rtl, 0)));
	}

      /* If we got something, return it.  But first, set the alignment
	 if the address is a register.  */
      if (temp != 0)
	{
	  if (MEM_P (temp) && REG_P (XEXP (temp, 0)))
	    mark_reg_pointer (XEXP (temp, 0), DECL_ALIGN (exp));

	  return temp;
	}

      /* If the mode of DECL_RTL does not match that of the decl, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  */
      if (REG_P (decl_rtl) && GET_MODE (decl_rtl) != DECL_MODE (exp))
	{
	  enum machine_mode pmode;

	  /* Get the signedness to be used for this variable.  Ensure we get
	     the same mode we got when the variable was declared.  */
	  if (code == SSA_NAME
	      && (g = SSA_NAME_DEF_STMT (ssa_name))
	      && gimple_code (g) == GIMPLE_CALL)
	    pmode = promote_function_mode (type, mode, &unsignedp,
					   TREE_TYPE
					   (TREE_TYPE (gimple_call_fn (g))),
					   2);
	  else
	    pmode = promote_decl_mode (exp, &unsignedp);
	  gcc_assert (GET_MODE (decl_rtl) == pmode);

	  temp = gen_lowpart_SUBREG (mode, decl_rtl);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_UNSIGNED_SET (temp, unsignedp);
	  return temp;
	}

      return decl_rtl;

    case INTEGER_CST:
      temp = immed_double_const (TREE_INT_CST_LOW (exp),
				 TREE_INT_CST_HIGH (exp), mode);

      return temp;

    case VECTOR_CST:
      {
	tree tmp = NULL_TREE;
	if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_FRACT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_UFRACT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_ACCUM
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_UACCUM)
	  return const_vector_from_tree (exp);
	if (GET_MODE_CLASS (mode) == MODE_INT)
	  {
	    tree type_for_mode = lang_hooks.types.type_for_mode (mode, 1);
	    if (type_for_mode)
	      tmp = fold_unary_loc (loc, VIEW_CONVERT_EXPR, type_for_mode, exp);
	  }
	if (!tmp)
	  tmp = build_constructor_from_list (type,
					     TREE_VECTOR_CST_ELTS (exp));
	return expand_expr (tmp, ignore ? const0_rtx : target,
			    tmode, modifier);
      }

    case CONST_DECL:
      return expand_expr (DECL_INITIAL (exp), target, VOIDmode, modifier);

    case REAL_CST:
      /* If optimized, generate immediate CONST_DOUBLE
	 which will be turned into memory by reload if necessary.

	 We used to force a register so that loop.c could see it.  But
	 this does not allow gen_* patterns to perform optimizations with
	 the constants.  It also produces two insns in cases like "x = 1.0;".
	 On most machines, floating-point constants are not permitted in
	 many insns, so we'd end up copying it to a register in any case.

	 Now, we do the copying in expand_binop, if appropriate.  */
      return CONST_DOUBLE_FROM_REAL_VALUE (TREE_REAL_CST (exp),
					   TYPE_MODE (TREE_TYPE (exp)));

    case FIXED_CST:
      return CONST_FIXED_FROM_FIXED_VALUE (TREE_FIXED_CST (exp),
					   TYPE_MODE (TREE_TYPE (exp)));

    case COMPLEX_CST:
      /* Handle evaluating a complex constant in a CONCAT target.  */
      if (original_target && GET_CODE (original_target) == CONCAT)
	{
	  enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));
	  rtx rtarg, itarg;

	  rtarg = XEXP (original_target, 0);
	  itarg = XEXP (original_target, 1);

	  /* Move the real and imaginary parts separately.  */
	  op0 = expand_expr (TREE_REALPART (exp), rtarg, mode, EXPAND_NORMAL);
	  op1 = expand_expr (TREE_IMAGPART (exp), itarg, mode, EXPAND_NORMAL);

	  if (op0 != rtarg)
	    emit_move_insn (rtarg, op0);
	  if (op1 != itarg)
	    emit_move_insn (itarg, op1);

	  return original_target;
	}

      /* ... fall through ...  */

    case STRING_CST:
      temp = expand_expr_constant (exp, 1, modifier);

      /* temp contains a constant address.
	 On RISC machines where a constant address isn't valid,
	 make some insns to get that address into a register.  */
      if (modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_INITIALIZER
	  && modifier != EXPAND_SUM
	  && ! memory_address_addr_space_p (mode, XEXP (temp, 0),
					    MEM_ADDR_SPACE (temp)))
	return replace_equiv_address (temp,
				      copy_rtx (XEXP (temp, 0)));
      return temp;

    case SAVE_EXPR:
      {
	tree val = treeop0;
	rtx ret = expand_expr_real_1 (val, target, tmode, modifier, alt_rtl);

	if (!SAVE_EXPR_RESOLVED_P (exp))
	  {
	    /* We can indeed still hit this case, typically via builtin
	       expanders calling save_expr immediately before expanding
	       something.  Assume this means that we only have to deal
	       with non-BLKmode values.  */
	    gcc_assert (GET_MODE (ret) != BLKmode);

	    val = build_decl (EXPR_LOCATION (exp),
			      VAR_DECL, NULL, TREE_TYPE (exp));
	    DECL_ARTIFICIAL (val) = 1;
	    DECL_IGNORED_P (val) = 1;
	    treeop0 = val;
	    TREE_OPERAND (exp, 0) = treeop0;
	    SAVE_EXPR_RESOLVED_P (exp) = 1;

	    if (!CONSTANT_P (ret))
	      ret = copy_to_reg (ret);
	    SET_DECL_RTL (val, ret);
	  }

        return ret;
      }


    case CONSTRUCTOR:
      /* If we don't need the result, just ensure we evaluate any
	 subexpressions.  */
      if (ignore)
	{
	  unsigned HOST_WIDE_INT idx;
	  tree value;

	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, value)
	    expand_expr (value, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  return const0_rtx;
	}

      return expand_constructor (exp, target, modifier, false);

    case TARGET_MEM_REF:
      {
	addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (exp));
	struct mem_address addr;
	int icode, align;

	get_address_description (exp, &addr);
	op0 = addr_for_mem_ref (&addr, as, true);
	op0 = memory_address_addr_space (mode, op0, as);
	temp = gen_rtx_MEM (mode, op0);
	set_mem_attributes (temp, exp, 0);
	set_mem_addr_space (temp, as);
	align = MAX (TYPE_ALIGN (TREE_TYPE (exp)),
		     get_object_alignment (exp, BIGGEST_ALIGNMENT));
	if (mode != BLKmode
	    && (unsigned) align < GET_MODE_ALIGNMENT (mode)
	    /* If the target does not have special handling for unaligned
	       loads of mode then it can use regular moves for them.  */
	    && ((icode = optab_handler (movmisalign_optab, mode))
		!= CODE_FOR_nothing))
	  {
	    rtx reg, insn;

	    /* We've already validated the memory, and we're creating a
	       new pseudo destination.  The predicates really can't fail.  */
	    reg = gen_reg_rtx (mode);

	    /* Nor can the insn generator.  */
	    insn = GEN_FCN (icode) (reg, temp);
	    gcc_assert (insn != NULL_RTX);
	    emit_insn (insn);

	    return reg;
	  }
	return temp;
      }

    case MEM_REF:
      {
	addr_space_t as
	  = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 1))));
	enum machine_mode address_mode;
	tree base = TREE_OPERAND (exp, 0);
	gimple def_stmt;
	int icode, align;
	/* Handle expansion of non-aliased memory with non-BLKmode.  That
	   might end up in a register.  */
	if (TREE_CODE (base) == ADDR_EXPR)
	  {
	    HOST_WIDE_INT offset = mem_ref_offset (exp).low;
	    tree bit_offset;
	    base = TREE_OPERAND (base, 0);
	    if (!DECL_P (base))
	      {
		HOST_WIDE_INT off;
		base = get_addr_base_and_unit_offset (base, &off);
		gcc_assert (base);
		offset += off;
	      }
	    /* If we are expanding a MEM_REF of a non-BLKmode non-addressable
	       decl we must use bitfield operations.  */
	    if (DECL_P (base)
		&& !TREE_ADDRESSABLE (base)
		&& DECL_MODE (base) != BLKmode
		&& DECL_RTL_SET_P (base)
		&& !MEM_P (DECL_RTL (base)))
	      {
		tree bftype;
		if (offset == 0
		    && host_integerp (TYPE_SIZE (TREE_TYPE (exp)), 1)
		    && (GET_MODE_BITSIZE (DECL_MODE (base))
			== TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (exp)))))
		  return expand_expr (build1 (VIEW_CONVERT_EXPR,
					      TREE_TYPE (exp), base),
				      target, tmode, modifier);
		bit_offset = bitsize_int (offset * BITS_PER_UNIT);
		bftype = TREE_TYPE (base);
		if (TYPE_MODE (TREE_TYPE (exp)) != BLKmode)
		  bftype = TREE_TYPE (exp);
		return expand_expr (build3 (BIT_FIELD_REF, bftype,
					    base,
					    TYPE_SIZE (TREE_TYPE (exp)),
					    bit_offset),
				    target, tmode, modifier);
	      }
	  }
	address_mode = targetm.addr_space.address_mode (as);
	base = TREE_OPERAND (exp, 0);
	if ((def_stmt = get_def_for_expr (base, BIT_AND_EXPR)))
	  {
	    tree mask = gimple_assign_rhs2 (def_stmt);
	    base = build2 (BIT_AND_EXPR, TREE_TYPE (base),
			   gimple_assign_rhs1 (def_stmt), mask);
	    TREE_OPERAND (exp, 0) = base;
	  }
	align = MAX (TYPE_ALIGN (TREE_TYPE (exp)),
		     get_object_alignment (exp, BIGGEST_ALIGNMENT));
	op0 = expand_expr (base, NULL_RTX, VOIDmode, EXPAND_SUM);
	op0 = memory_address_addr_space (address_mode, op0, as);
	if (!integer_zerop (TREE_OPERAND (exp, 1)))
	  {
	    rtx off
	      = immed_double_int_const (mem_ref_offset (exp), address_mode);
	    op0 = simplify_gen_binary (PLUS, address_mode, op0, off);
	  }
	op0 = memory_address_addr_space (mode, op0, as);
	temp = gen_rtx_MEM (mode, op0);
	set_mem_attributes (temp, exp, 0);
	set_mem_addr_space (temp, as);
	if (TREE_THIS_VOLATILE (exp))
	  MEM_VOLATILE_P (temp) = 1;
	if (mode != BLKmode
	    && (unsigned) align < GET_MODE_ALIGNMENT (mode)
	    /* If the target does not have special handling for unaligned
	       loads of mode then it can use regular moves for them.  */
	    && ((icode = optab_handler (movmisalign_optab, mode))
		!= CODE_FOR_nothing))
	  {
	    rtx reg, insn;

	    /* We've already validated the memory, and we're creating a
	       new pseudo destination.  The predicates really can't fail.  */
	    reg = gen_reg_rtx (mode);

	    /* Nor can the insn generator.  */
	    insn = GEN_FCN (icode) (reg, temp);
	    emit_insn (insn);

	    return reg;
	  }
	return temp;
      }

    case ARRAY_REF:

      {
	tree array = treeop0;
	tree index = treeop1;

	/* Fold an expression like: "foo"[2].
	   This is not done in fold so it won't happen inside &.
	   Don't fold if this is for wide characters since it's too
	   difficult to do correctly and this is a very rare case.  */

	if (modifier != EXPAND_CONST_ADDRESS
	    && modifier != EXPAND_INITIALIZER
	    && modifier != EXPAND_MEMORY)
	  {
	    tree t = fold_read_from_constant_string (exp);

	    if (t)
	      return expand_expr (t, target, tmode, modifier);
	  }

	/* If this is a constant index into a constant array,
	   just get the value from the array.  Handle both the cases when
	   we have an explicit constructor and when our operand is a variable
	   that was declared const.  */

	if (modifier != EXPAND_CONST_ADDRESS
	    && modifier != EXPAND_INITIALIZER
	    && modifier != EXPAND_MEMORY
	    && TREE_CODE (array) == CONSTRUCTOR
	    && ! TREE_SIDE_EFFECTS (array)
	    && TREE_CODE (index) == INTEGER_CST)
	  {
	    unsigned HOST_WIDE_INT ix;
	    tree field, value;

	    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (array), ix,
				      field, value)
	      if (tree_int_cst_equal (field, index))
		{
		  if (!TREE_SIDE_EFFECTS (value))
		    return expand_expr (fold (value), target, tmode, modifier);
		  break;
		}
	  }

	else if (optimize >= 1
		 && modifier != EXPAND_CONST_ADDRESS
		 && modifier != EXPAND_INITIALIZER
		 && modifier != EXPAND_MEMORY
		 && TREE_READONLY (array) && ! TREE_SIDE_EFFECTS (array)
		 && TREE_CODE (array) == VAR_DECL && DECL_INITIAL (array)
		 && TREE_CODE (DECL_INITIAL (array)) != ERROR_MARK
		 && const_value_known_p (array))
	  {
	    if (TREE_CODE (index) == INTEGER_CST)
	      {
		tree init = DECL_INITIAL (array);

		if (TREE_CODE (init) == CONSTRUCTOR)
		  {
		    unsigned HOST_WIDE_INT ix;
		    tree field, value;

		    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), ix,
					      field, value)
		      if (tree_int_cst_equal (field, index))
			{
			  if (TREE_SIDE_EFFECTS (value))
			    break;

			  if (TREE_CODE (value) == CONSTRUCTOR)
			    {
			      /* If VALUE is a CONSTRUCTOR, this
				 optimization is only useful if
				 this doesn't store the CONSTRUCTOR
				 into memory.  If it does, it is more
				 efficient to just load the data from
				 the array directly.  */
			      rtx ret = expand_constructor (value, target,
							    modifier, true);
			      if (ret == NULL_RTX)
				break;
			    }

			  return expand_expr (fold (value), target, tmode,
					      modifier);
			}
		  }
		else if(TREE_CODE (init) == STRING_CST)
		  {
		    tree index1 = index;
		    tree low_bound = array_ref_low_bound (exp);
		    index1 = fold_convert_loc (loc, sizetype,
					       treeop1);

		    /* Optimize the special-case of a zero lower bound.

		       We convert the low_bound to sizetype to avoid some problems
		       with constant folding.  (E.g. suppose the lower bound is 1,
		       and its mode is QI.  Without the conversion,l (ARRAY
		       +(INDEX-(unsigned char)1)) becomes ((ARRAY+(-(unsigned char)1))
		       +INDEX), which becomes (ARRAY+255+INDEX).  Opps!)  */

		    if (! integer_zerop (low_bound))
		      index1 = size_diffop_loc (loc, index1,
					    fold_convert_loc (loc, sizetype,
							      low_bound));

		    if (0 > compare_tree_int (index1,
					      TREE_STRING_LENGTH (init)))
		      {
			tree type = TREE_TYPE (TREE_TYPE (init));
			enum machine_mode mode = TYPE_MODE (type);

			if (GET_MODE_CLASS (mode) == MODE_INT
			    && GET_MODE_SIZE (mode) == 1)
			  return gen_int_mode (TREE_STRING_POINTER (init)
					       [TREE_INT_CST_LOW (index1)],
					       mode);
		      }
		  }
	      }
	  }
      }
      goto normal_inner_ref;

    case COMPONENT_REF:
      /* If the operand is a CONSTRUCTOR, we can just extract the
	 appropriate field if it is present.  */
      if (TREE_CODE (treeop0) == CONSTRUCTOR)
	{
	  unsigned HOST_WIDE_INT idx;
	  tree field, value;

	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (treeop0),
				    idx, field, value)
	    if (field == treeop1
		/* We can normally use the value of the field in the
		   CONSTRUCTOR.  However, if this is a bitfield in
		   an integral mode that we can fit in a HOST_WIDE_INT,
		   we must mask only the number of bits in the bitfield,
		   since this is done implicitly by the constructor.  If
		   the bitfield does not meet either of those conditions,
		   we can't do this optimization.  */
		&& (! DECL_BIT_FIELD (field)
		    || ((GET_MODE_CLASS (DECL_MODE (field)) == MODE_INT)
			&& (GET_MODE_BITSIZE (DECL_MODE (field))
			    <= HOST_BITS_PER_WIDE_INT))))
	      {
		if (DECL_BIT_FIELD (field)
		    && modifier == EXPAND_STACK_PARM)
		  target = 0;
		op0 = expand_expr (value, target, tmode, modifier);
		if (DECL_BIT_FIELD (field))
		  {
		    HOST_WIDE_INT bitsize = TREE_INT_CST_LOW (DECL_SIZE (field));
		    enum machine_mode imode = TYPE_MODE (TREE_TYPE (field));

		    if (TYPE_UNSIGNED (TREE_TYPE (field)))
		      {
			op1 = GEN_INT (((HOST_WIDE_INT) 1 << bitsize) - 1);
			op0 = expand_and (imode, op0, op1, target);
		      }
		    else
		      {
			tree count
			  = build_int_cst (NULL_TREE,
					   GET_MODE_BITSIZE (imode) - bitsize);

			op0 = expand_shift (LSHIFT_EXPR, imode, op0, count,
					    target, 0);
			op0 = expand_shift (RSHIFT_EXPR, imode, op0, count,
					    target, 0);
		      }
		  }

		return op0;
	      }
	}
      goto normal_inner_ref;

    case BIT_FIELD_REF:
    case ARRAY_RANGE_REF:
    normal_inner_ref:
      {
	enum machine_mode mode1, mode2;
	HOST_WIDE_INT bitsize, bitpos;
	tree offset;
	int volatilep = 0, must_force_mem;
	bool packedp = false;
	tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					&mode1, &unsignedp, &volatilep, true);
	rtx orig_op0, memloc;

	/* If we got back the original object, something is wrong.  Perhaps
	   we are evaluating an expression too early.  In any event, don't
	   infinitely recurse.  */
	gcc_assert (tem != exp);

	if (TYPE_PACKED (TREE_TYPE (TREE_OPERAND (exp, 0)))
	    || (TREE_CODE (TREE_OPERAND (exp, 1)) == FIELD_DECL
		&& DECL_PACKED (TREE_OPERAND (exp, 1))))
	  packedp = true;

	/* If TEM's type is a union of variable size, pass TARGET to the inner
	   computation, since it will need a temporary and TARGET is known
	   to have to do.  This occurs in unchecked conversion in Ada.  */
	orig_op0 = op0
	  = expand_expr (tem,
			 (TREE_CODE (TREE_TYPE (tem)) == UNION_TYPE
			  && (TREE_CODE (TYPE_SIZE (TREE_TYPE (tem)))
			      != INTEGER_CST)
			  && modifier != EXPAND_STACK_PARM
			  ? target : NULL_RTX),
			 VOIDmode,
			 (modifier == EXPAND_INITIALIZER
			  || modifier == EXPAND_CONST_ADDRESS
			  || modifier == EXPAND_STACK_PARM)
			 ? modifier : EXPAND_NORMAL);


	/* If the bitfield is volatile, we want to access it in the
	   field's mode, not the computed mode.
	   If a MEM has VOIDmode (external with incomplete type),
	   use BLKmode for it instead.  */
	if (MEM_P (op0))
	  {
	    if (volatilep && flag_strict_volatile_bitfields > 0)
	      op0 = adjust_address (op0, mode1, 0);
	    else if (GET_MODE (op0) == VOIDmode)
	      op0 = adjust_address (op0, BLKmode, 0);
	  }

	mode2
	  = CONSTANT_P (op0) ? TYPE_MODE (TREE_TYPE (tem)) : GET_MODE (op0);

	/* If we have either an offset, a BLKmode result, or a reference
	   outside the underlying object, we must force it to memory.
	   Such a case can occur in Ada if we have unchecked conversion
	   of an expression from a scalar type to an aggregate type or
	   for an ARRAY_RANGE_REF whose type is BLKmode, or if we were
	   passed a partially uninitialized object or a view-conversion
	   to a larger size.  */
	must_force_mem = (offset
			  || mode1 == BLKmode
			  || bitpos + bitsize > GET_MODE_BITSIZE (mode2));

	/* Handle CONCAT first.  */
	if (GET_CODE (op0) == CONCAT && !must_force_mem)
	  {
	    if (bitpos == 0
		&& bitsize == GET_MODE_BITSIZE (GET_MODE (op0)))
	      return op0;
	    if (bitpos == 0
		&& bitsize == GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 0)))
		&& bitsize)
	      {
		op0 = XEXP (op0, 0);
		mode2 = GET_MODE (op0);
	      }
	    else if (bitpos == GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 0)))
		     && bitsize == GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 1)))
		     && bitpos
		     && bitsize)
	      {
		op0 = XEXP (op0, 1);
		bitpos = 0;
		mode2 = GET_MODE (op0);
	      }
	    else
	      /* Otherwise force into memory.  */
	      must_force_mem = 1;
	  }

	/* If this is a constant, put it in a register if it is a legitimate
	   constant and we don't need a memory reference.  */
	if (CONSTANT_P (op0)
	    && mode2 != BLKmode
	    && LEGITIMATE_CONSTANT_P (op0)
	    && !must_force_mem)
	  op0 = force_reg (mode2, op0);

	/* Otherwise, if this is a constant, try to force it to the constant
	   pool.  Note that back-ends, e.g. MIPS, may refuse to do so if it
	   is a legitimate constant.  */
	else if (CONSTANT_P (op0) && (memloc = force_const_mem (mode2, op0)))
	  op0 = validize_mem (memloc);

	/* Otherwise, if this is a constant or the object is not in memory
	   and need be, put it there.  */
	else if (CONSTANT_P (op0) || (!MEM_P (op0) && must_force_mem))
	  {
	    tree nt = build_qualified_type (TREE_TYPE (tem),
					    (TYPE_QUALS (TREE_TYPE (tem))
					     | TYPE_QUAL_CONST));
	    memloc = assign_temp (nt, 1, 1, 1);
	    emit_move_insn (memloc, op0);
	    op0 = memloc;
	  }

	if (offset)
	  {
	    enum machine_mode address_mode;
	    rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode,
					  EXPAND_SUM);

	    gcc_assert (MEM_P (op0));

	    address_mode
	      = targetm.addr_space.address_mode (MEM_ADDR_SPACE (op0));
	    if (GET_MODE (offset_rtx) != address_mode)
	      offset_rtx = convert_to_mode (address_mode, offset_rtx, 0);

	    if (GET_MODE (op0) == BLKmode
		/* A constant address in OP0 can have VOIDmode, we must
		   not try to call force_reg in that case.  */
		&& GET_MODE (XEXP (op0, 0)) != VOIDmode
		&& bitsize != 0
		&& (bitpos % bitsize) == 0
		&& (bitsize % GET_MODE_ALIGNMENT (mode1)) == 0
		&& MEM_ALIGN (op0) == GET_MODE_ALIGNMENT (mode1))
	      {
		op0 = adjust_address (op0, mode1, bitpos / BITS_PER_UNIT);
		bitpos = 0;
	      }

	    op0 = offset_address (op0, offset_rtx,
				  highest_pow2_factor (offset));
	  }

	/* If OFFSET is making OP0 more aligned than BIGGEST_ALIGNMENT,
	   record its alignment as BIGGEST_ALIGNMENT.  */
	if (MEM_P (op0) && bitpos == 0 && offset != 0
	    && is_aligning_offset (offset, tem))
	  set_mem_align (op0, BIGGEST_ALIGNMENT);

	/* Don't forget about volatility even if this is a bitfield.  */
	if (MEM_P (op0) && volatilep && ! MEM_VOLATILE_P (op0))
	  {
	    if (op0 == orig_op0)
	      op0 = copy_rtx (op0);

	    MEM_VOLATILE_P (op0) = 1;
	  }

	/* In cases where an aligned union has an unaligned object
	   as a field, we might be extracting a BLKmode value from
	   an integer-mode (e.g., SImode) object.  Handle this case
	   by doing the extract into an object as wide as the field
	   (which we know to be the width of a basic mode), then
	   storing into memory, and changing the mode to BLKmode.  */
	if (mode1 == VOIDmode
	    || REG_P (op0) || GET_CODE (op0) == SUBREG
	    || (mode1 != BLKmode && ! direct_load[(int) mode1]
		&& GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
		&& GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT
		&& modifier != EXPAND_CONST_ADDRESS
		&& modifier != EXPAND_INITIALIZER)
	    /* If the field is volatile, we always want an aligned
	       access.  */
	    || (volatilep && flag_strict_volatile_bitfields > 0)
	    /* If the field isn't aligned enough to fetch as a memref,
	       fetch it as a bit field.  */
	    || (mode1 != BLKmode
		&& (((TYPE_ALIGN (TREE_TYPE (tem)) < GET_MODE_ALIGNMENT (mode)
		      || (bitpos % GET_MODE_ALIGNMENT (mode) != 0)
		      || (MEM_P (op0)
			  && (MEM_ALIGN (op0) < GET_MODE_ALIGNMENT (mode1)
			      || (bitpos % GET_MODE_ALIGNMENT (mode1) != 0))))
		     && ((modifier == EXPAND_CONST_ADDRESS
			  || modifier == EXPAND_INITIALIZER)
			 ? STRICT_ALIGNMENT
			 : SLOW_UNALIGNED_ACCESS (mode1, MEM_ALIGN (op0))))
		    || (bitpos % BITS_PER_UNIT != 0)))
	    /* If the type and the field are a constant size and the
	       size of the type isn't the same size as the bitfield,
	       we must use bitfield operations.  */
	    || (bitsize >= 0
		&& TYPE_SIZE (TREE_TYPE (exp))
		&& TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST
		&& 0 != compare_tree_int (TYPE_SIZE (TREE_TYPE (exp)),
					  bitsize)))
	  {
	    enum machine_mode ext_mode = mode;

	    if (ext_mode == BLKmode
		&& ! (target != 0 && MEM_P (op0)
		      && MEM_P (target)
		      && bitpos % BITS_PER_UNIT == 0))
	      ext_mode = mode_for_size (bitsize, MODE_INT, 1);

	    if (ext_mode == BLKmode)
	      {
		if (target == 0)
		  target = assign_temp (type, 0, 1, 1);

		if (bitsize == 0)
		  return target;

		/* In this case, BITPOS must start at a byte boundary and
		   TARGET, if specified, must be a MEM.  */
		gcc_assert (MEM_P (op0)
			    && (!target || MEM_P (target))
			    && !(bitpos % BITS_PER_UNIT));

		emit_block_move (target,
				 adjust_address (op0, VOIDmode,
						 bitpos / BITS_PER_UNIT),
				 GEN_INT ((bitsize + BITS_PER_UNIT - 1)
					  / BITS_PER_UNIT),
				 (modifier == EXPAND_STACK_PARM
				  ? BLOCK_OP_CALL_PARM : BLOCK_OP_NORMAL));

		return target;
	      }

	    op0 = validize_mem (op0);

	    if (MEM_P (op0) && REG_P (XEXP (op0, 0)))
	      mark_reg_pointer (XEXP (op0, 0), MEM_ALIGN (op0));

	    op0 = extract_bit_field (op0, bitsize, bitpos, unsignedp, packedp,
				     (modifier == EXPAND_STACK_PARM
				      ? NULL_RTX : target),
				     ext_mode, ext_mode);

	    /* If the result is a record type and BITSIZE is narrower than
	       the mode of OP0, an integral mode, and this is a big endian
	       machine, we must put the field into the high-order bits.  */
	    if (TREE_CODE (type) == RECORD_TYPE && BYTES_BIG_ENDIAN
		&& GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
		&& bitsize < (HOST_WIDE_INT) GET_MODE_BITSIZE (GET_MODE (op0)))
	      op0 = expand_shift (LSHIFT_EXPR, GET_MODE (op0), op0,
				  size_int (GET_MODE_BITSIZE (GET_MODE (op0))
					    - bitsize),
				  op0, 1);

	    /* If the result type is BLKmode, store the data into a temporary
	       of the appropriate type, but with the mode corresponding to the
	       mode for the data we have (op0's mode).  It's tempting to make
	       this a constant type, since we know it's only being stored once,
	       but that can cause problems if we are taking the address of this
	       COMPONENT_REF because the MEM of any reference via that address
	       will have flags corresponding to the type, which will not
	       necessarily be constant.  */
	    if (mode == BLKmode)
	      {
		HOST_WIDE_INT size = GET_MODE_BITSIZE (ext_mode);
		rtx new_rtx;

		/* If the reference doesn't use the alias set of its type,
		   we cannot create the temporary using that type.  */
		if (component_uses_parent_alias_set (exp))
		  {
		    new_rtx = assign_stack_local (ext_mode, size, 0);
		    set_mem_alias_set (new_rtx, get_alias_set (exp));
		  }
		else
		  new_rtx = assign_stack_temp_for_type (ext_mode, size, 0, type);

		emit_move_insn (new_rtx, op0);
		op0 = copy_rtx (new_rtx);
		PUT_MODE (op0, BLKmode);
		set_mem_attributes (op0, exp, 1);
	      }

	    return op0;
	  }

	/* If the result is BLKmode, use that to access the object
	   now as well.  */
	if (mode == BLKmode)
	  mode1 = BLKmode;

	/* Get a reference to just this component.  */
	if (modifier == EXPAND_CONST_ADDRESS
	    || modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  op0 = adjust_address_nv (op0, mode1, bitpos / BITS_PER_UNIT);
	else
	  op0 = adjust_address (op0, mode1, bitpos / BITS_PER_UNIT);

	if (op0 == orig_op0)
	  op0 = copy_rtx (op0);

	set_mem_attributes (op0, exp, 0);
	if (REG_P (XEXP (op0, 0)))
	  mark_reg_pointer (XEXP (op0, 0), MEM_ALIGN (op0));

	MEM_VOLATILE_P (op0) |= volatilep;
	if (mode == mode1 || mode1 == BLKmode || mode1 == tmode
	    || modifier == EXPAND_CONST_ADDRESS
	    || modifier == EXPAND_INITIALIZER)
	  return op0;
	else if (target == 0)
	  target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

	convert_move (target, op0, unsignedp);
	return target;
      }

    case OBJ_TYPE_REF:
      return expand_expr (OBJ_TYPE_REF_EXPR (exp), target, tmode, modifier);

    case CALL_EXPR:
      /* All valid uses of __builtin_va_arg_pack () are removed during
	 inlining.  */
      if (CALL_EXPR_VA_ARG_PACK (exp))
	error ("%Kinvalid use of %<__builtin_va_arg_pack ()%>", exp);
      {
	tree fndecl = get_callee_fndecl (exp), attr;

	if (fndecl
	    && (attr = lookup_attribute ("error",
					 DECL_ATTRIBUTES (fndecl))) != NULL)
	  error ("%Kcall to %qs declared with attribute error: %s",
		 exp, identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 1)),
		 TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));
	if (fndecl
	    && (attr = lookup_attribute ("warning",
					 DECL_ATTRIBUTES (fndecl))) != NULL)
	  warning_at (tree_nonartificial_location (exp),
		      0, "%Kcall to %qs declared with attribute warning: %s",
		      exp, identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 1)),
		      TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));

	/* Check for a built-in function.  */
	if (fndecl && DECL_BUILT_IN (fndecl))
	  {
	    gcc_assert (DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_FRONTEND);
	    return expand_builtin (exp, target, subtarget, tmode, ignore);
	  }
      }
      return expand_call (exp, target, ignore);

    case VIEW_CONVERT_EXPR:
      op0 = NULL_RTX;

      /* If we are converting to BLKmode, try to avoid an intermediate
	 temporary by fetching an inner memory reference.  */
      if (mode == BLKmode
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST
	  && TYPE_MODE (TREE_TYPE (treeop0)) != BLKmode
	  && handled_component_p (treeop0))
      {
	enum machine_mode mode1;
	HOST_WIDE_INT bitsize, bitpos;
	tree offset;
	int unsignedp;
	int volatilep = 0;
	tree tem
	  = get_inner_reference (treeop0, &bitsize, &bitpos,
				 &offset, &mode1, &unsignedp, &volatilep,
				 true);
	rtx orig_op0;

	/* ??? We should work harder and deal with non-zero offsets.  */
	if (!offset
	    && (bitpos % BITS_PER_UNIT) == 0
	    && bitsize >= 0
	    && compare_tree_int (TYPE_SIZE (TREE_TYPE (exp)), bitsize) == 0)
	  {
	    /* See the normal_inner_ref case for the rationale.  */
	    orig_op0
	      = expand_expr (tem,
			     (TREE_CODE (TREE_TYPE (tem)) == UNION_TYPE
			      && (TREE_CODE (TYPE_SIZE (TREE_TYPE (tem)))
				  != INTEGER_CST)
			      && modifier != EXPAND_STACK_PARM
			      ? target : NULL_RTX),
			     VOIDmode,
			     (modifier == EXPAND_INITIALIZER
			      || modifier == EXPAND_CONST_ADDRESS
			      || modifier == EXPAND_STACK_PARM)
			     ? modifier : EXPAND_NORMAL);

	    if (MEM_P (orig_op0))
	      {
		op0 = orig_op0;

		/* Get a reference to just this component.  */
		if (modifier == EXPAND_CONST_ADDRESS
		    || modifier == EXPAND_SUM
		    || modifier == EXPAND_INITIALIZER)
		  op0 = adjust_address_nv (op0, mode, bitpos / BITS_PER_UNIT);
		else
		  op0 = adjust_address (op0, mode, bitpos / BITS_PER_UNIT);

		if (op0 == orig_op0)
		  op0 = copy_rtx (op0);

		set_mem_attributes (op0, treeop0, 0);
		if (REG_P (XEXP (op0, 0)))
		  mark_reg_pointer (XEXP (op0, 0), MEM_ALIGN (op0));

		MEM_VOLATILE_P (op0) |= volatilep;
	      }
	  }
      }

      if (!op0)
	op0 = expand_expr (treeop0,
			   NULL_RTX, VOIDmode, modifier);

      /* If the input and output modes are both the same, we are done.  */
      if (mode == GET_MODE (op0))
	;
      /* If neither mode is BLKmode, and both modes are the same size
	 then we can use gen_lowpart.  */
      else if (mode != BLKmode && GET_MODE (op0) != BLKmode
	       && GET_MODE_SIZE (mode) == GET_MODE_SIZE (GET_MODE (op0))
	       && !COMPLEX_MODE_P (GET_MODE (op0)))
	{
	  if (GET_CODE (op0) == SUBREG)
	    op0 = force_reg (GET_MODE (op0), op0);
	  temp = gen_lowpart_common (mode, op0);
	  if (temp)
	    op0 = temp;
	  else
	    {
	      if (!REG_P (op0) && !MEM_P (op0))
		op0 = force_reg (GET_MODE (op0), op0);
	      op0 = gen_lowpart (mode, op0);
	    }
	}
      /* If both types are integral, convert from one mode to the other.  */
      else if (INTEGRAL_TYPE_P (type) && INTEGRAL_TYPE_P (TREE_TYPE (treeop0)))
	op0 = convert_modes (mode, GET_MODE (op0), op0,
			     TYPE_UNSIGNED (TREE_TYPE (treeop0)));
      /* As a last resort, spill op0 to memory, and reload it in a
	 different mode.  */
      else if (!MEM_P (op0))
	{
	  /* If the operand is not a MEM, force it into memory.  Since we
	     are going to be changing the mode of the MEM, don't call
	     force_const_mem for constants because we don't allow pool
	     constants to change mode.  */
	  tree inner_type = TREE_TYPE (treeop0);

	  gcc_assert (!TREE_ADDRESSABLE (exp));

	  if (target == 0 || GET_MODE (target) != TYPE_MODE (inner_type))
	    target
	      = assign_stack_temp_for_type
		(TYPE_MODE (inner_type),
		 GET_MODE_SIZE (TYPE_MODE (inner_type)), 0, inner_type);

	  emit_move_insn (target, op0);
	  op0 = target;
	}

      /* At this point, OP0 is in the correct mode.  If the output type is
	 such that the operand is known to be aligned, indicate that it is.
	 Otherwise, we need only be concerned about alignment for non-BLKmode
	 results.  */
      if (MEM_P (op0))
	{
	  op0 = copy_rtx (op0);

	  if (TYPE_ALIGN_OK (type))
	    set_mem_align (op0, MAX (MEM_ALIGN (op0), TYPE_ALIGN (type)));
	  else if (STRICT_ALIGNMENT
		   && mode != BLKmode
		   && MEM_ALIGN (op0) < GET_MODE_ALIGNMENT (mode))
	    {
	      tree inner_type = TREE_TYPE (treeop0);
	      HOST_WIDE_INT temp_size
		= MAX (int_size_in_bytes (inner_type),
		       (HOST_WIDE_INT) GET_MODE_SIZE (mode));
	      rtx new_rtx
		= assign_stack_temp_for_type (mode, temp_size, 0, type);
	      rtx new_with_op0_mode
		= adjust_address (new_rtx, GET_MODE (op0), 0);

	      gcc_assert (!TREE_ADDRESSABLE (exp));

	      if (GET_MODE (op0) == BLKmode)
		emit_block_move (new_with_op0_mode, op0,
				 GEN_INT (GET_MODE_SIZE (mode)),
				 (modifier == EXPAND_STACK_PARM
				  ? BLOCK_OP_CALL_PARM : BLOCK_OP_NORMAL));
	      else
		emit_move_insn (new_with_op0_mode, op0);

	      op0 = new_rtx;
	    }

	  op0 = adjust_address (op0, mode, 0);
	}

      return op0;

      /* Use a compare and a jump for BLKmode comparisons, or for function
	 type comparisons is HAVE_canonicalize_funcptr_for_compare.  */

      /* Although TRUTH_{AND,OR}IF_EXPR aren't present in GIMPLE, they
	 are occassionally created by folding during expansion.  */
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      if (! ignore
	  && (target == 0
	      || modifier == EXPAND_STACK_PARM
	      || ! safe_from_p (target, treeop0, 1)
	      || ! safe_from_p (target, treeop1, 1)
	      /* Make sure we don't have a hard reg (such as function's return
		 value) live across basic blocks, if not optimizing.  */
	      || (!optimize && REG_P (target)
		  && REGNO (target) < FIRST_PSEUDO_REGISTER)))
	target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

      if (target)
	emit_move_insn (target, const0_rtx);

      op1 = gen_label_rtx ();
      jumpifnot_1 (code, treeop0, treeop1, op1, -1);

      if (target)
	emit_move_insn (target, const1_rtx);

      emit_label (op1);
      return ignore ? const0_rtx : target;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator iter;

	gcc_assert (ignore);

	for (iter = tsi_start (exp); !tsi_end_p (iter); tsi_next (&iter))
	  expand_expr (tsi_stmt (iter), const0_rtx, VOIDmode, modifier);
      }
      return const0_rtx;

    case COND_EXPR:
      /* A COND_EXPR with its type being VOID_TYPE represents a
	 conditional jump and is handled in
	 expand_gimple_cond_expr.  */
      gcc_assert (!VOID_TYPE_P (type));

        /* Note that COND_EXPRs whose type is a structure or union
  	 are required to be constructed to contain assignments of
  	 a temporary variable, so that we can evaluate them here
  	 for side effect only.  If type is void, we must do likewise.  */

        gcc_assert (!TREE_ADDRESSABLE (type)
		    && !ignore
		    && TREE_TYPE (treeop1) != void_type_node
		    && TREE_TYPE (treeop2) != void_type_node);

       /* If we are not to produce a result, we have no target.  Otherwise,
 	 if a target was specified use it; it will not be used as an
 	 intermediate target unless it is safe.  If no target, use a
 	 temporary.  */

       if (modifier != EXPAND_STACK_PARM
 	  && original_target
 	  && safe_from_p (original_target, treeop0, 1)
 	  && GET_MODE (original_target) == mode
#ifdef HAVE_conditional_move
 	  && (! can_conditionally_move_p (mode)
 	      || REG_P (original_target))
#endif
 	  && !MEM_P (original_target))
 	temp = original_target;
       else
 	temp = assign_temp (type, 0, 0, 1);

       do_pending_stack_adjust ();
       NO_DEFER_POP;
       op0 = gen_label_rtx ();
       op1 = gen_label_rtx ();
       jumpifnot (treeop0, op0, -1);
       store_expr (treeop1, temp,
 		  modifier == EXPAND_STACK_PARM,
		  false);

       emit_jump_insn (gen_jump (op1));
       emit_barrier ();
       emit_label (op0);
       store_expr (treeop2, temp,
 		  modifier == EXPAND_STACK_PARM,
		  false);

       emit_label (op1);
       OK_DEFER_POP;
       return temp;

    case VEC_COND_EXPR:
      target = expand_vec_cond_expr (type, treeop0, treeop1, treeop2, target);
      return target;

    case MODIFY_EXPR:
      {
	tree lhs = treeop0;
	tree rhs = treeop1;
	gcc_assert (ignore);

	/* Check for |= or &= of a bitfield of size one into another bitfield
	   of size 1.  In this case, (unless we need the result of the
	   assignment) we can do this more efficiently with a
	   test followed by an assignment, if necessary.

	   ??? At this point, we can't get a BIT_FIELD_REF here.  But if
	   things change so we do, this code should be enhanced to
	   support it.  */
	if (TREE_CODE (lhs) == COMPONENT_REF
	    && (TREE_CODE (rhs) == BIT_IOR_EXPR
		|| TREE_CODE (rhs) == BIT_AND_EXPR)
	    && TREE_OPERAND (rhs, 0) == lhs
	    && TREE_CODE (TREE_OPERAND (rhs, 1)) == COMPONENT_REF
	    && integer_onep (DECL_SIZE (TREE_OPERAND (lhs, 1)))
	    && integer_onep (DECL_SIZE (TREE_OPERAND (TREE_OPERAND (rhs, 1), 1))))
	  {
	    rtx label = gen_label_rtx ();
	    int value = TREE_CODE (rhs) == BIT_IOR_EXPR;
	    do_jump (TREE_OPERAND (rhs, 1),
		     value ? label : 0,
		     value ? 0 : label, -1);
	    expand_assignment (lhs, build_int_cst (TREE_TYPE (rhs), value),
			       MOVE_NONTEMPORAL (exp));
	    do_pending_stack_adjust ();
	    emit_label (label);
	    return const0_rtx;
	  }

	expand_assignment (lhs, rhs, MOVE_NONTEMPORAL (exp));
	return const0_rtx;
      }

    case ADDR_EXPR:
      return expand_expr_addr_expr (exp, target, tmode, modifier);

    case REALPART_EXPR:
      op0 = expand_normal (treeop0);
      return read_complex_part (op0, false);

    case IMAGPART_EXPR:
      op0 = expand_normal (treeop0);
      return read_complex_part (op0, true);

    case RETURN_EXPR:
    case LABEL_EXPR:
    case GOTO_EXPR:
    case SWITCH_EXPR:
    case ASM_EXPR:
      /* Expanded in cfgexpand.c.  */
      gcc_unreachable ();

    case TRY_CATCH_EXPR:
    case CATCH_EXPR:
    case EH_FILTER_EXPR:
    case TRY_FINALLY_EXPR:
      /* Lowered by tree-eh.c.  */
      gcc_unreachable ();

    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
    case TARGET_EXPR:
    case CASE_LABEL_EXPR:
    case VA_ARG_EXPR:
    case BIND_EXPR:
    case INIT_EXPR:
    case CONJ_EXPR:
    case COMPOUND_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case LOOP_EXPR:
    case EXIT_EXPR:
      /* Lowered by gimplify.c.  */
      gcc_unreachable ();

    case FDESC_EXPR:
      /* Function descriptors are not valid except for as
	 initialization constants, and should not be expanded.  */
      gcc_unreachable ();

    case WITH_SIZE_EXPR:
      /* WITH_SIZE_EXPR expands to its first argument.  The caller should
	 have pulled out the size to use in whatever context it needed.  */
      return expand_expr_real (treeop0, original_target, tmode,
			       modifier, alt_rtl);

    case REALIGN_LOAD_EXPR:
      {
        tree oprnd0 = treeop0;
        tree oprnd1 = treeop1;
        tree oprnd2 = treeop2;
        rtx op2;

        this_optab = optab_for_tree_code (code, type, optab_default);
        expand_operands (oprnd0, oprnd1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);
        op2 = expand_normal (oprnd2);
        temp = expand_ternary_op (mode, this_optab, op0, op1, op2,
				  target, unsignedp);
        gcc_assert (temp);
        return temp;
      }

    case DOT_PROD_EXPR:
      {
	tree oprnd0 = treeop0;
	tree oprnd1 = treeop1;
	tree oprnd2 = treeop2;
	rtx op2;

	expand_operands (oprnd0, oprnd1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);
	op2 = expand_normal (oprnd2);
	target = expand_widen_pattern_expr (&ops, op0, op1, op2,
					    target, unsignedp);
	return target;
      }

    case COMPOUND_LITERAL_EXPR:
      {
	/* Initialize the anonymous variable declared in the compound
	   literal, then return the variable.  */
	tree decl = COMPOUND_LITERAL_EXPR_DECL (exp);

	/* Create RTL for this variable.  */
	if (!DECL_RTL_SET_P (decl))
	  {
	    if (DECL_HARD_REGISTER (decl))
	      /* The user specified an assembler name for this variable.
	         Set that up now.  */
	      rest_of_decl_compilation (decl, 0, 0);
	    else
	      expand_decl (decl);
	  }

	return expand_expr_real (decl, original_target, tmode,
				 modifier, alt_rtl);
      }

    default:
      return expand_expr_real_2 (&ops, target, tmode, modifier);
    }
}

/* Subroutine of above: reduce EXP to the precision of TYPE (in the
   signedness of TYPE), possibly returning the result in TARGET.  */
static rtx
reduce_to_bit_field_precision (rtx exp, rtx target, tree type)
{
  HOST_WIDE_INT prec = TYPE_PRECISION (type);
  if (target && GET_MODE (target) != GET_MODE (exp))
    target = 0;
  /* For constant values, reduce using build_int_cst_type. */
  if (CONST_INT_P (exp))
    {
      HOST_WIDE_INT value = INTVAL (exp);
      tree t = build_int_cst_type (type, value);
      return expand_expr (t, target, VOIDmode, EXPAND_NORMAL);
    }
  else if (TYPE_UNSIGNED (type))
    {
      rtx mask = immed_double_int_const (double_int_mask (prec),
					 GET_MODE (exp));
      return expand_and (GET_MODE (exp), exp, mask, target);
    }
  else
    {
      tree count = build_int_cst (NULL_TREE,
				  GET_MODE_BITSIZE (GET_MODE (exp)) - prec);
      exp = expand_shift (LSHIFT_EXPR, GET_MODE (exp), exp, count, target, 0);
      return expand_shift (RSHIFT_EXPR, GET_MODE (exp), exp, count, target, 0);
    }
}

/* Subroutine of above: returns 1 if OFFSET corresponds to an offset that
   when applied to the address of EXP produces an address known to be
   aligned more than BIGGEST_ALIGNMENT.  */

static int
is_aligning_offset (const_tree offset, const_tree exp)
{
  /* Strip off any conversions.  */
  while (CONVERT_EXPR_P (offset))
    offset = TREE_OPERAND (offset, 0);

  /* We must now have a BIT_AND_EXPR with a constant that is one less than
     power of 2 and which is larger than BIGGEST_ALIGNMENT.  */
  if (TREE_CODE (offset) != BIT_AND_EXPR
      || !host_integerp (TREE_OPERAND (offset, 1), 1)
      || compare_tree_int (TREE_OPERAND (offset, 1),
			   BIGGEST_ALIGNMENT / BITS_PER_UNIT) <= 0
      || !exact_log2 (tree_low_cst (TREE_OPERAND (offset, 1), 1) + 1) < 0)
    return 0;

  /* Look at the first operand of BIT_AND_EXPR and strip any conversion.
     It must be NEGATE_EXPR.  Then strip any more conversions.  */
  offset = TREE_OPERAND (offset, 0);
  while (CONVERT_EXPR_P (offset))
    offset = TREE_OPERAND (offset, 0);

  if (TREE_CODE (offset) != NEGATE_EXPR)
    return 0;

  offset = TREE_OPERAND (offset, 0);
  while (CONVERT_EXPR_P (offset))
    offset = TREE_OPERAND (offset, 0);

  /* This must now be the address of EXP.  */
  return TREE_CODE (offset) == ADDR_EXPR && TREE_OPERAND (offset, 0) == exp;
}

/* Return the tree node if an ARG corresponds to a string constant or zero
   if it doesn't.  If we return nonzero, set *PTR_OFFSET to the offset
   in bytes within the string that ARG is accessing.  The type of the
   offset will be `sizetype'.  */

tree
string_constant (tree arg, tree *ptr_offset)
{
  tree array, offset, lower_bound;
  STRIP_NOPS (arg);

  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST)
	{
	  *ptr_offset = size_zero_node;
	  return TREE_OPERAND (arg, 0);
	}
      else if (TREE_CODE (TREE_OPERAND (arg, 0)) == VAR_DECL)
	{
	  array = TREE_OPERAND (arg, 0);
	  offset = size_zero_node;
	}
      else if (TREE_CODE (TREE_OPERAND (arg, 0)) == ARRAY_REF)
	{
	  array = TREE_OPERAND (TREE_OPERAND (arg, 0), 0);
	  offset = TREE_OPERAND (TREE_OPERAND (arg, 0), 1);
	  if (TREE_CODE (array) != STRING_CST
	      && TREE_CODE (array) != VAR_DECL)
	    return 0;

	  /* Check if the array has a nonzero lower bound.  */
	  lower_bound = array_ref_low_bound (TREE_OPERAND (arg, 0));
	  if (!integer_zerop (lower_bound))
	    {
	      /* If the offset and base aren't both constants, return 0.  */
	      if (TREE_CODE (lower_bound) != INTEGER_CST)
	        return 0;
	      if (TREE_CODE (offset) != INTEGER_CST)
		return 0;
	      /* Adjust offset by the lower bound.  */
	      offset = size_diffop (fold_convert (sizetype, offset),
				    fold_convert (sizetype, lower_bound));
	    }
	}
      else
	return 0;
    }
  else if (TREE_CODE (arg) == PLUS_EXPR || TREE_CODE (arg) == POINTER_PLUS_EXPR)
    {
      tree arg0 = TREE_OPERAND (arg, 0);
      tree arg1 = TREE_OPERAND (arg, 1);

      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg0) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (arg0, 0)) == STRING_CST
	      || TREE_CODE (TREE_OPERAND (arg0, 0)) == VAR_DECL))
	{
	  array = TREE_OPERAND (arg0, 0);
	  offset = arg1;
	}
      else if (TREE_CODE (arg1) == ADDR_EXPR
	       && (TREE_CODE (TREE_OPERAND (arg1, 0)) == STRING_CST
		   || TREE_CODE (TREE_OPERAND (arg1, 0)) == VAR_DECL))
	{
	  array = TREE_OPERAND (arg1, 0);
	  offset = arg0;
	}
      else
	return 0;
    }
  else
    return 0;

  if (TREE_CODE (array) == STRING_CST)
    {
      *ptr_offset = fold_convert (sizetype, offset);
      return array;
    }
  else if (TREE_CODE (array) == VAR_DECL
	   || TREE_CODE (array) == CONST_DECL)
    {
      int length;

      /* Variables initialized to string literals can be handled too.  */
      if (!const_value_known_p (array)
	  || !DECL_INITIAL (array)
	  || TREE_CODE (DECL_INITIAL (array)) != STRING_CST)
	return 0;

      /* Avoid const char foo[4] = "abcde";  */
      if (DECL_SIZE_UNIT (array) == NULL_TREE
	  || TREE_CODE (DECL_SIZE_UNIT (array)) != INTEGER_CST
	  || (length = TREE_STRING_LENGTH (DECL_INITIAL (array))) <= 0
	  || compare_tree_int (DECL_SIZE_UNIT (array), length) < 0)
	return 0;

      /* If variable is bigger than the string literal, OFFSET must be constant
	 and inside of the bounds of the string literal.  */
      offset = fold_convert (sizetype, offset);
      if (compare_tree_int (DECL_SIZE_UNIT (array), length) > 0
	  && (! host_integerp (offset, 1)
	      || compare_tree_int (offset, length) >= 0))
	return 0;

      *ptr_offset = offset;
      return DECL_INITIAL (array);
    }

  return 0;
}

/* Generate code to calculate OPS, and exploded expression
   using a store-flag instruction and return an rtx for the result.
   OPS reflects a comparison.

   If TARGET is nonzero, store the result there if convenient.

   Return zero if there is no suitable set-flag instruction
   available on this machine.

   Once expand_expr has been called on the arguments of the comparison,
   we are committed to doing the store flag, since it is not safe to
   re-evaluate the expression.  We emit the store-flag insn by calling
   emit_store_flag, but only expand the arguments if we have a reason
   to believe that emit_store_flag will be successful.  If we think that
   it will, but it isn't, we have to simulate the store-flag with a
   set/jump/set sequence.  */

static rtx
do_store_flag (sepops ops, rtx target, enum machine_mode mode)
{
  enum rtx_code code;
  tree arg0, arg1, type;
  tree tem;
  enum machine_mode operand_mode;
  int unsignedp;
  rtx op0, op1;
  rtx subtarget = target;
  location_t loc = ops->location;

  arg0 = ops->op0;
  arg1 = ops->op1;

  /* Don't crash if the comparison was erroneous.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  type = TREE_TYPE (arg0);
  operand_mode = TYPE_MODE (type);
  unsignedp = TYPE_UNSIGNED (type);

  /* We won't bother with BLKmode store-flag operations because it would mean
     passing a lot of information to emit_store_flag.  */
  if (operand_mode == BLKmode)
    return 0;

  /* We won't bother with store-flag operations involving function pointers
     when function pointers must be canonicalized before comparisons.  */
#ifdef HAVE_canonicalize_funcptr_for_compare
  if (HAVE_canonicalize_funcptr_for_compare
      && ((TREE_CODE (TREE_TYPE (arg0)) == POINTER_TYPE
	   && (TREE_CODE (TREE_TYPE (TREE_TYPE (arg0)))
	       == FUNCTION_TYPE))
	  || (TREE_CODE (TREE_TYPE (arg1)) == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (TREE_TYPE (arg1)))
		  == FUNCTION_TYPE))))
    return 0;
#endif

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  /* Get the rtx comparison code to use.  We know that EXP is a comparison
     operation of some type.  Some comparisons against 1 and -1 can be
     converted to comparisons with zero.  Do so here so that the tests
     below will be aware that we have a comparison with zero.   These
     tests will not catch constants in the first operand, but constants
     are rarely passed as the first operand.  */

  switch (ops->code)
    {
    case EQ_EXPR:
      code = EQ;
      break;
    case NE_EXPR:
      code = NE;
      break;
    case LT_EXPR:
      if (integer_onep (arg1))
	arg1 = integer_zero_node, code = unsignedp ? LEU : LE;
      else
	code = unsignedp ? LTU : LT;
      break;
    case LE_EXPR:
      if (! unsignedp && integer_all_onesp (arg1))
	arg1 = integer_zero_node, code = LT;
      else
	code = unsignedp ? LEU : LE;
      break;
    case GT_EXPR:
      if (! unsignedp && integer_all_onesp (arg1))
	arg1 = integer_zero_node, code = GE;
      else
	code = unsignedp ? GTU : GT;
      break;
    case GE_EXPR:
      if (integer_onep (arg1))
	arg1 = integer_zero_node, code = unsignedp ? GTU : GT;
      else
	code = unsignedp ? GEU : GE;
      break;

    case UNORDERED_EXPR:
      code = UNORDERED;
      break;
    case ORDERED_EXPR:
      code = ORDERED;
      break;
    case UNLT_EXPR:
      code = UNLT;
      break;
    case UNLE_EXPR:
      code = UNLE;
      break;
    case UNGT_EXPR:
      code = UNGT;
      break;
    case UNGE_EXPR:
      code = UNGE;
      break;
    case UNEQ_EXPR:
      code = UNEQ;
      break;
    case LTGT_EXPR:
      code = LTGT;
      break;

    default:
      gcc_unreachable ();
    }

  /* Put a constant second.  */
  if (TREE_CODE (arg0) == REAL_CST || TREE_CODE (arg0) == INTEGER_CST
      || TREE_CODE (arg0) == FIXED_CST)
    {
      tem = arg0; arg0 = arg1; arg1 = tem;
      code = swap_condition (code);
    }

  /* If this is an equality or inequality test of a single bit, we can
     do this by shifting the bit being tested to the low-order bit and
     masking the result with the constant 1.  If the condition was EQ,
     we xor it with 1.  This does not require an scc insn and is faster
     than an scc insn even if we have it.

     The code to make this transformation was moved into fold_single_bit_test,
     so we just call into the folder and expand its result.  */

  if ((code == NE || code == EQ)
      && TREE_CODE (arg0) == BIT_AND_EXPR && integer_zerop (arg1)
      && integer_pow2p (TREE_OPERAND (arg0, 1))
      && (TYPE_PRECISION (ops->type) != 1 || TYPE_UNSIGNED (ops->type)))
    {
      tree type = lang_hooks.types.type_for_mode (mode, unsignedp);
      return expand_expr (fold_single_bit_test (loc,
						code == NE ? NE_EXPR : EQ_EXPR,
						arg0, arg1, type),
			  target, VOIDmode, EXPAND_NORMAL);
    }

  if (! get_subtarget (target)
      || GET_MODE (subtarget) != operand_mode)
    subtarget = 0;

  expand_operands (arg0, arg1, subtarget, &op0, &op1, EXPAND_NORMAL);

  if (target == 0)
    target = gen_reg_rtx (mode);

  /* Try a cstore if possible.  */
  return emit_store_flag_force (target, code, op0, op1,
				operand_mode, unsignedp,
				(TYPE_PRECISION (ops->type) == 1
				 && !TYPE_UNSIGNED (ops->type)) ? -1 : 1);
}


/* Stubs in case we haven't got a casesi insn.  */
#ifndef HAVE_casesi
# define HAVE_casesi 0
# define gen_casesi(a, b, c, d, e) (0)
# define CODE_FOR_casesi CODE_FOR_nothing
#endif

/* Attempt to generate a casesi instruction.  Returns 1 if successful,
   0 otherwise (i.e. if there is no casesi instruction).  */
int
try_casesi (tree index_type, tree index_expr, tree minval, tree range,
	    rtx table_label ATTRIBUTE_UNUSED, rtx default_label,
	    rtx fallback_label ATTRIBUTE_UNUSED)
{
  enum machine_mode index_mode = SImode;
  int index_bits = GET_MODE_BITSIZE (index_mode);
  rtx op1, op2, index;
  enum machine_mode op_mode;

  if (! HAVE_casesi)
    return 0;

  /* Convert the index to SImode.  */
  if (GET_MODE_BITSIZE (TYPE_MODE (index_type)) > GET_MODE_BITSIZE (index_mode))
    {
      enum machine_mode omode = TYPE_MODE (index_type);
      rtx rangertx = expand_normal (range);

      /* We must handle the endpoints in the original mode.  */
      index_expr = build2 (MINUS_EXPR, index_type,
			   index_expr, minval);
      minval = integer_zero_node;
      index = expand_normal (index_expr);
      if (default_label)
        emit_cmp_and_jump_insns (rangertx, index, LTU, NULL_RTX,
				 omode, 1, default_label);
      /* Now we can safely truncate.  */
      index = convert_to_mode (index_mode, index, 0);
    }
  else
    {
      if (TYPE_MODE (index_type) != index_mode)
	{
	  index_type = lang_hooks.types.type_for_size (index_bits, 0);
	  index_expr = fold_convert (index_type, index_expr);
	}

      index = expand_normal (index_expr);
    }

  do_pending_stack_adjust ();

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[0].mode;
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[0].predicate)
      (index, op_mode))
    index = copy_to_mode_reg (op_mode, index);

  op1 = expand_normal (minval);

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[1].mode;
  op1 = convert_modes (op_mode, TYPE_MODE (TREE_TYPE (minval)),
		       op1, TYPE_UNSIGNED (TREE_TYPE (minval)));
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[1].predicate)
      (op1, op_mode))
    op1 = copy_to_mode_reg (op_mode, op1);

  op2 = expand_normal (range);

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[2].mode;
  op2 = convert_modes (op_mode, TYPE_MODE (TREE_TYPE (range)),
		       op2, TYPE_UNSIGNED (TREE_TYPE (range)));
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[2].predicate)
      (op2, op_mode))
    op2 = copy_to_mode_reg (op_mode, op2);

  emit_jump_insn (gen_casesi (index, op1, op2,
			      table_label, !default_label
					   ? fallback_label : default_label));
  return 1;
}

/* Attempt to generate a tablejump instruction; same concept.  */
#ifndef HAVE_tablejump
#define HAVE_tablejump 0
#define gen_tablejump(x, y) (0)
#endif

/* Subroutine of the next function.

   INDEX is the value being switched on, with the lowest value
   in the table already subtracted.
   MODE is its expected mode (needed if INDEX is constant).
   RANGE is the length of the jump table.
   TABLE_LABEL is a CODE_LABEL rtx for the table itself.

   DEFAULT_LABEL is a CODE_LABEL rtx to jump to if the
   index value is out of range.  */

static void
do_tablejump (rtx index, enum machine_mode mode, rtx range, rtx table_label,
	      rtx default_label)
{
  rtx temp, vector;

  if (INTVAL (range) > cfun->cfg->max_jumptable_ents)
    cfun->cfg->max_jumptable_ents = INTVAL (range);

  /* Do an unsigned comparison (in the proper mode) between the index
     expression and the value which represents the length of the range.
     Since we just finished subtracting the lower bound of the range
     from the index expression, this comparison allows us to simultaneously
     check that the original index expression value is both greater than
     or equal to the minimum value of the range and less than or equal to
     the maximum value of the range.  */

  if (default_label)
    emit_cmp_and_jump_insns (index, range, GTU, NULL_RTX, mode, 1,
			     default_label);

  /* If index is in range, it must fit in Pmode.
     Convert to Pmode so we can index with it.  */
  if (mode != Pmode)
    index = convert_to_mode (Pmode, index, 1);

  /* Don't let a MEM slip through, because then INDEX that comes
     out of PIC_CASE_VECTOR_ADDRESS won't be a valid address,
     and break_out_memory_refs will go to work on it and mess it up.  */
#ifdef PIC_CASE_VECTOR_ADDRESS
  if (flag_pic && !REG_P (index))
    index = copy_to_mode_reg (Pmode, index);
#endif

  /* ??? The only correct use of CASE_VECTOR_MODE is the one inside the
     GET_MODE_SIZE, because this indicates how large insns are.  The other
     uses should all be Pmode, because they are addresses.  This code
     could fail if addresses and insns are not the same size.  */
  index = gen_rtx_PLUS (Pmode,
			gen_rtx_MULT (Pmode, index,
				      GEN_INT (GET_MODE_SIZE (CASE_VECTOR_MODE))),
			gen_rtx_LABEL_REF (Pmode, table_label));
#ifdef PIC_CASE_VECTOR_ADDRESS
  if (flag_pic)
    index = PIC_CASE_VECTOR_ADDRESS (index);
  else
#endif
    index = memory_address (CASE_VECTOR_MODE, index);
  temp = gen_reg_rtx (CASE_VECTOR_MODE);
  vector = gen_const_mem (CASE_VECTOR_MODE, index);
  convert_move (temp, vector, 0);

  emit_jump_insn (gen_tablejump (temp, table_label));

  /* If we are generating PIC code or if the table is PC-relative, the
     table and JUMP_INSN must be adjacent, so don't output a BARRIER.  */
  if (! CASE_VECTOR_PC_RELATIVE && ! flag_pic)
    emit_barrier ();
}

int
try_tablejump (tree index_type, tree index_expr, tree minval, tree range,
	       rtx table_label, rtx default_label)
{
  rtx index;

  if (! HAVE_tablejump)
    return 0;

  index_expr = fold_build2 (MINUS_EXPR, index_type,
			    fold_convert (index_type, index_expr),
			    fold_convert (index_type, minval));
  index = expand_normal (index_expr);
  do_pending_stack_adjust ();

  do_tablejump (index, TYPE_MODE (index_type),
		convert_modes (TYPE_MODE (index_type),
			       TYPE_MODE (TREE_TYPE (range)),
			       expand_normal (range),
			       TYPE_UNSIGNED (TREE_TYPE (range))),
		table_label, default_label);
  return 1;
}

/* Return a CONST_VECTOR rtx for a VECTOR_CST tree.  */
static rtx
const_vector_from_tree (tree exp)
{
  rtvec v;
  int units, i;
  tree link, elt;
  enum machine_mode inner, mode;

  mode = TYPE_MODE (TREE_TYPE (exp));

  if (initializer_zerop (exp))
    return CONST0_RTX (mode);

  units = GET_MODE_NUNITS (mode);
  inner = GET_MODE_INNER (mode);

  v = rtvec_alloc (units);

  link = TREE_VECTOR_CST_ELTS (exp);
  for (i = 0; link; link = TREE_CHAIN (link), ++i)
    {
      elt = TREE_VALUE (link);

      if (TREE_CODE (elt) == REAL_CST)
	RTVEC_ELT (v, i) = CONST_DOUBLE_FROM_REAL_VALUE (TREE_REAL_CST (elt),
							 inner);
      else if (TREE_CODE (elt) == FIXED_CST)
	RTVEC_ELT (v, i) = CONST_FIXED_FROM_FIXED_VALUE (TREE_FIXED_CST (elt),
							 inner);
      else
	RTVEC_ELT (v, i) = immed_double_int_const (tree_to_double_int (elt),
						   inner);
    }

  /* Initialize remaining elements to 0.  */
  for (; i < units; ++i)
    RTVEC_ELT (v, i) = CONST0_RTX (inner);

  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Build a decl for a personality function given a language prefix.  */

tree
build_personality_function (const char *lang)
{
  const char *unwind_and_version;
  tree decl, type;
  char *name;

  switch (targetm.except_unwind_info (&global_options))
    {
    case UI_NONE:
      return NULL;
    case UI_SJLJ:
      unwind_and_version = "_sj0";
      break;
    case UI_DWARF2:
    case UI_TARGET:
      unwind_and_version = "_v0";
      break;
    default:
      gcc_unreachable ();
    }

  name = ACONCAT (("__", lang, "_personality", unwind_and_version, NULL));

  type = build_function_type_list (integer_type_node, integer_type_node,
				   long_long_unsigned_type_node,
				   ptr_type_node, ptr_type_node, NULL_TREE);
  decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
		     get_identifier (name), type);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* Zap the nonsensical SYMBOL_REF_DECL for this.  What we're left with
     are the flags assigned by targetm.encode_section_info.  */
  SET_SYMBOL_REF_DECL (XEXP (DECL_RTL (decl), 0), NULL);

  return decl;
}

/* Extracts the personality function of DECL and returns the corresponding
   libfunc.  */

rtx
get_personality_function (tree decl)
{
  tree personality = DECL_FUNCTION_PERSONALITY (decl);
  enum eh_personality_kind pk;

  pk = function_needs_eh_personality (DECL_STRUCT_FUNCTION (decl));
  if (pk == eh_personality_none)
    return NULL;

  if (!personality
      && pk == eh_personality_any)
    personality = lang_hooks.eh_personality ();

  if (pk == eh_personality_lang)
    gcc_assert (personality != NULL_TREE);

  return XEXP (DECL_RTL (personality), 0);
}

#include "gt-expr.h"
