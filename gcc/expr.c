/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "gvarargs.h"
#include "typeclass.h"

#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.  */

#ifdef STACK_GROWS_DOWNWARD
#ifdef PUSH_ROUNDING
#define PUSH_ARGS_REVERSED	/* If it's last to first */
#endif
#endif

#ifndef STACK_PUSH_CODE
#ifdef STACK_GROWS_DOWNWARD
#define STACK_PUSH_CODE PRE_DEC
#else
#define STACK_PUSH_CODE PRE_INC
#endif
#endif

/* Like STACK_BOUNDARY but in units of bytes, not bits.  */
#define STACK_BYTES (STACK_BOUNDARY / BITS_PER_UNIT)

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
int cse_not_expected;

/* Nonzero to generate code for all the subroutines within an
   expression before generating the upper levels of the expression.
   Nowadays this is never zero.  */
int do_preexpand_calls = 1;

/* Number of units that we should eventually pop off the stack.
   These are the arguments to function calls that have already returned.  */
int pending_stack_adjust;

/* Nonzero means stack pops must not be deferred, and deferred stack
   pops must not be output.  It is nonzero inside a function call,
   inside a conditional expression, inside a statement expression,
   and in other cases as well.  */
int inhibit_defer_pop;

/* A list of all cleanups which belong to the arguments of
   function calls being expanded by expand_call.  */
tree cleanups_this_call;

/* Nonzero means __builtin_saveregs has already been done in this function.
   The value is the pseudoreg containing the value __builtin_saveregs
   returned.  */
static rtx saveregs_value;

rtx store_expr ();
static void store_constructor ();
static rtx store_field ();
static rtx expand_builtin ();
static rtx compare ();
static rtx do_store_flag ();
static void preexpand_calls ();
static rtx expand_increment ();
static void init_queue ();

void do_pending_stack_adjust ();
static void do_jump_for_compare ();
static void do_jump_by_parts_equality ();
static void do_jump_by_parts_equality_rtx ();
static void do_jump_by_parts_greater ();

/* Record for each mode whether we can move a register directly to or
   from an object of that mode in memory.  If we can't, we won't try
   to use that mode directly when accessing a field of that mode.  */

static char direct_load[NUM_MACHINE_MODES];
static char direct_store[NUM_MACHINE_MODES];

/* MOVE_RATIO is the number of move instructions that is better than
   a block move.  */

#ifndef MOVE_RATIO
#if defined (HAVE_movstrqi) || defined (HAVE_movstrhi) || defined (HAVE_movstrsi) || defined (HAVE_movstrdi) || defined (HAVE_movstrti)
#define MOVE_RATIO 2
#else
/* A value of around 6 would minimize code size; infinity would minimize
   execution time.  */
#define MOVE_RATIO 15
#endif
#endif

/* This array records the insn_code of insns to perform block moves.  */
static enum insn_code movstr_optab[NUM_MACHINE_MODES];

/* SLOW_UNALIGNED_ACCESS is non-zero if unaligned accesses are very slow. */

#ifndef SLOW_UNALIGNED_ACCESS
#define SLOW_UNALIGNED_ACCESS 0
#endif

/* This is run once per compilation to set up which modes can be used
   directly in memory and to initialize the block move optab.  */

void
init_expr_once ()
{
  rtx insn, pat;
  enum machine_mode mode;
  /* Try indexing by frame ptr and try by stack ptr.
     It is known that on the Convex the stack ptr isn't a valid index.
     With luck, one or the other is valid on any machine.  */
  rtx mem = gen_rtx (MEM, VOIDmode, stack_pointer_rtx);
  rtx mem1 = gen_rtx (MEM, VOIDmode, frame_pointer_rtx);

  start_sequence ();
  insn = emit_insn (gen_rtx (SET, 0, 0));
  pat = PATTERN (insn);

  for (mode = VOIDmode; (int) mode < NUM_MACHINE_MODES;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      int regno;
      rtx reg;
      int num_clobbers;

      direct_load[(int) mode] = direct_store[(int) mode] = 0;
      PUT_MODE (mem, mode);
      PUT_MODE (mem1, mode);

      /* See if there is some register that can be used in this mode and
	 directly loaded or stored from memory.  */

      if (mode != VOIDmode && mode != BLKmode)
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER
	     && (direct_load[(int) mode] == 0 || direct_store[(int) mode] == 0);
	     regno++)
	  {
	    if (! HARD_REGNO_MODE_OK (regno, mode))
	      continue;

	    reg = gen_rtx (REG, mode, regno);

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

      movstr_optab[(int) mode] = CODE_FOR_nothing;
    }

  end_sequence ();

#ifdef HAVE_movstrqi
  if (HAVE_movstrqi)
    movstr_optab[(int) QImode] = CODE_FOR_movstrqi;
#endif
#ifdef HAVE_movstrhi
  if (HAVE_movstrhi)
    movstr_optab[(int) HImode] = CODE_FOR_movstrhi;
#endif
#ifdef HAVE_movstrsi
  if (HAVE_movstrsi)
    movstr_optab[(int) SImode] = CODE_FOR_movstrsi;
#endif
#ifdef HAVE_movstrdi
  if (HAVE_movstrdi)
    movstr_optab[(int) DImode] = CODE_FOR_movstrdi;
#endif
#ifdef HAVE_movstrti
  if (HAVE_movstrti)
    movstr_optab[(int) TImode] = CODE_FOR_movstrti;
#endif
}
      
/* This is run at the start of compiling a function.  */

void
init_expr ()
{
  init_queue ();

  pending_stack_adjust = 0;
  inhibit_defer_pop = 0;
  cleanups_this_call = 0;
  saveregs_value = 0;
  forced_labels = 0;
}

/* Save all variables describing the current status into the structure *P.
   This is used before starting a nested function.  */

void
save_expr_status (p)
     struct function *p;
{
  /* Instead of saving the postincrement queue, empty it.  */
  emit_queue ();

  p->pending_stack_adjust = pending_stack_adjust;
  p->inhibit_defer_pop = inhibit_defer_pop;
  p->cleanups_this_call = cleanups_this_call;
  p->saveregs_value = saveregs_value;
  p->forced_labels = forced_labels;

  pending_stack_adjust = 0;
  inhibit_defer_pop = 0;
  cleanups_this_call = 0;
  saveregs_value = 0;
  forced_labels = 0;
}

/* Restore all variables describing the current status from the structure *P.
   This is used after a nested function.  */

void
restore_expr_status (p)
     struct function *p;
{
  pending_stack_adjust = p->pending_stack_adjust;
  inhibit_defer_pop = p->inhibit_defer_pop;
  cleanups_this_call = p->cleanups_this_call;
  saveregs_value = p->saveregs_value;
  forced_labels = p->forced_labels;
}

/* Manage the queue of increment instructions to be output
   for POSTINCREMENT_EXPR expressions, etc.  */

static rtx pending_chain;

/* Queue up to increment (or change) VAR later.  BODY says how:
   BODY should be the same thing you would pass to emit_insn
   to increment right away.  It will go to emit_insn later on.

   The value is a QUEUED expression to be used in place of VAR
   where you want to guarantee the pre-incrementation value of VAR.  */

static rtx
enqueue_insn (var, body)
     rtx var, body;
{
  pending_chain = gen_rtx (QUEUED, GET_MODE (var),
			   var, NULL_RTX, NULL_RTX, body, pending_chain);
  return pending_chain;
}

/* Use protect_from_queue to convert a QUEUED expression
   into something that you can put immediately into an instruction.
   If the queued incrementation has not happened yet,
   protect_from_queue returns the variable itself.
   If the incrementation has happened, protect_from_queue returns a temp
   that contains a copy of the old value of the variable.

   Any time an rtx which might possibly be a QUEUED is to be put
   into an instruction, it must be passed through protect_from_queue first.
   QUEUED expressions are not meaningful in instructions.

   Do not pass a value through protect_from_queue and then hold
   on to it for a while before putting it in an instruction!
   If the queue is flushed in between, incorrect code will result.  */

rtx
protect_from_queue (x, modify)
     register rtx x;
     int modify;
{
  register RTX_CODE code = GET_CODE (x);

#if 0  /* A QUEUED can hang around after the queue is forced out.  */
  /* Shortcut for most common case.  */
  if (pending_chain == 0)
    return x;
#endif

  if (code != QUEUED)
    {
      /* A special hack for read access to (MEM (QUEUED ...))
	 to facilitate use of autoincrement.
	 Make a copy of the contents of the memory location
	 rather than a copy of the address, but not
	 if the value is of mode BLKmode.  */
      if (code == MEM && GET_MODE (x) != BLKmode
	  && GET_CODE (XEXP (x, 0)) == QUEUED && !modify)
	{
	  register rtx y = XEXP (x, 0);
	  XEXP (x, 0) = QUEUED_VAR (y);
	  if (QUEUED_INSN (y))
	    {
	      register rtx temp = gen_reg_rtx (GET_MODE (x));
	      emit_insn_before (gen_move_insn (temp, x),
				QUEUED_INSN (y));
	      return temp;
	    }
	  return x;
	}
      /* Otherwise, recursively protect the subexpressions of all
	 the kinds of rtx's that can contain a QUEUED.  */
      if (code == MEM)
	XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
      else if (code == PLUS || code == MULT)
	{
	  XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
	  XEXP (x, 1) = protect_from_queue (XEXP (x, 1), 0);
	}
      return x;
    }
  /* If the increment has not happened, use the variable itself.  */
  if (QUEUED_INSN (x) == 0)
    return QUEUED_VAR (x);
  /* If the increment has happened and a pre-increment copy exists,
     use that copy.  */
  if (QUEUED_COPY (x) != 0)
    return QUEUED_COPY (x);
  /* The increment has happened but we haven't set up a pre-increment copy.
     Set one up now, and use it.  */
  QUEUED_COPY (x) = gen_reg_rtx (GET_MODE (QUEUED_VAR (x)));
  emit_insn_before (gen_move_insn (QUEUED_COPY (x), QUEUED_VAR (x)),
		    QUEUED_INSN (x));
  return QUEUED_COPY (x);
}

/* Return nonzero if X contains a QUEUED expression:
   if it contains anything that will be altered by a queued increment.
   We handle only combinations of MEM, PLUS, MINUS and MULT operators
   since memory addresses generally contain only those.  */

static int
queued_subexp_p (x)
     rtx x;
{
  register enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case QUEUED:
      return 1;
    case MEM:
      return queued_subexp_p (XEXP (x, 0));
    case MULT:
    case PLUS:
    case MINUS:
      return queued_subexp_p (XEXP (x, 0))
	|| queued_subexp_p (XEXP (x, 1));
    }
  return 0;
}

/* Perform all the pending incrementations.  */

void
emit_queue ()
{
  register rtx p;
  while (p = pending_chain)
    {
      QUEUED_INSN (p) = emit_insn (QUEUED_BODY (p));
      pending_chain = QUEUED_NEXT (p);
    }
}

static void
init_queue ()
{
  if (pending_chain)
    abort ();
}

/* Copy data from FROM to TO, where the machine modes are not the same.
   Both modes may be integer, or both may be floating.
   UNSIGNEDP should be nonzero if FROM is an unsigned type.
   This causes zero-extension instead of sign-extension.  */

void
convert_move (to, from, unsignedp)
     register rtx to, from;
     int unsignedp;
{
  enum machine_mode to_mode = GET_MODE (to);
  enum machine_mode from_mode = GET_MODE (from);
  int to_real = GET_MODE_CLASS (to_mode) == MODE_FLOAT;
  int from_real = GET_MODE_CLASS (from_mode) == MODE_FLOAT;
  enum insn_code code;
  rtx libcall;

  /* rtx code for making an equivalent value.  */
  enum rtx_code equiv_code = (unsignedp ? ZERO_EXTEND : SIGN_EXTEND);

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (to_real != from_real)
    abort ();

  /* If FROM is a SUBREG that indicates that we have already done at least
     the required extension, strip it.  We don't handle such SUBREGs as
     TO here.  */

  if (GET_CODE (from) == SUBREG && SUBREG_PROMOTED_VAR_P (from)
      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (from)))
	  >= GET_MODE_SIZE (to_mode))
      && SUBREG_PROMOTED_UNSIGNED_P (from) == unsignedp)
    from = gen_lowpart (to_mode, from), from_mode = to_mode;

  if (GET_CODE (to) == SUBREG && SUBREG_PROMOTED_VAR_P (to))
    abort ();

  if (to_mode == from_mode
      || (from_mode == VOIDmode && CONSTANT_P (from)))
    {
      emit_move_insn (to, from);
      return;
    }

  if (to_real)
    {
#ifdef HAVE_extendsfdf2
      if (HAVE_extendsfdf2 && from_mode == SFmode && to_mode == DFmode)
	{
	  emit_unop_insn (CODE_FOR_extendsfdf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_extendsfxf2
      if (HAVE_extendsfxf2 && from_mode == SFmode && to_mode == XFmode)
	{
	  emit_unop_insn (CODE_FOR_extendsfxf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_extendsftf2
      if (HAVE_extendsftf2 && from_mode == SFmode && to_mode == TFmode)
	{
	  emit_unop_insn (CODE_FOR_extendsftf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_extenddfxf2
      if (HAVE_extenddfxf2 && from_mode == DFmode && to_mode == XFmode)
	{
	  emit_unop_insn (CODE_FOR_extenddfxf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_extenddftf2
      if (HAVE_extenddftf2 && from_mode == DFmode && to_mode == TFmode)
	{
	  emit_unop_insn (CODE_FOR_extenddftf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncdfsf2
      if (HAVE_truncdfsf2 && from_mode == DFmode && to_mode == SFmode)
	{
	  emit_unop_insn (CODE_FOR_truncdfsf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncxfsf2
      if (HAVE_truncxfsf2 && from_mode == XFmode && to_mode == SFmode)
	{
	  emit_unop_insn (CODE_FOR_truncxfsf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctfsf2
      if (HAVE_trunctfsf2 && from_mode == TFmode && to_mode == SFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctfsf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncxfdf2
      if (HAVE_truncxfdf2 && from_mode == XFmode && to_mode == DFmode)
	{
	  emit_unop_insn (CODE_FOR_truncxfdf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctfdf2
      if (HAVE_trunctfdf2 && from_mode == TFmode && to_mode == DFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctfdf2, to, from, UNKNOWN);
	  return;
	}
#endif

      libcall = (rtx) 0;
      switch (from_mode)
	{
	case SFmode:
	  switch (to_mode)
	    {
	    case DFmode:
	      libcall = extendsfdf2_libfunc;
	      break;

	    case XFmode:
	      libcall = extendsfxf2_libfunc;
	      break;

	    case TFmode:
	      libcall = extendsftf2_libfunc;
	      break;
	    }
	  break;

	case DFmode:
	  switch (to_mode)
	    {
	    case SFmode:
	      libcall = truncdfsf2_libfunc;
	      break;

	    case XFmode:
	      libcall = extenddfxf2_libfunc;
	      break;

	    case TFmode:
	      libcall = extenddftf2_libfunc;
	      break;
	    }
	  break;

	case XFmode:
	  switch (to_mode)
	    {
	    case SFmode:
	      libcall = truncxfsf2_libfunc;
	      break;

	    case DFmode:
	      libcall = truncxfdf2_libfunc;
	      break;
	    }
	  break;

	case TFmode:
	  switch (to_mode)
	    {
	    case SFmode:
	      libcall = trunctfsf2_libfunc;
	      break;

	    case DFmode:
	      libcall = trunctfdf2_libfunc;
	      break;
	    }
	  break;
	}

      if (libcall == (rtx) 0)
	/* This conversion is not implemented yet.  */
	abort ();

      emit_library_call (libcall, 1, to_mode, 1, from, from_mode);
      emit_move_insn (to, hard_libcall_value (to_mode));
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
	  convert_move (gen_lowpart (word_mode, to), from, unsignedp);
	  emit_unop_insn (code, to,
			  gen_lowpart (word_mode, to), equiv_code);
	  return;
	}

      /* No special multiword conversion insn; do it by hand.  */
      start_sequence ();

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
	{
#ifdef HAVE_slt
	  if (HAVE_slt
	      && insn_operand_mode[(int) CODE_FOR_slt][0] == word_mode
	      && STORE_FLAG_VALUE == -1)
	    {
	      emit_cmp_insn (lowfrom, const0_rtx, NE, NULL_RTX,
			     lowpart_mode, 0, 0);
	      fill_value = gen_reg_rtx (word_mode);
	      emit_insn (gen_slt (fill_value));
	    }
	  else
#endif
	    {
	      fill_value
		= expand_shift (RSHIFT_EXPR, lowpart_mode, lowfrom,
				size_int (GET_MODE_BITSIZE (lowpart_mode) - 1),
				NULL_RTX, 0);
	      fill_value = convert_to_mode (word_mode, fill_value, 1);
	    }
	}

      /* Fill the remaining words.  */
      for (i = GET_MODE_SIZE (lowpart_mode) / UNITS_PER_WORD; i < nwords; i++)
	{
	  int index = (WORDS_BIG_ENDIAN ? nwords - i - 1 : i);
	  rtx subword = operand_subword (to, index, 1, to_mode);

	  if (subword == 0)
	    abort ();

	  if (fill_value != subword)
	    emit_move_insn (subword, fill_value);
	}

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, to, from, NULL_RTX,
			      gen_rtx (equiv_code, to_mode, from));
      return;
    }

  if (GET_MODE_BITSIZE (from_mode) > BITS_PER_WORD)
    {
      convert_move (to, gen_lowpart (word_mode, from), 0);
      return;
    }

  /* Handle pointer conversion */			/* SPEE 900220 */
  if (to_mode == PSImode)
    {
      if (from_mode != SImode)
	from = convert_to_mode (SImode, from, unsignedp);

#ifdef HAVE_truncsipsi
      if (HAVE_truncsipsi)
	{
	  emit_unop_insn (CODE_FOR_truncsipsi, to, from, UNKNOWN);
	  return;
	}
#endif /* HAVE_truncsipsi */
      abort ();
    }

  if (from_mode == PSImode)
    {
      if (to_mode != SImode)
	{
	  from = convert_to_mode (SImode, from, unsignedp);
	  from_mode = SImode;
	}
      else
	{
#ifdef HAVE_extendpsisi
	  if (HAVE_extendpsisi)
	    {
	      emit_unop_insn (CODE_FOR_extendpsisi, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_extendpsisi */
	  abort ();
	}
    }

  /* Now follow all the conversions between integers
     no more than a word long.  */

  /* For truncation, usually we can just refer to FROM in a narrower mode.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode)
      && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (to_mode),
				GET_MODE_BITSIZE (from_mode))
      && ((GET_CODE (from) == MEM
	   && ! MEM_VOLATILE_P (from)
	   && direct_load[(int) to_mode]
	   && ! mode_dependent_address_p (XEXP (from, 0)))
	  || GET_CODE (from) == REG
	  || GET_CODE (from) == SUBREG))
    {
      emit_move_insn (to, gen_lowpart (to_mode, from));
      return;
    }

  /* For truncation, usually we can just refer to FROM in a narrower mode.  */
  if (GET_MODE_BITSIZE (to_mode) > GET_MODE_BITSIZE (from_mode))
    {
      /* Convert directly if that works.  */
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
      else
	{
	  enum machine_mode intermediate;

	  /* Search for a mode to convert via.  */
	  for (intermediate = from_mode; intermediate != VOIDmode;
	       intermediate = GET_MODE_WIDER_MODE (intermediate))
	    if ((can_extend_p (to_mode, intermediate, unsignedp)
		 != CODE_FOR_nothing)
		&& (can_extend_p (intermediate, from_mode, unsignedp)
		    != CODE_FOR_nothing))
	      {
		convert_move (to, convert_to_mode (intermediate, from,
						   unsignedp), unsignedp);
		return;
	      }

	  /* No suitable intermediate mode.  */
	  abort ();
	}
    }

  /* Support special truncate insns for certain modes.  */ 

  if (from_mode == DImode && to_mode == SImode)
    {
#ifdef HAVE_truncdisi2
      if (HAVE_truncdisi2)
	{
	  emit_unop_insn (CODE_FOR_truncdisi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == DImode && to_mode == HImode)
    {
#ifdef HAVE_truncdihi2
      if (HAVE_truncdihi2)
	{
	  emit_unop_insn (CODE_FOR_truncdihi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == DImode && to_mode == QImode)
    {
#ifdef HAVE_truncdiqi2
      if (HAVE_truncdiqi2)
	{
	  emit_unop_insn (CODE_FOR_truncdiqi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == SImode && to_mode == HImode)
    {
#ifdef HAVE_truncsihi2
      if (HAVE_truncsihi2)
	{
	  emit_unop_insn (CODE_FOR_truncsihi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == SImode && to_mode == QImode)
    {
#ifdef HAVE_truncsiqi2
      if (HAVE_truncsiqi2)
	{
	  emit_unop_insn (CODE_FOR_truncsiqi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == HImode && to_mode == QImode)
    {
#ifdef HAVE_trunchiqi2
      if (HAVE_trunchiqi2)
	{
	  emit_unop_insn (CODE_FOR_trunchiqi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  /* Handle truncation of volatile memrefs, and so on;
     the things that couldn't be truncated directly,
     and for which there was no special instruction.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode))
    {
      rtx temp = force_reg (to_mode, gen_lowpart (to_mode, from));
      emit_move_insn (to, temp);
      return;
    }

  /* Mode combination is not recognized.  */
  abort ();
}

/* Return an rtx for a value that would result
   from converting X to mode MODE.
   Both X and MODE may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.
   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.

   This function *must not* call protect_from_queue
   except when putting X into an insn (in which case convert_move does it).  */

rtx
convert_to_mode (mode, x, unsignedp)
     enum machine_mode mode;
     rtx x;
     int unsignedp;
{
  register rtx temp;
 
  /* If FROM is a SUBREG that indicates that we have already done at least
     the required extension, strip it.  */

  if (GET_CODE (x) == SUBREG && SUBREG_PROMOTED_VAR_P (x)
      && GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))) >= GET_MODE_SIZE (mode)
      && SUBREG_PROMOTED_UNSIGNED_P (x) == unsignedp)
    x = gen_lowpart (mode, x);

  if (mode == GET_MODE (x))
    return x;

  /* There is one case that we must handle specially: If we are converting
     a CONST_INT into a mode whose size is twice HOST_BITS_PER_WIDE_INT and
     we are to interpret the constant as unsigned, gen_lowpart will do
     the wrong if the constant appears negative.  What we want to do is
     make the high-order word of the constant zero, not all ones.  */

  if (unsignedp && GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_BITSIZE (mode) == 2 * HOST_BITS_PER_WIDE_INT
      && GET_CODE (x) == CONST_INT && INTVAL (x) < 0)
    return immed_double_const (INTVAL (x), (HOST_WIDE_INT) 0, mode);

  /* We can do this with a gen_lowpart if both desired and current modes
     are integer, and this is either a constant integer, a register, or a
     non-volatile MEM.  Except for the constant case, we must be narrowing
     the operand.  */

  if (GET_CODE (x) == CONST_INT
      || (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_CLASS (GET_MODE (x)) == MODE_INT
	  && (GET_CODE (x) == CONST_DOUBLE
	      || (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (GET_MODE (x))
		  && ((GET_CODE (x) == MEM && ! MEM_VOLATILE_P (x))
		      && direct_load[(int) mode]
		      || GET_CODE (x) == REG)))))
    return gen_lowpart (mode, x);

  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

/* Generate several move instructions to copy LEN bytes
   from block FROM to block TO.  (These are MEM rtx's with BLKmode).
   The caller must pass FROM and TO
    through protect_from_queue before calling.
   ALIGN (in bytes) is maximum alignment we can assume.  */

struct move_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  rtx from;
  rtx from_addr;
  int autinc_from;
  int explicit_inc_from;
  int len;
  int offset;
  int reverse;
};

static void move_by_pieces_1 ();
static int move_by_pieces_ninsns ();

static void
move_by_pieces (to, from, len, align)
     rtx to, from;
     int len, align;
{
  struct move_by_pieces data;
  rtx to_addr = XEXP (to, 0), from_addr = XEXP (from, 0);
  int max_size = MOVE_MAX + 1;

  data.offset = 0;
  data.to_addr = to_addr;
  data.from_addr = from_addr;
  data.to = to;
  data.from = from;
  data.autinc_to
    = (GET_CODE (to_addr) == PRE_INC || GET_CODE (to_addr) == PRE_DEC
       || GET_CODE (to_addr) == POST_INC || GET_CODE (to_addr) == POST_DEC);
  data.autinc_from
    = (GET_CODE (from_addr) == PRE_INC || GET_CODE (from_addr) == PRE_DEC
       || GET_CODE (from_addr) == POST_INC
       || GET_CODE (from_addr) == POST_DEC);

  data.explicit_inc_from = 0;
  data.explicit_inc_to = 0;
  data.reverse
    = (GET_CODE (to_addr) == PRE_DEC || GET_CODE (to_addr) == POST_DEC);
  if (data.reverse) data.offset = len;
  data.len = len;

  /* If copying requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!(data.autinc_from && data.autinc_to)
      && move_by_pieces_ninsns (len, align) > 2)
    {
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (plus_constant (from_addr, len));
	  data.autinc_from = 1;
	  data.explicit_inc_from = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (from_addr);
	  data.autinc_from = 1;
	  data.explicit_inc_from = 1;
	}
#endif
      if (!data.autinc_from && CONSTANT_P (from_addr))
	data.from_addr = copy_addr_to_reg (from_addr);
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (plus_constant (to_addr, len));
	  data.autinc_to = 1;
	  data.explicit_inc_to = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (to_addr);
	  data.autinc_to = 1;
	  data.explicit_inc_to = 1;
	}
#endif
      if (!data.autinc_to && CONSTANT_P (to_addr))
	data.to_addr = copy_addr_to_reg (to_addr);
    }

  if (! (STRICT_ALIGNMENT || SLOW_UNALIGNED_ACCESS)
      || align > MOVE_MAX || align >= BIGGEST_ALIGNMENT / BITS_PER_UNIT)
    align = MOVE_MAX;

  /* First move what we can in the largest integer mode, then go to
     successively smaller modes.  */

  while (max_size > 1)
    {
      enum machine_mode mode = VOIDmode, tmode;
      enum insn_code icode;

      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (mode == VOIDmode)
	break;

      icode = mov_optab->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing
	  && align >= MIN (BIGGEST_ALIGNMENT / BITS_PER_UNIT,
			   GET_MODE_SIZE (mode)))
	move_by_pieces_1 (GEN_FCN (icode), mode, &data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  if (data.len != 0)
    abort ();
}

/* Return number of insns required to move L bytes by pieces.
   ALIGN (in bytes) is maximum alignment we can assume.  */

static int
move_by_pieces_ninsns (l, align)
     unsigned int l;
     int align;
{
  register int n_insns = 0;
  int max_size = MOVE_MAX + 1;

  if (! (STRICT_ALIGNMENT || SLOW_UNALIGNED_ACCESS)
      || align > MOVE_MAX || align >= BIGGEST_ALIGNMENT / BITS_PER_UNIT)
    align = MOVE_MAX;

  while (max_size > 1)
    {
      enum machine_mode mode = VOIDmode, tmode;
      enum insn_code icode;

      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (mode == VOIDmode)
	break;

      icode = mov_optab->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing
	  && align >= MIN (BIGGEST_ALIGNMENT / BITS_PER_UNIT,
			   GET_MODE_SIZE (mode)))
	n_insns += l / GET_MODE_SIZE (mode), l %= GET_MODE_SIZE (mode);

      max_size = GET_MODE_SIZE (mode);
    }

  return n_insns;
}

/* Subroutine of move_by_pieces.  Move as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
move_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) ();
     enum machine_mode mode;
     struct move_by_pieces *data;
{
  register int size = GET_MODE_SIZE (mode);
  register rtx to1, from1;

  while (data->len >= size)
    {
      if (data->reverse) data->offset -= size;

      to1 = (data->autinc_to
	     ? gen_rtx (MEM, mode, data->to_addr)
	     : change_address (data->to, mode,
			       plus_constant (data->to_addr, data->offset)));
      from1 =
	(data->autinc_from
	 ? gen_rtx (MEM, mode, data->from_addr)
	 : change_address (data->from, mode,
			   plus_constant (data->from_addr, data->offset)));

#ifdef HAVE_PRE_DECREMENT
      if (data->explicit_inc_to < 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (-size)));
      if (data->explicit_inc_from < 0)
	emit_insn (gen_add2_insn (data->from_addr, GEN_INT (-size)));
#endif

      emit_insn ((*genfun) (to1, from1));
#ifdef HAVE_POST_INCREMENT
      if (data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (size)));
      if (data->explicit_inc_from > 0)
	emit_insn (gen_add2_insn (data->from_addr, GEN_INT (size)));
#endif

      if (! data->reverse) data->offset += size;

      data->len -= size;
    }
}

/* Emit code to move a block Y to a block X.
   This may be done with string-move instructions,
   with multiple scalar move instructions, or with a library call.

   Both X and Y must be MEM rtx's (perhaps inside VOLATILE)
   with mode BLKmode.
   SIZE is an rtx that says how long they are.
   ALIGN is the maximum alignment we can assume they have,
   measured in bytes.  */

void
emit_block_move (x, y, size, align)
     rtx x, y;
     rtx size;
     int align;
{
  if (GET_MODE (x) != BLKmode)
    abort ();

  if (GET_MODE (y) != BLKmode)
    abort ();

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);
  size = protect_from_queue (size, 0);

  if (GET_CODE (x) != MEM)
    abort ();
  if (GET_CODE (y) != MEM)
    abort ();
  if (size == 0)
    abort ();

  if (GET_CODE (size) == CONST_INT
      && (move_by_pieces_ninsns (INTVAL (size), align) < MOVE_RATIO))
    move_by_pieces (x, y, INTVAL (size), align);
  else
    {
      /* Try the most limited insn first, because there's no point
	 including more than one in the machine description unless
	 the more limited one has some advantage.  */

      rtx opalign = GEN_INT (align);
      enum machine_mode mode;

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	{
	  enum insn_code code = movstr_optab[(int) mode];

	  if (code != CODE_FOR_nothing
	      /* We don't need MODE to be narrower than BITS_PER_HOST_WIDE_INT
		 here because if SIZE is less than the mode mask, as it is
		 returned by the macro, it will definately be less than the
		 actual mode mask.  */
	      && (unsigned) INTVAL (size) <= GET_MODE_MASK (mode)
	      && (insn_operand_predicate[(int) code][0] == 0
		  || (*insn_operand_predicate[(int) code][0]) (x, BLKmode))
	      && (insn_operand_predicate[(int) code][1] == 0
		  || (*insn_operand_predicate[(int) code][1]) (y, BLKmode))
	      && (insn_operand_predicate[(int) code][3] == 0
		  || (*insn_operand_predicate[(int) code][3]) (opalign,
							       VOIDmode)))
	    {
	      rtx op2;
	      rtx last = get_last_insn ();
	      rtx pat;

	      op2 = convert_to_mode (mode, size, 1);
	      if (insn_operand_predicate[(int) code][2] != 0
		  && ! (*insn_operand_predicate[(int) code][2]) (op2, mode))
		op2 = copy_to_mode_reg (mode, op2);

	      pat = GEN_FCN ((int) code) (x, y, op2, opalign);
	      if (pat)
		{
		  emit_insn (pat);
		  return;
		}
	      else
		delete_insns_since (last);
	    }
	}

#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (memcpy_libfunc, 0,
			 VOIDmode, 3, XEXP (x, 0), Pmode,
			 XEXP (y, 0), Pmode,
			 convert_to_mode (Pmode, size, 1), Pmode);
#else
      emit_library_call (bcopy_libfunc, 0,
			 VOIDmode, 3, XEXP (y, 0), Pmode,
			 XEXP (x, 0), Pmode,
			 convert_to_mode (Pmode, size, 1), Pmode);
#endif
    }
}

/* Copy all or part of a value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

void
move_block_to_reg (regno, x, nregs, mode)
     int regno;
     rtx x;
     int nregs;
     enum machine_mode mode;
{
  int i;
  rtx pat, last;

  if (CONSTANT_P (x) && ! LEGITIMATE_CONSTANT_P (x))
    x = validize_mem (force_const_mem (mode, x));

  /* See if the machine can do this with a load multiple insn.  */
#ifdef HAVE_load_multiple
  last = get_last_insn ();
  pat = gen_load_multiple (gen_rtx (REG, word_mode, regno), x,
			   GEN_INT (nregs));
  if (pat)
    {
      emit_insn (pat);
      return;
    }
  else
    delete_insns_since (last);
#endif

  for (i = 0; i < nregs; i++)
    emit_move_insn (gen_rtx (REG, word_mode, regno + i),
		    operand_subword_force (x, i, mode));
}

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

void
move_block_from_reg (regno, x, nregs)
     int regno;
     rtx x;
     int nregs;
{
  int i;
  rtx pat, last;

  /* See if the machine can do this with a store multiple insn.  */
#ifdef HAVE_store_multiple
  last = get_last_insn ();
  pat = gen_store_multiple (x, gen_rtx (REG, word_mode, regno),
			    GEN_INT (nregs));
  if (pat)
    {
      emit_insn (pat);
      return;
    }
  else
    delete_insns_since (last);
#endif

  for (i = 0; i < nregs; i++)
    {
      rtx tem = operand_subword (x, i, 1, BLKmode);

      if (tem == 0)
	abort ();

      emit_move_insn (tem, gen_rtx (REG, word_mode, regno + i));
    }
}

/* Mark NREGS consecutive regs, starting at REGNO, as being live now.  */

void
use_regs (regno, nregs)
     int regno;
     int nregs;
{
  int i;

  for (i = 0; i < nregs; i++)
    emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, word_mode, regno + i)));
}

/* Mark the instructions since PREV as a libcall block.
   Add REG_LIBCALL to PREV and add a REG_RETVAL to the most recent insn.  */

static rtx
group_insns (prev)
     rtx prev;
{
  rtx insn_first;
  rtx insn_last;

  /* Find the instructions to mark */
  if (prev)
    insn_first = NEXT_INSN (prev);
  else
    insn_first = get_insns ();

  insn_last = get_last_insn ();

  REG_NOTES (insn_last) = gen_rtx (INSN_LIST, REG_RETVAL, insn_first,
				   REG_NOTES (insn_last));

  REG_NOTES (insn_first) = gen_rtx (INSN_LIST, REG_LIBCALL, insn_last,
				    REG_NOTES (insn_first));
}

/* Write zeros through the storage of OBJECT.
   If OBJECT has BLKmode, SIZE is its length in bytes.  */

void
clear_storage (object, size)
     rtx object;
     int size;
{
  if (GET_MODE (object) == BLKmode)
    {
#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (memset_libfunc, 0,
			 VOIDmode, 3,
			 XEXP (object, 0), Pmode, const0_rtx, Pmode,
			 GEN_INT (size), Pmode);
#else
      emit_library_call (bzero_libfunc, 0,
			 VOIDmode, 2,
			 XEXP (object, 0), Pmode,
			 GEN_INT (size), Pmode);
#endif
    }
  else
    emit_move_insn (object, const0_rtx);
}

/* Generate code to copy Y into X.
   Both Y and X must have the same mode, except that
   Y can be a constant with VOIDmode.
   This mode cannot be BLKmode; use emit_block_move for that.

   Return the last instruction emitted.  */

rtx
emit_move_insn (x, y)
     rtx x, y;
{
  enum machine_mode mode = GET_MODE (x);
  enum machine_mode submode;
  enum mode_class class = GET_MODE_CLASS (mode);
  int i;

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (mode == BLKmode || (GET_MODE (y) != mode && GET_MODE (y) != VOIDmode))
    abort ();

  if (CONSTANT_P (y) && ! LEGITIMATE_CONSTANT_P (y))
    y = force_const_mem (mode, y);

  /* If X or Y are memory references, verify that their addresses are valid
     for the machine.  */
  if (GET_CODE (x) == MEM
      && ((! memory_address_p (GET_MODE (x), XEXP (x, 0))
	   && ! push_operand (x, GET_MODE (x)))
	  || (flag_force_addr
	      && CONSTANT_ADDRESS_P (XEXP (x, 0)))))
    x = change_address (x, VOIDmode, XEXP (x, 0));

  if (GET_CODE (y) == MEM
      && (! memory_address_p (GET_MODE (y), XEXP (y, 0))
	  || (flag_force_addr
	      && CONSTANT_ADDRESS_P (XEXP (y, 0)))))
    y = change_address (y, VOIDmode, XEXP (y, 0));

  if (mode == BLKmode)
    abort ();

  if (class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
    submode = mode_for_size (GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT,
			     (class == MODE_COMPLEX_INT
			      ? MODE_INT : MODE_FLOAT),
			     0);

  if (mov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    return
      emit_insn (GEN_FCN (mov_optab->handlers[(int) mode].insn_code) (x, y));

  /* Expand complex moves by moving real part and imag part, if posible.  */
  else if ((class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
	   && submode != BLKmode
	   && (mov_optab->handlers[(int) submode].insn_code
	       != CODE_FOR_nothing))
    {
      /* Don't split destination if it is a stack push.  */
      int stack = push_operand (x, GET_MODE (x));
      rtx prev = get_last_insn ();

      /* Tell flow that the whole of the destination is being set.  */
      if (GET_CODE (x) == REG)
	emit_insn (gen_rtx (CLOBBER, VOIDmode, x));

      /* If this is a stack, push the highpart first, so it
	 will be in the argument order.

	 In that case, change_address is used only to convert
	 the mode, not to change the address.  */
      emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		 ((stack ? change_address (x, submode, (rtx) 0)
		   : gen_highpart (submode, x)),
		  gen_highpart (submode, y)));
      emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		 ((stack ? change_address (x, submode, (rtx) 0)
		   : gen_lowpart (submode, x)),
		  gen_lowpart (submode, y)));

      group_insns (prev);

      return get_last_insn ();
    }

  /* This will handle any multi-word mode that lacks a move_insn pattern.
     However, you will get better code if you define such patterns,
     even if they must turn into multiple assembler instructions.  */
  else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    {
      rtx last_insn = 0;
      rtx prev_insn = get_last_insn ();

      for (i = 0;
	   i < (GET_MODE_SIZE (mode)  + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
	   i++)
	{
	  rtx xpart = operand_subword (x, i, 1, mode);
	  rtx ypart = operand_subword (y, i, 1, mode);

	  /* If we can't get a part of Y, put Y into memory if it is a
	     constant.  Otherwise, force it into a register.  If we still
	     can't get a part of Y, abort.  */
	  if (ypart == 0 && CONSTANT_P (y))
	    {
	      y = force_const_mem (mode, y);
	      ypart = operand_subword (y, i, 1, mode);
	    }
	  else if (ypart == 0)
	    ypart = operand_subword_force (y, i, mode);

	  if (xpart == 0 || ypart == 0)
	    abort ();

	  last_insn = emit_move_insn (xpart, ypart);
	}
      /* Mark these insns as a libcall block.  */
      group_insns (prev_insn);

      return last_insn;
    }
  else
    abort ();
}

/* Pushing data onto the stack.  */

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.
   Note that it is not possible for the value returned to be a QUEUED.
   The value may be virtual_outgoing_args_rtx.

   EXTRA is the number of bytes of padding to push in addition to SIZE.
   BELOW nonzero means this padding comes at low addresses;
   otherwise, the padding comes at high addresses.  */

rtx
push_block (size, extra, below)
     rtx size;
     int extra, below;
{
  register rtx temp;
  if (CONSTANT_P (size))
    anti_adjust_stack (plus_constant (size, extra));
  else if (GET_CODE (size) == REG && extra == 0)
    anti_adjust_stack (size);
  else
    {
      rtx temp = copy_to_mode_reg (Pmode, size);
      if (extra != 0)
	temp = expand_binop (Pmode, add_optab, temp, GEN_INT (extra),
			     temp, 0, OPTAB_LIB_WIDEN);
      anti_adjust_stack (temp);
    }

#ifdef STACK_GROWS_DOWNWARD
  temp = virtual_outgoing_args_rtx;
  if (extra != 0 && below)
    temp = plus_constant (temp, extra);
#else
  if (GET_CODE (size) == CONST_INT)
    temp = plus_constant (virtual_outgoing_args_rtx,
			  - INTVAL (size) - (below ? 0 : extra));
  else if (extra != 0 && !below)
    temp = gen_rtx (PLUS, Pmode, virtual_outgoing_args_rtx,
		    negate_rtx (Pmode, plus_constant (size, extra)));
  else
    temp = gen_rtx (PLUS, Pmode, virtual_outgoing_args_rtx,
		    negate_rtx (Pmode, size));
#endif

  return memory_address (GET_CLASS_NARROWEST_MODE (MODE_INT), temp);
}

rtx
gen_push_operand ()
{
  return gen_rtx (STACK_PUSH_CODE, Pmode, stack_pointer_rtx);
}

/* Generate code to push X onto the stack, assuming it has mode MODE and
   type TYPE.
   MODE is redundant except when X is a CONST_INT (since they don't
   carry mode info).
   SIZE is an rtx for the size of data to be copied (in bytes),
   needed only if X is BLKmode.

   ALIGN (in bytes) is maximum alignment we can assume.

   If PARTIAL is nonzero, then copy that many of the first words
   of X into registers starting with REG, and push the rest of X.
   The amount of space pushed is decreased by PARTIAL words,
   rounded *down* to a multiple of PARM_BOUNDARY.
   REG must be a hard register in this case.

   EXTRA is the amount in bytes of extra space to leave next to this arg.
   This is ignored if an argument block has already been allocated.

   On a machine that lacks real push insns, ARGS_ADDR is the address of
   the bottom of the argument block for this call.  We use indexing off there
   to store the arg.  On machines with push insns, ARGS_ADDR is 0 when a
   argument block has not been preallocated.

   ARGS_SO_FAR is the size of args previously pushed for this call.  */

void
emit_push_insn (x, mode, type, size, align, partial, reg, extra,
		args_addr, args_so_far)
     register rtx x;
     enum machine_mode mode;
     tree type;
     rtx size;
     int align;
     int partial;
     rtx reg;
     int extra;
     rtx args_addr;
     rtx args_so_far;
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

  /* Invert direction if stack is post-update.  */
  if (STACK_PUSH_CODE == POST_INC || STACK_PUSH_CODE == POST_DEC)
    if (where_pad != none)
      where_pad = (where_pad == downward ? upward : downward);

  xinner = x = protect_from_queue (x, 0);

  if (mode == BLKmode)
    {
      /* Copy a block into the stack, entirely or partially.  */

      register rtx temp;
      int used = partial * UNITS_PER_WORD;
      int offset = used % (PARM_BOUNDARY / BITS_PER_UNIT);
      int skip;
      
      if (size == 0)
	abort ();

      used -= offset;

      /* USED is now the # of bytes we need not copy to the stack
	 because registers will take care of them.  */

      if (partial != 0)
	xinner = change_address (xinner, BLKmode,
				 plus_constant (XEXP (xinner, 0), used));

      /* If the partial register-part of the arg counts in its stack size,
	 skip the part of stack space corresponding to the registers.
	 Otherwise, start copying to the beginning of the stack space,
	 by setting SKIP to 0.  */
#ifndef REG_PARM_STACK_SPACE
      skip = 0;
#else
      skip = used;
#endif

#ifdef PUSH_ROUNDING
      /* Do it with several push insns if that doesn't take lots of insns
	 and if there is no difficulty with push insns that skip bytes
	 on the stack for alignment purposes.  */
      if (args_addr == 0
	  && GET_CODE (size) == CONST_INT
	  && skip == 0
	  && (move_by_pieces_ninsns ((unsigned) INTVAL (size) - used, align)
	      < MOVE_RATIO)
	  /* Here we avoid the case of a structure whose weak alignment
	     forces many pushes of a small amount of data,
	     and such small pushes do rounding that causes trouble.  */
	  && ((! STRICT_ALIGNMENT && ! SLOW_UNALIGNED_ACCESS)
	      || align >= BIGGEST_ALIGNMENT / BITS_PER_UNIT
	      || PUSH_ROUNDING (align) == align)
	  && PUSH_ROUNDING (INTVAL (size)) == INTVAL (size))
	{
	  /* Push padding now if padding above and stack grows down,
	     or if padding below and stack grows up.
	     But if space already allocated, this has already been done.  */
	  if (extra && args_addr == 0
	      && where_pad != none && where_pad != stack_direction)
	    anti_adjust_stack (GEN_INT (extra));

	  move_by_pieces (gen_rtx (MEM, BLKmode, gen_push_operand ()), xinner,
			  INTVAL (size) - used, align);
	}
      else
#endif /* PUSH_ROUNDING */
	{
	  /* Otherwise make space on the stack and copy the data
	     to the address of that space.  */

	  /* Deduct words put into registers from the size we must copy.  */
	  if (partial != 0)
	    {
	      if (GET_CODE (size) == CONST_INT)
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
	  else if (GET_CODE (args_so_far) == CONST_INT)
	    temp = memory_address (BLKmode,
				   plus_constant (args_addr,
						  skip + INTVAL (args_so_far)));
	  else
	    temp = memory_address (BLKmode,
				   plus_constant (gen_rtx (PLUS, Pmode,
							   args_addr, args_so_far),
						  skip));

	  /* TEMP is the address of the block.  Copy the data there.  */
	  if (GET_CODE (size) == CONST_INT
	      && (move_by_pieces_ninsns ((unsigned) INTVAL (size), align)
		  < MOVE_RATIO))
	    {
	      move_by_pieces (gen_rtx (MEM, BLKmode, temp), xinner,
			      INTVAL (size), align);
	      goto ret;
	    }
	  /* Try the most limited insn first, because there's no point
	     including more than one in the machine description unless
	     the more limited one has some advantage.  */
#ifdef HAVE_movstrqi
	  if (HAVE_movstrqi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_BITSIZE (QImode) - 1))))
	    {
	      emit_insn (gen_movstrqi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size, GEN_INT (align)));
	      goto ret;
	    }
#endif
#ifdef HAVE_movstrhi
	  if (HAVE_movstrhi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_BITSIZE (HImode) - 1))))
	    {
	      emit_insn (gen_movstrhi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size, GEN_INT (align)));
	      goto ret;
	    }
#endif
#ifdef HAVE_movstrsi
	  if (HAVE_movstrsi)
	    {
	      emit_insn (gen_movstrsi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size, GEN_INT (align)));
	      goto ret;
	    }
#endif
#ifdef HAVE_movstrdi
	  if (HAVE_movstrdi)
	    {
	      emit_insn (gen_movstrdi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size, GEN_INT (align)));
	      goto ret;
	    }
#endif

#ifndef ACCUMULATE_OUTGOING_ARGS
	  /* If the source is referenced relative to the stack pointer,
	     copy it to another register to stabilize it.  We do not need
	     to do this if we know that we won't be changing sp.  */

	  if (reg_mentioned_p (virtual_stack_dynamic_rtx, temp)
	      || reg_mentioned_p (virtual_outgoing_args_rtx, temp))
	    temp = copy_to_reg (temp);
#endif

	  /* Make inhibit_defer_pop nonzero around the library call
	     to force it to pop the bcopy-arguments right away.  */
	  NO_DEFER_POP;
#ifdef TARGET_MEM_FUNCTIONS
	  emit_library_call (memcpy_libfunc, 0,
			     VOIDmode, 3, temp, Pmode, XEXP (xinner, 0), Pmode,
			     size, Pmode);
#else
	  emit_library_call (bcopy_libfunc, 0,
			     VOIDmode, 3, XEXP (xinner, 0), Pmode, temp, Pmode,
			     size, Pmode);
#endif
	  OK_DEFER_POP;
	}
    }
  else if (partial > 0)
    {
      /* Scalar partly in registers.  */

      int size = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      int i;
      int not_stack;
      /* # words of start of argument
	 that we must make space for but need not store.  */
      int offset = partial % (PARM_BOUNDARY / BITS_PER_WORD);
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
	 allocate on the stack.  */
      not_stack = partial - offset;

      /* If the partial register-part of the arg counts in its stack size,
	 skip the part of stack space corresponding to the registers.
	 Otherwise, start copying to the beginning of the stack space,
	 by setting SKIP to 0.  */
#ifndef REG_PARM_STACK_SPACE
      skip = 0;
#else
      skip = not_stack;
#endif

      if (CONSTANT_P (x) && ! LEGITIMATE_CONSTANT_P (x))
	x = validize_mem (force_const_mem (mode, x));

      /* If X is a hard register in a non-integer mode, copy it into a pseudo;
	 SUBREGs of such registers are not allowed.  */
      if ((GET_CODE (x) == REG && REGNO (x) < FIRST_PSEUDO_REGISTER
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
						  * UNITS_PER_WORD)));
    }
  else
    {
      rtx addr;

      /* Push padding now if padding above and stack grows down,
	 or if padding below and stack grows up.
	 But if space already allocated, this has already been done.  */
      if (extra && args_addr == 0
	  && where_pad != none && where_pad != stack_direction)
	anti_adjust_stack (GEN_INT (extra));

#ifdef PUSH_ROUNDING
      if (args_addr == 0)
	addr = gen_push_operand ();
      else
#endif
	if (GET_CODE (args_so_far) == CONST_INT)
	  addr
	    = memory_address (mode,
			      plus_constant (args_addr, INTVAL (args_so_far)));
      else
	addr = memory_address (mode, gen_rtx (PLUS, Pmode, args_addr,
					      args_so_far));

      emit_move_insn (gen_rtx (MEM, mode, addr), x);
    }

 ret:
  /* If part should go in registers, copy that part
     into the appropriate registers.  Do this now, at the end,
     since mem-to-mem copies above may do function calls.  */
  if (partial > 0)
    move_block_to_reg (REGNO (reg), x, partial, mode);

  if (extra && args_addr == 0 && where_pad == stack_direction)
    anti_adjust_stack (GEN_INT (extra));
}

/* Output a library call to function FUN (a SYMBOL_REF rtx)
   (emitting the queue unless NO_QUEUE is nonzero),
   for a value of mode OUTMODE,
   with NARGS different arguments, passed as alternating rtx values
   and machine_modes to convert them to.
   The rtx values should have been passed through protect_from_queue already.

   NO_QUEUE will be true if and only if the library call is a `const' call
   which will be enclosed in REG_LIBCALL/REG_RETVAL notes; it is equivalent
   to the variable is_const in expand_call.

   NO_QUEUE must be true for const calls, because if it isn't, then
   any pending increment will be emitted between REG_LIBCALL/REG_RETVAL notes,
   and will be lost if the libcall sequence is optimized away.

   NO_QUEUE must be false for non-const calls, because if it isn't, the
   call insn will have its CONST_CALL_P bit set, and it will be incorrectly
   optimized.  For instance, the instruction scheduler may incorrectly
   move memory references across the non-const call.  */

void
emit_library_call (va_alist)
     va_dcl
{
  va_list p;
  struct args_size args_size;
  register int argnum;
  enum machine_mode outmode;
  int nargs;
  rtx fun;
  rtx orgfun;
  int inc;
  int count;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far;
  struct arg { rtx value; enum machine_mode mode; rtx reg; int partial;
	       struct args_size offset; struct args_size size; };
  struct arg *argvec;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  int no_queue = 0;
  rtx use_insns;

  va_start (p);
  orgfun = fun = va_arg (p, rtx);
  no_queue = va_arg (p, int);
  outmode = va_arg (p, enum machine_mode);
  nargs = va_arg (p, int);

  /* Copy all the libcall-arguments out of the varargs data
     and into a vector ARGVEC.

     Compute how to pass each argument.  We only support a very small subset
     of the full argument passing conventions to limit complexity here since
     library functions shouldn't have many args.  */

  argvec = (struct arg *) alloca (nargs * sizeof (struct arg));

  INIT_CUMULATIVE_ARGS (args_so_far, (tree)0, fun);

  args_size.constant = 0;
  args_size.var = 0;

  for (count = 0; count < nargs; count++)
    {
      rtx val = va_arg (p, rtx);
      enum machine_mode mode = va_arg (p, enum machine_mode);

      /* We cannot convert the arg value to the mode the library wants here;
	 must do it earlier where we know the signedness of the arg.  */
      if (mode == BLKmode
	  || (GET_MODE (val) != mode && GET_MODE (val) != VOIDmode))
	abort ();

      /* On some machines, there's no way to pass a float to a library fcn.
	 Pass it as a double instead.  */
#ifdef LIBGCC_NEEDS_DOUBLE
      if (LIBGCC_NEEDS_DOUBLE && mode == SFmode)
	val = convert_to_mode (DFmode, val, 0), mode = DFmode;
#endif

      /* There's no need to call protect_from_queue, because
	 either emit_move_insn or emit_push_insn will do that.  */

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (GET_CODE (val) != REG && GET_CODE (val) != MEM
	  && ! (CONSTANT_P (val) && LEGITIMATE_CONSTANT_P (val)))
	val = force_operand (val, NULL_RTX);

      argvec[count].value = val;
      argvec[count].mode = mode;

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, mode, NULL_TREE, 1))
	abort ();
#endif

      argvec[count].reg = FUNCTION_ARG (args_so_far, mode, NULL_TREE, 1);
      if (argvec[count].reg && GET_CODE (argvec[count].reg) == EXPR_LIST)
	abort ();
#ifdef FUNCTION_ARG_PARTIAL_NREGS
      argvec[count].partial
	= FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, NULL_TREE, 1);
#else
      argvec[count].partial = 0;
#endif

      locate_and_pad_parm (mode, NULL_TREE,
			   argvec[count].reg && argvec[count].partial == 0,
			   NULL_TREE, &args_size, &argvec[count].offset,
			   &argvec[count].size);

      if (argvec[count].size.var)
	abort ();

#ifndef REG_PARM_STACK_SPACE
      if (argvec[count].partial)
	argvec[count].size.constant -= argvec[count].partial * UNITS_PER_WORD;
#endif

      if (argvec[count].reg == 0 || argvec[count].partial != 0
#ifdef REG_PARM_STACK_SPACE
	  || 1
#endif
	  )
	args_size.constant += argvec[count].size.constant;

#ifdef ACCUMULATE_OUTGOING_ARGS
      /* If this arg is actually passed on the stack, it might be
	 clobbering something we already put there (this library call might
	 be inside the evaluation of an argument to a function whose call
	 requires the stack).  This will only occur when the library call
	 has sufficient args to run out of argument registers.  Abort in
	 this case; if this ever occurs, code must be added to save and
	 restore the arg slot.  */

      if (argvec[count].reg == 0 || argvec[count].partial != 0)
	abort ();
#endif

      FUNCTION_ARG_ADVANCE (args_so_far, mode, (tree)0, 1);
    }
  va_end (p);

  /* If this machine requires an external definition for library
     functions, write one out.  */
  assemble_external_libcall (fun);

#ifdef STACK_BOUNDARY
  args_size.constant = (((args_size.constant + (STACK_BYTES - 1))
			 / STACK_BYTES) * STACK_BYTES);
#endif

#ifdef REG_PARM_STACK_SPACE
  args_size.constant = MAX (args_size.constant,
			    REG_PARM_STACK_SPACE ((tree) 0));
#endif

#ifdef ACCUMULATE_OUTGOING_ARGS
  if (args_size.constant > current_function_outgoing_args_size)
    current_function_outgoing_args_size = args_size.constant;
  args_size.constant = 0;
#endif

#ifndef PUSH_ROUNDING
  argblock = push_block (GEN_INT (args_size.constant), 0, 0);
#endif

#ifdef PUSH_ARGS_REVERSED
  inc = -1;
  argnum = nargs - 1;
#else
  inc = 1;
  argnum = 0;
#endif

  /* Push the args that need to be pushed.  */

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (! (reg != 0 && partial == 0))
	emit_push_insn (val, mode, NULL_TREE, NULL_RTX, 0, partial, reg, 0,
			argblock, GEN_INT (argvec[count].offset.constant));
      NO_DEFER_POP;
    }

#ifdef PUSH_ARGS_REVERSED
  argnum = nargs - 1;
#else
  argnum = 0;
#endif

  /* Now load any reg parms into their regs.  */

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (reg != 0 && partial == 0)
	emit_move_insn (reg, val);
      NO_DEFER_POP;
    }

  /* For version 1.37, try deleting this entirely.  */
  if (! no_queue)
    emit_queue ();

  /* Any regs containing parms remain in use through the call.  */
  start_sequence ();
  for (count = 0; count < nargs; count++)
    if (argvec[count].reg != 0)
      emit_insn (gen_rtx (USE, VOIDmode, argvec[count].reg));

  use_insns = get_insns ();
  end_sequence ();

  fun = prepare_call_address (fun, NULL_TREE, &use_insns);

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;

  /* We pass the old value of inhibit_defer_pop + 1 to emit_call_1, which
     will set inhibit_defer_pop to that value.  */

  emit_call_1 (fun, get_identifier (XSTR (orgfun, 0)), args_size.constant, 0,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       outmode != VOIDmode ? hard_libcall_value (outmode) : NULL_RTX,
	       old_inhibit_defer_pop + 1, use_insns, no_queue);

  /* Now restore inhibit_defer_pop to its actual original value.  */
  OK_DEFER_POP;
}

/* Expand an assignment that stores the value of FROM into TO.
   If WANT_VALUE is nonzero, return an rtx for the value of TO.
   (This may contain a QUEUED rtx.)
   Otherwise, the returned value is not meaningful.

   SUGGEST_REG is no longer actually used.
   It used to mean, copy the value through a register
   and return that register, if that is possible.
   But now we do this if WANT_VALUE.

   If the value stored is a constant, we return the constant.  */

rtx
expand_assignment (to, from, want_value, suggest_reg)
     tree to, from;
     int want_value;
     int suggest_reg;
{
  register rtx to_rtx = 0;
  rtx result;

  /* Don't crash if the lhs of the assignment was erroneous.  */

  if (TREE_CODE (to) == ERROR_MARK)
    return expand_expr (from, NULL_RTX, VOIDmode, 0);

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.
     Assignment of an array element at a constant index
     has the same problem.  */

  if (TREE_CODE (to) == COMPONENT_REF
      || TREE_CODE (to) == BIT_FIELD_REF
      || (TREE_CODE (to) == ARRAY_REF
	  && TREE_CODE (TREE_OPERAND (to, 1)) == INTEGER_CST
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (to))) == INTEGER_CST))
    {
      enum machine_mode mode1;
      int bitsize;
      int bitpos;
      tree offset;
      int unsignedp;
      int volatilep = 0;
      tree tem = get_inner_reference (to, &bitsize, &bitpos, &offset,
				      &mode1, &unsignedp, &volatilep);

      /* If we are going to use store_bit_field and extract_bit_field,
	 make sure to_rtx will be safe for multiple use.  */

      if (mode1 == VOIDmode && want_value)
	tem = stabilize_reference (tem);

      to_rtx = expand_expr (tem, NULL_RTX, VOIDmode, 0);
      if (offset != 0)
	{
	  rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, 0);

	  if (GET_CODE (to_rtx) != MEM)
	    abort ();
	  to_rtx = change_address (to_rtx, VOIDmode,
				   gen_rtx (PLUS, Pmode, XEXP (to_rtx, 0),
					    force_reg (Pmode, offset_rtx)));
	}
      if (volatilep)
	{
	  if (GET_CODE (to_rtx) == MEM)
	    MEM_VOLATILE_P (to_rtx) = 1;
#if 0  /* This was turned off because, when a field is volatile
	  in an object which is not volatile, the object may be in a register,
	  and then we would abort over here.  */
	  else
	    abort ();
#endif
	}

      result = store_field (to_rtx, bitsize, bitpos, mode1, from,
			    (want_value
			     /* Spurious cast makes HPUX compiler happy.  */
			     ? (enum machine_mode) TYPE_MODE (TREE_TYPE (to))
			     : VOIDmode),
			    unsignedp,
			    /* Required alignment of containing datum.  */
			    TYPE_ALIGN (TREE_TYPE (tem)) / BITS_PER_UNIT,
			    int_size_in_bytes (TREE_TYPE (tem)));
      preserve_temp_slots (result);
      free_temp_slots ();

      return result;
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, NULL_RTX, VOIDmode, 0);

  /* In case we are returning the contents of an object which overlaps
     the place the value is being stored, use a safe function when copying
     a value through a pointer into a structure value return block.  */
  if (TREE_CODE (to) == RESULT_DECL && TREE_CODE (from) == INDIRECT_REF
      && current_function_returns_struct
      && !current_function_returns_pcc_struct)
    {
      rtx from_rtx = expand_expr (from, NULL_RTX, VOIDmode, 0);
      rtx size = expr_size (from);

#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (memcpy_libfunc, 0,
			 VOIDmode, 3, XEXP (to_rtx, 0), Pmode,
			 XEXP (from_rtx, 0), Pmode,
			 size, Pmode);
#else
      emit_library_call (bcopy_libfunc, 0,
			 VOIDmode, 3, XEXP (from_rtx, 0), Pmode,
			 XEXP (to_rtx, 0), Pmode,
			 size, Pmode);
#endif

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      return to_rtx;
    }

  /* Compute FROM and store the value in the rtx we got.  */

  result = store_expr (from, to_rtx, want_value);
  preserve_temp_slots (result);
  free_temp_slots ();
  return result;
}

/* Generate code for computing expression EXP,
   and storing the value into TARGET.
   Returns TARGET or an equivalent value.
   TARGET may contain a QUEUED rtx.

   If SUGGEST_REG is nonzero, copy the value through a register
   and return that register, if that is possible.

   If the value stored is a constant, we return the constant.  */

rtx
store_expr (exp, target, suggest_reg)
     register tree exp;
     register rtx target;
     int suggest_reg;
{
  register rtx temp;
  int dont_return_target = 0;

  if (TREE_CODE (exp) == COMPOUND_EXPR)
    {
      /* Perform first part of compound expression, then assign from second
	 part.  */
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      return store_expr (TREE_OPERAND (exp, 1), target, suggest_reg);
    }
  else if (TREE_CODE (exp) == COND_EXPR && GET_MODE (target) == BLKmode)
    {
      /* For conditional expression, get safe form of the target.  Then
	 test the condition, doing the appropriate assignment on either
	 side.  This avoids the creation of unnecessary temporaries.
	 For non-BLKmode, it is more efficient not to do this.  */

      rtx lab1 = gen_label_rtx (), lab2 = gen_label_rtx ();

      emit_queue ();
      target = protect_from_queue (target, 1);

      NO_DEFER_POP;
      jumpifnot (TREE_OPERAND (exp, 0), lab1);
      store_expr (TREE_OPERAND (exp, 1), target, suggest_reg);
      emit_queue ();
      emit_jump_insn (gen_jump (lab2));
      emit_barrier ();
      emit_label (lab1);
      store_expr (TREE_OPERAND (exp, 2), target, suggest_reg);
      emit_queue ();
      emit_label (lab2);
      OK_DEFER_POP;
      return target;
    }
  else if (suggest_reg && GET_CODE (target) == MEM
	   && GET_MODE (target) != BLKmode)
    /* If target is in memory and caller wants value in a register instead,
       arrange that.  Pass TARGET as target for expand_expr so that,
       if EXP is another assignment, SUGGEST_REG will be nonzero for it.
       We know expand_expr will not use the target in that case.  */
    {
      temp = expand_expr (exp, cse_not_expected ? NULL_RTX : target,
			  GET_MODE (target), 0);
      if (GET_MODE (temp) != BLKmode && GET_MODE (temp) != VOIDmode)
	temp = copy_to_reg (temp);
      dont_return_target = 1;
    }
  else if (queued_subexp_p (target))
    /* If target contains a postincrement, it is not safe
       to use as the returned value.  It would access the wrong
       place by the time the queued increment gets output.
       So copy the value through a temporary and use that temp
       as the result.  */
    {
      if (GET_MODE (target) != BLKmode && GET_MODE (target) != VOIDmode)
	{
	  /* Expand EXP into a new pseudo.  */
	  temp = gen_reg_rtx (GET_MODE (target));
	  temp = expand_expr (exp, temp, GET_MODE (target), 0);
	}
      else
	temp = expand_expr (exp, NULL_RTX, GET_MODE (target), 0);
      dont_return_target = 1;
    }
  else if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is an scalar in a register that is stored in a wider mode
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    {
      temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);
      convert_move (SUBREG_REG (target), temp,
		    SUBREG_PROMOTED_UNSIGNED_P (target));
      return temp;
    }
  else
    {
      temp = expand_expr (exp, target, GET_MODE (target), 0);
      /* DO return TARGET if it's a specified hardware register.
	 expand_return relies on this.  */
      if (!(target && GET_CODE (target) == REG
	    && REGNO (target) < FIRST_PSEUDO_REGISTER)
	  && CONSTANT_P (temp))
	dont_return_target = 1;
    }

  /* If value was not generated in the target, store it there.
     Convert the value to TARGET's type first if nec.  */

  if (temp != target && TREE_CODE (exp) != ERROR_MARK)
    {
      target = protect_from_queue (target, 1);
      if (GET_MODE (temp) != GET_MODE (target)
	  && GET_MODE (temp) != VOIDmode)
	{
	  int unsignedp = TREE_UNSIGNED (TREE_TYPE (exp));
	  if (dont_return_target)
	    {
	      /* In this case, we will return TEMP,
		 so make sure it has the proper mode.
		 But don't forget to store the value into TARGET.  */
	      temp = convert_to_mode (GET_MODE (target), temp, unsignedp);
	      emit_move_insn (target, temp);
	    }
	  else
	    convert_move (target, temp, unsignedp);
	}

      else if (GET_MODE (temp) == BLKmode && TREE_CODE (exp) == STRING_CST)
	{
	  /* Handle copying a string constant into an array.
	     The string constant may be shorter than the array.
	     So copy just the string's actual length, and clear the rest.  */
	  rtx size;

	  /* Get the size of the data type of the string,
	     which is actually the size of the target.  */
	  size = expr_size (exp);
	  if (GET_CODE (size) == CONST_INT
	      && INTVAL (size) < TREE_STRING_LENGTH (exp))
	    emit_block_move (target, temp, size,
			     TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
	  else
	    {
	      /* Compute the size of the data to copy from the string.  */
	      tree copy_size
		= fold (build (MIN_EXPR, sizetype,
			       size_binop (CEIL_DIV_EXPR,
					   TYPE_SIZE (TREE_TYPE (exp)),
					   size_int (BITS_PER_UNIT)),
			       convert (sizetype,
					build_int_2 (TREE_STRING_LENGTH (exp), 0))));
	      rtx copy_size_rtx = expand_expr (copy_size, NULL_RTX,
					       VOIDmode, 0);
	      rtx label = 0;

	      /* Copy that much.  */
	      emit_block_move (target, temp, copy_size_rtx,
			       TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);

	      /* Figure out how much is left in TARGET
		 that we have to clear.  */
	      if (GET_CODE (copy_size_rtx) == CONST_INT)
		{
		  temp = plus_constant (XEXP (target, 0),
					TREE_STRING_LENGTH (exp));
		  size = plus_constant (size,
					- TREE_STRING_LENGTH (exp));
		}
	      else
		{
		  enum machine_mode size_mode = Pmode;

		  temp = force_reg (Pmode, XEXP (target, 0));
		  temp = expand_binop (size_mode, add_optab, temp,
				       copy_size_rtx, NULL_RTX, 0,
				       OPTAB_LIB_WIDEN);

		  size = expand_binop (size_mode, sub_optab, size,
				       copy_size_rtx, NULL_RTX, 0,
				       OPTAB_LIB_WIDEN);

		  emit_cmp_insn (size, const0_rtx, LT, NULL_RTX,
				 GET_MODE (size), 0, 0);
		  label = gen_label_rtx ();
		  emit_jump_insn (gen_blt (label));
		}

	      if (size != const0_rtx)
		{
#ifdef TARGET_MEM_FUNCTIONS
		  emit_library_call (memset_libfunc, 0, VOIDmode, 3,
				     temp, Pmode, const0_rtx, Pmode, size, Pmode);
#else
		  emit_library_call (bzero_libfunc, 0, VOIDmode, 2,
				     temp, Pmode, size, Pmode);
#endif
		}
	      if (label)
		emit_label (label);
	    }
	}
      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
      else
	emit_move_insn (target, temp);
    }
  if (dont_return_target)
    return temp;
  return target;
}

/* Store the value of constructor EXP into the rtx TARGET.
   TARGET is either a REG or a MEM.  */

static void
store_constructor (exp, target)
     tree exp;
     rtx target;
{
  tree type = TREE_TYPE (exp);

  /* We know our target cannot conflict, since safe_from_p has been called.  */
#if 0
  /* Don't try copying piece by piece into a hard register
     since that is vulnerable to being clobbered by EXP.
     Instead, construct in a pseudo register and then copy it all.  */
  if (GET_CODE (target) == REG && REGNO (target) < FIRST_PSEUDO_REGISTER)
    {
      rtx temp = gen_reg_rtx (GET_MODE (target));
      store_constructor (exp, temp);
      emit_move_insn (target, temp);
      return;
    }
#endif

  if (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE)
    {
      register tree elt;

      /* Inform later passes that the whole union value is dead.  */
      if (TREE_CODE (type) == UNION_TYPE)
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* If we are building a static constructor into a register,
	 set the initial value as zero so we can fold the value into
	 a constant.  */
      else if (GET_CODE (target) == REG && TREE_STATIC (exp))
	emit_move_insn (target, const0_rtx);

      /* If the constructor has fewer fields than the structure,
	 clear the whole structure first.  */
      else if (list_length (CONSTRUCTOR_ELTS (exp))
	       != list_length (TYPE_FIELDS (type)))
	clear_storage (target, int_size_in_bytes (type));
      else
	/* Inform later passes that the old value is dead.  */
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding field of TARGET.  */

      for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	{
	  register tree field = TREE_PURPOSE (elt);
	  register enum machine_mode mode;
	  int bitsize;
	  int bitpos;
	  int unsignedp;

	  /* Just ignore missing fields.
	     We cleared the whole structure, above,
	     if any fields are missing.  */
	  if (field == 0)
	    continue;

	  bitsize = TREE_INT_CST_LOW (DECL_SIZE (field));
	  unsignedp = TREE_UNSIGNED (field);
	  mode = DECL_MODE (field);
	  if (DECL_BIT_FIELD (field))
	    mode = VOIDmode;

	  if (TREE_CODE (DECL_FIELD_BITPOS (field)) != INTEGER_CST)
	    /* ??? This case remains to be written.  */
	    abort ();

	  bitpos = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));

	  store_field (target, bitsize, bitpos, mode, TREE_VALUE (elt),
		       /* The alignment of TARGET is
			  at least what its type requires.  */
		       VOIDmode, 0,
		       TYPE_ALIGN (type) / BITS_PER_UNIT,
		       int_size_in_bytes (type));
	}
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      register tree elt;
      register int i;
      tree domain = TYPE_DOMAIN (type);
      HOST_WIDE_INT minelt = TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain));
      HOST_WIDE_INT maxelt = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain));
      tree elttype = TREE_TYPE (type);

      /* If the constructor has fewer fields than the structure,
	 clear the whole structure first.  Similarly if this this is
	 static constructor of a non-BLKmode object.  */

      if (list_length (CONSTRUCTOR_ELTS (exp)) < maxelt - minelt + 1
	  || (GET_CODE (target) == REG && TREE_STATIC (exp)))
	clear_storage (target, maxelt - minelt + 1);
      else
	/* Inform later passes that the old value is dead.  */
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding element of TARGET, determined
	 by counting the elements.  */
      for (elt = CONSTRUCTOR_ELTS (exp), i = 0;
	   elt;
	   elt = TREE_CHAIN (elt), i++)
	{
	  register enum machine_mode mode;
	  int bitsize;
	  int bitpos;
	  int unsignedp;

	  mode = TYPE_MODE (elttype);
	  bitsize = GET_MODE_BITSIZE (mode);
	  unsignedp = TREE_UNSIGNED (elttype);

	  bitpos = (i * TREE_INT_CST_LOW (TYPE_SIZE (elttype)));

	  store_field (target, bitsize, bitpos, mode, TREE_VALUE (elt),
		       /* The alignment of TARGET is
			  at least what its type requires.  */
		       VOIDmode, 0,
		       TYPE_ALIGN (type) / BITS_PER_UNIT,
		       int_size_in_bytes (type));
	}
    }

  else
    abort ();
}

/* Store the value of EXP (an expression tree)
   into a subfield of TARGET which has mode MODE and occupies
   BITSIZE bits, starting BITPOS bits from the start of TARGET.
   If MODE is VOIDmode, it means that we are storing into a bit-field.

   If VALUE_MODE is VOIDmode, return nothing in particular.
   UNSIGNEDP is not used in this case.

   Otherwise, return an rtx for the value stored.  This rtx
   has mode VALUE_MODE if that is convenient to do.
   In this case, UNSIGNEDP must be nonzero if the value is an unsigned type.

   ALIGN is the alignment that TARGET is known to have, measured in bytes.
   TOTAL_SIZE is the size in bytes of the structure, or -1 if varying.  */

static rtx
store_field (target, bitsize, bitpos, mode, exp, value_mode,
	     unsignedp, align, total_size)
     rtx target;
     int bitsize, bitpos;
     enum machine_mode mode;
     tree exp;
     enum machine_mode value_mode;
     int unsignedp;
     int align;
     int total_size;
{
  HOST_WIDE_INT width_mask = 0;

  if (bitsize < HOST_BITS_PER_WIDE_INT)
    width_mask = ((HOST_WIDE_INT) 1 << bitsize) - 1;

  /* If we are storing into an unaligned field of an aligned union that is
     in a register, we may have the mode of TARGET being an integer mode but
     MODE == BLKmode.  In that case, get an aligned object whose size and
     alignment are the same as TARGET and store TARGET into it (we can avoid
     the store if the field being stored is the entire width of TARGET).  Then
     call ourselves recursively to store the field into a BLKmode version of
     that object.  Finally, load from the object into TARGET.  This is not
     very efficient in general, but should only be slightly more expensive
     than the otherwise-required unaligned accesses.  Perhaps this can be
     cleaned up later.  */

  if (mode == BLKmode
      && (GET_CODE (target) == REG || GET_CODE (target) == SUBREG))
    {
      rtx object = assign_stack_temp (GET_MODE (target),
				      GET_MODE_SIZE (GET_MODE (target)), 0);
      rtx blk_object = copy_rtx (object);

      PUT_MODE (blk_object, BLKmode);

      if (bitsize != GET_MODE_BITSIZE (GET_MODE (target)))
	emit_move_insn (object, target);

      store_field (blk_object, bitsize, bitpos, mode, exp, VOIDmode, 0,
		   align, total_size);

      emit_move_insn (target, object);

      return target;
    }

  /* If the structure is in a register or if the component
     is a bit field, we cannot use addressing to access it.
     Use bit-field techniques or SUBREG to store in it.  */

  if (mode == VOIDmode
      || (mode != BLKmode && ! direct_store[(int) mode])
      || GET_CODE (target) == REG
      || GET_CODE (target) == SUBREG)
    {
      rtx temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);
      /* Store the value in the bitfield.  */
      store_bit_field (target, bitsize, bitpos, mode, temp, align, total_size);
      if (value_mode != VOIDmode)
	{
	  /* The caller wants an rtx for the value.  */
	  /* If possible, avoid refetching from the bitfield itself.  */
	  if (width_mask != 0
	      && ! (GET_CODE (target) == MEM && MEM_VOLATILE_P (target)))
	    {
	      tree count;
	      enum machine_mode tmode;

	      if (unsignedp)
		return expand_and (temp, GEN_INT (width_mask), NULL_RTX);
	      tmode = GET_MODE (temp);
	      if (tmode == VOIDmode)
		tmode = value_mode;
	      count = build_int_2 (GET_MODE_BITSIZE (tmode) - bitsize, 0);
	      temp = expand_shift (LSHIFT_EXPR, tmode, temp, count, 0, 0);
	      return expand_shift (RSHIFT_EXPR, tmode, temp, count, 0, 0);
	    }
	  return extract_bit_field (target, bitsize, bitpos, unsignedp,
				    NULL_RTX, value_mode, 0, align,
				    total_size);
	}
      return const0_rtx;
    }
  else
    {
      rtx addr = XEXP (target, 0);
      rtx to_rtx;

      /* If a value is wanted, it must be the lhs;
	 so make the address stable for multiple use.  */

      if (value_mode != VOIDmode && GET_CODE (addr) != REG
	  && ! CONSTANT_ADDRESS_P (addr)
	  /* A frame-pointer reference is already stable.  */
	  && ! (GET_CODE (addr) == PLUS
		&& GET_CODE (XEXP (addr, 1)) == CONST_INT
		&& (XEXP (addr, 0) == virtual_incoming_args_rtx
		    || XEXP (addr, 0) == virtual_stack_vars_rtx)))
	addr = copy_to_reg (addr);

      /* Now build a reference to just the desired component.  */

      to_rtx = change_address (target, mode,
			       plus_constant (addr, (bitpos / BITS_PER_UNIT)));
      MEM_IN_STRUCT_P (to_rtx) = 1;

      return store_expr (exp, to_rtx, value_mode != VOIDmode);
    }
}

/* Given an expression EXP that may be a COMPONENT_REF, a BIT_FIELD_REF,
   or an ARRAY_REF, look for nested COMPONENT_REFs, BIT_FIELD_REFs, or
   ARRAY_REFs at constant positions and find the ultimate containing object,
   which we return.

   We set *PBITSIZE to the size in bits that we want, *PBITPOS to the
   bit position, and *PUNSIGNEDP to the signedness of the field.
   If the position of the field is variable, we store a tree
   giving the variable offset (in units) in *POFFSET.
   This offset is in addition to the bit position.
   If the position is not variable, we store 0 in *POFFSET.

   If any of the extraction expressions is volatile,
   we store 1 in *PVOLATILEP.  Otherwise we don't change that.

   If the field is a bit-field, *PMODE is set to VOIDmode.  Otherwise, it
   is a mode that can be used to access the field.  In that case, *PBITSIZE
   is redundant.

   If the field describes a variable-sized object, *PMODE is set to
   VOIDmode and *PBITSIZE is set to -1.  An access cannot be made in
   this case, but the address of the object can be found.  */

tree
get_inner_reference (exp, pbitsize, pbitpos, poffset, pmode, punsignedp, pvolatilep)
     tree exp;
     int *pbitsize;
     int *pbitpos;
     tree *poffset;
     enum machine_mode *pmode;
     int *punsignedp;
     int *pvolatilep;
{
  tree size_tree = 0;
  enum machine_mode mode = VOIDmode;
  tree offset = 0;

  if (TREE_CODE (exp) == COMPONENT_REF)
    {
      size_tree = DECL_SIZE (TREE_OPERAND (exp, 1));
      if (! DECL_BIT_FIELD (TREE_OPERAND (exp, 1)))
	mode = DECL_MODE (TREE_OPERAND (exp, 1));
      *punsignedp = TREE_UNSIGNED (TREE_OPERAND (exp, 1));
    }
  else if (TREE_CODE (exp) == BIT_FIELD_REF)
    {
      size_tree = TREE_OPERAND (exp, 1);
      *punsignedp = TREE_UNSIGNED (exp);
    }
  else
    {
      mode = TYPE_MODE (TREE_TYPE (exp));
      *pbitsize = GET_MODE_BITSIZE (mode);
      *punsignedp = TREE_UNSIGNED (TREE_TYPE (exp));
    }
      
  if (size_tree)
    {
      if (TREE_CODE (size_tree) != INTEGER_CST)
	mode = BLKmode, *pbitsize = -1;
      else
	*pbitsize = TREE_INT_CST_LOW (size_tree);
    }

  /* Compute cumulative bit-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */

  *pbitpos = 0;

  while (1)
    {
      if (TREE_CODE (exp) == INDIRECT_REF && flag_volatile)
 	*pvolatilep = 1;

      if (TREE_CODE (exp) == COMPONENT_REF || TREE_CODE (exp) == BIT_FIELD_REF)
	{
	  tree pos = (TREE_CODE (exp) == COMPONENT_REF
		      ? DECL_FIELD_BITPOS (TREE_OPERAND (exp, 1))
		      : TREE_OPERAND (exp, 2));

	  if (TREE_CODE (pos) == PLUS_EXPR)
	    {
	      tree constant, var;
	      if (TREE_CODE (TREE_OPERAND (pos, 0)) == INTEGER_CST)
		{
		  constant = TREE_OPERAND (pos, 0);
		  var = TREE_OPERAND (pos, 1);
		}
	      else if (TREE_CODE (TREE_OPERAND (pos, 1)) == INTEGER_CST)
		{
		  constant = TREE_OPERAND (pos, 1);
		  var = TREE_OPERAND (pos, 0);
		}
	      else
		abort ();
	      *pbitpos += TREE_INT_CST_LOW (constant);
	      if (offset)
		offset = size_binop (PLUS_EXPR, offset,
				     size_binop (FLOOR_DIV_EXPR, var,
						 size_int (BITS_PER_UNIT)));
	      else
		offset = size_binop (FLOOR_DIV_EXPR, var,
				     size_int (BITS_PER_UNIT));
	    }
	  else if (TREE_CODE (pos) == INTEGER_CST)
	    *pbitpos += TREE_INT_CST_LOW (pos);
	  else
	    {
	      /* Assume here that the offset is a multiple of a unit.
		 If not, there should be an explicitly added constant.  */
	      if (offset)
		offset = size_binop (PLUS_EXPR, offset,
				     size_binop (FLOOR_DIV_EXPR, pos,
						 size_int (BITS_PER_UNIT)));
	      else
		offset = size_binop (FLOOR_DIV_EXPR, pos,
				     size_int (BITS_PER_UNIT));
	    }
	}

      else if (TREE_CODE (exp) == ARRAY_REF
	       && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	       && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST)
	{
	  *pbitpos += (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))
		       * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (exp))));
	}
      else if (TREE_CODE (exp) != NON_LVALUE_EXPR
	       && ! ((TREE_CODE (exp) == NOP_EXPR
		      || TREE_CODE (exp) == CONVERT_EXPR)
		     && (TYPE_MODE (TREE_TYPE (exp))
			 == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))))
	break;

      /* If any reference in the chain is volatile, the effect is volatile.  */
      if (TREE_THIS_VOLATILE (exp))
	*pvolatilep = 1;
      exp = TREE_OPERAND (exp, 0);
    }

  /* If this was a bit-field, see if there is a mode that allows direct
     access in case EXP is in memory.  */
  if (mode == VOIDmode && *pbitpos % *pbitsize == 0)
    {
      mode = mode_for_size (*pbitsize, MODE_INT, 0);
      if (mode == BLKmode)
	mode = VOIDmode;
    }

  *pmode = mode;
  *poffset = offset;
#if 0
  /* We aren't finished fixing the callers to really handle nonzero offset.  */
  if (offset != 0)
    abort ();
#endif

  return exp;
}

/* Given an rtx VALUE that may contain additions and multiplications,
   return an equivalent value that just refers to a register or memory.
   This is done by generating instructions to perform the arithmetic
   and returning a pseudo-register containing the value.

   The returned value may be a REG, SUBREG, MEM or constant.  */

rtx
force_operand (value, target)
     rtx value, target;
{
  register optab binoptab = 0;
  /* Use a temporary to force order of execution of calls to
     `force_operand'.  */
  rtx tmp;
  register rtx op2;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  register rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);

  if (GET_CODE (value) == PLUS)
    binoptab = add_optab;
  else if (GET_CODE (value) == MINUS)
    binoptab = sub_optab;
  else if (GET_CODE (value) == MULT)
    {
      op2 = XEXP (value, 1);
      if (!CONSTANT_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      tmp = force_operand (XEXP (value, 0), subtarget);
      return expand_mult (GET_MODE (value), tmp,
			  force_operand (op2, NULL_RTX),
			  target, 0);
    }

  if (binoptab)
    {
      op2 = XEXP (value, 1);
      if (!CONSTANT_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      if (binoptab == sub_optab && GET_CODE (op2) == CONST_INT)
	{
	  binoptab = add_optab;
	  op2 = negate_rtx (GET_MODE (value), op2);
	}

      /* Check for an addition with OP2 a constant integer and our first
	 operand a PLUS of a virtual register and something else.  In that
	 case, we want to emit the sum of the virtual register and the
	 constant first and then add the other value.  This allows virtual
	 register instantiation to simply modify the constant rather than
	 creating another one around this addition.  */
      if (binoptab == add_optab && GET_CODE (op2) == CONST_INT
	  && GET_CODE (XEXP (value, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (value, 0), 0)) == REG
	  && REGNO (XEXP (XEXP (value, 0), 0)) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (XEXP (XEXP (value, 0), 0)) <= LAST_VIRTUAL_REGISTER)
	{
	  rtx temp = expand_binop (GET_MODE (value), binoptab,
				   XEXP (XEXP (value, 0), 0), op2,
				   subtarget, 0, OPTAB_LIB_WIDEN);
	  return expand_binop (GET_MODE (value), binoptab, temp,
			       force_operand (XEXP (XEXP (value, 0), 1), 0),
			       target, 0, OPTAB_LIB_WIDEN);
	}
				   
      tmp = force_operand (XEXP (value, 0), subtarget);
      return expand_binop (GET_MODE (value), binoptab, tmp,
			   force_operand (op2, NULL_RTX),
			   target, 0, OPTAB_LIB_WIDEN);
      /* We give UNSIGNEP = 0 to expand_binop
	 because the only operations we are expanding here are signed ones.  */
    }
  return value;
}

/* Subroutine of expand_expr:
   save the non-copied parts (LIST) of an expr (LHS), and return a list
   which can restore these values to their previous values,
   should something modify their storage.  */

static tree
save_noncopied_parts (lhs, list)
     tree lhs;
     tree list;
{
  tree tail;
  tree parts = 0;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
      parts = chainon (parts, save_noncopied_parts (lhs, TREE_VALUE (tail)));
    else
      {
	tree part = TREE_VALUE (tail);
	tree part_type = TREE_TYPE (part);
	tree to_be_saved = build (COMPONENT_REF, part_type, lhs, part);
	rtx target = assign_stack_temp (TYPE_MODE (part_type),
					int_size_in_bytes (part_type), 0);
	if (! memory_address_p (TYPE_MODE (part_type), XEXP (target, 0)))
	  target = change_address (target, TYPE_MODE (part_type), NULL_RTX);
	parts = tree_cons (to_be_saved,
			   build (RTL_EXPR, part_type, NULL_TREE,
				  (tree) target),
			   parts);
	store_expr (TREE_PURPOSE (parts), RTL_EXPR_RTL (TREE_VALUE (parts)), 0);
      }
  return parts;
}

/* Subroutine of expand_expr:
   record the non-copied parts (LIST) of an expr (LHS), and return a list
   which specifies the initial values of these parts.  */

static tree
init_noncopied_parts (lhs, list)
     tree lhs;
     tree list;
{
  tree tail;
  tree parts = 0;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
      parts = chainon (parts, init_noncopied_parts (lhs, TREE_VALUE (tail)));
    else
      {
	tree part = TREE_VALUE (tail);
	tree part_type = TREE_TYPE (part);
	tree to_be_initialized = build (COMPONENT_REF, part_type, lhs, part);
	parts = tree_cons (TREE_PURPOSE (tail), to_be_initialized, parts);
      }
  return parts;
}

/* Subroutine of expand_expr: return nonzero iff there is no way that
   EXP can reference X, which is being modified.  */

static int
safe_from_p (x, exp)
     rtx x;
     tree exp;
{
  rtx exp_rtl = 0;
  int i, nops;

  if (x == 0)
    return 1;

  /* If this is a subreg of a hard register, declare it unsafe, otherwise,
     find the underlying pseudo.  */
  if (GET_CODE (x) == SUBREG)
    {
      x = SUBREG_REG (x);
      if (GET_CODE (x) == REG && REGNO (x) < FIRST_PSEUDO_REGISTER)
	return 0;
    }

  /* If X is a location in the outgoing argument area, it is always safe.  */
  if (GET_CODE (x) == MEM
      && (XEXP (x, 0) == virtual_outgoing_args_rtx
	  || (GET_CODE (XEXP (x, 0)) == PLUS
	      && XEXP (XEXP (x, 0), 0) == virtual_outgoing_args_rtx)))
    return 1;

  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case 'd':
      exp_rtl = DECL_RTL (exp);
      break;

    case 'c':
      return 1;

    case 'x':
      if (TREE_CODE (exp) == TREE_LIST)
	return ((TREE_VALUE (exp) == 0
		 || safe_from_p (x, TREE_VALUE (exp)))
		&& (TREE_CHAIN (exp) == 0
		    || safe_from_p (x, TREE_CHAIN (exp))));
      else
	return 0;

    case '1':
      return safe_from_p (x, TREE_OPERAND (exp, 0));

    case '2':
    case '<':
      return (safe_from_p (x, TREE_OPERAND (exp, 0))
	      && safe_from_p (x, TREE_OPERAND (exp, 1)));

    case 'e':
    case 'r':
      /* Now do code-specific tests.  EXP_RTL is set to any rtx we find in
	 the expression.  If it is set, we conflict iff we are that rtx or
	 both are in memory.  Otherwise, we check all operands of the
	 expression recursively.  */

      switch (TREE_CODE (exp))
	{
	case ADDR_EXPR:
	  return staticp (TREE_OPERAND (exp, 0));

	case INDIRECT_REF:
	  if (GET_CODE (x) == MEM)
	    return 0;
	  break;

	case CALL_EXPR:
	  exp_rtl = CALL_EXPR_RTL (exp);
	  if (exp_rtl == 0)
	    {
	      /* Assume that the call will clobber all hard registers and
		 all of memory.  */
	      if ((GET_CODE (x) == REG && REGNO (x) < FIRST_PSEUDO_REGISTER)
		  || GET_CODE (x) == MEM)
		return 0;
	    }

	  break;

	case RTL_EXPR:
	  exp_rtl = RTL_EXPR_RTL (exp);
	  if (exp_rtl == 0)
	    /* We don't know what this can modify.  */
	    return 0;

	  break;

	case WITH_CLEANUP_EXPR:
	  exp_rtl = RTL_EXPR_RTL (exp);
	  break;

	case SAVE_EXPR:
	  exp_rtl = SAVE_EXPR_RTL (exp);
	  break;

	case BIND_EXPR:
	  /* The only operand we look at is operand 1.  The rest aren't
	     part of the expression.  */
	  return safe_from_p (x, TREE_OPERAND (exp, 1));

	case METHOD_CALL_EXPR:
	  /* This takes a rtx argument, but shouldn't appear here. */
	  abort ();
	}

      /* If we have an rtx, we do not need to scan our operands.  */
      if (exp_rtl)
	break;

      nops = tree_code_length[(int) TREE_CODE (exp)];
      for (i = 0; i < nops; i++)
	if (TREE_OPERAND (exp, i) != 0
	    && ! safe_from_p (x, TREE_OPERAND (exp, i)))
	  return 0;
    }

  /* If we have an rtl, find any enclosed object.  Then see if we conflict
     with it.  */
  if (exp_rtl)
    {
      if (GET_CODE (exp_rtl) == SUBREG)
	{
	  exp_rtl = SUBREG_REG (exp_rtl);
	  if (GET_CODE (exp_rtl) == REG
	      && REGNO (exp_rtl) < FIRST_PSEUDO_REGISTER)
	    return 0;
	}

      /* If the rtl is X, then it is not safe.  Otherwise, it is unless both
	 are memory and EXP is not readonly.  */
      return ! (rtx_equal_p (x, exp_rtl)
		|| (GET_CODE (x) == MEM && GET_CODE (exp_rtl) == MEM
		    && ! TREE_READONLY (exp)));
    }

  /* If we reach here, it is safe.  */
  return 1;
}

/* Subroutine of expand_expr: return nonzero iff EXP is an
   expression whose type is statically determinable.  */

static int
fixed_type_p (exp)
     tree exp;
{
  if (TREE_CODE (exp) == PARM_DECL
      || TREE_CODE (exp) == VAR_DECL
      || TREE_CODE (exp) == CALL_EXPR || TREE_CODE (exp) == TARGET_EXPR
      || TREE_CODE (exp) == COMPONENT_REF
      || TREE_CODE (exp) == ARRAY_REF)
    return 1;
  return 0;
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

   EXPAND_CONST_ADDRESS says that it is okay to return a MEM
   with a constant address even if that address is not normally legitimate.
   EXPAND_INITIALIZER and EXPAND_SUM also have this effect.

   If MODIFIER is EXPAND_SUM then when EXP is an addition
   we can return an rtx of the form (MULT (REG ...) (CONST_INT ...))
   or a nest of (PLUS ...) and (MINUS ...) where the terms are
   products as above, or REG or MEM, or constant.
   Ordinarily in such cases we would output mul or add instructions
   and then return a pseudo reg containing the sum.

   EXPAND_INITIALIZER is much like EXPAND_SUM except that
   it also marks a label as absolutely required (it can't be dead).
   It also makes a ZERO_EXTEND or SIGN_EXTEND instead of emitting extend insns.
   This is used for outputting expressions used in initializers.  */

rtx
expand_expr (exp, target, tmode, modifier)
     register tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  register rtx op0, op1, temp;
  tree type = TREE_TYPE (exp);
  int unsignedp = TREE_UNSIGNED (type);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  optab this_optab;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);
  rtx original_target = target;
  int ignore = target == const0_rtx;
  tree context;

  /* Don't use hard regs as subtargets, because the combiner
     can only handle pseudo regs.  */
  if (subtarget && REGNO (subtarget) < FIRST_PSEUDO_REGISTER)
    subtarget = 0;
  /* Avoid subtargets inside loops,
     since they hide some invariant expressions.  */
  if (preserve_subexpressions_p ())
    subtarget = 0;

  if (ignore) target = 0, original_target = 0;

  /* If will do cse, generate all results into pseudo registers
     since 1) that allows cse to find more things
     and 2) otherwise cse could produce an insn the machine
     cannot support.  */

  if (! cse_not_expected && mode != BLKmode && target
      && (GET_CODE (target) != REG || REGNO (target) < FIRST_PSEUDO_REGISTER))
    target = subtarget;

  /* Ensure we reference a volatile object even if value is ignored.  */
  if (ignore && TREE_THIS_VOLATILE (exp)
      && mode != VOIDmode && mode != BLKmode)
    {
      target = gen_reg_rtx (mode);
      temp = expand_expr (exp, target, VOIDmode, modifier);
      if (temp != target)
	emit_move_insn (target, temp);
      return target;
    }

  switch (code)
    {
    case LABEL_DECL:
      {
	tree function = decl_function_context (exp);
	/* Handle using a label in a containing function.  */
	if (function != current_function_decl && function != 0)
	  {
	    struct function *p = find_function_data (function);
	    /* Allocate in the memory associated with the function
	       that the label is in.  */
	    push_obstacks (p->function_obstack,
			   p->function_maybepermanent_obstack);

	    p->forced_labels = gen_rtx (EXPR_LIST, VOIDmode,
					label_rtx (exp), p->forced_labels);
	    pop_obstacks ();
	  }
	else if (modifier == EXPAND_INITIALIZER)
	  forced_labels = gen_rtx (EXPR_LIST, VOIDmode,
				   label_rtx (exp), forced_labels);
	temp = gen_rtx (MEM, FUNCTION_MODE,
			gen_rtx (LABEL_REF, Pmode, label_rtx (exp)));
	if (function != current_function_decl && function != 0)
	  LABEL_REF_NONLOCAL_P (XEXP (temp, 0)) = 1;
	return temp;
      }

    case PARM_DECL:
      if (DECL_RTL (exp) == 0)
	{
	  error_with_decl (exp, "prior parameter's size depends on `%s'");
	  return CONST0_RTX (mode);
	}

    case FUNCTION_DECL:
    case VAR_DECL:
    case RESULT_DECL:
      if (DECL_RTL (exp) == 0)
	abort ();
      /* Ensure variable marked as used
	 even if it doesn't go through a parser.  */
      TREE_USED (exp) = 1;
      /* Handle variables inherited from containing functions.  */
      context = decl_function_context (exp);

      /* We treat inline_function_decl as an alias for the current function
	 because that is the inline function whose vars, types, etc.
	 are being merged into the current function.
	 See expand_inline_function.  */
      if (context != 0 && context != current_function_decl
	  && context != inline_function_decl
	  /* If var is static, we don't need a static chain to access it.  */
	  && ! (GET_CODE (DECL_RTL (exp)) == MEM
		&& CONSTANT_P (XEXP (DECL_RTL (exp), 0))))
	{
	  rtx addr;

	  /* Mark as non-local and addressable.  */
	  DECL_NONLOCAL (exp) = 1;
	  mark_addressable (exp);
	  if (GET_CODE (DECL_RTL (exp)) != MEM)
	    abort ();
	  addr = XEXP (DECL_RTL (exp), 0);
	  if (GET_CODE (addr) == MEM)
	    addr = gen_rtx (MEM, Pmode, fix_lexical_addr (XEXP (addr, 0), exp));
	  else
	    addr = fix_lexical_addr (addr, exp);
	  return change_address (DECL_RTL (exp), mode, addr);
	}

      /* This is the case of an array whose size is to be determined
	 from its initializer, while the initializer is still being parsed.
	 See expand_decl.  */
      if (GET_CODE (DECL_RTL (exp)) == MEM
	  && GET_CODE (XEXP (DECL_RTL (exp), 0)) == REG)
	return change_address (DECL_RTL (exp), GET_MODE (DECL_RTL (exp)),
			       XEXP (DECL_RTL (exp), 0));
      if (GET_CODE (DECL_RTL (exp)) == MEM
	  && modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_SUM
	  && modifier != EXPAND_INITIALIZER)
	{
	  /* DECL_RTL probably contains a constant address.
	     On RISC machines where a constant address isn't valid,
	     make some insns to get that address into a register.  */
	  if (!memory_address_p (DECL_MODE (exp), XEXP (DECL_RTL (exp), 0))
	      || (flag_force_addr
		  && CONSTANT_ADDRESS_P (XEXP (DECL_RTL (exp), 0))))
	    return change_address (DECL_RTL (exp), VOIDmode,
				   copy_rtx (XEXP (DECL_RTL (exp), 0)));
	}

      /* If the mode of DECL_RTL does not match that of the decl, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  */

      if (GET_CODE (DECL_RTL (exp)) == REG
	  && GET_MODE (DECL_RTL (exp)) != mode)
	{
	  enum machine_mode decl_mode = DECL_MODE (exp);

	  /* Get the signedness used for this variable.  Ensure we get the
	     same mode we got when the variable was declared.  */

	  PROMOTE_MODE (decl_mode, unsignedp, type);

	  if (decl_mode != GET_MODE (DECL_RTL (exp)))
	    abort ();

	  temp = gen_rtx (SUBREG, mode, DECL_RTL (exp), 0);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	  return temp;
	}

      return DECL_RTL (exp);

    case INTEGER_CST:
      return immed_double_const (TREE_INT_CST_LOW (exp),
				 TREE_INT_CST_HIGH (exp),
				 mode);

    case CONST_DECL:
      return expand_expr (DECL_INITIAL (exp), target, VOIDmode, 0);

    case REAL_CST:
      /* If optimized, generate immediate CONST_DOUBLE
	 which will be turned into memory by reload if necessary. 
     
	 We used to force a register so that loop.c could see it.  But
	 this does not allow gen_* patterns to perform optimizations with
	 the constants.  It also produces two insns in cases like "x = 1.0;".
	 On most machines, floating-point constants are not permitted in
	 many insns, so we'd end up copying it to a register in any case.

	 Now, we do the copying in expand_binop, if appropriate.  */
      return immed_real_const (exp);

    case COMPLEX_CST:
    case STRING_CST:
      if (! TREE_CST_RTL (exp))
	output_constant_def (exp);

      /* TREE_CST_RTL probably contains a constant address.
	 On RISC machines where a constant address isn't valid,
	 make some insns to get that address into a register.  */
      if (GET_CODE (TREE_CST_RTL (exp)) == MEM
	  && modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_INITIALIZER
	  && modifier != EXPAND_SUM
	  && !memory_address_p (mode, XEXP (TREE_CST_RTL (exp), 0)))
	return change_address (TREE_CST_RTL (exp), VOIDmode,
			       copy_rtx (XEXP (TREE_CST_RTL (exp), 0)));
      return TREE_CST_RTL (exp);

    case SAVE_EXPR:
      context = decl_function_context (exp);
      /* We treat inline_function_decl as an alias for the current function
	 because that is the inline function whose vars, types, etc.
	 are being merged into the current function.
	 See expand_inline_function.  */
      if (context == current_function_decl || context == inline_function_decl)
	context = 0;

      /* If this is non-local, handle it.  */
      if (context)
	{
	  temp = SAVE_EXPR_RTL (exp);
	  if (temp && GET_CODE (temp) == REG)
	    {
	      put_var_into_stack (exp);
	      temp = SAVE_EXPR_RTL (exp);
	    }
	  if (temp == 0 || GET_CODE (temp) != MEM)
	    abort ();
	  return change_address (temp, mode,
				 fix_lexical_addr (XEXP (temp, 0), exp));
	}
      if (SAVE_EXPR_RTL (exp) == 0)
	{
	  if (mode == BLKmode)
	    temp
	      = assign_stack_temp (mode,
				   int_size_in_bytes (TREE_TYPE (exp)), 0);
	  else
	    {
	      enum machine_mode var_mode = mode;

	      if (TREE_CODE (type) == INTEGER_TYPE
		  || TREE_CODE (type) == ENUMERAL_TYPE
		  || TREE_CODE (type) == BOOLEAN_TYPE
		  || TREE_CODE (type) == CHAR_TYPE
		  || TREE_CODE (type) == REAL_TYPE
		  || TREE_CODE (type) == POINTER_TYPE
		  || TREE_CODE (type) == OFFSET_TYPE)
		{
		  PROMOTE_MODE (var_mode, unsignedp, type);
		}

	      temp = gen_reg_rtx (var_mode);
	    }

	  SAVE_EXPR_RTL (exp) = temp;
	  store_expr (TREE_OPERAND (exp, 0), temp, 0);
	  if (!optimize && GET_CODE (temp) == REG)
	    save_expr_regs = gen_rtx (EXPR_LIST, VOIDmode, temp,
				      save_expr_regs);
	}

      /* If the mode of SAVE_EXPR_RTL does not match that of the expression, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  Note
	 that `unsignedp' was modified above in this case.  */

      if (GET_CODE (SAVE_EXPR_RTL (exp)) == REG
	  && GET_MODE (SAVE_EXPR_RTL (exp)) != mode)
	{
	  temp = gen_rtx (SUBREG, mode, SAVE_EXPR_RTL (exp), 0);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	  return temp;
	}

      return SAVE_EXPR_RTL (exp);

    case EXIT_EXPR:
      /* Exit the current loop if the body-expression is true.  */
      {
	rtx label = gen_label_rtx ();
	do_jump (TREE_OPERAND (exp, 0), label, NULL_RTX);
	expand_exit_loop (NULL_PTR);
	emit_label (label);
      }
      return const0_rtx;

    case LOOP_EXPR:
      expand_start_loop (1);
      expand_expr_stmt (TREE_OPERAND (exp, 0));
      expand_end_loop ();

      return const0_rtx;

    case BIND_EXPR:
      {
	tree vars = TREE_OPERAND (exp, 0);
	int vars_need_expansion = 0;

	/* Need to open a binding contour here because
	   if there are any cleanups they most be contained here.  */
	expand_start_bindings (0);

	/* Mark the corresponding BLOCK for output in its proper place.  */
	if (TREE_OPERAND (exp, 2) != 0
	    && ! TREE_USED (TREE_OPERAND (exp, 2)))
	  insert_block (TREE_OPERAND (exp, 2));

	/* If VARS have not yet been expanded, expand them now.  */
	while (vars)
	  {
	    if (DECL_RTL (vars) == 0)
	      {
		vars_need_expansion = 1;
		expand_decl (vars);
	      }
	    expand_decl_init (vars);
	    vars = TREE_CHAIN (vars);
	  }

	temp = expand_expr (TREE_OPERAND (exp, 1), target, tmode, modifier);

	expand_end_bindings (TREE_OPERAND (exp, 0), 0, 0);

	return temp;
      }

    case RTL_EXPR:
      if (RTL_EXPR_SEQUENCE (exp) == const0_rtx)
	abort ();
      emit_insns (RTL_EXPR_SEQUENCE (exp));
      RTL_EXPR_SEQUENCE (exp) = const0_rtx;
      return RTL_EXPR_RTL (exp);

    case CONSTRUCTOR:
      /* All elts simple constants => refer to a constant in memory.  But
	 if this is a non-BLKmode mode, let it store a field at a time
	 since that should make a CONST_INT or CONST_DOUBLE when we
	 fold.  */
      if (TREE_STATIC (exp) && (mode == BLKmode || TREE_ADDRESSABLE (exp)))
	{
	  rtx constructor = output_constant_def (exp);
	  if (modifier != EXPAND_CONST_ADDRESS
	      && modifier != EXPAND_INITIALIZER
	      && modifier != EXPAND_SUM
	      && !memory_address_p (GET_MODE (constructor),
				    XEXP (constructor, 0)))
	    constructor = change_address (constructor, VOIDmode,
					  XEXP (constructor, 0));
	  return constructor;
	}

      if (ignore)
	{
	  tree elt;
	  for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	    expand_expr (TREE_VALUE (elt), const0_rtx, VOIDmode, 0);
	  return const0_rtx;
	}
      else
	{
	  if (target == 0 || ! safe_from_p (target, exp))
	    {
	      if (mode != BLKmode && ! TREE_ADDRESSABLE (exp))
		target = gen_reg_rtx (mode);
	      else
		{
		  rtx safe_target = assign_stack_temp (mode, int_size_in_bytes (type), 0);
		  if (target)
		    MEM_IN_STRUCT_P (safe_target) = MEM_IN_STRUCT_P (target);
		  target = safe_target;
		}
	    }
	  store_constructor (exp, target);
	  return target;
	}

    case INDIRECT_REF:
      {
	tree exp1 = TREE_OPERAND (exp, 0);
	tree exp2;

	/* A SAVE_EXPR as the address in an INDIRECT_EXPR is generated
	   for  *PTR += ANYTHING  where PTR is put inside the SAVE_EXPR.
	   This code has the same general effect as simply doing
	   expand_expr on the save expr, except that the expression PTR
	   is computed for use as a memory address.  This means different
	   code, suitable for indexing, may be generated.  */
	if (TREE_CODE (exp1) == SAVE_EXPR
	    && SAVE_EXPR_RTL (exp1) == 0
	    && TREE_CODE (exp2 = TREE_OPERAND (exp1, 0)) != ERROR_MARK
	    && TYPE_MODE (TREE_TYPE (exp1)) == Pmode
	    && TYPE_MODE (TREE_TYPE (exp2)) == Pmode)
	  {
	    temp = expand_expr (TREE_OPERAND (exp1, 0), NULL_RTX,
				VOIDmode, EXPAND_SUM);
	    op0 = memory_address (mode, temp);
	    op0 = copy_all_regs (op0);
	    SAVE_EXPR_RTL (exp1) = op0;
	  }
	else
	  {
	    op0 = expand_expr (exp1, NULL_RTX, VOIDmode, EXPAND_SUM);
	    op0 = memory_address (mode, op0);
	  }

	temp = gen_rtx (MEM, mode, op0);
	/* If address was computed by addition,
	   mark this as an element of an aggregate.  */
	if (TREE_CODE (TREE_OPERAND (exp, 0)) == PLUS_EXPR
	    || (TREE_CODE (TREE_OPERAND (exp, 0)) == SAVE_EXPR
		&& TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == PLUS_EXPR)
	    || TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
	    || TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	    || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE
	    || (TREE_CODE (exp1) == ADDR_EXPR
		&& (exp2 = TREE_OPERAND (exp1, 0))
		&& (TREE_CODE (TREE_TYPE (exp2)) == ARRAY_TYPE
		    || TREE_CODE (TREE_TYPE (exp2)) == RECORD_TYPE
		    || TREE_CODE (TREE_TYPE (exp2)) == UNION_TYPE)))
	  MEM_IN_STRUCT_P (temp) = 1;
	MEM_VOLATILE_P (temp) = TREE_THIS_VOLATILE (exp) || flag_volatile;
#if 0 /* It is incorrectto set RTX_UNCHANGING_P here, because the fact that
	 a location is accessed through a pointer to const does not mean
	 that the value there can never change.  */
	RTX_UNCHANGING_P (temp) = TREE_READONLY (exp);
#endif
	return temp;
      }

    case ARRAY_REF:
      if (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST
	  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	{
	  /* Nonconstant array index or nonconstant element size.
	     Generate the tree for *(&array+index) and expand that,
	     except do it in a language-independent way
	     and don't complain about non-lvalue arrays.
	     `mark_addressable' should already have been called
	     for any array for which this case will be reached.  */

	  /* Don't forget the const or volatile flag from the array element. */
	  tree variant_type = build_type_variant (type,
						  TREE_READONLY (exp),
						  TREE_THIS_VOLATILE (exp));
	  tree array_adr = build1 (ADDR_EXPR, build_pointer_type (variant_type),
				   TREE_OPERAND (exp, 0));
	  tree index = TREE_OPERAND (exp, 1);
	  tree elt;

	  /* Convert the integer argument to a type the same size as a pointer
	     so the multiply won't overflow spuriously.  */
	  if (TYPE_PRECISION (TREE_TYPE (index)) != POINTER_SIZE)
	    index = convert (type_for_size (POINTER_SIZE, 0), index);

	  /* Don't think the address has side effects
	     just because the array does.
	     (In some cases the address might have side effects,
	     and we fail to record that fact here.  However, it should not
	     matter, since expand_expr should not care.)  */
	  TREE_SIDE_EFFECTS (array_adr) = 0;

	  elt = build1 (INDIRECT_REF, type,
			fold (build (PLUS_EXPR, TYPE_POINTER_TO (variant_type),
				     array_adr,
				     fold (build (MULT_EXPR,
						  TYPE_POINTER_TO (variant_type),
						  index, size_in_bytes (type))))));

	  /* Volatility, etc., of new expression is same as old expression.  */
	  TREE_SIDE_EFFECTS (elt) = TREE_SIDE_EFFECTS (exp);
	  TREE_THIS_VOLATILE (elt) = TREE_THIS_VOLATILE (exp);
	  TREE_READONLY (elt) = TREE_READONLY (exp);

	  return expand_expr (elt, target, tmode, modifier);
	}

      /* Fold an expression like: "foo"[2].
	 This is not done in fold so it won't happen inside &.  */
      {
	int i;
	tree arg0 = TREE_OPERAND (exp, 0);
	tree arg1 = TREE_OPERAND (exp, 1);

	if (TREE_CODE (arg0) == STRING_CST
	    && TREE_CODE (arg1) == INTEGER_CST
	    && !TREE_INT_CST_HIGH (arg1)
	    && (i = TREE_INT_CST_LOW (arg1)) < TREE_STRING_LENGTH (arg0))
	  {
	    if (TREE_TYPE (TREE_TYPE (arg0)) == integer_type_node)
	      {
		exp = build_int_2 (((int *)TREE_STRING_POINTER (arg0))[i], 0);
		TREE_TYPE (exp) = integer_type_node;
		return expand_expr (exp, target, tmode, modifier);
	      }
	    if (TREE_TYPE (TREE_TYPE (arg0)) == char_type_node)
	      {
		exp = build_int_2 (TREE_STRING_POINTER (arg0)[i], 0);
		TREE_TYPE (exp) = integer_type_node;
		return expand_expr (convert (TREE_TYPE (TREE_TYPE (arg0)), exp), target, tmode, modifier);
	      }
	  }
      }

      /* If this is a constant index into a constant array,
	 just get the value from the array.  Handle both the cases when
	 we have an explicit constructor and when our operand is a variable
	 that was declared const.  */

      if (TREE_CODE (TREE_OPERAND (exp, 0)) == CONSTRUCTOR
	  && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0)))
	{
	  tree index = fold (TREE_OPERAND (exp, 1));
	  if (TREE_CODE (index) == INTEGER_CST
	      && TREE_INT_CST_HIGH (index) == 0)
	    {
	      int i = TREE_INT_CST_LOW (index);
	      tree elem = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0));

	      while (elem && i--)
		elem = TREE_CHAIN (elem);
	      if (elem)
		return expand_expr (fold (TREE_VALUE (elem)), target,
				    tmode, modifier);
	    }
	}
	  
      else if (TREE_READONLY (TREE_OPERAND (exp, 0))
	       && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
	       && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == ARRAY_TYPE
	       && TREE_CODE (TREE_OPERAND (exp, 0)) == VAR_DECL
	       && DECL_INITIAL (TREE_OPERAND (exp, 0))
	       && optimize >= 1
	       && (TREE_CODE (DECL_INITIAL (TREE_OPERAND (exp, 0)))
		   != ERROR_MARK))
	{
	  tree index = fold (TREE_OPERAND (exp, 1));
	  if (TREE_CODE (index) == INTEGER_CST
	      && TREE_INT_CST_HIGH (index) == 0)
	    {
	      int i = TREE_INT_CST_LOW (index);
	      tree init = DECL_INITIAL (TREE_OPERAND (exp, 0));

	      if (TREE_CODE (init) == CONSTRUCTOR)
		{
		  tree elem = CONSTRUCTOR_ELTS (init);

		  while (elem && i--)
		    elem = TREE_CHAIN (elem);
		  if (elem)
		    return expand_expr (fold (TREE_VALUE (elem)), target,
					tmode, modifier);
		}
	      else if (TREE_CODE (init) == STRING_CST
		       && i < TREE_STRING_LENGTH (init))
		{
		  temp = GEN_INT (TREE_STRING_POINTER (init)[i]);
		  return convert_to_mode (mode, temp, 0);
		}
	    }
	}
      /* Treat array-ref with constant index as a component-ref.  */

    case COMPONENT_REF:
    case BIT_FIELD_REF:
      /* If the operand is a CONSTRUCTOR, we can just extract the
	 appropriate field if it is present.  */
      if (code != ARRAY_REF
	  && TREE_CODE (TREE_OPERAND (exp, 0)) == CONSTRUCTOR)
	{
	  tree elt;

	  for (elt = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0)); elt;
	       elt = TREE_CHAIN (elt))
	    if (TREE_PURPOSE (elt) == TREE_OPERAND (exp, 1))
	      return expand_expr (TREE_VALUE (elt), target, tmode, modifier);
	}

      {
	enum machine_mode mode1;
	int bitsize;
	int bitpos;
	tree offset;
	int volatilep = 0;
	tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					&mode1, &unsignedp, &volatilep);

	/* In some cases, we will be offsetting OP0's address by a constant.
	   So get it as a sum, if possible.  If we will be using it
	   directly in an insn, we validate it.  */
	op0 = expand_expr (tem, NULL_RTX, VOIDmode, EXPAND_SUM);

	/* If this is a constant, put it into a register if it is a
	   legimate constant and memory if it isn't.  */
	if (CONSTANT_P (op0))
	  {
	    enum machine_mode mode = TYPE_MODE (TREE_TYPE (tem));
	    if (LEGITIMATE_CONSTANT_P (op0))
	      op0 = force_reg (mode, op0);
	    else
	      op0 = validize_mem (force_const_mem (mode, op0));
	  }

	if (offset != 0)
	  {
	    rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, 0);

	    if (GET_CODE (op0) != MEM)
	      abort ();
	    op0 = change_address (op0, VOIDmode,
				  gen_rtx (PLUS, Pmode, XEXP (op0, 0),
					   force_reg (Pmode, offset_rtx)));
	  }

	/* Don't forget about volatility even if this is a bitfield.  */
	if (GET_CODE (op0) == MEM && volatilep && ! MEM_VOLATILE_P (op0))
	  {
	    op0 = copy_rtx (op0);
	    MEM_VOLATILE_P (op0) = 1;
	  }

	if (mode1 == VOIDmode
	    || (mode1 != BLKmode && ! direct_load[(int) mode1]
		&& modifier != EXPAND_CONST_ADDRESS
		&& modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	    || GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
	  {
	    /* In cases where an aligned union has an unaligned object
	       as a field, we might be extracting a BLKmode value from
	       an integer-mode (e.g., SImode) object.  Handle this case
	       by doing the extract into an object as wide as the field
	       (which we know to be the width of a basic mode), then
	       storing into memory, and changing the mode to BLKmode.  */
	    enum machine_mode ext_mode = mode;

	    if (ext_mode == BLKmode)
	      ext_mode = mode_for_size (bitsize, MODE_INT, 1);

	    if (ext_mode == BLKmode)
	      abort ();

	    op0 = extract_bit_field (validize_mem (op0), bitsize, bitpos,
				     unsignedp, target, ext_mode, ext_mode,
				     TYPE_ALIGN (TREE_TYPE (tem)) / BITS_PER_UNIT,
				     int_size_in_bytes (TREE_TYPE (tem)));
	    if (mode == BLKmode)
	      {
		rtx new = assign_stack_temp (ext_mode,
					     bitsize / BITS_PER_UNIT, 0);

		emit_move_insn (new, op0);
		op0 = copy_rtx (new);
		PUT_MODE (op0, BLKmode);
	      }

	    return op0;
	  }

	/* Get a reference to just this component.  */
	if (modifier == EXPAND_CONST_ADDRESS
	    || modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  op0 = gen_rtx (MEM, mode1, plus_constant (XEXP (op0, 0),
						    (bitpos / BITS_PER_UNIT)));
	else
	  op0 = change_address (op0, mode1,
				plus_constant (XEXP (op0, 0),
					       (bitpos / BITS_PER_UNIT)));
	MEM_IN_STRUCT_P (op0) = 1;
	MEM_VOLATILE_P (op0) |= volatilep;
	if (mode == mode1 || mode1 == BLKmode || mode1 == tmode)
	  return op0;
	if (target == 0)
	  target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);
	convert_move (target, op0, unsignedp);
	return target;
      }

    case OFFSET_REF:
      {
	tree base = build_unary_op (ADDR_EXPR, TREE_OPERAND (exp, 0), 0);
	tree addr = build (PLUS_EXPR, type, base, TREE_OPERAND (exp, 1));
	op0 = expand_expr (addr, NULL_RTX, VOIDmode, EXPAND_SUM);
	temp = gen_rtx (MEM, mode, memory_address (mode, op0));
	MEM_IN_STRUCT_P (temp) = 1;
	MEM_VOLATILE_P (temp) = TREE_THIS_VOLATILE (exp) || flag_volatile;
#if 0 /* It is incorrectto set RTX_UNCHANGING_P here, because the fact that
	 a location is accessed through a pointer to const does not mean
	 that the value there can never change.  */
	RTX_UNCHANGING_P (temp) = TREE_READONLY (exp);
#endif
	return temp;
      }

      /* Intended for a reference to a buffer of a file-object in Pascal.
	 But it's not certain that a special tree code will really be
	 necessary for these.  INDIRECT_REF might work for them.  */
    case BUFFER_REF:
      abort ();

    /* IN_EXPR: Inlined pascal set IN expression.

       Algorithm:
         rlo       = set_low - (set_low%bits_per_word);
	 the_word  = set [ (index - rlo)/bits_per_word ];
	 bit_index = index % bits_per_word;
	 bitmask   = 1 << bit_index;
	 return !!(the_word & bitmask);  */
    case IN_EXPR:
      preexpand_calls (exp);
      {
	tree set = TREE_OPERAND (exp, 0);
	tree index = TREE_OPERAND (exp, 1);
	tree set_type = TREE_TYPE (set);

	tree set_low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (set_type));
	tree set_high_bound = TYPE_MAX_VALUE (TYPE_DOMAIN (set_type));

	rtx index_val;
	rtx lo_r;
	rtx hi_r;
	rtx rlow;
	rtx diff, quo, rem, addr, bit, result;
	rtx setval, setaddr;
	enum machine_mode index_mode = TYPE_MODE (TREE_TYPE (index));

	if (target == 0)
	  target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

	/* If domain is empty, answer is no.  */
	if (tree_int_cst_lt (set_high_bound, set_low_bound))
	  return const0_rtx;

	index_val = expand_expr (index, 0, VOIDmode, 0);
	lo_r = expand_expr (set_low_bound, 0, VOIDmode, 0);
	hi_r = expand_expr (set_high_bound, 0, VOIDmode, 0);
	setval = expand_expr (set, 0, VOIDmode, 0);
	setaddr = XEXP (setval, 0); 

	/* Compare index against bounds, if they are constant.  */
	if (GET_CODE (index_val) == CONST_INT
	    && GET_CODE (lo_r) == CONST_INT)
	  {
	    if (INTVAL (index_val) < INTVAL (lo_r))
	      return const0_rtx;
	  }

	if (GET_CODE (index_val) == CONST_INT
	    && GET_CODE (hi_r) == CONST_INT)
	  {
	    if (INTVAL (hi_r) < INTVAL (index_val))
	      return const0_rtx;
	  }

	/* If we get here, we have to generate the code for both cases
	   (in range and out of range).  */

	op0 = gen_label_rtx ();
	op1 = gen_label_rtx ();

	if (! (GET_CODE (index_val) == CONST_INT
	       && GET_CODE (lo_r) == CONST_INT))
	  {
	    emit_cmp_insn (index_val, lo_r, LT, 0, GET_MODE (index_val), 0, 0);
	    emit_jump_insn (gen_blt (op1));
	  }

	if (! (GET_CODE (index_val) == CONST_INT
	       && GET_CODE (hi_r) == CONST_INT))
	  {
	    emit_cmp_insn (index_val, hi_r, GT, 0, GET_MODE (index_val), 0, 0);
	    emit_jump_insn (gen_bgt (op1));
	  }

	/* Calculate the element number of bit zero in the first word
	   of the set.  */
	if (GET_CODE (lo_r) == CONST_INT)
	  rlow = gen_rtx (CONST_INT, VOIDmode,
			  INTVAL (lo_r) & ~ (1 << BITS_PER_UNIT));
	else
	  rlow = expand_binop (index_mode, and_optab,
			       lo_r, gen_rtx (CONST_INT, VOIDmode,
					      ~ (1 << BITS_PER_UNIT)),
			       0, 0, OPTAB_LIB_WIDEN);

	diff = expand_binop (index_mode, sub_optab,
			     index_val, rlow, 0, 0, OPTAB_LIB_WIDEN);

	quo = expand_divmod (0, TRUNC_DIV_EXPR, index_mode, diff,
			     gen_rtx (CONST_INT, VOIDmode, BITS_PER_UNIT),
			     0, 0);
	rem = expand_divmod (1, TRUNC_MOD_EXPR, index_mode, index_val,
			     gen_rtx (CONST_INT, VOIDmode, BITS_PER_UNIT),
			     0, 0);
	addr = memory_address (byte_mode,
			       expand_binop (index_mode, add_optab,
					     diff, setaddr));
	/* Extract the bit we want to examine */
	bit = expand_shift (RSHIFT_EXPR, byte_mode,
			    gen_rtx (MEM, byte_mode, addr), rem, 0, 1);
	result = expand_binop (SImode, and_optab, bit, const1_rtx, target,
			       1, OPTAB_LIB_WIDEN);
	emit_move_insn (target, result);

	/* Output the code to handle the out-of-range case.  */
	emit_jump (op0);
	emit_label (op1);
	emit_move_insn (target, const0_rtx);
	emit_label (op0);
	return target;
      }

    case WITH_CLEANUP_EXPR:
      if (RTL_EXPR_RTL (exp) == 0)
	{
	  RTL_EXPR_RTL (exp)
	    = expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);
	  cleanups_this_call
	    = tree_cons (NULL_TREE, TREE_OPERAND (exp, 2), cleanups_this_call);
	  /* That's it for this cleanup.  */
	  TREE_OPERAND (exp, 2) = 0;
	}
      return RTL_EXPR_RTL (exp);

    case CALL_EXPR:
      /* Check for a built-in function.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == FUNCTION_DECL
	  && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	return expand_builtin (exp, target, subtarget, tmode, ignore);
      /* If this call was expanded already by preexpand_calls,
	 just return the result we got.  */
      if (CALL_EXPR_RTL (exp) != 0)
	return CALL_EXPR_RTL (exp);
      return expand_call (exp, target, ignore);

    case NON_LVALUE_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case REFERENCE_EXPR:
      if (TREE_CODE (type) == VOID_TYPE || ignore)
	{
	  expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}
      if (mode == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	return expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, modifier);
      if (TREE_CODE (type) == UNION_TYPE)
	{
	  tree valtype = TREE_TYPE (TREE_OPERAND (exp, 0));
	  if (target == 0)
	    {
	      if (mode == BLKmode)
		{
		  if (TYPE_SIZE (type) == 0
		      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
		    abort ();
		  target = assign_stack_temp (BLKmode,
					      (TREE_INT_CST_LOW (TYPE_SIZE (type))
					       + BITS_PER_UNIT - 1)
					      / BITS_PER_UNIT, 0);
		}
	      else
		target = gen_reg_rtx (mode);
	    }
	  if (GET_CODE (target) == MEM)
	    /* Store data into beginning of memory target.  */
	    store_expr (TREE_OPERAND (exp, 0),
			change_address (target, TYPE_MODE (valtype), 0), 0);

	  else if (GET_CODE (target) == REG)
	    /* Store this field into a union of the proper type.  */
	    store_field (target, GET_MODE_BITSIZE (TYPE_MODE (valtype)), 0,
			 TYPE_MODE (valtype), TREE_OPERAND (exp, 0),
			 VOIDmode, 0, 1,
			 int_size_in_bytes (TREE_TYPE (TREE_OPERAND (exp, 0))));
	  else
	    abort ();

	  /* Return the entire union.  */
	  return target;
	}
      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, mode, 0);
      if (GET_MODE (op0) == mode || GET_MODE (op0) == VOIDmode)
	return op0;
      if (modifier == EXPAND_INITIALIZER)
	return gen_rtx (unsignedp ? ZERO_EXTEND : SIGN_EXTEND, mode, op0);
      if (flag_force_mem && GET_CODE (op0) == MEM)
	op0 = copy_to_reg (op0);

      if (target == 0)
	return convert_to_mode (mode, op0, TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      else
	convert_move (target, op0, TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case PLUS_EXPR:
      /* We come here from MINUS_EXPR when the second operand is a constant. */
    plus_expr:
      this_optab = add_optab;

      /* If we are adding a constant, an RTL_EXPR that is sp, fp, or ap, and
	 something else, make sure we add the register to the constant and
	 then to the other thing.  This case can occur during strength
	 reduction and doing it this way will produce better code if the
	 frame pointer or argument pointer is eliminated.

	 fold-const.c will ensure that the constant is always in the inner
	 PLUS_EXPR, so the only case we need to do anything about is if
	 sp, ap, or fp is our second argument, in which case we must swap
	 the innermost first argument and our second argument.  */

      if (TREE_CODE (TREE_OPERAND (exp, 0)) == PLUS_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 1)) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (exp, 1)) == RTL_EXPR
	  && (RTL_EXPR_RTL (TREE_OPERAND (exp, 1)) == frame_pointer_rtx
	      || RTL_EXPR_RTL (TREE_OPERAND (exp, 1)) == stack_pointer_rtx
	      || RTL_EXPR_RTL (TREE_OPERAND (exp, 1)) == arg_pointer_rtx))
	{
	  tree t = TREE_OPERAND (exp, 1);

	  TREE_OPERAND (exp, 1) = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	  TREE_OPERAND (TREE_OPERAND (exp, 0), 0) = t;
	}

      /* If the result is to be Pmode and we are adding an integer to
	 something, we might be forming a constant.  So try to use
	 plus_constant.  If it produces a sum and we can't accept it,
	 use force_operand.  This allows P = &ARR[const] to generate
	 efficient code on machines where a SYMBOL_REF is not a valid
	 address.

	 If this is an EXPAND_SUM call, always return the sum.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
	  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  && (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER
	      || mode == Pmode))
	{
	  op1 = expand_expr (TREE_OPERAND (exp, 1), subtarget, VOIDmode,
			     EXPAND_SUM);
	  op1 = plus_constant (op1, TREE_INT_CST_LOW (TREE_OPERAND (exp, 0)));
	  if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	    op1 = force_operand (op1, target);
	  return op1;
	}

      else if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	       && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_INT
	       && (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER
		   || mode == Pmode))
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode,
			     EXPAND_SUM);
	  op0 = plus_constant (op0, TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)));
	  if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	    op0 = force_operand (op0, target);
	  return op0;
	}

      /* No sense saving up arithmetic to be done
	 if it's all in the wrong mode to form part of an address.
	 And force_operand won't know whether to sign-extend or
	 zero-extend.  */
      if ((modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	  || mode != Pmode) goto binop;

      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
	subtarget = 0;

      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, modifier);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, modifier);

      /* Make sure any term that's a sum with a constant comes last.  */
      if (GET_CODE (op0) == PLUS
	  && CONSTANT_P (XEXP (op0, 1)))
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}
      /* If adding to a sum including a constant,
	 associate it to put the constant outside.  */
      if (GET_CODE (op1) == PLUS
	  && CONSTANT_P (XEXP (op1, 1)))
	{
	  rtx constant_term = const0_rtx;

	  temp = simplify_binary_operation (PLUS, mode, XEXP (op1, 0), op0);
	  if (temp != 0)
	    op0 = temp;
	  /* Ensure that MULT comes first if there is one.  */
	  else if (GET_CODE (op0) == MULT)
	    op0 = gen_rtx (PLUS, mode, op0, XEXP (op1, 0));
	  else
	    op0 = gen_rtx (PLUS, mode, XEXP (op1, 0), op0);

	  /* Let's also eliminate constants from op0 if possible.  */
	  op0 = eliminate_constant_term (op0, &constant_term);

	  /* CONSTANT_TERM and XEXP (op1, 1) are known to be constant, so
	     their sum should be a constant.  Form it into OP1, since the 
	     result we want will then be OP0 + OP1.  */

	  temp = simplify_binary_operation (PLUS, mode, constant_term,
					    XEXP (op1, 1));
	  if (temp != 0)
	    op1 = temp;
	  else
	    op1 = gen_rtx (PLUS, mode, constant_term, XEXP (op1, 1));
	}

      /* Put a constant term last and put a multiplication first.  */
      if (CONSTANT_P (op0) || GET_CODE (op1) == MULT)
	temp = op1, op1 = op0, op0 = temp;

      temp = simplify_binary_operation (PLUS, mode, op0, op1);
      return temp ? temp : gen_rtx (PLUS, mode, op0, op1);

    case MINUS_EXPR:
      /* Handle difference of two symbolic constants,
	 for the sake of an initializer.  */
      if ((modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  && really_constant_p (TREE_OPERAND (exp, 0))
	  && really_constant_p (TREE_OPERAND (exp, 1)))
	{
	  rtx op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX,
				 VOIDmode, modifier);
	  rtx op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX,
				 VOIDmode, modifier);
	  return gen_rtx (MINUS, mode, op0, op1);
	}
      /* Convert A - const to A + (-const).  */
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  exp = build (PLUS_EXPR, type, TREE_OPERAND (exp, 0),
		       fold (build1 (NEGATE_EXPR, type,
				     TREE_OPERAND (exp, 1))));
	  goto plus_expr;
	}
      this_optab = sub_optab;
      goto binop;

    case MULT_EXPR:
      preexpand_calls (exp);
      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
	{
	  register tree t1 = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = t1;
	}

      /* Attempt to return something suitable for generating an
	 indexed address, for machines that support that.  */

      if (modifier == EXPAND_SUM && mode == Pmode
	  && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, EXPAND_SUM);

	  /* Apply distributive law if OP0 is x+c.  */
	  if (GET_CODE (op0) == PLUS
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT)
	    return gen_rtx (PLUS, mode,
			    gen_rtx (MULT, mode, XEXP (op0, 0),
				     GEN_INT (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)))),
			    GEN_INT (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))
				     * INTVAL (XEXP (op0, 1))));

	  if (GET_CODE (op0) != REG)
	    op0 = force_operand (op0, NULL_RTX);
	  if (GET_CODE (op0) != REG)
	    op0 = copy_to_mode_reg (mode, op0);

	  return gen_rtx (MULT, mode, op0,
			  GEN_INT (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))));
	}

      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
	subtarget = 0;

      /* Check for multiplying things that have been extended
	 from a narrower type.  If this machine supports multiplying
	 in that narrower type with a result in the desired type,
	 do it that way, and avoid the explicit type-conversion.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == NOP_EXPR
	  && TREE_CODE (type) == INTEGER_TYPE
	  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0))))
	  && ((TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	       && int_fits_type_p (TREE_OPERAND (exp, 1),
				   TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	       /* Don't use a widening multiply if a shift will do.  */
	       && ((GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 1))))
		    > HOST_BITS_PER_WIDE_INT)
		   || exact_log2 (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))) < 0))
	      ||
	      (TREE_CODE (TREE_OPERAND (exp, 1)) == NOP_EXPR
	       && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)))
		   ==
		   TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))))
	       /* If both operands are extended, they must either both
		  be zero-extended or both be sign-extended.  */
	       && (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)))
		   ==
		   TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))))))
	{
	  enum machine_mode innermode
	    = TYPE_MODE (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)));
	  this_optab = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
			? umul_widen_optab : smul_widen_optab);
	  if (mode == GET_MODE_WIDER_MODE (innermode)
	      && this_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	    {
	      op0 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				 NULL_RTX, VOIDmode, 0);
	      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
		op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX,
				   VOIDmode, 0);
	      else
		op1 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 1), 0),
				   NULL_RTX, VOIDmode, 0);
	      goto binop2;
	    }
	}
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
      return expand_mult (mode, op0, op1, target, unsignedp);

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
	subtarget = 0;
      /* Possible optimization: compute the dividend with EXPAND_SUM
	 then if the divisor is constant can optimize the case
	 where some terms of the dividend have coeffs divisible by it.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
      return expand_divmod (0, code, mode, op0, op1, target, unsignedp);

    case RDIV_EXPR:
      this_optab = flodiv_optab;
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
	subtarget = 0;
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
      return expand_divmod (1, code, mode, op0, op1, target, unsignedp);

    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
      abort ();			/* Not used for C.  */

    case FIX_TRUNC_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      expand_fix (target, op0, unsignedp);
      return target;

    case FLOAT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      /* expand_float can't figure out what to do if FROM has VOIDmode.
	 So give it the correct mode.  With -O, cse will optimize this.  */
      if (GET_MODE (op0) == VOIDmode)
	op0 = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
				op0);
      expand_float (target, op0,
		    TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case NEGATE_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      temp = expand_unop (mode, neg_optab, op0, target, 0);
      if (temp == 0)
	abort ();
      return temp;

    case ABS_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);

      /* Handle complex values specially.  */
      {
	enum machine_mode opmode
	  = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));

	if (GET_MODE_CLASS (opmode) == MODE_COMPLEX_INT
	    || GET_MODE_CLASS (opmode) == MODE_COMPLEX_FLOAT)
	  return expand_complex_abs (opmode, op0, target, unsignedp);
      }

      /* Unsigned abs is simply the operand.  Testing here means we don't
	 risk generating incorrect code below.  */
      if (TREE_UNSIGNED (type))
	return op0;

      /* First try to do it with a special abs instruction.  */
      temp = expand_unop (mode, abs_optab, op0, target, 0);
      if (temp != 0)
	return temp;

      /* If this machine has expensive jumps, we can do integer absolute
	 value of X as (((signed) x >> (W-1)) ^ x) - ((signed) x >> (W-1)),
	 where W is the width of MODE.  */

      if (GET_MODE_CLASS (mode) == MODE_INT && BRANCH_COST >= 2)
	{
	  rtx extended = expand_shift (RSHIFT_EXPR, mode, op0,
				       size_int (GET_MODE_BITSIZE (mode) - 1),
				       NULL_RTX, 0);

	  temp = expand_binop (mode, xor_optab, extended, op0, target, 0,
			       OPTAB_LIB_WIDEN);
	  if (temp != 0)
	    temp = expand_binop (mode, sub_optab, temp, extended, target, 0,
				 OPTAB_LIB_WIDEN);

	  if (temp != 0)
	    return temp;
	}

      /* If that does not win, use conditional jump and negate.  */
      target = original_target;
      temp = gen_label_rtx ();
      if (target == 0 || ! safe_from_p (target, TREE_OPERAND (exp, 0))
	  || (GET_CODE (target) == REG
	      && REGNO (target) < FIRST_PSEUDO_REGISTER))
	target = gen_reg_rtx (mode);
      emit_move_insn (target, op0);
      emit_cmp_insn (target,
		     expand_expr (convert (type, integer_zero_node),
				  NULL_RTX, VOIDmode, 0),
		     GE, NULL_RTX, mode, 0, 0);
      NO_DEFER_POP;
      emit_jump_insn (gen_bge (temp));
      op0 = expand_unop (mode, neg_optab, target, target, 0);
      if (op0 != target)
	emit_move_insn (target, op0);
      emit_label (temp);
      OK_DEFER_POP;
      return target;

    case MAX_EXPR:
    case MIN_EXPR:
      target = original_target;
      if (target == 0 || ! safe_from_p (target, TREE_OPERAND (exp, 1))
	  || (GET_CODE (target) == REG
	      && REGNO (target) < FIRST_PSEUDO_REGISTER))
	target = gen_reg_rtx (mode);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);

      /* First try to do it with a special MIN or MAX instruction.
	 If that does not win, use a conditional jump to select the proper
	 value.  */
      this_optab = (TREE_UNSIGNED (type)
		    ? (code == MIN_EXPR ? umin_optab : umax_optab)
		    : (code == MIN_EXPR ? smin_optab : smax_optab));

      temp = expand_binop (mode, this_optab, op0, op1, target, unsignedp,
			   OPTAB_WIDEN);
      if (temp != 0)
	return temp;

      if (target != op0)
	emit_move_insn (target, op0);
      op0 = gen_label_rtx ();
      if (code == MAX_EXPR)
	temp = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare_from_rtx (target, op1, GEU, 1, mode, NULL_RTX, 0)
		: compare_from_rtx (target, op1, GE, 0, mode, NULL_RTX, 0));
      else
	temp = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare_from_rtx (target, op1, LEU, 1, mode, NULL_RTX, 0)
		: compare_from_rtx (target, op1, LE, 0, mode, NULL_RTX, 0));
      if (temp == const0_rtx)
	emit_move_insn (target, op1);
      else if (temp != const_true_rtx)
	{
	  if (bcc_gen_fctn[(int) GET_CODE (temp)] != 0)
	    emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (temp)]) (op0));
	  else
	    abort ();
	  emit_move_insn (target, op1);
	}
      emit_label (op0);
      return target;

/* ??? Can optimize when the operand of this is a bitwise operation,
   by using a different bitwise operation.  */
    case BIT_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, one_cmpl_optab, op0, target, 1);
      if (temp == 0)
	abort ();
      return temp;

    case FFS_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, ffs_optab, op0, target, 1);
      if (temp == 0)
	abort ();
      return temp;

/* ??? Can optimize bitwise operations with one arg constant.
   Can optimize (a bitwise1 n) bitwise2 (a bitwise3 b)
   and (a bitwise1 b) bitwise2 b (etc)
   but that is probably not worth while.  */

/* BIT_AND_EXPR is for bitwise anding.
   TRUTH_AND_EXPR is for anding two boolean values
   when we want in all cases to compute both of them.
   In general it is fastest to do TRUTH_AND_EXPR by
   computing both operands as actual zero-or-1 values
   and then bitwise anding.  In cases where there cannot
   be any side effects, better code would be made by
   treating TRUTH_AND_EXPR like TRUTH_ANDIF_EXPR;
   but the question is how to recognize those cases.  */

    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:
      this_optab = and_optab;
      goto binop;

/* See comment above about TRUTH_AND_EXPR; it applies here too.  */
    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:
      this_optab = ior_optab;
      goto binop;

    case BIT_XOR_EXPR:
      this_optab = xor_optab;
      goto binop;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
	subtarget = 0;
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      return expand_shift (code, mode, op0, TREE_OPERAND (exp, 1), target,
			   unsignedp);

/* Could determine the answer when only additive constants differ.
   Also, the addition of one can be handled by changing the condition.  */
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      preexpand_calls (exp);
      temp = do_store_flag (exp, target, tmode != VOIDmode ? tmode : mode, 0);
      if (temp != 0)
	return temp;
      /* For foo != 0, load foo, and if it is nonzero load 1 instead. */
      if (code == NE_EXPR && integer_zerop (TREE_OPERAND (exp, 1))
	  && original_target
	  && GET_CODE (original_target) == REG
	  && (GET_MODE (original_target)
	      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	{
	  temp = expand_expr (TREE_OPERAND (exp, 0), original_target, VOIDmode, 0);
	  if (temp != original_target)
	    temp = copy_to_reg (temp);
	  op1 = gen_label_rtx ();
	  emit_cmp_insn (temp, const0_rtx, EQ, NULL_RTX,
			 GET_MODE (temp), unsignedp, 0);
	  emit_jump_insn (gen_beq (op1));
	  emit_move_insn (temp, const1_rtx);
	  emit_label (op1);
	  return temp;
	}
      /* If no set-flag instruction, must generate a conditional
	 store into a temporary variable.  Drop through
	 and handle this like && and ||.  */

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      if (target == 0 || ! safe_from_p (target, exp)
	  /* Make sure we don't have a hard reg (such as function's return
	     value) live across basic blocks, if not optimizing.  */
	  || (!optimize && GET_CODE (target) == REG
	      && REGNO (target) < FIRST_PSEUDO_REGISTER))
	target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);
      emit_clr_insn (target);
      op1 = gen_label_rtx ();
      jumpifnot (exp, op1);
      emit_0_to_1_insn (target);
      emit_label (op1);
      return target;

    case TRUTH_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      /* The parser is careful to generate TRUTH_NOT_EXPR
	 only with operands that are always zero or one.  */
      temp = expand_binop (mode, xor_optab, op0, const1_rtx,
			   target, 1, OPTAB_LIB_WIDEN);
      if (temp == 0)
	abort ();
      return temp;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      return expand_expr (TREE_OPERAND (exp, 1),
			  (ignore ? const0_rtx : target),
			  VOIDmode, 0);

    case COND_EXPR:
      {
	/* Note that COND_EXPRs whose type is a structure or union
	   are required to be constructed to contain assignments of
	   a temporary variable, so that we can evaluate them here
	   for side effect only.  If type is void, we must do likewise.  */

	/* If an arm of the branch requires a cleanup,
	   only that cleanup is performed.  */

	tree singleton = 0;
	tree binary_op = 0, unary_op = 0;
	tree old_cleanups = cleanups_this_call;
	cleanups_this_call = 0;

	/* If this is (A ? 1 : 0) and A is a condition, just evaluate it and
	   convert it to our mode, if necessary.  */
	if (integer_onep (TREE_OPERAND (exp, 1))
	    && integer_zerop (TREE_OPERAND (exp, 2))
	    && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<')
	  {
	    op0 = expand_expr (TREE_OPERAND (exp, 0), target, mode, modifier);
	    if (GET_MODE (op0) == mode)
	      return op0;
	    if (target == 0)
	      target = gen_reg_rtx (mode);
	    convert_move (target, op0, unsignedp);
	    return target;
	  }

	/* If we are not to produce a result, we have no target.  Otherwise,
	   if a target was specified use it; it will not be used as an
	   intermediate target unless it is safe.  If no target, use a 
	   temporary.  */

	if (mode == VOIDmode || ignore)
	  temp = 0;
	else if (original_target
		 && safe_from_p (original_target, TREE_OPERAND (exp, 0)))
	  temp = original_target;
	else if (mode == BLKmode)
	  {
	    if (TYPE_SIZE (type) == 0
		|| TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	      abort ();
	    temp = assign_stack_temp (BLKmode,
				      (TREE_INT_CST_LOW (TYPE_SIZE (type))
				       + BITS_PER_UNIT - 1)
				      / BITS_PER_UNIT, 0);
	  }
	else
	  temp = gen_reg_rtx (mode);

	/* Check for X ? A + B : A.  If we have this, we can copy
	   A to the output and conditionally add B.  Similarly for unary
	   operations.  Don't do this if X has side-effects because
	   those side effects might affect A or B and the "?" operation is
	   a sequence point in ANSI.  (We test for side effects later.)  */

	if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 1))) == '2'
	    && operand_equal_p (TREE_OPERAND (exp, 2),
				TREE_OPERAND (TREE_OPERAND (exp, 1), 0), 0))
	  singleton = TREE_OPERAND (exp, 2), binary_op = TREE_OPERAND (exp, 1);
	else if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 2))) == '2'
		 && operand_equal_p (TREE_OPERAND (exp, 1),
				     TREE_OPERAND (TREE_OPERAND (exp, 2), 0), 0))
	  singleton = TREE_OPERAND (exp, 1), binary_op = TREE_OPERAND (exp, 2);
	else if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 1))) == '1'
		 && operand_equal_p (TREE_OPERAND (exp, 2),
				     TREE_OPERAND (TREE_OPERAND (exp, 1), 0), 0))
	  singleton = TREE_OPERAND (exp, 2), unary_op = TREE_OPERAND (exp, 1);
	else if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 2))) == '1'
		 && operand_equal_p (TREE_OPERAND (exp, 1),
				     TREE_OPERAND (TREE_OPERAND (exp, 2), 0), 0))
	  singleton = TREE_OPERAND (exp, 1), unary_op = TREE_OPERAND (exp, 2);

	/* If we had X ? A + 1 : A and we can do the test of X as a store-flag
	   operation, do this as A + (X != 0).  Similarly for other simple
	   binary operators.  */
	if (singleton && binary_op
	    && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
	    && (TREE_CODE (binary_op) == PLUS_EXPR
		|| TREE_CODE (binary_op) == MINUS_EXPR
		|| TREE_CODE (binary_op) == BIT_IOR_EXPR
		|| TREE_CODE (binary_op) == BIT_XOR_EXPR
		|| TREE_CODE (binary_op) == BIT_AND_EXPR)
	    && integer_onep (TREE_OPERAND (binary_op, 1))
	    && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<')
	  {
	    rtx result;
	    optab boptab = (TREE_CODE (binary_op) == PLUS_EXPR ? add_optab
			    : TREE_CODE (binary_op) == MINUS_EXPR ? sub_optab
			    : TREE_CODE (binary_op) == BIT_IOR_EXPR ? ior_optab
			    : TREE_CODE (binary_op) == BIT_XOR_EXPR ? xor_optab
			    : and_optab);

	    /* If we had X ? A : A + 1, do this as A + (X == 0).

	       We have to invert the truth value here and then put it
	       back later if do_store_flag fails.  We cannot simply copy
	       TREE_OPERAND (exp, 0) to another variable and modify that
	       because invert_truthvalue can modify the tree pointed to
	       by its argument.  */
	    if (singleton == TREE_OPERAND (exp, 1))
	      TREE_OPERAND (exp, 0)
		= invert_truthvalue (TREE_OPERAND (exp, 0));

	    result = do_store_flag (TREE_OPERAND (exp, 0),
				    (safe_from_p (temp, singleton)
				     ? temp : NULL_RTX),
				    mode, BRANCH_COST <= 1);

	    if (result)
	      {
		op1 = expand_expr (singleton, NULL_RTX, VOIDmode, 0);
		return expand_binop (mode, boptab, op1, result, temp,
				     unsignedp, OPTAB_LIB_WIDEN);
	      }
	    else if (singleton == TREE_OPERAND (exp, 1))
	      TREE_OPERAND (exp, 0)
		= invert_truthvalue (TREE_OPERAND (exp, 0));
	  }
	    
	NO_DEFER_POP;
	op0 = gen_label_rtx ();

	if (singleton && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0)))
	  {
	    if (temp != 0)
	      {
		/* If the target conflicts with the other operand of the
		   binary op, we can't use it.  Also, we can't use the target
		   if it is a hard register, because evaluating the condition
		   might clobber it.  */
		if ((binary_op
		     && ! safe_from_p (temp, TREE_OPERAND (binary_op, 1)))
		    || (GET_CODE (temp) == REG
			&& REGNO (temp) < FIRST_PSEUDO_REGISTER))
		  temp = gen_reg_rtx (mode);
		store_expr (singleton, temp, 0);
	      }
	    else
	      expand_expr (singleton,
			   ignore ? const1_rtx : NULL_RTX, VOIDmode, 0);
	    if (cleanups_this_call)
	      {
		sorry ("aggregate value in COND_EXPR");
		cleanups_this_call = 0;
	      }
	    if (singleton == TREE_OPERAND (exp, 1))
	      jumpif (TREE_OPERAND (exp, 0), op0);
	    else
	      jumpifnot (TREE_OPERAND (exp, 0), op0);

	    if (binary_op && temp == 0)
	      /* Just touch the other operand.  */
	      expand_expr (TREE_OPERAND (binary_op, 1),
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	    else if (binary_op)
	      store_expr (build (TREE_CODE (binary_op), type,
				 make_tree (type, temp),
				 TREE_OPERAND (binary_op, 1)),
			  temp, 0);
	    else
	      store_expr (build1 (TREE_CODE (unary_op), type,
				  make_tree (type, temp)),
			  temp, 0);
	    op1 = op0;
	  }
#if 0
	/* This is now done in jump.c and is better done there because it
	   produces shorter register lifetimes.  */
	   
	/* Check for both possibilities either constants or variables
	   in registers (but not the same as the target!).  If so, can
	   save branches by assigning one, branching, and assigning the
	   other.  */
	else if (temp && GET_MODE (temp) != BLKmode
		 && (TREE_CONSTANT (TREE_OPERAND (exp, 1))
		     || ((TREE_CODE (TREE_OPERAND (exp, 1)) == PARM_DECL
			  || TREE_CODE (TREE_OPERAND (exp, 1)) == VAR_DECL)
			 && DECL_RTL (TREE_OPERAND (exp, 1))
			 && GET_CODE (DECL_RTL (TREE_OPERAND (exp, 1))) == REG
			 && DECL_RTL (TREE_OPERAND (exp, 1)) != temp))
		 && (TREE_CONSTANT (TREE_OPERAND (exp, 2))
		     || ((TREE_CODE (TREE_OPERAND (exp, 2)) == PARM_DECL
			  || TREE_CODE (TREE_OPERAND (exp, 2)) == VAR_DECL)
			 && DECL_RTL (TREE_OPERAND (exp, 2))
			 && GET_CODE (DECL_RTL (TREE_OPERAND (exp, 2))) == REG
			 && DECL_RTL (TREE_OPERAND (exp, 2)) != temp)))
	  {
	    if (GET_CODE (temp) == REG && REGNO (temp) < FIRST_PSEUDO_REGISTER)
	      temp = gen_reg_rtx (mode);
	    store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    jumpifnot (TREE_OPERAND (exp, 0), op0);
	    store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    op1 = op0;
	  }
#endif
	/* Check for A op 0 ? A : FOO and A op 0 ? FOO : A where OP is any
	   comparison operator.  If we have one of these cases, set the
	   output to A, branch on A (cse will merge these two references),
	   then set the output to FOO.  */
	else if (temp
		 && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<'
		 && integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
		 && operand_equal_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				     TREE_OPERAND (exp, 1), 0)
		 && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
		 && safe_from_p (temp, TREE_OPERAND (exp, 2)))
	  {
	    if (GET_CODE (temp) == REG && REGNO (temp) < FIRST_PSEUDO_REGISTER)
	      temp = gen_reg_rtx (mode);
	    store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    jumpif (TREE_OPERAND (exp, 0), op0);
	    store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    op1 = op0;
	  }
	else if (temp
		 && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<'
		 && integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
		 && operand_equal_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				     TREE_OPERAND (exp, 2), 0)
		 && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
		 && safe_from_p (temp, TREE_OPERAND (exp, 1)))
	  {
	    if (GET_CODE (temp) == REG && REGNO (temp) < FIRST_PSEUDO_REGISTER)
	      temp = gen_reg_rtx (mode);
	    store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    jumpifnot (TREE_OPERAND (exp, 0), op0);
	    store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    op1 = op0;
	  }
	else
	  {
	    op1 = gen_label_rtx ();
	    jumpifnot (TREE_OPERAND (exp, 0), op0);
	    if (temp != 0)
	      store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    else
	      expand_expr (TREE_OPERAND (exp, 1),
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	    if (cleanups_this_call)
	      {
		sorry ("aggregate value in COND_EXPR");
		cleanups_this_call = 0;
	      }

	    emit_queue ();
	    emit_jump_insn (gen_jump (op1));
	    emit_barrier ();
	    emit_label (op0);
	    if (temp != 0)
	      store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    else
	      expand_expr (TREE_OPERAND (exp, 2),
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	  }

	if (cleanups_this_call)
	  {
	    sorry ("aggregate value in COND_EXPR");
	    cleanups_this_call = 0;
	  }

	emit_queue ();
	emit_label (op1);
	OK_DEFER_POP;
	cleanups_this_call = old_cleanups;
	return temp;
      }

    case TARGET_EXPR:
      {
	/* Something needs to be initialized, but we didn't know
	   where that thing was when building the tree.  For example,
	   it could be the return value of a function, or a parameter
	   to a function which lays down in the stack, or a temporary
	   variable which must be passed by reference.

	   We guarantee that the expression will either be constructed
	   or copied into our original target.  */

	tree slot = TREE_OPERAND (exp, 0);
	tree exp1;

	if (TREE_CODE (slot) != VAR_DECL)
	  abort ();

	if (target == 0)
	  {
	    if (DECL_RTL (slot) != 0)
	      {
		target = DECL_RTL (slot);
		/* If we have already expanded the slot, so don't do
		   it again.  (mrs)  */
		if (TREE_OPERAND (exp, 1) == NULL_TREE)
		  return target;
	      }
	    else
	      {
		target = assign_stack_temp (mode, int_size_in_bytes (type), 0);
		/* All temp slots at this level must not conflict.  */
		preserve_temp_slots (target);
		DECL_RTL (slot) = target;
	      }

#if 0
	    /* I bet this needs to be done, and I bet that it needs to
	       be above, inside the else clause.  The reason is
	       simple, how else is it going to get cleaned up? (mrs)

	       The reason is probably did not work before, and was
	       commented out is because this was re-expanding already
	       expanded target_exprs (target == 0 and DECL_RTL (slot)
	       != 0) also cleaning them up many times as well.  :-( */

	    /* Since SLOT is not known to the called function
	       to belong to its stack frame, we must build an explicit
	       cleanup.  This case occurs when we must build up a reference
	       to pass the reference as an argument.  In this case,
	       it is very likely that such a reference need not be
	       built here.  */

	    if (TREE_OPERAND (exp, 2) == 0)
	      TREE_OPERAND (exp, 2) = maybe_build_cleanup (slot);
	    if (TREE_OPERAND (exp, 2))
	      cleanups_this_call = tree_cons (NULL_TREE, TREE_OPERAND (exp, 2),
					      cleanups_this_call);
#endif
	  }
	else
	  {
	    /* This case does occur, when expanding a parameter which
	       needs to be constructed on the stack.  The target
	       is the actual stack address that we want to initialize.
	       The function we call will perform the cleanup in this case.  */

	    DECL_RTL (slot) = target;
	  }

	exp1 = TREE_OPERAND (exp, 1);
	/* Mark it as expanded.  */
	TREE_OPERAND (exp, 1) = NULL_TREE;

	return expand_expr (exp1, target, tmode, modifier);
      }

    case INIT_EXPR:
      {
	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);
	tree noncopied_parts = 0;
	tree lhs_type = TREE_TYPE (lhs);

	temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
	if (TYPE_NONCOPIED_PARTS (lhs_type) != 0 && !fixed_type_p (rhs))
	  noncopied_parts = init_noncopied_parts (stabilize_reference (lhs),
						  TYPE_NONCOPIED_PARTS (lhs_type));
	while (noncopied_parts != 0)
	  {
	    expand_assignment (TREE_VALUE (noncopied_parts),
			       TREE_PURPOSE (noncopied_parts), 0, 0);
	    noncopied_parts = TREE_CHAIN (noncopied_parts);
	  }
	return temp;
      }

    case MODIFY_EXPR:
      {
	/* If lhs is complex, expand calls in rhs before computing it.
	   That's so we don't compute a pointer and save it over a call.
	   If lhs is simple, compute it first so we can give it as a
	   target if the rhs is just a call.  This avoids an extra temp and copy
	   and that prevents a partial-subsumption which makes bad code.
	   Actually we could treat component_ref's of vars like vars.  */

	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);
	tree noncopied_parts = 0;
	tree lhs_type = TREE_TYPE (lhs);

	temp = 0;

	if (TREE_CODE (lhs) != VAR_DECL
	    && TREE_CODE (lhs) != RESULT_DECL
	    && TREE_CODE (lhs) != PARM_DECL)
	  preexpand_calls (exp);

	/* Check for |= or &= of a bitfield of size one into another bitfield
	   of size 1.  In this case, (unless we need the result of the
	   assignment) we can do this more efficiently with a
	   test followed by an assignment, if necessary.

	   ??? At this point, we can't get a BIT_FIELD_REF here.  But if
	   things change so we do, this code should be enhanced to
	   support it.  */
	if (ignore
	    && TREE_CODE (lhs) == COMPONENT_REF
	    && (TREE_CODE (rhs) == BIT_IOR_EXPR
		|| TREE_CODE (rhs) == BIT_AND_EXPR)
	    && TREE_OPERAND (rhs, 0) == lhs
	    && TREE_CODE (TREE_OPERAND (rhs, 1)) == COMPONENT_REF
	    && TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (lhs, 1))) == 1
	    && TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (TREE_OPERAND (rhs, 1), 1))) == 1)
	  {
	    rtx label = gen_label_rtx ();

	    do_jump (TREE_OPERAND (rhs, 1),
		     TREE_CODE (rhs) == BIT_IOR_EXPR ? label : 0,
		     TREE_CODE (rhs) == BIT_AND_EXPR ? label : 0);
	    expand_assignment (lhs, convert (TREE_TYPE (rhs),
					     (TREE_CODE (rhs) == BIT_IOR_EXPR
					      ? integer_one_node
					      : integer_zero_node)),
			       0, 0);
	    do_pending_stack_adjust ();
	    emit_label (label);
	    return const0_rtx;
	  }

	if (TYPE_NONCOPIED_PARTS (lhs_type) != 0
	    && ! (fixed_type_p (lhs) && fixed_type_p (rhs)))
	  noncopied_parts = save_noncopied_parts (stabilize_reference (lhs),
						  TYPE_NONCOPIED_PARTS (lhs_type));

	temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
	while (noncopied_parts != 0)
	  {
	    expand_assignment (TREE_PURPOSE (noncopied_parts),
			       TREE_VALUE (noncopied_parts), 0, 0);
	    noncopied_parts = TREE_CHAIN (noncopied_parts);
	  }
	return temp;
      }

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      return expand_increment (exp, 0);

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Faster to treat as pre-increment if result is not used.  */
      return expand_increment (exp, ! ignore);

    case ADDR_EXPR:
      /* Are we taking the address of a nested function?  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == FUNCTION_DECL
	  && decl_function_context (TREE_OPERAND (exp, 0)) != 0)
	{
	  op0 = trampoline_address (TREE_OPERAND (exp, 0));
	  op0 = force_operand (op0, target);
	}
      else
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode,
			     (modifier == EXPAND_INITIALIZER
			      ? modifier : EXPAND_CONST_ADDRESS));
	  if (GET_CODE (op0) != MEM)
	    abort ();
  
	  if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	    return XEXP (op0, 0);
	  op0 = force_operand (XEXP (op0, 0), target);
	}
      if (flag_force_addr && GET_CODE (op0) != REG)
	return force_reg (Pmode, op0);
      return op0;

    case ENTRY_VALUE_EXPR:
      abort ();

    /* COMPLEX type for Extended Pascal & Fortran  */
    case COMPLEX_EXPR:
      {
	enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));

	rtx prev;

	/* Get the rtx code of the operands.  */
	op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
	op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);

	if (! target)
	  target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

	prev = get_last_insn ();

	/* Tell flow that the whole of the destination is being set.  */
	if (GET_CODE (target) == REG)
	  emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

	/* Move the real (op0) and imaginary (op1) parts to their location.  */
	emit_move_insn (gen_realpart (mode, target), op0);
	emit_move_insn (gen_imagpart (mode, target), op1);

	/* Complex construction should appear as a single unit.  */
	group_insns (prev);

	return target;
      }

    case REALPART_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      return gen_realpart (mode, op0);
      
    case IMAGPART_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      return gen_imagpart (mode, op0);

    case CONJ_EXPR:
      {
	enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));
	rtx imag_t;
	rtx prev;
	
	op0  = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);

	if (! target)
	  target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));
								    
	prev = get_last_insn ();

	/* Tell flow that the whole of the destination is being set.  */
	if (GET_CODE (target) == REG)
	  emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

	/* Store the realpart and the negated imagpart to target.  */
	emit_move_insn (gen_realpart (mode, target), gen_realpart (mode, op0));

	imag_t = gen_imagpart (mode, target);
	temp   = expand_unop (mode, neg_optab,
			      gen_imagpart (mode, op0), imag_t, 0);
	if (temp != imag_t)
	  emit_move_insn (imag_t, temp);

	/* Conjugate should appear as a single unit */
	group_insns (prev);

	return target;
      }

    case ERROR_MARK:
      return const0_rtx;

    default:
      return (*lang_expand_expr) (exp, target, tmode, modifier);
    }

  /* Here to do an ordinary binary operator, generating an instruction
     from the optab already placed in `this_optab'.  */
 binop:
  preexpand_calls (exp);
  if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1)))
    subtarget = 0;
  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
 binop2:
  temp = expand_binop (mode, this_optab, op0, op1, target,
		       unsignedp, OPTAB_LIB_WIDEN);
  if (temp == 0)
    abort ();
  return temp;
}

/* Return the alignment in bits of EXP, a pointer valued expression.
   But don't return more than MAX_ALIGN no matter what.
   The alignment returned is, by default, the alignment of the thing that
   EXP points to (if it is not a POINTER_TYPE, 0 is returned).

   Otherwise, look at the expression to see if we can do better, i.e., if the
   expression is actually pointing at an object whose alignment is tighter.  */

static int
get_pointer_alignment (exp, max_align)
     tree exp;
     unsigned max_align;
{
  unsigned align, inner;

  if (TREE_CODE (TREE_TYPE (exp)) != POINTER_TYPE)
    return 0;

  align = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
  align = MIN (align, max_align);

  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case NON_LVALUE_EXPR:
	  exp = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (TREE_TYPE (exp)) != POINTER_TYPE)
	    return align;
	  inner = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
	  inner = MIN (inner, max_align);
	  align = MAX (align, inner);
	  break;

	case PLUS_EXPR:
	  /* If sum of pointer + int, restrict our maximum alignment to that
	     imposed by the integer.  If not, we can't do any better than
	     ALIGN.  */
	  if (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST)
	    return align;

	  while (((TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)) * BITS_PER_UNIT)
		  & (max_align - 1))
		 != 0)
	    max_align >>= 1;

	  exp = TREE_OPERAND (exp, 0);
	  break;

	case ADDR_EXPR:
	  /* See what we are pointing at and look at its alignment.  */
	  exp = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (exp) == FUNCTION_DECL)
	    align = MAX (align, FUNCTION_BOUNDARY);
	  else if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
	    align = MAX (align, DECL_ALIGN (exp));
#ifdef CONSTANT_ALIGNMENT
	  else if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'c')
	    align = CONSTANT_ALIGNMENT (exp, align);
#endif
	  return MIN (align, max_align);

	default:
	  return align;
	}
    }
}

/* Return the tree node and offset if a given argument corresponds to
   a string constant.  */

static tree
string_constant (arg, ptr_offset)
     tree arg;
     tree *ptr_offset;
{
  STRIP_NOPS (arg);

  if (TREE_CODE (arg) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST)
    {
      *ptr_offset = integer_zero_node;
      return TREE_OPERAND (arg, 0);
    }
  else if (TREE_CODE (arg) == PLUS_EXPR)
    {
      tree arg0 = TREE_OPERAND (arg, 0);
      tree arg1 = TREE_OPERAND (arg, 1);

      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg0) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == STRING_CST)
	{
	  *ptr_offset = arg1;
	  return TREE_OPERAND (arg0, 0);
	}
      else if (TREE_CODE (arg1) == ADDR_EXPR
	       && TREE_CODE (TREE_OPERAND (arg1, 0)) == STRING_CST)
	{
	  *ptr_offset = arg0;
	  return TREE_OPERAND (arg1, 0);
	}
    }

  return 0;
}

/* Compute the length of a C string.  TREE_STRING_LENGTH is not the right
   way, because it could contain a zero byte in the middle.
   TREE_STRING_LENGTH is the size of the character array, not the string.

   Unfortunately, string_constant can't access the values of const char
   arrays with initializers, so neither can we do so here.  */

static tree
c_strlen (src)
     tree src;
{
  tree offset_node;
  int offset, max;
  char *ptr;

  src = string_constant (src, &offset_node);
  if (src == 0)
    return 0;
  max = TREE_STRING_LENGTH (src);
  ptr = TREE_STRING_POINTER (src);
  if (offset_node && TREE_CODE (offset_node) != INTEGER_CST)
    {
      /* If the string has an internal zero byte (e.g., "foo\0bar"), we can't
	 compute the offset to the following null if we don't know where to
	 start searching for it.  */
      int i;
      for (i = 0; i < max; i++)
	if (ptr[i] == 0)
	  return 0;
      /* We don't know the starting offset, but we do know that the string
	 has no internal zero bytes.  We can assume that the offset falls
	 within the bounds of the string; otherwise, the programmer deserves
	 what he gets.  Subtract the offset from the length of the string,
	 and return that.  */
      /* This would perhaps not be valid if we were dealing with named
         arrays in addition to literal string constants.  */
      return size_binop (MINUS_EXPR, size_int (max), offset_node);
    }

  /* We have a known offset into the string.  Start searching there for
     a null character.  */
  if (offset_node == 0)
    offset = 0;
  else
    {
      /* Did we get a long long offset?  If so, punt.  */
      if (TREE_INT_CST_HIGH (offset_node) != 0)
	return 0;
      offset = TREE_INT_CST_LOW (offset_node);
    }
  /* If the offset is known to be out of bounds, warn, and call strlen at
     runtime.  */
  if (offset < 0 || offset > max)
    {
      warning ("offset outside bounds of constant string");
      return 0;
    }
  /* Use strlen to search for the first zero byte.  Since any strings
     constructed with build_string will have nulls appended, we win even
     if we get handed something like (char[4])"abcd".

     Since OFFSET is our starting index into the string, no further
     calculation is needed.  */
  return size_int (strlen (ptr + offset));
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget;
     enum machine_mode mode;
     int ignore;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  rtx op0;
  rtx lab1, insns;
  enum machine_mode value_mode = TYPE_MODE (TREE_TYPE (exp));
  optab builtin_optab;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_FABS:
      /* build_function_call changes these into ABS_EXPR.  */
      abort ();

    case BUILT_IN_SIN:
    case BUILT_IN_COS:
    case BUILT_IN_FSQRT:
      /* If not optimizing, call the library function.  */
      if (! optimize)
	break;

      if (arglist == 0
	  /* Arg could be wrong type if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != REAL_TYPE)
	return CONST0_RTX (TYPE_MODE (TREE_TYPE (exp)));

      /* Stabilize and compute the argument.  */
      if (TREE_CODE (TREE_VALUE (arglist)) != VAR_DECL
	  && TREE_CODE (TREE_VALUE (arglist)) != PARM_DECL)
	{
	  exp = copy_node (exp);
	  arglist = copy_node (arglist);
	  TREE_OPERAND (exp, 1) = arglist;
	  TREE_VALUE (arglist) = save_expr (TREE_VALUE (arglist));
	}
      op0 = expand_expr (TREE_VALUE (arglist), subtarget, VOIDmode, 0);

      /* Make a suitable register to place result in.  */
      target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

      emit_queue ();
      start_sequence ();

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_SIN:
	  builtin_optab = sin_optab; break;
	case BUILT_IN_COS:
	  builtin_optab = cos_optab; break;
	case BUILT_IN_FSQRT:
	  builtin_optab = sqrt_optab; break;
	default:
	  abort ();
	}

      /* Compute into TARGET.
	 Set TARGET to wherever the result comes back.  */
      target = expand_unop (TYPE_MODE (TREE_TYPE (TREE_VALUE (arglist))),
			    builtin_optab, op0, target, 0);

      /* If we were unable to expand via the builtin, stop the
	 sequence (without outputting the insns) and break, causing
	 a call the the library function.  */
      if (target == 0)
	{
	  end_sequence ();
	  break;
        }

      /* Check the results by default.  But if flag_fast_math is turned on,
	 then assume sqrt will always be called with valid arguments.  */

      if (! flag_fast_math)
	{
	  /* Don't define the builtin FP instructions
	     if your machine is not IEEE.  */
	  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
	    abort ();

	  lab1 = gen_label_rtx ();

	  /* Test the result; if it is NaN, set errno=EDOM because
	     the argument was not in the domain.  */
	  emit_cmp_insn (target, target, EQ, 0, GET_MODE (target), 0, 0);
	  emit_jump_insn (gen_beq (lab1));

#if TARGET_EDOM
	  {
#ifdef GEN_ERRNO_RTX
	    rtx errno_rtx = GEN_ERRNO_RTX;
#else
	    rtx errno_rtx
	      = gen_rtx (MEM, word_mode, gen_rtx (SYMBOL_REF, Pmode, "*errno"));
#endif

	    emit_move_insn (errno_rtx, GEN_INT (TARGET_EDOM));
	  }
#else
	  /* We can't set errno=EDOM directly; let the library call do it.
	     Pop the arguments right away in case the call gets deleted. */
	  NO_DEFER_POP;
	  expand_call (exp, target, 0);
	  OK_DEFER_POP;
#endif

	  emit_label (lab1);
	}

      /* Output the entire sequence. */
      insns = get_insns ();
      end_sequence ();
      emit_insns (insns);
 
      return target;

    case BUILT_IN_SAVEREGS:
      /* Don't do __builtin_saveregs more than once in a function.
	 Save the result of the first call and reuse it.  */
      if (saveregs_value != 0)
	return saveregs_value;
      {
	/* When this function is called, it means that registers must be
	   saved on entry to this function.  So we migrate the
	   call to the first insn of this function.  */
	rtx temp;
	rtx seq;
	rtx valreg, saved_valreg;

	/* Now really call the function.  `expand_call' does not call
	   expand_builtin, so there is no danger of infinite recursion here.  */
	start_sequence ();

#ifdef EXPAND_BUILTIN_SAVEREGS
	/* Do whatever the machine needs done in this case.  */
	temp = EXPAND_BUILTIN_SAVEREGS (arglist);
#else
	/* The register where the function returns its value
	   is likely to have something else in it, such as an argument.
	   So preserve that register around the call.  */
	if (value_mode != VOIDmode)
	  {
	    valreg = hard_libcall_value (value_mode);
	    saved_valreg = gen_reg_rtx (value_mode);
	    emit_move_insn (saved_valreg, valreg);
	  }

	/* Generate the call, putting the value in a pseudo.  */
	temp = expand_call (exp, target, ignore);

	if (value_mode != VOIDmode)
	  emit_move_insn (valreg, saved_valreg);
#endif

	seq = get_insns ();
	end_sequence ();

	saveregs_value = temp;

	/* This won't work inside a SEQUENCE--it really has to be
	   at the start of the function.  */
	if (in_sequence_p ())
	  {
	    /* Better to do this than to crash.  */
	    error ("`va_start' used within `({...})'");
	    return temp;
	  }

	/* Put the sequence after the NOTE that starts the function.  */
	emit_insns_before (seq, NEXT_INSN (get_insns ()));
	return temp;
      }

      /* __builtin_args_info (N) returns word N of the arg space info
	 for the current function.  The number and meanings of words
	 is controlled by the definition of CUMULATIVE_ARGS.  */
    case BUILT_IN_ARGS_INFO:
      {
	int nwords = sizeof (CUMULATIVE_ARGS) / sizeof (int);
	int i;
	int *word_ptr = (int *) &current_function_args_info;
	tree type, elts, result;

	if (sizeof (CUMULATIVE_ARGS) % sizeof (int) != 0)
	  fatal ("CUMULATIVE_ARGS type defined badly; see %s, line %d",
		 __FILE__, __LINE__);

	if (arglist != 0)
	  {
	    tree arg = TREE_VALUE (arglist);
	    if (TREE_CODE (arg) != INTEGER_CST)
	      error ("argument of __builtin_args_info must be constant");
	    else
	      {
		int wordnum = TREE_INT_CST_LOW (arg);

		if (wordnum < 0 || wordnum >= nwords)
		  error ("argument of __builtin_args_info out of range");
		else
		  return GEN_INT (word_ptr[wordnum]);
	      }
	  }
	else
	  error ("missing argument in __builtin_args_info");

	return const0_rtx;

#if 0
	for (i = 0; i < nwords; i++)
	  elts = tree_cons (NULL_TREE, build_int_2 (word_ptr[i], 0));

	type = build_array_type (integer_type_node,
				 build_index_type (build_int_2 (nwords, 0)));
	result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (elts));
	TREE_CONSTANT (result) = 1;
	TREE_STATIC (result) = 1;
	result = build (INDIRECT_REF, build_pointer_type (type), result);
	TREE_CONSTANT (result) = 1;
	return expand_expr (result, NULL_RTX, VOIDmode, 0);
#endif
      }

      /* Return the address of the first anonymous stack arg.  */
    case BUILT_IN_NEXT_ARG:
      {
	tree fntype = TREE_TYPE (current_function_decl);
	if (!(TYPE_ARG_TYPES (fntype) != 0
	      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		  != void_type_node)))
	  {
	    error ("`va_start' used in function with fixed args");
	    return const0_rtx;
	  }
      }

      return expand_binop (Pmode, add_optab,
			   current_function_internal_arg_pointer,
			   current_function_arg_offset_rtx,
			   NULL_RTX, 0, OPTAB_LIB_WIDEN);

    case BUILT_IN_CLASSIFY_TYPE:
      if (arglist != 0)
	{
	  tree type = TREE_TYPE (TREE_VALUE (arglist));
	  enum tree_code code = TREE_CODE (type);
	  if (code == VOID_TYPE)
	    return GEN_INT (void_type_class);
	  if (code == INTEGER_TYPE)
	    return GEN_INT (integer_type_class);
	  if (code == CHAR_TYPE)
	    return GEN_INT (char_type_class);
	  if (code == ENUMERAL_TYPE)
	    return GEN_INT (enumeral_type_class);
	  if (code == BOOLEAN_TYPE)
	    return GEN_INT (boolean_type_class);
	  if (code == POINTER_TYPE)
	    return GEN_INT (pointer_type_class);
	  if (code == REFERENCE_TYPE)
	    return GEN_INT (reference_type_class);
	  if (code == OFFSET_TYPE)
	    return GEN_INT (offset_type_class);
	  if (code == REAL_TYPE)
	    return GEN_INT (real_type_class);
	  if (code == COMPLEX_TYPE)
	    return GEN_INT (complex_type_class);
	  if (code == FUNCTION_TYPE)
	    return GEN_INT (function_type_class);
	  if (code == METHOD_TYPE)
	    return GEN_INT (method_type_class);
	  if (code == RECORD_TYPE)
	    return GEN_INT (record_type_class);
	  if (code == UNION_TYPE)
	    return GEN_INT (union_type_class);
	  if (code == ARRAY_TYPE)
	    return GEN_INT (array_type_class);
	  if (code == STRING_TYPE)
	    return GEN_INT (string_type_class);
	  if (code == SET_TYPE)
	    return GEN_INT (set_type_class);
	  if (code == FILE_TYPE)
	    return GEN_INT (file_type_class);
	  if (code == LANG_TYPE)
	    return GEN_INT (lang_type_class);
	}
      return GEN_INT (no_type_class);

    case BUILT_IN_CONSTANT_P:
      if (arglist == 0)
	return const0_rtx;
      else
	return (TREE_CODE_CLASS (TREE_CODE (TREE_VALUE (arglist))) == 'c'
		? const1_rtx : const0_rtx);

    case BUILT_IN_FRAME_ADDRESS:
      /* The argument must be a nonnegative integer constant.
	 It counts the number of frames to scan up the stack.
	 The value is the address of that frame.  */
    case BUILT_IN_RETURN_ADDRESS:
      /* The argument must be a nonnegative integer constant.
	 It counts the number of frames to scan up the stack.
	 The value is the return address saved in that frame.  */
      if (arglist == 0)
	/* Warning about missing arg was already issued.  */
	return const0_rtx;
      else if (TREE_CODE (TREE_VALUE (arglist)) != INTEGER_CST)
	{
	  error ("invalid arg to __builtin_return_address");
	  return const0_rtx;
	}
      else if (tree_int_cst_lt (TREE_VALUE (arglist), integer_zero_node))
	{
	  error ("invalid arg to __builtin_return_address");
	  return const0_rtx;
	}
      else
	{
	  int count = TREE_INT_CST_LOW (TREE_VALUE (arglist)); 
	  rtx tem = frame_pointer_rtx;
	  int i;

	  /* Scan back COUNT frames to the specified frame.  */
	  for (i = 0; i < count; i++)
	    {
	      /* Assume the dynamic chain pointer is in the word that
		 the frame address points to, unless otherwise specified.  */
#ifdef DYNAMIC_CHAIN_ADDRESS
	      tem = DYNAMIC_CHAIN_ADDRESS (tem);
#endif
	      tem = memory_address (Pmode, tem);
	      tem = copy_to_reg (gen_rtx (MEM, Pmode, tem));
	    }

	  /* For __builtin_frame_address, return what we've got.  */
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    return tem;

	  /* For __builtin_return_address,
	     Get the return address from that frame.  */
#ifdef RETURN_ADDR_RTX
	  return RETURN_ADDR_RTX (count, tem);
#else
	  tem = memory_address (Pmode,
				plus_constant (tem, GET_MODE_SIZE (Pmode)));
	  return copy_to_reg (gen_rtx (MEM, Pmode, tem));
#endif
	}

    case BUILT_IN_ALLOCA:
      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	return const0_rtx;
      current_function_calls_alloca = 1;
      /* Compute the argument.  */
      op0 = expand_expr (TREE_VALUE (arglist), NULL_RTX, VOIDmode, 0);

      /* Allocate the desired space.  */
      target = allocate_dynamic_stack_space (op0, target, BITS_PER_UNIT);

      /* Record the new stack level for nonlocal gotos.  */
      if (nonlocal_goto_handler_slot != 0)
	emit_stack_save (SAVE_NONLOCAL, &nonlocal_goto_stack_level, NULL_RTX);
      return target;

    case BUILT_IN_FFS:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	return const0_rtx;

      /* Compute the argument.  */
      op0 = expand_expr (TREE_VALUE (arglist), subtarget, VOIDmode, 0);
      /* Compute ffs, into TARGET if possible.
	 Set TARGET to wherever the result comes back.  */
      target = expand_unop (TYPE_MODE (TREE_TYPE (TREE_VALUE (arglist))),
			    ffs_optab, op0, target, 1);
      if (target == 0)
	abort ();
      return target;

    case BUILT_IN_STRLEN:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
	return const0_rtx;
      else
	{
	  tree src = TREE_VALUE (arglist);
	  tree len = c_strlen (src);

	  int align
	    = get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;

	  rtx result, src_rtx, char_rtx;
	  enum machine_mode insn_mode = value_mode, char_mode;
	  enum insn_code icode;

	  /* If the length is known, just return it. */
	  if (len != 0)
	    return expand_expr (len, target, mode, 0);

	  /* If SRC is not a pointer type, don't do this operation inline. */
	  if (align == 0)
	    break;

	  /* Call a function if we can't compute strlen in the right mode. */

	  while (insn_mode != VOIDmode)
	    {
	      icode = strlen_optab->handlers[(int) insn_mode].insn_code;
	      if (icode != CODE_FOR_nothing)
		break;

	      insn_mode = GET_MODE_WIDER_MODE (insn_mode);
	    }
	  if (insn_mode == VOIDmode)
	    break;

	  /* Make a place to write the result of the instruction.  */
	  result = target;
	  if (! (result != 0
		 && GET_CODE (result) == REG
		 && GET_MODE (result) == insn_mode
		 && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	    result = gen_reg_rtx (insn_mode);

	  /* Make sure the operands are acceptable to the predicates.  */

	  if (! (*insn_operand_predicate[(int)icode][0]) (result, insn_mode))
	    result = gen_reg_rtx (insn_mode);

	  src_rtx = memory_address (BLKmode,
				    expand_expr (src, NULL_RTX, Pmode,
						 EXPAND_NORMAL));
	  if (! (*insn_operand_predicate[(int)icode][1]) (src_rtx, Pmode))
	    src_rtx = copy_to_mode_reg (Pmode, src_rtx);

	  char_rtx = const0_rtx;
	  char_mode = insn_operand_mode[(int)icode][2];
	  if (! (*insn_operand_predicate[(int)icode][2]) (char_rtx, char_mode))
	    char_rtx = copy_to_mode_reg (char_mode, char_rtx);

	  emit_insn (GEN_FCN (icode) (result,
				      gen_rtx (MEM, BLKmode, src_rtx),
				      char_rtx, GEN_INT (align)));

	  /* Return the value in the proper mode for this function.  */
	  if (GET_MODE (result) == value_mode)
	    return result;
	  else if (target != 0)
	    {
	      convert_move (target, result, 0);
	      return target;
	    }
	  else
	    return convert_to_mode (value_mode, result, 0);
	}

    case BUILT_IN_STRCPY:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE)
	return const0_rtx;
      else
	{
	  tree len = c_strlen (TREE_VALUE (TREE_CHAIN (arglist)));

	  if (len == 0)
	    break;

	  len = size_binop (PLUS_EXPR, len, integer_one_node);

	  chainon (arglist, build_tree_list (NULL_TREE, len));
	}

      /* Drops in.  */
    case BUILT_IN_MEMCPY:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
	return const0_rtx;
      else
	{
	  tree dest = TREE_VALUE (arglist);
	  tree src = TREE_VALUE (TREE_CHAIN (arglist));
	  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

	  int src_align
	    = get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	  int dest_align
	    = get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	  rtx dest_rtx;

	  /* If either SRC or DEST is not a pointer type, don't do
	     this operation in-line.  */
	  if (src_align == 0 || dest_align == 0)
	    {
	      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STRCPY)
		TREE_CHAIN (TREE_CHAIN (arglist)) = 0;
	      break;
	    }

	  dest_rtx = expand_expr (dest, NULL_RTX, Pmode, EXPAND_NORMAL);

	  /* Copy word part most expediently.  */
	  emit_block_move (gen_rtx (MEM, BLKmode,
				    memory_address (BLKmode, dest_rtx)),
			   gen_rtx (MEM, BLKmode,
				    memory_address (BLKmode,
						    expand_expr (src, NULL_RTX,
								 Pmode,
								 EXPAND_NORMAL))),
			   expand_expr (len, NULL_RTX, VOIDmode, 0),
			   MIN (src_align, dest_align));
	  return dest_rtx;
	}

/* These comparison functions need an instruction that returns an actual
   index.  An ordinary compare that just sets the condition codes
   is not enough.  */
#ifdef HAVE_cmpstrsi
    case BUILT_IN_STRCMP:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE)
	return const0_rtx;
      else if (!HAVE_cmpstrsi)
	break;
      {
	tree arg1 = TREE_VALUE (arglist);
	tree arg2 = TREE_VALUE (TREE_CHAIN (arglist));
	tree offset;
	tree len, len2;

	len = c_strlen (arg1);
	if (len)
	  len = size_binop (PLUS_EXPR, integer_one_node, len);
	len2 = c_strlen (arg2);
	if (len2)
	  len2 = size_binop (PLUS_EXPR, integer_one_node, len2);

	/* If we don't have a constant length for the first, use the length
	   of the second, if we know it.  We don't require a constant for
	   this case; some cost analysis could be done if both are available
	   but neither is constant.  For now, assume they're equally cheap.

	   If both strings have constant lengths, use the smaller.  This
	   could arise if optimization results in strcpy being called with
	   two fixed strings, or if the code was machine-generated.  We should
	   add some code to the `memcmp' handler below to deal with such
	   situations, someday.  */
	if (!len || TREE_CODE (len) != INTEGER_CST)
	  {
	    if (len2)
	      len = len2;
	    else if (len == 0)
	      break;
	  }
	else if (len2 && TREE_CODE (len2) == INTEGER_CST)
	  {
	    if (tree_int_cst_lt (len2, len))
	      len = len2;
	  }

	chainon (arglist, build_tree_list (NULL_TREE, len));
      }

      /* Drops in.  */
    case BUILT_IN_MEMCMP:
      /* If not optimizing, call the library function.  */
      if (!optimize)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
	return const0_rtx;
      else if (!HAVE_cmpstrsi)
	break;
      {
	tree arg1 = TREE_VALUE (arglist);
	tree arg2 = TREE_VALUE (TREE_CHAIN (arglist));
	tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
	rtx result;

	int arg1_align
	  = get_pointer_alignment (arg1, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	int arg2_align
	  = get_pointer_alignment (arg2, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	enum machine_mode insn_mode
	  = insn_operand_mode[(int) CODE_FOR_cmpstrsi][0];

	/* If we don't have POINTER_TYPE, call the function.  */
	if (arg1_align == 0 || arg2_align == 0)
	  {
	    if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STRCMP)
	      TREE_CHAIN (TREE_CHAIN (arglist)) = 0;
	    break;
	  }

	/* Make a place to write the result of the instruction.  */
	result = target;
	if (! (result != 0
	       && GET_CODE (result) == REG && GET_MODE (result) == insn_mode
	       && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	  result = gen_reg_rtx (insn_mode);

	emit_insn (gen_cmpstrsi (result,
				 gen_rtx (MEM, BLKmode,
					  expand_expr (arg1, NULL_RTX, Pmode,
						       EXPAND_NORMAL)),
				 gen_rtx (MEM, BLKmode,
					  expand_expr (arg2, NULL_RTX, Pmode,
						       EXPAND_NORMAL)),
				 expand_expr (len, NULL_RTX, VOIDmode, 0),
				 GEN_INT (MIN (arg1_align, arg2_align))));

	/* Return the value in the proper mode for this function.  */
	mode = TYPE_MODE (TREE_TYPE (exp));
	if (GET_MODE (result) == mode)
	  return result;
	else if (target != 0)
	  {
	    convert_move (target, result, 0);
	    return target;
	  }
	else
	  return convert_to_mode (mode, result, 0);
      }	
#else
    case BUILT_IN_STRCMP:
    case BUILT_IN_MEMCMP:
      break;
#endif

    default:			/* just do library call, if unknown builtin */
      error ("built-in function %s not currently supported",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */

  return expand_call (exp, target, ignore);
}

/* Expand code for a post- or pre- increment or decrement
   and return the RTX for the result.
   POST is 1 for postinc/decrements and 0 for preinc/decrements.  */

static rtx
expand_increment (exp, post)
     register tree exp;
     int post;
{
  register rtx op0, op1;
  register rtx temp, value;
  register tree incremented = TREE_OPERAND (exp, 0);
  optab this_optab = add_optab;
  int icode;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  int op0_is_copy = 0;

  /* Stabilize any component ref that might need to be
     evaluated more than once below.  */
  if (TREE_CODE (incremented) == BIT_FIELD_REF
      || (TREE_CODE (incremented) == COMPONENT_REF
	  && (TREE_CODE (TREE_OPERAND (incremented, 0)) != INDIRECT_REF
	      || DECL_BIT_FIELD (TREE_OPERAND (incremented, 1)))))
    incremented = stabilize_reference (incremented);

  /* Compute the operands as RTX.
     Note whether OP0 is the actual lvalue or a copy of it:
     I believe it is a copy iff it is a register or subreg
     and insns were generated in computing it.   */

  temp = get_last_insn ();
  op0 = expand_expr (incremented, NULL_RTX, VOIDmode, 0);

  /* If OP0 is a SUBREG made for a promoted variable, we cannot increment
     in place but intead must do sign- or zero-extension during assignment,
     so we copy it into a new register and let the code below use it as
     a copy.

     Note that we can safely modify this SUBREG since it is know not to be
     shared (it was made by the expand_expr call above).  */

  if (GET_CODE (op0) == SUBREG && SUBREG_PROMOTED_VAR_P (op0))
    SUBREG_REG (op0) = copy_to_reg (SUBREG_REG (op0));

  op0_is_copy = ((GET_CODE (op0) == SUBREG || GET_CODE (op0) == REG)
		 && temp != get_last_insn ());
  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);

  /* Decide whether incrementing or decrementing.  */
  if (TREE_CODE (exp) == POSTDECREMENT_EXPR
      || TREE_CODE (exp) == PREDECREMENT_EXPR)
    this_optab = sub_optab;

  /* If OP0 is not the actual lvalue, but rather a copy in a register,
     then we cannot just increment OP0.  We must
     therefore contrive to increment the original value.
     Then we can return OP0 since it is a copy of the old value.  */
  if (op0_is_copy)
    {
      /* This is the easiest way to increment the value wherever it is.
	 Problems with multiple evaluation of INCREMENTED
	 are prevented because either (1) it is a component_ref,
	 in which case it was stabilized above, or (2) it is an array_ref
	 with constant index in an array in a register, which is
	 safe to reevaluate.  */
      tree newexp = build ((this_optab == add_optab
			    ? PLUS_EXPR : MINUS_EXPR),
			   TREE_TYPE (exp),
			   incremented,
			   TREE_OPERAND (exp, 1));
      temp = expand_assignment (incremented, newexp, ! post, 0);
      return post ? op0 : temp;
    }

  /* Convert decrement by a constant into a negative increment.  */
  if (this_optab == sub_optab
      && GET_CODE (op1) == CONST_INT)
    {
      op1 = GEN_INT (- INTVAL (op1));
      this_optab = add_optab;
    }

  if (post)
    {
      /* We have a true reference to the value in OP0.
	 If there is an insn to add or subtract in this mode, queue it.  */

#if 0  /* Turned off to avoid making extra insn for indexed memref.  */
      op0 = stabilize (op0);
#endif

      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing
	  /* Make sure that OP0 is valid for operands 0 and 1
	     of the insn we want to queue.  */
	  && (*insn_operand_predicate[icode][0]) (op0, mode)
	  && (*insn_operand_predicate[icode][1]) (op0, mode))
	{
	  if (! (*insn_operand_predicate[icode][2]) (op1, mode))
	    op1 = force_reg (mode, op1);

	  return enqueue_insn (op0, GEN_FCN (icode) (op0, op0, op1));
	}
    }

  /* Preincrement, or we can't increment with one simple insn.  */
  if (post)
    /* Save a copy of the value before inc or dec, to return it later.  */
    temp = value = copy_to_reg (op0);
  else
    /* Arrange to return the incremented value.  */
    /* Copy the rtx because expand_binop will protect from the queue,
       and the results of that would be invalid for us to return
       if our caller does emit_queue before using our result.  */
    temp = copy_rtx (value = op0);

  /* Increment however we can.  */
  op1 = expand_binop (mode, this_optab, value, op1, op0,
		      TREE_UNSIGNED (TREE_TYPE (exp)), OPTAB_LIB_WIDEN);
  /* Make sure the value is stored into OP0.  */
  if (op1 != op0)
    emit_move_insn (op0, op1);

  return temp;
}

/* Expand all function calls contained within EXP, innermost ones first.
   But don't look within expressions that have sequence points.
   For each CALL_EXPR, record the rtx for its value
   in the CALL_EXPR_RTL field.  */

static void
preexpand_calls (exp)
     tree exp;
{
  register int nops, i;
  int type = TREE_CODE_CLASS (TREE_CODE (exp));

  if (! do_preexpand_calls)
    return;

  /* Only expressions and references can contain calls.  */

  if (type != 'e' && type != '<' && type != '1' && type != '2' && type != 'r')
    return;

  switch (TREE_CODE (exp))
    {
    case CALL_EXPR:
      /* Do nothing if already expanded.  */
      if (CALL_EXPR_RTL (exp) != 0)
	return;

      /* Do nothing to built-in functions.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) != ADDR_EXPR
	  || TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) != FUNCTION_DECL
	  || ! DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	CALL_EXPR_RTL (exp) = expand_call (exp, NULL_RTX, 0);
      return;

    case COMPOUND_EXPR:
    case COND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      /* If we find one of these, then we can be sure
	 the adjust will be done for it (since it makes jumps).
	 Do it now, so that if this is inside an argument
	 of a function, we don't get the stack adjustment
	 after some other args have already been pushed.  */
      do_pending_stack_adjust ();
      return;

    case BLOCK:
    case RTL_EXPR:
    case WITH_CLEANUP_EXPR:
      return;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return;
    }

  nops = tree_code_length[(int) TREE_CODE (exp)];
  for (i = 0; i < nops; i++)
    if (TREE_OPERAND (exp, i) != 0)
      {
	type = TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, i)));
	if (type == 'e' || type == '<' || type == '1' || type == '2'
	    || type == 'r')
	  preexpand_calls (TREE_OPERAND (exp, i));
      }
}

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

void
init_pending_stack_adjust ()
{
  pending_stack_adjust = 0;
}

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.  */

void
clear_pending_stack_adjust ()
{
#ifdef EXIT_IGNORE_STACK
  if (! flag_omit_frame_pointer && EXIT_IGNORE_STACK
      && ! (DECL_INLINE (current_function_decl) && ! flag_no_inline)
      && ! flag_inline_functions)
    pending_stack_adjust = 0;
#endif
}

/* Pop any previously-pushed arguments that have not been popped yet.  */

void
do_pending_stack_adjust ()
{
  if (inhibit_defer_pop == 0)
    {
      if (pending_stack_adjust != 0)
	adjust_stack (GEN_INT (pending_stack_adjust));
      pending_stack_adjust = 0;
    }
}

/* Expand all cleanups up to OLD_CLEANUPS.
   Needed here, and also for language-dependent calls.  */

void
expand_cleanups_to (old_cleanups)
     tree old_cleanups;
{
  while (cleanups_this_call != old_cleanups)
    {
      expand_expr (TREE_VALUE (cleanups_this_call), NULL_RTX, VOIDmode, 0);
      cleanups_this_call = TREE_CHAIN (cleanups_this_call);
    }
}

/* Expand conditional expressions.  */

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.
   LABEL is an rtx of code CODE_LABEL, in this function and all the
   functions here.  */

void
jumpifnot (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, label, NULL_RTX);
}

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */

void
jumpif (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, NULL_RTX, label);
}

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.
   Either of IF_FALSE_LABEL and IF_TRUE_LABEL may be zero,
   meaning fall through in that case.

   do_jump always does any pending stack adjust except when it does not
   actually perform a jump.  An example where there is no jump
   is when EXP is `(foo (), 0)' and IF_FALSE_LABEL is null.

   This function is responsible for optimizing cases such as
   &&, || and comparison operators in EXP.  */

void
do_jump (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  register enum tree_code code = TREE_CODE (exp);
  /* Some cases need to create a label to jump to
     in order to properly fall through.
     These cases set DROP_THROUGH_LABEL nonzero.  */
  rtx drop_through_label = 0;
  rtx temp;
  rtx comparison = 0;
  int i;
  tree type;

  emit_queue ();

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      temp = integer_zerop (exp) ? if_false_label : if_true_label;
      if (temp)
	emit_jump (temp);
      break;

#if 0
      /* This is not true with #pragma weak  */
    case ADDR_EXPR:
      /* The address of something can never be zero.  */
      if (if_true_label)
	emit_jump (if_true_label);
      break;
#endif

    case NOP_EXPR:
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == COMPONENT_REF
	  || TREE_CODE (TREE_OPERAND (exp, 0)) == BIT_FIELD_REF
	  || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_REF)
	goto normal;
    case CONVERT_EXPR:
      /* If we are narrowing the operand, we have to do the compare in the
	 narrower mode.  */
      if ((TYPE_PRECISION (TREE_TYPE (exp))
	   < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	goto normal;
    case NON_LVALUE_EXPR:
    case REFERENCE_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These cannot change zero->non-zero or vice versa.  */
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      break;

#if 0
      /* This is never less insns than evaluating the PLUS_EXPR followed by
	 a test and can be longer if the test is eliminated.  */
    case PLUS_EXPR:
      /* Reduce to minus.  */
      exp = build (MINUS_EXPR, TREE_TYPE (exp),
		   TREE_OPERAND (exp, 0),
		   fold (build1 (NEGATE_EXPR, TREE_TYPE (TREE_OPERAND (exp, 1)),
				 TREE_OPERAND (exp, 1))));
      /* Process as MINUS.  */
#endif

    case MINUS_EXPR:
      /* Non-zero iff operands of minus differ.  */
      comparison = compare (build (NE_EXPR, TREE_TYPE (exp),
				   TREE_OPERAND (exp, 0),
				   TREE_OPERAND (exp, 1)),
			    NE, NE);
      break;

    case BIT_AND_EXPR:
      /* If we are AND'ing with a small constant, do this comparison in the
	 smallest type that fits.  If the machine doesn't have comparisons
	 that small, it will be converted back to the wider comparison.
	 This helps if we are testing the sign bit of a narrower object.
	 combine can't do this for us because it can't know whether a
	 ZERO_EXTRACT or a compare in a smaller mode exists, but we do.  */

      if (! SLOW_BYTE_ACCESS
	  && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	  && TYPE_PRECISION (TREE_TYPE (exp)) <= HOST_BITS_PER_WIDE_INT
	  && (i = floor_log2 (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)))) >= 0
	  && (type = type_for_size (i + 1, 1)) != 0
	  && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (exp))
	  && (cmp_optab->handlers[(int) TYPE_MODE (type)].insn_code
	      != CODE_FOR_nothing))
	{
	  do_jump (convert (type, exp), if_false_label, if_true_label);
	  break;
	}
      goto normal;

    case TRUTH_NOT_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
      break;

    case TRUTH_ANDIF_EXPR:
      if (if_false_label == 0)
	if_false_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), if_false_label, NULL_RTX);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
	if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), NULL_RTX, if_true_label);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      free_temp_slots ();
      emit_queue ();
      do_pending_stack_adjust ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
      {
	int bitsize, bitpos, unsignedp;
	enum machine_mode mode;
	tree type;
	tree offset;
	int volatilep = 0;

	/* Get description of this reference.  We don't actually care
	   about the underlying object here.  */
	get_inner_reference (exp, &bitsize, &bitpos, &offset,
			     &mode, &unsignedp, &volatilep);

	type = type_for_size (bitsize, unsignedp);
	if (! SLOW_BYTE_ACCESS
	    && type != 0 && bitsize >= 0
	    && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (exp))
	    && (cmp_optab->handlers[(int) TYPE_MODE (type)].insn_code
		!= CODE_FOR_nothing))
	  {
	    do_jump (convert (type, exp), if_false_label, if_true_label);
	    break;
	  }
	goto normal;
      }

    case COND_EXPR:
      /* Do (a ? 1 : 0) and (a ? 0 : 1) as special cases.  */
      if (integer_onep (TREE_OPERAND (exp, 1))
	  && integer_zerop (TREE_OPERAND (exp, 2)))
	do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);

      else if (integer_zerop (TREE_OPERAND (exp, 1))
	       && integer_onep (TREE_OPERAND (exp, 2)))
	do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);

      else
	{
	  register rtx label1 = gen_label_rtx ();
	  drop_through_label = gen_label_rtx ();
	  do_jump (TREE_OPERAND (exp, 0), label1, NULL_RTX);
	  /* Now the THEN-expression.  */
	  do_jump (TREE_OPERAND (exp, 1),
		   if_false_label ? if_false_label : drop_through_label,
		   if_true_label ? if_true_label : drop_through_label);
	  /* In case the do_jump just above never jumps.  */
	  do_pending_stack_adjust ();
	  emit_label (label1);
	  /* Now the ELSE-expression.  */
	  do_jump (TREE_OPERAND (exp, 2),
		   if_false_label ? if_false_label : drop_through_label,
		   if_true_label ? if_true_label : drop_through_label);
	}
      break;

    case EQ_EXPR:
      if (integer_zerop (TREE_OPERAND (exp, 1)))
	do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
      else if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
		== MODE_INT)
	       && 
	       !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_equality (exp, if_false_label, if_true_label);
      else
	comparison = compare (exp, EQ, EQ);
      break;

    case NE_EXPR:
      if (integer_zerop (TREE_OPERAND (exp, 1)))
	do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      else if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
		== MODE_INT)
	       && 
	       !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_equality (exp, if_true_label, if_false_label);
      else
	comparison = compare (exp, NE, NE);
      break;

    case LT_EXPR:
      if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	   == MODE_INT)
	  && !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_greater (exp, 1, if_false_label, if_true_label);
      else
	comparison = compare (exp, LT, LTU);
      break;

    case LE_EXPR:
      if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	   == MODE_INT)
	  && !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_greater (exp, 0, if_true_label, if_false_label);
      else
	comparison = compare (exp, LE, LEU);
      break;

    case GT_EXPR:
      if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	   == MODE_INT)
	  && !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_greater (exp, 0, if_false_label, if_true_label);
      else
	comparison = compare (exp, GT, GTU);
      break;

    case GE_EXPR:
      if ((GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	   == MODE_INT)
	  && !can_compare_p (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	do_jump_by_parts_greater (exp, 1, if_true_label, if_false_label);
      else
	comparison = compare (exp, GE, GEU);
      break;

    default:
    normal:
      temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);
#if 0
      /* This is not needed any more and causes poor code since it causes
	 comparisons and tests from non-SI objects to have different code
	 sequences.  */
      /* Copy to register to avoid generating bad insns by cse
	 from (set (mem ...) (arithop))  (set (cc0) (mem ...)).  */
      if (!cse_not_expected && GET_CODE (temp) == MEM)
	temp = copy_to_reg (temp);
#endif
      do_pending_stack_adjust ();
      if (GET_CODE (temp) == CONST_INT)
	comparison = (temp == const0_rtx ? const0_rtx : const_true_rtx);
      else if (GET_CODE (temp) == LABEL_REF)
	comparison = const_true_rtx;
      else if (GET_MODE_CLASS (GET_MODE (temp)) == MODE_INT
	       && !can_compare_p (GET_MODE (temp)))
	/* Note swapping the labels gives us not-equal.  */
	do_jump_by_parts_equality_rtx (temp, if_true_label, if_false_label);
      else if (GET_MODE (temp) != VOIDmode)
	comparison = compare_from_rtx (temp, CONST0_RTX (GET_MODE (temp)),
				       NE, TREE_UNSIGNED (TREE_TYPE (exp)),
				       GET_MODE (temp), NULL_RTX, 0);
      else
	abort ();
    }

  /* Do any postincrements in the expression that was tested.  */
  emit_queue ();

  /* If COMPARISON is nonzero here, it is an rtx that can be substituted
     straight into a conditional jump instruction as the jump condition.
     Otherwise, all the work has been done already.  */

  if (comparison == const_true_rtx)
    {
      if (if_true_label)
	emit_jump (if_true_label);
    }
  else if (comparison == const0_rtx)
    {
      if (if_false_label)
	emit_jump (if_false_label);
    }
  else if (comparison)
    do_jump_for_compare (comparison, if_false_label, if_true_label);

  free_temp_slots ();

  if (drop_through_label)
    {
      /* If do_jump produces code that might be jumped around,
	 do any stack adjusts from that code, before the place
	 where control merges in.  */
      do_pending_stack_adjust ();
      emit_label (drop_through_label);
    }
}

/* Given a comparison expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.
   The code of EXP is ignored; we always test GT if SWAP is 0,
   and LT if SWAP is 1.  */

static void
do_jump_by_parts_greater (exp, swap, if_false_label, if_true_label)
     tree exp;
     int swap;
     rtx if_false_label, if_true_label;
{
  rtx op0 = expand_expr (TREE_OPERAND (exp, swap), NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (TREE_OPERAND (exp, !swap), NULL_RTX, VOIDmode, 0);
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  rtx drop_through_label = 0;
  int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)));
  int i;

  if (! if_true_label || ! if_false_label)
    drop_through_label = gen_label_rtx ();
  if (! if_true_label)
    if_true_label = drop_through_label;
  if (! if_false_label)
    if_false_label = drop_through_label;

  /* Compare a word at a time, high order first.  */
  for (i = 0; i < nwords; i++)
    {
      rtx comp;
      rtx op0_word, op1_word;

      if (WORDS_BIG_ENDIAN)
	{
	  op0_word = operand_subword_force (op0, i, mode);
	  op1_word = operand_subword_force (op1, i, mode);
	}
      else
	{
	  op0_word = operand_subword_force (op0, nwords - 1 - i, mode);
	  op1_word = operand_subword_force (op1, nwords - 1 - i, mode);
	}

      /* All but high-order word must be compared as unsigned.  */
      comp = compare_from_rtx (op0_word, op1_word,
			       (unsignedp || i > 0) ? GTU : GT,
			       unsignedp, word_mode, NULL_RTX, 0);
      if (comp == const_true_rtx)
	emit_jump (if_true_label);
      else if (comp != const0_rtx)
	do_jump_for_compare (comp, NULL_RTX, if_true_label);

      /* Consider lower words only if these are equal.  */
      comp = compare_from_rtx (op0_word, op1_word, NE, unsignedp, word_mode,
			       NULL_RTX, 0);
      if (comp == const_true_rtx)
	emit_jump (if_false_label);
      else if (comp != const0_rtx)
	do_jump_for_compare (comp, NULL_RTX, if_false_label);
    }

  if (if_false_label)
    emit_jump (if_false_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Given an EQ_EXPR expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.  */

static void
do_jump_by_parts_equality (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  rtx op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  int i;
  rtx drop_through_label = 0;

  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    {
      rtx comp = compare_from_rtx (operand_subword_force (op0, i, mode),
				   operand_subword_force (op1, i, mode),
				   EQ, TREE_UNSIGNED (TREE_TYPE (exp)),
				   word_mode, NULL_RTX, 0);
      if (comp == const_true_rtx)
	emit_jump (if_false_label);
      else if (comp != const0_rtx)
	do_jump_for_compare (comp, if_false_label, NULL_RTX);
    }

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Jump according to whether OP0 is 0.
   We assume that OP0 has an integer mode that is too wide
   for the available compare insns.  */

static void
do_jump_by_parts_equality_rtx (op0, if_false_label, if_true_label)
     rtx op0;
     rtx if_false_label, if_true_label;
{
  int nwords = GET_MODE_SIZE (GET_MODE (op0)) / UNITS_PER_WORD;
  int i;
  rtx drop_through_label = 0;

  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    {
      rtx comp = compare_from_rtx (operand_subword_force (op0, i,
							  GET_MODE (op0)),
				   const0_rtx, EQ, 1, word_mode, NULL_RTX, 0);
      if (comp == const_true_rtx)
	emit_jump (if_false_label);
      else if (comp != const0_rtx)
	do_jump_for_compare (comp, if_false_label, NULL_RTX);
    }

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Given a comparison expression in rtl form, output conditional branches to
   IF_TRUE_LABEL, IF_FALSE_LABEL, or both.  */

static void
do_jump_for_compare (comparison, if_false_label, if_true_label)
     rtx comparison, if_false_label, if_true_label;
{
  if (if_true_label)
    {
      if (bcc_gen_fctn[(int) GET_CODE (comparison)] != 0)
	emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (comparison)]) (if_true_label));
      else
	abort ();

      if (if_false_label)
	emit_jump (if_false_label);
    }
  else if (if_false_label)
    {
      rtx insn;
      rtx prev = PREV_INSN (get_last_insn ());
      rtx branch = 0;

      /* Output the branch with the opposite condition.  Then try to invert
	 what is generated.  If more than one insn is a branch, or if the
	 branch is not the last insn written, abort. If we can't invert
	 the branch, emit make a true label, redirect this jump to that,
	 emit a jump to the false label and define the true label.  */

      if (bcc_gen_fctn[(int) GET_CODE (comparison)] != 0)
	emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (comparison)]) (if_false_label));
      else
	abort ();

      /* Here we get the insn before what was just emitted.
	 On some machines, emitting the branch can discard
	 the previous compare insn and emit a replacement.  */
      if (prev == 0)
	/* If there's only one preceding insn...  */
	insn = get_insns ();
      else
	insn = NEXT_INSN (prev);

      for (insn = NEXT_INSN (insn); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == JUMP_INSN)
	  {
	    if (branch)
	      abort ();
	    branch = insn;
	  }

      if (branch != get_last_insn ())
	abort ();

      if (! invert_jump (branch, if_false_label))
	{
	  if_true_label = gen_label_rtx ();
	  redirect_jump (branch, if_true_label);
	  emit_jump (if_false_label);
	  emit_label (if_true_label);
	}
    }
}

/* Generate code for a comparison expression EXP
   (including code to compute the values to be compared)
   and set (CC0) according to the result.
   SIGNED_CODE should be the rtx operation for this comparison for
   signed data; UNSIGNED_CODE, likewise for use if data is unsigned.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.  */

static rtx
compare (exp, signed_code, unsigned_code)
     register tree exp;
     enum rtx_code signed_code, unsigned_code;
{
  register rtx op0
    = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
  register rtx op1
    = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
  register tree type = TREE_TYPE (TREE_OPERAND (exp, 0));
  register enum machine_mode mode = TYPE_MODE (type);
  int unsignedp = TREE_UNSIGNED (type);
  enum rtx_code code = unsignedp ? unsigned_code : signed_code;

  return compare_from_rtx (op0, op1, code, unsignedp, mode,
			   ((mode == BLKmode)
			    ? expr_size (TREE_OPERAND (exp, 0)) : NULL_RTX),
			   TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
}

/* Like compare but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.

   If ALIGN is non-zero, it is the alignment of this type; if zero, the
   size of MODE should be used.  */

rtx
compare_from_rtx (op0, op1, code, unsignedp, mode, size, align)
     register rtx op0, op1;
     enum rtx_code code;
     int unsignedp;
     enum machine_mode mode;
     rtx size;
     int align;
{
  /* If one operand is constant, make it the second one.  */

  if (GET_CODE (op0) == CONST_INT || GET_CODE (op0) == CONST_DOUBLE)
    {
      rtx tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT)
    return simplify_relational_operation (code, mode, op0, op1);

#if 0
  /* There's no need to do this now that combine.c can eliminate lots of
     sign extensions.  This can be less efficient in certain cases on other
     machines.

  /* If this is a signed equality comparison, we can do it as an
     unsigned comparison since zero-extension is cheaper than sign
     extension and comparisons with zero are done as unsigned.  This is
     the case even on machines that can do fast sign extension, since
     zero-extension is easier to combinen with other operations than
     sign-extension is.  If we are comparing against a constant, we must
     convert it to what it would look like unsigned.  */
  if ((code == EQ || code == NE) && ! unsignedp
      && GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT)
    {
      if (GET_CODE (op1) == CONST_INT
	  && (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0))) != INTVAL (op1))
	op1 = GEN_INT (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0)));
      unsignedp = 1;
    }
#endif
	
  emit_cmp_insn (op0, op1, code, size, mode, unsignedp, align);

  return gen_rtx (code, VOIDmode, cc0_rtx, const0_rtx);
}

/* Generate code to calculate EXP using a store-flag instruction
   and return an rtx for the result.  EXP is either a comparison
   or a TRUTH_NOT_EXPR whose operand is a comparison.

   If TARGET is nonzero, store the result there if convenient.

   If ONLY_CHEAP is non-zero, only do this if it is likely to be very
   cheap.

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
do_store_flag (exp, target, mode, only_cheap)
     tree exp;
     rtx target;
     enum machine_mode mode;
     int only_cheap;
{
  enum rtx_code code;
  tree arg0, arg1, type;
  tree tem;
  enum machine_mode operand_mode;
  int invert = 0;
  int unsignedp;
  rtx op0, op1;
  enum insn_code icode;
  rtx subtarget = target;
  rtx result, label, pattern, jump_pat;

  /* If this is a TRUTH_NOT_EXPR, set a flag indicating we must invert the
     result at the end.  We can't simply invert the test since it would
     have already been inverted if it were valid.  This case occurs for
     some floating-point comparisons.  */

  if (TREE_CODE (exp) == TRUTH_NOT_EXPR)
    invert = 1, exp = TREE_OPERAND (exp, 0);

  arg0 = TREE_OPERAND (exp, 0);
  arg1 = TREE_OPERAND (exp, 1);
  type = TREE_TYPE (arg0);
  operand_mode = TYPE_MODE (type);
  unsignedp = TREE_UNSIGNED (type);

  /* We won't bother with BLKmode store-flag operations because it would mean
     passing a lot of information to emit_store_flag.  */
  if (operand_mode == BLKmode)
    return 0;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  /* Get the rtx comparison code to use.  We know that EXP is a comparison
     operation of some type.  Some comparisons against 1 and -1 can be
     converted to comparisons with zero.  Do so here so that the tests
     below will be aware that we have a comparison with zero.   These
     tests will not catch constants in the first operand, but constants
     are rarely passed as the first operand.  */

  switch (TREE_CODE (exp))
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
      if (integer_all_onesp (arg1))
	arg1 = integer_zero_node, code = unsignedp ? LTU : LT;
      else
	code = unsignedp ? LEU : LE;
      break;
    case GT_EXPR:
      if (integer_all_onesp (arg1))
	arg1 = integer_zero_node, code = unsignedp ? GEU : GE;
      else
	code = unsignedp ? GTU : GT;
      break;
    case GE_EXPR:
      if (integer_onep (arg1))
	arg1 = integer_zero_node, code = unsignedp ? GTU : GT;
      else
	code = unsignedp ? GEU : GE;
      break;
    default:
      abort ();
    }

  /* Put a constant second.  */
  if (TREE_CODE (arg0) == REAL_CST || TREE_CODE (arg0) == INTEGER_CST)
    {
      tem = arg0; arg0 = arg1; arg1 = tem;
      code = swap_condition (code);
    }

  /* If this is an equality or inequality test of a single bit, we can
     do this by shifting the bit being tested to the low-order bit and
     masking the result with the constant 1.  If the condition was EQ,
     we xor it with 1.  This does not require an scc insn and is faster
     than an scc insn even if we have it.  */

  if ((code == NE || code == EQ)
      && TREE_CODE (arg0) == BIT_AND_EXPR && integer_zerop (arg1)
      && integer_pow2p (TREE_OPERAND (arg0, 1))
      && TYPE_PRECISION (type) <= HOST_BITS_PER_WIDE_INT)
    {
      int bitnum = exact_log2 (INTVAL (expand_expr (TREE_OPERAND (arg0, 1),
						    NULL_RTX, VOIDmode, 0)));

      if (subtarget == 0 || GET_CODE (subtarget) != REG
	  || GET_MODE (subtarget) != operand_mode
	  || ! safe_from_p (subtarget, TREE_OPERAND (arg0, 0)))
	subtarget = 0;

      op0 = expand_expr (TREE_OPERAND (arg0, 0), subtarget, VOIDmode, 0);

      if (bitnum != 0)
	op0 = expand_shift (RSHIFT_EXPR, GET_MODE (op0), op0,
			    size_int (bitnum), target, 1);

      if (GET_MODE (op0) != mode)
	op0 = convert_to_mode (mode, op0, 1);

      if (bitnum != TYPE_PRECISION (type) - 1)
	op0 = expand_and (op0, const1_rtx, target);

      if ((code == EQ && ! invert) || (code == NE && invert))
	op0 = expand_binop (mode, xor_optab, op0, const1_rtx, target, 0,
			    OPTAB_LIB_WIDEN);

      return op0;
    }

  /* Now see if we are likely to be able to do this.  Return if not.  */
  if (! can_compare_p (operand_mode))
    return 0;
  icode = setcc_gen_code[(int) code];
  if (icode == CODE_FOR_nothing
      || (only_cheap && insn_operand_mode[(int) icode][0] != mode))
    {
      /* We can only do this if it is one of the special cases that
	 can be handled without an scc insn.  */
      if ((code == LT && integer_zerop (arg1))
	  || (! only_cheap && code == GE && integer_zerop (arg1)))
	;
      else if (BRANCH_COST >= 0
	       && ! only_cheap && (code == NE || code == EQ)
	       && TREE_CODE (type) != REAL_TYPE
	       && ((abs_optab->handlers[(int) operand_mode].insn_code
		    != CODE_FOR_nothing)
		   || (ffs_optab->handlers[(int) operand_mode].insn_code
		       != CODE_FOR_nothing)))
	;
      else
	return 0;
    }
      
  preexpand_calls (exp);
  if (subtarget == 0 || GET_CODE (subtarget) != REG
      || GET_MODE (subtarget) != operand_mode
      || ! safe_from_p (subtarget, arg1))
    subtarget = 0;

  op0 = expand_expr (arg0, subtarget, VOIDmode, 0);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);

  if (target == 0)
    target = gen_reg_rtx (mode);

  /* Pass copies of OP0 and OP1 in case they contain a QUEUED.  This is safe
     because, if the emit_store_flag does anything it will succeed and
     OP0 and OP1 will not be used subsequently.  */

  result = emit_store_flag (target, code,
			    queued_subexp_p (op0) ? copy_rtx (op0) : op0,
			    queued_subexp_p (op1) ? copy_rtx (op1) : op1,
			    operand_mode, unsignedp, 1);

  if (result)
    {
      if (invert)
	result = expand_binop (mode, xor_optab, result, const1_rtx,
			       result, 0, OPTAB_LIB_WIDEN);
      return result;
    }

  /* If this failed, we have to do this with set/compare/jump/set code.  */
  if (target == 0 || GET_CODE (target) != REG
      || reg_mentioned_p (target, op0) || reg_mentioned_p (target, op1))
    target = gen_reg_rtx (GET_MODE (target));

  emit_move_insn (target, invert ? const0_rtx : const1_rtx);
  result = compare_from_rtx (op0, op1, code, unsignedp,
			     operand_mode, NULL_RTX, 0);
  if (GET_CODE (result) == CONST_INT)
    return (((result == const0_rtx && ! invert)
	     || (result != const0_rtx && invert))
	    ? const0_rtx : const1_rtx);

  label = gen_label_rtx ();
  if (bcc_gen_fctn[(int) code] == 0)
    abort ();

  emit_jump_insn ((*bcc_gen_fctn[(int) code]) (label));
  emit_move_insn (target, invert ? const1_rtx : const0_rtx);
  emit_label (label);

  return target;
}

/* Generate a tablejump instruction (used for switch statements).  */

#ifdef HAVE_tablejump

/* INDEX is the value being switched on, with the lowest value
   in the table already subtracted.
   MODE is its expected mode (needed if INDEX is constant).
   RANGE is the length of the jump table.
   TABLE_LABEL is a CODE_LABEL rtx for the table itself.

   DEFAULT_LABEL is a CODE_LABEL rtx to jump to if the
   index value is out of range.  */

void
do_tablejump (index, mode, range, table_label, default_label)
     rtx index, range, table_label, default_label;
     enum machine_mode mode;
{
  register rtx temp, vector;

  /* Do an unsigned comparison (in the proper mode) between the index
     expression and the value which represents the length of the range.
     Since we just finished subtracting the lower bound of the range
     from the index expression, this comparison allows us to simultaneously
     check that the original index expression value is both greater than
     or equal to the minimum value of the range and less than or equal to
     the maximum value of the range.  */

  emit_cmp_insn (range, index, LTU, NULL_RTX, mode, 0, 0);
  emit_jump_insn (gen_bltu (default_label));

  /* If index is in range, it must fit in Pmode.
     Convert to Pmode so we can index with it.  */
  if (mode != Pmode)
    index = convert_to_mode (Pmode, index, 1);

  /* If flag_force_addr were to affect this address
     it could interfere with the tricky assumptions made
     about addresses that contain label-refs,
     which may be valid only very near the tablejump itself.  */
  /* ??? The only correct use of CASE_VECTOR_MODE is the one inside the
     GET_MODE_SIZE, because this indicates how large insns are.  The other
     uses should all be Pmode, because they are addresses.  This code
     could fail if addresses and insns are not the same size.  */
  index = memory_address_noforce
    (CASE_VECTOR_MODE,
     gen_rtx (PLUS, Pmode,
	      gen_rtx (MULT, Pmode, index,
		       GEN_INT (GET_MODE_SIZE (CASE_VECTOR_MODE))),
	      gen_rtx (LABEL_REF, Pmode, table_label)));
  temp = gen_reg_rtx (CASE_VECTOR_MODE);
  vector = gen_rtx (MEM, CASE_VECTOR_MODE, index);
  RTX_UNCHANGING_P (vector) = 1;
  convert_move (temp, vector, 0);

  emit_jump_insn (gen_tablejump (temp, table_label));

#ifndef CASE_VECTOR_PC_RELATIVE
  /* If we are generating PIC code or if the table is PC-relative, the
     table and JUMP_INSN must be adjacent, so don't output a BARRIER.  */
  if (! flag_pic)
    emit_barrier ();
#endif
}

#endif /* HAVE_tablejump */
