/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002 Free Software Foundation, Inc.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
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
#include "ggc.h"
#include "langhooks.h"
#include "intl.h"
#include "tm_p.h"

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.

   They should if the stack and args grow in opposite directions, but
   only if we have push insns.  */

#ifdef PUSH_ROUNDING

#if defined (STACK_GROWS_DOWNWARD) != defined (ARGS_GROW_DOWNWARD)
#define PUSH_ARGS_REVERSED	/* If it's last to first.  */
#endif

#endif

#ifndef STACK_PUSH_CODE
#ifdef STACK_GROWS_DOWNWARD
#define STACK_PUSH_CODE PRE_DEC
#else
#define STACK_PUSH_CODE PRE_INC
#endif
#endif

/* Assume that case vectors are not pc-relative.  */
#ifndef CASE_VECTOR_PC_RELATIVE
#define CASE_VECTOR_PC_RELATIVE 0
#endif

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
int cse_not_expected;

/* Chain of pending expressions for PLACEHOLDER_EXPR to replace.  */
static tree placeholder_list = 0;

/* This structure is used by move_by_pieces to describe the move to
   be performed.  */
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
  unsigned HOST_WIDE_INT len;
  HOST_WIDE_INT offset;
  int reverse;
};

/* This structure is used by store_by_pieces to describe the clear to
   be performed.  */

struct store_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  unsigned HOST_WIDE_INT len;
  HOST_WIDE_INT offset;
  rtx (*constfun) PARAMS ((PTR, HOST_WIDE_INT, enum machine_mode));
  PTR constfundata;
  int reverse;
};

extern struct obstack permanent_obstack;

static rtx enqueue_insn		PARAMS ((rtx, rtx));
static unsigned HOST_WIDE_INT move_by_pieces_ninsns
				PARAMS ((unsigned HOST_WIDE_INT,
					 unsigned int));
static void move_by_pieces_1	PARAMS ((rtx (*) (rtx, ...), enum machine_mode,
					 struct move_by_pieces *));
static rtx clear_by_pieces_1	PARAMS ((PTR, HOST_WIDE_INT,
					 enum machine_mode));
static void clear_by_pieces	PARAMS ((rtx, unsigned HOST_WIDE_INT,
					 unsigned int));
static void store_by_pieces_1	PARAMS ((struct store_by_pieces *,
					 unsigned int));
static void store_by_pieces_2	PARAMS ((rtx (*) (rtx, ...),
					 enum machine_mode,
					 struct store_by_pieces *));
static rtx get_subtarget	PARAMS ((rtx));
static int is_zeros_p		PARAMS ((tree));
static int mostly_zeros_p	PARAMS ((tree));
static void store_constructor_field PARAMS ((rtx, unsigned HOST_WIDE_INT,
					     HOST_WIDE_INT, enum machine_mode,
					     tree, tree, int, int));
static void store_constructor	PARAMS ((tree, rtx, int, HOST_WIDE_INT));
static rtx store_field		PARAMS ((rtx, HOST_WIDE_INT,
					 HOST_WIDE_INT, enum machine_mode,
					 tree, enum machine_mode, int, tree,
					 int));
static rtx var_rtx		PARAMS ((tree));
static HOST_WIDE_INT highest_pow2_factor PARAMS ((tree));
static HOST_WIDE_INT highest_pow2_factor_for_type PARAMS ((tree, tree));
static int is_aligning_offset	PARAMS ((tree, tree));
static rtx expand_increment	PARAMS ((tree, int, int));
static void do_jump_by_parts_greater PARAMS ((tree, int, rtx, rtx));
static void do_jump_by_parts_equality PARAMS ((tree, rtx, rtx));
static void do_compare_and_jump	PARAMS ((tree, enum rtx_code, enum rtx_code,
					 rtx, rtx));
static rtx do_store_flag	PARAMS ((tree, rtx, enum machine_mode, int));
#ifdef PUSH_ROUNDING
static void emit_single_push_insn PARAMS ((enum machine_mode, rtx, tree));
#endif
static void do_tablejump PARAMS ((rtx, enum machine_mode, rtx, rtx, rtx));

/* Record for each mode whether we can move a register directly to or
   from an object of that mode in memory.  If we can't, we won't try
   to use that mode directly when accessing a field of that mode.  */

static char direct_load[NUM_MACHINE_MODES];
static char direct_store[NUM_MACHINE_MODES];

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction sequences, we will do a movstr or libcall instead.  */

#ifndef MOVE_RATIO
#if defined (HAVE_movstrqi) || defined (HAVE_movstrhi) || defined (HAVE_movstrsi) || defined (HAVE_movstrdi) || defined (HAVE_movstrti)
#define MOVE_RATIO 2
#else
/* If we are optimizing for space (-Os), cut down the default move ratio.  */
#define MOVE_RATIO (optimize_size ? 3 : 15)
#endif
#endif

/* This macro is used to determine whether move_by_pieces should be called
   to perform a structure copy.  */
#ifndef MOVE_BY_PIECES_P
#define MOVE_BY_PIECES_P(SIZE, ALIGN) \
  (move_by_pieces_ninsns (SIZE, ALIGN) < (unsigned int) MOVE_RATIO)
#endif

/* This array records the insn_code of insns to perform block moves.  */
enum insn_code movstr_optab[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block clears.  */
enum insn_code clrstr_optab[NUM_MACHINE_MODES];

/* SLOW_UNALIGNED_ACCESS is non-zero if unaligned accesses are very slow.  */

#ifndef SLOW_UNALIGNED_ACCESS
#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) STRICT_ALIGNMENT
#endif

/* This is run once per compilation to set up which modes can be used
   directly in memory and to initialize the block move optab.  */

void
init_expr_once ()
{
  rtx insn, pat;
  enum machine_mode mode;
  int num_clobbers;
  rtx mem, mem1;

  start_sequence ();

  /* Try indexing by frame ptr and try by stack ptr.
     It is known that on the Convex the stack ptr isn't a valid index.
     With luck, one or the other is valid on any machine.  */
  mem = gen_rtx_MEM (VOIDmode, stack_pointer_rtx);
  mem1 = gen_rtx_MEM (VOIDmode, frame_pointer_rtx);

  insn = emit_insn (gen_rtx_SET (0, NULL_RTX, NULL_RTX));
  pat = PATTERN (insn);

  for (mode = VOIDmode; (int) mode < NUM_MACHINE_MODES;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      int regno;
      rtx reg;

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

	    reg = gen_rtx_REG (mode, regno);

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

  end_sequence ();
}

/* This is run at the start of compiling a function.  */

void
init_expr ()
{
  cfun->expr = (struct expr_status *) xmalloc (sizeof (struct expr_status));

  pending_chain = 0;
  pending_stack_adjust = 0;
  stack_pointer_delta = 0;
  inhibit_defer_pop = 0;
  saveregs_value = 0;
  apply_args_value = 0;
  forced_labels = 0;
}

void
mark_expr_status (p)
     struct expr_status *p;
{
  if (p == NULL)
    return;

  ggc_mark_rtx (p->x_saveregs_value);
  ggc_mark_rtx (p->x_apply_args_value);
  ggc_mark_rtx (p->x_forced_labels);
}

void
free_expr_status (f)
     struct function *f;
{
  free (f->expr);
  f->expr = NULL;
}

/* Small sanity check that the queue is empty at the end of a function.  */

void
finish_expr_for_function ()
{
  if (pending_chain)
    abort ();
}

/* Manage the queue of increment instructions to be output
   for POSTINCREMENT_EXPR expressions, etc.  */

/* Queue up to increment (or change) VAR later.  BODY says how:
   BODY should be the same thing you would pass to emit_insn
   to increment right away.  It will go to emit_insn later on.

   The value is a QUEUED expression to be used in place of VAR
   where you want to guarantee the pre-incrementation value of VAR.  */

static rtx
enqueue_insn (var, body)
     rtx var, body;
{
  pending_chain = gen_rtx_QUEUED (GET_MODE (var), var, NULL_RTX, NULL_RTX,
				  body, pending_chain);
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
     rtx x;
     int modify;
{
  RTX_CODE code = GET_CODE (x);

#if 0  /* A QUEUED can hang around after the queue is forced out.  */
  /* Shortcut for most common case.  */
  if (pending_chain == 0)
    return x;
#endif

  if (code != QUEUED)
    {
      /* A special hack for read access to (MEM (QUEUED ...)) to facilitate
	 use of autoincrement.  Make a copy of the contents of the memory
	 location rather than a copy of the address, but not if the value is
	 of mode BLKmode.  Don't modify X in place since it might be
	 shared.  */
      if (code == MEM && GET_MODE (x) != BLKmode
	  && GET_CODE (XEXP (x, 0)) == QUEUED && !modify)
	{
	  rtx y = XEXP (x, 0);
	  rtx new = replace_equiv_address_nv (x, QUEUED_VAR (y));

	  if (QUEUED_INSN (y))
	    {
	      rtx temp = gen_reg_rtx (GET_MODE (x));

	      emit_insn_before (gen_move_insn (temp, new),
				QUEUED_INSN (y));
	      return temp;
	    }

	  /* Copy the address into a pseudo, so that the returned value
	     remains correct across calls to emit_queue.  */
	  return replace_equiv_address (new, copy_to_reg (XEXP (new, 0)));
	}

      /* Otherwise, recursively protect the subexpressions of all
	 the kinds of rtx's that can contain a QUEUED.  */
      if (code == MEM)
	{
	  rtx tem = protect_from_queue (XEXP (x, 0), 0);
	  if (tem != XEXP (x, 0))
	    {
	      x = copy_rtx (x);
	      XEXP (x, 0) = tem;
	    }
	}
      else if (code == PLUS || code == MULT)
	{
	  rtx new0 = protect_from_queue (XEXP (x, 0), 0);
	  rtx new1 = protect_from_queue (XEXP (x, 1), 0);
	  if (new0 != XEXP (x, 0) || new1 != XEXP (x, 1))
	    {
	      x = copy_rtx (x);
	      XEXP (x, 0) = new0;
	      XEXP (x, 1) = new1;
	    }
	}
      return x;
    }
  /* If the increment has not happened, use the variable itself.  Copy it
     into a new pseudo so that the value remains correct across calls to
     emit_queue.  */
  if (QUEUED_INSN (x) == 0)
    return copy_to_reg (QUEUED_VAR (x));
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

int
queued_subexp_p (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case QUEUED:
      return 1;
    case MEM:
      return queued_subexp_p (XEXP (x, 0));
    case MULT:
    case PLUS:
    case MINUS:
      return (queued_subexp_p (XEXP (x, 0))
	      || queued_subexp_p (XEXP (x, 1)));
    default:
      return 0;
    }
}

/* Perform all the pending incrementations.  */

void
emit_queue ()
{
  rtx p;
  while ((p = pending_chain))
    {
      rtx body = QUEUED_BODY (p);

      if (GET_CODE (body) == SEQUENCE)
	{
	  QUEUED_INSN (p) = XVECEXP (QUEUED_BODY (p), 0, 0);
	  emit_insn (QUEUED_BODY (p));
	}
      else
	QUEUED_INSN (p) = emit_insn (QUEUED_BODY (p));
      pending_chain = QUEUED_NEXT (p);
    }
}

/* Copy data from FROM to TO, where the machine modes are not the same.
   Both modes may be integer, or both may be floating.
   UNSIGNEDP should be nonzero if FROM is an unsigned type.
   This causes zero-extension instead of sign-extension.  */

void
convert_move (to, from, unsignedp)
     rtx to, from;
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

  if (VECTOR_MODE_P (to_mode) || VECTOR_MODE_P (from_mode))
    {
      if (GET_MODE_BITSIZE (from_mode) != GET_MODE_BITSIZE (to_mode))
	abort ();

      if (VECTOR_MODE_P (to_mode))
	from = simplify_gen_subreg (to_mode, from, GET_MODE (from), 0);
      else
	to = simplify_gen_subreg (from_mode, to, GET_MODE (to), 0);

      emit_move_insn (to, from);
      return;
    }

  if (to_real != from_real)
    abort ();

  if (to_real)
    {
      rtx value, insns;

      if (GET_MODE_BITSIZE (from_mode) < GET_MODE_BITSIZE (to_mode))
	{
	  /* Try converting directly if the insn is supported.  */
	  if ((code = can_extend_p (to_mode, from_mode, 0))
	      != CODE_FOR_nothing)
	    {
	      emit_unop_insn (code, to, from, UNKNOWN);
	      return;
	    }
	}

#ifdef HAVE_trunchfqf2
      if (HAVE_trunchfqf2 && from_mode == HFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_trunchfqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctqfqf2
      if (HAVE_trunctqfqf2 && from_mode == TQFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctqfqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncsfqf2
      if (HAVE_truncsfqf2 && from_mode == SFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_truncsfqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncdfqf2
      if (HAVE_truncdfqf2 && from_mode == DFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_truncdfqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncxfqf2
      if (HAVE_truncxfqf2 && from_mode == XFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_truncxfqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctfqf2
      if (HAVE_trunctfqf2 && from_mode == TFmode && to_mode == QFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctfqf2, to, from, UNKNOWN);
	  return;
	}
#endif

#ifdef HAVE_trunctqfhf2
      if (HAVE_trunctqfhf2 && from_mode == TQFmode && to_mode == HFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctqfhf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncsfhf2
      if (HAVE_truncsfhf2 && from_mode == SFmode && to_mode == HFmode)
	{
	  emit_unop_insn (CODE_FOR_truncsfhf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncdfhf2
      if (HAVE_truncdfhf2 && from_mode == DFmode && to_mode == HFmode)
	{
	  emit_unop_insn (CODE_FOR_truncdfhf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncxfhf2
      if (HAVE_truncxfhf2 && from_mode == XFmode && to_mode == HFmode)
	{
	  emit_unop_insn (CODE_FOR_truncxfhf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctfhf2
      if (HAVE_trunctfhf2 && from_mode == TFmode && to_mode == HFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctfhf2, to, from, UNKNOWN);
	  return;
	}
#endif

#ifdef HAVE_truncsftqf2
      if (HAVE_truncsftqf2 && from_mode == SFmode && to_mode == TQFmode)
	{
	  emit_unop_insn (CODE_FOR_truncsftqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncdftqf2
      if (HAVE_truncdftqf2 && from_mode == DFmode && to_mode == TQFmode)
	{
	  emit_unop_insn (CODE_FOR_truncdftqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncxftqf2
      if (HAVE_truncxftqf2 && from_mode == XFmode && to_mode == TQFmode)
	{
	  emit_unop_insn (CODE_FOR_truncxftqf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_trunctftqf2
      if (HAVE_trunctftqf2 && from_mode == TFmode && to_mode == TQFmode)
	{
	  emit_unop_insn (CODE_FOR_trunctftqf2, to, from, UNKNOWN);
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

	    default:
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

	    default:
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

	    default:
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

	    default:
	      break;
	    }
	  break;

	default:
	  break;
	}

      if (libcall == (rtx) 0)
	/* This conversion is not implemented yet.  */
	abort ();

      start_sequence ();
      value = emit_library_call_value (libcall, NULL_RTX, LCT_CONST, to_mode,
				       1, from, from_mode);
      insns = get_insns ();
      end_sequence ();
      emit_libcall_block (insns, to, value, gen_rtx_FLOAT_TRUNCATE (to_mode,
								    from));
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
	  if (GET_CODE (to) == REG)
	    emit_insn (gen_rtx_CLOBBER (VOIDmode, to));
	  convert_move (gen_lowpart (word_mode, to), from, unsignedp);
	  emit_unop_insn (code, to,
			  gen_lowpart (word_mode, to), equiv_code);
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
	{
#ifdef HAVE_slt
	  if (HAVE_slt
	      && insn_data[(int) CODE_FOR_slt].operand[0].mode == word_mode
	      && STORE_FLAG_VALUE == -1)
	    {
	      emit_cmp_insn (lowfrom, const0_rtx, NE, NULL_RTX,
			     lowpart_mode, 0);
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
			      gen_rtx_fmt_e (equiv_code, to_mode, copy_rtx (from)));
      return;
    }

  /* Truncating multi-word to a word or less.  */
  if (GET_MODE_BITSIZE (from_mode) > BITS_PER_WORD
      && GET_MODE_BITSIZE (to_mode) <= BITS_PER_WORD)
    {
      if (!((GET_CODE (from) == MEM
	     && ! MEM_VOLATILE_P (from)
	     && direct_load[(int) to_mode]
	     && ! mode_dependent_address_p (XEXP (from, 0)))
	    || GET_CODE (from) == REG
	    || GET_CODE (from) == SUBREG))
	from = force_reg (from_mode, from);
      convert_move (to, gen_lowpart (word_mode, from), 0);
      return;
    }

  /* Handle pointer conversion.  */			/* SPEE 900220.  */
  if (to_mode == PQImode)
    {
      if (from_mode != QImode)
	from = convert_to_mode (QImode, from, unsignedp);

#ifdef HAVE_truncqipqi2
      if (HAVE_truncqipqi2)
	{
	  emit_unop_insn (CODE_FOR_truncqipqi2, to, from, UNKNOWN);
	  return;
	}
#endif /* HAVE_truncqipqi2 */
      abort ();
    }

  if (from_mode == PQImode)
    {
      if (to_mode != QImode)
	{
	  from = convert_to_mode (QImode, from, unsignedp);
	  from_mode = QImode;
	}
      else
	{
#ifdef HAVE_extendpqiqi2
	  if (HAVE_extendpqiqi2)
	    {
	      emit_unop_insn (CODE_FOR_extendpqiqi2, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_extendpqiqi2 */
	  abort ();
	}
    }

  if (to_mode == PSImode)
    {
      if (from_mode != SImode)
	from = convert_to_mode (SImode, from, unsignedp);

#ifdef HAVE_truncsipsi2
      if (HAVE_truncsipsi2)
	{
	  emit_unop_insn (CODE_FOR_truncsipsi2, to, from, UNKNOWN);
	  return;
	}
#endif /* HAVE_truncsipsi2 */
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
#ifdef HAVE_extendpsisi2
	  if (! unsignedp && HAVE_extendpsisi2)
	    {
	      emit_unop_insn (CODE_FOR_extendpsisi2, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_extendpsisi2 */
#ifdef HAVE_zero_extendpsisi2
	  if (unsignedp && HAVE_zero_extendpsisi2)
	    {
	      emit_unop_insn (CODE_FOR_zero_extendpsisi2, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_zero_extendpsisi2 */
	  abort ();
	}
    }

  if (to_mode == PDImode)
    {
      if (from_mode != DImode)
	from = convert_to_mode (DImode, from, unsignedp);

#ifdef HAVE_truncdipdi2
      if (HAVE_truncdipdi2)
	{
	  emit_unop_insn (CODE_FOR_truncdipdi2, to, from, UNKNOWN);
	  return;
	}
#endif /* HAVE_truncdipdi2 */
      abort ();
    }

  if (from_mode == PDImode)
    {
      if (to_mode != DImode)
	{
	  from = convert_to_mode (DImode, from, unsignedp);
	  from_mode = DImode;
	}
      else
	{
#ifdef HAVE_extendpdidi2
	  if (HAVE_extendpdidi2)
	    {
	      emit_unop_insn (CODE_FOR_extendpdidi2, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_extendpdidi2 */
	  abort ();
	}
    }

  /* Now follow all the conversions between integers
     no more than a word long.  */

  /* For truncation, usually we can just refer to FROM in a narrower mode.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode)
      && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (to_mode),
				GET_MODE_BITSIZE (from_mode)))
    {
      if (!((GET_CODE (from) == MEM
	     && ! MEM_VOLATILE_P (from)
	     && direct_load[(int) to_mode]
	     && ! mode_dependent_address_p (XEXP (from, 0)))
	    || GET_CODE (from) == REG
	    || GET_CODE (from) == SUBREG))
	from = force_reg (from_mode, from);
      if (GET_CODE (from) == REG && REGNO (from) < FIRST_PSEUDO_REGISTER
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
	  if (flag_force_mem)
	    from = force_not_mem (from);

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
	  shift_amount = build_int_2 (GET_MODE_BITSIZE (to_mode)
				      - GET_MODE_BITSIZE (from_mode), 0);
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

  if (from_mode == TImode && to_mode == DImode)
    {
#ifdef HAVE_trunctidi2
      if (HAVE_trunctidi2)
	{
	  emit_unop_insn (CODE_FOR_trunctidi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == SImode)
    {
#ifdef HAVE_trunctisi2
      if (HAVE_trunctisi2)
	{
	  emit_unop_insn (CODE_FOR_trunctisi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == HImode)
    {
#ifdef HAVE_trunctihi2
      if (HAVE_trunctihi2)
	{
	  emit_unop_insn (CODE_FOR_trunctihi2, to, from, UNKNOWN);
	  return;
	}
#endif
      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == QImode)
    {
#ifdef HAVE_trunctiqi2
      if (HAVE_trunctiqi2)
	{
	  emit_unop_insn (CODE_FOR_trunctiqi2, to, from, UNKNOWN);
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
  return convert_modes (mode, VOIDmode, x, unsignedp);
}

/* Return an rtx for a value that would result
   from converting X from mode OLDMODE to mode MODE.
   Both modes may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.

   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.

   You can give VOIDmode for OLDMODE, if you are sure X has a nonvoid mode.

   This function *must not* call protect_from_queue
   except when putting X into an insn (in which case convert_move does it).  */

rtx
convert_modes (mode, oldmode, x, unsignedp)
     enum machine_mode mode, oldmode;
     rtx x;
     int unsignedp;
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
      && GET_CODE (x) == CONST_INT && INTVAL (x) < 0)
    {
      HOST_WIDE_INT val = INTVAL (x);

      if (oldmode != VOIDmode
	  && HOST_BITS_PER_WIDE_INT > GET_MODE_BITSIZE (oldmode))
	{
	  int width = GET_MODE_BITSIZE (oldmode);

	  /* We need to zero extend VAL.  */
	  val &= ((HOST_WIDE_INT) 1 << width) - 1;
	}

      return immed_double_const (val, (HOST_WIDE_INT) 0, mode);
    }

  /* We can do this with a gen_lowpart if both desired and current modes
     are integer, and this is either a constant integer, a register, or a
     non-volatile MEM.  Except for the constant case where MODE is no
     wider than HOST_BITS_PER_WIDE_INT, we must be narrowing the operand.  */

  if ((GET_CODE (x) == CONST_INT
       && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
      || (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_CLASS (oldmode) == MODE_INT
	  && (GET_CODE (x) == CONST_DOUBLE
	      || (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (oldmode)
		  && ((GET_CODE (x) == MEM && ! MEM_VOLATILE_P (x)
		       && direct_load[(int) mode])
		      || (GET_CODE (x) == REG
			  && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (mode),
						    GET_MODE_BITSIZE (GET_MODE (x)))))))))
    {
      /* ?? If we don't know OLDMODE, we have to assume here that
	 X does not need sign- or zero-extension.   This may not be
	 the case, but it's the best we can do.  */
      if (GET_CODE (x) == CONST_INT && oldmode != VOIDmode
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

	  return GEN_INT (trunc_int_for_mode (val, mode));
	}

      return gen_lowpart (mode, x);
    }

  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

/* This macro is used to determine what the largest unit size that
   move_by_pieces can use is.  */

/* MOVE_MAX_PIECES is the number of bytes at a time which we can
   move efficiently, as opposed to  MOVE_MAX which is the maximum
   number of bytes we can move with a single instruction.  */

#ifndef MOVE_MAX_PIECES
#define MOVE_MAX_PIECES   MOVE_MAX
#endif

/* Generate several move instructions to copy LEN bytes from block FROM to
   block TO.  (These are MEM rtx's with BLKmode).  The caller must pass FROM
   and TO through protect_from_queue before calling.

   If PUSH_ROUNDING is defined and TO is NULL, emit_single_push_insn is
   used to push FROM to the stack.

   ALIGN is maximum alignment we can assume.  */

void
move_by_pieces (to, from, len, align)
     rtx to, from;
     unsigned HOST_WIDE_INT len;
     unsigned int align;
{
  struct move_by_pieces data;
  rtx to_addr, from_addr = XEXP (from, 0);
  unsigned int max_size = MOVE_MAX_PIECES + 1;
  enum machine_mode mode = VOIDmode, tmode;
  enum insn_code icode;

  data.offset = 0;
  data.from_addr = from_addr;
  if (to)
    {
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
      && move_by_pieces_ninsns (len, align) > 2)
    {
      /* Find the mode of the largest move...  */
      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (USE_LOAD_PRE_DECREMENT (mode) && data.reverse && ! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (plus_constant (from_addr, len));
	  data.autinc_from = 1;
	  data.explicit_inc_from = -1;
	}
      if (USE_LOAD_POST_INCREMENT (mode) && ! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (from_addr);
	  data.autinc_from = 1;
	  data.explicit_inc_from = 1;
	}
      if (!data.autinc_from && CONSTANT_P (from_addr))
	data.from_addr = copy_addr_to_reg (from_addr);
      if (USE_STORE_PRE_DECREMENT (mode) && data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (plus_constant (to_addr, len));
	  data.autinc_to = 1;
	  data.explicit_inc_to = -1;
	}
      if (USE_STORE_POST_INCREMENT (mode) && ! data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (to_addr);
	  data.autinc_to = 1;
	  data.explicit_inc_to = 1;
	}
      if (!data.autinc_to && CONSTANT_P (to_addr))
	data.to_addr = copy_addr_to_reg (to_addr);
    }

  if (! SLOW_UNALIGNED_ACCESS (word_mode, align)
      || align > MOVE_MAX * BITS_PER_UNIT || align >= BIGGEST_ALIGNMENT)
    align = MOVE_MAX * BITS_PER_UNIT;

  /* First move what we can in the largest integer mode, then go to
     successively smaller modes.  */

  while (max_size > 1)
    {
      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (mode == VOIDmode)
	break;

      icode = mov_optab->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	move_by_pieces_1 (GEN_FCN (icode), mode, &data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  if (data.len > 0)
    abort ();
}

/* Return number of insns required to move L bytes by pieces.
   ALIGN (in bits) is maximum alignment we can assume.  */

static unsigned HOST_WIDE_INT
move_by_pieces_ninsns (l, align)
     unsigned HOST_WIDE_INT l;
     unsigned int align;
{
  unsigned HOST_WIDE_INT n_insns = 0;
  unsigned HOST_WIDE_INT max_size = MOVE_MAX + 1;

  if (! SLOW_UNALIGNED_ACCESS (word_mode, align)
      || align > MOVE_MAX * BITS_PER_UNIT || align >= BIGGEST_ALIGNMENT)
    align = MOVE_MAX * BITS_PER_UNIT;

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
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	n_insns += l / GET_MODE_SIZE (mode), l %= GET_MODE_SIZE (mode);

      max_size = GET_MODE_SIZE (mode);
    }

  if (l)
    abort ();
  return n_insns;
}

/* Subroutine of move_by_pieces.  Move as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
move_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) PARAMS ((rtx, ...));
     enum machine_mode mode;
     struct move_by_pieces *data;
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
	  abort ();
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

/* Emit code to move a block Y to a block X.
   This may be done with string-move instructions,
   with multiple scalar move instructions, or with a library call.

   Both X and Y must be MEM rtx's (perhaps inside VOLATILE)
   with mode BLKmode.
   SIZE is an rtx that says how long they are.
   ALIGN is the maximum alignment we can assume they have.

   Return the address of the new block, if memcpy is called and returns it,
   0 otherwise.  */

rtx
emit_block_move (x, y, size)
     rtx x, y;
     rtx size;
{
  rtx retval = 0;
#ifdef TARGET_MEM_FUNCTIONS
  static tree fn;
  tree call_expr, arg_list;
#endif
  unsigned int align = MIN (MEM_ALIGN (x), MEM_ALIGN (y));

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

  if (GET_CODE (size) == CONST_INT && MOVE_BY_PIECES_P (INTVAL (size), align))
    move_by_pieces (x, y, INTVAL (size), align);
  else
    {
      /* Try the most limited insn first, because there's no point
	 including more than one in the machine description unless
	 the more limited one has some advantage.  */

      rtx opalign = GEN_INT (align / BITS_PER_UNIT);
      enum machine_mode mode;

      /* Since this is a move insn, we don't care about volatility.  */
      volatile_ok = 1;

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	{
	  enum insn_code code = movstr_optab[(int) mode];
	  insn_operand_predicate_fn pred;

	  if (code != CODE_FOR_nothing
	      /* We don't need MODE to be narrower than BITS_PER_HOST_WIDE_INT
		 here because if SIZE is less than the mode mask, as it is
		 returned by the macro, it will definitely be less than the
		 actual mode mask.  */
	      && ((GET_CODE (size) == CONST_INT
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

	      pat = GEN_FCN ((int) code) (x, y, op2, opalign);
	      if (pat)
		{
		  emit_insn (pat);
		  volatile_ok = 0;
		  return 0;
		}
	      else
		delete_insns_since (last);
	    }
	}

      volatile_ok = 0;

      /* X, Y, or SIZE may have been passed through protect_from_queue.

	 It is unsafe to save the value generated by protect_from_queue
	 and reuse it later.  Consider what happens if emit_queue is
	 called before the return value from protect_from_queue is used.

	 Expansion of the CALL_EXPR below will call emit_queue before
	 we are finished emitting RTL for argument setup.  So if we are
	 not careful we could get the wrong value for an argument.

	 To avoid this problem we go ahead and emit code to copy X, Y &
	 SIZE into new pseudos.  We can then place those new pseudos
	 into an RTL_EXPR and use them later, even after a call to
	 emit_queue.

	 Note this is not strictly needed for library calls since they
	 do not call emit_queue before loading their arguments.  However,
	 we may need to have library calls call emit_queue in the future
	 since failing to do so could cause problems for targets which
	 define SMALL_REGISTER_CLASSES and pass arguments in registers.  */
      x = copy_to_mode_reg (Pmode, XEXP (x, 0));
      y = copy_to_mode_reg (Pmode, XEXP (y, 0));

#ifdef TARGET_MEM_FUNCTIONS
      size = copy_to_mode_reg (TYPE_MODE (sizetype), size);
#else
      size = convert_to_mode (TYPE_MODE (integer_type_node), size,
			      TREE_UNSIGNED (integer_type_node));
      size = copy_to_mode_reg (TYPE_MODE (integer_type_node), size);
#endif

#ifdef TARGET_MEM_FUNCTIONS
      /* It is incorrect to use the libcall calling conventions to call
	 memcpy in this context.

	 This could be a user call to memcpy and the user may wish to
	 examine the return value from memcpy.

	 For targets where libcalls and normal calls have different conventions
	 for returning pointers, we could end up generating incorrect code.

	 So instead of using a libcall sequence we build up a suitable
	 CALL_EXPR and expand the call in the normal fashion.  */
      if (fn == NULL_TREE)
	{
	  tree fntype;

	  /* This was copied from except.c, I don't know if all this is
	     necessary in this context or not.  */
	  fn = get_identifier ("memcpy");
	  fntype = build_pointer_type (void_type_node);
	  fntype = build_function_type (fntype, NULL_TREE);
	  fn = build_decl (FUNCTION_DECL, fn, fntype);
	  ggc_add_tree_root (&fn, 1);
	  DECL_EXTERNAL (fn) = 1;
	  TREE_PUBLIC (fn) = 1;
	  DECL_ARTIFICIAL (fn) = 1;
	  TREE_NOTHROW (fn) = 1;
	  make_decl_rtl (fn, NULL);
	  assemble_external (fn);
	}

      /* We need to make an argument list for the function call.

	 memcpy has three arguments, the first two are void * addresses and
	 the last is a size_t byte count for the copy.  */
      arg_list
	= build_tree_list (NULL_TREE,
			   make_tree (build_pointer_type (void_type_node), x));
      TREE_CHAIN (arg_list)
	= build_tree_list (NULL_TREE,
			   make_tree (build_pointer_type (void_type_node), y));
      TREE_CHAIN (TREE_CHAIN (arg_list))
	 = build_tree_list (NULL_TREE, make_tree (sizetype, size));
      TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (arg_list))) = NULL_TREE;

      /* Now we have to build up the CALL_EXPR itself.  */
      call_expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fn)), fn);
      call_expr = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fn)),
			 call_expr, arg_list, NULL_TREE);
      TREE_SIDE_EFFECTS (call_expr) = 1;

      retval = expand_expr (call_expr, NULL_RTX, VOIDmode, 0);
#else
      emit_library_call (bcopy_libfunc, LCT_NORMAL,
			 VOIDmode, 3, y, Pmode, x, Pmode,
			 convert_to_mode (TYPE_MODE (integer_type_node), size,
					  TREE_UNSIGNED (integer_type_node)),
			 TYPE_MODE (integer_type_node));
#endif

      /* If we are initializing a readonly value, show the above call
	 clobbered it.  Otherwise, a load from it may erroneously be hoisted
	 from a loop.  */
      if (RTX_UNCHANGING_P (x))
	emit_insn (gen_rtx_CLOBBER (VOIDmode, x));
    }

  return retval;
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
   The number of registers to be filled is NREGS.  SIZE indicates the number
   of bytes in the object X.  */

void
move_block_from_reg (regno, x, nregs, size)
     int regno;
     rtx x;
     int nregs;
     int size;
{
  int i;
#ifdef HAVE_store_multiple
  rtx pat;
  rtx last;
#endif
  enum machine_mode mode;

  if (nregs == 0)
    return;

  /* If SIZE is that of a mode no bigger than a word, just use that
     mode's store operation.  */
  if (size <= UNITS_PER_WORD
      && (mode = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0)) != BLKmode
      && !FUNCTION_ARG_REG_LITTLE_ENDIAN)
    {
      emit_move_insn (adjust_address (x, mode, 0), gen_rtx_REG (mode, regno));
      return;
    }

  /* Blocks smaller than a word on a BYTES_BIG_ENDIAN machine must be aligned
     to the left before storing to memory.  Note that the previous test
     doesn't handle all cases (e.g. SIZE == 3).  */
  if (size < UNITS_PER_WORD
      && BYTES_BIG_ENDIAN
      && !FUNCTION_ARG_REG_LITTLE_ENDIAN)
    {
      rtx tem = operand_subword (x, 0, 1, BLKmode);
      rtx shift;

      if (tem == 0)
	abort ();

      shift = expand_shift (LSHIFT_EXPR, word_mode,
			    gen_rtx_REG (word_mode, regno),
			    build_int_2 ((UNITS_PER_WORD - size)
					 * BITS_PER_UNIT, 0), NULL_RTX, 0);
      emit_move_insn (tem, shift);
      return;
    }

  /* See if the machine can do this with a store multiple insn.  */
#ifdef HAVE_store_multiple
  if (HAVE_store_multiple)
    {
      last = get_last_insn ();
      pat = gen_store_multiple (x, gen_rtx_REG (word_mode, regno),
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

      if (tem == 0)
	abort ();

      emit_move_insn (tem, gen_rtx_REG (word_mode, regno + i));
    }
}

/* Emit code to move a block SRC to a block DST, where DST is non-consecutive
   registers represented by a PARALLEL.  SSIZE represents the total size of
   block SRC in bytes, or -1 if not known.  */
/* ??? If SSIZE % UNITS_PER_WORD != 0, we make the blatant assumption that
   the balance will be in what would be the low-order memory addresses, i.e.
   left justified for big endian, right justified for little endian.  This
   happens to be true for the targets currently using this support.  If this
   ever changes, a new target macro along the lines of FUNCTION_ARG_PADDING
   would be needed.  */

void
emit_group_load (dst, orig_src, ssize)
     rtx dst, orig_src;
     int ssize;
{
  rtx *tmps, src;
  int start, i;

  if (GET_CODE (dst) != PARALLEL)
    abort ();

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (dst, 0, 0), 0))
    start = 0;
  else
    start = 1;

  tmps = (rtx *) alloca (sizeof (rtx) * XVECLEN (dst, 0));

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
	  shift = (bytelen - (ssize - bytepos)) * BITS_PER_UNIT;
	  bytelen = ssize - bytepos;
	  if (bytelen <= 0)
	    abort ();
	}

      /* If we won't be loading directly from memory, protect the real source
	 from strange tricks we might play; but make sure that the source can
	 be loaded directly into the destination.  */
      src = orig_src;
      if (GET_CODE (orig_src) != MEM
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
      if (GET_CODE (src) == MEM
	  && MEM_ALIGN (src) >= GET_MODE_ALIGNMENT (mode)
	  && bytepos * BITS_PER_UNIT % GET_MODE_ALIGNMENT (mode) == 0
	  && bytelen == GET_MODE_SIZE (mode))
	{
	  tmps[i] = gen_reg_rtx (mode);
	  emit_move_insn (tmps[i], adjust_address (src, mode, bytepos));
	}
      else if (GET_CODE (src) == CONCAT)
	{
	  if ((bytepos == 0
	       && bytelen == GET_MODE_SIZE (GET_MODE (XEXP (src, 0))))
	      || (bytepos == (HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (XEXP (src, 0)))
		  && bytelen == GET_MODE_SIZE (GET_MODE (XEXP (src, 1)))))
	    {
	      tmps[i] = XEXP (src, bytepos != 0);
	      if (! CONSTANT_P (tmps[i])
		  && (GET_CODE (tmps[i]) != REG || GET_MODE (tmps[i]) != mode))
		tmps[i] = extract_bit_field (tmps[i], bytelen * BITS_PER_UNIT,
					     0, 1, NULL_RTX, mode, mode, ssize);
	    }
	  else if (bytepos == 0)
	    {
	      rtx mem = assign_stack_temp (GET_MODE (src),
					   GET_MODE_SIZE (GET_MODE (src)), 0);
	      emit_move_insn (mem, src);
	      tmps[i] = adjust_address (mem, mode, 0);
	    }
	  else
	    abort ();
	}
      else if (CONSTANT_P (src)
	       || (GET_CODE (src) == REG && GET_MODE (src) == mode))
	tmps[i] = src;
      else
	tmps[i] = extract_bit_field (src, bytelen * BITS_PER_UNIT,
				     bytepos * BITS_PER_UNIT, 1, NULL_RTX,
				     mode, mode, ssize);

      if (BYTES_BIG_ENDIAN && shift)
	expand_binop (mode, ashl_optab, tmps[i], GEN_INT (shift),
		      tmps[i], 0, OPTAB_WIDEN);
    }

  emit_queue ();

  /* Copy the extracted pieces into the proper (probable) hard regs.  */
  for (i = start; i < XVECLEN (dst, 0); i++)
    emit_move_insn (XEXP (XVECEXP (dst, 0, i), 0), tmps[i]);
}

/* Emit code to move a block SRC to a block DST, where SRC is non-consecutive
   registers represented by a PARALLEL.  SSIZE represents the total size of
   block DST, or -1 if not known.  */

void
emit_group_store (orig_dst, src, ssize)
     rtx orig_dst, src;
     int ssize;
{
  rtx *tmps, dst;
  int start, i;

  if (GET_CODE (src) != PARALLEL)
    abort ();

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (src, 0, 0), 0))
    start = 0;
  else
    start = 1;

  tmps = (rtx *) alloca (sizeof (rtx) * XVECLEN (src, 0));

  /* Copy the (probable) hard regs into pseudos.  */
  for (i = start; i < XVECLEN (src, 0); i++)
    {
      rtx reg = XEXP (XVECEXP (src, 0, i), 0);
      tmps[i] = gen_reg_rtx (GET_MODE (reg));
      emit_move_insn (tmps[i], reg);
    }
  emit_queue ();

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
      emit_group_store (temp, src, ssize);
      emit_group_load (dst, temp, ssize);
      return;
    }
  else if (GET_CODE (dst) != MEM && GET_CODE (dst) != CONCAT)
    {
      dst = gen_reg_rtx (GET_MODE (orig_dst));
      /* Make life a bit easier for combine.  */
      emit_move_insn (dst, const0_rtx);
    }

  /* Process the pieces.  */
  for (i = start; i < XVECLEN (src, 0); i++)
    {
      HOST_WIDE_INT bytepos = INTVAL (XEXP (XVECEXP (src, 0, i), 1));
      enum machine_mode mode = GET_MODE (tmps[i]);
      unsigned int bytelen = GET_MODE_SIZE (mode);
      rtx dest = dst;

      /* Handle trailing fragments that run over the size of the struct.  */
      if (ssize >= 0 && bytepos + (HOST_WIDE_INT) bytelen > ssize)
	{
	  if (BYTES_BIG_ENDIAN)
	    {
	      int shift = (bytelen - (ssize - bytepos)) * BITS_PER_UNIT;
	      expand_binop (mode, ashr_optab, tmps[i], GEN_INT (shift),
			    tmps[i], 0, OPTAB_WIDEN);
	    }
	  bytelen = ssize - bytepos;
	}

      if (GET_CODE (dst) == CONCAT)
	{
	  if (bytepos + bytelen <= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0))))
	    dest = XEXP (dst, 0);
	  else if (bytepos >= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0))))
	    {
	      bytepos -= GET_MODE_SIZE (GET_MODE (XEXP (dst, 0)));
	      dest = XEXP (dst, 1);
	    }
	  else
	    abort ();
	}

      /* Optimize the access just a bit.  */
      if (GET_CODE (dest) == MEM
	  && MEM_ALIGN (dest) >= GET_MODE_ALIGNMENT (mode)
	  && bytepos * BITS_PER_UNIT % GET_MODE_ALIGNMENT (mode) == 0
	  && bytelen == GET_MODE_SIZE (mode))
	emit_move_insn (adjust_address (dest, mode, bytepos), tmps[i]);
      else
	store_bit_field (dest, bytelen * BITS_PER_UNIT, bytepos * BITS_PER_UNIT,
			 mode, tmps[i], ssize);
    }

  emit_queue ();

  /* Copy from the pseudo into the (probable) hard reg.  */
  if (GET_CODE (dst) == REG)
    emit_move_insn (orig_dst, dst);
}

/* Generate code to copy a BLKmode object of TYPE out of a
   set of registers starting with SRCREG into TGTBLK.  If TGTBLK
   is null, a stack temporary is created.  TGTBLK is returned.

   The primary purpose of this routine is to handle functions
   that return BLKmode structures in registers.  Some machines
   (the PA for example) want to return all small structures
   in registers regardless of the structure's alignment.  */

rtx
copy_blkmode_from_reg (tgtblk, srcreg, type)
     rtx tgtblk;
     rtx srcreg;
     tree type;
{
  unsigned HOST_WIDE_INT bytes = int_size_in_bytes (type);
  rtx src = NULL, dst = NULL;
  unsigned HOST_WIDE_INT bitsize = MIN (TYPE_ALIGN (type), BITS_PER_WORD);
  unsigned HOST_WIDE_INT bitpos, xbitpos, big_endian_correction = 0;

  if (tgtblk == 0)
    {
      tgtblk = assign_temp (build_qualified_type (type,
						  (TYPE_QUALS (type)
						   | TYPE_QUAL_CONST)),
			    0, 1, 1);
      preserve_temp_slots (tgtblk);
    }

  /* This code assumes srcreg is at least a full word.  If it isn't, copy it
     into a new pseudo which is a full word.

     If FUNCTION_ARG_REG_LITTLE_ENDIAN is set and convert_to_mode does a copy,
     the wrong part of the register gets copied so we fake a type conversion
     in place.  */
  if (GET_MODE (srcreg) != BLKmode
      && GET_MODE_SIZE (GET_MODE (srcreg)) < UNITS_PER_WORD)
    {
      if (FUNCTION_ARG_REG_LITTLE_ENDIAN)
	srcreg = simplify_gen_subreg (word_mode, srcreg, GET_MODE (srcreg), 0);
      else
	srcreg = convert_to_mode (word_mode, srcreg, TREE_UNSIGNED (type));
    }

  /* Structures whose size is not a multiple of a word are aligned
     to the least significant byte (to the right).  On a BYTES_BIG_ENDIAN
     machine, this means we must skip the empty high order bytes when
     calculating the bit offset.  */
  if (BYTES_BIG_ENDIAN
      && !FUNCTION_ARG_REG_LITTLE_ENDIAN
      && bytes % UNITS_PER_WORD)
    big_endian_correction
      = (BITS_PER_WORD - ((bytes % UNITS_PER_WORD) * BITS_PER_UNIT));

  /* Copy the structure BITSIZE bites at a time.

     We could probably emit more efficient code for machines which do not use
     strict alignment, but it doesn't seem worth the effort at the current
     time.  */
  for (bitpos = 0, xbitpos = big_endian_correction;
       bitpos < bytes * BITS_PER_UNIT;
       bitpos += bitsize, xbitpos += bitsize)
    {
      /* We need a new source operand each time xbitpos is on a
	 word boundary and when xbitpos == big_endian_correction
	 (the first time through).  */
      if (xbitpos % BITS_PER_WORD == 0
	  || xbitpos == big_endian_correction)
	src = operand_subword_force (srcreg, xbitpos / BITS_PER_WORD,
				     GET_MODE (srcreg));

      /* We need a new destination operand each time bitpos is on
	 a word boundary.  */
      if (bitpos % BITS_PER_WORD == 0)
	dst = operand_subword (tgtblk, bitpos / BITS_PER_WORD, 1, BLKmode);

      /* Use xbitpos for the source extraction (right justified) and
	 xbitpos for the destination store (left justified).  */
      store_bit_field (dst, bitsize, bitpos % BITS_PER_WORD, word_mode,
		       extract_bit_field (src, bitsize,
					  xbitpos % BITS_PER_WORD, 1,
					  NULL_RTX, word_mode, word_mode,
					  BITS_PER_WORD),
		       BITS_PER_WORD);
    }

  return tgtblk;
}

/* Add a USE expression for REG to the (possibly empty) list pointed
   to by CALL_FUSAGE.  REG must denote a hard register.  */

void
use_reg (call_fusage, reg)
     rtx *call_fusage, reg;
{
  if (GET_CODE (reg) != REG
      || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    abort ();

  *call_fusage
    = gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_USE (VOIDmode, reg), *call_fusage);
}

/* Add USE expressions to *CALL_FUSAGE for each of NREGS consecutive regs,
   starting at REGNO.  All of these registers must be hard registers.  */

void
use_regs (call_fusage, regno, nregs)
     rtx *call_fusage;
     int regno;
     int nregs;
{
  int i;

  if (regno + nregs > FIRST_PSEUDO_REGISTER)
    abort ();

  for (i = 0; i < nregs; i++)
    use_reg (call_fusage, gen_rtx_REG (reg_raw_mode[regno + i], regno + i));
}

/* Add USE expressions to *CALL_FUSAGE for each REG contained in the
   PARALLEL REGS.  This is for calls that pass values in multiple
   non-contiguous locations.  The Irix 6 ABI has examples of this.  */

void
use_group_regs (call_fusage, regs)
     rtx *call_fusage;
     rtx regs;
{
  int i;

  for (i = 0; i < XVECLEN (regs, 0); i++)
    {
      rtx reg = XEXP (XVECEXP (regs, 0, i), 0);

      /* A NULL entry means the parameter goes both on the stack and in
	 registers.  This can also be a MEM for targets that pass values
	 partially on the stack and partially in registers.  */
      if (reg != 0 && GET_CODE (reg) == REG)
	use_reg (call_fusage, reg);
    }
}


int
can_store_by_pieces (len, constfun, constfundata, align)
     unsigned HOST_WIDE_INT len;
     rtx (*constfun) PARAMS ((PTR, HOST_WIDE_INT, enum machine_mode));
     PTR constfundata;
     unsigned int align;
{
  unsigned HOST_WIDE_INT max_size, l;
  HOST_WIDE_INT offset = 0;
  enum machine_mode mode, tmode;
  enum insn_code icode;
  int reverse;
  rtx cst;

  if (! MOVE_BY_PIECES_P (len, align))
    return 0;

  if (! SLOW_UNALIGNED_ACCESS (word_mode, align)
      || align > MOVE_MAX * BITS_PER_UNIT || align >= BIGGEST_ALIGNMENT)
    align = MOVE_MAX * BITS_PER_UNIT;

  /* We would first store what we can in the largest integer mode, then go to
     successively smaller modes.  */

  for (reverse = 0;
       reverse <= (HAVE_PRE_DECREMENT || HAVE_POST_DECREMENT);
       reverse++)
    {
      l = len;
      mode = VOIDmode;
      max_size = MOVE_MAX_PIECES + 1;
      while (max_size > 1)
	{
	  for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	       tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	    if (GET_MODE_SIZE (tmode) < max_size)
	      mode = tmode;

	  if (mode == VOIDmode)
	    break;

	  icode = mov_optab->handlers[(int) mode].insn_code;
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
      if (l != 0)
	abort ();
    }

  return 1;
}

/* Generate several move instructions to store LEN bytes generated by
   CONSTFUN to block TO.  (A MEM rtx with BLKmode).  CONSTFUNDATA is a
   pointer which will be passed as argument in every CONSTFUN call.
   ALIGN is maximum alignment we can assume.  */

void
store_by_pieces (to, len, constfun, constfundata, align)
     rtx to;
     unsigned HOST_WIDE_INT len;
     rtx (*constfun) PARAMS ((PTR, HOST_WIDE_INT, enum machine_mode));
     PTR constfundata;
     unsigned int align;
{
  struct store_by_pieces data;

  if (! MOVE_BY_PIECES_P (len, align))
    abort ();
  to = protect_from_queue (to, 1);
  data.constfun = constfun;
  data.constfundata = constfundata;
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
}

/* Generate several move instructions to clear LEN bytes of block TO.  (A MEM
   rtx with BLKmode).  The caller must pass TO through protect_from_queue
   before calling. ALIGN is maximum alignment we can assume.  */

static void
clear_by_pieces (to, len, align)
     rtx to;
     unsigned HOST_WIDE_INT len;
     unsigned int align;
{
  struct store_by_pieces data;

  data.constfun = clear_by_pieces_1;
  data.constfundata = NULL;
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
}

/* Callback routine for clear_by_pieces.
   Return const0_rtx unconditionally.  */

static rtx
clear_by_pieces_1 (data, offset, mode)
     PTR data ATTRIBUTE_UNUSED;
     HOST_WIDE_INT offset ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return const0_rtx;
}

/* Subroutine of clear_by_pieces and store_by_pieces.
   Generate several move instructions to store LEN bytes of block TO.  (A MEM
   rtx with BLKmode).  The caller must pass TO through protect_from_queue
   before calling.  ALIGN is maximum alignment we can assume.  */

static void
store_by_pieces_1 (data, align)
     struct store_by_pieces *data;
     unsigned int align;
{
  rtx to_addr = XEXP (data->to, 0);
  unsigned HOST_WIDE_INT max_size = MOVE_MAX_PIECES + 1;
  enum machine_mode mode = VOIDmode, tmode;
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
      && move_by_pieces_ninsns (data->len, align) > 2)
    {
      /* Determine the main mode we'll be using.  */
      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (USE_STORE_PRE_DECREMENT (mode) && data->reverse && ! data->autinc_to)
	{
	  data->to_addr = copy_addr_to_reg (plus_constant (to_addr, data->len));
	  data->autinc_to = 1;
	  data->explicit_inc_to = -1;
	}

      if (USE_STORE_POST_INCREMENT (mode) && ! data->reverse
	  && ! data->autinc_to)
	{
	  data->to_addr = copy_addr_to_reg (to_addr);
	  data->autinc_to = 1;
	  data->explicit_inc_to = 1;
	}

      if ( !data->autinc_to && CONSTANT_P (to_addr))
	data->to_addr = copy_addr_to_reg (to_addr);
    }

  if (! SLOW_UNALIGNED_ACCESS (word_mode, align)
      || align > MOVE_MAX * BITS_PER_UNIT || align >= BIGGEST_ALIGNMENT)
    align = MOVE_MAX * BITS_PER_UNIT;

  /* First store what we can in the largest integer mode, then go to
     successively smaller modes.  */

  while (max_size > 1)
    {
      for (tmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmode != VOIDmode; tmode = GET_MODE_WIDER_MODE (tmode))
	if (GET_MODE_SIZE (tmode) < max_size)
	  mode = tmode;

      if (mode == VOIDmode)
	break;

      icode = mov_optab->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing && align >= GET_MODE_ALIGNMENT (mode))
	store_by_pieces_2 (GEN_FCN (icode), mode, data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  if (data->len != 0)
    abort ();
}

/* Subroutine of store_by_pieces_1.  Store as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
store_by_pieces_2 (genfun, mode, data)
     rtx (*genfun) PARAMS ((rtx, ...));
     enum machine_mode mode;
     struct store_by_pieces *data;
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
clear_storage (object, size)
     rtx object;
     rtx size;
{
#ifdef TARGET_MEM_FUNCTIONS
  static tree fn;
  tree call_expr, arg_list;
#endif
  rtx retval = 0;
  unsigned int align = (GET_CODE (object) == MEM ? MEM_ALIGN (object)
			: GET_MODE_ALIGNMENT (GET_MODE (object)));

  /* If OBJECT is not BLKmode and SIZE is the same size as its mode,
     just move a zero.  Otherwise, do this a piece at a time.  */
  if (GET_MODE (object) != BLKmode
      && GET_CODE (size) == CONST_INT
      && GET_MODE_SIZE (GET_MODE (object)) == (unsigned int) INTVAL (size))
    emit_move_insn (object, CONST0_RTX (GET_MODE (object)));
  else
    {
      object = protect_from_queue (object, 1);
      size = protect_from_queue (size, 0);

      if (GET_CODE (size) == CONST_INT
	  && MOVE_BY_PIECES_P (INTVAL (size), align))
	clear_by_pieces (object, INTVAL (size), align);
      else
	{
	  /* Try the most limited insn first, because there's no point
	     including more than one in the machine description unless
	     the more limited one has some advantage.  */

	  rtx opalign = GEN_INT (align / BITS_PER_UNIT);
	  enum machine_mode mode;

	  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
	       mode = GET_MODE_WIDER_MODE (mode))
	    {
	      enum insn_code code = clrstr_optab[(int) mode];
	      insn_operand_predicate_fn pred;

	      if (code != CODE_FOR_nothing
		  /* We don't need MODE to be narrower than
		     BITS_PER_HOST_WIDE_INT here because if SIZE is less than
		     the mode mask, as it is returned by the macro, it will
		     definitely be less than the actual mode mask.  */
		  && ((GET_CODE (size) == CONST_INT
		       && ((unsigned HOST_WIDE_INT) INTVAL (size)
			   <= (GET_MODE_MASK (mode) >> 1)))
		      || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
		  && ((pred = insn_data[(int) code].operand[0].predicate) == 0
		      || (*pred) (object, BLKmode))
		  && ((pred = insn_data[(int) code].operand[2].predicate) == 0
		      || (*pred) (opalign, VOIDmode)))
		{
		  rtx op1;
		  rtx last = get_last_insn ();
		  rtx pat;

		  op1 = convert_to_mode (mode, size, 1);
		  pred = insn_data[(int) code].operand[1].predicate;
		  if (pred != 0 && ! (*pred) (op1, mode))
		    op1 = copy_to_mode_reg (mode, op1);

		  pat = GEN_FCN ((int) code) (object, op1, opalign);
		  if (pat)
		    {
		      emit_insn (pat);
		      return 0;
		    }
		  else
		    delete_insns_since (last);
		}
	    }

	  /* OBJECT or SIZE may have been passed through protect_from_queue.

	     It is unsafe to save the value generated by protect_from_queue
	     and reuse it later.  Consider what happens if emit_queue is
	     called before the return value from protect_from_queue is used.

	     Expansion of the CALL_EXPR below will call emit_queue before
	     we are finished emitting RTL for argument setup.  So if we are
	     not careful we could get the wrong value for an argument.

	     To avoid this problem we go ahead and emit code to copy OBJECT
	     and SIZE into new pseudos.  We can then place those new pseudos
	     into an RTL_EXPR and use them later, even after a call to
	     emit_queue.

	     Note this is not strictly needed for library calls since they
	     do not call emit_queue before loading their arguments.  However,
	     we may need to have library calls call emit_queue in the future
	     since failing to do so could cause problems for targets which
	     define SMALL_REGISTER_CLASSES and pass arguments in registers.  */
	  object = copy_to_mode_reg (Pmode, XEXP (object, 0));

#ifdef TARGET_MEM_FUNCTIONS
	  size = copy_to_mode_reg (TYPE_MODE (sizetype), size);
#else
	  size = convert_to_mode (TYPE_MODE (integer_type_node), size,
				  TREE_UNSIGNED (integer_type_node));
	  size = copy_to_mode_reg (TYPE_MODE (integer_type_node), size);
#endif

#ifdef TARGET_MEM_FUNCTIONS
	  /* It is incorrect to use the libcall calling conventions to call
	     memset in this context.

	     This could be a user call to memset and the user may wish to
	     examine the return value from memset.

	     For targets where libcalls and normal calls have different
	     conventions for returning pointers, we could end up generating
	     incorrect code.

	     So instead of using a libcall sequence we build up a suitable
	     CALL_EXPR and expand the call in the normal fashion.  */
	  if (fn == NULL_TREE)
	    {
	      tree fntype;

	      /* This was copied from except.c, I don't know if all this is
		 necessary in this context or not.  */
	      fn = get_identifier ("memset");
	      fntype = build_pointer_type (void_type_node);
	      fntype = build_function_type (fntype, NULL_TREE);
	      fn = build_decl (FUNCTION_DECL, fn, fntype);
	      ggc_add_tree_root (&fn, 1);
	      DECL_EXTERNAL (fn) = 1;
	      TREE_PUBLIC (fn) = 1;
	      DECL_ARTIFICIAL (fn) = 1;
	      TREE_NOTHROW (fn) = 1;
	      make_decl_rtl (fn, NULL);
	      assemble_external (fn);
	    }

	  /* We need to make an argument list for the function call.

	     memset has three arguments, the first is a void * addresses, the
	     second an integer with the initialization value, the last is a
	     size_t byte count for the copy.  */
	  arg_list
	    = build_tree_list (NULL_TREE,
			       make_tree (build_pointer_type (void_type_node),
					  object));
	  TREE_CHAIN (arg_list)
	    = build_tree_list (NULL_TREE,
			       make_tree (integer_type_node, const0_rtx));
	  TREE_CHAIN (TREE_CHAIN (arg_list))
	    = build_tree_list (NULL_TREE, make_tree (sizetype, size));
	  TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (arg_list))) = NULL_TREE;

	  /* Now we have to build up the CALL_EXPR itself.  */
	  call_expr = build1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (fn)), fn);
	  call_expr = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fn)),
			     call_expr, arg_list, NULL_TREE);
	  TREE_SIDE_EFFECTS (call_expr) = 1;

	  retval = expand_expr (call_expr, NULL_RTX, VOIDmode, 0);
#else
	  emit_library_call (bzero_libfunc, LCT_NORMAL,
			     VOIDmode, 2, object, Pmode, size,
			     TYPE_MODE (integer_type_node));
#endif

	  /* If we are initializing a readonly value, show the above call
	     clobbered it.  Otherwise, a load from it may erroneously be
	     hoisted from a loop.  */
	  if (RTX_UNCHANGING_P (object))
	    emit_insn (gen_rtx_CLOBBER (VOIDmode, object));
	}
    }

  return retval;
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
  rtx y_cst = NULL_RTX;
  rtx last_insn;

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (mode == BLKmode || (GET_MODE (y) != mode && GET_MODE (y) != VOIDmode))
    abort ();

  /* Never force constant_p_rtx to memory.  */
  if (GET_CODE (y) == CONSTANT_P_RTX)
    ;
  else if (CONSTANT_P (y) && ! LEGITIMATE_CONSTANT_P (y))
    {
      y_cst = y;
      y = force_const_mem (mode, y);
    }

  /* If X or Y are memory references, verify that their addresses are valid
     for the machine.  */
  if (GET_CODE (x) == MEM
      && ((! memory_address_p (GET_MODE (x), XEXP (x, 0))
	   && ! push_operand (x, GET_MODE (x)))
	  || (flag_force_addr
	      && CONSTANT_ADDRESS_P (XEXP (x, 0)))))
    x = validize_mem (x);

  if (GET_CODE (y) == MEM
      && (! memory_address_p (GET_MODE (y), XEXP (y, 0))
	  || (flag_force_addr
	      && CONSTANT_ADDRESS_P (XEXP (y, 0)))))
    y = validize_mem (y);

  if (mode == BLKmode)
    abort ();

  last_insn = emit_move_insn_1 (x, y);

  if (y_cst && GET_CODE (x) == REG)
    set_unique_reg_note (last_insn, REG_EQUAL, y_cst);

  return last_insn;
}

/* Low level part of emit_move_insn.
   Called just like emit_move_insn, but assumes X and Y
   are basically valid.  */

rtx
emit_move_insn_1 (x, y)
     rtx x, y;
{
  enum machine_mode mode = GET_MODE (x);
  enum machine_mode submode;
  enum mode_class class = GET_MODE_CLASS (mode);

  if ((unsigned int) mode >= (unsigned int) MAX_MACHINE_MODE)
    abort ();

  if (mov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    return
      emit_insn (GEN_FCN (mov_optab->handlers[(int) mode].insn_code) (x, y));

  /* Expand complex moves by moving real part and imag part, if possible.  */
  else if ((class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
	   && BLKmode != (submode = mode_for_size ((GET_MODE_UNIT_SIZE (mode)
						    * BITS_PER_UNIT),
						   (class == MODE_COMPLEX_INT
						    ? MODE_INT : MODE_FLOAT),
						   0))
	   && (mov_optab->handlers[(int) submode].insn_code
	       != CODE_FOR_nothing))
    {
      /* Don't split destination if it is a stack push.  */
      int stack = push_operand (x, GET_MODE (x));

#ifdef PUSH_ROUNDING
      /* In case we output to the stack, but the size is smaller machine can
	 push exactly, we need to use move instructions.  */
      if (stack
	  && (PUSH_ROUNDING (GET_MODE_SIZE (submode))
	      != GET_MODE_SIZE (submode)))
	{
	  rtx temp;
	  HOST_WIDE_INT offset1, offset2;

	  /* Do not use anti_adjust_stack, since we don't want to update
	     stack_pointer_delta.  */
	  temp = expand_binop (Pmode,
#ifdef STACK_GROWS_DOWNWARD
			       sub_optab,
#else
			       add_optab,
#endif
			       stack_pointer_rtx,
			       GEN_INT
				 (PUSH_ROUNDING
				  (GET_MODE_SIZE (GET_MODE (x)))),
			       stack_pointer_rtx, 0, OPTAB_LIB_WIDEN);

	  if (temp != stack_pointer_rtx)
	    emit_move_insn (stack_pointer_rtx, temp);

#ifdef STACK_GROWS_DOWNWARD
	  offset1 = 0;
	  offset2 = GET_MODE_SIZE (submode);
#else
	  offset1 = -PUSH_ROUNDING (GET_MODE_SIZE (GET_MODE (x)));
	  offset2 = (-PUSH_ROUNDING (GET_MODE_SIZE (GET_MODE (x)))
		     + GET_MODE_SIZE (submode));
#endif

	  emit_move_insn (change_address (x, submode,
					  gen_rtx_PLUS (Pmode,
						        stack_pointer_rtx,
							GEN_INT (offset1))),
			  gen_realpart (submode, y));
	  emit_move_insn (change_address (x, submode,
					  gen_rtx_PLUS (Pmode,
						        stack_pointer_rtx,
							GEN_INT (offset2))),
			  gen_imagpart (submode, y));
	}
      else
#endif
      /* If this is a stack, push the highpart first, so it
	 will be in the argument order.

	 In that case, change_address is used only to convert
	 the mode, not to change the address.  */
      if (stack)
	{
	  /* Note that the real part always precedes the imag part in memory
	     regardless of machine's endianness.  */
#ifdef STACK_GROWS_DOWNWARD
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx_MEM (submode, XEXP (x, 0)),
		      gen_imagpart (submode, y)));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx_MEM (submode, XEXP (x, 0)),
		      gen_realpart (submode, y)));
#else
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx_MEM (submode, XEXP (x, 0)),
		      gen_realpart (submode, y)));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx_MEM (submode, XEXP (x, 0)),
		      gen_imagpart (submode, y)));
#endif
	}
      else
	{
	  rtx realpart_x, realpart_y;
	  rtx imagpart_x, imagpart_y;

	  /* If this is a complex value with each part being smaller than a
	     word, the usual calling sequence will likely pack the pieces into
	     a single register.  Unfortunately, SUBREG of hard registers only
	     deals in terms of words, so we have a problem converting input
	     arguments to the CONCAT of two registers that is used elsewhere
	     for complex values.  If this is before reload, we can copy it into
	     memory and reload.  FIXME, we should see about using extract and
	     insert on integer registers, but complex short and complex char
	     variables should be rarely used.  */
	  if (GET_MODE_BITSIZE (mode) < 2 * BITS_PER_WORD
	      && (reload_in_progress | reload_completed) == 0)
	    {
	      int packed_dest_p
		= (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER);
	      int packed_src_p
		= (REG_P (y) && REGNO (y) < FIRST_PSEUDO_REGISTER);

	      if (packed_dest_p || packed_src_p)
		{
		  enum mode_class reg_class = ((class == MODE_COMPLEX_FLOAT)
					       ? MODE_FLOAT : MODE_INT);

		  enum machine_mode reg_mode
		    = mode_for_size (GET_MODE_BITSIZE (mode), reg_class, 1);

		  if (reg_mode != BLKmode)
		    {
		      rtx mem = assign_stack_temp (reg_mode,
						   GET_MODE_SIZE (mode), 0);
		      rtx cmem = adjust_address (mem, mode, 0);

		      cfun->cannot_inline
			= N_("function using short complex types cannot be inline");

		      if (packed_dest_p)
			{
			  rtx sreg = gen_rtx_SUBREG (reg_mode, x, 0);

			  emit_move_insn_1 (cmem, y);
			  return emit_move_insn_1 (sreg, mem);
			}
		      else
			{
			  rtx sreg = gen_rtx_SUBREG (reg_mode, y, 0);

			  emit_move_insn_1 (mem, sreg);
			  return emit_move_insn_1 (x, cmem);
			}
		    }
		}
	    }

	  realpart_x = gen_realpart (submode, x);
	  realpart_y = gen_realpart (submode, y);
	  imagpart_x = gen_imagpart (submode, x);
	  imagpart_y = gen_imagpart (submode, y);

	  /* Show the output dies here.  This is necessary for SUBREGs
	     of pseudos since we cannot track their lifetimes correctly;
	     hard regs shouldn't appear here except as return values.
	     We never want to emit such a clobber after reload.  */
	  if (x != y
	      && ! (reload_in_progress || reload_completed)
	      && (GET_CODE (realpart_x) == SUBREG
		  || GET_CODE (imagpart_x) == SUBREG))
	    emit_insn (gen_rtx_CLOBBER (VOIDmode, x));

	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (realpart_x, realpart_y));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (imagpart_x, imagpart_y));
	}

      return get_last_insn ();
    }

  /* This will handle any multi-word mode that lacks a move_insn pattern.
     However, you will get better code if you define such patterns,
     even if they must turn into multiple assembler instructions.  */
  else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    {
      rtx last_insn = 0;
      rtx seq, inner;
      int need_clobber;
      int i;

#ifdef PUSH_ROUNDING

      /* If X is a push on the stack, do the push now and replace
	 X with a reference to the stack pointer.  */
      if (push_operand (x, GET_MODE (x)))
	{
	  rtx temp;
	  enum rtx_code code;
	  
	  /* Do not use anti_adjust_stack, since we don't want to update
	     stack_pointer_delta.  */
	  temp = expand_binop (Pmode,
#ifdef STACK_GROWS_DOWNWARD
			       sub_optab,
#else
			       add_optab,
#endif
			       stack_pointer_rtx,
			       GEN_INT
				 (PUSH_ROUNDING
				  (GET_MODE_SIZE (GET_MODE (x)))),
			       stack_pointer_rtx, 0, OPTAB_LIB_WIDEN);

          if (temp != stack_pointer_rtx)
            emit_move_insn (stack_pointer_rtx, temp);

	  code = GET_CODE (XEXP (x, 0));

	  /* Just hope that small offsets off SP are OK.  */
	  if (code == POST_INC)
	    temp = gen_rtx_PLUS (Pmode, stack_pointer_rtx, 
				GEN_INT (-((HOST_WIDE_INT)
					   GET_MODE_SIZE (GET_MODE (x)))));
	  else if (code == POST_DEC)
	    temp = gen_rtx_PLUS (Pmode, stack_pointer_rtx, 
				GEN_INT (GET_MODE_SIZE (GET_MODE (x))));
	  else
	    temp = stack_pointer_rtx;

	  x = change_address (x, VOIDmode, temp);
	}
#endif

      /* If we are in reload, see if either operand is a MEM whose address
	 is scheduled for replacement.  */
      if (reload_in_progress && GET_CODE (x) == MEM
	  && (inner = find_replacement (&XEXP (x, 0))) != XEXP (x, 0))
	x = replace_equiv_address_nv (x, inner);
      if (reload_in_progress && GET_CODE (y) == MEM
	  && (inner = find_replacement (&XEXP (y, 0))) != XEXP (y, 0))
	y = replace_equiv_address_nv (y, inner);

      start_sequence ();

      need_clobber = 0;
      for (i = 0;
	   i < (GET_MODE_SIZE (mode) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
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

	  need_clobber |= (GET_CODE (xpart) == SUBREG);

	  last_insn = emit_move_insn (xpart, ypart);
	}

      seq = gen_sequence ();
      end_sequence ();

      /* Show the output dies here.  This is necessary for SUBREGs
	 of pseudos since we cannot track their lifetimes correctly;
	 hard regs shouldn't appear here except as return values.
	 We never want to emit such a clobber after reload.  */
      if (x != y
	  && ! (reload_in_progress || reload_completed)
	  && need_clobber != 0)
	emit_insn (gen_rtx_CLOBBER (VOIDmode, x));

      emit_insn (seq);

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
  rtx temp;

  size = convert_modes (Pmode, ptr_mode, size, 1);
  if (CONSTANT_P (size))
    anti_adjust_stack (plus_constant (size, extra));
  else if (GET_CODE (size) == REG && extra == 0)
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
      if (GET_CODE (size) == CONST_INT)
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
emit_single_push_insn (mode, x, type)
     rtx x;
     enum machine_mode mode;
     tree type;
{
  rtx dest_addr;
  unsigned rounded_size = PUSH_ROUNDING (GET_MODE_SIZE (mode));
  rtx dest;
  enum insn_code icode;
  insn_operand_predicate_fn pred;

  stack_pointer_delta += PUSH_ROUNDING (GET_MODE_SIZE (mode));
  /* If there is push pattern, use it.  Otherwise try old way of throwing
     MEM representing push operation to move expander.  */
  icode = push_optab->handlers[(int) mode].insn_code;
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
  else
    {
#ifdef STACK_GROWS_DOWNWARD
      dest_addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				GEN_INT (-(HOST_WIDE_INT) rounded_size));
#else
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
   words of X into registers starting with REG, and push the rest of X.
   The amount of space pushed is decreased by PARTIAL words,
   rounded *down* to a multiple of PARM_BOUNDARY.
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
emit_push_insn (x, mode, type, size, align, partial, reg, extra,
		args_addr, args_so_far, reg_parm_stack_space,
                alignment_pad)
     rtx x;
     enum machine_mode mode;
     tree type;
     rtx size;
     unsigned int align;
     int partial;
     rtx reg;
     int extra;
     rtx args_addr;
     rtx args_so_far;
     int reg_parm_stack_space;
     rtx alignment_pad;
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

  xinner = x = protect_from_queue (x, 0);

  if (mode == BLKmode)
    {
      /* Copy a block into the stack, entirely or partially.  */

      rtx temp;
      int used = partial * UNITS_PER_WORD;
      int offset = used % (PARM_BOUNDARY / BITS_PER_UNIT);
      int skip;

      if (size == 0)
	abort ();

      used -= offset;

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
	  && GET_CODE (size) == CONST_INT
	  && skip == 0
	  && (MOVE_BY_PIECES_P ((unsigned) INTVAL (size) - used, align))
	  /* Here we avoid the case of a structure whose weak alignment
	     forces many pushes of a small amount of data,
	     and such small pushes do rounding that causes trouble.  */
	  && ((! SLOW_UNALIGNED_ACCESS (word_mode, align))
	      || align >= BIGGEST_ALIGNMENT
	      || (PUSH_ROUNDING (align / BITS_PER_UNIT)
		  == (align / BITS_PER_UNIT)))
	  && PUSH_ROUNDING (INTVAL (size)) == INTVAL (size))
	{
	  /* Push padding now if padding above and stack grows down,
	     or if padding below and stack grows up.
	     But if space already allocated, this has already been done.  */
	  if (extra && args_addr == 0
	      && where_pad != none && where_pad != stack_direction)
	    anti_adjust_stack (GEN_INT (extra));

	  move_by_pieces (NULL, xinner, INTVAL (size) - used, align);
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
				   plus_constant (gen_rtx_PLUS (Pmode,
								args_addr,
								args_so_far),
						  skip));
	  target = gen_rtx_MEM (BLKmode, temp);

	  if (type != 0)
	    {
	      set_mem_attributes (target, type, 1);
	      /* Function incoming arguments may overlap with sibling call
		 outgoing arguments and we cannot allow reordering of reads
		 from function arguments with stores to outgoing arguments
		 of sibling calls.  */
	      set_mem_alias_set (target, 0);
	    }
	  else
	    set_mem_align (target, align);

	  /* TEMP is the address of the block.  Copy the data there.  */
	  if (GET_CODE (size) == CONST_INT
	      && MOVE_BY_PIECES_P ((unsigned) INTVAL (size), align))
	    {
	      move_by_pieces (target, xinner, INTVAL (size), align);
	      goto ret;
	    }
	  else
	    {
	      rtx opalign = GEN_INT (align / BITS_PER_UNIT);
	      enum machine_mode mode;

	      for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
		   mode != VOIDmode;
		   mode = GET_MODE_WIDER_MODE (mode))
		{
		  enum insn_code code = movstr_optab[(int) mode];
		  insn_operand_predicate_fn pred;

		  if (code != CODE_FOR_nothing
		      && ((GET_CODE (size) == CONST_INT
			   && ((unsigned HOST_WIDE_INT) INTVAL (size)
			       <= (GET_MODE_MASK (mode) >> 1)))
			  || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
		      && (!(pred = insn_data[(int) code].operand[0].predicate)
			  || ((*pred) (target, BLKmode)))
		      && (!(pred = insn_data[(int) code].operand[1].predicate)
			  || ((*pred) (xinner, BLKmode)))
		      && (!(pred = insn_data[(int) code].operand[3].predicate)
			  || ((*pred) (opalign, VOIDmode))))
		    {
		      rtx op2 = convert_to_mode (mode, size, 1);
		      rtx last = get_last_insn ();
		      rtx pat;

		      pred = insn_data[(int) code].operand[2].predicate;
		      if (pred != 0 && ! (*pred) (op2, mode))
			op2 = copy_to_mode_reg (mode, op2);

		      pat = GEN_FCN ((int) code) (target, xinner,
						  op2, opalign);
		      if (pat)
			{
			  emit_insn (pat);
			  goto ret;
			}
		      else
			delete_insns_since (last);
		    }
		}
	    }

	  if (!ACCUMULATE_OUTGOING_ARGS)
	    {
	      /* If the source is referenced relative to the stack pointer,
		 copy it to another register to stabilize it.  We do not need
		 to do this if we know that we won't be changing sp.  */

	      if (reg_mentioned_p (virtual_stack_dynamic_rtx, temp)
		  || reg_mentioned_p (virtual_outgoing_args_rtx, temp))
		temp = copy_to_reg (temp);
	    }

	  /* Make inhibit_defer_pop nonzero around the library call
	     to force it to pop the bcopy-arguments right away.  */
	  NO_DEFER_POP;
#ifdef TARGET_MEM_FUNCTIONS
	  emit_library_call (memcpy_libfunc, LCT_NORMAL,
			     VOIDmode, 3, temp, Pmode, XEXP (xinner, 0), Pmode,
			     convert_to_mode (TYPE_MODE (sizetype),
					      size, TREE_UNSIGNED (sizetype)),
			     TYPE_MODE (sizetype));
#else
	  emit_library_call (bcopy_libfunc, LCT_NORMAL,
			     VOIDmode, 3, XEXP (xinner, 0), Pmode, temp, Pmode,
			     convert_to_mode (TYPE_MODE (integer_type_node),
					      size,
					      TREE_UNSIGNED (integer_type_node)),
			     TYPE_MODE (integer_type_node));
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
      skip = (reg_parm_stack_space == 0) ? 0 : not_stack;

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
						  * UNITS_PER_WORD)),
			  reg_parm_stack_space, alignment_pad);
    }
  else
    {
      rtx addr;
      rtx target = NULL_RTX;
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
	  if (GET_CODE (args_so_far) == CONST_INT)
	    addr
	      = memory_address (mode,
				plus_constant (args_addr,
					       INTVAL (args_so_far)));
	  else
	    addr = memory_address (mode, gen_rtx_PLUS (Pmode, args_addr,
						       args_so_far));
	  target = addr;
	  dest = gen_rtx_MEM (mode, addr);
	  if (type != 0)
	    {
	      set_mem_attributes (dest, type, 1);
	      /* Function incoming arguments may overlap with sibling call
		 outgoing arguments and we cannot allow reordering of reads
		 from function arguments with stores to outgoing arguments
		 of sibling calls.  */
	      set_mem_alias_set (dest, 0);
	    }

	  emit_move_insn (dest, x);
	}

    }

 ret:
  /* If part should go in registers, copy that part
     into the appropriate registers.  Do this now, at the end,
     since mem-to-mem copies above may do function calls.  */
  if (partial > 0 && reg != 0)
    {
      /* Handle calls that pass values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      if (GET_CODE (reg) == PARALLEL)
	emit_group_load (reg, x, -1);  /* ??? size? */
      else
	move_block_to_reg (REGNO (reg), x, partial, mode);
    }

  if (extra && args_addr == 0 && where_pad == stack_direction)
    anti_adjust_stack (GEN_INT (extra));

  if (alignment_pad && args_addr == 0)
    anti_adjust_stack (alignment_pad);
}

/* Return X if X can be used as a subtarget in a sequence of arithmetic
   operations.  */

static rtx
get_subtarget (x)
     rtx x;
{
  return ((x == 0
	   /* Only registers can be subtargets.  */
	   || GET_CODE (x) != REG
	   /* If the register is readonly, it can't be set more than once.  */
	   || RTX_UNCHANGING_P (x)
	   /* Don't use hard regs to avoid extending their life.  */
	   || REGNO (x) < FIRST_PSEUDO_REGISTER
	   /* Avoid subtargets inside loops,
	      since they hide some invariant expressions.  */
	   || preserve_subexpressions_p ())
	  ? 0 : x);
}

/* Expand an assignment that stores the value of FROM into TO.
   If WANT_VALUE is nonzero, return an rtx for the value of TO.
   (This may contain a QUEUED rtx;
   if the value is constant, this rtx is a constant.)
   Otherwise, the returned value is NULL_RTX.

   SUGGEST_REG is no longer actually used.
   It used to mean, copy the value through a register
   and return that register, if that is possible.
   We now use WANT_VALUE to decide whether to do this.  */

rtx
expand_assignment (to, from, want_value, suggest_reg)
     tree to, from;
     int want_value;
     int suggest_reg ATTRIBUTE_UNUSED;
{
  rtx to_rtx = 0;
  rtx result;

  /* Don't crash if the lhs of the assignment was erroneous.  */

  if (TREE_CODE (to) == ERROR_MARK)
    {
      result = expand_expr (from, NULL_RTX, VOIDmode, 0);
      return want_value ? result : NULL_RTX;
    }

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.
     Assignment of an array element at a constant index, and assignment of
     an array element in an unaligned packed structure field, has the same
     problem.  */

  if (TREE_CODE (to) == COMPONENT_REF || TREE_CODE (to) == BIT_FIELD_REF
      || TREE_CODE (to) == ARRAY_REF || TREE_CODE (to) == ARRAY_RANGE_REF)
    {
      enum machine_mode mode1;
      HOST_WIDE_INT bitsize, bitpos;
      rtx orig_to_rtx;
      tree offset;
      int unsignedp;
      int volatilep = 0;
      tree tem;

      push_temp_slots ();
      tem = get_inner_reference (to, &bitsize, &bitpos, &offset, &mode1,
				 &unsignedp, &volatilep);

      /* If we are going to use store_bit_field and extract_bit_field,
	 make sure to_rtx will be safe for multiple use.  */

      if (mode1 == VOIDmode && want_value)
	tem = stabilize_reference (tem);

      orig_to_rtx = to_rtx = expand_expr (tem, NULL_RTX, VOIDmode, 0);

      if (offset != 0)
	{
	  rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, EXPAND_SUM);

	  if (GET_CODE (to_rtx) != MEM)
	    abort ();

#ifdef POINTERS_EXTEND_UNSIGNED
	  if (GET_MODE (offset_rtx) != Pmode)
	    offset_rtx = convert_memory_address (Pmode, offset_rtx);
#else
	  if (GET_MODE (offset_rtx) != ptr_mode)
	    offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);
#endif

	  /* A constant address in TO_RTX can have VOIDmode, we must not try
	     to call force_reg for that case.  Avoid that case.  */
	  if (GET_CODE (to_rtx) == MEM
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
				   highest_pow2_factor_for_type (TREE_TYPE (to),
								 offset));
	}

      if (GET_CODE (to_rtx) == MEM)
	{
	  tree old_expr = MEM_EXPR (to_rtx);

	  /* If the field is at offset zero, we could have been given the
	     DECL_RTX of the parent struct.  Don't munge it.  */
	  to_rtx = shallow_copy_rtx (to_rtx);

	  set_mem_attributes (to_rtx, to, 0);

	  /* If we changed MEM_EXPR, that means we're now referencing
	     the COMPONENT_REF, which means that MEM_OFFSET must be
	     relative to that field.  But we've not yet reflected BITPOS
	     in TO_RTX.  This will be done in store_field.  Adjust for
	     that by biasing MEM_OFFSET by -bitpos.  */
	  if (MEM_EXPR (to_rtx) != old_expr && MEM_OFFSET (to_rtx)
	      && (bitpos / BITS_PER_UNIT) != 0)
	    set_mem_offset (to_rtx, GEN_INT (INTVAL (MEM_OFFSET (to_rtx))
					     - (bitpos / BITS_PER_UNIT)));
	}

      /* Deal with volatile and readonly fields.  The former is only done
	 for MEM.  Also set MEM_KEEP_ALIAS_SET_P if needed.  */
      if (volatilep && GET_CODE (to_rtx) == MEM)
	{
	  if (to_rtx == orig_to_rtx)
	    to_rtx = copy_rtx (to_rtx);
	  MEM_VOLATILE_P (to_rtx) = 1;
	}

      if (TREE_CODE (to) == COMPONENT_REF
	  && TREE_READONLY (TREE_OPERAND (to, 1)))
	{
	  if (to_rtx == orig_to_rtx)
	    to_rtx = copy_rtx (to_rtx);
	  RTX_UNCHANGING_P (to_rtx) = 1;
	}

      if (GET_CODE (to_rtx) == MEM && ! can_address_p (to))
	{
	  if (to_rtx == orig_to_rtx)
	    to_rtx = copy_rtx (to_rtx);
	  MEM_KEEP_ALIAS_SET_P (to_rtx) = 1;
	}

      result = store_field (to_rtx, bitsize, bitpos, mode1, from,
			    (want_value
			     /* Spurious cast for HPUX compiler.  */
			     ? ((enum machine_mode)
				TYPE_MODE (TREE_TYPE (to)))
			     : VOIDmode),
			    unsignedp, TREE_TYPE (tem), get_alias_set (to));

      preserve_temp_slots (result);
      free_temp_slots ();
      pop_temp_slots ();

      /* If the value is meaningful, convert RESULT to the proper mode.
	 Otherwise, return nothing.  */
      return (want_value ? convert_modes (TYPE_MODE (TREE_TYPE (to)),
					  TYPE_MODE (TREE_TYPE (from)),
					  result,
					  TREE_UNSIGNED (TREE_TYPE (to)))
	      : NULL_RTX);
    }

  /* If the rhs is a function call and its value is not an aggregate,
     call the function before we start to compute the lhs.
     This is needed for correct code for cases such as
     val = setjmp (buf) on machines where reference to val
     requires loading up part of an address in a separate insn.

     Don't do this if TO is a VAR_DECL or PARM_DECL whose DECL_RTL is REG
     since it might be a promoted variable where the zero- or sign- extension
     needs to be done.  Handling this in the normal way is safe because no
     computation is done before the call.  */
  if (TREE_CODE (from) == CALL_EXPR && ! aggregate_value_p (from)
      && TREE_CODE (TYPE_SIZE (TREE_TYPE (from))) == INTEGER_CST
      && ! ((TREE_CODE (to) == VAR_DECL || TREE_CODE (to) == PARM_DECL)
	    && GET_CODE (DECL_RTL (to)) == REG))
    {
      rtx value;

      push_temp_slots ();
      value = expand_expr (from, NULL_RTX, VOIDmode, 0);
      if (to_rtx == 0)
	to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_WRITE);

      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      if (GET_CODE (to_rtx) == PARALLEL)
	emit_group_load (to_rtx, value, int_size_in_bytes (TREE_TYPE (from)));
      else if (GET_MODE (to_rtx) == BLKmode)
	emit_block_move (to_rtx, value, expr_size (from));
      else
	{
#ifdef POINTERS_EXTEND_UNSIGNED
	  if (POINTER_TYPE_P (TREE_TYPE (to))
	      && GET_MODE (to_rtx) != GET_MODE (value))
	    value = convert_memory_address (GET_MODE (to_rtx), value);
#endif
	  emit_move_insn (to_rtx, value);
	}
      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : NULL_RTX;
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_WRITE);

  /* Don't move directly into a return register.  */
  if (TREE_CODE (to) == RESULT_DECL
      && (GET_CODE (to_rtx) == REG || GET_CODE (to_rtx) == PARALLEL))
    {
      rtx temp;

      push_temp_slots ();
      temp = expand_expr (from, 0, GET_MODE (to_rtx), 0);

      if (GET_CODE (to_rtx) == PARALLEL)
	emit_group_load (to_rtx, temp, int_size_in_bytes (TREE_TYPE (from)));
      else
	emit_move_insn (to_rtx, temp);

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : NULL_RTX;
    }

  /* In case we are returning the contents of an object which overlaps
     the place the value is being stored, use a safe function when copying
     a value through a pointer into a structure value return block.  */
  if (TREE_CODE (to) == RESULT_DECL && TREE_CODE (from) == INDIRECT_REF
      && current_function_returns_struct
      && !current_function_returns_pcc_struct)
    {
      rtx from_rtx, size;

      push_temp_slots ();
      size = expr_size (from);
      from_rtx = expand_expr (from, NULL_RTX, VOIDmode, 0);

#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (memmove_libfunc, LCT_NORMAL,
			 VOIDmode, 3, XEXP (to_rtx, 0), Pmode,
			 XEXP (from_rtx, 0), Pmode,
			 convert_to_mode (TYPE_MODE (sizetype),
					  size, TREE_UNSIGNED (sizetype)),
			 TYPE_MODE (sizetype));
#else
      emit_library_call (bcopy_libfunc, LCT_NORMAL,
			 VOIDmode, 3, XEXP (from_rtx, 0), Pmode,
			 XEXP (to_rtx, 0), Pmode,
			 convert_to_mode (TYPE_MODE (integer_type_node),
					  size, TREE_UNSIGNED (integer_type_node)),
			 TYPE_MODE (integer_type_node));
#endif

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : NULL_RTX;
    }

  /* Compute FROM and store the value in the rtx we got.  */

  push_temp_slots ();
  result = store_expr (from, to_rtx, want_value);
  preserve_temp_slots (result);
  free_temp_slots ();
  pop_temp_slots ();
  return want_value ? result : NULL_RTX;
}

/* Generate code for computing expression EXP,
   and storing the value into TARGET.
   TARGET may contain a QUEUED rtx.

   If WANT_VALUE is nonzero, return a copy of the value
   not in TARGET, so that we can be sure to use the proper
   value in a containing expression even if TARGET has something
   else stored in it.  If possible, we copy the value through a pseudo
   and return that pseudo.  Or, if the value is constant, we try to
   return the constant.  In some cases, we return a pseudo
   copied *from* TARGET.

   If the mode is BLKmode then we may return TARGET itself.
   It turns out that in BLKmode it doesn't cause a problem.
   because C has no operators that could combine two different
   assignments into the same BLKmode object with different values
   with no sequence point.  Will other languages need this to
   be more thorough?

   If WANT_VALUE is 0, we return NULL, to make sure
   to catch quickly any cases where the caller uses the value
   and fails to set WANT_VALUE.  */

rtx
store_expr (exp, target, want_value)
     tree exp;
     rtx target;
     int want_value;
{
  rtx temp;
  int dont_return_target = 0;
  int dont_store_target = 0;

  if (TREE_CODE (exp) == COMPOUND_EXPR)
    {
      /* Perform first part of compound expression, then assign from second
	 part.  */
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      return store_expr (TREE_OPERAND (exp, 1), target, want_value);
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

      do_pending_stack_adjust ();
      NO_DEFER_POP;
      jumpifnot (TREE_OPERAND (exp, 0), lab1);
      start_cleanup_deferral ();
      store_expr (TREE_OPERAND (exp, 1), target, 0);
      end_cleanup_deferral ();
      emit_queue ();
      emit_jump_insn (gen_jump (lab2));
      emit_barrier ();
      emit_label (lab1);
      start_cleanup_deferral ();
      store_expr (TREE_OPERAND (exp, 2), target, 0);
      end_cleanup_deferral ();
      emit_queue ();
      emit_label (lab2);
      OK_DEFER_POP;

      return want_value ? target : NULL_RTX;
    }
  else if (queued_subexp_p (target))
    /* If target contains a postincrement, let's not risk
       using it as the place to generate the rhs.  */
    {
      if (GET_MODE (target) != BLKmode && GET_MODE (target) != VOIDmode)
	{
	  /* Expand EXP into a new pseudo.  */
	  temp = gen_reg_rtx (GET_MODE (target));
	  temp = expand_expr (exp, temp, GET_MODE (target), 0);
	}
      else
	temp = expand_expr (exp, NULL_RTX, GET_MODE (target), 0);

      /* If target is volatile, ANSI requires accessing the value
	 *from* the target, if it is accessed.  So make that happen.
	 In no case return the target itself.  */
      if (! MEM_VOLATILE_P (target) && want_value)
	dont_return_target = 1;
    }
  else if (want_value && GET_CODE (target) == MEM && ! MEM_VOLATILE_P (target)
	   && GET_MODE (target) != BLKmode)
    /* If target is in memory and caller wants value in a register instead,
       arrange that.  Pass TARGET as target for expand_expr so that,
       if EXP is another assignment, WANT_VALUE will be nonzero for it.
       We know expand_expr will not use the target in that case.
       Don't do this if TARGET is volatile because we are supposed
       to write it and then read it.  */
    {
      temp = expand_expr (exp, target, GET_MODE (target), 0);
      if (GET_MODE (temp) != BLKmode && GET_MODE (temp) != VOIDmode)
	{
	  /* If TEMP is already in the desired TARGET, only copy it from
	     memory and don't store it there again.  */
	  if (temp == target
	      || (rtx_equal_p (temp, target)
		  && ! side_effects_p (temp) && ! side_effects_p (target)))
	    dont_store_target = 1;
	  temp = copy_to_reg (temp);
	}
      dont_return_target = 1;
    }
  else if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is an scalar in a register that is stored in a wider mode
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    {
      rtx inner_target = 0;

      /* If we don't want a value, we can do the conversion inside EXP,
	 which will often result in some optimizations.  Do the conversion
	 in two steps: first change the signedness, if needed, then
	 the extend.  But don't do this if the type of EXP is a subtype
	 of something else since then the conversion might involve
	 more than just converting modes.  */
      if (! want_value && INTEGRAL_TYPE_P (TREE_TYPE (exp))
	  && TREE_TYPE (TREE_TYPE (exp)) == 0)
	{
	  if (TREE_UNSIGNED (TREE_TYPE (exp))
	      != SUBREG_PROMOTED_UNSIGNED_P (target))
	    exp
	      = convert
		(signed_or_unsigned_type (SUBREG_PROMOTED_UNSIGNED_P (target),
					  TREE_TYPE (exp)),
		 exp);

	  exp = convert (type_for_mode (GET_MODE (SUBREG_REG (target)),
					SUBREG_PROMOTED_UNSIGNED_P (target)),
			 exp);

	  inner_target = SUBREG_REG (target);
	}

      temp = expand_expr (exp, inner_target, VOIDmode, 0);

      /* If TEMP is a volatile MEM and we want a result value, make
	 the access now so it gets done only once.  Likewise if
	 it contains TARGET.  */
      if (GET_CODE (temp) == MEM && want_value
	  && (MEM_VOLATILE_P (temp)
	      || reg_mentioned_p (SUBREG_REG (target), XEXP (temp, 0))))
	temp = copy_to_reg (temp);

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

      /* If we promoted a constant, change the mode back down to match
	 target.  Otherwise, the caller might get confused by a result whose
	 mode is larger than expected.  */

      if (want_value && GET_MODE (temp) != GET_MODE (target))
	{
	  if (GET_MODE (temp) != VOIDmode)
	    {
	      temp = gen_lowpart_SUBREG (GET_MODE (target), temp);
	      SUBREG_PROMOTED_VAR_P (temp) = 1;
	      SUBREG_PROMOTED_UNSIGNED_P (temp)
		= SUBREG_PROMOTED_UNSIGNED_P (target);
	    }
	  else
	    temp = convert_modes (GET_MODE (target),
				  GET_MODE (SUBREG_REG (target)),
				  temp, SUBREG_PROMOTED_UNSIGNED_P (target));
	}

      return want_value ? temp : NULL_RTX;
    }
  else
    {
      temp = expand_expr (exp, target, GET_MODE (target), 0);
      /* Return TARGET if it's a specified hardware register.
	 If TARGET is a volatile mem ref, either return TARGET
	 or return a reg copied *from* TARGET; ANSI requires this.

	 Otherwise, if TEMP is not TARGET, return TEMP
	 if it is constant (for efficiency),
	 or if we really want the correct value.  */
      if (!(target && GET_CODE (target) == REG
	    && REGNO (target) < FIRST_PSEUDO_REGISTER)
	  && !(GET_CODE (target) == MEM && MEM_VOLATILE_P (target))
	  && ! rtx_equal_p (temp, target)
	  && (CONSTANT_P (temp) || want_value))
	dont_return_target = 1;
    }

  /* If TEMP is a VOIDmode constant and the mode of the type of EXP is not
     the same as that of TARGET, adjust the constant.  This is needed, for
     example, in case it is a CONST_DOUBLE and we want only a word-sized
     value.  */
  if (CONSTANT_P (temp) && GET_MODE (temp) == VOIDmode
      && TREE_CODE (exp) != ERROR_MARK
      && GET_MODE (target) != TYPE_MODE (TREE_TYPE (exp)))
    temp = convert_modes (GET_MODE (target), TYPE_MODE (TREE_TYPE (exp)),
			  temp, TREE_UNSIGNED (TREE_TYPE (exp)));

  /* If value was not generated in the target, store it there.
     Convert the value to TARGET's type first if necessary.
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
      && ! dont_store_target
	 /* If store_expr stores a DECL whose DECL_RTL(exp) == TARGET,
	    but TARGET is not valid memory reference, TEMP will differ
	    from TARGET although it is really the same location.  */
      && (TREE_CODE_CLASS (TREE_CODE (exp)) != 'd'
	  || target != DECL_RTL_IF_SET (exp)))
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
	  /* Handle copying a string constant into an array.  The string
	     constant may be shorter than the array.  So copy just the string's
	     actual length, and clear the rest.  First get the size of the data
	     type of the string, which is actually the size of the target.  */
	  rtx size = expr_size (exp);

	  if (GET_CODE (size) == CONST_INT
	      && INTVAL (size) < TREE_STRING_LENGTH (exp))
	    emit_block_move (target, temp, size);
	  else
	    {
	      /* Compute the size of the data to copy from the string.  */
	      tree copy_size
		= size_binop (MIN_EXPR,
			      make_tree (sizetype, size),
			      size_int (TREE_STRING_LENGTH (exp)));
	      rtx copy_size_rtx = expand_expr (copy_size, NULL_RTX,
					       VOIDmode, 0);
	      rtx label = 0;

	      /* Copy that much.  */
	      copy_size_rtx = convert_to_mode (ptr_mode, copy_size_rtx, 0);
	      emit_block_move (target, temp, copy_size_rtx);

	      /* Figure out how much is left in TARGET that we have to clear.
		 Do all calculations in ptr_mode.  */
	      if (GET_CODE (copy_size_rtx) == CONST_INT)
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

#ifdef POINTERS_EXTEND_UNSIGNED
		  if (GET_MODE (copy_size_rtx) != Pmode)
		    copy_size_rtx = convert_memory_address (Pmode,
							    copy_size_rtx);
#endif

		  target = offset_address (target, copy_size_rtx,
					   highest_pow2_factor (copy_size));
		  label = gen_label_rtx ();
		  emit_cmp_and_jump_insns (size, const0_rtx, LT, NULL_RTX,
					   GET_MODE (size), 0, label);
		}

	      if (size != const0_rtx)
		clear_storage (target, size);

	      if (label)
		emit_label (label);
	    }
	}
      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      else if (GET_CODE (target) == PARALLEL)
	emit_group_load (target, temp, int_size_in_bytes (TREE_TYPE (exp)));
      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp));
      else
	emit_move_insn (target, temp);
    }

  /* If we don't want a value, return NULL_RTX.  */
  if (! want_value)
    return NULL_RTX;

  /* If we are supposed to return TEMP, do so as long as it isn't a MEM.
     ??? The latter test doesn't seem to make sense.  */
  else if (dont_return_target && GET_CODE (temp) != MEM)
    return temp;

  /* Return TARGET itself if it is a hard register.  */
  else if (want_value && GET_MODE (target) != BLKmode
	   && ! (GET_CODE (target) == REG
		 && REGNO (target) < FIRST_PSEUDO_REGISTER))
    return copy_to_reg (target);

  else
    return target;
}

/* Return 1 if EXP just contains zeros.  */

static int
is_zeros_p (exp)
     tree exp;
{
  tree elt;

  switch (TREE_CODE (exp))
    {
    case CONVERT_EXPR:
    case NOP_EXPR:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      return is_zeros_p (TREE_OPERAND (exp, 0));

    case INTEGER_CST:
      return integer_zerop (exp);

    case COMPLEX_CST:
      return
	is_zeros_p (TREE_REALPART (exp)) && is_zeros_p (TREE_IMAGPART (exp));

    case REAL_CST:
      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (exp), dconst0);

    case VECTOR_CST:
      for (elt = TREE_VECTOR_CST_ELTS (exp); elt;
	   elt = TREE_CHAIN (elt))
	if (!is_zeros_p (TREE_VALUE (elt)))
	  return 0;

      return 1;

    case CONSTRUCTOR:
      if (TREE_TYPE (exp) && TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	return CONSTRUCTOR_ELTS (exp) == NULL_TREE;
      for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	if (! is_zeros_p (TREE_VALUE (elt)))
	  return 0;

      return 1;

    default:
      return 0;
    }
}

/* Return 1 if EXP contains mostly (3/4)  zeros.  */

static int
mostly_zeros_p (exp)
     tree exp;
{
  if (TREE_CODE (exp) == CONSTRUCTOR)
    {
      int elts = 0, zeros = 0;
      tree elt = CONSTRUCTOR_ELTS (exp);
      if (TREE_TYPE (exp) && TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	{
	  /* If there are no ranges of true bits, it is all zero.  */
	  return elt == NULL_TREE;
	}
      for (; elt; elt = TREE_CHAIN (elt))
	{
	  /* We do not handle the case where the index is a RANGE_EXPR,
	     so the statistic will be somewhat inaccurate.
	     We do make a more accurate count in store_constructor itself,
	     so since this function is only used for nested array elements,
	     this should be close enough.  */
	  if (mostly_zeros_p (TREE_VALUE (elt)))
	    zeros++;
	  elts++;
	}

      return 4 * zeros >= 3 * elts;
    }

  return is_zeros_p (exp);
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
store_constructor_field (target, bitsize, bitpos, mode, exp, type, cleared,
			 alias_set)
     rtx target;
     unsigned HOST_WIDE_INT bitsize;
     HOST_WIDE_INT bitpos;
     enum machine_mode mode;
     tree exp, type;
     int cleared;
     int alias_set;
{
  if (TREE_CODE (exp) == CONSTRUCTOR
      && bitpos % BITS_PER_UNIT == 0
      /* If we have a non-zero bitpos for a register target, then we just
	 let store_field do the bitfield handling.  This is unlikely to
	 generate unnecessary clear instructions anyways.  */
      && (bitpos == 0 || GET_CODE (target) == MEM))
    {
      if (GET_CODE (target) == MEM)
	target
	  = adjust_address (target,
			    GET_MODE (target) == BLKmode
			    || 0 != (bitpos
				     % GET_MODE_ALIGNMENT (GET_MODE (target)))
			    ? BLKmode : VOIDmode, bitpos / BITS_PER_UNIT);


      /* Update the alias set, if required.  */
      if (GET_CODE (target) == MEM && ! MEM_KEEP_ALIAS_SET_P (target)
	  && MEM_ALIAS_SET (target) != 0)
	{
	  target = copy_rtx (target);
	  set_mem_alias_set (target, alias_set);
	}

      store_constructor (exp, target, cleared, bitsize / BITS_PER_UNIT);
    }
  else
    store_field (target, bitsize, bitpos, mode, exp, VOIDmode, 0, type,
		 alias_set);
}

/* Store the value of constructor EXP into the rtx TARGET.
   TARGET is either a REG or a MEM; we know it cannot conflict, since
   safe_from_p has been called.
   CLEARED is true if TARGET is known to have been zero'd.
   SIZE is the number of bytes of TARGET we are allowed to modify: this
   may not be the same as the size of EXP if we are assigning to a field
   which has been packed to exclude padding bits.  */

static void
store_constructor (exp, target, cleared, size)
     tree exp;
     rtx target;
     int cleared;
     HOST_WIDE_INT size;
{
  tree type = TREE_TYPE (exp);
#ifdef WORD_REGISTER_OPERATIONS
  HOST_WIDE_INT exp_size = int_size_in_bytes (type);
#endif

  if (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
      || TREE_CODE (type) == QUAL_UNION_TYPE)
    {
      tree elt;

      /* We either clear the aggregate or indicate the value is dead.  */
      if ((TREE_CODE (type) == UNION_TYPE
	   || TREE_CODE (type) == QUAL_UNION_TYPE)
	  && ! cleared
	  && ! CONSTRUCTOR_ELTS (exp))
	/* If the constructor is empty, clear the union.  */
	{
	  clear_storage (target, expr_size (exp));
	  cleared = 1;
	}

      /* If we are building a static constructor into a register,
	 set the initial value as zero so we can fold the value into
	 a constant.  But if more than one register is involved,
	 this probably loses.  */
      else if (! cleared && GET_CODE (target) == REG && TREE_STATIC (exp)
	       && GET_MODE_SIZE (GET_MODE (target)) <= UNITS_PER_WORD)
	{
	  emit_move_insn (target, CONST0_RTX (GET_MODE (target)));
	  cleared = 1;
	}

      /* If the constructor has fewer fields than the structure
	 or if we are initializing the structure to mostly zeros,
	 clear the whole structure first.  Don't do this if TARGET is a
	 register whose mode size isn't equal to SIZE since clear_storage
	 can't handle this case.  */
      else if (! cleared && size > 0
	       && ((list_length (CONSTRUCTOR_ELTS (exp))
		    != fields_length (type))
		   || mostly_zeros_p (exp))
	       && (GET_CODE (target) != REG
		   || ((HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (target))
		       == size)))
	{
	  clear_storage (target, GEN_INT (size));
	  cleared = 1;
	}

      if (! cleared)
	emit_insn (gen_rtx_CLOBBER (VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding field of TARGET.  */

      for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	{
	  tree field = TREE_PURPOSE (elt);
	  tree value = TREE_VALUE (elt);
	  enum machine_mode mode;
	  HOST_WIDE_INT bitsize;
	  HOST_WIDE_INT bitpos = 0;
	  int unsignedp;
	  tree offset;
	  rtx to_rtx = target;

	  /* Just ignore missing fields.
	     We cleared the whole structure, above,
	     if any fields are missing.  */
	  if (field == 0)
	    continue;

	  if (cleared && is_zeros_p (value))
	    continue;

	  if (host_integerp (DECL_SIZE (field), 1))
	    bitsize = tree_low_cst (DECL_SIZE (field), 1);
	  else
	    bitsize = -1;

	  unsignedp = TREE_UNSIGNED (field);
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
	      rtx offset_rtx;

	      if (contains_placeholder_p (offset))
		offset = build (WITH_RECORD_EXPR, sizetype,
				offset, make_tree (TREE_TYPE (exp), target));

	      offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, 0);
	      if (GET_CODE (to_rtx) != MEM)
		abort ();

#ifdef POINTERS_EXTEND_UNSIGNED
	      if (GET_MODE (offset_rtx) != Pmode)
		offset_rtx = convert_memory_address (Pmode, offset_rtx);
#else
	      if (GET_MODE (offset_rtx) != ptr_mode)
		offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);
#endif

	      to_rtx = offset_address (to_rtx, offset_rtx,
				       highest_pow2_factor (offset));
	    }

	  if (TREE_READONLY (field))
	    {
	      if (GET_CODE (to_rtx) == MEM)
		to_rtx = copy_rtx (to_rtx);

	      RTX_UNCHANGING_P (to_rtx) = 1;
	    }

#ifdef WORD_REGISTER_OPERATIONS
	  /* If this initializes a field that is smaller than a word, at the
	     start of a word, try to widen it to a full word.
	     This special case allows us to output C++ member function
	     initializations in a form that the optimizers can understand.  */
	  if (GET_CODE (target) == REG
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
		  type = type_for_size (BITS_PER_WORD, TREE_UNSIGNED (type));
		  value = convert (type, value);
		}

	      if (BYTES_BIG_ENDIAN)
		value
		  = fold (build (LSHIFT_EXPR, type, value,
				 build_int_2 (BITS_PER_WORD - bitsize, 0)));
	      bitsize = BITS_PER_WORD;
	      mode = word_mode;
	    }
#endif

	  if (GET_CODE (to_rtx) == MEM && !MEM_KEEP_ALIAS_SET_P (to_rtx)
	      && DECL_NONADDRESSABLE_P (field))
	    {
	      to_rtx = copy_rtx (to_rtx);
	      MEM_KEEP_ALIAS_SET_P (to_rtx) = 1;
	    }

	  store_constructor_field (to_rtx, bitsize, bitpos, mode,
				   value, type, cleared,
				   get_alias_set (TREE_TYPE (field)));
	}
    }
  else if (TREE_CODE (type) == ARRAY_TYPE
	   || TREE_CODE (type) == VECTOR_TYPE)
    {
      tree elt;
      int i;
      int need_to_clear;
      tree domain = TYPE_DOMAIN (type);
      tree elttype = TREE_TYPE (type);
      int const_bounds_p;
      HOST_WIDE_INT minelt = 0;
      HOST_WIDE_INT maxelt = 0;

      /* Vectors are like arrays, but the domain is stored via an array
	 type indirectly.  */
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  /* Note that although TYPE_DEBUG_REPRESENTATION_TYPE uses
	     the same field as TYPE_DOMAIN, we are not guaranteed that
	     it always will.  */
	  domain = TYPE_DEBUG_REPRESENTATION_TYPE (type);
	  domain = TYPE_DOMAIN (TREE_TYPE (TYPE_FIELDS (domain)));
	}

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

      /* If the constructor has fewer elements than the array,
         clear the whole array first.  Similarly if this is
         static constructor of a non-BLKmode object.  */
      if (cleared || (GET_CODE (target) == REG && TREE_STATIC (exp)))
	need_to_clear = 1;
      else
	{
	  HOST_WIDE_INT count = 0, zero_count = 0;
	  need_to_clear = ! const_bounds_p;

	  /* This loop is a more accurate version of the loop in
	     mostly_zeros_p (it handles RANGE_EXPR in an index).
	     It is also needed to check for missing elements.  */
	  for (elt = CONSTRUCTOR_ELTS (exp);
	       elt != NULL_TREE && ! need_to_clear;
	       elt = TREE_CHAIN (elt))
	    {
	      tree index = TREE_PURPOSE (elt);
	      HOST_WIDE_INT this_node_count;

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
	      if (mostly_zeros_p (TREE_VALUE (elt)))
		zero_count += this_node_count;
	    }

	  /* Clear the entire array first if there are any missing elements,
	     or if the incidence of zero elements is >= 75%.  */
	  if (! need_to_clear
	      && (count < maxelt - minelt + 1 || 4 * zero_count >= 3 * count))
	    need_to_clear = 1;
	}

      if (need_to_clear && size > 0)
	{
	  if (! cleared)
	    {
	      if (REG_P (target))
		emit_move_insn (target,  CONST0_RTX (GET_MODE (target)));
	      else
		clear_storage (target, GEN_INT (size));
	    }
	  cleared = 1;
	}
      else if (REG_P (target))
	/* Inform later passes that the old value is dead.  */
	emit_insn (gen_rtx_CLOBBER (VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding element of TARGET, determined
	 by counting the elements.  */
      for (elt = CONSTRUCTOR_ELTS (exp), i = 0;
	   elt;
	   elt = TREE_CHAIN (elt), i++)
	{
	  enum machine_mode mode;
	  HOST_WIDE_INT bitsize;
	  HOST_WIDE_INT bitpos;
	  int unsignedp;
	  tree value = TREE_VALUE (elt);
	  tree index = TREE_PURPOSE (elt);
	  rtx xtarget = target;

	  if (cleared && is_zeros_p (value))
	    continue;

	  unsignedp = TREE_UNSIGNED (elttype);
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
	      rtx index_r, pos_rtx, hi_r, loop_top, loop_end;
	      struct nesting *loop;
	      HOST_WIDE_INT lo, hi, count;
	      tree position;

	      /* If the range is constant and "small", unroll the loop.  */
	      if (const_bounds_p
		  && host_integerp (lo_index, 0)
		  && host_integerp (hi_index, 0)
		  && (lo = tree_low_cst (lo_index, 0),
		      hi = tree_low_cst (hi_index, 0),
		      count = hi - lo + 1,
		      (GET_CODE (target) != MEM
		       || count <= 2
		       || (host_integerp (TYPE_SIZE (elttype), 1)
			   && (tree_low_cst (TYPE_SIZE (elttype), 1) * count
			       <= 40 * 8)))))
		{
		  lo -= minelt;  hi -= minelt;
		  for (; lo <= hi; lo++)
		    {
		      bitpos = lo * tree_low_cst (TYPE_SIZE (elttype), 0);

		      if (GET_CODE (target) == MEM
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
		  hi_r = expand_expr (hi_index, NULL_RTX, VOIDmode, 0);
		  loop_top = gen_label_rtx ();
		  loop_end = gen_label_rtx ();

		  unsignedp = TREE_UNSIGNED (domain);

		  index = build_decl (VAR_DECL, NULL_TREE, domain);

		  index_r
		    = gen_reg_rtx (promote_mode (domain, DECL_MODE (index),
						 &unsignedp, 0));
		  SET_DECL_RTL (index, index_r);
		  if (TREE_CODE (value) == SAVE_EXPR
		      && SAVE_EXPR_RTL (value) == 0)
		    {
		      /* Make sure value gets expanded once before the
                         loop.  */
		      expand_expr (value, const0_rtx, VOIDmode, 0);
		      emit_queue ();
		    }
		  store_expr (lo_index, index_r, 0);
		  loop = expand_start_loop (0);

		  /* Assign value to element index.  */
		  position
		    = convert (ssizetype,
			       fold (build (MINUS_EXPR, TREE_TYPE (index),
					    index, TYPE_MIN_VALUE (domain))));
		  position = size_binop (MULT_EXPR, position,
					 convert (ssizetype,
						  TYPE_SIZE_UNIT (elttype)));

		  pos_rtx = expand_expr (position, 0, VOIDmode, 0);
		  xtarget = offset_address (target, pos_rtx,
					    highest_pow2_factor (position));
		  xtarget = adjust_address (xtarget, mode, 0);
		  if (TREE_CODE (value) == CONSTRUCTOR)
		    store_constructor (value, xtarget, cleared,
				       bitsize / BITS_PER_UNIT);
		  else
		    store_expr (value, xtarget, 0);

		  expand_exit_loop_if_false (loop,
					     build (LT_EXPR, integer_type_node,
						    index, hi_index));

		  expand_increment (build (PREINCREMENT_EXPR,
					   TREE_TYPE (index),
					   index, integer_one_node), 0, 0);
		  expand_end_loop ();
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
		index = convert (ssizetype,
				 fold (build (MINUS_EXPR, index,
					      TYPE_MIN_VALUE (domain))));

	      position = size_binop (MULT_EXPR, index,
				     convert (ssizetype,
					      TYPE_SIZE_UNIT (elttype)));
	      xtarget = offset_address (target,
					expand_expr (position, 0, VOIDmode, 0),
					highest_pow2_factor (position));
	      xtarget = adjust_address (xtarget, mode, 0);
	      store_expr (value, xtarget, 0);
	    }
	  else
	    {
	      if (index != 0)
		bitpos = ((tree_low_cst (index, 0) - minelt)
			  * tree_low_cst (TYPE_SIZE (elttype), 1));
	      else
		bitpos = (i * tree_low_cst (TYPE_SIZE (elttype), 1));

	      if (GET_CODE (target) == MEM && !MEM_KEEP_ALIAS_SET_P (target)
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
    }

  /* Set constructor assignments.  */
  else if (TREE_CODE (type) == SET_TYPE)
    {
      tree elt = CONSTRUCTOR_ELTS (exp);
      unsigned HOST_WIDE_INT nbytes = int_size_in_bytes (type), nbits;
      tree domain = TYPE_DOMAIN (type);
      tree domain_min, domain_max, bitlength;

      /* The default implementation strategy is to extract the constant
	 parts of the constructor, use that to initialize the target,
	 and then "or" in whatever non-constant ranges we need in addition.

	 If a large set is all zero or all ones, it is
	 probably better to set it using memset (if available) or bzero.
	 Also, if a large set has just a single range, it may also be
	 better to first clear all the first clear the set (using
	 bzero/memset), and set the bits we want.  */

      /* Check for all zeros.  */
      if (elt == NULL_TREE && size > 0)
	{
	  if (!cleared)
	    clear_storage (target, GEN_INT (size));
	  return;
	}

      domain_min = convert (sizetype, TYPE_MIN_VALUE (domain));
      domain_max = convert (sizetype, TYPE_MAX_VALUE (domain));
      bitlength = size_binop (PLUS_EXPR,
			      size_diffop (domain_max, domain_min),
			      ssize_int (1));

      nbits = tree_low_cst (bitlength, 1);

      /* For "small" sets, or "medium-sized" (up to 32 bytes) sets that
	 are "complicated" (more than one range), initialize (the
	 constant parts) by copying from a constant.  */
      if (GET_MODE (target) != BLKmode || nbits <= 2 * BITS_PER_WORD
	  || (nbytes <= 32 && TREE_CHAIN (elt) != NULL_TREE))
	{
	  unsigned int set_word_size = TYPE_ALIGN (TREE_TYPE (exp));
	  enum machine_mode mode = mode_for_size (set_word_size, MODE_INT, 1);
	  char *bit_buffer = (char *) alloca (nbits);
	  HOST_WIDE_INT word = 0;
	  unsigned int bit_pos = 0;
	  unsigned int ibit = 0;
	  unsigned int offset = 0;  /* In bytes from beginning of set.  */

	  elt = get_set_constructor_bits (exp, bit_buffer, nbits);
	  for (;;)
	    {
	      if (bit_buffer[ibit])
		{
		  if (BYTES_BIG_ENDIAN)
		    word |= (1 << (set_word_size - 1 - bit_pos));
		  else
		    word |= 1 << bit_pos;
		}

	      bit_pos++;  ibit++;
	      if (bit_pos >= set_word_size || ibit == nbits)
		{
		  if (word != 0 || ! cleared)
		    {
		      rtx datum = GEN_INT (word);
		      rtx to_rtx;

		      /* The assumption here is that it is safe to use
			 XEXP if the set is multi-word, but not if
			 it's single-word.  */
		      if (GET_CODE (target) == MEM)
			to_rtx = adjust_address (target, mode, offset);
		      else if (offset == 0)
			to_rtx = target;
		      else
			abort ();
		      emit_move_insn (to_rtx, datum);
		    }

		  if (ibit == nbits)
		    break;
		  word = 0;
		  bit_pos = 0;
		  offset += set_word_size / BITS_PER_UNIT;
		}
	    }
	}
      else if (!cleared)
	/* Don't bother clearing storage if the set is all ones.  */
	if (TREE_CHAIN (elt) != NULL_TREE
	    || (TREE_PURPOSE (elt) == NULL_TREE
		? nbits != 1
		: ( ! host_integerp (TREE_VALUE (elt), 0)
		   || ! host_integerp (TREE_PURPOSE (elt), 0)
		   || (tree_low_cst (TREE_VALUE (elt), 0)
		       - tree_low_cst (TREE_PURPOSE (elt), 0) + 1
		       != (HOST_WIDE_INT) nbits))))
	  clear_storage (target, expr_size (exp));

      for (; elt != NULL_TREE; elt = TREE_CHAIN (elt))
	{
	  /* Start of range of element or NULL.  */
	  tree startbit = TREE_PURPOSE (elt);
	  /* End of range of element, or element value.  */
	  tree endbit   = TREE_VALUE (elt);
#ifdef TARGET_MEM_FUNCTIONS
	  HOST_WIDE_INT startb, endb;
#endif
	  rtx bitlength_rtx, startbit_rtx, endbit_rtx, targetx;

	  bitlength_rtx = expand_expr (bitlength,
				       NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	  /* Handle non-range tuple element like [ expr ].  */
	  if (startbit == NULL_TREE)
	    {
	      startbit = save_expr (endbit);
	      endbit = startbit;
	    }

	  startbit = convert (sizetype, startbit);
	  endbit = convert (sizetype, endbit);
	  if (! integer_zerop (domain_min))
	    {
	      startbit = size_binop (MINUS_EXPR, startbit, domain_min);
	      endbit = size_binop (MINUS_EXPR, endbit, domain_min);
	    }
	  startbit_rtx = expand_expr (startbit, NULL_RTX, MEM,
				      EXPAND_CONST_ADDRESS);
	  endbit_rtx = expand_expr (endbit, NULL_RTX, MEM,
				    EXPAND_CONST_ADDRESS);

	  if (REG_P (target))
	    {
	      targetx
		= assign_temp
		  ((build_qualified_type (type_for_mode (GET_MODE (target), 0),
					  TYPE_QUAL_CONST)),
		   0, 1, 1);
	      emit_move_insn (targetx, target);
	    }

	  else if (GET_CODE (target) == MEM)
	    targetx = target;
	  else
	    abort ();

#ifdef TARGET_MEM_FUNCTIONS
	  /* Optimization:  If startbit and endbit are
	     constants divisible by BITS_PER_UNIT,
	     call memset instead.  */
	  if (TREE_CODE (startbit) == INTEGER_CST
	      && TREE_CODE (endbit) == INTEGER_CST
	      && (startb = TREE_INT_CST_LOW (startbit)) % BITS_PER_UNIT == 0
	      && (endb = TREE_INT_CST_LOW (endbit) + 1) % BITS_PER_UNIT == 0)
	    {
	      emit_library_call (memset_libfunc, LCT_NORMAL,
				 VOIDmode, 3,
				 plus_constant (XEXP (targetx, 0),
						startb / BITS_PER_UNIT),
				 Pmode,
				 constm1_rtx, TYPE_MODE (integer_type_node),
				 GEN_INT ((endb - startb) / BITS_PER_UNIT),
				 TYPE_MODE (sizetype));
	    }
	  else
#endif
	    emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__setbits"),
			       LCT_NORMAL, VOIDmode, 4, XEXP (targetx, 0),
			       Pmode, bitlength_rtx, TYPE_MODE (sizetype),
			       startbit_rtx, TYPE_MODE (sizetype),
			       endbit_rtx, TYPE_MODE (sizetype));

	  if (REG_P (target))
	    emit_move_insn (target, targetx);
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

   TYPE is the type of the underlying object,

   ALIAS_SET is the alias set for the destination.  This value will
   (in general) be different from that for TARGET, since TARGET is a
   reference to the containing structure.  */

static rtx
store_field (target, bitsize, bitpos, mode, exp, value_mode, unsignedp, type,
	     alias_set)
     rtx target;
     HOST_WIDE_INT bitsize;
     HOST_WIDE_INT bitpos;
     enum machine_mode mode;
     tree exp;
     enum machine_mode value_mode;
     int unsignedp;
     tree type;
     int alias_set;
{
  HOST_WIDE_INT width_mask = 0;

  if (TREE_CODE (exp) == ERROR_MARK)
    return const0_rtx;

  /* If we have nothing to store, do nothing unless the expression has
     side-effects.  */
  if (bitsize == 0)
    return expand_expr (exp, const0_rtx, VOIDmode, 0);
  else if (bitsize >=0 && bitsize < HOST_BITS_PER_WIDE_INT)
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
      rtx object
	= assign_temp
	  (build_qualified_type (type, TYPE_QUALS (type) | TYPE_QUAL_CONST),
	   0, 1, 1);
      rtx blk_object = adjust_address (object, BLKmode, 0);

      if (bitsize != (HOST_WIDE_INT) GET_MODE_BITSIZE (GET_MODE (target)))
	emit_move_insn (object, target);

      store_field (blk_object, bitsize, bitpos, mode, exp, VOIDmode, 0, type,
		   alias_set);

      emit_move_insn (target, object);

      /* We want to return the BLKmode version of the data.  */
      return blk_object;
    }

  if (GET_CODE (target) == CONCAT)
    {
      /* We're storing into a struct containing a single __complex.  */

      if (bitpos != 0)
	abort ();
      return store_expr (exp, target, 0);
    }

  /* If the structure is in a register or if the component
     is a bit field, we cannot use addressing to access it.
     Use bit-field techniques or SUBREG to store in it.  */

  if (mode == VOIDmode
      || (mode != BLKmode && ! direct_store[(int) mode]
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
      || GET_CODE (target) == REG
      || GET_CODE (target) == SUBREG
      /* If the field isn't aligned enough to store as an ordinary memref,
	 store it as a bit field.  */
      || (mode != BLKmode && SLOW_UNALIGNED_ACCESS (mode, MEM_ALIGN (target))
	  && (MEM_ALIGN (target) < GET_MODE_ALIGNMENT (mode)
	      || bitpos % GET_MODE_ALIGNMENT (mode)))
      /* If the RHS and field are a constant size and the size of the
	 RHS isn't the same size as the bitfield, we must use bitfield
	 operations.  */
      || (bitsize >= 0
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST
	  && compare_tree_int (TYPE_SIZE (TREE_TYPE (exp)), bitsize) != 0))
    {
      rtx temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);

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
			     temp, 1);

      /* Unless MODE is VOIDmode or BLKmode, convert TEMP to
	 MODE.  */
      if (mode != VOIDmode && mode != BLKmode
	  && mode != TYPE_MODE (TREE_TYPE (exp)))
	temp = convert_modes (mode, TYPE_MODE (TREE_TYPE (exp)), temp, 1);

      /* If the modes of TARGET and TEMP are both BLKmode, both
	 must be in memory and BITPOS must be aligned on a byte
	 boundary.  If so, we simply do a block copy.  */
      if (GET_MODE (target) == BLKmode && GET_MODE (temp) == BLKmode)
	{
	  if (GET_CODE (target) != MEM || GET_CODE (temp) != MEM
	      || bitpos % BITS_PER_UNIT != 0)
	    abort ();

	  target = adjust_address (target, VOIDmode, bitpos / BITS_PER_UNIT);
	  emit_block_move (target, temp,
			   GEN_INT ((bitsize + BITS_PER_UNIT - 1)
				    / BITS_PER_UNIT));

	  return value_mode == VOIDmode ? const0_rtx : target;
	}

      /* Store the value in the bitfield.  */
      store_bit_field (target, bitsize, bitpos, mode, temp,
		       int_size_in_bytes (type));

      if (value_mode != VOIDmode)
	{
	  /* The caller wants an rtx for the value.
	     If possible, avoid refetching from the bitfield itself.  */
	  if (width_mask != 0
	      && ! (GET_CODE (target) == MEM && MEM_VOLATILE_P (target)))
	    {
	      tree count;
	      enum machine_mode tmode;

	      tmode = GET_MODE (temp);
	      if (tmode == VOIDmode)
		tmode = value_mode;

	      if (unsignedp)
		return expand_and (tmode, temp,
				   GEN_INT (trunc_int_for_mode (width_mask,
								tmode)),
				   NULL_RTX);

	      count = build_int_2 (GET_MODE_BITSIZE (tmode) - bitsize, 0);
	      temp = expand_shift (LSHIFT_EXPR, tmode, temp, count, 0, 0);
	      return expand_shift (RSHIFT_EXPR, tmode, temp, count, 0, 0);
	    }

	  return extract_bit_field (target, bitsize, bitpos, unsignedp,
				    NULL_RTX, value_mode, VOIDmode,
				    int_size_in_bytes (type));
	}
      return const0_rtx;
    }
  else
    {
      rtx addr = XEXP (target, 0);
      rtx to_rtx = target;

      /* If a value is wanted, it must be the lhs;
	 so make the address stable for multiple use.  */

      if (value_mode != VOIDmode && GET_CODE (addr) != REG
	  && ! CONSTANT_ADDRESS_P (addr)
	  /* A frame-pointer reference is already stable.  */
	  && ! (GET_CODE (addr) == PLUS
		&& GET_CODE (XEXP (addr, 1)) == CONST_INT
		&& (XEXP (addr, 0) == virtual_incoming_args_rtx
		    || XEXP (addr, 0) == virtual_stack_vars_rtx)))
	to_rtx = replace_equiv_address (to_rtx, copy_to_reg (addr));

      /* Now build a reference to just the desired component.  */

      to_rtx = adjust_address (target, mode, bitpos / BITS_PER_UNIT);

      if (to_rtx == target)
	to_rtx = copy_rtx (to_rtx);

      MEM_SET_IN_STRUCT_P (to_rtx, 1);
      if (!MEM_KEEP_ALIAS_SET_P (to_rtx) && MEM_ALIAS_SET (to_rtx) != 0)
	set_mem_alias_set (to_rtx, alias_set);

      return store_expr (exp, to_rtx, value_mode != VOIDmode);
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

   If the field is a bit-field, *PMODE is set to VOIDmode.  Otherwise, it
   is a mode that can be used to access the field.  In that case, *PBITSIZE
   is redundant.

   If the field describes a variable-sized object, *PMODE is set to
   VOIDmode and *PBITSIZE is set to -1.  An access cannot be made in
   this case, but the address of the object can be found.  */

tree
get_inner_reference (exp, pbitsize, pbitpos, poffset, pmode,
		     punsignedp, pvolatilep)
     tree exp;
     HOST_WIDE_INT *pbitsize;
     HOST_WIDE_INT *pbitpos;
     tree *poffset;
     enum machine_mode *pmode;
     int *punsignedp;
     int *pvolatilep;
{
  tree size_tree = 0;
  enum machine_mode mode = VOIDmode;
  tree offset = size_zero_node;
  tree bit_offset = bitsize_zero_node;
  tree placeholder_ptr = 0;
  tree tem;

  /* First get the mode, signedness, and size.  We do this from just the
     outermost expression.  */
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
      *punsignedp = TREE_UNSIGNED (TREE_TYPE (exp));

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
      if (TREE_CODE (exp) == BIT_FIELD_REF)
	bit_offset = size_binop (PLUS_EXPR, bit_offset, TREE_OPERAND (exp, 2));
      else if (TREE_CODE (exp) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (exp, 1);
	  tree this_offset = DECL_FIELD_OFFSET (field);

	  /* If this field hasn't been filled in yet, don't go
	     past it.  This should only happen when folding expressions
	     made during type construction.  */
	  if (this_offset == 0)
	    break;
	  else if (! TREE_CONSTANT (this_offset)
		   && contains_placeholder_p (this_offset))
	    this_offset = build (WITH_RECORD_EXPR, sizetype, this_offset, exp);

	  offset = size_binop (PLUS_EXPR, offset, this_offset);
	  bit_offset = size_binop (PLUS_EXPR, bit_offset,
				   DECL_FIELD_BIT_OFFSET (field));

	  /* ??? Right now we don't do anything with DECL_OFFSET_ALIGN.  */
	}

      else if (TREE_CODE (exp) == ARRAY_REF
	       || TREE_CODE (exp) == ARRAY_RANGE_REF)
	{
	  tree index = TREE_OPERAND (exp, 1);
	  tree array = TREE_OPERAND (exp, 0);
	  tree domain = TYPE_DOMAIN (TREE_TYPE (array));
	  tree low_bound = (domain ? TYPE_MIN_VALUE (domain) : 0);
	  tree unit_size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (array)));

	  /* We assume all arrays have sizes that are a multiple of a byte.
	     First subtract the lower bound, if any, in the type of the
	     index, then convert to sizetype and multiply by the size of the
	     array element.  */
	  if (low_bound != 0 && ! integer_zerop (low_bound))
	    index = fold (build (MINUS_EXPR, TREE_TYPE (index),
				 index, low_bound));

	  /* If the index has a self-referential type, pass it to a
	     WITH_RECORD_EXPR; if the component size is, pass our
	     component to one.  */
	  if (! TREE_CONSTANT (index)
	      && contains_placeholder_p (index))
	    index = build (WITH_RECORD_EXPR, TREE_TYPE (index), index, exp);
	  if (! TREE_CONSTANT (unit_size)
	      && contains_placeholder_p (unit_size))
	    unit_size = build (WITH_RECORD_EXPR, sizetype, unit_size, array);

	  offset = size_binop (PLUS_EXPR, offset,
			       size_binop (MULT_EXPR,
					   convert (sizetype, index),
					   unit_size));
	}

      else if (TREE_CODE (exp) == PLACEHOLDER_EXPR)
	{
	  tree new = find_placeholder (exp, &placeholder_ptr);

	  /* If we couldn't find the replacement, return the PLACEHOLDER_EXPR.
	     We might have been called from tree optimization where we
	     haven't set up an object yet.  */
	  if (new == 0)
	    break;
	  else
	    exp = new;

	  continue;
	}
      else if (TREE_CODE (exp) != NON_LVALUE_EXPR
	       && TREE_CODE (exp) != VIEW_CONVERT_EXPR
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

  /* If OFFSET is constant, see if we can return the whole thing as a
     constant bit position.  Otherwise, split it up.  */
  if (host_integerp (offset, 0)
      && 0 != (tem = size_binop (MULT_EXPR, convert (bitsizetype, offset),
				 bitsize_unit_node))
      && 0 != (tem = size_binop (PLUS_EXPR, tem, bit_offset))
      && host_integerp (tem, 0))
    *pbitpos = tree_low_cst (tem, 0), *poffset = 0;
  else
    *pbitpos = tree_low_cst (bit_offset, 0), *poffset = offset;

  *pmode = mode;
  return exp;
}

/* Return 1 if T is an expression that get_inner_reference handles.  */

int
handled_component_p (t)
     tree t;
{
  switch (TREE_CODE (t))
    {
    case BIT_FIELD_REF:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      return 1;

    case NOP_EXPR:
    case CONVERT_EXPR:
      return (TYPE_MODE (TREE_TYPE (t))
	      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (t, 0))));

    default:
      return 0;
    }
}

/* Given an rtx VALUE that may contain additions and multiplications, return
   an equivalent value that just refers to a register, memory, or constant.
   This is done by generating instructions to perform the arithmetic and
   returning a pseudo-register containing the value.

   The returned value may be a REG, SUBREG, MEM or constant.  */

rtx
force_operand (value, target)
     rtx value, target;
{
  optab binoptab = 0;
  /* Use a temporary to force order of execution of calls to
     `force_operand'.  */
  rtx tmp;
  rtx op2;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = get_subtarget (target);

  /* Check for a PIC address load.  */
  if ((GET_CODE (value) == PLUS || GET_CODE (value) == MINUS)
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
			  target, 1);
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
      /* We give UNSIGNEDP = 0 to expand_binop
	 because the only operations we are expanding here are signed ones.  */
    }

#ifdef INSN_SCHEDULING
  /* On machines that have insn scheduling, we want all memory reference to be
     explicit, so we need to deal with such paradoxical SUBREGs.  */
  if (GET_CODE (value) == SUBREG && GET_CODE (SUBREG_REG (value)) == MEM
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
safe_from_p (x, exp, top_p)
     rtx x;
     tree exp;
     int top_p;
{
  rtx exp_rtl = 0;
  int i, nops;
  static tree save_expr_list;

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
      || (GET_CODE (x) == MEM
	  && (XEXP (x, 0) == virtual_outgoing_args_rtx
	      || (GET_CODE (XEXP (x, 0)) == PLUS
		  && XEXP (XEXP (x, 0), 0) == virtual_outgoing_args_rtx))))
    return 1;

  /* If this is a subreg of a hard register, declare it unsafe, otherwise,
     find the underlying pseudo.  */
  if (GET_CODE (x) == SUBREG)
    {
      x = SUBREG_REG (x);
      if (GET_CODE (x) == REG && REGNO (x) < FIRST_PSEUDO_REGISTER)
	return 0;
    }

  /* A SAVE_EXPR might appear many times in the expression passed to the
     top-level safe_from_p call, and if it has a complex subexpression,
     examining it multiple times could result in a combinatorial explosion.
     E.g. on an Alpha running at least 200MHz, a Fortran test case compiled
     with optimization took about 28 minutes to compile -- even though it was
     only a few lines long.  So we mark each SAVE_EXPR we see with TREE_PRIVATE
     and turn that off when we are done.  We keep a list of the SAVE_EXPRs
     we have processed.  Note that the only test of top_p was above.  */

  if (top_p)
    {
      int rtn;
      tree t;

      save_expr_list = 0;

      rtn = safe_from_p (x, exp, 0);

      for (t = save_expr_list; t != 0; t = TREE_CHAIN (t))
	TREE_PRIVATE (TREE_PURPOSE (t)) = 0;

      return rtn;
    }

  /* Now look at our tree code and possibly recurse.  */
  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case 'd':
      exp_rtl = DECL_RTL_IF_SET (exp);
      break;

    case 'c':
      return 1;

    case 'x':
      if (TREE_CODE (exp) == TREE_LIST)
	return ((TREE_VALUE (exp) == 0
		 || safe_from_p (x, TREE_VALUE (exp), 0))
		&& (TREE_CHAIN (exp) == 0
		    || safe_from_p (x, TREE_CHAIN (exp), 0)));
      else if (TREE_CODE (exp) == ERROR_MARK)
	return 1;	/* An already-visited SAVE_EXPR? */
      else
	return 0;

    case '1':
      return safe_from_p (x, TREE_OPERAND (exp, 0), 0);

    case '2':
    case '<':
      return (safe_from_p (x, TREE_OPERAND (exp, 0), 0)
	      && safe_from_p (x, TREE_OPERAND (exp, 1), 0));

    case 'e':
    case 'r':
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
		  || GET_CODE (DECL_RTL (exp)) != MEM)
		return 0;
	      else
		exp_rtl = XEXP (DECL_RTL (exp), 0);
	    }
	  break;

	case INDIRECT_REF:
	  if (GET_CODE (x) == MEM
	      && alias_sets_conflict_p (MEM_ALIAS_SET (x),
					get_alias_set (exp)))
	    return 0;
	  break;

	case CALL_EXPR:
	  /* Assume that the call will clobber all hard registers and
	     all of memory.  */
	  if ((GET_CODE (x) == REG && REGNO (x) < FIRST_PSEUDO_REGISTER)
	      || GET_CODE (x) == MEM)
	    return 0;
	  break;

	case RTL_EXPR:
	  /* If a sequence exists, we would have to scan every instruction
	     in the sequence to see if it was safe.  This is probably not
	     worthwhile.  */
	  if (RTL_EXPR_SEQUENCE (exp))
	    return 0;

	  exp_rtl = RTL_EXPR_RTL (exp);
	  break;

	case WITH_CLEANUP_EXPR:
	  exp_rtl = WITH_CLEANUP_EXPR_RTL (exp);
	  break;

	case CLEANUP_POINT_EXPR:
	  return safe_from_p (x, TREE_OPERAND (exp, 0), 0);

	case SAVE_EXPR:
	  exp_rtl = SAVE_EXPR_RTL (exp);
	  if (exp_rtl)
	    break;

	  /* If we've already scanned this, don't do it again.  Otherwise,
	     show we've scanned it and record for clearing the flag if we're
	     going on.  */
	  if (TREE_PRIVATE (exp))
	    return 1;

	  TREE_PRIVATE (exp) = 1;
	  if (! safe_from_p (x, TREE_OPERAND (exp, 0), 0))
	    {
	      TREE_PRIVATE (exp) = 0;
	      return 0;
	    }

	  save_expr_list = tree_cons (exp, NULL_TREE, save_expr_list);
	  return 1;

	case BIND_EXPR:
	  /* The only operand we look at is operand 1.  The rest aren't
	     part of the expression.  */
	  return safe_from_p (x, TREE_OPERAND (exp, 1), 0);

	case METHOD_CALL_EXPR:
	  /* This takes an rtx argument, but shouldn't appear here.  */
	  abort ();

	default:
	  break;
	}

      /* If we have an rtx, we do not need to scan our operands.  */
      if (exp_rtl)
	break;

      nops = first_rtl_op (TREE_CODE (exp));
      for (i = 0; i < nops; i++)
	if (TREE_OPERAND (exp, i) != 0
	    && ! safe_from_p (x, TREE_OPERAND (exp, i), 0))
	  return 0;

      /* If this is a language-specific tree code, it may require
	 special handling.  */
      if ((unsigned int) TREE_CODE (exp)
	  >= (unsigned int) LAST_AND_UNUSED_TREE_CODE
	  && !(*lang_hooks.safe_from_p) (x, exp))
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
	 are memory and they conflict.  */
      return ! (rtx_equal_p (x, exp_rtl)
		|| (GET_CODE (x) == MEM && GET_CODE (exp_rtl) == MEM
		    && true_dependence (exp_rtl, VOIDmode, x,
					rtx_addr_varies_p)));
    }

  /* If we reach here, it is safe.  */
  return 1;
}

/* Subroutine of expand_expr: return rtx if EXP is a
   variable or parameter; else return 0.  */

static rtx
var_rtx (exp)
     tree exp;
{
  STRIP_NOPS (exp);
  switch (TREE_CODE (exp))
    {
    case PARM_DECL:
    case VAR_DECL:
      return DECL_RTL (exp);
    default:
      return 0;
    }
}

#ifdef MAX_INTEGER_COMPUTATION_MODE

void
check_max_integer_computation_mode (exp)
     tree exp;
{
  enum tree_code code;
  enum machine_mode mode;

  /* Strip any NOPs that don't change the mode.  */
  STRIP_NOPS (exp);
  code = TREE_CODE (exp);

  /* We must allow conversions of constants to MAX_INTEGER_COMPUTATION_MODE.  */
  if (code == NOP_EXPR
      && TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
    return;

  /* First check the type of the overall operation.   We need only look at
     unary, binary and relational operations.  */
  if (TREE_CODE_CLASS (code) == '1'
      || TREE_CODE_CLASS (code) == '2'
      || TREE_CODE_CLASS (code) == '<')
    {
      mode = TYPE_MODE (TREE_TYPE (exp));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && mode > MAX_INTEGER_COMPUTATION_MODE)
	internal_error ("unsupported wide integer operation");
    }

  /* Check operand of a unary op.  */
  if (TREE_CODE_CLASS (code) == '1')
    {
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && mode > MAX_INTEGER_COMPUTATION_MODE)
	internal_error ("unsupported wide integer operation");
    }

  /* Check operands of a binary/comparison op.  */
  if (TREE_CODE_CLASS (code) == '2' || TREE_CODE_CLASS (code) == '<')
    {
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && mode > MAX_INTEGER_COMPUTATION_MODE)
	internal_error ("unsupported wide integer operation");

      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 1)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && mode > MAX_INTEGER_COMPUTATION_MODE)
	internal_error ("unsupported wide integer operation");
    }
}
#endif

/* Return the highest power of two that EXP is known to be a multiple of.
   This is used in updating alignment of MEMs in array references.  */

static HOST_WIDE_INT
highest_pow2_factor (exp)
     tree exp;
{
  HOST_WIDE_INT c0, c1;

  switch (TREE_CODE (exp))
    {
    case INTEGER_CST:
      /* We can find the lowest bit that's a one.  If the low
	 HOST_BITS_PER_WIDE_INT bits are zero, return BIGGEST_ALIGNMENT.
	 We need to handle this case since we can find it in a COND_EXPR,
	 a MIN_EXPR, or a MAX_EXPR.  If the constant overlows, we have an
	 erroneous program, so return BIGGEST_ALIGNMENT to avoid any
	 later ICE.  */
      if (TREE_CONSTANT_OVERFLOW (exp))
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

    case NON_LVALUE_EXPR:  case NOP_EXPR:  case CONVERT_EXPR:
    case SAVE_EXPR: case WITH_RECORD_EXPR:
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

/* Similar, except that it is known that the expression must be a multiple
   of the alignment of TYPE.  */

static HOST_WIDE_INT
highest_pow2_factor_for_type (type, exp)
     tree type;
     tree exp;
{
  HOST_WIDE_INT type_align, factor;

  factor = highest_pow2_factor (exp);
  type_align = TYPE_ALIGN (type) / BITS_PER_UNIT;
  return MAX (factor, type_align);
}

/* Return an object on the placeholder list that matches EXP, a
   PLACEHOLDER_EXPR.  An object "matches" if it is of the type of the
   PLACEHOLDER_EXPR or a pointer type to it.  For further information, see
   tree.def.  If no such object is found, return 0.  If PLIST is nonzero, it
   is a location which initially points to a starting location in the
   placeholder list (zero means start of the list) and where a pointer into
   the placeholder list at which the object is found is placed.  */

tree
find_placeholder (exp, plist)
     tree exp;
     tree *plist;
{
  tree type = TREE_TYPE (exp);
  tree placeholder_expr;

  for (placeholder_expr
       = plist && *plist ? TREE_CHAIN (*plist) : placeholder_list;
       placeholder_expr != 0;
       placeholder_expr = TREE_CHAIN (placeholder_expr))
    {
      tree need_type = TYPE_MAIN_VARIANT (type);
      tree elt;

      /* Find the outermost reference that is of the type we want.  If none,
	 see if any object has a type that is a pointer to the type we
	 want.  */
      for (elt = TREE_PURPOSE (placeholder_expr); elt != 0;
	   elt = ((TREE_CODE (elt) == COMPOUND_EXPR
		   || TREE_CODE (elt) == COND_EXPR)
		  ? TREE_OPERAND (elt, 1)
		  : (TREE_CODE_CLASS (TREE_CODE (elt)) == 'r'
		     || TREE_CODE_CLASS (TREE_CODE (elt)) == '1'
		     || TREE_CODE_CLASS (TREE_CODE (elt)) == '2'
		     || TREE_CODE_CLASS (TREE_CODE (elt)) == 'e')
		  ? TREE_OPERAND (elt, 0) : 0))
	if (TYPE_MAIN_VARIANT (TREE_TYPE (elt)) == need_type)
	  {
	    if (plist)
	      *plist = placeholder_expr;
	    return elt;
	  }

      for (elt = TREE_PURPOSE (placeholder_expr); elt != 0;
	   elt
	   = ((TREE_CODE (elt) == COMPOUND_EXPR
	       || TREE_CODE (elt) == COND_EXPR)
	      ? TREE_OPERAND (elt, 1)
	      : (TREE_CODE_CLASS (TREE_CODE (elt)) == 'r'
		 || TREE_CODE_CLASS (TREE_CODE (elt)) == '1'
		 || TREE_CODE_CLASS (TREE_CODE (elt)) == '2'
		 || TREE_CODE_CLASS (TREE_CODE (elt)) == 'e')
	      ? TREE_OPERAND (elt, 0) : 0))
	if (POINTER_TYPE_P (TREE_TYPE (elt))
	    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (elt)))
		== need_type))
	  {
	    if (plist)
	      *plist = placeholder_expr;
	    return build1 (INDIRECT_REF, need_type, elt);
	  }
    }

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
   EXPAND_INITIALIZER and EXPAND_SUM also have this effect.  */

rtx
expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  rtx op0, op1, temp;
  tree type = TREE_TYPE (exp);
  int unsignedp = TREE_UNSIGNED (type);
  enum machine_mode mode;
  enum tree_code code = TREE_CODE (exp);
  optab this_optab;
  rtx subtarget, original_target;
  int ignore;
  tree context;

  /* Handle ERROR_MARK before anybody tries to access its type.  */
  if (TREE_CODE (exp) == ERROR_MARK || TREE_CODE (type) == ERROR_MARK)
    {
      op0 = CONST0_RTX (tmode);
      if (op0 != 0)
	return op0;
      return const0_rtx;
    }

  mode = TYPE_MODE (type);
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  subtarget = get_subtarget (target);
  original_target = target;
  ignore = (target == const0_rtx
	    || ((code == NON_LVALUE_EXPR || code == NOP_EXPR
		 || code == CONVERT_EXPR || code == REFERENCE_EXPR
		 || code == COND_EXPR || code == VIEW_CONVERT_EXPR)
		&& TREE_CODE (type) == VOID_TYPE));

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
	  if (GET_CODE (temp) == MEM)
	    temp = copy_to_reg (temp);
	  return const0_rtx;
	}

      if (TREE_CODE_CLASS (code) == '1' || code == COMPONENT_REF
	  || code == INDIRECT_REF || code == BUFFER_REF)
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode,
			    modifier);

      else if (TREE_CODE_CLASS (code) == '2' || TREE_CODE_CLASS (code) == '<'
	       || code == ARRAY_REF || code == ARRAY_RANGE_REF)
	{
	  expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, modifier);
	  expand_expr (TREE_OPERAND (exp, 1), const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}
      else if ((code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
	       && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 1)))
	/* If the second operand has no side effects, just evaluate
	   the first.  */
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode,
			    modifier);
      else if (code == BIT_FIELD_REF)
	{
	  expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, modifier);
	  expand_expr (TREE_OPERAND (exp, 1), const0_rtx, VOIDmode, modifier);
	  expand_expr (TREE_OPERAND (exp, 2), const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}

      target = 0;
    }

#ifdef MAX_INTEGER_COMPUTATION_MODE
  /* Only check stuff here if the mode we want is different from the mode
     of the expression; if it's the same, check_max_integer_computiation_mode
     will handle it.  Do we really need to check this stuff at all?  */

  if (target
      && GET_MODE (target) != mode
      && TREE_CODE (exp) != INTEGER_CST
      && TREE_CODE (exp) != PARM_DECL
      && TREE_CODE (exp) != ARRAY_REF
      && TREE_CODE (exp) != ARRAY_RANGE_REF
      && TREE_CODE (exp) != COMPONENT_REF
      && TREE_CODE (exp) != BIT_FIELD_REF
      && TREE_CODE (exp) != INDIRECT_REF
      && TREE_CODE (exp) != CALL_EXPR
      && TREE_CODE (exp) != VAR_DECL
      && TREE_CODE (exp) != RTL_EXPR)
    {
      enum machine_mode mode = GET_MODE (target);

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && mode > MAX_INTEGER_COMPUTATION_MODE)
	internal_error ("unsupported wide integer operation");
    }

  if (tmode != mode
      && TREE_CODE (exp) != INTEGER_CST
      && TREE_CODE (exp) != PARM_DECL
      && TREE_CODE (exp) != ARRAY_REF
      && TREE_CODE (exp) != ARRAY_RANGE_REF
      && TREE_CODE (exp) != COMPONENT_REF
      && TREE_CODE (exp) != BIT_FIELD_REF
      && TREE_CODE (exp) != INDIRECT_REF
      && TREE_CODE (exp) != VAR_DECL
      && TREE_CODE (exp) != CALL_EXPR
      && TREE_CODE (exp) != RTL_EXPR
      && GET_MODE_CLASS (tmode) == MODE_INT
      && tmode > MAX_INTEGER_COMPUTATION_MODE)
    internal_error ("unsupported wide integer operation");

  check_max_integer_computation_mode (exp);
#endif

  /* If will do cse, generate all results into pseudo registers
     since 1) that allows cse to find more things
     and 2) otherwise cse could produce an insn the machine
     cannot support.  And exception is a CONSTRUCTOR into a multi-word
     MEM: that's much more likely to be most efficient into the MEM.  */

  if (! cse_not_expected && mode != BLKmode && target
      && (GET_CODE (target) != REG || REGNO (target) < FIRST_PSEUDO_REGISTER)
      && ! (code == CONSTRUCTOR && GET_MODE_SIZE (mode) > UNITS_PER_WORD))
    target = subtarget;

  switch (code)
    {
    case LABEL_DECL:
      {
	tree function = decl_function_context (exp);
	/* Handle using a label in a containing function.  */
	if (function != current_function_decl
	    && function != inline_function_decl && function != 0)
	  {
	    struct function *p = find_function_data (function);
	    p->expr->x_forced_labels
	      = gen_rtx_EXPR_LIST (VOIDmode, label_rtx (exp),
				   p->expr->x_forced_labels);
	  }
	else
	  {
	    if (modifier == EXPAND_INITIALIZER)
	      forced_labels = gen_rtx_EXPR_LIST (VOIDmode,
						 label_rtx (exp),
						 forced_labels);
	  }

	temp = gen_rtx_MEM (FUNCTION_MODE,
			    gen_rtx_LABEL_REF (Pmode, label_rtx (exp)));
	if (function != current_function_decl
	    && function != inline_function_decl && function != 0)
	  LABEL_REF_NONLOCAL_P (XEXP (temp, 0)) = 1;
	return temp;
      }

    case PARM_DECL:
      if (DECL_RTL (exp) == 0)
	{
	  error_with_decl (exp, "prior parameter's size depends on `%s'");
	  return CONST0_RTX (mode);
	}

      /* ... fall through ...  */

    case VAR_DECL:
      /* If a static var's type was incomplete when the decl was written,
	 but the type is complete now, lay out the decl now.  */
      if (DECL_SIZE (exp) == 0 && COMPLETE_TYPE_P (TREE_TYPE (exp))
	  && (TREE_STATIC (exp) || DECL_EXTERNAL (exp)))
	{
	  rtx value = DECL_RTL_IF_SET (exp);

	  layout_decl (exp, 0);

	  /* If the RTL was already set, update its mode and memory
	     attributes.  */
	  if (value != 0)
	    {
	      PUT_MODE (value, DECL_MODE (exp));
	      SET_DECL_RTL (exp, 0);
	      set_mem_attributes (value, exp, 1);
	      SET_DECL_RTL (exp, value);
	    }
	}

      /* ... fall through ...  */

    case FUNCTION_DECL:
    case RESULT_DECL:
      if (DECL_RTL (exp) == 0)
	abort ();

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
	  if (DECL_NO_STATIC_CHAIN (current_function_decl))
	    abort ();
	  mark_addressable (exp);
	  if (GET_CODE (DECL_RTL (exp)) != MEM)
	    abort ();
	  addr = XEXP (DECL_RTL (exp), 0);
	  if (GET_CODE (addr) == MEM)
	    addr
	      = replace_equiv_address (addr,
				       fix_lexical_addr (XEXP (addr, 0), exp));
	  else
	    addr = fix_lexical_addr (addr, exp);

	  temp = replace_equiv_address (DECL_RTL (exp), addr);
	}

      /* This is the case of an array whose size is to be determined
	 from its initializer, while the initializer is still being parsed.
	 See expand_decl.  */

      else if (GET_CODE (DECL_RTL (exp)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (exp), 0)) == REG)
	temp = validize_mem (DECL_RTL (exp));

      /* If DECL_RTL is memory, we are in the normal case and either
	 the address is not valid or it is not a register and -fforce-addr
	 is specified, get the address into a register.  */

      else if (GET_CODE (DECL_RTL (exp)) == MEM
	       && modifier != EXPAND_CONST_ADDRESS
	       && modifier != EXPAND_SUM
	       && modifier != EXPAND_INITIALIZER
	       && (! memory_address_p (DECL_MODE (exp),
				       XEXP (DECL_RTL (exp), 0))
		   || (flag_force_addr
		       && GET_CODE (XEXP (DECL_RTL (exp), 0)) != REG)))
	temp = replace_equiv_address (DECL_RTL (exp),
				      copy_rtx (XEXP (DECL_RTL (exp), 0)));

      /* If we got something, return it.  But first, set the alignment
	 if the address is a register.  */
      if (temp != 0)
	{
	  if (GET_CODE (temp) == MEM && GET_CODE (XEXP (temp, 0)) == REG)
	    mark_reg_pointer (XEXP (temp, 0), DECL_ALIGN (exp));

	  return temp;
	}

      /* If the mode of DECL_RTL does not match that of the decl, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  */

      if (GET_CODE (DECL_RTL (exp)) == REG
	  && GET_MODE (DECL_RTL (exp)) != DECL_MODE (exp))
	{
	  /* Get the signedness used for this variable.  Ensure we get the
	     same mode we got when the variable was declared.  */
	  if (GET_MODE (DECL_RTL (exp))
	      != promote_mode (type, DECL_MODE (exp), &unsignedp, 
			       (TREE_CODE (exp) == RESULT_DECL ? 1 : 0)))
	    abort ();

	  temp = gen_lowpart_SUBREG (mode, DECL_RTL (exp));
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	  return temp;
	}

      return DECL_RTL (exp);

    case INTEGER_CST:
      temp = immed_double_const (TREE_INT_CST_LOW (exp),
				 TREE_INT_CST_HIGH (exp), mode);

      /* ??? If overflow is set, fold will have done an incomplete job,
	 which can result in (plus xx (const_int 0)), which can get
	 simplified by validate_replace_rtx during virtual register
	 instantiation, which can result in unrecognizable insns.
	 Avoid this by forcing all overflows into registers.  */
      if (TREE_CONSTANT_OVERFLOW (exp)
	  && modifier != EXPAND_INITIALIZER)
	temp = force_reg (mode, temp);

      return temp;

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
	output_constant_def (exp, 1);

      /* TREE_CST_RTL probably contains a constant address.
	 On RISC machines where a constant address isn't valid,
	 make some insns to get that address into a register.  */
      if (GET_CODE (TREE_CST_RTL (exp)) == MEM
	  && modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_INITIALIZER
	  && modifier != EXPAND_SUM
	  && (! memory_address_p (mode, XEXP (TREE_CST_RTL (exp), 0))
	      || (flag_force_addr
		  && GET_CODE (XEXP (TREE_CST_RTL (exp), 0)) != REG)))
	return replace_equiv_address (TREE_CST_RTL (exp),
				      copy_rtx (XEXP (TREE_CST_RTL (exp), 0)));
      return TREE_CST_RTL (exp);

    case EXPR_WITH_FILE_LOCATION:
      {
	rtx to_return;
	const char *saved_input_filename = input_filename;
	int saved_lineno = lineno;
	input_filename = EXPR_WFL_FILENAME (exp);
	lineno = EXPR_WFL_LINENO (exp);
	if (EXPR_WFL_EMIT_LINE_NOTE (exp))
	  emit_line_note (input_filename, lineno);
	/* Possibly avoid switching back and forth here.  */
	to_return = expand_expr (EXPR_WFL_NODE (exp), target, tmode, modifier);
	input_filename = saved_input_filename;
	lineno = saved_lineno;
	return to_return;
      }

    case SAVE_EXPR:
      context = decl_function_context (exp);

      /* If this SAVE_EXPR was at global context, assume we are an
	 initialization function and move it into our context.  */
      if (context == 0)
	SAVE_EXPR_CONTEXT (exp) = current_function_decl;

      /* We treat inline_function_decl as an alias for the current function
	 because that is the inline function whose vars, types, etc.
	 are being merged into the current function.
	 See expand_inline_function.  */
      if (context == current_function_decl || context == inline_function_decl)
	context = 0;

      /* If this is non-local, handle it.  */
      if (context)
	{
	  /* The following call just exists to abort if the context is
	     not of a containing function.  */
	  find_function_data (context);

	  temp = SAVE_EXPR_RTL (exp);
	  if (temp && GET_CODE (temp) == REG)
	    {
	      put_var_into_stack (exp);
	      temp = SAVE_EXPR_RTL (exp);
	    }
	  if (temp == 0 || GET_CODE (temp) != MEM)
	    abort ();
	  return
	    replace_equiv_address (temp,
				   fix_lexical_addr (XEXP (temp, 0), exp));
	}
      if (SAVE_EXPR_RTL (exp) == 0)
	{
	  if (mode == VOIDmode)
	    temp = const0_rtx;
	  else
	    temp = assign_temp (build_qualified_type (type,
						      (TYPE_QUALS (type)
						       | TYPE_QUAL_CONST)),
				3, 0, 0);

	  SAVE_EXPR_RTL (exp) = temp;
	  if (!optimize && GET_CODE (temp) == REG)
	    save_expr_regs = gen_rtx_EXPR_LIST (VOIDmode, temp,
						save_expr_regs);

	  /* If the mode of TEMP does not match that of the expression, it
	     must be a promoted value.  We pass store_expr a SUBREG of the
	     wanted mode but mark it so that we know that it was already
	     extended.  Note that `unsignedp' was modified above in
	     this case.  */

	  if (GET_CODE (temp) == REG && GET_MODE (temp) != mode)
	    {
	      temp = gen_lowpart_SUBREG (mode, SAVE_EXPR_RTL (exp));
	      SUBREG_PROMOTED_VAR_P (temp) = 1;
	      SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	    }

	  if (temp == const0_rtx)
	    expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
	  else
	    store_expr (TREE_OPERAND (exp, 0), temp, 0);

	  TREE_USED (exp) = 1;
	}

      /* If the mode of SAVE_EXPR_RTL does not match that of the expression, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  */

      if (GET_CODE (SAVE_EXPR_RTL (exp)) == REG
	  && GET_MODE (SAVE_EXPR_RTL (exp)) != mode)
	{
	  /* Compute the signedness and make the proper SUBREG.  */
	  promote_mode (type, mode, &unsignedp, 0);
	  temp = gen_lowpart_SUBREG (mode, SAVE_EXPR_RTL (exp));
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	  return temp;
	}

      return SAVE_EXPR_RTL (exp);

    case UNSAVE_EXPR:
      {
	rtx temp;
	temp = expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);
	TREE_OPERAND (exp, 0) = unsave_expr_now (TREE_OPERAND (exp, 0));
	return temp;
      }

    case PLACEHOLDER_EXPR:
      {
	tree old_list = placeholder_list;
	tree placeholder_expr = 0;

	exp = find_placeholder (exp, &placeholder_expr);
	if (exp == 0)
	  abort ();

	placeholder_list = TREE_CHAIN (placeholder_expr);
	temp = expand_expr (exp, original_target, tmode, modifier);
	placeholder_list = old_list;
	return temp;
      }

      /* We can't find the object or there was a missing WITH_RECORD_EXPR.  */
      abort ();

    case WITH_RECORD_EXPR:
      /* Put the object on the placeholder list, expand our first operand,
	 and pop the list.  */
      placeholder_list = tree_cons (TREE_OPERAND (exp, 1), NULL_TREE,
				    placeholder_list);
      target = expand_expr (TREE_OPERAND (exp, 0), original_target, tmode,
			    modifier);
      placeholder_list = TREE_CHAIN (placeholder_list);
      return target;

    case GOTO_EXPR:
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == LABEL_DECL)
	expand_goto (TREE_OPERAND (exp, 0));
      else
	expand_computed_goto (TREE_OPERAND (exp, 0));
      return const0_rtx;

    case EXIT_EXPR:
      expand_exit_loop_if_false (NULL,
				 invert_truthvalue (TREE_OPERAND (exp, 0)));
      return const0_rtx;

    case LABELED_BLOCK_EXPR:
      if (LABELED_BLOCK_BODY (exp))
	expand_expr_stmt_value (LABELED_BLOCK_BODY (exp), 0, 1);
      /* Should perhaps use expand_label, but this is simpler and safer.  */
      do_pending_stack_adjust ();
      emit_label (label_rtx (LABELED_BLOCK_LABEL (exp)));
      return const0_rtx;

    case EXIT_BLOCK_EXPR:
      if (EXIT_BLOCK_RETURN (exp))
	sorry ("returned value in block_exit_expr");
      expand_goto (LABELED_BLOCK_LABEL (EXIT_BLOCK_LABELED_BLOCK (exp)));
      return const0_rtx;

    case LOOP_EXPR:
      push_temp_slots ();
      expand_start_loop (1);
      expand_expr_stmt_value (TREE_OPERAND (exp, 0), 0, 1);
      expand_end_loop ();
      pop_temp_slots ();

      return const0_rtx;

    case BIND_EXPR:
      {
	tree vars = TREE_OPERAND (exp, 0);
	int vars_need_expansion = 0;

	/* Need to open a binding contour here because
	   if there are any cleanups they must be contained here.  */
	expand_start_bindings (2);

	/* Mark the corresponding BLOCK for output in its proper place.  */
	if (TREE_OPERAND (exp, 2) != 0
	    && ! TREE_USED (TREE_OPERAND (exp, 2)))
	  insert_block (TREE_OPERAND (exp, 2));

	/* If VARS have not yet been expanded, expand them now.  */
	while (vars)
	  {
	    if (!DECL_RTL_SET_P (vars))
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
      if (RTL_EXPR_SEQUENCE (exp))
	{
	  if (RTL_EXPR_SEQUENCE (exp) == const0_rtx)
	    abort ();
	  emit_insns (RTL_EXPR_SEQUENCE (exp));
	  RTL_EXPR_SEQUENCE (exp) = const0_rtx;
	}
      preserve_rtl_expr_result (RTL_EXPR_RTL (exp));
      free_temps_for_rtl_expr (exp);
      return RTL_EXPR_RTL (exp);

    case CONSTRUCTOR:
      /* If we don't need the result, just ensure we evaluate any
	 subexpressions.  */
      if (ignore)
	{
	  tree elt;

	  for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	    expand_expr (TREE_VALUE (elt), const0_rtx, VOIDmode, 0);

	  return const0_rtx;
	}

      /* All elts simple constants => refer to a constant in memory.  But
	 if this is a non-BLKmode mode, let it store a field at a time
	 since that should make a CONST_INT or CONST_DOUBLE when we
	 fold.  Likewise, if we have a target we can use, it is best to
	 store directly into the target unless the type is large enough
	 that memcpy will be used.  If we are making an initializer and
	 all operands are constant, put it in memory as well.  */
      else if ((TREE_STATIC (exp)
		&& ((mode == BLKmode
		     && ! (target != 0 && safe_from_p (target, exp, 1)))
		    || TREE_ADDRESSABLE (exp)
		    || (host_integerp (TYPE_SIZE_UNIT (type), 1)
			&& (! MOVE_BY_PIECES_P
			    (tree_low_cst (TYPE_SIZE_UNIT (type), 1),
			     TYPE_ALIGN (type)))
			&& ! mostly_zeros_p (exp))))
	       || (modifier == EXPAND_INITIALIZER && TREE_CONSTANT (exp)))
	{
	  rtx constructor = output_constant_def (exp, 1);

	  if (modifier != EXPAND_CONST_ADDRESS
	      && modifier != EXPAND_INITIALIZER
	      && modifier != EXPAND_SUM)
	    constructor = validize_mem (constructor);

	  return constructor;
	}
      else
	{
	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */
	  if (target == 0 || ! safe_from_p (target, exp, 1)
	      || GET_CODE (target) == PARALLEL)
	    target
	      = assign_temp (build_qualified_type (type,
						   (TYPE_QUALS (type)
						    | (TREE_READONLY (exp)
						       * TYPE_QUAL_CONST))),
			     0, TREE_ADDRESSABLE (exp), 1);

	  store_constructor (exp, target, 0,
			     int_size_in_bytes (TREE_TYPE (exp)));
	  return target;
	}

    case INDIRECT_REF:
      {
	tree exp1 = TREE_OPERAND (exp, 0);
	tree index;
	tree string = string_constant (exp1, &index);

	/* Try to optimize reads from const strings.  */
 	if (string
 	    && TREE_CODE (string) == STRING_CST
 	    && TREE_CODE (index) == INTEGER_CST
	    && compare_tree_int (index, TREE_STRING_LENGTH (string)) < 0
 	    && GET_MODE_CLASS (mode) == MODE_INT
 	    && GET_MODE_SIZE (mode) == 1
	    && modifier != EXPAND_WRITE)
 	  return
	    GEN_INT (trunc_int_for_mode (TREE_STRING_POINTER (string)
					 [TREE_INT_CST_LOW (index)], mode));

	op0 = expand_expr (exp1, NULL_RTX, VOIDmode, EXPAND_SUM);
	op0 = memory_address (mode, op0);
	temp = gen_rtx_MEM (mode, op0);
	set_mem_attributes (temp, exp, 0);

	/* If we are writing to this object and its type is a record with
	   readonly fields, we must mark it as readonly so it will
	   conflict with readonly references to those fields.  */
	if (modifier == EXPAND_WRITE && readonly_fields_p (type))
	  RTX_UNCHANGING_P (temp) = 1;

	return temp;
      }

    case ARRAY_REF:
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) != ARRAY_TYPE)
	abort ();

      {
	tree array = TREE_OPERAND (exp, 0);
	tree domain = TYPE_DOMAIN (TREE_TYPE (array));
	tree low_bound = domain ? TYPE_MIN_VALUE (domain) : integer_zero_node;
	tree index = convert (sizetype, TREE_OPERAND (exp, 1));
	HOST_WIDE_INT i;

	/* Optimize the special-case of a zero lower bound.

	   We convert the low_bound to sizetype to avoid some problems
	   with constant folding.  (E.g. suppose the lower bound is 1,
	   and its mode is QI.  Without the conversion,  (ARRAY
	   +(INDEX-(unsigned char)1)) becomes ((ARRAY+(-(unsigned char)1))
	   +INDEX), which becomes (ARRAY+255+INDEX).  Oops!)  */

	if (! integer_zerop (low_bound))
	  index = size_diffop (index, convert (sizetype, low_bound));

	/* Fold an expression like: "foo"[2].
	   This is not done in fold so it won't happen inside &.
	   Don't fold if this is for wide characters since it's too
	   difficult to do correctly and this is a very rare case.  */

	if (modifier != EXPAND_CONST_ADDRESS && modifier != EXPAND_INITIALIZER
	    && TREE_CODE (array) == STRING_CST
	    && TREE_CODE (index) == INTEGER_CST
	    && compare_tree_int (index, TREE_STRING_LENGTH (array)) < 0
	    && GET_MODE_CLASS (mode) == MODE_INT
	    && GET_MODE_SIZE (mode) == 1)
	  return
	    GEN_INT (trunc_int_for_mode (TREE_STRING_POINTER (array)
					 [TREE_INT_CST_LOW (index)], mode));

	/* If this is a constant index into a constant array,
	   just get the value from the array.  Handle both the cases when
	   we have an explicit constructor and when our operand is a variable
	   that was declared const.  */

	if (modifier != EXPAND_CONST_ADDRESS && modifier != EXPAND_INITIALIZER
	    && TREE_CODE (array) == CONSTRUCTOR && ! TREE_SIDE_EFFECTS (array)
	    && TREE_CODE (index) == INTEGER_CST
	    && 0 > compare_tree_int (index,
				     list_length (CONSTRUCTOR_ELTS
						  (TREE_OPERAND (exp, 0)))))
	  {
	    tree elem;

	    for (elem = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0)),
		 i = TREE_INT_CST_LOW (index);
		 elem != 0 && i != 0; i--, elem = TREE_CHAIN (elem))
	      ;

	    if (elem)
	      return expand_expr (fold (TREE_VALUE (elem)), target, tmode,
				  modifier);
	  }

	else if (optimize >= 1
		 && modifier != EXPAND_CONST_ADDRESS
		 && modifier != EXPAND_INITIALIZER
		 && TREE_READONLY (array) && ! TREE_SIDE_EFFECTS (array)
		 && TREE_CODE (array) == VAR_DECL && DECL_INITIAL (array)
		 && TREE_CODE (DECL_INITIAL (array)) != ERROR_MARK)
	  {
	    if (TREE_CODE (index) == INTEGER_CST)
	      {
		tree init = DECL_INITIAL (array);

		if (TREE_CODE (init) == CONSTRUCTOR)
		  {
		    tree elem;

		    for (elem = CONSTRUCTOR_ELTS (init);
			 (elem
			  && !tree_int_cst_equal (TREE_PURPOSE (elem), index));
			 elem = TREE_CHAIN (elem))
		      ;

		    if (elem && !TREE_SIDE_EFFECTS (TREE_VALUE (elem)))
		      return expand_expr (fold (TREE_VALUE (elem)), target,
					  tmode, modifier);
		  }
		else if (TREE_CODE (init) == STRING_CST
			 && 0 > compare_tree_int (index,
						  TREE_STRING_LENGTH (init)))
		  {
		    tree type = TREE_TYPE (TREE_TYPE (init));
		    enum machine_mode mode = TYPE_MODE (type);

		    if (GET_MODE_CLASS (mode) == MODE_INT
			&& GET_MODE_SIZE (mode) == 1)
		      return GEN_INT (trunc_int_for_mode
				      (TREE_STRING_POINTER (init)
				       [TREE_INT_CST_LOW (index)], mode));
		  }
	      }
	  }
      }
      /* Fall through.  */

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_RANGE_REF:
      /* If the operand is a CONSTRUCTOR, we can just extract the
	 appropriate field if it is present.  Don't do this if we have
	 already written the data since we want to refer to that copy
	 and varasm.c assumes that's what we'll do.  */
      if (code == COMPONENT_REF
	  && TREE_CODE (TREE_OPERAND (exp, 0)) == CONSTRUCTOR
	  && TREE_CST_RTL (TREE_OPERAND (exp, 0)) == 0)
	{
	  tree elt;

	  for (elt = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0)); elt;
	       elt = TREE_CHAIN (elt))
	    if (TREE_PURPOSE (elt) == TREE_OPERAND (exp, 1)
		/* We can normally use the value of the field in the
		   CONSTRUCTOR.  However, if this is a bitfield in
		   an integral mode that we can fit in a HOST_WIDE_INT,
		   we must mask only the number of bits in the bitfield,
		   since this is done implicitly by the constructor.  If
		   the bitfield does not meet either of those conditions,
		   we can't do this optimization.  */
		&& (! DECL_BIT_FIELD (TREE_PURPOSE (elt))
		    || ((GET_MODE_CLASS (DECL_MODE (TREE_PURPOSE (elt)))
			 == MODE_INT)
			&& (GET_MODE_BITSIZE (DECL_MODE (TREE_PURPOSE (elt)))
			    <= HOST_BITS_PER_WIDE_INT))))
	      {
		op0 = expand_expr (TREE_VALUE (elt), target, tmode, modifier);
		if (DECL_BIT_FIELD (TREE_PURPOSE (elt)))
		  {
		    HOST_WIDE_INT bitsize
		      = TREE_INT_CST_LOW (DECL_SIZE (TREE_PURPOSE (elt)));
		    enum machine_mode imode
		      = TYPE_MODE (TREE_TYPE (TREE_PURPOSE (elt)));

		    if (TREE_UNSIGNED (TREE_TYPE (TREE_PURPOSE (elt))))
		      {
			op1 = GEN_INT (((HOST_WIDE_INT) 1 << bitsize) - 1);
			op0 = expand_and (imode, op0, op1, target);
		      }
		    else
		      {
			tree count
			  = build_int_2 (GET_MODE_BITSIZE (imode) - bitsize,
					 0);

			op0 = expand_shift (LSHIFT_EXPR, imode, op0, count,
					    target, 0);
			op0 = expand_shift (RSHIFT_EXPR, imode, op0, count,
					    target, 0);
		      }
		  }

		return op0;
	      }
	}

      {
	enum machine_mode mode1;
	HOST_WIDE_INT bitsize, bitpos;
	tree offset;
	int volatilep = 0;
	tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					&mode1, &unsignedp, &volatilep);
	rtx orig_op0;

	/* If we got back the original object, something is wrong.  Perhaps
	   we are evaluating an expression too early.  In any event, don't
	   infinitely recurse.  */
	if (tem == exp)
	  abort ();

	/* If TEM's type is a union of variable size, pass TARGET to the inner
	   computation, since it will need a temporary and TARGET is known
	   to have to do.  This occurs in unchecked conversion in Ada.  */

	orig_op0 = op0
	  = expand_expr (tem,
			 (TREE_CODE (TREE_TYPE (tem)) == UNION_TYPE
			  && (TREE_CODE (TYPE_SIZE (TREE_TYPE (tem)))
			      != INTEGER_CST)
			  ? target : NULL_RTX),
			 VOIDmode,
			 (modifier == EXPAND_INITIALIZER
			  || modifier == EXPAND_CONST_ADDRESS)
			 ? modifier : EXPAND_NORMAL);

	/* If this is a constant, put it into a register if it is a
	   legitimate constant and OFFSET is 0 and memory if it isn't.  */
	if (CONSTANT_P (op0))
	  {
	    enum machine_mode mode = TYPE_MODE (TREE_TYPE (tem));
	    if (mode != BLKmode && LEGITIMATE_CONSTANT_P (op0)
		&& offset == 0)
	      op0 = force_reg (mode, op0);
	    else
	      op0 = validize_mem (force_const_mem (mode, op0));
	  }

	if (offset != 0)
	  {
	    rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, EXPAND_SUM);

	    /* If this object is in a register, put it into memory.
	       This case can't occur in C, but can in Ada if we have
	       unchecked conversion of an expression from a scalar type to
	       an array or record type.  */
	    if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG
		|| GET_CODE (op0) == CONCAT || GET_CODE (op0) == ADDRESSOF)
	      {
		/* If the operand is a SAVE_EXPR, we can deal with this by
		   forcing the SAVE_EXPR into memory.  */
		if (TREE_CODE (TREE_OPERAND (exp, 0)) == SAVE_EXPR)
		  {
		    put_var_into_stack (TREE_OPERAND (exp, 0));
		    op0 = SAVE_EXPR_RTL (TREE_OPERAND (exp, 0));
		  }
		else
		  {
		    tree nt
		      = build_qualified_type (TREE_TYPE (tem),
					      (TYPE_QUALS (TREE_TYPE (tem))
					       | TYPE_QUAL_CONST));
		    rtx memloc = assign_temp (nt, 1, 1, 1);

		    emit_move_insn (memloc, op0);
		    op0 = memloc;
		  }
	      }

	    if (GET_CODE (op0) != MEM)
	      abort ();

#ifdef POINTERS_EXTEND_UNSIGNED
	    if (GET_MODE (offset_rtx) != Pmode)
	      offset_rtx = convert_memory_address (Pmode, offset_rtx);
#else
	    if (GET_MODE (offset_rtx) != ptr_mode)
	      offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);
#endif

	    /* A constant address in OP0 can have VOIDmode, we must not try
	       to call force_reg for that case.  Avoid that case.  */
	    if (GET_CODE (op0) == MEM
		&& GET_MODE (op0) == BLKmode
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
	if (GET_CODE (op0) == MEM && bitpos == 0 && offset != 0
	    && is_aligning_offset (offset, tem))
	  set_mem_align (op0, BIGGEST_ALIGNMENT);

	/* Don't forget about volatility even if this is a bitfield.  */
	if (GET_CODE (op0) == MEM && volatilep && ! MEM_VOLATILE_P (op0))
	  {
	    if (op0 == orig_op0)
	      op0 = copy_rtx (op0);

	    MEM_VOLATILE_P (op0) = 1;
	  }

	/* The following code doesn't handle CONCAT.
	   Assume only bitpos == 0 can be used for CONCAT, due to
	   one element arrays having the same mode as its element.  */
	if (GET_CODE (op0) == CONCAT)
	  {
	    if (bitpos != 0 || bitsize != GET_MODE_BITSIZE (GET_MODE (op0)))
	      abort ();
	    return op0;
	  }

	/* In cases where an aligned union has an unaligned object
	   as a field, we might be extracting a BLKmode value from
	   an integer-mode (e.g., SImode) object.  Handle this case
	   by doing the extract into an object as wide as the field
	   (which we know to be the width of a basic mode), then
	   storing into memory, and changing the mode to BLKmode.  */
	if (mode1 == VOIDmode
	    || GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG
	    || (mode1 != BLKmode && ! direct_load[(int) mode1]
		&& GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
		&& GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT
		&& modifier != EXPAND_CONST_ADDRESS
		&& modifier != EXPAND_INITIALIZER)
	    /* If the field isn't aligned enough to fetch as a memref,
	       fetch it as a bit field.  */
	    || (mode1 != BLKmode
		&& SLOW_UNALIGNED_ACCESS (mode1, MEM_ALIGN (op0))
		&& ((TYPE_ALIGN (TREE_TYPE (tem))
		     < GET_MODE_ALIGNMENT (mode))
		    || (bitpos % GET_MODE_ALIGNMENT (mode) != 0)))
	    /* If the type and the field are a constant size and the
	       size of the type isn't the same size as the bitfield,
	       we must use bitfield operations.  */
	    || (bitsize >= 0
		&& (TREE_CODE (TYPE_SIZE (TREE_TYPE (exp)))
		    == INTEGER_CST)
		&& 0 != compare_tree_int (TYPE_SIZE (TREE_TYPE (exp)),
					  bitsize)))
	  {
	    enum machine_mode ext_mode = mode;

	    if (ext_mode == BLKmode
		&& ! (target != 0 && GET_CODE (op0) == MEM
		      && GET_CODE (target) == MEM
		      && bitpos % BITS_PER_UNIT == 0))
	      ext_mode = mode_for_size (bitsize, MODE_INT, 1);

	    if (ext_mode == BLKmode)
	      {
		/* In this case, BITPOS must start at a byte boundary and
		   TARGET, if specified, must be a MEM.  */
		if (GET_CODE (op0) != MEM
		    || (target != 0 && GET_CODE (target) != MEM)
		    || bitpos % BITS_PER_UNIT != 0)
		  abort ();

		op0 = adjust_address (op0, VOIDmode, bitpos / BITS_PER_UNIT);
		if (target == 0)
		  target = assign_temp (type, 0, 1, 1);

		emit_block_move (target, op0,
				 GEN_INT ((bitsize + BITS_PER_UNIT - 1)
					  / BITS_PER_UNIT));

		return target;
	      }

	    op0 = validize_mem (op0);

	    if (GET_CODE (op0) == MEM && GET_CODE (XEXP (op0, 0)) == REG)
	      mark_reg_pointer (XEXP (op0, 0), MEM_ALIGN (op0));

	    op0 = extract_bit_field (op0, bitsize, bitpos,
				     unsignedp, target, ext_mode, ext_mode,
				     int_size_in_bytes (TREE_TYPE (tem)));

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

	    if (mode == BLKmode)
	      {
		rtx new = assign_temp (build_qualified_type
				       (type_for_mode (ext_mode, 0),
					TYPE_QUAL_CONST), 0, 1, 1);

		emit_move_insn (new, op0);
		op0 = copy_rtx (new);
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
	if (GET_CODE (XEXP (op0, 0)) == REG)
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

    case VTABLE_REF:
      {
	rtx insn, before = get_last_insn (), vtbl_ref;

	/* Evaluate the interior expression.  */
	subtarget = expand_expr (TREE_OPERAND (exp, 0), target,
				 tmode, modifier);

	/* Get or create an instruction off which to hang a note.  */
	if (REG_P (subtarget))
	  {
	    target = subtarget;
	    insn = get_last_insn ();
	    if (insn == before)
	      abort ();
	    if (! INSN_P (insn))
	      insn = prev_nonnote_insn (insn);
	  }
	else
	  {
	    target = gen_reg_rtx (GET_MODE (subtarget));
	    insn = emit_move_insn (target, subtarget);
	  }

	/* Collect the data for the note.  */
	vtbl_ref = XEXP (DECL_RTL (TREE_OPERAND (exp, 1)), 0);
	vtbl_ref = plus_constant (vtbl_ref,
				  tree_low_cst (TREE_OPERAND (exp, 2), 0));
	/* Discard the initial CONST that was added.  */
	vtbl_ref = XEXP (vtbl_ref, 0);

	REG_NOTES (insn)
	  = gen_rtx_EXPR_LIST (REG_VTABLE_REF, vtbl_ref, REG_NOTES (insn));

	return target;
      }

      /* Intended for a reference to a buffer of a file-object in Pascal.
	 But it's not certain that a special tree code will really be
	 necessary for these.  INDIRECT_REF might work for them.  */
    case BUFFER_REF:
      abort ();

    case IN_EXPR:
      {
	/* Pascal set IN expression.

	   Algorithm:
	       rlo       = set_low - (set_low%bits_per_word);
	       the_word  = set [ (index - rlo)/bits_per_word ];
	       bit_index = index % bits_per_word;
	       bitmask   = 1 << bit_index;
	       return !!(the_word & bitmask);  */

	tree set = TREE_OPERAND (exp, 0);
	tree index = TREE_OPERAND (exp, 1);
	int iunsignedp = TREE_UNSIGNED (TREE_TYPE (index));
	tree set_type = TREE_TYPE (set);
	tree set_low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (set_type));
	tree set_high_bound = TYPE_MAX_VALUE (TYPE_DOMAIN (set_type));
	rtx index_val = expand_expr (index, 0, VOIDmode, 0);
	rtx lo_r = expand_expr (set_low_bound, 0, VOIDmode, 0);
	rtx hi_r = expand_expr (set_high_bound, 0, VOIDmode, 0);
	rtx setval = expand_expr (set, 0, VOIDmode, 0);
	rtx setaddr = XEXP (setval, 0);
	enum machine_mode index_mode = TYPE_MODE (TREE_TYPE (index));
	rtx rlow;
	rtx diff, quo, rem, addr, bit, result;

	/* If domain is empty, answer is no.  Likewise if index is constant
	   and out of bounds.  */
	if (((TREE_CODE (set_high_bound) == INTEGER_CST
	     && TREE_CODE (set_low_bound) == INTEGER_CST
	     && tree_int_cst_lt (set_high_bound, set_low_bound))
	     || (TREE_CODE (index) == INTEGER_CST
		 && TREE_CODE (set_low_bound) == INTEGER_CST
		 && tree_int_cst_lt (index, set_low_bound))
	     || (TREE_CODE (set_high_bound) == INTEGER_CST
		 && TREE_CODE (index) == INTEGER_CST
		 && tree_int_cst_lt (set_high_bound, index))))
	  return const0_rtx;

	if (target == 0)
	  target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

	/* If we get here, we have to generate the code for both cases
	   (in range and out of range).  */

	op0 = gen_label_rtx ();
	op1 = gen_label_rtx ();

	if (! (GET_CODE (index_val) == CONST_INT
	       && GET_CODE (lo_r) == CONST_INT))
	  emit_cmp_and_jump_insns (index_val, lo_r, LT, NULL_RTX,
				   GET_MODE (index_val), iunsignedp, op1);

	if (! (GET_CODE (index_val) == CONST_INT
	       && GET_CODE (hi_r) == CONST_INT))
	  emit_cmp_and_jump_insns (index_val, hi_r, GT, NULL_RTX,
				   GET_MODE (index_val), iunsignedp, op1);

	/* Calculate the element number of bit zero in the first word
	   of the set.  */
	if (GET_CODE (lo_r) == CONST_INT)
	  rlow = GEN_INT (INTVAL (lo_r)
			  & ~((HOST_WIDE_INT) 1 << BITS_PER_UNIT));
	else
	  rlow = expand_binop (index_mode, and_optab, lo_r,
			       GEN_INT (~((HOST_WIDE_INT) 1 << BITS_PER_UNIT)),
			       NULL_RTX, iunsignedp, OPTAB_LIB_WIDEN);

	diff = expand_binop (index_mode, sub_optab, index_val, rlow,
			     NULL_RTX, iunsignedp, OPTAB_LIB_WIDEN);

	quo = expand_divmod (0, TRUNC_DIV_EXPR, index_mode, diff,
			     GEN_INT (BITS_PER_UNIT), NULL_RTX, iunsignedp);
	rem = expand_divmod (1, TRUNC_MOD_EXPR, index_mode, index_val,
			     GEN_INT (BITS_PER_UNIT), NULL_RTX, iunsignedp);

	addr = memory_address (byte_mode,
			       expand_binop (index_mode, add_optab, diff,
					     setaddr, NULL_RTX, iunsignedp,
					     OPTAB_LIB_WIDEN));

	/* Extract the bit we want to examine.  */
	bit = expand_shift (RSHIFT_EXPR, byte_mode,
			    gen_rtx_MEM (byte_mode, addr),
			    make_tree (TREE_TYPE (index), rem),
			    NULL_RTX, 1);
	result = expand_binop (byte_mode, and_optab, bit, const1_rtx,
			       GET_MODE (target) == byte_mode ? target : 0,
			       1, OPTAB_LIB_WIDEN);

	if (result != target)
	  convert_move (target, result, 1);

	/* Output the code to handle the out-of-range case.  */
	emit_jump (op0);
	emit_label (op1);
	emit_move_insn (target, const0_rtx);
	emit_label (op0);
	return target;
      }

    case WITH_CLEANUP_EXPR:
      if (WITH_CLEANUP_EXPR_RTL (exp) == 0)
	{
	  WITH_CLEANUP_EXPR_RTL (exp)
	    = expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);
	  expand_decl_cleanup (NULL_TREE, TREE_OPERAND (exp, 1));

	  /* That's it for this cleanup.  */
	  TREE_OPERAND (exp, 1) = 0;
	}
      return WITH_CLEANUP_EXPR_RTL (exp);

    case CLEANUP_POINT_EXPR:
      {
	/* Start a new binding layer that will keep track of all cleanup
	   actions to be performed.  */
	expand_start_bindings (2);

	target_temp_slot_level = temp_slot_level;

	op0 = expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);
	/* If we're going to use this value, load it up now.  */
	if (! ignore)
	  op0 = force_not_mem (op0);
	preserve_temp_slots (op0);
	expand_end_bindings (NULL_TREE, 0, 0);
      }
      return op0;

    case CALL_EXPR:
      /* Check for a built-in function.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	      == FUNCTION_DECL)
	  && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
        {
	  if (DECL_BUILT_IN_CLASS (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	      == BUILT_IN_FRONTEND)
	    return (*lang_expand_expr) (exp, original_target, tmode, modifier);
	  else
	    return expand_builtin (exp, target, subtarget, tmode, ignore);
	}

      return expand_call (exp, target, ignore);

    case NON_LVALUE_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case REFERENCE_EXPR:
      if (TREE_OPERAND (exp, 0) == error_mark_node)
	return const0_rtx;

      if (TREE_CODE (type) == UNION_TYPE)
	{
	  tree valtype = TREE_TYPE (TREE_OPERAND (exp, 0));

	  /* If both input and output are BLKmode, this conversion isn't doing
	     anything except possibly changing memory attribute.  */
	  if (mode == BLKmode && TYPE_MODE (valtype) == BLKmode)
	    {
	      rtx result = expand_expr (TREE_OPERAND (exp, 0), target, tmode,
					modifier);

	      result = copy_rtx (result);
	      set_mem_attributes (result, exp, 0);
	      return result;
	    }

	  if (target == 0)
	    target = assign_temp (type, 0, 1, 1);

	  if (GET_CODE (target) == MEM)
	    /* Store data into beginning of memory target.  */
	    store_expr (TREE_OPERAND (exp, 0),
			adjust_address (target, TYPE_MODE (valtype), 0), 0);

	  else if (GET_CODE (target) == REG)
	    /* Store this field into a union of the proper type.  */
	    store_field (target,
			 MIN ((int_size_in_bytes (TREE_TYPE
						  (TREE_OPERAND (exp, 0)))
			       * BITS_PER_UNIT),
			      (HOST_WIDE_INT) GET_MODE_BITSIZE (mode)),
			 0, TYPE_MODE (valtype), TREE_OPERAND (exp, 0),
			 VOIDmode, 0, type, 0);
	  else
	    abort ();

	  /* Return the entire union.  */
	  return target;
	}

      if (mode == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode,
			     modifier);

	  /* If the signedness of the conversion differs and OP0 is
	     a promoted SUBREG, clear that indication since we now
	     have to do the proper extension.  */
	  if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))) != unsignedp
	      && GET_CODE (op0) == SUBREG)
	    SUBREG_PROMOTED_VAR_P (op0) = 0;

	  return op0;
	}

      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, mode, modifier);
      if (GET_MODE (op0) == mode)
	return op0;

      /* If OP0 is a constant, just convert it into the proper mode.  */
      if (CONSTANT_P (op0))
	{
	  tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	  enum machine_mode inner_mode = TYPE_MODE (inner_type);

          if (modifier == EXPAND_INITIALIZER)
	    return simplify_gen_subreg (mode, op0, inner_mode,
					subreg_lowpart_offset (mode,
							       inner_mode));
	  else
	    return convert_modes (mode, inner_mode, op0,
				  TREE_UNSIGNED (inner_type));
	}

      if (modifier == EXPAND_INITIALIZER)
	return gen_rtx_fmt_e (unsignedp ? ZERO_EXTEND : SIGN_EXTEND, mode, op0);

      if (target == 0)
	return
	  convert_to_mode (mode, op0,
			   TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      else
	convert_move (target, op0,
		      TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case VIEW_CONVERT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, mode, modifier);

      /* If the input and output modes are both the same, we are done.
	 Otherwise, if neither mode is BLKmode and both are within a word, we
	 can use gen_lowpart.  If neither is true, make sure the operand is
	 in memory and convert the MEM to the new mode.  */
      if (TYPE_MODE (type) == GET_MODE (op0))
	;
      else if (TYPE_MODE (type) != BLKmode && GET_MODE (op0) != BLKmode
	       && GET_MODE_SIZE (TYPE_MODE (type)) <= UNITS_PER_WORD
	       && GET_MODE_SIZE (GET_MODE (op0)) <= UNITS_PER_WORD)
	op0 = gen_lowpart (TYPE_MODE (type), op0);
      else if (GET_CODE (op0) != MEM)
	{
	  /* If the operand is not a MEM, force it into memory.  Since we
	     are going to be be changing the mode of the MEM, don't call
	     force_const_mem for constants because we don't allow pool
	     constants to change mode.  */
	  tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

	  if (TREE_ADDRESSABLE (exp))
	    abort ();

	  if (target == 0 || GET_MODE (target) != TYPE_MODE (inner_type))
	    target
	      = assign_stack_temp_for_type
		(TYPE_MODE (inner_type),
		 GET_MODE_SIZE (TYPE_MODE (inner_type)), 0, inner_type);

	  emit_move_insn (target, op0);
	  op0 = target;
	}

      /* At this point, OP0 is in the correct mode.  If the output type is such
	 that the operand is known to be aligned, indicate that it is.
	 Otherwise, we need only be concerned about alignment for non-BLKmode
	 results.  */
      if (GET_CODE (op0) == MEM)
	{
	  op0 = copy_rtx (op0);

	  if (TYPE_ALIGN_OK (type))
	    set_mem_align (op0, MAX (MEM_ALIGN (op0), TYPE_ALIGN (type)));
	  else if (TYPE_MODE (type) != BLKmode && STRICT_ALIGNMENT
		   && MEM_ALIGN (op0) < GET_MODE_ALIGNMENT (TYPE_MODE (type)))
	    {
	      tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	      HOST_WIDE_INT temp_size
		= MAX (int_size_in_bytes (inner_type),
		       (HOST_WIDE_INT) GET_MODE_SIZE (TYPE_MODE (type)));
	      rtx new = assign_stack_temp_for_type (TYPE_MODE (type),
						    temp_size, 0, type);
	      rtx new_with_op0_mode = adjust_address (new, GET_MODE (op0), 0);

	      if (TREE_ADDRESSABLE (exp))
		abort ();

	      if (GET_MODE (op0) == BLKmode)
		emit_block_move (new_with_op0_mode, op0,
				 GEN_INT (GET_MODE_SIZE (TYPE_MODE (type))));
	      else
		emit_move_insn (new_with_op0_mode, op0);

	      op0 = new;
	    }
      
	  op0 = adjust_address (op0, TYPE_MODE (type), 0);
	}

      return op0;

    case PLUS_EXPR:
      /* We come here from MINUS_EXPR when the second operand is a
         constant.  */
    plus_expr:
      this_optab = ! unsignedp && flag_trapv
                   && (GET_MODE_CLASS (mode) == MODE_INT)
                   ? addv_optab : add_optab;

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
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	      && TREE_CONSTANT (TREE_OPERAND (exp, 1)))
	    {
	      rtx constant_part;

	      op1 = expand_expr (TREE_OPERAND (exp, 1), subtarget, VOIDmode,
				 EXPAND_SUM);
	      /* Use immed_double_const to ensure that the constant is
		 truncated according to the mode of OP1, then sign extended
		 to a HOST_WIDE_INT.  Using the constant directly can result
		 in non-canonical RTL in a 64x32 cross compile.  */
	      constant_part
		= immed_double_const (TREE_INT_CST_LOW (TREE_OPERAND (exp, 0)),
				      (HOST_WIDE_INT) 0,
				      TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 1))));
	      op1 = plus_constant (op1, INTVAL (constant_part));
	      if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
		op1 = force_operand (op1, target);
	      return op1;
	    }

	  else if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
		   && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_INT
		   && TREE_CONSTANT (TREE_OPERAND (exp, 0)))
	    {
	      rtx constant_part;

	      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode,
				 (modifier == EXPAND_INITIALIZER
				 ? EXPAND_INITIALIZER : EXPAND_SUM));
	      if (! CONSTANT_P (op0))
		{
		  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX,
				     VOIDmode, modifier);
		  /* Don't go to both_summands if modifier
		     says it's not right to return a PLUS.  */
		  if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
		    goto binop2;
		  goto both_summands;
		}
	      /* Use immed_double_const to ensure that the constant is
		 truncated according to the mode of OP1, then sign extended
		 to a HOST_WIDE_INT.  Using the constant directly can result
		 in non-canonical RTL in a 64x32 cross compile.  */
	      constant_part
		= immed_double_const (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)),
				      (HOST_WIDE_INT) 0,
				      TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))));
	      op0 = plus_constant (op0, INTVAL (constant_part));
	      if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
		op0 = force_operand (op0, target);
	      return op0;
	    }
	}

      /* No sense saving up arithmetic to be done
	 if it's all in the wrong mode to form part of an address.
	 And force_operand won't know whether to sign-extend or
	 zero-extend.  */
      if ((modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
	  || mode != ptr_mode)
	goto binop;

      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
	subtarget = 0;

      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, modifier);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, modifier);

    both_summands:
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
	    op0 = gen_rtx_PLUS (mode, op0, XEXP (op1, 0));
	  else
	    op0 = gen_rtx_PLUS (mode, XEXP (op1, 0), op0);

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
	    op1 = gen_rtx_PLUS (mode, constant_term, XEXP (op1, 1));
	}

      /* Put a constant term last and put a multiplication first.  */
      if (CONSTANT_P (op0) || GET_CODE (op1) == MULT)
	temp = op1, op1 = op0, op0 = temp;

      temp = simplify_binary_operation (PLUS, mode, op0, op1);
      return temp ? temp : gen_rtx_PLUS (mode, op0, op1);

    case MINUS_EXPR:
      /* For initializers, we are allowed to return a MINUS of two
	 symbolic constants.  Here we handle all cases when both operands
	 are constant.  */
      /* Handle difference of two symbolic constants,
	 for the sake of an initializer.  */
      if ((modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  && really_constant_p (TREE_OPERAND (exp, 0))
	  && really_constant_p (TREE_OPERAND (exp, 1)))
	{
	  rtx op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode,
				 modifier);
	  rtx op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode,
				 modifier);

	  /* If the last operand is a CONST_INT, use plus_constant of
	     the negated constant.  Else make the MINUS.  */
	  if (GET_CODE (op1) == CONST_INT)
	    return plus_constant (op0, - INTVAL (op1));
	  else
	    return gen_rtx_MINUS (mode, op0, op1);
	}
      /* Convert A - const to A + (-const).  */
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  tree negated = fold (build1 (NEGATE_EXPR, type,
				       TREE_OPERAND (exp, 1)));

	  if (TREE_UNSIGNED (type) || TREE_OVERFLOW (negated))
	    /* If we can't negate the constant in TYPE, leave it alone and
	       expand_binop will negate it for us.  We used to try to do it
	       here in the signed version of TYPE, but that doesn't work
	       on POINTER_TYPEs.  */;
	  else
	    {
	      exp = build (PLUS_EXPR, type, TREE_OPERAND (exp, 0), negated);
	      goto plus_expr;
	    }
	}
      this_optab = ! unsignedp && flag_trapv
                   && (GET_MODE_CLASS(mode) == MODE_INT)
                   ? subv_optab : sub_optab;
      goto binop;

    case MULT_EXPR:
      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
	{
	  tree t1 = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = t1;
	}

      /* Attempt to return something suitable for generating an
	 indexed address, for machines that support that.  */

      if (modifier == EXPAND_SUM && mode == ptr_mode
	  && host_integerp (TREE_OPERAND (exp, 1), 0))
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode,
			     EXPAND_SUM);

	  /* If we knew for certain that this is arithmetic for an array
	     reference, and we knew the bounds of the array, then we could
	     apply the distributive law across (PLUS X C) for constant C.
	     Without such knowledge, we risk overflowing the computation
	     when both X and C are large, but X+C isn't.  */
	  /* ??? Could perhaps special-case EXP being unsigned and C being
	     positive.  In that case we are certain that X+C is no smaller
	     than X and so the transformed expression will overflow iff the
	     original would have.  */

	  if (GET_CODE (op0) != REG)
	    op0 = force_operand (op0, NULL_RTX);
	  if (GET_CODE (op0) != REG)
	    op0 = copy_to_mode_reg (mode, op0);

	  return
	    gen_rtx_MULT (mode, op0,
			  GEN_INT (tree_low_cst (TREE_OPERAND (exp, 1), 0)));
	}

      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
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
	  optab other_optab = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
			? smul_widen_optab : umul_widen_optab);
	  this_optab = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
			? umul_widen_optab : smul_widen_optab);
	  if (mode == GET_MODE_WIDER_MODE (innermode))
	    {
	      if (this_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
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
	      else if (other_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing
		       && innermode == word_mode)
		{
		  rtx htem;
		  op0 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				     NULL_RTX, VOIDmode, 0);
		  if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
		    op1 = convert_modes (innermode, mode,
					 expand_expr (TREE_OPERAND (exp, 1),
						      NULL_RTX, VOIDmode, 0),
					 unsignedp);
		  else
		    op1 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 1), 0),
				       NULL_RTX, VOIDmode, 0);
		  temp = expand_binop (mode, other_optab, op0, op1, target,
				       unsignedp, OPTAB_LIB_WIDEN);
		  htem = expand_mult_highpart_adjust (innermode,
						      gen_highpart (innermode, temp),
						      op0, op1,
						      gen_highpart (innermode, temp),
						      unsignedp);
		  emit_move_insn (gen_highpart (innermode, temp), htem);
		  return temp;
		}
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
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
	subtarget = 0;
      /* Possible optimization: compute the dividend with EXPAND_SUM
	 then if the divisor is constant can optimize the case
	 where some terms of the dividend have coeffs divisible by it.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
      return expand_divmod (0, code, mode, op0, op1, target, unsignedp);

    case RDIV_EXPR:
      /* Emit a/b as a*(1/b).  Later we may manage CSE the reciprocal saving
         expensive divide.  If not, combine will rebuild the original
         computation.  */
      if (flag_unsafe_math_optimizations && optimize && !optimize_size
	  && TREE_CODE (type) == REAL_TYPE
	  && !real_onep (TREE_OPERAND (exp, 0)))
        return expand_expr (build (MULT_EXPR, type, TREE_OPERAND (exp, 0),
				   build (RDIV_EXPR, type,
					  build_real (type, dconst1),
					  TREE_OPERAND (exp, 1))),
			    target, tmode, unsignedp);
      this_optab = sdiv_optab;
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
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
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode,
                          ! unsignedp && flag_trapv
                          && (GET_MODE_CLASS(mode) == MODE_INT)
                          ? negv_optab : neg_optab, op0, target, 0);
      if (temp == 0)
	abort ();
      return temp;

    case ABS_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);

      /* Handle complex values specially.  */
      if (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT
	  || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	return expand_complex_abs (mode, op0, target, unsignedp);

      /* Unsigned abs is simply the operand.  Testing here means we don't
	 risk generating incorrect code below.  */
      if (TREE_UNSIGNED (type))
	return op0;

      return expand_abs (mode, op0, target, unsignedp,
			 safe_from_p (target, TREE_OPERAND (exp, 0), 1));

    case MAX_EXPR:
    case MIN_EXPR:
      target = original_target;
      if (target == 0 || ! safe_from_p (target, TREE_OPERAND (exp, 1), 1)
	  || (GET_CODE (target) == MEM && MEM_VOLATILE_P (target))
	  || GET_MODE (target) != mode
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

      /* At this point, a MEM target is no longer useful; we will get better
	 code without it.  */

      if (GET_CODE (target) == MEM)
	target = gen_reg_rtx (mode);

      if (target != op0)
	emit_move_insn (target, op0);

      op0 = gen_label_rtx ();

      /* If this mode is an integer too wide to compare properly,
	 compare word by word.  Rely on cse to optimize constant cases.  */
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && ! can_compare_p (GE, mode, ccp_jump))
	{
	  if (code == MAX_EXPR)
	    do_jump_by_parts_greater_rtx (mode, TREE_UNSIGNED (type),
					  target, op1, NULL_RTX, op0);
	  else
	    do_jump_by_parts_greater_rtx (mode, TREE_UNSIGNED (type),
					  op1, target, NULL_RTX, op0);
	}
      else
	{
	  int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1)));
	  do_compare_rtx_and_jump (target, op1, code == MAX_EXPR ? GE : LE,
				   unsignedp, mode, NULL_RTX, NULL_RTX,
				   op0);
	}
      emit_move_insn (target, op1);
      emit_label (op0);
      return target;

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

      /* BIT_AND_EXPR is for bitwise anding.  TRUTH_AND_EXPR is for anding two
	 boolean values when we want in all cases to compute both of them.  In
	 general it is fastest to do TRUTH_AND_EXPR by computing both operands
	 as actual zero-or-1 values and then bitwise anding.  In cases where
	 there cannot be any side effects, better code would be made by
	 treating TRUTH_AND_EXPR like TRUTH_ANDIF_EXPR; but the question is
	 how to recognize those cases.  */

    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:
      this_optab = and_optab;
      goto binop;

    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:
      this_optab = ior_optab;
      goto binop;

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      this_optab = xor_optab;
      goto binop;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
	subtarget = 0;
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      return expand_shift (code, mode, op0, TREE_OPERAND (exp, 1), target,
			   unsignedp);

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
      temp = do_store_flag (exp, target, tmode != VOIDmode ? tmode : mode, 0);
      if (temp != 0)
	return temp;

      /* For foo != 0, load foo, and if it is nonzero load 1 instead.  */
      if (code == NE_EXPR && integer_zerop (TREE_OPERAND (exp, 1))
	  && original_target
	  && GET_CODE (original_target) == REG
	  && (GET_MODE (original_target)
	      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	{
	  temp = expand_expr (TREE_OPERAND (exp, 0), original_target,
			      VOIDmode, 0);

	  /* If temp is constant, we can just compute the result.  */
	  if (GET_CODE (temp) == CONST_INT)
	    {
	      if (INTVAL (temp) != 0)
	        emit_move_insn (target, const1_rtx);
	      else
	        emit_move_insn (target, const0_rtx);

	      return target;
	    }

	  if (temp != original_target)
	    {
	      enum machine_mode mode1 = GET_MODE (temp);
	      if (mode1 == VOIDmode)
		mode1 = tmode != VOIDmode ? tmode : mode;
	      
	      temp = copy_to_mode_reg (mode1, temp);
	    }

	  op1 = gen_label_rtx ();
	  emit_cmp_and_jump_insns (temp, const0_rtx, EQ, NULL_RTX,
				   GET_MODE (temp), unsignedp, op1);
	  emit_move_insn (temp, const1_rtx);
	  emit_label (op1);
	  return temp;
	}

      /* If no set-flag instruction, must generate a conditional
	 store into a temporary variable.  Drop through
	 and handle this like && and ||.  */

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      if (! ignore
	  && (target == 0 || ! safe_from_p (target, exp, 1)
	      /* Make sure we don't have a hard reg (such as function's return
		 value) live across basic blocks, if not optimizing.  */
	      || (!optimize && GET_CODE (target) == REG
		  && REGNO (target) < FIRST_PSEUDO_REGISTER)))
	target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

      if (target)
	emit_clr_insn (target);

      op1 = gen_label_rtx ();
      jumpifnot (exp, op1);

      if (target)
	emit_0_to_1_insn (target);

      emit_label (op1);
      return ignore ? const0_rtx : target;

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
      /* If we would have a "singleton" (see below) were it not for a
	 conversion in each arm, bring that conversion back out.  */
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == NOP_EXPR
	  && TREE_CODE (TREE_OPERAND (exp, 2)) == NOP_EXPR
	  && (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0))
	      == TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 2), 0))))
	{
	  tree iftrue = TREE_OPERAND (TREE_OPERAND (exp, 1), 0);
	  tree iffalse = TREE_OPERAND (TREE_OPERAND (exp, 2), 0);

	  if ((TREE_CODE_CLASS (TREE_CODE (iftrue)) == '2'
	       && operand_equal_p (iffalse, TREE_OPERAND (iftrue, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (iffalse)) == '2'
		  && operand_equal_p (iftrue, TREE_OPERAND (iffalse, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (iftrue)) == '1'
		  && operand_equal_p (iffalse, TREE_OPERAND (iftrue, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (iffalse)) == '1'
		  && operand_equal_p (iftrue, TREE_OPERAND (iffalse, 0), 0)))
	    return expand_expr (build1 (NOP_EXPR, type,
					build (COND_EXPR, TREE_TYPE (iftrue),
					       TREE_OPERAND (exp, 0),
					       iftrue, iffalse)),
				target, tmode, modifier);
	}

      {
	/* Note that COND_EXPRs whose type is a structure or union
	   are required to be constructed to contain assignments of
	   a temporary variable, so that we can evaluate them here
	   for side effect only.  If type is void, we must do likewise.  */

	/* If an arm of the branch requires a cleanup,
	   only that cleanup is performed.  */

	tree singleton = 0;
	tree binary_op = 0, unary_op = 0;

	/* If this is (A ? 1 : 0) and A is a condition, just evaluate it and
	   convert it to our mode, if necessary.  */
	if (integer_onep (TREE_OPERAND (exp, 1))
	    && integer_zerop (TREE_OPERAND (exp, 2))
	    && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<')
	  {
	    if (ignore)
	      {
		expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode,
			     modifier);
		return const0_rtx;
	      }

	    op0 = expand_expr (TREE_OPERAND (exp, 0), target, mode, modifier);
	    if (GET_MODE (op0) == mode)
	      return op0;

	    if (target == 0)
	      target = gen_reg_rtx (mode);
	    convert_move (target, op0, unsignedp);
	    return target;
	  }

	/* Check for X ? A + B : A.  If we have this, we can copy A to the
	   output and conditionally add B.  Similarly for unary operations.
	   Don't do this if X has side-effects because those side effects
	   might affect A or B and the "?" operation is a sequence point in
	   ANSI.  (operand_equal_p tests for side effects.)  */

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

	/* If we are not to produce a result, we have no target.  Otherwise,
	   if a target was specified use it; it will not be used as an
	   intermediate target unless it is safe.  If no target, use a
	   temporary.  */

	if (ignore)
	  temp = 0;
	else if (original_target
		 && (safe_from_p (original_target, TREE_OPERAND (exp, 0), 1)
		     || (singleton && GET_CODE (original_target) == REG
			 && REGNO (original_target) >= FIRST_PSEUDO_REGISTER
			 && original_target == var_rtx (singleton)))
		 && GET_MODE (original_target) == mode
#ifdef HAVE_conditional_move
		 && (! can_conditionally_move_p (mode)
		     || GET_CODE (original_target) == REG
		     || TREE_ADDRESSABLE (type))
#endif
		 && (GET_CODE (original_target) != MEM
		     || TREE_ADDRESSABLE (type)))
	  temp = original_target;
	else if (TREE_ADDRESSABLE (type))
	  abort ();
	else
	  temp = assign_temp (type, 0, 0, 1);

	/* If we had X ? A + C : A, with C a constant power of 2, and we can
	   do the test of X as a store-flag operation, do this as
	   A + ((X != 0) << log C).  Similarly for other simple binary
	   operators.  Only do for C == 1 if BRANCH_COST is low.  */
	if (temp && singleton && binary_op
	    && (TREE_CODE (binary_op) == PLUS_EXPR
		|| TREE_CODE (binary_op) == MINUS_EXPR
		|| TREE_CODE (binary_op) == BIT_IOR_EXPR
		|| TREE_CODE (binary_op) == BIT_XOR_EXPR)
	    && (BRANCH_COST >= 3 ? integer_pow2p (TREE_OPERAND (binary_op, 1))
		: integer_onep (TREE_OPERAND (binary_op, 1)))
	    && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<')
	  {
	    rtx result;
	    optab boptab = (TREE_CODE (binary_op) == PLUS_EXPR
                            ? (TYPE_TRAP_SIGNED (TREE_TYPE (binary_op))
                               ? addv_optab : add_optab)
                            : TREE_CODE (binary_op) == MINUS_EXPR
                              ? (TYPE_TRAP_SIGNED (TREE_TYPE (binary_op))
                                 ? subv_optab : sub_optab)
                            : TREE_CODE (binary_op) == BIT_IOR_EXPR ? ior_optab
                            : xor_optab);

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
				    (safe_from_p (temp, singleton, 1)
				     ? temp : NULL_RTX),
				    mode, BRANCH_COST <= 1);

	    if (result != 0 && ! integer_onep (TREE_OPERAND (binary_op, 1)))
	      result = expand_shift (LSHIFT_EXPR, mode, result,
				     build_int_2 (tree_log2
						  (TREE_OPERAND
						   (binary_op, 1)),
						  0),
				     (safe_from_p (temp, singleton, 1)
				      ? temp : NULL_RTX), 0);

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

	do_pending_stack_adjust ();
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
		     && ! safe_from_p (temp, TREE_OPERAND (binary_op, 1), 1))
		    || (GET_CODE (temp) == REG
			&& REGNO (temp) < FIRST_PSEUDO_REGISTER))
		  temp = gen_reg_rtx (mode);
		store_expr (singleton, temp, 0);
	      }
	    else
	      expand_expr (singleton,
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	    if (singleton == TREE_OPERAND (exp, 1))
	      jumpif (TREE_OPERAND (exp, 0), op0);
	    else
	      jumpifnot (TREE_OPERAND (exp, 0), op0);

	    start_cleanup_deferral ();
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
	/* Check for A op 0 ? A : FOO and A op 0 ? FOO : A where OP is any
	   comparison operator.  If we have one of these cases, set the
	   output to A, branch on A (cse will merge these two references),
	   then set the output to FOO.  */
	else if (temp
		 && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<'
		 && integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
		 && operand_equal_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				     TREE_OPERAND (exp, 1), 0)
		 && (! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
		     || TREE_CODE (TREE_OPERAND (exp, 1)) == SAVE_EXPR)
		 && safe_from_p (temp, TREE_OPERAND (exp, 2), 1))
	  {
	    if (GET_CODE (temp) == REG
		&& REGNO (temp) < FIRST_PSEUDO_REGISTER)
	      temp = gen_reg_rtx (mode);
	    store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    jumpif (TREE_OPERAND (exp, 0), op0);

	    start_cleanup_deferral ();
	    store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    op1 = op0;
	  }
	else if (temp
		 && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == '<'
		 && integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
		 && operand_equal_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				     TREE_OPERAND (exp, 2), 0)
		 && (! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))
		     || TREE_CODE (TREE_OPERAND (exp, 2)) == SAVE_EXPR)
		 && safe_from_p (temp, TREE_OPERAND (exp, 1), 1))
	  {
	    if (GET_CODE (temp) == REG
		&& REGNO (temp) < FIRST_PSEUDO_REGISTER)
	      temp = gen_reg_rtx (mode);
	    store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    jumpifnot (TREE_OPERAND (exp, 0), op0);

	    start_cleanup_deferral ();
	    store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    op1 = op0;
	  }
	else
	  {
	    op1 = gen_label_rtx ();
	    jumpifnot (TREE_OPERAND (exp, 0), op0);

	    start_cleanup_deferral ();

	    /* One branch of the cond can be void, if it never returns. For
	       example A ? throw : E  */
	    if (temp != 0
		&& TREE_TYPE (TREE_OPERAND (exp, 1)) != void_type_node)
	      store_expr (TREE_OPERAND (exp, 1), temp, 0);
	    else
	      expand_expr (TREE_OPERAND (exp, 1),
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	    end_cleanup_deferral ();
	    emit_queue ();
	    emit_jump_insn (gen_jump (op1));
	    emit_barrier ();
	    emit_label (op0);
	    start_cleanup_deferral ();
	    if (temp != 0
		&& TREE_TYPE (TREE_OPERAND (exp, 2)) != void_type_node)
	      store_expr (TREE_OPERAND (exp, 2), temp, 0);
	    else
	      expand_expr (TREE_OPERAND (exp, 2),
			   ignore ? const0_rtx : NULL_RTX, VOIDmode, 0);
	  }

	end_cleanup_deferral ();

	emit_queue ();
	emit_label (op1);
	OK_DEFER_POP;

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
	tree cleanups = NULL_TREE;
	tree exp1;

	if (TREE_CODE (slot) != VAR_DECL)
	  abort ();

	if (! ignore)
	  target = original_target;

	/* Set this here so that if we get a target that refers to a
	   register variable that's already been used, put_reg_into_stack
	   knows that it should fix up those uses.  */
	TREE_USED (slot) = 1;

	if (target == 0)
	  {
	    if (DECL_RTL_SET_P (slot))
	      {
		target = DECL_RTL (slot);
		/* If we have already expanded the slot, so don't do
		   it again.  (mrs)  */
		if (TREE_OPERAND (exp, 1) == NULL_TREE)
		  return target;
	      }
	    else
	      {
		target = assign_temp (type, 2, 0, 1);
		/* All temp slots at this level must not conflict.  */
		preserve_temp_slots (target);
		SET_DECL_RTL (slot, target);
		if (TREE_ADDRESSABLE (slot))
		  put_var_into_stack (slot);

		/* Since SLOT is not known to the called function
		   to belong to its stack frame, we must build an explicit
		   cleanup.  This case occurs when we must build up a reference
		   to pass the reference as an argument.  In this case,
		   it is very likely that such a reference need not be
		   built here.  */

		if (TREE_OPERAND (exp, 2) == 0)
		  TREE_OPERAND (exp, 2) = maybe_build_cleanup (slot);
		cleanups = TREE_OPERAND (exp, 2);
	      }
	  }
	else
	  {
	    /* This case does occur, when expanding a parameter which
	       needs to be constructed on the stack.  The target
	       is the actual stack address that we want to initialize.
	       The function we call will perform the cleanup in this case.  */

	    /* If we have already assigned it space, use that space,
	       not target that we were passed in, as our target
	       parameter is only a hint.  */
	    if (DECL_RTL_SET_P (slot))
	      {
		target = DECL_RTL (slot);
		/* If we have already expanded the slot, so don't do
                   it again.  (mrs)  */
		if (TREE_OPERAND (exp, 1) == NULL_TREE)
		  return target;
	      }
	    else
	      {
		SET_DECL_RTL (slot, target);
		/* If we must have an addressable slot, then make sure that
		   the RTL that we just stored in slot is OK.  */
		if (TREE_ADDRESSABLE (slot))
		  put_var_into_stack (slot);
	      }
	  }

	exp1 = TREE_OPERAND (exp, 3) = TREE_OPERAND (exp, 1);
	/* Mark it as expanded.  */
	TREE_OPERAND (exp, 1) = NULL_TREE;

	store_expr (exp1, target, 0);

	expand_decl_cleanup (NULL_TREE, cleanups);

	return target;
      }

    case INIT_EXPR:
      {
	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);

	temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
	return temp;
      }

    case MODIFY_EXPR:
      {
	/* If lhs is complex, expand calls in rhs before computing it.
	   That's so we don't compute a pointer and save it over a
	   call.  If lhs is simple, compute it first so we can give it
	   as a target if the rhs is just a call.  This avoids an
	   extra temp and copy and that prevents a partial-subsumption
	   which makes bad code.  Actually we could treat
	   component_ref's of vars like vars.  */

	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);

	temp = 0;

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
	    && integer_onep (DECL_SIZE (TREE_OPERAND (lhs, 1)))
	    && integer_onep (DECL_SIZE (TREE_OPERAND (TREE_OPERAND (rhs, 1), 1))))
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

	temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
	
	return temp;
      }

    case RETURN_EXPR:
      if (!TREE_OPERAND (exp, 0))
	expand_null_return ();
      else
	expand_return (TREE_OPERAND (exp, 0));
      return const0_rtx;

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      return expand_increment (exp, 0, ignore);

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Faster to treat as pre-increment if result is not used.  */
      return expand_increment (exp, ! ignore, ignore);

    case ADDR_EXPR:
      /* Are we taking the address of a nested function?  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == FUNCTION_DECL
	  && decl_function_context (TREE_OPERAND (exp, 0)) != 0
	  && ! DECL_NO_STATIC_CHAIN (TREE_OPERAND (exp, 0))
	  && ! TREE_STATIC (exp))
	{
	  op0 = trampoline_address (TREE_OPERAND (exp, 0));
	  op0 = force_operand (op0, target);
	}
      /* If we are taking the address of something erroneous, just
	 return a zero.  */
      else if (TREE_CODE (TREE_OPERAND (exp, 0)) == ERROR_MARK)
	return const0_rtx;
      /* If we are taking the address of a constant and are at the
	 top level, we have to use output_constant_def since we can't
	 call force_const_mem at top level.  */
      else if (cfun == 0
	       && (TREE_CODE (TREE_OPERAND (exp, 0)) == CONSTRUCTOR
		   || (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0)))
		       == 'c')))
	op0 = XEXP (output_constant_def (TREE_OPERAND (exp, 0), 0), 0);
      else
	{
	  /* We make sure to pass const0_rtx down if we came in with
	     ignore set, to avoid doing the cleanups twice for something.  */
	  op0 = expand_expr (TREE_OPERAND (exp, 0),
			     ignore ? const0_rtx : NULL_RTX, VOIDmode,
			     (modifier == EXPAND_INITIALIZER
			      ? modifier : EXPAND_CONST_ADDRESS));

	  /* If we are going to ignore the result, OP0 will have been set
	     to const0_rtx, so just return it.  Don't get confused and
	     think we are taking the address of the constant.  */
	  if (ignore)
	    return op0;

	  /* Pass 1 for MODIFY, so that protect_from_queue doesn't get
	     clever and returns a REG when given a MEM.  */
	  op0 = protect_from_queue (op0, 1);

	  /* We would like the object in memory.  If it is a constant, we can
	     have it be statically allocated into memory.  For a non-constant,
	     we need to allocate some memory and store the value into it.  */

	  if (CONSTANT_P (op0))
	    op0 = force_const_mem (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
				   op0);
	  else if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG
		   || GET_CODE (op0) == CONCAT || GET_CODE (op0) == ADDRESSOF
		   || GET_CODE (op0) == PARALLEL)
	    {
	      /* If the operand is a SAVE_EXPR, we can deal with this by
		 forcing the SAVE_EXPR into memory.  */
	      if (TREE_CODE (TREE_OPERAND (exp, 0)) == SAVE_EXPR)
		{
		  put_var_into_stack (TREE_OPERAND (exp, 0));
		  op0 = SAVE_EXPR_RTL (TREE_OPERAND (exp, 0));
		}
	      else
		{
		  /* If this object is in a register, it can't be BLKmode.  */
		  tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
		  rtx memloc = assign_temp (inner_type, 1, 1, 1);

		  if (GET_CODE (op0) == PARALLEL)
		    /* Handle calls that pass values in multiple
		       non-contiguous locations.  The Irix 6 ABI has examples
		       of this.  */
		    emit_group_store (memloc, op0, 
				      int_size_in_bytes (inner_type));
		  else
		    emit_move_insn (memloc, op0);
		  
		  op0 = memloc;
		}
	    }

	  if (GET_CODE (op0) != MEM)
	    abort ();

	  mark_temp_addr_taken (op0);
	  if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	    {
	      op0 = XEXP (op0, 0);
#ifdef POINTERS_EXTEND_UNSIGNED
	      if (GET_MODE (op0) == Pmode && GET_MODE (op0) != mode
		  && mode == ptr_mode)
		op0 = convert_memory_address (ptr_mode, op0);
#endif
	      return op0;
	    }

	  /* If OP0 is not aligned as least as much as the type requires, we
	     need to make a temporary, copy OP0 to it, and take the address of
	     the temporary.  We want to use the alignment of the type, not of
	     the operand.  Note that this is incorrect for FUNCTION_TYPE, but
	     the test for BLKmode means that can't happen.  The test for
	     BLKmode is because we never make mis-aligned MEMs with
	     non-BLKmode.

	     We don't need to do this at all if the machine doesn't have
	     strict alignment.  */
	  if (STRICT_ALIGNMENT && GET_MODE (op0) == BLKmode
	      && (TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0)))
		  > MEM_ALIGN (op0))
	      && MEM_ALIGN (op0) < BIGGEST_ALIGNMENT)
	    {
	      tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	      rtx new
		= assign_stack_temp_for_type
		  (TYPE_MODE (inner_type),
		   MEM_SIZE (op0) ? INTVAL (MEM_SIZE (op0))
		   : int_size_in_bytes (inner_type),
		   1, build_qualified_type (inner_type,
					    (TYPE_QUALS (inner_type)
					     | TYPE_QUAL_CONST)));

	      if (TYPE_ALIGN_OK (inner_type))
		abort ();

	      emit_block_move (new, op0, expr_size (TREE_OPERAND (exp, 0)));
	      op0 = new;
	    }

	  op0 = force_operand (XEXP (op0, 0), target);
	}

      if (flag_force_addr
	  && GET_CODE (op0) != REG
	  && modifier != EXPAND_CONST_ADDRESS
	  && modifier != EXPAND_INITIALIZER
	  && modifier != EXPAND_SUM)
	op0 = force_reg (Pmode, op0);

      if (GET_CODE (op0) == REG
	  && ! REG_USERVAR_P (op0))
	mark_reg_pointer (op0, TYPE_ALIGN (TREE_TYPE (type)));

#ifdef POINTERS_EXTEND_UNSIGNED
      if (GET_MODE (op0) == Pmode && GET_MODE (op0) != mode
	  && mode == ptr_mode)
	op0 = convert_memory_address (ptr_mode, op0);
#endif

      return op0;

    case ENTRY_VALUE_EXPR:
      abort ();

    /* COMPLEX type for Extended Pascal & Fortran  */
    case COMPLEX_EXPR:
      {
	enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));
	rtx insns;

	/* Get the rtx code of the operands.  */
	op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
	op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);

	if (! target)
	  target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

	start_sequence ();

	/* Move the real (op0) and imaginary (op1) parts to their location.  */
	emit_move_insn (gen_realpart (mode, target), op0);
	emit_move_insn (gen_imagpart (mode, target), op1);

	insns = get_insns ();
	end_sequence ();

	/* Complex construction should appear as a single unit.  */
	/* If TARGET is a CONCAT, we got insns like RD = RS, ID = IS,
	   each with a separate pseudo as destination.
	   It's not correct for flow to treat them as a unit.  */
	if (GET_CODE (target) != CONCAT)
	  emit_no_conflict_block (insns, target, op0, op1, NULL_RTX);
	else
	  emit_insns (insns);

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
	enum machine_mode partmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));
	rtx imag_t;
	rtx insns;

	op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);

	if (! target)
	  target = gen_reg_rtx (mode);

	start_sequence ();

	/* Store the realpart and the negated imagpart to target.  */
	emit_move_insn (gen_realpart (partmode, target),
			gen_realpart (partmode, op0));

	imag_t = gen_imagpart (partmode, target);
	temp = expand_unop (partmode,
                            ! unsignedp && flag_trapv
                            && (GET_MODE_CLASS(partmode) == MODE_INT)
                            ? negv_optab : neg_optab,
			    gen_imagpart (partmode, op0), imag_t, 0);
	if (temp != imag_t)
	  emit_move_insn (imag_t, temp);

	insns = get_insns ();
	end_sequence ();

	/* Conjugate should appear as a single unit
	   If TARGET is a CONCAT, we got insns like RD = RS, ID = - IS,
	   each with a separate pseudo as destination.
	   It's not correct for flow to treat them as a unit.  */
	if (GET_CODE (target) != CONCAT)
	  emit_no_conflict_block (insns, target, op0, NULL_RTX, NULL_RTX);
	else
	  emit_insns (insns);

	return target;
      }

    case TRY_CATCH_EXPR:
      {
	tree handler = TREE_OPERAND (exp, 1);

	expand_eh_region_start ();

	op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);

	expand_eh_region_end_cleanup (handler);

	return op0;
      }

    case TRY_FINALLY_EXPR:
      {
	tree try_block = TREE_OPERAND (exp, 0);
	tree finally_block = TREE_OPERAND (exp, 1);
	rtx finally_label = gen_label_rtx ();
	rtx done_label = gen_label_rtx ();
	rtx return_link = gen_reg_rtx (Pmode);
	tree cleanup = build (GOTO_SUBROUTINE_EXPR, void_type_node,
			      (tree) finally_label, (tree) return_link);
	TREE_SIDE_EFFECTS (cleanup) = 1;

	/* Start a new binding layer that will keep track of all cleanup
	   actions to be performed.  */
	expand_start_bindings (2);

	target_temp_slot_level = temp_slot_level;

	expand_decl_cleanup (NULL_TREE, cleanup);
	op0 = expand_expr (try_block, target, tmode, modifier);

	preserve_temp_slots (op0);
	expand_end_bindings (NULL_TREE, 0, 0);
	emit_jump (done_label);
	emit_label (finally_label);
	expand_expr (finally_block, const0_rtx, VOIDmode, 0);
	emit_indirect_jump (return_link);
	emit_label (done_label);
	return op0;
      }

    case GOTO_SUBROUTINE_EXPR:
      {
	rtx subr = (rtx) TREE_OPERAND (exp, 0);
	rtx return_link = *(rtx *) &TREE_OPERAND (exp, 1);
	rtx return_address = gen_label_rtx ();
	emit_move_insn (return_link,
			gen_rtx_LABEL_REF (Pmode, return_address));
	emit_jump (subr);
	emit_label (return_address);
	return const0_rtx;
      }

    case VA_ARG_EXPR:
      return expand_builtin_va_arg (TREE_OPERAND (exp, 0), type);

    case EXC_PTR_EXPR:
      return get_exception_pointer (cfun);

    case FDESC_EXPR:
      /* Function descriptors are not valid except for as
	 initialization constants, and should not be expanded.  */
      abort ();

    default:
      return (*lang_expand_expr) (exp, original_target, tmode, modifier);
    }

  /* Here to do an ordinary binary operator, generating an instruction
     from the optab already placed in `this_optab'.  */
 binop:
  if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
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

/* Subroutine of above: returns 1 if OFFSET corresponds to an offset that
   when applied to the address of EXP produces an address known to be
   aligned more than BIGGEST_ALIGNMENT.  */

static int
is_aligning_offset (offset, exp)
     tree offset;
     tree exp;
{
  /* Strip off any conversions and WITH_RECORD_EXPR nodes.  */
  while (TREE_CODE (offset) == NON_LVALUE_EXPR
	 || TREE_CODE (offset) == NOP_EXPR
	 || TREE_CODE (offset) == CONVERT_EXPR
	 || TREE_CODE (offset) == WITH_RECORD_EXPR)
    offset = TREE_OPERAND (offset, 0);

  /* We must now have a BIT_AND_EXPR with a constant that is one less than
     power of 2 and which is larger than BIGGEST_ALIGNMENT.  */
  if (TREE_CODE (offset) != BIT_AND_EXPR
      || !host_integerp (TREE_OPERAND (offset, 1), 1)
      || compare_tree_int (TREE_OPERAND (offset, 1), BIGGEST_ALIGNMENT) <= 0
      || !exact_log2 (tree_low_cst (TREE_OPERAND (offset, 1), 1) + 1) < 0)
    return 0;

  /* Look at the first operand of BIT_AND_EXPR and strip any conversion.
     It must be NEGATE_EXPR.  Then strip any more conversions.  */
  offset = TREE_OPERAND (offset, 0);
  while (TREE_CODE (offset) == NON_LVALUE_EXPR
	 || TREE_CODE (offset) == NOP_EXPR
	 || TREE_CODE (offset) == CONVERT_EXPR)
    offset = TREE_OPERAND (offset, 0);

  if (TREE_CODE (offset) != NEGATE_EXPR)
    return 0;

  offset = TREE_OPERAND (offset, 0);
  while (TREE_CODE (offset) == NON_LVALUE_EXPR
	 || TREE_CODE (offset) == NOP_EXPR
	 || TREE_CODE (offset) == CONVERT_EXPR)
    offset = TREE_OPERAND (offset, 0);

  /* This must now be the address either of EXP or of a PLACEHOLDER_EXPR
     whose type is the same as EXP.  */
  return (TREE_CODE (offset) == ADDR_EXPR
	  && (TREE_OPERAND (offset, 0) == exp
	      || (TREE_CODE (TREE_OPERAND (offset, 0)) == PLACEHOLDER_EXPR
		  && (TREE_TYPE (TREE_OPERAND (offset, 0))
		      == TREE_TYPE (exp)))));
}

/* Return the tree node if a ARG corresponds to a string constant or zero
   if it doesn't.  If we return non-zero, set *PTR_OFFSET to the offset
   in bytes within the string that ARG is accessing.  The type of the
   offset will be `sizetype'.  */

tree
string_constant (arg, ptr_offset)
     tree arg;
     tree *ptr_offset;
{
  STRIP_NOPS (arg);

  if (TREE_CODE (arg) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST)
    {
      *ptr_offset = size_zero_node;
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
	  *ptr_offset = convert (sizetype, arg1);
	  return TREE_OPERAND (arg0, 0);
	}
      else if (TREE_CODE (arg1) == ADDR_EXPR
	       && TREE_CODE (TREE_OPERAND (arg1, 0)) == STRING_CST)
	{
	  *ptr_offset = convert (sizetype, arg0);
	  return TREE_OPERAND (arg1, 0);
	}
    }

  return 0;
}

/* Expand code for a post- or pre- increment or decrement
   and return the RTX for the result.
   POST is 1 for postinc/decrements and 0 for preinc/decrements.  */

static rtx
expand_increment (exp, post, ignore)
     tree exp;
     int post, ignore;
{
  rtx op0, op1;
  rtx temp, value;
  tree incremented = TREE_OPERAND (exp, 0);
  optab this_optab = add_optab;
  int icode;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  int op0_is_copy = 0;
  int single_insn = 0;
  /* 1 means we can't store into OP0 directly,
     because it is a subreg narrower than a word,
     and we don't dare clobber the rest of the word.  */
  int bad_subreg = 0;

  /* Stabilize any component ref that might need to be
     evaluated more than once below.  */
  if (!post
      || TREE_CODE (incremented) == BIT_FIELD_REF
      || (TREE_CODE (incremented) == COMPONENT_REF
	  && (TREE_CODE (TREE_OPERAND (incremented, 0)) != INDIRECT_REF
	      || DECL_BIT_FIELD (TREE_OPERAND (incremented, 1)))))
    incremented = stabilize_reference (incremented);
  /* Nested *INCREMENT_EXPRs can happen in C++.  We must force innermost
     ones into save exprs so that they don't accidentally get evaluated
     more than once by the code below.  */
  if (TREE_CODE (incremented) == PREINCREMENT_EXPR
      || TREE_CODE (incremented) == PREDECREMENT_EXPR)
    incremented = save_expr (incremented);

  /* Compute the operands as RTX.
     Note whether OP0 is the actual lvalue or a copy of it:
     I believe it is a copy iff it is a register or subreg
     and insns were generated in computing it.  */

  temp = get_last_insn ();
  op0 = expand_expr (incremented, NULL_RTX, VOIDmode, 0);

  /* If OP0 is a SUBREG made for a promoted variable, we cannot increment
     in place but instead must do sign- or zero-extension during assignment,
     so we copy it into a new register and let the code below use it as
     a copy.

     Note that we can safely modify this SUBREG since it is know not to be
     shared (it was made by the expand_expr call above).  */

  if (GET_CODE (op0) == SUBREG && SUBREG_PROMOTED_VAR_P (op0))
    {
      if (post)
	SUBREG_REG (op0) = copy_to_reg (SUBREG_REG (op0));
      else
	bad_subreg = 1;
    }
  else if (GET_CODE (op0) == SUBREG
	   && GET_MODE_BITSIZE (GET_MODE (op0)) < BITS_PER_WORD)
    {
      /* We cannot increment this SUBREG in place.  If we are
	 post-incrementing, get a copy of the old value.  Otherwise,
	 just mark that we cannot increment in place.  */
      if (post)
	op0 = copy_to_reg (op0);
      else
	bad_subreg = 1;
    }

  op0_is_copy = ((GET_CODE (op0) == SUBREG || GET_CODE (op0) == REG)
		 && temp != get_last_insn ());
  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);

  /* Decide whether incrementing or decrementing.  */
  if (TREE_CODE (exp) == POSTDECREMENT_EXPR
      || TREE_CODE (exp) == PREDECREMENT_EXPR)
    this_optab = sub_optab;

  /* Convert decrement by a constant into a negative increment.  */
  if (this_optab == sub_optab
      && GET_CODE (op1) == CONST_INT)
    {
      op1 = GEN_INT (-INTVAL (op1));
      this_optab = add_optab;
    }

  if (TYPE_TRAP_SIGNED (TREE_TYPE (exp)))
    this_optab = this_optab == add_optab ? addv_optab : subv_optab;

  /* For a preincrement, see if we can do this with a single instruction.  */
  if (!post)
    {
      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing
	  /* Make sure that OP0 is valid for operands 0 and 1
	     of the insn we want to queue.  */
	  && (*insn_data[icode].operand[0].predicate) (op0, mode)
	  && (*insn_data[icode].operand[1].predicate) (op0, mode)
	  && (*insn_data[icode].operand[2].predicate) (op1, mode))
	single_insn = 1;
    }

  /* If OP0 is not the actual lvalue, but rather a copy in a register,
     then we cannot just increment OP0.  We must therefore contrive to
     increment the original value.  Then, for postincrement, we can return
     OP0 since it is a copy of the old value.  For preincrement, expand here
     unless we can do it with a single insn.

     Likewise if storing directly into OP0 would clobber high bits
     we need to preserve (bad_subreg).  */
  if (op0_is_copy || (!post && !single_insn) || bad_subreg)
    {
      /* This is the easiest way to increment the value wherever it is.
	 Problems with multiple evaluation of INCREMENTED are prevented
	 because either (1) it is a component_ref or preincrement,
	 in which case it was stabilized above, or (2) it is an array_ref
	 with constant index in an array in a register, which is
	 safe to reevaluate.  */
      tree newexp = build (((TREE_CODE (exp) == POSTDECREMENT_EXPR
			     || TREE_CODE (exp) == PREDECREMENT_EXPR)
			    ? MINUS_EXPR : PLUS_EXPR),
			   TREE_TYPE (exp),
			   incremented,
			   TREE_OPERAND (exp, 1));

      while (TREE_CODE (incremented) == NOP_EXPR
	     || TREE_CODE (incremented) == CONVERT_EXPR)
	{
	  newexp = convert (TREE_TYPE (incremented), newexp);
	  incremented = TREE_OPERAND (incremented, 0);
	}

      temp = expand_assignment (incremented, newexp, ! post && ! ignore , 0);
      return post ? op0 : temp;
    }

  if (post)
    {
      /* We have a true reference to the value in OP0.
	 If there is an insn to add or subtract in this mode, queue it.
	 Queueing the increment insn avoids the register shuffling
	 that often results if we must increment now and first save
	 the old value for subsequent use.  */

#if 0  /* Turned off to avoid making extra insn for indexed memref.  */
      op0 = stabilize (op0);
#endif

      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing
	  /* Make sure that OP0 is valid for operands 0 and 1
	     of the insn we want to queue.  */
	  && (*insn_data[icode].operand[0].predicate) (op0, mode)
	  && (*insn_data[icode].operand[1].predicate) (op0, mode))
	{
	  if (! (*insn_data[icode].operand[2].predicate) (op1, mode))
	    op1 = force_reg (mode, op1);

	  return enqueue_insn (op0, GEN_FCN (icode) (op0, op0, op1));
	}
      if (icode != (int) CODE_FOR_nothing && GET_CODE (op0) == MEM)
	{
	  rtx addr = (general_operand (XEXP (op0, 0), mode)
		      ? force_reg (Pmode, XEXP (op0, 0))
		      : copy_to_reg (XEXP (op0, 0)));
	  rtx temp, result;

	  op0 = replace_equiv_address (op0, addr);
	  temp = force_reg (GET_MODE (op0), op0);
	  if (! (*insn_data[icode].operand[2].predicate) (op1, mode))
	    op1 = force_reg (mode, op1);

	  /* The increment queue is LIFO, thus we have to `queue'
	     the instructions in reverse order.  */
	  enqueue_insn (op0, gen_move_insn (op0, temp));
	  result = enqueue_insn (temp, GEN_FCN (icode) (temp, temp, op1));
	  return result;
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

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

void
init_pending_stack_adjust ()
{
  pending_stack_adjust = 0;
}

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.

   Note, if the current function calls alloca, then it must have a
   frame pointer regardless of the value of flag_omit_frame_pointer.  */

void
clear_pending_stack_adjust ()
{
#ifdef EXIT_IGNORE_STACK
  if (optimize > 0
      && (! flag_omit_frame_pointer || current_function_calls_alloca)
      && EXIT_IGNORE_STACK
      && ! (DECL_INLINE (current_function_decl) && ! flag_no_inline)
      && ! flag_inline_functions)
    {
      stack_pointer_delta -= pending_stack_adjust,
      pending_stack_adjust = 0;
    }
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
  enum tree_code code = TREE_CODE (exp);
  /* Some cases need to create a label to jump to
     in order to properly fall through.
     These cases set DROP_THROUGH_LABEL nonzero.  */
  rtx drop_through_label = 0;
  rtx temp;
  int i;
  tree type;
  enum machine_mode mode;

#ifdef MAX_INTEGER_COMPUTATION_MODE
  check_max_integer_computation_mode (exp);
#endif

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
	  || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_REF
	  || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_RANGE_REF)
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

    case WITH_RECORD_EXPR:
      /* Put the object on the placeholder list, recurse through our first
	 operand, and pop the list.  */
      placeholder_list = tree_cons (TREE_OPERAND (exp, 1), NULL_TREE,
				    placeholder_list);
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      placeholder_list = TREE_CHAIN (placeholder_list);
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
      do_compare_and_jump (build (NE_EXPR, TREE_TYPE (exp),
				  TREE_OPERAND (exp, 0),
				  TREE_OPERAND (exp, 1)),
			   NE, NE, if_false_label, if_true_label);
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
	  && (i = tree_floor_log2 (TREE_OPERAND (exp, 1))) >= 0
	  && (mode = mode_for_size (i + 1, MODE_INT, 0)) != BLKmode
	  && (type = type_for_mode (mode, 1)) != 0
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
      start_cleanup_deferral ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
	if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), NULL_RTX, if_true_label);
      start_cleanup_deferral ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case COMPOUND_EXPR:
      push_temp_slots ();
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      preserve_temp_slots (NULL_RTX);
      free_temp_slots ();
      pop_temp_slots ();
      emit_queue ();
      do_pending_stack_adjust ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      {
	HOST_WIDE_INT bitsize, bitpos;
	int unsignedp;
	enum machine_mode mode;
	tree type;
	tree offset;
	int volatilep = 0;

	/* Get description of this reference.  We don't actually care
	   about the underlying object here.  */
	get_inner_reference (exp, &bitsize, &bitpos, &offset, &mode,
			     &unsignedp, &volatilep);

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
	  rtx label1 = gen_label_rtx ();
	  drop_through_label = gen_label_rtx ();

	  do_jump (TREE_OPERAND (exp, 0), label1, NULL_RTX);

	  start_cleanup_deferral ();
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
	  end_cleanup_deferral ();
	}
      break;

    case EQ_EXPR:
      {
	tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

	if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
	    || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
	  {
	    tree exp0 = save_expr (TREE_OPERAND (exp, 0));
	    tree exp1 = save_expr (TREE_OPERAND (exp, 1));
	    do_jump
	      (fold
	       (build (TRUTH_ANDIF_EXPR, TREE_TYPE (exp),
		       fold (build (EQ_EXPR, TREE_TYPE (exp),
				    fold (build1 (REALPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp0)),
				    fold (build1 (REALPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp1)))),
		       fold (build (EQ_EXPR, TREE_TYPE (exp),
				    fold (build1 (IMAGPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp0)),
				    fold (build1 (IMAGPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp1)))))),
	       if_false_label, if_true_label);
	  }

	else if (integer_zerop (TREE_OPERAND (exp, 1)))
	  do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);

	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
		 && !can_compare_p (EQ, TYPE_MODE (inner_type), ccp_jump))
	  do_jump_by_parts_equality (exp, if_false_label, if_true_label);
	else
	  do_compare_and_jump (exp, EQ, EQ, if_false_label, if_true_label);
	break;
      }

    case NE_EXPR:
      {
	tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

	if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
	    || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
	  {
	    tree exp0 = save_expr (TREE_OPERAND (exp, 0));
	    tree exp1 = save_expr (TREE_OPERAND (exp, 1));
	    do_jump
	      (fold
	       (build (TRUTH_ORIF_EXPR, TREE_TYPE (exp),
		       fold (build (NE_EXPR, TREE_TYPE (exp),
				    fold (build1 (REALPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp0)),
				    fold (build1 (REALPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp1)))),
		       fold (build (NE_EXPR, TREE_TYPE (exp),
				    fold (build1 (IMAGPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp0)),
				    fold (build1 (IMAGPART_EXPR,
						  TREE_TYPE (inner_type),
						  exp1)))))),
	       if_false_label, if_true_label);
	  }

	else if (integer_zerop (TREE_OPERAND (exp, 1)))
	  do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);

	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
		 && !can_compare_p (NE, TYPE_MODE (inner_type), ccp_jump))
	  do_jump_by_parts_equality (exp, if_true_label, if_false_label);
	else
	  do_compare_and_jump (exp, NE, NE, if_false_label, if_true_label);
	break;
      }

    case LT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && ! can_compare_p (LT, mode, ccp_jump))
	do_jump_by_parts_greater (exp, 1, if_false_label, if_true_label);
      else
	do_compare_and_jump (exp, LT, LTU, if_false_label, if_true_label);
      break;

    case LE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && ! can_compare_p (LE, mode, ccp_jump))
	do_jump_by_parts_greater (exp, 0, if_true_label, if_false_label);
      else
	do_compare_and_jump (exp, LE, LEU, if_false_label, if_true_label);
      break;

    case GT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && ! can_compare_p (GT, mode, ccp_jump))
	do_jump_by_parts_greater (exp, 0, if_false_label, if_true_label);
      else
	do_compare_and_jump (exp, GT, GTU, if_false_label, if_true_label);
      break;

    case GE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && ! can_compare_p (GE, mode, ccp_jump))
	do_jump_by_parts_greater (exp, 1, if_true_label, if_false_label);
      else
	do_compare_and_jump (exp, GE, GEU, if_false_label, if_true_label);
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
      {
	enum rtx_code cmp, rcmp;
	int do_rev;

	if (code == UNORDERED_EXPR)
	  cmp = UNORDERED, rcmp = ORDERED;
	else
	  cmp = ORDERED, rcmp = UNORDERED;
	mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));

	do_rev = 0;
	if (! can_compare_p (cmp, mode, ccp_jump)
	    && (can_compare_p (rcmp, mode, ccp_jump)
		/* If the target doesn't provide either UNORDERED or ORDERED
		   comparisons, canonicalize on UNORDERED for the library.  */
		|| rcmp == UNORDERED))
	  do_rev = 1;

        if (! do_rev)
	  do_compare_and_jump (exp, cmp, cmp, if_false_label, if_true_label);
	else
	  do_compare_and_jump (exp, rcmp, rcmp, if_true_label, if_false_label);
      }
      break;

    {
      enum rtx_code rcode1;
      enum tree_code tcode2;

      case UNLT_EXPR:
	rcode1 = UNLT;
	tcode2 = LT_EXPR;
	goto unordered_bcc;
      case UNLE_EXPR:
	rcode1 = UNLE;
	tcode2 = LE_EXPR;
	goto unordered_bcc;
      case UNGT_EXPR:
	rcode1 = UNGT;
	tcode2 = GT_EXPR;
	goto unordered_bcc;
      case UNGE_EXPR:
	rcode1 = UNGE;
	tcode2 = GE_EXPR;
	goto unordered_bcc;
      case UNEQ_EXPR:
	rcode1 = UNEQ;
	tcode2 = EQ_EXPR;
	goto unordered_bcc;

      unordered_bcc:
        mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
	if (can_compare_p (rcode1, mode, ccp_jump))
	  do_compare_and_jump (exp, rcode1, rcode1, if_false_label,
			       if_true_label);
	else
	  {
	    tree op0 = save_expr (TREE_OPERAND (exp, 0));
	    tree op1 = save_expr (TREE_OPERAND (exp, 1));
	    tree cmp0, cmp1;

	    /* If the target doesn't support combined unordered
	       compares, decompose into UNORDERED + comparison.  */
	    cmp0 = fold (build (UNORDERED_EXPR, TREE_TYPE (exp), op0, op1));
	    cmp1 = fold (build (tcode2, TREE_TYPE (exp), op0, op1));
	    exp = build (TRUTH_ORIF_EXPR, TREE_TYPE (exp), cmp0, cmp1);
	    do_jump (exp, if_false_label, if_true_label);
	  }
      }
      break;

      /* Special case:
		__builtin_expect (<test>, 0)	and
		__builtin_expect (<test>, 1)

	 We need to do this here, so that <test> is not converted to a SCC
	 operation on machines that use condition code registers and COMPARE
	 like the PowerPC, and then the jump is done based on whether the SCC
	 operation produced a 1 or 0.  */
    case CALL_EXPR:
      /* Check for a built-in function.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR)
	{
	  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	  tree arglist = TREE_OPERAND (exp, 1);

	  if (TREE_CODE (fndecl) == FUNCTION_DECL
	      && DECL_BUILT_IN (fndecl)
	      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT
	      && arglist != NULL_TREE
	      && TREE_CHAIN (arglist) != NULL_TREE)
	    {
	      rtx seq = expand_builtin_expect_jump (exp, if_false_label,
						    if_true_label);

	      if (seq != NULL_RTX)
		{
		  emit_insn (seq);
		  return;
		}
	    }
	}
      /* fall through and generate the normal code.  */

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
      /* Do any postincrements in the expression that was tested.  */
      emit_queue ();

      if (GET_CODE (temp) == CONST_INT 
	  || (GET_CODE (temp) == CONST_DOUBLE && GET_MODE (temp) == VOIDmode)
	  || GET_CODE (temp) == LABEL_REF)
	{
	  rtx target = temp == const0_rtx ? if_false_label : if_true_label;
	  if (target)
	    emit_jump (target);
	}
      else if (GET_MODE_CLASS (GET_MODE (temp)) == MODE_INT
	       && ! can_compare_p (NE, GET_MODE (temp), ccp_jump))
	/* Note swapping the labels gives us not-equal.  */
	do_jump_by_parts_equality_rtx (temp, if_true_label, if_false_label);
      else if (GET_MODE (temp) != VOIDmode)
	do_compare_rtx_and_jump (temp, CONST0_RTX (GET_MODE (temp)),
				 NE, TREE_UNSIGNED (TREE_TYPE (exp)),
				 GET_MODE (temp), NULL_RTX,
				 if_false_label, if_true_label);
      else
	abort ();
    }

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
  int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)));

  do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label, if_true_label);
}

/* Compare OP0 with OP1, word at a time, in mode MODE.
   UNSIGNEDP says to do unsigned comparison.
   Jump to IF_TRUE_LABEL if OP0 is greater, IF_FALSE_LABEL otherwise.  */

void
do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label, if_true_label)
     enum machine_mode mode;
     int unsignedp;
     rtx op0, op1;
     rtx if_false_label, if_true_label;
{
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  rtx drop_through_label = 0;
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
      do_compare_rtx_and_jump (op0_word, op1_word, GT,
			       (unsignedp || i > 0), word_mode, NULL_RTX,
			       NULL_RTX, if_true_label);

      /* Consider lower words only if these are equal.  */
      do_compare_rtx_and_jump (op0_word, op1_word, NE, unsignedp, word_mode,
			       NULL_RTX, NULL_RTX, if_false_label);
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
    do_compare_rtx_and_jump (operand_subword_force (op0, i, mode),
			     operand_subword_force (op1, i, mode),
			     EQ, TREE_UNSIGNED (TREE_TYPE (exp)),
			     word_mode, NULL_RTX, if_false_label, NULL_RTX);

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Jump according to whether OP0 is 0.
   We assume that OP0 has an integer mode that is too wide
   for the available compare insns.  */

void
do_jump_by_parts_equality_rtx (op0, if_false_label, if_true_label)
     rtx op0;
     rtx if_false_label, if_true_label;
{
  int nwords = GET_MODE_SIZE (GET_MODE (op0)) / UNITS_PER_WORD;
  rtx part;
  int i;
  rtx drop_through_label = 0;

  /* The fastest way of doing this comparison on almost any machine is to
     "or" all the words and compare the result.  If all have to be loaded
     from memory and this is a very wide item, it's possible this may
     be slower, but that's highly unlikely.  */

  part = gen_reg_rtx (word_mode);
  emit_move_insn (part, operand_subword_force (op0, 0, GET_MODE (op0)));
  for (i = 1; i < nwords && part != 0; i++)
    part = expand_binop (word_mode, ior_optab, part,
			 operand_subword_force (op0, i, GET_MODE (op0)),
			 part, 1, OPTAB_WIDEN);

  if (part != 0)
    {
      do_compare_rtx_and_jump (part, const0_rtx, EQ, 1, word_mode,
			       NULL_RTX, if_false_label, if_true_label);

      return;
    }

  /* If we couldn't do the "or" simply, do this with a series of compares.  */
  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, GET_MODE (op0)),
			     const0_rtx, EQ, 1, word_mode, NULL_RTX,
			     if_false_label, NULL_RTX);

  if (if_true_label)
    emit_jump (if_true_label);

  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Generate code for a comparison of OP0 and OP1 with rtx code CODE.
   (including code to compute the values to be compared)
   and set (CC0) according to the result.
   The decision as to signed or unsigned comparison must be made by the caller.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.  */

rtx
compare_from_rtx (op0, op1, code, unsignedp, mode, size)
     rtx op0, op1;
     enum rtx_code code;
     int unsignedp;
     enum machine_mode mode;
     rtx size;
{
  rtx tem;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
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

  if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT
      && (tem = simplify_relational_operation (code, mode, op0, op1)) != 0)
    return tem;

#if 0
  /* There's no need to do this now that combine.c can eliminate lots of
     sign extensions.  This can be less efficient in certain cases on other
     machines.  */

  /* If this is a signed equality comparison, we can do it as an
     unsigned comparison since zero-extension is cheaper than sign
     extension and comparisons with zero are done as unsigned.  This is
     the case even on machines that can do fast sign extension, since
     zero-extension is easier to combine with other operations than
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

  emit_cmp_insn (op0, op1, code, size, mode, unsignedp);

  return gen_rtx_fmt_ee (code, VOIDmode, cc0_rtx, const0_rtx);
}

/* Like do_compare_and_jump but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.  */

void
do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode, size,
			 if_false_label, if_true_label)
     rtx op0, op1;
     enum rtx_code code;
     int unsignedp;
     enum machine_mode mode;
     rtx size;
     rtx if_false_label, if_true_label;
{
  rtx tem;
  int dummy_true_label = 0;

  /* Reverse the comparison if that is safe and we want to jump if it is
     false.  */
  if (! if_true_label && ! FLOAT_MODE_P (mode))
    {
      if_true_label = if_false_label;
      if_false_label = 0;
      code = reverse_condition (code);
    }

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
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

  if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT
      && (tem = simplify_relational_operation (code, mode, op0, op1)) != 0)
    {
      if (tem == const_true_rtx)
	{
	  if (if_true_label)
	    emit_jump (if_true_label);
	}
      else
	{
	  if (if_false_label)
	    emit_jump (if_false_label);
	}
      return;
    }

#if 0
  /* There's no need to do this now that combine.c can eliminate lots of
     sign extensions.  This can be less efficient in certain cases on other
     machines.  */

  /* If this is a signed equality comparison, we can do it as an
     unsigned comparison since zero-extension is cheaper than sign
     extension and comparisons with zero are done as unsigned.  This is
     the case even on machines that can do fast sign extension, since
     zero-extension is easier to combine with other operations than
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

  if (! if_true_label)
    {
      dummy_true_label = 1;
      if_true_label = gen_label_rtx ();
    }

  emit_cmp_and_jump_insns (op0, op1, code, size, mode, unsignedp,
			   if_true_label);

  if (if_false_label)
    emit_jump (if_false_label);
  if (dummy_true_label)
    emit_label (if_true_label);
}

/* Generate code for a comparison expression EXP (including code to compute
   the values to be compared) and a conditional jump to IF_FALSE_LABEL and/or
   IF_TRUE_LABEL.  One of the labels can be NULL_RTX, in which case the
   generated code will drop through.
   SIGNED_CODE should be the rtx operation for this comparison for
   signed data; UNSIGNED_CODE, likewise for use if data is unsigned.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.  */

static void
do_compare_and_jump (exp, signed_code, unsigned_code, if_false_label,
		     if_true_label)
     tree exp;
     enum rtx_code signed_code, unsigned_code;
     rtx if_false_label, if_true_label;
{
  rtx op0, op1;
  tree type;
  enum machine_mode mode;
  int unsignedp;
  enum rtx_code code;

  /* Don't crash if the comparison was erroneous.  */
  op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
  if (TREE_CODE (TREE_OPERAND (exp, 0)) == ERROR_MARK)
    return;

  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
  if (TREE_CODE (TREE_OPERAND (exp, 1)) == ERROR_MARK)
    return;

  type = TREE_TYPE (TREE_OPERAND (exp, 0));
  mode = TYPE_MODE (type);
  if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
      && (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST
	  || (GET_MODE_BITSIZE (mode)
	      > GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp,
								      1)))))))
    {
      /* op0 might have been replaced by promoted constant, in which
	 case the type of second argument should be used.  */
      type = TREE_TYPE (TREE_OPERAND (exp, 1));
      mode = TYPE_MODE (type);
    }
  unsignedp = TREE_UNSIGNED (type);
  code = unsignedp ? unsigned_code : signed_code;

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* If function pointers need to be "canonicalized" before they can
     be reliably compared, then canonicalize them.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == POINTER_TYPE
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	  == FUNCTION_TYPE))
    {
      rtx new_op0 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op0, op0));
      op0 = new_op0;
    }

  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 1))) == POINTER_TYPE
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 1))))
	  == FUNCTION_TYPE))
    {
      rtx new_op1 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op1, op1));
      op1 = new_op1;
    }
#endif

  /* Do any postincrements in the expression that was tested.  */
  emit_queue ();

  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode,
			   ((mode == BLKmode)
			    ? expr_size (TREE_OPERAND (exp, 0)) : NULL_RTX),
			   if_false_label, if_true_label);
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
  rtx result, label;

  /* If this is a TRUTH_NOT_EXPR, set a flag indicating we must invert the
     result at the end.  We can't simply invert the test since it would
     have already been inverted if it were valid.  This case occurs for
     some floating-point comparisons.  */

  if (TREE_CODE (exp) == TRUTH_NOT_EXPR)
    invert = 1, exp = TREE_OPERAND (exp, 0);

  arg0 = TREE_OPERAND (exp, 0);
  arg1 = TREE_OPERAND (exp, 1);

  /* Don't crash if the comparison was erroneous.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  type = TREE_TYPE (arg0);
  operand_mode = TYPE_MODE (type);
  unsignedp = TREE_UNSIGNED (type);

  /* We won't bother with BLKmode store-flag operations because it would mean
     passing a lot of information to emit_store_flag.  */
  if (operand_mode == BLKmode)
    return 0;

  /* We won't bother with store-flag operations involving function pointers
     when function pointers must be canonicalized before comparisons.  */
#ifdef HAVE_canonicalize_funcptr_for_compare
  if (HAVE_canonicalize_funcptr_for_compare
      && ((TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == POINTER_TYPE
	   && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	       == FUNCTION_TYPE))
	  || (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 1))) == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 1))))
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
      && integer_pow2p (TREE_OPERAND (arg0, 1)))
    {
      tree inner = TREE_OPERAND (arg0, 0);
      int bitnum = tree_log2 (TREE_OPERAND (arg0, 1));
      int ops_unsignedp;

      /* If INNER is a right shift of a constant and it plus BITNUM does
	 not overflow, adjust BITNUM and INNER.  */

      if (TREE_CODE (inner) == RSHIFT_EXPR
	  && TREE_CODE (TREE_OPERAND (inner, 1)) == INTEGER_CST
	  && TREE_INT_CST_HIGH (TREE_OPERAND (inner, 1)) == 0
	  && bitnum < TYPE_PRECISION (type)
	  && 0 > compare_tree_int (TREE_OPERAND (inner, 1),
				   bitnum - TYPE_PRECISION (type)))
	{
	  bitnum += TREE_INT_CST_LOW (TREE_OPERAND (inner, 1));
	  inner = TREE_OPERAND (inner, 0);
	}

      /* If we are going to be able to omit the AND below, we must do our
	 operations as unsigned.  If we must use the AND, we have a choice.
	 Normally unsigned is faster, but for some machines signed is.  */
      ops_unsignedp = (bitnum == TYPE_PRECISION (type) - 1 ? 1
#ifdef LOAD_EXTEND_OP
		       : (LOAD_EXTEND_OP (operand_mode) == SIGN_EXTEND ? 0 : 1)
#else
		       : 1
#endif
		       );

      if (! get_subtarget (subtarget)
	  || GET_MODE (subtarget) != operand_mode
	  || ! safe_from_p (subtarget, inner, 1))
	subtarget = 0;

      op0 = expand_expr (inner, subtarget, VOIDmode, 0);

      if (bitnum != 0)
	op0 = expand_shift (RSHIFT_EXPR, operand_mode, op0,
			    size_int (bitnum), subtarget, ops_unsignedp);

      if (GET_MODE (op0) != mode)
	op0 = convert_to_mode (mode, op0, ops_unsignedp);

      if ((code == EQ && ! invert) || (code == NE && invert))
	op0 = expand_binop (mode, xor_optab, op0, const1_rtx, subtarget,
			    ops_unsignedp, OPTAB_LIB_WIDEN);

      /* Put the AND last so it can combine with more things.  */
      if (bitnum != TYPE_PRECISION (type) - 1)
	op0 = expand_and (mode, op0, const1_rtx, subtarget);

      return op0;
    }

  /* Now see if we are likely to be able to do this.  Return if not.  */
  if (! can_compare_p (code, operand_mode, ccp_store_flag))
    return 0;

  icode = setcc_gen_code[(int) code];
  if (icode == CODE_FOR_nothing
      || (only_cheap && insn_data[(int) icode].operand[0].mode != mode))
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

  if (! get_subtarget (target)
      || GET_MODE (subtarget) != operand_mode
      || ! safe_from_p (subtarget, arg1, 1))
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
  if (GET_CODE (target) != REG
      || reg_mentioned_p (target, op0) || reg_mentioned_p (target, op1))
    target = gen_reg_rtx (GET_MODE (target));

  emit_move_insn (target, invert ? const0_rtx : const1_rtx);
  result = compare_from_rtx (op0, op1, code, unsignedp,
			     operand_mode, NULL_RTX);
  if (GET_CODE (result) == CONST_INT)
    return (((result == const0_rtx && ! invert)
	     || (result != const0_rtx && invert))
	    ? const0_rtx : const1_rtx);

  /* The code of RESULT may not match CODE if compare_from_rtx
     decided to swap its operands and reverse the original code.

     We know that compare_from_rtx returns either a CONST_INT or
     a new comparison code, so it is safe to just extract the
     code from RESULT.  */
  code = GET_CODE (result);

  label = gen_label_rtx ();
  if (bcc_gen_fctn[(int) code] == 0)
    abort ();

  emit_jump_insn ((*bcc_gen_fctn[(int) code]) (label));
  emit_move_insn (target, invert ? const1_rtx : const0_rtx);
  emit_label (label);

  return target;
}


/* Stubs in case we haven't got a casesi insn.  */
#ifndef HAVE_casesi
# define HAVE_casesi 0
# define gen_casesi(a, b, c, d, e) (0)
# define CODE_FOR_casesi CODE_FOR_nothing
#endif

/* If the machine does not have a case insn that compares the bounds,
   this means extra overhead for dispatch tables, which raises the
   threshold for using them.  */
#ifndef CASE_VALUES_THRESHOLD
#define CASE_VALUES_THRESHOLD (HAVE_casesi ? 4 : 5)
#endif /* CASE_VALUES_THRESHOLD */

unsigned int
case_values_threshold ()
{
  return CASE_VALUES_THRESHOLD;
}

/* Attempt to generate a casesi instruction.  Returns 1 if successful,
   0 otherwise (i.e. if there is no casesi instruction).  */
int
try_casesi (index_type, index_expr, minval, range,
	    table_label, default_label)
     tree index_type, index_expr, minval, range;
     rtx table_label ATTRIBUTE_UNUSED;
     rtx default_label;
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
      rtx rangertx = expand_expr (range, NULL_RTX, VOIDmode, 0);

      /* We must handle the endpoints in the original mode.  */
      index_expr = build (MINUS_EXPR, index_type,
			  index_expr, minval);
      minval = integer_zero_node;
      index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
      emit_cmp_and_jump_insns (rangertx, index, LTU, NULL_RTX,
			       omode, 1, default_label);
      /* Now we can safely truncate.  */
      index = convert_to_mode (index_mode, index, 0);
    }
  else
    {
      if (TYPE_MODE (index_type) != index_mode)
	{
	  index_expr = convert (type_for_size (index_bits, 0),
				index_expr);
	  index_type = TREE_TYPE (index_expr);
	}

      index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
    }
  emit_queue ();
  index = protect_from_queue (index, 0);
  do_pending_stack_adjust ();

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[0].mode;
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[0].predicate)
      (index, op_mode))
    index = copy_to_mode_reg (op_mode, index);

  op1 = expand_expr (minval, NULL_RTX, VOIDmode, 0);

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[1].mode;
  op1 = convert_modes (op_mode, TYPE_MODE (TREE_TYPE (minval)),
		       op1, TREE_UNSIGNED (TREE_TYPE (minval)));
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[1].predicate)
      (op1, op_mode))
    op1 = copy_to_mode_reg (op_mode, op1);

  op2 = expand_expr (range, NULL_RTX, VOIDmode, 0);

  op_mode = insn_data[(int) CODE_FOR_casesi].operand[2].mode;
  op2 = convert_modes (op_mode, TYPE_MODE (TREE_TYPE (range)),
		       op2, TREE_UNSIGNED (TREE_TYPE (range)));
  if (! (*insn_data[(int) CODE_FOR_casesi].operand[2].predicate)
      (op2, op_mode))
    op2 = copy_to_mode_reg (op_mode, op2);

  emit_jump_insn (gen_casesi (index, op1, op2,
			      table_label, default_label));
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
do_tablejump (index, mode, range, table_label, default_label)
     rtx index, range, table_label, default_label;
     enum machine_mode mode;
{
  rtx temp, vector;

  /* Do an unsigned comparison (in the proper mode) between the index
     expression and the value which represents the length of the range.
     Since we just finished subtracting the lower bound of the range
     from the index expression, this comparison allows us to simultaneously
     check that the original index expression value is both greater than
     or equal to the minimum value of the range and less than or equal to
     the maximum value of the range.  */

  emit_cmp_and_jump_insns (index, range, GTU, NULL_RTX, mode, 1,
			   default_label);

  /* If index is in range, it must fit in Pmode.
     Convert to Pmode so we can index with it.  */
  if (mode != Pmode)
    index = convert_to_mode (Pmode, index, 1);

  /* Don't let a MEM slip thru, because then INDEX that comes
     out of PIC_CASE_VECTOR_ADDRESS won't be a valid address,
     and break_out_memory_refs will go to work on it and mess it up.  */
#ifdef PIC_CASE_VECTOR_ADDRESS
  if (flag_pic && GET_CODE (index) != REG)
    index = copy_to_mode_reg (Pmode, index);
#endif

  /* If flag_force_addr were to affect this address
     it could interfere with the tricky assumptions made
     about addresses that contain label-refs,
     which may be valid only very near the tablejump itself.  */
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
    index = memory_address_noforce (CASE_VECTOR_MODE, index);
  temp = gen_reg_rtx (CASE_VECTOR_MODE);
  vector = gen_rtx_MEM (CASE_VECTOR_MODE, index);
  RTX_UNCHANGING_P (vector) = 1;
  convert_move (temp, vector, 0);

  emit_jump_insn (gen_tablejump (temp, table_label));

  /* If we are generating PIC code or if the table is PC-relative, the
     table and JUMP_INSN must be adjacent, so don't output a BARRIER.  */
  if (! CASE_VECTOR_PC_RELATIVE && ! flag_pic)
    emit_barrier ();
}

int
try_tablejump (index_type, index_expr, minval, range,
	       table_label, default_label)
     tree index_type, index_expr, minval, range;
     rtx table_label, default_label;
{
  rtx index;

  if (! HAVE_tablejump)
    return 0;

  index_expr = fold (build (MINUS_EXPR, index_type,
			    convert (index_type, index_expr),
			    convert (index_type, minval)));
  index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
  emit_queue ();
  index = protect_from_queue (index, 0);
  do_pending_stack_adjust ();

  do_tablejump (index, TYPE_MODE (index_type),
		convert_modes (TYPE_MODE (index_type),
			       TYPE_MODE (TREE_TYPE (range)),
			       expand_expr (range, NULL_RTX,
					    VOIDmode, 0),
			       TREE_UNSIGNED (TREE_TYPE (range))),
		table_label, default_label);
  return 1;
}
