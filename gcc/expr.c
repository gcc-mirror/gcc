/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988, 92-97, 1998 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include <stdio.h>
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "typeclass.h"
#include "defaults.h"

#include "bytecode.h"
#include "bc-opcode.h"
#include "bc-typecd.h"
#include "bc-optab.h"
#include "bc-emit.h"


#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.

   They should if the stack and args grow in opposite directions, but
   only if we have push insns.  */

#ifdef PUSH_ROUNDING

#if defined (STACK_GROWS_DOWNWARD) != defined (ARGS_GROW_DOWNWARD)
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

/* When temporaries are created by TARGET_EXPRs, they are created at
   this level of temp_slot_level, so that they can remain allocated
   until no longer needed.  CLEANUP_POINT_EXPRs define the lifetime
   of TARGET_EXPRs.  */
int target_temp_slot_level;

/* Nonzero means __builtin_saveregs has already been done in this function.
   The value is the pseudoreg containing the value __builtin_saveregs
   returned.  */
static rtx saveregs_value;

/* Similarly for __builtin_apply_args.  */
static rtx apply_args_value;

/* Don't check memory usage, since code is being emitted to check a memory
   usage.  Used when flag_check_memory_usage is true, to avoid infinite
   recursion.  */
static int in_check_memory_usage;

/* This structure is used by move_by_pieces to describe the move to
   be performed.  */
struct move_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  int to_struct;
  rtx from;
  rtx from_addr;
  int autinc_from;
  int explicit_inc_from;
  int from_struct;
  int len;
  int offset;
  int reverse;
};

/* This structure is used by clear_by_pieces to describe the clear to
   be performed.  */

struct clear_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  int to_struct;
  int len;
  int offset;
  int reverse;
};

/* Used to generate bytecodes: keep track of size of local variables,
   as well as depth of arithmetic stack. (Notice that variables are
   stored on the machine's stack, not the arithmetic stack.) */

static rtx get_push_address	PROTO ((int));
extern int local_vars_size;
extern int stack_depth;
extern int max_stack_depth;
extern struct obstack permanent_obstack;
extern rtx arg_pointer_save_area;

static rtx enqueue_insn		PROTO((rtx, rtx));
static int queued_subexp_p	PROTO((rtx));
static void init_queue		PROTO((void));
static void move_by_pieces	PROTO((rtx, rtx, int, int));
static int move_by_pieces_ninsns PROTO((unsigned int, int));
static void move_by_pieces_1	PROTO((rtx (*) (), enum machine_mode,
				       struct move_by_pieces *));
static void clear_by_pieces	PROTO((rtx, int, int));
static void clear_by_pieces_1	PROTO((rtx (*) (), enum machine_mode,
				       struct clear_by_pieces *));
static int is_zeros_p		PROTO((tree));
static int mostly_zeros_p	PROTO((tree));
static void store_constructor	PROTO((tree, rtx, int));
static rtx store_field		PROTO((rtx, int, int, enum machine_mode, tree,
				       enum machine_mode, int, int, int));
static tree save_noncopied_parts PROTO((tree, tree));
static tree init_noncopied_parts PROTO((tree, tree));
static int safe_from_p		PROTO((rtx, tree, int));
static int fixed_type_p		PROTO((tree));
static rtx var_rtx		PROTO((tree));
static int get_pointer_alignment PROTO((tree, unsigned));
static tree string_constant	PROTO((tree, tree *));
static tree c_strlen		PROTO((tree));
static rtx expand_builtin	PROTO((tree, rtx, rtx,
				       enum machine_mode, int));
static int apply_args_size	PROTO((void));
static int apply_result_size	PROTO((void));
static rtx result_vector	PROTO((int, rtx));
static rtx expand_builtin_apply_args PROTO((void));
static rtx expand_builtin_apply	PROTO((rtx, rtx, rtx));
static void expand_builtin_return PROTO((rtx));
static rtx expand_increment	PROTO((tree, int, int));
void bc_expand_increment	PROTO((struct increment_operator *, tree));
rtx bc_allocate_local		PROTO((int, int));
void bc_store_memory 		PROTO((tree, tree));
tree bc_expand_component_address PROTO((tree));
tree bc_expand_address 		PROTO((tree));
void bc_expand_constructor 	PROTO((tree));
void bc_adjust_stack 		PROTO((int));
tree bc_canonicalize_array_ref	PROTO((tree));
void bc_load_memory		PROTO((tree, tree));
void bc_load_externaddr		PROTO((rtx));
void bc_load_externaddr_id	PROTO((tree, int));
void bc_load_localaddr		PROTO((rtx));
void bc_load_parmaddr		PROTO((rtx));
static void preexpand_calls	PROTO((tree));
static void do_jump_by_parts_greater PROTO((tree, int, rtx, rtx));
void do_jump_by_parts_greater_rtx PROTO((enum machine_mode, int, rtx, rtx, rtx, rtx));
static void do_jump_by_parts_equality PROTO((tree, rtx, rtx));
static void do_jump_by_parts_equality_rtx PROTO((rtx, rtx, rtx));
static void do_jump_for_compare	PROTO((rtx, rtx, rtx));
static rtx compare		PROTO((tree, enum rtx_code, enum rtx_code));
static rtx do_store_flag	PROTO((tree, rtx, enum machine_mode, int));
extern tree truthvalue_conversion       PROTO((tree));

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
enum insn_code movstr_optab[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block clears.  */
enum insn_code clrstr_optab[NUM_MACHINE_MODES];

/* SLOW_UNALIGNED_ACCESS is non-zero if unaligned accesses are very slow.  */

#ifndef SLOW_UNALIGNED_ACCESS
#define SLOW_UNALIGNED_ACCESS STRICT_ALIGNMENT
#endif

/* Register mappings for target machines without register windows.  */
#ifndef INCOMING_REGNO
#define INCOMING_REGNO(OUT) (OUT)
#endif
#ifndef OUTGOING_REGNO
#define OUTGOING_REGNO(IN) (IN)
#endif

/* Maps used to convert modes to const, load, and store bytecodes.  */
enum bytecode_opcode mode_to_const_map[MAX_MACHINE_MODE];
enum bytecode_opcode mode_to_load_map[MAX_MACHINE_MODE];
enum bytecode_opcode mode_to_store_map[MAX_MACHINE_MODE];

/* Initialize maps used to convert modes to const, load, and store
   bytecodes.  */

void
bc_init_mode_to_opcode_maps ()
{
  int mode;

  for (mode = 0; mode < (int) MAX_MACHINE_MODE; mode++)
    mode_to_const_map[mode]
      = mode_to_load_map[mode]
      = mode_to_store_map[mode] = neverneverland;
      
#define DEF_MODEMAP(SYM, CODE, UCODE, CONST, LOAD, STORE) \
  mode_to_const_map[(int) SYM] = CONST; \
  mode_to_load_map[(int) SYM] = LOAD; \
  mode_to_store_map[(int) SYM] = STORE;

#include "modemap.def"
#undef DEF_MODEMAP
}

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
  insn = emit_insn (gen_rtx (SET, 0, NULL_RTX, NULL_RTX));
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
    }

  end_sequence ();
}
      
/* This is run at the start of compiling a function.  */

void
init_expr ()
{
  init_queue ();

  pending_stack_adjust = 0;
  inhibit_defer_pop = 0;
  saveregs_value = 0;
  apply_args_value = 0;
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
  p->saveregs_value = saveregs_value;
  p->apply_args_value = apply_args_value;
  p->forced_labels = forced_labels;

  pending_stack_adjust = 0;
  inhibit_defer_pop = 0;
  saveregs_value = 0;
  apply_args_value = 0;
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
  saveregs_value = p->saveregs_value;
  apply_args_value = p->apply_args_value;
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
      /* A special hack for read access to (MEM (QUEUED ...)) to facilitate
	 use of autoincrement.  Make a copy of the contents of the memory
	 location rather than a copy of the address, but not if the value is
	 of mode BLKmode.  Don't modify X in place since it might be
	 shared.  */
      if (code == MEM && GET_MODE (x) != BLKmode
	  && GET_CODE (XEXP (x, 0)) == QUEUED && !modify)
	{
	  register rtx y = XEXP (x, 0);
	  register rtx new = gen_rtx (MEM, GET_MODE (x), QUEUED_VAR (y));

	  MEM_IN_STRUCT_P (new) = MEM_IN_STRUCT_P (x);
	  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (x);
	  MEM_VOLATILE_P (new) = MEM_VOLATILE_P (x);

	  if (QUEUED_INSN (y))
	    {
	      register rtx temp = gen_reg_rtx (GET_MODE (new));
	      emit_insn_before (gen_move_insn (temp, new),
				QUEUED_INSN (y));
	      return temp;
	    }
	  return new;
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
      rtx value;

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

      value = emit_library_call_value (libcall, NULL_RTX, 1, to_mode,
				       1, from, from_mode);
      emit_move_insn (to, value);
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
	    emit_insn (gen_rtx (CLOBBER, VOIDmode, to));
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
			      gen_rtx (equiv_code, to_mode, copy_rtx (from)));
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

  /* Handle pointer conversion */			/* SPEE 900220 */
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
	  if (HAVE_extendpsisi2)
	    {
	      emit_unop_insn (CODE_FOR_extendpsisi2, to, from, UNKNOWN);
	      return;
	    }
#endif /* HAVE_extendpsisi2 */
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
	  emit_unop_insn (code, to, from, equiv_code);
	  return;
	}
      else
	{
	  enum machine_mode intermediate;

	  /* Search for a mode to convert via.  */
	  for (intermediate = from_mode; intermediate != VOIDmode;
	       intermediate = GET_MODE_WIDER_MODE (intermediate))
	    if (((can_extend_p (to_mode, intermediate, unsignedp)
		  != CODE_FOR_nothing)
		 || (GET_MODE_SIZE (to_mode) < GET_MODE_SIZE (intermediate)
		     && TRULY_NOOP_TRUNCATION (to_mode, intermediate)))
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
  register rtx temp;

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

	  return GEN_INT (val);
	}

      return gen_lowpart (mode, x);
    }

  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

/* Generate several move instructions to copy LEN bytes
   from block FROM to block TO.  (These are MEM rtx's with BLKmode).
   The caller must pass FROM and TO
    through protect_from_queue before calling.
   ALIGN (in bytes) is maximum alignment we can assume.  */

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

  data.to_struct = MEM_IN_STRUCT_P (to);
  data.from_struct = MEM_IN_STRUCT_P (from);

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

  if (! SLOW_UNALIGNED_ACCESS
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
  if (data.len > 0)
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

  if (! SLOW_UNALIGNED_ACCESS
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
	     : copy_rtx (change_address (data->to, mode,
					 plus_constant (data->to_addr,
							data->offset))));
      MEM_IN_STRUCT_P (to1) = data->to_struct;

      from1
	= (data->autinc_from
	   ? gen_rtx (MEM, mode, data->from_addr)
	   : copy_rtx (change_address (data->from, mode,
				       plus_constant (data->from_addr,
						      data->offset))));
      MEM_IN_STRUCT_P (from1) = data->from_struct;

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
   measured in bytes. 

   Return the address of the new block, if memcpy is called and returns it,
   0 otherwise.  */

rtx
emit_block_move (x, y, size, align)
     rtx x, y;
     rtx size;
     int align;
{
  rtx retval = 0;

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
		 returned by the macro, it will definitely be less than the
		 actual mode mask.  */
	      && ((GET_CODE (size) == CONST_INT
		   && ((unsigned HOST_WIDE_INT) INTVAL (size)
		       <= (GET_MODE_MASK (mode) >> 1)))
		  || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
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
		  return 0;
		}
	      else
		delete_insns_since (last);
	    }
	}

#ifdef TARGET_MEM_FUNCTIONS
      retval
	= emit_library_call_value (memcpy_libfunc, NULL_RTX, 0,
				   ptr_mode, 3, XEXP (x, 0), Pmode,
				   XEXP (y, 0), Pmode,
				   convert_to_mode (TYPE_MODE (sizetype), size,
						    TREE_UNSIGNED (sizetype)),
				   TYPE_MODE (sizetype));
#else
      emit_library_call (bcopy_libfunc, 0,
			 VOIDmode, 3, XEXP (y, 0), Pmode,
			 XEXP (x, 0), Pmode,
			 convert_to_mode (TYPE_MODE (integer_type_node), size,
					  TREE_UNSIGNED (integer_type_node)),
			 TYPE_MODE (integer_type_node));
#endif
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
  rtx pat, last;

  if (nregs == 0)
    return;

  if (CONSTANT_P (x) && ! LEGITIMATE_CONSTANT_P (x))
    x = validize_mem (force_const_mem (mode, x));

  /* See if the machine can do this with a load multiple insn.  */
#ifdef HAVE_load_multiple
  if (HAVE_load_multiple)
    {
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
    }
#endif

  for (i = 0; i < nregs; i++)
    emit_move_insn (gen_rtx (REG, word_mode, regno + i),
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
  rtx pat, last;
  enum machine_mode mode;

  /* If SIZE is that of a mode no bigger than a word, just use that
     mode's store operation.  */
  if (size <= UNITS_PER_WORD
      && (mode = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0)) != BLKmode)
    {
      emit_move_insn (change_address (x, mode, NULL),
		      gen_rtx (REG, mode, regno));
      return;
    }
    
  /* Blocks smaller than a word on a BYTES_BIG_ENDIAN machine must be aligned
     to the left before storing to memory.  Note that the previous test
     doesn't handle all cases (e.g. SIZE == 3).  */
  if (size < UNITS_PER_WORD && BYTES_BIG_ENDIAN)
    {
      rtx tem = operand_subword (x, 0, 1, BLKmode);
      rtx shift;

      if (tem == 0)
	abort ();

      shift = expand_shift (LSHIFT_EXPR, word_mode,
			    gen_rtx (REG, word_mode, regno),
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
      pat = gen_store_multiple (x, gen_rtx (REG, word_mode, regno),
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

      emit_move_insn (tem, gen_rtx (REG, word_mode, regno + i));
    }
}

/* Emit code to move a block Y to a block X, where X is non-consecutive
   registers represented by a PARALLEL.  */

void
emit_group_load (x, y)
     rtx x, y;
{
  rtx target_reg, source;
  int i;

  if (GET_CODE (x) != PARALLEL)
    abort ();

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (x, 0, 0), 0))
    i = 0;
  else
    i = 1;

  for (; i < XVECLEN (x, 0); i++)
    {
      rtx element = XVECEXP (x, 0, i);

      target_reg = XEXP (element, 0);

      if (GET_CODE (y) == MEM)
	source = change_address (y, GET_MODE (target_reg),
				 plus_constant (XEXP (y, 0),
						INTVAL (XEXP (element, 1))));
      else if (XEXP (element, 1) == const0_rtx)
	{
	  if (GET_MODE (target_reg) == GET_MODE (y))
	    source = y;
	  /* Allow for the target_reg to be smaller than the input register
	     to allow for AIX with 4 DF arguments after a single SI arg.  The
	     last DF argument will only load 1 word into the integer registers,
	     but load a DF value into the float registers.  */
	  else if ((GET_MODE_SIZE (GET_MODE (target_reg))
		    <= GET_MODE_SIZE (GET_MODE (y)))
		   && GET_MODE (target_reg) == word_mode)
	    /* This might be a const_double, so we can't just use SUBREG.  */
	    source = operand_subword (y, 0, 0, VOIDmode);
	  else if (GET_MODE_SIZE (GET_MODE (target_reg))
		   == GET_MODE_SIZE (GET_MODE (y)))
	    source = gen_lowpart (GET_MODE (target_reg), y);
	  else
	    abort ();	    
	}
      else
	abort ();

      emit_move_insn (target_reg, source);
    }
}

/* Emit code to move a block Y to a block X, where Y is non-consecutive
   registers represented by a PARALLEL.  */

void
emit_group_store (x, y)
     rtx x, y;
{
  rtx source_reg, target;
  int i;

  if (GET_CODE (y) != PARALLEL)
    abort ();

  /* Check for a NULL entry, used to indicate that the parameter goes
     both on the stack and in registers.  */
  if (XEXP (XVECEXP (y, 0, 0), 0))
    i = 0;
  else
    i = 1;

  for (; i < XVECLEN (y, 0); i++)
    {
      rtx element = XVECEXP (y, 0, i);

      source_reg = XEXP (element, 0);

      if (GET_CODE (x) == MEM)
	target = change_address (x, GET_MODE (source_reg),
				 plus_constant (XEXP (x, 0),
						INTVAL (XEXP (element, 1))));
      else if (XEXP (element, 1) == const0_rtx)
	{
	  target = x;
	  if (GET_MODE (target) != GET_MODE (source_reg))
	    target = gen_lowpart (GET_MODE (source_reg), target);
	}
      else
	abort ();

      emit_move_insn (target, source_reg);
    }
}

/* Add a USE expression for REG to the (possibly empty) list pointed
   to by CALL_FUSAGE.  REG must denote a hard register.  */

void
use_reg (call_fusage, reg)
     rtx *call_fusage, reg;
{
  if (GET_CODE (reg) != REG
      || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    abort();

  *call_fusage
    = gen_rtx (EXPR_LIST, VOIDmode,
	       gen_rtx (USE, VOIDmode, reg), *call_fusage);
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
    use_reg (call_fusage, gen_rtx (REG, reg_raw_mode[regno + i], regno + i));
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

/* Generate several move instructions to clear LEN bytes of block TO.
   (A MEM rtx with BLKmode).   The caller must pass TO through
   protect_from_queue before calling. ALIGN (in bytes) is maximum alignment
   we can assume.  */

static void
clear_by_pieces (to, len, align)
     rtx to;
     int len, align;
{
  struct clear_by_pieces data;
  rtx to_addr = XEXP (to, 0);
  int max_size = MOVE_MAX + 1;

  data.offset = 0;
  data.to_addr = to_addr;
  data.to = to;
  data.autinc_to
    = (GET_CODE (to_addr) == PRE_INC || GET_CODE (to_addr) == PRE_DEC
       || GET_CODE (to_addr) == POST_INC || GET_CODE (to_addr) == POST_DEC);

  data.explicit_inc_to = 0;
  data.reverse
    = (GET_CODE (to_addr) == PRE_DEC || GET_CODE (to_addr) == POST_DEC);
  if (data.reverse) data.offset = len;
  data.len = len;

  data.to_struct = MEM_IN_STRUCT_P (to);

  /* If copying requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!data.autinc_to
      && move_by_pieces_ninsns (len, align) > 2)
    {
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

  if (! SLOW_UNALIGNED_ACCESS
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
	clear_by_pieces_1 (GEN_FCN (icode), mode, &data);

      max_size = GET_MODE_SIZE (mode);
    }

  /* The code above should have handled everything.  */
  if (data.len != 0)
    abort ();
}

/* Subroutine of clear_by_pieces.  Clear as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
clear_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) ();
     enum machine_mode mode;
     struct clear_by_pieces *data;
{
  register int size = GET_MODE_SIZE (mode);
  register rtx to1;

  while (data->len >= size)
    {
      if (data->reverse) data->offset -= size;

      to1 = (data->autinc_to
	     ? gen_rtx (MEM, mode, data->to_addr)
	     : copy_rtx (change_address (data->to, mode,
					 plus_constant (data->to_addr,
							data->offset))));
      MEM_IN_STRUCT_P (to1) = data->to_struct;

#ifdef HAVE_PRE_DECREMENT
      if (data->explicit_inc_to < 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (-size)));
#endif

      emit_insn ((*genfun) (to1, const0_rtx));
#ifdef HAVE_POST_INCREMENT
      if (data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to_addr, GEN_INT (size)));
#endif

      if (! data->reverse) data->offset += size;

      data->len -= size;
    }
}

/* Write zeros through the storage of OBJECT.
   If OBJECT has BLKmode, SIZE is its length in bytes and ALIGN is
   the maximum alignment we can is has, measured in bytes.

   If we call a function that returns the length of the block, return it.  */

rtx
clear_storage (object, size, align)
     rtx object;
     rtx size;
     int align;
{
  rtx retval = 0;

  if (GET_MODE (object) == BLKmode)
    {
      object = protect_from_queue (object, 1);
      size = protect_from_queue (size, 0);

      if (GET_CODE (size) == CONST_INT
	  && (move_by_pieces_ninsns (INTVAL (size), align) < MOVE_RATIO))
	clear_by_pieces (object, INTVAL (size), align);

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
	      enum insn_code code = clrstr_optab[(int) mode];

	      if (code != CODE_FOR_nothing
		  /* We don't need MODE to be narrower than
		     BITS_PER_HOST_WIDE_INT here because if SIZE is less than
		     the mode mask, as it is returned by the macro, it will
		     definitely be less than the actual mode mask.  */
		  && ((GET_CODE (size) == CONST_INT
		       && ((unsigned HOST_WIDE_INT) INTVAL (size)
			   <= (GET_MODE_MASK (mode) >> 1)))
		      || GET_MODE_BITSIZE (mode) >= BITS_PER_WORD)
		  && (insn_operand_predicate[(int) code][0] == 0
		      || (*insn_operand_predicate[(int) code][0]) (object,
								   BLKmode))
		  && (insn_operand_predicate[(int) code][2] == 0
		      || (*insn_operand_predicate[(int) code][2]) (opalign,
								   VOIDmode)))
		{
		  rtx op1;
		  rtx last = get_last_insn ();
		  rtx pat;

		  op1 = convert_to_mode (mode, size, 1);
		  if (insn_operand_predicate[(int) code][1] != 0
		      && ! (*insn_operand_predicate[(int) code][1]) (op1,
								     mode))
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


#ifdef TARGET_MEM_FUNCTIONS
	  retval
	    = emit_library_call_value (memset_libfunc, NULL_RTX, 0,
				       ptr_mode, 3,
				       XEXP (object, 0), Pmode,
				       const0_rtx,
				       TYPE_MODE (integer_type_node),
				       convert_to_mode
				       (TYPE_MODE (sizetype), size,
					TREE_UNSIGNED (sizetype)),
				       TYPE_MODE (sizetype));
#else
	  emit_library_call (bzero_libfunc, 0,
			     VOIDmode, 2,
			     XEXP (object, 0), Pmode,	
			     convert_to_mode
			     (TYPE_MODE (integer_type_node), size,
			      TREE_UNSIGNED (integer_type_node)),
			     TYPE_MODE (integer_type_node));
#endif
	}
    }
  else
    emit_move_insn (object, CONST0_RTX (GET_MODE (object)));

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

  return emit_move_insn_1 (x, y);
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
  int i;

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
      rtx insns;

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
		     (gen_rtx (MEM, submode, (XEXP (x, 0))),
		      gen_imagpart (submode, y)));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx (MEM, submode, (XEXP (x, 0))),
		      gen_realpart (submode, y)));
#else
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx (MEM, submode, (XEXP (x, 0))),
		      gen_realpart (submode, y)));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_rtx (MEM, submode, (XEXP (x, 0))),
		      gen_imagpart (submode, y)));
#endif
	}
      else
	{
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_realpart (submode, x), gen_realpart (submode, y)));
	  emit_insn (GEN_FCN (mov_optab->handlers[(int) submode].insn_code)
		     (gen_imagpart (submode, x), gen_imagpart (submode, y)));
	}

      return get_last_insn ();
    }

  /* This will handle any multi-word mode that lacks a move_insn pattern.
     However, you will get better code if you define such patterns,
     even if they must turn into multiple assembler instructions.  */
  else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    {
      rtx last_insn = 0;
      rtx insns;
      
#ifdef PUSH_ROUNDING

      /* If X is a push on the stack, do the push now and replace
	 X with a reference to the stack pointer.  */
      if (push_operand (x, GET_MODE (x)))
	{
	  anti_adjust_stack (GEN_INT (GET_MODE_SIZE (GET_MODE (x))));
	  x = change_address (x, VOIDmode, stack_pointer_rtx);
	}
#endif
			     
      /* Show the output dies here.  */
      if (x != y)
        emit_insn (gen_rtx (CLOBBER, VOIDmode, x));

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

  size = convert_modes (Pmode, ptr_mode, size, 1);
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

/* Return an rtx for the address of the beginning of a as-if-it-was-pushed
   block of SIZE bytes.  */

static rtx
get_push_address (size)
	int size;
{
  register rtx temp;

  if (STACK_PUSH_CODE == POST_DEC)
    temp = gen_rtx (PLUS, Pmode, stack_pointer_rtx, GEN_INT (size));
  else if (STACK_PUSH_CODE == POST_INC)
    temp = gen_rtx (MINUS, Pmode, stack_pointer_rtx, GEN_INT (size));
  else
    temp = stack_pointer_rtx;

  return copy_to_reg (temp);
}

/* Generate code to push X onto the stack, assuming it has mode MODE and
   type TYPE.
   MODE is redundant except when X is a CONST_INT (since they don't
   carry mode info).
   SIZE is an rtx for the size of data to be copied (in bytes),
   needed only if X is BLKmode.

   ALIGN (in bytes) is maximum alignment we can assume.

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
	  && ((! SLOW_UNALIGNED_ACCESS)
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

	  if (flag_check_memory_usage && ! in_check_memory_usage)
	    {
	      rtx temp;
	      
	      in_check_memory_usage = 1;
	      temp = get_push_address (INTVAL(size) - used);
	      if (GET_CODE (x) == MEM && type && AGGREGATE_TYPE_P (type))
		emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
				   temp, ptr_mode,
				   XEXP (xinner, 0), ptr_mode,
				   GEN_INT (INTVAL(size) - used),
				   TYPE_MODE (sizetype));
	      else
		emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
				   temp, ptr_mode,
			 	   GEN_INT (INTVAL(size) - used),
				   TYPE_MODE (sizetype),
				   GEN_INT (MEMORY_USE_RW),
				   TYPE_MODE (integer_type_node));
	      in_check_memory_usage = 0;
	    }
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
	  if (flag_check_memory_usage && ! in_check_memory_usage)
	    {
	      rtx target;
	      
	      in_check_memory_usage = 1;
	      target = copy_to_reg (temp);
	      if (GET_CODE (x) == MEM && type && AGGREGATE_TYPE_P (type))
		emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
				   target, ptr_mode,
				   XEXP (xinner, 0), ptr_mode,
				   size, TYPE_MODE (sizetype));
	      else
	        emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
				   target, ptr_mode,
			 	   size, TYPE_MODE (sizetype),
				   GEN_INT (MEMORY_USE_RW),
				   TYPE_MODE (integer_type_node));
	      in_check_memory_usage = 0;
	    }

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
	      rtx pat = gen_movstrqi (gen_rtx (MEM, BLKmode, temp),
				      xinner, size, GEN_INT (align));
	      if (pat != 0)
		{
		  emit_insn (pat);
		  goto ret;
		}
	    }
#endif
#ifdef HAVE_movstrhi
	  if (HAVE_movstrhi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_BITSIZE (HImode) - 1))))
	    {
	      rtx pat = gen_movstrhi (gen_rtx (MEM, BLKmode, temp),
				      xinner, size, GEN_INT (align));
	      if (pat != 0)
		{
		  emit_insn (pat);
		  goto ret;
		}
	    }
#endif
#ifdef HAVE_movstrsi
	  if (HAVE_movstrsi)
	    {
	      rtx pat = gen_movstrsi (gen_rtx (MEM, BLKmode, temp),
				      xinner, size, GEN_INT (align));
	      if (pat != 0)
		{
		  emit_insn (pat);
		  goto ret;
		}
	    }
#endif
#ifdef HAVE_movstrdi
	  if (HAVE_movstrdi)
	    {
	      rtx pat = gen_movstrdi (gen_rtx (MEM, BLKmode, temp),
				      xinner, size, GEN_INT (align));
	      if (pat != 0)
		{
		  emit_insn (pat);
		  goto ret;
		}
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
			     convert_to_mode (TYPE_MODE (sizetype),
					      size, TREE_UNSIGNED (sizetype)),
			     TYPE_MODE (sizetype));
#else
	  emit_library_call (bcopy_libfunc, 0,
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
      rtx target = NULL_RTX;

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
	{
	  if (GET_CODE (args_so_far) == CONST_INT)
	    addr
	      = memory_address (mode,
				plus_constant (args_addr, 
					       INTVAL (args_so_far)));
          else
	    addr = memory_address (mode, gen_rtx (PLUS, Pmode, args_addr,
						  args_so_far));
	  target = addr;
	}

      emit_move_insn (gen_rtx (MEM, mode, addr), x);

      if (flag_check_memory_usage && ! in_check_memory_usage)
	{
	  in_check_memory_usage = 1;
	  if (target == 0)
	    target = get_push_address (GET_MODE_SIZE (mode));

	  if (GET_CODE (x) == MEM && type && AGGREGATE_TYPE_P (type))
	    emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
			       target, ptr_mode,
			       XEXP (x, 0), ptr_mode,
			       GEN_INT (GET_MODE_SIZE (mode)),
			       TYPE_MODE (sizetype));
	  else
	    emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			       target, ptr_mode,
			       GEN_INT (GET_MODE_SIZE (mode)),
			       TYPE_MODE (sizetype),
			       GEN_INT (MEMORY_USE_RW),
			       TYPE_MODE (integer_type_node));
	  in_check_memory_usage = 0;
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
	emit_group_load (reg, x);
      else
	move_block_to_reg (REGNO (reg), x, partial, mode);
    }

  if (extra && args_addr == 0 && where_pad == stack_direction)
    anti_adjust_stack (GEN_INT (extra));
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
     int suggest_reg;
{
  register rtx to_rtx = 0;
  rtx result;

  /* Don't crash if the lhs of the assignment was erroneous.  */

  if (TREE_CODE (to) == ERROR_MARK)
    {
      result = expand_expr (from, NULL_RTX, VOIDmode, 0);
      return want_value ? result : NULL_RTX;
    }

  if (output_bytecode)
    {
      tree dest_innermost;

      bc_expand_expr (from);
      bc_emit_instruction (duplicate);

      dest_innermost = bc_expand_address (to);

      /* Can't deduce from TYPE that we're dealing with a bitfield, so
	 take care of it here.  */

      bc_store_memory (TREE_TYPE (to), dest_innermost);
      return NULL;
    }

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.
     Assignment of an array element at a constant index, and assignment of
     an array element in an unaligned packed structure field, has the same
     problem.  */

  if (TREE_CODE (to) == COMPONENT_REF || TREE_CODE (to) == BIT_FIELD_REF
      || TREE_CODE (to) == ARRAY_REF)
    {
      enum machine_mode mode1;
      int bitsize;
      int bitpos;
      tree offset;
      int unsignedp;
      int volatilep = 0;
      tree tem;
      int alignment;

      push_temp_slots ();
      tem = get_inner_reference (to, &bitsize, &bitpos, &offset, &mode1,
				 &unsignedp, &volatilep, &alignment);

      /* If we are going to use store_bit_field and extract_bit_field,
	 make sure to_rtx will be safe for multiple use.  */

      if (mode1 == VOIDmode && want_value)
	tem = stabilize_reference (tem);

      to_rtx = expand_expr (tem, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_DONT);
      if (offset != 0)
	{
	  rtx offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, 0);

	  if (GET_CODE (to_rtx) != MEM)
	    abort ();
	  to_rtx = change_address (to_rtx, VOIDmode,
				   gen_rtx (PLUS, ptr_mode, XEXP (to_rtx, 0),
					    force_reg (ptr_mode, offset_rtx)));
	}
      if (volatilep)
	{
	  if (GET_CODE (to_rtx) == MEM)
	    {
	      /* When the offset is zero, to_rtx is the address of the
		 structure we are storing into, and hence may be shared.
		 We must make a new MEM before setting the volatile bit.  */
	      if (offset == 0)
		to_rtx = copy_rtx (to_rtx);

	      MEM_VOLATILE_P (to_rtx) = 1;
	    }
#if 0  /* This was turned off because, when a field is volatile
	  in an object which is not volatile, the object may be in a register,
	  and then we would abort over here.  */
	  else
	    abort ();
#endif
	}

      if (TREE_CODE (to) == COMPONENT_REF
	  && TREE_READONLY (TREE_OPERAND (to, 1)))
	{
	  if (offset == 0)
	    to_rtx = copy_rtx (to_rtx);

	  RTX_UNCHANGING_P (to_rtx) = 1;
	}

      /* Check the access.  */
      if (flag_check_memory_usage && GET_CODE (to_rtx) == MEM)
	{
	  rtx to_addr;
	  int size;
	  int best_mode_size;
	  enum machine_mode best_mode;

	  best_mode = get_best_mode (bitsize, bitpos,
	  			     TYPE_ALIGN (TREE_TYPE (tem)),
	  			     mode1, volatilep);
	  if (best_mode == VOIDmode)
	    best_mode = QImode;

	  best_mode_size = GET_MODE_BITSIZE (best_mode);
	  to_addr = plus_constant (XEXP (to_rtx, 0), (bitpos / BITS_PER_UNIT));
	  size = CEIL ((bitpos % best_mode_size) + bitsize, best_mode_size);
	  size *= GET_MODE_SIZE (best_mode);

	  /* Check the access right of the pointer.  */
	  if (size)
	    emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
			       to_addr, ptr_mode,
			       GEN_INT (size), TYPE_MODE (sizetype),
			       GEN_INT (MEMORY_USE_WO),
			       TYPE_MODE (integer_type_node));
	}

      result = store_field (to_rtx, bitsize, bitpos, mode1, from,
			    (want_value
			     /* Spurious cast makes HPUX compiler happy.  */
			     ? (enum machine_mode) TYPE_MODE (TREE_TYPE (to))
			     : VOIDmode),
			    unsignedp,
			    /* Required alignment of containing datum.  */
			    alignment,
			    int_size_in_bytes (TREE_TYPE (tem)));
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

     Don't do this if TO is a VAR_DECL whose DECL_RTL is REG since it might be
     a promoted variable where the zero- or sign- extension needs to be done.
     Handling this in the normal way is safe because no computation is done
     before the call.  */
  if (TREE_CODE (from) == CALL_EXPR && ! aggregate_value_p (from)
      && TREE_CODE (TYPE_SIZE (TREE_TYPE (from))) == INTEGER_CST
      && ! (TREE_CODE (to) == VAR_DECL && GET_CODE (DECL_RTL (to)) == REG))
    {
      rtx value;

      push_temp_slots ();
      value = expand_expr (from, NULL_RTX, VOIDmode, 0);
      if (to_rtx == 0)
	to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_WO);

      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      if (GET_CODE (to_rtx) == PARALLEL)
	emit_group_load (to_rtx, value);
      else if (GET_MODE (to_rtx) == BLKmode)
	emit_block_move (to_rtx, value, expr_size (from),
			 TYPE_ALIGN (TREE_TYPE (from)) / BITS_PER_UNIT);
      else
	emit_move_insn (to_rtx, value);
      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : NULL_RTX;
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_WO);

  /* Don't move directly into a return register.  */
  if (TREE_CODE (to) == RESULT_DECL && GET_CODE (to_rtx) == REG)
    {
      rtx temp;

      push_temp_slots ();
      temp = expand_expr (from, 0, GET_MODE (to_rtx), 0);
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
      from_rtx = expand_expr (from, NULL_RTX, VOIDmode,
			      EXPAND_MEMORY_USE_DONT);

      /* Copy the rights of the bitmap.  */
      if (flag_check_memory_usage)
	emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
			   XEXP (to_rtx, 0), ptr_mode,
			   XEXP (from_rtx, 0), ptr_mode,
			   convert_to_mode (TYPE_MODE (sizetype),
					    size, TREE_UNSIGNED (sizetype)),
			   TYPE_MODE (sizetype));

#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (memcpy_libfunc, 0,
			 VOIDmode, 3, XEXP (to_rtx, 0), Pmode,
			 XEXP (from_rtx, 0), Pmode,
			 convert_to_mode (TYPE_MODE (sizetype),
					  size, TREE_UNSIGNED (sizetype)),
			 TYPE_MODE (sizetype));
#else
      emit_library_call (bcopy_libfunc, 0,
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
     register tree exp;
     register rtx target;
     int want_value;
{
  register rtx temp;
  int dont_return_target = 0;

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
  else if (want_value && GET_CODE (target) == MEM && ! MEM_VOLATILE_P (target)
	   && GET_MODE (target) != BLKmode)
    /* If target is in memory and caller wants value in a register instead,
       arrange that.  Pass TARGET as target for expand_expr so that,
       if EXP is another assignment, WANT_VALUE will be nonzero for it.
       We know expand_expr will not use the target in that case.
       Don't do this if TARGET is volatile because we are supposed
       to write it and then read it.  */
    {
      temp = expand_expr (exp, cse_not_expected ? NULL_RTX : target,
			  GET_MODE (target), 0);
      if (GET_MODE (temp) != BLKmode && GET_MODE (temp) != VOIDmode)
	temp = copy_to_reg (temp);
      dont_return_target = 1;
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
  else if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is an scalar in a register that is stored in a wider mode
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    {
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
	}
	 
      temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);

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
	temp = convert_modes (GET_MODE (SUBREG_REG (target)),
			      TYPE_MODE (TREE_TYPE (exp)), temp,
			      SUBREG_PROMOTED_UNSIGNED_P (target));

      convert_move (SUBREG_REG (target), temp,
		    SUBREG_PROMOTED_UNSIGNED_P (target));
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

  if (flag_check_memory_usage
      && GET_CODE (target) == MEM
      && AGGREGATE_TYPE_P (TREE_TYPE (exp)))
    {
      if (GET_CODE (temp) == MEM)
        emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
			   XEXP (target, 0), ptr_mode,
			   XEXP (temp, 0), ptr_mode,
			   expr_size (exp), TYPE_MODE (sizetype));
      else
        emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
			   XEXP (target, 0), ptr_mode, 
			   expr_size (exp), TYPE_MODE (sizetype),
			   GEN_INT (MEMORY_USE_WO), 
			   TYPE_MODE (integer_type_node));
    }

  /* If value was not generated in the target, store it there.
     Convert the value to TARGET's type first if nec.  */

  if (! rtx_equal_p (temp, target) && TREE_CODE (exp) != ERROR_MARK)
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
	  rtx addr;

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
		= size_binop (MIN_EXPR,
			      make_tree (sizetype, size),
			      convert (sizetype,
				       build_int_2 (TREE_STRING_LENGTH (exp), 0)));
	      rtx copy_size_rtx = expand_expr (copy_size, NULL_RTX,
					       VOIDmode, 0);
	      rtx label = 0;

	      /* Copy that much.  */
	      emit_block_move (target, temp, copy_size_rtx,
			       TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);

	      /* Figure out how much is left in TARGET that we have to clear.
		 Do all calculations in ptr_mode.  */

	      addr = XEXP (target, 0);
	      addr = convert_modes (ptr_mode, Pmode, addr, 1);

	      if (GET_CODE (copy_size_rtx) == CONST_INT)
		{
		  addr = plus_constant (addr, TREE_STRING_LENGTH (exp));
		  size = plus_constant (size, - TREE_STRING_LENGTH (exp));
		}
	      else
		{
		  addr = force_reg (ptr_mode, addr);
		  addr = expand_binop (ptr_mode, add_optab, addr,
				       copy_size_rtx, NULL_RTX, 0,
				       OPTAB_LIB_WIDEN);

		  size = expand_binop (ptr_mode, sub_optab, size,
				       copy_size_rtx, NULL_RTX, 0,
				       OPTAB_LIB_WIDEN);

		  emit_cmp_insn (size, const0_rtx, LT, NULL_RTX,
				 GET_MODE (size), 0, 0);
		  label = gen_label_rtx ();
		  emit_jump_insn (gen_blt (label));
		}

	      if (size != const0_rtx)
		{
		  /* Be sure we can write on ADDR.  */
		  if (flag_check_memory_usage)
		    emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
				       addr, ptr_mode,
				       size, TYPE_MODE (sizetype),
 				       GEN_INT (MEMORY_USE_WO), 
				       TYPE_MODE (integer_type_node));
#ifdef TARGET_MEM_FUNCTIONS
		  emit_library_call (memset_libfunc, 0, VOIDmode, 3,
				     addr, ptr_mode,
				     const0_rtx, TYPE_MODE (integer_type_node),
				     convert_to_mode (TYPE_MODE (sizetype),
						      size,
						      TREE_UNSIGNED (sizetype)),
				     TYPE_MODE (sizetype));
#else
		  emit_library_call (bzero_libfunc, 0, VOIDmode, 2,
				     addr, ptr_mode,
				     convert_to_mode (TYPE_MODE (integer_type_node),
						      size,
						      TREE_UNSIGNED (integer_type_node)),
				     TYPE_MODE (integer_type_node));
#endif
		}

	      if (label)
		emit_label (label);
	    }
	}
      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      else if (GET_CODE (target) == PARALLEL)
	emit_group_load (target, temp);
      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
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
      return is_zeros_p (TREE_OPERAND (exp, 0));

    case INTEGER_CST:
      return TREE_INT_CST_LOW (exp) == 0 && TREE_INT_CST_HIGH (exp) == 0;

    case COMPLEX_CST:
      return
	is_zeros_p (TREE_REALPART (exp)) && is_zeros_p (TREE_IMAGPART (exp));

    case REAL_CST:
      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (exp), dconst0);

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

   This provides a recursive shortcut back to store_constructor when it isn't
   necessary to go through store_field.  This is so that we can pass through
   the cleared field to let store_constructor know that we may not have to
   clear a substructure if the outer structure has already been cleared.  */

static void
store_constructor_field (target, bitsize, bitpos,
			 mode, exp, type, cleared)
     rtx target;
     int bitsize, bitpos;
     enum machine_mode mode;
     tree exp, type;
     int cleared;
{
  if (TREE_CODE (exp) == CONSTRUCTOR
      && bitpos % BITS_PER_UNIT == 0
      /* If we have a non-zero bitpos for a register target, then we just
	 let store_field do the bitfield handling.  This is unlikely to
	 generate unnecessary clear instructions anyways.  */
      && (bitpos == 0 || GET_CODE (target) == MEM))
    {
      if (bitpos != 0)
	target = change_address (target, VOIDmode,
				 plus_constant (XEXP (target, 0),
						bitpos / BITS_PER_UNIT));
      store_constructor (exp, target, cleared);
    }
  else
    store_field (target, bitsize, bitpos, mode, exp,
		 VOIDmode, 0, TYPE_ALIGN (type) / BITS_PER_UNIT,
		 int_size_in_bytes (type));
}

/* Store the value of constructor EXP into the rtx TARGET.
   TARGET is either a REG or a MEM.
   CLEARED is true if TARGET is known to have been zero'd.  */

static void
store_constructor (exp, target, cleared)
     tree exp;
     rtx target;
     int cleared;
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
      store_constructor (exp, temp, 0);
      emit_move_insn (target, temp);
      return;
    }
#endif

  if (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
      || TREE_CODE (type) == QUAL_UNION_TYPE)
    {
      register tree elt;

      /* Inform later passes that the whole union value is dead.  */
      if (TREE_CODE (type) == UNION_TYPE
	  || TREE_CODE (type) == QUAL_UNION_TYPE)
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* If we are building a static constructor into a register,
	 set the initial value as zero so we can fold the value into
	 a constant.  But if more than one register is involved,
	 this probably loses.  */
      else if (GET_CODE (target) == REG && TREE_STATIC (exp)
	       && GET_MODE_SIZE (GET_MODE (target)) <= UNITS_PER_WORD)
	{
	  if (! cleared)
	    emit_move_insn (target, CONST0_RTX (GET_MODE (target)));

	  cleared = 1;
	}

      /* If the constructor has fewer fields than the structure
	 or if we are initializing the structure to mostly zeros,
	 clear the whole structure first.  */
      else if ((list_length (CONSTRUCTOR_ELTS (exp))
		!= list_length (TYPE_FIELDS (type)))
	       || mostly_zeros_p (exp))
	{
	  if (! cleared)
	    clear_storage (target, expr_size (exp),
			   TYPE_ALIGN (type) / BITS_PER_UNIT);

	  cleared = 1;
	}
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
	  int bitpos = 0;
	  int unsignedp;
	  tree pos, constant = 0, offset = 0;
	  rtx to_rtx = target;

	  /* Just ignore missing fields.
	     We cleared the whole structure, above,
	     if any fields are missing.  */
	  if (field == 0)
	    continue;

	  if (cleared && is_zeros_p (TREE_VALUE (elt)))
	    continue;

	  bitsize = TREE_INT_CST_LOW (DECL_SIZE (field));
	  unsignedp = TREE_UNSIGNED (field);
	  mode = DECL_MODE (field);
	  if (DECL_BIT_FIELD (field))
	    mode = VOIDmode;

	  pos = DECL_FIELD_BITPOS (field);
	  if (TREE_CODE (pos) == INTEGER_CST)
	    constant = pos;
	  else if (TREE_CODE (pos) == PLUS_EXPR
		   && TREE_CODE (TREE_OPERAND (pos, 1)) == INTEGER_CST)
	    constant = TREE_OPERAND (pos, 1), offset = TREE_OPERAND (pos, 0);
	  else
	    offset = pos;

	  if (constant)
	    bitpos = TREE_INT_CST_LOW (constant);

	  if (offset)
	    {
	      rtx offset_rtx;

	      if (contains_placeholder_p (offset))
		offset = build (WITH_RECORD_EXPR, sizetype,
				offset, make_tree (TREE_TYPE (exp), target));

	      offset = size_binop (FLOOR_DIV_EXPR, offset,
				   size_int (BITS_PER_UNIT));

	      offset_rtx = expand_expr (offset, NULL_RTX, VOIDmode, 0);
	      if (GET_CODE (to_rtx) != MEM)
		abort ();

	      to_rtx
		= change_address (to_rtx, VOIDmode,
				  gen_rtx (PLUS, ptr_mode, XEXP (to_rtx, 0),
					   force_reg (ptr_mode, offset_rtx)));
	    }
	  if (TREE_READONLY (field))
	    {
	      if (GET_CODE (to_rtx) == MEM)
		to_rtx = copy_rtx (to_rtx);

	      RTX_UNCHANGING_P (to_rtx) = 1;
	    }

	  store_constructor_field (to_rtx, bitsize, bitpos,
				   mode, TREE_VALUE (elt), type, cleared);
	}
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      register tree elt;
      register int i;
      int need_to_clear;
      tree domain = TYPE_DOMAIN (type);
      HOST_WIDE_INT minelt = TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain));
      HOST_WIDE_INT maxelt = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain));
      tree elttype = TREE_TYPE (type);

      /* If the constructor has fewer elements than the array,
         clear the whole array first.  Similarly if this this is
         static constructor of a non-BLKmode object.  */
      if (cleared || (GET_CODE (target) == REG && TREE_STATIC (exp)))
	need_to_clear = 1;
      else
	{
	  HOST_WIDE_INT count = 0, zero_count = 0;
	  need_to_clear = 0;
	  /* This loop is a more accurate version of the loop in
	     mostly_zeros_p (it handles RANGE_EXPR in an index).
	     It is also needed to check for missing elements.  */
	  for (elt = CONSTRUCTOR_ELTS (exp);
	       elt != NULL_TREE;
	       elt = TREE_CHAIN (elt))
	    {
	      tree index = TREE_PURPOSE (elt);
	      HOST_WIDE_INT this_node_count;
	      if (index != NULL_TREE && TREE_CODE (index) == RANGE_EXPR)
		{
		  tree lo_index = TREE_OPERAND (index, 0);
		  tree hi_index = TREE_OPERAND (index, 1);
		  if (TREE_CODE (lo_index) != INTEGER_CST
		      || TREE_CODE (hi_index) != INTEGER_CST)
		    {
		      need_to_clear = 1;
		      break;
		    }
		  this_node_count = TREE_INT_CST_LOW (hi_index)
		    - TREE_INT_CST_LOW (lo_index) + 1;
		}
	      else
		this_node_count = 1;
	      count += this_node_count;
	      if (mostly_zeros_p (TREE_VALUE (elt)))
		zero_count += this_node_count;
	    }
	  /* Clear the entire array first if there are any missing elements,
	     or if the incidence of zero elements is >= 75%.  */
	  if (count < maxelt - minelt + 1
	      || 4 * zero_count >= 3 * count)
	    need_to_clear = 1;
	}
      if (need_to_clear)
	{
	  if (! cleared)
	    clear_storage (target, expr_size (exp),
			   TYPE_ALIGN (type) / BITS_PER_UNIT);
	  cleared = 1;
	}
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
	  tree value = TREE_VALUE (elt);
	  tree index = TREE_PURPOSE (elt);
	  rtx xtarget = target;

	  if (cleared && is_zeros_p (value))
	    continue;

	  mode = TYPE_MODE (elttype);
	  bitsize = GET_MODE_BITSIZE (mode);
	  unsignedp = TREE_UNSIGNED (elttype);

	  if (index != NULL_TREE && TREE_CODE (index) == RANGE_EXPR)
	    {
	      tree lo_index = TREE_OPERAND (index, 0);
	      tree hi_index = TREE_OPERAND (index, 1);
	      rtx index_r, pos_rtx, addr, hi_r, loop_top, loop_end;
	      struct nesting *loop;
	      HOST_WIDE_INT lo, hi, count;
	      tree position;

	      /* If the range is constant and "small", unroll the loop.  */
	      if (TREE_CODE (lo_index) == INTEGER_CST
		  && TREE_CODE (hi_index) == INTEGER_CST
		  && (lo = TREE_INT_CST_LOW (lo_index),
		      hi = TREE_INT_CST_LOW (hi_index),
		      count = hi - lo + 1,
		      (GET_CODE (target) != MEM
		       || count <= 2
		       || (TREE_CODE (TYPE_SIZE (elttype)) == INTEGER_CST
			   && TREE_INT_CST_LOW (TYPE_SIZE (elttype)) * count
			   <= 40 * 8))))
		{
		  lo -= minelt;  hi -= minelt;
		  for (; lo <= hi; lo++)
		    {
		      bitpos = lo * TREE_INT_CST_LOW (TYPE_SIZE (elttype));
		      store_constructor_field (target, bitsize, bitpos,
					       mode, value, type, cleared);
		    }
		}
	      else
		{
		  hi_r = expand_expr (hi_index, NULL_RTX, VOIDmode, 0);
		  loop_top = gen_label_rtx ();
		  loop_end = gen_label_rtx ();

		  unsignedp = TREE_UNSIGNED (domain);

		  index = build_decl (VAR_DECL, NULL_TREE, domain);

		  DECL_RTL (index) = index_r
		    = gen_reg_rtx (promote_mode (domain, DECL_MODE (index),
						 &unsignedp, 0));

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
		  position = size_binop (EXACT_DIV_EXPR, TYPE_SIZE (elttype),
					 size_int (BITS_PER_UNIT));
		  position = size_binop (MULT_EXPR,
					 size_binop (MINUS_EXPR, index,
						     TYPE_MIN_VALUE (domain)),
					 position);
		  pos_rtx = expand_expr (position, 0, VOIDmode, 0);
		  addr = gen_rtx (PLUS, Pmode, XEXP (target, 0), pos_rtx);
		  xtarget = change_address (target, mode, addr);
		  if (TREE_CODE (value) == CONSTRUCTOR)
		    store_constructor (value, xtarget, cleared);
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

		  /* Needed by stupid register allocation. to extend the
		     lifetime of pseudo-regs used by target past the end
		     of the loop.  */
		  emit_insn (gen_rtx (USE, GET_MODE (target), target));
		}
	    }
	  else if ((index != 0 && TREE_CODE (index) != INTEGER_CST)
	      || TREE_CODE (TYPE_SIZE (elttype)) != INTEGER_CST)
	    {
	      rtx pos_rtx, addr;
	      tree position;

	      if (index == 0)
		index = size_int (i);

	      if (minelt)
		index = size_binop (MINUS_EXPR, index,
				    TYPE_MIN_VALUE (domain));
	      position = size_binop (EXACT_DIV_EXPR, TYPE_SIZE (elttype),
				     size_int (BITS_PER_UNIT));
	      position = size_binop (MULT_EXPR, index, position);
	      pos_rtx = expand_expr (position, 0, VOIDmode, 0);
	      addr = gen_rtx (PLUS, Pmode, XEXP (target, 0), pos_rtx);
	      xtarget = change_address (target, mode, addr);
	      store_expr (value, xtarget, 0);
	    }
	  else
	    {
	      if (index != 0)
		bitpos = ((TREE_INT_CST_LOW (index) - minelt)
			  * TREE_INT_CST_LOW (TYPE_SIZE (elttype)));
	      else
		bitpos = (i * TREE_INT_CST_LOW (TYPE_SIZE (elttype)));
	      store_constructor_field (target, bitsize, bitpos,
				       mode, value, type, cleared);
	    }
	}
    }
  /* set constructor assignments */
  else if (TREE_CODE (type) == SET_TYPE)
    {
      tree elt = CONSTRUCTOR_ELTS (exp);
      rtx xtarget = XEXP (target, 0);
      int set_word_size = TYPE_ALIGN (type);
      int nbytes = int_size_in_bytes (type), nbits;
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
      if (elt == NULL_TREE)
	{
	  if (!cleared)
	    clear_storage (target, expr_size (exp),
			   TYPE_ALIGN (type) / BITS_PER_UNIT);
	  return;
	}

      domain_min = convert (sizetype, TYPE_MIN_VALUE (domain));
      domain_max = convert (sizetype, TYPE_MAX_VALUE (domain));
      bitlength = size_binop (PLUS_EXPR,
			      size_binop (MINUS_EXPR, domain_max, domain_min),
			      size_one_node);

      if (nbytes < 0 || TREE_CODE (bitlength) != INTEGER_CST)
	abort ();
      nbits = TREE_INT_CST_LOW (bitlength);

      /* For "small" sets, or "medium-sized" (up to 32 bytes) sets that
	 are "complicated" (more than one range), initialize (the
	 constant parts) by copying from a constant.  */	 
      if (GET_MODE (target) != BLKmode || nbits <= 2 * BITS_PER_WORD
	  || (nbytes <= 32 && TREE_CHAIN (elt) != NULL_TREE))
	{
	  int set_word_size = TYPE_ALIGN (TREE_TYPE (exp));
	  enum machine_mode mode = mode_for_size (set_word_size, MODE_INT, 1);
	  char *bit_buffer = (char *) alloca (nbits);
	  HOST_WIDE_INT word = 0;
	  int bit_pos = 0;
	  int ibit = 0;
	  int offset = 0;  /* In bytes from beginning of set.  */
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
			{
			  to_rtx = plus_constant (XEXP (target, 0), offset);
			  to_rtx = change_address (target, mode, to_rtx);
			}
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
	{
	  /* Don't bother clearing storage if the set is all ones.  */
	  if (TREE_CHAIN (elt) != NULL_TREE
	      || (TREE_PURPOSE (elt) == NULL_TREE
		  ? nbits != 1
		  : (TREE_CODE (TREE_VALUE (elt)) != INTEGER_CST
		     || TREE_CODE (TREE_PURPOSE (elt)) != INTEGER_CST
		     || (TREE_INT_CST_LOW (TREE_VALUE (elt))
			 - TREE_INT_CST_LOW (TREE_PURPOSE (elt)) + 1
			 != nbits))))
	    clear_storage (target, expr_size (exp),
			   TYPE_ALIGN (type) / BITS_PER_UNIT);
	}
	  
      for (; elt != NULL_TREE; elt = TREE_CHAIN (elt))
	{
	  /* start of range of element or NULL */
	  tree startbit = TREE_PURPOSE (elt);
	  /* end of range of element, or element value */
	  tree endbit   = TREE_VALUE (elt);
	  HOST_WIDE_INT startb, endb;
	  rtx  bitlength_rtx, startbit_rtx, endbit_rtx, targetx;

	  bitlength_rtx = expand_expr (bitlength,
			    NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	  /* handle non-range tuple element like [ expr ]  */
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
	      targetx = assign_stack_temp (GET_MODE (target),
					   GET_MODE_SIZE (GET_MODE (target)),
					   0);
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
	      emit_library_call (memset_libfunc, 0,
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
	    {
	      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__setbits"),
				 0, VOIDmode, 4, XEXP (targetx, 0), Pmode,
				 bitlength_rtx, TYPE_MODE (sizetype),
				 startbit_rtx, TYPE_MODE (sizetype),
				 endbit_rtx, TYPE_MODE (sizetype));
	    }
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

  if (TREE_CODE (exp) == ERROR_MARK)
    return const0_rtx;

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

      MEM_IN_STRUCT_P (object) = 1;
      MEM_IN_STRUCT_P (blk_object) = 1;
      PUT_MODE (blk_object, BLKmode);

      if (bitsize != GET_MODE_BITSIZE (GET_MODE (target)))
	emit_move_insn (object, target);

      store_field (blk_object, bitsize, bitpos, mode, exp, VOIDmode, 0,
		   align, total_size);

      /* Even though we aren't returning target, we need to
	 give it the updated value.  */
      emit_move_insn (target, object);

      return blk_object;
    }

  /* If the structure is in a register or if the component
     is a bit field, we cannot use addressing to access it.
     Use bit-field techniques or SUBREG to store in it.  */

  if (mode == VOIDmode
      || (mode != BLKmode && ! direct_store[(int) mode])
      || GET_CODE (target) == REG
      || GET_CODE (target) == SUBREG
      /* If the field isn't aligned enough to store as an ordinary memref,
	 store it as a bit field.  */
      || (SLOW_UNALIGNED_ACCESS
	  && align * BITS_PER_UNIT < GET_MODE_ALIGNMENT (mode))
      || (SLOW_UNALIGNED_ACCESS && bitpos % GET_MODE_ALIGNMENT (mode) != 0))
    {
      rtx temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);

      /* If BITSIZE is narrower than the size of the type of EXP
	 we will be narrowing TEMP.  Normally, what's wanted are the
	 low-order bits.  However, if EXP's type is a record and this is
	 big-endian machine, we want the upper BITSIZE bits.  */
      if (BYTES_BIG_ENDIAN && GET_MODE_CLASS (GET_MODE (temp)) == MODE_INT
	  && bitsize < GET_MODE_BITSIZE (GET_MODE (temp))
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

	  target = change_address (target, VOIDmode,
				   plus_constant (XEXP (target, 0),
						bitpos / BITS_PER_UNIT));

	  emit_block_move (target, temp,
			   GEN_INT ((bitsize + BITS_PER_UNIT - 1)
				    / BITS_PER_UNIT),
			   1);

	  return value_mode == VOIDmode ? const0_rtx : target;
	}

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

      to_rtx = copy_rtx (change_address (target, mode,
					 plus_constant (addr,
							(bitpos
							 / BITS_PER_UNIT))));
      MEM_IN_STRUCT_P (to_rtx) = 1;

      return store_expr (exp, to_rtx, value_mode != VOIDmode);
    }
}

/* Given an expression EXP that may be a COMPONENT_REF, a BIT_FIELD_REF,
   or an ARRAY_REF, look for nested COMPONENT_REFs, BIT_FIELD_REFs, or
   ARRAY_REFs and find the ultimate containing object, which we return.

   We set *PBITSIZE to the size in bits that we want, *PBITPOS to the
   bit position, and *PUNSIGNEDP to the signedness of the field.
   If the position of the field is variable, we store a tree
   giving the variable offset (in units) in *POFFSET.
   This offset is in addition to the bit position.
   If the position is not variable, we store 0 in *POFFSET.
   We set *PALIGNMENT to the alignment in bytes of the address that will be
   computed.  This is the alignment of the thing we return if *POFFSET
   is zero, but can be more less strictly aligned if *POFFSET is nonzero.

   If any of the extraction expressions is volatile,
   we store 1 in *PVOLATILEP.  Otherwise we don't change that.

   If the field is a bit-field, *PMODE is set to VOIDmode.  Otherwise, it
   is a mode that can be used to access the field.  In that case, *PBITSIZE
   is redundant.

   If the field describes a variable-sized object, *PMODE is set to
   VOIDmode and *PBITSIZE is set to -1.  An access cannot be made in
   this case, but the address of the object can be found.   */

tree
get_inner_reference (exp, pbitsize, pbitpos, poffset, pmode,
		     punsignedp, pvolatilep, palignment)
     tree exp;
     int *pbitsize;
     int *pbitpos;
     tree *poffset;
     enum machine_mode *pmode;
     int *punsignedp;
     int *pvolatilep;
     int *palignment;
{
  tree orig_exp = exp;
  tree size_tree = 0;
  enum machine_mode mode = VOIDmode;
  tree offset = integer_zero_node;
  int alignment = BIGGEST_ALIGNMENT;

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
      if (TREE_CODE (exp) == COMPONENT_REF || TREE_CODE (exp) == BIT_FIELD_REF)
	{
	  tree pos = (TREE_CODE (exp) == COMPONENT_REF
		      ? DECL_FIELD_BITPOS (TREE_OPERAND (exp, 1))
		      : TREE_OPERAND (exp, 2));
	  tree constant = integer_zero_node, var = pos;

	  /* If this field hasn't been filled in yet, don't go
	     past it.  This should only happen when folding expressions
	     made during type construction.  */
	  if (pos == 0)
	    break;

	  /* Assume here that the offset is a multiple of a unit.
	     If not, there should be an explicitly added constant.  */
	  if (TREE_CODE (pos) == PLUS_EXPR
	      && TREE_CODE (TREE_OPERAND (pos, 1)) == INTEGER_CST)
	    constant = TREE_OPERAND (pos, 1), var = TREE_OPERAND (pos, 0);
	  else if (TREE_CODE (pos) == INTEGER_CST)
	    constant = pos, var = integer_zero_node;

	  *pbitpos += TREE_INT_CST_LOW (constant);
	  offset = size_binop (PLUS_EXPR, offset,
			       size_binop (EXACT_DIV_EXPR, var,
					   size_int (BITS_PER_UNIT)));
	}

      else if (TREE_CODE (exp) == ARRAY_REF)
	{
	  /* This code is based on the code in case ARRAY_REF in expand_expr
	     below.  We assume here that the size of an array element is
	     always an integral multiple of BITS_PER_UNIT.  */

	  tree index = TREE_OPERAND (exp, 1);
	  tree domain = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (exp, 0)));
	  tree low_bound
	    = domain ? TYPE_MIN_VALUE (domain) : integer_zero_node;
	  tree index_type = TREE_TYPE (index);

	  if (! integer_zerop (low_bound))
	    index = fold (build (MINUS_EXPR, index_type, index, low_bound));

	  if (TYPE_PRECISION (index_type) != TYPE_PRECISION (sizetype))
	    {
	      index = convert (type_for_size (TYPE_PRECISION (sizetype), 0),
			       index);
	      index_type = TREE_TYPE (index);
	    }

	  index = fold (build (MULT_EXPR, index_type, index,
			       convert (index_type,
					TYPE_SIZE (TREE_TYPE (exp)))));

	  if (TREE_CODE (index) == INTEGER_CST
	      && TREE_INT_CST_HIGH (index) == 0)
	    *pbitpos += TREE_INT_CST_LOW (index);
	  else
	    {
	      if (contains_placeholder_p (index))
		index = build (WITH_RECORD_EXPR, sizetype, index, exp);

	      offset = size_binop (PLUS_EXPR, offset,
				   size_binop (FLOOR_DIV_EXPR, index,
					       size_int (BITS_PER_UNIT)));
	    }
	}
      else if (TREE_CODE (exp) != NON_LVALUE_EXPR
	       && ! ((TREE_CODE (exp) == NOP_EXPR
		      || TREE_CODE (exp) == CONVERT_EXPR)
		     && ! (TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE
			   && (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0)))
			       != UNION_TYPE))
		     && (TYPE_MODE (TREE_TYPE (exp))
			 == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))))
	break;

      /* If any reference in the chain is volatile, the effect is volatile.  */
      if (TREE_THIS_VOLATILE (exp))
	*pvolatilep = 1;

      /* If the offset is non-constant already, then we can't assume any
	 alignment more than the alignment here.  */
      if (! integer_zerop (offset))
	alignment = MIN (alignment, TYPE_ALIGN (TREE_TYPE (exp)));

      exp = TREE_OPERAND (exp, 0);
    }

  if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
    alignment = MIN (alignment, DECL_ALIGN (exp));
  else if (TREE_TYPE (exp) != 0)
    alignment = MIN (alignment, TYPE_ALIGN (TREE_TYPE (exp)));

  if (integer_zerop (offset))
    offset = 0;

  if (offset != 0 && contains_placeholder_p (offset))
    offset = build (WITH_RECORD_EXPR, sizetype, offset, orig_exp);

  *pmode = mode;
  *poffset = offset;
  *palignment = alignment / BITS_PER_UNIT;
  return exp;
}

/* Subroutine of expand_exp: compute memory_usage from modifier.  */
static enum memory_use_mode
get_memory_usage_from_modifier (modifier)
     enum expand_modifier modifier;
{
  switch (modifier)
    {
    case EXPAND_NORMAL:
    case EXPAND_SUM:
      return MEMORY_USE_RO;
      break;
    case EXPAND_MEMORY_USE_WO:
      return MEMORY_USE_WO;
      break;
    case EXPAND_MEMORY_USE_RW:
      return MEMORY_USE_RW;
      break;
    case EXPAND_MEMORY_USE_DONT:
      /* EXPAND_CONST_ADDRESS and EXPAND_INITIALIZER are converted into
	 MEMORY_USE_DONT, because they are modifiers to a call of
	 expand_expr in the ADDR_EXPR case of expand_expr.  */
    case EXPAND_CONST_ADDRESS:
    case EXPAND_INITIALIZER:
      return MEMORY_USE_DONT;
    case EXPAND_MEMORY_USE_BAD:
    default:
      abort ();
    }
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
      /* We give UNSIGNEDP = 0 to expand_binop
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
	rtx target = assign_temp (part_type, 0, 1, 1);
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
   EXP can reference X, which is being modified.  TOP_P is nonzero if this
   call is going to be used to determine whether we need a temporary
   for EXP, as opposed to a recursive call to this function.  */

static int
safe_from_p (x, exp, top_p)
     rtx x;
     tree exp;
     int top_p;
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
      || (top_p && TREE_TYPE (exp) != 0 && TYPE_SIZE (TREE_TYPE (exp)) != 0
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST
	  && (TREE_CODE (TREE_TYPE (exp)) != ARRAY_TYPE
	      || TYPE_ARRAY_MAX_SIZE (TREE_TYPE (exp)) == NULL_TREE
	      || TREE_CODE (TYPE_ARRAY_MAX_SIZE (TREE_TYPE (exp)))
	      != INTEGER_CST)
	  && GET_MODE (x) == BLKmode))
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
		 || safe_from_p (x, TREE_VALUE (exp), 0))
		&& (TREE_CHAIN (exp) == 0
		    || safe_from_p (x, TREE_CHAIN (exp), 0)));
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
	  return (staticp (TREE_OPERAND (exp, 0))
		  || safe_from_p (x, TREE_OPERAND (exp, 0), 0)
		  || TREE_STATIC (exp));

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
	  /* If a sequence exists, we would have to scan every instruction
	     in the sequence to see if it was safe.  This is probably not
	     worthwhile.  */
	  if (RTL_EXPR_SEQUENCE (exp))
	    return 0;

	  exp_rtl = RTL_EXPR_RTL (exp);
	  break;

	case WITH_CLEANUP_EXPR:
	  exp_rtl = RTL_EXPR_RTL (exp);
	  break;

	case CLEANUP_POINT_EXPR:
	  return safe_from_p (x, TREE_OPERAND (exp, 0), 0);

	case SAVE_EXPR:
	  exp_rtl = SAVE_EXPR_RTL (exp);
	  break;

	case BIND_EXPR:
	  /* The only operand we look at is operand 1.  The rest aren't
	     part of the expression.  */
	  return safe_from_p (x, TREE_OPERAND (exp, 1), 0);

	case METHOD_CALL_EXPR:
	  /* This takes a rtx argument, but shouldn't appear here.  */
	  abort ();
	  
	default:
	  break;
	}

      /* If we have an rtx, we do not need to scan our operands.  */
      if (exp_rtl)
	break;

      nops = tree_code_length[(int) TREE_CODE (exp)];
      for (i = 0; i < nops; i++)
	if (TREE_OPERAND (exp, i) != 0
	    && ! safe_from_p (x, TREE_OPERAND (exp, i), 0))
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
     register tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  /* Chain of pending expressions for PLACEHOLDER_EXPR to replace.
     This is static so it will be accessible to our recursive callees.  */
  static tree placeholder_list = 0;
  register rtx op0, op1, temp;
  tree type = TREE_TYPE (exp);
  int unsignedp = TREE_UNSIGNED (type);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  optab this_optab;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);
  rtx original_target = target;
  /* Maybe defer this until sure not doing bytecode?  */
  int ignore = (target == const0_rtx
		|| ((code == NON_LVALUE_EXPR || code == NOP_EXPR
		     || code == CONVERT_EXPR || code == REFERENCE_EXPR
		     || code == COND_EXPR)
		    && TREE_CODE (type) == VOID_TYPE));
  tree context;
  /* Used by check-memory-usage to make modifier read only.  */
  enum expand_modifier ro_modifier;

  /* Make a read-only version of the modifier.  */
  if (modifier == EXPAND_NORMAL || modifier == EXPAND_SUM
      || modifier == EXPAND_CONST_ADDRESS || modifier == EXPAND_INITIALIZER)
    ro_modifier = modifier;
  else
    ro_modifier = EXPAND_NORMAL;

  if (output_bytecode && modifier != EXPAND_INITIALIZER)
    {
      bc_expand_expr (exp);
      return NULL;
    }

  /* Don't use hard regs as subtargets, because the combiner
     can only handle pseudo regs.  */
  if (subtarget && REGNO (subtarget) < FIRST_PSEUDO_REGISTER)
    subtarget = 0;
  /* Avoid subtargets inside loops,
     since they hide some invariant expressions.  */
  if (preserve_subexpressions_p ())
    subtarget = 0;

  /* If we are going to ignore this result, we need only do something
     if there is a side-effect somewhere in the expression.  If there
     is, short-circuit the most common cases here.  Note that we must
     not call expand_expr with anything but const0_rtx in case this
     is an initial expansion of a size that contains a PLACEHOLDER_EXPR.  */

  if (ignore)
    {
      if (! TREE_SIDE_EFFECTS (exp))
	return const0_rtx;

      /* Ensure we reference a volatile object even if value is ignored.  */
      if (TREE_THIS_VOLATILE (exp)
	  && TREE_CODE (exp) != FUNCTION_DECL
	  && mode != VOIDmode && mode != BLKmode)
	{
	  temp = expand_expr (exp, NULL_RTX, VOIDmode, ro_modifier);
	  if (GET_CODE (temp) == MEM)
	    temp = copy_to_reg (temp);
	  return const0_rtx;
	}

      if (TREE_CODE_CLASS (code) == '1')
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx,
			    VOIDmode, ro_modifier);
      else if (TREE_CODE_CLASS (code) == '2'
	       || TREE_CODE_CLASS (code) == '<')
	{
	  expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, ro_modifier);
	  expand_expr (TREE_OPERAND (exp, 1), const0_rtx, VOIDmode, ro_modifier);
	  return const0_rtx;
	}
      else if ((code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
	       && ! TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 1)))
	/* If the second operand has no side effects, just evaluate
	   the first.  */
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx,
			    VOIDmode, ro_modifier);

      target = 0;
    }

  /* If will do cse, generate all results into pseudo registers
     since 1) that allows cse to find more things
     and 2) otherwise cse could produce an insn the machine
     cannot support.  */

  if (! cse_not_expected && mode != BLKmode && target
      && (GET_CODE (target) != REG || REGNO (target) < FIRST_PSEUDO_REGISTER))
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
      if (DECL_SIZE (exp) == 0 && TYPE_SIZE (TREE_TYPE (exp)) != 0
	  && (TREE_STATIC (exp) || DECL_EXTERNAL (exp)))
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  layout_decl (exp, 0);
	  PUT_MODE (DECL_RTL (exp), DECL_MODE (exp));
	  pop_obstacks ();
	}

      /* Only check automatic variables.  Currently, function arguments are
         not checked (this can be done at compile-time with prototypes).
         Aggregates are not checked.  */
      if (flag_check_memory_usage && code == VAR_DECL
	  && GET_CODE (DECL_RTL (exp)) == MEM
	  && DECL_CONTEXT (exp) != NULL_TREE
	  && ! TREE_STATIC (exp)
	  && ! AGGREGATE_TYPE_P (TREE_TYPE (exp)))
	{
	  enum memory_use_mode memory_usage;
	  memory_usage = get_memory_usage_from_modifier (modifier);

	  if (memory_usage != MEMORY_USE_DONT)
	    emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
			       XEXP (DECL_RTL (exp), 0), ptr_mode,
			       GEN_INT (int_size_in_bytes (type)),
			       TYPE_MODE (sizetype),
			       GEN_INT (memory_usage),
			       TYPE_MODE (integer_type_node));
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
	    addr = gen_rtx (MEM, Pmode,
			    fix_lexical_addr (XEXP (addr, 0), exp));
	  else
	    addr = fix_lexical_addr (addr, exp);
	  temp = change_address (DECL_RTL (exp), mode, addr);
	}

      /* This is the case of an array whose size is to be determined
	 from its initializer, while the initializer is still being parsed.
	 See expand_decl.  */

      else if (GET_CODE (DECL_RTL (exp)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (exp), 0)) == REG)
	temp = change_address (DECL_RTL (exp), GET_MODE (DECL_RTL (exp)),
			       XEXP (DECL_RTL (exp), 0));

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
	temp = change_address (DECL_RTL (exp), VOIDmode,
			       copy_rtx (XEXP (DECL_RTL (exp), 0)));

      /* If we got something, return it.  But first, set the alignment
	 the address is a register.  */
      if (temp != 0)
	{
	  if (GET_CODE (temp) == MEM && GET_CODE (XEXP (temp, 0)) == REG)
	    mark_reg_pointer (XEXP (temp, 0),
			      DECL_ALIGN (exp) / BITS_PER_UNIT);

	  return temp;
	}

      /* If the mode of DECL_RTL does not match that of the decl, it
	 must be a promoted value.  We return a SUBREG of the wanted mode,
	 but mark it so that we know that it was already extended.  */

      if (GET_CODE (DECL_RTL (exp)) == REG
	  && GET_MODE (DECL_RTL (exp)) != mode)
	{
	  /* Get the signedness used for this variable.  Ensure we get the
	     same mode we got when the variable was declared.  */
	  if (GET_MODE (DECL_RTL (exp))
	      != promote_mode (type, DECL_MODE (exp), &unsignedp, 0))
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
      return expand_expr (DECL_INITIAL (exp), target, VOIDmode,
      			  EXPAND_MEMORY_USE_BAD);

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
	  && (! memory_address_p (mode, XEXP (TREE_CST_RTL (exp), 0))
	      || (flag_force_addr
		  && GET_CODE (XEXP (TREE_CST_RTL (exp), 0)) != REG)))
	return change_address (TREE_CST_RTL (exp), VOIDmode,
			       copy_rtx (XEXP (TREE_CST_RTL (exp), 0)));
      return TREE_CST_RTL (exp);

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
	  return change_address (temp, mode,
				 fix_lexical_addr (XEXP (temp, 0), exp));
	}
      if (SAVE_EXPR_RTL (exp) == 0)
	{
	  if (mode == VOIDmode)
	    temp = const0_rtx;
	  else
	    temp = assign_temp (type, 3, 0, 0);

	  SAVE_EXPR_RTL (exp) = temp;
	  if (!optimize && GET_CODE (temp) == REG)
	    save_expr_regs = gen_rtx (EXPR_LIST, VOIDmode, temp,
				      save_expr_regs);

	  /* If the mode of TEMP does not match that of the expression, it
	     must be a promoted value.  We pass store_expr a SUBREG of the
	     wanted mode but mark it so that we know that it was already
	     extended.  Note that `unsignedp' was modified above in
	     this case.  */

	  if (GET_CODE (temp) == REG && GET_MODE (temp) != mode)
	    {
	      temp = gen_rtx (SUBREG, mode, SAVE_EXPR_RTL (exp), 0);
	      SUBREG_PROMOTED_VAR_P (temp) = 1;
	      SUBREG_PROMOTED_UNSIGNED_P (temp) = unsignedp;
	    }

	  if (temp == const0_rtx)
	    expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode,
			 EXPAND_MEMORY_USE_BAD);
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
	  temp = gen_rtx (SUBREG, mode, SAVE_EXPR_RTL (exp), 0);
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
	tree placeholder_expr;

	/* If there is an object on the head of the placeholder list,
	   see if some object in it of type TYPE or a pointer to it.  For
	   further information, see tree.def.  */
	for (placeholder_expr = placeholder_list;
	     placeholder_expr != 0;
	     placeholder_expr = TREE_CHAIN (placeholder_expr))
	  {
	    tree need_type = TYPE_MAIN_VARIANT (type);
	    tree object = 0;
	    tree old_list = placeholder_list;
	    tree elt;

	    /* Find the outermost reference that is of the type we want.
	       If none, see if any object has a type that is a pointer to 
	       the type we want.  */
	    for (elt = TREE_PURPOSE (placeholder_expr);
		 elt != 0 && object == 0;
		 elt
		 = ((TREE_CODE (elt) == COMPOUND_EXPR
		     || TREE_CODE (elt) == COND_EXPR)
		    ? TREE_OPERAND (elt, 1)
		    : (TREE_CODE_CLASS (TREE_CODE (elt)) == 'r'
		       || TREE_CODE_CLASS (TREE_CODE (elt)) == '1'
		       || TREE_CODE_CLASS (TREE_CODE (elt)) == '2'
		       || TREE_CODE_CLASS (TREE_CODE (elt)) == 'e')
		    ? TREE_OPERAND (elt, 0) : 0))
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (elt)) == need_type)
		object = elt;

	    for (elt = TREE_PURPOSE (placeholder_expr);
		 elt != 0 && object == 0;
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
		object = build1 (INDIRECT_REF, need_type, elt);

	    if (object != 0)
	      {
		/* Expand this object skipping the list entries before
		   it was found in case it is also a PLACEHOLDER_EXPR.
		   In that case, we want to translate it using subsequent
		   entries.  */
		placeholder_list = TREE_CHAIN (placeholder_expr);
		temp = expand_expr (object, original_target, tmode,
				    ro_modifier);
		placeholder_list = old_list;
		return temp;
	      }
	  }
      }

      /* We can't find the object or there was a missing WITH_RECORD_EXPR.  */
      abort ();

    case WITH_RECORD_EXPR:
      /* Put the object on the placeholder list, expand our first operand,
	 and pop the list.  */
      placeholder_list = tree_cons (TREE_OPERAND (exp, 1), NULL_TREE,
				    placeholder_list);
      target = expand_expr (TREE_OPERAND (exp, 0), original_target,
			    tmode, ro_modifier);
      placeholder_list = TREE_CHAIN (placeholder_list);
      return target;

    case EXIT_EXPR:
      expand_exit_loop_if_false (NULL_PTR,
				 invert_truthvalue (TREE_OPERAND (exp, 0)));
      return const0_rtx;

    case LOOP_EXPR:
      push_temp_slots ();
      expand_start_loop (1);
      expand_expr_stmt (TREE_OPERAND (exp, 0));
      expand_end_loop ();
      pop_temp_slots ();

      return const0_rtx;

    case BIND_EXPR:
      {
	tree vars = TREE_OPERAND (exp, 0);
	int vars_need_expansion = 0;

	/* Need to open a binding contour here because
	   if there are any cleanups they must be contained here.  */
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

	temp = expand_expr (TREE_OPERAND (exp, 1), target, tmode, ro_modifier);

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
	    expand_expr (TREE_VALUE (elt), const0_rtx, VOIDmode,
	    		 EXPAND_MEMORY_USE_BAD);
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
		    || (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
			&& (move_by_pieces_ninsns
			    (TREE_INT_CST_LOW (TYPE_SIZE (type))/BITS_PER_UNIT,
			     TYPE_ALIGN (type) / BITS_PER_UNIT)
			    > MOVE_RATIO)
			&& ! mostly_zeros_p (exp))))
	       || (modifier == EXPAND_INITIALIZER && TREE_CONSTANT (exp)))
	{
	  rtx constructor = output_constant_def (exp);
	  if (modifier != EXPAND_CONST_ADDRESS
	      && modifier != EXPAND_INITIALIZER
	      && modifier != EXPAND_SUM
	      && (! memory_address_p (GET_MODE (constructor),
				      XEXP (constructor, 0))
		  || (flag_force_addr
		      && GET_CODE (XEXP (constructor, 0)) != REG)))
	    constructor = change_address (constructor, VOIDmode,
					  XEXP (constructor, 0));
	  return constructor;
	}

      else
	{
	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */
	  if (target == 0 || ! safe_from_p (target, exp, 1)
	      || GET_CODE (target) == PARALLEL)
	    {
	      if (mode != BLKmode && ! TREE_ADDRESSABLE (exp))
		target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);
	      else
		target = assign_temp (type, 0, 1, 1);
	    }

	  if (TREE_READONLY (exp))
	    {
	      if (GET_CODE (target) == MEM)
		target = copy_rtx (target);

	      RTX_UNCHANGING_P (target) = 1;
	    }

	  store_constructor (exp, target, 0);
	  return target;
	}

    case INDIRECT_REF:
      {
	tree exp1 = TREE_OPERAND (exp, 0);
	tree exp2;

	op0 = expand_expr (exp1, NULL_RTX, VOIDmode, EXPAND_SUM);
	op0 = memory_address (mode, op0);

	if (flag_check_memory_usage && !AGGREGATE_TYPE_P (TREE_TYPE (exp)))
	  {
	    enum memory_use_mode memory_usage;
	    memory_usage = get_memory_usage_from_modifier (modifier);

            if (memory_usage != MEMORY_USE_DONT)
	      {
		in_check_memory_usage = 1;
		emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
				   op0, ptr_mode,
				   GEN_INT (int_size_in_bytes (type)),
				   TYPE_MODE (sizetype),
				   GEN_INT (memory_usage),
				   TYPE_MODE (integer_type_node));
		in_check_memory_usage = 0;
	      }
	  }

	temp = gen_rtx (MEM, mode, op0);
	/* If address was computed by addition,
	   mark this as an element of an aggregate.  */
	if (TREE_CODE (TREE_OPERAND (exp, 0)) == PLUS_EXPR
	    || (TREE_CODE (TREE_OPERAND (exp, 0)) == SAVE_EXPR
		&& TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == PLUS_EXPR)
	    || AGGREGATE_TYPE_P (TREE_TYPE (exp))
	    || (TREE_CODE (exp1) == ADDR_EXPR
		&& (exp2 = TREE_OPERAND (exp1, 0))
		&& AGGREGATE_TYPE_P (TREE_TYPE (exp2))))
	  MEM_IN_STRUCT_P (temp) = 1;
	MEM_VOLATILE_P (temp) = TREE_THIS_VOLATILE (exp) | flag_volatile;

	/* It is incorrect to set RTX_UNCHANGING_P from TREE_READONLY
	   here, because, in C and C++, the fact that a location is accessed
	   through a pointer to const does not mean that the value there can
	   never change.  Languages where it can never change should
	   also set TREE_STATIC.  */
	RTX_UNCHANGING_P (temp) = TREE_READONLY (exp) & TREE_STATIC (exp);
	return temp;
      }

    case ARRAY_REF:
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) != ARRAY_TYPE)
	abort ();

      {
	tree array = TREE_OPERAND (exp, 0);
	tree domain = TYPE_DOMAIN (TREE_TYPE (array));
	tree low_bound = domain ? TYPE_MIN_VALUE (domain) : integer_zero_node;
	tree index = TREE_OPERAND (exp, 1);
	tree index_type = TREE_TYPE (index);
	HOST_WIDE_INT i;

	/* Optimize the special-case of a zero lower bound.

	   We convert the low_bound to sizetype to avoid some problems
	   with constant folding.  (E.g. suppose the lower bound is 1,
	   and its mode is QI.  Without the conversion,  (ARRAY
	   +(INDEX-(unsigned char)1)) becomes ((ARRAY+(-(unsigned char)1))
	   +INDEX), which becomes (ARRAY+255+INDEX).  Oops!)

	   But sizetype isn't quite right either (especially if
	   the lowbound is negative).  FIXME */

	if (! integer_zerop (low_bound))
	  index = fold (build (MINUS_EXPR, index_type, index,
			       convert (sizetype, low_bound)));

	/* Fold an expression like: "foo"[2].
	   This is not done in fold so it won't happen inside &.
	   Don't fold if this is for wide characters since it's too
	   difficult to do correctly and this is a very rare case.  */

	if (TREE_CODE (array) == STRING_CST
	    && TREE_CODE (index) == INTEGER_CST
	    && !TREE_INT_CST_HIGH (index)
	    && (i = TREE_INT_CST_LOW (index)) < TREE_STRING_LENGTH (array)
	    && GET_MODE_CLASS (mode) == MODE_INT
	    && GET_MODE_SIZE (mode) == 1)
	  return GEN_INT (TREE_STRING_POINTER (array)[i]);

	/* If this is a constant index into a constant array,
	   just get the value from the array.  Handle both the cases when
	   we have an explicit constructor and when our operand is a variable
	   that was declared const.  */

	if (TREE_CODE (array) == CONSTRUCTOR && ! TREE_SIDE_EFFECTS (array))
	  {
	    if (TREE_CODE (index) == INTEGER_CST
		&& TREE_INT_CST_HIGH (index) == 0)
	      {
		tree elem = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0));

		i = TREE_INT_CST_LOW (index);
		while (elem && i--)
		  elem = TREE_CHAIN (elem);
		if (elem)
		  return expand_expr (fold (TREE_VALUE (elem)), target,
				      tmode, ro_modifier);
	      }
	  }
	  
	else if (optimize >= 1
		 && TREE_READONLY (array) && ! TREE_SIDE_EFFECTS (array)
		 && TREE_CODE (array) == VAR_DECL && DECL_INITIAL (array)
		 && TREE_CODE (DECL_INITIAL (array)) != ERROR_MARK)
	  {
	    if (TREE_CODE (index) == INTEGER_CST)
	      {
		tree init = DECL_INITIAL (array);

		i = TREE_INT_CST_LOW (index);
		if (TREE_CODE (init) == CONSTRUCTOR)
		  {
		    tree elem = CONSTRUCTOR_ELTS (init);

		    while (elem
			   && !tree_int_cst_equal (TREE_PURPOSE (elem), index))
		      elem = TREE_CHAIN (elem);
		    if (elem)
		      return expand_expr (fold (TREE_VALUE (elem)), target,
					  tmode, ro_modifier);
		  }
		else if (TREE_CODE (init) == STRING_CST
			 && TREE_INT_CST_HIGH (index) == 0
			 && (TREE_INT_CST_LOW (index)
			     < TREE_STRING_LENGTH (init)))
		  return (GEN_INT
			  (TREE_STRING_POINTER
			   (init)[TREE_INT_CST_LOW (index)]));
	      }
	  }
      }

      /* ... fall through ... */

    case COMPONENT_REF:
    case BIT_FIELD_REF:
      /* If the operand is a CONSTRUCTOR, we can just extract the
	 appropriate field if it is present.  Don't do this if we have
	 already written the data since we want to refer to that copy
	 and varasm.c assumes that's what we'll do.  */
      if (code != ARRAY_REF
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
		op0 =  expand_expr (TREE_VALUE (elt), target, tmode, modifier);
		if (DECL_BIT_FIELD (TREE_PURPOSE (elt)))
		  {
		    int bitsize = DECL_FIELD_SIZE (TREE_PURPOSE (elt));

		    if (TREE_UNSIGNED (TREE_TYPE (TREE_PURPOSE (elt))))
		      {
			op1 = GEN_INT (((HOST_WIDE_INT) 1 << bitsize) - 1);
			op0 = expand_and (op0, op1, target);
		      }
		    else
		      {
			enum machine_mode imode
			  = TYPE_MODE (TREE_TYPE (TREE_PURPOSE (elt)));
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
	int bitsize;
	int bitpos;
	tree offset;
	int volatilep = 0;
	int alignment;
	tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					&mode1, &unsignedp, &volatilep,
					&alignment);

	/* If we got back the original object, something is wrong.  Perhaps
	   we are evaluating an expression too early.  In any event, don't
	   infinitely recurse.  */
	if (tem == exp)
	  abort ();

	/* If TEM's type is a union of variable size, pass TARGET to the inner
	   computation, since it will need a temporary and TARGET is known
	   to have to do.  This occurs in unchecked conversion in Ada.  */
  
	op0 = expand_expr (tem,
			   (TREE_CODE (TREE_TYPE (tem)) == UNION_TYPE
			    && (TREE_CODE (TYPE_SIZE (TREE_TYPE (tem)))
				!= INTEGER_CST)
			    ? target : NULL_RTX),
			   VOIDmode,
			   modifier == EXPAND_INITIALIZER
			   ? modifier : EXPAND_NORMAL);

	/* If this is a constant, put it into a register if it is a
	   legitimate constant and memory if it isn't.  */
	if (CONSTANT_P (op0))
	  {
	    enum machine_mode mode = TYPE_MODE (TREE_TYPE (tem));
	    if (mode != BLKmode && LEGITIMATE_CONSTANT_P (op0))
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
				  gen_rtx (PLUS, ptr_mode, XEXP (op0, 0),
					   force_reg (ptr_mode, offset_rtx)));
	  }

	/* Don't forget about volatility even if this is a bitfield.  */
	if (GET_CODE (op0) == MEM && volatilep && ! MEM_VOLATILE_P (op0))
	  {
	    op0 = copy_rtx (op0);
	    MEM_VOLATILE_P (op0) = 1;
	  }

	/* Check the access.  */
	if (flag_check_memory_usage && GET_CODE (op0) == MEM)
          {
	    enum memory_use_mode memory_usage;
	    memory_usage = get_memory_usage_from_modifier (modifier);

	    if (memory_usage != MEMORY_USE_DONT)
	      {
		rtx to;
		int size;

		to = plus_constant (XEXP (op0, 0), (bitpos / BITS_PER_UNIT));
		size = (bitpos % BITS_PER_UNIT) + bitsize + BITS_PER_UNIT - 1;

        	/* Check the access right of the pointer.  */
		if (size > BITS_PER_UNIT)
		  emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
				     to, ptr_mode,
				     GEN_INT (size / BITS_PER_UNIT),
				     TYPE_MODE (sizetype),
				     GEN_INT (memory_usage), 
				     TYPE_MODE (integer_type_node));
	      }
	  }

	/* In cases where an aligned union has an unaligned object
	   as a field, we might be extracting a BLKmode value from
	   an integer-mode (e.g., SImode) object.  Handle this case
	   by doing the extract into an object as wide as the field
	   (which we know to be the width of a basic mode), then
	   storing into memory, and changing the mode to BLKmode.
	   If we ultimately want the address (EXPAND_CONST_ADDRESS or
	   EXPAND_INITIALIZER), then we must not copy to a temporary.  */
	if (mode1 == VOIDmode
	    || GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG
	    || (modifier != EXPAND_CONST_ADDRESS
		&& modifier != EXPAND_INITIALIZER
		&& ((mode1 != BLKmode && ! direct_load[(int) mode1]
		     && GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
		     && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
		    /* If the field isn't aligned enough to fetch as a memref,
		       fetch it as a bit field.  */
		    || (SLOW_UNALIGNED_ACCESS
			&& ((TYPE_ALIGN (TREE_TYPE (tem)) < GET_MODE_ALIGNMENT (mode))
			    || (bitpos % GET_MODE_ALIGNMENT (mode) != 0))))))
	  {
	    enum machine_mode ext_mode = mode;

	    if (ext_mode == BLKmode)
	      ext_mode = mode_for_size (bitsize, MODE_INT, 1);

	    if (ext_mode == BLKmode)
	      {
		/* In this case, BITPOS must start at a byte boundary and
		   TARGET, if specified, must be a MEM.  */
		if (GET_CODE (op0) != MEM
		    || (target != 0 && GET_CODE (target) != MEM)
		    || bitpos % BITS_PER_UNIT != 0)
		  abort ();

		op0 = change_address (op0, VOIDmode,
				      plus_constant (XEXP (op0, 0),
						     bitpos / BITS_PER_UNIT));
		if (target == 0)
		  target = assign_temp (type, 0, 1, 1);

		emit_block_move (target, op0,
				 GEN_INT ((bitsize + BITS_PER_UNIT - 1)
					  / BITS_PER_UNIT),
				 1);
		
		return target;
	      }

	    op0 = validize_mem (op0);

	    if (GET_CODE (op0) == MEM && GET_CODE (XEXP (op0, 0)) == REG)
	      mark_reg_pointer (XEXP (op0, 0), alignment);

	    op0 = extract_bit_field (op0, bitsize, bitpos,
				     unsignedp, target, ext_mode, ext_mode,
				     alignment,
				     int_size_in_bytes (TREE_TYPE (tem)));

	    /* If the result is a record type and BITSIZE is narrower than
	       the mode of OP0, an integral mode, and this is a big endian
	       machine, we must put the field into the high-order bits.  */
	    if (TREE_CODE (type) == RECORD_TYPE && BYTES_BIG_ENDIAN
		&& GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
		&& bitsize < GET_MODE_BITSIZE (GET_MODE (op0)))
	      op0 = expand_shift (LSHIFT_EXPR, GET_MODE (op0), op0,
				  size_int (GET_MODE_BITSIZE (GET_MODE (op0))
					    - bitsize),
				  op0, 1);

	    if (mode == BLKmode)
	      {
		rtx new = assign_stack_temp (ext_mode,
					     bitsize / BITS_PER_UNIT, 0);

		emit_move_insn (new, op0);
		op0 = copy_rtx (new);
		PUT_MODE (op0, BLKmode);
		MEM_IN_STRUCT_P (op0) = 1;
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
	  op0 = gen_rtx (MEM, mode1, plus_constant (XEXP (op0, 0),
						    (bitpos / BITS_PER_UNIT)));
	else
	  op0 = change_address (op0, mode1,
				plus_constant (XEXP (op0, 0),
					       (bitpos / BITS_PER_UNIT)));
	if (GET_CODE (XEXP (op0, 0)) == REG)
	  mark_reg_pointer (XEXP (op0, 0), alignment);

	MEM_IN_STRUCT_P (op0) = 1;
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

	preexpand_calls (exp);

	/* If domain is empty, answer is no.  Likewise if index is constant
	   and out of bounds.  */
	if ((TREE_CODE (set_high_bound) == INTEGER_CST
	     && TREE_CODE (set_low_bound) == INTEGER_CST
	     && tree_int_cst_lt (set_high_bound, set_low_bound)
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
	  {
	    emit_cmp_insn (index_val, lo_r, LT, NULL_RTX,
			   GET_MODE (index_val), iunsignedp, 0);
	    emit_jump_insn (gen_blt (op1));
	  }

	if (! (GET_CODE (index_val) == CONST_INT
	       && GET_CODE (hi_r) == CONST_INT))
	  {
	    emit_cmp_insn (index_val, hi_r, GT, NULL_RTX,
			   GET_MODE (index_val), iunsignedp, 0);
	    emit_jump_insn (gen_bgt (op1));
	  }

	/* Calculate the element number of bit zero in the first word
	   of the set.  */
	if (GET_CODE (lo_r) == CONST_INT)
	  rlow = GEN_INT (INTVAL (lo_r)
			  & ~ ((HOST_WIDE_INT) 1 << BITS_PER_UNIT));
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

	/* Extract the bit we want to examine */
	bit = expand_shift (RSHIFT_EXPR, byte_mode,
			    gen_rtx (MEM, byte_mode, addr),
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
      if (RTL_EXPR_RTL (exp) == 0)
	{
	  RTL_EXPR_RTL (exp)
	    = expand_expr (TREE_OPERAND (exp, 0), target, tmode, ro_modifier);
	  expand_decl_cleanup (NULL_TREE, TREE_OPERAND (exp, 2));

	  /* That's it for this cleanup.  */
	  TREE_OPERAND (exp, 2) = 0;
	}
      return RTL_EXPR_RTL (exp);

    case CLEANUP_POINT_EXPR:
      {
	extern int temp_slot_level;
	/* Start a new binding layer that will keep track of all cleanup
	   actions to be performed.  */
	expand_start_bindings (0);

	target_temp_slot_level = temp_slot_level;

	op0 = expand_expr (TREE_OPERAND (exp, 0), target, tmode, ro_modifier);
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
      if (TREE_CODE (type) == UNION_TYPE)
	{
	  tree valtype = TREE_TYPE (TREE_OPERAND (exp, 0));
	  if (target == 0)
	    {
	      if (mode != BLKmode)
		target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);
	      else
		target = assign_temp (type, 0, 1, 1);
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

      if (mode == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode,
			     ro_modifier);

	  /* If the signedness of the conversion differs and OP0 is
	     a promoted SUBREG, clear that indication since we now
	     have to do the proper extension.  */
	  if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))) != unsignedp
	      && GET_CODE (op0) == SUBREG)
	    SUBREG_PROMOTED_VAR_P (op0) = 0;

	  return op0;
	}

      op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, mode, 0);
      if (GET_MODE (op0) == mode)
	return op0;

      /* If OP0 is a constant, just convert it into the proper mode.  */
      if (CONSTANT_P (op0))
	return
	  convert_modes (mode, TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
			 op0, TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));

      if (modifier == EXPAND_INITIALIZER)
	return gen_rtx (unsignedp ? ZERO_EXTEND : SIGN_EXTEND, mode, op0);

      if (target == 0)
	return
	  convert_to_mode (mode, op0,
			   TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      else
	convert_move (target, op0,
		      TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case PLUS_EXPR:
      /* We come here from MINUS_EXPR when the second operand is a
         constant.  */
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

      /* If the result is to be ptr_mode and we are adding an integer to
	 something, we might be forming a constant.  So try to use
	 plus_constant.  If it produces a sum and we can't accept it,
	 use force_operand.  This allows P = &ARR[const] to generate
	 efficient code on machines where a SYMBOL_REF is not a valid
	 address.

	 If this is an EXPAND_SUM call, always return the sum.  */
      if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER
	  || mode == ptr_mode)
	{
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	      && TREE_CONSTANT (TREE_OPERAND (exp, 1)))
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
		   && TREE_CONSTANT (TREE_OPERAND (exp, 0)))
	    {
	      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode,
				 EXPAND_SUM);
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
	      op0 = plus_constant (op0, TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)));
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

      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
	subtarget = 0;

      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, ro_modifier);
      op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, ro_modifier);

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
      /* For initializers, we are allowed to return a MINUS of two
	 symbolic constants.  Here we handle all cases when both operands
	 are constant.  */
      /* Handle difference of two symbolic constants,
	 for the sake of an initializer.  */
      if ((modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	  && really_constant_p (TREE_OPERAND (exp, 0))
	  && really_constant_p (TREE_OPERAND (exp, 1)))
	{
	  rtx op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX,
				 VOIDmode, ro_modifier);
	  rtx op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX,
				 VOIDmode, ro_modifier);

	  /* If the last operand is a CONST_INT, use plus_constant of
	     the negated constant.  Else make the MINUS.  */
	  if (GET_CODE (op1) == CONST_INT)
	    return plus_constant (op0, - INTVAL (op1));
	  else
	    return gen_rtx (MINUS, mode, op0, op1);
	}
      /* Convert A - const to A + (-const).  */
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  tree negated = fold (build1 (NEGATE_EXPR, type,
				       TREE_OPERAND (exp, 1)));

	  /* Deal with the case where we can't negate the constant
	     in TYPE.  */
	  if (TREE_UNSIGNED (type) || TREE_OVERFLOW (negated))
	    {
	      tree newtype = signed_type (type);
	      tree newop0 = convert (newtype, TREE_OPERAND (exp, 0));
	      tree newop1 = convert (newtype, TREE_OPERAND (exp, 1));
	      tree newneg = fold (build1 (NEGATE_EXPR, newtype, newop1));

	      if (! TREE_OVERFLOW (newneg))
		return expand_expr (convert (type, 
					     build (PLUS_EXPR, newtype,
						    newop0, newneg)),
				    target, tmode, ro_modifier);
	    }
	  else
	    {
	      exp = build (PLUS_EXPR, type, TREE_OPERAND (exp, 0), negated);
	      goto plus_expr;
	    }
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

      if (modifier == EXPAND_SUM && mode == ptr_mode
	  && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode,
			     EXPAND_SUM);

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
		    op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX,
				       VOIDmode, 0);
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
      preexpand_calls (exp);
      if (! safe_from_p (subtarget, TREE_OPERAND (exp, 1), 1))
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
      temp = expand_unop (mode, neg_optab, op0, target, 0);
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
      if (GET_MODE_CLASS (mode) == MODE_INT && !can_compare_p (mode))
	{
	  if (code == MAX_EXPR)
	    do_jump_by_parts_greater_rtx (mode, TREE_UNSIGNED (type),
					  target, op1, NULL_RTX, op0);
	  else
	    do_jump_by_parts_greater_rtx (mode, TREE_UNSIGNED (type),
					  op1, target, NULL_RTX, op0);
	  emit_move_insn (target, op1);
	}
      else
	{
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
	}
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
      preexpand_calls (exp);
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
      preexpand_calls (exp);
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
	  tree true = TREE_OPERAND (TREE_OPERAND (exp, 1), 0);
	  tree false = TREE_OPERAND (TREE_OPERAND (exp, 2), 0);

	  if ((TREE_CODE_CLASS (TREE_CODE (true)) == '2'
	       && operand_equal_p (false, TREE_OPERAND (true, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (false)) == '2'
		  && operand_equal_p (true, TREE_OPERAND (false, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (true)) == '1'
		  && operand_equal_p (false, TREE_OPERAND (true, 0), 0))
	      || (TREE_CODE_CLASS (TREE_CODE (false)) == '1'
		  && operand_equal_p (true, TREE_OPERAND (false, 0), 0)))
	    return expand_expr (build1 (NOP_EXPR, type,
					build (COND_EXPR, TREE_TYPE (true),
					       TREE_OPERAND (exp, 0),
					       true, false)),
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
			     ro_modifier);
		return const0_rtx;
	      }

	    op0 = expand_expr (TREE_OPERAND (exp, 0), target, mode, ro_modifier);
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
		 && ! (GET_CODE (original_target) == MEM
		       && MEM_VOLATILE_P (original_target)))
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
	    optab boptab = (TREE_CODE (binary_op) == PLUS_EXPR ? add_optab
			    : TREE_CODE (binary_op) == MINUS_EXPR ? sub_optab
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
	    if (GET_CODE (temp) == REG && REGNO (temp) < FIRST_PSEUDO_REGISTER)
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
	    if (GET_CODE (temp) == REG && REGNO (temp) < FIRST_PSEUDO_REGISTER)
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
	    if (temp != 0)
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
	    if (temp != 0)
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
	rtx temp;

	if (TREE_CODE (slot) != VAR_DECL)
	  abort ();

	if (! ignore)
	  target = original_target;

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
		target = assign_temp (type, 2, 0, 1);
		/* All temp slots at this level must not conflict.  */
		preserve_temp_slots (target);
		DECL_RTL (slot) = target;
		if (TREE_ADDRESSABLE (slot))
		  {
		    TREE_ADDRESSABLE (slot) = 0;
		    mark_addressable (slot);
		  }

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
		DECL_RTL (slot) = target;
		/* If we must have an addressable slot, then make sure that
		   the RTL that we just stored in slot is OK.  */
		if (TREE_ADDRESSABLE (slot))
		  {
		    TREE_ADDRESSABLE (slot) = 0;
		    mark_addressable (slot);
		  }
	      }
	  }

	exp1 = TREE_OPERAND (exp, 3) = TREE_OPERAND (exp, 1);
	/* Mark it as expanded.  */
	TREE_OPERAND (exp, 1) = NULL_TREE;

	TREE_USED (slot) = 1;
	store_expr (exp1, target, 0);

	expand_decl_cleanup (NULL_TREE, cleanups);
	
	return target;
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
	    && TREE_CODE (lhs) != PARM_DECL
	    && ! (TREE_CODE (lhs) == INDIRECT_REF
		  && TYPE_READONLY (TREE_TYPE (TREE_OPERAND (lhs, 0)))))
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
      return expand_increment (exp, 0, ignore);

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Faster to treat as pre-increment if result is not used.  */
      return expand_increment (exp, ! ignore, ignore);

    case ADDR_EXPR:
      /* If nonzero, TEMP will be set to the address of something that might
	 be a MEM corresponding to a stack slot.  */
      temp = 0;

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

	  op0 = protect_from_queue (op0, 0);

	  /* We would like the object in memory.  If it is a constant,
	     we can have it be statically allocated into memory.  For
	     a non-constant (REG, SUBREG or CONCAT), we need to allocate some
	     memory and store the value into it.  */

	  if (CONSTANT_P (op0))
	    op0 = force_const_mem (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
				   op0);
	  else if (GET_CODE (op0) == MEM)
	    {
	      mark_temp_addr_taken (op0);
	      temp = XEXP (op0, 0);
	    }

	  else if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG
		   || GET_CODE (op0) == CONCAT)
	    {
	      /* If this object is in a register, it must be not
		 be BLKmode.  */
	      tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	      rtx memloc = assign_temp (inner_type, 1, 1, 1);

	      mark_temp_addr_taken (memloc);
	      emit_move_insn (memloc, op0);
	      op0 = memloc;
	    }

	  if (GET_CODE (op0) != MEM)
	    abort ();
  
	  if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
	    {
	      temp = XEXP (op0, 0);
#ifdef POINTERS_EXTEND_UNSIGNED
	      if (GET_MODE (temp) == Pmode && GET_MODE (temp) != mode
		  && mode == ptr_mode)
		temp = convert_memory_address (ptr_mode, temp);
#endif
	      return temp;
	    }

	  op0 = force_operand (XEXP (op0, 0), target);
	}

      if (flag_force_addr && GET_CODE (op0) != REG)
	op0 = force_reg (Pmode, op0);

      if (GET_CODE (op0) == REG
	  && ! REG_USERVAR_P (op0))
	mark_reg_pointer (op0, TYPE_ALIGN (TREE_TYPE (type)) / BITS_PER_UNIT);

      /* If we might have had a temp slot, add an equivalent address
	 for it.  */
      if (temp != 0)
	update_temp_slot_address (temp, op0);

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
	
	op0  = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);

	if (! target)
	  target = gen_reg_rtx (mode);
								    
	start_sequence ();

	/* Store the realpart and the negated imagpart to target.  */
	emit_move_insn (gen_realpart (partmode, target),
			gen_realpart (partmode, op0));

	imag_t = gen_imagpart (partmode, target);
	temp = expand_unop (partmode, neg_optab,
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

	expand_eh_region_end (handler);

	return op0;
      }

    case POPDCC_EXPR:
      {
	rtx dcc = get_dynamic_cleanup_chain ();
	emit_move_insn (dcc, validize_mem (gen_rtx (MEM, Pmode, dcc)));
	return const0_rtx;
      }

    case POPDHC_EXPR:
      {
	rtx dhc = get_dynamic_handler_chain ();
	emit_move_insn (dhc, validize_mem (gen_rtx (MEM, Pmode, dhc)));
	return const0_rtx;
      }

    case ERROR_MARK:
      op0 = CONST0_RTX (tmode);
      if (op0 != 0)
	return op0;
      return const0_rtx;

    default:
      return (*lang_expand_expr) (exp, original_target, tmode, modifier);
    }

  /* Here to do an ordinary binary operator, generating an instruction
     from the optab already placed in `this_optab'.  */
 binop:
  preexpand_calls (exp);
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


/* Emit bytecode to evaluate the given expression EXP to the stack.  */

void
bc_expand_expr (exp)
    tree exp;
{
  enum tree_code code;
  tree type, arg0;
  rtx r;
  struct binary_operator *binoptab;
  struct unary_operator *unoptab;
  struct increment_operator *incroptab;
  struct bc_label *lab, *lab1;
  enum bytecode_opcode opcode;
  
  
  code = TREE_CODE (exp);
  
  switch (code)
    {
    case PARM_DECL:
      
      if (DECL_RTL (exp) == 0)
	{
	  error_with_decl (exp, "prior parameter's size depends on `%s'");
	  return;
	}
      
      bc_load_parmaddr (DECL_RTL (exp));
      bc_load_memory (TREE_TYPE (exp), exp);
      
      return;
      
    case VAR_DECL:
      
      if (DECL_RTL (exp) == 0)
	abort ();
      
#if 0
      if (BYTECODE_LABEL (DECL_RTL (exp)))
	bc_load_externaddr (DECL_RTL (exp));
      else
	bc_load_localaddr (DECL_RTL (exp));
#endif
      if (TREE_PUBLIC (exp))
	bc_load_externaddr_id (DECL_ASSEMBLER_NAME (exp),
			       BYTECODE_BC_LABEL (DECL_RTL (exp))->offset);
      else
	bc_load_localaddr (DECL_RTL (exp));
      
      bc_load_memory (TREE_TYPE (exp), exp);
      return;
      
    case INTEGER_CST:
      
#ifdef DEBUG_PRINT_CODE
      fprintf (stderr, " [%x]\n", TREE_INT_CST_LOW (exp));
#endif
      bc_emit_instruction (mode_to_const_map[(int) (DECL_BIT_FIELD (exp)
					     ? SImode
					     : TYPE_MODE (TREE_TYPE (exp)))],
			   (HOST_WIDE_INT) TREE_INT_CST_LOW (exp));
      return;
      
    case REAL_CST:
      
#if 0
#ifdef DEBUG_PRINT_CODE
      fprintf (stderr, " [%g]\n", (double) TREE_INT_CST_LOW (exp));
#endif
      /* FIX THIS: find a better way to pass real_cst's. -bson */
      bc_emit_instruction (mode_to_const_map[TYPE_MODE (TREE_TYPE (exp))],
			   (double) TREE_REAL_CST (exp));
#else
      abort ();
#endif

      return;
      
    case CALL_EXPR:
      
      /* We build a call description vector describing the type of
	 the return value and of the arguments; this call vector,
	 together with a pointer to a location for the return value
	 and the base of the argument list, is passed to the low
	 level machine dependent call subroutine, which is responsible
	 for putting the arguments wherever real functions expect
	 them, as well as getting the return value back.  */
      {
	tree calldesc = 0, arg;
	int nargs = 0, i;
	rtx retval;
	
	/* Push the evaluated args on the evaluation stack in reverse
	   order.  Also make an entry for each arg in the calldesc
	   vector while we're at it.  */
	
	TREE_OPERAND (exp, 1) = nreverse (TREE_OPERAND (exp, 1));
	
	for (arg = TREE_OPERAND (exp, 1); arg; arg = TREE_CHAIN (arg))
	  {
	    ++nargs;
	    bc_expand_expr (TREE_VALUE (arg));
	    
	    calldesc = tree_cons ((tree) 0,
				  size_in_bytes (TREE_TYPE (TREE_VALUE (arg))),
				  calldesc);
	    calldesc = tree_cons ((tree) 0,
				  bc_runtime_type_code (TREE_TYPE (TREE_VALUE (arg))),
				  calldesc);
	  }
	
	TREE_OPERAND (exp, 1) = nreverse (TREE_OPERAND (exp, 1));
	
	/* Allocate a location for the return value and push its
	   address on the evaluation stack.  Also make an entry
	   at the front of the calldesc for the return value type.  */
	
	type = TREE_TYPE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))));
	retval = bc_allocate_local (int_size_in_bytes (type), TYPE_ALIGN (type));
	bc_load_localaddr (retval);
	
	calldesc = tree_cons ((tree) 0, size_in_bytes (type), calldesc);
	calldesc = tree_cons ((tree) 0, bc_runtime_type_code (type), calldesc);
	
	/* Prepend the argument count.  */
	calldesc = tree_cons ((tree) 0,
			      build_int_2 (nargs, 0),
			      calldesc);
	
	/* Push the address of the call description vector on the stack.  */
	calldesc = build_nt (CONSTRUCTOR, (tree) 0, calldesc);
	TREE_TYPE (calldesc) = build_array_type (integer_type_node,
						 build_index_type (build_int_2 (nargs * 2, 0)));
	r = output_constant_def (calldesc);
	bc_load_externaddr (r);
	
	/* Push the address of the function to be called.  */
	bc_expand_expr (TREE_OPERAND (exp, 0));
	
	/* Call the function, popping its address and the calldesc vector
	   address off the evaluation stack in the process.  */
	bc_emit_instruction (call);
	
	/* Pop the arguments off the stack.  */
	bc_adjust_stack (nargs);
	
	/* Load the return value onto the stack.  */
	bc_load_localaddr (retval);
	bc_load_memory (type, TREE_OPERAND (exp, 0));
      }
      return;
      
    case SAVE_EXPR:
      
      if (!SAVE_EXPR_RTL (exp))
	{
	  /* First time around: copy to local variable */
	  SAVE_EXPR_RTL (exp) = bc_allocate_local (int_size_in_bytes (TREE_TYPE (exp)),
						   TYPE_ALIGN (TREE_TYPE(exp)));
	  bc_expand_expr (TREE_OPERAND (exp, 0));
	  bc_emit_instruction (duplicate);
	  
	  bc_load_localaddr (SAVE_EXPR_RTL (exp));
	  bc_store_memory (TREE_TYPE (exp), TREE_OPERAND (exp, 0));
	}
      else
	{
	  /* Consecutive reference: use saved copy */
	  bc_load_localaddr (SAVE_EXPR_RTL (exp));
	  bc_load_memory (TREE_TYPE (exp), TREE_OPERAND (exp, 0));
	}
      return;
      
#if 0
      /* FIXME: the XXXX_STMT codes have been removed in GCC2, but
	 how are they handled instead? */
    case LET_STMT:
      
      TREE_USED (exp) = 1;
      bc_expand_expr (STMT_BODY (exp));
      return;
#endif
      
    case NOP_EXPR:
    case CONVERT_EXPR:
      
      bc_expand_expr (TREE_OPERAND (exp, 0));
      bc_expand_conversion (TREE_TYPE (TREE_OPERAND (exp, 0)), TREE_TYPE (exp));
      return;
      
    case MODIFY_EXPR:
      
      expand_assignment (TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1), 0, 0);
      return;
      
    case ADDR_EXPR:
      
      bc_expand_address (TREE_OPERAND (exp, 0));
      return;
      
    case INDIRECT_REF:
      
      bc_expand_expr (TREE_OPERAND (exp, 0));
      bc_load_memory (TREE_TYPE (exp), TREE_OPERAND (exp, 0));
      return;
      
    case ARRAY_REF:
      
      bc_expand_expr (bc_canonicalize_array_ref (exp));
      return;
      
    case COMPONENT_REF:
      
      bc_expand_component_address (exp);
      
      /* If we have a bitfield, generate a proper load */
      bc_load_memory (TREE_TYPE (TREE_OPERAND (exp, 1)), TREE_OPERAND (exp, 1));
      return;
      
    case COMPOUND_EXPR:
      
      bc_expand_expr (TREE_OPERAND (exp, 0));
      bc_emit_instruction (drop);
      bc_expand_expr (TREE_OPERAND (exp, 1));
      return;
      
    case COND_EXPR:
      
      bc_expand_expr (TREE_OPERAND (exp, 0));
      bc_expand_truth_conversion (TREE_TYPE (TREE_OPERAND (exp, 0)));
      lab = bc_get_bytecode_label ();
      bc_emit_bytecode (xjumpifnot);
      bc_emit_bytecode_labelref (lab);
      
#ifdef DEBUG_PRINT_CODE
      fputc ('\n', stderr);
#endif
      bc_expand_expr (TREE_OPERAND (exp, 1));
      lab1 = bc_get_bytecode_label ();
      bc_emit_bytecode (jump);
      bc_emit_bytecode_labelref (lab1);
      
#ifdef DEBUG_PRINT_CODE
      fputc ('\n', stderr);
#endif
      
      bc_emit_bytecode_labeldef (lab);
      bc_expand_expr (TREE_OPERAND (exp, 2));
      bc_emit_bytecode_labeldef (lab1);
      return;
      
    case TRUTH_ANDIF_EXPR:
      
      opcode = xjumpifnot;
      goto andorif;
      
    case TRUTH_ORIF_EXPR:
      
      opcode = xjumpif;
      goto andorif;
      
    case PLUS_EXPR:
      
      binoptab = optab_plus_expr;
      goto binop;
      
    case MINUS_EXPR:
      
      binoptab = optab_minus_expr;
      goto binop;
      
    case MULT_EXPR:
      
      binoptab = optab_mult_expr;
      goto binop;
      
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      
      binoptab = optab_trunc_div_expr;
      goto binop;
      
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      
      binoptab = optab_trunc_mod_expr;
      goto binop;
      
    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
      abort ();			/* Not used for C.  */
      
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case FFS_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      abort ();			/* FIXME */
      
    case RDIV_EXPR:
      
      binoptab = optab_rdiv_expr;
      goto binop;
      
    case BIT_AND_EXPR:
      
      binoptab = optab_bit_and_expr;
      goto binop;
      
    case BIT_IOR_EXPR:
      
      binoptab = optab_bit_ior_expr;
      goto binop;
      
    case BIT_XOR_EXPR:
      
      binoptab = optab_bit_xor_expr;
      goto binop;
      
    case LSHIFT_EXPR:
      
      binoptab = optab_lshift_expr;
      goto binop;
      
    case RSHIFT_EXPR:
      
      binoptab = optab_rshift_expr;
      goto binop;
      
    case TRUTH_AND_EXPR:
      
      binoptab = optab_truth_and_expr;
      goto binop;
      
    case TRUTH_OR_EXPR:
      
      binoptab = optab_truth_or_expr;
      goto binop;
      
    case LT_EXPR:
      
      binoptab = optab_lt_expr;
      goto binop;
      
    case LE_EXPR:
      
      binoptab = optab_le_expr;
      goto binop;
      
    case GE_EXPR:
      
      binoptab = optab_ge_expr;
      goto binop;
      
    case GT_EXPR:
      
      binoptab = optab_gt_expr;
      goto binop;
      
    case EQ_EXPR:
      
      binoptab = optab_eq_expr;
      goto binop;
      
    case NE_EXPR:
      
      binoptab = optab_ne_expr;
      goto binop;
      
    case NEGATE_EXPR:
      
      unoptab = optab_negate_expr;
      goto unop;
      
    case BIT_NOT_EXPR:
      
      unoptab = optab_bit_not_expr;
      goto unop;
      
    case TRUTH_NOT_EXPR:
      
      unoptab = optab_truth_not_expr;
      goto unop;
      
    case PREDECREMENT_EXPR:
      
      incroptab = optab_predecrement_expr;
      goto increment;
      
    case PREINCREMENT_EXPR:
      
      incroptab = optab_preincrement_expr;
      goto increment;
      
    case POSTDECREMENT_EXPR:
      
      incroptab = optab_postdecrement_expr;
      goto increment;
      
    case POSTINCREMENT_EXPR:
      
      incroptab = optab_postincrement_expr;
      goto increment;
      
    case CONSTRUCTOR:
      
      bc_expand_constructor (exp);
      return;
      
    case ERROR_MARK:
    case RTL_EXPR:
      
      return;
      
    case BIND_EXPR:
      {
	tree vars = TREE_OPERAND (exp, 0);
	int vars_need_expansion = 0;
	
	/* Need to open a binding contour here because
	   if there are any cleanups they most be contained here.  */
	expand_start_bindings (0);
	
	/* Mark the corresponding BLOCK for output.  */
	if (TREE_OPERAND (exp, 2) != 0)
	  TREE_USED (TREE_OPERAND (exp, 2)) = 1;
	
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
	
	bc_expand_expr (TREE_OPERAND (exp, 1));
	
	expand_end_bindings (TREE_OPERAND (exp, 0), 0, 0);
	
	return;
      }
      
    default:
      abort ();
    }
  
  abort ();
  
 binop:
  
  bc_expand_binary_operation (binoptab, TREE_TYPE (exp),
			      TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1));
  return;
  
  
 unop:
  
  bc_expand_unary_operation (unoptab, TREE_TYPE (exp), TREE_OPERAND (exp, 0));
  return;
  
  
 andorif:
  
  bc_expand_expr (TREE_OPERAND (exp, 0));
  bc_expand_truth_conversion (TREE_TYPE (TREE_OPERAND (exp, 0)));
  lab = bc_get_bytecode_label ();
  
  bc_emit_instruction (duplicate);
  bc_emit_bytecode (opcode);
  bc_emit_bytecode_labelref (lab);
  
#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
  
  bc_emit_instruction (drop);
  
  bc_expand_expr (TREE_OPERAND (exp, 1));
  bc_expand_truth_conversion (TREE_TYPE (TREE_OPERAND (exp, 1)));
  bc_emit_bytecode_labeldef (lab);
  return;
  
  
 increment:
  
  type = TREE_TYPE (TREE_OPERAND (exp, 0));
  
  /* Push the quantum.  */
  bc_expand_expr (TREE_OPERAND (exp, 1));
  
  /* Convert it to the lvalue's type.  */
  bc_expand_conversion (TREE_TYPE (TREE_OPERAND (exp, 1)), type);
  
  /* Push the address of the lvalue */
  bc_expand_expr (build1 (ADDR_EXPR, TYPE_POINTER_TO (type), TREE_OPERAND (exp, 0)));
  
  /* Perform actual increment */
  bc_expand_increment (incroptab, type);
  return;
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

  if (! POINTER_TYPE_P (TREE_TYPE (exp)))
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
	  if (! POINTER_TYPE_P (TREE_TYPE (exp)))
	    return align;

	  inner = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
	  align = MIN (inner, max_align);
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
	    align = FUNCTION_BOUNDARY;
	  else if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
	    align = DECL_ALIGN (exp);
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

rtx
expand_builtin_return_addr (fndecl_code, count, tem)
     enum built_in_function fndecl_code;
     int count;
     rtx tem;
{
  int i;

  /* Some machines need special handling before we can access
     arbitrary frames.  For example, on the sparc, we must first flush
     all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
  if (count > 0)
    SETUP_FRAME_ADDRESSES ();
#endif

  /* On the sparc, the return address is not in the frame, it is in a
     register.  There is no way to access it off of the current frame
     pointer, but it can be accessed off the previous frame pointer by
     reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
  if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
    count--;
#endif

  /* Scan back COUNT frames to the specified frame.  */
  for (i = 0; i < count; i++)
    {
      /* Assume the dynamic chain pointer is in the word that the
	 frame address points to, unless otherwise specified.  */
#ifdef DYNAMIC_CHAIN_ADDRESS
      tem = DYNAMIC_CHAIN_ADDRESS (tem);
#endif
      tem = memory_address (Pmode, tem);
      tem = copy_to_reg (gen_rtx (MEM, Pmode, tem));
    }

  /* For __builtin_frame_address, return what we've got.  */
  if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
    return tem;

  /* For __builtin_return_address, Get the return address from that
     frame.  */
#ifdef RETURN_ADDR_RTX
  tem = RETURN_ADDR_RTX (count, tem);
#else
  tem = memory_address (Pmode,
			plus_constant (tem, GET_MODE_SIZE (Pmode)));
  tem = gen_rtx (MEM, Pmode, tem);
#endif
  return tem;
}

/* __builtin_setjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.  Much of
   the code below (and for longjmp) is copied from the handling of
   non-local gotos.

   NOTE: This is intended for use by GNAT and the exception handling
   scheme in the compiler and will only work in the method used by
   them.  */

rtx
expand_builtin_setjmp (buf_addr, target)
     rtx buf_addr;
     rtx target;
{
  rtx lab1 = gen_label_rtx (), lab2 = gen_label_rtx ();
  enum machine_mode sa_mode = Pmode, value_mode;
  rtx stack_save;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  int return_pops
    =  RETURN_POPS_ARGS (get_identifier ("__dummy"),
			 build_function_type (void_type_node, NULL_TREE),
			 0);
  rtx next_arg_reg;
  CUMULATIVE_ARGS args_so_far;
  rtx op0;
  int i;

  value_mode = TYPE_MODE (integer_type_node);

#ifdef POINTERS_EXTEND_UNSIGNED
  buf_addr = convert_memory_address (Pmode, buf_addr);
#endif

  buf_addr = force_reg (Pmode, buf_addr);

  if (target == 0 || GET_CODE (target) != REG
      || REGNO (target) < FIRST_PSEUDO_REGISTER)
    target = gen_reg_rtx (value_mode);

  emit_queue ();

  CONST_CALL_P (emit_note (NULL_PTR, NOTE_INSN_SETJMP)) = 1;
  current_function_calls_setjmp = 1;

  /* We store the frame pointer and the address of lab1 in the buffer
     and use the rest of it for the stack save area, which is
     machine-dependent.  */
  emit_move_insn (gen_rtx (MEM, Pmode, buf_addr),
		  virtual_stack_vars_rtx);
  emit_move_insn
    (validize_mem (gen_rtx (MEM, Pmode,
			    plus_constant (buf_addr,
					   GET_MODE_SIZE (Pmode)))),
     gen_rtx (LABEL_REF, Pmode, lab1));

#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    sa_mode = insn_operand_mode[(int) CODE_FOR_save_stack_nonlocal][0];
#endif

  stack_save = gen_rtx (MEM, sa_mode,
			plus_constant (buf_addr,
				       2 * GET_MODE_SIZE (Pmode)));
  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);

#ifdef HAVE_setjmp
  if (HAVE_setjmp)
    emit_insn (gen_setjmp ());
#endif

  /* Set TARGET to zero and branch around the other case.  */
  emit_move_insn (target, const0_rtx);
  emit_jump_insn (gen_jump (lab2));
  emit_barrier ();
  emit_label (lab1);

  /* Note that setjmp clobbers FP when we get here, so we have to make
     sure it's marked as used by this function.  */
  emit_insn (gen_rtx (USE, VOIDmode, hard_frame_pointer_rtx));

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  emit_insn (gen_rtx (CLOBBER, VOIDmode, static_chain_rtx));

  /* Now put in the code to restore the frame pointer, and argument
     pointer, if needed.  The code below is from expand_end_bindings
     in stmt.c; see detailed documentation there.  */
#ifdef HAVE_nonlocal_goto
  if (! HAVE_nonlocal_goto)
#endif
    emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);

  /* Do we need to do something like:
     
     current_function_has_nonlocal_label = 1;

     here?  It seems like we might have to, or some subset of that
     functionality, but I am unsure.  (mrs) */

#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  if (fixed_regs[ARG_POINTER_REGNUM])
    {
#ifdef ELIMINABLE_REGS
      static struct elims {int from, to;} elim_regs[] = ELIMINABLE_REGS;

      for (i = 0; i < sizeof elim_regs / sizeof elim_regs[0]; i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == sizeof elim_regs / sizeof elim_regs [0])
#endif
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.
	     If there hasn't be space allocated for it yet, make
	     some now.  */
	  if (arg_pointer_save_area == 0)
	    arg_pointer_save_area
	      = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
	  emit_move_insn (virtual_incoming_args_rtx,
			  copy_to_reg (arg_pointer_save_area));
	}
    }
#endif

#ifdef HAVE_nonlocal_goto_receiver
  if (HAVE_nonlocal_goto_receiver)
    emit_insn (gen_nonlocal_goto_receiver ());
#endif
  /* The static chain pointer contains the address of dummy function.
     We need to call it here to handle some PIC cases of restoring a
     global pointer.  Then return 1.  */
  op0 = copy_to_mode_reg (Pmode, static_chain_rtx);

  /* We can't actually call emit_library_call here, so do everything
     it does, which isn't much for a libfunc with no args.  */
  op0 = memory_address (FUNCTION_MODE, op0);

  INIT_CUMULATIVE_ARGS (args_so_far, NULL_TREE,
			gen_rtx (SYMBOL_REF, Pmode, "__dummy"), 1);
  next_arg_reg = FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1);

#ifndef ACCUMULATE_OUTGOING_ARGS
#ifdef HAVE_call_pop
  if (HAVE_call_pop)
    emit_call_insn (gen_call_pop (gen_rtx (MEM, FUNCTION_MODE, op0),
				  const0_rtx, next_arg_reg,
				  GEN_INT (return_pops)));
  else
#endif
#endif

#ifdef HAVE_call
    if (HAVE_call)
      emit_call_insn (gen_call (gen_rtx (MEM, FUNCTION_MODE, op0),
				const0_rtx, next_arg_reg, const0_rtx));
    else
#endif
      abort ();

#ifdef HAVE_builtin_setjmp_receiver
  if (HAVE_builtin_setjmp_receiver)
    emit_insn (gen_builtin_setjmp_receiver ());
#endif

  emit_move_insn (target, const1_rtx);
  emit_label (lab2);
  return target;
}


/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

#define CALLED_AS_BUILT_IN(NODE) \
   (!strncmp (IDENTIFIER_POINTER (DECL_NAME (NODE)), "__builtin_", 10))

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
      /* Treat these like sqrt, but only if the user asks for them.  */
      if (! flag_fast_math)
	break;
    case BUILT_IN_FSQRT:
      /* If not optimizing, call the library function.  */
      if (! optimize)
	break;

      if (arglist == 0
	  /* Arg could be wrong type if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != REAL_TYPE)
	break;

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

#ifdef TARGET_EDOM
	  {
#ifdef GEN_ERRNO_RTX
	    rtx errno_rtx = GEN_ERRNO_RTX;
#else
	    rtx errno_rtx
	      = gen_rtx (MEM, word_mode, gen_rtx (SYMBOL_REF, Pmode, "errno"));
#endif

	    emit_move_insn (errno_rtx, GEN_INT (TARGET_EDOM));
	  }
#else
	  /* We can't set errno=EDOM directly; let the library call do it.
	     Pop the arguments right away in case the call gets deleted.  */
	  NO_DEFER_POP;
	  expand_call (exp, target, 0);
	  OK_DEFER_POP;
#endif

	  emit_label (lab1);
	}

      /* Output the entire sequence.  */
      insns = get_insns ();
      end_sequence ();
      emit_insns (insns);
 
      return target;

      /* __builtin_apply_args returns block of memory allocated on
	 the stack into which is stored the arg pointer, structure
	 value address, static chain, and all the registers that might
	 possibly be used in performing a function call.  The code is
	 moved to the start of the function so the incoming values are
	 saved.  */
    case BUILT_IN_APPLY_ARGS:
      /* Don't do __builtin_apply_args more than once in a function.
	 Save the result of the first call and reuse it.  */
      if (apply_args_value != 0)
	return apply_args_value;
      {
	/* When this function is called, it means that registers must be
	   saved on entry to this function.  So we migrate the
	   call to the first insn of this function.  */
	rtx temp;
	rtx seq;

	start_sequence ();
	temp = expand_builtin_apply_args ();
	seq = get_insns ();
	end_sequence ();

	apply_args_value = temp;

	/* Put the sequence after the NOTE that starts the function.
	   If this is inside a SEQUENCE, make the outer-level insn
	   chain current, so the code is placed at the start of the
	   function.  */
	push_topmost_sequence ();
	emit_insns_before (seq, NEXT_INSN (get_insns ()));
	pop_topmost_sequence ();
	return temp;
      }

      /* __builtin_apply (FUNCTION, ARGUMENTS, ARGSIZE) invokes
	 FUNCTION with a copy of the parameters described by
	 ARGUMENTS, and ARGSIZE.  It returns a block of memory
	 allocated on the stack into which is stored all the registers
	 that might possibly be used for returning the result of a
	 function.  ARGUMENTS is the value returned by
	 __builtin_apply_args.  ARGSIZE is the number of bytes of
	 arguments that must be copied.  ??? How should this value be
	 computed?  We'll also need a safe worst case value for varargs
	 functions.  */
    case BUILT_IN_APPLY:
      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
	return const0_rtx;
      else
	{
	  int i;
	  tree t;
	  rtx ops[3];

	  for (t = arglist, i = 0; t; t = TREE_CHAIN (t), i++)
	    ops[i] = expand_expr (TREE_VALUE (t), NULL_RTX, VOIDmode, 0);

	  return expand_builtin_apply (ops[0], ops[1], ops[2]);
	}

      /* __builtin_return (RESULT) causes the function to return the
	 value described by RESULT.  RESULT is address of the block of
	 memory returned by __builtin_apply.  */
    case BUILT_IN_RETURN:
      if (arglist
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  && POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist))))
	expand_builtin_return (expand_expr (TREE_VALUE (arglist),
					    NULL_RTX, VOIDmode, 0));
      return const0_rtx;

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
	    rtx valreg = hard_libcall_value (value_mode);
	    rtx saved_valreg = gen_reg_rtx (value_mode);

	    emit_move_insn (saved_valreg, valreg);
	    temp = expand_call (exp, target, ignore);
	    emit_move_insn (valreg, saved_valreg);
	  }
	else
	  /* Generate the call, putting the value in a pseudo.  */
	  temp = expand_call (exp, target, ignore);
#endif

	seq = get_insns ();
	end_sequence ();

	saveregs_value = temp;

	/* Put the sequence after the NOTE that starts the function.
	   If this is inside a SEQUENCE, make the outer-level insn
	   chain current, so the code is placed at the start of the
	   function.  */
	push_topmost_sequence ();
	emit_insns_before (seq, NEXT_INSN (get_insns ()));
	pop_topmost_sequence ();
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
	      error ("argument of `__builtin_args_info' must be constant");
	    else
	      {
		int wordnum = TREE_INT_CST_LOW (arg);

		if (wordnum < 0 || wordnum >= nwords || TREE_INT_CST_HIGH (arg))
		  error ("argument of `__builtin_args_info' out of range");
		else
		  return GEN_INT (word_ptr[wordnum]);
	      }
	  }
	else
	  error ("missing argument in `__builtin_args_info'");

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
	return expand_expr (result, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_BAD);
#endif
      }

      /* Return the address of the first anonymous stack arg.  */
    case BUILT_IN_NEXT_ARG:
      {
	tree fntype = TREE_TYPE (current_function_decl);

	if ((TYPE_ARG_TYPES (fntype) == 0
	     || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		 == void_type_node))
	    && ! current_function_varargs)
	  {
	    error ("`va_start' used in function with fixed args");
	    return const0_rtx;
	  }

	if (arglist)
	  {
	    tree last_parm = tree_last (DECL_ARGUMENTS (current_function_decl));
	    tree arg = TREE_VALUE (arglist);

	    /* Strip off all nops for the sake of the comparison.  This
	       is not quite the same as STRIP_NOPS.  It does more.  
	       We must also strip off INDIRECT_EXPR for C++ reference
	       parameters.  */
	    while (TREE_CODE (arg) == NOP_EXPR
		   || TREE_CODE (arg) == CONVERT_EXPR
		   || TREE_CODE (arg) == NON_LVALUE_EXPR
		   || TREE_CODE (arg) == INDIRECT_REF)
	      arg = TREE_OPERAND (arg, 0);
	    if (arg != last_parm)
	      warning ("second parameter of `va_start' not last named argument");
	  }
	else if (! current_function_varargs)
	  /* Evidently an out of date version of <stdarg.h>; can't validate
	     va_start's second argument, but can still work as intended.  */
	  warning ("`__builtin_next_arg' called without an argument");
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
	  if (code == UNION_TYPE || code == QUAL_UNION_TYPE)
	    return GEN_INT (union_type_class);
	  if (code == ARRAY_TYPE)
	    {
	      if (TYPE_STRING_FLAG (type))
		return GEN_INT (string_type_class);
	      else
		return GEN_INT (array_type_class);
	    }
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
	{
	  tree arg = TREE_VALUE (arglist);

	  STRIP_NOPS (arg);
	  return (TREE_CODE_CLASS (TREE_CODE (arg)) == 'c'
		  || (TREE_CODE (arg) == ADDR_EXPR
		      && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST)
		  ? const1_rtx : const0_rtx);
	}

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
      else if (TREE_CODE (TREE_VALUE (arglist)) != INTEGER_CST
	       || tree_int_cst_sgn (TREE_VALUE (arglist)) < 0)
	{
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    error ("invalid arg to `__builtin_frame_address'");
	  else
	    error ("invalid arg to `__builtin_return_address'");
	  return const0_rtx;
	}
      else
	{
	  rtx tem = expand_builtin_return_addr (DECL_FUNCTION_CODE (fndecl),
						TREE_INT_CST_LOW (TREE_VALUE (arglist)),
						hard_frame_pointer_rtx);

	  /* Some ports cannot access arbitrary stack frames.  */
	  if (tem == NULL)
	    {
	      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
		warning ("unsupported arg to `__builtin_frame_address'");
	      else
		warning ("unsupported arg to `__builtin_return_address'");
	      return const0_rtx;
	    }

	  /* For __builtin_frame_address, return what we've got.  */
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    return tem;

	  if (GET_CODE (tem) != REG)
	    tem = copy_to_reg (tem);
	  return tem;
	}

    /* Returns the address of the area where the structure is returned.
       0 otherwise.  */
    case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      if (arglist != 0
          || ! AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl)))
          || GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) != MEM)
        return const0_rtx;
      else
        return XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);

    case BUILT_IN_ALLOCA:
      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	break;

      /* Compute the argument.  */
      op0 = expand_expr (TREE_VALUE (arglist), NULL_RTX, VOIDmode, 0);

      /* Allocate the desired space.  */
      return allocate_dynamic_stack_space (op0, target, BITS_PER_UNIT);

    case BUILT_IN_FFS:
      /* If not optimizing, call the library function.  */
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	break;

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
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist))))
	break;
      else
	{
	  tree src = TREE_VALUE (arglist);
	  tree len = c_strlen (src);

	  int align
	    = get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;

	  rtx result, src_rtx, char_rtx;
	  enum machine_mode insn_mode = value_mode, char_mode;
	  enum insn_code icode;

	  /* If the length is known, just return it.  */
	  if (len != 0)
	    return expand_expr (len, target, mode, EXPAND_MEMORY_USE_BAD);

	  /* If SRC is not a pointer type, don't do this operation inline.  */
	  if (align == 0)
	    break;

	  /* Call a function if we can't compute strlen in the right mode.  */

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
				    expand_expr (src, NULL_RTX, ptr_mode,
						 EXPAND_NORMAL));

	  if (! (*insn_operand_predicate[(int)icode][1]) (src_rtx, Pmode))
	    src_rtx = copy_to_mode_reg (Pmode, src_rtx);

	  /* Check the string is readable and has an end.  */
	  if (flag_check_memory_usage)
	    emit_library_call (chkr_check_str_libfunc, 1, VOIDmode, 2,
			       src_rtx, ptr_mode,
			       GEN_INT (MEMORY_USE_RO),
			       TYPE_MODE (integer_type_node));

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
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))))
	break;
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
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || (TREE_CODE (TREE_TYPE (TREE_VALUE
				    (TREE_CHAIN (TREE_CHAIN (arglist)))))
	      != INTEGER_TYPE))
	break;
      else
	{
	  tree dest = TREE_VALUE (arglist);
	  tree src = TREE_VALUE (TREE_CHAIN (arglist));
	  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
	  tree type;

	  int src_align
	    = get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	  int dest_align
	    = get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	  rtx dest_rtx, dest_mem, src_mem, src_rtx, dest_addr, len_rtx;

	  /* If either SRC or DEST is not a pointer type, don't do
	     this operation in-line.  */
	  if (src_align == 0 || dest_align == 0)
	    {
	      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STRCPY)
		TREE_CHAIN (TREE_CHAIN (arglist)) = 0;
	      break;
	    }

	  dest_rtx = expand_expr (dest, NULL_RTX, ptr_mode, EXPAND_SUM);
	  dest_mem = gen_rtx (MEM, BLKmode,
			      memory_address (BLKmode, dest_rtx));
	  /* There could be a void* cast on top of the object.  */
	  while (TREE_CODE (dest) == NOP_EXPR)
	    dest = TREE_OPERAND (dest, 0);
	  type = TREE_TYPE (TREE_TYPE (dest));
	  MEM_IN_STRUCT_P (dest_mem) = AGGREGATE_TYPE_P (type);
	  src_rtx = expand_expr (src, NULL_RTX, ptr_mode, EXPAND_SUM);
	  src_mem = gen_rtx (MEM, BLKmode,
			     memory_address (BLKmode, src_rtx));
	  len_rtx = expand_expr (len, NULL_RTX, VOIDmode, 0);

	  /* Just copy the rights of SRC to the rights of DEST.  */
	  if (flag_check_memory_usage)
	    emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
			       dest_rtx, ptr_mode,
			       src_rtx, ptr_mode,
			       len_rtx, TYPE_MODE (sizetype));

	  /* There could be a void* cast on top of the object.  */
	  while (TREE_CODE (src) == NOP_EXPR)
	    src = TREE_OPERAND (src, 0);
	  type = TREE_TYPE (TREE_TYPE (src));
	  MEM_IN_STRUCT_P (src_mem) = AGGREGATE_TYPE_P (type);

	  /* Copy word part most expediently.  */
	  dest_addr
	    = emit_block_move (dest_mem, src_mem, len_rtx,
			       MIN (src_align, dest_align));

	  if (dest_addr == 0)
	    dest_addr = force_operand (dest_rtx, NULL_RTX);

	  return dest_addr;
	}

    case BUILT_IN_MEMSET:
      /* If not optimizing, call the library function.  */
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	      != INTEGER_TYPE)
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || (INTEGER_CST
	      != (TREE_CODE (TREE_TYPE
			     (TREE_VALUE
			      (TREE_CHAIN (TREE_CHAIN (arglist))))))))
	break;
      else
	{
	  tree dest = TREE_VALUE (arglist);
	  tree val = TREE_VALUE (TREE_CHAIN (arglist));
	  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
	  tree type;

	  int dest_align
	    = get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
	  rtx dest_rtx, dest_mem, dest_addr, len_rtx;

	  /* If DEST is not a pointer type, don't do this 
	     operation in-line.  */
	  if (dest_align == 0)
	    break;

	  /* If VAL is not 0, don't do this operation in-line. */
	  if (expand_expr (val, NULL_RTX, VOIDmode, 0) != const0_rtx)
	    break;

	  dest_rtx = expand_expr (dest, NULL_RTX, ptr_mode, EXPAND_SUM);
	  dest_mem = gen_rtx (MEM, BLKmode,
			      memory_address (BLKmode, dest_rtx));
	  len_rtx = expand_expr (len, NULL_RTX, VOIDmode, 0);

	  /* Just check DST is writable and mark it as readable.  */
	  if (flag_check_memory_usage)
	    emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
			       dest_rtx, ptr_mode,
			       len_rtx, TYPE_MODE (sizetype),
			       GEN_INT (MEMORY_USE_WO),
			       TYPE_MODE (integer_type_node));


	  /* There could be a void* cast on top of the object.  */
	  while (TREE_CODE (dest) == NOP_EXPR)
	    dest = TREE_OPERAND (dest, 0);
	  type = TREE_TYPE (TREE_TYPE (dest));
	  MEM_IN_STRUCT_P (dest_mem) = AGGREGATE_TYPE_P (type);

	  dest_addr = clear_storage (dest_mem, len_rtx, dest_align);

	  if (dest_addr == 0)
	    dest_addr = force_operand (dest_rtx, NULL_RTX);

	  return dest_addr;
	}

/* These comparison functions need an instruction that returns an actual
   index.  An ordinary compare that just sets the condition codes
   is not enough.  */
#ifdef HAVE_cmpstrsi
    case BUILT_IN_STRCMP:
      /* If not optimizing, call the library function.  */
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      /* If we need to check memory accesses, call the library function.  */
      if (flag_check_memory_usage)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))))
	break;
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
      if (!optimize && ! CALLED_AS_BUILT_IN (fndecl))
	break;

      /* If we need to check memory accesses, call the library function.  */
      if (flag_check_memory_usage)
	break;

      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
	break;
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
					  expand_expr (arg1, NULL_RTX,
						       ptr_mode,
						       EXPAND_NORMAL)),
				 gen_rtx (MEM, BLKmode,
					  expand_expr (arg2, NULL_RTX,
						       ptr_mode,
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

    case BUILT_IN_SETJMP:
      if (arglist == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist))))
	break;

      {
	rtx buf_addr = expand_expr (TREE_VALUE (arglist), subtarget,
				    VOIDmode, 0);
	return expand_builtin_setjmp (buf_addr, target);
      }

      /* __builtin_longjmp is passed a pointer to an array of five words
	 and a value, which is a dummy.  It's similar to the C library longjmp
	 function but works with __builtin_setjmp above.  */
    case BUILT_IN_LONGJMP:
      if (arglist == 0 || TREE_CHAIN (arglist) == 0
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist))))
	break;

      {
	tree dummy_id = get_identifier ("__dummy");
	tree dummy_type = build_function_type (void_type_node, NULL_TREE);
	tree dummy_decl = build_decl (FUNCTION_DECL, dummy_id, dummy_type); 
#ifdef POINTERS_EXTEND_UNSIGNED
	rtx buf_addr
	  = force_reg (Pmode,
		       convert_memory_address
		       (Pmode,
			expand_expr (TREE_VALUE (arglist),
				     NULL_RTX, VOIDmode, 0)));
#else
	rtx buf_addr
	  = force_reg (Pmode, expand_expr (TREE_VALUE (arglist),
					   NULL_RTX,
					   VOIDmode, 0));
#endif
	rtx fp = gen_rtx (MEM, Pmode, buf_addr);
	rtx lab = gen_rtx (MEM, Pmode,
			   plus_constant (buf_addr, GET_MODE_SIZE (Pmode)));
	enum machine_mode sa_mode
#ifdef HAVE_save_stack_nonlocal
	  = (HAVE_save_stack_nonlocal
	     ? insn_operand_mode[(int) CODE_FOR_save_stack_nonlocal][0]
	     : Pmode);
#else
	= Pmode;
#endif
	rtx stack = gen_rtx (MEM, sa_mode,
			     plus_constant (buf_addr,
					    2 * GET_MODE_SIZE (Pmode)));

	DECL_EXTERNAL (dummy_decl) = 1;
	TREE_PUBLIC (dummy_decl) = 1;
	make_decl_rtl (dummy_decl, NULL_PTR, 1);

	/* Expand the second expression just for side-effects.  */
	expand_expr (TREE_VALUE (TREE_CHAIN (arglist)),
		     const0_rtx, VOIDmode, 0);

	assemble_external (dummy_decl);
	TREE_USED (dummy_decl) = 1;

	/* Pick up FP, label, and SP from the block and jump.  This code is
	   from expand_goto in stmt.c; see there for detailed comments.  */
#if HAVE_nonlocal_goto
	if (HAVE_nonlocal_goto)
	  emit_insn (gen_nonlocal_goto (fp, lab, stack,
					XEXP (DECL_RTL (dummy_decl), 0)));
      else
#endif
	{
	  lab = copy_to_reg (lab);
	  emit_move_insn (hard_frame_pointer_rtx, fp);
	  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);

	  /* Put in the static chain register the address of the dummy
	     function.  */
	  emit_move_insn (static_chain_rtx, XEXP (DECL_RTL (dummy_decl), 0));
	  emit_insn (gen_rtx (USE, VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
	  emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
	  emit_indirect_jump (lab);
	}

	return const0_rtx;
      }

      /* Various hooks for the DWARF 2 __throw routine.  */
    case BUILT_IN_UNWIND_INIT:
      expand_builtin_unwind_init ();
      return const0_rtx;
    case BUILT_IN_FP:
      return frame_pointer_rtx;
    case BUILT_IN_SP:
      return stack_pointer_rtx;
#ifdef DWARF2_UNWIND_INFO
    case BUILT_IN_DWARF_FP_REGNUM:
      return expand_builtin_dwarf_fp_regnum ();
    case BUILT_IN_DWARF_REG_SIZE:
      return expand_builtin_dwarf_reg_size (TREE_VALUE (arglist), target);
#endif
    case BUILT_IN_FROB_RETURN_ADDR:
      return expand_builtin_frob_return_addr (TREE_VALUE (arglist));
    case BUILT_IN_EXTRACT_RETURN_ADDR:
      return expand_builtin_extract_return_addr (TREE_VALUE (arglist));
    case BUILT_IN_SET_RETURN_ADDR_REG:
      expand_builtin_set_return_addr_reg (TREE_VALUE (arglist));
      return const0_rtx;
    case BUILT_IN_EH_STUB:
      return expand_builtin_eh_stub ();
    case BUILT_IN_SET_EH_REGS:
      expand_builtin_set_eh_regs (TREE_VALUE (arglist),
				  TREE_VALUE (TREE_CHAIN (arglist)));
      return const0_rtx;

    default:			/* just do library call, if unknown builtin */
      error ("built-in function `%s' not currently supported",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */

  return expand_call (exp, target, ignore);
}

/* Built-in functions to perform an untyped call and return.  */

/* For each register that may be used for calling a function, this
   gives a mode used to copy the register's value.  VOIDmode indicates
   the register is not used for calling a function.  If the machine
   has register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_args_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for returning values, this gives
   a mode used to copy the register's value.  VOIDmode indicates the
   register is not used for returning values.  If the machine has
   register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_result_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for calling a function, this
   gives the offset of that register into the block returned by
   __builtin_apply_args.  0 indicates that the register is not
   used for calling a function.  */
static int apply_args_reg_offset[FIRST_PSEUDO_REGISTER];

/* Return the offset of register REGNO into the block returned by 
   __builtin_apply_args.  This is not declared static, since it is
   needed in objc-act.c.  */

int 
apply_args_register_offset (regno)
     int regno;
{
  apply_args_size ();

  /* Arguments are always put in outgoing registers (in the argument
     block) if such make sense.  */
#ifdef OUTGOING_REGNO
  regno = OUTGOING_REGNO(regno);
#endif
  return apply_args_reg_offset[regno];
}

/* Return the size required for the block returned by __builtin_apply_args,
   and initialize apply_args_mode.  */

static int
apply_args_size ()
{
  static int size = -1;
  int align, regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      /* The first value is the incoming arg-pointer.  */
      size = GET_MODE_SIZE (Pmode);

      /* The second value is the structure value address unless this is
	 passed as an "invisible" first argument.  */
      if (struct_value_rtx)
	size += GET_MODE_SIZE (Pmode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_ARG_REGNO_P (regno))
	  {
	    /* Search for the proper mode for copying this register's
	       value.  I'm not sure this is right, but it works so far.  */
	    enum machine_mode best_mode = VOIDmode;

	    for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
		 mode != VOIDmode;
		 mode = GET_MODE_WIDER_MODE (mode))
	      if (HARD_REGNO_MODE_OK (regno, mode)
		  && HARD_REGNO_NREGS (regno, mode) == 1)
		best_mode = mode;

	    if (best_mode == VOIDmode)
	      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
		   mode != VOIDmode;
		   mode = GET_MODE_WIDER_MODE (mode))
		if (HARD_REGNO_MODE_OK (regno, mode)
		    && (mov_optab->handlers[(int) mode].insn_code
			!= CODE_FOR_nothing))
		  best_mode = mode;

	    mode = best_mode;
	    if (mode == VOIDmode)
	      abort ();

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    apply_args_reg_offset[regno] = size;
	    size += GET_MODE_SIZE (mode);
	    apply_args_mode[regno] = mode;
	  }
	else
	  {
	    apply_args_mode[regno] = VOIDmode;
	    apply_args_reg_offset[regno] = 0;
	  }
    }
  return size;
}

/* Return the size required for the block returned by __builtin_apply,
   and initialize apply_result_mode.  */

static int
apply_result_size ()
{
  static int size = -1;
  int align, regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      size = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_VALUE_REGNO_P (regno))
	  {
	    /* Search for the proper mode for copying this register's
	       value.  I'm not sure this is right, but it works so far.  */
	    enum machine_mode best_mode = VOIDmode;

	    for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
		 mode != TImode;
		 mode = GET_MODE_WIDER_MODE (mode))
	      if (HARD_REGNO_MODE_OK (regno, mode))
		best_mode = mode;

	    if (best_mode == VOIDmode)
	      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
		   mode != VOIDmode;
		   mode = GET_MODE_WIDER_MODE (mode))
		if (HARD_REGNO_MODE_OK (regno, mode)
		    && (mov_optab->handlers[(int) mode].insn_code
			!= CODE_FOR_nothing))
		  best_mode = mode;

	    mode = best_mode;
	    if (mode == VOIDmode)
	      abort ();

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_result_mode[regno] = mode;
	  }
	else
	  apply_result_mode[regno] = VOIDmode;

      /* Allow targets that use untyped_call and untyped_return to override
	 the size so that machine-specific information can be stored here.  */
#ifdef APPLY_RESULT_SIZE
      size = APPLY_RESULT_SIZE;
#endif
    }
  return size;
}

#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
/* Create a vector describing the result block RESULT.  If SAVEP is true,
   the result block is used to save the values; otherwise it is used to
   restore the values.  */

static rtx
result_vector (savep, result)
     int savep;
     rtx result;
{
  int regno, size, align, nelts;
  enum machine_mode mode;
  rtx reg, mem;
  rtx *savevec = (rtx *) alloca (FIRST_PSEUDO_REGISTER * sizeof (rtx));
  
  size = nelts = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx (REG, mode, savep ? regno : INCOMING_REGNO (regno));
	mem = change_address (result, mode,
			      plus_constant (XEXP (result, 0), size));
	savevec[nelts++] = (savep
			    ? gen_rtx (SET, VOIDmode, mem, reg)
			    : gen_rtx (SET, VOIDmode, reg, mem));
	size += GET_MODE_SIZE (mode);
      }
  return gen_rtx (PARALLEL, VOIDmode, gen_rtvec_v (nelts, savevec));
}
#endif /* HAVE_untyped_call or HAVE_untyped_return */

/* Save the state required to perform an untyped call with the same
   arguments as were passed to the current function.  */

static rtx
expand_builtin_apply_args ()
{
  rtx registers;
  int size, align, regno;
  enum machine_mode mode;

  /* Create a block where the arg-pointer, structure value address,
     and argument registers can be saved.  */
  registers = assign_stack_local (BLKmode, apply_args_size (), -1);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    size += GET_MODE_SIZE (Pmode);

  /* Save each register used in calling a function to the block.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	rtx tem;

	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;

	tem = gen_rtx (REG, mode, INCOMING_REGNO (regno));

#ifdef STACK_REGS
        /* For reg-stack.c's stack register household.
	   Compare with a similar piece of code in function.c.  */

        emit_insn (gen_rtx (USE, mode, tem));
#endif

	emit_move_insn (change_address (registers, mode,
					plus_constant (XEXP (registers, 0),
						       size)),
			tem);
	size += GET_MODE_SIZE (mode);
      }

  /* Save the arg pointer to the block.  */
  emit_move_insn (change_address (registers, Pmode, XEXP (registers, 0)),
		  copy_to_reg (virtual_incoming_args_rtx));
  size = GET_MODE_SIZE (Pmode);

  /* Save the structure value address unless this is passed as an
     "invisible" first argument.  */
  if (struct_value_incoming_rtx)
    {
      emit_move_insn (change_address (registers, Pmode,
				      plus_constant (XEXP (registers, 0),
						     size)),
		      copy_to_reg (struct_value_incoming_rtx));
      size += GET_MODE_SIZE (Pmode);
    }

  /* Return the address of the block.  */
  return copy_addr_to_reg (XEXP (registers, 0));
}

/* Perform an untyped call and save the state required to perform an
   untyped return of whatever value was returned by the given function.  */

static rtx
expand_builtin_apply (function, arguments, argsize)
     rtx function, arguments, argsize;
{
  int size, align, regno;
  enum machine_mode mode;
  rtx incoming_args, result, reg, dest, call_insn;
  rtx old_stack_level = 0;
  rtx call_fusage = 0;

  /* Create a block where the return registers can be saved.  */
  result = assign_stack_local (BLKmode, apply_result_size (), -1);

  /* ??? The argsize value should be adjusted here.  */

  /* Fetch the arg pointer from the ARGUMENTS block.  */
  incoming_args = gen_reg_rtx (Pmode);
  emit_move_insn (incoming_args,
		  gen_rtx (MEM, Pmode, arguments));
#ifndef STACK_GROWS_DOWNWARD
  incoming_args = expand_binop (Pmode, sub_optab, incoming_args, argsize,
				incoming_args, 0, OPTAB_LIB_WIDEN);
#endif

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* Push a new argument block and copy the arguments.  */
  do_pending_stack_adjust ();

  /* Save the stack with nonlocal if available */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_save (SAVE_NONLOCAL, &old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);

  /* Push a block of memory onto the stack to store the memory arguments.
     Save the address in a register, and copy the memory arguments.  ??? I
     haven't figured out how the calling convention macros effect this,
     but it's likely that the source and/or destination addresses in
     the block copy will need updating in machine specific ways.  */
  dest = allocate_dynamic_stack_space (argsize, 0, 0);
  emit_block_move (gen_rtx (MEM, BLKmode, dest),
		   gen_rtx (MEM, BLKmode, incoming_args),
		   argsize,
		   PARM_BOUNDARY / BITS_PER_UNIT);

  /* Refer to the argument block.  */
  apply_args_size ();
  arguments = gen_rtx (MEM, BLKmode, arguments);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    size += GET_MODE_SIZE (Pmode);

  /* Restore each of the registers previously saved.  Make USE insns
     for each of these registers for use in making the call.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx (REG, mode, regno);
	emit_move_insn (reg,
			change_address (arguments, mode,
					plus_constant (XEXP (arguments, 0),
						       size)));

	use_reg (&call_fusage, reg);
	size += GET_MODE_SIZE (mode);
      }

  /* Restore the structure value address unless this is passed as an
     "invisible" first argument.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    {
      rtx value = gen_reg_rtx (Pmode);
      emit_move_insn (value,
		      change_address (arguments, Pmode,
				      plus_constant (XEXP (arguments, 0),
						     size)));
      emit_move_insn (struct_value_rtx, value);
      if (GET_CODE (struct_value_rtx) == REG)
	  use_reg (&call_fusage, struct_value_rtx);
      size += GET_MODE_SIZE (Pmode);
    }

  /* All arguments and registers used for the call are set up by now!  */
  function = prepare_call_address (function, NULL_TREE, &call_fusage, 0);

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (function) != SYMBOL_REF)
    function = memory_address (FUNCTION_MODE, function);

  /* Generate the actual call instruction and save the return value.  */
#ifdef HAVE_untyped_call
  if (HAVE_untyped_call)
    emit_call_insn (gen_untyped_call (gen_rtx (MEM, FUNCTION_MODE, function),
				      result, result_vector (1, result)));
  else
#endif
#ifdef HAVE_call_value
  if (HAVE_call_value)
    {
      rtx valreg = 0;

      /* Locate the unique return register.  It is not possible to
	 express a call that sets more than one return register using
	 call_value; use untyped_call for that.  In fact, untyped_call
	 only needs to save the return registers in the given block.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((mode = apply_result_mode[regno]) != VOIDmode)
	  {
	    if (valreg)
	      abort (); /* HAVE_untyped_call required.  */
	    valreg = gen_rtx (REG, mode, regno);
	  }

      emit_call_insn (gen_call_value (valreg,
				      gen_rtx (MEM, FUNCTION_MODE, function),
				      const0_rtx, NULL_RTX, const0_rtx));

      emit_move_insn (change_address (result, GET_MODE (valreg),
				      XEXP (result, 0)),
		      valreg);
    }
  else
#endif
    abort ();

  /* Find the CALL insn we just emitted.  */
  for (call_insn = get_last_insn ();
       call_insn && GET_CODE (call_insn) != CALL_INSN;
       call_insn = PREV_INSN (call_insn))
    ;

  if (! call_insn)
    abort ();

  /* Put the register usage information on the CALL.  If there is already
     some usage information, put ours at the end.  */
  if (CALL_INSN_FUNCTION_USAGE (call_insn))
    {
      rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (call_insn); XEXP (link, 1) != 0;
	   link = XEXP (link, 1))
	;

      XEXP (link, 1) = call_fusage;
    }
  else
    CALL_INSN_FUNCTION_USAGE (call_insn) = call_fusage;

  /* Restore the stack.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_restore (SAVE_NONLOCAL, old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);

  /* Return the address of the result block.  */
  return copy_addr_to_reg (XEXP (result, 0));
}

/* Perform an untyped return.  */

static void
expand_builtin_return (result)
     rtx result;
{
  int size, align, regno;
  enum machine_mode mode;
  rtx reg;
  rtx call_fusage = 0;

  apply_result_size ();
  result = gen_rtx (MEM, BLKmode, result);

#ifdef HAVE_untyped_return
  if (HAVE_untyped_return)
    {
      emit_jump_insn (gen_untyped_return (result, result_vector (0, result)));
      emit_barrier ();
      return;
    }
#endif

  /* Restore the return value and note that each value is used.  */
  size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx (REG, mode, INCOMING_REGNO (regno));
	emit_move_insn (reg,
			change_address (result, mode,
					plus_constant (XEXP (result, 0),
						       size)));

	push_to_sequence (call_fusage);
	emit_insn (gen_rtx (USE, VOIDmode, reg));
	call_fusage = get_insns ();
	end_sequence ();
	size += GET_MODE_SIZE (mode);
      }

  /* Put the USE insns before the return.  */
  emit_insns (call_fusage);

  /* Return whatever values was restored by jumping directly to the end
     of the function.  */
  expand_null_return ();
}

/* Expand code for a post- or pre- increment or decrement
   and return the RTX for the result.
   POST is 1 for postinc/decrements and 0 for preinc/decrements.  */

static rtx
expand_increment (exp, post, ignore)
     register tree exp;
     int post, ignore;
{
  register rtx op0, op1;
  register rtx temp, value;
  register tree incremented = TREE_OPERAND (exp, 0);
  optab this_optab = add_optab;
  int icode;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  int op0_is_copy = 0;
  int single_insn = 0;
  /* 1 means we can't store into OP0 directly,
     because it is a subreg narrower than a word,
     and we don't dare clobber the rest of the word.  */
  int bad_subreg = 0;

  if (output_bytecode)
    {
      bc_expand_expr (exp);
      return NULL_RTX;
    }

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
     and insns were generated in computing it.   */

  temp = get_last_insn ();
  op0 = expand_expr (incremented, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_RW);

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
  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode,
		     EXPAND_MEMORY_USE_BAD);

  /* Decide whether incrementing or decrementing.  */
  if (TREE_CODE (exp) == POSTDECREMENT_EXPR
      || TREE_CODE (exp) == PREDECREMENT_EXPR)
    this_optab = sub_optab;

  /* Convert decrement by a constant into a negative increment.  */
  if (this_optab == sub_optab
      && GET_CODE (op1) == CONST_INT)
    {
      op1 = GEN_INT (- INTVAL (op1));
      this_optab = add_optab;
    }

  /* For a preincrement, see if we can do this with a single instruction.  */
  if (!post)
    {
      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing
	  /* Make sure that OP0 is valid for operands 0 and 1
	     of the insn we want to queue.  */
	  && (*insn_operand_predicate[icode][0]) (op0, mode)
	  && (*insn_operand_predicate[icode][1]) (op0, mode)
	  && (*insn_operand_predicate[icode][2]) (op1, mode))
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
	  && (*insn_operand_predicate[icode][0]) (op0, mode)
	  && (*insn_operand_predicate[icode][1]) (op0, mode))
	{
	  if (! (*insn_operand_predicate[icode][2]) (op1, mode))
	    op1 = force_reg (mode, op1);

	  return enqueue_insn (op0, GEN_FCN (icode) (op0, op0, op1));
	}
      if (icode != (int) CODE_FOR_nothing && GET_CODE (op0) == MEM)
	{
	  rtx addr = (general_operand (XEXP (op0, 0), mode)
		      ? force_reg (Pmode, XEXP (op0, 0))
		      : copy_to_reg (XEXP (op0, 0)));
	  rtx temp, result;

	  op0 = change_address (op0, VOIDmode, addr);
	  temp = force_reg (GET_MODE (op0), op0);
	  if (! (*insn_operand_predicate[icode][2]) (op1, mode))
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
  op1 = expand_binop (mode, this_optab, value, op1,
  		      flag_check_memory_usage ? NULL_RTX : op0,
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
      if (CALL_EXPR_RTL (exp) != 0
	  /* Do nothing if the call returns a variable-sized object.  */
	  || TREE_CODE (TYPE_SIZE (TREE_TYPE(exp))) != INTEGER_CST
	  /* Do nothing to built-in functions.  */
	  || (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		  == FUNCTION_DECL)
	      && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))))
	return;

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
    case CLEANUP_POINT_EXPR:
    case TRY_CATCH_EXPR:
      return;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return;
      
    default:
      break;
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
  if (optimize > 0
      && ! flag_omit_frame_pointer && EXIT_IGNORE_STACK
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
  enum machine_mode mode;

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
      {
	int bitsize, bitpos, unsignedp;
	enum machine_mode mode;
	tree type;
	tree offset;
	int volatilep = 0;
	int alignment;

	/* Get description of this reference.  We don't actually care
	   about the underlying object here.  */
	get_inner_reference (exp, &bitsize, &bitpos, &offset,
			     &mode, &unsignedp, &volatilep,
			     &alignment);

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

	if (integer_zerop (TREE_OPERAND (exp, 1)))
	  do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
		 || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
	  do_jump
	    (fold
	     (build (TRUTH_ANDIF_EXPR, TREE_TYPE (exp),
		     fold (build (EQ_EXPR, TREE_TYPE (exp),
				  fold (build1 (REALPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 0))),
				  fold (build1 (REALPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 1))))),
		     fold (build (EQ_EXPR, TREE_TYPE (exp),
				  fold (build1 (IMAGPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 0))),
				  fold (build1 (IMAGPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 1))))))),
	     if_false_label, if_true_label);
	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
		 && !can_compare_p (TYPE_MODE (inner_type)))
	  do_jump_by_parts_equality (exp, if_false_label, if_true_label);
	else
	  comparison = compare (exp, EQ, EQ);
	break;
      }

    case NE_EXPR:
      {
	tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

	if (integer_zerop (TREE_OPERAND (exp, 1)))
	  do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
		 || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
	  do_jump
	    (fold
	     (build (TRUTH_ORIF_EXPR, TREE_TYPE (exp),
		     fold (build (NE_EXPR, TREE_TYPE (exp),
				  fold (build1 (REALPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 0))),
				  fold (build1 (REALPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 1))))),
		     fold (build (NE_EXPR, TREE_TYPE (exp),
				  fold (build1 (IMAGPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 0))),
				  fold (build1 (IMAGPART_EXPR,
						TREE_TYPE (inner_type),
						TREE_OPERAND (exp, 1))))))),
	     if_false_label, if_true_label);
	else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
		 && !can_compare_p (TYPE_MODE (inner_type)))
	  do_jump_by_parts_equality (exp, if_true_label, if_false_label);
	else
	  comparison = compare (exp, NE, NE);
	break;
      }

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
      rtx comp = compare_from_rtx (part, const0_rtx, EQ, 1, word_mode,
				   NULL_RTX, 0);

      if (comp == const_true_rtx)
	emit_jump (if_false_label);
      else if (comp == const0_rtx)
	emit_jump (if_true_label);
      else
	do_jump_for_compare (comp, if_false_label, if_true_label);

      return;
    }

  /* If we couldn't do the "or" simply, do this with a series of compares.  */
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
      rtx prev = get_last_insn ();
      rtx branch = 0;

      /* Output the branch with the opposite condition.  Then try to invert
	 what is generated.  If more than one insn is a branch, or if the
	 branch is not the last insn written, abort. If we can't invert
	 the branch, emit make a true label, redirect this jump to that,
	 emit a jump to the false label and define the true label.  */

      if (bcc_gen_fctn[(int) GET_CODE (comparison)] != 0)
	emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (comparison)])(if_false_label));
      else
	abort ();

      /* Here we get the first insn that was just emitted.  It used to be  the
	 case that, on some machines, emitting the branch would discard
	 the previous compare insn and emit a replacement.  This isn't
	 done anymore, but abort if we see that PREV is deleted.  */

      if (prev == 0)
	insn = get_insns ();
      else if (INSN_DELETED_P (prev))
	abort ();
      else
	insn = NEXT_INSN (prev);

      for (; insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == JUMP_INSN)
	  {
	    if (branch)
	      abort ();
	    branch = insn;
	  }

      if (branch != get_last_insn ())
	abort ();

      JUMP_LABEL (branch) = if_false_label;
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

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* If function pointers need to be "canonicalized" before they can
     be reliably compared, then canonicalize them.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0)))
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	  == FUNCTION_TYPE))
    {
      rtx new_op0 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op0, op0));
      op0 = new_op0;
    }

  if (HAVE_canonicalize_funcptr_for_compare
      && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 1)))
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 1))))
	  == FUNCTION_TYPE))
    {
      rtx new_op1 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op1, op1));
      op1 = new_op1;
    }
#endif

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
  rtx tem;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if ((CONSTANT_P (op0) && ! CONSTANT_P (op1))
      || (GET_CODE (op0) == CONST_INT && GET_CODE (op1) != CONST_INT))
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

  /* We won't bother with store-flag operations involving function pointers
     when function pointers must be canonicalized before comparisons.  */
#ifdef HAVE_canonicalize_funcptr_for_compare
  if (HAVE_canonicalize_funcptr_for_compare
      && ((POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0)))
	   && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	       == FUNCTION_TYPE))
	  || (POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 1)))
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
	  && (bitnum + TREE_INT_CST_LOW (TREE_OPERAND (inner, 1))
	      < TYPE_PRECISION (type)))
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

      if (subtarget == 0 || GET_CODE (subtarget) != REG
	  || GET_MODE (subtarget) != operand_mode
	  || ! safe_from_p (subtarget, inner, 1))
	subtarget = 0;

      op0 = expand_expr (inner, subtarget, VOIDmode, 0);

      if (bitnum != 0)
	op0 = expand_shift (RSHIFT_EXPR, GET_MODE (op0), op0,
			    size_int (bitnum), subtarget, ops_unsignedp);

      if (GET_MODE (op0) != mode)
	op0 = convert_to_mode (mode, op0, ops_unsignedp);

      if ((code == EQ && ! invert) || (code == NE && invert))
	op0 = expand_binop (mode, xor_optab, op0, const1_rtx, subtarget,
			    ops_unsignedp, OPTAB_LIB_WIDEN);

      /* Put the AND last so it can combine with more things.  */
      if (bitnum != TYPE_PRECISION (type) - 1)
	op0 = expand_and (op0, const1_rtx, subtarget);

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

  emit_cmp_insn (index, range, GTU, NULL_RTX, mode, 1, 0);
  emit_jump_insn (gen_bgtu (default_label));

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
  index = gen_rtx (PLUS, Pmode,
		   gen_rtx (MULT, Pmode, index,
			    GEN_INT (GET_MODE_SIZE (CASE_VECTOR_MODE))),
		   gen_rtx (LABEL_REF, Pmode, table_label));
#ifdef PIC_CASE_VECTOR_ADDRESS
  if (flag_pic)
    index = PIC_CASE_VECTOR_ADDRESS (index);
  else
#endif
    index = memory_address_noforce (CASE_VECTOR_MODE, index);
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


/* Emit a suitable bytecode to load a value from memory, assuming a pointer
   to that value is on the top of the stack. The resulting type is TYPE, and
   the source declaration is DECL.  */

void
bc_load_memory (type, decl)
     tree type, decl;
{
  enum bytecode_opcode opcode;
  
  
  /* Bit fields are special.  We only know about signed and
     unsigned ints, and enums.  The latter are treated as
     signed integers.  */
  
  if (DECL_BIT_FIELD (decl))
    if (TREE_CODE (type) == ENUMERAL_TYPE
	|| TREE_CODE (type) == INTEGER_TYPE)
      opcode = TREE_UNSIGNED (type) ? zxloadBI : sxloadBI;
    else
      abort ();
  else
    /* See corresponding comment in bc_store_memory.  */
    if (TYPE_MODE (type) == BLKmode
	|| TYPE_MODE (type) == VOIDmode)
      return;
    else
      opcode = mode_to_load_map [(int) TYPE_MODE (type)];

  if (opcode == neverneverland)
    abort ();
  
  bc_emit_bytecode (opcode);
  
#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}


/* Store the contents of the second stack slot to the address in the
   top stack slot.  DECL is the declaration of the destination and is used
   to determine whether we're dealing with a bitfield.  */

void
bc_store_memory (type, decl)
     tree type, decl;
{
  enum bytecode_opcode opcode;
  
  
  if (DECL_BIT_FIELD (decl))
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE
	  || TREE_CODE (type) == INTEGER_TYPE)
	opcode = sstoreBI;
      else
	abort ();
    }
  else
    if (TYPE_MODE (type) == BLKmode)
      {
	/* Copy structure.  This expands to a block copy instruction, storeBLK.
	   In addition to the arguments expected by the other store instructions,
	   it also expects a type size (SImode) on top of the stack, which is the
	   structure size in size units (usually bytes).  The two first arguments
	   are already on the stack; so we just put the size on level 1.  For some
	   other languages, the size may be variable, this is why we don't encode
	   it as a storeBLK literal, but rather treat it as a full-fledged expression.  */
	
	bc_expand_expr (TYPE_SIZE (type));
	opcode = storeBLK;
      }
    else
      opcode = mode_to_store_map [(int) TYPE_MODE (type)];

  if (opcode == neverneverland)
    abort ();

  bc_emit_bytecode (opcode);
  
#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}


/* Allocate local stack space sufficient to hold a value of the given
   SIZE at alignment boundary ALIGNMENT bits.  ALIGNMENT must be an
   integral power of 2.  A special case is locals of type VOID, which
   have size 0 and alignment 1 - any "voidish" SIZE or ALIGNMENT is
   remapped into the corresponding attribute of SI.  */

rtx
bc_allocate_local (size, alignment)
     int size, alignment;
{
  rtx retval;
  int byte_alignment;

  if (size < 0)
    abort ();

  /* Normalize size and alignment  */
  if (!size)
    size = UNITS_PER_WORD;

  if (alignment < BITS_PER_UNIT)
    byte_alignment = 1 << (INT_ALIGN - 1);
  else
    /* Align */
    byte_alignment = alignment / BITS_PER_UNIT;

  if (local_vars_size & (byte_alignment - 1))
    local_vars_size += byte_alignment - (local_vars_size & (byte_alignment - 1));

  retval = bc_gen_rtx ((char *) 0, local_vars_size, (struct bc_label *) 0);
  local_vars_size += size;

  return retval;
}


/* Allocate variable-sized local array. Variable-sized arrays are
   actually pointers to the address in memory where they are stored.  */

rtx
bc_allocate_variable_array (size)
     tree size;
{
  rtx retval;
  const int ptralign = (1 << (PTR_ALIGN - 1));

  /* Align pointer */
  if (local_vars_size & ptralign)
    local_vars_size +=  ptralign - (local_vars_size & ptralign);

  /* Note down local space needed: pointer to block; also return
     dummy rtx */

  retval = bc_gen_rtx ((char *) 0, local_vars_size, (struct bc_label *) 0);
  local_vars_size += POINTER_SIZE / BITS_PER_UNIT;
  return retval;
}


/* Push the machine address for the given external variable offset.  */

void
bc_load_externaddr (externaddr)
     rtx externaddr;
{
  bc_emit_bytecode (constP);
  bc_emit_code_labelref (BYTECODE_LABEL (externaddr),
			 BYTECODE_BC_LABEL (externaddr)->offset);

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}


/* Like above, but expects an IDENTIFIER.  */

void
bc_load_externaddr_id (id, offset)
     tree id;
     int offset;
{
  if (!IDENTIFIER_POINTER (id))
    abort ();

  bc_emit_bytecode (constP);
  bc_emit_code_labelref (xstrdup (IDENTIFIER_POINTER (id)), offset);

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}


/* Push the machine address for the given local variable offset.  */

void
bc_load_localaddr (localaddr)
     rtx localaddr;
{
  bc_emit_instruction (localP, (HOST_WIDE_INT) BYTECODE_BC_LABEL (localaddr)->offset);
}


/* Push the machine address for the given parameter offset.
   NOTE: offset is in bits.  */

void
bc_load_parmaddr (parmaddr)
     rtx parmaddr;
{
  bc_emit_instruction (argP, ((HOST_WIDE_INT) BYTECODE_BC_LABEL (parmaddr)->offset
			      / BITS_PER_UNIT));
}


/* Convert a[i] into *(a + i).  */

tree
bc_canonicalize_array_ref (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);
  tree array_adr = build1 (ADDR_EXPR, TYPE_POINTER_TO (type),
			   TREE_OPERAND (exp, 0));
  tree index = TREE_OPERAND (exp, 1);


  /* Convert the integer argument to a type the same size as a pointer
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (index)) != POINTER_SIZE)
    index = convert (type_for_size (POINTER_SIZE, 0), index);

  /* The array address isn't volatile even if the array is.
     (Of course this isn't terribly relevant since the bytecode
     translator treats nearly everything as volatile anyway.)  */
  TREE_THIS_VOLATILE (array_adr) = 0;

  return build1 (INDIRECT_REF, type,
		 fold (build (PLUS_EXPR,
			      TYPE_POINTER_TO (type),
			      array_adr,
			      fold (build (MULT_EXPR,
					   TYPE_POINTER_TO (type),
					   index,
					   size_in_bytes (type))))));
}


/* Load the address of the component referenced by the given
   COMPONENT_REF expression.

   Returns innermost lvalue.  */

tree
bc_expand_component_address (exp)
     tree exp;
{
  tree tem, chain;
  enum machine_mode mode;
  int bitpos = 0;
  HOST_WIDE_INT SIval;


  tem = TREE_OPERAND (exp, 1);
  mode = DECL_MODE (tem);


  /* Compute cumulative bit offset for nested component refs
     and array refs, and find the ultimate containing object.  */

  for (tem = exp;; tem = TREE_OPERAND (tem, 0))
    {
      if (TREE_CODE (tem) == COMPONENT_REF)
	bitpos += TREE_INT_CST_LOW (DECL_FIELD_BITPOS (TREE_OPERAND (tem, 1)));
      else
	if (TREE_CODE (tem) == ARRAY_REF
	    && TREE_CODE (TREE_OPERAND (tem, 1)) == INTEGER_CST
	    && TREE_CODE (TYPE_SIZE (TREE_TYPE (tem))) == INTEGER_CST)

	  bitpos += (TREE_INT_CST_LOW (TREE_OPERAND (tem, 1))
		     * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tem)))
		     /* * TYPE_SIZE_UNIT (TREE_TYPE (tem)) */);
	else
	  break;
    }

  bc_expand_expr (tem);


  /* For bitfields also push their offset and size */
  if (DECL_BIT_FIELD (TREE_OPERAND (exp, 1)))
    bc_push_offset_and_size (bitpos, /* DECL_SIZE_UNIT */ (TREE_OPERAND (exp, 1)));
  else
    if (SIval = bitpos / BITS_PER_UNIT)
      bc_emit_instruction (addconstPSI, SIval);

  return (TREE_OPERAND (exp, 1));
}


/* Emit code to push two SI constants */

void
bc_push_offset_and_size (offset, size)
     HOST_WIDE_INT offset, size;
{
  bc_emit_instruction (constSI, offset);
  bc_emit_instruction (constSI, size);
}


/* Emit byte code to push the address of the given lvalue expression to
   the stack.  If it's a bit field, we also push offset and size info.

   Returns innermost component, which allows us to determine not only
   its type, but also whether it's a bitfield.  */

tree
bc_expand_address (exp)
     tree exp;
{
  /* Safeguard */
  if (!exp || TREE_CODE (exp) == ERROR_MARK)
    return (exp);


  switch (TREE_CODE (exp))
    {
    case ARRAY_REF:

      return (bc_expand_address (bc_canonicalize_array_ref (exp)));

    case COMPONENT_REF:

      return (bc_expand_component_address (exp));

    case INDIRECT_REF:

      bc_expand_expr (TREE_OPERAND (exp, 0));

      /* For variable-sized types: retrieve pointer.  Sometimes the
	 TYPE_SIZE tree is NULL.  Is this a bug or a feature?  Let's
	 also make sure we have an operand, just in case...  */

      if (TREE_OPERAND (exp, 0)
	  && TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)))) != INTEGER_CST)
	bc_emit_instruction (loadP);

      /* If packed, also return offset and size */
      if (DECL_BIT_FIELD (TREE_OPERAND (exp, 0)))
	
	bc_push_offset_and_size (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (TREE_OPERAND (exp, 0))),
				 TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (exp, 0))));

      return (TREE_OPERAND (exp, 0));

    case FUNCTION_DECL:

      bc_load_externaddr_id (DECL_ASSEMBLER_NAME (exp),
			     BYTECODE_BC_LABEL (DECL_RTL (exp))->offset);
      break;

    case PARM_DECL:

      bc_load_parmaddr (DECL_RTL (exp));

      /* For variable-sized types: retrieve pointer */
      if (TYPE_SIZE (TREE_TYPE (exp))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST)
	bc_emit_instruction (loadP);

      /* If packed, also return offset and size */
      if (DECL_BIT_FIELD (exp))
	bc_push_offset_and_size (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (exp)),
				 TREE_INT_CST_LOW (DECL_SIZE (exp)));

      break;

    case RESULT_DECL:

      bc_emit_instruction (returnP);
      break;

    case VAR_DECL:

#if 0
      if (BYTECODE_LABEL (DECL_RTL (exp)))
	bc_load_externaddr (DECL_RTL (exp));
#endif

      if (DECL_EXTERNAL (exp))
	bc_load_externaddr_id (DECL_ASSEMBLER_NAME (exp),
			       (BYTECODE_BC_LABEL (DECL_RTL (exp)))->offset);
      else
	bc_load_localaddr (DECL_RTL (exp));

      /* For variable-sized types: retrieve pointer */
      if (TYPE_SIZE (TREE_TYPE (exp))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST)
	bc_emit_instruction (loadP);

      /* If packed, also return offset and size */
      if (DECL_BIT_FIELD (exp))
	bc_push_offset_and_size (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (exp)),
				 TREE_INT_CST_LOW (DECL_SIZE (exp)));
      
      break;

    case STRING_CST:
      {
	rtx r;
	
	bc_emit_bytecode (constP);
	r = output_constant_def (exp);
	bc_emit_code_labelref (BYTECODE_LABEL (r), BYTECODE_BC_LABEL (r)->offset);

#ifdef DEBUG_PRINT_CODE
	fputc ('\n', stderr);
#endif
      }
      break;

    default:

      abort();
      break;
    }

  /* Most lvalues don't have components.  */
  return (exp);
}


/* Emit a type code to be used by the runtime support in handling
   parameter passing.   The type code consists of the machine mode
   plus the minimal alignment shifted left 8 bits.  */

tree
bc_runtime_type_code (type)
     tree type;
{
  int val;

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case RECORD_TYPE:

      val = (int) TYPE_MODE (type) | TYPE_ALIGN (type) << 8;
      break;

    case ERROR_MARK:

      val = 0;
      break;

    default:

      abort ();
    }
  return build_int_2 (val, 0);
}


/* Generate constructor label */

char *
bc_gen_constr_label ()
{
  static int label_counter;
  static char label[20];

  sprintf (label, "*LR%d", label_counter++);

  return (obstack_copy0 (&permanent_obstack, label, strlen (label)));
}


/* Evaluate constructor CONSTR and return pointer to it on level one.  We
   expand the constructor data as static data, and push a pointer to it.
   The pointer is put in the pointer table and is retrieved by a constP
   bytecode instruction.  We then loop and store each constructor member in
   the corresponding component.  Finally, we return the original pointer on
   the stack.  */

void
bc_expand_constructor (constr)
     tree constr;
{
  char *l;
  HOST_WIDE_INT ptroffs;
  rtx constr_rtx;

  
  /* Literal constructors are handled as constants, whereas
     non-literals are evaluated and stored element by element
     into the data segment.  */
  
  /* Allocate space in proper segment and push pointer to space on stack.
   */

  l = bc_gen_constr_label ();

  if (TREE_CONSTANT (constr))
    {
      text_section ();

      bc_emit_const_labeldef (l);
      bc_output_constructor (constr, int_size_in_bytes (TREE_TYPE (constr)));
    }
  else
    {
      data_section ();

      bc_emit_data_labeldef (l);
      bc_output_data_constructor (constr);
    }

  
  /* Add reference to pointer table and recall pointer to stack;
     this code is common for both types of constructors: literals
     and non-literals.  */

  ptroffs = bc_define_pointer (l);
  bc_emit_instruction (constP, ptroffs);

  /* This is all that has to be done if it's a literal.  */
  if (TREE_CONSTANT (constr))
    return;


  /* At this point, we have the pointer to the structure on top of the stack.
     Generate sequences of store_memory calls for the constructor.  */
  
  /* constructor type is structure */
  if (TREE_CODE (TREE_TYPE (constr)) == RECORD_TYPE)
    {
      register tree elt;
      
      /* If the constructor has fewer fields than the structure,
	 clear the whole structure first.  */
      
      if (list_length (CONSTRUCTOR_ELTS (constr))
	  != list_length (TYPE_FIELDS (TREE_TYPE (constr))))
	{
	  bc_emit_instruction (duplicate);
	  bc_emit_instruction (constSI, (HOST_WIDE_INT) int_size_in_bytes (TREE_TYPE (constr)));
	  bc_emit_instruction (clearBLK);
	}
      
      /* Store each element of the constructor into the corresponding
	 field of TARGET.  */
      
      for (elt = CONSTRUCTOR_ELTS (constr); elt; elt = TREE_CHAIN (elt))
	{
	  register tree field = TREE_PURPOSE (elt);
	  register enum machine_mode mode;
	  int bitsize;
	  int bitpos;
	  int unsignedp;
	  
	  bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) /* * DECL_SIZE_UNIT (field) */;
	  mode = DECL_MODE (field);
	  unsignedp = TREE_UNSIGNED (field);

	  bitpos = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  
	  bc_store_field (elt, bitsize, bitpos, mode, TREE_VALUE (elt), TREE_TYPE (TREE_VALUE (elt)),
			  /* The alignment of TARGET is
			     at least what its type requires.  */
			  VOIDmode, 0,
			  TYPE_ALIGN (TREE_TYPE (constr)) / BITS_PER_UNIT,
			  int_size_in_bytes (TREE_TYPE (constr)));
	}
    }
  else
    
    /* Constructor type is array */
    if (TREE_CODE (TREE_TYPE (constr)) == ARRAY_TYPE)
      {
	register tree elt;
	register int i;
	tree domain = TYPE_DOMAIN (TREE_TYPE (constr));
	int minelt = TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain));
	int maxelt = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain));
	tree elttype = TREE_TYPE (TREE_TYPE (constr));
	
	/* If the constructor has fewer fields than the structure,
	   clear the whole structure first.  */
	
	if (list_length (CONSTRUCTOR_ELTS (constr)) < maxelt - minelt + 1)
	  {
	    bc_emit_instruction (duplicate);
	    bc_emit_instruction (constSI, (HOST_WIDE_INT) int_size_in_bytes (TREE_TYPE (constr)));
	    bc_emit_instruction (clearBLK);
	  }
	
	
	/* Store each element of the constructor into the corresponding
	   element of TARGET, determined by counting the elements.  */
	
	for (elt = CONSTRUCTOR_ELTS (constr), i = 0;
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
	    
	    bitpos = (i * TREE_INT_CST_LOW (TYPE_SIZE (elttype))
		      /* * TYPE_SIZE_UNIT (elttype) */ );
	    
	    bc_store_field (elt, bitsize, bitpos, mode,
			    TREE_VALUE (elt), TREE_TYPE (TREE_VALUE (elt)),
			    /* The alignment of TARGET is
			       at least what its type requires.  */
			    VOIDmode, 0,
			    TYPE_ALIGN (TREE_TYPE (constr)) / BITS_PER_UNIT,
			    int_size_in_bytes (TREE_TYPE (constr)));
	  }
  
      }
}


/* Store the value of EXP (an expression tree) into member FIELD of
   structure at address on stack, which has type TYPE, mode MODE and
   occupies BITSIZE bits, starting BITPOS bits from the beginning of the
   structure.

   ALIGN is the alignment that TARGET is known to have, measured in bytes.
   TOTAL_SIZE is its size in bytes, or -1 if variable.  */

void
bc_store_field (field, bitsize, bitpos, mode, exp, type,
		value_mode, unsignedp, align, total_size)
     int bitsize, bitpos;
     enum machine_mode mode;
     tree field, exp, type;
     enum machine_mode value_mode;
     int unsignedp;
     int align;
     int total_size;
{

  /* Expand expression and copy pointer */
  bc_expand_expr (exp);
  bc_emit_instruction (over);


  /* If the component is a bit field, we cannot use addressing to access
     it.  Use bit-field techniques to store in it.  */

  if (DECL_BIT_FIELD (field))
    {
      bc_store_bit_field (bitpos, bitsize, unsignedp);
      return;
    }
  else
    /* Not bit field */
    {
      HOST_WIDE_INT offset = bitpos / BITS_PER_UNIT;

      /* Advance pointer to the desired member */
      if (offset)
	bc_emit_instruction (addconstPSI, offset);

      /* Store */
      bc_store_memory (type, field);
    }
}


/* Store SI/SU in bitfield */

void
bc_store_bit_field (offset, size, unsignedp)
     int offset, size, unsignedp;
{
  /* Push bitfield offset and size */
  bc_push_offset_and_size (offset, size);

  /* Store */
  bc_emit_instruction (sstoreBI);
}


/* Load SI/SU from bitfield */

void
bc_load_bit_field (offset, size, unsignedp)
     int offset, size, unsignedp;
{
  /* Push bitfield offset and size */
  bc_push_offset_and_size (offset, size);

  /* Load: sign-extend if signed, else zero-extend */
  bc_emit_instruction (unsignedp ? zxloadBI : sxloadBI);
}


/* Adjust interpreter stack by NLEVELS.  Positive means drop NLEVELS
   (adjust stack pointer upwards), negative means add that number of
   levels (adjust the stack pointer downwards).  Only positive values
   normally make sense.  */

void
bc_adjust_stack (nlevels)
     int nlevels;
{
  switch (nlevels)
    {
    case 0:
      break;
      
    case 2:
      bc_emit_instruction (drop);
      
    case 1:
      bc_emit_instruction (drop);
      break;
      
    default:
      
      bc_emit_instruction (adjstackSI, (HOST_WIDE_INT) nlevels);
      stack_depth -= nlevels;
    }

#if defined (VALIDATE_STACK_FOR_BC)
  VALIDATE_STACK_FOR_BC ();
#endif
}
