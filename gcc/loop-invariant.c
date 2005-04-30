/* Rtl-level loop invariant motion.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This implements the loop invariant motion pass.  It is very simple
   (no calls, libcalls, etc.).  This should be sufficient to cleanup things like
   address arithmetics -- other more complicated invariants should be
   eliminated on tree level either in tree-ssa-loop-im.c or in tree-ssa-pre.c.
   
   We proceed loop by loop -- it is simpler than trying to handle things
   globally and should not lose much.  First we inspect all sets inside loop
   and create a dependency graph on insns (saying "to move this insn, you must
   also move the following insns").

   We then need to determine what to move.  We estimate the number of registers
   used and move as many invariants as possible while we still have enough free
   registers.  We prefer the expensive invariants.
   
   Then we move the selected invariants out of the loop, creating a new
   temporaries for them if necessary.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "expr.h"
#include "output.h"
#include "function.h"
#include "flags.h"
#include "df.h"

/* The data stored for the loop.  */

struct loop_data
{
  struct loop *outermost_exit;	/* The outermost exit of the loop.  */
  bool has_call;		/* True if the loop contains a call.  */
};

#define LOOP_DATA(LOOP) ((struct loop_data *) (LOOP)->aux)

/* The description of an use.  */

struct use
{
  rtx *pos;			/* Position of the use.  */
  rtx insn;			/* The insn in that the use occurs.  */

  struct use *next;		/* Next use in the list.  */
};

/* The description of a def.  */

struct def
{
  struct use *uses;		/* The list of uses that are uniquely reached
				   by it.  */
  unsigned n_uses;		/* Number of such uses.  */
  unsigned invno;		/* The corresponding invariant.  */
};

/* The data stored for each invariant.  */

struct invariant
{
  /* The number of the invariant.  */
  unsigned invno;

  /* Whether we already processed the invariant.  */
  bool processed;

  /* The definition of the invariant.  */
  struct def *def;

  /* The insn in that it is defined.  */
  rtx insn;

  /* Whether it is always executed.  */
  bool always_executed;

  /* Whether to move the invariant.  */
  bool move;

  /* Cost if the invariant.  */
  unsigned cost;

  /* The invariants it depends on.  */
  bitmap depends_on;

  /* Used for detecting already visited invariants during determining
     costs of movements.  */
  unsigned stamp;
};

/* The actual stamp for marking already visited invariants during determining
   costs of movements.  */

static unsigned actual_stamp;

typedef struct invariant *invariant_p;

DEF_VEC_P(invariant_p);
DEF_VEC_ALLOC_P(invariant_p, heap);

/* The invariants.  */

static VEC(invariant_p,heap) *invariants;

/* Test for possibility of invariantness of X.  */

static bool
check_maybe_invariant (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      return true;

    case PC:
    case CC0:
    case UNSPEC_VOLATILE:
    case CALL:
      return false;

    case REG:
      return true;

    case MEM:
      /* Load/store motion is done elsewhere.  ??? Perhaps also add it here?
	 It should not be hard, and might be faster than "elsewhere".  */

      /* Just handle the most trivial case where we load from an unchanging
	 location (most importantly, pic tables).  */
      if (MEM_READONLY_P (x))
	break;

      return false;

    case ASM_OPERANDS:
      /* Don't mess with insns declared volatile.  */
      if (MEM_VOLATILE_P (x))
	return false;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (!check_maybe_invariant (XEXP (x, i)))
	    return false;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (!check_maybe_invariant (XVECEXP (x, i, j)))
	      return false;
	}
    }

  return true;
}

/* Determines the basic blocks inside LOOP that are always executed and
   stores their bitmap to ALWAYS_REACHED.  MAY_EXIT is a bitmap of
   basic blocks that may either exit the loop, or contain the call that
   does not have to return.  BODY is body of the loop obtained by
   get_loop_body_in_dom_order.  */

static void
compute_always_reached (struct loop *loop, basic_block *body,
			bitmap may_exit, bitmap always_reached)
{
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    {
      if (dominated_by_p (CDI_DOMINATORS, loop->latch, body[i]))
	bitmap_set_bit (always_reached, i);

      if (bitmap_bit_p (may_exit, i))
	return;
    }
}

/* Finds exits out of the LOOP with body BODY.  Marks blocks in that we may
   exit the loop by cfg edge to HAS_EXIT and MAY_EXIT.  In MAY_EXIT
   additionally mark blocks that may exit due to a call.  */

static void
find_exits (struct loop *loop, basic_block *body,
	    bitmap may_exit, bitmap has_exit)
{
  unsigned i;
  edge_iterator ei;
  edge e;
  struct loop *outermost_exit = loop, *aexit;
  bool has_call = false;
  rtx insn;

  for (i = 0; i < loop->num_nodes; i++)
    {
      if (body[i]->loop_father == loop)
	{
	  FOR_BB_INSNS (body[i], insn)
	    {
	      if (CALL_P (insn)
		  && !CONST_OR_PURE_CALL_P (insn))
		{
		  has_call = true;
		  bitmap_set_bit (may_exit, i);
		  break;
		}
	    }

	  FOR_EACH_EDGE (e, ei, body[i]->succs)
	    {
	      if (flow_bb_inside_loop_p (loop, e->dest))
		continue;

	      bitmap_set_bit (may_exit, i);
	      bitmap_set_bit (has_exit, i);
	      outermost_exit = find_common_loop (outermost_exit,
						 e->dest->loop_father);
	    }
	  continue;
	}
     
      /* Use the data stored for the subloop to decide whether we may exit
	 through it.  It is sufficient to do this for header of the loop,
	 as other basic blocks inside it must be dominated by it.  */
      if (body[i]->loop_father->header != body[i])
	continue;

      if (LOOP_DATA (body[i]->loop_father)->has_call)
	{
	  has_call = true;
	  bitmap_set_bit (may_exit, i);
	}
      aexit = LOOP_DATA (body[i]->loop_father)->outermost_exit;
      if (aexit != loop)
	{
	  bitmap_set_bit (may_exit, i);
	  bitmap_set_bit (has_exit, i);

	  if (flow_loop_nested_p (aexit, outermost_exit))
	    outermost_exit = aexit;
	}
    }

  loop->aux = xcalloc (1, sizeof (struct loop_data));
  LOOP_DATA (loop)->outermost_exit = outermost_exit;
  LOOP_DATA (loop)->has_call = has_call;
}

/* Check whether we may assign a value to X from a register.  */

static bool
may_assign_reg_p (rtx x)
{
  return can_copy_p (GET_MODE (x));
}

/* Finds definitions that may correspond to invariants in LOOP with body BODY.
   DF is the dataflow object.  */

static void
find_defs (struct loop *loop, basic_block *body, struct df *df)
{
  unsigned i;
  bitmap blocks = BITMAP_ALLOC (NULL);

  for (i = 0; i < loop->num_nodes; i++)
    bitmap_set_bit (blocks, body[i]->index);

  df_analyze_subcfg (df, blocks, DF_UD_CHAIN | DF_HARD_REGS | DF_EQUIV_NOTES);
  BITMAP_FREE (blocks);
}

/* Creates a new invariant for definition DEF in INSN, depending on invariants
   in DEPENDS_ON.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  */

static void
create_new_invariant (struct def *def, rtx insn, bitmap depends_on,
		      bool always_executed)
{
  struct invariant *inv = xmalloc (sizeof (struct invariant));
  rtx set = single_set (insn);

  inv->def = def;
  inv->always_executed = always_executed;
  inv->depends_on = depends_on;

  /* If the set is simple, usually by moving it we move the whole store out of
     the loop.  Otherwise we save only cost of the computation.  */
  if (def)
    inv->cost = rtx_cost (set, SET);
  else
    inv->cost = rtx_cost (SET_SRC (set), SET);

  inv->move = false;
  inv->processed = false;
  inv->stamp = 0;
  inv->insn = insn;

  inv->invno = VEC_length (invariant_p, invariants);
  if (def)
    def->invno = inv->invno;
  VEC_safe_push (invariant_p, heap, invariants, inv);

  if (dump_file)
    {
      fprintf (dump_file,
	       "Set in insn %d is invariant (%d), cost %d, depends on ",
	       INSN_UID (insn), inv->invno, inv->cost);
      dump_bitmap (dump_file, inv->depends_on);
    }
}

/* Record USE at DEF.  */

static void
record_use (struct def *def, rtx *use, rtx insn)
{
  struct use *u = xmalloc (sizeof (struct use));

  if (GET_CODE (*use) == SUBREG)
    use = &SUBREG_REG (*use);
  gcc_assert (REG_P (*use));

  u->pos = use;
  u->insn = insn;
  u->next = def->uses;
  def->uses = u;
  def->n_uses++;
}

/* Finds the invariants INSN depends on and store them to the DEPENDS_ON
   bitmap.  DF is the dataflow object.  */

static bool
check_dependencies (rtx insn, struct df *df, bitmap depends_on)
{
  struct df_link *uses, *defs;
  struct ref *use, *def;
  basic_block bb = BLOCK_FOR_INSN (insn), def_bb;
  struct def *def_data;
  
  for (uses = DF_INSN_USES (df, insn); uses; uses = uses->next)
    {
      use = uses->ref;

      defs = DF_REF_CHAIN (use);
      if (!defs)
	continue;

      if (defs->next)
	return false;

      def = defs->ref;
      def_data = DF_REF_DATA (def);
      if (!def_data)
	return false;

      def_bb = DF_REF_BB (def);
      if (!dominated_by_p (CDI_DOMINATORS, bb, def_bb))
	return false;

      bitmap_set_bit (depends_on, def_data->invno);
    }

  return true;
}

/* Finds invariant in INSN.  ALWAYS_REACHED is true if the insn is always
   executed.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  DF is the dataflow
   object.  */

static void
find_invariant_insn (rtx insn, bool always_reached, bool always_executed,
		     struct df *df)
{
  struct ref *ref;
  struct def *def;
  bitmap depends_on;
  rtx set, dest;
  bool simple = true;

  /* Until we get rid of LIBCALLS.  */
  if (find_reg_note (insn, REG_RETVAL, NULL_RTX)
      || find_reg_note (insn, REG_LIBCALL, NULL_RTX)
      || find_reg_note (insn, REG_NO_CONFLICT, NULL_RTX))
    return;
      
  set = single_set (insn);
  if (!set)
    return;
  dest = SET_DEST (set);

  if (!REG_P (dest)
      || HARD_REGISTER_P (dest))
    simple = false;

  if (!check_maybe_invariant (SET_SRC (set))
      || !may_assign_reg_p (SET_DEST (set)))
    return;

  if (may_trap_p (PATTERN (insn)))
    {
      if (!always_reached)
	return;

      /* Unless the exceptions are handled, the behavior is undefined
 	 if the trap occurs.  */
      if (flag_non_call_exceptions)
	return;
    }

  depends_on = BITMAP_ALLOC (NULL);
  if (!check_dependencies (insn, df, depends_on))
    {
      BITMAP_FREE (depends_on);
      return;
    }

  if (simple)
    {
      ref = df_find_def (df, insn, dest);
      def = xcalloc (1, sizeof (struct def));
      DF_REF_DATA (ref) = def;
    }
  else
    def = NULL;

  create_new_invariant (def, insn, depends_on, always_executed);
}

/* Record registers used in INSN that have an unique invariant definition.
   DF is the dataflow object.  */

static void
record_uses (rtx insn, struct df *df)
{
  struct df_link *uses, *defs;
  struct ref *use, *def;
  basic_block bb = BLOCK_FOR_INSN (insn), def_bb;
  
  for (uses = DF_INSN_USES (df, insn); uses; uses = uses->next)
    {
      use = uses->ref;

      defs = DF_REF_CHAIN (use);
      if (!defs || defs->next)
	continue;
      def = defs->ref;
      if (!DF_REF_DATA (def))
	continue;

      def_bb = DF_REF_BB (def);
      if (!dominated_by_p (CDI_DOMINATORS, bb, def_bb))
	continue;

      record_use (DF_REF_DATA (def), DF_REF_LOC (use), DF_REF_INSN (use));
    }
}

/* Finds invariants in INSN.  ALWAYS_REACHED is true if the insn is always
   executed.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  DF is the dataflow
   object.  */

static void
find_invariants_insn (rtx insn, bool always_reached, bool always_executed,
		      struct df *df)
{
  find_invariant_insn (insn, always_reached, always_executed, df);
  record_uses (insn, df);
}

/* Finds invariants in basic block BB.  ALWAYS_REACHED is true if the
   basic block is always executed.  ALWAYS_EXECUTED is true if the basic
   block is always executed, unless the program ends due to a function
   call.  DF is the dataflow object.  */

static void
find_invariants_bb (basic_block bb, bool always_reached, bool always_executed,
		    struct df *df)
{
  rtx insn;

  FOR_BB_INSNS (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      find_invariants_insn (insn, always_reached, always_executed, df);

      if (always_reached
	  && CALL_P (insn)
	  && !CONST_OR_PURE_CALL_P (insn))
	always_reached = false;
    }
}

/* Finds invariants in LOOP with body BODY.  ALWAYS_REACHED is the bitmap of
   basic blocks in BODY that are always executed.  ALWAYS_EXECUTED is the
   bitmap of basic blocks in BODY that are always executed unless the program
   ends due to a function call.  DF is the dataflow object.  */

static void
find_invariants_body (struct loop *loop, basic_block *body,
		      bitmap always_reached, bitmap always_executed,
		      struct df *df)
{
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    find_invariants_bb (body[i],
			bitmap_bit_p (always_reached, i),
			bitmap_bit_p (always_executed, i),
			df);
}

/* Finds invariants in LOOP.  DF is the dataflow object.  */

static void
find_invariants (struct loop *loop, struct df *df)
{
  bitmap may_exit = BITMAP_ALLOC (NULL);
  bitmap always_reached = BITMAP_ALLOC (NULL);
  bitmap has_exit = BITMAP_ALLOC (NULL);
  bitmap always_executed = BITMAP_ALLOC (NULL);
  basic_block *body = get_loop_body_in_dom_order (loop);

  find_exits (loop, body, may_exit, has_exit);
  compute_always_reached (loop, body, may_exit, always_reached);
  compute_always_reached (loop, body, has_exit, always_executed);

  find_defs (loop, body, df);
  find_invariants_body (loop, body, always_reached, always_executed, df);

  BITMAP_FREE (always_reached);
  BITMAP_FREE (always_executed);
  BITMAP_FREE (may_exit);
  BITMAP_FREE (has_exit);
  free (body);
}

/* Frees a list of uses USE.  */

static void
free_use_list (struct use *use)
{
  struct use *next;

  for (; use; use = next)
    {
      next = use->next;
      free (use);
    }
}

/* Calculates cost and number of registers needed for moving invariant INV
   out of the loop and stores them to *COST and *REGS_NEEDED.  */

static void
get_inv_cost (struct invariant *inv, int *comp_cost, unsigned *regs_needed)
{
  int acomp_cost;
  unsigned aregs_needed;
  unsigned depno;
  struct invariant *dep;
  bitmap_iterator bi;

  *comp_cost = 0;
  *regs_needed = 0;
  if (inv->move
      || inv->stamp == actual_stamp)
    return;
  inv->stamp = actual_stamp;

  (*regs_needed)++;
  (*comp_cost) += inv->cost;

  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, depno, bi)
    {
      dep = VEC_index (invariant_p, invariants, depno);

      get_inv_cost (dep, &acomp_cost, &aregs_needed);

      if (aregs_needed
	  /* We need to check always_executed, since if the original value of
	     the invariant may be preserved, we may need to keep it in a
	     separate register.  TODO check whether the register has an
	     use outside of the loop.  */
	  && dep->always_executed
	  && !dep->def->uses->next)
	{
	  /* If this is a single use, after moving the dependency we will not
	     need a new register.  */
	  aregs_needed--;
	}

      (*regs_needed) += aregs_needed;
      (*comp_cost) += acomp_cost;
    }
}

/* Calculates gain for eliminating invariant INV.  REGS_USED is the number
   of registers used in the loop, N_INV_USES is the number of uses of
   invariants, NEW_REGS is the number of new variables already added due to
   the invariant motion.  The number of registers needed for it is stored in
   *REGS_NEEDED.  */

static int
gain_for_invariant (struct invariant *inv, unsigned *regs_needed,
		    unsigned new_regs, unsigned regs_used, unsigned n_inv_uses)
{
  int comp_cost, size_cost;

  get_inv_cost (inv, &comp_cost, regs_needed);
  actual_stamp++;

  size_cost = (global_cost_for_size (new_regs + *regs_needed,
				     regs_used, n_inv_uses)
	       - global_cost_for_size (new_regs, regs_used, n_inv_uses));

  return comp_cost - size_cost;
}

/* Finds invariant with best gain for moving.  Returns the gain, stores
   the invariant in *BEST and number of registers needed for it to
   *REGS_NEEDED.  REGS_USED is the number of registers used in
   the loop, N_INV_USES is the number of uses of invariants.  NEW_REGS
   is the number of new variables already added due to invariant motion.  */

static int
best_gain_for_invariant (struct invariant **best, unsigned *regs_needed,
			 unsigned new_regs, unsigned regs_used,
			 unsigned n_inv_uses)
{
  struct invariant *inv;
  int gain = 0, again;
  unsigned aregs_needed, invno;

  for (invno = 0; VEC_iterate (invariant_p, invariants, invno, inv); invno++)
    {
      if (inv->move)
	continue;

      again = gain_for_invariant (inv, &aregs_needed,
				  new_regs, regs_used, n_inv_uses);
      if (again > gain)
	{
	  gain = again;
	  *best = inv;
	  *regs_needed = aregs_needed;
	}
    }

  return gain;
}

/* Marks invariant INVNO and all its dependencies for moving.  */

static void
set_move_mark (unsigned invno)
{
  struct invariant *inv = VEC_index (invariant_p, invariants, invno);
  bitmap_iterator bi;

  if (inv->move)
    return;
  inv->move = true;

  if (dump_file)
    fprintf (dump_file, "Decided to move invariant %d\n", invno);

  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, invno, bi)
    {
      set_move_mark (invno);
    }
}

/* Determines which invariants to move.  DF is the dataflow object.  */

static void
find_invariants_to_move (struct df *df)
{
  unsigned i, regs_used, n_inv_uses, regs_needed = 0, new_regs;
  struct invariant *inv = NULL;

  if (!VEC_length (invariant_p, invariants))
    return;

  /* Now something slightly more involved.  First estimate the number of used
     registers.  */
  n_inv_uses = 0;

  /* We do not really do a good job in this estimation; put some initial bound
     here to stand for induction variables etc. that we do not detect.  */
  regs_used = 2;

  for (i = 0; i < df->n_regs; i++)
    {
      if (!DF_REGNO_FIRST_DEF (df, i) && DF_REGNO_LAST_USE (df, i))
	{
	  /* This is a value that is used but not changed inside loop.  */
	  regs_used++;
	}
    }

  for (i = 0; VEC_iterate (invariant_p, invariants, i, inv); i++)
    {
      if (inv->def)
	n_inv_uses += inv->def->n_uses;
    }

  new_regs = 0;
  while (best_gain_for_invariant (&inv, &regs_needed,
				  new_regs, regs_used, n_inv_uses) > 0)
    {
      set_move_mark (inv->invno);
      new_regs += regs_needed;
    }
}

/* Move invariant INVNO out of the LOOP.  DF is the dataflow object.  */

static void
move_invariant_reg (struct loop *loop, unsigned invno, struct df *df)
{
  struct invariant *inv = VEC_index (invariant_p, invariants, invno);
  unsigned i;
  basic_block preheader = loop_preheader_edge (loop)->src;
  rtx reg, set;
  struct use *use;
  bitmap_iterator bi;

  if (inv->processed)
    return;
  inv->processed = true;

  if (inv->depends_on)
    {
      EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, i, bi)
	{
	  move_invariant_reg (loop, i, df);
	}
    }

  /* Move the set out of the loop.  If the set is always executed (we could
     omit this condition if we know that the register is unused outside of the
     loop, but it does not seem worth finding out) and it has no uses that
     would not be dominated by it, we may just move it (TODO).  Otherwise we
     need to create a temporary register.  */
  set = single_set (inv->insn);
  reg = gen_reg_rtx (GET_MODE (SET_DEST (set)));
  df_pattern_emit_after (df, gen_move_insn (SET_DEST (set), reg),
			 BLOCK_FOR_INSN (inv->insn), inv->insn);
  SET_DEST (set) = reg;
  reorder_insns (inv->insn, inv->insn, BB_END (preheader));
  df_insn_modify (df, preheader, inv->insn);

  /* Replace the uses we know to be dominated.  It saves work for copy
     propagation, and also it is necessary so that dependent invariants
     are computed right.  */
  if (inv->def)
    {
      for (use = inv->def->uses; use; use = use->next)
	{
	  *use->pos = reg;
	  df_insn_modify (df, BLOCK_FOR_INSN (use->insn), use->insn);
	}
    }
}

/* Move selected invariant out of the LOOP.  Newly created regs are marked
   in TEMPORARY_REGS.  DF is the dataflow object.  */

static void
move_invariants (struct loop *loop, struct df *df)
{
  struct invariant *inv;
  unsigned i;

  for (i = 0; VEC_iterate (invariant_p, invariants, i, inv); i++)
    {
      if (inv->move)
	move_invariant_reg (loop, i, df);
    }
}

/* Initializes invariant motion data.  */

static void
init_inv_motion_data (void)
{
  actual_stamp = 1;

  invariants = VEC_alloc (invariant_p, heap, 100);
}

/* Frees the data allocated by invariant motion.  DF is the dataflow
   object.  */

static void
free_inv_motion_data (struct df *df)
{
  unsigned i;
  struct def *def;
  struct invariant *inv;

  for (i = 0; i < df->n_defs; i++)
    {
      if (!df->defs[i])
	continue;

      def = DF_REF_DATA (df->defs[i]);
      if (!def)
	continue;

      free_use_list (def->uses);
      free (def);
      DF_REF_DATA (df->defs[i]) = NULL;
    }

  for (i = 0; VEC_iterate (invariant_p, invariants, i, inv); i++)
    {
      BITMAP_FREE (inv->depends_on);
      free (inv);
    }
  VEC_free (invariant_p, heap, invariants);
}

/* Move the invariants out of the LOOP.  DF is the dataflow object.  */

static void
move_single_loop_invariants (struct loop *loop, struct df *df)
{
  init_inv_motion_data ();

  find_invariants (loop, df);
  find_invariants_to_move (df);
  move_invariants (loop, df);

  free_inv_motion_data (df);
}

/* Releases the auxiliary data for LOOP.  */

static void
free_loop_data (struct loop *loop)
{
  struct loop_data *data = LOOP_DATA (loop);

  free (data);
  loop->aux = NULL;
}

/* Move the invariants out of the LOOPS.  */

void
move_loop_invariants (struct loops *loops)
{
  struct loop *loop;
  unsigned i;
  struct df *df = df_init ();

  /* Process the loops, innermost first.  */
  loop = loops->tree_root;
  while (loop->inner)
    loop = loop->inner;

  while (loop != loops->tree_root)
    {
      move_single_loop_invariants (loop, df);

      if (loop->next)
	{
	  loop = loop->next;
	  while (loop->inner)
	    loop = loop->inner;
	}
      else
	loop = loop->outer;
    }

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      free_loop_data (loops->parray[i]);

  df_finish (df);
}
