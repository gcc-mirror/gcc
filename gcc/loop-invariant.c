/* RTL-level loop invariant motion.
   Copyright (C) 2004-2018 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This implements the loop invariant motion pass.  It is very simple
   (no calls, no loads/stores, etc.).  This should be sufficient to cleanup
   things like address arithmetics -- other more complicated invariants should
   be eliminated on GIMPLE either in tree-ssa-loop-im.c or in tree-ssa-pre.c.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "cfgrtl.h"
#include "cfgloop.h"
#include "expr.h"
#include "params.h"
#include "rtl-iter.h"
#include "dumpfile.h"

/* The data stored for the loop.  */

struct loop_data
{
  struct loop *outermost_exit;	/* The outermost exit of the loop.  */
  bool has_call;		/* True if the loop contains a call.  */
  /* Maximal register pressure inside loop for given register class
     (defined only for the pressure classes).  */
  int max_reg_pressure[N_REG_CLASSES];
  /* Loop regs referenced and live pseudo-registers.  */
  bitmap_head regs_ref;
  bitmap_head regs_live;
};

#define LOOP_DATA(LOOP) ((struct loop_data *) (LOOP)->aux)

/* The description of an use.  */

struct use
{
  rtx *pos;			/* Position of the use.  */
  rtx_insn *insn;		/* The insn in that the use occurs.  */
  unsigned addr_use_p;		/* Whether the use occurs in an address.  */
  struct use *next;		/* Next use in the list.  */
};

/* The description of a def.  */

struct def
{
  struct use *uses;		/* The list of uses that are uniquely reached
				   by it.  */
  unsigned n_uses;		/* Number of such uses.  */
  unsigned n_addr_uses;		/* Number of uses in addresses.  */
  unsigned invno;		/* The corresponding invariant.  */
  bool can_prop_to_addr_uses;	/* True if the corresponding inv can be
				   propagated into its address uses.  */
};

/* The data stored for each invariant.  */

struct invariant
{
  /* The number of the invariant.  */
  unsigned invno;

  /* The number of the invariant with the same value.  */
  unsigned eqto;

  /* The number of invariants which eqto this.  */
  unsigned eqno;

  /* If we moved the invariant out of the loop, the original regno
     that contained its value.  */
  int orig_regno;

  /* If we moved the invariant out of the loop, the register that contains its
     value.  */
  rtx reg;

  /* The definition of the invariant.  */
  struct def *def;

  /* The insn in that it is defined.  */
  rtx_insn *insn;

  /* Whether it is always executed.  */
  bool always_executed;

  /* Whether to move the invariant.  */
  bool move;

  /* Whether the invariant is cheap when used as an address.  */
  bool cheap_address;

  /* Cost of the invariant.  */
  unsigned cost;

  /* Used for detecting already visited invariants during determining
     costs of movements.  */
  unsigned stamp;

  /* The invariants it depends on.  */
  bitmap depends_on;
};

/* Currently processed loop.  */
static struct loop *curr_loop;

/* Table of invariants indexed by the df_ref uid field.  */

static unsigned int invariant_table_size = 0;
static struct invariant ** invariant_table;

/* Entry for hash table of invariant expressions.  */

struct invariant_expr_entry
{
  /* The invariant.  */
  struct invariant *inv;

  /* Its value.  */
  rtx expr;

  /* Its mode.  */
  machine_mode mode;

  /* Its hash.  */
  hashval_t hash;
};

/* The actual stamp for marking already visited invariants during determining
   costs of movements.  */

static unsigned actual_stamp;

typedef struct invariant *invariant_p;


/* The invariants.  */

static vec<invariant_p> invariants;

/* Check the size of the invariant table and realloc if necessary.  */

static void
check_invariant_table_size (void)
{
  if (invariant_table_size < DF_DEFS_TABLE_SIZE ())
    {
      unsigned int new_size = DF_DEFS_TABLE_SIZE () + (DF_DEFS_TABLE_SIZE () / 4);
      invariant_table = XRESIZEVEC (struct invariant *, invariant_table, new_size);
      memset (&invariant_table[invariant_table_size], 0,
	      (new_size - invariant_table_size) * sizeof (struct invariant *));
      invariant_table_size = new_size;
    }
}

/* Test for possibility of invariantness of X.  */

static bool
check_maybe_invariant (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  switch (code)
    {
    CASE_CONST_ANY:
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
      if (MEM_READONLY_P (x) && !MEM_VOLATILE_P (x))
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

/* Returns the invariant definition for USE, or NULL if USE is not
   invariant.  */

static struct invariant *
invariant_for_use (df_ref use)
{
  struct df_link *defs;
  df_ref def;
  basic_block bb = DF_REF_BB (use), def_bb;

  if (DF_REF_FLAGS (use) & DF_REF_READ_WRITE)
    return NULL;

  defs = DF_REF_CHAIN (use);
  if (!defs || defs->next)
    return NULL;
  def = defs->ref;
  check_invariant_table_size ();
  if (!invariant_table[DF_REF_ID (def)])
    return NULL;

  def_bb = DF_REF_BB (def);
  if (!dominated_by_p (CDI_DOMINATORS, bb, def_bb))
    return NULL;
  return invariant_table[DF_REF_ID (def)];
}

/* Computes hash value for invariant expression X in INSN.  */

static hashval_t
hash_invariant_expr_1 (rtx_insn *insn, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;
  hashval_t val = code;
  int do_not_record_p;
  df_ref use;
  struct invariant *inv;

  switch (code)
    {
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      return hash_rtx (x, GET_MODE (x), &do_not_record_p, NULL, false);

    case REG:
      use = df_find_use (insn, x);
      if (!use)
	return hash_rtx (x, GET_MODE (x), &do_not_record_p, NULL, false);
      inv = invariant_for_use (use);
      if (!inv)
	return hash_rtx (x, GET_MODE (x), &do_not_record_p, NULL, false);

      gcc_assert (inv->eqto != ~0u);
      return inv->eqto;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	val ^= hash_invariant_expr_1 (insn, XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    val ^= hash_invariant_expr_1 (insn, XVECEXP (x, i, j));
	}
      else if (fmt[i] == 'i' || fmt[i] == 'n')
	val ^= XINT (x, i);
      else if (fmt[i] == 'p')
	val ^= constant_lower_bound (SUBREG_BYTE (x));
    }

  return val;
}

/* Returns true if the invariant expressions E1 and E2 used in insns INSN1
   and INSN2 have always the same value.  */

static bool
invariant_expr_equal_p (rtx_insn *insn1, rtx e1, rtx_insn *insn2, rtx e2)
{
  enum rtx_code code = GET_CODE (e1);
  int i, j;
  const char *fmt;
  df_ref use1, use2;
  struct invariant *inv1 = NULL, *inv2 = NULL;
  rtx sub1, sub2;

  /* If mode of only one of the operands is VOIDmode, it is not equivalent to
     the other one.  If both are VOIDmode, we rely on the caller of this
     function to verify that their modes are the same.  */
  if (code != GET_CODE (e2) || GET_MODE (e1) != GET_MODE (e2))
    return false;

  switch (code)
    {
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      return rtx_equal_p (e1, e2);

    case REG:
      use1 = df_find_use (insn1, e1);
      use2 = df_find_use (insn2, e2);
      if (use1)
	inv1 = invariant_for_use (use1);
      if (use2)
	inv2 = invariant_for_use (use2);

      if (!inv1 && !inv2)
	return rtx_equal_p (e1, e2);

      if (!inv1 || !inv2)
	return false;

      gcc_assert (inv1->eqto != ~0u);
      gcc_assert (inv2->eqto != ~0u);
      return inv1->eqto == inv2->eqto;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  sub1 = XEXP (e1, i);
	  sub2 = XEXP (e2, i);

	  if (!invariant_expr_equal_p (insn1, sub1, insn2, sub2))
	    return false;
	}

      else if (fmt[i] == 'E')
	{
	  if (XVECLEN (e1, i) != XVECLEN (e2, i))
	    return false;

	  for (j = 0; j < XVECLEN (e1, i); j++)
	    {
	      sub1 = XVECEXP (e1, i, j);
	      sub2 = XVECEXP (e2, i, j);

	      if (!invariant_expr_equal_p (insn1, sub1, insn2, sub2))
		return false;
	    }
	}
      else if (fmt[i] == 'i' || fmt[i] == 'n')
	{
	  if (XINT (e1, i) != XINT (e2, i))
	    return false;
	}
      else if (fmt[i] == 'p')
	{
	  if (maybe_ne (SUBREG_BYTE (e1), SUBREG_BYTE (e2)))
	    return false;
	}
      /* Unhandled type of subexpression, we fail conservatively.  */
      else
	return false;
    }

  return true;
}

struct invariant_expr_hasher : free_ptr_hash <invariant_expr_entry>
{
  static inline hashval_t hash (const invariant_expr_entry *);
  static inline bool equal (const invariant_expr_entry *,
			    const invariant_expr_entry *);
};

/* Returns hash value for invariant expression entry ENTRY.  */

inline hashval_t
invariant_expr_hasher::hash (const invariant_expr_entry *entry)
{
  return entry->hash;
}

/* Compares invariant expression entries ENTRY1 and ENTRY2.  */

inline bool
invariant_expr_hasher::equal (const invariant_expr_entry *entry1,
			      const invariant_expr_entry *entry2)
{
  if (entry1->mode != entry2->mode)
    return 0;

  return invariant_expr_equal_p (entry1->inv->insn, entry1->expr,
				 entry2->inv->insn, entry2->expr);
}

typedef hash_table<invariant_expr_hasher> invariant_htab_type;

/* Checks whether invariant with value EXPR in machine mode MODE is
   recorded in EQ.  If this is the case, return the invariant.  Otherwise
   insert INV to the table for this expression and return INV.  */

static struct invariant *
find_or_insert_inv (invariant_htab_type *eq, rtx expr, machine_mode mode,
		    struct invariant *inv)
{
  hashval_t hash = hash_invariant_expr_1 (inv->insn, expr);
  struct invariant_expr_entry *entry;
  struct invariant_expr_entry pentry;
  invariant_expr_entry **slot;

  pentry.expr = expr;
  pentry.inv = inv;
  pentry.mode = mode;
  slot = eq->find_slot_with_hash (&pentry, hash, INSERT);
  entry = *slot;

  if (entry)
    return entry->inv;

  entry = XNEW (struct invariant_expr_entry);
  entry->inv = inv;
  entry->expr = expr;
  entry->mode = mode;
  entry->hash = hash;
  *slot = entry;

  return inv;
}

/* Finds invariants identical to INV and records the equivalence.  EQ is the
   hash table of the invariants.  */

static void
find_identical_invariants (invariant_htab_type *eq, struct invariant *inv)
{
  unsigned depno;
  bitmap_iterator bi;
  struct invariant *dep;
  rtx expr, set;
  machine_mode mode;
  struct invariant *tmp;

  if (inv->eqto != ~0u)
    return;

  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, depno, bi)
    {
      dep = invariants[depno];
      find_identical_invariants (eq, dep);
    }

  set = single_set (inv->insn);
  expr = SET_SRC (set);
  mode = GET_MODE (expr);
  if (mode == VOIDmode)
    mode = GET_MODE (SET_DEST (set));

  tmp = find_or_insert_inv (eq, expr, mode, inv);
  inv->eqto = tmp->invno;

  if (tmp->invno != inv->invno && inv->always_executed)
    tmp->eqno++;

  if (dump_file && inv->eqto != inv->invno)
    fprintf (dump_file,
	     "Invariant %d is equivalent to invariant %d.\n",
	     inv->invno, inv->eqto);
}

/* Find invariants with the same value and record the equivalences.  */

static void
merge_identical_invariants (void)
{
  unsigned i;
  struct invariant *inv;
  invariant_htab_type eq (invariants.length ());

  FOR_EACH_VEC_ELT (invariants, i, inv)
    find_identical_invariants (&eq, inv);
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
  rtx_insn *insn;

  for (i = 0; i < loop->num_nodes; i++)
    {
      if (body[i]->loop_father == loop)
	{
	  FOR_BB_INSNS (body[i], insn)
	    {
	      if (CALL_P (insn)
		  && (RTL_LOOPING_CONST_OR_PURE_CALL_P (insn)
		      || !RTL_CONST_OR_PURE_CALL_P (insn)))
		{
		  has_call = true;
		  bitmap_set_bit (may_exit, i);
		  break;
		}
	    }

	  FOR_EACH_EDGE (e, ei, body[i]->succs)
	    {
	      if (! flow_bb_inside_loop_p (loop, e->dest))
		{
		  bitmap_set_bit (may_exit, i);
		  bitmap_set_bit (has_exit, i);
		  outermost_exit = find_common_loop (outermost_exit,
						     e->dest->loop_father);
		}
	      /* If we enter a subloop that might never terminate treat
	         it like a possible exit.  */
	      if (flow_loop_nested_p (loop, e->dest->loop_father))
		bitmap_set_bit (may_exit, i);
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

  if (loop->aux == NULL)
    {
      loop->aux = xcalloc (1, sizeof (struct loop_data));
      bitmap_initialize (&LOOP_DATA (loop)->regs_ref, &reg_obstack);
      bitmap_initialize (&LOOP_DATA (loop)->regs_live, &reg_obstack);
    }
  LOOP_DATA (loop)->outermost_exit = outermost_exit;
  LOOP_DATA (loop)->has_call = has_call;
}

/* Check whether we may assign a value to X from a register.  */

static bool
may_assign_reg_p (rtx x)
{
  return (GET_MODE (x) != VOIDmode
	  && GET_MODE (x) != BLKmode
	  && can_copy_p (GET_MODE (x))
	  /* Do not mess with the frame pointer adjustments that can
	     be generated e.g. by expand_builtin_setjmp_receiver.  */
	  && x != frame_pointer_rtx
	  && (!REG_P (x)
	      || !HARD_REGISTER_P (x)
	      || REGNO_REG_CLASS (REGNO (x)) != NO_REGS));
}

/* Finds definitions that may correspond to invariants in LOOP with body
   BODY.  */

static void
find_defs (struct loop *loop)
{
  if (dump_file)
    {
      fprintf (dump_file,
	       "*****starting processing of loop %d ******\n",
	       loop->num);
    }

  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_chain_add_problem (DF_UD_CHAIN);
  df_live_add_problem ();
  df_live_set_all_dirty ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_analyze_loop (loop);
  check_invariant_table_size ();

  if (dump_file)
    {
      df_dump_region (dump_file);
      fprintf (dump_file,
	       "*****ending processing of loop %d ******\n",
	       loop->num);
    }
}

/* Creates a new invariant for definition DEF in INSN, depending on invariants
   in DEPENDS_ON.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  The newly created invariant
   is returned.  */

static struct invariant *
create_new_invariant (struct def *def, rtx_insn *insn, bitmap depends_on,
		      bool always_executed)
{
  struct invariant *inv = XNEW (struct invariant);
  rtx set = single_set (insn);
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));

  inv->def = def;
  inv->always_executed = always_executed;
  inv->depends_on = depends_on;

  /* If the set is simple, usually by moving it we move the whole store out of
     the loop.  Otherwise we save only cost of the computation.  */
  if (def)
    {
      inv->cost = set_rtx_cost (set, speed);
      /* ??? Try to determine cheapness of address computation.  Unfortunately
         the address cost is only a relative measure, we can't really compare
	 it with any absolute number, but only with other address costs.
	 But here we don't have any other addresses, so compare with a magic
	 number anyway.  It has to be large enough to not regress PR33928
	 (by avoiding to move reg+8,reg+16,reg+24 invariants), but small
	 enough to not regress 410.bwaves either (by still moving reg+reg
	 invariants).
	 See http://gcc.gnu.org/ml/gcc-patches/2009-10/msg01210.html .  */
      if (SCALAR_INT_MODE_P (GET_MODE (SET_DEST (set))))
	inv->cheap_address = address_cost (SET_SRC (set), word_mode,
					   ADDR_SPACE_GENERIC, speed) < 3;
      else
	inv->cheap_address = false;
    }
  else
    {
      inv->cost = set_src_cost (SET_SRC (set), GET_MODE (SET_DEST (set)),
				speed);
      inv->cheap_address = false;
    }

  inv->move = false;
  inv->reg = NULL_RTX;
  inv->orig_regno = -1;
  inv->stamp = 0;
  inv->insn = insn;

  inv->invno = invariants.length ();
  inv->eqto = ~0u;

  /* Itself.  */
  inv->eqno = 1;

  if (def)
    def->invno = inv->invno;
  invariants.safe_push (inv);

  if (dump_file)
    {
      fprintf (dump_file,
	       "Set in insn %d is invariant (%d), cost %d, depends on ",
	       INSN_UID (insn), inv->invno, inv->cost);
      dump_bitmap (dump_file, inv->depends_on);
    }

  return inv;
}

/* Return a canonical version of X for the address, from the point of view,
   that all multiplications are represented as MULT instead of the multiply
   by a power of 2 being represented as ASHIFT.

   Callers should prepare a copy of X because this function may modify it
   in place.  */

static void
canonicalize_address_mult (rtx x)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, x, NONCONST)
    {
      rtx sub = *iter;
      scalar_int_mode sub_mode;
      if (is_a <scalar_int_mode> (GET_MODE (sub), &sub_mode)
	  && GET_CODE (sub) == ASHIFT
	  && CONST_INT_P (XEXP (sub, 1))
	  && INTVAL (XEXP (sub, 1)) < GET_MODE_BITSIZE (sub_mode)
	  && INTVAL (XEXP (sub, 1)) >= 0)
	{
	  HOST_WIDE_INT shift = INTVAL (XEXP (sub, 1));
	  PUT_CODE (sub, MULT);
	  XEXP (sub, 1) = gen_int_mode (HOST_WIDE_INT_1 << shift, sub_mode);
	  iter.skip_subrtxes ();
	}
    }
}

/* Maximum number of sub expressions in address.  We set it to
   a small integer since it's unlikely to have a complicated
   address expression.  */

#define MAX_CANON_ADDR_PARTS (5)

/* Collect sub expressions in address X with PLUS as the seperator.
   Sub expressions are stored in vector ADDR_PARTS.  */

static void
collect_address_parts (rtx x, vec<rtx> *addr_parts)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, x, NONCONST)
    {
      rtx sub = *iter;

      if (GET_CODE (sub) != PLUS)
	{
	  addr_parts->safe_push (sub);
	  iter.skip_subrtxes ();
	}
    }
}

/* Compare function for sorting sub expressions X and Y based on
   precedence defined for communitive operations.  */

static int
compare_address_parts (const void *x, const void *y)
{
  const rtx *rx = (const rtx *)x;
  const rtx *ry = (const rtx *)y;
  int px = commutative_operand_precedence (*rx);
  int py = commutative_operand_precedence (*ry);

  return (py - px);
}

/* Return a canonical version address for X by following steps:
     1) Rewrite ASHIFT into MULT recursively.
     2) Divide address into sub expressions with PLUS as the
	separator.
     3) Sort sub expressions according to precedence defined
	for communative operations.
     4) Simplify CONST_INT_P sub expressions.
     5) Create new canonicalized address and return.
   Callers should prepare a copy of X because this function may
   modify it in place.  */

static rtx
canonicalize_address (rtx x)
{
  rtx res;
  unsigned int i, j;
  machine_mode mode = GET_MODE (x);
  auto_vec<rtx, MAX_CANON_ADDR_PARTS> addr_parts;

  /* Rewrite ASHIFT into MULT.  */
  canonicalize_address_mult (x);
  /* Divide address into sub expressions.  */
  collect_address_parts (x, &addr_parts);
  /* Unlikely to have very complicated address.  */
  if (addr_parts.length () < 2
      || addr_parts.length () > MAX_CANON_ADDR_PARTS)
    return x;

  /* Sort sub expressions according to canonicalization precedence.  */
  addr_parts.qsort (compare_address_parts);

  /* Simplify all constant int summary if possible.  */
  for (i = 0; i < addr_parts.length (); i++)
    if (CONST_INT_P (addr_parts[i]))
      break;

  for (j = i + 1; j < addr_parts.length (); j++)
    {
      gcc_assert (CONST_INT_P (addr_parts[j]));
      addr_parts[i] = simplify_gen_binary (PLUS, mode,
					   addr_parts[i],
					   addr_parts[j]);
    }

  /* Chain PLUS operators to the left for !CONST_INT_P sub expressions.  */
  res = addr_parts[0];
  for (j = 1; j < i; j++)
    res = simplify_gen_binary (PLUS, mode, res, addr_parts[j]);

  /* Pickup the last CONST_INT_P sub expression.  */
  if (i < addr_parts.length ())
    res = simplify_gen_binary (PLUS, mode, res, addr_parts[i]);

  return res;
}

/* Given invariant DEF and its address USE, check if the corresponding
   invariant expr can be propagated into the use or not.  */

static bool
inv_can_prop_to_addr_use (struct def *def, df_ref use)
{
  struct invariant *inv;
  rtx *pos = DF_REF_REAL_LOC (use), def_set, use_set;
  rtx_insn *use_insn = DF_REF_INSN (use);
  rtx_insn *def_insn;
  bool ok;

  inv = invariants[def->invno];
  /* No need to check if address expression is expensive.  */
  if (!inv->cheap_address)
    return false;

  def_insn = inv->insn;
  def_set = single_set (def_insn);
  if (!def_set)
    return false;

  validate_unshare_change (use_insn, pos, SET_SRC (def_set), true);
  ok = verify_changes (0);
  /* Try harder with canonicalization in address expression.  */
  if (!ok && (use_set = single_set (use_insn)) != NULL_RTX)
    {
      rtx src, dest, mem = NULL_RTX;

      src = SET_SRC (use_set);
      dest = SET_DEST (use_set);
      if (MEM_P (src))
	mem = src;
      else if (MEM_P (dest))
	mem = dest;

      if (mem != NULL_RTX
	  && !memory_address_addr_space_p (GET_MODE (mem),
					   XEXP (mem, 0),
					   MEM_ADDR_SPACE (mem)))
	{
	  rtx addr = canonicalize_address (copy_rtx (XEXP (mem, 0)));
	  if (memory_address_addr_space_p (GET_MODE (mem),
					   addr, MEM_ADDR_SPACE (mem)))
	    ok = true;
	}
    }
  cancel_changes (0);
  return ok;
}

/* Record USE at DEF.  */

static void
record_use (struct def *def, df_ref use)
{
  struct use *u = XNEW (struct use);

  u->pos = DF_REF_REAL_LOC (use);
  u->insn = DF_REF_INSN (use);
  u->addr_use_p = (DF_REF_TYPE (use) == DF_REF_REG_MEM_LOAD
		   || DF_REF_TYPE (use) == DF_REF_REG_MEM_STORE);
  u->next = def->uses;
  def->uses = u;
  def->n_uses++;
  if (u->addr_use_p)
    {
      /* Initialize propagation information if this is the first addr
	 use of the inv def.  */
      if (def->n_addr_uses == 0)
	def->can_prop_to_addr_uses = true;

      def->n_addr_uses++;
      if (def->can_prop_to_addr_uses && !inv_can_prop_to_addr_use (def, use))
	def->can_prop_to_addr_uses = false;
    }
}

/* Finds the invariants USE depends on and store them to the DEPENDS_ON
   bitmap.  Returns true if all dependencies of USE are known to be
   loop invariants, false otherwise.  */

static bool
check_dependency (basic_block bb, df_ref use, bitmap depends_on)
{
  df_ref def;
  basic_block def_bb;
  struct df_link *defs;
  struct def *def_data;
  struct invariant *inv;

  if (DF_REF_FLAGS (use) & DF_REF_READ_WRITE)
    return false;

  defs = DF_REF_CHAIN (use);
  if (!defs)
    {
      unsigned int regno = DF_REF_REGNO (use);

      /* If this is the use of an uninitialized argument register that is
	 likely to be spilled, do not move it lest this might extend its
	 lifetime and cause reload to die.  This can occur for a call to
	 a function taking complex number arguments and moving the insns
	 preparing the arguments without moving the call itself wouldn't
	 gain much in practice.  */
      if ((DF_REF_FLAGS (use) & DF_HARD_REG_LIVE)
	  && FUNCTION_ARG_REGNO_P (regno)
	  && targetm.class_likely_spilled_p (REGNO_REG_CLASS (regno)))
	return false;

      return true;
    }

  if (defs->next)
    return false;

  def = defs->ref;
  check_invariant_table_size ();
  inv = invariant_table[DF_REF_ID (def)];
  if (!inv)
    return false;

  def_data = inv->def;
  gcc_assert (def_data != NULL);

  def_bb = DF_REF_BB (def);
  /* Note that in case bb == def_bb, we know that the definition
     dominates insn, because def has invariant_table[DF_REF_ID(def)]
     defined and we process the insns in the basic block bb
     sequentially.  */
  if (!dominated_by_p (CDI_DOMINATORS, bb, def_bb))
    return false;

  bitmap_set_bit (depends_on, def_data->invno);
  return true;
}


/* Finds the invariants INSN depends on and store them to the DEPENDS_ON
   bitmap.  Returns true if all dependencies of INSN are known to be
   loop invariants, false otherwise.  */

static bool
check_dependencies (rtx_insn *insn, bitmap depends_on)
{
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;
  basic_block bb = BLOCK_FOR_INSN (insn);

  FOR_EACH_INSN_INFO_USE (use, insn_info)
    if (!check_dependency (bb, use, depends_on))
      return false;
  FOR_EACH_INSN_INFO_EQ_USE (use, insn_info)
    if (!check_dependency (bb, use, depends_on))
      return false;

  return true;
}

/* Pre-check candidate DEST to skip the one which can not make a valid insn
   during move_invariant_reg.  SIMPLE is to skip HARD_REGISTER.  */
static bool
pre_check_invariant_p (bool simple, rtx dest)
{
  if (simple && REG_P (dest) && DF_REG_DEF_COUNT (REGNO (dest)) > 1)
    {
      df_ref use;
      unsigned int i = REGNO (dest);
      struct df_insn_info *insn_info;
      df_ref def_rec;

      for (use = DF_REG_USE_CHAIN (i); use; use = DF_REF_NEXT_REG (use))
	{
	  rtx_insn *ref = DF_REF_INSN (use);
	  insn_info = DF_INSN_INFO_GET (ref);

	  FOR_EACH_INSN_INFO_DEF (def_rec, insn_info)
	    if (DF_REF_REGNO (def_rec) == i)
	      {
		/* Multi definitions at this stage, most likely are due to
		   instruction constraints, which requires both read and write
		   on the same register.  Since move_invariant_reg is not
		   powerful enough to handle such cases, just ignore the INV
		   and leave the chance to others.  */
		return false;
	      }
	}
    }
  return true;
}

/* Finds invariant in INSN.  ALWAYS_REACHED is true if the insn is always
   executed.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  */

static void
find_invariant_insn (rtx_insn *insn, bool always_reached, bool always_executed)
{
  df_ref ref;
  struct def *def;
  bitmap depends_on;
  rtx set, dest;
  bool simple = true;
  struct invariant *inv;

  /* We can't move a CC0 setter without the user.  */
  if (HAVE_cc0 && sets_cc0_p (insn))
    return;

  set = single_set (insn);
  if (!set)
    return;
  dest = SET_DEST (set);

  if (!REG_P (dest)
      || HARD_REGISTER_P (dest))
    simple = false;

  if (!may_assign_reg_p (dest)
      || !pre_check_invariant_p (simple, dest)
      || !check_maybe_invariant (SET_SRC (set)))
    return;

  /* If the insn can throw exception, we cannot move it at all without changing
     cfg.  */
  if (can_throw_internal (insn))
    return;

  /* We cannot make trapping insn executed, unless it was executed before.  */
  if (may_trap_or_fault_p (PATTERN (insn)) && !always_reached)
    return;

  depends_on = BITMAP_ALLOC (NULL);
  if (!check_dependencies (insn, depends_on))
    {
      BITMAP_FREE (depends_on);
      return;
    }

  if (simple)
    def = XCNEW (struct def);
  else
    def = NULL;

  inv = create_new_invariant (def, insn, depends_on, always_executed);

  if (simple)
    {
      ref = df_find_def (insn, dest);
      check_invariant_table_size ();
      invariant_table[DF_REF_ID (ref)] = inv;
    }
}

/* Record registers used in INSN that have a unique invariant definition.  */

static void
record_uses (rtx_insn *insn)
{
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;
  struct invariant *inv;

  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      inv = invariant_for_use (use);
      if (inv)
	record_use (inv->def, use);
    }
  FOR_EACH_INSN_INFO_EQ_USE (use, insn_info)
    {
      inv = invariant_for_use (use);
      if (inv)
	record_use (inv->def, use);
    }
}

/* Finds invariants in INSN.  ALWAYS_REACHED is true if the insn is always
   executed.  ALWAYS_EXECUTED is true if the insn is always executed,
   unless the program ends due to a function call.  */

static void
find_invariants_insn (rtx_insn *insn, bool always_reached, bool always_executed)
{
  find_invariant_insn (insn, always_reached, always_executed);
  record_uses (insn);
}

/* Finds invariants in basic block BB.  ALWAYS_REACHED is true if the
   basic block is always executed.  ALWAYS_EXECUTED is true if the basic
   block is always executed, unless the program ends due to a function
   call.  */

static void
find_invariants_bb (basic_block bb, bool always_reached, bool always_executed)
{
  rtx_insn *insn;

  FOR_BB_INSNS (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      find_invariants_insn (insn, always_reached, always_executed);

      if (always_reached
	  && CALL_P (insn)
	  && (RTL_LOOPING_CONST_OR_PURE_CALL_P (insn)
	      || ! RTL_CONST_OR_PURE_CALL_P (insn)))
	always_reached = false;
    }
}

/* Finds invariants in LOOP with body BODY.  ALWAYS_REACHED is the bitmap of
   basic blocks in BODY that are always executed.  ALWAYS_EXECUTED is the
   bitmap of basic blocks in BODY that are always executed unless the program
   ends due to a function call.  */

static void
find_invariants_body (struct loop *loop, basic_block *body,
		      bitmap always_reached, bitmap always_executed)
{
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    find_invariants_bb (body[i],
			bitmap_bit_p (always_reached, i),
			bitmap_bit_p (always_executed, i));
}

/* Finds invariants in LOOP.  */

static void
find_invariants (struct loop *loop)
{
  auto_bitmap may_exit;
  auto_bitmap always_reached;
  auto_bitmap has_exit;
  auto_bitmap always_executed;
  basic_block *body = get_loop_body_in_dom_order (loop);

  find_exits (loop, body, may_exit, has_exit);
  compute_always_reached (loop, body, may_exit, always_reached);
  compute_always_reached (loop, body, has_exit, always_executed);

  find_defs (loop);
  find_invariants_body (loop, body, always_reached, always_executed);
  merge_identical_invariants ();

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

/* Return pressure class and number of hard registers (through *NREGS)
   for destination of INSN. */
static enum reg_class
get_pressure_class_and_nregs (rtx_insn *insn, int *nregs)
{
  rtx reg;
  enum reg_class pressure_class;
  rtx set = single_set (insn);

  /* Considered invariant insns have only one set.  */
  gcc_assert (set != NULL_RTX);
  reg = SET_DEST (set);
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  if (MEM_P (reg))
    {
      *nregs = 0;
      pressure_class = NO_REGS;
    }
  else
    {
      if (! REG_P (reg))
	reg = NULL_RTX;
      if (reg == NULL_RTX)
	pressure_class = GENERAL_REGS;
      else
	{
	  pressure_class = reg_allocno_class (REGNO (reg));
	  pressure_class = ira_pressure_class_translate[pressure_class];
	}
      *nregs
	= ira_reg_class_max_nregs[pressure_class][GET_MODE (SET_SRC (set))];
    }
  return pressure_class;
}

/* Calculates cost and number of registers needed for moving invariant INV
   out of the loop and stores them to *COST and *REGS_NEEDED.  *CL will be
   the REG_CLASS of INV.  Return
     -1: if INV is invalid.
      0: if INV and its depends_on have same reg_class
      1: if INV and its depends_on have different reg_classes.  */

static int
get_inv_cost (struct invariant *inv, int *comp_cost, unsigned *regs_needed,
	      enum reg_class *cl)
{
  int i, acomp_cost;
  unsigned aregs_needed[N_REG_CLASSES];
  unsigned depno;
  struct invariant *dep;
  bitmap_iterator bi;
  int ret = 1;

  /* Find the representative of the class of the equivalent invariants.  */
  inv = invariants[inv->eqto];

  *comp_cost = 0;
  if (! flag_ira_loop_pressure)
    regs_needed[0] = 0;
  else
    {
      for (i = 0; i < ira_pressure_classes_num; i++)
	regs_needed[ira_pressure_classes[i]] = 0;
    }

  if (inv->move
      || inv->stamp == actual_stamp)
    return -1;
  inv->stamp = actual_stamp;

  if (! flag_ira_loop_pressure)
    regs_needed[0]++;
  else
    {
      int nregs;
      enum reg_class pressure_class;

      pressure_class = get_pressure_class_and_nregs (inv->insn, &nregs);
      regs_needed[pressure_class] += nregs;
      *cl = pressure_class;
      ret = 0;
    }

  if (!inv->cheap_address
      || inv->def->n_uses == 0
      || inv->def->n_addr_uses < inv->def->n_uses
      /* Count cost if the inv can't be propagated into address uses.  */
      || !inv->def->can_prop_to_addr_uses)
    (*comp_cost) += inv->cost * inv->eqno;

#ifdef STACK_REGS
  {
    /* Hoisting constant pool constants into stack regs may cost more than
       just single register.  On x87, the balance is affected both by the
       small number of FP registers, and by its register stack organization,
       that forces us to add compensation code in and around the loop to
       shuffle the operands to the top of stack before use, and pop them
       from the stack after the loop finishes.

       To model this effect, we increase the number of registers needed for
       stack registers by two: one register push, and one register pop.
       This usually has the effect that FP constant loads from the constant
       pool are not moved out of the loop.

       Note that this also means that dependent invariants can not be moved.
       However, the primary purpose of this pass is to move loop invariant
       address arithmetic out of loops, and address arithmetic that depends
       on floating point constants is unlikely to ever occur.  */
    rtx set = single_set (inv->insn);
    if (set
	&& IS_STACK_MODE (GET_MODE (SET_SRC (set)))
	&& constant_pool_constant_p (SET_SRC (set)))
      {
	if (flag_ira_loop_pressure)
	  regs_needed[ira_stack_reg_pressure_class] += 2;
	else
	  regs_needed[0] += 2;
      }
  }
#endif

  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, depno, bi)
    {
      bool check_p;
      enum reg_class dep_cl = ALL_REGS;
      int dep_ret;

      dep = invariants[depno];

      /* If DEP is moved out of the loop, it is not a depends_on any more.  */
      if (dep->move)
	continue;

      dep_ret = get_inv_cost (dep, &acomp_cost, aregs_needed, &dep_cl);

      if (! flag_ira_loop_pressure)
	check_p = aregs_needed[0] != 0;
      else
	{
	  for (i = 0; i < ira_pressure_classes_num; i++)
	    if (aregs_needed[ira_pressure_classes[i]] != 0)
	      break;
	  check_p = i < ira_pressure_classes_num;

	  if ((dep_ret == 1) || ((dep_ret == 0) && (*cl != dep_cl)))
	    {
	      *cl = ALL_REGS;
	      ret = 1;
	    }
	}
      if (check_p
	  /* We need to check always_executed, since if the original value of
	     the invariant may be preserved, we may need to keep it in a
	     separate register.  TODO check whether the register has an
	     use outside of the loop.  */
	  && dep->always_executed
	  && !dep->def->uses->next)
	{
	  /* If this is a single use, after moving the dependency we will not
	     need a new register.  */
	  if (! flag_ira_loop_pressure)
	    aregs_needed[0]--;
	  else
	    {
	      int nregs;
	      enum reg_class pressure_class;

	      pressure_class = get_pressure_class_and_nregs (inv->insn, &nregs);
	      aregs_needed[pressure_class] -= nregs;
	    }
	}

      if (! flag_ira_loop_pressure)
	regs_needed[0] += aregs_needed[0];
      else
	{
	  for (i = 0; i < ira_pressure_classes_num; i++)
	    regs_needed[ira_pressure_classes[i]]
	      += aregs_needed[ira_pressure_classes[i]];
	}
      (*comp_cost) += acomp_cost;
    }
  return ret;
}

/* Calculates gain for eliminating invariant INV.  REGS_USED is the number
   of registers used in the loop, NEW_REGS is the number of new variables
   already added due to the invariant motion.  The number of registers needed
   for it is stored in *REGS_NEEDED.  SPEED and CALL_P are flags passed
   through to estimate_reg_pressure_cost. */

static int
gain_for_invariant (struct invariant *inv, unsigned *regs_needed,
		    unsigned *new_regs, unsigned regs_used,
		    bool speed, bool call_p)
{
  int comp_cost, size_cost;
  /* Workaround -Wmaybe-uninitialized false positive during
     profiledbootstrap by initializing it.  */
  enum reg_class cl = NO_REGS;
  int ret;

  actual_stamp++;

  ret = get_inv_cost (inv, &comp_cost, regs_needed, &cl);

  if (! flag_ira_loop_pressure)
    {
      size_cost = (estimate_reg_pressure_cost (new_regs[0] + regs_needed[0],
					       regs_used, speed, call_p)
		   - estimate_reg_pressure_cost (new_regs[0],
						 regs_used, speed, call_p));
    }
  else if (ret < 0)
    return -1;
  else if ((ret == 0) && (cl == NO_REGS))
    /* Hoist it anyway since it does not impact register pressure.  */
    return 1;
  else
    {
      int i;
      enum reg_class pressure_class;

      for (i = 0; i < ira_pressure_classes_num; i++)
	{
	  pressure_class = ira_pressure_classes[i];

	  if (!reg_classes_intersect_p (pressure_class, cl))
	    continue;

	  if ((int) new_regs[pressure_class]
	      + (int) regs_needed[pressure_class]
	      + LOOP_DATA (curr_loop)->max_reg_pressure[pressure_class]
	      + IRA_LOOP_RESERVED_REGS
	      > ira_class_hard_regs_num[pressure_class])
	    break;
	}
      if (i < ira_pressure_classes_num)
	/* There will be register pressure excess and we want not to
	   make this loop invariant motion.  All loop invariants with
	   non-positive gains will be rejected in function
	   find_invariants_to_move.  Therefore we return the negative
	   number here.

	   One could think that this rejects also expensive loop
	   invariant motions and this will hurt code performance.
	   However numerous experiments with different heuristics
	   taking invariant cost into account did not confirm this
	   assumption.  There are possible explanations for this
	   result:
           o probably all expensive invariants were already moved out
             of the loop by PRE and gimple invariant motion pass.
           o expensive invariant execution will be hidden by insn
             scheduling or OOO processor hardware because usually such
             invariants have a lot of freedom to be executed
             out-of-order.
	   Another reason for ignoring invariant cost vs spilling cost
	   heuristics is also in difficulties to evaluate accurately
	   spill cost at this stage.  */
	return -1;
      else
	size_cost = 0;
    }

  return comp_cost - size_cost;
}

/* Finds invariant with best gain for moving.  Returns the gain, stores
   the invariant in *BEST and number of registers needed for it to
   *REGS_NEEDED.  REGS_USED is the number of registers used in the loop.
   NEW_REGS is the number of new variables already added due to invariant
   motion.  */

static int
best_gain_for_invariant (struct invariant **best, unsigned *regs_needed,
			 unsigned *new_regs, unsigned regs_used,
			 bool speed, bool call_p)
{
  struct invariant *inv;
  int i, gain = 0, again;
  unsigned aregs_needed[N_REG_CLASSES], invno;

  FOR_EACH_VEC_ELT (invariants, invno, inv)
    {
      if (inv->move)
	continue;

      /* Only consider the "representatives" of equivalent invariants.  */
      if (inv->eqto != inv->invno)
	continue;

      again = gain_for_invariant (inv, aregs_needed, new_regs, regs_used,
      				  speed, call_p);
      if (again > gain)
	{
	  gain = again;
	  *best = inv;
	  if (! flag_ira_loop_pressure)
	    regs_needed[0] = aregs_needed[0];
	  else
	    {
	      for (i = 0; i < ira_pressure_classes_num; i++)
		regs_needed[ira_pressure_classes[i]]
		  = aregs_needed[ira_pressure_classes[i]];
	    }
	}
    }

  return gain;
}

/* Marks invariant INVNO and all its dependencies for moving.  */

static void
set_move_mark (unsigned invno, int gain)
{
  struct invariant *inv = invariants[invno];
  bitmap_iterator bi;

  /* Find the representative of the class of the equivalent invariants.  */
  inv = invariants[inv->eqto];

  if (inv->move)
    return;
  inv->move = true;

  if (dump_file)
    {
      if (gain >= 0)
	fprintf (dump_file, "Decided to move invariant %d -- gain %d\n",
		 invno, gain);
      else
	fprintf (dump_file, "Decided to move dependent invariant %d\n",
		 invno);
    };

  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, invno, bi)
    {
      set_move_mark (invno, -1);
    }
}

/* Determines which invariants to move.  */

static void
find_invariants_to_move (bool speed, bool call_p)
{
  int gain;
  unsigned i, regs_used, regs_needed[N_REG_CLASSES], new_regs[N_REG_CLASSES];
  struct invariant *inv = NULL;

  if (!invariants.length ())
    return;

  if (flag_ira_loop_pressure)
    /* REGS_USED is actually never used when the flag is on.  */
    regs_used = 0;
  else
    /* We do not really do a good job in estimating number of
       registers used; we put some initial bound here to stand for
       induction variables etc.  that we do not detect.  */
    {
      unsigned int n_regs = DF_REG_SIZE (df);

      regs_used = 2;

      for (i = 0; i < n_regs; i++)
	{
	  if (!DF_REGNO_FIRST_DEF (i) && DF_REGNO_LAST_USE (i))
	    {
	      /* This is a value that is used but not changed inside loop.  */
	      regs_used++;
	    }
	}
    }

  if (! flag_ira_loop_pressure)
    new_regs[0] = regs_needed[0] = 0;
  else
    {
      for (i = 0; (int) i < ira_pressure_classes_num; i++)
	new_regs[ira_pressure_classes[i]] = 0;
    }
  while ((gain = best_gain_for_invariant (&inv, regs_needed,
					  new_regs, regs_used,
					  speed, call_p)) > 0)
    {
      set_move_mark (inv->invno, gain);
      if (! flag_ira_loop_pressure)
	new_regs[0] += regs_needed[0];
      else
	{
	  for (i = 0; (int) i < ira_pressure_classes_num; i++)
	    new_regs[ira_pressure_classes[i]]
	      += regs_needed[ira_pressure_classes[i]];
	}
    }
}

/* Replace the uses, reached by the definition of invariant INV, by REG.

   IN_GROUP is nonzero if this is part of a group of changes that must be
   performed as a group.  In that case, the changes will be stored.  The
   function `apply_change_group' will validate and apply the changes.  */

static int
replace_uses (struct invariant *inv, rtx reg, bool in_group)
{
  /* Replace the uses we know to be dominated.  It saves work for copy
     propagation, and also it is necessary so that dependent invariants
     are computed right.  */
  if (inv->def)
    {
      struct use *use;
      for (use = inv->def->uses; use; use = use->next)
	validate_change (use->insn, use->pos, reg, true);

      /* If we aren't part of a larger group, apply the changes now.  */
      if (!in_group)
	return apply_change_group ();
    }

  return 1;
}

/* Whether invariant INV setting REG can be moved out of LOOP, at the end of
   the block preceding its header.  */

static bool
can_move_invariant_reg (struct loop *loop, struct invariant *inv, rtx reg)
{
  df_ref def, use;
  unsigned int dest_regno, defs_in_loop_count = 0;
  rtx_insn *insn = inv->insn;
  basic_block bb = BLOCK_FOR_INSN (inv->insn);

  /* We ignore hard register and memory access for cost and complexity reasons.
     Hard register are few at this stage and expensive to consider as they
     require building a separate data flow.  Memory access would require using
     df_simulate_* and can_move_insns_across functions and is more complex.  */
  if (!REG_P (reg) || HARD_REGISTER_P (reg))
    return false;

  /* Check whether the set is always executed.  We could omit this condition if
     we know that the register is unused outside of the loop, but it does not
     seem worth finding out.  */
  if (!inv->always_executed)
    return false;

  /* Check that all uses that would be dominated by def are already dominated
     by it.  */
  dest_regno = REGNO (reg);
  for (use = DF_REG_USE_CHAIN (dest_regno); use; use = DF_REF_NEXT_REG (use))
    {
      rtx_insn *use_insn;
      basic_block use_bb;

      use_insn = DF_REF_INSN (use);
      use_bb = BLOCK_FOR_INSN (use_insn);

      /* Ignore instruction considered for moving.  */
      if (use_insn == insn)
	continue;

      /* Don't consider uses outside loop.  */
      if (!flow_bb_inside_loop_p (loop, use_bb))
	continue;

      /* Don't move if a use is not dominated by def in insn.  */
      if (use_bb == bb && DF_INSN_LUID (insn) >= DF_INSN_LUID (use_insn))
	return false;
      if (!dominated_by_p (CDI_DOMINATORS, use_bb, bb))
	return false;
    }

  /* Check for other defs.  Any other def in the loop might reach a use
     currently reached by the def in insn.  */
  for (def = DF_REG_DEF_CHAIN (dest_regno); def; def = DF_REF_NEXT_REG (def))
    {
      basic_block def_bb = DF_REF_BB (def);

      /* Defs in exit block cannot reach a use they weren't already.  */
      if (single_succ_p (def_bb))
	{
	  basic_block def_bb_succ;

	  def_bb_succ = single_succ (def_bb);
	  if (!flow_bb_inside_loop_p (loop, def_bb_succ))
	    continue;
	}

      if (++defs_in_loop_count > 1)
	return false;
    }

  return true;
}

/* Move invariant INVNO out of the LOOP.  Returns true if this succeeds, false
   otherwise.  */

static bool
move_invariant_reg (struct loop *loop, unsigned invno)
{
  struct invariant *inv = invariants[invno];
  struct invariant *repr = invariants[inv->eqto];
  unsigned i;
  basic_block preheader = loop_preheader_edge (loop)->src;
  rtx reg, set, dest, note;
  bitmap_iterator bi;
  int regno = -1;

  if (inv->reg)
    return true;
  if (!repr->move)
    return false;

  /* If this is a representative of the class of equivalent invariants,
     really move the invariant.  Otherwise just replace its use with
     the register used for the representative.  */
  if (inv == repr)
    {
      if (inv->depends_on)
	{
	  EXECUTE_IF_SET_IN_BITMAP (inv->depends_on, 0, i, bi)
	    {
	      if (!move_invariant_reg (loop, i))
		goto fail;
	    }
	}

      /* If possible, just move the set out of the loop.  Otherwise, we
	 need to create a temporary register.  */
      set = single_set (inv->insn);
      reg = dest = SET_DEST (set);
      if (GET_CODE (reg) == SUBREG)
	reg = SUBREG_REG (reg);
      if (REG_P (reg))
	regno = REGNO (reg);

      if (!can_move_invariant_reg (loop, inv, dest))
	{
	  reg = gen_reg_rtx_and_attrs (dest);

	  /* Try replacing the destination by a new pseudoregister.  */
	  validate_change (inv->insn, &SET_DEST (set), reg, true);

	  /* As well as all the dominated uses.  */
	  replace_uses (inv, reg, true);

	  /* And validate all the changes.  */
	  if (!apply_change_group ())
	    goto fail;

	  emit_insn_after (gen_move_insn (dest, reg), inv->insn);
	}
      else if (dump_file)
	fprintf (dump_file, "Invariant %d moved without introducing a new "
			    "temporary register\n", invno);
      reorder_insns (inv->insn, inv->insn, BB_END (preheader));
      df_recompute_luids (preheader);

      /* If there is a REG_EQUAL note on the insn we just moved, and the
	 insn is in a basic block that is not always executed or the note
	 contains something for which we don't know the invariant status,
	 the note may no longer be valid after we move the insn.  Note that
	 uses in REG_EQUAL notes are taken into account in the computation
	 of invariants, so it is safe to retain the note even if it contains
	 register references for which we know the invariant status.  */
      if ((note = find_reg_note (inv->insn, REG_EQUAL, NULL_RTX))
	  && (!inv->always_executed
	      || !check_maybe_invariant (XEXP (note, 0))))
	remove_note (inv->insn, note);
    }
  else
    {
      if (!move_invariant_reg (loop, repr->invno))
	goto fail;
      reg = repr->reg;
      regno = repr->orig_regno;
      if (!replace_uses (inv, reg, false))
	goto fail;
      set = single_set (inv->insn);
      emit_insn_after (gen_move_insn (SET_DEST (set), reg), inv->insn);
      delete_insn (inv->insn);
    }

  inv->reg = reg;
  inv->orig_regno = regno;

  return true;

fail:
  /* If we failed, clear move flag, so that we do not try to move inv
     again.  */
  if (dump_file)
    fprintf (dump_file, "Failed to move invariant %d\n", invno);
  inv->move = false;
  inv->reg = NULL_RTX;
  inv->orig_regno = -1;

  return false;
}

/* Move selected invariant out of the LOOP.  Newly created regs are marked
   in TEMPORARY_REGS.  */

static void
move_invariants (struct loop *loop)
{
  struct invariant *inv;
  unsigned i;

  FOR_EACH_VEC_ELT (invariants, i, inv)
    move_invariant_reg (loop, i);
  if (flag_ira_loop_pressure && resize_reg_info ())
    {
      FOR_EACH_VEC_ELT (invariants, i, inv)
	if (inv->reg != NULL_RTX)
	  {
	    if (inv->orig_regno >= 0)
	      setup_reg_classes (REGNO (inv->reg),
				 reg_preferred_class (inv->orig_regno),
				 reg_alternate_class (inv->orig_regno),
				 reg_allocno_class (inv->orig_regno));
	    else
	      setup_reg_classes (REGNO (inv->reg),
				 GENERAL_REGS, NO_REGS, GENERAL_REGS);
	  }
    }
}

/* Initializes invariant motion data.  */

static void
init_inv_motion_data (void)
{
  actual_stamp = 1;

  invariants.create (100);
}

/* Frees the data allocated by invariant motion.  */

static void
free_inv_motion_data (void)
{
  unsigned i;
  struct def *def;
  struct invariant *inv;

  check_invariant_table_size ();
  for (i = 0; i < DF_DEFS_TABLE_SIZE (); i++)
    {
      inv = invariant_table[i];
      if (inv)
	{
	  def = inv->def;
	  gcc_assert (def != NULL);

	  free_use_list (def->uses);
	  free (def);
	  invariant_table[i] = NULL;
	}
    }

  FOR_EACH_VEC_ELT (invariants, i, inv)
    {
      BITMAP_FREE (inv->depends_on);
      free (inv);
    }
  invariants.release ();
}

/* Move the invariants out of the LOOP.  */

static void
move_single_loop_invariants (struct loop *loop)
{
  init_inv_motion_data ();

  find_invariants (loop);
  find_invariants_to_move (optimize_loop_for_speed_p (loop),
			   LOOP_DATA (loop)->has_call);
  move_invariants (loop);

  free_inv_motion_data ();
}

/* Releases the auxiliary data for LOOP.  */

static void
free_loop_data (struct loop *loop)
{
  struct loop_data *data = LOOP_DATA (loop);
  if (!data)
    return;

  bitmap_clear (&LOOP_DATA (loop)->regs_ref);
  bitmap_clear (&LOOP_DATA (loop)->regs_live);
  free (data);
  loop->aux = NULL;
}



/* Registers currently living.  */
static bitmap_head curr_regs_live;

/* Current reg pressure for each pressure class.  */
static int curr_reg_pressure[N_REG_CLASSES];

/* Record all regs that are set in any one insn.  Communication from
   mark_reg_{store,clobber} and global_conflicts.  Asm can refer to
   all hard-registers.  */
static rtx regs_set[(FIRST_PSEUDO_REGISTER > MAX_RECOG_OPERANDS
		     ? FIRST_PSEUDO_REGISTER : MAX_RECOG_OPERANDS) * 2];
/* Number of regs stored in the previous array.  */
static int n_regs_set;

/* Return pressure class and number of needed hard registers (through
   *NREGS) of register REGNO.  */
static enum reg_class
get_regno_pressure_class (int regno, int *nregs)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      enum reg_class pressure_class;

      pressure_class = reg_allocno_class (regno);
      pressure_class = ira_pressure_class_translate[pressure_class];
      *nregs
	= ira_reg_class_max_nregs[pressure_class][PSEUDO_REGNO_MODE (regno)];
      return pressure_class;
    }
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno)
	   && ! TEST_HARD_REG_BIT (eliminable_regset, regno))
    {
      *nregs = 1;
      return ira_pressure_class_translate[REGNO_REG_CLASS (regno)];
    }
  else
    {
      *nregs = 0;
      return NO_REGS;
    }
}

/* Increase (if INCR_P) or decrease current register pressure for
   register REGNO.  */
static void
change_pressure (int regno, bool incr_p)
{
  int nregs;
  enum reg_class pressure_class;

  pressure_class = get_regno_pressure_class (regno, &nregs);
  if (! incr_p)
    curr_reg_pressure[pressure_class] -= nregs;
  else
    {
      curr_reg_pressure[pressure_class] += nregs;
      if (LOOP_DATA (curr_loop)->max_reg_pressure[pressure_class]
	  < curr_reg_pressure[pressure_class])
	LOOP_DATA (curr_loop)->max_reg_pressure[pressure_class]
	  = curr_reg_pressure[pressure_class];
    }
}

/* Mark REGNO birth.  */
static void
mark_regno_live (int regno)
{
  struct loop *loop;

  for (loop = curr_loop;
       loop != current_loops->tree_root;
       loop = loop_outer (loop))
    bitmap_set_bit (&LOOP_DATA (loop)->regs_live, regno);
  if (!bitmap_set_bit (&curr_regs_live, regno))
    return;
  change_pressure (regno, true);
}

/* Mark REGNO death.  */
static void
mark_regno_death (int regno)
{
  if (! bitmap_clear_bit (&curr_regs_live, regno))
    return;
  change_pressure (regno, false);
}

/* Mark setting register REG.  */
static void
mark_reg_store (rtx reg, const_rtx setter ATTRIBUTE_UNUSED,
		void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg))
    return;

  regs_set[n_regs_set++] = reg;

  unsigned int end_regno = END_REGNO (reg);
  for (unsigned int regno = REGNO (reg); regno < end_regno; ++regno)
    mark_regno_live (regno);
}

/* Mark clobbering register REG.  */
static void
mark_reg_clobber (rtx reg, const_rtx setter, void *data)
{
  if (GET_CODE (setter) == CLOBBER)
    mark_reg_store (reg, setter, data);
}

/* Mark register REG death.  */
static void
mark_reg_death (rtx reg)
{
  unsigned int end_regno = END_REGNO (reg);
  for (unsigned int regno = REGNO (reg); regno < end_regno; ++regno)
    mark_regno_death (regno);
}

/* Mark occurrence of registers in X for the current loop.  */
static void
mark_ref_regs (rtx x)
{
  RTX_CODE code;
  int i;
  const char *fmt;

  if (!x)
    return;

  code = GET_CODE (x);
  if (code == REG)
    {
      struct loop *loop;

      for (loop = curr_loop;
	   loop != current_loops->tree_root;
	   loop = loop_outer (loop))
	bitmap_set_bit (&LOOP_DATA (loop)->regs_ref, REGNO (x));
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      mark_ref_regs (XEXP (x, i));
    else if (fmt[i] == 'E')
      {
	int j;

	for (j = 0; j < XVECLEN (x, i); j++)
	  mark_ref_regs (XVECEXP (x, i, j));
      }
}

/* Calculate register pressure in the loops.  */
static void
calculate_loop_reg_pressure (void)
{
  int i;
  unsigned int j;
  bitmap_iterator bi;
  basic_block bb;
  rtx_insn *insn;
  rtx link;
  struct loop *loop, *parent;

  FOR_EACH_LOOP (loop, 0)
    if (loop->aux == NULL)
      {
	loop->aux = xcalloc (1, sizeof (struct loop_data));
	bitmap_initialize (&LOOP_DATA (loop)->regs_ref, &reg_obstack);
	bitmap_initialize (&LOOP_DATA (loop)->regs_live, &reg_obstack);
      }
  ira_setup_eliminable_regset ();
  bitmap_initialize (&curr_regs_live, &reg_obstack);
  FOR_EACH_BB_FN (bb, cfun)
    {
      curr_loop = bb->loop_father;
      if (curr_loop == current_loops->tree_root)
	continue;

      for (loop = curr_loop;
	   loop != current_loops->tree_root;
	   loop = loop_outer (loop))
	bitmap_ior_into (&LOOP_DATA (loop)->regs_live, DF_LR_IN (bb));

      bitmap_copy (&curr_regs_live, DF_LR_IN (bb));
      for (i = 0; i < ira_pressure_classes_num; i++)
	curr_reg_pressure[ira_pressure_classes[i]] = 0;
      EXECUTE_IF_SET_IN_BITMAP (&curr_regs_live, 0, j, bi)
	change_pressure (j, true);

      FOR_BB_INSNS (bb, insn)
	{
	  if (! NONDEBUG_INSN_P (insn))
	    continue;

	  mark_ref_regs (PATTERN (insn));
	  n_regs_set = 0;
	  note_stores (PATTERN (insn), mark_reg_clobber, NULL);

	  /* Mark any registers dead after INSN as dead now.  */

	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_DEAD)
	      mark_reg_death (XEXP (link, 0));

	  /* Mark any registers set in INSN as live,
	     and mark them as conflicting with all other live regs.
	     Clobbers are processed again, so they conflict with
	     the registers that are set.  */

	  note_stores (PATTERN (insn), mark_reg_store, NULL);

	  if (AUTO_INC_DEC)
	    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	      if (REG_NOTE_KIND (link) == REG_INC)
		mark_reg_store (XEXP (link, 0), NULL_RTX, NULL);

	  while (n_regs_set-- > 0)
	    {
	      rtx note = find_regno_note (insn, REG_UNUSED,
					  REGNO (regs_set[n_regs_set]));
	      if (! note)
		continue;

	      mark_reg_death (XEXP (note, 0));
	    }
	}
    }
  bitmap_clear (&curr_regs_live);
  if (flag_ira_region == IRA_REGION_MIXED
      || flag_ira_region == IRA_REGION_ALL)
    FOR_EACH_LOOP (loop, 0)
      {
	EXECUTE_IF_SET_IN_BITMAP (&LOOP_DATA (loop)->regs_live, 0, j, bi)
	  if (! bitmap_bit_p (&LOOP_DATA (loop)->regs_ref, j))
	    {
	      enum reg_class pressure_class;
	      int nregs;

	      pressure_class = get_regno_pressure_class (j, &nregs);
	      LOOP_DATA (loop)->max_reg_pressure[pressure_class] -= nregs;
	    }
      }
  if (dump_file == NULL)
    return;
  FOR_EACH_LOOP (loop, 0)
    {
      parent = loop_outer (loop);
      fprintf (dump_file, "\n  Loop %d (parent %d, header bb%d, depth %d)\n",
	       loop->num, (parent == NULL ? -1 : parent->num),
	       loop->header->index, loop_depth (loop));
      fprintf (dump_file, "\n    ref. regnos:");
      EXECUTE_IF_SET_IN_BITMAP (&LOOP_DATA (loop)->regs_ref, 0, j, bi)
	fprintf (dump_file, " %d", j);
      fprintf (dump_file, "\n    live regnos:");
      EXECUTE_IF_SET_IN_BITMAP (&LOOP_DATA (loop)->regs_live, 0, j, bi)
	fprintf (dump_file, " %d", j);
      fprintf (dump_file, "\n    Pressure:");
      for (i = 0; (int) i < ira_pressure_classes_num; i++)
	{
	  enum reg_class pressure_class;

	  pressure_class = ira_pressure_classes[i];
	  if (LOOP_DATA (loop)->max_reg_pressure[pressure_class] == 0)
	    continue;
	  fprintf (dump_file, " %s=%d", reg_class_names[pressure_class],
		   LOOP_DATA (loop)->max_reg_pressure[pressure_class]);
	}
      fprintf (dump_file, "\n");
    }
}



/* Move the invariants out of the loops.  */

void
move_loop_invariants (void)
{
  struct loop *loop;

  if (flag_ira_loop_pressure)
    {
      df_analyze ();
      regstat_init_n_sets_and_refs ();
      ira_set_pseudo_classes (true, dump_file);
      calculate_loop_reg_pressure ();
      regstat_free_n_sets_and_refs ();
    }
  df_set_flags (DF_EQ_NOTES + DF_DEFER_INSN_RESCAN);
  /* Process the loops, innermost first.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      curr_loop = loop;
      /* move_single_loop_invariants for very large loops
	 is time consuming and might need a lot of memory.  */
      if (loop->num_nodes <= (unsigned) LOOP_INVARIANT_MAX_BBS_IN_LOOP)
	move_single_loop_invariants (loop);
    }

  FOR_EACH_LOOP (loop, 0)
    {
      free_loop_data (loop);
    }

  if (flag_ira_loop_pressure)
    /* There is no sense to keep this info because it was most
       probably outdated by subsequent passes.  */
    free_reg_info ();
  free (invariant_table);
  invariant_table = NULL;
  invariant_table_size = 0;

  checking_verify_flow_info ();
}
