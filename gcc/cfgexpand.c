/* A pass for lowering trees to RTL.
   Copyright (C) 2004-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "varasm.h"
#include "stor-layout.h"
#include "stmt.h"
#include "print-tree.h"
#include "tm_p.h"
#include "predict.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "basic-block.h"
#include "insn-codes.h"
#include "optabs.h"
#include "flags.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "expr.h"
#include "langhooks.h"
#include "bitmap.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "except.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "toplev.h"
#include "debug.h"
#include "params.h"
#include "tree-inline.h"
#include "value-prof.h"
#include "target.h"
#include "tree-ssa-live.h"
#include "tree-outof-ssa.h"
#include "sbitmap.h"
#include "cfgloop.h"
#include "regs.h" /* For reg_renumber.  */
#include "insn-attr.h" /* For INSN_SCHEDULING.  */
#include "asan.h"
#include "tree-ssa-address.h"
#include "recog.h"
#include "output.h"
#include "builtins.h"
#include "tree-chkp.h"
#include "rtl-chkp.h"

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#endif

/* This variable holds information helping the rewriting of SSA trees
   into RTL.  */
struct ssaexpand SA;

/* This variable holds the currently expanded gimple statement for purposes
   of comminucating the profile info to the builtin expanders.  */
gimple currently_expanding_gimple_stmt;

static rtx expand_debug_expr (tree);

/* Return an expression tree corresponding to the RHS of GIMPLE
   statement STMT.  */

tree
gimple_assign_rhs_to_tree (gimple stmt)
{
  tree t;
  enum gimple_rhs_class grhs_class;

  grhs_class = get_gimple_rhs_class (gimple_expr_code (stmt));

  if (grhs_class == GIMPLE_TERNARY_RHS)
    t = build3 (gimple_assign_rhs_code (stmt),
		TREE_TYPE (gimple_assign_lhs (stmt)),
		gimple_assign_rhs1 (stmt),
		gimple_assign_rhs2 (stmt),
		gimple_assign_rhs3 (stmt));
  else if (grhs_class == GIMPLE_BINARY_RHS)
    t = build2 (gimple_assign_rhs_code (stmt),
		TREE_TYPE (gimple_assign_lhs (stmt)),
		gimple_assign_rhs1 (stmt),
		gimple_assign_rhs2 (stmt));
  else if (grhs_class == GIMPLE_UNARY_RHS)
    t = build1 (gimple_assign_rhs_code (stmt),
		TREE_TYPE (gimple_assign_lhs (stmt)),
		gimple_assign_rhs1 (stmt));
  else if (grhs_class == GIMPLE_SINGLE_RHS)
    {
      t = gimple_assign_rhs1 (stmt);
      /* Avoid modifying this tree in place below.  */
      if ((gimple_has_location (stmt) && CAN_HAVE_LOCATION_P (t)
	   && gimple_location (stmt) != EXPR_LOCATION (t))
	  || (gimple_block (stmt)
	      && currently_expanding_to_rtl
	      && EXPR_P (t)))
	t = copy_node (t);
    }
  else
    gcc_unreachable ();

  if (gimple_has_location (stmt) && CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, gimple_location (stmt));

  return t;
}


#ifndef STACK_ALIGNMENT_NEEDED
#define STACK_ALIGNMENT_NEEDED 1
#endif

#define SSAVAR(x) (TREE_CODE (x) == SSA_NAME ? SSA_NAME_VAR (x) : x)

/* Associate declaration T with storage space X.  If T is no
   SSA name this is exactly SET_DECL_RTL, otherwise make the
   partition of T associated with X.  */
static inline void
set_rtl (tree t, rtx x)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      SA.partition_to_pseudo[var_to_partition (SA.map, t)] = x;
      if (x && !MEM_P (x))
	set_reg_attrs_for_decl_rtl (SSA_NAME_VAR (t), x);
      /* For the benefit of debug information at -O0 (where vartracking
         doesn't run) record the place also in the base DECL if it's
	 a normal variable (not a parameter).  */
      if (x && x != pc_rtx && TREE_CODE (SSA_NAME_VAR (t)) == VAR_DECL)
	{
	  tree var = SSA_NAME_VAR (t);
	  /* If we don't yet have something recorded, just record it now.  */
	  if (!DECL_RTL_SET_P (var))
	    SET_DECL_RTL (var, x);
	  /* If we have it set already to "multiple places" don't
	     change this.  */
	  else if (DECL_RTL (var) == pc_rtx)
	    ;
	  /* If we have something recorded and it's not the same place
	     as we want to record now, we have multiple partitions for the
	     same base variable, with different places.  We can't just
	     randomly chose one, hence we have to say that we don't know.
	     This only happens with optimization, and there var-tracking
	     will figure out the right thing.  */
	  else if (DECL_RTL (var) != x)
	    SET_DECL_RTL (var, pc_rtx);
	}
    }
  else
    SET_DECL_RTL (t, x);
}

/* This structure holds data relevant to one variable that will be
   placed in a stack slot.  */
struct stack_var
{
  /* The Variable.  */
  tree decl;

  /* Initially, the size of the variable.  Later, the size of the partition,
     if this variable becomes it's partition's representative.  */
  HOST_WIDE_INT size;

  /* The *byte* alignment required for this variable.  Or as, with the
     size, the alignment for this partition.  */
  unsigned int alignb;

  /* The partition representative.  */
  size_t representative;

  /* The next stack variable in the partition, or EOC.  */
  size_t next;

  /* The numbers of conflicting stack variables.  */
  bitmap conflicts;
};

#define EOC  ((size_t)-1)

/* We have an array of such objects while deciding allocation.  */
static struct stack_var *stack_vars;
static size_t stack_vars_alloc;
static size_t stack_vars_num;
static hash_map<tree, size_t> *decl_to_stack_part;

/* Conflict bitmaps go on this obstack.  This allows us to destroy
   all of them in one big sweep.  */
static bitmap_obstack stack_var_bitmap_obstack;

/* An array of indices such that stack_vars[stack_vars_sorted[i]].size
   is non-decreasing.  */
static size_t *stack_vars_sorted;

/* The phase of the stack frame.  This is the known misalignment of
   virtual_stack_vars_rtx from PREFERRED_STACK_BOUNDARY.  That is,
   (frame_offset+frame_phase) % PREFERRED_STACK_BOUNDARY == 0.  */
static int frame_phase;

/* Used during expand_used_vars to remember if we saw any decls for
   which we'd like to enable stack smashing protection.  */
static bool has_protected_decls;

/* Used during expand_used_vars.  Remember if we say a character buffer
   smaller than our cutoff threshold.  Used for -Wstack-protector.  */
static bool has_short_buffer;

/* Compute the byte alignment to use for DECL.  Ignore alignment
   we can't do with expected alignment of the stack boundary.  */

static unsigned int
align_local_variable (tree decl)
{
  unsigned int align = LOCAL_DECL_ALIGNMENT (decl);
  DECL_ALIGN (decl) = align;
  return align / BITS_PER_UNIT;
}

/* Align given offset BASE with ALIGN.  Truncate up if ALIGN_UP is true,
   down otherwise.  Return truncated BASE value.  */

static inline unsigned HOST_WIDE_INT
align_base (HOST_WIDE_INT base, unsigned HOST_WIDE_INT align, bool align_up)
{
  return align_up ? (base + align - 1) & -align : base & -align;
}

/* Allocate SIZE bytes at byte alignment ALIGN from the stack frame.
   Return the frame offset.  */

static HOST_WIDE_INT
alloc_stack_frame_space (HOST_WIDE_INT size, unsigned HOST_WIDE_INT align)
{
  HOST_WIDE_INT offset, new_frame_offset;

  if (FRAME_GROWS_DOWNWARD)
    {
      new_frame_offset
	= align_base (frame_offset - frame_phase - size,
		      align, false) + frame_phase;
      offset = new_frame_offset;
    }
  else
    {
      new_frame_offset
	= align_base (frame_offset - frame_phase, align, true) + frame_phase;
      offset = new_frame_offset;
      new_frame_offset += size;
    }
  frame_offset = new_frame_offset;

  if (frame_offset_overflow (frame_offset, cfun->decl))
    frame_offset = offset = 0;

  return offset;
}

/* Accumulate DECL into STACK_VARS.  */

static void
add_stack_var (tree decl)
{
  struct stack_var *v;

  if (stack_vars_num >= stack_vars_alloc)
    {
      if (stack_vars_alloc)
	stack_vars_alloc = stack_vars_alloc * 3 / 2;
      else
	stack_vars_alloc = 32;
      stack_vars
	= XRESIZEVEC (struct stack_var, stack_vars, stack_vars_alloc);
    }
  if (!decl_to_stack_part)
    decl_to_stack_part = new hash_map<tree, size_t>;

  v = &stack_vars[stack_vars_num];
  decl_to_stack_part->put (decl, stack_vars_num);

  v->decl = decl;
  v->size = tree_to_uhwi (DECL_SIZE_UNIT (SSAVAR (decl)));
  /* Ensure that all variables have size, so that &a != &b for any two
     variables that are simultaneously live.  */
  if (v->size == 0)
    v->size = 1;
  v->alignb = align_local_variable (SSAVAR (decl));
  /* An alignment of zero can mightily confuse us later.  */
  gcc_assert (v->alignb != 0);

  /* All variables are initially in their own partition.  */
  v->representative = stack_vars_num;
  v->next = EOC;

  /* All variables initially conflict with no other.  */
  v->conflicts = NULL;

  /* Ensure that this decl doesn't get put onto the list twice.  */
  set_rtl (decl, pc_rtx);

  stack_vars_num++;
}

/* Make the decls associated with luid's X and Y conflict.  */

static void
add_stack_var_conflict (size_t x, size_t y)
{
  struct stack_var *a = &stack_vars[x];
  struct stack_var *b = &stack_vars[y];
  if (!a->conflicts)
    a->conflicts = BITMAP_ALLOC (&stack_var_bitmap_obstack);
  if (!b->conflicts)
    b->conflicts = BITMAP_ALLOC (&stack_var_bitmap_obstack);
  bitmap_set_bit (a->conflicts, y);
  bitmap_set_bit (b->conflicts, x);
}

/* Check whether the decls associated with luid's X and Y conflict.  */

static bool
stack_var_conflict_p (size_t x, size_t y)
{
  struct stack_var *a = &stack_vars[x];
  struct stack_var *b = &stack_vars[y];
  if (x == y)
    return false;
  /* Partitions containing an SSA name result from gimple registers
     with things like unsupported modes.  They are top-level and
     hence conflict with everything else.  */
  if (TREE_CODE (a->decl) == SSA_NAME || TREE_CODE (b->decl) == SSA_NAME)
    return true;

  if (!a->conflicts || !b->conflicts)
    return false;
  return bitmap_bit_p (a->conflicts, y);
}

/* Callback for walk_stmt_ops.  If OP is a decl touched by add_stack_var
   enter its partition number into bitmap DATA.  */

static bool
visit_op (gimple, tree op, tree, void *data)
{
  bitmap active = (bitmap)data;
  op = get_base_address (op);
  if (op
      && DECL_P (op)
      && DECL_RTL_IF_SET (op) == pc_rtx)
    {
      size_t *v = decl_to_stack_part->get (op);
      if (v)
	bitmap_set_bit (active, *v);
    }
  return false;
}

/* Callback for walk_stmt_ops.  If OP is a decl touched by add_stack_var
   record conflicts between it and all currently active other partitions
   from bitmap DATA.  */

static bool
visit_conflict (gimple, tree op, tree, void *data)
{
  bitmap active = (bitmap)data;
  op = get_base_address (op);
  if (op
      && DECL_P (op)
      && DECL_RTL_IF_SET (op) == pc_rtx)
    {
      size_t *v = decl_to_stack_part->get (op);
      if (v && bitmap_set_bit (active, *v))
	{
	  size_t num = *v;
	  bitmap_iterator bi;
	  unsigned i;
	  gcc_assert (num < stack_vars_num);
	  EXECUTE_IF_SET_IN_BITMAP (active, 0, i, bi)
	    add_stack_var_conflict (num, i);
	}
    }
  return false;
}

/* Helper routine for add_scope_conflicts, calculating the active partitions
   at the end of BB, leaving the result in WORK.  We're called to generate
   conflicts when FOR_CONFLICT is true, otherwise we're just tracking
   liveness.  */

static void
add_scope_conflicts_1 (basic_block bb, bitmap work, bool for_conflict)
{
  edge e;
  edge_iterator ei;
  gimple_stmt_iterator gsi;
  walk_stmt_load_store_addr_fn visit;

  bitmap_clear (work);
  FOR_EACH_EDGE (e, ei, bb->preds)
    bitmap_ior_into (work, (bitmap)e->src->aux);

  visit = visit_op;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      walk_stmt_load_store_addr_ops (stmt, work, NULL, NULL, visit);
    }
  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (gimple_clobber_p (stmt))
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  size_t *v;
	  /* Nested function lowering might introduce LHSs
	     that are COMPONENT_REFs.  */
	  if (TREE_CODE (lhs) != VAR_DECL)
	    continue;
	  if (DECL_RTL_IF_SET (lhs) == pc_rtx
	      && (v = decl_to_stack_part->get (lhs)))
	    bitmap_clear_bit (work, *v);
	}
      else if (!is_gimple_debug (stmt))
	{
	  if (for_conflict
	      && visit == visit_op)
	    {
	      /* If this is the first real instruction in this BB we need
	         to add conflicts for everything live at this point now.
		 Unlike classical liveness for named objects we can't
		 rely on seeing a def/use of the names we're interested in.
		 There might merely be indirect loads/stores.  We'd not add any
		 conflicts for such partitions.  */
	      bitmap_iterator bi;
	      unsigned i;
	      EXECUTE_IF_SET_IN_BITMAP (work, 0, i, bi)
		{
		  struct stack_var *a = &stack_vars[i];
		  if (!a->conflicts)
		    a->conflicts = BITMAP_ALLOC (&stack_var_bitmap_obstack);
		  bitmap_ior_into (a->conflicts, work);
		}
	      visit = visit_conflict;
	    }
	  walk_stmt_load_store_addr_ops (stmt, work, visit, visit, visit);
	}
    }
}

/* Generate stack partition conflicts between all partitions that are
   simultaneously live.  */

static void
add_scope_conflicts (void)
{
  basic_block bb;
  bool changed;
  bitmap work = BITMAP_ALLOC (NULL);
  int *rpo;
  int n_bbs;

  /* We approximate the live range of a stack variable by taking the first
     mention of its name as starting point(s), and by the end-of-scope
     death clobber added by gimplify as ending point(s) of the range.
     This overapproximates in the case we for instance moved an address-taken
     operation upward, without also moving a dereference to it upwards.
     But it's conservatively correct as a variable never can hold values
     before its name is mentioned at least once.

     We then do a mostly classical bitmap liveness algorithm.  */

  FOR_ALL_BB_FN (bb, cfun)
    bb->aux = BITMAP_ALLOC (&stack_var_bitmap_obstack);

  rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  n_bbs = pre_and_rev_post_order_compute (NULL, rpo, false);

  changed = true;
  while (changed)
    {
      int i;
      changed = false;
      for (i = 0; i < n_bbs; i++)
	{
	  bitmap active;
	  bb = BASIC_BLOCK_FOR_FN (cfun, rpo[i]);
	  active = (bitmap)bb->aux;
	  add_scope_conflicts_1 (bb, work, false);
	  if (bitmap_ior_into (active, work))
	    changed = true;
	}
    }

  FOR_EACH_BB_FN (bb, cfun)
    add_scope_conflicts_1 (bb, work, true);

  free (rpo);
  BITMAP_FREE (work);
  FOR_ALL_BB_FN (bb, cfun)
    BITMAP_FREE (bb->aux);
}

/* A subroutine of partition_stack_vars.  A comparison function for qsort,
   sorting an array of indices by the properties of the object.  */

static int
stack_var_cmp (const void *a, const void *b)
{
  size_t ia = *(const size_t *)a;
  size_t ib = *(const size_t *)b;
  unsigned int aligna = stack_vars[ia].alignb;
  unsigned int alignb = stack_vars[ib].alignb;
  HOST_WIDE_INT sizea = stack_vars[ia].size;
  HOST_WIDE_INT sizeb = stack_vars[ib].size;
  tree decla = stack_vars[ia].decl;
  tree declb = stack_vars[ib].decl;
  bool largea, largeb;
  unsigned int uida, uidb;

  /* Primary compare on "large" alignment.  Large comes first.  */
  largea = (aligna * BITS_PER_UNIT > MAX_SUPPORTED_STACK_ALIGNMENT);
  largeb = (alignb * BITS_PER_UNIT > MAX_SUPPORTED_STACK_ALIGNMENT);
  if (largea != largeb)
    return (int)largeb - (int)largea;

  /* Secondary compare on size, decreasing  */
  if (sizea > sizeb)
    return -1;
  if (sizea < sizeb)
    return 1;

  /* Tertiary compare on true alignment, decreasing.  */
  if (aligna < alignb)
    return -1;
  if (aligna > alignb)
    return 1;

  /* Final compare on ID for sort stability, increasing.
     Two SSA names are compared by their version, SSA names come before
     non-SSA names, and two normal decls are compared by their DECL_UID.  */
  if (TREE_CODE (decla) == SSA_NAME)
    {
      if (TREE_CODE (declb) == SSA_NAME)
	uida = SSA_NAME_VERSION (decla), uidb = SSA_NAME_VERSION (declb);
      else
	return -1;
    }
  else if (TREE_CODE (declb) == SSA_NAME)
    return 1;
  else
    uida = DECL_UID (decla), uidb = DECL_UID (declb);
  if (uida < uidb)
    return 1;
  if (uida > uidb)
    return -1;
  return 0;
}

struct part_traits : unbounded_int_hashmap_traits <size_t, bitmap> {};
typedef hash_map<size_t, bitmap, part_traits> part_hashmap;

/* If the points-to solution *PI points to variables that are in a partition
   together with other variables add all partition members to the pointed-to
   variables bitmap.  */

static void
add_partitioned_vars_to_ptset (struct pt_solution *pt,
			       part_hashmap *decls_to_partitions,
			       hash_set<bitmap> *visited, bitmap temp)
{
  bitmap_iterator bi;
  unsigned i;
  bitmap *part;

  if (pt->anything
      || pt->vars == NULL
      /* The pointed-to vars bitmap is shared, it is enough to
	 visit it once.  */
      || visited->add (pt->vars))
    return;

  bitmap_clear (temp);

  /* By using a temporary bitmap to store all members of the partitions
     we have to add we make sure to visit each of the partitions only
     once.  */
  EXECUTE_IF_SET_IN_BITMAP (pt->vars, 0, i, bi)
    if ((!temp
	 || !bitmap_bit_p (temp, i))
	&& (part = decls_to_partitions->get (i)))
      bitmap_ior_into (temp, *part);
  if (!bitmap_empty_p (temp))
    bitmap_ior_into (pt->vars, temp);
}

/* Update points-to sets based on partition info, so we can use them on RTL.
   The bitmaps representing stack partitions will be saved until expand,
   where partitioned decls used as bases in memory expressions will be
   rewritten.  */

static void
update_alias_info_with_stack_vars (void)
{
  part_hashmap *decls_to_partitions = NULL;
  size_t i, j;
  tree var = NULL_TREE;

  for (i = 0; i < stack_vars_num; i++)
    {
      bitmap part = NULL;
      tree name;
      struct ptr_info_def *pi;

      /* Not interested in partitions with single variable.  */
      if (stack_vars[i].representative != i
          || stack_vars[i].next == EOC)
        continue;

      if (!decls_to_partitions)
	{
	  decls_to_partitions = new part_hashmap;
	  cfun->gimple_df->decls_to_pointers = new hash_map<tree, tree>;
	}

      /* Create an SSA_NAME that points to the partition for use
         as base during alias-oracle queries on RTL for bases that
	 have been partitioned.  */
      if (var == NULL_TREE)
	var = create_tmp_var (ptr_type_node);
      name = make_ssa_name (var);

      /* Create bitmaps representing partitions.  They will be used for
         points-to sets later, so use GGC alloc.  */
      part = BITMAP_GGC_ALLOC ();
      for (j = i; j != EOC; j = stack_vars[j].next)
	{
	  tree decl = stack_vars[j].decl;
	  unsigned int uid = DECL_PT_UID (decl);
	  bitmap_set_bit (part, uid);
	  decls_to_partitions->put (uid, part);
	  cfun->gimple_df->decls_to_pointers->put (decl, name);
	  if (TREE_ADDRESSABLE (decl))
	    TREE_ADDRESSABLE (name) = 1;
	}

      /* Make the SSA name point to all partition members.  */
      pi = get_ptr_info (name);
      pt_solution_set (&pi->pt, part, false);
    }

  /* Make all points-to sets that contain one member of a partition
     contain all members of the partition.  */
  if (decls_to_partitions)
    {
      unsigned i;
      hash_set<bitmap> visited;
      bitmap temp = BITMAP_ALLOC (&stack_var_bitmap_obstack);

      for (i = 1; i < num_ssa_names; i++)
	{
	  tree name = ssa_name (i);
	  struct ptr_info_def *pi;

	  if (name
	      && POINTER_TYPE_P (TREE_TYPE (name))
	      && ((pi = SSA_NAME_PTR_INFO (name)) != NULL))
	    add_partitioned_vars_to_ptset (&pi->pt, decls_to_partitions,
					   &visited, temp);
	}

      add_partitioned_vars_to_ptset (&cfun->gimple_df->escaped,
				     decls_to_partitions, &visited, temp);

      delete decls_to_partitions;
      BITMAP_FREE (temp);
    }
}

/* A subroutine of partition_stack_vars.  The UNION portion of a UNION/FIND
   partitioning algorithm.  Partitions A and B are known to be non-conflicting.
   Merge them into a single partition A.  */

static void
union_stack_vars (size_t a, size_t b)
{
  struct stack_var *vb = &stack_vars[b];
  bitmap_iterator bi;
  unsigned u;

  gcc_assert (stack_vars[b].next == EOC);
   /* Add B to A's partition.  */
  stack_vars[b].next = stack_vars[a].next;
  stack_vars[b].representative = a;
  stack_vars[a].next = b;

  /* Update the required alignment of partition A to account for B.  */
  if (stack_vars[a].alignb < stack_vars[b].alignb)
    stack_vars[a].alignb = stack_vars[b].alignb;

  /* Update the interference graph and merge the conflicts.  */
  if (vb->conflicts)
    {
      EXECUTE_IF_SET_IN_BITMAP (vb->conflicts, 0, u, bi)
	add_stack_var_conflict (a, stack_vars[u].representative);
      BITMAP_FREE (vb->conflicts);
    }
}

/* A subroutine of expand_used_vars.  Binpack the variables into
   partitions constrained by the interference graph.  The overall
   algorithm used is as follows:

	Sort the objects by size in descending order.
	For each object A {
	  S = size(A)
	  O = 0
	  loop {
	    Look for the largest non-conflicting object B with size <= S.
	    UNION (A, B)
	  }
	}
*/

static void
partition_stack_vars (void)
{
  size_t si, sj, n = stack_vars_num;

  stack_vars_sorted = XNEWVEC (size_t, stack_vars_num);
  for (si = 0; si < n; ++si)
    stack_vars_sorted[si] = si;

  if (n == 1)
    return;

  qsort (stack_vars_sorted, n, sizeof (size_t), stack_var_cmp);

  for (si = 0; si < n; ++si)
    {
      size_t i = stack_vars_sorted[si];
      unsigned int ialign = stack_vars[i].alignb;
      HOST_WIDE_INT isize = stack_vars[i].size;

      /* Ignore objects that aren't partition representatives. If we
         see a var that is not a partition representative, it must
         have been merged earlier.  */
      if (stack_vars[i].representative != i)
        continue;

      for (sj = si + 1; sj < n; ++sj)
	{
	  size_t j = stack_vars_sorted[sj];
	  unsigned int jalign = stack_vars[j].alignb;
	  HOST_WIDE_INT jsize = stack_vars[j].size;

	  /* Ignore objects that aren't partition representatives.  */
	  if (stack_vars[j].representative != j)
	    continue;

	  /* Do not mix objects of "small" (supported) alignment
	     and "large" (unsupported) alignment.  */
	  if ((ialign * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT)
	      != (jalign * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT))
	    break;

	  /* For Address Sanitizer do not mix objects with different
	     sizes, as the shorter vars wouldn't be adequately protected.
	     Don't do that for "large" (unsupported) alignment objects,
	     those aren't protected anyway.  */
	  if ((flag_sanitize & SANITIZE_ADDRESS) && ASAN_STACK && isize != jsize
	      && ialign * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT)
	    break;

	  /* Ignore conflicting objects.  */
	  if (stack_var_conflict_p (i, j))
	    continue;

	  /* UNION the objects, placing J at OFFSET.  */
	  union_stack_vars (i, j);
	}
    }

  update_alias_info_with_stack_vars ();
}

/* A debugging aid for expand_used_vars.  Dump the generated partitions.  */

static void
dump_stack_var_partition (void)
{
  size_t si, i, j, n = stack_vars_num;

  for (si = 0; si < n; ++si)
    {
      i = stack_vars_sorted[si];

      /* Skip variables that aren't partition representatives, for now.  */
      if (stack_vars[i].representative != i)
	continue;

      fprintf (dump_file, "Partition %lu: size " HOST_WIDE_INT_PRINT_DEC
	       " align %u\n", (unsigned long) i, stack_vars[i].size,
	       stack_vars[i].alignb);

      for (j = i; j != EOC; j = stack_vars[j].next)
	{
	  fputc ('\t', dump_file);
	  print_generic_expr (dump_file, stack_vars[j].decl, dump_flags);
	}
      fputc ('\n', dump_file);
    }
}

/* Assign rtl to DECL at BASE + OFFSET.  */

static void
expand_one_stack_var_at (tree decl, rtx base, unsigned base_align,
			 HOST_WIDE_INT offset)
{
  unsigned align;
  rtx x;

  /* If this fails, we've overflowed the stack frame.  Error nicely?  */
  gcc_assert (offset == trunc_int_for_mode (offset, Pmode));

  x = plus_constant (Pmode, base, offset);
  x = gen_rtx_MEM (DECL_MODE (SSAVAR (decl)), x);

  if (TREE_CODE (decl) != SSA_NAME)
    {
      /* Set alignment we actually gave this decl if it isn't an SSA name.
         If it is we generate stack slots only accidentally so it isn't as
	 important, we'll simply use the alignment that is already set.  */
      if (base == virtual_stack_vars_rtx)
	offset -= frame_phase;
      align = offset & -offset;
      align *= BITS_PER_UNIT;
      if (align == 0 || align > base_align)
	align = base_align;

      /* One would think that we could assert that we're not decreasing
	 alignment here, but (at least) the i386 port does exactly this
	 via the MINIMUM_ALIGNMENT hook.  */

      DECL_ALIGN (decl) = align;
      DECL_USER_ALIGN (decl) = 0;
    }

  set_mem_attributes (x, SSAVAR (decl), true);
  set_rtl (decl, x);
}

struct stack_vars_data
{
  /* Vector of offset pairs, always end of some padding followed
     by start of the padding that needs Address Sanitizer protection.
     The vector is in reversed, highest offset pairs come first.  */
  vec<HOST_WIDE_INT> asan_vec;

  /* Vector of partition representative decls in between the paddings.  */
  vec<tree> asan_decl_vec;

  /* Base pseudo register for Address Sanitizer protected automatic vars.  */
  rtx asan_base;

  /* Alignment needed for the Address Sanitizer protected automatic vars.  */
  unsigned int asan_alignb;
};

/* A subroutine of expand_used_vars.  Give each partition representative
   a unique location within the stack frame.  Update each partition member
   with that location.  */

static void
expand_stack_vars (bool (*pred) (size_t), struct stack_vars_data *data)
{
  size_t si, i, j, n = stack_vars_num;
  HOST_WIDE_INT large_size = 0, large_alloc = 0;
  rtx large_base = NULL;
  unsigned large_align = 0;
  tree decl;

  /* Determine if there are any variables requiring "large" alignment.
     Since these are dynamically allocated, we only process these if
     no predicate involved.  */
  large_align = stack_vars[stack_vars_sorted[0]].alignb * BITS_PER_UNIT;
  if (pred == NULL && large_align > MAX_SUPPORTED_STACK_ALIGNMENT)
    {
      /* Find the total size of these variables.  */
      for (si = 0; si < n; ++si)
	{
	  unsigned alignb;

	  i = stack_vars_sorted[si];
	  alignb = stack_vars[i].alignb;

	  /* All "large" alignment decls come before all "small" alignment
	     decls, but "large" alignment decls are not sorted based on
	     their alignment.  Increase large_align to track the largest
	     required alignment.  */
	  if ((alignb * BITS_PER_UNIT) > large_align)
	    large_align = alignb * BITS_PER_UNIT;

	  /* Stop when we get to the first decl with "small" alignment.  */
	  if (alignb * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT)
	    break;

	  /* Skip variables that aren't partition representatives.  */
	  if (stack_vars[i].representative != i)
	    continue;

	  /* Skip variables that have already had rtl assigned.  See also
	     add_stack_var where we perpetrate this pc_rtx hack.  */
	  decl = stack_vars[i].decl;
	  if ((TREE_CODE (decl) == SSA_NAME
	      ? SA.partition_to_pseudo[var_to_partition (SA.map, decl)]
	      : DECL_RTL (decl)) != pc_rtx)
	    continue;

	  large_size += alignb - 1;
	  large_size &= -(HOST_WIDE_INT)alignb;
	  large_size += stack_vars[i].size;
	}

      /* If there were any, allocate space.  */
      if (large_size > 0)
	large_base = allocate_dynamic_stack_space (GEN_INT (large_size), 0,
						   large_align, true);
    }

  for (si = 0; si < n; ++si)
    {
      rtx base;
      unsigned base_align, alignb;
      HOST_WIDE_INT offset;

      i = stack_vars_sorted[si];

      /* Skip variables that aren't partition representatives, for now.  */
      if (stack_vars[i].representative != i)
	continue;

      /* Skip variables that have already had rtl assigned.  See also
	 add_stack_var where we perpetrate this pc_rtx hack.  */
      decl = stack_vars[i].decl;
      if ((TREE_CODE (decl) == SSA_NAME
	   ? SA.partition_to_pseudo[var_to_partition (SA.map, decl)]
	   : DECL_RTL (decl)) != pc_rtx)
	continue;

      /* Check the predicate to see whether this variable should be
	 allocated in this pass.  */
      if (pred && !pred (i))
	continue;

      alignb = stack_vars[i].alignb;
      if (alignb * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT)
	{
	  base = virtual_stack_vars_rtx;
	  if ((flag_sanitize & SANITIZE_ADDRESS) && ASAN_STACK && pred)
	    {
	      HOST_WIDE_INT prev_offset
		= align_base (frame_offset,
			      MAX (alignb, ASAN_RED_ZONE_SIZE),
			      FRAME_GROWS_DOWNWARD);
	      tree repr_decl = NULL_TREE;
	      offset
		= alloc_stack_frame_space (stack_vars[i].size
					   + ASAN_RED_ZONE_SIZE,
					   MAX (alignb, ASAN_RED_ZONE_SIZE));

	      data->asan_vec.safe_push (prev_offset);
	      data->asan_vec.safe_push (offset + stack_vars[i].size);
	      /* Find best representative of the partition.
		 Prefer those with DECL_NAME, even better
		 satisfying asan_protect_stack_decl predicate.  */
	      for (j = i; j != EOC; j = stack_vars[j].next)
		if (asan_protect_stack_decl (stack_vars[j].decl)
		    && DECL_NAME (stack_vars[j].decl))
		  {
		    repr_decl = stack_vars[j].decl;
		    break;
		  }
		else if (repr_decl == NULL_TREE
			 && DECL_P (stack_vars[j].decl)
			 && DECL_NAME (stack_vars[j].decl))
		  repr_decl = stack_vars[j].decl;
	      if (repr_decl == NULL_TREE)
		repr_decl = stack_vars[i].decl;
	      data->asan_decl_vec.safe_push (repr_decl);
	      data->asan_alignb = MAX (data->asan_alignb, alignb);
	      if (data->asan_base == NULL)
		data->asan_base = gen_reg_rtx (Pmode);
	      base = data->asan_base;

	      if (!STRICT_ALIGNMENT)
		base_align = crtl->max_used_stack_slot_alignment;
	      else
		base_align = MAX (crtl->max_used_stack_slot_alignment,
				  GET_MODE_ALIGNMENT (SImode)
				  << ASAN_SHADOW_SHIFT);
	    }
	  else
	    {
	      offset = alloc_stack_frame_space (stack_vars[i].size, alignb);
	      base_align = crtl->max_used_stack_slot_alignment;
	    }
	}
      else
	{
	  /* Large alignment is only processed in the last pass.  */
	  if (pred)
	    continue;
	  gcc_assert (large_base != NULL);

	  large_alloc += alignb - 1;
	  large_alloc &= -(HOST_WIDE_INT)alignb;
	  offset = large_alloc;
	  large_alloc += stack_vars[i].size;

	  base = large_base;
	  base_align = large_align;
	}

      /* Create rtl for each variable based on their location within the
	 partition.  */
      for (j = i; j != EOC; j = stack_vars[j].next)
	{
	  expand_one_stack_var_at (stack_vars[j].decl,
				   base, base_align,
				   offset);
	}
    }

  gcc_assert (large_alloc == large_size);
}

/* Take into account all sizes of partitions and reset DECL_RTLs.  */
static HOST_WIDE_INT
account_stack_vars (void)
{
  size_t si, j, i, n = stack_vars_num;
  HOST_WIDE_INT size = 0;

  for (si = 0; si < n; ++si)
    {
      i = stack_vars_sorted[si];

      /* Skip variables that aren't partition representatives, for now.  */
      if (stack_vars[i].representative != i)
	continue;

      size += stack_vars[i].size;
      for (j = i; j != EOC; j = stack_vars[j].next)
	set_rtl (stack_vars[j].decl, NULL);
    }
  return size;
}

/* A subroutine of expand_one_var.  Called to immediately assign rtl
   to a variable to be allocated in the stack frame.  */

static void
expand_one_stack_var (tree var)
{
  HOST_WIDE_INT size, offset;
  unsigned byte_align;

  size = tree_to_uhwi (DECL_SIZE_UNIT (SSAVAR (var)));
  byte_align = align_local_variable (SSAVAR (var));

  /* We handle highly aligned variables in expand_stack_vars.  */
  gcc_assert (byte_align * BITS_PER_UNIT <= MAX_SUPPORTED_STACK_ALIGNMENT);

  offset = alloc_stack_frame_space (size, byte_align);

  expand_one_stack_var_at (var, virtual_stack_vars_rtx,
			   crtl->max_used_stack_slot_alignment, offset);
}

/* A subroutine of expand_one_var.  Called to assign rtl to a VAR_DECL
   that will reside in a hard register.  */

static void
expand_one_hard_reg_var (tree var)
{
  rest_of_decl_compilation (var, 0, 0);
}

/* A subroutine of expand_one_var.  Called to assign rtl to a VAR_DECL
   that will reside in a pseudo register.  */

static void
expand_one_register_var (tree var)
{
  tree decl = SSAVAR (var);
  tree type = TREE_TYPE (decl);
  machine_mode reg_mode = promote_decl_mode (decl, NULL);
  rtx x = gen_reg_rtx (reg_mode);

  set_rtl (var, x);

  /* Note if the object is a user variable.  */
  if (!DECL_ARTIFICIAL (decl))
    mark_user_reg (x);

  if (POINTER_TYPE_P (type))
    mark_reg_pointer (x, get_pointer_alignment (var));
}

/* A subroutine of expand_one_var.  Called to assign rtl to a VAR_DECL that
   has some associated error, e.g. its type is error-mark.  We just need
   to pick something that won't crash the rest of the compiler.  */

static void
expand_one_error_var (tree var)
{
  machine_mode mode = DECL_MODE (var);
  rtx x;

  if (mode == BLKmode)
    x = gen_rtx_MEM (BLKmode, const0_rtx);
  else if (mode == VOIDmode)
    x = const0_rtx;
  else
    x = gen_reg_rtx (mode);

  SET_DECL_RTL (var, x);
}

/* A subroutine of expand_one_var.  VAR is a variable that will be
   allocated to the local stack frame.  Return true if we wish to
   add VAR to STACK_VARS so that it will be coalesced with other
   variables.  Return false to allocate VAR immediately.

   This function is used to reduce the number of variables considered
   for coalescing, which reduces the size of the quadratic problem.  */

static bool
defer_stack_allocation (tree var, bool toplevel)
{
  /* Whether the variable is small enough for immediate allocation not to be
     a problem with regard to the frame size.  */
  bool smallish
    = ((HOST_WIDE_INT) tree_to_uhwi (DECL_SIZE_UNIT (var))
       < PARAM_VALUE (PARAM_MIN_SIZE_FOR_STACK_SHARING));

  /* If stack protection is enabled, *all* stack variables must be deferred,
     so that we can re-order the strings to the top of the frame.
     Similarly for Address Sanitizer.  */
  if (flag_stack_protect || ((flag_sanitize & SANITIZE_ADDRESS) && ASAN_STACK))
    return true;

  /* We handle "large" alignment via dynamic allocation.  We want to handle
     this extra complication in only one place, so defer them.  */
  if (DECL_ALIGN (var) > MAX_SUPPORTED_STACK_ALIGNMENT)
    return true;

  /* When optimization is enabled, DECL_IGNORED_P variables originally scoped
     might be detached from their block and appear at toplevel when we reach
     here.  We want to coalesce them with variables from other blocks when
     the immediate contribution to the frame size would be noticeable.  */
  if (toplevel && optimize > 0 && DECL_IGNORED_P (var) && !smallish)
    return true;

  /* Variables declared in the outermost scope automatically conflict
     with every other variable.  The only reason to want to defer them
     at all is that, after sorting, we can more efficiently pack
     small variables in the stack frame.  Continue to defer at -O2.  */
  if (toplevel && optimize < 2)
    return false;

  /* Without optimization, *most* variables are allocated from the
     stack, which makes the quadratic problem large exactly when we
     want compilation to proceed as quickly as possible.  On the
     other hand, we don't want the function's stack frame size to
     get completely out of hand.  So we avoid adding scalars and
     "small" aggregates to the list at all.  */
  if (optimize == 0 && smallish)
    return false;

  return true;
}

/* A subroutine of expand_used_vars.  Expand one variable according to
   its flavor.  Variables to be placed on the stack are not actually
   expanded yet, merely recorded.
   When REALLY_EXPAND is false, only add stack values to be allocated.
   Return stack usage this variable is supposed to take.
*/

static HOST_WIDE_INT
expand_one_var (tree var, bool toplevel, bool really_expand)
{
  unsigned int align = BITS_PER_UNIT;
  tree origvar = var;

  var = SSAVAR (var);

  if (TREE_TYPE (var) != error_mark_node && TREE_CODE (var) == VAR_DECL)
    {
      /* Because we don't know if VAR will be in register or on stack,
	 we conservatively assume it will be on stack even if VAR is
	 eventually put into register after RA pass.  For non-automatic
	 variables, which won't be on stack, we collect alignment of
	 type and ignore user specified alignment.  Similarly for
	 SSA_NAMEs for which use_register_for_decl returns true.  */
      if (TREE_STATIC (var)
	  || DECL_EXTERNAL (var)
	  || (TREE_CODE (origvar) == SSA_NAME && use_register_for_decl (var)))
	align = MINIMUM_ALIGNMENT (TREE_TYPE (var),
				   TYPE_MODE (TREE_TYPE (var)),
				   TYPE_ALIGN (TREE_TYPE (var)));
      else if (DECL_HAS_VALUE_EXPR_P (var)
	       || (DECL_RTL_SET_P (var) && MEM_P (DECL_RTL (var))))
	/* Don't consider debug only variables with DECL_HAS_VALUE_EXPR_P set
	   or variables which were assigned a stack slot already by
	   expand_one_stack_var_at - in the latter case DECL_ALIGN has been
	   changed from the offset chosen to it.  */
	align = crtl->stack_alignment_estimated;
      else
	align = MINIMUM_ALIGNMENT (var, DECL_MODE (var), DECL_ALIGN (var));

      /* If the variable alignment is very large we'll dynamicaly allocate
	 it, which means that in-frame portion is just a pointer.  */
      if (align > MAX_SUPPORTED_STACK_ALIGNMENT)
	align = POINTER_SIZE;
    }

  if (SUPPORTS_STACK_ALIGNMENT
      && crtl->stack_alignment_estimated < align)
    {
      /* stack_alignment_estimated shouldn't change after stack
         realign decision made */
      gcc_assert (!crtl->stack_realign_processed);
      crtl->stack_alignment_estimated = align;
    }

  /* stack_alignment_needed > PREFERRED_STACK_BOUNDARY is permitted.
     So here we only make sure stack_alignment_needed >= align.  */
  if (crtl->stack_alignment_needed < align)
    crtl->stack_alignment_needed = align;
  if (crtl->max_used_stack_slot_alignment < align)
    crtl->max_used_stack_slot_alignment = align;

  if (TREE_CODE (origvar) == SSA_NAME)
    {
      gcc_assert (TREE_CODE (var) != VAR_DECL
		  || (!DECL_EXTERNAL (var)
		      && !DECL_HAS_VALUE_EXPR_P (var)
		      && !TREE_STATIC (var)
		      && TREE_TYPE (var) != error_mark_node
		      && !DECL_HARD_REGISTER (var)
		      && really_expand));
    }
  if (TREE_CODE (var) != VAR_DECL && TREE_CODE (origvar) != SSA_NAME)
    ;
  else if (DECL_EXTERNAL (var))
    ;
  else if (DECL_HAS_VALUE_EXPR_P (var))
    ;
  else if (TREE_STATIC (var))
    ;
  else if (TREE_CODE (origvar) != SSA_NAME && DECL_RTL_SET_P (var))
    ;
  else if (TREE_TYPE (var) == error_mark_node)
    {
      if (really_expand)
        expand_one_error_var (var);
    }
  else if (TREE_CODE (var) == VAR_DECL && DECL_HARD_REGISTER (var))
    {
      if (really_expand)
	{
	  expand_one_hard_reg_var (var);
	  if (!DECL_HARD_REGISTER (var))
	    /* Invalid register specification.  */
	    expand_one_error_var (var);
	}
    }
  else if (use_register_for_decl (var))
    {
      if (really_expand)
        expand_one_register_var (origvar);
    }
  else if (! valid_constant_size_p (DECL_SIZE_UNIT (var)))
    {
      /* Reject variables which cover more than half of the address-space.  */
      if (really_expand)
	{
	  error ("size of variable %q+D is too large", var);
	  expand_one_error_var (var);
	}
    }
  else if (defer_stack_allocation (var, toplevel))
    add_stack_var (origvar);
  else
    {
      if (really_expand)
        expand_one_stack_var (origvar);
      return tree_to_uhwi (DECL_SIZE_UNIT (var));
    }
  return 0;
}

/* A subroutine of expand_used_vars.  Walk down through the BLOCK tree
   expanding variables.  Those variables that can be put into registers
   are allocated pseudos; those that can't are put on the stack.

   TOPLEVEL is true if this is the outermost BLOCK.  */

static void
expand_used_vars_for_block (tree block, bool toplevel)
{
  tree t;

  /* Expand all variables at this level.  */
  for (t = BLOCK_VARS (block); t ; t = DECL_CHAIN (t))
    if (TREE_USED (t)
        && ((TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != RESULT_DECL)
	    || !DECL_NONSHAREABLE (t)))
      expand_one_var (t, toplevel, true);

  /* Expand all variables at containing levels.  */
  for (t = BLOCK_SUBBLOCKS (block); t ; t = BLOCK_CHAIN (t))
    expand_used_vars_for_block (t, false);
}

/* A subroutine of expand_used_vars.  Walk down through the BLOCK tree
   and clear TREE_USED on all local variables.  */

static void
clear_tree_used (tree block)
{
  tree t;

  for (t = BLOCK_VARS (block); t ; t = DECL_CHAIN (t))
    /* if (!TREE_STATIC (t) && !DECL_EXTERNAL (t)) */
    if ((TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != RESULT_DECL)
	|| !DECL_NONSHAREABLE (t))
      TREE_USED (t) = 0;

  for (t = BLOCK_SUBBLOCKS (block); t ; t = BLOCK_CHAIN (t))
    clear_tree_used (t);
}

enum {
  SPCT_FLAG_DEFAULT = 1,
  SPCT_FLAG_ALL = 2,
  SPCT_FLAG_STRONG = 3,
  SPCT_FLAG_EXPLICIT = 4
};

/* Examine TYPE and determine a bit mask of the following features.  */

#define SPCT_HAS_LARGE_CHAR_ARRAY	1
#define SPCT_HAS_SMALL_CHAR_ARRAY	2
#define SPCT_HAS_ARRAY			4
#define SPCT_HAS_AGGREGATE		8

static unsigned int
stack_protect_classify_type (tree type)
{
  unsigned int ret = 0;
  tree t;

  switch (TREE_CODE (type))
    {
    case ARRAY_TYPE:
      t = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if (t == char_type_node
	  || t == signed_char_type_node
	  || t == unsigned_char_type_node)
	{
	  unsigned HOST_WIDE_INT max = PARAM_VALUE (PARAM_SSP_BUFFER_SIZE);
	  unsigned HOST_WIDE_INT len;

	  if (!TYPE_SIZE_UNIT (type)
	      || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
	    len = max;
	  else
	    len = tree_to_uhwi (TYPE_SIZE_UNIT (type));

	  if (len < max)
	    ret = SPCT_HAS_SMALL_CHAR_ARRAY | SPCT_HAS_ARRAY;
	  else
	    ret = SPCT_HAS_LARGE_CHAR_ARRAY | SPCT_HAS_ARRAY;
	}
      else
	ret = SPCT_HAS_ARRAY;
      break;

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case RECORD_TYPE:
      ret = SPCT_HAS_AGGREGATE;
      for (t = TYPE_FIELDS (type); t ; t = TREE_CHAIN (t))
	if (TREE_CODE (t) == FIELD_DECL)
	  ret |= stack_protect_classify_type (TREE_TYPE (t));
      break;

    default:
      break;
    }

  return ret;
}

/* Return nonzero if DECL should be segregated into the "vulnerable" upper
   part of the local stack frame.  Remember if we ever return nonzero for
   any variable in this function.  The return value is the phase number in
   which the variable should be allocated.  */

static int
stack_protect_decl_phase (tree decl)
{
  unsigned int bits = stack_protect_classify_type (TREE_TYPE (decl));
  int ret = 0;

  if (bits & SPCT_HAS_SMALL_CHAR_ARRAY)
    has_short_buffer = true;

  if (flag_stack_protect == SPCT_FLAG_ALL
      || flag_stack_protect == SPCT_FLAG_STRONG
      || (flag_stack_protect == SPCT_FLAG_EXPLICIT
	  && lookup_attribute ("stack_protect",
			       DECL_ATTRIBUTES (current_function_decl))))
    {
      if ((bits & (SPCT_HAS_SMALL_CHAR_ARRAY | SPCT_HAS_LARGE_CHAR_ARRAY))
	  && !(bits & SPCT_HAS_AGGREGATE))
	ret = 1;
      else if (bits & SPCT_HAS_ARRAY)
	ret = 2;
    }
  else
    ret = (bits & SPCT_HAS_LARGE_CHAR_ARRAY) != 0;

  if (ret)
    has_protected_decls = true;

  return ret;
}

/* Two helper routines that check for phase 1 and phase 2.  These are used
   as callbacks for expand_stack_vars.  */

static bool
stack_protect_decl_phase_1 (size_t i)
{
  return stack_protect_decl_phase (stack_vars[i].decl) == 1;
}

static bool
stack_protect_decl_phase_2 (size_t i)
{
  return stack_protect_decl_phase (stack_vars[i].decl) == 2;
}

/* And helper function that checks for asan phase (with stack protector
   it is phase 3).  This is used as callback for expand_stack_vars.
   Returns true if any of the vars in the partition need to be protected.  */

static bool
asan_decl_phase_3 (size_t i)
{
  while (i != EOC)
    {
      if (asan_protect_stack_decl (stack_vars[i].decl))
	return true;
      i = stack_vars[i].next;
    }
  return false;
}

/* Ensure that variables in different stack protection phases conflict
   so that they are not merged and share the same stack slot.  */

static void
add_stack_protection_conflicts (void)
{
  size_t i, j, n = stack_vars_num;
  unsigned char *phase;

  phase = XNEWVEC (unsigned char, n);
  for (i = 0; i < n; ++i)
    phase[i] = stack_protect_decl_phase (stack_vars[i].decl);

  for (i = 0; i < n; ++i)
    {
      unsigned char ph_i = phase[i];
      for (j = i + 1; j < n; ++j)
	if (ph_i != phase[j])
	  add_stack_var_conflict (i, j);
    }

  XDELETEVEC (phase);
}

/* Create a decl for the guard at the top of the stack frame.  */

static void
create_stack_guard (void)
{
  tree guard = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
			   VAR_DECL, NULL, ptr_type_node);
  TREE_THIS_VOLATILE (guard) = 1;
  TREE_USED (guard) = 1;
  expand_one_stack_var (guard);
  crtl->stack_protect_guard = guard;
}

/* Prepare for expanding variables.  */
static void
init_vars_expansion (void)
{
  /* Conflict bitmaps, and a few related temporary bitmaps, go here.  */
  bitmap_obstack_initialize (&stack_var_bitmap_obstack);

  /* A map from decl to stack partition.  */
  decl_to_stack_part = new hash_map<tree, size_t>;

  /* Initialize local stack smashing state.  */
  has_protected_decls = false;
  has_short_buffer = false;
}

/* Free up stack variable graph data.  */
static void
fini_vars_expansion (void)
{
  bitmap_obstack_release (&stack_var_bitmap_obstack);
  if (stack_vars)
    XDELETEVEC (stack_vars);
  if (stack_vars_sorted)
    XDELETEVEC (stack_vars_sorted);
  stack_vars = NULL;
  stack_vars_sorted = NULL;
  stack_vars_alloc = stack_vars_num = 0;
  delete decl_to_stack_part;
  decl_to_stack_part = NULL;
}

/* Make a fair guess for the size of the stack frame of the function
   in NODE.  This doesn't have to be exact, the result is only used in
   the inline heuristics.  So we don't want to run the full stack var
   packing algorithm (which is quadratic in the number of stack vars).
   Instead, we calculate the total size of all stack vars.  This turns
   out to be a pretty fair estimate -- packing of stack vars doesn't
   happen very often.  */

HOST_WIDE_INT
estimated_stack_frame_size (struct cgraph_node *node)
{
  HOST_WIDE_INT size = 0;
  size_t i;
  tree var;
  struct function *fn = DECL_STRUCT_FUNCTION (node->decl);

  push_cfun (fn);

  init_vars_expansion ();

  FOR_EACH_LOCAL_DECL (fn, i, var)
    if (auto_var_in_fn_p (var, fn->decl))
      size += expand_one_var (var, true, false);

  if (stack_vars_num > 0)
    {
      /* Fake sorting the stack vars for account_stack_vars ().  */
      stack_vars_sorted = XNEWVEC (size_t, stack_vars_num);
      for (i = 0; i < stack_vars_num; ++i)
	stack_vars_sorted[i] = i;
      size += account_stack_vars ();
    }

  fini_vars_expansion ();
  pop_cfun ();
  return size;
}

/* Helper routine to check if a record or union contains an array field. */

static int
record_or_union_type_has_array_p (const_tree tree_type)
{
  tree fields = TYPE_FIELDS (tree_type);
  tree f;

  for (f = fields; f; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	tree field_type = TREE_TYPE (f);
	if (RECORD_OR_UNION_TYPE_P (field_type)
	    && record_or_union_type_has_array_p (field_type))
	  return 1;
	if (TREE_CODE (field_type) == ARRAY_TYPE)
	  return 1;
      }
  return 0;
}

/* Check if the current function has local referenced variables that
   have their addresses taken, contain an array, or are arrays.  */

static bool
stack_protect_decl_p ()
{
  unsigned i;
  tree var;

  FOR_EACH_LOCAL_DECL (cfun, i, var)
    if (!is_global_var (var))
      {
	tree var_type = TREE_TYPE (var);
	if (TREE_CODE (var) == VAR_DECL
	    && (TREE_CODE (var_type) == ARRAY_TYPE
		|| TREE_ADDRESSABLE (var)
		|| (RECORD_OR_UNION_TYPE_P (var_type)
		    && record_or_union_type_has_array_p (var_type))))
	  return true;
      }
  return false;
}

/* Check if the current function has calls that use a return slot.  */

static bool
stack_protect_return_slot_p ()
{
  basic_block bb;
  
  FOR_ALL_BB_FN (bb, cfun)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);
	/* This assumes that calls to internal-only functions never
	   use a return slot.  */
	if (is_gimple_call (stmt)
	    && !gimple_call_internal_p (stmt)
	    && aggregate_value_p (TREE_TYPE (gimple_call_fntype (stmt)),
				  gimple_call_fndecl (stmt)))
	  return true;
      }
  return false;
}

/* Expand all variables used in the function.  */

static rtx_insn *
expand_used_vars (void)
{
  tree var, outer_block = DECL_INITIAL (current_function_decl);
  vec<tree> maybe_local_decls = vNULL;
  rtx_insn *var_end_seq = NULL;
  unsigned i;
  unsigned len;
  bool gen_stack_protect_signal = false;

  /* Compute the phase of the stack frame for this function.  */
  {
    int align = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;
    int off = STARTING_FRAME_OFFSET % align;
    frame_phase = off ? align - off : 0;
  }

  /* Set TREE_USED on all variables in the local_decls.  */
  FOR_EACH_LOCAL_DECL (cfun, i, var)
    TREE_USED (var) = 1;
  /* Clear TREE_USED on all variables associated with a block scope.  */
  clear_tree_used (DECL_INITIAL (current_function_decl));

  init_vars_expansion ();

  if (targetm.use_pseudo_pic_reg ())
    pic_offset_table_rtx = gen_reg_rtx (Pmode);

  hash_map<tree, tree> ssa_name_decls;
  for (i = 0; i < SA.map->num_partitions; i++)
    {
      tree var = partition_to_var (SA.map, i);

      gcc_assert (!virtual_operand_p (var));

      /* Assign decls to each SSA name partition, share decls for partitions
         we could have coalesced (those with the same type).  */
      if (SSA_NAME_VAR (var) == NULL_TREE)
	{
	  tree *slot = &ssa_name_decls.get_or_insert (TREE_TYPE (var));
	  if (!*slot)
	    *slot = create_tmp_reg (TREE_TYPE (var));
	  replace_ssa_name_symbol (var, *slot);
	}

      /* Always allocate space for partitions based on VAR_DECLs.  But for
	 those based on PARM_DECLs or RESULT_DECLs and which matter for the
	 debug info, there is no need to do so if optimization is disabled
	 because all the SSA_NAMEs based on these DECLs have been coalesced
	 into a single partition, which is thus assigned the canonical RTL
	 location of the DECLs.  If in_lto_p, we can't rely on optimize,
	 a function could be compiled with -O1 -flto first and only the
	 link performed at -O0.  */
      if (TREE_CODE (SSA_NAME_VAR (var)) == VAR_DECL)
	expand_one_var (var, true, true);
      else if (DECL_IGNORED_P (SSA_NAME_VAR (var)) || optimize || in_lto_p)
	{
	  /* This is a PARM_DECL or RESULT_DECL.  For those partitions that
	     contain the default def (representing the parm or result itself)
	     we don't do anything here.  But those which don't contain the
	     default def (representing a temporary based on the parm/result)
	     we need to allocate space just like for normal VAR_DECLs.  */
	  if (!bitmap_bit_p (SA.partition_has_default_def, i))
	    {
	      expand_one_var (var, true, true);
	      gcc_assert (SA.partition_to_pseudo[i]);
	    }
	}
    }

  if (flag_stack_protect == SPCT_FLAG_STRONG)
      gen_stack_protect_signal
	= stack_protect_decl_p () || stack_protect_return_slot_p ();

  /* At this point all variables on the local_decls with TREE_USED
     set are not associated with any block scope.  Lay them out.  */

  len = vec_safe_length (cfun->local_decls);
  FOR_EACH_LOCAL_DECL (cfun, i, var)
    {
      bool expand_now = false;

      /* Expanded above already.  */
      if (is_gimple_reg (var))
	{
	  TREE_USED (var) = 0;
	  goto next;
	}
      /* We didn't set a block for static or extern because it's hard
	 to tell the difference between a global variable (re)declared
	 in a local scope, and one that's really declared there to
	 begin with.  And it doesn't really matter much, since we're
	 not giving them stack space.  Expand them now.  */
      else if (TREE_STATIC (var) || DECL_EXTERNAL (var))
	expand_now = true;

      /* Expand variables not associated with any block now.  Those created by
	 the optimizers could be live anywhere in the function.  Those that
	 could possibly have been scoped originally and detached from their
	 block will have their allocation deferred so we coalesce them with
	 others when optimization is enabled.  */
      else if (TREE_USED (var))
	expand_now = true;

      /* Finally, mark all variables on the list as used.  We'll use
	 this in a moment when we expand those associated with scopes.  */
      TREE_USED (var) = 1;

      if (expand_now)
	expand_one_var (var, true, true);

    next:
      if (DECL_ARTIFICIAL (var) && !DECL_IGNORED_P (var))
	{
	  rtx rtl = DECL_RTL_IF_SET (var);

	  /* Keep artificial non-ignored vars in cfun->local_decls
	     chain until instantiate_decls.  */
	  if (rtl && (MEM_P (rtl) || GET_CODE (rtl) == CONCAT))
	    add_local_decl (cfun, var);
	  else if (rtl == NULL_RTX)
	    /* If rtl isn't set yet, which can happen e.g. with
	       -fstack-protector, retry before returning from this
	       function.  */
	    maybe_local_decls.safe_push (var);
	}
    }

  /* We duplicated some of the decls in CFUN->LOCAL_DECLS.

     +-----------------+-----------------+
     | ...processed... | ...duplicates...|
     +-----------------+-----------------+
                       ^
		       +-- LEN points here.

     We just want the duplicates, as those are the artificial
     non-ignored vars that we want to keep until instantiate_decls.
     Move them down and truncate the array.  */
  if (!vec_safe_is_empty (cfun->local_decls))
    cfun->local_decls->block_remove (0, len);

  /* At this point, all variables within the block tree with TREE_USED
     set are actually used by the optimized function.  Lay them out.  */
  expand_used_vars_for_block (outer_block, true);

  if (stack_vars_num > 0)
    {
      add_scope_conflicts ();

      /* If stack protection is enabled, we don't share space between
	 vulnerable data and non-vulnerable data.  */
      if (flag_stack_protect != 0
	  && (flag_stack_protect != SPCT_FLAG_EXPLICIT
	      || (flag_stack_protect == SPCT_FLAG_EXPLICIT
		  && lookup_attribute ("stack_protect",
				       DECL_ATTRIBUTES (current_function_decl)))))
	add_stack_protection_conflicts ();

      /* Now that we have collected all stack variables, and have computed a
	 minimal interference graph, attempt to save some stack space.  */
      partition_stack_vars ();
      if (dump_file)
	dump_stack_var_partition ();
    }

  switch (flag_stack_protect)
    {
    case SPCT_FLAG_ALL:
      create_stack_guard ();
      break;

    case SPCT_FLAG_STRONG:
      if (gen_stack_protect_signal
	  || cfun->calls_alloca || has_protected_decls
	  || lookup_attribute ("stack_protect",
			       DECL_ATTRIBUTES (current_function_decl)))
	create_stack_guard ();
      break;

    case SPCT_FLAG_DEFAULT:
      if (cfun->calls_alloca || has_protected_decls
	  || lookup_attribute ("stack_protect",
			       DECL_ATTRIBUTES (current_function_decl)))
	create_stack_guard ();
      break;

    case SPCT_FLAG_EXPLICIT:
      if (lookup_attribute ("stack_protect",
			    DECL_ATTRIBUTES (current_function_decl)))
	create_stack_guard ();
      break;
    default:
      ;
    }

  /* Assign rtl to each variable based on these partitions.  */
  if (stack_vars_num > 0)
    {
      struct stack_vars_data data;

      data.asan_vec = vNULL;
      data.asan_decl_vec = vNULL;
      data.asan_base = NULL_RTX;
      data.asan_alignb = 0;

      /* Reorder decls to be protected by iterating over the variables
	 array multiple times, and allocating out of each phase in turn.  */
      /* ??? We could probably integrate this into the qsort we did
	 earlier, such that we naturally see these variables first,
	 and thus naturally allocate things in the right order.  */
      if (has_protected_decls)
	{
	  /* Phase 1 contains only character arrays.  */
	  expand_stack_vars (stack_protect_decl_phase_1, &data);

	  /* Phase 2 contains other kinds of arrays.  */
	  if (flag_stack_protect == SPCT_FLAG_ALL
	      || flag_stack_protect == SPCT_FLAG_STRONG
	      || (flag_stack_protect == SPCT_FLAG_EXPLICIT
		  && lookup_attribute ("stack_protect",
				       DECL_ATTRIBUTES (current_function_decl))))
	    expand_stack_vars (stack_protect_decl_phase_2, &data);
	}

      if ((flag_sanitize & SANITIZE_ADDRESS) && ASAN_STACK)
	/* Phase 3, any partitions that need asan protection
	   in addition to phase 1 and 2.  */
	expand_stack_vars (asan_decl_phase_3, &data);

      if (!data.asan_vec.is_empty ())
	{
	  HOST_WIDE_INT prev_offset = frame_offset;
	  HOST_WIDE_INT offset, sz, redzonesz;
	  redzonesz = ASAN_RED_ZONE_SIZE;
	  sz = data.asan_vec[0] - prev_offset;
	  if (data.asan_alignb > ASAN_RED_ZONE_SIZE
	      && data.asan_alignb <= 4096
	      && sz + ASAN_RED_ZONE_SIZE >= (int) data.asan_alignb)
	    redzonesz = ((sz + ASAN_RED_ZONE_SIZE + data.asan_alignb - 1)
			 & ~(data.asan_alignb - HOST_WIDE_INT_1)) - sz;
	  offset
	    = alloc_stack_frame_space (redzonesz, ASAN_RED_ZONE_SIZE);
	  data.asan_vec.safe_push (prev_offset);
	  data.asan_vec.safe_push (offset);
	  /* Leave space for alignment if STRICT_ALIGNMENT.  */
	  if (STRICT_ALIGNMENT)
	    alloc_stack_frame_space ((GET_MODE_ALIGNMENT (SImode)
				      << ASAN_SHADOW_SHIFT)
				     / BITS_PER_UNIT, 1);

	  var_end_seq
	    = asan_emit_stack_protection (virtual_stack_vars_rtx,
					  data.asan_base,
					  data.asan_alignb,
					  data.asan_vec.address (),
					  data.asan_decl_vec.address (),
					  data.asan_vec.length ());
	}

      expand_stack_vars (NULL, &data);

      data.asan_vec.release ();
      data.asan_decl_vec.release ();
    }

  fini_vars_expansion ();

  /* If there were any artificial non-ignored vars without rtl
     found earlier, see if deferred stack allocation hasn't assigned
     rtl to them.  */
  FOR_EACH_VEC_ELT_REVERSE (maybe_local_decls, i, var)
    {
      rtx rtl = DECL_RTL_IF_SET (var);

      /* Keep artificial non-ignored vars in cfun->local_decls
	 chain until instantiate_decls.  */
      if (rtl && (MEM_P (rtl) || GET_CODE (rtl) == CONCAT))
	add_local_decl (cfun, var);
    }
  maybe_local_decls.release ();

  /* If the target requires that FRAME_OFFSET be aligned, do it.  */
  if (STACK_ALIGNMENT_NEEDED)
    {
      HOST_WIDE_INT align = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;
      if (!FRAME_GROWS_DOWNWARD)
	frame_offset += align - 1;
      frame_offset &= -align;
    }

  return var_end_seq;
}


/* If we need to produce a detailed dump, print the tree representation
   for STMT to the dump file.  SINCE is the last RTX after which the RTL
   generated for STMT should have been appended.  */

static void
maybe_dump_rtl_for_gimple_stmt (gimple stmt, rtx_insn *since)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n;; ");
      print_gimple_stmt (dump_file, stmt, 0,
			 TDF_SLIM | (dump_flags & TDF_LINENO));
      fprintf (dump_file, "\n");

      print_rtl (dump_file, since ? NEXT_INSN (since) : since);
    }
}

/* Maps the blocks that do not contain tree labels to rtx labels.  */

static hash_map<basic_block, rtx_code_label *> *lab_rtx_for_bb;

/* Returns the label_rtx expression for a label starting basic block BB.  */

static rtx_code_label *
label_rtx_for_bb (basic_block bb ATTRIBUTE_UNUSED)
{
  gimple_stmt_iterator gsi;
  tree lab;

  if (bb->flags & BB_RTL)
    return block_label (bb);

  rtx_code_label **elt = lab_rtx_for_bb->get (bb);
  if (elt)
    return *elt;

  /* Find the tree label if it is present.  */

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      glabel *lab_stmt;

      lab_stmt = dyn_cast <glabel *> (gsi_stmt (gsi));
      if (!lab_stmt)
	break;

      lab = gimple_label_label (lab_stmt);
      if (DECL_NONLOCAL (lab))
	break;

      return jump_target_rtx (lab);
    }

  rtx_code_label *l = gen_label_rtx ();
  lab_rtx_for_bb->put (bb, l);
  return l;
}


/* A subroutine of expand_gimple_cond.  Given E, a fallthrough edge
   of a basic block where we just expanded the conditional at the end,
   possibly clean up the CFG and instruction sequence.  LAST is the
   last instruction before the just emitted jump sequence.  */

static void
maybe_cleanup_end_of_block (edge e, rtx_insn *last)
{
  /* Special case: when jumpif decides that the condition is
     trivial it emits an unconditional jump (and the necessary
     barrier).  But we still have two edges, the fallthru one is
     wrong.  purge_dead_edges would clean this up later.  Unfortunately
     we have to insert insns (and split edges) before
     find_many_sub_basic_blocks and hence before purge_dead_edges.
     But splitting edges might create new blocks which depend on the
     fact that if there are two edges there's no barrier.  So the
     barrier would get lost and verify_flow_info would ICE.  Instead
     of auditing all edge splitters to care for the barrier (which
     normally isn't there in a cleaned CFG), fix it here.  */
  if (BARRIER_P (get_last_insn ()))
    {
      rtx_insn *insn;
      remove_edge (e);
      /* Now, we have a single successor block, if we have insns to
	 insert on the remaining edge we potentially will insert
	 it at the end of this block (if the dest block isn't feasible)
	 in order to avoid splitting the edge.  This insertion will take
	 place in front of the last jump.  But we might have emitted
	 multiple jumps (conditional and one unconditional) to the
	 same destination.  Inserting in front of the last one then
	 is a problem.  See PR 40021.  We fix this by deleting all
	 jumps except the last unconditional one.  */
      insn = PREV_INSN (get_last_insn ());
      /* Make sure we have an unconditional jump.  Otherwise we're
	 confused.  */
      gcc_assert (JUMP_P (insn) && !any_condjump_p (insn));
      for (insn = PREV_INSN (insn); insn != last;)
	{
	  insn = PREV_INSN (insn);
	  if (JUMP_P (NEXT_INSN (insn)))
	    {
	      if (!any_condjump_p (NEXT_INSN (insn)))
		{
		  gcc_assert (BARRIER_P (NEXT_INSN (NEXT_INSN (insn))));
		  delete_insn (NEXT_INSN (NEXT_INSN (insn)));
		}
	      delete_insn (NEXT_INSN (insn));
	    }
	}
    }
}

/* A subroutine of expand_gimple_basic_block.  Expand one GIMPLE_COND.
   Returns a new basic block if we've terminated the current basic
   block and created a new one.  */

static basic_block
expand_gimple_cond (basic_block bb, gcond *stmt)
{
  basic_block new_bb, dest;
  edge new_edge;
  edge true_edge;
  edge false_edge;
  rtx_insn *last2, *last;
  enum tree_code code;
  tree op0, op1;

  code = gimple_cond_code (stmt);
  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);
  /* We're sometimes presented with such code:
       D.123_1 = x < y;
       if (D.123_1 != 0)
         ...
     This would expand to two comparisons which then later might
     be cleaned up by combine.  But some pattern matchers like if-conversion
     work better when there's only one compare, so make up for this
     here as special exception if TER would have made the same change.  */
  if (SA.values
      && TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE
      && TREE_CODE (op1) == INTEGER_CST
      && ((gimple_cond_code (stmt) == NE_EXPR
	   && integer_zerop (op1))
	  || (gimple_cond_code (stmt) == EQ_EXPR
	      && integer_onep (op1)))
      && bitmap_bit_p (SA.values, SSA_NAME_VERSION (op0)))
    {
      gimple second = SSA_NAME_DEF_STMT (op0);
      if (gimple_code (second) == GIMPLE_ASSIGN)
	{
	  enum tree_code code2 = gimple_assign_rhs_code (second);
	  if (TREE_CODE_CLASS (code2) == tcc_comparison)
	    {
	      code = code2;
	      op0 = gimple_assign_rhs1 (second);
	      op1 = gimple_assign_rhs2 (second);
	    }
	  /* If jumps are cheap and the target does not support conditional
	     compare, turn some more codes into jumpy sequences.  */
	  else if (BRANCH_COST (optimize_insn_for_speed_p (), false) < 4
		   && targetm.gen_ccmp_first == NULL)
	    {
	      if ((code2 == BIT_AND_EXPR
		   && TYPE_PRECISION (TREE_TYPE (op0)) == 1
		   && TREE_CODE (gimple_assign_rhs2 (second)) != INTEGER_CST)
		  || code2 == TRUTH_AND_EXPR)
		{
		  code = TRUTH_ANDIF_EXPR;
		  op0 = gimple_assign_rhs1 (second);
		  op1 = gimple_assign_rhs2 (second);
		}
	      else if (code2 == BIT_IOR_EXPR || code2 == TRUTH_OR_EXPR)
		{
		  code = TRUTH_ORIF_EXPR;
		  op0 = gimple_assign_rhs1 (second);
		  op1 = gimple_assign_rhs2 (second);
		}
	    }
	}
    }

  last2 = last = get_last_insn ();

  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);
  set_curr_insn_location (gimple_location (stmt));

  /* These flags have no purpose in RTL land.  */
  true_edge->flags &= ~EDGE_TRUE_VALUE;
  false_edge->flags &= ~EDGE_FALSE_VALUE;

  /* We can either have a pure conditional jump with one fallthru edge or
     two-way jump that needs to be decomposed into two basic blocks.  */
  if (false_edge->dest == bb->next_bb)
    {
      jumpif_1 (code, op0, op1, label_rtx_for_bb (true_edge->dest),
		true_edge->probability);
      maybe_dump_rtl_for_gimple_stmt (stmt, last);
      if (true_edge->goto_locus != UNKNOWN_LOCATION)
	set_curr_insn_location (true_edge->goto_locus);
      false_edge->flags |= EDGE_FALLTHRU;
      maybe_cleanup_end_of_block (false_edge, last);
      return NULL;
    }
  if (true_edge->dest == bb->next_bb)
    {
      jumpifnot_1 (code, op0, op1, label_rtx_for_bb (false_edge->dest),
		   false_edge->probability);
      maybe_dump_rtl_for_gimple_stmt (stmt, last);
      if (false_edge->goto_locus != UNKNOWN_LOCATION)
	set_curr_insn_location (false_edge->goto_locus);
      true_edge->flags |= EDGE_FALLTHRU;
      maybe_cleanup_end_of_block (true_edge, last);
      return NULL;
    }

  jumpif_1 (code, op0, op1, label_rtx_for_bb (true_edge->dest),
	    true_edge->probability);
  last = get_last_insn ();
  if (false_edge->goto_locus != UNKNOWN_LOCATION)
    set_curr_insn_location (false_edge->goto_locus);
  emit_jump (label_rtx_for_bb (false_edge->dest));

  BB_END (bb) = last;
  if (BARRIER_P (BB_END (bb)))
    BB_END (bb) = PREV_INSN (BB_END (bb));
  update_bb_for_insn (bb);

  new_bb = create_basic_block (NEXT_INSN (last), get_last_insn (), bb);
  dest = false_edge->dest;
  redirect_edge_succ (false_edge, new_bb);
  false_edge->flags |= EDGE_FALLTHRU;
  new_bb->count = false_edge->count;
  new_bb->frequency = EDGE_FREQUENCY (false_edge);
  add_bb_to_loop (new_bb, bb->loop_father);
  new_edge = make_edge (new_bb, dest, 0);
  new_edge->probability = REG_BR_PROB_BASE;
  new_edge->count = new_bb->count;
  if (BARRIER_P (BB_END (new_bb)))
    BB_END (new_bb) = PREV_INSN (BB_END (new_bb));
  update_bb_for_insn (new_bb);

  maybe_dump_rtl_for_gimple_stmt (stmt, last2);

  if (true_edge->goto_locus != UNKNOWN_LOCATION)
    {
      set_curr_insn_location (true_edge->goto_locus);
      true_edge->goto_locus = curr_insn_location ();
    }

  return new_bb;
}

/* Mark all calls that can have a transaction restart.  */

static void
mark_transaction_restart_calls (gimple stmt)
{
  struct tm_restart_node dummy;
  tm_restart_node **slot;

  if (!cfun->gimple_df->tm_restart)
    return;

  dummy.stmt = stmt;
  slot = cfun->gimple_df->tm_restart->find_slot (&dummy, NO_INSERT);
  if (slot)
    {
      struct tm_restart_node *n = *slot;
      tree list = n->label_or_list;
      rtx_insn *insn;

      for (insn = next_real_insn (get_last_insn ());
	   !CALL_P (insn);
	   insn = next_real_insn (insn))
	continue;

      if (TREE_CODE (list) == LABEL_DECL)
	add_reg_note (insn, REG_TM, label_rtx (list));
      else
	for (; list ; list = TREE_CHAIN (list))
	  add_reg_note (insn, REG_TM, label_rtx (TREE_VALUE (list)));
    }
}

/* A subroutine of expand_gimple_stmt_1, expanding one GIMPLE_CALL
   statement STMT.  */

static void
expand_call_stmt (gcall *stmt)
{
  tree exp, decl, lhs;
  bool builtin_p;
  size_t i;

  if (gimple_call_internal_p (stmt))
    {
      expand_internal_call (stmt);
      return;
    }

  exp = build_vl_exp (CALL_EXPR, gimple_call_num_args (stmt) + 3);

  CALL_EXPR_FN (exp) = gimple_call_fn (stmt);
  decl = gimple_call_fndecl (stmt);
  builtin_p = decl && DECL_BUILT_IN (decl);

  /* If this is not a builtin function, the function type through which the
     call is made may be different from the type of the function.  */
  if (!builtin_p)
    CALL_EXPR_FN (exp)
      = fold_convert (build_pointer_type (gimple_call_fntype (stmt)),
		      CALL_EXPR_FN (exp));

  TREE_TYPE (exp) = gimple_call_return_type (stmt);
  CALL_EXPR_STATIC_CHAIN (exp) = gimple_call_chain (stmt);

  for (i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      gimple def;
      /* TER addresses into arguments of builtin functions so we have a
	 chance to infer more correct alignment information.  See PR39954.  */
      if (builtin_p
	  && TREE_CODE (arg) == SSA_NAME
	  && (def = get_gimple_for_ssa_name (arg))
	  && gimple_assign_rhs_code (def) == ADDR_EXPR)
	arg = gimple_assign_rhs1 (def);
      CALL_EXPR_ARG (exp, i) = arg;
    }

  if (gimple_has_side_effects (stmt))
    TREE_SIDE_EFFECTS (exp) = 1;

  if (gimple_call_nothrow_p (stmt))
    TREE_NOTHROW (exp) = 1;

  CALL_EXPR_TAILCALL (exp) = gimple_call_tail_p (stmt);
  CALL_EXPR_RETURN_SLOT_OPT (exp) = gimple_call_return_slot_opt_p (stmt);
  if (decl
      && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (decl) == BUILT_IN_ALLOCA
	  || DECL_FUNCTION_CODE (decl) == BUILT_IN_ALLOCA_WITH_ALIGN))
    CALL_ALLOCA_FOR_VAR_P (exp) = gimple_call_alloca_for_var_p (stmt);
  else
    CALL_FROM_THUNK_P (exp) = gimple_call_from_thunk_p (stmt);
  CALL_EXPR_VA_ARG_PACK (exp) = gimple_call_va_arg_pack_p (stmt);
  SET_EXPR_LOCATION (exp, gimple_location (stmt));
  CALL_WITH_BOUNDS_P (exp) = gimple_call_with_bounds_p (stmt);

  /* Ensure RTL is created for debug args.  */
  if (decl && DECL_HAS_DEBUG_ARGS_P (decl))
    {
      vec<tree, va_gc> **debug_args = decl_debug_args_lookup (decl);
      unsigned int ix;
      tree dtemp;

      if (debug_args)
	for (ix = 1; (*debug_args)->iterate (ix, &dtemp); ix += 2)
	  {
	    gcc_assert (TREE_CODE (dtemp) == DEBUG_EXPR_DECL);
	    expand_debug_expr (dtemp);
	  }
    }

  lhs = gimple_call_lhs (stmt);
  if (lhs)
    expand_assignment (lhs, exp, false);
  else
    expand_expr (exp, const0_rtx, VOIDmode, EXPAND_NORMAL);

  mark_transaction_restart_calls (stmt);
}


/* Generate RTL for an asm statement (explicit assembler code).
   STRING is a STRING_CST node containing the assembler code text,
   or an ADDR_EXPR containing a STRING_CST.  VOL nonzero means the
   insn is volatile; don't optimize it.  */

static void
expand_asm_loc (tree string, int vol, location_t locus)
{
  rtx body;

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);

  body = gen_rtx_ASM_INPUT_loc (VOIDmode,
				ggc_strdup (TREE_STRING_POINTER (string)),
				locus);

  MEM_VOLATILE_P (body) = vol;

  emit_insn (body);
}

/* Return the number of times character C occurs in string S.  */
static int
n_occurrences (int c, const char *s)
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}

/* A subroutine of expand_asm_operands.  Check that all operands have
   the same number of alternatives.  Return true if so.  */

static bool
check_operand_nalternatives (const vec<const char *> &constraints)
{
  unsigned len = constraints.length();
  if (len > 0)
    {
      int nalternatives = n_occurrences (',', constraints[0]);

      if (nalternatives + 1 > MAX_RECOG_ALTERNATIVES)
	{
	  error ("too many alternatives in %<asm%>");
	  return false;
	}

      for (unsigned i = 1; i < len; ++i)
	if (n_occurrences (',', constraints[i]) != nalternatives)
	  {
	    error ("operand constraints for %<asm%> differ "
		   "in number of alternatives");
	    return false;
	  }
    }
  return true;
}

/* Check for overlap between registers marked in CLOBBERED_REGS and
   anything inappropriate in T.  Emit error and return the register
   variable definition for error, NULL_TREE for ok.  */

static bool
tree_conflicts_with_clobbers_p (tree t, HARD_REG_SET *clobbered_regs)
{
  /* Conflicts between asm-declared register variables and the clobber
     list are not allowed.  */
  tree overlap = tree_overlaps_hard_reg_set (t, clobbered_regs);

  if (overlap)
    {
      error ("asm-specifier for variable %qE conflicts with asm clobber list",
	     DECL_NAME (overlap));

      /* Reset registerness to stop multiple errors emitted for a single
	 variable.  */
      DECL_REGISTER (overlap) = 0;
      return true;
    }

  return false;
}

/* Generate RTL for an asm statement with arguments.
   STRING is the instruction template.
   OUTPUTS is a list of output arguments (lvalues); INPUTS a list of inputs.
   Each output or input has an expression in the TREE_VALUE and
   a tree list in TREE_PURPOSE which in turn contains a constraint
   name in TREE_VALUE (or NULL_TREE) and a constraint string
   in TREE_PURPOSE.
   CLOBBERS is a list of STRING_CST nodes each naming a hard register
   that is clobbered by this insn.

   LABELS is a list of labels, and if LABELS is non-NULL, FALLTHRU_BB
   should be the fallthru basic block of the asm goto.

   Not all kinds of lvalue that may appear in OUTPUTS can be stored directly.
   Some elements of OUTPUTS may be replaced with trees representing temporary
   values.  The caller should copy those temporary values to the originally
   specified lvalues.

   VOL nonzero means the insn is volatile; don't optimize it.  */

static void
expand_asm_stmt (gasm *stmt)
{
  class save_input_location
  {
    location_t old;

  public:
    explicit save_input_location(location_t where)
    {
      old = input_location;
      input_location = where;
    }

    ~save_input_location()
    {
      input_location = old;
    }
  };

  location_t locus = gimple_location (stmt);

  if (gimple_asm_input_p (stmt))
    {
      const char *s = gimple_asm_string (stmt);
      tree string = build_string (strlen (s), s);
      expand_asm_loc (string, gimple_asm_volatile_p (stmt), locus);
      return;
    }

  /* There are some legacy diagnostics in here, and also avoids a
     sixth parameger to targetm.md_asm_adjust.  */
  save_input_location s_i_l(locus);

  unsigned noutputs = gimple_asm_noutputs (stmt);
  unsigned ninputs = gimple_asm_ninputs (stmt);
  unsigned nlabels = gimple_asm_nlabels (stmt);
  unsigned i;

  /* ??? Diagnose during gimplification?  */
  if (ninputs + noutputs + nlabels > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in %<asm%>", MAX_RECOG_OPERANDS);
      return;
    }

  auto_vec<tree, MAX_RECOG_OPERANDS> output_tvec;
  auto_vec<tree, MAX_RECOG_OPERANDS> input_tvec;
  auto_vec<const char *, MAX_RECOG_OPERANDS> constraints;

  /* Copy the gimple vectors into new vectors that we can manipulate.  */

  output_tvec.safe_grow (noutputs);
  input_tvec.safe_grow (ninputs);
  constraints.safe_grow (noutputs + ninputs);

  for (i = 0; i < noutputs; ++i)
    {
      tree t = gimple_asm_output_op (stmt, i);
      output_tvec[i] = TREE_VALUE (t);
      constraints[i] = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
    }
  for (i = 0; i < ninputs; i++)
    {
      tree t = gimple_asm_input_op (stmt, i);
      input_tvec[i] = TREE_VALUE (t);
      constraints[i + noutputs]
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
    }

  /* ??? Diagnose during gimplification?  */
  if (! check_operand_nalternatives (constraints))
    return;

  /* Count the number of meaningful clobbered registers, ignoring what
     we would ignore later.  */
  auto_vec<rtx> clobber_rvec;
  HARD_REG_SET clobbered_regs;
  CLEAR_HARD_REG_SET (clobbered_regs);

  if (unsigned n = gimple_asm_nclobbers (stmt))
    {
      clobber_rvec.reserve (n);
      for (i = 0; i < n; i++)
	{
	  tree t = gimple_asm_clobber_op (stmt, i);
          const char *regname = TREE_STRING_POINTER (TREE_VALUE (t));
	  int nregs, j;

	  j = decode_reg_name_and_count (regname, &nregs);
	  if (j < 0)
	    {
	      if (j == -2)
		{
		  /* ??? Diagnose during gimplification?  */
		  error ("unknown register name %qs in %<asm%>", regname);
		}
	      else if (j == -4)
		{
		  rtx x = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode));
		  clobber_rvec.safe_push (x);
		}
	      else
		{
		  /* Otherwise we should have -1 == empty string
		     or -3 == cc, which is not a register.  */
		  gcc_assert (j == -1 || j == -3);
		}
	    }
	  else
	    for (int reg = j; reg < j + nregs; reg++)
	      {
		/* Clobbering the PIC register is an error.  */
		if (reg == (int) PIC_OFFSET_TABLE_REGNUM)
		  {
		    /* ??? Diagnose during gimplification?  */
		    error ("PIC register clobbered by %qs in %<asm%>",
			   regname);
		    return;
		  }

	        SET_HARD_REG_BIT (clobbered_regs, reg);
	        rtx x = gen_rtx_REG (reg_raw_mode[reg], reg);
		clobber_rvec.safe_push (x);
	      }
	}
    }
  unsigned nclobbers = clobber_rvec.length();

  /* First pass over inputs and outputs checks validity and sets
     mark_addressable if needed.  */
  /* ??? Diagnose during gimplification?  */

  for (i = 0; i < noutputs; ++i)
    {
      tree val = output_tvec[i];
      tree type = TREE_TYPE (val);
      const char *constraint;
      bool is_inout;
      bool allows_reg;
      bool allows_mem;

      /* Try to parse the output constraint.  If that fails, there's
	 no point in going further.  */
      constraint = constraints[i];
      if (!parse_output_constraint (&constraint, i, ninputs, noutputs,
				    &allows_mem, &allows_reg, &is_inout))
	return;

      if (! allows_reg
	  && (allows_mem
	      || is_inout
	      || (DECL_P (val)
		  && REG_P (DECL_RTL (val))
		  && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type))))
	mark_addressable (val);
    }

  for (i = 0; i < ninputs; ++i)
    {
      bool allows_reg, allows_mem;
      const char *constraint;

      constraint = constraints[i + noutputs];
      if (! parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
				    constraints.address (),
				    &allows_mem, &allows_reg))
	return;

      if (! allows_reg && allows_mem)
	mark_addressable (input_tvec[i]);
    }

  /* Second pass evaluates arguments.  */

  /* Make sure stack is consistent for asm goto.  */
  if (nlabels > 0)
    do_pending_stack_adjust ();
  int old_generating_concat_p = generating_concat_p;

  /* Vector of RTX's of evaluated output operands.  */
  auto_vec<rtx, MAX_RECOG_OPERANDS> output_rvec;
  auto_vec<int, MAX_RECOG_OPERANDS> inout_opnum;
  rtx_insn *after_rtl_seq = NULL, *after_rtl_end = NULL;

  output_rvec.safe_grow (noutputs);

  for (i = 0; i < noutputs; ++i)
    {
      tree val = output_tvec[i];
      tree type = TREE_TYPE (val);
      bool is_inout, allows_reg, allows_mem, ok;
      rtx op;

      ok = parse_output_constraint (&constraints[i], i, ninputs,
				    noutputs, &allows_mem, &allows_reg,
				    &is_inout);
      gcc_assert (ok);

      /* If an output operand is not a decl or indirect ref and our constraint
	 allows a register, make a temporary to act as an intermediate.
	 Make the asm insn write into that, then we will copy it to
	 the real output operand.  Likewise for promoted variables.  */

      generating_concat_p = 0;

      if ((TREE_CODE (val) == INDIRECT_REF
	   && allows_mem)
	  || (DECL_P (val)
	      && (allows_mem || REG_P (DECL_RTL (val)))
	      && ! (REG_P (DECL_RTL (val))
		    && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type)))
	  || ! allows_reg
	  || is_inout)
	{
	  op = expand_expr (val, NULL_RTX, VOIDmode,
			    !allows_reg ? EXPAND_MEMORY : EXPAND_WRITE);
	  if (MEM_P (op))
	    op = validize_mem (op);

	  if (! allows_reg && !MEM_P (op))
	    error ("output number %d not directly addressable", i);
	  if ((! allows_mem && MEM_P (op))
	      || GET_CODE (op) == CONCAT)
	    {
	      rtx old_op = op;
	      op = gen_reg_rtx (GET_MODE (op));

	      generating_concat_p = old_generating_concat_p;

	      if (is_inout)
		emit_move_insn (op, old_op);

	      push_to_sequence2 (after_rtl_seq, after_rtl_end);
	      emit_move_insn (old_op, op);
	      after_rtl_seq = get_insns ();
	      after_rtl_end = get_last_insn ();
	      end_sequence ();
	    }
	}
      else
	{
	  op = assign_temp (type, 0, 1);
	  op = validize_mem (op);
	  if (!MEM_P (op) && TREE_CODE (val) == SSA_NAME)
	    set_reg_attrs_for_decl_rtl (SSA_NAME_VAR (val), op);

	  generating_concat_p = old_generating_concat_p;

	  push_to_sequence2 (after_rtl_seq, after_rtl_end);
	  expand_assignment (val, make_tree (type, op), false);
	  after_rtl_seq = get_insns ();
	  after_rtl_end = get_last_insn ();
	  end_sequence ();
	}
      output_rvec[i] = op;

      if (is_inout)
	inout_opnum.safe_push (i);
    }

  auto_vec<rtx, MAX_RECOG_OPERANDS> input_rvec;
  auto_vec<machine_mode, MAX_RECOG_OPERANDS> input_mode;

  input_rvec.safe_grow (ninputs);
  input_mode.safe_grow (ninputs);

  generating_concat_p = 0;

  for (i = 0; i < ninputs; ++i)
    {
      tree val = input_tvec[i];
      tree type = TREE_TYPE (val);
      bool allows_reg, allows_mem, ok;
      const char *constraint;
      rtx op;

      constraint = constraints[i + noutputs];
      ok = parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
				   constraints.address (),
				   &allows_mem, &allows_reg);
      gcc_assert (ok);

      /* EXPAND_INITIALIZER will not generate code for valid initializer
	 constants, but will still generate code for other types of operand.
	 This is the behavior we want for constant constraints.  */
      op = expand_expr (val, NULL_RTX, VOIDmode,
			allows_reg ? EXPAND_NORMAL
			: allows_mem ? EXPAND_MEMORY
			: EXPAND_INITIALIZER);

      /* Never pass a CONCAT to an ASM.  */
      if (GET_CODE (op) == CONCAT)
	op = force_reg (GET_MODE (op), op);
      else if (MEM_P (op))
	op = validize_mem (op);

      if (asm_operand_ok (op, constraint, NULL) <= 0)
	{
	  if (allows_reg && TYPE_MODE (type) != BLKmode)
	    op = force_reg (TYPE_MODE (type), op);
	  else if (!allows_mem)
	    warning (0, "asm operand %d probably doesn%'t match constraints",
		     i + noutputs);
	  else if (MEM_P (op))
	    {
	      /* We won't recognize either volatile memory or memory
		 with a queued address as available a memory_operand
		 at this point.  Ignore it: clearly this *is* a memory.  */
	    }
	  else
	    gcc_unreachable ();
	}
      input_rvec[i] = op;
      input_mode[i] = TYPE_MODE (type);
    }

  /* For in-out operands, copy output rtx to input rtx.  */
  unsigned ninout = inout_opnum.length();
  for (i = 0; i < ninout; i++)
    {
      int j = inout_opnum[i];
      rtx o = output_rvec[j];

      input_rvec.safe_push (o);
      input_mode.safe_push (GET_MODE (o));

      char buffer[16];
      sprintf (buffer, "%d", j);
      constraints.safe_push (ggc_strdup (buffer));
    }
  ninputs += ninout;

  /* Sometimes we wish to automatically clobber registers across an asm.
     Case in point is when the i386 backend moved from cc0 to a hard reg --
     maintaining source-level compatibility means automatically clobbering
     the flags register.  */
  rtx_insn *after_md_seq = NULL;
  if (targetm.md_asm_adjust)
    after_md_seq = targetm.md_asm_adjust (output_rvec, input_rvec,
					  constraints, clobber_rvec,
					  clobbered_regs);

  /* Do not allow the hook to change the output and input count,
     lest it mess up the operand numbering.  */
  gcc_assert (output_rvec.length() == noutputs);
  gcc_assert (input_rvec.length() == ninputs);
  gcc_assert (constraints.length() == noutputs + ninputs);

  /* But it certainly can adjust the clobbers.  */
  nclobbers = clobber_rvec.length();

  /* Third pass checks for easy conflicts.  */
  /* ??? Why are we doing this on trees instead of rtx.  */

  bool clobber_conflict_found = 0;
  for (i = 0; i < noutputs; ++i)
    if (tree_conflicts_with_clobbers_p (output_tvec[i], &clobbered_regs))
	clobber_conflict_found = 1;
  for (i = 0; i < ninputs - ninout; ++i)
    if (tree_conflicts_with_clobbers_p (input_tvec[i], &clobbered_regs))
	clobber_conflict_found = 1;

  /* Make vectors for the expression-rtx, constraint strings,
     and named operands.  */

  rtvec argvec = rtvec_alloc (ninputs);
  rtvec constraintvec = rtvec_alloc (ninputs);
  rtvec labelvec = rtvec_alloc (nlabels);

  rtx body = gen_rtx_ASM_OPERANDS ((noutputs == 0 ? VOIDmode
				    : GET_MODE (output_rvec[0])),
				   ggc_strdup (gimple_asm_string (stmt)),
				   empty_string, 0, argvec, constraintvec,
				   labelvec, locus);
  MEM_VOLATILE_P (body) = gimple_asm_volatile_p (stmt);

  for (i = 0; i < ninputs; ++i)
    {
      ASM_OPERANDS_INPUT (body, i) = input_rvec[i];
      ASM_OPERANDS_INPUT_CONSTRAINT_EXP (body, i)
	= gen_rtx_ASM_INPUT_loc (input_mode[i],
				 constraints[i + noutputs],
				 locus);
    }

  /* Copy labels to the vector.  */
  rtx_code_label *fallthru_label = NULL;
  if (nlabels > 0)
    {
      basic_block fallthru_bb = NULL;
      edge fallthru = find_fallthru_edge (gimple_bb (stmt)->succs);
      if (fallthru)
	fallthru_bb = fallthru->dest;

      for (i = 0; i < nlabels; ++i)
	{
	  tree label = TREE_VALUE (gimple_asm_label_op (stmt, i));
	  rtx_insn *r;
	  /* If asm goto has any labels in the fallthru basic block, use
	     a label that we emit immediately after the asm goto.  Expansion
	     may insert further instructions into the same basic block after
	     asm goto and if we don't do this, insertion of instructions on
	     the fallthru edge might misbehave.  See PR58670.  */
	  if (fallthru_bb && label_to_block_fn (cfun, label) == fallthru_bb)
	    {
	      if (fallthru_label == NULL_RTX)
	        fallthru_label = gen_label_rtx ();
	      r = fallthru_label;
	    }
	  else
	    r = label_rtx (label);
	  ASM_OPERANDS_LABEL (body, i) = gen_rtx_LABEL_REF (Pmode, r);
	}
    }

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTCONSTRAINT OUTPUTNUMBER
			       ARGVEC CONSTRAINTS OPNAMES))
     If there is more than one, put them inside a PARALLEL.  */

  if (nlabels > 0 && nclobbers == 0)
    {
      gcc_assert (noutputs == 0);
      emit_jump_insn (body);
    }
  else if (noutputs == 0 && nclobbers == 0)
    {
      /* No output operands: put in a raw ASM_OPERANDS rtx.  */
      emit_insn (body);
    }
  else if (noutputs == 1 && nclobbers == 0)
    {
      ASM_OPERANDS_OUTPUT_CONSTRAINT (body) = constraints[0];
      emit_insn (gen_rtx_SET (output_rvec[0], body));
    }
  else
    {
      rtx obody = body;
      int num = noutputs;

      if (num == 0)
	num = 1;

      body = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num + nclobbers));

      /* For each output operand, store a SET.  */
      for (i = 0; i < noutputs; ++i)
	{
	  rtx src, o = output_rvec[i];
	  if (i == 0)
	    {
	      ASM_OPERANDS_OUTPUT_CONSTRAINT (obody) = constraints[0];
	      src = obody;
	    }
	  else
	    {
	      src = gen_rtx_ASM_OPERANDS (GET_MODE (o),
					  ASM_OPERANDS_TEMPLATE (obody),
					  constraints[i], i, argvec,
					  constraintvec, labelvec, locus);
	      MEM_VOLATILE_P (src) = gimple_asm_volatile_p (stmt);
	    }
	  XVECEXP (body, 0, i) = gen_rtx_SET (o, src);
	}

      /* If there are no outputs (but there are some clobbers)
	 store the bare ASM_OPERANDS into the PARALLEL.  */
      if (i == 0)
	XVECEXP (body, 0, i++) = obody;

      /* Store (clobber REG) for each clobbered register specified.  */
      for (unsigned j = 0; j < nclobbers; ++j)
	{
	  rtx clobbered_reg = clobber_rvec[j];

	  /* Do sanity check for overlap between clobbers and respectively
	     input and outputs that hasn't been handled.  Such overlap
	     should have been detected and reported above.  */
	  if (!clobber_conflict_found && REG_P (clobbered_reg))
	    {
	      /* We test the old body (obody) contents to avoid
		 tripping over the under-construction body.  */
	      for (unsigned k = 0; k < noutputs; ++k)
		if (reg_overlap_mentioned_p (clobbered_reg, output_rvec[k]))
		  internal_error ("asm clobber conflict with output operand");

	      for (unsigned k = 0; k < ninputs - ninout; ++k)
		if (reg_overlap_mentioned_p (clobbered_reg, input_rvec[k]))
		  internal_error ("asm clobber conflict with input operand");
	    }

	  XVECEXP (body, 0, i++) = gen_rtx_CLOBBER (VOIDmode, clobbered_reg);
	}

      if (nlabels > 0)
	emit_jump_insn (body);
      else
	emit_insn (body);
    }

  generating_concat_p = old_generating_concat_p;

  if (fallthru_label)
    emit_label (fallthru_label);

  if (after_md_seq)
    emit_insn (after_md_seq);
  if (after_rtl_seq)
    emit_insn (after_rtl_seq);

  free_temp_slots ();
  crtl->has_asm_statement = 1;
}

/* Emit code to jump to the address
   specified by the pointer expression EXP.  */

static void
expand_computed_goto (tree exp)
{
  rtx x = expand_normal (exp);

  do_pending_stack_adjust ();
  emit_indirect_jump (x);
}

/* Generate RTL code for a `goto' statement with target label LABEL.
   LABEL should be a LABEL_DECL tree node that was or will later be
   defined with `expand_label'.  */

static void
expand_goto (tree label)
{
#ifdef ENABLE_CHECKING
  /* Check for a nonlocal goto to a containing function.  Should have
     gotten translated to __builtin_nonlocal_goto.  */
  tree context = decl_function_context (label);
  gcc_assert (!context || context == current_function_decl);
#endif

  emit_jump (jump_target_rtx (label));
}

/* Output a return with no value.  */

static void
expand_null_return_1 (void)
{
  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();
  emit_jump (return_label);
}

/* Generate RTL to return from the current function, with no value.
   (That is, we do not do anything about returning any value.)  */

void
expand_null_return (void)
{
  /* If this function was declared to return a value, but we
     didn't, clobber the return registers so that they are not
     propagated live to the rest of the function.  */
  clobber_return_register ();

  expand_null_return_1 ();
}

/* Generate RTL to return from the current function, with value VAL.  */

static void
expand_value_return (rtx val)
{
  /* Copy the value to the return location unless it's already there.  */

  tree decl = DECL_RESULT (current_function_decl);
  rtx return_reg = DECL_RTL (decl);
  if (return_reg != val)
    {
      tree funtype = TREE_TYPE (current_function_decl);
      tree type = TREE_TYPE (decl);
      int unsignedp = TYPE_UNSIGNED (type);
      machine_mode old_mode = DECL_MODE (decl);
      machine_mode mode;
      if (DECL_BY_REFERENCE (decl))
        mode = promote_function_mode (type, old_mode, &unsignedp, funtype, 2);
      else
        mode = promote_function_mode (type, old_mode, &unsignedp, funtype, 1);

      if (mode != old_mode)
	val = convert_modes (mode, old_mode, val, unsignedp);

      if (GET_CODE (return_reg) == PARALLEL)
	emit_group_load (return_reg, val, type, int_size_in_bytes (type));
      else
	emit_move_insn (return_reg, val);
    }

  expand_null_return_1 ();
}

/* Generate RTL to evaluate the expression RETVAL and return it
   from the current function.  */

static void
expand_return (tree retval, tree bounds)
{
  rtx result_rtl;
  rtx val = 0;
  tree retval_rhs;
  rtx bounds_rtl;

  /* If function wants no value, give it none.  */
  if (TREE_CODE (TREE_TYPE (TREE_TYPE (current_function_decl))) == VOID_TYPE)
    {
      expand_normal (retval);
      expand_null_return ();
      return;
    }

  if (retval == error_mark_node)
    {
      /* Treat this like a return of no value from a function that
	 returns a value.  */
      expand_null_return ();
      return;
    }
  else if ((TREE_CODE (retval) == MODIFY_EXPR
	    || TREE_CODE (retval) == INIT_EXPR)
	   && TREE_CODE (TREE_OPERAND (retval, 0)) == RESULT_DECL)
    retval_rhs = TREE_OPERAND (retval, 1);
  else
    retval_rhs = retval;

  result_rtl = DECL_RTL (DECL_RESULT (current_function_decl));

  /* Put returned bounds to the right place.  */
  bounds_rtl = DECL_BOUNDS_RTL (DECL_RESULT (current_function_decl));
  if (bounds_rtl)
    {
      rtx addr = NULL;
      rtx bnd = NULL;

      if (bounds && bounds != error_mark_node)
	{
	  bnd = expand_normal (bounds);
	  targetm.calls.store_returned_bounds (bounds_rtl, bnd);
	}
      else if (REG_P (bounds_rtl))
	{
	  if (bounds)
	    bnd = chkp_expand_zero_bounds ();
	  else
	    {
	      addr = expand_normal (build_fold_addr_expr (retval_rhs));
	      addr = gen_rtx_MEM (Pmode, addr);
	      bnd = targetm.calls.load_bounds_for_arg (addr, NULL, NULL);
	    }

	  targetm.calls.store_returned_bounds (bounds_rtl, bnd);
	}
      else
	{
	  int n;

	  gcc_assert (GET_CODE (bounds_rtl) == PARALLEL);

	  if (bounds)
	    bnd = chkp_expand_zero_bounds ();
	  else
	    {
	      addr = expand_normal (build_fold_addr_expr (retval_rhs));
	      addr = gen_rtx_MEM (Pmode, addr);
	    }

	  for (n = 0; n < XVECLEN (bounds_rtl, 0); n++)
	    {
	      rtx slot = XEXP (XVECEXP (bounds_rtl, 0, n), 0);
	      if (!bounds)
		{
		  rtx offs = XEXP (XVECEXP (bounds_rtl, 0, n), 1);
		  rtx from = adjust_address (addr, Pmode, INTVAL (offs));
		  bnd = targetm.calls.load_bounds_for_arg (from, NULL, NULL);
		}
	      targetm.calls.store_returned_bounds (slot, bnd);
	    }
	}
    }
  else if (chkp_function_instrumented_p (current_function_decl)
	   && !BOUNDED_P (retval_rhs)
	   && chkp_type_has_pointer (TREE_TYPE (retval_rhs))
	   && TREE_CODE (retval_rhs) != RESULT_DECL)
    {
      rtx addr = expand_normal (build_fold_addr_expr (retval_rhs));
      addr = gen_rtx_MEM (Pmode, addr);

      gcc_assert (MEM_P (result_rtl));

      chkp_copy_bounds_for_stack_parm (result_rtl, addr, TREE_TYPE (retval_rhs));
    }

  /* If we are returning the RESULT_DECL, then the value has already
     been stored into it, so we don't have to do anything special.  */
  if (TREE_CODE (retval_rhs) == RESULT_DECL)
    expand_value_return (result_rtl);

  /* If the result is an aggregate that is being returned in one (or more)
     registers, load the registers here.  */

  else if (retval_rhs != 0
	   && TYPE_MODE (TREE_TYPE (retval_rhs)) == BLKmode
	   && REG_P (result_rtl))
    {
      val = copy_blkmode_to_reg (GET_MODE (result_rtl), retval_rhs);
      if (val)
	{
	  /* Use the mode of the result value on the return register.  */
	  PUT_MODE (result_rtl, GET_MODE (val));
	  expand_value_return (val);
	}
      else
	expand_null_return ();
    }
  else if (retval_rhs != 0
	   && !VOID_TYPE_P (TREE_TYPE (retval_rhs))
	   && (REG_P (result_rtl)
	       || (GET_CODE (result_rtl) == PARALLEL)))
    {
      /* Compute the return value into a temporary (usually a pseudo reg).  */
      val
	= assign_temp (TREE_TYPE (DECL_RESULT (current_function_decl)), 0, 1);
      val = expand_expr (retval_rhs, val, GET_MODE (val), EXPAND_NORMAL);
      val = force_not_mem (val);
      expand_value_return (val);
    }
  else
    {
      /* No hard reg used; calculate value into hard return reg.  */
      expand_expr (retval, const0_rtx, VOIDmode, EXPAND_NORMAL);
      expand_value_return (result_rtl);
    }
}

/* A subroutine of expand_gimple_stmt, expanding one gimple statement
   STMT that doesn't require special handling for outgoing edges.  That
   is no tailcalls and no GIMPLE_COND.  */

static void
expand_gimple_stmt_1 (gimple stmt)
{
  tree op0;

  set_curr_insn_location (gimple_location (stmt));

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
      op0 = gimple_goto_dest (stmt);
      if (TREE_CODE (op0) == LABEL_DECL)
	expand_goto (op0);
      else
	expand_computed_goto (op0);
      break;
    case GIMPLE_LABEL:
      expand_label (gimple_label_label (as_a <glabel *> (stmt)));
      break;
    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
      break;
    case GIMPLE_SWITCH:
      expand_case (as_a <gswitch *> (stmt));
      break;
    case GIMPLE_ASM:
      expand_asm_stmt (as_a <gasm *> (stmt));
      break;
    case GIMPLE_CALL:
      expand_call_stmt (as_a <gcall *> (stmt));
      break;

    case GIMPLE_RETURN:
      {
	tree bnd = gimple_return_retbnd (as_a <greturn *> (stmt));
	op0 = gimple_return_retval (as_a <greturn *> (stmt));

	if (op0 && op0 != error_mark_node)
	  {
	    tree result = DECL_RESULT (current_function_decl);

	    /* If we are not returning the current function's RESULT_DECL,
	       build an assignment to it.  */
	    if (op0 != result)
	      {
		/* I believe that a function's RESULT_DECL is unique.  */
		gcc_assert (TREE_CODE (op0) != RESULT_DECL);

		/* ??? We'd like to use simply expand_assignment here,
		   but this fails if the value is of BLKmode but the return
		   decl is a register.  expand_return has special handling
		   for this combination, which eventually should move
		   to common code.  See comments there.  Until then, let's
		   build a modify expression :-/  */
		op0 = build2 (MODIFY_EXPR, TREE_TYPE (result),
			      result, op0);
	      }
	    /* Mark we have return statement with missing bounds.  */
	    if (!bnd && chkp_function_instrumented_p (cfun->decl))
	      bnd = error_mark_node;
	  }

	if (!op0)
	  expand_null_return ();
	else
	  expand_return (op0, bnd);
      }
      break;

    case GIMPLE_ASSIGN:
      {
	gassign *assign_stmt = as_a <gassign *> (stmt);
	tree lhs = gimple_assign_lhs (assign_stmt);

	/* Tree expand used to fiddle with |= and &= of two bitfield
	   COMPONENT_REFs here.  This can't happen with gimple, the LHS
	   of binary assigns must be a gimple reg.  */

	if (TREE_CODE (lhs) != SSA_NAME
	    || get_gimple_rhs_class (gimple_expr_code (stmt))
	       == GIMPLE_SINGLE_RHS)
	  {
	    tree rhs = gimple_assign_rhs1 (assign_stmt);
	    gcc_assert (get_gimple_rhs_class (gimple_expr_code (stmt))
			== GIMPLE_SINGLE_RHS);
	    if (gimple_has_location (stmt) && CAN_HAVE_LOCATION_P (rhs))
	      SET_EXPR_LOCATION (rhs, gimple_location (stmt));
	    if (TREE_CLOBBER_P (rhs))
	      /* This is a clobber to mark the going out of scope for
		 this LHS.  */
	      ;
	    else
	      expand_assignment (lhs, rhs,
				 gimple_assign_nontemporal_move_p (
				   assign_stmt));
	  }
	else
	  {
	    rtx target, temp;
	    bool nontemporal = gimple_assign_nontemporal_move_p (assign_stmt);
	    struct separate_ops ops;
	    bool promoted = false;

	    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
	    if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
	      promoted = true;

	    ops.code = gimple_assign_rhs_code (assign_stmt);
	    ops.type = TREE_TYPE (lhs);
	    switch (get_gimple_rhs_class (ops.code))
	      {
		case GIMPLE_TERNARY_RHS:
		  ops.op2 = gimple_assign_rhs3 (assign_stmt);
		  /* Fallthru */
		case GIMPLE_BINARY_RHS:
		  ops.op1 = gimple_assign_rhs2 (assign_stmt);
		  /* Fallthru */
		case GIMPLE_UNARY_RHS:
		  ops.op0 = gimple_assign_rhs1 (assign_stmt);
		  break;
		default:
		  gcc_unreachable ();
	      }
	    ops.location = gimple_location (stmt);

	    /* If we want to use a nontemporal store, force the value to
	       register first.  If we store into a promoted register,
	       don't directly expand to target.  */
	    temp = nontemporal || promoted ? NULL_RTX : target;
	    temp = expand_expr_real_2 (&ops, temp, GET_MODE (target),
				       EXPAND_NORMAL);

	    if (temp == target)
	      ;
	    else if (promoted)
	      {
		int unsignedp = SUBREG_PROMOTED_SIGN (target);
		/* If TEMP is a VOIDmode constant, use convert_modes to make
		   sure that we properly convert it.  */
		if (CONSTANT_P (temp) && GET_MODE (temp) == VOIDmode)
		  {
		    temp = convert_modes (GET_MODE (target),
					  TYPE_MODE (ops.type),
					  temp, unsignedp);
		    temp = convert_modes (GET_MODE (SUBREG_REG (target)),
					  GET_MODE (target), temp, unsignedp);
		  }

		convert_move (SUBREG_REG (target), temp, unsignedp);
	      }
	    else if (nontemporal && emit_storent_insn (target, temp))
	      ;
	    else
	      {
		temp = force_operand (temp, target);
		if (temp != target)
		  emit_move_insn (target, temp);
	      }
	  }
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* Expand one gimple statement STMT and return the last RTL instruction
   before any of the newly generated ones.

   In addition to generating the necessary RTL instructions this also
   sets REG_EH_REGION notes if necessary and sets the current source
   location for diagnostics.  */

static rtx_insn *
expand_gimple_stmt (gimple stmt)
{
  location_t saved_location = input_location;
  rtx_insn *last = get_last_insn ();
  int lp_nr;

  gcc_assert (cfun);

  /* We need to save and restore the current source location so that errors
     discovered during expansion are emitted with the right location.  But
     it would be better if the diagnostic routines used the source location
     embedded in the tree nodes rather than globals.  */
  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  expand_gimple_stmt_1 (stmt);

  /* Free any temporaries used to evaluate this statement.  */
  free_temp_slots ();

  input_location = saved_location;

  /* Mark all insns that may trap.  */
  lp_nr = lookup_stmt_eh_lp (stmt);
  if (lp_nr)
    {
      rtx_insn *insn;
      for (insn = next_real_insn (last); insn;
	   insn = next_real_insn (insn))
	{
	  if (! find_reg_note (insn, REG_EH_REGION, NULL_RTX)
	      /* If we want exceptions for non-call insns, any
		 may_trap_p instruction may throw.  */
	      && GET_CODE (PATTERN (insn)) != CLOBBER
	      && GET_CODE (PATTERN (insn)) != USE
	      && insn_could_throw_p (insn))
	    make_reg_eh_region_note (insn, 0, lp_nr);
	}
    }

  return last;
}

/* A subroutine of expand_gimple_basic_block.  Expand one GIMPLE_CALL
   that has CALL_EXPR_TAILCALL set.  Returns non-null if we actually
   generated a tail call (something that might be denied by the ABI
   rules governing the call; see calls.c).

   Sets CAN_FALLTHRU if we generated a *conditional* tail call, and
   can still reach the rest of BB.  The case here is __builtin_sqrt,
   where the NaN result goes through the external function (with a
   tailcall) and the normal result happens via a sqrt instruction.  */

static basic_block
expand_gimple_tailcall (basic_block bb, gcall *stmt, bool *can_fallthru)
{
  rtx_insn *last2, *last;
  edge e;
  edge_iterator ei;
  int probability;
  gcov_type count;

  last2 = last = expand_gimple_stmt (stmt);

  for (last = NEXT_INSN (last); last; last = NEXT_INSN (last))
    if (CALL_P (last) && SIBLING_CALL_P (last))
      goto found;

  maybe_dump_rtl_for_gimple_stmt (stmt, last2);

  *can_fallthru = true;
  return NULL;

 found:
  /* ??? Wouldn't it be better to just reset any pending stack adjust?
     Any instructions emitted here are about to be deleted.  */
  do_pending_stack_adjust ();

  /* Remove any non-eh, non-abnormal edges that don't go to exit.  */
  /* ??? I.e. the fallthrough edge.  HOWEVER!  If there were to be
     EH or abnormal edges, we shouldn't have created a tail call in
     the first place.  So it seems to me we should just be removing
     all edges here, or redirecting the existing fallthru edge to
     the exit block.  */

  probability = 0;
  count = 0;

  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (!(e->flags & (EDGE_ABNORMAL | EDGE_EH)))
	{
	  if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    {
	      e->dest->count -= e->count;
	      e->dest->frequency -= EDGE_FREQUENCY (e);
	      if (e->dest->count < 0)
		e->dest->count = 0;
	      if (e->dest->frequency < 0)
		e->dest->frequency = 0;
	    }
	  count += e->count;
	  probability += e->probability;
	  remove_edge (e);
	}
      else
	ei_next (&ei);
    }

  /* This is somewhat ugly: the call_expr expander often emits instructions
     after the sibcall (to perform the function return).  These confuse the
     find_many_sub_basic_blocks code, so we need to get rid of these.  */
  last = NEXT_INSN (last);
  gcc_assert (BARRIER_P (last));

  *can_fallthru = false;
  while (NEXT_INSN (last))
    {
      /* For instance an sqrt builtin expander expands if with
	 sibcall in the then and label for `else`.  */
      if (LABEL_P (NEXT_INSN (last)))
	{
	  *can_fallthru = true;
	  break;
	}
      delete_insn (NEXT_INSN (last));
    }

  e = make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_ABNORMAL
		 | EDGE_SIBCALL);
  e->probability += probability;
  e->count += count;
  BB_END (bb) = last;
  update_bb_for_insn (bb);

  if (NEXT_INSN (last))
    {
      bb = create_basic_block (NEXT_INSN (last), get_last_insn (), bb);

      last = BB_END (bb);
      if (BARRIER_P (last))
	BB_END (bb) = PREV_INSN (last);
    }

  maybe_dump_rtl_for_gimple_stmt (stmt, last2);

  return bb;
}

/* Return the difference between the floor and the truncated result of
   a signed division by OP1 with remainder MOD.  */
static rtx
floor_sdiv_adjust (machine_mode mode, rtx mod, rtx op1)
{
  /* (mod != 0 ? (op1 / mod < 0 ? -1 : 0) : 0) */
  return gen_rtx_IF_THEN_ELSE
    (mode, gen_rtx_NE (BImode, mod, const0_rtx),
     gen_rtx_IF_THEN_ELSE
     (mode, gen_rtx_LT (BImode,
			gen_rtx_DIV (mode, op1, mod),
			const0_rtx),
      constm1_rtx, const0_rtx),
     const0_rtx);
}

/* Return the difference between the ceil and the truncated result of
   a signed division by OP1 with remainder MOD.  */
static rtx
ceil_sdiv_adjust (machine_mode mode, rtx mod, rtx op1)
{
  /* (mod != 0 ? (op1 / mod > 0 ? 1 : 0) : 0) */
  return gen_rtx_IF_THEN_ELSE
    (mode, gen_rtx_NE (BImode, mod, const0_rtx),
     gen_rtx_IF_THEN_ELSE
     (mode, gen_rtx_GT (BImode,
			gen_rtx_DIV (mode, op1, mod),
			const0_rtx),
      const1_rtx, const0_rtx),
     const0_rtx);
}

/* Return the difference between the ceil and the truncated result of
   an unsigned division by OP1 with remainder MOD.  */
static rtx
ceil_udiv_adjust (machine_mode mode, rtx mod, rtx op1 ATTRIBUTE_UNUSED)
{
  /* (mod != 0 ? 1 : 0) */
  return gen_rtx_IF_THEN_ELSE
    (mode, gen_rtx_NE (BImode, mod, const0_rtx),
     const1_rtx, const0_rtx);
}

/* Return the difference between the rounded and the truncated result
   of a signed division by OP1 with remainder MOD.  Halfway cases are
   rounded away from zero, rather than to the nearest even number.  */
static rtx
round_sdiv_adjust (machine_mode mode, rtx mod, rtx op1)
{
  /* (abs (mod) >= abs (op1) - abs (mod)
      ? (op1 / mod > 0 ? 1 : -1)
      : 0) */
  return gen_rtx_IF_THEN_ELSE
    (mode, gen_rtx_GE (BImode, gen_rtx_ABS (mode, mod),
		       gen_rtx_MINUS (mode,
				      gen_rtx_ABS (mode, op1),
				      gen_rtx_ABS (mode, mod))),
     gen_rtx_IF_THEN_ELSE
     (mode, gen_rtx_GT (BImode,
			gen_rtx_DIV (mode, op1, mod),
			const0_rtx),
      const1_rtx, constm1_rtx),
     const0_rtx);
}

/* Return the difference between the rounded and the truncated result
   of a unsigned division by OP1 with remainder MOD.  Halfway cases
   are rounded away from zero, rather than to the nearest even
   number.  */
static rtx
round_udiv_adjust (machine_mode mode, rtx mod, rtx op1)
{
  /* (mod >= op1 - mod ? 1 : 0) */
  return gen_rtx_IF_THEN_ELSE
    (mode, gen_rtx_GE (BImode, mod,
		       gen_rtx_MINUS (mode, op1, mod)),
     const1_rtx, const0_rtx);
}

/* Convert X to MODE, that must be Pmode or ptr_mode, without emitting
   any rtl.  */

static rtx
convert_debug_memory_address (machine_mode mode, rtx x,
			      addr_space_t as)
{
  machine_mode xmode = GET_MODE (x);

#ifndef POINTERS_EXTEND_UNSIGNED
  gcc_assert (mode == Pmode
	      || mode == targetm.addr_space.address_mode (as));
  gcc_assert (xmode == mode || xmode == VOIDmode);
#else
  rtx temp;

  gcc_assert (targetm.addr_space.valid_pointer_mode (mode, as));

  if (GET_MODE (x) == mode || GET_MODE (x) == VOIDmode)
    return x;

  if (GET_MODE_PRECISION (mode) < GET_MODE_PRECISION (xmode))
    x = simplify_gen_subreg (mode, x, xmode,
			     subreg_lowpart_offset
			     (mode, xmode));
  else if (POINTERS_EXTEND_UNSIGNED > 0)
    x = gen_rtx_ZERO_EXTEND (mode, x);
  else if (!POINTERS_EXTEND_UNSIGNED)
    x = gen_rtx_SIGN_EXTEND (mode, x);
  else
    {
      switch (GET_CODE (x))
	{
	case SUBREG:
	  if ((SUBREG_PROMOTED_VAR_P (x)
	       || (REG_P (SUBREG_REG (x)) && REG_POINTER (SUBREG_REG (x)))
	       || (GET_CODE (SUBREG_REG (x)) == PLUS
		   && REG_P (XEXP (SUBREG_REG (x), 0))
		   && REG_POINTER (XEXP (SUBREG_REG (x), 0))
		   && CONST_INT_P (XEXP (SUBREG_REG (x), 1))))
	      && GET_MODE (SUBREG_REG (x)) == mode)
	    return SUBREG_REG (x);
	  break;
	case LABEL_REF:
	  temp = gen_rtx_LABEL_REF (mode, LABEL_REF_LABEL (x));
	  LABEL_REF_NONLOCAL_P (temp) = LABEL_REF_NONLOCAL_P (x);
	  return temp;
	case SYMBOL_REF:
	  temp = shallow_copy_rtx (x);
	  PUT_MODE (temp, mode);
	  return temp;
	case CONST:
	  temp = convert_debug_memory_address (mode, XEXP (x, 0), as);
	  if (temp)
	    temp = gen_rtx_CONST (mode, temp);
	  return temp;
	case PLUS:
	case MINUS:
	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      temp = convert_debug_memory_address (mode, XEXP (x, 0), as);
	      if (temp)
		return gen_rtx_fmt_ee (GET_CODE (x), mode, temp, XEXP (x, 1));
	    }
	  break;
	default:
	  break;
	}
      /* Don't know how to express ptr_extend as operation in debug info.  */
      return NULL;
    }
#endif /* POINTERS_EXTEND_UNSIGNED */

  return x;
}

/* Map from SSA_NAMEs to corresponding DEBUG_EXPR_DECLs created
   by avoid_deep_ter_for_debug.  */

static hash_map<tree, tree> *deep_ter_debug_map;

/* Split too deep TER chains for debug stmts using debug temporaries.  */

static void
avoid_deep_ter_for_debug (gimple stmt, int depth)
{
  use_operand_p use_p;
  ssa_op_iter iter;
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      if (TREE_CODE (use) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (use))
	continue;
      gimple g = get_gimple_for_ssa_name (use);
      if (g == NULL)
	continue;
      if (depth > 6 && !stmt_ends_bb_p (g))
	{
	  if (deep_ter_debug_map == NULL)
	    deep_ter_debug_map = new hash_map<tree, tree>;

	  tree &vexpr = deep_ter_debug_map->get_or_insert (use);
	  if (vexpr != NULL)
	    continue;
	  vexpr = make_node (DEBUG_EXPR_DECL);
	  gimple def_temp = gimple_build_debug_bind (vexpr, use, g);
	  DECL_ARTIFICIAL (vexpr) = 1;
	  TREE_TYPE (vexpr) = TREE_TYPE (use);
	  DECL_MODE (vexpr) = TYPE_MODE (TREE_TYPE (use));
	  gimple_stmt_iterator gsi = gsi_for_stmt (g);
	  gsi_insert_after (&gsi, def_temp, GSI_NEW_STMT);
	  avoid_deep_ter_for_debug (def_temp, 0);
	}
      else
	avoid_deep_ter_for_debug (g, depth + 1);
    }
}

/* Return an RTX equivalent to the value of the parameter DECL.  */

static rtx
expand_debug_parm_decl (tree decl)
{
  rtx incoming = DECL_INCOMING_RTL (decl);

  if (incoming
      && GET_MODE (incoming) != BLKmode
      && ((REG_P (incoming) && HARD_REGISTER_P (incoming))
	  || (MEM_P (incoming)
	      && REG_P (XEXP (incoming, 0))
	      && HARD_REGISTER_P (XEXP (incoming, 0)))))
    {
      rtx rtl = gen_rtx_ENTRY_VALUE (GET_MODE (incoming));

#ifdef HAVE_window_save
      /* DECL_INCOMING_RTL uses the INCOMING_REGNO of parameter registers.
	 If the target machine has an explicit window save instruction, the
	 actual entry value is the corresponding OUTGOING_REGNO instead.  */
      if (REG_P (incoming)
	  && OUTGOING_REGNO (REGNO (incoming)) != REGNO (incoming))
	incoming
	  = gen_rtx_REG_offset (incoming, GET_MODE (incoming),
				OUTGOING_REGNO (REGNO (incoming)), 0);
      else if (MEM_P (incoming))
	{
	  rtx reg = XEXP (incoming, 0);
	  if (OUTGOING_REGNO (REGNO (reg)) != REGNO (reg))
	    {
	      reg = gen_raw_REG (GET_MODE (reg), OUTGOING_REGNO (REGNO (reg)));
	      incoming = replace_equiv_address_nv (incoming, reg);
	    }
	  else
	    incoming = copy_rtx (incoming);
	}
#endif

      ENTRY_VALUE_EXP (rtl) = incoming;
      return rtl;
    }

  if (incoming
      && GET_MODE (incoming) != BLKmode
      && !TREE_ADDRESSABLE (decl)
      && MEM_P (incoming)
      && (XEXP (incoming, 0) == virtual_incoming_args_rtx
	  || (GET_CODE (XEXP (incoming, 0)) == PLUS
	      && XEXP (XEXP (incoming, 0), 0) == virtual_incoming_args_rtx
	      && CONST_INT_P (XEXP (XEXP (incoming, 0), 1)))))
    return copy_rtx (incoming);

  return NULL_RTX;
}

/* Return an RTX equivalent to the value of the tree expression EXP.  */

static rtx
expand_debug_expr (tree exp)
{
  rtx op0 = NULL_RTX, op1 = NULL_RTX, op2 = NULL_RTX;
  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  machine_mode inner_mode = VOIDmode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (exp));
  addr_space_t as;

  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case tcc_expression:
      switch (TREE_CODE (exp))
	{
	case COND_EXPR:
	case DOT_PROD_EXPR:
	case SAD_EXPR:
	case WIDEN_MULT_PLUS_EXPR:
	case WIDEN_MULT_MINUS_EXPR:
	case FMA_EXPR:
	  goto ternary;

	case TRUTH_ANDIF_EXPR:
	case TRUTH_ORIF_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	  goto binary;

	case TRUTH_NOT_EXPR:
	  goto unary;

	default:
	  break;
	}
      break;

    ternary:
      op2 = expand_debug_expr (TREE_OPERAND (exp, 2));
      if (!op2)
	return NULL_RTX;
      /* Fall through.  */

    binary:
    case tcc_binary:
      op1 = expand_debug_expr (TREE_OPERAND (exp, 1));
      if (!op1)
	return NULL_RTX;
      switch (TREE_CODE (exp))
	{
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	case LROTATE_EXPR:
	case RROTATE_EXPR:
	case WIDEN_LSHIFT_EXPR:
	  /* Ensure second operand isn't wider than the first one.  */
	  inner_mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 1)));
	  if (SCALAR_INT_MODE_P (inner_mode))
	    {
	      machine_mode opmode = mode;
	      if (VECTOR_MODE_P (mode))
		opmode = GET_MODE_INNER (mode);
	      if (SCALAR_INT_MODE_P (opmode)
		  && (GET_MODE_PRECISION (opmode)
		      < GET_MODE_PRECISION (inner_mode)))
		op1 = simplify_gen_subreg (opmode, op1, inner_mode,
					   subreg_lowpart_offset (opmode,
								  inner_mode));
	    }
	  break;
	default:
	  break;
	}
      /* Fall through.  */

    unary:
    case tcc_unary:
      inner_mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      op0 = expand_debug_expr (TREE_OPERAND (exp, 0));
      if (!op0)
	return NULL_RTX;
      break;

    case tcc_comparison:
      unsignedp = TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)));
      goto binary;

    case tcc_type:
    case tcc_statement:
      gcc_unreachable ();

    case tcc_constant:
    case tcc_exceptional:
    case tcc_declaration:
    case tcc_reference:
    case tcc_vl_exp:
      break;
    }

  switch (TREE_CODE (exp))
    {
    case STRING_CST:
      if (!lookup_constant_def (exp))
	{
	  if (strlen (TREE_STRING_POINTER (exp)) + 1
	      != (size_t) TREE_STRING_LENGTH (exp))
	    return NULL_RTX;
	  op0 = gen_rtx_CONST_STRING (Pmode, TREE_STRING_POINTER (exp));
	  op0 = gen_rtx_MEM (BLKmode, op0);
	  set_mem_attributes (op0, exp, 0);
	  return op0;
	}
      /* Fall through...  */

    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
      op0 = expand_expr (exp, NULL_RTX, mode, EXPAND_INITIALIZER);
      return op0;

    case COMPLEX_CST:
      gcc_assert (COMPLEX_MODE_P (mode));
      op0 = expand_debug_expr (TREE_REALPART (exp));
      op1 = expand_debug_expr (TREE_IMAGPART (exp));
      return gen_rtx_CONCAT (mode, op0, op1);

    case DEBUG_EXPR_DECL:
      op0 = DECL_RTL_IF_SET (exp);

      if (op0)
	return op0;

      op0 = gen_rtx_DEBUG_EXPR (mode);
      DEBUG_EXPR_TREE_DECL (op0) = exp;
      SET_DECL_RTL (exp, op0);

      return op0;

    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
    case LABEL_DECL:
    case CONST_DECL:
    case RESULT_DECL:
      op0 = DECL_RTL_IF_SET (exp);

      /* This decl was probably optimized away.  */
      if (!op0)
	{
	  if (TREE_CODE (exp) != VAR_DECL
	      || DECL_EXTERNAL (exp)
	      || !TREE_STATIC (exp)
	      || !DECL_NAME (exp)
	      || DECL_HARD_REGISTER (exp)
	      || DECL_IN_CONSTANT_POOL (exp)
	      || mode == VOIDmode)
	    return NULL;

	  op0 = make_decl_rtl_for_debug (exp);
	  if (!MEM_P (op0)
	      || GET_CODE (XEXP (op0, 0)) != SYMBOL_REF
	      || SYMBOL_REF_DECL (XEXP (op0, 0)) != exp)
	    return NULL;
	}
      else
	op0 = copy_rtx (op0);

      if (GET_MODE (op0) == BLKmode
	  /* If op0 is not BLKmode, but mode is, adjust_mode
	     below would ICE.  While it is likely a FE bug,
	     try to be robust here.  See PR43166.  */
	  || mode == BLKmode
	  || (mode == VOIDmode && GET_MODE (op0) != VOIDmode))
	{
	  gcc_assert (MEM_P (op0));
	  op0 = adjust_address_nv (op0, mode, 0);
	  return op0;
	}

      /* Fall through.  */

    adjust_mode:
    case PAREN_EXPR:
    CASE_CONVERT:
      {
	inner_mode = GET_MODE (op0);

	if (mode == inner_mode)
	  return op0;

	if (inner_mode == VOIDmode)
	  {
	    if (TREE_CODE (exp) == SSA_NAME)
	      inner_mode = TYPE_MODE (TREE_TYPE (exp));
	    else
	      inner_mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
	    if (mode == inner_mode)
	      return op0;
	  }

	if (FLOAT_MODE_P (mode) && FLOAT_MODE_P (inner_mode))
	  {
	    if (GET_MODE_BITSIZE (mode) == GET_MODE_BITSIZE (inner_mode))
	      op0 = simplify_gen_subreg (mode, op0, inner_mode, 0);
	    else if (GET_MODE_BITSIZE (mode) < GET_MODE_BITSIZE (inner_mode))
	      op0 = simplify_gen_unary (FLOAT_TRUNCATE, mode, op0, inner_mode);
	    else
	      op0 = simplify_gen_unary (FLOAT_EXTEND, mode, op0, inner_mode);
	  }
	else if (FLOAT_MODE_P (mode))
	  {
	    gcc_assert (TREE_CODE (exp) != SSA_NAME);
	    if (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))))
	      op0 = simplify_gen_unary (UNSIGNED_FLOAT, mode, op0, inner_mode);
	    else
	      op0 = simplify_gen_unary (FLOAT, mode, op0, inner_mode);
	  }
	else if (FLOAT_MODE_P (inner_mode))
	  {
	    if (unsignedp)
	      op0 = simplify_gen_unary (UNSIGNED_FIX, mode, op0, inner_mode);
	    else
	      op0 = simplify_gen_unary (FIX, mode, op0, inner_mode);
	  }
	else if (CONSTANT_P (op0)
		 || GET_MODE_PRECISION (mode) <= GET_MODE_PRECISION (inner_mode))
	  op0 = simplify_gen_subreg (mode, op0, inner_mode,
				     subreg_lowpart_offset (mode,
							    inner_mode));
	else if (UNARY_CLASS_P (exp)
		 ? TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)))
		 : unsignedp)
	  op0 = simplify_gen_unary (ZERO_EXTEND, mode, op0, inner_mode);
	else
	  op0 = simplify_gen_unary (SIGN_EXTEND, mode, op0, inner_mode);

	return op0;
      }

    case MEM_REF:
      if (!is_gimple_mem_ref_addr (TREE_OPERAND (exp, 0)))
	{
	  tree newexp = fold_binary (MEM_REF, TREE_TYPE (exp),
				     TREE_OPERAND (exp, 0),
				     TREE_OPERAND (exp, 1));
	  if (newexp)
	    return expand_debug_expr (newexp);
	}
      /* FALLTHROUGH */
    case INDIRECT_REF:
      inner_mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      op0 = expand_debug_expr (TREE_OPERAND (exp, 0));
      if (!op0)
	return NULL;

      if (TREE_CODE (exp) == MEM_REF)
	{
	  if (GET_CODE (op0) == DEBUG_IMPLICIT_PTR
	      || (GET_CODE (op0) == PLUS
		  && GET_CODE (XEXP (op0, 0)) == DEBUG_IMPLICIT_PTR))
	    /* (mem (debug_implicit_ptr)) might confuse aliasing.
	       Instead just use get_inner_reference.  */
	    goto component_ref;

	  op1 = expand_debug_expr (TREE_OPERAND (exp, 1));
	  if (!op1 || !CONST_INT_P (op1))
	    return NULL;

	  op0 = plus_constant (inner_mode, op0, INTVAL (op1));
	}

      as = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))));

      op0 = convert_debug_memory_address (targetm.addr_space.address_mode (as),
					  op0, as);
      if (op0 == NULL_RTX)
	return NULL;

      op0 = gen_rtx_MEM (mode, op0);
      set_mem_attributes (op0, exp, 0);
      if (TREE_CODE (exp) == MEM_REF
	  && !is_gimple_mem_ref_addr (TREE_OPERAND (exp, 0)))
	set_mem_expr (op0, NULL_TREE);
      set_mem_addr_space (op0, as);

      return op0;

    case TARGET_MEM_REF:
      if (TREE_CODE (TMR_BASE (exp)) == ADDR_EXPR
	  && !DECL_RTL_SET_P (TREE_OPERAND (TMR_BASE (exp), 0)))
	return NULL;

      op0 = expand_debug_expr
	    (tree_mem_ref_addr (build_pointer_type (TREE_TYPE (exp)), exp));
      if (!op0)
	return NULL;

      as = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))));
      op0 = convert_debug_memory_address (targetm.addr_space.address_mode (as),
					  op0, as);
      if (op0 == NULL_RTX)
	return NULL;

      op0 = gen_rtx_MEM (mode, op0);

      set_mem_attributes (op0, exp, 0);
      set_mem_addr_space (op0, as);

      return op0;

    component_ref:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	machine_mode mode1;
	HOST_WIDE_INT bitsize, bitpos;
	tree offset;
	int volatilep = 0;
	tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					&mode1, &unsignedp, &volatilep, false);
	rtx orig_op0;

	if (bitsize == 0)
	  return NULL;

	orig_op0 = op0 = expand_debug_expr (tem);

	if (!op0)
	  return NULL;

	if (offset)
	  {
	    machine_mode addrmode, offmode;

	    if (!MEM_P (op0))
	      return NULL;

	    op0 = XEXP (op0, 0);
	    addrmode = GET_MODE (op0);
	    if (addrmode == VOIDmode)
	      addrmode = Pmode;

	    op1 = expand_debug_expr (offset);
	    if (!op1)
	      return NULL;

	    offmode = GET_MODE (op1);
	    if (offmode == VOIDmode)
	      offmode = TYPE_MODE (TREE_TYPE (offset));

	    if (addrmode != offmode)
	      op1 = simplify_gen_subreg (addrmode, op1, offmode,
					 subreg_lowpart_offset (addrmode,
								offmode));

	    /* Don't use offset_address here, we don't need a
	       recognizable address, and we don't want to generate
	       code.  */
	    op0 = gen_rtx_MEM (mode, simplify_gen_binary (PLUS, addrmode,
							  op0, op1));
	  }

	if (MEM_P (op0))
	  {
	    if (mode1 == VOIDmode)
	      /* Bitfield.  */
	      mode1 = smallest_mode_for_size (bitsize, MODE_INT);
	    if (bitpos >= BITS_PER_UNIT)
	      {
		op0 = adjust_address_nv (op0, mode1, bitpos / BITS_PER_UNIT);
		bitpos %= BITS_PER_UNIT;
	      }
	    else if (bitpos < 0)
	      {
		HOST_WIDE_INT units
		  = (-bitpos + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
		op0 = adjust_address_nv (op0, mode1, units);
		bitpos += units * BITS_PER_UNIT;
	      }
	    else if (bitpos == 0 && bitsize == GET_MODE_BITSIZE (mode))
	      op0 = adjust_address_nv (op0, mode, 0);
	    else if (GET_MODE (op0) != mode1)
	      op0 = adjust_address_nv (op0, mode1, 0);
	    else
	      op0 = copy_rtx (op0);
	    if (op0 == orig_op0)
	      op0 = shallow_copy_rtx (op0);
	    set_mem_attributes (op0, exp, 0);
	  }

	if (bitpos == 0 && mode == GET_MODE (op0))
	  return op0;

        if (bitpos < 0)
          return NULL;

	if (GET_MODE (op0) == BLKmode)
	  return NULL;

	if ((bitpos % BITS_PER_UNIT) == 0
	    && bitsize == GET_MODE_BITSIZE (mode1))
	  {
	    machine_mode opmode = GET_MODE (op0);

	    if (opmode == VOIDmode)
	      opmode = TYPE_MODE (TREE_TYPE (tem));

	    /* This condition may hold if we're expanding the address
	       right past the end of an array that turned out not to
	       be addressable (i.e., the address was only computed in
	       debug stmts).  The gen_subreg below would rightfully
	       crash, and the address doesn't really exist, so just
	       drop it.  */
	    if (bitpos >= GET_MODE_BITSIZE (opmode))
	      return NULL;

	    if ((bitpos % GET_MODE_BITSIZE (mode)) == 0)
	      return simplify_gen_subreg (mode, op0, opmode,
					  bitpos / BITS_PER_UNIT);
	  }

	return simplify_gen_ternary (SCALAR_INT_MODE_P (GET_MODE (op0))
				     && TYPE_UNSIGNED (TREE_TYPE (exp))
				     ? SIGN_EXTRACT
				     : ZERO_EXTRACT, mode,
				     GET_MODE (op0) != VOIDmode
				     ? GET_MODE (op0)
				     : TYPE_MODE (TREE_TYPE (tem)),
				     op0, GEN_INT (bitsize), GEN_INT (bitpos));
      }

    case ABS_EXPR:
      return simplify_gen_unary (ABS, mode, op0, mode);

    case NEGATE_EXPR:
      return simplify_gen_unary (NEG, mode, op0, mode);

    case BIT_NOT_EXPR:
      return simplify_gen_unary (NOT, mode, op0, mode);

    case FLOAT_EXPR:
      return simplify_gen_unary (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp,
									 0)))
				 ? UNSIGNED_FLOAT : FLOAT, mode, op0,
				 inner_mode);

    case FIX_TRUNC_EXPR:
      return simplify_gen_unary (unsignedp ? UNSIGNED_FIX : FIX, mode, op0,
				 inner_mode);

    case POINTER_PLUS_EXPR:
      /* For the rare target where pointers are not the same size as
	 size_t, we need to check for mis-matched modes and correct
	 the addend.  */
      if (op0 && op1
	  && GET_MODE (op0) != VOIDmode && GET_MODE (op1) != VOIDmode
	  && GET_MODE (op0) != GET_MODE (op1))
	{
	  if (GET_MODE_BITSIZE (GET_MODE (op0)) < GET_MODE_BITSIZE (GET_MODE (op1))
	      /* If OP0 is a partial mode, then we must truncate, even if it has
		 the same bitsize as OP1 as GCC's representation of partial modes
		 is opaque.  */
	      || (GET_MODE_CLASS (GET_MODE (op0)) == MODE_PARTIAL_INT
		  && GET_MODE_BITSIZE (GET_MODE (op0)) == GET_MODE_BITSIZE (GET_MODE (op1))))
	    op1 = simplify_gen_unary (TRUNCATE, GET_MODE (op0), op1,
				      GET_MODE (op1));
	  else
	    /* We always sign-extend, regardless of the signedness of
	       the operand, because the operand is always unsigned
	       here even if the original C expression is signed.  */
	    op1 = simplify_gen_unary (SIGN_EXTEND, GET_MODE (op0), op1,
				      GET_MODE (op1));
	}
      /* Fall through.  */
    case PLUS_EXPR:
      return simplify_gen_binary (PLUS, mode, op0, op1);

    case MINUS_EXPR:
      return simplify_gen_binary (MINUS, mode, op0, op1);

    case MULT_EXPR:
      return simplify_gen_binary (MULT, mode, op0, op1);

    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (unsignedp)
	return simplify_gen_binary (UDIV, mode, op0, op1);
      else
	return simplify_gen_binary (DIV, mode, op0, op1);

    case TRUNC_MOD_EXPR:
      return simplify_gen_binary (unsignedp ? UMOD : MOD, mode, op0, op1);

    case FLOOR_DIV_EXPR:
      if (unsignedp)
	return simplify_gen_binary (UDIV, mode, op0, op1);
      else
	{
	  rtx div = simplify_gen_binary (DIV, mode, op0, op1);
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = floor_sdiv_adjust (mode, mod, op1);
	  return simplify_gen_binary (PLUS, mode, div, adj);
	}

    case FLOOR_MOD_EXPR:
      if (unsignedp)
	return simplify_gen_binary (UMOD, mode, op0, op1);
      else
	{
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = floor_sdiv_adjust (mode, mod, op1);
	  adj = simplify_gen_unary (NEG, mode,
				    simplify_gen_binary (MULT, mode, adj, op1),
				    mode);
	  return simplify_gen_binary (PLUS, mode, mod, adj);
	}

    case CEIL_DIV_EXPR:
      if (unsignedp)
	{
	  rtx div = simplify_gen_binary (UDIV, mode, op0, op1);
	  rtx mod = simplify_gen_binary (UMOD, mode, op0, op1);
	  rtx adj = ceil_udiv_adjust (mode, mod, op1);
	  return simplify_gen_binary (PLUS, mode, div, adj);
	}
      else
	{
	  rtx div = simplify_gen_binary (DIV, mode, op0, op1);
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = ceil_sdiv_adjust (mode, mod, op1);
	  return simplify_gen_binary (PLUS, mode, div, adj);
	}

    case CEIL_MOD_EXPR:
      if (unsignedp)
	{
	  rtx mod = simplify_gen_binary (UMOD, mode, op0, op1);
	  rtx adj = ceil_udiv_adjust (mode, mod, op1);
	  adj = simplify_gen_unary (NEG, mode,
				    simplify_gen_binary (MULT, mode, adj, op1),
				    mode);
	  return simplify_gen_binary (PLUS, mode, mod, adj);
	}
      else
	{
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = ceil_sdiv_adjust (mode, mod, op1);
	  adj = simplify_gen_unary (NEG, mode,
				    simplify_gen_binary (MULT, mode, adj, op1),
				    mode);
	  return simplify_gen_binary (PLUS, mode, mod, adj);
	}

    case ROUND_DIV_EXPR:
      if (unsignedp)
	{
	  rtx div = simplify_gen_binary (UDIV, mode, op0, op1);
	  rtx mod = simplify_gen_binary (UMOD, mode, op0, op1);
	  rtx adj = round_udiv_adjust (mode, mod, op1);
	  return simplify_gen_binary (PLUS, mode, div, adj);
	}
      else
	{
	  rtx div = simplify_gen_binary (DIV, mode, op0, op1);
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = round_sdiv_adjust (mode, mod, op1);
	  return simplify_gen_binary (PLUS, mode, div, adj);
	}

    case ROUND_MOD_EXPR:
      if (unsignedp)
	{
	  rtx mod = simplify_gen_binary (UMOD, mode, op0, op1);
	  rtx adj = round_udiv_adjust (mode, mod, op1);
	  adj = simplify_gen_unary (NEG, mode,
				    simplify_gen_binary (MULT, mode, adj, op1),
				    mode);
	  return simplify_gen_binary (PLUS, mode, mod, adj);
	}
      else
	{
	  rtx mod = simplify_gen_binary (MOD, mode, op0, op1);
	  rtx adj = round_sdiv_adjust (mode, mod, op1);
	  adj = simplify_gen_unary (NEG, mode,
				    simplify_gen_binary (MULT, mode, adj, op1),
				    mode);
	  return simplify_gen_binary (PLUS, mode, mod, adj);
	}

    case LSHIFT_EXPR:
      return simplify_gen_binary (ASHIFT, mode, op0, op1);

    case RSHIFT_EXPR:
      if (unsignedp)
	return simplify_gen_binary (LSHIFTRT, mode, op0, op1);
      else
	return simplify_gen_binary (ASHIFTRT, mode, op0, op1);

    case LROTATE_EXPR:
      return simplify_gen_binary (ROTATE, mode, op0, op1);

    case RROTATE_EXPR:
      return simplify_gen_binary (ROTATERT, mode, op0, op1);

    case MIN_EXPR:
      return simplify_gen_binary (unsignedp ? UMIN : SMIN, mode, op0, op1);

    case MAX_EXPR:
      return simplify_gen_binary (unsignedp ? UMAX : SMAX, mode, op0, op1);

    case BIT_AND_EXPR:
    case TRUTH_AND_EXPR:
      return simplify_gen_binary (AND, mode, op0, op1);

    case BIT_IOR_EXPR:
    case TRUTH_OR_EXPR:
      return simplify_gen_binary (IOR, mode, op0, op1);

    case BIT_XOR_EXPR:
    case TRUTH_XOR_EXPR:
      return simplify_gen_binary (XOR, mode, op0, op1);

    case TRUTH_ANDIF_EXPR:
      return gen_rtx_IF_THEN_ELSE (mode, op0, op1, const0_rtx);

    case TRUTH_ORIF_EXPR:
      return gen_rtx_IF_THEN_ELSE (mode, op0, const_true_rtx, op1);

    case TRUTH_NOT_EXPR:
      return simplify_gen_relational (EQ, mode, inner_mode, op0, const0_rtx);

    case LT_EXPR:
      return simplify_gen_relational (unsignedp ? LTU : LT, mode, inner_mode,
				      op0, op1);

    case LE_EXPR:
      return simplify_gen_relational (unsignedp ? LEU : LE, mode, inner_mode,
				      op0, op1);

    case GT_EXPR:
      return simplify_gen_relational (unsignedp ? GTU : GT, mode, inner_mode,
				      op0, op1);

    case GE_EXPR:
      return simplify_gen_relational (unsignedp ? GEU : GE, mode, inner_mode,
				      op0, op1);

    case EQ_EXPR:
      return simplify_gen_relational (EQ, mode, inner_mode, op0, op1);

    case NE_EXPR:
      return simplify_gen_relational (NE, mode, inner_mode, op0, op1);

    case UNORDERED_EXPR:
      return simplify_gen_relational (UNORDERED, mode, inner_mode, op0, op1);

    case ORDERED_EXPR:
      return simplify_gen_relational (ORDERED, mode, inner_mode, op0, op1);

    case UNLT_EXPR:
      return simplify_gen_relational (UNLT, mode, inner_mode, op0, op1);

    case UNLE_EXPR:
      return simplify_gen_relational (UNLE, mode, inner_mode, op0, op1);

    case UNGT_EXPR:
      return simplify_gen_relational (UNGT, mode, inner_mode, op0, op1);

    case UNGE_EXPR:
      return simplify_gen_relational (UNGE, mode, inner_mode, op0, op1);

    case UNEQ_EXPR:
      return simplify_gen_relational (UNEQ, mode, inner_mode, op0, op1);

    case LTGT_EXPR:
      return simplify_gen_relational (LTGT, mode, inner_mode, op0, op1);

    case COND_EXPR:
      return gen_rtx_IF_THEN_ELSE (mode, op0, op1, op2);

    case COMPLEX_EXPR:
      gcc_assert (COMPLEX_MODE_P (mode));
      if (GET_MODE (op0) == VOIDmode)
	op0 = gen_rtx_CONST (GET_MODE_INNER (mode), op0);
      if (GET_MODE (op1) == VOIDmode)
	op1 = gen_rtx_CONST (GET_MODE_INNER (mode), op1);
      return gen_rtx_CONCAT (mode, op0, op1);

    case CONJ_EXPR:
      if (GET_CODE (op0) == CONCAT)
	return gen_rtx_CONCAT (mode, XEXP (op0, 0),
			       simplify_gen_unary (NEG, GET_MODE_INNER (mode),
						   XEXP (op0, 1),
						   GET_MODE_INNER (mode)));
      else
	{
	  machine_mode imode = GET_MODE_INNER (mode);
	  rtx re, im;

	  if (MEM_P (op0))
	    {
	      re = adjust_address_nv (op0, imode, 0);
	      im = adjust_address_nv (op0, imode, GET_MODE_SIZE (imode));
	    }
	  else
	    {
	      machine_mode ifmode = int_mode_for_mode (mode);
	      machine_mode ihmode = int_mode_for_mode (imode);
	      rtx halfsize;
	      if (ifmode == BLKmode || ihmode == BLKmode)
		return NULL;
	      halfsize = GEN_INT (GET_MODE_BITSIZE (ihmode));
	      re = op0;
	      if (mode != ifmode)
		re = gen_rtx_SUBREG (ifmode, re, 0);
	      re = gen_rtx_ZERO_EXTRACT (ihmode, re, halfsize, const0_rtx);
	      if (imode != ihmode)
		re = gen_rtx_SUBREG (imode, re, 0);
	      im = copy_rtx (op0);
	      if (mode != ifmode)
		im = gen_rtx_SUBREG (ifmode, im, 0);
	      im = gen_rtx_ZERO_EXTRACT (ihmode, im, halfsize, halfsize);
	      if (imode != ihmode)
		im = gen_rtx_SUBREG (imode, im, 0);
	    }
	  im = gen_rtx_NEG (imode, im);
	  return gen_rtx_CONCAT (mode, re, im);
	}

    case ADDR_EXPR:
      op0 = expand_debug_expr (TREE_OPERAND (exp, 0));
      if (!op0 || !MEM_P (op0))
	{
	  if ((TREE_CODE (TREE_OPERAND (exp, 0)) == VAR_DECL
	       || TREE_CODE (TREE_OPERAND (exp, 0)) == PARM_DECL
	       || TREE_CODE (TREE_OPERAND (exp, 0)) == RESULT_DECL)
	      && (!TREE_ADDRESSABLE (TREE_OPERAND (exp, 0))
		  || target_for_debug_bind (TREE_OPERAND (exp, 0))))
	    return gen_rtx_DEBUG_IMPLICIT_PTR (mode, TREE_OPERAND (exp, 0));

	  if (handled_component_p (TREE_OPERAND (exp, 0)))
	    {
	      HOST_WIDE_INT bitoffset, bitsize, maxsize;
	      tree decl
		= get_ref_base_and_extent (TREE_OPERAND (exp, 0),
					   &bitoffset, &bitsize, &maxsize);
	      if ((TREE_CODE (decl) == VAR_DECL
		   || TREE_CODE (decl) == PARM_DECL
		   || TREE_CODE (decl) == RESULT_DECL)
		  && (!TREE_ADDRESSABLE (decl)
		      || target_for_debug_bind (decl))
		  && (bitoffset % BITS_PER_UNIT) == 0
		  && bitsize > 0
		  && bitsize == maxsize)
		{
		  rtx base = gen_rtx_DEBUG_IMPLICIT_PTR (mode, decl);
		  return plus_constant (mode, base, bitoffset / BITS_PER_UNIT);
		}
	    }

	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == MEM_REF
	      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		 == ADDR_EXPR)
	    {
	      op0 = expand_debug_expr (TREE_OPERAND (TREE_OPERAND (exp, 0),
						     0));
	      if (op0 != NULL
		  && (GET_CODE (op0) == DEBUG_IMPLICIT_PTR
		      || (GET_CODE (op0) == PLUS
			  && GET_CODE (XEXP (op0, 0)) == DEBUG_IMPLICIT_PTR
			  && CONST_INT_P (XEXP (op0, 1)))))
		{
		  op1 = expand_debug_expr (TREE_OPERAND (TREE_OPERAND (exp, 0),
							 1));
		  if (!op1 || !CONST_INT_P (op1))
		    return NULL;

		  return plus_constant (mode, op0, INTVAL (op1));
		}
	    }

	  return NULL;
	}

      as = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (exp)));
      op0 = convert_debug_memory_address (mode, XEXP (op0, 0), as);

      return op0;

    case VECTOR_CST:
      {
	unsigned i;

	op0 = gen_rtx_CONCATN
	  (mode, rtvec_alloc (TYPE_VECTOR_SUBPARTS (TREE_TYPE (exp))));

	for (i = 0; i < VECTOR_CST_NELTS (exp); ++i)
	  {
	    op1 = expand_debug_expr (VECTOR_CST_ELT (exp, i));
	    if (!op1)
	      return NULL;
	    XVECEXP (op0, 0, i) = op1;
	  }

	return op0;
      }

    case CONSTRUCTOR:
      if (TREE_CLOBBER_P (exp))
	return NULL;
      else if (TREE_CODE (TREE_TYPE (exp)) == VECTOR_TYPE)
	{
	  unsigned i;
	  tree val;

	  op0 = gen_rtx_CONCATN
	    (mode, rtvec_alloc (TYPE_VECTOR_SUBPARTS (TREE_TYPE (exp))));

	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), i, val)
	    {
	      op1 = expand_debug_expr (val);
	      if (!op1)
		return NULL;
	      XVECEXP (op0, 0, i) = op1;
	    }

	  if (i < TYPE_VECTOR_SUBPARTS (TREE_TYPE (exp)))
	    {
	      op1 = expand_debug_expr
		(build_zero_cst (TREE_TYPE (TREE_TYPE (exp))));

	      if (!op1)
		return NULL;

	      for (; i < TYPE_VECTOR_SUBPARTS (TREE_TYPE (exp)); i++)
		XVECEXP (op0, 0, i) = op1;
	    }

	  return op0;
	}
      else
	goto flag_unsupported;

    case CALL_EXPR:
      /* ??? Maybe handle some builtins?  */
      return NULL;

    case SSA_NAME:
      {
	gimple g = get_gimple_for_ssa_name (exp);
	if (g)
	  {
	    tree t = NULL_TREE;
	    if (deep_ter_debug_map)
	      {
		tree *slot = deep_ter_debug_map->get (exp);
		if (slot)
		  t = *slot;
	      }
	    if (t == NULL_TREE)
	      t = gimple_assign_rhs_to_tree (g);
	    op0 = expand_debug_expr (t);
	    if (!op0)
	      return NULL;
	  }
	else
	  {
	    int part = var_to_partition (SA.map, exp);

	    if (part == NO_PARTITION)
	      {
		/* If this is a reference to an incoming value of parameter
		   that is never used in the code or where the incoming
		   value is never used in the code, use PARM_DECL's
		   DECL_RTL if set.  */
		if (SSA_NAME_IS_DEFAULT_DEF (exp)
		    && TREE_CODE (SSA_NAME_VAR (exp)) == PARM_DECL)
		  {
		    op0 = expand_debug_parm_decl (SSA_NAME_VAR (exp));
		    if (op0)
		      goto adjust_mode;
		    op0 = expand_debug_expr (SSA_NAME_VAR (exp));
		    if (op0)
		      goto adjust_mode;
		  }
		return NULL;
	      }

	    gcc_assert (part >= 0 && (unsigned)part < SA.map->num_partitions);

	    op0 = copy_rtx (SA.partition_to_pseudo[part]);
	  }
	goto adjust_mode;
      }

    case ERROR_MARK:
      return NULL;

    /* Vector stuff.  For most of the codes we don't have rtl codes.  */
    case REALIGN_LOAD_EXPR:
    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
    case VEC_COND_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
    case VEC_PERM_EXPR:
      return NULL;

    /* Misc codes.  */
    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case OBJ_TYPE_REF:
    case WITH_SIZE_EXPR:
      return NULL;

    case DOT_PROD_EXPR:
      if (SCALAR_INT_MODE_P (GET_MODE (op0))
	  && SCALAR_INT_MODE_P (mode))
	{
	  op0
	    = simplify_gen_unary (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp,
									  0)))
				  ? ZERO_EXTEND : SIGN_EXTEND, mode, op0,
				  inner_mode);
	  op1
	    = simplify_gen_unary (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp,
									  1)))
				  ? ZERO_EXTEND : SIGN_EXTEND, mode, op1,
				  inner_mode);
	  op0 = simplify_gen_binary (MULT, mode, op0, op1);
	  return simplify_gen_binary (PLUS, mode, op0, op2);
	}
      return NULL;

    case WIDEN_MULT_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
      if (SCALAR_INT_MODE_P (GET_MODE (op0))
	  && SCALAR_INT_MODE_P (mode))
	{
	  inner_mode = GET_MODE (op0);
	  if (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))))
	    op0 = simplify_gen_unary (ZERO_EXTEND, mode, op0, inner_mode);
	  else
	    op0 = simplify_gen_unary (SIGN_EXTEND, mode, op0, inner_mode);
	  if (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1))))
	    op1 = simplify_gen_unary (ZERO_EXTEND, mode, op1, inner_mode);
	  else
	    op1 = simplify_gen_unary (SIGN_EXTEND, mode, op1, inner_mode);
	  op0 = simplify_gen_binary (MULT, mode, op0, op1);
	  if (TREE_CODE (exp) == WIDEN_MULT_EXPR)
	    return op0;
	  else if (TREE_CODE (exp) == WIDEN_MULT_PLUS_EXPR)
	    return simplify_gen_binary (PLUS, mode, op0, op2);
	  else
	    return simplify_gen_binary (MINUS, mode, op2, op0);
	}
      return NULL;

    case MULT_HIGHPART_EXPR:
      /* ??? Similar to the above.  */
      return NULL;

    case WIDEN_SUM_EXPR:
    case WIDEN_LSHIFT_EXPR:
      if (SCALAR_INT_MODE_P (GET_MODE (op0))
	  && SCALAR_INT_MODE_P (mode))
	{
	  op0
	    = simplify_gen_unary (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp,
									  0)))
				  ? ZERO_EXTEND : SIGN_EXTEND, mode, op0,
				  inner_mode);
	  return simplify_gen_binary (TREE_CODE (exp) == WIDEN_LSHIFT_EXPR
				      ? ASHIFT : PLUS, mode, op0, op1);
	}
      return NULL;

    case FMA_EXPR:
      return simplify_gen_ternary (FMA, mode, inner_mode, op0, op1, op2);

    default:
    flag_unsupported:
#ifdef ENABLE_CHECKING
      debug_tree (exp);
      gcc_unreachable ();
#else
      return NULL;
#endif
    }
}

/* Return an RTX equivalent to the source bind value of the tree expression
   EXP.  */

static rtx
expand_debug_source_expr (tree exp)
{
  rtx op0 = NULL_RTX;
  machine_mode mode = VOIDmode, inner_mode;

  switch (TREE_CODE (exp))
    {
    case PARM_DECL:
      {
	mode = DECL_MODE (exp);
	op0 = expand_debug_parm_decl (exp);
	if (op0)
	   break;
	/* See if this isn't an argument that has been completely
	   optimized out.  */
	if (!DECL_RTL_SET_P (exp)
	    && !DECL_INCOMING_RTL (exp)
	    && DECL_ABSTRACT_ORIGIN (current_function_decl))
	  {
	    tree aexp = DECL_ORIGIN (exp);
	    if (DECL_CONTEXT (aexp)
		== DECL_ABSTRACT_ORIGIN (current_function_decl))
	      {
		vec<tree, va_gc> **debug_args;
		unsigned int ix;
		tree ddecl;
		debug_args = decl_debug_args_lookup (current_function_decl);
		if (debug_args != NULL)
		  {
		    for (ix = 0; vec_safe_iterate (*debug_args, ix, &ddecl);
			 ix += 2)
		      if (ddecl == aexp)
			return gen_rtx_DEBUG_PARAMETER_REF (mode, aexp);
		  }
	      }
	  }
	break;
      }
    default:
      break;
    }

  if (op0 == NULL_RTX)
    return NULL_RTX;

  inner_mode = GET_MODE (op0);
  if (mode == inner_mode)
    return op0;

  if (FLOAT_MODE_P (mode) && FLOAT_MODE_P (inner_mode))
    {
      if (GET_MODE_BITSIZE (mode) == GET_MODE_BITSIZE (inner_mode))
	op0 = simplify_gen_subreg (mode, op0, inner_mode, 0);
      else if (GET_MODE_BITSIZE (mode) < GET_MODE_BITSIZE (inner_mode))
	op0 = simplify_gen_unary (FLOAT_TRUNCATE, mode, op0, inner_mode);
      else
	op0 = simplify_gen_unary (FLOAT_EXTEND, mode, op0, inner_mode);
    }
  else if (FLOAT_MODE_P (mode))
    gcc_unreachable ();
  else if (FLOAT_MODE_P (inner_mode))
    {
      if (TYPE_UNSIGNED (TREE_TYPE (exp)))
	op0 = simplify_gen_unary (UNSIGNED_FIX, mode, op0, inner_mode);
      else
	op0 = simplify_gen_unary (FIX, mode, op0, inner_mode);
    }
  else if (CONSTANT_P (op0)
	   || GET_MODE_BITSIZE (mode) <= GET_MODE_BITSIZE (inner_mode))
    op0 = simplify_gen_subreg (mode, op0, inner_mode,
			       subreg_lowpart_offset (mode, inner_mode));
  else if (TYPE_UNSIGNED (TREE_TYPE (exp)))
    op0 = simplify_gen_unary (ZERO_EXTEND, mode, op0, inner_mode);
  else
    op0 = simplify_gen_unary (SIGN_EXTEND, mode, op0, inner_mode);

  return op0;
}

/* Ensure INSN_VAR_LOCATION_LOC (insn) doesn't have unbound complexity.
   Allow 4 levels of rtl nesting for most rtl codes, and if we see anything
   deeper than that, create DEBUG_EXPRs and emit DEBUG_INSNs before INSN.  */

static void
avoid_complex_debug_insns (rtx_insn *insn, rtx *exp_p, int depth)
{
  rtx exp = *exp_p;

  if (exp == NULL_RTX)
    return;

  if ((OBJECT_P (exp) && !MEM_P (exp)) || GET_CODE (exp) == CLOBBER)
    return;

  if (depth == 4)
    {
      /* Create DEBUG_EXPR (and DEBUG_EXPR_DECL).  */
      rtx dval = make_debug_expr_from_rtl (exp);

      /* Emit a debug bind insn before INSN.  */
      rtx bind = gen_rtx_VAR_LOCATION (GET_MODE (exp),
				       DEBUG_EXPR_TREE_DECL (dval), exp,
				       VAR_INIT_STATUS_INITIALIZED);

      emit_debug_insn_before (bind, insn);
      *exp_p = dval;
      return;
    }

  const char *format_ptr = GET_RTX_FORMAT (GET_CODE (exp));
  int i, j;
  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (exp)); i++)
    switch (*format_ptr++)
      {
      case 'e':
	avoid_complex_debug_insns (insn, &XEXP (exp, i), depth + 1);
	break;

      case 'E':
      case 'V':
	for (j = 0; j < XVECLEN (exp, i); j++)
	  avoid_complex_debug_insns (insn, &XVECEXP (exp, i, j), depth + 1);
	break;

      default:
	break;
      }
}

/* Expand the _LOCs in debug insns.  We run this after expanding all
   regular insns, so that any variables referenced in the function
   will have their DECL_RTLs set.  */

static void
expand_debug_locations (void)
{
  rtx_insn *insn;
  rtx_insn *last = get_last_insn ();
  int save_strict_alias = flag_strict_aliasing;

  /* New alias sets while setting up memory attributes cause
     -fcompare-debug failures, even though it doesn't bring about any
     codegen changes.  */
  flag_strict_aliasing = 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (DEBUG_INSN_P (insn))
      {
	tree value = (tree)INSN_VAR_LOCATION_LOC (insn);
	rtx val;
	rtx_insn *prev_insn, *insn2;
	machine_mode mode;

	if (value == NULL_TREE)
	  val = NULL_RTX;
	else
	  {
	    if (INSN_VAR_LOCATION_STATUS (insn)
		== VAR_INIT_STATUS_UNINITIALIZED)
	      val = expand_debug_source_expr (value);
	    /* The avoid_deep_ter_for_debug function inserts
	       debug bind stmts after SSA_NAME definition, with the
	       SSA_NAME as the whole bind location.  Disable temporarily
	       expansion of that SSA_NAME into the DEBUG_EXPR_DECL
	       being defined in this DEBUG_INSN.  */
	    else if (deep_ter_debug_map && TREE_CODE (value) == SSA_NAME)
	      {
		tree *slot = deep_ter_debug_map->get (value);
		if (slot)
		  {
		    if (*slot == INSN_VAR_LOCATION_DECL (insn))
		      *slot = NULL_TREE;
		    else
		      slot = NULL;
		  }
		val = expand_debug_expr (value);
		if (slot)
		  *slot = INSN_VAR_LOCATION_DECL (insn);
	      }
	    else
	      val = expand_debug_expr (value);
	    gcc_assert (last == get_last_insn ());
	  }

	if (!val)
	  val = gen_rtx_UNKNOWN_VAR_LOC ();
	else
	  {
	    mode = GET_MODE (INSN_VAR_LOCATION (insn));

	    gcc_assert (mode == GET_MODE (val)
			|| (GET_MODE (val) == VOIDmode
			    && (CONST_SCALAR_INT_P (val)
				|| GET_CODE (val) == CONST_FIXED
				|| GET_CODE (val) == LABEL_REF)));
	  }

	INSN_VAR_LOCATION_LOC (insn) = val;
	prev_insn = PREV_INSN (insn);
	for (insn2 = insn; insn2 != prev_insn; insn2 = PREV_INSN (insn2))
	  avoid_complex_debug_insns (insn2, &INSN_VAR_LOCATION_LOC (insn2), 0);
      }

  flag_strict_aliasing = save_strict_alias;
}

/* Performs swapping operands of commutative operations to expand
   the expensive one first.  */

static void
reorder_operands (basic_block bb)
{
  unsigned int *lattice;  /* Hold cost of each statement.  */
  unsigned int i = 0, n = 0;
  gimple_stmt_iterator gsi;
  gimple_seq stmts;
  gimple stmt;
  bool swap;
  tree op0, op1;
  ssa_op_iter iter;
  use_operand_p use_p;
  gimple def0, def1;

  /* Compute cost of each statement using estimate_num_insns.  */
  stmts = bb_seq (bb);
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt))
        gimple_set_uid (stmt, n++);
    }
  lattice = XNEWVEC (unsigned int, n);
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      unsigned cost;
      stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;
      cost = estimate_num_insns (stmt, &eni_size_weights);
      lattice[i] = cost;
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  gimple def_stmt;
	  if (TREE_CODE (use) != SSA_NAME)
	    continue;
	  def_stmt = get_gimple_for_ssa_name (use);
	  if (!def_stmt)
	    continue;
	  lattice[i] += lattice[gimple_uid (def_stmt)];
	}
      i++;
      if (!is_gimple_assign (stmt)
	  || !commutative_tree_code (gimple_assign_rhs_code (stmt)))
	continue;
      op0 = gimple_op (stmt, 1);
      op1 = gimple_op (stmt, 2);
      if (TREE_CODE (op0) != SSA_NAME
	  || TREE_CODE (op1) != SSA_NAME)
	continue;
      /* Swap operands if the second one is more expensive.  */
      def0 = get_gimple_for_ssa_name (op0);
      def1 = get_gimple_for_ssa_name (op1);
      if (!def1)
	continue;
      swap = false;
      if (!def0 || lattice[gimple_uid (def1)] > lattice[gimple_uid (def0)])
	swap = true;
      if (swap)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Swap operands in stmt:\n");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      fprintf (dump_file, "Cost left opnd=%d, right opnd=%d\n",
		       def0 ? lattice[gimple_uid (def0)] : 0,
		       lattice[gimple_uid (def1)]);
	    }
	  swap_ssa_operands (stmt, gimple_assign_rhs1_ptr (stmt),
			     gimple_assign_rhs2_ptr (stmt));
	}
    }
  XDELETE (lattice);
}

/* Expand basic block BB from GIMPLE trees to RTL.  */

static basic_block
expand_gimple_basic_block (basic_block bb, bool disable_tail_calls)
{
  gimple_stmt_iterator gsi;
  gimple_seq stmts;
  gimple stmt = NULL;
  rtx_note *note;
  rtx_insn *last;
  edge e;
  edge_iterator ei;

  if (dump_file)
    fprintf (dump_file, "\n;; Generating RTL for gimple basic block %d\n",
	     bb->index);

  /* Note that since we are now transitioning from GIMPLE to RTL, we
     cannot use the gsi_*_bb() routines because they expect the basic
     block to be in GIMPLE, instead of RTL.  Therefore, we need to
     access the BB sequence directly.  */
  if (optimize)
    reorder_operands (bb);
  stmts = bb_seq (bb);
  bb->il.gimple.seq = NULL;
  bb->il.gimple.phi_nodes = NULL;
  rtl_profile_for_bb (bb);
  init_rtl_bb_info (bb);
  bb->flags |= BB_RTL;

  /* Remove the RETURN_EXPR if we may fall though to the exit
     instead.  */
  gsi = gsi_last (stmts);
  if (!gsi_end_p (gsi)
      && gimple_code (gsi_stmt (gsi)) == GIMPLE_RETURN)
    {
      greturn *ret_stmt = as_a <greturn *> (gsi_stmt (gsi));

      gcc_assert (single_succ_p (bb));
      gcc_assert (single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun));

      if (bb->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && !gimple_return_retval (ret_stmt))
	{
	  gsi_remove (&gsi, false);
	  single_succ_edge (bb)->flags |= EDGE_FALLTHRU;
	}
    }

  gsi = gsi_start (stmts);
  if (!gsi_end_p (gsi))
    {
      stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) != GIMPLE_LABEL)
	stmt = NULL;
    }

  rtx_code_label **elt = lab_rtx_for_bb->get (bb);

  if (stmt || elt)
    {
      last = get_last_insn ();

      if (stmt)
	{
	  expand_gimple_stmt (stmt);
	  gsi_next (&gsi);
	}

      if (elt)
	emit_label (*elt);

      /* Java emits line number notes in the top of labels.
	 ??? Make this go away once line number notes are obsoleted.  */
      BB_HEAD (bb) = NEXT_INSN (last);
      if (NOTE_P (BB_HEAD (bb)))
	BB_HEAD (bb) = NEXT_INSN (BB_HEAD (bb));
      note = emit_note_after (NOTE_INSN_BASIC_BLOCK, BB_HEAD (bb));

      maybe_dump_rtl_for_gimple_stmt (stmt, last);
    }
  else
    BB_HEAD (bb) = note = emit_note (NOTE_INSN_BASIC_BLOCK);

  NOTE_BASIC_BLOCK (note) = bb;

  for (; !gsi_end_p (gsi); gsi_next (&gsi))
    {
      basic_block new_bb;

      stmt = gsi_stmt (gsi);

      /* If this statement is a non-debug one, and we generate debug
	 insns, then this one might be the last real use of a TERed
	 SSA_NAME, but where there are still some debug uses further
	 down.  Expanding the current SSA name in such further debug
	 uses by their RHS might lead to wrong debug info, as coalescing
	 might make the operands of such RHS be placed into the same
	 pseudo as something else.  Like so:
	   a_1 = a_0 + 1;   // Assume a_1 is TERed and a_0 is dead
	   use(a_1);
	   a_2 = ...
           #DEBUG ... => a_1
	 As a_0 and a_2 don't overlap in lifetime, assume they are coalesced.
	 If we now would expand a_1 by it's RHS (a_0 + 1) in the debug use,
	 the write to a_2 would actually have clobbered the place which
	 formerly held a_0.

	 So, instead of that, we recognize the situation, and generate
	 debug temporaries at the last real use of TERed SSA names:
	   a_1 = a_0 + 1;
           #DEBUG #D1 => a_1
	   use(a_1);
	   a_2 = ...
           #DEBUG ... => #D1
	 */
      if (MAY_HAVE_DEBUG_INSNS
	  && SA.values
	  && !is_gimple_debug (stmt))
	{
	  ssa_op_iter iter;
	  tree op;
	  gimple def;

	  location_t sloc = curr_insn_location ();

	  /* Look for SSA names that have their last use here (TERed
	     names always have only one real use).  */
	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	    if ((def = get_gimple_for_ssa_name (op)))
	      {
		imm_use_iterator imm_iter;
		use_operand_p use_p;
		bool have_debug_uses = false;

		FOR_EACH_IMM_USE_FAST (use_p, imm_iter, op)
		  {
		    if (gimple_debug_bind_p (USE_STMT (use_p)))
		      {
			have_debug_uses = true;
			break;
		      }
		  }

		if (have_debug_uses)
		  {
		    /* OP is a TERed SSA name, with DEF its defining
		       statement, and where OP is used in further debug
		       instructions.  Generate a debug temporary, and
		       replace all uses of OP in debug insns with that
		       temporary.  */
		    gimple debugstmt;
		    tree value = gimple_assign_rhs_to_tree (def);
		    tree vexpr = make_node (DEBUG_EXPR_DECL);
		    rtx val;
		    machine_mode mode;

		    set_curr_insn_location (gimple_location (def));

		    DECL_ARTIFICIAL (vexpr) = 1;
		    TREE_TYPE (vexpr) = TREE_TYPE (value);
		    if (DECL_P (value))
		      mode = DECL_MODE (value);
		    else
		      mode = TYPE_MODE (TREE_TYPE (value));
		    DECL_MODE (vexpr) = mode;

		    val = gen_rtx_VAR_LOCATION
			(mode, vexpr, (rtx)value, VAR_INIT_STATUS_INITIALIZED);

		    emit_debug_insn (val);

		    FOR_EACH_IMM_USE_STMT (debugstmt, imm_iter, op)
		      {
			if (!gimple_debug_bind_p (debugstmt))
			  continue;

			FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
			  SET_USE (use_p, vexpr);

			update_stmt (debugstmt);
		      }
		  }
	      }
	  set_curr_insn_location (sloc);
	}

      currently_expanding_gimple_stmt = stmt;

      /* Expand this statement, then evaluate the resulting RTL and
	 fixup the CFG accordingly.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  new_bb = expand_gimple_cond (bb, as_a <gcond *> (stmt));
	  if (new_bb)
	    return new_bb;
	}
      else if (gimple_debug_bind_p (stmt))
	{
	  location_t sloc = curr_insn_location ();
	  gimple_stmt_iterator nsi = gsi;

	  for (;;)
	    {
	      tree var = gimple_debug_bind_get_var (stmt);
	      tree value;
	      rtx val;
	      machine_mode mode;

	      if (TREE_CODE (var) != DEBUG_EXPR_DECL
		  && TREE_CODE (var) != LABEL_DECL
		  && !target_for_debug_bind (var))
		goto delink_debug_stmt;

	      if (gimple_debug_bind_has_value_p (stmt))
		value = gimple_debug_bind_get_value (stmt);
	      else
		value = NULL_TREE;

	      last = get_last_insn ();

	      set_curr_insn_location (gimple_location (stmt));

	      if (DECL_P (var))
		mode = DECL_MODE (var);
	      else
		mode = TYPE_MODE (TREE_TYPE (var));

	      val = gen_rtx_VAR_LOCATION
		(mode, var, (rtx)value, VAR_INIT_STATUS_INITIALIZED);

	      emit_debug_insn (val);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  /* We can't dump the insn with a TREE where an RTX
		     is expected.  */
		  PAT_VAR_LOCATION_LOC (val) = const0_rtx;
		  maybe_dump_rtl_for_gimple_stmt (stmt, last);
		  PAT_VAR_LOCATION_LOC (val) = (rtx)value;
		}

	    delink_debug_stmt:
	      /* In order not to generate too many debug temporaries,
	         we delink all uses of debug statements we already expanded.
		 Therefore debug statements between definition and real
		 use of TERed SSA names will continue to use the SSA name,
		 and not be replaced with debug temps.  */
	      delink_stmt_imm_use (stmt);

	      gsi = nsi;
	      gsi_next (&nsi);
	      if (gsi_end_p (nsi))
		break;
	      stmt = gsi_stmt (nsi);
	      if (!gimple_debug_bind_p (stmt))
		break;
	    }

	  set_curr_insn_location (sloc);
	}
      else if (gimple_debug_source_bind_p (stmt))
	{
	  location_t sloc = curr_insn_location ();
	  tree var = gimple_debug_source_bind_get_var (stmt);
	  tree value = gimple_debug_source_bind_get_value (stmt);
	  rtx val;
	  machine_mode mode;

	  last = get_last_insn ();

	  set_curr_insn_location (gimple_location (stmt));

	  mode = DECL_MODE (var);

	  val = gen_rtx_VAR_LOCATION (mode, var, (rtx)value,
				      VAR_INIT_STATUS_UNINITIALIZED);

	  emit_debug_insn (val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      /* We can't dump the insn with a TREE where an RTX
		 is expected.  */
	      PAT_VAR_LOCATION_LOC (val) = const0_rtx;
	      maybe_dump_rtl_for_gimple_stmt (stmt, last);
	      PAT_VAR_LOCATION_LOC (val) = (rtx)value;
	    }

	  set_curr_insn_location (sloc);
	}
      else
	{
	  gcall *call_stmt = dyn_cast <gcall *> (stmt);
	  if (call_stmt
	      && gimple_call_tail_p (call_stmt)
	      && disable_tail_calls)
	    gimple_call_set_tail (call_stmt, false);

	  if (call_stmt && gimple_call_tail_p (call_stmt))
	    {
	      bool can_fallthru;
	      new_bb = expand_gimple_tailcall (bb, call_stmt, &can_fallthru);
	      if (new_bb)
		{
		  if (can_fallthru)
		    bb = new_bb;
		  else
		    return new_bb;
		}
	    }
	  else
	    {
	      def_operand_p def_p;
	      def_p = SINGLE_SSA_DEF_OPERAND (stmt, SSA_OP_DEF);

	      if (def_p != NULL)
		{
		  /* Ignore this stmt if it is in the list of
		     replaceable expressions.  */
		  if (SA.values
		      && bitmap_bit_p (SA.values,
				       SSA_NAME_VERSION (DEF_FROM_PTR (def_p))))
		    continue;
		}
	      last = expand_gimple_stmt (stmt);
	      maybe_dump_rtl_for_gimple_stmt (stmt, last);
	    }
	}
    }

  currently_expanding_gimple_stmt = NULL;

  /* Expand implicit goto and convert goto_locus.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (e->goto_locus != UNKNOWN_LOCATION)
	set_curr_insn_location (e->goto_locus);
      if ((e->flags & EDGE_FALLTHRU) && e->dest != bb->next_bb)
	{
	  emit_jump (label_rtx_for_bb (e->dest));
	  e->flags &= ~EDGE_FALLTHRU;
	}
    }

  /* Expanded RTL can create a jump in the last instruction of block.
     This later might be assumed to be a jump to successor and break edge insertion.
     We need to insert dummy move to prevent this. PR41440. */
  if (single_succ_p (bb)
      && (single_succ_edge (bb)->flags & EDGE_FALLTHRU)
      && (last = get_last_insn ())
      && JUMP_P (last))
    {
      rtx dummy = gen_reg_rtx (SImode);
      emit_insn_after_noloc (gen_move_insn (dummy, dummy), last, NULL);
    }

  do_pending_stack_adjust ();

  /* Find the block tail.  The last insn in the block is the insn
     before a barrier and/or table jump insn.  */
  last = get_last_insn ();
  if (BARRIER_P (last))
    last = PREV_INSN (last);
  if (JUMP_TABLE_DATA_P (last))
    last = PREV_INSN (PREV_INSN (last));
  BB_END (bb) = last;

  update_bb_for_insn (bb);

  return bb;
}


/* Create a basic block for initialization code.  */

static basic_block
construct_init_block (void)
{
  basic_block init_block, first_block;
  edge e = NULL;
  int flags;

  /* Multiple entry points not supported yet.  */
  gcc_assert (EDGE_COUNT (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs) == 1);
  init_rtl_bb_info (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  init_rtl_bb_info (EXIT_BLOCK_PTR_FOR_FN (cfun));
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->flags |= BB_RTL;
  EXIT_BLOCK_PTR_FOR_FN (cfun)->flags |= BB_RTL;

  e = EDGE_SUCC (ENTRY_BLOCK_PTR_FOR_FN (cfun), 0);

  /* When entry edge points to first basic block, we don't need jump,
     otherwise we have to jump into proper target.  */
  if (e && e->dest != ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb)
    {
      tree label = gimple_block_label (e->dest);

      emit_jump (jump_target_rtx (label));
      flags = 0;
    }
  else
    flags = EDGE_FALLTHRU;

  init_block = create_basic_block (NEXT_INSN (get_insns ()),
				   get_last_insn (),
				   ENTRY_BLOCK_PTR_FOR_FN (cfun));
  init_block->frequency = ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency;
  init_block->count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  add_bb_to_loop (init_block, ENTRY_BLOCK_PTR_FOR_FN (cfun)->loop_father);
  if (e)
    {
      first_block = e->dest;
      redirect_edge_succ (e, init_block);
      e = make_edge (init_block, first_block, flags);
    }
  else
    e = make_edge (init_block, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FALLTHRU);
  e->probability = REG_BR_PROB_BASE;
  e->count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;

  update_bb_for_insn (init_block);
  return init_block;
}

/* For each lexical block, set BLOCK_NUMBER to the depth at which it is
   found in the block tree.  */

static void
set_block_levels (tree block, int level)
{
  while (block)
    {
      BLOCK_NUMBER (block) = level;
      set_block_levels (BLOCK_SUBBLOCKS (block), level + 1);
      block = BLOCK_CHAIN (block);
    }
}

/* Create a block containing landing pads and similar stuff.  */

static void
construct_exit_block (void)
{
  rtx_insn *head = get_last_insn ();
  rtx_insn *end;
  basic_block exit_block;
  edge e, e2;
  unsigned ix;
  edge_iterator ei;
  basic_block prev_bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
  rtx_insn *orig_end = BB_END (prev_bb);

  rtl_profile_for_bb (EXIT_BLOCK_PTR_FOR_FN (cfun));

  /* Make sure the locus is set to the end of the function, so that
     epilogue line numbers and warnings are set properly.  */
  if (LOCATION_LOCUS (cfun->function_end_locus) != UNKNOWN_LOCATION)
    input_location = cfun->function_end_locus;

  /* Generate rtl for function exit.  */
  expand_function_end ();

  end = get_last_insn ();
  if (head == end)
    return;
  /* While emitting the function end we could move end of the last basic
     block.  */
  BB_END (prev_bb) = orig_end;
  while (NEXT_INSN (head) && NOTE_P (NEXT_INSN (head)))
    head = NEXT_INSN (head);
  /* But make sure exit_block starts with RETURN_LABEL, otherwise the
     bb frequency counting will be confused.  Any instructions before that
     label are emitted for the case where PREV_BB falls through into the
     exit block, so append those instructions to prev_bb in that case.  */
  if (NEXT_INSN (head) != return_label)
    {
      while (NEXT_INSN (head) != return_label)
	{
	  if (!NOTE_P (NEXT_INSN (head)))
	    BB_END (prev_bb) = NEXT_INSN (head);
	  head = NEXT_INSN (head);
	}
    }
  exit_block = create_basic_block (NEXT_INSN (head), end, prev_bb);
  exit_block->frequency = EXIT_BLOCK_PTR_FOR_FN (cfun)->frequency;
  exit_block->count = EXIT_BLOCK_PTR_FOR_FN (cfun)->count;
  add_bb_to_loop (exit_block, EXIT_BLOCK_PTR_FOR_FN (cfun)->loop_father);

  ix = 0;
  while (ix < EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds))
    {
      e = EDGE_PRED (EXIT_BLOCK_PTR_FOR_FN (cfun), ix);
      if (!(e->flags & EDGE_ABNORMAL))
	redirect_edge_succ (e, exit_block);
      else
	ix++;
    }

  e = make_edge (exit_block, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FALLTHRU);
  e->probability = REG_BR_PROB_BASE;
  e->count = EXIT_BLOCK_PTR_FOR_FN (cfun)->count;
  FOR_EACH_EDGE (e2, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (e2 != e)
      {
	e->count -= e2->count;
	exit_block->count -= e2->count;
	exit_block->frequency -= EDGE_FREQUENCY (e2);
      }
  if (e->count < 0)
    e->count = 0;
  if (exit_block->count < 0)
    exit_block->count = 0;
  if (exit_block->frequency < 0)
    exit_block->frequency = 0;
  update_bb_for_insn (exit_block);
}

/* Helper function for discover_nonconstant_array_refs.
   Look for ARRAY_REF nodes with non-constant indexes and mark them
   addressable.  */

static tree
discover_nonconstant_array_refs_r (tree * tp, int *walk_subtrees,
				   void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;
  else if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
    {
      while (((TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	      && is_gimple_min_invariant (TREE_OPERAND (t, 1))
	      && (!TREE_OPERAND (t, 2)
		  || is_gimple_min_invariant (TREE_OPERAND (t, 2))))
	     || (TREE_CODE (t) == COMPONENT_REF
		 && (!TREE_OPERAND (t,2)
		     || is_gimple_min_invariant (TREE_OPERAND (t, 2))))
	     || TREE_CODE (t) == BIT_FIELD_REF
	     || TREE_CODE (t) == REALPART_EXPR
	     || TREE_CODE (t) == IMAGPART_EXPR
	     || TREE_CODE (t) == VIEW_CONVERT_EXPR
	     || CONVERT_EXPR_P (t))
	t = TREE_OPERAND (t, 0);

      if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	{
	  t = get_base_address (t);
	  if (t && DECL_P (t)
              && DECL_MODE (t) != BLKmode)
	    TREE_ADDRESSABLE (t) = 1;
	}

      *walk_subtrees = 0;
    }

  return NULL_TREE;
}

/* RTL expansion is not able to compile array references with variable
   offsets for arrays stored in single register.  Discover such
   expressions and mark variables as addressable to avoid this
   scenario.  */

static void
discover_nonconstant_array_refs (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);
	if (!is_gimple_debug (stmt))
	  walk_gimple_op (stmt, discover_nonconstant_array_refs_r, NULL);
      }
}

/* This function sets crtl->args.internal_arg_pointer to a virtual
   register if DRAP is needed.  Local register allocator will replace
   virtual_incoming_args_rtx with the virtual register.  */

static void
expand_stack_alignment (void)
{
  rtx drap_rtx;
  unsigned int preferred_stack_boundary;

  if (! SUPPORTS_STACK_ALIGNMENT)
    return;

  if (cfun->calls_alloca
      || cfun->has_nonlocal_label
      || crtl->has_nonlocal_goto)
    crtl->need_drap = true;

  /* Call update_stack_boundary here again to update incoming stack
     boundary.  It may set incoming stack alignment to a different
     value after RTL expansion.  TARGET_FUNCTION_OK_FOR_SIBCALL may
     use the minimum incoming stack alignment to check if it is OK
     to perform sibcall optimization since sibcall optimization will
     only align the outgoing stack to incoming stack boundary.  */
  if (targetm.calls.update_stack_boundary)
    targetm.calls.update_stack_boundary ();

  /* The incoming stack frame has to be aligned at least at
     parm_stack_boundary.  */
  gcc_assert (crtl->parm_stack_boundary <= INCOMING_STACK_BOUNDARY);

  /* Update crtl->stack_alignment_estimated and use it later to align
     stack.  We check PREFERRED_STACK_BOUNDARY if there may be non-call
     exceptions since callgraph doesn't collect incoming stack alignment
     in this case.  */
  if (cfun->can_throw_non_call_exceptions
      && PREFERRED_STACK_BOUNDARY > crtl->preferred_stack_boundary)
    preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;
  else
    preferred_stack_boundary = crtl->preferred_stack_boundary;
  if (preferred_stack_boundary > crtl->stack_alignment_estimated)
    crtl->stack_alignment_estimated = preferred_stack_boundary;
  if (preferred_stack_boundary > crtl->stack_alignment_needed)
    crtl->stack_alignment_needed = preferred_stack_boundary;

  gcc_assert (crtl->stack_alignment_needed
	      <= crtl->stack_alignment_estimated);

  crtl->stack_realign_needed
    = INCOMING_STACK_BOUNDARY < crtl->stack_alignment_estimated;
  crtl->stack_realign_tried = crtl->stack_realign_needed;

  crtl->stack_realign_processed = true;

  /* Target has to redefine TARGET_GET_DRAP_RTX to support stack
     alignment.  */
  gcc_assert (targetm.calls.get_drap_rtx != NULL);
  drap_rtx = targetm.calls.get_drap_rtx ();

  /* stack_realign_drap and drap_rtx must match.  */
  gcc_assert ((stack_realign_drap != 0) == (drap_rtx != NULL));

  /* Do nothing if NULL is returned, which means DRAP is not needed.  */
  if (NULL != drap_rtx)
    {
      crtl->args.internal_arg_pointer = drap_rtx;

      /* Call fixup_tail_calls to clean up REG_EQUIV note if DRAP is
         needed. */
      fixup_tail_calls ();
    }
}


static void
expand_main_function (void)
{
#if (defined(INVOKE__main)				\
     || (!defined(HAS_INIT_SECTION)			\
	 && !defined(INIT_SECTION_ASM_OP)		\
	 && !defined(INIT_ARRAY_SECTION_ASM_OP)))
  emit_library_call (init_one_libfunc (NAME__MAIN), LCT_NORMAL, VOIDmode, 0);
#endif
}


/* Expand code to initialize the stack_protect_guard.  This is invoked at
   the beginning of a function to be protected.  */

#ifndef HAVE_stack_protect_set
# define HAVE_stack_protect_set		0
# define gen_stack_protect_set(x,y)	(gcc_unreachable (), NULL_RTX)
#endif

static void
stack_protect_prologue (void)
{
  tree guard_decl = targetm.stack_protect_guard ();
  rtx x, y;

  x = expand_normal (crtl->stack_protect_guard);
  y = expand_normal (guard_decl);

  /* Allow the target to copy from Y to X without leaking Y into a
     register.  */
  if (HAVE_stack_protect_set)
    {
      rtx insn = gen_stack_protect_set (x, y);
      if (insn)
	{
	  emit_insn (insn);
	  return;
	}
    }

  /* Otherwise do a straight move.  */
  emit_move_insn (x, y);
}

/* Translate the intermediate representation contained in the CFG
   from GIMPLE trees to RTL.

   We do conversion per basic block and preserve/update the tree CFG.
   This implies we have to do some magic as the CFG can simultaneously
   consist of basic blocks containing RTL and GIMPLE trees.  This can
   confuse the CFG hooks, so be careful to not manipulate CFG during
   the expansion.  */

namespace {

const pass_data pass_data_expand =
{
  RTL_PASS, /* type */
  "expand", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_EXPAND, /* tv_id */
  ( PROP_ssa | PROP_gimple_leh | PROP_cfg
    | PROP_gimple_lcx
    | PROP_gimple_lvec
    | PROP_gimple_lva), /* properties_required */
  PROP_rtl, /* properties_provided */
  ( PROP_ssa | PROP_trees ), /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_expand : public rtl_opt_pass
{
public:
  pass_expand (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_expand, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_expand

unsigned int
pass_expand::execute (function *fun)
{
  basic_block bb, init_block;
  sbitmap blocks;
  edge_iterator ei;
  edge e;
  rtx_insn *var_seq, *var_ret_seq;
  unsigned i;

  timevar_push (TV_OUT_OF_SSA);
  rewrite_out_of_ssa (&SA);
  timevar_pop (TV_OUT_OF_SSA);
  SA.partition_to_pseudo = XCNEWVEC (rtx, SA.map->num_partitions);

  if (MAY_HAVE_DEBUG_STMTS && flag_tree_ter)
    {
      gimple_stmt_iterator gsi;
      FOR_EACH_BB_FN (bb, cfun)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  if (gimple_debug_bind_p (gsi_stmt (gsi)))
	    avoid_deep_ter_for_debug (gsi_stmt (gsi), 0);
    }

  /* Make sure all values used by the optimization passes have sane
     defaults.  */
  reg_renumber = 0;

  /* Some backends want to know that we are expanding to RTL.  */
  currently_expanding_to_rtl = 1;
  /* Dominators are not kept up-to-date as we may create new basic-blocks.  */
  free_dominance_info (CDI_DOMINATORS);

  rtl_profile_for_bb (ENTRY_BLOCK_PTR_FOR_FN (fun));

  if (chkp_function_instrumented_p (current_function_decl))
    chkp_reset_rtl_bounds ();

  insn_locations_init ();
  if (!DECL_IS_BUILTIN (current_function_decl))
    {
      /* Eventually, all FEs should explicitly set function_start_locus.  */
      if (LOCATION_LOCUS (fun->function_start_locus) == UNKNOWN_LOCATION)
	set_curr_insn_location
	  (DECL_SOURCE_LOCATION (current_function_decl));
      else
	set_curr_insn_location (fun->function_start_locus);
    }
  else
    set_curr_insn_location (UNKNOWN_LOCATION);
  prologue_location = curr_insn_location ();

#ifdef INSN_SCHEDULING
  init_sched_attrs ();
#endif

  /* Make sure first insn is a note even if we don't want linenums.
     This makes sure the first insn will never be deleted.
     Also, final expects a note to appear there.  */
  emit_note (NOTE_INSN_DELETED);

  /* Mark arrays indexed with non-constant indices with TREE_ADDRESSABLE.  */
  discover_nonconstant_array_refs ();

  targetm.expand_to_rtl_hook ();
  crtl->stack_alignment_needed = STACK_BOUNDARY;
  crtl->max_used_stack_slot_alignment = STACK_BOUNDARY;
  crtl->stack_alignment_estimated = 0;
  crtl->preferred_stack_boundary = STACK_BOUNDARY;
  fun->cfg->max_jumptable_ents = 0;

  /* Resovle the function section.  Some targets, like ARM EABI rely on knowledge
     of the function section at exapnsion time to predict distance of calls.  */
  resolve_unique_section (current_function_decl, 0, flag_function_sections);

  /* Expand the variables recorded during gimple lowering.  */
  timevar_push (TV_VAR_EXPAND);
  start_sequence ();

  var_ret_seq = expand_used_vars ();

  var_seq = get_insns ();
  end_sequence ();
  timevar_pop (TV_VAR_EXPAND);

  /* Honor stack protection warnings.  */
  if (warn_stack_protect)
    {
      if (fun->calls_alloca)
	warning (OPT_Wstack_protector,
		 "stack protector not protecting local variables: "
		 "variable length buffer");
      if (has_short_buffer && !crtl->stack_protect_guard)
	warning (OPT_Wstack_protector,
		 "stack protector not protecting function: "
		 "all local arrays are less than %d bytes long",
		 (int) PARAM_VALUE (PARAM_SSP_BUFFER_SIZE));
    }

  /* Set up parameters and prepare for return, for the function.  */
  expand_function_start (current_function_decl);

  /* If we emitted any instructions for setting up the variables,
     emit them before the FUNCTION_START note.  */
  if (var_seq)
    {
      emit_insn_before (var_seq, parm_birth_insn);

      /* In expand_function_end we'll insert the alloca save/restore
	 before parm_birth_insn.  We've just insertted an alloca call.
	 Adjust the pointer to match.  */
      parm_birth_insn = var_seq;
    }

  /* Now that we also have the parameter RTXs, copy them over to our
     partitions.  */
  for (i = 0; i < SA.map->num_partitions; i++)
    {
      tree var = SSA_NAME_VAR (partition_to_var (SA.map, i));

      if (TREE_CODE (var) != VAR_DECL
	  && !SA.partition_to_pseudo[i])
	SA.partition_to_pseudo[i] = DECL_RTL_IF_SET (var);
      gcc_assert (SA.partition_to_pseudo[i]);

      /* If this decl was marked as living in multiple places, reset
	 this now to NULL.  */
      if (DECL_RTL_IF_SET (var) == pc_rtx)
	SET_DECL_RTL (var, NULL);

      /* Some RTL parts really want to look at DECL_RTL(x) when x
	 was a decl marked in REG_ATTR or MEM_ATTR.  We could use
	 SET_DECL_RTL here making this available, but that would mean
	 to select one of the potentially many RTLs for one DECL.  Instead
	 of doing that we simply reset the MEM_EXPR of the RTL in question,
	 then nobody can get at it and hence nobody can call DECL_RTL on it.  */
      if (!DECL_RTL_SET_P (var))
	{
	  if (MEM_P (SA.partition_to_pseudo[i]))
	    set_mem_expr (SA.partition_to_pseudo[i], NULL);
	}
    }

  /* If we have a class containing differently aligned pointers
     we need to merge those into the corresponding RTL pointer
     alignment.  */
  for (i = 1; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      int part;
      rtx r;

      if (!name
	  /* We might have generated new SSA names in
	     update_alias_info_with_stack_vars.  They will have a NULL
	     defining statements, and won't be part of the partitioning,
	     so ignore those.  */
	  || !SSA_NAME_DEF_STMT (name))
	continue;
      part = var_to_partition (SA.map, name);
      if (part == NO_PARTITION)
	continue;

      /* Adjust all partition members to get the underlying decl of
	 the representative which we might have created in expand_one_var.  */
      if (SSA_NAME_VAR (name) == NULL_TREE)
	{
	  tree leader = partition_to_var (SA.map, part);
	  gcc_assert (SSA_NAME_VAR (leader) != NULL_TREE);
	  replace_ssa_name_symbol (name, SSA_NAME_VAR (leader));
	}
      if (!POINTER_TYPE_P (TREE_TYPE (name)))
	continue;

      r = SA.partition_to_pseudo[part];
      if (REG_P (r))
	mark_reg_pointer (r, get_pointer_alignment (name));
    }

  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (DECL_NAME (current_function_decl)
      && MAIN_NAME_P (DECL_NAME (current_function_decl))
      && DECL_FILE_SCOPE_P (current_function_decl))
    expand_main_function ();

  /* Initialize the stack_protect_guard field.  This must happen after the
     call to __main (if any) so that the external decl is initialized.  */
  if (crtl->stack_protect_guard)
    stack_protect_prologue ();

  expand_phi_nodes (&SA);

  /* Register rtl specific functions for cfg.  */
  rtl_register_cfg_hooks ();

  init_block = construct_init_block ();

  /* Clear EDGE_EXECUTABLE on the entry edge(s).  It is cleaned from the
     remaining edges later.  */
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (fun)->succs)
    e->flags &= ~EDGE_EXECUTABLE;

  lab_rtx_for_bb = new hash_map<basic_block, rtx_code_label *>;
  FOR_BB_BETWEEN (bb, init_block->next_bb, EXIT_BLOCK_PTR_FOR_FN (fun),
		  next_bb)
    bb = expand_gimple_basic_block (bb, var_ret_seq != NULL_RTX);

  if (MAY_HAVE_DEBUG_INSNS)
    expand_debug_locations ();

  if (deep_ter_debug_map)
    {
      delete deep_ter_debug_map;
      deep_ter_debug_map = NULL;
    }

  /* Free stuff we no longer need after GIMPLE optimizations.  */
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  delete_tree_cfg_annotations ();

  timevar_push (TV_OUT_OF_SSA);
  finish_out_of_ssa (&SA);
  timevar_pop (TV_OUT_OF_SSA);

  timevar_push (TV_POST_EXPAND);
  /* We are no longer in SSA form.  */
  fun->gimple_df->in_ssa_p = false;
  loops_state_clear (LOOP_CLOSED_SSA);

  /* Expansion is used by optimization passes too, set maybe_hot_insn_p
     conservatively to true until they are all profile aware.  */
  delete lab_rtx_for_bb;
  free_histograms ();

  construct_exit_block ();
  insn_locations_finalize ();

  if (var_ret_seq)
    {
      rtx_insn *after = return_label;
      rtx_insn *next = NEXT_INSN (after);
      if (next && NOTE_INSN_BASIC_BLOCK_P (next))
	after = next;
      emit_insn_after (var_ret_seq, after);
    }

  /* Zap the tree EH table.  */
  set_eh_throw_stmt_table (fun, NULL);

  /* We need JUMP_LABEL be set in order to redirect jumps, and hence
     split edges which edge insertions might do.  */
  rebuild_jump_labels (get_insns ());

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (fun),
		  EXIT_BLOCK_PTR_FOR_FN (fun), next_bb)
    {
      edge e;
      edge_iterator ei;
      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  if (e->insns.r)
	    {
	      rebuild_jump_labels_chain (e->insns.r);
	      /* Put insns after parm birth, but before
		 NOTE_INSNS_FUNCTION_BEG.  */
	      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (fun)
		  && single_succ_p (ENTRY_BLOCK_PTR_FOR_FN (fun)))
		{
		  rtx_insn *insns = e->insns.r;
		  e->insns.r = NULL;
		  if (NOTE_P (parm_birth_insn)
		      && NOTE_KIND (parm_birth_insn) == NOTE_INSN_FUNCTION_BEG)
		    emit_insn_before_noloc (insns, parm_birth_insn, e->dest);
		  else
		    emit_insn_after_noloc (insns, parm_birth_insn, e->dest);
		}
	      else
		commit_one_edge_insertion (e);
	    }
	  else
	    ei_next (&ei);
	}
    }

  /* We're done expanding trees to RTL.  */
  currently_expanding_to_rtl = 0;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (fun)->next_bb,
		  EXIT_BLOCK_PTR_FOR_FN (fun), next_bb)
    {
      edge e;
      edge_iterator ei;
      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  /* Clear EDGE_EXECUTABLE.  This flag is never used in the backend.  */
	  e->flags &= ~EDGE_EXECUTABLE;

	  /* At the moment not all abnormal edges match the RTL
	     representation.  It is safe to remove them here as
	     find_many_sub_basic_blocks will rediscover them.
	     In the future we should get this fixed properly.  */
	  if ((e->flags & EDGE_ABNORMAL)
	      && !(e->flags & EDGE_SIBCALL))
	    remove_edge (e);
	  else
	    ei_next (&ei);
	}
    }

  blocks = sbitmap_alloc (last_basic_block_for_fn (fun));
  bitmap_ones (blocks);
  find_many_sub_basic_blocks (blocks);
  sbitmap_free (blocks);
  purge_all_dead_edges ();

  expand_stack_alignment ();

  /* Fixup REG_EQUIV notes in the prologue if there are tailcalls in this
     function.  */
  if (crtl->tail_call_emit)
    fixup_tail_calls ();

  /* After initial rtl generation, call back to finish generating
     exception support code.  We need to do this before cleaning up
     the CFG as the code does not expect dead landing pads.  */
  if (fun->eh->region_tree != NULL)
    finish_eh_generation ();

  /* Remove unreachable blocks, otherwise we cannot compute dominators
     which are needed for loop state verification.  As a side-effect
     this also compacts blocks.
     ???  We cannot remove trivially dead insns here as for example
     the DRAP reg on i?86 is not magically live at this point.
     gcc.c-torture/execute/ipa-sra-2.c execution, -Os -m32 fails otherwise.  */
  cleanup_cfg (CLEANUP_NO_INSN_DEL);

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  /* Initialize pseudos allocated for hard registers.  */
  emit_initial_value_sets ();

  /* And finally unshare all RTL.  */
  unshare_all_rtl ();

  /* There's no need to defer outputting this function any more; we
     know we want to output it.  */
  DECL_DEFER_OUTPUT (current_function_decl) = 0;

  /* Now that we're done expanding trees to RTL, we shouldn't have any
     more CONCATs anywhere.  */
  generating_concat_p = 0;

  if (dump_file)
    {
      fprintf (dump_file,
	       "\n\n;;\n;; Full RTL generated for this function:\n;;\n");
      /* And the pass manager will dump RTL for us.  */
    }

  /* If we're emitting a nested function, make sure its parent gets
     emitted as well.  Doing otherwise confuses debug info.  */
    {
      tree parent;
      for (parent = DECL_CONTEXT (current_function_decl);
	   parent != NULL_TREE;
	   parent = get_containing_scope (parent))
	if (TREE_CODE (parent) == FUNCTION_DECL)
	  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (parent)) = 1;
    }

  /* We are now committed to emitting code for this function.  Do any
     preparation, such as emitting abstract debug info for the inline
     before it gets mangled by optimization.  */
  if (cgraph_function_possibly_inlined_p (current_function_decl))
    (*debug_hooks->outlining_inline_function) (current_function_decl);

  TREE_ASM_WRITTEN (current_function_decl) = 1;

  /* After expanding, the return labels are no longer needed. */
  return_label = NULL;
  naked_return_label = NULL;

  /* After expanding, the tm_restart map is no longer needed.  */
  if (fun->gimple_df->tm_restart)
    fun->gimple_df->tm_restart = NULL;

  /* Tag the blocks with a depth number so that change_scope can find
     the common parent easily.  */
  set_block_levels (DECL_INITIAL (fun->decl), 0);
  default_rtl_profile ();

  timevar_pop (TV_POST_EXPAND);

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_expand (gcc::context *ctxt)
{
  return new pass_expand (ctxt);
}
