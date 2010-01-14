/* Single entry single exit control flow regions.
   Copyright (C) 2008, 2009  Free Software Foundation, Inc.
   Contributed by Jan Sjodin <jan.sjodin@amd.com> and
   Sebastian Pop <sebastian.pop@amd.com>.

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
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "toplev.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "value-prof.h"
#include "pointer-set.h"
#include "gimple.h"
#include "sese.h"

/* Print to stderr the element ELT.  */

static void
debug_rename_elt (rename_map_elt elt)
{
  fprintf (stderr, "(");
  print_generic_expr (stderr, elt->old_name, 0);
  fprintf (stderr, ", ");
  print_generic_expr (stderr, elt->expr, 0);
  fprintf (stderr, ")\n");
}

/* Helper function for debug_rename_map.  */

static int
debug_rename_map_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct rename_map_elt_s *entry = (struct rename_map_elt_s *) *slot;
  debug_rename_elt (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

void
debug_rename_map (htab_t map)
{
  htab_traverse (map, debug_rename_map_1, NULL);
}

/* Computes a hash function for database element ELT.  */

hashval_t
rename_map_elt_info (const void *elt)
{
  return SSA_NAME_VERSION (((const struct rename_map_elt_s *) elt)->old_name);
}

/* Compares database elements E1 and E2.  */

int
eq_rename_map_elts (const void *e1, const void *e2)
{
  const struct rename_map_elt_s *elt1 = (const struct rename_map_elt_s *) e1;
  const struct rename_map_elt_s *elt2 = (const struct rename_map_elt_s *) e2;

  return (elt1->old_name == elt2->old_name);
}



/* Print to stderr the element ELT.  */

static void
debug_ivtype_elt (ivtype_map_elt elt)
{
  fprintf (stderr, "(%s, ", elt->cloog_iv);
  print_generic_expr (stderr, elt->type, 0);
  fprintf (stderr, ")\n");
}

/* Helper function for debug_ivtype_map.  */

static int
debug_ivtype_map_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct ivtype_map_elt_s *entry = (struct ivtype_map_elt_s *) *slot;
  debug_ivtype_elt (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

void
debug_ivtype_map (htab_t map)
{
  htab_traverse (map, debug_ivtype_map_1, NULL);
}

/* Computes a hash function for database element ELT.  */

hashval_t
ivtype_map_elt_info (const void *elt)
{
  return htab_hash_pointer (((const struct ivtype_map_elt_s *) elt)->cloog_iv);
}

/* Compares database elements E1 and E2.  */

int
eq_ivtype_map_elts (const void *e1, const void *e2)
{
  const struct ivtype_map_elt_s *elt1 = (const struct ivtype_map_elt_s *) e1;
  const struct ivtype_map_elt_s *elt2 = (const struct ivtype_map_elt_s *) e2;

  return (elt1->cloog_iv == elt2->cloog_iv);
}



/* Record LOOP as occuring in REGION.  */

static void
sese_record_loop (sese region, loop_p loop)
{
  if (sese_contains_loop (region, loop))
    return;

  bitmap_set_bit (SESE_LOOPS (region), loop->num);
  VEC_safe_push (loop_p, heap, SESE_LOOP_NEST (region), loop);
}

/* Build the loop nests contained in REGION.  Returns true when the
   operation was successful.  */

void
build_sese_loop_nests (sese region)
{
  unsigned i;
  basic_block bb;
  struct loop *loop0, *loop1;

  FOR_EACH_BB (bb)
    if (bb_in_sese_p (bb, region))
      {
	struct loop *loop = bb->loop_father;

	/* Only add loops if they are completely contained in the SCoP.  */
	if (loop->header == bb
	    && bb_in_sese_p (loop->latch, region))
	  sese_record_loop (region, loop);
      }

  /* Make sure that the loops in the SESE_LOOP_NEST are ordered.  It
     can be the case that an inner loop is inserted before an outer
     loop.  To avoid this, semi-sort once.  */
  for (i = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), i, loop0); i++)
    {
      if (VEC_length (loop_p, SESE_LOOP_NEST (region)) == i + 1)
	break;

      loop1 = VEC_index (loop_p, SESE_LOOP_NEST (region), i + 1);
      if (loop0->num > loop1->num)
	{
	  VEC_replace (loop_p, SESE_LOOP_NEST (region), i, loop1);
	  VEC_replace (loop_p, SESE_LOOP_NEST (region), i + 1, loop0);
	}
    }
}

/* For a USE in BB, if BB is outside REGION, mark the USE in the
   LIVEOUTS set.  */

static void
sese_build_liveouts_use (sese region, bitmap liveouts, basic_block bb,
			 tree use)
{
  unsigned ver;
  basic_block def_bb;

  if (TREE_CODE (use) != SSA_NAME)
    return;

  ver = SSA_NAME_VERSION (use);
  def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));

  if (!def_bb
      || !bb_in_sese_p (def_bb, region)
      || bb_in_sese_p (bb, region))
    return;

  bitmap_set_bit (liveouts, ver);
}

/* Marks for rewrite all the SSA_NAMES defined in REGION and that are
   used in BB that is outside of the REGION.  */

static void
sese_build_liveouts_bb (sese region, bitmap liveouts, basic_block bb)
{
  gimple_stmt_iterator bsi;
  edge e;
  edge_iterator ei;
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi); gsi_next (&bsi))
      sese_build_liveouts_use (region, liveouts, bb,
			       PHI_ARG_DEF_FROM_EDGE (gsi_stmt (bsi), e));

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);

      if (is_gimple_debug (stmt))
	continue;

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	sese_build_liveouts_use (region, liveouts, bb, USE_FROM_PTR (use_p));
    }
}

/* For a USE in BB, return true if BB is outside REGION and it's not
   in the LIVEOUTS set.  */

static bool
sese_bad_liveouts_use (sese region, bitmap liveouts, basic_block bb,
		       tree use)
{
  unsigned ver;
  basic_block def_bb;

  if (TREE_CODE (use) != SSA_NAME)
    return false;

  ver = SSA_NAME_VERSION (use);

  /* If it's in liveouts, the variable will get a new PHI node, and
     the debug use will be properly adjusted.  */
  if (bitmap_bit_p (liveouts, ver))
    return false;

  def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));

  if (!def_bb
      || !bb_in_sese_p (def_bb, region)
      || bb_in_sese_p (bb, region))
    return false;

  return true;
}

/* Reset debug stmts that reference SSA_NAMES defined in REGION that
   are not marked as liveouts.  */

static void
sese_reset_debug_liveouts_bb (sese region, bitmap liveouts, basic_block bb)
{
  gimple_stmt_iterator bsi;
  ssa_op_iter iter;
  use_operand_p use_p;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);

      if (!is_gimple_debug (stmt))
	continue;

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	if (sese_bad_liveouts_use (region, liveouts, bb,
				   USE_FROM_PTR (use_p)))
	  {
	    gimple_debug_bind_reset_value (stmt);
	    update_stmt (stmt);
	    break;
	  }
    }
}

/* Build the LIVEOUTS of REGION: the set of variables defined inside
   and used outside the REGION.  */

static void
sese_build_liveouts (sese region, bitmap liveouts)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    sese_build_liveouts_bb (region, liveouts, bb);
  if (MAY_HAVE_DEBUG_INSNS)
    FOR_EACH_BB (bb)
      sese_reset_debug_liveouts_bb (region, liveouts, bb);
}

/* Builds a new SESE region from edges ENTRY and EXIT.  */

sese
new_sese (edge entry, edge exit)
{
  sese region = XNEW (struct sese_s);

  SESE_ENTRY (region) = entry;
  SESE_EXIT (region) = exit;
  SESE_LOOPS (region) = BITMAP_ALLOC (NULL);
  SESE_LOOP_NEST (region) = VEC_alloc (loop_p, heap, 3);
  SESE_ADD_PARAMS (region) = true;
  SESE_PARAMS (region) = VEC_alloc (tree, heap, 3);

  return region;
}

/* Deletes REGION.  */

void
free_sese (sese region)
{
  if (SESE_LOOPS (region))
    SESE_LOOPS (region) = BITMAP_ALLOC (NULL);

  VEC_free (tree, heap, SESE_PARAMS (region));
  VEC_free (loop_p, heap, SESE_LOOP_NEST (region));

  XDELETE (region);
}

/* Add exit phis for USE on EXIT.  */

static void
sese_add_exit_phis_edge (basic_block exit, tree use, edge false_e, edge true_e)
{
  gimple phi = create_phi_node (use, exit);

  create_new_def_for (gimple_phi_result (phi), phi,
		      gimple_phi_result_ptr (phi));
  add_phi_arg (phi, use, false_e, UNKNOWN_LOCATION);
  add_phi_arg (phi, use, true_e, UNKNOWN_LOCATION);
}

/* Insert in the block BB phi nodes for variables defined in REGION
   and used outside the REGION.  The code generation moves REGION in
   the else clause of an "if (1)" and generates code in the then
   clause that is at this point empty:

   | if (1)
   |   empty;
   | else
   |   REGION;
*/

void
sese_insert_phis_for_liveouts (sese region, basic_block bb,
			       edge false_e, edge true_e)
{
  unsigned i;
  bitmap_iterator bi;
  bitmap liveouts = BITMAP_ALLOC (NULL);

  update_ssa (TODO_update_ssa);

  sese_build_liveouts (region, liveouts);
  EXECUTE_IF_SET_IN_BITMAP (liveouts, 0, i, bi)
    sese_add_exit_phis_edge (bb, ssa_name (i), false_e, true_e);
  BITMAP_FREE (liveouts);

  update_ssa (TODO_update_ssa);
}

/* Get the definition of NAME before the SESE.  Keep track of the
   basic blocks that have been VISITED in a bitmap.  */

static tree
get_vdef_before_sese (sese region, tree name, sbitmap visited)
{
  unsigned i;
  gimple stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (stmt);

  if (!def_bb || !bb_in_sese_p (def_bb, region))
    return name;

  if (TEST_BIT (visited, def_bb->index))
    return NULL_TREE;

  SET_BIT (visited, def_bb->index);

  switch (gimple_code (stmt))
    {
    case GIMPLE_PHI:
      for (i = 0; i < gimple_phi_num_args (stmt); i++)
	{
	  tree arg = gimple_phi_arg_def (stmt, i);
	  tree res;

	  if (gimple_bb (SSA_NAME_DEF_STMT (arg))
	      && def_bb->index == gimple_bb (SSA_NAME_DEF_STMT (arg))->index)
	    continue;

	  res = get_vdef_before_sese (region, arg, visited);
	  if (res)
	    return res;
	}
      return NULL_TREE;

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      {
	use_operand_p use_p = gimple_vuse_op (stmt);
	tree use = USE_FROM_PTR (use_p);

	if (def_bb->index == gimple_bb (SSA_NAME_DEF_STMT (use))->index)
	  RESET_BIT (visited, def_bb->index);

	return get_vdef_before_sese (region, use, visited);
      }

    default:
      return NULL_TREE;
    }
}

/* Adjust a virtual phi node PHI that is placed at the end of the
   generated code for SCOP:

   | if (1)
   |   generated code from REGION;
   | else
   |   REGION;

   The FALSE_E edge comes from the original code, TRUE_E edge comes
   from the code generated for the SCOP.  */

static void
sese_adjust_vphi (sese region, gimple phi, edge true_e)
{
  unsigned i;

  gcc_assert (gimple_phi_num_args (phi) == 2);

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_edge (phi, i) == true_e)
      {
	tree true_arg, false_arg, before_scop_arg;
	sbitmap visited;

	true_arg = gimple_phi_arg_def (phi, i);
	if (!SSA_NAME_IS_DEFAULT_DEF (true_arg))
	  return;

	false_arg = gimple_phi_arg_def (phi, i == 0 ? 1 : 0);
	if (SSA_NAME_IS_DEFAULT_DEF (false_arg))
	  return;

	visited = sbitmap_alloc (last_basic_block);
	sbitmap_zero (visited);
	before_scop_arg = get_vdef_before_sese (region, false_arg, visited);
	gcc_assert (before_scop_arg != NULL_TREE);
	SET_PHI_ARG_DEF (phi, i, before_scop_arg);
	sbitmap_free (visited);
      }
}

/* Returns the expression associated to OLD_NAME in MAP.  */

static tree
get_rename (htab_t map, tree old_name)
{
  struct rename_map_elt_s tmp;
  PTR *slot;

  tmp.old_name = old_name;
  slot = htab_find_slot (map, &tmp, NO_INSERT);

  if (slot && *slot)
    return ((rename_map_elt) *slot)->expr;

  return old_name;
}

/* Register in MAP the rename tuple (OLD_NAME, EXPR).  */

void
set_rename (htab_t map, tree old_name, tree expr)
{
  struct rename_map_elt_s tmp;
  PTR *slot;

  if (old_name == expr)
    return;

  tmp.old_name = old_name;
  slot = htab_find_slot (map, &tmp, INSERT);

  if (!slot)
    return;

  if (*slot)
    free (*slot);

  *slot = new_rename_map_elt (old_name, expr);
}

static void rename_variables_in_expr (htab_t, tree);

/* Renames the operand OP of expression T following the tuples
   (OLD_NAME, EXPR) in RENAME_MAP.  */

static void
rename_variables_in_operand (htab_t rename_map, tree t, int op)
{
  tree operand = TREE_OPERAND (t, op);

  if (TREE_CODE (operand) == SSA_NAME)
    {
      tree new_name = get_rename (rename_map, operand);

      if (new_name != operand)
	TREE_OPERAND (t, op) = new_name;
    }
  else
    rename_variables_in_expr (rename_map, operand);
}

/* Renames the expression T following the tuples (OLD_NAME, EXPR) in
   RENAME_MAP.  */

static void
rename_variables_in_expr (htab_t rename_map, tree t)
{
  if (!t)
    return;

  switch (TREE_CODE_LENGTH (TREE_CODE (t)))
    {
    case 3:
      rename_variables_in_operand (rename_map, t, 2);

    case 2:
      rename_variables_in_operand (rename_map, t, 1);

    case 1:
      rename_variables_in_operand (rename_map, t, 0);

    default:
      return;
    }
}

/* Renames all the loop->nb_iterations expressions following the
   tuples (OLD_NAME, EXPR) in RENAME_MAP.  */

void
rename_nb_iterations (htab_t rename_map)
{
  loop_iterator li;
  struct loop *loop;

  FOR_EACH_LOOP (li, loop, 0)
    {
      rename_variables_in_expr (rename_map, loop->nb_iterations);
    }
}

/* Adjusts the phi nodes in the block BB for variables defined in
   SCOP_REGION and used outside the SCOP_REGION.  The code generation
   moves SCOP_REGION in the else clause of an "if (1)" and generates
   code in the then clause:

   | if (1)
   |   generated code from REGION;
   | else
   |   REGION;

   To adjust the phi nodes after the condition, the RENAME_MAP is
   used.  */

void
sese_adjust_liveout_phis (sese region, htab_t rename_map, basic_block bb,
			  edge false_e, edge true_e)
{
  gimple_stmt_iterator si;

  for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
    {
      unsigned i;
      unsigned false_i = 0;
      gimple phi = gsi_stmt (si);
      tree res = gimple_phi_result (phi);

      if (!is_gimple_reg (res))
	{
	  sese_adjust_vphi (region, phi, true_e);
	  continue;
	}

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	if (gimple_phi_arg_edge (phi, i) == false_e)
	  {
	    false_i = i;
	    break;
	  }

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	if (gimple_phi_arg_edge (phi, i) == true_e)
	  {
	    tree old_name = gimple_phi_arg_def (phi, false_i);
	    tree expr = get_rename (rename_map, old_name);
	    gimple_seq stmts;

	    gcc_assert (old_name != expr);

	    if (TREE_CODE (expr) != SSA_NAME
		&& is_gimple_reg (old_name))
	      {
		tree type = TREE_TYPE (old_name);
		tree var = create_tmp_var (type, "var");

		expr = build2 (MODIFY_EXPR, type, var, expr);
		expr = force_gimple_operand (expr, &stmts, true, NULL);
		gsi_insert_seq_on_edge_immediate (true_e, stmts);
	      }

	    SET_PHI_ARG_DEF (phi, i, expr);
	    set_rename (rename_map, old_name, res);
	  }
    }
}

/* Rename the SSA_NAMEs used in STMT and that appear in MAP.  */

static void
rename_variables_in_stmt (gimple stmt, htab_t map, gimple_stmt_iterator *insert_gsi)
{
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
    {
      tree use = USE_FROM_PTR (use_p);
      tree expr = get_rename (map, use);
      tree type_use = TREE_TYPE (use);
      tree type_expr = TREE_TYPE (expr);
      gimple_seq stmts;

      if (use == expr)
	continue;

      if (type_use != type_expr
	  || (TREE_CODE (expr) != SSA_NAME
	      && is_gimple_reg (use)))
	{
	  tree var;

	  if (is_gimple_debug (stmt))
	    {
	      if (gimple_debug_bind_p (stmt))
		gimple_debug_bind_reset_value (stmt);
	      else
		gcc_unreachable ();

	      break;
	    }

	  var = create_tmp_var (type_use, "var");

	  if (type_use != type_expr)
	    expr = fold_convert (type_use, expr);

	  expr = build2 (MODIFY_EXPR, type_use, var, expr);
	  expr = force_gimple_operand (expr, &stmts, true, NULL);
	  gsi_insert_seq_before (insert_gsi, stmts, GSI_SAME_STMT);
	}

      replace_exp (use_p, expr);
    }

  update_stmt (stmt);
}

/* Returns true if NAME is a parameter of SESE.  */

static bool
is_parameter (sese region, tree name)
{
  int i;
  tree p;

  for (i = 0; VEC_iterate (tree, SESE_PARAMS (region), i, p); i++)
    if (p == name)
      return true;

  return false;
}

/* Returns true if NAME is an induction variable.  */

static bool
is_iv (tree name)
{
  return gimple_code (SSA_NAME_DEF_STMT (name)) == GIMPLE_PHI;
}

static void expand_scalar_variables_stmt (gimple, basic_block, sese,
					  htab_t, gimple_stmt_iterator *);
static tree
expand_scalar_variables_expr (tree, tree, enum tree_code, tree, basic_block,
			      sese, htab_t, gimple_stmt_iterator *);

static tree
expand_scalar_variables_call (gimple stmt, basic_block bb, sese region,
			      htab_t map, gimple_stmt_iterator *gsi)
{
  int i, nargs = gimple_call_num_args (stmt);
  VEC (tree, gc) *args = VEC_alloc (tree, gc, nargs);
  tree fn_type = TREE_TYPE (gimple_call_fn (stmt));
  tree fn = gimple_call_fndecl (stmt);
  tree call_expr, var, lhs;
  gimple call;

  for (i = 0; i < nargs; i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      tree t = TREE_TYPE (arg);

      var = create_tmp_var (t, "var");
      arg = expand_scalar_variables_expr (t, arg, TREE_CODE (arg), NULL,
					  bb, region, map, gsi);
      arg = build2 (MODIFY_EXPR, t, var, arg);
      arg = force_gimple_operand_gsi (gsi, arg, true, NULL,
				      true, GSI_SAME_STMT);
      VEC_quick_push (tree, args, arg);
    }

  lhs = gimple_call_lhs (stmt);
  var = create_tmp_var (TREE_TYPE (lhs), "var");
  call_expr = build_call_vec (fn_type, fn, args);
  call = gimple_build_call_from_tree (call_expr);
  var = make_ssa_name (var, call);
  gimple_call_set_lhs (call, var);
  gsi_insert_before (gsi, call, GSI_SAME_STMT);

  return var;
}

/* Copies at GSI all the scalar computations on which the ssa_name OP0
   depends on in the SESE: these are all the scalar variables used in
   the definition of OP0, that are defined outside BB and still in the
   SESE, i.e. not a parameter of the SESE.  The expression that is
   returned contains only induction variables from the generated code:
   MAP contains the induction variables renaming mapping, and is used
   to translate the names of induction variables.  */

static tree
expand_scalar_variables_ssa_name (tree op0, basic_block bb,
				  sese region, htab_t map,
				  gimple_stmt_iterator *gsi)
{
  gimple def_stmt;
  tree new_op;

  if (is_parameter (region, op0)
      || is_iv (op0))
    return get_rename (map, op0);

  def_stmt = SSA_NAME_DEF_STMT (op0);

  /* Check whether we already have a rename for OP0.  */
  new_op = get_rename (map, op0);

  if (new_op != op0
      && gimple_bb (SSA_NAME_DEF_STMT (new_op)) == bb)
    return new_op;

  if (gimple_bb (def_stmt) == bb)
    {
      /* If the defining statement is in the basic block already
	 we do not need to create a new expression for it, we
	 only need to ensure its operands are expanded.  */
      expand_scalar_variables_stmt (def_stmt, bb, region, map, gsi);
      return new_op;
    }
  else
    {
      if (!gimple_bb (def_stmt)
	  || !bb_in_sese_p (gimple_bb (def_stmt), region))
	return new_op;

      switch (gimple_code (def_stmt))
	{
	case GIMPLE_ASSIGN:
	  {
	    tree var0 = gimple_assign_rhs1 (def_stmt);
	    enum tree_code subcode = gimple_assign_rhs_code (def_stmt);
	    tree var1 = gimple_assign_rhs2 (def_stmt);
	    tree type = gimple_expr_type (def_stmt);

	    return expand_scalar_variables_expr (type, var0, subcode, var1, bb,
						 region, map, gsi);
	  }

	case GIMPLE_CALL:
	  return expand_scalar_variables_call (def_stmt, bb, region, map, gsi);

	default:
	  gcc_unreachable ();
	  return new_op;
	}
    }
}

/* Copies at GSI all the scalar computations on which the expression
   OP0 CODE OP1 depends on in the SESE: these are all the scalar
   variables used in OP0 and OP1, defined outside BB and still defined
   in the SESE, i.e. not a parameter of the SESE.  The expression that
   is returned contains only induction variables from the generated
   code: MAP contains the induction variables renaming mapping, and is
   used to translate the names of induction variables.  */

static tree
expand_scalar_variables_expr (tree type, tree op0, enum tree_code code,
			      tree op1, basic_block bb, sese region,
			      htab_t map, gimple_stmt_iterator *gsi)
{
  if (TREE_CODE_CLASS (code) == tcc_constant
      || TREE_CODE_CLASS (code) == tcc_declaration)
    return op0;

  /* For data references we have to duplicate also its memory
     indexing.  */
  if (TREE_CODE_CLASS (code) == tcc_reference)
    {
      switch (code)
	{
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  {
	    tree op = TREE_OPERAND (op0, 0);
	    tree res = expand_scalar_variables_expr
	      (type, op, TREE_CODE (op), NULL, bb, region, map, gsi);
	    return build1 (code, type, res);
	  }

	case INDIRECT_REF:
	  {
	    tree old_name = TREE_OPERAND (op0, 0);
	    tree expr = expand_scalar_variables_ssa_name
	      (old_name, bb, region, map, gsi);

	    if (TREE_CODE (expr) != SSA_NAME
		&& is_gimple_reg (old_name))
	      {
		tree type = TREE_TYPE (old_name);
		tree var = create_tmp_var (type, "var");

		expr = build2 (MODIFY_EXPR, type, var, expr);
		expr = force_gimple_operand_gsi (gsi, expr, true, NULL,
						 true, GSI_SAME_STMT);
	      }

	    return fold_build1 (code, type, expr);
	  }

	case ARRAY_REF:
	  {
	    tree op00 = TREE_OPERAND (op0, 0);
	    tree op01 = TREE_OPERAND (op0, 1);
	    tree op02 = TREE_OPERAND (op0, 2);
	    tree op03 = TREE_OPERAND (op0, 3);
	    tree base = expand_scalar_variables_expr
	      (TREE_TYPE (op00), op00, TREE_CODE (op00), NULL, bb, region,
	       map, gsi);
	    tree subscript = expand_scalar_variables_expr
	      (TREE_TYPE (op01), op01, TREE_CODE (op01), NULL, bb, region,
	       map, gsi);

	    return build4 (ARRAY_REF, type, base, subscript, op02, op03);
	  }

	default:
	  /* The above cases should catch everything.  */
	  gcc_unreachable ();
	}
    }

  if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = expand_scalar_variables_expr (op0_type, op0, op0_code,
						    NULL, bb, region, map, gsi);

      return fold_build1 (code, type, op0_expr);
    }

  if (TREE_CODE_CLASS (code) == tcc_binary
      || TREE_CODE_CLASS (code) == tcc_comparison)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = expand_scalar_variables_expr (op0_type, op0, op0_code,
						    NULL, bb, region, map, gsi);
      tree op1_type = TREE_TYPE (op1);
      enum tree_code op1_code = TREE_CODE (op1);
      tree op1_expr = expand_scalar_variables_expr (op1_type, op1, op1_code,
						    NULL, bb, region, map, gsi);

      return fold_build2 (code, type, op0_expr, op1_expr);
    }

  if (code == SSA_NAME)
    return expand_scalar_variables_ssa_name (op0, bb, region, map, gsi);

  if (code == ADDR_EXPR)
    {
      tree op00 = TREE_OPERAND (op0, 0);

      if (handled_component_p (op00)
	  && TREE_CODE (op00) == ARRAY_REF)
	{
	  tree e = expand_scalar_variables_expr (TREE_TYPE (op00), op00,
						 TREE_CODE (op00),
						 NULL, bb, region, map, gsi);
	  return fold_build1 (code, TREE_TYPE (op0), e);
	}

      return op0;
    }

  gcc_unreachable ();
  return NULL;
}

/* Copies at the beginning of BB all the scalar computations on which
   STMT depends on in the SESE: these are all the scalar variables used
   in STMT, defined outside BB and still defined in the SESE, i.e. not a
   parameter of the SESE.  The expression that is returned contains
   only induction variables from the generated code: MAP contains the
   induction variables renaming mapping, and is used to translate the
   names of induction variables.  */

static void
expand_scalar_variables_stmt (gimple stmt, basic_block bb, sese region,
			      htab_t map, gimple_stmt_iterator *gsi)
{
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
    {
      tree use = USE_FROM_PTR (use_p);
      tree type = TREE_TYPE (use);
      enum tree_code code = TREE_CODE (use);
      tree use_expr;

      if (!is_gimple_reg (use))
	continue;

      /* Don't expand USE if we already have a rename for it.  */
      use_expr = get_rename (map, use);
      if (use_expr != use)
	continue;

      use_expr = expand_scalar_variables_expr (type, use, code, NULL, bb,
					       region, map, gsi);
      use_expr = fold_convert (type, use_expr);

      if (use_expr == use)
	continue;

      if (is_gimple_debug (stmt))
	{
	  if (gimple_debug_bind_p (stmt))
	    gimple_debug_bind_reset_value (stmt);
	  else
	    gcc_unreachable ();

	  break;
	}

      if (TREE_CODE (use_expr) != SSA_NAME)
	{
	  tree var = create_tmp_var (type, "var");

	  use_expr = build2 (MODIFY_EXPR, type, var, use_expr);
	  use_expr = force_gimple_operand_gsi (gsi, use_expr, true, NULL,
					       true, GSI_SAME_STMT);
	}

      replace_exp (use_p, use_expr);
    }

  update_stmt (stmt);
}

/* Copies at the beginning of BB all the scalar computations on which
   BB depends on in the SESE: these are all the scalar variables used
   in BB, defined outside BB and still defined in the SESE, i.e. not a
   parameter of the SESE.  The expression that is returned contains
   only induction variables from the generated code: MAP contains the
   induction variables renaming mapping, and is used to translate the
   names of induction variables.  */

static void
expand_scalar_variables (basic_block bb, sese region, htab_t map)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);
      expand_scalar_variables_stmt (stmt, bb, region, map, &gsi);
      gsi_next (&gsi);
    }
}

/* Rename all the SSA_NAMEs from block BB according to the MAP.  */

static void
rename_variables (basic_block bb, htab_t map)
{
  gimple_stmt_iterator gsi;
  gimple_stmt_iterator insert_gsi = gsi_start_bb (bb);

  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    rename_variables_in_stmt (gsi_stmt (gsi), map, &insert_gsi);
}

/* Remove condition from BB.  */

static void
remove_condition (basic_block bb)
{
  gimple last = last_stmt (bb);

  if (last && gimple_code (last) == GIMPLE_COND)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      gsi_remove (&gsi, true);
    }
}

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag set.  */

edge
get_true_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_TRUE_VALUE)
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag cleared.  */

edge
get_false_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_TRUE_VALUE))
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Returns true when NAME is defined in LOOP.  */

static bool
name_defined_in_loop_p (tree name, loop_p loop)
{
  gimple stmt = SSA_NAME_DEF_STMT (name);

  return (gimple_bb (stmt)->loop_father == loop);
}

/* Returns true when EXPR contains SSA_NAMEs defined in LOOP.  */

static bool
expr_defined_in_loop_p (tree expr, loop_p loop)
{
  switch (TREE_CODE_LENGTH (TREE_CODE (expr)))
    {
    case 3:
      return expr_defined_in_loop_p (TREE_OPERAND (expr, 0), loop)
	|| expr_defined_in_loop_p (TREE_OPERAND (expr, 1), loop)
	|| expr_defined_in_loop_p (TREE_OPERAND (expr, 2), loop);

    case 2:
      return expr_defined_in_loop_p (TREE_OPERAND (expr, 0), loop)
	|| expr_defined_in_loop_p (TREE_OPERAND (expr, 1), loop);

    case 1:
      return expr_defined_in_loop_p (TREE_OPERAND (expr, 0), loop);

    case 0:
      return TREE_CODE (expr) == SSA_NAME
	&& name_defined_in_loop_p (expr, loop);

    default:
      return false;
    }
}

/* Returns the gimple statement that uses NAME outside the loop it is
   defined in, returns NULL if there is no such loop close phi node.
   An invariant of the loop closed SSA form is that the only use of a
   variable, outside the loop it is defined in, is in the loop close
   phi node that just follows the loop.  */

static gimple
alive_after_loop (tree name)
{
  use_operand_p use_p;
  imm_use_iterator imm_iter;
  loop_p loop = gimple_bb (SSA_NAME_DEF_STMT (name))->loop_father;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple stmt = USE_STMT (use_p);

      if (gimple_code (stmt) == GIMPLE_PHI
	  && gimple_bb (stmt)->loop_father != loop)
	return stmt;
    }

  return NULL;
}

/* Return true if a close phi has not yet been inserted for the use of
   variable NAME on the single exit of LOOP.  */

static bool
close_phi_not_yet_inserted_p (loop_p loop, tree name)
{
  gimple_stmt_iterator psi;
  basic_block bb = single_exit (loop)->dest;

  for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
    if (gimple_phi_arg_def (gsi_stmt (psi), 0) == name)
      return false;

  return true;
}

/* A structure for passing parameters to add_loop_exit_phis.  */

typedef struct alep {
  loop_p loop;
  VEC (rename_map_elt, heap) *new_renames;
} *alep_p;

/* Helper function for htab_traverse in insert_loop_close_phis.  */

static int
add_loop_exit_phis (void **slot, void *data)
{
  struct rename_map_elt_s *entry;
  alep_p a;
  loop_p loop;
  tree expr, new_name, old_name;
  bool def_in_loop_p, used_outside_p, need_close_phi_p;
  gimple old_close_phi;

  if (!slot || !*slot || !data)
    return 1;

  entry = (struct rename_map_elt_s *) *slot;
  a = (alep_p) data;
  loop = a->loop;
  new_name = expr = entry->expr;
  old_name = entry->old_name;

  def_in_loop_p = expr_defined_in_loop_p (expr, loop);
  if (!def_in_loop_p)
    return 1;

  /* Remove the old rename from the map when the expression is defined
     in the loop that we're closing.  */
  free (*slot);
  *slot = NULL;

  if (TREE_CODE (expr) != SSA_NAME)
    return 1;

  old_close_phi = alive_after_loop (old_name);
  used_outside_p = (old_close_phi != NULL);
  need_close_phi_p = (used_outside_p
		      && close_phi_not_yet_inserted_p (loop, new_name));

  /* Insert a loop close phi node.  */
  if (need_close_phi_p)
    {
      basic_block bb = single_exit (loop)->dest;
      gimple phi = create_phi_node (new_name, bb);
      tree new_res = create_new_def_for (gimple_phi_result (phi), phi,
					 gimple_phi_result_ptr (phi));

      add_phi_arg (phi, new_name, single_pred_edge (bb), UNKNOWN_LOCATION);
      VEC_safe_push (rename_map_elt, heap, a->new_renames,
		     new_rename_map_elt (gimple_phi_result (old_close_phi),
					 new_res));
    }

  return 1;
}

/* Traverses MAP and removes from it all the tuples (OLD, NEW) where
   NEW is defined in LOOP.  Inserts on the exit of LOOP the close phi
   node "RES = phi (NEW)" corresponding to "OLD_RES = phi (OLD)" in
   the original code.  Inserts in MAP the tuple (OLD_RES, RES).  */

void
insert_loop_close_phis (htab_t map, loop_p loop)
{
  int i;
  struct alep a;
  rename_map_elt elt;

  a.loop = loop;
  a.new_renames = VEC_alloc (rename_map_elt, heap, 3);
  update_ssa (TODO_update_ssa);
  htab_traverse (map, add_loop_exit_phis, &a);
  update_ssa (TODO_update_ssa);

  for (i = 0; VEC_iterate (rename_map_elt, a.new_renames, i, elt); i++)
    {
      set_rename (map, elt->old_name, elt->expr);
      free (elt);
    }

  VEC_free (rename_map_elt, heap, a.new_renames);
}

/* Helper structure for htab_traverse in insert_guard_phis.  */

struct igp {
  basic_block bb;
  edge true_edge, false_edge;
  htab_t before_guard;
};

/* Return the default name that is before the guard.  */

static tree
default_before_guard (htab_t before_guard, tree old_name)
{
  tree res = get_rename (before_guard, old_name);

  if (res == old_name)
    {
      if (is_gimple_reg (res))
	return fold_convert (TREE_TYPE (res), integer_zero_node);
      return gimple_default_def (cfun, SSA_NAME_VAR (res));
    }

  return res;
}

/* Prepares EXPR to be a good phi argument when the phi result is
   RES.  Insert needed statements on edge E.  */

static tree
convert_for_phi_arg (tree expr, tree res, edge e)
{
  tree type = TREE_TYPE (res);

  if (TREE_TYPE (expr) != type)
    expr = fold_convert (type, expr);

  if (TREE_CODE (expr) != SSA_NAME
      && !is_gimple_min_invariant (expr))
    {
      tree var = create_tmp_var (type, "var");
      gimple_seq stmts;

      expr = build2 (MODIFY_EXPR, type, var, expr);
      expr = force_gimple_operand (expr, &stmts, true, NULL);
      gsi_insert_seq_on_edge_immediate (e, stmts);
    }

  return expr;
}

/* Helper function for htab_traverse in insert_guard_phis.  */

static int
add_guard_exit_phis (void **slot, void *s)
{
  struct rename_map_elt_s *entry = (struct rename_map_elt_s *) *slot;
  struct igp *i = (struct igp *) s;
  basic_block bb = i->bb;
  edge true_edge = i->true_edge;
  edge false_edge = i->false_edge;
  tree res = entry->old_name;
  tree name1 = entry->expr;
  tree name2 = default_before_guard (i->before_guard, res);
  gimple phi;

  /* Nothing to be merged if the name before the guard is the same as
     the one after.  */
  if (name1 == name2)
    return 1;

  name1 = convert_for_phi_arg (name1, res, true_edge);
  name2 = convert_for_phi_arg (name2, res, false_edge);

  phi = create_phi_node (res, bb);
  res = create_new_def_for (gimple_phi_result (phi), phi,
			    gimple_phi_result_ptr (phi));

  add_phi_arg (phi, name1, true_edge, UNKNOWN_LOCATION);
  add_phi_arg (phi, name2, false_edge, UNKNOWN_LOCATION);

  entry->expr = res;
  *slot = entry;
  return 1;
}

/* Iterate over RENAME_MAP and get tuples of the form (OLD, NAME1).
   If there is a correspondent tuple (OLD, NAME2) in BEFORE_GUARD,
   with NAME1 different than NAME2, then insert in BB the phi node:

   | RES = phi (NAME1 (on TRUE_EDGE), NAME2 (on FALSE_EDGE))"

   if there is no tuple for OLD in BEFORE_GUARD, insert

   | RES = phi (NAME1 (on TRUE_EDGE),
   |            DEFAULT_DEFINITION of NAME1 (on FALSE_EDGE))".

   Finally register in RENAME_MAP the tuple (OLD, RES).  */

void
insert_guard_phis (basic_block bb, edge true_edge, edge false_edge,
		   htab_t before_guard, htab_t rename_map)
{
  struct igp i;
  i.bb = bb;
  i.true_edge = true_edge;
  i.false_edge = false_edge;
  i.before_guard = before_guard;

  update_ssa (TODO_update_ssa);
  htab_traverse (rename_map, add_guard_exit_phis, &i);
  update_ssa (TODO_update_ssa);
}

/* Create a duplicate of the basic block BB.  NOTE: This does not
   preserve SSA form.  */

static void
graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb, htab_t map)
{
  gimple_stmt_iterator gsi, gsi_tgt;

  gsi_tgt = gsi_start_bb (new_bb);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      def_operand_p def_p;
      ssa_op_iter op_iter;
      gimple stmt = gsi_stmt (gsi);
      gimple copy;

      if (gimple_code (stmt) == GIMPLE_LABEL)
	continue;

      /* Create a new copy of STMT and duplicate STMT's virtual
	 operands.  */
      copy = gimple_copy (stmt);
      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);
      mark_sym_for_renaming (gimple_vop (cfun));

      maybe_duplicate_eh_stmt (copy, stmt);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* Create new names for all the definitions created by COPY and
	 add replacement mappings for each new name.  */
      FOR_EACH_SSA_DEF_OPERAND (def_p, copy, op_iter, SSA_OP_ALL_DEFS)
	{
	  tree old_name = DEF_FROM_PTR (def_p);
	  tree new_name = create_new_def_for (old_name, copy, def_p);
	  set_rename (map, old_name, new_name);
	}
    }
}

/* Copies BB and includes in the copied BB all the statements that can
   be reached following the use-def chains from the memory accesses,
   and returns the next edge following this new block.  */

edge
copy_bb_and_scalar_dependences (basic_block bb, sese region,
				edge next_e, htab_t map)
{
  basic_block new_bb = split_edge (next_e);

  next_e = single_succ_edge (new_bb);
  graphite_copy_stmts_from_block (bb, new_bb, map);
  remove_condition (new_bb);
  remove_phi_nodes (new_bb);
  expand_scalar_variables (new_bb, region, map);
  rename_variables (new_bb, map);

  return next_e;
}

/* Returns the outermost loop in SCOP that contains BB.  */

struct loop *
outermost_loop_in_sese (sese region, basic_block bb)
{
  struct loop *nest;

  nest = bb->loop_father;
  while (loop_outer (nest)
	 && loop_in_sese_p (loop_outer (nest), region))
    nest = loop_outer (nest);

  return nest;
}

/* Sets the false region of an IF_REGION to REGION.  */

void
if_region_set_false_region (ifsese if_region, sese region)
{
  basic_block condition = if_region_get_condition_block (if_region);
  edge false_edge = get_false_edge_from_guard_bb (condition);
  basic_block dummy = false_edge->dest;
  edge entry_region = SESE_ENTRY (region);
  edge exit_region = SESE_EXIT (region);
  basic_block before_region = entry_region->src;
  basic_block last_in_region = exit_region->src;
  void **slot = htab_find_slot_with_hash (current_loops->exits, exit_region,
					  htab_hash_pointer (exit_region),
					  NO_INSERT);

  entry_region->flags = false_edge->flags;
  false_edge->flags = exit_region->flags;

  redirect_edge_pred (entry_region, condition);
  redirect_edge_pred (exit_region, before_region);
  redirect_edge_pred (false_edge, last_in_region);
  redirect_edge_succ (false_edge, single_succ (dummy));
  delete_basic_block (dummy);

  exit_region->flags = EDGE_FALLTHRU;
  recompute_all_dominators ();

  SESE_EXIT (region) = false_edge;

  if (if_region->false_region)
    free (if_region->false_region);
  if_region->false_region = region;

  if (slot)
    {
      struct loop_exit *loop_exit = GGC_CNEW (struct loop_exit);

      memcpy (loop_exit, *((struct loop_exit **) slot), sizeof (struct loop_exit));
      htab_clear_slot (current_loops->exits, slot);

      slot = htab_find_slot_with_hash (current_loops->exits, false_edge,
				       htab_hash_pointer (false_edge),
				       INSERT);
      loop_exit->e = false_edge;
      *slot = loop_exit;
      false_edge->src->loop_father->exits->next = loop_exit;
    }
}

/* Creates an IFSESE with CONDITION on edge ENTRY.  */

ifsese
create_if_region_on_edge (edge entry, tree condition)
{
  edge e;
  edge_iterator ei;
  sese sese_region = XNEW (struct sese_s);
  sese true_region = XNEW (struct sese_s);
  sese false_region = XNEW (struct sese_s);
  ifsese if_region = XNEW (struct ifsese_s);
  edge exit = create_empty_if_region_on_edge (entry, condition);

  if_region->region = sese_region;
  if_region->region->entry = entry;
  if_region->region->exit = exit;

  FOR_EACH_EDGE (e, ei, entry->dest->succs)
    {
      if (e->flags & EDGE_TRUE_VALUE)
	{
	  true_region->entry = e;
	  true_region->exit = single_succ_edge (e->dest);
	  if_region->true_region = true_region;
	}
      else if (e->flags & EDGE_FALSE_VALUE)
	{
	  false_region->entry = e;
	  false_region->exit = single_succ_edge (e->dest);
	  if_region->false_region = false_region;
	}
    }

  return if_region;
}

/* Moves REGION in a condition expression:
   | if (1)
   |   ;
   | else
   |   REGION;
*/

ifsese
move_sese_in_condition (sese region)
{
  basic_block pred_block = split_edge (SESE_ENTRY (region));
  ifsese if_region;

  SESE_ENTRY (region) = single_succ_edge (pred_block);
  if_region = create_if_region_on_edge (single_pred_edge (pred_block), integer_one_node);
  if_region_set_false_region (if_region, region);

  return if_region;
}

/* Replaces the condition of the IF_REGION with CONDITION:
   | if (CONDITION)
   |   true_region;
   | else
   |   false_region;
*/

void
set_ifsese_condition (ifsese if_region, tree condition)
{
  sese region = if_region->region;
  edge entry = region->entry;
  basic_block bb = entry->dest;
  gimple last = last_stmt (bb);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gimple cond_stmt;

  gcc_assert (gimple_code (last) == GIMPLE_COND);

  gsi_remove (&gsi, true);
  gsi = gsi_last_bb (bb);
  condition = force_gimple_operand_gsi (&gsi, condition, true, NULL,
					false, GSI_NEW_STMT);
  cond_stmt = gimple_build_cond_from_tree (condition, NULL_TREE, NULL_TREE);
  gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);
}

/* Returns the scalar evolution of T in REGION.  Every variable that
   is not defined in the REGION is considered a parameter.  */

tree
scalar_evolution_in_region (sese region, loop_p loop, tree t)
{
  gimple def;
  struct loop *def_loop;
  basic_block before = block_before_sese (region);

  if (TREE_CODE (t) != SSA_NAME
      || loop_in_sese_p (loop, region))
    return instantiate_scev (before, loop,
			     analyze_scalar_evolution (loop, t));

  if (!defined_in_sese_p (t, region))
    return t;

  def = SSA_NAME_DEF_STMT (t);
  def_loop = loop_containing_stmt (def);

  if (loop_in_sese_p (def_loop, region))
    {
      t = analyze_scalar_evolution (def_loop, t);
      def_loop = superloop_at_depth (def_loop, loop_depth (loop) + 1);
      t = compute_overall_effect_of_inner_loop (def_loop, t);
      return t;
    }
  else
    return instantiate_scev (before, loop, t);
}
