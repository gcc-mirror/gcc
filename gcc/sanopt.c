/* Optimize and expand sanitizer functions.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-pass.h"
#include "asan.h"
#include "gimple-pretty-print.h"
#include "tm_p.h"
#include "langhooks.h"
#include "ubsan.h"
#include "params.h"
#include "tree-ssa-operands.h"
#include "tree-hash-traits.h"


/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

struct sanopt_info
{
  /* True if this BB might call (directly or indirectly) free/munmap
     or similar operation.  */
  bool has_freeing_call_p;

  /* True if HAS_FREEING_CALL_P flag has been computed.  */
  bool has_freeing_call_computed_p;

  /* True if there is a block with HAS_FREEING_CALL_P flag set
     on any path between an immediate dominator of BB, denoted
     imm(BB), and BB.  */
  bool imm_dom_path_with_freeing_call_p;

  /* True if IMM_DOM_PATH_WITH_FREEING_CALL_P has been computed.  */
  bool imm_dom_path_with_freeing_call_computed_p;

  /* Number of possibly freeing calls encountered in this bb
     (so far).  */
  uint64_t freeing_call_events;

  /* True if BB is currently being visited during computation
     of IMM_DOM_PATH_WITH_FREEING_CALL_P flag.  */
  bool being_visited_p;

  /* True if this BB has been visited in the dominator walk.  */
  bool visited_p;
};

/* If T has a single definition of form T = T2, return T2.  */

static tree
maybe_get_single_definition (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      gimple g = SSA_NAME_DEF_STMT (t);
      if (gimple_assign_single_p (g))
	return gimple_assign_rhs1 (g);
    }
  return NULL_TREE;
}

/* Tree triplet for vptr_check_map.  */
struct sanopt_tree_triplet
{
  tree t1, t2, t3;
};

/* Traits class for tree triplet hash maps below.  */

struct sanopt_tree_triplet_hash : typed_noop_remove <sanopt_tree_triplet>
{
  typedef sanopt_tree_triplet value_type;
  typedef sanopt_tree_triplet compare_type;

  static inline hashval_t
  hash (const sanopt_tree_triplet &ref)
  {
    inchash::hash hstate (0);
    inchash::add_expr (ref.t1, hstate);
    inchash::add_expr (ref.t2, hstate);
    inchash::add_expr (ref.t3, hstate);
    return hstate.end ();
  }

  static inline bool
  equal (const sanopt_tree_triplet &ref1, const sanopt_tree_triplet &ref2)
  {
    return operand_equal_p (ref1.t1, ref2.t1, 0)
	   && operand_equal_p (ref1.t2, ref2.t2, 0)
	   && operand_equal_p (ref1.t3, ref2.t3, 0);
  }

  static inline void
  mark_deleted (sanopt_tree_triplet &ref)
  {
    ref.t1 = reinterpret_cast<tree> (1);
  }

  static inline void
  mark_empty (sanopt_tree_triplet &ref)
  {
    ref.t1 = NULL;
  }

  static inline bool
  is_deleted (const sanopt_tree_triplet &ref)
  {
    return ref.t1 == (void *) 1;
  }

  static inline bool
  is_empty (const sanopt_tree_triplet &ref)
  {
    return ref.t1 == NULL;
  }
};

/* This is used to carry various hash maps and variables used
   in sanopt_optimize_walker.  */

struct sanopt_ctx
{
  /* This map maps a pointer (the first argument of UBSAN_NULL) to
     a vector of UBSAN_NULL call statements that check this pointer.  */
  hash_map<tree, auto_vec<gimple> > null_check_map;

  /* This map maps a pointer (the second argument of ASAN_CHECK) to
     a vector of ASAN_CHECK call statements that check the access.  */
  hash_map<tree_operand_hash, auto_vec<gimple> > asan_check_map;

  /* This map maps a tree triplet (the first, second and fourth argument
     of UBSAN_VPTR) to a vector of UBSAN_VPTR call statements that check
     that virtual table pointer.  */
  hash_map<sanopt_tree_triplet_hash, auto_vec<gimple> > vptr_check_map;

  /* Number of IFN_ASAN_CHECK statements.  */
  int asan_num_accesses;
};


/* Return true if there might be any call to free/munmap operation
   on any path in between DOM (which should be imm(BB)) and BB.  */

static bool
imm_dom_path_with_freeing_call (basic_block bb, basic_block dom)
{
  sanopt_info *info = (sanopt_info *) bb->aux;
  edge e;
  edge_iterator ei;

  if (info->imm_dom_path_with_freeing_call_computed_p)
    return info->imm_dom_path_with_freeing_call_p;

  info->being_visited_p = true;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      sanopt_info *pred_info = (sanopt_info *) e->src->aux;

      if (e->src == dom)
	continue;

      if ((pred_info->imm_dom_path_with_freeing_call_computed_p
	  && pred_info->imm_dom_path_with_freeing_call_p)
	  || (pred_info->has_freeing_call_computed_p
	      && pred_info->has_freeing_call_p))
	{
	  info->imm_dom_path_with_freeing_call_computed_p = true;
	  info->imm_dom_path_with_freeing_call_p = true;
	  info->being_visited_p = false;
	  return true;
	}
    }

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      sanopt_info *pred_info = (sanopt_info *) e->src->aux;

      if (e->src == dom)
	continue;

      if (pred_info->has_freeing_call_computed_p)
	continue;

      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (e->src); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);

	  if (is_gimple_call (stmt) && !nonfreeing_call_p (stmt))
	    {
	      pred_info->has_freeing_call_p = true;
	      break;
	    }
	}

      pred_info->has_freeing_call_computed_p = true;
      if (pred_info->has_freeing_call_p)
	{
	  info->imm_dom_path_with_freeing_call_computed_p = true;
	  info->imm_dom_path_with_freeing_call_p = true;
	  info->being_visited_p = false;
	  return true;
	}
    }

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (e->src == dom)
	continue;

      basic_block src;
      for (src = e->src; src != dom; )
	{
	  sanopt_info *pred_info = (sanopt_info *) src->aux;
	  if (pred_info->being_visited_p)
	    break;
	  basic_block imm = get_immediate_dominator (CDI_DOMINATORS, src);
	  if (imm_dom_path_with_freeing_call (src, imm))
	    {
	      info->imm_dom_path_with_freeing_call_computed_p = true;
	      info->imm_dom_path_with_freeing_call_p = true;
	      info->being_visited_p = false;
	      return true;
	    }
	  src = imm;
	}
    }

  info->imm_dom_path_with_freeing_call_computed_p = true;
  info->imm_dom_path_with_freeing_call_p = false;
  info->being_visited_p = false;
  return false;
}

/* Get the first dominating check from the list of stored checks.
   Non-dominating checks are silently dropped.  */

static gimple
maybe_get_dominating_check (auto_vec<gimple> &v)
{
  for (; !v.is_empty (); v.pop ())
    {
      gimple g = v.last ();
      sanopt_info *si = (sanopt_info *) gimple_bb (g)->aux;
      if (!si->visited_p)
	/* At this point we shouldn't have any statements
	   that aren't dominating the current BB.  */
	return g;
    }
  return NULL;
}

/* Optimize away redundant UBSAN_NULL calls.  */

static bool
maybe_optimize_ubsan_null_ifn (struct sanopt_ctx *ctx, gimple stmt)
{
  gcc_assert (gimple_call_num_args (stmt) == 3);
  tree ptr = gimple_call_arg (stmt, 0);
  tree cur_align = gimple_call_arg (stmt, 2);
  gcc_assert (TREE_CODE (cur_align) == INTEGER_CST);
  bool remove = false;

  auto_vec<gimple> &v = ctx->null_check_map.get_or_insert (ptr);
  gimple g = maybe_get_dominating_check (v);
  if (!g)
    {
      /* For this PTR we don't have any UBSAN_NULL stmts recorded, so there's
	 nothing to optimize yet.  */
      v.safe_push (stmt);
      return false;
    }

  /* We already have recorded a UBSAN_NULL check for this pointer. Perhaps we
     can drop this one.  But only if this check doesn't specify stricter
     alignment.  */

  tree align = gimple_call_arg (g, 2);
  int kind = tree_to_shwi (gimple_call_arg (g, 1));
  /* If this is a NULL pointer check where we had segv anyway, we can
     remove it.  */
  if (integer_zerop (align)
      && (kind == UBSAN_LOAD_OF
	  || kind == UBSAN_STORE_OF
	  || kind == UBSAN_MEMBER_ACCESS))
    remove = true;
  /* Otherwise remove the check in non-recovering mode, or if the
     stmts have same location.  */
  else if (integer_zerop (align))
    remove = (flag_sanitize_recover & SANITIZE_NULL) == 0
	      || flag_sanitize_undefined_trap_on_error
	      || gimple_location (g) == gimple_location (stmt);
  else if (tree_int_cst_le (cur_align, align))
    remove = (flag_sanitize_recover & SANITIZE_ALIGNMENT) == 0
	      || flag_sanitize_undefined_trap_on_error
	      || gimple_location (g) == gimple_location (stmt);

  if (!remove && gimple_bb (g) == gimple_bb (stmt)
      && tree_int_cst_compare (cur_align, align) == 0)
    v.pop ();

  if (!remove)
    v.safe_push (stmt);
  return remove;
}

/* Optimize away redundant UBSAN_VPTR calls.  The second argument
   is the value loaded from the virtual table, so rely on FRE to find out
   when we can actually optimize.  */

static bool
maybe_optimize_ubsan_vptr_ifn (struct sanopt_ctx *ctx, gimple stmt)
{
  gcc_assert (gimple_call_num_args (stmt) == 5);
  sanopt_tree_triplet triplet;
  triplet.t1 = gimple_call_arg (stmt, 0);
  triplet.t2 = gimple_call_arg (stmt, 1);
  triplet.t3 = gimple_call_arg (stmt, 3);

  auto_vec<gimple> &v = ctx->vptr_check_map.get_or_insert (triplet);
  gimple g = maybe_get_dominating_check (v);
  if (!g)
    {
      /* For this PTR we don't have any UBSAN_VPTR stmts recorded, so there's
	 nothing to optimize yet.  */
      v.safe_push (stmt);
      return false;
    }

  return true;
}

/* Returns TRUE if ASan check of length LEN in block BB can be removed
   if preceded by checks in V.  */

static bool
can_remove_asan_check (auto_vec<gimple> &v, tree len, basic_block bb)
{
  unsigned int i;
  gimple g;
  gimple to_pop = NULL;
  bool remove = false;
  basic_block last_bb = bb;
  bool cleanup = false;

  FOR_EACH_VEC_ELT_REVERSE (v, i, g)
    {
      basic_block gbb = gimple_bb (g);
      sanopt_info *si = (sanopt_info *) gbb->aux;
      if (gimple_uid (g) < si->freeing_call_events)
	{
	  /* If there is a potentially freeing call after g in gbb, we should
	     remove it from the vector, can't use in optimization.  */
	  cleanup = true;
	  continue;
	}

      tree glen = gimple_call_arg (g, 2);
      gcc_assert (TREE_CODE (glen) == INTEGER_CST);

      /* If we've checked only smaller length than we want to check now,
	 we can't remove the current stmt.  If g is in the same basic block,
	 we want to remove it though, as the current stmt is better.  */
      if (tree_int_cst_lt (glen, len))
	{
	  if (gbb == bb)
	    {
	      to_pop = g;
	      cleanup = true;
	    }
	  continue;
	}

      while (last_bb != gbb)
	{
	  /* Paths from last_bb to bb have been checked before.
	     gbb is necessarily a dominator of last_bb, but not necessarily
	     immediate dominator.  */
	  if (((sanopt_info *) last_bb->aux)->freeing_call_events)
	    break;

	  basic_block imm = get_immediate_dominator (CDI_DOMINATORS, last_bb);
	  gcc_assert (imm);
	  if (imm_dom_path_with_freeing_call (last_bb, imm))
	    break;

	  last_bb = imm;
	}
      if (last_bb == gbb)
	remove = true;
      break;
    }

  if (cleanup)
    {
      unsigned int j = 0, l = v.length ();
      for (i = 0; i < l; i++)
	if (v[i] != to_pop
	    && (gimple_uid (v[i])
		== ((sanopt_info *)
		    gimple_bb (v[i])->aux)->freeing_call_events))
	  {
	    if (i != j)
	      v[j] = v[i];
	    j++;
	  }
      v.truncate (j);
    }

  return remove;
}

/* Optimize away redundant ASAN_CHECK calls.  */

static bool
maybe_optimize_asan_check_ifn (struct sanopt_ctx *ctx, gimple stmt)
{
  gcc_assert (gimple_call_num_args (stmt) == 4);
  tree ptr = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  basic_block bb = gimple_bb (stmt);
  sanopt_info *info = (sanopt_info *) bb->aux;

  if (TREE_CODE (len) != INTEGER_CST)
    return false;
  if (integer_zerop (len))
    return false;

  gimple_set_uid (stmt, info->freeing_call_events);

  auto_vec<gimple> *ptr_checks = &ctx->asan_check_map.get_or_insert (ptr);

  tree base_addr = maybe_get_single_definition (ptr);
  auto_vec<gimple> *base_checks = NULL;
  if (base_addr)
    {
      base_checks = &ctx->asan_check_map.get_or_insert (base_addr);
      /* Original pointer might have been invalidated.  */
      ptr_checks = ctx->asan_check_map.get (ptr);
    }

  gimple g = maybe_get_dominating_check (*ptr_checks);
  gimple g2 = NULL;

  if (base_checks)
    /* Try with base address as well.  */
    g2 = maybe_get_dominating_check (*base_checks);

  if (g == NULL && g2 == NULL)
    {
      /* For this PTR we don't have any ASAN_CHECK stmts recorded, so there's
	 nothing to optimize yet.  */
      ptr_checks->safe_push (stmt);
      if (base_checks)
	base_checks->safe_push (stmt);
      return false;
    }

  bool remove = false;

  if (ptr_checks)
    remove = can_remove_asan_check (*ptr_checks, len, bb);

  if (!remove && base_checks)
    /* Try with base address as well.  */
    remove = can_remove_asan_check (*base_checks, len, bb);

  if (!remove)
    {
      ptr_checks->safe_push (stmt);
      if (base_checks)
	base_checks->safe_push (stmt);
    }

  return remove;
}

/* Try to optimize away redundant UBSAN_NULL and ASAN_CHECK calls.

   We walk blocks in the CFG via a depth first search of the dominator
   tree; we push unique UBSAN_NULL or ASAN_CHECK statements into a vector
   in the NULL_CHECK_MAP or ASAN_CHECK_MAP hash maps as we enter the
   blocks.  When leaving a block, we mark the block as visited; then
   when checking the statements in the vector, we ignore statements that
   are coming from already visited blocks, because these cannot dominate
   anything anymore.  CTX is a sanopt context.  */

static void
sanopt_optimize_walker (basic_block bb, struct sanopt_ctx *ctx)
{
  basic_block son;
  gimple_stmt_iterator gsi;
  sanopt_info *info = (sanopt_info *) bb->aux;
  bool asan_check_optimize = (flag_sanitize & SANITIZE_ADDRESS) != 0;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);
      bool remove = false;

      if (!is_gimple_call (stmt))
	{
	  /* Handle asm volatile or asm with "memory" clobber
	     the same as potentionally freeing call.  */
	  gasm *asm_stmt = dyn_cast <gasm *> (stmt);
	  if (asm_stmt
	      && asan_check_optimize
	      && (gimple_asm_clobbers_memory_p (asm_stmt)
		  || gimple_asm_volatile_p (asm_stmt)))
	    info->freeing_call_events++;
	  gsi_next (&gsi);
	  continue;
	}

      if (asan_check_optimize && !nonfreeing_call_p (stmt))
	info->freeing_call_events++;

      if (gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_UBSAN_NULL:
	    remove = maybe_optimize_ubsan_null_ifn (ctx, stmt);
	    break;
	  case IFN_UBSAN_VPTR:
	    remove = maybe_optimize_ubsan_vptr_ifn (ctx, stmt);
	    break;
	  case IFN_ASAN_CHECK:
	    if (asan_check_optimize)
	      remove = maybe_optimize_asan_check_ifn (ctx, stmt);
	    if (!remove)
	      ctx->asan_num_accesses++;
	    break;
	  default:
	    break;
	  }

      if (remove)
	{
	  /* Drop this check.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Optimizing out\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	  unlink_stmt_vdef (stmt);
	  gsi_remove (&gsi, true);
	}
      else
	gsi_next (&gsi);
    }

  if (asan_check_optimize)
    {
      info->has_freeing_call_p = info->freeing_call_events != 0;
      info->has_freeing_call_computed_p = true;
    }

  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    sanopt_optimize_walker (son, ctx);

  /* We're leaving this BB, so mark it to that effect.  */
  info->visited_p = true;
}

/* Try to remove redundant sanitizer checks in function FUN.  */

static int
sanopt_optimize (function *fun)
{
  struct sanopt_ctx ctx;
  ctx.asan_num_accesses = 0;

  /* Set up block info for each basic block.  */
  alloc_aux_for_blocks (sizeof (sanopt_info));

  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Recursively walk the dominator tree optimizing away
     redundant checks.  */
  sanopt_optimize_walker (ENTRY_BLOCK_PTR_FOR_FN (fun), &ctx);

  free_aux_for_blocks ();

  return ctx.asan_num_accesses;
}

/* Perform optimization of sanitize functions.  */

namespace {

const pass_data pass_data_sanopt =
{
  GIMPLE_PASS, /* type */
  "sanopt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_sanopt : public gimple_opt_pass
{
public:
  pass_sanopt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sanopt, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_sanitize; }
  virtual unsigned int execute (function *);

}; // class pass_sanopt

unsigned int
pass_sanopt::execute (function *fun)
{
  basic_block bb;
  int asan_num_accesses = 0;

  /* Try to remove redundant checks.  */
  if (optimize
      && (flag_sanitize
	  & (SANITIZE_NULL | SANITIZE_ALIGNMENT
	     | SANITIZE_ADDRESS | SANITIZE_VPTR)))
    asan_num_accesses = sanopt_optimize (fun);
  else if (flag_sanitize & SANITIZE_ADDRESS)
    {
      gimple_stmt_iterator gsi;
      FOR_EACH_BB_FN (bb, fun)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  {
 	    gimple stmt = gsi_stmt (gsi);
	    if (is_gimple_call (stmt) && gimple_call_internal_p (stmt)
		&& gimple_call_internal_fn (stmt) == IFN_ASAN_CHECK)
	      ++asan_num_accesses;
	  }
    }

  bool use_calls = ASAN_INSTRUMENTATION_WITH_CALL_THRESHOLD < INT_MAX
    && asan_num_accesses >= ASAN_INSTRUMENTATION_WITH_CALL_THRESHOLD;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  gimple stmt = gsi_stmt (gsi);
	  bool no_next = false;

	  if (!is_gimple_call (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if (gimple_call_internal_p (stmt))
	    {
	      enum internal_fn ifn = gimple_call_internal_fn (stmt);
	      switch (ifn)
		{
		case IFN_UBSAN_NULL:
		  no_next = ubsan_expand_null_ifn (&gsi);
		  break;
		case IFN_UBSAN_BOUNDS:
		  no_next = ubsan_expand_bounds_ifn (&gsi);
		  break;
		case IFN_UBSAN_OBJECT_SIZE:
		  no_next = ubsan_expand_objsize_ifn (&gsi);
		  break;
		case IFN_UBSAN_VPTR:
		  no_next = ubsan_expand_vptr_ifn (&gsi);
		  break;
		case IFN_ASAN_CHECK:
		  no_next = asan_expand_check_ifn (&gsi, use_calls);
		  break;
		default:
		  break;
		}
	    }
	  else if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	    {
	      tree callee = gimple_call_fndecl (stmt);
	      switch (DECL_FUNCTION_CODE (callee))
		{
		case BUILT_IN_UNREACHABLE:
		  if (flag_sanitize & SANITIZE_UNREACHABLE
		      && !lookup_attribute ("no_sanitize_undefined",
					    DECL_ATTRIBUTES (fun->decl)))
		    no_next = ubsan_instrument_unreachable (&gsi);
		  break;
		default:
		  break;
		}
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Expanded\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	      fprintf (dump_file, "\n");
	    }

	  if (!no_next)
	    gsi_next (&gsi);
	}
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_sanopt (gcc::context *ctxt)
{
  return new pass_sanopt (ctxt);
}
