/* Optimize and expand sanitizer functions.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.
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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pass.h"
#include "tree-ssa-operands.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "ubsan.h"
#include "tree-hash-traits.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cfghooks.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "varasm.h"

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

static gimple *
maybe_get_single_definition (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      gimple *g = SSA_NAME_DEF_STMT (t);
      if (gimple_assign_single_p (g))
	return g;
    }
  return NULL;
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

  static hashval_t
  hash (const sanopt_tree_triplet &ref)
  {
    inchash::hash hstate (0);
    inchash::add_expr (ref.t1, hstate);
    inchash::add_expr (ref.t2, hstate);
    inchash::add_expr (ref.t3, hstate);
    return hstate.end ();
  }

  static bool
  equal (const sanopt_tree_triplet &ref1, const sanopt_tree_triplet &ref2)
  {
    return operand_equal_p (ref1.t1, ref2.t1, 0)
	   && operand_equal_p (ref1.t2, ref2.t2, 0)
	   && operand_equal_p (ref1.t3, ref2.t3, 0);
  }

  static void
  mark_deleted (sanopt_tree_triplet &ref)
  {
    ref.t1 = reinterpret_cast<tree> (1);
  }

  static const bool empty_zero_p = true;

  static void
  mark_empty (sanopt_tree_triplet &ref)
  {
    ref.t1 = NULL;
  }

  static bool
  is_deleted (const sanopt_tree_triplet &ref)
  {
    return ref.t1 == reinterpret_cast<tree> (1);
  }

  static bool
  is_empty (const sanopt_tree_triplet &ref)
  {
    return ref.t1 == NULL;
  }
};

/* Tree couple for ptr_check_map.  */
struct sanopt_tree_couple
{
  tree ptr;
  bool pos_p;
};

/* Traits class for tree triplet hash maps below.  */

struct sanopt_tree_couple_hash : typed_noop_remove <sanopt_tree_couple>
{
  typedef sanopt_tree_couple value_type;
  typedef sanopt_tree_couple compare_type;

  static hashval_t
  hash (const sanopt_tree_couple &ref)
  {
    inchash::hash hstate (0);
    inchash::add_expr (ref.ptr, hstate);
    hstate.add_int (ref.pos_p);
    return hstate.end ();
  }

  static bool
  equal (const sanopt_tree_couple &ref1, const sanopt_tree_couple &ref2)
  {
    return operand_equal_p (ref1.ptr, ref2.ptr, 0)
	   && ref1.pos_p == ref2.pos_p;
  }

  static void
  mark_deleted (sanopt_tree_couple &ref)
  {
    ref.ptr = reinterpret_cast<tree> (1);
  }

  static const bool empty_zero_p = true;

  static void
  mark_empty (sanopt_tree_couple &ref)
  {
    ref.ptr = NULL;
  }

  static bool
  is_deleted (const sanopt_tree_couple &ref)
  {
    return ref.ptr == reinterpret_cast<tree> (1);
  }

  static bool
  is_empty (const sanopt_tree_couple &ref)
  {
    return ref.ptr == NULL;
  }
};

/* This is used to carry various hash maps and variables used
   in sanopt_optimize_walker.  */

class sanopt_ctx
{
public:
  /* This map maps a pointer (the first argument of UBSAN_NULL) to
     a vector of UBSAN_NULL call statements that check this pointer.  */
  hash_map<tree, auto_vec<gimple *> > null_check_map;

  /* This map maps a pointer (the second argument of ASAN_CHECK) to
     a vector of ASAN_CHECK call statements that check the access.  */
  hash_map<tree_operand_hash, auto_vec<gimple *> > asan_check_map;

  /* This map maps a tree triplet (the first, second and fourth argument
     of UBSAN_VPTR) to a vector of UBSAN_VPTR call statements that check
     that virtual table pointer.  */
  hash_map<sanopt_tree_triplet_hash, auto_vec<gimple *> > vptr_check_map;

  /* This map maps a couple (tree and boolean) to a vector of UBSAN_PTR
     call statements that check that pointer overflow.  */
  hash_map<sanopt_tree_couple_hash, auto_vec<gimple *> > ptr_check_map;

  /* Number of IFN_ASAN_CHECK statements.  */
  int asan_num_accesses;

  /* True when the current functions constains an ASAN_MARK.  */
  bool contains_asan_mark;
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
	  gimple *stmt = gsi_stmt (gsi);
	  gasm *asm_stmt;

	  if ((is_gimple_call (stmt) && !nonfreeing_call_p (stmt))
	      || ((asm_stmt = dyn_cast <gasm *> (stmt))
		  && (gimple_asm_clobbers_memory_p (asm_stmt)
		      || gimple_asm_volatile_p (asm_stmt))))
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

static gimple *
maybe_get_dominating_check (auto_vec<gimple *> &v)
{
  for (; !v.is_empty (); v.pop ())
    {
      gimple *g = v.last ();
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
maybe_optimize_ubsan_null_ifn (class sanopt_ctx *ctx, gimple *stmt)
{
  gcc_assert (gimple_call_num_args (stmt) == 3);
  tree ptr = gimple_call_arg (stmt, 0);
  tree cur_align = gimple_call_arg (stmt, 2);
  gcc_assert (TREE_CODE (cur_align) == INTEGER_CST);
  bool remove = false;

  auto_vec<gimple *> &v = ctx->null_check_map.get_or_insert (ptr);
  gimple *g = maybe_get_dominating_check (v);
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
	      || (flag_sanitize_trap & SANITIZE_NULL) != 0
	      || gimple_location (g) == gimple_location (stmt);
  else if (tree_int_cst_le (cur_align, align))
    remove = (flag_sanitize_recover & SANITIZE_ALIGNMENT) == 0
	      || (flag_sanitize_trap & SANITIZE_ALIGNMENT) != 0
	      || gimple_location (g) == gimple_location (stmt);

  if (!remove && gimple_bb (g) == gimple_bb (stmt)
      && tree_int_cst_compare (cur_align, align) == 0)
    v.pop ();

  if (!remove)
    v.safe_push (stmt);
  return remove;
}

/* Return true when pointer PTR for a given CUR_OFFSET is already sanitized
   in a given sanitization context CTX.  */

static bool
has_dominating_ubsan_ptr_check (sanopt_ctx *ctx, tree ptr,
				offset_int &cur_offset)
{
  bool pos_p = !wi::neg_p (cur_offset);
  sanopt_tree_couple couple;
  couple.ptr = ptr;
  couple.pos_p = pos_p;

  auto_vec<gimple *> &v = ctx->ptr_check_map.get_or_insert (couple);
  gimple *g = maybe_get_dominating_check (v);
  if (!g)
    return false;

  /* We already have recorded a UBSAN_PTR check for this pointer.  Perhaps we
     can drop this one.  But only if this check doesn't specify larger offset.
     */
  tree offset = gimple_call_arg (g, 1);
  gcc_assert (TREE_CODE (offset) == INTEGER_CST);
  offset_int ooffset = wi::sext (wi::to_offset (offset), POINTER_SIZE);

  if (pos_p)
    {
      if (wi::les_p (cur_offset, ooffset))
	return true;
    }
  else if (!pos_p && wi::les_p (ooffset, cur_offset))
    return true;

  return false;
}

/* Record UBSAN_PTR check of given context CTX.  Register pointer PTR on
   a given OFFSET that it's handled by GIMPLE STMT.  */

static void
record_ubsan_ptr_check_stmt (sanopt_ctx *ctx, gimple *stmt, tree ptr,
			     const offset_int &offset)
{
  sanopt_tree_couple couple;
  couple.ptr = ptr;
  couple.pos_p = !wi::neg_p (offset);

  auto_vec<gimple *> &v = ctx->ptr_check_map.get_or_insert (couple);
  v.safe_push (stmt);
}

/* Optimize away redundant UBSAN_PTR calls.  */

static bool
maybe_optimize_ubsan_ptr_ifn (sanopt_ctx *ctx, gimple *stmt)
{
  poly_int64 bitsize, pbitpos;
  machine_mode mode;
  int volatilep = 0, reversep, unsignedp = 0;
  tree offset;

  gcc_assert (gimple_call_num_args (stmt) == 2);
  tree ptr = gimple_call_arg (stmt, 0);
  tree off = gimple_call_arg (stmt, 1);

  if (TREE_CODE (off) != INTEGER_CST)
    return false;

  if (integer_zerop (off))
    return true;

  offset_int cur_offset = wi::sext (wi::to_offset (off), POINTER_SIZE);
  if (has_dominating_ubsan_ptr_check (ctx, ptr, cur_offset))
    return true;

  tree base = ptr;
  if (TREE_CODE (base) == ADDR_EXPR)
    {
      base = TREE_OPERAND (base, 0);

      HOST_WIDE_INT bitpos;
      base = get_inner_reference (base, &bitsize, &pbitpos, &offset, &mode,
				  &unsignedp, &reversep, &volatilep);
      if ((offset == NULL_TREE || TREE_CODE (offset) == INTEGER_CST)
	  && DECL_P (base)
	  && ((!VAR_P (base)
	       && TREE_CODE (base) != PARM_DECL
	       && TREE_CODE (base) != RESULT_DECL)
	      || !DECL_REGISTER (base))
	  && pbitpos.is_constant (&bitpos))
	{
	  offset_int expr_offset;
	  if (offset)
	    expr_offset = wi::to_offset (offset) + bitpos / BITS_PER_UNIT;
	  else
	    expr_offset = bitpos / BITS_PER_UNIT;
	  expr_offset = wi::sext (expr_offset, POINTER_SIZE);
	  offset_int total_offset = expr_offset + cur_offset;
	  if (total_offset != wi::sext (total_offset, POINTER_SIZE))
	    {
	      record_ubsan_ptr_check_stmt (ctx, stmt, ptr, cur_offset);
	      return false;
	    }

	  /* If BASE is a fixed size automatic variable or
	     global variable defined in the current TU, we don't have
	     to instrument anything if offset is within address
	     of the variable.  */
	  if ((VAR_P (base)
	       || TREE_CODE (base) == PARM_DECL
	       || TREE_CODE (base) == RESULT_DECL)
	      && DECL_SIZE_UNIT (base)
	      && TREE_CODE (DECL_SIZE_UNIT (base)) == INTEGER_CST
	      && (!is_global_var (base) || decl_binds_to_current_def_p (base)))
	    {
	      offset_int base_size = wi::to_offset (DECL_SIZE_UNIT (base));
	      if (!wi::neg_p (expr_offset)
		  && wi::les_p (total_offset, base_size))
		{
		  if (!wi::neg_p (total_offset)
		      && wi::les_p (total_offset, base_size))
		    return true;
		}
	    }

	  /* Following expression: UBSAN_PTR (&MEM_REF[ptr + x], y) can be
	     handled as follows:

	     1) sign (x) == sign (y), then check for dominating check of (x + y)
	     2) sign (x) != sign (y), then first check if we have a dominating
		check for ptr + x.  If so, then we have 2 situations:
		a) sign (x) == sign (x + y), here we are done, example:
		   UBSAN_PTR (&MEM_REF[ptr + 100], -50)
		b) check for dominating check of ptr + x + y.
	     */

	  bool sign_cur_offset = !wi::neg_p (cur_offset);
	  bool sign_expr_offset = !wi::neg_p (expr_offset);

	  tree base_addr
	    = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (base)), base);

	  bool add = false;
	  if (sign_cur_offset == sign_expr_offset)
	    {
	      if (has_dominating_ubsan_ptr_check (ctx, base_addr, total_offset))
		return true;
	      else
		add = true;
	    }
	  else
	    {
	      if (!has_dominating_ubsan_ptr_check (ctx, base_addr, expr_offset))
		; /* Don't record base_addr + expr_offset, it's not a guarding
		     check.  */
	      else
		{
		  bool sign_total_offset = !wi::neg_p (total_offset);
		  if (sign_expr_offset == sign_total_offset)
		    return true;
		  else
		    {
		      if (has_dominating_ubsan_ptr_check (ctx, base_addr,
							  total_offset))
			return true;
		      else
			add = true;
		    }
		}
	    }

	  /* Record a new dominating check for base_addr + total_offset.  */
	  if (add && !operand_equal_p (base, base_addr, 0))
	    record_ubsan_ptr_check_stmt (ctx, stmt, base_addr,
					 total_offset);
	}
    }

  /* For this PTR we don't have any UBSAN_PTR stmts recorded, so there's
     nothing to optimize yet.  */
  record_ubsan_ptr_check_stmt (ctx, stmt, ptr, cur_offset);

  return false;
}

/* Optimize away redundant UBSAN_VPTR calls.  The second argument
   is the value loaded from the virtual table, so rely on FRE to find out
   when we can actually optimize.  */

static bool
maybe_optimize_ubsan_vptr_ifn (class sanopt_ctx *ctx, gimple *stmt)
{
  gcc_assert (gimple_call_num_args (stmt) == 5);
  sanopt_tree_triplet triplet;
  triplet.t1 = gimple_call_arg (stmt, 0);
  triplet.t2 = gimple_call_arg (stmt, 1);
  triplet.t3 = gimple_call_arg (stmt, 3);

  auto_vec<gimple *> &v = ctx->vptr_check_map.get_or_insert (triplet);
  gimple *g = maybe_get_dominating_check (v);
  if (!g)
    {
      /* For this PTR we don't have any UBSAN_VPTR stmts recorded, so there's
	 nothing to optimize yet.  */
      v.safe_push (stmt);
      return false;
    }

  return true;
}

/* Checks whether value of T in CHECK and USE is the same.  */

static bool
same_value_p (gimple *check, gimple *use, tree t)
{
  tree check_vuse = gimple_vuse (check);
  tree use_vuse = gimple_vuse (use);

  if (TREE_CODE (t) == SSA_NAME
      || is_gimple_min_invariant (t)
      || ! use_vuse)
    return true;

  if (check_vuse == use_vuse)
    return true;

  return false;
}

/* Returns TRUE if ASan check of length LEN in block BB can be removed
   if preceded by checks in V.  */

static bool
can_remove_asan_check (auto_vec<gimple *> &v, tree len, basic_block bb,
		       gimple *base_stmt, tree base_addr)
{
  unsigned int i;
  gimple *g;
  gimple *to_pop = NULL;
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
      if (last_bb != gbb)
	break;
      // In case of base_addr residing in memory we also need to check aliasing
      remove = ! base_addr || same_value_p (g, base_stmt, base_addr);
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
maybe_optimize_asan_check_ifn (class sanopt_ctx *ctx, gimple *stmt)
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

  auto_vec<gimple *> *ptr_checks = &ctx->asan_check_map.get_or_insert (ptr);

  gimple *base_stmt = maybe_get_single_definition (ptr);
  tree base_addr = base_stmt ? gimple_assign_rhs1 (base_stmt) : NULL_TREE;
  auto_vec<gimple *> *base_checks = NULL;
  if (base_addr)
    {
      base_checks = &ctx->asan_check_map.get_or_insert (base_addr);
      /* Original pointer might have been invalidated.  */
      ptr_checks = ctx->asan_check_map.get (ptr);
    }

  gimple *g = maybe_get_dominating_check (*ptr_checks);
  gimple *g2 = NULL;

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
    remove = can_remove_asan_check (*ptr_checks, len, bb, NULL, NULL);

  if (!remove && base_checks)
    /* Try with base address as well.  */
    remove = can_remove_asan_check (*base_checks, len, bb, base_stmt,
				    base_addr);

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
sanopt_optimize_walker (basic_block bb, class sanopt_ctx *ctx)
{
  basic_block son;
  gimple_stmt_iterator gsi;
  sanopt_info *info = (sanopt_info *) bb->aux;
  bool asan_check_optimize
    = ((flag_sanitize & (SANITIZE_ADDRESS | SANITIZE_HWADDRESS)) != 0);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
    {
      gimple *stmt = gsi_stmt (gsi);
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

      /* If __asan_before_dynamic_init ("module"); is followed by
	 __asan_after_dynamic_init (); without intervening memory loads/stores,
	 there is nothing to guard, so optimize both away.  */
      if (asan_check_optimize
	  && gimple_call_builtin_p (stmt, BUILT_IN_ASAN_BEFORE_DYNAMIC_INIT))
	{
	  gcc_assert (!hwasan_sanitize_p ());
	  use_operand_p use;
	  gimple *use_stmt;
	  if (single_imm_use (gimple_vdef (stmt), &use, &use_stmt))
	    {
	      if (is_gimple_call (use_stmt)
		  && gimple_call_builtin_p (use_stmt,
					    BUILT_IN_ASAN_AFTER_DYNAMIC_INIT))
		{
		  unlink_stmt_vdef (use_stmt);
		  gimple_stmt_iterator gsi2 = gsi_for_stmt (use_stmt);
		  gsi_remove (&gsi2, true);
		  remove = true;
		}
	    }
	}

      if (gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_UBSAN_NULL:
	    remove = maybe_optimize_ubsan_null_ifn (ctx, stmt);
	    break;
	  case IFN_UBSAN_VPTR:
	    remove = maybe_optimize_ubsan_vptr_ifn (ctx, stmt);
	    break;
	  case IFN_UBSAN_PTR:
	    remove = maybe_optimize_ubsan_ptr_ifn (ctx, stmt);
	    break;
	  case IFN_HWASAN_CHECK:
	  case IFN_ASAN_CHECK:
	    if (asan_check_optimize)
	      remove = maybe_optimize_asan_check_ifn (ctx, stmt);
	    if (!remove)
	      ctx->asan_num_accesses++;
	    break;
	  case IFN_ASAN_MARK:
	    ctx->contains_asan_mark = true;
	    break;
	  default:
	    break;
	  }

      if (remove)
	{
	  /* Drop this check.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Optimizing out: ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	    }
	  unlink_stmt_vdef (stmt);
	  gsi_remove (&gsi, true);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Leaving: ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	    }

	  gsi_next (&gsi);
	}
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
sanopt_optimize (function *fun, bool *contains_asan_mark)
{
  class sanopt_ctx ctx;
  ctx.asan_num_accesses = 0;
  ctx.contains_asan_mark = false;

  /* Set up block info for each basic block.  */
  alloc_aux_for_blocks (sizeof (sanopt_info));

  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Recursively walk the dominator tree optimizing away
     redundant checks.  */
  sanopt_optimize_walker (ENTRY_BLOCK_PTR_FOR_FN (fun), &ctx);

  free_aux_for_blocks ();

  *contains_asan_mark = ctx.contains_asan_mark;
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
  bool gate (function *) final override
  {
    /* SANITIZE_RETURN is handled in the front-end.  When trapping,
       SANITIZE_UNREACHABLE is handled by builtin_decl_unreachable.  */
    unsigned int mask = SANITIZE_RETURN;
    if (flag_sanitize_trap & SANITIZE_UNREACHABLE)
      mask |= SANITIZE_UNREACHABLE;
    return flag_sanitize & ~mask;
  }
  unsigned int execute (function *) final override;

}; // class pass_sanopt

/* Sanitize all ASAN_MARK unpoison calls that are not reachable by a BB
   that contains an ASAN_MARK poison.  All these ASAN_MARK unpoison call
   can be removed as all variables are unpoisoned in a function prologue.  */

static void
sanitize_asan_mark_unpoison (void)
{
  /* 1) Find all BBs that contain an ASAN_MARK poison call.  */
  auto_bitmap with_poison;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (asan_mark_p (stmt, ASAN_MARK_POISON))
	    {
	      bitmap_set_bit (with_poison, bb->index);
	      break;
	    }
	}
    }

  auto_sbitmap poisoned (last_basic_block_for_fn (cfun) + 1);
  bitmap_clear (poisoned);
  /* We now treat with_poison as worklist.  */
  bitmap worklist = with_poison;

  /* 2) Propagate the information to all reachable blocks.  */
  while (!bitmap_empty_p (worklist))
    {
      unsigned i = bitmap_clear_first_set_bit (worklist);
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      gcc_assert (bb);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!bitmap_bit_p (poisoned, e->dest->index))
	  {
	    bitmap_set_bit (poisoned, e->dest->index);
	    bitmap_set_bit (worklist, e->dest->index);
	  }
    }

  /* 3) Iterate all BBs not included in POISONED BBs and remove unpoison
	ASAN_MARK preceding an ASAN_MARK poison (which can still happen).  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bitmap_bit_p (poisoned, bb->index))
	continue;

      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	    {
	      if (asan_mark_p (stmt, ASAN_MARK_POISON))
		break;
	      else
		{
		  if (dump_file)
		    fprintf (dump_file, "Removing ASAN_MARK unpoison\n");
		  unlink_stmt_vdef (stmt);
		  release_defs (stmt);
		  gsi_remove (&gsi, true);
		  continue;
		}
	    }

	  gsi_next (&gsi);
	}
    }
}

/* Return true when STMT is either ASAN_CHECK call or a call of a function
   that can contain an ASAN_CHECK.  */

static bool
maybe_contains_asan_check (gimple *stmt)
{
  if (is_gimple_call (stmt))
    {
      if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	return false;
      else
	return !(gimple_call_flags (stmt) & ECF_CONST);
    }
  else if (is_a<gasm *> (stmt))
    return true;

  return false;
}

/* Sanitize all ASAN_MARK poison calls that are not followed by an ASAN_CHECK
   call.  These calls can be removed.  */

static void
sanitize_asan_mark_poison (void)
{
  /* 1) Find all BBs that possibly contain an ASAN_CHECK.  */
  auto_bitmap with_check;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (maybe_contains_asan_check (stmt))
	    {
	      bitmap_set_bit (with_check, bb->index);
	      break;
	    }
	}
    }

  auto_sbitmap can_reach_check (last_basic_block_for_fn (cfun) + 1);
  bitmap_clear (can_reach_check);
  /* We now treat with_check as worklist.  */
  bitmap worklist = with_check;

  /* 2) Propagate the information to all definitions blocks.  */
  while (!bitmap_empty_p (worklist))
    {
      unsigned i = bitmap_clear_first_set_bit (worklist);
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      gcc_assert (bb);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!bitmap_bit_p (can_reach_check, e->src->index))
	  {
	    bitmap_set_bit (can_reach_check, e->src->index);
	    bitmap_set_bit (worklist, e->src->index);
	  }
    }

  /* 3) Iterate all BBs not included in CAN_REACH_CHECK BBs and remove poison
	ASAN_MARK not followed by a call to function having an ASAN_CHECK.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bitmap_bit_p (can_reach_check, bb->index))
	continue;

      gimple_stmt_iterator gsi;
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (maybe_contains_asan_check (stmt))
	    break;
	  else if (asan_mark_p (stmt, ASAN_MARK_POISON))
	    {
	      if (dump_file)
		fprintf (dump_file, "Removing ASAN_MARK poison\n");
	      unlink_stmt_vdef (stmt);
	      release_defs (stmt);
	      gimple_stmt_iterator gsi2 = gsi;
	      gsi_prev (&gsi);
	      gsi_remove (&gsi2, true);
	      continue;
	    }

	  gsi_prev (&gsi);
	}
    }
}

/* Rewrite all usages of tree OP which is a PARM_DECL with a VAR_DECL
   that is it's DECL_VALUE_EXPR.  */

static tree
rewrite_usage_of_param (tree *op, int *walk_subtrees, void *)
{
  if (TREE_CODE (*op) == PARM_DECL && DECL_HAS_VALUE_EXPR_P (*op))
    {
      *op = DECL_VALUE_EXPR (*op);
      *walk_subtrees = 0;
    }

  return NULL;
}

/* For a given function FUN, rewrite all addressable parameters so that
   a new automatic variable is introduced.  Right after function entry
   a parameter is assigned to the variable.  */

static void
sanitize_rewrite_addressable_params (function *fun)
{
  gimple *g;
  gimple_seq stmts = NULL;
  bool has_any_addressable_param = false;
  auto_vec<tree> clear_value_expr_list;

  for (tree arg = DECL_ARGUMENTS (current_function_decl);
       arg; arg = DECL_CHAIN (arg))
    {
      tree type = TREE_TYPE (arg);
      if (TREE_ADDRESSABLE (arg)
	  && !TREE_ADDRESSABLE (type)
	  && !TREE_THIS_VOLATILE (arg)
	  && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	{
	  TREE_ADDRESSABLE (arg) = 0;
	  DECL_NOT_GIMPLE_REG_P (arg) = 0;
	  /* The parameter is no longer addressable.  */
	  has_any_addressable_param = true;

	  /* Create a new automatic variable.  */
	  tree var = build_decl (DECL_SOURCE_LOCATION (arg),
				 VAR_DECL, DECL_NAME (arg), type);
	  TREE_ADDRESSABLE (var) = 1;
	  DECL_IGNORED_P (var) = 1;

	  gimple_add_tmp_var (var);

	  /* We skip parameters that have a DECL_VALUE_EXPR.  */
	  if (DECL_HAS_VALUE_EXPR_P (arg))
	    continue;

	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "Rewriting parameter whose address is taken: ");
	      print_generic_expr (dump_file, arg, dump_flags);
	      fputc ('\n', dump_file);
	    }

	  SET_DECL_PT_UID (var, DECL_PT_UID (arg));

	  /* Assign value of parameter to newly created variable.  */
	  if ((TREE_CODE (type) == COMPLEX_TYPE
	       || TREE_CODE (type) == VECTOR_TYPE))
	    {
	      /* We need to create a SSA name that will be used for the
		 assignment.  */
	      tree tmp = get_or_create_ssa_default_def (cfun, arg);
	      g = gimple_build_assign (var, tmp);
	      gimple_set_location (g, DECL_SOURCE_LOCATION (arg));
	      gimple_seq_add_stmt (&stmts, g);
	    }
	  else
	    {
	      g = gimple_build_assign (var, arg);
	      gimple_set_location (g, DECL_SOURCE_LOCATION (arg));
	      gimple_seq_add_stmt (&stmts, g);
	    }

	  if (target_for_debug_bind (arg))
	    {
	      g = gimple_build_debug_bind (arg, var, NULL);
	      gimple_seq_add_stmt (&stmts, g);
	      clear_value_expr_list.safe_push (arg);
	    }

	  DECL_HAS_VALUE_EXPR_P (arg) = 1;
	  SET_DECL_VALUE_EXPR (arg, var);
	}
    }

  if (!has_any_addressable_param)
    return;

  /* Replace all usages of PARM_DECLs with the newly
     created variable VAR.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_stmt_iterator it = gsi_for_stmt (stmt);
	  walk_gimple_stmt (&it, NULL, rewrite_usage_of_param, NULL);
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = dyn_cast<gphi *> (gsi_stmt (gsi));
          for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      hash_set<tree> visited_nodes;
	      walk_tree (gimple_phi_arg_def_ptr (phi, i),
			 rewrite_usage_of_param, NULL, &visited_nodes);
	    }
	}
    }

  /* Unset value expr for parameters for which we created debug bind
     expressions.  */
  for (tree arg : clear_value_expr_list)
    {
      DECL_HAS_VALUE_EXPR_P (arg) = 0;
      SET_DECL_VALUE_EXPR (arg, NULL_TREE);
    }

  /* Insert default assignments at the beginning of a function.  */
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (fun);
  entry_bb = split_edge (single_succ_edge (entry_bb));

  gimple_stmt_iterator gsi = gsi_start_bb (entry_bb);
  gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);
}

unsigned int
pass_sanopt::execute (function *fun)
{
  /* n.b. ASAN_MARK is used for both HWASAN and ASAN.
     asan_num_accesses is hence used to count either HWASAN_CHECK or ASAN_CHECK
     stuff.  This is fine because you can only have one of these active at a
     time.  */
  basic_block bb;
  int asan_num_accesses = 0;
  bool contains_asan_mark = false;
  int ret = 0;

  /* Try to remove redundant checks.  */
  if (optimize
      && (flag_sanitize
	  & (SANITIZE_NULL | SANITIZE_ALIGNMENT | SANITIZE_HWADDRESS
	     | SANITIZE_ADDRESS | SANITIZE_VPTR | SANITIZE_POINTER_OVERFLOW)))
    asan_num_accesses = sanopt_optimize (fun, &contains_asan_mark);
  else if (flag_sanitize & (SANITIZE_ADDRESS | SANITIZE_HWADDRESS))
    {
      gimple_stmt_iterator gsi;
      FOR_EACH_BB_FN (bb, fun)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);
	    if (gimple_call_internal_p (stmt, IFN_ASAN_CHECK))
	      ++asan_num_accesses;
	    else if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	      contains_asan_mark = true;
	  }
    }

  if (asan_num_accesses || contains_asan_mark || asan_sanitize_stack_p ()
      || hwasan_sanitize_stack_p ())
    asan_maybe_insert_dynamic_shadow_at_function_entry (fun);

  if (contains_asan_mark)
    {
      sanitize_asan_mark_unpoison ();
      sanitize_asan_mark_poison ();
    }

  if (asan_sanitize_stack_p () || hwasan_sanitize_stack_p ())
    sanitize_rewrite_addressable_params (fun);

  bool use_calls = param_asan_instrumentation_with_call_threshold < INT_MAX
    && asan_num_accesses >= param_asan_instrumentation_with_call_threshold;

  hash_map<tree, tree> shadow_vars_mapping;
  bool need_commit_edge_insert = false;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  gimple *stmt = gsi_stmt (gsi);
	  bool no_next = false;

	  if (!is_gimple_call (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if (gimple_call_internal_p (stmt))
	    {
	      enum internal_fn ifn = gimple_call_internal_fn (stmt);
	      int this_ret = TODO_cleanup_cfg;
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
		case IFN_UBSAN_PTR:
		  no_next = ubsan_expand_ptr_ifn (&gsi);
		  break;
		case IFN_UBSAN_VPTR:
		  no_next = ubsan_expand_vptr_ifn (&gsi);
		  break;
		case IFN_HWASAN_CHECK:
		  no_next = hwasan_expand_check_ifn (&gsi, use_calls);
		  break;
		case IFN_ASAN_CHECK:
		  no_next = asan_expand_check_ifn (&gsi, use_calls);
		  break;
		case IFN_ASAN_MARK:
		  no_next = asan_expand_mark_ifn (&gsi);
		  break;
		case IFN_ASAN_POISON:
		  no_next = asan_expand_poison_ifn (&gsi,
						    &need_commit_edge_insert,
						    shadow_vars_mapping);
		  break;
		case IFN_HWASAN_MARK:
		  no_next = hwasan_expand_mark_ifn (&gsi);
		  break;
		default:
		  this_ret = 0;
		  break;
		}
	      ret |= this_ret;
	    }
	  else if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	    {
	      tree callee = gimple_call_fndecl (stmt);
	      switch (DECL_FUNCTION_CODE (callee))
		{
		case BUILT_IN_UNREACHABLE:
		  if (sanitize_flags_p (SANITIZE_UNREACHABLE))
		    no_next = ubsan_instrument_unreachable (&gsi);
		  break;
		default:
		  break;
		}
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Expanded: ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	    }

	  if (!no_next)
	    gsi_next (&gsi);
	}
    }

  if (need_commit_edge_insert)
    gsi_commit_edge_inserts ();

  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_sanopt (gcc::context *ctxt)
{
  return new pass_sanopt (ctxt);
}
