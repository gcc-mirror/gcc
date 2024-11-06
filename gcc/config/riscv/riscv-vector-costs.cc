/* Cost model implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#define IN_TARGET_CODE 1

#define INCLUDE_MEMORY
#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "basic-block.h"
#include "rtl.h"
#include "gimple.h"
#include "targhooks.h"
#include "cfgloop.h"
#include "fold-const.h"
#include "tm_p.h"
#include "tree-vectorizer.h"
#include "gimple-iterator.h"
#include "bitmap.h"
#include "ssa.h"
#include "backend.h"
#include "tree-data-ref.h"
#include "tree-ssa-loop-niter.h"
#include "tree-hash-traits.h"

/* This file should be included last.  */
#include "riscv-vector-costs.h"

namespace riscv_vector {

/* Dynamic LMUL philosophy - Local linear-scan SSA live range based analysis
   determine LMUL

     - Collect all vectorize STMTs locally for each loop block.
     - Build program point based graph, ignore non-vectorize STMTs:

	   vectorize STMT 0 - point 0
	   scalar STMT 0 - ignore.
	   vectorize STMT 1 - point 1
	   ...
     - Compute the number of live V_REGs live at each program point
     - Determine LMUL in VECTOR COST model according to the program point
       which has maximum live V_REGs.

     Note:

     - BIGGEST_MODE is the biggest LMUL auto-vectorization element mode.
       It's important for mixed size auto-vectorization (Conversions, ... etc).
       E.g. For a loop that is vectorizing conversion of INT32 -> INT64.
       The biggest mode is DImode and LMUL = 8, LMUL = 4 for SImode.
       We compute the number live V_REGs at each program point according to
       this information.
     - We only compute program points and live ranges locally (within a block)
       since we just need to compute the number of live V_REGs at each program
       point and we are not really allocating the registers for each SSA.
       We can make the variable has another local live range in another block
       if it live out/live in to another block.  Such approach doesn't affect
       out accurate live range analysis.
     - Current analysis didn't consider any instruction scheduling which
       may improve the register pressure.  So we are conservatively doing the
       analysis which may end up with smaller LMUL.
       TODO: Maybe we could support a reasonable live range shrink algorithm
       which take advantage of instruction scheduling.
     - We may have these following possible autovec modes analysis:

	 1. M8 -> M4 -> M2 -> M1 (stop analysis here) -> MF2 -> MF4 -> MF8
	 2. M8 -> M1(M4) -> MF2(M2) -> MF4(M1) (stop analysis here) -> MF8(MF2)
	 3. M1(M8) -> MF2(M4) -> MF4(M2) -> MF8(M1)
*/

static bool
is_gimple_assign_or_call (gimple *stmt)
{
  return is_gimple_assign (stmt) || is_gimple_call (stmt);
}

/* Return the program point of 1st vectorized lanes statement.  */
static unsigned int
get_first_lane_point (const vec<stmt_point> program_points,
		      stmt_vec_info stmt_info)
{
  for (const auto program_point : program_points)
    if (program_point.stmt_info == DR_GROUP_FIRST_ELEMENT (stmt_info))
      return program_point.point;
  return 0;
}

/* Return the program point of last vectorized lanes statement.  */
static unsigned int
get_last_lane_point (const vec<stmt_point> program_points,
		     stmt_vec_info stmt_info)
{
  unsigned int max_point = 0;
  for (auto s = DR_GROUP_FIRST_ELEMENT (stmt_info); s != NULL;
       s = DR_GROUP_NEXT_ELEMENT (s))
    {
      for (const auto program_point : program_points)
	if (program_point.stmt_info == s && program_point.point > max_point)
	  max_point = program_point.point;
    }
  return max_point;
}

/* Return the last variable that is in the live range list.  */
static pair *
get_live_range (hash_map<tree, pair> *live_ranges, tree arg)
{
  auto *r = live_ranges->get (arg);
  if (r)
    return r;
  else
    {
      tree t = arg;
      gimple *def_stmt = NULL;
      while (t && TREE_CODE (t) == SSA_NAME && !r
	     && (def_stmt = SSA_NAME_DEF_STMT (t)))
	{
	  if (gimple_assign_cast_p (def_stmt))
	    {
	      t = gimple_assign_rhs1 (def_stmt);
	      r = live_ranges->get (t);
	      def_stmt = NULL;
	    }
	  else
	    /* FIXME: Currently we don't see any fold for
	       non-conversion statements.  */
	    t = NULL_TREE;
	}
      if (r)
	return r;
      else
	{
	  bool insert_p = live_ranges->put (arg, pair (0, 0));
	  gcc_assert (!insert_p);
	  return live_ranges->get (arg);
	}
    }
}

/* Collect all STMTs that are vectorized and compute their program points.
   Note that we don't care about the STMTs that are not vectorized and
   we only build the local graph (within a block) of program points.

   Loop:
     bb 2:
       STMT 1 (be vectorized)      -- point 0
       STMT 2 (not be vectorized)  -- ignored
       STMT 3 (be vectorized)      -- point 1
       STMT 4 (be vectorized)      -- point 2
       STMT 5 (be vectorized)      -- point 3
       ...
     bb 3:
       STMT 1 (be vectorized)      -- point 0
       STMT 2 (be vectorized)      -- point 1
       STMT 3 (not be vectorized)  -- ignored
       STMT 4 (not be vectorized)  -- ignored
       STMT 5 (be vectorized)      -- point 2
       ...
*/
static void
compute_local_program_points (
  vec_info *vinfo,
  hash_map<basic_block, vec<stmt_point>> &program_points_per_bb)
{
  if (loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (vinfo))
    {
      class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
      basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
      unsigned int nbbs = loop->num_nodes;
      gimple_stmt_iterator si;
      unsigned int i;
      /* Collect the stmts that is vectorized and mark their program point.  */
      for (i = 0; i < nbbs; i++)
	{
	  unsigned int point = 1;
	  basic_block bb = bbs[i];
	  vec<stmt_point> program_points = vNULL;
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Compute local program points for bb %d:\n",
			     bb->index);
	  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	    {
	      if (!is_gimple_assign_or_call (gsi_stmt (si)))
		continue;
	      stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (si));
	      enum stmt_vec_info_type type
		= STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
	      if (type != undef_vec_info_type)
		{
		  stmt_point info = {point, gsi_stmt (si), stmt_info};
		  program_points.safe_push (info);
		  point++;
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "program point %d: %G", info.point,
				     gsi_stmt (si));
		}
	    }
	  program_points_per_bb.put (bb, program_points);
	}
    }
}

static machine_mode
get_biggest_mode (machine_mode mode1, machine_mode mode2)
{
  unsigned int mode1_size = GET_MODE_BITSIZE (mode1).to_constant ();
  unsigned int mode2_size = GET_MODE_BITSIZE (mode2).to_constant ();
  return mode1_size >= mode2_size ? mode1 : mode2;
}

/* Return true if OP is invariant.  */

static bool
loop_invariant_op_p (class loop *loop,
		     tree op)
{
  if (is_gimple_constant (op))
    return true;
  if (SSA_NAME_IS_DEFAULT_DEF (op)
      || !flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (op))))
    return true;
  return false;
}

/* Return true if the variable should be counted into liveness.  */
static bool
variable_vectorized_p (class loop *loop, stmt_vec_info stmt_info, tree var,
		       bool lhs_p)
{
  if (!var)
    return false;
  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  enum stmt_vec_info_type type
    = STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
  if (is_gimple_call (stmt) && gimple_call_internal_p (stmt))
    {
      if (gimple_call_internal_fn (stmt) == IFN_MASK_STORE
	  || gimple_call_internal_fn (stmt) == IFN_MASK_LOAD)
	{
	  /* .MASK_LOAD (_5, 32B, _33)
			  ^    ^    ^
	     Only the 3rd argument will be vectorized and consume
	     a vector register.  */
	  if (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
	      || (is_gimple_reg (var) && !POINTER_TYPE_P (TREE_TYPE (var))))
	    return true;
	  else
	    return false;
	}
    }
  else if (is_gimple_assign (stmt))
    {
      tree_code tcode = gimple_assign_rhs_code (stmt);
      /* vi variant doesn't need to allocate such statement.
	 E.g. tmp_15 = _4 + 1; will be transformed into vadd.vi
	 so the INTEGER_CST '1' doesn't need a vector register.  */
      switch (tcode)
	{
	case PLUS_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case BIT_AND_EXPR:
	  return TREE_CODE (var) != INTEGER_CST
		 || !tree_fits_shwi_p (var)
		 || !IN_RANGE (tree_to_shwi (var), -16, 15);
	case MINUS_EXPR:
	  return TREE_CODE (var) != INTEGER_CST
		 || !tree_fits_shwi_p (var)
		 || !IN_RANGE (tree_to_shwi (var), -16, 15)
		 || gimple_assign_rhs1 (stmt) != var;
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  return gimple_assign_rhs2 (stmt) != var
		 || !loop_invariant_op_p (loop, var);
	default:
	  break;
	}
    }

  if (lhs_p)
    return is_gimple_reg (var)
	   && (!POINTER_TYPE_P (TREE_TYPE (var))
	       || type != store_vec_info_type);
  else
    return poly_int_tree_p (var)
	   || (is_gimple_val (var)
	       && (!POINTER_TYPE_P (TREE_TYPE (var))
		   || type != load_vec_info_type));
}

/* Compute local live ranges of each vectorized variable.
   Note that we only compute local live ranges (within a block) since
   local live ranges information is accurate enough for us to determine
   the LMUL/vectorization factor of the loop.

   Loop:
     bb 2:
       STMT 1               -- point 0
       STMT 2 (def SSA 1)   -- point 1
       STMT 3 (use SSA 1)   -- point 2
       STMT 4               -- point 3
     bb 3:
       STMT 1               -- point 0
       STMT 2               -- point 1
       STMT 3               -- point 2
       STMT 4 (use SSA 2)   -- point 3

   The live range of SSA 1 is [1, 3] in bb 2.
   The live range of SSA 2 is [0, 4] in bb 3.  */
static machine_mode
compute_local_live_ranges (
  loop_vec_info loop_vinfo,
  const hash_map<basic_block, vec<stmt_point>> &program_points_per_bb,
  hash_map<basic_block, hash_map<tree, pair>> &live_ranges_per_bb)
{
  machine_mode biggest_mode = QImode;
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  if (!program_points_per_bb.is_empty ())
    {
      auto_vec<tree> visited_vars;
      unsigned int i;
      for (hash_map<basic_block, vec<stmt_point>>::iterator iter
	   = program_points_per_bb.begin ();
	   iter != program_points_per_bb.end (); ++iter)
	{
	  basic_block bb = (*iter).first;
	  vec<stmt_point> program_points = (*iter).second;
	  bool existed_p = false;
	  hash_map<tree, pair> *live_ranges
	    = &live_ranges_per_bb.get_or_insert (bb, &existed_p);
	  gcc_assert (!existed_p);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Compute local live ranges for bb %d:\n",
			     bb->index);
	  for (const auto program_point : program_points)
	    {
	      unsigned int point = program_point.point;
	      gimple *stmt = program_point.stmt;
	      tree lhs = gimple_get_lhs (stmt);
	      if (variable_vectorized_p (loop, program_point.stmt_info, lhs,
					 true))
		{
		  biggest_mode = get_biggest_mode (biggest_mode,
						   TYPE_MODE (TREE_TYPE (lhs)));
		  bool existed_p = false;
		  pair &live_range
		    = live_ranges->get_or_insert (lhs, &existed_p);
		  gcc_assert (!existed_p);
		  if (STMT_VINFO_MEMORY_ACCESS_TYPE (program_point.stmt_info)
		      == VMAT_LOAD_STORE_LANES)
		    point = get_first_lane_point (program_points,
						  program_point.stmt_info);
		  live_range = pair (point, point);
		}
	      for (i = 0; i < gimple_num_args (stmt); i++)
		{
		  tree var = gimple_arg (stmt, i);
		  if (variable_vectorized_p (loop, program_point.stmt_info, var,
					     false))
		    {
		      biggest_mode
			= get_biggest_mode (biggest_mode,
					    TYPE_MODE (TREE_TYPE (var)));
		      bool existed_p = false;
		      pair &live_range
			= live_ranges->get_or_insert (var, &existed_p);
		      if (STMT_VINFO_MEMORY_ACCESS_TYPE (
			    program_point.stmt_info)
			  == VMAT_LOAD_STORE_LANES)
			point = get_last_lane_point (program_points,
						     program_point.stmt_info);
		      else if (existed_p)
			point = MAX (live_range.second, point);
		      if (existed_p)
			/* We will grow the live range for each use.  */
			live_range = pair (live_range.first, point);
		      else
			{
			  gimple *def_stmt;
			  if (TREE_CODE (var) == SSA_NAME
			      && (def_stmt = SSA_NAME_DEF_STMT (var))
			      && gimple_bb (def_stmt) == bb
			      && is_gimple_assign_or_call (def_stmt))
			    {
			      live_ranges->remove (var);
			      for (unsigned int j = 0;
				   j < gimple_num_args (def_stmt); j++)
				{
				  tree arg = gimple_arg (def_stmt, j);
				  auto *r = get_live_range (live_ranges, arg);
				  gcc_assert (r);
				  (*r).second = MAX (point, (*r).second);
				  biggest_mode = get_biggest_mode (
				    biggest_mode, TYPE_MODE (TREE_TYPE (arg)));
				}
			    }
			  else
			    /* The splat vector lives the whole block.  */
			    live_range = pair (0, program_points.length ());
			}
		    }
		}
	    }
	  if (dump_enabled_p ())
	    for (hash_map<tree, pair>::iterator iter = live_ranges->begin ();
		 iter != live_ranges->end (); ++iter)
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "%T: type = %T, start = %d, end = %d\n",
			       (*iter).first, TREE_TYPE ((*iter).first),
			       (*iter).second.first, (*iter).second.second);
	}
    }
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Biggest mode = %s\n",
		     GET_MODE_NAME (biggest_mode));
  return biggest_mode;
}

/* Compute the mode for MODE, BIGGEST_MODE and LMUL.

   E.g. If mode = SImode, biggest_mode = DImode, LMUL = M4.
	Then return RVVM4SImode (LMUL = 4, element mode = SImode).  */
static unsigned int
compute_nregs_for_mode (loop_vec_info loop_vinfo, machine_mode mode,
			machine_mode biggest_mode, int lmul)
{
  unsigned int rgroup_size = LOOP_VINFO_LENS (loop_vinfo).is_empty ()
			       ? 1
			       : LOOP_VINFO_LENS (loop_vinfo).length ();
  unsigned int mode_size = GET_MODE_SIZE (mode).to_constant ();
  unsigned int biggest_size = GET_MODE_SIZE (biggest_mode).to_constant ();
  gcc_assert (biggest_size >= mode_size);
  unsigned int ratio = biggest_size / mode_size;
  /* RVV mask bool modes always consume 1 vector register regardless LMUL.  */
  unsigned int nregs = mode == BImode ? 1 : lmul / ratio;
  return MAX (nregs, 1) * rgroup_size;
}

/* This function helps to determine whether current LMUL will cause
   potential vector register (V_REG) spillings according to live range
   information.

     - First, compute how many variable are alive of each program point
       in each bb of the loop.
     - Second, compute how many V_REGs are alive of each program point
       in each bb of the loop according the BIGGEST_MODE and the variable
       mode.
     - Third, Return the maximum V_REGs are alive of the loop.  */
static unsigned int
max_number_of_live_regs (loop_vec_info loop_vinfo, const basic_block bb,
			 const hash_map<tree, pair> &live_ranges,
			 unsigned int max_point, machine_mode biggest_mode,
			 int lmul)
{
  unsigned int max_nregs = 0;
  unsigned int i;
  unsigned int live_point = 0;
  auto_vec<unsigned int> live_vars_vec;
  live_vars_vec.safe_grow_cleared (max_point, true);
  for (hash_map<tree, pair>::iterator iter = live_ranges.begin ();
       iter != live_ranges.end (); ++iter)
    {
      tree var = (*iter).first;
      pair live_range = (*iter).second;
      for (i = live_range.first + 1; i <= live_range.second; i++)
	{
	  machine_mode mode;
	  if (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE)
	    mode = BImode;
	  /* Constants do not have a mode, just use the biggest so
	     compute_nregs will return 1.  */
	  else if (TREE_CODE (var) == INTEGER_CST)
	    mode = biggest_mode;
	  else
	    mode = TYPE_MODE (TREE_TYPE (var));
	  unsigned int nregs
	    = compute_nregs_for_mode (loop_vinfo, mode, biggest_mode, lmul);
	  live_vars_vec[i] += nregs;
	  if (live_vars_vec[i] > max_nregs)
	    {
	      max_nregs = live_vars_vec[i];
	      live_point = i;
	    }
	}
    }

  /* Collect user explicit RVV type.  */
  auto_vec<basic_block> all_preds
    = get_all_dominated_blocks (CDI_POST_DOMINATORS, bb);
  tree t;
  FOR_EACH_SSA_NAME (i, t, cfun)
    {
      machine_mode mode = TYPE_MODE (TREE_TYPE (t));
      if (!lookup_vector_type_attribute (TREE_TYPE (t))
	  && !riscv_v_ext_vls_mode_p (mode))
	continue;

      gimple *def = SSA_NAME_DEF_STMT (t);
      if (gimple_bb (def) && !all_preds.contains (gimple_bb (def)))
	continue;
      use_operand_p use_p;
      imm_use_iterator iterator;

      FOR_EACH_IMM_USE_FAST (use_p, iterator, t)
	{
	  if (!USE_STMT (use_p) || is_gimple_debug (USE_STMT (use_p))
	      || !dominated_by_p (CDI_POST_DOMINATORS, bb,
				  gimple_bb (USE_STMT (use_p))))
	    continue;

	  int regno_alignment = riscv_get_v_regno_alignment (mode);
	  max_nregs += regno_alignment;
	  if (dump_enabled_p ())
	    dump_printf_loc (
	      MSG_NOTE, vect_location,
	      "Explicit used SSA %T, vectype = %T, mode = %s, cause %d "
	      "V_REG live in bb %d at program point %d\n",
	      t, TREE_TYPE (t), GET_MODE_NAME (mode), regno_alignment,
	      bb->index, live_point);
	  break;
	}
    }

  if (dump_enabled_p ())
    dump_printf_loc (
      MSG_NOTE, vect_location,
      "Maximum lmul = %d, At most %d number of live V_REG at program "
      "point %d for bb %d\n",
      lmul, max_nregs, live_point, bb->index);
  return max_nregs;
}

/* Get STORE value.  */
static tree
get_store_value (gimple *stmt)
{
  if (is_gimple_call (stmt) && gimple_call_internal_p (stmt))
    {
      if (gimple_call_internal_fn (stmt) == IFN_MASK_STORE)
	return gimple_call_arg (stmt, 3);
      else
	gcc_unreachable ();
    }
  else
    return gimple_assign_rhs1 (stmt);
}

/* Return true if additional vector vars needed.  */
static bool
need_additional_vector_vars_p (stmt_vec_info stmt_info)
{
  enum stmt_vec_info_type type
    = STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
  if (type == load_vec_info_type || type == store_vec_info_type)
    {
      if (STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	  && STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) == VMAT_GATHER_SCATTER)
	return true;

      machine_mode mode = TYPE_MODE (STMT_VINFO_VECTYPE (stmt_info));
      int lmul = riscv_get_v_regno_alignment (mode);
      if (DR_GROUP_SIZE (stmt_info) * lmul > RVV_M8)
	return true;
    }
  return false;
}

/* Return the LMUL of the current analysis.  */
static int
compute_estimated_lmul (loop_vec_info loop_vinfo, machine_mode mode)
{
  gcc_assert (GET_MODE_BITSIZE (mode).is_constant ());
  int regno_alignment = riscv_get_v_regno_alignment (loop_vinfo->vector_mode);
  if (riscv_v_ext_vls_mode_p (loop_vinfo->vector_mode))
    return regno_alignment;
  else if (known_eq (LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo), 1U))
    {
      int estimated_vf = vect_vf_for_cost (loop_vinfo);
      int estimated_lmul = estimated_vf * GET_MODE_BITSIZE (mode).to_constant ()
			   / TARGET_MIN_VLEN;
      if (estimated_lmul > RVV_M8)
	return regno_alignment;
      else
	return estimated_lmul;
    }
  else
    {
      /* Estimate the VLA SLP LMUL.  */
      if (regno_alignment > RVV_M1)
	return regno_alignment;
      else if (mode != QImode
	       || LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo).is_constant ())
	{
	  int ratio;
	  if (can_div_trunc_p (BYTES_PER_RISCV_VECTOR,
			       GET_MODE_SIZE (loop_vinfo->vector_mode), &ratio))
	    {
	      if (ratio == 1)
		return RVV_M4;
	      else if (ratio == 2)
		return RVV_M2;
	    }
	}
    }
  return 0;
}

/* Update the live ranges according PHI.

   Loop:
     bb 2:
       STMT 1               -- point 0
       STMT 2 (def SSA 1)   -- point 1
       STMT 3 (use SSA 1)   -- point 2
       STMT 4               -- point 3
     bb 3:
       SSA 2 = PHI<SSA 1>
       STMT 1               -- point 0
       STMT 2               -- point 1
       STMT 3 (use SSA 2)   -- point 2
       STMT 4               -- point 3

   Before this function, the SSA 1 live range is [2, 3] in bb 2
   and SSA 2 is [0, 3] in bb 3.

   Then, after this function, we update SSA 1 live range in bb 2
   into [2, 4] since SSA 1 is live out into bb 3.  */
static void
update_local_live_ranges (
  vec_info *vinfo,
  hash_map<basic_block, vec<stmt_point>> &program_points_per_bb,
  hash_map<basic_block, hash_map<tree, pair>> &live_ranges_per_bb,
  machine_mode *biggest_mode)
{
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (vinfo);
  if (!loop_vinfo)
    return;

  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  unsigned int i, j;
  gphi_iterator psi;
  gimple_stmt_iterator si;
  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Update local program points for bb %d:\n",
			 bbs[i]->index);
      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  gphi *phi = psi.phi ();
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (phi);
	  if (STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info))
	      == undef_vec_info_type)
	    continue;

	  for (j = 0; j < gimple_phi_num_args (phi); j++)
	    {
	      edge e = gimple_phi_arg_edge (phi, j);
	      tree def = gimple_phi_arg_def (phi, j);
	      auto *live_ranges = live_ranges_per_bb.get (bb);
	      auto *live_range = live_ranges->get (def);
	      if (poly_int_tree_p (def))
		{
		  /* Insert live range of INTEGER_CST or POLY_CST since we will
		     need to allocate a vector register for it.

		     E.g. # j_17 = PHI <j_11(9), 0(5)> will be transformed
		     into # vect_vec_iv_.8_24 = PHI <_25(9), { 0, ... }(5)>

		     The live range for such value is short which only lives
		     from program point 0 to 1.  */
		  if (live_range)
		    {
		      unsigned int start = (*live_range).first;
		      (*live_range).first = 0;
		      if (dump_enabled_p ())
			dump_printf_loc (
			  MSG_NOTE, vect_location,
			  "Update %T start point from %d to 0:\n", def, start);
		    }
		  else
		    {
		      live_ranges->put (def, pair (0, 1));
		      auto &program_points = (*program_points_per_bb.get (bb));
		      if (program_points.is_empty ())
			{
			  stmt_point info = {1, phi, stmt_info};
			  program_points.safe_push (info);
			}
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
					 "Add %T start point from 0 to 1:\n",
					 def);
		    }
		  continue;
		}
	      if (live_range && flow_bb_inside_loop_p (loop, e->src))
		{
		  unsigned int start = (*live_range).first;
		  (*live_range).first = 0;
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "Update %T start point from %d to %d:\n",
				     def, start, (*live_range).first);
		}
	      live_ranges = live_ranges_per_bb.get (e->src);
	      if (!program_points_per_bb.get (e->src))
		continue;
	      unsigned int max_point
		= (*program_points_per_bb.get (e->src)).length ();
	      live_range = live_ranges->get (def);
	      if (!live_range)
		continue;

	      unsigned int end = (*live_range).second;
	      (*live_range).second = max_point;
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Update %T end point from %d to %d:\n", def,
				 end, (*live_range).second);
	    }
	}
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  if (!is_gimple_assign_or_call (gsi_stmt (si)))
	    continue;
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (si));
	  enum stmt_vec_info_type type
	    = STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
	  if (need_additional_vector_vars_p (stmt_info))
	    {
	      /* For non-adjacent load/store STMT, we will potentially
		 convert it into:

		   1. MASK_LEN_GATHER_LOAD (..., perm indice).
		   2. Contiguous load/store + VEC_PERM (..., perm indice)

		We will be likely using one more vector variable.  */
	      unsigned int max_point
		= (*program_points_per_bb.get (bb)).length ();
	      auto *live_ranges = live_ranges_per_bb.get (bb);
	      bool existed_p = false;
	      tree var = type == load_vec_info_type
			   ? gimple_get_lhs (gsi_stmt (si))
			   : get_store_value (gsi_stmt (si));
	      tree sel_type = build_nonstandard_integer_type (
		TYPE_PRECISION (TREE_TYPE (var)), 1);
	      *biggest_mode
		= get_biggest_mode (*biggest_mode, TYPE_MODE (sel_type));
	      tree sel = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				     get_identifier ("vect_perm"), sel_type);
	      pair &live_range = live_ranges->get_or_insert (sel, &existed_p);
	      gcc_assert (!existed_p);
	      live_range = pair (0, max_point);
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Add perm indice %T, start = 0, end = %d\n",
				 sel, max_point);
	      if (!LOOP_VINFO_LENS (loop_vinfo).is_empty ()
		  && LOOP_VINFO_LENS (loop_vinfo).length () > 1)
		{
		  /* If we are vectorizing a permutation when the rgroup number
		     > 1, we will need additional mask to shuffle the second
		     vector.  */
		  tree mask = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					  get_identifier ("vect_perm_mask"),
					  boolean_type_node);
		  pair &live_range
		    = live_ranges->get_or_insert (mask, &existed_p);
		  gcc_assert (!existed_p);
		  live_range = pair (0, max_point);
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "Add perm mask %T, start = 0, end = %d\n",
				     mask, max_point);
		}
	    }
	}
    }
}

/* Compute the maximum live V_REGS.  */
static bool
has_unexpected_spills_p (loop_vec_info loop_vinfo)
{
  /* Compute local program points.
     It's a fast and effective computation.  */
  hash_map<basic_block, vec<stmt_point>> program_points_per_bb;
  compute_local_program_points (loop_vinfo, program_points_per_bb);

  /* Compute local live ranges.  */
  hash_map<basic_block, hash_map<tree, pair>> live_ranges_per_bb;
  machine_mode biggest_mode
    = compute_local_live_ranges (loop_vinfo, program_points_per_bb,
				 live_ranges_per_bb);

  /* Update live ranges according to PHI.  */
  update_local_live_ranges (loop_vinfo, program_points_per_bb,
			    live_ranges_per_bb, &biggest_mode);

  int lmul = compute_estimated_lmul (loop_vinfo, biggest_mode);
  gcc_assert (lmul <= RVV_M8);
  /* TODO: We calculate the maximum live vars base on current STMTS
     sequence.  We can support live range shrink if it can give us
     big improvement in the future.  */
  if (lmul > RVV_M1)
    {
      if (!live_ranges_per_bb.is_empty ())
	{
	  unsigned int max_nregs = 0;
	  for (hash_map<basic_block, hash_map<tree, pair>>::iterator iter
	       = live_ranges_per_bb.begin ();
	       iter != live_ranges_per_bb.end (); ++iter)
	    {
	      basic_block bb = (*iter).first;
	      unsigned int max_point
		= (*program_points_per_bb.get (bb)).length () + 1;
	      if ((*iter).second.is_empty ())
		continue;
	      /* We prefer larger LMUL unless it causes register spillings. */
	      unsigned int nregs
		= max_number_of_live_regs (loop_vinfo, bb, (*iter).second,
					   max_point, biggest_mode, lmul);
	      if (nregs > max_nregs)
		max_nregs = nregs;
	    }
	  live_ranges_per_bb.empty ();
	  if (max_nregs > V_REG_NUM)
	    return true;
	}
    }
  if (!program_points_per_bb.is_empty ())
    {
      for (hash_map<basic_block, vec<stmt_point>>::iterator iter
	   = program_points_per_bb.begin ();
	   iter != program_points_per_bb.end (); ++iter)
	{
	  vec<stmt_point> program_points = (*iter).second;
	  if (!program_points.is_empty ())
	    program_points.release ();
	}
      program_points_per_bb.empty ();
    }
  return false;
}

costs::costs (vec_info *vinfo, bool costing_for_scalar)
  : vector_costs (vinfo, costing_for_scalar)
{
  if (costing_for_scalar)
    m_cost_type = SCALAR_COST;
  else if (riscv_v_ext_vector_mode_p (vinfo->vector_mode))
    m_cost_type = VLA_VECTOR_COST;
  else
    m_cost_type = VLS_VECTOR_COST;
}

/* Do one-time initialization of the costs given that we're
   costing the loop vectorization described by LOOP_VINFO.  */
void
costs::analyze_loop_vinfo (loop_vec_info loop_vinfo)
{
  /* Detect whether we're vectorizing for VLA and should apply the unrolling
     heuristic described above m_unrolled_vls_niters.  */
  record_potential_vls_unrolling (loop_vinfo);

  /* Detect whether the LOOP has unexpected spills.  */
  record_potential_unexpected_spills (loop_vinfo);
}

/* Analyze the vectorized program statements and use dynamic LMUL
   heuristic to detect whether the loop has unexpected spills.  */
void
costs::record_potential_unexpected_spills (loop_vec_info loop_vinfo)
{
  /* We only want to apply the heuristic if LOOP_VINFO is being
     vectorized for VLA and known NITERS VLS loop.  */
  if (rvv_max_lmul == RVV_DYNAMIC
      && (m_cost_type == VLA_VECTOR_COST
	  || (m_cost_type == VLS_VECTOR_COST
	      && LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))))
    {
      bool post_dom_available_p = dom_info_available_p (CDI_POST_DOMINATORS);
      if (!post_dom_available_p)
	calculate_dominance_info (CDI_POST_DOMINATORS);
      m_has_unexpected_spills_p = has_unexpected_spills_p (loop_vinfo);
      if (!post_dom_available_p)
	free_dominance_info (CDI_POST_DOMINATORS);
    }
}

/* Decide whether to use the unrolling heuristic described above
   m_unrolled_vls_niters, updating that field if so.  LOOP_VINFO
   describes the loop that we're vectorizing.  */
void
costs::record_potential_vls_unrolling (loop_vec_info loop_vinfo)
{
  /* We only want to apply the heuristic if LOOP_VINFO is being
     vectorized for VLA.  */
  if (m_cost_type != VLA_VECTOR_COST)
    return;

  /* We don't want to apply the heuristic to outer loops, since it's
     harder to track two levels of unrolling.  */
  if (LOOP_VINFO_LOOP (loop_vinfo)->inner)
    return;

  /* Only handle cases in which the number of VLS iterations
     would be known at compile time but the number of SVE iterations
     would not.  */
  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || BYTES_PER_RISCV_VECTOR.is_constant ())
    return;

  /* Guess how many times the VLS loop would iterate and make
     sure that it is within the complete unrolling limit.  Even if the
     number of iterations is small enough, the number of statements might
     not be, which is why we need to estimate the number of statements too.  */
  unsigned int vls_vf = vect_vf_for_cost (loop_vinfo);
  unsigned HOST_WIDE_INT unrolled_vls_niters
    = LOOP_VINFO_INT_NITERS (loop_vinfo) / vls_vf;
  if (unrolled_vls_niters > (unsigned int) param_max_completely_peel_times)
    return;

  /* Record that we're applying the heuristic and should try to estimate
     the number of statements in the VLS loop.  */
  m_unrolled_vls_niters = unrolled_vls_niters;
}

/* Return true if (a) we're applying the VLS vs. VLA unrolling
   heuristic described above m_unrolled_vls_niters and (b) the heuristic
   says that we should prefer the VLS loop.  */
bool
costs::prefer_unrolled_loop () const
{
  if (!m_unrolled_vls_stmts)
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Number of insns in"
		     " unrolled VLS loop = " HOST_WIDE_INT_PRINT_UNSIGNED "\n",
		     m_unrolled_vls_stmts);

  /* The balance here is tricky.  On the one hand, we can't be sure whether
     the code is vectorizable with VLS or not.  However, even if
     it isn't vectorizable with VLS, there's a possibility that
     the scalar code could also be unrolled.  Some of the code might then
     benefit from SLP, or from using LDP and STP.  We therefore apply
     the heuristic regardless of can_use_vls_p.  */
  return (m_unrolled_vls_stmts
	  && (m_unrolled_vls_stmts
	      <= (unsigned int) param_max_completely_peeled_insns));
}

bool
costs::better_main_loop_than_p (const vector_costs *uncast_other) const
{
  auto other = static_cast<const costs *> (uncast_other);
  auto this_loop_vinfo = as_a<loop_vec_info> (this->m_vinfo);
  auto other_loop_vinfo = as_a<loop_vec_info> (other->m_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Comparing two main loops (%s at VF %d vs %s at VF %d)\n",
		     GET_MODE_NAME (this_loop_vinfo->vector_mode),
		     vect_vf_for_cost (this_loop_vinfo),
		     GET_MODE_NAME (other_loop_vinfo->vector_mode),
		     vect_vf_for_cost (other_loop_vinfo));

  /* Apply the unrolling heuristic described above m_unrolled_vls_niters.  */
  if (bool (m_unrolled_vls_stmts) != bool (other->m_unrolled_vls_stmts)
      && m_cost_type != other->m_cost_type)
    {
      bool this_prefer_unrolled = this->prefer_unrolled_loop ();
      bool other_prefer_unrolled = other->prefer_unrolled_loop ();
      if (this_prefer_unrolled != other_prefer_unrolled)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Preferring VLS loop because"
			     " it can be unrolled\n");
	  return other_prefer_unrolled;
	}
    }
  else if (rvv_max_lmul == RVV_DYNAMIC)
    {
      if (other->m_has_unexpected_spills_p)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Preferring smaller LMUL loop because"
			     " it has unexpected spills\n");
	  return true;
	}
      else if (riscv_v_ext_vector_mode_p (other_loop_vinfo->vector_mode))
	{
	  if (LOOP_VINFO_NITERS_KNOWN_P (other_loop_vinfo))
	    {
	      if (maybe_gt (LOOP_VINFO_INT_NITERS (this_loop_vinfo),
			    LOOP_VINFO_VECT_FACTOR (this_loop_vinfo)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "Keep current LMUL loop because"
				     " known NITERS exceed the new VF\n");
		  return false;
		}
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Keep current LMUL loop because"
				 " it is unknown NITERS\n");
	      return false;
	    }
	}
    }
  /* If NITERS is unknown, we should not use VLS modes to vectorize
     the loop since we don't support partial vectors for VLS modes,
     that is, we will have full vectors (VLSmodes) on loop body
     and partial vectors (VLAmodes) on loop epilogue which is very
     inefficient.  Instead, we should apply partial vectors (VLAmodes)
     on loop body without an epilogue on unknown NITERS loop.  */
  else if (!LOOP_VINFO_NITERS_KNOWN_P (this_loop_vinfo)
	   && m_cost_type == VLS_VECTOR_COST)
    return false;

  return vector_costs::better_main_loop_than_p (other);
}

/* Returns the group size i.e. the number of vectors to be loaded by a
   segmented load/store instruction.  Return 0 if it is no segmented
   load/store.  */
static int
segment_loadstore_group_size (enum vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info)
{
  if (stmt_info
      && (kind == vector_load || kind == vector_store)
      && STMT_VINFO_DATA_REF (stmt_info))
    {
      stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      if (stmt_info
	  && STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) == VMAT_LOAD_STORE_LANES)
	return DR_GROUP_SIZE (stmt_info);
    }
  return 0;
}

/* Adjust vectorization cost after calling riscv_builtin_vectorization_cost.
   For some statement, we would like to further fine-grain tweak the cost on
   top of riscv_builtin_vectorization_cost handling which doesn't have any
   information on statement operation codes etc.  */

unsigned
costs::adjust_stmt_cost (enum vect_cost_for_stmt kind, loop_vec_info loop,
			 stmt_vec_info stmt_info,
			 slp_tree, tree vectype, int stmt_cost)
{
  const cpu_vector_cost *costs = get_vector_costs ();
  switch (kind)
    {
    case scalar_to_vec:
      stmt_cost += (FLOAT_TYPE_P (vectype) ? costs->regmove->FR2VR
		    : costs->regmove->GR2VR);
      break;
    case vec_to_scalar:
      stmt_cost += (FLOAT_TYPE_P (vectype) ? costs->regmove->VR2FR
		    : costs->regmove->VR2GR);
      break;
    case vector_load:
    case vector_store:
	{
	  if (stmt_info && stmt_info->stmt && STMT_VINFO_DATA_REF (stmt_info))
	    {
	      /* Segment loads and stores.  When the group size is > 1
		 the vectorizer will add a vector load/store statement for
		 each vector in the group.  Here we additionally add permute
		 costs for each.  */
	      /* TODO: Indexed and ordered/unordered cost.  */
	      int group_size = segment_loadstore_group_size (kind, stmt_info);
	      if (group_size > 1)
		{
		  switch (group_size)
		    {
		    case 2:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_2;
		      else
			stmt_cost += costs->vls->segment_permute_2;
		      break;
		    case 3:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_3;
		      else
			stmt_cost += costs->vls->segment_permute_3;
		      break;
		    case 4:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_4;
		      else
			stmt_cost += costs->vls->segment_permute_4;
		      break;
		    case 5:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_5;
		      else
			stmt_cost += costs->vls->segment_permute_5;
		      break;
		    case 6:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_6;
		      else
			stmt_cost += costs->vls->segment_permute_6;
		      break;
		    case 7:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_7;
		      else
			stmt_cost += costs->vls->segment_permute_7;
		      break;
		    case 8:
		      if (riscv_v_ext_vector_mode_p (loop->vector_mode))
			stmt_cost += costs->vla->segment_permute_8;
		      else
			stmt_cost += costs->vls->segment_permute_8;
		      break;
		    default:
		      break;
		    }
		}
	      else
		{
		  /* Unit-stride vector loads and stores do not have offset
		     addressing as opposed to scalar loads and stores.
		     If the address depends on a variable we need an additional
		     add/sub for each load/store in the worst case.  */
		  data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
		  class loop *father = stmt_info->stmt->bb->loop_father;
		  if (!loop && father && !father->inner && father->superloops)
		    {
		      tree ref;
		      if (TREE_CODE (dr->ref) != MEM_REF
			  || !(ref = TREE_OPERAND (dr->ref, 0))
			  || TREE_CODE (ref) != SSA_NAME)
			break;

		      if (SSA_NAME_IS_DEFAULT_DEF (ref))
			break;

		      if (memrefs.contains ({ref, cst0}))
			break;

		      memrefs.add ({ref, cst0});

		      /* In case we have not seen REF before and the base
			 address is a pointer operation try a bit harder.  */
		      tree base = DR_BASE_ADDRESS (dr);
		      if (TREE_CODE (base) == POINTER_PLUS_EXPR
			  || TREE_CODE (base) == POINTER_DIFF_EXPR)
			{
			  /* Deconstruct BASE's first operand.  If it is a
			     binary operation, i.e. a base and an "offset"
			     store this pair.  Only increase the stmt_cost if
			     we haven't seen it before.  */
			  tree argp = TREE_OPERAND (base, 1);
			  typedef std::pair<tree, tree> addr_pair;
			  addr_pair pair;
			  if (TREE_CODE_CLASS (TREE_CODE (argp)) == tcc_binary)
			    {
			      tree argp0 = tree_strip_nop_conversions
				(TREE_OPERAND (argp, 0));
			      tree argp1 = TREE_OPERAND (argp, 1);
			      pair = addr_pair (argp0, argp1);
			      if (memrefs.contains (pair))
				break;

			      memrefs.add (pair);
			      stmt_cost
				+= builtin_vectorization_cost (scalar_stmt,
							       NULL_TREE, 0);
			    }
			}
		    }
		}
	    }
	  break;
	}

    default:
      break;
    }
  return stmt_cost;
}

unsigned
costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
		      stmt_vec_info stmt_info, slp_tree node, tree vectype,
		      int misalign, vect_cost_model_location where)
{
  int stmt_cost
    = targetm.vectorize.builtin_vectorization_cost (kind, vectype, misalign);

  /* Do one-time initialization based on the vinfo.  */
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo);
  if (!m_analyzed_vinfo)
    {
      if (loop_vinfo)
	analyze_loop_vinfo (loop_vinfo);

      memrefs.empty ();
      m_analyzed_vinfo = true;
    }

  if (stmt_info)
    {
      /* If we're applying the VLA vs. VLS unrolling heuristic,
	 estimate the number of statements in the unrolled VLS
	 loop.  For simplicity, we assume that one iteration of the
	 VLS loop would need the same number of statements
	 as one iteration of the VLA loop.  */
      if (where == vect_body && m_unrolled_vls_niters)
	m_unrolled_vls_stmts += count * m_unrolled_vls_niters;
    }

  if (vectype)
    stmt_cost = adjust_stmt_cost (kind, loop_vinfo, stmt_info, node, vectype,
				  stmt_cost);

  return record_stmt_cost (stmt_info, where, count * stmt_cost);
}

/* For some target specific vectorization cost which can't be handled per stmt,
   we check the requisite conditions and adjust the vectorization cost
   accordingly if satisfied.  One typical example is to model and adjust
   loop_len cost for known_lt (NITERS, VF).  */

void
costs::adjust_vect_cost_per_loop (loop_vec_info loop_vinfo)
{
  if (LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo)
      && !LOOP_VINFO_USING_DECREMENTING_IV_P (loop_vinfo))
    {
      /* In middle-end loop vectorizer, we don't count the loop_len cost in
	 vect_estimate_min_profitable_iters when NITERS < VF, that is, we only
	 count cost of len that we need to iterate loop more than once with VF.
	 It's correct for most of the cases:

	 E.g. VF = [4, 4]
	   for (int i = 0; i < 3; i ++)
	     a[i] += b[i];

	 We don't need to cost MIN_EXPR or SELECT_VL for the case above.

	 However, for some inefficient vectorized cases, it does use MIN_EXPR
	 to generate len.

	 E.g. VF = [256, 256]

	 Loop body:
	   # loop_len_110 = PHI <18(2), _119(11)>
	   ...
	   _117 = MIN_EXPR <ivtmp_114, 18>;
	   _118 = 18 - _117;
	   _119 = MIN_EXPR <_118, POLY_INT_CST [256, 256]>;
	   ...

	 Epilogue:
	   ...
	   _112 = .VEC_EXTRACT (vect_patt_27.14_109, _111);

	 We cost 1 unconditionally for this situation like other targets which
	 apply mask as the loop control.  */
      rgroup_controls *rgc;
      unsigned int num_vectors_m1;
      unsigned int body_stmts = 0;
      FOR_EACH_VEC_ELT (LOOP_VINFO_LENS (loop_vinfo), num_vectors_m1, rgc)
	if (rgc->type)
	  body_stmts += num_vectors_m1 + 1;

      add_stmt_cost (body_stmts, scalar_stmt, NULL, NULL, NULL_TREE, 0,
		     vect_body);
    }
}

void
costs::finish_cost (const vector_costs *scalar_costs)
{
  if (loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo))
    {
      adjust_vect_cost_per_loop (loop_vinfo);
    }
  vector_costs::finish_cost (scalar_costs);
}

} // namespace riscv_vector
