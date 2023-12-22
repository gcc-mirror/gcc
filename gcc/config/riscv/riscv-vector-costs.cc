/* Cost model implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
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
	  int point = 1;
	  basic_block bb = bbs[i];
	  vec<stmt_point> program_points = vNULL;
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Compute local program points for bb %d:\n",
			     bb->index);
	  for (si = gsi_start_bb (bbs[i]); !gsi_end_p (si); gsi_next (&si))
	    {
	      if (!(is_gimple_assign (gsi_stmt (si))
		    || is_gimple_call (gsi_stmt (si))))
		continue;
	      stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (si));
	      enum stmt_vec_info_type type
		= STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
	      if (type != undef_vec_info_type)
		{
		  stmt_point info = {point, gsi_stmt (si)};
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
  const hash_map<basic_block, vec<stmt_point>> &program_points_per_bb,
  hash_map<basic_block, hash_map<tree, pair>> &live_ranges_per_bb)
{
  machine_mode biggest_mode = QImode;
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
	      if (lhs != NULL_TREE && is_gimple_reg (lhs)
		  && !POINTER_TYPE_P (TREE_TYPE (lhs)))
		{
		  biggest_mode = get_biggest_mode (biggest_mode,
						   TYPE_MODE (TREE_TYPE (lhs)));
		  bool existed_p = false;
		  pair &live_range
		    = live_ranges->get_or_insert (lhs, &existed_p);
		  gcc_assert (!existed_p);
		  live_range = pair (point, point);
		}
	      for (i = 0; i < gimple_num_args (stmt); i++)
		{
		  tree var = gimple_arg (stmt, i);
		  /* Both IMM and REG are included since a VECTOR_CST may be
		     potentially held in a vector register.  However, it's not
		     accurate, since a PLUS_EXPR can be vectorized into vadd.vi
		     if IMM is -16 ~ 15.

		     TODO: We may elide the cases that the unnecessary IMM in
		     the future.  */
		  if (poly_int_tree_p (var)
		      || (is_gimple_val (var)
			  && !POINTER_TYPE_P (TREE_TYPE (var))))
		    {
		      biggest_mode
			= get_biggest_mode (biggest_mode,
					    TYPE_MODE (TREE_TYPE (var)));
		      bool existed_p = false;
		      pair &live_range
			= live_ranges->get_or_insert (var, &existed_p);
		      if (existed_p)
			/* We will grow the live range for each use.  */
			live_range = pair (live_range.first, point);
		      else
			/* We assume the variable is live from the start of
			   this block.  */
			live_range = pair (0, point);
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
compute_nregs_for_mode (machine_mode mode, machine_mode biggest_mode, int lmul)
{
  unsigned int mode_size = GET_MODE_SIZE (mode).to_constant ();
  unsigned int biggest_size = GET_MODE_SIZE (biggest_mode).to_constant ();
  gcc_assert (biggest_size >= mode_size);
  unsigned int ratio = biggest_size / mode_size;
  return lmul / ratio;
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
max_number_of_live_regs (const basic_block bb,
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
	  machine_mode mode = TYPE_MODE (TREE_TYPE (var));
	  unsigned int nregs
	    = compute_nregs_for_mode (mode, biggest_mode, lmul);
	  live_vars_vec[i] += nregs;
	  if (live_vars_vec[i] > max_nregs)
	    max_nregs = live_vars_vec[i];
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

/* Return true if it is non-contiguous load/store.  */
static bool
non_contiguous_memory_access_p (stmt_vec_info stmt_info)
{
  enum stmt_vec_info_type type
    = STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
  return ((type == load_vec_info_type || type == store_vec_info_type)
	  && !adjacent_dr_p (STMT_VINFO_DATA_REF (stmt_info)));
}

/* Return the LMUL of the current analysis.  */
static int
compute_estimated_lmul (loop_vec_info other_loop_vinfo, machine_mode mode)
{
  gcc_assert (GET_MODE_BITSIZE (mode).is_constant ());
  int regno_alignment
    = riscv_get_v_regno_alignment (other_loop_vinfo->vector_mode);
  if (known_eq (LOOP_VINFO_SLP_UNROLLING_FACTOR (other_loop_vinfo), 1U))
    {
      int estimated_vf = vect_vf_for_cost (other_loop_vinfo);
      return estimated_vf * GET_MODE_BITSIZE (mode).to_constant ()
	     / TARGET_MIN_VLEN;
    }
  else if (regno_alignment > 1)
    return regno_alignment;
  else
    {
      int ratio;
      if (can_div_trunc_p (BYTES_PER_RISCV_VECTOR,
			   LOOP_VINFO_SLP_UNROLLING_FACTOR (other_loop_vinfo),
			   &ratio))
	return TARGET_MAX_LMUL / ratio;
      else
	gcc_unreachable ();
    }
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
			  stmt_point info = {1, phi};
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
	  if (!(is_gimple_assign (gsi_stmt (si))
		|| is_gimple_call (gsi_stmt (si))))
	    continue;
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (si));
	  enum stmt_vec_info_type type
	    = STMT_VINFO_TYPE (vect_stmt_to_vectorize (stmt_info));
	  if (non_contiguous_memory_access_p (stmt_info))
	    {
	      /* For non-adjacent load/store STMT, we will potentially
		 convert it into:

		   1. MASK_LEN_GATHER_LOAD (..., perm indice).
		   2. Continguous load/store + VEC_PERM (..., perm indice)

		We will be likely using one more vector variable.  */
	      unsigned int max_point
		= (*program_points_per_bb.get (bb)).length () - 1;
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
	    }
	}
    }
}

/* Return true that the LMUL of new COST model is preferred.  */
static bool
preferred_new_lmul_p (loop_vec_info other_loop_vinfo)
{
  /* Compute local program points.
     It's a fast and effective computation.  */
  hash_map<basic_block, vec<stmt_point>> program_points_per_bb;
  compute_local_program_points (other_loop_vinfo, program_points_per_bb);

  /* Compute local live ranges.  */
  hash_map<basic_block, hash_map<tree, pair>> live_ranges_per_bb;
  machine_mode biggest_mode
    = compute_local_live_ranges (program_points_per_bb, live_ranges_per_bb);

  /* Update live ranges according to PHI.  */
  update_local_live_ranges (other_loop_vinfo, program_points_per_bb,
			    live_ranges_per_bb, &biggest_mode);

  int lmul = compute_estimated_lmul (other_loop_vinfo, biggest_mode);
  /* TODO: We calculate the maximum live vars base on current STMTS
     sequence.  We can support live range shrink if it can give us
     big improvement in the future.  */
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
	  /* We prefer larger LMUL unless it causes register spillings.  */
	  unsigned int nregs
	    = max_number_of_live_regs (bb, (*iter).second, max_point,
				       biggest_mode, lmul);
	  if (nregs > max_nregs)
	    max_nregs = nregs;
	}
      live_ranges_per_bb.empty ();
      return max_nregs > V_REG_NUM;
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
  return lmul > RVV_M1;
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
  /* Record the number of times that the vector loop would execute,
     if known.  */
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  auto scalar_niters = max_stmt_executions_int (loop);
  if (scalar_niters >= 0)
    {
      unsigned int vf = vect_vf_for_cost (loop_vinfo);
      if (LOOP_VINFO_LENS (loop_vinfo).is_empty ())
	m_num_vector_iterations = scalar_niters / vf;
      else
	m_num_vector_iterations = CEIL (scalar_niters, vf);
    }

  /* Detect whether we're vectorizing for VLA and should apply the unrolling
     heuristic described above m_unrolled_vls_niters.  */
  record_potential_vls_unrolling (loop_vinfo);
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
  if (bool (m_unrolled_vls_stmts) != bool (other->m_unrolled_vls_stmts))
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

  if (!LOOP_VINFO_NITERS_KNOWN_P (this_loop_vinfo)
      && riscv_autovec_lmul == RVV_DYNAMIC)
    {
      if (!riscv_v_ext_vector_mode_p (this_loop_vinfo->vector_mode))
	return false;
      bool post_dom_available_p = dom_info_available_p (CDI_POST_DOMINATORS);
      if (!post_dom_available_p)
	calculate_dominance_info (CDI_POST_DOMINATORS);
      bool preferred_p = preferred_new_lmul_p (other_loop_vinfo);
      if (!post_dom_available_p)
	free_dominance_info (CDI_POST_DOMINATORS);
      return preferred_p;
    }

  return vector_costs::better_main_loop_than_p (other);
}

unsigned
costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
		      stmt_vec_info stmt_info, slp_tree, tree vectype,
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

      m_analyzed_vinfo = true;
    }

  if (stmt_info)
    {
      /* If we're applying the VLA vs. VLS unrolling heuristic,
	 estimate the number of statements in the unrolled VLS
	 loop.  For simplicitly, we assume that one iteration of the
	 VLS loop would need the same number of statements
	 as one iteration of the VLA loop.  */
      if (where == vect_body && m_unrolled_vls_niters)
	m_unrolled_vls_stmts += count * m_unrolled_vls_niters;
    }

  return record_stmt_cost (stmt_info, where, count * stmt_cost);
}

void
costs::finish_cost (const vector_costs *scalar_costs)
{
  vector_costs::finish_cost (scalar_costs);
}

} // namespace riscv_vector
