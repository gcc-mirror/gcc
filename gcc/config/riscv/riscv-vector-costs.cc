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
static hash_map<class loop *, autovec_info> loop_autovec_infos;

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
	  int point = 0;
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
  live_vars_vec.safe_grow_cleared (max_point + 1, true);
  for (hash_map<tree, pair>::iterator iter = live_ranges.begin ();
       iter != live_ranges.end (); ++iter)
    {
      tree var = (*iter).first;
      pair live_range = (*iter).second;
      for (i = live_range.first; i <= live_range.second; i++)
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
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Maximum lmul = %d, %d number of live V_REG at program "
		     "point %d for bb %d\n",
		     lmul, max_nregs, live_point, bb->index);
  return max_nregs;
}

/* Return the LMUL of the current analysis.  */
static int
get_current_lmul (class loop *loop)
{
  return loop_autovec_infos.get (loop)->current_lmul;
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
		= (*program_points_per_bb.get (e->src)).length () - 1;
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

costs::costs (vec_info *vinfo, bool costing_for_scalar)
  : vector_costs (vinfo, costing_for_scalar)
{}

/* Return true that the LMUL of new COST model is preferred.  */
bool
costs::preferred_new_lmul_p (const vector_costs *uncast_other) const
{
  auto other = static_cast<const costs *> (uncast_other);
  auto this_loop_vinfo = as_a<loop_vec_info> (this->m_vinfo);
  auto other_loop_vinfo = as_a<loop_vec_info> (other->m_vinfo);
  class loop *loop = LOOP_VINFO_LOOP (this_loop_vinfo);

  if (loop_autovec_infos.get (loop) && loop_autovec_infos.get (loop)->end_p)
    return false;
  else if (loop_autovec_infos.get (loop))
    loop_autovec_infos.get (loop)->current_lmul
      = loop_autovec_infos.get (loop)->current_lmul / 2;
  else
    {
      int regno_alignment
	= riscv_get_v_regno_alignment (other_loop_vinfo->vector_mode);
      if (known_eq (LOOP_VINFO_SLP_UNROLLING_FACTOR (other_loop_vinfo), 1U))
	regno_alignment = RVV_M8;
      loop_autovec_infos.put (loop, {regno_alignment, regno_alignment, false});
    }

  int lmul = get_current_lmul (loop);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Comparing two main loops (%s at VF %d vs %s at VF %d)\n",
		     GET_MODE_NAME (this_loop_vinfo->vector_mode),
		     vect_vf_for_cost (this_loop_vinfo),
		     GET_MODE_NAME (other_loop_vinfo->vector_mode),
		     vect_vf_for_cost (other_loop_vinfo));

  /* Compute local program points.
     It's a fast and effective computation.  */
  hash_map<basic_block, vec<stmt_point>> program_points_per_bb;
  compute_local_program_points (other->m_vinfo, program_points_per_bb);

  /* Compute local live ranges.  */
  hash_map<basic_block, hash_map<tree, pair>> live_ranges_per_bb;
  machine_mode biggest_mode
    = compute_local_live_ranges (program_points_per_bb, live_ranges_per_bb);

  /* If we can use simple VLS modes to handle NITERS element.
     We don't need to use VLA modes with partial vector auto-vectorization.  */
  if (LOOP_VINFO_NITERS_KNOWN_P (this_loop_vinfo)
      && known_le (tree_to_poly_int64 (LOOP_VINFO_NITERS (this_loop_vinfo))
		     * GET_MODE_SIZE (biggest_mode).to_constant (),
		   (int) RVV_M8 * BYTES_PER_RISCV_VECTOR)
      && pow2p_hwi (LOOP_VINFO_INT_NITERS (this_loop_vinfo)))
    return vector_costs::better_main_loop_than_p (other);

  /* Update live ranges according to PHI.  */
  update_local_live_ranges (other->m_vinfo, program_points_per_bb,
			    live_ranges_per_bb, &biggest_mode);

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
	    = (*program_points_per_bb.get (bb)).length () - 1;
	  if ((*iter).second.is_empty ())
	    continue;
	  /* We prefer larger LMUL unless it causes register spillings.  */
	  unsigned int nregs
	    = max_number_of_live_regs (bb, (*iter).second, max_point,
				       biggest_mode, lmul);
	  if (nregs > max_nregs)
	    max_nregs = nregs;
	  live_ranges_per_bb.empty ();
	}
      live_ranges_per_bb.empty ();
      if (loop_autovec_infos.get (loop)->current_lmul == RVV_M1
	  || max_nregs <= V_REG_NUM)
	loop_autovec_infos.get (loop)->end_p = true;
      if (loop_autovec_infos.get (loop)->current_lmul > RVV_M1)
	return max_nregs > V_REG_NUM;
      return false;
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

bool
costs::better_main_loop_than_p (const vector_costs *uncast_other) const
{
  auto other = static_cast<const costs *> (uncast_other);

  if (!flag_vect_cost_model)
    return vector_costs::better_main_loop_than_p (other);

  if (riscv_autovec_lmul == RVV_DYNAMIC)
    {
      bool post_dom_available_p = dom_info_available_p (CDI_POST_DOMINATORS);
      if (!post_dom_available_p)
	calculate_dominance_info (CDI_POST_DOMINATORS);
      bool preferred_p = preferred_new_lmul_p (uncast_other);
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
  /* TODO: Use default STMT cost model.
	   We will support more accurate STMT cost model later.  */
  int stmt_cost = default_builtin_vectorization_cost (kind, vectype, misalign);
  return record_stmt_cost (stmt_info, where, count * stmt_cost);
}

void
costs::finish_cost (const vector_costs *scalar_costs)
{
  vector_costs::finish_cost (scalar_costs);
}

} // namespace riscv_vector
