/* Handling inline asm in the analyzer.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "pretty-print.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "options.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/region-model-reachability.h"
#include "stmt.h"

#if ENABLE_ANALYZER

namespace ana {

/* Minimal asm support for the analyzer.

   The objective of this code is to:
   - minimize false positives from the analyzer on the Linux kernel
   (which makes heavy use of inline asm), whilst
   - avoiding having to "teach" the compiler anything about specific strings
   in asm statements.

   Specifically, we want to:

   (a) mark asm outputs and certain other regions as having been written to,
       to avoid false postives from -Wanalyzer-use-of-uninitialized-value.

   (b) identify some of these stmts as "deterministic" so that we can
       write consistent outputs given consistent inputs, so that we can
       avoid false positives for paths in which an asm is invoked twice
       with the same inputs and is expected to emit the same output.

   This file implements heuristics for achieving the above.  */

/* Determine if ASM_STMT is deterministic, in the sense of (b) above.

   Consider this x86 function taken from the Linux kernel
   (arch/x86/include/asm/barrier.h):

     static inline unsigned long array_index_mask_nospec(unsigned long index,
							 unsigned long size)
     {
       unsigned long mask;

       asm volatile ("cmp %1,%2; sbb %0,%0;"
		     :"=r" (mask)
		     :"g"(size),"r" (index)
		     :"cc");
       return mask;
     }

   The above is a mitigation for Spectre-variant-1 attacks, for clamping
   an array access to within the range of [0, size] if the CPU speculates
   past the array bounds.

   However, it is ultimately used to implement wdev_to_wvif:

     static inline struct wfx_vif *
     wdev_to_wvif(struct wfx_dev *wdev, int vif_id)
     {
       vif_id = array_index_nospec(vif_id, ARRAY_SIZE(wdev->vif));
       if (!wdev->vif[vif_id]) {
	 return NULL;
       }
       return (struct wfx_vif *)wdev->vif[vif_id]->drv_priv;
     }

   which is used by:

     if (wdev_to_wvif(wvif->wdev, 1))
       return wdev_to_wvif(wvif->wdev, 1)->vif;

   The code has been written to assume that wdev_to_wvif is deterministic,
   and won't change from returning non-NULL at the "if" clause to
   returning NULL at the "->vif" dereference.

   By treating the above specific "asm volatile" as deterministic we avoid
   a false positive from -Wanalyzer-null-dereference.  */

static bool
deterministic_p (const gasm *asm_stmt)
{
  /* Assume something volatile with no inputs is querying
     changeable state e.g. rdtsc.  */
  if (gimple_asm_ninputs (asm_stmt) == 0
      && gimple_asm_volatile_p (asm_stmt))
    return false;

  /* Otherwise assume it's purely a function of its inputs.  */
  return true;
}

/* Update this model for the asm STMT, using CTXT to report any
   diagnostics.

   Compare with cfgexpand.cc: expand_asm_stmt.  */

void
region_model::on_asm_stmt (const gasm *stmt, region_model_context *ctxt)
{
  logger *logger = ctxt ? ctxt->get_logger () : NULL;
  LOG_SCOPE (logger);

  const unsigned noutputs = gimple_asm_noutputs (stmt);
  const unsigned ninputs = gimple_asm_ninputs (stmt);

  auto_vec<tree> output_tvec;
  auto_vec<tree> input_tvec;
  auto_vec<const char *> constraints;

  /* Copy the gimple vectors into new vectors that we can manipulate.  */
  output_tvec.safe_grow (noutputs, true);
  input_tvec.safe_grow (ninputs, true);
  constraints.safe_grow (noutputs + ninputs, true);

  for (unsigned i = 0; i < noutputs; ++i)
    {
      tree t = gimple_asm_output_op (stmt, i);
      output_tvec[i] = TREE_VALUE (t);
      constraints[i] = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
    }
  for (unsigned i = 0; i < ninputs; i++)
    {
      tree t = gimple_asm_input_op (stmt, i);
      input_tvec[i] = TREE_VALUE (t);
      constraints[i + noutputs]
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
    }

  /* Determine which regions are reachable from the inputs
     to this stmt.  */
  reachable_regions reachable_regs (this);

  int num_errors = 0;

  auto_vec<const region *> output_regions (noutputs);
  for (unsigned i = 0; i < noutputs; ++i)
    {
      tree val = output_tvec[i];
      const char *constraint;
      bool is_inout;
      bool allows_reg;
      bool allows_mem;

      const region *dst_reg = get_lvalue (val, ctxt);
      output_regions.quick_push (dst_reg);
      reachable_regs.add (dst_reg, true);

      /* Try to parse the output constraint.  If that fails, there's
	 no point in going further.  */
      constraint = constraints[i];
      if (!parse_output_constraint (&constraint, i, ninputs, noutputs,
				    &allows_mem, &allows_reg, &is_inout))
	{
	  if (logger)
	    logger->log ("error parsing constraint for output %i: %qs",
			 i, constraint);
	  num_errors++;
	  continue;
	}

      if (logger)
	{
	  logger->log ("output %i: %qs %qE"
		       " is_inout: %i allows_reg: %i allows_mem: %i",
		       i, constraint, val,
		       (int)is_inout, (int)allows_reg, (int)allows_mem);
	  logger->start_log_line ();
	  logger->log_partial ("  region: ");
	  dst_reg->dump_to_pp (logger->get_printer (), true);
	  logger->end_log_line ();
	}

    }

  /* Ideally should combine with inout_svals to determine the
     "effective inputs" and use this for the asm_output_svalue.  */

  auto_vec<const svalue *> input_svals (ninputs);
  for (unsigned i = 0; i < ninputs; i++)
    {
      tree val = input_tvec[i];
      const char *constraint = constraints[i + noutputs];
      bool allows_reg, allows_mem;
      if (! parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
				    constraints.address (),
				    &allows_mem, &allows_reg))
	{
	  if (logger)
	    logger->log ("error parsing constraint for input %i: %qs",
			 i, constraint);
	  num_errors++;
	  continue;
	}

      tree src_expr = input_tvec[i];
      const svalue *src_sval = get_rvalue (src_expr, ctxt);
      check_for_poison (src_sval, src_expr, NULL, ctxt);
      input_svals.quick_push (src_sval);
      reachable_regs.handle_sval (src_sval);

      if (logger)
	{
	  logger->log ("input %i: %qs %qE"
		       " allows_reg: %i allows_mem: %i",
		       i, constraint, val,
		       (int)allows_reg, (int)allows_mem);
	  logger->start_log_line ();
	  logger->log_partial ("  sval: ");
	  src_sval->dump_to_pp (logger->get_printer (), true);
	  logger->end_log_line ();
	}
    }

  if (num_errors > 0)
    gcc_unreachable ();

  if (logger)
    {
      logger->log ("reachability: ");
      reachable_regs.dump_to_pp (logger->get_printer ());
      logger->end_log_line ();
    }

  /* Given the regions that were reachable from the inputs we
     want to clobber them.
     This is similar to region_model::handle_unrecognized_call,
     but the unknown call policies seems too aggressive (e.g. purging state
     from anything that's ever escaped).  Instead, clobber any clusters
     that were reachable in *this* asm stmt, rather than those that
     escaped, and we don't treat the values as having escaped.
     We also assume that asm stmts don't affect sm-state.  */
  for (auto iter = reachable_regs.begin_mutable_base_regs ();
       iter != reachable_regs.end_mutable_base_regs (); ++iter)
    {
      const region *base_reg = *iter;
      if (base_reg->symbolic_for_unknown_ptr_p ()
	  || !base_reg->tracked_p ())
	continue;

      binding_cluster *cluster = m_store.get_or_create_cluster (base_reg);
      cluster->on_asm (stmt, m_mgr->get_store_manager (),
		       conjured_purge (this, ctxt));
    }

  /* Update the outputs.  */
  for (unsigned output_idx = 0; output_idx < noutputs; output_idx++)
    {
      tree dst_expr = output_tvec[output_idx];
      const region *dst_reg = output_regions[output_idx];

      const svalue *sval;
      if (deterministic_p (stmt)
	  && input_svals.length () <= asm_output_svalue::MAX_INPUTS)
	sval = m_mgr->get_or_create_asm_output_svalue (TREE_TYPE (dst_expr),
						       stmt,
						       output_idx,
						       input_svals);
      else
	{
	  sval = m_mgr->get_or_create_conjured_svalue (TREE_TYPE (dst_expr),
						       stmt,
						       dst_reg,
						       conjured_purge (this,
								       ctxt));
	}
      set_value (dst_reg, sval, ctxt);
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
