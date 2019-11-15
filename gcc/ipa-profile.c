/* Basic IPA optimizations based on profile.
   Copyright (C) 2003-2019 Free Software Foundation, Inc.

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

/* ipa-profile pass implements the following analysis propagating profille
   inter-procedurally.

   - Count histogram construction.  This is a histogram analyzing how much
     time is spent executing statements with a given execution count read
     from profile feedback. This histogram is complete only with LTO,
     otherwise it contains information only about the current unit.

     The information is used to set hot/cold thresholds.
   - Next speculative indirect call resolution is performed:  the local
     profile pass assigns profile-id to each function and provide us with a
     histogram specifying the most common target.  We look up the callgraph
     node corresponding to the target and produce a speculative call.

     This call may or may not survive through IPA optimization based on decision
     of inliner. 
   - Finally we propagate the following flags: unlikely executed, executed
     once, executed at startup and executed at exit.  These flags are used to
     control code size/performance threshold and code placement (by producing
     .text.unlikely/.text.hot/.text.startup/.text.exit subsections).  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "data-streamer.h"
#include "gimple-iterator.h"
#include "ipa-utils.h"
#include "profile.h"
#include "value-prof.h"
#include "tree-inline.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"

/* Entry in the histogram.  */

struct histogram_entry
{
  gcov_type count;
  int time;
  int size;
};

/* Histogram of profile values.
   The histogram is represented as an ordered vector of entries allocated via
   histogram_pool. During construction a separate hashtable is kept to lookup
   duplicate entries.  */

vec<histogram_entry *> histogram;
static object_allocator<histogram_entry> histogram_pool ("IPA histogram");

/* Hashtable support for storing SSA names hashed by their SSA_NAME_VAR.  */

struct histogram_hash : nofree_ptr_hash <histogram_entry>
{
  static inline hashval_t hash (const histogram_entry *);
  static inline int equal (const histogram_entry *, const histogram_entry *);
};

inline hashval_t
histogram_hash::hash (const histogram_entry *val)
{
  return val->count;
}

inline int
histogram_hash::equal (const histogram_entry *val, const histogram_entry *val2)
{
  return val->count == val2->count;
}

/* Account TIME and SIZE executed COUNT times into HISTOGRAM.
   HASHTABLE is the on-side hash kept to avoid duplicates.  */

static void
account_time_size (hash_table<histogram_hash> *hashtable,
		   vec<histogram_entry *> &histogram,
		   gcov_type count, int time, int size)
{
  histogram_entry key = {count, 0, 0};
  histogram_entry **val = hashtable->find_slot (&key, INSERT);

  if (!*val)
    {
      *val = histogram_pool.allocate ();
      **val = key;
      histogram.safe_push (*val);
    }
  (*val)->time += time;
  (*val)->size += size;
}

int
cmp_counts (const void *v1, const void *v2)
{
  const histogram_entry *h1 = *(const histogram_entry * const *)v1;
  const histogram_entry *h2 = *(const histogram_entry * const *)v2;
  if (h1->count < h2->count)
    return 1;
  if (h1->count > h2->count)
    return -1;
  return 0;
}

/* Dump HISTOGRAM to FILE.  */

static void
dump_histogram (FILE *file, vec<histogram_entry *> histogram)
{
  unsigned int i;
  gcov_type overall_time = 0, cumulated_time = 0, cumulated_size = 0, overall_size = 0;
  
  fprintf (dump_file, "Histogram:\n");
  for (i = 0; i < histogram.length (); i++)
    {
      overall_time += histogram[i]->count * histogram[i]->time;
      overall_size += histogram[i]->size;
    }
  if (!overall_time)
    overall_time = 1;
  if (!overall_size)
    overall_size = 1;
  for (i = 0; i < histogram.length (); i++)
    {
      cumulated_time += histogram[i]->count * histogram[i]->time;
      cumulated_size += histogram[i]->size;
      fprintf (file, "  %" PRId64": time:%i (%2.2f) size:%i (%2.2f)\n",
	       (int64_t) histogram[i]->count,
	       histogram[i]->time,
	       cumulated_time * 100.0 / overall_time,
	       histogram[i]->size,
	       cumulated_size * 100.0 / overall_size);
   }
}

/* Collect histogram from CFG profiles.  */

static void
ipa_profile_generate_summary (void)
{
  struct cgraph_node *node;
  gimple_stmt_iterator gsi;
  basic_block bb;

  hash_table<histogram_hash> hashtable (10);
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    if (ENTRY_BLOCK_PTR_FOR_FN (DECL_STRUCT_FUNCTION (node->decl))->count.ipa_p ())
      FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
	{
	  int time = 0;
	  int size = 0;
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gimple_code (stmt) == GIMPLE_CALL
		  && !gimple_call_fndecl (stmt))
		{
		  histogram_value h;
		  h = gimple_histogram_value_of_type
			(DECL_STRUCT_FUNCTION (node->decl),
			 stmt, HIST_TYPE_INDIR_CALL);
		  /* No need to do sanity check: gimple_ic_transform already
		     takes away bad histograms.  */
		  if (h)
		    {
		      gcov_type val, count, all;
		      if (get_nth_most_common_value (NULL, "indirect call", h,
						     &val, &count, &all))
			{
			  struct cgraph_edge * e = node->get_edge (stmt);
			  if (e && !e->indirect_unknown_callee)
			    continue;

			  e->indirect_info->common_target_id = val;
			  e->indirect_info->common_target_probability
			    = GCOV_COMPUTE_SCALE (count, all);
			  if (e->indirect_info->common_target_probability > REG_BR_PROB_BASE)
			    {
			      if (dump_file)
				fprintf (dump_file, "Probability capped to 1\n");
			      e->indirect_info->common_target_probability = REG_BR_PROB_BASE;
			    }
			}
		      gimple_remove_histogram_value (DECL_STRUCT_FUNCTION (node->decl),
						      stmt, h);
		    }
		}
	      time += estimate_num_insns (stmt, &eni_time_weights);
	      size += estimate_num_insns (stmt, &eni_size_weights);
	    }
	  if (bb->count.ipa_p () && bb->count.initialized_p ())
	    account_time_size (&hashtable, histogram, bb->count.ipa ().to_gcov_type (),
			       time, size);
	}
  histogram.qsort (cmp_counts);
}

/* Serialize the ipa info for lto.  */

static void
ipa_profile_write_summary (void)
{
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_profile);
  unsigned int i;

  streamer_write_uhwi_stream (ob->main_stream, histogram.length ());
  for (i = 0; i < histogram.length (); i++)
    {
      streamer_write_gcov_count_stream (ob->main_stream, histogram[i]->count);
      streamer_write_uhwi_stream (ob->main_stream, histogram[i]->time);
      streamer_write_uhwi_stream (ob->main_stream, histogram[i]->size);
    }
  lto_destroy_simple_output_block (ob);
}

/* Deserialize the ipa info for lto.  */

static void
ipa_profile_read_summary (void)
{
  struct lto_file_decl_data ** file_data_vec
    = lto_get_file_decl_data ();
  struct lto_file_decl_data * file_data;
  int j = 0;

  hash_table<histogram_hash> hashtable (10);

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      class lto_input_block *ib
	= lto_create_simple_input_block (file_data,
					 LTO_section_ipa_profile,
					 &data, &len);
      if (ib)
	{
          unsigned int num = streamer_read_uhwi (ib);
	  unsigned int n;
	  for (n = 0; n < num; n++)
	    {
	      gcov_type count = streamer_read_gcov_count (ib);
	      int time = streamer_read_uhwi (ib);
	      int size = streamer_read_uhwi (ib);
	      account_time_size (&hashtable, histogram,
				 count, time, size);
	    }
	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_profile,
					  ib, data, len);
	}
    }
  histogram.qsort (cmp_counts);
}

/* Data used by ipa_propagate_frequency.  */

struct ipa_propagate_frequency_data
{
  cgraph_node *function_symbol;
  bool maybe_unlikely_executed;
  bool maybe_executed_once;
  bool only_called_at_startup;
  bool only_called_at_exit;
};

/* Worker for ipa_propagate_frequency_1.  */

static bool
ipa_propagate_frequency_1 (struct cgraph_node *node, void *data)
{
  struct ipa_propagate_frequency_data *d;
  struct cgraph_edge *edge;

  d = (struct ipa_propagate_frequency_data *)data;
  for (edge = node->callers;
       edge && (d->maybe_unlikely_executed || d->maybe_executed_once
	        || d->only_called_at_startup || d->only_called_at_exit);
       edge = edge->next_caller)
    {
      if (edge->caller != d->function_symbol)
	{
          d->only_called_at_startup &= edge->caller->only_called_at_startup;
	  /* It makes sense to put main() together with the static constructors.
	     It will be executed for sure, but rest of functions called from
	     main are definitely not at startup only.  */
	  if (MAIN_NAME_P (DECL_NAME (edge->caller->decl)))
	    d->only_called_at_startup = 0;
          d->only_called_at_exit &= edge->caller->only_called_at_exit;
	}

      /* When profile feedback is available, do not try to propagate too hard;
	 counts are already good guide on function frequencies and roundoff
	 errors can make us to push function into unlikely section even when
	 it is executed by the train run.  Transfer the function only if all
	 callers are unlikely executed.  */
      if (profile_info
	  && !(edge->callee->count.ipa () == profile_count::zero ())
	  && (edge->caller->frequency != NODE_FREQUENCY_UNLIKELY_EXECUTED
	      || (edge->caller->inlined_to
		  && edge->caller->inlined_to->frequency
		     != NODE_FREQUENCY_UNLIKELY_EXECUTED)))
	  d->maybe_unlikely_executed = false;
      if (edge->count.ipa ().initialized_p ()
	  && !edge->count.ipa ().nonzero_p ())
	continue;
      switch (edge->caller->frequency)
        {
	case NODE_FREQUENCY_UNLIKELY_EXECUTED:
	  break;
	case NODE_FREQUENCY_EXECUTED_ONCE:
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "  Called by %s that is executed once\n",
		       edge->caller->name ());
	    d->maybe_unlikely_executed = false;
	    ipa_call_summary *s = ipa_call_summaries->get (edge);
	    if (s != NULL && s->loop_depth)
	      {
		d->maybe_executed_once = false;
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "  Called in loop\n");
	      }
	    break;
	  }
	case NODE_FREQUENCY_HOT:
	case NODE_FREQUENCY_NORMAL:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Called by %s that is normal or hot\n",
		     edge->caller->name ());
	  d->maybe_unlikely_executed = false;
	  d->maybe_executed_once = false;
	  break;
	}
    }
  return edge != NULL;
}

/* Return ture if NODE contains hot calls.  */

bool
contains_hot_call_p (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->maybe_hot_p ())
      return true;
    else if (!e->inline_failed
	     && contains_hot_call_p (e->callee))
      return true;
  for (e = node->indirect_calls; e; e = e->next_callee)
    if (e->maybe_hot_p ())
      return true;
  return false;
}

/* See if the frequency of NODE can be updated based on frequencies of its
   callers.  */
bool
ipa_propagate_frequency (struct cgraph_node *node)
{
  struct ipa_propagate_frequency_data d = {node, true, true, true, true};
  bool changed = false;

  /* We cannot propagate anything useful about externally visible functions
     nor about virtuals.  */
  if (!node->local
      || node->alias
      || (opt_for_fn (node->decl, flag_devirtualize)
	  && DECL_VIRTUAL_P (node->decl)))
    return false;
  gcc_assert (node->analyzed);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing frequency %s\n", node->name ());

  node->call_for_symbol_and_aliases (ipa_propagate_frequency_1, &d,
				     true);

  if ((d.only_called_at_startup && !d.only_called_at_exit)
      && !node->only_called_at_startup)
    {
       node->only_called_at_startup = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at startup.\n",
		  node->name ());
       changed = true;
    }
  if ((d.only_called_at_exit && !d.only_called_at_startup)
      && !node->only_called_at_exit)
    {
       node->only_called_at_exit = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at exit.\n",
		  node->name ());
       changed = true;
    }

  /* With profile we can decide on hot/normal based on count.  */
  if (node->count. ipa().initialized_p ())
    {
      bool hot = false;
      if (!(node->count. ipa() == profile_count::zero ())
	  && node->count. ipa() >= get_hot_bb_threshold ())
	hot = true;
      if (!hot)
	hot |= contains_hot_call_p (node);
      if (hot)
	{
	  if (node->frequency != NODE_FREQUENCY_HOT)
	    {
	      if (dump_file)
		fprintf (dump_file, "Node %s promoted to hot.\n",
			 node->name ());
	      node->frequency = NODE_FREQUENCY_HOT;
	      return true;
	    }
	  return false;
	}
      else if (node->frequency == NODE_FREQUENCY_HOT)
	{
	  if (dump_file)
	    fprintf (dump_file, "Node %s reduced to normal.\n",
		     node->name ());
	  node->frequency = NODE_FREQUENCY_NORMAL;
	  changed = true;
	}
    }
  /* These come either from profile or user hints; never update them.  */
  if (node->frequency == NODE_FREQUENCY_HOT
      || node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return changed;
  if (d.maybe_unlikely_executed)
    {
      node->frequency = NODE_FREQUENCY_UNLIKELY_EXECUTED;
      if (dump_file)
	fprintf (dump_file, "Node %s promoted to unlikely executed.\n",
		 node->name ());
      changed = true;
    }
  else if (d.maybe_executed_once && node->frequency != NODE_FREQUENCY_EXECUTED_ONCE)
    {
      node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      if (dump_file)
	fprintf (dump_file, "Node %s promoted to executed once.\n",
		 node->name ());
      changed = true;
    }
  return changed;
}

/* Check that number of arguments of N agrees with E.
   Be conservative when summaries are not present.  */

static bool
check_argument_count (struct cgraph_node *n, struct cgraph_edge *e)
{
  if (!ipa_node_params_sum || !ipa_edge_args_sum)
    return true;
  class ipa_node_params *info = IPA_NODE_REF (n->function_symbol ());
  if (!info)
    return true;
  ipa_edge_args *e_info = IPA_EDGE_REF (e);
  if (!e_info)
    return true;
  if (ipa_get_param_count (info) != ipa_get_cs_argument_count (e_info)
      && (ipa_get_param_count (info) >= ipa_get_cs_argument_count (e_info)
	  || !stdarg_p (TREE_TYPE (n->decl))))
    return false;
  return true;
}

/* Simple ipa profile pass propagating frequencies across the callgraph.  */

static unsigned int
ipa_profile (void)
{
  struct cgraph_node **order;
  struct cgraph_edge *e;
  int order_pos;
  bool something_changed = false;
  int i;
  gcov_type overall_time = 0, cutoff = 0, cumulated = 0, overall_size = 0;
  struct cgraph_node *n,*n2;
  int nindirect = 0, ncommon = 0, nunknown = 0, nuseless = 0, nconverted = 0;
  int nmismatch = 0, nimpossible = 0;
  bool node_map_initialized = false;

  if (dump_file)
    dump_histogram (dump_file, histogram);
  for (i = 0; i < (int)histogram.length (); i++)
    {
      overall_time += histogram[i]->count * histogram[i]->time;
      overall_size += histogram[i]->size;
    }
  if (overall_time)
    {
      gcov_type threshold;

      gcc_assert (overall_size);

      cutoff = (overall_time * param_hot_bb_count_ws_permille + 500) / 1000;
      threshold = 0;
      for (i = 0; cumulated < cutoff; i++)
	{
	  cumulated += histogram[i]->count * histogram[i]->time;
          threshold = histogram[i]->count;
	}
      if (!threshold)
	threshold = 1;
      if (dump_file)
	{
	  gcov_type cumulated_time = 0, cumulated_size = 0;

          for (i = 0;
	       i < (int)histogram.length () && histogram[i]->count >= threshold;
	       i++)
	    {
	      cumulated_time += histogram[i]->count * histogram[i]->time;
	      cumulated_size += histogram[i]->size;
	    }
	  fprintf (dump_file, "Determined min count: %" PRId64
		   " Time:%3.2f%% Size:%3.2f%%\n", 
		   (int64_t)threshold,
		   cumulated_time * 100.0 / overall_time,
		   cumulated_size * 100.0 / overall_size);
	}

      if (in_lto_p)
	{
	  if (dump_file)
	    fprintf (dump_file, "Setting hotness threshold in LTO mode.\n");
          set_hot_bb_threshold (threshold);
	}
    }
  histogram.release ();
  histogram_pool.release ();

  /* Produce speculative calls: we saved common target from porfiling into
     e->common_target_id.  Now, at link time, we can look up corresponding
     function node and produce speculative call.  */

  FOR_EACH_DEFINED_FUNCTION (n)
    {
      bool update = false;

      if (!opt_for_fn (n->decl, flag_ipa_profile))
	continue;

      for (e = n->indirect_calls; e; e = e->next_callee)
	{
	  if (n->count.initialized_p ())
	    nindirect++;
	  if (e->indirect_info->common_target_id)
	    {
	      if (!node_map_initialized)
	        init_node_map (false);
	      node_map_initialized = true;
	      ncommon++;
	      n2 = find_func_by_profile_id (e->indirect_info->common_target_id);
	      if (n2)
		{
		  if (dump_file)
		    {
		      fprintf (dump_file, "Indirect call -> direct call from"
			       " other module %s => %s, prob %3.2f\n",
			       n->dump_name (),
			       n2->dump_name (),
			       e->indirect_info->common_target_probability
			       / (float)REG_BR_PROB_BASE);
		    }
		  if (e->indirect_info->common_target_probability
		      < REG_BR_PROB_BASE / 2)
		    {
		      nuseless++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: probability is too low.\n");
		    }
		  else if (!e->maybe_hot_p ())
		    {
		      nuseless++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: call is cold.\n");
		    }
		  else if (n2->get_availability () <= AVAIL_INTERPOSABLE
			   && n2->can_be_discarded_p ())
		    {
		      nuseless++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: target is overwritable "
				 "and can be discarded.\n");
		    }
		  else if (!check_argument_count (n2, e))
		    {
		      nmismatch++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: "
				 "parameter count mismatch\n");
		    }
		  else if (e->indirect_info->polymorphic
			   && !opt_for_fn (n->decl, flag_devirtualize)
			   && !possible_polymorphic_call_target_p (e, n2))
		    {
		      nimpossible++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: "
				 "function is not in the polymorphic "
				 "call target list\n");
		    }
		  else
		    {
		      /* Target may be overwritable, but profile says that
			 control flow goes to this particular implementation
			 of N2.  Speculate on the local alias to allow inlining.
		       */
		      if (!n2->can_be_discarded_p ())
			{
			  cgraph_node *alias;
			  alias = dyn_cast<cgraph_node *> (n2->noninterposable_alias ());
			  if (alias)
			    n2 = alias;
			}
		      nconverted++;
		      e->make_speculative
			(n2,
			 e->count.apply_probability
				     (e->indirect_info->common_target_probability));
		      update = true;
		    }
		}
	      else
		{
		  if (dump_file)
		    fprintf (dump_file, "Function with profile-id %i not found.\n",
			     e->indirect_info->common_target_id);
		  nunknown++;
		}
	    }
	 }
       if (update)
	 ipa_update_overall_fn_summary (n);
     }
  if (node_map_initialized)
    del_node_map ();
  if (dump_file && nindirect)
    fprintf (dump_file,
	     "%i indirect calls trained.\n"
	     "%i (%3.2f%%) have common target.\n"
	     "%i (%3.2f%%) targets was not found.\n"
	     "%i (%3.2f%%) targets had parameter count mismatch.\n"
	     "%i (%3.2f%%) targets was not in polymorphic call target list.\n"
	     "%i (%3.2f%%) speculations seems useless.\n"
	     "%i (%3.2f%%) speculations produced.\n",
	     nindirect,
	     ncommon, ncommon * 100.0 / nindirect,
	     nunknown, nunknown * 100.0 / nindirect,
	     nmismatch, nmismatch * 100.0 / nindirect,
	     nimpossible, nimpossible * 100.0 / nindirect,
	     nuseless, nuseless * 100.0 / nindirect,
	     nconverted, nconverted * 100.0 / nindirect);

  order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  order_pos = ipa_reverse_postorder (order);
  for (i = order_pos - 1; i >= 0; i--)
    {
      if (order[i]->local
	  && opt_for_fn (order[i]->decl, flag_ipa_profile)
	  && ipa_propagate_frequency (order[i]))
	{
	  for (e = order[i]->callees; e; e = e->next_callee)
	    if (e->callee->local && !e->callee->aux)
	      {
	        something_changed = true;
	        e->callee->aux = (void *)1;
	      }
	}
      order[i]->aux = NULL;
    }

  while (something_changed)
    {
      something_changed = false;
      for (i = order_pos - 1; i >= 0; i--)
	{
	  if (order[i]->aux
	      && opt_for_fn (order[i]->decl, flag_ipa_profile)
	      && ipa_propagate_frequency (order[i]))
	    {
	      for (e = order[i]->callees; e; e = e->next_callee)
		if (e->callee->local && !e->callee->aux)
		  {
		    something_changed = true;
		    e->callee->aux = (void *)1;
		  }
	    }
	  order[i]->aux = NULL;
	}
    }
  free (order);
  return 0;
}

namespace {

const pass_data pass_data_ipa_profile =
{
  IPA_PASS, /* type */
  "profile_estimate", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PROFILE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_profile : public ipa_opt_pass_d
{
public:
  pass_ipa_profile (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_profile, ctxt,
		      ipa_profile_generate_summary, /* generate_summary */
		      ipa_profile_write_summary, /* write_summary */
		      ipa_profile_read_summary, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_ipa_profile || in_lto_p; }
  virtual unsigned int execute (function *) { return ipa_profile (); }

}; // class pass_ipa_profile

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_profile (gcc::context *ctxt)
{
  return new pass_ipa_profile (ctxt);
}
