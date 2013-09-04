/* Basic IPA optimizations based on profile.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.

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
#include "tm.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "gimple.h"
#include "ggc.h"
#include "flags.h"
#include "target.h"
#include "tree-iterator.h"
#include "ipa-utils.h"
#include "hash-table.h"
#include "profile.h"
#include "params.h"
#include "value-prof.h"
#include "alloc-pool.h"
#include "tree-inline.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "ipa-inline.h"

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
static alloc_pool histogram_pool;

/* Hashtable support for storing SSA names hashed by their SSA_NAME_VAR.  */

struct histogram_hash : typed_noop_remove <histogram_entry>
{
  typedef histogram_entry value_type;
  typedef histogram_entry compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
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
account_time_size (hash_table <histogram_hash> hashtable,
		   vec<histogram_entry *> &histogram,
		   gcov_type count, int time, int size)
{
  histogram_entry key = {count, 0, 0};
  histogram_entry **val = hashtable.find_slot (&key, INSERT);

  if (!*val)
    {
      *val = (histogram_entry *) pool_alloc (histogram_pool);
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
      fprintf (file, "  "HOST_WIDEST_INT_PRINT_DEC": time:%i (%2.2f) size:%i (%2.2f)\n",
	       (HOST_WIDEST_INT) histogram[i]->count,
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
  hash_table <histogram_hash> hashtable;
  basic_block bb;

  hashtable.create (10);
  histogram_pool = create_alloc_pool ("IPA histogram", sizeof (struct histogram_entry),
				      10);
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->symbol.decl))
      {
	int time = 0;
	int size = 0;
        for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    gimple stmt = gsi_stmt (gsi);
	    if (gimple_code (stmt) == GIMPLE_CALL
		&& !gimple_call_fndecl (stmt))
	      {
		histogram_value h;
		h = gimple_histogram_value_of_type
		      (DECL_STRUCT_FUNCTION (node->symbol.decl),
		       stmt, HIST_TYPE_INDIR_CALL);
		/* No need to do sanity check: gimple_ic_transform already
		   takes away bad histograms.  */
		if (h)
		  {
		    /* counter 0 is target, counter 1 is number of execution we called target,
		       counter 2 is total number of executions.  */
		    if (h->hvalue.counters[2])
		      {
			struct cgraph_edge * e = cgraph_edge (node, stmt);
			e->indirect_info->common_target_id
			  = h->hvalue.counters [0];
			e->indirect_info->common_target_probability
			  = GCOV_COMPUTE_SCALE (h->hvalue.counters [1], h->hvalue.counters [2]);
			if (e->indirect_info->common_target_probability > REG_BR_PROB_BASE)
			  {
			    if (dump_file)
			      fprintf (dump_file, "Probability capped to 1\n");
			    e->indirect_info->common_target_probability = REG_BR_PROB_BASE;
			  }
		      }
		    gimple_remove_histogram_value (DECL_STRUCT_FUNCTION (node->symbol.decl),
						    stmt, h);
		  }
	      }
	    time += estimate_num_insns (stmt, &eni_time_weights);
	    size += estimate_num_insns (stmt, &eni_size_weights);
	  }
	account_time_size (hashtable, histogram, bb->count, time, size);
      }
  hashtable.dispose ();
  histogram.qsort (cmp_counts);
}

/* Serialize the ipa info for lto.  */

static void
ipa_profile_write_summary (void)
{
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_profile);
  unsigned int i;

  streamer_write_uhwi_stream (ob->main_stream, histogram.length());
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
  hash_table <histogram_hash> hashtable;
  int j = 0;

  hashtable.create (10);
  histogram_pool = create_alloc_pool ("IPA histogram", sizeof (struct histogram_entry),
				      10);

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      struct lto_input_block *ib
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
	      account_time_size (hashtable, histogram,
				 count, time, size);
	    }
	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_profile,
					  ib, data, len);
	}
    }
  hashtable.dispose ();
  histogram.qsort (cmp_counts);
}

/* Data used by ipa_propagate_frequency.  */

struct ipa_propagate_frequency_data
{
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
      if (edge->caller != node)
	{
          d->only_called_at_startup &= edge->caller->only_called_at_startup;
	  /* It makes sense to put main() together with the static constructors.
	     It will be executed for sure, but rest of functions called from
	     main are definitely not at startup only.  */
	  if (MAIN_NAME_P (DECL_NAME (edge->caller->symbol.decl)))
	    d->only_called_at_startup = 0;
          d->only_called_at_exit &= edge->caller->only_called_at_exit;
	}
      if (!edge->frequency)
	continue;
      switch (edge->caller->frequency)
        {
	case NODE_FREQUENCY_UNLIKELY_EXECUTED:
	  break;
	case NODE_FREQUENCY_EXECUTED_ONCE:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Called by %s that is executed once\n",
		     cgraph_node_name (edge->caller));
	  d->maybe_unlikely_executed = false;
	  if (inline_edge_summary (edge)->loop_depth)
	    {
	      d->maybe_executed_once = false;
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file, "  Called in loop\n");
	    }
	  break;
	case NODE_FREQUENCY_HOT:
	case NODE_FREQUENCY_NORMAL:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Called by %s that is normal or hot\n",
		     cgraph_node_name (edge->caller));
	  d->maybe_unlikely_executed = false;
	  d->maybe_executed_once = false;
	  break;
	}
    }
  return edge != NULL;
}

/* See if the frequency of NODE can be updated based on frequencies of its
   callers.  */
bool
ipa_propagate_frequency (struct cgraph_node *node)
{
  struct ipa_propagate_frequency_data d = {true, true, true, true};
  bool changed = false;

  /* We can not propagate anything useful about externally visible functions
     nor about virtuals.  */
  if (!node->local.local
      || (flag_devirtualize && DECL_VIRTUAL_P (node->symbol.decl)))
    return false;
  gcc_assert (node->symbol.analyzed);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing frequency %s\n", cgraph_node_name (node));

  cgraph_for_node_and_aliases (node, ipa_propagate_frequency_1, &d, true);

  if ((d.only_called_at_startup && !d.only_called_at_exit)
      && !node->only_called_at_startup)
    {
       node->only_called_at_startup = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at startup.\n",
		  cgraph_node_name (node));
       changed = true;
    }
  if ((d.only_called_at_exit && !d.only_called_at_startup)
      && !node->only_called_at_exit)
    {
       node->only_called_at_exit = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at exit.\n",
		  cgraph_node_name (node));
       changed = true;
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
		 cgraph_node_name (node));
      changed = true;
    }
  else if (d.maybe_executed_once && node->frequency != NODE_FREQUENCY_EXECUTED_ONCE)
    {
      node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      if (dump_file)
	fprintf (dump_file, "Node %s promoted to executed once.\n",
		 cgraph_node_name (node));
      changed = true;
    }
  return changed;
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
      if (dump_file)
	{
	  gcov_type min, cumulated_time = 0, cumulated_size = 0;

	  fprintf (dump_file, "Overall time: "HOST_WIDEST_INT_PRINT_DEC"\n", 
		   (HOST_WIDEST_INT)overall_time);
	  min = get_hot_bb_threshold ();
          for (i = 0; i < (int)histogram.length () && histogram[i]->count >= min;
	       i++)
	    {
	      cumulated_time += histogram[i]->count * histogram[i]->time;
	      cumulated_size += histogram[i]->size;
	    }
	  fprintf (dump_file, "GCOV min count: "HOST_WIDEST_INT_PRINT_DEC
		   " Time:%3.2f%% Size:%3.2f%%\n", 
		   (HOST_WIDEST_INT)min,
		   cumulated_time * 100.0 / overall_time,
		   cumulated_size * 100.0 / overall_size);
	}
      cutoff = (overall_time * PARAM_VALUE (HOT_BB_COUNT_WS_PERMILLE) + 500) / 1000;
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
	  fprintf (dump_file, "Determined min count: "HOST_WIDEST_INT_PRINT_DEC
		   " Time:%3.2f%% Size:%3.2f%%\n", 
		   (HOST_WIDEST_INT)threshold,
		   cumulated_time * 100.0 / overall_time,
		   cumulated_size * 100.0 / overall_size);
	}
      if (threshold > get_hot_bb_threshold ()
	  || in_lto_p)
	{
	  if (dump_file)
	    fprintf (dump_file, "Threshold updated.\n");
          set_hot_bb_threshold (threshold);
	}
    }
  histogram.release();
  free_alloc_pool (histogram_pool);

  /* Produce speculative calls: we saved common traget from porfiling into
     e->common_target_id.  Now, at link time, we can look up corresponding
     function node and produce speculative call.  */

  FOR_EACH_DEFINED_FUNCTION (n)
    {
      bool update = false;

      for (e = n->indirect_calls; e; e = e->next_callee)
	{
	  if (n->count)
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
			       " other module %s/%i => %s/%i, prob %3.2f\n",
			       xstrdup (cgraph_node_name (n)), n->symbol.order,
			       xstrdup (cgraph_node_name (n2)), n2->symbol.order,
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
		  else if (!cgraph_maybe_hot_edge_p (e))
		    {
		      nuseless++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: call is cold.\n");
		    }
		  else if (cgraph_function_body_availability (n2)
			   <= AVAIL_OVERWRITABLE
			   && symtab_can_be_discarded ((symtab_node) n2))
		    {
		      nuseless++;
		      if (dump_file)
			fprintf (dump_file,
				 "Not speculating: target is overwritable "
				 "and can be discarded.\n");
		    }
		  else
		    {
		      /* Target may be overwritable, but profile says that
			 control flow goes to this particular implementation
			 of N2.  Speculate on the local alias to allow inlining.
		       */
		      if (!symtab_can_be_discarded ((symtab_node) n2))
			n2 = cgraph (symtab_nonoverwritable_alias ((symtab_node)n2));
		      nconverted++;
		      cgraph_turn_edge_to_speculative
			(e, n2,
			 apply_scale (e->count,
				      e->indirect_info->common_target_probability),
			 apply_scale (e->frequency,
				      e->indirect_info->common_target_probability));
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
	 inline_update_overall_summary (n);
     }
  if (node_map_initialized)
    del_node_map ();
  if (dump_file && nindirect)
    fprintf (dump_file,
	     "%i indirect calls trained.\n"
	     "%i (%3.2f%%) have common target.\n"
	     "%i (%3.2f%%) targets was not found.\n"
	     "%i (%3.2f%%) speculations seems useless.\n"
	     "%i (%3.2f%%) speculations produced.\n",
	     nindirect,
	     ncommon, ncommon * 100.0 / nindirect,
	     nunknown, nunknown * 100.0 / nindirect,
	     nuseless, nuseless * 100.0 / nindirect,
	     nconverted, nconverted * 100.0 / nindirect);

  order = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  order_pos = ipa_reverse_postorder (order);
  for (i = order_pos - 1; i >= 0; i--)
    {
      if (order[i]->local.local && ipa_propagate_frequency (order[i]))
	{
	  for (e = order[i]->callees; e; e = e->next_callee)
	    if (e->callee->local.local && !e->callee->symbol.aux)
	      {
	        something_changed = true;
	        e->callee->symbol.aux = (void *)1;
	      }
	}
      order[i]->symbol.aux = NULL;
    }

  while (something_changed)
    {
      something_changed = false;
      for (i = order_pos - 1; i >= 0; i--)
	{
	  if (order[i]->symbol.aux && ipa_propagate_frequency (order[i]))
	    {
	      for (e = order[i]->callees; e; e = e->next_callee)
		if (e->callee->local.local && !e->callee->symbol.aux)
		  {
		    something_changed = true;
		    e->callee->symbol.aux = (void *)1;
		  }
	    }
	  order[i]->symbol.aux = NULL;
	}
    }
  free (order);
  return 0;
}

static bool
gate_ipa_profile (void)
{
  return flag_ipa_profile;
}

namespace {

const pass_data pass_data_ipa_profile =
{
  IPA_PASS, /* type */
  "profile_estimate", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
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
  pass_ipa_profile(gcc::context *ctxt)
    : ipa_opt_pass_d(pass_data_ipa_profile, ctxt,
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
  bool gate () { return gate_ipa_profile (); }
  unsigned int execute () { return ipa_profile (); }

}; // class pass_ipa_profile

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_profile (gcc::context *ctxt)
{
  return new pass_ipa_profile (ctxt);
}
