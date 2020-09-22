/* Search for references that a functions loads or stores.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by David Cepelik and Jan Hubicka

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

/* Mod/ref pass records summary about loads and stores performed by the
   function.  This is later used by alias analysis to disambiguate memory
   accesses across function calls.  The summary has a form of decision tree and
   contains:

    - base alias set
      and for each:
      - ref alias set

   In future more information will be tracked.

   This file contains a tree pass and an IPA pass.  Both performs the same
   analys however tree pass is executed during early and late optimization
   passes to propagate info downwards in the compilation order.  IPA pass
   propagates across the callgraph and is able to handle recursion and works on
   whole program during link-time analysis.

   LTO mode differs from the local mode by not recording alias sets but types
   that are translated to alias sets later.  This is necessary in order stream
   the information because the alias sets are rebuild at stream-in time and may
   not correspond to ones seen during analysis.  For this reason part of analysis
   is duplicated.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "gimple-iterator.h"
#include "tree-dfa.h"
#include "cgraph.h"
#include "ipa-utils.h"
#include "symbol-summary.h"
#include "gimple-pretty-print.h"
#include "gimple-walk.h"
#include "print-tree.h"
#include "tree-streamer.h"
#include "alias.h"
#include "calls.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"

/* Class (from which there is one global instance) that holds modref summaries
   for all analyzed functions.  */
class GTY((user)) modref_summaries
  : public fast_function_summary <modref_summary *, va_gc>
{
public:
  modref_summaries (symbol_table *symtab)
      : fast_function_summary <modref_summary *, va_gc> (symtab) {}
  virtual void insert (cgraph_node *, modref_summary *state);
  virtual void duplicate (cgraph_node *src_node,
			  cgraph_node *dst_node,
			  modref_summary *src_data,
			  modref_summary *dst_data);
  /* This flag controls whether newly inserted functions should be analyzed
     in IPA or normal mode.  Functions inserted between IPA analysis and
     ipa-modref pass execution needs to be analyzed in IPA mode while all
     other insertions leads to normal analysis.  */
  bool ipa;
};

/* Global variable holding all modref summaries.  */
static GTY(()) fast_function_summary <modref_summary *, va_gc> *summaries;

/* Summary for a single function which this pass produces.  */

modref_summary::modref_summary ()
  : loads (NULL), stores (NULL), loads_lto (NULL),
    stores_lto (NULL), finished (0)
{
}

modref_summary::~modref_summary ()
{
  if (loads)
    ggc_delete (loads);
  if (stores)
    ggc_delete (stores);
  if (loads_lto)
    ggc_delete (loads_lto);
  if (stores_lto)
    ggc_delete (stores_lto);
}

/* Dump records TT to OUT.  */

static void
dump_records (modref_records *tt, FILE *out)
{
  fprintf (out, "    Limits: %i bases, %i refs\n",
	   (int)tt->max_bases, (int)tt->max_refs);
  if (tt->every_base)
    {
      fprintf (out, "    Every base\n");
      return;
    }
  size_t i;
  modref_base_node <alias_set_type> *n;
  FOR_EACH_VEC_SAFE_ELT (tt->bases, i, n)
    {
      fprintf (out, "      Base %i: alias set %i\n", (int)i, n->base);
      if (n->every_ref)
	{
	  fprintf (out, "      Every ref\n");
	  continue;
	}
      size_t j;
      modref_ref_node <alias_set_type> *r;
      FOR_EACH_VEC_SAFE_ELT (n->refs, j, r)
	{
	  fprintf (out, "        Ref %i: alias set %i\n", (int)j, r->ref);
	}
    }
}

/* Dump records TT to OUT.  */

static void
dump_lto_records (modref_records_lto *tt, FILE *out)
{
  fprintf (out, "    Limits: %i bases, %i refs\n",
	   (int)tt->max_bases, (int)tt->max_refs);
  if (tt->every_base)
    {
      fprintf (out, "    Every base\n");
      return;
    }
  size_t i;
  modref_base_node <tree> *n;
  FOR_EACH_VEC_SAFE_ELT (tt->bases, i, n)
    {
      fprintf (out, "      Base %i:", (int)i);
      print_generic_expr (dump_file, n->base);
      fprintf (out, " (alias set %i)\n",
	       n->base ? get_alias_set (n->base) : 0);
      if (n->every_ref)
	{
	  fprintf (out, "      Every ref\n");
	  continue;
	}
      size_t j;
      modref_ref_node <tree> *r;
      FOR_EACH_VEC_SAFE_ELT (n->refs, j, r)
	{
	  fprintf (out, "        Ref %i:", (int)j);
	  print_generic_expr (dump_file, r->ref);
	  fprintf (out, " (alias set %i)\n",
		   r->ref ? get_alias_set (r->ref) : 0);
	}
    }
}

/* Dump summary.  */

void
modref_summary::dump (FILE *out)
{
  if (loads)
    {
      fprintf (out, "  loads:\n");
      dump_records (loads, out);
    }
  if (stores)
    {
      fprintf (out, "  stores:\n");
      dump_records (stores, out);
    }
  if (loads_lto)
    {
      fprintf (out, "  LTO loads:\n");
      dump_lto_records (loads_lto, out);
    }
  if (stores_lto)
    {
      fprintf (out, "  LTO stores:\n");
      dump_lto_records (stores_lto, out);
    }
}


/* Get function summary for FUNC if it exists, return NULL otherwise.  */

modref_summary *
get_modref_function_summary (cgraph_node *func)
{
  /* Avoid creation of the summary too early (e.g. when front-end calls us).  */
  if (!summaries)
    return NULL;

  /* A single function body may be represented by multiple symbols with
     different visibility.  For example, if FUNC is an interposable alias,
     we don't want to return anything, even if we have summary for the target
     function.  */
  enum availability avail;
  func = func->function_or_virtual_thunk_symbol
	     (&avail, cgraph_node::get (current_function_decl));
  if (avail <= AVAIL_INTERPOSABLE)
    return NULL;

  /* Attempt to get summary for FUNC.  If analysis of FUNC hasn't finished yet,
     don't return anything.  */
  modref_summary *r = summaries->get (func);
  if (r && r->finished)
    return r;

  return NULL;
}

/* Record access into the modref_records data structure.  */

static void
record_access (modref_records *tt, ao_ref *ref)
{
  alias_set_type base_set = !flag_strict_aliasing ? 0
			    : ao_ref_base_alias_set (ref);
  alias_set_type ref_set = !flag_strict_aliasing ? 0
			    : (ao_ref_alias_set (ref));
  if (dump_file)
    {
       fprintf (dump_file, "   - Recording base_set=%i ref_set=%i\n",
	        base_set, ref_set);
    }
  tt->insert (base_set, ref_set);
}

/* IPA version of record_access_tree.  */

static void
record_access_lto (modref_records_lto *tt, ao_ref *ref)
{
  /* get_alias_set sometimes use different type to compute the alias set
     than TREE_TYPE (base).  Do same adjustments.  */
  tree base_type = NULL_TREE, ref_type = NULL_TREE;
  if (flag_strict_aliasing)
    {
      tree base;

      base = ref->ref;
      while (handled_component_p (base))
	base = TREE_OPERAND (base, 0);

      base_type = reference_alias_ptr_type_1 (&base);

      if (!base_type)
	base_type = TREE_TYPE (base);
      else
	base_type = TYPE_REF_CAN_ALIAS_ALL (base_type)
		    ? NULL_TREE : TREE_TYPE (base_type);

      tree ref_expr = ref->ref;
      ref_type = reference_alias_ptr_type_1 (&ref_expr);

      if (!ref_type)
	ref_type = TREE_TYPE (ref_expr);
      else
	ref_type = TYPE_REF_CAN_ALIAS_ALL (ref_type)
		   ? NULL_TREE : TREE_TYPE (ref_type);

      /* Sanity check that we are in sync with what get_alias_set does.  */
      gcc_checking_assert ((!base_type && !ao_ref_base_alias_set (ref))
			   || get_alias_set (base_type)
			      == ao_ref_base_alias_set (ref));
      gcc_checking_assert ((!ref_type && !ao_ref_alias_set (ref))
			   || get_alias_set (ref_type)
			      == ao_ref_alias_set (ref));

      /* Do not bother to record types that have no meaningful alias set.
	 Also skip variably modified types since these go to local streams.  */
      if (base_type && (!get_alias_set (base_type)
			|| variably_modified_type_p (base_type, NULL_TREE)))
	base_type = NULL_TREE;
      if (ref_type && (!get_alias_set (ref_type)
		       || variably_modified_type_p (ref_type, NULL_TREE)))
	ref_type = NULL_TREE;
    }
  if (dump_file)
    {
      fprintf (dump_file, "   - Recording base type:");
      print_generic_expr (dump_file, base_type);
      fprintf (dump_file, " (alias set %i) ref type:",
	       base_type ? get_alias_set (base_type) : 0);
      print_generic_expr (dump_file, ref_type);
      fprintf (dump_file, " (alias set %i)\n",
	       ref_type ? get_alias_set (ref_type) : 0);
    }

  tt->insert (base_type, ref_type);
}

/* Returns true if and only if we should store the access to EXPR.
   Some accesses, e.g. loads from automatic variables, are not interesting.  */

static bool
record_access_p (tree expr)
{
  /* Non-escaping memory is fine  */
  tree t = get_base_address (expr);
  if (t && (INDIRECT_REF_P (t)
	    || TREE_CODE (t) == MEM_REF
	    || TREE_CODE (t) == TARGET_MEM_REF)
	&& TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
	&& !ptr_deref_may_alias_global_p (TREE_OPERAND (t, 0)))
    {
      if (dump_file)
	fprintf (dump_file, "   - Non-escaping memory, ignoring.\n");
      return false;
    }

  /* Automatic variables are fine.  */
  if (DECL_P (t)
      && auto_var_in_fn_p (t, current_function_decl))
    {
      if (dump_file)
	fprintf (dump_file, "   - Automatic variable, ignoring.\n");
      return false;
    }

  /* Read-only variables are fine.  */
  if (DECL_P (t) && TREE_READONLY (t))
    {
      if (dump_file)
	fprintf (dump_file, "   - Read-only variable, ignoring.\n");
      return false;
    }

  return true;
}

/* Return true if ECF flags says that stores can be ignored.  */

static bool
ignore_stores_p (tree caller, int flags)
{
  if (flags & ECF_PURE)
    return true;
  if ((flags & (ECF_NORETURN | ECF_NOTHROW)) == (ECF_NORETURN | ECF_NOTHROW)
      || (!opt_for_fn (caller, flag_exceptions) && (flags & ECF_NORETURN)))
    return true;
  return false;
}

/* Analyze function call STMT in function F.  */

static bool
analyze_call (modref_summary *cur_summary,
	      gimple *stmt)
{
  /* Check flags on the function call.  In certain cases, analysis can be
     simplified.  */
  int flags = gimple_call_flags (stmt);
  if (flags & (ECF_CONST | ECF_NOVOPS))
    {
      if (dump_file)
	fprintf (dump_file,
		 " - ECF_CONST | ECF_NOVOPS, ignoring all stores and all loads "
		 "except for args.\n");
      return true;
    }

  /* Pure functions do not affect global memory.  Stores by functions which are
     noreturn and do not throw can safely be ignored.  */
  bool ignore_stores = ignore_stores_p (current_function_decl, flags);

  /* Next, we try to get the callee's function declaration.  The goal is to
     merge their summary with ours.  */
  tree callee = gimple_call_fndecl (stmt);

  /* Check if this is an indirect call.  */
  if (!callee)
    {
      /* If the indirect call does not write memory, our store summary is
	 unaffected, but we have to discard our loads summary (we don't know
	 anything about the loads that the called function performs).  */
      if (ignore_stores)
	{
	  if (dump_file)
	    fprintf (dump_file, " - Indirect call which does not write memory, "
		    "discarding loads.\n");
	  if (cur_summary->loads)
	    cur_summary->loads->collapse ();
	  if (cur_summary->loads_lto)
	    cur_summary->loads_lto->collapse ();
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, " - Indirect call.\n");
      return false;
    }

  struct cgraph_node *callee_node = cgraph_node::get_create (callee);

  /* We can not safely optimize based on summary of callee if it does
     not always bind to current def: it is possible that memory load
     was optimized out earlier which may not happen in the interposed
     variant.  */
  if (!callee_node->binds_to_current_def_p ())
    {
      if (dump_file)
	fprintf (dump_file, " - May be interposed: collapsing loads.\n");
      if (cur_summary->loads)
	cur_summary->loads->collapse ();
      if (cur_summary->loads_lto)
	cur_summary->loads_lto->collapse ();
    }

  /* If this is a recursive call, the target summary is the same as ours, so
     there's nothing to do.  */
  if (recursive_call_p (current_function_decl, callee))
    {
      if (dump_file)
	fprintf (dump_file, " - Skipping recursive call.\n");
      return true;
    }

  gcc_assert (callee_node != NULL);

  /* Get the function symbol and its availability.  */
  enum availability avail;
  callee_node = callee_node->function_symbol (&avail);
  if (avail <= AVAIL_INTERPOSABLE)
    {
      /* Keep stores summary, but discard all loads for interposable function
	 symbols.  */
      if (ignore_stores)
	{
	  if (cur_summary->loads)
	    cur_summary->loads->collapse ();
	  if (cur_summary->loads_lto)
	    cur_summary->loads_lto->collapse ();
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, " - Function availability <= AVAIL_INTERPOSABLE.\n");
      return false;
    }

  /* Get callee's modref summary.  As above, if there's no summary, we either
     have to give up or, if stores are ignored, we can just purge loads.  */
  modref_summary *callee_summary = summaries->get (callee_node);
  if (!callee_summary)
    {
      if (ignore_stores)
	{
	  if (cur_summary->loads)
	    cur_summary->loads->collapse ();
	  if (cur_summary->loads_lto)
	    cur_summary->loads_lto->collapse ();
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, " - No modref summary available for callee.\n");
      return false;
    }

  /* Merge with callee's summary.  */
  if (cur_summary->loads)
    cur_summary->loads->merge (callee_summary->loads);
  if (cur_summary->loads_lto)
    cur_summary->loads_lto->merge (callee_summary->loads_lto);
  if (!ignore_stores)
    {
      if (cur_summary->stores)
	cur_summary->stores->merge (callee_summary->stores);
      if (cur_summary->stores_lto)
	cur_summary->stores_lto->merge (callee_summary->stores_lto);
    }

  return true;
}

/* Helper for analyze_stmt.  */

static bool
analyze_load (gimple *, tree, tree op, void *data)
{
  modref_summary *summary = (modref_summary *)data;

  if (dump_file)
    {
      fprintf (dump_file, " - Analyzing load: ");
      print_generic_expr (dump_file, op);
      fprintf (dump_file, "\n");
    }

  if (!record_access_p (op))
    return false;

  ao_ref r;
  ao_ref_init (&r, op);

  if (summary->loads)
    record_access (summary->loads, &r);
  if (summary->loads_lto)
    record_access_lto (summary->loads_lto, &r);
  return false;
}

/* Helper for analyze_stmt.  */

static bool
analyze_store (gimple *, tree, tree op, void *data)
{
  modref_summary *summary = (modref_summary *)data;

  if (dump_file)
    {
      fprintf (dump_file, " - Analyzing store: ");
      print_generic_expr (dump_file, op);
      fprintf (dump_file, "\n");
    }

  if (!record_access_p (op))
    return false;

  ao_ref r;
  ao_ref_init (&r, op);

  if (summary->stores)
    record_access (((modref_summary *)data)->stores, &r);
  if (summary->stores_lto)
    record_access_lto (((modref_summary *)data)->stores_lto, &r);
  return false;
}

/* Analyze statement STMT of function F.
   If IPA is true do not merge in side effects of calls.  */

static bool
analyze_stmt (modref_summary *summary, gimple *stmt, bool ipa)
{
  /* There is no need to record clobbers.  */
  if (gimple_clobber_p (stmt))
    return false;
  /* Analyze all loads and stores in STMT.  */
  walk_stmt_load_store_ops (stmt, summary,
			    analyze_load, analyze_store);
  /* or call analyze_load_ipa, analyze_store_ipa */

  switch (gimple_code (stmt))
   {
   case GIMPLE_ASM:
     /* If the ASM statement does not read nor write memory, there's nothing
	to do.  Otherwise just give up.  */
     if (!gimple_asm_clobbers_memory_p (as_a <gasm *> (stmt)))
       return true;
     if (dump_file)
       fprintf (dump_file, " - Function contains GIMPLE_ASM statement "
	       "which clobbers memory.\n");
     return false;
   case GIMPLE_CALL:
     if (!ipa)
       return analyze_call (summary, stmt);
     return true;
   default:
     /* Nothing to do for other types of statements.  */
     return true;
   }
}

/* Analyze function F.  IPA indicates whether we're running in tree mode (false)
   or the IPA mode (true).  */

static void
analyze_function (function *f, bool ipa)
{
  if (dump_file)
    fprintf (dump_file, "modref analyzing '%s' (ipa=%i)...\n",
	     function_name (f), ipa);

  /* Don't analyze this function if it's compiled with -fno-strict-aliasing.  */
  if (!flag_ipa_modref)
    return;

  /* Initialize the summary.  */
  if (!summaries)
    summaries = new (ggc_alloc <modref_summaries> ())
		     modref_summaries (symtab);
  else /* Remove existing summary if we are re-running the pass.  */
    summaries->remove (cgraph_node::get (f->decl));

  ((modref_summaries *)summaries)->ipa = ipa;

  modref_summary *summary = summaries->get_create (cgraph_node::get (f->decl));

  /* Compute no-LTO summaries when local optimization is going to happen.  */
  bool nolto = (!ipa || ((!flag_lto || flag_fat_lto_objects) && !in_lto_p)
		|| (in_lto_p && !flag_wpa
		    && flag_incremental_link != INCREMENTAL_LINK_LTO));

  /* Compute LTO when LTO streaming is going to happen.  */
  bool lto = ipa && ((flag_lto && !in_lto_p)
		     || flag_wpa
		     || flag_incremental_link == INCREMENTAL_LINK_LTO);

  /* Create and initialize summary for F.
     Note that summaries may be already allocated from previous
     run of the pass.  */
  if (nolto)
    {
      gcc_assert (!summary->loads);
      summary->loads
	 = new (ggc_alloc <modref_tree<alias_set_type> > ())
		modref_records (param_modref_max_bases,
				param_modref_max_refs);
      gcc_assert (!summary->stores);
      summary->stores
	 = new (ggc_alloc <modref_tree<alias_set_type> > ())
		modref_records (param_modref_max_bases,
				param_modref_max_refs);
    }
  if (lto)
    {
      gcc_assert (!summary->loads_lto);
      summary->loads_lto
	 = new (ggc_alloc <modref_tree<tree> > ())
		modref_records_lto (param_modref_max_bases,
				    param_modref_max_refs);
      gcc_assert (!summary->stores_lto);
      summary->stores_lto
	 = new (ggc_alloc <modref_tree<tree> > ())
		modref_records_lto (param_modref_max_bases,
				    param_modref_max_refs);
    }
  summary->finished = false;

  /* Analyze each statement in each basic block of the function.  If the
     statement cannot be analyzed (for any reason), the entire function cannot
     be analyzed by modref.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, f)
    {
      gimple_stmt_iterator si;
      for (si = gsi_after_labels (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  if (!analyze_stmt (summary, gsi_stmt (si), ipa))
	    {
	      cgraph_node *fnode = cgraph_node::get (current_function_decl);
	      summaries->remove (fnode);
	      if (dump_file)
		fprintf (dump_file,
			 " - modref done with result: not tracked.\n");
	      return;
	    }
	}
    }

  if (!ipa)
    summary->finished = true;

  if (dump_file)
    {
      fprintf (dump_file, " - modref done with result: tracked.\n");
      summary->dump (dump_file);
    }
}

/* Callback for generate_summary.  */

static void
modref_generate (void)
{
  struct cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *f = DECL_STRUCT_FUNCTION (node->decl);
      if (!f)
	continue;
      push_cfun (f);
      analyze_function (f, true);
      pop_cfun ();
    }
}

/* Called when a new function is inserted to callgraph late.  */

void
modref_summaries::insert (struct cgraph_node *node, modref_summary *)
{
  if (!DECL_STRUCT_FUNCTION (node->decl))
    return;
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  analyze_function (DECL_STRUCT_FUNCTION (node->decl), ipa);
  pop_cfun ();
}

/* Called when new clone is inserted to callgraph late.  */

void
modref_summaries::duplicate (cgraph_node *, cgraph_node *,
			     modref_summary *src_data,
			     modref_summary *dst_data)
{
  dst_data->finished = src_data->finished;
  if (src_data->stores)
    {
      dst_data->stores = new (ggc_alloc <modref_tree<alias_set_type> > ())
			      modref_records
				 (src_data->stores->max_bases,
				  src_data->stores->max_refs);
      dst_data->stores->merge (src_data->stores);
    }
  if (src_data->loads)
    {
      dst_data->loads = new (ggc_alloc <modref_tree<alias_set_type> > ())
			     modref_records
				(src_data->loads->max_bases,
				 src_data->loads->max_refs);
      dst_data->loads->merge (src_data->loads);
    }
  if (src_data->stores_lto)
    {
      dst_data->stores_lto = new (ggc_alloc <modref_tree<tree> > ())
				  modref_records_lto
				    (src_data->stores_lto->max_bases,
				     src_data->stores_lto->max_refs);
      dst_data->stores_lto->merge (src_data->stores_lto);
    }
  if (src_data->loads_lto)
    {
      dst_data->loads_lto = new (ggc_alloc <modref_tree<tree> > ())
				  modref_records_lto
				    (src_data->stores_lto->max_bases,
				     src_data->stores_lto->max_refs);
      dst_data->loads_lto->merge (src_data->loads_lto);
    }
}

namespace
{
/* Definition of the modref pass on GIMPLE.  */
const pass_data pass_data_modref = {
  GIMPLE_PASS,
  "modref",
  OPTGROUP_IPA,
  TV_TREE_MODREF,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  0,
};

class pass_modref : public gimple_opt_pass
{
  public:
    pass_modref (gcc::context *ctxt)
	: gimple_opt_pass (pass_data_modref, ctxt) {}

    /* opt_pass methods: */
    opt_pass *clone ()
    {
      return new pass_modref (m_ctxt);
    }
    virtual bool gate (function *)
    {
      return flag_ipa_modref;
    }
    virtual unsigned int execute (function *);
};

/* Encode TT to the output block OB using the summary streaming API.  */

static void
write_modref_records (modref_records_lto *tt, struct output_block *ob)
{
  streamer_write_uhwi (ob, tt->max_bases);
  streamer_write_uhwi (ob, tt->max_refs);

  streamer_write_uhwi (ob, tt->every_base);
  streamer_write_uhwi (ob, vec_safe_length (tt->bases));
  size_t i;
  modref_base_node <tree> *base_node;
  FOR_EACH_VEC_SAFE_ELT (tt->bases, i, base_node)
    {
      stream_write_tree (ob, base_node->base, true);

      streamer_write_uhwi (ob, base_node->every_ref);
      streamer_write_uhwi (ob, vec_safe_length (base_node->refs));
      size_t j;
      modref_ref_node <tree> *ref_node;
      FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	{
	  stream_write_tree (ob, ref_node->ref, true);
	}
    }
}

/* Read a modref_tree from the input block IB using the data from DATA_IN.
   This assumes that the tree was encoded using write_modref_tree.
   Either nolto_ret or lto_ret is initialized by the tree depending whether
   LTO streaming is expected or not.  */

void
read_modref_records (lto_input_block *ib, struct data_in *data_in,
		     modref_records **nolto_ret,
		     modref_records_lto **lto_ret)
{
  size_t max_bases = streamer_read_uhwi (ib);
  size_t max_refs = streamer_read_uhwi (ib);

  /* Decide whether we want to turn LTO data types to non-LTO (i.e. when
     LTO re-streaming is not going to happen).  */
  if (flag_wpa || flag_incremental_link == INCREMENTAL_LINK_LTO)
    *lto_ret = new (ggc_alloc <modref_records_lto> ()) modref_records_lto
			      (max_bases, max_refs);
  else
    *nolto_ret = new (ggc_alloc <modref_records> ()) modref_records
			      (max_bases, max_refs);

  size_t every_base = streamer_read_uhwi (ib);
  size_t nbase = streamer_read_uhwi (ib);

  gcc_assert (!every_base || nbase == 0);
  if (every_base)
    {
      if (*nolto_ret)
	(*nolto_ret)->collapse ();
      if (*lto_ret)
	(*lto_ret)->collapse ();
    }
  for (size_t i = 0; i < nbase; i++)
    {
      tree base_tree = stream_read_tree (ib, data_in);
      modref_base_node <alias_set_type> *nolto_base_node = NULL;
      modref_base_node <tree> *lto_base_node = NULL;

      /* At stream in time we have LTO alias info.  Check if we streamed in
	 something obviously unnecessary.  Do not glob types by alias sets;
	 it is not 100% clear that ltrans types will get merged same way.
	 Types may get refined based on ODR type conflicts.  */
      if (base_tree && !get_alias_set (base_tree))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Streamed in alias set 0 type ");
	      print_generic_expr (dump_file, base_tree);
	      fprintf (dump_file, "\n");
	    }
	  base_tree = NULL;
	}

      if (*nolto_ret)
	nolto_base_node = (*nolto_ret)->insert_base (base_tree
						     ? get_alias_set (base_tree)
						     : 0);
      if (*lto_ret)
	lto_base_node = (*lto_ret)->insert_base (base_tree);
      size_t every_ref = streamer_read_uhwi (ib);
      size_t nref = streamer_read_uhwi (ib);

      gcc_assert (!every_ref || nref == 0);
      if (every_ref)
	{
	  if (nolto_base_node)
	    nolto_base_node->collapse ();
	  if (lto_base_node)
	    lto_base_node->collapse ();
	}
      for (size_t j = 0; j < nref; j++)
	{
	  tree ref_tree = stream_read_tree (ib, data_in);

	  if (ref_tree && !get_alias_set (ref_tree))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Streamed in alias set 0 type ");
		  print_generic_expr (dump_file, ref_tree);
		  fprintf (dump_file, "\n");
		}
	      base_tree = NULL;
	    }

	  if (nolto_base_node)
	    nolto_base_node->insert_ref (ref_tree ? get_alias_set (ref_tree)
					 : 0, max_refs);
	  if (lto_base_node)
	    lto_base_node->insert_ref (ref_tree, max_refs);
	}
    }
}

/* Callback for write_summary.  */

static void
modref_write ()
{
  struct output_block *ob = create_output_block (LTO_section_ipa_modref);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;
  int i;

  if (!summaries)
    {
      streamer_write_uhwi (ob, 0);
      streamer_write_char_stream (ob->main_stream, 0);
      produce_asm (ob, NULL);
      destroy_output_block (ob);
      return;
    }

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);

      if (cnode && cnode->definition && !cnode->alias
	  && summaries->get (cnode))
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);

      if (cnode && cnode->definition && !cnode->alias)
	{

	  modref_summary *r = summaries->get (cnode);

	  if (!r)
	    continue;

	  streamer_write_uhwi (ob, lto_symtab_encoder_encode (encoder, cnode));

	  streamer_write_uhwi (ob, r->loads_lto ? 1 : 0);
	  streamer_write_uhwi (ob, r->stores_lto ? 1 : 0);
	  if (r->loads_lto)
	    write_modref_records (r->loads_lto, ob);
	  if (r->stores_lto)
	    write_modref_records (r->stores_lto, ob);
	}
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

static void
read_section (struct lto_file_decl_data *file_data, const char *data,
	      size_t len)
{
  const struct lto_function_header *header
    = (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  unsigned int i;
  unsigned int f_count;

  lto_input_block ib ((const char *) data + main_offset, header->main_size,
		      file_data->mode_table);

  data_in
    = lto_data_in_create (file_data, (const char *) data + string_offset,
			  header->string_size, vNULL);
  f_count = streamer_read_uhwi (&ib);
  for (i = 0; i < f_count; i++)
    {
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      unsigned int index = streamer_read_uhwi (&ib);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast <cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));

      modref_summary *modref_sum = summaries->get_create (node);
      modref_sum->finished = false;
      int have_loads = streamer_read_uhwi (&ib);
      int have_stores = streamer_read_uhwi (&ib);
      gcc_assert (!modref_sum->loads_lto
		  && !modref_sum->stores_lto
		  && !modref_sum->loads
		  && !modref_sum->stores);
      if (have_loads)
	 read_modref_records (&ib, data_in,
			      &modref_sum->loads,
			      &modref_sum->loads_lto);
      if (have_stores)
	 read_modref_records (&ib, data_in,
			      &modref_sum->stores,
			      &modref_sum->stores_lto);
      if (dump_file)
	{
	  fprintf (dump_file, "Read modref for %s\n",
		   node->dump_name ());
	  modref_sum->dump (dump_file);
	}
      if (flag_ltrans)
	modref_sum->finished = true;
    }

  lto_free_section_data (file_data, LTO_section_ipa_modref, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Callback for read_summary.  */

static void
modref_read (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  if (!summaries)
    summaries = new (ggc_alloc <modref_summaries> ())
		     modref_summaries (symtab);
  ((modref_summaries *)summaries)->ipa = true;

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_summary_section_data (file_data,
						       LTO_section_ipa_modref,
						       &len);
      if (data)
	read_section (file_data, data, len);
      else
	/* Fatal error here.  We do not want to support compiling ltrans units
	   with different version of compiler or different flags than the WPA
	   unit, so this should never happen.  */
	fatal_error (input_location,
		     "IPA modref summary is missing in input file");
    }
}

/* Definition of the modref IPA pass.  */
const pass_data pass_data_ipa_modref =
{
  IPA_PASS,           /* type */
  "modref",       /* name */
  OPTGROUP_IPA,       /* optinfo_flags */
  TV_IPA_MODREF, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_modref : public ipa_opt_pass_d
{
public:
  pass_ipa_modref (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_modref, ctxt,
		      modref_generate, /* generate_summary */
		      modref_write,    /* write_summary */
		      modref_read,     /* read_summary */
		      modref_write,    /* write_optimization_summary */
		      modref_read,     /* read_optimization_summary */
		      NULL,            /* stmt_fixup */
		      0,               /* function_transform_todo_flags_start */
		      NULL,            /* function_transform */
		      NULL)            /* variable_transform */
  {}

  /* opt_pass methods: */
  opt_pass *clone () { return new pass_ipa_modref (m_ctxt); }
  virtual bool gate (function *)
  {
    return true;
  }
  virtual unsigned int execute (function *);

};

}

unsigned int pass_modref::execute (function *f)
{
  /* If new function is being added during IPA, we can skip analysis.  */
  if (summaries && ((modref_summaries *)summaries)->ipa)
    return 0;
  analyze_function (f, false);
  return 0;
}

gimple_opt_pass *
make_pass_modref (gcc::context *ctxt)
{
  return new pass_modref (ctxt);
}

ipa_opt_pass_d *
make_pass_ipa_modref (gcc::context *ctxt)
{
  return new pass_ipa_modref (ctxt);
}

/* Skip edges from and to nodes without ipa_pure_const enabled.
   Ignore not available symbols.  */

static bool
ignore_edge (struct cgraph_edge *e)
{
  enum availability avail;
  cgraph_node *callee = e->callee->function_or_virtual_thunk_symbol
			  (&avail, e->caller);

  return (avail <= AVAIL_INTERPOSABLE
	  || !summaries->get (callee)
	  || flags_from_decl_or_type (e->callee->decl)
	     & (ECF_CONST | ECF_NOVOPS));
}

/* Run the IPA pass.  This will take a function's summaries and calls and
   construct new summaries which represent a transitive closure.  So that
   summary of an analyzed function contains information about the loads and
   stores that the function or any function that it calls does.  */

unsigned int pass_ipa_modref::execute (function *)
{
  if (!summaries)
    return 0;

  struct cgraph_node **order = XCNEWVEC (struct cgraph_node *,
					 symtab->cgraph_count);
  int order_pos;
  order_pos = ipa_reduced_postorder (order, true, ignore_edge);
  int i;

  /* Iterate over all strongly connected components in post-order.  */
  for (i = 0; i < order_pos; i++)
    {
      bool its_hopeless = false;
      modref_records *loads = NULL;
      modref_records *stores = NULL;
      modref_records_lto *loads_lto = NULL;
      modref_records_lto *stores_lto = NULL;

      /* Get the component's representative.  That's just any node in the
	 component from which we can traverse the entire component.  */
      struct cgraph_node *component_node = order[i];
      cgraph_node *first = NULL;

      if (dump_file)
	fprintf (dump_file, "Start of SCC component\n");

      /* Walk the component.  CUR is the current node of the component that's
	 being processed.  */
      for (struct cgraph_node *cur = component_node; cur && !its_hopeless;
	   cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
	{
	  /* Merge in summaries from CUR.  */
	  modref_summary *cur_summary = summaries->get (cur);

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s\n",
		     cur->dump_name ());

	  /* We don't know anything about CUR, hence we cannot tell anything
	     about the entire component.  */
	  if (!cur_summary)
	    {
	      if (dump_file)
		fprintf (dump_file, "    No summary\n");
	      its_hopeless = true;
	      break;
	    }

	  /* Summaries are all going to be same, pick first ones and merge
	     everything in.  */
	  if (!first)
	    {
	      first = cur;
	      loads = cur_summary->loads;
	      stores = cur_summary->stores;
	      loads_lto = cur_summary->loads_lto;
	      stores_lto = cur_summary->stores_lto;
	    }
	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      if (e->indirect_info->ecf_flags & (ECF_CONST | ECF_NOVOPS))
		continue;
	      if (ignore_stores_p (cur->decl, e->indirect_info->ecf_flags))
		{
		  if (dump_file)
		    fprintf (dump_file, "    Indirect call: "
			     "collapsing loads\n");
		  if (loads)
		    loads->collapse ();
		  if (loads_lto)
		    loads_lto->collapse ();
		}
	      else
		{
		  if (dump_file)
		    fprintf (dump_file, "    Indirect call: giving up\n");
		  its_hopeless = true;
		}
	    }

	  /* Walk every function that CUR calls and merge its summary.  */
	  for (cgraph_edge *callee_edge = cur->callees; callee_edge;
	       callee_edge = callee_edge->next_callee)
	    {
	      int flags = flags_from_decl_or_type (callee_edge->callee->decl);
	      modref_summary *callee_summary;
	      struct cgraph_node *callee;

	      if (flags & (ECF_CONST | ECF_NOVOPS))
		continue;

	      if (dump_file)
		fprintf (dump_file, "    Call to %s\n",
			 cur->dump_name ());

	      /* We can not safely optimize based on summary of callee if it
		 does not always bind to current def: it is possible that
		 memory load was optimized out earlier which may not happen in
		 the interposed variant.  */
	      if (!callee_edge->binds_to_current_def_p ())
		{
		  if (loads)
		    loads->collapse ();
		  if (loads_lto)
		    loads_lto->collapse ();
		  if (dump_file)
		    fprintf (dump_file, "      May not bind local;"
			     " collapsing loads\n");
		}

	      /* Get the callee and its summary.  */
	      enum availability avail;
	      callee = callee_edge->callee->function_or_virtual_thunk_symbol
			 (&avail, cur);

	      /* See if we can derive something from ECF flags.  Be careful on
		 not skipping calls within the SCC component:  we must merge
		 all their summaries.
		 If we switch to iterative dataflow that may be necessary
		 for future improvements this may go away.  */
	      if (callee->aux
		  && ((struct ipa_dfs_info *)cur->aux)->scc_no
		     == ((struct ipa_dfs_info *)callee->aux)->scc_no)
		flags = 0;

	      bool ignore_stores = ignore_stores_p (cur->decl, flags);

	      /* We don't know anything about CALLEE, hence we cannot tell
		 anything about the entire component.  */

	      if (avail <= AVAIL_INTERPOSABLE
		  || !(callee_summary = summaries->get (callee)))
		{
		  if (!ignore_stores)
		    {
		      its_hopeless = true;
		      if (dump_file && avail <= AVAIL_INTERPOSABLE)
			fprintf (dump_file, "      Call target interposable"
				 "or not available\n");
		      else if (dump_file)
			fprintf (dump_file, "      No call target summary\n");
		      break;
		    }
		  else
		    {
		      if (loads)
			loads->collapse ();
		      if (loads_lto)
			loads_lto->collapse ();
		      if (dump_file && avail <= AVAIL_INTERPOSABLE)
			fprintf (dump_file, "      Call target interposable"
				 "or not available; collapsing loads\n");
		      else if (dump_file)
			fprintf (dump_file, "      No call target summary;"
				 " collapsing loads\n");
		      continue;
		    }
		}

	      /* Merge in callee's information.  */
	      if (callee_summary->loads
		  && callee_summary->loads != loads)
		loads->merge (callee_summary->loads);
	      if (callee_summary->stores
		  && callee_summary->stores != stores)
		stores->merge (callee_summary->stores);
	      if (callee_summary->loads_lto
		  && callee_summary->loads_lto != loads_lto)
		loads_lto->merge (callee_summary->loads_lto);
	      if (callee_summary->stores_lto
		  && callee_summary->stores_lto != stores_lto)
		stores_lto->merge (callee_summary->stores_lto);
	    }
	}

	/* At this time, ipa_loads and ipa_stores contain information
	   about all loads and stores done by any of the component's nodes and
	   all functions that any of the nodes calls.  We will now propagate
	   this information to all nodes in the component.  Therefore, we will
	   walk the component one more time to do it.  */
	for (struct cgraph_node *cur = component_node; cur;
	   cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
	{
	  modref_summary *cur_summary = summaries->get (cur);
	  if (!cur_summary)
	    {
	      /* The function doesn't have a summary.  We must have noticed
		 that during the first pass and the hopeless flag must
		 therefore be set.  Skip the function.  */
	      gcc_assert (its_hopeless);
	    }
	  else if (its_hopeless)
	    {
	      if (dump_file)
		fprintf (dump_file, "Cleared modref info for %s\n",
			 cur->dump_name ());
	      summaries->remove (cur);
	    }
	  else
	    {
	      if (cur == first)
		;
	      else
		{
		  if (loads)
		    cur_summary->loads->merge (loads);
		  if (stores)
		    cur_summary->stores->merge (stores);
		  if (loads_lto)
		    cur_summary->loads_lto->merge (loads_lto);
		  if (stores_lto)
		    cur_summary->stores_lto->merge (stores_lto);
		}
	      cur_summary->finished = true;
	      if (dump_file)
		{
		  fprintf (dump_file, "Propagated modref for %s%s%s\n",
			   cur->dump_name (),
			   TREE_READONLY (cur->decl) ? " (const)" : "",
			   DECL_PURE_P (cur->decl) ? " (pure)" : "");
		  cur_summary->dump (dump_file);
		}
	    }
	}
    }
  ((modref_summaries *)summaries)->ipa = false;
  ipa_free_postorder_info ();
  return 0;
}

/* Summaries must stay alive until end of compilation.  */

void
ipa_modref_c_finalize ()
{
  if (summaries)
    ggc_delete (summaries);
  summaries = NULL;
}

#include "gt-ipa-modref.h"
