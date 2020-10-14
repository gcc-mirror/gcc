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
   accesses across function calls.  The summary has a form of decision tree
   described in ipa-modref-tree.h.

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
#include "value-range.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"

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
  static modref_summaries *create_ggc (symbol_table *symtab)
  {
    return new (ggc_alloc_no_dtor<modref_summaries> ())
	     modref_summaries (symtab);
  }
};

class modref_summary_lto;

/* Class (from which there is one global instance) that holds modref summaries
   for all analyzed functions.  */
class GTY((user)) modref_summaries_lto
  : public fast_function_summary <modref_summary_lto *, va_gc>
{
public:
  modref_summaries_lto (symbol_table *symtab)
      : fast_function_summary <modref_summary_lto *, va_gc> (symtab),
	propagated (false) {}
  virtual void insert (cgraph_node *, modref_summary_lto *state);
  virtual void duplicate (cgraph_node *src_node,
			  cgraph_node *dst_node,
			  modref_summary_lto *src_data,
			  modref_summary_lto *dst_data);
  static modref_summaries_lto *create_ggc (symbol_table *symtab)
  {
    return new (ggc_alloc_no_dtor<modref_summaries_lto> ())
	     modref_summaries_lto (symtab);
  }
  bool propagated;
};

/* Global variable holding all modref summaries
   (from analysis to IPA propagation time).  */
static GTY(()) fast_function_summary <modref_summary *, va_gc>
	 *summaries;

/* Global variable holding all modref optimizaiton summaries
   (from IPA propagation time or used by local optimization pass).  */
static GTY(()) fast_function_summary <modref_summary *, va_gc>
	 *optimization_summaries;

/* LTO summaries hold info from analysis to LTO streaming or from LTO
   stream-in through propagation to LTO stream-out.  */
static GTY(()) fast_function_summary <modref_summary_lto *, va_gc>
	 *summaries_lto;

/* Summary for a single function which this pass produces.  */

modref_summary::modref_summary ()
  : loads (NULL), stores (NULL)
{
}

modref_summary::~modref_summary ()
{
  if (loads)
    ggc_delete (loads);
  if (stores)
    ggc_delete (stores);
}

/* Return true if summary is potentially useful for optimization.  */

bool
modref_summary::useful_p (int ecf_flags)
{
  if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
    return false;
  if (loads && !loads->every_base)
    return true;
  if (ecf_flags & ECF_PURE)
    return false;
  return stores && !stores->every_base;
}

/* Single function summary used for LTO.  */

typedef modref_tree <tree> modref_records_lto;
struct GTY(()) modref_summary_lto
{
  /* Load and stores in functions using types rather then alias sets.

     This is necessary to make the information streamable for LTO but is also
     more verbose and thus more likely to hit the limits.  */
  modref_records_lto *loads;
  modref_records_lto *stores;

  modref_summary_lto ();
  ~modref_summary_lto ();
  void dump (FILE *);
  bool useful_p (int ecf_flags);
};

/* Summary for a single function which this pass produces.  */

modref_summary_lto::modref_summary_lto ()
  : loads (NULL), stores (NULL)
{
}

modref_summary_lto::~modref_summary_lto ()
{
  if (loads)
    ggc_delete (loads);
  if (stores)
    ggc_delete (stores);
}


/* Return true if lto summary is potentially useful for optimization.  */

bool
modref_summary_lto::useful_p (int ecf_flags)
{
  if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
    return false;
  if (loads && !loads->every_base)
    return true;
  if (ecf_flags & ECF_PURE)
    return false;
  return stores && !stores->every_base;
}

/* Dump A to OUT.  */

static void
dump_access (modref_access_node *a, FILE *out)
{
  fprintf (out, "          access:");
  if (a->parm_index != -1)
    {
      fprintf (out, " Parm %i", a->parm_index);
      if (a->parm_offset_known)
	{
	  fprintf (out, " param offset:");
	  print_dec ((poly_int64_pod)a->parm_offset, out, SIGNED);
	}
    }
  if (a->range_info_useful_p ())
    {
      fprintf (out, " offset:");
      print_dec ((poly_int64_pod)a->offset, out, SIGNED);
      fprintf (out, " size:");
      print_dec ((poly_int64_pod)a->size, out, SIGNED);
      fprintf (out, " max_size:");
      print_dec ((poly_int64_pod)a->max_size, out, SIGNED);
    }
  fprintf (out, "\n");
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
	  if (r->every_access)
	    {
	      fprintf (out, "          Every access\n");
	      continue;
	    }
	  size_t k;
	  modref_access_node *a;
	  FOR_EACH_VEC_SAFE_ELT (r->accesses, k, a)
	    dump_access (a, out);
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
	  if (r->every_access)
	    {
	      fprintf (out, "          Every access\n");
	      continue;
	    }
	  size_t k;
	  modref_access_node *a;
	  FOR_EACH_VEC_SAFE_ELT (r->accesses, k, a)
	    dump_access (a, out);
	}
    }
}

/* Dump summary.  */

void
modref_summary::dump (FILE *out)
{
  fprintf (out, "  loads:\n");
  dump_records (loads, out);
  fprintf (out, "  stores:\n");
  dump_records (stores, out);
}

/* Dump summary.  */

void
modref_summary_lto::dump (FILE *out)
{
  fprintf (out, "  loads:\n");
  dump_lto_records (loads, out);
  fprintf (out, "  stores:\n");
  dump_lto_records (stores, out);
}

/* Get function summary for FUNC if it exists, return NULL otherwise.  */

modref_summary *
get_modref_function_summary (cgraph_node *func)
{
  /* Avoid creation of the summary too early (e.g. when front-end calls us).  */
  if (!optimization_summaries)
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

  modref_summary *r = optimization_summaries->get (func);
  return r;
}

/* Construct modref_access_node from REF.  */
static modref_access_node
get_access (ao_ref *ref)
{
  tree base;

  base = ao_ref_base (ref);
  modref_access_node a = {ref->offset, ref->size, ref->max_size,
			  0, -1, false};
  if (TREE_CODE (base) == MEM_REF || TREE_CODE (base) == TARGET_MEM_REF)
    {
      tree memref = base;
      base = TREE_OPERAND (base, 0);
      if (TREE_CODE (base) == SSA_NAME
	  && SSA_NAME_IS_DEFAULT_DEF (base)
	  && TREE_CODE (SSA_NAME_VAR (base)) == PARM_DECL)
	{
	  a.parm_index = 0;
	  for (tree t = DECL_ARGUMENTS (current_function_decl);
	       t != SSA_NAME_VAR (base); t = DECL_CHAIN (t))
	    {
	      if (!t)
		{
		  a.parm_index = -1;
		  break;
		}
	      a.parm_index++;
	    }
	  if (TREE_CODE (memref) == MEM_REF)
	    {
	      a.parm_offset_known
		 = wi::to_poly_wide (TREE_OPERAND
					 (memref, 1)).to_shwi (&a.parm_offset);
	    }
	  else
	    a.parm_offset_known = false;
	}
      else
	a.parm_index = -1;
    }
  else
    a.parm_index = -1;
  return a;
}

/* Record access into the modref_records data structure.  */

static void
record_access (modref_records *tt, ao_ref *ref)
{
  alias_set_type base_set = !flag_strict_aliasing ? 0
			    : ao_ref_base_alias_set (ref);
  alias_set_type ref_set = !flag_strict_aliasing ? 0
			    : (ao_ref_alias_set (ref));
  modref_access_node a = get_access (ref);
  if (dump_file)
    {
       fprintf (dump_file, "   - Recording base_set=%i ref_set=%i parm=%i\n",
		base_set, ref_set, a.parm_index);
    }
  tt->insert (base_set, ref_set, a);
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
  modref_access_node a = get_access (ref);
  if (dump_file)
    {
      fprintf (dump_file, "   - Recording base type:");
      print_generic_expr (dump_file, base_type);
      fprintf (dump_file, " (alias set %i) ref type:",
	       base_type ? get_alias_set (base_type) : 0);
      print_generic_expr (dump_file, ref_type);
      fprintf (dump_file, " (alias set %i) parm:%i\n",
	       ref_type ? get_alias_set (ref_type) : 0,
	       a.parm_index);
    }

  tt->insert (base_type, ref_type, a);
}

/* Returns true if and only if we should store the access to EXPR.
   Some accesses, e.g. loads from automatic variables, are not interesting.  */

static bool
record_access_p (tree expr)
{
  if (refs_local_or_readonly_memory_p (expr))
    {
      if (dump_file)
	fprintf (dump_file, "   - Read-only or local, ignoring.\n");
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

/* Merge side effects of call STMT to function with CALLEE_SUMMARY
   int CUR_SUMMARY.  Return true if something changed.
   If IGNORE_STORES is true, do not merge stores.  */

bool
merge_call_side_effects (modref_summary *cur_summary,
			 gimple *stmt, modref_summary *callee_summary,
			 bool ignore_stores, cgraph_node *callee_node)
{
  auto_vec <modref_parm_map, 32> parm_map;
  bool changed = false;

  if (dump_file)
    fprintf (dump_file, " - Merging side effects of %s with parm map:",
	     callee_node->dump_name ());

  parm_map.safe_grow_cleared (gimple_call_num_args (stmt));
  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree op = gimple_call_arg (stmt, i);
      bool offset_known;
      poly_int64 offset;

      offset_known = unadjusted_ptr_and_unit_offset (op, &op, &offset);
      if (TREE_CODE (op) == SSA_NAME
	  && SSA_NAME_IS_DEFAULT_DEF (op)
	  && TREE_CODE (SSA_NAME_VAR (op)) == PARM_DECL)
	{
	  int index = 0;
	  for (tree t = DECL_ARGUMENTS (current_function_decl);
	       t != SSA_NAME_VAR (op); t = DECL_CHAIN (t))
	    {
	      if (!t)
		{
		  index = -1;
		  break;
		}
	      index++;
	    }
	  parm_map[i].parm_index = index;
	  parm_map[i].parm_offset_known = offset_known;
	  parm_map[i].parm_offset = offset;
	}
      else if (points_to_local_or_readonly_memory_p (op))
	parm_map[i].parm_index = -2;
      else
	parm_map[i].parm_index = -1;
      if (dump_file)
	{
	  fprintf (dump_file, " %i", parm_map[i].parm_index);
	  if (parm_map[i].parm_offset_known)
	    {
	      fprintf (dump_file, " offset:");
	      print_dec ((poly_int64_pod)parm_map[i].parm_offset,
			 dump_file, SIGNED);
	    }
	}
    }
  if (dump_file)
    fprintf (dump_file, "\n");

  /* Merge with callee's summary.  */
  changed |= cur_summary->loads->merge (callee_summary->loads, &parm_map);
  if (!ignore_stores)
    changed |= cur_summary->stores->merge (callee_summary->stores,
					   &parm_map);
  return changed;
}

/* Analyze function call STMT in function F.
   Remember recursive calls in RECURSIVE_CALLS.  */

static bool
analyze_call (modref_summary *cur_summary,
	      gimple *stmt, vec <gimple *> *recursive_calls)
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
	  cur_summary->loads->collapse ();
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
      cur_summary->loads->collapse ();
    }

  /* If this is a recursive call, the target summary is the same as ours, so
     there's nothing to do.  */
  if (recursive_call_p (current_function_decl, callee))
    {
      recursive_calls->safe_push (stmt);
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
	  cur_summary->loads->collapse ();
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, " - Function availability <= AVAIL_INTERPOSABLE.\n");
      return false;
    }

  /* Get callee's modref summary.  As above, if there's no summary, we either
     have to give up or, if stores are ignored, we can just purge loads.  */
  modref_summary *callee_summary = optimization_summaries->get (callee_node);
  if (!callee_summary)
    {
      if (ignore_stores)
	{
	  cur_summary->loads->collapse ();
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, " - No modref summary available for callee.\n");
      return false;
    }

  merge_call_side_effects (cur_summary, stmt, callee_summary, ignore_stores,
			   callee_node);

  return true;
}

/* Support analyzis in non-lto and lto mode in parallel.  */

struct summary_ptrs
{
  struct modref_summary *nolto;
  struct modref_summary_lto *lto;
};

/* Helper for analyze_stmt.  */

static bool
analyze_load (gimple *, tree, tree op, void *data)
{
  modref_summary *summary = ((summary_ptrs *)data)->nolto;
  modref_summary_lto *summary_lto = ((summary_ptrs *)data)->lto;

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

  if (summary)
    record_access (summary->loads, &r);
  if (summary_lto)
    record_access_lto (summary_lto->loads, &r);
  return false;
}

/* Helper for analyze_stmt.  */

static bool
analyze_store (gimple *, tree, tree op, void *data)
{
  modref_summary *summary = ((summary_ptrs *)data)->nolto;
  modref_summary_lto *summary_lto = ((summary_ptrs *)data)->lto;

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

  if (summary)
    record_access (summary->stores, &r);
  if (summary_lto)
    record_access_lto (summary_lto->stores, &r);
  return false;
}

/* Analyze statement STMT of function F.
   If IPA is true do not merge in side effects of calls.  */

static bool
analyze_stmt (modref_summary *summary, modref_summary_lto *summary_lto,
	      gimple *stmt, bool ipa, vec <gimple *> *recursive_calls)
{
  /* In general we can not ignore clobbers because they are barries for code
     motion, however after inlining it is safe to do becuase local optimization
     passes do not consider clobbers from other functions.
     Similar logic is in ipa-pure-consts.  */
  if ((ipa || cfun->after_inlining) && gimple_clobber_p (stmt))
    return true;

  struct summary_ptrs sums = {summary, summary_lto};

  /* Analyze all loads and stores in STMT.  */
  walk_stmt_load_store_ops (stmt, &sums,
			    analyze_load, analyze_store);

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
       return analyze_call (summary, stmt, recursive_calls);
     return true;
   default:
     /* Nothing to do for other types of statements.  */
     return true;
   }
}

/* Remove summary of current function because during the function body
   scan we determined it is not useful.  LTO, NOLTO and IPA determines the
   mode of scan.  */

static void
remove_summary (bool lto, bool nolto, bool ipa)
{
  cgraph_node *fnode = cgraph_node::get (current_function_decl);
  if (!ipa)
    optimization_summaries->remove (fnode);
  else
    {
      if (nolto)
	summaries->remove (fnode);
      if (lto)
	summaries_lto->remove (fnode);
    }
  if (dump_file)
    fprintf (dump_file,
	     " - modref done with result: not tracked.\n");
}

/* Analyze function F.  IPA indicates whether we're running in local mode
   (false) or the IPA mode (true).  */

static void
analyze_function (function *f, bool ipa)
{
  if (dump_file)
    fprintf (dump_file, "modref analyzing '%s' (ipa=%i)%s%s\n",
	     function_name (f), ipa,
	     TREE_READONLY (current_function_decl) ? " (const)" : "",
	     DECL_PURE_P (current_function_decl) ? " (pure)" : "");

  /* Don't analyze this function if it's compiled with -fno-strict-aliasing.  */
  if (!flag_ipa_modref)
    return;

  /* Compute no-LTO summaries when local optimization is going to happen.  */
  bool nolto = (!ipa || ((!flag_lto || flag_fat_lto_objects) && !in_lto_p)
		|| (in_lto_p && !flag_wpa
		    && flag_incremental_link != INCREMENTAL_LINK_LTO));
  /* Compute LTO when LTO streaming is going to happen.  */
  bool lto = ipa && ((flag_lto && !in_lto_p)
		     || flag_wpa
		     || flag_incremental_link == INCREMENTAL_LINK_LTO);
  cgraph_node *fnode = cgraph_node::get (current_function_decl);

  modref_summary *summary = NULL;
  modref_summary_lto *summary_lto = NULL;

  /* Initialize the summary.
     If we run in local mode there is possibly pre-existing summary from
     IPA pass.  Dump it so it is easy to compare if mod-ref info has
     improved.  */
  if (!ipa)
    {
      if (!optimization_summaries)
	optimization_summaries = modref_summaries::create_ggc (symtab);
      else /* Remove existing summary if we are re-running the pass.  */
	{
	  if (dump_file
	      && (summary
		  = optimization_summaries->get (cgraph_node::get (f->decl)))
		 != NULL
	      && summary->loads)
	    {
	      fprintf (dump_file, "Past summary:\n");
	      optimization_summaries->get
		 (cgraph_node::get (f->decl))->dump (dump_file);
	    }
	  optimization_summaries->remove (cgraph_node::get (f->decl));
	}
      summary = optimization_summaries->get_create (cgraph_node::get (f->decl));
      gcc_checking_assert (nolto && !lto);
    }
  /* In IPA mode we analyze every function precisely once.  Asser that.  */
  else
    {
      if (nolto)
	{
	  if (!summaries)
	    summaries = modref_summaries::create_ggc (symtab);
	  else
	    summaries->remove (cgraph_node::get (f->decl));
	  summary = summaries->get_create (cgraph_node::get (f->decl));
	}
      if (lto)
	{
	  if (!summaries_lto)
	    summaries_lto = modref_summaries_lto::create_ggc (symtab);
	  else
	    summaries_lto->remove (cgraph_node::get (f->decl));
	  summary_lto = summaries_lto->get_create (cgraph_node::get (f->decl));
	}
     }


  /* Create and initialize summary for F.
     Note that summaries may be already allocated from previous
     run of the pass.  */
  if (nolto)
    {
      gcc_assert (!summary->loads);
      summary->loads = modref_records::create_ggc (param_modref_max_bases,
						   param_modref_max_refs,
						   param_modref_max_accesses);
      gcc_assert (!summary->stores);
      summary->stores = modref_records::create_ggc (param_modref_max_bases,
						    param_modref_max_refs,
						    param_modref_max_accesses);
    }
  if (lto)
    {
      gcc_assert (!summary_lto->loads);
      summary_lto->loads = modref_records_lto::create_ggc
				 (param_modref_max_bases,
				  param_modref_max_refs,
				  param_modref_max_accesses);
      gcc_assert (!summary_lto->stores);
      summary_lto->stores = modref_records_lto::create_ggc
				 (param_modref_max_bases,
				  param_modref_max_refs,
				  param_modref_max_accesses);
    }
  int ecf_flags = flags_from_decl_or_type (current_function_decl);
  auto_vec <gimple *, 32> recursive_calls;

  /* Analyze each statement in each basic block of the function.  If the
     statement cannot be analyzed (for any reason), the entire function cannot
     be analyzed by modref.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, f)
    {
      gimple_stmt_iterator si;
      for (si = gsi_after_labels (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  if (!analyze_stmt (summary, summary_lto,
			     gsi_stmt (si), ipa, &recursive_calls)
	      || ((!summary || !summary->useful_p (ecf_flags))
		  && (!summary_lto || !summary_lto->useful_p (ecf_flags))))
	    {
	      remove_summary (lto, nolto, ipa);
	      return;
	    }
	}
    }

  /* In non-IPA mode we need to perform iterative datafow on recursive calls.
     This needs to be done after all other side effects are computed.  */
  if (!ipa)
    {
      bool changed = true;
      while (changed)
	{
	  changed = false;
	  for (unsigned i = 0; i < recursive_calls.length (); i++)
	    {
	      changed |= merge_call_side_effects
			  (summary, recursive_calls[i], summary,
			   ignore_stores_p (current_function_decl,
					    gimple_call_flags
						 (recursive_calls[i])),
			   fnode);
	      if (!summary->useful_p (ecf_flags))
		{
		  remove_summary (lto, nolto, ipa);
		  return;
		}
	    }
	}
    }
  if (summary && !summary->useful_p (ecf_flags))
    {
      if (!ipa)
	optimization_summaries->remove (fnode);
      else
	summaries->remove (fnode);
      summary = NULL;
    }
  if (summary_lto && !summary_lto->useful_p (ecf_flags))
    {
      summaries_lto->remove (fnode);
      summary_lto = NULL;
    }

  if (dump_file)
    {
      fprintf (dump_file, " - modref done with result: tracked.\n");
      if (summary)
	summary->dump (dump_file);
      if (summary_lto)
	summary_lto->dump (dump_file);
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
  /* Local passes ought to be executed by the pass manager.  */
  if (this == optimization_summaries)
    {
      optimization_summaries->remove (node);
      return;
    }
  if (!DECL_STRUCT_FUNCTION (node->decl))
    {
      summaries->remove (node);
      return;
    }
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  analyze_function (DECL_STRUCT_FUNCTION (node->decl), true);
  pop_cfun ();
}

/* Called when a new function is inserted to callgraph late.  */

void
modref_summaries_lto::insert (struct cgraph_node *node, modref_summary_lto *)
{
  /* We do not support adding new function when IPA information is already
     propagated.  This is done only by SIMD cloning that is not very
     critical.  */
  if (!DECL_STRUCT_FUNCTION (node->decl)
      || propagated)
    {
      summaries_lto->remove (node);
      return;
    }
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  analyze_function (DECL_STRUCT_FUNCTION (node->decl), true);
  pop_cfun ();
}

/* Called when new clone is inserted to callgraph late.  */

void
modref_summaries::duplicate (cgraph_node *, cgraph_node *dst,
			     modref_summary *src_data,
			     modref_summary *dst_data)
{
  /* Do not duplicte optimization summaries; we do not handle parameter
     transforms on them.  */
  if (this == optimization_summaries)
    {
      optimization_summaries->remove (dst);
      return;
    }
  dst_data->stores = modref_records::create_ggc
			(src_data->stores->max_bases,
			 src_data->stores->max_refs,
			 src_data->stores->max_accesses);
  dst_data->stores->copy_from (src_data->stores);
  dst_data->loads = modref_records::create_ggc
			(src_data->loads->max_bases,
			 src_data->loads->max_refs,
			 src_data->loads->max_accesses);
  dst_data->loads->copy_from (src_data->loads);
}

/* Called when new clone is inserted to callgraph late.  */

void
modref_summaries_lto::duplicate (cgraph_node *, cgraph_node *,
				 modref_summary_lto *src_data,
				 modref_summary_lto *dst_data)
{
  dst_data->stores = modref_records_lto::create_ggc
			(src_data->stores->max_bases,
			 src_data->stores->max_refs,
			 src_data->stores->max_accesses);
  dst_data->stores->copy_from (src_data->stores);
  dst_data->loads = modref_records_lto::create_ggc
			(src_data->loads->max_bases,
			 src_data->loads->max_refs,
			 src_data->loads->max_accesses);
  dst_data->loads->copy_from (src_data->loads);
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
  streamer_write_uhwi (ob, tt->max_accesses);

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
	  streamer_write_uhwi (ob, ref_node->every_access);
	  streamer_write_uhwi (ob, vec_safe_length (ref_node->accesses));

	  size_t k;
	  modref_access_node *access_node;
	  FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
	    {
	      streamer_write_hwi (ob, access_node->parm_index);
	      if (access_node->parm_index != -1)
		{
		  streamer_write_uhwi (ob, access_node->parm_offset_known);
		  if (access_node->parm_offset_known)
		    {
		      streamer_write_poly_int64 (ob, access_node->parm_offset);
		      streamer_write_poly_int64 (ob, access_node->offset);
		      streamer_write_poly_int64 (ob, access_node->size);
		      streamer_write_poly_int64 (ob, access_node->max_size);
		    }
		}
	    }
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
  size_t max_accesses = streamer_read_uhwi (ib);

  if (lto_ret)
    *lto_ret = modref_records_lto::create_ggc (max_bases, max_refs,
					       max_accesses);
  if (nolto_ret)
    *nolto_ret = modref_records::create_ggc (max_bases, max_refs,
					     max_accesses);
  gcc_checking_assert (lto_ret || nolto_ret);

  size_t every_base = streamer_read_uhwi (ib);
  size_t nbase = streamer_read_uhwi (ib);

  gcc_assert (!every_base || nbase == 0);
  if (every_base)
    {
      if (nolto_ret)
	(*nolto_ret)->collapse ();
      if (lto_ret)
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

      if (nolto_ret)
	nolto_base_node = (*nolto_ret)->insert_base (base_tree
						     ? get_alias_set (base_tree)
						     : 0);
      if (lto_ret)
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
	      ref_tree = NULL;
	    }

	  modref_ref_node <alias_set_type> *nolto_ref_node = NULL;
	  modref_ref_node <tree> *lto_ref_node = NULL;

	  if (nolto_base_node)
	    nolto_ref_node
	      = nolto_base_node->insert_ref (ref_tree
					     ? get_alias_set (ref_tree) : 0,
					     max_refs);
	  if (lto_base_node)
	    lto_ref_node = lto_base_node->insert_ref (ref_tree, max_refs);

	  size_t every_access = streamer_read_uhwi (ib);
	  size_t naccesses = streamer_read_uhwi (ib);

	  if (nolto_ref_node)
	    nolto_ref_node->every_access = every_access;
	  if (lto_ref_node)
	    lto_ref_node->every_access = every_access;

	  for (size_t k = 0; k < naccesses; k++)
	    {
	      int parm_index = streamer_read_hwi (ib);
	      bool parm_offset_known = false;
	      poly_int64 parm_offset = 0;
	      poly_int64 offset = 0;
	      poly_int64 size = -1;
	      poly_int64 max_size = -1;

	      if (parm_index != -1)
		{
		  parm_offset_known = streamer_read_uhwi (ib);
		  if (parm_offset_known)
		    {
		      parm_offset = streamer_read_poly_int64 (ib);
		      offset = streamer_read_poly_int64 (ib);
		      size = streamer_read_poly_int64 (ib);
		      max_size = streamer_read_poly_int64 (ib);
		    }
		}
	      modref_access_node a = {offset, size, max_size, parm_offset,
				      parm_index, parm_offset_known};
	      if (nolto_ref_node)
		nolto_ref_node->insert_access (a, max_accesses);
	      if (lto_ref_node)
		lto_ref_node->insert_access (a, max_accesses);
	    }
	}
    }
  if (lto_ret)
    (*lto_ret)->cleanup ();
  if (nolto_ret)
    (*nolto_ret)->cleanup ();
}

/* Callback for write_summary.  */

static void
modref_write ()
{
  struct output_block *ob = create_output_block (LTO_section_ipa_modref);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;
  int i;

  if (!summaries_lto)
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
      modref_summary_lto *r;

      if (cnode && cnode->definition && !cnode->alias
	  && (r = summaries_lto->get (cnode))
	  && r->useful_p (flags_from_decl_or_type (cnode->decl)))
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);

      if (cnode && cnode->definition && !cnode->alias)
	{

	  modref_summary_lto *r = summaries_lto->get (cnode);

	  if (!r || !r->useful_p (flags_from_decl_or_type (cnode->decl)))
	    continue;

	  streamer_write_uhwi (ob, lto_symtab_encoder_encode (encoder, cnode));

	  write_modref_records (r->loads, ob);
	  write_modref_records (r->stores, ob);
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

      modref_summary *modref_sum = summaries
				   ? summaries->get_create (node) : NULL;
      modref_summary_lto *modref_sum_lto = summaries_lto
					   ? summaries_lto->get_create (node)
					   : NULL;

      if (optimization_summaries)
	modref_sum = optimization_summaries->get_create (node);

      gcc_assert (!modref_sum || (!modref_sum->loads
				  && !modref_sum->stores));
      gcc_assert (!modref_sum_lto || (!modref_sum_lto->loads
				      && !modref_sum_lto->stores));
      read_modref_records (&ib, data_in,
			   modref_sum ? &modref_sum->loads : NULL,
			   modref_sum_lto ? &modref_sum_lto->loads : NULL);
      read_modref_records (&ib, data_in,
			   modref_sum ? &modref_sum->stores : NULL,
			   modref_sum_lto ? &modref_sum_lto->stores : NULL);
      if (dump_file)
	{
	  fprintf (dump_file, "Read modref for %s\n",
		   node->dump_name ());
	  if (modref_sum)
	    modref_sum->dump (dump_file);
	  if (modref_sum_lto)
	    modref_sum_lto->dump (dump_file);
	}
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

  gcc_checking_assert (!optimization_summaries && !summaries && !summaries_lto);
  if (flag_ltrans)
    optimization_summaries = modref_summaries::create_ggc (symtab);
  else
    {
      if (flag_wpa || flag_incremental_link == INCREMENTAL_LINK_LTO)
	summaries_lto = modref_summaries_lto::create_ggc (symtab);
      if (!flag_wpa
	  || (flag_incremental_link == INCREMENTAL_LINK_LTO
	      && flag_fat_lto_objects))
	summaries = modref_summaries::create_ggc (symtab);
    }

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

/* Update parameter indexes in TT according to MAP.  */

void
remap_arguments (vec <int> *map, modref_records *tt)
{
  size_t i;
  modref_base_node <alias_set_type> *base_node;
  FOR_EACH_VEC_SAFE_ELT (tt->bases, i, base_node)
    {
      size_t j;
      modref_ref_node <alias_set_type> *ref_node;
      FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	{
	  size_t k;
	  modref_access_node *access_node;
	  FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
	    if (access_node->parm_index > 0)
	      {
		if (access_node->parm_index < (int)map->length ())
		  access_node->parm_index = (*map)[access_node->parm_index];
		else
		  access_node->parm_index = -1;
	      }
	}
    }
}

/* If signature changed, update the summary.  */

static unsigned int
modref_transform (struct cgraph_node *node)
{
  if (!node->clone.param_adjustments || !optimization_summaries)
    return 0;
  modref_summary *r = optimization_summaries->get (node);
  if (!r)
    return 0;
  if (dump_file)
    {
      fprintf (dump_file, "Updating summary for %s from:\n",
	       node->dump_name ());
      r->dump (dump_file);
    }

  size_t i, max = 0;
  ipa_adjusted_param *p;

  FOR_EACH_VEC_SAFE_ELT (node->clone.param_adjustments->m_adj_params, i, p)
    {
      int idx = node->clone.param_adjustments->get_original_index (i);
      if (idx > (int)max)
	max = idx;
    }

  auto_vec <int, 32> map;

  map.reserve (max + 1);
  for (i = 0; i <= max; i++)
    map.quick_push (-1);
  FOR_EACH_VEC_SAFE_ELT (node->clone.param_adjustments->m_adj_params, i, p)
    {
      int idx = node->clone.param_adjustments->get_original_index (i);
      if (idx >= 0)
	map[idx] = i;
    }
  remap_arguments (&map, r->loads);
  remap_arguments (&map, r->stores);
  if (dump_file)
    {
      fprintf (dump_file, "to:\n");
      r->dump (dump_file);
    }
  return 0;
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
		      modref_transform,/* function_transform */
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
  /* We merge summaries of inline clones into summaries of functions they
     are inlined to.  For that reason the complete function bodies must
     act as unit.  */
  if (!e->inline_failed)
    return false;
  enum availability avail;
  cgraph_node *callee = e->callee->function_or_virtual_thunk_symbol
			  (&avail, e->caller);

  return (avail <= AVAIL_INTERPOSABLE
	  || ((!optimization_summaries || !optimization_summaries->get (callee))
	      && (!summaries_lto || !summaries_lto->get (callee)))
	  || flags_from_decl_or_type (e->callee->decl)
	     & (ECF_CONST | ECF_NOVOPS));
}

/* Compute parm_map for CALLE_EDGE.  */

static void
compute_parm_map (cgraph_edge *callee_edge, vec<modref_parm_map> *parm_map)
{
  class ipa_edge_args *args;
  if (ipa_node_params_sum
      && !callee_edge->call_stmt_cannot_inline_p
      && (args = IPA_EDGE_REF (callee_edge)) != NULL)
    {
      int i, count = ipa_get_cs_argument_count (args);
      class ipa_node_params *caller_parms_info, *callee_pi;
      class ipa_call_summary *es
	     = ipa_call_summaries->get (callee_edge);
      cgraph_node *callee
	 = callee_edge->callee->function_or_virtual_thunk_symbol
			      (NULL, callee_edge->caller);

      caller_parms_info = IPA_NODE_REF (callee_edge->caller->inlined_to
					? callee_edge->caller->inlined_to
					: callee_edge->caller);
      callee_pi = IPA_NODE_REF (callee);

      (*parm_map).safe_grow (count);

      for (i = 0; i < count; i++)
	{
	  if (es && es->param[i].points_to_local_or_readonly_memory)
	    {
	      (*parm_map)[i].parm_index = -2;
	      continue;
	    }

	  struct ipa_jump_func *jf
	     = ipa_get_ith_jump_func (args, i);
	  if (jf && callee_pi)
	    {
	      tree cst = ipa_value_from_jfunc (caller_parms_info,
					       jf,
					       ipa_get_type
						 (callee_pi, i));
	      if (cst && points_to_local_or_readonly_memory_p (cst))
		{
		  (*parm_map)[i].parm_index = -2;
		  continue;
		}
	    }
	  if (jf && jf->type == IPA_JF_PASS_THROUGH)
	    {
	      (*parm_map)[i].parm_index
		= ipa_get_jf_pass_through_formal_id (jf);
	      if (ipa_get_jf_pass_through_operation (jf) == NOP_EXPR)
		{
		  (*parm_map)[i].parm_offset_known = true;
		  (*parm_map)[i].parm_offset = 0;
		}
	      else if (ipa_get_jf_pass_through_operation (jf)
		       == POINTER_PLUS_EXPR
		       && ptrdiff_tree_p (ipa_get_jf_pass_through_operand (jf),
					  &(*parm_map)[i].parm_offset))
		(*parm_map)[i].parm_offset_known = true;
	      else
		(*parm_map)[i].parm_offset_known = false;
	      continue;
	    }
	  if (jf && jf->type == IPA_JF_ANCESTOR)
	    {
	      (*parm_map)[i].parm_index = ipa_get_jf_ancestor_formal_id (jf);
	      (*parm_map)[i].parm_offset_known = true;
	      gcc_checking_assert
		(!(ipa_get_jf_ancestor_offset (jf) & (BITS_PER_UNIT - 1)));
	      (*parm_map)[i].parm_offset
		 = ipa_get_jf_ancestor_offset (jf) >> LOG2_BITS_PER_UNIT;
 	    }
	  else
	    (*parm_map)[i].parm_index = -1;
	}
      if (dump_file)
	{
	  fprintf (dump_file, "  Parm map: ");
	  for (i = 0; i < count; i++)
	    fprintf (dump_file, " %i", (*parm_map)[i].parm_index);
	  fprintf (dump_file, "\n");
	}
    }
}

/* Call EDGE was inlined; merge summary from callee to the caller.  */

void
ipa_merge_modref_summary_after_inlining (cgraph_edge *edge)
{
  if (!summaries && !summaries_lto)
    return;

  struct cgraph_node *to = (edge->caller->inlined_to
			    ? edge->caller->inlined_to : edge->caller);
  class modref_summary *to_info = summaries ? summaries->get (to) : NULL;
  class modref_summary_lto *to_info_lto = summaries_lto
					  ? summaries_lto->get (to) : NULL;

  if (!to_info && !to_info_lto)
    {
      if (summaries)
	summaries->remove (edge->callee);
      if (summaries_lto)
	summaries_lto->remove (edge->callee);
      return;
    }

  class modref_summary *callee_info = summaries ? summaries->get (edge->callee)
				      : NULL;
  class modref_summary_lto *callee_info_lto
		 = summaries_lto ? summaries_lto->get (edge->callee) : NULL;
  int flags = flags_from_decl_or_type (edge->callee->decl);

  if (!callee_info && to_info)
    {
      if (ignore_stores_p (edge->caller->decl, flags))
	to_info->loads->collapse ();
      else
	{
	  summaries->remove (to);
	  to_info = NULL;
	}
    }
  if (!callee_info_lto && to_info_lto)
    {
      if (ignore_stores_p (edge->caller->decl, flags))
	to_info_lto->loads->collapse ();
      else
	{
	  summaries_lto->remove (to);
	  to_info_lto = NULL;
	}
    }
  if (callee_info || callee_info_lto)
    {
      auto_vec <modref_parm_map, 32> parm_map;

      compute_parm_map (edge, &parm_map);

      if (!ignore_stores_p (edge->caller->decl, flags))
	{
	  if (to_info && callee_info)
	    to_info->stores->merge (callee_info->stores, &parm_map);
	  if (to_info_lto && callee_info_lto)
	    to_info_lto->stores->merge (callee_info_lto->stores, &parm_map);
	}
      if (to_info && callee_info)
	to_info->loads->merge (callee_info->loads, &parm_map);
      if (to_info_lto && callee_info_lto)
	to_info_lto->loads->merge (callee_info_lto->loads, &parm_map);
    }
  if (summaries)
    {
      if (to_info && !to_info->useful_p (flags))
	{
	  if (dump_file)
	    fprintf (dump_file, "Removed mod-ref summary for %s\n",
		     to->dump_name ());
	  summaries->remove (to);
	}
      else if (to_info && dump_file)
	{
	  if (dump_file)
	    fprintf (dump_file, "Updated mod-ref summary for %s\n",
		     to->dump_name ());
	  to_info->dump (dump_file);
	}
      if (callee_info)
	summaries->remove (edge->callee);
    }
  if (summaries_lto)
    {
      if (to_info_lto && !to_info_lto->useful_p (flags))
	{
	  if (dump_file)
	    fprintf (dump_file, "Removed mod-ref summary for %s\n",
		     to->dump_name ());
	  summaries_lto->remove (to);
	}
      else if (to_info_lto && dump_file)
	{
	  if (dump_file)
	    fprintf (dump_file, "Updated mod-ref summary for %s\n",
		     to->dump_name ());
	  to_info_lto->dump (dump_file);
	}
      if (callee_info_lto)
	summaries_lto->remove (edge->callee);
    }
  return;
}

/* Collapse loads and return true if something changed.  */

bool
collapse_loads (modref_summary *cur_summary,
		modref_summary_lto *cur_summary_lto)
{
  bool changed = false;

  if (cur_summary && !cur_summary->loads->every_base)
    {
      cur_summary->loads->collapse ();
      changed = true;
    }
  if (cur_summary_lto
      && !cur_summary_lto->loads->every_base)
    {
      cur_summary_lto->loads->collapse ();
      changed = true;
    }
  return changed;
}

/* Perform iterative dataflow on SCC component starting in COMPONENT_NODE.  */

static void
modref_propagate_in_scc (cgraph_node *component_node)
{
  bool changed = true;
  int iteration = 0;

  while (changed)
    {
      changed = false;
      for (struct cgraph_node *cur = component_node; cur;
	   cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
	{
	  cgraph_node *node = cur->inlined_to ? cur->inlined_to : cur;
	  modref_summary *cur_summary = optimization_summaries
					? optimization_summaries->get (node)
					: NULL;
	  modref_summary_lto *cur_summary_lto = summaries_lto
						? summaries_lto->get (node)
						: NULL;

	  if (!cur_summary && !cur_summary_lto)
	    continue;

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");

	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      if (e->indirect_info->ecf_flags & (ECF_CONST | ECF_NOVOPS))
		continue;
	      if (ignore_stores_p (cur->decl, e->indirect_info->ecf_flags))
		{
		  if (dump_file)
		    fprintf (dump_file, "    Indirect call: "
			     "collapsing loads\n");
		  changed |= collapse_loads (cur_summary, cur_summary_lto);
		}
	      else
		{
		  if (dump_file)
		    fprintf (dump_file, "    Indirect call: giving up\n");
		  if (optimization_summaries)
		    optimization_summaries->remove (node);
		  if (summaries_lto)
		    summaries_lto->remove (node);
		  changed = true;
		  cur_summary = NULL;
		  cur_summary_lto = NULL;
		  break;
		}
	    }

	  if (!cur_summary && !cur_summary_lto)
	    continue;

	  for (cgraph_edge *callee_edge = cur->callees; callee_edge;
	       callee_edge = callee_edge->next_callee)
	    {
	      int flags = flags_from_decl_or_type (callee_edge->callee->decl);
	      modref_summary *callee_summary = NULL;
	      modref_summary_lto *callee_summary_lto = NULL;
	      struct cgraph_node *callee;

	      if (flags & (ECF_CONST | ECF_NOVOPS)
		  || !callee_edge->inline_failed)
		continue;

	      /* Get the callee and its summary.  */
	      enum availability avail;
	      callee = callee_edge->callee->function_or_virtual_thunk_symbol
			 (&avail, cur);

	      /* It is not necessary to re-process calls outside of the
		 SCC component.  */
	      if (iteration > 0
		  && (!callee->aux
		      || ((struct ipa_dfs_info *)cur->aux)->scc_no
			  != ((struct ipa_dfs_info *)callee->aux)->scc_no))
		continue;

	      if (dump_file)
		fprintf (dump_file, "    Call to %s\n",
			 callee_edge->callee->dump_name ());

	      bool ignore_stores = ignore_stores_p (cur->decl, flags);

	      if (avail <= AVAIL_INTERPOSABLE)
		{
		  if (!ignore_stores)
		    {
		      if (dump_file)
			fprintf (dump_file, "      Call target interposable"
				 " or not available\n");

		      if (optimization_summaries)
			optimization_summaries->remove (node);
		      if (summaries_lto)
			summaries_lto->remove (node);
		      cur_summary = NULL;
		      cur_summary_lto = NULL;
		      changed = true;
		      break;
		    }
		  else
		    {
		      if (dump_file)
			fprintf (dump_file, "      Call target interposable"
				 " or not available; collapsing loads\n");

		      changed |= collapse_loads (cur_summary, cur_summary_lto);
		      continue;
		    }
		}

	      /* We don't know anything about CALLEE, hence we cannot tell
		 anything about the entire component.  */

	      if (cur_summary
		  && !(callee_summary = optimization_summaries->get (callee)))
		{
		  if (!ignore_stores)
		    {
		      if (dump_file)
			fprintf (dump_file, "      No call target summary\n");

		      optimization_summaries->remove (node);
		      cur_summary = NULL;
		      changed = true;
		    }
		  else
		    {
		      if (dump_file)
			fprintf (dump_file, "      No call target summary;"
				 " collapsing loads\n");

		      if (!cur_summary->loads->every_base)
			{
			  cur_summary->loads->collapse ();
			  changed = true;
			}
		    }
		}
	      if (cur_summary_lto
		  && !(callee_summary_lto = summaries_lto->get (callee)))
		{
		  if (!ignore_stores)
		    {
		      if (dump_file)
			fprintf (dump_file, "      No call target summary\n");

		      summaries_lto->remove (node);
		      cur_summary_lto = NULL;
		      changed = true;
		    }
		  else
		    {
		      if (dump_file)
			fprintf (dump_file, "      No call target summary;"
				 " collapsing loads\n");

		      if (!cur_summary_lto->loads->every_base)
			{
			  cur_summary_lto->loads->collapse ();
			  changed = true;
			}
		    }
		}

	      /* We can not safely optimize based on summary of callee if it
		 does not always bind to current def: it is possible that
		 memory load was optimized out earlier which may not happen in
		 the interposed variant.  */
	      if (!callee_edge->binds_to_current_def_p ())
		{
		  changed |= collapse_loads (cur_summary, cur_summary_lto);
		  if (dump_file)
		    fprintf (dump_file, "      May not bind local;"
			     " collapsing loads\n");
		}


	      auto_vec <modref_parm_map, 32> parm_map;

	      compute_parm_map (callee_edge, &parm_map);

	      /* Merge in callee's information.  */
	      if (callee_summary)
		{
		  changed |= cur_summary->loads->merge
				  (callee_summary->loads, &parm_map);
		  if (!ignore_stores)
		    changed |= cur_summary->stores->merge
				    (callee_summary->stores, &parm_map);
		}
	      if (callee_summary_lto)
		{
		  changed |= cur_summary_lto->loads->merge
				  (callee_summary_lto->loads, &parm_map);
		  if (!ignore_stores)
		    changed |= cur_summary_lto->stores->merge
				    (callee_summary_lto->stores, &parm_map);
		}
	      if (dump_file && changed)
		{
		  if (cur_summary)
		    cur_summary->dump (dump_file);
		  if (cur_summary_lto)
		    cur_summary_lto->dump (dump_file);
		}
	    }
	}
      iteration++;
    }
  if (dump_file)
    {
      fprintf (dump_file,
	       "Propagation finished in %i iterations\n", iteration);
      for (struct cgraph_node *cur = component_node; cur;
	   cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
	if (!cur->inlined_to)
	  {
	    modref_summary *cur_summary = optimization_summaries
					  ? optimization_summaries->get (cur)
					  : NULL;
	    modref_summary_lto *cur_summary_lto = summaries_lto
						  ? summaries_lto->get (cur)
						  : NULL;

	    fprintf (dump_file, "Propagated modref for %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");
	    if (optimization_summaries)
	      {
		if (cur_summary)
		  cur_summary->dump (dump_file);
		else
		  fprintf (dump_file, "  Not tracked\n");
	      }
	    if (summaries_lto)
	      {
		if (cur_summary_lto)
		  cur_summary_lto->dump (dump_file);
		else
		  fprintf (dump_file, "  Not tracked (lto)\n");
	      }
	  }
   }
}

/* Run the IPA pass.  This will take a function's summaries and calls and
   construct new summaries which represent a transitive closure.  So that
   summary of an analyzed function contains information about the loads and
   stores that the function or any function that it calls does.  */

unsigned int
pass_ipa_modref::execute (function *)
{
  if (!summaries && !summaries_lto)
    return 0;

  if (optimization_summaries)
    ggc_delete (optimization_summaries);
  optimization_summaries = summaries;
  summaries = NULL;

  struct cgraph_node **order = XCNEWVEC (struct cgraph_node *,
					 symtab->cgraph_count);
  int order_pos;
  order_pos = ipa_reduced_postorder (order, true, ignore_edge);
  int i;

  /* Iterate over all strongly connected components in post-order.  */
  for (i = 0; i < order_pos; i++)
    {
      /* Get the component's representative.  That's just any node in the
	 component from which we can traverse the entire component.  */
      struct cgraph_node *component_node = order[i];

      if (dump_file)
	fprintf (dump_file, "\n\nStart of SCC component\n");

      modref_propagate_in_scc (component_node);
    }
  if (summaries_lto)
    ((modref_summaries_lto *)summaries_lto)->propagated = true;
  ipa_free_postorder_info ();
  free (order);
  return 0;
}

/* Summaries must stay alive until end of compilation.  */

void
ipa_modref_c_finalize ()
{
  if (optimization_summaries)
    ggc_delete (optimization_summaries);
  optimization_summaries = NULL;
  gcc_checking_assert (!summaries);
  if (summaries_lto)
    {
      ggc_delete (summaries_lto);
      summaries_lto = NULL;
    }
}

#include "gt-ipa-modref.h"
